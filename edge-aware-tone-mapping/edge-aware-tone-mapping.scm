; Edge-aware Tone Mapping Filter for GIMP
; Copyright (C) 2012 Martin Hulbek Møller
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Version 1.0 - First version
;
; Overview:
;
;   The effect of processing a photo using an HDR tone mapper (https://en.wikipedia.org/wiki/Tone_mapping)
;   can lead to very impressive results. This is a tone mapping filter for GIMP that can be used for
;   changing the visibility of details and the overall contract of an image. Sometimes, tone mapping can
;   produce a "halo" effect in high-contrast areas--e.g. around the border between a dark foreground object
;   and a bright sky behind it. This filter is aiming at eliminating such halos.
;
;   Is is inspired heavily by the technique described in the article "Local Laplacian Filters: Edge-aware
;   Image Processing with a Laplacian Pyramid" (http://people.csail.mit.edu/sparis/publi/2011/siggraph/).
;
; Where to find it in GIMP:
;
;   The filter will appear as a single menu item: Filters > Enhance > Edge-aware Tone Mapping.
;
; Usage:
;
;   The filter will work with the layer that is currently active. It has six options:
;
;    - Mode
;      The filter performs a lot of computations and will run for longer time than an average GIMP filter.
;      Therefore, I have included these three modes:
;
;       - Preview/evaluate settings
;         This mode is for experimenting with filter settings and previewing the results. The filter will
;         run (relatively) fast but produce results with a posterisation-like look
;         (https://en.wikipedia.org/wiki/Posterization).
;
;       - Image with low contrast
;         In this mode the filter will run roughly 33% faster compared to the third mode but it should
;         only by used with low-contrast images (images without any dark or bright areas).
;
;       - Image with normal contrast
;         This mode can be used for all images but is the mode that runs for the longest time.
;
;    - Noise Reduction
;      Try increasing this setting if you experience that noise or compression artifacts in the image
;      become too visible.
;
;    - Detail/Edge Threshold
;      The filter measures the mutual contrasts between pixels in the image and divides these contrasts
;      into two categories; details and edges. This setting is a threshold that separates the two
;      categories. When the amount of contract is less than this threshold the filter will categorize it
;      as a detail, e.g. it could part of the texture on a surface. Contrasts greater than the threshold
;      are categorize as edges that contribute to the overall contrast of the image, e.g. the difference
;      between dark and bright areas. With a lower detail/edge threshold, fewer contrasts to be
;      categorize as details and vice versa.
;
;    - Details (Smooth <-> Enhance)
;      This setting, which can be either negative or a positive describes the amount of change the filter
;      should apply to contrasts that were categorize as details. A negative value will decrease those
;      contrasts so the corresponding details will appear more smooth. A positive value will increase the
;      contrasts so the details become more visible.
;
;    - Edges (Compress <-> Expand)
;      This setting applies to contrasts that were categorize as edges and it works the same way as for
;      details. A negative value will decrease the overall contrast of the image and a positive value will
;      increase it.
;
;    - Keep Adjustment as Separate Layer
;      If this is checked the result of the filter will be kept as a separate layer. It can be useful if
;      the effect of the filter does not suit some areas of the image. A layer mask can be added afterwards
;      to remove the effect from those areas.
;
;   To get the effect of an HDR tone mapper set the Details setting to a positive value and the Edges
;   setting to a negative value to compress the overall contrast and enhance the details.

(define (script-fu-edge-aware-tone-mapping original-image original-drawable mode noise-reduction detail-edge-threshold detail-handling edge-handling keep-adjustment-as-layer)
	; Begin.
	(gimp-progress-init "Tone Mapping..." -1)
	(gimp-context-push)
	(gimp-image-undo-group-start original-image)

	; Add 1 to the noise reduction since an amount of 1 is neutral.
	(set! noise-reduction (+ noise-reduction 1))

	(let*
		(
			; A function to create a smooth transition from 0 to 1 for a value x in the interval [begin; end].
			(smoothstep
				(lambda (x begin end)
					(cond
						((< x begin) 0)
						((> x end) 1)
						(else
							(begin
								; Normalize x.
								(set! x (/ (- x begin) (- end begin)))

								; Calculate smoothstep polynomial.
								(* x x (- 3 (* 2 x)))
							)
						)
					)
				)
			)

			; A function to create a new Gaussian pyramid image from a drawable.
			(create-gaussian-pyramid
				(lambda (drawable minimum-scale-size)
					(let*
						(
							; Input drawable properties:
							(drawable-height (car (gimp-drawable-height drawable)))
							(drawable-width (car (gimp-drawable-width drawable)))
							(image (car (gimp-drawable-get-image drawable)))
							(image-type (car (gimp-image-base-type image)))

							; The new Gaussian pyramid image:
							(pyramid-image (car (gimp-image-new drawable-width drawable-height image-type)))

							; Iteration variables:
							(current-height (/ drawable-height 2))
							(current-width (/ drawable-width 2))
							(current-level (car (gimp-layer-new-from-drawable drawable pyramid-image)))
							(current-level-number 1)
						)

						; Initialize the bottom level of the pyramid.
						(gimp-image-add-layer pyramid-image current-level 0)

						(gimp-channel-set-visible current-level TRUE)
						(gimp-layer-set-mode current-level NORMAL-MODE)
						(gimp-layer-set-opacity current-level 100)

						; Create the remaining levels.
						(while (and (> current-height minimum-scale-size) (> current-width minimum-scale-size))
							; Create new level as a down-scaled copy of the previous level.
							(set! current-level (car (gimp-layer-copy current-level FALSE)))
							(gimp-image-add-layer pyramid-image current-level current-level-number)
							(gimp-layer-scale-full current-level current-width current-height FALSE INTERPOLATION-LINEAR)

							(gimp-channel-set-visible current-level TRUE)
							(gimp-layer-set-mode current-level NORMAL-MODE)
							(gimp-layer-set-opacity current-level 100)

							; Prepare for next iteration.
							(set! current-level-number (+ current-level-number 1))
							(set! current-height (/ current-height 2))
							(set! current-width (/ current-width 2))
						)

						; Done.
						pyramid-image
					)
				)
			)

			; A function to convert a Gaussian pyramid to a Laplacian pyramid.
			(convert-to-laplacian-pyramid
				(lambda (image)
					(let*
						(
							; All levels in the pyramid:
							(layers (gimp-image-get-layers image))
							(levels (reverse (vector->list (cadr layers))))

							; Iteration variables:
							(current-level (car levels))
							(current-level-number (- (car layers) 1))
							(succeeding-level)
							(succeeding-levels (cdr levels))
							(succeeding-level-copy)
							(upscaled-layer)
						)

						; Ensure the top level is fully visible and has the normal blend mode.
						(gimp-layer-set-mode current-level NORMAL-MODE)
						(gimp-layer-set-opacity current-level 100)
						(gimp-channel-set-visible current-level TRUE)

						; Create a disposable copy of the top level so the original Gaussian top level can remain untouched.
						(set! current-level (car (gimp-layer-copy current-level FALSE)))
						(gimp-image-add-layer image current-level current-level-number)

						(while (not (null? succeeding-levels))
							(set! succeeding-level (car succeeding-levels))

							; Create a copy of the succeeding layer to be used in the calculation of level difference.
							(gimp-layer-set-mode succeeding-level NORMAL-MODE)
							(gimp-layer-set-opacity succeeding-level 100)
							(gimp-channel-set-visible succeeding-level TRUE)

							(set! succeeding-level-copy (car (gimp-layer-copy succeeding-level FALSE)))
							(gimp-image-add-layer image succeeding-level-copy current-level-number)

							; Create an up-scaled copy of the current level.
							(set! upscaled-layer (car (gimp-layer-copy current-level FALSE)))
							(gimp-image-add-layer image upscaled-layer current-level-number)
							(gimp-layer-scale-full
								upscaled-layer
								(car (gimp-drawable-width succeeding-level))
								(car (gimp-drawable-height succeeding-level))
								FALSE
								INTERPOLATION-LINEAR)

							(gimp-layer-set-mode upscaled-layer GRAIN-EXTRACT-MODE)

							; Create a new Laplacian image at the current level.
							(set! succeeding-level-copy (car (gimp-image-merge-down image upscaled-layer EXPAND-AS-NECESSARY)))

							; Remove the corresponding Gaussian image at the current level.
							(gimp-image-remove-layer image current-level)

							; Prepare for next iteration.
							(set! current-level (car succeeding-levels))
							(set! succeeding-levels (cdr succeeding-levels))
							(set! current-level-number (- current-level-number 1))
						)

						; Finally, remove the remaining bottom level of the Gaussian pyramid.
						(gimp-image-remove-layer image current-level)
					)
				)
			)

			; A function to create a mapping function for the tone mapper. The mapping function is
			; created as look-up array to be applied to an image using GIMP's Curves function.
			(create-mapping-function
				(lambda (middle-index detail-radius noise-reduction-amount)
					; Make sure middle-index and detail-radius are integers. (Otherwise, it could really mess up this function.)
					(set! middle-index (round middle-index))
					(set! detail-radius (round detail-radius))

					(let*
						(
							(bias 128)
							(curve (cons-array 256 'byte))
							(detailCurveStrength 3)
							(index 0)
							(normalized-detail-handling (expt detailCurveStrength (/ (- detail-handling) 100)))

							; A function to define the detail part of the mapping curve.
							(detail-mapping-function
								(lambda (x)
									(if (= x 0)
										; Special case for index zero. (In case the detail radius is 0 the general case would divide by 0.)
										0
										; General case.
										((if (negative? x) - +) (* (expt (/ (abs x) detail-radius) normalized-detail-handling) detail-radius))
									)
								)
							)

							; Function to define the edge parts of the mapping curve.
							(edge-mapping-function
								(lambda (x)
									((if (negative? x) - +) x (* (/ edge-handling 100) (- (abs x) detail-radius)))
								)
							)
						)

						; Populate each entry of the mapping curve.
						(while (<= index 255)
							(let*
								(
									(zero-biased-x (- index middle-index))
									(zero-biased-y
										(if (or (< zero-biased-x (- detail-radius)) (> zero-biased-x detail-radius))
											(edge-mapping-function zero-biased-x)
											(detail-mapping-function zero-biased-x)
										)
									)
								)

								; Apply noise reduction.
								(if (and (<= (abs zero-biased-x) noise-reduction-amount) (< (abs zero-biased-x) (abs zero-biased-y)))
									(let*
										(
											(amount (smoothstep (abs zero-biased-x) (/ noise-reduction-amount 2) noise-reduction-amount))
										)

										(set! zero-biased-y (+ (* zero-biased-y amount) (* zero-biased-x (- 1 amount))))
									)
								)

								; Set curve value.
								(aset curve index (max 0 (min 255 (+ bias (round zero-biased-y)))))

								(set! index (+ index 1))
							)
						)

						; Done, return.
						curve
					)
				)
			)

			; A function to collapse the layers of a Laplacian pyramid to an single layer.
			(collapse-laplacian-pyramid
				(lambda (image)
					(let*
						(
							; All levels in the pyramid:
							(levels (reverse (vector->list (cadr (gimp-image-get-layers image)))))

							; Iteration variables:
							(current-level (car levels))
							(current-level-number 0)
							(succeeding-level)
							(succeeding-levels (cdr levels))
						)

						; Ensure the first level is fully visible and has the normal blend mode.
						(gimp-layer-set-mode current-level NORMAL-MODE)
						(gimp-layer-set-opacity current-level 100)
						(gimp-channel-set-visible current-level TRUE)

						; Collapse all levels from the top until the bottom is reached.
						(while (not (null? succeeding-levels))
							; Up-scale current level to match the succeeding.
							(set! succeeding-level (car succeeding-levels))

							(gimp-layer-scale-full
								current-level
								(car (gimp-drawable-width succeeding-level))
								(car (gimp-drawable-height succeeding-level))
								FALSE
								INTERPOLATION-LINEAR)

							; Combine the current level with the succeeding level.
							(gimp-layer-set-mode succeeding-level GRAIN-MERGE-MODE)
							(gimp-layer-set-opacity succeeding-level 100)
							(gimp-channel-set-visible succeeding-level TRUE)

							(set! current-level (car (gimp-image-merge-down image succeeding-level EXPAND-AS-NECESSARY)))

							; Prepare for next iteration.
							(set! succeeding-levels (cdr succeeding-levels))
						)

						; Return the collapsed level.
						current-level
					)
				)
			)

			; A function to perform tone-mapping of a drawable.
			(create-intermediate-laplacian-pyramid
				(lambda (gray-scale-image low-intensity high-intensity detail-radius noise-reduction-amount progress-callback)
					(let*
						(
							(contrast-reduction-factor (/ 256 (+ (- high-intensity low-intensity) 1)))

							; Clone of the original drawable that can be modified without affecting the original.
							(gray-scale-drawable (car (gimp-layer-copy (vector-ref (cadr (gimp-image-get-layers gray-scale-image)) 0) FALSE)))

							; A Gaussian pyramid built from the gray-scale input image. This is the input to the main tone mapping algorithm.
							(original-gaussian-pyramid)

							; This Laplacian pyramid will be constructed iteratively and finally collapsed into the final result.
							(result-laplacian-pyramid)
						)

						(gimp-image-add-layer gray-scale-image gray-scale-drawable -1)

						; Reduce image contrast :-/ (To avoid clipping of highlights and shadows.)
						(gimp-levels gray-scale-drawable HISTOGRAM-VALUE 0 255 1 low-intensity high-intensity)
						(set! detail-radius (/ detail-radius contrast-reduction-factor))
						(set! noise-reduction-amount (/ noise-reduction-amount contrast-reduction-factor))

						; Create a Gaussian pyramid from the original gray-scale image.
						(set! original-gaussian-pyramid (create-gaussian-pyramid gray-scale-drawable 2))

						; Initialize an intermediate Laplacian pyramid with zero values in the all levels except from the top level
						; which is identical to the corresponding Gaussian level.
						(gimp-context-set-background '(0 0 0))

						(set! result-laplacian-pyramid (car (gimp-image-duplicate original-gaussian-pyramid)))
						(for-each
							(lambda (level)
								(gimp-drawable-fill level BACKGROUND-FILL)
							)

							(cdr (reverse (vector->list (cadr (gimp-image-get-layers result-laplacian-pyramid)))))
						)

						; Main loop: Iterate through each of the possible pixel values [low-intensity; high-intensity] of the input image
						; and update for value the Laplacian pyramid at locations where the Gaussian pyramid is equal to that value.
						(let*
							(
								(current-pixel-value low-intensity)
								(remapped-laplacian-pyramid 0)
							)

							(while (<= current-pixel-value high-intensity)

								; Calculate an intermediate Laplacian pyramid based on a remapped version of the original image.
								(let*
									(
										(remapped-gray-scale-drawable (car (gimp-layer-copy gray-scale-drawable FALSE)))
										(mapping-function (create-mapping-function current-pixel-value detail-radius noise-reduction-amount))
									)

									; Make a temporary copy of the original gray-scale layer and apply the mapping function to it. 
									(gimp-image-add-layer gray-scale-image remapped-gray-scale-drawable -1)
									(gimp-curves-explicit remapped-gray-scale-drawable HISTOGRAM-VALUE 256 mapping-function)

									; Build a new intermediate Laplacian pyramid from the remapped layer.
									(set! remapped-laplacian-pyramid (create-gaussian-pyramid remapped-gray-scale-drawable 2))
									(convert-to-laplacian-pyramid remapped-laplacian-pyramid)

									; Clean up the temporary remapped layer.
									(gimp-image-remove-layer gray-scale-image remapped-gray-scale-drawable)
								)

								; Update the progress bar.
								(progress-callback (- current-pixel-value low-intensity))

								; Copy all pixels from the intermediate Laplacian pyramid to the result pyramid at all locations where the corresponding pixel in the
								; original Gaussian pyramid is equal to the pixel value of the current iteration. Do this for all level of the pyramids except from the top level.
								(let*
									(
										(current-level 0)
										(gaussian-levels (vector->list (cadr (gimp-image-get-layers original-gaussian-pyramid))))
										(laplacian-levels (vector->list (cadr (gimp-image-get-layers result-laplacian-pyramid))))
										(remapped-laplacian-levels (vector->list (cadr (gimp-image-get-layers remapped-laplacian-pyramid))))
									)

									(while (> (length gaussian-levels) 1)

										(let*
											(
												(masked-laplacian-level (car (gimp-layer-new-from-drawable (car remapped-laplacian-levels) result-laplacian-pyramid)))
												(masked-laplacian-level-mask 0)
											)

											; Copy the current layer from the intermediate Laplacian pyramid to the result pyramid.
											(gimp-image-add-layer result-laplacian-pyramid masked-laplacian-level current-level)

											; Create a mask that has the value 255 where the corresponding pixel in the Gaussian layer
											; is equal to the current pixel value and zeros everywhere else.
											(set! masked-laplacian-level-mask (car (gimp-layer-new-from-drawable (car gaussian-levels) result-laplacian-pyramid)))
											(gimp-image-add-layer result-laplacian-pyramid masked-laplacian-level-mask current-level)
											(gimp-layer-set-mode masked-laplacian-level-mask MULTIPLY-MODE)
											(gimp-threshold masked-laplacian-level-mask current-pixel-value current-pixel-value)

											; Apply the mask to the intermediate Laplacian layer in the primary pyramid
											(set! masked-laplacian-level (car (gimp-image-merge-down result-laplacian-pyramid masked-laplacian-level-mask EXPAND-AS-NECESSARY)))

											; Finally, merge the masked Laplacian layer with the result Laplacian layer.
											(gimp-layer-set-mode masked-laplacian-level ADDITION-MODE)
											(gimp-image-merge-down result-laplacian-pyramid masked-laplacian-level EXPAND-AS-NECESSARY)

											; Next pyramid level.
											(set! current-level (+ current-level 1))
											(set! gaussian-levels (cdr gaussian-levels))
											(set! laplacian-levels (cdr laplacian-levels))
											(set! remapped-laplacian-levels (cdr remapped-laplacian-levels))
										)
									)
								)

								; Next pixel value.
								(gimp-image-delete remapped-laplacian-pyramid)
								(set! current-pixel-value (+ current-pixel-value 1))
							)
						)

						; Clean up.
						(gimp-image-remove-layer gray-scale-image gray-scale-drawable)
						(gimp-image-delete original-gaussian-pyramid)
						
						; Return.
						result-laplacian-pyramid
					)
				)
			)

			; A function to create a gray-scale luminance image from a source color or gray-scale image. Selection is taken into account.
			(get-luminance-image
				(lambda (drawable)
					(let*
						(
							(image (car (gimp-drawable-get-image drawable)))
							(gray-scale-image)
							(offset-x (car (gimp-drawable-offsets drawable)))
							(offset-y (cadr (gimp-drawable-offsets drawable)))
							(selection-bounds (gimp-selection-bounds image))
						)

						; Create a new b/w image from the provided image.
						(if (= (car (gimp-image-base-type image)) RGB)
							(begin
								; Convert an RGB image to b/w.
								(set! gray-scale-image (car (plug-in-decompose 1 image drawable "Value" 0)))
							)
							(begin
								; Copy a b/w image.
								(set! gray-scale-image (car (gimp-image-new (car (gimp-drawable-width drawable)) (car (gimp-drawable-height drawable)) GRAY)))
								(gimp-image-add-layer gray-scale-image (car (gimp-layer-new-from-drawable drawable gray-scale-image)) -1)
							)
						)

						; Crop the image if there is an active selection.
						(if (= (car selection-bounds) TRUE)
							(let*
								(
									(x1 (cadr selection-bounds))
									(y1 (caddr selection-bounds))
									(x2 (cadddr selection-bounds))
									(y2 (car (cddddr selection-bounds)))
								)

								(gimp-image-crop gray-scale-image (- x2 x1) (- y2 y1) (- x1 offset-x) (- y1 offset-y))
								(set! offset-x x1)
								(set! offset-y y1)
							)
						)

						(list gray-scale-image offset-x offset-y)
					)
				)
			)

			(detail-radius (/ detail-edge-threshold 2))

			; Configuration of contrast reduction:
			(low-intensity (cond ((= mode 0) 112) (else 0)))
			(high-intensity (cond ((= mode 0) 143) (else 255)))
			(final-iteration (cond ((= mode 0) 31) ((= mode 1) 255) (else 383)))

			; Progress bar maintenance.
			(update-progress (lambda (iteration-offset)
				(lambda (iteration)
					(gimp-progress-update (/ (+ iteration iteration-offset) final-iteration))
					(gimp-progress-set-text "Tone Mapping...")
				))
			)

			; A gray-scale copy of the original image.
			(luminance-image (get-luminance-image original-drawable))
			(gray-scale-image (car luminance-image))
			(offset-x (cadr luminance-image))
			(offset-y (caddr luminance-image))

			; The primary tone mapping pyramid for any mode (fast preview, normal, or clipping avoidance):
			(primary-pyramid (create-intermediate-laplacian-pyramid gray-scale-image low-intensity high-intensity detail-radius noise-reduction (update-progress 0)))
			(primary-layer)

			; Clipping avoidance mode uses an additional pyramid with compressed contrast to prevent clipping.
			(low-contrast-layer)
		)

		; In clipping avoidance mode create an additional pyramid compressed contrast to prevent clipping.
		(if (= mode 2)
			(begin
				; Fade from the primary pyramid a the most detailed levels (lower level) to the low-contrast pyramid at higher less detailed levels.
				(let*
					(
						; Create pyramid with compressed contrast.
						(low-contrast-pyramid (create-intermediate-laplacian-pyramid gray-scale-image 64 191 detail-radius noise-reduction (update-progress 256)))

						; All levels of the primary pyramid and the low-contrast pyramid.
						(primary-levels (gimp-image-get-layers primary-pyramid))
						(primary-layers (vector->list (cadr primary-levels)))
						(low-contrast-layers (vector->list (cadr (gimp-image-get-layers low-contrast-pyramid))))

						(num-levels (car primary-levels))
						(current-level 0)

						; Crossfade function:
						(detail-fraction 3)
						(crossfade-function
							(lambda (x)
								(* 100 (smoothstep x 0 (- (/ num-levels detail-fraction) 1)))
							)
						)
					)

					; Fade pyramid layers.
					(gimp-context-set-background '(128 128 128))

					(while (< current-level num-levels)
						(gimp-edit-bucket-fill-full (car primary-layers) BG-BUCKET-FILL NORMAL-MODE (crossfade-function current-level) 255 FALSE FALSE 0 0 0)
						(gimp-edit-bucket-fill-full (car low-contrast-layers) BG-BUCKET-FILL NORMAL-MODE (- 100 (crossfade-function current-level)) 255 FALSE FALSE 0 0 0)

						; Next level.
						(set! current-level (+ current-level 1))
						(set! primary-layers (cdr primary-layers))
						(set! low-contrast-layers (cdr low-contrast-layers))
					)

					; Collapse the low-contrast pyramid and stretch its histogram.
					(set! low-contrast-layer (collapse-laplacian-pyramid low-contrast-pyramid))
					(gimp-levels low-contrast-layer HISTOGRAM-VALUE 64 191 1 0 255)
				)
			)
		)

		; Collapse the primary pyramid. If in clipping avoidance mode combine it with the low-contrast pyramid.
		(set! primary-layer (collapse-laplacian-pyramid primary-pyramid))
		(gimp-levels primary-layer HISTOGRAM-VALUE low-intensity high-intensity 1 0 255)

		; Blend primary pyramid with low-contrast pyramid if in clipping avoidance mode.
		(if (not (null? low-contrast-layer))
			(let*
				(
					(low-contrast-pyramid (car (gimp-drawable-get-image low-contrast-layer)))
				)
				
				; Copy low-contrast layer to the primary pyramid image and delete the low-contrast pyramid.
				(set! low-contrast-layer (car (gimp-layer-new-from-drawable low-contrast-layer primary-pyramid)))
				(gimp-image-add-layer primary-pyramid low-contrast-layer 1)
				(gimp-image-delete low-contrast-pyramid)

				; Blend layers.
				(gimp-layer-set-mode primary-layer GRAIN-MERGE-MODE)
				(set! primary-layer (car (gimp-image-merge-down primary-pyramid primary-layer EXPAND-AS-NECESSARY)))
			)
		)

		; Move the result layer to the original image.
		(set! primary-layer (car (gimp-layer-new-from-drawable primary-layer original-image)))
		(gimp-image-add-layer original-image primary-layer (car (gimp-image-get-layer-position original-image original-drawable)))
		(gimp-layer-translate primary-layer offset-x offset-y)
		(gimp-layer-set-mode primary-layer VALUE-MODE)
		(gimp-image-delete primary-pyramid)

		(if (= keep-adjustment-as-layer TRUE)
			(gimp-drawable-set-name primary-layer (string-append (car (gimp-drawable-get-name original-drawable)) " - Tone Mapped"))
			(gimp-image-merge-down original-image primary-layer EXPAND-AS-NECESSARY)
		)

		; Clean up.
		(gimp-image-delete gray-scale-image)
	)

	; Done.
	(gimp-image-undo-group-end original-image)
	(gimp-displays-flush)
	(gimp-progress-end)
	(gimp-context-pop)
)

(script-fu-register
	"script-fu-edge-aware-tone-mapping"         ; Function name
	"Edge-aware Tone Mapping..."                ; Menu label
	"Edge-aware filter for HDR tone mapping\
and contrast compression or expansion."             ; Description
	"Martin Hulbek Møller"                      ; Author
	"Copyright 2012, Martin Hulbek Møller"      ; Copyright notice
	"November 3, 2012"                          ; Date created
	"RGB* GRAY*"                                ; Image type that the script works on

	SF-IMAGE       "image"                              0
	SF-DRAWABLE    "drawable"                           0
	SF-OPTION     _"Mode"                               '("Preview/evaluate settings (relatively fast)" "Image with low contrast (slow)" "Image with normal contrast (even slower)")
	SF-OPTION     _"Noise Reduction"                    '("Deactivated" "1" "2" "3" "4")
	SF-ADJUSTMENT _"Detail/Edge Threshold"              '(80 0 255 1 10 0 0)
	SF-ADJUSTMENT _"Details (Smooth <-> Enhance)"       '(0 -100 100 1 10 0 0)
	SF-ADJUSTMENT _"Edges (Compress <-> Expand)"        '(0 -100 100 1 10 0 0)
	SF-TOGGLE     _"Keep Adjustment as Separate Layer"  TRUE
)
(script-fu-menu-register "script-fu-edge-aware-tone-mapping" "<Image>/Filters/Enhance")