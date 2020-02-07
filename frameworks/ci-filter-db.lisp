(in-package :core-image)

(setf *core-filter-db*
  (list	'(:ci-addition-composition . ("CIAdditionCompositing" . ("inputBackgroundImage")))
	'(:ci-affine-clamp . ("CIAffineClamp" . ( "inputTransform")))
	'(:ci-affine-tile . ("CIAffineTile" . ( "inputTransform")))
	'(:ci-affine-transform . ("CIAffineTransform" . ( "inputTransform")))
	'(:ci-area-average . ("CIAreaAverage" . ( "inputExtent")))
	'(:ci-area-histogram . ("CIAreaHistogram" . ( "inputExtent" "inputCount" "inputScale")))
	'(:ci-area-maximum . ("CIAreaMaximum" . ( "inputExtent")))
	'(:ci-area-maximum-alpha . ("CIAreaMaximumAlpha" . ( "inputExtent")))
	'(:ci-area-minimum . ("CIAreaMinimum" . ( "inputExtent")))
	'(:ci-area-minimum-alpha . ("CIAreaMinimumAlpha" . ( "inputExtent")))
	'(:ci-bars-swipe-transition . ("CIBarsSwipeTransition" . (
								  "inputTargetImage"
								  "inputAngle"
								  "inputWidth"
								  "inputBarOffset"
								  "inputTime")))
	'(:ci-blend-with-mask . ("CIBlendWithMask" . ( "inputBackgroundImage" "inputMaskImage")))
	'(:ci-bloom . ("CIBloom" . ("inputRadius" "inputIntensity")))
	'(:ci-box-blur . ("CIBoxBlur" . ( "inputRadius")))
	'(:ci-bump-distortion . ("CIBumpDistortion" . ( "inputCenter" "inputRadius" "inputScale")))
	'(:ci-bump-distortion-linear . ("CIBumpDistortionLinear" . (
								    "inputCenter"
								    "inputRadius"
								    "inputAngle"
								    "inputScale")))
	'(:ci-checker-board-generator . ("CICheckerboardGenerator" . ("inputCenter"
								      "inputColor0"
								      "inputColor1"
								      "inputWidth"
								      "inputSharpness")))
	'(:ci-circle-splash-distortion . ("CICircleSplashDistortion" . (
									"inputCenter"
									"inputRadius")))
	'(:ci-circular-screen . ("CICircularScreen" . ( "inputCenter" "inputWidth"
						       "inputSharpness")))
	'(:ci-circular-wrap . ("CICircularWrap" . ( "inputCenter" "inputRadius" "inputAngle")))
	'(:ci-cmyk-halftone . ("CICMYKHalftone" . ( "inputCenter" "inputWidth"
						   "inputAngle" "inputSharpness" "inputGCR" "inputUCR")))
	'(:ci-color-blend-mode . ("CIColorBlendMode" . ( "inputBackgroundImage")))
	'(:ci-color-burn-blend-mode . ("CIColorBurnBlendMode" . ( "inputBackgroundImage")))
	'(:ci-color-controls . ("CIColorControls" . ( "inputSaturation" "inputBrightness"
						     "inputContrast")))
	'(:ci-color-cube . ("CIColorCube" . ( "inputCubeDimension" "inputCubeData")))
	'(:ci-color-dodge-blend-mode . ("CIColorDodgeBlendMode" . ( "inputBackgroundImage")))
	'(:ci-color-invert . ("CIColorInvert" . ()))
	'(:ci-color-map . ("CIColorMap" . ( "inputGradientImage")))
	'(:ci-color-matrix . ("CIColorMatrix" . ( "inputRVector" "inputGVector"
						 "inputBVector" "inputAVector" "inputBiasVector")))
	'(:ci-color-monochrome . ("CIColorMonochrome" . ( "inputColor" "inputIntensity")))
	'(:ci-color-posterize . ("CIColorPosterize" . ( "inputLevels")))
	'(:ci-column-average . ("CIColumnAverage" . ( "inputExtent")))
	'(:ci-comic-effect . ("CIComicEffect" . ()))
	'(:ci-constant-color-generator . ("CIConstantColorGenerator" . ("inputColor")))
	'(:ci-copy-machine-transition . ("CICopyMachineTransition" . (
								      "inputTargetImage"
								      "inputExtent"
								      "inputColor"
								      "inputTime"
								      "inputAngle"
								      "inputWidth"
								      "inputOpacity")))
	'(:ci-crop . ("CICrop" . ( "inputRectangle")))
	'(:ci-crystallize . ("CICrystallize" . ( "inputRadius" "inputCenter")))
	'(:ci-darken-blend-mode . ("CIDarkenBlendMode" . ( "inputBackgroundImage")))
	'(:ci-difference-blend-mode . ("CIDifferenceBlendMode" . ( "inputBackgroundImage")))
	'(:ci-disc-blue . ("CIDiscBlur" . ( "inputRadius")))
	'(:ci-disintegrate-with-mask-transition . ("CIDisintegrateWithMaskTransition"
						   . ( "inputTargetImage"
						      "inputMaskImage" "inputTime"
						      "inputShadowRadius" "inputShadowDensity"
						      "inputShadowOffset")))
	'(:ci-displacement-distortion . ("CIDisplacementDistortion" . ( "inputDisplacementImage"
								       "inputScale")))
	'(:ci-dissolve-transition . ("CIDissolveTransition" . ( "inputTargetImage" "inputTime")))
	'(:ci-dot-screen . ("CIDotScreen" . ( "inputCenter" "inputAngle" "inputWidth"
					     "inputSharpness")))
	'(:ci-edges . ("CIEdges" . ( "inputIntensity")))
	'(:ci-edge-work . ("CIEdgeWork" . ( "inputRadius")))
	'(:ci-eightfold-reflected-tile . ("CIEightfoldReflectedTile" . ( "inputCenter"
									"inputAngle" "inputWidth")))
	'(:ci-exclusion-blend-mode . ("CIExclusionBlendMode" . ( "inputBackgroundImage")))
	'(:ci-exposure-adjust . ("CIExposureAdjust" . ( "inputEV")))
	'(:ci-false-color . ("CIFalseColor" . ( "inputColor0" "inputColor1")))
	'(:ci-flash-transition . ("CIFlashTransition" . ( "inputTargetImage" "inputCenter"
							 "inputExtent" "inputColor" "inputTime"
							 "inputMaxStriationRadius"
							 "inputStriationStrength"
							 "inputStriationContrast"
							 "inputFadeThreshold")))
	'(:ci-fourfold-reflected-tile . ("CIFourfoldReflectedTile" . ( "inputCenter"
								      "inputAngle" "inputAcuteAngle"
								      "inputWidth")))
	'(:ci-fourfold-rotated-tile . ("CIFourfoldRotatedTile" . ( "inputCenter"
								  "inputAngle" "inputWidth")))
	'(:ci-fourfold-translated-tile . ("CIFourfoldTranslatedTile" . ( "inputCenter"
									"inputAngle" "inputAcuteAngle"
									"inputWidth")))
	'(:ci-gamma-adjust . ("CIGammaAdjust" . ( "inputPower")))
	'(:ci-gaussian-blur . ("CIGaussianBlur" . ( "inputRadius")))
	'(:ci-gaussian-gradient . ("CIGaussianGradient" . ("inputCenter" "inputColor0" "inputColor1"
							   "inputRadius")))
	'(:ci-glass-distortion . ("CIGlassDistortion" . ( "inputTexture" "inputCenter"
							 "inputScale")))
	'(:ci-glass-lozenge . ("CIGlassLozenge" . ( "inputPoint0" "inputPoint1"
						   "inputRadius" "inputRefraction")))
	'(:ci-glide-reflected-tile . ("CIGlideReflectedTile" . ( "inputCenter"
								"inputAngle" "inputWidth")))
	'(:ci-gloom . ("CIGloom" . ( "inputRadius" "inputIntensity")))
	'(:ci-hard-light-blend-mode . ("CIHardLightBlendMode" . ( "inputBackgroundImage")))
	'(:ci-hatched-screen . ("CIHatchedScreen" . ( "inputCenter" "inputAngle"
						     "inputWidth" "inputSharpness")))
	'(:ci-height-field-from-mask . ("CIHeightFieldFromMask" . ( "inputRadius")))
	'(:ci-hexagonal-pixellate . ("CIHexagonalPixellate" . ( "inputCenter" "inputScale")))
	'(:ci-highlight0shadow-adjust . ("CIHighlightShadowAdjust" . ( "inputHighlightAmount"
								      "inputShadowAmount")))
	'(:ci-hole-distortion . ("CIHoleDistortion" . ( "inputCenter" "inputRadius")))
	'(:ci-hue-adjust . ("CIHueAdjust" . ( "inputAngle")))
	'(:ci-hue-blend-mode . ("CIHueBlendMode" . ( "inputBackgroundImage")))
	'(:ci-kaleidoscope . ("CIKaleidoscope" . ( "inputCount" "inputCenter" "inputAngle")))
	'(:ci-lanczos-scale-transform . ("CILanczosScaleTransform" . ( "inputScale" "inputAspectRatio")))
	'(:ci-lenticular-halo-generator . ("CILenticularHaloGenerator" . ("inputCenter" "inputColor"
									  "inputHaloRadius" "inputHaloWidth"
									  "inputHaloOverlap" "inputStriationStrength"
									  "inputStriationContrast" "inputTime")))
	'(:ci-lighten-blend-mode . ("CILightenBlendMode" . ( "inputBackgroundImage")))
	'(:ci-linear-gradient . ("CILinearGradient" . ("inputPoint0" "inputPoint1" "inputColor0" "inputColor1")))
	'(:ci-line-overlay . ("CILineOverlay" . ( "inputNRNoiseLevel" "inputNRSharpness"
						 "inputEdgeIntensity" "inputThreshold" "inputContrast")))
	'(:ci-line-screen . ("CILineScreen" . ( "inputCenter" "inputAngle" "inputWidth" "inputSharpness")))
	'(:ci-luminosity-blend-mode . ("CILuminosityBlendMode" . ( "inputBackgroundImage")))
	'(:ci-mask-to-alpha . ("CIMaskToAlpha" . ()))
	'(:ci-maximum-component . ("CIMaximumComponent" . ()))
	'(:ci-maximum-compositing . ("CIMaximumCompositing" . ( "inputBackgroundImage")))
	'(:ci-median-filter . ("CIMedianFilter" . ()))
	'(:ci-minimum-component . ("CIMinimumComponent" . ()))
	'(:ci-minimum-compositing . ("CIMinimumCompositing" . ( "inputBackgroundImage")))
	'(:ci-mod-transition . ("CIModTransition" . ( "inputTargetImage" "inputCenter"
						     "inputTime" "inputAngle" "inputRadius" "inputCompression")))
	'(:ci-motion-blur . ("CIMotionBlur" . ( "inputRadius" "inputAngle")))
	'(:ci-multiply-blend-mode . ("CIMultiplyBlendMode" . ( "inputBackgroundImage")))
	'(:ci-multiply-compositing . ("CIMultiplyCompositing" . ( "inputBackgroundImage")))
	'(:ci-noise-reduction . ("CINoiseReduction" . ( "inputNoiseLevel" "inputSharpness")))
	'(:ci-op-tile . ("CIOpTile" . ( "inputCenter" "inputScale" "inputAngle" "inputWidth")))
	'(:ci-overlay-blend-mode . ("CIOverlayBlendMode" . ( "inputBackgroundImage")))
	'(:ci-page-curl-transition . ("CIPageCurlTransition" . ( "inputTargetImage"
								"inputBacksideImage" "inputShadingImage"
								"inputExtent" "inputTime" "inputAngle"
								"inputRadius")))
	'(:ci-parallelogram-tile . ("CIParallelogramTile" . ( "inputCenter" "inputAngle"
							     "inputAcuteAngle" "inputWidth")))
	'(:ci-perspective-tile . ("CIPerspectiveTile" . ( "inputTopLeft" "inputTopRight"
							 "inputBottomRight" "inputBottomLeft")))
	'(:ci-perspective-transform . ("CIPerspectiveTransform" . ( "inputTopLeft"
								   "inputTopRight" "inputBottomRight"
								   "inputBottomLeft")))
	'(:ci-pinch-distortion . ("CIPinchDistortion" . ( "inputCenter" "inputRadius"
							 "inputScale")))
	'(:ci-pixellate . ("CIPixellate" . ( "inputCenter" "inputScale")))
	'(:ci-pointillize . ("CIPointillize" . ( "inputRadius" "inputCenter")))
	'(:ci-radial-gradient . ("CIRadialGradient" . ("inputCenter" "inputRadius0"
						       "inputRadius1" "inputColor0" "inputColor1")))
	'(:ci-random-generator . ("CIRandomGenerator" . nil))
	'(:ci-ripple-transition . ("CIRippleTransition" . ( "inputTargetImage" "inputShadingImage"
							   "inputCenter" "inputExtent" "inputTime" "inputWidth"
							   "inputScale")))
	'(:ci-row-average . ("CIRowAverage" . ( "inputExtent")))
	'(:ci-saturation-blend-mode . ("CISaturationBlendMode" . ( "inputBackgroundImage")))
	'(:ci-screen-blend-mode . ("CIScreenBlendMode" . ( "inputBackgroundImage")))
	'(:ci-sepia-tone . ("CISepiaTone" . ( "inputIntensity")))
	'(:ci-shaded-material . ("CIShadedMaterial" . ( "inputShadingImage" "inputScale")))
	'(:ci-sharpen-luminance . ("CISharpenLuminance" . ( "inputSharpness")))
	'(:ci-sixfold-reflected-tile . ("CISixfoldReflectedTile" . ( "inputCenter"
								    "inputAngle" "inputWidth")))
	'(:ci-sixfold-rotated-tile . ("CISixfoldRotatedTile" . ( "inputCenter" "inputAngle"
								"inputWidth")))
	'(:ci-soft-light-blend-mode . ("CISoftLightBlendMode" . ( "inputBackgroundImage")))
	'(:ci-source-atop-compositing . ("CISourceAtopCompositing" . ( "inputBackgroundImage")))
	'(:ci-source-in-compositing . ("CISourceInCompositing" . ( "inputBackgroundImage")))
	'(:ci-source-out-compositing . ("CISourceOutCompositing" . ( "inputBackgroundImage")))
	'(:ci-source-over-compositing . ("CISourceOverCompositing" . ( "inputBackgroundImage")))
	'(:ci-spot-color . ("CISpotColor" . ( "inputCenterColor1" "inputReplacementColor1"
					     "inputCloseness1" "inputContrast1" "inputCenterColor2"
					     "inputReplacementColor2" "inputCloseness2" "inputContrast2"
					     "inputCenterColor3" "inputReplacementColor3" "inputCloseness3"
					     "inputContrast3")))
	'(:ci-spot-light . ("CISpotLight" . ( "inputLightPosition" "inputLightPointsAt"
					     "inputBrightness" "inputConcentration" "inputColor")))
	'(:ci-star-shine-generator . ("CIStarShineGenerator" . ("inputCenter" "inputColor" "inputRadius"
								"inputCrossScale" "inputCrossAngle"
								"inputCrossOpacity" "inputCrossWidth"
								"inputEpsilon")))
	'(:ci-straighten-filter . ("CIStraightenFilter" . ( "inputAngle")))
	'(:ci-stripes-generator . ("CIStripesGenerator" . ("inputCenter" "inputColor0" "inputColor1"
							   "inputWidth" "inputSharpness")))
	'(:ci-sunbeams-generator . ("CISunbeamsGenerator" . ("inputCenter" "inputColor" "inputSunRadius"
							     "inputMaxStriationRadius" "inputStriationStrength"
							     "inputStriationContrast" "inputTime")))
	'(:ci-swipe-transition . ("CISwipeTransition" . ( "inputTargetImage" "inputExtent"
							 "inputColor" "inputTime" "inputAngle" "inputWidth"
							 "inputOpacity")))
	'(:ci-temperature-and-tint . ("CITemperatureAndTint" . ( "inputNeutral" "inputTargetNeutral")))
	'(:ci-tone-curve . ("CIToneCurve" . ( "inputPoint0" "inputPoint1" "inputPoint2"
					     "inputPoint3" "inputPoint4")))
	'(:ci-torus-lens-distortion . ("CITorusLensDistortion" . ( "inputCenter" "inputRadius"
								  "inputWidth" "inputRefraction")))
	'(:ci-triangle-tile . ("CITriangleTile" . ( "inputCenter" "inputAngle" "inputWidth")))
	'(:ci-twelvefold-reflected-tile . ("CITwelvefoldReflectedTile" . ( "inputCenter"
									  "inputAngle" "inputWidth")))
	'(:ci-twirl-distortion . ("CITwirlDistortion" . ( "inputCenter" "inputRadius" "inputAngle")))
	'(:ci-unsharp-mask . ("CIUnsharpMask" . ( "inputRadius" "inputIntensity")))
	'(:ci-vibrance . ("CIVibrance" . ( "inputAmount")))
	'(:ci-vortex-distortion . ("CIVortexDistortion" . ( "inputCenter" "inputRadius" "inputAngle")))
	'(:ci-white-point-adjust . ("CIWhitePointAdjust" . ( "inputColor")))
	'(:ci-zoom-blur . ("CIZoomBlur" . ("inputCenter" "inputAmount")))))
