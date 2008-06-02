"hge_tut05 - Using distortion mesh.
NOTE: you've to include the HGEConstants pool dictionary to evaluate this workspace code."

"Globals we're using in this workspace"
texture := nil.
distortionMesh := nil.
font := nil.
meshRows := 16.
meshColumns := 16.
cellWidth := 512.0 / (meshColumns - 1).
cellHeight := 512.0 / (meshRows - 1).
meshX := 144.
meshY := 44.
time := 0.0.
effect := 1.

"Get HGE interface"
hge := HGE new.

frameCallback := ExternalCallback block: 
				[| deltaTime key |
				deltaTime := hge timerGetDelta.

				time := time + deltaTime.

				"Switch the effect when the space bar is pressed"
				key := hge inputGetKey.
				key = HGEK_SPACE
					ifTrue: 
						[(effect := effect + 1) > 3 ifTrue: [effect := 1].
						distortionMesh clearWith: (ARGB fromInteger: 16rFF000000)].

				"Calculate new displacements and coloring for one of the three effects"
				effect = 1 
					ifTrue: 
						[1 to: meshRows - 2
							do: 
								[:row | 
								1 to: meshColumns - 2
									do: 
										[:column | 
										| deltaX deltaY |
										deltaX := (time * 10 + ((row + column) / 2)) cos * 5.
										deltaY := (time * 10 + ((row + column) / 2)) sin * 5.
										distortionMesh 
											displacement: deltaX @ deltaY
											forNodeAt: column @ row
											withReference: HGEDISP_NODE]]].

				effect = 2 
					ifTrue: 
						[0 to: meshRows - 1
							do: 
								[:row | 
								1 to: meshColumns - 2
									do: 
										[:column | 
										| deltaX color |
										deltaX := (time * 5 + (column / 2)) cos * 15.
										distortionMesh 
											displacement: deltaX @ 0
											forNodeAt: column @ row
											withReference: HGEDISP_NODE.
										color := (((time * 5 + ((row + column) / 2)) cos + 1) * 35) asInteger.
										distortionMesh 
											color: (ARGB fromInteger: (16rFF bitShift: 24) | (color bitShift: 16) | (color bitShift: 8) | color)
											forNodeAt: column @ row]]].

				effect = 3 
					ifTrue: 
						[0 to: meshRows - 1
							do: 
								[:row | 
								0 to: meshColumns - 1
									do: 
										[:column | 
										| r a deltaX deltaY color |
										r := ((column - (meshColumns / 2)) squared + (row - (meshRows / 2)) squared) sqrt.
										a := r * (time * 2) cos * 0.1.
										deltaX := a sin * (row * cellHeight - 256) + (a cos * (column * cellWidth - 256)).
										deltaY := a cos * (row * cellHeight - 256) - (a sin * (column * cellWidth - 256)).
										distortionMesh 
													displacement: deltaX @ deltaY
													forNodeAt: column @ row
													withReference: HGEDISP_CENTER.
										color := (((r + (time * 4)) cos + 1) * 40) asInteger.
										distortionMesh 
											color: (ARGB fromInteger: (16rFF bitShift: 24) | (color bitShift: 16) | ((color / 2) asInteger bitShift: 8))
											forNodeAt: column @ row]]].

				"Continue execution until ESC is pressed or the user closes the window"
				key = HGEK_ESCAPE]
			descriptor: (ExternalDescriptor 
					callingConvention: 'stdcall:'
					returnType: 'bool'
					argumentTypes: '').

renderCallback := ExternalCallback block: 
				["Render graphics"
				hge gfxBeginScene.
				hge gfxClear: ARGB black asParameter.
				distortionMesh renderAt: meshX @ meshY.
				floatString := String writeStream.
				hge timerGetDelta printOn: floatString decimalPlaces: 3.
				font 
					render: 'dt:' , floatString contents , Character lf asString , 'FPS:' , hge timerGetFPS printString 
							, Character lf asString , Character lf asString 
							, 'Use your' , Character lf asString 
							, 'SPACE!'
					at: 5 @ 5.
				hge gfxEndScene.

				false]
			descriptor: (ExternalDescriptor 
					callingConvention: 'stdcall:'
					returnType: 'bool'
					argumentTypes: '').


hge systemSetState: HGE_LOGFILE value: 'hge_tut05.log'.
hge systemSetState: HGE_FRAMEFUNC value: frameCallback asParameter.
hge systemSetState: HGE_RENDERFUNC value: renderCallback asParameter.
hge systemSetState: HGE_TITLE value: 'HGE Tutorial 05 - Using distortion mesh'.
hge systemSetState: HGE_WINDOWED value: true.
hge systemSetState: HGE_SCREENWIDTH value: 800.
hge systemSetState: HGE_SCREENHEIGHT value: 600.
hge systemSetState: HGE_SCREENBPP value: 32.
hge systemSetState: HGE_USESOUND value: false.

hge systemInitiate 
	ifTrue: 
		[data := 
				["Load a texture"
				texture := HGETexture fromFile: 'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\tutorials\precompiled\texture.jpg'] 
						on: HGEError
						do: [:ex | ].

		"If the texture file is not found, display an error message and shutdown; continue otherwise.
		We handle the exception in an ugly way to mimic the C tutorial behaviour; after all, this is
		workspace code..."
		data isNil 
			ifTrue: [MessageBox errorMsg: 'Can''t load texture.jpg' caption: 'Error']
			ifFalse: 
				["Create a distortion mesh"
				distortionMesh := HGEDistortionMesh columns: meshColumns rows: meshRows.
				distortionMesh texture: texture.
				distortionMesh textureRectangle: texture rectangle.
				distortionMesh blendMode: BLEND_COLORADD | BLEND_ALPHABLEND | BLEND_ZWRITE.
				distortionMesh clearWith: (ARGB fromInteger: 16rFF000000).

				"Load a font"
				font := HGEFont fromFile: 'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\tutorials\precompiled\font1.fnt'.

				"Let's rock now!"
				hge systemStart]]
	ifFalse: [MessageBox errorMsg: hge systemGetErrorMessage caption: 'Error'].

"Restore video mode and free all allocated resources"
hge systemShutdown
