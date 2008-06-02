"hge_tut07 - Thousand of Hares.
NOTE: you've to include the HGEConstants pool dictionary to evaluate this workspace code."

"#include <god/save/us/from/the/C++/hell.h>"
backgroundTexture := nil.
hareSprite := nil.
backgroundSprite := nil.
font := nil.
screenWidth := 800.
screenHeight := 600.
minHares := 100.
maxHares := 2000.
visibleHares := 1000.
blendingMode := 0.
hares := Array new: maxHares.
spriteBlendingModes := Array 
			with: BLEND_COLORMUL | BLEND_ALPHABLEND | BLEND_NOZWRITE
			with: BLEND_COLORADD | BLEND_ALPHABLEND | BLEND_NOZWRITE
			with: BLEND_COLORMUL | BLEND_ALPHABLEND | BLEND_NOZWRITE
			with: BLEND_COLORMUL | BLEND_ALPHAADD | BLEND_NOZWRITE
			with: BLEND_COLORMUL | BLEND_ALPHABLEND | BLEND_NOZWRITE.
fontColors := Array 
			with: ARGB white
			with: (ARGB fromInteger: 16rFF000000)
			with: ARGB white
			with: (ARGB fromInteger: 16rFF000000)
			with: ARGB white.
spriteColors := Array 
			with: (Array 
					with: ARGB white
					with: (ARGB fromInteger: 16rFFFFE080)
					with: (ARGB fromInteger: 16rFF80A0FF)
					with: (ARGB fromInteger: 16rFFA0FF80)
					with: (ARGB fromInteger: 16rFFFF80A0))
			with: (Array 
					with: (ARGB fromInteger: 16rFF000000)
					with: (ARGB fromInteger: 16rFF303000)
					with: (ARGB fromInteger: 16rFF000060)
					with: (ARGB fromInteger: 16rFF006000)
					with: (ARGB fromInteger: 16rFF600000))
			with: (Array 
					with: ARGB white
					with: (ARGB fromInteger: 16r80FFE080)
					with: (ARGB fromInteger: 16r8080A0FF)
					with: (ARGB fromInteger: 16r80A0FF80)
					with: (ARGB fromInteger: 16r80FF80A0))
			with: (Array 
					with: ARGB white
					with: (ARGB fromInteger: 16r80FFE080)
					with: (ARGB fromInteger: 16r8080A0FF)
					with: (ARGB fromInteger: 16r80A0FF80)
					with: (ARGB fromInteger: 16r80FF80A0))
			with: (Array 
					with: (ARGB fromInteger: 16r40202020)
					with: (ARGB fromInteger: 16r40302010)
					with: (ARGB fromInteger: 16r40102030)
					with: (ARGB fromInteger: 16r40203010)
					with: (ARGB fromInteger: 16r40102030)).

"Get HGE interface"
hge := HGE new.

frameCallback := ExternalCallback block: 
				[| deltaTime key |
				deltaTime := hge timerGetDelta.

				"Process keys"
				key := hge inputGetKey.
				key = HGEK_UP ifTrue: [visibleHares < maxHares ifTrue: [visibleHares := visibleHares + 100]].
				key = HGEK_DOWN ifTrue: [visibleHares > minHares ifTrue: [visibleHares := visibleHares - 100]].
				(key = HGEK_SPACE or: [blendingMode = 0]) 
					ifTrue: 
						[| random |
						(blendingMode := blendingMode + 1) > 5 ifTrue: [blendingMode := 1].
						hareSprite blendMode: (spriteBlendingModes at: blendingMode).
						font color: (fontColors at: blendingMode).
						random := HGERandomInteger between: 1 and: 5.
						hares do: [:hare | hare color: ((spriteColors at: blendingMode) at: random next)]].

				"Update the scene"
				hares 
					from: 1
					to: visibleHares
					do: 
						[:hare | 
						hare position: hare position + (hare deltaPosition * deltaTime).
						(hare position x between: 0 and: screenWidth) 
							ifFalse: [hare deltaPosition x: hare deltaPosition x negated].
						(hare position y between: 0 and: screenHeight) 
							ifFalse: [hare deltaPosition y: hare deltaPosition y negated].
						hare scale: hare scale + (hare deltaScale * deltaTime).
						(hare scale between: 0.5 and: 2.0) ifFalse: [hare deltaScale: hare deltaScale negated].
						hare rotation: hare rotation + (hare deltaRotation * deltaTime)].

				"Continue execution until ESC is pressed or the user closes the window"
				key = HGEK_ESCAPE]
			descriptor: (ExternalDescriptor 
					callingConvention: 'stdcall:'
					returnType: 'bool'
					argumentTypes: '').

renderCallback := ExternalCallback block: 
				["Render the scene"
				hge gfxBeginScene.
				backgroundSprite renderAt: Point zero.
				hares 
					from: 1
					to: visibleHares
					do: [:hare | hare render].
				font 
					render: 'UP and DOWN to adjust number of hares: ' , visibleHares printString , Character lf asString 
							, 'SPACE to change blending mode: ' , blendingMode printString 
							, Character lf asString , 'FPS: ' 
							, hge timerGetFPS printString
					at: 7 @ 7.
				hge gfxEndScene.

				false]
			descriptor: (ExternalDescriptor 
					callingConvention: 'stdcall:'
					returnType: 'bool'
					argumentTypes: '').


"Set desired system states and initialize HGE"
hge systemSetState: HGE_LOGFILE value: 'hge_tut07.log'.
hge systemSetState: HGE_FRAMEFUNC value: frameCallback asParameter.
hge systemSetState: HGE_RENDERFUNC value: renderCallback asParameter.
hge systemSetState: HGE_TITLE value: 'HGE Tutorial 07 - Thousand of Hares'.
hge systemSetState: HGE_USESOUND value: false.
hge systemSetState: HGE_WINDOWED value: true.
hge systemSetState: HGE_SCREENWIDTH value: screenWidth.
hge systemSetState: HGE_SCREENHEIGHT value: screenHeight.
hge systemSetState: HGE_SCREENBPP value: 32.

hge systemInitiate 
	ifTrue: 
		[data := 
				["Load textures"
				backgroundTexture := HGETexture fromFile: 'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\tutorials\precompiled\bg2.png'] 
						on: HGEError
						do: [:ex | ].

		"If one of the texture files is not found, display an error message and shutdown; continue otherwise.
		We handle the exception in an ugly way to mimic the C tutorial behaviour; after all, this is
		workspace code..."
		data isNil 
			ifTrue: [MessageBox errorMsg: 'Can''t load bg2.png or zazaka.png' caption: 'Error']
			ifFalse: 
				["Load font, create sprites"
				font := HGEFont fromFile: 'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\tutorials\precompiled\font2.fnt'.
				hareSprite := HGESprite fromFile: 'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\tutorials\precompiled\zazaka.png'.
				backgroundSprite := HGESprite fromTexture: backgroundTexture rectangle: (Rectangle origin: 0 @ 0 extent: screenWidth @ screenHeight).
				backgroundSprite blendMode: BLEND_COLORADD | BLEND_ALPHABLEND | BLEND_NOZWRITE.
				backgroundSprite color: (ARGB fromInteger: 16rFF000000) forVertex: 1.
				backgroundSprite color: (ARGB fromInteger: 16rFF000000) forVertex: 2.
				backgroundSprite color: (ARGB fromInteger: 16rFF000040) forVertex: 3.
				backgroundSprite color: (ARGB fromInteger: 16rFF000040) forVertex: 4.

				"Fill the hares array with hareSprite RandomSprites"
				1 to: maxHares do: [:index | hares at: index put: (RandomSprite for: hareSprite)].

				"Let's rock now!"
				hge systemStart]]
	ifFalse: [MessageBox errorMsg: hge systemGetErrorMessage caption: 'Error'].

"Restore video mode and free all allocated resources"
hge systemShutdown
