"hge_tut03 - Using helper classes.
NOTE: you've to include the HGEConstants pool dictionary to evaluate this workspace code."

"Globals we're using in this workspace"
sound := nil.
texture := nil.
sprite := nil.
font := nil.
particleSystem := nil.
x := 100.0.
y := 100.0.
dx := 0.0.
dy := 0.0.
speed := 90.
friction := 0.98.

"Get HGE interface"
hge := HGE new.

"Plays collision sound with parameters based on sprite position and speed"
boom := 
		[| panning pitch |
		panning := ((x - 400) / 4) asInteger.
		pitch := (dx * dx + (dy * dy)) * 0.0005 + 0.2.
		sound 
			playWithVolume: 100
			panning: panning
			pitch: pitch].

frameCallback := ExternalCallback block: 
				[| deltaTime |
				"Get the time elapsed since last call of frameCallback.
				This will help us to synchronize on different machines and video modes."
				deltaTime := hge timerGetDelta.

				"Process keys"
				(hge inputGetKeyState: HGEK_LEFT) ifTrue: [dx := dx - (speed * deltaTime)].
				(hge inputGetKeyState: HGEK_RIGHT) ifTrue: [dx := dx + (speed * deltaTime)].
				(hge inputGetKeyState: HGEK_UP) ifTrue: [dy := dy - (speed * deltaTime)].
				(hge inputGetKeyState: HGEK_DOWN) ifTrue: [dy := dy + (speed * deltaTime)].

				"Do some movement calculations and collision detection"
				dx := dx * friction.
				dy := dy * friction.
				x := x + dx.
				y := y + dy.

				x > 784 
					ifTrue: 
						[x := 784 - (x - 784).
						dx := dx negated.
						boom value].

				x < 16 
					ifTrue: 
						[x := 16 + 16 - x.
						dx := dx negated.
						boom value].

				y > 584 
					ifTrue: 
						[y := 584 - (y - 584).
						dy := dy negated.
						boom value].

				y < 16 
					ifTrue: 
						[y := 16 + 16 - y.
						dy := dy negated.
						boom value].

				"Update particle system"
				particleSystem info nEmission: ((dx * dx + (dy * dy)) * 2) asInteger.
				particleSystem moveTo: x @ y.
				particleSystem update: deltaTime.

				"Continue execution until ESC is pressed or the user closes the window"
				hge inputGetKeyState: HGEK_ESCAPE]
			descriptor: (ExternalDescriptor 
					callingConvention: 'stdcall:'
					returnType: 'bool'
					argumentTypes: '').

"This callback will be called by HGE when the application window should be redrawn.
Our rendering code goes here."
renderCallback := ExternalCallback block: 
				["Render graphics"
				hge gfxBeginScene.
				hge gfxClear: ARGB black asParameter.
				particleSystem render.
				sprite renderAt: x @ y.
				floatString := String writeStream.
				hge timerGetDelta printOn: floatString decimalPlaces: 3.
				font render: 'dt:' , floatString contents, Character lf asString , 'FPS:' , hge timerGetFPS printString , ' (constant)' at: 5 @ 5.
				hge gfxEndScene.

				"renderCallback should always return false"
				false]
			descriptor: (ExternalDescriptor 
					callingConvention: 'stdcall:'
					returnType: 'bool'
					argumentTypes: '').


hge systemSetState: HGE_LOGFILE value: 'hge_tut03.log'.
hge systemSetState: HGE_FRAMEFUNC value: frameCallback asParameter.
hge systemSetState: HGE_RENDERFUNC value: renderCallback asParameter.
hge systemSetState: HGE_TITLE value: 'HGE Tutorial 03 - Using helper classes'.
hge systemSetState: HGE_FPS value: 100.
hge systemSetState: HGE_WINDOWED value: true.
hge systemSetState: HGE_SCREENWIDTH value: 800.
hge systemSetState: HGE_SCREENHEIGHT value: 600.
hge systemSetState: HGE_SCREENBPP value: 32.

hge systemInitiate 
	ifTrue: 
		[data := 
				["Load sound and texture"
				sound := HGEEffect fromFile: 'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\tutorials\precompiled\menu.wav'.
				texture := HGETexture fromFile: 'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\tutorials\precompiled\particles.png'] 
						on: HGEError
						do: [:ex | ].

		"If one of the data files is not found, display an error message and shutdown; continue otherwise.
		We handle the exception in an ugly way to mimic the C tutorial behaviour; after all, this is
		workspace code..."
		data isNil 
			ifTrue: [MessageBox errorMsg: 'Can''t load menu.wav or particles.png' caption: 'Error']
			ifFalse: 
				["Create and set up a sprite"
				sprite := HGESprite fromTexture: texture rectangle: (Rectangle origin: 96 @ 64 extent: 32 @ 32).
				sprite color: (ARGB fromInteger: 16rFFFFA000).
				sprite hotSpot: 16 @ 16.

				"Load a font"
				font := HGEFont fromFile: 'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\tutorials\precompiled\font1.fnt'.

				"Create and set up a particle system"
				particleSystemSprite := HGESprite fromTexture: texture rectangle: (Rectangle origin: 32 @ 32 extent: 32 @ 32).
				particleSystemSprite blendMode: BLEND_COLORMUL | BLEND_ALPHAADD | BLEND_NOZWRITE.
				particleSystemSprite hotSpot: 16 @ 16.
				particleSystem := HGEParticleSystem 
							fromFile: 'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\tutorials\precompiled\trail.psi'
							sprite: particleSystemSprite.
				particleSystem fire.

				"Let's rock now!"
				hge systemStart]]
	ifFalse: [MessageBox errorMsg: hge systemGetErrorMessage caption: 'Error'].

"Restore video mode and free all allocated resources"
hge systemShutdown
