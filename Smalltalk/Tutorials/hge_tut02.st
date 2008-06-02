"hge_tut02 - Using input, sound and rendering.
NOTE: you've to include the HGEConstants pool dictionary to evaluate this workspace code."

"Globals we're using in this workspace"
sound := nil.
texture := nil.
x := 100.0.
y := 100.0.
dx := 0.0.
dy := 0.0.
speed := 90.
friction := 0.98.
quad := HGEQuad new.

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

				"Set up quad's screen coordinates"
				(quad v at: 1) x: x - 16 y: y - 16.
				(quad v at: 2) x: x + 16 y: y - 16.
				(quad v at: 3) x: x + 16 y: y + 16.
				(quad v at: 4) x: x - 16 y: y + 16.

				"Continue execution until ESC is pressed or the user closes the window"
				hge inputGetKeyState: HGEK_ESCAPE]
			descriptor: (ExternalDescriptor 
					callingConvention: 'stdcall:'
					returnType: 'bool'
					argumentTypes: '').

"This callback will be called by HGE when the application window should be redrawn.
Our rendering code goes here."
renderCallback := ExternalCallback block: 
				["Begin rendering quads.
				This method must be called before any actual rendering."
				hge gfxBeginScene.

				"Clear screen with black color"
				hge gfxClear: ARGB black asParameter.

				"Render quads here. This time just one of them will serve our needs."
				quad render.

				"End rendering and update the screen"
				hge gfxEndScene.

				"renderCallback should always return false"
				false]
			descriptor: (ExternalDescriptor 
					callingConvention: 'stdcall:'
					returnType: 'bool'
					argumentTypes: '').

"Set up log file, frame callback, render callback and window title"
hge systemSetState: HGE_LOGFILE value: 'hge_tut02.log'.
hge systemSetState: HGE_FRAMEFUNC value: frameCallback asParameter.
hge systemSetState: HGE_RENDERFUNC value: renderCallback asParameter.
hge systemSetState: HGE_TITLE value: 'HGE Tutorial 02 - Using input, sound and rendering'.

"Set up video mode"
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
				["Set up quad's texture"
				quad tex: texture.

				"Set up quad which we will use for rendering sprite"
				quad blend: BLEND_ALPHAADD | BLEND_COLORMUL | BLEND_ZWRITE.

				quad v do: 
						[:vertex | 
						"Set up z-coordinate of vertices"
						vertex z: 0.5.

						"Set up color"
						vertex col: (ARGB fromInteger: 16rFFFFA000) asParameter].

				"Set up quad's texture coordinates.
				0,0 means top left corner and 1,1 bottom right corner of the texture."
				(quad v at: 1) tx: 96.0 / 128.0 ty: 64.0 / 128.0.
				(quad v at: 2) tx: 128.0 / 128.0 ty: 64.0 / 128.0.
				(quad v at: 3) tx: 128.0 / 128.0 ty: 96.0 / 128.0.
				(quad v at: 4) tx: 96.0 / 128.0 ty: 96.0 / 128.0.

				"Let's rock now!"
				hge systemStart]]
	ifFalse: [MessageBox errorMsg: hge systemGetErrorMessage caption: 'Error'].

"Restore video mode and free all allocated resources"
hge systemShutdown
