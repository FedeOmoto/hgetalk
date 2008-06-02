"hge_tut01 - Minimal HGE application.
NOTE: you've to include the HGEConstants pool dictionary to evaluate this workspace code."

"Get HGE interface"
hge := HGE new.

"This callback will be called by HGE once per frame. Our game loop code goes here.
In this example we just check whether ESC key has been pressed."
frameCallback := ExternalCallback block: 
				["By returning 'true' we tell HGE to stop running the application, on 'false' continue execution"
				hge inputGetKeyState: HGEK_ESCAPE]
			descriptor: (ExternalDescriptor 
					callingConvention: 'stdcall:'
					returnType: 'bool'
					argumentTypes: '').

"Set our frame callback"
hge systemSetState: HGE_FRAMEFUNC value: frameCallback asParameter.

"Set the window title"
hge systemSetState: HGE_TITLE value: 'HGE Tutorial 01 - Minimal HGE application'.

"Run in windowed mode. Default window size is 800x600"
hge systemSetState: HGE_WINDOWED value: true.

"Don't use BASS for sound"
hge systemSetState: HGE_USESOUND value: false.

"Tries to initiate HGE with the states set.
If something goes wrong, 'false' is returned and more specific description
of what have happened can be read with #systemGetErrorMessage."
hge systemInitiate 
	ifTrue: 
		["Starts calling frameCallback.
		Note that the execution stops here until 'true' is returned from framecallback."
		hge systemStart]
	ifFalse: 
		["If HGE initialization failed show error message"
		MessageBox errorMsg: hge systemGetErrorMessage caption: 'Error'].

"Now ESC has been pressed or the user has closed the window by other means"

"Restore video mode and free all allocated resources"
hge systemShutdown.
