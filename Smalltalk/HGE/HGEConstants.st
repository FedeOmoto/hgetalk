Smalltalk at: #HGEConstants put: (PoolConstantsDictionary named: #HGEConstants).

HGEConstants at: 'HGE_VERSION' put: 16r181.

"HGE Blending constants"
HGEConstants at: 'BLEND_COLORADD' put: 1.
HGEConstants at: 'BLEND_COLORMUL' put: 0.
HGEConstants at: 'BLEND_ALPHABLEND' put: 2.
HGEConstants at: 'BLEND_ALPHAADD' put: 0.
HGEConstants at: 'BLEND_ZWRITE' put: 4.
HGEConstants at: 'BLEND_NOZWRITE' put: 0.
HGEConstants at: 'BLEND_DEFAULT' put: (HGEConstants at: 'BLEND_COLORMUL') |
								(HGEConstants at: 'BLEND_ALPHABLEND') |
								(HGEConstants at: 'BLEND_NOZWRITE').

HGEConstants at: 'BLEND_DEFAULT_Z' put: (HGEConstants at: 'BLEND_COLORMUL') |
								(HGEConstants at: 'BLEND_ALPHABLEND') |
								(HGEConstants at: 'BLEND_ZWRITE').

"HGE_FPS system state special constants"
HGEConstants at: 'HGEFPS_UNLIMITED' put: 0.
HGEConstants at: 'HGEFPS_VSYNC' put: -1.

"HGE_POWERSTATUS system state special constants"
HGEConstants at: 'HGEPWR_AC' put: -1.
HGEConstants at: 'HGEPWR_UNSUPPORTED' put: -2.

"HGE Primitive type constants"
HGEConstants at: 'HGEPRIM_LINES' put: 2.
HGEConstants at: 'HGEPRIM_TRIPLES' put: 3.
HGEConstants at: 'HGEPRIM_QUADS' put: 4.

"HGE Input Event type constants"
HGEConstants at: 'INPUT_KEYDOWN' put: 1.
HGEConstants at: 'INPUT_KEYUP' put: 2.
HGEConstants at: 'INPUT_MBUTTONDOWN' put: 3.
HGEConstants at: 'INPUT_MBUTTONUP' put: 4.
HGEConstants at: 'INPUT_MOUSEMOVE' put: 5.
HGEConstants at: 'INPUT_MOUSEWHEEL' put: 6.

"HGE Input Event flags"
HGEConstants at: 'HGEINP_SHIFT' put: 1.
HGEConstants at: 'HGEINP_CTRL' put: 2.
HGEConstants at: 'HGEINP_ALT' put: 4.
HGEConstants at: 'HGEINP_CAPSLOCK' put: 8.
HGEConstants at: 'HGEINP_SCROLLLOCK' put: 16.
HGEConstants at: 'HGEINP_NUMLOCK' put: 32.
HGEConstants at: 'HGEINP_REPEAT' put: 64.

"HGE System state constants:
-enum hgeBoolState"
HGEConstants at: 'HGE_WINDOWED' put: 1.
HGEConstants at: 'HGE_ZBUFFER' put: 2.
HGEConstants at: 'HGE_TEXTUREFILTER' put: 3.
HGEConstants at: 'HGE_USESOUND' put: 4.
HGEConstants at: 'HGE_DONTSUSPEND' put: 5.
HGEConstants at: 'HGE_HIDEMOUSE' put: 6.
HGEConstants at: 'HGE_SHOWSPLASH' put: 7.

"HGE System state constants:
-enum hgeFuncState"
HGEConstants at: 'HGE_FRAMEFUNC' put: 8.
HGEConstants at: 'HGE_RENDERFUNC' put: 9.
HGEConstants at: 'HGE_FOCUSLOSTFUNC' put: 10.
HGEConstants at: 'HGE_FOCUSGAINFUNC' put: 11.
HGEConstants at: 'HGE_GFXRESTOREFUNC' put: 12.
HGEConstants at: 'HGE_EXITFUNC' put: 13.

"HGE System state constants:
-enum hgeHwndState"
HGEConstants at: 'HGE_HWND' put: 15.
HGEConstants at: 'HGE_HWNDPARENT' put: 16.

"HGE System state constants:
-enum hgeIntState"
HGEConstants at: 'HGE_SCREENWIDTH' put: 17.
HGEConstants at: 'HGE_SCREENHEIGHT' put: 18.
HGEConstants at: 'HGE_SCREENBPP' put: 19.
HGEConstants at: 'HGE_SAMPLERATE' put: 20.
HGEConstants at: 'HGE_FXVOLUME' put: 21.
HGEConstants at: 'HGE_MUSVOLUME' put: 22.
HGEConstants at: 'HGE_STREAMVOLUME' put: 23.
HGEConstants at: 'HGE_FPS' put: 24.
HGEConstants at: 'HGE_POWERSTATUS' put: 25.

"HGE System state constants:
-enum hgeStringState"
HGEConstants at: 'HGE_ICON' put: 26.
HGEConstants at: 'HGE_TITLE' put: 27.
HGEConstants at: 'HGE_INIFILE' put: 28.
HGEConstants at: 'HGE_LOGFILE' put: 29.

"HGE Virtual-key codes"
HGEConstants at: 'HGEK_LBUTTON' put: 16r01.
HGEConstants at: 'HGEK_RBUTTON' put: 16r02.
HGEConstants at: 'HGEK_MBUTTON' put: 16r04.
HGEConstants at: 'HGEK_ESCAPE' put: 16r1B.
HGEConstants at: 'HGEK_BACKSPACE' put: 16r08.
HGEConstants at: 'HGEK_TAB' put: 16r09.
HGEConstants at: 'HGEK_ENTER' put: 16r0D.
HGEConstants at: 'HGEK_SPACE' put: 16r20.
HGEConstants at: 'HGEK_SHIFT' put: 16r10.
HGEConstants at: 'HGEK_CTRL' put: 16r11.
HGEConstants at: 'HGEK_ALT' put: 16r12.
HGEConstants at: 'HGEK_LWIN' put: 16r5B.
HGEConstants at: 'HGEK_RWIN' put: 16r5C.
HGEConstants at: 'HGEK_APPS' put: 16r5D.
HGEConstants at: 'HGEK_PAUSE' put: 16r13.
HGEConstants at: 'HGEK_CAPSLOCK' put: 16r14.
HGEConstants at: 'HGEK_NUMLOCK' put: 16r90.
HGEConstants at: 'HGEK_SCROLLLOCK' put: 16r91.
HGEConstants at: 'HGEK_PGUP' put: 16r21.
HGEConstants at: 'HGEK_PGDN' put: 16r22.
HGEConstants at: 'HGEK_HOME' put: 16r24.
HGEConstants at: 'HGEK_END' put: 16r23.
HGEConstants at: 'HGEK_INSERT' put: 16r2D.
HGEConstants at: 'HGEK_DELETE' put: 16r2E.
HGEConstants at: 'HGEK_LEFT' put: 16r25.
HGEConstants at: 'HGEK_UP' put: 16r26.
HGEConstants at: 'HGEK_RIGHT' put: 16r27.
HGEConstants at: 'HGEK_DOWN' put: 16r28.
HGEConstants at: 'HGEK_0' put: 16r30.
HGEConstants at: 'HGEK_1' put: 16r31.
HGEConstants at: 'HGEK_2' put: 16r32.
HGEConstants at: 'HGEK_3' put: 16r33.
HGEConstants at: 'HGEK_4' put: 16r34.
HGEConstants at: 'HGEK_5' put: 16r35.
HGEConstants at: 'HGEK_6' put: 16r36.
HGEConstants at: 'HGEK_7' put: 16r37.
HGEConstants at: 'HGEK_8' put: 16r38.
HGEConstants at: 'HGEK_9' put: 16r39.
HGEConstants at: 'HGEK_A' put: 16r41.
HGEConstants at: 'HGEK_B' put: 16r42.
HGEConstants at: 'HGEK_C' put: 16r43.
HGEConstants at: 'HGEK_D' put: 16r44.
HGEConstants at: 'HGEK_E' put: 16r45.
HGEConstants at: 'HGEK_F' put: 16r46.
HGEConstants at: 'HGEK_G' put: 16r47.
HGEConstants at: 'HGEK_H' put: 16r48.
HGEConstants at: 'HGEK_I' put: 16r49.
HGEConstants at: 'HGEK_J' put: 16r4A.
HGEConstants at: 'HGEK_K' put: 16r4B.
HGEConstants at: 'HGEK_L' put: 16r4C.
HGEConstants at: 'HGEK_M' put: 16r4D.
HGEConstants at: 'HGEK_N' put: 16r4E.
HGEConstants at: 'HGEK_O' put: 16r4F.
HGEConstants at: 'HGEK_P' put: 16r50.
HGEConstants at: 'HGEK_Q' put: 16r51.
HGEConstants at: 'HGEK_R' put: 16r52.
HGEConstants at: 'HGEK_S' put: 16r53.
HGEConstants at: 'HGEK_T' put: 16r54.
HGEConstants at: 'HGEK_U' put: 16r55.
HGEConstants at: 'HGEK_V' put: 16r56.
HGEConstants at: 'HGEK_W' put: 16r57.
HGEConstants at: 'HGEK_X' put: 16r58.
HGEConstants at: 'HGEK_Y' put: 16r59.
HGEConstants at: 'HGEK_Z' put: 16r5A.
HGEConstants at: 'HGEK_GRAVE' put: 16rC0.
HGEConstants at: 'HGEK_MINUS' put: 16rBD.
HGEConstants at: 'HGEK_EQUALS' put: 16rBB.
HGEConstants at: 'HGEK_BACKSLASH' put: 16rDC.
HGEConstants at: 'HGEK_LBRACKET' put: 16rDB.
HGEConstants at: 'HGEK_RBRACKET' put: 16rDD.
HGEConstants at: 'HGEK_SEMICOLON' put: 16rBA.
HGEConstants at: 'HGEK_APOSTROPHE' put: 16rDE.
HGEConstants at: 'HGEK_COMMA' put: 16rBC.
HGEConstants at: 'HGEK_PERIOD' put: 16rBE.
HGEConstants at: 'HGEK_SLASH' put: 16rBF.
HGEConstants at: 'HGEK_NUMPAD0' put: 16r60.
HGEConstants at: 'HGEK_NUMPAD1' put: 16r61.
HGEConstants at: 'HGEK_NUMPAD2' put: 16r62.
HGEConstants at: 'HGEK_NUMPAD3' put: 16r63.
HGEConstants at: 'HGEK_NUMPAD4' put: 16r64.
HGEConstants at: 'HGEK_NUMPAD5' put: 16r65.
HGEConstants at: 'HGEK_NUMPAD6' put: 16r66.
HGEConstants at: 'HGEK_NUMPAD7' put: 16r67.
HGEConstants at: 'HGEK_NUMPAD8' put: 16r68.
HGEConstants at: 'HGEK_NUMPAD9' put: 16r69.
HGEConstants at: 'HGEK_MULTIPLY' put: 16r6A.
HGEConstants at: 'HGEK_DIVIDE' put: 16r6F.
HGEConstants at: 'HGEK_ADD' put: 16r6B.
HGEConstants at: 'HGEK_SUBTRACT' put: 16r6D.
HGEConstants at: 'HGEK_DECIMAL' put: 16r6E.
HGEConstants at: 'HGEK_F1' put: 16r70.
HGEConstants at: 'HGEK_F2' put: 16r71.
HGEConstants at: 'HGEK_F3' put: 16r72.
HGEConstants at: 'HGEK_F4' put: 16r73.
HGEConstants at: 'HGEK_F5' put: 16r74.
HGEConstants at: 'HGEK_F6' put: 16r75.
HGEConstants at: 'HGEK_F7' put: 16r76.
HGEConstants at: 'HGEK_F8' put: 16r77.
HGEConstants at: 'HGEK_F9' put: 16r78.
HGEConstants at: 'HGEK_F10' put: 16r79.
HGEConstants at: 'HGEK_F11' put: 16r7A.
HGEConstants at: 'HGEK_F12' put: 16r7B.

"HGEFont Constants"
HGEConstants at: 'HGETEXT_LEFT' put: 0.
HGEConstants at: 'HGETEXT_RIGHT' put: 1.
HGEConstants at: 'HGETEXT_CENTER' put: 2.
HGEConstants at: 'HGETEXT_HORZMASK' put: 16r03.
HGEConstants at: 'HGETEXT_TOP' put: 0.
HGEConstants at: 'HGETEXT_BOTTOM' put: 4.
HGEConstants at: 'HGETEXT_MIDDLE' put: 8.
HGEConstants at: 'HGETEXT_VERTMASK' put: 16r0C.

"HGEDistortionMesh Constants"
HGEConstants at: 'HGEDISP_NODE' put: 0.
HGEConstants at: 'HGEDISP_TOPLEFT' put: 1.
HGEConstants at: 'HGEDISP_CENTER' put: 2.
