| package |
package := Package name: 'HGE'.
package paxVersion: 1;
	basicComment: 'Copyright (c) 2008, Federico Omoto
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the author nor the names of its contributors may
      be used to endorse or promote products derived from this software
      without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'''' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
'.


package classNames
	add: #HGE;
	add: #HGEChannel;
	add: #HGEColorRGB;
	add: #HGEDistortionMesh;
	add: #HGEEffect;
	add: #HGEError;
	add: #HGEFont;
	add: #HGEHandle;
	add: #HGEInputEvent;
	add: #HGELibrary;
	add: #HGEParticleSystem;
	add: #HGEParticleSystemInfo;
	add: #HGEQuad;
	add: #HGERandom;
	add: #HGERandomFloat;
	add: #HGERandomInteger;
	add: #HGESprite;
	add: #HGETarget;
	add: #HGETexture;
	add: #HGETriple;
	add: #HGEVertex;
	add: #RandomSprite;
	yourself.

package methodNames
	add: #Integer -> #asHGEEffect;
	add: #Integer -> #asHGETarget;
	add: #Integer -> #asHGETexture;
	yourself.

package globalNames
	add: #HGEConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus';
	yourself).

package!

"Class Definitions"!

Object subclass: #RandomSprite
	instanceVariableNames: 'sprite position deltaPosition scale deltaScale rotation deltaRotation color'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #HGEError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #HGELibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGE
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'HGEConstants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGEColorRGB
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGEDistortionMesh
	instanceVariableNames: 'texture'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGEFont
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'HGEConstants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGEInputEvent
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGEParticleSystem
	instanceVariableNames: 'sprite'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGEParticleSystemInfo
	instanceVariableNames: 'sprite'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGEQuad
	instanceVariableNames: 'texture'
	classVariableNames: ''
	poolDictionaries: 'HGEConstants'
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGESprite
	instanceVariableNames: 'texture'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGETriple
	instanceVariableNames: 'texture'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalStructure subclass: #HGEVertex
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DWORD subclass: #HGEChannel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DWORD subclass: #HGEHandle
	instanceVariableNames: 'hge needsToFreeExternalResources'
	classVariableNames: ''
	poolDictionaries: 'HGEConstants'
	classInstanceVariableNames: ''!
HGEHandle subclass: #HGEEffect
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
HGEHandle subclass: #HGETarget
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
HGEHandle subclass: #HGETexture
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Random subclass: #HGERandom
	instanceVariableNames: 'minNumber maxNumber next'
	classVariableNames: 'Seed'
	poolDictionaries: 'HGEConstants'
	classInstanceVariableNames: ''!
HGERandom subclass: #HGERandomFloat
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
HGERandom subclass: #HGERandomInteger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Integer methodsFor!

asHGEEffect
	"Answer an HGEEffect from the receiver.
	NOTE: if you're working at a low level (using just the bindings), be careful of not freeing
	the effect (by calling HGE>>effectFree:) as it will be automatically released using
	finalization. If you want to be in control of the release process you can use
	HGEEffect>>needsToFreeExternalResources: after calling this method."

	^HGEEffect new value: self!

asHGETarget
	"Answer an HGETarget from the receiver.
	NOTE: if you're working at a low level (using just the bindings), be careful of not freeing
	the render target (by calling HGE>>targetFree:) as it will be automatically released using
	finalization. If you want to be in control of the release process you can use
	HGEEffect>>needsToFreeExternalResources: after calling this method."

	^HGETarget new value: self!

asHGETexture
	"Answer an HGETexture from the receiver.
	NOTE: if you're working at a low level (using just the bindings), be careful of not freeing
	the texture (by calling HGE>>textureFree:) as it will be automatically released using
	finalization. If you want to be in control of the release process you can use
	HGEEffect>>needsToFreeExternalResources: after calling this method."

	^HGETexture new value: self! !
!Integer categoriesFor: #asHGEEffect!converting!public! !
!Integer categoriesFor: #asHGETarget!converting!public! !
!Integer categoriesFor: #asHGETexture!converting!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #HGEConstants put: (PoolConstantsDictionary named: #HGEConstants)!
HGEConstants at: 'BLEND_ALPHAADD' put: 16r0!
HGEConstants at: 'BLEND_ALPHABLEND' put: 16r2!
HGEConstants at: 'BLEND_COLORADD' put: 16r1!
HGEConstants at: 'BLEND_COLORMUL' put: 16r0!
HGEConstants at: 'BLEND_DEFAULT' put: 16r2!
HGEConstants at: 'BLEND_DEFAULT_Z' put: 16r6!
HGEConstants at: 'BLEND_NOZWRITE' put: 16r0!
HGEConstants at: 'BLEND_ZWRITE' put: 16r4!
HGEConstants at: 'HGE_DONTSUSPEND' put: 16r5!
HGEConstants at: 'HGE_EXITFUNC' put: 16rD!
HGEConstants at: 'HGE_FOCUSGAINFUNC' put: 16rB!
HGEConstants at: 'HGE_FOCUSLOSTFUNC' put: 16rA!
HGEConstants at: 'HGE_FPS' put: 16r18!
HGEConstants at: 'HGE_FRAMEFUNC' put: 16r8!
HGEConstants at: 'HGE_FXVOLUME' put: 16r15!
HGEConstants at: 'HGE_GFXRESTOREFUNC' put: 16rC!
HGEConstants at: 'HGE_HIDEMOUSE' put: 16r6!
HGEConstants at: 'HGE_HWND' put: 16rF!
HGEConstants at: 'HGE_HWNDPARENT' put: 16r10!
HGEConstants at: 'HGE_ICON' put: 16r1A!
HGEConstants at: 'HGE_INIFILE' put: 16r1C!
HGEConstants at: 'HGE_LOGFILE' put: 16r1D!
HGEConstants at: 'HGE_MUSVOLUME' put: 16r16!
HGEConstants at: 'HGE_POWERSTATUS' put: 16r19!
HGEConstants at: 'HGE_RENDERFUNC' put: 16r9!
HGEConstants at: 'HGE_SAMPLERATE' put: 16r14!
HGEConstants at: 'HGE_SCREENBPP' put: 16r13!
HGEConstants at: 'HGE_SCREENHEIGHT' put: 16r12!
HGEConstants at: 'HGE_SCREENWIDTH' put: 16r11!
HGEConstants at: 'HGE_SHOWSPLASH' put: 16r7!
HGEConstants at: 'HGE_STREAMVOLUME' put: 16r17!
HGEConstants at: 'HGE_TEXTUREFILTER' put: 16r3!
HGEConstants at: 'HGE_TITLE' put: 16r1B!
HGEConstants at: 'HGE_USESOUND' put: 16r4!
HGEConstants at: 'HGE_VERSION' put: 16r181!
HGEConstants at: 'HGE_WINDOWED' put: 16r1!
HGEConstants at: 'HGE_ZBUFFER' put: 16r2!
HGEConstants at: 'HGEDISP_CENTER' put: 16r2!
HGEConstants at: 'HGEDISP_NODE' put: 16r0!
HGEConstants at: 'HGEDISP_TOPLEFT' put: 16r1!
HGEConstants at: 'HGEFPS_UNLIMITED' put: 16r0!
HGEConstants at: 'HGEFPS_VSYNC' put: -16r1!
HGEConstants at: 'HGEINP_ALT' put: 16r4!
HGEConstants at: 'HGEINP_CAPSLOCK' put: 16r8!
HGEConstants at: 'HGEINP_CTRL' put: 16r2!
HGEConstants at: 'HGEINP_NUMLOCK' put: 16r20!
HGEConstants at: 'HGEINP_REPEAT' put: 16r40!
HGEConstants at: 'HGEINP_SCROLLLOCK' put: 16r10!
HGEConstants at: 'HGEINP_SHIFT' put: 16r1!
HGEConstants at: 'HGEK_0' put: 16r30!
HGEConstants at: 'HGEK_1' put: 16r31!
HGEConstants at: 'HGEK_2' put: 16r32!
HGEConstants at: 'HGEK_3' put: 16r33!
HGEConstants at: 'HGEK_4' put: 16r34!
HGEConstants at: 'HGEK_5' put: 16r35!
HGEConstants at: 'HGEK_6' put: 16r36!
HGEConstants at: 'HGEK_7' put: 16r37!
HGEConstants at: 'HGEK_8' put: 16r38!
HGEConstants at: 'HGEK_9' put: 16r39!
HGEConstants at: 'HGEK_A' put: 16r41!
HGEConstants at: 'HGEK_ADD' put: 16r6B!
HGEConstants at: 'HGEK_ALT' put: 16r12!
HGEConstants at: 'HGEK_APOSTROPHE' put: 16rDE!
HGEConstants at: 'HGEK_APPS' put: 16r5D!
HGEConstants at: 'HGEK_B' put: 16r42!
HGEConstants at: 'HGEK_BACKSLASH' put: 16rDC!
HGEConstants at: 'HGEK_BACKSPACE' put: 16r8!
HGEConstants at: 'HGEK_C' put: 16r43!
HGEConstants at: 'HGEK_CAPSLOCK' put: 16r14!
HGEConstants at: 'HGEK_COMMA' put: 16rBC!
HGEConstants at: 'HGEK_CTRL' put: 16r11!
HGEConstants at: 'HGEK_D' put: 16r44!
HGEConstants at: 'HGEK_DECIMAL' put: 16r6E!
HGEConstants at: 'HGEK_DELETE' put: 16r2E!
HGEConstants at: 'HGEK_DIVIDE' put: 16r6F!
HGEConstants at: 'HGEK_DOWN' put: 16r28!
HGEConstants at: 'HGEK_E' put: 16r45!
HGEConstants at: 'HGEK_END' put: 16r23!
HGEConstants at: 'HGEK_ENTER' put: 16rD!
HGEConstants at: 'HGEK_EQUALS' put: 16rBB!
HGEConstants at: 'HGEK_ESCAPE' put: 16r1B!
HGEConstants at: 'HGEK_F' put: 16r46!
HGEConstants at: 'HGEK_F1' put: 16r70!
HGEConstants at: 'HGEK_F10' put: 16r79!
HGEConstants at: 'HGEK_F11' put: 16r7A!
HGEConstants at: 'HGEK_F12' put: 16r7B!
HGEConstants at: 'HGEK_F2' put: 16r71!
HGEConstants at: 'HGEK_F3' put: 16r72!
HGEConstants at: 'HGEK_F4' put: 16r73!
HGEConstants at: 'HGEK_F5' put: 16r74!
HGEConstants at: 'HGEK_F6' put: 16r75!
HGEConstants at: 'HGEK_F7' put: 16r76!
HGEConstants at: 'HGEK_F8' put: 16r77!
HGEConstants at: 'HGEK_F9' put: 16r78!
HGEConstants at: 'HGEK_G' put: 16r47!
HGEConstants at: 'HGEK_GRAVE' put: 16rC0!
HGEConstants at: 'HGEK_H' put: 16r48!
HGEConstants at: 'HGEK_HOME' put: 16r24!
HGEConstants at: 'HGEK_I' put: 16r49!
HGEConstants at: 'HGEK_INSERT' put: 16r2D!
HGEConstants at: 'HGEK_J' put: 16r4A!
HGEConstants at: 'HGEK_K' put: 16r4B!
HGEConstants at: 'HGEK_L' put: 16r4C!
HGEConstants at: 'HGEK_LBRACKET' put: 16rDB!
HGEConstants at: 'HGEK_LBUTTON' put: 16r1!
HGEConstants at: 'HGEK_LEFT' put: 16r25!
HGEConstants at: 'HGEK_LWIN' put: 16r5B!
HGEConstants at: 'HGEK_M' put: 16r4D!
HGEConstants at: 'HGEK_MBUTTON' put: 16r4!
HGEConstants at: 'HGEK_MINUS' put: 16rBD!
HGEConstants at: 'HGEK_MULTIPLY' put: 16r6A!
HGEConstants at: 'HGEK_N' put: 16r4E!
HGEConstants at: 'HGEK_NUMLOCK' put: 16r90!
HGEConstants at: 'HGEK_NUMPAD0' put: 16r60!
HGEConstants at: 'HGEK_NUMPAD1' put: 16r61!
HGEConstants at: 'HGEK_NUMPAD2' put: 16r62!
HGEConstants at: 'HGEK_NUMPAD3' put: 16r63!
HGEConstants at: 'HGEK_NUMPAD4' put: 16r64!
HGEConstants at: 'HGEK_NUMPAD5' put: 16r65!
HGEConstants at: 'HGEK_NUMPAD6' put: 16r66!
HGEConstants at: 'HGEK_NUMPAD7' put: 16r67!
HGEConstants at: 'HGEK_NUMPAD8' put: 16r68!
HGEConstants at: 'HGEK_NUMPAD9' put: 16r69!
HGEConstants at: 'HGEK_O' put: 16r4F!
HGEConstants at: 'HGEK_P' put: 16r50!
HGEConstants at: 'HGEK_PAUSE' put: 16r13!
HGEConstants at: 'HGEK_PERIOD' put: 16rBE!
HGEConstants at: 'HGEK_PGDN' put: 16r22!
HGEConstants at: 'HGEK_PGUP' put: 16r21!
HGEConstants at: 'HGEK_Q' put: 16r51!
HGEConstants at: 'HGEK_R' put: 16r52!
HGEConstants at: 'HGEK_RBRACKET' put: 16rDD!
HGEConstants at: 'HGEK_RBUTTON' put: 16r2!
HGEConstants at: 'HGEK_RIGHT' put: 16r27!
HGEConstants at: 'HGEK_RWIN' put: 16r5C!
HGEConstants at: 'HGEK_S' put: 16r53!
HGEConstants at: 'HGEK_SCROLLLOCK' put: 16r91!
HGEConstants at: 'HGEK_SEMICOLON' put: 16rBA!
HGEConstants at: 'HGEK_SHIFT' put: 16r10!
HGEConstants at: 'HGEK_SLASH' put: 16rBF!
HGEConstants at: 'HGEK_SPACE' put: 16r20!
HGEConstants at: 'HGEK_SUBTRACT' put: 16r6D!
HGEConstants at: 'HGEK_T' put: 16r54!
HGEConstants at: 'HGEK_TAB' put: 16r9!
HGEConstants at: 'HGEK_U' put: 16r55!
HGEConstants at: 'HGEK_UP' put: 16r26!
HGEConstants at: 'HGEK_V' put: 16r56!
HGEConstants at: 'HGEK_W' put: 16r57!
HGEConstants at: 'HGEK_X' put: 16r58!
HGEConstants at: 'HGEK_Y' put: 16r59!
HGEConstants at: 'HGEK_Z' put: 16r5A!
HGEConstants at: 'HGEPRIM_LINES' put: 16r2!
HGEConstants at: 'HGEPRIM_QUADS' put: 16r4!
HGEConstants at: 'HGEPRIM_TRIPLES' put: 16r3!
HGEConstants at: 'HGEPWR_AC' put: -16r1!
HGEConstants at: 'HGEPWR_UNSUPPORTED' put: -16r2!
HGEConstants at: 'HGETEXT_BOTTOM' put: 16r4!
HGEConstants at: 'HGETEXT_CENTER' put: 16r2!
HGEConstants at: 'HGETEXT_HORZMASK' put: 16r3!
HGEConstants at: 'HGETEXT_LEFT' put: 16r0!
HGEConstants at: 'HGETEXT_MIDDLE' put: 16r8!
HGEConstants at: 'HGETEXT_RIGHT' put: 16r1!
HGEConstants at: 'HGETEXT_TOP' put: 16r0!
HGEConstants at: 'HGETEXT_VERTMASK' put: 16rC!
HGEConstants at: 'INPUT_KEYDOWN' put: 16r1!
HGEConstants at: 'INPUT_KEYUP' put: 16r2!
HGEConstants at: 'INPUT_MBUTTONDOWN' put: 16r3!
HGEConstants at: 'INPUT_MBUTTONUP' put: 16r4!
HGEConstants at: 'INPUT_MOUSEMOVE' put: 16r5!
HGEConstants at: 'INPUT_MOUSEWHEEL' put: 16r6!
HGEConstants shrink!

"Classes"!

RandomSprite guid: (GUID fromString: '{27656040-E1A3-42FF-AA65-8AA1C76E2F75}')!
RandomSprite comment: 'RandomSprite is a helper class for tutorial 07.'!
!RandomSprite categoriesForClass!Kernel-Objects! !
!RandomSprite methodsFor!

color
	"Answer the receiver's color instance variable."

	^color!

color: anARGB 
	"Set the receiver's color instance variable to anARGB."

	color := anARGB!

deltaPosition
	"Answer the receiver's deltaPosition instance variable."

	^deltaPosition!

deltaPosition: aPoint 
	"Set the receiver's deltaPosition instance variable to aPoint."

	deltaPosition := aPoint!

deltaRotation
	"Answer the receiver's deltaRotation instance variable."

	^deltaRotation!

deltaRotation: aFloat 
	"Set the receiver's deltaRotation instance variable to aFloat."

	deltaRotation := aFloat!

deltaScale
	"Answer the receiver's deltaScale instance variable."

	^deltaScale!

deltaScale: aFloat 
	"Set the receiver's deltaScale instance variable to aFloat."

	deltaScale := aFloat!

initialize
	"Private - Initialize the receiver."

	| random |
	random := HGERandom new.
	position := (random between: 0 and: 800) next @ (random between: 0 and: 600) next.
	deltaPosition := (random between: -200 and: 200) next @ random next.
	scale := (random between: 0.5 and: 2.0) next.
	deltaScale := (random between: -1.0 and: 1.0) next.
	rotation := (random between: 0 and: Float pi * 2) next.
	deltaRotation := (random between: -1.0 and: 1.0) next!

position
	"Answer the receiver's position instance variable."

	^position!

position: aPoint 
	"Set the receiver's position instance variable to aPoint."

	position := aPoint!

render
	"Render the receiver's sprite."

	sprite color: color.
	sprite 
		renderAt: position
		rotatedBy: rotation
		scaledBy: scale @ 0!

rotation
	"Answer the receiver's rotation instance variable."

	^rotation!

rotation: aFloat 
	"Set the receiver's rotation instance variable to aFloat."

	rotation := aFloat!

scale
	"Answer the receiver's scale instance variable."

	^scale!

scale: aFloat 
	"Set the receiver's scale instance variable to aFloat."

	scale := aFloat!

sprite: anHGESprite 
	"Private - Set the receiver's sprite."

	sprite := anHGESprite! !
!RandomSprite categoriesFor: #color!accessing!public! !
!RandomSprite categoriesFor: #color:!accessing!public! !
!RandomSprite categoriesFor: #deltaPosition!accessing!public! !
!RandomSprite categoriesFor: #deltaPosition:!accessing!public! !
!RandomSprite categoriesFor: #deltaRotation!accessing!public! !
!RandomSprite categoriesFor: #deltaRotation:!accessing!public! !
!RandomSprite categoriesFor: #deltaScale!accessing!public! !
!RandomSprite categoriesFor: #deltaScale:!accessing!public! !
!RandomSprite categoriesFor: #initialize!initializing!private! !
!RandomSprite categoriesFor: #position!accessing!public! !
!RandomSprite categoriesFor: #position:!accessing!public! !
!RandomSprite categoriesFor: #render!public! !
!RandomSprite categoriesFor: #rotation!accessing!public! !
!RandomSprite categoriesFor: #rotation:!accessing!public! !
!RandomSprite categoriesFor: #scale!accessing!public! !
!RandomSprite categoriesFor: #scale:!accessing!public! !
!RandomSprite categoriesFor: #sprite:!accessing!private! !

!RandomSprite class methodsFor!

for: anHGESprite 
	"Answer an instance of the receiver to represent anHGESprite with random rotation,
	scaling and screen position."

	^super new initialize sprite: anHGESprite!

new
	"Should not implement. Use #for:"

	^self shouldNotImplement! !
!RandomSprite class categoriesFor: #for:!instance creation!public! !
!RandomSprite class categoriesFor: #new!instance creation!public! !

HGEError guid: (GUID fromString: '{2CF9A009-307D-4146-9415-E37B7130E4FF}')!
HGEError comment: ''!
!HGEError categoriesForClass!Kernel-Exception Handling! !
HGELibrary guid: (GUID fromString: '{25548621-4E79-4CA7-B8FB-6F30BA5A2054}')!
HGELibrary comment: ''!
!HGELibrary categoriesForClass!External-Libraries! !
!HGELibrary methodsFor!

hgeColorRGBCreate
	"Creates an HGEColorRGB object whose four component colour values are 0.0.

		hgeColorRGB* hgeColorRGBCreate();"

	<stdcall: HGEColorRGB* hgeColorRGBCreate>
	^self invalidCall!

hgeColorRGBCreateFromInteger: col 
	"Creates an HGEColorRGB object from the integer col.

		hgeColorRGB* hgeColorRGBCreateFromInteger(
			DWORD col
		);"

	<stdcall: HGEColorRGB* hgeColorRGBCreateFromInteger dword>
	^self invalidCall!

hgeColorRGBCreateFromRGBA: r g: g b: b a: a 
	"Creates an HGEColorRGB object whose four component colour values are given.
	Each component is expected to be between 0.0 and 1.1.

		hgeColorRGB* hgeColorRGBCreateFromRGBA(
			float _r, float _g, float _b, float _a
		);"

	<stdcall: HGEColorRGB* hgeColorRGBCreateFromRGBA float float float float>
	^self invalidCall!

hgeColorRGBDestroy: col 
	"Calls the col C++ destructor.

		void hgeColorRGBDestroy(
			hgeColorRGB *col
		);"

	<stdcall: void hgeColorRGBDestroy HGEColorRGB*>
	^self invalidCall!

hgeCreate: hgeVersion 
	"Creates an HGE object if needed and returns a pointer to the HGE interface.

		HGE* hgeCreate(
			int ver
		);"

	<stdcall: HGE* hgeCreate sdword>
	^self invalidCall!

hgeDistortionMeshCreateFromDistortionMesh: dm 
	"Creates an HGEDistortionMesh object from a distortion mesh.

		hgeDistortionMesh* hgeDistortionMesh(
			const hgeDistortionMesh &dm
		);"

	<stdcall: HGEDistortionMesh* hgeDistortionMeshCreateFromDistortionMesh HGEDistortionMesh*>
	^self invalidCall!

hgeDistortionMeshCreateWithColumns: cols rows: rows 
	"Creates an HGEDistortionMesh object with the specified number of mesh columns
	and rows.

		hgeDistortionMesh* hgeDistortionMesh(
			int cols,
			int rows
		);"

	<stdcall: HGEDistortionMesh* hgeDistortionMeshCreateWithColumns sdword sdword>
	^self invalidCall!

hgeDistortionMeshDestroy: dm 
	"Calls the dm C++ destructor.

		void hgeDistortionMeshDestroy(
			hgeDistortionMesh *dm
		);"

	<stdcall: void hgeDistortionMeshDestroy HGEDistortionMesh*>
	^self invalidCall!

hgeFontCreate: filename bMipmap: bMipmap 
	"Creates an HGEFont object from the file named filename.

		hgeFont* hgeFontCreate(
			const char *filename,
			bool bMipmap=false
		);"

	<stdcall: HGEFont* hgeFontCreate char* bool>
	^self invalidCall!

hgeFontDestroy: fnt 
	"Calls the fnt C++ destructor.

		void hgeFontDestroy(
			hgeFont *fnt
		);"

	<stdcall: void hgeFontDestroy HGEFont*>
	^self invalidCall!

hgeParticleSystemCreateFromFile: filename sprite: sprite 
	"Creates an HGEParticleSystem object from the file named filename (containing the
	particle system description) and a sprite to use for the particle system.

		hgeParticleSystem* hgeParticleSystemCreateFromFile(
			const char *filename,
			hgeSprite *sprite
		);"

	<stdcall: HGEParticleSystem* hgeParticleSystemCreateFromFile char* HGESprite*>
	^self invalidCall!

hgeParticleSystemCreateFromParticleSystem: ps 
	"Creates an HGEParticleSystem object from a particle system.

		hgeParticleSystem* hgeParticleSystemCreateFromParticleSystem(
			const hgeParticleSystem &ps
		);"

	<stdcall: HGEParticleSystem* hgeParticleSystemCreateFromParticleSystem HGEParticleSystem*>
	^self invalidCall!

hgeParticleSystemCreateFromParticleSystemInfo: psi 
	"Creates an HGEParticleSystem object from an HGEParticleSystemInfo describing
	the particle system.

		hgeParticleSystem* hgeParticleSystemCreateFromParticleSystemInfo(
			hgeParticleSystemInfo *psi
		);"

	<stdcall: HGEParticleSystem* hgeParticleSystemCreateFromParticleSystemInfo HGEParticleSystemInfo*>
	^self invalidCall!

hgeParticleSystemDestroy: ps 
	"Calls the ps C++ destructor.

		void hgeParticleSystemDestroy(
			hgeParticleSystem *ps
		);"

	<stdcall: void hgeParticleSystemDestroy HGEParticleSystem*>
	^self invalidCall!

hgeSpriteCreateFromSprite: spr 
	"Creates an HGESprite object from a sprite.

		hgeSprite* hgeSpriteCreateFromSprite(
			const hgeSprite &spr
		);"

	<stdcall: HGESprite* hgeSpriteCreateFromSprite HGESprite*>
	^self invalidCall!

hgeSpriteCreateFromTexture: tex x: x y: y w: w h: h 
	"Creates an HGESprite object from a texture.

		hgeSprite* hgeSpriteCreateFromTexture(
			HTEXTURE tex,
			float x,
			float y,
			float w,
			float h
		);"

	<stdcall: HGESprite* hgeSpriteCreateFromTexture HGETexture float float float float>
	^self invalidCall!

hgeSpriteDestroy: spr 
	"Calls the spr C++ destructor.

		void hgeSpriteDestroy(
			hgeSprite *spr
		);"

	<stdcall: void hgeSpriteDestroy HGESprite*>
	^self invalidCall! !
!HGELibrary categoriesFor: #hgeColorRGBCreate!public! !
!HGELibrary categoriesFor: #hgeColorRGBCreateFromInteger:!public! !
!HGELibrary categoriesFor: #hgeColorRGBCreateFromRGBA:g:b:a:!public! !
!HGELibrary categoriesFor: #hgeColorRGBDestroy:!public! !
!HGELibrary categoriesFor: #hgeCreate:!public! !
!HGELibrary categoriesFor: #hgeDistortionMeshCreateFromDistortionMesh:!public! !
!HGELibrary categoriesFor: #hgeDistortionMeshCreateWithColumns:rows:!public! !
!HGELibrary categoriesFor: #hgeDistortionMeshDestroy:!public! !
!HGELibrary categoriesFor: #hgeFontCreate:bMipmap:!public! !
!HGELibrary categoriesFor: #hgeFontDestroy:!public! !
!HGELibrary categoriesFor: #hgeParticleSystemCreateFromFile:sprite:!public! !
!HGELibrary categoriesFor: #hgeParticleSystemCreateFromParticleSystem:!public! !
!HGELibrary categoriesFor: #hgeParticleSystemCreateFromParticleSystemInfo:!public! !
!HGELibrary categoriesFor: #hgeParticleSystemDestroy:!public! !
!HGELibrary categoriesFor: #hgeSpriteCreateFromSprite:!public! !
!HGELibrary categoriesFor: #hgeSpriteCreateFromTexture:x:y:w:h:!public! !
!HGELibrary categoriesFor: #hgeSpriteDestroy:!public! !

!HGELibrary class methodsFor!

fileName
	^'C:\Documents and Settings\Administrator\My Documents\Visual Studio 2008\Projects\hge181\hge.dll'! !
!HGELibrary class categoriesFor: #fileName!public! !

HGE guid: (GUID fromString: '{722FCD0C-8858-41B2-B11D-FA8C50A14162}')!
HGE comment: ''!
!HGE categoriesForClass!External-Data-Structured! !
!HGE methodsFor!

basicFree
	"Private - Free external resources owned by the receiver."

	Transcript
		show: 'HGE>>basicFree';
		cr.
	self release!

channelGetLength: channel 
	"Retrieves the total length of a playing channel.

		float Channel_GetLength(
			HCHANNEL channel
		);"

	<virtual stdcall: float 68 HGEChannel>
	^self invalidCall!

channelGetPos: channel 
	"Retrieves a playing channel's position.

		float Channel_GetPos(
			HCHANNEL channel
		);"

	<virtual stdcall: float 69 HGEChannel>
	^self invalidCall!

channelIsPlaying: channel 
	"Tests if an audio channel is currently playing.

		bool Channel_IsPlaying(
			HCHANNEL channel
		);"

	<virtual stdcall: bool 67 HGEChannel>
	^self invalidCall!

channelIsSliding: channel 
	"Tests if a channel parameters are sliding.

		bool Channel_IsSliding(
			HCHANNEL channel
		);"

	<virtual stdcall: bool 72 HGEChannel>
	^self invalidCall!

channelPause: channel 
	"Pauses an audio channel.

		void Channel_Pause(
			HCHANNEL channel
		);"

	<virtual stdcall: void 61 HGEChannel>
	^self invalidCall!

channelPauseAll
	"Pauses all active audio channels.

		void Channel_PauseAll();"

	<virtual stdcall: void 64>
	^self invalidCall!

channelResume: channel 
	"Resumes a paused audio channel.

		void Channel_Resume(
			HCHANNEL channel
		);"

	<virtual stdcall: void 62 HGEChannel>
	^self invalidCall!

channelResumeAll
	"Resumes all active audio channels.

		void Channel_ResumeAll();"

	<virtual stdcall: void 65>
	^self invalidCall!

channelSetPanning: channel pan: pan 
	"Changes an audio channel panning.

		void Channel_SetPanning(
			HCHANNEL channel,
			int pan
		);"

	<virtual stdcall: void 58 HGEChannel sdword>
	^self invalidCall!

channelSetPitch: channel pitch: pitch 
	"Changes an audio channel pitch.

		void Channel_SetPitch(
			HCHANNEL channel,
			float pitch
		);"

	<virtual stdcall: void 60 HGEChannel float>
	^self invalidCall!

channelSetPos: channel fSeconds: fSeconds 
	"Skips forwards or backwards in a playing channel.

		void Channel_SetPos(
			HCHANNEL channel,
			float fSeconds
		);"

	<virtual stdcall: void 70 HGEChannel float>
	^self invalidCall!

channelSetVolume: channel volume: volume 
	"Changes an audio channel volume.

		void Channel_SetVolume(
			HCHANNEL channel,
			int volume
		);"

	<virtual stdcall: void 59 HGEChannel sdword>
	^self invalidCall!

channelSlideTo: channel time: time volume: volume pan: pan pitch: pitch 
	"Starts sliding a channel volume, panning or pitch.

		void Channel_SlideTo(
			HCHANNEL channel,
			float time,
			int volume,
			int pan = -101,
			float pitch = -1
		);"

	<virtual stdcall: void 71 HGEChannel float sdword sdword float>
	^self invalidCall!

channelStop: channel 
	"Stops an audio channel.

		void Channel_Stop(
			HCHANNEL channel
		);"

	<virtual stdcall: void 63 HGEChannel>
	^self invalidCall!

channelStopAll
	"Stops all active audio channels.

		void Channel_StopAll();"

	<virtual stdcall: void 66>
	^self invalidCall!

effectFree: effect 
	"Deletes loaded sound effect and frees associated resources.

		void Effect_Free(
			HEFFECT effect
		);"

	<virtual stdcall: void 40 dword>
	^self invalidCall!

effectLoad: filename 
	"From the C++ side the size parameter is optional with a default value of 0:
	If this parameter isn't 0, it is the size of memory block containing the sound effect
	in one of the known formats (WAV, MP3, MP2, MP1 and OGG) and the parameter
	filename is treated as a pointer to this block."

	^self effectLoad: filename size: 0!

effectLoad: filename size: size 
	"Loads a sound effect from memory, resource pack or disk file.
	The filename argument is intentionally void* so we can pass a String, an ExternalAddress,
	a ByteArray or any other byte object.

		HEFFECT Effect_Load(
			const char *filename,
			DWORD size = 0
		);"

	<virtual stdcall: dword 39 void* dword>
	^self invalidCall!

effectPlay: effect 
	"Starts playing a sound effect.
	It is safe to return an HGEChannel instance since those objects are not finalizable.

		HCHANNEL Effect_Play(
			HEFFECT effect
		);"

	<virtual stdcall: HGEChannel 41 dword>
	^self invalidCall!

effectPlayEx: effect volume: volume pan: pan pitch: pitch 
	"From the C++ side the volume, pan, pitch and loop parameters are optional.
	We provide this method as usually you don't want to loop the effect:

	volume
		Optional effect volume from 0 to 100. Default is 100, meaning maximum volume. 

	pan
		Optional effect panning from -100 to 100. Default is 0, meaning center panning. 

	pitch
		Optional effect pitch multiplier. Default is 1.0, meaning default pitch. 

	loop
		Optional parameter. If true, the effect is looped until the audio channel is stopped manually. Default is false."

	^self 
		effectPlayEx: effect
		volume: volume
		pan: pan
		pitch: pitch
		loop: false!

effectPlayEx: effect volume: volume pan: pan pitch: pitch loop: loop 
	"Starts playing a sound effect with additional parameters.
	It is safe to return an HGEChannel instance since those objects are not finalizable.

		HCHANNEL Effect_PlayEx(
			HEFFECT effect,
			int volume = 100,
			int pan = 0,
			float pitch = 1.0,
			bool loop = false
		);"

	<virtual stdcall: HGEChannel 42 dword sdword sdword float bool>
	^self invalidCall!

gfxBeginScene
	"From the C++ side the target parameter is optional with a default value of 0:
	If 0 or omitted - the screen surface is used."

	^self gfxBeginScene: 0!

gfxBeginScene: target 
	"Starts rendering graphics.

		bool Gfx_BeginScene(
			HTARGET target = 0
		);"

	<virtual stdcall: bool 84 dword>
	^self invalidCall!

gfxClear: color 
	"Clears render target (screen or texture) and z-buffer.

		void Gfx_Clear(
			DWORD color
		);"

	<virtual stdcall: void 86 dword>
	^self invalidCall!

gfxEndScene
	"Ends rendering graphics and updates the screen if needed.

		void Gfx_EndScene();"

	<virtual stdcall: void 85>
	^self invalidCall!

gfxRenderQuad: quad 
	"Renders a quad.

		void Gfx_RenderQuad(
			const hgeQuad *quad
		);"

	<virtual stdcall: void 89 HGEQuad*>
	^self invalidCall!

inputGetKey
	"Returns the last key pressed since previous call to frame function.

		int Input_GetKey();"

	<virtual stdcall: sdword 81>
	^self invalidCall!

inputGetKeyState: key 
	"Determines whether a key or mouse button is up or down at the time the function is called.

		bool Input_GetKeyState(
			int key
		);"

	<virtual stdcall: bool 79 sdword>
	^self invalidCall!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^true!

randomFloat: min max: max 
	"Generates float random number.

		float Random_Float(
			float min,
			float max
		);"

	<virtual stdcall: float 35 float float>
	^self invalidCall!

randomInt: min max: max 
	"Generates int random number.

		int Random_Int(
			int min,
			int max
		);"

	<virtual stdcall: sdword 34 sdword sdword>
	^self invalidCall!

randomSeed: seed 
	"Sets random number generator's seed.

		void Random_Seed(
			int seed = 0
		);"

	<virtual stdcall: void 33 sdword>
	^self invalidCall!

release
	"Frees HGE interface and deletes HGE object if needed.

		void Release();"

	<virtual stdcall: void 1>
	^self invalidCall!

systemGetErrorMessage
	"Returns last occured HGE error description.

		char *System_GetErrorMessage();"

	<virtual stdcall: char* 5>
	^self invalidCall!

systemInitiate
	"Initializes all hardware and software needed to run engine and creates application window.

		bool System_Initiate();"

	<virtual stdcall: bool 2>
	^self invalidCall!

systemSetState: state value: value 
	"Wrapper for the various #systemSetState*:value: methods."

	(value isKindOf: Boolean) ifTrue: [^self systemSetStateBool: state value: value].
	(value isKindOf: ExternalAddress) ifTrue: [^self systemSetStateFunc: state value: value].
	(value isKindOf: ExternalHandle) ifTrue: [^self systemSetStateHwnd: state value: value].
	(value isKindOf: Integer) ifTrue: [^self systemSetStateInt: state value: value].
	(value isKindOf: String) ifTrue: [^self systemSetStateString: state value: value].
	HGEError 
		signal: 'Unsupported value object class: ' , value printString , ' (' , value class printString 
				, ')'!

systemSetStateBool: state value: value 
	"Sets internal system states.

		void System_SetStateBool(
			hgeBoolState state,
			bool value
		);"

	<virtual stdcall: void 9 sdword bool>
	^self invalidCall!

systemSetStateFunc: state value: value 
	"Sets internal system states.

		void System_SetStateFunc(
			hgeFuncState state,
			hgeCallback value
		);"

	<virtual stdcall: void 10 sdword lpvoid>
	^self invalidCall!

systemSetStateHwnd: state value: value 
	"Sets internal system states.

		void System_SetStateHwnd(
			hgeHwndState state,
			HWND value
		);"

	<virtual stdcall: void 11 sdword handle>
	^self invalidCall!

systemSetStateInt: state value: value 
	"Sets internal system states.

		void System_SetStateInt(
			hgeIntState state,
			int value
		);"

	<virtual stdcall: void 12 sdword sdword>
	^self invalidCall!

systemSetStateString: state value: value 
	"Sets internal system states.

		void System_SetStateString(
			hgeStringState state,
			const char *value
		);"

	<virtual stdcall: void 13 sdword char*>
	^self invalidCall!

systemShutdown
	"Restores video mode, frees all allocated resources and destroys application window.

		void System_Shutdown();"

	<virtual stdcall: void 3>
	^self invalidCall!

systemStart
	"Starts running user defined frame function.

		bool System_Start();"

	<virtual stdcall: bool 4>
	^self invalidCall!

targetCreate: width height: height zbuffer: zbuffer 
	"Creates a render target.

		HTARGET Target_Create(
			int width,
			int height,
			bool zbuffer
		);"

	<virtual stdcall: dword 94 sdword sdword bool>
	^self invalidCall!

targetFree: target 
	"Deletes a render target and frees associated resources.

		void Target_Free(
			HTARGET target
		);"

	<virtual stdcall: void 95 dword>
	^self invalidCall!

targetGetTexture: target 
	"Returns a render target's texture handle.

		HTEXTURE Target_GetTexture(
			HTARGET target
		);"

	<virtual stdcall: dword 96 dword>
	^self invalidCall!

textureCreate: width height: height 
	"Creates an empty texture.

		HTEXTURE Texture_Create(
			int width,
			int height
		);"

	<virtual stdcall: dword 97 sdword sdword>
	^self invalidCall!

textureFree: texture 
	"Deletes loaded or created texture and frees associated resources.

		void Texture_Free(
			HTEXTURE texture
		);"

	<virtual stdcall: void 99 dword>
	^self invalidCall!

textureGetHeight: texture 
	"From the C++ side the bOriginal parameter is optional with a default value of false:
	If false or omitted, the texture height in video memory is returned.
	If true original image file height is returned."

	^self textureGetHeight: texture bOriginal: false!

textureGetHeight: texture bOriginal: bOriginal 
	"Returns a texture height in pixels.

		int Texture_GetHeight(
			HTEXTURE texture,
			bool bOriginal = false
		);"

	<virtual stdcall: sdword 101 dword bool>
	^self invalidCall!

textureGetWidth: texture 
	"From the C++ side the bOriginal parameter is optional with a default value of false:
	If false or omitted, the texture width in video memory is returned.
	If true original image file width is returned."

	^self textureGetWidth: texture bOriginal: false!

textureGetWidth: texture bOriginal: bOriginal 
	"Returns a texture width in pixels.

		int Texture_GetWidth(
			HTEXTURE texture,
			bool bOriginal = false
		);"

	<virtual stdcall: sdword 100 dword bool>
	^self invalidCall!

textureLoad: filename 
	"From the C++ side the size and bMipmap parameters are optional:

	size
		If this parameter isn't 0, it is the size of memory block containing the texture in one
		of the known formats (BMP, DDS, DIB, JPG, PNG and TGA) and the parameter 
		filename is treated as a pointer to this block.

	bMipmap
		If true, a set of mipmap levels are automatically created and used for rendering."

	^self 
		textureLoad: filename
		size: 0
		bMipmap: false!

textureLoad: filename size: size bMipmap: bMipmap 
	"Loads a texture from memory, resource pack or disk file.
	The filename argument is intentionally void* so we can pass a String, an ExternalAddress,
	a ByteArray or any other byte object.

		HTEXTURE Texture_Load(
			const char *filename,
			DWORD size = 0,
			bool bMipmap = false
		);"

	<virtual stdcall: dword 98 void* dword bool>
	^self invalidCall!

timerGetDelta
	"Returns time elapsed since last frame function call.

		float Timer_GetDelta();"

	<virtual stdcall: float 37>
	^self invalidCall!

timerGetFPS
	"Answer the current frames-per-second rate.

		int Timer_GetFPS();"

	<virtual stdcall: sdword 38>
	^self invalidCall! !
!HGE categoriesFor: #basicFree!private!realizing/unrealizing! !
!HGE categoriesFor: #channelGetLength:!audio channel!public! !
!HGE categoriesFor: #channelGetPos:!audio channel!public! !
!HGE categoriesFor: #channelIsPlaying:!audio channel!public! !
!HGE categoriesFor: #channelIsSliding:!audio channel!public! !
!HGE categoriesFor: #channelPause:!audio channel!public! !
!HGE categoriesFor: #channelPauseAll!audio channel!public! !
!HGE categoriesFor: #channelResume:!audio channel!public! !
!HGE categoriesFor: #channelResumeAll!audio channel!public! !
!HGE categoriesFor: #channelSetPanning:pan:!audio channel!public! !
!HGE categoriesFor: #channelSetPitch:pitch:!audio channel!public! !
!HGE categoriesFor: #channelSetPos:fSeconds:!audio channel!public! !
!HGE categoriesFor: #channelSetVolume:volume:!audio channel!public! !
!HGE categoriesFor: #channelSlideTo:time:volume:pan:pitch:!audio channel!public! !
!HGE categoriesFor: #channelStop:!audio channel!public! !
!HGE categoriesFor: #channelStopAll!audio channel!public! !
!HGE categoriesFor: #effectFree:!public!sound effect! !
!HGE categoriesFor: #effectLoad:!public!sound effect! !
!HGE categoriesFor: #effectLoad:size:!public!sound effect! !
!HGE categoriesFor: #effectPlay:!public!sound effect! !
!HGE categoriesFor: #effectPlayEx:volume:pan:pitch:!public!sound effect! !
!HGE categoriesFor: #effectPlayEx:volume:pan:pitch:loop:!public!sound effect! !
!HGE categoriesFor: #gfxBeginScene!graphics!public! !
!HGE categoriesFor: #gfxBeginScene:!graphics!public! !
!HGE categoriesFor: #gfxClear:!graphics!public! !
!HGE categoriesFor: #gfxEndScene!graphics!public! !
!HGE categoriesFor: #gfxRenderQuad:!graphics!public! !
!HGE categoriesFor: #inputGetKey!input!public! !
!HGE categoriesFor: #inputGetKeyState:!input!public! !
!HGE categoriesFor: #needsFree!private!realizing/unrealizing! !
!HGE categoriesFor: #randomFloat:max:!public!random number generation! !
!HGE categoriesFor: #randomInt:max:!public!random number generation! !
!HGE categoriesFor: #randomSeed:!public!random number generation! !
!HGE categoriesFor: #release!interface!public! !
!HGE categoriesFor: #systemGetErrorMessage!public!system! !
!HGE categoriesFor: #systemInitiate!public!system! !
!HGE categoriesFor: #systemSetState:value:!public!system! !
!HGE categoriesFor: #systemSetStateBool:value:!private!system! !
!HGE categoriesFor: #systemSetStateFunc:value:!private!system! !
!HGE categoriesFor: #systemSetStateHwnd:value:!private!system! !
!HGE categoriesFor: #systemSetStateInt:value:!private!system! !
!HGE categoriesFor: #systemSetStateString:value:!private!system! !
!HGE categoriesFor: #systemShutdown!public!system! !
!HGE categoriesFor: #systemStart!public!system! !
!HGE categoriesFor: #targetCreate:height:zbuffer:!public!render target! !
!HGE categoriesFor: #targetFree:!public!render target! !
!HGE categoriesFor: #targetGetTexture:!public!render target! !
!HGE categoriesFor: #textureCreate:height:!public!texture! !
!HGE categoriesFor: #textureFree:!public!texture! !
!HGE categoriesFor: #textureGetHeight:!public!texture! !
!HGE categoriesFor: #textureGetHeight:bOriginal:!public!texture! !
!HGE categoriesFor: #textureGetWidth:!public!texture! !
!HGE categoriesFor: #textureGetWidth:bOriginal:!public!texture! !
!HGE categoriesFor: #textureLoad:!public!texture! !
!HGE categoriesFor: #textureLoad:size:bMipmap:!public!texture! !
!HGE categoriesFor: #timerGetDelta!public!timer! !
!HGE categoriesFor: #timerGetFPS!public!timer! !

!HGE class methodsFor!

new
	"Answer a new instance of the receiver.
	Here we use Finalization and Object Liberation Strategy, but instead you may call HGELibrary>>hgeCreate:
	every time you need access to HGE. Just be sure to have a corresponding HGE>>release for each call to
	HGELibrary>>hgeCreate:"

	^(self fromAddress: (HGELibrary default hgeCreate: HGE_VERSION) asParameter)
		beFinalizable;
		yourself! !
!HGE class categoriesFor: #new!instance creation!public! !

HGEColorRGB guid: (GUID fromString: '{E21FEA6C-B1F3-4736-8B8B-C0002F6089C3}')!
HGEColorRGB comment: ''!
!HGEColorRGB categoriesForClass!External-Data-Structured! !
!HGEColorRGB methodsFor!

a
	"Answer the receiver's a field as a Smalltalk object."

	^(bytes floatAtOffset: 16)!

a: anObject
	"Set the receiver's a field to the value of anObject."

	bytes floatAtOffset: 16 put: anObject!

alpha
	"Answer the receiver's alpha component (a SmallInteger in the range 0..255)."

	^(self a * 255) asInteger!

alpha: anInteger 
	"Set the receiver's alpha component."

	^self a: ((anInteger / 255 min: 1.0) max: 0.0) asFloat!

b
	"Answer the receiver's b field as a Smalltalk object."

	^(bytes floatAtOffset: 12)!

b: anObject
	"Set the receiver's b field to the value of anObject."

	bytes floatAtOffset: 12 put: anObject!

basicFree
	"Private - Free external resources owned by the receiver."

	Transcript
		show: 'HGEColorRGB>>basicFree';
		cr.
	HGELibrary default hgeColorRGBDestroy: self!

blue
	"Answer the receiver's blue component (a SmallInteger in the range 0..255)."

	^(self b * 255) asInteger!

blue: anInteger 
	"Set the receiver's blue component."

	^self b: ((anInteger / 255 min: 1.0) max: 0.0) asFloat!

g
	"Answer the receiver's g field as a Smalltalk object."

	^(bytes floatAtOffset: 8)!

g: anObject
	"Set the receiver's g field to the value of anObject."

	bytes floatAtOffset: 8 put: anObject!

green
	"Answer the receiver's green component (a SmallInteger in the range 0..255)."

	^(self g * 255) asInteger!

green: anInteger 
	"Set the receiver's green component."

	^self g: ((anInteger / 255 min: 1.0) max: 0.0) asFloat!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^true!

r
	"Answer the receiver's r field as a Smalltalk object."

	^(bytes floatAtOffset: 4)!

r: anObject
	"Set the receiver's r field to the value of anObject."

	bytes floatAtOffset: 4 put: anObject!

red
	"Answer the receiver's red component (a SmallInteger in the range 0..255)."

	^(self r * 255) asInteger!

red: anInteger 
	"Set the receiver's red component."

	^self r: ((anInteger / 255 min: 1.0) max: 0.0) asFloat! !
!HGEColorRGB categoriesFor: #a!**compiled accessors**!private! !
!HGEColorRGB categoriesFor: #a:!**compiled accessors**!private! !
!HGEColorRGB categoriesFor: #alpha!accessing!public! !
!HGEColorRGB categoriesFor: #alpha:!accessing!public! !
!HGEColorRGB categoriesFor: #b!**compiled accessors**!private! !
!HGEColorRGB categoriesFor: #b:!**compiled accessors**!private! !
!HGEColorRGB categoriesFor: #basicFree!private!realizing/unrealizing! !
!HGEColorRGB categoriesFor: #blue!accessing!public! !
!HGEColorRGB categoriesFor: #blue:!accessing!public! !
!HGEColorRGB categoriesFor: #g!**compiled accessors**!private! !
!HGEColorRGB categoriesFor: #g:!**compiled accessors**!private! !
!HGEColorRGB categoriesFor: #green!accessing!public! !
!HGEColorRGB categoriesFor: #green:!accessing!public! !
!HGEColorRGB categoriesFor: #needsFree!private!realizing/unrealizing! !
!HGEColorRGB categoriesFor: #r!**compiled accessors**!private! !
!HGEColorRGB categoriesFor: #r:!**compiled accessors**!private! !
!HGEColorRGB categoriesFor: #red!accessing!public! !
!HGEColorRGB categoriesFor: #red:!accessing!public! !

!HGEColorRGB class methodsFor!

a: alphaValue r: redValue g: greenValue b: blueValue 
	"Answer an instance whose four component colour values are given.
	Each component is expected to be between 0.0 and 1.1."

	^(self 
		fromAddress: (HGELibrary default 
				hgeColorRGBCreateFromRGBA: redValue
				g: greenValue
				b: blueValue
				a: alphaValue) asParameter)
		beFinalizable;
		yourself!

alpha: alphaValue red: redValue green: greenValue blue: blueValue 
	"Answer an instance whose four component colour values are given."

	^self 
		a: ((alphaValue / 255 min: 1.0) max: 0.0) asFloat
		r: ((redValue / 255 min: 1.0) max: 0.0) asFloat
		g: ((greenValue / 255 min: 1.0) max: 0.0) asFloat
		b: ((blueValue / 255 min: 1.0) max: 0.0) asFloat!

applicationWorkspace
	"Answer the background colour for an application's workspace area (e.g. the background of
	the main window MDI applications). This is typically a dark grey."

	^self fromARGB: ARGB applicationWorkspace!

black
	"Answer an instance respresenting black."

	^self fromARGB: ARGB black!

blue
	"Answer an instance respresenting blue."

	^self fromARGB: ARGB blue!

brown
	"Answer an instance respresenting brown."

	^self fromARGB: ARGB brown!

buttonFace
	^self fromARGB: ARGB buttonFace!

choose
	"Answer a new instance of the receiver chosen from the common color dialog."

	^self fromARGB: ARGB choose!

cyan
	"Answer an instance respresenting cyan."

	^self fromARGB: ARGB cyan!

darkBlue
	"Answer an instance respresenting darkBlue."

	^self fromARGB: ARGB darkBlue!

darkCyan
	"Answer an instance respresenting dark cyan."

	^self fromARGB: ARGB darkCyan!

darkGray
	"Answer an instance respresenting darkGray."

	^self fromARGB: ARGB darkGray!

darkGreen
	"Answer an instance respresenting darkGreen."

	^self fromARGB: ARGB darkGreen!

darkMagenta
	"Answer an instance respresenting darkMagenta."

	^self fromARGB: ARGB darkMagenta!

darkRed
	"Answer an instance respresenting darkRed."

	^self fromARGB: ARGB darkRed!

darkShadow3d
	"Answer the 3D shadow system color."

	^self fromARGB: ARGB darkShadow3d!

default
	"Answer the special color respresenting the default colour."

	^self fromARGB: ARGB default!

defineFields
	"Class data members:

		float	r,g,b,a;

		HGEColorRGB compileDefinition"

	self
		defineField: #r
			type: FLOATField new
			offset: 4;
		defineField: #g
			type: FLOATField new
			offset: 8;
		defineField: #b
			type: FLOATField new
			offset: 12;
		defineField: #a
			type: FLOATField new
			offset: 16!

dialog
	"Answer the standard dialog background system color."

	^self fromARGB: ARGB dialog!

face3d
	"Answer the 3D button/menu face system color."

	^self fromARGB: ARGB face3d!

fromARGB: anARGB 
	"Answer an instance of the receiver from anARGB."

	^self fromInteger: anARGB!

fromArray: anArray 
	"Answer an instance whose four components are given as #(alpha red green blue)."

	^self 
		alpha: anArray first
		red: anArray second
		green: anArray third
		blue: anArray fourth!

fromInteger: anInteger 
	"Answer an instance with the specified code value."

	^(self 
		fromAddress: (HGELibrary default hgeColorRGBCreateFromInteger: anInteger asDword) asParameter)
		beFinalizable;
		yourself!

fromNormalizedArray: anArray 
	"Answer an instance whose four components are given as #(alpha red green blue).
	 Each component is expected to be between 0.0 and 1.1."

	^self 
		a: anArray first
		r: anArray second
		g: anArray third
		b: anArray fourth!

gray
	"Answer an instance respresenting gray."

	^self fromARGB: ARGB gray!

green
	"Answer an instance respresenting green."

	^self fromARGB: ARGB green!

highlight
	"Answer the Highlight system color."

	^self fromARGB: ARGB highlight!

highlight3d
	"Answer the 3D Highlight system color."

	^self fromARGB: ARGB highlight3d!

icon
	"Answers an Icon that can be used to represent this class"

	^Color icon!

light3d
	"Answer the light 3D system color."

	^self fromARGB: ARGB light3d!

magenta
	"Answer an instance respresenting magenta."

	^self fromARGB: ARGB magenta!

menu
	"Answer the standard window menu background color."

	^self fromARGB: ARGB menu!

new
	"Answer an instance whose four component colour values are 0."

	^(self fromAddress: HGELibrary default hgeColorRGBCreate asParameter)
		beFinalizable;
		yourself!

none
	"Answer the special IndexedColour respresenting no colour."

	^self fromARGB: ARGB none!

red
	"Answer an instance respresenting red."

	^self fromARGB: ARGB red!

shadow3d
	"Answer the 3D shadow system color."

	^self fromARGB: ARGB shadow3d!

systemColor: anInteger 
	"Answer a new sub-instance of the receiver for the specified system colour."

	^self fromARGB: (ARGB systemColor: anInteger)!

tooltip
	"Answer the standard tooltip background colour."

	^self fromARGB: ARGB tooltip!

tooltipText
	"Answer the standard tooltip text colour."

	^self fromARGB: ARGB tooltipText!

white
	"Answer an instance respresenting white."

	^self fromARGB: ARGB white!

window
	"Answer the standard window background system color."

	^self fromARGB: ARGB window!

windowText
	"Answer the standard window text system color."

	^self fromARGB: ARGB windowText!

yellow
	"Answer an instance respresenting yellow."

	^self fromARGB: ARGB yellow! !
!HGEColorRGB class categoriesFor: #a:r:g:b:!instance creation!public! !
!HGEColorRGB class categoriesFor: #alpha:red:green:blue:!instance creation!public! !
!HGEColorRGB class categoriesFor: #applicationWorkspace!instance creation!public! !
!HGEColorRGB class categoriesFor: #black!instance creation!public! !
!HGEColorRGB class categoriesFor: #blue!instance creation!public! !
!HGEColorRGB class categoriesFor: #brown!instance creation!public! !
!HGEColorRGB class categoriesFor: #buttonFace!instance creation!public! !
!HGEColorRGB class categoriesFor: #choose!instance creation!public! !
!HGEColorRGB class categoriesFor: #cyan!instance creation!public! !
!HGEColorRGB class categoriesFor: #darkBlue!instance creation!public! !
!HGEColorRGB class categoriesFor: #darkCyan!instance creation!public! !
!HGEColorRGB class categoriesFor: #darkGray!instance creation!public! !
!HGEColorRGB class categoriesFor: #darkGreen!instance creation!public! !
!HGEColorRGB class categoriesFor: #darkMagenta!instance creation!public! !
!HGEColorRGB class categoriesFor: #darkRed!instance creation!public! !
!HGEColorRGB class categoriesFor: #darkShadow3d!instance creation!public! !
!HGEColorRGB class categoriesFor: #default!instance creation!public! !
!HGEColorRGB class categoriesFor: #defineFields!initializing!public! !
!HGEColorRGB class categoriesFor: #dialog!instance creation!public! !
!HGEColorRGB class categoriesFor: #face3d!instance creation!public! !
!HGEColorRGB class categoriesFor: #fromARGB:!instance creation!public! !
!HGEColorRGB class categoriesFor: #fromArray:!instance creation!public! !
!HGEColorRGB class categoriesFor: #fromInteger:!instance creation!public! !
!HGEColorRGB class categoriesFor: #fromNormalizedArray:!instance creation!public! !
!HGEColorRGB class categoriesFor: #gray!instance creation!public! !
!HGEColorRGB class categoriesFor: #green!instance creation!public! !
!HGEColorRGB class categoriesFor: #highlight!instance creation!public! !
!HGEColorRGB class categoriesFor: #highlight3d!instance creation!public! !
!HGEColorRGB class categoriesFor: #icon!constants!public! !
!HGEColorRGB class categoriesFor: #light3d!instance creation!public! !
!HGEColorRGB class categoriesFor: #magenta!instance creation!public! !
!HGEColorRGB class categoriesFor: #menu!instance creation!public! !
!HGEColorRGB class categoriesFor: #new!instance creation!public! !
!HGEColorRGB class categoriesFor: #none!instance creation!public! !
!HGEColorRGB class categoriesFor: #red!instance creation!public! !
!HGEColorRGB class categoriesFor: #shadow3d!instance creation!public! !
!HGEColorRGB class categoriesFor: #systemColor:!instance creation!public! !
!HGEColorRGB class categoriesFor: #tooltip!instance creation!public! !
!HGEColorRGB class categoriesFor: #tooltipText!instance creation!public! !
!HGEColorRGB class categoriesFor: #white!instance creation!public! !
!HGEColorRGB class categoriesFor: #window!instance creation!public! !
!HGEColorRGB class categoriesFor: #windowText!instance creation!public! !
!HGEColorRGB class categoriesFor: #yellow!instance creation!public! !

HGEDistortionMesh guid: (GUID fromString: '{DC51046B-CCEC-4B5F-B3D4-2DC21E5385B7}')!
HGEDistortionMesh comment: 'HGEDistortionMesh is an HGE helper class that allows you to create effects like water, lenses, page wraps, various twists and even real-time morphing.'!
!HGEDistortionMesh categoriesForClass!External-Data-Structured! !
!HGEDistortionMesh methodsFor!

basicFree
	"Private - Free external resources owned by the receiver."

	Transcript
		show: 'HGEDistortionMesh>>basicFree';
		cr.
	HGELibrary default hgeDistortionMeshDestroy: self!

blendMode: anInteger 
	"Sets the distortion mesh blending mode.

		void SetBlendMode(
			int blend
		);"

	<virtual stdcall: void 5 sdword>
	^self invalidCall!

clear: color z: z 
	"Clears the displacements and coloring of the mesh.

		void Clear(
			DWORD color = 0xFFFFFFFF,
			float z = 0.5f
		);"

	<virtual stdcall: void 2 dword float>
	^self invalidCall!

clearWith: anARGB 
	"Clears the displacements and coloring of the mesh."

	self clearWith: anARGB zOrder: 0.5!

clearWith: anARGB zOrder: aFloat 
	"Clears the displacements and coloring of the mesh."

	self clear: anARGB asParameter z: aFloat!

color: anARGB forNodeAt: aPoint 
	"Sets color for the distortion node at column and row as specified by aPoint.
	Note that node colum and row numeration is zero based."

	self 
		setColor: aPoint x
		row: aPoint y
		color: anARGB asParameter!

copy
	"Answer a copy of the receiver.
	TODO: implement this method correctly."

	^(HGELibrary default hgeDistortionMeshCreateFromDistortionMesh: self)
		texture: self texture; "needs only this?"
		beFinalizable;
		yourself!

displacement: aDeltaPoint forNodeAt: aNodePoint withReference: anInteger 
	"Sets X-axis and Y-axis displacement by aDeltaPoint for the distortion node at column
	and row as specified by aNodePoint with the displacement reference point anInteger.
	Note that node colum and row numeration is zero based.
	TODO: find a better selector name and modify the required tutorials accordingly."

	self 
		setDisplacement: aNodePoint x
		row: aNodePoint y
		dx: aDeltaPoint x
		dy: aDeltaPoint y
		ref: anInteger!

getTexture
	"Returns the current distortion mesh texture.

		HTEXTURE GetTexture();"

	<virtual stdcall: dword 9>
	^self invalidCall!

getTextureRect: x y: y w: w h: h 
	"Returns the current distortion mesh texture region.

		void GetTextureRect(
			float *x,
			float *y,
			float *w,
			float *h
		);"

	<virtual stdcall: void 10 float* float* float* float*>
	^self invalidCall!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^true!

render: x y: y 
	"Renders the distorted region of the texture.

		void Render(
			float x,
			float y
		);"

	<virtual stdcall: void 1 float float>
	^self invalidCall!

renderAt: aPoint 
	"Renders the distorted region of the texture to the screen."

	self render: aPoint x y: aPoint y!

setColor: col row: row color: color 
	"Sets color and alpha for the specified distortion node.

		void SetColor(
			int col,
			int row,
			DWORD color
		);"

	<virtual stdcall: void 7 sdword sdword dword>
	^self invalidCall!

setDisplacement: col row: row dx: dx dy: dy ref: ref 
	"Sets displacement for the specified distortion node.

		void SetDisplacement(
			int col,
			int row,
			float dx,
			float dy,
			int ref
		);"

	<virtual stdcall: void 8 sdword sdword float float sdword>
	^self invalidCall!

setTexture: tex 
	"Sets the texture to use for distortion.

		void SetTexture(
			HTEXTURE tex
		);"

	<virtual stdcall: void 3 dword>
	^self invalidCall!

setTextureRect: x y: y w: w h: h 
	"Sets the texture region to use for distortion.

		void SetTextureRect(
			float x,
			float y,
			float w,
			float h
		);"

	<virtual stdcall: void 4 float float float float>
	^self invalidCall!

texture
	"Answer the receiver's texture."

	^texture!

texture: anHGETexture 
	"Set the receiver's texture.
	Note that we must keep a reference to anHGETexture to prevent it from being GC'd
	while the receiver is using it."

	texture := anHGETexture.
	texture ~= self getTexture ifTrue: [self setTexture: anHGETexture value]!

textureRectangle
	"Answer the receiver's texture coordinates and extent as a Rectangle."

	| x y width height |
	x := FLOAT new.
	y := FLOAT new.
	width := FLOAT new.
	height := FLOAT new.
	self 
		getTextureRect: x
		y: y
		w: width
		h: height.
	^Rectangle origin: x @ y extent: width @ height!

textureRectangle: aRectangle 
	"Set the receiver's texture coordinates and extent specified by aRectangle."

	self 
		setTextureRect: aRectangle left
		y: aRectangle top
		w: aRectangle width
		h: aRectangle height! !
!HGEDistortionMesh categoriesFor: #basicFree!private!realizing/unrealizing! !
!HGEDistortionMesh categoriesFor: #blendMode:!public! !
!HGEDistortionMesh categoriesFor: #clear:z:!private! !
!HGEDistortionMesh categoriesFor: #clearWith:!public! !
!HGEDistortionMesh categoriesFor: #clearWith:zOrder:!public! !
!HGEDistortionMesh categoriesFor: #color:forNodeAt:!public! !
!HGEDistortionMesh categoriesFor: #copy!copying!public! !
!HGEDistortionMesh categoriesFor: #displacement:forNodeAt:withReference:!public! !
!HGEDistortionMesh categoriesFor: #getTexture!private! !
!HGEDistortionMesh categoriesFor: #getTextureRect:y:w:h:!private! !
!HGEDistortionMesh categoriesFor: #needsFree!private!realizing/unrealizing! !
!HGEDistortionMesh categoriesFor: #render:y:!private! !
!HGEDistortionMesh categoriesFor: #renderAt:!public! !
!HGEDistortionMesh categoriesFor: #setColor:row:color:!private! !
!HGEDistortionMesh categoriesFor: #setDisplacement:row:dx:dy:ref:!private! !
!HGEDistortionMesh categoriesFor: #setTexture:!private! !
!HGEDistortionMesh categoriesFor: #setTextureRect:y:w:h:!private! !
!HGEDistortionMesh categoriesFor: #texture!accessing!public! !
!HGEDistortionMesh categoriesFor: #texture:!accessing!public! !
!HGEDistortionMesh categoriesFor: #textureRectangle!public! !
!HGEDistortionMesh categoriesFor: #textureRectangle:!public! !

!HGEDistortionMesh class methodsFor!

columns: colums rows: rows 
	"Answer a new instance of the receiver with the specified number of mesh columns and rows."

	^(self 
		fromAddress: (HGELibrary default hgeDistortionMeshCreateWithColumns: colums rows: rows) asParameter)
		beFinalizable;
		yourself!

new
	"Should not implement. Use #columns:rows:"

	^self shouldNotImplement! !
!HGEDistortionMesh class categoriesFor: #columns:rows:!instance creation!public! !
!HGEDistortionMesh class categoriesFor: #new!instance creation!public! !

HGEFont guid: (GUID fromString: '{E6288ABB-5534-4B1F-A4CC-4FF17FA67A32}')!
HGEFont comment: 'HGEFont is an HGE helper class for rendering text with bitmap fonts.'!
!HGEFont categoriesForClass!External-Data-Structured! !
!HGEFont methodsFor!

basicFree
	"Private - Free external resources owned by the receiver."

	Transcript
		show: 'HGEFont>>basicFree';
		cr.
	HGELibrary default hgeFontDestroy: self!

color: anARGB 
	"Set the receiver's color to anARGB."

	self setColor: anARGB asParameter!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^true!

render: aString at: aPoint 
	"Renders aString to the screen at aPoint (left aligned)."

	self 
		render: aString
		at: aPoint
		align: HGETEXT_LEFT!

render: aString at: aPoint align: anInteger 
	"Renders aString to the screen at aPoint with alignment anInteger."

	self 
		render: aPoint x
		y: aPoint y
		align: anInteger
		string: aString!

render: x y: y align: align string: string 
	"Renders the text string to the screen.

		void Render(
			float x,
			float y,
			int align,
			const char *string
		);"

	<virtual stdcall: void 1 float float sdword char*>
	^self invalidCall!

setColor: col 
	"Sets the font tint and alpha.

		void SetColor(
			DWORD col
		);"

	<virtual stdcall: void 4 dword>
	^self invalidCall! !
!HGEFont categoriesFor: #basicFree!private!realizing/unrealizing! !
!HGEFont categoriesFor: #color:!public! !
!HGEFont categoriesFor: #needsFree!private!realizing/unrealizing! !
!HGEFont categoriesFor: #render:at:!public! !
!HGEFont categoriesFor: #render:at:align:!public! !
!HGEFont categoriesFor: #render:y:align:string:!private! !
!HGEFont categoriesFor: #setColor:!private! !

!HGEFont class methodsFor!

fromFile: aString 
	"Answer a new instance of the receiver from the file named aString."

	^self fromFile: aString mipmap: false!

fromFile: aString mipmap: aBoolean 
	"Answer a new instance of the receiver from the file named aString with mipmap levels
	created according to aBoolean."

	^(self fromAddress: (HGELibrary default hgeFontCreate: aString bMipmap: aBoolean) asParameter)
		beFinalizable;
		yourself!

new
	"Should not implement. Use #fromFile:mipmap: or #fromFile:"

	^self shouldNotImplement! !
!HGEFont class categoriesFor: #fromFile:!instance creation!public! !
!HGEFont class categoriesFor: #fromFile:mipmap:!instance creation!public! !
!HGEFont class categoriesFor: #new!instance creation!public! !

HGEInputEvent guid: (GUID fromString: '{D9C923D8-788C-42D7-98E3-06B95E0E3E80}')!
HGEInputEvent comment: 'HGEInputEvent is used with HGE>>inputGetEvent method to receive buffered input events.

Members:

type
	Type of the event, could be one of these:
	INPUT_KEYDOWN - a keyboard key was pressed down
	INPUT_KEYUP - a keyboard key was released
	INPUT_MBUTTONDOWN - a mouse button was pressed down
	INPUT_MBUTTONUP - a mouse button was released
	INPUT_MOUSEMOVE - mouse was moved
	INPUT_MOUSEWHEEL - mouse wheel was rotated
key
	The key code. Is valid only for INPUT_KEYDOWN, INPUT_KEYUP, INPUT_MBUTTONDOWN and INPUT_MBUTTONUP events. 
flags
	Bitwise or (|) combination of the following constants, specifying the event context: HGEINP_SHIFT - one of the Shift keys is pressed down
	HGEINP_CTRL - one of the Ctrl keys is pressed down
	HGEINP_ALT - one of the Alt keys is pressed down
	HGEINP_CAPSLOCK - Caps Lock is toggled on
	HGEINP_NUMLOCK - Num Lock is toggled on
	HGEINP_SCROLLLOCK - Scroll Lock is toggled on
	HGEINP_REPEAT - for INPUT_KEYDOWN: the event is generated due to autorepeat feature; for INPUT_MBUTTONDOWN: the event is double click
chr
	The character code, cosidering the current keyboard states and input locale. Is valid only for INPUT_KEYDOWN and INPUT_KEYUP events. 
wheel
	The number of notches the mouse wheel was rotated through. A positive value indicates that the wheel was rotated forward, away from the user;
	a negative value indicates that the wheel was rotated backward, toward the user. Is valid only for INPUT_MOUSEWHEEL event. 
x
	Mouse cursor X-position. 
y
	Mouse cursor Y-position.'!
!HGEInputEvent categoriesForClass!External-Data-Structured! !
!HGEInputEvent methodsFor!

chr
	"Answer the receiver's chr field as a Smalltalk object."

	^(bytes sdwordAtOffset: 12)!

chr: anObject
	"Set the receiver's chr field to the value of anObject."

	bytes sdwordAtOffset: 12 put: anObject!

flags
	"Answer the receiver's flags field as a Smalltalk object."

	^(bytes sdwordAtOffset: 8)!

flags: anObject
	"Set the receiver's flags field to the value of anObject."

	bytes sdwordAtOffset: 8 put: anObject!

key
	"Answer the receiver's key field as a Smalltalk object."

	^(bytes sdwordAtOffset: 4)!

key: anObject
	"Set the receiver's key field to the value of anObject."

	bytes sdwordAtOffset: 4 put: anObject!

type
	"Answer the receiver's type field as a Smalltalk object."

	^(bytes sdwordAtOffset: 0)!

type: anObject
	"Set the receiver's type field to the value of anObject."

	bytes sdwordAtOffset: 0 put: anObject!

wheel
	"Answer the receiver's wheel field as a Smalltalk object."

	^(bytes sdwordAtOffset: 16)!

wheel: anObject
	"Set the receiver's wheel field to the value of anObject."

	bytes sdwordAtOffset: 16 put: anObject!

x
	"Answer the receiver's x field as a Smalltalk object."

	^(bytes floatAtOffset: 20)!

x: anObject
	"Set the receiver's x field to the value of anObject."

	bytes floatAtOffset: 20 put: anObject!

y
	"Answer the receiver's y field as a Smalltalk object."

	^(bytes floatAtOffset: 24)!

y: anObject
	"Set the receiver's y field to the value of anObject."

	bytes floatAtOffset: 24 put: anObject! !
!HGEInputEvent categoriesFor: #chr!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #chr:!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #flags!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #flags:!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #key!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #key:!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #type!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #type:!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #wheel!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #wheel:!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #x!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #x:!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #y!**compiled accessors**!public! !
!HGEInputEvent categoriesFor: #y:!**compiled accessors**!public! !

!HGEInputEvent class methodsFor!

defineFields
	"HGE Input Event structure.

		HGEInputEvent compileDefinition

		struct hgeInputEvent
		{
			int	type;		// event type
			int	key;		// key code
			int	flags;	// event flags
			int	chr;		// character code
			int	wheel	// wheel shift
			float	x;		// mouse cursor x-coordinate
			float	y;		// mouse cursor y-coordinate
		};"

	self
		defineField: #type type: SDWORDField new;
		defineField: #key type: SDWORDField new;
		defineField: #flags type: SDWORDField new;
		defineField: #chr type: SDWORDField new;
		defineField: #wheel type: SDWORDField new;
		defineField: #x type: FLOATField new;
		defineField: #y type: FLOATField new! !
!HGEInputEvent class categoriesFor: #defineFields!initializing!public! !

HGEParticleSystem guid: (GUID fromString: '{6BB0E9E5-954A-4DAB-BE00-6F21E5EE1C7A}')!
HGEParticleSystem comment: 'HGEParticleSystem is an HGE helper class for advanced 2D particle systems. '!
!HGEParticleSystem categoriesForClass!External-Data-Structured! !
!HGEParticleSystem methodsFor!

basicFree
	"Private - Free external resources owned by the receiver."

	Transcript
		show: 'HGEParticleSystem>>basicFree';
		cr.
	HGELibrary default hgeParticleSystemDestroy: self!

copy
	"Answer a copy of the receiver.
	TODO: implement this method correctly."

	^(HGELibrary default hgeParticleSystemCreateFromParticleSystem: self)
		sprite: self sprite; "needs only this?"
		beFinalizable;
		yourself!

fire
	"Fires the particle system.

		void Fire();"

	<virtual stdcall: void 3>
	^self invalidCall!

fireAt: aPoint 
	"Fires the particle system at aPoint."

	self fireAt: aPoint x y: aPoint y!

fireAt: x y: y 
	"Fires the particle system at the specified screen position.

		void FireAt(
			float x,
			float y
		);"

	<virtual stdcall: void 2 float float>
	^self invalidCall!

info
	"Answer the receiver's info field as a Smalltalk object."

	^HGEParticleSystemInfo fromAddress: (bytes yourAddress + 4)!

info: anObject
	"Set the receiver's info field to the value of anObject."

	anObject replaceBytesOf: bytes from: 5 to: 140 startingAt: 1!

moveTo: aPoint 
	"Moves the particle system's emitter to aPoint leaving all active particles where they are."

	self moveTo: aPoint moveParticles: false!

moveTo: aPoint moveParticles: aBoolen 
	"Moves particle system to aPoint.
	If aBoolean is true all active particles are moved to the new position, if false just the
	particle system's emitter is moved."

	self 
		moveTo: aPoint x
		y: aPoint y
		bMoveParticles: aBoolen!

moveTo: x y: y 
	"From the C++ side the bMoveParticles parameter is optional with a default value of
	false:
	If true all active particles are moved to the new position, if false just the particle
	system's emitter is moved."

	self 
		moveTo: x
		y: y
		bMoveParticles: false!

moveTo: x y: y bMoveParticles: bMoveParticles 
	"Moves particle system to a new position.

		void MoveTo(
			float x,
			float y,
			bool bMoveParticles = false
		);"

	<virtual stdcall: void 6 float float bool>
	^self invalidCall!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^true!

render
	"Renders the particle system to the screen.

		void Render();"

	<virtual stdcall: void 1>
	^self invalidCall!

sprite
	"Answer the receiver's sprite."

	^sprite!

sprite: anObject 
	"Private - Set the receiver's sprite."

	sprite := anObject!

update: deltaTimeFloat 
	"Updates the particle system. 

		void Update(
			float fDeltaTime
		);"

	<virtual stdcall: void 5 float>
	^self invalidCall! !
!HGEParticleSystem categoriesFor: #basicFree!private!realizing/unrealizing! !
!HGEParticleSystem categoriesFor: #copy!copying!public! !
!HGEParticleSystem categoriesFor: #fire!public! !
!HGEParticleSystem categoriesFor: #fireAt:!public! !
!HGEParticleSystem categoriesFor: #fireAt:y:!private! !
!HGEParticleSystem categoriesFor: #info!**compiled accessors**!public! !
!HGEParticleSystem categoriesFor: #info:!**compiled accessors**!public! !
!HGEParticleSystem categoriesFor: #moveTo:!public! !
!HGEParticleSystem categoriesFor: #moveTo:moveParticles:!public! !
!HGEParticleSystem categoriesFor: #moveTo:y:!private! !
!HGEParticleSystem categoriesFor: #moveTo:y:bMoveParticles:!private! !
!HGEParticleSystem categoriesFor: #needsFree!private!realizing/unrealizing! !
!HGEParticleSystem categoriesFor: #render!public! !
!HGEParticleSystem categoriesFor: #sprite!accessing!public! !
!HGEParticleSystem categoriesFor: #sprite:!accessing!private! !
!HGEParticleSystem categoriesFor: #update:!public! !

!HGEParticleSystem class methodsFor!

defineFields
	"Class public data members:

		hgeParticleSystemInfo info;

		HGEParticleSystem compileDefinition"

	self 
		defineField: #info
		type: (StructureField type: HGEParticleSystemInfo)
		offset: 4!

fromFile: aString sprite: anHGESprite 
	"Answer a new instance of the receiver from the file named aString (containing the
	particle system description) and anHGESprite to use for the particle system.
	Note that we must keep a reference to anHGESprite to prevent it from being GC'd
	while the receiver is using it."

	^(self 
		fromAddress: (HGELibrary default hgeParticleSystemCreateFromFile: aString sprite: anHGESprite) 
				asParameter)
		sprite: anHGESprite;
		beFinalizable;
		yourself!

fromParticleSytemInfo: anHGEParticleSystemInfo 
	"Answer a new instance of the receiver from anHGEParticleSystemInfo describing
	the particle system.
	Note that we must keep a reference to anHGEParticleSystemInfo's sprite to prevent
	it from being GC'd while the receiver is using it."

	^(self 
		fromAddress: (HGELibrary default 
				hgeParticleSystemCreateFromParticleSystemInfo: anHGEParticleSystemInfo) asParameter)
		sprite: anHGEParticleSystemInfo sprite;
		beFinalizable;
		yourself!

new
	"Should not implement. Use #fromFile:sprite: or #fromParticleSystemInfo:"

	^self shouldNotImplement! !
!HGEParticleSystem class categoriesFor: #defineFields!initializing!public! !
!HGEParticleSystem class categoriesFor: #fromFile:sprite:!instance creation!public! !
!HGEParticleSystem class categoriesFor: #fromParticleSytemInfo:!instance creation!public! !
!HGEParticleSystem class categoriesFor: #new!instance creation!public! !

HGEParticleSystemInfo guid: (GUID fromString: '{D73E2C55-E0F8-4AFA-A3B6-E63A4E56C6BE}')!
HGEParticleSystemInfo comment: 'HGEParticleSystemInfo is used to define all particle system parameters.'!
!HGEParticleSystemInfo categoriesForClass!External-Data-Structured! !
!HGEParticleSystemInfo methodsFor!

bRelative
	"Answer the receiver's bRelative field as a Smalltalk object."

	^(bytes dwordAtOffset: 28) asBoolean!

bRelative: anObject
	"Set the receiver's bRelative field to the value of anObject."

	bytes dwordAtOffset: 28 put: anObject asParameter!

colColorEnd
	"Answer the receiver's colColorEnd field as a Smalltalk object."

	^HGEColorRGB fromAddress: (bytes yourAddress + 108)!

colColorEnd: anObject
	"Set the receiver's colColorEnd field to the value of anObject."

	anObject replaceBytesOf: bytes from: 109 to: 128 startingAt: 1!

colColorStart
	"Answer the receiver's colColorStart field as a Smalltalk object."

	^HGEColorRGB fromAddress: (bytes yourAddress + 88)!

colColorStart: anObject
	"Set the receiver's colColorStart field to the value of anObject."

	anObject replaceBytesOf: bytes from: 89 to: 108 startingAt: 1!

fAlphaVar
	"Answer the receiver's fAlphaVar field as a Smalltalk object."

	^(bytes floatAtOffset: 132)!

fAlphaVar: anObject
	"Set the receiver's fAlphaVar field to the value of anObject."

	bytes floatAtOffset: 132 put: anObject!

fColorVar
	"Answer the receiver's fColorVar field as a Smalltalk object."

	^(bytes floatAtOffset: 128)!

fColorVar: anObject
	"Set the receiver's fColorVar field to the value of anObject."

	bytes floatAtOffset: 128 put: anObject!

fDirection
	"Answer the receiver's fDirection field as a Smalltalk object."

	^(bytes floatAtOffset: 20)!

fDirection: anObject
	"Set the receiver's fDirection field to the value of anObject."

	bytes floatAtOffset: 20 put: anObject!

fGravityMax
	"Answer the receiver's fGravityMax field as a Smalltalk object."

	^(bytes floatAtOffset: 44)!

fGravityMax: anObject
	"Set the receiver's fGravityMax field to the value of anObject."

	bytes floatAtOffset: 44 put: anObject!

fGravityMin
	"Answer the receiver's fGravityMin field as a Smalltalk object."

	^(bytes floatAtOffset: 40)!

fGravityMin: anObject
	"Set the receiver's fGravityMin field to the value of anObject."

	bytes floatAtOffset: 40 put: anObject!

fLifetime
	"Answer the receiver's fLifetime field as a Smalltalk object."

	^(bytes floatAtOffset: 8)!

fLifetime: anObject
	"Set the receiver's fLifetime field to the value of anObject."

	bytes floatAtOffset: 8 put: anObject!

fParticleLifeMax
	"Answer the receiver's fParticleLifeMax field as a Smalltalk object."

	^(bytes floatAtOffset: 16)!

fParticleLifeMax: anObject
	"Set the receiver's fParticleLifeMax field to the value of anObject."

	bytes floatAtOffset: 16 put: anObject!

fParticleLifeMin
	"Answer the receiver's fParticleLifeMin field as a Smalltalk object."

	^(bytes floatAtOffset: 12)!

fParticleLifeMin: anObject
	"Set the receiver's fParticleLifeMin field to the value of anObject."

	bytes floatAtOffset: 12 put: anObject!

fRadialAccelMax
	"Answer the receiver's fRadialAccelMax field as a Smalltalk object."

	^(bytes floatAtOffset: 52)!

fRadialAccelMax: anObject
	"Set the receiver's fRadialAccelMax field to the value of anObject."

	bytes floatAtOffset: 52 put: anObject!

fRadialAccelMin
	"Answer the receiver's fRadialAccelMin field as a Smalltalk object."

	^(bytes floatAtOffset: 48)!

fRadialAccelMin: anObject
	"Set the receiver's fRadialAccelMin field to the value of anObject."

	bytes floatAtOffset: 48 put: anObject!

fSizeEnd
	"Answer the receiver's fSizeEnd field as a Smalltalk object."

	^(bytes floatAtOffset: 68)!

fSizeEnd: anObject
	"Set the receiver's fSizeEnd field to the value of anObject."

	bytes floatAtOffset: 68 put: anObject!

fSizeStart
	"Answer the receiver's fSizeStart field as a Smalltalk object."

	^(bytes floatAtOffset: 64)!

fSizeStart: anObject
	"Set the receiver's fSizeStart field to the value of anObject."

	bytes floatAtOffset: 64 put: anObject!

fSizeVar
	"Answer the receiver's fSizeVar field as a Smalltalk object."

	^(bytes floatAtOffset: 72)!

fSizeVar: anObject
	"Set the receiver's fSizeVar field to the value of anObject."

	bytes floatAtOffset: 72 put: anObject!

fSpeedMax
	"Answer the receiver's fSpeedMax field as a Smalltalk object."

	^(bytes floatAtOffset: 36)!

fSpeedMax: anObject
	"Set the receiver's fSpeedMax field to the value of anObject."

	bytes floatAtOffset: 36 put: anObject!

fSpeedMin
	"Answer the receiver's fSpeedMin field as a Smalltalk object."

	^(bytes floatAtOffset: 32)!

fSpeedMin: anObject
	"Set the receiver's fSpeedMin field to the value of anObject."

	bytes floatAtOffset: 32 put: anObject!

fSpinEnd
	"Answer the receiver's fSpinEnd field as a Smalltalk object."

	^(bytes floatAtOffset: 80)!

fSpinEnd: anObject
	"Set the receiver's fSpinEnd field to the value of anObject."

	bytes floatAtOffset: 80 put: anObject!

fSpinStart
	"Answer the receiver's fSpinStart field as a Smalltalk object."

	^(bytes floatAtOffset: 76)!

fSpinStart: anObject
	"Set the receiver's fSpinStart field to the value of anObject."

	bytes floatAtOffset: 76 put: anObject!

fSpinVar
	"Answer the receiver's fSpinVar field as a Smalltalk object."

	^(bytes floatAtOffset: 84)!

fSpinVar: anObject
	"Set the receiver's fSpinVar field to the value of anObject."

	bytes floatAtOffset: 84 put: anObject!

fSpread
	"Answer the receiver's fSpread field as a Smalltalk object."

	^(bytes floatAtOffset: 24)!

fSpread: anObject
	"Set the receiver's fSpread field to the value of anObject."

	bytes floatAtOffset: 24 put: anObject!

fTangentialAccelMax
	"Answer the receiver's fTangentialAccelMax field as a Smalltalk object."

	^(bytes floatAtOffset: 60)!

fTangentialAccelMax: anObject
	"Set the receiver's fTangentialAccelMax field to the value of anObject."

	bytes floatAtOffset: 60 put: anObject!

fTangentialAccelMin
	"Answer the receiver's fTangentialAccelMin field as a Smalltalk object."

	^(bytes floatAtOffset: 56)!

fTangentialAccelMin: anObject
	"Set the receiver's fTangentialAccelMin field to the value of anObject."

	bytes floatAtOffset: 56 put: anObject!

nEmission
	"Answer the receiver's nEmission field as a Smalltalk object."

	^(bytes sdwordAtOffset: 4)!

nEmission: anObject
	"Set the receiver's nEmission field to the value of anObject."

	bytes sdwordAtOffset: 4 put: anObject!

sprite
	"Answer the receiver's sprite field as a Smalltalk object."

	^HGESprite fromAddress: (bytes sdwordAtOffset: 0)!

sprite: anObject 
	"Set the receiver's sprite field to the value of anObject."

	sprite := anObject.	"Note that we must keep a reference to the sprite to prevent its premature death"
	bytes dwordAtOffset: 0 put: anObject yourAddress! !
!HGEParticleSystemInfo categoriesFor: #bRelative!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #bRelative:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #colColorEnd!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #colColorEnd:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #colColorStart!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #colColorStart:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fAlphaVar!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fAlphaVar:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fColorVar!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fColorVar:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fDirection!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fDirection:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fGravityMax!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fGravityMax:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fGravityMin!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fGravityMin:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fLifetime!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fLifetime:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fParticleLifeMax!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fParticleLifeMax:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fParticleLifeMin!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fParticleLifeMin:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fRadialAccelMax!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fRadialAccelMax:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fRadialAccelMin!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fRadialAccelMin:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSizeEnd!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSizeEnd:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSizeStart!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSizeStart:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSizeVar!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSizeVar:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpeedMax!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpeedMax:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpeedMin!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpeedMin:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpinEnd!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpinEnd:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpinStart!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpinStart:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpinVar!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpinVar:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpread!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fSpread:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fTangentialAccelMax!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fTangentialAccelMax:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fTangentialAccelMin!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #fTangentialAccelMin:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #nEmission!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #nEmission:!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #sprite!**compiled accessors**!public! !
!HGEParticleSystemInfo categoriesFor: #sprite:!public! !

!HGEParticleSystemInfo class methodsFor!

defineFields
	"HGEParticleSystemInfo is used to define all particle system parameters.

		HGEParticleSystemInfo compileDefinition

		struct hgeParticleSystemInfo
		{
			hgeSprite*	sprite;		// texture + blend mode
			int		nEmission;	// particles per sec
			float		fLifetime;

			float		fParticleLifeMin;
			float		fParticleLifeMax;

			float		fDirection;
			float		fSpread;
			bool		bRelative;

			float		fSpeedMin;
			float		fSpeedMax;

			float		fGravityMin;
			float		fGravityMax;

			float		fRadialAccelMin;
			float		fRadialAccelMax;

			float		fTangentialAccelMin;
			float		fTangentialAccelMax;

			float		fSizeStart;
			float		fSizeEnd;
			float		fSizeVar;

			float		fSpinStart;
			float		fSpinEnd;
			float		fSpinVar;

			hgeColor	colColorStart;	// + alpha
			hgeColor	colColorEnd;
			float		fColorVar;
			float		fAlphaVar;
		};"

	self
		defineField: #sprite type: (PointerField type: HGESprite);
		defineField: #nEmission type: SDWORDField new;
		defineField: #fLifetime type: FLOATField new;
		defineField: #fParticleLifeMin type: FLOATField new;
		defineField: #fParticleLifeMax type: FLOATField new;
		defineField: #fDirection type: FLOATField new;
		defineField: #fSpread type: FLOATField new;
		defineField: #bRelative type: BOOLField new;
		defineField: #fSpeedMin type: FLOATField new;
		defineField: #fSpeedMax type: FLOATField new;
		defineField: #fGravityMin type: FLOATField new;
		defineField: #fGravityMax type: FLOATField new;
		defineField: #fRadialAccelMin type: FLOATField new;
		defineField: #fRadialAccelMax type: FLOATField new;
		defineField: #fTangentialAccelMin type: FLOATField new;
		defineField: #fTangentialAccelMax type: FLOATField new;
		defineField: #fSizeStart type: FLOATField new;
		defineField: #fSizeEnd type: FLOATField new;
		defineField: #fSizeVar type: FLOATField new;
		defineField: #fSpinStart type: FLOATField new;
		defineField: #fSpinEnd type: FLOATField new;
		defineField: #fSpinVar type: FLOATField new;
		defineField: #colColorStart type: (StructureField type: HGEColorRGB);
		defineField: #colColorEnd type: (StructureField type: HGEColorRGB);
		defineField: #fColorVar type: FLOATField new;
		defineField: #fAlphaVar type: FLOATField new! !
!HGEParticleSystemInfo class categoriesFor: #defineFields!initializing!public! !

HGEQuad guid: (GUID fromString: '{E5623F22-0968-439A-B0D2-5B5D9D581830}')!
HGEQuad comment: 'HGEQuad is an arbitrary colored and textured four-vertex 2D polygon. It is a fundamental HGE graphic primitive.

Members:

v
	An array of four vertices, describing the quad. 
tex
	Quad''s texture handle or 0. 
blend
	Quad''s blending mode. '!
!HGEQuad categoriesForClass!External-Data-Structured! !
!HGEQuad methodsFor!

blend
	"Answer the receiver's blend field as a Smalltalk object."

	^(bytes sdwordAtOffset: 100)!

blend: anObject
	"Set the receiver's blend field to the value of anObject."

	bytes sdwordAtOffset: 100 put: anObject!

render
	"Renders the receiver."

	(HGELibrary default hgeCreate: HGE_VERSION)
		gfxRenderQuad: self;
		release!

tex
	"Answer the receiver's tex field as a Smalltalk object."

	^texture isNil ifTrue: [bytes dwordAtOffset: 96] ifFalse: [texture]!

tex: anObject 
	"Set the receiver's tex field to the value of anObject.
	If anObject is an HGETexture we must keep a reference to prevent it from being GC'd
	while the receiver is using it."

	anObject class = HGETexture ifTrue: [texture := anObject].
	bytes dwordAtOffset: 96 put: anObject!

v
	"Answer the receiver's v field as a Smalltalk object."

	^StructureArray fromAddress: (bytes yourAddress) length: 4 elementClass: HGEVertex!

v: anObject
	"Set the receiver's v field to the value of anObject."

	| size |
	size := anObject byteSize min: (4 * 24).
	anObject replaceBytesOf: bytes from: 1 to: size startingAt: 1! !
!HGEQuad categoriesFor: #blend!**compiled accessors**!public! !
!HGEQuad categoriesFor: #blend:!**compiled accessors**!public! !
!HGEQuad categoriesFor: #render!graphics!public! !
!HGEQuad categoriesFor: #tex!public! !
!HGEQuad categoriesFor: #tex:!public! !
!HGEQuad categoriesFor: #v!**compiled accessors**!public! !
!HGEQuad categoriesFor: #v:!**compiled accessors**!public! !

!HGEQuad class methodsFor!

defineFields
	"HGE Quad structure.

		HGEQuad compileDefinition

		struct hgeQuad
		{
			hgeVertex		v[4];
			HTEXTURE	tex;
			int			blend;
		};"

	self
		defineField: #v type: (StructureArrayField type: HGEVertex length: 4);
		defineField: #tex type: DWORDField new;
		defineField: #blend type: SDWORDField new! !
!HGEQuad class categoriesFor: #defineFields!initializing!public! !

HGESprite guid: (GUID fromString: '{B491BDC2-7E2F-4AAD-A08E-C6081309BC1C}')!
HGESprite comment: 'HGESprite is an HGE helper class for sprite entities.'!
!HGESprite categoriesForClass!External-Data-Structured! !
!HGESprite methodsFor!

basicFree
	"Private - Free external resources owned by the receiver."

	Transcript
		show: 'HGESprite>>basicFree';
		cr.
	HGELibrary default hgeSpriteDestroy: self!

blendMode: anInteger 
	"Sets the sprite blending mode.

		void SetBlendMode(
			int blend
		);"

	<virtual stdcall: void 9 sdword>
	^self invalidCall!

color: anARGB 
	"Sets tint and alpha for the entire sprite."

	self color: anARGB forVertex: 0!

color: anARGB forVertex: anInteger 
	"Sets tint and alpha for the specified vertex:
	Vertices are indexed clockwise starting from top-left one in the range 1-4."

	self setColor: anARGB asParameter i: anInteger - 1!

copy
	"Answer a copy of the receiver.
	TODO: implement this method correctly."

	^(HGELibrary default hgeSpriteCreateFromSprite: self)
		texture: self texture; "needs only this?"
		beFinalizable;
		yourself!

getTexture
	"Returns the current sprite texture.

		HTEXTURE GetTexture();"

	<virtual stdcall: dword 12>
	^self invalidCall!

hotSpot: aPoint 
	"Sets the sprite anchor point."

	self setHotSpot: aPoint x y: aPoint y!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^true!

render: x y: y 
	"Renders the sprite to the screen.

		void Render(
			float x,
			float y
		);"

	<virtual stdcall: void 1 float float>
	^self invalidCall!

renderAt: aPoint 
	"Renders the sprite to the screen."

	self render: aPoint x y: aPoint y!

renderAt: aPoint rotatedBy: radians 
	"Renders the sprite to the screen rotated by radians."

	self 
		renderAt: aPoint
		rotatedBy: radians
		scaledBy: 1 @ 1!

renderAt: aPoint rotatedBy: radians scaledBy: aScalePoint 
	"Renders the sprite to the screen rotated by radians and scaled by aScalePoint."

	self 
		renderEx: aPoint x
		y: aPoint y
		rot: radians
		hscale: aScalePoint x
		vscale: aScalePoint y!

renderAt: aPoint scaledBy: aScalePoint 
	"Renders the sprite to the screen scaled by aScalePoint."

	self 
		renderAt: aPoint
		rotatedBy: 0
		scaledBy: aScalePoint!

renderEx: x y: y rot: rot hscale: hscale vscale: vscale 
	"Renders the sprite to the screen with scaling and rotation.

		void RenderEx(
			float x,
			float y,
			float rot,
			float hscale = 1.0f,
			float vscale = 0.0f
		);"

	<virtual stdcall: void 2 float float float float float>
	^self invalidCall!

setColor: col 
	"From the C++ side the i parameter is optional with a default value of -1:
	Optional index of the vertex to be changed. Vertices are indexed clockwise starting
	from top-left one in the range 0-3. If =-1 or omitted, all four vertices will be changed."

	self setColor: col i: -1!

setColor: col i: i 
	"Sets tint and alpha for the specified vertex or the entire sprite.

		void SetColor(
			DWORD col,
			int i = -1
		);"

	<virtual stdcall: void 7 dword sdword>
	^self invalidCall!

setHotSpot: x y: y 
	"Sets the sprite anchor point.

		void SetHotSpot(
			float x,
			float y
		);"

	<virtual stdcall: void 10 float float>
	^self invalidCall!

setTexture: tex 
	"Changes the sprite texture.

		void SetTexture(
			HTEXTURE tex
		);"

	<virtual stdcall: void 5 dword>
	^self invalidCall!

texture
	"Answer the receiver's texture."

	^texture!

texture: anHGETexture 
	"Set the receiver's texture."

	texture := anHGETexture.
	texture ~= self getTexture ifTrue: [self setTexture: anHGETexture value]! !
!HGESprite categoriesFor: #basicFree!private!realizing/unrealizing! !
!HGESprite categoriesFor: #blendMode:!public! !
!HGESprite categoriesFor: #color:!public! !
!HGESprite categoriesFor: #color:forVertex:!public! !
!HGESprite categoriesFor: #copy!copying!public! !
!HGESprite categoriesFor: #getTexture!private! !
!HGESprite categoriesFor: #hotSpot:!public! !
!HGESprite categoriesFor: #needsFree!private!realizing/unrealizing! !
!HGESprite categoriesFor: #render:y:!private! !
!HGESprite categoriesFor: #renderAt:!public! !
!HGESprite categoriesFor: #renderAt:rotatedBy:!public! !
!HGESprite categoriesFor: #renderAt:rotatedBy:scaledBy:!public! !
!HGESprite categoriesFor: #renderAt:scaledBy:!public! !
!HGESprite categoriesFor: #renderEx:y:rot:hscale:vscale:!private! !
!HGESprite categoriesFor: #setColor:!private! !
!HGESprite categoriesFor: #setColor:i:!private! !
!HGESprite categoriesFor: #setHotSpot:y:!private! !
!HGESprite categoriesFor: #setTexture:!private! !
!HGESprite categoriesFor: #texture!accessing!public! !
!HGESprite categoriesFor: #texture:!accessing!public! !

!HGESprite class methodsFor!

fromBytes: aByteObject 
	"Answer a new instance of the receiver from aByteObject.
	For supported image formats see HGE>>textureLoad:"

	| texture |
	texture := HGETexture fromBytes: aByteObject.
	^self fromTexture: texture!

fromBytes: aByteObject rectangle: aRectangle 
	"Answer a new instance of the receiver from aByteObject with texture coordinates and
	extent defined by aRectangle."

	^self 
		fromBytes: aByteObject
		rectangle: aRectangle
		mipmap: false!

fromBytes: aByteObject rectangle: aRectangle mipmap: aBoolean 
	"Answer a new instance of the receiver from aByteObject with texture coordinates and
	extent defined by aRectangle, and mipmap levels created according to aBoolean."

	| texture |
	texture := HGETexture fromBytes: aByteObject mipmap: aBoolean.
	^self fromTexture: texture rectangle: aRectangle!

fromFile: aString 
	"Answer a new instance of the receiver from the file named aString.
	For supported image formats see HGE>>textureLoad:"

	| texture |
	texture := HGETexture fromFile: aString.
	^self fromTexture: texture!

fromFile: aString rectangle: aRectangle 
	"Answer a new instance of the receiver from the file named aString with texture
	coordinates and extent defined by aRectangle."

	^self 
		fromFile: aString
		rectangle: aRectangle
		mipmap: false!

fromFile: aString rectangle: aRectangle mipmap: aBoolean 
	"Answer a new instance of the receiver from the file named aString with texture
	coordinates and extent defined by aRectangle, and mipmap levels created
	according to aBoolean."

	| texture |
	texture := HGETexture fromFile: aString mipmap: aBoolean.
	^self fromTexture: texture rectangle: aRectangle!

fromTexture: anHGETexture 
	"Answer a new instance of the receiver from anHGETexture."

	^self fromTexture: anHGETexture rectangle: anHGETexture rectangle!

fromTexture: anHGETexture rectangle: aRectangle 
	"Answer a new instance of the receiver from anHGETexture with texture coordinates
	and extent defined by aRectangle.
	Note that we must keep a reference to anHGETexture to prevent it from being GC'd
	while the receiver is using it."

	^(self 
		fromAddress: (HGELibrary default 
				hgeSpriteCreateFromTexture: anHGETexture
				x: aRectangle left
				y: aRectangle top
				w: aRectangle width
				h: aRectangle height) asParameter)
		texture: anHGETexture;
		beFinalizable;
		yourself!

new
	"Should not implement. Use #fromTexture:rectangle:, #fromFile:rectangle:mipmap:,
	#fromBytes:rectangle:mipmap: or one of their derivatives."

	^self shouldNotImplement! !
!HGESprite class categoriesFor: #fromBytes:!instance creation!public! !
!HGESprite class categoriesFor: #fromBytes:rectangle:!instance creation!public! !
!HGESprite class categoriesFor: #fromBytes:rectangle:mipmap:!instance creation!public! !
!HGESprite class categoriesFor: #fromFile:!instance creation!public! !
!HGESprite class categoriesFor: #fromFile:rectangle:!instance creation!public! !
!HGESprite class categoriesFor: #fromFile:rectangle:mipmap:!instance creation!public! !
!HGESprite class categoriesFor: #fromTexture:!instance creation!public! !
!HGESprite class categoriesFor: #fromTexture:rectangle:!instance creation!public! !
!HGESprite class categoriesFor: #new!instance creation!public! !

HGETriple guid: (GUID fromString: '{E9003872-B89E-4C71-AB7D-E013A3CA10D3}')!
HGETriple comment: 'HGETriple is an arbitrary colored and textured 2D triangle.

Members:

v
	An array of three vertices, describing the triple. 
tex
	Triple''s texture handle or 0. 
blend
	Triple''s blending mode. '!
!HGETriple categoriesForClass!External-Data-Structured! !
!HGETriple methodsFor!

blend
	"Answer the receiver's blend field as a Smalltalk object."

	^(bytes sdwordAtOffset: 76)!

blend: anObject
	"Set the receiver's blend field to the value of anObject."

	bytes sdwordAtOffset: 76 put: anObject!

tex
	"Answer the receiver's tex field as a Smalltalk object."

	^texture isNil ifTrue: [bytes dwordAtOffset: 72] ifFalse: [texture]!

tex: anObject 
	"Set the receiver's tex field to the value of anObject.
	If anObject is an HGETexture we must keep a reference to prevent it from being GC'd
	while the receiver is using it."

	anObject class = HGETexture ifTrue: [texture := anObject].
	bytes dwordAtOffset: 72 put: anObject!

v
	"Answer the receiver's v field as a Smalltalk object."

	^StructureArray fromAddress: (bytes yourAddress) length: 3 elementClass: HGEVertex!

v: anObject
	"Set the receiver's v field to the value of anObject."

	| size |
	size := anObject byteSize min: (3 * 24).
	anObject replaceBytesOf: bytes from: 1 to: size startingAt: 1! !
!HGETriple categoriesFor: #blend!**compiled accessors**!public! !
!HGETriple categoriesFor: #blend:!**compiled accessors**!public! !
!HGETriple categoriesFor: #tex!public! !
!HGETriple categoriesFor: #tex:!public! !
!HGETriple categoriesFor: #v!**compiled accessors**!public! !
!HGETriple categoriesFor: #v:!**compiled accessors**!public! !

!HGETriple class methodsFor!

defineFields
	"HGE Triple structure.

		HGETriple compileDefinition

		struct hgeTriple
		{
			hgeVertex		v[3];
			HTEXTURE	tex;
			int			blend;
		};"

	self
		defineField: #v type: (StructureArrayField type: HGEVertex length: 3);
		defineField: #tex type: DWORDField new;
		defineField: #blend type: SDWORDField new! !
!HGETriple class categoriesFor: #defineFields!initializing!public! !

HGEVertex guid: (GUID fromString: '{6EE29C98-CAD2-4B1B-9B98-CFB4E167BD6D}')!
HGEVertex comment: 'HGEVertex is used to describe vertices of which HGE graphic primitives consist.

Members:

x
	Screen X-coordinate. 
y
	Screen Y-coordinate. 
z
	Vertex Z-order ranging from 0.0 to 1.0.
	If Z-buffer isn''t used, the value is ignored. 
col
	Vertex color and opacity. 
tx
	Texture X-coordinate in range 0.0 to 1.0. 
ty
	Texture Y-coordinate in range 0.0 to 1.0. 

Remarks:

If you specify values outside the 0.0-1.0 range for texture coordinates, the texture data will be
repeated on every integer junction. For example, it is an easy way to implement "rotozoom" effect. '!
!HGEVertex categoriesForClass!External-Data-Structured! !
!HGEVertex methodsFor!

col
	"Answer the receiver's col field as a Smalltalk object."

	^(bytes dwordAtOffset: 12)!

col: anObject
	"Set the receiver's col field to the value of anObject."

	bytes dwordAtOffset: 12 put: anObject!

tx
	"Answer the receiver's tx field as a Smalltalk object."

	^(bytes floatAtOffset: 16)!

tx: anObject
	"Set the receiver's tx field to the value of anObject."

	bytes floatAtOffset: 16 put: anObject!

tx: txCoord ty: tyCoord 
	"Set the receiver's tx and ty field."

	self tx: txCoord.
	self ty: tyCoord!

ty
	"Answer the receiver's ty field as a Smalltalk object."

	^(bytes floatAtOffset: 20)!

ty: anObject
	"Set the receiver's ty field to the value of anObject."

	bytes floatAtOffset: 20 put: anObject!

x
	"Answer the receiver's x field as a Smalltalk object."

	^(bytes floatAtOffset: 0)!

x: anObject
	"Set the receiver's x field to the value of anObject."

	bytes floatAtOffset: 0 put: anObject!

x: xCoord y: yCoord 
	"Set the receiver's x and y field."

	self x: xCoord.
	self y: yCoord!

y
	"Answer the receiver's y field as a Smalltalk object."

	^(bytes floatAtOffset: 4)!

y: anObject
	"Set the receiver's y field to the value of anObject."

	bytes floatAtOffset: 4 put: anObject!

z
	"Answer the receiver's z field as a Smalltalk object."

	^(bytes floatAtOffset: 8)!

z: anObject
	"Set the receiver's z field to the value of anObject."

	bytes floatAtOffset: 8 put: anObject! !
!HGEVertex categoriesFor: #col!**compiled accessors**!public! !
!HGEVertex categoriesFor: #col:!**compiled accessors**!public! !
!HGEVertex categoriesFor: #tx!**compiled accessors**!public! !
!HGEVertex categoriesFor: #tx:!**compiled accessors**!public! !
!HGEVertex categoriesFor: #tx:ty:!public! !
!HGEVertex categoriesFor: #ty!**compiled accessors**!public! !
!HGEVertex categoriesFor: #ty:!**compiled accessors**!public! !
!HGEVertex categoriesFor: #x!**compiled accessors**!public! !
!HGEVertex categoriesFor: #x:!**compiled accessors**!public! !
!HGEVertex categoriesFor: #x:y:!public! !
!HGEVertex categoriesFor: #y!**compiled accessors**!public! !
!HGEVertex categoriesFor: #y:!**compiled accessors**!public! !
!HGEVertex categoriesFor: #z!**compiled accessors**!public! !
!HGEVertex categoriesFor: #z:!**compiled accessors**!public! !

!HGEVertex class methodsFor!

defineFields
	"HGE Vertex structure.

		HGEVertex compileDefinition

		struct hgeVertex
		{
			float		x, y;		// screen position    
			float		z;		// Z-buffer depth 0..1
			DWORD	col;		// color
			float		tx, ty;	// texture coordinates
		};"

	self
		defineField: #x type: FLOATField new;
		defineField: #y type: FLOATField new;
		defineField: #z type: FLOATField new;
		defineField: #col type: DWORDField new;
		defineField: #tx type: FLOATField new;
		defineField: #ty type: FLOATField new! !
!HGEVertex class categoriesFor: #defineFields!initializing!public! !

HGEChannel guid: (GUID fromString: '{1932C934-3B13-4A17-8A39-0896D763E80D}')!
HGEChannel comment: 'HGEChannel is a wrapper class for the HCHANNEL handle type.'!
!HGEChannel categoriesForClass!External-Data-Structured! !
!HGEChannel class methodsFor!

new
	"You're not supposed to instantiate the receiver directly, instead you get instances with various
	HGE methods (like #effectPlay:) or for example when you call HGEEffect>>play."

	^self shouldNotImplement! !
!HGEChannel class categoriesFor: #new!instance creation!public! !

HGEHandle guid: (GUID fromString: '{8220FF56-CACC-4BF4-A58E-C8C62F99006F}')!
HGEHandle comment: 'HGEHandle is an abstract class of DWORD to represent various HGE handle types.'!
!HGEHandle categoriesForClass!External-Data-Structured! !
!HGEHandle methodsFor!

basicFree
	"Private - Free external resources owned by the receiver."

	Transcript
		show: self class printString , '>>basicFree';
		cr.
	needsToFreeExternalResources ifTrue: [self freeExternalResources].
	hge release!

freeExternalResources
	"Private - Free external resources owned by the receiver."

	^self subclassResponsibility!

hge
	"Private - Answer the receiver's HGE library."

	^hge!

initialize
	"Private - Initialize the receiver.
	These objects are finalizable since they may reference owned external resources."

	hge := HGELibrary default hgeCreate: HGE_VERSION.
	needsToFreeExternalResources := true.
	self beFinalizable!

loadFrom: anAddress length: anInteger 
	"Loads a texture, sound effect, music or compressed audio stream from anAddress and sets
	the receiver's value accordingly."

	^self subclassResponsibility!

needsFree
	"Private - Answer whether the receiver requires freeing of any external resources."

	^true!

needsToFreeExternalResources: aBoolean 
	"Private - Set the receiver's needsToFreeExternalResources instance variable."

	needsToFreeExternalResources := aBoolean! !
!HGEHandle categoriesFor: #basicFree!private!realizing/unrealizing! !
!HGEHandle categoriesFor: #freeExternalResources!private!realizing/unrealizing! !
!HGEHandle categoriesFor: #hge!accessing!private! !
!HGEHandle categoriesFor: #initialize!initializing!private! !
!HGEHandle categoriesFor: #loadFrom:length:!public! !
!HGEHandle categoriesFor: #needsFree!private!realizing/unrealizing! !
!HGEHandle categoriesFor: #needsToFreeExternalResources:!accessing!private! !

!HGEHandle class methodsFor!

fromAddress: anAddress 
	"Should not implement. Use #fromAddress:length: or one of their derivatives."

	^self shouldNotImplement!

fromAddress: anAddress length: anInteger 
	"Answer a new instance of the receiver from anAddress with size anInteger"

	^self new loadFrom: anAddress length: anInteger!

fromBytes: aByteObject 
	"Answer a new instance of the receiver from aByteObject"

	^self fromAddress: aByteObject length: aByteObject size!

fromFile: aString 
	"Answer a new instance of the receiver from the file named aString."

	^self fromAddress: aString length: 0! !
!HGEHandle class categoriesFor: #fromAddress:!instance creation!public! !
!HGEHandle class categoriesFor: #fromAddress:length:!instance creation!public! !
!HGEHandle class categoriesFor: #fromBytes:!instance creation!public! !
!HGEHandle class categoriesFor: #fromFile:!instance creation!public! !

HGEEffect guid: (GUID fromString: '{32E00EB2-8C3C-4D4E-811A-6A3022AF9FFC}')!
HGEEffect comment: 'HGEEffect is a wrapper class for the HEFFECT handle type.'!
!HGEEffect categoriesForClass!External-Data-Structured! !
!HGEEffect methodsFor!

freeExternalResources
	"Private - Free external resources owned by the receiver."

	self value ~= 0 ifTrue: [hge effectFree: self value]!

loadFrom: anAddress length: anInteger 
	"Loads a sound effect from anAddress and sets the receiver's value accordingly.
	For information on the arguments see HGE>>effectLoad:
	TODO: move to superclass."

	(self value: (hge effectLoad: anAddress size: anInteger)) = 0 
		ifTrue: [HGEError signal: hge systemGetErrorMessage]!

play
	"Plays the receiver."

	^hge effectPlay: self value!

playWithVolume: volInteger panning: panInteger pitch: aFloat 
	"Plays the receiver with additional parameters (see HGE>>effectPlayEx:volume:pan:pitch:).
	We provide this method as usually you don't want to loop the effect."

	^hge 
		effectPlayEx: self value
		volume: volInteger
		pan: panInteger
		pitch: aFloat!

playWithVolume: volInteger panning: panInteger pitch: aFloat loop: aBoolean 
	"Plays the receiver with additional parameters (see HGE>>effectPlayEx:volume:pan:pitch:)."

	^hge 
		effectPlayEx: self value
		volume: volInteger
		pan: panInteger
		pitch: aFloat
		loop: aBoolean! !
!HGEEffect categoriesFor: #freeExternalResources!private!realizing/unrealizing! !
!HGEEffect categoriesFor: #loadFrom:length:!public! !
!HGEEffect categoriesFor: #play!public!sound effect! !
!HGEEffect categoriesFor: #playWithVolume:panning:pitch:!public!sound effect! !
!HGEEffect categoriesFor: #playWithVolume:panning:pitch:loop:!public!sound effect! !

HGETarget guid: (GUID fromString: '{EF1E3AE0-0D28-4215-9CEB-21F22199F7C1}')!
HGETarget comment: 'HGETarget is a wrapper class for the HTARGET handle type.'!
!HGETarget categoriesForClass!External-Data-Structured! !
!HGETarget methodsFor!

freeExternalResources
	"Private - Free external resources owned by the receiver."

	self value ~= 0 ifTrue: [hge targetFree: self value]!

texture
	"Answer the receiver's texture.
	Note that render target's textures should not be freed!!"

	^(hge targetGetTexture: self value) asHGETexture needsToFreeExternalResources: false! !
!HGETarget categoriesFor: #freeExternalResources!private!realizing/unrealizing! !
!HGETarget categoriesFor: #texture!public! !

!HGETarget class methodsFor!

extent: aPoint 
	"Answer an instance of the receiver of the specified size."

	^self extent: aPoint zBuffer: false!

extent: aPoint zBuffer: aBoolean 
	"Answer an instance of the receiver of the specified size with a Z-buffer created and
	used for rendering to the render target according to aBoolean."

	^self 
		width: aPoint x
		height: aPoint y
		zBuffer: aBoolean!

width: width height: height 
	"Answer an instance of the receiver of the specified size."

	^self 
		width: width
		height: height
		zBuffer: false!

width: width height: height zBuffer: aBoolean 
	"Answer an instance of the receiver of the specified size with a Z-buffer created and
	used for rendering to the render target according to aBoolean."

	| target |
	target := self new.
	(target value: (target hge 
				targetCreate: width
				height: height
				zbuffer: aBoolean)) 
		= 0 ifTrue: [HGEError signal: target hge systemGetErrorMessage].
	^target! !
!HGETarget class categoriesFor: #extent:!public! !
!HGETarget class categoriesFor: #extent:zBuffer:!public! !
!HGETarget class categoriesFor: #width:height:!instance creation!public! !
!HGETarget class categoriesFor: #width:height:zBuffer:!instance creation!public! !

HGETexture guid: (GUID fromString: '{DF4ED350-EC0C-4E7A-8C5B-1A2837E385F2}')!
HGETexture comment: 'HGETexture is a wrapper class for the HTEXTURE handle type.'!
!HGETexture categoriesForClass!External-Data-Structured! !
!HGETexture methodsFor!

freeExternalResources
	"Private - Free external resources owned by the receiver."

	self value ~= 0 ifTrue: [hge textureFree: self value]!

height
	"Answer the height of the receiver."

	^hge textureGetHeight: self value!

loadFrom: anAddress length: anInteger 
	"Loads a texture from anAddress and sets the receiver's value accordingly.
	For information on the arguments see HGE>>textureLoad:
	TODO: move to superclass."

	self 
		loadFrom: anAddress
		length: anInteger
		mipmap: false!

loadFrom: anAddress length: anInteger mipmap: aBoolean 
	"Loads a texture from anAddress and sets the receiver's value accordingly.
	For information on the arguments see HGE>>textureLoad:"

	(self value: (hge 
				textureLoad: anAddress
				size: anInteger
				bMipmap: aBoolean)) = 0 
		ifTrue: [HGEError signal: hge systemGetErrorMessage]!

rectangle
	"Answer the receiver's rectangle."

	^Rectangle origin: 0 @ 0 extent: self width @ self height!

width
	"Answer the width of the receiver."

	^hge textureGetWidth: self value! !
!HGETexture categoriesFor: #freeExternalResources!private!realizing/unrealizing! !
!HGETexture categoriesFor: #height!public! !
!HGETexture categoriesFor: #loadFrom:length:!public! !
!HGETexture categoriesFor: #loadFrom:length:mipmap:!public! !
!HGETexture categoriesFor: #rectangle!public! !
!HGETexture categoriesFor: #width!public! !

!HGETexture class methodsFor!

extent: aPoint 
	"Answer an empty instance of the receiver of the specified size."

	^self width: aPoint x height: aPoint y!

fromAddress: anAddress length: anInteger mipmap: aBoolean 
	"Answer a new instance of the receiver from anAddress with size anInteger and mipmap
	levels created according to aBoolean."

	^self new 
		loadFrom: anAddress
		length: anInteger
		mipmap: aBoolean!

fromBytes: aByteObject mipmap: aBoolean 
	"Answer a new instance of the receiver from aByteObject and mipmap levels created
	according to aBoolean."

	^self 
		fromAddress: aByteObject
		length: aByteObject size
		mipmap: aBoolean!

fromFile: aString mipmap: aBoolean 
	"Answer a new instance of the receiver from the file named aString and mipmap levels
	created according to aBoolean."

	^self 
		fromAddress: aString
		length: 0
		mipmap: aBoolean!

width: width height: height 
	"Answer an empty instance of the receiver of the specified size."

	| texture |
	texture := self new.
	^texture value: (texture hge textureCreate: width height: height)! !
!HGETexture class categoriesFor: #extent:!public! !
!HGETexture class categoriesFor: #fromAddress:length:mipmap:!instance creation!public! !
!HGETexture class categoriesFor: #fromBytes:mipmap:!instance creation!public! !
!HGETexture class categoriesFor: #fromFile:mipmap:!instance creation!public! !
!HGETexture class categoriesFor: #width:height:!instance creation!public! !

HGERandom guid: (GUID fromString: '{F4144710-C703-4E0F-BF47-BAC7CAA854D6}')!
HGERandom comment: 'HGERandom is an abstract class for the HGERandomFloat and HGERandomInteger concrete subclasses that generate float and integer random numbers respectively. Attempts to instantiate HGERandom will answer an instance of the default generator, an HGERandomFloat.'!
!HGERandom categoriesForClass!Collections-Streams! !
!HGERandom methodsFor!

between: min and: max 
	"Tells HGERandom to produce pseudo-random sequences between min and max (inclusive)."

	minNumber := min.
	maxNumber := max!

initialize
	"Private - Initialize the receiver."

	self between: 0 and: 1!

next
	"Answer a pseudo-random number between minNumber and maxNumber."

	^next isNil 
		ifTrue: 
			[Seed := (214013 * Seed + 2531011) asDword.
			self nextRandom]
		ifFalse: 
			[| answer |
			answer := next.
			next := nil.
			answer]!

nextRandom
	"Private - Answer a pseudo-random number between minNumber and maxNumber."

	^self subclassResponsibility!

peek
	"Answer a pseudo-random number between minNumber and maxNumber generated
	from the next seed, but do not advance down the stream (i.e. self peek = self peek)."

	"Implementation Note: We use the next inst. var to as a buffer for the next value."

	^next ifNil: [next := self next]!

seed: anInteger 
	"Set the seed of the HGERandom stream to anInteger (or use the system clock if zero)."

	anInteger = 0 ifTrue: [Seed := self class systemClockSeed] ifFalse: [Seed := anInteger]! !
!HGERandom categoriesFor: #between:and:!acessing!public! !
!HGERandom categoriesFor: #initialize!initializing!private! !
!HGERandom categoriesFor: #next!acessing!public! !
!HGERandom categoriesFor: #nextRandom!acessing!private! !
!HGERandom categoriesFor: #peek!acessing!public! !
!HGERandom categoriesFor: #seed:!acessing!public! !

!HGERandom class methodsFor!

between: min and: max 
	"Answer a new HGERandom number generator (seeded off the system clock if the
	receiver was never used before or use the last Seed value instead) that produces
	pseudo-random sequences between min and max (inclusive)."

	^self new between: min and: max!

isAbstract
	"HGERandom is an abstract class."

	^self == ##(self)!

new
	"Answer a new HGERandom number generator seeded off the system clock if the
	receiver was never used before or use the last Seed value instead."

	^Seed isNil ifTrue: [self seed: 0] ifFalse: [self seed: Seed]!

newDefault
	"Private - Answer the default random number generator (an HGERandomFloat)."

	^HGERandomFloat new!

seed: anInteger 
	"Anwer a new HGERandom stream with the initial seed anInteger."

	^(super seed: anInteger) initialize!

systemClockSeed
	"Answer the current value of the system millisecond clock."

	^Delay millisecondClockValue! !
!HGERandom class categoriesFor: #between:and:!instance creation!public! !
!HGERandom class categoriesFor: #isAbstract!public!Testing! !
!HGERandom class categoriesFor: #new!instance creation!public! !
!HGERandom class categoriesFor: #newDefault!instance creation!private! !
!HGERandom class categoriesFor: #seed:!instance creation!public! !
!HGERandom class categoriesFor: #systemClockSeed!enquiries!public! !

HGERandomFloat guid: (GUID fromString: '{902FF007-1E37-4D2D-B41A-08AE58DE2B6A}')!
HGERandomFloat comment: 'HGERandomFloat is a wrapper class to encapsulate the functionality provided by the HGE>>randomFloat:max: method (althought we directly implement the algorithm in Smalltalk).'!
!HGERandomFloat categoriesForClass!Collections-Streams! !
!HGERandomFloat methodsFor!

nextRandom
	"Private - Answer the next <Float> between minNumber and maxNumber in the
	pseudo-random number stream."

	^(minNumber + ((Seed bitShift: -16) * (1 / 65535) * (maxNumber - minNumber))) asFloat! !
!HGERandomFloat categoriesFor: #nextRandom!acessing!private! !

HGERandomInteger guid: (GUID fromString: '{A0675D70-286E-4722-B312-37FBDA552D89}')!
HGERandomInteger comment: 'HGERandomInteger is a wrapper class to encapsulate the functionality provided by the HGE>>randomInt:max: method (althought we directly implement the algorithm in Smalltalk).'!
!HGERandomInteger categoriesForClass!Collections-Streams! !
!HGERandomInteger methodsFor!

nextRandom
	"Private - Answer the next <Integer> between minNumber and maxNumber in the
	pseudo-random number stream."

	^minNumber + ((Seed bitXor: (Seed bitShift: -15)) rem: maxNumber - minNumber + 1)! !
!HGERandomInteger categoriesFor: #nextRandom!accessing!private! !

"Binary Globals"!

