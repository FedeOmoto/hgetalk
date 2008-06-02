This version of HGE was modified to use DirectX 9, contains a few bug
fixes for bugs I've found in the process of porting it to Smalltalk and
uses the latest zlib and libpng versions (upgraded from zlib 1.2.1 to 1.2.3
and from libpng 1.2.8 to 1.2.29).
It also contains modifications to be callable from Dolphin Smalltalk
(exported functions in the DLL for the various helper classes constructors
and made all C++ methods virtual).
Although HGE supports various C++ compilers the one I'm using is Visual
C++ 2008 Express Edition, wich is free and lets you generate DLLs. It's
the one I recommend as I provide project/solution files for this compiler.

If you have questions, suggestions, bug reports or if you want to join
the HGETalk project please send me an E-Mail at:
federico.omoto@gmail.com

The project is hosted at Google Code, you can also join from there:
http://code.google.com/p/hgetalk/
