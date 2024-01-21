
This software is distributed under the terms of the GNU Library
General Public License, see the LICENSE file for details.

What do you need ?

	- A PC with a 3Dfx Voodoo based board (Diamond Monster 3D, R3D, Obsidian, etc.);

	- The Mesa Library 2.2;

	- The Glide library 2.1.1 (the driver probably works also with some
	  older version, you can find it at www.3dfx.com) for your OS:
		Win95 (tested by me and other people),
	  	NT (successfully tested by Henri Fousse <arnaud@pobox.oleane.com>),
		MS-DOS,
		etc.;

	- A compiler supported by the Glide library (Micro$oft VC++ (the only
	  compiler tested), Watcom, etc.);

	- A lot of patience, this is an alpha release.


What is able to do ?

	- It is able accelerate points, lines and polygon with flat
	  shading, gouraud shading, Z-buffer, texture mapping (with several limitations) and
	  also the first release is quite fast;

How to compile ?

	- Setup	an environment where you can compile a Glide based program (you
	  can find all the instruction in the Glide documentation);

	- Copy the driver files (mesa3dfx/src/fxmesa.c, mesa3dfx/src/fxtri.h, mesa3dfx/src/fxline.h,
	  mesa3dfx/src/fxpnt.h) in the mesa src directory (Mesa-2.2/src);

	- Remember to define (for example with the /D option on the VC++) in your
	  Makefile:
		__MSC__ (or some other symbol, read the Glide
		documentation for more info);

		FX (for 3Dfx Mesa driver);

	- Compile all the Mesa-2.2/src/*.c files to obtain the mesa.lib;

	- Compile all the Mesa-2.2/src-glu/*.c files to obtain the glu.lib,
	  (I have encountered some trouble compiling the library with optimization on);

	- Compile all the tk/*.c files to obtain the tk.lib (it is basically a tklib emulator)

	- Now your are ready to compile any OpenGL program :)

	- Remember to link the glu.lib, mesa.lib and the Glide library to your exes;

	- To discover how open the 3Dfx screen, read the sources under
	  the demos directory (otherwise you can use the tklib emulator).


Know BUGS and problems:

	- only square texture supported

	- maximum texture size: 256x256 (this is an hardware limit)

	- only 640x480 supported

	- only one context supported

	- the depth buffer works only with glPerspective and glFrustrum (the driver depth buffer
	works in homogeneous coordiantes and the w coord is always 1.0 with glOrtho !)

Did you find a lot BUGs and problems ? Good, send me an email.


David Bucciarelli (tech.hmw@plus.it)

Humanware s.r.l. 
Via XXIV Maggio 62
Pisa, Italy
Tel./Fax +39-50-554108
email: info.hmw@plus.it

