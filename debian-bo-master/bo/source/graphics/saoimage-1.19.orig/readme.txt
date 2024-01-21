readme.txt -- SAOimage information and installation instructions

User documentation:

	The main source is manual.tex, a LaTeX document in the doc
subdirectory.
	There is a UNIX man page which may be installed, but it is not
as complete as the user manual.
	There is also some obsolete user documentation organized by
topic in ASCII text files in the doc subdirectory.  The cmdline.txt
file describes switches used when bringing up saoimage at runtime,
and useable in the command line pop-up window.

To build SAOimage under VMS:

	$ @make

	Refer to readme.vms in the vms subdirectory for details.

To build SAOimage under UNIX:

	% cp makefile.<machine> makefile    # copy machine-specific makefile
	% make

Choosing a makefile:
	There are several makefiles to choose from: .apo, .dec, .hp,
.ibm, .mips, .sgi, and .sun.  makefile.sun expects the most in terms of
system utilities and up-to date libraries.  These are unlikely to be
available on non-Sun machines.  makefile.hp expects the least, and should
work on machines as diverse as Apple, Convex, Cray, and Masscomp; it is an
example of a makefile for a System V system.  makefile.dec is similar
to makefile.hp, but for machines with LSB byte order (this only affects
reading FITS files).

Editing the makefile:
	After choosing a closest makefile and copying it to "makefile",
you may wish to make further adjustments.  All changes are at the top,
so don't be discouraged by the size of the makefile.  The switches are
described at the top of the makefile.
	If you do not use IRAF, delete the -DIMTOOL switch from IFLAGS.
If you leave this switch in, but don't have IRAF installed, SAOimage
will give a nasty warning message every time you bring it up.
	If you don't have a PostScript printer, you may omit the
-PSCRIPT switch.
	If you have gcc, by all means use it!  It produces a much
better executable than CC.  To use gcc, you must remove the "#"
signs from the two gcc lines.  You may add further optimization
switches to the gcc line if you wish.

A potentially editless alternative to selecting a makefile:
	A driver makefile exists that sets architecture-dependent flags
for an assortment of popular (and supported) architectures.  In a mixed-
architecture environment, it is probably simpler to use the entries in
the master makefile, and let this file select the appropriate machine-
dependent makefile.

	% cp makefile.main makefile	    # copy master makefile
	% make				    # lists possible architectures
	% make <architecture>		    # do it

If you are making your own changes to the source code:
	To add new image formats, just add appropriate calls in
init_image() (to return dimensions) and load_image() (to load the
passed buffer) in imgread.c.  You should add a commandline switch
needed to recognize your image in parse_filetype() in cmdimage.c.
If your image file names have a unique suffix, you may use that
to recognize the file type in check_image() in imgcheck.c.
	To add remote IO from other processes, use one of the
connection records in hfiles/control.h (initialized defs/control.def)
and follow the example in irafio.c.  It is very simple: you just put
the pointer to your callback in the record and open the connection.
	To add or change buttons, you must edit the ____.mnu files
in the panel subdirectory.  You must look at the hfiles in btnlib
and btnlib/tool to understand how the buttons are described.  You
may use text strings and/or your own bitmaps.  To make new buttons,
compile the library in btnlib/tool and then do the make in the panel
subdirectory.  The button font is in the code, it does not use any
runtime resources.

Acknowledgements:
	SAOimage originated with showimg under X10.  Showimg was
written by Bill Wyatt, and has continued to be extend under X11 by
Doug Mink.  The user interface ideas for SAOimage were developed
in collaboration with Richard Burg and Eric Mandell.  Jay
Travisano did the VMS port and has continued to support it as
SAOimage evolved.  John Roll has put work into the remote IO.
	Rich Burg and Jay Travisano are both with the Space Telescope
Science Institute.  The others mentioned are all with the Harvard
Smithsonian Center for Astrophysics.
	Numerous others have contributed bug fixes, porting information,
and helpful suggestions.  They are credited in the source files where
their contributions appear.  

Copyright:
	SAOimage is copyrighted by the Smithsonian Institution.  It
is available without charge, as is, to anyone who wishes to use it.
The Smithsonian takes no responsibility for the appropriateness of
the code for any application, the timeliness of bug fixes, or the
continued compatablility of future versions with any code linked to
this program.
	Others are free to use, distribute, add to, or change the code.
In fact fixes, extensions, and improvements are encouraged.  We would
appreciate donations of such code for inclusion in the generally
available version.
	You may identify yourself with your added code and offer it
under whatever arrangement you choose.   But you must keep the
Smithsonian's copyright on original source code.  You may not
represent this program as anything other than freely available code
made available by the Smithsonian Astrophysical Observatory.
	Developers should identify themselves to SAO so they can be
notified in advance of impending changes, receive advance copies of
development code, and be informed of others doing similar work for
possible collaboration.

email address:
vanhilst@cs.washington.edu (INTERNET)
mvh@cfa.harvard.edu (INTERNET)
mvh@cfa (BITNET)
6699::VANHILST (SPAN)

