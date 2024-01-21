tofrodos Ver 1.0
Copyright (c) 1996 by Christopher S L Heng. All rights reserved.
----------------------------------------------------------------

$Id: readme.txt 1.2 1996/06/22 20:01:32 chris Exp $ /*


CONTENTS
--------

1. WHAT IS TOFRODOS?
2. HOW TO INSTALL TOFRODOS?
3. PORTING TO OTHER SYSTEMS OR COMPILERS
4. COPYRIGHT AND CONTACTING THE AUTHOR


1. WHAT IS TOFRODOS?
--------------------

DOS text files traditionally have CR/LF (carriage return/line feed) pairs
as their new line delimiters while Unix text files traditionally have
LFs (line feeds) to terminate each line.

tofrodos comprises two programs, "fromdos" and "todos", which convert
text files to and from these formats. Use "fromdos" to convert DOS
text files to the Unix format, and "todos" to convert Unix text files
to the DOS format.


2. HOW TO INSTALL TOFRODOS?
---------------------------

The tofrodos package comes with sources, documentation and binaries for
both Linux and DOS.

To install the DOS versions, simply copy todos.exe and fromdos.exe to
a directory on your path. You will have to read the manual page from
a Unix system (like Linux) or use a DOS program that can read Unix manual
pages. For a short summary of options, run the program with the -h or
/h option. And of course, you can always read the source code.

To install the Linux versions, you have to find out if your system
supports ELF. If it does, use the todos.elf and fromdos.elf binaries.
These probably require the shared libraries 5.2.18 or above since I
compiled them with 5.2.18. To install, do the following:

	make install.elf

If your system can only run a.out binaries, you probably have to use
todos.out and fromdos.out. I compiled these binaries with 4.7.2 of
the libraries and they probably require at least these versions.
To install, do the following:

	make install.out

If you are not sure whether you have an ELF or a.out system, or if you
like to recompile the binaries yourself, simply do

	make install

This will recompile and install the binaries. To install them somewhere
other than /usr/local/bin, redefine BINDIR and MANDIR for the binary and
manual page destinations respectively. For example, to install the
binaries in /usr/bin and the manual page in /usr/man/man1, do

	make BINDIR=/usr/bin MANDIR=/usr/man/man1 install

Or, if you like, you can always compile with "make all" and install the
binaries and manual page yourself manually.

To read the manual page, simply type

	man fromdos


3. PORTING TO OTHER SYSTEMS OR COMPILERS
----------------------------------------

If you want to compile tofrodos for a system other than Linux, you may
or may not have some work in store for you. The program is actually
VERY trivial, so the work you need to do is probably (hopefully) minimal.

The first place to look into is probably the config.h file, where I
tried to place as much system and compiler macros as I could bother.

If you are compiling on other Unix systems, tweaking the config.h file
macros may well be all that you need to do. I don't know. I never tried
it on any Unix system or Unix-clone other than Linux. Let me know how
it works out, especially if you had to hack something other than changing
the values of the macros in config.h to get it to work.

If you are compiling for DOS, you may need to do some work. Depending on
your compiler, you may need to find replacements for some of the functions
which I assume are present in the compiler's default libraries. I tried
to identify potential missing functions in the config.h file, but it is
probably not complete. Remember also that if you want the program to
handle wildcard arguments your runtime system must expand command line
arguments before passing them to main(), since COMMAND.COM does not
automatically expand wildcards the way Unix shells do. If you use
Borland C/C++ 3.1, this probably means that you will need to link with
wildargs.obj. If you use Watcom C/C++ you will need to use the source
for wildargv.c (in the watcom SRC directory). Please don't ask me how
to get the program to compile/link for DOS for any compiler. If you
run into too much trouble compiling it for DOS, I would suggest that
you either use my precompiled executables or just rewrite the whole
program. The program is too simple to waste time trying to port to
any brain-dead system or compiler.


4. COPYRIGHT AND CONTACTING THE AUTHOR
--------------------------------------

The program and its accompanying files and documentation are
Copyright (c) 1996 by Christopher S L Heng. All rights reserved.
They are distributed under the terms of the GNU General Public License
Version 2, a copy of which is enclosed in this package in the file COPYING.

You can contact me at the following email addresses:
	ChristopherHeng@pobox.org.sg
	cyfheng@singnet.com.sg

While I generally like to hear from you if you are using this program,
especially if you find any bugs in it, I make no promises about fixing
anything or even replying.
