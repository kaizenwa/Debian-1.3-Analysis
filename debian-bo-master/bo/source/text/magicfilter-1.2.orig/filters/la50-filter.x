%include <filters.h>
#
# Magic filter setup file for the DEC LA-50 printer
# THIS FILE IS UNTESTED!
#
# This file is in the public domain.
#
# This file has been automatically adapted to your system.
%ifndef HAVE_GHOSTSCRIPT
# However, since you didn't have GhostScript installed, most entries
# are going to be rejects.
%endif
#
/*
 * Printer resolution
 * DPI is set to the lowest common denominator of XDPI and YDPI
 */
%define XDPI 144
%define YDPI 72
%define DPI 144
/*
 * PostScript
 */
%ifdef HAVE_GHOSTSCRIPT
%define HANDLE_PS	filter	PATH_GHOSTSCRIPT -q -dSAFER -dNOPAUSE -r144x72 -sDEVICE=la50 -sOutputFile=- -
%endif
/*
 * Text
 */
%define HANDLE_TEXT	text
/*
 * Native here to override PCL magic
 */
# wild guess: native control codes start with <ESC>
0       \033            cat

%include <stdconv.mh>

# Default entry -- for normal (text) files.  MUST BE LAST.
default                 HANDLE_TEXT
