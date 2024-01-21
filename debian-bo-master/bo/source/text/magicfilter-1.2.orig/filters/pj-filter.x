%include <filters.h>
#
# Magic filter setup file for the HP PaintJet
# THIS FILE IS UNTESTED!
#
# This file has been automatically adapted to your system.
%ifndef HAVE_GHOSTSCRIPT
# However, since you didn't have GhostScript installed, most entries
# are going to be rejects.
%endif
#
/*
 * Printer resolution
 */
%define DPI 180
%define IS_COLOR 1
/*
 * PostScript
 */
%ifdef HAVE_GHOSTSCRIPT
%define HANDLE_PS	filter	PATH_GHOSTSCRIPT -q -dSAFER -dNOPAUSE -r180 -sDEVICE=pj -sOutputFile=- -
%endif
/*
 * PCL, PJL
 */
%define HANDLE_PCL	cat
/*
 * Text
 */
%define HANDLE_TEXT	cat	\eE\e&k2G\e(0N	\eE
%define LATIN1_OK 1

%include <stdconv.mh>

# wild guess: PCL control codes start with <ESC>
0	\033		HANDLE_PCL

# Default entry -- for normal (text) files.  MUST BE LAST.
default			HANDLE_TEXT
