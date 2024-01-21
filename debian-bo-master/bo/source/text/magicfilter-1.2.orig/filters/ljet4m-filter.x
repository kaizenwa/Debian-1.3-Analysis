%include <filters.h>
#
# Magic filter setup file for 600 DPI LaserJet 4 series with PostScript
# (i.e. LJ 4MP, 4M, 4M+, 4MV or 4SiMX or 4P, 4, 4+, 4V or 4Si with PostScript
# option installed).
#
# This file has been automatically adapted to your system.
#
/*
 * Printer resolution
 */
%define DPI 600
/*
 * PostScript
 */
%define NATIVE_PS 1
/*
 * PCL, PJL
 */
%define HANDLE_PCL5E	cat
%define HANDLE_PCL	cat
%define HANDLE_PJL	cat
/*
 * Text
 */
%define HANDLE_TEXT	cat	\eE\e&k2G\e(0N	\eE
%define LATIN1_OK

%include <stdconv.mh>

# wild guess: PCL control codes start with <ESC>
0	\033		HANDLE_PCL

# Default entry -- for normal (text) files.  MUST BE LAST.
default			HANDLE_TEXT
