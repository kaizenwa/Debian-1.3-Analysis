.\" -*- nroff -*-
.\" Like TP, but if specified indent is more than half
.\" the current line-length - indent, use the default indent.
.de Tp
.ie \\n(.$=0:((0\\$1)*2u>(\\n(.lu-\\n(.iu)) .TP
.el .TP "\\$1"
..
.TH VTPRINTCAP MAN5_SECT "VER_DY VER_MO VER_YR" "vtprint vVERSION_NO"
.SH NAME
vtprintcap \- database of terminal printer control codes
.SH SYNOPSIS
.B LIBFILE
.SH DESCRIPTION
.B vtprintcap
is a simple flat file database containing a list of the names of various
terminals and the codes those terminals use to start and stop an attached
printer (media hard copy). This database is used by \fBvtprint\fP(1) and
related programs.
.PP
Each entry is composed of three lines.  The first line is a comma-delimited
list of names which may be used to reference this entry.  The second line
the sequence of characters used by the terminal to turn on an attached 
hard copy device and redirect output to that device.   The third line contains
the sequence of characters used to turn the device off and restore normal
output to the screen.
.PP
An entry must have all three lines to be considered valid.  Currently no
comment lines are permitted to exist between these lines: such lines
would be interpreted as the printer control codes themselves! 
.PP
The rules for selecting a terminal name are simple: you must follow the
same rules as for \fBTERMCAP\fP(MAN5_SECT) names.  Terminal names should
be entered in \fBLIBFILE\fP with exactly the same names as are
in \fBTERMCAP\fP.
.PP
The control code strings in the second and third lines of an entry are 
governed as follows: ANSI C style escapes for character
constants (e.g. \\n for newline) are permitted, constant strings are
permitted, and quotes may be used to as delimiters in the string.  (This
is useful if you wish you use a digit immediately following a numeric
escape. For example, ESC-5 should be represented as \\033"5" to avoid
confusion in with \\335.  In general, the rules outlined for ANSI 
constant escaping are enforced.
.PP
Additionally, \fBLIBFILE\fP may contain comment lines, which are indicated
by placing a hash mark (#) in the first column of the line.  Remember
however that entries must \fInot\fP be broken up by comment lines!
.SH EXAMPLE
Here is a sample entry common to DEC vt100 and vt102 terminals:
.PP
.IP
# Sample vtprintcap entry for vt100 & vt102 terminals
.br
vt100, vt102
.br
\\033[5i
.br
\\033[4i
.PP
This entry would be used whenever the user's TERM environment variable 
was set to "vt102" or "vt100", using ESC [5i and ESC [4i as the printer
control codes to turn a printer on and off, respectively.
.SH DISTRIBUTION
.B vtprintcap
is part of the 
.B vtprint
package, which can be obtained by anonymous ftp at ftp.sdsu.edu in the
.B /pub/vtprint
directory.  Please read the
.B INDEX
and
.B README
files before downloading.
.LP
If you do not have ftp availability, then you can request a
uuencoded copy of 
.B vtprint
be sent to you via e-mail from the author.  The author is also willing to
make other arrangements as needed, within certain limitations.
.SH FILES
.B TERMCAP
.br
.B LIBFILE
.SH BUGS
.B vtprintcap
handling code currently could get confused if a printer control code 
is exactly the same as the name of a terminal, or contained the name
of a terminal delimited by commas.  As this is very unlikely to every
occur, it shouldn't be a problem.
.SH SEE ALSO
.B "termcap\fR(MAN5_SECT)"
.B "vtprint\fR(MAN1_SECT)"
