.\" -*- nroff -*-
.\" Like TP, but if specified indent is more than half
.\" the current line-length - indent, use the default indent.
.de Tp
.ie \\n(.$=0:((0\\$1)*2u>(\\n(.lu-\\n(.iu)) .TP
.el .TP "\\$1"
..
.TH VTPRTOFF MAN1_SECT "VER_DY VER_MO VER_YR" "vtprint vVERSION_NO"
.SH NAME
vtprtoff,vtprton \- turn off or on printer attached to local terminal
.SH SYNOPSIS
.B vtprtoff
[
.B \-dDeEhlqQNtvw
] [
.B \-L
.I vtprintcap
] [
.B \-T
.I termtype
] [
.B \-V
.I device
]
.PP
.B vtprton
[
.B \-dDeEhlqQNtvw
] [
.B \-L
.I vtprintcap
] [
.B \-T
.I termtype
] [
.B \-V
.I device
]
.SH DESCRIPTION
.B vtprtoff
is a simple program that turns off (or on) redirection of ASCII text to
a printer connected to a 
terminal or terminal emulator.  It uses terminal escape sequences to
achieve this end.
.SH OPTIONS
.TP
.B \-d
Use the tty device file instead of stdout for output.  This is defined as
.B DEVTTY
on this system. (This is operating system specific and may vary from system to
system.)
.TP
.B \-D
Use the stdout stream for output, rather than a tty device file.
.TP
.B \-e
Forces \fBvtprtoff\fP to ignore the TERM environment variable and use
the builtin control codes instead.
.TP
.B \-E
Undoes the effect of the \fB\-e\fP option, which allows \fBvtprtoff\fP to
attempt to find an entry in \fBLIBFILE\fP for the TERM value and use the
corresponding control codes.
.TP
.B \-h
Print out a simple usage message.
.TP
.B \-l
Print out the license agreement.
.TP
.BI \-L " vtprintcap"
Specifies an alternate file to use instead of \fBLIBFILE\fP.
.TP
.B \-q
Quiet mode.  Suppress various status messages from being displayed.
.TP
.B \-Q
Cancels the effect of the \fB\-q\fP option, allowing normal progress 
reporting to occur.
.TP
.B \-t
Force the use of the TERM variable to perform a look up of the control
sequences to be used in \fBLIBFILE\fP.  If an entry in that file can't
be found, \fBvtprtoff\fP will abort with an error.
.TP
.BI \-T " termtype"
Use the value of \fItermtype\fP instead of the value of the TERM environment
variable when performing lookups in \fBLIBFILE\fP.
.TP
.B \-v
Display version information.
.TP
.BI \-V " device"
Specifies an alternate device file instead of \fBDEVTTY\fP to use.
.TP
.B \-w
Display important warranty waiver information.  NO WARRANTY!
.SH ENVIRONMENT
.IP TERM
the type of the terminal 
.B vtprtoff
will assume is being used.
.IP VTPRINT
string of options to be used by
.B vtprtoff.
.SH AUTHOR
.B vtprtoff
was written by Garrett D'Amore, on June 5, 1994 and last modified on
VER_MO VER_DY, VER_YR.  It is heavily derived
from a similar program, called
.B lprint,
by the same author.
He can be reached via Internet e-mail at garrett@sciences.sdsu.edu.
.SH DISTRIBUTION
The latest version of
.B vtprtoff
is available as part of the
.B vtprint
package, and 
can be obtained by anonymous ftp at ftp.sdsu.edu in the
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
.SH COPYRIGHT
.B vtprtoff
is copyrighted 1994, by Garrett D'Amore.  It may be freely redistributed
or modified, so long as this and any other copyright notices are included
in their original form with the program.  The user is granted the right to 
use this program without limitation.
.SH DISCLAIMER
.B vtprtoff
is provided WITHOUT WARRANTY.  The user agrees to indemnify
the author from any claims of damage or loss arising from the use
of or inability to use this program.  In other words, USE AT YOUR
OWN RISK! 
.SH FILES
.B LIBFILE
.SH SEE ALSO
.B "LPR_BIN\fR(LPR_SECT), "
.B "vtprint\fR(MAN1_SECT), "
.B "vtprintcap\fR(MAN5_SECT)"
