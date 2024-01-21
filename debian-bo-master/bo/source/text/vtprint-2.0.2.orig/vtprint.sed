.\" -*- nroff -*-
.\" Like TP, but if specified indent is more than half
.\" the current line-length - indent, use the default indent.
.de Tp
.ie \\n(.$=0:((0\\$1)*2u>(\\n(.lu-\\n(.iu)) .TP
.el .TP "\\$1"
..
.TH VTPRINT MAN1_SECT "VER_DY VER_MO VER_YR" "vtprint vVERSION_NO"
.SH NAME
vtprint \- print files from UNIX host to printer attached to local terminal
.SH SYNOPSIS
.B vtprint
[
.B \-bBcCdDeEfFhlnNqQtvw
] [
.B \-L
.I vtprintcap
] [
.B \-T
.I termtype
] [
.B \-V
.I device
] [
.IR filename\.\.\.
]
.SH DESCRIPTION
.B vtprint
is a simple filter that prints ASCII text to a printer connected to a 
terminal or terminal emulator.  It uses terminal escape sequences to
print, and can substitute for 
.B "LPR_BIN\fR(LPR_SECT)"
in circumstances where the printer is not connected directly to the host or
available via TCP/IP.
.SH OPTIONS
.TP
.B \-b
Causes \fBvtprint\fP to open the output stream in binary modes, overriding
any automatic postprocessing done by the host (e.g. CR/LF translations).
This has no effect if the output stream is not a tty device.
.TP
.B \-B
Undoes the effect of the \fB\-b\fP option.  Note that this does \fInot\fP
open the output stream in text mode if it would not normally be opened in
that mode.
.TP
.B \-c
Causes \fBvtprint\fP
to append any linefeeds (ASCII 0xA) with carriage returns
(ASCII 0xD).  This is useful for printing data from a UNIX or similiar
host to an MS-DOS system's printer that expects CR/LF line termination.
.TP
.B \-C
Causes \fBvtprint\fP
to pass all carriage returns and linefeeds unmodified.
.TP
.B \-d
Use the tty device file instead of stdout for output.  This is defined as
.B DEVTTY
on this system. (This is operating system specific and may vary from system to
system.) This is useful for using \fBvtprint\fP
as a pipe called by programs that suppress or redirect the stdout stream
of the pipe or printing process.
.TP
.B \-D
Use the stdout stream for output, rather than a tty device file.
.TP
.B \-e
Forces \fBvtprint\fP to ignore the TERM environment variable and use
the builtin control codes instead.
.TP
.B \-E
Undoes the effect of the \fB\-e\fP option, which allows \fBvtprint\fP to
attempt to find an entry in \fBLIBFILE\fP for the TERM value and use the
corresponding control codes.
.TP
.B \-f
Requests \fBvtprint\fP to suppress inclusion of formfeeds (ASCII 0xC) between 
multiple files.  Note that this has no affect on any formfeeds that may
be present in the input files already.
.TP
.B \-F
Requests \fBvtprint\fP to include formfeeds between multiple files specified
on the command-line and at the end of the last file printed.
.TP
.B \-h
Print out a simple usage message.
.TP
.B \-l
Print out the \fBvtprint\fP license agreement.
.TP
.BI \-L " vtprintcap"
Specifies an alternate file to use instead of \fBLIBFILE\fP.
.TP
.B \-n
Causes \fBvtprint\fP to strip the CR from any CR/LF sequences.  This will
not affect handling of any CRs present elsewhere in the file.
.TP
.B \-N
Causes \fBvtprint\fP
to pass all carriage returns and linefeeds unmodified.
.TP
.B \-q
Quiet mode.  Suppress various status messages from being displayed, useful
in situations where \fBvtprint\fP
is used as part of a shell script, for example.
.TP
.B \-Q
Cancels the effect of the \fB\-q\fP option, allowing normal progress 
reporting to occur.
.TP
.B \-t
Force the use of the TERM variable to perform a look up of the control
sequences to be used in \fBLIBFILE\fP.  If an entry in that file can't
be found, \fBvtprint\fP will abort with an error.
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
.TP
.B \-\-
This option changes no special operational parameters of
.B vtprint.
But it does indicate to the program that all of the command-line arguments
which follow are to be parsed as filenames, even if they begin with a dash.
Hence to process a single file with the name "file" you would call 
.B vtprint
as "
\fBvtprint\fP  \-\-  \-file".
.SH ENVIRONMENT
.IP TERM
the type of the terminal 
.B vtprint
will assume is being used.
.IP VTPRINT
string of options to be used by
.B vtprint.
.SH AUTHOR
.B vtprint
was written by Garrett D'Amore, on December 27, 1993 and last modified on
VER_MO VER_DY, VER_YR.  It is heavily derived
from a similar program, called
.B lprint,
by the same author.
He can be reached via Internet e-mail at garrett@sciences.sdsu.edu.
.SH DISTRIBUTION
The latest version of
.B vtprint
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
.SH MAILING LIST
A mailing list for the discussion of topics related to and about
\fBvtprint\fP exists.  To subscribe to this mailing list, send a
message to "vtprint-request@sdsu.edu" with the word "subscribe" as
the body.  An automated list server will reply with information about
the mailing list.
.SH COPYRIGHT
.B vtprint
is copyrighted 1994, by Garrett D'Amore.  It may be freely redistributed
or modified, so long as this and any other copyright notices are included
in their original form with the program.  The user is granted the right to 
use this program without limitation.
.SH DISCLAIMER
.B vtprint
is provided WITHOUT WARRANTY.  The user agrees to indemnify
the author from any claims of damage or loss arising from the use
of or inability to use this program.  In other words, USE AT YOUR
OWN RISK! 
.SH CAVEATS
.B vtprint
cannot be used to print files containing non-printable characters, as
these may cause conflicts with the terminal or emulator used.
.PP
When used over a serial (modem) connection, \fBvtprint\fP may not work
properly unless hardware (CTS/RTS) flow control is used.  This is due to
the fact that most printers are slower than serial connections in common
use.  
.PP	
.B vtprint
uses terminal escape sequences to print, and not all
terminal emulators
support these sequences, and of those that do, some support it differently.
You can find a listing of some of the programs known to work and not to
work with
.B vtprint
in the file
.B COMPAT.
The best thing to do is try your particular emulator, and find out if
it works.  The author would appreciate any reports of
success or failure, or strange behavior.  You can send those to him via
Internet e-mail.  (See the
.B AUTHOR
chapter for how to reach him.)
.LP
The \fBLIBFILE\fP file is still largely incomplete.  The author would very
much appreciate it if individuals with more information different terminals
would get in contact with him so that support can be added for these terminals
in the master archive.
.LP
Do \fInot\fP
background this program, as it has no way of knowing when it is in the
background and when it is in the foreground.  This will result in your 
foreground process' output being printed as well as the output from
.B vtprint.
.SH FILES
.B LIBFILE
.SH TODOS
Fix any bugs present, clean-up the documentation, continue to build
upon entries in the \fBCOMPAT\fP and \fBLIBFILE\fP files, and  finally, 
finish getting a college education!
.SH BUGS
.B vtprint 
doesn't handle some signals properly, especially the SIGKILL and SIGSTOP
signals, which can't be caught anyway.  Also it doesn't check to see if
you have backgrounded the process.  The resulting condition may leave all
output directed to the printer instead of the screen.  The fix for this
condition is to run the provided
.B vtprtoff
program with no arguments, which will restore normal screen output.  (You
probably won't be able to see what you're typing while you start 
.B vtprtoff
from the shell.)
.SH SEE ALSO
.B "LPR_BIN\fR(LPR_SECT), "
.B "vtprintcap\fR(MAN5_SECT), "
.B "vtprtoff\fR(MAN1_SECT)"
