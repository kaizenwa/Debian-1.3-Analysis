.TL
The vtprint FAQ
.AU
Garrett D'Amore
<garrett@sciences.sdsu.edu>
.AI
San Diego State University
.DA
Version 0.9
June 19, 1994
.AB
This document attempts to address some of the most common questions
relating to the vtprint program.  However, this document is still very
much under development, so there may be significant
omissions or errors.  The author apologizes for any such errors or
omissions, and kindly requests that the reader pass on any corrections
or requests for clarification or more data to him.
.AE
.PP
.so FAQ.macros
.bp
.QH 1 "About \fBvtprint\fP"
.QH 2 "What is \fBvtprint\fP?"
.B vtprint
is a program that makes it possible to obtain seamless
printing from a remote host to a printer at home without
requiring a TCP/IP connection to the remote machine or root access
on the remote machine.  It will communicate over a serial line or
a regular network connection.  Other methods of connection may also
be possible.
.IP
.B vtprint
runs on the remote system (currently only UNIX-like systems
are supported).  Software on your local machine may vary
considerably.
.QH 2 "Who writes/supports \fBvtprint\fP?"
The author of \fBvtprint\fP is Garrett D'Amore.  He can be reached at 
<garrett@sciences.sdsu.edu>.  He is actively supporting \fBvtprint\fP
users, and is working on new development with \fBvtprint\fP.
.QH 2 "Where do I get \fBvtprint\fP?"
The original source archive for \fBvtprint\fP is available
via ftp.  The current version (as of June 19, 1994) is
2.0 and can be found at ftp.sdsu.edu in \fB/pub/vtprint/\fP as a GNU
\fBgzip\fP compressed tar archive (\fBvtprint-2.0.tar.gz\fP).
Currently, only a source distribution is being made available.
Also, users without ftp capabability should contact the author to
make author arrangements.
.QH 2 "What do I need to use \fBvtprint\fP?"
Well, to compile and install it, you need an ANSI C compiler.  (If
your site has it installed already, you don't need to worry about
this.)
.IP
To use it, you need a terminal (or terminal emulator) that supports
printer control escape sequences.  If you have a terminal or emulator
that supports escape sequences other than those already in listed in the
\fBvtprintcap\fP file, then you will also need to know what those sequences
are.  (Check your terminal's documentation.)
.IP
You also need (obviously) a printer and some sort of connection to
the remote host.
.QH 1 "Installing \fBvtprint\fP"
.QH 2 "How do I install \fBvtprint?\fP"
If you are have administrative powers on your remote host, then
please read the file \fBINSTALL\fP included in the original source
archive and follow the directions outlined there.
.QH 2 "What if I don't have root access on the remote host?"
If you don't have root access on the remote host, you can still
install \fBvtprint\fP.   Instead of using the "make install" and
"make install.man" options, use the options "make userinstall" and
"make userinstall.man".  This will place the necessary files in a
suitable location for a user to use a private copy of \fBvtprint\fP.
.QH 2 "How do I set up my account to use \fBvtprint\fP?"
If your system administrator installed a master copy of \fBvtprint\fP, then
it should be immediately available for use.  However, if you installed
a private copy, or your system administrator did not install it in a general
location, you will need to change a few environmental variables.
.IP
You should verify that \fBvtprint\fP is on your PATH and that the
man pages are locatable somewhere on your MANPATH.  If you installed a
personal copy, you will also need to set the environmental variable
VTPRINT to contain the string "-L \fI<full path to vtprintcap>\fP".
You may wish to add other options to the VTPRINT variable
to set up a default operating mode for \fBvtprint\fP.  Please read the
man page for details on what options are available.
.QH 1 "Using \fBvtprint\fP"
.QH 2 "What help is available for using \fBvtprint\fP?"
Besides the \fBFAQ\fP that you are reading now, there are a number
of resources that can help you use \fBvtprint\fP.
First, there are the provided man pages (e.g. \fBvtprint.1\fP).
Also there is a brief synopsis available by typing "vtprint -h".  
Finally, you can subscribe to the \fBvtprint\fP mailing list (where
\fBvtprint\fP topics
are discussed, and official announcements from the author are posted)
by sending mail to "vtprint-request@sdsu.edu" with a line "subscribe"
in the body of the message. 
.IP
Of course, you can still contact the author directly if you run into
difficulties that these resources can't help you solve.
.QH 2 "How do I print text files using \fBvtprint\fP?"
Type "vtprint \fI<filelist>\fP".  There are also several options
available.  See the man page for details on the options.
Alternatively, you can use \fBvtprint\fP as the receiving end of a
pipe.
.QH 2 "How do I print binary files using \fBvtprint\fP?"
\fBvtprint\fP does not support printing of binary files.
This is because \fBvtprint\fP
uses certain escape codes, and binary files may contain escape
codes which will confuse \fBvtprint\fP (and possibly your terminal emulator).
.QH 2 "What about PostScript?"
Because PostScript is just ASCII text, \fBvtprint\fP can print PostScript
to your printer. But it DOES NOT know how to perform conversion
from regular text to PostScript, which means that if your printer 
is a PostScript only printer, you should set up some sort of filter
to perform ASCII to PostScript conversion before piping it to
\fBvtprint\fP. For example, the following shell script might be used to
send PostScript to a printer from a text file:
.ID
#!/bin/sh
.br
while [ -n $1 ]; do { enscript $1 | vtprint; shift; }
.DE
.IP
Some time in the \fIfar\fP future, \fBvtprint\fP may have PostScript
translation added to it if it is requested by sufficiently many people.
.QH 1 "Using \fBvtprint\fP with other UNIX-based programs."
.QH 2 "How do I use \fBvtprint\fP with \fBelm\fP?"
In \fBelm\fP, press "O"
(for \fIOptions\fP).  Then press "P" (for
\fIPrint mail using\fP).  Now enter "vtprint \fI[options]\fP".  I've
found that the option string "-cdq" works well for me.  Finally, save
your change by pressing ">".  Now you can press "I" to return to the
main mail index.  Pressing "P" on the mail index screen will print the
currently selected message using \fBvtprint\fP.
.IP
This may not work with very old versions of \fBelm\fP.  It has worked
find for me since about version 2.3 or so -- I haven't tried it with
any older versions.
.QH 2 "How do I use \fBvtprint\fP with \fBmail\fP?"
I am not sure if this works for all mailers, but with the SunOS \fBmail\fP,
if you use the command "| \fI[message-list]\fP vtprint \fI[options]\fP", the
message will be piped to \fBvtprint\fP.  This strategy (using pipe) works
for a great many programs besides plain old \fBmail\fP.
.QH 2 "How do I use \fBvtprint\fP with \fBpine\fP?"
You do not need to use \fBvtprint\fP with \fBpine\fP.  The code for
\fBvtprint\fP is derived from code the author originally saw in
\fBpine\fP!  To use \fBpine\fP, select the "attached-to-ansi" printer
option, and print away!
.QH 2 "How do I use \fBvtprint\fP with \fBtin\fP?"
You need to edit the "~/.tin/.tinrc" file.  There is a line that looks
like:
.ID
default_printer=\fI[something]\fP
.DE
You need merely change the value of \fIsomething\fP to be "vtprint" along
with any options you want to use.  I've found that \fBtin\fP is much
more finnicky about the options used by \fBvtprint\fP, so I use
the "-cdq" options.
.IP
Incidentally, \fBvtprint\fP may not work with versions of \fBtin\fP prior
to 1.2pl2.  The problem was a bug in \fBtin\fP, and is fixed in more
recent versions.
.QH 2 "How do I set up program X to use \fBvtprint\fP?"
I have listed above some of the more commonly used programs.  If your
program isn't listed, and you think it should be, tell me!
.IP
The general solution is to check out your program's documentation to
see if the printer program it uses is configurable.  If so, set it to
"vtprint".  You may need to use special options \-\- the options 
"-cd" will solve most problems for many people.
.IP
If the printer program can't be changed, then see if you can use a 
pipe from your program.  If so, just pipe the output you wish to print
to \fBvtprint\fP.
.QH 1 Problems
.QH 2 "It just doesn't work!  Nothing is output by the printer, or the \
text appears on my screen but not on the printer."
This is probably a problem with your communications package.  In
all likelihood, your communications program may not support required
printer escape codes.  Most MS-DOS and Windows packages do support
these codes, but the author only knows of TWO Macintosh programs and
ONE Unix program that support \fBvtprint\fP.  A simple thing to do is
try "vtprint <somefile>" by itself.  If you get output, then
your terminal emulator will probably work with \fBvtprint\fP.
.IP
Read the file \fBCOMPAT\fP for details about compatibility with various
communications programs and terminals.
.IP
Also, make sure that your printer is set up correctly.  Try
printing from a local program, and make sure that the default is
printing device is set to your printer.  (E.g. PRN: must be the
same as the port your printer is on, under MS-DOS.
.QH 2 "\fBvtprint\fP prints a single line, but additional lines are starting \
at the wrong horizontal location, or aren't showing up at all! or Why \
can't I get anything past the first line or two to print correctly?"
You may not have CR/LF mapping set up correctly on your printer,
comm package, host, or \fBvtprint\fP.  You can change the settings in any
of these locations to fix it.  
.IP
Check out the "-c" and "-n" switches in the man pages.
.QH 2 "Why aren't page breaks included between documents?"
Some terminal emulators don't handle formfeeds properly.  \fBTelemate 4.12\fP
is known to eat them, and \fBMicrophone II\fP requires a setting
change to fix this problem.
.QH 2 "Why does \fBvtprint\fP fail on big documents?"
\fBvtprint\fP may choke on some large documents if you are not using proper
flow control settings.  Normally, you should use "hardware" flow control,
which is also sometimes known as CTS/RTS flow control.  The reason for
this is that the printer may fall behind the serial line, and the local
communications program needs to able to tell the host "wait for me to
catch up with you before you give me any more data -- I've not got any
room for it at the moment!")  You could use "XON/XOFF" flow control,
but this is discouraged as it may interfere with some other software
packages which use CTRL-S and CTRL-Q for other functions (such as binary
transfer protocols).
.QH 2 "Why does \fBvtprint\fP echo text (or some text) to the screen?"
Some communications programs or terminals may send a copy of the printed
file to the screen as well as the program.  This is legal in many cases,
since many terminal standards don't define whether screen echoing 
when printing should be done or not.
.FQT
