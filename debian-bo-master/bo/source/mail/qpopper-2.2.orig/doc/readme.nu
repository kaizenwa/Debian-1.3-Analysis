Popper mods contributed by John Norstad of Northwestern University
(j-norstad@nwu.edu).


Summary 
=======

1. Properly clean up on abnormal termination and rewrite the mail drop
file. The QC3 mods to catch SIGHUP signals and time out reads were not
enough. You also have to catch SIGPIPE signals to avoid being killed
when the client connection is aborted while the server is writing
(e.g., if you cancel downloading a large message in Eudora, a common
operation).

2. Fixed some bad log messages.

3. Added a mod from Don Lewis <gdonl@gv.ssi1.com>. Under SunOS 4.1.3,
and possibly other systems, the check for null passwords doesn't work.
QC3 checked only for a null password pointer in the struct returned by
getpwnam. You also have to check for an empty string returned by
getpwnam.

4. Added a -s command line option to generate statistics messages in
the log. One message is issued for each session:

Stats: username aaa bbb ccc ddd

where:

aaa = number of messages deleted. 
bbb = number of bytes deleted. 
ccc = number of messages left on server. 
ddd = number of bytes left on server.

5. (The big one). Added a "POP bulletin" feature. This feature gives
system administrators a way to send important announcements to all POP
users without having to do sendmail mass mailings.

The feature is enabled via the -b command line option. This option is
followed by the path of the bulletin directory.

The bulletin directory contains one file per bulletin. Each file
contains a complete single mail message with header and body in
mailbox format. The first line of each such bulletin must be a "From "
line. The easiest way for sysadmins to create such bulletins is to
mail themselves a copy of the bulletin (using the account to which
they want replies to be sent), then use their mail program to save the
message to a file in the bulletin directory in mailbox format. The
bulletin directory must be world readable.

The name of each bulletin file begins with the bulletin number, and
may optionally continue with any other characters. E.g., the file name
of bulletin number 23 might be:

        23.pophost_down_sunday

Popper creates a file named ".popbull" in the home directory of each
user. This file contains a single line recording the highest numbered
bulletin received by the user.

Bulletins are processed by popper in pop_dropcopy.c, immediately after
copying the mail drop to the temporary mail drop, but before building
the temporary mail drop index. All bulletins which this user has not
received previously are appended to the temporary mail drop file.

When bulletins are copied to the temporary mail drop file, all "To"
header lines are replaced by "To: username@myhost". Any "Status:"
header lines are deleted. Otherwise, the bulletins are copied as is.

When a new user checks for mail the first time, popper creates the
.popbull file in the user's home directory and seeds it with the
current maximum bulletin number. Thus new users do not get old
bulletins.

All errors are logged and cause the bulletins to not be copied. E.g.,
if the bulletin directory cannot be located, or the .popbull file
doesn't contain a number, or a bulletin does not begin with a "From "
line, or a bulletin name does not begin with a number, etc.

I use bulletin numbers instead of last mod date/times because I want
to make it possible for a sysadmin to, for example, fix a spelling
error in a bulletin without having to force all pop users to receive a
new copy of the bulletin.

6. Changed the default timeout from 30 to 300 seconds (5 minutes).
This value should be reasonably "safe" for even slow dialup
connections.

7. Included a mod from Steve Dorner to implement a new "XTND XLST"
command.

8. Updated the manpage.

9. Changed the version number to just plain version 2.0.


Detailed mods by source code file 
=================================

version.h:

Changed version number to 2.0.

---

popper.c:

Added a call to catch SIGPIPE signals in addition to SIGHUP signals.
Both signals are treated the same way: They set the "hangup" global to
"true". Popper gets a SIGPIPE signal if the client aborts the TCP
stream while recieving data (e.g., in the middle of downloading a
large message). Popper must rewrite the mailbox file in this case. In
QC3, popper was being killed and was not rewriting the mailbox file.

Rewrote the state loop to make it a bit more clear. Fixed an error in
the QC3 log messages. QC3 was writing "POP mailbox update failed"
messages when the POP mailbox update actually succeeded, rather than
when it failed.

---

pop_pass.c:

Added a mod from Don Lewis <gdonl@gv.ssi1.com>. Under SunOS 4.1.3, and
possibly other systems, the check for null passwords doesn't work. QC3
checked only for a null password pointer in the struct returned by
getpwnam. You also have to check for an empty string returned by
getpwnam.

---

pop_updt.c:

Added stats logging code. If the -s switch is specified on the popper
command line, the following log message is generated:

Stats: username aaa bbb ccc ddd

where:

aaa = number of messages deleted 
bbb = number of bytes deleted 
ccc = number of messages left on server 
ddd = number of bytes left on server

---

popper.h:

Add a new field named "stats" to the POP struct. This integer field is
non-zero if stats are requested.

Add a new field named "bulldir" to the POP struct. This field records
the bulletin directory path, or is NULL if the bulletin feature is not
enabled via the -b switch.

Add extern declaration for pop_xlst.

Changed default timeout from 30 seconds to 300 seconds. The default
should be a "safe" value. 30 seconds isn't long enough for dialup
connections, for example.

---

pop_init.c:

Process the new -s command line switch.

Process the new -b command line switch.

---

pop_bull.c:

New source file for the bulletin system.

---

pop_dropcopy.c:

Add call to pop_bull.

---

xtnd_xlst.c:

New source file for Steve Dorner's "xlst" command.

---

pop_get_subcommand.c:

Add table entry for Steve Dorner's "xlst" command.

---

Makefile:

Add pop_bull.c and xtnd_xlst.c to CSRCS and pop_bull.o and xtnd_xlst.o
to OBJS.

---

popper.8

Updated the manpage to describe the changes.
