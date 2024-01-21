.COVER
.TL
The LINUX User-Space NFS Server(\*F)
'FS
This is a rewrite of the original README file (which you can now find in
README.HISTORIC).
.FE
.AU "Version 2.2
.AF "
.COVEND
.\"
.\"
.\"
.\"
.\"
.H 1 "Overview
.\"
This package contains all necessary programs to make your Linux machine
act as an NFS server, being an NFS daemon (\fIrpc.nfsd\fP), a mount daemon
(\fIrpc.mountd\fP), optionally, the uid mapping daemon (\fIrpc.ugidd\fP), and the
showmount utility.  It was originally developed by Mark Shand, and
further enhanced by Donald Becker, Rick Sladkey, Orest Zborowski, Fred
van Kempen, and Olaf Kirch.
.P
Unlike other NFS daemons, the Linux \fInfsd\fP runs entirely in user space.
This makes it a
tad slower than other NFS implementations, and also makes it hard
to support a couple of UNIX file system features supported by other \fInfsd\fP
implementations, especially several concurrent server processes.
.\"
.\"
.\"
.H 1 "Building and installing unfsd
.\"
To compile and install the programs in this package, you first have to
run the BUILD script. It will ask you a couple of questions about your
preferred configuration. It tries to be helpful by informing you about
why it asking you which question, but a brief overview may be useful
nevertheless:
.DL
.LI "\fBuid/gid mapping:\fP
Occasionally, you will want to serve NFS clients whose assignment
of uids and gids to user names differs from that on the client. The
unfsd package offers you a mechanism to dynamically map the client's uid
space to that of the server, and vice versa. This is achieved by running
the \fIrpc.ugidd\fP daemon on the client machine, and instructing the server
machine to use it to map the client's credentials presented to the server
in an NFS call, and remap file owner information in all NFS replies.
.P
While this is convenient, it also presents a security problem because
\fIrpc.ugidd\fP can be abused by attackers to obtain a list of valid user
names for the client machine. This can be helped somewhat by making
ugidd check the requester's IP address against the \fIhosts.allow\fP
and \fIhosts.deny\fP files also used by the \fItcpd\fP wrapper
program.
.P
Therefore, the BUILD script asks you about whether you want to use ugidd
at all, and if you do, whether you wish to protect it using the host access
control files. Note that you still have to configure access control as
described below.
.P
If you do enable host access checking for \fIrpc.ugidd\fP, the BUILD script
will try to locate \fIlibwrap.a\fP which is needed for this. This library
is part of Wietse Venema's TCP wrapper package. BUILD looks in several
standard locations such as \fI/usr/lib\fP. If it does not find the library
(e.g. because you keep it in weird places like \fI/usr/i486-linux/lib\fP),
it will ask you for its full path name.
.P
If you disable \fIugidd\fP support, the daemon will not be compiled, and
the manpage will not be installed.
.P
.LI "\fBfile access control:\fP
For security reasons, \fImountd\fP and \fInfsd\fP make sure that vital
files such as \fI/etc/exports\fP are owned by the correct user and have
an appropriate access mode. BUILD will ask you which user and group
should own \fIexports\fP.  By default, this will be root/root.
.LI "\fBmount request logging:\fP
If you enable this option, \fIrpc.mountd\fP will log all attempts to mount a
directory via NFS from your server machine. This is very helpful in
monitoring NFS server usage, and for catching attempts at attcking your
machine via NFS.
.P
When enabled, \fImountd\fP will log all successful mount attempts to
\fIsyslog\fP's \fBdaemon\fP facility at level \fBnotice\fP. Failed mount
attempts are logged at level \fBwarning\fP.
.LE
.P
After completing these questions, BUILD will run a configure script to
detect certain system capabilities. This will take a while on your first
attempt. Repeated invocations of configure will run a lot faster because
the results of the tests are cached. If you want to start out with a fresh
build on a different release of Linux, you should make sure to get rid of
these cached values by running `\fCmake distclean\fP' first.
.P
You can then compile and install \fInfsd\fP by typing `\fCmake\fP' and/or
(as root) `\fCmake install\fP.' This will also install the manual pages.
.\"
.\"
.\"
.H 1 "Configuring \fInfsd\fP
.\"
To turn your Linux box into an NFS server, you have to start the
following programs from \fI/etc/rc.d/rc.inet2\fP (or wherever your favorite
Linux distribution starts network daemons from):
.DL
.LI *
\fIrpc.portmap\fP
.LI *
\fIrpc.mountd\fP
.LI *
\fIrpc.nfsd\fP
.LI *
\fIrpc.ugidd\fP (optional)
.LI *
\fIrpc.pcnfsd\fP (optional, not contained in this package)
.LE
.P
To make directories available to NFS clients, you have to enter
them in your \fIexports\fP file along with the hosts allowed to mount them.
The list of options and a sample file are given in the \fIexports(5)\fP
manual page (and the whole topic is covered quite extensively in the
Linux Network Administrator's Guide anyway), so I will not discuss this
here. If somebody feels like filling in the missing parts here, please
send me the diffs.
.P
.\"
.\"
.\"
.H 1 "Configuring \fIrpc.ugidd\fP access control
To protect \fIrpc.ugidd\fP from unauthorized access, you just have to add lines
to \fI/etc/hosts.allow\fP and/or \fI/etc/hosts.deny\fP detailing which
NFS servers that are allowed to access it. If your NFS server has the IP
address 193.175.30.33, you would add the following to \fIhosts.allow\fP:
.VERBON 22
# Allow ugidd access only to NFS server
ugidd: 193.175.30.33
.VERBOFF
.P
.\"
.\"
.\"
.H 1 "Common Problems (a.k.a. Dependencies)
.DL
.LI *
Root squashing is enabled by default, which means that requests from the
root user are treated as if they originated from the nobody user. If you
want root on the NFS client to be able to access files with full prvilege,
you have to add \fBno_root_squash\fP to the option list in \fI/etc/exports\fP.
.LI *
The most specific entry applies. This means if you export both \fI/usr\fP
and \fI/usr/local\fP to a client, and the client mounts \fI/usr\fP from the
server, the options for \fI/usr/local\fP will still apply when the client
accesses 
.LI *
Wildcards in client names only do not match dots. This means that the entry
\fB*.foo.com\fP only matches hosts named \fBjoe.foo.com\fP etc, but not
\fBjoe.sales.foo.com\fP. You may call this a bug (and I may replace the
current pattern matching code with wildmat if there is enough demand).
.LI *
Changes to the \fIexports\fP file do not take effect until both
\fInfsd\fP and \fImountd\fP have re-read the file. You either have to
kill both daemons and restart them, or send them a HUP signal:
.VERBON 22
# killall -HUP \fIrpc.mountd\fP \fIrpc.nfsd\fP
.VERBOFF
.LI *
NFS operation between two Linux boxes can be quite slow. There are a number
of reasons for this, only one of which is that unfsd runs in user space.
Another (and much worse) problem is that the Linux NFS \fIclient\fP code
currently does no proper caching, read-ahead and write-behind of NFS data.
This problem can be helped by increasing the RPC transfer size on the client
by adding the `\fBrsize=8192,wsize=8192\fP' mount options. This will at least
improve throughput when reading or writing large files. You are still in a
lose-lose situation when applications write data line by line or with
no output buffering at all.
.LE
.H 1 Copyright
Much of the code in this package was originally written by Mark Shand,
and is placed under the following copyright:
.P
.B
.in +3n
.ll -6n
This software may be used for any purpose provided the above
copyright notice is retained. It is supplied as is, with no
warranties expressed or implied.
.ll +6n
.in -3n
.R
.P
Other code, especially that written by Rick Sladkey and some replacement
routines included from the GNU libc, are covered by the GNU General
Public License, version 2, or (at your option) any later version.
.\"
.\"
.\"
.H 1 "Bug Reports
.\"
If you think you have encountered a bug in \fInfsd\fP or any of the other
programs in this package, please follow the instructions in the file
BUGS.
