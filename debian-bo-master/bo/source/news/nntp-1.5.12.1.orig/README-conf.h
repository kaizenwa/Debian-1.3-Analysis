     You will need to customize conf.h to get the server and programs running
on your system. 

    >>> Also, you should see README.SVR2 if you are compiling this on
    >>> a System V Release 2 machine, as there is some extra stuff you need
    >>> to do.

    >>> Also, you should see README.SVR3 if you are compiling this on
    >>> a System V Release 3 machine, as there is some extra stuff you need
    >>> to do.

    >>> Also, you should see README.SVR4 if you are compiling this on
    >>> a System V Release 4 machine, as there is some extra stuff you need
    >>> to do.

    >>> Also, you should see README.MTXNIU if you are compiling this
    >>> on a system running MTXNIU BSD + NFS. 

    >>> Also, you should see README.IRIX, if you are compiling this on the
    >>> SGI IRIS.

    >>> Also, you should see README.XENIX, if you are compiling this on
    >>> a machine running SCO XENIX with TCP.

    >>> Also, you should see README.HPUX, if you are compiling this under
    >>> HP-UX.

     FIRST, copy conf.h.dist to conf.h and alter ONLY conf.h.

     This is sort of a walk through conf.h so you can get some idea of
what parameters need to be changed.  You should probably print this
file out (or keep it in a separate window if you're on a workstation)
and edit conf.h as you read through it.  For each #define in conf.h,
the default value is listed in parenthesis after its name in this
document.  Manual entries mentioned here are in the "doc" directory of
the NNTP distribution.

     First are some compile-time type options, for compiling in
certain code.  The options should be "#undef"ed if you don't want
them, and "#defined" if you do.

ALONE		(undefined)

     Defines whether we're a stand alone version of the server, or
whether we're running under inetd.  Define this if you do NOT have inetd.
If you do have inetd, keep it undef'ed.

FASTFORK	(undefined)

     If ALONE is defined, then this option tells us not to read the
active file when we fork, but rather for the parent daemon to re-read
it every READINTVL (below) seconds.  This should make forking off children
a little bit faster.

LOAD	(defined as 5)
	You can have nntp findout the load average on a BSD-type machine
(sun or ultrix) and if the load average is higher than LOAD, the connection
will be rejected.

DYNAMIC_ART_ARRAY	(defined)
	Originally, nntpd assumes a specific maximum number of articles on
line per group. (See MAX_ARTICLES definition below.) This has proven to be
a problem at some sites, so with this defined nntp will dynamically allocate
a larger article array as needed.
	
BSD_42		(undefined)

     If you have a 4.2 BSD system (as opposed to a 4.3 BSD system),
this needs to be defined.  Really it does only two things: changes
the log level to be compatible with 4.2, and automatically defines
DBM (below).  If, somehow, you already have ndbm, then you should
kill the lines which auto-define it.

BSD_43		(undefined)

     If you have a 4.3 BSD system (as opposed to a 4.4 BSD system),
this needs to be defined.  


BSD_44		(defined)

     If you have a 4.4 BSD system (FreeBSD, BSD/OS, NetBSD, etc...),
this needs to be defined.  

CMU_MACH	(undefined)
     Define if you are running CMU's MACH. NeXT is handled as a BSD_43
machine.

USG		(undefined)

     Compiles in code to support System V; some of these appear down
below. 

SVR4		(undefined)

     Compiles in code to support System V Release 4

TLI		(undefined)
     Compiles in code to support the Transport Layer Interface of System V
Release 3 and later. 

**** The following four definitions have to do with the format of the ****
**** news history file. You must select the same format for NNTP that ****
**** you chose when you built your news software. If you don't, NNTP  ****
**** will NOT WORK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ****

DBM		(undefined)

     If you don't have the ndbm routines in your standard library (i.e.,
if you're not running 4.3 BSD (or later)), you'll have to define this; all it
does is replace the ndbm calls with the earlier, unwieldy dbm calls.

>>> If you define DBM, be sure to edit server/Makefile to have  "-ldbm"
>>> on the LIBS line, i.e.

	LIBS = -ldbm
[This does not apply if you are compiling on the SGI IRIX platform.]


NDBM		(defined)
     Define if you have the 4.3BSD ndbm routines and used them to build your
news software.

DBZ		(undefined)
     Define this is you are using the DBZ libraries. If you DO define this,
you will need to make alterations to makefile to insure that things will work.
If you built your news software using DBZ, you MUST build NNTP with DBZ. Many
people have success by use the cnews library as a link library.

USGHIST		(undefined)
     Define if you don't use dbm/ndbm for the history file, but instead
you use the USG-style history file format.  IF YOU DO NOT DEFINE ANY OF
DBZ, DBM OR NDBM ABOVE, THIS IS THE DEFAULT.

CNEWS	    (undefined)
        If you're running CNEWS instead of BNEWS define this. NNTP will not
work with CNEWS if you don't define this.

BATCHED_INPUT	(undefined)
	If this is defined, then the CNews-style batched input is used
to collect incoming articled into a file which periodically is sent to the
incoming news processor. 

MSGID	(undefined)
     Define this if you want to run a msgid daemon to keep track of recent
message ids.  This helps to avoid duplication when you have many feed sites.

LAI_TCP		(undefined)
	This should be defined if you are compiling on SCO Xenix with TCP/IP.
It may work on other systems as well.

EXCELAN		(undefined)
	This will compile in support for the EXCELAN EXOS TCP/IP routines.
It is known to work with Unisys 5000-series computer.

U_LONG		(undefined)
	Define this if your system does not know what a u_long is.

VOIDSIG		(defined)
     This should be defined if your signals return void on your system.
SunOS 4.X, Ultrix 3.X, Ultrix 4.x, Solaris, BSD 4.4 and IRIX return void.
Most others return int.

MMAP		(undefined)
	Define this if you run on a version of Unix that has the mmap() system
call. SunOS and Solbourne's OS/MP are two versions of Unix that do. NOTE: Not
all versions of MMAP are created equal. If you have a problem with NNTP and
this is defined. Please undefine it, recompile, reinstall and try again.

DIRENT		(undefined)
	Define this if you have dirent.h.

vfork		(undefined)

     If you DON'T have vfork, replace this line with:

#define	vfork	fork

If you DO have vfork, be sure that this remains undefined.

MINFREE		(4000)
	This is the minimum number of kbytes or blocks (depending on what the
system) that must be free on the news spool partition before nntp will allow
an XFER command to function.

POSTBUFFER	(1000)
	NNTP will allow posting until there is less than MINFREE-POSTBUFFER
blocks or kbytes available. This allows posting to continue while XFERs are
stopped.

MINFILES	(MINFREE/4)
	This is the minimum number of inodes that must be available on the
news spool partition before nntp will allow any function that will create 
more files. If you define this, please be careful not to make it a large 
number. I recommend something around MINFREE/4.

SETPROCTITLE	(undefined)
	This will replace the process name with information about what nntp
is doing. This is known to work on BSD-flavored Unix, but may not work on
USG Unix.

IHAVE_DEBUG	(undefined)

     Enables logging of each message-id as it is offered via the IHAVE
command.  This produces huge log files, but is useful if you suspect
a site is repeatedly offering the same article to your site after you
have rejected it.

SUBNET		(defined)
     If you are running 4.3 BSD (or later) or have support for subnets on
your local net, this will include subnet support for the access
file.  Basically, a routine goes out and looks at all your ethernet
interfaces, and figures out subnet masks from them.  It then
uses these to resolve subnets from IP addresses.

DAMAGED_NETMASK	(undefined)

     4.3 supports subnet masks of any bit-width, but user programs
are *very* hard pressed to deal with masks which are not a multiple
of 8 bits wide.  If you have a weird netmask, define DAMAGED_NETMASK.
The code which uses it is in server/subnet.c.

NETMASK		(undefined)

     The code in server/subnet.c wants to use 4BSD ioctls to determine
the subnet masks for each network interface.  However, you may be able
to support subnets without having such ioctls (HPUX is an example of
such a system; SunOS 3.3 is another).  If you will be satisfied by
having a compiled-in netmask, define NETMASK to be a hex constant
describing your netmask (e.g., 0xffffff00).  You must also define
SUBNET as well.

DECNET		(undefined)

     Compile in DECNET support into the server. This works under Ultrix (and
not VMS!).

UMASK		(undefined)
     This should be defined if you are running CNEWS and are concerned
that batch files may be created that can be altered by anyone. Defining
this as 022 should work safely for most systems, but experiment to be
sure.

FAKESYSLOG	(undefined)

     This is useful if your system doesn't support syslog, but you'd
like logging none the less.  By defining FAKESYSLOG to be the name of
a file, e.g., "/usr/lib/news/nntplog", you can have all nntp messages
logged to that file, ala syslog.  If you define FAKESYSLOG, you must
define LOG and SYSLOG, below.  The code for the fake syslog routines
are in ../server/fakesyslog.c, and are largely joe-code.

FAKEAPPEND	(undefined)
	If your host supports the BSD fdopen() function and the O_APPEND flag
to open(), you should define FAKEAPPEND with FAKESYSLOG so that
multiple copies of nntpd don't trash the log with buffered fprintf's.
NOTE: FAKEAPPEND does nothing if FAKESYSLOG is not defined.

SYSLOG		(LOG_NEWS)

     nntpd uses the syslog system to report errors, and optionally, to
log usage statistics.  If SYSLOG is defined, errors will be
reported via the syslog() library routine; if it is not defined, no errors
will be reported.

     If you just define SYSLOG, only errors will be reported.  If you
want more information, such as statistics, you should define LOG, below.
Defining LOG will cause additional information besides errors to be
logged via SYSLOG.

     If you have syslog(), define SYSLOG to be the name of the facility
under which nntpd should log things.  If you are using FAKESYSLOG
above, it really doesn't matter what facility name you choose; LOG_NEWS
is fine.

LOG		(undefined)

     When LOG is defined, we log copious amounts of information via
syslog to a special file.  One a busy system like ucbvax, this produces
about 100K of log information per day.  Look in ../server/SYSLOG to
get an idea of what will be logged.  You can use the scripts
provided in ../support to produce statistics on your NNTP server if
you run with LOG.

TIMEOUT		(2 hours)

     If a server is idle in command mode for TIMEOUT amount of time,
it will close the connection with an error message.  This prevents
old servers from clogging the system.  Timeout should be at least two
hours so people can go eat lunch and leave an rn on their terminal.

XFER_TIMEOUT	(30 minutes)

     This is like TIMEOUT, above, but takes effect when the server is
receiving news via IHAVE or POST.  If at least one line is not received
in XFER_TIMEOUT amount of time, the server aborts with an error.

LISTGROUP	(defined)
     Support the LISTGROUP addition to nntp used by many newsreaders
to get a quick list of all the available article numbers in a group.

XHDR		(defined)
     Enables the XHDR command, which is an extention of the NNTP spec.
XHDR allows client programs to see header lines (e.g., subject) from
an article or range of articles.  If you have overview files on your
system this uses them to make the most common headers fetch must faster.

XOVER		(defined)
     Defines whether we want to include the XOVER command, described
in the top-level README file of this distribution.

OVER_XREFS	(defined)
     Define this if your .overview files have Xref information in them.
If this is different from your .overview files the data that NNTP generates
on-the-fly (if needed) will not match the file data.

OVER_XREF_PREFIX	(defined)
     If OVER_XREFS is defined, put the "xref: " prefix in front of the
xref data.  This is the usual way of inserting the xref data into an
overview file.

OVER_FMT_FILE	("/usr/lib/news/overview.fmt")
     This file is supplied to the client if they use a "LIST OVERVIEW.FMT"
command.  A default version of this file is included with this distribution.

OVERVIEW_DIR	(undefined)
     Define this if your overview files are in a separate hierarchy from
your spool directory for news.  If each .overview file is in each group's
spool directory, leave this undefined.

XROVER	(defined)
    A new command to supply reference overview information.  This is a
very compact stream of information that tells a newsreader how to link
all the articles together without having to send as much information as
the entire overview file.

ROVER_DIR	(undefined)
     If your system maintains the reference overview files (.rover)
and they aren't in the spool directory for each group, define this to
point to their hierarchy.

XINDEX	(undefined)
     Optional support for tin's index files.

XINDEX_DIR	("/usr/spool/news/.index")
     The directory for tin's index files, if used.

XTHREAD	(undefined)
     Optional support for trn's thread files (maintained by mthreads).

THREAD_DIR	(undefined)
     Define this if you have put trn's thread files in a separate
hierarchy from the news spool.

LONG_THREAD_NAMES	(undefined)
     Define this if you have mthreads using long thread file names instead
of the directory hierarchy for thread files.

POSTER		("usenet")
     If your nntpd is run as root, nntpd will attempt to setuid()
and setgid() to the uid and gid of whoever POSTER is defined as.
If your nntpd isn't running as root (i.e., it might run as "usenet"),
either undefine this, or define it to be a user which exists but
is not used -- the setuid will fail in any event.

DOMAINMATCH	(defined)
     Defined to allow the use of domain specifications in the nntp access
file. Specifications for domains are of the form *.domain.name and can be
used instead of individually naming hosts or networks.

AUTH		(defined)
	Add the AUTHINFO extension. Read the file AUTHORIZATION in the root
directory of the NNTP distribution for more information.

GENAUTH		(undefined)
     Use this to define a directory where generic authenticators reside.
Since the client gets to choose what programs to be issued, this directory
MUST NOT contain anything else but authenticators.

PASSFILE	(/etc/nntp.sys)
	This file contains the password authentication information that the
client uses if AUTH is defined and in use.

ACCESS_FILE	("/usr/lib/news/nntp_access")

     Specifies the location of the remote access file. See the manual entry,
nntpd.8c, for a better explanation. A sample access file is in
../support/access_file.

ACTIVE_TIMES_FILE ("/usr/lib/news/active.times")
     This needs to be defined to point at your news-software-maintained
active.times file.  If you don't have an active.times file, you can either
run acttimes (see the support directory) or point this at an empty file.
If you do the latter, NEWGROUPS won't work, of course.

Next, we have some common files:

ACTIVE_FILE	("/usr/lib/news/active")

     Specifies the location of the "active" file.

DISTRIBUTIONS_FILE  ("/usr/lib/news/distributions")
     Specifies the location of the file that defines valid distributions for
this site. The format of the file is usually the name of the distribution
(e.g. "tx" for the state of Texas), some spaces or a tab, and a short descrip-
tion of the area that the distribution covers (e.g. "The State of Texas"). This
is used by the "LIST DISTRIBUTIONS" command.

SUBSCRIPTIONS_FILE  ("/usr/lib/news/subscriptions")
     The default list of newsgroups for your site.  The client queries this
file via LIST SUBSCRIPTIONS.

NEWSGROUPS_FILE  ("/usr/lib/news/newsgroups")
     Specifies the location of the file that contains newsgroup descriptions.
The format of the file is usually the name of the newsgroup, a tab, and a short
description of the newsgroup (usually from the checkgroups control message).
This file is used by the "LIST NEWSGROUPS" command.

HISTORY_FILE	("/usr/lib/news/history")
     Specifies the location of the "history" file. This is used with NEWNEWS
and ARTICLE/HEAD/BODY/STAT when given an message-id argument.

SPOOLDIR	("/usr/spool/news")
     This is the directory where news is stored on the server.

INEWS		("/usr/lib/news/inews")
     Specifies the location of inews, for posting.  Note that this is NOT the
same as the mini-inews in the inews directory supplied with the client NNTP
distribution, which should only be installed on client machines.  INEWS should
be the pathname of real, live, honest-to-God inews.  Your inews may be
in a different place, such as /usr/bin/inews.

RNEWS		("/usr/bin/rnews")
     Specifies the location of the rnews program which is used for dealing with
news received from other systems via the IHAVE command; it is often a link to
inews.

---- The following variables apply only if you are using C News batching. ----

NONEWSRUN	(undefined)
	Define this only if you plan to use the daemon version of relaynews.

TOOBIG		(300000, unless NONEWSRUN is defined)
	Under CNews-style batching, a file that is larger than this gets
sent to be unbatched. (Size is in bytes.)

TOOMANY		(1024, unless NONEWSRUN is defined)
	Under CNews-style batching, if the number of batched articles is
bigger than this, the batch file gets unbatched.

TOOOLD		(5 minutes)
	Under CNews-style batching, a file that is older than this gets
sent to be unbatched.

COPYSIZE	(8192)
	Under CNews-style batching, the number of bytes to copy at one time.

MAXDIGITS	(25)
	

MAXSTR		(1024)

INDIR		("/usr/spool/news/in.coming")
	Under CNews-style batching, the directory in which the batching takes
place.

BATCH_FILE	("/usr/spool/news/in.coming/nntp.XXXXXX")
	The filename template for batch files under CNews-style batching.

NEWSRUN		("/usr/lib/newsbin/input/newsrun")
	The name of the program to which batch files are fed once they are
created under CNews-style unbatching.
_________________________________________________________________________
Look carefully before modifying any of these!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-------------------------------------------------------------------------

FCNTL		(defined if SYSV is defined)

	Some systems define things like O_RDONLY, etc. in <fcntl.h>.
If FCNTL is defined, <fcntl.h> will be included.

FTRUNCATE	(defined if dgux)
	Use ftruncate() even if this is a  System V like machine.

NDIR		(defined if USG is defined)
     Uses the ndir compatability library, and includes <ndir.h>.

READINTVL	(600 seconds)

     If the server is compiled with FASTFORK and ALONE, then this number
tells how often to check if the active file has changed (and to read it in if
it has changed since the last time).  See README in the "server" directory of
the NNTP distribution.  If you are not compiled with FASTFORK and ALONE
(hint: you're not going to), don't worry about this.

MAX_ARTICLES	(4096)
	This is the maximum articles per newsgroup that nntp can handle
if DYNAMIC_ART_ARRAY is not defined.
