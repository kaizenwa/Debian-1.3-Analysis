/***********************************************
 *          MANDATORY CONFIGURATION            *
 ***********************************************/


/*  This is the command used to send mail. This should be a non-interactive
 *  command that reads an rfc822 format message from standard input, adds
 *  the necessary headers (Date, Message-ID, etc...) and dispatches it
 *  according to the Cc: and To: headers.
 *
 *  You'll probably want to leave this as sendmail, but:
 *
 *      MAKE SURE THE PATH IS RIGHT.
 *
 *  If you don't have sendmail, use /bin/false :-).
 */
/* #define MAIL_COMMAND "/usr/sbin/sendmail -t" */
/* #define MAIL_COMMAND "/usr/lib/sendmail -t"  */



/*  The following macros control the generation of email addresses.
 *  (In particular, the part that goes after the @.)  The algorithm
 *  is as follows:
 *
 *  If DOMAIN_NAME is defined (to a string), then if it begins with
 *  '/', it is taken as a file containing the domain name otherwise
 *  it is taken as the domain name itself.
 *
 *  If DOMAIN_NAME is not defined, then we find the nodename with
 *  uname(2).  If it is not fully qualified (i.e. it doesn't contain
 *  a '.'), we try to gethostbyname + aliases to find the FQDN.
 *
 *  Finally, unless DOMAIN_HACK is defined to 0, if the nodename is
 *  "a.b.c" we do address lookup on "b.c", and if it succeeds we
 *  assume that "b.c" is the proper domain name.  This will Do
 *  The Right Thing for most sites (and fail horribly on others).
 *
 *  At no point will a domain name not containing a '.' be accepted.
 */

/* #define DOMAIN_NAME "/your/file/here"  */
/* #define DOMAIN_NAME "your_domain_here" */

#define DOMAIN_HACK	1




/***********************************************
 *             MISC CONFIGURATION              *
 ***********************************************/



/*  This is the deafult editor.  This can be overridden with X resources,
 *  so you need not change this here.
 *
 *  %s   is the name of the file to edit, and
 *  %i   is the line where the cursor will initially be positioned.
 *
 *  Both %s and %i are optional, although the editor will not be much use
 *  if you don't specify %s...
 */
#define DEFAULT_EDIT_COMMAND "xterm -e vi +%i %s"



/*  The default nntp server.  Basically: don't set this, use $NNTPSERVER
 *  or the resource Knews.nntpServer instead.
 */
/* #define DEFAULT_NNTPSERVER "your.server" */

/*  If both the resource Knews.nntpServer and $NNTPSERVER are
 *  unset, this will be used for nntp server.  No need to set it.
 */
/* #define DEFAULT_DEFAULT_NNTPSERVER "your.server" */

/*  The path of the standard shell, no need to set this unless you
 *  have no /bin/sh.  Don't put a bogus shell here.
 */
/* #define BIN_SH "/bin/sh" */





/*
 *  Set one or both of these to 1 if you have libjpeg and/or libpng
 *  installed on your system.  Knews will then be able to display
 *  inline images.
 *
 *  If you set either of these you will probably need to set up
 *  library search paths in knews.tmpl for libjpeg and libpng.
 */

#define HAVE_JPEG	0
#define HAVE_PNG	0





/***************************************
 *                HACKS                *
 ***************************************/

/*
 *  Below are a few quick hacks for weird systems.  No need to bother
 *  with this unless you get errors during compilation.
 */


/*  If you don't have POSIX.2 regexps, set the following to 0 to use
 *  Henry Spencer's package provided in the directory regexp/.
 *
 *  To see if your system has this, check for a global include file
 *  <regex.h> that defines the type  regex_t  and the functions
 *  regcomp() and regexec().  But that's no guarantee:  To the best
 *  of my knowledge, the Solaris 2.4 regex implementation is bogus,
 *  so you will need this anyway.
 *
 *  Note that the GNU regex package provides sufficient POSIX support
 *  plus a few extensions, so you can get that and link against it
 *  if you prefer.
 */
#define HAVE_POSIX_REGEXPS	1



/*  If your C library doesn't have memmove (e.g. SunOS 4.1.x), set this to 0.
 */
#define HAVE_MEMMOVE		1


/*  If you don't have an up-to-date Xmu library (i.e. R5 or later),
 *  set this to 0.  This library is needed for editres support (editres
 *  is a nice application that allows you to set resources interactively).
 *  This may be needed on some HP/UX X11R4/5 hybrids.
 */
#define HAVE_XMU		1


/*  If your system has the Xpm library, set this to 1.  The Xpm library
 *  is needed for knews' string-to-xpm converter, which make things like
 *  this possible:
 *
 *  Knews*backgroundPixmap:	~/dir/texture.xpm
 */
#define HAVE_XPM		0


/*  If your system doesn't have the POSIX sigaction() function, defining
 *  this to 0 will make knews use the old, unreliable signal() funtion
 *  instead.  You'll need this if you get errors while compiling src/child.c.
 */
#define HAVE_SIGACTION		1


/*  If your system doesn't have the (completely non-standard) socketpair()
 *  function, you may set this to 0.  This will make knews incapable of
 *  talking to 'fake' servers, but that's no great loss.  You'll need this
 *  if you get errors while compiling src/server.c.
 */
#define HAVE_SOCKETPAIR		1
