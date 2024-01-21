/** Intro

Ye Olde Computer Shoppe - YOCS #4 - install(1), v1.2

Hi gang!

  When making new programs it's usually a rather tedious business to
have to strip all files, copy them, chown them, and finally chmod them
using 4 separate commands. install(1) does it all. I copied the options
from a sun manual page, so I think it's pretty standard. (including a
unused "-c" option to make install(1) copy in stead of move. install(1)
always copies)

  Install(1) knows two major actions:
	- Making a directory:
	  "install -d pathname" Will create the given directory, and
	   any directories in the path up to it.
	- Copying files: Without the "-d" install works just like
	   the cp(1) command
  Install(1)s behaviour can be modified with the following options:
	"-m omode" - set the created directory's (or copied file's)
		mode to omode. Octal notation only.
	"-o owner" - perform a chown on the created file/directory.
		The group ownership will be taken as the given
		owner's group.
	"-g group" - perform a chgrp on the created file/directory.
		This allows setting a different group than the one
		associated with the owner.
	"-s"       - Strip the file. Only for executables.
		Note that stripping may cause links to be lost if they
		existed for the destination file.
  If install(1) is used for creating a directory, and the directory
exists already, then the total effect will be that of the subsidiary
actions (chown/chmod).

  The previous version forgot the group id given with the '-g' option
if it was followed by a '-o' (which sets owner id, and should set
group id only if no '-g' is given).

Greetings, Bert Laverman (bert@rakis.iaf.nl)

*/

/*

Hacked to fix some bugs and add message catalog support.

Michael Haardt

*/

/*

Hacked to make it more portable. Introduced NO_NLS to explicitly
switch those support off, switched to use S_ISDIR.

Joerg Wittenberger

*/

#if 0
static const char *RCSid = "$Id: install.c,v 1.1.1.1 1996/07/11 14:25:35 jw Exp $" ;
#endif

/*
 * $Log: install.c,v $
 * Revision 1.1.1.1  1996/07/11 14:25:35  jw
 * Ein brauchbares install
 *
 * Revision 1.2  1992/01/01  15:26:37  yocs
 * Fixed option handling to work correctly if a '-g' is given
 * before a '-o'. Previously is forgot the '-g' setting in favour
 * of the '-o' user's group.
 *
 * Revision 1.1  1991/04/09  22:54:27  yocs
 * Initial revision
 * */

/** Prepare defines */

#undef  _POSIX_SOURCE
#define _POSIX_SOURCE   1
#undef  _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 2

/*** NLS
   Either switch NLS support off explicitly or try to switch on if
   _XOPEN_SOURCE
*/

#ifndef NO_NLS
#ifdef _XOPEN_SOURCE
#include <nl_types.h>
#else
#define NO_NLS
#endif
#endif

#ifdef NO_NLS
typedef long nl_catd;
static nl_catd catopen(char *name, int oflag) { return 0; }
static char *catgets(nl_catd catd, int set_id, int msg_id, char *msg) { return msg; }
static void catclose(nl_catd catd) { }
#endif

/** Include */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <grp.h>
#include <stdio.h>

extern char *optarg;
extern int optind,opterr,optopt;
int getopt(int argc, char * const *argv, const char *optstring);

/** Connect names */

/*** stat
    connect or emulate stat macros
*/

#ifndef S_ISDIR
#define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif

/** Implementation */

#define	CPBUFSIZE	16384	/* Copy in blocks of CPBUSIZE bytes */
#define INSTALLFILE     "INSTALLFILE"

/*** globals */

static FILE *installfile  = (FILE*)0;
static char cpbuf [CPBUFSIZE];
static uid_t uid = (uid_t)-1;	/* Owner to be, default: same as user */
static gid_t gid = (gid_t)-1;	/* Group to be, default: same as user */
static mode_t mode = 0755;	/* File mode, default: executable/dir */
static int dostrip = 0;		/* Strip the file after copying? */
static nl_catd catd;

/*** prototypes */

int omode(const char *arg);
void Fixup(const char *name);
void Makedir(char *name);
void CopyFile(const char *from, const char *to);
void CopyToDir(int argc, char *argv[], char *dir);



/*** omode( arg ) - compute octal file mode
 *
 *   omode returns the mode value given in arg.
 *
 * Parameter:
 *	arg	- String to parse
 * Returns:
 *	-1 if arg contains invalid characters
 *	the value of arg otherwise
 * Error exits:
 *     none.
 */
int omode(const char *arg)
{ int m = 0;

#ifdef	DEBUG
  fprintf( stderr, "omode( \"%s\" );\n", arg );
#endif

  if (*arg == '\0')
    return -1;
  while ((*arg >= '0') && (*arg < '8'))
    m = (m << 3) + *arg++ - '0';
  if (*arg != '\0')
    return -1;

    return m;
}

static void myexit(int n)
{
  if (installfile!=(FILE*)0) fclose(installfile);
  exit(n);
}

static void Usage(void)
{
#ifdef	DEBUG
  fprintf( stderr, "Usage();\n" );
#endif

  fprintf( stderr, catgets(catd,1,1,"Usage: install [-cs] [-g group] [-o owner] [-m mode] file1 file2\n"));
  fprintf( stderr, catgets(catd,1,2,"       install [-cs] [-g group] [-o owner] [-m mode] file ... dir\n"));
  fprintf( stderr, catgets(catd,1,3,"       install -d [-g group] [-o owner] [-m mode] dir\n"));
  myexit( 1 );
}


/*** Fixup( name ) - Fix ownership and mode
 *
 *   Fixup tries to set the required mode bits, and changes the owner-
 * ship if requested by -o/-g.
 *
 * Parameter:
 *	name	- name of the file to fix.
 * Returns:
 *	(void)
 * Error exits:
 *     Failed calls to chmod(2) or chown(2) will cause the program to
 *   abort with return value 1.
 */
void Fixup( const char *name )
{
#ifdef	DEBUG
  fprintf( stderr, "Fixup( \"%s\" );\n", name );
#endif

  if (chmod( name, mode ))	/* Change mode bits */
  { fputs( "install: chmod: " , stderr); perror( name );
    myexit( 1 );
  }
  if ((uid != (uid_t)-1) &&		/*  and ownership */
    chown( name, uid, gid ))
  { fputs( "install: chown: ", stderr ); perror( name );
    myexit( 1 );
  }
}


/*** Makedir( name ) - make directories in given path
 *
 *   Makedir scans the given pathname, making the missing directories
 * along the way. Newly created directories, or else the one named by the
 * pathname if it already exists, are optionally chmodded and chowned, as
 * directed by the -m, -g, and -o parameters.
 *
 * Parameter:
 *	name	- Pathname to scan/create
 * Returns:
 *	(void)
 * Error exits:
 *     Failed calls to mkdir(2), chmod(2), or chown(2) will cause the
 *   program to abort with return value 1.
 */
void Makedir( name )
  char *name;
{ char *p = name,	/* pointer te end of pathname being checked */
	c,		/* character at that point, '/' or '\0' */
	isnew;		/* -1 if directory was newly created */

#ifdef	DEBUG
  fprintf( stderr, "Makedir( \"%s\" );\n", name );
#endif

  while (*p != '\0')
  { do { p++;			/* Skip to next '/' or end-of-string */
    } while ((*p != '/') && (*p != '\0'));
    c = *p;			/* Save character */
    *p = '\0';			/* Terminate pathname temporarily */
    if ((isnew = access( name, F_OK )) != 0)	/* exists? */
    { if (installfile!=(FILE*)0) { fputs(name,installfile); fputc('\n',installfile); }
      if (mkdir( name, 0755 ))	/* Create it if not */
      { fputs( "install: mkdir: ", stderr ); perror( name );
	myexit( 1 );
      }
    }
    if (isnew || (c == '\0'))
      Fixup( name );
    *p = c;			/* Restore string */
  }
}


/*** CopyToDir( argc, argv, dir ) - Copy files to a directory
 *
 *   CopyToDir copies the argc files given in argv to the directory
 * dir. The files' indiviual names will stay the same. All destination
 * files are stripped, chmodded, or chowned if requested.
 *
 * Parameters:
 *	argc	- Number of files to copy
 *	argv	- Array of file names
 *	dir	- Directory to copy to
 * Returns:
 *	(void)
 * Error exits:
 *     Failure to copy - and subsequently do things to - a file will
 *   abort the program. No attempt is made to undo the copying if failure
 *   occurs halfway.
 */
void CopyToDir(int argc, char **argv, char *dir )
{
  char dest [256], *base;

#ifdef	DEBUG
  fprintf( stderr, "CopyToDir( %d, [\"%s\", ...], \"%s\" );\n"
		 , argc, *argv, dir );
#endif

  while (argc-- > 0)
  { if ((base = strrchr( *argv, '/' )) == NULL)
      base = *argv;
    else
      base++;
    (void) strcpy( dest, dir );
    (void) strcat( dest, "/" );
    (void) strcat( dest, base );
    CopyFile( *argv, dest );
    argv++;
  }
}


/*** CopyFile( from, to ) - Copy a file (on)to another
 *
 *   CopyFile copies the file specified by from, to the one specified
 * by to. If dostrip is true, strip(1) is used on the file after
 * copying. Also optionally calls are made to chmod(2) and chown(2)
 * as specified by the -m, -g, and -o flags.
 *
 * Parameters:
 *	from	- pathname of the sourcefile
 *	to	- pathname of the destination
 * Returns:
 *	(void)
 * Error exits:
 *     Failed calls to open(2), read(2), write(2), system(3), chmod(2),
 *   or chown(2) will cause the program to abort with return value 1.
 */
void CopyFile(const char *from, const char *to )
{ 
  int fin, fout;
  size_t n;
  char cmnd [256];

#ifdef	DEBUG
  fprintf( stderr, "CopyFile( \"%s\", \"%s\" );\n", from, to );
#endif

  if (installfile!=(FILE*)0) { fputs(to,installfile); fputc('\n',installfile); }
  if ((fin = open( from, O_RDONLY )) < 0)
  { perror( from );
    myexit( 1 );
  }
  if ((fout = open( to, O_WRONLY|O_CREAT|O_TRUNC, mode )) < 0)
  { perror( to );
    myexit( 1 );
  }
  while ((n = read( fin, cpbuf, CPBUFSIZE )) != 0)
  { if (n == (size_t)(-1))
    { perror( from );
      myexit( 1 );
    }
    if (write( fout, cpbuf, n ) != n)
    { perror( to );
      myexit( 1 );
    }
  }
  if (close(fin)==-1)
  {
    perror(from);
    myexit(1);
  }
  if (close(fout)==-1)
  {
    perror(to);
    myexit(1);
  }
  if (dostrip)
  { (void) sprintf( cmnd, "strip %s", to );
    if (system( cmnd ))
    { fprintf( stderr, catgets(catd,1,4,"install: strip of %s failed.\n"), to );
      myexit( 1 );
    }
  }
  Fixup( to );
}


/*** main */
int main(int argc, char *argv[])
{
  int		 dodir = 0;
  struct passwd	*p;
  struct group	*g;
  int		 opt;
  struct stat	 s;

  setlocale(LC_ALL,"");
  catd=catopen("install",0);
#ifdef	DEBUG
  int i;
  fprintf( stderr, "main( %d", argc );
  for (i = 0; i < argc; i++) fprintf( stderr, ", \"%s\"", argv [i] );
  fprintf( stderr, " );\n" );
#endif

  while ((opt = getopt( argc, argv, "h?csdg:o:m:" )) != EOF)
  {
#ifdef	DEBUG
    if ((opt == 'g') || (opt == 'o') || (opt == 'm'))
      fprintf( stderr, "Option '%c', \"%s\"\n", opt, optarg );
    else
      fprintf( stderr, "Option '%c'\n", opt );
#endif
    switch (opt)
    { case 'c':			/* For compatibility; copy files */
	break;
      case 's':			/* strip files after copy */
	dostrip = 1;
	break;
      case 'd':			/* Install a directory */
	dodir = 1;
	break;
      case 'g':			/* Set group ownership */
	if (uid == (uid_t)-1)		/*  make sure we have a user id */
	  uid = getuid( );
	if ((g = getgrnam( optarg )) == NULL)
	{ fprintf( stderr, catgets(catd,1,5,"install: unknown group %s.\n"), optarg );
	  myexit( 1 );
	}
	gid = g->gr_gid;
	break;
      case 'o':			/* Set ownership */
	if ((p = getpwnam( optarg )) == NULL) /* find user id */
	{ fprintf( stderr, catgets(catd,1,6,"install: unknown user %s.\n"), optarg );
	  myexit( 1 );
	}
	uid = p->pw_uid;	/* copy uid & gid */
	if (gid == (gid_t)-1)		/*  copy gid only if no "-g" yet */
	  gid = p->pw_gid;
	break;
      case 'm':			/* Set file mode */
        if ((mode = omode( optarg )) == -1)
	{ fprintf( stderr, catgets(catd,1,7,"install: bad mode %s.\n"), optarg );
	  myexit( 1 );
	}
	break;
      default:
        Usage( );
	myexit( 1 );
    }
  }

  if ((uid != (uid_t)-1) && (getuid( ) != 0))
  { fprintf( stderr, catgets(catd,1,8,"install: must be root to use -o or -g.\n"));
    myexit( 1 );
  }
  if (getenv(INSTALLFILE)!=(char*)0)
  {
    if ((installfile=fopen(getenv(INSTALLFILE),"a"))==(FILE*)0)
    {
      fprintf(stderr,catgets(catd,1,9,"install: logfile %s can not be opened.\n"),getenv(INSTALLFILE));
      myexit(1);
    }
  }
  if (dodir)			/* Create a directory, */
  { if ((argc-optind) != 1) Usage( );
    Makedir( argv [optind] );
  }
  else				/* or else copy files */
  { if ((argc-optind) < 2) Usage( );
    if ((stat( argv [argc-1], &s) == 0) && S_ISDIR(s.st_mode))
      CopyToDir( argc-optind-1, &argv [optind], argv [argc-1] );
    else
    { if ((argc-optind) != 2) Usage( );
      CopyFile( argv [optind], argv [optind+1] );
    }
  }
  myexit( 0 );
  return 255;
}

/** Tail
Local Variables:
outline-regexp: "/?\\*\\*+"
End:
*/
