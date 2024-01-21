/*
 * afio.c
 *
 * Manipulate archives and files.
 *
 * Copyright (c) 1985 Lachman Associates, Inc..
 *
 * This software was written by Mark Brukhartz at Lachman Associates,
 * Inc.. It may be distributed within the following restrictions:
 *	(1) It may not be sold at a profit.
 *	(2) This credit and notice must remain intact.
 * This software may be distributed with other software by a commercial
 * vendor, provided that it is included at no additional charge.
 *
 * Please report bugs to "uunet!sawmill!prslnk!buhrt"
 *
 * Options:
 *  o Define INDEX to use index() in place of strchr() (v7, BSD).
 *  o Define MEMCPY when an efficient memcpy() exists (SysV).
 *  o Define MKDIR when a mkdir() system call is present (4.2BSD, SysVr3).
 *  o Define NOVOID if your compiler doesn't like void casts.
 *  o Define SYSTIME to use <sys/time.h> rather than <time.h> (4.2BSD).
 *  o Define VOIDFIX to allow pointers to functions returning void (non-PCC).
 *  o Define CTC3B2 to support AT&T 3B2 streaming cartridge tape.
 *  o Define HAVEFCNTL if you have <fcntl.h>
 *  o Define MYTEMPNAM if you don't have tempnam()
 *  o Define UNIXPC if you are on a 3b1, 7300, etc.
 *  o Define HAVEMEMCMP if you have memcmp otherwise assumes bcmp
 *  o Define DEFFMTCMD to being how to format the media you use the most.
 *  o Define LONGZFILE if you want .Z to be tagged on the end of a 14 char
 *
 * BUGS:
 *	Still needs '286 floppy support.
 *
 * Added by Jeff Buhrt:
 *	Floppy Verify/format/restart output in the middle of a set,
 *	compress files on output, extended error messages and logging
 *
 * Added by Dave Gymer:
 *	Lotsa bugfixes, Linux support, recognition of .Z files in an archive
 *	that were compressed already (and shouldn't be uncompressed).
 *	Displays compression ratios.
 *
 * See the HISTORY file for more revision info.
 */

static char *ident = "$Header: /u/buhrt/src/afio/RCS/afio.c,v 2.3 1991/09/25 20:08:33 buhrt Exp $";

#include <stdio.h>
#include <errno.h>
#ifdef sun
/* fix SunOS errno.h not declaring what the manpage says it declares 
   bogosity. */
 extern int sys_nerr;
 extern char *sys_errlist[];
#endif
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <pwd.h>
#include <grp.h>
#include "patchlevel.h"

#ifdef linux
#include <utime.h>

/* for flushing floppy cache */
#include <linux/fd.h>
#endif

#ifndef	major
#include <sys/sysmacros.h>
#endif /* major */





#include "afio.h"

/* define 1 to enable file descriptor leak debugging code */
#define FDDEBUG 0

     /*
      * Static variables.
      */
     STATIC short Fflag;	/*
				 * floppy flag (write when buf full)
				 * set -sdisk_size as well
				 */
     STATIC short Zflag;	/* compress the files that we can */
     STATIC short verifyflag;	/* Verify (floppy) flag */
     STATIC short verifycnt;
#ifdef	CTC3B2
     STATIC short Cflag;	/* Enable 3B2 CTC streaming (kludge) */
#endif /* CTC3B2 */
     STATIC short aflag;	/* Preserve atime (while munging ctime) */
     STATIC short dflag;	/* Don't create missing directories */
     STATIC short fflag;	/* Fork before writing to archive */
     STATIC short gflag;	/* Change to input file directories */
            short hflag;	/* Follow symbolic links */
     STATIC short jflag;	/* Don't generate sparse filesystem blocks */
     STATIC short kflag;	/* Skip initial junk to find a header */
            short lflag;	/* Link rather than copying (when possible) */
     STATIC short mflag;	/* Ignore archived timestamps */
     STATIC short nflag;	/* Keep newer existing files */
     STATIC short uflag;	/* Report files with unseen links */
     STATIC short vflag;	/* Verbose */
     STATIC short xflag;	/* Retain file ownership */
     STATIC short zflag;	/* Print final statistics */
     STATIC short hidequit;	/* show the quit option? */
     STATIC short abspaths;	/* allow absolute path names? */
     STATIC uint arbsize = BLOCK;	/* Archive block size */
     STATIC short areof;	/* End of input volume reached */
     STATIC int arfd = -1;	/* Archive file descriptor */
     STATIC off_t arleft;	/* Space remaining within current volume */
     STATIC char *arname;	/* Expanded archive name */
     STATIC uint arpad;		/* Final archive block padding boundary */
     STATIC char arspec[PATHSIZE];	/* Specified archive name */
     STATIC off_t aruntil;	/* Volume size limit */
     STATIC int askfornext=0;	/* Ask for next disk on input eof? */
     STATIC uint arvolume = 1;	/* Volume number */
     STATIC uint buflen;	/* Archive buffer length */
     STATIC char *buffer;	/* Archive buffer */
     STATIC char *bufidx;	/* Archive buffer index */
     STATIC char *bufend;	/* End of data within archive buffer */
     STATIC Child *children;	/* Child processes */
     STATIC char *formatcmd = DEFFMTCMD;	/* how to format */
     STATIC ushort gid;		/* Group ID */
     STATIC Link *linkbase[256];/* Unresolved link information */
     STATIC FILE *logfile = NULL;	/* log same errors as stderr would */
     STATIC ushort mask;	/* File creation mask */
     STATIC char *myname;	/* Arg0 */
     extern char *optarg;	/* Option argument */
     extern int optind;		/* Command line index */
     STATIC int outpid;		/* Process ID of outstanding outflush() */
     STATIC char pwd[PATHSIZE];	/* Working directory (with "-g") */
     STATIC int pipepid;	/* Pipeline process ID */
     STATIC time_t timenow;	/* Current time */
     STATIC time_t timewait;	/* Time spent awaiting new media */
     STATIC off_t total;	/* Total number of bytes transferred */
     STATIC int ttyf = -1;	/* For interactive queries (yuk) */
     STATIC ushort uid;		/* User ID */
     int uncompressrun = 0;	/* is uncompress running? its pid if so */
     char uncompto[PATHSIZE];	/* name we uncompressed to */
     STATIC int anycorrupt = 0; /* if any data is corrupted */
     int printbytepos = 0;      /* print position of each file in archive */
     unsigned long bytepos;     /* position of first byte of current file */
     STATIC char *controlscript=NULL;  /* script to pipe control files to */

main (ac, av)
     int ac;
     reg char **av;
{
  reg int c;
  reg uint group = 1;
  VOIDFN (*fn) () = NULL;
  time_t timedone;
  auto char remote[PATHSIZE];

  if (myname = strrchr (*av, '/'))
    ++myname;
  else
    myname = *av;
  mask = umask (0);
  uid = getuid ();
  gid = getgid ();
  if (uid == 0)
    xflag = 1;

  /* ignore SIGPIPE to deal with gzip -d exiting prematurely */
  VOID signal (SIGPIPE, SIG_IGN);
  while (c = options (ac, av, 
         "aioprtIOVCb:c:de:fghjklmns:uvxXy:Y:zFKZL:R:qAE:G:M:w:W:T:SBD:P:Q:U")
        )
    {
      switch (c)
	{
	case 'r':
	  if (fn)
	    usage ();
	  fn = readcheck;
	  break;
	case 'i':
	  if (fn)
	    usage ();
	  fn = in;
	  break;
	case 'o':
	  if (fn)
	    usage ();
	  fn = out;
	  break;
	case 'p':
	  if (fn)
	    usage ();
	  fn = pass;
	  break;
	case 't':
	  if (fn)
	    usage ();
	  fn = toc;
	  break;
	case 'I':
	  if (fn)
	    usage ();
	  fn = copyin;
	  break;
	case 'O':
	  if (fn)
	    usage ();
	  fn = copyout;
	  break;
	case 'V':
	  VOID printf ("%s: Version %s dated %s\n",
		       myname, VERSION, DATE);
	  exit (0);
#ifdef	CTC3B2
	case 'C':
	  ++Cflag;
	  arbsize = 31 * 512;
	  group = 10;
	  aruntil = 1469 * 31 * 512;
	  break;
#endif /* CTC3B2 */
	case 'a':
	  ++aflag;
	  break;
	case 'b':
	  if ((arbsize = (uint) optsize (optarg)) == 0)
	    fatal (optarg, "Bad block size");
	  break;
	case 'c':
	  if ((group = (uint) optsize (optarg)) == 0)
	    fatal (optarg, "Bad buffer count");
	  break;
	case 'd':
	  ++dflag;
	  break;
	case 'e':
	  arpad = (uint) optsize (optarg);
	  break;
	case 'f':
	  ++fflag;
	  break;
	case 'g':
	  ++gflag;
	  break;
	case 'h':
	  ++hflag;
	  break;
	case 'j':
	  ++jflag;
	  break;
	case 'k':
	  ++kflag;
	  break;
	case 'l':
	  ++lflag;
	  break;
	case 'm':
	  ++mflag;
	  break;
	case 'n':
	  ++nflag;
	  break;
	case 's':
	  aruntil = optsize (optarg);
	  if (aruntil == 0)  askfornext = 1;
	  break;
	case 'F':
	  ++Fflag;
	  break;
	case 'Z':
	  ++Zflag;
	  break;
	case 'K':
	  ++verifyflag;
	  break;
	case 'u':
	  ++uflag;
	  break;
	case 'v':
	  ++vflag;
	  break;
	case 'x':
	  xflag = 1;
	  break;
	case 'X':
	  xflag = 0;
	  break;
	case 'y':
	  nameadd (optarg, 0);
	  break;
	case 'Y':
	  nameadd (optarg, 1);
	  break;
	case 'z':
	  ++zflag;
	  break;
	case 'L':
	  if ((logfile = fopen (optarg, "a")) == (FILE *) 0)
	    {
	      fprintf (stderr,
		       "Can't open %s to append, get help\n",
		       optarg);
	      exit (1);
	    }
	  break;
	case 'R':
	  formatcmd = optarg;
	  break;
	case 'q':
	  hidequit = TRUE;
	  break;
	case 'A':
	  abspaths = TRUE;
	  break;
        case 'E':
          if(!readcompexts(optarg))
	    {
	      fprintf (stderr,
		       "Can't read configuration file %s\n",
		       optarg);
	      exit (1);
	    }
          break;
        case 'G':
          gzipfactor=optsize(optarg);
          if((gzipfactor <1) || (gzipfactor >9))
	    {
	      fprintf (stderr,
		       "%s: Illegal gzip speed factor (Must be 1--9)\n",
		       optarg);
	      exit (1);
	    }
          break;
        case 'M':
          maxmem=optsize(optarg);
          break;
        case 'T':
          compthreshold=optsize(optarg);
          break;
        case 'w':
          if(!nameaddfile(optarg,0))
	    {
	      fprintf (stderr,
		       "Can't read configuration file %s\n",
		       optarg);
	      exit (1);
	    }
          break;
        case 'W':
          if(!nameaddfile(optarg,1))
	    {
	      fprintf (stderr,
		       "Can't read configuration file %s\n",
		       optarg);
	      exit (1);
	    }
          break;
        case 'S':
          ignoreslash=0;
          break;
        case 'B':
          printbytepos=1;
          break;
        case 'D':
          controlscript=optarg;
          break;
  	case 'Q':
	  compressargs=1;
  	  add_arg(optarg);
  	  break;
  	case 'P':
  	  compressprog=optarg;
  	  break;
  	case 'U':	/* compress All files */
  	  forceZflag=1;
   	  break;

	default:
	  usage ();
	}
    }
  if (fn == NULL || av[optind] == NULL)
    usage ();

  if(!compressprog) compressprog = PRG_COMPRESS;
  compress_arg_list[0] = compressprog;

  if (Fflag)
    {
      if ((buflen = aruntil) == 0)
	usage ();
    }
  else
    buflen = arbsize * group;
  if (aruntil && (aruntil < arbsize))
    {
      fprintf (stderr, "Media size %d is less than buffer size %d\n",
	       aruntil, arbsize);
      usage ();
    }
  if (arpad == 0)
    arpad = arbsize;
  if (fn != pass)
    {
      reg char *colon;
      reg char *equal;
      reg int isoutput = (fn == out || fn == copyout);

      arname = strcpy (arspec, av[optind++]);
      if (colon = strchr (arspec, ':'))
	{
	  *colon++ = '\0';
	  if (equal = strchr (arspec, '='))
	    *equal++ = '\0';
	  VOID sprintf (arname = remote,
			"!rsh %s '%s -%c -b %u -c %u %s'",
			arspec, equal ? equal : myname,
			isoutput ? 'O' : 'I', arbsize,
			group, colon);
	  if (equal)
	    *--equal = '=';
	  *--colon = ':';
	}
      if (gflag && *arname != '/' && *arname != '!')
	fatal (arspec, "Relative pathname");
      VOID signal (SIGINT, goodbye);
      /*
       * +BLOCK is added to make sure we don't overrun buffer on a
       * read (internal read(1) length is thus met)
       */
      if ((buffer = bufidx = bufend = malloc (buflen + BLOCK)) == NULL)
	fatal (arspec, "Cannot allocate I/O buffer");

      /*
       * open a floppy at the last moment (if output), otherwise now
       * note we set arleft prematurely so we don't have to open the
       * disk now
       */
      if (!Fflag || !isoutput)
	{
	  if (nextopen (isoutput ? O_WRONLY : O_RDONLY) < 0)
	    goodbye (1);
	}
      else
	arleft = aruntil;
    }
  timenow = time ((time_t *) NULL);
  (*fn) (av + optind);
  timedone = time ((time_t *) NULL);
  if (uflag)
    linkleft ();
  if (vflag || (fn == toc))
    fflush(stdout);
  if (zflag)
    {
      reg FILE *stream;

      stream = fn == toc || fn == copyin || arfd == STDOUT ? stderr : stdout;
      VOID fprintf (stream, "%s: ", myname);
      prsize (stream, total);
      VOID fprintf (stream, " bytes %s in %lu seconds. The backup was successfull!\n",
		    fn == pass
		    ? "transferred"
		    : fn == out || fn == copyout
		    ? "written"
		    : "read",
		    timedone - timenow - timewait);
    }
  if (logfile != (FILE *) 0)
    {
      VOID fprintf (logfile, "%s: Successfully backed up ", myname);
      prsize (logfile, total);
      VOID fprintf (logfile,
		    " bytes %s in %lu seconds (+waited %d seconds for disk swapping (%u disks)) finished at %s",
		    (fn == pass ? "transferred" : (fn == out
						   || fn == copyout ? "written" : "read")),
		    timedone - timenow - timewait,
		    timewait, arvolume, ctime (&timedone));
    }
  nextclos ();
  goodbye (anycorrupt);
  /* NOTREACHED */
}


/*
 * copyin()
 *
 * Copy directly from the archive to the standard output.
 */
STATIC VOIDFN
copyin (av)
     reg char **av;
{
  reg int got;
  reg uint have;

  if (*av)
    fatal (*av, "Extraneous argument");
  while (!areof || askfornext)
  { 
      VOID infill ();
      while (have = bufend - bufidx)
	  if ((got = writeall (STDOUT, bufidx, have)) < 0)
	      fatal ("<stdout>", syserr ());
	  else 
	  {
	      total+=have;
	      if (got > 0)
		  bufidx += got;
	      else
	        return;
	  }
  }
}

/*
 * copyout()
 *
 * Copy directly from the standard input to the archive.
 */
STATIC VOIDFN
copyout (av)
     reg char **av;
{
  reg int got;
  reg uint want;

  if (*av)
    fatal (*av, "Extraneous argument");
  for (;;)
    { 
      while ((want = bufend - bufidx) == 0)
	outflush (NOTDONE);
      if ((got = read (STDIN, bufidx, want)) < 0)
	fatal ("<stdin>", syserr ());
      else if (got == 0)
	break;
      else
      {
	  bufidx += got;
	  total += got; /* actually a bit too early for bytes written count */
      } 
    }
  outflush (DONE);
  if (fflag)
    outwait ();
}

/*
 * dirchg()
 *
 * Change to the directory containing a given file.
 */
STATIC int
dirchg (name, local)
     reg char *name;
     reg char *local;
{
  reg char *last;
  reg int len;
  auto char dir[PATHSIZE];

  if (*name != '/')
    return (warn (name, "Relative pathname"));
  for (last = name + strlen (name); last[-1] != '/'; --last)
    ;
  len = last - name;
  strncpy (dir, name, len)[len] = '\0';
  VOID strcpy (local, *last ? last : ".");
  if (strcmp (dir, pwd) == 0)
    return (0);
  if (chdir (dir) < 0)
    return (warn (name, syserr ()));
  VOID strcpy (pwd, dir);
  return (0);
}

/*
 * dirmake()
 *
 * Make a directory. Returns zero if successful, -1 otherwise.
 */
STATIC int
dirmake (name, asb)
     reg char *name;
     reg Stat *asb;
{
  if (mkdir (name, asb->sb_mode & S_IPOPN) < 0)
    return (-1);
/* First do the chown, then the chmod, because the chown may clear
   the suid/sgid bits we want to set.
*/
  if (xflag)
    VOID chown (name,
		uid == 0 ? ush (asb->sb_uid) : uid,
		ush (asb->sb_gid));
  if (asb->sb_mode & S_IPEXE)
    VOID chmod (name, asb->sb_mode & S_IPERM);
  return (0);
}

/*
 * dirneed()
 *
 * Recursively create missing directories (with the same permissions
 * as their first existing parent). Temporarily modifies the 'name'
 * argument string. Returns zero if successful, -1 otherwise.
 */
STATIC int
dirneed (name)
     char *name;
{
  reg char *cp;
  reg char *last;
  reg int ok;
  static Stat sb;

  last = NULL;
  for (cp = name; *cp;)
    if (*cp++ == '/')
      last = cp;
  if (last == NULL)
    return (STAT (".", &sb));
  *--last = '\0';
  ok = STAT (*name ? name : "/", &sb) == 0
    ? ((sb.sb_mode & S_IFMT) == S_IFDIR)
    : (!dflag && dirneed (name) == 0 && dirmake (name, &sb) == 0);
  *last = '/';
  return (ok ? 0 : -1);
}

/*
 * fatal()
 *
 * Print fatal message and exit.
 */
STATIC void
fatal (what, why)
     char *what;
     char *why;
{
  VOID warn (what, why);
  goodbye (1);
}



/*
 * writeall()
 *
 * Write all bytes in buf or return -1.  Used to fix invalud assumptions
 * about write() elsewhere.
 */
STATIC
int writeall(int fd, const char *buf, int count)
{
 int put,totalput;

 totalput=0;
 while(totalput<count)
 {
     put=write(fd,buf+totalput,count-totalput);

     if(put<0) return put;
     totalput+=put;
 }

 return count;
}




/*
 * in()
 *
 * Read an archive.
 */
STATIC VOIDFN
in (av)
     reg char **av;
{
  auto Stat sb;
  auto char name[PATHSIZE];
  int sel,res;

  if (*av)
    fatal (*av, "Extraneous argument");
  name[0] = '\0';
  while (inhead (name, &sb) == 0)
    {
      if (((sel = namecmp (name,&sb)) < 0) || inentry (name, &sb) < 0)
	if (inskip (sb.sb_size) < 0)
	  VOID warn (name, "Skipped file data is corrupt");
      if (vflag && (sel == 0))
	{
	  if(printbytepos) fprintf(stderr,"%lu ",bytepos);

	  if (*uncompto)
	    res = fprintf (stderr, "%s -- uncompressed\n", uncompto);
	  else
	    res = fprintf (stderr, "%s -- okay\n", name);

          /* check for broken pipe on stderr */
          if(res<0) {
	      if(errno == EPIPE)
		  fatal("<stderr>", syserr());
          }
	}
    }
}


/*
 * readcheck()
 *
 * Read an archive and check contents against existing files
 */

Stat atime_sb;       /* set in openincheck */
int atime_sb_valid;  /* set in openincheck */

STATIC VOIDFN
readcheck (av)
     reg char **av;
{
  auto Stat sb;
  auto char name[PATHSIZE];
  auto char local[PATHSIZE];
  int sel, res;
#ifdef linux
  auto struct utimbuf tstamp;
#else
  auto time_t tstamp[2];
#endif

  if (*av)
    fatal (*av, "Extraneous argument");
  name[0] = '\0';
  while (inhead (name, &sb) == 0)
    {
      if ((sel = namecmp (name,&sb)) < 0) {
	if (inskip (sb.sb_size) < 0)
	  VOID warn (name, "Skipped file data is corrupt");
	continue;
      }
      if (vflag) {
	strcpy(local, name);
	tocentry (local, &sb);
      }

      atime_sb_valid=0;
      if ((res = incheckentry(name, &sb)) < 0)
	inskip (sb.sb_size);
      anycorrupt |= (res < 0);

      if(aflag && atime_sb_valid && ((sb.sb_mode & S_IFMT)==S_IFREG))
      {
	  /* reset access time, this distroys the ctime btw. */
#ifdef linux
	  tstamp.actime = atime_sb.sb_atime;
	  tstamp.modtime = atime_sb.sb_mtime;
	  VOID utime (name, &tstamp);
#else
	  tstamp[0] = atime_sb.sb_atime;
	  tstamp[1] = atime_sb
.sb_mtime;
	  VOID utime (name, tstamp);
#endif
	}


    }
}

/*
 * inalloc()
 *
 * Allocate input buffer space (which was previously indexed
 * by inavail()).
 */
STATIC void
inalloc (len)
     reg uint len;
{
  bufidx += len;
  total += len;
}

/*
 * inascii()
 *
 * Read an ASCII header. Returns zero if successful;
 * -1 otherwise. Assumes that the entire magic number
 * has been read.
 */

STATIC int
inascii (magic, name, asb)
     reg char *magic;
     reg char *name;
     reg Stat *asb;
{
  auto uint namelen;
  auto char header[H_STRLEN + 1];
  PStat pasb;

  if (strncmp (magic, M_ASCII, M_STRLEN) != 0)
    return (-1);
  if (inread (header, H_STRLEN) < 0)
    return (warnarch ("Corrupt ASCII header", (off_t) H_STRLEN));
  header[H_STRLEN] = '\0';
#if 0
  if (sscanf (header, H_SCAN, &asb->sb_dev,
	      &asb->sb_ino, &asb->sb_mode, &asb->sb_uid,
	      &asb->sb_gid, &asb->sb_nlink, &asb->sb_rdev,
	      &asb->sb_mtime, &namelen, &asb->sb_size) != H_COUNT)
    return (warnarch ("Bad ASCII header", (off_t) H_STRLEN));
#else
  /* this should be much more portable than the one above */
  if (sscanf (header, PH_SCAN, &pasb.st_dev,
	      &pasb.st_ino, &pasb.st_mode, &pasb.st_uid,
	      &pasb.st_gid, &pasb.st_nlink, &pasb.st_rdev,
	      &pasb.st_mtime, &namelen, &pasb.st_size) != H_COUNT)
    return (warnarch ("Bad ASCII header", (off_t) H_STRLEN));
  /* now, we let the compiler cast the info to the right types (field sizes) */
  asb->sb_dev = pasb.st_dev;
  asb->sb_ino = pasb.st_ino;
  asb->sb_mode = pasb.st_mode;
  asb->sb_uid = pasb.st_uid;
  asb->sb_gid = pasb.st_gid;
  asb->sb_nlink = pasb.st_nlink;
  asb->sb_rdev = pasb.st_rdev;
  asb->sb_mtime = pasb.st_mtime;
  asb->sb_size = pasb.st_size;
#endif
  if (namelen == 0 || namelen >= PATHSIZE)
    return (warnarch ("Bad ASCII pathname length", (off_t) H_STRLEN));
  if (inread (name, namelen) < 0)
    return (warnarch ("Corrupt ASCII pathname", (off_t) namelen));
  if (name[namelen - 1] != '\0')
    return (warnarch ("Bad ASCII pathname", (off_t) namelen));
  return (0);
}

/*
 * inavail()
 *
 * Index availible input data within the buffer. Stores a pointer
 * to the data and its length in given locations. Returns zero with
 * valid data, -1 if unreadable portions were replaced with nulls.
 */
STATIC int
inavail (bufp, lenp)
     reg char **bufp;
     uint *lenp;
{
  reg uint have;
  reg int corrupt = 0;

  while ((have = bufend - bufidx) == 0)
    corrupt |= infill ();
  *bufp = bufidx;
  *lenp = have;
  return (corrupt);
}

/*
 * inbinary()
 *
 * Read a binary header. Returns the number of trailing alignment
 * bytes to skip; -1 if unsuccessful.
 */
STATIC int
inbinary (magic, name, asb)
     reg char *magic;
     reg char *name;
     reg Stat *asb;
{
  reg uint namefull;
  auto Binary binary;

  if ((int) *((ushort *) magic) != M_BINARY)
    return (-1);
  memcpy ((char *) &binary,
	  magic + sizeof (ushort),
	  M_STRLEN - sizeof (ushort));
  if (inread ((char *) &binary + M_STRLEN - sizeof (ushort),
	      sizeof (binary) - (M_STRLEN - sizeof (ushort))) < 0)
    return (warnarch ("Corrupt binary header",
		   (off_t) sizeof (binary) - (M_STRLEN - sizeof (ushort))));
  asb->sb_dev = binary.b_dev;
  asb->sb_ino = binary.b_ino;
  asb->sb_mode = binary.b_mode;
  asb->sb_uid = binary.b_uid;
  asb->sb_gid = binary.b_gid;
  asb->sb_nlink = binary.b_nlink;
  asb->sb_rdev = binary.b_rdev;
  asb->sb_mtime = binary.b_mtime[0] << 16 | binary.b_mtime[1];
  asb->sb_size = binary.b_size[0] << 16 | binary.b_size[1];
  if (binary.b_name == 0 || binary.b_name >= PATHSIZE)
    return (warnarch ("Bad binary pathname length",
		   (off_t) sizeof (binary) - (M_STRLEN - sizeof (ushort))));
  if (inread (name, namefull = binary.b_name + binary.b_name % 2) < 0)
    return (warnarch ("Corrupt binary pathname", (off_t) namefull));
  if (name[binary.b_name - 1] != '\0')
    return (warnarch ("Bad binary pathname", (off_t) namefull));
  return (asb->sb_size % 2);
}

/*
 * indata()
 *
 * Install data from an archive. Returns given file descriptor.
 */
STATIC int
indata (fd, size, name)
     int fd;
     reg off_t size;
     char *name;
{
  reg uint chunk;
  reg char *oops;
  reg int sparse;
  reg int corrupt;
  auto char *buf;
  auto uint avail;

  corrupt = sparse = 0;
  oops = NULL;
  while (size)
    {
      corrupt |= inavail (&buf, &avail);
      size -= (chunk = size < avail ? (uint) size : avail);
      if (oops == NULL && (sparse = fswrite (fd, buf, chunk)) < 0)
	oops = syserr ();
      inalloc (chunk);
    }
  if (corrupt)
    VOID warn (name, "Corrupt archive data");
  if (oops)
    VOID warn (name, oops);
  else if (sparse > 0
	   && (lseek (fd, (off_t) - 1, 1) < 0
	       || write (fd, "", 1) != 1))
    VOID warn (name, syserr ());
  return (fd);
}

/*
 * incheckdata()
 *
 * Check data from an archive. Returns given file descriptor.
 */
STATIC int
incheckdata (fd, size, name, asb, comp)
     int fd;
     reg off_t size;
     char *name;
     reg Stat *asb;
     int comp;
{
  reg uint chunk;
  reg char *oops;
  reg int sparse;
  reg int corrupt, warned = 0;
  auto char *buf;
  auto uint avail;
  auto int pfd[2], pfdc[2];
  auto int pid, comppid;

  corrupt = sparse = 0;
  oops = NULL;
  if (comp) {
    if (pipe(pfd) < 0) {
      perror("pipe"); exit(1);
    }
    switch ((pid = xfork("incheckdata, decompressing", NODIE))) {
    case -1:
      perror("fork");
      exit(1);
    case 0: /* child */
      VOID close (pfd[1]);
      if (pipe(pfdc) < 0) {
	perror("pipe"); exit(1);
      }
      switch ((comppid = xfork ("incheckdata, decomp", NODIE))) {
      case -1:
	perror("fork");
	exit(1);
      case 0: /* child */
	if (arfd != STDIN && arfd != STDOUT) VOID close (arfd);
	VOID close (pfdc[0]);
	VOID close (fileno (stdin));
	VOID dup (pfd[0]);
	VOID close (pfd[0]);
	VOID close (fileno (stdout));
	if (dup (pfdc[1]) < 0)
	  exit(1);
	close(pfdc[1]);
	if(compressargs)
	    execvp (compressprog, compress_arg_list);
	else
	    execlp (compressprog, compressprog, "-d", "-c", 0);
	fprintf (stderr, "Could not uncompress, errno %d\n", errno);
	exit(1);
	break;
      default:
	{
	  char buff1[512];
	  char buff2[512];
	  off_t n, n1, n2;

	  close(pfdc[1]);
	  n1 = n2 = 1;
	  while ((n1 > 0) && (n2 > 0)) {
	    n = sizeof(buff1);
	    n1 = read(fd, buff1, n);
	    n2 = read(pfdc[0], buff2, n);
	    size -= n1;
	    if (n1 < 0 || n2 < 0 || n1 != n2) {
	      VOID warn (name, "Archive data and file cannot be aligned");
	      corrupt = 1;
	      break;
	    }
	    else {
	      if (memcmp(buff1, buff2, n1) != 0) {
		if (!warned++)
		  VOID warn (name, "Difference in archive data and file");
		corrupt = 1;
	      }
	    }
	  }
	  if (corrupt)
	    kill(comppid, SIGTERM);
	  close(pfdc[0]);
	  close(fd);
	  if (xwait (comppid, "incheckentry xwait() gunzip", FALSE) != 0) {
	    warn(name, "uncompressing failure");
	    corrupt = 1;
	  }
	  exit(corrupt ? 2 : 0);
	}
      }
    default:
      close(pfd[0]);
      uncompressrun = pid;
      while (asb->sb_size) {
	corrupt |= inavail (&buf, &avail);
	if (corrupt) {
	  break;
	}
	asb->sb_size -= (chunk = asb->sb_size < avail ? (uint) asb->sb_size : avail);
	if ((sparse = write (pfd[1], buf, chunk)) < 0)
	  oops = syserr();
	inalloc(chunk);
      }
      close(pfd[1]);
      corrupt |= (xwait (pid, "incheckentry xwait()", FALSE) != 0);
      break;
    }
  }
  else {
    char buff1[512];
    off_t n, n1, n2;
    while (asb->sb_size && !corrupt) {
      if (!(corrupt |= inavail (&buf, &avail))) {
	n2 = asb->sb_size < avail ? (uint) asb->sb_size : avail;
	n = (n2 < sizeof(buff1)) ? n2 : sizeof(buff1);
	n1 = read(fd, buff1, n);
	asb->sb_size -= n;
	if (n1 < 0 || n2 < 0 || n1 != n)
	  corrupt = 1;
	else
	  corrupt |= memcmp(buff1, buf, n) != 0;
	inalloc(n);
      }
    }
  }
  close(fd);
  if (corrupt) {
    VOID warn (name, "Corrupt archive data");
    return -1;
  }
  return 0;
}

/*
 * inentry()
 *
 * Install a single archive entry. Returns zero if successful, -1 otherwise.
 */
STATIC int
inentry (name, asb)
     char *name;
     reg Stat *asb;
{
  reg Link *linkp;
  reg int ifd;
  reg int ofd;
#ifdef linux
  auto struct utimbuf tstamp;
#else
  auto time_t tstamp[2];
#endif

  if ((ofd = openotty (name, asb, linkp = linkfrom (asb), 0, Zflag)) > 0)
    {
      if (asb->sb_size || linkp == NULL || linkp->l_size == 0)
	VOID close (indata (ofd, asb->sb_size, name));
      else if ((ifd = open (linkp->l_path->p_name, O_RDONLY)) < 0)
	VOID warn (linkp->l_path->p_name, syserr ());
      else
	{
	  passdata (linkp->l_path->p_name, ifd, name, ofd);
	  VOID close (ifd);
	  VOID close (ofd);
	}
      /* safety */
      if (uncompressrun)
	{
	  VOID xwait (uncompressrun, "inentry xwait()", TRUE);
	  uncompressrun = 0;
	}
    }
  else if (ofd < 0)
    return (-1);
  else if (inskip (asb->sb_size) < 0)
    VOID warn (name, "Redundant file data is corrupt");

  /* Cannot set utime on symlink (at least not under Linux) */
  if((asb->sb_mode & S_IFMT) != S_IFLNK)
  {
#ifdef linux
      tstamp.actime = tstamp.modtime = mflag ? timenow : asb->sb_mtime;
      VOID utime (name, &tstamp);
#else
      tstamp[0] = tstamp[1] = mflag ? timenow : asb->sb_mtime;
      VOID utime (name, tstamp);
#endif
  }

  return (0);
}

/*
 * incheckentry()
 *
 * Check a single archive entry. Returns zero if successful, -1 otherwise.
 */
STATIC int
incheckentry (name, asb)
     char *name;
     reg Stat *asb;
{
  reg Link *linkp;
  reg int ifd;
  auto int compression;
#ifdef linux
  auto struct utimbuf tstamp;
#else
  auto time_t tstamp[2];
#endif

  if (ISCONTROL(asb))
  {
      /* process control file */
      if(inentry (name, asb) < 0)
	  if (inskip (asb->sb_size) < 0)
	      VOID warn (name, "Skipped file data is corrupt");

      /* do not verify the control file, return success */
      return(0);
  }
  
  uncompressrun = 0;
  if ((ifd = openincheck (name, asb, &compression, Zflag,
			  linkp = linkfrom (asb))) > 0)
    {
      if (asb->sb_size || linkp == NULL || linkp->l_size == 0)
	return incheckdata(ifd, asb->sb_size, name, asb, compression);
    }
  else if (ifd < 0)
    return (-1);
  else if (inskip (asb->sb_size) < 0)
    VOID warn (name, "Redundant file data is corrupt");
  if (ifd > 0)
    close(ifd);
  return (0);
}

/*
 * infill()
 *
 * Fill the archive buffer. Remembers mid-buffer read failures and
 * reports them the next time through. Replaces unreadable data with
 * null characters. Returns zero with valid data, -1 otherwise.
 */
STATIC int
infill ()
{
  reg int got;
  static int failed;
  off_t readsize;

  bufend = bufidx = buffer;
  if (!failed)
    {
      if (areof)
	if (total == 0)
	  fatal (arspec, "No input");
	else
	{
	    if((aruntil!=0) || askfornext)
		next (O_RDONLY, "Input EOF");
	    else
		fatal (arspec, "Premature input EOF");	    
	}
#if 0
    fprintf(stderr,"aruntil=%ld arleft=%ld arbsize=%ld\n",aruntil,arleft,arbsize);
#endif
      if (aruntil && (arleft == 0))
	next (O_RDONLY, "Input limit reached");
      
      if(aruntil) readsize=arleft; else readsize=buffer + buflen - bufend;
      if(readsize>arbsize) readsize=arbsize;

      while (!failed
	     && !areof
             && (aruntil == 0 || arleft >= readsize)
	     && buffer + buflen - bufend >= readsize)
	{
	  if ((got = read (arfd, bufend, readsize)) > 0)
	    {
	      bufend += got;
	      arleft -= got;
	    }
	  else if (got < 0)
	    failed = warnarch (syserr (),
			       (off_t) 0 - (bufend - bufidx));
	  else
	    ++areof;
	}
    }
  if (failed && bufend == buffer)
    {
      failed = 0;
      for (got = 0; got < arbsize; ++got)
	*bufend++ = '\0';
      return (-1);
    }
  return (0);
}

/*
 * inhead()
 *
 * Read a header. Quietly translates old-fashioned binary cpio headers
 * (and arranges to skip the possible alignment byte). Returns zero if
 * successful, -1 upon archive trailer.
 */
STATIC int
inhead (name, asb)
     reg char *name;
     reg Stat *asb;
{
  reg off_t skipped;
  auto char magic[M_STRLEN];
  static int align;

#if FDDEBUG
/* debug code added by KH.  Are we leaking file descriptors? */
	  { int i; i=dup(0); close(i); fprintf(stderr,"%d",i); }
#endif

  if (align > 0)
    VOID inskip ((off_t) align);
  align = 0;

  bytepos=total; /* position of current file */

  for (;;)
    {
      VOID inread (magic, M_STRLEN);
      skipped = 0;
      while ((align = inascii (magic, name, asb)) < 0
	     && (align = inbinary (magic, name, asb)) < 0
	     && (align = inswab (magic, name, asb)) < 0)
	{
	  if (++skipped == 1)
	    {
	      if (!kflag && total - sizeof (magic) == 0)
		fatal (arspec, "Unrecognizable archive");
	      VOID warnarch ("Bad magic number",
			       (off_t) sizeof (magic));
	      if (name[0])
		VOID warn (name, "May be corrupt");
	    }
	  memcpy (magic, magic + 1, sizeof (magic) - 1);
	  VOID inread (magic + sizeof (magic) - 1, 1);
	}
      if (skipped)
	{
	  VOID warnarch ("Apparently resynchronized",
			   (off_t) sizeof (magic));
	  VOID warn (name, "Continuing");
	}
      if (strcmp (name, TRAILER) == 0)
	return (-1);
      if (nameopt (name) >= 0)
	break;
      VOID inskip (asb->sb_size + align);
    }
#ifdef	S_IFLNK
  if ((asb->sb_mode & S_IFMT) == S_IFLNK)
    {
      if (inread (asb->sb_link, (uint) asb->sb_size) < 0)
	{
	  VOID warn (name, "Corrupt symbolic link");
	  return (inhead (name, asb));
	}
      asb->sb_link[asb->sb_size] = '\0';
      asb->sb_size = 0;
    }
#endif /* S_IFLNK */
  if ((name[0] == '/') && !abspaths)
    if (name[1])
      while (name[0] = name[1])
	++name;
    else
      name[0] = '.';
  asb->sb_atime = asb->sb_ctime = asb->sb_mtime;
  return (0);
}

/*
 * inread()
 *
 * Read a given number of characters from the input archive. Returns
 * zero with valid data, -1 if unreadable portions were replaced by
 * null characters.
 */
STATIC int
inread (dst, len)
     reg char *dst;
     uint len;
{
  reg uint have;
  reg uint want;
  reg int corrupt = 0;
  char *endx = dst + len;

  while (want = endx - dst)
    {
      while ((have = bufend - bufidx) == 0)
	corrupt |= infill ();
      if (have > want)
	have = want;
      memcpy (dst, bufidx, have);
      bufidx += have;
      dst += have;
      total += have;
    }
  return (corrupt);
}

/*
 * inskip()
 *
 * Skip input archive data. Returns zero under normal circumstances,
 * -1 if unreadable data is encountered.
 */
STATIC int
inskip (len)
     reg off_t len;
{
  reg uint chunk;
  reg int corrupt = 0;

  while (len)
    {
      while ((chunk = bufend - bufidx) == 0)
	corrupt |= infill ();
      if (chunk > len)
	chunk = len;
      bufidx += chunk;
      len -= chunk;
      total += chunk;
    }
  return (corrupt);
}

/*
 * inswab()
 *
 * Read a reversed byte order binary header. Returns the number
 * of trailing alignment bytes to skip; -1 if unsuccessful.
 */
STATIC int
inswab (magic, name, asb)
     reg char *magic;
     reg char *name;
     reg Stat *asb;
{
  reg ushort namesize;
  reg uint namefull;
  auto Binary binary;

  if ((int) *((ushort *) magic) != swab (M_BINARY))
    return (-1);
  memcpy ((char *) &binary,
	  magic + sizeof (ushort),
	  M_STRLEN - sizeof (ushort));
  if (inread ((char *) &binary + M_STRLEN - sizeof (ushort),
	      sizeof (binary) - (M_STRLEN - sizeof (ushort))) < 0)
    return (warnarch ("Corrupt swapped header",
		   (off_t) sizeof (binary) - (M_STRLEN - sizeof (ushort))));
  asb->sb_dev = (dev_t) swab (binary.b_dev);
  asb->sb_ino = (ino_t) swab (binary.b_ino);
  asb->sb_mode = swab (binary.b_mode);
  asb->sb_uid = swab (binary.b_uid);
  asb->sb_gid = swab (binary.b_gid);
  asb->sb_nlink = swab (binary.b_nlink);
  asb->sb_rdev = (dev_t) swab (binary.b_rdev);
  asb->sb_mtime = swab (binary.b_mtime[0]) << 16 | swab (binary.b_mtime[1]);
  asb->sb_size = swab (binary.b_size[0]) << 16 | swab (binary.b_size[1]);
  if ((namesize = swab (binary.b_name)) == 0 || namesize >= PATHSIZE)
    return (warnarch ("Bad swapped pathname length",
		   (off_t) sizeof (binary) - (M_STRLEN - sizeof (ushort))));
  if (inread (name, namefull = namesize + namesize % 2) < 0)
    return (warnarch ("Corrupt swapped pathname", (off_t) namefull));
  if (name[namesize - 1] != '\0')
    return (warnarch ("Bad swapped pathname", (off_t) namefull));
  return (asb->sb_size % 2);
}

/*
 * lineget()
 *
 * Get a line from a given stream. Returns 0 if successful, -1 at EOF.
 */
STATIC int
lineget (stream, buf)
     reg FILE *stream;
     reg char *buf;
{
  reg int c;

  for (;;)
    {
      if ((c = getc (stream)) == EOF)
	return (-1);
      if (c == '\n')
	break;
      *buf++ = c;
    }
  *buf = '\0';
  return (0);
}

/*
 * linkalso()
 *
 * Add a destination pathname to an existing chain. Assumes that
 * at least one element is present.
 */
STATIC void
linkalso (linkp, name)
     reg Link *linkp;
     char *name;
{
  reg Path *path;

  if (((path = (Path *) memget (sizeof (Path))) == NULL)
      || ((path->p_name = memstr (name)) == NULL))
    return;
  path->p_forw = NULL;
  path->p_back = linkp->l_path->p_back;
  path->p_back->p_forw = path;
  linkp->l_path->p_back = path;
}

/*
 * linkfrom()
 *
 * Find a file to link from. Returns a pointer to a link
 * structure, or NULL if unsuccessful.
 */
STATIC Link *
linkfrom (asb)
     reg Stat *asb;
{
  reg Link *linkp;
  reg Link *linknext;
  reg Path *path;
  reg Path *pathnext;
  reg Link **abase;

  for (linkp = *(abase = linkhash (asb->sb_ino)); linkp; linkp = linknext)
    if (linkp->l_nlink == 0)
      {
	for (path = linkp->l_path; path; path = pathnext)
	  {
	    pathnext = path->p_forw;
	    free (path->p_name);
	  }
	free ((char *) linkp->l_path);
	if (linknext = linkp->l_forw)
	  linknext->l_back = linkp->l_back;
	if (linkp->l_back)
	  linkp->l_back->l_forw = linkp->l_forw;
	else
	  *abase = linkp->l_forw;
	free ((char *) linkp);
      }
    else if (linkp->l_ino == asb->sb_ino
	     && linkp->l_dev == asb->sb_dev)
      {
	--linkp->l_nlink;
	return (linkp);
      }
    else
      linknext = linkp->l_forw;
  return (NULL);
}


/*
 * linkleft()
 *
 * Complain about files with unseen links.
 */
STATIC void
linkleft ()
{
  reg Link *lp;
  reg Link **base;

  for (base = linkbase; base < linkbase + nel (linkbase); ++base)
    for (lp = *base; lp; lp = lp->l_forw)
      if (lp->l_nlink)
	VOID warn (lp->l_path->p_name, "Unseen link(s)");
}

/*
 * linkto()
 *
 * Remember a file with outstanding links. Returns a
 * pointer to the associated link structure, or NULL
 * when linking is not possible.
 */
STATIC Link *
linkto (name, asb)
     char *name;
     reg Stat *asb;
{
  reg Link *linkp;
  reg Path *path;
  reg Link **abase;

  if (((asb->sb_mode & S_IFMT) == S_IFDIR)
      || ((linkp = (Link *) memget (sizeof (Link))) == NULL)
      || ((path = (Path *) memget (sizeof (Path))) == NULL)
      || ((path->p_name = memstr (name)) == NULL))
    return (NULL);
  linkp->l_dev = asb->sb_dev;
  linkp->l_ino = asb->sb_ino;
  linkp->l_nlink = asb->sb_nlink - 1;
  linkp->l_size = asb->sb_size;
  linkp->l_path = path;
  path->p_forw = NULL;
  path->p_back = path;
  if (linkp->l_forw = *(abase = linkhash (asb->sb_ino)))
    linkp->l_forw->l_back = linkp;
  linkp->l_back = NULL;
  return (*abase = linkp);
}

#ifndef	MEMCPY

/*
 * memcpy()
 *
 * A simple block move.
 */
STATIC char *
memcpy (to, from, len)
     reg char *to;
     reg char *from;
     uint len;
{
  reg char *toend;

  for (toend = to + len; to < toend; *to++ = *from++)
    ;
  return (to);
}

#endif /* MEMCPY */

/*
 * memget()
 *
 * Allocate memory.
 */
STATIC char *
memget (len)
     uint len;
{
  reg char *mem;
  static short outofmem;

  if ((mem = malloc (len)) == NULL && !outofmem)
    outofmem = warn ("memget()", "Out of memory");
  return (mem);
}

/*
 * memstr()
 *
 * Duplicate a string into dynamic memory.
 */
STATIC char *
memstr (str)
     reg char *str;
{
  reg char *mem;

  if (mem = memget ((uint) strlen (str) + 1))
    VOID strcpy (mem, str);
  return (mem);
}

#ifndef	MKDIR

/*
 * mkdir()
 *
 * Make a directory via "/bin/mkdir". Sets errno to a
 * questionably sane value upon failure.
 */
STATIC int
mkdir (name, mode)
     reg char *name;
     reg ushort mode;
{
  reg int pid;

  if ((pid = xfork ("mkdir()", DIE)) == 0)
    {
      VOID close (fileno (stdin));
      VOID close (fileno (stdout));
      VOID close (fileno (stderr));
      VOID open ("/dev/null", O_RDWR);
      VOID dup (fileno (stdin));
      VOID dup (fileno (stdin));
      VOID umask (~mode);
      VOID execl ("/bin/mkdir", "mkdir", name, (char *) NULL);
      exit (1);
    }
  if (xwait (pid, "mkdir()", TRUE) == 0)
    return (0);
  errno = EACCES;
  return (-1);
}

#endif /* MKDIR */


/*
 * nameopt()
 *
 * Optimize a pathname. Confused by "<symlink>/.." twistiness.
 * Returns the number of final pathname elements (zero for "/"
 * or ".") or -1 if unsuccessful.
 */
STATIC int
nameopt (begin)
     char *begin;
{
  reg char *name;
  reg char *item;
  reg int idx;
  int absolute;
  auto char *element[PATHELEM];

  absolute = (*(name = begin) == '/');
  idx = 0;
  for (;;)
    {
      if (idx == PATHELEM)
	return (warn (begin, "Too many elements"));
      while (*name == '/')
	++name;
      if (*name == '\0')
	break;
      element[idx] = item = name;
      while (*name && *name != '/')
	++name;
      if (*name)
	*name++ = '\0';
      if (strcmp (item, "..") == 0)
	if (idx == 0)
	  if (absolute)
	    ;
	  else
	    ++idx;
	else if (strcmp (element[idx - 1], "..") == 0)
	  ++idx;
	else
	  --idx;
      else if (strcmp (item, ".") != 0)
	++idx;
    }
  if (idx == 0)
    element[idx++] = absolute ? "" : ".";
  element[idx] = NULL;
  name = begin;
  if (absolute)
    *name++ = '/';
  for (idx = 0; item = element[idx]; ++idx, *name++ = '/')
    while (*item)
      *name++ = *item++;
  *--name = '\0';
  return (idx);
}

/*
 * next()
 *
 * Advance to the next archive volume.
 */
STATIC void
next (mode, why)
     reg int mode;
     reg char *why;
{
  reg time_t began;
  auto char msg[200];
  auto char answer[20];

  began = time ((time_t *) NULL);
  nextclos ();
  VOID warnarch (why, (off_t) 0);
  if (arfd == STDIN || arfd == STDOUT)
    goodbye (1);
  ++arvolume;			/* change disk # here */
  if(Fflag)
  {
  VOID sprintf (msg, "\
%s: Ready for disk %u on %s\n\
%s: \"quit\" to abort,\"f\" to format, anything else to proceed. > \07",
 		myname,
 		arvolume,
 		arspec,
 		myname);
  }
  else
  {
  VOID sprintf (msg, "\
%s: Ready for volume %u on %s\n\
%s: \"quit\" to abort, anything else to proceed. > \07",
 		myname,
 		arvolume,
 		arspec,
 		myname);
}

  for (;;)
    {
      nextask (msg, answer, sizeof (answer));
      if (strcmp (answer, "quit") == 0)
	fatal (arspec, "Aborted");
      else
	{
 	    while (Fflag && strcmp(answer,"f") == 0) {
 	      fprintf (stderr, "formating using %s ...\n",formatcmd);
 	      if (system (formatcmd) != 0)
 	      {
 		  strcpy(msg,"\n");
 		  strcat(msg,myname);
 		  strcat(msg,": Format failed! Try again (y/n)? > ");
                   nextask (msg, answer, sizeof (answer));
 		  if (answer[0] == 'y') answer[0] = 'f';
 		  else exit(1);
 	      }
 	      else break;
 	    }
 
	    if (nextopen (mode) == 0)
		break;
	}
    }
  VOID warnarch ("Continuing", (off_t) 0);
  timewait += time ((time_t *) NULL) - began;
}

/*
 * nextask()
 *
 * Ask a question and get a response. Ignores spaces and tabs.
 */
STATIC void
nextask (msg, answer, limit)
     reg char *msg;
     reg char *answer;
     reg int limit;
{
  reg int idx;
  reg int got;
  auto char c;

  if (ttyf < 0)
    ttyf = openqtty ();
  if (ttyf < 0)
    fatal (TTY, "Unavailable");
  VOID writeall (ttyf, msg, (uint) strlen (msg));
  idx = 0;
  while ((got = read (ttyf, &c, 1)) == 1)
    if (c == '\04' || c == '\n')
      break;
    else if (c == ' ' || c == '\t')
      continue;
    else if (idx < limit - 1)
      answer[idx++] = c;
  if (got < 0)
    fatal (TTY, syserr ());
  answer[idx] = '\0';
}

/*
 * nextclos()
 *
 * Close an archive.
 */
STATIC void
nextclos ()
{
  if (arfd != STDIN && arfd != STDOUT && !fflag)
    {
      VOID close (arfd);
      arfd = -1;
    }
  areof = 0;
  if (arname && *arname == '!')
    pipewait ();
}

/*
 * nextopen()
 *
 * Open an archive. Returns 0 if successful, -1 otherwise.
 */
STATIC int
nextopen (mode)
     int mode;
{
#if 1
  if (arfd != -1)
    close (arfd);
#endif
  if (*arname == '!')
    arfd = pipeopen (mode);
  else if (strcmp (arname, "-") == 0)
    arfd = mode ? STDOUT : STDIN;
  else
    {
#ifdef	CTC3B2
      if (Cflag)
	{
	  reg int oops;
	  reg int fd;

	  oops = ((fd = open (arname, O_RDWR | O_CTSPECIAL)) < 0
		  || ioctl (fd, STREAMON) < 0);
	  VOID close (fd);
	  if (oops)
	    return (warnarch (syserr (), (off_t) 0));
	}
#endif /* CTC3B2 */

#ifdef linux
      /* Do O_SYNC writing to the floppy drive */
      if(Fflag && mode) 
        arfd = open (arname, mode | O_CREAT | O_TRUNC | O_SYNC, 0666 & ~mask);
      else
        arfd = mode ? creat (arname, 0666 & ~mask) : open (arname, mode);
#else
      arfd = mode ? creat (arname, 0666 & ~mask) : open (arname, mode);
#endif /* linux */

    }
  if (arfd < 0)
    return (warnarch (syserr (), (off_t) 0));
  arleft = aruntil;
  return (0);
}

/*
 * openin()
 *
 * Open the next input file. Returns a file descriptor, 0 if no data
 * exists, or -1 at EOF. This kludge works because standard input is
 * in use, preventing open() from returning zero.
 */
STATIC int
openin (name, asb, cratio)
     char *name;
     reg Stat *asb;
     int *cratio;
{
  int fd;
  auto char local[PATHSIZE];
  char *label;

  if (cratio)
    *cratio = 100;
  for (;;)
    {
      if (lineget (stdin, name) < 0)
	return (-1);

      /* control file syntax is //--SOURCEFILE[ LABEL] */
      if(strncmp(name,"//--",4)==0)
      {
	  strcpy(local,&name[4]);
	  
	  label=index(local,' ');
	  if(label!=NULL)
	  {
	      *label='\000';	/* separate filename and label */
	      label++;
	  }
	  else
	      label=NOLABEL;
	  if(*label=='\000') label=NOLABEL;

	  sprintf(name,"%s/%s",CONTROLNAME,label);

	  /* Now, local is the filename, label is the label, and name is
             the filename in the archive, with the label embedded.
          */

	  if (STAT (local, asb) < 0)
	  {
	      VOID warn (local, syserr ());
	      continue;
	  }

          /* Sun machines put trash in sb_rdev in stead of setting it to 0
	     for regular files.  Afio wants a 0 */
          if(! (S_ISBLK(asb->sb_mode)||S_ISCHR(asb->sb_mode)) ) asb->sb_rdev=0;

	  if ((asb->sb_mode & S_IFMT) != S_IFREG)
	  {
	      VOID warn (local, "Cannot read control file from this source.");
	      continue;
	  }

	  /* Flag that this is a control file.
	     An archive entry is a control file if it is a regular file and
             if the ISCONTROL flag is set.  The filename is not important.
	  */
	  asb->sb_rdev |= RDEV_ISCONTROL;

	  if (asb->sb_size == 0)
	      return (0);
	  if ((fd = open (local, O_RDONLY)) >= 0)
	      return (fd);
	  VOID warn (local, syserr ());
	  continue;	  
      }

      /* not a control file, make a normal archive entry */

      if (nameopt (name) < 0)
	  continue;
      if (!gflag)
	  VOID strcpy (local, name);
      else if (dirchg (name, local) < 0)
	  continue;

      if ((hflag ? STAT (local, asb) : LSTAT (local, asb)) < 0)
	{
	  VOID warn (name, syserr ());
	  continue;
	}

      /* Sun machines put trash in sb_rdev in stead of setting it to 0
	 for regular files.  Afio wants a 0 */
      if(! (S_ISBLK(asb->sb_mode)||S_ISCHR(asb->sb_mode)) ) asb->sb_rdev=0;

      switch (asb->sb_mode & S_IFMT)
	{
	case S_IFDIR:
	  asb->sb_nlink = 1;
	  asb->sb_size = 0; 
	  return (0);
#ifdef	S_IFLNK
	case S_IFLNK:
	  if ((asb->sb_size = readlink (local,
			      asb->sb_link, sizeof (asb->sb_link) - 1)) < 0)
	    {
	      VOID warn (name, syserr ());
	      continue;
	    }
	  asb->sb_link[asb->sb_size] = '\0';
	  return (0);
#endif /* S_IFLNK */
	case S_IFREG:
	  if (asb->sb_size == 0)
	    return (0);
	  if ((fd = open (local, O_RDONLY)) >= 0)
	    {
	      if (Zflag)
		compressfile (&fd, name, asb, cratio);

	      return (fd);
	    }
	  VOID warn (name, syserr ());
	  break;
	default:
	  asb->sb_size = 0;
	  return (0);
	}
    }
}

/*
 * openincheck()
 *
 * Open the input file. Returns a file descriptor, 0 if no data
 * exists, or -1 at EOF. This kludge works because standard input is
 * in use, preventing open() from returning zero.
 */
STATIC int
openincheck (name, asb, comp, dozflag, linkp)
     char *name;
     reg Stat *asb;
     int *comp;
     int dozflag;
     Link *linkp;
{
  int fd;
  auto char local[PATHSIZE];
  auto char archlink[PATHSIZE];
  char* namedot;

  *uncompto = '\0';
  *comp = 0;
  if (dozflag && ((asb->sb_mode & S_IFMT) == S_IFREG) &&
      ((namedot = strrchr (name, '.')) != NULL) &&
      (asb->sb_rdev & RDEV_NOTCOMPR) == 0 &&
      (strcmp (namedot, ".z") == 0))
    {
      *namedot = '\0';
      strcpy(uncompto, name);
      *comp = 1;
    }
  else
    namedot = NULL;		/* not uncompressing */
  if (nameopt (name) < 0)
    return -1;
  if (!gflag)
    VOID strcpy (local, name);
  else if (dirchg (name, local) < 0)
    return 0;
  switch (asb->sb_mode & S_IFMT)
    {
    case S_IFDIR:
      asb->sb_nlink = 1;
      asb->sb_size = 0;
      /* do not check if directory exists */
      /* do not check directory permissions and access times */
      return (0);
#ifdef	S_IFLNK
    case S_IFLNK:
      strcpy(archlink,asb->sb_link);

      if ((asb->sb_size = 
             readlink (local, asb->sb_link, sizeof (asb->sb_link) - 1)) < 0)
	{
	  VOID warn (name, syserr ());
	  asb->sb_size = 0; 
	  return 0;
	}
      asb->sb_link[asb->sb_size] = '\0';
      
      if(strcmp(archlink,asb->sb_link)!=0)
	  VOID warn (name, "Difference in symlink destination filename");

     asb->sb_size = 0;
     return (0);
#endif /* S_IFLNK */
    case S_IFREG:
      /* does not check file permissions and access times */
      if (asb->sb_size == 0)
	return (0);

      /* get last access time if we want to restore it */
      if(aflag)
      {
	  if (STAT (local, &atime_sb) < 0)
	  {
	      /* silently fail, the open below will report an error */
	  }
	  else
	  {
	      atime_sb_valid=1;
	  }	  
      }

      if ((fd = open (local, O_RDONLY)) >= 0)
	{
	  return (fd);
	}
      VOID warn (name, syserr ());
      return 0;
    default:
      asb->sb_size = 0;
      return (0);
    }
}

/*
 * opencontrolscript()
 *
 * Start a control script. Returns the stdin of the script,
 * -1 if unsuccessful.
 * If no control script option given, return file handle of /dev/null.
 */
STATIC int
opencontrolscript (name)
 char *name;
{
    auto int pfd[2];
    int comppid;
    char *label;

    *uncompto='\000';  /* for -v output on restore, verify */

    if(controlscript == NULL )
    {
	/* ignore data */
	warnarch("No -D option given, ignoring control file.",(off_t)0);
	return open("/dev/null",O_WRONLY);
    }

    if(strcmp(controlscript,"")==0)
    {
	/* silently ignore data */
	return open("/dev/null",O_WRONLY);
    }

    label=index(name,'/');
    if(label==NULL) label=NOLABEL; else label++;
    
    if (pipe (pfd) < 0) 
    {
	warn("Control script",syserr());
	return -1;
    }

    if ((comppid = xfork ("opencontrolscript(in), running control script", NODIE)) == 0)
    {
        /* Ignoring SIGPIPE may cause strange results in some shell scripts */
	VOID signal (SIGPIPE, SIG_DFL);

	if (arfd != STDIN && arfd != STDOUT) VOID close (arfd);
	VOID close (pfd[1]);	/* child */
	VOID close (fileno (stdin));
	VOID dup (pfd[0]);
	VOID close (pfd[0]);
	
        execlp (controlscript, controlscript, label, 0);

	warnarch("Problems running control script:",(off_t)0);	       
	warn(controlscript,syserr());
	exit (1);
    }

    /*parent*/
    
    if (comppid < 0)
    {
	warn("Control script",syserr());
	return -1;
    }

    close (pfd[0]);

    /* this enables some safety checks */
    uncompressrun = comppid;

    return pfd[1];
}

/*
 * openotty()
 *
 * Open an output file. Returns the output file descriptor,
 * 0 if no data is required or -1 if unsuccessful. Note that
 * UNIX open() will never return 0 because the standard input
 * is in use.
 */
STATIC int
openotty (name, asb, linkp, ispass, dozflag)
     char *name;
     reg Stat *asb;
     Link *linkp;
     reg int ispass;
     int dozflag;
{
  reg int exists;
  reg int fd;
  reg ushort perm;
  ushort operm;
  ushort ouid;
  ushort ogid;
  Path *path;
  auto Stat osb;
#ifdef	S_IFLNK
  reg int ssize;
  auto char sname[PATHSIZE];
#endif /* S_IFLNK */
  char *namedot;
  auto int pfd[2];
  int comppid;

  if(ISCONTROL(asb))
      return opencontrolscript(name);
  
  *uncompto = '\0';
  /*
   * -iZ try to uncompress a compress'd regular file
   */
  if (dozflag && !linkp && ((asb->sb_mode & S_IFMT) == S_IFREG) &&
      ((namedot = strrchr (name, '.')) != NULL) &&
      (asb->sb_rdev & RDEV_NOTCOMPR) == 0 &&
      (strcmp (namedot, ".z") == 0))
    {
      *namedot = '\0';
    }
  else
    namedot = NULL;		/* not uncompressing */
  if (exists = (LSTAT (name, &osb) == 0))
    if (ispass
	&& osb.sb_ino == asb->sb_ino
	&& osb.sb_dev == asb->sb_dev)
      return (warn (name, "Same file"));
    else if ((osb.sb_mode & S_IFMT) == (asb->sb_mode & S_IFMT))
      operm = osb.sb_mode & (xflag ? S_IPERM : S_IPOPN);
    else if (afremove (name, &osb) < 0)
      return (warn (name, syserr ()));
    else
      exists = 0;
  if (linkp)
    {
      if (exists)
	if (asb->sb_ino == osb.sb_ino
	    && asb->sb_dev == osb.sb_dev)
	  return (0);
	else if (unlink (name) < 0)
	  return (warn (name, syserr ()));
	else
	  exists = 0;
      for (path = linkp->l_path; path; path = path->p_forw)
	if (link (path->p_name, name) == 0
	    || (errno == ENOENT
		&& dirneed (name) == 0
		&& link (path->p_name, name) == 0))
	  return (0);
	else if (errno != EXDEV)
	  return (warn (name, syserr ()));
      VOID warn (name, "Link broken");
      linkalso (linkp, name);
    }
  perm = asb->sb_mode & (xflag ? S_IPERM : S_IPOPN);
  if (exists)
    {
      ouid = osb.sb_uid;
      ogid = osb.sb_gid;
    }
  switch (asb->sb_mode & S_IFMT)
    {
    case S_IFBLK:
    case S_IFCHR:
      fd = 0;
      if (exists)
	if (asb->sb_rdev == osb.sb_rdev)
	  if (perm != operm && chmod (name, perm) < 0)
	    return (warn (name, syserr ()));
	  else
	    break;
	else if (afremove (name, &osb) < 0)
	  return (warn (name, syserr ()));
	else
	  exists = 0;
      if (mknod (name, asb->sb_mode, asb->sb_rdev) < 0
	  && (errno != ENOENT
	      || dirneed (name) < 0
	      || mknod (name, asb->sb_mode, asb->sb_rdev) < 0))
	return (warn (name, syserr ()));
      break;
    case S_IFDIR:
      if (exists)
	if (xflag && (asb->sb_uid != ouid || asb->sb_gid != ogid) &&
		 chown (name, asb->sb_uid, asb->sb_gid) < 0)
	  return (warn (name, syserr ()));
	else if (perm != operm && chmod (name, perm) < 0)
	  return (warn (name, syserr ()));
	else;
      else if (dirneed (name) < 0 || dirmake (name, asb) < 0)
	return (warn (name, syserr ()));
      return (0);
#ifdef	S_IFIFO
    case S_IFIFO:
      fd = 0;
      if (exists)
	if (perm != operm && chmod (name, perm) < 0)
	  return (warn (name, syserr ()));
	else;
      else if (mknod (name, asb->sb_mode, (dev_t) 0) < 0
	       && (errno != ENOENT
		   || dirneed (name) < 0
		   || mknod (name, asb->sb_mode, (dev_t) 0) < 0))
	return (warn (name, syserr ()));
      break;
#endif /* S_IFIFO */
#ifdef	S_IFSOCK
    case S_IFSOCK:
      fd = 0;
      if (exists)
	if (perm != operm && chmod (name, perm) < 0)
	  return (warn (name, syserr ()));
	else;
      else if (mknod (name, asb->sb_mode, (dev_t) 0) < 0
	       && (errno != ENOENT
		   || dirneed (name) < 0
		   || mknod (name, asb->sb_mode, (dev_t) 0) < 0))
	return (warn (name, syserr ()));
      break;
#endif /* S_IFSOCK */
#ifdef	S_IFLNK
    case S_IFLNK:
      if (exists)
	if ((ssize = readlink (name, sname, sizeof (sname))) < 0)
	  return (warn (name, syserr ()));
	else if (strncmp (sname, asb->sb_link, ssize) == 0)
	  return (0);
	else if (afremove (name, &osb) < 0)
	  return (warn (name, syserr ()));
	else
	  exists = 0;
      if (symlink (asb->sb_link, name) < 0
	  && (errno != ENOENT
	      || dirneed (name) < 0
	      || symlink (asb->sb_link, name) < 0))
	return (warn (name, syserr ()));
      return (0);		/* Can't chown()/chmod() a symbolic link */
#endif /* S_IFLNK */
    case S_IFREG:
      if (exists)
	if (nflag && osb.sb_mtime > asb->sb_mtime)
	  return (warn (name, "Newer file exists"));
	else if (unlink (name) < 0)
	  return (warn (name, syserr ()));
	else
	  exists = 0;
      if ((fd = creat (name, perm)) < 0
	  && (errno != ENOENT
	      || dirneed (name) < 0
	      || (fd = creat (name, perm)) < 0))
	return (warn (name, syserr ()));
      if (dozflag && !linkp && namedot)
	{
	  if (pipe (pfd) >= 0)
	    {
	      if ((comppid = xfork ("openotty(in), compressing", NODIE)) == 0)
		{
		  if (arfd != STDIN && arfd != STDOUT) VOID close (arfd);
		  VOID close (pfd[1]);	/* child */
		  VOID close (fileno (stdin));
		  VOID dup (pfd[0]);
		  VOID close (pfd[0]);

		  VOID close (fileno (stdout));
		  if (dup (fd) < 0)
		    exit (1);
		  if(compressargs)
		      execvp (compressprog, compress_arg_list);
		  else
		      execlp (compressprog, compressprog, "-d", "-c", 0);
		  fprintf (stderr, "Could not uncompress, errno %d\n", errno);
		  exit (1);
	      }
	      else
		  /* life seems ok */
		  if (comppid > 0)
		  {
		      close (fd);
		      fd = pfd[1];
		      close (pfd[0]);
		      uncompressrun = comppid;
		      strcpy (uncompto, name);
#if 0
		      *namedot = '.';
#endif
		      break;
		  }
	  }
	  /* we failed try again, not uncompressing the file */
	  unlink (name);
	  *namedot = '.';
	  return (openotty (name, asb, linkp, ispass, FALSE));
	}

      break;
    default:
      return (warn (name, "Unknown filetype"));
    }
  if (xflag && (!exists || asb->sb_uid != osb.sb_uid
		|| asb->sb_gid != osb.sb_gid))
    if (chown (name, uid == 0 ? ush (asb->sb_uid) : uid,
	       ush (asb->sb_gid)))
      perror (name);
    else /* chown may have cleared suid/sgid flags; restore them */
	if(perm&S_IPEXE)
	    if(chmod (name, perm) < 0)
		return (warn (name, syserr ()));
  if (linkp == NULL && asb->sb_nlink > 1)
    VOID linkto (name, asb);
  return (fd);
}

/*
 * openqtty()
 *
 * Open the terminal for interactive queries (sigh). Assumes that
 * background processes ignore interrupts and that the open() or
 * the isatty() will fail for processes which are not attached to
 * terminals. Returns a file descriptor (-1 if unsuccessful).
 */
int
openqtty ()
{
  reg VOIDFN (*intr) ();
  int fd;

  fd = -1;
  if (!Fflag)
    {
      if ((intr = signal (SIGINT, SIG_IGN)) == SIG_IGN)
	return (fd);
      VOID signal (SIGINT, intr);
    }

  if ((fd = open (TTY, O_RDWR)) < 0)
    {
      VOID warn (TTY, syserr ());
    }
  else if (!isatty (fd))
    VOID warn (TTY, "Is not a tty");
  return (fd);
}

/*
 * options()
 *
 * Decode most reasonable forms of UNIX option syntax. Takes main()-
 * style argument indices (argc/argv) and a string of valid option
 * letters. Letters denoting options with arguments must be followed
 * by colons. With valid options, returns the option letter and points
 * "optarg" at the associated argument (if any). Returns '?' for bad
 * options and missing arguments. Returns zero when no options remain,
 * leaving "optind" indexing the first remaining argument.
 */
STATIC int
options (ac, av, proto)
     int ac;
     register char **av;
     char *proto;
{
  register int c;
  register char *idx;
  static int optsub;

  if (optind == 0)
    {
      optind = 1;
      optsub = 0;
    }
  optarg = NULL;
  if (optind >= ac)
    return (0);
  if (optsub == 0 && (av[optind][0] != '-' || av[optind][1] == '\0'))
    return (0);
  switch (c = av[optind][++optsub])
    {
    case '\0':
      ++optind;
      optsub = 0;
      return (options (ac, av, proto));
    case '-':
      ++optind;
      optsub = 0;
      return (0);
    case ':':
      return ('?');
    }
  if ((idx = strchr (proto, c)) == NULL)
    return ('?');
  if (idx[1] != ':')
    return (c);
  optarg = &av[optind][++optsub];
  ++optind;
  optsub = 0;
  if (*optarg)
    return (c);
  if (optind >= ac)
    return ('?');
  optarg = av[optind++];
  return (c);
}

/*
 * optsize()
 *
 * Interpret a "size" argument. Recognizes suffices for blocks
 * (512-byte), kilobytes and megabytes and blocksize. Returns
 * the size in bytes.
 */
STATIC off_t
optsize (str)
     char *str;
{
  reg char *idx;
  reg off_t number;
  reg off_t result;

  result = 0;
  idx = str;
  for (;;)
    {
      number = 0;
      while (*idx >= '0' && *idx <= '9')
	number = number * 10 + *idx++ - '0';
      switch (*idx++)
	{
	case 'b':
	  result += number * 512;
	  continue;
	case 'k':
	  result += number * 1024;
	  continue;
	case 'm':
	  result += number * 1024L * 1024L;
	  continue;
	case 'x':
	  result += number * arbsize;
	  continue;
	case '+':
	  result += number;
	  continue;
	case '\0':
	  result += number;
	  break;
	default:
	  break;
	}
      break;
    }
  if (*--idx)
    fatal (str, "Unrecognizable value");
  return (result);
}

/*
 * out()
 *
 * Write an archive.
 */
STATIC VOIDFN
out (av)
     char **av;
{
    reg int fd;
    auto Stat sb;
    auto char name[PATHSIZE];
    auto int compression;
#ifdef linux
    auto struct utimbuf tstamp;
#else
    auto time_t tstamp[2];
#endif
    
    if (*av)
	fatal (*av, "Extraneous argument");
    
    while ((fd = openin (name, &sb, &compression)) >= 0)
    {
	bytepos=total; /* offset of this file in archive */
	
	if (!lflag && sb.sb_nlink > 1)
	{
	    if (linkfrom (&sb))
		sb.sb_size = 0;
	    else
		VOID linkto (name, &sb);
	}
	
	outhead (name, &sb);
	if (fd)
	    if (fd==ZIPFD) 
	    {
		outdatazip(zipfdfd,name,sb.sb_size);
		close(zipfdfd);
	    } 
	    else if (fd==MEMFD) 
	    {
		outdatamem(name,sb.sb_size);
	    }
	    else
		VOID close (outdata (fd, name, sb.sb_size));
	
	if (vflag)
	{
	    if(printbytepos) fprintf(stderr,"%lu ",bytepos);
	    
	    if ((name[0] == '/') && !abspaths && (name[1]!=0))
		fprintf (stderr, "%s -- ", &name[1]); 
	    else
		fprintf (stderr, "%s -- ", name); 
	    
	    /* We do not check for a broken pipe on stderr, we wouldn't want
	       to stop making the archive if this happens.
	       */
	    
#if 0
	    /* debug code added by KH.  Are we leaking file descriptors? */
	{ int i; i=dup(0); close(i); fprintf(stderr,"%d",i); }
#endif
	    if ((fd==ZIPFD)||(fd==MEMFD))
		VOID fprintf (stderr, "(%02d%%)\n", compression);
	    else
		VOID fputs ("okay\n", stderr);
	}
	
	if(aflag && ((sb.sb_mode & S_IFMT)==S_IFREG))
	{
	    /* reset access time, this distroys the ctime btw. */
#ifdef linux
	    tstamp.actime = sb.sb_atime;
	    tstamp.modtime = sb.sb_mtime;
	    VOID utime (name, &tstamp);
#else
	    tstamp[0] = sb.sb_atime;
	    tstamp[1] = sb.sb_mtime;
	    VOID utime (name, tstamp);
#endif
	}
    }
    outeof (TRAILER, TRAILZ);
}

/*
 * outalloc()
 *
 * Allocate buffer space previously referenced by outavail().
 */
STATIC void
outalloc (len)
     reg uint len;
{
  bufidx += len;
  total += len;
}

/*
 * outavail()
 *
 * Index buffer space for archive output. Stores a buffer pointer
 * at a given location. Returns the number of bytes available.
 */
STATIC uint
outavail (bufp)
     reg char **bufp;
{
  reg uint have;

  while ((have = bufend - bufidx) == 0)
    { 
      outflush (NOTDONE);
    }

  *bufp = bufidx;
  return (have);
}

/*
 * outdata()
 *
 * Write archive data. Continues after file read errors, padding with
 * null characters if neccessary. Always returns the given input file
 * descriptor.
 */
STATIC int
outdata (fd, name, size)
     int fd;
     char *name;
     reg off_t size;
{
  reg uint chunk;
  reg int got;
  reg int oops;
  reg uint avail;
  auto char *buf;

  oops = got = 0;
  while (size)
    {
      avail = outavail (&buf);
      size -= (chunk = size < avail ? (uint) size : avail);
      if (oops == 0 && (got = read (fd, buf, chunk)) < 0)
	{
	  oops = warn (name, syserr ());
	  got = 0;
	}
      if (got < chunk) /* Gee, is this portable?? --KH */
	{
	  if (oops == 0)
	    oops = warn (name, "Early EOF");
	  while (got < chunk)
	    buf[got++] = '\0';
	}
      outalloc (chunk);
    }
  return (fd);
}

/*
 * outdatazip()
 *
 * Write archive data. Continues after file read errors, padding with
 * null characters if neccessary.
 *
 * Special version for reading from gzip through a pipe.
 */
void
outdatazip (fd, name, size)
     int fd;
     char *name;
     reg off_t size;
{
  reg uint chunk;
  reg int got;
  reg int oops;
  reg uint avail;
  auto char *buf;

  /* read from pipe */
  while (size)
    {
      avail = outavail (&buf);
      chunk = (size < avail ? (uint) size : avail);

      got = read (fd, buf, chunk);
      if(got<=0) break;

      outalloc (got);
      size-=got;
    }
  waitforgzip(); /* wait for child to exit */

  if(size!=0)
        warn (name, "Error zipping file, written damaged copy to archive.");

  /* pad with zeros, if necessary */
  while (size)
    {
      avail = outavail (&buf);
      size -= (chunk = size < avail ? (uint) size : avail);
      got=0;
      while (got < chunk)
          buf[got++] = '\0';
      outalloc (chunk);
    }

}

/*
 * outdatamem()
 *
 * Write archive data.
 *
 * Special version for reading from internal memory buffer.
 */
void
outdatamem (name, size)
     char *name;
     reg off_t size;
{
  reg uint chunk;
  reg int got;
  reg uint avail;
  auto char *buf;

  /* read from memory */
  memreset();
  
  while (size)
    {
      avail = outavail (&buf);
      chunk = (size < avail ? (uint) size : avail);

      got=memread (buf, chunk);
      if (got==0) break;

      outalloc (got);
      size-=got;
    }

  memfree();
}

/*
 * outeof()
 *
 * Write an archive trailer.
 */
STATIC void
outeof (name, namelen)
     char *name;
     reg uint namelen;
{
  reg off_t pad;
  auto char header[M_STRLEN + H_STRLEN + 1];

  if (pad = (total + M_STRLEN + H_STRLEN + namelen) % arpad)
    pad = arpad - pad;
  VOID strcpy (header, M_ASCII);
  VOID sprintf (header + M_STRLEN, H_PRINT, 0, 0,
		0, 0, 0, 1, 0, (time_t) 0, namelen, pad);
  outwrite (header, M_STRLEN + H_STRLEN);
  outwrite (name, namelen);
  outpad (pad);
  outflush (DONE);
  if (fflag)
    outwait ();
}

/*
 * outflush()
 *
 * Flush the output buffer. Optionally fork()s to allow the
 * parent to refill the buffer while the child waits for the
 * write() to complete.
 */
STATIC void
outflush (done)
     int done;
{
  int wrstat;

  /*
   * in this case we are a floppy and want to write the floppy from one
   * buffer (to have a copy of the data to verify against)
   */
  bufend = buffer + (aruntil ? min (buflen, arleft) : buflen);
  if (Fflag && (done == NOTDONE) && ((bufend - bufidx) > 0))
    {
      return;
    }

  /*
   * Otherwise open up the next volume once we've run out of space
   */
  if ( !Fflag && aruntil && arleft == 0)
     next (O_WRONLY, "Output limit reached");

  wrstat = writedisk (1);
  bufend = (bufidx = buffer) + (aruntil ? min (buflen, arleft) : buflen);
}

/*
 * outhead()
 *
 * Write an archive header.
 */
STATIC void
outhead (name, asb)
     reg char *name;
     reg Stat *asb;
{
  reg uint namelen;
  auto char header[M_STRLEN + H_STRLEN + 1];

  if ((name[0] == '/') && !abspaths)
    if (name[1])
      ++name;
    else
      name = ".";
  namelen = (uint) strlen (name) + 1;
  VOID strcpy (header, M_ASCII);
  VOID sprintf (header + M_STRLEN, H_PRINT, ush (asb->sb_dev),
		ush (asb->sb_ino), ush (asb->sb_mode), ush (asb->sb_uid),
		ush (asb->sb_gid), ush (asb->sb_nlink), ush (asb->sb_rdev),
		mflag ? timenow : asb->sb_mtime, namelen, asb->sb_size);
  outwrite (header, M_STRLEN + H_STRLEN);
  outwrite (name, namelen);
#ifdef	S_IFLNK
  if ((asb->sb_mode & S_IFMT) == S_IFLNK)
    outwrite (asb->sb_link, (uint) asb->sb_size);
#endif /* S_IFLNK */
}

/*
 * outpad()
 *
 * Pad the archive.
 */
STATIC void
outpad (pad)
     reg off_t pad;
{
  reg int idx;
  reg int len;

  while (pad)
    {
      if ((len = bufend - bufidx) > pad)
	len = pad;
      for (idx = 0; idx < len; ++idx)
	*bufidx++ = '\0';
      total += len;
      outflush (NOTDONE);
      pad -= len;
    }
}

/*
 * outwait()
 *
 * Wait for the last background outflush() process (if any). The child
 * exit value is zero if successful, 255 if a write() returned zero or
 * the value of errno if a write() was unsuccessful.
 */
STATIC void
outwait ()
{
  auto int status;

  if (outpid == 0)
    return;
  status = xwait (outpid, "outwait()", TRUE);
  outpid = 0;
  if (status)
    fatal (arspec, "Child error");
}

/*
 * outwrite()
 *
 * Write archive data.
 */
STATIC void
outwrite (idx, len)
     reg char *idx;
     uint len;
{
  reg uint have;
  reg uint want;
  reg char *endx = idx + len;

  while (want = endx - idx)
    {
      while ((have = bufend - bufidx) == 0)
	outflush (NOTDONE);
      if (have > want)
	have = want;
      memcpy (bufidx, idx, have);
      bufidx += have;
      idx += have;
      total += have;
    }
}

/*
 * pass()
 *
 * Copy within the filesystem.
 */
STATIC VOIDFN
pass (av)
     reg char **av;
{
  reg int fd;
  reg char **avx;
  auto Stat sb;
  auto char name[PATHSIZE];

  for (avx = av; *avx; ++avx)
    {
      if (gflag && **avx != '/')
	fatal (*avx, "Relative pathname");
      if (STAT (*avx, &sb) < 0)
	fatal (*avx, syserr ());
      if ((sb.sb_mode & S_IFMT) != S_IFDIR)
	fatal (*avx, "Not a directory");
    }
  while ((fd = openin (name, &sb, (int *) 0)) >= 0)
    {
      if (passitem (name, &sb, fd, av))
	VOID close (fd);
      if (vflag)
	{
	  if (*uncompto)
	    VOID fprintf (stderr, "%s -- uncompressed\n", uncompto);
	  else
	    VOID fprintf (stderr, "%s -- okay\n", name);
	}
    }
}

/*
 * passdata()
 *
 * Copy data to one file. Doesn't believe in input file
 * descriptor zero (see description of kludge in openin()
 * comments). Closes the provided output file descriptor.
 */
STATIC void
passdata (from, ifd, to, ofd)
     char *from;
     reg int ifd;
     char *to;
     reg int ofd;
{
  reg int got;
  reg int sparse;
  auto char block[FSBUF];

  if (ifd)
    {
      VOID lseek (ifd, (off_t) 0, 0);
      sparse = 0;
      while ((got = read (ifd, block, sizeof (block))) > 0
	     && (sparse = fswrite (ofd, block, (uint) got)) >= 0)
	total += got;
      if (got)
	VOID warn (got < 0 ? from : to, syserr ());
      else if (sparse > 0
	       && (lseek (ofd, (off_t) - sparse, 1) < 0
		   || writeall (ofd, block, (uint) sparse) != sparse))
	VOID warn (to, syserr ());
    }
  VOID close (ofd);
}

/*
 * passitem()
 *
 * Copy one file. Returns given input file descriptor.
 */
STATIC int
passitem (from, asb, ifd, dir)
     char *from;
     Stat *asb;
     reg int ifd;
     reg char **dir;
{
  reg int ofd;

#ifdef linux
  auto struct utimbuf tstamp;
#else
  auto time_t tstamp[2];
#endif
  auto char to[PATHSIZE];

  while (*dir)
    {
      if (nameopt (strcat (strcat (strcpy (to, *dir++), "/"), from)) < 0)
	continue;
      if ((ofd = openotty (to, asb,
		lflag ? linkto (from, asb) : linkfrom (asb), 1, Zflag)) < 0)
	continue;
      if (ofd > 0)
	passdata (from, ifd, to, ofd);
#ifdef linux
      tstamp.actime = tstamp.modtime = mflag ? timenow : asb->sb_mtime;
      VOID utime (to, &tstamp);
#else
      tstamp[0] = tstamp[1] = mflag ? timenow : asb->sb_mtime;
      VOID utime (to, tstamp);
#endif
      /* safety */
      if (uncompressrun)
	{
	  VOID xwait (uncompressrun, "passitem xwait()", TRUE);
	  uncompressrun = 0;
	}
    }
  return (ifd);
}

/*
 * pipechld()
 *
 * Child portion of pipeline fork.
 */
STATIC int
pipechld (mode, pfd)
     int mode;
     reg int *pfd;
{
  reg char **av;
  auto char *arg[32];

  av = arg;
  if ((*av = getenv ("SHELL")) && **av)
    ++av;
  else
    *av++ = "/bin/sh";
  *av++ = "-c";
  *av++ = arname + 1;
  *av = NULL;
  if (mode)
    {
      VOID close (pfd[1]);
      VOID close (STDIN);
      VOID dup (pfd[0]);
      VOID close (pfd[0]);
      VOID close (STDOUT);
      VOID open ("/dev/null", O_WRONLY);
    }
  else
    {
      VOID close (STDIN);
      VOID open ("/dev/null", O_RDONLY);
      VOID close (pfd[0]);
      VOID close (STDOUT);
      VOID dup (pfd[1]);
      VOID close (pfd[1]);
    }
  if (ttyf >= 0)
    VOID close (ttyf);
  VOID execvp (arg[0], arg);
  VOID warn (arg[0], syserr ());
  _exit (1);
}

/*
 * pipeopen()
 *
 * Open an archive via a pipeline. Returns a file
 * descriptor, or -1 if unsuccessful.
 */
STATIC int
pipeopen (mode)
     reg int mode;
{
  auto int pfd[2];

  if (pipe (pfd) < 0)
    return (-1);
  if ((pipepid = xfork ("pipeopen()", DIE)) == 0)
    pipechld (mode, pfd);
  if (mode)
    {
      VOID close (pfd[0]);
      return (pfd[1]);
    }
  else
    {
      VOID close (pfd[1]);
      return (pfd[0]);
    }
}

/*
 * pipewait()
 *
 * Await a pipeline.
 */
STATIC void
pipewait ()
{
  reg int status;

  if (pipepid == 0)
    return;
  status = xwait (pipepid, "pipewait()", TRUE);
  pipepid = 0;
  if (status)
    fatal (arspec, "Pipeline error");
}

/*
 * prsize()
 *
 * Print a file offset.
 */
STATIC void
prsize (stream, size)
     FILE *stream;
     reg off_t size;
{
  reg off_t n;

  if (n = (size / (1024L * 1024L)))
    {
      VOID fprintf (stream, "%ldm+", n);
      size -= n * 1024 * 1024;
    }
  if (n = (size / 1024))
    {
      VOID fprintf (stream, "%ldk+", n);
      size -= n * 1024;
    }
  VOID fprintf (stream, "%ld", size);
}

#ifndef	MKDIR

/*
 * rmdir()
 *
 * Remove a directory via "/bin/rmdir". Sets errno to a
 * questionably sane value upon failure.
 */
STATIC int
rmdir (name)
     reg char *name;
{
  reg int pid;

  if ((pid = xfork ("rmdir()", DIE)) == 0)
    {
      VOID close (fileno (stdin));
      VOID close (fileno (stdout));
      VOID close (fileno (stderr));
      VOID open ("/dev/null", O_RDWR);
      VOID dup (fileno (stdin));
      VOID dup (fileno (stdin));
      VOID execl ("/bin/rmdir", "rmdir", name, (char *) NULL);
      exit (1);
    }
  if (xwait (pid, "rmdir()", TRUE) == 0)
    return (0);
  errno = EACCES;
  return (-1);
}

#endif /* MKDIR */

/*
 * fswrite()
 *
 * Write a filesystem block. Seeks past sparse blocks. Returns
 * 0 if the block was written, the given length for a sparse
 * block or -1 if unsuccessful.
 */
STATIC int
fswrite (fd, buf, len)
     int fd;
     char *buf;
     uint len;
{
  reg char *bidx;
  reg char *bend;

  /*
   * if -j (no sparse blocks) or compressrun (piping to uncompress utility)
   * then do not bother looking for empty buffers, just write the data.
   */
  if (jflag || uncompressrun)
    return (writeall (fd, buf, len) == len ? 0 : -1);
  bend = (bidx = buf) + len;
  while (bidx < bend)
    if (*bidx++)
      return (writeall (fd, buf, len) == len ? 0 : -1);
  return (lseek (fd, (off_t) len, 1) < 0 ? -1 : len);
}

/*
 * syserr()
 *
 * Return pointer to appropriate system error message.
 */
STATIC char *
syserr ()
{
  static char msg[40];

  if (errno > 0 && errno < sys_nerr)
    return ((char *) sys_errlist[errno]);
  VOID sprintf (msg, "Unknown error (errno %d)", errno);
  return (msg);
}

/*
 * toc()
 *
 * Print archive table of contents.
 */
STATIC VOIDFN
toc (av)
     reg char **av;
{
  auto Stat sb;
  auto char name[PATHSIZE];

  if (*av)
    fatal (*av, "Extraneous argument");
  name[0] = '\0';
  while (inhead (name, &sb) == 0)
    {
	if (ISCONTROL(&sb))
	{
	    /* list it */
	    if (namecmp (name,&sb) == 0)
		tocentry (name, &sb);
	    
	    /* process control file */
	    if(inentry (name, &sb) < 0)
		if (inskip (sb.sb_size) < 0)
		    VOID warn (name, "Skipped file data is corrupt");

	    continue;
	}

	if (namecmp (name,&sb) == 0)
	    tocentry (name, &sb);
	if (inskip (sb.sb_size) < 0)
	    VOID warn (name, "File data is corrupt");
    }
}

/*
 * tocentry()
 *
 * Print a single table-of-contents entry.
 */
STATIC void
tocentry (name, asb)
     char *name;
     reg Stat *asb;
{
  reg Time *atm;
  reg Link *from;
  reg Passwd *pwp;
  reg Group *grp;
  static char *month[] =
  {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  };
  int res;
 
  if(printbytepos) printf("%lu ",bytepos);

  if (vflag)
    {
      tocmode (asb->sb_mode);
      VOID printf (" %2d", asb->sb_nlink);
      atm = localtime (&asb->sb_mtime);
      if (pwp = getpwuid (ush (asb->sb_uid)))
	VOID printf (" %-8s", pwp->pw_name);
      else
	VOID printf (" %-8u", ush (asb->sb_uid));
      if (grp = getgrgid (ush (asb->sb_gid)))
	VOID printf (" %-8s", grp->gr_name);
      else
	VOID printf (" %-8u", ush (asb->sb_gid));
      switch (asb->sb_mode & S_IFMT)
	{
	case S_IFBLK:
	case S_IFCHR:
	  VOID printf (" %3d, %3d",
		       major (asb->sb_rdev), minor (asb->sb_rdev));
	  break;
	case S_IFREG:
	  VOID printf (" %8ld", asb->sb_size);
	  break;
	default:
	  VOID printf ("         ");
	}
      VOID printf (" %3s %2d %02d:%02d:%02d %4d ",
		   month[atm->tm_mon], atm->tm_mday, atm->tm_hour,
		   atm->tm_min, atm->tm_sec, atm->tm_year + 1900);
    }

  {
    char *namedot = strrchr (name, '.');

    if (Zflag && (asb->sb_mode & S_IFMT) == S_IFREG
	&& (asb->sb_rdev & RDEV_NOTCOMPR) == 0
	&& namedot && namedot[1] == 'z' && !namedot[2])
      *namedot = '\0';
    else
      namedot = 0;

    if (ISCONTROL(asb))
	res = printf("//--%s",name);
    else
	res = printf ("%s", name);

    /* to find out about broken pipe as early as possible */ 
    if(res > 0) res = fflush(stdout);
    /* check for broken pipe on stdout, this ends the listing */
    if(res<0) {
	if(errno == EPIPE)
	    fatal("<stdout>", syserr());
    }


    if (vflag && namedot)
      VOID printf (" -- compressed");
   }

  if (vflag || lflag)
    {
      if (asb->sb_nlink > 1)
	if (from = linkfrom (asb))
	  VOID printf (" -> %s",
		       from->l_path->p_name);
	else
	  VOID linkto (name, asb);
#ifdef	S_IFLNK
      if ((asb->sb_mode & S_IFMT) == S_IFLNK)
	VOID printf (" S-> %s", asb->sb_link);
#endif /* S_IFLNK */
    }

  putchar ('\n');
}

/*
 * tocmode()
 *
 * Fancy file mode display.
 */
STATIC void
tocmode (mode)
     reg ushort mode;
{
  switch (mode & S_IFMT)
    {
    case S_IFREG:
      putchar ('-');
      break;
    case S_IFDIR:
      putchar ('d');
      break;
#ifdef	S_IFLNK
    case S_IFLNK:
      putchar ('l');
      break;
#endif /* S_IFLNK */
#ifdef	S_IFSOCK
    case S_IFSOCK:
      putchar ('s');
      break;
#endif /* S_IFSOCK */
    case S_IFBLK:
      putchar ('b');
      break;
    case S_IFCHR:
      putchar ('c');
      break;
#ifdef	S_IFIFO
    case S_IFIFO:
      putchar ('p');
      break;
#endif /* S_IFIFO */
    default:
      VOID printf ("[%o]", mode >> S_IFSHF);
    }
  putchar (mode & 0400 ? 'r' : '-');
  putchar (mode & 0200 ? 'w' : '-');
  putchar (mode & 0100
	   ? mode & 04000 ? 's' : 'x'
	   : mode & 04000 ? 'S' : '-');
  putchar (mode & 0040 ? 'r' : '-');
  putchar (mode & 0020 ? 'w' : '-');
  putchar (mode & 0010
	   ? mode & 02000 ? 's' : 'x'
	   : mode & 02000 ? 'S' : '-');
  putchar (mode & 0004 ? 'r' : '-');
  putchar (mode & 0002 ? 'w' : '-');
  putchar (mode & 0001
	   ? mode & 01000 ? 't' : 'x'
	   : mode & 01000 ? 'T' : '-');
}

/*
 * usage()
 *
 * Print a helpful message and exit.
 */
STATIC void
usage ()
{
  VOID fprintf (stderr, "\n\
Usage:	[filename generator] | %s -o [options] archive  : write archive\n\
        %s -i [options] archive  : install archive\n\
        %s -t [options] archive  : list table-of-contents of archive\n\
        %s -r [options] archive  : verify archive against filesystem\n\
Frequently used options:\n\
 General: -v : verbose        -Z : gzip files\n\
 Tape:    -s [volsize]   : size of volume, can have suffix k or m\n\
          -b [blocksize] : block size (default is 5120)\n\
          -c [count]     : buffer count blocks between doing I/O\n\
 Floppy:  -F : device is a floppy drive, -s required    -K : verify floppies\n\
 Install: -n : protect newer files  -k : skip corrupt data at beginning\n\
 Select:  -y [pattern] : only process files matching pattern\n\
          -Y [pattern] : do not process files matching pattern\n",
		myname, myname, myname, myname);
  VOID fprintf (stderr,"Version %s dated %s\n", VERSION, DATE);
  exit (1);
}

/*
 * warn()
 *
 * Print a warning message. Always returns -1.
 */
STATIC int
warn (what, why)
     char *what;
     char *why;
{
  long dietime;
  dietime = time ((time_t *) NULL);

  VOID fprintf (stderr,
		"%s: \"%s\": %s\n",
		myname, what, why);
  /* ctime() prodives the \n for the fprintf. */
  if (logfile != (FILE *) 0)
    VOID fprintf (logfile, "%s: \"%s\": %s (disk %u) at %s",
		  myname, what, why, arvolume, ctime (&dietime));
  return (-1);
}

/*
 * warnarch()
 *
 * Print an archive-related warning message, including
 * an adjusted file offset. Always returns -1.
 */
STATIC int
warnarch (msg, adjust)
     char *msg;
     off_t adjust;
{
  VOID fprintf (stderr, "%s: \"%s\" [offset ", myname, arspec);
  prsize (stderr, total - adjust);
  VOID fprintf (stderr, "]: %s\n", msg);
  return (-1);
}

/*
 * xfork()
 *
 * Create a child.
 */
STATIC int
xfork (what, die)
     reg char *what;
     int die;
{
  reg int pid;
  reg Child *cp;
  reg int idx;
  static uint delay[] =
  {1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144};

  /* flush error logfile before fork */
  if(logfile != (FILE *)0) fflush(logfile);

#ifdef SIGCHLD
   VOID signal (SIGCHLD, SIG_DFL);	/* SysV mostly... */
#else
   VOID signal (SIGCLD, SIG_DFL);  /* some SysV's... */
#endif
  for (idx = 0; (pid = fork ()) < 0; ++idx)
    {
      if (idx == sizeof (delay))
	if (die)
	  fatal (arspec, syserr ());
	else
	  return (-1);
      VOID warn (what, "Trouble forking...");
      if (Fflag && !die)	/* give up and go on... */
	return (-1);
      sleep (delay[idx]);
    }
  if (idx)
    VOID warn (what, "...successful fork");
  cp = (Child *) memget (sizeof (*cp));
  cp->c_pid = pid;
  cp->c_flags = 0;
  cp->c_status = 0;
  cp->c_forw = children;
  children = cp;
  return (pid);
}

/*
 * xpause()
 *
 * Await a child.
 */
STATIC void
xpause ()
{
  reg Child *cp;
  reg int pid;
  auto int status;

  do
    {
      while ((pid = wait (&status)) < 0)
	;
      for (cp = children; cp && cp->c_pid != pid; cp = cp->c_forw)
	;
    }
  while (cp == NULL);
  cp->c_flags |= CF_EXIT;
  cp->c_status = status;
}

/*
 * xwait()
 *
 * Find the status of a child.
 */
STATIC int
xwait (pid, what, compstat2)
     reg int pid;
     char *what;
     int compstat2;
{
  reg int status;
  reg Child *cp;
  reg Child **acp;
  auto char why[100];

  for (acp = &children; cp = *acp; acp = &cp->c_forw)
    if (cp->c_pid == pid)
      break;
  if (cp == NULL)
    fatal (what, "Lost child");
  while ((cp->c_flags & CF_EXIT) == 0)
    xpause ();
  status = cp->c_status;
  *acp = cp->c_forw;
  free ((char *) cp);
  if (status == 0)
    return (0);
  if (status & 0377)
    VOID sprintf (why, "Killed by signal %d%s",
		  status & 0177, status & 0200 ? " -- core dumped" : "");
  else
    VOID sprintf (why, "Exit %d", (status >> 8) & 0377);

  if ((!compstat2 && (((status >> 8) & 0377) != 2)) || compstat2)
    return (warn (what, why));
  else
    return ((status >> 8) & 0377);
}


/* right now we verify the whole disk */
void
verify (error)
     int error;
{
  char *verbuf;
  char *buf;
  reg time_t began;
  int got, readamt, len;
  int wrstat;
  auto char msg[200];
  auto char answer[20];

  if (*arname == '!')
    {
      VOID warn ("Can't verify a piped command", "");
      return;
    }
  if (!error)
    {
      if ((verbuf = malloc (arbsize)) == NULL)
	fatal (arspec, "Cannot allocate Verify I/O buffer");

      /*
       * close as O_WRONLY and reopen as O_RDONLY to verify (on a
       * disk this is a good thing)
       */
      /* fprintf(stderr,"closing..\n"); */
 
      nextclos ();
      verifycnt++;

      if (nextopen (O_RDONLY) < 0)
	{
	  VOID warn ("re-open for verify failed", "");
	  error = 1;
	}
      else
	{
#ifdef linux
          /* flush the floppy cache. (added by KH) */
          if(Fflag)
	    {   /*   fprintf(stderr,"flushing..\n"); */
              if (ioctl(arfd,FDFLUSH,NULL) < 0) 
                  warn(arname,"can't flush device cache.");
          
            }
#endif
	  fprintf (stderr, "Verifying disk %u...\n", arvolume );
	  for (buf = buffer; len = bufidx - buf;)
	    {
	      readamt = min (len, arbsize);
	      if ((got = read (arfd, verbuf, readamt)) == readamt)
		{
#ifdef HAVEMEMCMP
		  if (memcmp (verbuf, buf, got) != 0)
#else
		  if (bcmp (verbuf, buf, got) != 0)
#endif
		    {
		      VOID warn ("Verify failed", "");
		      error = 1;
		      break;
		    }
		  else
		    buf += got;
		}
	      else
		{
		  VOID warn ("Read returned short", "");
		  fprintf (stderr, "Read %d wanted %d bytes\n", got,
			   readamt);
		  error = 1;
		  break;
		}
	    }
	}
      free (verbuf);
    }
  if (error)
    {
      int answernum = 0;
      nextclos ();

      for (;;)
	{
	    began = time ((time_t *) NULL);
	    VOID sprintf (msg, "\
%s: %s of disk %u has FAILED!\07\n\
\tEnter 1 to RETRY this disk\n\
\tEnter 2 to REFORMAT this disk before a RETRY\n\07\n%s",
			  myname,
			  ((error && (verifycnt == 0)) ? "Writing" :
			   "Verify"),
			  arvolume, hidequit ? "" :
			  "\tEnter \"quit\" to ABORT the backup\n\07");
	    nextask (msg, answer, sizeof (answer));
	    timewait += time ((time_t *) NULL) - began;
	    answernum = atoi (answer);

	  if (answernum == 1)	/* note: recursive here... */
	    {
	      /* if we can't open, try again */
	      if (nextopen (O_WRONLY) < 0)
		continue;
	    }
	  else if ((answernum == 2) )
	  {
	      if (system (formatcmd) != 0)
	      {
		  fprintf (stderr, "Format failed!\n");
		  answernum = 0;/* error, don't autowrite */
	      }
	      else 
	      {
		  fprintf (stderr, "Format successful!\n");
		  /* if we can't open, try again */
		  if (nextopen (O_WRONLY) < 0)
		      continue;      
		  return;
	      }	
	  }
	  else if (strcmp (answer, "quit") == 0)
	      fatal (arspec, "Quiting during a verify");
	}
    }
}


int
writedisk (realwrite)
     int realwrite;
{
  reg char *buf;
  reg int got;
  reg uint len;
  int wrstat;
static int firsttime = 1;  
  /*
   * If we're double buffering wait for any pending writes to
   * complete before starting next writing
   */

  if (fflag)
      outwait ();
  
  /*
   * if we are a floppy open the disk at the last moment
   * call verify w/ an error if the disk can't be opened
   */
  if (Fflag)
  {
      if( ! firsttime  )
	  next( O_WRONLY, "Next disk needed");
      else 
	  while( nextopen (O_WRONLY) < 0)
	  {
	      verifycnt = 0;
	      verify (1);
	  }
      firsttime = 0;	  
  }

  /*
   * If we're double buffering spawn a child to do the
   * actual writing, and return immediately
   */

  if( fflag ) 
  {
      outpid = xfork ("outflush()", DIE);
      if( outpid != 0 )
      {
	  /* what does this do? It seems to be needed to let -s -f work */
	  arleft -= bufidx - buffer;

          nextclos(); /* added by KH to make verify work */

	  return(0);
      }
      else
	  VOID nice (-10);
  }

  do 
  {
      wrstat = 0 ;

      for (buf = buffer; len = bufidx - buf;)
      {
	  if ((got = write (arfd, buf,
			    *arname == '!' ? len : min (len, arbsize))) > 0)
	     {
		 buf += got;
		 if (realwrite)
		     arleft -= got;
	     }
	  else if (fflag)
	     {
		 VOID warn (arspec, got < 0
			    ? syserr ()
			    : "Apparently full");
		 _exit (1);
	     }
	  else if (got < 0)
	     {
		 if(errno==EPIPE)
		     fatal(arspec, syserr());

		 VOID warn (arspec, syserr ());
                 wrstat = 1;
                 break;
	     }
	  else
	     {
		 /* check here if verifying... */
		 next (O_WRONLY, "Apparently full");
		wrstat = 1;
		 break;
	     }
	}
      if (Fflag && verifyflag)
	   {
	       verifycnt = 0;
	       verify ( wrstat );
	   }

      /* Added KH: bug fix for double prompting bug if no -K and -f given */
      /* It took me far too long to find this.  IMO it was a big mistake to
         use arleft, which usually denotes the space left in the buffer, to
         denote the space left on the device in this routine.
         Talking about spaghetti variables.  I'd rather not think about what
         will happen if the write/verify error handling stuff gets used.
      */
      if (Fflag && !verifyflag) arleft = aruntil; 

	       
  } while( wrstat && Fflag && verifyflag );
  
  if( fflag )
    _exit( wrstat );
  else 	      
    return ( wrstat );
}

void
goodbye (status)
     int status;
{
  /* log that we died */
  if (status && (logfile != (FILE *) 0))
    {
      long dietime;
      dietime = time ((time_t *) NULL);

      VOID fprintf (logfile, "%s: the backup has failed (status %d), died at %s",
		    myname, status, ctime (&dietime));

    }

  exit (status);
}

#ifdef MYTEMPNAM
/* author:	Monty Walls
 * written:	4/17/89
 * Copyright:	Copyright (c) 1989 by Monty Walls.
 *		Not derived from licensed software.
 *
 *		Permission to copy and/or distribute granted under the
 *		following conditions:
 *
 *		1). This notice must remain intact.
 *		2). The author is not responsible for the consequences of use
 *			this software, no matter how awful, even if they
 *			arise from defects in it.
 *		3). Altered version must not be represented as being the
 *			original software.
 */

#define MAXPREFIX	5
#define TMPNAME		"tmp"
#ifndef P_tmpdir
#define P_tmpdir	"/tmp"
#define L_tmpnam	14
#endif

extern char *mktemp ();
extern char *strcat ();
extern char *strcpy ();
extern char *getenv ();

char *
tempnam (dir, name)
     char *dir;
     char *name;
{
  char *buf, *tmpdir;

  /*
   * This is kind of like the chicken & the egg.
   * Do we use the users preference or the programmers?
   */
#ifdef USE_ENV_VAR
  if ((tmpdir = getenv ("TMPDIR")) == (char *) NULL)
    {
      if ((tmpdir = dir) == (char *) NULL)
	tmpdir = P_tmpdir;
    }
#else
  if ((tmpdir = dir) == (char *) NULL)
    {
      if ((tmpdir = getenv ("TMPDIR")) == (char *) NULL)
	tmpdir = P_tmpdir;
    }
#endif
  /* now lets check and see if we can work there */
  if (access (tmpdir, R_OK + W_OK + X_OK) < 0)
    return ((char *) NULL);

  if (name == (char *) NULL)
    name = TMPNAME;
  else if (strlen (name) > MAXPREFIX)
    name[5] = '\0';		/* this is according to SYS5 */

  /* the magic value 2 is for '\0' & '/' */
  if ((buf = (char *) malloc (L_tmpnam + strlen (tmpdir) + 2)) == (char *) NULL)
    return ((char *) NULL);

  strcpy (buf, tmpdir);
  strcat (buf, "/");
  strcat (buf, name);
  strcat (buf, ".XXXXXX");

  /* pass our completed pattern to mktemp */
  return (mktemp (buf));
}

#endif /* MYTEMPNAM */
