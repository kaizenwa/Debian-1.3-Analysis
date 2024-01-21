/*
    SYSDEP.H
    Declare operating system-dependent functions required by Yorick,
    and define operating system-dependent macros.

    $Id: sysdep.h,v 1.1 1993/08/27 18:32:09 munro Exp $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#ifndef SYSDEP_H
#define SYSDEP_H
/*--------------------------------------------------------------------------*/

/* If the header file setjmp.h cannot be found, you must find the
   correct definition of the data type jmp_buf, which is normally
   an array capable of holding enough state information to make the
   longjmp.  Put the declaration here.  */
/* #define JMP_BUF_DECLARATION typedef int jmp_buf[???]; */

/* If the system math library does not have hypotenuse c= hypot(a,b),
   define the following and nonc.c will create a reasonable facsimile:  */
/* #define NO_HYPOT */

/* The following characters delimit directories in a pathname, or
   pathnames in a list of pathnames.  */
#define DIR_SEP '/'
#define DIR_SEP_S "/"
#define PATH_SEP ":"

#ifdef DOS_FILENAMES
#undef DIR_SEP
#define DIR_SEP '\\'
#undef DIR_SEP_S
#define DIR_SEP_S "\\"
#endif

#ifdef MAC_FILENAMES
#undef DIR_SEP
#define DIR_SEP ':'
#undef DIR_SEP_S
#define DIR_SEP_S ":"
#define PATH_SEP ";"
#define getenv GetStringResource
#endif

/* The following is used as the pathname separator delimiter in calls
   to strtok to separate a list of directory names.  */
#define PATH_SEP_DELIMIT PATH_SEP " \t"

/* BUFFERING
   Since Yorick has its own caching scheme which involves potentially
   variable buffer sizes (controlled by IOStream->blockSize), my first
   thought was to use the ANSI setbuf function to "unbuffered" I/O.
   This might seem like a good idea, but it turns out that both on UNIX
   and MacIntosh (and probably DOS), "unbuffered" means to read the
   stream one byte at a time, NOT to use the space passed to fread/fwrite
   as the buffer, as I originally assumed.
   On UNIX, the only solution is to use the raw read/write functions,
   which simply use the space passed to them as the buffer.  On the
   MacIntosh under Think C, read/write are implemented as wrappers for
   fread/fwrite, so you may as well define NO_UNIX_READ there; I don't
   know what sort of performance hit you will take for not setting the
   buffer size.  The code using this is in binio.c (search for BUFFERING,
   READ_BYTES, and WRITE_BYTES).  FLUSH_BYTES is in cache.c  */
#undef READ_BYTES
#undef WRITE_BYTES
#undef FLUSH_BYTES
#undef SEEK_BYTES
#undef TELL_BYTES

#ifndef NO_UNIX_READ
#define READ_BYTES(file, buffer, n) read(fileno(file), buffer, n)
#define WRITE_BYTES(file, buffer, n) write(fileno(file), buffer, n)
#define FLUSH_BYTES(file)
#define SEEK_BYTES(file, off, w) (lseek(fileno(file), off, w)==-1L)
#define TELL_BYTES(file) lseek(fileno(file), 0L, SEEK_CUR)
#else
/* Could also define READ_BYTES to some other non-UNIX alternative.  */
#define READ_BYTES(file, buffer, n) fread(buffer, sizeof(char), n, file)
#define WRITE_BYTES(file, buffer, n) fwrite(buffer, sizeof(char), n, file)
#define FLUSH_BYTES(file) fflush(file)
#define SEEK_BYTES(file, off, w) fseek(file, off, w)
#define TELL_BYTES(file) ftell(file)
#endif

/* To use getrusage() and gettimeofday() instead of times(), try this: */
/* #define BSD_TIMER */
/* If neither works, a crude timer can be built using only ANSI standard
   library routines with the following:  */
/* #define CLOCK_FUNCTION clock
   #define TICKS_PER_SEC CLOCKS_PER_SEC    */
/* On a MacIntosh, the following works better:  */
/* #include <pascal.h>
   #include <Events.h>
   #define CLOCK_FUNCTION TickCount
   #define TICKS_PER_SEC 60           */
/* You may be able to do better by modifying Ytimer in sysdep.c.  */

/* If the getpwnam function is not defined in <pwd.h>, set this */
/* #define NO_GETPWNAM */

/* If the readlink routine does not exist, set this */
/* #define NO_SOFT_LINKS */

/* sysdep.c defines the following functions:
      int YGetLaunchDir(char *argv0)
         - tracks down the directory in which Yorick's executable resides.
	   Include files necessary for startup are in subdirectories of
	   this directory.
      int YstdinNB(int noWait)
         - If you are running without Gist, this routine is necessary;
	   with Gist, it is never called.  YstdinNB(0) pauses until
	   input arrives on stdin, then returns 0.  YstdinNB(1) returns
	   immediately (NB is for "non-blocking") no matter what; its value
	   is 0 if and only if input is present on stdin, so that fgets
	   will not block.
      void Ytimer(double *cpu, double *sys, double *wall)
         - returns elapsed cpu, system, and wall clock times in seconds
	   (from an arbitrary origin -- used for timing differences only)
      char *Ytimestamp(void)
         - returns result of ANSI ctime, with trailing "\n" stripped
      void SetSignals(int flags)
         - sets up signal handling for keyboard interrupt (^C), floating
	   point interrupts, segmentation and illegal address violations,
	   and alignment errors
      YSetCWD, YGetHome, YPATHstrtok, YExpandName, YNameTail, YNameHead,
      YNameToHead, YIsAbsolute, YIsDotRelative are described in yio.h,
      as is Ygetenv
 */

/*--------------------------------------------------------------------------*/
#endif
