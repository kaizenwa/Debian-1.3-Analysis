/*
 * File: rsys.r
 *  Contents: [flushrec], [getrec], getstrg, host, longread, [putrec], putstr
 */

#ifdef RecordIO
/*
 * flushrec - force buffered output to be written with a record break.
 *  Applies only to files with mode "s".
 */

novalue flushrec(fd)
FILE *fd;
{
#if SASC
   afwrite("", 1, 0, fd);
#endif					/* SASC */
}

/*
 * getrec - read a record into buf from file fd. At most maxi characters
 *  are read.  getrec returns the length of the record.
 *  Returns -1 if EOF and -2 if length was limited by
 *  maxi. [[ Needs ferror() check. ]]
 *  This function is meaningful only for files opened with mode "s".
 */

int getrec(buf, maxi, fd)
register char *buf;
int maxi;
FILE *fd;
   {
#ifdef SASC
   register int l;

   l = afreadh(buf, 1, maxi+1, fd);     /* read record or maxi+1 chars */
   if (l == 0) return -1;
   if (l <= maxi) return l;
   ungetc(buf[maxi], fd);               /* if record not used up, push
                                           back last char read */
   return -2;
#endif					/* SASC */
   }
#endif					/* RecordIO */

/*
 * getstrg - read a line into buf from file fd.  At most maxi characters
 *  are read.  getstrg returns the length of the line, not counting
 *  the newline.  Returns -1 if EOF and -2 if length was limited by
 *  maxi. [[ Needs ferror() check. ]]
 */

int getstrg(buf, maxi, fd)
register char *buf;
int maxi;
FILE *fd;
   {
   register int c, l;


#if AMIGA
#if LATTICE
   /* This code is special for Lattice 4.0.  It was different for
    *  Lattice 3.10 and probably won't work for other C compilers.
    */
   extern struct UFB _ufbs[];

   if (IsInteractive(_ufbs[fileno(fd)].ufbfh))
      return read(fileno(fd),buf,maxi);
#endif					/* LATTICE */
#endif					/* AMIGA */

#ifdef XWindows
   if (isatty(fileno(fd))) wflushall();
#endif					/* XWindows */

   l = 0;
   while (1) {
#ifdef Graphics
      /* insert non-blocking read/code to service windows here */
#endif					/* Graphics */
      if ((c = fgetc(fd)) == '\n') break;
      if (c == EOF)
	 if (l > 0) return l;
	 else return -1;
      if (++l > maxi) {
	 ungetc(c, fd);
	 return -2;
	 }
      *buf++ = c;
      }
   return l;
   }

/*
 * iconhost - return some sort of host name into the buffer pointed at
 *  by hostname.  This code accommodates several different host name
 *  fetching schemes.
 */
novalue iconhost(hostname)
char *hostname;
   {

#ifdef WhoHost
   /*
    * The host name is in /usr/include/whoami.h. (V7, 4.[01]bsd)
    */
   whohost(hostname);
#endif					/* WhoHost */

#ifdef UtsName
   {
   /*
    * Use the uname system call.  (System III & V)
    */
   struct utsname utsn;
   uname(&utsn);
   strcpy(hostname,utsn.nodename);
   }
#endif					/* UtsName */

#ifdef GetHost
   /*
    * Use the gethostname system call.  (4.2bsd)
    */
   gethostname(hostname,MaxCvtLen);
#endif					/* GetHost */

#if VMS
   /*
    * VMS has its own special logic.
    */
   char *h;
   if (!(h = getenv("ICON_HOST")) && !(h = getenv("SYS$NODE")))
      h = "VAX/VMS";
   strcpy(hostname,h);
#endif					/* VMS */

#ifdef HostStr
   /*
    * The string constant HostStr contains the host name.
    */
   strcpy(hostname,HostStr);
#endif					/* HostStr */

   }

#ifdef WhoHost
#define HdrFile "/usr/include/whoami.h"

/*
 * whohost - look for a line of the form
 *  #define sysname "name"
 * in HdrFile and return the name.
 */
novalue whohost(hostname)
char *hostname;
   {
   char buf[BUFSIZ];
   FILE *fd;

   fd = fopen(HdrFile, ReadText);
   if (fd == NULL) {
      sprintf(buf, "Cannot open %s, no value for &host\n", HdrFile);
      syserr(buf);
   }

   for (;;) {   /* each line in the file */
      if (fgets(buf, sizeof buf, fd) == NULL) {
         sprintf(buf, "No #define for sysname in %s, no value for &host\n",
            HdrFile);
         syserr(buf);
      }
      if (sscanf(buf,"#define sysname \"%[^\"]\"", hostname) == 1) {
         fclose(fd);
         return;
      }
   }
   }
#endif					/* WhoHost */

/*
 * Read a long string in shorter parts. (Standard read may not handle long
 *  strings.)
 */
word longread(s,width,len,fd)
FILE *fd;
int width;
char *s;
long len;
{
   tended char *ts = s;
   long tally = 0;
   long n = 0;
 
#ifdef XWindows
   if (isatty(fileno(fd))) wflushall();
#endif					/* XWindows */

   while (len > 0) {
      n = fread(ts, width, (int)((len < MaxIn) ? len : MaxIn), fd);
      if (n <= 0)
         return tally;
      tally += n;
      ts += n;
      len -= n;
      }  
   return tally;
   }

#ifdef RecordIO
/*
 * Write string referenced by descriptor d, avoiding a record break.
 *  Applies only to files openend with mode "s".
 */

int putrec(f, d)
register FILE *f;
dptr d;
   {
#if SASC
   register char *s;
   register word l;

   l = StrLen(*d);
   if (l == 0)
      return Succeeded;
   s = StrLoc(*d);

   if (afwriteh(s,1,l,f) < l)
      return Failed;
   else
      return Succeeded;
   /*
    * Note:  Because RecordIO depends on SASC, and because SASC
    *  uses its own malloc rather than the Icon malloc, file usage
    *  cannot cause a garbage collection.  This may require
    *  reevaluation if RecordIO is supported for any other compiler.
    */
#endif					/* SASC */
   }
#endif					/* RecordIO */

/*
 * Print string referenced by descriptor d. Note, d must not move during
 *   a garbage collection.
 */

int putstr(f, d)
register FILE *f;
dptr d;
   {
   register char *s;
   register word l;
   register int  i;

   l = StrLen(*d);
   if (l == 0)
      return  Succeeded;
   s = StrLoc(*d);

#ifdef MSWindows
#ifdef ConsoleWindow
   if ((f == stdout && !(ConsoleFlags & StdOutRedirect)) ||
	(f == stderr && !(ConsoleFlags & StdErrRedirect))) {
      if (ConsoleBinding == NULL)
         ConsoleBinding = OpenConsole();
#if BORLAND_286
      goto Lab1;
#else
      wputstr((wbp)ConsoleBinding, s, l);
#endif
      return Succeeded;
      }
Lab1:
#endif					/* ConsoleWindow */
#endif					/* MSWindows */
#ifdef PresentationManager
   if (ConsoleFlags & OutputToBuf) {
      /* check for overflow */
      if (MaxReadStr * 4 - ((int)ConsoleStringBufPtr - (int)ConsoleStringBuf) < l + 1)
	 return Failed;
      /* big enough */
      memcpy(ConsoleStringBufPtr, s, l);
      ConsoleStringBufPtr += l;
      *ConsoleStringBufPtr = '\0';
      } /* End of if - push to buffer */
   else if ((f == stdout && !(ConsoleFlags & StdOutRedirect)) ||
	    (f == stderr && !(ConsoleFlags & StdErrRedirect)))
      wputstr((wbinding *)PMOpenConsole(), s, l);
   return Succeeded;
#endif					/* PresentationManager */
#if VMS
   /*
    * This is to get around a bug in VMS C's fwrite routine.
    */
   for (i = 0; i < l; i++)
      if (putc(s[i], f) == EOF)
         break;
   if (i == l)
      return Succeeded;
   else
      return Failed;
#else					/* VMS */
   if (longwrite(s,l,f) < 0)
      return Failed;
   else
      return Succeeded;
#endif					/* VMS */
   }

/*
 * idelay(n) - delay for n milliseconds
 */
idelay(n)
int n;
   {

/*
 * The following code is operating-system dependent [@fsys.01].
 */
#if OS2
#if OS2_32
   DosSleep(n);
   return Succeeded;
#else					/* OS2_32 */
   return Failed;
#endif					/* OS2_32 */
#endif					/* OS2 */

#if VMS
   delay_vms(n);
   return Succeeded;
#endif					/* VMS */

#if SASC 
   sleepd(0.001*n);
   return Succeeded;
#endif                                   /* SASC */

#if UNIX
#ifndef NoSelect
   struct timeval t;
   t.tv_sec = n / 1000;
   t.tv_usec = (n % 1000) * 1000;
   select(1, FD_NULL, FD_NULL, FD_NULL, &t);
   return Succeeded;
#else					/* NoSelect */
   return Failed;
#endif					/* NoSelect */
#endif					/* UNIX */

#if MSDOS
#if NT
   Sleep(n);
   return Succeeded;
#else					/* NT */
#if BORLAND_286
   /* evil busy wait */
    clock_t start = clock();   
    while ((double)((clock() - start) / CLK_TCK) / 1000 < n);
    return Succeeded;
#else					/* BORLAND_286 */
   return Failed;
#endif					/* BORLAND_286 */
#endif					/* NT */
#endif					/* MSDOS */

#if MACINTOSH
   void MacDelay(int n);
   MacDelay(n);
   return Succeeded;
#endif					/* MACINTOSH */

#if SCCX_MX
   msleep(n);
   return Succeeded;
#endif					/* MCCX_MX */

#if PORT || AMIGA || ARM || ATARI_ST || MVS || VM
   return Failed;
#endif

   /*
    * End of operating-system dependent code.
    */
   }
