/* file intrinsics for S-Lang */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */



#include "config.h"

#include <stdio.h>
#include <string.h>


#ifdef __unix__
# include <sys/types.h>
# ifdef HAVE_FCNTL_H
#  include <fcntl.h>
# endif
# ifdef HAVE_SYS_FCNTL_H
#  include <sys/fcntl.h>
# endif
# include <sys/file.h>
#endif

#if defined (__os2__) && defined (__EMX__) 
# include <sys/types.h> /* sys/stat.h requires sys/types.h */
#endif /* __os2__ */

#ifdef msdos
# include <io.h>
# if !defined(__WATCOMC__)
#   include <dir.h>
# else
#   include <direct.h>
# endif
#endif

#if defined(__DECC) && defined(VMS)
# include <unixio.h>
# include <unixlib.h>
#endif

#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef O_RDWR
#ifndef VMS
# include <fcntl.h>
#endif
#endif

#include <errno.h>

#if 0
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/time.h>
#endif

#include "slang.h"
#include "_slang.h"
#include "slfile.h"

SL_File_Table_Type SL_File_Table[SL_MAX_FILES];


#if 0
static int tcp_open (char *service, char *host)
{
   struct servent *sp;
   struct hostent *hp;
   struct sockaddr_in Read_Socket;
   int s;
   
   if (NULL == (sp = getservbyname (service, "tcp")))
     {
	SLang_doerror ("Unknown service");
	return -1;
     }
   
   if (NULL == (hp = gethostbyname(host)))
     {
	SLang_doerror ("Unknown host");
	return -2;
     }
   
   bzero ((char *) &Read_Socket, sizeof (Read_Socket));
   bcopy (hp->h_addr, (char *) &Read_Socket.sin_addr, hp->h_length);
   Read_Socket.sin_family = hp->h_addrtype;
   Read_Socket.sin_port = sp->s_port;
   
   if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0)
     {
	SLang_doerror ("Unable to create socket");
	return -3;
     }
   
   if (connect (s, (char *) &Read_Socket, sizeof (Read_Socket)) < 0)
     {
	SLang_doerror ("Unable to connect.");
	return -4;
     }
   
   return s;
}

#endif



static SL_File_Table_Type *get_file_table_entry(void)
{
   SL_File_Table_Type *t = SL_File_Table, *tmax;
   
   tmax = t + SL_MAX_FILES;
   while (t < tmax)
     {
	if (t->fd == -1) return t;
	t++;
     }
   
   return NULL;
}

static char *remake_string(char *s, unsigned int len)
{
   len++;			       /* for null terminator */
   if (s == NULL)
     {
	s = (char *) SLMALLOC(len);
     }
   else s = (char *) SLREALLOC(s, len);
   
   if (s == NULL) SLang_Error = SL_MALLOC_ERROR;
   return (s);
}

/* add trailing slash to dir */
static void fixup_dir(char *dir)
{
#ifndef VMS
   int n;
 
   if ((n = strlen(dir)) > 1) 
     {
	n--;
#if defined(pc_system) 
      if ( dir[n] != '/' && dir[n] != '\\' ) 
      	strcat(dir, "\\" );
#else
      if (dir[n] != '/' ) 
      	strcat(dir, "/" );
#endif
     }
#endif /* !VMS */
}

static void slget_cwd(void)
{
   char cwd[256];

#ifndef HAVE_GETCWD
   if(!getwd(cwd))
     *cwd = 0;
#else 
#if defined (__EMX__)
   _getcwd2(cwd, 254);		       /* includes drive specifier */
#else
   getcwd(cwd, 254);		       /* djggp includes drive specifier */
#endif
#endif
   
#ifndef VMS
#ifdef __GO32__
   /* You never know about djgpp since it favors unix */
     {
	char ch, *p;
	p = cwd;
	while ((ch = *p) != 0)
	  {
	     if (ch == '/') *p = '\\';
	     p++;
	  }
     }
#endif
   fixup_dir(cwd);
#endif
   SLang_push_string (cwd);
}


static unsigned int file_process_flags(char *mode)
{
   char ch;
   unsigned int flags = 0;
   
   while ((ch = *mode++) != 0)
     {
	switch (ch)
	  {
	   case 'r': flags |= SL_READ; 
	     break;
	   case 'w':
	   case 'a':
	   case 'A':
	     flags |= SL_WRITE; 
	     break;
	   case '+': flags |= SL_WRITE | SL_READ; 
	     break;
	   case 'b': flags |= SL_BINARY; 
	     break;
	     
	   default:
	     return(0);
	  }
     }
   return (flags);
}

/* returns -1 upon failure or returns a handle to file */
static int SLfopen (char *file, char *mode)
{
   FILE *fp;
   SL_File_Table_Type *t;
   unsigned int flags;
   
   if ((t = get_file_table_entry()) == NULL) return (-1);
   if (0 == (flags = file_process_flags(mode))) return (-1);
   
   if ((fp = fopen(file, mode)) == NULL) return (-1);
   t->fp = fp;
   t->fd = fileno(fp);
   t->flags = flags;
#ifdef HAS_SUBPROCESSES
   t->pid = -1;
#endif
   return ((int) (t - SL_File_Table));
}

#if 0
int SLtcp_open(char *service, char *host)
{
   SL_File_Table_Type *t;
   int fd;
   
   if ((t = get_file_table_entry()) == NULL) return (-1);
   
   if ((fd = tcp_open (service, host)) < 0) return -1;
   t->fp = fdopen (fd, "r+");
   t->fd = fd;
   t->flags = SL_SOCKET | SL_READ | SL_WRITE;
   return ((int) (t - SL_File_Table));
}
#endif

/* returns pointer to file entry if it is open and consistent with 
   flags.  Returns NULL otherwise */
static SL_File_Table_Type *pop_fp (unsigned int flags)
{
   int n;
   SL_File_Table_Type *t;
   
   if (SLang_pop_integer (&n)) return NULL;
   
   if ((n < 0) || (n >= SL_MAX_FILES)) return(NULL);
   
   t = SL_File_Table + n;
   if (t->fd == -1) return (NULL);
   if (flags & t->flags) return (t);
   return NULL;
}

   

/* returns 0 upon failure or 1 if successful */
static int SLfclose (void)
{
   int ret = 0;

   SL_File_Table_Type *t = pop_fp (0xFFFF);
   
   if (t == NULL) return 0;
   if (t->fp != NULL)
     {
	if (EOF != fclose (t->fp)) ret = 1;
     }
#ifdef USE_SUBPROCESSES
   else if (t->fd != -1)
     {
	if (close (t->fd) == 0) ret = 1; 
     }
#endif
   t->fp = NULL;  t->fd = -1;

   return (ret);
}

/* returns number of characters read and pushes the string to the stack.  
   If it fails, it returns -1 */
static int SLfgets (void)
{
   char buf[256];
   char *s = NULL, *s1;
   register char *b = buf, *bmax = b + 256;
   register int ch;
   unsigned int len = 0, dlen;
   SL_File_Table_Type *t;
   register FILE *fp;
   

   if (NULL == (t = pop_fp (SL_READ))) return (-1);
   fp = t->fp;
   while (EOF != (ch = getc(fp)))
     {
	if (b == bmax)
	  {
	     if (NULL == (s1 = remake_string(s, len + 256)))
	       {
		  if (s != NULL) SLFREE(s);
		  return(-1);
	       }
	     s = s1;
	     b = buf;
	     strncpy(s + len, b, 256);
	     len += 256;
	  }
	*b++ = (char) ch;
	if (ch == '\n') break;
     }
   
   dlen = (unsigned int) (b - buf);
   if ((dlen == 0) && (s == NULL)) return(0);
   
   
   if (NULL == (s1 = remake_string(s, len + dlen)))
     {
	if (s != NULL) SLFREE(s);
	return(-1);
     }
   
   strncpy(s1 + len, buf, dlen);
   len += dlen;
   *(s1 + len) = 0;   /* null terminate since strncpy may not have */
   SLang_push_malloced_string(s1);
   return((int) len);
}

static int SLfputs (void)
{
   SL_File_Table_Type *t;
   char *s;
   int ret;
   
   if (NULL == (t = pop_fp (SL_WRITE)))
     return -1;

   if (SLpop_string (&s))
     return -1;
   
   if (EOF == fputs(s, t->fp)) ret = 0;
   else ret = 1;
   
   SLFREE (s);
   return ret;
}

static int SLfflush (void)
{
   SL_File_Table_Type *t;
   
   if (NULL == (t = pop_fp (SL_WRITE))) return (-1);
   
   if (EOF == fflush(t->fp)) return (0);
   return (1);
}


static void chdir_cmd (void)
{
   char *s;
   
   _SLerrno_Return_Status = 0;

   if (SLpop_string (&s))
     return;
   
   errno = 0;
   while (-1 == chdir (s))
     {
#ifdef EINTR
	if (errno == EINTR)
	  continue;
#endif
	_SLerrno_set_return_status ();
	break;
     }
   SLFREE (s);
}

static void mkdir_cmd (void)
{   
   char *s;
   int mode;
   
   _SLerrno_Return_Status = 0;
   if (SLang_pop_integer (&mode) || SLpop_string (&s))
     return;
   
   errno = 0;
#if defined (msdos) || (defined (__os2__) && !defined (__EMX__))
# define MKDIR(x,y) mkdir(x)
#else
# define MKDIR mkdir
#endif
   
   while (-1 == MKDIR(s, mode))
     {
#ifdef EINTR
	if (errno == EINTR)
	  continue;
#endif
	_SLerrno_set_return_status ();
	break;
     }
   SLFREE(s);
}


static int slfile_stdin = 0;
static int slfile_stdout = 1;
static int slfile_stderr = 2;

static SLang_Name_Type SLFiles_Name_Table[] = 
{
   MAKE_INTRINSIC(".fopen", SLfopen, INT_TYPE, 2),
   /* Prototype: Integer fopen(String file, String mode);
    * This function opens @file@ in @mode@ and returns a handle to the file.
    * returning handle to file.  The string @mode@ can be any one of the
    * following values:
    * @ "r"    Read only
    * @ "w"    Write only
    * @ "a"    Append
    * @ "r+"   Reading and writing at the beginning of the file.
    * @ "w+"   Reading and writing.  File is created if it does not
    * @           exist; otherwise, it is truncated.
    * @ "a+"   Reading and writing at the end of the file.  It is created
    * @           if it does not exist. 
    * In addition, the mode string can also include the letter @b@ as the
    * last character to indicate that the file is to be opened in binary
    * mode. This function returns -1 if the file could not be opened.  Any
    * other value is a handle to the file. This handle must be used in other
    * file operations.
    * See also: @fclose@, @fgets@, @fputs@
    */
   MAKE_INTRINSIC(".fclose", SLfclose, INT_TYPE, 0),
   /* Prototype: Integer fclose (Integer handle);
    * This function is used to close a file specified by the integer
    * parameter @handle@.  The file must have been previously opened by a call
    * to @fopen@.  It returns:
    * @ 0     The file could not be closed or buffers could not be flushed.
    * @ 1     Success
    * @ -1    The handle was not associated with an open file handle.
    * Note: Most C programmers ignore the return value from the C function @fclose@.
    * In S-Lang, this return value must be dealt with.  The simplist way is to 
    * code it as:
    * @ () = fclose (handle);
    * if one really wants to ignore it.
    * See also: @fopen@, @fflush@
    */
   MAKE_INTRINSIC(".fgets", SLfgets, INT_TYPE, 0),
   /* Prototype: Integer fgets (Integer handle);
    * @fgets@ Reads a line from the open file specified by handle. It
    * returns: -1 if handle not associated with an open file, 0 if at end
    * of the file; otherwise, returns the number of characters read as well
    * as the string of characters read.
    * For example:
    * @  variable buf, fp, n;
    * @  fp = fopen("myfile", "r");
    * @	 if (fp < 0) error("File not open!");
    * @	 n = fgets(fp);
    * @	 if (n == 0) print("End of File!"); 
    * @	 else if (n == -1) print("fgets failed!");
    * @	 else buf = ();  % characters left on the stack.
    * Related Functions: @fopen@, @fclose@, @fputs@, @Sprintf@, @strchop@
    */
     
   MAKE_INTRINSIC(".getcwd", slget_cwd, VOID_TYPE, 0),
   /* Prototype: String getcwd ();
    * This function returns the value of the current working directory.  Under
    * Unix and MSDOS, it includes the trailing slash character,
    */
   MAKE_INTRINSIC(".mkdir", mkdir_cmd, VOID_TYPE, 0),
   /* Prototype: Void mkdir (String dir, Integer mode);
    * The @mkdir@ function creates a directory whose name is given by @dir@.
    * The @mode@ parameter is used to specify the protection of the created
    * directory.  This function does not explicitly return any information
    * regarding whether or not the directory was created.  This information
    * may be obtained from the global variable @_Return_Status@.
    * 
    * Example:
    * @ mkdir ("subdir", 0777);
    * @ if (_Return_Status) error ("mkdir failed.");
    *
    * Note: The @mode@ parameter may not be meaningful on all systems.
    * Related Functions: @getcwd@, @chdir@
    * Related Variables: @_Return_Status@
    */
   MAKE_INTRINSIC(".chdir", chdir_cmd, VOID_TYPE, 0),
   MAKE_INTRINSIC(".fflush", SLfflush, INT_TYPE, 0),
   /* Usage: int fflush(int handle);
      flushes an output stream.
      returns: -1 if handle is not associated with an open stream
                0 if fflush fails (lack of disk space, etc...)
		1 success */
    
   MAKE_INTRINSIC(".fputs", SLfputs, INT_TYPE, 0),
   /* Usage: int fputs(string buf, int handle);
    * Writes a null terminated string (buf) to the open output stream (handle).
    *  returns: -1 if handle is not associated with an open output stream.
    *            1 indicates success. */
   
#ifdef HAS_SUBPROCESSES
   MAKE_INTRINSIC(".create_process", SLcreate_child_process, INT_TYPE, 1),
   /*Prototype: (handle, pid) = create_process (String name);
    * This function returns the pid of the created process as well as 
    * an open file handle for both read and write.  It returns -1 for both
    * parameters upon failure. */
#endif
   
#if 0
   MAKE_INTRINSIC(".tcp_open", SLtcp_open, INT_TYPE, 2),
   /*Prototype: Integer tcp_open (String service, String host);
    */
   MAKE_INTRINSIC(".unix_select", SLinput_pending, INT_TYPE, 2),
   /*Prototype : Integer unix_select(Integer handle, integer secs);
    */
#endif
   MAKE_VARIABLE(".stdin", &slfile_stdin, INT_TYPE, 1),
   /* Prototype: Integer stdin = 0;
    */
   MAKE_VARIABLE(".stdout", &slfile_stdout, INT_TYPE, 1),
   /* Prototype: Integer stdout = 1;
    */
   MAKE_VARIABLE(".stderr", &slfile_stderr, INT_TYPE, 1),
   /* Prototype: Integer stderr = 2;
    */
   MAKE_VARIABLE("._Return_Status", &_SLerrno_Return_Status, INT_TYPE, 1),
   /* Prototype: Integer _Return_Status;
    * This value of this variable is meaningful only for functions which utilize
    * it for returning status information.  If the value of this variable is 
    * zero, the function was successful.  Its value will be non-zero in the 
    * event of a failure.
    */
   SLANG_END_TABLE
};


int init_SLfiles()
{
   int i;
   SL_File_Table_Type *s = SL_File_Table;
   
   for (i = 3; i < SL_MAX_FILES; i++) s[i].fd = -1;
   
   s->fd = fileno(stdin);  s->flags = SL_READ; s->fp = stdin; s++;
   s->fd = fileno(stdout); s->flags = SL_WRITE; s->fp = stdout; s++;
   s->fd = fileno(stderr); s->flags = SL_READ | SL_WRITE; s->fp = stderr;
   
   if (!SLdefine_for_ifdef ("SLFILES")) return 0;
   return SLang_add_table(SLFiles_Name_Table, "_Files");
}

   
   
