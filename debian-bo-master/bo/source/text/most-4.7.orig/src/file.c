#include "config.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <errno.h>

#ifndef toupper
#define toupper(c)	((c)-'a'+'A')
#endif

#ifndef VMS
#  include <fcntl.h>
#  include <sys/types.h>
#  include <sys/stat.h>
#  ifdef HAVE_UNISTD_H
#   include <unistd.h>
#  endif
#else
#  include <types.h>
#  include <stat.h>
#endif /* NOT VMS */

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <slang.h>
#include "jdmacros.h"

#ifndef O_RDONLY
# define O_RDONLY 0
#endif

#include "window.h"
#include "file.h"
#include "most.h"
#include "buffer.h"
#include "display.h"
#include "sysdep.h"

char *Most_File_Ring[MOST_MAX_FILES];
char Most_C_Dir[MAX_PATHLEN];                 /* current working dir */
int Most_Num_Files;


#if !defined(VMS)
static int zopen(char *file, int mode, int *size)
{
   int fd;
   char buf[4], sysbuf[512], zfile[256];
   struct stat st;
   
# ifdef O_BINARY
   mode |= O_BINARY;
# endif
# ifdef __DJGPP__
   Most_Z_Opt = 1;
#endif

   if (Most_Z_Opt) 
     {
	if (stat(file, &st)) return(-1);
	*size = st.st_size;
	return (open(file,mode));
     }
   fd = open(file, O_RDONLY);
   
   if (fd < 0) return fd;
   

   if (4 == read(fd, buf, 4))
     {
	if ((buf[0] == (char) 0x1f)
	    && (buf[1] == (char) 0x8b)
	    && (buf[2] == (char) 0x08)
	    && ((buf[3] == (char) 0x08) || (buf[3] == (char)0x00)))
	  {
	     close (fd);
	     most_flush_message ("Uncompressing file...");
	     
	     sprintf (zfile, "%s-%d", MOST_GUNZIP_TMP_FILE, getpid ());
	     sprintf(sysbuf,"gzip -dc %s >%s", file, zfile);
	     system(sysbuf);
	     
	     if (stat(zfile, &st))
	       {
		  unlink(zfile);
		  return(-1);
	       }
	     *size = st.st_size;
	     fd = open(zfile, O_RDONLY);
	     unlink(zfile);
	     return fd;
	  }
     }
   
   close (fd);
   if (stat(file, &st)) return(-1);
   *size = st.st_size;
   return (open(file,mode));
}
#endif /* VMS */

/* If file[0] == 0, the file represents stdin */
static int insert_file(char *file)
{
   int size = 0, fd;
   /* int mode; */
#ifdef VMS
   struct stat st;
   unsigned recsz = 512;
   extern int stat(char *, struct stat *);
#endif

    if (file[0] == '\0')        /* assume stdin */
      {
	 fd = 0;
	 strcpy (Most_Buf->file, "*stdin*");
      }
    else
      {
#ifdef VMS 
	 if (stat(file, &st)) return(-1);
	 /* mode = st.st_mode & S_IFMT; */
	 size = st.st_size;
         recsz = st.st_fab_mrs;
	 if (recsz <= 255) recsz = 255;
          /* VMS share options (shr=put) suggested by Henk D. Davids <hdavids@mswe.dnet.ms.philips.nl> */
          /* VMS share options (shr=upi,get,put) suggested by Mark Pizzolato <mark@infocomm.com> */
          fd = open(file,O_RDONLY,"ctx=rec","mbf=8","mbc=16","rop=RAH","shr=upi,get,put");
          /* if (fd < 0) fd = open(file,O_RDONLY); */
#else
	 fd = zopen(file, O_RDONLY, &size);
#endif          
      }
   
   if (fd < 0) return(-1);
   
   if (!fd || (size <= 0)) size = 16384;
#ifdef VMS
   Most_Buf->rec = recsz;
#endif
   Most_Buf->fd = fd;
   
   /* This will fail on really large files. */
   Most_Eob = Most_Beg = Most_Buf->beg = (unsigned char *) MOSTMALLOC(size);
   Most_Buf->size = size;
   
   return most_read_file_dsc (1);
}

static void update_buffer_windows (Most_Buffer_Type *b)
{
   Most_Window_Type *w;
   
   if (NULL == (w = Most_Win))
     return;
   do
     {
	if (w->buf == b) w->dirty_flag = 1;
	w = w->next;
     }
   while (w != Most_Win);
}

   
static int first_time_hack = 0;	       /* tru if reading file for first time */
/* if read something, return non zero (1) */
int most_read_file_dsc (int many)
{
   int fd = Most_Buf->fd, n = 0, i;
   int dsize, size, passes = 0;
   unsigned char *pos;
#ifdef VMS
   int recsz = Most_Buf->rec;
#endif
   
   if (fd < 0) return 0;
   
   if (fd == 0) dsize = 4 * 1024; else dsize = 16 * 1024;
   
   while (many--)
     {
	if (passes++ == 1)
	  {
	     most_flush_message ("Reading...");
	  }
	
	     
	if ((fd == 0) && (Most_Beg != Most_Eob))
	  {
	     size = (Most_Eob - Most_Beg) + dsize;
	     
	     if (Most_Buf->size > size) pos = Most_Beg; 
	     else 
	       {
		  size = Most_Buf->size + 16 * 1024;
		  pos = (unsigned char *) MOSTREALLOC(Most_Beg, (unsigned) size);
		  Most_Buf->size = size;
	       }
	     
	     Most_C_Pos = pos + (Most_C_Pos - Most_Beg);
#if 0
	     if ((Most_Win != NULL) && (Most_Win->buf == Most_Buf))
	       {
		  Most_Win->beg_pos = pos + (Most_Win->beg_pos - Most_Beg);
		  Most_Win->curs_pos = pos + (Most_Win->curs_pos - Most_Beg);
	       }
#endif
	     Most_Eob = pos + (int) (Most_Eob - Most_Beg);
	     Most_Beg = pos;
	  }
	else if (dsize + Most_Eob > Most_Beg + Most_Buf->size) 
	  dsize = (Most_Beg + Most_Buf->size) - Most_Eob;
	
	pos = Most_Eob;
	n = 0;
	
	while (1)
	  {
#ifdef VMS
	     i = read (fd, (char *) pos, recsz);
#else
	     i = read (fd, (char *) pos, dsize - n);
#endif
	     if (i == -1)
	       {
#ifdef EINTR
		  if (errno == EINTR) continue;
#endif
		  break;
	       }
	     
	     n += i;
	     pos += i;
	     if ((n >= dsize) || (i == 0))
	       break;
	  }
	
	if (n != 0) n = 1;
	Most_Eob = Most_Buf->end = pos;
	Most_Buf->beg = Most_Beg;
	if ((i < 0) || ((i == 0) && ((n < dsize) 
				     || ((fd != 0) && (n == dsize)))))
	  {
	     if (fd != 0) close(fd);
	     Most_Buf->fd = -1;
	     break;
	  }
     }
   
   if (first_time_hack)
     {
	/* This has to go here so that count_lines will work properly */
	if (!Most_B_Opt && !Most_K_Opt)
	for (pos = Most_Beg; (pos < (Most_Beg + 32)) && (pos < Most_Eob); pos++)
	  {
	     if (0 == (*pos & 0x7F)) Most_B_Opt = 1;
	  }
     }

   Most_Num_Lines = most_count_lines (Most_Beg, Most_Eob);
   update_buffer_windows (Most_Buf);

   if (passes > 1)
     {
	most_message("reading...done", 0);
	most_put_message();
	most_message(Most_Global_Msg, 0);
     }
   
   return n;
}

/* This routines makes sure line n is read in. */
void most_read_to_line(int n)
{
   int dn;
   if (Most_Buf->fd == -1) return;
   n = n + 2 * SLtt_Screen_Rows;
   dn = n - Most_Num_Lines;
   if (dn < 0) return;
   
   /* dn is the number of lines to read.
      Assume average of 40 bytes/line, then 40 * dn need read and we are 
      reading 16K at a time.  Thus, */
   dn = (40 * dn)/(16000);
   if (dn == 0) dn = 1;
   while ((Most_Buf->fd != -1) && (n >= Most_Num_Lines)) most_read_file_dsc(dn);
}

   
int most_find_file(char *file)
{
   Most_Buffer_Type *new_buf;
   int n;
   char msg[1024], *msgp;
   int ret = 0; 
    
   new_buf = most_create_buffer(file);
   (void) most_switch_to_buffer(new_buf);
   
   first_time_hack = 1;
   if (insert_file(file) < 0)
     {
	sprintf (msg, "%s failed to open.", file);
	n = strlen (msg);
	msgp = (char *) MOSTMALLOC((unsigned int) (n + 1));
	strcpy (msgp, msg);
	Most_Buf->beg = (unsigned char *) msgp;
	Most_Buf->end = Most_Buf->beg + n;
	Most_Buf->fd = -1;
	Most_Num_Lines = 1;
	ret = -1;
     }
   first_time_hack = 0;
   
   Most_Beg = Most_Buf->beg;
   Most_Eob = Most_Buf->end;
   Most_C_Pos = Most_Beg;
   Most_C_Line = 1;
   Most_Column = 1;
   return ret;
}

/* if the file is visible in a window, move to the window and return 1
   else return 0 */
Most_Window_Type *most_file_visible(char *file)
{
    Most_Window_Type *w;
    w = Most_Win;
    Most_Win = Most_Top_Win;
    do
      {
	 if (!strcmp(Most_Win->buf->file,file))
            {
                most_set_window(Most_Win);
                return(Most_Win);
            }
          Most_Win = Most_Win->next;
      }
    while (Most_Win != Most_Top_Win);
    Most_Win = w;
    return(NULL);
}

void most_reread_file (void)
{
   char file[MAX_PATHLEN];
   int line = Most_C_Line;
   
   if (-1 == access(Most_Buf->file, 0))        /* does it exist? */
     {
	most_message("File not found.",1);
	return;
     }
   
   most_one_window ();
   
   strcpy (file, Most_Buf->file);
   most_free_window_buffer ();
   (void) most_find_file (file);
   most_goto_line (line);
   most_window_buffer ();
}

static int find_file_in_window(char *file)
{
   if (NULL != most_file_visible(file)) 
     {
	most_message ("File is already visible.", 1);
	return -2;
     }
   
   if (-1 == access(file, 0))        /* does it exist? */
     {
	most_message("File not found.",1);
	return -1;
     }
   most_free_window_buffer();
   (void) most_find_file(file);

   most_window_buffer();
   
   most_redraw_window();
   most_update_status();
   return 0;
}


static void format_next_file (char *mbuf, int j, char *file)
{
   int len, max_len;
   
   len = strlen (file);
   max_len = SLtt_Screen_Cols - 25;
   if (len > max_len)
     {
	sprintf(mbuf, "Next File (%d): ...%s", j, file + (len - max_len));
     }
   else	sprintf(mbuf, "Next File (%d): %s", j, file);
}


void most_do_next_file(int *j)
{
   char mbuf[256], ch, *curr_file;
    
   most_select_minibuffer();

   if (*j >= Most_Num_Files) *j = 0;
   curr_file = Most_File_Ring[*j];
   
   while (1)
     {
	format_next_file (mbuf, *j, curr_file);
	
	SLsmg_write_string (mbuf);
	SLsmg_erase_eol ();
	SLsmg_gotorc (SLtt_Screen_Rows - 1, 0);
   
	SLsmg_refresh ();
   
	 ch = most_getkey();
	 if (ch != '\033') break;
	 if (ch = most_getkey(), (ch != 'O') && (ch != '[')) continue;
	 if (ch = most_getkey(), (ch != 'A') && (ch != 'B')) continue;
          
	 if (ch == 'B')
            {
	       if (*j == 0) *j = Most_Num_Files;
	       (*j)--;
            }
	 else  /* ch == 'A' */
	   {
	      (*j)++;
	      if (*j == Most_Num_Files) *j = 0;
	   }
	 curr_file = Most_File_Ring[*j];
      }
   
   most_exit_minibuffer();
   
   (*j)++;
   if ((ch == 'Q') || (ch == 'q')) 
     {
	most_exit_most ();
     }
   else find_file_in_window(curr_file);
}

/* extracts directory from file string, returns false if no dir */
int most_head(char *file, char *dir)
{
   int n;
   
   if (file == NULL) return 0;
   
    (void) strcpy(dir,file);
    n = strlen(file) - 1;
#ifdef VMS    
    while((n > -1) && (file[n] != ']') && (file[n] != ':')) n--;
#else    
    while((n > -1) && file[n] != '/') n--;
#endif
    n++;
    dir[n] = '\0';
    return(n);
}

/* returns a pointer to the tail of file */
static int tail(char *filed, char **filep)
{
    int n;
    n = strlen(filed) - 1;
#ifdef VMS    
    while((n > -1) && ((filed[n] != ']') || (filed[n] != ':'))) n--;
#else    
    while((n > -1) && (filed[n] != '/')) n--;
#endif
    n++;
    *filep = (filed + n);
    return(n);
}

/* assume path is big enough to hold new expanded version */
static int expand_path(char *path)
{
#ifndef VMS
    int n;
#endif
    /* really cheat here-- let system do it.  The path must exist!! */
    if (chdir(path))
      {
          most_message(path,1);
          return(0);
      }
    else
      {
          most_get_cdir(path);
          chdir(Most_C_Dir);
#ifndef VMS
          n = strlen(path);
          if (path[n-1] == '/') return(1);
          path[n++] = '/'; path[n] = 0;
#endif
      }
    return(1);
}


#if 0
static void most_cd (void)
{
    char tmp_dir[MAX_PATHLEN];
    int n;
    
   if (Most_Secure_Mode)
     {
	most_message ("CD disabled in secure mode.", 1);
	return;
     }
   
    strcpy(tmp_dir,Most_C_Dir);
    if (most_read_from_minibuffer("cd ",Most_C_Dir) == -1) return;
    if (!chdir(Most_C_Dir))
      {
          most_get_cdir(Most_C_Dir);         /* expands ../ etc... */
          n = strlen(Most_C_Dir);
#ifndef VMS
          if (Most_C_Dir[n-1] == '/') return;
          Most_C_Dir[n++] = '/'; Most_C_Dir[n] = 0;
#endif          
          return;
      }
    strcpy(Most_C_Dir,tmp_dir);
    chdir(Most_C_Dir);
    most_message("Unable to change directory.",1);
}
#endif

void most_user_get_file()
{
   char path[MAX_PATHLEN], file[MAX_PATHLEN], *name;
#ifdef VMS
   int i;
#endif

   if (Most_Secure_Mode)
     {
	most_message ("Getting a file is not permitted in secure mode.", 1);
	return;
     }
   
   if (!most_head(Most_Win->buf->file,file))
      strcpy(file,Most_C_Dir);

   if (most_read_from_minibuffer("Find File: ",file) == -1) return;

    if (most_head(file,path))
      {
          expand_path(path);
          tail(file,&name);
          strcat(path,name);
          name = path;
      }
    else name = file;
#ifdef VMS
   for (i=0; i < strlen(name); i++) name[i] = toupper(name[i]);
#endif

   if (find_file_in_window(name) < 0) return;

    /*
    **  add to file ring if successful
    */
    if ((NULL != most_file_visible(name)) 
	&& (Most_Num_Files < MOST_MAX_FILES))
     {
	Most_File_Ring[Most_Num_Files] = (char*) MOSTMALLOC(strlen(name) + 1);
	strcpy(Most_File_Ring[Most_Num_Files++],name);     
     }
}


void most_get_cdir(char *dir)
{
#ifdef sequent
   getwd(dir);
#else
# ifdef HAVE_GETCWD
   getcwd(dir, MAX_PATHLEN);
# else
   getwd(dir);
# endif
#endif
   
#ifndef VMS
     {
	int i;
	i = strlen(dir); 
	if (i && (dir[i - 1]  != '/')) dir[i++] = '/';
	dir[i] = 0;
     }
#endif
}
