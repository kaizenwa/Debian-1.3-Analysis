/* -*- mode: C; mode: fold; -*- */
/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include "config.h"
#include "jed-feat.h"

/*{{{ system include files */

#include <stdio.h>
#include <slang.h>

#include "jdmacros.h"

#include <string.h>
#include <limits.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef __unix__
# include <sys/types.h>
# include <sys/stat.h>
# include <sys/file.h>
#endif

#ifdef REAL_UNIX_SYSTEM
# include <utime.h>
#endif

#ifdef HAVE_SYS_FCNTL_H
# include <sys/fcntl.h>
#endif

#ifdef __os2__
#include <fcntl.h>
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>
 
 typedef struct HOLDFEA *PHOLDFEA;
 PHOLDFEA QueryEAs (char *name);
 int WriteEAs (char *name, PHOLDFEA pHoldFEA);
#endif

#ifdef msdos
#include <fcntl.h>
#include <io.h>
#include <sys/stat.h>
#endif
#ifdef __GO32__
#include <fcntl.h>
#endif

#if defined(__DECC) && defined(VMS)
# include <unixio.h>
# include <unixlib.h>
#endif

#include <errno.h>

#ifdef __WIN32__
# include <windows.h>
# define sleep Sleep
#endif

/* Was anything missed? */
#ifndef O_RDONLY
#ifdef VMS
#include <file.h>
#else
#include <fcntl.h>
#endif
#endif


/*}}}*/
/*{{{ local inclue files */

#include "buffer.h"
#include "file.h"
#include "misc.h"
#include "sysdep.h"
#include "paste.h"
#include "ins.h"
#include "ledit.h"

/*}}}*/

#if defined (msdos) || defined (__os2_16__)
#define MAX_LINE_LEN 1024
#else
#define MAX_LINE_LEN 64 * 1024
#endif

int Jed_Backup_By_Copying = 0;

#ifdef VMS
/*{{{ vms_stupid_open */

static int vms_max_rec_size;
#include <stat.h>
#include <rms.h>
static int VMS_write_rfm_fixed;
int vms_stupid_open(char *file)
{
   struct stat s;
   char rat_buf[80], rfm_buf[80], mrs_buf[40], *rfm = "var";
   unsigned short mode = 0, c;
   int ret;

   VMS_write_rfm_fixed = 0;

   strcpy(rfm_buf, "rfm=");
   
   
   if (0 == stat(file, &s))
     {
	strcpy(rat_buf, "rat");
	c = s.st_fab_rat;
	if (c & FAB$M_FTN)  strcat(rat_buf, ",ftn");
	if (c & FAB$M_CR)  strcat(rat_buf, ",cr");
	if (c & FAB$M_PRN)  strcat(rat_buf, ",prn");
	if (c & FAB$M_BLK)  strcat(rat_buf, ",blk");
	if (rat_buf[3] != 0) rat_buf[3] = '='; else *rat_buf = 0;

	c = s.st_fab_rfm;
	switch(c)
	  {
	   case FAB$C_UDF:  rfm = "udf"; break;
	   case FAB$C_FIX:  
	     rfm = "fix"; 
	     if (s.st_fab_rat & (FAB$M_CR | FAB$M_CR))
	       VMS_write_rfm_fixed = 1;
	     break;

	   case FAB$C_VAR:  rfm = "var"; break;
	   case FAB$C_VFC:  rfm = "vfc"; break;
	   case FAB$C_STM:  rfm = "stm"; break;
	   case FAB$C_STMLF:  rfm = "stmlf"; break;
	   case FAB$C_STMCR:  rfm = "stmcr"; break;
	  }
	mode = s.st_mode & 0777;
     }
   else strcpy (rat_buf, "rat=cr");
   
   strcat(rfm_buf, rfm);
   
   if (vms_max_rec_size <= 0) vms_max_rec_size = 255;
   sprintf(mrs_buf, "mrs=%d", vms_max_rec_size);
      
   if (*rfm == 's')		       /* stream */
     {
	ret = creat(file, mode, rfm_buf);
     }
   else
     {
	if (*rat_buf) ret = creat(file, mode, rfm_buf, mrs_buf, rat_buf);
	else ret = creat(file, mode, rfm_buf, mrs_buf);
     }
   if (ret >= 0) chmod(file, mode);
   return ret;
}

/*}}}*/
#endif

int Require_Final_Newline = 0;

/* 0 = read, 1 = write , 2 = append... */
static int sys_open(char *file, int acces) /*{{{*/
{
   int fp = -1;
   int flags;
   unsigned int mode;
#ifdef VMS
   char *p, neew[JED_MAX_PATH_LEN];
#endif
   
#ifdef O_BINARY
	mode = O_BINARY;
#else
	mode = 0;
#endif

   flags = file_status(file);
   if ((flags < 0) || (flags > 1)) return(fp);
   
   /* on VMS I cheat since I do not want to deal with RMS at this point */
#ifdef VMS
   VMS_write_rfm_fixed = 0;
   strcpy(neew, file);
   p = neew; while (*p) if (*p == ';') *p = 0; else p++;
   
   if (acces == 0) fp = open(file, O_RDONLY, "ctx=rec","mbf=8","mbc=32","rop=RAH","shr=upi,get,put");
   else if (acces == 1)
     {
	fp = vms_stupid_open(neew);
     }
   
   else if (acces == 2) fp = open(file, O_WRONLY | O_APPEND | O_CREAT | mode);
#else
   
   switch(acces)
     {
      case 0: flags =  O_RDONLY; 
	break;
      case 1: flags =  O_WRONLY | O_CREAT | O_TRUNC;
	break;
      case 2: flags =  O_WRONLY | O_CREAT | O_APPEND; 
	break;
      default: return(fp);
     }
   
   flags |= mode;
   
#if defined (msdos) || defined (__os2__)
   fp = open(file, flags, S_IREAD | S_IWRITE); 
#else
   fp = open(file, flags, 0666); 
#endif
#endif /* VMS */
   return(fp);
}

/*}}}*/

char *file_type(char *file) /*{{{*/
{
   char *p, *psave;
   if ((file == (char *) NULL) || (*file == 0)) return( (char *) NULL);

   file = extract_file(file);
   p = file; while (*p != 0) p++;
   psave = p;
   while((p != file) && (*p != '.')) p--;
   if (*p == '.') p++;
   if (p == file) return(psave); else return(p);
}

/*}}}*/
void set_file_modes () /*{{{*/
{
   char *type;

   if (CBuf == NULL) return;
   CBuf->c_time = sys_time();
   if (CBuf->file[0])
     {
#if defined(msdos) && !defined(W_OK)
#define W_OK 2
#define F_OK 0
#endif

#ifdef W_OK
#ifdef __GO32__
# define access i386_access
#endif
	char name[512];
#ifndef VMS
	  {
	     /* COmment:  I need a more generic call to contruct a directory
	      * name.  site.sl already does this properly.
	      */
	     int n;
	     strcpy (name, CBuf->dir);
	     n = strlen (name);
	     if (n >
#ifdef pcsystem
		 3		       /* allow C:/file */
#else
		 1		       /* allow /file */
#endif
		 )
	       {
		  name[n - 1] = 0;     /* knock off slash */
		  if (!access (name, F_OK) && (access (name, W_OK)))
		    CBuf->flags |= READ_ONLY;
	       }
	  }
#endif  /* NOT VMS */
	sprintf (name, "%s%s", CBuf->dir, CBuf->file);
	if (!access(name, F_OK) && access(name, W_OK))
	  CBuf->flags |= READ_ONLY;
#endif /* W_OK */
	CBuf->flags |= AUTO_SAVE_BUFFER;
	CBuf->hits = 0;
	type = file_type(CBuf->file);
     }
   else type = (char *) NULL;

   if (type == (char *) NULL) CBuf->modes = NO_MODE;
   else if (SLang_run_hooks("mode_hook", type, NULL));
   else CBuf->modes = NO_MODE;
}

/*}}}*/

/*{{{ reading/inserting files */

int read_file(char *file) /*{{{*/
{
   int fp;
   int n, status;

   if ((fp = sys_open(file, 0)) < 0)
     {
	status = file_status(file);
	if (!status) return(-1);  /* file does not exist */
	return(-2); /* exists but not readable */
     }

   n = read_file_pointer(fp);
   close(fp);
   eob();
   if ('\n' == *(CLine->data + Point)) make_line(2);

   VFile_Mode = VFILE_TEXT;
   return n;
}

/*}}}*/
int insert_file_pointer(VFILE *vp) /*{{{*/
{
   int n = 0;
   unsigned int num;
   unsigned char *vbuf;
   
   Suspend_Screen_Update = 1;
   while(NULL != (vbuf = (unsigned char *) vgets(vp, &num)))
     {
	n++;
	if (SLang_Error) break;
	quick_insert(vbuf, (int) num);
     }
   return(n);
}

/*}}}*/
int insert_file(char *file) /*{{{*/
{
   VFILE *vp;
   int n;
   
   if (NULL == (vp = vopen(file, 0, VFile_Mode))) return(-1);
   n = insert_file_pointer(vp);
   vclose(vp);
   return(n);
}

/*}}}*/

/*}}}*/

/*{{{ writing to files */

#ifdef __unix__
# define BUFSIZE 0x10000
#else
#ifdef VMS
# define BUFSIZE 0x3FFF
#else 
# define BUFSIZE 512
#endif
#endif

static int Output_Buffer_Size = BUFSIZE;
static char Output_Buffer[BUFSIZE];
static char *Output_Bufferp;
static char *Output_Bufferp_max;


/* definitely perform the write.  Return number of chars written */
static int jed_write1(int fd, char *b, unsigned int n) /*{{{*/
{
#if !defined(msdos) || defined(__WATCOMC__) || defined(__WIN32__)
   int len;
   unsigned int total = 0;
#ifdef VMS
   register char *bmax;
#endif
   
   while (total < n)
     {
	len = n - total;
#ifdef VMS
	if (VMS_write_rfm_fixed)
	  {
	  }   
	/* VMS wants to terminate a record with a cr so adjust for this 
	 * unfortunate fact.  The len - 1 stuff is so *bmax does not peek 
	 * beyond its boundary.
	 */
	bmax = b + (len - 1);
	while ((bmax > b) && (*bmax != '\n')) bmax--;
	if (bmax == b) bmax = b + (len - 1); /* cannot be helped */
	len = (int) (bmax - b) + 1;
#endif
	while (-1 == (len = write (fd, b, len)))
	  {
#ifdef EINTR
	     if (errno == EINTR)
	       continue;
#endif
#ifdef EAGAIN
	     if (errno == EAGAIN)
	       {
		  sleep (1);
		  continue;
	       }
#endif
#ifdef ENOSPC
	     if (errno == ENOSPC)
	       {
		  msg_error ("Write Failed: Disk Full.");
		  return total;
	       }
#endif
	     msg_error ("Write Failed: Unknown Error.");
	     return total;
	  }
	
	total += (unsigned int) len;
	b += len;
     }
   return total;
#else
   int num = -1;
   asm mov ah, 40h
   asm mov bx, fd
   asm mov cx, n
   asm push ds
   asm lds dx, dword ptr b
   asm int 21h
   asm pop ds
   asm jc L1
   asm mov num, ax		       /* number of bytes written */
   L1: 
   return(num);
#endif
}

/*}}}*/


/* RMS wants to start a NEW record after a write so just forget it! */
/* maybe do write-- return number of chars possibly written */
static int jed_write(int fd, char *b, unsigned int n) /*{{{*/
{
   int num, max, nl_flag = 0;
   unsigned int nsave = n;
   int cr_flag = CBuf->flags & ADD_CR_ON_WRITE_FLAG;
   
#ifdef MAP_CR_TO_NL_FLAG
   if (CBuf->flags & MAP_CR_TO_NL_FLAG)
     {
	char *bmax = b + n;
	char *p, *pmax, ch;
	p = Output_Bufferp;
	pmax = Output_Bufferp_max;
	
	while (b < bmax)
	  {
	     ch = *b++;
	     if ((ch == '\r') || (ch == '\n'))
	       {
		  if (cr_flag)
		    {
		       *p++ = ch;
		       if (p == pmax)
			 {
			    num = (int) (Output_Bufferp_max - Output_Bufferp);
			    if (num != jed_write1 (fd, Output_Bufferp, num))
			      return -1;
			    Output_Bufferp = Output_Buffer;
			    p = Output_Bufferp;
			 }
		    }
		  *p++ = '\n';
	       }
	     else *p++ = ch;
	     
	     if (p == pmax)
	       {
		  num = (int) (Output_Bufferp_max - Output_Bufferp);
		  if (num != jed_write1 (fd, Output_Buffer, num))
		    return -1;
		  Output_Bufferp = Output_Buffer;
		  p = Output_Bufferp;
	       }
	  }
	Output_Bufferp = p;
	return nsave;
     }
#endif		  
   /* amount of space left in buffer */
   /* copy whats in b to the output buffer */
   while (n > 0)
     {
	num = (int) (Output_Bufferp_max - Output_Bufferp);
	if ((int) n > num)
	  {
#ifdef VMS
	     max = (int) (Output_Bufferp - Output_Buffer);
	     if (max)
	       {
		  if (max != jed_write1(fd, Output_Buffer, max))
		    return(-1);
		  Output_Bufferp = Output_Buffer;
		  continue;
	       }
#endif		  
	     max = num;
	     SLMEMCPY(Output_Bufferp, b, max);
	     Output_Bufferp += max;
	  }

	else if (cr_flag && 
		 (*(b + (n - 1)) == '\n') && (VFile_Mode == VFILE_TEXT))
	  {
	     max = n - 1;
	     SLMEMCPY(Output_Bufferp, b, max);
	     Output_Bufferp += max;
	     *Output_Bufferp++ = '\r';
	     max++;

	     /* can only write the \r */
	     if (n == (unsigned int) num) nl_flag = 1; else *Output_Bufferp++ = '\n';
	  }
	else
	  {
	     max = n;
	     SLMEMCPY(Output_Bufferp, b, max);
	     Output_Bufferp += max;
	  }
	
	if (Output_Bufferp == Output_Bufferp_max)
	  {
	     Output_Bufferp = Output_Buffer;
	     if (Output_Buffer_Size != jed_write1(fd, Output_Buffer, Output_Buffer_Size)) return(-1);
	     if (nl_flag)
	       {
		  nl_flag = 0;
		  *Output_Bufferp++ = '\n';
	       }
	  }
	b += max;
	n -= max;
     }
   return(nsave);
}

/*}}}*/

/* returns -1 on failure */
int write_region_to_fp(int fp) /*{{{*/
{
   register int pnt, len;
   register Line *first, *last;
   int last_pnt, n = 0;
   char *err = "Write Failed!";

#ifndef VMS
   char nl = '\n';
#endif
   
   Output_Bufferp = Output_Buffer;
   Output_Bufferp_max = Output_Buffer + BUFSIZE;
   Output_Buffer_Size = BUFSIZE;

#ifdef VMS
   if (VMS_write_rfm_fixed && (vms_max_rec_size <= BUFSIZE))
     {
	Output_Buffer_Size = vms_max_rec_size;
     }
   else VMS_write_rfm_fixed = 0;
#endif   
   
   if (!check_region(&Number_One)) return(-1);
   last = CLine; last_pnt = Point;

   pop_mark(&Number_One);
   first = CLine; pnt = Point;

   /* first should never be null without hitting last first.  If this
      ever happens, check_region failed. */
   while (first != last)
     {
	len = first->len - pnt;
	if (len != jed_write(fp, (char *) (first->data + pnt), len))
	  {
	     msg_error(err);
	  }
	
	/* This goes here inside the loop because it is possible for external
	   events to set error_buffer */
	pnt = 0;
	if (SLang_Error) break;
	first = first->next;
	n++;
     }

   if (!SLang_Error && (last_pnt != 0))
     {
	len = last_pnt - pnt;
	if (len != jed_write(fp, (char *) (last->data + pnt), len))
	  {
	     msg_error(err);
	  }
	n++;
     }
#ifndef VMS
   if ((Require_Final_Newline) && (CBuf->end == last))
     {
	eob(); if (Point) jed_write(fp, &nl, 1);
     }
#endif
   

   /* Now flush output buffer if necessary */
   
   len = (int) (Output_Bufferp - Output_Buffer);
   if (!SLang_Error && len) if (len != jed_write1(fp, Output_Buffer, len))
     {
	msg_error(err);
     }
   
   Output_Bufferp = Output_Buffer;

   
   pop_spot();
   VFile_Mode = VFILE_TEXT;
   if (SLang_Error) return(-1);
   return(n);
}

/*}}}*/

static int jed_close (int fp) /*{{{*/
{
   while (-1 == close(fp))
     {
#ifdef EINTR
#ifndef pc_system
	if (errno == EINTR) 
	  {
	     errno = 0;
	     sleep (1);
	     continue;
	  }
#endif
#endif
	msg_error ("Error closing file.  File system may be full.");
	return -1;
     }
   return 0;
}

/*}}}*/

int write_region (char *file) /*{{{*/
{
   int fp;
   int n;
   char msg[JED_MAX_PATH_LEN];

   if (!check_region(&Number_Zero)) return(-1);
   if ((fp = sys_open(file, 1)) < 0)
     {
	sprintf(msg, "Unable to open %s for writing.", file);
	msg_error(msg);
	return(-1);
     }
   n = write_region_to_fp(fp);
   if (-1 == jed_close (fp)) n = -1;

   return(n);
}

/*}}}*/


/* returns -1 on failure and number of lines on success */
static int write_file(char *file) /*{{{*/
{
   Mark *m;
   int n = -1;
   int fnl;
      
#ifdef VMS
   register Line *l;
   register int len = 0, max = 0;
#endif
   
   push_spot();
   
#if JED_HAS_SAVE_NARROW
   jed_push_narrow ();
   jed_widen_whole_buffer (CBuf);
#endif
   
#ifdef VMS
   l = CBuf->beg;
   while (l != NULL)
     {
	len = l->len;
	if (len > max) max = len;
	l = l->next;
     }
   vms_max_rec_size = max;
#endif
   
   bob();
   push_mark();  m = CBuf->marks;
   eob();
   fnl = Require_Final_Newline;
   if (CBuf->flags & BINARY_FILE) 
     {
	VFile_Mode = VFILE_BINARY;
	Require_Final_Newline = 0;

#ifdef VMS
	vms_max_rec_size = 512;
#endif
     }

   n = write_region(file);
   
   Require_Final_Newline = fnl;
   VFile_Mode = VFILE_TEXT;
   if (m == CBuf->marks) pop_mark(&Number_Zero);
   
#if JED_HAS_SAVE_NARROW
   jed_pop_narrow ();
#endif   
   
   pop_spot();
   return(n);
}

/*}}}*/

int append_to_file(char *file) /*{{{*/
{
   int fp;
   int n;

   if ((fp = sys_open(file, 2)) < 0) return(-1);
   n = write_region_to_fp(fp);
   if (-1 == jed_close (fp)) n = -1;
   check_buffers();
   return(n);
}

/*}}}*/

/*}}}*/

static int make_autosave_filename(char *save, char *dir, char *file) /*{{{*/
{
   char *s;

   if (*file == 0) return(0);
      
   
   if (SLang_run_hooks("make_autosave_filename", dir, file) && !SLang_Error)
     {
	if (SLpop_string(&s)) return(0);
	strncpy(save, s, JED_MAX_PATH_LEN - 2);
	save[JED_MAX_PATH_LEN - 1] = 0;
	SLFREE(s);
     }
   else
     {
#ifdef __unix__
	sprintf(save, "%s#%s#", dir, file);
#else
#if defined (msdos) || defined (__os2__)
	sprintf(save, "%s#%s", dir, file);
#else
	sprintf(save, "%s_$%s;1", dir, file);
#endif
#endif
     }
   return(1);
}

/*}}}*/

#ifndef VMS
# ifdef REAL_UNIX_SYSTEM
int jed_copy_file (char *from, char *to) /*{{{*/
{
   int mode;
   short uid, gid;
   FILE *fp0, *fp1;
   char buf[0x7FFF];
   unsigned int readlen;
   int ret;
   struct stat st;
   struct utimbuf ut;

   if (1 != sys_chmod (from, 0, &mode, &uid, &gid))
     return -1;		       /* from does not exist as regular file */
   
   /* Need file modification times so that they can be preserved. */
   if (-1 == stat (from, &st))
     return -1;
   
   fp0 = fopen (from, "rb");
   if (fp0 == NULL) return -1;
   
   fp1 = fopen (to, "wb");
   if (fp1 == NULL) 
     {
	(void) fclose (fp0);
	return -1;
     }
   
   (void) chmod (to, 0600);

   ret = 0;
   do
     {
	readlen = fread (buf, 1, sizeof(buf), fp0);
	if (readlen)
	  {
	     if (readlen != fwrite (buf, 1, readlen, fp1))
	       {
		  ret = -1;
		  break;
	       }
	  }
     }
   while (readlen == sizeof (buf));
   
   fclose (fp0);

   if (EOF == fclose (fp1))
     {
	ret = -1;
     }
   
   (void) sys_chmod (to, 1, &mode, &uid, &gid);
   /* Set file modification times */
   ut.actime = st.st_atime;
   ut.modtime = st.st_mtime;
   (void) utime (to, &ut);
   
   return ret;
}
/*}}}*/
# endif
#endif

#ifndef VMS   
static int perform_backup (char *from, char *to, int try_force_rename) /*{{{*/
{
   int ret = -1;
   
   if (try_force_rename
       || (Jed_Backup_By_Copying == 0))
     {
	(void) unlink(to);
	ret = rename (from, to);
     }
#ifdef REAL_UNIX_SYSTEM
   /* Needs porting to OS/2 and DOS */
   if (ret == -1)
     ret = jed_copy_file (from, to);
#endif

   return ret;
}
/*}}}*/
#endif

int write_file_with_backup(char *dir, char *file) /*{{{*/
{
   char neew[JED_MAX_PATH_LEN]; char autosave_file[JED_MAX_PATH_LEN];
   int n;
   int mode, do_mode;
   short uid, gid;
#ifndef VMS
   char *old = NULL;
   int backup_went_ok;
#endif
#ifdef __os2__
   PHOLDFEA EAs;
#endif
   
   if (*file == 0) return(-1);

   sprintf(neew, "%s%s", dir, file);
   
   *autosave_file = 0;
   if (CBuf->flags & AUTO_SAVE_BUFFER)
      (void) make_autosave_filename(autosave_file, dir, file);

   do_mode = sys_chmod(neew, 0, &mode, &uid, &gid);
   if ((do_mode != 0) && (do_mode != 1)) return -1;
   /* IF do_mode == 0, then the file does not exist.  This means that 
    * there is nothing to backup.  If do_mode == 1, the file is a regular
    * file which we do want to backup.
    */
   
#ifndef VMS
   
# ifdef __os2__
   EAs = QueryEAs (neew);
# endif
   
   backup_went_ok = 0;
   if (do_mode
       && ((CBuf->flags & NO_BACKUP_FLAG) == 0)
       && SLang_run_hooks("make_backup_filename", dir, file))
     {
	if ((0 == SLpop_string(&old))
	    && (*old != 0))
	  {
	     backup_went_ok = (0 == perform_backup (neew, old, 0));
	  }
     }
#endif				       /* NOT VMS */
   
   if (-1 != (n = write_file(neew)))
     {
	if (*autosave_file)
	  {
	     sys_delete_file(autosave_file);
	  }
	     
	if (do_mode) /* must be an existing file, so preserve mode */
	  {
#ifdef msdos
	     /* Want the archive bit set since this is a new version */
	     mode |= 1 << 5;
#endif
	     sys_chmod (neew, 1, &mode, &uid, &gid);
#ifdef __os2__
	     WriteEAs (neew, EAs);
#endif
	  }
	/* This is for NFS time problems */
	CBuf->c_time = sys_file_mod_time(neew);
	
	/* Since we wrote the buffer to the file, it is not modified. */
	if (
#ifdef __os2__
	    !strcmpi(file, CBuf->file) && !strcmp(dir, CBuf->dir)
#else
	    !strcmp(file, CBuf->file) && !strcmp(dir, CBuf->dir)
#endif
	    )
	  CBuf->flags &= ~FILE_MODIFIED;
	
	/* CBuf->c_time = sys_time(); */
     }
#ifndef VMS
   /* error -- put it back */
   else if (backup_went_ok) perform_backup (old, neew, 1); 
   
   if (old != NULL) SLFREE(old);
#endif
   return(n);
}

/*}}}*/

void auto_save_buffer(Buffer *b) /*{{{*/
{
   char tmp[JED_MAX_PATH_LEN];
   Buffer *old_buf;
   unsigned int vfm = VFile_Mode;
   
   if (b == NULL) return;
   b->hits = 0;
   if ((0 == (b->flags & BUFFER_TRASHED))
       || (0 == (b->flags & (AUTO_SAVE_BUFFER | AUTO_SAVE_JUST_SAVE))))
     return;

#if !JED_HAS_SAVE_NARROW
   if (b->narrow != NULL) return;
#endif

   old_buf = CBuf;
   switch_to_buffer(b);
   
   if (b->flags & BINARY_FILE)  VFile_Mode = VFILE_BINARY; 
   else VFile_Mode = VFILE_TEXT;

   if (b->flags & AUTO_SAVE_BUFFER)
     {
	if (make_autosave_filename(tmp, b->dir, b->file))
	  {
	     flush_message("autosaving..."); 
	     sys_delete_file(tmp);
	     write_file(tmp);
#ifndef pc_system
	     (void) chmod (tmp, 0600);
#endif
	     message("autosaving...done");
	  }
      }
   else if (write_file_with_backup(b->dir, b->file) >= 0)
     {
	b->flags &= ~BUFFER_TRASHED;
     }

   switch_to_buffer(old_buf);
   VFile_Mode = vfm;
}

/*}}}*/

void auto_save_all() /*{{{*/
{
    Buffer *b;

   if (NULL == (b = CBuf)) return;
   do
     {
	jed_widen_whole_buffer (b);
	if ((*b->file != 0) && (b->hits != 0)) auto_save_buffer(b);
	b = b->next;
     }
   while (b != CBuf);
}

/*}}}*/

#ifdef REAL_UNIX_SYSTEM
/*{{{ symbolic link stuff */

static int is_link(char *f, char *f1) /*{{{*/
{
   struct stat s;
   unsigned int l;
   int is_dir = 0;
   char work[JED_MAX_PATH_LEN];
   
   l = strlen(f);
   if (l == 0) return 0;
   l--;
   if ((l > 1) && (f[l] == '/'))
     {
	strcpy(work, f);
	is_dir = 1;
	f = work;
	f[l] = 0;
     }
   

   if (( lstat(f, &s) == -1 ) 
       /* || ((s.st_mode & S_IFMT)  S_IFLNK)) */
       || (((s.st_mode & S_IFMT) & S_IFLNK) == 0))
     return(0);
   
   l = JED_MAX_PATH_LEN - 1;
   if (is_dir)
     {
	/* This way, it will be expanded properly upon return */
	*f1++ = '.'; *f1++ = '.'; *f1++ = '/';
	l -= 4;			       /* 1 for final slash */
     }
	     
   if (-1 == (int) (l = readlink(f, f1, l))) return 0;
   if (is_dir) f1[l++] = '/';
   f1[l] = 0;
   return(1);
}

/*}}}*/

/* This routine should be modified to work with all components of the path
 * and not just the last component.  For example, suppose /usr/lib/jed is a 
 * link to /usr/local/src/jed and one tries to read the file
 * /usr/lib/jed/lib/site.sl.  This should be expanded to 
 * /usr/local/lib/jed/lib/site.sl.  Currently, it will only have an effect if
 * site.sl is itself a link.
 */
char *expand_link(char *f) /*{{{*/
{
   char work[JED_MAX_PATH_LEN], lnk[JED_MAX_PATH_LEN];
   char *d = expand_filename(f);
 
  
   if (is_link(d, lnk))
     {
	char *w, *w1, ch;
	
	if (*lnk == '/')	       /* absolute */
	  strcpy (work, lnk);
	else
	  {
	     strcpy (work, d);
	     *(extract_file(work)) = 0;
	     strcat (work, lnk);
	  }
	
	/* Collapse multiple // since expand_filename will interpret them
	 * differently.
	 */
	w = w1 = work;
	while ((ch = *w1++) != 0)
	  {
	     *w++ = ch;
	     if (ch == '/') while (*w1 == '/') w1++;
	  }
	*w = 0;
	
	d = expand_filename(work);
     }
   
   return (d);
}

/*}}}*/

/*}}}*/
#endif

void visit_file(char *dir, char *file) /*{{{*/
{
#ifndef __os2__
   if (strcmp(file, CBuf->file) || strcmp(dir, CBuf->dir))
#else
   if (strcmpi(file, CBuf->file) || strcmp(dir, CBuf->dir))
#endif
     {
	if (NULL == find_buffer(file)) strcpy(CBuf->name, file); 
	else uniquely_name_buffer(file);
   
	strcpy(CBuf->dir, dir);
	strcpy(CBuf->file, file);
     }
   /* We have already taken care of this in write buffer function.
    */
   /* CBuf->c_time = sys_time(); */

   check_buffers();
}

/*}}}*/

char *dir_file_merge(char *dir, char *file) /*{{{*/
/* 
 * returns result of merging dir with file. If file is empty, dir is
 * considered to be whole file.
 */
{
   char name[512];

   strcpy (name, dir);
   if ((file != NULL) && *file)
     {
	fixup_dir(name);
	strcat(name, file);
     }
   return expand_filename(name);
}

/*}}}*/

int file_status(char *file) /*{{{*/
/*
 *  Returns a coded integer about file.  If the file does not exist, 0 is
 *  returned.  Meaning:
 *
 *     2 file is a directory
 *     1 file exists
 *     0 file does not exist.
 *    -1 no access.
 *    -2 path invalid
 *    -3 unknown error
 */
{
   int mode = 0;
   short uid, gid;
   return sys_chmod(file, 0, &mode, &uid, &gid);
}

/*}}}*/
int file_changed_on_disk(char *file) /*{{{*/
{
   unsigned long t;
   Buffer *buf;
   if (NULL == (buf = find_file_buffer(file))) return(0);
   if (buf->flags & FILE_MODIFIED) return 1;
   t = sys_file_mod_time(file);
   return(t > buf->c_time);
}

/*}}}*/
int file_time_cmp(char *file1, char *file2) /*{{{*/
{
   unsigned long t1, t2;
   
   t1 = sys_file_mod_time(file1);
   t2 = sys_file_mod_time(file2);
    /*These times are modification  times from 1970.  Hence, the bigger 
     * number represents the more recent change.  Let '>' denote 
     * 'more recent than'.  This function returns:
     *	   1:  file1 > file2
     *	   0:  file 1 == file2
     *	   -1: otherwise. 
     *	So:
     */
   if (t1 == t2) return(0);
   if (t1 > t2) return(1); 
   return(-1);
}

/*}}}*/

void auto_save(void) /*{{{*/
{
   auto_save_buffer(CBuf);
}

/*}}}*/

void check_buffer(Buffer *b) /*{{{*/
{
#ifdef REAL_UNIX_SYSTEM
   /* Update (or even invalidate) inode information since directories may have
    * been renamed, etc...
    */
   (void) get_inode_info (b->dir, &b->device, &b->inode);
#endif

   if ((*b->file != 0)
       && file_changed_on_disk(dir_file_merge(b->dir, b->file)))
     {
	b->flags |= FILE_MODIFIED;
     }
   else b->flags &= ~FILE_MODIFIED;
}

/*}}}*/
void check_buffers() /*{{{*/
{
   Buffer *b = CBuf;
   do
     {
	check_buffer(b);
	b = b->next;
     }
   while (b != CBuf);
}

/*}}}*/

void set_file_trans(int *mode) /*{{{*/
{
   if (*mode) VFile_Mode = VFILE_BINARY; else VFile_Mode = VFILE_TEXT;
}

/*}}}*/

int read_file_pointer(int fp) /*{{{*/
{
   int n = 1;
   unsigned int num;
   unsigned char *vbuf;
   VFILE *vp;
   
   if (SLang_Error || (vp = vstream(fp, MAX_LINE_LEN, VFile_Mode)) == NULL) return(-1);
   
   if (NULL == (vbuf = (unsigned char *) vgets(vp, &num))) return(0);
   
#ifdef KEEP_SPACE_INFO
   if (CLine->space < num)
#endif
     remake_line(CLine->len + num + 1);
   
   SLMEMCPY((char *) CLine->data, (char *) vbuf, (int) num);
   CLine->len = num;
   
   while(NULL != (vbuf = (unsigned char *) vgets(vp, &num)))
     {
	n++;
	if ((num == 1) && (*vbuf == '\n')) make_line(num); else make_line(num + 1);
	SLMEMCPY((char *) CLine->data, (char *) vbuf, (int) num);
	CLine->len = num;
	if (SLang_Error) break;
     }
   if (vp->buf != NULL) SLFREE(vp->buf);
   
   if (vp->cr_flag) CBuf->flags |= ADD_CR_ON_WRITE_FLAG;
   else CBuf->flags &= ~ADD_CR_ON_WRITE_FLAG;
   
   SLFREE(vp);
   return n;
}

/*}}}*/

#ifdef REAL_UNIX_SYSTEM
int get_inode_info (char *dir, int *device, int *inode) /*{{{*/
{
   struct stat st;
   char work[512];
   char *f;
   
   f = extract_file (dir);
   if (*f != 0)
     {
	unsigned int len = f - dir;
	strncpy (work, dir, len);
	work[len] = 0;
	dir = work;
     }
   
   *inode = *device = -1;
   if (-1 == stat (dir, &st)) return -1;
   
   *inode = st.st_ino;
   *device = st.st_dev;
   
   return 0;
}
/*}}}*/
#endif
   
