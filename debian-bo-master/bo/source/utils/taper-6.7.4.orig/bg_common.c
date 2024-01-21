/*
   Time-stamp: <96/08/04 17:27:59 yusuf>

   $Id: bg_common.c,v 1.8 1996/08/04 16:38:58 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: bg_common.c,v 1.8 1996/08/04 16:38:58 yusuf Exp $";
#endif /* lint */



/* Common routines to background programs and main
   taper program */
   
   
#include "taper.h"   


void mail_finish(char *prog)
/* Prints a message to the mail file about completion */
{
    time_t t;
    char s[100];
    
    time(&t);
    sprintf(s, "\n%s finished at %s\n", prog, ctime(&t));
    write(mf, s, strlen(s));
}


void sendmail()
{
/* Closes mail file, sends mail and deletes mail file */
    char s[MAX_FNAME];
    
    mail_finish("Backup");
    close(mf);
    sprintf(s, "%s %s -s \"Taper Unattended Backup\" < %s", MAIL_PROG, MAIL_TO, mailfn);   /* send mail */
    system(s);
    unlink(mailfn);
}


_errstat do_exit(int c) {
    char err[100];
    _s32  adjc=0;
    
    while (errs[adjc].err_no)			 /* find error in template */
      if (errs[adjc].err_no == c)
        break;
      else adjc++;
    strcpy(err, strerror(errno));
    strcat(err, " while ");
    (errs[adjc].no_append) ? strcpy(err, errs[adjc].mess) :
    (errs[adjc].err_no == 0) ? strcat(err, "Unknown error") :
                               strcat(err, errs[adjc].mess);
    write_log(err);
    (win_main == NULL) ? fprintf(stderr, "%s\n", err) :
                         message_box(err, MB_OK);
    if (errs[adjc].exit || !c) {		 /* need to properly exit */
    	write_log("Exiting with error");
    	if (lf) 
	   write(lf, "\n\n", strlen("\n\n"));
	my_free_all();				 /* free all user memory */
	exit(-1);
    }
    log_errors++;
    if (mf) { 					 /* write to mail file */
	strcat(err, "\n");
	write(mf, err, strlen(err));
	strcpy(err, "Backup ABORTED\n");
	write(mf, err, strlen(err));
	sendmail();				 /* send mail */
    }
    return -1;
}


_errstat exclude_list(char *fn, char *list)
{
/* Determines whether to exclude file 'fn' from archive 
 * 
 * returns 1 if should be excluded (ie. in list)
 * returns 0 if shouldn't be excluded
*/
    char *s, *b;
    char tok[MAX_FNAME];
    int fin=0;
   
    if (!*list) return 0;                         /* empty exclusion list */ 
    strcpy(tok, list);
    s=tok;
    while (!fin) {
	b=s;
	while (*s && (*s != ' ')) s++;		 /* look for space or eos */
	if (!*s) fin=1;				 /* end of string */
	*s = 0; 
	if (!strcasecmp(b, &fn[strlen(fn)-strlen(b)]))
	  return 1;
	s++;
    }
    return 0;
}


_errstat exclude_compression(char *fn)
{
/* Checks to see if file is one of the files not to compress.
 * 
 * returns 1 if should be excluded (ie. in list)
 * returns 0 if shouldn't be excluded
*/
    return exclude_list(fn, exclude_compress);
}

    
_errstat malloc_comp_buffers()
{
    ULONG pci;
    struct compress_identity *ci;
    
    compress(COMPRESS_ACTION_IDENTITY, NULL, NULL, 0, NULL, &pci);   /* get compression buffer */
    ci = (struct compress_identity *) pci;
    cbuf = my_malloc(ci->memory);
    if (cbuf == NULL) return do_exit(ERROR_MEMORY);
    comp_buffer1 = my_malloc(COMPRESS2_BUFFER_SIZE);
    if (comp_buffer1 == NULL) return do_exit(ERROR_MEMORY);
    comp_buffer2 = my_malloc(COMPRESS2_BUFFER_SIZE);
    if (comp_buffer2 == NULL) return do_exit(ERROR_MEMORY);
    return 0;
}


void free_comp_buffers()
{
    if (comp_buffer1) my_free(comp_buffer1);
    if (comp_buffer2) my_free(comp_buffer2);
    if (cbuf) my_free(cbuf);
}


void write_log(char *s)
{
    time_t  t;
    char    s1[500], s2[500];
    
    if (!lf) return;				 /* no log file open */
    t = time(NULL);
    strcpy(s1, ctime(&t));
    s1[strlen(s1)-1] = 0;			 /* remove trailing \n */
    sprintf(s2, "%s:  %s\n", s1, s);
    write(lf, s2, strlen(s2));
}


void write_error_log(char *s)
{
/* Writes an error to the log file. 
 * 
 * Format:   Date:  ERROR: Error while s
*/ 
    char s1[5000];
    
    sprintf(s1, "ERROR:  %s while %s", strerror(errno), s);
    write_log(s1);
    log_errors++;
}


void write_warning_log(char *s)
{
/* Writes a warning to the log file. 
 * 
 * Format:   Date:  WARNING: s
*/ 
    char s1[200];
    
    sprintf(s1, "WARNING:  %s", s);
    log_warnings++;
    write_log(s1);
}


void write_fatal_log(char *s)
{
/* Writes a fatal error to the log file. 
 * 
 * Format:   Date:  FATAL ERROR: Error while s
*/ 
    char s1[5000];
    
    sprintf(s1, "FATAL ERROR:  %s while %s", strerror(errno), s);
    log_errors++;
    write_log(s1);
}


   

void taper_tmpnam(char *s)
{
/* Makes a temporary filename for use by taper.
 * Simply calls tempnam and appends taper to it
 */
    char *x=tempnam(temp_dir, "taper");
    
    strcpy(s, x); 
    free(x);
}


void change_dollar(char *s)
{
/* Reads a string an replaces $ with \$
 * To avoid shell expansions
 */
    
    char  *s1=s, *s2;
    
    while (*s1) {
	if (*s1 == '$') {
	    for (s2=&s[strlen(s)]; s2>=s1; s2--)
	      *(s2+1) = *s2;
	    *s1 = '\\';
	    s1++;
	}
	s1++;
    }
}


_errstat make_dirs(char *s)
/* Makes directories for a filename.
 * For example, if s = t/usd/y/f, it will make directories
 * t, then usd then y. Permission are rwr-xr-x and the owner
 * is the owner of the person running the program.
 * 
 * Returns 0 if error occurred, otherwise 1
*/ 
{
    char *s1;
    char s2[MAX_FNAME], s3[MAX_FNAME];
    int  c=0;
    DIR *dir;
    
    strcpy(s3, s);
    if(strrchr(s3, '/') == NULL)		 /* filename only */
      return 1;
    *strrchr(s3, '/') = 0;			 /* remove filename */
    
    *s2 = 0;
    s1 = s3;
    if (*s1 == '/') {				 /* we don't try and create */
	strcpy(s2, "/");			 /* the root directory */
	s1++;
	c = 1;
    }
    while (*s1) {
	while (*s1 && (*s1 != '/')) 
	    s2[c++] = *s1++;
	s2[c] = 0;
	if (*s1) s1++;
	if ((dir=opendir(s2)) == NULL) 		 /* check to see if dir exists */
	  if (errno == ENOENT) {		 /* no it doesn't */
	      if (mkdir(s2, 493) == -1) {		 /* try and create */
		  sprintf(s3, "creating directory %s", s2);
		  write_error_log(s3);
		  return 0;
	      }
	  }
	  else {
	      sprintf(s3, "opening directory %s", s2);
	      write_error_log(s3);
	      return 0;
	  }
	
	if (dir)				 /* close dir if it was opened */
	  closedir(dir);
	s2[c++] = '/';
    }
    return 1;
}
    

_errstat my_rename(char *oldf, char *newf)
{
/* Renames filename 'old' to 'new'. New is overwritten
 * 
 * If it is a cross-device link, the file is copied.
 * 
 * The tr_buffer is used and assumed to have been initialized
 * and of size max_tr_size
 * 
 * Returns 0 if OK, -1 otherwise
*/
    int oldfd, newfd, e;
    struct stat b;
    
    if (rename(oldf, newf) == 0)
      return 0;
    if (errno != EXDEV)				 /* couldn't rename*/
      return do_exit(ERROR_RENAMING);
    /* cross device - must copy file */
    
    oldfd = open(oldf, O_RDONLY);
    if (oldfd == -1) return do_exit(ERROR_RENAMING);
    fstat(oldfd, &b);
    newfd = creat(newf, b.st_mode);
    if (newfd == -1) {
	close(oldfd);
	return do_exit(ERROR_RENAMING);
    }
    e = my_filecopy(oldfd, newfd);
    close(oldfd);
    close(newfd);
    (e == -1) ?  unlink(newf) : unlink(oldf);	 /* if error, remove new file, else old file */
    if (e == -1) return do_exit(ERROR_RENAMING);
    return 0;
}


_errstat my_filecopy(int oldfd, int newfd)
{
/* Copies the contents of file oldf to newf.
 * 
 * Assumes both files are opened and positioned correctly.
 * Uses tr_buffer and assumes that it is initialised and is
 * of size max_tr_size
 * 
 * Returns 0 if OK, -1 otherwise
*/
    _s32 x;

    while (1) {
	x = read(oldfd, tr_buffer, max_tr_size);
	if (!x) break;
	if (x == -1)
	  return -1;
	write(newfd, tr_buffer, x);
    }				 
    return 0;
}


_errstat setowners(char *s, _errstat ret, struct file_info *fi)
{
    char l[500];
    _s32  sz;
    struct utimbuf ut;

    if (!S_ISLNK(fi->mode)) {			 /* don't set anything for links */
	sprintf(l, "Setting permissions, modes & times for %s", s);
	if (log_level > 2) write_log(l);
	sz = 0;
	sz += chown(s, fi->uid, fi->gid);
	sz += chmod(s, fi->mode);		 /* directories/links */
	ut.actime = fi->atime;			 /* fix up times */
	ut.modtime = fi->mtime;
	sz += utime(s, &ut);
	if (sz)
	  write_error_log(l);
	if (ret == -4) return ret;
    }
    return (ret == -3) ? -2 : 1;		 /* we passed file */
}
