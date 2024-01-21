/*
   Time-stamp: <96/07/19 20:11:42 yusuf>

   $Id: bg_backup.c,v 1.6 1996/07/27 20:42:05 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: bg_backup.c,v 1.6 1996/07/27 20:42:05 yusuf Exp $";
#endif /* lint */



/* This is the program to compress files for use with taper.

 *  It expects the following command line arguments:
 *   
 *    log_file            name of log file
 *    log_level           log level
 *    compress_files      name of file which contains files to compress
 *    fifo_name           name of FIFO to write compressed filenames to
 *    compress_type       compression type
 *    shared memory ID    0 if no ID
 *    exclude compression list of files to exclude from compression
 *    tmp_dir             temporary directory
 *    min_free            amount of disk space that must be free
 * 
 * 
 * If the child stops because not enough disk space, sends a DISK_FULL
 *   to FIFO
*/

#include "taper.h"
#include <sys/vfs.h>

_s8 stop_compressing;
_errstat my_gzip(char *file, char *wf)
{
    int ifd, ofd;
    char l[1024];

    sprintf(l, "B:Internal gzipping %s to %s", file, wf);
    if (log_level > 3) write_log(l);
    ifd = open(file, O_RDONLY);
    if (ifd==-1) {
	sprintf(l, "B:%s while opening file", strerror(errno));
	if (log_level > 3) write_log(l);
	return -1;
    }
    ofd = creat(wf, S_IREAD|S_IWRITE);
    if (ofd==-1) {
	sprintf(l, "B:%s while creating file", strerror(errno));
	if (log_level > 3) write_log(l);
	close(ifd); 
	return -1;
    }
    zip(ifd, ofd);
    close(ifd); close(ofd);
    return 0;
}

        
_errstat my_compress(char *file, char *wf)
{
/* Compresses the 'file' to output file 'wf' 
 * 
 * To aid speed, reads the whole file into memory and compresses
 * 
 * Returns 0 if OK, -1 if error, -2 if compressed file would be larger
 * 
 * Returns an error if given a NULL file
 * If the file is > COMPRESS_MAX_ORG (0x700000000), then an error
 *   is returned - I can't ever see a file this long - and if it is,
 *   it's probably best not to compress
 */
    int fd;
    struct stat b;
    ULONG   c_len;
    char    l[1024];

    sprintf(l, "B:Original compressing %s to %s", file, wf);
    if (log_level > 3) write_log(l);
    if ((comp_buffer1 == NULL) || (comp_buffer2 == NULL))
      return -1;
    fd=open(file, O_RDONLY); 
    if (fd == -1) 
	return -1;
    if (fstat(fd, &b) == -1) {close(fd); return -1;} 
    if (b.st_size == 0) {close(fd); return -1;}	 /* not going to compress empty files */
    if (b.st_size > COMPRESS_MAX_COM) {close(fd); return -1;}   /* too large for compress to handle */
    if (b.st_size > COMPRESS2_BUFFER_SIZE) {close(fd); return -1;}   /* too large for compress buffer */
    if (read(fd, comp_buffer1, b.st_size) == -1) {close(fd); return -1;};
    close(fd);
    compress(COMPRESS_ACTION_COMPRESS, cbuf, (UBYTE *) comp_buffer1, b.st_size,
	     (UBYTE *) comp_buffer2, &c_len);
    if (c_len == 0) return -1;			 /* error compressing */
    if (c_len >= b.st_size) return -2;		 /* compressed size is bigger */
    fd=creat(wf, S_IREAD|S_IWRITE); if (fd==-1) return -1;
    if (write(fd, comp_buffer2, c_len)==-1) {close(fd); return -1;}
    close(fd);
    return 0;
}
    

void end_prog(int signal)
{
/* Child has received terminate signal */
    stop_compressing=1;
}

_s32 free_space()
{
/* Returns the number of K of diskspace free */
    struct statfs st;
    
    if (statfs(temp_dir, &st) < 0) return -1;
    if (st.f_bsize > 1024) return st.f_bavail * (st.f_bavail / 1024);
    else if (st.f_bsize < 1024) return st.f_bavail / (1024 / st.f_bavail);
    return st.f_bavail;
}

int main(int argc, char *argv[])
{
    
    char com[MAX_FNAME], tmpbuf[MAX_FNAME], ftoc[MAX_FNAME];    
    int x=0, total_sleep;
    _s32 fs;
    int compression;
    char *fifo_name, *compress_files;
    FILE *fdfifo=NULL, *cf=NULL;
#ifndef TRIPLE_BUFFER
    int shm_id;
#endif

    fifo_name = argv[4];
    fdfifo = fopen(fifo_name, "w");		 /* open FIFO  */
    init_memory();
    strcpy(log_file, argv[1]);			 /* get command line args */
    log_level = atoi(argv[2]);
    compress_files = argv[3];
    compression = atoi(argv[5]);
    shm_id = atoi(argv[6]);
    strcpy(exclude_compress, argv[7]);
    strcpy(temp_dir, argv[8]);
    min_free = atoi(argv[9]);

#ifdef TRIPLE_BUFFER    
    shm = (shm_id == 0) ? NULL : (struct shared_mems *) shmat(shm_id, 0, 0);
#else
    shm = NULL;
#endif    
    lf = open(log_file, O_WRONLY|O_APPEND);	 /* open log file */
    if (lf == -1) goto fin;
    if (fdfifo == NULL) {
	if (log_level > 1) write_log("B:ERROR: Couldn't open FIFO");
	goto fin;
    }
    fprintf(fdfifo, "CHILD\n");
    cf = fopen(compress_files, "r");		 /* that contains files to compress */   
    if (cf == NULL) goto fin;
    
    if (compression == 2) {
	if (malloc_comp_buffers() == -1) {
	    if (log_level > 3) write_log("B:Unable to make compress 2 buffers - not compressing");
	    goto fin;
	}
    }
    signal(SIGTERM, end_prog);			 /* stop compressing when get this */
    
    stop_compressing=0;				 /* install TERM handler */
    while (!stop_compressing && !feof(cf)) {
	if (stop_compressing) goto fin;		 /* child been asked to stop */
	fgets(ftoc, sizeof(ftoc), cf);
	ftoc[strlen(ftoc)-1] = 0;		 /* remove trailing \n */
	if (!exclude_compression(ftoc)) {	 /* check we'd compress this */
	    total_sleep=0; 
	    while ((fs=free_space()) < min_free) {
		if (fs == -1) break;		 /* couldn't get free space - assume OK */
		if (!total_sleep) {		 /* only for initial */
		    fprintf(fdfifo, "DISK FULL\n");
		    fflush(fdfifo);
		}
		if (log_level > 3) {
		    sprintf(com, "B: Only %dK free - waiting 15s", fs);
		    write_log(com);
		}
		sleep(15);
		total_sleep++;
		if (total_sleep == 25) {
		    if (log_level > 1) write_log("B:Child timed out waiting for free disk space");
		    log_errors++;
		    goto fin;
		}
	    }
	    taper_tmpnam(tmpbuf);		 /* get temp filename */
	    sprintf(com, " :Compressing %s to %s", ftoc, tmpbuf);
	    *com = (compression == 1) ? 'E' : 'I';
	    if (log_level > 2) write_log(com);
	    switch(compression) {
	     case 1:		 /* external gzip */
		sprintf(com, "%s \"%s\" > %s 2>/dev/null", COMPRESS_PROG, 
			ftoc, tmpbuf);	 /* compress file */
		change_dollar(com);
		x = system(com);
		break;
	     case 2:		 /* internal compress */
		x = my_compress(ftoc, tmpbuf);
		  break;
	     case 3:		 /* internal gzip */
		x = my_gzip(ftoc, tmpbuf);
		break;
	     default:
		x = -1; break;
	    }
	     
	    if (x==0) 		 /* OK compression */
	      x = fprintf(fdfifo, "%s\n%s\n", ftoc, tmpbuf);
	    else {			 /* problem compressing */
		(x==-1) ?
		  sprintf(com, "B:couldn't compress %s err=%d", ftoc,  errno) :/* error copmressing */
		sprintf(com, "B:didn't compress %s as it grew", ftoc);
		if (log_level > 3)  write_log(com);   /* compressed was larger */
		x = fprintf(fdfifo, "%s\n%s\n", ftoc, FIFO_ERR);
		unlink(tmpbuf);			 /* remove file */
	    }
	    fflush(fdfifo);		 /* to ensure received */
	}
    }

    fin:;
    if (fdfifo != NULL) fclose(fdfifo);
    if (cf != NULL) fclose(cf);
    unlink(compress_files);			 /* remove the file */
    if (log_level > 3) write_log("Backup child about to finish");
    if (lf) close(lf);
    free_comp_buffers();
    my_free_all();
#ifdef TRIPLE_BUFFER    
    if (shm)
      shmdt((char *) shm);
#endif    
    return 0;
}



