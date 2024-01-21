/*
   Time-stamp: <96/07/19 20:11:50 yusuf>

   $Id: bg_restore.c,v 1.5 1996/07/27 20:42:05 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: bg_restore.c,v 1.5 1996/07/27 20:42:05 yusuf Exp $";
#endif /* lint */



/* This is the program to de-compress files for use with taper.

 *  It expects the following command line arguments:
 *   
 *    log_file            name of log file
 *    log_level           log level
 *    fifo_name           name of FIFO to write compressed filenames to
 *    shared memory ID    0 if no ID
 *    overwrte level
 *    restore_mode
 *    temp_dir            where to write temporary directories
 * 
 *    FIFO contains:
 * 
 *           filename of file                        ]
 *           compressed filename or END\n            ] for each file
 *           struct file_info file this file         ]
*/

#include "taper.h"


_s8 stop_child;
void end_prog(int signal)
{
/* Child has received terminate signal */
    stop_child=1;
}

void cont_prog(int signal)
{
/* Child has received SIGUSR1 - just continue */
}

int main(int argc, char *argv[])
{
    _s32 totread, toread, red;
    char *fifo_name;
    FILE *fdfifo;
    struct file_info fi;
    char   s[MAX_FNAME], ff[MAX_FNAME], fn[MAX_FNAME], fn1[MAX_FNAME];
    char   l[500];
    int    of=0, ifd;
    struct stat statbuf, ibuf, obuf;
    UBYTE  *cmp_buf, *uncmp_buf;
    ULONG  xx, x;
#ifndef TRIPLE_BUFFER
    int  shm_id;
#endif
    char *tr_buffer1;

    fifo_name = argv[3];
    fdfifo = fopen(fifo_name, "r");		 /* open FIFO  */
    if (fdfifo == NULL) exit(-1);
    stop_child = 0;
    signal(SIGTERM, end_prog);			 /* stop compressing when get this */
    signal(SIGUSR1, cont_prog);
    init_memory();
    strcpy(log_file, argv[1]);			 /* get command line args */
    log_level = atoi(argv[2]);
    shm_id = atoi(argv[4]);
    ovrwrite = atoi(argv[5]);
    restore_mode = atoi(argv[6]);
    strcpy(temp_dir, argv[7]);
    tr_buffer = my_malloc(max_tr_size);		 /* make a transfer buffer */
    tr_buffer1 = my_malloc(max_tr_size);	 /* for comparing */
#ifdef TRIPLE_BUFFER    
    shm = (shm_id == 0) ? NULL : (struct shared_mems *) shmat(shm_id, 0, 0);
#else
    shm = my_malloc(sizeof(struct shared_mems)); /* just for memory */
    memset(shm, 0, sizeof(struct shared_mems));
#endif    
    lf = open(log_file, O_WRONLY|O_APPEND);	 /* open log file */
    if (lf == -1) goto fin;
    if (log_level > 3) {
	sprintf(l, "R:bg_restore started - mode %d", restore_mode);
	write_log(l);
    }
    if ((tr_buffer == NULL) || (tr_buffer1 == NULL)) {
	if (log_level > 1) {
	    write_log("ERROR: R:Unable to allocate transfer buffers");
	    log_errors++;
	}
	goto fin;
    }
    if (malloc_comp_buffers() == -1) {
	if (log_level > 1) {
	    write_log("ERROR: R:Unable to make compress 2 buffers - not compressing");
	    log_errors++;
	}
	goto fin;
    }
    
    while (!stop_child) {
	if (fgets(s, sizeof(s), fdfifo) == NULL) /* name of file */
	  break;
	if (fgets(ff, sizeof(ff), fdfifo) == NULL)/* name of temporary file where file is */
	  break;
	if (!strcmp(ff, "END\n"))		 /* finished */
	  break;
	if (fread(&fi, sizeof(fi), 1, fdfifo) == 0)	 /* read archive info  */
	  break;
	if (ff[strlen(ff)-1] == '\n')
	  ff[strlen(ff)-1] = 0;
	if (s[strlen(s)-1] == '\n')
	  s[strlen(s)-1] = 0;
	sprintf(l, "R:FIFO info is %s --> %s", ff, s);
	if (log_level > 2) write_log(l);
	if (!strcmp(ff, FIFO_ERR)) 		 /* error writing out this entry */
	  continue;
	
	sprintf(l, "R:Getting file info for file %s", s);
	if (!make_dirs(s))			 /* make directories */
	  goto next_file;			 /* failed in making directories */
	if (log_level > 3) write_log(l);
	if ((of = lstat(s, &statbuf)) == -1)	 /* try and get info about file */
	  if (errno != ENOENT) {		 /* not an error is couldn't */
	    write_error_log(l);
	    goto next_file;
	  }
	
	if (restore_mode != RESTORE_VERIFY) {
	    switch (ovrwrite) {
	     case 0:				 /* no overwrite */
		if (of != -1) {			 /* file existed */
		    sprintf(l, "R:File %s exists. Not overwriting", s);
		    write_warning_log(l);
		  goto next_file;
		}
		break;
	     case 1:				 /* only overwrite if backup file more recent */
		if ((fi.ctime <= statbuf.st_ctime) && (of != -1)) {
		    sprintf(l, "R:File %s not overwritten because not as recent", s);
		    write_warning_log(l);
		    goto next_file;
		}
		break;
	    }
	    unlink(s);				 /* remove whatever file was there */
	}
	
	taper_tmpnam(fn1);
	if (fi.compressed == 0) {
	    if (log_level > 2) {
		sprintf(l, "Renaming %s to %s", ff, (restore_mode == RESTORE_VERIFY) ? fn1 : s);
		write_log(l);
	    }
	    my_rename(ff, (restore_mode == RESTORE_VERIFY) ? fn1 : s);/* rename this file  */
	    *ff = 0;				 /* the file doesn't exist anymore */
	}

	if (fi.compressed == 1) {		 /* must uncompress using gzip */
	    sprintf(fn, "%s %s > \"%s\"", DECOMPRESS_PROG, ff, (restore_mode == RESTORE_VERIFY) ? fn1 : s);
	    change_dollar(fn);			 /* fixes so that $ are changed to \$ */
	    sprintf(l, "Externally uncompressing file %s", s);
	    if (log_level > 2) write_log(l);
	    if (system(fn) != 0)			 /* do decompress */
	      write_error_log(l);
	}
	    
	if (fi.compressed==2) {			 /* internal compression */
	    cmp_buf = (UBYTE *) comp_buffer1;
	    uncmp_buf = (UBYTE *) comp_buffer2;
	    if ((fi.act_size > COMPRESS2_BUFFER_SIZE) || (comp_buffer1 == NULL))  {
		if (fi.act_size > COMPRESS2_BUFFER_SIZE) {
		    if (log_level > 3) write_log("R:Compressed file is bigger than buffer - allocating");
		}
		else {
		    if (log_level > 3) write_log("R:Couldn't create buffers earlier");
		}
		if (log_level > 3) write_log("R:Creating memory block to receive compressed file");
		cmp_buf=my_malloc(fi.act_size);
		if (cmp_buf==NULL) {
		    do_exit(ERROR_MEMORY);
		    my_free(cmp_buf);
		    goto next_file;
		}
		uncmp_buf=my_malloc(fi.size+COMPRESS_OVERRUN);
		if (uncmp_buf==NULL) {
		    my_free(cmp_buf);
		    do_exit(ERROR_MEMORY);
		    goto next_file;
		}
	    }
	    strcpy(l, "R:Reading in compressed file");
	    if (log_level > 3) write_log(l);
	    of=open(ff, O_RDONLY);		 /* read file into memory */
	    if (of==-1) {
		write_error_log(l);
		if (cmp_buf != comp_buffer1) my_free(cmp_buf);
		if (uncmp_buf != comp_buffer2) my_free(uncmp_buf);
		goto next_file;
	    }
	    read(of, cmp_buf, fi.act_size);	 /* read in all file */
	    close(of);
	    
	    compress(COMPRESS_ACTION_DECOMPRESS, cbuf,   /* decompress */
		     cmp_buf, fi.act_size, uncmp_buf, &xx);
	    sprintf(l, "R:Creating & writing out compressed file %s", 
		   (restore_mode == RESTORE_VERIFY) ? fn1 : s);
	    if (log_level > 3) write_log(l);
	    of=creat((restore_mode == RESTORE_VERIFY) ? fn1 : s, S_IREAD|S_IWRITE);/* write it out */
	    if (of==-1) {
		if (cmp_buf != comp_buffer1) my_free(cmp_buf);
		if (uncmp_buf != comp_buffer2) my_free(uncmp_buf);
		write_error_log(l);
		goto next_file;
	    }
	    x = write(of, uncmp_buf, fi.size);	 /* write uncompressed file */
	    close(of);
	    if (cmp_buf != comp_buffer1) my_free(cmp_buf);
	    if (uncmp_buf != comp_buffer2) my_free(uncmp_buf);
	}
	
	if (fi.compressed == 3) {
	    sprintf(l, "R:Internally unzipping file %s", s);
	    if (log_level > 3) write_log(l);
	    ifd=open(ff, O_RDONLY);
	    if (ifd==-1) {
		sprintf(l, "R:Opening %s", ff);
		write_error_log(l);
		goto next_file;
	    }
	    of=creat((restore_mode == RESTORE_VERIFY) ? fn1:s, S_IREAD|S_IWRITE);/* write it out */
	    if (of==-1) {
		sprintf(l, "R:Opening %s", s);
		close(ifd);
		write_error_log(l);
		goto next_file;
	    }
	    if (unzip(ifd, of) != 0) {
		sprintf(l, "R:Error internally unzipping %s", s);
		write_warning_log(l);
	    }
	    close(ifd); close(of);
	}

	if (restore_mode == RESTORE_VERIFY) {
	    sprintf(l, "R:Trying to open %s", s);
	    if (log_level > 3) write_log(l);
	    ifd = open(s, O_RDONLY);
	    if (ifd == -1) {
		sprintf(l, "%s has gone from disk - not compared", s);
		write_warning_log(l);
		goto next_file;
	    }
	    sprintf(l, "R:Trying to open %s to compare it with", fn1);
	    if (log_level > 3) write_log(l);
	    of = open(fn1, O_RDONLY);
	    if (of == -1) {
		close(ifd);
		sprintf(l, "opening %s", s);
		write_error_log(l);
		goto next_file;
	    }
	    fstat(ifd, &ibuf); fstat(of, &obuf);
	    if (ibuf.st_size != obuf.st_size) {
		sprintf(l, "ERROR: File sizes of %s do not agree: tape=%d, disk=%d", s, (int) obuf.st_size, (int) ibuf.st_size);
		write_log(l);
		log_errors++;
	    }
	    totread=0;
	    toread = min(ibuf.st_size, obuf.st_size);
	    while (totread < toread) {
		red = read(ifd, tr_buffer, min(max_tr_size, toread - totread));
		if (red == -1) {
		    sprintf(l, "reading from disk file %s", s);
		    write_error_log(l);
		    break;
		}
		red = read(of, tr_buffer1, min(max_tr_size, toread - totread));
		if (red == -1) {
		    sprintf(l, "reading from file %s", fn1);
		    write_error_log(l);
		    break;
		}
		if (memcmp(tr_buffer, tr_buffer1, min(max_tr_size, toread - totread))) {
		    sprintf(l, "ERROR: File %s differs between tape & disk", s);
		    write_log(l);
		    log_errors++;
		    break;
		}
		totread += red;
	    }
	    if (totread == toread) {
		sprintf(l, "File %s is the same on disk & tape", s);
		if (log_level > 1) write_log(l);
	    }
	    close(ifd); close(of); unlink(fn1);
	}
	else {
	    sprintf(l, "R:Read in %s successfully", s);
	    if (log_level > 1) write_log(l);
	    setowners(s, 0, &fi);
	}
	
	next_file:
	  if (*ff) unlink(ff);			 /* remove temporary file */
    }						 /* continue loooping */
    
    fin:;
    if (fdfifo != NULL) fclose(fdfifo);
    unlink(fifo_name);
    if (log_level > 3) write_log("R:Restore child about to finish");
    if (lf) close(lf);
    free_comp_buffers();
    my_free(tr_buffer);
    my_free(tr_buffer1);
    my_free_all();
#ifdef TRIPLE_BUFFER    
    if (shm)
      shmdt((char *) shm);
#else
    my_free(shm);
#endif    
    return 0;
}
