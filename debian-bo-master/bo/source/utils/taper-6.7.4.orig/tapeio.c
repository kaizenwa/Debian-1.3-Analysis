/*
   Time-stamp: <96/08/05 19:49:31 yusuf>

   $Id: tapeio.c,v 1.17 1996/08/05 19:02:02 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: tapeio.c,v 1.17 1996/08/05 19:02:02 yusuf Exp $";
#endif /* lint */



#include "taper.h"

char pipetoc[MAX_FNAME];
FILE *fd_pipetoc=NULL;
char pipefc[MAX_FNAME];
FILE *fd_pipefc=NULL;

void paint_main(void);		       /* in taper.c */
size_t raw_read(int fd, char *data, size_t count)
{
/* Does a read from the tape */
    
    size_t x, x1, rc;
    int  serr;
    char *m;
    
    rc = 0;
    while (1) {
	x = read(fd, data+rc, count-rc);
	serr = errno;
	if (x != -1) x += rc;			 /* adjust for already got */
	if (x == count) return x;		 /* got all data */
	if (x == -1) {				 /* error reading */
	    if (errno == ENOSPC) return 0;	 /* end of tape */
	    if (errno == ENODATA) return 0;	 /* no data = end of tape */
	    if (retryabort("reading from tape") == -1) {
		errno = serr;
		return -1;
	    }
	    continue;
	}
/*  We have a short read: two possibilites
     a. end of tape
     b. i/o error

    plan - read another block into temp buffer 'm' and see what error returns
      if for some reason, data is succesfully got, then the amount that
      was short read above is placed in the read buffer.

      eg. block_size = 10,000, count = 10,000 : above returned 9,000
       now attempt to read another 10,0000 (can only read in block_size)
        and get 10,000 bytes.

       the first 1,000 bytes of this 2nd read is put in the read buffer
       the other 9,000 bytes are discarded.

      This is a very unusual situation and, basically, shouldn't occur
*/						 
	rc = x;					 /* rc = how much got */
	m = alloca(block_size);
	while (rc < count) {
	  x1 = read(fd, m, block_size);
	  if (x1 == 0) return rc;			 /* must assume end of tape */
	  if (x1 == -1) {			 /* good - error condition */
	    if ((errno == ENOSPC) || (errno == ENODATA)) /* correct - end of tape */
	      return rc;
	    if (retryabort("reading from tape") == -1)  { /* a real error */
	      errno = serr;
	      return -1;
	    }
	  }
	  /* we actually got some data -?why * - anyway, put in buffer */
	  memcpy(data+rc, m, min(x1, count-rc));
	  rc += x1; 
	}
	if (rc >= count) return count;		 /* got all data */
    }
    return -1;					 /* Should never get here */
}


size_t raw_write(int fd, char *data, size_t count)
{
/* Does a write to the tape 
 
 * Returns number of bytes written : if less than count, assume end of tape
 *  If returns -number of bytes written, couldn't write any more
 *     bytes due to I/O error (ie. return value of -7 indicates than 7
			       bytes written and then I/O error)
 * If couldn't write any bytes due to I/O error, returns -(count+1)
 */
    
    size_t x, x1, rc;
    
    rc = 0;
    while (1) {
	x = write(fd, data+rc, count-rc);
	if (x != -1) x += rc;			 /* adjust for already got */
	if (x == count) return x;		 /* got all data */
	if (x == -1) {				 /* error writing */
	    if (errno == ENOSPC) return 0;	 /* was no space */
	    return -(count+1);
	}
/*  We have a short write : two possibilites
     a. end of tape
     b. i/o error

    plan - write another block beginning where above left off
      if for some reason, data is succesfully written, then
      we are stuffed and totally out of sync. But this should
      never occur  (unless of course, nothing could be written above)
*/						 
	rc = x;
	while (rc < count) {
	  x1 = write(fd, data+rc, block_size);	 /* write a block */
	  if (x1 == 0) return rc;		 /* must assume end of tape */
	  if (x1 == -1) {			 /* good - error condition */
	    if ((errno == ENOSPC) || (errno == ENODATA)) /* correct - end of tape */
	      return rc;			 /* correct - end of tape */
	    if (rc == 0) return -(count+1);	 /*  none written  */
	    return -rc;				 /* real error - wrote rc bytes */
	  }
	  rc += x1; 
	}
	if (rc >= count) return count;		 /* got all data */
    }
    return -(count+1);				 /* Should never get here */
}


_errstat is_regfile(int d)
{
/* Returns 1 if dv is the file handle of a regular file */
    struct stat sb;
    
    fstat(d, &sb);
    return S_ISREG(sb.st_mode);
}


_errstat tape_eom(int allowerr)
{
/* If allowerr = 1, errors allowed */    
    struct mtop m;
    _errstat x;
    
    m.mt_op = MTEOM;
    m.mt_count = 1;

/* When doing eom, must flush buffers */    
    if (write_offset != -1) {
	flush_buffers();
	wait_finish_writing();
    }
    read_offset = -1;				 /* discard rest of block */
    if (log_level > 2) write_log("Going to eom");
    blocks_passed = 0;
    x = (is_regfile(dv)) ? lseek(dv, 0L, SEEK_END) : 
                              ioctl(dv, MTIOCTOP, (char *) &m);
    if ((x == -1) && (!allowerr)) 		 /* only do error */
      do_exit(ERROR_TAPE_EOM);			 /* if errors not allowed */
    return x;
}


_errstat tape_fsf(int count, int allowerr)
{
/* If allowerr = 1, errors allowed */    
    struct mtop m;
    char   l[200];
    _errstat x;
    
    m.mt_op = MTFSF;
    m.mt_count = count;

/* When doing fsf, must flush buffers */    
    if (write_offset != -1) {
	flush_buffers();
	wait_finish_writing();
    }
    read_offset = -1;				 /* discard rest of block */
    sprintf(l, "Advancing %d volumes", count);
    if (log_level > 2) write_log(l);
    if (!have_fsf) {
	if (log_level) {
	    write_log("ERROR: You tape drive doesn't have fsf");
	    log_errors++;
	}
	return do_exit(ERROR_NO_FSF);		 /* can't fsf */
    }
    blocks_passed = 0;
    x = (is_regfile(dv)) ? lseek(dv, 0L, SEEK_END) : 
                              ioctl(dv, MTIOCTOP, (char *) &m);
    if ((x == -1) && (!allowerr)) 		 /* only do error */
      do_exit(ERROR_TAPE_FSF);			 /* if errors not allowed */
    return x;
}


_errstat tape_seek(int toblock)
{
    struct mtop m;
    struct mtget mg;
    _s32 tb;
    char s[200];
    
    if (!can_seek) return -1;			 /* can't seek */
    if (is_regfile(dv)) {
	if (log_level > 2) write_log("File seek to correct block");
	if (toblock < blocks_passed) return 0;
	lseek(dv, block_size*(toblock-blocks_passed), SEEK_CUR);
	read_offset = -1;
	blocks_passed = toblock;
	return 0;
    }
    if (toblock-blocks_passed < min_seek) {
      sprintf(s, "Only need to pass %d blocks - leaving to stream",
	      toblock-blocks_passed);
      if (log_level > 2) write_log(s);
      return -1; /* continue streaming */
    }
    
    read_offset = -1;
    if (log_level > 2) write_log("ioctl get info");
    memset(&mg, 0, sizeof(mg));
    if (ioctl(dv, MTIOCGET, (char *) &mg) == -1)
      tb = toblock;				 /* if tape drive doesn't */
    else					 /* support this - assume that */
      tb = mg.mt_blkno;				 /* first block is 0 */
    if (log_level > 2) write_log("ioctl seek");
    m.mt_op = MTSEEK;
    m.mt_count = toblock+(tb-blocks_passed);
    if (tb-blocks_passed) {
      sprintf(s, "seek is adjusting %d", tb-blocks_passed);
      if (log_level > 2) write_log(s);
    }
    if (ioctl(dv, MTIOCTOP, (char *) &m) != -1) {
	blocks_passed = toblock;
	return 0;
    }
    return -1;
}


_errstat tape_goto_block(WINDOW *mes, int line, _s32 block)
{
/* Positions tape at block 'block'
 * 
 * If the position is backwards, does nothing
 * Corrupts the read buffer
 * 
 * Returns 0 if OK, -1 if error
*/
    _s32 c;
    char l[400];
    
    if (block < blocks_passed) return 0;
    c = blocks_passed;
    sprintf(l, "Seeking block %d", block);
    if (log_level > 2) write_log(l);
    if (mes) status_box(mes, l, line,  FALSE, 1);
    if (tape_seek(block) != -1) return 0;
    if (log_level > 2) write_log("Couln't ioctl seek - trying manually");
    read_offset = -1;
    while (c < block) {
	if (raw_read(dv, read_buffer, block_size) != block_size)
	  return do_exit(ERROR_TAPE_GOTO);
	blocks_passed++;
	c++;
    }
    return 0;
}


_errstat tape_read_u16(_u16 *x)
{
/* Reads an  unsigned 16 bit integer from the tape
 * 
 * Takes into account big/little endian 
 * 
 * Returns -1 if error, 0 otherwise
 */
    char s[sizeof(_u16)];
    
    if (tape_read(s, sizeof(s)) != sizeof(s)) return -1;
    *x = little2machu16((_u16 *) s);
    return 0;
}


_errstat tape_read_s32(_s32 *x)
{
/* Reads a signed 32 bit integer from the tape
 * 
 * Takes into account big/little endian 
 * 
 * Returns -1 if error, 0 otherwise
 */
    char s[sizeof(_s32)];
    
    if (tape_read(s, sizeof(s)) != sizeof(s)) return -1;
    *x = little2machs32((_s32 *) s);
    return 0;
}


_errstat tape_readheader(struct tape_header *t, _s8 allowz)
{
/* Reads the tape header. It assumes that the tape is at the
 * beginning and no data has been read in as yet.
 * 
 * Any leading zeroes are ignored. If there is a whole block of
 * zeroes (actually, the last sizeof(struct tape_header) bytes
 *	   are not checked), this is assumed to be an error.
 * 
 * We also assume that a whole block must be read in - if this
 * is the tape header, we can't be at the end of the tape so
 * we won't accept partial block reads
 * 
 * If allowz is 1, then if it can't read any bytes from the
 * tape, it returns (setting the tape_header structure to 0)
 * otherwise, this is an error

 * Returns 0 if read OK, -1 if error
 */
    char s[100];
    char *m;
    struct mtop mt;
    _errstat done_it=0;
    
    while (1) {
	memset(t, 0, sizeof(*t));
	if (log_level > 2) write_log("Reading in block for tape header");
	read_buffer_count = raw_read(dv, read_buffer, block_size);
	blocks_passed=1;
	if (read_buffer_count != block_size) {	 /* read in a block */
	    if (tape_type == TAPE_TYPE_ZFTAPE) { /* zftape has a problem sometimes, so read again to double check */
		if (log_level > 2) write_log("Invalid read count - trying to re-set block size");
		if ((read_buffer_count == -1) && (errno == EINVAL)) {   /* correct block size initially */
		    if (tape_get_blk_size(dv) == -1) return (allowz) ? 0 : -1;
		    if (log_level > 2) write_log("Rereading tape header block");
		    read_buffer_count = raw_read(dv, read_buffer, block_size);
		    if (read_buffer_count != block_size) {
			if (allowz) return 0;	 /* errors allowed */
			write_fatal_log("reading in tape header block");
			return -1;
		    }
		    goto success_read;
		}
		if (allowz) return 0;		 /* allowed to have nothing */
	    }
	    else {
		if (allowz) return 0;		 /* errors allowed */
		write_fatal_log("reading in tape header block");
		return -1;
	    }
	}
	success_read:;
	read_offset = 0;
	
	if (log_level > 2) write_log("Skipping leading zeroes");
	while ((*(read_buffer+read_offset) == 0) && 
	       (read_offset < (block_size-sizeof(struct tape_header))))
	  read_offset++;				 /* look for first non-zero byte */
	if (read_offset == block_size-sizeof(struct tape_header))  {
	    if (allowz) return 0;			 /* allowed to have nothing */
	    if (log_level) {
		write_log("Too many leading zeroes while reading in tape header");
		log_errors++;
	    }
	    return -1;				 /* error - full block of zeroes */
	}
	if (log_level > 2) {
	    sprintf(s, "Found %d leading zeroes", read_offset);
	    write_log(s);
	}
	*t = *((struct tape_header *) (read_buffer+read_offset));
	tape_header_endianize2mach(t);
	if (log_level > 2) {
	    sprintf(s, "Read in tape header %x %u %d", t->magic, t->archive_id, t->tape_number);
	    write_log(s);
	}
	read_offset += sizeof(struct tape_header);
	if ((t->magic == TAPER_MAGIC_NUMBER) || 
	    (t->magic == TAPER_4_MAGIC_NUMBER) ||
	    (t->magic == TAPER_64_MAGIC)) 
	  break;				 /* found magics - must be OK */
	if (tape_type == TAPE_TYPE_ZFTAPE) {	 /* zftape sometimes misses first read so try again */
	    if (!is_regfile(dv) && !done_it) {	 /* only if using tape drive */
		mt.mt_op = MTREW;		 /* try reading more than one segment  */
		mt.mt_count = 1;
		ioctl(dv, MTIOCTOP, (char *) &mt);   /* rewind */
		m = malloc((28672/block_size+1)*block_size);
		if (m == NULL)
		  do_exit(ERROR_MEMORY);
		sprintf(s, "Special zftape. Reading %d, Read %ld",((28672/block_size+1)*block_size),
			(long) raw_read(dv, m, (28672/block_size+1)*block_size));   /* read more than 28K */
		if (log_level > 2) write_log(s);
		free(m);
		mt.mt_op = MTREW;
		mt.mt_count = 1;
		sprintf(s, "Special zftape. Rewinding %d",/* rewind */
			ioctl (dv, MTIOCTOP, (char *) &mt));	
		if (log_level > 2) write_log(s);
		fsync(dv);
		close(dv);			 /* by close */
		if (tape_open(O_RDWR)  == -1) /* and reopen */
		  return -1;
		done_it = 1;			 /* only do it once */
	    }
	    else 
	      break;				 /* already done it or reg file */
	}
	else					 /* not zftape */
	  break;
    }
    return 0;
}

						 
_errstat check_tape(WINDOW *mes, int line, _s32 tape_required)
{
/* Prompts user for tape_required tape on archive in ifd.archive_id
   Also makes sure correct archive id is in tape  
 
   Returns -1 if error or user aborted
   Returns 1 if OK
*/

    char   l[200];
    struct tape_header tdh1;
    WINDOW *mymes=NULL;

    if ( (tdh.archive_id == ifd.archive_id) &&
         (tdh.tape_number == tape_required) ) return 1; /* correct tape already read */
    if (mf) {					 /* doing unattended */
	strcpy(l, "Wrong tape in drive. Backup ABORTED\n");
	write(mf, l, strlen(l));
	return -1;
    }
    tape_close();
    sprintf(l, "Prompting for insertion of tape %d", tape_required);
    if (log_level > 2) write_log(l);
    sprintf(l, "Please insert tape %d of archive %u in tape drive", tape_required, ifd.archive_id);
    if (!message_box(l, MB_OKCANCEL)) {
	if (lf) {
	    write_log("ERROR: User aborted while prompting for new tape");
	    log_errors++;
	}
        return -1;
    }
    paint_main();
    if (mes==NULL) {
	mymes = status_box(mymes, "                                  ", 1, TRUE, 1);
	line = 1;
    }
    else
      mymes = mes;
    while (1) {
	touchwin(mymes);
	status_box(mymes, "Identifying tape", line, FALSE, 1);
	tape_close();
	sprintf(l, "Opening tape to ensure that tape #%d of archive %d is in drive", 
		tape_required, ifd.archive_id);
	if (log_level > 2) write_log(l);
	if (tape_rewind() == -1) return -1;
	if (tape_open(O_RDWR) == -1) 
	    continue;
	vols_passed = 0;			 /* no volumes passed on this tape */
	tdh1.magic = 0;
	tape_readheader(&tdh1, 0);
	if ((tdh1.magic != TAPER_MAGIC_NUMBER) && (tdh1.magic != TAPER_64_MAGIC)) {
	    if (!message_box("This tape doesn't contain taper data", MB_OKCANCEL))
		return -1;
	    paint_main();
	    continue;
	}
	if (tdh1.archive_id != ifd.archive_id) {
	    sprintf(l, "This tape isn't part of archive %d", ifd.archive_id);
	    if (!message_box(l, MB_OKCANCEL))
	      return -1;
	    paint_main();
	    continue;
	}
	if (tdh1.tape_number != tape_required) {
	    sprintf(l, "This is tape %d. I require %d", tdh1.tape_number, tape_required);
	    if (!message_box(l, MB_OKCANCEL))
	      return -1;
	    paint_main();
	    continue;
	}
	break;
    }
    if (mes==NULL) {
	close_statusbox(mymes);
	paint_main();
    }
    else
      status_box(mes, "", line, FALSE, 1);
    tdh = tdh1;
    return 1;
}



_errstat tape_rewind()
{
/* Rewinds tape. Tape must be closed prior to calling of rewind.
 * Returns 0 if rewound, -1 if error

   If allow_err = 1, then no error message is printed (error code still returned)
 * 
 * It doesn't use tape_open/tape_close to avoid buffering problems
*/
    _errstat err=0, myerrno, x;
    struct mtop m;
    
    m.mt_count = 1;
    m.mt_op = MTREW;
    
    if (!have_rewind) return 0;
    if (log_level > 2) write_log("Rewinding tape");
    if ((x=open(tape, O_RDWR)) == -1)		 /* try and open tape */
      return do_exit(ERROR_OPENING_BACKUP);
    if (!is_regfile(x))
      err = ioctl (x, MTIOCTOP, (char *) &m);	 /* do status if not regular file */
    myerrno = errno;				 /* preserve errno accross close */
    close(x);					 /* close */
    errno = myerrno;
    return (err == -1) ? do_exit(ERROR_REWIND) : 0;
}


void tape_set_blk_size()
{
/* Sets the block size, if required */    
    struct mtop m;
    _errstat err;
    char l[100];

    block_size = org_block_size;
    m.mt_op = MTSETBLK;
    m.mt_count = block_size;

    if (set_blksize || get_blksize) {
	sprintf(l, "Setting block size to %d", block_size);
	if (log_level > 2) write_log(l);
	err = (is_regfile(dv)) ? 0 : ioctl(dv, MTIOCTOP, (char *) &m);
	/*if (err == -1) do_exit(ERROR_SETTING_BLKSIZE); */
	if (init_buffers(1) == -1) do_exit(ERROR_SETTING_BLKSIZE);/* reallocate buffer sizes */
    }
}

_errstat tape_get_blk_size(_s32 x)
{
    struct mtblksz m;
    _errstat err;
    char s[100];
    _s32 obs;

    if (get_blksize) {
	if (log_level > 2) write_log("Getting block size");
	m.mt_blksz = block_size;
	obs = block_size;
	err = (is_regfile(x)) ? 0 : ioctl(x, MTIOC_ZFTAPE_GETBLKSZ, (char *) &m);
	if (err == -1) 
	  do_exit(ERROR_GETTING_BLKSIZE);
	else
	  block_size = m.mt_blksz;
	sprintf(s, "Got block size %d", block_size);
	if (log_level > 2) write_log(s);
	if (block_size == 1) {			 /* if got block size of 1, means that */
	    block_size = obs;			 /* block size hasn't been set for this */
	    tape_set_blk_size();		 /* volume - so set it */
	}
	else
	  if (init_buffers(1) == -1) return -1;	 /* reallocate buffer sizes */
    }
    return 0;
}


_errstat tape_erase()
{
    struct mtop m;
    _errstat err;

    tape_set_blk_size();
    if (!erase_tape) {
	if (log_level > 2) write_log("Not erasing tape");
	return 0;					 /* don't need to erase tape */
    }

    m.mt_op = MTERASE;
    m.mt_count = 1;
    
    if (log_level > 2) write_log("Erasing tape");
    err = (is_regfile(dv)) ? 0 : ioctl(dv, MTIOCTOP, (char *) &m);
    if (err == -1) return do_exit(ERROR_ERASING_TAPE);
    return 0;
}

void clear_buffer_pointers(void)
{
    left_tr = 0;
    read_offset = -1; 
    read_buffer_count = 0;
    write_offset = 0;
    w_cur_pos = 0;
    w_current_buffer = w_buffer_1;
    write_buf_no = NULL;
}


_errstat mount_device(void)
{
/* If a mounted type, tries to mount the device.
 * 
 * Returns -1 if error, 0 otherwise */
    if (tape_type != TAPE_TYPE_REMOVABLE) return 0;
    return 0;
}


_errstat ummount_device(void)
{
/* Tries to umount device */
    if (tape_type != TAPE_TYPE_REMOVABLE) return 0;
    return 0;
}


_errstat ntape_open(int mode)
{
/* Opens non-rewinding device. 
 
   If allower == 1, allows errors, otherwise error message */
    char l[200];

    if (!have_rewind) return tape_open(mode);
    if (mount_device() == -1) return -1;
    sprintf(l, "Opening non-rewinding %s", ntape);
    if (log_level > 2) write_log(l);
    clear_buffer_pointers();
    dv = open(ntape, mode);
    blocks_passed = 0;
    if (dv == -1) 
	return do_exit(ERROR_OPENING_BACKUP);
    if (tape_get_blk_size(dv) == -1) return -1;
    return dv;
}


_errstat tape_open_engine(int mode, int rezero)
{
/* Opens rewinding device */
    char l[200];

    if (mount_device() == -1) return -1;
    sprintf(l, "Opening rewinding %s", tape);
    if (log_level > 2) write_log(l);
    if (rezero) clear_buffer_pointers();	 /* only if user wants us to */
    dv = open(tape, mode);
    blocks_passed = 0;
    if (dv == -1) 
	return do_exit(ERROR_OPENING_BACKUP);
    if (tape_get_blk_size(dv) == -1) return -1;
    return dv;
}


_errstat tape_open(int mode)
{
/* Opens rewinding device */
    return tape_open_engine(mode, 1);
}


_errstat tape_close_engine(_s8 flush)
{
    _errstat ret;

    /* closes tape device */
    if (dv==-1) return 0;			 /* not open */
    if (flush) {
	flush_buffers();
	wait_finish_writing();
    }
    if (write_pid) {
	kill(write_pid, SIGTERM);
	if (log_level > 3) write_log("Close - killed child done");
	waitpid(write_pid, NULL, 0);
#ifdef TRIPLE_BUFFER	
	fclose(fd_pipetoc); fclose(fd_pipefc);
	unlink(pipetoc); unlink(pipefc);
	signal(SIGPIPE, SIG_DFL);
#endif	
	if (log_level > 3) write_log("Close - waitpid child done");
	write_pid = 0;
    }
    fsync(dv);
    if (log_level > 2) write_log("Closing tape");
    ret = (dv) ? close(dv) : 0;
    dv = -1;
    if (ummount_device() == -1) return -1;
    return ret;
}


_errstat tape_close()
{
    return tape_close_engine(1);
}


int read_from_buf(char *buf, size_t count)
{
    size_t tr_bytes, fit_in;
    
    bytes_short = block_size;
    if (read_offset == -1) {			 /* nothing in buffer */
	if (log_level > 2) write_log("Reading in block from tape");
	if (tape_size) {			 /* ?end of tape */
	    if (blocks_passed * block_size > tape_size * 1048576) {
		if (log_level > 2) write_log("Forced end of tape");
		return 0;
	    }
	}
	read_buffer_count = raw_read(dv, read_buffer, block_size);   /* read in a block */
	if (read_buffer_count == -1) {
	    if (log_level) write_log("reading data from tape");
	    return -1;
	}
	blocks_passed++;
	read_offset = 0;			 /* set offset to zero */
    }
    if (read_buffer_count == 0) return 0;	 /* no more data in buffer */
    tr_bytes = 0;				 /* either now or previously */
    while (tr_bytes < count) {			 /* transfer bytes to user buffer */
	if (read_offset >= read_buffer_count) {	 /* run out of bytes in buffer */
	    if (read_buffer_count < block_size)	{ /* means that last time, didn't get all that */
		bytes_short = block_size - read_buffer_count;
		return tr_bytes;			 /* we wanted - ie. end of tape */
	    }
	    if (log_level > 2) write_log("Reading in block from tape");
	    if (tape_size) {				 /* ?end of tape */
		if (blocks_passed * block_size > tape_size * 1048576) {
		    if (log_level > 2) write_log("Forced end of tape");
		    return tr_bytes;
		}
	    }
	    read_buffer_count = raw_read(dv, read_buffer, block_size);
	    if (read_buffer_count == -1) {
		if (log_level) write_log("reading data from tape");
		return -1;
	    }
	    blocks_passed++;
	    if (read_buffer_count < 1) return tr_bytes;   /* if couldn't get any more, accept what we got */
	    read_offset = 0;
	}
	fit_in = min(count-tr_bytes, read_buffer_count-read_offset);
	memcpy(buf+tr_bytes, read_buffer+read_offset, fit_in);
	tr_bytes += fit_in;
	read_offset += fit_in;
    }
    return tr_bytes;
}


_errstat write_tape_header(struct tape_header *tdh)
{
/* Writes the tape header. Assumes that the tape is at the 
 * beginning. Also assumes that there may be valid data waiting
 * to be written in the write buffer
 * 
 * Because we should be at the beginning of the tape, incomplete
 * writes are not tolerated
 * 
 * Returns 0 if OK, -1 if error
 */
    char *my_write_buffer=NULL;
    _s32  oldwo;
    char  s[500];
    
    my_write_buffer = my_malloc(block_size);
    if (my_write_buffer == NULL)
      do_exit(ERROR_MEMORY);
    if (log_level > 2) {
	sprintf(s, "Writing tape header %x %u %d", tdh->magic, tdh->archive_id, tdh->tape_number);
	write_log(s);
    }
    ifd.number_tapes++;
    if (write_offset + sizeof(struct tape_header) < block_size) {
	if (log_level > 2) write_log("Just adding tape header to front of write buffer");
	memcpy(my_write_buffer+sizeof(struct tape_header),   /* room in buffer */
	       write_buffer, write_offset);	 /* for the tape header */
	*(struct tape_header *) my_write_buffer = *tdh;
	tape_header_endianize2little((struct tape_header *) my_write_buffer);
	write_offset += sizeof(struct tape_header);
	memcpy(write_buffer, my_write_buffer, write_offset);
	my_free(my_write_buffer);
	return 0;
    }

/* No room in write buffer for tape header - therefore, write out
 * the tape header plus what we can of write buffer and then remove
 * what's been written from write buffer
 */
    if (log_level > 2) write_log("No room in buffer for tape header");
    *(struct tape_header *) my_write_buffer = *tdh;
    tape_header_endianize2little((struct tape_header *) my_write_buffer);
    memcpy(my_write_buffer+sizeof(struct tape_header),
	   write_buffer, block_size - sizeof(struct tape_header));
    while (1) {
	if (raw_write(dv, my_write_buffer, block_size) < block_size) {
	  if (retryabort("writing tape header to tape") == -1) {
	      write_fatal_log("writing tape header block");
	      my_free(my_write_buffer);
	      return -1;
	  }
	}
	else
	  break;
    }
    blocks_passed++;
    oldwo = write_offset;
    write_offset -= (block_size - sizeof(struct tape_header));
    if (write_offset)
      memcpy(write_buffer, write_buffer+oldwo-write_offset, write_offset);
    my_free(my_write_buffer);
    return 0;
}




_errstat do_write_block(_s8 *buf_to_write, _s32 old_length, _s8 fc)
{
/* Writes the buffer at buf_to_write of old_length out to
 * the tape. If fc, then will write out the last partial
 * block, otherwise buffers it
 * 
 * If runs out of space, sets left_tr to how many bytes it wrote
 * and returns -2
 * 
 * Returns -1 if error, 0 otherwise. 
 
 */
    size_t c, c1;
    _s32  tr;
    char  s[100];
    _errstat x;
    
    tr = 0;					 /* transferred 0 */
    w_current_writing = buf_to_write;
    if (old_length < 0) old_length = 0;
    while (tr <= old_length) {
	if (old_length-tr+write_offset < block_size) {
	    memcpy(write_buffer+write_offset, buf_to_write+tr,   /* copy remaining */
		   old_length-tr);   /* data to write buffer */
	    memset(write_buffer+old_length-tr+write_offset, 0, block_size-
		   (old_length-tr+write_offset));   /* pad out rest with zeroes */
	    if (fc) {				 
		if (log_level > 3) 		 /* flushing for close */
		  write_log("W: half block being written out");
	    }
	    else {				 /* not flushing */
		write_offset += (old_length-tr);
		if (log_level > 3) {
		    sprintf(s, "W: half block buffered %d", write_offset);
		    write_log(s);
		}
		left_tr = 0;
		return 0;			 /* not tripleb buffering */
	    }
	}
	else {
	    memcpy(write_buffer+write_offset, buf_to_write+tr, 
		   block_size-write_offset);
	}
	tr += (block_size - write_offset);
	
	if (log_level > 3) {
	    sprintf(s, "W: writing block %d. Write offset %d.", tr, write_offset);
	    write_log(s);
	}
	if (tape_size) 				 /* see if user has specified tape size */
	    if (blocks_passed*block_size > tape_size * 1048576) {
		if (log_level > 2) write_log("Forced end of tape");
		c = 0;
		bytes_short = block_size;
		goto pr;
	    }

	c = raw_write(dv, write_buffer, block_size);/* write data */
	if (log_level > 2) {
	    sprintf(s, "Wrote block to tape [%ld bytes]", (long) c);
	    write_log(s);
	}

	pr:;
	blocks_passed++;
	if (c == block_size) {			 /* wrote whole block successfully */
	    write_offset = 0;			 /* nothing left in write buffer */
	    continue;
	}	
	if (c < 0) {				 /* error */
	    blocks_passed--;		
	    x = -1;				 /* signify I/O error */
	    c = (c == (-block_size+1)) ? 0 : -c; /* convert to actual # bytes written */
	    sprintf(s, "W: Error %d doing writing. dv = %d. Wrote %d bytes", errno, dv, -c);
	    if (log_level > 3) write_log(s);
/* This is because blocks_passed will be stuffed up with partial block writes 
   Is OK if none of the block was written since it will be back in sync,
 * but if only part of the block was, we'll be in trouble  */
	    if ((log_level) & !c) 
		write_warning_log("Write child encountered an error - you must recreate info file after this backup");
	}
	else {
	    x = -2;					 /* end of space */
	    if (log_level > 3) 
	      write_log("W: End of tape encountered");
	}

/* Couldn't write all of block - either because end of tape (x==-2)
 * or I/O error (x==-1) */
	bytes_short = block_size - c;
	for (c1=0; c1<c; c1++)			 /* move data that wasn't written to  */
	  write_buffer[c1] = write_buffer[c1+c];/* beginning of buffer */
	write_offset = block_size - c;
	left_tr = tr;				 /* tell parent how much is left */
	if (log_level > 3) {
	    sprintf(s, "W: Short write - left_tr = %d", left_tr);
	    write_log(s);
	}
	return x;				 /* tell parent couldn't write it all*/
    }
    left_tr = 0;
    if (log_level > 3) write_log("W: finished writing & exiting");
    return 0;
}



_errstat do_next_tape(void)
{
/* The child ran out of space while trying to write. Must
 * be end of tape. Get new tape and write out what's left
 * in the buffer. Returns -1 if error, 0 otherwise
 */
    WINDOW *mes=NULL;
    _s8 *buf_to_write;
    struct tape_header tdh1;
    _s32 c;
    char  *my_wbuf=NULL;
    char l[200];
    
    if (log_level > 2) write_log("End of tape reached while writing.");
    ifd.number_tsi++;
    tsi = my_realloc(tsi, ifd.number_tsi*sizeof(struct tape_size_info));
    if (tsi == NULL) return do_exit(ERROR_MEMORY);
    tsi[ifd.number_tsi-1].tape_number = ifd.number_tapes;   /* update tsi information */
    tsi[ifd.number_tsi-1].volume = ifd.number_volumes;
    tsi[ifd.number_tsi-1].blocks = blocks_passed;   
    tsi[ifd.number_tsi-1].lb_bytes_short = bytes_short;
    tape_close_engine(0);
    buf_to_write = w_current_writing;		 /* buffer child was writing to */
    my_wbuf = my_malloc(block_size);
    if (my_wbuf == NULL) return do_exit(ERROR_MEMORY);
    memcpy(my_wbuf, write_buffer, block_size);	 /* save contents of write buffer */
    mes = status_box(mes, "Identifying tape", 1, TRUE, 1);
    while (1) {
	c = message_box("Insert next tape", MB_OKCANCEL); paint_main();
	if (c == 0) {
	    if (log_level) {
		write_log("User aborted asking for next tape");
		log_errors++;
	    }
	    c = -1;
	    goto end;
	}
	if (mes) {
	    touchwin(mes); wrefresh(mes);
	    status_box(mes, "Identifying tape", 1, FALSE, 1);
	}
	if (tape_rewind() == -1) 
	    continue;
	if (tape_open_engine(O_RDWR, 0) == -1) 
	    continue;
	tdh1.magic = 0;
	tape_readheader(&tdh1, 1);
	tape_close_engine(0);
	if ((tdh1.magic == TAPER_MAGIC_NUMBER) || (tdh1.magic == TAPER_64_MAGIC)) {
	    if (tdh1.archive_id == 0) 
	      break;
	    else
	      if (message_box("This is tape contains taper data. Confirm overwrite", MB_YESNO))
	        break;
	}
	else {					 /* freshly formatted taper tape */
	    if (message_box("Unknown tape data. Confirm overwrite", MB_YESNO))
	      break;
	}
    }

    touchwin(mes);
    mes = status_box(mes, "Erasing tape", 1, FALSE, 1);   
    if (tape_open_engine(O_RDWR, 0) == -1) {
	write_error_log("Error opening new tape");
	c=-1; 
	goto end;
    }
    tape_erase();				 /* erase tape */
    status_box(mes, "Rewinding tape & writing tape header", 1, FALSE, 1);   
    tape_close_engine(0);
	    
    if (log_level > 2) write_log("Opening new tape for data writing");
    if (tape_open_engine(O_RDWR, 0) == -1) {
	write_error_log("Error opening new tape");
	c=-1; 
	goto end;
    }
    
    tape_set_blk_size();
    tdh.tape_number++;
    memcpy(write_buffer, my_wbuf, block_size);	 /* restore contents of write buffer */
    if (write_tape_header(&tdh) == -1)  {
	c=-1;
	goto end;
    }

    status_box(mes, "Writing data to new tape", 1, FALSE, 1);
    sprintf(l, "Writing rest of buffer %d to new tape",
	    ((buf_to_write == w_buffer_1) ? 1 : 2));
    if (log_level > 2) write_log(l);
    c=do_write_block(buf_to_write+left_tr, 
		     ((buf_to_write == w_buffer_1) ? buf_length_1 : buf_length_2) - left_tr,
		     ((buf_to_write == w_buffer_1) ? for_close_1 : for_close_2));
    
    end:;
      if (my_wbuf) my_free(my_wbuf);
      close_statusbox(mes);
      touchwin(win_main); wrefresh(win_main);
      return (c<0) ? -1 : 0;
}


void child_term(int sig)
{
    if (dv) close(dv);
    free_buffers();
#ifdef TRIPLE_BUFFER    
    fclose(fd_pipetoc); fclose(fd_pipefc);
#endif    
    exit(0);
}

void child_segv(int sig)
{
    write_log("W: Child Segmentation fault");
#ifdef TRIPLE_BUFFER    
    fprintf(fd_pipefc, "Error\n"); fflush(fd_pipefc);
#endif    
    write_buf_no = NULL;
    child_term(0);
}


void do_write_child(void)
{
/* This is the child process that runs in the background. It 
 * sleeps until it write_buf_no becomes non-null. It then
 * assumes that write_buf_no contains the address of the
 * buffer to write, buf_length is how much is in that buffer
 * and for_close tells whether this is done prior to a close
 * operation or not.
 * 
 * The child has to exit if runs out of space since parent
 * needs to be able to close the backup device which it
 * can't do if the child has it open. The child could
 * close it, but then it needs to open the new device
 * and it won't know when/how etc..
 * 
 * If an error occurs, write_buf_no == -1, and the child is
 * killed
 * 
*/
    char s[100];
    _errstat  ret;

    signal(SIGTERM, child_term);
    signal(SIGSEGV, child_segv);
    my_free_all();				 /* don't need any of these */
    free(read_buffer); 
    read_buffer = NULL;
    free(tr_buffer);
    tr_buffer = NULL;
    win_main = NULL;
    while (1) {					 /* buffers */
#ifdef TRIPLE_BUFFER	
	if (log_level > 3) write_log("W:Waiting for pipe to say go");
	fgets(s, sizeof(s), fd_pipetoc);	 /* wait until data ready */
#endif	
	if (log_level > 3) {
	    sprintf(s, "W: Child about to write out buffer %d, length %d, close %c", 
		    ((write_buf_no == w_buffer_1) ? 1 : 2),
		    ((write_buf_no == w_buffer_1) ? buf_length_1:buf_length_2),
		    '0'+((write_buf_no == w_buffer_1) ? for_close_1 : for_close_2));
	    write_log(s);
	}
	ret = do_write_block((_s8 *) write_buf_no, 
			     ((write_buf_no == w_buffer_1) ? buf_length_1 : buf_length_2),
			     ((write_buf_no == w_buffer_1) ? for_close_1 : for_close_2) );
	write_buf_no = NULL;			 /* this will ensure that all bytes of write_buf_no are zero */
	write_buf_no = (_vptr) (_s32) ret;	 /* write buf_no are zero */
    	if (ret < 0) {
	    (ret == -1) ? sprintf(s, "W: Child exiting because of error %d", errno) :
	                  sprintf(s, "W: Child exiting because out of space");
	    if (log_level > 3) write_log(s);
	    child_term(0);
	}
#ifdef TRIPLE_BUFFER
	fprintf(fd_pipefc, "Data\n"); fflush(fd_pipefc);/* tell parent we've finished */
#endif	
    }
    
}


_errstat wait_finish_writing()
{
/* Waits for child to finish writing current buffer. If child returns
 * an error, prompts user for abort/retry. If user wants to retry,
 * then reatempts to write the block
 
 * Returns 0 if OK, -1 if user aborted
 * Returns -2 if new tape
 */
    
    int x, serr;
#ifdef TRIPLE_BUFFER
    char s[100];
#endif

    if (!write_pid)				 /* if there is a write child, must clear the message from FIFO */
      if (write_buf_no == NULL) return 0;	 /* nothing pending */
#ifdef TRIPLE_BUFFER    
    if (log_level > 3) write_log("Waiting for child to finish writing block");
    if (write_buf_no != (char *) -3) {		 /* this indicates that child has died */
	fgets(s, sizeof(s), fd_pipefc);		 /* wait until child ready */
	if (!strcmp(s, "Error\n")) {		 /* write child ended in error */
	    err:;
	    write_pid = 0;				 /* this child ended in error */
	    write_offset = 0;			 /* we don't know how much child had written */
	    w_cur_pos = 0;				 /* so must assume backup is corrupt */
	    left_tr = 0;
	    write_buf_no = NULL;
	    return do_exit(ERROR_CHILD_SEGFAULT);
	}
    }
    else
      goto err;
    if (write_buf_no == NULL) return 0;		 /* child finished writing */
    waitpid(write_pid, NULL, WNOHANG);		 /* unzombie this */
    fclose(fd_pipefc); fclose(fd_pipetoc);	 /* remove FIFOs */
    unlink(pipefc); unlink(pipetoc);
    write_pid = 0;
#endif    
    if (write_buf_no == (char *) -2) {		 /* child returned end of tape */
	write_buf_no = NULL;
	if (do_next_tape() == -1) return -1;
	return -2;
    }
/* Child returned -1 (ie. couldn't write)
 * Therefore, we will try to finish the writing */
    while (1) {					 /* try and write out block child missed */
	if (log_level > 2) write_log("Child couldn't write - main trying to");
	x = do_write_block(w_current_writing,	
			   ((w_current_writing == w_buffer_1) ? buf_length_1 : buf_length_2),
			   ((w_current_writing == w_buffer_1) ? for_close_1 : for_close_2));
	if (x == 0) {				 /* successful write */
	    write_buf_no = NULL;
	    return 0;
	}
	if (x == -1) {				 /* error writing out buffer */
	    serr = errno;			 /* save error */
	    if (retryabort("writing to tape") == -1) {
		errno = serr;
		if (log_level) 
		  write_error_log("writing to tape");
		write_buf_no = NULL;		 /* prevent reprocessing this error */
		return -1;
	    }
	}
	if (x == -2) {				 /* end of tape reached while writing block */
	    write_buf_no = NULL;
	    if (do_next_tape() == -1) return -1;
	    return -2;
	}
    }
    return -1;                                  /* Should never get here */
}


void parent_pipe(int sig)
{
/* Called if parent tries to write to non-existent pipe - ie. child
 *  has died */
    write_buf_no = (_vptr) ((_s32) -3);
}


_errstat flush_buffers()
{
/* Flushes the write buffer
 * 
 * An end of tape is assumed if a write returns a count less than 'count'  -- OR --
 * zero bytes written  -- OR --
 * wrote less bytes than we asked for 
 * 
 * Semaphores are used to co-ordinate with child:
 *  the pending semaphore is up (1) when there is NO data pending, 0 when there is
 *  the cf semaphore is down (0) when child is free, 1 when it is not
 * 
 * Returns -1 for error, 0 otherwise
 * 
 */
#ifdef TRIPLE_BUFFER
    pid_t f;
#endif
    int x;
    char s[100];

    if (!dv) return 0;				 /* backup device not open */
    if ((w_cur_pos < 1) && (write_offset == 0) && (left_tr == 0))   /* nothing left to write */
      return 0;

/* ?Are we closing on this buffer */
    if (w_current_buffer == w_buffer_1) {
	for_close_1 = (w_cur_pos == DOUBLE_BUFFER_SIZE) ? 0 : 1;
	if (w_cur_pos < DOUBLE_BUFFER_SIZE)
	  memset(w_current_buffer+w_cur_pos, 0,  DOUBLE_BUFFER_SIZE-w_cur_pos);
    }
    else {
	for_close_2 = (w_cur_pos == DOUBLE_BUFFER_SIZE) ? 0 : 1;
	if (w_cur_pos < DOUBLE_BUFFER_SIZE)
	  memset(w_current_buffer+w_cur_pos, 0,  DOUBLE_BUFFER_SIZE-w_cur_pos);
    }

    x = wait_finish_writing();			 /* wait for child to finish writing */
    if (x == -1) {				 /* it will also take care of end of tapes */
	if (log_level > 2) write_log("wait_finish returned error");
	w_cur_pos = 0; write_offset = 0; left_tr = 0;
	return -1;
    }
      
#ifdef TRIPLE_BUFFER
    if (write_pid == 0) {			 /* there is no child so start one up*/
	write_buf_no = NULL;			 /* tell child nothing to start with */
	w_current_writing = NULL;		 /* not currently writing anything */
	if (log_level > 3) write_log("About to fork of write child");
	taper_tmpnam(pipetoc);		 /* set up communication pipes */
	taper_tmpnam(pipefc);
	if ((mknod(pipetoc, S_IFIFO|S_IREAD|S_IWRITE, 0)) == -1) 
	  return do_exit(ERROR_CREATING_FIFO);
	if ((mknod(pipefc, S_IFIFO|S_IREAD|S_IWRITE, 0)) == -1) {
	    unlink(pipetoc);
	    do_exit(ERROR_CREATING_FIFO);
	}
	f = fork();
	if (f == 0) {
	    if (log_level > 3) write_log("W:Write child forked off");
	    fd_pipefc = fopen(pipefc, "w");	 
	    fd_pipetoc = fopen(pipetoc, "r");
	    do_write_child();			 /* do child loop */
	}
	if (f == -1)  {				 /* we are in the parent */
	    if (log_level > 1) write_warning_log("Unable to fork - not triple buffering");
	    unlink(pipetoc); unlink(pipefc);
	    write_pid = 0;
	}
	else {
	    write_pid = f;
	    fd_pipefc = fopen(pipefc, "r");	 
	    fd_pipetoc = fopen(pipetoc, "w");
	    signal(SIGPIPE, parent_pipe);
	}
    }
#endif

    if (log_level > 3) {
	sprintf(s, "Setting up to write out current buffer %d", 
		(w_current_buffer == w_buffer_1) ? 1 : 2);
	write_log(s);
    }
    if (w_current_buffer == w_buffer_1) 
      buf_length_1 = w_cur_pos;
    else
      buf_length_2 = w_cur_pos;

    write_buf_no = NULL;			 /* ensure all bytes are zero */
#ifdef TRIPLE_BUFFER    
    write_buf_no = (write_pid) ? w_current_buffer :
                 (_vptr) (_s32) do_write_block(w_current_buffer, w_cur_pos, (w_cur_pos != DOUBLE_BUFFER_SIZE));   
    if (write_pid) {
	fprintf(fd_pipetoc, "Data\n");	 /* tell child data is ready */
	fflush(fd_pipetoc);
    }
#else						 /* not triple buffering */
    write_buf_no = (_vptr) (_s32) do_write_block(w_current_buffer, w_cur_pos, (w_cur_pos != DOUBLE_BUFFER_SIZE));
#endif  

/* Now swap buffers around - write to the other one */
    w_current_buffer = (w_current_buffer == w_buffer_1) ? w_buffer_2
      : w_buffer_1;				
    if (log_level > 3) {
	sprintf(s, "Now spooling to buffer %d", (w_current_buffer == w_buffer_1) ? 1 : 2);
	write_log(s);
    }
    w_cur_pos = 0;
    return 0;
}



int tape_write(_vptr buf, size_t count)
{
    size_t tr_bytes;
    size_t fit_in;
    
    tr_bytes = 0;				 /* either now or previously */
    while (tr_bytes < count) {
	fit_in = min(count-tr_bytes,DOUBLE_BUFFER_SIZE-w_cur_pos);
    	memcpy(w_current_buffer+w_cur_pos, (char *) buf+tr_bytes, fit_in);
	w_cur_pos += fit_in; 
	tr_bytes += fit_in;
	if (w_cur_pos >= DOUBLE_BUFFER_SIZE)
	  if (flush_buffers() == -1)
	    return -1;
    }
    return tr_bytes;
} 


int tape_read(_vptr buf, size_t count)
{
/* An end of tape is assumed if we get an return code=-1 and errno=ENOSPC   --OR--
 * the count returned is less than we asked for 
 * 
 * note that if the return count is 0, then this is assumed to be an
 * end of volume and not and end of tape, so the zero is returned.
*/
    size_t c;
    
    c =  read_from_buf(buf, count);
    if (c == count) return c;			 /* OK - written what we asked for */
    if (c == -1) {				 /* error */
      if (errno != ENOSPC)			 /* make sure not an end of tape error */
        return -1;				 /* no - return error */
      else
        c=0;					 
    }						 /* none would have been written */
/* Must be at end of tape */
    if (log_level > 2) write_log("End of tape reached while reading.");
    if (update_tsi) {				 /* update tsi */
	ifd.number_tsi++;
	tsi = my_realloc(tsi, sizeof(struct tape_size_info)*ifd.number_tsi);
	if (tsi == NULL) return do_exit(ERROR_MEMORY);
	tsi[ifd.number_tsi-1].tape_number = tdh.tape_number;
	tsi[ifd.number_tsi-1].volume = ifd.number_volumes;
	tsi[ifd.number_tsi-1].blocks = blocks_passed;
	tsi[ifd.number_tsi-1].lb_bytes_short = bytes_short;
    }
    if (c == 0) return 0;			 /* end of tape */
    if (check_tape(NULL, 0, tdh.tape_number+1) == -1)/* get next tape */
      return -1;
    return read_from_buf((char *) buf+c, count-c)+c;/* return data */
}


_errstat tape_read_namelen(char *s)
{
/* Reads a name from the tape. The first is a name len
 * which is an signed 32 bit then the length
 *
 * Returns -1 error. 0 otherwise
*/
    _s32 x;
    
    if (tape_read_s32(&x) == -1) return -1;
    if (tape_read(s, x) != x) return -1;
    return 0;
}


_errstat tape_read_fi(struct file_info *fi)
{
/* Reads a file_info from the tape. Converts anything to big endian
 * if required
 * 
 * Returns -1 if error, 0 otherwise
*/
    struct file_info fi1;
    
    if (tape_read((char *) &fi1, sizeof(fi1)) != sizeof(fi1)) return -1;
    fi_endianize2mach(&fi1, fi);
    return 0;
}

    
_errstat tape_write_s32(_s32 *x)
{
/* Writes signed 32 bit integer from the tape
 * 
 * Takes into account big/little endian 
 * 
 * Returns -1 if error, 0 otherwise
 */
    _s32 s;
    
    s = mach2littles32(x);
    if (tape_write((char *) &s, sizeof(s)) != sizeof(s)) return -1;
    return 0;
}


_errstat tape_write_namelen(char *s)
{
/* Reads a name from the tape. The first is a name len
 * which is an signed 32 bit then the length
 *
 * Returns -1 error. 0 otherwise
*/
    _s32 x;
    
    x = strlen(s)+1;
    if (tape_write_s32(&x) == -1) return -1;
    if (tape_write(s, x) != x) return -1;
    return 0;
}


_errstat tape_write_fi(struct file_info *fi)
{
/* Reads a file_info from the tape. Converts anything to big endian
 * if required
 * 
 * Returns -1 if error, 0 otherwise
*/
    struct file_info fi1;

    fi_endianize2little(fi, &fi1);
    if (tape_write((char *) &fi1, sizeof(fi1)) != sizeof(fi1)) return -1;
    return 0;
}


_errstat get_tape_header(WINDOW *mess, int line, struct tape_header *tdh)
{
/* Tries to get the tape header of the tape currently in the drive.
 * 
 * Opens the tape but doesn't close it

   Returns -1 if error
	   BAD_MAGIC   bad magic number
	   TAPE_EXIST  successful return
*/

    memset(tdh, 0, sizeof(*tdh));
    if (mess)
      mess = status_box(mess, "Rewinding tape", line, FALSE, 1);
    if (tape_rewind() == -1) return -1;		 /* rewind tape */
     
    if (mess)
      status_box(mess, "Identifying tape", line, FALSE, 1);
    if (tape_open(O_RDWR) == -1) 		 /* open rewinding device */
      return -1;

    if (tape_readheader(tdh, 1) == -1)   	 /* could open but can't */
	return BAD_MAGIC;			 /* read - therefore is bad magic */
    
    if ((tdh->magic != TAPER_MAGIC_NUMBER) && (tdh->magic != TAPER_64_MAGIC)) {
	if (tdh->magic == TAPER_4_MAGIC_NUMBER)
	  if (mess)
	    message_box("Sorry, taper 4 archives are not compatible", MB_OK);
	return BAD_MAGIC;
    }
    
    if ((tdh->magic == TAPER_MAGIC_NUMBER) && (tdh->archive_id == 0))
	return TAPE_EXIST_EMPTY;
    return TAPE_EXIST;
}


_errstat tape_write_volheader(struct volume_header *vh)
{
    struct volume_header vh1;

    volheader_endianize2little(*vh, &vh1);
    if (tape_write((char *) &vh1, sizeof(struct volume_header)) == -1)
      return -1;
    return 0;
}


_errstat tape_read_volheader(struct volume_header *vh, int allower)
{
/* Reads a volume header.
 * 
 * If allower == 1, allows error, otherwise doesn't
 * 
 * Returns -1 if error, 0 otherwise.
 */
    struct volume_header vh1;
    
    if (log_level > 2) write_log("Reading in volume header");
    if (tape_read((char *) &vh1, sizeof(struct volume_header)) != sizeof(struct volume_header)) { /* volume magic number */
	if (allower) {
	    if (log_level > 2) write_log("Allowable error reading in volume header");
	    return 0;
	}
	write_fatal_log("reading in volume header");
	return -1;
    }
    volheader_endianize2mach(vh1, vh);
    return 0;
}


_errstat read_volheader(struct volume_header *vh, _s8 read_vh, _s8 into_mem)
{
/* Reads in volume header. If into_mem, places the information into
 * memory, otherwise discards it. If read_vh, then expects to read
 * the volume header, otherwise assumes that it is already read in.
 * 
 * returns -1 if error, 0 otherwise
*/
    char *z;
    _errstat x;
    
    if (read_vh) {
	if (log_level > 2) write_log("Reading in volume header");
	if (tape_read_volheader(vh, 0) == -1) return -1;
    }
    if ((vh->volume_magic != VOLUME_MAGIC) &&	 /* volume magic doesn't agree - warning */
	(vh->volume_magic != VOLUME_64_MAGIC))
      write_warning_log("Volume magic number incorrect");   /* try to proceed */
    if (log_level > 2) write_log("Reading in volume header details");
    if (into_mem) {
	vol_headers = my_realloc(vol_headers, ifd.size_volume_headers + vh->size_header);
	if (vol_headers == NULL)
	  return do_exit(ERROR_MEMORY);
	memcpy( ((char *) vol_headers)+ifd.size_volume_headers, vh, sizeof(struct volume_header));
	if (tape_read(((char *) vol_headers)+ifd.size_volume_headers+sizeof(*vh), vh->size_header-sizeof(*vh)) == -1)
	  return -1;
	return 0;
    }
    z = my_malloc(vh->size_header);		 /* skip past header details */
    if (z == NULL) return do_exit(ERROR_MEMORY);
    x = tape_read(z, vh->size_header-sizeof(*vh));
    my_free(z);
    return x;
}


_errstat goto_end_vol(WINDOW *mes, int ln, _s32 vol, _s32 at_vol, _s8 get_th, _s8 to_end)
{
/* Positions tape at beginning of volume 'vol'+1. Assumes at at_vol
 * Prints status on line 'ln' of box 'mes'
 * If get_th == 1, then:
 *   opens the rewinding device
 *   gets tape header
 * otherwise assumes that's rewinding is open & it's been got
 * 
 * if to_end is TRUE, then goes positions for append
 * 
 * 
 * If !fast_fsf, then the drive is closed & re-opened as
 * non-rewinding & re-positioned, otherwise quickly fsf
 * 
 * Returns -1 if error or abort
 *   otherwise 
 *     if fsf returns the volume # we are at the beginning of
 *     if not fsf returns the number of blocks skipped
*/
    int c, want_tape, pass_blocks, pass_vols;
    struct volume_tape_info *vt;
    char s[100];

    vt = vt_info + vol -1;			 /* get information of  */
    want_tape = vt->end_tape;
    pass_blocks = 0;
    pass_vols = 0;
    vt = vt_info;
    for (c=0;c<vol;c++) {			 /* work out how many blocks to pass */
	if ((vt->end_tape == want_tape) && (vt->volume <= vol) && 
	    (vt->volume >= at_vol)) {
	    pass_blocks += vt->blocks_on_last_tape;
	    pass_vols++;
	}
	vt++;
    }

    if (get_th) 				 /* only get if user wants */
      while (1) {				 /* use to get tape header */
	  c = get_tape_header(mes, ln, &tdh);
	  at_vol = 1;
	  if (c==-1) return -1;
	  if (c==BAD_MAGIC) {
	      message_box("This is not a taper tape", MB_OK);
	      touchwin(win_main); wrefresh(win_main);
	  }
	  if (c==TAPE_EXIST) break;
      }

    if (check_tape(mes, ln, want_tape) == -1)	 /* make sure correct tape in drive */
	  return -1;				 /* user aborted or error */
    sprintf(s, "Advancing to volume %d", vol+1);
    status_box(mes, s, ln, FALSE, 1);   
    if (have_fsf) {
	if (!fast_fsf) {
	    tape_close();			 /* close rewinding device opened earlier */
	    if ((dv = ntape_open(O_RDWR)) == -1) /* now open non-rewinding so we can position */
	      return -1;
	    if (to_end) {
		if (tape_eom(FALSE) == -1) 
		  return -1;
	    }
	    else {
		if (tape_fsf(pass_vols, FALSE) == -1)/* reposition */
		  return do_exit(ERROR_TAPE_FSF);
	    }
	    tape_close();			 /* close it to effect change */
	    if ((dv = tape_open(O_RDWR)) == -1)	 /* reopen at new position (rewinding device)*/
	      return -1;
	    return pass_vols+1;
	}
	else {					 /* have a fast fsf */
	    if (!(pass_vols-at_vol+1)) return 0;
	    if (to_end) {
		if (tape_eom(FALSE) == -1) 
		  return -1;
	    }
	    else {
		if (tape_fsf(pass_vols-at_vol+1, FALSE) == -1) 
		  return -1;
	    }
	}
	clear_buffer_pointers();
	tape_set_blk_size();		 
	return pass_vols+1;
    }

    if (blocks_passed < pass_blocks) {		 /* ?need to advance */
	for (c=blocks_passed; c<pass_blocks; c++) {   /* yes */
	    if (raw_read(dv, read_buffer, block_size) != block_size)
	      return do_exit(ERROR_SKIPPING);
	}
    }
    blocks_passed = 0;				 /* equivalent of fsf */
    clear_buffer_pointers();			 /* discard whatever was left */
    status_box(mes, "", ln, FALSE, 1);
    return pass_blocks;
}

    

