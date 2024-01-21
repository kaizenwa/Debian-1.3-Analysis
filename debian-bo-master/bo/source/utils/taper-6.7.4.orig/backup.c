/*
   Time-stamp: <96/08/04 19:27:32 yusuf>

   $Id: backup.c,v 1.30 1996/08/05 19:01:59 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: backup.c,v 1.30 1996/08/05 19:01:59 yusuf Exp $";
#endif /* lint */




/* Tape structure
 * 
 * Taper header (at beginning of each tape)
 * ----------------------------------------
 * long taper_magic number  (identifies taper backups  )
 * long archive ID (unqiue # that IDs archives         ) 
 * int tape number                                     )
 * char archive_title 
 * 
 * 
 * Volume
 * ------
 * 
 * Volume header information
 * 
 * DATA
 *               : file_info
 *               : file_name
 *               : file (straight or compressed) only if file IS_IFREG
 *               :    S_ISDIR   nothing
 *               :    S_ISLNK   the name of the symbolic link (# chars first)
 * 
 * 
 * File info block:
 * 
 *   long   actual size  with uncompressed files, = size
 *   volume           
 *   position in vol  
 *   rdev             )
 *   uid              )
 *   gid              )
 *   mode             )     Taken from stat call
 *   org mode (mode of original file for links)
 *   size             )
 *   a time           ) 
 *   m time           )
 *   c time           )  
 *   backup time 
 *   length of name+1
 *   compressed or not compressed
 *   checksum
 *
 * 
 * Information file structure 
 * 
 * long magic
 * long archive_id                        )
 * int number_tapes                       )   in structure
 * long info_file_size                    )
 * int number_volumes                     )
 * long size_of_volume_headers
 * long no_in_archive                     ) 
 * char archive_title                     )  
 * 
 * IF COMPRESSED long size_compressed
 * 
 * For each file:
 *       file_info
 *       file_name
 * 
 * 
 * Volume details
 *   vol header                   )   for each volume
 *   files selected               )   on archive
 * 
 * Volume/tape information
 *       volume    tape it's on   - for each volume
 * 
 * Tape size information
 *       tape      size in blocks
 * 
 * 
 * The info file is written in little endian format.
 * 
 * The info file can be compressed (using internal gzip).
 * If it is, then there is an additional field - length of compressed
 * info file
*/


#include "taper.h"
#include "backup.h"

FILE *fdfifo;
char fifo_name[MAX_FNAME];

void backup_stop_child(void)
{
/* Stops child from compressing and deletes whatever files
 * are left in temporary buffers */
    char s[MAX_FNAME];
    
    if (backup_child) {
	kill(backup_child, SIGTERM);		 /* tell child to stop compressing */
	waitpid(backup_child, NULL, 0);
	backup_child = 0;
    }
    while (!feof(fdfifo)) {
	if (fgets(s, sizeof(s), fdfifo) == NULL) /* original filename */
	  break;
	if (fgets(s, sizeof(s), fdfifo) == NULL) /* compressed name */
	  break;
	if (s[strlen(s)-1] == '\n')		 /* remove trailing \n */
	  s[strlen(s)-1] = 0;
	unlink(s);				 /* delete it */
    }
    fclose(fdfifo);				 /* close FIFO */
    fdfifo = NULL;
    unlink(fifo_name);				 /* remove file list & FIFO */
}


_errstat backup_dpd(char *full_path, struct stat *b)
{
/* The routine for each file found by process_dir */
    
    struct file_info fi;
    dev_t dev;
    
    dev = get_file_info(full_path, &fi, 0, b);
    if (dev == 0) return 0;			 /* error - ignore */
    if (S_ISDIR(fi.mode)) 			 /* directories have trailing '/' */
	if (full_path[strlen(full_path)-1] != '/') 
          strcat(full_path, "/");
    if ( (proc_dev == 0) ||			 /* don't backup if on  */
	 ((proc_dev == 1) && (proc_dev != dev)) ) {/* /proc device */ 
	if (add_one_file_engine(NULL, &fi, full_path) != -1)
	  total_selected += sizeof(struct file_info) + fi.name_len;
    }
    return 0;
}


_errstat add_actual_files(WINDOW *mes_box)
{
    _s32 count;
    struct oa_file_entry *se;
    
    for (count=0; count<no_sel; count++) {
	se = find_entry(sel_files, count);
	if ( (se->selected == 1) || /* only if directly selected */
	   (se->selected == 4) ) {
	    process_dir(NULL, 0, name(se), se->incremental, backup_dpd, TRUE);
	}
    }
    return 0;
}


_errstat backup_add_missing_dirs(WINDOW *mes)
{
/* This routines adds any directories that are required. */
    
    cp = info + sizeof(struct info_file_header);
    if (make_cf(cp, ifd.no_in_archive) == -1) return -1; /* find common path */
    return add_missing_dirs(mes, TRUE);
}


_errstat write_nameinfo(struct file_info *fi, char *cp)
{
/* An error writing to the backup device is a fatal error
   Returns 0 if all OK
   Returns -1 if error
*/
    char l[MAX_FNAME+50];
    
    sprintf(l, "Writing info & filename for %s", cp);
    if (tape_write_fi(fi) == -1) {
	write_fatal_log(l);			 /* write info */
	return -1;
    }
    if (tape_write((char *) cp, fi->name_len) == -1) {/*  write filename */
	write_fatal_log(l);
	return -1;
    }
    return 0;
}


_errstat backup_file(WINDOW *mes, _s32 pos_in_archive)
{
/* Backup the file pointed to by cp and update cp 
 * An error writing to the backup device is a fatal error

   Returns 0 if all OK
   Returns -1 if error
*/
    
    struct file_info *fi;
    char   tmpf[MAX_FNAME];

    char   *fn, l[MAX_FNAME+50];
    int    fd;
    _s32   x, totalread, totalwrite;
    struct stat sbuf, csbuf;
    WINDOW *m=NULL;
    
    fi = (struct file_info *) cp;		 /* fi points to file info */
    fn = cp + sizeof(struct file_info);
    fi->pos_in_archive = pos_in_archive;	 /* set pos in volume */

    if (S_ISLNK(fi->mode)) {			 /* a soft link */
	memset(tmpf, 0, sizeof(tmpf));		 /* name of original file */
	readlink(fn, tmpf, sizeof(tmpf));	 /* is written */
	fi->checksum = 0;
	fi->act_size = strlen(tmpf)+1+sizeof(_s32);
	fi->size = strlen(tmpf)+1+sizeof(_s32);
	sprintf(l, "Writing link name for %s [%s]", fn, tmpf);
	if (log_level > 1) write_log(l);
	if (write_nameinfo( fi, fn) == -1) {
	    write_fatal_log(l);
	    return -1;
	}
	x = strlen(tmpf)+1;
	if (tape_write_namelen(tmpf) == -1) {
	    write_fatal_log(l);
	    return -1;
	}
	return 0;
    }
	
    if (!(S_ISREG(fi->mode))) { 		 /* if file isn't an ordinary */
	fi->checksum = 0;
	sprintf(l, "Storing directory %s", fn);
	if (log_level > 1) write_log(l);
	return write_nameinfo( fi, fn);		 /* file, only write out name */
    }

    *tmpf = 0;					 /* must be a regular file */
    fi->compressed = 0;				 /* not compressed */
    if ((compression) && (!exclude_compression(fn)) && (fdfifo != NULL)) {/* compress if want to and not excluded */
	sprintf(l, "Reading filename from FIFO for %s", fn);
	if (log_level > 2) write_log(l);
	f:;
	if (fgets(tmpf, sizeof(tmpf), fdfifo) == NULL) {   /* gets the original filename */
	    if (tmpf[strlen(tmpf)-1] == '\n')
	      tmpf[strlen(tmpf)-1] = 0;
	    sprintf(l, "Couldn't read filename from FIFO for %s - saving uncompressed", fn);
	    if (log_level > 2) write_log(l);
	    *tmpf = 0;
	    goto non_compress;
	}
	if (m) {close_statusbox(m); touchwin(mes); wrefresh(mes);}
	if (tmpf[strlen(tmpf)-1] == '\n')
	  tmpf[strlen(tmpf)-1] = 0;
	if (!strcmp(tmpf, "DISK FULL")) {	 /* child is waiting for disk space */
	    if (mes) {
		sprintf(l, "Child has no disk space - waiting");
		if (log_level > 2) write_log(l);
		m = status_box(mes, "Child waiting for disk space", 1, TRUE, 1);
		goto f;				 /* do it again, sam */
	    }
	}
	if (strcmp(tmpf, fn)) {			 /* checks that FIFO & me agree */
	    sprintf(l, "Mismatch reading in FIFO output for %s [got %s] - saving uncompressed", fn, tmpf);
	    if (log_level > 2) write_log(l); 
	    *tmpf=0;
	    goto non_compress;
	}
	sprintf(l, "Reading compressed filename from FIFO for %s", fn);   /* read name of compressed file */
	if (log_level > 2) write_log(l);
	if (fgets(tmpf, sizeof(tmpf), fdfifo) == NULL) {
	    if (tmpf[strlen(tmpf)-1] == '\n')
	      tmpf[strlen(tmpf)-1] = 0;
	    sprintf(l, "Couldn't read compressed filename from FIFO for %s - saving uncompressed", fn);
	    if (log_level > 2) write_log(l);
	    *tmpf=0;
	    goto non_compress;
	}
	if (tmpf[strlen(tmpf)-1] == '\n')
	  tmpf[strlen(tmpf)-1] = 0;
	if (!strcmp(tmpf, FIFO_ERR)) {
	    sprintf(l, "FIFO didn't compress %s - saving uncompressed", fn);
	    if (log_level > 2) write_log(l);
	    *tmpf=0;
	    goto non_compress;			 /* assume no compression */
	}
	sprintf(l, "Opening compressed file %s", tmpf);
	if (log_level > 2) write_log(l);
	fd = open(tmpf, O_RDWR);		 /* open the temp file */
	if (fd == -1) {
	    write_error_log(l);
	    unlink(tmpf);			 /* compressed file, delete it */
	    fi->checksum = -1;			 /* mark file as invalid */
	    return write_nameinfo( fi, fn);
	}
	sprintf(l, "Getting compressed file status info for %s", tmpf);
	if (log_level > 2) write_log(l);
	if (fstat(fd, &csbuf) == -1) {		 /* get compressed file size */
	    write_error_log(l);
	    fi->checksum = -1;			 /* mark file as invalid */
	    close(fd);
	    unlink(tmpf);
	    return write_nameinfo( fi, fn);
	}
	
	fi->compressed = compression;		 /* file is compressed */
	fi->act_size = csbuf.st_size;		 /* actual size = size of compressed file */
    }
    else {					 /* no compression needed */
	non_compress:
	sprintf(l, "Opening file %s", fn);
	if (log_level > 2) write_log(l);
	fd = open(fn, O_RDONLY);		 /* open source file */
	if (fd == -1) {
	    write_error_log(l);
	    fi->checksum = -1;			 /* mark file as invalid */
	    return write_nameinfo( fi, fn);
	}
	sprintf(l, "Getting file status info for %s", fn);
	if (log_level > 2) write_log(l);
	if (fstat(fd, &sbuf) == -1) {		 /* get actual file size */
	    write_error_log(l);
	    fi->checksum = -1;			 /* mark file as invalid */
	    close(fd);
	    return write_nameinfo( fi, fn);
	}
	fi->act_size = sbuf.st_size;		 /* update file info */
    }
    
    fi->checksum = calc_checksum(fd);		 /* calculate checksum */

    sprintf(l, "Backing up file %s; actual size %d, on tape size %d.", 
	    fn, fi->size, fi->act_size);
    if (log_level > 1) write_log(l);
    if (write_nameinfo(fi, fn) == -1) {		 /* couldn't */
	write_fatal_log(l);
	close(fd);
	return -1;				 /* write info/name - fatal */
    }
    if (fi->checksum == -1) {			 /* couldn't get checksum - not fatal */
	close(fd);
	return 0;
    }
    totalread = 0;
    totalwrite = 0;
    while (1) {					 /* copy file accross */
	x = read(fd, tr_buffer, max_tr_size);	 /* to device file   */
	if (!x) break;
	if (x == -1) {
	    write_error_log("Error reading file while transferring data to backup device");
	    while (totalwrite < fi->act_size) 
	      totalwrite += tape_write((char *) tr_buffer, min(max_tr_size,/* pad rubbish data to the end so */
				       fi->act_size - totalwrite));
	    totalread = totalwrite;
	    break;				 /* so that the archive integrity is maintained */
	}
	totalread += x;
	
	if (totalwrite < fi->act_size) {	 /* don't write too much data to buffer */
	    if ((x = tape_write((char *) tr_buffer, min(x, fi->act_size-totalwrite))) == -1) {
		close(fd);
		return -1;
	    }
	    totalwrite += x;
	}
    }
    close(fd); 
    
    if (totalread > totalwrite) {
	sprintf(l, "%s grew while writing. Data at end of file not backed up", fn);
	write_error_log(l);
    }
    
    if (*tmpf)					 /* remove temporary files */
      unlink(tmpf);
    return 0;
}

_errstat write_volume_header(_s32 no_files)
{
/* Writes the volume header to the tape and appends volume header
 * information to the block in memory.

 *  Returns 0 if OK, -1 if error
*/
    _s32 c;
    struct oa_file_entry *fe;
    int  c1, sz;
    char *cur_pos;
    struct volume_header *vh1, vh;
    char null_string=0;

    if (log_level > 2) write_log("Writing volume header");
    vh.volume_magic = VOLUME_MAGIC;
    vh.backup_time = time(NULL);		 /* time backed up */
    strcpy(vh.volume_title, volume_title);
    vh.no_sels = no_sel;
    vh.no_in_volume = no_files;
    sz = sizeof(struct volume_header);
    fe=sel_files;				 /* work out how much space */
    for (c=0; c<no_sel; c++) {			 /* needed for volume header */
	sz += sizeof(_s32) + strlen(name(fe))+ 1 +
	  sizeof(_s32)+ 0 + 1;			 /* room for filter */
	advance_ofe(&fe);
    }
    vh.size_header = sz;
    ifd.size_volume_headers += sz;		 /* update size */
    if (tape_write_volheader(&vh) == -1) return -1;
    vol_headers = my_realloc(vol_headers, ifd.size_volume_headers);
    if (vol_headers == NULL)
      return do_exit(ERROR_MEMORY);
    cur_pos = (char *) vol_headers;
    for (c=0; c<ifd.number_volumes-1; c++) {	 /* skip past existing entries */
	vh1 = (struct volume_header *) cur_pos;	 /* in volume headers */
	cur_pos += sizeof(struct volume_header);/* past header */
	for (c1=0; c1<vh1->no_sels; c1++) {
	    sz = *(_s32 *) cur_pos;		 /* skip selection name */
	    cur_pos += sz + sizeof(_s32);
	    sz = *(_s32 *) cur_pos;		 /* skip filter */
	    cur_pos += sz + sizeof(_s32);
	}
    }
    *((struct volume_header *)  cur_pos) = vh;
    cur_pos += sizeof(struct volume_header);
    fe=sel_files;				 /* write file selections */
    for (c=0; c<no_sel; c++) {
	c1 = strlen(name(fe)) + 1;		 /* file name */
	if (tape_write_namelen(name(fe)) == -1)
	  return -1;
	memcpy(cur_pos, &c1, sizeof(_s32));	 /* filename length */
	cur_pos += sizeof(_s32);		 /* advance to filename pos */
	strcpy(cur_pos, name(fe));
	cur_pos += c1;				 /* past filename */
	c1 = 0+1;				 /* filter */
	if (tape_write_namelen(&null_string) == -1)
	  return -1;
	memcpy(cur_pos, &c1, sizeof(_s32));
	cur_pos += sizeof(_s32);
	strcpy(cur_pos, "");			 /* filter */
	cur_pos += c1;
	advance_ofe(&fe);
    }
    return 0;					 
}


_errstat make_comp_file(char *s)
{
/* Creates a file and gives it a name then writes the names of the files
 * to be compressed, filename after filename.
 * 
 * Returns -1 if error, 0 otherwise
 */
    FILE *cf;
    int c;
    cp = info + sizeof(struct info_file_header);
    taper_tmpnam(s);
    cf = fopen(s, "w+");			 /* create & open file */
    if (cf == NULL) return -1;
    
    for (c=0; c<ifd.no_in_archive;c++) {
	if (((struct file_info *) cp)->pos_in_archive == 0)   /* only if not already in archive */
	  if (S_ISREG(((struct file_info *) cp)->mode)) {   /* only regular files */
	      fputs(cp+sizeof(struct file_info), cf);
	      fputs("\n", cf);
	  }
	cp+=sizeof(struct file_info);		 /* advance to next entry */
	PAST_STRING_FI(cp);
    }
    fclose(cf);
    return 0;
}

_errstat  do_backup(void) {				 
/* For aborting a backup, 
 * 
 * fi.checksum = -2 is written - this means a backup
 *   was aborted. The info file is changed accordingly
*/
    _s32   count=0, no_old_files, no_written, org_files;
    _u32   bytes_processed, bytes_written;
    WINDOW *mes=NULL;
    char   s[100], s2[30], s3[30], s4[30], tmpbuf[MAX_FNAME], cfile[MAX_FNAME];
    time_t t_start, t_current;
    struct volume_tape_info vti;
    int    x;
    _s8    quitting;
    struct file_info *fi;
    char   sa[9][150];
    struct volume_header v;
	
    if (!no_sel)				 /* none selected */
      return 0;
    if (!no_windows)
      mes = status_box(mes, "          Opening backup device...          ", 3, TRUE, 6);

    status_box(mes, "Adding to archive directory...", 3, FALSE, 1);
    if (!append) {
	tdh.magic = TAPER_MAGIC_NUMBER;		 /* identify our directory volume */
	tdh.tape_number = 1;			 /* first volume */
	ifd.archive_id = tdh.archive_id;	 /* setup info file header */
	ifd.info_file_size = sizeof(struct info_file_header);
	ifd.magic = INFO_MAGIC;
	ifd.number_tapes = 0;
	ifd.number_tsi = 0;
	ifd.number_volumes = 0;
	ifd.size_volume_headers = 0;
	ifd.no_in_archive = 0;
	strcpy(ifd.archive_title, archive_title);
	strcpy(tdh.archive_title, archive_title);
	no_old_files = 0;
	vt_info = my_realloc(vt_info, sizeof(struct volume_tape_info));
    }
    else {
	ifd = *((struct info_file_header *) info);/* get original ifd */
	ifd.magic = INFO_MAGIC;			 /* uncompressed originally */
	no_old_files = ifd.no_in_archive;
	vt_info = my_realloc(vt_info, sizeof(struct volume_tape_info)*(ifd.number_volumes+1));
    }

    org_files = no_old_files;
    if (!append) {
	if (tape_open(O_RDWR) == -1) return -1;
	tape_set_blk_size();
	if (write_tape_header(&tdh) == -1)
	  return -1;
    }
    else {
	if (goto_end_vol(mes, 3, ifd.number_volumes, 1, TRUE, TRUE) == -1)/* goto end of */
	  return -1;				 /* previous volume */
	tape_set_blk_size();
    }

    ifd.number_volumes++;
    vti.volume = ifd.number_volumes;		 /* update volume/tape info */
    vti.start_tape = ifd.number_tapes;
    cp = info + sizeof(struct info_file_header);
    if (append) 				 /* move past existing entries */
      for (count=0; count < no_old_files; count++) {
	  cp += sizeof(struct file_info);
	  PAST_STRING_FI(cp);
      }

    if (add_actual_files(mes) == -1)
      return -1;				 /* add filenames to directory */
    
    memcpy(info, &ifd, sizeof(struct info_file_header));
    status_box(mes, "", 3, FALSE, 1);
    if (backup_add_missing_dirs(mes) == -1)	 /* add directories that are required */
      return -1;
    status_box(mes, "", 1, FALSE, 1);

    if (write_volume_header(ifd.no_in_archive-no_old_files) == -1) return -1;/* write volume header */
/* free superflous memory */    
    sel_files = my_realloc(sel_files, 1);
    archive_files = my_realloc(archive_files, 1);
    len_archive_files = 0; cur_af_size = 0;

    bytes_written=0;
    bytes_processed=0;
    print_kb(s3, total_selected);
    fdfifo = NULL;
    *cfile=0;
    if (compression) 
      if (make_comp_file(cfile) != -1) {
	  taper_tmpnam(fifo_name);
	  if ((mknod(fifo_name, S_IFIFO|S_IREAD|S_IWRITE, 0)) == -1) 
	    return do_exit(ERROR_CREATING_FIFO);
	  if (backup_child) {				 /* a process still going on */
	      kill(backup_child, SIGKILL);
	      waitpid(backup_child, NULL, 0);
	      backup_child = 0;
	  }
	  if (log_level > 3) write_log("About to fork of backup child");
	  backup_child = fork();
	  if (backup_child == 0) {		 /* we are in child process */
	      close(dv);
	      chdir(original_cur_dir);
#ifdef TRIPLE_BUFFER
	      sprintf(tmpbuf, "B:Executing bg_backup %s %d %s %s %d %d \"%s\" %s %d",
		      log_file, log_level, cfile, fifo_name, compression, shm_id, exclude_compress, temp_dir, min_free);

	      sprintf(s2, "%d", shm_id);
#else
	      sprintf(tmpbuf, "B:Executing bg_backup %s %d %s %s %d %d \"%s\" %s %d",
			log_file, log_level, cfile, fifo_name, compression, 0, exclude_compress, temp_dir, min_free);
	      sprintf(s2, "%d", 0);
#endif
	      if (log_level > 3) write_log(tmpbuf);
	      sprintf(s, "%d", compression);
	      sprintf(s3, "%d", log_level);
	      sprintf(s4, "%d", min_free);
	      if (execlp("bg_backup", "bg_backup", log_file, s3, cfile, fifo_name, s, s2, exclude_compress, temp_dir, s4, NULL) == -1) {		  write_error_log("starting background backup");
		  fdfifo = fopen(fifo_name, "w");
		  fprintf(fdfifo, "NO CHILD\n"); /* no child! */
		  fclose(fdfifo);
		  exit(-1);			 /* should never get here */
	      }
	  }
	  
	  if (backup_child == -1) {
	      unlink(fifo_name); 
	      if (*cfile) unlink(cfile);
	      backup_child = 0;
	      return do_exit(ERROR_UNABLE_FORK);
	  }
	  if ((fdfifo = fopen(fifo_name, "r")) == NULL) {
	      if (backup_child) {
		  kill(backup_child, SIGKILL);		 /* kill child process  */
		  waitpid(backup_child, NULL, 0);
		  backup_child = 0;
	      }
	      return do_exit(ERROR_OPENING_FIFO);
	  }
	  fgets(tmpbuf, sizeof(tmpbuf), fdfifo);
	  tmpbuf[strlen(tmpbuf)-1] = 0;		 /* remove trailing '\n' */
	  if(!strcmp(tmpbuf, "NO CHILD")) {
	      waitpid(backup_child, NULL, WNOHANG);
	      return do_exit(ERROR_NO_BACKUP_CHILD);
	  }
	  if (comp_head_start) {			 /* let the compression program */
	      status_box(mes, "Giving compression a head start", 3, FALSE, 1);
	      sleep(60*comp_head_start);		 /* have a head start */
	      t_start = time(NULL);		 /* restart timer */
	  }
      }
    cp = info + sizeof(struct info_file_header);
    if (mes) nodelay(mes, TRUE);
    quitting=0;
    no_written=0;
    t_start = time(NULL);
    for (count=0; count< ifd.no_in_archive; count++) {
	*s = (mes == NULL) ? 0 : wgetch(mes);
	if (((*s == 'q') || (*s == 'Q')) && (!quitting)) {
	    if (message_box("Confirm abort backup", MB_YESNO)) {
		touchwin(mes); wrefresh(mes);
		backup_stop_child();		 /* stop child from compressing */
		fi = (struct file_info *) cp;
		fi->act_size = -1;
		fi->name_len = 0;
		fi->checksum = -2;
		write_nameinfo(fi, "");
		ifd.no_in_archive = no_old_files;/* correct info file */
		memcpy(&v, get_vh(vol_headers, ifd.number_volumes), sizeof(struct volume_header));   /* this needed to */
		v.no_in_volume = no_written;	 /* ensure aligned access */
		memcpy(get_vh(vol_headers, ifd.number_volumes), &v, sizeof(struct volume_header));   
		write_log("Backup aborted by user");
		log_errors++;
		break;
	    }
	}
	if (((struct file_info *) cp)->pos_in_archive == 0) {  /* only need new ones */
	    no_written++;
	    status_box(mes, cp+sizeof(struct file_info), 0, FALSE, 1);
	    sprintf(s, "File %d of %d", count, ifd.no_in_archive);
	    status_box(mes, s, 2, FALSE, 1);
	    sprintf(s, "Processed %s of %s", print_kb(s2, bytes_processed), 
		    print_kb(s3, total_selected));
	    status_box(mes, s, 3, FALSE, 1);
	    if (bytes_written)
	      sprintf(s, "Written %s : Ratio %.2f",
		      print_kb(s2, bytes_written), 
		      (float) bytes_processed/(float) bytes_written);
		else
	      sprintf(s, "Written %s : Ratio 1.0",
		      print_kb(s2, bytes_written));
	    status_box(mes, s, 4, FALSE, 1);
	    sprintf(s, "Total on archive %s", print_kb(s2, bytes_written+total_compressed));
	    status_box(mes, s, 5, FALSE, 1);
	    t_current = time(NULL);
	    x = t_current-t_start;
	    if (total_selected) 
	      sprintf(s, "%.0f%% done, Elapsed %s, Remaining %s",  (float) bytes_processed/(float) total_selected * 100,
		      convtime(s2, t_start, t_current),
		      (bytes_processed == 0) ? "" :
		      convtime(s3, x, x / ((float) bytes_processed/(float) total_selected)));
	    else				 /* if bytes=0, base on # files */
	      sprintf(s, "%d%% done, Elapsed %s, Remaining %s",  100*count/ifd.no_in_archive,
		      convtime(s2, t_start, t_current),
		      (count == 0) ? "" :
		      convtime(s3, x,  x / (count/ifd.no_in_archive)));
	    status_box(mes, s, 7, FALSE, 1);
	    if (x) {
		sprintf(s, "Backup rate %s/min [%s/min]",
			print_mb(tmpbuf, bytes_written/x*60),
			  print_mb(s2, bytes_processed/x*60));
		status_box(mes, s, 8, FALSE, 1);
	    }
	    if (!no_windows) {
		touchwin(mes); wrefresh(mes);	 /* in case new tape */
	    }
	    if (backup_file(mes, ++no_old_files) == -1) {
		tape_close();			 /* rewinding device so will auto update headers */
		backup_stop_child();
		if (*cfile) unlink(cfile);
		waitpid(backup_child, NULL, WNOHANG);
		return -1;			 /* error message handled by backup file */
	    }
	    bytes_processed += sizeof(struct file_info) + ((struct file_info *) cp)->name_len;
	    bytes_written += sizeof(struct file_info) + ((struct file_info *) cp)->name_len;
	    if ((S_ISREG(((struct file_info *) cp)->mode)) ||
		(S_ISLNK(((struct file_info *) cp)->mode)) ) {   /* only add this if regular file */
		bytes_processed += ((struct file_info *) cp)->size;
		bytes_written += ((struct file_info *) cp)->act_size;
	    }
	}
	
	cp += sizeof(struct file_info);
	PAST_STRING_FI(cp);
    }
    t_current = time(NULL);
    x = t_current - t_start;
    if (x == 0) x=1;
    status_box(mes, "", 0, FALSE, 1);
    status_box(mes, "", 2, FALSE, 1);
    status_box(mes, "", 4, FALSE, 1);
    status_box(mes, "", 6, FALSE, 1);
    status_box(mes, "Updating header segments", 3, FALSE, 1);
    tape_close();				 /* rewinding device so will auto update headers */
    vti.blocks_on_last_tape = blocks_passed;
    vti.end_tape = ifd.number_tapes;
    memcpy(vt_info+ifd.number_volumes-1, &vti, sizeof(vti));   
/* Code for info file at end here */
    if (write_out_info_file(info, ifd.archive_id) != -1)
      if (compress_info) compress_info_file(ifd.archive_id);
    if (fdfifo) {
      fclose(fdfifo);				 /* close FIFO */
      unlink(fifo_name);				 /* remove file list & FIFO */
    }
    if (*cfile) unlink(cfile);			 /* in case child didn't */
    waitpid(backup_child, NULL, 0);
    backup_child = 0;
    close_statusbox(mes);
    if (win_main) {
	touchwin(win_main); 
	wrefresh(win_main);
    }
    sprintf(sa[0], "Backup Finished");
    strcpy(sa[1], "");
    sprintf(sa[2], "Backed up: %d files,  %s  [%s]", ifd.no_in_archive - org_files,
	    print_mb(s2, bytes_written),
	    print_mb(s3, bytes_processed));
    if (log_level > 1) write_log(sa[2]);
    if (mf) {write(mf, sa[2], strlen(sa[2])); write(mf, "\n", 1);}
        sprintf(sa[3], "Total on archive %s [%s]. Ratio %.2f", 
	    print_mb(s2, bytes_written+total_compressed),
	    print_mb(s3, bytes_processed+total_uncompressed),
	    (total_compressed+bytes_written == 0) ? 1 : 
	       (float) (total_uncompressed+bytes_processed)/
	       (float) (total_compressed+bytes_written));
    if (log_level > 1) write_log(sa[3]);
    if (mf) {write(mf, sa[3], strlen(sa[3])); write(mf, "\n", 1);}
    strcpy(sa[4], "");
    sprintf(sa[5], "Time elapsed %s.", convtime(s2, t_start, t_current));
    if (log_level > 1) write_log(sa[5]);
    if (mf) {write(mf, sa[5], strlen(sa[5])); write(mf, "\n", 1);}
    sprintf(sa[6], "Backup rate %s/min [%s/min]", 
	    print_mb(tmpbuf, bytes_written/x*60),
	    print_mb(s3, bytes_processed/x*60));
    if (log_level > 1) write_log(sa[6]);
    if (mf) {write(mf, sa[6], strlen(sa[6])); write(mf, "\n", 1);}
    strcpy(sa[7], "");
    sprintf(sa[8], "%d warnings, %d errors", log_warnings, log_errors);
    if (log_level > 1) write_log(sa[8]);
    if (mf) {write(mf, sa[8], strlen(sa[8])); write(mf, "\n", 1);}
    if (!no_windows) multi_message_box(sa, 9, MB_OK);
    return 0;
}


_errstat read_in_fileset(int argc, char *argv[])
{
/* Reads in the files */
    FILE *f;
    char ln[MAX_FNAME], s[MAX_FNAME];
    struct oa_file_entry *ce;
    _s32 c, c1;
    struct direntry entry;
    time_t t;
    
    c = 0;
    while (c < argc) {
	if ( (!strcmp(argv[c], "-U") ||
	      (!strcmp(argv[c], "-unattended-file"))) ) {
		  c++;
		  if (c == argc) {
		      strcpy(ln, "Illegal file/directory/set name specified\n");
		      write(mf, ln, strlen(ln));
		      strcpy(ln, "BACKUP ABORTED\n");
		      write(mf, ln, strlen(ln));
		      return -1;
		  }
		  if (*argv[c] == '@') {
		      strcpy(dir_cur_dir, taper_info_files);
		      f = backrest_restore_backupset(&argv[c][1]);/* open file set */
		      if (f==NULL) {
			  sprintf(ln, "WARNING: Couldn't open file set %s\n", &argv[c][1]);
			  write(mf, ln, strlen(ln));
			  continue;
		      }
		      sprintf(ln, "Using file set %s\n", &argv[c][1]);
		      write(mf, ln, strlen(ln));
		      while (!feof(f)) {
			  fgets(ln, sizeof(ln), f);
			  if (ln[strlen(ln)-1] == '\n')
			    ln[strlen(ln)-1] = 0;
			  if (*ln) {
			      ce=sel_files;
			      for (c1=0; c1<no_sel; c1++) {		 /* make sure not already in */
				  if (!strcmp(name(ce), ln)) 
				    break;
				  advance_ofe(&ce);
			      }
			      if (c1==no_sel)
				if (get_statinfo(ln, &entry.info) != -1) {/* ignore if error */
				    select_entry_1(&entry, ln);
				    write(mf, "  Selected ", strlen("  Selected "));
				    write(mf, ln, strlen(ln));
				    strcpy(ln, " [from fileset]\n");
				    write(mf, ln, strlen(ln));
				}
			      else {
				  sprintf(s, "WARNING: %s is not found in fileset %s\n", ln, &argv[c][1]);
				  write(mf, s, strlen(s));
			      }
			  }
			  fgets(ln, sizeof(ln), f);		 /* ignore filter */
		      }
		      fclose(f);
		  }
		  else {
		      if (get_statinfo(argv[c], &entry.info) != -1) {/* ignore if error */
			  select_entry_1(&entry, argv[c]);
			  sprintf(ln, "Selected %s\n", argv[c]);
			  write(mf, ln, strlen(ln));
		      }
		      else {
			  sprintf(ln, "WARNING: %s is not found\n", &argv[c][1]);
			  write(mf, ln, strlen(ln));
		      }
		  }
	      }
	c++;
    }
    time(&t);
    sprintf(ln, "\nBackup commenced at %s\n", ctime(&t));
    write(mf, ln, strlen(ln));
    return 1;
}

   
_errstat utils_write_nullheader(WINDOW **mes);
_errstat check_backup(void) {
/* Make sure backup is a valid device and confirm with use about
   an erase if it has been requested.         */
    int x;
    WINDOW *mes=NULL;
    char info_file[MAX_FNAME];
    struct stat buf;
    _s32 oldarch;

    if (stat(tape, &buf) == -1) {		 /* touch file if needed */
	if (tape_type == TAPE_TYPE_FILE) {
	    if (log_level > 2) write_log("Creating backup file");
	    close(creat(tape, S_IREAD|S_IWRITE));
	    x = TAPE_EXIST_EMPTY;
	}
	else
	  return do_exit(ERROR_OPENING_BACKUP);
    }
    else
      x= do_read_vol_dir(-1, tape, O_RDWR, append, FALSE);
    if (x==-1) return -1;
    if (x == BAD_MAGIC) {
	if (!no_windows) {			 
	    if (bmessage_box("Unknown tape data. Overwrite?", MB_YESNO)) append = 0;
	    else return 0;;
	}
	else {					 /* unattended mode */
	    if (!tape_overwrite) {
		if (mf) write(mf, "Unknown data on tape. Tape not overwritten.\n", 
			      strlen("Unknown data on tape. Tape not overwritten.\n"));
		if (mf) write(mf, "Use --tape-overwrite-on [+O] to force overwriting\n",
		              strlen("Use --tape-overwrite-on [+O] to force overwriting\n"));
		write_warning_log("Unknown data on the tape. Tape not overwritten");
		return 0;	 /* need to have authority to overwrite */
	    }
	    append = 0;				 /*   to overwrite tapes in unattended mode */
	}
    }
    
    if ((x == TAPE_EXIST) && (!append)) {
	if (!no_windows) {
	    if (!bmessage_box("Taper data on tape. Confirm erase", MB_YESNO))
	      return 0;
	}
    }
	
    if (x == TAPE_EXIST_EMPTY) {		 /* empty taper tape */
	append = 0;
	return 1;
    }
    
    if ((x == BAD_MAGIC) || (!append)) {	 /* no taper data on tape */
	append = 0;
	if (!no_windows)
	  mes = status_box(mes, "Erasing data on tape", 1, TRUE, 1);
	if (is_regfile(dv)) {
	    unlink(tape);				 /* regular files get deleted */
	    close(creat(tape, S_IREAD|S_IWRITE));/* and re-created to get zero length */
	}
	else {
	  oldarch = tdh.archive_id;
	    utils_write_nullheader(&mes);
	    tape_close();
	}
	strcpy(info_file, taper_info_files);	 /* look for information file */
	if (info_file[strlen(info_file)-1] != '/')	 /* associated with this archive */
	  strcat(info_file, "/");
	sprintf(info_file, "%staper_info.%u", info_file, oldarch);
	unlink(info_file);			 /* attempt to delete info file */
	if (mes) 
	  close_statusbox(mes);
	return 1;
    }
	
    
    if (x == TAPE_NOEXIST) {
	append = 0;
	return 1;
    }
    
    return 1;
}
    


void taper_backup(int argc, char *argv[])
{
    int      x;
    int      old_append;

    old_append = append;			 /* save value of append for future use */
    if (open_logfile("Backup") == -1)
      return;
    if (!no_windows) {
	backrest_init_windows() ;		 /* Draw windows */
	backrest_clear_screen();
    }
    else {					 /* open file to mail things to */
	taper_tmpnam(mailfn);
	mf = open(mailfn, O_CREAT|O_RDWR, S_IREAD|S_IWRITE);
	if (mf==-1) {
	    do_exit(ERROR_OPENING_MAIL);
	    return;
	}
    }
    
    if (check_device_names() == -1) goto endbackup;/* check devices & other parms */
    if (backrest_do_mallocs() == -1)		 /* couldn't allocate memory */
      goto endbackup;
    if (check_backup() > 0) {
	if (!append) {				 /* no archive */
	    tdh.archive_id = time(NULL);	 /* set defaults */
	    ifd.archive_id = tdh.archive_id;
	    if (!no_windows)
	      get_string(win_main, archive_title, MAX_ARCHIVETITLE, "Enter archive title");
	    strcpy(tdh.archive_title, archive_title);
	    strcpy(ifd.archive_title, archive_title);
	}
#ifdef MEMORY_TIGHT
	archive_files = my_realloc(archive_files, 1);   /* free this memory */
	len_archive_files = 0; 
#endif	
	print_title_line();
	chdir(cur_dir);
	if (!no_windows) {
	    if (get_string(win_main, volume_title, MAX_ARCHIVETITLE, "Enter volume title"))
	      x = backup_select_files(cur_dir);	 /* select files */
	    else 
	      x = 0;
	}
	else 					 /* unattended mode */
	    x = read_in_fileset(argc, argv);
	
	getcwd(cur_dir, sizeof(cur_dir));
	backrest_kill_windows();
	print_my_name();
	if (x > 0)  do_backup();
	if (dv > 0) tape_close();
	
    }
    else {
	backrest_kill_windows();
	log_errors = 1;				 /* the fatal error */
    }

    backrest_free_memory();

    if (mf) 
	  sendmail(); 					 /* if mail file */
    append = old_append;
   
    endbackup:;
    close_logfile("Backup");
}
