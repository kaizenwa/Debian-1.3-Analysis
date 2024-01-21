/*
   Time-stamp: <96/07/19 20:17:55 yusuf>

   $Id: mkinfo.c,v 1.18 1996/07/27 20:42:10 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: mkinfo.c,v 1.18 1996/07/27 20:42:10 yusuf Exp $";
#endif /* lint */



#include "taper.h"

PRIVATE int info_fd;
PRIVATE _errstat write_info(_vptr buf, _s32 szbuf, char *lmes)
{
    if (write(info_fd, buf, szbuf) != szbuf) {
	write_error_log(lmes);
	return do_exit(ERROR_WRITING_INFO);
    }
    return 0;
}

PUBLIC _u32 mktr, mktrc;
PRIVATE _errstat mkinfo_action(struct file_info *fi, char *fn, struct oa_file_entry *dummy)
{
    char l[MAX_FNAME];
    _s32  x, p=0;
    
    strcpy(l, "Writing filename & file info");
    if (log_level > 2) write_log(l);
    if (S_ISLNK(fi->mode)) 
      if (!fi->act_size) {
	  if (log_level > 2) {
	      sprintf(l, "Adding length of link to %s", fn);
	      write_log(l);
	  }
	  tape_read_namelen(l);
	  fi->act_size = strlen(l)+1+sizeof(_s32);
	  fi->size = strlen(l)+1+sizeof(_s32);
	  p = 1;
      }
    if (write_info(fi, sizeof(struct file_info), l) == -1) return -1;
    if (write_info(fn, fi->name_len, l) == -1) return -1;
    x = sizeof(*fi)+fi->name_len;
    ifd.info_file_size += x;
    mktr += x; mktrc += x;
    if (S_ISLNK(fi->mode) || S_ISREG(fi->mode)) {
	mktr += fi->act_size;
	mktrc += fi->size;
    }
    
    ifd.no_in_archive++;
    if (fi->checksum == -1) {
	sprintf(l, "There was a problem when backing up file %s, therefore file is ignored", fn);
	write_warning_log(l);
    }
    if ((!old_archive) && (fi->checksum == -2)) {
	sprintf(l, "Backup was aborted before %s was backed up", fn);
	write_warning_log(l);
    }
    return p;
}


PUBLIC _s32 file_no;
PUBLIC _s8 passing;				 /* for cumulative  */
PUBLIC void mkinfo_print_status(WINDOW *mes_box, _s32 cur_in_vol, 
			 _s32 no_in_vol, _s32 vol,
			 _u32 file_size, char *fn,
			 time_t t_start, time_t t_current)
{
    char num1[50], scr[MAX_FNAME], num2[50];
    _s32 x;
    
    (passing == TRUE) ? sprintf(scr, "Passing %s", fn) :
                        sprintf(scr, "Reading %s", fn);
    status_box(mes_box, scr, 0, FALSE, 1);
    if (restore_mode != RESTORE_VERIFY) file_no++;   /* in verify, will be updated in restore */
    sprintf(scr, "Volume %d. File %d of %d", vol, cur_in_vol, no_in_vol);
    status_box(mes_box, scr, 2, FALSE, 1);
    sprintf(scr, "Total read: file %d, size %s", file_no, print_kb(num1, mktr));
    status_box(mes_box, scr, 3, FALSE, 1);
    sprintf(scr, "Time elapsed %s", convtime(num1, t_start, t_current));
    status_box(mes_box, scr, 5, FALSE, 1);
    x=t_current-t_start;
    if (x) {
	(passing == TRUE) ?
	sprintf(scr, "Mkinfo rate %s/min [%s/min]",
		print_mb(num1, mktr/x*60),
		print_mb(num2, mktrc/x*60)) :
	sprintf(scr, "Restore rate %s/min [%s/min]",
		print_mb(num1, mktr/x*60),
		print_mb(num2, mktrc/x*60));
	status_box(mes_box, scr, 7, FALSE, 1);
    }
}


PUBLIC _errstat mkinfoce(struct file_info *fi, char *fn)
{
/* Function called by traverse if checksum error encounterd 
   If wants to abort, returns -1, 0 otherwise
 */
    char s[4][150];
    _s32 x;
    
    if (bad_checksum != 1) return 0;		 /* don't ask if doesn't want it */
    sprintf(s[0], "Checksum error for");
    my_strcpy(s[1], fn, 60);
    strcpy(s[2], "");
    sprintf(s[3], "Assume aborted backup");
    if (multi_message_box(s, 4, MB_YESNO) == 1) {
	x = sizeof(*fi)+fi->name_len;		 /* remove things placed into info file */
	lseek(info_fd, -x, SEEK_CUR);		 /* move back pointer */
	ifd.info_file_size -= x;
	ifd.no_in_archive--;
	return -1;
    }
    return 0;
}


int advance_volume(WINDOW *mes_box)
{
/* Advances tape by one volume 
   returns -1 if error, 0 otherwise
 ~*/    
    if (have_fsf) {				 /* if a tape device is being */
	status_box(mes_box, "", 0, FALSE, 1);
	status_box(mes_box, "", 2, FALSE, 1);
	status_box(mes_box, "", 5, FALSE, 1);
	status_box(mes_box, "", 6, FALSE, 1);
	status_box(mes_box, "Looking for next volume", 3, FALSE, 1);
	if (fast_fsf) {
	    if (log_level > 2) write_log("Doing a fast FSF");
	    if (tape_fsf(1, TRUE) == -1)	 /* unable */
	      return -1;			 /* assume EOT */
	    read_offset = -1;
	}
	else {					 /* slow fsf */
	    tape_close();			 /* reposition tape */
	    /* should rewind since we expect a rewinding to be open */
	    if (ntape_open(O_RDONLY) == -1) {	 /* it's a pity that we have to rewind */
		write_fatal_log("Opening non-rewinding device to do fsf");
		return -1;
	    }
	    vols_passed++;			 /* passed another vol on this tape */
	    if (tape_fsf(vols_passed, TRUE) == -1)/* couldn't fsf - assume end of data */
	      return -1;
	    tape_close();		
	    if (tape_open(O_RDONLY) == -1) {/* open rewinding */
		write_fatal_log("Reopening device after repositioning");
		return -1;
	    }
	}
    }
    else {
	if (log_level > 2) write_log("Simulating fsf by discarding rest of block");
	blocks_passed = 0;			 /* new volume */
	read_offset = -1;
    }
    return 0;
}


PRIVATE _time_t t_start, t_current;
PUBLIC _errstat mkinfo_loop(file_passed_action action, print_status ps)	 
{
/* This loops through the archive. It calls action for each file
 * in encounters.
 * 
 * We use the non-rewinding device so that when the end of tape is
 * reached, the user doesn't have to wait for the device to rewind
 * before s/he can insert the next tape
 * 
 * Note that for regular files, when moving on to a new volume, then
 * the rest of the data from the block currently read must be discarded
 * because there is no easy was to do a tape_fsf(dv,1) on regular files.
 * 
 * Assumes a rewinding tape device is open and tape header has been read in
 *
 * Returns -1 if an error occurred
 */
    WINDOW *mes_box=NULL;
    _s32  cur_vol, in_vol;
    struct volume_tape_info *vi;
    struct volume_header vh, v;
    _s32 oldtsi;
    _u32 ifs, tr;
    char  s[200];

    file_no=0; 
    mktr = 0; mktrc = 0;
    mes_box = status_box(mes_box, "                                               ", 2, TRUE, 5);/* create window */
    nodelay(mes_box, TRUE);
    status_box(mes_box, "Identifying tape", 3, FALSE, 1);
    ifd.magic = INFO_MAGIC;
    if (check_tape(mes_box, 3, 1) == -1) goto err;/* make sure first tape in drive */
    cur_vol = 0;				 
    t_start = time(NULL);
    if (read_volheader(&vh, 1, 1) == -1) goto err;
    ifd.size_volume_headers = vh.size_header;
    ifd.number_volumes = 0;
    while (1) {
	ifd.number_volumes++;
	vt_info = my_realloc(vt_info, sizeof(struct volume_tape_info)*ifd.number_volumes);
	vi = vt_info+ifd.number_volumes-1;	 /* update volume/tape info */
	vi->volume = ifd.number_volumes;
	vi->start_tape = tdh.tape_number;
	if (vh.volume_magic == VOLUME_MAGIC_INFO) {	 /* this volume is an info file - skip it */
	    status_box(mes_box, "Skipping past info file", 3, FALSE, 1);
	    if (tape_read(&ifs, sizeof(_u32)) == -1) goto err;   /* read size of info file */
	    tr = 0;
	    while (tr < ifs) {			 /* skip past info file */
		if (tape_read(tr_buffer, min(ifs-tr, max_tr_size)) == -1) return -1;
		tr += min(ifs-tr, max_tr_size);
	    }
	}
	else {
	    if (log_level > 2) {
		sprintf(s, "Mkinfo header : volume=%d, no in vol=%d, no sels=%d",
			ifd.number_volumes, vh.no_in_volume, vh.no_sels);
		write_log(s);
	    }
	    if (traverse_volume(action, vh.no_in_volume, t_start, 
				mes_box, 1, ps, &in_vol, FALSE, 
				(action == mkinfo_action) ? mkinfoce : NULL) < 0) /* traverse this volume */
	      goto err;
	}
	memcpy(&v, get_vh(vol_headers, cur_vol+1), sizeof(struct volume_header));   /* needed to ensure */
	v.no_in_volume = in_vol;		 /* aligned access */
	memcpy(get_vh(vol_headers, cur_vol+1), &v, sizeof(struct volume_header));
	cur_vol++;
	vi->end_tape = tdh.tape_number;
	vi->blocks_on_last_tape = blocks_passed;
	oldtsi = ifd.number_tsi;
	if (advance_volume(mes_box) == -1) break;
	memset(&vh, 0, sizeof(vh));
	if (tape_read_volheader(&vh, 1) == -1) break;
	if ((vh.volume_magic != VOLUME_MAGIC) &&
	    (vh.volume_magic != VOLUME_64_MAGIC)) /* not magic */
	  break;				 /* therefore assume eot */
	if (vh.no_in_volume == -1) {		 /* volumes subsequently have been deleted */
	    if (log_level > 2) write_log("Previously erased volumes detected");
	    break;
	}
	if (read_volheader(&vh, 0, 1) == -1) goto err;
	ifd.size_volume_headers += vh.size_header; /* update size of volume headers */
    }

    ifd.number_tsi = oldtsi;			 /* ignore the last one while getting next volume */
    close_statusbox(mes_box);			 /* some tape drives return end tape when no more volumes */
    return 0;
    
    err:;
      close_statusbox(mes_box);
      return -1;
}


PRIVATE _errstat recreate(void) 
{
    WINDOW *mes_box=NULL;
    char   ifn[MAX_FNAME];

    passing = TRUE;
    info_fd = 0;
    mes_box = status_box(mes_box, "Rewinding tape", 2, TRUE, 4);/* create window */
    if (tape_rewind() == -1) goto err;
    status_box(mes_box, "Identifying tape", 2, FALSE, 1);
    if (tape_open(O_RDWR) == -1) goto err;	 /* open rewinding device */
    if (tape_readheader(&tdh, 0) == -1) goto err;
    if ((tdh.magic != TAPER_MAGIC_NUMBER) && (tdh.magic != TAPER_64_MAGIC)) {
	if (tdh.magic == TAPER_4_MAGIC_NUMBER)
	  message_box("Sorry, taper 4 archives are not compatible", MB_OK);
	do_exit(ERROR_MAGIC);
	goto err;
    }

    memset(&ifd, 0, sizeof(ifd));		 /* clear all variables */
    ifd.archive_id = tdh.archive_id;
    strcpy(ifd.archive_title, tdh.archive_title);
    ifd.number_tapes = 1;
    ifd.info_file_size = sizeof(ifd);
    ifd.no_in_archive = 0;
    ifd.number_tsi = 0;
    update_tsi = 1;
    ifd.number_volumes = 1;
    print_title_line();				 /* refresh title line */
    if ((info_fd = open_info_file(FALSE, tdh.archive_id)) == -1) goto err;
    vt_info = my_realloc(vt_info, sizeof(struct volume_tape_info));
    vt_info->volume = 1;			 /* first volume always on tape 1 */
    vt_info->start_tape = 1;

    if (write_info(&ifd, sizeof(ifd), "Writing info file header") == -1) {
	do_exit(ERROR_WRITING_INFO);
	goto err;
    }

    if (mkinfo_loop(mkinfo_action, mkinfo_print_status) == -1) {
	if (log_level > 2) write_log("Mkinfo loop returned an error");
	goto err;
    }
    touchwin(mes_box);
    status_box(mes_box, "Closing & Rewinding tape", 2, FALSE, 1);
    if (log_level > 2) write_log("Writing volume header information");
    if (write(info_fd, vol_headers, ifd.size_volume_headers) == -1) {
	do_exit(ERROR_WRITING_INFO);
	goto err;
    }
    if (log_level > 2) write_log("Writing VT information");
    if (write(info_fd, vt_info, sizeof(struct volume_tape_info)*ifd.number_volumes) == -1) {
	do_exit(ERROR_WRITING_INFO);
	goto err;
    }
    if (log_level > 2) write_log("Writing tape size information");
    if (ifd.number_tsi)
      if (write(info_fd, tsi, sizeof(struct tape_size_info)*ifd.number_tsi) == -1) {
	  do_exit(ERROR_WRITING_INFO);
	  goto err;
      }
    if (log_level > 2) write_log("Rewinding and writing header information");
    lseek(info_fd, 0, SEEK_SET);		 /* update info file */
    if (write_info(&ifd, sizeof(ifd), "Updating info file header") == -1) {
	do_exit(ERROR_WRITING_INFO);
	goto err;
    }
    if (log_level > 2) write_log("Closing info file");
    close(info_fd);				 /* close info file */
#ifdef TAPER_BIG_ENDIAN    
/* If using big endian, the info file has to be converted
 * to little endian */
    status_box(mes_box, "Endianizing info file", 2, FALSE, 1);
    info = my_realloc(info, ifd.info_file_size);
    if ((info_fd = open_info_file(TRUE, tdh.archive_id)) == -1) goto err;
    if (read(info_fd, info, ifd.info_file_size) != ifd.info_file_size) goto err;
    close(info_fd);
    if (write_out_info_file(info, ifd.archive_id) == -1) goto err;
#endif    
    if (compress_info) {
	compress_info_file(ifd.archive_id);
	status_box(mes_box, "Compressing info file", 2, FALSE, 1);
    }
    close_statusbox(mes_box);
    return 0;
    
    err:;
	if (info_fd) close(info_fd);
	make_info_filename(ifn, tdh.archive_id); /* erase info file */
	unlink(ifn);
        if (mes_box) close_statusbox(mes_box);
	return -1;   
}

    
PRIVATE _errstat mkinfo_do_mallocs(void)
{
    vt_info = my_malloc(1);
    tsi = my_malloc(1);
    vol_headers = my_malloc(sizeof(_s32));
    info = my_malloc(1);
    if ((info == NULL) || (vt_info == NULL) || (vol_headers == NULL)) return -1;
    return 0;
}


PRIVATE void mkinfo_free_memory(void)
{
    my_free(vt_info);
    my_free(tsi);
    my_free(vol_headers);
    my_free(info);
}


void taper_mkinfo(void)
{
    _s32   x;
    char  sa[9][150];
    char  s2[30], s3[30];

    if (!message_box("Insert tape in drive", MB_OKCANCEL))
      return;
    if (mkinfo_do_mallocs() == -1) return;
    if (open_logfile("Mkinfo") == -1) return;
    if (check_device_names() == -1) return;	 /* check devices & other parms */
    recreate(); 
    tape_close();
    touchwin(win_main); wrefresh(win_main);
    t_current = time(NULL);
    x = t_current - t_start;
    if (x==0) x=1;
    sprintf(sa[0], "Mkinfo/Check archive finished");
    strcpy(sa[1], "");
    sprintf(sa[2], "Found: %d files in  %d volumes, ", ifd.no_in_archive,
	    ifd.number_volumes);
    if (log_level > 1) write_log(sa[2]);
    sprintf(sa[3], "Total on archive %s [%s]. Ratio %.2f", 
	    print_mb(s2, mktr),
	    print_mb(s3, mktrc),
	    (mktr == 0) ? 1 : (float) mktrc / (float) mktr);
    if (log_level > 1) write_log(sa[3]);
    strcpy(sa[4], "");
    sprintf(sa[5], "Time elapsed %s.", convtime(s2, t_start, t_current));
    if (log_level > 1) write_log(sa[5]);

    sprintf(sa[6], "Mkinfo rate %s/min [%s/min]", 
	    print_mb(s2, mktr/x*60),
	    print_mb(s3, mktrc/x*60));
    if (log_level > 1) write_log(sa[6]);
    strcpy(sa[7], "");
    sprintf(sa[8], "%d warnings, %d errors", log_warnings, log_errors);
    if (log_level > 1) write_log(sa[8]);
    multi_message_box(sa, 9, MB_OK);
    mkinfo_free_memory();
    close_logfile("Mkinfo");
    return;			                 /* succesful return */
}
