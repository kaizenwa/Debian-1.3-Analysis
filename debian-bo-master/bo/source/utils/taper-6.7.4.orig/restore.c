/*
   Time-stamp: <96/08/05 19:57:22 yusuf>

   $Id: restore.c,v 1.25 1996/08/05 19:02:01 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: restore.c,v 1.25 1996/08/05 19:02:01 yusuf Exp $";
#endif /* lint */



#include "taper.h"
#include "restore.h"


/* Functions for using mkinfo to do total restores */
extern PUBLIC _s32 file_no;					 /* for cumulative  */
extern PUBLIC _u32 mktr, mktrc;
PUBLIC _errstat mkinfo_loop(file_passed_action action, print_status ps);
PUBLIC void mkinfo_print_status(WINDOW *mes_box, _s32 cur_in_vol, 
			 _s32 no_in_vol, _s32 vol,
			 _u32 file_size, char *fn,
			 time_t t_start, time_t t_current);
PRIVATE FILE *fdfifo;				 /* for the FIFO */
PRIVATE char fifo_name[MAX_FNAME];


PRIVATE struct oa_file_entry *find_first_file(_s32 *cpos, struct oa_file_entry *fe)
{
/* Looks through the currently selected files and works out
 * which is the first to be read in
 * 
 * Assumes archive directory is sorted in pos_in_archive order
 * so can break as soon as it finds a file to be restored
 * 
 * Starts search at fe
 * 
 * Returns a pointer to the file entry
*/

    while (*cpos <  no_in_archive) {
	if (fe->selected)			 /* only have to go through archive*/
	  if (S_ISREG(fe->i.mode) || (S_ISLNK(fe->i.mode))) /* to recreate reg files & */
	    return fe;
	advance_ofe(&fe);			 /* we have information on everything else */
	(*cpos)++;
    }
    return NULL;
}


PUBLIC _errstat restorece(struct file_info *fi, char *fn)
{
/* Function called by traverse if checksum error encounterd 
   If wants to abort, returns -1, 0 otherwise
 */
    char s[4][150];

    if (bad_checksum != 1) return 0;		 /* no prompting  */
    sprintf(s[0], "Checksum error for");
    my_strcpy(s[1], fn, 60);
    strcpy(s[2], "");
    sprintf(s[3], "Assume aborted backup");
    if (multi_message_box(s, 4, MB_YESNO) == 1)
      return -1;
    return 0;
}


PRIVATE _u32 rtotal_read;
PRIVATE _u32 rtotal_processed;
PRIVATE _u32 rtotal_files;
PRIVATE _errstat restore_action(struct file_info *fi, char *fn, struct oa_file_entry *fe)
/* Acts on the restore loop finding filename fn
 * Action returns 1 if it advanced past file (ie. it read it in)
 * or 0 if it didn't read it in
 * 
 * Returns -1 if error
 * Returns -2 if no more files to be restored from this volume and did read this file
 * Returns -3 if no more files to be restored and didn't read this file
 * Returns -4 if no more files to be read in from archive
*/ 
{
    char      s[MAX_FNAME], l[MAX_FNAME*2], tmpf[MAX_FNAME];
    _errstat  ret, x;

    if (restore_mode == RESTORE_NORMAL) {	 /* if not doing full backup */
	rtotal_processed += fi->name_len+sizeof(struct file_info);   /* make sure we want this file */
	if (S_ISREG(fi->mode))
	  rtotal_processed += fi->act_size;
	if (!fe->selected)			 /* not selected */
	  return 0;				 /* tell loop that we didn't touch it */
	fe->selected = 0;			 /* mark as unselected now that it's read */
	ret = -3;
    }
    else
      ret = 0;

    if (fi->checksum == -1) {
	sprintf(l, "There was a problem when backing up file %s, therefore file not restored", fn);
	write_warning_log(l);
	if (ret == -4) return ret;
	return (ret == -3) ? -2 : 1;		 /* we haven't passed file but it was never backed up*/
    }

    if ((fi->checksum == -2) && (!old_archive)) {
	sprintf(l, "Backup was aborted before %s was backed up", fn);
	write_warning_log(l);
	if (ret == -4) return ret;
	return (ret == -3) ? -2 : 1;		 /* we haven't passed file but it was never backed up*/
    }

    switch(restore_mode) {
     case RESTORE_NORMAL:
	rtotal_files++;				 /* increment file count */
	strcpy(fn, stripped_cf);		 /* make filename as user */
	if (*stripped_cf)			 
	  strcat(fn, "/");			 /* wants it */
	strcat(fn, name(fe));
	rtotal_read += fi->name_len+sizeof(struct file_info);    
	break;
     case RESTORE_FULL:
	strcpy(s, fn);				 /* remove leading / */
	strcpy(fn, &s[1]);
	break;
     case RESTORE_VERIFY:
	strcpy(s, fn);
	break;
    }

    if (restore_mode != RESTORE_VERIFY) {
	if (*rel_path) {				 /* make pathname */
	    strcpy(s, rel_path);			 /* taking into account */
	    if (!((s[strlen(s)-1] == '/') ||	 /* the relative path supplied by */
		  (fn[0] == '/')))			 /* the user */
	      strcat(s, "/");
	    strcat(s, fn);
	}
	else 
	  strcpy(s, fn);
    }
    
    mktr += sizeof(*fi)+fi->name_len;
    mktrc += sizeof(*fi)+fi->name_len;
    file_no++;					 /* increment file count */

    if (!S_ISREG(fi->mode)) 			 /* if not reg file, checksum should be 0 */
	if (fi->checksum != 0) 
          if (restore_mode != RESTORE_NORMAL)
	      if (restorece(fi, s) == -1) 	 /* user wants to abort this volume */
                return -4;
		
    if (S_ISREG(fi->mode)) { 
	rtotal_read += fi->size;
	mktr += fi->act_size;
	mktrc += fi->size;
    }
    else {					 /* only create directories if not reg files */
	if (restore_mode != RESTORE_VERIFY) 	 /* don't make dirs if just a verify */
	  if (!make_dirs(s))			 /* make directories */
	  return ret;				 /* failed in making directories */
    }

    if (S_ISLNK(fi->mode)) {			 /* soft link */
	sprintf(l, "Reading in link info for %s", s);
	if (log_level > 2) write_log(l);
	if (tape_read_namelen(tmpf) == -1) 	 /* read link name */
	  return -1;
	if (restore_mode == RESTORE_VERIFY) 
	  return (ret == -3) ? -2 : 1;		 /* we passed file */
	    
	sprintf(l, "Creating symbolic link file %s", s);
	if (log_level > 1) write_log(l);
	if (symlink(tmpf, s) == -1) 
	  write_error_log(l);
	return setowners(s, ret, fi);
    }

    if (S_ISDIR(fi->mode))  {			 /* directory */
	if (restore_mode == RESTORE_VERIFY) 
	  return (ret == -3) ? -2 : 1;		 /* we passed file */
	if (s[strlen(s)-1] == '/')
	  s[strlen(s)-1] = 0;
	sprintf(l, "Creating directory %s", s);
	if (log_level > 1) write_log(l);
	if (mkdir(s, 493) == -1)
	  if (errno != EEXIST)			 /* directory exists is not an error */
	    write_error_log(l); 
	return setowners(s, ret, fi);
    }

    if (!S_ISREG(fi->mode)) {			 /* device */
	if (restore_mode == RESTORE_VERIFY) 
	  return (ret == -3) ? -2 : 1;		 /* we passed file */
	sprintf(l, "Creating device %s", s);
	if (log_level > 1) write_log(l);
	if (mknod(s, fi->mode, fi->dev) == -1)
	  write_error_log(l);
	return setowners(s, ret, fi);
    }
    
/* OK - this is a regular file. Write the details to the FIFO
 * and write the file to a temporary file */

    fprintf(fdfifo, "%s\n", s); 		 /* write filename */
    taper_tmpnam(tmpf);				 /* make temporary name to store data in */
    if (log_level > 2) {
	sprintf(l, "Writing %s to %s to FIFO", s, tmpf);
	write_log(l);
    }
    
    x = read_into_temp(fi, tmpf, s);
    if (x == -1) 
      fprintf(fdfifo, "%s\n", FIFO_ERR);	 /* couldn't create temp file */
    if (x == -2) {
	if (restore_mode != RESTORE_NORMAL)
	  if (restorece(fi, s) == -1) 		 /* user wants to abort this volume */
	      return -4;
    }
    if (x != -1) {
	fprintf(fdfifo, "%s\n", tmpf);		 /* queue it for child to process */
	fwrite(fi, sizeof(*fi), 1, fdfifo);	 /* write file info */
    }
    fflush(fdfifo);
    if (ret == -4) return ret;
    return (ret == -3) ? -2 : 1;		 /* we passed file */
}
								     

void restore_print_status(WINDOW *mes_box, _s32 cur_in_vol,
				_s32 no_in_vol, _s32 vol,
				_u32 file_size, char *fn,
				_time_t t_start, _time_t t_current)
{
    char scr[MAX_FNAME], num1[50], num2[50];
    _s32 x;
    
    sprintf(scr, "Reading %s", fn);
    status_box(mes_box, scr, 0, FALSE, 1);
    if (cur_in_vol) 
      sprintf(scr, "Volume %d of %d. File %d of %d", vol, ifd.number_volumes,
	      cur_in_vol, no_in_vol);
    else
      sprintf(scr, "Volume %d of %d", vol, ifd.number_volumes);
    status_box(mes_box, scr, 2, FALSE, 1);
    sprintf(scr, "Read %s of %s", print_kb(num1, rtotal_read), 
	    print_kb(num2, total_selected));
    status_box(mes_box, scr, 3, FALSE, 1);
    if (cur_in_vol) {
	sprintf(scr, "Passing %s of %s", print_kb(num1, rtotal_processed), 
		print_kb(num2, total_compressed));
	status_box(mes_box, scr, 4, FALSE, 1);
    }
    t_current = time(NULL);
    if (total_selected) 
      sprintf(scr, "%.0f%% done, time elapsed %s",  (float) rtotal_processed/(float) total_compressed*100,
	      convtime(num1, t_start, t_current));
    else				 /* if total bytes=0 then base on # files */
      sprintf(scr, "Time elapsed %s", convtime(num1, t_start, t_current));
    status_box(mes_box, scr, 6, FALSE, 1);
    x=t_current-t_start;
    if (x) {
	sprintf(scr, "Restore rate %s/min [%s/min]",
		print_mb(num1, rtotal_read/x*60),
		print_mb(num2, rtotal_processed/x*60));
	status_box(mes_box, scr, 7, FALSE, 1);
    }

}


PRIVATE _errstat restore_devs_dirs(void)
/* This routine restores anything other than regular files & links. It doesn't
   need to go through the archive for these - it has the information in the
   info file
*/
{
    _s32 c;
    struct oa_file_entry *fe;
    char fn[MAX_FNAME];
    
    fe = archive_files;
    for (c=0; c<no_in_archive; c++) {
	if (fe->selected) 
	    if (!(S_ISREG(fe->i.mode) || S_ISLNK(fe->i.mode))) {
		strcpy(fn, cf);			 /* make the full pathname */
		strcat(fn, name(fe));
		if (restore_action(&fe->i, fn, fe) == -1) 
		  return -1;
	    }
	advance_ofe(&fe);
    }
    return 0;
}


PRIVATE time_t t_start, t_current;		 /* for time stats */
PRIVATE _errstat start_restore(WINDOW *mes_box)
{
/* Does the initial things for restore:
 * 
 * reads tape header, checks proper archive,
 * sets up child process etc..
 * 
 * Returns 0 if all OK, -1 otherwise
 * 
*/
    char l[200];
    char s2[30], s3[30], s4[30], s5[10];

    file_no = 0;
    status_box(mes_box, "Identifying tape", 2, FALSE, 1);
    if (tape_rewind() == -1) return -1;		 /* rewind tape */
    if (tape_open(O_RDONLY) == -1)		 /* open rewinding */
	return -1;
    if (tape_readheader(&tdh, 0) == -1) return -1;   /* read tape header */
    if ((tdh.magic != TAPER_MAGIC_NUMBER) && (tdh.magic != TAPER_64_MAGIC))
	return do_exit(ERROR_MAGIC);
    taper_tmpnam(fifo_name);			 /* set up FIFO */
    if ((mknod(fifo_name, S_IFIFO|S_IREAD|S_IWRITE, 0)) == -1)  
	return do_exit(ERROR_CREATING_FIFO);
    if (restore_child) {				 /* a process still going on */
	kill(restore_child, SIGKILL);
	waitpid(restore_child, NULL, 0);
	restore_child = 0;
    }
    if (log_level > 3) write_log("About to fork off restore child");
    restore_child = fork();			 /* fork off a child to do restoration */
    if (restore_child == -1) {
	unlink(fifo_name);
	restore_child = 0;
	return do_exit(ERROR_UNABLE_FORK);
    }
    if (restore_child == 0) {			 /* this is the child */
	close(dv);
	chdir(original_cur_dir);
#ifdef TRIPLE_BUFFER
	sprintf(l, "R:Executing bg_restore %s %d %s %d %d %d \"%s\"",
		log_file, log_level, fifo_name, shm_id, ovrwrite, restore_mode, temp_dir);
	sprintf(s2, "%d", shm_id);
#else
	sprintf(l, "R:Executing bg_restore %s %d %s %d %d %d \"%s\"",
		log_file, log_level, fifo_name, 0, ovrwrite, restore_mode, temp_dir);
	sprintf(s2, "%d", 0);
#endif
	if (log_level > 3) write_log(l);
	sprintf(s3, "%d", log_level);
	sprintf(s4, "%d", ovrwrite);
	sprintf(s5, "%d", restore_mode);
	if (execlp("bg_restore", "bg_restore", log_file, s3, fifo_name, s2, s4, s5, temp_dir, NULL) == -1) {
	    write_error_log("Unable to start background restore");
	    fclose(fopen(fifo_name, "r"));
	    exit(-1);			 /* should never get here */
	}
    }
    
    if ((fdfifo = fopen(fifo_name, "w")) == NULL) {/* open fifo */
	if (restore_child) {
	    kill(restore_child, SIGTERM);
	    waitpid(restore_child, NULL, 0);
	    restore_child = 0;
	    unlink(fifo_name);
 	}
	return -1;
    }
    sleep(2);					 /* give time for restore child to start */
    waitpid(restore_child, NULL, WNOHANG);	 /* in case child not started */
    if (kill(restore_child, SIGUSR1) == -1)	 /* restore child couldn't start */
      if (errno == ESRCH) {
	  unlink(fifo_name);
	  return do_exit(ERROR_NO_RESTORE_CHILD);
      }
    t_start = time(NULL);
    return 0;
}


PRIVATE void end_restore(void)
{
/* Closes tape, tells child we are finished etc..
*/
    if (fdfifo != NULL)	{			 /* tell FIFO we are finished */
	fprintf(fdfifo, "0\ndummy\nEND\n");
	fclose(fdfifo);
    }
    if (restore_child) waitpid(restore_child, NULL, 0);/* wait for child to finish */
    restore_child = 0;
    t_current = time(NULL);
    tape_close();
    touchwin(win_main); wrefresh(win_main);
}
								     


int calc_file_pos(_s32 *lpos, struct oa_file_entry **cur_fe, struct oa_file_entry *fe, _s32 *tape, _s32 *block, _s32 *offset)
{
/* Works out exactly where abouts 'fe' is in an archive:
 * 
 *  on what tape, on what volume, on what block within that tape 
 *   (block relative to start of volume) and how many bytes in that
 *   block we are
 * 
 * If we 'position' the tape (ie. by asking for a tape that
			      is mid volume)
 * then we return 1
 * 
 * if cur_fe is not NULL, then assumes that we have already found the
 * position of cur_fe and it is in block, tape etc..
 * 
 * 
 * Of course, assumes that info file is in memory
*/
    struct volume_tape_info *vti=vt_info;
    struct oa_file_entry *fe1;
    struct volume_header *vh;
    int  mw;
    _s32 c;
    
    while (1) {					 /* find on what tape */
	if (vti->volume == fe->i.volume) break;	 /* this vol begins */
	vti++;
    }
    if (*cur_fe == NULL) {			 /* start search at beginning */
	*tape = vti->start_tape;
	*block = 0;
	vh = (struct volume_header *) get_vh(vol_headers, fe->i.volume);
	*offset = vh->size_header;
	if (fe->i.volume == 1)
	  *offset += sizeof(struct tape_header);
    }
    fe1 = (*cur_fe == NULL) ? archive_files : *cur_fe;
    if (*cur_fe == NULL) *lpos = 0;
    while (*lpos < fe->i.pos_in_archive-1) { /* calc size of files prior to this */
	if (fe1->i.volume == fe->i.volume) {
	    *offset += fe1->i.name_len;
	    *offset += sizeof(struct file_info);
	    if (!S_ISDIR(fe1->i.mode)) 
	      if ((fe1->i.checksum != -1)  
	         && !((fe1->i.checksum == -2) && (!old_archive)) )
	      *offset += fe1->i.act_size;
	    if (*offset/block_size) {
		*block  += *offset/block_size;
		*offset = *offset%block_size;
	    }
	}
	advance_ofe(&fe1);
	(*lpos)++;
    }
    *cur_fe = fe1;
/* We now now how many blocks into the volume we need to go */
/* We now have to work out whether we have to use a different tape */
    c = 0;
    mw = 0;
    while (c < ifd.number_tsi) {
	if (tsi[c].volume == fe->i.volume) {
	    if (*block < tsi[c].blocks-1) return mw;/* this block is on this tape */
	    if (*block == tsi[c].blocks-1) {	 /* accounts for want something from last block */
		if (*offset <= block_size - tsi[c].lb_bytes_short) return mw;
	    }
	    (*tape)++; mw=1;
	    *offset += sizeof(struct tape_header);
	    *block -= (tsi[c].blocks-1);
	    *offset -= (block_size - tsi[c].lb_bytes_short);
	    if (*offset < 0) {
		(*block)--;
		*offset += block_size;
	    }
	}
	c++;
    }
    return mw;
}
								     

PRIVATE void do_restore(void)
{
    WINDOW *mes_box=NULL;
    char l[200];
    struct oa_file_entry *fe, *cur_fe, *fcur_fe;
    _s32 tape, block, offset, at_vol, lpos, flpos, z=0, lvol;
    char fn[MAX_FNAME];
    struct file_info fi;
    char sa[6][150];
    char s2[30];
    
/* An error reading from the backup device is a fatal error */
    if ((!no_sel) && (restore_mode == RESTORE_NORMAL))
      return;
    rtotal_read = 0;
    rtotal_processed = 0;
    rtotal_files = 0;
    mes_box = status_box(mes_box, "                                         ", 2, TRUE, 5);/* create window */
    if (sort_dir) {
	status_box(mes_box, "Sorting archive directory", 2, FALSE, 1);
	do_sort_dir(0);
    }
    nodelay(mes_box, TRUE);
    status_box(mes_box, "Restoring devices/directories", 2, FALSE, 1);
    if (restore_devs_dirs() == -1) return;
    fe = find_first_file(&z, archive_files);	 /* get first file to restore */
    if (fe == NULL) return;			 /* no files to restore */
    if (start_restore(mes_box) == -1) return;	 /* do initial things */
    
    at_vol = 1;
    lvol = 0;
    fcur_fe = archive_files; flpos = 0;
    while (1) {
	fe = find_first_file(&flpos, fcur_fe);
	fcur_fe = fe;
	if (fe == NULL) goto fin;		 /* no files to restore */
	if (fe->i.volume != lvol) cur_fe = NULL; /* reset search */
	status_box(mes_box, "", 0, FALSE, 1);
	status_box(mes_box, "", 2, FALSE, 1);
	status_box(mes_box, "", 4, FALSE, 1);
	status_box(mes_box, "", 5, FALSE, 1);
	status_box(mes_box, "", 6, FALSE, 1);
	status_box(mes_box, "", 7, FALSE, 1);
	if (calc_file_pos(&lpos, &cur_fe, fe, &tape, &block, &offset))/* work out exactly where this file is */
	  at_vol = fe->i.volume;
	if (check_tape(mes_box, 3, tape) == -1) goto fin;
	if (at_vol != fe->i.volume) 		 /* position on correct volume */
	    if (goto_end_vol(mes_box, 3, fe->i.volume-1, at_vol, 0, FALSE) == -1) goto fin;
	at_vol = fe->i.volume;
	if (tape_goto_block(mes_box, 3, block) == -1) goto fin;
	if ((read_offset != offset) && (offset)) {
	    if (read_offset > offset) {
		sprintf(l, "INTERNAL ERROR: At offset %d but want to go to %d",
			read_offset, offset);
		write_log(l); log_errors++;
		goto fin;
	    }
	    if (log_level > 2) {
		sprintf(l, "Offset is %d", offset - ((read_offset == -1) ? 0 : read_offset));
		write_log(l);
	    }
	    if (tape_read(write_buffer, offset - 
			  ((read_offset == -1) ? 0 : read_offset)) == -1) goto fin;
	}
	restore_print_status(mes_box, 0, 0, at_vol, fe->i.size, name(fe),
			     t_start, t_current);
	if (tape_read_fi(&fi) == -1) goto fin;
	if (tape_read(fn, fi.name_len) == -1) goto fin;
	restore_action(&fi, fn, fe);
    }
    
    status_box(mes_box, "", 0, FALSE, 1);
    status_box(mes_box, "Closing & Rewinding tape", 3, FALSE, 1);
    status_box(mes_box, "", 2, FALSE, 1);
    status_box(mes_box, "", 4, FALSE, 1);
    status_box(mes_box, "", 5, FALSE, 1);
    status_box(mes_box, "", 6, FALSE, 1);
    status_box(mes_box, "", 7, FALSE, 1);

    fin:;
    end_restore();
    sprintf(sa[0], "Restore Finished");
    strcpy(sa[1], "");
    sprintf(sa[2], "Restored: %d files,  %s", rtotal_files,
	    print_kb(s2, rtotal_read));
    if (log_level > 1) write_log(sa[2]);
    t_current = time(NULL);
    sprintf(sa[3], "Time elapsed %s.", convtime(s2, t_start, t_current));
    if (log_level > 1) write_log(sa[3]);
    strcpy(sa[4], "");
#ifdef TRIPLE_BUFFER
    sprintf(sa[5], "%d warnings, %d errors", log_warnings, log_errors);
#else						 /* if not using triple buffering */
    sprintf(sa[5], "Can't give warning/error count");
#endif    
    if (log_level > 1) write_log(sa[5]);
    multi_message_box(sa, 6, MB_OK);
}


    
PUBLIC _errstat read_vol_dir(_u32 archive_id) 	 /* also in utils.c */
{
    if (do_read_vol_dir(archive_id, tape, O_RDONLY, TRUE, TRUE) ==  TAPE_EXIST)
      return 1;
    return -1;
}

PRIVATE _s32 sz;
PRIVATE void print_diff_line(char *fn, struct stat *b)
{
    char s[80], s1[50];

    if (S_ISDIR(b->st_mode)) return;
    strncpy(s, fn, sizeof(s)-2);
    s[79] = 0;
    printf("  %-62s %12s\n", s, convert(s1, b->st_size));
    sz+=b->st_size;
}


PRIVATE _errstat restore_dpd(char *full_path, struct stat *b)
{
    print_diff_line(full_path, b);
    return 0;
}


PRIVATE void print_diffs(char *sel)
{
    struct stat b;
    
    get_statinfo(sel, &b);
    if (S_ISDIR(b.st_mode)) {
	printf("\nSelection %s\n", sel);
	process_dir(NULL, 0, sel, 1, restore_dpd, FALSE);
    }
    else
      if (file_more_recent(sel, &b))
        print_diff_line(sel, &b);
}


PRIVATE void print_diff(void)
{
/* Prints the files that have changed since an archive was made */
    _s32 c, c1, c2, c3;
    char   *xx1, *xx;
    struct volume_header vh, vh1;
    char s1[20];
    
    sz=0;
    printf("\n\nArchive %d ", tdh.archive_id);
    if (*tdh.archive_title)
      printf("%s\n", tdh.archive_title);
    else
      printf("<no title>\n");
    printf("Differences between archive and file system\n");
    printf("\nName                                                                     Size");
    printf("\n-----------------------------------------------------------------------------");
    xx = (char *) vol_headers;
    for (c=0; c<ifd.number_volumes;c++) {	 /* loop through each volume */
	memcpy((void *) &vh, xx, sizeof(struct volume_header));
	xx += sizeof(struct volume_header);
	for (c1=0; c1<vh.no_sels; c1++) {
	    xx += sizeof(_s32);
	    for (c2=0; c2<c; c2++) {		 /* make sure haven't already printed this filespec */
		xx1 = (char *) vol_headers;
		memcpy(&vh1, xx1, sizeof(struct volume_header));
		xx1 += sizeof(struct volume_header);
		for (c3=0; c3<vh1.no_sels; c3++) {   
		    xx1 += sizeof(_s32);
		    if (!strcmp(xx1, xx))	 /* we've already printed this */
		      goto already_printed;
		    while (*xx1++);
		    xx1 += sizeof(_s32); while (*xx1++);
		}
	    }
	    print_diffs(xx);
	    already_printed:
	      while (*xx++); xx +=sizeof(_s32); while (*xx++);
	}
    }
    printf("\n%-64s %12s\n", "TOTALS", convert(s1, sz));
}
    

PRIVATE void print_voldir(void) 
{
    _s32 c;
    struct oa_file_entry *fe;
    char   s[MAX_FNAME], s1[20], s2[20], *xx;
    struct tm t;
    struct volume_header vh;
    _s32   c_sz=0, u_sz=0;


    printf("Number volumes = %d\n", ifd.number_volumes);

    for (c=0; c<ifd.number_volumes;c++) {
	xx = (char *) get_vh(vol_headers, c+1);
	memcpy((void *) &vh, xx, sizeof(struct volume_header));
	if (*vh.volume_title)
	  printf("\nVolume %d %s\n", c+1, vh.volume_title);
	else
	  printf("\nVolume %d <no title>\n", c+1);
	printf("  Contains %d files\n", vh.no_in_volume);
	t = *localtime(&vh.backup_time);
	printf("  Backed up at %d/%d/%d %d:%d\n", t.tm_year, t.tm_mon+1, t.tm_mday,
		t.tm_hour, t.tm_min);
    }
    printf("\n\n%-25s %3s %5s %14s %12s %12s\n", "Pathname", "Vol", "Pos", "Backup", "File Size", "On tape");
    printf("----------------------------------------------------------------------------\n");
    fe = archive_files;
    for (c=0; c<ifd.no_in_archive; c++) {
	strcpy(s, name(fe)); s[22] = 0;
	t = *localtime(&fe->i.backup_time);
	printf("%-25s %3d %5d %2d/%2d/%2d %2d:%02d %12s %12s\n", s, abs(fe->i.volume), fe->i.pos_in_archive,t.tm_mday,
	       t.tm_mon+1, t.tm_year, t.tm_hour, t.tm_min, convert(s1, fe->i.size),
	       convert(s2, fe->i.act_size));
	c_sz += fe->i.act_size;
	u_sz += fe->i.size;
	advance_ofe(&fe);
    }
    printf("\n%-47s    %12s %12s\n", "TOTALS", convert(s1, u_sz), convert(s2, c_sz));
    return;
}

	
PRIVATE void restore_mfn(struct direntry *x, char *dir_name, char *prefix)
{
    char t[MAX_FNAME], pth[MAX_FNAME];
    int  fd;
    struct info_file_header ifd;
    
    strcpy(t, "ID ");
    strcat(t, &x->entry.d_name[strlen(prefix)]);
    strcpy(pth, dir_name);
    if (pth[strlen(pth)-1] != '/')
      strcat(pth, "/");
    strcat(pth, x->entry.d_name);
    fd = open(pth, O_RDONLY);
    if (fd != -1) {
	read(fd, &ifd, sizeof(ifd));
	strcat(t, " ");
	if (*ifd.archive_title)
	  strcat(t, ifd.archive_title);
	else
	  strcat(t, "<no title>");
	close(fd);
    }
    strcpy(x->entry.d_name, t);
}


PRIVATE void full_restore(void)
{
    char sam[8][150];
    char s2[50], s3[50];
    WINDOW *mes_box=NULL;
    long x;

    backrest_kill_windows();
    print_my_name();
    mes_box = status_box(mes_box, "Rewinding tape", 2, TRUE, 4);/* create window */
    if (start_restore(mes_box) == -1) return;
    ifd.archive_id = tdh.archive_id;		 /* so that title line prints correctly */
    ifd.number_tapes = -1;			 /* don't bother with updating etc.. */
    strcpy(ifd.archive_title, tdh.archive_title);
    print_title_line();				 /* refresh title line */
    mkinfo_loop(restore_action, mkinfo_print_status);
    end_restore();
    close_statusbox(mes_box);
    sprintf(sam[0], "%s Finished",
	    (restore_mode == RESTORE_VERIFY) ? "Verify": "Restore");
    strcpy(sam[1], "");
    sprintf(sam[2], "%s : %d files,  %s", 
	    (restore_mode == RESTORE_VERIFY) ? "Verified" : "Restored", file_no,
	    print_kb(s2, mktr));
    if (log_level > 1) write_log(sam[2]);
    strcpy(sam[3], "");
    t_current= time(NULL);
    sprintf(sam[4], "Time elapsed %s.", convtime(s2, t_start, t_current));
    if (log_level > 1) write_log(sam[4]);
    x = time(NULL) - t_start;
    sprintf(sam[5], "Full %s rate %s/min [%s/min]", 
	    (restore_mode == RESTORE_VERIFY) ? "verify": "restore",
            print_mb(s2, mktr/x*60),
	    print_mb(s3, mktrc/x*60));
    if (log_level > 1) write_log(sam[5]);
    strcpy(sam[6], "");
#ifdef TRIPLE_BUFFER
    sprintf(sam[7], "%d warnings, %d errors", log_warnings, log_errors);
#else						 /* if not using triple buffering */
    sprintf(sam[7], "Can't give warning/error count");
#endif    
    if (log_level > 1) write_log(sam[7]);
    multi_message_box(sam, 8, MB_OK);
}


PUBLIC _errstat select_archive(_u32 *archive_id, char noinfo) /* also used in utils.c */
{
/* Returns 0 if found archive
 * Returns -1 if error
 * Returns -2 if found archive but no info file 
 
 * If noinfo == FALSE, an info file must exist
 * If noinfo == TRUE, don't necessarily need an info file
 */
    
    _errstat ret;
    struct tape_header th;
    WINDOW *mes=NULL;
    char   df[MAX_FNAME];
    _s32    gdh;

    gdh = 0;
    if ((pr_dir == -1) || (diff_id == -1))	 /* work out if we need to */
      gdh = 1;					 /* read in tape header */
    if ((pr_dir == 0) && (diff_id == 0))
      gdh = 1;
    if (gdh) {
	if (!no_windows)
	  mes = status_box(mes, "Rewinding tape", 1, TRUE,  1); /* see what tape is */
	ret = get_tape_header(mes, 1, &th);	 /* in drive */
	tape_close();
	if (mes) close_statusbox(mes);
	if (ret == -1) goto selfile;			 /* error getting tape */
	if ((ret == TAPE_NOEXIST) && (!prompt_archive)) return do_exit(ERROR_OPENING_BACKUP);
	if (ret == BAD_MAGIC) {
	    message_box("This is not a taper archive", MB_OK);
	    return -1;
	}
    }
    else
      th.archive_id = (pr_dir == 0) ? diff_id : pr_dir;
    sprintf(df, "%s/taper_info.%d", taper_info_files, th.archive_id);
    ret = open(df, O_RDONLY);
    if (ret == -1) {
	if (!prompt_archive)
	  return do_exit(ERROR_NO_INFO);
	if (noinfo == FALSE) {
	    message_box("Info file doesn't exist", MB_OK);
	    return -1;
	}
	*archive_id = th.archive_id;
	return (message_box("No info file - do full restore", MB_YESNO) == 1) ?
	  -2 : -1;
    }
    else
      close(ret);
    if (!prompt_archive) {			 /* no prompting */
	*archive_id = th.archive_id;		 /* go straight to archive  */
	return 0;
    }

selfile:;
    *df = 0;
    if (ret != -1)
      if (*th.archive_title)
	sprintf(df, "ID %d %s", th.archive_id, th.archive_title); /* set as default archive */
      else
	sprintf(df, "ID %d <no title>", th.archive_id); /* set as default archive */
    ret = select_file(taper_info_files, "taper_info.", df, restore_mfn, FALSE);
    if ((ret == 0) || (ret == -1))		 /* abort or error */
      return -1;
    *archive_id = atol(&df[3]);
    if (*archive_id == 0) {			 /* not valid info file */
	message_box("Not a valid info file", MB_OK);
	return -1;
    }
    return 0;
}


PUBLIC void taper_restore(void) 
{

    _s32  x;
    _u32 arch;
    _errstat sa=0;

    backrest_do_mallocs();
    if (check_device_names() == -1) goto end;	 /* check devices & other parms */
    if (open_logfile(((restore_mode == RESTORE_VERIFY) ? "Verify archive" : "Restore")) == -1)
      goto end;
    if (no_windows)
      prompt_archive = 0;
    if (restore_mode != RESTORE_VERIFY) {
	sa = select_archive(&arch, TRUE);	 /* ?which archive */
	if (sa == -1) goto end;
	restore_mode =  (sa == -2) ? RESTORE_FULL : RESTORE_NORMAL;
    }

    if (!no_windows) {
	backrest_init_windows();
	backrest_clear_screen();
    }
    if (restore_mode != RESTORE_NORMAL) {	 /* full restore */
	full_restore();
	goto end;
    }
    if (read_vol_dir(arch) == -1)
      goto end;					 /*  Read volume dir */
    if (pr_dir) {
	print_voldir();
	tape_close();
    }
    else
      if (diff_id) {
	  print_diff();
	  tape_close();
      }
    else {
	info = my_realloc(info, 1);		 /* free memory */
	print_title_line();
	if (ifd.no_in_archive) {
	    x = select_restore_files();
	    switch(x) {
	       case 0:				 /* user aborted */
		break;				 /* user aborted */
	       case -1:				 /* error occurred */
		x = 0;
		log_errors = 1;
		break;
	     default:				 /* must be OK */
		if (!no_sel) {
		    message_box("No files selected", MB_OK);
		    x = 0;
		}
		break;
	    }
	}
	else {
	    message_box("No files in archive", MB_OK);
	    x = 0;
	}
	backrest_kill_windows();
	print_my_name();
	chdir(original_cur_dir);		 /* change directory to ensure */
	if (x) 
	    do_restore();			 /* restore to proper place */
    }
    
    end:;
    backrest_free_memory();
    close_logfile(((restore_mode == RESTORE_VERIFY) ? "Verify archive" : "Restore"));
    return;			                 /* succesful return */
}
    

PUBLIC void taper_verify(void)
{
    if (!message_box("Insert tape in drive", MB_OKCANCEL))
      return;
    restore_mode = RESTORE_VERIFY;
    taper_restore();
}

