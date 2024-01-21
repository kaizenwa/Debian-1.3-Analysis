/*
   Time-stamp: <96/07/19 20:13:36 yusuf>

   $Id: utils.c,v 1.13 1996/07/27 20:42:21 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: utils.c,v 1.13 1996/07/27 20:42:21 yusuf Exp $";
#endif /* lint */



#include "taper.h"

PUBLIC _errstat utils_write_nullheader(WINDOW **mes)
{
/* Erases a tape and writes an empty header */    

    if (*mes == NULL) {
	if (win_main) {
	    touchwin(win_main); 	
	    wrefresh(win_main);
	}
    }
    if (win_main)
      *mes = status_box(*mes, "Rewinding tape", 1, (*mes==NULL), 1);
    if (tape_rewind() == -1) return -1;
    status_box(*mes, "Opening tape", 1, FALSE, 1);
    if (tape_open(O_RDWR) == -1) return -1;
    status_box(*mes, "Erasing tape", 1, FALSE, 1);
    if (tape_erase() == -1) return -1;

    tdh.magic = TAPER_MAGIC_NUMBER;
    tdh.archive_id = 0;
    tdh.tape_number = 0;
    memset(tdh.archive_title, 0, MAX_ARCHIVETITLE);
    status_box(*mes, "Writing tape", 1, FALSE, 1);
    memcpy(write_buffer, &tdh, sizeof(tdh));
    if (log_level > 2) write_log("Writing null tape header");
    if (write(dv, write_buffer, block_size) != block_size)
      return do_exit(ERROR_WRITING);
    status_box(*mes, "Closing tape", 1, FALSE, 1);
    return 0;
}


PUBLIC void  utils_mktape(void)
{
    WINDOW *mes=NULL;

    clear_main();
    if (open_logfile("Make tape") == -1) return;
    if (check_device_names() == -1) return;	 /* check devices & other parms */
    if (!message_box("This will erase ALL data on tape. Proceed", MB_YESNO)) {
	close_logfile("Make tape");
	return;
    }

    utils_write_nullheader(&mes);
    tape_close();
    final_message("Make tape");
    close_statusbox(mes);
    close_logfile("Make tape");
    return;		
}


PUBLIC void utils_testfast_fsf(void)
{
    WINDOW *mes=NULL;
    char   *m=NULL;
    int    old_hfsf;

    clear_main();
    if (open_logfile("Test fast fsf") == -1) return;
    if (check_device_names() == -1) return;	 /* check devices & other parms */
    if (!message_box("This will erase ALL data. Proceed", MB_YESNO)) {
	close_logfile("Test fast fsf");
	return;
    }
    m = my_malloc(block_size*10);
    if (utils_write_nullheader(&mes) != -1) {
	tape_close();
	if (tape_open(O_RDWR) == -1) goto err;
	status_box(mes, "Writing data", 1, FALSE, 1);
	strcpy(m, "Volume#1");
	if (tape_write(m, block_size*10) != block_size * 10) goto err;
	tape_close();
	status_box(mes, "Advancing a volume", 1, FALSE, 1);
	if (ntape_open(O_RDWR) == -1) goto err;
	if (tape_fsf(1, FALSE) == -1) goto err;
	tape_close();
	status_box(mes, "Writing to second volume", 1, FALSE, 1);
	if (tape_open(O_RDWR) == -1) goto err;
	strcpy(m, "Volume#2");
	if (tape_write(m, block_size*10) != block_size*10) goto err;
	tape_close();
	
	status_box(mes, "Re-reading a bit of volume 1", 1, FALSE, 1);
        if (tape_open(O_RDWR) == -1) goto err;
	memset(m, 0, block_size*3);
	if (tape_read(m, block_size*3) != block_size*3) goto err;
	if (strcmp(m, "Volume#1")) {
	    if (log_level)
	      write_log("Could read 'volume#1' from volume 1");
	    message_box("Couldn't write to volume 1", MB_OK);
	    goto err;
	}
	status_box(mes, "Trying to do a fast fsf", 1, FALSE, 1);
	old_hfsf = have_fsf;			 /* force tape drive to have fsf */
	have_fsf = 1;
	if (tape_fsf(1, FALSE) == -1) {have_fsf = old_hfsf; goto err;}
	have_fsf = old_hfsf;
	status_box(mes, "Trying Reading in a bit of volume 2", 1, FALSE, 1);
	memset(m, 0, block_size*3);
	if (tape_read(m, block_size*3) != block_size*3) goto err;
	message_box((strcmp(m, "Volume#2")) ?
		    "Your tape drive can't do a fast fsf" :
		    "Your tape drive can do a fast fsf", MB_OK);
	if (log_level)
	  write_log((strcmp(m, "Volume#2")) ?
		    "Your tape drive can't do a fast fsf" :
		    "Your tape drive can do a fast fsf");
	status_box(mes, "Closing tape", 1, FALSE, 1);
    }
    
    err:;
    if (m) my_free(m);
    tape_close();
    final_message("Test fast fsf");
    close_statusbox(mes);
    close_logfile("Test fast fsf");
    return;		
}


PUBLIC void utils_test_can_seek(void)
{
    char *m=NULL;
    WINDOW *mes=NULL;
    int   c;
    int  old_cseek;
    
    clear_main();
    if (open_logfile("Test can seek") == -1) return;
    if (check_device_names() == -1) return;	 /* check devices & other parms */
    if (!message_box("This will erase ALL data. Proceed", MB_YESNO)) {
	close_logfile("Test can seek");
	return;
    }
    m = my_malloc(block_size);
    old_cseek = can_seek;			 /* save current settings */
    can_seek = 1;

    if (utils_write_nullheader(&mes) != -1) {
	tape_close();
	if (tape_open(O_RDWR) == -1) goto fin;
	status_box(mes, "Writing data", 1, FALSE, 1);   /* write out ten blocks */
	for (c=0; c<10; c++) {
	    sprintf(m, "Volume#1 - block #%d", c);
	    if (tape_write(m, block_size) != block_size) goto fin;
	}
	if (have_fsf) {
	    tape_close();
	    status_box(mes, "Advancing a volume", 1, FALSE, 1);
	    if (ntape_open(O_RDWR) == -1) goto fin;
	    if (tape_fsf(1, FALSE) == -1) goto fin;
	    tape_close();
	    if (tape_open(O_RDWR) == -1) goto fin;
	}
	status_box(mes, "Writing to second volume", 1, FALSE, 1);
	for (c=0; c<10; c++) {
	    sprintf(m, "Volume#2 - block #%d", c);
	    if (tape_write(m, block_size) != block_size) goto fin;
	}
	tape_close();
	status_box(mes, "Testing seek on volume #1", 1, FALSE, 1);
	if (tape_open(O_RDWR) == -1) goto fin;
	if (log_level > 2) write_log("Reading block #3");
	if (tape_seek(3) == -1) goto cant;
	memset(m, 0, block_size);
	if (tape_read(m, block_size) != block_size) goto fin;
	if (strcmp(m, "Volume#1 - block #3")) goto cant;
	if (log_level > 2) write_log("Reading block #7");
	if (tape_seek(7) == -1) goto cant;
	memset(m, 0, block_size);
	if (tape_read(m, block_size) != block_size) goto fin;
	if (strcmp(m, "Volume#1 - block #7")) goto cant;
	status_box(mes, "Advancing to volume #2", 1, FALSE, 1);
	if (log_level > 2) write_log("Advancing to vol #2");
	tape_close();
	if (ntape_open(O_RDWR) == -1) goto fin;
	if (have_fsf) {
	    if (tape_fsf(1, FALSE) == -1) goto fin;
	    tape_close();
	    if (tape_open(O_RDWR) == -1) goto fin;
	}
	else {
	    if (log_level > 2) write_log("fsf not supported - doing manually");
	    for (c=0; c<10; c++)
	      if (tape_read(m, block_size) != block_size) goto fin;
	    blocks_passed = 0;			 /* on new volume */
	    
	}
	status_box(mes, "Testing seek on volume #2", 1, FALSE, 1);
	if (log_level > 2) write_log("Reading block #5");
	if (tape_seek(5) == -1) goto cant;
	memset(m, 0, block_size);
	if (tape_read(m, block_size) != block_size) goto fin;
	if (strcmp(m, "Volume#2 - block #5")) goto cant;
	if (log_level > 2) write_log("Reading block #8");
	if (tape_seek(8) == -1) goto cant;
	memset(m, 0, block_size);
	if (tape_read(m, block_size) != block_size) goto fin;
	if (strcmp(m, "Volume#2 - block #8")) goto cant;
	message_box("Your tape drive can seek", MB_OK);
	if (log_level) write_log("Your tape drive can seek");
	goto fin;
	
	cant:;
	  message_box("Your tape drive cannot seek", MB_OK);
	  if (log_level) write_log("Your tape drive cannot seek");
	
    }
    
    fin:;
      if (m) my_free(m);
      can_seek = old_cseek;
      touchwin(mes); 	
      status_box(mes, "Closing tape", 1, FALSE, 1);
      tape_close();
      final_message("Test can seek");
      close_statusbox(mes);
      close_logfile("Test can seek");
      return;		
}



PUBLIC void utils_whereproc(void)
{
    char   s[MAX_FNAME],  s1[MAX_FNAME];
    struct stat buf;

    if (get_string(win_main, s, MAX_FNAME, "Name of proc directory") == -1)
      return;
    if (stat(s, &buf) == -1) 
      do_exit(ERROR_GETINFO);
    else { 
	sprintf(s1, "%s is on device %d", s, buf.st_dev);
	message_box(s1, MB_OK);
    }
    return;
}


_errstat select_archive(_u32 *archive_id, char noinfo);/* in restore.c */
_errstat read_vol_dir(_u32 archive_id);		 /* in restore.c */
_errstat taper_change_prefs(struct pref_info *options, _s8 allab);/* in taper.c */
PUBLIC void utils_erasevols(void)
{
    WINDOW *mes_box=NULL;
    static _s32 vols;				 /* needs to be */
    char s[100];				 /* static so can use initialiser */
    _u32 arch;
    _s32  x, onia, pos;
    struct volume_header vh;
    char *new_info=NULL, *new_cp;
    _s32 new_cur_size;
    struct file_info *fi;
    
    struct pref_info get_vols[] = {
	{"No vols to delete", &vols, 'I', 0},
	{NULL, NULL, 0, 0}
    };

    vols = 0;
    init_common_vars();
    backrest_do_mallocs();
    if (check_device_names() == -1) return;	 /* check devices & other parms */
    if (open_logfile("Erase Vols") == -1)
      return;
    if (no_windows)
      prompt_archive = 0;
    backrest_init_windows();
    backrest_clear_screen();
    if (select_archive(&arch, FALSE) ==-1)	 /* find out which archive user wants */
      goto end;
    if (read_vol_dir(arch) == -1)
      goto end;					 /*  Read volume dir */
    if (taper_change_prefs(get_vols, 1) == -1)
      goto end;

    if (vols == 0)				 /* no volumes selected */
      goto end;
    
    if (vols == ifd.number_volumes) {
	message_box("Use erase tape to delete whole tape", MB_OK);
	goto end;
    }
    
    if (vols > ifd.number_volumes) {
	sprintf(s, "There are only %d volumes on tape", ifd.number_volumes);
	message_box(s, MB_OK);
	goto end;
    }
    
    sprintf(s, "Delete volumes %d to %d inclusive",
	    ifd.number_volumes-vols+1,
	    ifd.number_volumes);
    if (!message_box(s, MB_YESNO))
      goto end;

    mes_box = status_box(mes_box, "                                         ", 2, TRUE, 5);/* create window */
    nodelay(mes_box, TRUE);
    mes_box = status_box(mes_box, "Opening tape", 3, FALSE, 1);/* create window */

    ifd.number_volumes -= vols;
    if (goto_end_vol(mes_box, 3, ifd.number_volumes, 1, TRUE, FALSE) == -1)
      goto end;

    write_offset = 0;				 /* make sure nothing's ahead of us */
    w_cur_pos = 0;
    vh.volume_magic = VOLUME_MAGIC;		 /* a sign that it's gone */
    vh.no_in_volume = -1;
    status_box(mes_box, "Deleting volume(s)", 3, FALSE, 1);
    if (tape_write((char *) &vh, sizeof(vh)) != sizeof(vh))
      goto end;
    tape_close();

    status_box(mes_box, "Updating info files", 3, FALSE, 1);
    ifd.info_file_size = sizeof(struct info_file_header);
    onia = ifd.no_in_archive;
    ifd.no_in_archive=0;
    new_info = my_malloc(MEM_BLOCK_SIZE);
    new_cur_size = MEM_BLOCK_SIZE;
    cp = info;					 /* now remove entries from */
    cp += sizeof(struct info_file_header);	 /* info file that are not needed */
    new_cp = new_info;
    new_cp += sizeof(struct info_file_header);
    for (x=0; x<onia; x++) {
	fi = (struct file_info *) cp;
	if (abs(fi->volume) <= ifd.number_volumes) {/* want this entry */
	    ifd.info_file_size += sizeof(struct file_info) + fi->name_len;
	    if (ifd.info_file_size > new_cur_size) {	 /* will exceed memory block */
		pos = new_cp - new_info;				 /* keep cp position */
		new_info = my_realloc(new_info, new_cur_size + MEM_BLOCK_SIZE);
		if (new_info == NULL) {
		    new_cur_size = 0;
		    do_exit(ERROR_MEMORY);
		    goto end;
		}
		new_cp = new_info + pos;
		new_cur_size += MEM_BLOCK_SIZE;
	    }
	    ifd.no_in_archive++;		 /* increment # in archive */
	    memcpy(new_cp, cp, sizeof(struct file_info));   /* copy from old info to new info */
	    strcpy(new_cp+sizeof(struct file_info),
		   cp+sizeof(struct file_info));
	    new_cp += sizeof(struct file_info);
	    PAST_STRING_FI(new_cp);
	}
	cp += sizeof(struct file_info);		 /* advance in old info */
	PAST_STRING_FI(cp);
    }
    ifd.size_volume_headers = get_vh(vol_headers, ifd.number_volumes+1)
                                 - (char *) vol_headers;/* calculate new size */
    
    memcpy(new_info, &ifd, sizeof(ifd));	 /* copy header file */
    write_out_info_file(new_info, ifd.archive_id);
    
    end:;
    if (new_info) my_free(new_info);
    backrest_kill_windows();
    print_my_name();
    final_message("Erase vols");
    backrest_free_memory();
    close_logfile("Erase vols");
    return;			                 /* succesful return */
}


PUBLIC void utils_test_mktape(void)
{
    _s32 x;
    WINDOW *mes=NULL;
    struct tape_header *tdh;
    
    clear_main();
    if (open_logfile("Test mktape") == -1) return;
    if (check_device_names() == -1) return;	 /* check devices & other parms */
    if (!message_box("Please insert new tape. ALL DATA WILL BE ERASED", MB_OKCANCEL)) {
	close_logfile("Test mktape");
	return;
    }
    touchwin(win_main); wrefresh(win_main);
    mes = status_box(mes, "Opening tape device", 1, TRUE, 1);
    if (tape_open(O_RDWR) == -1) goto exit;
    status_box(mes, "Trying to read data", 1, FALSE, 1);
    x = read(dv, write_buffer, block_size);
    if (x==block_size) {
	message_box("You do not need to run mktape on new tapes", MB_OK);
        if (log_level > 1) write_log("You do not need to run mktape on new tapes");
	goto exit;
    }
/* See if the situation resolves by writing data */    
    status_box(mes, "Couldn't read data - closing tape", 1, FALSE, 1);
    tape_close();
    status_box(mes, "Opening tape device", 1, FALSE, 1);
    if (tape_open(O_RDWR) == -1) goto exit;
    if (utils_write_nullheader(&mes) == -1) {
	do_exit(ERROR_WRITING);
	goto exit;
    }
/* Now try re-reading */
    status_box(mes, "Re-opening tape device", 1, FALSE, 1);
    if (tape_open(O_RDWR) == -1) 
      goto exit;
    status_box(mes, "Re-reading data", 1, FALSE, 1);
    memset(write_buffer, 0, block_size);
    x = read(dv, write_buffer, block_size);
    if (x==-1) {
	do_exit(ERROR_READING);
	goto exit;
    }
    tdh = (struct tape_header *) write_buffer;
    if (tdh->magic == TAPER_MAGIC_NUMBER) {
	message_box("You need to run mktape on new tapes", MB_OK);
        if (log_level > 1) write_log("You need to run mktape on new tapes");
	goto exit;
    }
    message_box("ERROR: Didn't read what was written", MB_OK);
    
    exit:;
    close_logfile("Test mktape");
    tape_close();
    return;
}


PRIVATE char *dir_list;
PRIVATE _s8 is_rec;
PRIVATE _s32 len;
_errstat utils_testlinks_dpd(char *full_path, struct stat *b)
{
    char *s;
    char olddir[MAX_FNAME], curdir[MAX_FNAME];
    _s32 c1;

    if (!S_ISDIR(b->st_mode)) return 0;
    getcwd(olddir, sizeof(olddir));		 /* save current directory */
    if (chdir(full_path) == -1) return 0;	 /* can't change directories */
    getcwd(curdir, sizeof(curdir));
    len++;					 /* add to list of directories we must process */
    if (len%100 == 0) 
      dir_list = my_realloc(dir_list, (len+99)*MAX_FNAME);
	strcpy((dir_list+(len-1)*MAX_FNAME), curdir);
	chdir(olddir);
    s=dir_list;
    for (c1=0; c1<len-1; c1++) {		 /* make sure this directory */
	if (!strcmp(s, curdir)) {		 /* is not in list */
	    is_rec = TRUE;
	    strcpy(dir_list+(len-1)*MAX_FNAME, full_path);
	    chdir(olddir);
	    return -1;				 /* tell process_dir to stop */
	}
    }
    return 0;
}


PUBLIC void utils_testlinks(void)
{
    char   s[MAX_FNAME], olddir[MAX_FNAME];
    WINDOW *mes=NULL;
    int    oldhl;

    strcpy(s, "/");
    if (get_string(win_main, s, MAX_FNAME, "Start directory") == -1)
      return;
    if (!*s) return;
    dir_list = my_malloc(100*MAX_FNAME);
    len=1;
    getcwd(olddir, sizeof(olddir)); 
    chdir(s); getcwd(s, sizeof(s));
    chdir(olddir);
    is_rec = FALSE;
    strcpy(dir_list, s);
    mes = status_box(mes, "Checking directories", 1, TRUE, 1);
    oldhl = hard_links;				 /* save old hard_links */
    hard_links = TRUE;				 /* say yes to trace links */
    process_dir(mes, 1, s, 0, utils_testlinks_dpd, TRUE);
    hard_links = oldhl;
    close_statusbox(mes); touchwin(win_main); wrefresh(win_main);
    if (is_rec == TRUE) 
	sprintf(s, "Recursive link in %s", dir_list+(len-1)*MAX_FNAME);
    else
      sprintf(s, "No recursive links found");
    message_box(s, MB_OK);
    my_free(dir_list);
}
    
