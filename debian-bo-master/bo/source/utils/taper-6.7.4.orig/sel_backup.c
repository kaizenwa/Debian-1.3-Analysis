/*
   Time-stamp: <96/08/04 19:06:07 yusuf>

   $Id: sel_backup.c,v 1.25 1996/08/04 18:08:37 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: sel_backup.c,v 1.25 1996/08/04 18:08:37 yusuf Exp $";
#endif /* lint */



#include "taper.h"
#include "backup.h"

select_details  file_sd, select_sd;
int dir_left_width;

void backup_asterix_line(WINDOW *win, _s32 ent_num, int line)
{
    struct direntry *entry;
    char       s2[MAX_FNAME];
    struct oa_file_entry *fe;
    char   c1;
    int    c;

    entry = directory + ent_num;
    strcpy(s2, global_cur_dir); 
    if (*s2)
      if (s2[strlen(s2)-1] != '/')
      strcat(s2, "/");
    strcat(s2, entry->entry.d_name);
    if (S_ISDIR(entry->info.st_mode))
      strcat(s2, "/");
    fe = sel_files;				 /* see if this entry has been selected */
    c1 = ' ';
    for (c=0; c<no_sel; c++) {
	if (!strcmp(s2, name(fe))) {
	    c1 = '*';
	    break;
	}
	if (strlen(name(fe)) <= strlen(s2))	 /* check that whole directory not selected */
	  if (!strncmp(s2, name(fe), strlen(name(fe)))) 
	    if (S_ISDIR(fe->i.mode)) {		 /* only if a directory */
	      c1 = '*';
	      break;
	  }
	advance_ofe(&fe);
    }
    if (exclude_archive(s2))
      c1 = ' ';
    mvwaddch(files, line+1, left_width-3, c1);
}


void backup_asterix_dir(WINDOW *win, _s32 start, char *p_scroll) {
/* Prints an asterix next to selected entries in the directory */    
    int        cur_line=1;
    _s32       cur_ent;
    
    cur_ent=start;
    while (cur_line < screen_ylen_files-3) {
	backup_asterix_line(win, cur_ent++, cur_line++);
	if (start+cur_line > directory_count)	 /* check that not at end of dir */
	  break;
    }
    wrefresh(win);
}


void backup_print_dir_line(WINDOW *win, _s32 ent_num, int line, char ref) {
/* Prints one entry in a directory at line 'line'  
 * Returns the size of the directory entry in bytes */
    
    struct direntry *entry;
    
    entry = directory + ent_num;
    print_dir_line(win, entry, line, ref, dir_left_width);
    backup_asterix_line(win, ent_num, line);
    centre(bottom, 0, entry->entry.d_name, COLOR_BOTTOM); wrefresh(bottom);
}


void backup_print_dir(WINDOW *win, _s32 start, char *p_scroll) {
/* This function prints the current directory into the left hand
 * window. It starts printing the directory at entry 'start'.
 * 
 * cur_dir is the name of the current directory, which is printed
 * at the top line of the screen
 * 
 * Format of an entry is:
 *   *name rwxrwxrwx 99999K
 * 
 * where * = ' ', '/' or '@' depending on file type 
*/
    int        cur_line=1;
    _s32       cur_ent;
    _s32       dsize=0;
    char       s[MAX_FNAME];
    struct direntry *entry;
    
    my_werase(win, COLOR_DIRECTORY);
    cur_ent=start;
    while (cur_line < dir_screen_ylen-2) {
	backup_print_dir_line(win, cur_ent, cur_line, FALSE);/* print line */
	cur_line++;				 /* increment */
	cur_ent++;
	if (start+cur_line > directory_count) /* check that not at end of dir */
	    break;
    }
    *p_scroll = 0;
    print_scroll_bar(win, directory_count, &dir_sd, win->_maxy,
		     win->_maxx, p_scroll);
    wattron(win, A_UNDERLINE); 
    entry=directory;				 /* calculate size of directory */
    for (cur_line=1; cur_line<=directory_count;cur_line++) {
	if (S_ISREG(entry->info.st_mode))	
	  dsize += entry->info.st_size;		 /* only reg files have a size */
	entry++;
    }
    convert(s, dsize);				 /* print dir size at right */
    mvwaddstr(win, 1, win->_maxx-strlen(s)-1, s);
    if (strlen(global_cur_dir) > win->_maxx-strlen(s)-2) {
	strcpy(s, &global_cur_dir[strlen(global_cur_dir)-(win->_maxx-strlen(s)-1)+2]);
	s[0] = '~';
    }
    else
      strcpy(s, global_cur_dir);
    mvwaddstr(win, 1, 1, s);
    wattroff(win, A_UNDERLINE);
}

void backup_print_selected_line(WINDOW *win, _s32 entry, int line, char ref) {
    
    struct oa_file_entry *s;
    char   s1[50], s2[200];
    
    s = find_entry(sel_files, entry);
    switch(s->selected) {
       case 1:
	(s->i.size < 1024) ? sprintf(s2, "F   %8d  %s ", s->i.size, name(s)) :
	   sprintf(s2, "F   %9s %s ", print_kb(s1, s->i.size), name(s));
	break;
       case 2:
       case 3:
	(s->i.size < 1024) ? sprintf(s2, "F ( %8d  %s)", s->i.size, name(s)) :
  	  sprintf(s2, "F ( %9s %s)", print_kb(s1, s->i.size), name(s));
	break;
    }
    if (s->incremental) *s2 = 'I';
    mvwaddstr(win, line+1, 5, s2);
    centre(bottom, 0, name(s), COLOR_BOTTOM); wrefresh(bottom);
    if (ref)
      wrefresh(win);
}


void backup_print_selected_size(WINDOW *win)
{
    char  s1[100], s[50];
    
    sprintf(s1, "Selected files %s", print_kb(s, total_selected));
    wattron(win, A_UNDERLINE); 
    mvwaddstr(win, 1, 3, s1);
    wattroff(win, A_UNDERLINE);
}

void backup_print_selected(WINDOW *win, _s32 top_sel, char *p_scroll) {
    _s32   cur;

    my_werase(win, COLOR_SELECTED);
    backup_print_selected_size(win);
    cur=1;
    while ((cur < screen_ylen_selection-2) && (top_sel+cur-1 < no_sel)) {
	backup_print_selected_line(win, top_sel+cur-1, cur, FALSE);
	cur++;
    }
    *p_scroll = 0;
    print_scroll_bar(win, no_sel, &select_sd, win->_maxy,
		     win->_maxx, p_scroll);
    wrefresh(win);
}


_s32 sz;
_errstat sel_backup_dpd(char *full_path, struct stat *b)
{
    sz += b->st_size;
    return 0;
}
_s32 get_dirsize(WINDOW *mes, int ln, char *full_dir, char inc)
{
/* gets the size of the directory and all it's contents
   if inc == 1, then incremental size is used
 * 
 * any errors received are ignored
*/

    sz = 0;
    if (process_dir(mes, ln, full_dir, inc, sel_backup_dpd, FALSE) == -1)
      return -1;
    return sz;
}


_errstat recalculate_size(struct oa_file_entry *ce, struct file_info *fi, int printbox)
{
/* Returns -1 if user aborted the recalc */    
    _s32 old_size, sz;
    WINDOW *s_box=NULL;
    char   s[MAX_FNAME];
    
    old_size = ce->i.size;
    strcpy(s, "Sizing "); strcat(s, name(ce));
    if (printbox && !no_windows) s_box=status_box(s_box, s, 1, TRUE, 1);
    sz = get_dirsize(s_box, 1, name(ce), ce->incremental);   
    if (printbox) close_statusbox(s_box);
    if (sz != -1) { 				 /* aborted directory sizing */
	ce->i.size = sz;
	total_selected += ce->i.size-old_size;
	return 0;
    }
    return -1;
}


void select_entry_engine(struct file_info *fi, struct oa_file_entry *ce, 
			 char *fn, int printbox, int add, int sel_method)
{
    _s32   c;
    struct oa_file_entry *se;
    
    if (add) {					 /* new file */
	no_sel++;
	ce->i.name_len = make4len(fn);
	add_sel_file(ce, fn);
	ce = find_entry(sel_files, no_sel-1);
	ce->i.size = 0;
	ce->incremental = incremental;
	strcpy(name(ce), fn);
    }
    ce->i.act_size = 0;
    ce->i.dev = fi->dev;
    ce->i.uid  = fi->uid;
    ce->i.gid = fi->gid;
    ce->i.mode = fi->mode;
    ce->i.org_mode = fi->org_mode;
    ce->i.atime = fi->atime;
    ce->i.ctime = fi->ctime;
    ce->i.mtime  = fi->mtime;
    ce->selected = sel_method;
    if (recalculate_size(ce, fi, printbox) == -1) {
	if (add) no_sel--;
	return;
    }
    if (!S_ISDIR(ce->i.mode)) {
	se = sel_files;				 /* make sure that this */
	for (c=0; c<no_sel; c++) {		 /* file is not going to  */
	    if (!strcmp(name(se), name(ce)))
	      continue;
	    if ( (strlen(name(se)) < strlen(name(ce))) &&   /* be indirectly */
		(name(se)[strlen(name(se))-1] == '/') )   /* selected */
	      if (!strncmp(name(se), name(ce), strlen(name(se)))) {
		  ce->selected = 3;
		  total_selected -= ce->i.size;
		  return;
	      }
	    advance_ofe(&se);
	}
	return;
    }
    se = sel_files;
    for (c=0; c<no_sel; c++) {			 /* no go through all selected */
	if (strlen(name(ce)) < strlen(name(se))) /* files checking for duplicate selectiongs */
	  if (!strncmp(name(se), name(ce), strlen(name(ce)))) {
	    switch(se->selected) {
	       case 0:
		se->selected = 2; break;
	       case 1:
		se->selected = 3; break;
	    }
	    total_selected -= se->i.size;	 /* don't double size the entry */
	}
	advance_ofe(&se);
    }
}


int select_entry_1(struct direntry *entry, char *s)
{
/* Returns 1 if added the entry,
 * Returns 0 if entry existed */
    struct file_info fi;
    struct oa_file_entry *ce;
    _s32   c=0;
	  
    ce = sel_files;
    while (c < no_sel)  {
	if (!strcmp(s, name(ce))) {
	    bmessage_box("File already selected.", MB_OK);
	    return 0;
	}
	c++;
	advance_ofe(&ce);
    }
    if (exclude_archive(s)) {
	bmessage_box("This file is on the file exclusion list", MB_OK);
	return 0;
    }
    fi.size = entry->info.st_size;
    fi.act_size = 0;				 /* make a fi structure */
    fi.dev = entry->info.st_dev;			 /* for the engine */
    fi.uid  = entry->info.st_uid;
    fi.gid = entry->info.st_gid;
    fi.mode = entry->info.st_mode;
    fi.org_mode = entry->org_info.st_mode;
    fi.atime = entry->info.st_atime;
    fi.ctime = entry->info.st_ctime;
    fi.mtime  = entry->info.st_mtime;
    fi.name_len = make4len(s);
    select_entry_engine(&fi, ce, s, 1, c==no_sel, 1);
    return 1;
}


int select_entry(_s32 cur_entry) {
/* Adds the 'cur_entry' entry into the select files. If a directory,
 * confirms with user that the whole subdirectory tree is to be
 * included
 * 
 * Returns 1 if entry added, 0 otherwise
*/
    struct direntry  *entry;
    char   s[MAX_FNAME];

    entry = directory + cur_entry;
    if (!strcmp(entry->entry.d_name, "..")) {
	beep();
	return 0;
    }
    if (dir_selection)
      if (S_ISDIR(entry->info.st_mode)) 
        if (!bmessage_box("Is a directory. Confirm?", MB_YESNO))
          return 0;
    strcpy(s, global_cur_dir); 
    if (s[strlen(s)-1] != '/')
      strcat(s, "/"); 
    strcat(s, entry->entry.d_name);
    if (S_ISDIR(entry->info.st_mode))
      if (s[strlen(s)-1] != '/')
	strcat(s, "/"); 
    return select_entry_1(entry, s);
}


void backup_update_selected(void)
{
/* Goes through archive and works out indirect selections 
   for sel_backup */

    _s32 c, c1;
    struct oa_file_entry *cur_ent, *se;

    cur_ent = sel_files;			 /* clear current selection */
    total_selected = 0;				 /* information */
    for (c=0; c<no_sel; c++) {			 
	switch(cur_ent->selected) {
	   case 2:
	    cur_ent->selected=0; break;
	   case 3:
	    cur_ent->selected=1; break;
	}
	advance_ofe(&cur_ent);
    }

    cur_ent = sel_files;
    for (c=0; c<no_sel; c++) {
	if ((cur_ent->selected == 1) || (cur_ent->selected == 4)) /* update total_selected size */
	  total_selected += cur_ent->i.size;
	if (S_ISDIR(cur_ent->i.mode)) {		 /* only bother if directory */
	    se=sel_files;
	    for (c1=0; c1<no_sel; c1++) {	 /* loop through looking for */
		if (c1 != c) {			 /* dependencies */
		    if (strlen(name(cur_ent)) <= strlen(name(se)))
		      if (!strncmp(name(cur_ent), name(se), strlen(name(cur_ent))))
		      switch(se->selected) {	 /* matches */
		       case 0:
			  se->selected = 2; break;
		       case 1:
			  se->selected = 3; break;
		       case 4:
			  se->selected = 5; break;
		      }
		}
		advance_ofe(&se);
	    }
	}
	advance_ofe(&cur_ent);
    }
}


int backup_toggle_selected(_s32 cur_entry)
{
/* Toggles selection between incremental & non-incremental mode */

    struct oa_file_entry *ce;

    ce = find_entry(sel_files,cur_entry);
    ce->incremental = !ce->incremental;
    if (recalculate_size(ce, &ce->i, 1) == -1)
      ce->incremental = !ce->incremental;	 /* user aborted - restore */
    backup_print_selected_size(selection);
    return 0;
}


void del_sel(struct oa_file_entry *fe)
/* Delete the entry fe from sel_files */
{
    char *dest, *src;
    _s32 bytes_to_go, len;
    
    dest = (char *) fe;				 /* now delete the entry */
    len = (memory_tight) ? sizeof(struct oa_file_entry) + fe->i.name_len   /* work out how many bytes to delete */
      : sizeof(struct fixed_oa);
    src = dest+len;				 
    bytes_to_go = len_sel_files - (src-(char *) sel_files);/* how many bytes to copy */
    while (bytes_to_go) {			 /* do the copy */
	*dest = *src;
	dest++; src++; bytes_to_go--;
    }
    len_sel_files -= len;
}

void backup_del_cur_entry_engine(_s32 entry)
{
    umode_t  mode;
    struct oa_file_entry *fe;
    
    fe = find_entry(sel_files, entry);
    total_selected -= fe->i.size;
    mode = fe->i.mode;				 /* save mode before it's deleted */
    del_sel(fe);				 /* do the delet */
    (no_sel)--;
    if (S_ISDIR(mode))				 /* don't have to worry if not dir */
      backup_update_selected();
    backup_print_selected_size(selection);
}
    
void backup_del_cur_entry(select_details *sd, _s32 *no_sel)
{
    backup_del_cur_entry_engine(sd->cur_entry);
}
    



void backup_save_backupset(int in_backup)
{
    backrest_save_backupset(1);
}
  

void backup_restore_backupset(void)
{
    char ln[MAX_FNAME];
    FILE *f;
    struct oa_file_entry *ce;
    struct direntry entry;
    _s32 c;
    char x[MAX_FNAME];
    char *x1;
    int x2;

    x2 = dir_screen_ylen;
    getcwd(x, sizeof(x));
    x1 = global_cur_dir;
    f = backrest_restore_backupset(NULL);
    global_cur_dir = x1;
    chdir(x);
    dir_screen_ylen = x2;
    strcpy(global_cur_dir, x);
    if (f==NULL) 
	return;
    while (!feof(f)) {
	fgets(ln, sizeof(ln), f);
	if (ln[strlen(ln)-1] == '\n')
	  ln[strlen(ln)-1] = 0;
	if (*ln) {
	    ce=sel_files;
	    for (c=0; c<no_sel; c++) {		 /* make sure not already in */
		if (!strcmp(name(ce), ln)) {
		    strcat(ln, " already selected");
		    bmessage_box(ln, MB_OK);
		    break;
		}
		advance_ofe(&ce);
	    }
	    if (c==no_sel)
	      if (get_statinfo(ln, &entry.info) != -1)/* ignore if error */
	      select_entry_1(&entry, ln);
	}
	fgets(ln, sizeof(ln), f);		 /* ignore filter */
    }
    fclose(f);
}

int backup_select_vol(_s32 entry)
{
/* Tag an entry in the volume window */    
    _s32 c=0;
    struct direntry c_entry;
    char *s;
    char s1[MAX_FNAME];
    
    s = vol_details;				 /* find appropriate */
    while (c++<entry)				 /* line in vol_details buffer */
      while (*s++);

    if (!strncmp(s, "Volume ", strlen("Volume ")) ||   /* don't use this */
	!strncmp(s, "Contains ", strlen("Contains ")) ||
	!strncmp(s, "Backed up at ", strlen("Backed up at ")) ) 
	    return 0;

    if (lstat(s, &c_entry.info) != -1)		 /* ignore if error */
      return select_entry_1(&c_entry, s);

    if (errno == ENOENT) {
	sprintf(s1, "%s doesn't exist", s);
	bmessage_box(s1, MB_OK);
    }
    return 0;
}

void backup_unselect_dir(select_details *sd, _s32 *dummy)
{
    struct direntry *entry;
    char       s2[MAX_FNAME];
    struct oa_file_entry *fe;
    char   c1=' ';
    int    c;

    entry = directory + sd->cur_entry;
    strcpy(s2, global_cur_dir);        /* make full pathname */
    if (*s2)
      if (s2[strlen(s2)-1] != '/')
      strcat(s2, "/");
    strcat(s2, entry->entry.d_name);
    if (S_ISDIR(entry->info.st_mode))
      strcat(s2, "/");
    fe = sel_files;		       /* see if this entry has been selected */
    for (c=0; c<no_sel; c++) {
	if (!strcmp(s2, name(fe))) {
	    c1 = '*';		       /* file is selected */
	    break;
	}
	if (strlen(name(fe)) <= strlen(s2))/* check that whole directory not selected */
	  if (!strncmp(s2, name(fe), strlen(name(fe)))) {
	      c1 = '!';		       /* file is indirectly selected */
	      break;
	  }
	advance_ofe(&fe);
    }
    if (!S_ISDIR(entry->info.st_mode)) /* see if it will be excluded */
      if (exclude_archive(s2))
        c1 = ' ';
    switch(c1) {
     case ' ':
	bmessage_box("File not selected", MB_OK);
	break;
     case '!':
	bmessage_box("File indirectly selected", MB_OK);
	break;
     case '*':
	backup_del_cur_entry_engine(c);/* delete entry */
	break;
    }
}




void add_root(struct direntry **dir, _s32 *dircount)
{
/*  Makes sure that a '..' directory entry exists and if it
    doesn't, adds it

    Returns nothing

*/
  _s32 c=0;
  struct direntry *d;


  d = *dir;
  while (c<*dircount) {
    if (!strcmp(d->entry.d_name, "..")) return;	 /* OK  */
    c++;
    d++;
  }
  /* no .. entry - add one */
  
  *dir = my_realloc((void *) *dir, sizeof(struct direntry)    
		    * ((*dircount)+1));		 /* make memory */
  for (c=*dircount-1; c>=0; c--)		 /* add this entry at top */
    *(*dir+c+1) = *(*dir+c);
  strcpy((*dir)->entry.d_name, "..");		 /* add the .. entry */
  (*dir)->org_info.st_mode = S_IFDIR;
  (*dircount)++;
}


int backup_select_files(char *cur_dir) {
/* Returns 0 if quit
 * Returns 1 if wants to backup
*/
    int   ret;
    char  p=0;
    char  old_dir[MAX_FNAME];

    dir_left_width = left_width;		 /* for directory routines */
    dir_screen_ylen = screen_ylen_files;
    directory_count = 0;
    global_cur_dir = cur_dir;			 /* to allow global use */
    if (read_dir(cur_dir, "", NULL,
		 &directory, &directory_count) == -1) return -1; /* read in directory */
    clear_sd(&dir_sd);
    clear_sd(&select_sd);
    clear_sd(&vol_sd);

    backup_print_selected(selection, select_sd.top_entry, &p); /* print selections  */
    p=0;
    print_on_vol_dir(on_vol, vol_sd.top_entry, &p);	/* print files currenty on volume */

    while (1) {
	ret = select_box(files, &directory_count, &dir_sd,
			 backup_print_dir, backup_print_dir_line, select_entry, 
			 backup_unselect_dir, backrest_paint,
			 selection, &no_sel, &select_sd,
			 backup_print_selected, NULL, NULL,
			 "Select file/directory for backup", 
			 backup_save_backupset, 
			 backup_restore_backupset);
	switch(ret) {
	 case SELECT_ABORT:
	    return 0;
	 case SELECT_FINISHED:
	    return 1;
	 case SELECT_TAB:
	    if (no_sel) 
	      ret =select_box(selection, &no_sel, &select_sd,
			      backup_print_selected, 
			      backup_print_selected_line, backup_toggle_selected,
			      backup_del_cur_entry, backrest_paint,
			      selection, &no_sel, &select_sd,
			      backup_print_selected, backup_asterix_dir, &dir_sd,
			      "Change to/from incremental backup",
			      backup_save_backupset, 
			      backup_restore_backupset);
	    switch(ret) {
	     case SELECT_ABORT:
		return 0;
	     case SELECT_FINISHED:
		return 1;
	     case SELECT_TAB:
		if (no_vol_details)		 /* if nothing in vol, will  */
		  ret = select_box(on_vol, &no_vol_details, &vol_sd,   /* fall through as SELECT_TAB */
				   print_on_vol_dir,
				   print_on_voldir_line, backup_select_vol, NULL,
				   backrest_paint, selection, &no_sel,
				   &select_sd, backup_print_selected,
				   NULL, NULL, "Backup file/directory",
				   backup_save_backupset, 
				   backup_restore_backupset);
		switch(ret) {
		 case SELECT_ABORT:
		    return 0;
		 case SELECT_FINISHED:
		    return 1;
		 default:
		    break;
		}
	    }
	    break; 
	 case SELECT_ENTER:
	    if (S_ISDIR((dir_sd.cur_entry+directory)->org_info.st_mode)) {
		*old_dir = 0;
		if (!strcmp((directory+dir_sd.cur_entry)->entry.d_name, "..")) {
		  if (strcmp(cur_dir, "/"))
		      strcpy(old_dir, cur_dir);
		  else {			 /* top dir only */
		    directory = my_realloc(directory, sizeof(struct direntry));
		    strcpy(directory->entry.d_name, ""); /* add the .. entry */
		    directory->org_info.st_mode = S_IFDIR;
		    directory_count = 1;
		    strcpy(cur_dir, ""); strcpy(old_dir, "..");
		    goto madedir;
		  }
		}
		strcat(cur_dir, "/");
		strcat(cur_dir, (directory+dir_sd.cur_entry)->entry.d_name);
		ret = read_dir(cur_dir, "", NULL,
			       &directory, &directory_count);
		add_root(&directory, &directory_count);
		if (ret == -1) return -1;
		if (ret == 0)
		  getcwd(cur_dir, MAX_FNAME);		
		else {
		    clear_sd(&dir_sd);
		    if (old_dir)
		      find_correct_sd(old_dir,&dir_sd, directory_count,
				      directory);
		}
	    madedir:;
		p=0;
		backup_print_dir(files, dir_sd.top_entry, &p);
		p=0;
		backup_print_selected(selection, select_sd.top_entry, &p); 
	    }
	    break;
	 default:
	    break;
	}
    }
    return 0; /* never get here */
}
