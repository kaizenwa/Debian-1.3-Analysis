/*
   Time-stamp: <96/07/19 20:18:18 yusuf>

   $Id: sel_restore.c,v 1.23 1996/07/27 20:42:12 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: sel_restore.c,v 1.23 1996/07/27 20:42:12 yusuf Exp $";
#endif /* lint */



#include "taper.h"
#include "restore.h"


/* Handling of selected files.
 * 
 * Files selected are stored in a memory block of file_entry starting
 * at sel_files. All the files on the volume are stored in a memory
 * block of file_entry starting at archive_files
 * 
 * When files are selected, they are put into this memory block and
 * the selected field of the structure in the memory block pointed to
 * by archive_files is set to 1.
 * 
 * If the file selected is a directory, then all the files in that directory
 * are searched for and their selected field is set to 2.
 * 
 * Selected field:  0 = not selected
 *                  1 = explicity selected by user
 *                  2 = indirectly selected by user
 *                  3 = explicity & indirectly selected by user
*/

extern select_details file_sd, select_sd, vol_sd;
char start_path[MAX_FNAME];

int lfmr_cmp(void const *key, void const *s)
{
    struct fixed_oa *k = (struct fixed_oa *) key;
    struct fixed_oa *d = (struct fixed_oa *) s;
    int c;
    
    c = strcmp(k->fn, d->fn);
    if (c) return c;
    if (abs(k->f.i.volume) < abs(d->f.i.volume)) return -1;
    if (abs(k->f.i.volume) > abs(d->f.i.volume)) return 1;
    return 0;
}

struct oa_file_entry *look_most_recent(struct oa_file_entry *fe,
				       struct oa_file_entry *starts)
{
/* Looks through archive files and finds the entry that is the most recent
   fe should point to a file entry in archive_files 
 
   It starts the search at the end of the archive and works backwards
   until a match is found. This entry is assumed to be the most recent 
 * 
 * The search is started at starts. Assumes that list is alpha order
 * if starts == NULL, start searching at beginning
 
 */

    struct oa_file_entry *f, *old_f, *cur;
    char s[MAX_FNAME];

    if (starts == NULL) { 			 /* we have to find first entry that matches */
	strcpy(s, cf); strcat(s, name(fe));	 /* make full pathname for search file */
	f = (struct oa_file_entry *) search_file(s, fe->i.volume);
	starts = f;
	if (f == NULL) return fe;			 /* problem - couldn't find it */
    }
    f = starts;
    old_f = f;
    cur = fe;
    while ((char *) f < ((char *) archive_files) + len_archive_files) {
	if (strlen(name(f)) <= strlen(name(fe))) {
	    if (strncmp(name(f), name(fe), strlen(name(f)))) /* name doesn't match */
	      return cur;			 /* stop searching */
	    if (abs(cur->i.volume) < abs(f->i.volume))
	      cur = f;
	}
	advance_ofe(&f);
    }
    return cur;
}


void restore_asterix_line(WINDOW *win, int ent_num, int line)
{
    struct dir_file_entry *fe;
    
    if (!ent_num) return;
    fe = dirs + ent_num-1;
    if (fe->o.in_archive_files)
      (fe->o.in_archive_files->selected) ?		 
        mvwaddch(files, line+1, left_width-3, '*') :
        mvwaddch(files, line+1, left_width-3, ' ');
    else
      mvwaddch(files, line+1, left_width-3,  ' ');
}


void restore_asterix_dir(WINDOW *win, _s32 start, char *p_scroll) {
    int    cur_line=1, cur_ent;
    
    cur_ent = start;
    while (cur_line < screen_ylen_files-3) {
	restore_asterix_line(win, cur_ent++, cur_line++); /*  */
	if (start+cur_line > in_dir)		 /* check that not at end of dir */
	  break;
    }
    wrefresh(win);
}


void restore_print_voldir_line(WINDOW *win, _s32 entry, int line, char ref) {
/*  Prints one entry in a directory at line 'line'  */

    char   s[NAME_MAX+1], s2[50];
    struct dir_file_entry *fe;
    struct tm t;
    
    int left_width=files->_maxx-3;
    if (entry) {
	fe = dirs + entry-1;
	get_file_type(s, fe->o.i.mode);
	*s2 = 0;
	if (S_ISDIR(fe->o.i.mode))
	  convert(s2, fe->o.dirsize);
	else
	  if (S_ISREG(fe->o.i.mode))
	  convert(s2, fe->o.i.size);
	if (S_ISLNK(fe->o.i.mode))
	  strcat(s, " @");
	else if (S_ISDIR(fe->o.i.mode))
	  strcat(s, "  ");
	else if (!(S_ISREG(fe->o.i.mode)))
	  strcat(s, " +");
	else
	  strcat(s, "  ");
	sprintf(s, "%s%-*s", s, left_width-(int) strlen(s)-1, fe->name);
	t = *localtime(&fe->o.i.backup_time);
	if (fe->o.i.checksum == -1)			 /* error backing up */
	  strcpy(s2, "BACK ERROR");
	if (fe->o.i.checksum == -2)
	  strcpy(s2, "   ABORTED");
	s[left_width-22] = 0;
	if (fe->o.i.pos_in_archive)
	  sprintf(s, "%s %d %5d %12s", s, abs(fe->o.i.volume), fe->o.i.pos_in_archive, s2);
	else
	  sprintf(s, "%s %d %5s %12s", s, abs(fe->o.i.volume), "-", s2);
	mvwaddstr(win, line+1, 2, s);
	restore_asterix_line(win, entry, line);
	centre(bottom, 0, fe->name, COLOR_BOTTOM); wrefresh(bottom);
    }
    else 
      if (*start_path)
        mvwaddstr(win, line+1, 2, "d  ../");
      else
        mvwaddstr(win, line+1, 2, "Top of tree");
    if (ref)
      wrefresh(win);
}

void restore_print_vol_dir(WINDOW *win, _s32 start, char *p_scroll) {
    int    cur_line=1, cur_ent;
    char   s[50], s1[MAX_FNAME], s2[MAX_FNAME];
    
    my_werase(win, COLOR_DIRECTORY);
    *p_scroll = 0;
    wattron(win, A_BOLD);
    *s1 = 0;					 /* print directory */
    strcpy(s1, stripped_cf); 
    strcat(s1, start_path);
    if (strlen(s1) > win->_maxx-strlen(s)-2) { /* make sure dir name not too long */
	strcpy(s2, &s1[strlen(s1)-(win->_maxx-strlen(s)-1)+2]);
	s2[0] = '~';
    }
    else
      strcpy(s2, s1);
    if (*s2)
      mvwaddstr(win, 1, 1, s2);
    else {
	if (only_vol)
	  mvwprintw(win, 1, 1, "Only volume %d", only_vol);
	else
	  mvwaddstr(win, 1, 1, "All volumes");
    }
    mvwaddstr(win, 2, win->_maxx-6, "Size");
    mvwaddstr(win, 2, win->_maxx-18, "Pos");
    mvwaddstr(win, 2, win->_maxx-24, "Vol");
    wattroff(win, A_BOLD);
    cur_ent = start;
    while (cur_line < screen_ylen_files-2) {
	restore_print_voldir_line(win, cur_ent, cur_line, FALSE); /* print line */
	cur_line++;				 /* increment */
	cur_ent++;
	if (start+cur_line > in_dir)		 /* check that not at end of dir */
	  break;
    }
    *p_scroll = 0;
    print_scroll_bar(win, no_in_archive, &file_sd, win->_maxy,
		     win->_maxx, p_scroll);
    wrefresh(win);
}

    
void restore_print_selected_line(WINDOW *win, _s32 entry, int line, char ref) {
    
    struct oa_file_entry *mr;
    char   s1[50];
    
    mr = find_entry(sel_files, entry);
    if (((mr->incremental == 1) ? mr->mm_size : mr->ab_size) < 1024) {
    (mr->selected == 1) ?  mvwprintw(win, line+1, 5, " %8d   %s ", 
				     (mr->incremental == 1) ? mr->mm_size : mr->ab_size,
				     name(mr)) :
                           mvwprintw(win, line+1, 5, "(%8d   %s)",  
				     (mr->incremental == 1) ? mr->mm_size : mr->ab_size,
				     name(mr));
    }
    else {
	(mr->selected == 1) ?  mvwprintw(win, line+1, 5, " %9s  %s ", 
					 print_kb(s1,(mr->incremental == 1) ? mr->mm_size : mr->ab_size),
					 name(mr)) :
	                       mvwprintw(win, line+1, 5, "(%9s  %s)",  
		                         print_kb(s1,(mr->incremental == 1) ? mr->mm_size : mr->ab_size),
		                         name(mr));
    }
    (mr->incremental == 1) ? mvwaddstr(win, line+1, win->_maxx-5, "M  ") :
                             mvwprintw(win, line+1, win->_maxx-5, "%03d", abs(mr->i.volume));
    wattroff(win, A_DIM);
    centre(bottom, 0, name(mr), COLOR_BOTTOM); wrefresh(bottom);
    if (ref)
      wrefresh(win);
}


void restore_print_selected_size(WINDOW *win)
{
    char s[100];
    
    wattron(win, A_BOLD); 
    mvwprintw(win, 1, 3, "Selected files %s", print_kb(s, total_selected));
    mvwaddstr(win, 1, win->_maxx-5, "Vol");
    wattroff(win, A_BOLD);
}


void  restore_print_selected(WINDOW *win, _s32 top_sel, char *p_scroll) {
    _s32   cur;
    
    my_werase(win, COLOR_SELECTED);
    restore_print_selected_size(win);
    cur=1;
    while ((cur < screen_ylen_selection-2) && (top_sel+cur-1 < no_sel)) {
	restore_print_selected_line(win, top_sel+cur-1, cur, FALSE);
	cur++;
    }
    *p_scroll = 0;
    print_scroll_bar(win, no_sel, &select_sd, win->_maxy,
		     win->_maxx, p_scroll);
    wrefresh(win);
}


void restore_look_for_indirect(void)
{
/* Goes through the restore selected files and looks for any
   selections that become duplicated.

   For example, if you had already selected /usr/bin/games/tetris,
   and then you selected /usr/bin/games, /usr/bin/games/tetris should
   become an indirect selection
*/
    _s32 c, c1;
    struct oa_file_entry *fe, *f;

    fe = sel_files;
    for (c=0; c<no_sel; c++) {
	f = sel_files;
	if (fe->selected == 1) {		 /* only attempt to look */
	for (c1=0; c1<no_sel; c1++) {		 /* if this is directly selected */
	    if (fe != f)
	      if (name(fe)[strlen(name(fe))-1] == '/')
		if (strlen(name(fe)) <= strlen(name(f)))
		  if (!strncmp(name(fe), name(f), strlen(name(fe))))
		    if ( ((f->incremental == 1)  && (fe->incremental == 1)) ||
			(abs(f->i.volume) == abs(fe->i.volume)) )
	              if (f->selected == 1)
			f->selected = 3;
	    advance_ofe(&f);
	}
	}
	advance_ofe(&fe);
    }
}


void tag_appropriate(_s32 num)
{
/* Tags the appropriate file in archive files for sel_restore.
   Takes into consideration whether selecting actual file or most recent file
 * 
 * num is the entry in sel_files whose tagging we are doing
 * the size of the tagged files is also updated
*/
    struct oa_file_entry *f, *g, *fe, *sfe, *starts;
    _s32 c=0;
    int i;
    WINDOW *mes=NULL;

    mes = status_box(mes, "Selecting appropriate files", 1, TRUE, 1); 
    sfe = find_entry(sel_files, num);
    i = sfe->incremental;
    if (sfe->incremental == 1) 
      fe = look_most_recent(sfe->in_archive_files, NULL);
    else
      fe = sfe->in_archive_files;
    if (fe->selected != 1)			 /* already been selected */
      if (!S_ISDIR(fe->i.mode))			 /* by some-one else */
        total_selected += fe->i.size;
    fe->selected = 1;
    sfe->mm_size = fe->i.size; sfe->ab_size = sfe->in_archive_files->i.size;
    if (!S_ISDIR(fe->i.mode)) {close_statusbox(mes); return;}

    sfe->mm_size = 0; sfe->ab_size = 0;		 /* get correct size */
    f = archive_files;				 /* was a dir - select all files */
    while (c < no_in_archive) {			 /* find first entry that matches */
	if (f == fe) break;
	if (strlen(name(fe)) <= strlen(name(f)))
	  if (!strncmp(name(fe), name(f), strlen(name(fe))))
	    break;
	advance_ofe(&f);
	c++;
    }
    if (c == no_in_archive)			 /* nothing */
      return;
    starts = f;
    while (c++ < no_in_archive ) {		 /* in dir recursively */
	if (f == fe)				 /* don't compare current */
	  goto next_file;
	if (strlen(name(fe)) <= strlen(name(f))) { /* this filename is shorter - therefore */
	  if (strncmp(name(fe), name(f), strlen(name(fe))))   /* first part of paths match */
	    break;
	}
	else
	  break;
	g = (i == 1) ? look_most_recent(f, f) : f;
	if (!S_ISDIR(g->i.mode))
	  if (abs(f->i.volume) == abs(fe->i.volume))   /* update sizes */
	  sfe->ab_size += g->i.size;	
	if ((i == 0) & (abs(f->i.volume) != abs(fe->i.volume))) 
	  goto next_file;
	if (!S_ISDIR(g->i.mode)) {	 /* update size of selected directory */
	    if (!g->selected) {
		sfe->mm_size += g->i.size;
		total_selected += g->i.size;
	    }
	}
	if ((g->selected == 0) || (g->selected == 2)) /* only */
	  g->selected = 2;
	else
	  g->selected = 3;
	next_file:
	  advance_ofe(&f);
    }

    close_statusbox(mes); 
}


void restore_update_selected(void)
{
/* Goes through archive and works out indirect selections */

    _s32 c;
    struct oa_file_entry *cur_ent;
    struct dir_file_entry *d;

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

    cur_ent = archive_files;			 /* in archive files */
    for (c=0; c<no_in_archive; c++) {
	cur_ent->selected = 0;
	advance_ofe(&cur_ent);
    }
    d = dirs;					 /* blank out dirs */
    for (c=0; c<in_dir; c++) {
	d->o.selected = 0;
	d++;
    }
    restore_look_for_indirect();		 /* update indirect selections */
    
    cur_ent = sel_files;			 /* now tag the files in  */
    for (c=0; c<no_sel; c++) {			 /* archive_files */
	if (cur_ent -> selected == 1)
	  tag_appropriate(c);			 /* will handle it all */
	advance_ofe(&cur_ent);
    }
}


int toggle_selection(_s32 cur_entry)
{
/* Toggles the currently selected item from most recent restore to volume
   restore & vice-versa
*/
    struct oa_file_entry *ofe;
    char dummy;
    
    ofe=find_entry(sel_files, cur_entry);
    ofe->incremental = !ofe->incremental;
    restore_update_selected();
    restore_print_selected_size(selection);
    restore_asterix_dir(files, file_sd.top_entry, &dummy);
    return 0;
}


void del_sel_engine(struct oa_file_entry *fe)
{
    char dummy;
    
    del_sel(fe);
    (no_sel)--;
    restore_update_selected();
    restore_print_selected_size(selection);
    restore_asterix_dir(files, file_sd.top_entry, &dummy);
}

void restore_del_cur_entry(select_details *sd, _s32 *no_sel)
{
    del_sel_engine(find_entry(sel_files, sd->cur_entry));
}


void restore_unselect_dir(select_details *sd, _s32 *dummy)
{
    struct oa_file_entry *fe, *fe1;
    _s32 c;

    fe = &(dirs + sd->cur_entry-1)->o;
    fe1=sel_files;
    for (c=0; c<no_sel;c++) {      /* look for this entry in selections */
	if (fe1->in_archive_files == fe->in_archive_files)
	  break;
	advance_ofe(&fe1);
    }
    if (c==no_sel) {
	(fe->in_archive_files->selected == 0)  ?
	  bmessage_box("File not selected", MB_OK) :
	  bmessage_box("File indirectly selected", MB_OK);
	return;
    }
    del_sel_engine(fe1);
}

       
	


void tag_voldir_line_1(struct oa_file_entry *fe, char *name)
{
/* Assumes the file is either not selected or only indirectly selected */
    int s1=1;
    struct oa_file_entry ocf;
    
    if (fe->selected==2)	{		 /* was indirectly selected */
	s1 = 3;
	total_selected -= fe->i.size;		 /* now directly selected */
    }
    no_sel++;
    ocf = *fe;
    ocf.incremental = most_recent;
    ocf.in_archive_files = fe;
    ocf.selected = s1;
    add_sel_file(&ocf, name);
    tag_appropriate(no_sel-1);
    restore_look_for_indirect();		 /* update indirect selections */
}


int tag_voldir_0(struct oa_file_entry *orgfe)
{
/* Tags this entry which is in archive_files */
    struct oa_file_entry *fe1;

    if (S_ISDIR(orgfe->i.mode)) 
      if (dir_selection)
        if (!message_box("Is a directory. Confirm?", MB_YESNO))
          return 0;

    if (most_recent == 1) 			 /* look at the actual */
	fe1 = look_most_recent(orgfe, NULL);
    else
      fe1 = orgfe;
    if ((fe1->selected == 1) || (fe1->selected == 3)) {
	message_box("File already selected", MB_OK);
	return 0;
    }
    tag_voldir_line_1(orgfe, name(orgfe));
    return 1;
}


int tag_voldir_line(_s32 cur_entry) {
/* Selects the file at cur_entry.
 * It then checks through the whole file directory looking
 * if any other files should also be selected (eg. if you
 * select the dir usr/games/tetris, then the file /usr/games/tetris/high_score
 * should automatically be selected)
 * 
 * Also adds file to selected list
 * Checks for duplicates
*/
    struct dir_file_entry *dfe;

    if (!cur_entry) return 0;			 /* top/previous dir */
    dfe = dirs + cur_entry-1;
    return tag_voldir_0(dfe->o.in_archive_files);
}


int rsdr(const void *cmp1, const void *cmp2)
{
/* Sorts the directory taking into account the sort_dir option */
    struct dir_file_entry *df1 = (struct dir_file_entry *) cmp1;
    struct dir_file_entry *df2 = (struct dir_file_entry *) cmp2;
    
    if (!sort_dir) {
	if (df1->o.i.pos_in_archive > df2->o.i.pos_in_archive)
	  return 1;
	if (df1->o.i.pos_in_archive < df2->o.i.pos_in_archive)
	  return -1;
	return 0;
    }
    if (S_ISDIR(df1->o.i.mode)) 
      if (!S_ISDIR(df2->o.i.mode))
      return -1;				 /* 1 is a dir but 2 isn't */
    if (S_ISDIR(df2->o.i.mode))
      if (!S_ISDIR(df1->o.i.mode))
        return 1;				 /* 2 is a dir but 1 isn't */
    if (strcmp(df1->name, df2->name))		 /* names */
      return strcmp(df1->name, df2->name);
    if (abs(df1->o.i.volume) > abs(df2->o.i.volume))/* next compare by volume */
      return 1;
    if (abs(df1->o.i.volume) < abs(df2->o.i.volume))
      return -1;
    return 0;
}


void make_mydir(int vol)
{
/* If volume is non-zero, then only selects those files which belong
 * to vol volume */
/* Makes a directory with the path pointed to by start_path */
    _s32 c=0;
    struct oa_file_entry *fe=archive_files;
    char *s, *x;

    if (*start_path)				 /* add trailing / if not already there */
      if (start_path[strlen(start_path)-1] != '/')
        strcat(start_path, "/");
    fe=archive_files;
    in_dir=0;
    for (c=0; c<no_in_archive;c++) {
	if (strlen(start_path) < strlen(name(fe))) 
	  if (!strncmp(name(fe), start_path, strlen(start_path))) {
	      s = &name(fe)[strlen(start_path)];
	      x = strchr(s, '/');
	      if ((x == NULL) || (*(x+1) == 0)) 
		if (((vol) && (abs(fe->i.volume) == vol)) || (!vol)) 
	          if ((fe->i.checksum != -1) && (fe->i.checksum != -2)) {	 /* don't include error files */
		      in_dir++;
		      dirs = my_realloc(dirs, in_dir*sizeof(struct dir_file_entry));
		      (dirs+in_dir-1)->o = *fe;
		      (dirs+in_dir-1)->o.in_archive_files = fe;/* tells where the original entry is */
		      strcpy((dirs+in_dir-1)->name, s);
		  }
	  }
	advance_ofe(&fe);
    }
    in_dir++;					 /* up dir */
    clear_sd(&file_sd);
    qsort(dirs, in_dir-1, sizeof(struct dir_file_entry), rsdr); 
}

    
void restore_restore_backupset(void)
{
    char ln[MAX_FNAME], s[MAX_FNAME];
    FILE *f;
    _s32 c;
    struct oa_file_entry *ce;

    f = backrest_restore_backupset(NULL);
    if (f==NULL) 
	return;
    while (!feof(f)) {
	fgets(ln, sizeof(ln), f);		 /* get name */
	if (ln[strlen(ln)-1] == '\n')
	  ln[strlen(ln)-1] = 0;
	if (*ln) {				 /* ignore empty lines */
	    ce = archive_files;			 /* look for name */
	    for (c=0; c<no_in_archive; c++) {	 /* in restore list */
		strcpy(s, cf);			 /* make full pathname */
		if (s[strlen(s)-1] != '/') strcat(s, "/");
		strcat(s, name(ce));
		if (!strcmp(s, ln)) {
		    if ((ce->selected != 1) && (ce->selected != 3)) /* already selected */
		      tag_voldir_line_1(ce, name(ce));
		    break;
		}
		advance_ofe(&ce);;
	    }
	    if (c==no_in_archive) {
		strcpy(s, "Couldn't find "); strcat(s, ln);
		bmessage_box(s, MB_OK);
	    }
	}
	fgets(ln, sizeof(ln), f);		 /* ignore filter */
    }
    fclose(f);
}

int restore_on_vol_select(_s32 entry)
{
/* Selects entry for restore */
    _s32 c=0;
    char *s;
    int  lv=0;
    struct oa_file_entry *fa;
    
    s = vol_details;				 /* find appropriate */
    while (c++<entry) {				 /* line in vol_details buffer */
	if (!strncmp(s, "Volume ", strlen("Volume ")))   /* get volume we are on */
	  lv = atoi(&s[strlen("Volume ")]);
	while (*s++);
    }

    if (lv==0) return 0;			 /* nothing */
    if (!strncmp(s, "Volume ", strlen("Volume ")) ||   /* don't use this */
	!strncmp(s, "Contains ", strlen("Contains ")) ||
	!strncmp(s, "Backed up at ", strlen("Backed up at ")) ) 
	    return 0;
    
    fa = (struct oa_file_entry *) search_file(s, lv);/* couldn't find it in archive */
    if (fa == NULL) return 0;
    return tag_voldir_0(fa);
}


int select_restore_files(void) 
{
/* Returns 0 if quit
 * Returns 1 if wants to restore
*/
    int ret, oldvol;
    struct dir_file_entry *s;
    char p;
    char old_start[MAX_FNAME];
    _s32 c;

    *start_path = 0;
    clear_sd(&file_sd);
    clear_sd(&select_sd);
    clear_sd(&vol_sd);

    ret=0;
    print_on_vol_dir(on_vol, vol_sd.top_entry, &p);	/* print files currently on volume */
    p=0;
    restore_print_selected(selection, select_sd.top_entry, &p);
    
    make_mydir(only_vol);
    while (1) {
	ret = select_box(files, &in_dir, &file_sd,
			 restore_print_vol_dir, restore_print_voldir_line,
			 tag_voldir_line, restore_unselect_dir, backrest_paint,
			 selection, &no_sel, &select_sd,
			 restore_print_selected, NULL, NULL,
			 "Select file/directory for restore",
			 backrest_save_backupset, restore_restore_backupset);
	switch(ret) {
	 case SELECT_ABORT:
	    return 0;
	 case SELECT_FINISHED:
	    return 1;
	 case SELECT_ENTER:
	    if (file_sd.cur_entry == 0) {	 /* going up */
		while (1) {
		    if (!*start_path)		 /* empty directory already */
		      break;
		    oldvol = (dirs+file_sd.cur_entry)->o.i.volume;
		    strcpy(old_start, start_path);
		    start_path[strlen(start_path)-1] = 0;   /* get rid of trailing / */
		    if (strrchr(start_path, '/') == NULL) 
		      *start_path = 0;
		    else
		      *strrchr(start_path, '/') = 0;
		    if (*start_path)
		      make_mydir(abs(dirs->o.i.volume));
		    else 
		      make_mydir(only_vol);
		    s=dirs;			 /* try & find old dir in new dir */
		    for (c=0; c<in_dir; c++) 	 /* so can position on it */
		      if ( (!strcmp(s->name, &old_start[strlen(start_path)])) &&
			  (abs(s->o.i.volume) == abs(oldvol)) )
		      break;
		    else
		      s++;
		    if (c < in_dir) {		 /* found entry */
			c++;			 /* to skip the .. entry */
			file_sd.cur_entry = c;		 
			file_sd.top_entry = c-5;	 /* give it 5 lines */
			file_sd.cursor_line = 6;
			if (file_sd.top_entry < 0) {/* not five entries in directory */
			    file_sd.cursor_line += file_sd.top_entry;
			    file_sd.top_entry = 0;
			}
		    }
		    if ((in_dir != 2) || (!auto_descend))
		      break;
		    if (!S_ISDIR(dirs->o.i.mode))/* make sure that it is a directory */
		      break;
		    if (!*start_path) 		 /* reached top */
		      break;
		    file_sd.cur_entry = 0;
		}
	    }
	    else
	      if (S_ISDIR((dirs+file_sd.cur_entry-1)->o.i.mode)) {
		  while (1) {			 /* auto-descend routine */
		      strcat(start_path, (dirs+file_sd.cur_entry-1)->name);
		      make_mydir(abs((dirs+file_sd.cur_entry-1)->o.i.volume));
		      if ((in_dir != 2) || (!auto_descend))
			break;
		      if (!S_ISDIR(dirs->o.i.mode))/* make sure that it is a directory */
			break;
		      file_sd.cur_entry = 1;
		  }
	      }
	    restore_print_vol_dir(files, file_sd.top_entry, &p);
	    break;
	 case SELECT_TAB:
	    if (no_sel) 
	      ret =select_box(selection, &no_sel, &select_sd,
			      restore_print_selected, 
			      restore_print_selected_line, toggle_selection,
			      restore_del_cur_entry, backrest_paint,
			      selection, &no_sel, &select_sd,
			      restore_print_selected,
			      restore_asterix_dir, &file_sd,
			      "Change to/from most recent restore",
			      backrest_save_backupset, restore_restore_backupset);
	    switch(ret) {
	       case SELECT_ABORT:
		return 0;
	       case SELECT_FINISHED:
		return 1;
	       case SELECT_TAB:
		if (no_vol_details)		 /* if nothing in vol, will  */
		  ret = select_box(on_vol, &no_vol_details, &vol_sd,   /* fall through as SELECT_TAB */
				   print_on_vol_dir,
				   print_on_voldir_line, restore_on_vol_select, NULL,
				   backrest_paint, selection, &no_sel,
				   &select_sd, restore_print_selected,
				   NULL, NULL, "Select file/directory for restore",
				   backrest_save_backupset, restore_restore_backupset);
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
	 default:
	    break;
	}
    }
    return 0; /* never get here */
}
    
