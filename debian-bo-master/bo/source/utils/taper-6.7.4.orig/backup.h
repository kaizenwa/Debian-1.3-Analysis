/*
   Time-stamp: <96/07/19 20:16:16 yusuf>

   $Id: backup.h,v 1.9 1996/07/27 20:42:04 yusuf Exp $
*/


/* Common variables */
extern int backup_select_files(char *cur_dir);
extern void select_entry_engine(struct file_info *fi, struct oa_file_entry *ce, 
			 char *fn, int printbox, int add, int sel_method);
extern int select_entry_1(struct direntry *entry, char *s);
