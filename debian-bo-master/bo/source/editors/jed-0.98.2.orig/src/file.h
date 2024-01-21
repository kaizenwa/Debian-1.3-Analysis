/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#include <stdio.h>
#ifdef VMS
#include <file.h>
#endif

#include "buffer.h"
#include "vfile.h"

extern int Require_Final_Newline;
extern int read_file(char *);
extern int insert_file(char *);
extern void set_file_modes(void);
extern void auto_save_buffer(Buffer *);
extern void auto_save_all(void);
extern void auto_save(void);
extern int write_region_to_fp(int);
extern int write_region(char *);
extern int read_file_pointer(int);
extern int insert_file_pointer(VFILE *);
extern int append_to_file(char *);
extern int write_file_with_backup(char *, char *);
extern void visit_file(char *, char *);
extern void fixup_dir(char *);
extern char *dir_file_merge(char *, char *);
extern int file_status(char *);
extern int file_changed_on_disk(char *);
extern int file_time_cmp(char *, char *);
extern char *file_type(char *);
extern void check_buffer(Buffer *);
extern void set_file_trans(int *);

#ifdef __unix__
extern char *expand_link(char *);
#endif
#ifdef REAL_UNIX_SYSTEM
extern int get_inode_info (char *, int *, int *);
extern int jed_copy_file (char *, char *);
extern int Jed_Backup_By_Copying;
#endif


