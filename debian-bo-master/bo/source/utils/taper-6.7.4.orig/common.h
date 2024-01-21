/*
   Time-stamp: <96/07/19 20:16:25 yusuf>

   $Id: common.h,v 1.27 1996/07/27 20:42:07 yusuf Exp $
*/


/* Functions in common.c */
extern void mail_finish(char *prog);
extern void sendmail(void);
extern  void paint_main(void);
extern _errstat file_more_recent(char *s1, struct stat *b);	 /* in sel_backup.c */
extern void advance_ofe(struct oa_file_entry **ofe);
extern char *name(struct oa_file_entry *ofe);
extern _u8 make4len(char *s);
extern _errstat add_archive_file(struct oa_file_entry *ofe, char *name);
extern _errstat add_sel_file(struct oa_file_entry *ofe, char *name);
extern struct oa_file_entry *find_entry(struct oa_file_entry *ofe, _s32 entry);
extern _errstat add_ofa(struct oa_file_entry *ofe, char *name);
extern void init_common_vars(void);
extern _errstat get_statinfo(char *s, struct stat *b);
extern _errstat make_dirs(char *s);
extern void make_info_filename(char *info_file, _u32 archive_id);
extern _errstat open_info_file(_s8 must_exist, _u32 archive_id);
extern void make_fn(char *dest, char *src, _s32 no_in_vol);
extern _errstat check_device_names(void);
extern _errstat add_one_file_engine(WINDOW *mes_box, struct file_info *fi, char *name);
extern dev_t get_file_info(char *file, struct file_info *fi, _s8 chk, struct stat *ob);
extern _errstat add_missing_dirs(WINDOW *mes, _s8 in_cp);
extern _errstat read_info_file(int fd);
extern _errstat do_read_vol_dir(_u32 archive_id, char *tape, int mode, _s8 rvdir, _s8 exist);
extern char *convtime(char *s, time_t t1, time_t t2);
extern _s32 calc_checksum(int fd);
extern _s32 mem_calc_checksum(char *m, _s32 sz);
extern _errstat open_logfile(char *prog);
extern void close_logfile(char *prog);
extern void write_log(char *s);
extern void write_error_log(char *s);
extern void write_warning_log(char *);
extern void write_fatal_log(char *s);
extern _errstat read_volheader(struct volume_header *vh, _s8 read_vh, _s8 into_mem);
extern char *get_vh(struct volume_header *svh, _s32 vol);
extern _errstat traverse_volume(file_passed_action action, _s32 no_in_vol,
			   time_t t_start, WINDOW *mes_box, _s8 full_traverse, 
			   print_status ps, _s32 *files_passed, _s8 use_info,
			   chksum_err ce);
extern void change_dollar(char *s);
extern void mail_finish(char *prog);
extern void final_message(char *prog);
extern void print_title_line(void);
extern _errstat make_cf(char *c, _s32 no_in_archive);
extern char *search_file(char *, _s32);
extern void backrest_init_windows(void);
extern void backrest_clear_screen(void);
extern void backrest_kill_windows(void);
extern void backrest_free_memory(void);
extern _errstat  backrest_do_mallocs(void);
extern void print_on_voldir_line(WINDOW *win, _s32 entry, int line, char ref);
extern void print_on_vol_dir(WINDOW *win, _s32 start, char *p_scroll);
extern void del_sel(struct oa_file_entry *fe);
extern void print_my_name(void);
extern _errstat do_sort_dir(int alpha);
extern void backrest_save_backupset(int in_backup);
extern FILE *backrest_restore_backupset(char *fn);
extern _errstat bmessage_box(char *s, int type);
extern void backrest_paint(void);
extern _errstat exclude_list(char *fn, char *list);
extern _errstat exclude_compression(char *fn);
extern _errstat exclude_archive(char *fn);
typedef _errstat (*do_process_dir) (char *full_path, struct stat *b);
extern _errstat process_dir(WINDOW *mes, int ln, char *full_dir, char inc, do_process_dir dpd, _s8 send_dir);
extern _errstat init_buffers(_s8 reall);
extern void free_buffers(void);
extern void free_sems(void);
extern char *print_mb(char *s1, _u32 bytes);
extern char *print_kb(char *s1, _u32 bytes);
extern _errstat fill_in_defaults(void);
extern void clear_main(void);
extern char *color_string(char fore, char back);
extern _errstat my_filecopy(int oldfd, int newfd);
extern _errstat my_rename(char *oldf, char *newf);
extern _errstat malloc_comp_buffers(void);
extern void free_comp_buffers(void);
extern  void taper_tmpnam(char *s);
extern _errstat setowners(char *s, _errstat ret, struct file_info *fi);
extern _errstat do_write_block(_s8 *buf_to_write, _s32 old_length, _s8 fc);
extern _errstat read_into_temp(struct file_info *fi, char *tmpf, char *fn);
extern void compress_info_file(_u32 archive_id);
extern _errstat read_u32(int fd, _u32 *x);
extern _errstat write_u32(int fd, _u32 *x);
extern _errstat write_out_info_file(char *info_file, _u32 archive_id);
