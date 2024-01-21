/*
   Time-stamp: <96/08/05 19:31:24 yusuf>

   $Id: vars.h,v 1.13 1996/08/05 19:02:05 yusuf Exp $
*/


/* Misc variables that parent/children need to share */
extern struct shared_mems *shm;			 
#ifdef TRIPLE_BUFFER
extern int    shm_id;
#endif
#define write_offset (shm->_write_offset)
#define left_tr (shm->_left_tr)
#define for_close_1 (shm->_for_close_1)
#define for_close_2 (shm->_for_close_2)
#define still_compressing (shm->_still_compressing)
#define log_errors (shm->_log_errors)
#define log_warnings (shm->_log_warnings)
#define write_pid (shm->_write_pid)
#define write_buf_no (shm->_write_buf_no)
#define buf_length_1 (shm->_buf_length_1)
#define buf_length_2 (shm->_buf_length_2)
#define blocks_passed (shm->_blocks_passed)
#define bytes_short (shm->_bytes_short)
#define w_current_buffer (shm->_w_current_buffer)
#define w_current_writing (shm->_w_current_writing)
#define w_buffer_1 (shm->_w_buffer_1)
#define w_buffer_2 (shm->_w_buffer_2)

/* Memory buffers */
extern int write_buffer_count;				 /* how many bytes in write buffer */
extern char *write_buffer;				 /* write buffer */
#ifdef TRIPLE_BUFFER
extern int    wb_shm_id;
extern int    wb1_shmid, wb2_shmid;
#endif

extern _s32 w_cur_pos;				 /* pos in double buffer */

extern int read_buffer_count;			 /* how many bytes in read buffer */
extern char *read_buffer;			 /* read buffer */
extern int read_offset;				 /* position in read buffer */

extern int block_size;				 /* size of buffer */
extern UBYTE *cbuf;				 /* compression buffer */

extern char *tr_buffer;				 /* transfer buffer */
extern UBYTE *comp_buffer1;			 /* compression buffer */
extern UBYTE *comp_buffer2;			 /* compression buffer */

/* Common window variables */
extern WINDOW    *win_main, *title, *files, *selection, *bottom, *on_vol;
extern int       screen_ylen, screen_xlen;		 /* physical dims of screen */
extern int       screen_ylen_files, screen_ylen_selection;/* length of screens */
extern int       left_width, right_width;


/* Options */
extern int  append;
extern int  auto_descend;
extern int  hard_links;
extern int   lstrip, ostrip;
extern int  tape_overwrite;
extern int  compression;				/* Default is internal compression */
extern int  sort_dir;
extern char  archive_title[MAX_ARCHIVETITLE];
extern char  volume_title[MAX_ARCHIVETITLE];
extern char  exclude_compress[MAX_FNAME];	 /* files to exclude in compression */
extern char  exclude_files[MAX_FNAME];		 /* files to exclude in archive */
extern char exclude_dirs[MAX_FNAME];
extern int  unattended;				 /* for unattended back sets */
extern int  ovrwrite;
extern char  rel_path[MAX_FNAME];
extern int  incremental;
extern int  most_recent;
extern int  prompt_archive;				 
extern _s32 pr_dir;
extern _u32 diff_id;
extern int  info_end;
extern int  no_windows;
extern char taper_info_files[MAX_FNAME];
extern int  compress_info;
extern int  dir_selection;
extern char log_file[MAX_FNAME];
extern int  log_level;
extern int  log_file_size;
extern int  max_tr_size;			 /* is used for reading/writing files to/from hard disk */
extern int  org_block_size;
extern dev_t proc_dev;				 /* device on which /proc is mounted */
extern int   only_vol;				 /* limit restore to volume */
extern int   comp_head_start;			 /* how much head start to give compression */
extern char  pref_file[MAX_FNAME];		 /* file of preference file */
extern int   memory_tight;			 /* memory tight  */
extern int   bad_checksum;			 /* ?prompt if encounter bad checksum */
extern char  temp_dir[MAX_FNAME];		 /* temporary directory */
extern int   ofs;				 /* stay on one filesystem */
extern int   old_archive;			 /* reconstructing old archive */
extern _s32  min_free;
extern int min_seek;				 /* min blocks to move before seeking */

/*  More options - these are all things that depend on type of tape drive used */
extern signed int tape_type;
extern signed char tape[MAX_FNAME];		 /* filename of rewinding tape device */
extern signed char ntape[MAX_FNAME];		 /* filename of non-rewinding tape device */
extern signed int erase_tape;			 /* these will all be */
extern signed int set_blksize;			 /*   filled in with the default */
extern signed int get_blksize;			 /*   applicable to the tape */
extern signed int fast_fsf;			 /*   drive selected */
extern signed int  block_size;			 /* is used for reading/writing data to/from tape device */
extern signed int   have_rewind;			 
extern signed int   have_fsf;
extern signed int   can_seek;

/* File counts and archive size counts */
extern  _u32 total_compressed;			 /* # file bytes in volume - compressed */
extern  _u32 total_uncompressed;		 /* # file bytes in volume - uncompressed */
extern _s32 no_in_archive;			 /* # files in archive */
extern _s32 no_sel;				 /* # files selected */
extern _u32 total_selected;			 /* # bytes selected */
extern _s32 no_vol_details;			 /* number of lines in vol_details */
extern _s32 in_dir;
extern int vols_passed;				 /* number of volumes passed on this tape */
extern int tape_size;

extern int update_tsi;				 /* tells read to update tsi */

/* Temporary file that contains what we are going to mail */
extern char mailfn[MAX_FNAME];			 /* name of mail temp file */

/* File handles */
extern int lf;					 /* log file */
extern int mf;					 /* mail file */
extern int dv;

/* Stipped & Common pathnames */
extern char cf[MAX_FNAME];			 /* common pathname prefix */
extern char stripped_cf[MAX_FNAME];		 /* portion of common pathname prefix that is not stripped */


/* Info files */
extern char *info;				 /* pointer to archive info */
extern char *cp;				 /* offset used into above */
extern struct tape_header tdh;			 /* headers for tape & */
extern struct info_file_header ifd;		 /* info file */
extern struct volume_header *vol_headers;	 /* volume headers */
extern char  *vol_details;			 /* information about volumes */
extern struct tape_size_info *tsi;		 /* info about tape size */


/* Memory allocation stuff for archive files, sel_files etc.. */
extern struct oa_file_entry *archive_files;	 /* files on archive */
extern _u32 len_archive_files;		 /* length of files on archive */
extern struct oa_file_entry *sel_files;		 /* files selected by user */
extern _u32 len_sel_files;
extern struct volume_tape_info *vt_info;	 /* which tape each volume is on */
extern struct dir_file_entry *dirs;		 /* restore directories */
extern _s32 cur_af_size, cur_info_size, cur_sf_size;/* size of memory blocks */

/* Children */
extern pid_t backup_child;
extern pid_t restore_child;

/* Directories */
extern char original_cur_dir[MAX_FNAME];
extern char cur_dir[MAX_FNAME];


/*  LIFTED FROM FTAPE-IOCTL.H from zftape
 *   let the user know at which block size the volume was created with
 *   this is important as we let the user change the blocksize with
 *   MTSETBLK. One can query the blocksize before reading an old 
 *   volume or before appending data to it. zftape does not allow
 *   read/write access with the wrong blocksize. This is rather
 *   unproblematic as long as people use tar with the standard blocksize
 *   of 10kb which is also zftapes default blocksize, but who knows :-)
 */
struct mtblksz {
  unsigned long mt_blksz;
};
#define MTIOC_ZFTAPE_GETBLKSZ _IOR('m', 104, struct mtblksz)

/* Window that shows what's on the archive */
extern select_details vol_sd;


/* Colours */
extern char color_title_foreground;
extern char color_title_background;
extern char color_main_foreground;
extern char color_main_background;
extern char color_dialog_foreground;
extern char color_dialog_background;
extern char color_status_foreground;
extern char color_status_background;
extern char color_selected_foreground;
extern char color_selected_background;
extern char color_directory_foreground;
extern char color_directory_background;
extern char color_onvol_foreground;
extern char color_onvol_background;
extern char color_bottom_foreground;
extern char color_bottom_background;
extern char color_help_foreground;
extern char color_help_background;
extern char color_form_foreground;
extern char color_form_background;


/* Options */
extern struct pref_template ap[];


/* Restore mdoes */
#define RESTORE_NORMAL 0
#define RESTORE_FULL 1
#define RESTORE_VERIFY 2
extern _s8 restore_mode;
