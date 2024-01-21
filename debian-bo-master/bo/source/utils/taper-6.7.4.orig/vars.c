/*
   Time-stamp: <96/08/05 19:36:37 yusuf>

   $Id: vars.c,v 1.13 1996/08/05 19:02:04 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: vars.c,v 1.13 1996/08/05 19:02:04 yusuf Exp $";
#endif /* lint */



/* Variables initialized */


#include "taper.h"

/* Note tape type must be first preference */
struct pref_template ap[] = {
    {"--append", 'a', &append, NULL, 'Y', TRUE},
    {"--archive-diff", 'A', &diff_id, NULL, 'A', TRUE},
    {"--both",'b',  tape, ntape, 'S', FALSE},
    {"--compress-type", 'c', &compression, NULL, 'I', TRUE},
    {"--bad-checksum", 'C', &bad_checksum, NULL, 'Y', TRUE},  
    {"--print-dir", 'd', &pr_dir, NULL, 'd', TRUE},
    {"--auto-descend", 'D', &auto_descend, NULL, 'Y', TRUE},
    {"--prompt-archive", 'e', &prompt_archive, NULL, 'Y', TRUE},	 
    {"--can-seek", 'E', &can_seek, NULL, 'Y', FALSE},  
    {"--tape-name", 'f', tape, NULL, 'S', FALSE},
    {"--exclude-files", 'F', exclude_files, NULL, 'S', TRUE},
    {"--volume-title", 'g', volume_title, NULL, 'S', TRUE},
    {"--get-blksize", 'G', &get_blksize, NULL, 'Y', FALSE},
    {"--hard-links", 'h', &hard_links, NULL, 'Y', TRUE},
    {"--comp-head-start", 'H', &comp_head_start, NULL, 'I', TRUE},
    {"--taper-info-files", 'i', taper_info_files, NULL, 'S', TRUE},
    {"--compress-info", 'I', &compress_info, NULL, 'Y', TRUE},  
    {"--proc-device", 'j', &proc_dev, NULL, 'I', TRUE},
    {"--min-before-seek", 'J', &min_seek, NULL, 'I', TRUE},
/*    {"--info-at-end", 'J', &info_end, NULL, 'Y', TRUE},    not yet */
    {"--erase-tape", 'k', &erase_tape, NULL, 'Y', FALSE},	 
    {"--log-file", 'l', log_file, NULL, 'S', TRUE},
    {"--limit-log-file", 'L', &log_file_size, NULL, 'I', TRUE},
    {"--memory-tight", 'M', &memory_tight, NULL, 'Y', TRUE},
    {"--log-level", 'm', &log_level, NULL, 'I', TRUE},
    {"--ntape-name",'n', ntape, NULL, 'S', FALSE},
    {"--one-file-system", 'N', &ofs, NULL, 'Y', TRUE},  
    {"--overwrite", 'o', &ovrwrite, NULL, 'I', TRUE},	 
    {"--tape-overwrite", 'O', &tape_overwrite, NULL, 'Y', TRUE},      
    {"--preference-file", 'p', NULL, NULL, 'P', TRUE},
    {"--tmp-dir", 'P', &temp_dir, NULL, 'S', TRUE},  
    {"--only-volume", 'q', &only_vol, NULL, 'I', TRUE},
    {"--fast-fsf",'Q',  &fast_fsf, NULL, 'Y', FALSE},
    {"--restore-path", 'r', rel_path, NULL, 'S', TRUE},
    {"--exclude-dirs", 'R', exclude_dirs, NULL, 'S', TRUE},
    {"--strip-paths", 's', &ostrip, NULL, 'I', TRUE},
    {"--set-blksize",'S', &set_blksize, NULL, 'Y', FALSE},
    {"--archive-title", 't', archive_title, NULL, 'S'},
    {"--tape-type",  'T', &tape_type, NULL, 'T', TRUE},
    {"--incremental-backup", 'u', &incremental, NULL, 'Y', TRUE},
    {"--unattended-file", 'U', NULL, NULL, 'U', TRUE},
    {"--version", 'v', NULL, NULL, 'v', TRUE},
    {"--recent-restore", 'w', &most_recent, NULL, 'Y', TRUE},	 
    {"--old-archive", 'W', &old_archive, NULL, 'Y', TRUE},  
    {"--block-size", 'x', &block_size, NULL, 'I', FALSE},
    {"--exclude-compress", 'X', exclude_compress, NULL, 'S', TRUE},
    {"--sort-dirs", 'y', &sort_dir, NULL, 'Y', TRUE},      
    {"--min-free", 'Y', &min_free, NULL, 'I', TRUE},      
    {"--prompt-dirs", 'z', &dir_selection, NULL, 'Y', TRUE},
    {"--tape-size", 'Z', &tape_size, NULL, 'I', FALSE},
    {"--color-title", '.', &color_title_foreground, &color_title_background, 'C', TRUE},
    {"--color-main", '.', &color_main_foreground, &color_main_background, 'C', TRUE},
    {"--color-dialog", '.', &color_dialog_foreground, &color_dialog_background, 'C', TRUE},
    {"--color-status", '.', &color_status_foreground, &color_status_background, 'C', TRUE},
    {"--color-directory", '.', &color_directory_foreground, &color_directory_background, 'C', TRUE},
    {"--color-on-volume", '.', &color_onvol_foreground, &color_onvol_background, 'C', TRUE},
    {"--color-selected", '.', &color_selected_foreground, &color_selected_background, 'C', TRUE},
    {"--color-bottom", '.', &color_bottom_foreground, &color_bottom_background, 'C', TRUE},
    {"--color-help", '.', &color_help_foreground, &color_help_background, 'C', TRUE},
    {"--color-form", '.', &color_form_foreground, &color_form_background, 'C', TRUE},
    {NULL, ' ', NULL, NULL, ' ', TRUE}
};


/* Colours */
char color_title_foreground=COLOR_BLUE;
char color_title_background=COLOR_WHITE;
char color_main_foreground=COLOR_WHITE;
char color_main_background=COLOR_BLUE;
char color_dialog_foreground=COLOR_WHITE;
char color_dialog_background=COLOR_RED;
char color_status_foreground=COLOR_WHITE;
char color_status_background=COLOR_RED;
char color_selected_foreground=COLOR_WHITE;
char color_selected_background=COLOR_BLUE;
char color_directory_foreground=COLOR_BLACK;
char color_directory_background=COLOR_GREEN;
char color_onvol_foreground=COLOR_WHITE;
char color_onvol_background=COLOR_MAGENTA;
char color_bottom_foreground=COLOR_BLUE;
char color_bottom_background=COLOR_WHITE;
char color_help_foreground=COLOR_BLACK;
char color_help_background=COLOR_CYAN;
char color_form_foreground=COLOR_WHITE;
char color_form_background=COLOR_BLACK;


/* Misc variables that parent/children need to share */

/* Memory buffers */
int write_buffer_count;				 /* how many bytes in write buffer */
char   *write_buffer;				 /* write buffer */
#ifdef TRIPLE_BUFFER
int    wb_shm_id;				 /* write block */
int    wb1_shmid, wb2_shmid;			 /* double buffers 1 & 2 */
#endif

_s32 w_cur_pos;					 /* pos in double buffer */

int read_buffer_count;				 /* how many bytes in read buffer */
char *read_buffer;				 /* read buffer */
int read_offset;				 /* position in read buffer */

int block_size;					 /* size of buffer */

/* Common window variables */
WINDOW    *title, *files, *selection, *bottom, *on_vol;
int       screen_ylen, screen_xlen;		 /* physical dims of screen */
int       screen_ylen_files, screen_ylen_selection;/* length of screens */
int       left_width, right_width;


/* Options */
int  append=1;
int  auto_descend=1;
int  hard_links=0;
int  lstrip, ostrip=99;
int  tape_overwrite=0;
int  compression=2;				/* Default is internal compression */
int  sort_dir=1;
char archive_title[MAX_ARCHIVETITLE]={0};
char volume_title[MAX_ARCHIVETITLE]={0};
char exclude_files[MAX_FNAME]={".o ~"};		 /* files to exclude in archive */
char exclude_dirs[MAX_FNAME]={"/tmp /usr/tmp /var/tmp"};/* directories to exclude in archive */
int  unattended=0;				 /* for unattended back sets */
char rel_path[MAX_FNAME]={0};
int  incremental=1;
int  most_recent=1;
int  prompt_archive=1;				 
_s32 pr_dir=0;
_u32 diff_id=0;
char taper_info_files[MAX_FNAME]={0};
int  info_end=0;				 /* writes out info file at end of tape */
int compress_info=1;
int  dir_selection=0;
int  log_file_size=2;				 /* default is 2MB */
int  org_block_size;
dev_t proc_dev=1;				 /* device on which /proc is mounted */
int  only_vol=0;				 /* limit restore to volume */
int  comp_head_start=0;				 /* how much head start to give compression */
char pref_file[MAX_FNAME];			 /* file of preference file */
int  memory_tight=0;				 /* memory tight  */
int  have_rewind=-1;
int  have_fsf=-1;
int  bad_checksum=1;
int  ofs=0;					 /* stay on one filesystem */
int  old_archive=0;				 /* reconstructing old archive */

/*  More options - these are all things that depend on type of tape drive used */
signed int  tape_type=-1;			 
signed char tape[MAX_FNAME]={-1};		 /* filename of rewinding tape device */
signed char ntape[MAX_FNAME]={-1};		 /* filename of non-rewinding tape device */
signed int  erase_tape=-1;			 /* these will all be */
signed int  set_blksize=-1;			 /*   filled in with the default */
signed int  get_blksize=-1;			 /*   applicable to the tape */
signed int  fast_fsf=-1;			 /*   drive selected */
signed int  block_size=-1;			 /* is used for reading/writing data to/from tape device */
signed int  can_seek=-1;			 /* whether tape drive can seek or not */


/* File counts and archive size counts */
_u32 total_compressed;				 /* # file bytes in volume - compressed */
_u32 total_uncompressed;			 /* # file bytes in volume - uncompressed */
_s32 no_in_archive=0;				 /* # files in archive */
_s32 no_sel=0;					 /* # files selected */
_u32 total_selected=0;				 /* # bytes selected */
_s32 no_vol_details;				 /* number of lines in vol_details */
_s32 in_dir;
int vols_passed;				 /* number of volumes passed on this tape */
int tape_size=0;				 /* size of tape (in MB) 0 = no size */

int update_tsi=0;				 /* tells read to update tsi */
/* File handles */
int dv;

/* Stipped & Common pathnames */
char cf[MAX_FNAME]={0};				 /* common pathname prefix */
char stripped_cf[MAX_FNAME];			 /* portion of common pathname prefix that is not stripped */


/* Info files */
char *info;					 /* pointer to archive info */
char *cp;					 /* offset used into above */
struct tape_header tdh;				 /* headers for tape & */
struct info_file_header ifd;			 /* info file */
struct volume_header *vol_headers;		 /* volume headers */
char   *vol_details;				 /* information about volumes */
struct tape_size_info *tsi;			 /* info about tape size */

/* Memory allocation stuff for archive files, sel_files etc.. */
struct oa_file_entry *archive_files;		 /* files on archive */
_u32 len_archive_files;				 /* length of files on archive */
struct oa_file_entry *sel_files;		 /* files selected by user */
_u32 len_sel_files;				 
struct volume_tape_info *vt_info;		 /* which tape each volume is on */
struct dir_file_entry *dirs;			 /* restore directories */
_s32 cur_af_size, cur_info_size, cur_sf_size;	 /* size of memory blocks */

/* Children */
pid_t backup_child=0;
pid_t restore_child=0;

/* Directories */
char original_cur_dir[MAX_FNAME];
char cur_dir[MAX_FNAME];


/* Window that shows what's on the archive */
select_details vol_sd;



