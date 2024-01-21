/*
   Time-stamp: <96/07/19 20:16:51 yusuf>

   $Id: structs.h,v 1.11 1996/07/27 20:42:15 yusuf Exp $
*/


/* Tape header */
struct tape_header {
    _u32     magic;
    _u32     archive_id;
    _s32     tape_number;
    char     archive_title[MAX_ARCHIVETITLE];
};


/* Header of info file */
struct info_file_header {
    _u32  magic;
    _u32  archive_id;
    _s32  number_tapes;
    _u32  info_file_size;			 /* note that this doesn't include the */
    _s32  number_volumes;			 /* volume/tape info at end of info file */
    _u32  size_volume_headers;			 /* size of volume headers */
    _s32  no_in_archive;
    _s32  number_tsi;				 /* # times end vol hit end of tape */
    char archive_title[MAX_ARCHIVETITLE];
};

/* A volume header consists of:
      magic number
      no in volume
      title of volume
      time of backup
      number of selection strings 
      selection strings (ie. the selection criteria used to
                         select files for this volume)
           each string contains :
	     string len     ) selection criteria
	     string         )
	     string len     ) filter - blank filter means select all
	     string         )
*/
struct volume_header {
    _s32 volume_magic;				 /* magic number of a volume */
    _s32 no_in_volume;				 /* # files in volume */
    char volume_title[MAX_ARCHIVETITLE];	 /* name of volume */
    _time_t backup_time;			 /* time volume backed up */
    _s32  no_sels;				 /* number of selections */
    _u32 size_header;				 /* size of volume header information */
};




/* Information for each file on tape */
struct file_info {
    _u32     act_size;	 
    _s32     volume;
    /* If the volume < 0, then this means that this file is a directory that
       was added because of a change in common path names. The volume is the
       negative of the original */
    _s32     pos_in_archive;
    _dev_t    dev;
    _uid_t    uid;				 
    _gid_t    gid;
    _umode_t  mode;				 /* mode of the file (if link, mode of link file) */
    _umode_t  org_mode;				 /* mode of the original file */
    _u32     size;
    _time_t  atime;
    _time_t  mtime;
    _time_t  ctime;
    _time_t  backup_time;
    _u8      name_len;
    _u8      compressed;
    _s32     checksum;				 /* just the sum of all the bytes in the file */
      						 /* if =-1, then problem backing up this file 
						       =-2, the backup was aborted */
};


/* Information about which tape each volume is on 
   Appened to end of info file */
struct volume_tape_info {
    _s32 volume;				 /* volume */
    _s32 start_tape;				 /* tape this volume starts */
    _s32 end_tape;				 /* tape this volume ends on */
    _u32 blocks_on_last_tape;			 /* # blocks that belong to this vol on end_tape */
};

/* Information about size of each tape 
 * Appended to end of inf file */
struct tape_size_info {
    _s32 tape_number;				 /* tape number */
    _s32 volume;
    _u32 blocks;				 /* # blocks on tape */
    _u32 lb_bytes_short;			 /* # bytes the last block is short by */
};


/* Representation in memory of a file on the archive */
struct oa_file_entry {
    struct file_info    i;
    _s8                 selected;		 /* 1 if selected directly, 2 if selected indirectly,
						    0 if not */
    _s8                 incremental;		 /* in sel_restore: 1 = restore most recent
						                    0 = restore volume given
						    in sel_backup:  1 = incremental backup
						                    0 = full backup */
    struct oa_file_entry   *in_archive_files;	 /* when used by sel_restore. For sel_backup, 1 = directly select */
    _u32                dirsize;		 /* size if directory */
    _u32                mm_size, ab_size;	 /* sizes used by sel_restore */
    _s8                 end_entry;		 /* = END_ENTRY - signifies end of file entry */

/* Notes on selected field:

   0 = not selected
   1 = selected directly
   2 = selected indirectly (because directory it is in was selected)
   3 = selected directly but then duplicated because directory it was
       in has subsequently been selected
*/		       
};						 

struct fixed_oa {
    struct oa_file_entry f;
    char fn[MAX_FNAME];
};

/* Representation in memory of the directory I create in sel_restore */
struct dir_file_entry {
    struct oa_file_entry o;
    char                name[MAX_FNAME];	 
    char                filter[MAX_FILTER];
};						 

struct shared_mems {				 /* things that are */
    volatile _s32   _write_offset;		 /* and the taper writing process */
    volatile _s8    _for_close_1;		 /* trying to close with buffer */
    volatile _s8    _for_close_2;		 /* trying to close with buffer */
    volatile _s8    _still_compressing;		 /* restore child is still compressing */
    volatile _s32   _log_errors;		 /* child of parent can change them */
    volatile _s32   _log_warnings;
    volatile pid_t  _write_pid;			 /* pid of write child */
    volatile _vptr  _write_buf_no;		 /* tells child which buffer to write */
    volatile _s32   _buf_length_1;		 /* how long the buffer 1 is */
    volatile _s32   _buf_length_2;		 /* how long buffer 2 is */
    volatile _s32   _left_tr;			 /* amount left to transfer out of a buffer after write */
    volatile _s32   _blocks_passed;		 /* number of blocks passed */
    volatile _u32   _bytes_short;		 /* # bytes that couldn't be read/written to last block on tape */
             _s8    *_w_current_buffer;		 /* buffer data being currently spooled to */
             _s8    *_w_current_writing;	 /* buffer child is currently writing to tape */
             _s8    *_w_buffer_1, *_w_buffer_2;	 /* double buffers */
};

struct pref_template {
    char   *pref_name;				 /* long command name & preference name */
    _s8    cln;					 /* short command name */
    _vptr  option_1;
    _vptr  option_2;				 /* for colours */
    _s8    handle;				 /* how to handle option */
						 /* Y = true/false, S = string */
						 /* I = integer, P = preference file name */
						 /* C = colour */
    _s8 save_pref;				 /* if FALSE, needs tape name in front of this pref */
};

struct pref_info {
    char * name;
    _vptr opt;
    _s8   type;
    _s32  length;
    /* Types:  S = string
               I = integer (0..99)
	       L = log level (0..3)
	       B = block size (ie. divide by 1024)
               Y = yes or no (0..1)
               O = overwrite (0..2)
               T = tape type (0..2)
               C = compression (0..2)
    */
};


typedef _errstat (*file_passed_action) (struct file_info *fi, char * fn, struct oa_file_entry *fe);
typedef void (*print_status) (WINDOW *mes_box, _s32 cur_in_vol, 
			      _s32 no_in_vol, _s32 vol,
			      _u32 file_size,
			      char *, _time_t start, _time_t t_current);
typedef _errstat (*chksum_err) (struct file_info *fi, char *fn);


#define PAST_STRING_FI(cp) \
  cp += (((struct file_info *) (cp-sizeof(struct file_info)))->name_len)
#define PAST_STRING_OAFE(cp) \
  cp += (((struct oa_file_entry *) (cp-sizeof(struct oa_file_entry)))->i.name_len)
