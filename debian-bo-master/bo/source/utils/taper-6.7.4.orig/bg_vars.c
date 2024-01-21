/*
   Time-stamp: <96/08/05 19:30:36 yusuf>

   $Id: bg_vars.c,v 1.9 1996/08/05 19:02:00 yusuf Exp $	

*/

#ifndef lint
static char vcid[] = "$Id: bg_vars.c,v 1.9 1996/08/05 19:02:00 yusuf Exp $";
#endif /* lint */



#include "taper.h"


struct err_temp errs[] = {
    {ERROR_ILLEGAL_DIR, "opening directory", FALSE, FALSE},
    {ERROR_GETINFO, "doing getinfo call", FALSE, FALSE},
    {ERROR_MEMORY, "Memory allocation error", TRUE, FALSE},
    {ERROR_USAGE, "", TRUE, TRUE},
    {ERROR_MAGIC, "Backup device contains unknown data", TRUE, FALSE},
    {ERROR_REWIND, "rewinding tape", FALSE, FALSE},
    {ERROR_NO_INFO, "No info file for this archive", TRUE, FALSE},
    {ERROR_CREATING_INFO, "creating info file", FALSE, FALSE},
    {ERROR_OPENING_BACKUP, "opening backup device", FALSE, FALSE},
    {ERROR_READING, "reading from backup device", FALSE, FALSE},
    {ERROR_WRITING, "writing to backup device", FALSE, FALSE},
    {ERROR_CONFIGURATION, "opening preferences file", FALSE, FALSE},
    {ERROR_OPENING_LOG, "opening log file", FALSE, FALSE},
    {ERROR_WRITING_INFO, "trying to write to info file", FALSE, FALSE},
    {ERROR_TRSIZE, "Illegal maximum transfer size", TRUE, FALSE},
    {ERROR_NEW_TAPE, "getting next tape in archive", FALSE, FALSE},
    {ERROR_EMPTY_ARCHIVE, "This tape is blank", TRUE, FALSE},
    {ERROR_TAPE_FSF, "doing fsf", FALSE, FALSE},
    {ERROR_WRITING_HEADER, "writing tape header", FALSE, FALSE},
    {ERROR_INFO_MISMATCH, "Mismatch between info filename and info file", TRUE, FALSE},
    {ERROR_READING_INFO, "reading from info file", FALSE, FALSE},
    {ERROR_CREATING_PREFS, "creating preference file", FALSE, FALSE},
    {ERROR_CREATING_COMLINE, "creating command line file", FALSE, FALSE},
    {ERROR_ERASING_TAPE, "erasing tape", FALSE, FALSE},
    {ERROR_OPENING_FLIST, "opening compression file list", FALSE, FALSE},
    {ERROR_CREATING_FIFO, "creating FIFO", FALSE, FALSE},
    {ERROR_OPENING_FIFO, "opening FIFO", FALSE, FALSE},
    {ERROR_UNABLE_FORK, "forking child", FALSE, FALSE},
    {ERROR_OPENING_SET, "opening file set", FALSE, FALSE},
    {ERROR_INFO_ISNT_DIR, "Info file directory is not a directory", TRUE, FALSE},
    {ERROR_OPENING_MAIL, "opening mail file", FALSE, FALSE},
    {ERROR_GETTING_BLKSIZE, "setting block size", FALSE, FALSE},
    {ERROR_SETTING_BLKSIZE, "getting block size", FALSE, FALSE},
    {ERROR_SETTING_SHARED_MEM, "setting up shared memory", FALSE, FALSE},
    {ERROR_NO_TAPE_TYPE, "Set tape drive using -T option", TRUE, TRUE},
    {ERROR_PREF_FORMAT, "Error in preference file format", TRUE, TRUE},
    {ERROR_ILLEGAL_TAPE_TYPE, "Illegal tape type", TRUE, TRUE},
    {ERROR_OPENING_PF, "opening /proc/filesystem", FALSE, FALSE},
    {ERROR_UNABLE_MOUNT, "Unable to mount device", TRUE, FALSE},
    {ERROR_UNABLE_UMOUNT, "umounting device", FALSE, FALSE},
    {ERROR_SKIPPING, "skipping volumes", FALSE, FALSE},
    {ERROR_RENAMING, "renaming file", FALSE, FALSE},
    {ERROR_INFO_FILE, "Error in info file format", TRUE, FALSE},
    {ERROR_FREE_MEMORY, "Internal error freeing memory", TRUE, FALSE},
    {ERROR_INFO_MAGIC, "Illegal info file magic number", TRUE, FALSE},
    {ERROR_INFO_OLD, "Old info file format - recreate info file", TRUE, FALSE},  
    {ERROR_NO_FSF, "Tape drive doesn't support fsf", TRUE, FALSE},
    {ERROR_COMPRESSING_INFO, "compressing info file", FALSE, FALSE},
    {ERROR_NO_BACKUP_CHILD, "Unable to start backup child", TRUE, FALSE},
    {ERROR_NO_RESTORE_CHILD, "Unable to start restore child", TRUE, FALSE},  
    {ERROR_NO_DISKSPACE, "Not enough disk space for compression", TRUE, FALSE},
    {ERROR_SETTING_SEMAPHORE, "creating semaphore", FALSE, FALSE},  
    {ERROR_SEMAPHORE_ACTION, "activating semaphore", FALSE, FALSE},  
    {ERROR_CHILD_SEGFAULT, "Write child seg faulted", TRUE, FALSE},  
    {ERROR_TAPE_EOM, "doing eom", FALSE, FALSE},
    {ERROR_TAPE_GOTO, "positining tape", FALSE, FALSE},  
    {ERROR_NONE, "", TRUE, TRUE},
    {0, "", TRUE, TRUE}
};

int  no_windows=0;
WINDOW *win_main;

UBYTE *comp_buffer1;				 /* compression buffer pointers */
UBYTE *comp_buffer2;
char *tr_buffer=NULL;				 /* transfer buffer */

char log_file[MAX_FNAME]={0};			 /* log file stuff */
int  log_level=2;
char exclude_compress[MAX_FNAME]={".gz .gif .Z .zip .jpg .bmp"};/* files to exclude in compression */
char temp_dir[MAX_FNAME]={"/usr/tmp"};		 /* temporary files directory */
int  ovrwrite=1;
int  max_tr_size=DEFAULT_TR_SIZE;		 /* is used for reading/writing files to/from hard disk */
_s32 min_free=COMPRESS_MIN_FREE;
int  min_seek=DEFAULT_MIN_SEEK;			 /* min blocks to pass before seeking */

int lf=0;					 /* log file */
UBYTE *cbuf;					 /* compression buffer */

struct shared_mems *shm;			 
#ifdef TRIPLE_BUFFER
int    shm_id;
#endif

_s8 restore_mode;

/* Temporary file that contains what we are going to mail 
   Don't have to pass to bg_ programs */
char mailfn[MAX_FNAME]={0};			 /* name of mail temp file */
int mf=0;					 /* mail file */
