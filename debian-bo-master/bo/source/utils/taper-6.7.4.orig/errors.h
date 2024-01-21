/*
   Time-stamp: <96/08/05 19:24:02 yusuf>
*/   


/* Contains information about errors */


#define ERROR_ILLEGAL_DIR -1			 /* Can't open directory */
#define ERROR_GETINFO -2		         /* Can't get stat info */
#define ERROR_MEMORY -3				 /* Unable to allocate memory*/
#define ERROR_OPENING_BACKUP -4			 /* Can't open backup device */
#define ERROR_USAGE -5				 /* Usage error */
#define ERROR_MAGIC -6				 /* Illegal magic number */
#define ERROR_REWIND -7				 /* Rewinding tape error */
#define ERROR_NO_INFO -8			 /* no info file found  */
#define ERROR_CREATING_INFO -9			 /* unable to create info file */
#define ERROR_WRITING -10			 /* Writing to backup device */
#define ERROR_READING -11			 /* Error reading from backup device */
#define ERROR_CONFIGURATION -12			 /* Error opening configuration file */
#define ERROR_OPENING_LOG -13			 /* Error opening log file */
#define ERROR_WRITING_INFO -14			 /* Error writing to info file */
#define ERROR_TRSIZE -15			 /* Error in size of maximum transfer block */
#define ERROR_NEW_TAPE -16			 /* Error getting new tape */
#define ERROR_EMPTY_ARCHIVE -17			 /* No archive on tape */
#define ERROR_TAPE_FSF -18			 /* Error while trying to do tape_fsf  */
#define ERROR_WRITING_HEADER -19		 /* Error writing tape header */
#define ERROR_INFO_MISMATCH -20			 /* Problem with info file */
#define ERROR_READING_INFO -21			 /* Error reading from info file */
#define ERROR_CREATING_PREFS -22		 /* Error while trying to create preference file */
#define ERROR_CREATING_COMLINE -23		 /* Error while trying to create command line command */
#define ERROR_ERASING_TAPE -24			 /* Error while trying to erase tape */
#define ERROR_OPENING_FLIST -25			 /* Error opening a filelist */
#define ERROR_CREATING_FIFO -26			 /* Error creating a FIFO */
#define ERROR_OPENING_FIFO -27			 /* Error opening FIFO */
#define ERROR_UNABLE_FORK -28			 /* Unable to fork */
#define ERROR_OPENING_SET -29			 /* Error opening set */
#define ERROR_CREATING_SET -30			 /* Error creating set */
#define ERROR_INFO_ISNT_DIR -31			 /* Info file directory is not a directory */
#define ERROR_OPENING_MAIL -32			 /* Error opening temporary mail file */
#define ERROR_GETTING_BLKSIZE -33		 /* Error trying to get block size */
#define ERROR_SETTING_BLKSIZE -34		 /* Error trying to set block size */
#define ERROR_SETTING_SHARED_MEM -35		 /* Error setting up shared memory regions */
#define ERROR_NO_TAPE_TYPE -36			 /* Haven't set the type of tape drive being used */
#define ERROR_PREF_FORMAT -37			 /* Illegal preference */
#define ERROR_ILLEGAL_TAPE_TYPE -38		 /* Illegal tape type */
#define ERROR_OPENING_PF -39			 /* Unable to open /proc/fileystems */
#define ERROR_UNABLE_MOUNT -40			 /* Unable to mount a device */
#define ERROR_UNABLE_UMOUNT -41			 /* Unable to unmount device */
#define ERROR_SKIPPING -42			 /* Error skipping volumes */
#define ERROR_RENAMING -43			 /* Error doing a rename */
#define ERROR_INFO_FILE -44			 /* Error in info file format */
#define ERROR_FREE_MEMORY -45			 /* Error in freeing memory */
#define ERROR_INFO_MAGIC -46			 /* Illegal info file */
#define ERROR_INFO_OLD -47			 /* Old info file format */
#define ERROR_NO_FSF -48			 /* Tape drive doesn't support fsf */
#define ERROR_COMPRESSING_INFO -49		 /* Error compressing info file */
#define ERROR_NO_BACKUP_CHILD -50		 /* No backup child*/
#define ERROR_NO_RESTORE_CHILD -51		 /* No restore child */
#define ERROR_NO_DISKSPACE -52			 /* Not enough disk space */
#define ERROR_SETTING_SEMAPHORE -53		 /* Error making semaphore */
#define ERROR_SEMAPHORE_ACTION -54		 /* Error incrementing or decrementing semaphore */
#define ERROR_CHILD_SEGFAULT -55		 /* Write child ended in seg fault */
#define ERROR_TAPE_EOM -56			 /* Error doing eom */
#define ERROR_TAPE_GOTO -57			 /* Positioning on a block */
#define ERROR_NONE -99				 /* no error - clean exit */
						 
						 
extern _errstat do_exit(int c);			 

struct err_temp {
    int  err_no;				 /* error number */
    char *mess;					 /* message for this error */
    int  no_append;				 /* ?append message to a strerrno */
    int  exit;					 /* ?exit if got this error */
};

extern struct err_temp errs[];
