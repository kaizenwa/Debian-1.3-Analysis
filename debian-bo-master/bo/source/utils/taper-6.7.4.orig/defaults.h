/*
   Time-stamp: <96/08/05 19:30:55 yusuf>

   $Id: defaults.h,v 1.11 1996/08/05 19:02:00 yusuf Exp $
*/


/* Default values */


#define EXIT_KEY 27				 /* that exits from changing preferences */    
#define ABORT_KEY (KEY_F0+5)			 /* key that aborts from changing preferences */

#define PROC_FILESYSTEMS "/proc/filesystems"	 /* name of where filesystems are found */

#define SCSI_DEFAULT_TAPE "/dev/st0"		 /* for SCSI users */
#define SCSI_DEFAULT_NTAPE "/dev/nst0"
#define ZFTAPE_DEFAULT_TAPE "/dev/qft0"	 	/* for zftape users */
#define ZFTAPE_DEFAULT_NTAPE "/dev/nqft0"	
#define FTAPE_DEFAULT_TAPE "/dev/ftape"		 /* for ftape users */
#define FTAPE_DEFAULT_NTAPE "/dev/nftape"	 
#define REMOVABLE_DEFAULT_TAPE "/dev/fd0"	 /* for removable media users */
#define REMOVABLE_DEFAULT_NTAPE "/dev/fd0"
#define IDE_DEFAULT_TAPE "/dev/ht0"		 /* default for IDE tapes */
#define IDE_DEFAULT_NTAPE "/dev/nht0"

#define DEFAULT_TAPER_INFO_FILES "taper_info"	 /* directory containing info files */
#define DEFAULT_GLOBAL_PREFS "/usr/local/etc"	 /* location of global preference files */
#define DEFAULT_PREFS "taper_prefs"		 /* default preference file name  */
#define DEFAULT_LOG_FILE "taper_log"		 /* default log file name  */

#define MAIL_PROG "mail"			 /* name of mail program */
#define MAIL_TO "root"				 /* who to mail messages to */

#define TAPE_TYPE_ENVIRON "TAPE_TYPE"		 /* type of tape being used */
#define TAPE_ENVIRON "TAPE"			 /* name of environment variable of rewinding tape */
#define NTAPE_ENVIRON "NTAPE"			 /* name of environment variable of non-rewinding tape device */
#define PREFS_ENVIRON "TAPER_PREFS"		 /* name of environment variable of preferences file */
#define LOGFILE_ENVIRON "TAPER_LOG_FILE"	 /* name of environment variable of log file */
#define LOGLEVEL_ENVIRON "TAPER_LOG_LEVEL"	 /* name of environment variable of logging level */
#define TAPER_INFO_FILES_ENVIRON "TAPER_INFO_FILES"/* name of environment variable of information file directories  */

#define COMPRESS_PROG "gzip -c"			 /* Compression to use */
						 /* Note that compression programme must return 0 */
#define DECOMPRESS_PROG "gzip -c -d"  	         /*  to indicate successful completion. If it doesn't
						  *  modify code in backup_file accordingly 

					      	    It is expected that the compression program  
							accepts a filename to compress and then writes
							the output to stdout */

#define MEM_BLOCK_SIZE 100000			 /* no of bytes to allocate in each memory alloc procedure */
#define DEFAULT_MIN_SEEK 10			 /* default # blocks to have to move before seeking */

#define ZFTAPE_DEFAULT_BLOCK_SIZE (28*1024)	 /* zftape expect max 28K blocks */
#define SCSI_DEFAULT_BLOCK_SIZE (28*1024)	 /* otherwise use 28K blocks */
#define FTAPE_DEFAULT_BLOCK_SIZE (28*1024)	 /* plain ftape - use 28K blocks */
#define FILE_DEFAULT_BLOCK_SIZE (32*1024)	 /* regular file */
#define REMOVABLE_DEFAULT_BLOCK_SIZE (32*1024)	 /* removable device */
#define IDE_DEFAULT_BLOCK_SIZE (28*1024)	 /* for IDE  */

#define DEFAULT_TR_SIZE 150000			 /* transfer to hard disk */

#define COMPRESS2_BUFFER_SIZE 1000000		 /* size of compress 2 buffer */

#define DOUBLE_BUFFER_SIZE 65536		 /* size of double buffer - Must be larger than block size*/

#define COMPRESS_MIN_FREE 4096			 /* minimum #K free for compress to work */
/*------------no user serviceable parts beyond here ----------------------------------------*/
#define PRIVATE static
#define PUBLIC 

#define FIFO_ERR "&*^Error&^*"			 /* String returned by FIFO if couldn't compress */

#define TAPE_TYPE_FTAPE 0
#define TAPE_TYPE_ZFTAPE 1
#define TAPE_TYPE_SCSI 2
#define TAPE_TYPE_FILE 3
#define TAPE_TYPE_REMOVABLE 4
#define TAPE_TYPE_IDE 5

#define MAX_ARCHIVETITLE 50			 /* max size of archive title */
#define MAX_FILTER 15				 /* max size of a filter */
#define MAX_FNAME NAME_MAX			 /* max size of filename */

#define TAPER_MAGIC_NUMBER 0x73912376		 /* new from 6.5 onwards */
#define TAPER_64_MAGIC 0x739123		
#define TAPER_4_MAGIC_NUMBER 0x729123		 /* for versions 4.x */
#define VOLUME_MAGIC 0x13921323			 /* for versions 6.6 onwards */
#define VOLUME_65_MAGIC 0x13921322		 /* from 6.5 onwards */
#define VOLUME_64_MAGIC 0x139213
#define VOLUME_MAGIC_INFO 0x43917312		 /* magic if this volume is an info file */

#define INFO_MAGIC 0x67128912			 /* magic number of uncompressed info file */
#define INFO_MAGIC_COMPRESSED 0x22191355	 /* magic number of compressed info file */
#define END_ENTRY 0x04

#define TAPE_EXIST 0				 /* return values for */
#define BAD_MAGIC 1				 /* read vol dir routine */
#define TAPE_NOEXIST 2				 /* tape doesn't exist */
#define TAPE_EXIST_EMPTY 3			 /* tape exists but is empty */

#define COLOR_ONVOL 7
#define COLOR_SELECTED 8
#define COLOR_BOTTOM 9
#define COLOR_TITLE 10

