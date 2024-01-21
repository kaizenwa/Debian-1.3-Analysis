#ifndef __STAT_LOADED
#define __STAT_LOADED	1

/*	STAT - V3.0	*/

/*
 * Include file for use with stat/fstat UNIX emulation functions
 */

typedef unsigned off_t;
typedef unsigned short ino_t;
typedef char *dev_t;
struct  stat
{
        dev_t   st_dev;		/* pointer to physical device name */
        ino_t   st_ino[3];	/* 3 words to receive fid */
        unsigned short st_mode;	/* file "mode" i.e. prot, dir, reg, etc. */
        int	st_nlink;	/* for compatibility - not really used */
        unsigned st_uid;	/* from ACP - QIO uic field */
        unsigned short st_gid;	/* group number extracted from st_uid */
        dev_t   st_rdev;	/* for compatibility - always zero */
        off_t   st_size;	/* file size in bytes */
        unsigned st_atime;	/* file access time; always same as st_mtime */
        unsigned st_mtime;	/* last modification time */
        unsigned st_ctime;	/* file creation time */
	char	st_fab_rfm;	/* record format */
	char	st_fab_rat;	/* record attributes */
	char	st_fab_fsz;	/* fixed header size */
	unsigned st_fab_mrs;	/* record size */
};

typedef struct stat stat_t; 

#define S_IFMT   0170000         /* type of file */
#define          S_IFDIR 0040000 /* directory */
#define          S_IFCHR 0020000 /* character special */
#define          S_IFBLK 0060000 /* block special */
#define          S_IFREG 0100000 /* regular */
#define          S_IFMPC 0030000 /* multiplexed char special */
#define          S_IFMPB 0070000 /* multiplexed block special */
#define S_ISUID  0004000         /* set user id on execution */
#define S_ISGID  0002000         /* set group id on execution */
#define S_ISVTX  0001000         /* save swapped text even after use */
#define S_IREAD  0000400         /* read permission, owner */
#define S_IWRITE 0000200         /* write permission, owner */
#define S_IEXEC  0000100         /* execute/search permission, owner */

#endif					/* __STAT_LOADED */
