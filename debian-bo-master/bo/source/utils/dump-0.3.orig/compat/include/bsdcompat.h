/*
 *	Ported to Linux's Second Extended File System as part of the
 *	dump and restore backup suit
 *	Remy Card <Remy.Card@freenix.fr>, 1994, 1995
 *
 */

#define	__dead		volatile

#define NBBY		8

#define MIN(a,b)	((a < b) ? a : b)

#define	DEV_BSIZE	512
#define	DEV_BSHIFT	9
#define	MAXBSIZE	EXT2_MAX_BLOCK_SIZE
#define ROOTINO		EXT2_ROOT_INO
#ifdef	EXT2_NODUMP_FL
#define UF_NODUMP	EXT2_NODUMP_FL
#endif

#define howmany(x,y)	(((x)+((y)-1))/(y))
#define roundup(x, y)	((((x)+((y)-1))/(y))*(y))
#define powerof2(x)	((((x)-1)&(x))==0)

#define dbtob(b)	((unsigned)(b) << DEV_BSHIFT)
#define fsbtodb(sb,b)	((int)(((long long)b * EXT2_BLOCK_SIZE(sb->super)) / DEV_BSIZE))

#define	sblock		fs
#define fs_fsize	fragsize
#define fs_bsize	blocksize
#define fs_size		super->s_blocks_count

#define	IFMT		S_IFMT
#define IFLNK		S_IFLNK
#define IFREG		S_IFREG
#define IFDIR		S_IFDIR
#define IFCHR		S_IFCHR
#define IFBLK		S_IFBLK
#define IFSOCK		S_IFSOCK
#define IFFIFO		S_IFIFO

typedef long long		quad_t;
typedef unsigned long long	u_quad_t;

/*
 * This is the ext2_inode structure but the fields have been renamed
 * to match 4.4BSD's names
 */
#define	NDADDR		12
#define	NIADDR		 3

#define NINDIR(fs)	EXT2_ADDR_PER_BLOCK(fs->super)

struct dinode {
	unsigned short	di_mode;
	unsigned short	di_uid;
	unsigned long	di_size;
	unsigned long	di_atime;
	unsigned long	di_ctime;
	unsigned long	di_mtime;
	unsigned long	di_dtime;
	unsigned short	di_gid;
	unsigned short	di_nlink;
	unsigned long	di_blocks;
	unsigned long	di_flags;
	unsigned long	di_reserved1;
	daddr_t		di_db[NDADDR];
	daddr_t		di_ib[NIADDR];
	unsigned long	di_gen;
	unsigned long	di_file_acl;
	unsigned long	di_dir_acl;
	unsigned long	di_faddr;
	unsigned char	di_frag;
	unsigned char	di_fsize;
	unsigned short	di_pad1;
	unsigned long	di_spare[2];
};

#define di_rdev		di_db[0]
/* #define di_ouid		di_uid */
/* #define di_ogid		di_gid */

/*
 * This is the ext2_dir_entry structure but the fields have been renamed
 * to match 4.4BSD's names
 *
 * This is the 4.4BSD directory entry structure
 */
#define DIRBLKSIZ	DEV_BSIZE
#define MAXNAMLEN	255

struct direct {
	unsigned long	d_ino;
	unsigned short	d_reclen;
#if	1
	unsigned char	d_namlen;
	unsigned char	d_type;
#else
	unsigned short	d_namlen;
#endif
	char		d_name[MAXNAMLEN + 1];
};

/*
 * File types
 */
#define DT_UNKNOWN	 0
#define DT_FIFO		 1
#define DT_CHR		 2
#define DT_DIR		 4
#define DT_BLK		 6
#define DT_REG		 8
#define DT_LNK		10
#define DT_SOCK		12

/*
 * Convert between stat structure types and directory types.
 */
#define IFTODT(mode)	(((mode) & 0170000) >> 12)
#define DTTOIF(dirtype)	((dirtype) << 12)

/*
 * The DIRSIZ macro gives the minimum record length which will hold
 * the directory entry.  This requires the amount of space in struct direct
 * without the d_name field, plus enough space for the name with a terminating
 * null byte (dp->d_namlen+1), rounded up to a 4 byte boundary.
 */
#if	0
#if (BYTE_ORDER == LITTLE_ENDIAN)
#define DIRSIZ(oldfmt, dp) \
    ((oldfmt) ? \
    ((sizeof (struct direct) - (MAXNAMLEN+1)) + (((dp)->d_type+1 + 3) &~ 3)) : \
    ((sizeof (struct direct) - (MAXNAMLEN+1)) + (((dp)->d_namlen+1 + 3) &~ 3)))
#else
#define DIRSIZ(oldfmt, dp) \
    ((sizeof (struct direct) - (MAXNAMLEN+1)) + (((dp)->d_namlen+1 + 3) &~ 3))
#endif
#else

#define DIRSIZ(oldfmt,dp)	EXT2_DIR_REC_LEN(((dp)->d_namlen & 0xff) + 1)

#endif

/*
 * This is the old (Net/2) BSD inode structure
 * copied from the FreeBSD 1.1.5.1 <ufs/dinode.h> include file
 */
#define	MAXFASTLINK	(((NDADDR + NIADDR) * sizeof(unsigned long)) - 1)

struct old_bsd_inode {
	unsigned short	di_mode;
	short		di_nlink;
	unsigned short	di_uid;
	unsigned short	di_gid;
#if	1
	union {
		u_quad_t	v;
		unsigned long	val[2];
	}		di_qsize;
#else
	u_quad_t	di_size;
#endif
	unsigned long	di_atime;
	long		di_atspare;
	unsigned long	di_mtime;
	long		di_mtspare;
	unsigned long	di_ctime;
	long		di_ctspare;
#if	0
	union {
		struct {
			daddr_t	di_udb[NDADDR];
			daddr_t	di_uib[NIADDR];
		} di_addr;
		char di_usymlink[MAXFASTLINK + 1];
	}		di_un;
#else
	daddr_t		di_db[NDADDR];
	daddr_t		di_ib[NIADDR];
#endif
	long		di_flags;
	long		di_blocks;
	long		di_gen;
	unsigned long	di_spare[4];
};

/*
 * This is the new (4.4) BSD inode structure
 * copied from the FreeBSD 2.0 <ufs/ufs/dinode.h> include file
 */
struct new_bsd_inode {
	unsigned short	di_mode;
	short		di_nlink;
	union {
		unsigned short	oldids[2];
		unsigned long	inumber;
	}		di_u;
	u_quad_t	di_size;
	struct timeval	di_atime;
	struct timeval	di_mtime;
	struct timeval	di_ctime;
	daddr_t		di_db[NDADDR];
	daddr_t		di_ib[NIADDR];
	unsigned long	di_flags;
	long		di_blocks;
	long		di_gen;
	unsigned long	di_uid;
	unsigned long	di_gid;
	long		di_spare[2];
};

#define	di_ouid		di_u.oldids[0]
#define	di_ogid		di_u.oldids[1]
