/* afio.h  defines for afio. */

#ifdef	SYSTIME
#include <sys/time.h>
#else /* SYSTIME */
#include <time.h>
#endif /* SYSTIME */

#ifdef	CTC3B2
#include <sys/vtoc.h>
#include <sys/ct.h>
#endif /* CTC3B2 */

#ifdef MYTEMPNAM
#include <sys/file.h>
#endif

#ifdef USESHMEM
#include <sys/ipc.h>
#include <sys/shm.h>

#define	NUMSHKEYS	20
#define	SHMEMSIZE	262144	/* 2^18 (dev3b1) */
#endif

/* done writing to the archive */
#define	FALSE	0
#define	TRUE	1
#define	NOTDONE	0
#define	DONE	1
#define	NODIE	0
#define	DIE	1

/* KH */
#define MEMFD 10000
#define ZIPFD 10001
int zipfdfd;

/* flags for the st_rdev field of regular files */
 /* file is not compressed, ignore any .z extension */
#define RDEV_NOTCOMPR 1
 /* file is a control file */
#define RDEV_ISCONTROL 2

/*
   An archive entry is a control file if it is a regular file and
   if the ISCONTROL flag is set.  The filename is not important.
*/
#define ISCONTROL(sb) ((((sb)->sb_mode & S_IFMT) == S_IFREG)&&((sb)->sb_rdev & RDEV_ISCONTROL))

/* pseudo filename for control files */
#define CONTROLNAME "CONTROL_FILE"

/* label value if no label given */
#define NOLABEL "no_label"

/*
 * Address link information base.
 */
#define	linkhash(ino)	\
	(linkbase + (ino) % nel(linkbase))

/*
 * Mininum value.
 */
#define	min(one, two)	\
	(one < two ? one : two)

/*
 * Number of array elements.
 */
#define	nel(a)		\
	(sizeof(a) / sizeof(*(a)))

/*
 * Remove a file or directory.
 */
#define	afremove(name, asb) \
	(((asb)->sb_mode & S_IFMT) == S_IFDIR ? rmdir(name) : unlink(name))

/*
 * Swap bytes.
 */
#define	swab(n)		\
	((((ushort)(n) >> 8) & 0xff) | (((ushort)(n) << 8) & 0xff00))

/*
 * Cast and reduce to unsigned short.
 */
#define	ush(n)		\
	(((ushort) (n)) & 0177777)

/*
 * Definitions.
 */
#define	reg	register	/* Convenience */
#define	uint	unsigned int	/* Not always in types.h */
#define	ushort	unsigned short	/* Not always in types.h */
#define	BLOCK	5120		/* Default archive block size */
#define	FSBUF	(8*1024)	/* Filesystem buffer size */
#define	H_COUNT	10		/* Number of items in ASCII header */
#define	H_PRINT	"%06o%06o%06o%06o%06o%06o%06o%011lo%06o%011lo"

/* H_SCAN is obsolete, replaced by PH_SCAN to be more portable. */
#define	H_SCAN	"%6ho%6ho%6ho%6ho%6ho%6ho%6ho%11lo%6o%11lo"

#define PH_SCAN  "%6lo%6lo%6lo%6lo%6lo%6lo%6lo%11lo%6o%11lo"

typedef struct {
long unsigned int st_dev;
long unsigned int st_ino;
long unsigned int st_mode;
long unsigned int st_uid;
long unsigned int st_gid;
long unsigned int st_nlink;
long unsigned int st_rdev;
long unsigned int st_mtime;
long unsigned int st_size;
} PStat;

#define	H_STRLEN 70		/* ASCII header string length */
#define	M_ASCII "070707"	/* ASCII magic number */
#define	M_BINARY 070707		/* Binary magic number */
#define	M_STRLEN 6		/* ASCII magic number length */
#define	NULLDEV	-1		/* Null device code */
#define	NULLINO	0		/* Null inode number */
#define	PATHELEM 256		/* Pathname element count limit */
#define	PATHSIZE 1024		/* Pathname length limit */
#define	S_IFSHF	12		/* File type shift (shb in stat.h) */
#define	S_IPERM	07777		/* File permission bits (shb in stat.h) */
#define	S_IPEXE	07000		/* Special execution bits (shb in stat.h) */
#define	S_IPOPN	0777		/* Open access bits (shb in stat.h) */
#define	STDIN	0		/* Standard input file descriptor */
#define	STDOUT	1		/* Standard output file descriptor */
#define	TTY	"/dev/tty"	/* For volume-change queries */

#ifndef PRG_COMPRESS
#define PRG_COMPRESS "compress"
#endif

/*
 * Some versions of the portable "C" compiler (PCC) can't handle
 * pointers to functions returning void.
 */
#ifdef	VOIDFIX
#define	VOIDFN	void		/* Expect "void (*fnptr)()" to work */
#else /* VOIDFIX */
#define	VOIDFN	int		/* Avoid PCC "void (*fnptr)()" bug */
#endif /* VOIDFIX */

/*
 * Trailer pathnames. All must be of the same length.
 */
#define	TRAILER	"TRAILER!!!"	/* Archive trailer (cpio compatible) */
#define	TRAILZ	11		/* Trailer pathname length (including null) */

/*
 * Open modes; there is no <fcntl.h> with v7 UNIX.
 */
#ifdef HAVEFCNTL
#include <fcntl.h>
#else
#define	O_RDONLY 0		/* Read-only */
#define	O_WRONLY 1		/* Write-only */
#define	O_RDWR	2		/* Read/write */
#endif
/*
 * V7 and BSD UNIX use old-fashioned names for a couple of
 * string functions.
 */
#ifdef	INDEX
#define	strchr	index		/* Forward character search */
#define	strrchr	rindex		/* Reverse character search */
#endif /* INDEX */

/*
 * Some compilers can't handle void casts.
 */
#ifdef	NOVOID
#define	VOID			/* Omit void casts */
#else /* NOVOID */
#define	VOID	(void)		/* Quiet lint about ignored return values */
#endif /* NOVOID */

/*
 * Adb is more palatable when static functions and variables are
 * declared as globals. Lint gives more useful information when
 * statics are truly static.
 */
#ifdef	lint
#define	STATIC	static		/* Declare static variables for lint */
#else /* lint */
#define	STATIC			/* Make static variables global for adb */
#endif /* lint */

/*
 * Simple types.
 */
typedef struct group Group;	/* Structure for getgrgid(3) */
typedef struct passwd Passwd;	/* Structure for getpwuid(3) */
typedef struct tm Time;		/* Structure for localtime(3) */

#ifdef	S_IFLNK
/*
 * File status with symbolic links. Kludged to hold symbolic
 * link pathname within structure.
 */
typedef struct
{
  struct stat sb_stat;
  char sb_link[PATHSIZE];
} Stat;

#define	STAT(name, asb)		stat(name, &(asb)->sb_stat)
#define	FSTAT(fd, asb)		fstat(fd, &(asb)->sb_stat)
#define	LSTAT(name, asb)	lstat(name, &(asb)->sb_stat)
#define	sb_dev		sb_stat.st_dev
#define	sb_ino		sb_stat.st_ino
#define	sb_mode		sb_stat.st_mode
#define	sb_nlink	sb_stat.st_nlink
#define	sb_uid		sb_stat.st_uid
#define	sb_gid		sb_stat.st_gid
#define	sb_rdev		sb_stat.st_rdev
#define	sb_size		sb_stat.st_size
#define	sb_atime	sb_stat.st_atime
#define	sb_mtime	sb_stat.st_mtime
#define	sb_ctime	sb_stat.st_ctime
#define	sb_blksize	sb_stat.st_blksize
#define	sb_blocks	sb_stat.st_blocks
#else /* !S_IFLNK */
/*
 * File status without symbolic links.
 */
typedef struct stat Stat;
#define	STAT(name, asb)		stat(name, asb)
#define	FSTAT(fd, asb)		fstat(fd, asb)
#define	LSTAT(name, asb)	stat(name, asb)
#define	sb_dev		st_dev
#define	sb_ino		st_ino
#define	sb_mode		st_mode
#define	sb_nlink	st_nlink
#define	sb_uid		st_uid
#define	sb_gid		st_gid
#define	sb_rdev		st_rdev
#define	sb_size		st_size
#define	sb_atime	st_atime
#define	sb_mtime	st_mtime
#define	sb_ctime	st_ctime
#endif /* !S_IFLNK */

/*
 * Binary archive header (obsolete).
 */
typedef struct
{
  short b_dev;			/* Device code */
  ushort b_ino;			/* Inode number */
  ushort b_mode;		/* Type and permissions */
  ushort b_uid;			/* Owner */
  ushort b_gid;			/* Group */
  short b_nlink;		/* Number of links */
  short b_rdev;			/* Real device */
  ushort b_mtime[2];		/* Modification time (hi/lo) */
  ushort b_name;		/* Length of pathname (with null) */
  ushort b_size[2];		/* Length of data */
} Binary;

/*
 * Child process structure.
 */
typedef struct child
{
  struct child *c_forw;		/* Forward link */
  int c_pid;			/* Process ID */
  int c_flags;			/* Flags (CF_) */
  int c_status;			/* Exit status */
} Child;

/*
 * Child process flags (c_flags).
 */
#define	CF_EXIT	(1<<0)		/* Exited */

/*
 * Hard link sources. One or more are chained from each link
 * structure.
 */
typedef struct name
{
  struct name *p_forw;		/* Forward chain (terminated) */
  struct name *p_back;		/* Backward chain (circular) */
  char *p_name;			/* Pathname to link from */
} Path;

/*
 * File linking information. One entry exists for each unique
 * file with with outstanding hard links.
 */
typedef struct link
{
  struct link *l_forw;		/* Forward chain (terminated) */
  struct link *l_back;		/* Backward chain (terminated) */
  dev_t l_dev;			/* Device */
  ino_t l_ino;			/* Inode */
  ushort l_nlink;		/* Unresolved link count */
  off_t l_size;			/* Length */
  Path *l_path;			/* Pathname(s) to link from */
} Link;


/*
 * Internal functions.
 */
VOIDFN copyin ();
VOIDFN copyout ();
void compressfile ();
int dirchg ();
int dirmake ();
int dirneed ();
void fatal ();
void goodbye ();
VOIDFN in ();
void inalloc ();
int inascii ();
int inavail ();
int inbinary ();
int indata ();
int inentry ();
int infill ();
int inhead ();
int inread ();
int inskip ();
int inswab ();
int lineget ();
void linkalso ();
Link *linkfrom ();
void linkleft ();
Link *linkto ();
#ifndef MEMCPY
char *memcpy ();
#endif
char *memget ();
char *memstr ();
#ifndef MKDIR
int mkdir ();
#endif
void nameadd ();
int namecmp ();
int nameopt ();
void next ();
void nextask ();
void nextclos ();
int nextopen ();
int openin ();
int openotty ();
int openqtty ();
int options ();
off_t optsize ();
VOIDFN out ();
void outalloc ();
uint outavail ();
int outdata ();

void outdatazip (); /* added KH */
void handler();     /* added KH */
void waitforgzip();     /* added KH */
void outdatamem (); /* added KH */
void memreset(); /* added KH */
int memread(char *buf,int count); /* added KH */
void memfree(); /* added KH */
int readcompexts(); /* added KH */
int nameaddfile(); /* added KH */

void outeof ();
void outflush ();
void outhead ();
void outpad ();
void outwait ();
void outwrite ();
VOIDFN pass ();
void passdata ();
int passitem ();
int pipechld ();
int pipeopen ();
void pipewait ();
void prsize ();
VOIDFN readcheck ();
#ifndef MKDIR
int rmdir ();
#endif
#ifndef linux
VOIDFN (*signal ())();
#endif
     int fswrite ();
#ifdef USESHMEM
     char *shmemalloc ();
     void shmemfree ();
#endif
     char *syserr ();
     VOIDFN toc ();
     void tocentry ();
     void tocmode ();
     void usage ();
     void verify ();
     int warn ();
     int warnarch ();
     int writedisk ();
     int xfork ();
     void xpause ();
     int xwait ();

extern	void clear_args(void);
extern  void add_arg(char *arg);
extern  char *compress_arg_list[];

extern short lflag;
extern short hflag;

extern int gzipfactor;
extern off_t maxmem;
extern long compthreshold;
extern int ignoreslash;
extern short Zflag;
extern int arfd;

extern int forceZflag;
extern char *compressprog;
extern int compressargs;
