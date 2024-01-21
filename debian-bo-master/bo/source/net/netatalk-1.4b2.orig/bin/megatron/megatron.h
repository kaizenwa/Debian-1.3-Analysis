#ifndef	STDIN
#	define	STDIN	"-"
#endif

/*
    Where it matters, data stored in either of these two structs is in
    network byte order.  Any routines that need to interpret this data
    locally would need to do the conversion.  Mostly this affects the 
    fork length variables.  Time values are affected as well, if any
    routines actually need to look at them.
 */

#define	NAMESIZ		64
#define COMMENTSIZ	200

#define DATA		0
#define RESOURCE	1
#define NUMFORKS	2

#define HEX2NAD		0	/* unhex */
#define BIN2NAD		1	/* unbin */
#define SINGLE2NAD	2	/* unsingle */
#define NAD2BIN		3	/* macbinary */
#define HEX2BIN		4	/* hqx2bin */
#define SINGLE2BIN	5	/* single2bin */
#define MEGATRON	6	/* megatron, default, usually HEX2NAD */
#define CONVERTS	7	/* # conversions defined */

struct FInfo {
    long		fdType;
    long		fdCreator;
    short		fdFlags;
    long		fdLocation;
    short		fdFldr;
};

struct FHeader {
    char		name[ NAMESIZ ];
    char		comment[ COMMENTSIZ ];
    long		forklen[ NUMFORKS ];
    u_long		create_date;
    u_long		mod_date;
    u_long		backup_date;
    struct FInfo	finder_info;
};

#define FILEIOFF_CREATE	0
#define FILEIOFF_MODIFY	4
#define FILEIOFF_BACKUP	8
#define FILEIOFF_ATTR	14
#define FINDERIOFF_TYPE		0
#define FINDERIOFF_CREATOR	4
#define FINDERIOFF_FLAGS	8
#define FINDERIOFF_LOC		10
#define FINDERIOFF_FLDR		14

/*
 * Difference in seconds between mac time and unix time, I think,
 * and the time suggested by Apple for unknown or zero time
 * in the AppleSingle/AppleDouble spec.
 */
#define TIME_DIFF	2082826800
#define TIME_ZERO	0x80000000L

#define	TRASH		0
#define	KEEP		1

#ifndef S_ISDIR
#	define S_ISDIR(s)	(( s & S_IFMT ) == S_IFDIR )
#endif
