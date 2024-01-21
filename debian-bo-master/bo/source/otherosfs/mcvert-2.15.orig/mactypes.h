#include <stdio.h>
#include <sys/types.h>
/*
#include <sys/dir.h>
*/
#include <sys/stat.h>

#ifdef TIMEVAL
#  include <sys/param.h>
#  include <sys/time.h>
#else
#  include <sys/timeb.h>
#endif

typedef unsigned char byte;     /* one byte, obviously */
typedef unsigned short word;    /* must be 2 bytes */
#ifndef ULONG
	typedef unsigned long ulong; /* 4 bytes */
#endif

/* declarations to keep lint informed */
char *calloc();
#include <string.h>
void exit();
long time();
ulong unix2mac();
ulong mac2unix();
char *ctime();
FILE *mopen();

#define TRUE  1
#define FALSE 0
#define CR 0x0d
#define LF 0x0a

/* various files and debug options */
extern FILE *devnull;	/* constant /dev/null for bit bucket output */
extern FILE *convert;	/* convert messages */
extern FILE *verbose;	/* verbose output */
extern FILE *debug;	/* debug output */
extern int   Debug;	/* debug level */
#define DEBUG (debug != devnull)

/* Compatibility issues */

#ifdef NOBYTEORDER
#define mac2word
#define mac2long
#define word2mac
#define long2mac
#else
#include <netinet/in.h>
#define mac2word (word) ntohs
#define mac2long (ulong) ntohl
#define word2mac (word) htons
#define long2mac (ulong) htonl
#endif

#ifdef NOBZEROBCOPY
#define bzero(dst,len) (memset(dst,0,len))
#define bcopy(src,dst,len) (memcpy(dst,src,len))
#endif

#define MAXEXTENSION 5          /* max(".bin", ".text", ".data", ".rsrc") */
#define SYSNAMELEN 1024         /* reasonable UNIX working name length */
#define NAMELEN 63              /* maximum legal Mac file name length */

/* Format of a bin file:
A bin file is composed of 128 byte blocks.  The first block is the
info_header (see below).  Then comes the data fork, null padded to fill the
last block.  Then comes the resource fork, padded to fill the last block.  A
proposal to follow with the text of the Get Info box has not been implemented,
to the best of my knowledge.  Version, zero1 and zero2 are what the receiving
program looks at to determine if a MacBinary transfer is being initiated.
*/

/* info file header (128 bytes) */
typedef struct {
	/* Unfortunately, the longs don't align to word boundaries */

/* decimal offset */

/* 000 */
        byte version;           /* there is only a version 0 at this time */
/* 001 */
        byte nlen;              /* Length of filename. */
/* 002 */
        byte name[NAMELEN];     /* Filename (only 1st nlen are significant) */
/* 065 */
        byte type[4];           /* File type. */
/* 069 */
        byte auth[4];           /* File creator. */
/* 073 */
        byte flags;             /* file flags: LkIvBnSyBzByChIt */
                                /* Locked, Invisible,Bundle, System */
                                /* Bozo, Busy, Changed, Init */
/* 074 */
        byte zero1;
/* 075 */
        byte icon_vert[2];      /* Vertical icon position within window */
/* 077 */
        byte icon_horiz[2];     /* Horizontal icon postion in window */
/* 079 */
        byte window_id[2];      /* Window or folder ID. */
/* 081 */
        byte protect;           /* = 1 for protected file, 0 otherwise */
/* 082 */
        byte zero2;
/* 083 */
        byte dlen[4];           /* Data Fork length (bytes) -   most sig.  */
/* 087 */
        byte rlen[4];           /* Resource Fork length         byte first */
/* 091 */
        byte ctim[4];           /* File's creation date. */
/* 095 */
        byte mtim[4];           /* File's "last modified" date. */
/* 099 */
        byte ilen[2];           /* GetInfo message length */
/* 101 */
        byte flags2;            /* Finder flags, bits 0-7 */
/* 102 */
        byte unused[14];
/* 116 */
        byte packlen[4];        /* length of total files when unpacked */
/* 120 */
        byte headlen[2];        /* length of secondary header */
/* 122 */
        byte uploadvers;        /* Version of MacBinary II that the uploading
                                 * program is written for */
/* 123 */
        byte readvers;          /* Minimum MacBinary II version needed to read
                                 * this file */
/* 124 */
        byte crc[2];            /* CRC of the previous 124 bytes */
/* 126 */
        byte padding[2];        /* two trailing unused bytes */
/* 128 */
}      info_header;

/* The *.info file of a MacTerminal file transfer either has exactly this
structure or has the protect bit in bit 6 (near the sign bit) of byte zero1.
The code I have for macbin suggests the difference, but I'm not so sure */

/*
 * Format of a hqx file:
 * (but see also binhex-40-specs.txt for legal variations)
 * 
 * It begins with a line that begins "(This file" and the rest is 64
 * character lines (except possibly the last, and not including
 * newlines) where the first begins and the last ends with a colon.
 * The characters between colons should be only from the set in tr86,
 * below, each of which corresponds to 6 bits of data.  Once that is
 * translated to 8 bit bytes, you have the real data, except that the
 * byte 0x90 may indicate, if the following character is nonzero, that
 * the previous byte is to be repeated 1 to 255 times all together
 * (that is, the count following 0x90 contains the total run length,
 * not the marginal repeat count).  The byte 0x90 is represented by
 * 0x9000.  The information in the file is the hqx_buf (see below), a
 * CRC word, the data fork, a CRC word, the resource fork, and a CRC
 * word.  There is considerable confusion about the flags.  An
 * official looking document unclearly states that the init bit is
 * always clear, as is the following byte.  The experience of others
 * suggests, however, that this is not the case.
 */

/* NOTE:
 * Jskud 25Jun92

 * a hqx file has no date/time information, and a binary constructed
 * from a hqx file will use the current time; therefore, reconverting
 * a hqx file repeatedly will generate different bin files!

 * The "flags" byte can change when refetched, for example, whether or
 * not the init bit is set (that is, the Finder has "seen" the file);
 * this can affect the hqx file, including the run length encoding, so
 * the hqx files can look quite different, although the actual
 * difference is miniscule; also, the init bit difference will not be
 * present when the hqx file is converted to a bin file, since both the
 * locked and init bits are cleared on bin file creation.

 * Because we've experienced this "spurious" difference, and since
 * the init bit is only thought meaningful when running on the Mac,
 * and to establish symmetry between bin file creation and hqx file
 * creation, we unconditionally clear the init and locked bits when
 * creating the hqx file.

 */

#define HQXLINELEN 64
typedef struct {
/* decimal offset */
/* 000 */
        byte version;           /* there is only a version 0 at this time */
/* 001 */
        byte type[4];           /* File type. */
/* 005 */
        byte auth[4];           /* File creator. */
/* 009 */
        byte flags;             /* file flags: LkIvBnSyBzByChIt */
/* 010 */
        byte protect;           /* ?Pr??????, don't know what ? bits mean */
/* 011 */
        byte dlen[4];           /* Data Fork length (bytes) -   most sig.  */
/* 015 */
        byte rlen[4];           /* Resource Fork length         byte first */
/* 019 */
        byte bugblank;          /* to fix obscure sun 3/60 problem that always
                                 * makes sizeof(hqx_header) even */
/* 020 */
}      hqx_header;

/* hqx file header buffer (includes file name) */
typedef struct {
        byte nlen;              /* Length of filename. */
        byte name[NAMELEN];     /* Filename: only nlen actually appear */
        hqx_header all_the_rest;/* and all the rest follows immediately */
}      hqx_buf;

/* every valid hqx header is at least this long */
#define MIN_HQX_HDR (sizeof(hqx_header) + 2)

/*
 *	The minimum number of hqx_lines in a file to ensure that we have
 *	enough lines to emit the entire header before starting a new file.
 * + 1 is for rounding, + 2 is for "(This file " and the blank line.
 */
#define MIN_HQX_LINES (sizeof(hqx_buf) * 4 / 3 / HQXLINELEN + 1 + 2)


/* Format of a Packit file:
Repeat the following sequence for each file in the Packit file:
    4 byte identifier ("PMag" = not compressed, "Pma4" = compressed)
    320 byte compression data (if compressed file)
        = preorder transversal of Huffman tree
        255 0 bits corresponding to nonleaf nodes
        256 1 bits corresponding to leaf nodes
        256 bytes associating leaf nodes with bytes
        1   completely wasted bit
    92 byte header (see pit_header below) *
    2 bytes CRC word for header *
    data fork (length from header) *
    resource fork (length from header) *
    2 bytes CRC word for forks *

Last file is followed by the 4 byte Ascii string, "Pend", and then the EOF.
The CRC calculations differ from those in the binhex format.

* these are in compressed form if compression is on for the file

*/

typedef struct {                /* Packit file header (92 bytes) */
        byte nlen;              /* Length of filename. */
        byte name[NAMELEN];     /* Filename (only 1st nlen are significant) */
        byte type[4];           /* File type. */
        byte auth[4];           /* File creator. */
        byte flags;             /* file flags: LkIvBnSyBzByChIt */
        byte zero1;
        byte protect;           /* = 1 for protected file, 0 otherwise */
        byte zero2;
        byte dlen[4];           /* Data Fork length (bytes) -   most sig.  */
        byte rlen[4];           /* Resource Fork length         byte first */
        byte ctim[4];           /* File's creation date. */
        byte mtim[4];           /* File's "last modified" date. */
}      pit_header;

/* types for constructing the Huffman tree */
typedef struct branch_st {
        byte flag;
        struct branch_st *one, *zero;
}         branch;

typedef struct leaf_st {
        byte flag;
        byte data;
}       leaf;
