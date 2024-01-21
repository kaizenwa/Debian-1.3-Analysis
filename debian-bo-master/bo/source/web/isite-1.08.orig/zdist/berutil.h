/*
(c)1992 OCLC Online Computer Library Center, Inc., 6565 Frantz Road, Dublin,
Ohio 43017-0702.  OCLC is a registered trademark of OCLC Online Computer
Library Center, Inc.

NOTICE TO USERS:  The BER Utilities ("Software") has been developed by OCLC
Online Computer Library Center, Inc.  Subject to the terms and conditions set
forth below, OCLC grants to user a perpetual, non-exclusive, royalty-free
license to use, reproduce, alter, modify, and create derivative works from
Software, and to sublicense Software subject to the following terms and
conditions:

SOFTWARE IS PROVIDED AS IS.  OCLC MAKES NO WARRANTIES, REPRESENTATIONS, OR
GUARANTEES WHETHER EXPRESS OR IMPLIED REGARDING SOFTWARE, ITS FITNESS FOR ANY
PARTICULAR PURPOSE, OR THE ACCURACY OF THE INFORMATION CONTAINED THEREIN.

User agrees that OCLC shall have no liability to user arising therefrom,
regardless of the basis of the action, including liability for special,
consequential, exemplary, or incidental damages, including lost profits,
even if it has been advised of the possibility thereof.

User shall cause the copyright notice of OCLC to appear on all copies of
Software, including derivative works made therefrom.
*/

/****

NOTE:  You will want to define your machine's integer byte order below, if
you are not defining it externally.  OCLC and Stanford use a file named
portinfo.h to define port specific values and include it before including
berutil.h.  If you do not define NATIVE_ORDER in such a way, you will want
to examine the definition of NATIVE_ORDER below.

You may also want to change the definition of COPY from memcpy to bcopy.

****/

#ifndef BERUTIL_H
#define BERUTIL_H

#ifndef __WINDOWS_H
#define far
#endif
#include "gdt.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef COPY
#define COPY(t, s, n) memcpy(t, s, n)
#endif

#ifndef NATIVE_ORDER
/* byteorder values */
#define HIGHTOLOW 1
#define LOWTOHIGH 2
#define PDP_ORDER 4

/* define native byte ordering here.  Options are HIGHTOLOW (most rational
   machines), LOWTOHIGH (ibm pc's) or PDP_ORDER */

#define NATIVE_ORDER HIGHTOLOW  /* most UNIX machines */
#endif

#ifndef CHAR
#define CHAR unsigned char
#endif

#define far

/* ASN.1 tag classes */
#define ASN1_UNIVERSAL   (CHAR)0
#define ASN1_APPLICATION (CHAR)1
#define ASN1_CONTEXT     (CHAR)2
#define ASN1_PRIVATE     (CHAR)3

/* ASN.1 tag forms */
#define ASN1_PRIMITIVE   (CHAR)0
#define ASN1_CONSTRUCTED (CHAR)1

/* ASN.1 UNIVERSAL data types */
#define ASN1_BOOLEAN           1
#define ASN1_INTEGER           2
#define ASN1_BITSTRING         3
#define ASN1_OCTETSTRING       4
#define ASN1_NULL              5
#define ASN1_OBJECTIDENTIFIER  6
#define ASN1_OBJECTDESCRIPTOR  7
#define ASN1_EXTERNAL          8
#define ASN1_SEQUENCE         16
#define ASN1_SET              17
#define ASN1_VISIBLESTRING    26
#define ASN1_GENERALSTRING    27

/* ASN.1 EXTERNAL encoding choices */
#define ASN1_single_ASN1_type  0
#define ASN1_octet_aligned     1
#define ASN1_arbitrary         2

#define NULL_DIR  ((DATA_DIR far *)NULL)

#define TAG_LEN(dir) ((dir->fldid<31) ? 1 : ((dir->fldid<128) ? 2 : 3))
#define LEN_LEN(dir) (dir->length<128L ? 1L:(dir->length<256L ? 2L :\
(dir->length<65536L ? 3L : 4L)))

typedef struct blk_dir
{
	short           block_type;
	struct dir_node far *dir;
	struct dir_node far *next_dir;
	struct dir_node far *last_dir;
	int             num_blocks;
	int             num_nodes;
	struct dir_node far* far *block_array;
} BLK_DIR;

typedef struct dir_node
{
	struct blk_dir  far *block;  /* dir block containing directory */
	struct dir_node far *parent; /* the parent of this node */
	struct dir_node far *prev;   /* the previous sibling of this node */
	struct dir_node far *next;   /* the next sibling of this node */
	union
	{
	    struct dir_node far *child;  /* first child, if constructed */
	    CHAR far*        data;       /* data, if primitive */
	} ptr;
	INT4 length;             /* if rebuilding: length of encoded data */
	INT4 count;              /* number of subfields, if constructed field
				    field length, if primitive field */
	INT4 user;               /* buffer for user assigned data */
	INT4 number;             /* buffer for holding numeric data */
	unsigned short fldid;    /* type of field */
	CHAR           Class;    /* ASN.1 class:
				      00 = universal
				      01 = application
				      10 = context specific
				      11 = private */
	CHAR           form;     /* ASN.1 primitive vs. constructed identifier
				      0 = primitive
				      1 = constructed */
} DATA_DIR;
typedef DATA_DIR far *PDATA_DIR;

extern int berutil_ByteOrder; /* 0 for LOW_TO_HIGH, 1 for HIGH_TO_LOW */

#ifdef SHORT_NAMES
#define asm_rec       asmrec
#define bld_dir       blddir
#define bld_rec       bldrec
#define dmake_bigger  dmbigger
#define dreplace_num  drepnum
#define dtag_found    dtagfnd
#define get_len       getlen
#define get_num       getnum
#define get_tag       gettag
#define IsCompleteBER iscomber
#define hex_dir       hexdir
#define hex_dirf      hexdirf
#define rec_len       reclen
#endif

#ifndef NO_PROTOTYPES
extern INT4          asm_rec(DATA_DIR far *, CHAR far *);
extern INT4          asn1len(CHAR far *);
extern int           bld_dir(CHAR far *, DATA_DIR far*);
extern CHAR far     *bld_rec(DATA_DIR far*, INT4*);
extern DATA_DIR far *daddchar(DATA_DIR far*, unsigned, CHAR, CHAR far *,
							  INT4);
extern DATA_DIR far *dadddir(DATA_DIR far*, DATA_DIR far*);
extern DATA_DIR far *daddbits(DATA_DIR far*, unsigned, CHAR, char far *);
extern DATA_DIR far *daddnum(DATA_DIR far*, unsigned, CHAR, CHAR far *,
							 int);
extern DATA_DIR far *daddoid(DATA_DIR far*, unsigned, CHAR, char far *);
extern DATA_DIR far *daddtag(DATA_DIR far*, unsigned, CHAR);
extern DATA_DIR far *dalloc(int);
extern int           ddeldir(DATA_DIR far*);
extern int           dfree(DATA_DIR far*);
extern char far     *dgetbits(DATA_DIR far*);
extern INT4          dgetnum(DATA_DIR far*);
extern char far     *dgetoid(DATA_DIR far *dir);
extern DATA_DIR far *dinit(DATA_DIR far*, unsigned, CHAR);
extern DATA_DIR far *dinstag(DATA_DIR far*, unsigned, CHAR);
extern DATA_DIR far *dmake(unsigned, CHAR, int);
extern int           dmake_bigger(DATA_DIR far*);
extern CHAR     far *dmalloc(DATA_DIR far *dir, int len);
extern DATA_DIR far *dreplace_num(DATA_DIR far*, INT4);
extern int           dtag_found(DATA_DIR far*, unsigned, CHAR);
extern int           get_len(INT4* reclen, CHAR far *ptr, INT4 len);
extern INT4          get_num(CHAR far*, INT4);
extern int           get_tag(int *tag, CHAR far *s, INT4 len);
extern int           IsCompleteBER(CHAR far *ptr, INT4 len, INT4 *remainder);
extern void          hex_dir(DATA_DIR far*, int);
extern void          hex_dirf(DATA_DIR*, int, FILE*);
extern INT4          rec_len(DATA_DIR far*);

#else
extern INT4          asm_rec();
extern INT4          asn1len();
extern int           bld_dir();
extern CHAR far     *bld_rec();
extern DATA_DIR far *daddchar();
extern DATA_DIR far *dadddir();
extern DATA_DIR far *daddnum();
extern DATA_DIR far *daddtag();
extern DATA_DIR far *dalloc();
extern int           ddeldir();
extern int           dfree();
extern INT4          dgetnum();
extern DATA_DIR far *dinit();
extern DATA_DIR far *dinstag();
extern DATA_DIR far *dmake();
extern int           dmake_bigger();
extern DATA_DIR far *dreplace_num();
extern int           dtag_found();
extern int           get_len();
extern INT4          get_num();
extern int           get_tag();
extern void          hex_dir();
extern void          hex_dirf();
extern INT4          rec_len();
#endif

#ifdef __cplusplus
}
#endif

#endif  /* #ifndef BERUTIL_H */

