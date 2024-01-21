/*
 * Copyright (c) 1990,1991 Regents of The University of Michigan.
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appears in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation, and that the name of The University
 * of Michigan not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission. This software is supplied as is without expressed or
 * implied warranties of any kind.
 *
 *	Research Systems Unix Group
 *	The University of Michigan
 *	c/o Mike Clark
 *	535 W. William Street
 *	Ann Arbor, Michigan
 *	+1-313-763-0525
 *	netatalk@itd.umich.edu
 */

/*
 * AppleDouble entry IDs.
 */
#define ADEID_DFORK	1
#define ADEID_RFORK	2
#define ADEID_NAME	3
#define ADEID_COMMENT	4
#define ADEID_ICONBW	5
#define ADEID_ICONCOL	6
#define ADEID_FILEI	7
#define ADEID_FINDERI	9

#define ADEID_MAX	10

/*
 * AppleDouble entry default offsets.
 * The layout looks like this:
 *
 *	  255	  200		  16	  32		  N
 *	|  NAME	|    COMMENT	| FILEI	|    FINDERI	| RFORK	|
 */
#define ADEDOFF_RFORK	589
#define ADEDOFF_NAME	86
#define ADEDOFF_COMMENT	341
#define ADEDOFF_FILEI	541
#define ADEDOFF_FINDERI	557

#define ADEDLEN_RFORK	0
#define ADEDLEN_NAME	0
#define ADEDLEN_COMMENT	0
#define ADEDLEN_FILEI	16
#define ADEDLEN_FINDERI	32

#define AD_MAGIC	0x00051607
#define AD_VERSION	0x00010000

#define AD_DATASZ	589

/*
 * The header of the AppleDouble Header File looks like this:
 *
 *	NAME			SIZE
 *	====			====
 *	Magic			4
 *	Version			4
 *	Home File System	16
 *	Number of Entries	2
 *	Entry Descriptors for each entry:
 *		Entry ID	4
 *		Offset		4
 *		Length		4
 */

struct ad_entry {
    long	ade_off;
    long	ade_len;
};

struct ad_fd {
    int		adf_fd;
    long	adf_off;
    int		adf_flags;
};

struct adouble {
    int			ad_magic;
    int			ad_version;
    char		ad_homefs[ 16 ];
    struct ad_entry	ad_eid[ ADEID_MAX ];
    char		ad_data[ AD_DATASZ ];
    struct ad_fd	ad_df, ad_hf;
};

#define ADFLAGS_DF	(1<<0)
#define ADFLAGS_HF	(1<<1)
#define ADFLAGS_DIR	(1<<2)

#define ad_dfileno(ad)		((ad)->ad_df.adf_fd)
#define ad_hfileno(ad)		((ad)->ad_hf.adf_fd)
#define ad_getversion(ad)	((ad)->ad_version)
#define ad_gethomefs(ad)	((ad)->ad_homefs);
#define ad_sethomefs(ad,buf) \
	(bcopy(buf,(ad)->ad_homefs,sizeof((ad)->ad_homefs)))
#define ad_getentrylen(ad,eid)	((ad)->ad_eid[(eid)].ade_len)
#define ad_setentrylen(ad,eid,len) \
	((ad)->ad_eid[(eid)].ade_len = (len))
#define ad_entry(ad,eid)	(((ad)->ad_eid[(eid)].ade_off + \
	(ad)->ad_eid[(eid)].ade_len > AD_DATASZ ) ? 0 : \
	(caddr_t)(ad)->ad_data + (ad)->ad_eid[(eid)].ade_off)
#define ad_getoflags(ad,adf)	(((adf)&ADFLAGS_HF) ? \
	(ad)->ad_hf.adf_flags : (ad)->ad_df.adf_flags)
#define ad_dtruncate(ad,size)	(ftruncate((ad)->ad_df.adf_fd,size))

extern char	*ad_path();
