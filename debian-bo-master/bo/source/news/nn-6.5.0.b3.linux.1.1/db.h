/*
 *	(c) Copyright 1990, Kim Fabricius Storm.  All rights reserved.
 *
 *	DATABASE ORGANIZATION:
 *
 *	The central nn information is contained in following files:
 *		DB_DIRECTORY/MASTER
 *		DB_DIRECTORY/GROUPS
 *		DB_DIRECTORY/DATA/nnn.x
 *		DB_DIRECTORY/DATA/nnn.d
 *
 * 	The MASTER file consists of a header and one entry for each news
 *	group.  The sequence of the group headers defines the group
 *	number associated with the group.
 *
 * 	The GROUPS file contains the names of the news groups; the names
 *	occur in the same sequence as in the MASTER file.
 *
 *	For each news group, the DATA directory contains two files whose
 *	name is constructed from the group number 'nnn':
 *
 *		nnn.x	Index file
 *		nnn.d	Data file
 *
 *	The index file provides a a mapping from article numbers to offsets
 *	in the data file.
 *
 *	The data file contains the actual header data.  Each article is
 *	represented by a Header, an array of Cross Postings, and the
 *	strings representing the sender name and the article subject:
 *
 *		header
 *		group_number 1 [ if cross posted ]
 *		group_number 2
 *		...
 *		sender name (null terminated) [if sender_length > 0]
 *		subject (null terminated) [if subject_length > 0]
 *
 *	For a digest, cross posted groups are only specified for the
 *	first entry (the header entry).
 *
 *	On disk, the article_number is negative for digest article
 *	header and zero for following sub articles.
 *
 *	The format of the index and data files are specified below.
 *
 *	Unless NETWORK_DATABASE is defined, the database will
 *	will contain machine dependent binary data.
 */

#ifndef _NN_DB_H
#define _NN_DB_H 1

#ifdef NETWORK_DATABASE
typedef int32 cross_post_number;
#ifdef NETWORK_BYTE_ORDER
#define NETW_CROSS_INT(cp) cp
#define NETW_CROSS_EXT(cp) cp
#else
#define NETW_CROSS_INT(cp) ntohl(cp)
#define NETW_CROSS_EXT(cp) htonl(cp)
#endif
#else
typedef group_number cross_post_number;
#define NETW_CROSS_INT(cp) cp
#define NETW_CROSS_EXT(cp) cp
#endif

typedef struct {
    article_number	dh_number;

    time_stamp	dh_date; /* encoded Date: filed (not a time_t value!!) */

    off_t	dh_hpos; /* absolute offset for first byte of header */
    off_t	dh_lpos; /* absolute offset for last byte of article */
    int16	dh_fpos; /* relative offset for first byte in article text */

    int16	dh_lines;
    int8	dh_replies;

    int8	dh_cross_postings;
    int8	dh_subject_length;
    int8	dh_sender_length;
} data_header;

#define DBUF_SIZE	255

typedef struct {
    int			dh_type;

#define	DH_NORMAL		0
#define	DH_DIGEST_HEADER	1
#define DH_SUB_DIGEST		2

    cross_post_number	dh_cross[DBUF_SIZE+1];
    char		dh_sender[DBUF_SIZE+1];
    char		dh_subject[DBUF_SIZE+1];
} data_dynamic_data;


/* open database files */

FILE *open_data_file();

/* data access */

off_t db_read_art();
off_t get_index_offset(), get_data_offset();

extern data_header db_hdr;
extern data_dynamic_data db_data;

#endif /* _NN_DB_H */

