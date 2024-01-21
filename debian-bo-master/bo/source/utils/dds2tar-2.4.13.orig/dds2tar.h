/*
 * This file is part of dds2tar.
 * Copyright by J"org Weule
 */

/*
 * Please change the value ST_BUFFER_BLOCKS at the top of the Makefile
 * on error with these lines.
 */
#if defined ST_BUFFER_BLOCKS && ST_BUFFER_BLOCKS == 0
#undef ST_BUFFER_BLOCKS
#include "/usr/src/linux/drivers/scsi/st_options.h"
#endif

#ifndef ST_BUFFER_BLOCKS
#define ST_BUFFER_BLOCKS 32
#endif

#include <sys/param.h>

extern const char dds_headline[];
extern const char dds_index_format[];
extern const char dds_index_scan_format[];
extern const char dds_old_headline[];
extern const char dds_old_index_format[];
extern const char dds_old_index_scan_format[];
extern const char dds_loctext[];
extern const char dds_locline1[];
extern const char dds_locline2[];

/*
 * It's faster to read some records than to skip over them,
 * if the number is smaller then DONT_SKIP.
 * I really don't know the right number here.
 */
#define DONT_SKIP ((cur_n>0)?((int)(1024/cur_n)):((int)(1024/buf_n)))

typedef union {
	char    chrptr[512];
	struct {
		char    name[100];
		char    dummy1[24];
		char    size[12];
		char    mtime[12];
		char    dummy2[8];
		char    linkflag;
		char    dummy3[100];
		char    magic[8];
	}
	hdr;
}

tar_record;

#define LF_LONGLINK 'K'
#define LF_LONGNAME 'L'

extern int tar_fb;
extern int tar_bs;
extern int tar_n;

extern tar_record *cur_block;
extern int cur_blkno;
extern int next_blkno;
extern int cur_n;
extern int cur_bs;
extern int buf_n;
extern int force_nochk;

extern char *cur_line;

extern int hash_mode;
extern int quick_mode;
extern int device;
extern int verbose;
extern int list_only;
extern FILE *index_fp;
extern int write_body;
extern int long_name_len;
extern char long_name[MAXPATHLEN<<2];


extern int dds_index(void);
extern int dds_cmp(char const *const *pattern);
extern int rt_line(
			  int *const ptr_blkno,
			  int *const ptr_recno,
			  int *const ptr_size,
			  char **const ptr_name
);
extern int rt_loc_line(void);
extern int extract_loc(char const *const *);

#ifdef  EXP_STUFF
extern int tar_dds(int const, char const *const *const);

#endif

extern int dds_is_tar_header_record(tar_record*const);
