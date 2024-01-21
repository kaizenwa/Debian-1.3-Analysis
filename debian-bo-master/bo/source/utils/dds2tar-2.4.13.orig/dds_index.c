
/*
 * This file is part of dds2tar.
 * Copyright by J"org Weule
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/mtio.h>
#include <unistd.h>
#include <string.h>
#include "dds2tar.h"
#include "dds_tape.h"

static int		cur_rec;
static int		cur_byte;
static const	char	hash_sign = '#';

/********
	* Skip to the next block.
	*
	********************************************************************/
void dds_index_skip(){
	/*
	 * The hp-DAT is slowed down with dds_getpos().
                * Is there a reason for this?
	 *
	 * Now the cur_blkno is incremented.
	 */
	cur_rec -= cur_bs;
	dds_read_next_block();
	cur_byte += cur_n;
	if (hash_mode == 1)
		if ((cur_blkno & 0x1f) == 0) {
			if ((cur_blkno & 0x7ff) == 0) {
				fprintf(stderr, " %d\n",cur_byte);
				fflush(stderr);
			}
			write(2, &hash_sign, 1);
		}
}

/********
	* Converting a tar-record to an entry in the index file.
	*
	*  input: tar_record is the header of one file in the archive.
        *         The location of the header is given by the
	*         block and index value.
	* output: The returned index is the location of the next header.
	*         Normalization of the pair (block,rec) with respect to
	*         the block length will give the right position.
	* extern: index_fp, verbose
	*         index_fp is the FILE to write the rec.
	*         verbose indicates the loggin mode.
	*
	*         If the name length of the file name is zero,
	*         and an empty block is found, the end
	*         of the archive is found and the program stops.
	*
	********************************************************************/
static int
dds_index_h(tar_record *const ptr, int const block, int rec)
{
	int     size;
	char   *p = ptr->hdr.magic;

#ifdef DDS_TRACE
	fputs("dds_index_h()\n", stderr);
#endif

	if ( dds_is_tar_header_record(ptr) == 0 ) {
		fprintf(stderr,"dds2index: skipping to next header\n");
		return ( rec + 1 ) ;
	}

	if (*p == 0)
		p = "magic ";
	if (ptr->hdr.linkflag == 'V') p = "label ";
	else if (p[6] == ' ')
		p[6] = 0;
	if (ptr->hdr.name[0] == 0) {
		int     i = 0;

		while ((((char *) ptr)[i] == 0) && (i < 512))
			i++;
		if (i == 512) {
			fprintf(index_fp, dds_index_format,
				"-end- ",
				block,
				rec,
				0, "(null)");
			close(device);
			exit(0);
		}
	}
	if (ptr->hdr.size[0] != 0)
		sscanf(ptr->hdr.size, "%o", &size);
	else
		size = 0;
	long_name_len = 0 ;
        if ( ptr->hdr.linkflag == LF_LONGNAME ) long_name_len = size ;
		
	{
		char *name = ptr->hdr.name ;
		if ( ptr->hdr.linkflag != LF_LONGLINK &&
		     ptr->hdr.linkflag != LF_LONGNAME &&
			long_name[0] != '\0' ) name = long_name ;
		sprintf(cur_line, dds_index_format,
			p,
			block,
			rec,
			size,
			name
			);
		fputs(cur_line, index_fp);
		if ( ptr->hdr.linkflag != LF_LONGLINK &&
		     ptr->hdr.linkflag != LF_LONGNAME ) {
			long_name[0] = '\0' ;
		}
	}
	if (verbose)
		fputs(cur_line, stderr);
	/* calculate the number of records */
	/* there was one header record */
	size += 511 + 512;
	size >>= 9;
	rec += size;
	return rec;
}

/********
        * Converting a table line to the numbers.
        *
        */
int
rt_line(
	       int *const ptr_blkno,
	       int *const ptr_recno,
	       int *const ptr_size,
	       char **const ptr_name
)
{
	char    p[16];
	int     i;
	char	const *dds_fmt = NULL;

#ifdef DDS_TRACE
	fprintf(stderr,"rt_line(...)\n");
#endif

	if (cur_line[16] == ':') {
		dds_fmt = dds_old_index_scan_format ;
		*ptr_name = cur_line + 27;
	} else {
		if (cur_line[17] == ':') {
			dds_fmt = dds_index_scan_format ;
			*ptr_name = cur_line + 28;
		} else return 1 ;
	} ;

	if ((i = sscanf(cur_line,
			dds_fmt,
			p,
			ptr_blkno,
			ptr_recno,
			ptr_size)) < 4)
		fprintf(stderr, "Wrong line inside the index file,"
			" scanned %d values\n", i);
#ifdef DDS_TRACE
	fprintf(stderr,"end >> rt_line(...)\n");
#endif
	return 0 ;
}

/* procedure to create the index table */
/********
	*  input: none
	* output: none
	* extern: device, index_fp
	*
	* The archive is read, and the index file is written.
	*
	************************************************************/

int
dds_index(void)
{


#ifdef DDS_TRACE
	fputs("dds_index()\n", stderr);
#endif


	fputs(dds_headline, index_fp);
	if (verbose) fputs(dds_headline, index_fp);
	next_blkno = -1 ;
	cur_rec = 0;
	dds_read_block();
	cur_byte = cur_n;
	if (hash_mode == 1)
		if ((cur_blkno & 0x1f) == 0) {
			write(2, &hash_sign, 1);
		}
	while (cur_n > 0) {
		cur_bs = cur_n >> 9;
		while (cur_rec < cur_bs) {
			int new_rec ;
			new_rec = dds_index_h(
						     cur_block + cur_rec,
						     cur_blkno,
						     cur_rec
				);
			if ( long_name_len > 0 ) {
				int i = 0 ;
				while ( long_name_len > (i<<9) ){
					cur_rec++;
					if ( cur_rec >= cur_bs )
						dds_index_skip();
					memcpy(long_name+((i++)<<9),
						cur_block+cur_rec,
						512);
				}
				long_name_len = 0 ;
			}
			cur_rec = new_rec ;
		}
		dds_index_skip();
	}
	if (cur_n <= 0) {
		perror("dds2tar");
		fprintf(stderr, "dds_index: unexpected end of tar archive \n");
		close(device);
		exit(9);
	}
	return 0;
}
