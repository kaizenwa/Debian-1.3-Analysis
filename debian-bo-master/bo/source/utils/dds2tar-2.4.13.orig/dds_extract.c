
/*
 * This file is part of dds2tar.
 * Copyright by J"org Weule
 */

#include <stdlib.h>
#include <stdio.h>
#include <sys/mtio.h>
#include <string.h>
#include <unistd.h>
#include <fnmatch.h>

#include "dds2tar.h"
#include "dds_tape.h"

/*
 * Compare two strings
 *
 */
int dds_strcmp(const char*n1,const char*n2){
	int i = 0 ;
	int j = 0 ;
	while ( 1 ){
		while ( n1[i] == '\\' ) i++ ;
		while ( n2[j] == '\\' ) j++ ;
		if ( i > 90 || j > 90 || n1[i] == '\0' || n2[j] == '\0' )
			return 0 ;
		if  ( n1[i] != n2[j] ) return 1 ;
		i++ ;
		j++ ;
	}
	return 0 ;
}

/*
 * To count the number of blocks written, we use the variable nblocks.
 */
static int nblocks = 0;

/*
 * Fill the buffer 'cur_block' the block of the given number.
 * first we check the block number of the copy that is hold inside the
 * buffer cur_block.
 */
static int
set_cur_block(int const sb)
{
	int     n;

#ifdef DDS_TRACE
	fprintf(stderr,"%d\n",__LINE__);
	fprintf(stderr, "set_cur_block(%d)\n", sb);
#endif

	if (sb != cur_blkno) {
		/*
		 * We have to read the block.
		 */
		next_blkno = dds_getpos(device);
		/*
		 * next_blkno stores now the number of the next block of the
		 * tape.
		 */
		n = sb - next_blkno;
		/*
		 * In some cases reading is faster then seeking.
		 */
		if ((n > 0) && (n < DONT_SKIP)) {
			do {
				dds_read_block();
			} while ((--n) > 0);
		}
		/*
		 * Now we should be at the position.
		 */
		n = sb - next_blkno;
		if (n != 0) {
			dds_seek(device, sb);
			next_blkno = sb;
		}
		/*
		 * Now we read the block. cur_n == 0 indicates a filemark.
		 */
		dds_read_block();
	}
	return 0;
}

/*
 * procedure to extract the files from the specified area (sb:sr-eb:er).
 * If area is of the form (sb:sr-0:0), the file at position (sb,sr) is
 * extracted. The length of the file is read from the tar header record.
 *
 * If list_only is set to 1, only the name of the extraction is printed.
 */
static int
extract(char const *const name, int const sb, int const sr)
{

	int     cur_rec, n, size;
	char   *p;

#ifdef DDS_TRACE
	fprintf(stderr,"%d\n",__LINE__);
	fprintf(stderr, "extract(%s,%d,%d)\n", name, sb, sr);
#endif

	/*
	 * Print only the name.
	 */
	if (list_only == 1) {
		printf("%7d%3d: %s\n", sb, sr, name);
		return 0;
	}
	/*
	 * Print the filename and the location for verbose output.
	 */
	if (verbose != 0) {
		fprintf(stderr,
			"dds2tar at rec %d: %7d%3d: %s\n",
			nblocks, sb, sr, name);
	}
	/*
	 * Check the buffer for the right tape block.
	 */
	set_cur_block(sb);

	cur_rec = sr;
	/*
	 * Check the header block.
	 * The Name field should contain the given name.
	 * The Program will stop if its not.
	 *
	 * Note that for links we can only compare the first
	 * characters, when the index contains somethins like
	 * 'linkname -> filename' (soft link) or
	 * 'linkname link to filename' (hard link)
	 * doesn't match 'linkname'.
	 */
	p = (char*) (cur_block + cur_rec) ;
	if ( dds_is_tar_header_record((tar_record*)p) == 0 ) {
		fprintf(stderr,
			" dds2tar: FATAL ERROR\n"
			" dds2tar: header expected, not found\n"
		);
		if ( force_nochk == 0 ) exit(5);
	}
	if (name != NULL)
	if ( ( (dds_strcmp(name, p)!=0 )
		&& (((tar_record*)p)->hdr.linkflag!='2')
		&& (((tar_record*)p)->hdr.linkflag!='1')
		) || ( strstr(name, p)!=name ) ) {
		fprintf(stderr,
			"dds2tar: FATAL ERROR\n"
			"dds2tar: looked for %s at %d %d\n"
			"dds2tar: not found %s at %d %d\n"
			"dds2tar: Link flag: %c\n"
			"dds2tar: Is it the right tape?\n",
			name, sb , sr ,
			name, dds_getpos(device) - 1, cur_rec,
			((tar_record*)p)->hdr.linkflag);
		if ( force_nochk == 0 ) exit(5);
	}
	/*
	 * First calculate the number of records of the object.
	 */
	size = 0;
	n = 1;

	p = cur_block[cur_rec].hdr.size;
	if (*p != '\0') {
		sscanf(p, "%o", &size);
		n = size;
		n += 1023;
		n >>= 9;
	}
	/*
	 * Now write the records to stdout.
	 */
	if (verbose) {
		fprintf(stderr,
			"dd2tar: %s needs %d blocks\n", name, n);
	}
	if ( write_body == 1 ) {
		cur_rec++;
		while ((size > 0) && (cur_n > 0)) {
			if (cur_rec >= cur_bs) {
				dds_read_block();
				cur_rec = 0;
			}
			write(1, cur_block + cur_rec, (size>=512)? 512:size );
			size -= 512 ;
			cur_rec++;
		}
		exit(0);
	}
	while ((n > 0) && (cur_n > 0)) {
		if (cur_rec >= cur_bs) {
			dds_read_block();
			cur_rec = 0;
		}
		write(1, cur_block + cur_rec, 512);
		nblocks++;
		n--;
		cur_rec++;
	}
	return 0;
}

#ifdef EXP_STUFF
int
extract_loc(char const *const *loc_list)
{
	int cur_rec ;

	while (*loc_list != NULL) {
		int     eb, er, sb, sr;

		sscanf(*loc_list, "%u:%u-%u:%u", &sb, &sr, &eb, &er);
		set_cur_block(sb);
		cur_rec = sr ;
		while (cur_n > 0) {
			int     i;

			if ((cur_n & 0x1ff) != 0) {
				fprintf(stderr,
					"tape record size (%d) is not a"
					" multiple of tar records\n"
					, cur_n
					);
				close(device);
				exit(6);
			}
			i = cur_n >> 9;
			if (cur_blkno == eb)
				i = er;
			while (cur_rec < i) {
				write(1, cur_block + cur_rec++, 512);
				nblocks++;
			}
			/*
			 * if eb==-1 extract until eof
			 */
			if ((cur_rec == er && cur_blkno == eb))
				break;
			i = cur_blkno + 1;
			cur_rec = 0;
			if ((cur_rec == er && cur_blkno == eb))
				break;
			dds_read_block();
		}
		loc_list++;
	}
	return 0;
}

#endif

/*
 * Now we are scanning the table of contents (index file) and match the
 * pathnames there with the given pattern. If a pattern matches, we
 * extract the parent directories (dir_extract()) and the file.
 */
int
dds_cmp(char const *const *const pattern_list)
{
	int i ;
	char *fgets_return_value ;
	char const *const *p;

	/*
	 * To scan the line of the table of contents (index file)
	 * we need some variables.
	 */
	char   *name = NULL;
	int     blkno, recno, size;

	/*
	 * List of directories entries.
	 */
	struct dir_list {
		char    n[128-2*sizeof(int)];	/* name of the dir */
		int     b, r;	/* block address */
	}
	       *dl;
	int     de = 0;		/* first empty list entry */

	/*
	 * Format of the table of contents.
	 *      dds2index --> tar_index_file == 0
	 *      tar -Rvt  --> tar_index_file == 1
	 */
	int     tar_index_file = 1;

	/*
	 * Bug fix for tar. First record number found inside the
	 * table of contents (index file).
	 */
	int     tar_first_recno = -1;

#ifdef DDS_TRACE
	fprintf(stderr,"%d\n",__LINE__);
	fprintf(stderr,"dds_cmp(%s ...)\n",*pattern_list);
#endif


	/*
	 * First we need some memory.
	 */
	dl = malloc(sizeof (*dl) * 64);
	if (dl == NULL) {
		close(device);
		fprintf(stderr, "dds2tar: no address space available\n");
		exit(7);
	}
	memset(cur_line, 0, 1024);

	/*
	 * Scan the table of conten|s (index file) until eof.
	 */
#ifdef DDS_TRACE
	fprintf(stderr,"%d\n",__LINE__);
#endif
	while (!feof(index_fp)) {
		fgets_return_value = fgets(cur_line, MAXPATHLEN<<2, index_fp);
		if ( fgets_return_value == NULL ) {
			if ( feof(index_fp) ) {
				break ;
			} else {
				perror("dds2tar");
				exit(1);
			}
		}
#ifdef DDS_TRACE
		fprintf(stderr,"%d\n",__LINE__);
		fputs(cur_line, stderr);
#endif

		/*
		 * Check for comment and empty lines.
		 */
		if ((*cur_line == '#') ||
		    (*cur_line == ' ') ||
		    (*cur_line == '\0'))
			continue;

		/*
		 * Check the line for location information.
		 */
#ifdef DDS_TRACE
	fprintf(stderr,"%d\n",__LINE__);
#endif
		if (0 == rt_loc_line())
			continue;

		/*
		 * Check for the first line of the dds2index.
		 */
		if ((0 == strcmp(cur_line, dds_headline)) 
		|| (0 == strcmp(cur_line, dds_old_headline))) {
			tar_index_file = 0;
			tar_n = buf_n ;
			tar_bs = buf_n >> 9 ;
			continue;
		}
#ifdef DDS_TRACE
		fprintf(stderr,"%d\n",__LINE__);
#endif

		/*
		 * dds2index indicates eof with the string '-end-'.
		 * This line has to be processed in the normal way.
		 * We can stop now processing.
		 */
		if ((*cur_line == '-') &&
		    (strncmp(cur_line, "-end-", 5) == 0)) {
#ifdef DDS_TRACE
			fprintf(stderr,"%d\n",__LINE__);
#endif
			break;
		}

		/*
		 * Scan the line of the index.
		 * Note: The index file of dds2index contains the magic string
		 *   of the tar header, witch depends on the used tar version.
		 */
		if (tar_index_file == 0) {
			rt_line(&blkno, &recno, &size, &name);
		} else {
			/*
			 * check for record line
			 */
			if (0 != strncmp(cur_line, "rec", 3))
				continue;

			recno = atoi(cur_line + 4);
			/*
		 	 * Now we are fixing a bug of gnu tar ...
			 * The first number should be zero, othewise we
			 * correct all the numbers.
			 * If tar_first_recno is .lt. zero, no recno is read
			 * up to now.
			 */
			if (tar_first_recno < 0)
				tar_first_recno = recno;
			recno -= tar_first_recno;
			/*
			 * Calculate the block number of the record.
			 */
			blkno = recno / tar_bs;
			recno -= blkno * tar_bs;
			blkno += tar_fb;
			if (name == NULL) {	/* calculate only once */
				if ( strlen(cur_line) >= 66 ) {
					name = cur_line + 65;
					while ( ( name[ 0] != '\0' )
					 && (	   ( name[-1] != ' '  )
						|| ( name[-6] != ' '  )
						|| ( name[-9] != ':'  )
					 )
					) name++ ;
					if ( name[-9] != ':' ) {
						name = cur_line + 16;
					}
				} else {
					name = cur_line + 16;
				}
			}
			while ( ( name[ 0] != '\0' )
			 &&     ( name != ( cur_line + 16 ) )
			 && (	   ( name[-1] != ' '  )
				|| ( name[-6] != ' '  )
				|| ( name[-9] != ':'  )
			 )
			) name++ ;
		}
#ifdef DDS_TRACE
		fprintf(stderr,"%d\n",__LINE__);
#endif
		i = strlen(name) -1 ;
		if (name[i] == '\n') name[i] = '\0', i-- ;
		/*
		 * We leave the list of directories empty on quick mode.
		 */
		if (( name[i] == '/' )&&( quick_mode == 0 )) {
			struct dir_list *dp;
			for (i = 0 , dp = dl ; i < de; i++ , dp ++ ) {
				if (strstr(name, dp->n) != name)
					break;
			}
			strcpy(dp->n, name);
			dp->b = blkno;
			dp->r = recno;
			de = i + 1 ;
		}
		/*
		 * Now we try to match one pattern with the name.
		 */
#ifdef DDS_TRACE
		fprintf(stderr,"%d\n",__LINE__);
		fprintf(stderr,"scanning pattern list for '%s'\n",name);
#endif
		p = pattern_list;
		while (*p != NULL) {
			static int ll_blkno = -1 ;
			static int ll_recno = -1 ;
			static int ln_blkno = -1 ;
			static int ln_recno = -1 ;
			static const char*ll = "././@LongLink" ;
#ifdef DDS_TRACE
			fprintf(stderr," p = '%p' , *p = %p \n",p,*p);
#endif
			/*
			 * Thanks Andreas Bagge for this trick.
			 * I use the fnmatch function from the
			 * source of gnu tar.
			 */
#ifdef DDS_TRACE
			fprintf(stderr,"fnmatch for '%s'\n",*p);
#endif
			if (0 == fnmatch(*p, name, FNM_LEADING_DIR)) {
				struct dir_list *dp;
				for (i = 0, dp = dl; i < de; i++, dp++) {
					char   *p = strstr(name, dp->n);
					if (p == name) {
						extract(dp->n, dp->b, dp->r);
					} else break ;
				}
				de = 0;
				if ( ln_blkno >= 0 ) extract(
						"././@LongLink",
						ln_blkno,
						ln_recno);
				if ( ll_blkno >= 0 ) extract(
						"././@LongLink",
						ll_blkno,
						ll_recno);
				extract(name, blkno, recno);
				break;
			}
			if ( ll==index(ll,name) ){
				if ( ln_blkno < 0 )
				ln_blkno = blkno , ln_recno = recno ;
				else
				ll_blkno = blkno , ll_recno = recno ;
			} else {
				ln_blkno = -1 ;
				ln_recno = -1 ;
				ll_blkno = -1 ;
				ll_recno = -1 ;
			}
			p++;
		}
#ifdef DDS_TRACE
		fprintf(stderr,"end of scanning pattern list\n");
#endif
	}
	/*
	 * Write an empty record to the end of the archive.
	 */
	memset(cur_block, 0, buf_n );
	write(1, cur_block, 512);
	nblocks++;
	if (verbose)
		fprintf(stderr, "dds2tar: %d blocks written\n", nblocks);
#ifdef DDS_TRACE
	fprintf(stderr,"%d\n",__LINE__);
	fprintf(stderr,"return of dds_cmp(...)");
#endif
	return 0;
}
