/*
    dds2tar  Digital-Data-Storage Extract Tool

             This tool makes use of the fast seek command of DAT devices.
             Files from a selected file archive can be extracted within
             one minute.

             J"org Weule                     weule@cs.uni-duesseldorf.de
                                             Fon: +49 211 751409

----------------------------------------------------------------------------

                              LICENSE

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published
    by the Free Software Foundation; either version 1, or (at your option)
    any	later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

----------------------------------------------------------------------------

    To anyone in Germany I declare the following:

    !  Ich werde gegen jede Verbreitung dieser Software rechtlich        !
    !  vorgehen, welche die Lizenzbestimmungen nicht einh"alt.           !

       Wie f"ur jede andere Software gilt auch hier:
       Schon mit dem Kopieren oder Benutzen von Teilen dieser Software
       werden die Lizenzbedingungen stillschweigend akzeptiert. Lesen Sie
       also die Lizenzbedingungen sorgf"altig! Unkenntnis sch"utzt nicht
       vor Strafe.

*/

/*-------------------------------------------------------------------------*/

#include <stdlib.h>		/* getenv() */
#include <stdio.h>
#include <string.h>		/* strcmp() strlen() strstr() */
#include <sys/types.h>		/* open() */
#include <fcntl.h>		/* open() */
#include <unistd.h>		/* open() */
#include <time.h>		/* ctime() */

#include "dds2tar.h"
#include "dds_tape.h"
#include "zf-cre-open.h"

/*-------------------------------------------------------------------------*/

static const char help_text_dds2index[] =
"This is dds2index, a tool for fast tape access.\n"
"Choose one of the following\n"
"-t,--table-of-contents F  get file location from file F\n"
"-z,--compress           filter location file through gzip\n"
"--z,--no-compress       don't filter location file through gzip\n"
"-f,--device F           read the archive from device F\n"
"-b,--block-size #       set block size\n"
"-q,--quick              don't extract parent directories from tape\n"
"-v,--verbose            verbose mode\n"
"--hash-mode             hash mode\n"
"--force                 force nochk\n"
"--help                  get help\n"
"-V,--version            print version number\n"
"\n"
"Environment:\n"
"TAPE                    specifies the tape device\n"
"DDS2TAR=[--compress] [-z] [-s #] [-b #]\n"
"                        set some defaults\n"
       ;

static const char help_text_dds2tar[] =
"This is dds2tar, a tool for fast tape access.\n"
"Usage: dds2tar [options... ] -t index [pathname ...]  | tar -f - ...\n"
"\n"
"dds2tar is a tool to read some tar-records from tape and write them\n"
"as a tar archive to stdout. You have to use tar to extract the\n"
"selected files. The index can either be created by dds2tar or tar.\n"
"\n"
"Choose one of the following\n"
"-t,--table-of-contents F  get file location from file F\n"
"-z,--compress           filter location file through gzip\n"
"--z,--no-compress       don't filter location file through gzip\n"
"-f,--device F           read the archive from device F\n"
"-o,--output F           write output to archive file\n"
"-b,--block-size #       set block size\n"
"-s,--first_block #      set number of the first block\n"
"--body                  print only the contents of the first file an quit\n"
"-v,--verbose            verbose mode\n"
"--help                  get help\n"
"-V,--version            print version number\n"
"\n"
"Environment:\n"
"TAPE                    specifies the tape device\n"
"DDS2TAR=[--compress] [-z] [-s #] [-b #]\n"
"                        set some defaults\n"
       ;

static const char help_text_mt_dds[] =
"This is mt-dds, a tool to control the dds-device.\n"
"Usage: mt-dds [-f device] <action>\n"
"\n"
"Choose one or none of the following actions:\n"
#ifdef HPDAT
"comp-on      set compression mode on\n"
"comp-off     set compression mode off\n"
"comp-query   query compression mode\n"
"comp-log     print compression log page\n"
#endif
"-b           set the buffer size\n"
"blksize      print the block size of the archive with format\n"
"tell         print current location\n"
"label        print the label of the current archive\n"
"             if it begins at the current position\n"
"filename     print the filename of the current record\n"
"             if a header record is found at the current position\n"
"ts           print the timestamp of the current archive\n"
"date         print the date of the current archive\n"
"             if it begins at the current position\n"
"date <timestamp> convert the stamp to a string\n"
       ;

static const char help_text_dds_dd[] =
"This is dds-dd, a tool to read the dds-device.\n"
"Usage: dds-dd` [-f device] | tar -f - -[x|t] ... \n";

/*-------------------------------------------------------------------------*/

static const char version[] =
"Version " VERSION " -- last change 1994 Dez 22." ;

/*-------------------------------------------------------------------------*/

/* This is the source for dds2tar, dds2index and mt-dds. The program will
   use the variable pg to decide witch version is running. The default
   can be set in the Makefile and is overwritten by the value of argv[0].
   If you give --dds2tar, --dds2index or --mt-dds as an option, the
   program will run instead of these.
*/

#define DDS2TAR name_dds2tar
static char const name_dds2tar[] = "dds2tar";

#define DDS2INDEX name_dds2index
static char const name_dds2index[] = "dds2index";

#define MTDDS name_mtdds
static char const name_mtdds[] = "mt-dds";

#define DDS_DD name_dds_dd
static char const name_dds_dd[] = "dds-dd";


#if ( PROGRAM == DDS2TAR )
char const *pg = name_dds2tar;
char const *help_text = help_text_dds2tar;

#elif ( PROGRAM == DDS2INDEX )
char const *pg = name_dds2index;
char const *help_text = help_text_dds2index;

#elif ( PROGRAM == DDS2TAR )
char const *pg = name_mtdds;
char const *help_text = help_text_mt_dds;

#elif ( PROGRAM == DDS_DD )
char const *pg = name_dds_dd;
char const *help_text = help_text_dds_dd;

#else
char const *pg = name_dds2tar;
char   *help_text = help_text_dds2tar;

#endif

#define LOCATION 1
#define EXTRACT 2

/*-------------------------------------------------------------------------*/

char const dds_old_headline[] =
"magic record blk:     size name\n";

char const dds_old_index_format[] =
"%6s%7d%3d:%9d %s\n";

/* magic blkno recno: size name */

char const dds_old_index_scan_format[] =
"%6c%7d%3d:%9d\n";

char const dds_headline[] =
"magic  record blk:     size name\n";

char const dds_index_format[] =
"%6s%7d%4d:%9d %s\n";

/* magic blkno recno: size name */

char const dds_index_scan_format[] =
"%6c%7d%4d:%9d\n";

/* magic blkno recno: size name */

char const dds_loctext[] =
"first block number is %d\n"
"block size is %d\n" "block length is %d\n";

char const dds_locline1[] =
"loc             number of the first block is %d\n";

char const dds_locline2[] =
"loc             block length is %d bytes = %d * 512 bytes\n";

/* to handle long link and long file names */
int long_name_len = 0 ;
char long_name[MAXPATHLEN<<2] = { '\0' };

int
rt_loc_line(void)
{
	int     i;

	if ((i = strncmp(cur_line, dds_locline1, 45)) == 0) {
		tar_fb = atoi(cur_line + 45);
	} else if ((i = strncmp(cur_line, "first block number is", 21)) == 0) {
		tar_fb = atoi(cur_line + 21);
	} else if ((i = strncmp(cur_line, dds_locline2, 31)) == 0) {
		tar_n = atoi(cur_line + 31);
		tar_bs = tar_n >> 9;
	} else if ((i = strncmp(cur_line, "block size is", 13)) == 0) {
		tar_bs = atoi(cur_line + 13);
		tar_n = tar_bs << 9;
	} else
		return 1;
	if ( buf_n < tar_n ) {
		cur_block = realloc ( cur_block , buf_n = tar_n );
		if ( cur_block == NULL ) {
			fprintf(stderr, "%s: No memory available.\n", pg);
			exit(2);
		}
	}
	return 0;
}


/*-------------------------------------------------------------------------*/

int     verbose = 0;
int     hash_mode = 0;
int     list_only = 0;		/* print only the matched names */
int	quick_mode = 0;		/* don't extract parent directories also */
int     device;			/* file number of the device */
int	write_body = 0 ;	/* write only the body of the first file */
int	get_label = 0 ;		/* get the label of the archive */
int	get_filename = 0 ;	/* get the filename of the current record */
int	get_blocksize = 0 ;	/* get the blocksize of the current record */
int	get_timestamp = 0 ;	/* get the timestamp of the archive */
int	get_date = 0 ;		/* get the date of the archive */
int	get_fileno = 0 ;	/* get the number of the current */
int	force_nochk = 0 ;	/* force tape read, no check of headers */

/* file io */
FILE   *index_fp = NULL;

/* archive location as set by arguments or index file */
int     tar_fb = 0;		/* first block of the archive */
int     tar_bs = 20;		/* block size in records of 512 bytes */
int     tar_n = 10240;		/* block length in bytes */

/* archive buffer and location as set by tape access */
tar_record *cur_block;
int     cur_blkno = -1;
int	next_blkno = -1 ;
int     cur_bs;
int     cur_n;
int	buf_n = ST_BUFFER_BLOCKS << 10 ;

/* line buffer to scan the index file */
char   *cur_line;

/*-------------------------------------------------------------------------*/

/********
	*  input: name of the device ( e.g. /dev/nst0, /dev/nrmt0 or NULL )
	*         open mode of the device ( e.g. O_RDONLY or O_WRONLY )
	*         verbose flag to indicate the logging mode (integer)
	* output: channel number ( integer ) for use with 'read()' or 'write()'
        *
        * side effects: none
	*
	**********************************************************************/

static int
open_device(char const *pathname, int const open_mode)
{
	int     fd;
	int     i = 1;
	char   const *p = "next try to open the tape %s\n";

	do {
		if ((fd = open(pathname, open_mode)) >= 0)
			break;
		sleep(2);
		if (verbose)
			fprintf(stderr, p, pathname);
	} while ((++i) <= 15);
	if (fd < 0) {
		perror("tape busy?");
		exit(1);
	}
	return fd;
}

/*-------------------------------------------------------------------------*/
int
strprefix(char**s,char*p,char*q){
	while ( p != 0 ) {
		char *t = *s ; while (( *p != 0 ) && ( *p == *t )) p++ , t++ ;
		if ( *p == 0 ) { *s = t ; return 1 ; }
		p = q ; q = NULL ;
	} return 0 ;
}
/*-------------------------------------------------------------------------*/

/*
 * dds2tar
 */
int
main(int argc, char const *const *argv)
{

#ifndef DEVICE
#define DEVICE "/dev/rmt0"
#endif

	int     n = 1;
	int     l = strlen(argv[0]);
	int     compressed_mode = T_MODE;
	char   *p;
	int     child_proc = -1;/* child not running */
	char const *index_file = NULL;
	int     print_location = 0;
	int     print_blksize = 0;
	int     mt_action = DDSCM_NULL;	/* turn compression on/off */
	int     mode = 0;	/* mode of dds2tar */
	char const *const *pattern_list = NULL;
	char const *device_name = DEVICE;

	if ((p = getenv("TAPE")) != NULL)
		device_name = p;

	if (!strcmp(argv[0] + l - 7, "dds2tar")) {
		pg = DDS2TAR;
		help_text = help_text_dds2tar;
	}
	if (!strcmp(argv[0] + l - 9, "dds2index")) {
		pg = DDS2INDEX;
		help_text = help_text_dds2index;
	}
	if (!strcmp(argv[0] + l - 6, "mt-dds")) {
		pg = MTDDS;
		help_text = help_text_mt_dds;
	}
	if (!strcmp(argv[0] + l - 6, "dds-dd")) {
		pg = DDS_DD;
		help_text = help_text_dds_dd;
	}

	/*
	 * Scanning the environment.
	 */
	p = getenv("DDS2TAR");
	while (p != NULL) {
		int count = 0 ;
		if (*p == ' ') p++;
		if ( *p == '\0' ) p = NULL ;
		else if (strprefix(&p, "-z","--compress")) {
			compressed_mode = C_MODE;
		} else if (strprefix(&p, "--first-block ","-s")) {
			sscanf(p, "%d%n", &tar_fb, &count);
			p += count;
		} else if (strprefix(&p, "--block-size ","-b ")){
			sscanf(p, "%d%n", &tar_bs, &count);
			tar_n = tar_bs * 512;
			buf_n = tar_n ;
			p += count;
		} else
			p = NULL;
	}

	/*
	 * scan arguments
	 */
	while (n < argc) {

		/*
		 * Macros to test arguments ...
		 */

#define T1(s)        if((!strcmp(argv[n],(s))))
#define T(s)    else if((!strcmp(argv[n],(s))))
#define TT(s,t) else if((!strcmp(argv[n],(s)))||(!strcmp(argv[n],(t))))
#define ELSE    else
#define ELSEIF(s) else if(s)

		/*
		 * Now scanning the arguments ...
		 */

		T1("-0") {
			fprintf(stdout, "dds2tar: %s\n", argv[0]);
			exit(0);
		}

		/*
		 * misc
		 */
		T("--help") {
			fprintf(stdout, help_text);
			exit(0);
		}
		TT("--version", "-V") {
			fprintf(stdout, "%s: %s\n", pg, version);
			exit(0);
		}

		/*
		 * Some more options ...
		 */
		TT("--verbose", "-v") verbose = 1;
		T("--hash-mode") hash_mode = 1;
		T("--force") force_nochk = 1;

		/*
		 * device name
		 */
		T("-f") device_name = argv[++n];

		/*
		 * index file
		 */
		TT("-z", "--compress") compressed_mode = C_MODE;
		TT("--z", "--no-compress") compressed_mode = T_MODE;
		TT("-t", "--table-of-contents") {
			index_file = argv[++n];
			/*
			 * The format of the index is checked by the program.
			 */
		}

		/*
		 * Location of the file
		 */
		TT("-s", "--first-block") {
			tar_fb = atoi(argv[++n]);
		}
		TT("-b", "--block-size") {
			tar_bs = atoi(argv[++n]);
			tar_n = tar_bs * 512;
			buf_n = tar_n ;
		}
		TT("-m", "--buffer-size") {
			fprintf(stderr,"buffer size is %d records, %d bytes\n",
				buf_n>>9,buf_n);
			exit(0);
		}

		ELSEIF(pg == DDS2TAR) {
			/*
			 * Mode of the program.
			 *
			 * If you install dds2tar as mt-dds, the program will
			 * act as mt-dds. Since for some operations you need
			 * root permissions on mt-dds, it would not be a good
			 * idea to let the user switch to dds2index or dds2tar
			 * in this case.
			 */

			T1("--dds2index") {
				pg = DDS2INDEX;

				help_text = help_text_dds2index;
			}
			T("--mt-dds") {
				pg = MTDDS;
				help_text = help_text_mt_dds;
			}
			T("--dds-dd") {
				pg = DDS_DD;
				help_text = help_text_dds_dd;
			}
			/*
			 * Mode of extraction ...
			 *
			 * The location list an experimental mode.
			 */
#ifdef EXP_STUFF
			T("--location-list") {
				pattern_list = argv + n + 1;
				mode = LOCATION;
				break;
			}
#endif
			/*
			 * Select a file for the output.
			 *
			 * Since mt-dds may be run as root, we write on screen
			 * in this case. Writing to a file is supported for
			 * dds2tar.
			 */
			TT("-o", "--output") {
				reopen(1, argv[++n], O_WRONLY | O_CREAT, 0660);
			}

			/*
			 * Do not read anything from tape.
			 */
			TT("-l", "--list") {
				list_only = 1;
				if (verbose)
					fputs("--list\n", stderr);
			}
			/*
			 * Write only the body of the file.
			 */
			T("--body"){
				write_body = 1 ;
			}

			/*
			 * Don't extract the parent directories from the tape.
			 */
			TT("-q","--quick"){
				quick_mode = 1 ;
			}

			/*
			 * Pipe the output to tar -x.
			 *
			 * This is for testing only.
			 */
			T("--test") {
				static char const *const a[5] =
				{
					"/bin/tar", "tfb", "-", "1", NULL
				};

				child_proc = creopen(1, 0, a[0], a);
			}
			T("--test-verbose") {
				static char const *const a[5] =
				{
					"/bin/tar", "tfbv", "-", "1", NULL
				};

				child_proc = creopen(1, 0, a[0], a);
			}
			T("--extract") {
				static char const *const a[5] =
				{
					"/bin/tar", "xfb", "-", "1", NULL
				};

				child_proc = creopen(1, 0, a[0], a);
			}
			ELSE {
				pattern_list = argv + n;
				if (verbose)
					fprintf(stderr,
						"first pattern is '%s'\n",
						*pattern_list
						);
				mode = EXTRACT;
				break;
			}
		}

		ELSEIF(pg == MTDDS) {
			/*
			 * Tape actions ...
			 */
			T1("tell") print_location = 1;
			T("label") get_label = 1 ;
			T("filename") get_filename = 1 ;
			T("bs") get_blocksize = 1 ;
			T("date") {
				get_date = 1 ;
				if ( argc > 2 ) {
					int j = atoi(argv[2]);
					fputs(ctime((time_t*)&j),stdout);
					exit(0);
				}
			}
			T("ts") get_timestamp = 1 ;
			T("blksize") print_blksize = 1 ;
#ifdef HPDAT
#define R(c) if ( 0 != geteuid() ) { fprintf(stderr,\
"You have to be root to do this : %s\n",c);exit(1);}
			T("comp-on") {
				R("comp-on");
				mt_action = DDSCM_ON;
			}
			T("comp-off") {
				R("comp-off");
				mt_action = DDSCM_OFF;
			}
			T("comp-query") {
				R("comp-query");
				mt_action = DDSCM_QUERY;
			}
			T("comp-log") {
				R("comp-log");
				mt_action = DDSCM_LOG;
			}
			T("load") {
				R("comp-load");
				mt_action = DDSCM_LOAD;
			}
			T("unload") {
				R("comp-unload");
				mt_action = DDSCM_UNLOAD;
			}
#endif
#ifdef EXP_STUFF
			/*
		 * tar-dds will handle the arguments in a very different way.
		 */
		}
		ELSEIF(pg == TAR_DDS) {

			break;
#endif
		}
		/*
		 * Next argument ...
		 */
		n++;
	}

	cur_block = malloc(buf_n);
	cur_line = malloc(MAXPATHLEN<<2);
	if (cur_block == NULL || cur_line == NULL) {
		fprintf(stderr, "%s: No memory available.\n", pg);
		exit(2);
	}
	/*
	 * Switch the program mode ... dds2tar, dds2index or mt-dds ...
	 */
	/*---------------------MT-DDS--------------------------------*/
	if (pg == MTDDS) {
#ifdef HPDAT
		/*
		 * Is setting of compression mode or log page selected ?
		 *
		 * I think you have to run this as root.
		 */
		if (mt_action != DDSCM_NULL) {
			device = open_device(device_name, O_WRONLY);
			set_compression_mode(device, mt_action);
		} else
#endif
		/*
		 * Print the current position (tree lines).
		 */
		{
			device = open_device(device_name, O_RDONLY);
			tar_fb = dds_getpos(device);
			dds_read_next_block();
			/* dds_seek(device, tar_fb); */
			dds_bsr();
			tar_bs = cur_n >> 9;
			if (( get_label || get_timestamp ||
				get_date || get_filename ) &&
				( dds_is_tar_header_record(cur_block) != 0 )) {
				if ( cur_block->hdr.linkflag == 'V' ) {
					int i , j = 0 ;
					char*p=cur_block->hdr.mtime ;
					for ( i = 0 ; i < 12 ; i++ ) {
						if ((p[i]>='0')&&(p[i]<='7')) {
							j <<= 3 ;
							j += p[i] - '0' ;
						}
					}
					if ( get_label )
						puts(cur_block->hdr.name);
					else if ( get_timestamp ){
						printf("%d\n",j);
					} else /* get_date */ {
						fputs(ctime((time_t*)&j),stdout);
					}
				} else
				if ( get_filename ) puts(cur_block->hdr.name);
			} else
			if (print_location != 0) {
				printf(dds_loctext, tar_fb, tar_bs, cur_n);
			} else
			if ( print_blksize != 0 ) {
				printf("%d\n",tar_bs);
			} else {
				/*
				 * Print the current position (one line).
				 */
				printf(" -s %d -b %d \n", tar_fb, tar_bs);
			}
		}
		close(device);
		/*---------------------DDS2TAR-------------------------------*/
	} else if (pg == DDS2TAR) {

		if (pattern_list == NULL)
			return 0;
		if (list_only != 1)
			device = open_device(device_name, O_RDONLY);
		index_fp = zfopen(index_file, compressed_mode, "r");
		switch (mode) {
#ifdef EXP_STUFF
		case LOCATION:
			extract_loc(pattern_list);
			break;
#endif
		case EXTRACT:
			dds_cmp(pattern_list);
			break;
		default:
			fprintf(stderr, "nothing to do ? Try --help\n");
		}
		if (list_only != 1)
			close(device);
		if (child_proc != -1)
			cclose(1);
		/*---------------------DDS2INDEX-----------------------------*/
	} else if (pg == DDS2INDEX) {
		device = open_device(device_name, O_RDONLY);
		index_fp = zfopen(index_file, compressed_mode, "w");
		dds_index();
		close(device);
		/*-----------------------------------------------------------*/
	} else if ( pg == DDS_DD ) {
		device = open_device(device_name, O_RDONLY);
		if ( verbose ) fprintf(stderr,"dds-dd ...\n");
		while ( dds_read_next_block() , cur_n )
			fwrite(cur_block,1,cur_n,stdout);
	} else {
		fprintf(stderr, "no program mode \n");
		/*-----------------------------------------------------------*/
	}

	return 0;
}
