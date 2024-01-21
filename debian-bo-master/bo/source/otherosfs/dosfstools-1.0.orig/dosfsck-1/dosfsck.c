/* dosfsck.c  -  User interface */

/* Written 1993 by Werner Almesberger */


#include <stdio.h>
#include <unistd.h>
#include <getopt.h>

#include "common.h"
#include "dosfsck.h"
#include "io.h"
#include "boot.h"
#include "fat.h"
#include "file.h"
#include "check.h"


int interactive = 0,list = 0,test = 0,verbose = 0,write_immed = 0;
void *mem_queue = NULL;


static void usage(char *name)
{
    fprintf(stderr,"usage: %s [ -aflrtvVw ] [ -d path -d ... ] "
      "[ -u path -u ... ]\n%15sdevice\n",name,"");
    fprintf(stderr,"  -a       automatically repair the file system\n");
    fprintf(stderr,"  -d path  drop that file\n");
    fprintf(stderr,"  -f       salvage unused chains to files\n");
    fprintf(stderr,"  -l       list path names\n");
    fprintf(stderr,"  -r       interactively repair the file system\n");
    fprintf(stderr,"  -t       test for bad clusters\n");
    fprintf(stderr,"  -u path  try to undelete that (non-directory) file\n");
    fprintf(stderr,"  -v       verbose mode\n");
    fprintf(stderr,"  -V       perform a verification pass\n");
    fprintf(stderr,"  -w       write changes to disk immediately\n");
    exit(2);
}


int main(int argc,char **argv)
{
    DOS_FS fs;
    int rw,salvage_files,verify,c;

    rw = salvage_files = verify = 0;
    while ((c = getopt(argc,argv,"ad:flrtu:vVw")) != EOF)
	switch (c) {
	    case 'a':
		rw = 1;
		break;
	    case 'd':
		file_add(optarg,fdt_drop);
		break;
	    case 'f':
		salvage_files = 1;
		break;
	    case 'l':
		list = 1;
		break;
	    case 'r':
		rw = 1;
		interactive = 1;
		break;
	    case 't':
		test = 1;
		break;
	    case 'u':
		file_add(optarg,fdt_undelete);
		break;
	    case 'v':
		verbose = 1;
		printf("dosfsck 0.1 (May 8 1993)\n");
		break;
	    case 'V':
		verify = 1;
		break;
	    case 'w':
		write_immed = 1;
		break;
	    default:
		usage(argv[0]);
	}
    if ((test || write_immed) && !rw) {
	fprintf(stderr,"-t and -w require -a or -r\n");
	exit(2);
    }
    if (optind != argc-1) usage(argv[0]);
    fs_open(argv[optind],rw);
    read_boot(&fs);
    if (verify) printf("Starting check/repair pass.\n");
    while (read_fat(&fs), scan_root(&fs)) qfree(&mem_queue);
    if (test) fix_bad(&fs);
    if (salvage_files) reclaim_file(&fs);
    else reclaim_free(&fs);
    file_unused();
    qfree(&mem_queue);
    if (verify) {
	printf("Starting verification pass.\n");
	read_fat(&fs);
	scan_root(&fs);
	reclaim_free(&fs);
	qfree(&mem_queue);
    }
    if (fs_changed() && rw)
	if (interactive)
	    rw = get_key("yn","Perform changes ? (y/n)") == 'y';
	else printf("Performing changes.\n");
    return fs_close(rw) ? 1 : 0;
}
