#include "internal.h"
#include <stdio.h>

const char	block_device_usage[] = "block_device mount-point";

static dev_t	device = 0;

int
match_mount(const struct FileInfo * i)
{
	if ( ((i->stat.st_mode & S_IFMT) == S_IFBLK)
	 && (i->stat.st_rdev == device) ) {
		printf("%s\n", i->source);
		exit(0);
	}
	return 0;
}

extern int
block_device_main(struct FileInfo * i, int argc, char * * argv)
{
	struct stat	s;
	if ( stat(argv[1], &s) != 0 )
		return -1;
	device = s.st_dev;
	i->source = "/dev";
	descend(i, match_mount);
	fprintf(
	 stderr
	,"Can't find special file for block device %d, %d.\n"
	,device >> 8 & 0xff
	,device & 0xff);
	return 1;
}
