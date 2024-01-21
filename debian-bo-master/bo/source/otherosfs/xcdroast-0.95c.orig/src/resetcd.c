/*
  resetcd.c: 
  29.4.95 T.Niederreiter

  Based on flushb.c 
*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <linux/cdrom.h>

const char *progname;

void usage() {
	fprintf(stderr, "Usage: %s <block-CD-device>\n", progname);
	exit(1);
}	
	
int main(int argc, char **argv) {
int fd;
	
	progname = argv[0];

	if (argc != 2)
		usage();

	fd = open(argv[1], O_RDONLY, 0);

	if (fd < 0) {
		perror("open");
		exit(1);
	}

	if (ioctl(fd, CDROMRESET, 0) < 0) {
/*
	this ioctl has been removed in kernels greater than 2.1.10
	don't display a warning in this case 
		perror("ioctl CDROMRESET");
		exit(1);
*/
	}

	close(fd);
	return 0;
}
