/* checkcd.c: Checks if medium is in drive */
/* Exit-status 1 = No CD. 0 = CD ready. */
/* 16.3.96 T. Niederreiter */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <fcntl.h>
#include <unistd.h>
#include <linux/cdrom.h>



/* Open cdrom device */

int OpenCdRom ( char *dev_name ) {
int cdfd;

      	cdfd = open(dev_name, O_RDONLY);

  	if (cdfd < 0) {
		/*
  		perror("open");
                fprintf(stderr,"Failed to open %s\n",dev_name);
		*/
		/* We failed to open the device, we assume that no
		   CD in the drive */
                exit(1);
	}

  	return cdfd;
}


void main(int argc, char **argv) {
char cddev[1024];
int cd_fd;
struct cdrom_tochdr toc;

	if (argc != 2) {
		printf("Usage: %s <block-CD-device>\n", argv[0]);
		exit(-1);
	} else {
		strcpy(cddev,argv[1]);
	}

	cd_fd = OpenCdRom (cddev);


	/* This seems to be a easy way to find out when the CD is ready */
	/* If the Read-Toc failed, we assume there is not CD in the drive */

	if (ioctl(cd_fd, CDROMREADTOCHDR, &toc)) {
		/* Medium not loaded */
		exit(1);
	} else {
		/* Medium loaded */
		exit(0);
	}

}

