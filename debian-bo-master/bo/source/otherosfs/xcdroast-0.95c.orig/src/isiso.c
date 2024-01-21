/*
 isiso.c 

 Looks in first block of a block-device and checks if there is an ISO9660-
 filesystem on it. This is done by checking for a string "CD001".
 Exit with status "0" means ISO-FS found, "1" not found.
 
 14.3.96 T.Niederreiter  
*/

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>

char buf[1024];
char devname[80];

main(int argc, char **argv) {
int fd;
int c;

	/* Default device to check */
    	strcpy(devname,"/dev/cdrom");

	if (argc != 1) {
      		strcpy(devname,argv[1]);
	}

    	fd = open(devname, O_RDONLY);

    	if (fd < 0) {
        	perror(devname);
        	exit(1);
    	}	

    	if (lseek(fd, 32768, SEEK_SET) < 0) {
        	perror("lseek");
        	exit(1);
    	}	

    	if (read(fd, buf, sizeof(buf)) != sizeof(buf)) {
        	perror("read");
        	exit(1);
    	}

    	if (strncmp(buf, "\001CD001\001", 8) != 0) {
         	exit(1);
        }
    	else {
		exit(0);
	}
}

