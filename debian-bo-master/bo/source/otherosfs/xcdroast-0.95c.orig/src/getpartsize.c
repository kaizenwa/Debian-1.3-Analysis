/*
  getpartsize.c: Output partition size
  16.1.95 T.Niederreiter
*/

#include <stdio.h>
#include <fcntl.h>
#include <linux/fs.h>

int getsize(char *dev) {
int fd,size;
	fd = open(dev,O_RDONLY);
	if (fd < 0) {
		return(-1);
	}

	if (ioctl(fd,BLKGETSIZE, &size)) {
		perror(dev);
		fprintf(stderr,"BLKGETSIZE failed for %s\n", dev);
		return(-2);
	}
	close(fd);
	return (size/=2048);     /* Return in MB */
}

/* Lets check if we can access this device  
   Should be called for whole disks (e.g. /dev/sda), to avoid
   multiple accesses to non-existent devices later */

int checkdevice(char *dev) {
int fd;

	fd = open(dev,O_RDONLY);
	if (fd >= 0) close(fd);
	return fd;
} 
		
main(int argc, char **argv) {
char dev[20],tmpstr[5];
char newdev[20];
int size,i;
int listall = 0;

 	if (argc == 2) { 
		strcpy(dev,argv[1]); 
 	} else 
	if ((argc == 3) && (strcmp(argv[1],"-a")== 0)) {
		strcpy(dev,argv[2]);
		listall = 1;
	}
	else { 
		exit(0);
	}

	if (listall) {
		if (checkdevice(dev) < 0) exit(0);
		for (i=1; i<16; i++) {
			sprintf(tmpstr,"%d",i);
			strcpy(newdev,dev);
			strcat(newdev,tmpstr);
			size=getsize(newdev);
			if (size >= 0) 
				printf("%-10s:%d\n",newdev,size);
		}
	} else {
		size = getsize(dev);
		printf("%d\n",size);	
	}

	exit(0);
}


