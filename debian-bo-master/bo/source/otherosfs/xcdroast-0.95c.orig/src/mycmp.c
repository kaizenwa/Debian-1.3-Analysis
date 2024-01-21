/*
	Compares 2 files with blocksize 2048
	17.1.96 T.Niederreiter
	Switches to read-ahead zero before reaching last blocks.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/fs.h>

#define BLOCKSIZE 2048

int fd1 = 0;
int fd2 = 0;
char name1[1024];
char name2[1024];
int maxrecords = 0;
int oldpercent = 0;
int blocksread = 0;
long ra;

extern int errno;

/* 
	Calculates how many percent of file have been already read.
	Outputs only if we got a new interger percent-value.
*/

void outpercent(int blocksread) {
int newpercent;

	newpercent = (blocksread * 100) / maxrecords;

	if (newpercent != oldpercent) {
		printf("%d\n",newpercent);
		fflush(stdout);
		oldpercent = newpercent;
	}
}

/* Set read-ahead to zero */

void disablereadahead() {

        if (ioctl(fd1, BLKRASET, 0)) {
            perror("BLKRASET");
            exit(1);
        }
}

void startcompare() {
unsigned char *buf1;
unsigned char *buf2;
int nread1,nread2;

	buf1 = (unsigned char *)malloc(BLOCKSIZE);
	buf2 = (unsigned char *)malloc(BLOCKSIZE);

	while (1) {
		if (maxrecords >= 0 && blocksread >= maxrecords)
			break;
		
		nread1 = read (fd1, buf1, BLOCKSIZE);
		nread2 = read (fd2, buf2, BLOCKSIZE);

		if (nread1 < 0) {
			perror("read");
			fprintf(stderr,"Read-error on %s\n",name1);
			exit(1);
		}
		if (nread2 < 0) {
			perror("read");
			fprintf(stderr,"Read-error on %s\n",name2);
			exit(1);
		}

		if (nread1 != nread2) {
			printf("Files differ on  block %d\n",blocksread);
			fflush(stdout);
			exit(0);
		}	
		if (nread1 == 0)
			break;        /* EOF */


		blocksread++;	

		if (memcmp(buf1,buf2,BLOCKSIZE) != 0) {
			printf("Files differ on block %d\n",blocksread);
			fflush(stdout);
			exit(0);
		}

		if (maxrecords > 0) outpercent(blocksread);

   		/* 128 blocks before end disable readahead */
                if (blocksread+128 == maxrecords) disablereadahead();

	}	
	free(buf1);
	free(buf2);
}
	
void main(int argc, char **argv) {

	if (argc == 4) {
		strcpy(name1,argv[1]);
		strcpy(name2,argv[2]);
		maxrecords = atoi(argv[3]);
	} else 
	if (argc == 3) {
		strcpy(name1,argv[1]);
		strcpy(name2,argv[2]);
		maxrecords = -1;		/* Copy all records */
	} else
	{
		printf("Usage: %s <file1> <file2> [blockcount]\n",	
			argv[0]);
		exit(0);
	}

	fd1 = open(name1, O_RDONLY);
	if (fd1 < 0) {
		perror("open");
		fprintf(stderr,"Failed to open %s\n",name1);
		exit(1);
	}

	fd2 = open(name2, O_RDONLY);
	if (fd2 < 0) {
		perror("open");
		fprintf(stderr,"Failed to open %s\n",name2);
		exit(1);
	}


        /* Read read-ahead-value and store it */
        if (ioctl(fd1, BLKRAGET, &ra)) {
                perror("BLKRAGET");
                exit(1);
        }

	startcompare();

        /* Restore original read-ahead-value */
        if (ioctl(fd1, BLKRASET, ra)) {
            perror("BLKRASET");
            exit(1);
        }

	close(fd1);
	close(fd2);

	/* All records read? */
	if (blocksread == maxrecords) {
		fprintf(stdout,"end\n");
	}
	exit(0);
}

