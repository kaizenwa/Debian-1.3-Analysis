/*
	Copies a file with blocksize 2048
	adapted from GNU-dd
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

int input_fd = 0;
int output_fd = 0;
char inputname[1024];
char outputname[1024];
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

        if (ioctl(input_fd, BLKRASET, 0)) {
            perror("BLKRASET");
            exit(1);
        }
}

void startcopying() {
unsigned char *ibuf;
int nread,nwritten;

	ibuf = (unsigned char *)malloc(BLOCKSIZE);

	while (1) {
		if (maxrecords >= 0 && blocksread >= maxrecords)
			break;
		
		nread = read (input_fd, ibuf, BLOCKSIZE);

		if (nread == 0) {
			fprintf(stdout,"EOF on %s\n",inputname);
			fflush(stdout);
			break; 
		}

		if (nread < 0) {
			fprintf(stdout,"Read-error on %s\n",inputname);
			fflush(stdout);
			exit(0);
		}

		blocksread++;	

	 	nwritten = write (output_fd, ibuf, nread);

		if (nwritten != nread) {
			fprintf(stdout,"Write-error on %s\n",inputname);
			fflush(stdout);
			exit(0);
		}	

		if (maxrecords > 0) outpercent(blocksread);

		/* 128 blocks before end disable readahead */
		if (blocksread+128 == maxrecords) disablereadahead();
	}	
	free(ibuf);
}
	
void main(int argc, char **argv) {

	if (argc == 4) {
		strcpy(inputname,argv[1]);
		strcpy(outputname,argv[2]);
		maxrecords = atoi(argv[3]);
	} else 
	if (argc == 3) {
		strcpy(inputname,argv[1]);
		strcpy(outputname,argv[2]);
		maxrecords = -1;		/* Copy all records */
	} else
	{
		printf("Usage: %s <inputfile> <outputfile> [blockcount]\n",	
			argv[0]);
		exit(0);
	}

	input_fd = open(inputname, O_RDONLY);
	if (input_fd < 0) {
		perror("open");
		fprintf(stderr,"Failed to open %s\n",inputname);
		exit(1);
	}

	output_fd = open(outputname, O_RDWR | O_CREAT | O_TRUNC, 0666);
	if (output_fd < 0) {
		perror("open");
		fprintf(stderr,"Failed to open %s\n",outputname);
		exit(1);
	}

	/* Read read-ahead-value and store it */
	if (ioctl(input_fd, BLKRAGET, &ra)) {
        	perror("BLKRAGET");
        	exit(1);
    	}

	startcopying();

	/* Restore original read-ahead-value */
        if (ioctl(input_fd, BLKRASET, ra)) {
            perror("BLKRASET");
            exit(1);
        }

	close(input_fd);
	close(output_fd);

	/* All blocks read? */
	if (blocksread == maxrecords) {
		fprintf(stdout,"end\n");
	}
	exit(0);
}

