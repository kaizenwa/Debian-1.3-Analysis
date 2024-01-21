#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <time.h>
#include <sys/stat.h>
#include "floppy_split.h"

void
usage(const char * name)
{
	fprintf(stderr, "Usage: %s input-file output-name kilobytes-per-floppy\n"
	 "\n\tOutput files will be written as output-name-<number>, where\n"
	 "\t<number> starts at 1 and is incremented for each file.\n"
	,name);
	exit(-1);
}

static void
writeFloppy(
 int			inputFd
,const char *	name
,int			floppyNumber
,int			totalFloppies
,long			bytesPerFloppy
,long			totalSize)
{
	union header_block	block;
	int				outputFd;
	char			filename[1024];
	unsigned long	checksum = 0;
	long			bytesToWrite = bytesPerFloppy;
	long			n;
	long			pad = 0;
	const struct tm * t = 0;

	n = totalSize - (bytesPerFloppy * (floppyNumber - 1));
	if ( n < bytesToWrite ) {
		bytesToWrite = n;
		if ( bytesToWrite % 512 > 0 )
			pad = 512 - (bytesToWrite % 512);
	}

	if ( t == 0 ) {
		time_t	tim;
		time(&tim);
		t = localtime(&tim);
	}

	memset((void *)&block, 0, sizeof(HEADER_SIZE));
	sprintf(filename, "%s-%d.bin", name, floppyNumber);
	sprintf(block.header.magic, "Floppy split 0.1");
	sprintf(block.header.name, "%s", name);
	sprintf(block.header.size, "%d", totalSize);
	sprintf(block.header.bytesInThisFloppy, "%d", bytesToWrite);
	sprintf(block.header.floppyNumber, "%d", floppyNumber);
	sprintf(block.header.totalFloppies, "%d", totalFloppies);
	strftime(
	 block.header.date
	,sizeof(block.header.date)
	,"%d-%b-%Y %H:%M %Z"
	,t);

	if ( (outputFd = open(filename, O_WRONLY|O_CREAT|O_TRUNC, 0666)) < 0 ) {
		perror(filename);
		exit(-1);
	}
	if ( lseek(outputFd, HEADER_SIZE, SEEK_SET) != HEADER_SIZE ) {
		perror(filename);
		exit(-1);
	}
	while ( bytesToWrite > 0 ) {
		char	buffer[8192];
		long	chunk = sizeof(buffer);
		long	status;
		long	byte;

		if ( bytesToWrite < chunk )
			chunk = bytesToWrite;

		status = read(inputFd, buffer, chunk);
		if ( status != chunk ) {
			perror("input-file read error");
			exit(-1);
		}

		for ( byte = 0; byte < chunk; byte++ )
			checksum += (buffer[byte] & 0xff);

		status = write(outputFd, buffer, chunk);
		if ( status != chunk ) {
			perror(filename);
			exit(-1);
		}
		bytesToWrite -= chunk;
	}
	if ( pad > 0 ) {
		char	buffer[512];
		memset(buffer, 0, sizeof(buffer)); 
		write(outputFd, buffer, pad);
	}
	if ( lseek(outputFd, 0, SEEK_SET) != 0 ) {
		perror(filename);
		exit(-1);
	}
	sprintf(block.header.checksum, "%x", checksum & 0xffffffff);
	if ( write(outputFd, (char *)&block, HEADER_SIZE) != HEADER_SIZE ) {
		perror(filename);
		exit(-1);
	}
	close(outputFd);
}

int
main(int argc, char * * argv)
{
	struct stat	s;
	int			fd;
	long		bytesPerFloppy;
	long		totalFloppies;
	int			i;

	if ( argc != 4 )
		usage(argv[0]);

	if ( (bytesPerFloppy = atol(argv[3])) < 1 ) {
		fprintf(stderr, "Invalid kilobytes-per-floppy count.\n");
		usage(argv[0]);
	}
	bytesPerFloppy *= 1024;
	bytesPerFloppy -= HEADER_SIZE;

	if ( (fd = open(argv[1], O_RDONLY)) < 0 || fstat(fd, &s) != 0 ) {
		perror(argv[1]);
		exit(-1);
	}

	totalFloppies = (s.st_size + bytesPerFloppy - 1) / bytesPerFloppy;

	for ( i = 1; i <= totalFloppies; i++ )
		writeFloppy(fd, argv[2], i, totalFloppies, bytesPerFloppy, s.st_size);

	close(fd);
	return 0;
}
