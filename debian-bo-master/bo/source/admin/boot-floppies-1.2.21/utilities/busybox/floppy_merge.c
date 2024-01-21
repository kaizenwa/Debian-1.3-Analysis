#include "internal.h"
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <linux/fd.h>
#include "../floppy_split.h"

#define	HEADER_SIZE	512
static const char	magic[] = "Floppy split";

const char floppy_merge_usage[] = "floppy_merge device";

static const char bad_floppy_message[] =
"Some important data was not read from the floppy disks. You should\n"
"resolve this problem and re-install using valid floppy disks. If you\n"
"are installing from a CD-ROM, there are floppy-disk \"image\" files on\n"
"the CD, which you can write to a blank floppy using DOS, Windows, OS/2,\n"
"Unix or Linux. They should be in a directory on the CD called \"dos\"\n"
"or \"disks\".\n"
"\n"
"Other things to try if you just can't get the floppies to install are\n"
"reformatting the floppies before writing them, and writing them using\n"
"a different system. Also, if you got the \"image\" files from a BBS or\n"
"Internet system and they are not working, download them again, and make\n"
"sure to use \"binary\" mode. If you are writing the floppies from Unix\n"
"or Linux, be sure to run the \"sync\" command before you remove each\n"
"floppy disk from the drive.\n";

static void
readFloppy(int fd, struct floppy_header * h)
{
	long			bytesInThisFloppy = atol(h->bytesInThisFloppy);
	long			bytesToRead = bytesInThisFloppy;
	long			bytesRead = 0;
	unsigned long	originalChecksum = strtoul(h->checksum, 0, 0x10);
	unsigned long	checksum = 0;
	int				byte = 0;
		
	while ( bytesToRead > 0 ) {
		char	buf[20 * 1024];
		long	chunk = sizeof(buf);
		int		status;

		if ( bytesToRead < chunk )
			chunk = bytesToRead;

		errno = 0;
		if ( (status = read(fd, buf, chunk)) != chunk ) {
			int 	old_errno = errno;
			char	garbage;

			fprintf(stderr, "Floppy read error: ");

			if ( old_errno == 0 && read(fd, buf, 1) != 1 )
				old_errno = errno;
					
			if ( old_errno == 0 )
				fprintf(stderr, "Unexpected end-of-file.\n");
			else
				fprintf(stderr, "%s\n", strerror(errno));

			fprintf(stderr, "%s\nPlease press ENTER to continue. "
			,bad_floppy_message);
			read(0, &garbage, 1);
			exit(-1);
		}

		bytesRead += chunk;
		bytesToRead -= chunk;

		fprintf(stderr, "\r  Read %ld bytes.   ", bytesRead);

		for ( byte = 0; byte < chunk; byte++ )
			checksum += (buf[byte] & 0xff);

		if ( (status = write(1, buf, chunk)) != chunk ) {
			perror("pipe write");
			exit(-1);
		}
	}
	if ( checksum != originalChecksum ) {
		char	garbage;

		fprintf(stderr
		,"\nError: the floppy disk checksum does not match the checksum made\n"
		"when it was written.\n%s\nPlease press ENTER to continue. "
		,bad_floppy_message);
		read(0, &garbage, 1);
		exit(-1);
	}
}

static void
getFloppies(const char * device)
{
	int		expectedDisk = 1;
	int		totalFloppies = 0;
	char	name[1024];
	int		fd;

	for ( ; ; ) {
		union header_block	block;
		char				garbage;
		int					diskNumber = 0;
		int					diskTotalFloppies;
		int					status;

		fprintf(stderr, "\r\aPlease insert disk %d and press ENTER.        "
		,expectedDisk);
		fflush(stderr);

		read(0, &garbage, 1);

		if ( (fd = open(device, O_RDONLY)) < 0 ) {
			perror(device);
			continue;
		}
		if ( ioctl(fd, FDFLUSH, 0) != 0 )
			perror("ioctl fdflush failed (not fatal)");

		if ( (status = read(fd, (char *)&block, HEADER_SIZE)) != HEADER_SIZE ) {
			fprintf(stderr, "Could not read floppy: ");
			if ( status > 0 )
				fprintf(stderr, "Unexpected end-of-file.\n");
			else
				fprintf(stderr, "%s\n", strerror(errno));
			continue;
		}
		if ( strncmp(block.header.magic, magic, sizeof(magic) - 1) != 0 ) {
			fprintf(stderr, "That doesn't look like the right floppy.\n");
			continue;
		}
		
		diskNumber = atoi(block.header.floppyNumber);
		diskTotalFloppies = atoi(block.header.totalFloppies);
		fprintf(stderr
		,"This is disk %d of %d in the %s series of %s.\n"
		,diskNumber
		,diskTotalFloppies
		,block.header.name
		,block.header.date);
		if ( diskNumber != expectedDisk ) {
			fprintf(stderr, "Wrong disk. I was looking for disk %d.\n"
			,expectedDisk);
			continue;
		}
		if ( diskNumber == 1 ) {
			strcpy(name, block.header.name);
			totalFloppies = diskTotalFloppies;
		}
		else if ( strcmp(name, block.header.name) != 0 ) {
			fprintf(stderr, "Wrong disk. this is from series %s,\n"
			 "You need disk %d of series %s.\n"
			,block.header.name
			,expectedDisk
			,name);
			continue;
		}
		readFloppy(fd, &block.header);
		ioctl(fd, FDFLUSH, 0);
		close(fd);
		if ( ++expectedDisk > totalFloppies )
			break;
	}
}

int
floppy_merge_main(struct FileInfo * i, int argc, char * * argv)
{
	if ( argc != 2 ) 
		usage(argv[0]);

	getFloppies(argv[1]);
	fprintf(stderr, "\rAll floppies read correctly.\n");
	return 0;
}
