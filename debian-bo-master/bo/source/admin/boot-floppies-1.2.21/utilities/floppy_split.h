#define	HEADER_SIZE	512
struct floppy_header {
	char	magic[32];
	char	name[100];
	char	size[16];
	char	bytesInThisFloppy[16];
	char	floppyNumber[8];
	char	totalFloppies[8];
	char	checksum[16];
	char	date[40];
};

union header_block {
	struct floppy_header	header;
	char					bytes[HEADER_SIZE];
};

