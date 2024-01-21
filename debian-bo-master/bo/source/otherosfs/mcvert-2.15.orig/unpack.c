#include "mactypes.h"

extern word magic[];
extern char *dir, *ext;
extern int info_only;

ulong pit_datalen, pit_rsrclen;
word hqx_crc, write_pit_fork();
char pitfname[SYSNAMELEN];	/* name of file being unpacked */
FILE *pitfile;			/* output file */

branch branchlist[255], *branchptr, *read_tree();
leaf leaflist[256], *leafptr;
word Huff_nibble, Huff_bit_count;

byte(*read_char) (), get_crc_byte(), getHuffbyte();

word 
un_pit()
{
	char PitId[4];
	int i;
	word pit_crc;

	hqx_crc = 0;
	/* Read and unpack until the PackIt End message is read */
	for (;;) {
		read_char = get_crc_byte;
		for (i = 0; i < 4; i++)
			PitId[i] = (char) get_crc_byte();
		if (!strncmp(PitId, "PEnd", 4))
			break;

		if (strncmp(PitId, "PMag", 4) && strncmp(PitId, "PMa4", 4))
			error("Unrecognized Packit format message %s", PitId);

		if (PitId[3] == '4') {
			/* if this file is compressed */
			/* read the Huffman decoding  */
			/* tree that is on the input  */
			/* and use Huffman decoding   */
			/* subsequently               */
			branchptr = branchlist;
			leafptr = leaflist;
			Huff_bit_count = 0;
			(void)read_tree();
			read_char = getHuffbyte;
		}
		read_pit_hdr();		       /* also calculates datalen,
					        * rsrclen, pitfile, pitfname */
		pit_crc = write_pit_fork(pit_datalen, (ulong)0);
		pit_crc = write_pit_fork(pit_rsrclen, (ulong)pit_crc);
		check_pit_crc(pit_crc, "  File data/rsrc CRC mismatch in %s", pitfname);
		mclose(&pitfile, "pitfile");
	}
	hqx_crc = (hqx_crc << 8) ^ magic[hqx_crc >> 8];
	hqx_crc = (hqx_crc << 8) ^ magic[hqx_crc >> 8];
	return hqx_crc;
}

check_pit_crc(calc_crc, msg, name)
	word calc_crc;
	char msg[], name[];

{
	word read_crc;

	read_crc = (*read_char) () << 8;
	read_crc |= (*read_char) ();
	if (read_crc != calc_crc)
		error(msg, name);
}

/*  
 * This routine reads the header of a packed file and appropriately
 * twiddles it, determines if it has CRC problems, creates the .bin
 * file, and puts the info into the .bin file.  Output is
 * pit_datalen, pit_rsrclen, pitfname, pitfile
 */
read_pit_hdr()
{
	register int n;
	register byte *pit_byte;
	register ulong pit_crc;
	pit_header pit;
	info_header info;
	short crc;
	char *action;
	long len;

	extern short calc_mb_crc();

	/* read the pit header and compute the CRC */
	pit_crc = 0;
	pit_byte = (byte *) & pit;
	for (n = 0; n < sizeof(pit_header); n++) {
		*pit_byte = (*read_char) ();
		pit_crc = ((pit_crc & 0xff) << 8)
		    ^ magic[*pit_byte++ ^ (pit_crc >> 8)];
	}

	/* stuff the pit header data into the info header */
	bzero((char*)&info, sizeof(info_header));
	info.nlen = pit.nlen;
	if (pit.nlen > sizeof(info.name))
		error("Error: corrupt PIT data format", "");
	/*
	 * expect a valid Macintosh file name since it came from a PIT file
	 * and don't potentially corrupt a valid copied name via macify
	 */
	strncpy((char*)info.name, (char*)pit.name, (int)pit.nlen);/* name */
	bcopy((char*)pit.type, (char*)info.type, 9);  /* type, author, flag */
	bcopy((char*)pit.dlen, (char*)info.dlen, 16); /* (d,r)len, (c,m)tim */
	info.flags &= 0x7e;		       /* reset lock bit, init bit */
	if (pit.protect & 0x40)
		info.protect = 1;	       /* copy protect bit */
	info.uploadvers = '\201';
	info.readvers = '\201';

	/* calculate MacBinary CRC */
	crc = calc_mb_crc((unsigned char*)&info, 124L, 0);
	info.crc[0] = (char) (crc >> 8);
	info.crc[1] = (char) crc;

	/* Create the .bin file and write the info to it */
	pit.name[pit.nlen] = '\0';
	unixify((char*)pit.name);

	len = strlen(dir) + strlen((char*)pit.name) + strlen(ext) + 1;
	if (len >= sizeof(pitfname))
		error("Error: generated pitfname would be too long", "");
	sprintf(pitfname, "%s/%s%s", dir, pit.name, ext);

	if (info_only)
		action = (read_char == get_crc_byte) ? "Packed" : "Compressed";
	else
		action = (read_char == get_crc_byte) ? "Unpacking" : "Decompressing";

	fprintf(convert, " %-14s%-30s type = \"%4.4s\", author = \"%4.4s\"\n",
	    action, pit.name, pit.type, pit.auth);
	fflush(convert);
	print_bin_hdr("Creating", &info);
	pitfile = mopen(pitfname, "", "w");
	check_pit_crc((word)pit_crc, "  File header CRC mismatch in %s", pitfname);
	if (1 != fwrite((char*)&info, sizeof(info_header), 1, pitfile))
		error("fwrite failed on pitfile", "");

	/* Get a couple of items we'll need later */
	bcopy((char*)pit.dlen, (char*)&pit_datalen, 4);
	pit_datalen = mac2long(pit_datalen);
	bcopy((char*)pit.rlen, (char*)&pit_rsrclen, 4);
	pit_rsrclen = mac2long(pit_rsrclen);
}

/* This routine copies bytes from the decoded input stream to the output
    and calculates the CRC.  It also pads to a multiple of 128 bytes on the
    output, which is part of the .bin format */
word 
write_pit_fork(nbytes, calc_crc)
	register ulong nbytes;
	register ulong calc_crc;
{
	register ulong b;
	int extra_bytes;

	/* pad fork to mult of * 128 bytes */
	extra_bytes = 127 - (nbytes + 127) % 128;
	while (nbytes--) {
		b = (*read_char) ();
		calc_crc = ((calc_crc & 0xff) << 8) ^ magic[b ^ (calc_crc >> 8)];
		if (EOF == putc((char)b, pitfile))
			error("Error: putc failed on pitfile", "");
	}
	while (extra_bytes--)
		if (EOF == putc(0, pitfile))
			error("Error: putc failed on pitfile", "");
	return (word) calc_crc;
}

/* This routine recursively reads the compression decoding data.
   It appears to be Huffman compression.  Every leaf is represented
   by a 1 bit, then the byte it represents.  A branch is represented
   by a 0 bit, then its zero and one sons */
branch *
read_tree()
{
	register branch *branchp;
	register leaf *leafp;
	register ulong b;

	if (!Huff_bit_count--) {
		Huff_nibble = get_crc_byte();
		Huff_bit_count = 7;
	}
	if ((Huff_nibble <<= 1) & 0x0100) {
		leafp = leafptr++;
		leafp->flag = 1;
		b = get_crc_byte();
		leafp->data = Huff_nibble | (b >> Huff_bit_count);
		Huff_nibble = b << (8 - Huff_bit_count);
		return (branch *) leafp;
	} else {
		branchp = branchptr++;
		branchp->flag = 0;
		branchp->zero = read_tree();
		branchp->one = read_tree();
		return branchp;
	}
}

/* This routine returns the next 8 bits.  It finds the byte in the
   Huffman decoding tree based on the bits from the input stream. */
byte 
getHuffbyte()
{
	register branch *branchp;

	branchp = branchlist;
	while (!branchp->flag) {
		if (!Huff_bit_count--) {
			Huff_nibble = get_crc_byte();
			Huff_bit_count = 7;
		}
		branchp = ((Huff_nibble <<= 1) & 0x0100) ? branchp->one : branchp->zero;
	}
	return ((leaf *) branchp)->data;
}

/* This routine returns the next byte on the .hqx input stream, hiding
    most file system details at a lower level.  .hqx CRC is maintained
    here */
byte 
get_crc_byte()
{
	register ulong c;
	extern byte *buf_ptr, *buf_end;

	if (buf_ptr == buf_end) {
		if (fill_hqxbuf(0) == 0)
			error("premature EOF reading PIT info from BinHex 4.0 file", "");
	}
	c = *buf_ptr++;
	hqx_crc = ((hqx_crc << 8) | c) ^ magic[hqx_crc >> 8];
	return (byte) c;
}
