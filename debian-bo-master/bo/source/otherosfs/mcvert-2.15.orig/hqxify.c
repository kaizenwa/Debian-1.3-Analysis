#include "mactypes.h"

/* HQXBUFLEN must be large enough to hold a complete hqx header */
#define HQXBUFLEN 512
byte hqxbuf[HQXBUFLEN + 1], *buf_ptr, *buf_end, *buf_start = hqxbuf + 1;

/* Note: line[0] stores the run length character,
 * so line is one greater than MAXLINE  */
#define MAXLINE 2048
byte line[MAXLINE + 1], *line_ptr, *line_end, *line_start = line + 1;

/* keep the compiler happy */
#define LINE_START ((char*)(line_start))

extern char *cmdname;

int line_count, file_count;
int save_state, total_bytes, save_run_length;
word save_accum;
char binfname[SYSNAMELEN], hqxfname[SYSNAMELEN];
FILE *hqxfile, *binfile;


/*
 * hqxsuspect caches whether or not we have gotten past all suspect data
 * at the head of a file, for example, a mail header.  hqxsuspect is set
 * on every new hqx file read, and reset only in non_suspect()
 */
int hqxsuspect;
extern suspect_shorter;
extern suspect_same;

extern int info_only;

/* This routine reads the header of a hqxed file and appropriately twiddles it,
    determines if it has CRC problems, creates the .bin file, and puts the info
    into the .bin file.
    Output is hqx_datalen, hqx_rsrclen, type, binfname, binfile.
	 Returns 0 iff failed to read header.
 */


int
hqx_to_bin_hdr(type, hqx_datalen, hqx_rsrclen, same_file_only)
	char *type;
	ulong *hqx_datalen, *hqx_rsrclen;
	int same_file_only;
{
	register byte *hqx_ptr, *hqx_end;
	register ulong calc_crc;
	hqx_buf *hqx_block;
	hqx_header *hqx;
	info_header info;
	ulong mtim;
	short crc;
	long len;

	extern word magic[];
	extern char *dir, *ext;
	extern short calc_mb_crc();

	/* read the hqx header, 
	 * assuming that I won't exhaust hqxbuf in so doing --
	 * that is, that hqxbuf is always large enough (it is)
	 */
	(void)fill_hqxbuf(same_file_only);
	/*
	 * If we are reading multiple files, then we could have the last
	 * unread line of the "previous" file be just a colon (since we are
	 * length driven, and stopped when we processed the expected
	 * lengths), and the prior fill_hqxbuf() call would find it and
	 * return 0, leaving the buffer unfilled.  So, if we have zero
	 * bytes so far, just fill it again.
	 */
	if (total_bytes == 0) {
		DEBUG && fprintf(debug,
			"%s: Note: had to call fill_hqxbuf again in hqx_to_bin_hdr\n",
			cmdname);
		DEBUG && fflush(debug);
		(void)fill_hqxbuf(same_file_only);
	}
	if (same_file_only == 1 && total_bytes == 0)
		return 0;
	if (total_bytes < MIN_HQX_HDR) {
		fprintf(verbose, "%s: %s (%d < %d): bad file format? -- exiting\n",
			cmdname, "error: hqx_header too short", total_bytes, MIN_HQX_HDR);
		fflush(verbose);
		exit(2);
	}

	hqx_block = (hqx_buf *) buf_ptr;
	hqx = (hqx_header *) (hqx_block->name + hqx_block->nlen);
	hqx_ptr = buf_ptr;
	hqx_end = (byte *) hqx + sizeof(hqx_header) - 1;
	calc_crc = 0;
	while (hqx_ptr < hqx_end)
		calc_crc = (((calc_crc & 0xff) << 8) | *hqx_ptr++) ^ magic[calc_crc >> 8];
	calc_crc = ((calc_crc & 0xff) << 8) ^ magic[calc_crc >> 8];
	calc_crc = ((calc_crc & 0xff) << 8) ^ magic[calc_crc >> 8];
	buf_ptr = hqx_ptr;

	/* stuff the hqx header data into the info header */
	bzero((char*)&info, sizeof(info_header));
	info.nlen = hqx_block->nlen;
	if (info.nlen > sizeof(info.name))
		error("Error: corrupt BinHex data format", "");
	strncpy((char*)info.name, (char*)hqx_block->name, (int)info.nlen);/* name */

	/* now make sure the resulting name is valid for length only */
	info.nlen = macify((char*)info.name, (int)info.nlen, 0);

	bcopy((char*)hqx->type, (char*)info.type, 9);       /* type, author, flag */
	info.flags &= 0x7e;		       /* reset lock bit, init bit */
	if (hqx->protect & 0x40)
		info.protect = 1;	       /* copy protect bit */
	bcopy((char*)hqx->dlen, (char*)info.dlen, 8);	       /* dlen, rlen */
	mtim = unix2mac((ulong)time((long*)0));
	bcopy((char*)&mtim, (char*)info.mtim, 4);
	bcopy((char*)&mtim, (char*)info.ctim, 4);
	info.uploadvers = '\201';
	info.readvers = '\201';

	/* calculate MacBinary CRC */
	crc = calc_mb_crc((unsigned char*)&info, 124L, 0);
	info.crc[0] = (char) (crc >> 8);
	info.crc[1] = (char) crc;

	/* Create the .bin file and write the info to it */

	unixify((char*)hqx_block->name);
	converting(info.name, (int)info.nlen, info.type, info.auth);
	print_bin_hdr("Creating", &info);

	len = strlen(dir) + strlen((char*)hqx_block->name) + strlen(ext) + 1;
	if (len >= sizeof(binfname))
		error("Error: generated binfname would be too long", "");
	sprintf(binfname, "%s/%s%s", dir, hqx_block->name, ext);
	binfile = mopen(binfname, "", "w");

	check_hqx_crc((word)calc_crc, "File header CRC mismatch in %s", binfname);
	if (1 != fwrite((char*)&info, sizeof(info), 1, binfile))
		error("fwrite failed on binfile", "");

	/* Get a couple of items we'll need later */
	bcopy((char*)info.dlen, (char*)hqx_datalen, 4);
	*hqx_datalen = mac2long(*hqx_datalen);
	bcopy((char*)info.rlen, (char*)hqx_rsrclen, 4);
	*hqx_rsrclen = mac2long(*hqx_rsrclen);
	bcopy((char*)info.type, (char*)type, 4);

	/* emit useful debugging info */
	DEBUG && fprintf(debug, "\tdata_len=%10ld\t\trsrc_len=%10ld\n",
		*hqx_datalen, *hqx_rsrclen);
	DEBUG && fflush(debug);

	return 1;
}

/* This routine reads the header of a bin file and appropriately twiddles it,
    creates the .hqx file, and puts the info into the .hqx file.
    Output is hqx_datalen, hqx_rsrclen, type, hqxfname, hqxfile */

bin_to_hqx_hdr(hqx_datalen, hqx_rsrclen)
	ulong *hqx_datalen, *hqx_rsrclen;
{
	register byte *hqx_ptr, *hqx_end;
	register ulong calc_crc;
	hqx_buf *hqx_block;
	hqx_header *hqx;
	info_header info;
	extern word magic[];
	extern char **hqxnames_left;
	extern char *ext;
	long len;

	len = strlen(*hqxnames_left);
	if (len >= sizeof(binfname))
		error("Error: specified binfname is too long", "");
	strcpy(binfname, *hqxnames_left++);
	binfile = mopen(binfname, ext, "r");

	if (!fread((char*)&info, sizeof(info), 1, binfile))
		error("Unexpected EOF in MacBinary header of %s", binfname);

	/* stuff the info header into the hqx header */
	hqx_block = (hqx_buf *) buf_ptr;
	hqx_block->nlen = info.nlen;
	if (info.nlen > sizeof(hqx_block->name))
		error("Error: corrupt MacBinary data format", "");
	strncpy((char*)hqx_block->name, (char*)info.name, (int)info.nlen);
	/*
	 * We expect a valid Macintosh file name since it came from a MacBinary file
	 * so we don't potentially corrupt a valid copied name via macify translate.
	 * File name length should not be a problem, so we do nothing.
	 */
	hqx = (hqx_header *) (hqx_block->name + hqx_block->nlen);
	hqx->version = 0;
	bcopy((char*)info.type, (char*)hqx->type, 9);   /* type, author, flags */
	hqx->flags &= 0x7e;		       /* reset lock bit, init bit */
	if (info.protect == 1)
		hqx->protect = 0;	       /* protect bit: 0x40 */
	else
		hqx->protect = 0;
	bcopy((char*)info.dlen, (char*)hqx->dlen, 8);	       /* dlen, rlen */

	/* Create the .hqx file and write the info to it */

#ifdef notdef
	   This is the right thing to check but the Sun optimizing
	   compiler gives a (valid) warning that info.nlen (one char,
	   255 max) is always less than sizeofhqxfname) which is
	   hardwired to 1024 since version 1.99.  Since we have already
	   checked info.nlen above, we skip the test and avoid the warning.

	if (info.nlen >= sizeof(hqxfname))
		error("Error: generated hqxfname would be too long", "");
#endif

	strncpy(hqxfname, (char*)info.name, (int)info.nlen);
	hqxfname[info.nlen] = '\0';
	unixify(hqxfname);
	converting(info.name, (int)info.nlen, info.type, info.auth);
	print_bin_hdr("Reading", &info);

	calc_crc = 0;
	hqx_ptr = (byte *) hqx_block;
	hqx_end = hqx_ptr + hqx_block->nlen + sizeof(hqx_header);
	while (hqx_ptr < hqx_end)
		calc_crc = (((calc_crc & 0xff) << 8) | *hqx_ptr++) ^ magic[calc_crc >> 8];
	calc_crc = ((calc_crc & 0xff) << 8) ^ magic[calc_crc >> 8];
	calc_crc = ((calc_crc & 0xff) << 8) ^ magic[calc_crc >> 8];
	buf_ptr = hqx_end;
	write_hqx_crc((word)calc_crc);

	/* Get a couple of items we'll need later */
	bcopy((char*)info.dlen, (char*)hqx_datalen, 4);
	*hqx_datalen = mac2long(*hqx_datalen);
	bcopy((char*)info.rlen, (char*)hqx_rsrclen, 4);
	*hqx_rsrclen = mac2long(*hqx_rsrclen);
}


/* This routine copies bytes from the decoded input stream to the output.  
    It also pads to a multiple of 128 bytes on the output, which is part
    of the .bin format */
word
hqx_to_bin_fork(nbytes)
	register ulong nbytes;
{
	register byte *cp;
	register ulong calc_crc;
	register int c_length;
	ulong extra_bytes;
	extern word magic[];
	long avail = 0;	/* used for internal consistency checking */
	int wrote;

	extra_bytes = 127 - (nbytes + 127) % 128;	/* pad fork to mult of
							 * 128 bytes */
	calc_crc = 0;
	for (;;) {
		cp = buf_ptr;
		c_length = (cp + nbytes > buf_end) ? buf_end - cp : nbytes;
		/* we can only check readily if we read it here */
		if (avail && c_length > avail)
			error("hqx_to_bin_fork: writing %ld too many bytes",
				(char*)c_length - avail);
		nbytes -= c_length;
		wrote = fwrite((char*)cp, sizeof(byte), c_length, binfile);
		if (wrote != c_length)
			error("hqx_to_bin_fork: fwrite on binfile wrote %ld bytes too few",
				(char*)c_length-wrote);
		while (c_length--)
			calc_crc = (((calc_crc & 0xff) << 8) | *cp++) ^ magic[calc_crc >> 8];
		if (!nbytes)
			break;
		avail = fill_hqxbuf(0);
	}
	buf_ptr = cp;
	while (extra_bytes--)
		if (EOF == putc(0, binfile))
			error("Error: putc failed on binfile", "");
	calc_crc = ((calc_crc & 0xff) << 8) ^ magic[calc_crc >> 8];
	calc_crc = ((calc_crc & 0xff) << 8) ^ magic[calc_crc >> 8];
	return (word) calc_crc;
}

/* This routine copies bytes from the input stream to the encoded output.  
    It also pads to a multiple of 128 bytes on the input, which is part
    of the .bin format */
word
bin_to_hqx_fork(nbytes)
	register ulong nbytes;
{
	register byte *cp;
	register ulong calc_crc;
	register int c_length;
	ulong extra_bytes;
	extern word magic[];

	extra_bytes = 127 - (nbytes + 127) % 128;	/* pad fork to mult of
							 * 128 bytes */
	calc_crc = 0;
	for (;;) {
		cp = buf_ptr;
		c_length = (cp + nbytes > buf_end) ? buf_end - cp : nbytes;
		nbytes -= c_length;
		if (c_length != fread((char*)cp, sizeof(byte), c_length, binfile))
			error("fread failed on binfile", "");
		buf_ptr += c_length;
		while (c_length--)
			calc_crc = (((calc_crc & 0xff) << 8) | *cp++) ^ magic[calc_crc >> 8];
		if (!nbytes)
			break;
		empty_hqxbuf();
	}
	buf_ptr = cp;

	fseek(binfile, (long)extra_bytes, 1);
	calc_crc = ((calc_crc & 0xff) << 8) ^ magic[calc_crc >> 8];
	calc_crc = ((calc_crc & 0xff) << 8) ^ magic[calc_crc >> 8];
	return (word) calc_crc;
}

/* Essentials for Binhex 8to6 run length encoding */
#define RUNCHAR 0x90
#define MAXRUN 255
#define IS_LEGAL <0x40
#define ISNT_LEGAL >0x3f
#define DONE 0x7F		/* tr68[':'] = DONE, since Binhex terminator is ':' */
#define SKIP 0x7E		/* tr68['\n'|'\r'] = SKIP, i. e. end of line char.  */
	/* We also treat '\0' as SKIP to handle files without trailing newline */

	/*
	   NOTE: we really don't do a very good job of handling lines
	   with ^M's as the end of line character, since we jettison
	   the entire UNIX line (which can look like multiple Mac
	   lines) when rejecting the line.  Easiest way to fix this is
	   to write our own version of fgets which breaks on ^M or ^J.
	 */

#define FAIL 0x7D		/* character illegal in binhex file */

byte tr86[] =
"!\"#$%&'()*+,-012345689@ABCDEFGHIJKLMNPQRSTUVXYZ[`abcdefhijklmpqr";
byte tr68[] = {
/* 0x00 */
    SKIP, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, SKIP, FAIL, FAIL, SKIP, FAIL, FAIL,
/* 0x10 */
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0x20 */
    FAIL, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
    0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, FAIL, FAIL,
/* 0x30 */
    0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, FAIL,
    0x14, 0x15, DONE, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0x40 */
    0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
    0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, FAIL,
/* 0x50 */
    0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, FAIL,
    0x2C, 0x2D, 0x2E, 0x2F, FAIL, FAIL, FAIL, FAIL,
/* 0x60 */
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, FAIL,
    0x37, 0x38, 0x39, 0x3A, 0x3B, 0x3C, FAIL, FAIL,
/* 0x70 */
    0x3D, 0x3E, 0x3F, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0x80 */
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0x90 */
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0xA0 */
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0xB0 */
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0xC0 */
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0xD0 */
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0xE0 */
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
/* 0xF0 */
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
    FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL, FAIL,
};

/*
 *  This procedure transparently reads and decodes the hqx input.
 *  It does run length and 6 to 8 decoding.
 *  As many lines as required are read to fill up buf.
 * Global Input
 *  buf_start
 *  line_ptr
 *  save_*
 * Global Output
 *  buf_ptr end of buffer used
 *  line_ptr
 *  save_*
 * Internal
 *  line     holds the encoded incoming ascii line.
 *  buf      holds the decoding binary binary. [what the heck does this mean?]
 *  buf[-1]  holds the character to be repeated for run length encoding.
 */
#define READING 0
#define SKIPPING 1
#define FIND_START_COLON 2
#define FINDING_COLON 3

/*

 * It's too bad we can't look for ``(This file ..'' when hunting for a
 * starting colon, but we don't emit such a line at the head of every
 * segment, so we can't expect to find one, now, can we?

 * We were really too brittle when scanning, since we only
 * accepted lines which were exactly 64 (HQXLINELEN) characters long.
 * Fixed in version 1.83.  But allowing any length valid line
 * permitted the false-match-in-mail-headers-problem for lines like "---".
 * Better header skipping provided in 1.84.

 * XXX: rather than decode line inplace, it would be better
 * to leave the original intact to enable better warning messages

 */

/* returns the number of bytes read */
fill_hqxbuf(same_file_only)
	int same_file_only;
{
	register ulong c, accum;
	register int not_in_a_run = TRUE, state68;
	register byte *fast_buf, *fast_line;
	static int phase = FIND_START_COLON;
	long avail;

	buf_ptr = fast_buf = buf_start;
	fast_line = line_ptr;
	state68 = save_state;
	accum = save_accum;
	if (save_run_length > 0) {
		c = save_run_length;
		save_run_length = 0;
		goto continue_run;
	}

	while (fast_buf < buf_end) {

next_char:
		if ((c = *fast_line++) ISNT_LEGAL) {
			if (c == DONE) {
				/* done processing the file, so get out */
				/* phase has already been set by read processing */
				break;
		  }

	next_line:
			if (!fgets(LINE_START, MAXLINE, hqxfile)) {
				if (same_file_only) {
					avail = fast_buf - buf_ptr;
					if (avail != 0) {
						DEBUG && fprintf(debug, "DEBUG: avail: %d\n", avail);
						DEBUG && fflush(debug);
						error(
				"Premature EOF reading non-initial BinHex 4.0 header from %s",
							hqxfname);
					} else {
						/* this routine may be called many times in a row,
						 * and we must ensure that we don't allow fast_line
						 * to march beyond the end of line (it did), so we jam
						 * fast_line to the start of the line, and salt the
						 * line to mark it invalid, so a new line will be fetched.
						 * Jeepers, how I hate the (il)logic of this routine!
						 */
						fast_line = line_start;
						*fast_line = SKIP;
						break;
					}
					/*NOTREACHED*/
				} else if (new_in_hqx_file() == 0) {
					/*
					 * Used to assume if no more input available through error or
					 * exhaustion while looking for valid data then must be done.
					 * But we are demand driven (now) and if called, must
					 * find data.  So we don't silently exit any more.
					 */
					error("Premature EOF while reading BinHex 4.0 %s", hqxfname);
				}
			}
			line_ptr = line_start;

			if (Debug > 1) {
					  fprintf(debug, "DEBUG: input: %s", line_start);
					  fflush(debug);
		 	}

	scan_line:

			/* ensure the the entire line is valid */

			fast_line = line_ptr;
			while ((*fast_line = tr68[*fast_line]) IS_LEGAL)
				fast_line++;

			/* grab the stopper */
			c = *fast_line;

			/* check for validity, transition phases as required */
			switch (phase) {

			case READING:
			case SKIPPING:
			case FINDING_COLON:
				if (SKIP == c && fast_line > line_ptr &&
					( 0 == hqxsuspect ||
						non_suspect((char *)line_ptr, (char *)fast_line) )
				) {
					/* the entire line is valid (again), so (resume) reading */
					/* hack: require the line to be non-empty to simplify logic */
					/* require line to non suspect [outside [mail] header] */
					phase = READING;
					break;
				}
				if (c == DONE && tr68[fast_line[1]] == SKIP) {
					/*
					 * valid encoded last line, so
					 * set phase for next line, and process this line --
					 * we exit the fill-the-buffer loop when the terminal
					 * colon is encountered during line processing
					 */
					phase = FIND_START_COLON;
					break;
				}

				/* line is not entirely valid, so do some flavor of skipping */
				phase = (phase == FINDING_COLON) ? FIND_START_COLON : SKIPPING;
				/* we're suspicious again, since we could be reading a
				 * concatenated multi-segmented file */
				hqxsuspect = 1;
				goto next_line;

			case FIND_START_COLON:
				if (*line_start == DONE) {
					/* can't transition to READING
					 * until know that entire line is valid
					 * so transition to intermediate state
					 */
					phase = FINDING_COLON;
					/* skip the initial colon */
					line_ptr++;
					goto scan_line;
				}
				goto next_line;

			}

			/* we've read in a valid line, so start processing it */
			fast_line = line_ptr;
			c = *fast_line++;

			/*
			 * Jskud 15Jul92: fix bug reported by Info-Mac Moderator Bill
			 * regarding case of last line just :
			 * The line is valid, but it has no data, so don't ingest it.
			 */

			if (c == DONE)
				break;

			if (Debug > 1) {
				fprintf(debug, "DEBUG: processing above line\n\n");
				fflush(debug);
			}

		}

		/* Finally, we have the next 6 bits worth of data in "c" as input. */
		/* Note: we use "c" as the output of this processing too */
		switch (state68++) {
		case 0:
			accum = c;
			goto next_char;
		case 1:
			accum = (accum << 6) | c;
			c = accum >> 4;
			break;
		case 2:
			accum = (accum << 6) | c;
			c = (accum >> 2) & 0xff;
			break;
		case 3:
			/* we avoid any miniscule optimizations here
			 * to maintain parallelism and clarity
			 * which should enhance program maintainability
			 */
			accum = (accum << 6) | c;
			c = accum & 0xff;
			state68 = 0;
			break;
		}
		if (not_in_a_run)
			if (c != RUNCHAR)
				*fast_buf++ = c;
			else {
				not_in_a_run = FALSE;
				goto next_char;
			}
		else {
			/* "c" has the run total length, not just the incremental,
			   hence the post decrement is correct */
			if (c--) {
				avail = buf_end - fast_buf;
				if (c > avail) {
					save_run_length = c - avail;
					c = avail;
				}
		continue_run:
				{
					register char ch = fast_buf[-1];

					while (c--)
						*fast_buf++ = ch;
				}

			} else
				/* handle special case of 0x9000 => 0x90 */
				*fast_buf++ = RUNCHAR;

			not_in_a_run = TRUE;
		}
	}

/* exit: */
	avail = fast_buf - buf_ptr;
	total_bytes += avail;
	buf_start[-1] = fast_buf[-1];
	line_ptr = fast_line;
	save_state = state68;
	save_accum = accum;

	return avail;

}

non_suspect(start, beyond)
	char *start;
	char *beyond;
{
	int looking_good;
	int len;
	register char *cp;
	register char *last;
	char *skip_msg = "Warning: skipping legal but suspect line in hqx file";

	/* ensure it's long enough */
	len = beyond - start;
	looking_good = len >= suspect_shorter;
	DEBUG && fprintf(debug, "DEBUG: non_suspect: long enough: %d\n",
		looking_good);
	DEBUG && fflush(debug);
	if (!looking_good) {
		fprintf(verbose, "%s -- line too short (%d < %d)\n",
			skip_msg, len, suspect_shorter);
		fflush(verbose);
	}

	/* ensure it's different enough */
	if (suspect_same && looking_good) {
		last = beyond - 1;
		for (cp = start; cp < last; cp++)
			if (*cp != *last) {
				break;
			}
		/* is different */
		looking_good = cp != last;
		DEBUG && fprintf(debug,
			"DEBUG: non_suspect: different enough: %d\n",
			looking_good);
		DEBUG && fflush(debug);
		if (!looking_good) {
			if (*last IS_LEGAL) {
				fprintf(verbose, "%s -- all one input char ('%c')\n",
					skip_msg, tr86[*last] );
			} else {
				fprintf(verbose, "%s -- all chars mapped to 0x%02x\n",
					skip_msg, *last );
			}
			fflush(verbose);
		}
	}

	hqxsuspect = !looking_good;

	return looking_good;
}

new_in_hqx_file()
{
	extern char **hqxnames_left;
	int result;
	long len;

	DEBUG && fprintf(debug, "entering new_in_hqx_file ...\n");
	DEBUG && fflush(debug);

	if (*hqxnames_left[0] == '\0' || *hqxnames_left[0] == '-') {
		result = FALSE;
		goto exit;
	}

	len = strlen(*hqxnames_left);
	if (len >= sizeof(hqxfname))
		error("Error: specified hqxfname is too long", "");
	strcpy(hqxfname, *hqxnames_left++);
	/*
	 * we used to use freopen,
	 * but now suffer the slight inefficiency of close/open
	 * to provide info_only and consistent handling
	 * with good software engineering methods
	 */
	mclose(&hqxfile, "hqxfile");
	hqxfile = mopen(hqxfname, ".hqx", "r");
	hqxsuspect = 1;
	(void)fgets(LINE_START, MAXLINE, hqxfile);
	result = TRUE;

exit:
	if (result == TRUE)
		DEBUG && fprintf(debug, "... opened %s\n", hqxfname);
	else
		DEBUG && fprintf(debug, "... nothing to open\n");
	DEBUG && fflush(debug);

	return result;
}

/*
 *  This procedure transparently encodes and writes the hqx output.  
 *  It does run length and 8 to 6 encoding.
 */
empty_hqxbuf()
{
	register ulong c, accum, last_c;
	register byte *fast_buf, *fast_line;
	register int state86, dont_look_for_runs = FALSE, run_length;
	extern int maxlines;

	run_length = save_run_length;
	last_c = buf_start[-1];
	fast_buf = buf_start;
	fast_line = line_ptr;
	state86 = save_state;
	accum = save_accum;
	while (fast_buf < buf_ptr) {
		c = *fast_buf++;
		if (dont_look_for_runs)
			dont_look_for_runs = FALSE;
		else if (last_c == c && run_length < MAXRUN) {
			run_length++;
			continue;
		} else {
			if (run_length > 1) {
				--fast_buf;
				if (run_length == 2 && last_c != RUNCHAR)
					c = last_c;
				else {
					c = RUNCHAR;
					*--fast_buf = run_length;
					dont_look_for_runs = TRUE;
				}
				run_length = 1;
			} else
				last_c = c;
			if (c == RUNCHAR && !dont_look_for_runs) {
				*--fast_buf = 0;
				dont_look_for_runs = TRUE;
			}
		}

		if (fast_line == line_end) {
			if (line_count++ == maxlines)
				new_out_hqx_file();
			fputs(LINE_START, hqxfile);
			fast_line = line_start;
		}
		switch (state86++) {
		case 0:
			*fast_line++ = tr86[c >> 2];
			accum = (c << 4) & 0x3f;
			break;
		case 1:
			*fast_line++ = tr86[(c >> 4) | accum];
			accum = (c << 2) & 0x3f;
			break;
		case 2:
			*fast_line++ = tr86[(c >> 6) | accum];
			if (fast_line == line_end) {
				if (line_count++ == maxlines)
					new_out_hqx_file();
				fputs(LINE_START, hqxfile);
				fast_line = line_start;
			}
			*fast_line++ = tr86[c & 0x3f];
			state86 = 0;
			break;
		}
	}
	save_run_length = run_length;
	buf_start[-1] = last_c;
	buf_ptr = buf_start;
	line_ptr = fast_line;
	save_state = state86;
	save_accum = accum;
}

new_out_hqx_file()
{
	char filename[SYSNAMELEN + 7];
	extern int maxlines;

	fprintf(hqxfile, "<<< End of Part %2d >>>\n", file_count);
	mclose(&hqxfile, "hqxfile");
	file_count++;
	if (maxlines)
		sprintf(filename, "%s%02d.hqx", hqxfname, file_count);
	else
		sprintf(filename, "%s.hqx", hqxfname);
	hqxfile = mopen(filename, "", "w");
	if (file_count > 1)
		fprintf(hqxfile, "<<< Start of Part %2d >>>\n", file_count);
	else
		fprintf(hqxfile, "(This file must be converted with BinHex 4.0)\n\n");
	line_count = 3;
}

check_hqx_crc(calc_crc, msg, name)
	word calc_crc;
	char msg[], name[];

{
	word read_crc;

	if (buf_ptr >= buf_end)
		(void)fill_hqxbuf(0);
	read_crc = *buf_ptr++ << 8;
	if (buf_ptr >= buf_end)
		(void)fill_hqxbuf(0);
	read_crc |= *buf_ptr++;
	if (read_crc != calc_crc)
		error(msg, name);
}

write_hqx_crc(calc_crc)
	word calc_crc;
{
	if (buf_ptr == buf_end)
		empty_hqxbuf();
	*buf_ptr++ = calc_crc >> 8;
	if (buf_ptr == buf_end)
		empty_hqxbuf();
	*buf_ptr++ = calc_crc;
}

un_hqx(unpit_flag)
	int unpit_flag;
{
	char type[5];	/* stuff EOS */
	ulong hqx_datalen, hqx_rsrclen;
	word un_pit();
	int unpitting, bytes_read;
	word calc_crc;
	extern char **hqxnames_left;
	int same_file_only = 0;
	int active;

	/* we read EOF on this to transision, so the stream must be valid */
	hqxfile = devnull;
	line_end = line_start + HQXLINELEN;
	buf_end = buf_start + HQXBUFLEN;

	while (1) {
		total_bytes = 0;
		line_ptr = line_start;
		/* ensure that the line buffer is considered empty */
		/* why we use SKIP and not newline, I'm not sure */
		line_ptr[0] = SKIP;
		save_state = 0;
		save_run_length = 0;

		active = hqx_to_bin_hdr(type, &hqx_datalen, &hqx_rsrclen, same_file_only);
		if (active == 0)
			break;
		same_file_only = 1;
		type[4] = 0; /* set EOS */

		unpitting = unpit_flag && !strcmp(type, "PIT ");
		DEBUG && fprintf(debug,
			"DEBUG: unpit_flag=%d type=%s unpitting=%d\n",
			unpit_flag, type, unpitting);
		DEBUG && fflush(debug);
		if (unpitting) {
			mclose(&binfile, "binfile");
			/* Do not unlink files we did not open */
		   if (!info_only)
				unlink(binfname);
			bytes_read = total_bytes - (buf_end - buf_ptr);
			calc_crc = un_pit();
			bytes_read = total_bytes - (buf_end - buf_ptr) - bytes_read;
			if (bytes_read != hqx_datalen) {
				fprintf(verbose,
				    "Warning - Extraneous characters ignored in %s\n", binfname);
				fflush(verbose);
			}
		} else {
			calc_crc = hqx_to_bin_fork(hqx_datalen);
		}
		check_hqx_crc(calc_crc, "File data CRC mismatch in %s", binfname);

		calc_crc = hqx_to_bin_fork(hqx_rsrclen);
		check_hqx_crc(calc_crc, "File rsrc CRC mismatch in %s", binfname);

		if (!unpitting) {
			mclose(&binfile, "binfile");
		}

	}
	mclose(&hqxfile, "hqxfile");
}

re_hqx()
{
	word calc_crc;
	ulong hqx_datalen, hqx_rsrclen;
	extern char **hqxnames_left;
	extern int maxlines;

	line_end = line_start + HQXLINELEN;
	buf_end = buf_start + HQXBUFLEN;
	while (*hqxnames_left[0] != '-') {
		/* we write the trailer, so the stream must be valid */
		hqxfile = devnull;

		/*
       * We use the trick of setting our line_count at the limit to
       * force an immediate transition on overflow.
		 */
		line_count = maxlines;

		file_count = 0;
		line_ptr = line_start;
		*line_ptr++ = ':';
		strcpy((char*)line_end, "\n");
		buf_ptr = buf_start;
		save_state = 0;
		save_run_length = 1;

		bin_to_hqx_hdr(&hqx_datalen, &hqx_rsrclen);	/* calculates hqxfname */

		/*
       * Now that we have hqxfname, start the new file if
		 * not yet started.  We no longer wait until the output
		 * buffer overflows, since empty files with short names didn't overflow!
		 */

		if (file_count == 0)
			new_out_hqx_file();

		calc_crc = bin_to_hqx_fork(hqx_datalen);
		write_hqx_crc(calc_crc);

		calc_crc = bin_to_hqx_fork(hqx_rsrclen);
		write_hqx_crc(calc_crc);
		/*
       * To end a run and to get the last stray bits,
		 * temporarily add a char.
		 */
		*buf_ptr = !buf_ptr[-1];
		buf_ptr++;
		empty_hqxbuf();
		/* now toss any extra output character generated */
		if (save_state != 2)
			--line_ptr;

		/* if we're at the end of the line now, write it out */
		if (line_ptr == line_end) {
			fputs(LINE_START, hqxfile);
			line_ptr = line_start;
		}

		/* paste the trailing colon onto the end of the line */
		/* recall that line_ptr points into the line, not at the line */
		strcpy((char*)line_ptr, ":\n");
		
		/* and flush the output buffer */
		fputs(LINE_START, hqxfile);

		mclose(&hqxfile, "hqxfile");
		mclose(&binfile, "binfile");

	}
}

