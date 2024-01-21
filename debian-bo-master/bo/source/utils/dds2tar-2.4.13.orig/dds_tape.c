
/*
 * This file is part of dds2tar.
 * Copyright by J"org Weule
 */

#include <stdlib.h>
#include <stdio.h>		/* fpirntf() */
#include <errno.h>		/* perror() */
#include <sys/mtio.h>		/* MTSEEK ... */
#include <sys/ioctl.h>		/* ioctl() */
#include <unistd.h>		/* close() exit() */
#include "dds_tape.h"

#include "dds2tar.h"
#include "dds_tape.h"

/*
 * rewind the last block
 */
int dds_bsr(void){
	struct mtop p ;
	p.mt_op = MTBSR ;
	p.mt_count = 1 ;
	return ioctl(device,MTIOCTOP,&p);
}

/*
 * To read one line of the device and check for errors, we use the following
 * code. If a filemark is read, we try the block again.
 *
 * Note that cur_blkno and next_blkno is changed.
 */
void
dds_read_next_block(void)
{
	int     err = 0;

#ifdef HPDAT
	if (next_blkno == -1)
#endif
		next_blkno = dds_getpos(device);
	cur_blkno = next_blkno;
	cur_n = 0;
#ifdef DDS_TRACE
	fprintf(stderr, "begin --> read_next_block()\n");
#endif
	do {
		/* If you write a tape with 'tar -b 256' on SunOS, reading */
		/* the tape needs the following workaround. SunOS splits the */
		/* block into three tape blocks of 65534, 65534 and 4 Bytes. */
		/* dds2tar is reading the tree blocks and the number is */
		/* the one of the first block. You can not use the tar */
		/* listing block is this case, because the calculation of the */
		/* right will fail. Note that 'dds2tar --dds-dd | tar ft -' */
		/* will show the index of the archive, 'tar t' fails. */
		/* */
		/* --dds-dd is in experimental state, so may be I will change */
		/* this. */
#if ( ST_BUFFER_BLOCKS > 32 )
		do {
			err = read(device, ((char *) cur_block) + cur_n,
					    buf_n - cur_n);
			if (err > 0) next_blkno++, cur_n += err;
		} while ((err > 0) && (cur_n < buf_n) && ((cur_n & 0x1ff) != 0));
#else
		err = read(device, cur_block, buf_n);
		if (err > 0) next_blkno++, cur_n += err ;
#endif
		if ((cur_n & 0x1ff) != 0) {
			fprintf(stderr, "not a tar archive, record size is"
				" %d bytes \n", cur_n);
			exit(4);
		}
		/*
		 * We are done, if err >= 0.
		 */
		if ( err >= 0 ) break ;

		/*
		 * If err < 0 ... exit or decrease the buffer ...
		 */
		if ((errno != EOVERFLOW) || (buf_n == 1)) {
			perror("dds2tar");
			exit(4);
		}
		buf_n >>= 1;
		fprintf(stderr,"decreasing buffer to %d bytes\n",buf_n);
		if (buf_n == 1) exit(4);
	} while ( 1 );
	cur_bs = cur_n >> 9;
#ifdef DDS_TRACE
	fprintf(stderr, "end ----> read_next_block()\n");
#endif
}

void
dds_read_block(void)
{
	do
		dds_read_next_block();
	while (cur_n == 0);
}

int
dds_getpos(int const dev)
{
	struct mtpos pos;
	int     i;

	i = ioctl(dev, MTIOCPOS, &pos);
	if (i != 0) {
		perror("dds2tar: ioctl MTIOCPOS");
		close(dev);
		exit(11);
	}
	i = pos.mt_blkno;
	if (i < 0) {
		fprintf(stderr, "dds2tar: eom detected ? blkno = %d \n", i);
		close(dev);
		exit(12);
	}
	return i;
}

int
dds_getloc(int const dev)
{
	struct mtpos pos;
	int     i;

	i = ioctl(dev, MTIOCPOS, &pos);
	if (i != 0) {
		perror("dds2tar: ioctl MTIOCLOC");
		close(dev);
		exit(11);
	}
	i = pos.mt_blkno;
	if (i < 0) {
		fprintf(stderr, "dds2tar: eom detected ? blkno = %d \n", i);
		close(dev);
		exit(12);
	}
	return i ;
}

int
dds_seek(int const dev, int const blkno)
{
	struct mtop op;
	int     i;

	op.mt_op = MTSEEK;
	op.mt_count = blkno;
	i = ioctl(dev, MTIOCTOP, &op);
	if (i != 0) {
		perror("ioctl SEEK");
		close(dev);
		exit(13);
	}
	return 0;
}

/*
 * This file is part of dds2tar.
 * Copyright by J"org Weule
 */

#ifdef HPDAT
#include <stdlib.h>		/* malloc() */
#include <stdio.h>		/* printf() */
#include <sys/ioctl.h>		/* ioctl() */
#include <string.h>		/* memset() bcopy() */
#include <unistd.h>		/* exit() */
#include "dds_tape.h"

typedef unsigned char byte;

typedef struct {
	int     inlen;
	int     outlen;
	byte    buf[1024];
}

ioctl_arg;

static void
copy_page(ioctl_arg * const arg, int const i)
{

	static struct {
		char const *const text;
		int const inlen;
		int const outlen;
		byte const buf[32];
	}
	const   comp_page[7] =
	{
		{
			"set_comp_off", 26, 128,
			{
				0x15, 0x10, 0x00, 0x00,
				0x14, 0x00, 0x00, 0x00,
				0x10, 0x00, 0x0f, 0x0e,
				0x40, 0x80, 0x00, 0x00,
				0x00, 0x20, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00
			}
		}
		,
		{
			"set_cpmp_on", 26, 128,
			{
				0x15, 0x10, 0x00, 0x00,
				0x14, 0x00, 0x00, 0x00,
				0x10, 0x00, 0x0f, 0x0e,
				0xc0, 0x80, 0x00, 0x00,
				0x00, 0x20, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00
			}
		}
		,
		{
			"get_comp", 6, 128,
			{
				0x1a, 0x00, 0x0f, 0x00, 0x40, 0x00
			}
		}
		,
		{
			"log_comp", 10, 128,
			{
				0x4d, 0x00, 0x79, 0x00,
				0x00, 0x00, 0x00, 0x00,
				0x80, 0x00
			}
		}
		,
		{
			"load_tape", 6, 128,
			{
				0x1b, 0x00, 0x00, 0x00, 0x01, 0x00
			}
		}
		,
		{
			"unload_tape", 6, 128,
			{
				0x1b, 0x00, 0x00, 0x00, 0x00, 0x00
			}
		}
		,
		{
			"mode_sense", 6, 128,
			{
				0x1a, 0x00, 0x00, 0x00, 0x0C, 0x00
			}
		}
	};

	arg->inlen = comp_page[i].inlen;
	arg->outlen = comp_page[i].outlen;
	memset(arg->buf, 0, 1024);
	bcopy(comp_page[i].buf, arg->buf, arg->inlen);
}

static int
print_error(byte const *const b, int len)
{

	static char const *const sense_key[] =
	{
		"no sense", "recovered error", "not ready", "medium error",
		"hardware error", "illegal request", "unit attention",
		"data protect", "blank check", "vendor specific",
		"copy aborted", "aborted command", "equal",
		"volume overflow", "miscompare", "reserved"
	};

	int     i;

	if (len > 32)
		len = 8 + b[7];
	printf(" err:                ");
	for (i = 0; i < len; i++)
		printf(" %2X", b[i]);
	printf("\n");
	printf("error sense: %s\n", sense_key[b[2] & 0xf]);
	printf("asc + ascq = %2x %2x\n", b[12], b[13]);
	return 0;
}

void
set_compression_mode(int dev, int const comp_mode)
{
	ioctl_arg *arg;

	if (comp_mode < 0 || comp_mode > 5)
		return;
	/*
	 * Allocate memory.
	 */
	arg = malloc(sizeof (ioctl_arg));
	if (arg == NULL) {
		fprintf(stderr, "dds2tar: not enough memory\n");
		exit(15);
	}
	/*
	 * do ioctl
	 */
	copy_page(arg, comp_mode);
	/*	arg->buf[12] = (comp_mode << 7) | 0x40; */
	if (ioctl(dev, 1, arg) != 0) {
		close(dev);
		print_error(arg->buf, arg->inlen);
		exit(16);
	}
	if (comp_mode == DDSCM_QUERY) {
		byte   *b;

		b = arg->buf + 12;
		fprintf(stderr, "data compression enable %d\n", b[2] >> 7);
	} else if (comp_mode == DDSCM_LOG) {
		int     i, j;
		byte   *b;

		b = arg->buf + 0;
		for (i = 0; i < b[3];) {

			static char const *const tab39[] =
			{
				NULL,
				NULL,
				NULL,
				NULL,
				NULL,
				"kilobytes to data compression",
				"kilobytes from data compression",
				"kilobytes to tape",
				"kilobytes from tape",
				NULL,
				NULL,
				NULL};

			byte   *p = b + i + 4;
			int     n, k, c;

			c = (p[0] << 8) + p[1];
			n = p[3];
			i += 4 + n;
			p += 4;
			j = 0;
			for (k = 0; k < n; k++)
				j = (j << 8) + p[k];

			if ((c < 12) && (tab39[c] != NULL))
				printf("%s: %d\n", tab39[c], j);
		}
	}
}

#endif
