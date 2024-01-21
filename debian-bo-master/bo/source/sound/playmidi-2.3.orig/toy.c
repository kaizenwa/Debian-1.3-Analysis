/************************************************************************
 * toy.c - take input from midi device, warp it some way, output it,
 * and save as format 0 midi file.
 * NOTE: THIS PROGRAM IS INCOMPLETE.  IT IS NOT MADE BY DEFAULT.
 * This code was written by by Nathan Laredo (laredo@gnu.ai.mit.edu)
 * Source code may be freely distributed in unmodified form.
 *************************************************************************/
#include <stdio.h>
#include <getopt.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/soundcard.h>

#define SEQUENCER_DEV	"/dev/sequencer"
#define SEQUENCERBLOCKSIZE	128

/*
 * The following are if you have more than one midi device on
 * your system as I do.   My Roland piano is on the 2nd midi port (GUS), and
 * my Korg daughterboard is on the 1st midi port (SB16).
 */

#define OUT_DEV 0
#define IN_DEV 1

SEQ_DEFINEBUF(SEQUENCERBLOCKSIZE);
unsigned char inputbuf[SEQUENCERBLOCKSIZE];
int seqfd;

/* indexed by high nibble of command */
int cmdlen[16] =
{0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 1, 1, 2, 0};

void seqbuf_dump()
{
    if (_seqbufptr)
	if (write(seqfd, _seqbuf, _seqbufptr) == -1) {
	    perror("write " SEQUENCER_DEV);
	    exit(-1);
	}
    _seqbufptr = 0;
}

int seqread()
{
    int count;

    if ((count = read(seqfd, inputbuf, 4)) == -1) {
	perror("read " SEQUENCER_DEV);
	exit(-1);
    }
    return count;
}

int main(argc, argv)
int argc;
char **argv;
{
    extern char *optarg;
    extern int optind;
    int i, transpose = 0, channel = 0, program = 0;
    unsigned char mid[8], cmd = 0;
    unsigned int oldticks = 0, ticks = 0, db = 0;

    while ((i = getopt(argc, argv, "c:p:t:")) != -1)
	switch (i) {
	case 'c':
	    channel = atoi(optarg);
	    break;
	case 'p':
	    program = atoi(optarg);
	    break;
	case 't':
	    transpose = atoi(optarg);
	    break;
	default:
	    fprintf(stderr, "usage: %s [-t semitones]"
		    " [-c channel] [-p program]\n", argv[0]);
	    exit(1);
	    break;
	}

    if ((seqfd = open(SEQUENCER_DEV, O_RDWR, 0)) < 0) {
	perror("open " SEQUENCER_DEV);
	exit(-1);
    }
    /*
     * the following controls the input device to only sound notes
     * originating from the midi port, and keypresses go to midi port
     */
    SEQ_START_TIMER();
    SEQ_MIDIOUT(IN_DEV, 0xb0);	/* channel 0 control */
    SEQ_MIDIOUT(IN_DEV, 122);	/* local control */
    SEQ_MIDIOUT(IN_DEV, 0);	/* off */
    if (program >= 1 && program <= 128) {
	if (channel < 1 || channel > 16)
	   channel = 1;
	SEQ_MIDIOUT(OUT_DEV, 0xc0 | (channel - 1));
	SEQ_MIDIOUT(OUT_DEV, program - 1);
    }
    SEQ_DUMPBUF();
    inputbuf[4] = 0;	/* for converting 3-byte timer ticks */
    while (1) {
	if (seqread()) {
	    if (inputbuf[0] == TMR_WAIT_ABS)
		ticks = (*(unsigned int *) &inputbuf[1]);
	    if (ticks != oldticks) {
		fprintf(stderr, "\n%d:%02d.%02d  ", ticks / 6000,
			(ticks / 100) % 60, ticks % 100);
		oldticks = ticks;
	    }
	    if (inputbuf[0] == SEQ_MIDIPUTC) {
		if (inputbuf[1] & 0x80)
		    cmd = (mid[db = 0] = inputbuf[1]) & 0xf0;
		else
		    mid[db] = inputbuf[1];
		db++;
		if (db == cmdlen[cmd >> 4] + 1) {
		    if (transpose && (cmd == 0x80 || cmd == 0x90))
			mid[1] += transpose;
		    for (i = 0; i < db; i++) {
			if (channel >= 1 && channel <= 16)
			    mid[0] = cmd | (channel - 1);
			fprintf(stderr, "%02x", mid[i]);
			SEQ_MIDIOUT(OUT_DEV, mid[i]);
			SEQ_DUMPBUF();
		    }
		    fprintf(stderr," ");
		    db = 1;
		}
	    }
	}
    }
    close(seqfd);
    exit(0);
}
/* end of file */
