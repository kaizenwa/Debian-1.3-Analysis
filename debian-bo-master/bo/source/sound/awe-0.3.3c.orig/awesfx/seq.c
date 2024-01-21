/*================================================================
 * seq.c
 *	sequencer control routine for awe sound driver
 *	written by Takashi Iwai
 *================================================================*/

#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <math.h>
#include <fcntl.h>
#ifdef __FreeBSD__
#  include <machine/soundcard.h>
#  include <awe_voice.h>
#elif defined(linux)
#  include <linux/soundcard.h>
#  include <linux/awe_voice.h>
#endif
#include "debugmsg.h"


#define SEQUENCER_DEV	"/dev/sequencer"
SEQ_DEFINEBUF(128);
int seqfd;

void seqbuf_dump()
{
	if (_seqbufptr)
		if (write(seqfd, _seqbuf, _seqbufptr) == -1) {
			perror("write " SEQUENCER_DEV);
			exit(-1);
		}
	_seqbufptr = 0;
}


#define MAX_CARDS	16
int nrsynths;
struct synth_info card_info[MAX_CARDS];
int awe_dev;
int max_synth_voices;

void seq_init(int reset)
{
	int i;

	if ((seqfd = open(SEQUENCER_DEV, O_WRONLY, 0)) < 0) {
		perror("open " SEQUENCER_DEV);
		exit(1);
	}

	if (ioctl(seqfd, SNDCTL_SEQ_NRSYNTHS, &nrsynths) == -1) {
		fprintf(stderr, "there is no soundcard\n");
		exit(1);
	}
	awe_dev = -1;
	for (i = 0; i < nrsynths; i++) {
		card_info[i].device = i;
		if (ioctl(seqfd, SNDCTL_SYNTH_INFO, &card_info[i]) == -1) {
			fprintf(stderr, "cannot get info on soundcard\n");
			perror(SEQUENCER_DEV);
			exit(1);
		}
		card_info[i].device = i;
		if (card_info[i].synth_type == SYNTH_TYPE_SAMPLE
		    && card_info[i].synth_subtype == SAMPLE_TYPE_AWE32)
			awe_dev = i;
	}

	if (awe_dev < 0) {
		fprintf(stderr, "No AWE synth device is found\n");
		exit(1);
	}

	if (reset) {
		DEBUG(0,fprintf(stderr, "resetting samples..\n"));
		if (ioctl(seqfd, SNDCTL_SEQ_RESETSAMPLES, &awe_dev) == -1) {
			perror("Sample reset");
			exit(1);
		}
	}
	max_synth_voices = card_info[awe_dev].nr_voices;
}

void seq_end(void)
{
	SEQ_DUMPBUF();
	ioctl(seqfd, SNDCTL_SEQ_SYNC);
	close(seqfd);
}

void seq_remove_samples(void)
{
	AWE_REMOVE_LAST_SAMPLES(seqfd, awe_dev);
	SEQ_DUMPBUF();
}

void seq_initialize_chip(void)
{
	AWE_INITIALIZE_CHIP(seqfd, awe_dev);
	SEQ_DUMPBUF();
	
}

void seq_set_gus_bank(int bank)
{
	AWE_SET_GUS_BANK(awe_dev, bank);
	SEQ_DUMPBUF();
}

int seq_load_patch(void *patch, int len)
{
	SEQ_DUMPBUF();
	return write(seqfd, patch, len);
}

int seq_mem_avail(void)
{
	int mem_avail = awe_dev;
	ioctl(seqfd, SNDCTL_SYNTH_MEMAVL, &mem_avail);
	return mem_avail;
}

