/*================================================================
 * gusload -- load a GUS patch file on AWE32 sound driver
 *
 * Copyright (C) 1996,1997 Takashi Iwai
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *================================================================*/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/fcntl.h>
#ifdef linux
#  include <linux/soundcard.h>
#  include <linux/awe_voice.h>
#elif defined(__FreeBSD__)
#  include <machine/soundcard.h>
#  include <awe_voice.h>
#endif
#include "guspatch.h"
#include "seq.h"
#include "debugmsg.h"

void seq_load_gus(FILE *fd);
#define swapi(a) a
#define swapl(a) a

static void usage()
{
	fprintf(stderr, "gusload -- load GUS patch file on AWE32 sound driver\n");
	fprintf(stderr, "   version 0.3.2  copyright (c) 1996,1997 by Takashi Iwai\n");
	fprintf(stderr, "usage: sfxload [-Iixv] [-b bank] [-p preset] GUSpatch\n");
	fprintf(stderr, " -I : initialize AWE32\n");
	fprintf(stderr, " -i : reset all samples\n");
	fprintf(stderr, " -x : remove last samples\n");
	fprintf(stderr, " -v : verbose mode\n");
	fprintf(stderr, " -p preset : set instrument number (default is internal value)\n");
	fprintf(stderr, " -b bank : set bank number (default is 0)\n");
	fprintf(stderr, " -c chorus: set chorus effect (0-100)\n");
	fprintf(stderr, " -r reverb: set reverb effect (0-100)\n");
	exit(1);
}

static int preset = -1;
static int bankchange = -1;
int verbose;


void main(int argc, char **argv)
{
	FILE *fd;
	char *gusfile;
	int init_chip = 0, init = 0, remove = 0;
	int c;

	while ((c = getopt(argc, argv, "b:iIp:vx")) != -1) {
		switch (c) {
		case 'I':
			init_chip = !init_chip;
			break;
		case 'i':
			init = !init;
			break;
		case 'x':
			remove = !remove;
			break;
		case 'v':
			verbose++;
			break;
		case 'b':
			bankchange = atoi(optarg);
			break;
		case 'p':
			preset = atoi(optarg);
			break;
		default:
			usage();
			exit(1);
		}
	}
		
	if (argc - optind < 1) {
		usage();
		exit(1);
	}

	gusfile = argv[optind]; optind++;
	if ((fd = fopen(gusfile, "r")) == NULL) {
		fprintf(stderr, "can't open GUS patch file %s\n", gusfile);
		exit(1);
	}

	/* open awe sequencer device */
	seq_init(init);
	if (init_chip) {
		DEBUG(1,fprintf(stderr, "initializing AWE32..\n"));
		seq_initialize_chip();
	}
	if (remove) {
		DEBUG(1,fprintf(stderr, "removing samples..\n"));
		seq_remove_samples();
	}

	DEBUG(1,fprintf(stderr, "uploading samples..\n"));
	seq_load_gus(fd);

	DEBUG(1,fprintf(stderr, "done\n"));

	printf("DRAM memory left = %d kB\n", seq_mem_avail()/1024);

	/* close sequencer */
	seq_end();
	fclose(fd);

	exit(0);
}

static GusPatchHeader header;
static GusInstrument ins;
static GusLayerData layer;
static GusPatchData sample;

/*
 * load voice record to the awe driver
 */
void seq_load_gus(FILE *fp)
{
	int j, len;
	struct patch_info *patch;

	/* Unix based routines, assume big-endian machine */
	/* read header */
	DEBUG(1,fprintf(stderr, "reading header\n"));
	fread(&header.header, sizeof(header.header), 1, fp);
	fread(&header.gravis_id, sizeof(header.gravis_id), 1, fp);
	fread(&header.description, sizeof(header.description), 1, fp);
	fread(&header.instruments, sizeof(header.instruments), 1, fp);
	fread(&header.voices, sizeof(header.voices), 1, fp);
	fread(&header.channels, sizeof(header.channels), 1, fp);
	fread(&header.wave_forms, sizeof(header.wave_forms), 1, fp);
	header.wave_forms = swapi( header.wave_forms );
	fread(&header.master_volume, sizeof(header.master_volume), 1, fp);
	header.master_volume = swapi( header.master_volume );
	fread(&header.data_size, sizeof(header.data_size), 1, fp);
	header.data_size = swapl( header.data_size );
	fread(&header.reserved, sizeof(header.reserved), 1, fp);
	/* read instrument header */
	DEBUG(1,fprintf(stderr, "reading instrument\n"));
	fread(&ins.instrument, sizeof(ins.instrument), 1, fp);
	fread(&ins.instrument_name, sizeof(ins.instrument_name), 1, fp);
	fread(&ins.instrument_size, sizeof(ins.instrument_size), 1, fp);
	ins.instrument_size = swapl( ins.instrument_size );
	fread(&ins.layers, sizeof(ins.layers), 1, fp);
	fread(&ins.reserved, sizeof(ins.reserved), 1, fp);
	/* read layer header */
	DEBUG(1,fprintf(stderr, "reading layer\n"));
	fread(&layer.layer_duplicate, sizeof(layer.layer_duplicate), 1, fp);
	fread(&layer.layer, sizeof(layer.layer), 1, fp);
	fread(&layer.layer_size, sizeof(layer.layer_size), 1, fp);
	layer.layer_size = swapl( layer.layer_size );
	fread(&layer.samples, sizeof(layer.samples), 1, fp);
	fread(&layer.reserved, sizeof(layer.reserved), 1, fp);
	if (strcmp(header.gravis_id, "ID#000002") != 0) {
		fprintf(stderr, "Not a GUS patch file\n");
		exit(1);
	}

	DEBUG(0,fprintf(stderr, "data size = %d\n", (int)header.data_size));

	/* read sample information */
	for (j = 0; j < layer.samples; j++) {
		/* read sample information */
		DEBUG(1,fprintf(stderr, "reading sample(%d)\n", j));
		fread(&sample.wave_name, sizeof(sample.wave_name), 1, fp);
		fread(&sample.fractions, sizeof(sample.fractions), 1, fp);
		fread(&sample.wave_size, sizeof(sample.wave_size), 1, fp);
		sample.wave_size = swapl( sample.wave_size );
		fread(&sample.start_loop, sizeof(sample.start_loop), 1, fp);
		fread(&sample.end_loop, sizeof(sample.end_loop), 1, fp);
		fread(&sample.sample_rate, sizeof(sample.sample_rate), 1, fp);
		sample.sample_rate = swapi( sample.sample_rate );
		fread(&sample.low_frequency, sizeof(sample.low_frequency), 1, fp);
		fread(&sample.high_frequency, sizeof(sample.high_frequency), 1, fp);
		fread(&sample.root_frequency, sizeof(sample.root_frequency), 1, fp);
		fread(&sample.tune, sizeof(sample.tune), 1, fp);
		fread(&sample.balance, sizeof(sample.balance), 1, fp);
		fread(&sample.envelope_rate, sizeof(sample.envelope_rate), 1, fp);
		fread(&sample.envelope_offset, sizeof(sample.envelope_offset), 1, fp);
		fread(&sample.tremolo_sweep, sizeof(sample.tremolo_sweep), 1, fp);
		fread(&sample.tremolo_rate, sizeof(sample.tremolo_rate), 1, fp);
		fread(&sample.tremolo_depth, sizeof(sample.tremolo_depth), 1, fp);
		fread(&sample.vibrato_sweep, sizeof(sample.vibrato_sweep), 1, fp);
		fread(&sample.vibrato_rate, sizeof(sample.vibrato_rate), 1, fp);
		fread(&sample.vibrato_depth, sizeof(sample.vibrato_depth), 1, fp);
		fread(&sample.modes, sizeof(sample.modes), 1, fp);
		fread(&sample.scale_frequency, sizeof(sample.scale_frequency), 1, fp);
		fread(&sample.scale_factor, sizeof(sample.scale_factor), 1, fp);
		sample.scale_factor = swapi( sample.scale_factor );
		fread(&sample.reserved, sizeof(sample.reserved), 1, fp);

		DEBUG(1,fprintf(stderr, "-- sample len = %d\n",
				(int)sample.wave_size));
		/* allocate sound driver patch data */
		len = sizeof(struct patch_info) + sample.wave_size - 1;
		patch = (struct patch_info*)calloc(len, 1);
		if (patch == NULL) {
			fprintf(stderr, "can't allocate patch buffer\n");
			exit(1);
		}
		patch->key = GUS_PATCH;
		patch->device_no = awe_dev;
		if (preset >= 0)
			patch->instr_no = preset;
		else
			patch->instr_no = ins.instrument;
		DEBUG(0,fprintf(stderr,"-- preset=%d\n", patch->instr_no));
		patch->mode = sample.modes;
		patch->len = sample.wave_size;
		patch->loop_start = sample.start_loop;
		patch->loop_end = sample.end_loop;
		patch->base_freq = sample.sample_rate;
		patch->base_note = sample.root_frequency;
		patch->high_note = sample.high_frequency;
		patch->low_note = sample.low_frequency;
		DEBUG(0,fprintf(stderr,"-- base freq=%d, note=%d (%d-%d)\n",
		      (int)patch->base_freq, (int)patch->base_note,
		      (int)patch->low_note, (int)patch->high_note));
		patch->panning = sample.balance;
		patch->detuning = sample.tune;
		memcpy(patch->env_rate, sample.envelope_rate, 6);
		memcpy(patch->env_offset, sample.envelope_offset, 6);
		if (sample.tremolo_rate > 0 && sample.tremolo_depth > 0)
			patch->mode |= WAVE_TREMOLO;
		patch->tremolo_sweep = sample.tremolo_sweep;
		patch->tremolo_rate = sample.tremolo_rate;
		patch->tremolo_depth = sample.tremolo_depth;
		DEBUG(0,fprintf(stderr,"-- tremolo rate=%d, depth=%d\n",
		      patch->tremolo_rate, patch->tremolo_depth));
		if (sample.vibrato_rate > 0 && sample.vibrato_depth > 0)
			patch->mode |= WAVE_VIBRATO;
		patch->vibrato_sweep = sample.vibrato_sweep;
		patch->vibrato_rate = sample.vibrato_rate;
		patch->vibrato_depth = sample.vibrato_depth;
		DEBUG(0,fprintf(stderr,"-- vibrato rate=%d, depth=%d\n",
		      patch->vibrato_rate, patch->vibrato_depth));
		patch->scale_frequency = sample.scale_frequency;
		patch->scale_factor = sample.scale_factor;
		patch->volume = header.master_volume;
#if SOUND_VERSION > 301
		patch->fractions = sample.fractions;
#endif
		/* allocate raw sample buffer */
		DEBUG(1,fprintf(stderr, "-- reading wave data\n"));
		fread(patch->data, 1, patch->len, fp);

		/* if -b option is specified, change the bank value */
		if (bankchange >= 0) {
			DEBUG(1,fprintf(stderr, "-- set bank number\n"));
			seq_set_gus_bank(bankchange);
		}

		DEBUG(1,fprintf(stderr, "-- transferring\n"));
		if (seq_load_patch(patch, len) == -1) {
			fprintf(stderr, "[Loading GUS %d]\n", j);
			perror("Error in loading info");
			exit(1);
		}

		/* free temporary buffer */
		free(patch);
	}
}

