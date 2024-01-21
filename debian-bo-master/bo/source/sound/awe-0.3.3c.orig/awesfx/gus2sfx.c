/*================================================================
 * gus2sfx -- convert GUS compatible sample to SFX file
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
#include <string.h>
#include <stdlib.h>
#include <sys/fcntl.h>
#include <math.h>
#ifdef __FreeBSD__
#  include <machine/soundcard.h>
#else
#  include <linux/soundcard.h>
#endif
#include "guspatch.h"
#include "sfx.h"
#include "awe_parm.h"
#include "debugmsg.h"

/*----------------------------------------------------------------*/

#define calc_note(freq)	(awe_mHz_to_abscent(freq) / 100)
#define calc_tune(freq)	(awe_mHz_to_abscent(freq) % 100)
static int calc_gus_cB(int val);
static int calc_gus_envelope_rate(int rate);
static int calc_gus_envelope_time(int rate, int start, int end);
static unsigned char calc_gus_sustain(int val);

static void gus_setup_envelope(int j, awe_voice_info *info);
static void gus_setup_tremolo(int j, awe_voice_info *info);
static void gus_setup_vibrato(int j, awe_voice_info *info);

static void usage();
static void read_gus_file(FILE *fp);
static void convert_gus(int bank, int preset, FILE *fp);


int verbose = 0;

static int drum_mode;
static VoiceInfo vinfo_rec;

/*----------------------------------------------------------------*/

static void usage()
{
	fprintf(stderr, "gus2sfx -- convert GUS patch to SFX file\n");
	fprintf(stderr, "usage: gus2sfx [-dv] gusfile sfxfile bank preset\n");
	fprintf(stderr, "  -d: drum mode, bank=drumset, preset=key number\n");
	fprintf(stderr, "  -v: verbose\n");
	exit(1);
}


void main(int argc, char **argv)
{
	FILE *fd;
	int bank, preset;
	int c;

	while ((c = getopt(argc, argv, "vd")) != -1) {
		switch (c) {
		case 'v':
			verbose++;
			break;
		case 'd':
			drum_mode = 1;
			break;
		default:
			usage();
			exit(1);
		}
	}
		
	if (argc - optind < 4) {
		usage();
		exit(1);
	}

	if ((fd = fopen(argv[optind], "r")) == NULL) {
		fprintf(stderr, "can't open GUS patch file %s\n", argv[optind]);
		exit(1);
	}
	optind++;
	read_gus_file(fd);
	fclose(fd);

	if ((fd = fopen(argv[optind], "w")) == NULL) {
		fprintf(stderr, "can't open SFX file %s for write\n", argv[optind]);
		exit(1);
	}
	optind++;
	bank = atoi(argv[optind]); optind++;
	preset = atoi(argv[optind]); optind++;
	
	convert_gus(bank, preset, fd);

	sfx_write_file(&vinfo_rec, fd);

	fclose(fd);
	exit(0);
}

/*----------------------------------------------------------------*/

static GusPatchHeader header;
static GusInstrument ins;
static GusLayerData layer;
static GusPatchData *sample;
static unsigned short **waves;

#define READ(var,fp)	fread(&var,sizeof(var),1,fp)

#define swapi(a) a
#define swapl(a) a


static void read_gus_file(FILE *fp)
{
	unsigned char *buf;
	unsigned short *sp;
	int i, j;
	int pats, bidir = 0;

	/* Unix based routines, assume big-endian machine */
	/* read header */
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
	fread(&ins.instrument, sizeof(ins.instrument), 1, fp);
	fread(&ins.instrument_name, sizeof(ins.instrument_name), 1, fp);
	fread(&ins.instrument_size, sizeof(ins.instrument_size), 1, fp);
	ins.instrument_size = swapl( ins.instrument_size );
	fread(&ins.layers, sizeof(ins.layers), 1, fp);
	fread(&ins.reserved, sizeof(ins.reserved), 1, fp);
	/* read layer header */
	fread(&layer.layer_duplicate, sizeof(layer.layer_duplicate), 1, fp);
	fread(&layer.layer, sizeof(layer.layer), 1, fp);
	fread(&layer.layer_size, sizeof(layer.layer_size), 1, fp);
	layer.layer_size = swapl( layer.layer_size );
	fread(&layer.samples, sizeof(layer.samples), 1, fp);
	fread(&layer.reserved, sizeof(layer.reserved), 1, fp);
	if (strcmp(header.gravis_id, "ID#000002") != 0) {
		fprintf(stderr, "illegal id: %s\n", header.gravis_id);
		exit(1);
	}

	DEBUG(0,fprintf(stderr, "data size = %d\n", (int)header.data_size));

	sample = (GusPatchData*)calloc(layer.samples, sizeof(GusPatchData));
	waves = (unsigned short**)calloc(layer.samples, sizeof(short*));

	/* read sample information */
	for (j = 0; j < layer.samples; j++) {
		/* read sample information */
		fread(&sample[j].wave_name, sizeof(sample[j].wave_name), 1, fp);
		fread(&sample[j].fractions, sizeof(sample[j].fractions), 1, fp);
		fread(&sample[j].wave_size, sizeof(sample[j].wave_size), 1, fp);
		sample[j].wave_size = swapl( sample[j].wave_size );
		fread(&sample[j].start_loop, sizeof(sample[j].start_loop), 1, fp);
		fread(&sample[j].end_loop, sizeof(sample[j].end_loop), 1, fp);
		fread(&sample[j].sample_rate, sizeof(sample[j].sample_rate), 1, fp);
		sample[j].sample_rate = swapi( sample[j].sample_rate );
		fread(&sample[j].low_frequency, sizeof(sample[j].low_frequency), 1, fp);
		fread(&sample[j].high_frequency, sizeof(sample[j].high_frequency), 1, fp);
		fread(&sample[j].root_frequency, sizeof(sample[j].root_frequency), 1, fp);
		fread(&sample[j].tune, sizeof(sample[j].tune), 1, fp);
		fread(&sample[j].balance, sizeof(sample[j].balance), 1, fp);
		fread(&sample[j].envelope_rate, sizeof(sample[j].envelope_rate), 1, fp);
		fread(&sample[j].envelope_offset, sizeof(sample[j].envelope_offset), 1, fp);
		fread(&sample[j].tremolo_sweep, sizeof(sample[j].tremolo_sweep), 1, fp);
		fread(&sample[j].tremolo_rate, sizeof(sample[j].tremolo_rate), 1, fp);
		fread(&sample[j].tremolo_depth, sizeof(sample[j].tremolo_depth), 1, fp);
		fread(&sample[j].vibrato_sweep, sizeof(sample[j].vibrato_sweep), 1, fp);
		fread(&sample[j].vibrato_rate, sizeof(sample[j].vibrato_rate), 1, fp);
		fread(&sample[j].vibrato_depth, sizeof(sample[j].vibrato_depth), 1, fp);
		fread(&sample[j].modes, sizeof(sample[j].modes), 1, fp);
		fread(&sample[j].scale_frequency, sizeof(sample[j].scale_frequency), 1, fp);
		fread(&sample[j].scale_factor, sizeof(sample[j].scale_factor), 1, fp);
		sample[j].scale_factor = swapi( sample[j].scale_factor );
		fread(&sample[j].reserved, sizeof(sample[j].reserved), 1, fp);

		/* allocate raw sample buffer */
		buf = (unsigned char*)malloc(sample[j].wave_size);
		if (buf == NULL) {
			fprintf(stderr, "can't malloc buffer\n");
			exit(1);
		}
		fread(buf, sample[j].wave_size, 1, fp);

		/* backward looping */
		if (sample[j].modes & GUS_MODE_LOOP_BACK) {
			long t = sample[j].start_loop;
			DEBUG(1,fprintf(stderr, "[%d] loop-back\n", j));
			sample[j].start_loop = sample[j].wave_size - sample[j].end_loop;
			sample[j].end_loop = sample[j].wave_size - t;
			sample[j].modes &= ~GUS_MODE_LOOP_BACK;
			sample[j].modes |= GUS_MODE_LOOP;
		}

		/* 16bit sample */
		if (sample[j].modes & GUS_MODE_16BIT) {
			sample[j].wave_size /= 2;
			sample[j].start_loop /= 2;
			sample[j].end_loop /= 2;
		}

		pats = 48;  /* default blank loop size */
		/* bidirectional (ping-pong) loop */
		if (sample[j].modes & GUS_MODE_LOOP_BIDIR) {
			DEBUG(1,fprintf(stderr, "[%d] bidirecion-loop\n", j));
			bidir = sample[j].end_loop - sample[j].start_loop;
			pats += bidir;
			sample[j].modes |= GUS_MODE_LOOP;
		}

		/* allocate converted sample buffer for AWE */
		sp = (unsigned short*)malloc((sample[j].wave_size + pats) * 2);
		if (sp == NULL) {
			fprintf(stderr, "can't malloc buffer\n");
			exit(1);
		}
		waves[j] = sp;

		/* copy from raw samples */
		if (sample[j].modes & GUS_MODE_16BIT) /* 16bit */
			memcpy(sp, buf, sample[j].wave_size * 2);
		else {
			/* 8bit -> 16bit */
			for (i = 0; i < sample[j].wave_size; i++)
				sp[i] = buf[i] << 8;
		}

		if (sample[j].modes & GUS_MODE_UNSIGNED) {
			/* unsigned -> signed */
			for (i = 0; i < sample[j].wave_size; i++)
				sp[i] ^= 0x8000;
		}

		if (sample[j].modes & GUS_MODE_LOOP_BIDIR) {
			/* bidirectional loop; copy reverse loop samples */
			int dst = sample[j].end_loop + bidir - 1;
			int src = sample[j].start_loop;
			for (i = 0; i < bidir; i++) {
				sp[dst] = sp[src];
				dst--;
				src++;
			}
			sample[j].end_loop += bidir;
			sample[j].wave_size += bidir;
		}

		/* append blank loop samples for one-shot sample */
		memset(sp + sample[j].wave_size, 0, 48 * 2);

		DEBUG(0,fprintf(stderr, "[%d] loop=%x-%x size=%x\n", j,
				(int)sample[j].start_loop,
				(int)sample[j].end_loop,
				(int)sample[j].wave_size));

		/* smooth for looping; not working well.. */
		/*
		if (sample[j].modes & GUS_MODE_LOOP) {
			int stp = sample[j].start_loop;
			int enp = sample[j].end_loop;
			for (i = 1; i <= 8; i++) {
				sp[enp-i] = sp[stp-i];
			}
			for (i = 9; i <= 16; i++) {
				sp[enp-i] = sp[enp-8] + (sp[enp-4] - sp[enp-8]) * (8-i) / 4;
			}
			for (i = 1; i <= 8; i++)
				sp[enp+i] = sp[stp+i];
		}
		*/
		free(buf);
	}
}

/*----------------------------------------------------------------*/

static void convert_gus(int bank, int preset, FILE *fp)
{
	int j;
	int nsize;
	VoiceInfo *vinfo = &vinfo_rec;

	vinfo->sfx_version = 2;
	vinfo->nwaves = layer.samples;
	vinfo->sample = (awe_sample_info*)calloc(AWE_SAMPLE_INFO_SIZE *
						 vinfo->nwaves, 1);
	/* load sample data */
	DEBUG(0,fprintf(stderr, "samples..\n"));
	nsize = 0;
	for (j = 0; j < layer.samples; j++) {
		int datasize = sample[j].wave_size + 48;
		awe_sample_info *smp = vinfo->sample + j;

		smp->sf_id = 0;
		smp->sample = j;
		smp->start = 0;
		smp->end = sample[j].wave_size;
		if (sample[j].modes & GUS_MODE_LOOP) {
			smp->loopstart = sample[j].start_loop;
			smp->loopend = sample[j].end_loop;
		} else {
			smp->loopstart = sample[j].wave_size + 8;
			smp->loopend = sample[j].wave_size + 40;
		}
		smp->size = datasize;
		nsize += datasize;
		DEBUG(0,fprintf(stderr, "(%d) sample size = %d\n", j, (int)smp->size));
	}

	/* load voice info */
	DEBUG(0,fprintf(stderr, "loading infos..\n"));
	vinfo->nvoices = layer.samples;
	vinfo->voice = (awe_voice_rec**)calloc(sizeof(awe_voice_rec*) * vinfo->nvoices, 1);
	for (j = 0; j < layer.samples; j++) {
		awe_voice_rec *vrec;
		awe_voice_info *info;

		vrec = (awe_voice_rec*)calloc(AWE_VOICE_REC_SIZE +
					      AWE_VOICE_INFO_SIZE, 1);
		vinfo->voice[j] = vrec;
		if (drum_mode) {
			vrec->bank = 128;
			vrec->instr = bank;
		} else {
			vrec->bank = bank;
			vrec->instr = preset;
		}
		vrec->nvoices = 1;

		info = vrec->info;
		awe_init_voice(info);

		info->sf_id = 0;
		info->sample = j;
		info->start = 0;
		info->end = 0;
		info->loopstart = 0;
		info->loopend = 0;

		/* set rate offset */
		info->rate_offset = awe_calc_rate_offset(sample[j].sample_rate);
		DEBUG(0,fprintf(stderr, "(%d) pitch offset=%d\n", j, info->rate_offset));
		info->mode = 0;
		if (sample[j].modes & GUS_MODE_LOOP)
			info->mode |= AWE_MODE_LOOPING;
		else
			info->mode |= AWE_MODE_NORELEASE;

		/* root key calculation */
		info->root = calc_note(sample[j].root_frequency);
		info->tune = -calc_tune(sample[j].root_frequency);
		DEBUG(0,fprintf(stderr, "(%d) root=%d(%d)\n", j, info->root, -info->tune));

		/* calculate key range */
		if (drum_mode) {
			info->low = info->high = preset;
		} else {
			info->low = calc_note(sample[j].low_frequency);
			info->high = calc_note(sample[j].high_frequency);
		}
		DEBUG(0,fprintf(stderr, "(%d) range=%d - %d\n", j, info->low, info->high));
		/* set initial attenuation */
		info->attenuation = awe_calc_attenuation(10 * calc_gus_cB(header.master_volume * 2));

		/* sample envelope conversion */
		if (sample[j].modes & GUS_MODE_ENVELOPE)
			gus_setup_envelope(j, info);

		/* tremolo & vibrato */
		gus_setup_tremolo(j, info);
		gus_setup_vibrato(j, info);

	}

	vinfo->samplepos = 24 + AWE_SAMPLE_INFO_SIZE * layer.samples +
		(AWE_VOICE_REC_SIZE + AWE_VOICE_INFO_SIZE) * layer.samples;
	vinfo->samplesize = nsize;
	vinfo->data = waves;
}


/* set up volume envelope */
static void gus_setup_envelope(int j, awe_voice_info *info)
{
	int attack, hold, decay, release;
	attack = calc_gus_envelope_time(sample[j].envelope_rate[0], 0,
					sample[j].envelope_offset[0]);
	hold = calc_gus_envelope_time(sample[j].envelope_rate[1],
				      sample[j].envelope_offset[0],
				      sample[j].envelope_offset[1]);
	decay = calc_gus_envelope_time(sample[j].envelope_rate[2],
				       sample[j].envelope_offset[1],
				       sample[j].envelope_offset[2]);
	release = calc_gus_envelope_time(sample[j].envelope_rate[3],
					 sample[j].envelope_offset[1],
					 sample[j].envelope_offset[4]);
	release += calc_gus_envelope_time(sample[j].envelope_rate[4],
					  sample[j].envelope_offset[3],
					  sample[j].envelope_offset[4]);
	release += calc_gus_envelope_time(sample[j].envelope_rate[5],
					  sample[j].envelope_offset[4],
					  sample[j].envelope_offset[5]);
	DEBUG(0,fprintf(stderr, "(%d) attack=%d, hold=%d, decay=%d, release=%d\n", j, attack, hold, decay, release));
	info->parm.volatkhld = awe_calc_atkhld(attack, hold);
	info->parm.voldcysus = (calc_gus_sustain(sample[j].envelope_offset[2]) << 8) |
		awe_calc_decay(decay);
	DEBUG(0,fprintf(stderr, "(%d) %d(%d)-%d(%d)-%d(%d)-%d(%d)-%d(%d)-%d(%d)\n", j,
			calc_gus_envelope_rate(sample[j].envelope_rate[0]),
			sample[j].envelope_offset[0],
			calc_gus_envelope_rate(sample[j].envelope_rate[1]),
			sample[j].envelope_offset[1],
			calc_gus_envelope_rate(sample[j].envelope_rate[2]),
			sample[j].envelope_offset[2],
			calc_gus_envelope_rate(sample[j].envelope_rate[3]),
			sample[j].envelope_offset[3],
			calc_gus_envelope_rate(sample[j].envelope_rate[4]),
			sample[j].envelope_offset[4],
			calc_gus_envelope_rate(sample[j].envelope_rate[5]),
			sample[j].envelope_offset[5]));
	info->parm.volrelease = 0x8000 | awe_calc_decay(release);

	/* recalculate initial attenuation */
	info->attenuation = awe_calc_attenuation(10 * calc_gus_cB(header.master_volume * sample[j].envelope_offset[0] / 127));
}


/* set up tremolo effect */
static void gus_setup_tremolo(int j, awe_voice_info *info)
{
	int rate, depth;
	if (sample[j].tremolo_rate == 0 || sample[j].tremolo_depth == 0)
		return;
	DEBUG(1, fprintf(stderr, "[%d] tremolo sweep=%d, rate=%d, depth=%d\n", j,
			 sample[j].tremolo_sweep,
			 sample[j].tremolo_rate,
			 sample[j].tremolo_depth));

	/* to mHz */
	rate = (int)sample[j].tremolo_rate * 1000 / 38;
	/* to centibels */
	depth = calc_gus_cB(sample[j].tremolo_depth);
	info->parm.tremfrq = ((unsigned short)awe_calc_tremolo_vol(depth) << 8)  | 
		(unsigned short)awe_calc_env_freq(awe_mHz_to_abscent(rate));
}


/* set up vibrato effect */
static void gus_setup_vibrato(int j, awe_voice_info *info)
{
	int rate, depth;
	if (sample[j].vibrato_rate == 0 || sample[j].vibrato_depth == 0)
		return;
	DEBUG(1, fprintf(stderr, "[%d] vibrato sweep=%d, rate=%d, depth=%d\n", j,
			 sample[j].vibrato_sweep,
			 sample[j].vibrato_rate,
			 sample[j].vibrato_depth));
	/* to mHz */
	rate = (int)sample[j].vibrato_rate * 1000 / 38;
	/* 0xff = 2 octave ?; to cents */
	depth = (int)sample[j].vibrato_depth * 100 / 128;
	info->parm.fm2frq2 = ((unsigned short)awe_calc_pitch_height(depth) << 8)  | 
		(unsigned short)awe_calc_env_freq(awe_mHz_to_abscent(rate));
}


/* convert envelope rate */
static int calc_gus_envelope_rate(int rate)
{
	int r, p;
	r = (3 - ((rate >> 6) & 3)) * 3;
	p = rate & 0x3f;
	p = p << r;
	return p;
}
	
/* calculate envelope time */
static int calc_gus_envelope_time(int rate, int start, int end)
{
	int p, t;
	p = calc_gus_envelope_rate(rate);
	t = end - start;
	if (t < 0) t = -t;
	return (int)(1000.0 * (double)(t << 13) / (double)p / 44100.0);
}

/* convert linear (0-255) to centibel */
static int calc_gus_cB(int val)
{
	return (int)(-log10((double)val / 255.0) * 200);
}


/* calcualte sustain level */
static unsigned char calc_gus_sustain(int val)
{
	int cB = calc_gus_cB(val);
	DEBUG(0,fprintf(stderr, "val=%d, cB=%d\n", val, cB));
	return awe_calc_sustain(cB);
}
