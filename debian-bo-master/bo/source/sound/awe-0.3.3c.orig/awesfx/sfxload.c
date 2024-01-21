/*================================================================
 * sfxload -- load soundfont info onto awe sound driver
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
#include <errno.h>
#include <stdlib.h>
#include <sys/fcntl.h>
#ifdef __FreeBSD__
#  include <machine/soundcard.h>
#else
#  include <sys/soundcard.h>
#endif
#include "sbk.h"
#include "layer.h"
#include "sfx.h"
#include "seq.h"
#include "debugmsg.h"


/* master volume */
#ifndef AWE_DEFAULT_VOLUME
#define AWE_DEFAULT_VOLUME	70	/* 70% */
#endif

/* maximum retry for error in loading */
#define MAX_TRY_RELOAD		5

int verbose = 0;

void seq_load_info(VoiceInfo *vp, FILE *fp);
void seq_load_data(VoiceInfo *vp, FILE *fp, char *filename);
void sbk_to_layer(SFInfo *sfinfo, LayInfo *layinfo);

static int is_sfx_file(char *fname);
static int is_sf2_file(char *fname);

static int search_file_name(char *fresult, char *fname, char *path);
static int file_exists(char *path);


static void usage()
{
	fprintf(stderr, "sfxload -- load sound info on AWE32 sound driver\n");
	fprintf(stderr, "   version 0.3.3  copyright (c) 1996,1997 by Takashi Iwai\n");
	fprintf(stderr, "usage: sfxload [-IixBvs] [-m mode] [-b bank] [-P sfdir]\n");
	fprintf(stderr, "               [-c chorus] [-r reverb] [-V volume] sfxfile [sf2file]\n");
	fprintf(stderr, " -I : initialize AWE32\n");
	fprintf(stderr, " -i : reset all samples\n");
	fprintf(stderr, " -x : remove last samples\n");
	fprintf(stderr, " -B : add 48 blank loop on each sample\n");
	fprintf(stderr, " -v : increment verbosity\n");
	/*fprintf(stderr, " -s : skip data checksum\n");*/
	fprintf(stderr, " -m mode : set instrument search mode, 0=first, 1=last, 2=merge (default=2)\n");
	fprintf(stderr, " -b bank : replace bank 0 to specified number\n");
	fprintf(stderr, " -c chorus: set chorus effect (0-100)\n");
	fprintf(stderr, " -r reverb: set reverb effect (0-100)\n");
	fprintf(stderr, " -V volume: set total volume (0-100)\n");
	fprintf(stderr, " -P sfdir: set SoundFont file search path\n");
#ifdef DEFAULT_SF_PATH
	fprintf(stderr, "   system default path is %s\n", DEFAULT_SF_PATH);
#endif
	exit(1);
}

static VoiceInfo vinfo;

static int chorus = -1, reverb = -1, volume = AWE_DEFAULT_VOLUME * 127 / 100;
static int bankchange = -1;
static int skip_checksum = 1;
static int num_infos = 0;


void main(int argc, char **argv)
{
	FILE *fd;
	char *sfxfile, *datafile;
	int init_chip = 0, init = 0, remove = 0;
	int c;
	char *default_sf_path;
	char sfxname[500], dataname[500];
	extern int auto_norelease;
	extern int default_choose_mode;

	default_sf_path = getenv("SFBANKDIR");
#ifdef DEFAULT_SF_PATH
	if (default_sf_path == NULL || *default_sf_path == 0)
		default_sf_path = strdup(DEFAULT_SF_PATH);
#endif
	auto_norelease = 0;
	while ((c = getopt(argc, argv, "aBb:c:iIm:P:r:svV:x")) != -1) {
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
		case 'B':
			auto_add_blank = !auto_add_blank;
			break;
		case 'a':
			auto_norelease = !auto_norelease;
			break;
		case 'v':
			verbose++;
			break;
		case 'V':
			volume = atoi(optarg) * 127 / 100;
			break;
		case 'b':
			bankchange = atoi(optarg);
			break;
		case 'c':
			chorus = atoi(optarg) * 255 / 100;
			break;
		case 'r':
			reverb = atoi(optarg) * 255 / 100;
			break;
		case 's':
			/*skip_checksum = !skip_checksum;*/
			break;
		case 'm':
			default_choose_mode = atoi(optarg);
			break;
		case 'P':
			default_sf_path = optarg;
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

	sfxfile = argv[optind]; optind++;
	if (! search_file_name(sfxname, sfxfile, default_sf_path)) {
		fprintf(stderr, "can't find such a file %s\n", sfxfile);
		exit(1);
	}
	if (optind < argc) {
		datafile = argv[optind];
		if (! search_file_name(dataname, datafile, default_sf_path)) {
			fprintf(stderr, "can't open SF file %s\n", datafile);
			exit(1);
		}
		datafile = dataname;
	} else
		datafile = NULL;

	DEBUG(0,fprintf(stderr, "found file = %s\n", sfxname));
	/* check file format */
	if (is_sf2_file(sfxname)) {
		/* load SF2 and convert it */
		static SFInfo sfinfo;
		static LayInfo layinfo;
		if ((fd = fopen(sfxname, "r")) == NULL) {
			fprintf(stderr, "can't open sf2 file %s\n", sfxfile);
			exit(1);
		}
		load_sbk(fd, &sfinfo);
		fclose(fd);

		sbk_to_layer(&sfinfo, &layinfo);
		free_sbk(&sfinfo);

		/* convert to awe_voice parameters */
		awe_convert_layer(&vinfo, &layinfo, 0);

		fd = NULL;  /* already read */
		datafile = sfxname;  /* read original file */

	} else if (is_sfx_file(sfxname)) {
		if ((fd = fopen(sfxname, "r")) == NULL) {
			fprintf(stderr, "can't open sfx file %s\n", sfxfile);
			exit(1);
		}
		if (! sfx_read_header(&vinfo, fd)) {
			fprintf(stderr, "sfxload: Not an SFX file\n");
			exit(1);
		}
	} else {
		fprintf(stderr, "Can't read file: %s\n", sfxfile);
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
	seq_load_data(&vinfo, fd, datafile);

	DEBUG(1,fprintf(stderr, "uploading information..\n"));
	seq_load_info(&vinfo, fd);

	DEBUG(0,fprintf(stderr, "# of voices=%d\n", num_infos));
	DEBUG(1,fprintf(stderr, "done\n"));

	DEBUG(0,fprintf(stderr,"DRAM memory left = %d kB\n", seq_mem_avail()/1024));

	/* close sequencer */
	seq_end();
	if (fd)
		fclose(fd);

	exit(0);
}


/*
 * load voice record to the awe driver
 */
void seq_load_info(VoiceInfo *vp, FILE *fp)
{
	int i, j;
	char *buf;
	awe_patch_info *patch;

	for (i = 0; i < vp->nvoices; i++) {
		awe_voice_rec vtmp, *curv, *rec;
		int len;
		if (fp) {
			/* if not loaded, read it on temporary */
			curv = &vtmp;
			fread(curv, AWE_VOICE_REC_SIZE, 1, fp);
		} else
			curv = vp->voice[i];

		/* total size of the record */
		len = AWE_VOICE_REC_SIZE +
			curv->nvoices * AWE_VOICE_INFO_SIZE;

		/* allocate buffer for all record */
		buf = (char*)calloc(len + AWE_PATCH_INFO_SIZE, 1);
		rec = (awe_voice_rec*)(buf + AWE_PATCH_INFO_SIZE);

		/* copy header */
		memcpy(rec, curv, AWE_VOICE_REC_SIZE);

		/* read or copy all voice parameters */
		if (fp)
			fread(rec->info, AWE_VOICE_INFO_SIZE * rec->nvoices, 1, fp);
		else
			memcpy(rec->info, curv->info, AWE_VOICE_INFO_SIZE * rec->nvoices);

		num_infos += rec->nvoices;

		/* if -b option is specified, change the bank value */
		if (bankchange >= 0) {
			if (rec->bank == 0)
				rec->bank = bankchange;
		}
		/* if chours & reverb effects are specified, set them */
		for (j = 0; j < rec->nvoices; j++) {
			if (chorus >= 0)
				rec->info[j].parm.chorus = (unsigned char)chorus;
			if (reverb >= 0)
				rec->info[j].parm.reverb = (unsigned char)reverb;
			if (volume >= 0)
				rec->info[j].amplitude = (unsigned char)volume;
		}

		/* ok, send the parameters to the driver.. */
		patch = (awe_patch_info*)buf;
		patch->key = AWE_PATCH;
		patch->device_no = awe_dev;
		patch->sf_id = 0;	/* must be zero */
		patch->len = len;	/* not including the patch header itself */
		patch->type = AWE_LOAD_INFO;

		if (seq_load_patch(patch, AWE_PATCH_INFO_SIZE + patch->len) == -1) {
			fprintf(stderr, "[Loading Info %d]\n", i);
			perror("Error in loading info");
			exit(1);
		}

		/* free temporary buffer */
		free(buf);
	}
}


void seq_load_data(VoiceInfo *vp, FILE *fp, char *filename)
{
	FILE *fdata;
	char *buf;
	awe_patch_info *patch;
	awe_sample_info *header;
	unsigned short *sample;
	int i;

	fdata = NULL;
	if (filename && (fdata = fopen(filename, "r")) == NULL) {
		fprintf(stderr, "can't open origianl sbk file %s\n",
			filename);
		exit(1);
	}

	for (i = 0; i < vp->nwaves; i++) {
		int len;
		awe_sample_info tsamp, *curs;

		if (fp) {
			/* if not loaded yet, read it from SFX file */
			curs = &tsamp;
			fread(curs, AWE_SAMPLE_INFO_SIZE, 1, fp);
		} else
			curs = &vp->sample[i];

		/* sample size is word size, so twice the value */
		len = AWE_SAMPLE_INFO_SIZE + curs->size * 2;

		/* allocate temporary buffer for all the sample */
		buf = (char*)calloc(len + AWE_PATCH_INFO_SIZE, 1);
		if (buf == NULL) {
			fprintf(stderr, "Error: can't malloc for size %d\n",
				len);
			exit(1);
		}
		patch = (awe_patch_info*)buf;
		header = (awe_sample_info*)(buf + AWE_PATCH_INFO_SIZE);
		sample = (unsigned short*)(buf + AWE_PATCH_INFO_SIZE +
					   AWE_SAMPLE_INFO_SIZE);

		/* copy sample info */
		memcpy(header, curs, AWE_SAMPLE_INFO_SIZE);

		/* sf_id must be zero */
		header->sf_id = 0;
		header->checksum_flag = 0;

		/* if this is not ROM sample, load it */
		if (header->size > 0) {
			int c;
			if (vp->sfx_version == 1) {
				/* sample data is in the different file */
				if (fdata == NULL) {
					fprintf(stderr, "No SoundFont sample file is specified.\n");
					exit(1);
				}
				fseek(fdata, header->start * 2 + vp->samplepos, SEEK_SET);
				fread(sample, header->size * 2, 1, fdata);
			} else {
				/* sample data follows after header */
				fread(sample, header->size * 2, 1, fp);
			}

			/* clear 48 samples at the tail for blank loop */
			memset(sample + header->end - header->start, 0,
			       (header->size - header->end + header->start) * 2);
			if (! skip_checksum) {
				header->checksum_flag = 1;
				header->checksum = 0;
				for (c = 0; c < header->size; c++) {
					header->checksum += sample[c];
				}
			}
		}

		/* ok, loading the patch.. */
		patch->key = AWE_PATCH;
		patch->device_no = awe_dev;
		patch->sf_id = 0; /* again, zero */
		patch->len = len; /* not including the patch header size */
		patch->type = AWE_LOAD_DATA;

		if (seq_load_patch(patch, AWE_PATCH_INFO_SIZE + patch->len) == -1) {
#ifdef __FreeBSD__
			if (errno == EINVAL) {
#else
 			if (errno == ENODATA) {
#endif
				int k;
				fprintf(stderr, "(%d) bad checksum:", i);
				for (k = 0; k < MAX_TRY_RELOAD; k++) {
					if (seq_load_patch(patch, AWE_PATCH_INFO_SIZE + patch->len) != -1)
						break;
				}
				if (k >= MAX_TRY_RELOAD) {
					perror("Error in reloading data");
					exit(1);
				}
				fprintf(stderr, "OK\n");
			} else {
				fprintf(stderr, "[Loading Data %d]\n", i);
				perror("Error in loading data");
				exit(1);
			}
		}

		/* free temporary buffer */
		free(buf);
	}

	if (filename)
		fclose(fdata);
}


/*
 * check the file is an SF2 file or not
 */
static int is_sf2_file(char *fname)
{
	FILE *fp;
	char buf[4];

	if ((fp = fopen(fname, "r")) == NULL)
		return 0;

	fread(buf, 1, 4, fp);
	if (memcmp(buf, "RIFF", 4) != 0) { /* RIFF format */
		fclose(fp);
		return 0;
	}
	fread(buf, 1, 4, fp); /* total size */
	fread(buf, 1, 4, fp); /* header id */
	fclose(fp);
	if (memcmp(buf, "sfbk", 4) != 0)
		return 0;
	return 1;
}
	

/*
 * check the file is an SFX file or not
 */
static int is_sfx_file(char *fname)
{
	FILE *fp;
	char buf[4];

	if ((fp = fopen(fname, "r")) == NULL)
		return 0;

	/* read 4bytes id */
	fread(buf, 1, 4, fp);
	fclose(fp);
	if (memcmp(buf, AWE_ID_STR, 4) != 0)
		return 0;

	return 1;
}

/*
 * search a file from path list
 */
static int search_file_name(char *fresult, char *fname, char *path)
{
	char *tok;

	if (fname[0] != '/' && path && *path) {
		for (tok = strtok(path, ":"); tok; tok = strtok(NULL, ":")) {
			strcpy(fresult, tok);
			if (*tok && tok[strlen(tok)-1] != '/')
				strcat(fresult, "/");
			strcat(fresult, fname);
			if (file_exists(fresult))
				return 1;
		}
	}
	strcpy(fresult, fname);
	return file_exists(fresult);
}

static int file_exists(char *path)
{
	char *lastp;
	if (access(path, R_OK) == 0) return 1;
	lastp = path + strlen(path);
	strcpy(lastp, ".sfx");
	if (access(path, R_OK) == 0) return 1;
	strcpy(lastp, ".SFX");
	if (access(path, R_OK) == 0) return 1;
	strcpy(lastp, ".SF2");
	if (access(path, R_OK) == 0) return 1;
	strcpy(lastp, ".sf2");
	if (access(path, R_OK) == 0) return 1;
	strcpy(lastp, ".sbk");
	if (access(path, R_OK) == 0) return 1;
	strcpy(lastp, ".SBK");
	if (access(path, R_OK) == 0) return 1;
	return 0;
}
