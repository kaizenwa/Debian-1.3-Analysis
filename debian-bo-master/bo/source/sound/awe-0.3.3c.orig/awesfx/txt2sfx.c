/*================================================================
 * txt2sfx --- convert textized SF2 information to SFX file
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
#include <stdlib.h>
#include <unistd.h>
#include "layer.h"
#include "sfx.h"

int verbose = 0;

static VoiceInfo vinfo;
static LayInfo linfo;

static void usage(void)
{
	fprintf(stderr, "usage: txt2sfx [-Bv] [-m mode] [-d sf2file] textfile sfxfile\n");
	fprintf(stderr, "   version 0.3.2  copyright (c) 1996,1997 by Takashi Iwai\n");
	fprintf(stderr, " -B : add 48 blank loop on each sample\n");
	fprintf(stderr, " -v : increase verbosity level\n");
	fprintf(stderr, " -m mode : instrument mode, 0=first, 1=last, 2=merge (default=1)\n");
	fprintf(stderr, " -d sf2file : include sample data in SFX file\n");
}

void main(int argc, char **argv)
{
	int c;
	FILE *fp;
	char *sffile = NULL;
	
	while ((c = getopt(argc, argv, "vd:m:")) != -1) {
		switch (c) {
		case 'v':
			verbose++;
			break;
		case 'd':
			sffile = optarg;
			break;
		case 'm':
			default_choose_mode = atoi(optarg);
			break;
		default:
			usage();
			exit(1);
		}
	}
	if (argc - optind < 2) {
		usage();
		exit(1);
	}

	if ((fp = fopen(argv[optind], "r")) == NULL) {
		fprintf(stderr, "can't open text file %s\n", argv[1]);
		exit(1);
	}
	optind++;

	load_sbk_text(&linfo, fp);
	fclose(fp);

	/*
	if (linfo.version == 1) {
		fprintf(stderr, "Sorry, txt2sfx only supports SF2 format file.\n");
		fprintf(stderr, "Please convert this to SF2 by sf1to2.exe\n");
		exit(1);
	}
	*/

	awe_convert_layer(&vinfo, &linfo, 0);

	if ((fp = fopen(argv[optind], "w")) == NULL) {
		fprintf(stderr, "can't open sfx file %s\n", argv[2]);
		exit(1);
	}
	optind++;

	if (sffile)
		sfx_write_file_with_data(&vinfo, fp, sffile);
	else
		sfx_write_file(&vinfo, fp);

	fclose(fp);

	exit(0);
}
