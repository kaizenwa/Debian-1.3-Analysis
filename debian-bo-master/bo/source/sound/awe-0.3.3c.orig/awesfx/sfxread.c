/*================================================================
 * reading / writing sfx file
 *================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "debugmsg.h"
#include "sfx.h"

void sfx_write_header(VoiceInfo *vp, FILE *fp)
{
	fwrite(AWE_ID_STR, 1, 4, fp);
	fwrite(&vp->sfx_version, sizeof(int), 1, fp);
	fwrite(&vp->samplepos, sizeof(int), 1, fp);
	fwrite(&vp->samplesize, sizeof(int), 1, fp);
	fwrite(&vp->nwaves, sizeof(int), 1, fp);
	fwrite(&vp->nvoices, sizeof(int), 1, fp);
}

int sfx_read_header(VoiceInfo *vp, FILE *fp)
{
	char id[4];
	fread(id, 1, 4, fp);
	if (strncmp(id, AWE_ID_STR, 4) != 0) {
		fprintf(stderr, "Illegal SFX file\n");
		return 0;
	}
	fread(&vp->sfx_version, sizeof(int), 1, fp);
	fread(&vp->samplepos, sizeof(int), 1, fp);
	fread(&vp->samplesize, sizeof(int), 1, fp);
	fread(&vp->nwaves, sizeof(int), 1, fp);
	fread(&vp->nvoices, sizeof(int), 1, fp);

	return 1;
}	


void sfx_write_file(VoiceInfo *vp, FILE *fp)
{
	int i;

	DEBUG(0,fprintf(stderr, "file dumping..\n"));

	sfx_write_header(vp, fp);

	DEBUG(0,fprintf(stderr, "waves filepos = %x\n", (int)ftell(fp)));
	for (i = 0; i < vp->nwaves; i++) {
		fwrite(vp->sample + i, AWE_SAMPLE_INFO_SIZE, 1, fp);
		if (vp->sfx_version == 2)
			fwrite(vp->data[i], vp->sample[i].size, 2, fp);
	}
	DEBUG(0,fprintf(stderr, "voices filepos = %x\n", (int)ftell(fp)));
	for (i = 0; i < vp->nvoices; i++) {
		fwrite(vp->voice[i], AWE_VOICE_REC_SIZE +
		       AWE_VOICE_INFO_SIZE * vp->voice[i]->nvoices, 1, fp);
	}

	DEBUG(0,fprintf(stderr, "\ndumping done\n"));
}


void sfx_write_file_with_data(VoiceInfo *vp, FILE *fp, char *filename)
{
	FILE *fdata;
	int i;

	if ((fdata = fopen(filename, "r")) == NULL) {
		fprintf(stderr, "can't open data file %s\n", filename);
		exit(1);
	}

	DEBUG(0,fprintf(stderr, "file dumping..\n"));

	vp->sfx_version = 2;
	sfx_write_header(vp, fp);

	for (i = 0; i < vp->nwaves; i++) {
		unsigned short *buf;
		fwrite(vp->sample + i, AWE_SAMPLE_INFO_SIZE, 1, fp);
		buf = (unsigned short*)malloc(vp->sample[i].size * 2);
		fseek(fdata, vp->sample[i].start * 2 + vp->samplepos,
		      SEEK_SET);
		fread(buf, vp->sample[i].size, 2, fdata);
		fwrite(buf, vp->sample[i].size, 2, fp);
		free(buf);
	}
	for (i = 0; i < vp->nvoices; i++) {
		fwrite(vp->voice[i], AWE_VOICE_REC_SIZE +
		       AWE_VOICE_INFO_SIZE * vp->voice[i]->nvoices, 1, fp);
	}

	DEBUG(0,fprintf(stderr, "\ndumping done\n"));
}


void sfx_read_file(VoiceInfo *vp, FILE *fp)
{
	int i;

	if (! sfx_read_header(vp, fp)) {
		fprintf(stderr, "sfx_read: not SFX file\n");
		exit(1);
	}

	vp->sample = (awe_sample_info*)calloc(vp->nwaves, AWE_SAMPLE_INFO_SIZE);
	if (vp->sample == NULL) {
		fprintf(stderr, "can't allocate sample buffer\n");
		exit(1);
	}

	vp->data = NULL;
	if (vp->sfx_version == 2) {
		vp->data = (unsigned short**)calloc(vp->nwaves,
						    sizeof(unsigned short **));
		if (vp->data == NULL) {
			fprintf(stderr, "can't allocate data buffer\n");
			exit(1);
		}
	}
	
	vp->voice = (awe_voice_rec**)calloc(vp->nvoices, sizeof(awe_voice_rec*));
	if (vp->voice == NULL) {
		fprintf(stderr, "can't allocate voice buffer\n");
		exit(1);
	}

	for (i = 0; i < vp->nwaves; i++) {
		fread(vp->sample + i, AWE_SAMPLE_INFO_SIZE, 1, fp);
		if (vp->sfx_version == 2) {
			vp->data[i] = (unsigned short*)calloc(vp->sample[i].size, 2);
			fread(vp->data[i], vp->sample[i].size, 2, fp);
		}
	}

	for (i = 0; i < vp->nvoices; i++) {
		awe_voice_rec vtmp;
		fread(&vtmp, AWE_VOICE_REC_SIZE, 1, fp);
		vp->voice[i] = (awe_voice_rec*)calloc
			(AWE_VOICE_REC_SIZE + AWE_VOICE_INFO_SIZE * vtmp.nvoices, 1);
		memcpy(vp->voice[i], &vtmp, sizeof(vtmp));
		fread(&vp->voice[i]->info[0], sizeof(awe_voice_info), vtmp.nvoices, fp);
	}
}
