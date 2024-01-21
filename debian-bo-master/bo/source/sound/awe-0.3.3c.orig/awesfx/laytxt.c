/*----------------------------------------------------------------
 * read textized sbk information
 *----------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sbk.h"
#include "layer.h"


/*----------------------------------------------------------------
 * prototypes
 *----------------------------------------------------------------*/

static void load_preset(LayInfo *lp, FILE *fp);
static void load_inst(LayInfo *lp, FILE *fp);
static void load_sample(LayInfo *lp, FILE *fp);
static char *strmatch(char *s, char *t);
static void load_layers(Layer *lp, int nlayers, FILE *fp);

static char linebuf[256];
/*#define READLINE(fp)	fgets(linebuf, sizeof(linebuf), fp)*/
static char *READLINE(FILE *fp)
{
	while (fgets(linebuf, sizeof(linebuf), fp)) {
		if (linebuf[0] == '#')
			continue;
		else
			return linebuf;
	}
	return NULL;
}




/*----------------------------------------------------------------
 * compare with tag and return the rest position
 *----------------------------------------------------------------*/

static char *strmatch(char *s, char *t)
{
	int len = strlen(t);
	if (strncmp(s, t, len) == 0)
		return s + len;
	else
		return NULL;
}


/*----------------------------------------------------------------
 * load sbk text
 *----------------------------------------------------------------*/

void load_sbk_text(LayInfo *lp, FILE *fp)
{
	while (READLINE(fp)) {
		if (strmatch(linebuf, "** ")) {
			char *tag = gettag(linebuf + 3);
			if (strmatch(tag, "SampleData:")) {
				lp->samplepos = atoi(gettag(NULL));
				lp->samplesize = atoi(gettag(NULL));
			} else if (strmatch(tag, "SoundFont:")) {
				lp->version = atoi(gettag(NULL));
				lp->minorversion = atoi(gettag(NULL));
			} else if (strmatch(tag, "Presets:")) {
				lp->npresets = atoi(gettag(NULL));
				load_preset(lp, fp);
			} else if (strmatch(tag, "Instruments:")) {
				lp->ninsts = atoi(gettag(NULL));
				load_inst(lp, fp);
			} else if (strmatch(tag, "SampleInfo:")) {
				lp->nsamples = atoi(gettag(NULL));
				load_sample(lp, fp);
			} else {
				fprintf(stderr, "unknown tag: %s\n", tag);
			}
				
		}
	}
}


static void load_preset(LayInfo *lp, FILE *fp)
{
	int i;

	lp->ptable = (Preset*)calloc(lp->npresets, sizeof(Preset));
	for (i = 0; i < lp->npresets; i++) {
		Preset *preset = lp->ptable + i;
		char *p, *v;

		READLINE(fp);
		strcpy(preset->name, gettag(linebuf));
		preset->choose = default_choose_mode;

		while ((p = gettag(NULL)) != NULL) {
			if ((v = strmatch(p, "preset=")) != NULL)
				preset->preset = atoi(v);
			else if ((v = strmatch(p, "bank=")) != NULL)
				preset->bank = atoi(v);
			else if ((v = strmatch(p, "layer=")) != NULL)
				preset->layers = atoi(v);
			else if ((v = strmatch(p, "first")) != NULL)
				preset->choose = 0;
			else if ((v = strmatch(p, "last")) != NULL)
				preset->choose = 1;
			else if ((v = strmatch(p, "merge")) != NULL)
				preset->choose = 2;
		}

		if (preset->layers == 0)
			preset->layer = NULL;
		else {
			preset->layer = (Layer*)calloc(preset->layers, sizeof(Layer));
			load_layers(preset->layer, preset->layers, fp);
		}

		if (i < lp->npresets - 1)
			preset->next = preset + 1;
		else
			preset->next = NULL;
	}
}

static void load_inst(LayInfo *lp, FILE *fp)
{
	int i;

	lp->itable = (Inst*)calloc(lp->ninsts, sizeof(Inst));
	for (i = 0; i < lp->ninsts; i++) {
		Inst *inst = lp->itable + i;
		char *p, *v;

		READLINE(fp);
		strcpy(inst->name, gettag(linebuf));

		while ((p = gettag(NULL)) != NULL) {
			if ((v = strmatch(p, "layer=")) != NULL)
				inst->layers = atoi(v);
		}
		if (inst->layers == 0)
			inst->layer = NULL;
		else {
			inst->layer = (Layer*)calloc(inst->layers, sizeof(Layer));
			load_layers(inst->layer, inst->layers, fp);
		}

		if (i < lp->ninsts - 1)
			inst->next = inst + 1;
		else
			inst->next = NULL;
	}
}


static void load_layers(Layer *lp, int nlayers, FILE *fp)
{
	int l;
	int i, bags;
	char *tok;

	for (l = 0; l < nlayers; l++, lp++) {
		/* BAG line */
		READLINE(fp);
		tok = gettag(linebuf);
		bags = atoi(gettag(NULL));

		for (i = 0; i < bags; i++) {
			READLINE(fp);
			text_to_layer(lp, linebuf + 4);
		}

		/* link to next */
		if (l < nlayers - 1)
			lp->next = lp + 1;
		else
			lp->next = NULL;
	}
}


static void load_sample(LayInfo *lp, FILE *fp)
{
	int i;
	char *p, *v;
	int in_rom = 1;

	lp->sample = (Sample*)calloc(lp->nsamples, sizeof(Sample));
	for (i = 0; i < lp->nsamples; i++) {
		Sample *sp = lp->sample + i;
		READLINE(fp);
		strcpy(sp->name, gettag(linebuf));
		while ((p = gettag(NULL)) != NULL) {
			if ((v = strmatch(p, "start=")) != NULL)
				sp->start = strtol(v, NULL, 16);
			else if ((v = strmatch(p, "end=")) != NULL)
				sp->end = strtol(v, NULL, 16);
			else if ((v = strmatch(p, "startloop=")) != NULL)
				sp->loopstart = strtol(v, NULL, 16);
			else if ((v = strmatch(p, "endloop=")) != NULL)
				sp->loopend = strtol(v, NULL, 16);
		}
		if (lp->version == 2) {
			char *q = linebuf;
			READLINE(fp);
			while ((p = gettag(q)) != NULL) {
				q = NULL;
				if ((v = strmatch(p, "rate=")) != NULL)
					sp->samplerate = strtol(v, NULL, 10);
				else if ((v = strmatch(p, "pitch=")) != NULL) {
					sp->pitch = strtol(strtok(v, ":"), NULL, 10);
					sp->correction = strtol(strtok(NULL, " "), NULL, 10);
				}
				else if ((v = strmatch(p, "link=")) != NULL)
					sp->link = strtol(v, NULL, 0);
				else if ((v = strmatch(p, "type=")) != NULL)
					sp->mode = strtol(v, NULL, 16);
			}
		} else {
			if (sp->start == 0)
				in_rom = 0;
			sp->loopstart++;
			sp->loopend += 2;
			sp->samplerate = 44100;
			sp->pitch = 60;
			sp->correction = 0;
			sp->link = 0;
			if (in_rom)
				sp->mode = 0x8001;
			else
				sp->mode = 1;
		}

		if (i < lp->nsamples - 1)
			sp->next = sp + 1;
		else
			sp->next = NULL;
	}
}
