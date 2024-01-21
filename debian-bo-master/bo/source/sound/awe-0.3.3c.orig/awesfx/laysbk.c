/*================================================================
 * convert sbk info to layer
 *================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "sbk.h"
#include "layer.h"

static Layer *alloc_layer(unsigned short *bag, tgenrec *gen, int start, int end);

void sbk_to_layer(SFInfo *sfinfo, LayInfo *linfo)
{
	int i, in_rom;

	linfo->version = sfinfo->version;
	linfo->minorversion = sfinfo->minorversion;

	linfo->samplepos = sfinfo->samplepos;
	linfo->samplesize = sfinfo->samplesize;

	linfo->npresets = sfinfo->nrpresets - 1;
	linfo->ptable = (Preset*)calloc(linfo->npresets, sizeof(Preset));
	for (i = 0; i < linfo->npresets; i++) {
		Preset *preset = linfo->ptable + i;
		strcpy(preset->name, sfinfo->presethdr[i].name);
		preset->bank = sfinfo->presethdr[i].bank;
		preset->preset = sfinfo->presethdr[i].preset;
		preset->choose = default_choose_mode;
		preset->layers = sfinfo->presethdr[i+1].bagNdx -
			sfinfo->presethdr[i].bagNdx;
		if (preset->layers == 0)
			preset->layer = NULL;
		else
			preset->layer = alloc_layer(sfinfo->presetbag,
						    sfinfo->presetgen,
						    sfinfo->presethdr[i].bagNdx,
						    sfinfo->presethdr[i+1].bagNdx);
		if (i < linfo->npresets - 1)
			preset->next = preset + 1;
		else
			preset->next = NULL;
	}

	linfo->ninsts = sfinfo->nrinsts - 1;
	linfo->itable = (Inst*)calloc(linfo->ninsts, sizeof(Inst));
	for (i = 0; i < linfo->ninsts; i++) {
		Inst *inst = linfo->itable + i;
		strcpy(inst->name, sfinfo->insthdr[i].name);
		inst->layers = sfinfo->insthdr[i+1].bagNdx -
			sfinfo->insthdr[i].bagNdx;
		if (inst->layers == 0)
			inst->layer = NULL;
		else
			inst->layer = alloc_layer(sfinfo->instbag,
						  sfinfo->instgen,
						  sfinfo->insthdr[i].bagNdx,
						  sfinfo->insthdr[i+1].bagNdx);
		if (i < linfo->ninsts - 1)
			inst->next = inst + 1;
		else
			inst->next = NULL;
	}

	if (sfinfo->version == 1)
		linfo->nsamples = sfinfo->nrinfos;
	else
		linfo->nsamples = sfinfo->nrinfos - 1;
	linfo->sample = (Sample*)calloc(linfo->nsamples, sizeof(Sample));
	in_rom = 1;
	for (i = 0; i < linfo->nsamples; i++) {
		Sample *sp = linfo->sample + i;
		strcpy(sp->name, sfinfo->samplenames[i].name);
		sp->start = sfinfo->sampleinfo[i].startsample;
		sp->end = sfinfo->sampleinfo[i].endsample;
		sp->loopstart = sfinfo->sampleinfo[i].startloop;
		sp->loopend = sfinfo->sampleinfo[i].endloop;
		if (sfinfo->version == 1) {
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
		} else {
			sp->samplerate = sfinfo->sampleinfo[i].samplerate;
			sp->pitch = sfinfo->sampleinfo[i].originalPitch;
			sp->correction = sfinfo->sampleinfo[i].pitchCorrection;
			sp->link = sfinfo->sampleinfo[i].samplelink;
			sp->mode = sfinfo->sampleinfo[i].sampletype;
		}

		if (i < linfo->nsamples - 1)
			sp->next = sp + 1;
		else
			sp->next = NULL;
	}

}


static Layer *alloc_layer(unsigned short *bag, tgenrec *gen, int start, int end)
{
	Layer *layer, *lp;
	int b, i;

	layer = (Layer*)calloc(end - start, sizeof(Layer));

	lp = layer;
	for (b = start; b < end; b++, lp++) {
		for (i = bag[b]; i < bag[b+1]; i++) {
			int type = gen[i].oper;
			int val = gen[i].amount;
			lp->set[type] = 1;
			switch (type) {
			case 43:
			case 44:
				lp->val[type].r.lo = val & 0xff;
				lp->val[type].r.hi = (val>>8) & 0xff;
				break;
			default:
				lp->val[type].val = val;
				break;
			}
		}
		if (b < end - 1)
			lp->next = lp + 1;
		else
			lp->next = NULL;
	}

	return layer;
}
