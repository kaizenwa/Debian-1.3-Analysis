/*================================================================
 * sfxtext -- print sfx information
 *================================================================*/

#include <stdio.h>
#include "sfx.h"

int verbose = 0;
static VoiceInfo vinfo_rec;

static void print_sfx(VoiceInfo *sp);

void main(int argc, char **argv)
{
	FILE *fp;

	if ((fp = fopen(argv[1], "r")) == NULL) {
		fprintf(stderr, "can't open sfx file %s\n", argv[1]);
		exit(1);
	}

	sfx_read_file(&vinfo_rec, fp);
	fclose(fp);
	print_sfx(&vinfo_rec);
	exit(0);
}


static void print_sfx(VoiceInfo *vp)
{
	int i;

	printf("** SFXVersion %d\n", vp->sfx_version);
	printf("** Sample %d %d\n", (int)vp->samplepos, (int)vp->samplesize);
	printf("** Samples %d\n", vp->nwaves);
	for (i = 0; i < vp->nwaves; i++) {
		printf("[%d:%d] sample=%x-%x loop=%x-%x size=%x\n",
		       i, vp->sample[i].sample,
		       (int)vp->sample[i].start, (int)vp->sample[i].end,
		       (int)vp->sample[i].loopstart, (int)vp->sample[i].loopend,
		       (int)vp->sample[i].size);
	}
	printf("\n");
	printf("** Voices %d\n", vp->nvoices);
	for (i = 0; i < vp->nvoices; i++) {
		int j;
		awe_voice_info *vrec;
		printf("[%d] bank=%d preset=%d\n",
		       i, vp->voice[i]->bank, vp->voice[i]->instr);
		vrec = vp->voice[i]->info;
		for (j = 0; j < vp->voice[i]->nvoices; j++) {
			printf("    (%d) sample=%d pitch_ofs=%d mode=%x\n",
			       j, vrec[j].sample, vrec[j].rate_offset,
			       vrec[j].mode);
			printf("         root=%d(%d), key=%d-%d vel=%d-%d\n",
			       vrec[j].root, -vrec[j].tune,
			       vrec[j].low, vrec[j].high,
			       vrec[j].vellow, vrec[j].velhigh);
			printf("         scaleTune=%d class=%d atten=%d amp=%d\n",
			       vrec[j].scaleTuning, vrec[j].exclusiveClass,
			       vrec[j].attenuation, vrec[j].amplitude);
			printf("         mod del=%x atk=%x dcy=%x rel=%x\n",
			       vrec[j].parm.moddelay,
			       vrec[j].parm.modatkhld,
			       vrec[j].parm.moddcysus,
			       vrec[j].parm.modrelease);
			printf("         vol del=%x atk=%x dcy=%x rel=%x\n",
			       vrec[j].parm.voldelay,
			       vrec[j].parm.volatkhld,
			       vrec[j].parm.voldcysus,
			       vrec[j].parm.volrelease);
			printf("         lfo1 del=%x fmmod=%x tremfrq=%x\n",
			       vrec[j].parm.lfo1delay,
			       vrec[j].parm.fmmod,
			       vrec[j].parm.tremfrq);
			printf("         lfo2 del=%x fm2frq2=%x\n",
			       vrec[j].parm.lfo2delay,
			       vrec[j].parm.fm2frq2);
			printf("         pefe=%x cutoff=%x filterQ=%x\n",
			       vrec[j].parm.pefe,
			       vrec[j].parm.cutoff,
			       vrec[j].parm.filterQ);
		}
	}
}
