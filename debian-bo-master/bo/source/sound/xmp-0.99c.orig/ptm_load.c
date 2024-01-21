/* ptm_load.c - Poly Tracker module loader
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: ptm_load.c,v 1.1 1997/03/13 13:29:01 claudio Exp $
 *
 * $Log: ptm_load.c,v $
 * Revision 1.1  1997/03/13 13:29:01  claudio
 * Initial revision
 *
 */

#include "config.h"

#ifdef LOAD_PTM

#include "load.h"
#include "ptm.h"

#ifdef MATH_LOG
#include <math.h>
#endif


/* PTM volume table formula (approximated):
 *
 * 64*(10**((23362+598*(64+20*log10(x/64))-64E3)/20E3))
 */

static int ptm_vol[] = {
     0,  5,  8, 10, 12, 14, 15, 17, 18, 20, 21, 22, 23, 25, 26,
    27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 37, 38, 39, 40,
    41, 42, 42, 43, 44, 45, 46, 46, 47, 48, 49, 49, 50, 51, 51,
    52, 53, 54, 54, 55, 56, 56, 57, 58, 58, 59, 59, 60, 61, 61,
    62, 63, 63, 64, 64
};


int ptm_load(FILE *f,struct dev_info *d)
{
    int c,r,i,smp_ofs[256];
    struct xxm_event *event;
    struct ptm_file_header pfh;
    struct ptm_instrument_header pih;
    uint8 n,b;

    LOAD_INIT();

    /* Load and convert header */
    fread(&pfh,1,sizeof(pfh),f);
    if(strncmp((char*)pfh.magic,"PTMF",4)) return -1;
    L_ENDIAN16(pfh.ordnum);
    L_ENDIAN16(pfh.insnum);
    L_ENDIAN16(pfh.patnum);
    L_ENDIAN16(pfh.chnnum);
    strcpy(module_name,(char*)pfh.name);
    strncpy((char*)xxh->name,module_name,22);
    xxh->len=pfh.ordnum;
    xxh->ins=pfh.insnum;
    xxh->pat=pfh.patnum;
    xxh->chn=pfh.chnnum;
    xxh->trk=xxh->pat*xxh->chn;
    xxh->smp=xxh->ins;
    xxh->tpo=6;	
    xxh->bpm=125;	
    xxh->flg|=XXM_FLG_LOOPS3M;	/* ??? */
    memcpy(xxo,pfh.order,256);
    for(i=0;i<pfh.patnum;i++) L_ENDIAN16(pfh.patseg[i]);

    c4_rate=C4_NTSC_RATE;

    strncpy(module_name,pfh.name,28);
    sprintf(module_type,"Poly Tracker %d.%02x module",
	pfh.vermaj,pfh.vermin);

    MODULE_INFO();

    INSTRUMENT_INIT();

    /* Read and convert instruments and samples */
    for(i=0;i<xxh->ins;i++) {
	xxi[i]=calloc(sizeof(struct xxm_instrument),1);
	fread(&pih,1,sizeof(pih),f);
	if((pih.type&3)!=1) continue;
#if 0 
	if(strncmp(pih.magic,"PTMS",4)) return -2;
#endif
	L_ENDIAN16(pih.c4spd);
	L_ENDIAN32(pih.smpofs);
	L_ENDIAN32(pih.length);
	L_ENDIAN32(pih.loopbeg);
	L_ENDIAN32(pih.loopend);
	smp_ofs[i]=pih.smpofs;
	xxih[i].nsm=!!(xxs[i].len=pih.length);
	xxs[i].lps=pih.loopbeg;
	xxs[i].lpe=pih.loopend;
	xxs[i].flg=pih.type&0x04?WAVE_LOOPING:0;
	xxs[i].flg|=pih.type&0x08?WAVE_LOOPING|WAVE_BIDIR_LOOP:0;
	xxs[i].flg|=pih.type&0x10?WAVE_16_BITS:0;
	xxi[i][0].vol=pih.vol;
	xxi[i][0].pan=0x80;
	xxi[i][0].sid=i;
	pih.magic[0]=0;
	str_adj((char*)pih.name);
	strncpy((char*)xxih[i].name,pih.name,24);
	if((opt.verbose>1)&&(strlen((char*)pih.name)||xxs[i].len))
	    report("[%2X] %-28.28s %05x%c%05x %05x %c V%02x %5d\n",
	    i,pih.name,xxs[i].len,pih.type&0x10?'+':' ',xxs[i].lps,
	    xxs[i].lpe,xxs[i].flg&WAVE_LOOPING?'L':' ',xxi[i][0].vol,
	    pih.c4spd);

	/* Convert C4SPD to relnote/finetune */
#ifdef MATH_LOG
	c=(1200.0*log(pih.c4spd)-1200.0*log(c4_rate))/M_LN2;
	xxi[i][0].xpo=c/100;
	xxi[i][0].fin=128*(c%100)/100;
#else
	c2spd_to_note(pih.c4spd,&xxi[i][0].xpo,&xxi[i][0].fin);
#endif
    }

    PATTERN_INIT();

    /* Read patterns */
    if(opt.verbose) report("Stored patterns: %d ",xxh->pat);
    for(i=0;i<xxh->pat;i++) {
	if(!pfh.patseg[i]) continue;
	PATTERN_ALLOC(i);
	xxp[i]->rows=64;
	TRACK_ALLOC(i);
	fseek(f,16L*pfh.patseg[i],SEEK_SET);
	r=0;
	while(r<64) {
	    fread(&b,1,1,f);
	    if(!b) { r++; continue; }
	    c=b&PTM_CH_MASK;
	    if(c>=xxh->chn) continue;
	    event=&EVENT(i,c,r);
	    if(b&PTM_NI_FOLLOW) {
		fread(&n,1,1,f); 
		switch(n) {
		    case 255: n=0; break; 	/* Empty note */
		    case 254: n=0x61; break;	/* Key off */
		}
		event->not=n;
		fread(&n,1,1,f);
		event->ins=n;
	    }
	    if(b&PTM_FX_FOLLOWS) {
		fread(&n,1,1,f);
		event->fxt=n;
		fread(&n,1,1,f);
		event->fxp=n;
		switch(event->fxt) {
		case FX_E:			/* Pan set */
		    if(MSN(event->fxp)==0x8) {
			event->fxt=FX_8;
			event->fxp=LSN(event->fxp)<<4;
		    }
		    break;
		case FX_H:			/* Multi retrig */
		    event->fxt=FX_R;
		    break;	
		case FX_I:			/* Fine vibrato */
		    event->fxt=FX_4;
		    break;	
		case FX_J:
		case FX_K:
		case FX_L:
		case FX_M:			/* Note slide */
		case FX_N:			/* Reverse sample */
		    event->fxt=event->fxp=0;
		    break;
		}
		if(event->fxt>FX_M) event->fxt=event->fxp=0;
	    } 
	    if(b&PTM_VOL_FOLLOWS) {
		fread(&n,1,1,f);
		event->vol=n+1;
	    }
	}
	if(opt.verbose) report(".");
    }

    if(opt.verbose) report("\nStored samples : %d ",xxh->smp);
    for(i=0;i<xxh->smp;i++) {
	if(!xxs[i].len) continue;
	fseek(f,smp_ofs[xxi[i][0].sid],SEEK_SET);
	/*xxs[xxi[i][0].sid].len--;*/
	device_writepatch(f,d,xxi[i][0].sid,c4_rate,
	    XMP_SMP_8BDIFF,&xxs[xxi[i][0].sid]);
	if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");
    
    vol_xlat=(int*)&ptm_vol;

    for(i=0;i<xxh->chn;i++)
	xxc[i].pan=pfh.chset[i]<<4;

    return 0;
}

#endif /* LOAD_PTM */
