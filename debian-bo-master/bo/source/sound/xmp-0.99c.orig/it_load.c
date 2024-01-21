/* it_load.c - Impulse Tracker module loader
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id$
 *
 * $Log$
 */

#include "config.h"

#ifdef LOAD_IT

#include "load.h"
#include "it.h"

#ifdef MATH_LOG
#include <math.h>
#endif


struct ffx_xlat {
    int pv;
    int pu;
    int pd;
    int v;
    int u;
    int d;
};


static uint32 *pp_ins;		/* Pointers to instruments */
static uint32 *pp_smp;		/* Pointers to samples */
static uint32 *pp_pat;		/* Pointers to patterns */


static uint8 fx[] = {
    FX_0, FX_F, FX_B, FX_D, FX_A, FX_2, FX_1, FX_3,
    FX_4, FX_T, FX_Z, FX_6, FX_5, NONE, NONE, FX_9,
    NONE, FX_R, FX_7, XTND, FX_F, NONE, NONE
};


static void xlat_fx(int c,struct xxm_event *e)
{
    uint8 h=MSN(e->fxp),l=LSN(e->fxp);
    static struct ffx_xlat ffx[32];

    if((e->fxt==0x01)&&(e->fxp>0x1f)) e->fxp=0x1f;
    if((e->fxt==0x14)&&(e->fxp<0x20)) e->fxp=0x20;

    switch(e->fxt=fx[e->fxt]) {
    case FX_A:	/* Fine volume slides */
	if(!e->fxp&&ffx[c].pv) { e->fxt=ffx[c].pv; e->fxp=ffx[c].v; }
	if(h==0xf) { e->fxt=FX_E; e->fxp=0xb0+l; }
	if(h==0xe) { e->fxt=FX_E; e->fxp=0xb0+l; }
	if(l==0xf) { e->fxt=FX_E; e->fxp=0xa0+h; }
	if(l==0xe) { e->fxt=FX_E; e->fxp=0xa0+h; }
	ffx[c].pv=e->fxt; ffx[c].v=e->fxp;
	break;
    case FX_2:	/* Fine portamento down */
	if(!e->fxp&&ffx[c].pd) { e->fxt=ffx[c].pd; e->fxp=ffx[c].d; }
	if(h==0xf) { e->fxt=FX_E; e->fxp=0x20+l; }
	if(h==0xe) { e->fxt=FX_X; e->fxp=0x20+l; }
	ffx[c].pd=e->fxt; ffx[c].d=e->fxp;
	break;
    case FX_1:	/* Fine portamento up */
	if(!e->fxp&&ffx[c].pu) { e->fxt=ffx[c].pu; e->fxp=ffx[c].u; }
	if(h==0xf) { e->fxt=FX_E; e->fxp=0x10+l; }
	if(h==0xe) { e->fxt=FX_X; e->fxp=0x10+l; }
	ffx[c].pu=e->fxt; ffx[c].u=e->fxp;
	break;
    case XTND:	/* Extended effect */
	e->fxt=FX_E;
	switch(h) {
	    case 0x1: e->fxp=LSN(e->fxp)|0x30; break;	/* Glissando */
	    case 0x2: e->fxp=LSN(e->fxp)|0x50; break;	/* Finetune */
	    case 0x3: e->fxp=LSN(e->fxp)|0x40; break;	/* Vibrato wave */
	    case 0x4: e->fxp=LSN(e->fxp)|0x70; break;	/* Tremolo wave */
	    case 0x5:
	    case 0x6:
	    case 0x7:
	    case 0x9:
	    case 0xa: e->fxt=e->fxp=0; break;		/* Ignore */
	    case 0x8: e->fxt=0x08; e->fxp=l<<4; break;	/* Set pan */
	    case 0xb: e->fxp=LSN(e->fxp)|0x60; break;	/* Pattern loop */
	}
	break;
    case NONE:	/* No effect */
	e->fxt=e->fxp=0;
	break;
    }
}


int it_load(FILE *f,struct dev_info *d)
{
    int r,c,i,j,pat_len;
    struct xxm_event *event;
    struct it_file_header ifh;
    struct it_instrument1_header i1h;
/*
    struct it_instrument2_header i2h;
*/
    struct it_sample_header ish;
    uint8 b,mask[L_CHANNELS];
    uint8 *cvt;
    uint16 fodder;

    LOAD_INIT();

    /* Load and convert header */
    fread(&ifh,1,sizeof(ifh),f);
    if(strncmp((char*)ifh.magic,"IMPM",4)) return -1;
    L_ENDIAN16(ifh.ordnum);
    L_ENDIAN16(ifh.insnum);
    L_ENDIAN16(ifh.smpnum);
    L_ENDIAN16(ifh.patnum);
    L_ENDIAN16(ifh.cwtv);
    L_ENDIAN16(ifh.cmwt);
    L_ENDIAN16(sfh.msglen);
    L_ENDIAN32(sfh.msgofs);
    strcpy(module_name,(char*)ifh.name);
    strncpy((char*)xxh->name,module_name,22);
    xxh->len=ifh.ordnum;
    xxh->ins=ifh.insnum;
    xxh->smp=ifh.smpnum;
    xxh->pat=ifh.patnum;
    pp_ins=calloc(4,xxh->ins);
    pp_smp=calloc(4,xxh->smp);
    pp_pat=calloc(4,xxh->pat);
    xxh->tpo=ifh.is;	
    xxh->bpm=ifh.it;	
    xxh->flg|=XXM_FLG_LOOPS3M;
    cvt=calloc(1,xxh->smp);
    for(i=0;i<32;i++) xxc[i].vol=ifh.chvol[i];
    for(i=0;i<32;i++) {
	if(ifh.chpan[i]==100) ifh.chpan[i]=32;
	if(~ifh.chpan[i]&0x80) xxh->chn=i+1;
	else ifh.chvol[i]=0;
	xxc[i].pan=(int)ifh.chpan[i]*0x80/32;
    }
    fread(xxo,1,xxh->len,f);
    /* S3M skips pattern 0xfe */
    for(i=0;i<(xxh->len-1);i++)
	if(xxo[i]==0xfe) {
	    memcpy(&xxo[i],&xxo[i+1],xxh->len-i-1);
	    xxh->len--;
	}

    for(i=0;i<xxh->ins;i++) {
	fread(&pp_ins[i],4,1,f);
	L_ENDIAN32(pp_ins[i]);
    }
    for(i=0;i<xxh->smp;i++) {
	fread(&pp_smp[i],4,1,f);
	L_ENDIAN32(pp_smp[i]);
    }
    for(i=0;i<xxh->pat;i++) {
	fread(&pp_pat[i],4,1,f);
	L_ENDIAN32(pp_pat[i]);
    }

    c4_rate=C4_NTSC_RATE;

    if(opt.verbose) {
	report("Impulse Tracker %d.%02d module: %-.28s\n",
	    ifh.cmwt/256,ifh.cmwt%256,module_name);
	report("Module length: %d patterns\n",xxh->len);
    }

    if(opt.verbose>1) {
	report("Stereo %s\n",ifh.flags&IT_STEREO?"enabled":"disabled");
	report("Using %s mode\n",ifh.flags&IT_USE_INST?"instrument":"sample");
	report("Using %s effects\n",ifh.flags&IT_OLD_FX?"old":"IT");
    }
    
    if(~ifh.flags&IT_USE_INST) xxh->ins=xxh->smp;
    INSTRUMENT_INIT();

    if(xxh->ins) report("Stored instruments: %d\n",xxh->ins);

    for(i=0;(ifh.flags&IT_USE_INST)&&(ifh.cmwt<0x200)&&(i<xxh->ins);i++) {
	fseek(f,pp_ins[i],SEEK_SET);
	fread(&i1h,1,sizeof(i1h),f);
	L_ENDIAN16(i1h.fadeout);
	str_adj((char*)i1h.name);
	strncpy((char*)xxih[i].name,i1h.name,24);
	xxih[i].nsm=96;			/* Yikes!!! */
	xxih[i].rls=i1h.fadeout;
	if(opt.verbose&&strlen((char*)i1h.name))
	    report("[%2X] %-26.26s\n",i,i1h.name);
	/* Well here we'll create one instrument definition for each note
	 * in the keyboard/instrument table */
	xxi[i]=calloc(sizeof(struct xxm_instrument),96);
	for(j=0;j<96;j++) {
	    xxi[i][j].xpo=j-i1h.keys[24+j*2];
	    xxi[i][j].sid=i1h.keys[25+j*2];
	}
    }

    if(opt.verbose) report("Stored samples: %d",xxh->smp);

    for(i=0;i<xxh->smp;i++) {
	if(~ifh.flags&IT_USE_INST)
	    xxi[i]=calloc(sizeof(struct xxm_instrument),1);
	fseek(f,pp_smp[i],SEEK_SET);
	fread(&ish,1,sizeof(ish),f);
    	if(strncmp(ish.magic,"IMPS",4)) return -1;
	L_ENDIAN32(ish.length);
	L_ENDIAN32(ish.loopbeg);
	L_ENDIAN32(ish.loopend);
	L_ENDIAN32(ish.c5spd);
	xxs[i].len=ish.length*(1+!!(ish.flags&IT_SMP_16BIT));
	xxs[i].lps=ish.loopbeg*(1+!!(ish.flags&IT_SMP_16BIT));
	xxs[i].lpe=ish.loopend*(1+!!(ish.flags&IT_SMP_16BIT));
	xxs[i].flg=ish.flags&IT_SMP_16BIT?WAVE_16_BITS:0;
	xxs[i].flg|=ish.flags&IT_SMP_LOOP?WAVE_LOOPING:0;
	xxs[i].flg|=ish.flags&IT_SMP_BLOOP?WAVE_BIDIR_LOOP:0;
        cvt[i]=ish.convert&1;
	if(~ifh.flags&IT_USE_INST) {
	    xxi[i][0].vol=ish.vol;
	    xxi[i][0].pan=0x80;
	    xxi[i][0].sid=i;
	    xxih[i].nsm=!!(xxs[i].len);
	    str_adj((char*)ish.name);
	    strncpy((char*)xxih[i].name,ish.name,24);
	}
	if(((opt.verbose&&(~ifh.flags&IT_USE_INST))||(opt.verbose>1))&&
	    (strlen((char*)ish.name)||(xxs[i].len>1)))
	    report("\n[%2X] %-26.26s %05x %05x %05x %c V%02x %5d",
		i,ish.name,xxs[i].len,xxs[i].lps,xxs[i].lpe,
		xxs[i].flg&WAVE_LOOPING?xxs[i].flg&WAVE_BIDIR_LOOP?'B':'L':' ',
		ish.vol,ish.c5spd);
	else if((opt.verbose==1)&&(ifh.flags&IT_USE_INST)) report(".");

	/* Convert C5SPD to relnote/finetune */
	if(~ifh.flags&IT_USE_INST) {
#ifdef MATH_LOG
	    c=(1200.0*log(ish.c5spd)-1200.0*log(c4_rate))/M_LN2;
	    xxi[i][0].xpo=c/100;
	    xxi[i][0].fin=128*(c%100)/100;
#else
	    c2spd_to_note(ish.c5spd,&xxi[i][0].xpo,&xxi[i][0].fin);
#endif
	}
    }
    if(opt.verbose) report("\n");

    if(opt.verbose)
	report("Stored Patterns: %d ",xxh->pat);

    PATTERN_INIT();

    /* Read patterns */
    for(i=0;i<xxh->pat;i++) {
	PATTERN_ALLOC(i);
	r=0;
	fseek(f,pp_pat[i],SEEK_SET);
	fread(&fodder,2,1,f);
	L_ENDIAN16(fodder);
	pat_len=fodder-8;
	fread(&fodder,2,1,f);
	L_ENDIAN16(fodder);
	xxp[i]->rows=fodder;
	TRACK_ALLOC(i);
	memset(mask,0,L_CHANNELS);
	fread(&fodder,2,1,f);
	fread(&fodder,2,1,f);
	while(--pat_len>=0) {
	    fread(&b,1,1,f);
	    if(!b) { r++; continue; }
	    c=(b-1)&63;
	    if(b&0x80) { fread(&mask[c],1,1,f); pat_len--; }
	    event=&EVENT(i,c,r);
	    if(mask[c]&0x01) {
		fread(&b,1,1,f); 
		switch(b) {
		    case 255: b=0; break; 	/* Empty note */
		    case 254: b=0x61; break;	/* Key off */
		    default : b-=11;
		}
		if(b>96) b=0;
		event->not=b;
		pat_len--;
	    }
	    if(mask[c]&0x02) {
		fread(&b,1,1,f);
		event->ins=b;
		pat_len--;
	    }
	    if(mask[c]&0x04) {
		fread(&b,1,1,f);
		if(b<=0x40) event->vol=b+0x10;
 		pat_len--;
	    }
	    if(mask[c]&0x08) {
		fread(&b,1,1,f);
		event->fxt=b;
		fread(&b,1,1,f);
		event->fxp=b;
		xlat_fx(c,event);
		pat_len-=2;
	    } 
	}
	if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");

    for(i=0;i<xxh->smp;i++)
	device_writepatch(f,d,i,c4_rate,cvt[i]?0:XMP_SMP_UNS,&xxs[i]);

    free(cvt);
    return 0;
}

#endif /* LOAD_IT */
