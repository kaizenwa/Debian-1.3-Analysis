/* mtm_load.c - Multitracker module loader
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: mtm_load.c,v 1.1 1997/03/06 13:49:05 claudio Exp $
 *
 * $Log: mtm_load.c,v $
 * Revision 1.1  1997/03/06 13:49:05  claudio
 * Initial revision
 *
 */

#include "config.h"

#ifdef LOAD_MTM

#include "load.h"
#include "mtm.h"


int mtm_load(FILE *f,struct dev_info *d)
{
    int i,j;
    struct mtm_file_header mfh;
    struct mtm_instrument_header mih;
    uint8 mt[192];
    uint16 mp[32];

    LOAD_INIT();

    fread(&mfh,1,sizeof(mfh),f);

    if(strncmp((char*)mfh.magic,"MTM",3)) return -1;

    xxh->trk=mfh.tracks+1;
    xxh->pat=mfh.patterns+1;
    xxh->len=mfh.modlen+1;
    xxh->ins=mfh.samples;
    xxh->smp=xxh->ins;
    xxh->chn=mfh.channels;
    xxh->tpo=6;
    xxh->bpm=125;

    strncpy(module_name,mfh.name,20);
    sprintf(module_type,"MultiTracker %d.%02d module",
        MSN(mfh.version),LSN(mfh.version));

    MODULE_INFO();

    INSTRUMENT_INIT();

    /* Read and convert instruments */
    for(i=0;i<xxh->ins;i++) {
        xxi[i]=calloc(sizeof(struct xxm_instrument),1);
	fread(&mih,1,sizeof(mih),f);
	L_ENDIAN32(mih.length);
	L_ENDIAN32(mih.loopstart);
	L_ENDIAN32(mih.loopend);
	xxih[i].nsm=!!(xxs[i].len=mih.length);
        xxs[i].lps=mih.loopstart;
        xxs[i].lpe=mih.loopend;
        xxs[i].flg=xxs[i].lpe?WAVE_LOOPING:0;	/* 1 == Forward loop */
	xxs[i].flg|=mfh.attr&1?WAVE_16_BITS:0;
        xxi[i][0].vol=mih.volume;
        xxi[i][0].fin=mih.finetune;
	xxi[i][0].pan=0x80;
	xxi[i][0].sid=i;
        strncpy((char*)xxih[i].name,mih.name,22);
        str_adj((char*)xxih[i].name);
        if((opt.verbose>1)&&(strlen((char*)xxih[i].name)||xxs[i].len))
            report("[%2X] %-22.22s %04x%c%04x %04x %c V%02x F%+03d\n",i,
	    xxih[i].name,xxs[i].len,xxs[i].flg&WAVE_16_BITS?'+':' ',
	    xxs[i].lps,xxs[i].lpe,xxs[i].flg&WAVE_LOOPING?'L':' ',
	    xxi[i][0].vol,xxi[i][0].fin);
    }

    fread(xxo,1,128,f);

    PATTERN_INIT();

    if(opt.verbose) report("Stored tracks  : %d ",xxh->trk-1);
    for(i=0;i<xxh->trk;i++) {
	xxt[i]=calloc(sizeof(struct xxm_track)+
	    sizeof(struct xxm_event)*mfh.rows,1);
	xxt[i]->rows=mfh.rows;
	if(!i) continue;
	fread(&mt,3,64,f);
	for(j=0;j<64;j++) {
	    if((xxt[i]->event[j].not=mt[j*3]>>2))
	        xxt[i]->event[j].not+=25;
	    xxt[i]->event[j].ins=((mt[j*3]&0x3)<<4)+MSN(mt[j*3+1]);
	    xxt[i]->event[j].fxt=LSN(mt[j*3+1]);
	    xxt[i]->event[j].fxp=mt[j*3+2];
	    if(xxt[i]->event[j].fxt>FX_F)
	        xxt[i]->event[j].fxt=xxt[i]->event[j].fxp=0;
	    /* Set pan effect translation */
	    if((xxt[i]->event[j].fxt==FX_E)&&
	       (MSN(xxt[i]->event[j].fxp)==0x8)) {
	    	xxt[i]->event[j].fxt=FX_8;
	    	xxt[i]->event[j].fxp<<=4;
	    }
	}
	if(opt.verbose&&!(i%xxh->chn)) report(".");
    }
    if(opt.verbose) report("\n");

    /* Read patterns */
    if(opt.verbose) report("Stored patterns: %d ",xxh->pat-1);
    for(i=0;i<xxh->pat;i++) {
	PATTERN_ALLOC(i);
        xxp[i]->rows=64;
        fread(&mp,2,32,f);
        for(j=0;j<xxh->chn;j++) {
	    L_ENDIAN16(mp[j]);
	    xxp[i]->info[j].index=mp[j];
	}
        if(opt.verbose) report(".");
    }

    /* Comments */
    for(i=0;i<mfh.extralen;i++)
	fread(&j,1,1,f);

    /* Read samples */
    if(opt.verbose) report("\nStored samples : %d ",xxh->smp);
    for(i=0;i<xxh->ins;i++) {
        device_writepatch(f,d,xxi[i][0].sid,c4_rate,
	    XMP_SMP_UNS,&xxs[xxi[i][0].sid]);
	if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");

    for(i=0;i<xxh->chn;i++) xxc[i].pan=mfh.pan[i]<<4;

    return 0;
}

#endif /* LOAD_MTM */
