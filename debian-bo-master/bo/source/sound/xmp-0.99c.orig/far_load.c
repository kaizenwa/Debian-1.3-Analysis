/* far_load.c - Farandole Composer module loader
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id$
 *
 * $Log$
 */

#include "config.h"

#ifdef LOAD_FAR

#include "load.h"
#include "far.h"


static uint8 fx[] = {
    FX_0, FX_1, FX_2, FX_3,
    FX_0, FX_X, FX_4, FX_Y,
    FX_A, FX_V, FX_0, FX_0,
    FX_E, FX_0, FX_0, FX_0
};


int far_load(FILE *f,struct dev_info *d)
{
    int i,j,vib=0;
    struct xxm_event *event;
    struct far_file_header ffh;
    struct far_file_header2 ffh2;
    struct far_instrument_header fih;
    struct far_event fe;
    uint8 sample_map[8];

    LOAD_INIT();

    fread(&ffh,1,sizeof(ffh),f);
    if(strncmp((char*)ffh.magic,"FAR",3)||(ffh.magic[3]!=0xfe))
        return -1;
    L_ENDIAN16(ffh.textlen);
    for(i=0;i<ffh.textlen;i++) fread(&j,1,1,f);
    fread(&ffh2,1,sizeof(ffh2),f);

    xxh->chn=16;
    xxh->pat=ffh2.patterns;
    xxh->len=ffh2.songlen;
    xxh->tpo=6;
    xxh->bpm=125;
    memcpy(xxo,ffh2.order,xxh->len);
    for(i=0;i<xxh->len;i++)
	if(xxo[i]>xxh->pat) xxh->pat=xxo[i];
    xxh->trk=xxh->chn*xxh->pat;

    strncpy(module_name,ffh.name,40);
    sprintf(module_type,"Farandole composer %d.%d module",
        MSN(ffh.version),LSN(ffh.version));
    
    MODULE_INFO();

    PATTERN_INIT();

    /* Read and convert patterns */
    if(opt.verbose) {
	report("Comment bytes  : %d\n",ffh.textlen);
	report("Stored patterns: %d ",xxh->pat);
    }
    for(i=0;i<xxh->pat;i++) {
	PATTERN_ALLOC(i);
	L_ENDIAN16(ffh2.patsize[i]);
        xxp[i]->rows=(ffh2.patsize[i]-2)/64;
	TRACK_ALLOC(i);
	fread(&j,1,2,f);
        for(j=0;j<xxp[i]->rows*xxh->chn;j++) {
            event=&EVENT(i,j%xxh->chn,j/xxh->chn);
            fread(&fe,1,4,f);
	    memset(event,0,sizeof(*event));
	    if(fe.note) event->not=fe.note+36;
	    if(event->not||fe.instrument) event->ins=fe.instrument+1;
	    fe.volume=LSN(fe.volume)*16+MSN(fe.volume);
	    if(fe.volume) event->vol=fe.volume-0x10;
	    event->fxt=fx[MSN(fe.effect)];
	    event->fxp=fx[LSN(fe.effect)];
	    switch(event->fxt) {
	    case FX_X:
		vib=event->fxp;
		event->fxt=event->fxp=0;
		break;
	    case FX_4:
		event->fxp=(event->fxp<<4)+vib;
		break;
	    case FX_Y:			/* Fine volume slide up */
		event->fxt=0x0e;
		event->fxp|=0xa;
		break;
	    case FX_A:			/* Fine volume slide down */
		event->fxt=0x0e;
		event->fxp|=0xb;
		break;
	    }
        }
        if(opt.verbose) report(".");
    }

    fread(sample_map,1,8,f);
    for(i=1;i<0x100;i<<=1) {
	for(j=0;j<8;j++) 
 	    if(sample_map[j]&i) xxh->ins++;
    }
    xxh->smp=xxh->ins;

    INSTRUMENT_INIT();

    /* Read and convert instruments and samples */
    if(opt.verbose) report("\nInstruments    : %d ",xxh->ins);
    for(i=0;i<xxh->ins;i++) {
        xxi[i]=calloc(sizeof(struct xxm_instrument),1);
	fread(&fih,1,sizeof(fih),f);
	L_ENDIAN32(fih.length);
	L_ENDIAN32(fih.loopstart);
	L_ENDIAN32(fih.loopend);
	fih.length&=0xffff;
	fih.loopstart&=0xffff;
	fih.loopend&=0xffff;
	xxih[i].nsm=!!(xxs[i].len=fih.length);
        xxs[i].lps=fih.loopstart;
        xxs[i].lpe=fih.loopend;
        xxs[i].flg=fih.sampletype?WAVE_16_BITS:0;
        xxs[i].flg|=fih.loopmode?WAVE_LOOPING:0;
        xxi[i][0].vol=fih.volume-1;
        xxi[i][0].sid=i;
        strncpy((char*)xxih[i].name,fih.name,24);
	fih.length=0; str_adj((char*)fih.name);
        if((opt.verbose>1)&&(strlen((char*)fih.name)||xxs[i].len))
            report("\n[%2X] %-32.32s %04x %04x %04x %c V%02x ",
		i,fih.name,xxs[i].len,xxs[i].lps,xxs[i].lpe,
		fih.loopmode?'L':' ',xxi[i][0].vol);
	if(sample_map[i/8]&(1<<(i%8)))
            device_writepatch(f,d,xxi[i][0].sid,c4_rate,0,&xxs[i]);
	if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");

    vol_base=0x100;

    for(i=0;i<xxh->chn;i++) xxc[i].pan=(((i+1)/2)%2)*0xff;

    return 0;
}

#endif /* LOAD_FAR */
