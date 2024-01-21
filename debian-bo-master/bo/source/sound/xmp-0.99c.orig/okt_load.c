/* okt_load.c - Oktalyzer module loader
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: okt_load.c,v 1.1 1997/03/13 13:28:27 claudio Exp $
 *
 * $Log: okt_load.c,v $
 * Revision 1.1  1997/03/13 13:28:27  claudio
 * Initial revision
 *
 */

#include "config.h"

#ifdef LOAD_OKT

#include "load.h"
#include "okt.h"


static int fx[] = {
    FX_0, FX_1, FX_2, FX_0, FX_0, FX_0, FX_0, FX_0,
    FX_0, FX_0, FX_Z, FX_Z, FX_Z, FX_0, FX_0, FX_0,
    FX_0, FX_0, FX_0, FX_0, FX_0, FX_0, FX_0, FX_0,
    FX_0, FX_B, FX_0, FX_0, FX_F, FX_0, FX_0, FX_C
};

int okt_load(FILE *f,struct dev_info *d)
{
    int i,j;
    struct xxm_event *event;
    struct okt_file_header ofh;
    struct okt_pattern_header oph;
    struct okt_sample_header osh;
    struct okt_event oe;

    LOAD_INIT();

    xxh->ins=36;
    xxh->chn=4;

    fread(&ofh,1,sizeof(ofh),f);
    if(strncmp((char*)ofh.magic,"OKTASONG",8)) return -1;

    strcpy(module_type,"Oktalyzer module");

    for(i=0;i<4;i++)
	if(B_ENDIAN16(ofh.ch_flags[i])) xxh->chn++;
    B_ENDIAN16(ofh.tempo);
    B_ENDIAN16(ofh.patterns);
    B_ENDIAN16(ofh.modlen);

    xxh->pat=ofh.patterns;
    xxh->trk=xxh->pat*xxh->chn;
    xxh->tpo=ofh.tempo;
    xxh->bpm=125;
    xxh->smp=xxh->ins;
    xxh->len=ofh.modlen;
    c4_rate=C4_NTSC_RATE;

    MODULE_INFO();

    INSTRUMENT_INIT();

    /* Read and convert instruments and samples */
    for(i=0;i<xxh->ins;i++) {
        xxi[i]=calloc(sizeof(struct xxm_instrument),1);
	B_ENDIAN32(ofh.ins[i].length);
	B_ENDIAN16(ofh.ins[i].loopstart);
	B_ENDIAN16(ofh.ins[i].looplen);
	xxih[i].nsm=!!(xxs[i].len=ofh.ins[i].length);
        xxs[i].lps=ofh.ins[i].loopstart;
        xxs[i].lpe=ofh.ins[i].loopstart+ofh.ins[i].looplen;
        xxs[i].flg=ofh.ins[i].looplen>2?WAVE_LOOPING:0;
        xxi[i][0].vol=ofh.ins[i].volume;
        xxi[i][0].pan=0x80;
        xxi[i][0].sid=i;
        strncpy((char*)xxih[i].name,ofh.ins[i].name,20);
        str_adj((char*)xxih[i].name);
        if((opt.verbose>1)&&(strlen((char*)xxih[i].name)||(xxs[i].len>1)))
            report("[%2X] %-14.14s %05x %05x %05x %c V%02x\n",i,
	    xxih[i].name,xxs[i].len,xxs[i].lps,xxs[i].lpe,xxs[i].flg
	    &WAVE_LOOPING?'L':' ',xxi[i][0].vol);
    }

    memcpy(xxo,ofh.orders,xxh->len);

    PATTERN_INIT();

    /* Read and convert patterns */
    if(opt.verbose) report("Stored patterns: %d ",xxh->pat);
    for(i=0;i<xxh->pat;i++) {
	fread(&oph,1,sizeof(oph),f);
	B_ENDIAN16(oph.rows);
	PATTERN_ALLOC(i);
        xxp[i]->rows=oph.rows;
	TRACK_ALLOC(i);
        for(j=0;j<oph.rows*xxh->chn;j++) {
	    event=&EVENT(i,j%xxh->chn,j/xxh->chn);
            fread(&oe,1,sizeof(oe),f);
	    memset(event,0,sizeof(*event));
	    if(oe.note) {
		event->not=36+oe.note;
	        event->ins=1+oe.ins;
	    }
	    event->fxt=fx[oe.fxt];
	    event->fxp=oe.fxp;
	    if((event->fxt==FX_C)&&(event->fxp>0x40)) {
		if(event->fxp<=0x50) {
		    event->fxt=FX_A;
		    event->fxp-=0x40;
		} else if (event->fxp<=0x60) {
		    event->fxt=FX_A;
		    event->fxp=(event->fxp-0x50)<<4;
                } else if (event->fxp<=0x70) {
		    event->fxt=FX_E;
		    event->fxp=0xb0|(event->fxp-0x60);
                } else if (event->fxp<=0x80) {
		    event->fxt=FX_E;
		    event->fxp=0xa0|(event->fxp-0x70);
		}
	    }
	    if(event->fxt==FX_Z)	/* Arpeggio fixup */
		event->fxp=(((24-MSN(event->fxp))%12)<<4)|LSN(event->fxp);
        }
        if(opt.verbose) report(".");
    }

    /* Read samples */
    if(opt.verbose) report("\nStored samples : %d ",xxh->smp);
    for(i=0;i<xxh->ins;i++) {
	fread(&osh,sizeof(osh),1,f);
        device_writepatch(f,d,xxi[i][0].sid,c4_rate,0,&xxs[xxi[i][0].sid]);
        if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");

    for(i=0;i<xxh->chn;i++) xxc[i].pan=(((i+1)/2)%2)*0xff;

    return 0;
}

#endif /* LOAD_OKT */
