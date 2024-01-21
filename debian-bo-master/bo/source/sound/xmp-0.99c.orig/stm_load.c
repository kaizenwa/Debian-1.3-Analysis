/* stm_load.c - Scream Tracker 2.21 module loader
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: stm_load.c,v 1.3 1997/03/13 13:15:06 claudio Exp $
 *
 * $Log: stm_load.c,v $
 * Revision 1.3  1997/03/13 13:15:06  claudio
 * Event volume bug fixed. Messages adjusted.
 *
 * Revision 1.2  1997/02/11 00:29:20  claudio
 * Conversion for XXM instead of XM.
 *
 * Revision 1.1  1997/01/26 22:41:17  claudio
 * Initial revision
 *
 */

#include "config.h"

#ifdef LOAD_STM

#include "load.h"
#include "stm.h"


static uint8 fx[] = {
    FX_0, FX_F, FX_B, FX_D, FX_A,
    FX_2, FX_1, FX_3, FX_4, FX_T
};


int stm_load(FILE *f,struct dev_info *d)
{
    int i,j;
    struct xxm_event *event;
    struct stm_file_header sfh;
    uint8 b;

    LOAD_INIT();

    xxh->ins=31;
    xxh->chn=4;

    fread(&sfh,1,sizeof(sfh),f);

    if(strncmp((char*)sfh.magic,"!Scream!",8)) return -1;
    if(sfh.type!=STM_TYPE_MODULE) return -1;

    xxh->pat=sfh.patterns;
    xxh->trk=xxh->pat*xxh->chn;
    xxh->tpo=MSN(sfh.tempo);
    xxh->bpm=125;
    xxh->smp=xxh->ins;
    c4_rate=C4_NTSC_RATE;

    strncpy(module_name,sfh.name,20);
    sprintf(module_type,"Scream Tracker %d.%02d module",
	sfh.vermaj,sfh.vermin);

    MODULE_INFO();

    INSTRUMENT_INIT();

    /* Read and convert instruments and samples */
    for(i=0;i<xxh->ins;i++) {
        xxi[i]=calloc(sizeof(struct xxm_instrument),1);
	L_ENDIAN16(sfh.ins[i].length);
	L_ENDIAN16(sfh.ins[i].loopbeg);
	L_ENDIAN16(sfh.ins[i].loopend);
	L_ENDIAN16(sfh.ins[i].c2spd);
	xxih[i].nsm=!!(xxs[i].len=sfh.ins[i].length);
        xxs[i].lps=sfh.ins[i].loopbeg;
        xxs[i].lpe=sfh.ins[i].loopend;
        if(xxs[i].lpe==0xffff) xxs[i].lpe=0;
        xxs[i].flg=xxs[i].lpe>0?WAVE_LOOPING:0;
        xxi[i][0].vol=sfh.ins[i].volume;
        xxi[i][0].pan=0x80;
        xxi[i][0].sid=i;
        strncpy((char*)xxih[i].name,sfh.ins[i].name,12);
        str_adj((char*)xxih[i].name);
        if((opt.verbose>1)&&(strlen((char*)xxih[i].name)||(xxs[i].len>1)))
            report("[%2X] %-14.14s %04x %04x %04x %c V%02x %5d\n",i,
	    xxih[i].name,xxs[i].len,xxs[i].lps,xxs[i].lpe,xxs[i].flg
	    &WAVE_LOOPING?'L':' ',xxi[i][0].vol,sfh.ins[i].c2spd);

	sfh.ins[i].c2spd=8363*sfh.ins[i].c2spd/8448;
	/* Convert C2SPD to relnote/finetune */
#ifdef MATH_LOG
        c=(1200.0*log(sfh.ins[i].c2spd)-1200.0*log(c4_rate))/M_LN2;
        xxi[i][0].xpo=c/100;
        xxi[i][0].fin=128*(c%100)/100;
#else
	c2spd_to_note(sfh.ins[i].c2spd,&xxi[i][0].xpo,&xxi[i][0].fin);
#endif
    }

    fread(xxo,1,128,f);
    for(i=0;i<128;i++) if(xxo[i]>xxh->pat) break;
    xxh->len=i;

    if(opt.verbose)
	report("Module length  : %d patterns\n",xxh->len);

    PATTERN_INIT();

    /* Read and convert patterns */
    if(opt.verbose) report("Stored patterns: %d ",xxh->pat);
    for(i=0;i<xxh->pat;i++) {
	PATTERN_ALLOC(i);
        xxp[i]->rows=64;
	TRACK_ALLOC(i);
        for(j=0;j<64*xxh->chn;j++) {
	    event=&EVENT(i,j%xxh->chn,j/xxh->chn);
            fread(&b,1,1,f);
	    memset(event,0,sizeof(*event));
	    switch(b) {
	    case 251:
	    case 252:
	    case 253:
		break;
	    case 255:
		b=0;
	    default:
		event->not=b?1+LSN(b)+12*(2+MSN(b)):0;
		fread(&b,1,1,f);
		event->vol=b&0x07;
		event->ins=(b&0xf8)>>3;
		fread(&b,1,1,f);
		event->vol+=(b&0xf0)>>1;
		if(event->vol>0x40) event->vol=0;
		else event->vol++;
		event->fxt=fx[LSN(b)];
		fread(&b,1,1,f);
		event->fxp=b;
		switch(event->fxt) {
		case FX_F:
		    event->fxp=MSN(event->fxp);
		    break;
		}
	    }
        }
        if(opt.verbose) report(".");
    }

    /* Read samples */
    if(opt.verbose) report("\nStored samples : %d ",xxh->smp);
    for(i=0;i<xxh->ins;i++) {
        device_writepatch(f,d,xxi[i][0].sid,c4_rate,0,&xxs[xxi[i][0].sid]);
        if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");

    for(i=0;i<xxh->chn;i++) xxc[i].pan=(((i+1)/2)%2)*0xff;

    return 0;
}

#endif /* LOAD_STM */
