/* 669_load.c - Composer 669 module loader
 * Copyright (C) 1997 C.Matsuoka and H.Carraro Jr
 *
 * $Id: 669_load.c,v 1.4 1997/03/17 21:01:24 claudio Exp $
 *
 * $Log: 669_load.c,v $
 * Revision 1.4  1997/03/17 21:01:24  claudio
 * Secondary effects used for tempo/break events.
 *
 * Revision 1.3  1997/03/13 13:02:29  claudio
 * Volume adjust & minor changes.
 *
 * Revision 1.2  1997/02/11 00:28:18  claudio
 * Verbosity levels adjusted.
 *
 * Revision 1.1  1997/02/08 00:18:55  claudio
 * Initial revision
 *
 */

#include "config.h"

#ifdef LOAD_669

#include "load.h"
#include "669.h"


static uint8 fx[] = {
    FX_0, FX_0, FX_0, FX_0,
    FX_0, FX_0, FX_0, FX_0,
    FX_0, FX_0, FX_1, FX_2,
    FX_3, FX_E, FX_4, FX_0
};


int ssn_load(FILE *f,struct dev_info *d)
{
    int i,j;
    struct xxm_event *event;
    struct ssn_file_header sfh;
    struct ssn_instrument_header sih;
    uint8 ev[3];

    LOAD_INIT();

    fread(&sfh,1,sizeof(sfh),f);
    if(strncmp((char*)sfh.marker,"if",2)&&strncmp((char*)sfh.marker,"JN",2))
        return -1;
    if(sfh.order[127]!=0xff) return -1;

    xxh->chn=8;
    xxh->ins=sfh.nos;
    xxh->pat=sfh.nop;
    xxh->trk=xxh->chn*xxh->pat;
    for(i=0;i<128;i++) if(sfh.order[i]>sfh.nop) break;
    xxh->len=i;
    memcpy(xxo,sfh.order,xxh->len);
    xxh->tpo=6;
    xxh->bpm=0x50;
    xxh->smp=xxh->ins;

    strcpy(module_type,strncmp((char*)sfh.marker,"if",2)?
	"UNIS 669 module":"Composer 669 module");

    MODULE_INFO();

    if(opt.verbose) {
	report("Message [1]    : %-36.36s\n",sfh.message);
	report("Message [2]    : %-36.36s\n",sfh.message+36);
	report("Message [3]    : %-36.36s\n",sfh.message+72);
    }

    INSTRUMENT_INIT();

    /* Read and convert instruments and samples */
    for(i=0;i<xxh->ins;i++) {
        xxi[i]=calloc(sizeof(struct xxm_instrument),1);
	fread(&sih,1,sizeof(sih),f);
	L_ENDIAN32(sih.length);
	L_ENDIAN32(sih.loopstart);
	L_ENDIAN32(sih.loopend);
	xxih[i].nsm=!!(xxs[i].len=sih.length);
        xxs[i].lps=sih.loopstart;
        xxs[i].lpe=sih.loopend>=0xfffff?0:sih.loopend;
        xxs[i].flg=xxs[i].lpe?WAVE_LOOPING:0;	/* 1 == Forward loop */
        xxi[i][0].vol=0x40;
        xxi[i][0].pan=0x80;
        xxi[i][0].sid=i;
        strncpy((char*)xxih[i].name,sih.name,13);
        str_adj((char*)xxih[i].name);
        if((opt.verbose>1)&&(strlen((char*)xxih[i].name)||(xxs[i].len>2)))
            report("[%2X] %-14.14s %04x %04x %04x %c\n",i,
	    xxih[i].name,xxs[i].len,xxs[i].lps,xxs[i].lpe,
	    xxs[i].flg&WAVE_LOOPING?'L':' ');
    }

    PATTERN_INIT();

    /* Read and convert patterns */
    if(opt.verbose) report("Stored patterns: %d ",xxh->pat);
    for(i=0;i<xxh->pat;i++) {
	PATTERN_ALLOC(i);
        xxp[i]->rows=64;
	TRACK_ALLOC(i);
	EVENT(i,0,0).f2t=FX_F;
	EVENT(i,0,0).f2p=sfh.tempo[i];
	EVENT(i,1,sfh.pbrk[i]).f2t=FX_D;
        for(j=0;j<64*8;j++) {
	    event=&EVENT(i,j%8,j/8);
            fread(&ev,1,3,f);
	    if((ev[0]&0xfe)!=0xfe) {
		event->not=25+(ev[0]>>2);
	        event->ins=1+MSN(ev[1])+((ev[0]&0x03)<<4);
	    }
	    if(ev[0]!=0xff) event->vol=(LSN(ev[1])<<2)+1;
	    if(ev[2]+1) {
		event->fxt=fx[MSN(ev[2])];
		event->fxp=fx[LSN(ev[2])];
		if(!event->fxp) event->fxp=0;
	 	switch(event->fxt) {
		case FX_0:
		    event->fxp=0;
		    break;
		case FX_4:
		    event->fxp|=0x80;	/* Wild guess */
		    break;
		case FX_E:
		    event->fxp=0x53;	/* Wild guess */
		    break;
		}
	    }
        }
        if(opt.verbose) report(".");
    }

    /* Read samples */
    if(opt.verbose) report("\nStored samples : %d ",xxh->smp);
    for(i=0;i<xxh->ins;i++) {
	if(xxs[i].len<=2) continue;
        device_writepatch(f,d,xxi[i][0].sid,c4_rate,XMP_SMP_UNS,&xxs[i]);
        if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");

    for(i=0;i<xxh->chn;i++) xxc[i].pan=(i%2)*0xff;
    return 0;
}

#endif /* LOAD_669 */
