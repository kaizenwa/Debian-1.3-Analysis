/* mod_load.c - Protracker module and variants loader
 * Copyright (C) 199[67] C.Matsuoka and H.Carraro Jr
 *
 * $Id: mod_load.c,v 1.6 1997/03/20 02:32:20 claudio Exp $
 *
 * $Log: mod_load.c,v $
 * Revision 1.6  1997/03/20 02:32:20  claudio
 * Sample info message changed to display loop end (instead of length).
 *
 * Revision 1.5  1997/03/18 15:49:43  claudio
 * Changed the algorithm to determine the number of patterns stored.
 *
 * Revision 1.4  1997/03/17 21:04:05  claudio
 * Added test to prevent text files to be loaded as mod15.
 *
 * Revision 1.3  1997/03/13 13:12:21  claudio
 * Code revision for xmp 0.99a (lots of changes).
 *
 * Revision 1.2  1997/01/02 00:15:11  claudio
 * Added support for 10-32 channel mods and NTSC timing.
 * Module restart fixed.
 *
 * Revision 1.1  1996/12/07 17:43:58  claudio
 * Initial revision
 *
 */

#include "config.h"

#ifdef LOAD_MOD

#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include "load.h"
#include "mod.h"

#define NINSTR 31

struct {
    char magic[5];
    int ch;
} mod_magic[] = {
    { "M.K.", 4 }, { "M!K!", 4 }, { "FLT4", 4 }, 
    { "FLT8", 8 }, { "OCTA", 8 }, { "",0 }
};


int mod_load(FILE *f,struct dev_info *d)
{
    int i,j;
    int smp_size,wow;
    struct xxm_event *event;
    struct stat st;
    union mod_header mh;
    uint8 mod_event[4];
    char id[32];

    LOAD_INIT();

    fstat(fileno(f),&st);
    xxh->tpo=6;
    xxh->bpm=125;
    xxh->ins=NINSTR;
    xxh->smp=xxh->ins;
    xxh->chn=0;
    smp_size=0;

    INSTRUMENT_INIT();

    memset(id,0,sizeof(id));
    fread(&mh,1,sizeof(struct mod_file_header),f);
    strncpy((char*)xxh->name,(char*)mh.m31.name,20);
    strncpy(module_name,(char*)mh.m31.name,20);
    strncpy(id,(char*)mh.m31.magic,4);

    if(xxh->ins>15) {
	for(i=0;mod_magic[i].ch;i++) {
	    if(!(strncmp((char*)mh.m31.magic,mod_magic[i].magic,4))) {
		xxh->chn=mod_magic[i].ch;
		break;
	    }
	}
	if(!xxh->chn) {
	    if(!strncmp((char*)mh.m31.magic+2,"CH",2) &&
	    isdigit(*mh.m31.magic)&&isdigit(mh.m31.magic[1])) {
		if((xxh->chn=(*mh.m31.magic-'0')*
		10+mh.m31.magic[1]-'0')>32) return -1;
	    } else if(!strncmp((char*)mh.m31.magic+1,"CHN",3)&&
		isdigit(*mh.m31.magic)) {
		if(!(xxh->chn=(*mh.m31.magic-'0'))) return -1;
            } else {
	        xxh->ins=xxh->smp=15;
	        xxh->chn=4;
	        strcpy(id,"15-instrument");
	        fseek(f,0,SEEK_SET);
	        fread(&mh,1,sizeof(struct mod15_file_header),f);
            }
	} 
    }

    if((xxh->ins==15)&&(mh.m15.ins[0].finetune>0x0f)) return -1;
    xxh->len=xxh->ins>15?mh.m31.songlen:mh.m15.songlen;
    xxh->rst=xxh->ins>15?mh.m31.endjump:mh.m15.endjump;
    if(xxh->rst>=0x7f) xxh->rst=0;
    memcpy(xxo,xxh->ins>15?mh.m31.orders:mh.m15.orders,128);

    /* Thunder's method to determine the number of patterns (shown below)
     * doesn't work for badly ripped mods. 
     * xxh->pat=(st.st_size-smp_size-header_size)/(64*xxh->chn*4);
     */

    for(i=0;i<128;i++)
	if(xxo[i]>xxh->pat) xxh->pat=xxo[i];
    xxh->pat++;

    for(i=0;i<xxh->ins;i++) {
	xxi[i]=calloc(sizeof(struct xxm_instrument),1);
	B_ENDIAN16(mh.m31.ins[i].length);
	B_ENDIAN16(mh.m31.ins[i].loopstart);
	B_ENDIAN16(mh.m31.ins[i].looplength);
	xxs[i].len=2*mh.m31.ins[i].length;
	xxs[i].lps=2*mh.m31.ins[i].loopstart;
	xxs[i].lpe=xxs[i].lps+2*mh.m31.ins[i].looplength;
	xxs[i].flg=mh.m31.ins[i].looplength>1?WAVE_LOOPING:0;
	xxi[i][0].fin=mh.m31.ins[i].finetune<<4;
	xxi[i][0].vol=mh.m31.ins[i].volume;
	xxi[i][0].pan=0x80;
	xxi[i][0].sid=i;
	xxih[i].nsm=!!(xxs[i].len);
	xxih[i].rls=0xfff;
	smp_size+=xxs[i].len;
	strncpy(xxih[i].name,mh.m31.ins[i].name,22);
	str_adj(xxih[i].name);
    } 

    /* Test for Mod's Grave WOW modules
     *
     * Stefan Danes <sdanes@marvels.hacktic.nl> said:
     * This weird format is identical to '8CHN' but still uses the 'M.K.' ID.
     * You can only test for WOW by calculating the size of the module for 8 
     * channels and comparing this to the actual module length. If it's equal, 
     * the module is an 8 channel WOW.
     */

    if((wow=(!strncmp((char*)mh.m31.magic,"M.K.",4)&&
	(0x43c+xxh->pat*8*4*0x40+smp_size==st.st_size))))
	xxh->chn=8;

    if(smp_size>st.st_size) return -2;
    if(xxh->pat>0x7f) return -1;
    if((xxh->len==0)||(xxh->len>0x7f)) return -1;
    xxh->trk=xxh->chn*xxh->pat;

    if(!wow)
        sprintf(module_type,"Protracker module (%s)",id);
    else
        sprintf(module_type,"Mod's Grave module (WOW)");

    MODULE_INFO();

    for(i=0;(opt.verbose>1)&&(i<xxh->ins);i++) {
        if((strlen((char*)xxih[i].name)||(xxs[i].len>2)))
	report("[%2X] %-22.22s %04x %04x %04x %c V%02x %+d\n",
	i,xxih[i].name,xxs[i].len,xxs[i].lps,
	xxs[i].lpe,mh.m31.ins[i].looplength>1?'L':' ',
	xxi[i][0].vol,(char)xxi[i][0].fin>>4);
    }

    PATTERN_INIT();

    /* Load and convert patterns */
    if(opt.verbose) report("Stored patterns: %d ",xxh->pat);
    for(i=0;i<xxh->pat;i++) {
	PATTERN_ALLOC(i);
	xxp[i]->rows=64;
	TRACK_ALLOC(i);
	for(j=0;j<(64*xxh->chn);j++) {
	    event=&EVENT(i,j%xxh->chn,j/xxh->chn);
	    fread(mod_event,1,4,f);

	    event->not=period_to_note((LSN(mod_event[0])<<8)+mod_event[1]);
	    event->ins=((MSN(mod_event[0])<<4)|MSN(mod_event[2]));
	    event->fxt=LSN(mod_event[2]);
	    event->fxp=mod_event[3];
	    if(!event->fxt&&event->fxp) event->fxt=FX_Z;
	    if(!event->fxp)
		switch(event->fxt) {
		case 0x05: event->fxt=0x03; break;
		case 0x06: event->fxt=0x04; break;
		case 0x01:
		case 0x02:
		case 0x0a: event->fxt=0x00;
	    }
	}
	if(opt.verbose) report(".");
    }

    if(opt.modrange) xxh->flg|=XXM_FLG_MODRNG;    
    if(opt.ntsc) {
	replay_rate=NTSC_RATE;
	c4_rate=C4_NTSC_RATE;
    }

    /* Load samples */
    if(opt.verbose) report("\nStored samples : %d ",xxh->smp);
    for(i=0;i<xxh->smp;i++) {
	if(!xxs[i].len) continue;
	device_writepatch(f,d,xxi[i][0].sid,c4_rate,0,&xxs[xxi[i][0].sid]);
	if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");

    for(i=0;i<xxh->chn;i++)
	xxc[i].pan=(((i+1)/2)%2)*0xff;

    return 0;
}

#endif /* LOAD_MOD */
