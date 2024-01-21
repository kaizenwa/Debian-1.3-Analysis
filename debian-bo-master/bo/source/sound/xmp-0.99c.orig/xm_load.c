/* xm.h - Fasttracker II extended module format definitions
 * Copyright (C) 199[67] C.Matsuoka and H.Carraro Jr
 *
 * $Id: xm_load.c,v 1.4 1997/03/20 02:37:30 claudio Exp $
 *
 * $Log: xm_load.c,v $
 * Revision 1.4  1997/03/20 02:37:30  claudio
 * Patterns with 0 (==0xff) rows correctly handled.
 *
 * Revision 1.3  1997/03/18 16:04:37  claudio
 * Volume column effect M (tone portamento) fixed.
 *
 * Revision 1.2  1997/03/13 13:21:58  claudio
 * Lots of changes for xmp 0.99.
 *
 * Revision 1.1  1996/12/07 17:44:59  claudio
 * Initial revision
 *
 */

#include "config.h"

#ifdef LOAD_XM

#include "load.h"
#include "xm.h"


int xm_load(FILE *f,struct dev_info *d)
{
    int i,j,r;
    int instr_no=0;
    uint8 *patbuf,*p,b;
    struct xxm_event *event;
    struct xm_file_header xfh;
    struct xm_pattern_header xph;
    struct xm_instrument_header xih;
    struct xm_instrument xi;
    struct xm_sample_header xsh[16];

    LOAD_INIT();

    fread(&xfh,sizeof(xfh),1,f);
    if(strncmp((char *)xfh.id,"Extended Module: ",17)) return -1;
    strncpy(module_name,(char*)xfh.name,20);
    strcpy(module_type,"Fast Tracker II module");

    L_ENDIAN16(xfh.songlen);
    L_ENDIAN16(xfh.restart);
    L_ENDIAN16(xfh.channels);
    L_ENDIAN16(xfh.patterns);
    L_ENDIAN16(xfh.instruments);
    L_ENDIAN16(xfh.tempo);
    L_ENDIAN16(xfh.bpm);

    xxh->len=xfh.songlen;
    xxh->rst=xfh.restart;
    xxh->chn=xfh.channels;
    xxh->pat=xfh.patterns;
    xxh->trk=xxh->chn*xxh->pat;
    xxh->ins=xfh.instruments;
    xxh->tpo=xfh.tempo;
    xxh->bpm=xfh.bpm;
    xxh->flg=xfh.flags&1;
    memcpy(xxo,xfh.order,xxh->len);

    MODULE_INFO();

    PATTERN_INIT();

    if(opt.verbose) report("Stored patterns: %d ",xxh->pat);
    for(i=0;i<xxh->pat;i++) {
        fread(&xph,sizeof(xph),1,f);
	L_ENDIAN16(xph.rows);
	L_ENDIAN16(xph.datasize);
	PATTERN_ALLOC(i);
	if(!(r=xxp[i]->rows=xph.rows)) r=0x100;
	TRACK_ALLOC(i);

        p=patbuf=calloc(1,xph.datasize);
        fread(patbuf,1,xph.datasize,f);
	for(j=0;j<(xxh->chn*r);j++) {
	    if((p-patbuf)>xph.datasize) break;
            event=&EVENT(i,j%xxh->chn,j/xxh->chn);
            if((b=*p++)&XM_EVENT_PACKING) {
                if(b&XM_EVENT_NOTE_FOLLOWS) event->not=*p++;
                if(b&XM_EVENT_INSTRUMENT_FOLLOWS) {
                    if(*p&XM_END_OF_SONG) break;
                    event->ins=*p++;
                }
                if(b&XM_EVENT_VOLUME_FOLLOWS) event->vol=*p++;
                if(b&XM_EVENT_FXTYPE_FOLLOWS) event->fxt=*p++;
                if(b&XM_EVENT_FXPARM_FOLLOWS) event->fxp=*p++;
            } else {
                event->not=b;
                event->ins=*p++;
                event->vol=*p++;
                event->fxt=*p++;
                event->fxp=*p++;
            }
	    if(!event->fxt&&event->fxp) event->fxp=FX_Z;
	    if(!event->vol) continue;

	    if((event->vol>=0x10)&&(event->vol<=0x50)) {   /* Volume set */
		event->vol-=0x0f;
		continue;
	    } else {
		event->f2t=event->fxt;
		event->f2p=event->fxp;
		event->fxt=event->fxp=0;
	    }
	    switch(event->vol>>4) {
	    case 0x06:				/* Volume slide down */
		event->fxt=FX_A;
		event->fxp=event->vol-0x60;
		break;
	    case 0x07:				/* Volume slide up */
		event->fxt=FX_A;
		event->fxp=(event->vol-0x70)<<4;
		break;
	    case 0x08:				/* Fine volume slide down */
		event->fxt=FX_E;
		event->fxp=0xb0|(event->vol-0x80);
		break;
	    case 0x09:				/* Fine volume slide up */
		event->fxt=FX_E;
		event->fxp=0xa0|(event->vol-0x90);
		break;
	    case 0x0a:				/* Set vibrato speed */
		event->fxt=FX_4;
	        event->fxp=(event->vol-0xa0)<<4;
		break;
	    case 0x0b:				/* Vibrato */
		event->fxt=FX_4;
	        event->fxp=event->vol-0xb0;
		break;
	    case 0x0c:				/* Set panning */
		event->fxt=FX_8;
		event->fxp=((event->vol-0xc0)<<4)+8;
		break;
	    case 0x0d:				/* Pan slide left */
		event->fxt=FX_P;
		event->fxp=(event->vol-0xd0)<<4;
		break;
	    case 0x0e:				/* Pan slide right */
		event->fxt=FX_P;
		event->fxp=event->vol-0xe0;
		break;
	    case 0x0f:				/* Tone portamento */
		event->fxt=FX_3;
		event->fxp=(event->vol-0xf0)<<4;
		break;
	    }
	    event->vol=0;
	}
        free(patbuf);
        if(opt.verbose) report(".");
    }

    PATTERN_ALLOC(i);
    xxp[i]->rows=64;
    xxt[i*xxh->chn]=calloc(1,sizeof(struct xxm_track)+
	sizeof(struct xxm_event)*64);
    xxt[i*xxh->chn]->rows=64;
    for(j=0;j<xxh->chn;j++) xxp[i]->info[j].index=i*xxh->chn;

    if(opt.verbose) report("\nInstruments    : %d ",xxh->ins);
    if(opt.verbose>1) report("\n");

    /* ESTIMATED value! We don't know the actual value at this point */
    xxh->smp=255;

    INSTRUMENT_INIT();

    for(i=0;i<xxh->ins;i++) {
        fread(&xih,sizeof(xih),1,f);
	L_ENDIAN16(xih.samples);
	strncpy(xxih[i].name,xih.name,22);
	str_adj(xxih[i].name);
	xxih[i].nsm=xih.samples;
        if((opt.verbose>1)&&(strlen((char*)xxih[i].name)||xxih[i].nsm))
            report("[%2X] %-22.22s %2d ",i,xxih[i].name,xxih[i].nsm);

        xxi[i]=calloc(sizeof(struct xxm_instrument),xxih[i].nsm);
        if(xxih[i].nsm) {
            fread(&xi,sizeof(xi),1,f);
	    for(j=0;j<24;j++) {
                L_ENDIAN16(xi.v_env[j]);
	        L_ENDIAN16(xi.p_env[j]);
            }
	    L_ENDIAN16(xi.v_fade);

	    /* Envelope */
	    xxih[i].rls=xi.v_fade;
	    xxih[i].aei.npt=xi.v_pts;
	    xxih[i].aei.sus=xi.v_sus;
	    xxih[i].aei.lps=xi.v_start;
	    xxih[i].aei.lpe=xi.v_end;
	    xxih[i].aei.flg=xi.v_type;
	    xxih[i].pei.npt=xi.p_pts;
	    xxih[i].pei.sus=xi.p_sus;
	    xxih[i].pei.lps=xi.p_start;
	    xxih[i].pei.lpe=xi.p_end;
	    xxih[i].pei.flg=xi.p_type;
	    xxae[i]=calloc(4,xxih[i].aei.npt);
	    xxpe[i]=calloc(4,xxih[i].pei.npt);
	    memcpy(xxae[i],xi.v_env,xxih[i].aei.npt*4);
	    memcpy(xxpe[i],xi.p_env,xxih[i].pei.npt*4);

	    memcpy(&xxim[i],xi.sample,96);
            for(j=0;j<xxih[i].nsm;j++) {
                fread(&xsh[j],sizeof(xsh[j]),1,f);
		L_ENDIAN32(xsh[j].length);
		L_ENDIAN32(xsh[j].loop_start);
		L_ENDIAN32(xsh[j].loop_length);
	        xxi[i][j].vol=xsh[j].volume;
	        xxi[i][j].pan=xsh[j].pan;
	        xxi[i][j].xpo=xsh[j].relnote;
	        xxi[i][j].fin=xsh[j].finetune;
	        xxi[i][j].vwf=xi.y_wave;
	        xxi[i][j].vde=xi.y_depth;
	        xxi[i][j].vra=xi.y_rate;
	        xxi[i][j].vsw=xi.y_sweep;
		xxi[i][j].sid=instr_no;
		xxs[instr_no].len=xsh[j].length;
		xxs[instr_no].lps=xsh[j].loop_start;
		xxs[instr_no].lpe=xsh[j].loop_start+xsh[j].loop_length;
		xxs[instr_no].flg=xsh[j].type&XM_SAMPLE_16BIT?WAVE_16_BITS:0;
		xxs[instr_no].flg|=xsh[j].type&XM_LOOP_FORWARD?WAVE_LOOPING:0;
		xxs[instr_no].flg|=xsh[j].type&XM_LOOP_PINGPONG?
		    WAVE_LOOPING|WAVE_BIDIR_LOOP:0;
		instr_no++;
            }
            for(j=0;j<xxih[i].nsm;j++) {
		if((opt.verbose>1)&&xsh[j].length)
		    report("%s[%1x] %05x%c%05x %05x %c "
		    	"V%02x F%+04d P%02x R%+03d",
			j?"\n\t\t\t\t":"\t",j,
			xxs[xxi[i][j].sid].len,
			xxs[xxi[i][j].sid].flg&WAVE_16_BITS?'+':' ',
			xxs[xxi[i][j].sid].lps,
			xxs[xxi[i][j].sid].lpe,
			xxs[xxi[i][j].sid].flg&WAVE_LOOPING?'L':
			xxs[xxi[i][j].sid].flg&WAVE_BIDIR_LOOP?'B':' ',
			xsh[j].volume,xsh[j].finetune,
			xsh[j].pan,xsh[j].relnote);
                device_writepatch(f,d,xxi[i][j].sid,c4_rate,
                    XMP_SMP_DIFF,&xxs[xxi[i][j].sid]);
            }            
            if(opt.verbose==1) report(".");
        }
        if((opt.verbose>1)&&(strlen((char*)xxih[i].name)||xih.samples))
            report("\n");
    }
    xxh->smp=instr_no;
    if(opt.verbose>1) report("Stored samples : %d",xxh->smp);
    if(opt.verbose) report("\n");
    for(i=0;i<xxh->chn;i++) xxc[i].pan=128;

    return 0;
}

#endif /* LOAD_XM */
