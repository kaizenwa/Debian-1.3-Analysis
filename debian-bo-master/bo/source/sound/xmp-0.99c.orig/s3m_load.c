/* s3m_load.c - Scream Tracker 3 module loader
 * Copyright (C) 199[67] C.Matsuoka and H.Carraro Jr
 *
 * $Id: s3m_load.c,v 1.8 1997/03/20 02:34:29 claudio Exp $
 *
 * $Log: s3m_load.c,v $
 * Revision 1.8  1997/03/20 02:34:29  claudio
 * Fine/extrafine volume slides fixed, and junk code removed.
 *
 * Revision 1.7  1997/03/17 23:28:11  claudio
 * Pattern loop bugfixes.
 *
 * Revision 1.6  1997/03/17 21:02:50  claudio
 * Pattern loop kluge removed.
 *
 * Revision 1.5  1997/03/13 13:13:37  claudio
 * Code revision for xmp 0.99a (lots of changes).
 *
 * Revision 1.4  1997/01/19 01:45:07  claudio
 * Fixed fine pitchbend effect and pan. Added warning for Adlib channels.
 *
 * Revision 1.3  1997/01/02 00:19:19  claudio
 * Added support for Amiga octave range and C2SPD to relnote/finetune
 * conversion.
 *
 * Revision 1.2  1996/12/10 19:18:22  claudio
 * Cosmetic changes.
 *
 * Revision 1.1  1996/12/07 17:43:07  claudio
 * Initial revision
 *
 */

#include "config.h"

#ifdef LOAD_S3M

#include "load.h"
#include "s3m.h"

#ifdef MATH_LOG
#include <math.h>
#endif

#define NONE 0xff
#define XTND 0xfe

struct ffx_xlat {
    int pv;
    int pu;
    int pd;
    int v;
    int u;
    int d;
};


static uint16 *pp_ins;		/* Parapointers to instruments */
static uint16 *pp_pat;		/* Parapointers to patterns */
static struct ffx_xlat ffx[32];
static int lp_flag;

static uint8 fx[] = {
    FX_0, FX_F, FX_B, FX_D, FX_A, FX_2, FX_1, FX_3,
    FX_4, FX_T, FX_Z, FX_6, FX_5, NONE, NONE, FX_9,
    NONE, FX_R, FX_7, XTND, FX_F, NONE, NONE, NONE,
    NONE, NONE, NONE
};


static void xlat_fx(int c,struct xxm_event *e)
{
    uint8 h=MSN(e->fxp),l=LSN(e->fxp);

    if((e->fxt==0x01)&&(e->fxp>0x1f)) e->fxp=0x1f;
    if((e->fxt==0x14)&&(e->fxp<0x20)) e->fxp=0x20;

    switch(e->fxt=fx[e->fxt]) {
    case FX_A:	/* Fine volume slides */
        if(!e->fxp&&ffx[c].pv) { e->fxt=ffx[c].pv; e->fxp=ffx[c].v; }
        if(l&&h==0xf) { e->fxt=FX_E; e->fxp=0xb0+l; }
        if(l&&h==0xe) { e->fxt=FX_E; e->fxp=0xb0+l; }
        if(h&&l==0xf) { e->fxt=FX_E; e->fxp=0xa0+h; }
        if(h&&l==0xe) { e->fxt=FX_E; e->fxp=0xa0+h; }
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
            case 0xb: e->fxp=LSN(e->fxp)|0x60;
                      lp_flag=(LSN(e->fxp))?-1:1;
                      break;				/* Pattern loop */
        }
        break;
    case NONE:	/* No effect */
        e->fxt=e->fxp=0;
        break;
    }
}


int s3m_load(FILE *f,struct dev_info *d)
{
    int c,r,i;
    struct xxm_event *event=0,dummy;
    struct s3m_file_header sfh;
    struct s3m_instrument_header sih;
    int pat_len;
    uint8 n,b,scratch[80];
    uint16 x16;

    memset(ffx,0,sizeof(ffx));
    lp_flag=1;

    LOAD_INIT();

    /* Load and convert header */
    fread(&sfh,1,sizeof(sfh),f);
    if(strncmp((char*)sfh.magic,"SCRM",4)) return -1;
    L_ENDIAN16(sfh.ordnum);
    L_ENDIAN16(sfh.insnum);
    L_ENDIAN16(sfh.patnum);
    L_ENDIAN16(sfh.ffi);
    str_adj((char*)sfh.name);
    strcpy(module_name,(char*)sfh.name);
    strncpy((char*)xxh->name,module_name,22);
    xxh->len=sfh.ordnum;
    xxh->ins=sfh.insnum;
    xxh->smp=xxh->ins;
    xxh->pat=sfh.patnum;
    pp_ins=calloc(2,xxh->ins);
    pp_pat=calloc(2,xxh->pat);
    if(sfh.flags&S3M_AMIGA_RANGE) xxh->flg|=XXM_FLG_MODRNG;
    xxh->tpo=sfh.is;	
    xxh->bpm=sfh.it;	
    xxh->flg|=XXM_FLG_LOOPS3M;
    for(i=0;i<32;i++) {
        if(sfh.chset[i]!=S3M_CH_OFF) xxh->chn=i+1;
        else continue;
	if((opt.verbose>2)&&((sfh.chset[i]&S3M_CH_PAN)>15))
            report("Warning: channel %2d is Adlib\n",i);
        xxc[i].pan=sfh.mv&0x80?((sfh.chset[i]&S3M_CH_PAN)>>3)*0xff:0x80;
    }
    xxh->trk=xxh->pat*xxh->chn;

    fread(xxo,1,xxh->len,f);
    /* S3M skips pattern 0xfe */
    for(i=0;i<(xxh->len-1);i++)
        if(xxo[i]==0xfe) {
            memcpy(&xxo[i],&xxo[i+1],xxh->len-i-1);
            xxh->len--;
        }

    for(i=0;i<xxh->ins;i++) {
        fread(&pp_ins[i],2,1,f);
	L_ENDIAN16(pp_ins[i]);
    }
    for(i=0;i<xxh->pat;i++) {
        fread(&pp_pat[i],2,1,f);
	L_ENDIAN16(pp_pat[i]);
    }

    /* Default pan positions */
    for(i=0,sfh.dp-=0xfc;!sfh.dp&&n&&(i<32);i++) {
        fread(scratch,1,1,f);
        if(scratch[0]&&S3M_PAN_SET)
            xxc[i].pan=scratch[0]<<4;
    }

    c4_rate=C4_NTSC_RATE;

    sprintf(module_type,"Scream Tracker 3.%02x module",sfh.version&0xff);

    MODULE_INFO();

    PATTERN_INIT();

    /* Read patterns */
    if(opt.verbose) report("Stored patterns: %d ",xxh->pat);
    for(i=0;i<xxh->pat;i++) {
	PATTERN_ALLOC(i);
	xxp[i]->rows=64;
	TRACK_ALLOC(i);
        if(!pp_pat[i]) continue;
        fseek(f,pp_pat[i]*16,SEEK_SET);
        r=0;
        fread(&x16,2,1,f); pat_len=x16;
	L_ENDIAN16(pat_len);
        pat_len-=2;
        while(--pat_len>=0) {
            fread(&b,1,1,f);
            if(b==S3M_EOR) {
                r++; lp_flag++;
                continue;
            }
            c=b&S3M_CH_MASK;
            event=c>=xxh->chn?&dummy:&EVENT(i,c,r);
		if(!lp_flag) {
		    event->f2t=FX_E;
		    event->f2p=0x60;
		    lp_flag++;
		}
            if(b&S3M_NI_FOLLOW) {
                fread(&n,1,1,f); 
                switch(n) {
                    case 255: n=0; break; 	/* Empty note */
                    case 254: n=0x61; break;	/* Key off */
                    default : n=1+12*MSN(n)+LSN(n);
                }
                event->not=n;
                fread(&n,1,1,f);
                event->ins=n;
                pat_len-=2;
            }
            if(b&S3M_VOL_FOLLOWS) {
                fread(&n,1,1,f);
                event->vol=n+1;
 		pat_len--;
            }
            if(b&S3M_FX_FOLLOWS) {
                fread(&n,1,1,f);
                event->fxt=n;
                fread(&n,1,1,f);
                event->fxp=n;
                xlat_fx(c,event);
                pat_len-=2;
            } 
        }
        if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");

    if(opt.verbose>1) {
        report("Stereo enabled : %s\n",sfh.mv%0x80?"yes":"no");
        report("Pan settings   : %s\n",sfh.dp?"no":"yes");
    }

    INSTRUMENT_INIT();

    /* Read and convert instruments and samples */
    if(opt.verbose) report("Instruments    : %d ",xxh->ins);
    for(i=0;i<xxh->ins;i++) {
        xxi[i]=calloc(sizeof(struct xxm_instrument),1);
        fseek(f,pp_ins[i]*16,SEEK_SET);
	fread(&sih,1,sizeof(sih),f);
        if(sih.type>2) continue;
    	if((sih.type==1)&&strncmp(sih.magic,"SCRS",4)) return -2;
	L_ENDIAN16(sih.memseg);
	L_ENDIAN32(sih.length);
	L_ENDIAN32(sih.loopbeg);
	L_ENDIAN32(sih.loopend);
	L_ENDIAN16(sih.c2spd);
	xxih[i].nsm=!!(xxs[i].len=sih.length);
        xxs[i].lps=sih.loopbeg;
        xxs[i].lpe=sih.loopend;
        xxs[i].flg=sih.flags&1?WAVE_LOOPING:0;
        xxi[i][0].vol=sih.vol;
        xxi[i][0].pan=0x80;
        xxi[i][0].sid=i;
	sih.magic[0]=0;
	str_adj((char*)sih.name);
        strncpy((char*)xxih[i].name,sih.name,24);
        if((opt.verbose>1)&&(strlen((char*)sih.name)||xxs[i].len))
            report("\n[%2X] %-28.28s %04x %04x %04x %c V%02x %5d ",
	    i,sih.name,xxs[i].len,xxs[i].lps,xxs[i].lpe,xxs[i].flg
	    &WAVE_LOOPING?'L':' ',xxi[i][0].vol,sih.c2spd);

	/* Convert C2SPD to relnote/finetune */
#ifdef MATH_LOG
        c=(1200.0*log(sih.c2spd)-1200.0*log(c4_rate))/M_LN2;
        xxi[i][0].xpo=c/100;
        xxi[i][0].fin=128*(c%100)/100;
#else
	c2spd_to_note(sih.c2spd,&xxi[i][0].xpo,&xxi[i][0].fin);
#endif

        if(!xxs[i].len) continue;
        fseek(f,16L*sih.memseg,SEEK_SET);
        device_writepatch(f,d,xxi[i][0].sid,c4_rate,XMP_SMP_UNS,&xxs[i]);
	if(opt.verbose) report(".");
    }
    if(opt.verbose) report("\n");

    return 0;
}

#endif /* LOAD_S3M */
