/* xm_play.c - Sequence and play extended extended modules (XXM)
 * Copyright (C) 1996 C.Matsuoka and H.Carraro Jr
 *
 * $Id: xm_play.c,v 1.7 1997/03/20 13:20:52 claudio Exp $
 *
 * $Log: xm_play.c,v $
 * Revision 1.7  1997/03/20 13:20:52  claudio
 * Fixed: continue vibrato effect, support for patterns with 0 (==0xff)
 * rows, vibrato/tremolo indexes in tone portamento.
 *
 * Revision 1.6  1997/03/18 16:13:55  claudio
 * Bugs fixed: sample offset, tempo setting, S3M pattern loop, instrument
 * retriggering, global volume setting and note delay.
 *
 * Revision 1.5  1997/03/13 13:22:28  claudio
 * Internal module format changed from XM to XXM and more changes
 * for xmp 0.99.
 *
 * Revision 1.4  1997/01/19 01:45:53  claudio
 * Fixed global volume slide and pattern loop.
 *
 * Revision 1.3  1997/01/02 00:21:27  claudio
 * Lots of changes. Check the Changes file for details.
 *
 * Revision 1.2  1996/12/10 19:17:05  claudio
 * Squeak, fadeout and pitchbending bugs fixed.
 *
 * Revision 1.1  1996/12/07 20:40:57  claudio
 * Initial revision
 *
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "xmp.h"
#include "xxm.h"
#include "devices.h"

#define NOT_IMPLEMENTED

#define SET(f) SET_FLAG(xc->flags,f)
#define RESET(f) RESET_FLAG(xc->flags,f)
#define TEST(f) TEST_FLAG(xc->flags,f)
#define INC_IN_RANGE(v,i,a,b) { v+=i; if(v>b) v=b; if(v<a) v=a; }
#define DO_ENVELOPE(t,i,e,s,a,b) { if((!(t&XXM_ENV_SUS)||\
    xc->release||(i<e[s*2]))&&(i<0xffff)) i++; if((t&XXM_ENV_LOOP)\
    &&(i>e[b*2])) i=e[a*2]; }

/* Pattern loop stack size */
#define ROW_MAX		0x0100

/* Global flags */
#define PATTERN_BREAK	0x0001 
#define PATTERN_LOOP	0x0002 
#define MODULE_ABORT	0x0004 
#define MODULE_RESTART	0x0008 

extern char module_name[MODULE_NAME_MAXSIZE];
extern struct options opt;
extern double replay_rate;
extern int c4_rate;
extern int vol_base;
extern int *vol_xlat;

extern struct xxm_header *xxh;
extern struct xxm_pattern **xxp;
extern struct xxm_track **xxt;
extern struct xxm_channel xxc[32];
extern uint8 xxo[256];
extern struct xxm_instrument_header *xxih;
extern struct xxm_instrument_map *xxim;
extern struct xxm_instrument **xxi;
extern struct xxm_sample *xxs;
extern uint16 **xxae;
extern uint16 **xxpe;
extern uint16 **xxfe;

extern struct xmp_channel xc[L_CHANNELS];
extern int p_channels;

static int global_flags=0;
static int row_cnt;
static int loop_row;
static int loop_cnt;
static uint8 *row_ptr;
static uint8 *loop_ptr;
static uint8 *loop_stack;

static int jump_pattern=-1;
static int delay_pattern=0;
static int jump_line=0;
static int global_vol;
static double tick_duration;
static int tempo;
static int global_vslide;
static int offset=0;

#define XXIH xxih[xc->ins]
#define XXIM xxim[xc->ins]
#define XXAE xxae[xc->ins]
#define XXPE xxpe[xc->ins]
#define XXFE xxfe[xc->ins]
#define XXI xxi[xc->ins]

/* Waveform tables borrowed from gmod */
static int waveform[4][64] = {
   {   0,  24,  49,  74,  97, 120, 141, 161, 180, 197, 212, 224,
     235, 244, 250, 253, 255, 253, 250, 244, 235, 224, 212, 197,
     180, 161, 141, 120,  97,  74,  49,  24,   0, -24, -49, -74,
     -97,-120,-141,-161,-180,-197,-212,-224,-235,-244,-250,-253,
    -255,-253,-250,-244,-235,-224,-212,-197,-180,-161,-141,-120,
     -97, -74, -49, -24  },	/* Sine */

   {   0,  -8, -16, -24, -32, -40, -48, -56, -64, -72, -80, -88,
     -96,-104,-112,-120,-128,-136,-144,-152,-160,-168,-176,-184,
    -192,-200,-208,-216,-224,-232,-240,-248, 255, 248, 240, 232,
     224, 216, 208, 200, 192, 184, 176, 168, 160, 152, 144, 136,
     128, 120, 112, 104,  96,  88,  80,  72,  64,  56,  48,  40,
      32,  24,  16,   8  },	/* Ramp down */

   { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
     255, 255, 255, 255, 255, 255, 255, 255,-255,-255,-255,-255,
    -255,-255,-255,-255,-255,-255,-255,-255,-255,-255,-255,-255,
    -255,-255,-255,-255,-255,-255,-255,-255,-255,-255,-255,-255,
    -255,-255,-255,-255  },	/* Square */

   {   0,   8,  16,  24,  32,  40,  48,  56,  64,  72,  80,  88,
      96, 104, 112, 120, 128, 136, 144, 152, 160, 168, 176, 184,
     192, 200, 208, 216, 224, 232, 240, 248,-255,-248,-240,-232,
    -224,-216,-208,-200,-192,-184,-176,-168,-160,-152,-144,-136,
    -128,-120,-112,-104, -96, -88, -80, -72, -64, -56, -48, -40,
     -32, -24, -16,  -8  }	/* Ramp up */
};

static struct retrig_t rval[] = {
    {   0,  1,  1 }, {  -1,  1,  1 }, {  -2,  1,  1 }, {  -4,  1,  1 },
    {  -8,  1,  1 }, { -16,  1,  1 }, {   0,  2,  3 }, {   0,  1,  2 },
    {   0,  1,  1 }, {   1,  1,  1 }, {   2,  1,  1 }, {   4,  1,  1 },
    {   8,  1,  1 }, {  16,  1,  1 }, {   0,  3,  2 }, {   0,  2,  1 },
    {   0,  0,  1 }	/* Note cut */
};
    

void sig_handler(int sig)
{
    switch(sig) {
    case SIGNAL_ABORT:
	SET_FLAG(global_flags,MODULE_ABORT);
	break;
    case SIGNAL_RESTART:
	SET_FLAG(global_flags,MODULE_ABORT|MODULE_RESTART);
	break;
    }
    fflush(stdout);
}


int get_envelope(uint16 *env,uint8 p,uint16 x)
{
    uint16 x1,x2;
    short y1,y2;

    p--; p<<=1;
    if((x1=env[p])<=x) return env[p+1];
    do { p-=2; x1=env[p]; } while((x1>x)&&p);
    y1=env[p+1];	
    x2=env[p+2];
    y2=env[p+3];

    return ((y2-y1)*(x-x1)/(x2-x1))+y1; 
}


void process_fx(uint8 note,uint8 fxt,uint8 fxp,struct xmp_channel* xc)
{
    /* Standard effects */
    switch(fxt) {
    case FX_0:
	break;
    case FX_1:				/* Portamento up */
	SET(PITCHBEND);
	if(fxp) xc->f_val=-fxp;
	else if(xc->f_val>0) xc->f_val*=-1;
	break;
    case FX_2:				/* Portamento down */
	SET(PITCHBEND);
	if(fxp) xc->f_val=fxp;
	else if(xc->f_val<0) xc->f_val*=-1;
	break;
    case FX_5:				/* Tone porta + vol slide */
	SET(VOL_SLIDE);
	if(fxp) xc->v_val=MSN(fxp)-LSN(fxp);
	fxp=0;
    case FX_3:				/* Tone portamento */
	if(TEST(NEW_NOTE)) {
	    xc->s_end=note_to_period(note+
		XXI[XXIM.ins[note]].xpo,
		XXI[XXIM.ins[note]].fin,
		xxh->flg&XXM_FLG_LINEAR);
	    RESET(NEW_NOTE);
	}
	xc->s_sgn=xc->period<xc->s_end?1:-1;
	if(fxp) xc->s_val=fxp;
	if(xc->s_end!=-1) SET(TONEPORTA);
	break;
    case FX_6:				/* Vibrato + vol slide */
	SET(VOL_SLIDE);
	if(fxp) xc->v_val=MSN(fxp)-LSN(fxp);
	fxp=0;
    case FX_4:				/* Vibrato */
	SET(VIBRATO);
	if(LSN(fxp)) xc->y_depth=LSN(fxp);
	if(MSN(fxp)) xc->y_rate=MSN(fxp);
	break;
    case FX_7:				/* Tremolo */
	SET(TREMOLO);
	if(MSN(fxp)) xc->t_rate=MSN(fxp);
	if(LSN(fxp)) xc->t_depth=LSN(fxp);
	break;
    case FX_8:				/* Set pan */
	SET(NEW_PAN);
	xc->pan=fxp;
	break;
    case FX_9:				/* Set sample offset */
	if(!fxp) offset=xc->offset;
	else xc->offset=offset=fxp*256;
	break;
    case FX_A:				/* Volume slide */
	SET(VOL_SLIDE);
	if(fxp) xc->v_val=MSN(fxp)-LSN(fxp);
	break;
    case FX_B:				/* Order jump */
	jump_pattern=fxp;
	break;
    case FX_C:				/* Volume set */
	SET(NEW_VOL);
	xc->volume=fxp;
	break;
    case FX_D:				/* Pattern break */
	SET_FLAG(global_flags,PATTERN_BREAK);
	jump_line=10*MSN(fxp)+LSN(fxp);
	break;
    case FX_E:				/* Extended effect */
	fxt=fxp>>4;
	fxp&=0x0f;
	switch(fxt) {
	case 0x1:			/* Fine portamento up */
	    SET(FINE_BEND);
	    if(fxp) xc->f_fval=-fxp*4;
	    else if(xc->f_val>0) xc->f_val*=-1;
	    break;
	case 0x2:			/* Fine portamento down */
	    SET(FINE_BEND);
	    if(fxp) xc->f_fval=fxp*4;
	    else if(xc->f_val<0) xc->f_val*=-1;
	    break;
	case 0x3:			/* Glissando toggle */
	    xc->gliss=fxp;
	    break;
	case 0x4:			/* Set vibrato waveform */
	    xc->y_type=fxp&3;
	    break;
	case 0x5:			/* Set finetune */
	    SET(FINETUNE);
	    xc->finetune=fxp;
	    break;
	case 0x6:			 /* Loop pattern */
	    if(fxp) {
		if(!TEST_FLAG(global_flags,PATTERN_LOOP))
		    if(loop_stack[loop_cnt]) {
			    if(--loop_stack[loop_cnt])
			    SET_FLAG(global_flags,PATTERN_LOOP);
		    } else {
			    loop_stack[loop_cnt]=fxp;
			    SET_FLAG(global_flags,PATTERN_LOOP);
		    }
		loop_cnt++;
	    } else
		if(!(TEST_FLAG(global_flags,PATTERN_LOOP)&&loop_cnt)){ 
		    RESET_FLAG(global_flags,PATTERN_LOOP);  
		    loop_ptr=row_ptr;
		    loop_row=row_cnt;
		    loop_cnt=0;
		} 
	    break;
	case 0x7:			/* Set tremolo waveform */
	    xc->t_type=fxp&3;
	    break;
	case 0x9:			/* Retrig note */
	    xc->retrig=xc->rcount=fxp?fxp+1:xc->rval;
	    xc->rval=xc->retrig;
	    xc->rtype=0;
	    break;
	case 0xa:			/* Fine volume slide up */
	    SET(FINE_VOLS);
	    if(fxp) xc->v_fval=fxp;
	    break;
	case 0xb:			/* Fine volume slide down */
	    SET(FINE_VOLS);
	    if(fxp) xc->v_fval=-fxp;
	    break;
	case 0xc:			/* Cut note */
	    xc->retrig=xc->rcount=fxp+1;
	    xc->rtype=0x10;
	    break;
	case 0xd:			/* Note delay */
	    xc->delay=fxp+1;
	    break;
	case 0xe:			/* Pattern delay */
	    delay_pattern=fxp;
	    break;
	}
	break;
    case FX_F:				/* Set tempo */
	if(fxp)
	    if(fxp<=32) tempo=fxp;
	    else tick_duration=replay_rate/fxp;
	break; 
    case FX_G:				/* Set global volume */
	global_vol=fxp;
	if(global_vol>0x40) global_vol=0x40;
	break;
    case FX_H:				/* Global volume slide */
	SET_FLAG(global_flags,GLOBAL_VSLIDE);
	if(fxp)
	    global_vslide=MSN(fxp)-LSN(fxp);
	break;
    case FX_K:				/* Key off or 64K offset */
	NOT_IMPLEMENTED;
	break;
    case FX_L:				/* Set envelope position */
	NOT_IMPLEMENTED;
	break;
    case FX_P:				/* Pan slide */
	SET(PAN_SLIDE);
	if(fxp) xc->p_val=MSN(fxp)-LSN(fxp);
	break;
    case FX_R:				/* Multi retrig */
	xc->retrig=xc->rcount=LSN(fxp)?LSN(fxp)+1:xc->rval;
	xc->rval=xc->retrig;
	xc->rtype=MSN(fxp);
	break;
    case FX_T:				/* Tremor */
	NOT_IMPLEMENTED;
	break;
    case FX_X:				/* Extra fine portamento */
	SET(FINE_BEND);
	switch(MSN(fxp)) {
	case 1:
	    xc->f_fval=-LSN(fxp);
	    break;
	case 2:
	    xc->f_fval=LSN(fxp);
	    break;
	}
	break;
    case FX_Z:				/* Arpeggio */
	if(fxp==0xff) fxp=xc->arp;
	xc->arp=fxp;
	xc->a_val[1]=100*MSN(fxp);
	xc->a_val[2]=100*LSN(fxp);
	break;
    }

}


void fetch_and_play(int t,struct xxm_event *e,struct xmp_channel *xc,
int ch,struct dev_info *d)
{
    uint8 note=0;
    uint32 finalvol;
    uint16 vol_envelope;
    int finalpan,pan_envelope,vol2;

    if(!t) {
	/* Reset & init values */
	xc->flags=0;
	if(!ch)
	    RESET_FLAG(global_flags,GLOBAL_VSLIDE);
	xc->delay=xc->retrig=0;
	xc->a_idx=xc->a_val[1]=xc->a_val[2]=0;

	note=e->not;

	if(e->ins>xxh->ins) return;
	if(e->ins) {
	    SET(NEW_INS);
	    xc->oldins=xc->ins;
	    xc->ins=e->ins-1;
	}
	if(note) {
	    if(note==0x61) xc->release=1;
	    if((note<0x61)&&XXIH.nsm) {
		SET(NEW_NOTE); note--;
		xc->s_end=note_to_period(note+
	 	    XXI[XXIM.ins[note]].xpo,
		    XXI[XXIM.ins[note]].fin,
		    xxh->flg&XXM_FLG_LINEAR);
	    }  
	    else RESET(NEW_INS);
	}
	if(e->vol) {			/* Volume set */
	    SET(NEW_VOL);
	    xc->volume=e->vol-1;
	}

	process_fx(note,e->fxt,e->fxp,xc);
	process_fx(note,e->f2t,e->f2p,xc);

	/* Note */
	if(TEST(NEW_NOTE)) {
	    xc->key=note;
	    xc->note=note+XXI[XXIM.ins[note]].xpo;
	    if (!TEST(FINETUNE)) xc->finetune=
		XXI[XXIM.ins[note]].fin;
	    xc->period=note_to_period(xc->note,xc->finetune,
		xxh->flg&XXM_FLG_LINEAR);
	}

	/* Instrument */
	if(TEST(NEW_INS)&&!TEST(NEW_VOL))
	    xc->volume=XXI[XXIM.ins[xc->key]].vol;
	if(TEST(NEW_NOTE|NEW_INS)) {
	    xc->p_idx=xc->v_idx=xc->insvib_idx=xc->release=0;
	    xc->fadeout=0x1fff;
	    xc->insvib_idx=0;
	    xc->insvib_swp=XXI->vsw;
	}
	if(TEST(TONEPORTA))
	    RESET(NEW_INS);

	/* Fadeout */
	if(xc->release)
	    xc->fadeout=(XXIH.aei.flg&XXM_ENV_ON)?
	    ((xc->fadeout>XXIH.rls)?
	    xc->fadeout-XXIH.rls:0):0;
       
	/* Process "fine" effects */
	if(TEST(FINE_VOLS)) xc->volume+=xc->v_fval;
	if(TEST(FINE_BEND)) xc->period=(4*xc->period+xc->f_fval)/4;

	if((TEST(NEW_NOTE)||(TEST(NEW_INS)&&(xc->ins!=xc->oldins)))) {
	    xc->y_idx=xc->t_idx=0;
            seq_setpatch(d,ch,XXI[XXIM.ins[xc->key]].sid);
	    seq_setnote(d,ch,xc->note);
	    if(!TEST(NEW_PAN))
		xc->pan=XXI[XXIM.ins[xc->key]].pan;
	}

#ifdef USE_PANEL
	if(TEST(NEW_NOTE|NEW_INS|NEW_VOL)) {
	    seq_echoback((xc->volume<<16)|(ch<<8)|ECHO_VOL);
	    seq_echoback((xc->ins<<16)|(ch<<8)|ECHO_INS);
	}
#endif

	if(offset) {
	    device_voicepos(offset,ch,d);
	    offset=0;
	}
    }

    vol_envelope=XXIH.aei.flg&XXM_ENV_ON?
	get_envelope(XXAE,XXIH.aei.npt,xc->v_idx):64;
    pan_envelope=XXIH.pei.flg&XXM_ENV_ON?
	get_envelope(XXPE,XXIH.pei.npt,xc->p_idx):32;

    /* Update envelopes */
    DO_ENVELOPE(XXIH.aei.flg,xc->v_idx,XXAE,
	XXIH.aei.sus,XXIH.aei.lps,XXIH.aei.lpe);
    DO_ENVELOPE(XXIH.pei.flg,xc->p_idx,XXPE,
	XXIH.pei.sus,XXIH.pei.lps,XXIH.pei.lpe);

#ifndef WARP
    vol2=xc->volume;
    if(TEST(TREMOLO))
	 vol2+=waveform[xc->t_type][xc->t_idx]*xc->t_depth/512;
    if(vol2>vol_base) vol2=vol_base;
    if(vol2<0) vol2=0;
	 
    finalvol=(uint32)(xc->fadeout*vol_envelope*global_vol*
	((int)vol2*0x40/vol_base))>>20;
    if(vol_xlat) finalvol=(vol_xlat[finalvol>>5]*100)>>6;
    else finalvol=(finalvol*100)>>11;	/* FIXME */
#endif

    xc->pitchbend=period_to_bend(xc->period+(TEST(VIBRATO)?
	waveform[xc->y_type][xc->y_idx]*xc->y_depth*64/8192:0)+
	waveform[XXI->vwf][xc->insvib_idx]*XXI->vde/(1024*(1+xc->insvib_swp)),
	xc->note,xxh->flg&XXM_FLG_MODRNG,xc->gliss,xxh->flg&XXM_FLG_LINEAR);

    finalpan=xc->pan+(pan_envelope-32)*(128-abs(xc->pan-128))/32;
    finalpan=xxc[ch].pan+(finalpan-128)*(128-abs(xxc[ch].pan-128))/128;

    /* Do delay */
    if(xc->delay) {
       if (--xc->delay) finalvol=0;
       else seq_setnote(d,ch,xc->note);
    }

    /* Do cut/retrig */
    if(xc->retrig) {
	if(!--xc->rcount) {
	    seq_setnote(d,ch,xc->note);
	    xc->volume+=rval[xc->rtype].s;
	    xc->volume*=rval[xc->rtype].m;
	    xc->volume/=rval[xc->rtype].d;
	    xc->rcount=xc->retrig;
	}
    }

    if(t) {	/* I hate this conditional */
	if(!ch&&TEST_FLAG(global_flags,GLOBAL_VSLIDE))
	    INC_IN_RANGE(global_vol,global_vslide,0,0xff);
	if(TEST(VOL_SLIDE)) INC_IN_RANGE(xc->volume,xc->v_val,0,vol_base);
	if(TEST(PAN_SLIDE)) INC_IN_RANGE(xc->pan,xc->p_val,0,0xff);
	if(TEST(PITCHBEND)) {
	    if(xxh->flg&XXM_FLG_LINEAR) {
		INC_IN_RANGE(xc->period,xc->f_val,MIN_PERIOD_L,MAX_PERIOD_L);
	    } else {
		INC_IN_RANGE(xc->period,xc->f_val,MIN_PERIOD_A,MAX_PERIOD_A);
	    }
	}
	/* Do tone portamento */
	if(TEST(TONEPORTA)) {
	    xc->period+=(xc->s_sgn*xc->s_val);
	    if((xc->s_sgn*xc->s_end)<=(xc->s_sgn*xc->period)) {
		xc->period=xc->s_end;
		RESET(TONEPORTA);
	    }
	} 
    }

    /* Update vibrato, tremolo and arpeggio indexes */
    xc->insvib_idx+=XXI->vra>>2; xc->insvib_idx%=64;
    if(xc->insvib_swp>1) xc->insvib_swp-=2; else xc->insvib_swp=0;
    xc->y_idx+=xc->y_rate; xc->y_idx%=64;
    xc->t_idx+=xc->t_rate; xc->t_idx%=64;
    xc->a_idx++; xc->a_idx%=3;

    seq_setbend(d,ch,(xc->pitchbend+xc->a_val[xc->a_idx]));
    seq_setpan(d,ch,(finalpan-0x80)*opt.mix/100*opt.reverse);
    if(!xc->mute) seq_setvol(d,ch,finalvol);
}

 
int play_module(struct dev_info *d)
{
    int r,o,t,c;		/* rows, order, tick, channel */
    uint8 *p=0;			/* pattern */
    double next_time;

    tick_duration=replay_rate/xxh->bpm;
    if(!xxh->tpo) xxh->tpo=6;
    tempo=xxh->tpo;
    global_flags=offset=0;

    if(opt.verbose) {
	report("Channels       : %d [ ",xxh->chn);
	for(c=0;c<xxh->chn;c++) report("%x ",xxc[c].pan>>4);
	report("]\n");
	report("Initial tempo  : %d frames per row at %d BPM\n",
	    xxh->tpo,xxh->bpm);
    }
    for(c=0;c<L_CHANNELS;c++) {
	memset((uint8*)&xc[c]+2*sizeof(int),0,sizeof(xc[c])-2*sizeof(int));
	xc[c].pch=c<p_channels?c:0xff;
    }

    loop_stack=calloc(sizeof(uint8),ROW_MAX);
    seq_resetvoices(d);
    device_numvoices(xxh->chn,d);
    seq_starttimer();
    seq_sync(0);
    next_time=tick_duration;
    seq_sync(next_time);
    seqbuf_dump();
    jump_pattern=-1;
    global_vol=0x40;
    global_vslide=0;
    signal(SIGNAL_ABORT,sig_handler);
    signal(SIGNAL_RESTART,sig_handler);

    for(o=opt.start;o<xxh->len;o++) {
	seq_echoback((xxo[o]<<16)|(o<<8)|ECHO_ORD);

	/* S3M uses 0xff as an end mark */
	if(xxo[o]==0xff) {
	    if(opt.ignoreff) {
		for(c=0;c<xxh->chn;c++)
		    seq_setvol(d,c,0);
		tick_duration=replay_rate/xxh->bpm;
		tempo=xxh->tpo;
		global_flags=0;
		xc[c].flags=0;
		continue;
	    }
	    if(opt.loop) o=xxh->rst;
	    else break;
	}
	r=(uint8)(xxp[xxo[o]]->rows-1)+1;
	if(jump_line>=r) jump_line=0;
	loop_row=-(jump_line+1);
	row_cnt=jump_line;
	loop_cnt=jump_line=0;

	for(;row_cnt<r;row_cnt++,row_ptr=p) {
	    seq_echoback((xxp[xxo[o]]->rows<<16)|(row_cnt<<8)|ECHO_ROW);
	    for(t=0;t<(tempo*(1+delay_pattern));t++) {
		for(c=0;c<xxh->chn;c++)
		    fetch_and_play(t,&EVENT(xxo[o],c,row_cnt),&xc[c],c,d);
		next_time+=tick_duration;
#ifndef WARP
		seq_sync(next_time);
#endif
		seqbuf_dump();
	    }
	    delay_pattern=0;

	    if(TEST_FLAG(global_flags,MODULE_ABORT|PATTERN_BREAK)) break;

	    if(TEST_FLAG(global_flags,PATTERN_LOOP)) {
		loop_cnt=0;
		p=loop_ptr;
		if(loop_row<0) {
		    row_cnt=-(loop_row+2);
		    RESET_FLAG(global_flags,PATTERN_LOOP);
		} else row_cnt=loop_row-1;
	    }
	}
	if(TEST_FLAG(global_flags,MODULE_ABORT)) break;
	if(opt.loop&&(o==(xxh->len-1)))
	    o=xxh->rst-1;
	if(jump_pattern!=-1) {
	    if(!opt.loop&&(jump_pattern<=o)) break;
	    o=jump_pattern-1;
	    jump_pattern=-1;
	}
	RESET_FLAG(global_flags,PATTERN_BREAK);
    }
    seq_echoback(ECHO_END);
    signal(SIGNAL_ABORT,SIG_DFL);
    signal(SIGNAL_RESTART,SIG_DFL);
    seq_resetvoices(d);
    while(seq_getmsg()!=ECHO_END) seqbuf_dump();
    if(opt.verbose) report("\n");
    seq_stoptimer();
    return TEST_FLAG(global_flags,MODULE_RESTART);
}

