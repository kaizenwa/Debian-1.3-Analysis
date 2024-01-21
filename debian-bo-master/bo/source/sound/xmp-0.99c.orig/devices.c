/* devices.c - output device wrapper
 * Copyright (C) 1996 C.Matsuoka and H.Carraro Jr
 * Loop click removal code Copyright (C) 1995 by Andrew Robinson
 *
 * $Id: devices.c,v 1.6 1997/03/20 02:36:50 claudio Exp $
 *
 * $Log: devices.c,v $
 * Revision 1.6  1997/03/20 02:36:50  claudio
 * Added more code to prevent sample loop clicking.
 *
 * Revision 1.5  1997/03/18 16:03:36  claudio
 * Added test in sample loop to prevent clicking.
 *
 * Revision 1.4  1997/03/13 13:25:53  claudio
 * device_writepatch() added and other changes for xmp 0.99.
 *
 * Revision 1.3  1997/01/19 01:42:14  claudio
 * Removed pan reverse/mixing stuff.
 *
 * Revision 1.2  1997/01/02 00:12:34  claudio
 * Header file ultrasound.h included if USE_AWE defined.
 *
 * Revision 1.1  1996/12/07 17:47:56  claudio
 * Initial revision
 *
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>
#ifdef _AIX
#include <strings.h>
#endif
#include "xmp.h"

#ifdef USE_OSS

#define USE_SEQ_MACROS
#define SEQ_OPEN_MODE O_RDWR

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <unistd.h>
#ifdef _AIX
#include <sys/select.h>
#endif

static struct synth_info si;
int seqfd;
SEQ_DEFINEBUF(2048);

#ifdef GUS_DEVICE
#include <sys/ultrasound.h>
#endif

#ifdef AWE_DEVICE
#include <sys/ultrasound.h>
#if defined(__linux__)&&defined(__i386__)
#include <linux/awe_voice.h>
#else
#include <sys/awe_voice.h>
#endif
#endif

#endif /* USE_OSS */

#include "devices.h"

#define SELECT_DEVICE(x) { if(!(id&&strcmp(id,x))&& \
    ((d=select_device(x))!=NULL)) { d->dev=i; break; } }

static struct dev_info *device;
static unsigned long __echo_msg;
static uint8 **xs;

extern struct options opt;
extern struct xmp_channel xc[L_CHANNELS];


int register_device(int a,char *i,char *n,int p)
{
    struct dev_info *d;

    d=malloc(sizeof(struct dev_info));
    d->id=malloc(strlen(i)+1);
    d->name=malloc(strlen(n)+1);
    d->num=a;
    d->pch=p;
    strcpy(d->id,i);
    strcpy(d->name,n);

    if(!device) device=d;
    else {
	struct dev_info *c;
	for(c=device;c->next;c=c->next);
	c->next=d;    
    }
    d->next=NULL;
    return 0;
}


struct dev_info *select_device(char *i)
{
    struct dev_info *d;

    for(d=device;d;d=d->next)
	if(!strcmp(d->id,i)) return d;

    return NULL;
}


void list_devices(char *s)
{
    struct dev_info *d;

    for(d=device;d&&(d->num!=NONE);d=d->next)
	report(s,d->id,d->name);
}


struct dev_info *get_device(char *id)
{
    struct dev_info *d=NULL;
#ifdef USE_OSS
    int i;

    if((seqfd=open("/dev/sequencer",SEQ_OPEN_MODE))==-1)
	/* To do: Use softmix instead */
	die("Cannot open /dev/sequencer.");

    if(ioctl(seqfd,SNDCTL_SEQ_NRSYNTHS,&i)==-1)
	die("Can't determine number of synths.");

    for(;i--;) {
	si.device=i;
	if(ioctl(seqfd,SNDCTL_SYNTH_INFO,&si)==-1)
	    die("Can't determine synth info.");
	if(si.synth_type==SYNTH_TYPE_SAMPLE) {
#ifdef GUS_DEVICE
	    if(si.synth_subtype==SAMPLE_TYPE_GUS) SELECT_DEVICE("gus");
#endif /* GUS_DEVICE */
#ifdef AWE_DEVICE
	    if(si.synth_subtype==SAMPLE_TYPE_AWE32) SELECT_DEVICE("awe");
#endif /* AWE_DEVICE */
	}
    }

    if(d==NULL) {
	if(id) {
	    if((d=select_device(id))!=NULL)
		die("Can't find a %s.",d->name);
	    die("Can't find the specified output device.");
	}
	die ("Can't find an output device.");
    }

    SEQ_VOLUME_MODE(d->dev,VOL_METHOD_LINEAR);
    seqbuf_dump();
    ioctl(seqfd,SNDCTL_SEQ_SYNC,0);
#endif /* USE_OSS */

    if(d==NULL) d=select_device("softmix");

    return d;
}


void device_numvoices(int n,struct dev_info *d)
{
#ifdef GUS_DEVICE
    if(d->num==GUS) {
	if(n<14) n=14;
	GUS_NUMVOICES(d->dev,n);
    }
#endif
}


void device_voicepos(int i,int ch,struct dev_info *d)
{
    if((ch=xc[ch].pch)==0xff) return;
#if(defined GUS_DEVICE)||(defined AWE_DEVICE)
    if((d->num==GUS)||(d->num==AWE))
	GUS_VOICE_POS(d->dev,ch,i);
#endif
}


void device_writepatch(FILE *f,struct dev_info *d,int id,int basefreq,
int flags,struct xxm_sample *xxs)
{
    int i;
    uint8 *buf;
#ifdef USE_OSS
    struct patch_info *patch;
#endif

    if(!xxs->len) return;
    
#ifdef USE_OSS
    if((d->num==GUS)||(d->num==AWE)) {
	if(opt.smp8bit&&TEST_FLAG(xxs->flg,WAVE_16_BITS)) {
	    patch=calloc(1,xxs->len/2+sizeof(struct patch_info));
	    buf=calloc(1,xxs->len);
	    fread(buf,1,xxs->len,f);
	    if(TEST_FLAG(flags,XMP_SMP_DIFF))
		diff2abs(xxs->len,xxs->flg&WAVE_16_BITS,buf);
	    else if(TEST_FLAG(flags,XMP_SMP_8BDIFF))
		diff2abs(xxs->len,0,buf);
	    xxs->len=xxs->len/2-1;
	    xxs->lpe/=2;
	    xxs->lps/=2;
	    for(i=0;i<xxs->len;i++) buf[i]=((uint16*)buf)[i]>>8;
	    memcpy(patch->data,buf,xxs->len);
	    free(buf);
	    RESET_FLAG(xxs->flg,WAVE_16_BITS);
	} else {
	    patch=calloc(1,xxs->len+sizeof(struct patch_info));
	    fread(patch->data,1,xxs->len,f);
	    if(TEST_FLAG(flags,XMP_SMP_DIFF))
		diff2abs(xxs->len,xxs->flg&WAVE_16_BITS,patch->data);
	    else if(TEST_FLAG(flags,XMP_SMP_8BDIFF))
		diff2abs(xxs->len,0,patch->data);
	}
	patch->key=GUS_PATCH;
	patch->device_no=d->dev;
	patch->instr_no=id;
	patch->mode=xxs->flg;
	patch->mode|=TEST_FLAG(flags,XMP_SMP_UNS)?WAVE_UNSIGNED:0;
	patch->len=xxs->len+1;
	patch->loop_start=xxs->lps;
	patch->loop_end=xxs->lpe;
	if(xxs->lps>xxs->len) patch->loop_start=0;
	if(xxs->lpe>xxs->len) patch->loop_end=xxs->len;
	if(~patch->mode&WAVE_16_BITS) {
	    if((patch->mode&WAVE_LOOPING)&&!(patch->mode&WAVE_BIDIR_LOOP)) {
		patch->data[patch->loop_end]=patch->data[patch->loop_start];
		/*
		 * Using gmod's method to minimize clicking - the following
		 * eight lines reproduce the algorithm used in gmod 2.0.2
		 * and are Copyright (C) 1995 by Andrew Robinson 
		 */
		if(patch->mode&WAVE_UNSIGNED)
		    patch->data[patch->loop_end-1]=
			((uint8)patch->data[patch->loop_end-2]+
			(uint8)patch->data[patch->loop_end])/2;
		else
		    patch->data[patch->loop_end-1]=
			((int8)patch->data[patch->loop_end-2]+
			(int8)patch->data[patch->loop_end])/2;
	    } else
		patch->data[xxs->len]=patch->data[xxs->len-1];
	}
	patch->base_note=C4_FREQ;
	patch->base_freq=basefreq;
	patch->high_note=0x7fffffff;
	patch->low_note=0;
	patch->volume=120;
	patch->panning=16;
	patch->detuning=0;
    	SEQ_WRPATCH(patch,sizeof(*patch)+patch->len-1);
	seqbuf_dump();
	free(patch);
    } else
#endif
    {
	xs[id]=calloc(1,xxs->len);
	fread(xs[id],1,xxs->len,f);
    }
}


/* Some functions to wrap OSS (for softmix) */

void seq_echoback(int i)
{
#ifdef USE_OSS
    SEQ_ECHO_BACK(i);
#endif
}


void seq_setpatch(struct dev_info *d,int ch,int n)
{
    if((ch=xc[ch].pch)==0xff) return;
#ifdef USE_OSS
    SEQ_SET_PATCH(d->dev,ch,n);
#endif
}


void seq_setvol(struct dev_info *d,int ch,int vol)
{
    if((ch=xc[ch].pch)==0xff) return;
#ifdef USE_OSS
    SEQ_START_NOTE(d->dev,ch,255,vol);
#endif
}


void seq_setnote(struct dev_info *d,int ch,int note)
{
    if((ch=xc[ch].pch)==0xff) return;
#ifdef USE_OSS
    SEQ_START_NOTE(d->dev,ch,note,0);
#endif
}


void seq_setpan(struct dev_info *d,int ch,int pan)
{
    if((ch=xc[ch].pch)==0xff) return;
#ifdef USE_OSS
#ifdef GUS_DEVICE
    if(d->num==GUS)
	GUS_VOICEBALA(d->dev,ch,(pan+0x80)>>4)
    else
#endif
	SEQ_PANNING(d->dev,ch,pan);
#endif
}


void seq_setbend(struct dev_info *d,int ch,int bend)
{
    if((ch=xc[ch].pch)==0xff) return;
#ifdef USE_OSS
    SEQ_PITCHBEND(d->dev,ch,bend);
#endif
}


void seq_starttimer()
{
#ifdef USE_OSS
    SEQ_START_TIMER();
#endif
}


void seq_stoptimer()
{
#ifdef USE_OSS
    SEQ_STOP_TIMER();
#endif
}


/* From here to the end we have the old sequencer.c */

void seq_resetvoices(struct dev_info *d)
{
#ifdef USE_OSS
    int i;

    device_numvoices(32,device);
    for(i=0;i<32;i++) {
	SEQ_EXPRESSION(d->dev,i,255);
	SEQ_MAIN_VOLUME(d->dev,i,100);
	SEQ_CONTROL(d->dev,i,CTRL_PITCH_BENDER_RANGE,8191);
	SEQ_BENDER(d->dev,i,0);
	SEQ_PANNING(d->dev,i,0);
	SEQ_STOP_NOTE(d->dev,i,255,0);
    }
    seqbuf_dump();
#endif
}


int seq_getmsg()
{
    return __echo_msg; 
}


void seqbuf_dump()
{
#ifdef USE_OSS
    int i,j;
    fd_set rfds,wfds;
    int msg;

    FD_ZERO(&rfds);
    FD_ZERO(&wfds);

    do {
	FD_SET(seqfd,&rfds);
	FD_SET(seqfd,&wfds);
    
	select(seqfd+1,&rfds,&wfds,NULL,NULL);
    
	if(FD_ISSET(seqfd,&rfds)) {
	    if((read(seqfd,&__echo_msg,4)==4)&&
		((__echo_msg&0xff)==SEQ_ECHO)) {
		__echo_msg>>=8;
		msg=__echo_msg>>8;
		if(opt.verbose) switch(__echo_msg&0xff) {
		case ECHO_ORD:
		    report("\rOrder %02X - Pattern %02X - Row      ",
			msg&0xff,msg>>8);
		    break;
		case ECHO_ROW:
		    report("\b\b\b\b\b%02X/%02X",msg&0xff,msg>>8);
   		    break;
		}
	    } else __echo_msg=ECHO_NONE;
	}
    
	if(FD_ISSET(seqfd,&wfds)&&((j=_seqbufptr)!=0)) {
	    if((i=write(seqfd,_seqbuf,_seqbufptr))==-1)
		die("Can't write to sequencer.");
	    else if(i<j) {
		_seqbufptr-=i;
		memmove(_seqbuf,_seqbuf+i,_seqbufptr);
	    } else
		_seqbufptr=0;
	}
    } while(_seqbufptr);
#endif
}


void seq_clearmem(struct dev_info *d)
{
#ifdef USE_OSS
    int i=d->dev;

    if((d->num==GUS)||(d->num==AWE))
	ioctl(seqfd,SNDCTL_SEQ_RESETSAMPLES,&i);
#endif
}


int seq_getfreemem(struct dev_info *d)
{
#ifdef USE_OSS
    int i=d->dev;

    if((d->num==GUS)||(d->num==AWE)) {
	ioctl(seqfd,SNDCTL_SYNTH_MEMAVL,&i);
	return i;
    }
#endif
    return -1;
}


void seq_sync(double next_time)
{
#ifdef USE_OSS
    static double this_time=0;

    if(next_time==0) {
	this_time=0;
	return;
    }
    if(next_time>this_time) {
	SEQ_WAIT_TIME(next_time);
	this_time=next_time;
    }
#endif
}

