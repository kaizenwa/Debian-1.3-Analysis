/* xmp.c - Fastracker II extended module player
 * Copyright (C) 1996 C.Matsuoka and H.Carraro Jr
 *
 * $Id: xmp.c,v 1.6 1997/03/20 13:19:52 claudio Exp $
 *
 * $Log: xmp.c,v $
 * Revision 1.6  1997/03/20 13:19:52  claudio
 * dump_pattern() fixed to recognize patterns with 256 rows.
 *
 * Revision 1.5  1997/03/18 16:07:45  claudio
 * Added secondary effect to dump_pattern().
 *
 * Revision 1.4  1997/03/13 13:21:26  claudio
 * Lots of changes for xmp 0.99.
 *
 * Revision 1.3  1997/01/19 01:43:09  claudio
 * Added incremental verbosity and ignoreff options.
 *
 * Revision 1.2  1997/01/02 00:19:59  claudio
 * Added options -o and -n.
 *
 * Revision 1.1  1996/12/07 20:40:26  claudio
 * Initial revision
 *
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>

#include "xmp.h"
#include "xxm.h"
#include "devices.h"

#define TEMP "/tmp/uncompressed.xmp"

extern char *optarg;
extern int optind;
extern char *ver_id;

struct dev_info *d;
struct fmt_info *fmt,*fmt_head;
char module_name[MODULE_NAME_MAXSIZE];
struct options opt;
double replay_rate;
int c4_rate;
int vol_base;
int *vol_xlat;
int p_channels;
struct xmp_channel xc[L_CHANNELS];

struct xxm_header *xxh;
struct xxm_pattern **xxp;		/* Patterns */
struct xxm_track **xxt;			/* Tracks */
struct xxm_channel xxc[32];		/* Channel info */
uint8 xxo[256];				/* Orders */
struct xxm_instrument_header *xxih;	/* Instrument headers */
struct xxm_instrument_map *xxim;	/* Instrument map */
struct xxm_instrument **xxi;		/* Instruments */
struct xxm_sample *xxs;			/* Samples */
uint16 **xxae;				/* Amplitude envelope */
uint16 **xxpe;				/* Pan envelope */
uint16 **xxfe;				/* Pitch envelope */


void dump_pattern(int i)
{
    int r,c,x;
    struct xxm_event *xxe;
    static char *note[12] = {
	"C-","C#","D-","D#","E-","F-",
	"F#","G-","G#","A-","A#","B-"
    };

    x=(uint8)(xxp[i]->rows-1)+1;
    printf("PATTERN %d (%d rows)\n     ",i,x);
    for(c=0;c<xxh->chn;c++)
	printf("[   CH %2d: %03x   ] ",c,xxp[i]->info[c].index);
    printf("\n");
    for(r=0;r<x;r++) {
	printf("[%02x] ",r);
	for(c=0;c<xxh->chn;c++) {
	    xxe=&xxt[xxp[i]->info[c].index]->event[r];
	    if(xxe->not==0x61)
		printf("___ ");
	    else if(xxe->not)
		printf("%s%c ",note[(xxe->not-1)%12],'0'+(xxe->not-1)/12);
	    else printf("... ");
	    if(xxe->ins)
		printf("%02X ",xxe->ins);
	    else printf(".. ");
	    if(xxe->vol)
		printf("%02X ",xxe->vol);
	    else printf(".. ");
	    printf("%c%02X ",xxe->fxt<=9?'0'+xxe->fxt:
		'A'-10+xxe->fxt,xxe->fxp);
	    printf("%c%02X |",xxe->f2t<=9?'0'+xxe->f2t:
		'A'-10+xxe->f2t,xxe->f2p);
	}
	printf("\n");
    }
    printf("\n");
}


void usage()
{
    report(
	"Usage: xmp [-8ADhiLlmnorVv] [-p pan] [-s num] [-d device] [-M ch]\n"
	"	   [-S ch] filename\n"
    );
    report("Available output devices:");
    list_devices(" %s");
    report("\n"
	"    -8      Convert samples to 8 bit\n"
	"    -A      Use Amiga frequency table\n"
#if 0
	"    -B bits Software mixer resolution\n"
#endif
	"    -D      Dump pattern data\n"
#if 0
	"    -F Hz   Software mixer output frequency\n"
#endif
	"    -h      Print this help message\n"
	"    -i      Ignore 0xff patterns in S3M files\n"
	"    -L      Use linear frequency table\n"
	"    -l      Enable module looping\n"
	"    -M ch   Mute channel\n"
	"    -m      Force mono mode\n"
	"    -n      Use NTSC timing in MOD files\n"
	"    -o      Force 3 octave range in MOD files\n"
	"    -p pan  Set panning amplitude to num%%\n"
	"    -r      Reverse left/right channels\n"
	"    -S ch   Set channel to solo mode\n"
	"    -s num  Start from the specified order\n"
	"    -V      Print version information\n"
	"    -v      Verbose mode (incremental)\n"
    );
}


void register_fmt(char *id,int (*loader)())
{
    struct fmt_info *f;

    f=malloc(sizeof(struct fmt_info));
    f->id=malloc(strlen(id)+1);
    strcpy(f->id,id);
    f->loader=loader;
    if(!fmt_head) fmt_head=f;
    else {
	struct fmt_info *i;
	for(i=fmt_head;i->next;i=i->next);
	i->next=f;
    }
    f->next=NULL;
}

void list_fmt(char *s)
{
    struct fmt_info *f;

    for(f=fmt_head;f;f=f->next)
	report(s,f->id);
}

int main(int argc,char **argv)
{
    FILE *f;
    int i,o,mem;
    char module[80],*dev_id=NULL;
    time_t t0,t1;
    int t;

    check_alignment();
#ifdef GUS_DEVICE
    register_device(GUS,"gus","Gravis Ultrasound",32);
#endif
#ifdef AWE_DEVICE
    register_device(AWE,"awe","Sound Blaster AWE-32",32);
#endif
#if 0
    register_device(SOFTMIX,"softmix","Software mixer",32);
#endif

#ifdef LOAD_XM
    register_fmt("Fast Tracker II extended modules (XM)",xm_load);
#endif
#ifdef LOAD_S3M
    register_fmt("Scream Tracker 3 modules (S3M)",s3m_load);
#endif
#ifdef LOAD_STM
    register_fmt("Scream Tracker 2 modules (STM)",stm_load);
#endif
#ifdef LOAD_PTM
    register_fmt("Polytracker modules (PTM)",ptm_load);
#endif
#ifdef LOAD_IT
    register_fmt("Impulse Tracker modules (IT)",it_load);
#endif
#ifdef LOAD_OKT
    register_fmt("Oktalyzer modules (OKT)",okt_load);
#endif
#ifdef LOAD_FAR
    register_fmt("Farandole Composer modules (FAR)",far_load);
#endif
#ifdef LOAD_MTM
    register_fmt("Multitracker modules (MTM)",mtm_load);
#endif
#ifdef LOAD_669
    register_fmt("Composer 669 modules (669)",ssn_load);
#endif
#ifdef LOAD_MOD
    register_fmt("Protracker modules and variants (MOD)",mod_load);
#endif

    /* Defaults */
    opt.reverse=1;
    opt.start=0;
    opt.mix=75;
    i=0;

    while((o=getopt(argc,argv,"8ADd:hiLlM:mnop:rVvS:s:"))!=-1) {
	switch(o) {
	case '8':
	    opt.smp8bit=1;
	    break;
	case 'A':
	    opt.period_mode=XMP_AMIGA_PERIOD_MODE;
	    break;
	case 'D':
	    opt.dump=1;
	    break;
	case 'd':
	    dev_id=malloc(strlen(optarg)); 
	    strcpy(dev_id,optarg);
	    break;
	case 'i':
	    opt.ignoreff=1;
	    break;
	case 'L':
	    opt.period_mode=XMP_LINEAR_PERIOD_MODE;
	    break;
	case 'l':
	    opt.loop=1;
	    break;
	case 'M':
	    if(atoi(optarg)<L_CHANNELS)
		xc[atoi(optarg)].mute=1;
	    break;
	case 'm':
	    opt.reverse=0;
	    break;
	case 'n':
	    opt.ntsc=1;
	    break;
	case 'o':
	    opt.modrange=1;
	    break;
	case 'p':
	    opt.mix=atoi(optarg);
	    if(opt.mix<0) opt.mix=0;
	    if(opt.mix>100) opt.mix=100;
	    break;
	case 'r':
	    opt.reverse=-1;
	    break;
	case 'S':
	    if(atoi(optarg)<L_CHANNELS) {
		for(;i<L_CHANNELS;i++) xc[i].mute=1;
		xc[atoi(optarg)].mute=0;
	    }
	    break;
	case 's':
	    opt.start=atoi(optarg);
	    break;
	case 'V':
	    report("%s\n",ver_id);
	    report("Available output devices:\n");
	    list_devices("\t%s\t\t%s\n");
	    report("Supported module formats:\n");
	    list_fmt("\t%s\n");
	    exit(-1);
	case 'v':
	    opt.verbose++;
	    break;
	case 'x':
	    opt.panel=1;
	    break;
	case 'h':
	    usage();
	default:
	    exit(-1);
	} 
    }
    
    if(!argv[optind]) {
	usage();
	exit(-1);
    }

    d=get_device(dev_id);
    seq_clearmem(d);

    if(opt.verbose>2) {
	report("Output device  : %s \n",d->name);
	if((mem=seq_getfreemem(d))!=-1)
	    report("Available mem  : %d Kb\n",mem/1024);
    }

    xxh=calloc(sizeof(struct xxm_header),1);
    p_channels=d->pch;

    while(optind<argc) {
	strcpy(module,argv[optind]);

	if(module[strlen(module)-1]=='z') {
	    char line[255];
	    sprintf(line,"gzip -dc %s > %s",module,TEMP);
	    if(opt.verbose>2)
		report("Decompression  : %s\n",line);
	    if(system(line)) {
		report("%s: Can't gunzip file.\n",module);
		optind++;
		continue;
	    }
	    strcpy(module,TEMP);
	}    
    
	if((f=fopen(module,"r"))==NULL) {
	    char line[255];
	    sprintf(line,"%s: %s",*argv,module);
	    perror(line);
	    optind++;
	    continue;
	}
    
	seq_clearmem(d);
	replay_rate=PAL_RATE;
	c4_rate=C4_PAL_RATE;
	vol_base=0x40;
	vol_xlat=NULL;

	for(fmt=fmt_head;fmt;fmt=fmt->next)
	    if((i=!fmt->loader(f,d))) break;
	fclose(f);

	unlink(TEMP);

	if(!i) {
	    report("%s: Unrecognized file format\n",module);
	    optind++;
	    continue;
	}

	if(opt.dump) {
	    for(i=0;i<xxh->pat;i++)
		dump_pattern(i);
	    exit(1);
	}

	if((opt.verbose>1)&&((mem=seq_getfreemem(d))!=-1))
	    report("Available mem  : %d Kb\n",mem/1024);

	str_adj(module_name);
	if(!*module_name) strcpy(module_name,"(untitled)");
 
	if(opt.period_mode==XMP_AMIGA_PERIOD_MODE)
	    xxh->flg&=~XXM_FLG_LINEAR;
	if(opt.period_mode==XMP_LINEAR_PERIOD_MODE)
	    xxh->flg|=XXM_FLG_LINEAR;

	if(opt.verbose>1) {
	    if(xxh->flg&XXM_FLG_MODRNG) report("Using Amiga period limits\n");
	    report("Module looping : %s\n",opt.loop?"yes":"no");
	    report("Period mode    : %s\n",
		xxh->flg&XXM_FLG_LINEAR?"linear":"Amiga");
	    report("Restart pos    : %d\n",xxh->rst);
	}
	if(opt.verbose>2) {
	    report("Base volume    : %d\n",vol_base);
	    report("C4 replay rate : %d\n",c4_rate);
	    report("Channel mixing : %d%%\n",opt.mix*opt.reverse);
	}
	time(&t0);
	if(!play_module(d)) optind++;
	time(&t1);
	t=difftime(t1,t0);
	if(opt.verbose)
	    report("Elapsed time: %d:%02d\n",t/60,t%60);

	for(i=0;i<xxh->trk;i++) free(xxt[i]);
	for(i=0;i<xxh->pat;i++) free(xxp[i]);
	for(i=0;i<xxh->ins;i++) {
	    free(xxfe[i]);
	    free(xxpe[i]);
	    free(xxae[i]);
	    free(xxi[i]);
	}
	free(xxt);
	free(xxp);
	free(xxi);
	free(xxs);
	free(xxim);
	free(xxih);
	free(xxfe);
	free(xxpe);
	free(xxae);
    }
    return 0;
}

