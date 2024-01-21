/*
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 */

#include "st.h"

/*
 * Sound Tools file format and effect tables.
 */

/* File format handlers. */

char *rawnames[] = {
	"raw",
	(char *) 0
};
extern rawstartread(), rawread();
extern rawstartwrite(), rawwrite(), rawstopwrite();

/*
char *raw2names[] = {
	"raw2",
	(char *) 0
};
extern raw2write();
*/

char *cdrnames[] = {
	"cdr",
	(char *) 0
};
extern cdrstartread(), cdrread(), cdrstopread();
extern cdrstartwrite(), cdrwrite(), cdrstopwrite();

char *vocnames[] = {
	"voc",
	(char *) 0
};
extern vocstartread(), vocread(), vocstopread();
extern vocstartwrite(), vocwrite(), vocstopwrite();

char *aunames[] = {
	"au",
#ifdef	NeXT
	"snd",
#endif
	(char *) 0
};
extern austartread();
extern austartwrite(), auread(), auwrite(), austopwrite();

char *wvenames[] = {
      "wve",
      (char *) 0
};
extern wvestartread();
extern wvestartwrite(), wveread(), wvewrite(), wvestopwrite();


char *aiffnames[] = {
	"aiff",
	"aif",
	(char *) 0
};
extern aiffstartread(), aiffread(), aiffstopread();
extern aiffstartwrite(), aiffwrite(), aiffstopwrite();

char *svxnames[] = {
	"8svx",
	(char *) 0
};
extern svxstartread(), svxread(), svxstopread();
extern svxstartwrite(), svxwrite(), svxstopwrite();

char *hcomnames[] = {
	"hcom",
	(char *) 0
};
extern hcomstartread(), hcomread(), hcomstopread();
extern hcomstartwrite(), hcomwrite(), hcomstopwrite();

char *sndtnames[] = {
	"sndt",
#ifdef	DOS
	"snd",
#endif
	(char *) 0
}; 
extern sndtstartread();
extern sndtstartwrite(), sndtwrite(), sndtstopwrite();

char *sndrnames[] = {
	"sndr",
	(char *) 0
};
extern sndrstartwrite();

char *ubnames[] = {
	"ub",
	"sou",
	"fssd",
#ifdef	MAC
	"snd",
#endif
	(char *) 0
};
extern ubstartread();
extern ubstartwrite();

char *sbnames[] = {
	"sb",
	(char *) 0
};
extern sbstartread();
extern sbstartwrite();

char *uwnames[] = {
	"uw",
	(char *) 0
};
extern uwstartread();
extern uwstartwrite();

char *swnames[] = {
	"sw",
	(char *) 0
};
extern swstartread();
extern swstartwrite();

char *ulnames[] = {
	"ul",
	(char *) 0
};
extern ulstartread();
extern ulstartwrite();


char *alnames[] = {
	"al",
	(char *) 0
};
extern alstartread();
extern alstartwrite();


char *sfnames[] = {
	"sf",
	(char *) 0
};
extern sfstartread();
extern sfstartwrite();

char *wavnames[] = {
	"wav",
	(char *) 0
};
extern wavstartread(), wavread();
extern wavstartwrite(), wavwrite(), wavstopwrite();

#if	defined(BLASTER) || defined(SBLAST) || defined(LINUXSOUND)
char *sbdspnames[] = {
	"sbdsp",
	(char *) 0
};
extern sbdspstartread(), sbdspread(), sbdspstopread();
extern sbdspstartwrite(), sbdspwrite(), sbdspstopwrite();
#endif

char *smpnames[] = {
	"smp",
	(char *) 0,
};

extern smpstartread(), smpread(), smpwrite();
extern smpstartwrite(), smpstopwrite();

char *maudnames[] = {
        "maud",
        (char *) 0,
};
extern maudstartread(), maudstopread(), maudread(), maudwrite();
extern maudstartwrite(), maudstopwrite();

char *autonames[] = {
	"auto",
	(char *) 0,
};

extern autostartread();
extern autostartwrite();

char *datnames[] = {
	"dat",
	(char *) 0
};
extern datstartread(), datread();
extern datstartwrite(), datwrite(), datstopwrite();

extern nothing();

EXPORT format_t formats[] = {
	{autonames, FILE_STEREO,
		autostartread, nothing, nothing,	/* Guess from header */
		autostartwrite, nothing, nothing},	/* patched run time */
	{smpnames, FILE_STEREO | FILE_LOOPS,
		smpstartread, smpread, nothing,	/* SampleVision sound */
		smpstartwrite, smpwrite, smpstopwrite},	/* Turtle Beach */
	{rawnames, FILE_STEREO,
		rawstartread, rawread, nothing, 	/* Raw format */
		rawstartwrite, rawwrite, nothing},
	/* Raw format that does mono->stereo automatically */
/*
	{raw2names, FILE_STEREO,
		rawstartread, rawread, nothing, 	
		rawstartwrite, raw2write, nothing},
*/
	{cdrnames, FILE_STEREO,
		cdrstartread, cdrread, cdrstopread,  /* CD-R format */
		cdrstartwrite, cdrwrite, cdrstopwrite},
	{vocnames, FILE_STEREO,
		vocstartread, vocread, vocstopread,  /* Sound Blaster .VOC */
		vocstartwrite, vocwrite, vocstopwrite},
	{aunames, FILE_STEREO,
		austartread, auread, nothing, 	/* SPARC .AU w/header */
		austartwrite, auwrite, austopwrite},	
	{wvenames, 0,       			/* Psion .wve */
		wvestartread, wveread, nothing,
		wvestartwrite, wvewrite, wvestopwrite},
	{ubnames, FILE_STEREO,
		ubstartread, rawread, nothing, 	/* unsigned byte raw */
		ubstartwrite, rawwrite, nothing},	/* Relies on raw */
	{sbnames, FILE_STEREO,
		sbstartread, rawread, nothing, 	/* signed byte raw */
		sbstartwrite, rawwrite, nothing},	
	{uwnames, FILE_STEREO,
		uwstartread, rawread, nothing, 	/* unsigned word raw */
		uwstartwrite, rawwrite, nothing},	
	{swnames, FILE_STEREO,
		swstartread, rawread, nothing, 	/* signed word raw */
		swstartwrite, rawwrite, nothing},
	{ulnames, FILE_STEREO,
		ulstartread, rawread, nothing, 	/* u-law byte raw */
		ulstartwrite, rawwrite, nothing},	
	{alnames, FILE_STEREO,
		alstartread, rawread, nothing, 	/* a-law byte raw */
		alstartwrite, rawwrite, nothing},	
	{aiffnames, FILE_STEREO,
		aiffstartread, aiffread, aiffstopread,    /* SGI/Apple AIFF */
		aiffstartwrite, aiffwrite, aiffstopwrite},
	{svxnames, FILE_STEREO,
		svxstartread, svxread, svxstopread,      /* Amiga 8SVX */
		svxstartwrite, svxwrite, svxstopwrite},
        {maudnames, FILE_STEREO,     			/* Amiga MAUD */
		maudstartread, maudread, maudstopread,
		maudstartwrite, maudwrite, maudstopwrite},
	{hcomnames, 0,
		hcomstartread, hcomread, hcomstopread, /* Mac FSSD/HCOM */
		hcomstartwrite, hcomwrite, hcomstopwrite},
	{sfnames, FILE_STEREO,
		sfstartread, rawread, nothing, 	/* IRCAM Sound File */
		sfstartwrite, rawwrite, nothing},	/* Relies on raw */
	{sndtnames, FILE_STEREO,
		sndtstartread, rawread, nothing,    /* Sndtool Sound File */
		sndtstartwrite, sndtwrite, sndtstopwrite},
	{sndrnames, FILE_STEREO,
		sndtstartread, rawread, nothing,    /* Sounder Sound File */
		sndrstartwrite, rawwrite, nothing},
	{wavnames, FILE_STEREO,
		wavstartread, wavread, nothing, 	/* Windows 3.0 .wav */
		wavstartwrite, wavwrite, wavstopwrite},	
#if	defined(BLASTER) || defined(SBLAST) || defined(LINUXSOUND)
	/* 386 Unix sound blaster players.  No more of these, please! */
	{sbdspnames, FILE_STEREO,
		sbdspstartread, sbdspread, sbdspstopread, 	/* /dev/sbdsp */
		sbdspstartwrite, sbdspwrite, sbdspstopwrite},	
#endif
	{datnames, 0,
		datstartread, datread, nothing, 	/* Text data samples */
		datstartwrite, datwrite, nothing},
	0
};

/* Effects handlers. */

extern null_drain();		/* dummy drain routine */

extern copy_getopts(), copy_start(), copy_flow(), copy_stop();
extern avg_getopts(), avg_start(), avg_flow(), avg_stop();
extern pred_getopts(), pred_start(), pred_flow(), pred_stop();
extern stat_getopts(), stat_start(), stat_flow(), stat_stop();
extern vibro_getopts(), vibro_start(), vibro_flow(), vibro_stop();
extern band_getopts(), band_start(), band_flow(), band_stop();
extern lowp_getopts(), lowp_start(), lowp_flow(), lowp_stop();
extern highp_getopts(), highp_start(), highp_flow(), highp_stop();
#ifdef	USE_DYN
extern dyn_getopts(), dyn_start(), dyn_flow(), dyn_stop();
#endif
extern echo_getopts(), echo_start(), echo_flow(), echo_drain(), echo_stop();
extern rate_getopts(), rate_start(), rate_flow(), rate_stop();
/*
extern down_getopts(), down_start(), down_flow(), down_stop();
*/
extern reverse_getopts(), reverse_start(), 
       reverse_flow(), reverse_drain(), reverse_stop();
extern map_getopts(), map_start(), map_flow();
extern cut_getopts(), cut_start(), cut_flow(), cut_stop();
extern split_getopts(), split_start(), split_flow(), split_stop();
extern pick_getopts(), pick_start(), pick_flow(), pick_stop();
extern resample_getopts(), resample_start(), resample_flow(), 
       resample_drain(), resample_stop();
extern mask_getopts(), mask_flow();

/*
 * EFF_CHAN means that the number of channels can change.
 * EFF_RATE means that the sample rate can change.
 * The first effect which can handle a data rate change, stereo->mono, etc.
 * is the default handler for that problem.
 * 
 * EFF_MCHAN just means that the effect is coded for multiple channels.
 */

EXPORT effect_t effects[] = {
	{"null", 0, 			/* stand-in, never gets called */
		nothing, nothing, nothing, null_drain, nothing},
	{"copy", EFF_MCHAN, 
		copy_getopts, copy_start, copy_flow, null_drain, nothing},
	{"avg", EFF_CHAN | EFF_MCHAN, 
		avg_getopts, avg_start, avg_flow, null_drain, avg_stop},
	{"split", EFF_CHAN | EFF_MCHAN, 
		split_getopts, split_start, split_flow, null_drain,split_stop},
	{"pick", EFF_CHAN | EFF_MCHAN, 
		pick_getopts, pick_start, pick_flow, null_drain, pick_stop},
	{"pred", 0,
		pred_getopts, pred_start, pred_flow, null_drain, pred_stop},
	{"stat", EFF_MCHAN | EFF_REPORT, 
		stat_getopts, stat_start, stat_flow, null_drain, stat_stop},
	{"vibro", 0, 
		vibro_getopts, vibro_start, vibro_flow, null_drain, nothing},
	{"echo", 0, 
		echo_getopts, echo_start, echo_flow, echo_drain, echo_stop},
	{"band", 0, 
		band_getopts, band_start, band_flow, null_drain, band_stop},
	{"lowp", 0, 
		lowp_getopts, lowp_start, lowp_flow, null_drain, lowp_stop},
	{"highp", 0, 
		highp_getopts, highp_start, highp_flow, null_drain,highp_stop},
#ifdef	USE_DYN
	{"dyn", 0, 
		dyn_getopts, dyn_start, dyn_flow, null_drain, dyn_stop},
#endif
	{"rate", EFF_RATE, 
		rate_getopts, rate_start, rate_flow, null_drain, nothing},
	/* fast sub-sampled downconverter.  doesn't quite work.
	{"down", EFF_RATE, 
		down_getopts, down_start, down_flow, null_drain, nothing}, */
	/* the good one! */
	{"resample", EFF_RATE, 
		resample_getopts, resample_start, resample_flow, 
		resample_drain, resample_stop},
	{"reverse", 0, 
		reverse_getopts, reverse_start, 
		reverse_flow, reverse_drain, reverse_stop},
	{"map", EFF_REPORT, 
		map_getopts, map_start, map_flow, null_drain, nothing},
	{"cut", EFF_MCHAN, 
		cut_getopts, cut_start, cut_flow, null_drain, nothing},
	{"mask", EFF_MCHAN, 
		mask_getopts, nothing, mask_flow, null_drain, nothing},
	0
};

