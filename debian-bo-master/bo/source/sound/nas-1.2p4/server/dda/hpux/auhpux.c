/**
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)auhpux.c,v 1.2 1995/11/28 02:44:01 greg Exp $
 */

/* Supplied by : William J Hunter (III), Email : whunter@melpar.esys.com	*/

/*---------------------------------------------------------
 Hewlett Packard Device Dependent Server Version 1.0

 CHANGES :	Author : J D Brister (University of Manchester, Computer Graphics Unit)
																		
 (C) Copyright 1995, The University of Manchester, United Kingdom
 
  THIS SOFTWARE IS PROVIDED `AS-IS'.  THE UNIVERSITY OF MANCHESTER,
 DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL THE UNIVERSITY
 OF MANCHESTER, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.						

 This program may be used freely within the UK academic community, but it must not be used 
 for commercial gain without the written permission of the authors.

 EMail :  cgu-info@cgu.mcc.ac.uk

 NOTE : THIS SERVER MUST BE RUN AS ROOT.
-----------------------------------------------------------*/

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/lock.h>
#include <sys/audio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <audio/audio.h>
#include <audio/Aproto.h>
#include <errno.h>

#include "dixstruct.h"				/* for RESTYPE */
#include "os.h"					/* for xalloc/xfree and NULL */
#include "au.h"

extern int errno=0;

extern void     AuProcessData();

static int      devAudio = -1,
                devAudioCtl = -1;

struct audio_describe		audio_describe;
struct raw_audio_config		raw_params;
struct audio_limits		audio_limits;
struct audio_select_thresholds	select_thresholds;


static AuUint8 *auOutputMono,
                        *auOutputStereo,
                        *auInputMono,
                        *emptyOutput;

static AuUint32 *monoSamples,
                          *stereoSamples;

static AuInt16  outputGain,
                         inputGain;

static AuUint32 outputMode, inputMode;
static AuBool    updateGains;
static AuFixedPoint currentOutputGain;

extern AuInt32  auMinibufSamples;

char *V_STRING;



#define	HPUX_VENDOR 	"HPUX /dev/audio"
#define	SERVER_CLIENT	0
#define       MINIBUF_SIZE	         1024

#define auMinSampleRate	(audio_describe.sample_rate[0])
#define auMaxSampleRate	(audio_describe.sample_rate[audio_describe.nrates-1])

#define auPhysicalOutputChangableMask					      \
    (AuCompDeviceGainMask | AuCompDeviceOutputModeMask)

#define auPhysicalOutputValueMask					       \
    (AuCompCommonAllMasks |						       \
     AuCompDeviceMinSampleRateMask |					       \
     AuCompDeviceMaxSampleRateMask |					       \
     AuCompDeviceOutputModeMask |					       \
     AuCompDeviceGainMask |						       \
     AuCompDeviceLocationMask |						       \
     AuCompDeviceChildrenMask)

#define auPhysicalInputChangableMask 						\
    (AuCompDeviceGainMask | AuCompDeviceLineModeMask)

#define auPhysicalInputValueMask					       \
    (AuCompCommonAllMasks |						       \
     AuCompDeviceMinSampleRateMask |					       \
     AuCompDeviceMaxSampleRateMask |					       \
     AuCompDeviceLocationMask |						       \
     AuCompDeviceGainMask |							\
     AuCompDeviceChildrenMask)


#define auBucketChangableMask	0
#define auBucketValueMask	AuCompBucketAllMasks



#ifndef BUILTIN_BUCKETS
#define NUM_BUILTIN_BUCKETS	0
#else						/* BUILTIN_BUCKETS */
static struct
{
    AuUint8        *data,
                         format,
                         numTracks;
    AuUint32        sampleRate,
                           numSamples;
    char          **comment;
}
                builtinBuckets[] =
{
    boingSamples,
    boingFormat,
    boingNumTracks,
    boingSampleRate,
    boingNumSamples,
    &boingComment,
};



#define NUM_BUILTIN_BUCKETS						       \
    (sizeof(builtinBuckets) / sizeof(builtinBuckets[0]))
#endif						/* BUILTIN_BUCKETS */

/*----------------------------------------------------------*/

static int
createServerComponents(auServerDeviceListSize, auServerBucketListSize,
		       auServerRadioListSize, auServerMinRate,
		       auServerMaxRate)
AuUint32       *auServerDeviceListSize,
               	    *auServerBucketListSize,
               	    *auServerRadioListSize,
                      *auServerMinRate,
                      *auServerMaxRate;
{
    AuDeviceID      stereo,
                    	   mono;
    ComponentPtr    d,
                             *p;

    extern RESTYPE  auComponentType;
    extern ComponentPtr *auServerDevices,	/* array of devices */
                   *auServerBuckets,		/* array of server owned
						 * buckets */
                   *auServerRadios,		/* array of server owned
						 * radios */
                    auDevices,			/* list of all devices */
                    auBuckets,			/* list of all buckets */
                    auRadios;			/* list of all radios */
    extern AuUint32 auNumServerDevices,		/* number of devices */
                    auNumActions,		/* number of defined actions */
                    auNumServerBuckets,		/* number of server owned
						 * buckets */
                    auNumServerRadios;		/* number of server owned
						 * radios */

    *auServerMinRate = auMinSampleRate;
    *auServerMaxRate = auMaxSampleRate;

    auNumServerDevices = *auServerDeviceListSize = *auServerBucketListSize =
	*auServerRadioListSize = 0;

    stereo = FakeClientID(SERVER_CLIENT);
    mono = FakeClientID(SERVER_CLIENT);

    AU_ALLOC_DEVICE(d, 1, 0);
    d->id = mono;
    d->changableMask = auPhysicalOutputChangableMask;
    d->valueMask = auPhysicalOutputValueMask;
    d->kind = AuComponentKindPhysicalOutput;
    d->use = AuComponentUseExportMask;
    d->access = AuAccessExportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 1;
    d->description.type = AuStringLatin1;
    d->description.string = "Mono Channel Output";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationCenterMask | AuDeviceLocationInternalMask; 
    d->numChildren = 0;
    d->minibuf = auOutputMono;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputMono;
    monoSamples = &d->minibufSamples;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 2, 1);
    d->id = stereo;
    d->changableMask = auPhysicalOutputChangableMask;
    d->valueMask = auPhysicalOutputValueMask;
    d->kind = AuComponentKindPhysicalOutput;
    d->use = AuComponentUseExportMask;
    d->access = AuAccessExportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 2;
    d->description.type = AuStringLatin1;
    d->description.string = "Stereo Channel Output";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationCenterMask | AuDeviceLocationInternalMask;
    d->numChildren = 1;
    d->children = (AuID *) ((AuUint8 *) d + PAD4(sizeof(ComponentRec)));
    d->childSwap = (char *) (d->children + d->numChildren);
    d->children[0] = mono;
    d->minibuf = auOutputStereo;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputStereo;
    stereoSamples = &d->minibufSamples;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 1, 0);
    d->id = FakeClientID(SERVER_CLIENT);
    d->changableMask = auPhysicalInputChangableMask;
    d->valueMask = auPhysicalInputValueMask;
    d->kind = AuComponentKindPhysicalInput;
    d->use = AuComponentUseImportMask;
    d->access = AuAccessImportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 1;
    d->description.type = AuStringLatin1;
    d->description.string = "Mono Channel Input";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationRightMask | AuDeviceLocationLeftMask | AuDeviceLocationExternalMask;	/* should extern mask be here ? */
    d->numChildren = 0;
    d->gain = AuFixedPointFromFraction(inputGain * 100, AUDIO_MAX_GAIN);
/**/    d->lineMode = inputMode == AUDIO_IN_MIKE ? AuDeviceLineModeHigh :
	AuDeviceLineModeLow;
    d->minibuf = auInputMono;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalInputMono;
    AU_ADD_DEVICE(d);

    /* set the array of server devices */
    if (!(auServerDevices =
       (ComponentPtr *) aualloc(sizeof(ComponentPtr) * auNumServerDevices)))
	return AuBadAlloc;

    p = auServerDevices;
    d = auDevices;

    while (d)
    {
	*p++ = d;
	d = d->next;
    }

#ifdef BUILTIN_BUCKETS
    for (i = 0; i < NUM_BUILTIN_BUCKETS; i++)
    {
	ALLOC_BUCKET(d);
	d->data = builtinBuckets[i].data;
	d->format = builtinBuckets[i].format;
	d->numTracks = builtinBuckets[i].numTracks;
	d->sampleRate = builtinBuckets[i].sampleRate;
	d->numSamples = builtinBuckets[i].numSamples;
	d->description.string = *builtinBuckets[i].comment;

	d->id = FakeClientID(SERVER_CLIENT);
	d->changableMask = auBucketChangableMask;
	d->valueMask = auBucketValueMask;
	d->kind = AuComponentKindBucket;
	d->use = AuComponentUseImportMask;
	d->access = AuAccessImportMask | AuAccessListMask;
	d->description.type = AuStringLatin1;
	d->description.len = strlen(d->description.string);
	d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	    d->numTracks;
	d->physicalDeviceMask = NotAPhysicalDevice;
	d->dataSize = d->numSamples * sizeofFormat(d->format) * d->numTracks;
	d->dataEnd = d->data + d->dataSize;
	d->read = d->write = d->data;
	d->destroyed = AuFalse;
	ADD_BUCKET(d);
    }

    /* set the array of server buckets */
    if (!(auServerBuckets = (ComponentPtr *) aualloc(sizeof(ComponentPtr) *
						     auNumServerBuckets)))
	return AuBadAlloc;

    p = auServerBuckets;
    d = auBuckets;

    while (d)
    {
	*p++ = d;
	d = d->next;
    }
#endif					       /* BUILTIN_BUCKETS */

    return AuSuccess;
}

/*----------------------------------------------------------*/

static void BlockUntilClear()
{
	int			clear = 0;
	struct audio_status	status_b;

	/*
	 * Ignore signal
	 */
	signal(SIGALRM, SIG_IGN);

	/*
	 * Reset Device
	 */

#ifndef NULL_AUDIO_DEVICE
	if (ioctl(devAudio,AUDIO_RESET,(RESET_RX_BUF | RESET_TX_BUF | RESET_RX_OVF | RESET_TX_UNF)) == -1 ) perror("Audio_Reset");
	errno = 0;

	do {
                if (ioctl(devAudio, AUDIO_GET_STATUS, &status_b) == -1)
		{
			perror("AUDIO_GET_STATUS");
			errno = 0;
		}
		else
		{
#ifdef __DEBUG__
			fprintf(stderr, "RX Status = %d\n",
					status_b.receive_status);
			fprintf(stderr, "TX Status = %d\n",
					status_b.transmit_status);
			fprintf(stderr, "RX Buffer Count = %d\n",
					status_b.receive_buffer_count);
			fprintf(stderr, "TX Buffer Count = %d\n",
					status_b.transmit_buffer_count);
			fprintf(stderr, "RX Overflow = %d\n",
					status_b.receive_overflow_count);
			fprintf(stderr, "TX Underflow = %d\n",
					status_b.transmit_underflow_count);
#endif

			clear = (status_b.transmit_status == AUDIO_DONE);
			if (!clear)
			{
				sleep(1);
			}
		}
	} while (!clear);
#endif
}

/*----------------------------------------------------------*/

static AuUint32
setSampleRate(rate)
AuUint32        rate;
{
	int			i;
	AuUint32		target_rate;
	struct itimerval	ntval, otval;
	int			timer_us;


	/* BlockUntilClear(); */

	for (i = 0; i < audio_describe.nrates; i++)
	{
		if (rate >= audio_describe.sample_rate[i])
		{
			target_rate = audio_describe.sample_rate[i];
		}
	}

#ifdef __DEBUG__
	fprintf(stderr,"Setting rate to %d (should be %d)\n",target_rate,rate);
#endif
#ifndef NULL_AUDIO_DEVICE
	if (ioctl(devAudio,AUDIO_SET_SAMPLE_RATE, target_rate) == -1)
           perror("Audio_set_sample_rate");
	  errno = 0;
#endif

	/*
	 * Reset timer
	 */
	timer_us = (auMinibufSamples * 500000) / target_rate;
#ifdef __DEBUG__
        fprintf(stderr, "Setting timer to %dus\n", timer_us);
#endif
	ntval.it_interval.tv_sec = 0;
	ntval.it_interval.tv_usec = timer_us;
	ntval.it_value.tv_sec = 0;
	ntval.it_value.tv_usec = timer_us;
	setitimer(ITIMER_REAL, &ntval, &otval);


	return(target_rate);
}

/*----------------------------------------------------------*/

static void
eventPosted()
{
#ifdef __DEBUG__
	fprintf(stderr,"An event has been posted\n");
#endif
}

/*----------------------------------------------------------*/

static void
serverReset()
{
    signal(SIGALRM, SIG_IGN);

#ifdef __DEBUG__
    fprintf(stderr,"Server resetting\n");
#endif
#ifndef NULL_AUDIO_DEVICE
    if (ioctl(devAudio,AUDIO_DRAIN, 0) == -1) perror("Audio_drain");	       /* drain everything out */
    errno = 0;

    close(devAudio);
#endif
    devAudio = -1;
}

/*----------------------------------------------------------*/

/**
  * Gains are mapped thusly:
  *
  *   Software   0 - 49     50 - 100
  *   Hardware   0 - 49     50 - 255
  */
static void
setPhysicalOutputGain(gain)
AuFixedPoint    gain;
{
    AuInt16         g = AuFixedPointIntegralAddend(gain);


    if (g < 50)
	outputGain = g;
    else
	/* (gain - 50) * (205 / 50) + 50 */
	outputGain = ((0x41999 * (g - 50)) >> 16) + 50;


    outputGain = g;

    updateGains = AuTrue;
    currentOutputGain = gain;

}

/*----------------------------------------------------------*/

static          AuFixedPoint
getPhysicalOutputGain()
{
    return currentOutputGain;
}

/*----------------------------------------------------------*/

static void setPhysicalOutputMode(lineMode)
AuUint8  lineMode;
{
  int ret;

#ifdef __DEBUG__
  fprintf(stderr,"Setting physical output mode......\n");
#endif

  if ((outputMode == AUDIO_OUT_INTERNAL)&& (lineMode & AuDeviceOutputModeHeadphone))
  {
	outputMode = AUDIO_OUT_EXTERNAL; 
  }
   else
   {
        if ((outputMode == AUDIO_OUT_EXTERNAL)&&(lineMode & AuDeviceOutputModeHeadphone))
        {
    	    outputMode = AUDIO_OUT_LINE;	
        }
#ifdef __AUDIO__II__
	else
	{
	    if ((outputMode == AUDIO_OUT_LINE)&&(lineMode & AuDeviceOutputModeHeadphone))
             {
	         outputMode = AUDIO_OUT_INTERNAL;
             } 
	}
#endif
    }
    
    updateGains = AuTrue; 

    if (ioctl(devAudio,AUDIO_SET_OUTPUT, outputMode)==-1)perror("set_output");
    errno = 0;
}

/*----------------------------------------------------------*/

static AuUint8
getPhysicalOutputMode()
{
    printf("Getting physical output mode....\n");
    return outputMode == AUDIO_OUT_INTERNAL ? AuDeviceOutputModeSpeaker :
	AuDeviceOutputModeHeadphone;

    return AuDeviceOutputModeHeadphone;

}

/*----------------------------------------------------------*/

static void
setPhysicalInputGainAndLineMode(gain, lineMode)
AuFixedPoint    gain;
AuUint8         lineMode;
{
    AuInt16         g = AuFixedPointIntegralAddend(gain);
    int in;
    if (g < 50)
	inputGain = g;
    else
	/* (gain - 50) * (205 / 50) + 50 */
	inputGain = ((0x41999 * (g - 50)) >> 16) + 50;

    /*inputGain = g;*/
    printf("Input gain : %d\n", g);
    updateGains = AuTrue;

#ifdef __DEBUG__
  fprintf(stderr,"Line mode = %d\n",lineMode);
#endif

#ifdef __AUDIO__II__
  if ((inputMode == AUDIO_IN_LINE)&& (lineMode & AuDeviceInputModeMicrophone))
  {
	inputMode = AUDIO_IN_MIKE; 	
  }
   else
   {
    	    inputMode = AUDIO_IN_LINE;
    }
#endif

#ifdef __DEBUG__ 
    fprintf(stderr,"Input mode = %d\n",inputMode);
#endif

    if (ioctl(devAudio,AUDIO_SET_INPUT, inputMode) == -1) perror("Audio_set_input");
    errno = 0;

  if (ioctl(devAudio,AUDIO_GET_INPUT, &in) == -1) perror("Audio_get_input");  

#ifdef __DEBUG__
    fprintf(stderr,"GET_INPUT : %d\n", in);
#endif
}

/*----------------------------------------------------------*/

static void
writeEmptyOutput()
{

#ifdef __DEBUG__
    fprintf(stderr, "Writing empty\n");
#endif
#ifndef NULL_AUDIO_DEVICE
    write(devAudio, emptyOutput, (auNativeBytesPerSample * auMinibufSamples));
#endif
}

/*----------------------------------------------------------*/

static void
writeOutput(p, n)
AuInt16        *p;
unsigned int    n;
{
#ifndef NULL_AUDIO_DEVICE
    write(devAudio, p, (auNativeBytesPerSample * n));
#endif
}

/*----------------------------------------------------------*/

static void
writePhysicalOutputsMono()
{
#ifdef __DEBUG__
    fprintf(stderr, "Writing mono\n");
#endif
    writeOutput(auOutputMono, *monoSamples);
}

/*----------------------------------------------------------*/

static void
writePhysicalOutputsStereo()
{
#ifdef __DEBUG__
    fprintf(stderr, "Writing stereo\n");
#endif
    writeOutput(auOutputStereo, (2 * (*stereoSamples)));
}

/*----------------------------------------------------------*/

static void
writePhysicalOutputsBoth()
{
#ifdef __DEBUG__
    fprintf(stderr, "Writing both\n");
#endif
    writeOutput(auOutputStereo, (2 * (*stereoSamples)));
}

/*----------------------------------------------------------*/

static void
readPhysicalInputs()
{
#ifdef __DEBUG__
    fprintf(stderr, "Writing input\n");
#endif
    read(devAudio, auInputMono, (auNativeBytesPerSample * auMinibufSamples));
}

/*----------------------------------------------------------*/

static void
setWritePhysicalOutputFunction(flow, funct)
CompiledFlowPtr flow;
void            (**funct) ();
{
    AuUint32        mask = flow->physicalDeviceMask;
    int		    num_channels = 1;

    fprintf(stderr, "Playing......\n");

    if ((mask & (PhysicalOutputMono | PhysicalOutputStereo)) ==
	(PhysicalOutputMono | PhysicalOutputStereo))
    {
	*funct = writePhysicalOutputsBoth;
        num_channels = 2;
 	fprintf(stderr, "Playing both......\n");
    }
    else if (mask & PhysicalOutputMono)
    {
	*funct = writePhysicalOutputsMono;
 	fprintf(stderr, "Playing mono......\n");
    }
    else if (mask & PhysicalOutputStereo)
    {
	*funct = writePhysicalOutputsStereo;
        num_channels = 2;
	 fprintf(stderr, "Playing stereo......\n");
    }
    else
    {
	*funct = writeEmptyOutput;
 	fprintf(stderr, "Playing m/t......\n");
    }

     /* BlockUntilClear(); */

#ifndef NULL_AUDIO_DEVICE
    if (ioctl(devAudio, AUDIO_SET_CHANNELS, num_channels) == -1)
    {
	perror("AUDIO_SET_CHANNELS");
         errno = 0;
    }
    else
    {
#ifdef __DEBUG__
	fprintf(stderr, "Set channels to %d\n", num_channels);
#endif
    }
#endif
}

/*----------------------------------------------------------*/

void
processAudioSignal(sig)
int             sig;
{
    struct audio_gain	gains;

    signal(SIGALRM, SIG_IGN);

    printf("Processing audio signal....\n");

    if (updateGains)
    {
#ifndef NULL_AUDIO_DEVICE
	if (ioctl(devAudio,AUDIO_GET_GAINS, &gains)==-1)perror("get_gains");
         errno = 0;
#endif
	gains.cgain[0].receive_gain = inputGain;
	gains.cgain[0].transmit_gain = outputGain;
	gains.cgain[0].monitor_gain = AUDIO_OFF_GAIN;
	gains.cgain[1].receive_gain = inputGain;
	gains.cgain[1].transmit_gain = outputGain;
	gains.cgain[1].monitor_gain = AUDIO_OFF_GAIN;
	gains.channel_mask = (AUDIO_CHANNEL_LEFT | AUDIO_CHANNEL_RIGHT);
#ifndef NULL_AUDIO_DEVICE
	if (ioctl(devAudio,AUDIO_SET_GAINS, &gains) == -1)perror("Audio_set_gains");
         errno = 0;
#endif
	updateGains = AuFalse;
    }

    AuProcessData();

    signal(SIGALRM, processAudioSignal);
}

/*----------------------------------------------------------*/

static void
enableProcessFlow()
{
#ifdef __DEBUG__
    fprintf(stderr, "Enabling flow\n");
#endif
    writeEmptyOutput();
    signal(SIGALRM, processAudioSignal);
}

/*----------------------------------------------------------*/

static void
disableProcessFlow()
{
    signal(SIGALRM, SIG_IGN);
#ifndef NULL_AUDIO_DEVICE
    if (ioctl(devAudio,AUDIO_DRAIN, 0) == -1)perror("Audio_drain");
    errno = 0;
#endif
#ifdef __DEBUG__
    fprintf(stderr, "Disabling flow\n");
#endif
}

/*----------------------------------------------------------*/

#define	PhysicalOneTrackBufferSize					       \
    PAD4(auMinibufSamples * auNativeBytesPerSample * 1)
#define	PhysicalTwoTrackBufferSize					       \
    PAD4(auMinibufSamples * auNativeBytesPerSample * 2)

/*----------------------------------------------------------*/

AuBool
AuInitPhysicalDevices()
{
    static AuUint8 *physicalBuffers;
    AuUint32        physicalBuffersSize;
    extern AuUint32 auPhysicalOutputBuffersSize;
    extern AuUint8 *auPhysicalOutputBuffers;
    extern AuBool   AuInitPhysicalDevices_dbri();
    struct itimerval        ntval, otval;
    int                     timer_us;



    int	temp_int, input;

    if (V_STRING)
    {
	aufree(V_STRING);
	V_STRING = (char *) 0;
    }



    if (devAudio == -1)
    {
#ifndef NULL_AUDIO_DEVICE
	if ((devAudio = open("/dev/audio", O_RDWR)) == -1 ||
	    (devAudioCtl = open("/dev/audioCtl", O_RDWR)) == -1)
        {
            perror("/dev/audio");
	   errno = 0;
             close(devAudio);
	    close(devAudioCtl);
	    devAudio = devAudioCtl = -1;
	    return AuFalse;
        }
#endif

	/*
	 * Set process to run in real time
	 */
	if (rtprio(0, 30) == -1)
	{
		perror("rtprio");
		errno = 0;
	}
	else
	{
		if (plock(PROCLOCK) == -1)
		{
			perror("plock");
			errno = 0;
		}
	}
    }

    if (!(V_STRING = (char *) aualloc(strlen(HPUX_VENDOR) + 1)))
	return AuFalse;

    strcpy(V_STRING, HPUX_VENDOR);

    if (physicalBuffers)
	aufree(physicalBuffers);

    if (emptyOutput)
	aufree(emptyOutput);

    auMinibufSamples = MINIBUF_SIZE;

    if (!(emptyOutput = (AuUint8 *) aualloc(auMinibufSamples)))
	return AuFalse;

/*
    auset(emptyOutput, 0xff, auMinibufSamples);
*/
    auset(emptyOutput, 0x00, auMinibufSamples);

    /* the output buffers need to be twice as large for output range checking */
    physicalBuffersSize =
	PhysicalOneTrackBufferSize +		/* mono input */
	PhysicalOneTrackBufferSize * 2 +	/* mono output */
	PhysicalTwoTrackBufferSize * 2;		/* stereo output */

    if (!(physicalBuffers = (AuUint8 *) aualloc(physicalBuffersSize)))
	return AuFalse;

    auInputMono = physicalBuffers;
    auOutputMono = auInputMono + PhysicalOneTrackBufferSize;
    auOutputStereo = auOutputMono + 2 * PhysicalOneTrackBufferSize;

    auPhysicalOutputBuffers = auOutputMono;
    auPhysicalOutputBuffersSize = physicalBuffersSize -
	PhysicalOneTrackBufferSize;

    signal(SIGALRM, SIG_IGN);
    fprintf(stderr,"Init function called......\n");
    timer_us = (auMinibufSamples * 500000) / 8000;
#ifdef __DEBUG__
    fprintf(stderr,"Setting timer to %dus\n", timer_us);
#endif
    ntval.it_interval.tv_sec = 0;
    ntval.it_interval.tv_usec = timer_us;
    ntval.it_value.tv_sec = 0;
    ntval.it_value.tv_usec = timer_us;
    setitimer(ITIMER_REAL, &ntval, &otval);


    AuRegisterCallback(AuCreateServerComponentsCB, createServerComponents);
    AuRegisterCallback(AuSetPhysicalOutputGainCB, setPhysicalOutputGain);
    AuRegisterCallback(AuGetPhysicalOutputGainCB, getPhysicalOutputGain);
    AuRegisterCallback(AuGetPhysicalOutputModeCB, getPhysicalOutputMode);
    AuRegisterCallback(AuSetPhysicalOutputModeCB, setPhysicalOutputMode);
    AuRegisterCallback(AuSetPhysicalInputGainAndLineModeCB,
		       setPhysicalInputGainAndLineMode);
    AuRegisterCallback(AuEnableProcessFlowCB, enableProcessFlow);
    AuRegisterCallback(AuDisableProcessFlowCB, disableProcessFlow);
    AuRegisterCallback(AuReadPhysicalInputsCB, readPhysicalInputs);
    AuRegisterCallback(AuSetWritePhysicalOutputFunctionCB,
		       setWritePhysicalOutputFunction);

    AuRegisterCallback(AuSetSampleRateCB, setSampleRate);
    AuRegisterCallback(AuEventPostedCB, eventPosted);

    /*
     * Get Device Information
     */
#ifndef NULL_AUDIO_DEVICE
    if (ioctl(devAudio,AUDIO_DESCRIBE, &audio_describe)==-1)perror("Audio_describe");
    errno = 0;
    if (ioctl(devAudio,AUDIO_RAW_GET_PARAMS, &raw_params)==-1)perror("raw_get_params");
    errno = 0;
#endif

    /*
     * Display configuration
     */
    switch (audio_describe.audio_id)
    {
    case AUDIO_ID_CS4215 :
#ifdef __DEBUG__
	fprintf(stderr, "	Control:	0x%x\n",
		raw_params.audio_conf_union.cs4215_conf.control);
	fprintf(stderr, "	DMA Status:	0x%x\n",
		raw_params.audio_conf_union.cs4215_conf.control);
	fprintf(stderr, "	Gain Control:	0x%x\n",
		raw_params.audio_conf_union.cs4215_conf.gainctl);
	fprintf(stderr, "	Over Range:	0x%x\n",
		raw_params.audio_conf_union.cs4215_conf.over_range);
	fprintf(stderr, "	PIO:		0x%x\n",
		raw_params.audio_conf_union.cs4215_conf.pio);
#endif
	break;
    case AUDIO_ID_PSB2160 :
	break;
    }

    /*
     * Get buffer limits
     */
#ifndef NULL_AUDIO_DEVICE
    if (ioctl(devAudio,AUDIO_GET_LIMITS, &audio_limits)==-1) perror("get_limits");
    errno = 0;

    /*
     * Set buffer sizes
     */
    if (ioctl(devAudio,AUDIO_SET_RXBUFSIZE, (8192 * 2))==-1)perror("set_rxbufsize");
    errno = 0;
    if (ioctl(devAudio,AUDIO_GET_RXBUFSIZE, &temp_int)==-1)perror("get_rxbufsize");
    errno = 0;
#ifdef __DEBUG__
    fprintf(stderr, "RX buffer size = %d\n", temp_int);
#endif
    if (ioctl(devAudio,AUDIO_SET_TXBUFSIZE, (8192 * 2))==-1)perror("set_txbufsize");
    errno = 0;
    if (ioctl(devAudio,AUDIO_GET_TXBUFSIZE, &temp_int)==-1)perror("get_txbufsize");
    errno = 0;
#ifdef __DEBUG__
    fprintf(stderr, "TX buffer size = %d\n", temp_int);
#endif
#endif

    /*
     * Set threshold
     */
    select_thresholds.read_threshold = (MINIBUF_SIZE);
    select_thresholds.write_threshold = (MINIBUF_SIZE);
#ifndef NULL_AUDIO_DEVICE
    if (ioctl(devAudio,AUDIO_SET_SEL_THRESHOLD, &select_thresholds)==-1)perror("set_sel_threshold");
    errno = 0;
    if (ioctl(devAudio,AUDIO_GET_SEL_THRESHOLD, &select_thresholds)==-1)perror("get_sel_thresohold");
    errno = 0;

#ifdef __DEBUG__
    fprintf(stderr,"Read threshold: %d\n", select_thresholds.read_threshold);
    fprintf(stderr,"Write threshold: %d\n", select_thresholds.write_threshold);
#endif
#endif

    /*
     * Set to NAS format
     */
#ifndef NULL_AUDIO_DEVICE
    if (ioctl(devAudio,AUDIO_SET_DATA_FORMAT, AUDIO_FORMAT_LINEAR16BIT)==-1)perror("set_date_format");
    errno = 0;
    if (ioctl(devAudio,AUDIO_GET_DATA_FORMAT, &temp_int)==-1)perror("get_data_format");
    errno = 0;
#endif
#ifdef __DEBUG__
    fprintf(stderr, "Audio Format = %d (Lin 16 = %d)\n",
		temp_int, AUDIO_FORMAT_LINEAR16BIT);
#endif

    /*
     * Send output to headphone jack
     * and input to mike jack
     */
#ifndef NULL_AUDIO_DEVICE
    if (ioctl(devAudio,AUDIO_SET_OUTPUT, AUDIO_OUT_INTERNAL)==-1)perror("set_output");
    errno = 0;
    if (ioctl(devAudio,AUDIO_GET_OUTPUT, &temp_int)==-1)perror("get_output");
    errno = 0;
    if (ioctl(devAudio,AUDIO_SET_INPUT, AUDIO_IN_MIKE)==-1)perror("set_input");
    errno = 0;
    fprintf(stderr, "Input Port   (Mike = %d) (Errorno : %d)\n", AUDIO_IN_MIKE, errno); 
    if (ioctl(devAudio,AUDIO_GET_INPUT, &input)==-1)perror("get_input");
    errno = 0;
#endif
#ifdef __DEBUG__
    fprintf(stderr, "Output Port = %d (Headphones = %d)\n", temp_int,
		AUDIO_OUT_EXTERNAL); 

     fprintf(stderr, "Input Port = %d  (Mike = %d) (Errorno : %d)\n", input, AUDIO_IN_MIKE, errno); 
#endif
    outputMode = temp_int;

    /*
     * Set number of tracks to 2
     */
#ifndef NULL_AUDIO_DEVICE
    if (ioctl(devAudio,AUDIO_SET_CHANNELS, 2)==-1)perror("set_channels");
    errno = 0;
    if (ioctl(devAudio,AUDIO_GET_CHANNELS, &temp_int)==-1)perror("get_channels");
    errno = 0;
#endif
#ifdef __DEBUG__
    fprintf(stderr, "Number of channels = %d\n", temp_int);
#endif

    currentOutputGain = outputGain < 50 ? AuFixedPointFromSum(outputGain, 0) :
	(outputGain - 50) * 0x3e70 + 0x320000;

    /* bogus resource so we can have a cleanup function at server reset */
    AddResource(FakeClientID(SERVER_CLIENT),
		CreateNewResourceType(serverReset), 0);

    return AuTrue;
}
