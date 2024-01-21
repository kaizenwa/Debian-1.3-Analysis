/***********************************************************
*  Rocks'n'Diamonds -- McDuffin Strikes Back!              *
*----------------------------------------------------------*
*  ©1995 Artsoft Development                               *
*        Holger Schemel                                    *
*        33659 Bielefeld-Senne                             *
*        Telefon: (0521) 493245                            *
*        eMail: aeglos@valinor.owl.de                      *
*               aeglos@uni-paderborn.de                    *
*               q99492@pbhrzx.uni-paderborn.de             *
*----------------------------------------------------------*
*  sound.c                                                 *
*                                                          *
*  Letzte Aenderung: 15.06.1995                            *
***********************************************************/

#ifndef SOUND_H
#define SOUND_H

#include "main.h"
#include <math.h>

#ifdef linux
#include <linux/soundcard.h>
#ifndef VOXWARE
#define VOXWARE
#endif
/* where is the right declaration for 'ioctl'? */
extern void ioctl(long, long, void *);
#endif

#define SND_BLOCKSIZE 4096

#ifdef _HPUX_SOURCE
#include <sys/audio.h>
#undef  SND_BLOCKSIZE
#define SND_BLOCKSIZE 32768
#define HPUX_AUDIO
#endif /* _HPUX_SOURCE */

/* the names of the sounds */
#define SND_AMOEBE	0
#define SND_ANTIGRAV	1
#define SND_AUTSCH	2
#define SND_BONG	3
#define SND_FUEL	4
#define SND_HALLOFFAME	5
#define SND_HOLZ	6
#define SND_HUI		7
#define SND_KABUMM	8
#define SND_KINK	9
#define SND_KLING	10
#define SND_LASER	11
#define SND_OEFFNEN	12
#define SND_QUIEK	13
#define SND_RHYTHMLOOP	14
#define SND_ROAAAR	15
#define SND_SIRR	16
#define SND_SLURP	17
#define SND_VOYAGER	18
#define SND_WARNTON	19
#define SND_WHOOSH	20
#define SND_TYGER	21

#define NUM_SOUNDS	22

#define MAX_SOUNDS_PLAYING	16

/* some values for PlaySound(), StopSound() and friends */
#define PSND_SILENCE		0
#define PSND_MAX_VOLUME_BITS	7
#define PSND_MIN_VOLUME		0
#define PSND_MAX_VOLUME		(1 << PSND_MAX_VOLUME_BITS)
#define PSND_NO_LOOP		0
#define PSND_LOOP		1
#define PSND_MIDDLE		0
#define PSND_MAX_STEREO_BITS	7
#define PSND_MAX_STEREO		(1 << PSND_MAX_STEREO_BITS)
#define PSND_MAX_LEFT		(-PSND_MAX_STEREO)
#define PSND_MAX_RIGHT		(+PSND_MAX_STEREO)
#define PSND_MAX_LEFT2RIGHT_BITS (PSND_MAX_STEREO_BITS+1)
#define PSND_MAX_LEFT2RIGHT	(1 << PSND_MAX_LEFT2RIGHT_BITS)

#define SSND_FADE_SOUND		(1<<0)
#define SSND_FADE_ALL_SOUNDS	(1<<1)
#define SSND_FADING		(SSND_FADE_SOUND | SSND_FADE_ALL_SOUNDS)
#define SSND_STOP_SOUND		(1<<2)
#define SSND_STOP_ALL_SOUNDS	(1<<3)
#define SSND_STOPPING		(SSND_STOP_SOUND | SSND_STOP_ALL_SOUNDS)

/* settings for sound path, sound device, etc. */
#ifndef SND_PATH
#define SND_PATH	"./sounds"
#endif

#define DEV_AUDIO	"/dev/audio"
#define DEV_DSP		"/dev/dsp"

#ifdef	VOXWARE
#define SOUND_DEVICE   	DEV_DSP
#else
#define SOUND_DEVICE	DEV_AUDIO
#endif

#define SOUND_OFF	0
#define	SOUND_AVAILABLE	1

#ifdef NO_SOUNDS
#define SOUND_STATUS	SOUND_OFF
#else
#define SOUND_STATUS	SOUND_AVAILABLE
#endif

struct SoundHeader_SUN
{
  unsigned long magic;
  unsigned long hdr_size;
  unsigned long data_size;
  unsigned long encoding;
  unsigned long sample_rate;
  unsigned long channels;
};

struct SoundHeader_8SVX
{
  char magic_FORM[4];
  unsigned long chunk_size;
  char magic_8SVX[4];
};

struct SoundInfo
{ 
  char *name;
  char *file_ptr, *data_ptr;
  long file_len, data_len;
};

struct SoundControl
{
  int nr;
  int volume;
  int stereo;
  BOOL active;
  BOOL loop;
  BOOL fade_sound;
  BOOL stop_sound;
  BOOL stop_all_sounds;
  int playingtime;
  long playingpos;
  long data_len;
  char *data_ptr;
};

/* sound server functions */
void SoundServer(void);
void SoundServer_InsertNewSound(struct SoundControl);
void SoundServer_StopSound(int);
void SoundServer_StopAllSounds(void);
void HPUX_Audio_Control(void);
unsigned char linear_to_ulaw(int);
int ulaw_to_linear(unsigned char);

/* application functions */
BOOL LoadSound(int);
void PlaySound(int);
void PlaySoundStereo(int, int);
void PlaySoundLoop(int);
void PlaySoundExt(int, int, int, BOOL);
void FadeSound(int);
void FadeSounds(void);
void StopSound(int);
void StopSounds(void);
void StopSoundExt(int, int);
void FreeSounds(void);

#endif
