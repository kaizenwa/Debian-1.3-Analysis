/*================================================================
 * sfx.h
 *	AWE32 sound driver parameter structure
 *================================================================*/

#ifndef SFX_H_DEF
#define SFX_H_DEF

#ifdef linux
#include <linux/awe_voice.h>
#else
#include <awe_voice.h>
#endif

#define AWE_ID_STR	"ASFX"

struct _VoiceInfo {
	int sfx_version;

	long samplepos, samplesize;

	int nwaves;
	awe_sample_info *sample;
	unsigned short **data;

	int nvoices;
	awe_voice_rec **voice;
};

#ifndef LAYER_H_DEF
typedef struct _VoiceInfo VoiceInfo;
#endif

void sfx_write_header(VoiceInfo *vp, FILE *fp);
int sfx_read_header(VoiceInfo *vp, FILE *fp);
void sfx_write_file(VoiceInfo *vp, FILE *fp);
void sfx_write_file_with_data(VoiceInfo *vp, FILE *fp, char *filename);
void sfx_read_file(VoiceInfo *vp, FILE *fp);



#endif
