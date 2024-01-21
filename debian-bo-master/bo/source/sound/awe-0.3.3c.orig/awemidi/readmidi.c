/*----------------------------------------------------------------
 * read midi file and parse events
 *----------------------------------------------------------------
 * Most of this code is derived from TiMidity (MIDI to WAVE
 * converter) by Tuukka Toivonen.
 *----------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include "midievent.h"
#include "util.h"
#include "controls.h"
#include "channel.h"

typedef struct {
	MidiEvent event;
	void *next;
} MidiEventList;

/*==*/

static MidiEventList *evlist;
static int event_count;
static int at;
static int check_gs_macro;
static unsigned long drumflag;

/* not use fseek due to piped input */
void skip(FILE *fp, size_t len)
{
  size_t c;
  char tmp[1024];
  while (len>0)
    {
      c=len;
      if (c>1024) c=1024;
      len-=c;
      if (c!=fread(tmp, 1, c, fp)) {
	      if (ctl)
		      ctl->cmsg(CMSG_ERROR, -1, "Error in skip file..");
      }
    }
}

/*#define SKIP(fp,len)	fseek(fp, len, SEEK_CUR)*/
#define SKIP(fp,len)	skip(fp, len)

/* Read variable-length number (7 bits per byte, MSB first) */
static int getvl(FILE *fp)
{
	int l = 0;
	unsigned char c;
	for (;;) {
		if (fread(&c, 1, 1, fp) != 1) {
			if (ctl)
				ctl->cmsg(CMSG_ERROR, -1, "Corrupt MIDI file");
			return -1;
		}
		l += (c & 0x7f);
		if (!(c & 0x80))
			return l;
		l <<= 7;
	}
}

#define MIDIEVENT(at,t,ch,pa,pb) \
  new=safe_malloc(sizeof(MidiEventList)); \
  new->event.time=at; \
  new->event.type=t; \
  new->event.channel=(unsigned char)ch; \
  new->event.a=(unsigned char)pa; \
  new->event.b=(unsigned char)pb; \
  new->event.misc=0; \
  new->next=0;\
  return new;

#define MAGIC_EOT ((MidiEventList *)(-1))

static unsigned char laststatus, lastchan;
static unsigned char nrpn=0;
static unsigned char rpn_msb[MAX_MIDI_CHANNELS], rpn_lsb[MAX_MIDI_CHANNELS]; /* one per channel */
static unsigned char nrpn_msb[MAX_MIDI_CHANNELS], nrpn_lsb[MAX_MIDI_CHANNELS]; /* one per channel */

/*
 * output message
 */
static MidiEventList *print_message(int type, int len, FILE *fp)
{
	static char *label[]={
		"Text event: ", "Text: ", "Copyright: ", "Track name: ",
		"Instrument: ", "Lyric: ", "Marker: ", "Cue point: ",
	};
	char *s;
	MidiEventList *new;
	int slen;

	if (type < 0 || type > 7) type = 0;
	slen = strlen(label[type]);
	s = (char*)safe_malloc(len + slen + 1);
	strcpy(s, label[type]);
	fread(s + slen, 1, len, fp);
	s[len + slen] = 0;

	new = safe_malloc(sizeof(MidiEventList));
	new->event.time = at;
	new->event.type = ME_LYRIC;
	new->event.misc = s;
	new->next = 0;
	return new;
}

static MidiEventList *nrpn_event(FILE *fp)
{
	static int event_type[] = {
		ME_LFO1_DELAY, ME_LFO1_FREQ, ME_LFO2_DELAY, ME_LFO2_FREQ,
		ME_ENV1_DELAY, ME_ENV1_ATTACK, ME_ENV1_HOLD, ME_ENV1_DECAY,
		ME_ENV1_SUSTAIN, ME_ENV1_RELEASE, ME_ENV2_DELAY, ME_ENV2_ATTACK,
		ME_ENV2_HOLD, ME_ENV2_DECAY, ME_ENV2_SUSTAIN, ME_ENV2_RELEASE,
		ME_INIT_PITCH, ME_LFO1_PITCH, ME_LFO2_PITCH, ME_ENV1_PITCH,
		ME_LFO1_VOLUME, ME_CUTOFF, ME_FILTERQ, ME_LFO1_CUTOFF,
		ME_ENV1_CUTOFF, ME_FX_CHORUS, ME_FX_REVERB,
	};
	MidiEventList *new;
	if (rpn_msb[lastchan] == 127 && rpn_lsb[lastchan] <= 26) {
		MIDIEVENT(at, event_type[rpn_lsb[lastchan]], lastchan,
			  nrpn_lsb[lastchan], nrpn_msb[lastchan]);
	}
	return NULL;
}

/*
 * parse system exclusive message
 * only support AWE32 chorus and reverb mode control
 */
static MidiEventList *system_exclusive(FILE *fp)
{
#if 0
	static unsigned char master_volume_macro[] = {
		0x7f,0x7f,0x04,0x01,/*ll,mm,*/
	};
	static unsigned char gm_on_macro[] = {
		0x7e,0x7f,0x09,0x01,
	};
	static unsigned char gs_on_sfx[] = {
		0x00,0x7f,0x00,0x41,
	}
	static unsigned char xg_on_macro[] = {
		0x43,0x10,0x4c,0x00,0x00,0x7e,0x00,
	};
	static unsigned char xg_main_drum_macro[] = {
		0x43,0x10,0x4c,0x08,0/*channel*/,0x07,/*1-3,*/
		/* 1=optional, 2=main, 3=sub */
	};
#endif
	static unsigned char gs_pfx_macro[] = {
		0x41,0x10,0x42,0x12,0x40,
	};
	static unsigned char awe_reverb_macro[] = {
		0x41,0x10,0x42,0x12,0x40,0x01,0x30,
	};
	static unsigned char awe_chorus_macro[] = {
		0x41,0x10,0x42,0x12,0x40,0x01,0x38,
	};
	unsigned char buf[1024];
	MidiEventList *new;
	int len;
	int i; char tmp[256];

	if ((len = getvl(fp)) < 0) return NULL;
	if (len > sizeof(buf)) {
		if (ctl)
			ctl->cmsg(CMSG_ERROR, -1, "too long sysex!");
		SKIP(fp, len);
		return NULL;
	}

	fread(buf, 1, len, fp);
	if (memcmp(buf, awe_reverb_macro, sizeof(awe_reverb_macro)) == 0) {
		if (ctl) ctl->cmsg(CMSG_INFO, 0, "SYSEX: reverb = %d", buf[7]);
		MIDIEVENT(at, ME_SET_REVERB_MODE, 0, buf[7], 0);
	}

	if (memcmp(buf, awe_chorus_macro, sizeof(awe_chorus_macro)) == 0) {
		if (ctl) ctl->cmsg(CMSG_INFO, 0, "SYSEX: chorus = %d", buf[7]);
		MIDIEVENT(at, ME_SET_CHORUS_MODE, 0, buf[7], 0);
	}

	if (check_gs_macro &&
	    memcmp(buf, gs_pfx_macro, sizeof(gs_pfx_macro)) == 0) {
		if ((buf[5] & 0xf0) == 0x10 && buf[6] == 0x15) {
			/* GS part mode change */
			int p = buf[5] & 0x0f;
			if (p == 0) p = 9;
			else if (p < 9) p--;
			if (buf[7] == 0)
				drumflag &= ~(1 << p);
			else
				drumflag |= (1 << p);
			if (ctl) ctl->cmsg(CMSG_INFO, 0, "SYSEX: drum part %d %s",
					   p, (buf[7] ? "on" : "off"));
		}
	}

	strcpy(tmp, "SYSEX:");
	for (i = 0; i < len; i++) {
		sprintf(tmp+i*3+6," %02x", buf[i]);
		if (i >= 50) {
			sprintf(tmp+(i+1)*3+6,"...");
			break;
		}
	}
	if (ctl)
		ctl->cmsg(CMSG_INFO, 2, tmp);
	
	return NULL;
}

/*
 * parse meta event
 * support only "end of track" and "tempo"
 */
static MidiEventList *meta_event(FILE *fp)
{
	MidiEventList *new;
	unsigned char type;
	unsigned char a, b, c;
	int len;

	fread(&type, 1, 1,fp);
	if ((len = getvl(fp)) < 0) return NULL;
	if (type > 0 && type < 16) {
		return print_message(type, len, fp);
	} else {
		switch(type) {
		case 0x2F: /* End of Track */
			return MAGIC_EOT;
			
		case 0x51: /* Tempo */
			fread(&a,1,1,fp);
			fread(&b,1,1,fp);
			fread(&c,1,1,fp);
			MIDIEVENT(at, ME_TEMPO, c, a, b);
		
		default:
		/*Warning("unknown meta event: type%d, len%d", type, len);*/
			SKIP(fp, len);
			break;
		}
	}
	return NULL;
}


/*
 * parse control change message
 */
static MidiEventList *control_change(FILE *fp, unsigned char type, unsigned char val)
{
	MidiEventList *new;
	unsigned char control;

	control=255;
	switch (type) {
	case 7: control=ME_MAINVOLUME; break;
	case 10: control=ME_PAN; break;
	case 11: control=ME_EXPRESSION; break;
	case 64: control=ME_SUSTAIN; break;
	case 120: control=ME_ALL_SOUNDS_OFF; break;
	case 121: control=ME_RESET_CONTROLLERS; break;
	case 123: control=ME_ALL_NOTES_OFF; break;
		
	case 91: control=ME_REVERB; break;
	case 93: control=ME_CHORUS; break;

	/* the SCC-1 tone bank switch commands. */
	case 0: control=ME_TONE_BANK; break;
	case 32: control=ME_TONE_BANK; break;
						
	case 100: nrpn=0; rpn_lsb[lastchan]=val; break;
	case 101: nrpn=0; rpn_msb[lastchan]=val; break;
	case 99: nrpn=1; rpn_msb[lastchan]=val; break;
	case 98: nrpn=1; rpn_lsb[lastchan]=val; break;
		
	case 6: /* send data entry MSB */
		if (nrpn) {
			nrpn_msb[lastchan] = val;
			break;
		}
						
		switch((rpn_msb[lastchan]<<8) | rpn_lsb[lastchan]) {
		case 0x0000:
			/* Pitch bend sensitivity */
			control=ME_PITCH_SENS;
			break;
							
		case 0x7F7F: /* RPN reset */
			control=ME_PITCH_SENS;
			val = 2;
			break;
		}
		break;

	case 38: /* data entry LSB */
		if (!nrpn)
			break;
		nrpn_lsb[lastchan] = val;
		return nrpn_event(fp);
	}
	if (control != 255) { 
		MIDIEVENT(at, control, lastchan, val, 0); 
	}
	return NULL;
}

/* Read a MIDI event, returning a freshly allocated element that can
   be linked to the event list */
static MidiEventList *read_midi_event(FILE *fp)
{
	unsigned char me, a, b;
	MidiEventList *new;

	for (;;) {
		int len;
		if ((len = getvl(fp)) < 0)
			return NULL;
		at += len;
		if (fread(&me, 1, 1, fp) != 1) {
			if (ctl)
				ctl->cmsg(CMSG_ERROR, -1, "error in reading midi file");
			return NULL;
		}

		if (me == 0xF0 || me == 0xF7) {
			/* SysEx event */
			if ((new = system_exclusive(fp)) != NULL)
				return new;
		} else if (me == 0xFF) {
			/* Meta event */
			if ((new = meta_event(fp)) != NULL)
				return new;
		} else {
			a = me;
			if (a & 0x80) { /* status byte */
				lastchan = a & 0x0F;
				laststatus = (a>>4) & 0x07;
				fread(&a, 1,1, fp);
				a &= 0x7F;
			}
			switch (laststatus) {
			case 0: /* Note off */
				fread(&b, 1,1, fp);
				b &= 0x7F;
				MIDIEVENT(at, ME_NOTEOFF, lastchan, a, b);

			case 1: /* Note on */
				fread(&b, 1,1, fp);
				b &= 0x7F;
				MIDIEVENT(at, ME_NOTEON, lastchan, a, b);

			case 2: /* Key Pressure */
				fread(&b, 1,1, fp);
				b &= 0x7F;
				MIDIEVENT(at, ME_KEYPRESSURE, lastchan, a, b);

			case 3: /* Control change */
				fread(&b, 1,1, fp);
				b &= 0x7F;
				if ((new = control_change(fp, a, b)) != NULL)
					return new;
				break;
				
			case 4: /* Program change */
				a &= 0x7f;
				MIDIEVENT(at, ME_PROGRAM, lastchan, a, 0);
				
			case 5: /* Channel pressure - NOT IMPLEMENTED */
				break;
				
			case 6: /* Pitch wheel */
				fread(&b, 1,1, fp);
				b &= 0x7F;
				MIDIEVENT(at, ME_PITCHWHEEL, lastchan, a, b);
				
			default: 
				if (ctl)
					ctl->cmsg(CMSG_WARNING, 0, "unknown status 0x%02X, channel 0x%02X",
					  laststatus, lastchan);
				break;
			}
		}
	}
  
	return new;
}

#undef MIDIEVENT


#define READID(str,fp)	fread(str,1,4,fp)

/* Instrument files are little-endian, MIDI files big-endian, so we
   need to do some conversions. */

#define XCHG_SHORT(x) ((((x)&0xFF)<<8) | (((x)>>8)&0xFF))
#ifdef __i486__
# define XCHG_LONG(x) \
     ({ int __value; \
        asm ("bswap %1; movl %1,%0" : "=g" (__value) : "r" (x)); \
       __value; })
#else
# define XCHG_LONG(x) ((((x)&0xFF)<<24) | \
		      (((x)&0xFF00)<<8) | \
		      (((x)&0xFF0000)>>8) | \
		      (((x)>>24)&0xFF))
#endif

/*#if ENDIAN==LITTLE_ENDIAN*/
#define BE_LONG(x)	XCHG_LONG(x)
#define BE_SHORT(x)	XCHG_SHORT(x)
/*
#else
#define BE_LONG(x)	(x)
#define BE_SHORT(x)	(x)
#endif
*/

static long READLONG(FILE *fp)
{
	long v;
	fread(&v, 4, 1, fp);
	return BE_LONG(v);
}

static short READSHORT(FILE *fp)
{
	short v;
	fread(&v, 2, 1, fp);
	return BE_SHORT(v);
}		


/* Read a midi track into the linked list, either merging with any previous
   tracks or appending to them. */
static int read_track(FILE *fp, int append, int shifted)
{
	MidiEventList *meep;
	MidiEventList *next, *new;
	int len;
	char tmp[4];

	meep=evlist;
	if (append && meep) {
		/* find the last event in the list */
		for (; meep->next; meep=meep->next)
			;
		at=meep->event.time;
	} else
		at=0;

	/* Check the formalities */
	READID(tmp, fp);
	len = READLONG(fp);
	if (memcmp(tmp, "MTrk", 4)) {
		if (ctl)
			ctl->cmsg(CMSG_ERROR, -1, "Corrupt MIDI file.");
		return -2;
	}

	for (;;) {
		new = read_midi_event(fp);
		if (new == NULL)
			return -2;
		else if (new == MAGIC_EOT) /* End-of-track Hack. */
			return 0;

		if (shifted) {
			if (new->event.type != ME_TEMPO &&
			    new->event.type != ME_LYRIC)
				new->event.channel += shifted;
		}
				
		next = meep->next;
		while (next && (next->event.time < new->event.time)) {
			meep=next;
			next=meep->next;
		}
	  
		new->next = next;
		meep->next = new;

		event_count++; /* Count the event. (About one?) */
		meep = new;
	}
}

/* Free the linked event list from memory. */
static void free_midi_list(void)
{
	MidiEventList *meep, *next;
	for (meep = evlist; meep; meep = next) {
		next = meep->next;
		safe_free(meep);
		meep = next;
	}
	evlist=0;
}


static void ConvertMidiList(MidiInfo *header)
{
	MidiEventList *meep;
	MidiEvent *evdata;
	int tempo;
	int prevt;
	int i;
	double curt;

	evdata = (MidiEvent*)safe_malloc(sizeof(MidiEvent) * event_count);
	header->nlists = event_count;
	header->list = evdata;
	header->tempo = MIDI_DEFAULT_TEMPO;

	meep = evlist->next;
	tempo = MIDI_DEFAULT_TEMPO;
	prevt = 0;
	curt = 0;
	i = 0;
	for (meep = evlist->next; meep; meep = meep->next) {
		/*double dtime = (double)(meep->event.time - prevt) *
			(double)tempo / 10000.0 / header->divisions;*/
		double dtime = (double)(meep->event.time - prevt) *
			(tempo / 10000) / header->divisions;
		curt += dtime;
		prevt = meep->event.time;
		if (meep->event.type == ME_TEMPO) {
			tempo = MidiTempo(&meep->event);
		}
		memcpy(evdata + i, &meep->event, sizeof(MidiEvent));
		evdata[i].csec = (int)curt;
		i++;
		if (i >= event_count)
			break;
	}
}


MidiEvent *ReadMidiFile(FILE *fp, MidiInfo *header)
{
	int len, divisions;
	short format, tracks, divisions_tmp;
	int i, shift;
	char tmp[4];

	event_count=0;
	at=0;
	evlist=0;

	drumflag = header->drumflag;
	check_gs_macro = header->check_gs_macro;
	READID(tmp, fp);
	len = READLONG(fp);
	if (memcmp(tmp, "MThd", 4) || len < 6) {
		if (ctl)
			ctl->cmsg(CMSG_ERROR, -1, "not a MIDI file");
		return 0;
	}

	format = READSHORT(fp);
	tracks = READSHORT(fp);
	divisions_tmp = READSHORT(fp);
	if (divisions_tmp<0) {
		divisions = -(divisions_tmp/256) * (divisions_tmp & 0xFF);
	}
	else
		divisions = divisions_tmp;

	if (len > 6) {
		SKIP(fp, len-6); /* skip the excess */
	}
	if (format < 0 || format >2) {
		if (ctl)
			ctl->cmsg(CMSG_ERROR, -1, "unknown MIDI file format %d", format);
		return 0;
	}

	header->format = format;
	header->tracks = tracks;
	header->divisions = divisions;

	if (header->track_nums >= 0 &&
	    header->tracks > header->track_nums + 1)
		header->multi_part = 1;

	if (ctl)
		ctl->cmsg(CMSG_INFO, 1, "format=%d, tracks=%d, divs=%d",
			format, tracks, divisions);
	/* Put a do-nothing event first in the list for easier processing */
	evlist = (MidiEventList*)safe_malloc(sizeof(MidiEventList));
	evlist->event.time = 0;
	evlist->event.type = ME_NONE;
	evlist->next = 0;

	switch(format) {
	case 0:
		if (read_track(fp, 0, 0)) {
			free_midi_list();
			return 0;
		}
		break;

	case 1:
		shift = 0;
		for (i=0; i < tracks; i++) {
			/* normally track 0 is used for controls */
			/* track 1-16 for channel 1-16 */
			/* track 17-32 for channel 17-32 */
			if (header->track_nums >= 0 &&
			    i > header->track_nums) shift = 16;
			if (read_track(fp, 0, shift)) {
				free_midi_list();
				return 0;
			}
		}
		break;

	case 2: /* We simply play the tracks sequentially */
		shift = 0;
		for (i=0; i < tracks; i++) {
			if (header->track_nums >= 0 &&
			    i > header->track_nums) shift = 16;
			if (read_track(fp, 1, 0)) {
				free_midi_list();
				return 0;
			}
		}
		break;
	}

	ConvertMidiList(header);
	free_midi_list();

	header->drumflag = drumflag;
	return header->list;
}


void FreeMidiFile(MidiInfo *header)
{
	int i;
	for (i = 0; i < header->nlists; i++) {
		if (header->list[i].misc)
			safe_free(header->list[i].misc);
	}
	safe_free(header->list);
}
