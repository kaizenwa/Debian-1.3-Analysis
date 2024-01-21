/* 
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include "util.h"
#include "midievent.h"
#include "controls.h"

void dumb_pass_playing_list(int number_of_files, char *list_of_files[]);
static void ctl_update(MidiInfo *mp);
static void ctl_total_time(int tt);
static void ctl_file_name(char *name);
static void ctl_current_time(int ct);
static void ctl_note(int ch, int note, int vel);
static void ctl_program(int ch, int val);
static void ctl_bank(int ch, int val);
static void ctl_volume(int channel, int val);
static void ctl_expression(int channel, int val);
static void ctl_panning(int channel, int val);
static void ctl_sustain(int channel, int val);
static void ctl_pitch_bend(int channel, int val);
static void ctl_pitch_sense(int channel, int val);
static void ctl_reset(MidiInfo *mp);
static int ctl_open(int using_stdin, int using_stdout);
static void ctl_close(void);
static int ctl_read(int *valp);
static int cmsg(int type, int verbosity_level, char *fmt, ...);
static void ctl_chorus(int mode);
static void ctl_reverb(int mode);
static void ctl_change(int type, int ch, int mode);
static void ctl_main_vol(int atten);

/**********************************/
/* export the interface functions */

#define ctl dumb_control_mode

ControlMode ctl= 
{
  "dumb interface", 'd',
  TRUE, /* need_args */
  FALSE, /* need_sync */
  FALSE, /* fuzzy_prev */
  0,0,0,0, /* verbose, trace, open, repeat */
  ctl_open,dumb_pass_playing_list, ctl_close, ctl_read, cmsg,
  ctl_update, ctl_reset, ctl_file_name, ctl_total_time, ctl_current_time, 
  ctl_note, ctl_program,ctl_bank, ctl_volume, 
  ctl_expression, ctl_panning, ctl_sustain, ctl_pitch_bend, ctl_pitch_sense,
  ctl_chorus, ctl_reverb, ctl_change, ctl_main_vol
};

static FILE *infp=stdin, *outfp=stdout; /* infp isn't actually used yet */

static int ctl_open(int using_stdin, int using_stdout)
{
  if (using_stdin && using_stdout)
    infp=outfp=stderr;
  else if (using_stdout)
    outfp=stderr;
  else if (using_stdin)
    infp=stdout;

  ctl.opened=1;
  return 0;
}

static void ctl_close(void)
{ 
  fflush(outfp);
  ctl.opened=0;
}

static int ctl_read(int *valp)
{
  return RC_NONE;
}

static int cmsg(int type, int verbosity_level, char *fmt, ...)
{
  va_list ap;
  switch (type) {
  case CMSG_INFO:
  case CMSG_TEXT:
    if (ctl.verbosity == 1 && verbosity_level == -1)
      return 0;
    if (ctl.verbosity < verbosity_level + 3)
      return 0;
    break;
  default:
    if (ctl.verbosity < verbosity_level) return 0;
    break;
  }
  va_start(ap, fmt);
  if (!ctl.opened)
    {
      vfprintf(stderr, fmt, ap);
      fprintf(stderr, "\n");
    }
  else
    {
      vfprintf(outfp, fmt, ap);
      fprintf(outfp, "\n");
    }
  va_end(ap);
  return 0;
}

static void ctl_update(MidiInfo *mp) {}

static void ctl_total_time(int tt)
{
  tt /= 100;
  if (ctl.verbosity > 0)
    fprintf(outfp, "Total Time %d:%02d\n", tt/60, tt%60);
}

static void ctl_file_name(char *name)
{
  if (ctl.verbosity > 0)
    fprintf(outfp, "Playing %s\n", name);
}

static void ctl_current_time(int ct) {}

static void ctl_note(int ch, int note, int vel) {}

static void ctl_program(int ch, int val) {}

static void ctl_bank(int ch, int bank) {}

static void ctl_volume(int channel, int val) {}

static void ctl_expression(int channel, int val) {}

static void ctl_panning(int channel, int val) {}

static void ctl_sustain(int channel, int val) {}

static void ctl_pitch_bend(int channel, int val) {}

static void ctl_pitch_sense(int channel, int val) {}

static void ctl_reset(MidiInfo *mp) {}

static void ctl_chorus(int mode) {}

static void ctl_reverb(int mode) {}

static void ctl_change(int type, int channel, int mode) {}

static void ctl_main_vol(int atten) {}

#undef ctl

void dumb_pass_playing_list(int number_of_files, char *list_of_files[])
{
    int i;
    static MidiInfo minfo;

    srandom(getpid());
    if (ctl->playing_mode & PLAY_SHUFFLE) {
	for (i = 0; i < number_of_files; i++) {
	    int j = random() % (number_of_files - i);
	    if (j) {
		char *tmp = list_of_files[i + j];
		list_of_files[i] = list_of_files[i + j];
		list_of_files[i + j] = tmp;
	    }
	}
    }

    i = 0;
    for (;;)
	{
	  memcpy(&minfo, &glinfo, sizeof(minfo));
          minfo.filename = list_of_files[i];
	  switch(play_midi_file(&minfo))
	    {
	    case RC_REALLY_PREVIOUS:
		if (i>0)
		    i--;
		break;
			
	    default: /* An error or something */
	    case RC_NEXT:
		if (i<number_of_files-1)
		    {
			i++;
			break;
		    }
		else if (ctl->playing_mode & PLAY_REPEAT)
		    i = 0;
		/* else fall through */
		
	    case RC_QUIT:
	    case RC_KILL:
		ctl_close();
		return;
	    }
	}
}
