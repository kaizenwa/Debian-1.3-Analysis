/* 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/ioctl.h>
#include "util.h"
#include "midievent.h"
#include "controls.h"

void pipe_pass_playing_list(int number_of_files, char *list_of_files[]);
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
static int ctl_blocking_read(int *valp);
static int pipe_read_ready(void);
int pipe_gets(char *str, int maxlen);

/**********************************/
/* export the interface functions */

#define ctl pipe_control_mode

ControlMode ctl= 
{
  "Netscape plug-in interface", 'p',
  FALSE, /* need_args */
  TRUE, /* need_sync */
  FALSE, /* fuzzy_prev */
  -1,0,0,0, /* verb, trace, opened, repeated */
  ctl_open,pipe_pass_playing_list, ctl_close, ctl_read, cmsg,
  ctl_update, ctl_reset, ctl_file_name, ctl_total_time, ctl_current_time, 
  ctl_note, ctl_program,ctl_bank, ctl_volume, 
  ctl_expression, ctl_panning, ctl_sustain, ctl_pitch_bend, ctl_pitch_sense,
  ctl_chorus, ctl_reverb, ctl_change, ctl_main_vol
};

static char curfile[256];

/*================================================================*/

static int ctl_open(int using_stdin, int using_stdout)
{
	ctl.opened = 1;
	return 0;
}

static void ctl_close(void)
{ 
	ctl.opened = 0;
}

static int cmsg(int type, int verbosity_level, char *fmt, ...)
{
  va_list ap;
  if (type==CMSG_TEXT || type==CMSG_INFO || type==CMSG_WARNING)
    return 0;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  return 0;
}

static void ctl_update(MidiInfo *mp) {}

static void ctl_total_time(int tt) {}

static void ctl_file_name(char *name) {}

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

int pipe_gets(char *str, int maxlen)
{
/* blocking reading */
	char *p;
	int len;

	/* at least 5 letters (4+\n) command */
	len = 0;
	for (p = str; ; p++) {
		read(fileno(stdin), p, 1);
		if (*p == '\n')
			break;
		len++;
	}
	*p = 0;
	return len;
}

static int pipe_read_ready()
{
	int num;
	/* see how many chars in buffer. */
	ioctl(fileno(stdin), FIONREAD, &num);
	return num;
}

static int ctl_blocking_read(int *valp)
{
	char buf[256], *tok, *arg;
	int rc;

	rc = pipe_gets(buf, sizeof(buf)-1);
	tok = strtok(buf, " ");

	switch (*tok) {
	case 'L': /* load file: filename is next line */
		return RC_LOAD_FILE;
	case 'Z': /* stop the program */
		return RC_KILL;
	case 'Q': /* stop playing */
		return RC_QUIT;
	case 'R': /* restart playing */
		return RC_PREVIOUS;
	case 'A': /* auto repeat mode */
		if ((arg = strtok(NULL, " ")) != NULL)
			ctl.repeated = atoi(arg);
		return RC_NONE;
	case 'D': /* drum channels */
		if ((arg = strtok(NULL, " ")) != NULL)
			glinfo.drumflag = atoi(arg);
		return RC_NONE;
	case 'f':
		return RC_FORWARD;
	case 'b':
		return RC_BACK;
	case 'S': /* pause until next STOP command */
		return *valp ? RC_CONTINUE : RC_PAUSE;
	}
	return RC_NONE;
}

static int ctl_read(int *valp)
{
	int num;
	if ((num = pipe_read_ready()) == 0)
		return RC_NONE;
	return ctl_blocking_read(valp);
}

void pipe_pass_playing_list(int number_of_files, char *list_of_files[])
{
	int cmd = RC_NONE;
	int val;
	char local[256];
	static MidiInfo minfo;

	while (cmd != RC_KILL) {
		switch (cmd) {
		case RC_LOAD_FILE:
			memcpy(&minfo, &glinfo, sizeof(minfo));
			pipe_gets(local, sizeof(local)-1);
			strcpy(curfile, local);
			minfo.filename = curfile;
			cmd = play_midi_file(&minfo);
			break;
		case RC_REALLY_PREVIOUS:
			cmd = play_midi_file(&minfo);
			break;
		case RC_ERROR:
			return;
		default:
			cmd = ctl_blocking_read(&val);
			break;
		}
	}
}
