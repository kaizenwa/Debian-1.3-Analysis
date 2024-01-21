/*
  controls.h
     This file is brought from TiMidity 0.2i, and modified for tkawe.
*/

/* Return values for ControlMode.read */

#ifndef CONTROLS_H_DEF

#define RC_ERROR -1
#define RC_NONE 0
#define RC_QUIT 1
#define RC_NEXT 2
#define RC_PREVIOUS 3 /* Restart this song at beginning, or the previous
			 song if we're less than a second into this one. */
#define RC_FORWARD 4
#define RC_BACK 5
#define RC_JUMP 6
#define RC_TOGGLE_PAUSE 7 /* Pause/continue */
#define RC_RESTART 8 /* Restart song at beginning */

#define RC_PAUSE 9 /* Really pause playing */
#define RC_CONTINUE 10 /* Continue if paused */
#define RC_REALLY_PREVIOUS 11 /* Really go to the previous song */
#define RC_CHANGE_VOLUME 12
#define RC_LOAD_FILE 13		/* Load a new midifile */
#define RC_TUNE_END 14		/* The tune is over, play it again sam? */
#define RC_CHANGE_CHORUS 15
#define RC_CHANGE_REVERB 16
#define RC_RESUME 17

#define RC_CHANGE_BASS 18
#define RC_CHANGE_TREBLE 19

#define RC_KILL 99

#define CMSG_INFO	0
#define CMSG_WARNING	1
#define CMSG_ERROR	2
#define CMSG_FATAL	3
#define CMSG_TRACE	4
#define CMSG_TIME	5
#define CMSG_TOTAL	6
#define CMSG_FILE	7
#define CMSG_TEXT	8

#define VERB_NORMAL	0
#define VERB_VERBOSE	1
#define VERB_NOISY	2
#define VERB_DEBUG	3
#define VERB_DEBUG_SILLY	4

#define PLAY_REPEAT	1
#define PLAY_SHUFFLE	2
#define PLAY_AUTOSTART	4
#define PLAY_AUTOEXIT	8
#define PLAY_TRACE	16
#define PLAY_NORMAL	32

typedef struct {
  char *id_name, id_character;
  int need_args, need_sync, fuzzy_prev;
  int verbosity, playing_mode, opened, repeated;

  int (*open)(int using_stdin, int using_stdout);
  void (*pass_playing_list)(int number_of_files, char *list_of_files[]);
  void (*close)(void);
  int (*read)(int *valp);
  int (*cmsg)(int type, int verbosity_level, char *fmt, ...);

  void (*update)(MidiInfo *mp);
  void (*reset)(MidiInfo *mp);
  void (*file_name)(char *name);
  void (*total_time)(int tt);
  void (*current_time)(int ct);

  void (*note)(int ch, int note, int vel);
  void (*program)(int channel, int val); /* val<0 means drum set -val */
  void (*bank)(int channel, int val);
  void (*volume)(int channel, int val);
  void (*expression)(int channel, int val);
  void (*panning)(int channel, int val);
  void (*sustain)(int channel, int val);
  void (*pitch_bend)(int channel, int val);
  void (*pitch_sense)(int channel, int val);
  
  void (*chorus_mode)(int mode);
  void (*reverb_mode)(int mode);	
  void (*effects_change)(int type, int channel, int value);

  void (*master_volume)(int atten);
} ControlMode;

extern ControlMode *ctl_list[], *ctl; 

#endif
