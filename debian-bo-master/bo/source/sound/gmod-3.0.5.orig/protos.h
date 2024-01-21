// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

/* This file is part of the GMOD package */

#ifndef __protos_h
#define __protos_h

#ifndef _STDIO_H
#include <stdio.h>
#endif

/* in compress.c */
short compress_voice (short last_voice, short position, int notes_per_track,
		      int skip, short first_voice = 0);

/* in cvt_period.c */
void period_to_note (int period, int *note, int *pitchbend);

/* in effects.c */
void set_speed (int parm, unsigned char clock_speed);
void set_volslide (int channel, int amount, char type);
void set_slideto (int channel, int rate, int note, unsigned char type,
		  unsigned char);
void set_arpeg (int channel, int amount);
void set_vibrato (int channel, int amount);

/* in gmod.c */
int start_playback (unsigned char);

/* in init.c */
void init_voices (void);

/* int load_669.c */
int load_669_module (FILE * mod_fd, struct song_info *song_char,
		     struct options_info options, unsigned char *buffer);

/* in load_mod.c */
int load_module (char *name, struct song_info *song_char,
		 struct options_info options);

/* in load_mtm.c */
int load_mtm_module (FILE * mod_fd, struct song_info *song_char, 
		     unsigned char *buffer);

/* in load_s3m.c */
int load_s3m_module (FILE ** mod_fd, struct song_info *song_char,
		     struct options_info options, unsigned char *buffer,
		     char *command);

/* in load_ult.c */
int load_ult_module (FILE * mod_fd, struct song_info *song_char,
		     struct options_info options, unsigned char *buffer);

/* in load_xm.c */
int load_xm_module (FILE ** mod_fd, struct song_info *song_char,
		    struct options_info options, unsigned char *buffer,
		    char *command);

/* in misc.c */
int panning (int ch);
void sync_time (void);
void free_patterns();
void stop_all_channels (unsigned char song_end);
void remove_noprint (char *string);

/* in parse.c */
int parse_args (int argc, char *argv[], struct options_info *options);

/* in patch_load.c */
/*
void patch_load (FILE * mod_fd, struct patch_info *patch, int sample_num,
		 int patch_type, Sample *);
		 */

/* in play_mod.c */
void play_module (int start_position, struct song_info *song_char,
		 struct options_info options, int start_delay);
int play_next_position (void);
int end_module (unsigned char);

/* in play_note.c */
int play_note (int channel, int position, int pattern, struct note_info *pat,
	       struct song_info *song_char, struct effect_info *effects,
	       struct options_info *options);

/* in play_voice.c */
void lets_play_voice (int channel, struct voice_info *v,
		      struct song_info *song_char, int tick_no);

#ifndef USE_X
/* in proc_event.c */
void NoXProcessEvent (void);
#endif

/* in proc_input.c */
unsigned int proc_input (void);

#ifndef USE_X
/* in signals.c */
void timer_set (int mode);
void timer_handler (int sig);

/* in terminal.c */
void terminal_set (int mode);
#endif
#endif
