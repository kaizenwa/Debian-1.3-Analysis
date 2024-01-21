// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

/* This file is part of the GMOD package */

#ifndef __structs_h
#define __structs_h

struct note_info
{
  unsigned char note;
  unsigned char sample;
  unsigned char command[2];
  unsigned char parm1[2], parm2[2];
};

struct voice_info
{
  int slide_period;
  int slide_period_goal;
  int slide_rate;

  short pitchbender;
  short finetune;
  short slide_goal;
  short last_rate;		/* last rate of slide */

  short arpeg_note[3];		/* for arpeggio */

  short position;		/* for "pattern-loop" command */
  short pattern;		/* for "pattern-loop" command */
  
  unsigned char sample;
  unsigned char note;

  /* Pitch sliding */

  unsigned char slide_pitch;
  unsigned char slide_dir;
  unsigned char glissando;
  unsigned char last_note;

  signed char volslide;
  unsigned char finevol;

  unsigned char loop_times;	/* for "pattern-loop" command */

  unsigned char cut_count;	/* for "note cut" command */
  unsigned char delay_count;	/* for "note delay" command */

  unsigned char arpeg_num;	/* for arpeggio */
  unsigned char arpeg_curr;	/* for arpeggio */

  unsigned char retrigger;	/* for note retrigger */
  unsigned char retrig_vol;

  unsigned char vibra_rate;	/* vibrato rate */
  unsigned char vibra_old_rate;	/* previous vibrato rate */
  unsigned char vibra_depth;	/* vibrato depth */
  unsigned char vibra_position;	/* vibrato position */
  unsigned char vibra_wave;	/* vibrato waveform */

  unsigned char tremolo;	/* tremolo rate */
  unsigned char tremolo_old;	/* previous tremolo rate */
  unsigned char tremolo_depth;	/* tremolo depth */
  unsigned char tremolo_position;	/* tremolo position */
  unsigned char tremolo_wave;	/* tremolo waveform */

  unsigned char last_info;
  signed char fineslide_adj;
};

struct effect_info
{
  short pattern;		/* position in pattern for jumps, etc */
  short position;		/* position in song for jumps, etc */
  unsigned char delay_notes;	/* for "pattern delay" command */
  unsigned char loop_chan;	/* for "pattern loop" command */
};

struct song_info
{
  short nr_tracks;
  short songlength;
  unsigned int nr_samples;
  unsigned int nr_patterns;
  unsigned char type;
  unsigned char nr_channels;
  unsigned char lowest_note;
  unsigned char highest_note;
  unsigned char play_speed;
  unsigned char tempo;
  unsigned char vol_type;
  unsigned char vol_on_zero;
  unsigned char slide_type;
  unsigned char panning[MAX_TRACK];
  unsigned char global_vol;
  unsigned char clock_speed;
  char name[NAME_LEN];
  char desc[DESC_LEN];
  char *comment;
};

struct options_info
{
  unsigned char main_volume;
#ifndef USE_X
  unsigned char loop_breaker;
#endif
  unsigned char show_empty_samples;
  unsigned char bpm_tempos;
  unsigned char compress;
  unsigned char tolerant;
  unsigned char repeat;
  unsigned char ntsc;
  unsigned char mixer;
  unsigned char extend_oct;
  unsigned char randomize;
  signed char pan_factor;
  unsigned char use_50hz;
};

struct x_struct
{
  char **fileStrings;
  int nrFileStrings;
};

#endif
