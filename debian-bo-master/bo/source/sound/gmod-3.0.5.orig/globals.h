// -*-C++-*-
// This file is part of the gmod package
// Copyright (C) 1997 by Andrew J. Robinson

/* This file is part of the GMOD package */

#ifndef __globals_h
#define __globals_h

extern int pattern_len[MAX_POSITION];
extern int pattern_tempo[MAX_POSITION];
extern struct note_info *pattern_table[MAX_PATTERN * MAX_TRACK];

extern struct voice_info voices[MAX_TRACK];

extern int tune[MAX_POSITION];
extern short voice_table[MAX_POSITION][MAX_TRACK];
extern double tick_duration;

//extern int seqfd;
extern int  mixerfd;
extern unsigned char sample_ok[MAX_SAMPLES] /*, sample_vol[MAX_SAMPLES] */ ;
/* extern unsigned long sample_speed[MAX_SAMPLES]; */
/* extern short sample_ft[MAX_SAMPLES]; */
extern int tmp, gus_dev;
extern double this_time, next_time;
extern int ticks_per_division;
extern double clock_rate;	/* HZ */

extern unsigned char _seqbuf[];
extern int _seqbuflen, _seqbufptr;

extern unsigned short base_freq_table[];
extern unsigned short period_table[];
extern short vibra_table[][NUM_VIBRA];

#ifndef USE_X
extern unsigned char stop_flag;
#endif
extern unsigned char background;
extern int actual_pos;
extern int position_change;

extern unsigned int seq_input;

#ifdef USE_X
extern struct x_struct x_info;
extern int current_mod;
#endif
#endif
