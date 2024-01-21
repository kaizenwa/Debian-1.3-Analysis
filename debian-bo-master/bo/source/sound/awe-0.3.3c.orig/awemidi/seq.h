/*
 */

#ifndef SEQ_H_DEF
#define SEQ_H_DEF

#include "midievent.h"

extern void seqbuf_dump();

int seq_init(MidiInfo *mp);
void seq_end(void);
void seq_clear(int csec);
int seq_curtime(void);
void seq_wait(int time);
void seq_wait_abs(int time);
void seq_sync(int cs);
void seq_set_bank(int v, int bank);
void seq_set_program(int v, int pgm);
void seq_start_note(int v, int note, int vel);
void seq_stop_note(int v, int note, int vel);
void seq_terminate_note(int v);
void seq_terminate_all(void);
void seq_stop_all(void);
void seq_aftertouch(int v, int note, int vel);
void seq_pitchsense(int v, int val);
void seq_pitchbend(int v, int val);
void seq_panning(int v, int val);
void seq_expression(int v, int val);
void seq_mainvolume(int v, int val);
void seq_set_chorus(int mode);
void seq_set_reverb(int mode);
void seq_send_effect(int v, int type, int val);
void seq_reset_control(void);
void seq_note_off_all(void);
void seq_sound_off_all(void);
void seq_sustain(int v, int val);
void seq_send_effect(int v, int type, int val);
void seq_change_volume(int vol);
void seq_set_exclusive(int mode);
void seq_set_drumchannels(int channels);
int seq_get_sf_id(void);
void seq_equalizer(int bass, int treble);

#endif
