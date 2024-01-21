/*================================================================
 * seq.h
 *	sequencer control routine
 *================================================================*/

#ifndef SEQ_H_DEF
#define SEQ_H_DEF

extern int seqfd;
extern int nrsynths;
extern int awe_dev;
extern int max_synth_voices;

#define SEQUENCER_DEV	"/dev/sequencer"

void seq_init(int reset);
void seq_end(void);
void seq_remove_samples(void);
void seq_initialize_chip(void);
void seq_set_gus_bank(int bank);
int seq_load_patch(void *patch, int len);
int seq_mem_avail(void);


#endif
