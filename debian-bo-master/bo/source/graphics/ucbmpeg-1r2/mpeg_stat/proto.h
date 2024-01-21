/* MPEGSTAT - analyzing tool for MPEG-I video streams
 * 
 *  Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 *
 * Technical University of Berlin, Germany, Dept. of Computer Science
 * Tom Pfeifer - Multimedia systems project - pfeifer@fokus.gmd.de
 *
 * Jens Brettin, Harald Masche, Alexander Schulze, Dirk Schubert
 *
 * This program uses parts of the source code of the Berkeley MPEG player
 *
 * ---------------------------
 *
 * Copyright (c) 1993 Technical University of Berlin, Germany
 *
 * for the parts of the Berkeley player used:
 *
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * ---------------------------
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notices and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA 
 * or the Technical University of Berlin BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA or the Technical University of Berlin HAS BEEN ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA and the Technical University of Berlin 
 * SPECIFICALLY DISCLAIM ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE 
 * UNIVERSITY OF CALIFORNIA and the Technical University of Berlin HAVE NO 
 * OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, 
 * OR MODIFICATIONS.
 */
#ifdef __STDC__
# define	P(s) s
#else
# define P(s) ()
#endif


/* util.c */
void correct_underflow P((void ));
int next_bits P((int num , unsigned int mask ));
char *get_ext_data P((int *size ));
int next_start_code P((void ));
char *get_extra_bit_info P((void ));

/* video.c */
void init_stats P((void ));
void PrintAllStats P((void ));
double ReadSysClock P((void ));
void PrintTimeInfo P((void ));
VidStream *NewVidStream P((int bufLength ));
void DestroyVidStream P((VidStream *astream ));
PictImage *NewPictImage P((unsigned int width , unsigned int height ));
void DestroyPictImage P((PictImage *apictimage ));
VidStream *mpegVidRsrc P((TimeStamp time_stamp , VidStream *vid_stream ));

/* parseblock.c */
int ParseReconBlock P((int n ));
int ParseAwayBlock P((int n ));

/* motionvector.c */
void ComputeForwVector P((int *recon_right_for_ptr , int *recon_down_for_ptr ));
void ComputeBackVector P((int *recon_right_back_ptr , int *recon_down_back_ptr ));

/* decoders.c */
void init_tables P((void ));
void decodeDCTDCSizeLum P((unsigned int *value ));
void decodeDCTDCSizeChrom P((unsigned int *value ));
int decodeDCTCoeffFirst P((unsigned int *run , int *level ));
int decodeDCTCoeffNext P((unsigned int *run , int *level ));

/* main.c */
#ifndef SIG_ONE_PARAM
void int_handler P((void ));
#else
void int_handler P((int signum ));
#endif
void main P((int argc , char **argv ));
void Usage P((void ));
void DoDitherImage P((unsigned char *l , unsigned char *Cr , unsigned char *Cb , unsigned char *disp , int h , int w ));

/* jrevdct.c */
void init_pre_idct P((void ));
void j_rev_dct_sparse P((DCTBLOCK data , int pos ));
void j_rev_dct P((DCTBLOCK data ));
void j_rev_dct_sparse P((DCTBLOCK data , int pos ));
void j_rev_dct P((DCTBLOCK data ));


/* readfile.c */
int get_more_data P((unsigned int **buf_start , int *max_length , int *length_ptr , unsigned int **buf_ptr ));
void init_read_sys P((void ));
int pure_get_more_data P((unsigned int *buf_start , int max_length , int *length_ptr , unsigned int **buf_ptr, int swap ));
int read_sys P((unsigned int **buf_start , int *max_length , int *length_ptr , unsigned int **buf_ptr, unsigned int start ));
int ReadStartCode P((
   unsigned int *startCode));

int ReadPackHeader P((double *systemClockTime,unsigned long *muxRate));

int ReadSystemHeader P((void ));

int ReadPacket P((unsigned char packetID,unsigned int **bs_ptr, int *max_length, int *length_ptr , unsigned int **buf_ptr ));

void ReadTimeStamp P((unsigned char *inputBuffer,unsigned char *hiBit,unsigned long *low4Bytes));

void ReadSTD P((unsigned char *inputBuffer,unsigned char *stdBufferScale,unsigned long *stdBufferSize));

void ReadRate P((unsigned char *inputBuffer,unsigned long *rate));

int MakeFloatClockTime P((unsigned char hiBit,unsigned long low4Bytes,double *floatClockTime));

/* Statistics routines in filter.c */
void init_block_struct P((BlockVals *a));
void init_stats P((void));
void PrintAllStats P((void));
void PrintSummaryStat P((int typ));
void CollectStats P((void));
unsigned int bitCountRead P((void));
void StartTime P((void));
void EndTime P((void));
double ReadSysClock P((void));
void PrintTimeInfo P((void));
void PrintQT P((FILE *stream,char *title,int changed,unsigned char *ptr));
void print_binary P(( FILE *fp, char *buf, int size));

#undef P











