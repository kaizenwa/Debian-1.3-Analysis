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

#include <stdio.h>
#include <setjmp.h>

/* X11/xmd.h correctly defines INT32, etc */
#ifndef XMD_H
typedef int INT32;
typedef short INT16;
typedef char INT8;
#endif
typedef unsigned int UINT32;
typedef unsigned short UINT16;
typedef unsigned char UINT8;
 
/* Define Parsing error codes. */

#define SKIP_PICTURE (-10)
#define SKIP_TO_START_CODE (-1)
#define PARSE_OK 1

/* Define BOOLEAN, TRUE, and FALSE. */

#define BOOLEAN int
#define TRUE 1
#define FALSE 0

/* Initial value for max_code_types */

#define MAX_CODE_TYPES 1024 /* The code assumes this will be 1024 or larger */

/* Set ring buffer size. */

#define RING_BUF_SIZE 5

/* Macros for picture code type. */

#define I_TYPE 1
#define P_TYPE 2
#define B_TYPE 3
#define D_TYPE 4
#define SKIP_P_TYPE 5
#define SKIP_B_TYPE 6

/* Start codes. */

#define SEQ_END_CODE 0x000001b7
#define SEQ_START_CODE 0x000001b3
#define GOP_START_CODE 0x000001b8
#define PICTURE_START_CODE 0x00000100
#define SLICE_MIN_START_CODE 0x00000101
#define SLICE_MAX_START_CODE 0x000001af
#define EXT_START_CODE 0x000001b5
#define USER_START_CODE 0x000001b2
#define SEQUENCE_ERROR_CODE 0x000001b4

/* Macros used with macroblock address decoding. */

#define MB_STUFFING 34
#define MB_ESCAPE 35

/* Lock flags for pict images. */

#define DISPLAY_LOCK 0x01
#define PAST_LOCK 0x02
#define FUTURE_LOCK 0x04

#define HYBRID_DITHER 0
#define HYBRID2_DITHER 1
#define FS4_DITHER 2
#define FS2_DITHER 3
#define FS2FAST_DITHER 4
#define Twox2_DITHER 5
#define GRAY_DITHER 6
#define FULL_COLOR_DITHER 7
#define NO_DITHER 8
#define ORDERED_DITHER 9
#define MONO_DITHER 10
#define MONO_THRESHOLD 11
#define ORDERED2_DITHER 12
#define MBORDERED_DITHER 13

/* External declaration of row,col to zig zag conversion matrix. */

extern const int scan[][8];

/* Temporary definition of time stamp structure. */

typedef int TimeStamp;

/* Structure with reconstructed pixel values. */

typedef struct pict_image {
  unsigned char *luminance;              /* Luminance plane.   */
  unsigned char *Cr;                     /* Cr plane.          */
  unsigned char *Cb;                     /* Cb plane.          */
  unsigned char *display;                /* Display plane.     */
  int locked;                            /* Lock flag.         */
  TimeStamp show_time;                   /* Presentation time. */

#ifdef SH_MEM
  XShmSegmentInfo shminfo;               /* Segment info.  */
  XImage *ximage;                        /* Ximage struct. */
#endif

} PictImage;

/* Group of pictures structure. */

typedef struct GoP {
  BOOLEAN drop_flag;                     /* Flag indicating dropped frame. */
  unsigned int tc_hours;                 /* Hour component of time code.   */
  unsigned int tc_minutes;               /* Minute component of time code. */
  unsigned int tc_seconds;               /* Second component of time code. */
  unsigned int tc_pictures;              /* Picture counter of time code.  */
  BOOLEAN closed_gop;                    /* Indicates no pred. vectors to
					    previous group of pictures.    */
  BOOLEAN broken_link;                   /* B frame unable to be decoded.  */
  char *ext_data;                        /* Extension data.                */
  char *user_data;                       /* User data.                     */
  int  ext_size;                         /* Length of Extension data       */
  int  user_size;                        /* Length of User data            */
  char *code_types;                      /* char array representing picture
					    coding types in display order  */
  int  max_code_types;                   /* current size of code_types     */
  int  picture_count;                    /* number of pictures in this GOP */
} GoP;

/* Picture structure. */

typedef struct pict {
  unsigned int temp_ref;                 /* Temporal reference.             */
  unsigned int code_type;                /* Frame type: P, B, I             */
  unsigned int vbv_delay;                /* Buffer delay.                   */
  BOOLEAN full_pel_forw_vector;          /* Forw. vectors specified in full
					    pixel values flag.              */
  unsigned int forw_r_size;              /* Used for vector decoding.       */
  unsigned int forw_f;                   /* Used for vector decoding.       */
  BOOLEAN full_pel_back_vector;          /* Back vectors specified in full 
					    pixel values flag.              */
  unsigned int back_r_size;              /* Used in decoding.               */
  unsigned int back_f;                   /* Used in decoding.               */
  char *extra_info;                      /* Extra bit picture info.         */
  char *ext_data;                        /* Extension data.                 */
  char *user_data;                       /* User data.                      */
  int  ext_size;                         /* Length of Extension data       */
  int  user_size;                        /* Length of User data            */
} Pict;

/* Slice structure. */

typedef struct slice {
  unsigned int vert_pos;                 /* Vertical position of slice. */
  unsigned int quant_scale;              /* Quantization scale.         */
  char *extra_info;                      /* Extra bit slice info.       */
} Slice;

/* Macroblock structure. */

typedef struct macroblock {
  int mb_address;                        /* Macroblock address.              */
  int past_mb_addr;                      /* Previous mblock address.         */
  int motion_h_forw_code;                /* Forw. horiz. motion vector code. */
  unsigned int motion_h_forw_r;          /* Used in decoding vectors.        */
  int motion_v_forw_code;                /* Forw. vert. motion vector code.  */
  unsigned int motion_v_forw_r;          /* Used in decdoinge vectors.       */
  int motion_h_back_code;                /* Back horiz. motion vector code.  */
  unsigned int motion_h_back_r;          /* Used in decoding vectors.        */
  int motion_v_back_code;                /* Back vert. motion vector code.   */
  unsigned int motion_v_back_r;          /* Used in decoding vectors.        */
  unsigned int cbp;                      /* Coded block pattern.             */
  BOOLEAN mb_intra;                      /* Intracoded mblock flag.          */
  BOOLEAN bpict_past_forw;               /* Past B frame forw. vector flag.  */
  BOOLEAN bpict_past_back;               /* Past B frame back vector flag.   */
  int past_intra_addr;                   /* Addr of last intracoded mblock.  */
  int recon_right_for_prev;              /* Past right forw. vector.         */
  int recon_down_for_prev;               /* Past down forw. vector.          */
  int recon_right_back_prev;             /* Past right back vector.          */
  int recon_down_back_prev;              /* Past down back vector.           */
} Macroblock;

/* Block structure. */

typedef struct block {
  short int dct_recon[8][8];             /* Reconstructed dct coeff matrix. */
  short int dct_dc_y_past;               /* Past lum. dc dct coefficient.   */
  short int dct_dc_cr_past;              /* Past cr dc dct coefficient.     */
  short int dct_dc_cb_past;              /* Past cb dc dct coefficient.     */
} Block;

/* Video stream structure. */

typedef struct vid_stream {
  unsigned int h_size;                         /* Horiz. size in pixels.     */
  unsigned int v_size;                         /* Vert. size in pixels.      */
  unsigned int mb_height;                      /* Vert. size in mblocks.     */
  unsigned int mb_width;                       /* Horiz. size in mblocks.    */
  unsigned char aspect_ratio;                  /* Code for aspect ratio.     */
  unsigned char orig_picture_rate;             /* Code for picture rate 
						  as it appears in the stream*/
  unsigned char picture_rate;                  /* a valid picture rate       */
  unsigned int bit_rate;                       /* Bit rate.                  */
  unsigned int vbv_buffer_size;                /* Minimum buffer size.       */
  BOOLEAN const_param_flag;                    /* Contrained parameter flag. */
  unsigned char intra_quant_matrix[8][8];      /* Quantization matrix for
						  intracoded frames.         */
  unsigned char non_intra_quant_matrix[8][8];  /* Quanitization matrix for 
						  non intracoded frames.     */
  char *ext_data;                              /* Extension data.            */
  char *user_data;                             /* User data.                 */
  int  ext_size;                               /* Length of Extension data   */
  int  user_size;                              /* Length of User data        */
  GoP group;                                   /* Current group of pict.     */
  Pict picture;                                /* Current picture.           */
  Slice slice;                                 /* Current slice.             */
  Macroblock mblock;                           /* Current macroblock.        */
  Block block;                                 /* Current block.             */
  int state;                                   /* State of decoding.         */
  int bit_offset;                              /* Bit offset in stream.      */
  unsigned int *buffer;                        /* Pointer to next byte in
						  buffer.                    */
  int buf_length;                              /* Length of remaining buffer.*/
  unsigned int *buf_start;                     /* Pointer to buffer start.   */
  int max_buf_length;                          /* Max lenght of buffer.      */
  PictImage *past;                             /* Past predictive frame.     */
  PictImage *future;                           /* Future predictive frame.   */
  PictImage *current;                          /* Current frame.             */
  PictImage *ring[RING_BUF_SIZE];              /* Ring buffer of frames.     */
} VidStream;   

/* Declaration of global pointer to current video stream. */

extern VidStream *curVidStream;

/* Dither flags external declaration. */
extern char *ditherFlags;

/* Definition of Contant integer scale factor. */

#define CONST_BITS 13

/* Misc DCT definitions */
#define DCTSIZE		8	/* The basic DCT block is 8x8 samples */
#define DCTSIZE2	64	/* DCTSIZE squared; # of elements in a block */

#define GLOBAL			/* a function referenced thru EXTERNs */
  
typedef short DCTELEM;
typedef DCTELEM DCTBLOCK[DCTSIZE2];
 

extern double realTimeStart;
extern int totNumFrames;

extern unsigned int bitCount;
extern int showEachFlag;
extern unsigned int cacheHit[8][8];
extern unsigned int cacheMiss[8][8];

/* Statistics collecting structure */
typedef struct {
  unsigned int frame, slice, block, size, btype, qs;
  unsigned int mb_skipped, mb_coded, cblks, nblks;
  int q[4][256];
  int chist[6];
} BlockVals;

typedef struct {
  int frametype;
  unsigned int totsize;
  unsigned int number;
  unsigned int i_mbsize;
  unsigned int p_mbsize;
  unsigned int b_mbsize;
  unsigned int bi_mbsize;
  unsigned int i_mbnum;
  unsigned int p_mbnum;
  unsigned int b_mbnum;
  unsigned int bi_mbnum;
  unsigned int i_mbcbp[64];
  unsigned int p_mbcbp[64];
  unsigned int b_mbcbp[64];
  unsigned int bi_mbcbp[64];
  unsigned int i_mbcoeff[64];
  unsigned int p_mbcoeff[64];
  unsigned int b_mbcoeff[64];
  unsigned int bi_mbcoeff[64];
  unsigned long qual;
  unsigned int qnum;
  double tottime;
} Statval;

/* System Layer Junk */
#define PACK_START_CODE             (unsigned int)0x000001ba
#define SYSTEM_HEADER_START_CODE    (unsigned int)0x000001bb
#define PACKET_START_CODE_MASK      (unsigned int)0xffffff00
#define PACKET_START_CODE_PREFIX    (unsigned int)0x00000100
#define ISO_11172_END_CODE          (unsigned int)0x000001b9

#define PACK_HEADER_SIZE 8

#define STD_AUDIO_STREAM_ID ((unsigned char) 0xb8)
#define STD_VIDEO_STREAM_ID ((unsigned char) 0xb9)
#define MIN_STREAM_ID_ID    ((unsigned char) 0xbc)
#define RESERVED_STREAM_ID  ((unsigned char) 0xbc)
#define PRIVATE_STREAM_1_ID ((unsigned char) 0xbd)
#define PADDING_STREAM_ID   ((unsigned char) 0xbe)
#define PRIVATE_STREAM_2_ID ((unsigned char) 0xbf)
#define NOT_PACKET_ID       ((unsigned char) 0xff)
#define KILL_BUFFER         ((unsigned char) 0xfe)


#define STD_SYSTEM_CLOCK_FREQ (unsigned long)90000
#define MUX_RATE_SCALE_FACTOR 50

#define MAX_STREAMS 8

