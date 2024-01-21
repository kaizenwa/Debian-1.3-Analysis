/* MPEGSTAT - analyzing tool for MPEG-I video streams
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
/* 
 * This file contains C code that implements
 * the video decoder model.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifndef MIPS
#include <sys/time.h>
#else
#include <sys/types.h>
#include <sys/system.h>
#endif

#include "decoders.h"
#include "video.h"
#include "util.h"
#include "proto.h"
#include "opts.h"

/* Declarations of functions. */
static int ReconIMBlock();
static void ReconPMBlock();
static void ReconBMBlock();
static void ReconBiMBlock();
static void ReconSkippedBlock();
static void DoPictureDisplay();
static int ParseSeqHead();
static int ParseGOP();
static void PrintGOP();
static int ParsePicture();
static int ParseSlice();
static int ParseMacroBlock();
static void ProcessSkippedPFrameMBlocks();
static void ProcessSkippedBFrameMBlocks();

/* Macro for returning 1 if num is positive, -1 if negative, 0 if 0. */

#define Sign(num) ((num > 0) ? 1 : ((num == 0) ? 0 : -1))


/* CONSTANTS */
/* Set up array for fast conversion from zig zag order to row/column
   coordinates.
*/

const int zigzag[64][2] = {
  0, 0, 1, 0, 0, 1, 0, 2, 1, 1, 2, 0, 3, 0, 2, 1, 1, 2, 0, 3, 0, 4, 1, 3,
  2, 2, 3, 1, 4, 0, 5, 0, 4, 1, 3, 2, 2, 3, 1, 4, 0, 5, 0, 6, 1, 5, 2, 4,
  3, 3, 4, 2, 5, 1, 6, 0, 7, 0, 6, 1, 5, 2, 4, 3, 3, 4, 2, 5, 1, 6, 0, 7,
  1, 7, 2, 6, 3, 5, 4, 4, 5, 3, 6, 2, 7, 1, 7, 2, 6, 3, 5, 4, 4, 5, 3, 6,
  2, 7, 3, 7, 4, 6, 5, 5, 6, 4, 7, 3, 7, 4, 6, 5, 5, 6, 4, 7, 5, 7, 6, 6,
  7, 5, 7, 6, 6, 7, 7, 7};
/* Array mapping zigzag to array pointer offset. */

const int zigzag_direct[64] = {
  0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5, 12,
  19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28, 35,
  42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51,
  58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63};

/* Set up array for fast conversion from row/column coordinates to
   zig zag order.
*/

const int scan[8][8] = {
  {0, 1, 5, 6, 14, 15, 27, 28},
  {2, 4, 7, 13, 16, 26, 29, 42},
  {3, 8, 12, 17, 25, 30, 41, 43},
  {9, 11, 18, 24, 31, 40, 44, 53},
  {10, 19, 23, 32, 39, 45, 52, 54},
  {20, 22, 33, 38, 46, 51, 55, 60},
  {21, 34, 37, 47, 50, 56, 59, 61},
  {35, 36, 48, 49, 57, 58, 62, 63}};

const char *VidRate[16]={"forbidden","23.976 frames/sec","24 frames/sec","25 frames/sec",
  "29.97 frames/sec","30 frames/sec","50 frames/sec","59.94 frames/sec","60 frames/sec",
  "reserved","reserved","reserved","reserved","reserved","reserved","reserved"};
const double VidRateNum[16]={1.0, 23.976, 24.0, 25.0, 29.97, 30.0, 50.0 ,59.94, 60.0,
  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0};

const char *PelRatio[16]={"forbidden","1 (VGA)","0.6735","0.7031 (16:9 625 lines)",
  "0.7615","0.8055","0.8437 (16:9 525 lines)","0.8935","0.9375 (CCIR 601, 625 lines)",
  "0.9815","1.0255","1.0695","1.1250 (CCIR 601, 525 lines)","1.1575","1.2015","reserved"};

/*
 * We use a lookup table to make sure values stay in the 0..255 range.
 * Since this is cropping (ie, x = (x < 0)?0:(x>255)?255:x; ), wee call this
 * table the "crop table".
 * MAX_NEG_CROP is the maximum neg/pos value we can handle.
 */

#define MAX_NEG_CROP 384
#define NUM_CROP_ENTRIES (256+2*MAX_NEG_CROP)
static unsigned char cropTbl[NUM_CROP_ENTRIES];

extern int ditherType;
char *ditherFlags;

/* Declare global pointer to vid stream used for current decoding. */

VidStream *curVidStream = NULL;

/* Max lum, chrom indices for illegal block checking. */

static int lmaxx;
static int lmaxy;
static int cmaxx;
static int cmaxy;

/* Error Handling Code */
char *errorLocation;
char *errorSpecifics;

/* DCT printing code */
char *dctSpecifics;

/* Boolean to keep track whether f_codes are within constrained params. */
BOOLEAN f_code_ok=TRUE;

/* Statistics Decls */

unsigned int bitCount = 0;

unsigned int pictureSizeCount;
unsigned int mbSizeCount;
unsigned int *mbCBPPtr, *mbCoeffPtr, *mbSizePtr;

Statval stat_a[4];

/* Rate Measuring values */
int rate_vals[60],rate_ptr=0,rate_sum=0,rate_started=0;
int rate_disp,rate_init=FALSE,rate_max=0,rate_min=0xFFFFFFF;


/* Keep track of block (Statistics) information */
BlockVals blks;

double realTimeStart;
int totNumFrames = 0;

/*
 *--------------------------------------------------------------
 *
 * NewVidStream --
 *
 *	Allocates and initializes a VidStream structure. Takes
 *      as parameter requested size for buffer length.
 *
 * Results:
 *	A pointer to the new VidStream structure.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

VidStream *
NewVidStream(bufLength)
  int bufLength;
{
  int i, j;
  VidStream *new;
  static unsigned char default_intra_matrix[64] = {
    8, 16, 19, 22, 26, 27, 29, 34,
    16, 16, 22, 24, 27, 29, 34, 37,
    19, 22, 26, 27, 29, 34, 34, 38,
    22, 22, 26, 27, 29, 34, 37, 40,
    22, 26, 27, 29, 32, 35, 40, 48,
    26, 27, 29, 32, 35, 40, 48, 58,
    26, 27, 29, 34, 38, 46, 56, 69,
    27, 29, 35, 38, 46, 56, 69, 83};

  /* Check for legal buffer length. */

  if (bufLength < 4)
    return NULL;

  /* Make buffer length multiple of 4. */

  bufLength = (bufLength + 3) >> 2;

  /* Allocate memory for new structure. */

  new = (VidStream *) malloc(sizeof(VidStream));
  if (new == NULL)
    return NULL;

  /* Initialize pointers to extension and user data. */

  new->group.ext_data = new->group.user_data =
    new->picture.extra_info = new->picture.user_data =
    new->picture.ext_data = new->slice.extra_info =
    new->ext_data = new->user_data = NULL;

  /* Initialize stuff for GOP checking and display */
  
  new->group.code_types = (char *) malloc(MAX_CODE_TYPES);
  if (new->group.code_types == NULL)
    return NULL;
  
  new->group.max_code_types = MAX_CODE_TYPES;
  new->group.picture_count = 0;
  new->picture.full_pel_forw_vector = -1;
  /* Copy default intra matrix. */

  for (i = 0; i < 8; i++) {
    for (j = 0; j < 8; j++) {
      new->intra_quant_matrix[j][i] = default_intra_matrix[i * 8 + j];
    }
  }

  /* Initialize crop table. */

  for (i = (-MAX_NEG_CROP); i < NUM_CROP_ENTRIES - MAX_NEG_CROP; i++) {
    if (i <= 0) {
      cropTbl[i + MAX_NEG_CROP] = 0;
    } else if (i >= 255) {
      cropTbl[i + MAX_NEG_CROP] = 255;
    } else {
      cropTbl[i + MAX_NEG_CROP] = i;
    }
  }

  /* Initialize non intra quantization matrix. */

  for (i = 0; i < 8; i++) {
    for (j = 0; j < 8; j++) {
      new->non_intra_quant_matrix[j][i] = 16;
    }
  }

  /* Initialize pointers to image spaces. */

  new->current = new->past = new->future = NULL;
  for (i = 0; i < RING_BUF_SIZE; i++) {
    new->ring[i] = NULL;
  }

  /* Create buffer. */

  new->buf_start = (unsigned int *) malloc(bufLength * 4);
  if (new->buf_start == NULL)
    return NULL;

  /*
   * Set max_buf_length to one less than actual length to deal with messy
   * data without proper seq. end codes.
   */

  new->max_buf_length = bufLength - 1;

  /* Initialize bitstream i/o fields. */

  new->bit_offset = 0;
  new->buf_length = 0;
  new->buffer = new->buf_start;


  /* Return structure. */

  return new;
}



/*
 *--------------------------------------------------------------
 *
 * DestroyVidStream --
 *
 *	Deallocates a VidStream structure.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
void
DestroyVidStream(astream)
  VidStream *astream;
{
  int i;

  if (astream->ext_data != NULL)
    free(astream->ext_data);

  if (astream->user_data != NULL)
    free(astream->user_data);

  if (astream->group.ext_data != NULL)
    free(astream->group.ext_data);

  if (astream->group.user_data != NULL)
    free(astream->group.user_data);

  if (astream->picture.extra_info != NULL)
    free(astream->picture.extra_info);

  if (astream->picture.ext_data != NULL)
    free(astream->picture.ext_data);

  if (astream->picture.user_data != NULL)
    free(astream->picture.user_data);

  if (astream->slice.extra_info != NULL)
    free(astream->slice.extra_info);

  if (astream->buf_start != NULL)
    free(astream->buf_start);

  for (i = 0; i < RING_BUF_SIZE; i++) {
    if (astream->ring[i] != NULL) {
      DestroyPictImage(astream->ring[i]);
      astream->ring[i] = NULL;
    }
  }

  free((char *) astream);
}




/*
 *--------------------------------------------------------------
 *
 * NewPictImage --
 *
 *	Allocates and initializes a PictImage structure.
 *      The width and height of the image space are passed in
 *      as parameters.
 *
 * Results:
 *	A pointer to the new PictImage structure.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

PictImage *
NewPictImage(width, height)
  unsigned int width, height;
{
  PictImage *new;

  /* Allocate memory space for new structure. */

  new = (PictImage *) malloc(sizeof(PictImage));


  /* Allocate memory for image spaces. */

  new->display = (unsigned char *) malloc(width * height);

  new->luminance = (unsigned char *) malloc(width * height);
  new->Cr = (unsigned char *) malloc(width * height / 4);
  new->Cb = (unsigned char *) malloc(width * height / 4);

  /* Reset locked flag. */

  new->locked = 0;

  /* Return pointer to new structure. */

  return new;
}



/*
 *--------------------------------------------------------------
 *
 * DestroyPictImage --
 *
 *	Deallocates a PictImage structure.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
void
DestroyPictImage(apictimage)
  PictImage *apictimage;
{
  if (apictimage->luminance != NULL) {
    free(apictimage->luminance);
  }
  if (apictimage->Cr != NULL) {
    free(apictimage->Cr);
  }
  if (apictimage->Cb != NULL) {
    free(apictimage->Cb);
  }

  if (apictimage->display != NULL) {
    free(apictimage->display);
  }
  free(apictimage);
}



/*
 *--------------------------------------------------------------
 *
 * mpegVidRsrc --
 *
 *      Parses bit stream until MB_QUANTUM number of
 *      macroblocks have been decoded or current slice or
 *      picture ends, whichever comes first. If the start
 *      of a frame is encountered, the frame is time stamped
 *      with the value passed in time_stamp. If the value
 *      passed in buffer is not null, the video stream buffer
 *      is set to buffer and the length of the buffer is
 *      expected in value passed in through length. The current
 *      video stream is set to vid_stream. If vid_stream
 *      is passed as NULL, a new VidStream structure is created
 *      and initialized and used as the current video stream.
 *
 * Results:
 *      A pointer to the video stream structure used.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed. If a picture is completed,
 *      a function is called to display the frame at the correct time.
 *
 *--------------------------------------------------------------
 */

VidStream *
mpegVidRsrc(time_stamp, vid_stream)
  TimeStamp time_stamp;
  VidStream *vid_stream;
{
  unsigned int data;
  int i, status;

  /* Setup error specifics */
  errorSpecifics=malloc(100);
  *errorSpecifics='\0';
  dctSpecifics=malloc(4000);
  *dctSpecifics='\0';

  /* If vid_stream is null, create new VidStream structure. */

  if (vid_stream == NULL) {
    return NULL;
  }
  /*
   * Set global curVidStream to vid_stream. Necessary because bit i/o use
   * curVidStream and are not passed vid_stream. Also set global bitstream
   * parameters.
   */

  curVidStream = vid_stream;
  bitOffset = curVidStream->bit_offset;
#ifdef UTIL2
  curBits = *curVidStream->buffer << bitOffset;
#else
  curBits = *curVidStream->buffer;
#endif
  bufLength = curVidStream->buf_length;
  bitBuffer = curVidStream->buffer;

  next_start_code();
  show_bits32(data);
  if (data != SEQ_START_CODE) {
    fprintf(stderr, "This is not an MPEG stream.\n");
    DestroyVidStream(curVidStream);
    exit(1);
  }
  
  /*
   * Process according to start code (or parse macroblock if not a start code
   * at all.
   */
  
  while (1) { 
  start:
    show_bits32(data);
    switch (data) {

    case SEQ_END_CODE:
    case 0x000001b9:   /*  handle ISO_11172_END_CODE too */
      /* Print last gop. */
      PrintGOP(vid_stream);
    
      /* Display last frame. */
      if (vid_stream->future != NULL) {
	vid_stream->current = vid_stream->future;
      }
      
      /* Sequence done. Do the right thing. For right now, exit. */
      
      
      PrintAllStats(); 
      PrintTimeInfo(); 
      
      DestroyVidStream(curVidStream);
      exit(0);
      break;
      
    case SEQ_START_CODE:
      
      /* Sequence start code. Parse sequence header. */
      if (opts&COLLECTING&OFFS_INFO) {
	fprintf(offs_fp,"sequence %d\n",bitCountRead());
      }
      
      if (ParseSeqHead(vid_stream) != PARSE_OK) {
	errorLocation="Sequence Header";
	goto error;
      }
      
      /*
       * Return after sequence start code so that application above can use
       * info in header.
       */
      
      break;
      
    case GOP_START_CODE:

      /* Group of Pictures start code.*/
      /* Print previous gop.  (For the first GOP, the function does nothing.) */
      PrintGOP(vid_stream);
      
      if (opts&COLLECTING&OFFS_INFO) {
        fprintf(offs_fp,"gop %d\n",bitCountRead());
      }
      
      /* Parse GOP Header */
      if (ParseGOP(vid_stream) != PARSE_OK) {
        errorLocation="Group of Pictures";
        goto error;
      }

      break;

    case PICTURE_START_CODE:
      
      /* Picture start code. Parse picture header and first slice header. */
      
      status = ParsePicture(vid_stream, time_stamp);
      
      if (status == SKIP_PICTURE) {
	next_start_code();
	while (!next_bits(32, PICTURE_START_CODE)) {
	  if (next_bits(32, GOP_START_CODE))
	    break;
	  else if (next_bits(32, SEQ_END_CODE))
	    break;
	  flush_bits(24);
	  next_start_code();
	}
	break;
      } else if (status != PARSE_OK) {
	errorLocation="Picture";
	goto error;
      }
      
      if (ParseSlice(vid_stream) != PARSE_OK) {
	errorLocation="Slice (1)";
	goto error;
      }
      break;

    case SEQUENCE_ERROR_CODE:
      flush_bits32;
      fprintf(stderr,"Sequence error code at byte %d\n", bitOffset/8);
      next_start_code();
      goto start;
      
    default:
      
      /* Check for slice start code. */
      
      if ((data >= SLICE_MIN_START_CODE) && (data <= SLICE_MAX_START_CODE)) {
	
	/* Slice start code. Parse slice header. */
	
	if (ParseSlice(vid_stream) != PARSE_OK) {
	  errorLocation="Slice (2)";
	  goto error;
	}
      } else {
	if ((unsigned int) (data & 0xfffffe00) == (unsigned int )0) {
	  /* Wasn't any valid start code for MPEG-1 video streams */
	  fprintf(stderr, "Invalid start code(0x%x at offset %d)%s\n",
		  data, bitCountRead()/8,
		  (bitCountRead() < 128) ? " perhaps MPEG-2 file?" : "");
	  exit(1);
	}
      }

      /* Parse until next bits are start code */
      
      do {
	if (ParseMacroBlock(vid_stream) != PARSE_OK) {
	  errorLocation = "Macro block";
	  goto error;
	}
      } while (!next_bits(23, 0x00000000));
      
      next_start_code();
      show_bits32(data);
      
      /*
       * If start code is outside range of slice start codes, frame is
       * complete, display frame.
       */
      
      if (((data < SLICE_MIN_START_CODE) || (data > SLICE_MAX_START_CODE)) &&
	  (data != SEQUENCE_ERROR_CODE)) {
	EndTime();
	stat_a[0].totsize += bitCountRead() - pictureSizeCount;
	if (opts&SIZE_INFO&COLLECTING) {
	  fprintf(size_fp,"%d\t%c\t%8d\n",
		  blks.frame,"0IPB"[stat_a[0].frametype],stat_a[0].totsize);
	}
	if (COLLECTING&opts&RATE_INFO) {
	  rate_sum-=rate_vals[rate_ptr];
	  rate_vals[rate_ptr]=stat_a[0].totsize;
	  rate_sum+=stat_a[0].totsize;
	  if (rate_started) fprintf(rate_fp,"%d\n",rate_sum);
	  else if (rate_ptr==(rate_disp-1)) rate_started=TRUE;
	  if (rate_sum>rate_max) rate_max=rate_sum;
	  if (rate_sum<rate_min) rate_min=rate_sum;
	  rate_ptr= (rate_ptr+1)%rate_disp;
	}
	if (COLLECTING) CollectStats();
	DoPictureDisplay(vid_stream);
      }
    }
  }
    
 error:
  /* "calling" routine must set errorLocation, errorSpecifics may be set */
  fprintf(stderr, "Error!!!! while parsing %s in frame %d, skipping to start code.  %s\n",
	  errorLocation,blks.frame,errorSpecifics);
  sprintf(errorSpecifics,"");
  if (next_start_code() == STREAM_UNDERFLOW) {
    fprintf(stderr, "Underflow!  Bailing out.\n");
    exit(1);
  }
  goto start;
}



/*
 *--------------------------------------------------------------
 *
 * ParseSeqHead --
 *
 *      Assumes bit stream is at the begining of the sequence
 *      header start code. Parses off the sequence header.
 *
 * Results:
 *      Fills the vid_stream structure with values derived and
 *      decoded from the sequence header. Allocates the pict image
 *      structures based on the dimensions of the image space
 *      found in the sequence header.
 *
 * Side effects:
 *      Bit stream irreversibly parsed off.
 *
 *--------------------------------------------------------------
 */

static int
ParseSeqHead(vid_stream)
  VidStream *vid_stream;
{

  unsigned int data;
  int i;

  /* Flush off sequence start code. */
  flush_bits32;

  /* Get horizontal size of image space. */
  get_bits12(data);
  vid_stream->h_size = data;

  /* Get vertical size of image space. */

  get_bits12(data);
  vid_stream->v_size = data;

  if (vid_stream->v_size ==0 || vid_stream->h_size == 0) {
    fprintf(stderr, "Exiting, image size appears to be zero!\n");
    exit(1);
  }

  /* Calculate macroblock width and height of image space. */

  vid_stream->mb_width = (vid_stream->h_size + 15) / 16;
  vid_stream->mb_height = (vid_stream->v_size + 15) / 16;

  /* If dither type is MBORDERED allocate ditherFlags. */

  if (ditherType == MBORDERED_DITHER) {
    ditherFlags = (char *) malloc(vid_stream->mb_width*vid_stream->mb_height);
  }

  /* Initialize lmaxx, lmaxy, cmaxx, cmaxy. */

  lmaxx = vid_stream->mb_width*16-8;
  lmaxy = vid_stream->mb_height*16-8;
  cmaxx = vid_stream->mb_width*8-8;
  cmaxy = vid_stream->mb_height*8-8;

  /*
   * Initialize ring buffer of pict images now that dimensions of image space
   * are known.
   */


  if (vid_stream->ring[0] == NULL) {
    for (i = 0; i < RING_BUF_SIZE; i++) {
      vid_stream->ring[i] = NewPictImage(vid_stream->mb_width * 16,
					 vid_stream->mb_height * 16);
    }
  }

  /* Parse of aspect ratio code. */

  get_bits4(data);
  vid_stream->aspect_ratio = (unsigned char) data;

  /* Parse off picture rate code. */

  get_bits4(data);
  vid_stream->picture_rate = (unsigned char) data;
  vid_stream->orig_picture_rate = (unsigned char) data;
  if (VidRateNum[data] == 1.0) {
    fprintf(stderr,
	    "Picture rate is invalid!  It is listed in Std as %s (code is %d)\n",
	    VidRate[data],data);
    fprintf(stderr,
	    "Any average dependent on the picture rate will be incorrect\n");
    fprintf(stderr,"\t(assumed to mean 30fps).  Probably a Xing sequence.\n\n");
    vid_stream->picture_rate=5;
  }
  if (opts&RATE_INFO) {
    /* Setup data rate collection structures */
    if (!rate_init) {
      if (opts&RATE_LENGTH_SET)
	rate_disp=rate_frames;
      else rate_disp=(int)(VidRateNum[data]+0.5);
      rate_ptr=0; rate_started=FALSE; rate_init=TRUE;
      for(i=0; i<rate_disp;i++) rate_vals[i]=0;
      rate_sum=0;
    } else if ((int)(VidRateNum[data]+0.5)!=rate_disp) {
	fprintf(rate_fp,"CHANGE in picture rate from %d to %d\n",
		(int)(VidRateNum[data]+0.5),rate_disp);
      }
  }

  /* Parse off bit rate. */
  get_bits18(data);
  vid_stream->bit_rate = data;
  
  /* Flush marker bit. */
  flush_bits(1);

  /* Parse off vbv buffer size. */
  get_bits10(data);
  vid_stream->vbv_buffer_size = data;

  if (data*1024>vid_stream->max_buf_length) {
    unsigned int *newbuf;
    int sz=1024*data+1;
    /* If they actually want a bigger buffer than we default to,
       let them have it! (if we can) */
    newbuf = (unsigned int *) realloc((char *)vid_stream->buf_start,4*sz);
    if (newbuf!=(unsigned int *)NULL) {
      vid_stream->max_buf_length=sz;
      bitBuffer=(bitBuffer-vid_stream->buf_start)+newbuf;
      vid_stream->buf_start=newbuf;
    }}

  /* Parse off constrained parameter flag. */
  get_bits1(data);
  if (data) {
    vid_stream->const_param_flag = TRUE;
  } else
    vid_stream->const_param_flag = FALSE;

  /*
   * If intra_quant_matrix_flag set, parse off intra quant matrix values.
   */
  get_bits1(data);
  if (data) {
    int new_table=FALSE;
    for (i = 0; i < 64; i++) {
      get_bits8(data);
      new_table = new_table ||
	(vid_stream->intra_quant_matrix[zigzag[i][1]][zigzag[i][0]] !=
	 (unsigned char) data);
      vid_stream->intra_quant_matrix[zigzag[i][1]][zigzag[i][0]] =
	(unsigned char) data;
    }
    if (opts&QSCALE_INFO) {
      PrintQT(qscale_fp,"IQ",new_table,(unsigned char *)vid_stream->intra_quant_matrix);
    }
    if ((opts&LOUD) || new_table) {
      PrintQT(stdout,"IQ",new_table,(unsigned char *)vid_stream->intra_quant_matrix);
    }
  }

  /*
   * If non intra quant matrix flag set, parse off non intra quant matrix
   * values.
   */

  get_bits1(data);
  if (data) {
    int new_table=FALSE;
    for (i = 0; i < 64; i++) {
      get_bits8(data);
      new_table = new_table ||
	(vid_stream->non_intra_quant_matrix[zigzag[i][1]][zigzag[i][0]] !=
	 (unsigned char) data);
      vid_stream->non_intra_quant_matrix[zigzag[i][1]][zigzag[i][0]] =
	(unsigned char) data;
    }
    if (opts&QSCALE_INFO) {
      PrintQT(qscale_fp,"NIQ",new_table,(unsigned char *)vid_stream->non_intra_quant_matrix);
    }
    if ((opts&LOUD) || new_table) {
      PrintQT(stdout,"NIQ",new_table,(unsigned char *)vid_stream->non_intra_quant_matrix);
    }
  }

  next_start_code();

  /*
   * If next start code is extension start code, parse off extension data.
   */

  if (next_bits(32, EXT_START_CODE)) {
    int sz;
    flush_bits32;
    if (vid_stream->ext_data != NULL) {
      free(vid_stream->ext_data);
      vid_stream->ext_data = NULL;
    }
    vid_stream->ext_data = get_ext_data(&sz);
    vid_stream->ext_size = sz;
  }
  /* If next start code is user start code, parse off user data. */

  if (next_bits(32, USER_START_CODE)) {
    int sz;
    flush_bits32;
    if (vid_stream->user_data != NULL) {
      free(vid_stream->user_data);
      vid_stream->user_data = NULL;
    }
    vid_stream->user_data = get_ext_data(&sz);
    vid_stream->user_size = sz;
    if (sz > 0 && opts&COLLECTING&USERDAT_INFO) {
      fprintf(userdat_fp, "Sequence Header user data:\n");
      print_binary(userdat_fp, vid_stream->user_data, vid_stream->user_size);
    }
  }
  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ParseGOP --
 *
 *      Parses of group of pictures header from bit stream
 *      associated with vid_stream.
 *
 * Results:
 *      Values in gop header placed into video stream structure.
 *
 * Side effects:
 *      Bit stream irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

static int
ParseGOP(vid_stream)
  VidStream *vid_stream;
{
  unsigned int data;

  /* Reset stuff for GOP checking */
  vid_stream->group.picture_count = 0;
  memset(vid_stream->group.code_types, 0, vid_stream->group.max_code_types);


  /* Flush group of pictures start code. WWWWWWOOOOOOOSSSSSSHHHHH!!! */

  flush_bits32;

  /* Parse off drop frame flag. */

  get_bits1(data);
  if (data) {
    vid_stream->group.drop_flag = TRUE;
  } else
    vid_stream->group.drop_flag = FALSE;

  /* Parse off hour component of time code. */

  get_bits5(data);
  vid_stream->group.tc_hours = data;

  /* Parse off minute component of time code. */

  get_bits6(data);
  vid_stream->group.tc_minutes = data;

  /* Flush marker bit. */

  flush_bits(1);

  /* Parse off second component of time code. */

  get_bits6(data);
  vid_stream->group.tc_seconds = data;

  /* Parse off picture count component of time code. */

  get_bits6(data);
  vid_stream->group.tc_pictures = data;

  /* Parse off closed gop and broken link flags. */

  get_bits2(data);
  if (data > 1) {
    vid_stream->group.closed_gop = TRUE;
    if (data > 2) {
      vid_stream->group.broken_link = TRUE;
    } else
      vid_stream->group.broken_link = FALSE;
  } else {
    vid_stream->group.closed_gop = FALSE;
    if (data) {
      vid_stream->group.broken_link = TRUE;
    } else
      vid_stream->group.broken_link = FALSE;
  }

  if (opts&COLLECTING&BLOCK_INFO) {
    fprintf(block_fp,"gop %c %c\n",
	    vid_stream->group.closed_gop ? 'C' : 'O',
	    vid_stream->group.broken_link ? 'B': 'W');
  }
  

  /* Goto next start code. */

  next_start_code();

  /* If next start code is extension data, parse off extension data. */

  if (next_bits(32, EXT_START_CODE)) {
    flush_bits32;
    if (vid_stream->group.ext_data != NULL) {
      free(vid_stream->group.ext_data);
      vid_stream->group.ext_data = NULL;
    }
    vid_stream->group.ext_data = get_ext_data(&vid_stream->group.ext_size);
  }
  /* If next start code is user data, parse off user data. */

  if (next_bits(32, USER_START_CODE)) {
    flush_bits32;
    if (vid_stream->group.user_data != NULL) {
      free(vid_stream->group.user_data);
      vid_stream->group.user_data = NULL;
    }
    vid_stream->group.user_data = get_ext_data(&vid_stream->group.user_size);
    if ( vid_stream->group.user_size > 0 && opts&COLLECTING&USERDAT_INFO) {
      fprintf(userdat_fp, "GOP Header user data:\n");
      print_binary(userdat_fp, vid_stream->user_data, vid_stream->user_size);
    }
  }

  return PARSE_OK;
}


/*
 *--------------------------------------------------------------
 *
 * PrintGOP --
 *
 *      Prints picture_coding_types for a group of pictures with some 
 *      limited error checking on the temporal references.
 *
 * Results:
 *      Printed to stdout.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void
PrintGOP(vid_stream)
  VidStream *vid_stream;
{
  int i, c, do_print;

  /* If this is the first GOP, do nothing.  (Empty GOPs will go undetected.) */
  if (vid_stream->group.picture_count == 0) return;

  do_print = opts & COLLECTING & LOUD;

  if (do_print) printf(" / ");
  for (i = 0; i < vid_stream->group.picture_count; i++) {
    /* Guard against accessing beyond end of code_types[] */
    if (i == vid_stream->group.max_code_types) break;

    switch (vid_stream->group.code_types[i]) {
    case I_TYPE:
      c = 'I';
      break;
    case P_TYPE:
      c = 'P';
      break;
    case B_TYPE:
      c = 'B';
      break;
    case D_TYPE:
      c = 'D';
      break;
    case SKIP_P_TYPE:
    case SKIP_B_TYPE:
      c = '-';
      break;
    case 0:
      /* The output will be ugly if there is an error. */
      fprintf(stderr, "\nError: temporal_reference %d missing from GOP\n", i);
      /* no break */
    default:
      c = '?';
      break;
    }
    if (do_print) printf("%c", c);
  }
  if (do_print) printf("\n");

  /* check for Property 2 in D.5.2 of 11172-2 */
  switch (vid_stream->group.code_types[0]) {
  case I_TYPE:
  case B_TYPE:
  case SKIP_B_TYPE:
    break;
  default:
    fprintf(stderr, "\nError: first picture of GOP in display order must be I or B (D.5.2)\n");
  }

  switch (vid_stream->group.code_types[i - 1]) {
  case I_TYPE:
  case P_TYPE:
    break;
  default:
    /* This message may be bogus if GOP size was too big. */
    fprintf(stderr, "\nError: last picture of GOP in display order must be I or P (D.5.2)\n");
  }
}

/*
 *--------------------------------------------------------------
 *
 * ParsePicture --
 *
 *      Parses picture header. Marks picture to be presented
 *      at particular time given a time stamp.
 *
 * Results:
 *      Values from picture header put into video stream structure.
 *
 * Side effects:
 *      Bit stream irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

static int
ParsePicture(vid_stream, time_stamp)
VidStream *vid_stream;
  TimeStamp time_stamp;
{
  unsigned int data;
  int i;
  static int last_bit_count=0;
#ifdef doesntwork
  static int vbv_size=0;
  static double vbv_delay=0.0;
#endif

  /* Flush header start code. */
  flush_bits32;

  /* Parse off temporal reference. */
  get_bits10(data);
  vid_stream->picture.temp_ref = data;

  /* Parse of picture type. */
  get_bits3(data);
  if (data < I_TYPE || data > D_TYPE) {
    fprintf(stderr, "\nError: picture_coding_type is forbidden or reserved (frame %d, type %d)\n",
	    blks.frame+1,data);
  } else if (vid_stream->group.picture_count == 0 && data != I_TYPE) {
    fprintf(stderr, "\nError: First picture of GOP in bitstream order must be I\n");
  }
  vid_stream->picture.code_type = data;

  if (vid_stream->group.picture_count < vid_stream->group.max_code_types) {
    if (vid_stream->group.code_types[vid_stream->picture.temp_ref] == 0) {
      /* Out-of-order temporal_references are currently not detected */
      vid_stream->group.code_types[vid_stream->picture.temp_ref] =
	vid_stream->picture.code_type;
    } else {
      fprintf(stderr, "\nError: temporal reference %d duplicated in GOP\n",
	      vid_stream->picture.temp_ref);
      /*
       * TODO: this is actually legal for GOP size >= 1024, since
       * temporal_reference is a modulo 1024 number.  (See 11172-2  2.4.3.4)
       */
    }
  } else if (vid_stream->group.picture_count == vid_stream->group.max_code_types) {
    /* Only print this message once */
    fprintf(stderr, "\nWarning: GOP size bigger than I can check\n");
    /*
     * TODO: could realloc & increase max_code_types, but would also need
     * to handle temporal_reference values being reused.
     */
  }


  /* Parse off vbv buffer delay value. */
  get_bits16(data);
  vid_stream->picture.vbv_delay = data;

#ifdef doesntwork
   Unfortunately, vbv_delay > picture_delay a lot, so this doesnt work at all.
   Need to be rethought out.

  {  /* Check out VBV Buffer fullness */
/*    static int last_bit_count=0, vbv_size=0, vbv_delay=0;*/
    static int rate = -1;
    static int buffer_size;
    static double picture_delay;

    if (rate == -1) {
      rate = vid_stream->bit_rate * 400;
      buffer_size = vid_stream->vbv_buffer_size*16*1024;
      picture_delay = (1.0/VidRateNum[vid_stream->picture_rate]);
      /* Dont do anything at first picture */
    } else {
      vbv_size += rate * vbv_delay;
      if (vbv_size > buffer_size) {
	fprintf(stderr,"VBV overflow at frame %d (%d > %d bits)\n",
		blks.frame, vbv_size, buffer_size);
      } 
      vbv_size -= (bitCountRead() - last_bit_count);
      if (vbv_size < 0) {
	fprintf(stderr,"VBV underflow at frame %d (%d < 0 bits)\n",
		blks.frame, vbv_size);
      } 
      if (picture_delay < vbv_delay) {
	fprintf(stderr,"Yikes, vbv_delay > one frame time! (%g < %g)\n",
		picture_delay,vbv_delay);
      }
      vbv_size += rate * (picture_delay = vbv_delay);  /* add in missed time */
      vbv_delay = data / 90000.0;
    }
  }
#endif

  blks.frame++;

  if (START_F&opts) {
    COLLECTING = (blks.frame>=start_opt)?COLLECT_ON:COLLECT_OFF;
  } 
  if (opts&END_F) {
    if (blks.frame > end_opt) {
      PrintGOP(vid_stream);
      printf("Done Collecting!\n");
      PrintAllStats(); 
      PrintTimeInfo(); 
      DestroyVidStream(vid_stream);
      exit(0);
    }
  }

  vid_stream->group.picture_count++;

  /* echo frame type on screen */
  if (opts&COLLECTING&LOUD) {
    printtype(vid_stream);
  }

  if (opts&COLLECTING&OFFS_INFO) {
    fprintf(offs_fp,"picture %d (%d)\n",
	    bitCountRead(), vid_stream->picture.temp_ref);
  }

  if ((vid_stream->picture.code_type == B_TYPE) &&
      ((vid_stream->future == NULL) ||
       ((vid_stream->past == NULL) && !(vid_stream->group.closed_gop)))) {
    /* According to 2-D.5.1 (p D-18) this is ok, if the refereneces are OK (bkwd from I) */
    last_bit_count = bitCountRead();
    vid_stream->group.code_types[vid_stream->picture.temp_ref] = SKIP_B_TYPE;
    return SKIP_PICTURE;
  }
  
  if ((vid_stream->picture.code_type == P_TYPE) &&
      (vid_stream->future == NULL)) {
    last_bit_count=bitCountRead();
    vid_stream->group.code_types[vid_stream->picture.temp_ref] = SKIP_P_TYPE;
    return SKIP_PICTURE;
  }

  StartTime();
  
  /* If P or B type frame... */

  if ((vid_stream->picture.code_type == 2) || 
      (vid_stream->picture.code_type == 3)) {

    /* Parse off forward vector full pixel flag. */
    get_bits1(data);
    if (data)
      vid_stream->picture.full_pel_forw_vector = TRUE;
    else
      vid_stream->picture.full_pel_forw_vector = FALSE;

    /* Parse of forw_r_code. */
    get_bits3(data);
    f_code_ok &= (data <= 4);

    /* Decode forw_r_code into forw_r_size and forw_f. */

    vid_stream->picture.forw_r_size = data - 1;
    vid_stream->picture.forw_f = (1 << vid_stream->picture.forw_r_size);
  }


  if (COLLECTING) {
    stat_a[0].frametype = vid_stream->picture.code_type;
    stat_a[0].totsize = 45;
    stat_a[0].number = 1;
    pictureSizeCount = bitCountRead();
    if (opts&COLLECTING&BLOCK_INFO) {
      blks.slice = 0; blks.block = 0;
      if (stat_a[0].frametype != 1) /* not an I */
	fprintf(block_fp,"frame %d %c %s %d\n", 
		blks.frame,
		"0IPB"[stat_a[0].frametype],
		(vid_stream->picture.full_pel_forw_vector?"full":"half"),
		vid_stream->picture.temp_ref);
      else fprintf(block_fp,"frame %d I none %d\n", 
		   blks.frame,
		   vid_stream->picture.temp_ref);

    }
  }


  /* If B type frame... */

  if (vid_stream->picture.code_type == 3) {

    /* Parse off back vector full pixel flag. */
    get_bits1(data);
    if (data)
      vid_stream->picture.full_pel_back_vector = TRUE;
    else
      vid_stream->picture.full_pel_back_vector = FALSE;

    /* Parse off back_r_code. */
    get_bits3(data);
    f_code_ok &= (data <= 4);

    /* Decode back_r_code into back_r_size and back_f. */

    vid_stream->picture.back_r_size = data - 1;
    vid_stream->picture.back_f = (1 << vid_stream->picture.back_r_size);
  }
  /* Get extra bit picture info. */

  if (vid_stream->picture.extra_info != NULL) {
    free(vid_stream->picture.extra_info);
    vid_stream->picture.extra_info = NULL;
  }
  vid_stream->picture.extra_info = get_extra_bit_info();

  /* Goto next start code. */
  next_start_code();

  /* If start code is extension start code, parse off extension data. */

  if (next_bits(32, EXT_START_CODE)) {
    flush_bits32;

    if (vid_stream->picture.ext_data != NULL) {
      free(vid_stream->picture.ext_data);
      vid_stream->picture.ext_data = NULL;
    }
    vid_stream->picture.ext_data = get_ext_data(&vid_stream->picture.ext_size);
  }
  /* If start code is user start code, parse off user data. */

  if (next_bits(32, USER_START_CODE)) {
    flush_bits32;

    if (vid_stream->picture.user_data != NULL) {
      free(vid_stream->picture.user_data);
      vid_stream->picture.user_data = NULL;
    }
    vid_stream->picture.user_data = get_ext_data(&vid_stream->picture.user_size);
    if ( vid_stream->group.user_size > 0 && opts&COLLECTING&USERDAT_INFO) {
      fprintf(userdat_fp, "Picture Header (%d) user data:\n", blks.frame);
      print_binary(userdat_fp, vid_stream->user_data, vid_stream->user_size);
    }
  }
  /* Find a pict image structure in ring buffer not currently locked. */

  i = 0;

  while (vid_stream->ring[i]->locked != 0) {
    if (++i >= RING_BUF_SIZE) {
      fprintf(stderr,"Fatal error. Ring buffer full.");
      exit(1);
    }
  }

  /* Set current pict image structure to the one just found in ring. */

  vid_stream->current = vid_stream->ring[i];

  /* Set time stamp. */

  vid_stream->current->show_time = time_stamp;

  /* Reset past macroblock address field. */

  vid_stream->mblock.past_mb_addr = -1;

  last_bit_count = bitCountRead();
  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ParseSlice --
 *
 *      Parses off slice header.
 *
 * Results:
 *      Values found in slice header put into video stream structure.
 *
 * Side effects:
 *      Bit stream irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

static int
ParseSlice(vid_stream)
  VidStream *vid_stream;
{
  unsigned int data;

  /* Flush slice start code. */

  flush_bits(24);

  /* Parse off slice vertical position. */

  get_bits8(data);
  vid_stream->slice.vert_pos = data;

  /* Parse off quantization scale. */

  get_bits5(data);
  vid_stream->slice.quant_scale = data;
  if (opts&COLLECTING&BLOCK_INFO) {
    blks.slice++;
    blks.qs=data;
    fprintf(block_fp,"slice %d %d\n",blks.slice,data);
  }


  if (opts&COLLECTING&OFFS_INFO) {
    fprintf(offs_fp,"slice %d (%d)\n",bitCountRead()-37,vid_stream->slice.vert_pos);
  }

  /* Parse off extra bit slice info. */

  if (vid_stream->slice.extra_info != NULL) {
    free(vid_stream->slice.extra_info);
    vid_stream->slice.extra_info = NULL;
  }
  vid_stream->slice.extra_info = get_extra_bit_info();

  /* Reset past intrablock address. */

  vid_stream->mblock.past_intra_addr = -2;

  /* Reset previous recon motion vectors. */

  vid_stream->mblock.recon_right_for_prev = 0;
  vid_stream->mblock.recon_down_for_prev = 0;
  vid_stream->mblock.recon_right_back_prev = 0;
  vid_stream->mblock.recon_down_back_prev = 0;

  /* Reset macroblock address. */
  vid_stream->mblock.mb_address = ((vid_stream->slice.vert_pos - 1) * 
				   vid_stream->mb_width) - 1;

  /* Reset past dct dc y, cr, and cb values. */

  vid_stream->block.dct_dc_y_past = 1024;
  vid_stream->block.dct_dc_cr_past = 1024;
  vid_stream->block.dct_dc_cb_past = 1024;

  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ParseMacroBlock --
 *
 *      Parseoff macroblock. Reconstructs DCT values. Applies
 *      inverse DCT, reconstructs motion vectors, calculates and
 *      set pixel values for macroblock in current pict image
 *      structure.
 *
 * Results:
 *      Here's where everything really happens. Welcome to the
 *      heart of darkness.
 *
 * Side effects:
 *      Bit stream irreversibly parsed off.
 *
 *--------------------------------------------------------------
 */

static int
ParseMacroBlock(vid_stream)
  VidStream *vid_stream;
{
  int addr_incr;
  unsigned int data;
  int mask, i, recon_right_for, recon_down_for, recon_right_back,
      recon_down_back;
  int zero_block_flag;
  BOOLEAN mb_quant, mb_motion_forw, mb_motion_back, mb_pattern;
  int no_dith_flag = 0;
  static char mbtyp[20];
  int result;
  unsigned int mb_scratch; /* used in Decode macros */

  mbSizeCount = bitCountRead();

  /*
   * Parse off macroblock address increment and add to macroblock address.
   */
  do {
    unsigned int index;				       
    show_bits11(index);				       
    addr_incr = mb_addr_inc[index].value;	
    flush_bits(mb_addr_inc[index].num_bits);	 
    if (mb_addr_inc[index].num_bits == 0) {

      /* Error in table lookup! */
      sprintf(errorSpecifics, 
	      "\n\tthe macroblock increment was bad (currently at byte offset %d)", 
	      bitCountRead()/8);
      return SKIP_TO_START_CODE;
    }
    if (addr_incr == MB_ESCAPE) {
      vid_stream->mblock.mb_address += 33;
      addr_incr = MB_STUFFING;
    }
  } while (addr_incr == MB_STUFFING);
  vid_stream->mblock.mb_address += addr_incr;

  if (vid_stream->mblock.mb_address > (vid_stream->mb_height *
				       vid_stream->mb_width - 1)) {
    sprintf(errorSpecifics,"\nMB address is %d, height is %d, width %d.\n",
	    vid_stream->mblock.mb_address,
	    vid_stream->mb_height,vid_stream->mb_width);
    return SKIP_TO_START_CODE;
  }

  /*
   * If macroblocks have been skipped, process skipped macroblocks.
   */

  if (vid_stream->mblock.mb_address - vid_stream->mblock.past_mb_addr > 1) {
    blks.mb_skipped += vid_stream->mblock.mb_address - vid_stream->mblock.past_mb_addr-1;
    if (vid_stream->picture.code_type == P_TYPE)
      ProcessSkippedPFrameMBlocks(vid_stream);
    else if (vid_stream->picture.code_type == B_TYPE)
      ProcessSkippedBFrameMBlocks(vid_stream);
  } else blks.mb_coded++;
  mbSizeCount = bitCountRead();

  /* Set past macroblock address to current macroblock address. */
  if ((vid_stream->picture.code_type == I_TYPE) &&
      (vid_stream->mblock.past_mb_addr+1 != vid_stream->mblock.mb_address)) {
    fprintf(stderr,"Missing data in frame %d:\n",blks.frame);
    fprintf(stderr,"\tAt MB %d, previous MB was %d\n", vid_stream->mblock.mb_address,
	    vid_stream->mblock.past_mb_addr);
  }
  vid_stream->mblock.past_mb_addr = vid_stream->mblock.mb_address;

  /* Based on picture type decode macroblock type. */
  switch (vid_stream->picture.code_type) {
  case I_TYPE:
    DecodeMBTypeI(mb_quant, mb_motion_forw, mb_motion_back, mb_pattern,
		  vid_stream->mblock.mb_intra);
    break;

  case P_TYPE:
    DecodeMBTypeP(mb_quant, mb_motion_forw, mb_motion_back, mb_pattern,
		  vid_stream->mblock.mb_intra);
    break;

  case B_TYPE:
    DecodeMBTypeB(mb_quant, mb_motion_forw, mb_motion_back, mb_pattern,
		  vid_stream->mblock.mb_intra);
    break;
  }

  /* If quantization flag set, parse off new quantization scale. */

  if (mb_quant == TRUE) {
    get_bits5(data);
    vid_stream->slice.quant_scale = data;
  }
  stat_a[vid_stream->picture.code_type].qual += vid_stream->slice.quant_scale;
  stat_a[vid_stream->picture.code_type].qnum++;
  if (opts&COLLECTING&BLOCK_INFO) {
    blks.qs=vid_stream->slice.quant_scale;
    blks.block++;
    /* Clear, so we dont get old data later */
    *dctSpecifics='\0';
  }
  if (opts&COLLECTING&QSCALE_INFO) {
    blks.q[vid_stream->picture.code_type][vid_stream->slice.quant_scale]++;
  }
  
  /* If forward motion vectors exist... */
  if (mb_motion_forw == TRUE) {

    /* Parse off and decode horizontal forward motion vector. */
    DecodeMotionVectors(vid_stream->mblock.motion_h_forw_code);

    /* If horiz. forward r data exists, parse off. */

    if ((vid_stream->picture.forw_f != 1) &&
	(vid_stream->mblock.motion_h_forw_code != 0)) {
      get_bitsn(vid_stream->picture.forw_r_size, data);
      vid_stream->mblock.motion_h_forw_r = data;
    }
    /* Parse off and decode vertical forward motion vector. */
    DecodeMotionVectors(vid_stream->mblock.motion_v_forw_code);

    /* If vert. forw. r data exists, parse off. */

    if ((vid_stream->picture.forw_f != 1) &&
	(vid_stream->mblock.motion_v_forw_code != 0)) {
      get_bitsn(vid_stream->picture.forw_r_size, data);
      vid_stream->mblock.motion_v_forw_r = data;
    }
  }
  /* If back motion vectors exist... */
  if (mb_motion_back == TRUE) {

    /* Parse off and decode horiz. back motion vector. */
    DecodeMotionVectors(vid_stream->mblock.motion_h_back_code);

    /* If horiz. back r data exists, parse off. */

    if ((vid_stream->picture.back_f != 1) &&
	(vid_stream->mblock.motion_h_back_code != 0)) {
      get_bitsn(vid_stream->picture.back_r_size, data);
      vid_stream->mblock.motion_h_back_r = data;
    }
    /* Parse off and decode vert. back motion vector. */
    DecodeMotionVectors(vid_stream->mblock.motion_v_back_code);

    /* If vert. back r data exists, parse off. */

    if ((vid_stream->picture.back_f != 1) &&
	(vid_stream->mblock.motion_v_back_code != 0)) {
      get_bitsn(vid_stream->picture.back_r_size, data);
      vid_stream->mblock.motion_v_back_r = data;
    }
  }
  if (COLLECTING) {
    if (vid_stream->mblock.mb_intra) {
      stat_a[0].i_mbnum++;
      mbCBPPtr = stat_a[0].i_mbcbp;
      mbCoeffPtr = stat_a[0].i_mbcoeff;
      mbSizePtr = &(stat_a[0].i_mbsize);
    } else if (mb_motion_back && mb_motion_forw) {
      stat_a[0].bi_mbnum++;
      mbCBPPtr = stat_a[0].bi_mbcbp;
      mbCoeffPtr = stat_a[0].bi_mbcoeff;
      mbSizePtr = &(stat_a[0].bi_mbsize);
    } else if (mb_motion_back) {
      stat_a[0].b_mbnum++;
      mbCBPPtr = stat_a[0].b_mbcbp;
      mbCoeffPtr = stat_a[0].b_mbcoeff;
      mbSizePtr = &(stat_a[0].b_mbsize);
    } else {
      stat_a[0].p_mbnum++;
      mbCBPPtr = stat_a[0].p_mbcbp;
      mbCoeffPtr = stat_a[0].p_mbcoeff;
      mbSizePtr = &(stat_a[0].p_mbsize);
    }
  }

  /* If mblock pattern flag set, parse and decode CBP (code block pattern). */
  if (mb_pattern == TRUE) {
    DecodeCBP(vid_stream->mblock.cbp);
  }
  /* Otherwise, set CBP to zero. */
  else
    vid_stream->mblock.cbp = 0;


  if (COLLECTING) mbCBPPtr[vid_stream->mblock.cbp]++;

  /* Reconstruct motion vectors depending on picture type. */
  if (vid_stream->picture.code_type == P_TYPE) {

    /*
     * If no forw motion vectors, reset previous and current vectors to 0.
     */

    if (!mb_motion_forw) {
      recon_right_for = 0;
      recon_down_for = 0;
      vid_stream->mblock.recon_right_for_prev = 0;
      vid_stream->mblock.recon_down_for_prev = 0;
    }
    /*
     * Otherwise, compute new forw motion vectors. Reset previous vectors to
     * current vectors.
     */

    else {
      ComputeForwVector(&recon_right_for, &recon_down_for);
    }
  }
  if (vid_stream->picture.code_type == B_TYPE) {

    /* Reset prev. and current vectors to zero if mblock is intracoded. */

    if (vid_stream->mblock.mb_intra) {
      vid_stream->mblock.recon_right_for_prev = 0;
      vid_stream->mblock.recon_down_for_prev = 0;
      vid_stream->mblock.recon_right_back_prev = 0;
      vid_stream->mblock.recon_down_back_prev = 0;
    } else {

      /* If no forw vectors, current vectors equal prev. vectors. */

      if (!mb_motion_forw) {
	recon_right_for = vid_stream->mblock.recon_right_for_prev;
	recon_down_for = vid_stream->mblock.recon_down_for_prev;
      }
      /*
       * Otherwise compute forw. vectors. Reset prev vectors to new values.
       */

      else {
        ComputeForwVector(&recon_right_for, &recon_down_for);
      }

      /* If no back vectors, set back vectors to prev back vectors. */

      if (!mb_motion_back) {
	recon_right_back = vid_stream->mblock.recon_right_back_prev;
	recon_down_back = vid_stream->mblock.recon_down_back_prev;
      }
      /* Otherwise compute new vectors and reset prev. back vectors. */

      else {
	ComputeBackVector(&recon_right_back, &recon_down_back);
      }

      /*
       * Store vector existance flags in structure for possible skipped
       * macroblocks to follow.
       */

      vid_stream->mblock.bpict_past_forw = mb_motion_forw;
      vid_stream->mblock.bpict_past_back = mb_motion_back;
    }
  }

  if (COLLECTING&opts&BLOCK_INFO) {
    if (vid_stream->mblock.mb_intra) {
      sprintf(mbtyp,"intra");
    } else if (mb_motion_back && mb_motion_forw) {
      sprintf(mbtyp,"forw+back <%d, %d> <%d, %d>",
	      recon_right_for, recon_down_for,
	      recon_right_back, recon_down_back);
    } else if (mb_motion_back) {
      sprintf(mbtyp,"back <%d, %d>", recon_right_back, recon_down_back);
    } else if (mb_motion_forw) {
      sprintf(mbtyp,"forw <%d, %d>", recon_right_for, recon_down_for);
    } else 

      sprintf(mbtyp,"0 motion, %s",
	      mb_quant ? "Quant and cbp" : "cbp");
#ifdef not_defined
    /* old code to print all flags, silly really */
    sprintf(mbtyp,"No motion? %1u%1u%1u%1u%1u (%x, %x, %x)",
	    mb_quant, mb_motion_forw, mb_motion_back, mb_pattern,
	    vid_stream->mblock.mb_intra,(mb_pattern?mb_scratch:0xffffffff),
	    coded_block_pattern[mb_scratch].cbp,
	    coded_block_pattern[mb_scratch].num_bits);
#endif
  }

  /* For each possible block in macroblock. */
{
   {
      for (mask = 32, i = 0; i < 6; mask >>= 1, i++) {

	/* If block exists... */
	if (vid_stream->mblock.mb_intra) {
	  zero_block_flag = 0;
	  if ((opts&VERIFY) || (opts&COLLECTING&HIST_INFO) ||
	      (opts&COLLECTING&DCT_INFO)) {
	    if ((result=ParseReconBlock(i))!=PARSE_OK) 
	      return result;
	  } else ParseAwayBlock(i);
	} else {
	  if (vid_stream->mblock.cbp & mask) {
	    blks.cblks++;
	    blks.chist[i]++;
	    zero_block_flag = 0;
	    if ((opts&VERIFY) || (opts&COLLECTING&HIST_INFO) ||
		(opts&COLLECTING&DCT_INFO)) {
	      if ((result=ParseReconBlock(i))!=PARSE_OK) 
		return result;
	    } else ParseAwayBlock(i);
	  } else {
	    zero_block_flag = 1;	    
	  }
	  blks.nblks++;
	}
	if (opts&VERIFY) { /* No need to do this if not verifying */
	  if (vid_stream->mblock.mb_intra) {
	    int result;
	    if ((result=ReconIMBlock(vid_stream, i)) != PARSE_OK) 
	      return result;
	  } else if (mb_motion_forw && mb_motion_back) {
	    ReconBiMBlock(vid_stream, i, recon_right_for, recon_down_for,
			  recon_right_back, recon_down_back, zero_block_flag);
	  } else if (mb_motion_forw || (vid_stream->picture.code_type == P_TYPE)) {
	    ReconPMBlock(vid_stream, i, recon_right_for, recon_down_for,
			 zero_block_flag);
	  } else if (mb_motion_back) {
	    ReconBMBlock(vid_stream, i, recon_right_back, recon_down_back,
			 zero_block_flag);
	  }
	}
      }
    }
  }

  if ((ditherType == MBORDERED_DITHER) && (!no_dith_flag)) {
    if ((vid_stream->picture.code_type == 2) &&
	(vid_stream->mblock.cbp == 0) &&
	(!vid_stream->mblock.mb_intra)) {

      ditherFlags[vid_stream->mblock.mb_address] = 0;
    }
    else {
      ditherFlags[vid_stream->mblock.mb_address] = 1;
    }
  }


  /* If D Type picture, flush marker bit. */
  if (vid_stream->picture.code_type == 4)
    flush_bits(1);

  /* If macroblock was intracoded, set macroblock past intra address. */
  if (vid_stream->mblock.mb_intra)
    vid_stream->mblock.past_intra_addr =
      vid_stream->mblock.mb_address;

  if (opts&COLLECTING&BLOCK_INFO) {
    if ((vid_stream->mblock.mb_intra) || (!mb_pattern))
      fprintf(block_fp,"block %d %c %d %d %s%s\n",
	      blks.block-1,"0IPBD"[vid_stream->picture.code_type],
	      blks.qs, bitCountRead() - mbSizeCount, mbtyp,
	      dctSpecifics /* null if not collecting them */
	      );
    else  {
	fprintf(block_fp,"block %d %c %d %d %s %1u%1u%1u%1u%1u%1u%s\n",
		blks.block-1,
		"0IPBD"[vid_stream->picture.code_type],
		blks.qs, bitCountRead() - mbSizeCount, mbtyp,
		(vid_stream->mblock.cbp&0x20)>>5,
		(vid_stream->mblock.cbp&0x10)>>4,
		(vid_stream->mblock.cbp&0x08)>>3,
		(vid_stream->mblock.cbp&0x04)>>2,
		(vid_stream->mblock.cbp&0x02)>>1,
		vid_stream->mblock.cbp&0x01,
		dctSpecifics /* null if not collecting them */
		);
      }
  }
  if (COLLECTING) *mbSizePtr += bitCountRead() - mbSizeCount;
  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ReconIMBlock --
 *
 *	Reconstructs intra coded macroblock.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

#define video_assert(x,expression)\
  if (!(expression)) {\
  sprintf (errorSpecifics,"\nBad crop value (%d) at line %d\n", x, __LINE__);\
  return SKIP_TO_START_CODE;}
#define assertCrop(x)	video_assert(x,((x) >= -MAX_NEG_CROP) && \
				 ((x) <= 2048+MAX_NEG_CROP))

static int
ReconIMBlock(vid_stream, bnum)
  VidStream *vid_stream;
  int bnum;
{
  int mb_row, mb_col, row, col, row_size, rr;
  unsigned char *dest;

  /* Calculate macroblock row and column from address. */

  mb_row = vid_stream->mblock.mb_address / vid_stream->mb_width;
  mb_col = vid_stream->mblock.mb_address % vid_stream->mb_width;


  /* If block is luminance block... */

  if (bnum < 4) {

    /* Calculate row and col values for upper left pixel of block. */

    row = mb_row * 16;
    col = mb_col * 16;
    if (bnum > 1)
      row += 8;
    if (bnum % 2)
      col += 8;

    /* Set dest to luminance plane of current pict image. */

    dest = vid_stream->current->luminance;

    /* Establish row size. */

    row_size = vid_stream->mb_width * 16;
  }
  /* Otherwise if block is Cb block... */

  else if (bnum == 4) {

    /* Set dest to Cb plane of current pict image. */

    dest = vid_stream->current->Cb;

    /* Establish row size. */

    row_size = vid_stream->mb_width * 8;

    /* Calculate row,col for upper left pixel of block. */

    row = mb_row * 8;
    col = mb_col * 8;
  }
  /* Otherwise block is Cr block, and ... */

  else {

    /* Set dest to Cr plane of current pict image. */

    dest = vid_stream->current->Cr;

    /* Establish row size. */

    row_size = vid_stream->mb_width * 8;

    /* Calculate row,col for upper left pixel value of block. */

    row = mb_row * 8;
    col = mb_col * 8;
  }

  /*
   * For each pixel in block, set to cropped reconstructed value from inverse
   * dct.
   */
  {
    short *sp = &vid_stream->block.dct_recon[0][0];
    unsigned char *cm = cropTbl + MAX_NEG_CROP;
    dest += row * row_size + col;
    for (rr = 0; rr < 4; rr++, sp += 16, dest += row_size) {
      dest[0] = cm[sp[0]];
	  assertCrop(sp[0]);
      dest[1] = cm[sp[1]];
      assertCrop(sp[1]);
      dest[2] = cm[sp[2]];
      assertCrop(sp[2]);
      dest[3] = cm[sp[3]];
      assertCrop(sp[3]);
      dest[4] = cm[sp[4]];
      assertCrop(sp[4]);
      dest[5] = cm[sp[5]];
      assertCrop(sp[5]);
      dest[6] = cm[sp[6]];
      assertCrop(sp[6]);
      dest[7] = cm[sp[7]];
      assertCrop(sp[7]);

      dest += row_size;
      dest[0] = cm[sp[8]];
      assertCrop(sp[8]);
      dest[1] = cm[sp[9]];
      assertCrop(sp[9]);
      dest[2] = cm[sp[10]];
      assertCrop(sp[10]);
      dest[3] = cm[sp[11]];
      assertCrop(sp[11]);
      dest[4] = cm[sp[12]];
      assertCrop(sp[12]);
      dest[5] = cm[sp[13]];
      assertCrop(sp[13]);
      dest[6] = cm[sp[14]];
      assertCrop(sp[14]);
      dest[7] = cm[sp[15]];
      assertCrop(sp[15]);
    }
  }
  return PARSE_OK;
}



/*
 *--------------------------------------------------------------
 *
 * ReconPMBlock --
 *
 *	Reconstructs forward predicted macroblocks.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void
ReconPMBlock(vid_stream, bnum, recon_right_for, recon_down_for, zflag)
  VidStream *vid_stream;
  int bnum, recon_right_for, recon_down_for, zflag;
{
  int mb_row, mb_col, row, col, row_size, rr;
  unsigned char *dest, *past;
  static int right_for, down_for, right_half_for, down_half_for;
  unsigned char *rindex1, *rindex2;
  unsigned char *index;
  short int *blockvals;
  int maxx, maxy, cc;
  int illegalBlock = 0;
  int row_start, row_end, rfirst, rlast, col_start, col_end, cfirst, clast;

  /* Calculate macroblock row and column from address. */

  mb_row = vid_stream->mblock.mb_address / vid_stream->mb_width;
  mb_col = vid_stream->mblock.mb_address % vid_stream->mb_width;

  if (bnum < 4) {

    /* Calculate right_for, down_for motion vectors. */

    right_for = recon_right_for >> 1;
    down_for = recon_down_for >> 1;
    right_half_for = recon_right_for & 0x1;
    down_half_for = recon_down_for & 0x1;

    /* Set dest to luminance plane of current pict image. */

    dest = vid_stream->current->luminance;

    if (vid_stream->picture.code_type == B_TYPE) {
      if (vid_stream->past != NULL)
	past = vid_stream->past->luminance;
    } else {

      /* Set predicitive frame to current future frame. */

      if (vid_stream->future != NULL)
	past = vid_stream->future->luminance;
    }

    /* Establish row size. */

    row_size = vid_stream->mb_width << 4;

    /* Calculate row,col of upper left pixel in block. */

    row = mb_row << 4;
    col = mb_col << 4;
    if (bnum > 1)
      row += 8;
    if (bnum % 2)
      col += 8;

    /* Check for block illegality. */

    maxx = lmaxx; maxy = lmaxy;

    if (row + down_for > maxy) illegalBlock |= 0x4;
    else if (row + down_for < 0)  illegalBlock |= 0x1;
    
    if (col + right_for > maxx) illegalBlock |= 0x2;
    else if (col + right_for < 0) illegalBlock |= 0x8;
    if (illegalBlock) {
      fprintf(stderr,"Illegal vector in luminance forward-reconstruction of <%d, %d>\n\
               frame %d, macro block %d, block %d\n",
	      row + down_for, col + right_for, blks.frame,
	      vid_stream->mblock.mb_address, bnum);
    }
  }
  /* Otherwise, block is NOT luminance block, ... */

  else {

    /* Construct motion vectors. */

    recon_right_for /= 2;
    recon_down_for /= 2;
    right_for = recon_right_for >> 1;
    down_for = recon_down_for >> 1;
    right_half_for = recon_right_for & 0x1;
    down_half_for = recon_down_for & 0x1;

    /* Establish row size. */

    row_size = vid_stream->mb_width << 3;

    /* Calculate row,col of upper left pixel in block. */

    row = mb_row << 3;
    col = mb_col << 3;

    /* Check for block illegality. */

    maxx = cmaxx; maxy = cmaxy; 

    if (row + down_for > maxy) illegalBlock |= 0x4;
    else if (row + down_for < 0) illegalBlock |= 0x1;

    if (col + right_for > maxx) illegalBlock  |= 0x2;
    else if (col + right_for < 0) illegalBlock |= 0x8;
    if (illegalBlock) {
      fprintf(stderr,"Illegal vector in Cr/Cb forward-reconstruction of <%d, %d>\n\
               frame %d, macro block %d, block %d\n",
	      row + down_for, col + right_for, blks.frame,
	      vid_stream->mblock.mb_address, bnum);
    }

    /* If block is Cr block... */

    if (bnum == 4) {

      /* Set dest to Cr plane of current pict image. */

      dest = vid_stream->current->Cr;

      if (vid_stream->picture.code_type == B_TYPE) {

	if (vid_stream->past != NULL)
	  past = vid_stream->past->Cr;
      } else {
	if (vid_stream->future != NULL)
	  past = vid_stream->future->Cr;
      }
    }
    /* Otherwise, block is Cb block... */

    else {

      /* Set dest to Cb plane of current pict image. */

      dest = vid_stream->current->Cb;

      if (vid_stream->picture.code_type == B_TYPE) {
	if (vid_stream->past != NULL)
	  past = vid_stream->past->Cb;
      } else {
	if (vid_stream->future != NULL)
	  past = vid_stream->future->Cb;
      }
    }
  }

  /* For each pixel in block... */


    index = dest + (row * row_size) + col;
    rindex1 = past + (row + down_for) * row_size + col + right_for;
    
    blockvals = &(vid_stream->block.dct_recon[0][0]);
    
    /*
     * Calculate predictive pixel value based on motion vectors and copy to
     * dest plane.
     */
    
    if ((!down_half_for) && (!right_half_for)) {
      unsigned char *cm = cropTbl + MAX_NEG_CROP;
      if (!zflag)
	for (rr = 0; rr < 4; rr++) {
	  index[0] = cm[(int) rindex1[0] + (int) blockvals[0]];
	  index[1] = cm[(int) rindex1[1] + (int) blockvals[1]];
	  index[2] = cm[(int) rindex1[2] + (int) blockvals[2]];
	  index[3] = cm[(int) rindex1[3] + (int) blockvals[3]];
	  index[4] = cm[(int) rindex1[4] + (int) blockvals[4]];
	  index[5] = cm[(int) rindex1[5] + (int) blockvals[5]];
	  index[6] = cm[(int) rindex1[6] + (int) blockvals[6]];
	  index[7] = cm[(int) rindex1[7] + (int) blockvals[7]];
	  index += row_size;
	  rindex1 += row_size;
	  
	  index[0] = cm[(int) rindex1[0] + (int) blockvals[8]];
	  index[1] = cm[(int) rindex1[1] + (int) blockvals[9]];
	  index[2] = cm[(int) rindex1[2] + (int) blockvals[10]];
	  index[3] = cm[(int) rindex1[3] + (int) blockvals[11]];
	  index[4] = cm[(int) rindex1[4] + (int) blockvals[12]];
	  index[5] = cm[(int) rindex1[5] + (int) blockvals[13]];
	  index[6] = cm[(int) rindex1[6] + (int) blockvals[14]];
	  index[7] = cm[(int) rindex1[7] + (int) blockvals[15]];
	  blockvals += 16;
	  index += row_size;
	  rindex1 += row_size;
	}
      else {
	if (right_for & 0x1) {
	  /* No alignment, use bye copy */
	  for (rr = 0; rr < 4; rr++) {
	    index[0] = rindex1[0];
	    index[1] = rindex1[1];
	    index[2] = rindex1[2];
	    index[3] = rindex1[3];
	    index[4] = rindex1[4];
	    index[5] = rindex1[5];
	    index[6] = rindex1[6];
	    index[7] = rindex1[7];
	    index += row_size;
	    rindex1 += row_size;
	    
	    index[0] = rindex1[0];
	    index[1] = rindex1[1];
	    index[2] = rindex1[2];
	    index[3] = rindex1[3];
	    index[4] = rindex1[4];
	    index[5] = rindex1[5];
	    index[6] = rindex1[6];
	    index[7] = rindex1[7];
	    index += row_size;
	    rindex1 += row_size;
	  }
	} else if (right_for & 0x2) {
	  /* Half-word bit aligned, use 16 bit copy */
	  short *src = (short *)rindex1;
	  short *dest = (short *)index;
	  row_size >>= 1;
	  for (rr = 0; rr < 4; rr++) {
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest[2] = src[2];
	    dest[3] = src[3];
	    dest += row_size;
	    src += row_size;
	    
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest[2] = src[2];
	    dest[3] = src[3];
	    dest += row_size;
	    src += row_size;
	  }
	} else {
	  /* Word aligned, use 32 bit copy */
	  int *src = (int *)rindex1;
	  int *dest = (int *)index;
	  row_size >>= 2;
	  for (rr = 0; rr < 4; rr++) {
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest += row_size;
	    src += row_size;
	    
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest += row_size;
	    src += row_size;
	  }
	}
      }
    } else {
      unsigned char *cm = cropTbl + MAX_NEG_CROP;
      rindex2 = rindex1 + right_half_for + (down_half_for * row_size);
      if (!zflag)
	for (rr = 0; rr < 4; rr++) {
	  index[0] = cm[((int) (rindex1[0] + rindex2[0]) >> 1) + blockvals[0]];
	  index[1] = cm[((int) (rindex1[1] + rindex2[1]) >> 1) + blockvals[1]];
	  index[2] = cm[((int) (rindex1[2] + rindex2[2]) >> 1) + blockvals[2]];
	  index[3] = cm[((int) (rindex1[3] + rindex2[3]) >> 1) + blockvals[3]];
	  index[4] = cm[((int) (rindex1[4] + rindex2[4]) >> 1) + blockvals[4]];
	  index[5] = cm[((int) (rindex1[5] + rindex2[5]) >> 1) + blockvals[5]];
	  index[6] = cm[((int) (rindex1[6] + rindex2[6]) >> 1) + blockvals[6]];
	  index[7] = cm[((int) (rindex1[7] + rindex2[7]) >> 1) + blockvals[7]];
	  index += row_size;
	  rindex1 += row_size;
	  rindex2 += row_size;
	  
	  index[0] = cm[((int) (rindex1[0] + rindex2[0]) >> 1) + blockvals[8]];
	  index[1] = cm[((int) (rindex1[1] + rindex2[1]) >> 1) + blockvals[9]];
	  index[2] = cm[((int) (rindex1[2] + rindex2[2]) >> 1) + blockvals[10]];
	  index[3] = cm[((int) (rindex1[3] + rindex2[3]) >> 1) + blockvals[11]];
	  index[4] = cm[((int) (rindex1[4] + rindex2[4]) >> 1) + blockvals[12]];
	  index[5] = cm[((int) (rindex1[5] + rindex2[5]) >> 1) + blockvals[13]];
	  index[6] = cm[((int) (rindex1[6] + rindex2[6]) >> 1) + blockvals[14]];
	  index[7] = cm[((int) (rindex1[7] + rindex2[7]) >> 1) + blockvals[15]];
	  blockvals += 16;
	  index += row_size;
	  rindex1 += row_size;
	  rindex2 += row_size;
	}
      else
	for (rr = 0; rr < 4; rr++) {
	  index[0] = (int) (rindex1[0] + rindex2[0]) >> 1;
	  index[1] = (int) (rindex1[1] + rindex2[1]) >> 1;
	  index[2] = (int) (rindex1[2] + rindex2[2]) >> 1;
	  index[3] = (int) (rindex1[3] + rindex2[3]) >> 1;
	  index[4] = (int) (rindex1[4] + rindex2[4]) >> 1;
	  index[5] = (int) (rindex1[5] + rindex2[5]) >> 1;
	  index[6] = (int) (rindex1[6] + rindex2[6]) >> 1;
	  index[7] = (int) (rindex1[7] + rindex2[7]) >> 1;
	  index += row_size;
	  rindex1 += row_size;
	  rindex2 += row_size;
	  
	  index[0] = (int) (rindex1[0] + rindex2[0]) >> 1;
	  index[1] = (int) (rindex1[1] + rindex2[1]) >> 1;
	  index[2] = (int) (rindex1[2] + rindex2[2]) >> 1;
	  index[3] = (int) (rindex1[3] + rindex2[3]) >> 1;
	  index[4] = (int) (rindex1[4] + rindex2[4]) >> 1;
	  index[5] = (int) (rindex1[5] + rindex2[5]) >> 1;
	  index[6] = (int) (rindex1[6] + rindex2[6]) >> 1;
	  index[7] = (int) (rindex1[7] + rindex2[7]) >> 1;
	  index += row_size;
	  rindex1 += row_size;
	  rindex2 += row_size;
	}
    }
}


/*
 *--------------------------------------------------------------
 *
 * ReconBMBlock --
 *
 *	Reconstructs back predicted macroblocks.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void
ReconBMBlock(vid_stream, bnum, recon_right_back, recon_down_back, zflag)
  VidStream *vid_stream;
  int bnum, recon_right_back, recon_down_back, zflag;
{
  int mb_row, mb_col, row, col, row_size, rr;
  unsigned char *dest, *future;
  int right_back, down_back, right_half_back, down_half_back;
  unsigned char *rindex1, *rindex2;
  unsigned char *index;
  short int *blockvals;

  int illegalBlock = 0;
  int maxx, maxy, cc;
  int row_start, row_end, rlast, rfirst, col_start, col_end, clast, cfirst;

  /* Calculate macroblock row and column from address. */

  mb_row = vid_stream->mblock.mb_address / vid_stream->mb_width;
  mb_col = vid_stream->mblock.mb_address % vid_stream->mb_width;

  /* If block is luminance block... */

  if (bnum < 4) {

    /* Calculate right_back, down_bakc motion vectors. */

    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;

    /* Set dest to luminance plane of current pict image. */

    dest = vid_stream->current->luminance;

    /*
     * If future frame exists, set future to luminance plane of future frame.
     */

    if (vid_stream->future != NULL)
      future = vid_stream->future->luminance;

    /* Establish row size. */

    row_size = vid_stream->mb_width << 4;

    /* Calculate row,col of upper left pixel in block. */

    row = mb_row << 4;
    col = mb_col << 4;
    if (bnum > 1)
      row += 8;
    if (bnum % 2)
      col += 8;

    /* Check for block illegality. */

    maxx = lmaxx; maxy = lmaxy;

    if (row + down_back  > maxy) illegalBlock |= 0x4;
    else if (row + down_back < 0)  illegalBlock |= 0x1;
    
    if (col + right_back  > maxx) illegalBlock |= 0x2;
    else if (col + right_back < 0) illegalBlock |= 0x8;
    if (illegalBlock) {
      fprintf(stderr,"Illegal vector in luminance backward-reconstruction of <%d, %d>\n\
               frame %d, macro block %d, block %d\n",
	      row + down_back, col + right_back, blks.frame,
	      vid_stream->mblock.mb_address, bnum);
    }

  }
  /* Otherwise, block is NOT luminance block, ... */

  else {

    /* Construct motion vectors. */

    recon_right_back /= 2;
    recon_down_back /= 2;
    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;

    /* Establish row size. */

    row_size = vid_stream->mb_width << 3;

    /* Calculate row,col of upper left pixel in block. */

    row = mb_row << 3;
    col = mb_col << 3;


    /* Check for block illegality. */

    maxx = cmaxx; maxy = cmaxy;

    if (row + down_back  > maxy) illegalBlock |= 0x4;
    else if (row + down_back < 0) illegalBlock |= 0x1;

    if (col + right_back  > maxx) illegalBlock  |= 0x2;
    else if (col + right_back < 0) illegalBlock |= 0x8;
    if (illegalBlock) {
      fprintf(stderr,"Illegal vector in Cr/Cb backward-reconstruction of <%d, %d>\n\
               frame %d, macro block %d, block %d\n",
	      row + down_back, col + right_back, blks.frame,
	      vid_stream->mblock.mb_address, bnum);
    }

    /* If block is Cr block... */

    if (bnum == 4) {

      /* Set dest to Cr plane of current pict image. */

      dest = vid_stream->current->Cr;

      /*
       * If future frame exists, set future to Cr plane of future image.
       */

      if (vid_stream->future != NULL)
	future = vid_stream->future->Cr;
    }
    /* Otherwise, block is Cb block... */

    else {

      /* Set dest to Cb plane of current pict image. */

      dest = vid_stream->current->Cb;

      /*
       * If future frame exists, set future to Cb plane of future frame.
       */

      if (vid_stream->future != NULL)
	future = vid_stream->future->Cb;
    }
  }

  /* For each pixel in block do... */

    
    index = dest + (row * row_size) + col;
    rindex1 = future + (row + down_back) * row_size + col + right_back;

    blockvals = &(vid_stream->block.dct_recon[0][0]);

    if ((!right_half_back) && (!down_half_back)) {
      unsigned char *cm = cropTbl + MAX_NEG_CROP;
      if (!zflag)
	for (rr = 0; rr < 4; rr++) {
	  index[0] = cm[(int) rindex1[0] + (int) blockvals[0]];
	  index[1] = cm[(int) rindex1[1] + (int) blockvals[1]];
	  index[2] = cm[(int) rindex1[2] + (int) blockvals[2]];
	  index[3] = cm[(int) rindex1[3] + (int) blockvals[3]];
	  index[4] = cm[(int) rindex1[4] + (int) blockvals[4]];
	  index[5] = cm[(int) rindex1[5] + (int) blockvals[5]];
	  index[6] = cm[(int) rindex1[6] + (int) blockvals[6]];
	  index[7] = cm[(int) rindex1[7] + (int) blockvals[7]];
	  index += row_size;
	  rindex1 += row_size;
	  
	  index[0] = cm[(int) rindex1[0] + (int) blockvals[8]];
	  index[1] = cm[(int) rindex1[1] + (int) blockvals[9]];
	  index[2] = cm[(int) rindex1[2] + (int) blockvals[10]];
	  index[3] = cm[(int) rindex1[3] + (int) blockvals[11]];
	  index[4] = cm[(int) rindex1[4] + (int) blockvals[12]];
	  index[5] = cm[(int) rindex1[5] + (int) blockvals[13]];
	  index[6] = cm[(int) rindex1[6] + (int) blockvals[14]];
	  index[7] = cm[(int) rindex1[7] + (int) blockvals[15]];
	  blockvals += 16;
	  index += row_size;
	  rindex1 += row_size;
	}
      else {
	if (right_back & 0x1) {
	  /* No alignment, use bye copy */
	  for (rr = 0; rr < 4; rr++) {
	    index[0] = rindex1[0];
	    index[1] = rindex1[1];
	    index[2] = rindex1[2];
	    index[3] = rindex1[3];
	    index[4] = rindex1[4];
	    index[5] = rindex1[5];
	    index[6] = rindex1[6];
	    index[7] = rindex1[7];
	    index += row_size;
	    rindex1 += row_size;
	    
	    index[0] = rindex1[0];
	    index[1] = rindex1[1];
	    index[2] = rindex1[2];
	    index[3] = rindex1[3];
	    index[4] = rindex1[4];
	    index[5] = rindex1[5];
	    index[6] = rindex1[6];
	    index[7] = rindex1[7];
	    index += row_size;
	    rindex1 += row_size;
	  }
	} else if (right_back & 0x2) {
	  /* Half-word bit aligned, use 16 bit copy */
	  short *src = (short *)rindex1;
	  short *dest = (short *)index;
	  row_size >>= 1;
	  for (rr = 0; rr < 4; rr++) {
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest[2] = src[2];
	    dest[3] = src[3];
	    dest += row_size;
	    src += row_size;
	    
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest[2] = src[2];
	    dest[3] = src[3];
	    dest += row_size;
	    src += row_size;
	  }
	} else {
	  /* Word aligned, use 32 bit copy */
	  int *src = (int *)rindex1;
	  int *dest = (int *)index;
	  row_size >>= 2;
	  for (rr = 0; rr < 4; rr++) {
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest += row_size;
	    src += row_size;
	    
	    dest[0] = src[0];
	    dest[1] = src[1];
	    dest += row_size;
	    src += row_size;
	  }
	}
      }
    } else {
      unsigned char *cm = cropTbl + MAX_NEG_CROP;
      rindex2 = rindex1 + right_half_back + (down_half_back * row_size);
      if (!zflag)
	for (rr = 0; rr < 4; rr++) {
	  index[0] = cm[((int) (rindex1[0] + rindex2[0]) >> 1) + blockvals[0]];
	  index[1] = cm[((int) (rindex1[1] + rindex2[1]) >> 1) + blockvals[1]];
	  index[2] = cm[((int) (rindex1[2] + rindex2[2]) >> 1) + blockvals[2]];
	  index[3] = cm[((int) (rindex1[3] + rindex2[3]) >> 1) + blockvals[3]];
	  index[4] = cm[((int) (rindex1[4] + rindex2[4]) >> 1) + blockvals[4]];
	  index[5] = cm[((int) (rindex1[5] + rindex2[5]) >> 1) + blockvals[5]];
	  index[6] = cm[((int) (rindex1[6] + rindex2[6]) >> 1) + blockvals[6]];
	  index[7] = cm[((int) (rindex1[7] + rindex2[7]) >> 1) + blockvals[7]];
	  index += row_size;
	  rindex1 += row_size;
	  rindex2 += row_size;
	  
	  index[0] = cm[((int) (rindex1[0] + rindex2[0]) >> 1) + blockvals[8]];
	  index[1] = cm[((int) (rindex1[1] + rindex2[1]) >> 1) + blockvals[9]];
	  index[2] = cm[((int) (rindex1[2] + rindex2[2]) >> 1) + blockvals[10]];
	  index[3] = cm[((int) (rindex1[3] + rindex2[3]) >> 1) + blockvals[11]];
	  index[4] = cm[((int) (rindex1[4] + rindex2[4]) >> 1) + blockvals[12]];
	  index[5] = cm[((int) (rindex1[5] + rindex2[5]) >> 1) + blockvals[13]];
	  index[6] = cm[((int) (rindex1[6] + rindex2[6]) >> 1) + blockvals[14]];
	  index[7] = cm[((int) (rindex1[7] + rindex2[7]) >> 1) + blockvals[15]];
	  blockvals += 16;
	  index += row_size;
	  rindex1 += row_size;
	  rindex2 += row_size;
	}
      else
	for (rr = 0; rr < 4; rr++) {
	  index[0] = (int) (rindex1[0] + rindex2[0]) >> 1;
	  index[1] = (int) (rindex1[1] + rindex2[1]) >> 1;
	  index[2] = (int) (rindex1[2] + rindex2[2]) >> 1;
	  index[3] = (int) (rindex1[3] + rindex2[3]) >> 1;
	  index[4] = (int) (rindex1[4] + rindex2[4]) >> 1;
	  index[5] = (int) (rindex1[5] + rindex2[5]) >> 1;
	  index[6] = (int) (rindex1[6] + rindex2[6]) >> 1;
	  index[7] = (int) (rindex1[7] + rindex2[7]) >> 1;
	  index += row_size;
	  rindex1 += row_size;
	  rindex2 += row_size;
	  
	  index[0] = (int) (rindex1[0] + rindex2[0]) >> 1;
	  index[1] = (int) (rindex1[1] + rindex2[1]) >> 1;
	  index[2] = (int) (rindex1[2] + rindex2[2]) >> 1;
	  index[3] = (int) (rindex1[3] + rindex2[3]) >> 1;
	  index[4] = (int) (rindex1[4] + rindex2[4]) >> 1;
	  index[5] = (int) (rindex1[5] + rindex2[5]) >> 1;
	  index[6] = (int) (rindex1[6] + rindex2[6]) >> 1;
	  index[7] = (int) (rindex1[7] + rindex2[7]) >> 1;
	  index += row_size;
	  rindex1 += row_size;
	  rindex2 += row_size;
	}
    }
}


/*
 *--------------------------------------------------------------
 *
 * ReconBiMBlock --
 *
 *	Reconstructs bidirectionally predicted macroblocks.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void
ReconBiMBlock(vid_stream, bnum, recon_right_for, recon_down_for,
	      recon_right_back, recon_down_back, zflag)
  VidStream *vid_stream;
  int bnum, recon_right_for, recon_down_for, recon_right_back, recon_down_back;
  int zflag;
{
  int mb_row, mb_col, row, col, row_size, rr;
  unsigned char *dest, *past, *future;
  int right_for, down_for, right_half_for, down_half_for;
  int right_back, down_back, right_half_back, down_half_back;
  unsigned char *index, *rindex1, *bindex1;
  short int *blockvals;
  int forw_row_start, back_row_start, forw_col_start, back_col_start;

  /* Calculate macroblock row and column from address. */

  mb_row = vid_stream->mblock.mb_address / vid_stream->mb_width;
  mb_col = vid_stream->mblock.mb_address % vid_stream->mb_width;

  /* If block is luminance block... */

  if (bnum < 4) {

    /*
     * Calculate right_for, down_for, right_half_for, down_half_for,
     * right_back, down_bakc, right_half_back, and down_half_back, motion
     * vectors.
     */

    right_for = recon_right_for >> 1;
    down_for = recon_down_for >> 1;
    right_half_for = recon_right_for & 0x1;
    down_half_for = recon_down_for & 0x1;

    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;

    /* Set dest to luminance plane of current pict image. */

    dest = vid_stream->current->luminance;

    /* If past frame exists, set past to luminance plane of past frame. */

    if (vid_stream->past != NULL)
      past = vid_stream->past->luminance;

    /*
     * If future frame exists, set future to luminance plane of future frame.
     */

    if (vid_stream->future != NULL)
      future = vid_stream->future->luminance;

    /* Establish row size. */

    row_size = (vid_stream->mb_width << 4);

    /* Calculate row,col of upper left pixel in block. */

    row = (mb_row << 4);
    col = (mb_col << 4);
    if (bnum > 1)
      row += 8;
    if (bnum & 0x01)
      col += 8;

    forw_col_start = col + right_for;
    forw_row_start = row + down_for;

    back_col_start = col + right_back;
    back_row_start = row + down_back;

    /* Check for illegal pred. blocks. */
    
    if ((forw_col_start > lmaxx) || (forw_col_start < 0) ||
	(forw_row_start > lmaxy) || (forw_row_start < 0))
      fprintf(stderr,"Illegal luminance forward vector in Bi-reconstruction <%d, %d>\n\
              in frame %d, macro block %d, block %d\n",
	      forw_col_start, forw_row_start,blks.frame,
	      vid_stream->mblock.mb_address, bnum);
    
    if ((back_col_start > lmaxx) || (back_col_start < 0) ||
        (back_row_start > lmaxy) || (back_row_start < 0)) 
      fprintf(stderr,"Illegal luminance backward vector in Bi-reconstruction <%d, %d>\n\
              in frame %d, macro block %d, block %d\n",
	      forw_col_start, forw_row_start,blks.frame,
	      vid_stream->mblock.mb_address, bnum);

  /* Otherwise, block is NOT luminance block, ... */

  } else {

    /* Construct motion vectors. */

    recon_right_for /= 2;
    recon_down_for /= 2;
    right_for = recon_right_for >> 1;
    down_for = recon_down_for >> 1;
    right_half_for = recon_right_for & 0x1;
    down_half_for = recon_down_for & 0x1;

    recon_right_back /= 2;
    recon_down_back /= 2;
    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;

    /* Establish row size. */

    row_size = (vid_stream->mb_width << 3);

    /* Calculate row,col of upper left pixel in block. */

    row = (mb_row << 3);
    col = (mb_col << 3);

    forw_col_start = col + right_for;
    forw_row_start = row + down_for;

    back_col_start = col + right_back;
    back_row_start = row + down_back;


    /* Check for illegal pred. blocks. */

    if ((forw_col_start > cmaxx) || (forw_col_start < 0) ||
	(forw_row_start > cmaxy) || (forw_row_start < 0))
      fprintf(stderr,"Illegal Cr/Cb forward vector in Bi-reconstruction <%d, %d>\n\
              in frame %d, macro block %d, block %d\n",
	      forw_col_start, forw_row_start,blks.frame,
	      vid_stream->mblock.mb_address, bnum);

    if ((back_col_start > cmaxx) || (back_col_start < 0) ||
        (back_row_start > cmaxy) || (back_row_start < 0)) 
      fprintf(stderr,"Illegal Cr/Cb backward vector in Bi-reconstruction <%d, %d>\n\
              in frame %d, macro block %d, block %d\n",
	      forw_col_start, forw_row_start,blks.frame,
	      vid_stream->mblock.mb_address, bnum);


    
    /* If block is Cr block... */

    if (bnum == 4) {

      /* Set dest to Cr plane of current pict image. */

      dest = vid_stream->current->Cr;

      /* If past frame exists, set past to Cr plane of past image. */

      if (vid_stream->past != NULL)
	past = vid_stream->past->Cr;

      /*
       * If future frame exists, set future to Cr plane of future image.
       */

      if (vid_stream->future != NULL)
	future = vid_stream->future->Cr;
    }
    /* Otherwise, block is Cb block... */

    else {

      /* Set dest to Cb plane of current pict image. */

      dest = vid_stream->current->Cb;

      /* If past frame exists, set past to Cb plane of past frame. */

      if (vid_stream->past != NULL)
	past = vid_stream->past->Cb;

      /*
       * If future frame exists, set future to Cb plane of future frame.
       */

      if (vid_stream->future != NULL)
	future = vid_stream->future->Cb;
    }
  }

  /* For each pixel in block... */

  index = dest + (row * row_size) + col;

  rindex1 = past + forw_row_start  * row_size + forw_col_start;

  bindex1 = future + back_row_start * row_size + back_col_start;

  blockvals = (short int *) &(vid_stream->block.dct_recon[0][0]);

  {
  unsigned char *cm = cropTbl + MAX_NEG_CROP;
  if (!zflag)
    for (rr = 0; rr < 4; rr++) {
      index[0] = cm[((int) (rindex1[0] + bindex1[0]) >> 1) + blockvals[0]];
      index[1] = cm[((int) (rindex1[1] + bindex1[1]) >> 1) + blockvals[1]];
      index[2] = cm[((int) (rindex1[2] + bindex1[2]) >> 1) + blockvals[2]];
      index[3] = cm[((int) (rindex1[3] + bindex1[3]) >> 1) + blockvals[3]];
      index[4] = cm[((int) (rindex1[4] + bindex1[4]) >> 1) + blockvals[4]];
      index[5] = cm[((int) (rindex1[5] + bindex1[5]) >> 1) + blockvals[5]];
      index[6] = cm[((int) (rindex1[6] + bindex1[6]) >> 1) + blockvals[6]];
      index[7] = cm[((int) (rindex1[7] + bindex1[7]) >> 1) + blockvals[7]];
      index += row_size;
      rindex1 += row_size;
      bindex1 += row_size;

      index[0] = cm[((int) (rindex1[0] + bindex1[0]) >> 1) + blockvals[8]];
      index[1] = cm[((int) (rindex1[1] + bindex1[1]) >> 1) + blockvals[9]];
      index[2] = cm[((int) (rindex1[2] + bindex1[2]) >> 1) + blockvals[10]];
      index[3] = cm[((int) (rindex1[3] + bindex1[3]) >> 1) + blockvals[11]];
      index[4] = cm[((int) (rindex1[4] + bindex1[4]) >> 1) + blockvals[12]];
      index[5] = cm[((int) (rindex1[5] + bindex1[5]) >> 1) + blockvals[13]];
      index[6] = cm[((int) (rindex1[6] + bindex1[6]) >> 1) + blockvals[14]];
      index[7] = cm[((int) (rindex1[7] + bindex1[7]) >> 1) + blockvals[15]];
      blockvals += 16;
      index += row_size;
      rindex1 += row_size;
      bindex1 += row_size;
    }

  else
    for (rr = 0; rr < 4; rr++) {
      index[0] = (int) (rindex1[0] + bindex1[0]) >> 1;
      index[1] = (int) (rindex1[1] + bindex1[1]) >> 1;
      index[2] = (int) (rindex1[2] + bindex1[2]) >> 1;
      index[3] = (int) (rindex1[3] + bindex1[3]) >> 1;
      index[4] = (int) (rindex1[4] + bindex1[4]) >> 1;
      index[5] = (int) (rindex1[5] + bindex1[5]) >> 1;
      index[6] = (int) (rindex1[6] + bindex1[6]) >> 1;
      index[7] = (int) (rindex1[7] + bindex1[7]) >> 1;
      index += row_size;
      rindex1 += row_size;
      bindex1 += row_size;

      index[0] = (int) (rindex1[0] + bindex1[0]) >> 1;
      index[1] = (int) (rindex1[1] + bindex1[1]) >> 1;
      index[2] = (int) (rindex1[2] + bindex1[2]) >> 1;
      index[3] = (int) (rindex1[3] + bindex1[3]) >> 1;
      index[4] = (int) (rindex1[4] + bindex1[4]) >> 1;
      index[5] = (int) (rindex1[5] + bindex1[5]) >> 1;
      index[6] = (int) (rindex1[6] + bindex1[6]) >> 1;
      index[7] = (int) (rindex1[7] + bindex1[7]) >> 1;
      index += row_size;
      rindex1 += row_size;
      bindex1 += row_size;
    }
  }
}

/*
 *--------------------------------------------------------------
 *
 * ProcessSkippedPFrameMBlocks --
 *
 *	Processes skipped macroblocks in P frames.
 *
 * Results:
 *	Calculates pixel values for luminance, Cr, and Cb planes
 *      in current pict image for skipped macroblocks.
 *
 * Side effects:
 *	Pixel values in pict image changed.
 *
 *--------------------------------------------------------------
 */

static void
ProcessSkippedPFrameMBlocks(vid_stream)
  VidStream *vid_stream;
{
  int row_size, half_row, mb_row, mb_col, row, col, rr;
  int addr, row_incr, half_row_incr, crow, ccol;
  int *dest, *src, *dest1, *src1;

  /* Calculate row sizes for luminance and Cr/Cb macroblock areas. */

  row_size = vid_stream->mb_width << 4;
  half_row = (row_size >> 1);
  row_incr = row_size >> 2;
  half_row_incr = half_row >> 2;

  /* For each skipped macroblock, do... */

  for (addr = vid_stream->mblock.past_mb_addr + 1;
       addr < vid_stream->mblock.mb_address; addr++) {

    if (opts&COLLECTING&BLOCK_INFO) {
      blks.block++;
      fprintf(block_fp,"block %d %c %d 0 skip\n",
	      blks.block-1,"0IPBD"[vid_stream->picture.code_type],
	      blks.qs);
    }

    /* Calculate macroblock row and col. */
    mb_row = addr / vid_stream->mb_width;
    mb_col = addr % vid_stream->mb_width;

    /* Calculate upper left pixel row,col for luminance plane. */

    row = mb_row << 4;
    col = mb_col << 4;


    /* For each row in macroblock luminance plane... */

    dest = (int *)(vid_stream->current->luminance + (row * row_size) + col);
    src = (int *)(vid_stream->future->luminance + (row * row_size) + col);

    for (rr = 0; rr < 8; rr++) {

      /* Copy pixel values from last I or P picture. */

      dest[0] = src[0];
      dest[1] = src[1];
      dest[2] = src[2];
      dest[3] = src[3];
      dest += row_incr;
      src += row_incr;

      dest[0] = src[0];
      dest[1] = src[1];
      dest[2] = src[2];
      dest[3] = src[3];
      dest += row_incr;
      src += row_incr;
    }

    /*
     * Divide row,col to get upper left pixel of macroblock in Cr and Cb
     * planes.
     */

    crow = row >> 1;
    ccol = col >> 1;

    /* For each row in Cr, and Cb planes... */

    dest = (int *)(vid_stream->current->Cr + (crow * half_row) + ccol);
    src = (int *)(vid_stream->future->Cr + (crow * half_row) + ccol);
    dest1 = (int *)(vid_stream->current->Cb + (crow * half_row) + ccol);
    src1 = (int *)(vid_stream->future->Cb + (crow * half_row) + ccol);

    for (rr = 0; rr < 4; rr++) {

      /* Copy pixel values from last I or P picture. */

      dest[0] = src[0];
      dest[1] = src[1];

      dest1[0] = src1[0];
      dest1[1] = src1[1];

      dest += half_row_incr;
      src += half_row_incr;
      dest1 += half_row_incr;
      src1 += half_row_incr;

      dest[0] = src[0];
      dest[1] = src[1];

      dest1[0] = src1[0];
      dest1[1] = src1[1];

      dest += half_row_incr;
      src += half_row_incr;
      dest1 += half_row_incr;
      src1 += half_row_incr;
    }

    if (ditherType == MBORDERED_DITHER) {
	/*
      MBOrderedDitherDisplayCopy(vid_stream, addr,
				 1, 0, 0, 0, 0, 0,
				 vid_stream->future->display,
				 (unsigned char *) NULL);
	*/
      ditherFlags[addr] = 0;
    }
  }

  vid_stream->mblock.recon_right_for_prev = 0;
  vid_stream->mblock.recon_down_for_prev = 0;
}




/*
 *--------------------------------------------------------------
 *
 * ProcessSkippedBFrameMBlocks --
 *
 *	Processes skipped macroblocks in B frames.
 *
 * Results:
 *	Calculates pixel values for luminance, Cr, and Cb planes
 *      in current pict image for skipped macroblocks.
 *
 * Side effects:
 *	Pixel values in pict image changed.
 *
 *--------------------------------------------------------------
 */

static void
ProcessSkippedBFrameMBlocks(vid_stream)
  VidStream *vid_stream;
{
  int row_size, half_row, mb_row, mb_col, row, col, rr;
  int right_half_for, down_half_for, c_right_half_for, c_down_half_for;
  int right_half_back, down_half_back, c_right_half_back, c_down_half_back;
  int addr, right_for, down_for;
  int recon_right_for, recon_down_for;
  int recon_right_back, recon_down_back;
  int right_back, down_back;
  int c_right_for, c_down_for;
  int c_right_back, c_down_back;
  unsigned char forw_lum[256];
  unsigned char forw_cr[64], forw_cb[64];
  unsigned char back_lum[256], back_cr[64], back_cb[64];
  int row_incr, half_row_incr;
  int ccol, crow;

  /* Calculate row sizes for luminance and Cr/Cb macroblock areas. */

  row_size = vid_stream->mb_width << 4;
  half_row = (row_size >> 1);
  row_incr = row_size >> 2;
  half_row_incr =  half_row >> 2;

  /* Establish motion vector codes based on full pixel flag. */

  if (vid_stream->picture.full_pel_forw_vector) {
    recon_right_for = vid_stream->mblock.recon_right_for_prev << 1;
    recon_down_for = vid_stream->mblock.recon_down_for_prev << 1;
  } else {
    recon_right_for = vid_stream->mblock.recon_right_for_prev;
    recon_down_for = vid_stream->mblock.recon_down_for_prev;
  }

  if (vid_stream->picture.full_pel_back_vector) {
    recon_right_back = vid_stream->mblock.recon_right_back_prev << 1;
    recon_down_back = vid_stream->mblock.recon_down_back_prev << 1;
  } else {
    recon_right_back = vid_stream->mblock.recon_right_back_prev;
    recon_down_back = vid_stream->mblock.recon_down_back_prev;
  }


  /* Calculate motion vectors. */
  
  if (vid_stream->mblock.bpict_past_forw) {
    right_for = recon_right_for >> 1;
    down_for = recon_down_for >> 1;
    right_half_for = recon_right_for & 0x1;
    down_half_for = recon_down_for & 0x1;
    
    recon_right_for /= 2;
    recon_down_for /= 2;
    c_right_for = recon_right_for >> 1;
    c_down_for = recon_down_for >> 1;
    c_right_half_for = recon_right_for & 0x1;
    c_down_half_for = recon_down_for & 0x1;
    
  }
  if (vid_stream->mblock.bpict_past_back) {
    right_back = recon_right_back >> 1;
    down_back = recon_down_back >> 1;
    right_half_back = recon_right_back & 0x1;
    down_half_back = recon_down_back & 0x1;
    
    recon_right_back /= 2;
    recon_down_back /= 2;
    c_right_back = recon_right_back >> 1;
    c_down_back = recon_down_back >> 1;
    c_right_half_back = recon_right_back & 0x1;
    c_down_half_back = recon_down_back & 0x1;
    
  }
  /* For each skipped macroblock, do... */
  
  for (addr = vid_stream->mblock.past_mb_addr + 1;
       addr < vid_stream->mblock.mb_address; addr++) {

    if (opts&COLLECTING&BLOCK_INFO) {
      blks.block++;
      fprintf(block_fp,"block %d %c %d 0 skip\n",
	      blks.block-1,"0IPBD"[vid_stream->picture.code_type],
	      blks.qs);
    }

    /* Calculate macroblock row and col. */
    
    mb_row = addr / vid_stream->mb_width;
    mb_col = addr % vid_stream->mb_width;
    
    /* Calculate upper left pixel row,col for luminance plane. */
    
    row = mb_row << 4;
    col = mb_col << 4;
    crow = row / 2;
    ccol = col / 2;
    
    /* If forward predicted, calculate prediction values. */
    
    if (vid_stream->mblock.bpict_past_forw) {
      
      ReconSkippedBlock(vid_stream->past->luminance, forw_lum,
			row, col, row_size, right_for, down_for,
			right_half_for, down_half_for, 16);
      ReconSkippedBlock(vid_stream->past->Cr, forw_cr, crow,
			ccol, half_row,
			c_right_for, c_down_for, c_right_half_for, c_down_half_for, 8);
      ReconSkippedBlock(vid_stream->past->Cb, forw_cb, crow,
			ccol, half_row,
			c_right_for, c_down_for, c_right_half_for, c_down_half_for, 8);
    }
    /* If back predicted, calculate prediction values. */
    
    if (vid_stream->mblock.bpict_past_back) {
      ReconSkippedBlock(vid_stream->future->luminance, back_lum,
			row, col, row_size, right_back, down_back,
			right_half_back, down_half_back, 16);
      ReconSkippedBlock(vid_stream->future->Cr, back_cr, crow,
			ccol, half_row,
			c_right_back, c_down_back,
			c_right_half_back, c_down_half_back, 8);
      ReconSkippedBlock(vid_stream->future->Cb, back_cb, crow,
			ccol, half_row,
			c_right_back, c_down_back,
			c_right_half_back, c_down_half_back, 8);
    }
    if (vid_stream->mblock.bpict_past_forw &&
	!vid_stream->mblock.bpict_past_back) {
      
      int *dest, *dest1;
      int *src, *src1;
      dest = (int *)(vid_stream->current->luminance + (row * row_size) + col);
      src = (int *)forw_lum;
      
      for (rr = 0; rr < 16; rr++) {
	
	/* memcpy(dest, forw_lum+(rr<<4), 16);  */
	dest[0] = src[0];
	dest[1] = src[1];
	dest[2] = src[2];
	dest[3] = src[3];
	dest += row_incr;
	src += 4;
      }
      
      dest = (int *)(vid_stream->current->Cr + (crow * half_row) + ccol);
      dest1 = (int *)(vid_stream->current->Cb + (crow * half_row) + ccol);
      src = (int *)forw_cr;
      src1 = (int *)forw_cb;
      
      for (rr = 0; rr < 8; rr++) {
	/*
	 * memcpy(dest, forw_cr+(rr<<3), 8); memcpy(dest1, forw_cb+(rr<<3),
	 * 8);
	 */
	
	dest[0] = src[0];
	dest[1] = src[1];
	
	dest1[0] = src1[0];
	dest1[1] = src1[1];
	
	dest += half_row_incr;
	dest1 += half_row_incr;
	src += 2;
	src1 += 2;
      }
    } else if (vid_stream->mblock.bpict_past_back &&
	       !vid_stream->mblock.bpict_past_forw) {
      
      int *src, *src1;
      int *dest, *dest1;
      dest = (int *)(vid_stream->current->luminance + (row * row_size) + col);
      src = (int *)back_lum;
      
      for (rr = 0; rr < 16; rr++) {
	dest[0] = src[0];
	dest[1] = src[1];
	dest[2] = src[2];
	dest[3] = src[3];
	dest += row_incr;
	src += 4;
      }
      
      
      dest = (int *)(vid_stream->current->Cr + (crow * half_row) + ccol);
      dest1 = (int *)(vid_stream->current->Cb + (crow * half_row) + ccol);
      src = (int *)back_cr;
      src1 = (int *)back_cb;
      
      for (rr = 0; rr < 8; rr++) {
	/*
	 * memcpy(dest, back_cr+(rr<<3), 8); memcpy(dest1, back_cb+(rr<<3),
	 * 8);
	 */
	
	dest[0] = src[0];
	dest[1] = src[1];
	
	dest1[0] = src1[0];
	dest1[1] = src1[1];
	
	dest += half_row_incr;
	dest1 += half_row_incr;
	src += 2;
	src1 += 2;
      }
    } else {
      
      unsigned char *src1, *src2, *src1a, *src2a;
      unsigned char *dest, *dest1;
      dest = vid_stream->current->luminance + (row * row_size) + col;
      src1 = forw_lum;
      src2 = back_lum;
      
      for (rr = 0; rr < 16; rr++) {
	dest[0] = (int) (src1[0] + src2[0]) >> 1;
	dest[1] = (int) (src1[1] + src2[1]) >> 1;
	dest[2] = (int) (src1[2] + src2[2]) >> 1;
	dest[3] = (int) (src1[3] + src2[3]) >> 1;
	dest[4] = (int) (src1[4] + src2[4]) >> 1;
	dest[5] = (int) (src1[5] + src2[5]) >> 1;
	dest[6] = (int) (src1[6] + src2[6]) >> 1;
	dest[7] = (int) (src1[7] + src2[7]) >> 1;
	dest[8] = (int) (src1[8] + src2[8]) >> 1;
	dest[9] = (int) (src1[9] + src2[9]) >> 1;
	dest[10] = (int) (src1[10] + src2[10]) >> 1;
	dest[11] = (int) (src1[11] + src2[11]) >> 1;
	dest[12] = (int) (src1[12] + src2[12]) >> 1;
	dest[13] = (int) (src1[13] + src2[13]) >> 1;
	dest[14] = (int) (src1[14] + src2[14]) >> 1;
	dest[15] = (int) (src1[15] + src2[15]) >> 1;
	dest += row_size;
	src1 += 16;
	src2 += 16;
      }
      
      
      dest = vid_stream->current->Cr + (crow * half_row) + ccol;
      dest1 = vid_stream->current->Cb + (crow * half_row) + ccol;
      src1 = forw_cr;
      src2 = back_cr;
      src1a = forw_cb;
      src2a = back_cb;
      
      for (rr = 0; rr < 8; rr++) {
	dest[0] = (int) (src1[0] + src2[0]) >> 1;
	dest[1] = (int) (src1[1] + src2[1]) >> 1;
	dest[2] = (int) (src1[2] + src2[2]) >> 1;
	dest[3] = (int) (src1[3] + src2[3]) >> 1;
	dest[4] = (int) (src1[4] + src2[4]) >> 1;
	dest[5] = (int) (src1[5] + src2[5]) >> 1;
	dest[6] = (int) (src1[6] + src2[6]) >> 1;
	dest[7] = (int) (src1[7] + src2[7]) >> 1;
	dest += half_row;
	src1 += 8;
	src2 += 8;
	
	dest1[0] = (int) (src1a[0] + src2a[0]) >> 1;
	dest1[1] = (int) (src1a[1] + src2a[1]) >> 1;
	dest1[2] = (int) (src1a[2] + src2a[2]) >> 1;
	dest1[3] = (int) (src1a[3] + src2a[3]) >> 1;
	dest1[4] = (int) (src1a[4] + src2a[4]) >> 1;
	dest1[5] = (int) (src1a[5] + src2a[5]) >> 1;
	dest1[6] = (int) (src1a[6] + src2a[6]) >> 1;
	dest1[7] = (int) (src1a[7] + src2a[7]) >> 1;
	dest1 += half_row;
	src1a += 8;
	src2a += 8;
      }
    }
    
    if (ditherType == MBORDERED_DITHER) {
      ditherFlags[addr] = 1;
    }
  }
}




/*
 *--------------------------------------------------------------
 *
 * ReconSkippedBlock --
 *
 *	Reconstructs predictive block for skipped macroblocks
 *      in B Frames.
 *
 * Results:
 *	No return values.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static void
ReconSkippedBlock(source, dest, row, col, row_size,
		  right, down, right_half, down_half, width)
  unsigned char *source;
  unsigned char *dest;
  int row, col, row_size, right, down, right_half, down_half, width;
{
  int rr;
  unsigned char *source2;

  source += ((row + down) * row_size) + col + right;

  if (width == 16) {
    if ((!right_half) && (!down_half)) {
	if (right & 0x1) {
	  /* No alignment, use bye copy */
	  for (rr = 0; rr < 16; rr++) {
	    dest[0] = source[0];
	    dest[1] = source[1];
	    dest[2] = source[2];
	    dest[3] = source[3];
	    dest[4] = source[4];
	    dest[5] = source[5];
	    dest[6] = source[6];
	    dest[7] = source[7];
	    dest[8] = source[8];
	    dest[9] = source[9];
	    dest[10] = source[10];
	    dest[11] = source[11];
	    dest[12] = source[12];
	    dest[13] = source[13];
	    dest[14] = source[14];
	    dest[15] = source[15];
	    dest += 16;
	    source += row_size;
	  }
	} else if (right & 0x2) {
	  /* Half-word bit aligned, use 16 bit copy */
	  short *src = (short *)source;
	  short *d = (short *)dest;
	  row_size >>= 1;
	  for (rr = 0; rr < 16; rr++) {
	    d[0] = src[0];
	    d[1] = src[1];
	    d[2] = src[2];
	    d[3] = src[3];
	    d[4] = src[4];
	    d[5] = src[5];
	    d[6] = src[6];
	    d[7] = src[7];
	    d += 8;
	    src += row_size;
	  }
	} else {
	  /* Word aligned, use 32 bit copy */
	  int *src = (int *)source;
	  int *d = (int *)dest;
	  row_size >>= 2;
	  for (rr = 0; rr < 16; rr++) {
	    d[0] = src[0];
	    d[1] = src[1];
	    d[2] = src[2];
	    d[3] = src[3];
	    d += 4;
	    src += row_size;
	  }
	}
    } else {
      source2 = source + right_half + (row_size * down_half);
      for (rr = 0; rr < width; rr++) {
	dest[0] = (int) (source[0] + source2[0]) >> 1;
	dest[1] = (int) (source[1] + source2[1]) >> 1;
	dest[2] = (int) (source[2] + source2[2]) >> 1;
	dest[3] = (int) (source[3] + source2[3]) >> 1;
	dest[4] = (int) (source[4] + source2[4]) >> 1;
	dest[5] = (int) (source[5] + source2[5]) >> 1;
	dest[6] = (int) (source[6] + source2[6]) >> 1;
	dest[7] = (int) (source[7] + source2[7]) >> 1;
	dest[8] = (int) (source[8] + source2[8]) >> 1;
	dest[9] = (int) (source[9] + source2[9]) >> 1;
	dest[10] = (int) (source[10] + source2[10]) >> 1;
	dest[11] = (int) (source[11] + source2[11]) >> 1;
	dest[12] = (int) (source[12] + source2[12]) >> 1;
	dest[13] = (int) (source[13] + source2[13]) >> 1;
	dest[14] = (int) (source[14] + source2[14]) >> 1;
	dest[15] = (int) (source[15] + source2[15]) >> 1;
	dest += width;
	source += row_size;
	source2 += row_size;
      }
    }
  } else {			/* (width == 8) */
    assert(width == 8);
    if ((!right_half) && (!down_half)) {
      if (right & 0x1) {
	for (rr = 0; rr < width; rr++) {
	  dest[0] = source[0];
	  dest[1] = source[1];
	  dest[2] = source[2];
	  dest[3] = source[3];
	  dest[4] = source[4];
	  dest[5] = source[5];
	  dest[6] = source[6];
	  dest[7] = source[7];
	  dest += 8;
	  source += row_size;
	}
      } else if (right & 0x02) {
	short *d = (short *)dest;
	short *src = (short *)source;
	row_size >>= 1;
	for (rr = 0; rr < width; rr++) {
	  d[0] = src[0];
	  d[1] = src[1];
	  d[2] = src[2];
	  d[3] = src[3];
	  d += 4;
	  src += row_size;
	}
      } else {
	int *d = (int *)dest;
	int *src = (int *)source;
	row_size >>= 2;
	for (rr = 0; rr < width; rr++) {
	  d[0] = src[0];
	  d[1] = src[1];
	  d += 2;
	  src += row_size;
	}
      }
    } else {
      source2 = source + right_half + (row_size * down_half);
      for (rr = 0; rr < width; rr++) {
	dest[0] = (int) (source[0] + source2[0]) >> 1;
	dest[1] = (int) (source[1] + source2[1]) >> 1;
	dest[2] = (int) (source[2] + source2[2]) >> 1;
	dest[3] = (int) (source[3] + source2[3]) >> 1;
	dest[4] = (int) (source[4] + source2[4]) >> 1;
	dest[5] = (int) (source[5] + source2[5]) >> 1;
	dest[6] = (int) (source[6] + source2[6]) >> 1;
	dest[7] = (int) (source[7] + source2[7]) >> 1;
	dest += width;
	source += row_size;
	source2 += row_size;
      }
    }
  }
}



/*
 *--------------------------------------------------------------
 *
 * DoPictureDisplay --
 *
 *	Converts image from Lum, Cr, Cb to colormap space. Puts
 *      image in lum plane. Updates past and future frame
 *      pointers. Dithers image. Sends to display mechanism.
 *
 * Results:
 *	Pict image structure locked if displaying or if frame
 *      is needed as past or future reference.
 *
 * Side effects:
 *	Lum plane pummelled.
 *
 *--------------------------------------------------------------
 */

static void
DoPictureDisplay(vid_stream)
  VidStream *vid_stream;
{
  
  /* Update past and future references if needed. */

  if ((vid_stream->picture.code_type == I_TYPE) || 
      (vid_stream->picture.code_type == P_TYPE)) {
    if (vid_stream->future == NULL) {
      vid_stream->future = vid_stream->current;
      vid_stream->future->locked |= FUTURE_LOCK;
    } else {
      if (vid_stream->past != NULL) {
	vid_stream->past->locked &= ~PAST_LOCK;
      }
      vid_stream->past = vid_stream->future;
      vid_stream->past->locked &= ~FUTURE_LOCK;
      vid_stream->past->locked |= PAST_LOCK;
      vid_stream->future = vid_stream->current;
      vid_stream->future->locked |= FUTURE_LOCK;
      vid_stream->current = vid_stream->past;
      /* OLD: ShowOutputVal(vid_stream); */
    }
  } else {
    /* OLD ShowOutputVal(vid_stream); */
    }
}
