/* MPEGSTAT - analyzing tool for MPEG-I video streams
 * 
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
#define NO_SANITY_CHECKS
#include <assert.h>
#include "video.h"
#include "proto.h"
#include "decoders.h"
#include "opts.h"

/* note the C routines in decoders.c are surpassed by macros on decoders.h */
/* External declarations. */

extern int zigzag_direct[];
extern BlockVals blks;

/* Macro for returning 1 if num is positive, -1 if negative, 0 if 0. */

#define Sign(num) ((num > 0) ? 1 : ((num == 0) ? 0 : -1))

/* Error handling code (used by decoder macros)  */
extern char *errorLocation, *errorSpecifics;

/* DCT Collection string */
extern char *dctSpecifics;


/*
 *--------------------------------------------------------------
 *
 * ParseReconBlock --
 *
 *	Parse values for block structure from bitstream.
 *      n is an indication of the position of the block within
 *      the macroblock (i.e. 0-5) and indicates the type of 
 *      block (i.e. luminance or chrominance). Reconstructs
 *      coefficients from values parsed and puts in 
 *      block.dct_recon array in vid stream structure.
 *      sparseFlag is set when the block contains only one
 *      coeffictient and is used by the IDCT.
 *
 * Results:
 *	
 *
 * Side effects:
 *      Bit stream irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

#define DCT_recon blockPtr->dct_recon
#define DCT_dc_y_past blockPtr->dct_dc_y_past
#define DCT_dc_cr_past blockPtr->dct_dc_cr_past
#define DCT_dc_cb_past blockPtr->dct_dc_cb_past

#define DeZigZag(pos,i) if ((i<64)&&(i>=0)) {pos=zigzag_direct[i];}\
         else {                                                    \
	 sprintf(errorSpecifics,"\nIllegal value in block (%d %d)... skipping...\n",blks.frame,n);\
         return SKIP_TO_START_CODE;}
int
ParseReconBlock(n)
     int n;
{
#ifdef RISC
  unsigned int temp_curBits;
  int temp_bitOffset;
  int temp_bufLength;
  unsigned int *temp_bitBuffer;
#endif
  
  Block *blockPtr = &curVidStream->block;
  int coeffCount;
  char scratch[100];
  
  if (bufLength < 150)
    correct_underflow();
  
#ifdef RISC
  temp_curBits = curBits;
  temp_bitOffset = bitOffset;
  temp_bufLength = bufLength;
  temp_bitBuffer = bitBuffer;
#endif
  
  if (opts&COLLECTING&DCT_INFO) {
    /* Note the below assumes called in 0..5 order */
    if (n==0) {
      sprintf(dctSpecifics," 0: ");
    } else {
      sprintf(scratch,"%1d: ",n);
      strcat(dctSpecifics, scratch);
    }
  }
  {
    /*
     * Copy the globals curBits, bitOffset, bufLength, and bitBuffer
     * into local variables with the same names, so the macros use the
     * local variables instead.  This allows register allocation and
     * can provide 1-2 fps speedup.  On machines with not so many registers,
     * don't do this.
     */
#ifdef RISC
    register unsigned int curBits = temp_curBits;
    register int bitOffset = temp_bitOffset;
    register int bufLength = temp_bufLength;
    register unsigned int *bitBuffer = temp_bitBuffer;
#endif
    
    int diff;
    int size, level, i, run, pos, coeff;
    short int *reconptr;
    unsigned char *iqmatrixptr, *niqmatrixptr;
    int qscale;
    
    reconptr = DCT_recon[0];
    
    /* 
     * Hand coded version of memset that's a little faster...
     * Old call:
     *	memset((char *) DCT_recon, 0, 64*sizeof(short int));
     */
    {
      INT32 *p;
      p = (INT32 *) reconptr;
      
      p[0] = p[1] = p[2] = p[3] = p[4] = p[5] = p[6] = p[7] = p[8] = p[9] = 
	p[10] = p[11] = p[12] = p[13] = p[14] = p[15] = p[16] = p[17] = p[18] =
	  p[19] = p[20] = p[21] = p[22] = p[23] = p[24] = p[25] = p[26] = p[27] =
	    p[28] = p[29] = p[30] = p[31] = 0;
      
    }
    
    if (curVidStream->mblock.mb_intra) {
      
      if (n < 4) {
	
	/*
	 * Get the luminance bits.  This code has been hand optimized to
	 * get by the normal bit parsing routines.  We get some speedup
	 * by grabbing the next 16 bits and parsing things locally.
	 * Thus, calls are translated as:
	 *
	 *	show_bitsX  <-->   next16bits >> (16-X)
	 *	get_bitsX   <-->   val = next16bits >> (16-flushed-X);
	 *			   flushed += X;
	 *			   next16bits &= bitMask[flushed];
	 *	flush_bitsX <-->   flushed += X;
	 *			   next16bits &= bitMask[flushed];
	 *
	 * I've streamlined the code a lot, so that we don't have to mask
	 * out the low order bits and a few of the extra adds are removed.
	 *	bsmith
	 */
	unsigned int next16bits, index, flushed;
	
	show_bits16(next16bits);
	index = next16bits >> (16-7);
	size = dct_dc_size_luminance[index].value;
	flushed = dct_dc_size_luminance[index].num_bits;
	next16bits &= bitMask[16+flushed];

	if (size != 0) {
	  flushed += size;
	  diff = next16bits >> (16-flushed);
          if (!(diff & bitTest[32-size])) {
	    diff = rBitMask[size] | (diff + 1);
	  }
	} else {
	  diff = 0;
	}
	flush_bits(flushed);

	if (n == 0) {
	  coeff = diff << 3;
	  if (curVidStream->mblock.mb_address -
	      curVidStream->mblock.past_intra_addr > 1) 
	    coeff += 1024;
	  else coeff += DCT_dc_y_past;
	  DCT_dc_y_past = coeff;
	} else {
	  coeff = DCT_dc_y_past + (diff << 3);
	  DCT_dc_y_past = coeff;
	}
	if (opts&COLLECTING&DCT_INFO) {
	  sprintf(scratch, "(%d) ",coeff);
	  strcat(dctSpecifics,scratch);
	}	    
      } else {
	
	/*
	 * Get the chrominance bits.  This code has been hand optimized
	 * as described above
	 */
	unsigned int next16bits, index, flushed;

	show_bits16(next16bits);
	index = next16bits >> (16-8);
	size = dct_dc_size_chrominance[index].value;
	flushed = dct_dc_size_chrominance[index].num_bits;
	next16bits &= bitMask[16+flushed];
	
	if (size != 0) {
	  flushed += size;
	  diff = next16bits >> (16-flushed);
          if (!(diff & bitTest[32-size])) {
	    diff = rBitMask[size] | (diff + 1);
	  }
	} else {
	  diff = 0;
	}
	flush_bits(flushed);
	
	if (n == 4) {
	  coeff = diff << 3;
	  if (curVidStream->mblock.mb_address -
	      curVidStream->mblock.past_intra_addr > 1) 
	    coeff += 1024;
	  else coeff += DCT_dc_cr_past;
	  DCT_dc_cr_past = coeff;

	} else {
	  coeff = diff << 3;
	  if (curVidStream->mblock.mb_address -
	      curVidStream->mblock.past_intra_addr > 1) 
	    coeff += 1024;
	  else coeff += DCT_dc_cb_past;
	  DCT_dc_cb_past = coeff;
	}

	if (opts&COLLECTING&DCT_INFO) {
	  sprintf(scratch, "(%d) ",coeff);
	  strcat(dctSpecifics,scratch);
	}	    
      }
      
      *reconptr = coeff;
      i = 0; pos = 0;
      coeffCount = (coeff != 0);
    
      if (curVidStream->picture.code_type != 4) {
	
	qscale = curVidStream->slice.quant_scale;
	iqmatrixptr = curVidStream->intra_quant_matrix[0];
	
	while (1) {

	  DecodeDCTCoeffNext(run, level);

	  if (run == END_OF_BLOCK) break;

	  i = i + run + 1;

	  DeZigZag(pos,i);

	  if (opts&COLLECTING&DCT_INFO) {
	    sprintf(scratch, "%d %d, ",pos,level);
	    strcat(dctSpecifics,scratch);
	  }	    
	  coeff = (level * qscale * ((int) iqmatrixptr[pos])) >> 3;
	  if (level < 0) {
	    coeff += (coeff & 1);
	  } else {
	    coeff -= (coeff & 1);
	  }
	  
	  reconptr[pos] = coeff;
	  
	  coeffCount++;/* Used to be if (coeff) {}, but didnt make sense */
	}
	
	
	{
	  extern unsigned int *mbCoeffPtr;
	  if (COLLECTING) mbCoeffPtr[pos]++;
	}
	
	flush_bits(2);
	
	goto end;
      }
    } else {
      
      niqmatrixptr = curVidStream->non_intra_quant_matrix[0];
      qscale = curVidStream->slice.quant_scale;
      
      DecodeDCTCoeffFirst(run, level);
      i = run;
      
      DeZigZag(pos,i);
      if (opts&COLLECTING&DCT_INFO) {
	sprintf(scratch, "%d %d, ",pos,level);
	strcat(dctSpecifics,scratch);
      }	    
      if (level < 0) {
	coeff = (((level<<1) - 1) * qscale * 
		 ((int) (niqmatrixptr[pos]))) >> 4; 
	coeff += (coeff & 1);
      } else {
	coeff = (((level<<1) + 1) * qscale * 
		 ((int) (*(niqmatrixptr+pos)))) >> 4; 
	coeff -= (coeff & 1);
      }
      reconptr[pos] = coeff;
      coeffCount = (coeff!=0);
      
      if (curVidStream->picture.code_type != 4) {
	
	while(1) {
	  
	  DecodeDCTCoeffNext(run, level);

	  if (run == END_OF_BLOCK) break;

	  i = i+run+1;
	  DeZigZag(pos,i);
	  if (opts&COLLECTING&DCT_INFO) {
	    sprintf(scratch, "%d %d, ",pos,level);
	    strcat(dctSpecifics,scratch);
	  }	    
	  if (level < 0) {
	      coeff = (((level<<1) - 1) * qscale * 
		       ((int) (niqmatrixptr[pos]))) >> 4; 
	      coeff += (coeff & 1);
	  } else {
	      coeff = (((level<<1) + 1) * qscale * 
		       ((int) (*(niqmatrixptr+pos)))) >> 4; 
	      coeff -= (coeff & 1);
	  }
	  reconptr[pos] = coeff;
	  coeffCount++;  /* Used to be if (coeff), but I couldnt see why */
	}
	{
	  extern unsigned int *mbCoeffPtr;
	  if (COLLECTING) mbCoeffPtr[pos]++;
	}

	flush_bits(2);

	goto end;
      }
    }
    
  end:

    if (coeffCount == 1) j_rev_dct_sparse (reconptr, pos);
    else j_rev_dct(reconptr);

#ifdef RISC
    temp_curBits = curBits;
    temp_bitOffset = bitOffset;
    temp_bufLength = bufLength;
    temp_bitBuffer = bitBuffer;
#endif

  }

#ifdef RISC
  curBits = temp_curBits;
  bitOffset = temp_bitOffset;
  bufLength = temp_bufLength;
  bitBuffer = temp_bitBuffer;
#endif
  return PARSE_OK;
}
	
#undef DCT_recon 
#undef DCT_dc_y_past 
#undef DCT_dc_cr_past 
#undef DCT_dc_cb_past 


/*
 *--------------------------------------------------------------
 *
 * ParseAwayBlock --
 *
 *	Parses off block values, throwing them away.
 *      Used when not verifying
 *
 * Results:
 *	None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

int
ParseAwayBlock(n)
     int n;
{
  unsigned int diff;
  unsigned int size, run;
  int level;

  if (bufLength < 100)
    correct_underflow();

  if (curVidStream->mblock.mb_intra) {

    /* If the block is a luminance block... */

    if (n < 4) {

      /* Parse and decode size of first coefficient. */

      DecodeDCTDCSizeLum(size);

      /* Parse first coefficient. */

      if (size != 0) {
	get_bitsn(size, diff);
      }
    }

    /* Otherwise, block is chrominance block... */

    else {

      /* Parse and decode size of first coefficient. */

      DecodeDCTDCSizeChrom(size);

      /* Parse first coefficient. */

      if (size != 0) {
	get_bitsn(size, diff);
      }
    }
  }

  /* Otherwise, block is not intracoded... */

  else {

    /* Decode and set first coefficient. */

    DecodeDCTCoeffFirst(run, level);
  }

  /* If picture is not D type (i.e. I, P, or B)... */

  if (curVidStream->picture.code_type != 4) {

    /* While end of macroblock has not been reached... */

    while (1) {

      /* Get the dct_coeff_next */

      DecodeDCTCoeffNext(run, level);

      if (run == END_OF_BLOCK) break;
    }

    /* End_of_block */

    flush_bits(2);
  }
  return PARSE_OK;
}
