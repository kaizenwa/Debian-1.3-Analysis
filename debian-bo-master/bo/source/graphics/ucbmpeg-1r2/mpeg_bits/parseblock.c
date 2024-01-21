/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */
#include <assert.h>
#include "video.h"
#include "proto.h"
#include "decoders.h"

extern int globalPFrame;

/* External declarations. */

extern int zigzag_direct[];

/* Macro for returning 1 if num is positive, -1 if negative, 0 if 0. */

#define Sign(num) ((num > 0) ? 1 : ((num == 0) ? 0 : -1))
static int globalCount;


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

void
ParseReconBlock(n)
     int n;
{
  Block *blockPtr = &curVidStream->block;
  int coeffCount;
  int diff;
  int size, level, i, pos, coeff;
  unsigned int run;
  short int *reconptr;
  unsigned char *iqmatrixptr, *niqmatrixptr;
  int qscale;

  reconptr = DCT_recon[0];
  if (bufLength < 100)
    correct_underflow();

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
    } else {
      
      /*
       * Get the chrominance bits.  This code has been hand optimized to
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
    }
    
    *reconptr = coeff;
    i = 0; pos = 0;
    coeffCount = (coeff != 0);
  
    if (curVidStream->picture.code_type != 4) {
      
      qscale = curVidStream->slice.quant_scale;
      iqmatrixptr = curVidStream->intra_quant_matrix[0];
      
      while(1) {
	
	decodeDCTCoeffNext(&run, &level);

	globalCount++;
	if (globalCount == 22236) {
	    i = i;
	}

	if (run == END_OF_BLOCK) break;

	i = i + run + 1;
	pos = zigzag_direct[i];
	coeff = (level * qscale * ((int) iqmatrixptr[pos])) >> 3;

	if (level < 0) {
	    coeff += (coeff & 1);
	} else {
	    coeff -= (coeff & 1);
	}

	reconptr[pos] = coeff;

	if (coeff) {
	  coeffCount++;
	}
      }

      {
	extern unsigned int *mbCoeffPtr;
	mbCoeffPtr[pos]++;
      }

      flush_bits(2);

      goto end;
    }
  }
  
  else {
    
    niqmatrixptr = curVidStream->non_intra_quant_matrix[0];
    qscale = curVidStream->slice.quant_scale;
    
    decodeDCTCoeffFirst(&run, &level);

    globalCount++;
    i = run;

    if (globalCount == 22236) {
	i = i;
    }
    pos = zigzag_direct[i];
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
    if (coeff) {
      coeffCount = 1;
    }

    if (curVidStream->picture.code_type != 4) {
      
      while(1) {
	
	decodeDCTCoeffNext(&run, &level);

	globalCount++;
	if (globalCount == 22236) {
	    i = i;
	}

	if (run == END_OF_BLOCK) break;

	i = i+run+1;
	if (i > 63) {
	    i = i;
	}
	pos = zigzag_direct[i];
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
	if (coeff) {
	  coeffCount++;
	}
      }

      {
	extern unsigned int *mbCoeffPtr;
	mbCoeffPtr[pos]++;
      }

      flush_bits(2);

      goto end;
    }
  }
  
end:

#ifdef BLEAH
if ( ! globalPFrame )
{
fprintf(stdout, "DCT = ");
for ( i = 0; i < 64; i++ )
    fprintf(stdout, "%d   ", reconptr[i]);
fprintf(stdout, "\n");
}
#endif

  if (coeffCount == 1) j_rev_dct_sparse (reconptr, pos);
  else j_rev_dct(reconptr);

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
 *      Used with grayscale dithering.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
ParseAwayBlock(n)
     int n;
{
  unsigned int size, run;

  if (bufLength < 100)
    correct_underflow();

  if (curVidStream->mblock.mb_intra) {

    /* If the block is a luminance block... */
    if (n < 4) {

      /* Parse and decode size of first coefficient. */
      decodeDCTDCSizeLum(&size);

      /* Parse first coefficient. */
      if (size != 0) {
	flush_bits(size);
      }
    }

    /* Otherwise, block is chrominance block... */
    else {

      /* Parse and decode size of first coefficient. */
      decodeDCTDCSizeChrom(&size);

      /* Parse first coefficient. */
      if (size != 0) {
	flush_bits(size);
      }
    }
  }

  /* Otherwise, block is not intracoded... */

  else {
    /* Parse first coefficient. */
    ParseDCTCoeffFirst(run, level);
  }

  /* If picture is not D type (i.e. I, P, or B)... */
  if (curVidStream->picture.code_type != 4) {

    /* While end of macroblock has not been reached... */
    while (1) {
      /* Get the dct_coeff_next */
      ParseDCTCoeffNext(run, level);
      if (run == END_OF_BLOCK) break;
    }
    /* End_of_block */
    flush_bits(2);
  }
}
