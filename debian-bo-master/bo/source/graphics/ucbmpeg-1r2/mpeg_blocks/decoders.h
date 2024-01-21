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
/*
 * decoders.h
 *
 * This file contains the declarations of structures required for Huffman
 * decoding
 *
 */

/* Include util.h for bit i/o parsing macros. */

#include "util.h"

/* Code for unbound values in decoding tables */
#define ERROR -1
#define DCT_ERROR 63

#define MACRO_BLOCK_STUFFING 34
#define MACRO_BLOCK_ESCAPE 35

/* Two types of DCT Coefficients */
#define DCT_COEFF_FIRST 0
#define DCT_COEFF_NEXT 1

/* Special values for DCT Coefficients */
#define END_OF_BLOCK 62
#define ESCAPE 61

/* Structure for an entry in the decoding table of 
 * macroblock_address_increment */
typedef struct {
  unsigned int value;       /* value for macroblock_address_increment */
  int num_bits;             /* length of the Huffman code */
} mb_addr_inc_entry;

/* Decoding table for macroblock_address_increment */
extern mb_addr_inc_entry mb_addr_inc[2048];


/* Structure for an entry in the decoding table of macroblock_type */
typedef struct {
  unsigned int mb_quant;              /* macroblock_quant */
  unsigned int mb_motion_forward;     /* macroblock_motion_forward */
  unsigned int mb_motion_backward;    /* macroblock_motion_backward */
  unsigned int mb_pattern;            /* macroblock_pattern */
  unsigned int mb_intra;              /* macroblock_intra */
  int num_bits;                       /* length of the Huffman code */
} mb_type_entry;

/* Decoding table for macroblock_type in predictive-coded pictures */
extern mb_type_entry mb_type_P[64];

/* Decoding table for macroblock_type in bidirectionally-coded pictures */
extern mb_type_entry mb_type_B[64];


/* Structures for an entry in the decoding table of coded_block_pattern */
typedef struct {
  unsigned int cbp;            /* coded_block_pattern */
  int num_bits;                /* length of the Huffman code */
} coded_block_pattern_entry;

/* External declaration of coded block pattern table. */

extern coded_block_pattern_entry coded_block_pattern[512];



/* Structure for an entry in the decoding table of motion vectors */
typedef struct {
  int code;              /* value for motion_horizontal_forward_code,
			  * motion_vertical_forward_code, 
			  * motion_horizontal_backward_code, or
			  * motion_vertical_backward_code.
			  */
  int num_bits;          /* length of the Huffman code */
} motion_vectors_entry;


/* Decoding table for motion vectors */
extern motion_vectors_entry motion_vectors[2048];


/* Structure for an entry in the decoding table of dct_dc_size */
typedef struct {
  unsigned int value;    /* value of dct_dc_size (luminance or chrominance) */
  int num_bits;          /* length of the Huffman code */
} dct_dc_size_entry;

/* External declaration of dct dc size lumiance table. */

extern dct_dc_size_entry dct_dc_size_luminance[128];

/* External declaration of dct dc size chrom table. */

extern dct_dc_size_entry dct_dc_size_chrominance[256];


/* DCT coeff tables. */

#define RUN_MASK 0xfc00
#define LEVEL_MASK 0x03f0
#define NUM_MASK 0x000f
#define RUN_SHIFT 10
#define LEVEL_SHIFT 4

/* External declaration of dct coeff tables. */

extern unsigned short int dct_coeff_tbl_0[256];
extern unsigned short int dct_coeff_tbl_1[16];
extern unsigned short int dct_coeff_tbl_2[4];
extern unsigned short int dct_coeff_tbl_3[4];
extern unsigned short int dct_coeff_next[256];
extern unsigned short int dct_coeff_first[256];

/*
 *--------------------------------------------------------------
 *
 * DecodeMBAddrInc --
 *
 *      Huffman Decoder for macro_block_address_increment; the location
 *      in which the result will be placed is being passed as argument.
 *      The decoded value is obtained by doing a table lookup on
 *      mb_addr_inc.
 *
 * Results:
 *      The decoded value for macro_block_address_increment or ERROR
 *      for unbound values will be placed in the location specified.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed.
 *
 *--------------------------------------------------------------
 */
#define DecodeMBAddrInc(val)				\
{							\
    unsigned int index;					\
    show_bits11(index);					\
    val = mb_addr_inc[index].value;			\
    flush_bits(mb_addr_inc[index].num_bits);		\
}

/*
 *--------------------------------------------------------------
 *
 * DecodeMotionVectors --
 *
 *      Huffman Decoder for the various motion vectors, including
 *      motion_horizontal_forward_code, motion_vertical_forward_code,
 *      motion_horizontal_backward_code, motion_vertical_backward_code.
 *      Location where the decoded result will be placed is being passed
 *      as argument. The decoded values are obtained by doing a table
 *      lookup on motion_vectors.
 *
 * Results:
 *      The decoded value for the motion vector or ERROR for unbound
 *      values will be placed in the location specified.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed.
 *
 *--------------------------------------------------------------
 */

#define DecodeMotionVectors(value)			\
{							\
  unsigned int index;					\
  show_bits11(index);					\
  value = motion_vectors[index].code;			\
  flush_bits(motion_vectors[index].num_bits);		\
}
/*
 *--------------------------------------------------------------
 *
 * DecodeMBTypeB --
 *
 *      Huffman Decoder for macro_block_type in bidirectionally-coded
 *      pictures;locations in which the decoded results: macroblock_quant,
 *      macroblock_motion_forward, macro_block_motion_backward,
 *      macroblock_pattern, macro_block_intra, will be placed are
 *      being passed as argument. The decoded values are obtained by
 *      doing a table lookup on mb_type_B.
 *
 * Results:
 *      The various decoded values for macro_block_type in
 *      bidirectionally-coded pictures or ERROR for unbound values will
 *      be placed in the locations specified.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed.
 *
 *--------------------------------------------------------------
 */
#define DecodeMBTypeB(quant, motion_fwd, motion_bwd, pat, intra)	\
{									\
  unsigned int index;							\
									\
  show_bits6(index);							\
									\
  quant = mb_type_B[index].mb_quant;					\
  motion_fwd = mb_type_B[index].mb_motion_forward;			\
  motion_bwd = mb_type_B[index].mb_motion_backward;			\
  pat = mb_type_B[index].mb_pattern;					\
  intra = mb_type_B[index].mb_intra;					\
  flush_bits(mb_type_B[index].num_bits);				\
}
/*
 *--------------------------------------------------------------
 *
 * DecodeMBTypeI --
 *
 *      Huffman Decoder for macro_block_type in intra-coded pictures;
 *      locations in which the decoded results: macroblock_quant,
 *      macroblock_motion_forward, macro_block_motion_backward,
 *      macroblock_pattern, macro_block_intra, will be placed are
 *      being passed as argument.
 *
 * Results:
 *      The various decoded values for macro_block_type in intra-coded
 *      pictures or ERROR for unbound values will be placed in the
 *      locations specified.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed.
 *
 *--------------------------------------------------------------
 */
#define DecodeMBTypeI(quant, motion_fwd, motion_bwd, pat, intra)	\
{									\
  unsigned int index;							\
  static int quantTbl[4] = {ERROR, 1, 0, 0};				\
									\
  show_bits2(index);							\
									\
  motion_fwd = 0;							\
  motion_bwd = 0;							\
  pat = 0;								\
  intra = 1;								\
  quant = quantTbl[index];						\
  if (index) {								\
    flush_bits (1 + quant);						\
  }									\
}
/*
 *--------------------------------------------------------------
 *
 * DecodeMBTypeP --
 *
 *      Huffman Decoder for macro_block_type in predictive-coded pictures;
 *      locations in which the decoded results: macroblock_quant,
 *      macroblock_motion_forward, macro_block_motion_backward,
 *      macroblock_pattern, macro_block_intra, will be placed are
 *      being passed as argument. The decoded values are obtained by
 *      doing a table lookup on mb_type_P.
 *
 * Results:
 *      The various decoded values for macro_block_type in
 *      predictive-coded pictures or ERROR for unbound values will be
 *      placed in the locations specified.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed.
 *
 *--------------------------------------------------------------
 */
#define DecodeMBTypeP(quant, motion_fwd, motion_bwd, pat, intra)	\
{									\
  unsigned int index;							\
									\
  show_bits6(index);							\
									\
  quant = mb_type_P[index].mb_quant;					\
  motion_fwd = mb_type_P[index].mb_motion_forward;			\
  motion_bwd = mb_type_P[index].mb_motion_backward;			\
  pat = mb_type_P[index].mb_pattern;					\
  intra = mb_type_P[index].mb_intra;					\
									\
  flush_bits(mb_type_P[index].num_bits);				\
}
/*
 *--------------------------------------------------------------
 *
 * DecodeCBP --
 *
 *      Huffman Decoder for coded_block_pattern; location in which the
 *      decoded result will be placed is being passed as argument. The
 *      decoded values are obtained by doing a table lookup on
 *      coded_block_pattern.
 *
 * Results:
 *      The decoded value for coded_block_pattern or ERROR for unbound
 *      values will be placed in the location specified.
 *
 * Side effects:
 *      Bit stream is irreversibly parsed.
 *
 *--------------------------------------------------------------
 */
#define DecodeCBP(coded_bp)						\
{									\
  unsigned int index;							\
									\
  show_bits9(index);							\
  coded_bp = coded_block_pattern[index].cbp;				\
  flush_bits(coded_block_pattern[index].num_bits);			\
}

/*
 * These macros are used to just parse away the correct number of bits
 * from the bitstream.  This is needed for the chrominance blocks when
 * doing gray dithering
 */

#define ParseDCTCoeff(dct_coeff_tbl, run, level)			\
{									\
  unsigned int temp, index;						\
  unsigned int value, next32bits, flushed;				\
									\
  show_bits32(next32bits);						\
									\
  index = next32bits >> 24;						\
									\
  if (index > 3) {							\
    value = dct_coeff_tbl[index];					\
    run = value >> RUN_SHIFT;						\
    if (run != END_OF_BLOCK) {						\
      if (run != ESCAPE) {						\
	 flushed = (value & NUM_MASK) + 2;				\
       } else {								\
	 flushed = (value & NUM_MASK) + 1;				\
	 temp = next32bits >> (18-flushed);				\
	 temp &= 0xff;							\
	 if (temp == 0) {						\
	    flushed += 22;						\
	 } else if (temp != 128) {					\
	    flushed += 14;						\
	 } else {							\
	    flushed += 22;						\
	 }								\
       }								\
       flush_bits(flushed);						\
    }									\
  } else {								\
    if (index == 2) { 							\
      index = next32bits >> 22;						\
      value = dct_coeff_tbl_2[index & 3];				\
    } else if (index == 3) { 						\
      index = next32bits >> 22;						\
      value = dct_coeff_tbl_3[index & 3];				\
    } else if (index) {	/* index == 1 */				\
      index = next32bits >> 20;						\
      value = dct_coeff_tbl_1[index & 15];				\
    } else {   /* index == 0 */						\
      index = next32bits >> 16;						\
      value = dct_coeff_tbl_0[index & 255];				\
    }									\
    run = 0;								\
    flushed = (value & NUM_MASK) + 2;					\
    flush_bits(flushed);						\
  }									\
}

#define ParseDCTCoeffFirst(runval, levelval)         \
{                                                     \
  ParseDCTCoeff(dct_coeff_first, runval, levelval);  \
}          

#define ParseDCTCoeffNext(runval, levelval)          \
{                                                     \
  ParseDCTCoeff(dct_coeff_next, runval, levelval);   \
}

