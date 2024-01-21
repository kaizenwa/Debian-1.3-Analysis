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
/* This file contains C code to implement an ordered dither. */

#include "video.h"
#include "proto.h"
#include "rgbl.h"

int r_values[R_RANGE];
int b_values[B_RANGE];
int g_values[G_RANGE];

void
InitRGBLColor()
{
  int i;

  for (i=0; i<R_RANGE; i++) {
    r_values[i] = ((i * 256) / (R_RANGE)) + (256/(R_RANGE*2));
  }

  for (i=0; i<G_RANGE; i++) {
    g_values[i] = ((i * 256) / (G_RANGE)) + (256/(G_RANGE*2));
  }

  for (i=0; i<B_RANGE; i++) {
    b_values[i] = ((i * 256) / (B_RANGE)) + (256/(B_RANGE*2));
  }

}


static unsigned char *r_pix_a;
static unsigned char *g_pix_a;
static unsigned char *b_pix_a;

double chrom_mult;
double lum_mult;
double chrom_add;
double lum_add;

void
InitRGBLDither()
{
  int y, ch, indx, r, b, g;
  double fl, fch, fr, fb, fcr, fcb, fg;
  r_pix_a = (unsigned char *) malloc(256*256*sizeof(char));
  g_pix_a = (unsigned char *) malloc(256*256*sizeof(char));
  b_pix_a = (unsigned char *) malloc(256*256*sizeof(char));
  
  for (y = 0; y<256; y++) {
    for (ch = 0; ch<256; ch++) {
      indx = ((y << 8) | ch);
      
      fl = (double) y;
      fl *= lum_mult;
      fl += lum_add;
      fch = ((double) ch) - 128.0;
      fch *= chrom_mult;
      fch += chrom_add;

      fr = fl + (1.40200 * fch);
      fb = fl + (1.77200 * fch);

      if (fr < 0.0) fr = 0.0;
      else if (fr > 255.0) fr = 255.0;
      
      if (fb < 0.0) fb = 0.0;
      else if (fb > 255.0) fb = 255.0;

      r = ((int) fr);
      b = ((int) fb);

      r = (r * R_RANGE) / 256;
      b = (b * B_RANGE) / 256;

      r_pix_a[indx] = r_values[r];
      b_pix_a[indx] = b_values[b];

      r = ch & 0xf0;
      b = (ch & 0x0f) << 4;

      fcr = ((double) r) - 128.0;
      fcb = ((double) b) - 128.0;
      fcr *= chrom_mult;
      fcb *= chrom_mult;
      fcr += chrom_add;
      fcb += chrom_add;

      fg = fl - (0.71414 * fcb) - (0.34414 * fcr);

      if (fg < 0.0) fg = 0.0;
      else if (fg > 255.0) fg = 255.0;

      g = ((int) fg);

      g = (g * G_RANGE) / 256;

      g_pix_a[indx] = g_values[g];
    }
  }
}     

void RGBLDitherImage(lum, cr, cb, out, h, w)
    unsigned char *lum;
    unsigned char *cr;
    unsigned char *cb;
    unsigned char *out;
    int w, h;
{
  unsigned char *o1, *o2, *o3, *o4, *o5, *o6, *l1, *l2;
  int i, j, l1_val, l2_val, g_val;
  int wadd;

  wadd = 15*w;
  o1 = out;
  o2 = (out + (3*w));
  o3 = (out + (6*w));
  o4 = (out + (9*w));
  o5 = (out + (12*w));
  o6 = (out + (15*w));
  l1 = lum;
  l2 = lum+w;
  
  for (i=0; i<h; i+=2) {
    for (j=0; j<w; j+= 2) {
      l1_val = (*l1++) << 8;
      l2_val = (*l2++) << 8;

      g_val = ((*cr) & 0xf0) | ((*cb) >> 4);

      o2[0] = o1[2] = o3[2] = r_pix_a[(l1_val | (*cb))];
      o2[1] = o2[2] = g_pix_a[(l1_val | g_val)];
      o1[0] = o1[1] = o3[0] = o3[1] = b_pix_a[(l1_val | (*cr))];
      
      
      o4[0] = o5[2] = o6[0] = r_pix_a[(l2_val | (*cb))];
      o4[1] = o4[2] = o6[1] = o6[2] = g_pix_a[(l2_val | g_val)];
      o5[0] = o5[1] = b_pix_a[(l2_val | (*cr))];

      l1_val = (*l1++) << 8;
      l2_val = (*l2++) << 8;

      o1[3] = o2[5] = o3[3] = r_pix_a[(l1_val | (*cb))];
      o1[4] = o1[5] = o3[4] = o3[5] = g_pix_a[(l1_val | g_val)];
      o2[3] = o2[4] = b_pix_a[(l1_val | (*cr))];
      
      o4[5] = o5[3] = o6[5] = r_pix_a[(l2_val | (*cb++))];
      o5[4] = o5[5] = g_pix_a[(l2_val | g_val)];
      o4[3] = o4[4] = o6[3] = o6[4] = b_pix_a[(l2_val | (*cr++))];

      o1 +=6; o2 +=6; o3 += 6; o4 += 6; o5 += 6; o6 += 6;
    }

    l1 += w;
    l2 += w;
    o1 += wadd;
    o2 += wadd;
    o3 += wadd;
    o4 += wadd;
    o5 += wadd;
    o6 += wadd;
  }
}

