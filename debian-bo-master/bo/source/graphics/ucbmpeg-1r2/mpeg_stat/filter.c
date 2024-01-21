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
 * ---------------------------
 *
 * Copyright (c) 1993 Technical University of Berlin, Germany
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
 * analyzing routines
 * Most of the specilized statistics routines are here.
 */

#include <stdio.h>
#include "video.h"
#include "opts.h"
#include "proto.h"
#ifndef MIPS
#include <sys/time.h>
#else
#include <sys/types.h>
#include <sys/system.h>
#endif

/* Variables from Video.c */
extern Statval stat_a[4];
extern BlockVals blks;
extern char *VidRate[16];
extern double VidRateNum[16];
extern char *PelRatio[16];
extern int sys_layer, vidBytes, audBytes, sysBytes;
extern int rate_disp, rate_max, rate_min, f_code_ok;

/* Local Routines */
#define max(a,b)  (a>b?a:b)
static void init_stat_struct();

/*
 * mvstat
 * 
 * Keep motion vector statistics
 *
 */
int mvstat (value, mode)
int value, mode;
{
  static int max_horizontal_f = 0, max_vertical_f = 0;
  static int  max_horizontal_b = 0, max_vertical_b = 0;
  static int ave_h_b = 0, ave_h_f = 0, ave_v_b = 0, ave_v_f = 0;
  static int count_h_b = 0, count_h_f = 0, count_v_b = 0, count_v_f = 0;
  int *ip;
  
  switch (mode) {
  case 0:
    ip = &max_horizontal_f;
    count_h_f++;
    ave_h_f += abs(value);
    break;
  case 1:
    ip = &max_vertical_f;
    count_v_f++;
    ave_v_f += abs(value);
    break;
  case 2:
    ip = &max_horizontal_b;
    count_h_b++;
    ave_h_b += abs(value);
    break;
  case 3:
    ip = &max_vertical_b;
      count_v_b++;
      ave_v_b += abs(value);
    break;
  case 5:
    return max_horizontal_f;
  case 6:
    return max_vertical_f;
  case 7:
    return max_horizontal_b;
  case 8:
    return max_vertical_b;
  case 10 :
    if (count_h_f > 0)
      return( ave_h_f / count_h_f);
    else return -1;
  case 11 :
    if (count_v_f > 0)
      return( ave_v_f / count_v_f);
    else return -1;
  case 12 :
    if (count_h_b > 0)
      return( ave_h_b / count_h_b);
    else return -1;
  case 13 :
    if (count_v_b > 0)
      return( ave_v_b / count_v_b);
    else return -1;
    
  }
  *ip = max (*ip, abs (value));
  return 0;
}


/*
 * printtype prints the frame type during data parsing.
 */
printtype(vs)
VidStream *vs;
{

  static int first_time = 1;
  
  if(first_time) {
    printf("Picture coding types (bitstream order / display order):\n");
    first_time = 0;
  }
  
  switch (vs->picture.code_type) {
  case B_TYPE:
    printf("B");
    break;
  case P_TYPE:
    printf("P");
    break;
  case I_TYPE:
    printf("I");
    break;
  default:
    printf("?");
  }
  fflush(stdout);
}

/*
 * init_block_struct
 *
 * Setup the block data structure to collect detailed data.
 *
 */
void init_block_struct(a)
BlockVals *a;
{
   int i,j;
   a->frame=-1;
   a->slice=-1;
   a->block=0;
   a->btype=0;
   a->qs=0;
   a->mb_skipped=0;
   a->mb_coded=0;
   a->cblks=0;
   a->nblks=0;
   for (i=0; i<6; i++) a->chist[i]=0;
   for (i=1; i<4; i++) {
     for (j=1; j<32;j++) {
       a->q[i][j]=0;
     }}
 }


/*
 * init_stats
 *
 * Setup the statistics structures
 *
 */
void
init_stats()
{
  int i, j;

  for (i = 0; i < 4; i++) {
    init_stat_struct(&(stat_a[i]));
    stat_a[i].frametype = i;
  }
  bitCount = 0;
  init_block_struct(&blks);
}

/*
 * PrintAllStats
 *
 * Print the summary statistics for the entire stream
 *
 */
void
PrintAllStats()
{
  int i, j;
  int average, pixelnum;

  unsigned int supertot, supernum;
  double supertime;

  pixelnum = curVidStream->mb_width * 16 * curVidStream->mb_height * 16;

  printf("\nSUMMARY:\n\n");

  supertot = stat_a[1].totsize + stat_a[2].totsize + stat_a[3].totsize;
  supernum = stat_a[1].number + stat_a[2].number + stat_a[3].number;
  supertime = stat_a[1].tottime + stat_a[2].tottime + stat_a[3].tottime;

  if (sys_layer) {
    printf("Total number of frames: %d.  Length is %2.2f sec\n\n",
	   supernum, supernum*1.0/VidRateNum[curVidStream->picture_rate]);
    } else {
      printf("Total Bytes read: %d. Total number of frames: %d.  Length is %2.2f sec\n\n",
	     bitCountRead()/8, supernum, supernum*1.0/VidRateNum[curVidStream->picture_rate]);
    }
  printf("Width: %d\tHeight: %d\n", curVidStream->h_size,curVidStream->v_size);

  printf("Avg. Frame Size: %d bytes + %d bits  (average rate %2.2f bits/sec)\n",
	 supertot / (8 * supernum), (supertot / supernum) % 8,
	 1.0*supertot* VidRateNum[curVidStream->picture_rate] / supernum );
  if (opts&RATE_INFO) {
    if (opts&RATE_LENGTH_SET) {
      if (sys_layer) {
	int totBytes=vidBytes+audBytes+sysBytes;
	printf("Breakdown of system layer:\n");
	printf("\tVideo (with packet headers): %d bytes (%2.2f%%) %d/(%d frames)\n",
	       vidBytes,vidBytes*100.0/totBytes, vidBytes/rate_disp,rate_disp);
	printf("\tAudio (with packet headers): %d bytes (%2.2f%%) %d/(%d frames)\n",
	       audBytes,audBytes*100.0/totBytes,audBytes/rate_disp,rate_disp);
	printf("\tSystem (other headers, other packets, etc): %d bytes (%2.2f%%) %d/(%d frames)\n",
	       sysBytes,sysBytes*100.0/totBytes,sysBytes/rate_disp,rate_disp);
      }
      printf("Max instantaneous video rate %d, minimum %d bits/(%d frames)\n",
	     rate_max,rate_min,rate_disp);
    } else {
      if (sys_layer) {
	int totBytes=vidBytes+audBytes+sysBytes;
	int rate_disp=(int)(VidRateNum[curVidStream->picture_rate]+0.5);
	printf("Breakdown of system layer:\n");
	printf("    Video (no packet headers): %d bytes (%2.2f%%) %d bytes/sec\n",
	       vidBytes,vidBytes*100.0/totBytes, vidBytes/rate_disp);
	printf("    Audio (no packet headers): %d bytes (%2.2f%%) %d bytes/sec\n",
	       audBytes,audBytes*100.0/totBytes,audBytes/rate_disp);
	printf("    System (other headers, packets, etc): %d bytes (%2.2f%%) %d bytes/sec\n",
	       sysBytes,sysBytes*100.0/totBytes,sysBytes/rate_disp);
      }
      printf("Max instantaneous video rate %d, minimum %d bits/sec\n",
	     rate_max,rate_min);
    }
  } else {
    if (sys_layer) {
      int totBytes=vidBytes+audBytes+sysBytes;
      int rate_disp=(int)(VidRateNum[curVidStream->picture_rate]+0.5);
      printf("Breakdown of system layer:\n");
      printf("    Video (no packet headers): %d bytes (%2.2f%%) %d bytes/sec\n",
	     vidBytes,vidBytes*100.0/totBytes, vidBytes/rate_disp);
      printf("    Audio (no packet headers): %d bytes (%2.2f%%) %d bytes/sec\n",
	     audBytes,audBytes*100.0/totBytes,audBytes/rate_disp);
      printf("    System (other headers, packets, etc): %d bytes (%2.2f%%) %d bytes/sec\n",
	     sysBytes,sysBytes*100.0/totBytes,sysBytes/rate_disp);
    }
  }
  
  printf("\nTotal Compression Rate: %5.2f %% of uncompressed 24 bit images\n", 
		((float)supertot/(8.0*(float)supernum))*100.0 / 
                ((float)pixelnum*3.0) );

  printf("                        = %5.2f bits per pixel\n\n", 
		.24 * (((float)supertot/(8.0*(float)supernum))*100.0 / 
                ((float)pixelnum*3.0)) );

  printf("Number of Macroblocks");
  printf(" [width * height = sum]: %d x %d = %d per frame\n", 
         curVidStream->mb_width, curVidStream->mb_height,
	 curVidStream->mb_width * curVidStream->mb_height);
  printf("Skipped Macroblocks = %d (%2.2f%%), Coded Macroblocks = %d (%2.2f%%)\n",
	 blks.mb_skipped,blks.mb_skipped*100.0/(blks.mb_skipped+blks.mb_coded),
	 blks.mb_coded,blks.mb_coded*100.0/(blks.mb_skipped+blks.mb_coded));
  if (blks.cblks==0) printf("\tNo partially coded macroblocks.\n\n");
  else {
    printf("\tCoded blocks: %2.2f%%\t[",100.0*blks.cblks/blks.nblks);
    for (i=0; i<6; i++) {
      printf(" %2.2f ",100.0*blks.chist[i]/blks.cblks);
    }
    printf("]\n\n");
  }

  if (opts&TIME_MEASURE)
    printf("Total Time Decoding: %.4g secs. %.4g sec/frame or %.4g frames/sec.\n\n", 
	   supertime,supertime / ((double) supernum),((double) supernum) / supertime);

  printf("MPEG-Viewer requirements:\n");
  printf("\tPixel aspect ratio of %s\n",PelRatio[curVidStream->aspect_ratio]);
  printf("\tRequired display speed: %s\n",VidRate[curVidStream->picture_rate]);
  printf("\tSpecified bit rate is ");
  if (curVidStream->bit_rate==0x3FFFF) printf ("variable\n");
  else {
    int rt=curVidStream->bit_rate;
    if (rt<3) printf("%d bits/sec",400*rt);
    else if (rt<2500) printf("%4.2f KBits/sec",rt*0.4);
    else printf("%4.2f MBits/sec",rt*0.0004);
    printf(" (%d * 400bits/sec)\n",rt);
  }
  printf("\tRequested buffer size is %dK ints (16 bits/int).\n",
	 curVidStream->vbv_buffer_size);
  printf("\tAnd the constrained parameter flag is %s.\n",
	 curVidStream->const_param_flag?"on":"off");
  { /* Check to see if it actually *meets* the constrained parameters */
    int num_mb = curVidStream->mb_width * curVidStream->mb_height;
    int pict_rate = curVidStream->orig_picture_rate;

    if ((curVidStream->bit_rate <= 4640) &&
	(curVidStream->vbv_buffer_size <= 20) &&
	(pict_rate >= 1) &&
	(pict_rate <= 5) &&
	(curVidStream->h_size <= 768) &&
	(curVidStream->v_size <= 576) &&
	(num_mb * VidRateNum[pict_rate] <= 9900) &&
	f_code_ok &&
	(num_mb <= 396))
      printf("\tThe stream meets the constrained parameter requirements.\n");
    else {
      if ((curVidStream->vbv_buffer_size <= 20) &&
	  (num_mb*15 <= 9900) &&
	  (curVidStream->h_size <= 768) &&
	  (curVidStream->v_size <= 576) &&
	  f_code_ok &&
	  (pict_rate==9) &&  /* Illegal Xing sequence */
	  (num_mb <= 396)) {
	printf("\tOther than its invalid picture & bit rates, the stream meets the\n");
	printf("\t\tconstrained parameters (with rate corrected to 15).\n");
      } else {
	printf("\tThe stream does not meet the constrained parameter requirements,\n");
	printf("\tdue to the following factors:\n");
	if (curVidStream->bit_rate > 4640) 
	  printf("\t\tBit rate is %s.\n",
		 (curVidStream->bit_rate==0x3FFFF) ? "variable":"too high");
	if (curVidStream->vbv_buffer_size > 20) 
	  printf("\t\tVBV buffer is too large (%d bits).\n",
		 curVidStream->vbv_buffer_size*20*1024);
	if (pict_rate == 0) 
	  printf("\t\tPicture rate is invalid (0).\n");
	if (VidRateNum[pict_rate] > 31) 
	  printf("\t\tPicture rate is too high.\n");
	if (curVidStream->h_size > 768) 
	  printf("\t\tPicture is too wide.\n");
	if (curVidStream->v_size > 576) 
	  printf("\t\tPicture is too high (tall).\n");
	if (pict_rate > 8) 
	  printf("\t\tPicture rate is invalid (code is %d).\n",pict_rate);
	if (num_mb*VidRateNum[pict_rate] > 9900)
	  printf("\t\tPixels per second is too high.\n");
	if (!f_code_ok) 
	  printf("\t\tMotion vectors are larger than 8 (f_code>4)\n");
	if (num_mb > 396) 
	  printf("\t\tThere are too many macroblocks per second.\n");
      }}}
  
  if (opts & BITS_INFO) {
    extern long ones, zeros;
    printf("\t%ld (%4.2f%%) ones, %ld (%4.2f%%) zeros\n",
	   ones, (100.0*ones)/((float) ones+zeros),
	   zeros, (100.0*zeros)/((float) ones+zeros));
  }

  if (mvstat(0,5)==0 && mvstat(0,6)==0 && mvstat(0,7)==0 && mvstat(8,0)==0) {
    printf("\nNo motion vectors.\n");
  } else {
    printf("\nLength of vectors in %s pixels:\n", 
	   curVidStream->picture.full_pel_forw_vector ? "full" : "half");
    
    printf ("\tHorizontal forward vectors, maximum : %3d\taverage: %3d\n", 
	    mvstat (0, 5), mvstat(0, 10));
    printf ("\tVertical forward vectors, maximum   : %3d\taverage: %3d\n\n", 
	    mvstat (0, 6), mvstat(0, 11));
    if (mvstat(0,7)==0 && mvstat(8,0)==0) {
      printf("\tNo backward vectors.\n");
    } else {
      printf ("\tHorizontal backward vectors, maximum: %3d\taverage: %3d\n", 
	      mvstat (0, 7), mvstat(0, 12));
      printf ("\tVertical backward vectors, maximum  : %3d\taverage: %3d\n", 
	      mvstat (0, 8), mvstat(0, 13));
    }
  }

  printf("\n\nFrame specific information:\n\n");
  
  for (i = 1; i < 4; i++) {
    if (stat_a[i].number == 0)
      continue;

    printf("    %-3d %c FRAMES, average is:\n", stat_a[i].number, "IPB"[i-1]);

    average = stat_a[i].totsize / (8 * stat_a[i].number);
    printf("\tSize: %d bytes + %d bits (%2.2f%%)\n",
	   average, (stat_a[i].totsize / stat_a[i].number) % 8,
	   100.0*stat_a[i].totsize/supertot);

    printf("\tCompression Rate: %5.2f%%\n", 
    (float)average*100.0/ ( (float) pixelnum*3.0));

    printf("\tQ Factor [scales quantization matrix]: %2.2f\n", 
	   (1.0*stat_a[i].qual)/stat_a[i].qnum);

    if (stat_a[i].bi_mbnum > 0) {
      printf("\t%5.2f%% interpolated Macro Blocks\n", 
	((float) stat_a[i].bi_mbnum*100.0) / (stat_a[i].i_mbnum + 
	stat_a[i].p_mbnum + stat_a[i].b_mbnum + stat_a[i].bi_mbnum));
    }
    if (opts&TIME_MEASURE) 
      printf("\tTime to Decode: %f secs.\n",
	     (stat_a[i].tottime / ((double) stat_a[i].number)));
    printf("\n");
    if (opts&QSCALE_INFO) {
      fprintf(qscale_fp,"Block Quality Information (for %c Frames)\n\tQuality\tNumber\n",
	      "0IPB"[i]);
      for (j=1; j<32; j++) {
	if (blks.q[i][j]!=0) {
	  fprintf(qscale_fp,"\t%d\t%d\n",j,blks.q[i][j]);
	}}
      fprintf(qscale_fp,"\n");
    }
  }
  if (curVidStream->user_data != NULL) {
    printf("\tUser Data Specified (at sequence level):\n");
    print_binary(stdout, curVidStream->user_data, curVidStream->user_size);
  }

  if (opts&HIST_INFO) {
    PrintSummaryStat(1);
    PrintSummaryStat(2);
    PrintSummaryStat(3);
  }
}



/*
 * PrintSummaryStat
 *
 * Print the detailed Historgrams on particular frame types
 *
 */
void
PrintSummaryStat(typ)
int typ;
{
  int i;
  
  fprintf(hist_fp,"\n");
  switch (stat_a[typ].frametype) {
  case I_TYPE:
    fprintf(hist_fp,"I FRAME\n");
    break;
  case P_TYPE:
    fprintf(hist_fp,"P FRAME\n");
    break;
  case B_TYPE:
    fprintf(hist_fp,"B FRAME\n");
    break;
  }
  
  fprintf(hist_fp,"Size: %d bytes + %d bits\n", stat_a[typ].totsize / 8, stat_a[typ].totsize % 8);
  if (stat_a[typ].i_mbnum > 0) {
    fprintf(hist_fp,"\tI Macro Block Stats:\n");
    fprintf(hist_fp,"\t%d I Macroblocks\n", stat_a[typ].i_mbnum);
    fprintf(hist_fp,"\tAvg. Size: %d bytes + %d bits\n",
	    stat_a[typ].i_mbsize / (8 * stat_a[typ].i_mbnum),
	    (stat_a[typ].i_mbsize * stat_a[typ].i_mbnum) % 8);
    fprintf(hist_fp,"\t\tCoded Block Pattern Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      fprintf(hist_fp,"\t%6d %6d %6d %6d %6d %6d %6d %6d\n", stat_a[typ].i_mbcbp[i],
	      stat_a[typ].i_mbcbp[i + 1], stat_a[typ].i_mbcbp[i + 2], stat_a[typ].i_mbcbp[i + 3],
	      stat_a[typ].i_mbcbp[i + 4], stat_a[typ].i_mbcbp[i + 5], stat_a[typ].i_mbcbp[i + 6],
	      stat_a[typ].i_mbcbp[i + 7]);
    }
    fprintf(hist_fp,"\n\t\tNumber of Coefficients/Block Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      fprintf(hist_fp,"\t%6d %6d %6d %6d %6d %6d %6d %6d\n", stat_a[typ].i_mbcoeff[i],
	      stat_a[typ].i_mbcoeff[i + 1], stat_a[typ].i_mbcoeff[i + 2],
	      stat_a[typ].i_mbcoeff[i + 3], stat_a[typ].i_mbcoeff[i + 4],
	      stat_a[typ].i_mbcoeff[i + 5], stat_a[typ].i_mbcoeff[i + 6],
	      stat_a[typ].i_mbcoeff[i + 7]);
    }
  }
  if (stat_a[typ].p_mbnum > 0) {
    fprintf(hist_fp,"\tP Macro Block Stats:\n");
    fprintf(hist_fp,"\t%d P Macroblocks\n", stat_a[typ].p_mbnum);
    fprintf(hist_fp,"\tAvg. Size: %d bytes + %d bits\n",
	    stat_a[typ].p_mbsize / (8 * stat_a[typ].p_mbnum),
	    (stat_a[typ].p_mbsize / stat_a[typ].p_mbnum) % 8);
    fprintf(hist_fp,"\t\tCoded Block Pattern Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      fprintf(hist_fp,"\t%6d %6d %6d %6d %6d %6d %6d %6d\n", stat_a[typ].p_mbcbp[i],
	      stat_a[typ].p_mbcbp[i + 1], stat_a[typ].p_mbcbp[i + 2], stat_a[typ].p_mbcbp[i + 3],
	      stat_a[typ].p_mbcbp[i + 4], stat_a[typ].p_mbcbp[i + 5], stat_a[typ].p_mbcbp[i + 6],
	      stat_a[typ].p_mbcbp[i + 7]);
    }
    fprintf(hist_fp,"\n\t\tNumber of Coefficients/Block Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      fprintf(hist_fp,"\t%6d %6d %6d %6d %6d %6d %6d %6d\n", stat_a[typ].p_mbcoeff[i],
	      stat_a[typ].p_mbcoeff[i + 1], stat_a[typ].p_mbcoeff[i + 2],
	      stat_a[typ].p_mbcoeff[i + 3], stat_a[typ].p_mbcoeff[i + 4],
	      stat_a[typ].p_mbcoeff[i + 5], stat_a[typ].p_mbcoeff[i + 6],
	      stat_a[typ].p_mbcoeff[i + 7]);
    }
  }
  if (stat_a[typ].b_mbnum > 0) {
    fprintf(hist_fp,"\tB Macro Block Stats:\n");
    fprintf(hist_fp,"\t%d B Macroblocks\n", stat_a[typ].b_mbnum);
    fprintf(hist_fp,"\tAvg. Size: %d bytes + %d bits\n",
	    stat_a[typ].b_mbsize / (8 * stat_a[typ].b_mbnum),
	    (stat_a[typ].b_mbsize / stat_a[typ].b_mbnum) % 8);
    fprintf(hist_fp,"\t\tCoded Block Pattern Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      fprintf(hist_fp,"\t%6d %6d %6d %6d %6d %6d %6d %6d\n", stat_a[typ].b_mbcbp[i],
	      stat_a[typ].b_mbcbp[i + 1], stat_a[typ].b_mbcbp[i + 2], stat_a[typ].b_mbcbp[i + 3],
	      stat_a[typ].b_mbcbp[i + 4], stat_a[typ].b_mbcbp[i + 5], stat_a[typ].b_mbcbp[i + 6],
	      stat_a[typ].b_mbcbp[i + 7]);
    }
    fprintf(hist_fp,"\n\t\tNumber of Coefficients/Block Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      fprintf(hist_fp,"\t%6d %6d %6d %6d %6d %6d %6d %6d\n", stat_a[typ].b_mbcoeff[i],
	      stat_a[typ].b_mbcoeff[i + 1], stat_a[typ].b_mbcoeff[i + 2],
	      stat_a[typ].b_mbcoeff[i + 3], stat_a[typ].b_mbcoeff[i + 4],
	      stat_a[typ].b_mbcoeff[i + 5], stat_a[typ].b_mbcoeff[i + 6],
	      stat_a[typ].b_mbcoeff[i + 7]);
    }
  }
  if (stat_a[typ].bi_mbnum > 0) {
    fprintf(hist_fp,"\tBi Macro Block Stats:\n");
    fprintf(hist_fp,"\t%d Bi Macroblocks\n", stat_a[typ].bi_mbnum);
    fprintf(hist_fp,"\tAvg. Size: %d bytes + %d bits\n",
	    stat_a[typ].bi_mbsize / (8 * stat_a[typ].bi_mbnum),
	    (stat_a[typ].bi_mbsize * stat_a[typ].bi_mbnum) % 8);
    fprintf(hist_fp,"\t\tCoded Block Pattern Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      fprintf(hist_fp,"\t%6d %6d %6d %6d %6d %6d %6d %6d\n", stat_a[typ].bi_mbcbp[i],
	      stat_a[typ].bi_mbcbp[i + 1], stat_a[typ].bi_mbcbp[i + 2], stat_a[typ].bi_mbcbp[i + 3],
	      stat_a[typ].bi_mbcbp[i + 4], stat_a[typ].bi_mbcbp[i + 5], stat_a[typ].bi_mbcbp[i + 6],
	      stat_a[typ].bi_mbcbp[i + 7]);
    }
    fprintf(hist_fp,"\n\t\tNumber of Coefficients/Block Histogram:\n");
    for (i = 0; i < 64; i += 8) {
      fprintf(hist_fp,"\t%6d %6d %6d %6d %6d %6d %6d %6d\n", stat_a[typ].bi_mbcoeff[i],
	      stat_a[typ].bi_mbcoeff[i + 1], stat_a[typ].bi_mbcoeff[i + 2],
	      stat_a[typ].bi_mbcoeff[i + 3], stat_a[typ].bi_mbcoeff[i + 4],
	      stat_a[typ].bi_mbcoeff[i + 5], stat_a[typ].bi_mbcoeff[i + 6],
	      stat_a[typ].bi_mbcoeff[i + 7]);
    }
  }
}

/*
 * init_stat_struct
 *
 * clear for use a sinlge statistics structure
 */
static void
init_stat_struct(astat)
  Statval *astat;
{
  int j;

  astat->frametype = 0;
  astat->totsize = 0;
  astat->number = 0;
  astat->i_mbsize = 0;
  astat->p_mbsize = 0;
  astat->b_mbsize = 0;
  astat->bi_mbsize = 0;
  astat->i_mbnum = 0;
  astat->p_mbnum = 0;
  astat->b_mbnum = 0;
  astat->bi_mbnum = 0;

  for (j = 0; j < 64; j++) {

    astat->i_mbcbp[j] = 0;
    astat->p_mbcbp[j] = 0;
    astat->b_mbcbp[j] = 0;
    astat->bi_mbcbp[j] = 0;
    astat->i_mbcoeff[j] = 0;
    astat->p_mbcoeff[j] = 0;
    astat->b_mbcoeff[j] = 0;
    astat->bi_mbcoeff[j] = 0;
  }
  astat->tottime = 0.0;
  astat->qual=0;  
  astat->qnum=0;
}


/*
 * CollectStats
 *
 * Move the proggressively collected data into its home
 *
 */
void
CollectStats()
{
  int i, j;

  i = stat_a[0].frametype;

  stat_a[i].totsize += stat_a[0].totsize;
  stat_a[i].number += stat_a[0].number;
  stat_a[i].i_mbsize += stat_a[0].i_mbsize;
  stat_a[i].p_mbsize += stat_a[0].p_mbsize;
  stat_a[i].b_mbsize += stat_a[0].b_mbsize;
  stat_a[i].bi_mbsize += stat_a[0].bi_mbsize;
  stat_a[i].i_mbnum += stat_a[0].i_mbnum;
  stat_a[i].p_mbnum += stat_a[0].p_mbnum;
  stat_a[i].b_mbnum += stat_a[0].b_mbnum;
  stat_a[i].bi_mbnum += stat_a[0].bi_mbnum;

  for (j = 0; j < 64; j++) {

    stat_a[i].i_mbcbp[j] += stat_a[0].i_mbcbp[j];
    stat_a[i].p_mbcbp[j] += stat_a[0].p_mbcbp[j];
    stat_a[i].b_mbcbp[j] += stat_a[0].b_mbcbp[j];
    stat_a[i].bi_mbcbp[j] += stat_a[0].bi_mbcbp[j];
    stat_a[i].i_mbcoeff[j] += stat_a[0].i_mbcoeff[j];
    stat_a[i].p_mbcoeff[j] += stat_a[0].p_mbcoeff[j];
    stat_a[i].b_mbcoeff[j] += stat_a[0].b_mbcoeff[j];
    stat_a[i].bi_mbcoeff[j] += stat_a[0].bi_mbcoeff[j];
  }

  stat_a[i].tottime += stat_a[0].tottime;

  init_stat_struct(&(stat_a[0]));

}


/*********************************/
/* A bunch of "obvious" routines */
/*********************************/

unsigned int
bitCountRead()
{
  return bitCount;
}

void
StartTime()
{
  stat_a[0].tottime = ReadSysClock();
}

void
EndTime()
{
  stat_a[0].tottime = ReadSysClock() - stat_a[0].tottime;
}

double
ReadSysClock()
{
  struct timeval tv;
  (void) gettimeofday(&tv, (struct timezone *)NULL);
  return (tv.tv_sec + tv.tv_usec / 1000000.0);
}

void
PrintTimeInfo()
{
  double spent;

  spent = ReadSysClock() - realTimeStart;

}




/*
 *
 *-------------------------------------------------------------
 *
 * PrintQT --
 *
 *    Called to print out Q tables
 *
 * Results:
 *    adds info to stream.
 *
 * Side effects:
 *     Writes to stream.
 *
 *-------------------------------------------------------------
 */
void PrintQT(stream,title,changed,ptr)
FILE *stream;
char *title;
int changed;
unsigned char ptr[];
{
  int i;
  
  if (changed) {
    fprintf(stream,"\nCustom %s matrix (offset %d, frame %d):\n",
	    title,bitCountRead(), blks.frame+1);
    for (i = 0; i < 64; i++) {
      if (i%8!=7) fprintf(stream,"%2d ",ptr[i]);
      else fprintf(stream,"%2d\n",ptr[i]);
    }
    fprintf(stream,"\n");
  } else fprintf(stream,"\nCustom %s matrix repeated at (offset %d, frame %d)\n",
	    title,bitCountRead(),blks.frame+1);
}



/* Utility routine to print possibly binary data */
void print_binary(fp, buf, size)
FILE *fp;
char *buf;
int size;
{
  int i,line,num_lines;
  char printables[17];
  
  num_lines=size/16;
  printables[16]=0;
  for (i=0;i<16;i++) printables[i]=' ';
  for (line=0; line<=num_lines; line++) {
    for (i=0; (i<16) && (i<size); i++) {
      fprintf(fp, "%2x ",*buf);
      printables[i]= isprint(*buf) ? *buf: '.';
      buf++;
    }
    if (i!=16) for(;i<16; i++) fprintf(fp, "   ");
    size-=16;
    fprintf(fp, "\t%s\n",printables);
    for (i=0;i<16;i++) printables[i]=' ';
  }}

