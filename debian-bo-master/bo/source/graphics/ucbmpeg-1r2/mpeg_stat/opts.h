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

extern int opts;
extern FILE *block_fp;
extern FILE *qscale_fp;
extern FILE *size_fp;
extern FILE *offs_fp;
extern FILE *rate_fp;
extern FILE *syslogOutput;
extern FILE *hist_fp;
extern FILE *userdat_fp;
extern int start_opt;
extern int end_opt;
extern int rate_frames;

#define BLOCK_INFO 1
#define LOUD 2
#define START_F 4
#define END_F 8
#define QSCALE_INFO 16
#define SIZE_INFO 32
#define OFFS_INFO 64
#define RATE_INFO 128
#define RATE_LENGTH_SET 256
#define TIME_MEASURE    512
#define SYSLAYER_LOG   1024
#define HIST_INFO      2048
#define VERIFY         4096
#define DCT_INFO       8192
#define USERDAT_INFO  16384
#define BITS_INFO     32768

extern int COLLECTING;
#define COLLECT_ON 0xFFFFFFF
#define COLLECT_OFF 0
