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

#include "video.h"
#include "proto.h"
#include <sys/types.h>
#include <signal.h>
#ifndef MIPS
#include <netinet/in.h>
#else
#include <bsd/netinet/in.h>
#endif

#include "util.h"
#include "dither.h"


/* 
  Are we a system layer parser or pure video?
  (-1 is uninit, 0 is video, 1 is sys_layer)
*/
int sys_layer=-1;
/* Keep track of bytes parsed in system layer */
int audBytes,vidBytes,sysBytes;

/* End of File flag. */
int EOF_flag=0;

/* Global file pointer to incoming data. */
extern FILE *input;

/* Options to control logging */
FILE *syslogOutput;
int opts=0;
#define SYSLAYER_LOG 0

/* Silly Constants.... */
#define PACK_START_CODE             (unsigned int)0x000001ba
#define SYSTEM_HEADER_START_CODE    (unsigned int)0x000001bb
#define PACKET_START_CODE_MASK      (unsigned int)0xffffff00
#define PACKET_START_CODE_PREFIX    (unsigned int)0x00000100
#define ISO_11172_END_CODE          (unsigned int)0x000001b9
  
#define PACK_HEADER_SIZE 8
  
#define STD_AUDIO_STREAM_ID 0xb8
#define STD_VIDEO_STREAM_ID 0xb9
#define MIN_STREAM_ID_ID    0xbc
#define RESERVED_STREAM_ID  0xbc
#define PRIVATE_STREAM_1_ID 0xbd
#define PADDING_STREAM_ID   0xbe
#define PRIVATE_STREAM_2_ID 0xbf
  
#define STD_SYSTEM_CLOCK_FREQ (unsigned long)90000
#define MUX_RATE_SCALE_FACTOR 50
#define MAX_STREAMS 8
#define NOT_PACKET_ID       0xff
#define KILL_BUFFER         0xfe
  
  /*
   *--------------------------------------------------------------
   *
   * get_more_data --
   *
   *	Called by get_more_data to read in more data from
   *      video MPG files (non-system-layer)
   *
   * Results:
   *	Input buffer updated, buffer length updated.
   *      Returns 1 if data read, 0 if EOF, -1 if error.
   *
   * Side effects:
   *      None.
   *
   *--------------------------------------------------------------
   */
int 
  get_more_data(bs_ptr, max_length, length_ptr, buf_ptr)
unsigned int **bs_ptr;
int *max_length, *length_ptr;
unsigned int **buf_ptr;
{
  static BOOLEAN swap;
  int ioBytes, data, result;
  unsigned int *mark;
  
  if (sys_layer==0) {
    return pure_get_more_data(*bs_ptr, *max_length, length_ptr, buf_ptr, swap);
  }
  if (sys_layer==-1) {
    /* Time to init ourselves */
    swap=(htonl(1)!=1);
    mark=*bs_ptr;
    ioBytes=fread(&data,1,4,input);
    if (ioBytes!=4) return 0;
    data=ntohl(data);
    if ((data==PACK_START_CODE) || (data==SYSTEM_HEADER_START_CODE)) {
      /* Yow, a System Layer Stream.  Much harder to parse.  Call in the
	 specialist.... */
      fprintf(stderr,"This is an MPEG System Layer Stream.  ");
      fprintf(stderr,"Audio is not analyzed.\n");
      sys_layer=1;
      init_read_sys();
      result= read_sys(bs_ptr, max_length, length_ptr, buf_ptr, data);
      return result;
    } else {
      /* No system Layer junk, just pretent we didn't peek,
	 and hereafter just call pure_get_more_data */
      sys_layer=0;
      **bs_ptr=data;
      *length_ptr=1;
      result= pure_get_more_data(*bs_ptr, *max_length, 
				 length_ptr, buf_ptr, swap);
      *buf_ptr=*bs_ptr;
      return result;
    }}
  /* A system layer stream (called after the 1st time), call the specialist */
  result=read_sys(bs_ptr, max_length, length_ptr, buf_ptr, 0);
  return result;
}

/*
 *-------------------------------------------------------------
 *
 * clear_data_stream
 *
 * Empties out internal buffers
 *
 */
void 
  clear_data_stream(bs_ptr, max_length, length_ptr, buf_ptr)
unsigned int **bs_ptr;
int *max_length, *length_ptr;
unsigned int **buf_ptr;
{
  /* Only internal buffer is in ReadPacket */
  if (sys_layer) {
    ReadPacket(KILL_BUFFER, bs_ptr, max_length, length_ptr, buf_ptr);
  }
}


/*
 *--------------------------------------------------------------
 *
 * pure_get_more_data --
 *      (get_more_data from ver 2.0 with swap added)
 *
 *	Called by get_more_data to read in more data from
 *      video MPG files (non-system-layer)
 *
 * Results:
 *	Input buffer updated, buffer length updated.
 *      Returns 1 if data read, 0 if EOF, -1 if error.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

int 
  pure_get_more_data(buf_start, max_length, length_ptr, buf_ptr, swap)
unsigned int *buf_start;
int max_length;
int *length_ptr;
unsigned int **buf_ptr;
BOOLEAN swap;
{
  
  int length, num_read, i, request;
  unsigned char *buffer, *mark;
  unsigned int *lmark;
  
  if (EOF_flag) return 0;
  
  length = *length_ptr;
  buffer = (unsigned char *) *buf_ptr;
  
  if (length > 0) {
    memcpy((unsigned char *) buf_start, buffer, (length*4));
    mark = ((unsigned char *) (buf_start + length));
  }
  else {
    mark = (unsigned char *) buf_start;
    length = 0;
  }
  
  request = (max_length-length)*4;
  
  
  num_read = fread( mark, 1, request, input);
  
  /* Paulo Villegas - 26/1/1993: Correction for 4-byte alignment */
  {
    int num_read_rounded;
    unsigned char *index;
    
    num_read_rounded = 4*(num_read/4);
    
    /* this can happen only if num_read<request; i.e. end of file reached */
    if( num_read_rounded < num_read )
      { 
 	num_read_rounded = 4*( num_read/4+1 );
 	/* fill in with zeros */
 	for( index=mark+num_read; index<mark+num_read_rounded; *(index++)=0 );
 	/* advance to the next 4-byte boundary */
 	num_read = num_read_rounded;
      }
  }
  
  if   (num_read < 0) {
    return -1;
  }
  else if (num_read == 0) {
    *buf_ptr = buf_start;
    
    /* Make 32 bits after end equal to 0 and 32
       bits after that equal to seq end code
       in order to prevent messy data from infinite
       recursion.
       */
    
    *(buf_start + length) = 0x0;
    *(buf_start + length+1) = SEQ_END_CODE;
    
    EOF_flag = 1;
    return 0;
  }
  
  lmark = (unsigned int *) mark;
  
  num_read = num_read/4;
  
  if (swap) {
    for (i=0; i<num_read; i++) {
      *lmark = htonl(*lmark);
      lmark++;
    }
  }
  
  *buf_ptr = buf_start;
  *length_ptr = length + num_read;
  
  return 1;
}




/* 
  Here is the specialist.... 
  Code is adapted from our program demux....
  A bunch of this needs to be #ifdef ANALYSIS'ed
  define __SYSREAD_LOGGING_ON__ to get  an output file for debugging
  */


/* Stream IDs */
static int gAudioStreamID;
static int gVideoStreamID;
static int gReservedStreamID;

#ifdef ANALYSIS
/* Statistics */
static int gNumAudioPackets;
static int gNumVideoPackets;
static int gNumPaddingPackets;
static int gNumReservedPackets;
static int gNumPrivate_1_Packets;
static int gNumPrivate_2_Packets;
#endif

/*
 *----------------------------------------------------------
 *
 *  init_read_sys
 *
 *      Called before read_sys is used to parse the file.
 *      Currently only sets up the logging file (when defined)
 *
 *  Results:  None
 *
 *  Side Effects: Zeros Byte counts, opens Hack Output if needed
 *
 *----------------------------------------------------------
 */
void init_read_sys() {
  audBytes=0; vidBytes=0; sysBytes=0;
}


/*
 *----------------------------------------------------------
 *
 *  read_sys
 *
 *      Parse out a packet of the system layer MPEG file.
 *
 *  Results:  Returns 0 if error or EOF
 *            Returns 1 if more data read (could be just one int)
 *
 *  Side Effects:  ReadPacket can change *bs_ptr to be a new buffer
 *                 buf_ptr will remain pointing at *length_ptr (at input)
 *                         into the buffer
 *                 *length_ptr will be changed to the new size
 *                 *max_length can be changed if a new buffer is alloc'd
 *
 *----------------------------------------------------------
 */
int read_sys(bs_ptr, max_length, length_ptr, buf_ptr, start)
     unsigned int **bs_ptr;
     int *max_length, *length_ptr;
     unsigned int **buf_ptr, start;  
     /* start is either a start code or 0 to indicate continued parsing */
{
  unsigned int startCode;
  int errorCode, PacketReply;
  unsigned char packetID;
  double systemClockTime;
  unsigned long muxRate;
  /* Statistics */
  static int numPacks = 0;
  static int numPackets = 0;
  static int numSystemHeaders = 0;
  static BOOLEAN Parse_done=FALSE;
  BOOLEAN match;
  
  if (!start) {
    errorCode = ReadStartCode(&startCode);
    if (EOF_flag) return 0;
    if (errorCode != 0) {
      fprintf(stderr, "Unable to read initial pack start code\n");
      return 0;
    }}
  else {
    errorCode = 0;
    startCode = start;
  }
  
  while (1) {
    match=FALSE;
    if (startCode == PACK_START_CODE) {
      ++numPacks; match=TRUE;
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "PACK #%d:\n", numPacks);
      }
      errorCode = ReadPackHeader( &systemClockTime, &muxRate);
      if (errorCode != 0) {
	fprintf(stderr, "Error in reading pack header\n");
	return 0;
      }
      errorCode = ReadStartCode( &startCode);
      if (errorCode != 0) {
	fprintf(stderr, "Error in reading start code\n");
	return 0;
      }
    }
    if (startCode == SYSTEM_HEADER_START_CODE) {
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "SYSTEM HEADER:\n");
      }
      ++numSystemHeaders; match=TRUE;
      errorCode = ReadSystemHeader();
      if (errorCode != 0) {
	fprintf(stderr, "Error in reading system header\n");
	return 0;
      }
      errorCode = ReadStartCode( &startCode);
      if (errorCode != 0) {
	fprintf(stderr,"Error in reading start code after system header\n");
	return 0;
      }
    }
    packetID = startCode & 0xff;
    while (((startCode & PACKET_START_CODE_MASK) == PACKET_START_CODE_PREFIX) &&
	   (packetID>=0xbc)) {
      ++numPackets; match=TRUE;
      packetID = startCode & 0xff;
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "PACKET ID %02x:\n", packetID);
      }
      PacketReply = ReadPacket(packetID, bs_ptr, max_length, length_ptr, buf_ptr);
      switch (PacketReply) {
      case 2: 
	return 1;
      case 1: 
	if (opts&SYSLAYER_LOG) {
	  fprintf(syslogOutput, "Problems in ReadPacket, returning partial.\n");
	}
	return 0;
      default: /* do nothing */
	break;
      }
      errorCode = ReadStartCode( &startCode);
      if (errorCode != 0) {
	fprintf(stderr,"Error in start code after packet\n");
	return 0;
      }
      if (startCode == PACK_START_CODE || startCode == ISO_11172_END_CODE)
	break;
    }
    if (startCode == ISO_11172_END_CODE) {
      match=TRUE;
      if (Parse_done) return 1;
#ifdef ANALYSIS
      fprintf(stderr, "Successful parse of MPEG system level\n");
      fprintf(stderr, "%d system headers, %d packs, %d packets\n",
	      numSystemHeaders, numPacks, numPackets);
      fprintf(stderr, "%d audio packets, %d video packets, %d padding packets\n",
	      gNumAudioPackets, gNumVideoPackets, gNumPaddingPackets);
      fprintf(stderr, "%d reserved packets, %d/%d private type 1/2 packets\n",
	      gNumReservedPackets, gNumPrivate_1_Packets, gNumPrivate_2_Packets);
#endif
      ReadPacket(NOT_PACKET_ID, bs_ptr, max_length, length_ptr, buf_ptr);
      Parse_done=TRUE;
      return 1;
    }
    if (errorCode != 0)
      return 1;
    if (!match) {
      fprintf(stderr,"\nNo match found for start code %08x in system layer, skipping\n",startCode);
      if (opts&SYSLAYER_LOG)
	fprintf(syslogOutput, "Error with start code %08x, did not match at %d\n",
		startCode, (int) ftell(input));  
      startCode=(int) find_start_code();
      if (opts&SYSLAYER_LOG) {
	if (startCode==EOF) 
	  fprintf(syslogOutput, "Found EOF in find_start_code\n");
	else fprintf(syslogOutput, "Found %08x at %d\n",
		     startCode, (int) ftell(input));  
      }
      if (startCode==EOF) {
	EOF_flag=1;
	return 0;
      }
    }
  }
  return 0; /* shouldnt get here */
}

/*
 *-----------------------------------------------------------
 *
 *  ReadStartCode
 *
 *      Parses a start code out of the stream
 *
 *  Results/Side Effects:  Sets *startCode to the code, returns
 *     1 on error, 0 on success
 *
 *-----------------------------------------------------------
 */
int ReadStartCode(startCode)
     unsigned int *startCode;
{
  int numRead;
  
  numRead = fread((unsigned char *)startCode, 1, 4, input);
  *startCode=htonl(*startCode);
  
  if (numRead < 4) {
    if ((opts&SYSLAYER_LOG)&&(EOF_flag==0)) {
      fprintf(syslogOutput, "Error in reading start code, only got %d bytes\n", 
              numRead);
    }
    EOF_flag=1;
    return 1;
  }
  if (opts&SYSLAYER_LOG) {
    fprintf(syslogOutput, "Read as start code: %08x\n", *startCode);
  }
  if ((*startCode&0xfffffe00) != 0) {
    int start_pos;
    fprintf(stderr,"Problem with system layer parse, skipping to start code\n");
    if (opts&SYSLAYER_LOG) {
      start_pos = (int) ftell(input);
      fprintf(syslogOutput, "Error with start code, not a start code! at %d\n",
	      start_pos);  
    }
    *startCode=(int) find_start_code();
    if (*startCode==EOF) {
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "Skipped all the way to EOF!\n");
	}
      EOF_flag=TRUE;
      return 0;
    }
    if (opts&SYSLAYER_LOG) {
      int end_pos=(int) ftell(input);
      fprintf(syslogOutput, "Found %08x at %d, skipped over %d bytes.\n",
	      *startCode, end_pos, end_pos-start_pos);
    }
  }
  sysBytes+=4;
  return 0;
}

/*
 *-----------------------------------------------------------
 *
 *  find_start_code
 *
 *      Parses a start code out of the stream by tossing bytes until it gets one
 *
 *  Results/Side Effects:  Parses bytes of the stream, returns code
 *                         Returns EOF in case of end of file
 *
 *-----------------------------------------------------------
 */
int find_start_code(void)
{
 NO_ZEROS:
  switch(fgetc(input)) {
  case 0:    goto ONE_ZERO;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }

 ONE_ZERO:
  switch(fgetc(input)) {
  case 0:    goto TWO_ZEROS;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }

 TWO_ZEROS:
  switch(fgetc(input)) {
  case 0x01:  goto CODE_FOUND;
  case 0x00:  goto TWO_ZEROS;
  case EOF:  goto EOF_FOUND;
  default:    goto NO_ZEROS;
  }

 CODE_FOUND:
  return 0x00000100+fgetc(input);

 EOF_FOUND:   /* received EOF */
  return EOF;
}



/*
 *-----------------------------------------------------------------
 *
 *  ReadPackHeader
 *
 *      Parses out the PACK header
 *
 *  Returns: 1 on error, 0 on success
 *
 *-------------------------------------------------------------------
 */
int ReadPackHeader(systemClockTime,muxRate)
     double *systemClockTime;
     unsigned long *muxRate;
{
  int numRead;
  unsigned char inputBuffer[PACK_HEADER_SIZE];
  unsigned long systemClockRef;
  unsigned char systemClockRefHiBit;
  int errorCode;
  
  numRead = fread(inputBuffer, 1, PACK_HEADER_SIZE, input);
  if (numRead < PACK_HEADER_SIZE) {
    EOF_flag=1;
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, 
              "Error in reading Pack header, only got %d bytes\n", numRead);
    }
    return 1;
  }
  sysBytes+=numRead;
  ReadTimeStamp(inputBuffer, &systemClockRefHiBit, &systemClockRef);
  errorCode = MakeFloatClockTime(systemClockRefHiBit, systemClockRef, 
				 systemClockTime);
  ReadRate(&inputBuffer[5], muxRate);
  *muxRate *= MUX_RATE_SCALE_FACTOR;
  if (opts&SYSLAYER_LOG) {
    fprintf(syslogOutput, "\tSystem clock reference: %d, %lu (0x%x%08x)\n",
	    (int)systemClockRefHiBit, systemClockRef,
	    (int)systemClockRefHiBit, systemClockRef);
    if (errorCode == 0) {
      fprintf(syslogOutput, "\tSystem clock time: %1.4lf\n", *systemClockTime);
    } else {
      fprintf(syslogOutput, "Error reading system clock time\n");
    }
    fprintf(syslogOutput, "\tmuxRate: %lu (0x%08x)\n", *muxRate, *muxRate);
  }
  return 0;
}

/*
 *------------------------------------------------------------------
 *
 *   ReadSystemHeader
 *
 *      Parse out the system header, setup out dtream IDs for parsing packets
 *
 *   Results:  Returns 1 on error, 0 on success.
 *             Sets gAudioStreamID and gVideoStreamID
 *
 *------------------------------------------------------------------
 */
int ReadSystemHeader()
{ 
  unsigned char *inputBuffer = NULL;
  int numRead;
  int pos,i;
  unsigned short headerSize;
  unsigned char streamID;
  /* Only needed for system log file */
  unsigned long rateBound;
  unsigned long audioBound;
  unsigned char fixedFlag;
  unsigned char cspsFlag;
  unsigned long videoBound;
  unsigned char sysAudioLockFlag;
  unsigned char sysVideoLockFlag;
  unsigned char stdBufferScale;
  unsigned long stdBufferSize;
  
  numRead = fread((char *)&headerSize, 1, 2, input); 
  headerSize=ntohs(headerSize);
  if (numRead != 2) {
    EOF_flag=1;
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, 
              "error in reading System header size, only got %d bytes\n", 
              numRead);
    }
    return 1;
  }
  inputBuffer = (unsigned char *) malloc(headerSize+1);
  sysBytes+=headerSize;
  if (inputBuffer == NULL) {
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, 
              "error in allocating %d bytes\n", headerSize);
    }
    return 1;
  }
  inputBuffer[headerSize]=0;
  numRead = fread(inputBuffer, 1, headerSize, input); 
  if (opts&SYSLAYER_LOG) {
    for(i=0;i<headerSize;i++) 
      fprintf(syslogOutput, 
              "%x ",*(inputBuffer+i));
    fprintf(syslogOutput,"\n");
  }
  if (numRead < headerSize) {
    EOF_flag=1;
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, 
              "error in reading System Header, only got %d bytes\n", 
              numRead);
    }
    return 1;
  }
  if (opts&SYSLAYER_LOG) {
    ReadRate(&inputBuffer[0], &rateBound);
    rateBound *= MUX_RATE_SCALE_FACTOR;
    fprintf(syslogOutput, "\trate_bound: %lu (0x%08x)\n", rateBound, rateBound);
    audioBound = (unsigned long)inputBuffer[3] >> 2;
    fprintf(syslogOutput, "\taudio_bound: %lu (0x%08x)\n", audioBound, audioBound);
    fixedFlag = (inputBuffer[3] >> 1) & 0x01;
    fprintf(syslogOutput, "\tfixed_flag: %d\n", fixedFlag);
    cspsFlag = inputBuffer[3] & 0x01;
    fprintf(syslogOutput, "\tCSPS_flag: %d\n", cspsFlag);
    videoBound = (unsigned long)inputBuffer[4] & 0x1f;
    fprintf(syslogOutput, "\tvideo_bound: %lu (0x%08x)\n", videoBound, videoBound);
    sysAudioLockFlag = (inputBuffer[4] & 0x80) >> 7;
    fprintf(syslogOutput, "\tsystem_audio_lock_flag: %d\n", sysAudioLockFlag);
    sysVideoLockFlag = (inputBuffer[4] & 0x40) >> 6;
    fprintf(syslogOutput, "\tsystem_video_lock_flag: %d\n", sysVideoLockFlag);
  }
  
  pos = 6;
  while ((inputBuffer[pos] & 0x80) == 0x80) {
    streamID = inputBuffer[pos];
    if (opts&SYSLAYER_LOG) {
      ReadSTD(&inputBuffer[pos + 1], &stdBufferScale, &stdBufferSize);
      fprintf(syslogOutput, 
              "\tRead STD_buffer_scale = %d, STD_buffer_size = %lu (0x%0x)\n",
              (int)stdBufferScale, stdBufferSize, stdBufferSize);
      fprintf(syslogOutput, "\tSystem Header: stream with ID 0x%x\n", streamID); 
    }
    switch (streamID) {
    case STD_VIDEO_STREAM_ID: 
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tSystem Header: Std video stream\n");
      }
      break;
    case STD_AUDIO_STREAM_ID: 
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tSystem Header: Std audio stream\n");
      }
      break;
    case RESERVED_STREAM_ID: 
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tSystem Header: Reserved stream\n");
      }
      break;
    case PADDING_STREAM_ID: 
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tSystem Header: Padding stream\n");
      }
      break;
    case PRIVATE_STREAM_1_ID: 
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tSystem Header: Private (1) stream\n");
      }
      break;
    case PRIVATE_STREAM_2_ID: 
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tSystem Header: Private (2) stream\n");
      }
      break;
    default:
      if (streamID < MIN_STREAM_ID_ID) {
	if (opts&SYSLAYER_LOG) {
	  fprintf(syslogOutput, "\tSystem Header: Illegal stream ID\n");
	}
	return 1;
      }
      switch (streamID >> 4) {
      case 0xc:
      case 0xd:
	if (opts&SYSLAYER_LOG) {
	  fprintf(syslogOutput, "\tSystem Header: audio stream #%d\n",
		  (streamID & 0x1f));
	}
	gAudioStreamID = streamID;
	break;
      case 0xe:
	if (opts&SYSLAYER_LOG) {
	  fprintf(syslogOutput, "\tSystem Header: video stream #%d\n",
		  (streamID & 0xf));
	}
	if ((gVideoStreamID != 0) && (gVideoStreamID!=streamID)) {
	  if (opts&SYSLAYER_LOG) {
	    fprintf(syslogOutput, 
		    "\tThis program can only handle a single video stream\n");
	  }
	  break;
	}
	gVideoStreamID = streamID;
	break;
      case 0xf:
	gReservedStreamID = streamID;
	if (opts&SYSLAYER_LOG) {
	  fprintf(syslogOutput, "S\tystem Header: reserved stream #%d\n",
		  (streamID & 0xf));
	}
	break;
      }
      break;
    }
    pos += 3;
  }
  if (inputBuffer != NULL)
    free(inputBuffer);
  return 0;
}

/*
 *-----------------------------------------------------------------
 *
 *  ReadPacket
 *
 *      Reads a single packet out of the stream, and puts it in the
 *      buffer if it is video.
 *
 *  Results:
 *      Changes the value of *length_ptr to be the new length (plus old)
 *      If the buffer is too small, can change *bs_ptr, *max_length, and *buf_ptr
 *      to be correct for a newly allocated buffer.
 *
 *  State:  The buffer is in ints, but the packets can be an arbitrary number
 *      of bytes, so leftover bytes are kept in static vars and added in on the
 *      next call.
 *
 *-----------------------------------------------------------------
 */   
int ReadPacket(packetID, bs_ptr, max_length, length_ptr, buf_ptr) 
     unsigned char packetID;
     unsigned int **bs_ptr;
     int *max_length;
     int *length_ptr;
     unsigned int **buf_ptr;
     /* Returns:
	0 - no error, but not video packet we want
	1 - error
	2 - got video packet into buffer
	*/
{   
  int ioBytes;
  unsigned char nextByte;
  unsigned short packetLength;
  unsigned char *packetBuffer = NULL;
  int pos;
  int numStuffBytes = 0;
  int packetDataLength;
  int byte_length;
  unsigned char scratch[10];
  int errorCode; /* For syslog */
  /* Leftovers from previous video packets */
  static unsigned int num_left=0, leftover_bytes=0;
  
  if (packetID==NOT_PACKET_ID) {
    /* Gross hack to handle unread bytes before end of stream */
    num_left=0;
    if (num_left!=0) {
      /* Sigh, deal with previous leftovers */
      *(*buf_ptr+*length_ptr)=leftover_bytes;
      *(*buf_ptr+*length_ptr+1)=ISO_11172_END_CODE;
      *length_ptr+=2;
    } else {
      *(*buf_ptr+*length_ptr)=ISO_11172_END_CODE;
      *length_ptr+=1;
    }
    return 1;
  } else if (packetID==KILL_BUFFER) {
    num_left=0;
    leftover_bytes=0;
    return 0;
  }
  
  ioBytes = fread(&packetLength, 1, 2, input);
  packetLength=htons(packetLength);
  if (ioBytes < 2) {
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, "ReadPacket: Error in reading packet length\n");
    }
    return 1;
  }
  if (opts&SYSLAYER_LOG) {
    fprintf(syslogOutput, 
	    "\tinput packet with ID %02x has length = %d at file offset %d\n", 
	    packetID, packetLength, (int) ftell(input));
  }
  if (packetID == gAudioStreamID) {
#ifdef ANALYSIS
    ++gNumAudioPackets;
#endif
  }
  else if (packetID == gVideoStreamID) {
#ifdef ANALYSIS     
    ++gNumVideoPackets;
#endif
  }
  else {
    switch (packetID) {
    case PADDING_STREAM_ID:
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tPadding packet.\n");
      }
#ifdef ANALYSIS
      ++gNumPaddingPackets;
#endif
      break;
    case RESERVED_STREAM_ID:
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tReserved packet.\n");
      }
#ifdef ANALYSIS
      ++gNumReservedPackets;
#endif
      break;
    case PRIVATE_STREAM_1_ID:
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tPrivate packet type 1.\n");
      }
#ifdef ANALYSIS
      ++gNumPrivate_1_Packets;
#endif
      break;
    case PRIVATE_STREAM_2_ID:
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput, "\tPrivate packet type 2.\n");
      }
#ifdef ANALYSIS
      ++gNumPrivate_2_Packets;
#endif
      break;
    default:
      fprintf(stderr, "\nUnknown packet type encountered. P'bly audio? (%x) at %d\n",
	      packetID,(int) ftell(input));
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput,"\tUnknown packet type encountered. P'bly audio. (%x), length %d at offset %d.\n",
		packetID,packetLength,(int) ftell(input));
	fflush(syslogOutput);
      }
    } /* switch */
  } /* else */

  if (packetID != gVideoStreamID) {		/* changed by jim */
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, "\tSkipping over this packet.\n");
    }
    fseek(input, packetLength, 1);
    sysBytes+=packetLength;
    return 0;
  }

  fread(&nextByte,1,1,input);
  pos = 0;
  while (nextByte & 0x80) {
    ++numStuffBytes;
    if (opts&SYSLAYER_LOG) {
      if (nextByte != 0xff)
	fprintf(syslogOutput, "\tWarning: stuffing byte = 0x%x not 0xff\n", 
		(int)nextByte);
    }
    ++pos;
    fread(&nextByte,1,1,input);
  }
  if (numStuffBytes > 0)
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, "\tSkipped %d stuffing bytes\n", numStuffBytes);
    }
  if ((nextByte >> 6) == 0x01) {
    pos += 2;
    fread(&nextByte,1,1,input);
    fread(&nextByte,1,1,input);
  } 
  if ((nextByte >> 4) == 0x02) {
    scratch[0] = nextByte;			/* jim */
    fread(&scratch[1],1,4,input);		/* jim */
    fread(&nextByte,1,1,input);
    pos+=5;
  }
  else if ((nextByte >> 4) == 0x03) {
    scratch[0] = nextByte;			/* jim */
    fread(&scratch[1],1,9,input);		/* jim */
    fread(&nextByte,1,1,input);
    pos += 10;
  } 
  else {
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, "\tRead 0x%02x (s.b. 0x0f)\n", nextByte);
    }
    fread(&nextByte,1,1,input);
    pos += 1;
  }
#define JIM_STUFF
#define xJIM_DEBUG
#ifdef JIM_STUFF
  if(((scratch[0] >> 4) == 0x02) || ((scratch[0] >> 4) == 0x03)) {
    unsigned long pts, dts;
    unsigned char pts_high, dts_high;
    
    fprintf(syslogOutput, "\ttimestamp at offset %d\n", ftell(input));
#ifdef JIM_DEBUG
    fprintf(syslogOutput, "\tdata: %02x%02x%02x%02x%02x%02x%02x%02x%02x%02x\n",
      savebyte, scratch[0], scratch[1], scratch[2], scratch[3], scratch[4],
	  scratch[5], scratch[6], scratch[7], scratch[8]);
#endif

    if (opts&SYSLAYER_LOG) {
      if((scratch[0] >> 4) == 0x3) {
        ReadTimeStamp(&scratch[5], &dts_high, &dts);
        fprintf(syslogOutput, "\tdts: %01x, %u (0x%08x)\n", dts_high, dts, dts);
      }
      ReadTimeStamp(scratch, &pts_high, &pts);
      fprintf(syslogOutput, "\tpts: %01x, %u (0x%08x)\n", pts_high, pts, pts);
    }
  }
#endif
  /* Read all the headers, now make room for packet */
  if (*bs_ptr+*max_length<*buf_ptr+packetLength/4+*length_ptr) {
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, "\tProblems with buffer length! %d %d\n",
	      *max_length-*length_ptr,packetLength/4);
    }
    if (*max_length-*length_ptr<packetLength/4) {
      /* Buffer too small for a packet (plus whats there),
	 time to enlarge it! */
      unsigned int *old=*bs_ptr;
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput,"\tMega Resize! old=%d, new=%d\n",
		*max_length,*length_ptr + packetLength/2);
      }
      *max_length = *length_ptr + packetLength/2;
      *bs_ptr=(unsigned int *)malloc(*max_length*4);
      if (*bs_ptr == NULL) {
	if (opts&SYSLAYER_LOG) {
	  fprintf(syslogOutput, "\tReadPacket: Error in allocating %d bytes\n",
		  *max_length);
	}
	return 1;
      }
      memcpy((unsigned char *)*bs_ptr,*buf_ptr,*length_ptr*4);
      free(old);
      *buf_ptr=*bs_ptr;
    } else {
      memcpy((unsigned char *)*bs_ptr,*buf_ptr,*length_ptr*4);
      *buf_ptr = *bs_ptr;
    }}
  byte_length=*length_ptr*4;
  if (num_left!=0) {
    /* Sigh, deal with previous leftovers */
    byte_length += num_left;
    *(*buf_ptr+*length_ptr)=leftover_bytes;
  }
  packetBuffer=((unsigned char *)*buf_ptr)+byte_length;
  packetDataLength = packetLength - pos;
  *packetBuffer++=nextByte;
  if (packetID == gVideoStreamID) {
    ioBytes=fread(packetBuffer, 1, packetDataLength-1, input);
    if (ioBytes!=packetDataLength-1) {
      EOF_flag=1;
      if (opts&SYSLAYER_LOG) {
	fprintf(syslogOutput,"\tEOF in middle of packet!\n");
      }
      return 1;
    }
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, "\tKeeping Video packet of length %d (%%=%d)\n",
              packetDataLength,packetDataLength%4);
    }
    if (1!=ntohl(1)) {
      unsigned int *mark=*buf_ptr+*length_ptr;
      int i;
      
      for (i=0; i < ((packetDataLength+num_left)&0xfffffffc); i+=4) {
	*mark=ntohl(*mark);
	mark++;
      }}
    byte_length = byte_length+packetDataLength;
    num_left=byte_length%4;
    *length_ptr = byte_length/4;
    leftover_bytes = *(*buf_ptr+*length_ptr);
    sysBytes+=packetLength-packetDataLength;
    vidBytes+=packetDataLength;
    return 2;
  }
  else if (packetID == gAudioStreamID) { 
    sysBytes+=packetLength-packetDataLength;
    audBytes+=packetDataLength;
    packetBuffer=(unsigned char *)(*buf_ptr+*length_ptr+1);
    fread(packetBuffer, 1, packetDataLength-1, input);
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, "\tReceived Audio packet of length %d\n",
	      packetDataLength);
    }
  }
  else /* Donno what it is, just nuke it */ {
    /* This code should be unreachable */
    sysBytes+=packetLength;
    packetBuffer=(unsigned char *)(*buf_ptr+*length_ptr+1);
    fread(packetBuffer, 1, packetDataLength-1, input);
    if (opts&SYSLAYER_LOG) {
      fprintf(syslogOutput, "\tReceived Unknown packet of length %d\n",
	      packetDataLength);
    }
  }
  return 0; 
}


/*
 * The remaining procedures are formatting utility procedures.
 */
void ReadTimeStamp(inputBuffer,hiBit,low4Bytes)
     unsigned char *inputBuffer, *hiBit;
     unsigned long *low4Bytes;
{
  *hiBit = ((unsigned long)inputBuffer[0] >> 3) & 0x01;
  *low4Bytes = (((unsigned long)inputBuffer[0] >> 1) & 0x03) << 30; 
  *low4Bytes |= (unsigned long)inputBuffer[1] << 22; 
  *low4Bytes |= ((unsigned long)inputBuffer[2] >> 1) << 15; 
  *low4Bytes |= (unsigned long)inputBuffer[3] << 7; 
  *low4Bytes |= ((unsigned long)inputBuffer[4]) >> 1; 
}

void ReadSTD(
	     unsigned char *inputBuffer,
	     unsigned char *stdBufferScale,
	     unsigned long *stdBufferSize) 
{
  *stdBufferScale = ((inputBuffer[0] & 0x20) >> 5); 
  *stdBufferSize = ((unsigned long)inputBuffer[0] & 0x1f) << 8;
  *stdBufferSize |= (unsigned long)inputBuffer[1];
}


void ReadRate(inputBuffer,rate)
     unsigned char *inputBuffer;
     unsigned long *rate;
{
  *rate = (inputBuffer[0] & 0x7f) << 15;
  *rate |= inputBuffer[1] << 7;
  *rate |= (inputBuffer[2] & 0xfe) >> 1;
}

#define FLOAT_0x10000 (double)((unsigned long)1 << 16)

int MakeFloatClockTime(hiBit,low4Bytes,floatClockTime)
     unsigned char hiBit;
     unsigned long low4Bytes;
     double *floatClockTime;
{
  if (hiBit != 0 && hiBit != 1) {
    *floatClockTime = 0.0;
    return 1;
  }
  *floatClockTime 
    = (double)hiBit*FLOAT_0x10000*FLOAT_0x10000 + (double)low4Bytes;
  *floatClockTime /= (double)STD_SYSTEM_CLOCK_FREQ;
  return 0;
}
