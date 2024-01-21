 /*===========================================================================*
 * readframe.c                                                               *
 *                                                                           *
 *      procedures to read in frames                                         *
 *                                                                           *
 * EXPORTED PROCEDURES:                                                      *
 *      ReadFrame                                                            *
 *      SetFileType                                                          *
 *      SetFileFormat                                                        *
 *      JMovie2JPEG                                                          *
 *                                                                           *
 *===========================================================================*/

/*
 * Copyright (c) 1993 The Regents of the University of California.
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
 *  $Header: /n/picasso/users/keving/encode/src/RCS/readframe.c,v 1.1 1993/07/22 22:23:43 keving Exp keving $
 *  $Log: readframe.c,v $
 * Revision 1.1  1993/07/22  22:23:43  keving
 * nothing
 *
 */


/*==============*
 * HEADER FILES *
 *==============*/

#include "all.h"
#include <time.h>
#include <errno.h>
#include "mtypes.h"
#include "frames.h"
#include "prototypes.h"
#include "parallel.h"
#include "param.h"
#include "readframe.h"
#include "fsize.h"
#include "rgbtoycc.h"
#include "jpeglib.h"

#define HEADER_SIZE 607   /*JFIF header size used on output images*/

#define PPM_READ_STATE_MAGIC    0
#define PPM_READ_STATE_WIDTH    1
#define PPM_READ_STATE_HEIGHT   2
#define PPM_READ_STATE_MAXVAL   3
#define PPM_READ_STATE_DONE     4


/*==================*
 * STATIC VARIABLES *
 *==================*/

static int  fileType = BASE_FILE_TYPE;



/*===============================*
 * INTERNAL PROCEDURE prototypes *
 *===============================*/

static char *ScanNextString _ANSI_ARGS_((char *inputLine, char *string));
static void ReadPNM _ANSI_ARGS_((FILE * fp, MpegFrame * mf));
static boolean  ReadPPM _ANSI_ARGS_((MpegFrame *mf, FILE *fpointer));
static void ReadYUV _ANSI_ARGS_((MpegFrame * mf, FILE *fpointer,
				 int width, int height));
static void ReadSub4 _ANSI_ARGS_((MpegFrame * mf, FILE *fpointer,
				  int width, int height));
static void ReadJPEG _ANSI_ARGS_((MpegFrame * mf, File *fp));


/*=====================*
 * EXPORTED PROCEDURES *
 *=====================*/

/*===========================================================================*
 *
 * ReadFrame
 *
 *      reads the given frame, performing conversion as necessary
 *      if addPath = TRUE, then must add the current path before the
 *      file name
 *
 * RETURNS:     frame modified
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
void
ReadFrame(frame, fileName, conversion, addPath)
    MpegFrame *frame;
    char *fileName;
    char *conversion;
    boolean addPath;
{
    FILE        *ifp;
    char        command[1024];
    char        fullFileName[1024];
#ifdef BLEAH
    static int32        readDiskTime = 0;
    int32       diskStartTime, diskEndTime;

time(&diskStartTime);
#endif

    if ( addPath ) {
	sprintf(fullFileName, "%s/%s", currentPath, fileName);
    } else {
	sprintf(fullFileName, "%s", fileName);
    }

#ifdef BLEAH
    if ( ! childProcess ) {
	fprintf(stdout, "+++++READING Frame %d  (type %d):  %s\n", frame->id,
		frame->type, fullFileName);
    }
#endif

    if ( fileType == ANY_FILE_TYPE ) {
	char *convertPtr, *commandPtr, *charPtr;

	/* replace every occurrence of '*' with fullFileName */
	convertPtr = conversion;
	commandPtr = command;
	while ( *convertPtr != '\0' ) {
	    while ( (*convertPtr != '\0') && (*convertPtr != '*') ) {
		*commandPtr = *convertPtr;
		commandPtr++;
		convertPtr++;
	    }

	    if ( *convertPtr == '*' ) {
		/* copy fullFileName */
		charPtr = fullFileName;
		while ( *charPtr != '\0' ) {
		    *commandPtr = *charPtr;
		    commandPtr++;
		    charPtr++;
		}

		convertPtr++;   /* go past '*' */
	    }
	}
	*commandPtr = '\0';

	if ( (ifp = popen(command, "r")) == NULL ) {
	    fprintf(stderr, "ERROR:  Couldn't execute input conversion command:\n");
	    fprintf(stderr, "\t%s\n", command);
	    fprintf(stderr, "errno = %d\n", errno);
	    if ( ioServer ) {
		fprintf(stderr, "IO SERVER:  EXITING!!!\n");
	    } else {
		fprintf(stderr, "SLAVE EXITING!!!\n");
	    }
	    exit(1);
	}
    } else {

	if ( (ifp = fopen(fullFileName, "r")) == NULL ) {
	    fprintf(stderr, "ERROR:  Couldn't open input file %s\n",
		    fullFileName);
	    exit(1);
	}
    }

    switch(baseFormat) {
	case YUV_FILE_TYPE:
	    ReadYUV(frame, ifp, realWidth, realHeight);
	    break;
	case PPM_FILE_TYPE:
	    if ( ! ReadPPM(frame, ifp) ) {
		fprintf(stderr, "Error reading PPM input file!!!\n");
		exit(1);
	    }
	    PPMtoYUV(frame);
	    break;
	case PNM_FILE_TYPE:
	    ReadPNM(ifp, frame);
	    PNMtoYUV(frame);
	    break;
	case SUB4_FILE_TYPE:
	    ReadSub4(frame, ifp, yuvWidth, yuvHeight);
	    break;
	  case JPEG_FILE_TYPE:

	    ReadJPEG(frame,ifp);
	    break;
	default:
	    break;
    }

    if ( fileType == ANY_FILE_TYPE ) {
	pclose(ifp);
    } else {
	fclose(ifp);
    }

#ifdef BLEAH
time(&diskEndTime);

    readDiskTime += (diskEndTime-diskStartTime);

fprintf(stdout, "cumulative disk read time:  %d seconds\n", readDiskTime);
#endif

    MotionSearchPreComputation(frame);
}


/*===========================================================================*
 *
 * SetFileType
 *
 *      set the file type to be either a base type (no conversion), or
 *      any type (conversion required)
 *
 * RETURNS:     nothing
 *
 * SIDE EFFECTS:    fileType
 *
 *===========================================================================*/
void
SetFileType(conversion)
    char *conversion;
{

    if ( strcmp(conversion, "*") == 0 ) {

	fileType = BASE_FILE_TYPE;
    } else {
	fileType = ANY_FILE_TYPE;
    }
}


/*===========================================================================*
 *
 * SetFileFormat
 *
 *      set the file format (PPM, PNM, YUV, JPEG, JMOVIE)
 *
 * RETURNS:     nothing
 *
 * SIDE EFFECTS:    baseFormat
 *
 *===========================================================================*/
void
SetFileFormat(format)
    char *format;
{

    if ( strcmp(format, "PPM") == 0 ) {
	baseFormat = PPM_FILE_TYPE;
    } else if ( strcmp(format, "YUV") == 0 ) {
	baseFormat = YUV_FILE_TYPE;
    } else if ( strcmp(format, "PNM") == 0 ) {
	baseFormat = PNM_FILE_TYPE;
    } else if ( strcmp(format, "JPEG") == 0 ) {
	baseFormat = JPEG_FILE_TYPE;
    } else if ( strcmp(format,"JMOVIE") == 0){
	baseFormat = JMOVIE_FILE_TYPE;
    } else if ( strcmp(format, "SUB4") == 0 ) {
	baseFormat = SUB4_FILE_TYPE;
    } else {
	fprintf(stderr, "ERROR:  Invalid file format:  %s\n", format);
	exit(1);
    }
}


/*************************JPEG LIBRARY INTERFACE*********************/
/*
 * THE BIG PICTURE:
 *
 * The rough outline this JPEG decompression operation is:
 *
 *      allocate and initialize JPEG decompression object
 *      specify data source (eg, a file)
 *      jpeg_read_header();     // obtain image dimensions and other parameters
 *      set parameters for decompression
 *      jpeg_start_decompress();
 *      while (scan lines remain to be read)
 *              jpeg_read_scanlines(...);
 *      jpeg_finish_decompress();
 *      release JPEG decompression object
 *
 */
/*===========================================================================*
 *
 * ReadJPEG  contributed by James Arthur Boucher of Boston University's
 *                                Multimedia Communications Lab
 *
 *      read a JPEG file and copy data into frame original data arrays
 *
 * RETURNS:     mf modified
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
static void
ReadJPEG(mf, fp)
    MpegFrame *mf;
    FILE *fp;
{

  /* This struct contains the JPEG decompression parameters and pointers to
   * working data (which is allocated as needed by the JPEG library).
   */
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr jerr;
  /* More stuff */
  JSAMPARRAY scanarray[3];
  JSAMPLE *in_data_ptr;
  uint8 *out_data_ptr_y;
  uint8 *out_data_ptr_cb;
  uint8 *out_data_ptr_cr;
  int ci,cb,cd,cp;
  JDIMENSION ncols[3];
  JDIMENSION nrows[3];
  jpeg_component_info *compptr;
  int buffer_height;
  int current_row[3];
  uint8 **orig[3];
  int h_samp[3],v_samp[3];
  int max_h_samp,max_v_samp;
  int temp_h, temp_v;
  int temp;

  /* Allocate and initialize JPEG decompression object */
   cinfo.err = jpeg_std_error(&jerr);

  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);

  /* specify data source (eg, a file) */

  jpeg_stdio_src(&cinfo, fp);

  /* read file parameters with jpeg_read_header() */

   
  (void) jpeg_read_header(&cinfo, TRUE);
  /* We can ignore the return value from jpeg_read_header since
   *   (a) suspension is not possible with the stdio data source, and
   *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
   */

  /* set parameters for decompression */
    cinfo.want_raw_output = TRUE;
    cinfo.out_color_space =JCS_YCbCr;

  /* calculate image output dimensions */
    jpeg_calc_output_dimensions(&cinfo);
  /* the above calculation will set these soon */
  /* for now we'll set them ourselves */


   /* tell mpeg_encode the size of the JPEG Image*/
    Fsize_Note(mf->id,(int)(cinfo.image_width),(int)(cinfo.image_height));

   /* Allocate memory for the raw YCbCr data to occupy*/
    Frame_AllocYCC(mf);      /*allocate space for mpeg frame*/

   /* copy pointers to array structure- this make the following
      code more compact  */
    orig[0] = mf->orig_y;
    orig[1] = mf->orig_cb;
    orig[2] = mf->orig_cr;

  /* Note that we can use the info obtained from jpeg_read_header.
   */

  /* Start decompressor */

  jpeg_start_decompress(&cinfo);


  /* JSAMPLEs per row in output buffer  */
  /* collect component subsample values*/
 for(cp=0,compptr = cinfo.comp_info;cp<cinfo.num_components;
		cp++,compptr++) {
     h_samp[cp] = compptr->h_samp_factor;
     v_samp[cp] = compptr->v_samp_factor;
   }
 /* calculate max subsample values*/
  temp_h = (h_samp[0]<h_samp[1]) ? h_samp[1] : h_samp[0];
  max_h_samp = (temp_h<h_samp[2]) ? h_samp[2]:temp_h;
  temp_v = (v_samp[0]<v_samp[1]) ? v_samp[1] : v_samp[0];
  max_v_samp = (temp_v<v_samp[2]) ? v_samp[2]:temp_v;

  /* Make an 8-row-high sample array that will go away when done with image */
  buffer_height = 8;  /* could be 2, 4,8 rows high */

  for(cp=0,compptr = cinfo.comp_info;cp<cinfo.num_components;
		cp++,compptr++) {
    ncols[cp] = (JDIMENSION)((cinfo.image_width*compptr->h_samp_factor)/
	     max_h_samp);
  
    nrows[cp] = (JDIMENSION)((buffer_height*compptr->v_samp_factor)/
	    max_v_samp);

  scanarray[cp] = (*cinfo.mem->alloc_sarray)
		((j_common_ptr) &cinfo, JPOOL_IMAGE, ncols[cp], nrows[cp]);

  }


  /*  while (scan lines remain to be read) */
  /*           jpeg_read_scanlines(...); */

  /* Here we use the library's state variable cinfo.output_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   */

  while (cinfo.output_scanline < cinfo.output_height) {

    (void) jpeg_read_raw_scanlines(&cinfo, scanarray, buffer_height);

/* alter subsample ratio's if neccessary */
if((h_samp[0]==2)&&(h_samp[1]==1)&&(h_samp[2]==1)&&
   (v_samp[0]==2)&&(v_samp[1]==1)&&(v_samp[2]==1)){
   /* we are 4:1:1 as expected by the encoder*/
}else if((h_samp[0]==2)&&(h_samp[1]==1)&&(h_samp[2]==1)&&
	 (v_samp[0]==1)&&(v_samp[1]==1)&&(v_samp[2]==1)){
   /* must subsample 2:1 vertically and adjust params*/
 for(ci=1; ci<3; ci++){
   for(cp=0; cp<(buffer_height/2);cp=cp+1){
     for(cd=0;cd<ncols[ci];cd++){
	temp =((scanarray[ci][cp*2][cd]+scanarray[ci][(cp*2)+1][cd])/2);
	scanarray[ci][cp][cd] = (JSAMPLE)(temp);
     }
   }
 }
  /* only reset values the first time through*/
  if(cinfo.output_scanline==buffer_height){
    nrows[1] = nrows[1]/2;
    nrows[2] = nrows[2]/2;
    max_v_samp = 2;
    v_samp[0] = 2;
  }
 }else{
  fprintf(stderr, "Not a supported subsampling ratio\n");
  exit(1);
}

/* transfer data from jpeg buffer to MPEG frame */
/* calculate the row we wish to output into */
  for(ci=0,compptr=cinfo.comp_info;ci<cinfo.num_components;
       ci++,compptr++){
  current_row[ci] =((cinfo.output_scanline - buffer_height)*
	       (v_samp[ci])/max_v_samp);  

  jcopy_sample_rows(scanarray[ci],0,(JSAMPARRAY)(orig[ci]),
		    current_row[ci],nrows[ci],ncols[ci]);
    }

}  

  /* Step 7: Finish decompression */

  (void) jpeg_finish_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* Step 8: Release JPEG decompression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_decompress(&cinfo);

  /* After finish_decompress, we can close the input file.
   * Here we postpone it until after no more JPEG errors are possible,
   * so as to simplify the setjmp error logic above.  (Actually, I don't
   * think that jpeg_destroy can do an error exit, but why assume anything...)
   */


  /* At this point you may want to check to see whether any corrupt-data
   * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
   * If you prefer to treat corrupt data as a fatal error, override the
   * error handler's emit_message method to call error_exit on a warning.
   */

  /* And we're done! */

}


/*
 * SOME FINE POINTS:
 *
 * In the above loop, we ignored the return value of jpeg_read_scanlines,
 * which is the number of scanlines actually read.  We could get away with
 * this for the same reasons discussed in the compression example.  Actually
 * there is one perfectly normal situation in which jpeg_read_scanlines may
 * return fewer lines than you asked for: at the bottom of the image.  But the
 * loop above can't ask for more lines than there are in the image since it
 * reads only one line at a time.
 *
 * In some high-speed operating modes, some data copying can be saved by
 * making the buffer passed to jpeg_read_scanlines be cinfo.rec_outbuf_height
 * lines high (or a multiple thereof).  This will usually be 1, 2, or 4 lines.
 *
 * To decompress multiple images, you can repeat the whole sequence, or you
 * can keep the JPEG object around and just repeat steps 2-7.  This will
 * save a little bit of startup/shutdown time.
 *
 * As with compression, some operating modes may require temporary files.
 * On some systems you may need to set up a signal handler to ensure that
 * temporary files are deleted if the program is interrupted.
 *
 * Scanlines are returned in the same order as they appear in the JPEG file,
 * which is standardly top-to-bottom.  If you must have data supplied
 * bottom-to-top, you can use one of the virtual arrays provided by the
 * JPEG memory manager to invert the data.  See wrrle.c for an example.
 */

/*===========================================================================*
 *
 * ReadPNM
 *
 *      read a PNM file
 *
 * RETURNS:     mf modified
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
static void
ReadPNM(fp, mf)
    FILE *fp;
    MpegFrame *mf;
{
    int x, y;
    xelval maxval;
    int format;

    mf->rgb_data = pnm_readpnm(fp, &x, &y, &maxval, &format);
    ERRCHK(mf, "pnm_readpnm");

    if (format != PPM_FORMAT) {
	if (maxval < 255) {
	    pnm_promoteformat(mf->rgb_data, x, y, maxval, format, 255, PPM_FORMAT);
	    maxval = 255;
	} else {
	    pnm_promoteformat(mf->rgb_data, x, y, maxval, format, maxval, PPM_FORMAT);
	}
    }
    if (maxval < 255) {
	pnm_promoteformat(mf->rgb_data, x, y, maxval, format, 255, format);
	maxval = 255;
    }
    /*
     * if this is the first frame read, set the global frame size
     */
    Fsize_Note(mf->id, x, y);

    mf->rgb_maxval = maxval;
    mf->rgb_format = PPM_FORMAT;
}



/*===========================================================================*
 *
 * ReadIOConvert
 *
 *      do conversion; return a pointer to the appropriate file
 *
 * RETURNS:     pointer to the appropriate file
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
FILE *
ReadIOConvert(fileName)
    char *fileName;
{
    FILE        *ifp;
    char        command[1024];
    char        fullFileName[1024];
    char *convertPtr, *commandPtr, *charPtr;

    sprintf(fullFileName, "%s/%s", currentPath, fileName);

#ifdef BLEAH
    if ( ! childProcess ) {
	fprintf(stdout, "+++++READING (IO CONVERT) Frame %d  (type %d):  %s\n", frame->id,
		frame->type, fullFileName); }
#endif

    if ( strcmp(ioConversion, "*") == 0 ) {
	ifp = fopen(fullFileName, "r");

	ERRCHK(ifp, "fopen");

	return ifp;
    }

    /* replace every occurrence of '*' with fullFileName */
    convertPtr = ioConversion;
    commandPtr = command;
    while ( *convertPtr != '\0' ) {
	while ( (*convertPtr != '\0') && (*convertPtr != '*') ) {
	    *commandPtr = *convertPtr;
	    commandPtr++;
	    convertPtr++;
	}

	if ( *convertPtr == '*' ) {
	    /* copy fullFileName */
	    charPtr = fullFileName;
	    while ( *charPtr != '\0' ) {
		*commandPtr = *charPtr;
		commandPtr++;
		charPtr++;
	    }

	    convertPtr++;   /* go past '*' */
	}
    }
    *commandPtr = '\0';

    if ( (ifp = popen(command, "r")) == NULL ) {
	fprintf(stderr, "ERROR:  Couldn't execute input conversion command:\n");
	fprintf(stderr, "\t%s\n", command);
	fprintf(stderr, "errno = %d\n", errno);
	if ( ioServer ) {
	    fprintf(stderr, "IO SERVER:  EXITING!!!\n");
	} else {
	    fprintf(stderr, "SLAVE EXITING!!!\n");
	}
	exit(1);
    }

    return ifp;
}



/*===========================================================================*
 *
 * ReadPPM
 *
 *      read a PPM file
 *
 * RETURNS:     TRUE if successful; FALSE otherwise; mf modified
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
static boolean
ReadPPM(mf, fpointer)
    MpegFrame *mf;
    FILE *fpointer;
{
    char    inputBuffer[71];
    char    string[71];
    char    *inputLine;
    int     height = 0, width = 0, maxVal;
    uint8   junk[4096];
    register int y;
    int     state;

    state = PPM_READ_STATE_MAGIC;

    while ( state != PPM_READ_STATE_DONE ) {
	if ( fgets(inputBuffer, 71, fpointer) == NULL ) {
	    return FALSE;
	}
	
	inputLine = inputBuffer;
 
	if ( inputLine[0] == '#' ) {
	    continue;
	}

	if ( inputLine[strlen(inputLine)-1] != '\n' ) {
	    return FALSE;
	}

	switch(state) {
	    case PPM_READ_STATE_MAGIC:
		if ( (inputLine = ScanNextString(inputLine, string)) == NULL ) {
		    return FALSE;
		}

		if ( strcmp(string, "P6") != 0 ) {
		    return FALSE;
		}
		state = PPM_READ_STATE_WIDTH;
		/* no break */
	    case PPM_READ_STATE_WIDTH:
		if ( (inputLine = ScanNextString(inputLine, string)) == NULL ) {
		    if ( inputLine == inputBuffer ) {
			return FALSE;
		    } else {
			break;
		    }
		}

		width = atoi(string);

		state = PPM_READ_STATE_HEIGHT;

		/* no break */
	    case PPM_READ_STATE_HEIGHT:
		if ( (inputLine = ScanNextString(inputLine, string)) == NULL ) {
		    if ( inputLine == inputBuffer ) {
			return FALSE;
		    } else {
			break;
		    }
		}

		height = atoi(string);

		state = PPM_READ_STATE_MAXVAL;

		/* no break */
	    case PPM_READ_STATE_MAXVAL:
		if ( (inputLine = ScanNextString(inputLine, string)) == NULL ) {
		    if ( inputLine == inputBuffer ) {
			return FALSE;
		    } else {
			break;
		    }
		}

		maxVal = atoi(string);

		state = PPM_READ_STATE_DONE;
		break;
	} /* end of switch */
    }

    Fsize_Note(mf->id, width, height);

    mf->rgb_maxval = maxVal;

    Frame_AllocPPM(mf);

    for ( y = 0; y < Fsize_y; y++ ) {
	fread(mf->ppm_data[y], sizeof(char), 3*Fsize_x, fpointer);

	/* read the leftover stuff on the right side */
	fread(junk, sizeof(char), 3*(width-Fsize_x), fpointer);
    }

    return TRUE;
}


/*===========================================================================*
 *
 * ReadYUV
 *
 *      read a YUV file
 *
 * RETURNS:     mf modified
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
static void
ReadYUV(mf, fpointer, width, height)
    MpegFrame *mf;
    FILE *fpointer;
    int width;
    int height;
{
    register int y;
    uint8   junk[4096];

    Fsize_Note(mf->id, width, height);

    Frame_AllocYCC(mf);

    for (y = 0; y < Fsize_y; y++) {                     /* Y */
	fread(mf->orig_y[y], 1, Fsize_x, fpointer);

	/* read the leftover stuff on the right side */
	if ( width != Fsize_x ) {
	    fread(junk, 1, width-Fsize_x, fpointer);
	}
    }

    /* read the leftover stuff on the bottom */
    for (y = Fsize_y; y < height; y++) {
	fread(junk, 1, width, fpointer);
    }

    for (y = 0; y < Fsize_y / 2; y++) {                 /* U */
	fread(mf->orig_cb[y], 1, Fsize_x / 2, fpointer);

	/* read the leftover stuff on the right side */
	if ( width != Fsize_x ) {
	    fread(junk, 1, (width-Fsize_x)/2, fpointer);
	}
    }

    /* read the leftover stuff on the bottom */
    for (y = Fsize_y / 2; y < height / 2; y++) {
	fread(junk, 1, width/2, fpointer);
    }

    for (y = 0; y < Fsize_y / 2; y++) {                 /* V */
	fread(mf->orig_cr[y], 1, Fsize_x / 2, fpointer);

	/* read the leftover stuff on the right side */
	if ( width != Fsize_x ) {
	    fread(junk, 1, (width-Fsize_x)/2, fpointer);
	}
    }

    /* ignore leftover stuff on the bottom */
}


/*===========================================================================*
 *
 * ReadSub4
 *
 *      read a YUV file (subsampled even further by 4:1 ratio)
 *
 * RETURNS:     mf modified
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
static void
ReadSub4(mf, fpointer, width, height)
    MpegFrame *mf;
    FILE *fpointer;
    int width;
    int height;
{
    register int y;
    register int x;
    uint8   buffer[1024];

    Fsize_Note(mf->id, width, height);

    Frame_AllocYCC(mf);

    for (y = 0; y < height/2; y++) {                    /* Y */
	fread(buffer, 1, width/2, fpointer);
	for ( x = 0; x < width/2; x++ ) {
	    mf->orig_y[2*y][2*x] = buffer[x];
	    mf->orig_y[2*y][2*x+1] = buffer[x];
	    mf->orig_y[2*y+1][2*x] = buffer[x];
	    mf->orig_y[2*y+1][2*x+1] = buffer[x];
	}
    }

    for (y = 0; y < height / 4; y++) {                  /* U */
	fread(buffer, 1, width/4, fpointer);
	for ( x = 0; x < width/4; x++ ) {
	    mf->orig_cb[2*y][2*x] = buffer[x];
	    mf->orig_cb[2*y][2*x+1] = buffer[x];
	    mf->orig_cb[2*y+1][2*x] = buffer[x];
	    mf->orig_cb[2*y+1][2*x+1] = buffer[x];
	}
    }

    for (y = 0; y < height / 4; y++) {                  /* V */
	fread(buffer, 1, width/4, fpointer);
	for ( x = 0; x < width/4; x++ ) {
	    mf->orig_cr[2*y][2*x] = buffer[x];
	    mf->orig_cr[2*y][2*x+1] = buffer[x];
	    mf->orig_cr[2*y+1][2*x] = buffer[x];
	    mf->orig_cr[2*y+1][2*x+1] = buffer[x];
	}
    }
}


/*=====================*
 * INTERNAL PROCEDURES *
 *=====================*/

/*===========================================================================*
 *
 * ScanNextString
 *
 *      read a string from a input line, ignoring whitespace
 *
 * RETURNS:     pointer to position in input line after string
 *              NULL if all whitespace
 *              puts string in 'string'
 *
 * SIDE EFFECTS:    file stream munched a bit
 *
 *===========================================================================*/
static char *
ScanNextString(inputLine, string)
    char *inputLine;
    char *string;
{
    /* skip whitespace */
    while ( isspace(*inputLine) && (*inputLine != '\n') ) {
	inputLine++;
    }

    if ( *inputLine == '\n' ) {
	return NULL;
    }

    while ( (! isspace(*inputLine)) && (*inputLine != '\n') ) {
	*string = *inputLine;
	string++;
	inputLine++;
    }

    *string = '\0';

    return inputLine;
}
/*=======================================================================*
 *                                                                       *
 * JMovie2JPEG                                                           *
 *                                                                       *
 *      Splits up a Parallax J_Movie into a set of JFIF image files      *
 *                                                                       *
 * RETURNS:     nothing                                                  *
 *                                                                       *
 * SIDE EFFECTS:    none                                                 *
 *                                                                       *
 *   Contributed By James Boucher(jboucher@flash.bu.edu)                 *
 *               Boston University Multimedia Communications Lab         *
 * This code was adapted from the Berkeley Playone.c and Brian Smith's   *
 * JGetFrame code after being modified on 10-7-93 by Dinesh Venkatesh    *
 * of BU.                                                                *
 *       This code converts a version 2 Parallax J_Movie into a          *
 * set of JFIF compatible JPEG images. It works for all image            *
 * sizes and qualities.                                                  *
 ************************************************************************/

void
JMovie2JPEG(infilename,obase,start,end)
    char *infilename;       /* input filename string */
    char *obase;            /* output filename base string=>obase##.jpg */ 
    int start;              /* first frame to be extracted */
    int end;                /* last frame to be extracted */
{
    FILE *inFile;         /* Jmovie file pointer */
    FILE *outFile;        /* JPEG file pointer for output file */
    extern char *malloc();
    int fd, i;            /* input file descriptor and a counting variable*/
    char ofname[256];     /* output filename string */
    int Temp = 0, temp = 0;  /* dummy variables */
    int image_offset = 0;    /* counting variable */
  /* J_Movie header infomation */
    int ver_no;           /* version number - expected to be 2 */
    int fps;              /* frame rate - frames per second */
    int no_frames;        /* total number of frames in jmovie */
    int bandwidth;        /* bandwidth required for normal playback*/
    int qfactor;          /* quality factor used to scale Q matrix */
    int mapsize;          /* number of color map entries - 2^24 */
    int audio_tracks;     /* number of audio tracks ==1    */
    int audiosize;        /*number of bytes in audio tracks */
    int *inoffsets;       /* input frame offsets from start of jmovie*/
    int width;            /* image width */
    int height;           /* image height */
    int size;             /* total image size in bytes */
    char op_code;         /* jmovie op_code */
    char jpeg_size[4];    /* jpeg data size */
static char junk[1000];   /* data sink for audio data */

/* The next array represents the default JFIF header for
quality = 100 and size = 320x240. The values are
adjusted as the J_Movie header is read.  The default
size of this array is set large so as to make room
for the appending of the jpeg bitstream. It can be
made smaller if you have a better idea of its expected size*/
static char inbuffer[300000] = {    
     0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46,  
     0x49, 0x46, 0x00, 0x01, 0x01, 0x00, 0x00, 0x01,
     0x00, 0x01, 0x00, 0x00, 0xFF, 0xC0, 0x00, 0x11,  
     0x08, 0x00, 0xF0, 0x01, 0x40, 0x03, 0x01, 0x21,
     0x00, 0x02, 0x11, 0x01, 0x03, 0x11, 0x01, 0xFF,  
     0xDB, 0x00, 0x84, 0x00, 0x10, 0x0B, 0x0C, 0x0E,
     0x0C, 0x0A, 0x10, 0x0E, 0x0D, 0x0E, 0x12,  
     0x11, 0x10, 0x13, 0x18, 0x28, 0x1A, 0x18, 0x16,
     0x16, 0x18, 0x31, 0x23, 0x25, 0x1D, 0x28, 0x3A,  
     0x33, 0x3D, 0x3C, 0x39, 0x33, 0x38, 0x37, 0x40,
     0x48, 0x5C, 0x4E, 0x40, 0x44, 0x57, 0x45, 0x37,  
     0x38, 0x50, 0x6D, 0x51, 0x57, 0x5F, 0x62, 0x67,
     0x68, 0x67, 0x3E, 0x4D, 0x71, 0x79, 0x70, 0x64,  
     0x78, 0x5C, 0x65, 0x67, 0x63, 0x01, 0x11, 0x12,
     0x12, 0x18, 0x15, 0x18, 0x2F, 0x1A, 0x1A, 0x2F,  
     0x63, 0x42, 0x38, 0x42, 0x63, 0x63, 0x63, 0x63,
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0x63,  
     0x63, 0x63, 0x63, 0x63, 0x63, 0x63, 0xFF, 0xC4,
     0x01, 0xA2, 0x00, 0x00, 0x01, 0x05, 0x01, 0x01,  
     0x01, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00,
     0x00, 0x00, 0x00, 0x00, 0x01, 0x02, 0x03, 0x04,  
     0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B,
     0x10, 0x00, 0x02, 0x01, 0x03, 0x03, 0x02, 0x04,  
     0x03, 0x05, 0x05, 0x04, 0x04, 0x00, 0x00, 0x01,
     0x7D, 0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05,  
     0x12, 0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61,
     0x07, 0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xA1,  
     0x08, 0x23, 0x42, 0xB1, 0xC1, 0x15, 0x52, 0xD1,
     0xF0, 0x24, 0x33, 0x62, 0x72, 0x82, 0x09,   
     0x0A, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x25, 0x26,
     0x27, 0x28, 0x29, 0x2A, 0x34, 0x35, 0x36, 0x37,  
     0x38, 0x39, 0x3A, 0x43, 0x44, 0x45, 0x46, 0x47,
     0x48, 0x49, 0x4A, 0x53, 0x54, 0x55, 0x56, 0x57,  
     0x58, 0x59, 0x5A, 0x63, 0x64, 0x65, 0x66, 0x67,
     0x68, 0x69, 0x6A, 0x73, 0x74, 0x75, 0x76, 0x77,  
     0x78, 0x79, 0x7A, 0x83, 0x84, 0x85, 0x86, 0x87,
     0x88, 0x89, 0x8A, 0x92, 0x93, 0x94, 0x95, 0x96,  
     0x97, 0x98, 0x99, 0x9A, 0xA2, 0xA3, 0xA4, 0xA5,
     0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xB2, 0xB3, 0xB4,  
     0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xC2, 0xC3,
     0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xD2,  
     0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA,
     0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8,  
     0xE9, 0xEA, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6,
     0xF7, 0xF8, 0xF9, 0xFA, 0x01, 0x00, 0x03, 0x01,  
     0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01,
     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x02,  
     0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 
     0x0A, 0x0B, 0x11, 0x00, 0x02, 0x01, 0x02, 0x04,  
     0x04, 0x03, 0x04, 0x07, 0x05, 0x04, 0x04, 0x00,
     0x01, 0x02, 0x77, 0x00, 0x01, 0x02, 0x03, 0x11,  
     0x04, 0x05, 0x21, 0x31, 0x06, 0x12, 0x41, 0x51,
     0x07, 0x61, 0x71, 0x13, 0x22, 0x32, 0x81, 0x08,  
     0x14, 0x42, 0x91, 0xA1, 0xB1, 0xC1, 0x09, 0x23,
     0x33, 0x52, 0xF0, 0x15, 0x62, 0x72, 0xD1,   
     0x0A, 0x16, 0x24, 0x34, 0xE1, 0x25, 0xF1, 0x17,
     0x18, 0x19, 0x1A, 0x26, 0x27, 0x28, 0x29, 0x2A,  
     0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x43, 0x44,
     0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x53, 0x54,  
     0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x63, 0x64,
     0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x73, 0x74,  
     0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x82, 0x83,
     0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x92,  
     0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A,
     0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9,  
     0xAA, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8,
     0xB9, 0xBA, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,  
     0xC8, 0xC9, 0xCA, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6,
     0xD7, 0xD8, 0xD9, 0xDA, 0xE2, 0xE3, 0xE4, 0xE5,  
     0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xF2, 0xF3, 0xF4,
     0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFF, 0xDA,  
     0x00, 0x0C, 0x03, 0x01, 0x00, 0x02, 0x11, 0x03,
     0x11, 0x00, 0x3F, 0x00  

    };
    
    if(start > end)
    {
	fprintf(stderr,"bad frame numbers\n");
	exit();
    }
    
     /* open J_Movie */
    inFile = fopen(infilename, "r");
    if (inFile == NULL) 
    {
	perror (infilename);
	exit (1);
    }
    
   /* get file descriptor */    
    fd = fileno(inFile);
    
/* The following lines parse the jpeg_movie header and recover the */
/* relavant information */

    fseek (inFile, (8*sizeof(char)),0);
    
    if (fread (&ver_no,sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading version");
	exit();
    }  
    if(ver_no != 2){
       perror("Unrecognized version - Quantization tables may be wrong\n");
     }
    if (fread (&(fps),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading fps");
	exit();
    }  
    if (fread (&(no_frames),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading no_frames");
	exit();
    }  

    inoffsets = (int *)malloc(no_frames*sizeof(int));
    
    if (fread (&(width),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading width");
	exit();
    }  
     /* set image width in JFIF header */
    inbuffer[27] = (char)(0xFF & (width >> 8));
    inbuffer[28] = (char)(0xFF & width);
 
    if (fread (&(height),sizeof(int), 1,inFile) != 1)
    {
	perror("Error in reading height");
	exit();
    }  
     /* set image height in JFIF header */
    inbuffer[25] = (char)(0xFF & (height >> 8));
    inbuffer[26] = (char)(0xFF & height);
    
    if (fread (&(bandwidth),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading bandwidth");
	exit();
    }  
    
    if (fread (&(qfactor),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading qfactor");
	exit();
    }  
     /* The default quality factor = 100, therefore, if
	our quality factor does not equal 100 we must
	scale the quantization matrices in the JFIF header*/    
    /* Note values are clipped to a max of 255 */
    if(qfactor != 100){
      for(Temp=44;Temp<108;Temp++){
	temp= (inbuffer[Temp]*qfactor)/100;
      inbuffer[Temp] = (char)((temp<255) ? temp : 255);
      }
      for(Temp=109;Temp<173;Temp++){
	temp = (inbuffer[Temp]*qfactor)/100;
      inbuffer[Temp] = (char)((temp<255) ? temp : 255);
      }    
    }
  
    if (fread (&(mapsize),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading mapsize");
	exit();
    }  
    if (fread (&(image_offset),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading image offset");
	exit();
    }
    if (fread (&(audio_tracks),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading audio tracks");
	exit();
    }
    
    fread(junk,sizeof(int),1,inFile);
    
    if (fread (&(audiosize),sizeof(int),1,inFile) != 1)
    {
	perror("Error in reading audiosize");
	exit();
    }
    
    fseek (inFile,(image_offset),0);
    

    if(no_frames <= end)
    {
	end = no_frames - 1;
    }
    

    for(i=0;i<no_frames;i++) 
    {
	fread(&(inoffsets[i]),sizeof(int),1,inFile);
    }/* Reads in the frame sizes into the array */
    
    rewind(inFile);

    /* Extract JFIF files from J_Movie */    
    for (i=start; i<=end ; i++) 
    {
	size = inoffsets[i]- inoffsets[i-1]- 5;
	lseek(fd, inoffsets[i-1],0); 
	read(fd, &(op_code), 1);
	while( op_code !=  0xffffffec)
	{
	    read(fd,junk,audiosize);
	    read(fd, &(op_code), 1);  
	    size = size - audiosize ;
	}/* To skip the audio bytes in each frame */
	read(fd,jpeg_size,4);
	read(fd,&(inbuffer[607]),(size));
	sprintf(ofname,"%s%d.jpg",obase,i);
	outFile = fopen(ofname, "w");
	fwrite(inbuffer,(size+607),sizeof(char),outFile);
	fclose(outFile);        
    }
    free(inoffsets);
    fclose(inFile);
}



