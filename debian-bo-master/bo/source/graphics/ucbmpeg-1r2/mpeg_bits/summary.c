/* read an MPEG file, decode the first frame, place it on the screen, and
 * parse the file to get:
 *      - number of frames
 *      - frames per second
 *      - other data
 */

#include <stdio.h>

typedef struct MpegDataStruct
{
    int numFrames;
    int fps;
} MpegData;


#ifdef BLEAH
MpegData *ScanMPEG(char *fileName)
{
    FILE *fpointer;

    if ( (fpointer = fopen(fileName, "r")) == NULL )
    {
	fprintf(stderr, "Error:  could not open MPEG file %s\n",
		fileName);
	return NULL;
    }

    /* read header information */

    /* now read first frame */
#ifdef BLEAH
    DecodeFrame(frame);
#endif

    /* now read rest of file for more information */


    fclose(fpointer);
}
#endif
