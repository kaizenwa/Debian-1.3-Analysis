/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char RcsId[] = "1.15";
#endif

/* mgr.c,v 1.15 1996/05/16 18:42:52 koziol Exp */

/***********************************************************
*
* Test program:  mgr
*
* Test the multi-file raster image interface
*
*************************************************************/

#define TESTFILE "tmgr.hdf"
#define DATAFILE "tmgr.dat"

#include "tproto.h"
#define MFGR_TESTER
#include "mfgr.h"

/* Local pre-processor macros */
#define XDIM    0
#define YDIM    1
#define MAX_IMG_NAME    64  /* Maximum length of image names for this test */

/* Local Data to verify image information in datafile */
const struct {
    char *name;
    int32 ncomp;
    int32 nt;
    int32 il;
    int32 dimsizes[2];
    int32 n_attr;
} datafile_info[]=
  { /* This information applies to the "tmgr.dat" file */
    {"Raster Image #0", 3, DFNT_UCHAR8, MFGR_INTERLACE_PIXEL, {13,15}, 2},
    {"Raster Image #1", 3, DFNT_UCHAR8, MFGR_INTERLACE_LINE, {13,15}, 2},
    {"Raster Image #2", 3, DFNT_UCHAR8, MFGR_INTERLACE_COMPONENT, {13,15}, 2},
    {"Test Image #1", 4, DFNT_UINT16, MFGR_INTERLACE_PIXEL, {21,23}, 3},
    {"Test Image #2", 2, DFNT_FLOAT64, MFGR_INTERLACE_PIXEL, {17,19}, 3}
  };

const uint8 image00[15][13][3]={
{{0 ,0 ,0 },{1 ,1 ,1 },{2 ,2 ,2 },{3 ,3 ,3 },{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 }},
{{1 ,1 ,1 },{2 ,2 ,2 },{3 ,3 ,3 },{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 }},
{{2 ,2 ,2 },{3 ,3 ,3 },{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 }},
{{3 ,3 ,3 },{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 }},
{{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 }},
{{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 }},
{{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 }},
{{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 }},
{{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 }},
{{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 }},
{{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 }},
{{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 },{23 ,23 ,23 }},
{{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 },{23 ,23 ,23 },{24 ,24 ,24 }},
{{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 },{23 ,23 ,23 },{24 ,24 ,24 },{25 ,25 ,25 }},
{{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 },{23 ,23 ,23 },{24 ,24 ,24 },{25 ,25 ,25 },{26 ,26 ,26 }}
};
const uint8 image1[15][13][3]={
{{0 ,1 ,2 },{3 ,4 ,5 },{6 ,7 ,8 },{9 ,10 ,11 },{12 ,0 ,1 },{2 ,3 ,4 },{5 ,6 ,7 },{8 ,9 ,10 },{11 ,12 ,0 },{1 ,2 ,3 },{4 ,5 ,6 },{7 ,8 ,9 },{10 ,11 ,12 }},
{{1 ,1 ,3 },{3 ,5 ,5 },{7 ,7 ,9 },{9 ,11 ,11 },{13 ,1 ,1 },{3 ,3 ,5 },{5 ,7 ,7 },{9 ,9 ,11 },{11 ,13 ,1 },{1 ,3 ,3 },{5 ,5 ,7 },{7 ,9 ,9 },{11 ,11 ,13 }},
{{2 ,3 ,2 },{3 ,6 ,7 },{6 ,7 ,10 },{11 ,10 ,11 },{14 ,2 ,3 },{2 ,3 ,6 },{7 ,6 ,7 },{10 ,11 ,10 },{11 ,14 ,2 },{3 ,2 ,3 },{6 ,7 ,6 },{7 ,10 ,11 },{10 ,11 ,14 }},
{{3 ,3 ,3 },{3 ,7 ,7 },{7 ,7 ,11 },{11 ,11 ,11 },{15 ,3 ,3 },{3 ,3 ,7 },{7 ,7 ,7 },{11 ,11 ,11 },{11 ,15 ,3 },{3 ,3 ,3 },{7 ,7 ,7 },{7 ,11 ,11 },{11 ,11 ,15 }},
{{4 ,5 ,6 },{7 ,4 ,5 },{6 ,7 ,12 },{13 ,14 ,15 },{12 ,4 ,5 },{6 ,7 ,4 },{5 ,6 ,7 },{12 ,13 ,14 },{15 ,12 ,4 },{5 ,6 ,7 },{4 ,5 ,6 },{7 ,12 ,13 },{14 ,15 ,12 }},
{{5 ,5 ,7 },{7 ,5 ,5 },{7 ,7 ,13 },{13 ,15 ,15 },{13 ,5 ,5 },{7 ,7 ,5 },{5 ,7 ,7 },{13 ,13 ,15 },{15 ,13 ,5 },{5 ,7 ,7 },{5 ,5 ,7 },{7 ,13 ,13 },{15 ,15 ,13 }},
{{6 ,7 ,6 },{7 ,6 ,7 },{6 ,7 ,14 },{15 ,14 ,15 },{14 ,6 ,7 },{6 ,7 ,6 },{7 ,6 ,7 },{14 ,15 ,14 },{15 ,14 ,6 },{7 ,6 ,7 },{6 ,7 ,6 },{7 ,14 ,15 },{14 ,15 ,14 }},
{{7 ,7 ,7 },{7 ,7 ,7 },{7 ,7 ,15 },{15 ,15 ,15 },{15 ,7 ,7 },{7 ,7 ,7 },{7 ,7 ,7 },{15 ,15 ,15 },{15 ,15 ,7 },{7 ,7 ,7 },{7 ,7 ,7 },{7 ,15 ,15 },{15 ,15 ,15 }},
{{8 ,9 ,10 },{11 ,12 ,13 },{14 ,15 ,8 },{9 ,10 ,11 },{12 ,8 ,9 },{10 ,11 ,12 },{13 ,14 ,15 },{8 ,9 ,10 },{11 ,12 ,8 },{9 ,10 ,11 },{12 ,13 ,14 },{15 ,8 ,9 },{10 ,11 ,12 }},
{{9 ,9 ,11 },{11 ,13 ,13 },{15 ,15 ,9 },{9 ,11 ,11 },{13 ,9 ,9 },{11 ,11 ,13 },{13 ,15 ,15 },{9 ,9 ,11 },{11 ,13 ,9 },{9 ,11 ,11 },{13 ,13 ,15 },{15 ,9 ,9 },{11 ,11 ,13 }},
{{10 ,11 ,10 },{11 ,14 ,15 },{14 ,15 ,10 },{11 ,10 ,11 },{14 ,10 ,11 },{10 ,11 ,14 },{15 ,14 ,15 },{10 ,11 ,10 },{11 ,14 ,10 },{11 ,10 ,11 },{14 ,15 ,14 },{15 ,10 ,11 },{10 ,11 ,14 }},
{{11 ,11 ,11 },{11 ,15 ,15 },{15 ,15 ,11 },{11 ,11 ,11 },{15 ,11 ,11 },{11 ,11 ,15 },{15 ,15 ,15 },{11 ,11 ,11 },{11 ,15 ,11 },{11 ,11 ,11 },{15 ,15 ,15 },{15 ,11 ,11 },{11 ,11 ,15 }},
{{12 ,13 ,14 },{15 ,12 ,13 },{14 ,15 ,12 },{13 ,14 ,15 },{12 ,12 ,13 },{14 ,15 ,12 },{13 ,14 ,15 },{12 ,13 ,14 },{15 ,12 ,12 },{13 ,14 ,15 },{12 ,13 ,14 },{15 ,12 ,13 },{14 ,15 ,12 }},
{{13 ,13 ,15 },{15 ,13 ,13 },{15 ,15 ,13 },{13 ,15 ,15 },{13 ,13 ,13 },{15 ,15 ,13 },{13 ,15 ,15 },{13 ,13 ,15 },{15 ,13 ,13 },{13 ,15 ,15 },{13 ,13 ,15 },{15 ,13 ,13 },{15 ,15 ,13 }},
{{14 ,15 ,14 },{15 ,14 ,15 },{14 ,15 ,14 },{15 ,14 ,15 },{14 ,14 ,15 },{14 ,15 ,14 },{15 ,14 ,15 },{14 ,15 ,14 },{15 ,14 ,14 },{15 ,14 ,15 },{14 ,15 ,14 },{15 ,14 ,15 },{14 ,15 ,14 }}
};
const uint8 image2[15][13][3]={
{{0 ,1 ,2 },{3 ,4 ,5 },{6 ,7 ,8 },{9 ,10 ,11 },{12 ,1 ,0 },{3 ,2 ,5 },{4 ,7 ,6 },{9 ,8 ,11 },{10 ,13 ,2 },{3 ,0 ,1 },{6 ,7 ,4 },{5 ,10 ,11 },{8 ,9 ,14 }},
{{3 ,2 ,1 },{0 ,7 ,6 },{5 ,4 ,11 },{10 ,9 ,8 },{15 ,4 ,5 },{6 ,7 ,0 },{1 ,2 ,3 },{12 ,13 ,14 },{15 ,8 ,5 },{4 ,7 ,6 },{1 ,0 ,3 },{2 ,13 ,12 },{15 ,14 ,9 }},
{{6 ,7 ,4 },{5 ,2 ,3 },{0 ,1 ,14 },{15 ,12 ,13 },{10 ,7 ,6 },{5 ,4 ,3 },{2 ,1 ,0 },{15 ,14 ,13 },{12 ,11 ,8 },{9 ,10 ,11 },{12 ,13 ,14 },{15 ,0 ,1 },{2 ,3 ,4 }},
{{9 ,8 ,11 },{10 ,13 ,12 },{15 ,14 ,1 },{0 ,3 ,2 },{5 ,10 ,11 },{8 ,9 ,14 },{15 ,12 ,13 },{2 ,3 ,0 },{1 ,6 ,11 },{10 ,9 ,8 },{15 ,14 ,13 },{12 ,3 ,2 },{1 ,0 ,7 }},
{{12 ,13 ,14 },{15 ,8 ,9 },{10 ,11 ,4 },{5 ,6 ,7 },{0 ,13 ,12 },{15 ,14 ,9 },{8 ,11 ,10 },{5 ,4 ,7 },{6 ,1 ,14 },{15 ,12 ,13 },{10 ,11 ,8 },{9 ,6 ,7 },{4 ,5 ,2 }},
{{0 ,1 ,2 },{3 ,4 ,5 },{6 ,7 ,8 },{9 ,10 ,11 },{12 ,1 ,0 },{3 ,2 ,5 },{4 ,7 ,6 },{9 ,8 ,11 },{10 ,13 ,2 },{3 ,0 ,1 },{6 ,7 ,4 },{5 ,10 ,11 },{8 ,9 ,14 }},
{{3 ,2 ,1 },{0 ,7 ,6 },{5 ,4 ,11 },{10 ,9 ,8 },{15 ,4 ,5 },{6 ,7 ,0 },{1 ,2 ,3 },{12 ,13 ,14 },{15 ,8 ,5 },{4 ,7 ,6 },{1 ,0 ,3 },{2 ,13 ,12 },{15 ,14 ,9 }},
{{6 ,7 ,4 },{5 ,2 ,3 },{0 ,1 ,14 },{15 ,12 ,13 },{10 ,7 ,6 },{5 ,4 ,3 },{2 ,1 ,0 },{15 ,14 ,13 },{12 ,11 ,8 },{9 ,10 ,11 },{12 ,13 ,14 },{15 ,0 ,1 },{2 ,3 ,4 }},
{{9 ,8 ,11 },{10 ,13 ,12 },{15 ,14 ,1 },{0 ,3 ,2 },{5 ,10 ,11 },{8 ,9 ,14 },{15 ,12 ,13 },{2 ,3 ,0 },{1 ,6 ,11 },{10 ,9 ,8 },{15 ,14 ,13 },{12 ,3 ,2 },{1 ,0 ,7 }},
{{12 ,13 ,14 },{15 ,8 ,9 },{10 ,11 ,4 },{5 ,6 ,7 },{0 ,13 ,12 },{15 ,14 ,9 },{8 ,11 ,10 },{5 ,4 ,7 },{6 ,1 ,14 },{15 ,12 ,13 },{10 ,11 ,8 },{9 ,6 ,7 },{4 ,5 ,2 }},
{{0 ,1 ,2 },{3 ,4 ,5 },{6 ,7 ,8 },{9 ,10 ,11 },{12 ,1 ,0 },{3 ,2 ,5 },{4 ,7 ,6 },{9 ,8 ,11 },{10 ,13 ,2 },{3 ,0 ,1 },{6 ,7 ,4 },{5 ,10 ,11 },{8 ,9 ,14 }},
{{3 ,2 ,1 },{0 ,7 ,6 },{5 ,4 ,11 },{10 ,9 ,8 },{15 ,4 ,5 },{6 ,7 ,0 },{1 ,2 ,3 },{12 ,13 ,14 },{15 ,8 ,5 },{4 ,7 ,6 },{1 ,0 ,3 },{2 ,13 ,12 },{15 ,14 ,9 }},
{{6 ,7 ,4 },{5 ,2 ,3 },{0 ,1 ,14 },{15 ,12 ,13 },{10 ,7 ,6 },{5 ,4 ,3 },{2 ,1 ,0 },{15 ,14 ,13 },{12 ,11 ,8 },{9 ,10 ,11 },{12 ,13 ,14 },{15 ,0 ,1 },{2 ,3 ,4 }},
{{9 ,8 ,11 },{10 ,13 ,12 },{15 ,14 ,1 },{0 ,3 ,2 },{5 ,10 ,11 },{8 ,9 ,14 },{15 ,12 ,13 },{2 ,3 ,0 },{1 ,6 ,11 },{10 ,9 ,8 },{15 ,14 ,13 },{12 ,3 ,2 },{1 ,0 ,7 }},
{{12 ,13 ,14 },{15 ,8 ,9 },{10 ,11 ,4 },{5 ,6 ,7 },{0 ,13 ,12 },{15 ,14 ,9 },{8 ,11 ,10 },{5 ,4 ,7 },{6 ,1 ,14 },{15 ,12 ,13 },{10 ,11 ,8 },{9 ,6 ,7 },{4 ,5 ,2 }}
};
const uint16 image3[23][21][4]={
{{0 ,1 ,2 ,3 },{1 ,2 ,3 ,4 },{2 ,3 ,4 ,5 },{3 ,4 ,5 ,6 },{4 ,5 ,6 ,7 },{5 ,6 ,7 ,8 },{6 ,7 ,8 ,9 },{7 ,8 ,9 ,10 },{8 ,9 ,10 ,11 },{9 ,10 ,11 ,12 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{10 ,11 ,12 ,13 },{2 ,2 ,2 ,2 },{12 ,13 ,14 ,15 },{2 ,2 ,2 ,2 },{14 ,15 ,16 ,17 },{2 ,2 ,2 ,2 },{16 ,17 ,18 ,19 },{2 ,2 ,2 ,2 },{18 ,19 ,20 ,21 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{20 ,21 ,22 ,23 },{2 ,2 ,2 ,2 },{1 ,2 ,3 ,4 },{2 ,2 ,2 ,2 },{3 ,4 ,5 ,6 },{2 ,2 ,2 ,2 },{5 ,6 ,7 ,8 },{2 ,2 ,2 ,2 },{7 ,8 ,9 ,10 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{9 ,10 ,11 ,12 },{2 ,2 ,2 ,2 },{11 ,12 ,13 ,14 },{2 ,2 ,2 ,2 },{13 ,14 ,15 ,16 },{2 ,2 ,2 ,2 },{15 ,16 ,17 ,18 },{2 ,2 ,2 ,2 },{17 ,18 ,19 ,20 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{19 ,20 ,21 ,22 },{2 ,2 ,2 ,2 },{0 ,1 ,2 ,3 },{2 ,2 ,2 ,2 },{2 ,3 ,4 ,5 },{2 ,2 ,2 ,2 },{4 ,5 ,6 ,7 },{2 ,2 ,2 ,2 },{6 ,7 ,8 ,9 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{8 ,9 ,10 ,11 },{2 ,2 ,2 ,2 },{10 ,11 ,12 ,13 },{2 ,2 ,2 ,2 },{12 ,13 ,14 ,15 },{2 ,2 ,2 ,2 },{14 ,15 ,16 ,17 },{2 ,2 ,2 ,2 },{16 ,17 ,18 ,19 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{18 ,19 ,20 ,21 },{19 ,20 ,21 ,22 },{20 ,21 ,22 ,23 },{0 ,1 ,2 ,3 },{1 ,2 ,3 ,4 },{2 ,3 ,4 ,5 },{3 ,4 ,5 ,6 },{4 ,5 ,6 ,7 },{5 ,6 ,7 ,8 },{6 ,7 ,8 ,9 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{7 ,8 ,9 ,10 },{8 ,9 ,10 ,11 },{9 ,10 ,11 ,12 },{10 ,11 ,12 ,13 },{11 ,12 ,13 ,14 },{12 ,13 ,14 ,15 },{13 ,14 ,15 ,16 },{14 ,15 ,16 ,17 },{15 ,16 ,17 ,18 },{16 ,17 ,18 ,19 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{17 ,18 ,19 ,20 },{18 ,19 ,20 ,21 },{19 ,20 ,21 ,22 },{20 ,21 ,22 ,23 },{0 ,1 ,2 ,3 },{1 ,2 ,3 ,4 },{2 ,3 ,4 ,5 },{3 ,4 ,5 ,6 },{4 ,5 ,6 ,7 },{5 ,6 ,7 ,8 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{6 ,7 ,8 ,9 },{7 ,8 ,9 ,10 },{8 ,9 ,10 ,11 },{9 ,10 ,11 ,12 },{10 ,11 ,12 ,13 },{11 ,12 ,13 ,14 },{12 ,13 ,14 ,15 },{13 ,14 ,15 ,16 },{14 ,15 ,16 ,17 },{15 ,16 ,17 ,18 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{16 ,17 ,18 ,19 },{17 ,18 ,19 ,20 },{18 ,19 ,20 ,21 },{19 ,20 ,21 ,22 },{20 ,21 ,22 ,23 },{0 ,1 ,2 ,3 },{1 ,2 ,3 ,4 },{2 ,3 ,4 ,5 },{3 ,4 ,5 ,6 },{4 ,5 ,6 ,7 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }}
};
const float64 image4[19][17][2]={
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}}
};

static void dump_image(void *data, int32 xdim, int32 ydim, int32 ncomp, int32 nt);
static void test_mgr_init(void);
static void test_mgr_image_b1a(void);
static void test_mgr_image_b1b(void);
static void test_mgr_image_b2a1aa(void);
static void test_mgr_image_b2a1bb1(void);
static void test_mgr_image_b2a1bb2(void);
static void test_mgr_image_b2a1cc1(void);
static void test_mgr_image_b2a1cc2(void);
static void test_mgr_image_b2a2aa(void);
static void test_mgr_image_b2a2bb(void);
static void test_mgr_image_b2a2cc(void);
static void test_mgr_image_b2b1(void);
static void test_mgr_image_b2b2(void);
static void test_mgr_image_b2b3(void);
static void test_mgr_image(void);
static void test_mgr_index(void);
static void test_mgr_interlace(void);
static void test_mgr_lut(void);
static void test_mgr_special(void);
static void test_mgr_attr(void);

#ifdef QAK
static void dump_image(void *data, int32 xdim, int32 ydim, int32 ncomp, int32 nt)
{
    int32 nt_size=DFKNTsize(nt);
    int32 i,j,k;

    for(i=0; i<ydim; i++)
      {
#ifdef QAK
          printf("%ld:",(long)i);
#endif /* QAK */
          for(j=0; j<xdim; j++)
            {
                printf("{");
                for(k=0; k<ncomp; k++)
                  {
                    switch(nt)
                      {
                          case DFNT_CHAR8:
                          case DFNT_UCHAR8:
#ifdef QAK
                            {
                                char *ptr=(char *)data;
                                printf("%c ",*ptr);
                            }
                            break;
#endif /* QAK */

                          case DFNT_UINT8:
                            {
                                unsigned char *ptr=(unsigned char *)data;
                                printf("%u ",(unsigned)*ptr);
                            }
                            break;

                          case DFNT_INT8:
                            {
                                char *ptr=(char *)data;
                                printf("%d ",(int)*ptr);
                            }
                            break;

                          case DFNT_UINT16:
                            {
                                uint16 *ptr=(uint16 *)data;
                                printf("%u ",(unsigned)*ptr);
                            }
                            break;

                          case DFNT_INT16:
                            {
                                int16 *ptr=(int16 *)data;
                                printf("%d ",(int)*ptr);
                            }
                            break;

                          case DFNT_UINT32:
                            {
                                uint32 *ptr=(uint32 *)data;
                                printf("%lu ",(unsigned long)*ptr);
                            }
                            break;

                          case DFNT_INT32:
                            {
                                int32 *ptr=(int32 *)data;
                                printf("%ld ",(long)*ptr);
                            }
                            break;

                          case DFNT_FLOAT32:
                            {
                                float32 *ptr=(float32 *)data;
                                printf("%f ",(double)*ptr);
                            }
                            break;

                          case DFNT_FLOAT64:
                            {
                                float64 *ptr=(float64 *)data;
                                printf("%f ",(double)*ptr);
                            }
                            break;

                          default:
                            printf("unknown NT: %ld\n",(long)nt);
                            break;

                      } /* end switch */
            if(k<(ncomp-1))
                printf(",");
                    data=(void *)((char *)data+nt_size);
                  } /* end for */
                printf("},");

            } /* end for */
          printf("\n");
      } /* end for */
}   /* dump_image() */
#endif /* QAK */

/* Test outline:
    I. Interface Initialization
        A. GRstart
        B. GRend
        C. GRfileinfo
    II. Create Images
        A. GRcreate/GRselect/GRendaccess w/GRgetiminfo
        B. Write/Read images
            1. With no Data
                a. Default fill value
                b. user defined fill value
            2. With real Data
                a. New Image
                    1. With default fill value
                        aa. Whole image
                        bb. Sub-setted image
                        cc. Sub-sampled image
                    2. With user defined vill value
                        aa. Whole image
                        bb. Sub-setted image
                        cc. Sub-sampled image
                b. Existing Image
                    1. Whole image
                    2. Sub-setted image
                    3. Sub-sampled image
    III. ID/Ref/Index Functions
        A. GRidtoref
        B. GRreftoindex
    IV. Interlace Functions [Need to be implemented]
        A. GRreqlutil
        B. GRreqimageil
    V. Palette Functions
        A. GRgetlutid w/GRgetlutinfo
        B. Read/Write Palettes
            1. GRwritelut
            2. GRreadlut
    VI. Special Element Functions [Need to be implemented]
        A. GRsetexternalfile
        B. GRsetaccesstype
        C. GRsetcompress
    VII. Atribute Functions
        A. GRattrinfo
        B. Read/Write Attributes
            1. GRsetattr
            2. GRgetattr
        C. GRfindattr
        
*/

/****************************************************************
**
**  test_mgr_init(): Multi-file Raster Initialization Test Routine
** 
**  I. Interface Initialization
**      A. GRstart
**      B. GRend
**      C. GRfileinfo
** 
****************************************************************/
static void
test_mgr_init(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 n_datasets;       /* number of datasets */
    int32 n_attrs;          /* number of attributes */
    int32 ret;              /* generic return value */

    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Initialization routines\n"););

    MESSAGE(8, printf("Try creating a new file and checking it out\n"););

    /* Ok, now create a new file */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Try initializing the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    ret=(intn)GRfileinfo(grid,&n_datasets,&n_attrs);
    CHECK(ret,FAIL,"GRfileinfo");

    if(n_datasets!=0 || n_attrs!=0)
      {
          MESSAGE(3, printf("Number of datasets/attributes in new file incorrect\n"););
          num_errs++;
      } /* end if */

    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");


    MESSAGE(8, printf("Try checking out an existing file\n"););

    /* Ok, now check an existing file */

    fid=Hopen(DATAFILE,DFACC_READ,0);
    CHECK(fid,FAIL,"Hopen");

    /* Try initializing the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    ret=(intn)GRfileinfo(grid,&n_datasets,&n_attrs);
    CHECK(ret,FAIL,"GRfileinfo");

    if(n_datasets!=5 || n_attrs!=2)
      {
          MESSAGE(3, printf("Number of datasets/attributes in existing file incorrect\n"););
          num_errs++;
      } /* end if */

    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
}   /* end test_mgr_init() */

/* Sub-tests for test_mgr_image() */
static void test_mgr_image_b1a(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B1a - Read/Write images - with no Data - Default Fill Value */
    MESSAGE(8, printf("Check out I/O on image with no data, using the default fill value\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={4,5};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        float32 image[5][4][3]; /* space for the image data */
        float32 image0[5][4][3]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Empty Image",3,DFNT_FLOAT32,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        HDmemset(image,255,dims[0]*dims[1]*3*sizeof(float32));
        /* '0' is the default fill value */
        HDmemset(image0,0,dims[0]*dims[1]*3*sizeof(float32));

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("Error reading data for image with default fill value\n"););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_image_b1a() */

static void test_mgr_image_b1b(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B1b - Read/Write images - with no Data - User-defined Fill Value */
    MESSAGE(8, printf("Check out I/O on image with no data, using User Defined fill-value\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={5,7};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        float64 image[7][5][4]; /* space for the image data */
        float64 fill_pixel[4]={1.3,-2.4,1000.3,.25};   /* pixel with fill values */
        float64 image0[7][5][4]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Empty Image2",4,DFNT_FLOAT64,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Set the fill-value */
        ret=GRsetattr(riid,FILL_ATTR,DFNT_FLOAT64,sizeof(fill_pixel)/sizeof(float64),fill_pixel);
        CHECK(ret,FAIL,"GRsetattr");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        HDmemset(image,0,dims[0]*dims[1]*4*sizeof(float64));
        /* fill the memory-only with the default pixel fill-value */
        HDmemfill(image0,fill_pixel,sizeof(fill_pixel),sizeof(image0)/sizeof(fill_pixel));

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("Error reading data for image with user defined fill-value\n"););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_image_b1b() */

static void test_mgr_image_b2a1aa(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B2a1aa - Read/Write images - with real Data - New Image - with Default Fill Value - Whole Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Whole Image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    3
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    8
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   2
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE int32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_INT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={1,-2};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={-2,1};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */
        intn i,j;       /* local counting variables */

        /* fill the memory-only with the default pixel fill-value */
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                } /* end for */
          } /* end for */
        HDmemcpy(image,image0,sizeof(image0));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1aa",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRwriteimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, whole image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_image_b2a1aa() */

static void test_mgr_image_b2a1bb1(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B2a1bb - Read/Write images - with real Data - New Image - with Default Fill Value - Sub-setted Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Writing Sub-setted Image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={40000,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={1230,1,65000,35000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if(((i>(TEST_YDIM/3)) && (i<(2*TEST_YDIM/3)))
                        && ((j>(TEST_XDIM/4)) && (j<(3*TEST_XDIM/4))))
                      {
                          if((j%2)==0)
                              HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                          else
                              HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1bb",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create sub-setted window with only the filled pixels in it */
        start[XDIM]=(TEST_XDIM/4)+1;
        start[YDIM]=(TEST_YDIM/3)+1;
        count[XDIM]=((3*TEST_XDIM/4)-(TEST_XDIM/4))-1;
        count[YDIM]=((2*TEST_YDIM/3)-(TEST_YDIM/3))-1;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,count,sub_image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the whole image back */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-setted image\n",__LINE__););

              MESSAGE(8, for(i=0; i<TEST_YDIM; i++) \
                      for(j=0; j<TEST_XDIM; j++) \
                          for(k=0; k<TEST_NCOMP; k++) \
                              if(image[i][j][k]!=image0[i][j][k]) \
                                  printf("Location: [%d][%d][%d] image=%u, image0=%u \n",i,j,k,(unsigned)image[i][j][k],(unsigned)image0[i][j][k]); );
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

} /* end test_mgr_image_b2a1bb1() */

static void test_mgr_image_b2a1bb2(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Reading Sub-setted Image\n"););
    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={40000,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={1230,1,65000,35000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j;       /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                    if(((i>(TEST_YDIM/3)) && (i<(2*TEST_YDIM/3)))
                        && ((j>(TEST_XDIM/4)) && (j<(3*TEST_XDIM/4))))
                      {
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1bb2",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create whole image */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the sub-set image back */
        start[XDIM]=(TEST_XDIM/4)+1;
        start[YDIM]=(TEST_YDIM/3)+1;
        count[XDIM]=((3*TEST_XDIM/4)-(TEST_XDIM/4))-1;
        count[YDIM]=((2*TEST_YDIM/3)-(TEST_YDIM/3))-1;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,count,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,sub_image,count[XDIM]*count[YDIM]*sizeof(fill_pixel)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-setted image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

} /* end test_mgr_image_b2a1bb2() */

static void test_mgr_image_b2a1cc1(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B2a1cc - Read/Write images - with real Data - New Image - with Default Fill Value - Sub-sampled Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Writing Sub-sampled Image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   5
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE int16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_INT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={-20000,-1,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={45,1230,1,32000,-32000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(TEST_VARTYPE)*TEST_YDIM*TEST_XDIM*TEST_NCOMP);
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((i%2)!=0 && (j%2)!=0)
                      {
                          if((j%3)==0)
                              HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(TEST_VARTYPE)*TEST_NCOMP);
                          else
                              HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(TEST_VARTYPE)*TEST_NCOMP);
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(TEST_VARTYPE)*TEST_YDIM*TEST_XDIM*TEST_NCOMP);

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1cc",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create sub-sampled window with only the filled pixels in it */
        start[XDIM]=1;
        start[YDIM]=1;
        count[XDIM]=TEST_XDIM/2;
        count[YDIM]=TEST_YDIM/2;
        stride[XDIM]=stride[YDIM]=2;
        ret=GRwriteimage(riid,start,stride,count,sub_image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the whole image back */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(TEST_VARTYPE)*TEST_YDIM*TEST_XDIM*TEST_NCOMP))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-sampled image\n",__LINE__););

              MESSAGE(8, for(i=0; i<TEST_YDIM; i++) \
                      for(j=0; j<TEST_XDIM; j++) \
                          for(k=0; k<TEST_NCOMP; k++) \
                            if(image[i][j][k]!=image0[i][j][k]) \
                                  printf("Location: [%d][%d][%d] image=%d, image0=%d \n",(int)i,(int)j,(int)k,(int)image[i][j][k],(int)image0[i][j][k]); );
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

}

static void test_mgr_image_b2a1cc2(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Reading Sub-sampled Image\n"););
    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={4000000,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={1230,1,65000,350000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j;       /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                    if((i%2) && (j%2))
                      {
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1cc2",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create whole image */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the sub-sample image back */
        start[XDIM]=1;
        start[YDIM]=1;
        count[XDIM]=TEST_XDIM/2;
        count[YDIM]=TEST_YDIM/2;
        stride[XDIM]=stride[YDIM]=2;
        ret=GRreadimage(riid,start,stride,count,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,sub_image,count[XDIM]*count[YDIM]*sizeof(fill_pixel)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-sampled image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

} /* end test_mgr_image_b2a1cc() */

static void test_mgr_image_b2a2aa(void)
{
#ifdef QAK
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B2a2aa - Read/Write images - with real Data - New Image - with User-Defined Fill Value - Whole Image */
/* The following test is unnecessary, fill-values only are important when writing out partial images */
    MESSAGE(8, printf("Check out I/O on new image with real data, with User-Defined fill-value, Whole Image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    3
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    8
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   2
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE int32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_INT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={1,-2};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={-2,1};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-value */
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                } /* end for */
          } /* end for */
        HDmemcpy(image,image0,sizeof(image0));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1aa",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRwriteimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, whole image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
#endif /* QAK */

} /* end test_mgr_image_b2a2aa() */

static void test_mgr_image_b2a2bb(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B2a2bb - Read/Write images - with real Data - New Image - with User-Defined Fill Value - Sub-setted Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with User-Defined fill-value, Writing Sub-setted Image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE float32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_FLOAT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={-3.75,4.5,-0.375,100.125};   /* pixel with fill values */
        TEST_VARTYPE pixel[TEST_NCOMP]={-20.00,4.875,0.125,1.0};   /* pixel with fill values */
        TEST_VARTYPE pixel2[TEST_NCOMP]={1.25,1.0,-6500.0,350.0};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemfill(image0,fill_pixel,sizeof(fill_pixel),sizeof(image0)/sizeof(fill_pixel));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if(((i>(TEST_YDIM/3)) && (i<(2*TEST_YDIM/3)))
                        && ((j>(TEST_XDIM/4)) && (j<(3*TEST_XDIM/4))))
                      {
                          if((j%2)==0)
                              HDmemcpy(&image0[i][j][0],pixel,sizeof(pixel));
                          else
                              HDmemcpy(&image0[i][j][0],pixel2,sizeof(pixel2));
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a2bb",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Set the fill-value */
        ret=GRsetattr(riid,FILL_ATTR,TEST_NT,TEST_NCOMP,fill_pixel);
        CHECK(ret,FAIL,"GRsetattr");

        /* Create sub-setted window with only the filled pixels in it */
        start[XDIM]=(TEST_XDIM/4)+1;
        start[YDIM]=(TEST_YDIM/3)+1;
        count[XDIM]=((3*TEST_XDIM/4)-(TEST_XDIM/4))-1;
        count[YDIM]=((2*TEST_YDIM/3)-(TEST_YDIM/3))-1;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,count,sub_image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the whole image back */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("Error reading data for new image with user-defined fill-value, sub-setted image\n"););

              MESSAGE(8, for(i=0; i<TEST_YDIM; i++) \
                      for(j=0; j<TEST_XDIM; j++) \
                          for(k=0; k<TEST_NCOMP; k++) \
                              if(image[i][j][k]!=image0[i][j][k]) \
                                  printf("Location: [%d][%d][%d] image=%f, image0=%f \n",i,j,k,(double)image[i][j][k],(double)image0[i][j][k]); );
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

/* The following test is unnecessary, fill-values only make a difference when writing out data -QAK */
#ifdef QAK
    MESSAGE(8, printf("Check out I/O on new image with real data, with User-Defined fill-value, Reading Sub-setted Image\n"););
    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={-3.4,4.5,-0.03,100.4};   /* pixel with fill values */
        TEST_VARTYPE pixel[TEST_NCOMP]={-20.00,4.8,0.3,1.0};   /* pixel with fill values */
        TEST_VARTYPE pixel2[TEST_NCOMP]={1.23,1.0,-6500.0,350.0};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],pixel,sizeof(pixel));
                    else
                        HDmemcpy(&image0[i][j][0],pixel2,sizeof(pixel2));
                    if(((i>(TEST_YDIM/3)) && (i<(2*TEST_YDIM/3)))
                        && ((j>(TEST_XDIM/4)) && (j<(3*TEST_XDIM/4))))
                      {
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a2bb2",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Set the fill-value */
        ret=GRsetattr(riid,FILL_ATTR,TEST_NT,TEST_NCOMP,fill_pixel);
        CHECK(ret,FAIL,"GRsetattr");

        /* Create whole image */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the sub-set image back */
        start[XDIM]=(TEST_XDIM/4)+1;
        start[YDIM]=(TEST_YDIM/3)+1;
        count[XDIM]=((3*TEST_XDIM/4)-(TEST_XDIM/4))-1;
        count[YDIM]=((2*TEST_YDIM/3)-(TEST_YDIM/3))-1;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,count,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,sub_image,count[XDIM]*count[YDIM]*sizeof(pixel)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-setted image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
#endif /* QAK */

} /* end test_mgr_image_b2a2bb() */

static void test_mgr_image_b2a2cc(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B2a2cc - Read/Write images - with real Data - New Image - with User-Defined Fill Value - Sub-sampled Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with User-Defined fill-value, Writing Sub-sampled Image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   5
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE int16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_INT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={-3,4,-13,100,1200};   /* pixel with fill values */
        TEST_VARTYPE pixel[TEST_NCOMP]={-20,4,0,1,-367};   /* pixel with fill values */
        TEST_VARTYPE pixel2[TEST_NCOMP]={1,-11,-6500,350,20};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemfill(image0,fill_pixel,sizeof(fill_pixel),sizeof(image0)/sizeof(fill_pixel));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((i%2)!=0 && (j%2)!=0)
                      {
                          if((j%3)==0)
                              HDmemcpy(&image0[i][j][0],pixel,sizeof(TEST_VARTYPE)*TEST_NCOMP);
                          else
                              HDmemcpy(&image0[i][j][0],pixel2,sizeof(TEST_VARTYPE)*TEST_NCOMP);
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(TEST_VARTYPE)*TEST_YDIM*TEST_XDIM*TEST_NCOMP);

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a2cc",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Set the fill-value */
        ret=GRsetattr(riid,FILL_ATTR,TEST_NT,TEST_NCOMP,fill_pixel);
        CHECK(ret,FAIL,"GRsetattr");

        /* Create sub-sampled window with only the filled pixels in it */
        start[XDIM]=1;
        start[YDIM]=1;
        count[XDIM]=TEST_XDIM/2;
        count[YDIM]=TEST_YDIM/2;
        stride[XDIM]=stride[YDIM]=2;
        ret=GRwriteimage(riid,start,stride,count,sub_image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the whole image back */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(TEST_VARTYPE)*TEST_YDIM*TEST_XDIM*TEST_NCOMP))
          {
              MESSAGE(3, printf("Error reading data for new image with user-defined fill-value, sub-sampled image\n"););

              MESSAGE(8, for(i=0; i<TEST_YDIM; i++) \
                      for(j=0; j<TEST_XDIM; j++) \
                          for(k=0; k<TEST_NCOMP; k++) \
                            if(image[i][j][k]!=image0[i][j][k]) \
                                  printf("Location: [%d][%d][%d] image=%d, image0=%d \n",(int)i,(int)j,(int)k,(int)image[i][j][k],(int)image0[i][j][k]); );
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

/* The following test is unnecessary, fill-values only make a difference when writing out data -QAK */
#ifdef QAK
    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Reading Sub-sampled Image\n"););
    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={4000000,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={1230,1,65000,350000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                    if((i%2) && (j%2))
                      {
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a2cc2",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create whole image */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the sub-sample image back */
        start[XDIM]=1;
        start[YDIM]=1;
        count[XDIM]=TEST_XDIM/2;
        count[YDIM]=TEST_YDIM/2;
        stride[XDIM]=stride[YDIM]=2;
        ret=GRreadimage(riid,start,stride,count,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,sub_image,count[XDIM]*count[YDIM]*sizeof(fill_pixel)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-sampled image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
#endif /* QAK */

} /* end test_mgr_image_b2a2cc() */

static void test_mgr_image_b2b1(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B2b1 - Read/Write images - with real Data - Existing Image - Whole Image */
    MESSAGE(8, printf("Check out I/O from Existing Image - Whole Image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(DATAFILE,DFACC_READ,0);
    CHECK(fid,FAIL,"Hopen");

    /* Try initializing the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 n_datasets;       /* number of datasets */
        int32 n_attrs;          /* number of attributes */
        intn i;     /* local counting variables */
        
        ret=(intn)GRfileinfo(grid,&n_datasets,&n_attrs);
        CHECK(ret,FAIL,"GRfileinfo");

        for(i=0; i<n_datasets; i++)
          {
              int32 riid;               /* RI ID for an image */
              char name[MAX_IMG_NAME];  /* storage for the image's name */
              int32 ncomp;              /* number of components */
              int32 nt;                 /* NT of the components */
              int32 il;                 /* interlace of the image data */
              int32 dimsizes[2];        /* dimension sizes of the image */
              int32 n_attr;             /* number of attributes with each image */
              VOIDP img_data;           /* buffer for the image data */

              /* Attach to the image */
              riid=GRselect(grid,i);
              CHECK(riid,FAIL,"GRselect");

              /* Get the Image information */
              *name='\0';
              ret=GRgetiminfo(riid,name,&ncomp,&nt,&il,dimsizes,&n_attr);
              CHECK(ret,FAIL,"GRgetiminfo");

              /* Check the name for correctness */
              if(HDstrcmp(name,datafile_info[i].name))
                {
                    MESSAGE(3, printf("Name for image %d is: %s, should be %s\n",i,name,datafile_info[i].name););
                    num_errs++;
                } /* end if */

              /* Check the # of components */
              if(ncomp!=datafile_info[i].ncomp)
                {
                    MESSAGE(3, printf("Number of components for image %d is: %ld, should be %ld\n",i,(long)ncomp,(long)datafile_info[i].ncomp););
                    num_errs++;
                } /* end if */

              /* Check the NT of components */
              if(nt!=datafile_info[i].nt)
                {
                    MESSAGE(3, printf("NT of components for image %d is: %ld, should be %ld\n",i,(long)nt,(long)datafile_info[i].nt););
                    num_errs++;
                } /* end if */

              /* Check the interlace of components */
              if(il!=datafile_info[i].il)
                {
                    MESSAGE(3, printf("Interlace of components for image %d is: %ld, should be %ld\n",i,(long)il,(long)datafile_info[i].il););
                    num_errs++;
                } /* end if */

              /* Check the x-dimension of the image */
              if(dimsizes[XDIM]!=datafile_info[i].dimsizes[XDIM])
                {
                    MESSAGE(3, printf("X-dim of image %d is: %ld, should be %ld\n",i,(long)dimsizes[XDIM],(long)datafile_info[i].dimsizes[XDIM]););
                    num_errs++;
                } /* end if */

              /* Check the y-dimension of the image */
              if(dimsizes[YDIM]!=datafile_info[i].dimsizes[YDIM])
                {
                    MESSAGE(3, printf("Y-dim of image %d is: %ld, should be %ld\n",i,(long)dimsizes[YDIM],(long)datafile_info[i].dimsizes[YDIM]););
                    num_errs++;
                } /* end if */

              /* Check the # of attributes of the image */
              if(n_attr!=datafile_info[i].n_attr)
                {
                    MESSAGE(3, printf("# of attributes for image %d is: %ld, should be %ld\n",i,(long)n_attr,(long)datafile_info[i].n_attr););
                    num_errs++;
                } /* end if */

              /* Check the image data itself */
              {
                  int32 start[2];
                  int32 stride[2];

                    img_data=HDmalloc(dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE));
                    CHECK(img_data,NULL,"HDmalloc");

                    HDmemset(img_data,0,dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE));

                    start[0]=start[1]=0;
                    stride[0]=stride[1]=1;
                    ret=GRreadimage(riid,start,stride,dimsizes,img_data);

                    switch(i)
                      {
                          case 0:
                              if(0!=HDmemcmp(img_data,image00,sizeof(image00)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                          case 1:
                              if(0!=HDmemcmp(img_data,image1,sizeof(image1)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                          case 2:
                              if(0!=HDmemcmp(img_data,image2,sizeof(image2)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                          case 3:
                              if(0!=HDmemcmp(img_data,image3,sizeof(image3)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                          case 4:
                              if(0!=HDmemcmp(img_data,image4,sizeof(image4)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                      } /* end switch */

                    HDfree(img_data);
              } /* end block */

              /* End access to the image */
              ret=GRendaccess(riid);
              CHECK(ret,FAIL,"GRendaccess");
          } /* end for */
      } /* end block */

    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

} /* end test_mgr_image_b2b1() */

static void test_mgr_image_b2b2(void)
{
/* B2b2 - Read/Write images - with real Data - Existing Image - Sub-setted Image */
    /* This test is unnecessary, I think this case has been adequately covered above -QAK */
} /* end test_mgr_image_b2b2() */

static void test_mgr_image_b2b3(void)
{
/* B2b3 - Read/Write images - with real Data - Existing Image - Sub-sampled Image */
    /* This test is unnecessary, I think this case has been adequately covered above -QAK */
} /* end test_mgr_image_b2b3() */

/****************************************************************
**
**  test_mgr_image(): Multi-file Raster Image I/O Test Routine
** 
**      A. GRcreate/GRselect/GRendaccess w/GRgetiminfo
**      B. Write/Read images
**          1. With no Data
**              a. Default fill value
**              b. user defined fill value
**          2. With real Data
**              a. New Image
**                  1. With default fill value
**                      aa. Whole image
**                      bb. Sub-setted image
**                      cc. Sub-sampled image
**                  2. With user defined vill value
**                      aa. Whole image
**                      bb. Sub-setted image
**                      cc. Sub-sampled image
**              b. Existing Image
**                  1. Whole image
**                  2. Sub-setted image
**                  3. Sub-sampled image
** 
****************************************************************/
static void
test_mgr_image(void)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Image I/O routines\n"););

    test_mgr_image_b1a();
    test_mgr_image_b1b();
    test_mgr_image_b2a1aa();
    test_mgr_image_b2a1bb1();
    test_mgr_image_b2a1bb2();
    test_mgr_image_b2a1cc1();
    test_mgr_image_b2a1cc2();
    test_mgr_image_b2a2aa();
    test_mgr_image_b2a2bb();
    test_mgr_image_b2a2cc();
    test_mgr_image_b2b1();
    test_mgr_image_b2b2();
    test_mgr_image_b2b3();
}   /* end test_mgr_image() */

/****************************************************************
**
**  test_mgr_index(): Multi-file Raster ID/Ref/Index Test Routine
** 
**  III. ID/Ref/Index Functions
**      A. GRidtoref
**      B. GRreftoindex
** 
****************************************************************/
static void
test_mgr_index(void)
{
    /* output message about test being performed */
    MESSAGE(6, printf("Testing Multi-File Raster id/ref/index routines\n"););

/* I believe that these are adequately tested in the test_mgr_image routine -QAK */
}   /* end test_mgr_index() */

/****************************************************************
**
**  test_mgr_interlace(): Multi-file Raster Interlace Test Routine
** 
**  IV. Interlace Functions [Need to be implemented]
**      A. GRreqlutil - tested in the palette test below.
**      B. GRreqimageil
** 
****************************************************************/
static void
test_mgr_interlace(void)
{
    int32 fid;              /* hdf file id */
    int32 grid;             /* grid for the interface */
    int32 n_datasets;       /* number of datasets */
    int32 n_attrs;          /* number of attributes */
    int32 ret;              /* generic return value */
    VOIDP image;            /* image to retrieve */

    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Interlace routines\n"););

    /* open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* initialize the gr interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        intn i,j;     /* local counting variables */
        
        ret=(intn)GRfileinfo(grid,&n_datasets,&n_attrs);
        CHECK(ret,FAIL,"GRfileinfo");

        for(i=0; i<n_datasets; i++)
          {
              int32 riid;               /* RI ID for an image */
              char name[MAX_IMG_NAME];  /* storage for the image's name */
              int32 ncomp;              /* number of components */
              int32 nt;                 /* NT of the components */
              int32 il;                 /* interlace of the image data */
              int32 start[2];
              int32 stride[2];
              int32 dimsizes[2];        /* dimension sizes of the image */
              int32 n_attr;             /* number of attributes with each image */
              VOIDP img_data;           /* buffer for the image data */

              /* Attach to the image */
              riid=GRselect(grid,i);
              CHECK(riid,FAIL,"GRselect");

              /* Get the Image information */
              *name='\0';
              ret=GRgetiminfo(riid,name,&ncomp,&nt,&il,dimsizes,&n_attr);
              CHECK(ret,FAIL,"GRgetiminfo");

              image=HDmalloc(dimsizes[XDIM]*dimsizes[YDIM]*ncomp*DFKNTsize(nt|DFNT_NATIVE));
              CHECK(image,NULL,"HDmalloc");

              start[0]=start[1]=0;
              stride[0]=stride[1]=1;
              ret=GRreadimage(riid,start,stride,dimsizes,image);

              /* Check the image data itself */
              for(j=(intn)MFGR_INTERLACE_PIXEL; j<=(intn)MFGR_INTERLACE_COMPONENT; j++)
                {
                    VOIDP pixel_buf;

                    img_data=HDmalloc(dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE));
                    CHECK(img_data,NULL,"HDmalloc");

                    pixel_buf=HDmalloc(dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE));
                    CHECK(pixel_buf,NULL,"HDmalloc");

                    HDmemset(img_data,0,dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE));

                    ret=GRreqimageil(riid,j);
                    CHECK(ret,FAIL,"GRreqimageil");

                    start[0]=start[1]=0;
                    stride[0]=stride[1]=1;
                    ret=GRreadimage(riid,start,stride,dimsizes,img_data);

                    GRIil_convert(image,MFGR_INTERLACE_PIXEL,pixel_buf,j,dimsizes,ncomp,nt);
                    if(0!=HDmemcmp(img_data,pixel_buf,
                          dimsizes[XDIM]*dimsizes[YDIM]*ncomp*DFKNTsize(nt|DFNT_NATIVE)))
                      {
                          MESSAGE(3, printf("Error reading data for image %d, j=%d\n",i,j););
                          num_errs++;
                      } /* end if */
                    HDfree(img_data);
                    HDfree(pixel_buf);
                } /* end for */

              HDfree(image);

              /* End access to the image */
              ret=GRendaccess(riid);
              CHECK(ret,FAIL,"GRendaccess");
          } /* end for */
      } /* end block */
    
    /* shut down the gr interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
}   /* end test_mgr_interlace() */

/****************************************************************
**
**  test_mgr_lut(): Multi-file Raster LUT/Palette Test Routine
** 
**  V. Palette Functions
**      A. GRgetlutid w/GRgetlutinfo
**      B. Read/Write Palettes
**          1. GRwritelut
**          2. GRreadlut
** 
****************************************************************/
static void
test_mgr_lut(void)
{
    int32 fid;              /* hdf file id */
    int32 grid;             /* grid for the interface */
    int32 ret;              /* generic return value */

    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Palette routines\n"););

    /* open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* initialize the gr interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

/* pick up here -QAK2 */
    {
        intn i,j;     /* local counting variables */
        int32 riid;               /* RI ID for an image */
        int32 lutid;              /* RI ID for an image */
        char name[MAX_IMG_NAME];  /* storage for the image's name */
        int32 ncomp;              /* number of components */
        int32 pal_ncomp;          /* number of palette components */
        int32 nt;                 /* NT of the components */
        int32 pal_nt;             /* NT of the palette components */
        int32 il;                 /* interlace of the image data */
        int32 pal_il;             /* interlace of the palette data */
        int32 dimsizes[2];        /* dimension sizes of the image */
        int32 pal_entries;        /* number of entries in the palette */
        int32 n_attr;             /* number of attributes with each image */
        uint8 *tmp_data;          /* temporary buffer pointer */
        VOIDP pal_data;           /* buffer for the palette data */

        /* Attach to the image */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Get the Image information */
        *name='\0';
        ret=GRgetiminfo(riid,name,&ncomp,&nt,&il,dimsizes,&n_attr);
        CHECK(ret,FAIL,"GRgetiminfo");

        lutid=GRgetlutid(riid,0);
        CHECK(lutid,FAIL,"GRgetlutid");
        
        /* Get the Palette information */
        ret=GRgetlutinfo(lutid,&pal_ncomp,&pal_nt,&pal_il,&pal_entries);
        CHECK(ret,FAIL,"GRgetlutinfo");

        /* Check the palette values, they should all be "nil" values */
        if(pal_ncomp!=0)
          {
              MESSAGE(3, printf("Incorrect palette components\n"););
              num_errs++;
          } /* end if */
        if(pal_nt!=DFNT_NONE)
          {
              MESSAGE(3, printf("Incorrect palette number-type\n"););
              num_errs++;
          } /* end if */
        if(pal_il!=(-1))
          {
              MESSAGE(3, printf("Incorrect palette interlace, pal_il=%d\n",(int)pal_il););
              num_errs++;
          } /* end if */
        if(pal_entries!=0)
          {
              MESSAGE(3, printf("Incorrect palette # of entries\n"););
              num_errs++;
          } /* end if */

        /* Set the palette components */
        pal_ncomp=3;
        pal_nt=DFNT_UINT8;
        pal_il=(int32)MFGR_INTERLACE_PIXEL;
        pal_entries=256;

        pal_data=HDmalloc(pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE));
        CHECK(pal_data,NULL,"HDmalloc");

        /* Initialize the palette data, in 'pixel' interlace */
        tmp_data=(uint8 *)pal_data;
        for(j=0; j<pal_entries; j++)
            for(i=0; i<pal_ncomp; i++)
                *tmp_data++=(uint8)(j*i);

        /* Write the palette out */
        ret=GRwritelut(lutid,pal_ncomp,pal_nt,pal_il,pal_entries,pal_data);
        CHECK(ret,FAIL,"GRwritelut");

        /* Check the image data itself */
        for(j=(intn)MFGR_INTERLACE_PIXEL; j<=(intn)MFGR_INTERLACE_COMPONENT; j++)
          {
              VOIDP pixel_buf;
              int32 dimsizes2[2];

              tmp_data=HDmalloc(pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE));
              CHECK(tmp_data,NULL,"HDmalloc");

              pixel_buf=HDmalloc(pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE));
              CHECK(pixel_buf,NULL,"HDmalloc");

              HDmemset(tmp_data,0,pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE));

              ret=GRreqlutil(lutid,j);
              CHECK(ret,FAIL,"GRreqlutil");

              ret=GRreadlut(lutid,tmp_data);

              dimsizes2[XDIM]=1;
              dimsizes2[YDIM]=pal_entries;
              GRIil_convert(pal_data,MFGR_INTERLACE_PIXEL,pixel_buf,j,dimsizes2,pal_ncomp,pal_nt);
              if(0!=HDmemcmp(tmp_data,pixel_buf,
                    pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE)))
                {
                    MESSAGE(3, printf("Error reading data for palette j=%d\n",j););
                    num_errs++;
                } /* end if */
              HDfree(tmp_data);
              HDfree(pixel_buf);
          } /* end for */

        HDfree(pal_data);

        /* End access to the image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
      } /* end block */
    
    /* shut down the gr interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
}   /* end test_mgr_lut() */

/****************************************************************
**
**  test_mgr_special(): Multi-file Raster Special Element Test Routine
** 
**  VI. Special Element Functions [Need to be implemented]
**      A. GRsetexternalfile
**      B. GRsetaccesstype
**      C. GRsetcompress
** 
****************************************************************/
static void
test_mgr_special(void)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Special Element routines\n"););
}   /* end test_mgr_special() */

/****************************************************************
**
**  test_mgr_attr(): Multi-file Raster Attribute Test Routine
** 
**  VII. Atribute Functions
**      A. GRattrinfo
**      B. Read/Write Attributes
**          1. GRsetattr
**          2. GRgetattr
**      C. GRfindattr
** 
****************************************************************/
static void
test_mgr_attr(void)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Attribute routines\n"););

/* I believe that these are adequately tested in the test_mgr_image routine -QAK */
}   /* end test_mgr_attr() */

/****************************************************************
**
**  test_mgr(): Main multi-file raster image test routine
** 
****************************************************************/
void
test_mgr(void)
{
    /*
        Each major outline portion has it's own main function:
        I. Interface Initialization     - test_mgr_init
        II. Create Images               - test_mgr_image
        III. ID/Ref/Index Functions     - test_mgr_index
        IV. Interlace Functions         - test_mgr_interlace
        V. Palette Functions            - test_mgr_lut
        VI. Special Element Functions   - test_mgr_special
        VII. Atribute Functions         - test_mgr_attr
    */

    /* Output message about test being performed */
    MESSAGE(5, printf("Testing Multi-file Raster routines\n"););

    test_mgr_init();
    test_mgr_image();
    test_mgr_index();
    test_mgr_interlace();
    test_mgr_lut();
    test_mgr_special();
    test_mgr_attr();
}   /* test_mgr() */

