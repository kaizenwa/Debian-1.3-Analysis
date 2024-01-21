/*
 *  Extern.h
 *
 *  Mike VanHilst, Smithsonian Astrophysical Observatory
 *  Cambridge, Massachusetts           December 31, 1987
 *
 *  updated with new names 21 September 1988, 30 January 1989
 *  updated with wcs 6 July 1995 by Doug Mink, SAO
 *
 *  externs to reference structures declared in MainInit.c
 */


extern struct windowRec btnbox;
extern struct colorRec color;
extern struct windowRec colorbox;
extern struct controlRec control;
extern struct coordRec coord;
extern struct windowRec dispbox;
extern struct windowRec graphbox;
extern struct imageRec img;
extern struct windowRec magnibox;
extern struct windowRec panbox;
extern struct cursorRec cursor;
extern struct bufferRec buffer;
extern struct WorldCoor *wcs;
extern char *wcscoor;        /* Output coordinate system */
extern char *wcscommand;     /* WCS command */
