/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * zgv.h - constants, typedefs.
 */

#define ZGV_VER		"2.7"


#define _PIC_OK			 0
#define _PICERR_NOFILE		-1
#define _PICERR_NOMEM		-2
#define _PICERR_BADMAGIC	-3
#define _PICERR_NOCOLOURMAP	-4
#define _PICERR_NOIMAGE		-5
#define _PICERR_UNSUPPORTED	-6
#define _PICERR_CORRUPT		-7
#define _PICERR_SHOWN_ALREADY	-8
#define _PICERR_ISRLE		-9
#define _PICERR_PNG_ERR		-10

typedef struct {
  int width,height;
  int bpp,numcols;
  char type[4];
  } PICINFO;

typedef void (*hffunc)(int,int);

typedef unsigned char byte;

extern int idx_light,idx_medium,idx_dark,idx_black;
extern int tagview_mode;
extern int zgv_ttyfd;
extern char zgv_pngerr[];

extern int wait_for_foreground();
