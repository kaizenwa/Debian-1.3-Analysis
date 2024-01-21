/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

#include <stdio.h>
#include "defs.h"


/* real globals used for dvi processing*/
fontdescvect* fontvect=NULL;
drawlistelement *thedrawlist=NULL;
pagelistelement *pagelist=NULL;
int pageanz;
int dvierrorflag;
pagelistelement *cpage=NULL;
float fshrink=1.0;
int ishrinkonly=0;
uchar colors=BLACKNWHITE;
uchar *greytab=NULL;

/* arguments as globals */
int verbose=0;
int numberargstrlen;
char  *numberargstr=NULL;

/* real globals for displaymanaging */
int vgaxdim, vgaydim;
int directscreen;
int vgastatuslen, vgastatushight, vgastatuslines, vgamaxstatuslines;
int dvixpos, dviypos;
int frameon, recton, texton;
int markon, hypon, statustype, statusforce;
int marksxpxl, marksypxl; 
float markdxmm, markdymm;
int textx1pxl, texty1pxl, textx2pxl, texty2pxl;
int textx3pxl, texty3pxl, textx4pxl, texty4pxl;
float rectxmm, rectymm, rectwmm, recthmm; 
char *dviname=NULL;


float unitomm,mmtounit;
int unitcomma;
char* unitname; 
 

/* global constants */
BMUNIT	*bit_masks=NULL; 
uchar *bit_count=NULL;
char* workdir;

/* options are global ... bad style */
char *tfmprefix=NULL;
char *fontprefix=NULL; 
char *fontformat=NULL;
char *startupfilename=NULL;
int savestartup;
long newmag;
int xres,yres;
int pagemovetop, moveoverpages, bookmkmode, unitstar;


/* globals for multible files and bookmarks ... */
fontdesc* fontdatabase=NULL;
filemark** filemks=NULL;
int nfilemks=0;
bookmarklist* curbmks=NULL;
bookmarklist* visbmks=NULL;
filemark* curfmk=NULL;
filemark* visfmk=NULL;
filemark fmkstar;

/* not realy global globals */
int truevgaxdim,truevgaydim; /*allow saving windowsize*/



















