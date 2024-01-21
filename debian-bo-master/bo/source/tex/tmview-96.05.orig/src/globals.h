/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */



extern fontdescvect* fontvect;
extern drawlistelement *thedrawlist;
extern pagelistelement *pagelist;
extern int pageanz;
extern int dvierrorflag;
extern pagelistelement *cpage;
extern float fshrink;
extern int ishrinkonly;
extern uchar colors;
extern uchar *greytab;

extern int verbose;
extern int numberargstrlen;
extern char  *numberargstr;

extern int vgaxdim, vgaydim;
extern int directscreen;
extern int vgastatuslen, vgastatushight, vgastatuslines, vgamaxstatuslines;
extern int dvixpos, dviypos;
extern int frameon, recton, texton;
extern int markon, hypon, statustype, statusforce;
extern int marksxpxl, marksypxl; 
extern float markdxmm, markdymm;
extern int textx1pxl, texty1pxl, textx2pxl, texty2pxl;
extern int textx3pxl, texty3pxl, textx4pxl, texty4pxl;
extern float rectxmm, rectymm, rectwmm, recthmm; 
extern char *dviname;


extern float unitomm,mmtounit;
extern int unitcomma;
extern char* unitname; 
 

extern BMUNIT	*bit_masks; 
extern uchar *bit_count;
extern char* workdir;


extern char *tfmprefix;
extern char *fontprefix; 
extern char *fontformat;
extern char *startupfilename;
extern savestartup;
extern long newmag;
extern int xres,yres;
extern int pagemovetop, moveoverpages, bookmkmode, unitstar;


extern fontdesc* fontdatabase;
extern filemark** filemks;
extern int nfilemks;
extern bookmarklist* curbmks;
extern bookmarklist* visbmks;
extern filemark* curfmk;
extern filemark* visfmk;
extern filemark fmkstar;

extern int truevgaxdim,truevgaydim; 


















