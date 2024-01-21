/*
 * CGMIN.C
 *
 * $Id: cgmin.c,v 1.1 1993/08/27 17:19:10 munro Exp $
 *
 * Define the CGM reader/echoer for GIST.
 *
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "cgmin.h"
#include "engine.h"

#include <stdio.h>
#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

#ifdef STDC_HEADERS
#include <string.h>
#else
#ifndef SIZE_T_TYPE
#define SIZE_T_TYPE unsigned long
#endif
extern SIZE_T_TYPE strlen(const char *);
extern int strcmp(const char *, const char *);
extern char *strcpy(char *, const char *);
extern int strncmp(const char *, const char *, SIZE_T_TYPE);
extern void *memcpy(void *, const void *, SIZE_T_TYPE);
#endif

extern void *malloc(long);
extern void free(void *);

extern void CGMinfo(void);  /* defined here */

/* function that returns the number of pages in a CGM file family */
extern int CGM_pages(void);

/* Some commercial GKS packages (e.g.- ATC) set the palette to
   0==black, 1==white, then use color 1 as the foreground color, but
   print it as black (sigh).  This flag tries to accomodate such
   deviant behavior.  */
extern int bg0fg1;
int bg0fg1= 0;

typedef struct CGM CGM;
typedef unsigned char Octet;

Engine *outEngines[8];
int outTypes[8];

static int EchoPage(int number, int inc);

static CGM *NewCGM(const char *name, FILE *file, int maxPages);
static CGM *DeleteCGM(CGM *cgm);
static void NextName(char *filename);
static int BeginCatalog(CGM *cgm);

static long ReadParameters(long nOctets, GpColor *colors, long maxColors);
static int SkipParameters(long nOctets);
static long ReadElement(int *class, int *id);

static long DePascalify(char *text, const Octet *pasctxt, long nMax);
static int Snarf16(Octet *param);
static long SnarfInteger(Octet *param);
static GpReal SnarfReal(Octet *param);
static void RememberPoint(GpReal x0, GpReal y0);
static char *SnarfMoreText(Octet *param);
static char *SnarfText(Octet *param);
static char *PathifyText(char *text, int path);
static GpReal SnarfSA(Octet *param, int md, GpReal dflt);
static int SnarfColor(Octet *param, long nOctets);
static void ConvertPoints(long n, const short *vdc,
			  GpReal *ndcx, GpReal *ndcy);
static int CheckScratch(long nPoints);

static void SetVDC(int *vdc);
static void SetClip(int on);
static void ResetPalette(int forPage);

static int DoClass0(int id, long nOctets);
static int DoClass1(int id, long nOctets);
static int DoClass2(int id, long nOctets);
static int DoClass3(int id, long nOctets);
static int DoClass4(int id, long nOctets);
static int DoClass5(int id, long nOctets);
static int DoClass6(int id, long nOctets);
static int DoClass7(int id, long nOctets);

/* ------------------------------------------------------------------------ */

static int currentPage= 0;

static char metafileName[256];                    /* from BEGIN METAFILE */
static char pictureName[256];          /* from most recent BEGIN PICTURE */
static char metafileDescription[256];       /* from METAFILE DESCRIPTION */
static int isGistCGM= 0;  /* set if metafileDescription begins "Gist;  " */

static int nBytesInt= 2;
static int vdcIllegible, realIllegible, indexIllegible,
  cvalIllegible, cndxIllegible, cselIllegible;

#define N_GIST_FONTS 20
static char *gistFonts[]= {
  "Courier", "Courier-Bold", "Courier-Oblique", "Courier-BoldOblique",
  "Times-Roman", "Times-Bold", "Times-Italic", "Times-BoldItalic",
  "Helvetica", "Helvetica-Bold", "Helvetica-Oblique", "Helvetica-BoldOblique",
  "Symbol", "Symbol", "Symbol", "Symbol",
  "NewCenturySchlbk-Roman", "NewCenturySchlbk-Bold",
  "NewCenturySchlbk-Italic", "NewCenturySchlbk-BoldItalic" };
static int defaultNumbers[]= {
  0, 8, 4, 12, 1, 9, 5, 13, 2, 10, 6, 14, 3, 11, 7, 15, 16, 17, 18, 19 };

/* Transformation from vdc->ndc is known on a per-page basis */
#define DEFAULT_METRIC (1.0/(25545.24*(ONE_INCH/25.4)))
static GpReal cgmScale= (1.0/25545.24), cgmScaleX= (1.0/25545.24),
  cgmScaleY= (1.0/25545.24), cgmOffsetX= 0.0, cgmOffsetY= 0.0;
int cgmLandscape= 0;
extern int cgmScaleToFit;
int cgmScaleToFit= 0;

/* May as well have a permanently allocated palette of maximal size */
static GpColorCell cgmPalette[256], defaultPalette[256];
static int cgmNColors= 0, defaultNColors=  0, cgmNColMax;
static int cgmColorChange= 0, alteredPalette= 0, setXPalette= 0,
  needPalette= 0;

struct CGMDesc {
  /* metafile descriptor elements */
  int colorMaxI, colorMaxV[6];
  int *fonts, nfonts, maxFonts;

  /* picture descriptor elements */
  int scaling;
  GpReal metric;
  int lwidth, msize, ewidth;
  int vdc[4];
  GpColorCell background;

#define ABSTRACT 0
#define METRIC 1
#define INDEXED 0
#define DIRECT 1
#define ABSOLUTE 0
#define SCALED 1
};

/* cgmDesc represents default metafile and picture descriptor settings as
   defined in the ANSI X3.122 - 1986 (Part 3) standard */
struct CGMDesc cgmDesc= {
  63, {0, 0, 0, 255, 255, 255}, defaultNumbers, N_GIST_FONTS, 0,
  0, DEFAULT_METRIC, 1, 1, 1, {0, 0, 32767, 32767}, {255, 255, 255, 255} };

/* pageDesc represents cgmDesc, as modified by the defaults
   replacement section of the metafile header */
struct CGMDesc pageDesc= {
  63, {0, 0, 0, 255, 255, 255}, defaultNumbers, N_GIST_FONTS, 0,
  0, DEFAULT_METRIC, 1, 1, 1, {0, 0, 32767, 32767}, {255, 255, 255, 255} };

/* pictDesc represents pageDesc, as modified
   by any picture descriptor elements */
struct CGMDesc pictDesc= {
  63, {0, 0, 0, 255, 255, 255}, defaultNumbers, N_GIST_FONTS, 0,
  0, DEFAULT_METRIC, 1, 1, 1, {0, 0, 32767, 32767}, {255, 255, 255, 255} };

struct CGMControls {
  int auxColor, transparency;
  short clipBox[4];
  int clipOn, clipDefault;
};

/* cgmControls represent default control settings as defined in the
   ANSI X3.122 - 1986 (Part 3) standard */
struct CGMControls cgmControls= {
  0, 1, {0, 0, 32767, 32767}, 1, 1 };

/* pageControls represent cgmControls, as modified by the defaults
   replacement section of the metafile header */
struct CGMControls pageControls= {
  0, 1, {0, 0, 32767, 32767}, 1, 1 };

/* pictControls represents pageControls, as modified
   by any picture descriptor elements */
struct CGMControls pictControls= {
  0, 1, {0, 0, 32767, 32767}, 1, 1 };

struct NonGistAttribs {
  /* non-Gist constructs */
  int eVisible;         /* edge visibility separate from gistA.e.type */
  int hindex, pindex;   /* separate hatch and pattern indices */
  GpReal upX, upY, baseX, baseY;  /* character up and base vectors */
  GpReal expand, spacing;         /* other text attributes */
  int prec, path;                 /* text precision, path */
};

struct CGMAttributes {

  /* line attributes (GpLines, GpDisjoint) */
  GpLineAttribs l;

  /* marker attributes (GpMarkers) */
  GpMarkerAttribs m;

  /* filled area attributes (GpFill) */
  GpFillAttribs f;

  /* text attributes (GpText) */
  GpTextAttribs t;

  /* non-Gist constructs */
  struct NonGistAttribs ng;
};

/* cgmDefaults represent default attribute settings as defined in the
   ANSI X3.122 - 1986 (Part 3) standard */
static struct CGMAttributes cgmDefaults= {
  { FG_COLOR, L_SOLID, 1.0 },                     /* line attributes */
  { FG_COLOR, M_ASTERISK, 1.0 },                  /* marker attributes */
  { FG_COLOR, F_SOLID, 0, 0.01, 0.01, 0.0, 0.0 }, /* fill attributes */
  { FG_COLOR, 0, 0.0156,
      TX_RIGHT, TH_NORMAL, TV_NORMAL },           /* text attributes */

  {0, 0, 0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 2, 0}   /* non-Gist attributes,
						     except pattern table */
  };

/* pageDefaults represent cgmDefaults, as modified by the defaults
   replacement section of the metafile header */
static struct CGMAttributes pageDefaults= {
  { FG_COLOR, L_SOLID, 1.0 },                     /* line attributes */
  { FG_COLOR, M_ASTERISK, 1.0 },                /* marker attributes */
  { FG_COLOR, F_SOLID, 0, 0.01, 0.01, 0.0, 0.0 }, /* fill attributes */
  { FG_COLOR, 0, 0.0156,
      TX_RIGHT, TH_NORMAL, TV_NORMAL },           /* text attributes */

  {0, 0, 0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 2, 0}   /* non-Gist attributes,
						     except pattern table */
  };

/* current values of non-Gist attributes
   (current values of others in gistA) */
static struct NonGistAttribs nonGistA= {
  0, 0, 0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 2, 0};

/* ------------------------------------------------------------------------ */

struct CGM {
  char *name;
  FILE *file;   /* non-zero only if currently open */
  CGM *prev, *next;
  int firstPage, nPages, maxPages;
  long *pageAddress, catalogEnd;
};

static CGM *currentCGM= 0, *firstCGM= 0, *catalogCGM= 0;

static int cgmCmdLen;  /* 2 or 4, length of previous ReadElement command */
static Octet *cmdString= 0;
static Octet *currentCmd;
static long cmdLength;

/* Name incrementer copied from cgm.c */
static void NextName(char *filename)
{
  int i, len= filename? strlen(filename) : 0;
  if (len>4 && (strcmp(filename+len-4, ".cgm")==0 ||
		strcmp(filename+len-4, ".CGM")==0)) i= len-4;
  else i= len;
  while (i-- > 0) {
    if (filename[i]=='9') {
      filename[i]= '0';
    } else {
      if (filename[i]=='Z' || filename[i]>='z') filename[i]= '0';
      else filename[i]++;
      break;
    }
  }
}

static CGM *NewCGM(const char *name, FILE *file, int maxPages)
{
  CGM *cgm= (CGM *)malloc(sizeof(CGM)+strlen(name)+1);
  long *pageAddress= (long *)malloc(sizeof(long)*maxPages);

  if (!cgm || !pageAddress) {
    if (cgm) free(cgm);
    if (pageAddress) free(pageAddress);
    Warning("memory manager failed trying to open ", name);
    return 0;
  }

  cgm->name= ((char *)cgm)+sizeof(CGM);
  strcpy(cgm->name, name);
  if (!file) {
    NextName(cgm->name);
    file= fopen(cgm->name, "rb");
    if (!file) {
      free(cgm);
      free(pageAddress);
      return 0;
    }
  }
  cgm->file= file;
  cgm->prev= cgm->next= 0;
  cgm->firstPage= 1;
  cgm->nPages= 0;
  cgm->maxPages= maxPages;
  cgm->catalogEnd= -1;
  cgm->pageAddress= pageAddress;

  return cgm;
}

static CGM *DeleteCGM(CGM *cgm)
{
  CGM *next= cgm->next;
  if (cgm->prev) cgm->prev->next= next;
  if (cgm->pageAddress) free(cgm->pageAddress);
  free(cgm);
  return next;
}

static int BeginCatalog(CGM *cgm)
{
  CGM *oldCGM= currentCGM;
  int class, id, bad;
  long nOctets;

  currentCGM= cgm;

  nOctets= ReadElement(&class, &id);

  if (class==0 && id==1) {
    bad= 0;
    for (;;) {
      SkipParameters(nOctets);
      nOctets= ReadElement(&class, &id);
      if (nOctets>=0) {
	if (class==0) {
	  if (id==3 || id==2) break;  /* begin picture or end metafile */
	  else if (id!=0) {
	    Warning("bad class 0 element in metafile descriptor", "");
	    bad= 1;
	    break;
	  }
	} else if (class!=1 && class!=6 && class!=7) {
	  Warning("bad element class in metafile descriptor", "");
	  bad= 1;
	  break;
	}
      } else {
	bad= 1;
	break;
      }
    }
  } else {
    Warning("BEGIN METAFILE element missing", "");
    bad= 1;
  }

  currentCGM= oldCGM;

  if (bad) {
    Warning(cgm->name, " is not a binary CGM, cannot open");
    return 1;
  }

  if (id==3) {
    cgm->pageAddress[0]= ftell(cgm->file)-cgmCmdLen;
    currentCGM= cgm;
    SkipParameters(nOctets);
    currentCGM= oldCGM;
    cgm->catalogEnd= ftell(cgm->file);
    cgm->nPages= 1;
  } else {
    cgm->catalogEnd= -1;
  }
  return 0;
}

int CatalogCGM(void)
{
  int class, id, nPage;
  long nOctets;
  CGM *tmp, *cgm= currentCGM;

  if (!catalogCGM) return 0;

  while (catalogCGM->catalogEnd<0) {
    if (catalogCGM!=currentCGM) {
      fclose(catalogCGM->file);
      catalogCGM->file= 0;
    }
    nPage= catalogCGM->firstPage+catalogCGM->nPages;
    catalogCGM= NewCGM(catalogCGM->name, (void *)0, 256);
    if (!catalogCGM || BeginCatalog(catalogCGM)) {
      if (catalogCGM) {
	fclose(catalogCGM->file);
	DeleteCGM(catalogCGM);
	catalogCGM= 0;
      }
      return 0;
    }
    catalogCGM->firstPage= nPage;
  }

  fseek(catalogCGM->file, catalogCGM->catalogEnd, SEEK_SET);

  for (;;) {
    currentCGM= catalogCGM;
    for (;;) {
      nOctets= ReadElement(&class, &id);
      if (nOctets>=0) {
	if (class==0) {
	  if (id==3 || id==2) break;  /* begin picture or end metafile */
	}
	SkipParameters(nOctets);
      } else {
	Warning("missing END METAFILE element in ", catalogCGM->name);
	class= 0;
	id= 2;
	break;
      }
    }
    currentCGM= cgm;

    if (id==3) {
      if (catalogCGM->maxPages==catalogCGM->nPages) {
	long *pa= (long *)malloc(sizeof(long)*(catalogCGM->maxPages+256));
	int i;
	if (!pa) {
	  catalogCGM->catalogEnd= -1;
	  if (catalogCGM!=currentCGM) fclose(catalogCGM->file);
	  catalogCGM= 0;
	  Warning("memory manager failed cataloging ", catalogCGM->name);
	  break;
	}
	for (i=0 ; i<catalogCGM->nPages ; i++)
	  pa[i]= catalogCGM->pageAddress[i];
	free(catalogCGM->pageAddress);
	catalogCGM->pageAddress= pa;
	catalogCGM->maxPages+= 256;
      }
      catalogCGM->pageAddress[catalogCGM->nPages++]=
	ftell(catalogCGM->file)-cgmCmdLen;
      SkipParameters(nOctets);
      catalogCGM->catalogEnd= ftell(catalogCGM->file);
      break;

    } else { /* found END METAFILE */
      tmp= catalogCGM;
      tmp->catalogEnd= -1;
      if (tmp!=currentCGM) {
	fclose(tmp->file);
	tmp->file= 0;
      }
      tmp= tmp->next= NewCGM(catalogCGM->name, (void *)0, 256);
      if (tmp) {
	if (BeginCatalog(tmp)) {
	  fclose(tmp->file);
	  DeleteCGM(tmp);
	  tmp= 0;
	} else {
	  tmp->prev= catalogCGM;
	  tmp->firstPage= catalogCGM->firstPage+catalogCGM->nPages;
	}
      }
      catalogCGM= tmp;
      if (!tmp) return 0;
    }
  }

  return 1;
}

/* ------------------------------------------------------------------------ */

int OpenCGM(const char *file)
{
  FILE *f= fopen(file, "rb");
  CGM *cgm;
  int class, id;
  long nOctets;

  if (!f) {
    Warning("unable to open CGM input file: ", file);
    return 1;
  }

  cgm= NewCGM(file, f, 256);
  if (!cgm) {
    fclose(f);
    return 1;
  }

  cmdString= 0;  /* just to be sure... */
  if (BeginCatalog(cgm)) {
    fclose(f);
    DeleteCGM(cgm);
    return 1;
  }

  if (fseek(f, 0L, SEEK_SET)) {
    fclose(f);
    Warning("fseek failed for CGM, cannot open ", file);
    return 1;
  }

  if (currentCGM) fclose(currentCGM->file);
  if (catalogCGM && catalogCGM!=currentCGM) fclose(catalogCGM->file);
  while (firstCGM) firstCGM= DeleteCGM(firstCGM);
  currentCGM= firstCGM= catalogCGM= cgm;

  /* Read metafile descriptor, setting page defaults */
  for (;;) {
    nOctets= ReadElement(&class, &id);
    if (nOctets<0) break;
    if (class==0) {
      if (id==2 || id==3) break;
      else DoClass0(id, nOctets);
    } else if (class==1) DoClass1(id, nOctets);
    else if (class==6) DoClass6(id, nOctets);
    else if (class==7) DoClass7(id, nOctets);
  }

  currentPage= 1;
  return 0;
}

int ReadCGM(int *mPage, int *nPage, int *sPage, int nPageGroups)
{
  int m0=0, m=1, n, s= 1;
  int echo_page_return= 0;

  if (!currentCGM) return 1;

  while (nPageGroups-- > 0) {
    m0= *mPage++;
    n= *nPage++;
    s= *sPage++;
    if ((m0<n && s<=0) || (m0>n && s>=0) || (s==0 && m0!=n)) {
      Warning("illegal page group, showing only first frame", "");
      n= m0;
      s= 1;
    } else if (s==0) {
      s= 1;
    }

    for (m=m0 ; s>0? m<=n : m>=n ; m+= s) {
      echo_page_return= EchoPage(m, m==m0? s : 0);
      if (echo_page_return) break;
    }
  }

  if (m==m0) {
    /* Last page group was empty, display first or last page */
    int offset;
    if (s>0) {
      while (currentCGM->next) currentCGM= currentCGM->next;
      while (currentCGM->nPages==0 && currentCGM->prev)
	currentCGM= currentCGM->prev;
      offset= currentCGM->nPages-1;
    } else {
      while (currentCGM->prev) currentCGM= currentCGM->prev;
      while (currentCGM->nPages==0 && currentCGM->next)
	currentCGM= currentCGM->next;
      offset= 0;
    }
    if (currentCGM->nPages>0) {
      if (echo_page_return!=2) EchoPage(currentCGM->firstPage+offset, 1);
    } else {
      Warning("no pictures in CGM file(s) ", currentCGM->name);
    }
  }

  return 0;
}

int CGMRelative(int offset)
{
  return currentPage+offset;
}

void CGMinfo(void)
{
  if (currentCGM) {
    CGM *last= currentCGM;
    char msg[80];
    while (last->next) last= last->next;
    sprintf(msg, "At page %d out of %s%d in CGM file ",
	    currentPage, catalogCGM? ">=":"",
	    last->firstPage+last->nPages-1);
    fputs(msg, stdout);
    fputs(firstCGM->name, stdout);  /* name might be very long */
    fputs("\n", stdout);
  } else {
    fputs("(No CGM input file, use open command)\n", stdout);
  }
}

int CGM_pages(void)
{
  int num_pgs= 0;
  if (currentCGM) {
    CGM *last= currentCGM;
    while (last->next) last= last->next;
    num_pgs= last->firstPage+last->nPages-1;
  }
  return num_pgs;
}

/* ------------------------------------------------------------------------ */

static int EchoPage(int number, int inc)
{
  CGM *cgm= currentCGM;
  int class, id;
  long nOctets;
  int value= 0;

  if (!cgm) return 1;

  while (number<cgm->firstPage && cgm->prev) cgm= cgm->prev;
  while (number>=cgm->firstPage+cgm->nPages && cgm->next) cgm= cgm->next;

  if (number>=cgm->firstPage+cgm->nPages) {
    if (cgm->catalogEnd>0) {
      if (!amBatch) {
	fputs("gist: STANDBY -- still cataloging ", stdout);
	fputs(cgm->name, stdout);
	fputs("\n", stdout);
      }
      while (CatalogCGM()) {
	if (cgm->catalogEnd<0) cgm= cgm->next; /* cgm->next must be non-0! */
	if (number<cgm->firstPage+cgm->nPages) break;
      }
    }
    if (number>=cgm->firstPage+cgm->nPages) {
      if (inc<0) return 0;
      else if (inc==0) return 1;
      number= cgm->firstPage+cgm->nPages-1;
      while (cgm->nPages==0 && cgm->prev) cgm= cgm->prev;
      if (cgm->nPages==0) return 1;
      value= 2;  /* distinguish cases where we still draw something */
    }
  } else if (number<cgm->firstPage) {
    if (inc>0) return 0;
    else if (inc==0) return 1;
    number= cgm->firstPage;
    while (cgm->nPages==0 && cgm->next) cgm= cgm->next;
    if (cgm->nPages==0) return 1;
    value= 2;  /* distinguish cases where we still draw something */
  }

  if (cgm!=currentCGM) {
    if (cgm!=catalogCGM) {
      cgm->file= fopen(cgm->name, "rb");
      if (!cgm->file) {
	Warning("no longer able to open CGM ", cgm->name);
	return 1;
      }
    }
    if (currentCGM!=catalogCGM) {
      fclose(currentCGM->file);
      currentCGM->file= 0;
    }
    currentCGM= cgm;
  }

  currentPage= number;

  if (fseek(cgm->file, cgm->pageAddress[number-cgm->firstPage], SEEK_SET)) {
    Warning("fseek failed to reach specified page in ", cgm->name);
    return 1;
  }

  gistError[0]= '\0';

  for (;;) {
    nOctets= ReadElement(&class, &id);
    if (nOctets<0) break;
    if (class==0) {
      if (id==5) { DoClass0(id, nOctets); break; }
      if (id==0 || id==3 || id==4) DoClass0(id, nOctets);
      else {
	Warning("misplaced delimiter element in CGM ", cgm->name);
	break;
      }
    } else if (class==1) {
      Warning("ignoring misplaced class 1 element in CGM ", cgm->name);
    } else if (class==2) DoClass2(id, nOctets);
    else if (class==3) DoClass3(id, nOctets);
    else if (class==4) DoClass4(id, nOctets);
    else if (class==5) DoClass5(id, nOctets);
    else if (class==6) DoClass6(id, nOctets);
    else if (class==7) DoClass7(id, nOctets);
    else {
      Warning("ignoring element of unknown class in CGM ", cgm->name);
    }
  }
  GpFlush(0);

  if (gistError[0]) Warning("(WARNING) ", gistError);

  if (currentCGM==catalogCGM &&
      currentPage==catalogCGM->firstPage+catalogCGM->nPages-1) {
    catalogCGM->catalogEnd= ftell(catalogCGM->file);
  }

  return value;
}

/* ------------------------------------------------------------------------ */
/* Allow for architectures in which a short does not have the same
   size or order as presumed in the CGM binary encoding standard
   (a short presumed to be 2 bytes in big-endian order, i.e.- MSB first).  */

#ifndef NOT_CGM_ORDER
#define CGM_WORDS(octets, nwords) ((short *)octets)

#else
#define CGM_WORDS(octets, nwords) Deorder(octets, nwords)
static short *Deorder(Octet *octets, long nwords);
static short *Deorder(Octet *octets, long nwords)
{
  short *words= (short *)octets;    /* reordering is done in place */
  short mask= (short)(-1 ^ 0xffff); /* mask for sign extension */
  long n= nwords;
  words+= n;
  octets+= n<<1;
  while (n--) {
    octets-= 2;
    *(--words)= octets[0]<<8 | octets[1];
  }
  if (mask) {
    long i;
    for (i=0 ; i<nwords ; i++) if (words[i] & 0x8000) words[i]|= mask;
  }
  return words;
}
#endif

/* ------------------------------------------------------------------------ */

static Octet *paramList= 0;
static long maxParams= 0;
static long blockSize= 16384;  /* can hold any Gist-written data */

static char fontName[256];

static long ReadParameters(long nOctets, GpColor *colors, long maxColors)
{
  Octet *oldParams, cmd[2];
  long newSize, i;
  long n, nTot= 0;
  int more= nOctets & 0x8000;
  Octet *newParams= colors? colors : paramList;

  for (;;) {
    nOctets&= 0x7fff;
    n= nOctets;
    if (n&1) n++;

    if (!colors) {
      if (nTot+n > maxParams) {
	oldParams= paramList;
	newSize= (1+(nTot+n-1)/blockSize)*blockSize;
	paramList= (Octet *)malloc((sizeof(short)/2)*newSize);
	if (!paramList) {
	  Warning("memory manager failed in CGM read", "");
	  return -2L;
	}
	maxParams= newSize;
	for (i=0 ; i<nTot ; i++) paramList[i]= oldParams[i];
	if (oldParams) free(oldParams);
      }
      newParams= paramList;
    } else {
      if (nTot+n > maxColors) {
	SkipParameters(nOctets);
	return nTot;
      }
    }

    if (nOctets>0) {
      if (cmdString) {
	if (n>cmdLength) { n= cmdLength;  more= 0; }
	memcpy(paramList+nTot, currentCmd, n);
	currentCmd+= n;
	cmdLength-= n;
      } else {
	if (fread(newParams+nTot, sizeof(char), n,
		  currentCGM->file)!=n) {
	  Warning("fread failed on CGM file ", currentCGM->name);
	  return -1L;
	}
      }
      nTot+= nOctets;  /* yes, may be 1 less than n */
    }

    if (!more) break;

    if (cmdString) {
      if (2>cmdLength) break;
      cmd[0]= *currentCmd++;
      cmd[1]= *currentCmd++;
      cmdLength-= 2;
    } else {
      if (fread(cmd, sizeof(char), 2L, currentCGM->file)!=2L) {
	Warning("fread failed on CGM file ", currentCGM->name);
	return -1L;
      }
    }
    nOctets= cmd[0]<<8 | cmd[1];
    more= nOctets & 0x8000;
  }

  return nTot;
}

static int SkipParameters(long nOctets)
{
  Octet cmd[2];
  int more= nOctets & 0x8000;

  for (;;) {
    nOctets&= 0x7fff;
    if (nOctets&1) nOctets++;

    if (nOctets>0) {
      if (cmdString) {
	if (nOctets>cmdLength) { nOctets= cmdLength;  more= 0; }
	currentCmd+= nOctets;
	cmdLength-= nOctets;
      } else {
	if (fseek(currentCGM->file, nOctets, SEEK_CUR)) {
	  Warning("fseek failed on CGM file ", currentCGM->name);
	  return 1;
	}
      }
    }

    if (!more) break;

    if (cmdString) {
      if (2>cmdLength) break;
      cmd[0]= *currentCmd++;
      cmd[1]= *currentCmd++;
      cmdLength-= 2;
    } else {
      if (fread(cmd, sizeof(char), 2L, currentCGM->file)!=2L) {
	Warning("fread failed (partition) on CGM file ", currentCGM->name);
	return 1;
      }
    }
    nOctets= cmd[0]<<8 | cmd[1];
    more= nOctets & 0x8000;
  }

  return 0;
}

static long ReadElement(int *class, int *id)
{
  Octet cmd[2];
  unsigned int n;

  if (cmdString) {
    if (2>cmdLength) { *class= *id= 0;   cmdLength= 0;  return 0; }
    cmd[0]= *currentCmd++;
    cmd[1]= *currentCmd++;
    cmdLength-= 2;
  } else {
    if (fread(cmd, sizeof(char), 2L, currentCGM->file)!=2L) {
      Warning("fread failed (command) on CGM file ", currentCGM->name);
      return -1L;
    }
  }
  cgmCmdLen= 2;

  *class= cmd[0]>>4;
  *id= (cmd[0]<<3 | cmd[1]>>5) & 0x7f;
  n= cmd[1] & 0x1f;

  if (n==0x1f) {
    if (cmdString) {
      if (2>cmdLength) { *class= *id= 0;   cmdLength= 0;  return 0; }
      cmd[0]= *currentCmd++;
      cmd[1]= *currentCmd++;
      cmdLength-= 2;
    } else {
      if (fread(cmd, sizeof(char), 2L, currentCGM->file)!=2L) {
	Warning("fread failed (command) on CGM file ", currentCGM->name);
	return -1L;
      }
    }
    n= cmd[0]<<8 | cmd[1];
    cgmCmdLen= 4;
  }

  return (long)n;
}

static long DePascalify(char *text, const Octet *pasctxt, long nMax)
{
  long nTot= nMax>0? 1 : 0;
  long nChars= nMax>0? *pasctxt++ : 0;
  int more= 0;

  if (nMax>0) nMax--;

  if (nChars==0xff) {
    if (nMax<2) {
      text[0]= '\0';
      return nTot+nMax;  /* i.e.- original nMax */
    }
    more= pasctxt[0]&0x80;
    nChars= (pasctxt[0]&0x7f)<<8 | pasctxt[1];
    pasctxt+= 2;
    nTot+= 2;
    nMax-= 2;
  }

  if (nChars>nMax) {
    nChars= nMax;
    more= 0;
  }

  if (nChars>0) memcpy(text, pasctxt, nChars);
  text[nChars]= '\0';
  pasctxt+= nChars;
  nTot+= nChars;
  nMax-= nChars;

  while (more) {
    if (nMax<2) return nTot+nMax;
    more= pasctxt[0]&0x80;
    nChars= (pasctxt[0]&0x7f)<<8 | pasctxt[1];
    pasctxt+= 2;
    nTot+= 2;
    nMax-= 2;
    if (nChars>nMax) {
      nChars= nMax;
      more= 0;
    }
    pasctxt+= nChars;
    nTot+= nChars;
    nMax-= nChars;
  }

  return nTot;
}

static int Snarf16(Octet *param)
{
  int value= param[0]<<8 | param[1];
  return value;
}

static long SnarfInteger(Octet *param)
{
  long value= 0;
  int nBytes= nBytesInt;
  while (nBytes-- > 0) value= (value<<8) | *param++;
  return value;
}

static GpReal SnarfReal(Octet *param)
{
  long ipart= param[0]<<8 | param[1];
  long fpart= param[2]<<8 | param[3];
  GpReal value= (GpReal)fpart/65536.0;
  if (ipart & 0x8000) ipart= (-1L ^ 0xffff) | ipart;
  return value + (GpReal)ipart;
}

static char cgmText[256];
static int cgmTextLen= 0;
static GpReal cgmX0, cgmY0;

static void RememberPoint(GpReal x0, GpReal y0)
{
  cgmX0= x0;
  cgmY0= y0;
}

static char *SnarfMoreText(Octet *param)
{
  cgmTextLen+= DePascalify(cgmText+cgmTextLen, param, 256-cgmTextLen)-1;
  return cgmText;
}

static char *SnarfText(Octet *param)
{
  cgmTextLen= 0;
  return SnarfMoreText(param);
}

/* ARGSUSED */
static char *PathifyText(char *text, int path)
{
  if (path) {
    long len= strlen(cgmText);
    int i;
    if (path!=1 && len>128) {
      len= 128;
      cgmText[len]= '\0';
    }
    if (path!=3) {
      char tmp;
      for (i=0 ; i<len-1-i ; i++) {
	tmp= cgmText[i];
	cgmText[i]= cgmText[len-1-i];
	cgmText[len-1-i]= tmp;
      }
    }
    if (path!=1) {
      cgmText[2*len-1]= '\0';
      for (i=len-1 ; i>0 ; i--) {
	cgmText[2*i]= cgmText[i];
	cgmText[2*i-1]= '\n';
      }
    }
  }
  return cgmText;
}

static GpReal SnarfSA(Octet *param, int md, GpReal dflt)
{
  if (md==SCALED) {
    return SnarfReal(param);
  } else { /* md==ABSOLUTE */
    return ((GpReal)Snarf16(param))*cgmScale/dflt;
  }
}

static int SnarfColor(Octet *param, long nOctets)
{
  int color;
  if (!cselIllegible && nOctets==1) {
    /* always takes this branch if isGistCGM */
    color= param[0];
  } else if (cselIllegible==16 && nOctets==2) {
    color= param[1];  /* could use Snarf16, but top byte meaningless */
  } else {
    color= FG_COLOR;
  }
  if (color!=FG_COLOR) {
    if (isGistCGM) {
      if (color<10) color= -1-color;           /* standard colors */
      else { color-= 10; needPalette= 1; }     /* palette colors */
    } else {
      if (bg0fg1 && color==0) color= BG_COLOR;
      else if (bg0fg1 && color==1) color= FG_COLOR;
      else { if (bg0fg1) color-= 2; needPalette= 1; }
    }
  }
  return color;
}

static void ConvertPoints(long n, const short *vdc,
			  GpReal *ndcx, GpReal *ndcy)
{
  long i;
  for (i=0 ; i<n ; i++) {
    ndcx[i]= cgmScaleX*((GpReal)vdc[2*i])+cgmOffsetX;
    ndcy[i]= cgmScaleY*((GpReal)vdc[2*i+1])+cgmOffsetY;
  }
}

static GpReal *ndcX= 0, *ndcY= 0;

static int CheckScratch(long nPoints)
{
  if (nPoints > (ndcY-ndcX)) {
    GpReal *sav= ndcX;
    nPoints= (1+(nPoints-1)/1024)*1024;
    ndcX= (GpReal *)malloc(sizeof(GpReal)*2*nPoints);
    if (!ndcX) {
      ndcX= sav;
      return 1;
    }
    if (sav) free(sav);
    ndcY= ndcX+nPoints;
  }
  return nPoints<=0;   /* i.e.- success (0) if nPoints>0 */
}

static void SetVDC(int *vdc)
{
 /* Try to fit the VDC extent onto an 8.5 by 11 inch page
    as best we can... */
 long dx= (long)vdc[2]-(long)vdc[0];
 long dy= (long)vdc[3]-(long)vdc[1];
 int landscape= (dy>0?dy:-dy) < (dx>0?dx:-dx);
 long xOffset, yOffset;

 if (landscape!=cgmLandscape) {
   int i;
   cgmLandscape= landscape;
   for (i=0 ; i<8 ; i++)
     if (outEngines[i]) GpLandscape(outEngines[i], landscape);
 }

 if (pictDesc.scaling==ABSTRACT) {
   /* Gist produced files (at default scale) have 11->26400 and
      8.5->20400 exactly.  For either of these cases, we want to
      reproduce the default cgmScale factor of 25545.24 exactly.  */
   if (cgmScaleToFit) {
     long dv= (dx>0?dx:-dx)<(dy>0?dy:-dy)? dy : dx;
     cgmScale= (19200.0/(GpReal)dv) / 25545.24;
     xOffset= (long)((0.25*ONE_INCH)/cgmScale);
     yOffset= (long)((2.75*ONE_INCH)/cgmScale);
   } else if (cgmLandscape) { /* 11-by-8.5 paper */
     if (17*dx > 22*dy) {
       yOffset= (17*dx-22*dy)/44;
       xOffset= 0;
       cgmScale= (26400.0/(GpReal)dx) / 25545.24;
     } else {
       xOffset= (22*dy-17*dx)/34;
       yOffset= 0;
       cgmScale= (20400.0/(GpReal)dy) / 25545.24;
     }
   } else {                   /* 8.5-by-11 paper */
     if (17*dy > 22*dx) {
       xOffset= (17*dy-22*dx)/44;
       yOffset= 0;
       cgmScale= (26400.0/(GpReal)dy) / 25545.24;
     } else {
       yOffset= (22*dx-17*dy)/17;
       xOffset= 0;
       cgmScale= (20400.0/(GpReal)dx) / 25545.24;
     }
   }
 } else { /* pictDesc.scaling==METRIC */
   xOffset= yOffset= 0;
   cgmScale= pictDesc.metric*(ONE_INCH/25.4);
 }
 if (cgmScale<0.0) cgmScale= -cgmScale;

 cgmScaleX= dx>0? cgmScale : -cgmScale;
 cgmScaleY= dy>0? cgmScale : -cgmScale;
 cgmOffsetX= -cgmScaleX*(GpReal)(vdc[0]-xOffset);
 cgmOffsetY= -cgmScaleY*(GpReal)(vdc[1]-yOffset);
}

static GpTransform unitTrans= {{0.0, 2.0, 0.0, 2.0}, {0.0, 2.0, 0.0, 2.0}};

static void SetClip(int on)
{
  if (on) {
    GpReal xbox[2], ybox[2];
    ConvertPoints(2L, pictControls.clipBox, xbox, ybox);
    if (xbox[0]<xbox[1]) {
      gistT.viewport.xmin= gistT.window.xmin= xbox[0];
      gistT.viewport.xmax= gistT.window.xmax= xbox[1];
    } else {
      gistT.viewport.xmin= gistT.window.xmin= xbox[1];
      gistT.viewport.xmax= gistT.window.xmax= xbox[0];
    }    
    if (ybox[0]<ybox[1]) {
      gistT.viewport.ymin= gistT.window.ymin= ybox[0];
      gistT.viewport.ymax= gistT.window.ymax= ybox[1];
    } else {
      gistT.viewport.ymin= gistT.window.ymin= ybox[1];
      gistT.viewport.ymax= gistT.window.ymax= ybox[0];
    }
    GpSetTrans(&gistT);
    gistClip= 1;
  } else {
    GpSetTrans(&unitTrans);
    gistClip= 0;
  }
}

static void ResetPalette(int forPage)
{
  int i;

  if (forPage) {
    int offset= isGistCGM? 10 : (bg0fg1? 2 : 0);
    int refreshPalette;
    for (i=0 ; i<cgmNColors ; i++) cgmPalette[i]= defaultPalette[i];
    cgmNColors= defaultNColors;
    /* Reset color mode on all engines, and
       set palette on any new engines */
    for (i=0 ; i<8 ; i++) {
      if (outEngines[i]) {
	refreshPalette= (cgmNColors>offset &&
			 (alteredPalette ||
			  outEngines[i]->palette!=cgmPalette+offset ||
			  outEngines[i]->nColors!=cgmNColors-offset));
	if (outTypes[i]==2) {
	  setXPalette= refreshPalette;
	} else {
	  outEngines[i]->colorMode= 0;
	  if (refreshPalette) GpSetPalette(outEngines[i], cgmPalette+offset,
					   cgmNColors-offset);
	}
      }
    }
    alteredPalette= 0;
    needPalette= 0;

  } else {
    cgmPalette[0].red=
      cgmPalette[0].green= cgmPalette[0].blue= 255;     /* background */
    cgmPalette[1].red=
      cgmPalette[1].green= cgmPalette[1].blue= 0;       /* foreground */
    cgmPalette[2].red=
      cgmPalette[2].green= cgmPalette[2].blue= 0;       /* black */
    cgmPalette[3].red=
      cgmPalette[3].green= cgmPalette[3].blue= 255;     /* white */
    cgmPalette[4].red= 255;
    cgmPalette[4].green= cgmPalette[4].blue= 0;         /* red */
    cgmPalette[5].green= 255;
    cgmPalette[5].red= cgmPalette[5].blue= 0;           /* green */
    cgmPalette[6].blue= 255;
    cgmPalette[6].red= cgmPalette[6].green= 0;          /* blue */
    cgmPalette[7].red= 0;
    cgmPalette[7].green= cgmPalette[7].blue= 255;       /* cyan */
    cgmPalette[8].green= 0;
    cgmPalette[8].red= cgmPalette[8].blue= 255;         /* magenta */
    cgmPalette[9].blue= 0;
    cgmPalette[9].red= cgmPalette[9].green= 255;        /* yellow */
    GpPutGray(10, cgmPalette);
    for (i=0 ; i<240 ; i++)
      cgmPalette[i+10].gray= i + ((i+8)/15);            /* 240 grays */
    GpPutRGB(240, cgmPalette+10);
    cgmNColors= 250;
    for (i=0 ; i<cgmNColors ; i++) defaultPalette[i]= cgmPalette[i];
    defaultNColors= cgmNColors;
    cgmNColMax= 256;
    alteredPalette= 1;  /* only used in forPage branch above */
  }

  cgmColorChange= 0;
}

static int cgmSmoothing= 0;

/* ------------------------------------------------------------------------ */

static int DoClass0(int id, long nOctets)
{
  /* Delimiter elements, class 0 */
  switch (id) {
  case 0:   /* NO-OP */
    SkipParameters(nOctets);
    break;

  case 1:   /* BEGIN METAFILE */
    {
      nOctets= ReadParameters(nOctets, (void *)0, 0L);
      DePascalify(metafileName, paramList, nOctets);
      /* Reset default page specifications to CGM defaults */
      pageDesc= cgmDesc;
      pageControls= cgmControls;
      pageDefaults= cgmDefaults;
      isGistCGM= 0;
      cgmLandscape= 0;
      /* set default palette */
      ResetPalette(0);
      /* reset illegibility flags */
      nBytesInt= 2;
      vdcIllegible= realIllegible= indexIllegible=
        cvalIllegible= cndxIllegible= cselIllegible= 0;
    }
    break;

  case 2:   /* END METAFILE */
    SkipParameters(nOctets);
    break;

  case 3:   /* BEGIN PICTURE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    DePascalify(pictureName, paramList, nOctets);
    /* Reset default page specifications */
    pictDesc= pageDesc;
    pictControls= pageControls;
    gistA.l= pageDefaults.l;
    gistA.m= pageDefaults.m;
    gistA.f= pageDefaults.f;
    gistA.t= pageDefaults.t;
    nonGistA= pageDefaults.ng;
    cgmSmoothing= 0;
    ResetPalette(1);

    /* Clear any previous picture, or begin a new page */
    GpClear(0, CONDITIONALLY);
#ifdef NEED_PICTURE_HOOKS
    {
      extern void begin_picture_hook(void);
      begin_picture_hook();
    }
#endif
    break;

  case 4:   /* BEGIN PICTURE BODY */
    SkipParameters(nOctets);
    SetVDC(pictDesc.vdc);
    SetClip(pictControls.clipOn);
    break;

  case 5:   /* END PICTURE */
    SkipParameters(nOctets);
#ifdef NEED_PICTURE_HOOKS
    {
      extern void end_picture_hook(void);
      end_picture_hook();
    }
#endif
    break;

  default:
    SkipParameters(nOctets);
    Warning("non-standard class 0 CGM element", "");
    break;
  }

  return 0;
}

static int DoClass1(int id, long nOctets)
{
  /* Metafile descriptor elements, class 1 */
  switch (id) {
  case 1:   /* METAFILE VERSION */
    SkipParameters(nOctets);
    break;

  case 2:   /* METAFILE DESCRIPTION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    DePascalify(metafileDescription, paramList, nOctets);
    if (strncmp(metafileDescription, "Gist;  ", 7)==0) isGistCGM= 1;
    else isGistCGM= 0;
    if (!amBatch) {
      fputs(currentCGM->name, stdout);
      fputs(" metafile description:\n", stdout);
      fputs(metafileDescription + (isGistCGM? 7 : 0), stdout);
      fputs("\n", stdout);
    }
    break;

  case 3:   /* VDC TYPE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets!=2 || paramList[0]!=0 || paramList[1]!=0) {
      Warning("CGM illegible, does not use integer VDC space", "");
      vdcIllegible= 1;
      return 1;
    }
    break;

  case 4:   /* INTEGER PRECISION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets==nBytesInt) nBytesInt= SnarfInteger(paramList)>>3;
    else nBytesInt= -1;
    break;

  case 5:   /* REAL PRECISION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets!=3*nBytesInt || SnarfInteger(paramList)!=1 ||
	SnarfInteger(paramList+nBytesInt)!=16 ||
	SnarfInteger(paramList+2*nBytesInt)!=16) {
      Warning("CGM partly illegible, not 32 bit fixed point reals", "");
      realIllegible= 1;
      return 1;
    }
    break;

  case 6:   /* INDEX PRECISION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets!=nBytesInt || SnarfInteger(paramList)!=16) {
      Warning("CGM partly illegible, not 16 bit index precision", "");
      indexIllegible= 1;
      return 1;
    }
    break;

  case 7:   /* COLOR PRECISION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets!=nBytesInt ||
	((cvalIllegible= SnarfInteger(paramList))!=8 && cvalIllegible!=16)) {
      Warning("CGM partly illegible, not 8 or 16 bit color precision", "");
      cvalIllegible= 1;
      return 1;
    } else {
      /* 8 is the default value, 16 grudgingly supported to allow ATC GKS
         produced files */
      if (cvalIllegible==8) cvalIllegible= 0;
    }
    break;

  case 8:   /* COLOR INDEX PRECISION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    { int cip= 8;
      if (nOctets!=nBytesInt ||
	  (cip= SnarfInteger(paramList))!=8) {
	Warning("CGM partly illegible, not 8 bit color index precision", "");
	cndxIllegible= cselIllegible= (cip==16? 16 : 1);
	return 1;
      }
    }
    break;

  case 9:   /* MAXIMUM COLOR INDEX */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets==1 && !cndxIllegible) {
      pageDesc.colorMaxI= paramList[0];
      cgmNColMax= pageDesc.colorMaxI+1;
    }
    break;

  case 10:  /* COLOR VALUE EXTENT */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets==6 && !cvalIllegible) {
      pageDesc.colorMaxV[0]= paramList[0];
      pageDesc.colorMaxV[1]= paramList[1];
      pageDesc.colorMaxV[2]= paramList[2];
      pageDesc.colorMaxV[3]= paramList[3];
      pageDesc.colorMaxV[4]= paramList[4];
      pageDesc.colorMaxV[5]= paramList[5];
    } else if (nOctets==12 && cvalIllegible==16) {
      pageDesc.colorMaxV[0]= Snarf16(paramList);
      pageDesc.colorMaxV[1]= Snarf16(paramList+2);
      pageDesc.colorMaxV[2]= Snarf16(paramList+4);
      pageDesc.colorMaxV[3]= Snarf16(paramList+6);
      pageDesc.colorMaxV[4]= Snarf16(paramList+8);
      pageDesc.colorMaxV[5]= Snarf16(paramList+10);
    }
    break;

  case 11:  /* METAFILE ELEMENT LIST */
    SkipParameters(nOctets);
    break;

  case 12:  /* METAFILE DEFAULTS REPLACEMENT */
    {
      nOctets= ReadParameters(nOctets, (void *)0, 0L);
      if (nOctets>0) {
	int class, id;
	cmdString= (Octet *)malloc(sizeof(Octet)*nOctets);
	if (!cmdString) {
	  Warning("memory manager failed in DoClass1(DEFAULTS)", "");
	  return 1;
	}
	memcpy(cmdString, paramList, nOctets);
	currentCmd= cmdString;
	cmdLength= nOctets;
	while (cmdLength>0) {
	  nOctets= ReadElement(&class, &id);
	  if (nOctets>=0) {
	    switch (class) {
	    case 2:
	      pictDesc= pageDesc;
	      DoClass2(id, nOctets);
	      pageDesc= pictDesc;
	      break;
	    case 3:
	      pictControls= pageControls;
	      DoClass3(id, nOctets);
	      pageControls= pictControls;
	      break;
	    case 5:
	      gistA.l= pageDefaults.l;
	      gistA.m= pageDefaults.m;
	      gistA.f= pageDefaults.f;
	      gistA.t= pageDefaults.t;
	      nonGistA= pageDefaults.ng;
	      DoClass5(id, nOctets);
	      pageDefaults.l= gistA.l;
	      pageDefaults.m= gistA.m;
	      pageDefaults.f= gistA.f;
	      pageDefaults.t= gistA.t;
	      pageDefaults.ng= nonGistA;
	      break;
	    case 6:
	      DoClass6(id, nOctets);
	      break;
	    default:
	      /* Other commands are illegal, but whining is pointless */
	      break;
	    }
	  }
	}
	free(cmdString);
	cmdString= 0;
      }
    }
    break;

  case 13:  /* FONT LIST */
    {
      long nPart;
      Octet *pasctxt= paramList;
      int i, *oldFonts;
      nOctets= ReadParameters(nOctets, (void *)0, 0L);
      pageDesc.nfonts= 0;
      do {
	nPart= DePascalify(fontName, pasctxt, nOctets);
	pasctxt+= nPart;
	nOctets-= nPart;
	for (i=0 ; i<N_GIST_FONTS ; i++)
	  if (strcmp(fontName, gistFonts[i])==0) break;
	if (i>=N_GIST_FONTS) i= 0;
	if (pageDesc.nfonts>=pageDesc.maxFonts) {
	  oldFonts= pageDesc.fonts;
	  pageDesc.fonts= (int *)malloc(sizeof(int)*(pageDesc.nfonts+20));
	  if (!pageDesc.fonts) {
	    pageDesc.fonts= oldFonts;
	    Warning("memory manager failed in DoClass1(FONT LIST)", "");
	    return 2;
	  }
	  pageDesc.maxFonts= pageDesc.nfonts+20;
	  for (i=0 ; i<pageDesc.nfonts ; i++) pageDesc.fonts[i]= oldFonts[i];
	  if (oldFonts && oldFonts!=defaultNumbers) free(oldFonts);
	}
	pageDesc.fonts[pageDesc.nfonts++]= i;
      } while (nOctets>0);
    }
    break;

  case 14:  /* CHARACTER SET LIST */
    SkipParameters(nOctets);
    break;

  case 15:  /* CHARACTER CODING ANNOUNCER */
    SkipParameters(nOctets);
    break;

  default:
    SkipParameters(nOctets);
    Warning("non-standard class 1 CGM element", "");
    break;
  }

  return 0;
}

static int DoClass2(int id, long nOctets)
{
  /* Picture descriptor elements, class 2 */
  switch (id) {
  case 1:   /* SCALING MODE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets>2 && paramList[1]) pictDesc.scaling= METRIC;
    else pictDesc.scaling= ABSTRACT;
    if (!realIllegible && nOctets==6) {
      if (pictDesc.scaling==METRIC) pictDesc.metric= SnarfReal(paramList+2);
      else pictDesc.metric= DEFAULT_METRIC;
    } else if (pictDesc.scaling==METRIC) {
      pictDesc.metric= DEFAULT_METRIC;
    }
    break;

  case 2:   /* COLOR SELECTION MODE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets!=2 || paramList[1]) {
      Warning("CGM partly illegible, not indexed color mode", "");
      cselIllegible= 1;
      return 1;
    } else cselIllegible= cndxIllegible;
    break;

  case 3:   /* LINE WIDTH SPECIFICATION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets!=2 || paramList[1]) pictDesc.lwidth= SCALED;
    else pictDesc.lwidth= ABSOLUTE;
    break;

  case 4:   /* MARKER SIZE SPECIFICATION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets!=2 || paramList[1]) pictDesc.msize= SCALED;
    else pictDesc.msize= ABSOLUTE;
    break;

  case 5:   /* EDGE WIDTH SPECIFICATION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets!=2 || paramList[1]) pictDesc.ewidth= SCALED;
    else pictDesc.ewidth= ABSOLUTE;
    break;

  case 6:   /* VDC EXTENT */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets==8) {
      short *vdc= CGM_WORDS(paramList, 4L);
      pictDesc.vdc[0]= vdc[0];
      pictDesc.vdc[1]= vdc[1];
      pictDesc.vdc[2]= vdc[2];
      pictDesc.vdc[3]= vdc[3];
      SetVDC(pictDesc.vdc);
      if (pictControls.clipDefault) {
	pictControls.clipBox[0]= vdc[0];
	pictControls.clipBox[1]= vdc[1];
	pictControls.clipBox[2]= vdc[2];
	pictControls.clipBox[3]= vdc[3];
      }
    }
    break;

  case 7:   /* BACKGROUND COLOR */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    {
      int red, green, blue;
      if (!cvalIllegible && nOctets==3) {
	/* This is actually ignored... */
	red= paramList[0];
	green= paramList[1];
	blue= paramList[2];
	GpPutNTSC(1, &pictDesc.background);
      } else if (cvalIllegible==16 && nOctets==6) {
	/* This is actually ignored... */
	red= Snarf16(paramList)>>8;
	green= Snarf16(paramList+2)>>8;
	blue= Snarf16(paramList+4)>>8;
	GpPutNTSC(1, &pictDesc.background);
      } else {
	nOctets= 0;
	red= green= blue= 0;
      }
      if (nOctets) {
	if (pictDesc.background.red!=red ||
	    pictDesc.background.green!=green ||
	    pictDesc.background.blue!=blue) {
	  char msg[80];
	  /* One warning each time the bozo tries to change it... */
	  sprintf(msg,
		  "WARNING- BACKGROUND COLOR %02x%02x%02x not supported.\n",
		  pictDesc.background.red, pictDesc.background.green,
		  pictDesc.background.blue);
	  fputs(msg, stdout);
	  fputs("   You may want to set \"Gist*background:\" X resource.\n",
		stdout);
	}
	pictDesc.background.red= red;
	pictDesc.background.green= green;
	pictDesc.background.blue= blue;
      }
    }
    break;

  default:
    SkipParameters(nOctets);
    Warning("non-standard class 2 CGM element", "");
    break;
  }

  return 0;
}

static int DoClass3(int id, long nOctets)
{
  /* Control elements, class 3 */
  switch (id) {
  case 1:   /* VDC INTEGER PRECISION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets!=nBytesInt || SnarfInteger(paramList)!=16) {
      Warning("CGM illegible, not 16 bit integer VDC coordinates", "");
      vdcIllegible= 1;
      return 1;
    } else vdcIllegible= 0;
    break;

  case 2:   /* VDC REAL PRECISION */
    SkipParameters(nOctets);
    break;

  case 3:   /* AUXILLIARY COLOR */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!cselIllegible && nOctets==1) pictControls.auxColor= paramList[0];
    break;

  case 4:   /* TRANSPARENCY */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets==2) {
      pictControls.transparency= paramList[1];
      gistA.t.opaque= !pictControls.transparency;
    }
    break;

  case 5:   /* CLIP RECTANGLE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets==8) {
      short *clipBox= CGM_WORDS(paramList, 4L);
      pictControls.clipBox[0]= clipBox[0];
      pictControls.clipBox[1]= clipBox[1];
      pictControls.clipBox[2]= clipBox[2];
      pictControls.clipBox[3]= clipBox[3];
      if (pictControls.clipOn) SetClip(1);
      pictControls.clipDefault= 0;
    }
    break;

  case 6:   /* CLIP INDICATOR */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets==2) {
      int clipOn= Snarf16(paramList);
      if (clipOn!=pictControls.clipOn) {
	pictControls.clipOn= clipOn;
	if (clipOn) SetClip(1);
	else SetClip(0);
      }
    }
    break;

  default:
    SkipParameters(nOctets);
    Warning("non-standard class 3 CGM element", "");
    break;
  }

  return 0;
}

static int DoClass4(int id, long nOctets)
{
  /* Graphical primitive elements, class 4 */

  if (cgmColorChange) {
    /* If a palette has been specified for this page, dump it to all
       non-X engines.  */
    int i, offset= isGistCGM? 10 : (bg0fg1? 2 : 0);
    for (i=0 ; i<8 ; i++) {
      if (outEngines[i] && outTypes[i]!=2) {
	outEngines[i]->colorMode= 1;
	GpSetPalette(outEngines[i], cgmPalette+offset, cgmNColors-offset);
      }
    }
    alteredPalette= 1;
    cgmColorChange= 0;
    setXPalette= 1;
  }
  if (setXPalette && (needPalette || id==9)) {
    /* X engines change palette only when necessary -- that is, if a
       non-standard color has been referenced, or this is a cell array.  */
    int i, offset= isGistCGM? 10 : (bg0fg1? 2 : 0);
    for (i=0 ; i<8 ; i++) {
      if (outEngines[i] && outTypes[i]==2) {
	outEngines[i]->colorMode= alteredPalette;
	GpSetPalette(outEngines[i], cgmPalette+offset, cgmNColors-offset);
      }
    }
    setXPalette= 0;
  }

  switch (id) {
  case 1:   /* POLYLINE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets>=4) {
      long nPoints= nOctets>>2;
      short *vdc= CGM_WORDS(paramList, nPoints<<1);
      int (*Lines)(long, const GpReal *, const GpReal *);

      if (cgmSmoothing) {
	long i;
	gistA.dl.closed= 0;
	gistA.dl.smooth= cgmSmoothing;
	gistA.dl.marks= gistA.dl.rays= 0;
	cgmSmoothing= 0;
	nPoints= nPoints/3;
	for (i=0 ; i<nPoints ; i++) {
	  vdc[2*i]= vdc[6*i];
	  vdc[2*i+1]= vdc[6*i+1];
	}
	Lines= &GaLines;
      } else {
	Lines= &GpLines;
      }

      if (!CheckScratch(nPoints)) {
	ConvertPoints(nPoints, vdc, ndcX, ndcY);
	Lines(nPoints, ndcX, ndcY);
      }
    }
    break;

  case 2:   /* DISJOINT POLYLINE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets>=8) {
      long nPoints= (nOctets>>2)&(~1L);
      short *vdc= CGM_WORDS(paramList, nPoints<<1);

      if (!CheckScratch(nPoints)) {
	long i, nSegs= nPoints>>1;
	for (i=0 ; i<nSegs ; i++) {
	  ndcX[i]= cgmScaleX*(GpReal)vdc[4*i]+cgmOffsetX;
	  ndcY[i]= cgmScaleY*(GpReal)vdc[4*i+1]+cgmOffsetY;
	  ndcX[i+nSegs]= cgmScaleX*(GpReal)vdc[4*i+2]+cgmOffsetX;
	  ndcY[i+nSegs]= cgmScaleY*(GpReal)vdc[4*i+3]+cgmOffsetY;
	}
	GpDisjoint(nSegs, ndcX, ndcY, ndcX+nSegs, ndcY+nSegs);
      }
    }
    break;

  case 3:   /* POLYMARKER */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets>=4) {
      long nPoints= nOctets>>2;
      short *vdc= CGM_WORDS(paramList, nPoints<<1);
      if (!CheckScratch(nPoints)) {
	ConvertPoints(nPoints, vdc, ndcX, ndcY);
	GpMarkers(nPoints, ndcX, ndcY);
      }
    }
    break;

  case 4:   /* TEXT */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets>=7) {
      short ndc[2], final;
      GpReal x0, y0;
      char *text;
      ndc[0]= Snarf16(paramList);
      ndc[1]= Snarf16(paramList+2);
      ConvertPoints(1L, ndc, &x0, &y0);
      final= Snarf16(paramList+4);
      text= SnarfText(paramList+6);
      if (final) {
	GpText(x0, y0, PathifyText(text,nonGistA.path));
	cgmTextLen= 0;
      }
      else RememberPoint(x0, y0);
    }
    break;

  case 5:   /* RESTRICTED TEXT */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets>=11) {
      short ndc[2], final;
      GpReal x0, y0;
      char *text;
      /* ignore box width, box height parameters */
      ndc[0]= Snarf16(paramList+4);
      ndc[1]= Snarf16(paramList+6);
      ConvertPoints(1L, ndc, &x0, &y0);
      final= Snarf16(paramList+8);
      text= SnarfText(paramList+10);
      if (final) {
	GpText(x0, y0, PathifyText(text,nonGistA.path));
	cgmTextLen= 0;
      }
      else RememberPoint(x0, y0);
    }
    break;

  case 6:   /* APPEND TEXT */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets>=12) {
      short final;
      char *text;
      final= Snarf16(paramList+2);
      text= SnarfMoreText(paramList+2);
      if (final) GpText(cgmX0, cgmY0, PathifyText(text,nonGistA.path));
    }
    break;

  case 7:   /* POLYGON */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets>=4) {
      long nPoints= nOctets>>2;
      short *vdc= CGM_WORDS(paramList, nPoints<<1);
      if (!CheckScratch(nPoints)) {
	int type= gistA.e.type;
	if (!nonGistA.eVisible) gistA.e.type= L_NONE;
	ConvertPoints(nPoints, vdc, ndcX, ndcY);
	if (gistA.f.style!=F_EMPTY) {
	  GpFill(nPoints, ndcX, ndcY);
	} else if (gistA.e.type!=L_NONE) {
	  gistA.dl.closed= 1;
	  gistA.dl.smooth= 0;
	  gistA.dl.marks= gistA.dl.rays= 0;
	  GaLines(nPoints, ndcX, ndcY);
	  gistA.dl.closed= 0;
	}
	gistA.e.type= type;
      }
    }
    break;

  case 8:   /* POLYGON SET */
    SkipParameters(nOctets);
    break;

  case 9:   /* CELL ARRAY */
    if (!cselIllegible && !vdcIllegible &&
	(nOctets&0x7fff)>=14+3*nBytesInt && !(nBytesInt&1)) {
      long width, height;
      int colorPrec, mode;
      short pqr[6];

      nOctets-= ReadParameters(14L+3*nBytesInt, (void *)0, 0L);
      pqr[0]= Snarf16(paramList);
      pqr[1]= Snarf16(paramList+2);
      pqr[2]= Snarf16(paramList+4);
      pqr[3]= Snarf16(paramList+6);
      pqr[4]= Snarf16(paramList+8);
      pqr[5]= Snarf16(paramList+10);
      width= SnarfInteger(paramList+12);
      height= SnarfInteger(paramList+12+nBytesInt);
      colorPrec= SnarfInteger(paramList+12+2*nBytesInt);
      mode= Snarf16(paramList+12+3*nBytesInt);

      if (mode==1 && (colorPrec==0 || colorPrec==8) &&
	  pqr[4]==pqr[2] && pqr[5]==pqr[1]) {
	long nColumns= width&1? width+1 : width;
	long nCells= nColumns*height;
	GpReal pqx[2], pqy[2];
	GpColor *colors= (GpColor *)malloc(sizeof(GpColor)*nCells);
	if (colors) {
	  if (ReadParameters(nOctets, colors, nCells)==nCells) {
	    if (isGistCGM) {
	      long i;
	      for (i=0 ; i<nCells ; i++) colors[i]-= 10;
	    }  /* bg0fg1 ignored here */
	    ConvertPoints(2L, pqr, pqx, pqy);
	    GpCells(pqx[0], pqy[0], pqx[1], pqy[1], width, height,
		    nColumns, colors);
	  }
	  free(colors);
	} else {
	  SkipParameters(nOctets);
	  Warning("memory manager failed in DoClass4(CELL ARRAY)", "");
	}
      } else {
	SkipParameters(nOctets);
      }
    } else {
      SkipParameters(nOctets);
    }
    break;

  case 10:  /* GENERALIZED DRAWING PRIMITIVE */
    SkipParameters(nOctets);
    break;

  case 11:  /* RECTANGLE */
    SkipParameters(nOctets);
    break;

  case 12:  /* CIRCLE */
    SkipParameters(nOctets);
    break;

  case 13:  /* CIRCULAR ARC 3 POINT */
    SkipParameters(nOctets);
    break;

  case 14:  /* CIRCULAR ARC 3 POINT CLOSE */
    SkipParameters(nOctets);
    break;

  case 15:  /* CIRCULAR ARC CENTER */
    SkipParameters(nOctets);
    break;

  case 16:  /* CIRCULAR ARC CENTER CLOSE */
    SkipParameters(nOctets);
    break;

  case 17:  /* ELLIPSE */
    SkipParameters(nOctets);
    break;

  case 18:  /* ELLIPTICAL ARC */
    SkipParameters(nOctets);
    break;

  case 19:  /* ELLIPTICAL ARC CLOSE */
    SkipParameters(nOctets);
    break;

  default:
    SkipParameters(nOctets);
    Warning("non-standard class 4 CGM element", "");
    break;
  }

  return 0;
}

static int DoClass5(int id, long nOctets)
{
  /* Attribute elements, class 5 */
  switch (id) {
  case 1:   /* LINE BUNDLE INDEX */
    /* Treat line bundle index as identical to line type */

  case 2:   /* LINE TYPE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!indexIllegible && nOctets==2) gistA.l.type= Snarf16(paramList);
    if (gistA.l.type<=0 || gistA.l.type>5) {
      if (gistA.l.type<=0) gistA.l.type= 1-gistA.l.type;
      if (gistA.l.type>5) gistA.l.type= gistA.l.type%5 + 1;
    }
    break;

  case 3:   /* LINE WIDTH */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if ((pictDesc.lwidth==ABSOLUTE && !vdcIllegible && nOctets==2) ||
	(pictDesc.lwidth==SCALED && !realIllegible && nOctets==4))
      gistA.l.width= SnarfSA(paramList, pictDesc.lwidth,
			     DEFAULT_LINE_WIDTH);
    break;

  case 4:   /* LINE COLOR */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    gistA.l.color= SnarfColor(paramList, nOctets);
    break;

  case 5:   /* MARKER BUNDLE INDEX */
    /* Treat marker bundle index as identical to marker type */

  case 6:   /* MARKER TYPE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!indexIllegible && nOctets==2) gistA.m.type= Snarf16(paramList);
    if (gistA.m.type<=0 || gistA.m.type>5) {
      if (gistA.m.type<=0) gistA.m.type= 1-gistA.m.type;
      if (gistA.m.type>5) gistA.m.type= gistA.m.type%5 + 1;
    }
    break;

  case 7:   /* MARKER SIZE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if ((pictDesc.msize==ABSOLUTE && !vdcIllegible && nOctets==2) ||
	(pictDesc.msize==SCALED && !realIllegible && nOctets==4))
      gistA.m.size= SnarfSA(paramList, pictDesc.msize,
			    DEFAULT_MARKER_SIZE);
    break;

  case 8:   /* MARKER COLOR */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    gistA.m.color= SnarfColor(paramList, nOctets);
    break;

  case 9:   /* TEXT BUNDLE INDEX */
    /* Treat text bundle index as identical to text font index */

  case 10:  /* TEXT FONT INDEX */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!indexIllegible && nOctets==2) gistA.t.font= Snarf16(paramList);
    if (gistA.t.font==0) gistA.t.font= 1;
    gistA.t.font--;
    if (gistA.t.font<0 || gistA.t.font>=pageDesc.nfonts) {
      if (gistA.t.font<0) gistA.t.font= -gistA.t.font;
      if (gistA.t.font>=pageDesc.nfonts)
	gistA.t.font= gistA.t.font%pageDesc.nfonts;
    }
    gistA.t.font= pageDesc.fonts[gistA.t.font];
    break;

  case 11:  /* TEXT PRECISION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets==2) nonGistA.prec= Snarf16(paramList);
    break;

  case 12:  /* CHARACTER EXPANSION FACTOR */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!realIllegible && nOctets==4)
      nonGistA.expand= SnarfReal(paramList);
    break;

  case 13:  /* CHARACTER SPACING */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!realIllegible && nOctets==4)
      nonGistA.spacing= SnarfReal(paramList);
    break;

  case 14:  /* TEXT COLOR */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    gistA.t.color= SnarfColor(paramList, nOctets);
    break;

  case 15:  /* CHARACTER HEIGHT */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets==2) {
      /* Point size is somewhat greater than CGM "character height"--
	 use the same factor of 0.8 used by the Gist CGMEngine to assure
	 that, at least, Gist-produced text is rendered correctly.  */
      gistA.t.height= (1.0/0.8)*cgmScale*((GpReal)Snarf16(paramList));
    }
    break;

  case 16:  /* CHARACTER ORIENTATION */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets==8) {
      short vdc[2];
      vdc[0]= Snarf16(paramList);
      vdc[1]= Snarf16(paramList+2);
      ConvertPoints(1L, vdc, &nonGistA.upX, &nonGistA.upY);
      vdc[0]= Snarf16(paramList+4);
      vdc[1]= Snarf16(paramList+6);
      ConvertPoints(1L, vdc, &nonGistA.baseX, &nonGistA.baseY);
      if (nonGistA.baseX>nonGistA.baseY) {
	if (nonGistA.baseX>-nonGistA.baseY) gistA.t.orient= TX_RIGHT;
	else gistA.t.orient= TX_DOWN;
      } else {
	if (nonGistA.baseX>-nonGistA.baseY) gistA.t.orient= TX_UP;
	else gistA.t.orient= TX_LEFT;
      }
    }
    break;

  case 17:  /* TEXT PATH */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets==2) nonGistA.path= Snarf16(paramList);
    break;

  case 18:  /* TEXT ALIGNMENT */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets>4) {
      gistA.t.alignH= Snarf16(paramList);
      gistA.t.alignV= Snarf16(paramList+2);
      if (gistA.t.alignH<0 || gistA.t.alignH>3) gistA.t.alignH= 0;
      if (gistA.t.alignV<0 || gistA.t.alignV>5) gistA.t.alignV= 0;
    }
    break;

  case 19:  /* CHARACTER SET INDEX */
    SkipParameters(nOctets);
    break;

  case 20:  /* ALTERNATE CHARACTER SET INDEX */
    SkipParameters(nOctets);
    break;

  case 21:  /* FILL BUNDLE INDEX */
    /* Treat fill bundle index as identical to interior style */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!indexIllegible && nOctets==2) gistA.f.style= Snarf16(paramList);
    break;

  case 22:  /* INTERIOR STYLE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets==2) gistA.f.style= Snarf16(paramList);
    break;

  case 23:  /* FILL COLOR */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    gistA.f.color= SnarfColor(paramList, nOctets);
    break;

  case 24:  /* HATCH INDEX */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!indexIllegible && nOctets==2) nonGistA.hindex= Snarf16(paramList);
    break;

  case 25:  /* PATTERN INDEX */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!indexIllegible && nOctets==2) nonGistA.pindex= Snarf16(paramList);
    break;

  case 26:  /* EDGE BUNDLE INDEX */
    /* Treat edge bundle index as identical to edge type */

  case 27:  /* EDGE TYPE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!indexIllegible && nOctets==2) gistA.e.type= Snarf16(paramList);
    break;

  case 28:  /* EDGE WIDTH */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if ((pictDesc.ewidth==ABSOLUTE && !vdcIllegible && nOctets==2) ||
	(pictDesc.ewidth==SCALED && !realIllegible && nOctets==4))
      gistA.e.width= SnarfSA(paramList, pictDesc.ewidth, DEFAULT_LINE_WIDTH);
    break;

  case 29:  /* EDGE COLOR */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    gistA.e.color= SnarfColor(paramList, nOctets);
    break;

  case 30:  /* EDGE VISIBILITY */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (nOctets==2) nonGistA.eVisible= Snarf16(paramList);
    break;

  case 31:  /* FILL REFERENCE POINT */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets==4) {
      short refp[2];
      refp[0]= Snarf16(paramList);
      refp[1]= Snarf16(paramList+2);
      ConvertPoints(1L, refp, &gistA.f.refpX, &gistA.f.refpY);
    }
    break;

  case 32:  /* PATTERN TABLE */
    SkipParameters(nOctets);
    break;

  case 33:  /* PATTERN SIZE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if (!vdcIllegible && nOctets==8) {
      short hw[2];
      short hx= Snarf16(paramList);
      short hy= Snarf16(paramList+2);
      short wx= Snarf16(paramList+4);
      short wy= Snarf16(paramList+6);
      if (hx<0) hx= -hx;
      if (hy<0) hy= -hy;
      if (wx<0) wx= -wx;
      if (wy<0) wy= -wy;
      if (wx<hx) hw[0]= hx;
      else hw[0]= wx;
      if (hy<wy) hw[1]= wy;
      else hw[1]= hy;
      ConvertPoints(1L, hw, &gistA.f.sizeX, &gistA.f.sizeY);
    }
    break;

  case 34:  /* COLOR TABLE */
    nOctets= ReadParameters(nOctets, (void *)0, 0L);
    if ((!cndxIllegible || cndxIllegible==16) &&
	((!cvalIllegible && nOctets>=4) ||
	 (cvalIllegible==16 && nOctets>=7))) {
      int i, begin= (cndxIllegible? paramList[1] : paramList[0]);
      int i0= (cndxIllegible? 2 : 1);
      int nColors= cvalIllegible? (nOctets-i0)/6 : (nOctets-i0)/3;
      unsigned long mins[3], deltas[3];
      mins[0]= pictDesc.colorMaxV[0];
      mins[1]= pictDesc.colorMaxV[1];
      mins[2]= pictDesc.colorMaxV[2];
      deltas[0]= pictDesc.colorMaxV[3]-mins[0];
      deltas[1]= pictDesc.colorMaxV[4]-mins[1];
      deltas[2]= pictDesc.colorMaxV[5]-mins[2];
#undef COLOR_8
#define COLOR_8(i,n) (255L*((n)-mins[i]))/deltas[i]
#undef COLOR_16
#define COLOR_16(i,n) (255L*(((unsigned int)(n))-mins[i]))/deltas[i]
      if (begin+nColors>cgmNColMax) nColors= cgmNColMax-begin;
      for (i=0 ; i<nColors ; i++) {
	if (!cvalIllegible) {
	  cgmPalette[i+begin].red= COLOR_8(0,paramList[3*i+i0]);
	  cgmPalette[i+begin].green= COLOR_8(1,paramList[3*i+i0+1]);
	  cgmPalette[i+begin].blue= COLOR_8(2,paramList[3*i+i0+2]);
	} else {
	  cgmPalette[i+begin].red= COLOR_16(0,Snarf16(paramList+6*i+i0));
	  cgmPalette[i+begin].green= COLOR_16(1,Snarf16(paramList+6*i+i0+2));
	  cgmPalette[i+begin].blue= COLOR_16(2,Snarf16(paramList+6*i+i0+4));
	}
      }
      if (nColors>0) {
	int offset= isGistCGM? 10 : (bg0fg1? 2 : 0);
	GpPutNTSC(nColors, cgmPalette+begin);
	if (!cgmColorChange || cgmNColors<begin+nColors)
	  cgmNColors= begin+nColors;
	if (cgmNColors>offset) cgmColorChange= 1;
      }
    }
    break;

  case 35:  /* ASPECT SOURCE FLAGS */
    SkipParameters(nOctets);
    break;

  default:
    SkipParameters(nOctets);
    Warning("non-standard class 5 CGM element", "");
    break;
  }

  return 0;
}

static int DoClass6(int id, long nOctets)
{
  /* Escape elements, class 6 */
  switch (id) {
  case 1:   /* ESCAPE */
    SkipParameters(nOctets);
    break;

  default:
    SkipParameters(nOctets);
    Warning("non-standard class 6 CGM element", "");
    break;
  }

  return 0;
}

static int DoClass7(int id, long nOctets)
{
  /* External elements, class 7 */
  switch (id) {
  case 1:   /* MESSAGE */
    SkipParameters(nOctets);
    break;

  case 2:   /* APPLICATION DATA */
    if (isGistCGM) {
      nOctets= ReadParameters(nOctets, (void *)0, 0L);
      if (nOctets==3) cgmSmoothing= Snarf16(paramList);
    } else {
      SkipParameters(nOctets);
    }
    break;

  default:
    SkipParameters(nOctets);
    Warning("non-standard class 7 CGM element", "");
    break;
  }

  return 0;
}

/* ------------------------------------------------------------------------ */
