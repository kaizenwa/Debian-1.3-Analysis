/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */


#include <stdio.h>
#include <string.h>
#include <math.h>
#include "defs.h"
#include "globals.h"
#include "subs.h"
#include "halfhyp.h"


/* 
The source in this file handles some hypertex-specials.  It is ment
to be used for hyperlinks within one single dvi-file only. So it's
not realy hyper, say it's some kind of halfhyper. For writing large
documents with lots of refences it's still useful. But it is not
connected to any internet or www stuff:

*******  tmview will not become fully hyper ****************

The code is actually stolen from xhdvi, which is fully hyper in the 
above mentiond sence. The originally begining of the file
hypertex.c from xhdvi follows:
*/
               
/*
 * Hypertex modifications to DVI previewer for X.
 * This portion of xhdvi is completely in the public domain. The
 * author renounces any copyright claims. It may be freely used for
 * commercial or non-commercial purposes. The author makes no claims
 * or guarantees - use this at your own risk.
 * 
 * Arthur Smith, U. of Washington, 1994
 *
 * 5/1994       code written from scratch, probably inspired by (but
 *                incompatible with) the CERN WWW library.
 * 3/1995       CERN WWW library called to do document fetching.
 */

/*
 * Implementation of HyperTeX through \specials 
 * One option: allow HTML tags and recognize them 
 * applicationDoSpecial strips leading blanks, so first char should
 * be '<' if it's a tag 
 */


/*
The changes made by me, Thomas Moor, are just about simplifications, since
tmview reads only complete pages, and about removing multi-dvi-file
and www stuff.
Since tmview already manages an array of data belonging to dvi-pages,
some of the local varibles of hypertex.c became global. 
*/


/* #define DEBUGHYP */

#define BEGIN 0
#define END 1

/* Structure to remember where we have been: */
typedef struct {
  char *refname;  /* File name (or href?) Not used by tmview anyway */
  int pageno;     /* Which page anchor was on */
  int x,y;        /* Approximate location on page... */
  int which;      /* Number of anchor on this page */
  int type;       /* And the other properties of this anchor */
  char *name;
  char *href;
} Anchors;

pagelistelement* HTeXcpage;  /* current page */
int cur_anchor_on_page;      /* Keep track of current page as it's read in */
int HTeXh, HTeXv;            /* Where am I */

#define HTeX_AnchorSTEP 20   /* allocate allways mem for 20 anchors */

Anchors *HTeX_visited = NULL;
int nHTeX_visited = 0;
int maxHTeX_visited = 0;

#define HTeX_NSTACK 32
int HTeXAnest[HTeX_NSTACK]; /* Maximum number of nested anchors */
int HTeXAnestlevel;         /* Current nesting level */

int HTeXreflevel;           /* 0 if not currently inside an href anchor */
char* URLbase = NULL;       /* dummy in tmview. no ulrstuff used */


void htex_img (int, char*);   /* dummy */
void htex_base (int, char*);  /* dummy */




/*****************************************************************************/
/* init or kill the anchorlist, set the current page                         */
/*****************************************************************************/

void htex_init(void)
{
   int i;
#ifdef DEBUGHYP
   pfprot("(htex: init)");
#endif
   for(i=0;i<pageanz;i++) pagelist[i].anchorsdone=NOTDONE;
   freemem(&URLbase);
}    

void FreeHTeXAnchorList(HTeX_Anchor* HTeXAp, int nHTeX){
  int i;
  for (i=0; i < nHTeX; i++) {
    if (HTeXAp[i].type&HTeX_A_NAME) freemem(&(HTeXAp[i].name));
    if (HTeXAp[i].type&HTeX_A_HREF) freemem(&(HTeXAp[i].href));
  }
}

void htex_kill(void)
{
   int i;
#ifdef DEBUGHYP
   pfprot("(htex: kill)");
#endif
   for(i=0;i<pageanz;i++) 
    if(pagelist[i].anchorsdone!=NOTDONE) {
      FreeHTeXAnchorList(pagelist[i].anchorlist,pagelist[i].nanchors);
      freemem(&(pagelist[i].anchorlist));
    }
   freemem(&(URLbase));
}    



void htex_beginpage(pagelistelement *page) /* Starting a new page */
{
  HTeXcpage=page;
  if(HTeXcpage->anchorsdone==DONE) return;
#ifdef DEBUGHYP
   pfprot("(htex: beginpage %d",page->num);
#endif
  HTeXcpage->anchorsdone=DOING;
  HTeXcpage->anchorlist=NULL;
  HTeXcpage->nanchors =0;
  HTeXcpage->maxanchors=0;
  HTeXAnestlevel = 0;   /* Start with zero nesting level for a page */
  HTeXreflevel = 0;
  cur_anchor_on_page = -1;
#ifdef DEBUGHYP
   pfprot(")");
#endif
}

void htex_endpage(void) { /* This page has been completed */
  int i;
  HTeX_Anchor *HTeXAp;

  if(HTeXcpage->anchorsdone!=DOING) return;
#ifdef DEBUGHYP
  pfprot("(htex: endpage %d)",HTeXcpage->num);
#endif

  /* Finish off boxes for nested anchors not done on this page */
  while (HTeXAnestlevel > 0) {
    HTeXAnestlevel--;
    HTeXAp = HTeXcpage->anchorlist + HTeXAnest[HTeXAnestlevel];
    if (HTeXAp->llx > HTeXh) HTeXAp->llx = HTeXh;
    if (HTeXAp->urx < HTeXh) HTeXAp->urx = HTeXh;
    if (HTeXAp->lly < HTeXv) HTeXAp->lly = HTeXv;
    if (HTeXAp->ury > HTeXv) HTeXAp->ury = HTeXv;
  }
  if(HTeXcpage->maxanchors!=HTeXcpage->nanchors) {		
    reallocmem(&(HTeXcpage->anchorlist),
                          HTeXcpage->nanchors *sizeof(HTeX_Anchor));
    HTeXcpage->maxanchors=HTeXcpage->nanchors;
  }
  HTeXcpage->anchorsdone = DONE;
}

/* A character or something was written: record position for current anchor */
/* x,y are pixel positions on current page, nonshrunken !! */
void htex_recordbits(long x, long y, long w, long h) {
  HTeX_Anchor *HTeXAp;
  long dvix, dviy, dvix2, dviy2;

  if(HTeXcpage->anchorsdone!=DOING) return;
  if(HTeXAnestlevel==0) return; 

  dvix = x;
  dviy = y;
  dvix2 = x+w;
  dviy2 = y+h;
  HTeXAp = HTeXcpage->anchorlist + HTeXAnest[HTeXAnestlevel-1];
  if (HTeXAp->llx > dvix)  HTeXAp->llx = dvix;
  if (HTeXAp->lly < dviy2)  HTeXAp->lly = dviy2;
  if (HTeXAp->urx < dvix2) HTeXAp->urx = dvix2;
  if (HTeXAp->ury > dviy) HTeXAp->ury = dviy;
#ifdef DEBUGHYP
/*  pfprot("(htex: record: New box for anchor %d, level %d: %d %d %d %d)",
			HTeXAnest[HTeXAnestlevel-1], HTeXAnestlevel,
			HTeXAp->llx, HTeXAp->lly, HTeXAp->urx, HTeXAp->ury);*/
#endif
}


/****************************************************************************/
/* parsing the html specials                                                */
/****************************************************************************/


/* Parses cp containing 'ref="string"more', returning pointer to "more" */
char *refscan(char* name, char** ref, char** str){
  char *cp;

  *str = name;
  for (cp=name; *cp; cp++) {
    if (*cp == '=') {
      *cp = 0;
      *ref = name;
      *str = cp+1;
      break;
    }
  }
  cp = *str;
  if (cp != name) {
    while (isspace(*cp)) cp++;
    if (*cp == '"') { /* Yes, this really is a string being set */
      *str = cp+1;
      while ((cp = strchr(cp+1, '"')) != NULL) {
	if (cp[-1] != '\\') break; /* Check if quote escaped */
      }
      if (cp != NULL) {
	*cp = 0;
	cp++;
      }
    } else {
      cp = NULL;
    }
  } else {
    cp = NULL;
  }
  return cp;
}


/* Following parses the stuff after the '<' in the html tag */
/* Only understands name and href in anchor */
/*     html: <A HREF="..." NAME="..."> */
/*     html: <A NAME="..." HREF="...> */

void htex_parseanchor(char* cp, HTeX_Anchor *anchor) {
  char *ref, *str;
  anchor->type = 0;
  anchor->href = NULL;
  anchor->name = NULL;
  while (isspace(*cp)) cp++;
    while ((*cp) && (*cp != '>')) {
      cp = refscan(cp, &ref, &str);
      if (cp == NULL) break;
      if (STRCASECMP(ref, "href") == 0) {
        anchor->type |= HTeX_A_HREF;
	stralloccpy(&(anchor->href), str);
      } else if (STRCASECMP(ref, "name") == 0) {
	anchor->type |= HTeX_A_NAME;
	stralloccpy(&(anchor->name), str);
      }
   }
}


/* Basically just want to parse the line... */
/* Should use WWW library stuff ? */

void htex_anchor(int beginend, char* cp) {
  int i;
  int oldllx, oldlly, oldurx, oldury;
  HTeX_Anchor *HTeXAp, *HTeXAp2;

  if (beginend == END) {
    HTeXAnestlevel--;
    if (HTeXAnestlevel < 0) {
      HTeXAnestlevel = 0; /* Ignore Extra </a>'s? */
    } else {
      HTeXAp = HTeXcpage->anchorlist + HTeXAnest[HTeXAnestlevel];
      if (HTeXAp->llx > HTeXh) HTeXAp->llx = HTeXh; 
      if (HTeXAp->urx < HTeXh) HTeXAp->urx = HTeXh;
      if (HTeXAp->lly < HTeXv) HTeXAp->lly = HTeXv;
      if (HTeXAp->ury > HTeXv) HTeXAp->ury = HTeXv; 
      oldllx = HTeXAp->llx;
      oldlly = HTeXAp->lly;
      oldurx = HTeXAp->urx;
      oldury = HTeXAp->ury;
#ifdef DDEBUGHYP
      pfprot("(htex: Added anchor %d, level %d, )",
			    HTeXAnest[HTeXAnestlevel], HTeXAnestlevel);
      if (HTeXAp->type&HTeX_A_HREF) 
	   pfprot("href = %s ", HTeXAp->href);
      if (HTeXAp->type&HTeX_A_NAME) 
	   pfprot("name = %s ", HTeXAp->name);
      pfprot("box %d %d %d %d)",
        HTeXAp->llx, HTeXAp->lly, HTeXAp->urx, HTeXAp->ury); 
#endif
      if (HTeXAnestlevel > 0) {
	HTeXAp = HTeXcpage->anchorlist + HTeXAnest[HTeXAnestlevel-1];
	  /* Check llx, lly, urx, ury info */
	if (oldllx < HTeXAp->llx) HTeXAp->llx = oldllx;
        if (oldlly > HTeXAp->lly) HTeXAp->lly = oldlly;
        if (oldurx > HTeXAp->urx) HTeXAp->urx = oldurx;
	if (oldury < HTeXAp->ury) HTeXAp->ury = oldury;			
      }
    }
  } else {      /* its a BEGIN */
    if (HTeXcpage->nanchors==HTeXcpage->maxanchors) {
      HTeXcpage->maxanchors += HTeX_AnchorSTEP;
      reallocmem(&(HTeXcpage->anchorlist),
                 HTeXcpage->maxanchors *sizeof(HTeX_Anchor));
    }
    HTeXAp = HTeXcpage->anchorlist + HTeXcpage->nanchors;
    /* Set type, and the name, href */
    htex_parseanchor(cp, HTeXAp);
    if (HTeXAp->type != 0) {
      cur_anchor_on_page++; /* Increment the count of anchors here */
      HTeXAp->urx = HTeXAp->llx = HTeXh; /* Current horiz pos.*/
      HTeXAp->ury = HTeXAp->lly = HTeXv; /* Current vert. pos. */
      if (HTeXAnestlevel >= HTeX_NSTACK) {
	  pfprot("(warning: htex: too many nested anchors)");
      } else {
	 HTeXAnest[HTeXAnestlevel++] = HTeXcpage->nanchors;
      }
      HTeXcpage->nanchors++;
    }
  }   
  if (beginend == END) {
    if (HTeXreflevel > 0) HTeXreflevel--;
  } else {
    if (HTeXAp->type&HTeX_A_HREF) HTeXreflevel++;
  }
}

/* Arthurs's hypertex format: */
/* only a subset implemented in tmview */

void htex_handletag(char* cp)
{
   int beginend=BEGIN;

   if (HTeXcpage->anchorsdone!=DOING) return;
#ifdef DEBUGHYP
   pfprot("(htex: handletag <%s> ...)",cp);
#endif
   if (*cp != '<') return;
   ++cp;
   while (isspace(*cp)) cp++;
   if (*cp == '/') {
     beginend = END;
     cp++;
   }
   switch(*cp) {
   case 'A':
   case 'a': /* Anchors */
     htex_anchor(beginend, cp+1);
     break;
   case 'b': /* Base name? */
     htex_base(beginend, cp);
     break;
   case 'i': /* Included images? */
     htex_img(beginend, cp);
     break;
   default: /* Tag not implemented yet */
     break;
   }
}

/* Dave Oliver's hypertex format: */
/* Only understand my (=Arthur's) version of anchors so far */
/* ie.: his TYPE=text for hrefs, frag for names */

hy_handletag(char* cp) {
  int beginend=BEGIN;
  

   if (HTeXcpage->anchorsdone!=DOING) return;
#ifdef DEBUGHYP
   pfprot("(htex: handletag (OLIVER) <%s> ...)",cp);
#endif
  while (isspace(*cp)) cp++;
  if (!STRNCASECMP(cp, "END", 3)) {
    beginend = END;
    cp += 3;
  }
  /* Leave the parsing to htex_anchor! */
  htex_anchor(beginend, cp);
}

/* this is to be called from the dvi-reader when a special occures */
int checkndoHyperTeX(char *cp,int h, int v) {
  int htexfound = 0;

  if (HTeXcpage->anchorsdone!=DOING) return;
#ifdef DEBUGHYP
  pfprot("\n(htex: checkndo <%s> ...)\n",cp);
#endif
  HTeXh=h;
  HTeXv=v;
  if (STRNCASECMP(cp, "html:", 5) == 0) {
    cp += 5;
    while (isspace(*cp)) cp++;
    htexfound = 1;
    htex_handletag(cp);
  } else if(STRNCASECMP(cp, "hyp", 3) == 0) {
    /* Dave Oliver's HyperTeX */
    htexfound = 1;
    cp += 4;
    hy_handletag(cp);
  }
#ifdef DEBUGHYP
   if(!htexfound) 
     pfprot("(htex: checkndo <%s> ignored)",cp);
#endif
  return htexfound;
}


/****************************************************************************/
/* search for an anchor or test if whithin an anchor ... user-interaction   */
/****************************************************************************/


#ifdef DEBUGHYP
/* list all anchor on a page */
htex_listpage(pagelistelement* page) {
  int ia;
  HTeX_Anchor *HTeXAp;
  char astr[256];

  pfprot("\nanchors on page %d\n",page->num);
  if(page->anchorsdone != DONE) {
    pfprot("not done\n");
    return;
  }
  if(page->anchorlist==NULL || page->nanchors==0) {
    pfprot("empty\n");
    return;
  }
  HTeXAp = page->anchorlist;
  for (ia=0; ia < page->nanchors; ia++, HTeXAp++) {
    if ((HTeXAp->type&HTeX_A_NAME) == 0) continue;
    pfprot("(%s)",HTeXAp->name);
  }
  pfprot("\n");
}
#endif



/* find the anchor on a page */
int htex_findonpage(char* href, pagelistelement* page) {
  int ia;
  HTeX_Anchor *HTeXAp;
  char astr[256];
  char *cp;
	
  if (href == NULL) return(-1); /* shouldn't happen? */

#ifdef DEBUGHYP
  htex_listpage(page);
#endif
  cp = href;
  while (*cp == '#') cp++;
  if(page->anchorsdone != DONE) return(-1);
  HTeXAp = page->anchorlist;
  if(page->anchorlist==NULL) return(-1);
  for (ia=0; ia < page->nanchors; ia++, HTeXAp++) {
    if ((HTeXAp->type&HTeX_A_NAME) == 0) continue;
    if (!strcmp(HTeXAp->name, cp)) break;
  }
  if(ia==page->nanchors) return(-1);
  return(ia);
}


/* find the anchor on any page done */
void htex_findondonepage(char* href, int* ip, int* ia) {
  HTeX_Anchor *HTeXAp;

  *ip=-1;
  *ia=-1;
  if (href == NULL) return; /* shouldn't happen? */
  for (*ip = 0; *ip < pageanz; (*ip)++) { 
    *ia =htex_findonpage(href,pagelist+ *ip);
    if(*ia != -1) break;
  }
  if(*ia==-1) *ip=-1;
#ifdef DEBUGHYP
  pfprot("(htex: foundondonepage page %d anchor %d",*ip,*ia);
#endif
}
	

/* Is this a *href an url ? */
int htex_is_url(char *href){
	char *cp;
	int ret = 0;

	cp = href;
/* Be flexible on local files: */
	if (STRNCASECMP(href, "file:", 5) == 0) {
		ret = 1;
	} else if(!strncmp(cp,"news:",5)) {
		ret = 1;
	} else if(!strncmp(cp,"news\\:",6)) {
		ret = 1;
	} else if(!strncmp(cp,"mailto:",7)) {
        	ret = 1;
	} else if(!strncmp(cp,"mailto\\:",8)) {
		ret = 1;
	} else if(!strstr(cp+3,":/")) {  
		ret = 0;
	} else if(!strncmp(cp,"http",4)) {
		ret = 1;
	} else if(!strncmp(cp,"gopher",6)) {
		ret = 1;
	} else if(!strncmp(cp,"ftp",3)) {
		ret = 1;
	} else if(!strncmp(cp,"wais",4)) {
		ret = 1;
	} else if(!strncmp(cp,"telnet",6)) {
		ret = 1;
	} else if(!strncmp(cp,"tn3270",6)) {
		ret = 1;
	} else if(!strncmp(cp,"rlogin",6)) {
		ret = 1;
	} else if(!strncmp(cp,"afs",3)) {
		ret = 1;
	} else if(!strncmp(cp,"prospero",8)) {
		ret = 1;
	} else {
		ret = 0;
	}
	return ret;
}



/* static char resultstr[600]; */

/* What happens when mouse moves on screen: */
int htex_withinanchor(pagelistelement* page,int x, int y, char** res)
/* x,y coordinates of mouse position wrt dvi-origin, but in pxl*/
{
  /* char namestr[256];
     char hrefstr[256]; */
  HTeX_Anchor *HTeXAp;
  int i, afound;

  *res=NULL;
  if (page == NULL) return(0);
  if (page->anchorsdone != DONE) return(0);
/* Find anchor that fits current position: */
  afound=-1;
  HTeXAp = page->anchorlist + page->nanchors - 1;
  for (i=page->nanchors - 1; i >= 0; i--, HTeXAp--) {
    if (!(HTeXAp->type & HTeX_A_HREF)) continue; /* tmview: only hrefs */
    if (HTeXAp->x1pxl > x) continue;
    if (HTeXAp->y1pxl > y) continue;
    if (HTeXAp->x2pxl < x) continue;
    if (HTeXAp->y2pxl < y) continue;
#ifdef DEBUGHYP
    pfprot("(htexwithinanchor ? #%d ...)", i);
#endif
    afound=i;
  }
  if(afound==-1) { *res=NULL; return(0); }
  *res = page->anchorlist[afound].href;
  /************************ reply NAMEs too ? not now, tmview ******** ...
  namestr[0]=0;
  hrefstr[0]=0;       
  if (HTeXAp->type & HTeX_A_NAME)       
    sprintf(namestr, "NAME=%s", HTeXAp->name); 
  if (HTeXAp->type & HTeX_A_HREF) 
    sprintf(hrefstr, "HREF=%s", HTeXAp->href);
  sprintf(resultstr, "%s %s",namestr, hrefstr); 
  *res=resultstr;
  *********************************************************************/
#ifdef DEBUGHYP
  pfprot("%s found done within)", *res);
#endif
  return(1);
}


int htex_clickmouse(pagelistelement* page,int x, int y, char** res)
{
  HTeX_Anchor *HTeXAp;
  int i, afound;

  *res=NULL;
  if (page == NULL) return(0);
  if (page->anchorsdone != DONE) return(0);
/* Find anchor that fits current position: same as above ...        */
/*                                         ... but above may change */
  afound=-1;
  HTeXAp = page->anchorlist + page->nanchors - 1;
  for (i=page->nanchors - 1; i >= 0; i--, HTeXAp--) {
    if (!(HTeXAp->type & HTeX_A_HREF)) continue; 
    if (HTeXAp->x1pxl > x) continue;
    if (HTeXAp->y1pxl > y) continue;
    if (HTeXAp->x2pxl < x) continue;
    if (HTeXAp->y2pxl < y) continue;
#ifdef DEBUGHYP
    pfprot("(htex: click at anchor #%d ...", i);
#endif
    afound=i;
  }
  if(afound==-1) return(0);
  *res=page->anchorlist[afound].href;
  return(1);
}

int htex_findnearanchor(pagelistelement* page,int *x, int *y)
{
  HTeX_Anchor *HTeXAp;
  int i, afound;
  long dist,d;

  if (page == NULL) return(0);
  if (page->anchorsdone != DONE) return(0);
/* Find anchor that is near the current position */
#ifdef DEBUGHYP
    pfprot("(htex: search nearanchor at %d %d ...",*x,*y);
#endif
  afound=-1;
  dist=1<<30;
  HTeXAp = page->anchorlist + page->nanchors - 1;
  for (i=page->nanchors - 1; i >= 0; i--, HTeXAp--) {
    if (!(HTeXAp->type & HTeX_A_HREF)) continue; 
    d=  ((long)(HTeXAp->x1pxl - *x))*(HTeXAp->x1pxl - *x)
       +((long)(HTeXAp->y2pxl - *y))*(HTeXAp->y2pxl - *y);
    if(d>dist) continue;
    afound=i;
    dist=d;
  }
  if(afound==-1) return(0);
#ifdef DEBUGHYP
    pfprot(" ... no (near)anchor) ");
#endif
  HTeXAp = page->anchorlist + afound;
  for (i=afound; i < page->nanchors; i++, HTeXAp++) {
    if (!(HTeXAp->type & HTeX_A_HREF)) continue; 
    if (HTeXAp->x1pxl <= *x &&
        HTeXAp->y1pxl <= *y && 
        HTeXAp->x2pxl >= *x &&
        HTeXAp->y2pxl >= *y) continue;
    break;
  }
  if(i>= page->nanchors) {
    HTeXAp = page->anchorlist;
    for (i=0; i < page->nanchors; i++, HTeXAp++) {
      if (!(HTeXAp->type & HTeX_A_HREF)) continue; 
      break;
    }
  }
  if(i>= page->nanchors) return(0); /* cant happen here */
#ifdef DEBUGHYP
  pfprot("(htex: nearanchor #%d)", i);
#endif
  *x=HTeXAp->x1pxl;
  *y=HTeXAp->y2pxl;
  return(1);
}

int htex_findnumberanchor(pagelistelement* page,int* num){
  HTeX_Anchor *HTeXAp;
  int i;
	
  if (page == NULL) return(0);
  if (page->anchorsdone != DONE) return(0);
/* Find anchor by number */
#ifdef DEBUGHYP
    pfprot("(htex: search anchor %d)",*num);
#endif
  HTeXAp = page->anchorlist + *num + 1;
  for (i= *num+1; i < page->nanchors; i++, HTeXAp++) 
    if (HTeXAp->type & HTeX_A_HREF) break; 
  if(i>= page->nanchors) {
    HTeXAp = page->anchorlist;
    for (i=0; i < page->nanchors; i++, HTeXAp++) 
      if (HTeXAp->type & HTeX_A_HREF) break; 
    if(i>= page->nanchors) return(0);
  }
#ifdef DEBUGHYP
    pfprot("(htex: found anchor %d at %d %d)",i,HTeXAp->x1pxl,HTeXAp->y2pxl);
#endif
  *num=i;
  textx1pxl= HTeXAp->x1pxl;
  texty1pxl= HTeXAp->y1pxl;
  textx2pxl= HTeXAp->x2pxl;
  texty2pxl= HTeXAp->y2pxl;
  textx3pxl= HTeXAp->x1pxl;
  texty3pxl= HTeXAp->y1pxl;
  textx4pxl= HTeXAp->x1pxl;
  texty4pxl= HTeXAp->y1pxl;
  return(1);
}


/* calculate  boxes (in pixels) around the anchor positions */
void htex_scalebox(double sch, double scv)
{
  HTeX_Anchor *HTeXAp;
  int i;
  int x1, y1, x2, y2;
	
  HTeXAp = HTeXcpage->anchorlist;
  for (i=0; i < HTeXcpage->nanchors; i++, HTeXAp++) {
    if ((HTeXAp->type&HTeX_A_HREF) == 0) continue; /* Only box hrefs */
    x1 = LFLOOR(sch*HTeXAp->llx)-1; /* -1 to look better */
    y1 = LFLOOR(scv*HTeXAp->ury);  
    x2 = LCEIL(sch*HTeXAp->urx);
    y2 = LCEIL(scv*HTeXAp->lly)+1; /* +1 to look better */
    HTeXAp->x1pxl=x1;
    HTeXAp->x2pxl=x2;
    HTeXAp->y1pxl=y1;
    HTeXAp->y2pxl=y2;
  }
}



/****************************************************************************/
/*dummys for things tmview cant do ...                                      */
/****************************************************************************/

void htex_base(int beginend, char* cp) {
  char *ref, *str;
	
  if (beginend == END) return;
  if (!STRNCASECMP(cp, "base", 4)) {
    cp += 4;
    cp = refscan(cp, &ref, &str);
    if (cp == NULL) return;
    while (isspace(*ref)) ref++;
    while (isspace(*str)) str++;
    if (STRCASECMP(ref, "href") == 0) {
      cp = str + strlen(str) - 1; /* Fix end of anchor */
      while (isspace(*cp)) cp--;
      if (*cp == '>') cp --;
      while (isspace(*cp)) cp--;
      cp++;
      *cp = '\0'; 
      stralloccpy(&URLbase, str); /* Change base */
      pfprot("(htex: <base = %s> ignored)", URLbase);
    }
  }
}
  
void htex_img(int beginend, char* cp) {
  char *ref, *str;
  char fullpathname[1024];
  if (beginend == END) return;
  if (!STRNCASECMP(cp, "img", 3)) {
    cp += 3;
    cp = refscan(cp, &ref, &str);
    if (cp == NULL) return;
    while (isspace(*ref)) ref++;
    while (isspace(*str)) str++;
    if (STRCASECMP(ref, "src") == 0) {
      cp = str + strlen(str) - 1; /* Fix end of anchor */
      while (isspace(*cp)) cp--;
      if (*cp == '>') cp --;
      while (isspace(*cp)) cp--;
      cp++;
      *cp = '\0'; 
      strcpy(fullpathname, str);
      /*
       * make_absolute(fullpathname, URLbase, 1024);
       * if (invokeviewer(fullpathname) != 1) ... then there is an Error ...
      */ 
      pfprot("(htex: <img src=%s> ignored)", fullpathname);
    }	 
  }
}











