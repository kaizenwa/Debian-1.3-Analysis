/* This is part of tmview, a dvi previewer. (c) 1995 Thomas Moor         */
/*                                                                       */
/* This program may be used without any warranty. It may be modified and */
/* distributed without any restrictions.                                 */

#include <stdio.h>
#include <math.h>

#include "defs.h"

#ifdef KPATHSEA
#include <kpathsea/magstep.h>
#include <kpathsea/tex-glyph.h>
#endif

#include "globals.h"
#include "subs.h"
#include "readpk.h"

/* #define DEBUGPK */


FILE* pkfile;
unsigned pkactbyte; 
int pkbitpos, pkdynf, pkrepcount;


void pkopen(char* fname) {
  if((pkfile = fopen(fname, "rb"))==0) {
    pfprot("\n fatal error: pk-file disapeard. \n");
    exit(1);
  }
}


#define pkclose()  fclose(pkfile)
#define pkskip(delta) fseek(pkfile,delta,1)
#define pkposit(to) fseek(pkfile,to,0)
#define pktell()  ftell(pkfile)


#define pkn1() (uchar) fgetc(pkfile)


unsigned short pkn2(void)  {  
  short b;
  b = pkn1();  
  return (b * 256 + pkn1());
}

unsigned long pkn3(void)  {  
  long b;  
  short c;
  b = pkn1();  
  c = pkn1();
  return ((b * 256 + c) * 256 + pkn1());
}


unsigned long pkn4(void)  {  
  unsigned long b,c,d;  
  b = pkn1();  
  c = pkn1();
  d = pkn1();
  return (((b * 256 + c) * 256 +d)*256+ pkn1());
}


short pki1(void)  {  
  short b;
  b = pkn1();  
  if (b > 127)  b -= 256;  
  return b;
}

short pki2(void)  {  
  short b;
  b = pkn1();  
  if (b > 127)  b -= 256;
  return (b * 256 + pkn1());
}

long pki3(void)  {  
  long b;
  b = pki1();  return (b * 65536L + pkn2());
}

long pki4(void)  {  
  long b;
  b = pki2();  
  return (b * 65536L + pkn2());
}

int pknib(void)  {  
  unsigned Result;
  if (pkbitpos <0) {
    pkactbyte = pkn1();  
    pkbitpos=4;
  }
  Result= pkactbyte >> pkbitpos;  
  pkbitpos -= 4;
  /*fprintf(prot,"(pknib: n %d p %d)",Result & 0xf,pkbitpos);*/
  return (Result & 0x0f);
}


int pkpackednum(void){
  int  i,j;
  /*fprintf(prot,"pkpackednum? ");*/
  if ((i = pknib()) == 0) {
    do {
      j = pknib();
      ++i;
    } while (j == 0);
    while (i > 0) {
      j = (j << 4) | pknib();
      --i;
    }
    return (j - 15 + ((13 - pkdynf) << 4) + pkdynf);
  } else {
    if (i <= pkdynf) return i;
    if (i < 14) return (((i - pkdynf - 1) << 4) + pknib() + pkdynf + 1);
    if (i == 14) pkrepcount = pkpackednum();
    else pkrepcount = 1;
    return pkpackednum();
  }
}


setpkname(char* fn, float fm, int delta, char* result) {
  char buff[MAXPATHSTR];
  int i,j;

  /*fprintf(prot,"(setpkname: %s ",fontformat);*/
  i=0;
  *result=0;
  while(1) {
    /*fprintf(prot,"%s ",result);*/
    j=i;
    for(;(fontformat[i] != 0) && (fontformat[i] != '@');i++);
    strncat(result,fontformat+j,i-j);
    if(fontformat[i] == 0) break;
    i++;
    switch(fontformat[i]){
    case 'm': case 'M':
      sprintf(buff,"%ld",(long)(fm+0.55+delta));  
      break;
    case 'r': case 'R':
      sprintf(buff,"%ld",(long)(5*fm+0.55+delta)); 
      break;
    case 'n': case 'N':
      strcpy(buff,fn);
      break;
    case 'k': case 'K':
      strncpy(buff,fn,8);
      buff[8]=0;
      break;
    default:
      *buff=0;
    }
    strcat(result,buff);
    i++;
  }
  /*fprintf(prot,"%s)",ffname);*/
}


int pkfind(char* ffpath,char* fontname,float fontmag,char* fftuto)  {  
  char ffname[MAXPATHSTR];
  int delta, foundfile;
  FILE* fofile;
  
#ifdef KPATHSEA                            /* use kpathsea if wanted */
  kpse_glyph_file_type font_ret;
  char *name=NULL;
  unsigned dpi;
  
  dpi = kpse_magstep_fix ((unsigned) (fontmag +.5), xres, NULL);
  name = kpse_find_pk (fontname, dpi, &font_ret);
  foundfile = (name!=NULL);
  if(name){
    strcpy(fftuto, name);
    freemem(&name);         
    if (!(strcmp(fontname,font_ret.name)==0)) 
          pfprot(" - using %s at %d instead ", font_ret.name, font_ret.dpi);
    else if (!kpse_bitmap_tolerance ((double)font_ret.dpi, (double) dpi))
          pfprot(" - loading at %d ", font_ret.dpi);
    } 
         
#else                                             /* use my own subs (slow) */

  setpkname(fontname,fontmag,0,ffname);
  foundfile=msearch(ffpath,ffname,fftuto);

  if(foundfile==0) {  
    pfverb(" - resizing ");
    for(delta=(int)(-0.002*fontmag) -1; delta<=(int)(0.002*fontmag)+1;delta++){
       if(delta==0) delta=1;
       setpkname(fontname,fontmag,delta,ffname);
       foundfile=msearch(ffpath,ffname,fftuto);
       if(foundfile!=0) break;
     }
  }
#endif

  if(foundfile==0) { 
        pfprot(" - no PKfile found "); 
        return(0);
  }
  fofile=fopen(fftuto,"rb");
  if ((fgetc(fofile)!=0xf7)||(fgetc(fofile)!=0x59)){  
    pfprot(" - error in PKfile ");  
    fclose(fofile);
    return(0);
  }
  pfprot(" - found PKfile ");
  pfverb("%s ",fftuto);
  fclose(fofile);
  return(1);
} 


int pkdef(char* fftuto,int fdb,long chksum)  {  
  long a, l, goonaddr, fpwidth, dchksum, needmem;
  unsigned long w,h;  
  uchar b, bb;
  uchar terminate;  short i, j, c;  
  chdesc *achar;
  fontdesc *font;
  int mchpk;
  
#ifdef DEBUGPK
  pfprot("(pkdef: ");
#endif
  font=fontdatabase+fdb;
  allocmem(&(font->pkfile),strlen(fftuto)+1);
  strcpy(font->pkfile, fftuto);
  pkopen(fftuto);
#ifdef DEBUGPK
  pfprot(" %s - charakters: ",font->fonam);
#endif
  mchpk=-1;
  terminate = 0;
  do {  b = pkn1();
    if (b >= 240) {
    switch (b) {
    case 240:  case 241:  case 242:  case 243:    /*skip specials */
      l = 0;
      for (i = 240; i <= b; i++)  l = (l << 8) + pkn1();
      pkskip(l);  
      break;
    case 244:  pkskip(4L);  break;
    case 245:  terminate = 1;  break;
    case 246:  break;
    case 247:  
      b = pkn1();  i = pkn1();
      pkskip((long) i + 4);  dchksum = pki4();
      /* some people were too confused. since MakeTeXPk and so. */ 
      if (chksum != 0) if (dchksum != 0) if (chksum != dchksum)
      pfverb(" - wrong checksum "); 
      pkskip(8L);  break;
    case 248:  case 249:  case 250:  case 251:
    case 252:  case 253:  case 254:  case 255:  break;
    }
  } else { /* a char */ 
    bb = b & 7;
    /* fprintf(prot,"(def type %d)",bb); */
    switch (bb) {
    case 0:  case 1:  case 2:  case 3:
      l = (((long) bb) << 8) + pkn1();  c = pkn1();  break;
    case 4:  case 5:  case 6:
      l = (((long) (bb & 3)) << 16) + pkn2();  c = pkn1();  break;
    case 7:  l = pki4();  c = pki4();  break;
    }
    goonaddr=l+pktell();
    /* fprintf(prot,"(goon off %u)",l);fflush(prot); */	
    if(c>255) {
      pfprot(" -  ignoring character %d ",c);
    } else { /* code < 256 */
      achar = font->chv+c;
#ifdef DEBUGPK
      pfprot(" c %d at 0x%x ", c, achar); 
#endif
      achar->flag = b;
      b &= 0x7;
      switch (b) {
      case 0:  case 1:  case 2:  case 3:
        fpwidth = pkn3();  pkn1();
        w = pkn1(); h = pkn1();
        achar->hof = pki1();  achar->vof = pki1();  break;
      case 4:  case 5:  case 6:
        fpwidth = pkn3();  pkn2();
        w = pkn2(); h = pkn2();
        achar->hof = pki2();  achar->vof = pki2();  break;
      case 7:
        fpwidth = pki4();  pki4(); pki4();
        w = pkn4();  h = pkn4();
        achar->hof = pki4();  achar->vof = pki4();  break;
      }
      needmem=((long)h)*((long)ROUNDUP(w,BITS_PER_BMUNIT/GREYSCALE))
              *BYTES_PER_BMUNIT;
      if (w > 0x7fff || h > 0x7fff || needmem > MAXLINMEM){
        pfprot(
       " - ignoring character %d (w=%ud h=%ud mem=%ud) ",
        c, w,h,needmem); 
      } else { /* mem ok */
        achar->fty |= PKTYPE;         
        achar->fontdataptr = fdb;
        achar->ch = c;
        achar->bmp.w = w;  
        achar->bmp.h = h; 
        achar->bmp.type=BLACKNWHITE;
        achar->bmp.bits= NULL;
        achar->bmp2.bits= NULL;
        achar->tfw = scaled(fpwidth,font->foscf);
        achar->addr = pktell();
        /* fprintf(prot,"width %d hight %d hof %d vof %d \n",
         w,h,achar->hof,achar->vof); */
      } /* end not too large */
    } /*end code  <256 */
    pkposit(goonaddr);
    if (mchpk < c)  mchpk = c; 
  }
  } while (!terminate);
  pkclose(); 
#ifdef DEBUGPK
  pfprot(" ... pkdef)"); 
#endif
  if (mchpk < 0) {
    pfprot(" - PKfile in error ");
    freemem(&(font->pkfile));
    return(0);
  } else  pfverb(" - PKfile ok ");
  if (mchpk > font->mch)  font->mch=mchpk;
  return(1);
}


void pkloadchar(chdesc *thechar){
	int	 n;
        uchar    theflag;
        char	paint_switch;
        char*   pkfilen;
	int	i, j;
	int	row_bit_pos;
	BMUNIT	*cp;
	BMUNIT	word;
	int	word_weight, bytes_wide;
	int	rows_left, h_bit, count;

        theflag = thechar->flag;
	pkdynf = theflag  >> 4;
        pkfilen=(fontdatabase+thechar->fontdataptr)->pkfile;
#ifdef DEBUGPK
	pfprot("(pkloadchar: charpt 0x%x,fontdataptr %d char %d ...",
           thechar,thechar->fontdataptr,thechar->ch); 
#endif
        pkopen(pkfilen);
        pkposit(thechar->addr);
	paint_switch = ((theflag & 8) != 0);
#ifdef DEBUGPK
        pfprot("width %d hight %d hof %d vof %d ",
                 thechar->bmp.w,thechar->bmp.h,
                 thechar->hof,thechar->vof); 
#endif
	/*
	 * now read rest of character code
	 */
        
	alloc_bitmapbw(&thechar->bmp);
	cp = thechar->bmp.bits;

       
        bytes_wide = thechar->bmp.bmu_wide * BYTES_PER_BMUNIT;
	pkbitpos = -1;
	if (pkdynf == 14) {	/* get raster by bits */
	    /* fprintf(prot,"raster: "); */
            clear_bitmap(&thechar->bmp);
	    for (i = 0; i < thechar->bmp.h; i++) {	/* get all rows */
		cp = ADD(thechar->bmp.bits, i * bytes_wide);
		row_bit_pos = BITS_PER_BMUNIT;
		for (j = 0; j < thechar->bmp.w; j++) {  /* get one row */
		    if (--pkbitpos < 0) {
			word = pkn1();
			pkbitpos = 7;
		    }
		    if (--row_bit_pos < 0) {
			cp++;
			row_bit_pos = BITS_PER_BMUNIT - 1;
		    }
		    if (word & (1 << pkbitpos)) *cp |= 1 << row_bit_pos;
		}
	    }
	}
	else {		/* get packed raster */
	    /* fprintf(prot,"packed raster \n"); fflush(prot); */
            rows_left = thechar->bmp.h;
	    h_bit = thechar->bmp.w;
	    pkrepcount = 0;
	    word_weight = BITS_PER_BMUNIT;
	    word = 0;
	    while (rows_left > 0) {
	      /* fprintf(prot,"rl %d ",rows_left); fflush(prot); */
	      count = pkpackednum();
              /* fprintf(prot,"count %d \n",count); fflush(prot); */
              while (count > 0) {
                /* fprintf(prot,"ct %d hb %d ww %d \n",
                   count,h_bit,word_weight); fflush(prot); */
	      if (count < word_weight && count < h_bit) {
	      	h_bit -= count;
	       	word_weight -= count;
	       	if (paint_switch)
        	  word |= bit_masks[count] << word_weight;
		  count = 0;
	      }
	      else if (count >= h_bit && h_bit <= word_weight) {
		if (paint_switch)
		word |= bit_masks[h_bit] << (word_weight - h_bit);
		*cp++ = word;
		/* "output" row(s) */
		for (i = pkrepcount * bytes_wide /
                           BYTES_PER_BMUNIT; i > 0; --i) {
		  *cp = *SUB(cp, bytes_wide);
		   ++cp;
		}
		rows_left -= pkrepcount + 1;
		pkrepcount = 0;
		word = 0;
		word_weight = BITS_PER_BMUNIT;
		count -= h_bit;
		h_bit = thechar->bmp.w;
	      } else {
		if (paint_switch) word |= bit_masks[word_weight];
	 	  *cp++ = word;
		word = 0;
		count -= word_weight;
	        h_bit -= word_weight;
		word_weight = BITS_PER_BMUNIT;
	        }
	      }
	      paint_switch = 1 - paint_switch;
	    }
	    if (cp != (BMUNIT*)
             ((uchar*) thechar->bmp.bits + bytes_wide * thechar->bmp.h)){
       	      pfprot("Wrong number of bits stored:  char. %d, font %s",
                      thechar->ch,pkfilen);
              exit(1);
            }
	    if (rows_left != 0 || h_bit != thechar->bmp.w) {
	      pfprot("Bad PKfile (%s), too many bits", 
                       pkfilen);
              exit(1);
            }
	}
        pkclose();
        /* print_bitmap(&thechar->bmp); */
#ifdef DEBUGPK
	pfprot("... pkloadchar)");
#endif
}


/* pixel count stuff. sample is stolen from xdvi */

int bmu_wide;

#define SETBMUW(a) bmu_wide=(a)

int sample(BMUNIT* bits,int bitskip,int w, int h) {
  BMUNIT *p, *endp;
  register BMUNIT *cp;
  int bitsleft;
  register int n,bitshift,wid;
  
  p=bits+(bitskip>>BITS_LOG2);
  endp=bits+h*bmu_wide;
  bitsleft=w;
  bitshift=BITS_PER_BMUNIT- (bitskip & (BITS_PER_BMUNIT-1));
  n=0;
  while(bitsleft) {
    wid=bitshift;
    if(wid>bitsleft) wid=bitsleft;
    if(wid>8) wid=8;
    bitshift-=wid;
    for(cp=p;cp<endp;cp+=bmu_wide) 
      n+=bit_count[(*cp >> bitshift) & bit_masks[wid]];
    if(bitshift==0) {
      bitshift=BITS_PER_BMUNIT;
      ++p;
    }
    bitsleft -= wid;
  }
  return n;
}

int samplerow(BMUNIT* bits,int bitskip,int w) {
  BMUNIT *p;
  int bitsleft;
  register int n,bitshift,wid;
  
  p=bits+(bitskip >> BITS_LOG2);
  bitsleft=w;
  bitshift=BITS_PER_BMUNIT- (bitskip & (BITS_PER_BMUNIT-1));
  n=0;
  while(bitsleft) {
    wid=bitshift;
    if(wid>bitsleft) wid=bitsleft;
    if(wid>8) wid=8;
    bitshift-=wid; 
    n+=bit_count[(*p >> bitshift) & bit_masks[wid]];
    if(bitshift==0) {
      bitshift=BITS_PER_BMUNIT;
      ++p;
    }
    bitsleft -= wid;
  }
  return n;
}

int samplecol(BMUNIT* bits,int bitskip,int h) {
  BMUNIT *endp, mask;
  register BMUNIT *cp;
  register int n;
  
  mask=((BMUNIT)1 << (BITS_PER_BMUNIT-1)) >> (bitskip & (BITS_PER_BMUNIT -1)); 
  endp=bits+h*bmu_wide;
  
  n=0;
  for(cp=bits+(bitskip >> BITS_LOG2);cp<endp;cp+=bmu_wide) 
     if(*cp & mask) ++n;
  return n;
}
  
#define TESTBIT(bits,x) ((*(bits+((x)>>BITS_LOG2)) & ((BMUNIT) 1 << (BITS_PER_BMUNIT-1)) >> ((x) & (BITS_PER_BMUNIT -1))) ? 1 : 0) 

/* 
now there comes some bitmap shrinking stuff 
the first two are for integer shrinking factors only, and are
stolen from xdvi. the second two work with float factors alwell
as with faktors less than 1. but they are not too fast.
*/

#if 0
void pkshrinkcharbw(chdesc* theChar) {
  int rowsleft, rows, colsleft, initcols;
  register int cols;
  int shrunkbuw,shrunkh;
  int ishrink;
  BMUNIT  *oldp, *newp;
  register BMUNIT m,*cp;
  int blevel;

  /* fprintf(prot,"(pkshrinkgbw)\n"); */

  freelrumem(&(theChar->bmp2.bits));
  touchlrumem(&(theChar->bmp.bits));
  ishrink=ROUND(fshrink);
  blevel=ishrink*ishrink/4;

  theChar->hof2=theChar->hof / ishrink;
  initcols=theChar->hof-theChar->hof2*ishrink;
  if(initcols <=0) initcols+=ishrink; else ++theChar->hof2;
  theChar->bmp2.w = theChar->hof2 + 
    ROUNDUP(theChar->bmp.w - theChar->hof,ishrink);
  cols=theChar->vof +1;      /* buffer cols */
  theChar->vof2 = cols / ishrink;
  rows=cols-theChar->vof2*ishrink;
  if(rows <= 0) {
    rows+=ishrink;
    --theChar->vof2;
  }
  theChar->bmp2.h=shrunkh=theChar->vof2 +
    ROUNDUP(theChar->bmp.h-cols,ishrink) +1;
  theChar->bmp2.type=BLACKNWHITE;
  alloc_bitmapbw(&theChar->bmp2);
  oldp=theChar->bmp.bits;
  if(oldp==NULL) {
    pfprot("\nfatal error: my ugly memory management. sorry\n");
    exit(1);
  }
  newp=theChar->bmp2.bits;
  shrunkbuw=theChar->bmp2.bmu_wide;
  rowsleft=theChar->bmp.h;
  memset(newp,0,BYTES_PER_BMUNIT*shrunkbuw*shrunkh);
  while(rowsleft) {
    if(rows>rowsleft) rows=rowsleft;
    colsleft=theChar->bmp.w;
    m=(1 << (BITS_PER_BMUNIT -1));
    cp=newp;
    cols=initcols;
    while(colsleft){
      if(cols>colsleft) cols=colsleft;
      if(sample(oldp,theChar->bmp.bmu_wide,theChar->bmp.w-colsleft,
                cols,rows)>=blevel) *cp|=m;
      if(m==1) {
        m=(1 << (BITS_PER_BMUNIT -1));
        ++cp;
      } else m >>=1;
      colsleft-=cols;
      cols=ishrink;
    }
    newp+=shrunkbuw;
    oldp+=rows*theChar->bmp.bmu_wide;
    rowsleft-=rows;
    rows=ishrink;
  }
  theChar->vof2=theChar->vof/ishrink;
  theChar->shrink=fshrink;
  /*print_bitmap(&(theChar->bmp)); */
  /*print_bitmap(&(theChar->bmp2));*/
}
#endif


void pkshrinkchargs(chdesc* theChar) {
  int rowsleft, rows, colsleft, initcols;
  register int cols;
  int shrunkbuw,shrunkh, ishrink;
  BMUNIT  *oldp, *newp;
  register BMUNIT m,*cp;

#ifdef DEBUGPK
	pfprot("(pkshrinkchargs: fontdatptr %d char %d ...",
           theChar->fontdataptr,theChar->ch); 
#endif

  freelrumem(&(theChar->bmp2.bits));
  touchlrumem(&(theChar->bmp.bits));
  ishrink=ROUND(fshrink);

  /* fprintf(prot,"fshr %f ishr %d\n",fshrink,ishrink); */

  theChar->hof2=theChar->hof / ishrink;
  initcols=theChar->hof-theChar->hof2*ishrink;
  if(initcols <=0) initcols+=ishrink; else ++theChar->hof2;
  theChar->bmp2.w = theChar->hof2 + 
    ROUNDUP(theChar->bmp.w - theChar->hof,ishrink);
  cols=theChar->vof +1;      /* buffer cols */
  theChar->vof2 = cols / ishrink;
  rows=cols-theChar->vof2*ishrink;
  if(rows <= 0) {
    rows+=ishrink;
    --theChar->vof2;
  }
  theChar->bmp2.h=shrunkh=theChar->vof2 +
    ROUNDUP(theChar->bmp.h-cols,ishrink) +1;
  theChar->bmp2.type=GREYSCALE;
  alloc_bitmapgs(&theChar->bmp2);
  oldp=theChar->bmp.bits;
  if(oldp==NULL) {
    pfprot("\nfatal error: my ugly memory management. sorry\n");
    exit(1);
  }
  newp=theChar->bmp2.bits;
  shrunkbuw=theChar->bmp2.bmu_wide;
  SETBMUW(theChar->bmp.bmu_wide);
  rowsleft=theChar->bmp.h;
  memset(newp,0,BYTES_PER_BMUNIT*shrunkbuw*shrunkh);
#ifndef GREYINBMU  
  while(rowsleft) {
    if(rows>rowsleft) rows=rowsleft;
    colsleft=theChar->bmp.w;
    m= BITS_PER_BMUNIT -GREYSCALE;
    cp=newp;
    cols=initcols;
    while(colsleft){
      if(cols>colsleft) cols=colsleft;
      *cp |= 
         greytab[(sample(oldp,theChar->bmp.w-colsleft, cols,rows))]  << m; 
      if(m) m-=GREYSCALE;
      else {
        m=BITS_PER_BMUNIT -colors;
        ++cp;
      };
      colsleft-=cols;
      cols=ishrink;
    }
    newp+=shrunkbuw;
    oldp+=rows*theChar->bmp.bmu_wide;
    rowsleft-=rows;
    rows=ishrink;
  }
#else     /* that is ifdef GREYINBMU */
  while(rowsleft) {
    if(rows>rowsleft) rows=rowsleft;
    colsleft=theChar->bmp.w;
    cp=newp;
    cols=initcols;
    while(colsleft){
      if(cols>colsleft) cols=colsleft;
      *cp |=  
           greytab[(sample(oldp,theChar->bmp.w-colsleft, cols,rows))]; 
      cp++;
      colsleft-=cols;
      cols=ishrink;
    }
    newp+=shrunkbuw;
    oldp+=rows*theChar->bmp.bmu_wide;
    rowsleft-=rows;
    rows=ishrink;
  }
#endif
  theChar->vof2=theChar->vof/ishrink;
  theChar->shrink=fshrink;
  /*print_bitmap(&(theChar->bmp)); */ 
  /*print_bitmap(&(theChar->bmp2)); */
#ifdef DEBUGPK
	pfprot("... pkshrinkchargs)"); 
#endif
}



void pkshrinkcharazgs(chdesc* theChar) {
  
  int destbmuw, srcbmuw, j,i;
  int rows,cols,firstcols,firstrows,srcrow,srccol,sam;
  int mrows,mcols,ishrink;
  BMUNIT m;
  BMUNIT *cp,*destptr,*srcptr;


#ifdef DEBUGPK
   pfprot("(pkshrinkazgs: fontdataptr %d char %d ...",
           theChar->fontdataptr,theChar->ch); 
#endif
 
  /* fshrink >=1 expected !! */

  if(fabs(fshrink-ROUND(fshrink))<.01) 
    {pkshrinkchargs(theChar); return;}

  freelrumem(&(theChar->bmp2.bits));
  touchlrumem(&(theChar->bmp.bits));
  /* ishrink=CEIL(fshrink); fixed size commented out. see below */
  theChar->shrink=fshrink; 
 
  
  theChar->hof2=    CEIL(theChar->hof / fshrink);
  firstcols=theChar->hof-ROUND((theChar->hof2-1)*fshrink);
  if(firstcols == 0) {
     firstcols=MAX(ROUND(fshrink),1);
     --theChar->hof2;
     }
  if(firstcols>=theChar->bmp.w) firstcols=theChar->bmp.w;
  
  theChar->vof2=    CEIL((theChar->vof+1) / fshrink);
  firstrows=theChar->vof+1-ROUND((theChar->vof2-1)*fshrink);
  if(firstrows == 0) {
     firstrows=MAX(ROUND(fshrink),1);
     --theChar->vof2;
  }
  --theChar->vof2;
  if(firstrows>=theChar->bmp.h) firstrows=theChar->bmp.h;
  
    
  theChar->bmp2.w = CEIL(theChar->bmp.w / fshrink) +2;                   
  theChar->bmp2.h = CEIL(theChar->bmp.h / fshrink) +2;
    
  if(firstrows<=0 || firstcols<=0)   
     pfprot("IMPORTANT DEBUGINFORMATION 
break with fro %d fco %d
fshr %f ishr %d hof %d vof %d hof2 %d vof2 %d\n",firstrows,firstcols,
       fshrink, ishrink,theChar->hof,theChar->vof,theChar->hof2,theChar->vof2);
  
       
  theChar->bmp2.type=GREYSCALE;
  alloc_bitmapgs(&theChar->bmp2);
  destptr=theChar->bmp2.bits;
  destbmuw=theChar->bmp2.bmu_wide;
  memset(destptr,0,BYTES_PER_BMUNIT*destbmuw*theChar->bmp2.h);
  
  srcptr=theChar->bmp.bits;
  srcbmuw=theChar->bmp.bmu_wide;
  SETBMUW(srcbmuw);

  if(srcptr==NULL) {
    pfprot("\nfatal error: my ugly memory management. sorry\n");
    exit(1);
  }

  /* pfprot("srcw %d srch %d destw %d desth %d \n", 
    theChar->bmp.w,theChar->bmp.h,theChar->bmp2.w,theChar->bmp2.h); */ 
  
        
/* #ifndef GREYINBMU  */
  rows=firstrows;
  srcrow=0;
  i=theChar->vof2+1; 
  mrows=ROUND(i*fshrink)-ROUND((i-1)*fshrink);
  while(rows>0) {

    m= BITS_PER_BMUNIT -GREYSCALE;
    cp=destptr;
    srccol=0;
    cols=firstcols;

    /*pfprot("col:: i %d srcrow %d rows %d mrows %d \n",i,srcrow,rows,mrows);*/

    j= -theChar->hof2; 
    mcols=ROUND((j+1)*fshrink)-ROUND(j*fshrink);
    while(cols>0) {
      sam=sample(srcptr, srccol, cols,rows);

      /* pfprot("  j %d srccol %d cols %d mcols %d sam %d\n",
              j,srccol,cols,mcols,sam);*/  

      if(sam!=0)                           
        *cp |= (BMUNIT)((COLORS_PER_GREY-1)*sam/(float)(mcols*mrows)) << m;    
      /* *cp |= (BMUNIT)(greytab[sam]) << m; */ 
      if(m) m-=GREYSCALE;
      else {
        m=BITS_PER_BMUNIT - GREYSCALE;
        ++cp;
      };
      j++;
      /*srccol=ROUND(j*fshrink)+theChar->hof; 
      cols=MIN(ishrink,theChar->bmp.w-srccol); */

      srccol+=cols; 
      mcols=ROUND((j+1)*fshrink)-ROUND(j*fshrink);
      cols=MIN(mcols,theChar->bmp.w-srccol); 
    }
    /* pfprot("did row\n");*/
    i--; 
    destptr+=destbmuw; 
    /* srcrow= (theChar->vof+1)-ROUND(i*fshrink);
    rows=MIN(ishrink,theChar->bmp.h-srcrow); */
    
    srcrow+=rows;
    mrows=ROUND(i*fshrink)-ROUND((i-1)*fshrink);
    rows=MIN(mrows,theChar->bmp.h-srcrow); 
   
    srcptr=theChar->bmp.bits+srcrow*srcbmuw;
  }
/* #else     that is ifdef GREYINBMU */
/* #endif */
  /* print_bitmap(&(theChar->bmp)); 
  print_bitmap(&(theChar->bmp2));*/
#ifdef DEBUGPK
  pfprot(" ...pkshrinkazgs)");
#endif
}


void pkshrinkcharazprecgs(chdesc* theChar) {
  
  int destbmuw, srcbmuw, j,i;
  int ox,oy,ix,iy,ow,oh,iw,ih;
  float fi,fj,frows,fcols,fsam,topweight,botweight,leftweight,rightweight;
  BMUNIT m;
  BMUNIT *cp,*destptr,*srcptroy,*srcptroyb,*srcptriy,*srcptriyb;


#ifdef DEBUGPK
   pfprot("(pkshrinkprecazgs: fontdptr %d char %d ...",
           theChar->fontdataptr,theChar->ch); 
#endif

  /* fshrink >=1 expected !! */

  if(fabs(fshrink-ROUND(fshrink))<.01) 
           {pkshrinkchargs(theChar); return;}

  freelrumem(&(theChar->bmp2.bits));
  touchlrumem(&(theChar->bmp.bits));
  theChar->shrink=fshrink; 
 
  
  theChar->hof2=    CEIL(theChar->hof / fshrink);
  theChar->vof2=    CEIL((theChar->vof+1) / fshrink) -1;  
    
  theChar->bmp2.w = CEIL(theChar->bmp.w / fshrink) +2;                   
  theChar->bmp2.h = CEIL(theChar->bmp.h / fshrink) +2;      
       
  theChar->bmp2.type=GREYSCALE;
  alloc_bitmapgs(&theChar->bmp2);
  destptr=theChar->bmp2.bits;
  destbmuw=theChar->bmp2.bmu_wide;
  memset(destptr,0,BYTES_PER_BMUNIT*destbmuw*theChar->bmp2.h);
  srcbmuw=theChar->bmp.bmu_wide;
  SETBMUW(srcbmuw);

  if(theChar->bmp.bits==NULL) {
    pfprot("\nfatal error: my ugly memory management. sorry\n");
    exit(1);
  }
  
/*pfprot("DEBUG 
fshr %f hof %d vof %d hof2 %d vof2 %d\n",
       fshrink, theChar->hof,theChar->vof,theChar->hof2,theChar->vof2); 
pfprot("srcw %d srch %d destw %d desth %d \n", 
    theChar->bmp.w,theChar->bmp.h,theChar->bmp2.w,theChar->bmp2.h); */

        
/* #ifndef GREYINBMU  */
  i=theChar->vof2+1; 
  fi=(float)(theChar->vof+1);
  frows=MIN(fi-fshrink*(i-1),theChar->bmp.h);
  while(1) {
    oy=theChar->vof+1-CEIL(fi);
    if(oy>=theChar->bmp.h) break;              /* loop end is here */
    oh=MIN(CEIL(fi)-FLOOR(fi-frows),theChar->bmp.h-oy);
    iy=MIN(theChar->vof+1-FLOOR(fi),theChar->bmp.h);
    ih=MIN(FLOOR(fi)-CEIL(fi-frows),theChar->bmp.h-iy);
         
    srcptroy= theChar->bmp.bits+oy*srcbmuw;
    srcptriy= theChar->bmp.bits+iy*srcbmuw;
    srcptriyb=theChar->bmp.bits+(iy+ih-1)*srcbmuw;
    srcptroyb=theChar->bmp.bits+(oy+oh-1)*srcbmuw;
    topweight=1-(CEIL(fi)-fi);
    botweight=1-((fi-frows)-FLOOR(fi-frows));

    /* pfprot("\nrow info at fi %f
fr %f tw %f bw %f oy %d oh %d iy %d ih %d\n",
       fi,frows,topweight,botweight,oy,oh,iy,ih); */

    m= BITS_PER_BMUNIT -GREYSCALE;
    cp=destptr;

    j= -theChar->hof2; 
    fj=(float)(-theChar->hof);
    fcols=MIN(fshrink*(j+1)-fj,theChar->bmp.w);
    while(1) {                        
      ox=FLOOR(fj)+theChar->hof;
      if(ox>=theChar->bmp.w) break;       /* loop end is here */
      ow=MIN(CEIL(fj+fcols)-FLOOR(fj),theChar->bmp.w-ox);
      ix=MIN(CEIL(fj)+theChar->hof,theChar->bmp.w);
      iw=MIN(FLOOR(fj+fcols)-CEIL(fj),theChar->bmp.w-ix);
    
    /*pfprot("   col info at fj %f
      fc %f ox %d ow %d ix %d iw %d\n",fj,fcols,ox,ow,ix,iw); */
    
      leftweight=1-(fj-FLOOR(fj));
      rightweight=1-(CEIL(fj+fcols)-fj-fcols);

      fsam=0;
      /* count inner part */
      if(iw!=0 && ih!=0)
        if(iw==1 && ih==1)
          fsam+=TESTBIT(srcptriy,ix);
        else
          fsam+=sample(srcptriy, ix, iw,ih);
      /* count top and bottom row */
      if(iy==oy+1 && iw!=0)
        fsam+=topweight*samplerow(srcptroy,ix, iw);
      if(iy+ih+1==oy+oh && iw!=0)
        fsam+=botweight*samplerow(srcptroyb,ix, iw);
      /* count left and right row and the edjes*/
      if(ox+1==ix) { 
        if(ih!=0) 
          fsam+=leftweight*samplecol(srcptriy, ox,ih);
        if(iy==oy+1)
          fsam+=leftweight*topweight*TESTBIT(srcptroy,ox);
        if(iy+ih+1==oy+oh)
          fsam+=leftweight*botweight*TESTBIT(srcptroyb,ox);
      }
      if(ox+ow==ix+iw+1) {
        if(ih!=0)
          fsam+=rightweight*samplecol(srcptriy, ox+ow-1,ih);
        if(iy==oy+1)
          fsam+=rightweight*topweight*TESTBIT(srcptroy,ox+ow-1);
        if(iy+ih+1==oy+oh)
          fsam+=rightweight*botweight*TESTBIT(srcptroyb,ox+ow-1);
      }


      /* pfprot("  j %d srccol %d cols %d mcols %d sam %d\n",
              j,srccol,cols,mcols,sam);*/
      
      /*if(cp-destptr>=destbmuw) 
        pfprot("DEBUG INFORMATION pkshrinkprecazgs cp > h-border !!\n");
      if(destptr-theChar->bmp2.bits>=theChar->bmp2.h*destbmuw) 
        pfprot("DEBUG INFORMATION pkshrinkprecazgs cp > v-border !!\n");*/
    
      if(fsam!=0)                     
       *cp |= (LROUND((COLORS_PER_GREY-1)*fsam/(fshrink*fshrink)) << m);     
      if(m) m-=GREYSCALE;
      else {
        m=BITS_PER_BMUNIT - GREYSCALE;
        ++cp;
      };
      j++;
      fj=j*fshrink;
      fcols=MIN(fshrink,-theChar->hof-fj+theChar->bmp.w);
    }
    /* pfprot("did row\n");*/
    i--; 
    destptr+=destbmuw; 
    fi=i*fshrink;
    frows=MIN(fshrink,fi-theChar->vof+1+theChar->bmp.h);
  }
/* #else     that is ifdef GREYINBMU */
/* #endif */
  /* print_bitmap(&(theChar->bmp)); 
  print_bitmap(&(theChar->bmp2));*/
#ifdef DEBUGPK
  pfprot("... pkshrinkprecazgs)");
#endif
}



#define BLEVEL 0.25

void pkshrinkcharazbw(chdesc* theChar) {
  
  int destbmuw, srcbmuw, j,i;
  int rows,cols,firstcols,firstrows,srcrow,srccol,sam;
  int mcols,mrows;
  BMUNIT m;
  BMUNIT *cp,*destptr,*srcptr;


  /* pfprot("(pkshrinkgazgs ...\n"); */  

  freelrumem(&(theChar->bmp2.bits));
  touchlrumem(&(theChar->bmp.bits));
  theChar->shrink=fshrink;

  theChar->hof2=    CEIL(theChar->hof / fshrink);
  if(fshrink<1) 
    firstcols=1;
  else {
    firstcols=theChar->hof-ROUND((theChar->hof2-1)*fshrink);
    if(firstcols == 0) {
      firstcols=MAX(ROUND(fshrink),1);
     --theChar->hof2;
    }
  }
  if(firstcols>=theChar->bmp.w) firstcols=theChar->bmp.w;
  
  theChar->vof2=    CEIL((theChar->vof+1) / fshrink);
  if(fshrink<1) 
    firstrows=1;
  else {
    firstrows=theChar->vof+1-ROUND((theChar->vof2-1)*fshrink);
    if(firstrows == 0) {
      firstrows=ROUND(fshrink);
      --theChar->vof2;
    }
  }
  --theChar->vof2;
  if(firstrows>=theChar->bmp.h) firstrows=theChar->bmp.h;
      
  theChar->bmp2.w = CEIL(theChar->bmp.w / fshrink) +2;                   
  theChar->bmp2.h = CEIL(theChar->bmp.h / fshrink) +2;
    
  if(firstrows<=0 || firstcols<=0)   
     pfprot("IMPORTANT DEBUGINFORMATION 
break with fro %d fco %d
fshr %f hof %d vof %d hof2 %d vof2 %d\n",firstrows,firstcols,
       fshrink, theChar->hof,theChar->vof,theChar->hof2,theChar->vof2);
  
    
  theChar->bmp2.w = CEIL(theChar->bmp.w / fshrink) +2; 
  theChar->bmp2.h = CEIL(theChar->bmp.h / fshrink) +2;
                
  theChar->bmp2.type=BLACKNWHITE;
  alloc_bitmapgs(&theChar->bmp2);
  destptr=theChar->bmp2.bits;
  destbmuw=theChar->bmp2.bmu_wide;
  memset(destptr,0,BYTES_PER_BMUNIT*destbmuw*theChar->bmp2.h);
  
  srcptr=theChar->bmp.bits;
  srcbmuw=theChar->bmp.bmu_wide;
  SETBMUW(srcbmuw);


  if(srcptr==NULL) {
    pfprot("\nfatal error: my ugly memory management. sorry\n");
    exit(1);
  }

  rows=firstrows;
  srcrow=0;
  i=theChar->vof2+1;
  mrows=MAX(1,ROUND(i*fshrink)-ROUND((i-1)*fshrink));
  while(rows>0) {
          
    m=(1 << (BITS_PER_BMUNIT -1));  
    cp=destptr;
    srccol=0;
    cols=firstcols;

    j=-theChar->hof2; 
    mcols=MAX(1,ROUND((j+1)*fshrink)-ROUND(j*fshrink));
    while(cols>0) {
      if(cols==1 && rows==1) 
        sam=TESTBIT(srcptr,srccol);
      else 
        sam=sample(srcptr, srccol, cols,rows);
      if(sam) if(sam >= ROUND(mcols*mrows*BLEVEL)) *cp|=m;
    
      if(m==1) {
        m=((BMUNIT)1 << (BITS_PER_BMUNIT -1));
        ++cp;
      } else m >>=1;
      j++;
      if(fshrink <1) {
        srccol=FLOOR((j+0.5)*fshrink)+theChar->hof;  
        mcols=1; 
      } else {
        srccol=ROUND(j*fshrink)+theChar->hof;  
        mcols=ROUND((j+1)*fshrink)-ROUND(j*fshrink);
      }
      cols=MIN(mcols,theChar->bmp.w-srccol); 
    }
    /* pfprot("did row\n");*/
    i--; 
    destptr+=destbmuw;
    if(fshrink <1) {  
      srcrow= (theChar->vof+1)-CEIL((i-0.5)*fshrink);
      mrows=1;
    } else {
      srcrow= (theChar->vof+1)-ROUND(i*fshrink);
      mrows=ROUND(i*fshrink)-ROUND((i-1)*fshrink);
    }
    rows=MIN(mrows,theChar->bmp.h-srcrow); 
   
    srcptr=theChar->bmp.bits+srcrow*srcbmuw;
  }
  /* print_bitmap(&(theChar->bmp)); 
  print_bitmap(&(theChar->bmp2));*/
  /* pfprot(".. did it\n"); */
}

















