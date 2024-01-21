/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * gifeng.c - GIF decoding engine.
 */


#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>
#include <stdlib.h>
#include "zgv.h"
#include "gifeng.h"


static byte *image;
hffunc howfar;
FILE *global_gif_infile;   /* only used for error cleanup */
byte *global_palette_ptr;  /* same here */

static int imagex,imagey,stoprightnow;
static int dc_cc,dc_eoi;                  /* the CC and EOI codes */
static int passnum,passyloc,passstep;     /* for interlaced GIFs */
static int interlaced,width,height,bpp,numcols;

static int pwr2[16]={1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
                             16384,32768};


/* now this is for the string table.
 * the st_ptr array stores which pos to back reference to,
 *  each string is [...]+ end char, [...] is traced back through
 *  the 'pointer' (index really), then back through the next, etc.
 *  a 'null pointer' is = to UNUSED.
 * the st_chr array gives the end char for each.
 *  an unoccupied slot is = to UNUSED.
 */
#define UNUSED 32767
#define MAXSTR 4096
static int st_ptr[MAXSTR],st_chr[MAXSTR],st_last;
static int st_ptr1st[MAXSTR];

/* this is for the byte -> bits mangler:
 *  dc_bitbox holds the bits, dc_bitsleft is number of bits left in dc_bitbox,
 *  blocksize is how many bytes of an image sub-block we have left.
 */
static int dc_bitbox,dc_bitsleft,blocksize;

static int local_colour_map;

struct {
  char sig[6];           /* should be GIF87a or GIF89a */
  byte wide_lo,wide_hi;  /* NUXI Problem Avoidance System (tm) */
  byte high_lo,high_hi;  /* these are 'screen size', BTW */
  byte misc;             /* misc, and bpp */
  byte back;             /* background index */
  byte zero;             /* if this ain't zero, problem */
  } gifhed;
  
/* BTW, the NUXI thing above is because most of this code is reused from
 * a GIF viewer I did for Tektronix 4200 series terminals. If you want a copy,
 * mail me, but I figure not many people use them.
 * (at least, not from Linux PCs :-))
 */
  
struct {
  byte left_lo,left_hi;  /* usually zero - ignore */
  byte top_lo,top_hi;
  byte wide_lo,wide_hi;  /* this is 'image size', often the same as screen */
  byte high_lo,high_hi;
  byte misc;
  } imagehed;


/* prototypes */
int readgif(char *,byte **,byte **,hffunc,int);
void getgifinfo(PICINFO *);
int readgifhed(FILE *);
void readcolmap(FILE *,byte *);
int readimagehed(FILE *);
int decompress(FILE *);
void inittable(void);
void addstring(int,int);
int readcode(int *,int,FILE *);
void outputstring(int);
int findfirstchr(int);
void outputchr(int);
void dummyfunc(int,int);


/* use like this:
 *   result=readgif("thingy.gif",&ptr_to_image_data,&ptr_to_palette_data,
 *                               show_how_far_func,errignore);
 * show_how_far_func, for percent complete displays etc., can be NULL.
 * any value of 'result' other than _PIC_OK means something went wrong;
 * see zgv.h for more details and error codes.
 */
int readgif(giffn,theimageptr,palptr,howfarfunc,errignore)
char *giffn;
byte **theimageptr;  /* pointer to pointer to image data */
byte **palptr;       /* same for palette */
hffunc howfarfunc;
int errignore;       /* don't deallocate pal on decompress err. if true */
{
byte *palette;
FILE *in;
int poserr;

imagex=imagey=stoprightnow=dc_bitbox=dc_bitsleft=blocksize=0;

if(howfarfunc==NULL)
  howfar=dummyfunc;
else
  howfar=howfarfunc;            /* passing down as arg has big perf. impact */
     /* besides, local variables are for pencil-necked Modula-2 programmers */

if((in=global_gif_infile=fopen(giffn,"rb"))==NULL)
  return(_PICERR_NOFILE);
else
  {
  if((palette=global_palette_ptr=(byte *)malloc(768))==NULL)
    {
    fclose(in);
    return(_PICERR_NOMEM);
    }

  if((poserr=  readgifhed(in)  )!=_PIC_OK)
    {
    fclose(in);
    free(palette); *palptr=NULL;
    return(poserr);
    }
    
  if(!local_colour_map) readcolmap(in,palette);
               
  if((poserr=  readimagehed(in))!=_PIC_OK) 
    {
    fclose(in);
    free(palette); *palptr=NULL;
    return(poserr);
    }
  
  if(local_colour_map) readcolmap(in,palette);
  
  if((image=(byte *)malloc(width*height))==NULL)
    {
    fclose(in);
    free(palette); *palptr=NULL;
    return(_PICERR_NOMEM);
    }

  if(errignore) memset(image,0,width*height);
  
  if( decompress(in) !=_PIC_OK)
    {
    fclose(in);
    *theimageptr=image;
    *palptr=palette;
    if(!errignore) { free(palette); *palptr=NULL; }
    return(_PICERR_CORRUPT);
    }
               
  fclose(in);
  *theimageptr=image;
  *palptr=palette;

  return(_PIC_OK);
  }
}


/* use if no real howfar func. is to be used */
void dummyfunc(a,b)
int a,b;
{
}


void getgifinfo(ginfo)
PICINFO *ginfo;
{
ginfo->width=width;
ginfo->height=height;
gifhed.wide_lo=0;  /* yes, it's nasty, but wide_lo is unimportant anyway :) */
strcpy(ginfo->type,gifhed.sig+3);
ginfo->bpp=bpp;
ginfo->numcols=numcols;
}

int readgifhed(in)
FILE *in;
{
fread(&gifhed,sizeof(gifhed),1,in);
if(strncmp(gifhed.sig,"GIF",3))
  return(_PICERR_BADMAGIC);
local_colour_map=(gifhed.misc&128)?0:1;
bpp=(gifhed.misc&7)+1;
numcols=pwr2[bpp];
return(_PIC_OK);
}


void readcolmap(in,palette)
FILE *in;
byte *palette;
{
int f;

for(f=0;f<numcols;f++)
  {
  palette[    f]=(byte)fgetc(in);
  palette[256+f]=(byte)fgetc(in);
  palette[512+f]=(byte)fgetc(in);  
  }
}


int readimagehed(in)
FILE *in;
{
int c,f;

c=fgetc(in);
while(c=='!')      /* oh damn it, they've put an ext. block in, ditch it */
  {
  fgetc(in);       /* function code, means nothing to me */
  c=fgetc(in);
  while(c!=0)
    {    /* well then, c = number of bytes, so ignore that many */
    for(f=1;f<=c;f++) fgetc(in);
    c=fgetc(in);
    }
  c=fgetc(in);     /* test for image again */
  }
if(c!=',')
  return(_PICERR_NOIMAGE);
fread(&imagehed,sizeof(imagehed),1,in);

if((imagehed.misc&128)==0 && local_colour_map)
  return(_PICERR_NOCOLOURMAP);

if((imagehed.misc&64)!=0)
  {
  interlaced=1;
  passnum=1;
  passyloc=0;
  passstep=8;
  }
else
  interlaced=0;

width=(imagehed.wide_lo+256*imagehed.wide_hi);
height=(imagehed.high_lo+256*imagehed.high_hi);

if(local_colour_map) numcols=pwr2[(imagehed.misc&7)+1];

return(_PIC_OK);
}


/* 'image' is used directly for speed;
 * otherwise it'd need to use twice as many parameters in a recursive
 * routine (outputstring)
 */

int decompress(in)
FILE *in;
{
int csize,orgcsize;
int newcode,oldcode,k;

csize=fgetc(in)+1;
orgcsize=csize;
inittable();

do
  {
  if(!readcode(&newcode,csize,in)) return(_PICERR_CORRUPT);
  oldcode=newcode;
  }
while(newcode==dc_cc);
outputstring(newcode);

while((newcode!=dc_eoi)&&(!stoprightnow))
  {
  if(!readcode(&newcode,csize,in)) return(_PICERR_CORRUPT);
  if(newcode!=dc_eoi)
    {
    if(newcode==dc_cc)
      {
      inittable();
      csize=orgcsize;
      if(!readcode(&newcode,csize,in)) return(_PICERR_CORRUPT);
      oldcode=newcode;
      outputstring(newcode);
      }
    else
      {
      if(st_chr[newcode]!=UNUSED)
        {
        outputstring(newcode);
        k=findfirstchr(newcode);
        }
      else
        {
        k=findfirstchr(oldcode);
        outputstring(oldcode);
        outputchr(k);
        }
      addstring(oldcode,k);
      if(st_last==(pwr2[csize]-1))
        {
        csize++;
        if(csize==13) csize=12;
        }
      oldcode=newcode;
      }
    }
  }

return(_PIC_OK);
}


void inittable(void)
{
int f;

for(f=0;f<MAXSTR;f++)
  {
  st_chr[f]=UNUSED;
  st_ptr[f]=UNUSED;
  }
for(f=0;f<numcols+2;f++)
  {
  st_ptr[f]=UNUSED;     /* these are root values... no back pointer */
  st_chr[f]=f;          /* for numcols and numcols+1, doesn't matter */
  }
st_last=numcols+1;      /* last occupied slot */
dc_cc=numcols;
dc_eoi=numcols+1;
if(numcols==2)
  {
  st_chr[2]=st_chr[3]=UNUSED;
  dc_cc=4;
  dc_eoi=5;
  st_chr[dc_cc]=dc_cc; st_chr[dc_eoi]=dc_eoi;
  st_last=5;
  }
}


/* add a string specified by oldstring + chr to string table */
void addstring(oldcode,chr)
int oldcode,chr;
{
st_last++;
while(st_chr[st_last]!=UNUSED)
  st_last++;
st_chr[st_last]=chr;
st_ptr[st_last]=oldcode;
if(st_ptr[oldcode]==UNUSED)          /* if we're pointing to a root... */
  st_ptr1st[st_last]=oldcode;        /* then that holds the first char */
else                                 /* otherwise... */
  st_ptr1st[st_last]=st_ptr1st[oldcode]; /* use their pointer to first */
}


/* read a code of bitlength numbits from in file */
int readcode(newcode,numbits,in)
int *newcode,numbits;
FILE *in;
{
int bitsfilled,got;

bitsfilled=got=0;
(*newcode)=0;

while(bitsfilled<numbits)
  {
  if(dc_bitsleft==0)        /* have we run out of bits? */
    {
    if(blocksize<=0)        /* end of block? */
      blocksize=fgetc(in);  /* start new block, blocksize = num of bytes */
    blocksize--;
    dc_bitbox=fgetc(in);    /* read eight more bits */
    if(feof(in))
      return(0);
    dc_bitsleft=8;
    }
  if(dc_bitsleft<(numbits-bitsfilled))
    got=dc_bitsleft;
  else
    got=numbits-bitsfilled;
  (*newcode)|=((dc_bitbox&(pwr2[got]-1))<<bitsfilled);
  dc_bitbox>>=got;
  dc_bitsleft-=got;
  bitsfilled+=got;
  }

if((*newcode)<0 || (*newcode)>MAXSTR-1) return(0);
return(1);
}


void outputstring(code)
int code;
{
if(st_ptr[code]!=UNUSED)
  outputstring(st_ptr[code]);
outputchr(st_chr[code]);
}


void outputchr(code)
int code;
{
if(!stoprightnow)
  {
  *(image+(interlaced?passyloc:imagey)*width+imagex)=code;
  imagex++;
  if(imagex>=width)
    {
    imagex=0;
    imagey++;
    if(interlaced)
      {
      passyloc+=passstep;
      if(passyloc>=height)
        {
        passnum++;
        passyloc=pwr2[4-passnum];
        passstep=pwr2[5-passnum];
        }
      }
    howfar(imagey,height);
    if(imagey==height) stoprightnow=1;
    }
  }
}


int findfirstchr(code)
int code;
{
if(st_ptr[code]!=UNUSED)       /* not first? then use brand new st_ptr1st! */
  code=st_ptr1st[code];                /* now with no artificial colouring */
return(st_chr[code]);
}


aborted_file_gif_cleanup()
{
free(image);
free(global_palette_ptr);
fclose(global_gif_infile);
}
