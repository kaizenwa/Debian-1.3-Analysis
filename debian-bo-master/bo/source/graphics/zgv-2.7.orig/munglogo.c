/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * munglogo.c - generates zgvlogo.h from zgvlogo.gif
 */

#include <stdio.h>
#include "zgv.h"
#include "gifeng.h"

int logow,logoh;

byte *readzgvlogo();


main()
{
byte *image;

printf("Making logo file from 'zgvlogo.gif'...\n");
image=readzgvlogo();
if(image==NULL)
  printf("Couldn't find 'zgvlogo.gif' or not 2-colour.\n");
else
  writezgvlogo(image);
free(image);
}


byte *readzgvlogo()
{
byte *image,*pal;
PICINFO gi;

if(readgif("zgvlogo.gif",&image,&pal,NULL,0)!=_PIC_OK)
  return(NULL);
getgifinfo(&gi);
free(pal);
logow=gi.width; logoh=gi.height;
if(gi.bpp!=1)
  return(NULL);
return(image);
}


writezgvlogo(image)
byte *image;
{
FILE *out;
int x,y,c=0;
byte *ptr;

if((out=fopen("zgvlogo.h","w"))==NULL)
  printf("Couldn't open 'zgvlogo.h'.\n");
else
  {
  fprintf(out,
    "/* zgv logo bytemap - automatically generated from zgvlogo.gif,\n"
    " * edits will be lost! Use your own GIF in place of zgvlogo.gif\n"
    " * if you want, it should be 620x80 2-colour. Then run 'munglogo'\n"
    " * on it to generate this file.\n"
    " * or you losing saddoes out there could just type 'make' :)\n"
    " */\n\n"
    "int logoh=%d,logow=%d;\n"
    "unsigned char zgvlogo[]={\n",logoh,logow);
  for(y=0;y<logoh;y++)
    {
    ptr=image+y*logow;
    for(x=0;x<logow;x++,ptr++)
      {
      if((x%8)==0) c=0;
      c+=((*ptr)&1);
      if((x%8)==7)
        fprintf(out,"%3d,",c);
      else
        c<<=1;
      if((x%160)==159) fputc('\n',out);
      }
    if((x%8)>0)
      {
      if((x%8)!=7) c<<=(7-(x%8));
      fprintf(out,"%3d,",c);
      }
    fputc('\n',out);
    }
  fprintf(out,"\n  0};\n");
  fclose(out);
  printf("Logo file made ok.\n");
  }

exit(0);
}
