/*
** GIFtrans v1.9
**
** Convert any GIF file into a GIF89a
** Allows for setting a transparent color and adding a comment
** Also code to analyze GIF contents
**
** Copyright (c) 24.2.94 by Andreas Ley <ley@rz.uni-karlsruhe.de>
**
** Permission to use, copy, modify, and distribute this software for any
** purpose and without fee is hereby granted, provided that the above
** copyright notice appears in all copies. This software is provided "as is"
** and without any express or implied warranties.
**
** This program has been tested on a HP9000/720 with HP-UX A.08.07
** In this environment, neither lint -u nor gcc -Wall produce any messages.
** If you encounter any errors or need to make any changes to port it
** to another platform, please contact me.
**
** Version history
**
** Version 1.9 - 1.6.94
**      Fixed a bug which caused color names to be rejected.
**
** Version 1.8 - 30.5.94
**      Accept #rrggbb style arguments.
**      Do nothing if rgb-color not found in GIF.
**
** Version 1.7 - 16.5.94
**      Added -l option to only list the color table.
**      Added -L option for verbose output without creating a gif.
**      Added -b option to change the background color index.
**      Display all matching color names for color table entries.
**      Fixed a bug which caused bad color names if rgb.txt starts with
**              whitespace.
**      Doesn't use strdup anymore.
**      Fixed =& bug on dec machines.
**
** Version 1.6 - 5.4.94
**      Added color names recognition.
**
** Version 1.5 - 15.3.94
**      Added basic verbose output to analyze GIFs.
**
** Version 1.4 - 8.3.94
**      Fixed off-by-one bug in Local Color table code.
**      Added option to add or remove a comment.
**      Transparency is no longer the default.
**
** Thanx for bug reports, ideas and fixes to
**      patricka@cs.kun.nl (Patrick Atoon)
**      wes@msc.edu (Wes Barris)
**      pmfitzge@ingr.com (Patrick M. Fitzgerald)
**      hoesel@chem.rug.nl (frans van hoesel)
**      boardman@jerry.sal.wisc.edu (Dan Boardman)
**      krweiss@chip.ucdavis.edu (Ken Weiss)
**      chuck@trantor.harris-atd.com (Chuck Musciano)
**      heycke@camis.stanford.edu (Torsten Heycke)
**      claw@spacsun.rice.edu (Colin Law)
**      jwalker@eos.ncsu.edu (Joseph C. Walker)
**      Bjorn.Borud@alkymi.unit.no (Bjorn Borud)
**      Christopher.Vance@adfa.oz.au (CJS Vance)
**      pederl@norway.hp.com (Peder Langlo)
**
** Original distribution site is
**      ftp://ftp.rz.uni-karlsruhe.de/pub/net/www/tools/giftrans.c
** Additional info can be found on
**      http://melmac.harris-atd.com/transparent_images.html
*/

char header[]="GIFtrans v1.9\n(c) 1994 by Andreas Ley\n";
char    rgb[]="/usr/lib/X11/rgb.txt";

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/param.h>

#define FALSE   (0)             /* This is the naked Truth */
#define TRUE    (1)             /* and this is the Light */

#define SUCCESS (0)
#define FAILURE (1)

struct entry {
        struct entry    *next;
        char            *name;
        int             red;
        int             green;
        int             blue;
        } *root;

#define NONE    (-1)
#define OTHER   (-2)
#define RGB     (-3)

struct color {
        int             index;
        int             red;
        int             green;
        int             blue;
        } bc,tc;

char    *image,*comment;
int     skipcomment;
int     list,verbose,output=TRUE;

char    true[]="True";
char    false[]="False";

#define readword(buffer)        ((buffer)[0]+256*(buffer)[1])
#define readflag(buffer)        ((buffer)?true:false)
#define hex(c)                  ('a'<=(c)&&(c)<='z'?(c)-'a'+10:'A'<=(c)&&(c)<='Z'?(c)-'A'+10:(c)-'0')


void writedata(dest,data,len)
FILE    *dest;
char    *data;
size_t  len;
{
        unsigned char   size;

        while (len) {
                size=len<256?len:255;
                (void)fwrite((void *)&size,1,1,dest);
                (void)fwrite((void *)data,(size_t)size,1,dest);
                data+=size;
                len-=size;
        }
        size=0;
        (void)fwrite((void *)&size,1,1,dest);
}


void skipdata(src)
FILE    *src;
{
        unsigned char   size;

        do {
                (void)fread((void *)&size,1,1,src);
                (void)fseek(src,(long int)size,SEEK_CUR);
        } while (!feof(src)&&size>0);
}


void transblock(src,dest)
FILE    *src;
FILE    *dest;
{
        unsigned char   size,buffer[256];

        (void)fread((void *)&size,1,1,src);
        if (output)
                (void)fwrite((void *)&size,1,1,dest);
        (void)fread((void *)buffer,(size_t)size,1,src);
        if (output)
                (void)fwrite((void *)buffer,(size_t)size,1,dest);
}


void transdata(src,dest)
FILE    *src;
FILE    *dest;
{
        unsigned char   size,buffer[256];

        do {
                (void)fread((void *)&size,1,1,src);
                if (output)
                        (void)fwrite((void *)&size,1,1,dest);
                (void)fread((void *)buffer,(size_t)size,1,src);
                if (output)
                        (void)fwrite((void *)buffer,(size_t)size,1,dest);
        } while (!feof(src)&&size>0);
}


int giftrans(src,dest)
FILE    *src;
FILE    *dest;
{
        unsigned char   buffer[3*256],lsd[7],gce[5];
        unsigned int    cnt,cols,size,gce_present;
        struct entry    *rgbptr;

        /* Header */
        (void)fread((void *)buffer,6,1,src);
        if(strncmp((char *)buffer,"GIF",3)) {
                (void)fprintf(stderr,"No GIF file!\n");
                return(1);
        }
        if (verbose) {
                buffer[6]='\0';
                (void)fprintf(stderr,"Header: \"%s\"\n",buffer);
        }
        if (output)
                (void)fputs("GIF89a",dest);

        /* Logical Screen Descriptor */
        (void)fread((void *)lsd,7,1,src);
        if (verbose) {
                (void)fprintf(stderr,"Logical Screen Descriptor:\n");
                (void)fprintf(stderr,"\tLogical Screen Width: %d pixels\n",readword(lsd));
                (void)fprintf(stderr,"\tLogical Screen Height: %d pixels\n",readword(lsd+2));
                (void)fprintf(stderr,"\tGlobal Color Table Flag: %s\n",readflag(lsd[4]&0x80));
                (void)fprintf(stderr,"\tColor Resolution: %d bits\n",(lsd[4]&0x70>>4)+1);
                if (lsd[4]&0x80) {
                        (void)fprintf(stderr,"\tSort Flag: %s\n",readflag(lsd[4]&0x8));
                        (void)fprintf(stderr,"\tSize of Global Color Table: %d colors\n",2<<(lsd[4]&0x7));
                        (void)fprintf(stderr,"\tBackground Color Index: %d\n",lsd[5]);
                }
                if (lsd[6])
                        (void)fprintf(stderr,"\tPixel Aspect Ratio: %d (Aspect Ratio %f)\n",lsd[6],((double)lsd[6]+15)/64);
        }

        /* Global Color Table */
        if(lsd[4]&0x80) {
                size=2<<(lsd[4]&0x7);
                (void)fread((void *)buffer,size,3,src);
                if (bc.index==RGB)
                        for(cnt=0;cnt<size&&bc.index==RGB;cnt++)
                                if (buffer[3*cnt]==bc.red&&buffer[3*cnt+1]==bc.green&&buffer[3*cnt+2]==bc.blue)
                                        bc.index=cnt;
                if (bc.index>=0)
                        lsd[5]=bc.index;
                if (tc.index==RGB)
                        for(cnt=0;cnt<size&&tc.index==RGB;cnt++)
                                if (buffer[3*cnt]==tc.red&&buffer[3*cnt+1]==tc.green&&buffer[3*cnt+2]==tc.blue)
                                        tc.index=cnt;
                if (tc.index==OTHER)
                        tc.index=lsd[5];
        }
        if (output)
                (void)fwrite((void *)lsd,7,1,dest);
        if(lsd[4]&0x80) {
                if (list||verbose) {
                        (void)fprintf(stderr,"Global Color Table:\n");
                        for(cnt=0;cnt<size;cnt++) {
                                (void)fprintf(stderr,"\tColor %d: Red %d, Green %d, Blue %d",cnt,buffer[3*cnt],buffer[3*cnt+1],buffer[3*cnt+2]);
                                (void)fprintf(stderr,", #%02x%02x%02x",buffer[3*cnt],buffer[3*cnt+1],buffer[3*cnt+2]);
                                for (rgbptr=root,cols=0;rgbptr;rgbptr=rgbptr->next)
                                        if (rgbptr->red==buffer[3*cnt]&&rgbptr->green==buffer[3*cnt+1]&&rgbptr->blue==buffer[3*cnt+2])
                                                (void)fprintf(stderr,"%s%s",cols++?", ":" (",rgbptr->name);
                                (void)fprintf(stderr,"%s\n",cols?")":"");
                        }
                }
                if (output)
                        (void)fwrite((void *)buffer,size,3,dest);
        }

        gce_present=FALSE;
        do {
                (void)fread((void *)buffer,1,1,src);
                switch (buffer[0]) {
                case 0x01:      /* Plain Text Extension */
                        if (verbose)
                                (void)fprintf(stderr,"Plain Text Extension: 0x%02x\n",buffer[0]);
                        if (output)
                                (void)fwrite((void *)buffer,1,1,dest);
                        transblock(src,dest);
                        transdata(src,dest);
                        break;
                case 0x2c:      /* Image Descriptor */
                        if (verbose)
                                (void)fprintf(stderr,"Image Descriptor: 0x%02x\n",buffer[0]);
                        /* Write Graphic Control Extension */
                        if (tc.index>=0||gce_present) {
                                if (output)
                                        (void)fputs("\041\371\004",dest);
                                if (!gce_present) {
                                        gce[0]=0;
                                        gce[1]=0;
                                        gce[2]=0;
                                }
                                if (tc.index>=0) {
                                        gce[0]|=0x01;   /* Set Transparent Color Flag */
                                        gce[3]=tc.index;        /* Set Transparent Color Index */
                                }
                                gce[4]=0;
                                if (output)
                                        (void)fwrite((void *)gce,5,1,dest);
                                }
                        /* Write Image Descriptor */
                        (void)fread((void *)(buffer+1),9,1,src);
                        if (verbose) {
                                (void)fprintf(stderr,"\tImage Left Position: %d pixels\n",readword(buffer+1));
                                (void)fprintf(stderr,"\tImage Top Position: %d pixels\n",readword(buffer+3));
                                (void)fprintf(stderr,"\tImage Width: %d pixels\n",readword(buffer+5));
                                (void)fprintf(stderr,"\tImage Height: %d pixels\n",readword(buffer+7));
                                (void)fprintf(stderr,"\tLocal Color Table Flag: %s\n",readflag(buffer[9]&0x80));
                                (void)fprintf(stderr,"\tInterlace Flag: %s\n",readflag(buffer[9]&0x40));
                                if (buffer[9]&0x80) {
                                        (void)fprintf(stderr,"\tSort Flag: %s\n",readflag(buffer[9]&0x20));
                                        (void)fprintf(stderr,"\tSize of Global Color Table: %d colors\n",2<<(buffer[9]&0x7));
                                }
                        }
                        if (output)
                                (void)fwrite((void *)buffer,10,1,dest);
                        /* Local Color Table */
                        if(buffer[8]&0x80) {
                                size=2<<(buffer[8]&0x7);
                                (void)fread((void *)buffer,size,3,src);
                                if (verbose) {
                                        (void)fprintf(stderr,"Local Color Table:\n");
                                        for(cnt=0;cnt<size;cnt++)
                                                (void)fprintf(stderr,"\tColor %d: Red %d, Green %d, Blue %d\n",cnt,buffer[3*cnt],buffer[3*cnt+1],buffer[3*cnt+2]);
                                }
                                if (output)
                                        (void)fwrite((void *)buffer,size,3,dest);
                        }
                        /* Table Based Image Data */
                        (void)fread((void *)buffer,1,1,src);
                        if (verbose) {
                                (void)fprintf(stderr,"Table Based Image Data:\n");
                                (void)fprintf(stderr,"\tLZW Minimum Code Size: 0x%02x\n",buffer[0]);
                        }
                        if (output)
                                (void)fwrite((void *)buffer,1,1,dest);
                        transdata(src,dest);
                        gce_present=FALSE;
                        break;
                case 0x3b:      /* Trailer */
                        if (verbose)
                                (void)fprintf(stderr,"Trailer: 0x%02x\n",buffer[0]);
                        if (comment&&*comment&&output) {
                                (void)fputs("\041\376",dest);
                                writedata(dest,comment,strlen(comment));
                        }
                        if (output)
                                (void)fwrite((void *)buffer,1,1,dest);
                        break;
                case 0x21:      /* Extension */
                        (void)fread((void *)(buffer+1),1,1,src);
                        switch (buffer[1]) {
                        case 0xf9:      /* Graphic Control Extension */
                                if (verbose)
                                        (void)fprintf(stderr,"Graphic Control Extension: 0x%02x 0x%02x",buffer[0],buffer[1]);
                                (void)fread((void *)buffer,1,1,src);
                                if (verbose)
                                        (void)fprintf(stderr," (%d)\n",buffer[0]);
                                size=buffer[0];
                                (void)fread((void *)gce,size,1,src);
                                if (verbose) {
                                        (void)fprintf(stderr,"\tDisposal Method: %d ",gce[0]&0x1c>>2);
                                        switch (gce[0]&0x1c>>2) {
                                        case 0:
                                                (void)fprintf(stderr,"(no disposal specified)\n");
                                                break;
                                        case 1:
                                                (void)fprintf(stderr,"(do not dispose)\n");
                                                break;
                                        case 2:
                                                (void)fprintf(stderr,"(restore to background color)\n");
                                                break;
                                        case 3:
                                                (void)fprintf(stderr,"(restore to previous)\n");
                                                break;
                                        default:
                                                (void)fprintf(stderr,"(to be defined)\n");
                                        }
                                        (void)fprintf(stderr,"\tUser Input Flag: %s\n",readflag(gce[0]&0x2));
                                        (void)fprintf(stderr,"\tTransparent Color Flag: %s\n",readflag(gce[0]&0x1));
                                        (void)fprintf(stderr,"\tDelay Time: %d\n",readword(gce+1));
                                        if (gce[0]&0x1)
                                                (void)fprintf(stderr,"\tTransparent Color Index: %d\n",gce[3]);
                                }
                                (void)fread((void *)buffer,1,1,src);
                                gce_present=TRUE;
                                break;
                        case 0xfe:      /* Comment Extension */
                                if (verbose)
                                        (void)fprintf(stderr,"Comment Extension: 0x%02x 0x%02x\n",buffer[0],buffer[1]);
                                if (skipcomment)
                                        skipdata(src);
                                else {
                                        if (output)
                                                (void)fwrite((void *)buffer,2,1,dest);
                                        transdata(src,dest);
                                }
                                break;
                        case 0xff:      /* Application Extension */
                                if (verbose)
                                        (void)fprintf(stderr,"Application Extension: 0x%02x 0x%02x\n",buffer[0],buffer[1]);
                                if (output)
                                        (void)fwrite((void *)buffer,2,1,dest);
                                transblock(src,dest);
                                transdata(src,dest);
                                break;
                        default:
                                (void)fprintf(stderr,"0x%08lx: Unknown label 0x%02x!\n",ftell(src)-1,buffer[1]);
                                return(1);
                        }
                        break;
                default:
                        (void)fprintf(stderr,"0x%08lx: Unknown extension 0x%02x!\n",ftell(src)-1,buffer[0]);
                        return(1);
                }
        } while (buffer[0]!=0x3b&&!feof(src));
        return(buffer[0]==0x3b?SUCCESS:FAILURE);
}



int getindex(c,arg)
struct color    *c;
char    *arg;
{
        struct entry    *ptr;

        if ('0'<=*arg&&*arg<='9')
                c->index=atoi(arg);
        else if (*arg=='#') {
                if (strlen(arg)==4) {
                        c->index=RGB;
                        c->red=hex(arg[1])<<4;
                        c->green=hex(arg[2])<<4;
                        c->blue=hex(arg[3])<<4;
                }
                else if (strlen(arg)==7) {
                        c->index=RGB;
                        c->red=(hex(arg[1])<<4)+hex(arg[2]);
                        c->green=(hex(arg[3])<<4)+hex(arg[4]);
                        c->blue=(hex(arg[5])<<4)+hex(arg[6]);
                }
                else {
                        (void)fprintf(stderr,"%s: illegal color specification: %s\n",image,arg);
                        return(FAILURE);
                }
        }
        else {
                for (ptr=root;ptr&&c->index!=RGB;ptr=ptr->next)
                        if (!strcmp(ptr->name,arg)) {
                                c->index=RGB;
                                c->red=ptr->red;
                                c->green=ptr->green;
                                c->blue=ptr->blue;
                        }
                if (c->index!=RGB) {
                        (void)fprintf(stderr,"%s: no such color: %s\n",image,arg);
                        return(FAILURE);
                }
        }
        return(SUCCESS);
}



void usage()
{
        (void)fprintf(stderr,"Usage: %s [-t index|-T] [-b index] [-c comment|-C] [-l|-L|-V] [filename]\n",image);
        (void)fprintf(stderr,"Convert any GIF file into a GIF89a, with the folloing changes possible:\n");
        (void)fprintf(stderr,"-t Specify the index of the transparent color within the color table\n");
        (void)fprintf(stderr,"-T Index of the transparent color is the background color index\n");
        (void)fprintf(stderr,"-b Specify the index of the background color within the color table\n");
        (void)fprintf(stderr,"-c Add a comment\n");
        (void)fprintf(stderr,"-C Remove old comment\n");
        (void)fprintf(stderr,"-l Only list the color table\n");
        (void)fprintf(stderr,"-L Verbose output of GIFs contents\n");
        (void)fprintf(stderr,"-V Verbose output while converting\n");
        (void)fprintf(stderr,"Colors may be specified as index, as rgb.txt entry or in the #rrggbb form.\n");
        exit(1);
}


int main(argc,argv)
int     argc;
char    *argv[];
{
        int             c;
        extern char     *optarg;
        extern int      optind;
        char            error[2*MAXPATHLEN+14],line[BUFSIZ],*ptr,*nptr;
        struct entry    **next;
        FILE            *src;
        int             stat;

        image=argv[0];
        root=NULL;
        if ((src=fopen(rgb,"r"))) {
                next= &root;
                while (fgets(line,sizeof(line),src)) {
                        *next=(struct entry *)malloc(sizeof(struct entry));
                        for (ptr=line;strchr(" \t",*ptr);ptr++);
                        for (nptr=ptr;!strchr(" \t",*ptr);ptr++);
                        *ptr++='\0';
                        (*next)->red=atoi(nptr);
                        for (;strchr(" \t",*ptr);ptr++);
                        for (nptr=ptr;!strchr(" \t",*ptr);ptr++);
                        *ptr++='\0';
                        (*next)->green=atoi(nptr);
                        for (;strchr(" \t",*ptr);ptr++);
                        for (nptr=ptr;!strchr(" \t",*ptr);ptr++);
                        *ptr++='\0';
                        (*next)->blue=atoi(nptr);
                        for (;strchr(" \t",*ptr);ptr++);
                        for (nptr=ptr;!strchr(" \t\r\n",*ptr);ptr++);
                        *ptr='\0';
                        (void)strcpy((*next)->name=(char *)malloc(strlen(nptr)+1),nptr);
                        (*next)->next=NULL;
                        next= &(*next)->next;
                }
                (void)fclose(src);
        }
        else {
                (void)sprintf(error,"%s: cannot open %s",image,rgb);
                perror(error);
                return(FAILURE);
        }

        bc.index=NONE;
        tc.index=NONE;
        comment=NULL;
        skipcomment=FALSE;
        verbose=FALSE;
        while ((c=getopt(argc,argv,"t:Tb:c:ClLVvh?")) != EOF)
                switch ((char)c) {
                case 'b':
                        if (getindex(&bc,optarg))
                                return(FAILURE);
                        break;
                case 't':
                        if (getindex(&tc,optarg))
                                return(FAILURE);
                        break;
                case 'T':
                        tc.index=OTHER;
                        break;
                case 'c':
                        comment=optarg;
                        break;
                case 'C':
                        skipcomment=TRUE;
                        break;
                case 'l':
                        list=TRUE;
                        output=FALSE;
                        break;
                case 'L':
                        verbose=TRUE;
                        output=FALSE;
                        break;
                case 'V':
                        verbose=TRUE;
                        break;
                case 'v':
                        (void)fprintf(stderr,header);
                        return(0);
                case 'h':
                        (void)fprintf(stderr,header);
                case '?':
                        usage();
                }
        if (optind+1<argc||(bc.index==NONE&&tc.index==NONE&&comment==NULL&&!skipcomment&&!list&&!verbose))
                usage();

        if (optind<argc)
                if (strcmp(argv[optind],"-"))
                        if ((src=fopen(argv[optind],"r"))!=NULL) {
                                stat=giftrans(src,stdout);
                                (void)fclose(src);
                        }
                        else {
                                (void)sprintf(error,"%s: cannot open %s",image,argv[optind]);
                                perror(error);
                                stat=FAILURE;
                        }
                else
                        stat=giftrans(stdin,stdout);
        else
                stat=giftrans(stdin,stdout);

        return(stat);
}
