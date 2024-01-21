/****************************************************************************/
/*
//  HEXTYPE.C - A little Hex-Dump program. (c) 1992,94 by JPM !!
**  (ported to UNIX 1993 - Major changes done Dec 94)
**
**  GNU-C : gcc hextype.c -o hextype
**
*/
/****************************************************************************/

#include <malloc.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef linux
# include <getopt.h>
#endif
#include <string.h>

#define FALSE   (0)
#define TRUE    (1)

typedef unsigned char byte;
typedef unsigned long ulong;

#define tohex(c) ( ( ((c) < 10) ? ((c) | 0x30) : ((c) + 0x37) ) )

void Header(void)
{
   printf("\nHEXType - V 3.0 Written 1996 by Jürgen P. Meier\n");
   printf("           This is not Public Domain. It's FREEWARE !\n");
   printf("           Do with it what ever you want.\n");
   return;
}

void Usage(void)
{
   printf("\n Usage: hextype -ln | -bn files...\n");
   printf("     files... : List of files to display\n");
   printf("     -ln : Display only the first <n> Lines.\n");
   printf("     -bn : Display only the first <n> Bytes.\n");
   return;
}

void main(int argc,char *argv[])
{
  extern char *optarg;
  extern int optind, errno;
  char c;
  ulong anzl = 0;
  ulong anzb = 0;
  int i,j;
  byte *mem,*next,*buff,path[1025];
  FILE *file;
  ulong num,fc=0L;
  ulong ll,ld,bl,bd,bc;
/*
 0         1         2         3         4         5         6         7
 0123456789012345678901234567890123456789012345678901234567890123456789012345
"00000000 : XX XX XX XX XX XX XX XX-XX XX XX XX XX XX XX XX : 0123456789ABCDEF";
*/
  byte dump[100]= "00000000 : XX XX XX XX XX XX XX XX-XX XX XX XX XX XX XX XX : 0123456789ABCDEF";


  /* Header(); */

  while ((c = getopt(argc,argv,"l:L:b:B:v")) != -1)
    {
      switch (c)
	{
	case 'l':
	case 'L':
#ifdef DEBUG
	  printf("Sowing only the first %s lines.\n",optarg);
#endif
	  anzb = 0;
	  anzl = atoi (optarg);
	  break;
	case 'b':
	case 'B':
#ifdef DEBUG
	  printf("Sowing only the first %s bytes.\n",optarg);
#endif
	  anzl = 0;
	  anzb = atoi (optarg);
	  break;
	case ':':
	  fprintf(stderr,"\nError: missing argument!\n");
	  Usage();
	  exit(1);
	  break;
	case 'v':
	  Header();
	default:
	  Usage();
	  exit(1);
	}
    }
  
  next=&dump[61];
  mem=(byte *) calloc(0x4002,sizeof(byte));
  
  ll = anzl ? anzl : 0x0FFFFFFFL;
  bl = anzb ? anzb : 0xFFFFFFFFL;
  for (j=optind;j<argc;j++)
    {
      strcpy(path,argv[j]);
      if((file=fopen(path,"r")))
	{
	  printf("*** File: %s\n",path);
	  fc++;
	  bc=0;
	  
	  ld=ll+1;
	  bd=bl;
	  
	  while(bd && ld && (num=fread(mem,sizeof(byte),0x4000,file)))
	    {
	      buff=mem;
	      while(bd && --ld && num)
		{
		  if(bd < (ulong) 17)
		    {
		      if(num>bd)
			num=bd;
		      bd=0L;
		    }
		  sprintf((char *) dump,"%08lX",bc);
		  dump[8]=' ';
		  bc+=1;
		  if(num>15)
		    {
		      for(i=0;i<16;i++)
			{
			  dump[(i*3)+11] = tohex(buff[i] >> 4);
			  dump[(i*3)+12] = tohex(buff[i] & 0x0F);
			  next[i] = (((buff[i] < 32) || ((buff[i] > 127) && (buff[i] < 160))) ? '.' : buff[i]);
			}
		      puts(dump);
		      num-=16;
		      bd-=(bd<(ulong) 16) ? bd : 16;
		      buff+=16;
		      continue;
		    }
		  for(i=0;i<num;i++)
		    {
		      dump[(i*3)+11] = tohex(buff[i] >> 4);
		      dump[(i*3)+12] = tohex(buff[i] & 0x0F);
		      next[i] = (((buff[i] < 32) || ((buff[i] > 127) && (buff[i] < 160))) ? '.' : buff[i]);
		    }
		  for(i=num;i<16;i++)
		    {
		      dump[(i*3)+11] = ' ';
		      dump[(i*3)+12] = ' ';
		      next[i] = ' ';
		    }
		  puts(dump);
		  if(bd)
		    bd-=num;
		  num=0; 
		}
	    }
	  fclose(file);
	}
      else
	{
	  fprintf(stderr,"%s: cannot open file %s: %s\n",argv[0],path,strerror(errno));
	}
    }
  free(mem);
  printf("\nDisplayed: %lu File%s.\n",fc,(fc!=1 ? "s" : ""));
  exit (0); 
}
/*EOF*/
