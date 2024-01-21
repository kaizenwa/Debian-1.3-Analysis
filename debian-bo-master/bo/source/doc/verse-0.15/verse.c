/* This is a Program to read a database and produce a daily scriptural verse
 * It expects the Database in VERSE_LIB (defined in Makefile)
 * 
 * 0.11 Feb 4 1995 Fixed bug with wrong month calculation
 * Written by Christoph Lameter, December 12, 1994
 * Christoph.Lameter@f961.n102.z1.fidonet.org or clameter@netcom.com (will soon expire)
 * 
 * Copyright: GPL
 */
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>

char *file_text;        /* Pointer to the verses in Memory */
char *p,*nl;       /* Pointers needed for searching text */
FILE *f;
int bytes;          /* Size of the versefile */
char sstring[10];   /* String to look for */
struct tm *local_time;
time_t now;
      
int main(int argc,char **argv)
   {	/* Compose String to look for */
	(void) time (&now);
	local_time = localtime (&now);
	sprintf(sstring,"R%02d%02d",local_time->tm_mon+1,local_time->tm_mday);
	if (argc!=1)
	  { fputs("verse takes no arguments!\n",stderr);exit (1);
	  }
	f=fopen(VERSE_LIB,"r");
	if (f==NULL)
	  { fputs("Cannot find Verse Library:" VERSE_LIB "\n",stdout);exit(1);
	  }
	/* Get the File into Memory */
	/* Gymnastics to get UNIX to tell us the filesize */
	fseek(f,0,SEEK_END);
	bytes=ftell(f);
	fseek(f,0,SEEK_SET);
	/* Read file into memory */
	file_text=malloc(bytes+1);
	file_text[bytes]=0; /* End Marker */
	if (fread(file_text,sizeof(char),bytes,f)!=bytes)
	  { fprintf(stderr,"Cannot read %d bytes from versefile\n",bytes);exit(1);
	  }
	fclose(f);
	/* Look for the String */
	for(p=file_text;*p!=0;p++)
	  {  nl=p+1;
	     while (*nl!='\n' && *nl!=0) nl++;
	     if (strncmp(p,sstring,5)==0)
	       {  /* Found a match !!! */
		  p+=5;
		  fwrite(p,sizeof(char),nl-p+1,stdout);
	       }
	     p=nl;
	  }
	/* Successful completion */
	return 0;
     }
