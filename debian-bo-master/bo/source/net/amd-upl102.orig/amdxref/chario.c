/*
This software is provided as-is, with no warranties for suitability of
use or support implied.  This package makes no guarantees that it will
perform in any manner.  The authors and Texas A&M University accept no
liability for any damages incurred while using this software.

This software may be copied, redistributed, and updated in any
fashion as long as this comment header is not removed or altered.

Douglas Lee Schales
Doug.Schales@sc.tamu.edu
Texas A&M University
Supercomputer Center

01/26/1993
*/
#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <string.h>
#include <malloc.h>
#include "stdunix.h"

#include "chario.h"

struct file_str {
     struct file_str *next;
     dev_t dev;
     ino_t inode;
     FILE *fp; /* NULL for internal buffer pointed to by buffer */
     int linenum;
     char *filename;
     unsigned char *buffer;
     int bufptr;
};

static struct file_str *current = (struct file_str *)0;
static struct file_str *tail = (struct file_str *)0;

int __lastchar;
int __pushed = 0;

int
fetchar(void)
{
     int c = EOF;

     if(__pushed){
	  c = __lastchar;
	  __pushed = 0;
	  return c; /* short circuit to avoid line inc */
     }
     else do {
	  struct file_str *old = current;
	  if(!current)
	       return EOF;
	  else if(!current->fp){
	       if(current->buffer[current->bufptr])
		    c = current->buffer[current->bufptr++];
	  }
	  else if(current->fp){
	       c = getc(current->fp);
	  }

	  if(c == EOF){
	       if(current->fp){
		    if(current->filename)
			 free(current->filename);
		    fclose(current->fp);
	       }
	       current = current->next;
	       free(old);
	       if(!current){
		    tail = 0;
		    break;
	       }
	  }
     } while(c == EOF);

     if(c == '\n')
	  current->linenum++;
     return c;
}

int
getlinenum(void)
{
     if(current)
	  return current->linenum;
     else
	  return -1;
}

char *
getfilename(void)
{
     if(current)
	  return current->filename;
     else
	  return "*last*";
}

int
addmembuf(char *buffer, char *name)
{
     struct file_str *fs;

     fs = (struct file_str *)malloc(sizeof(struct file_str));
     fs->linenum = 1;
     fs->buffer = (unsigned char *)buffer;
     fs->bufptr = 0;
     fs->fp = (FILE *)0;
     fs->filename = name;
     fs->next = (struct file_str *)0;
     if(tail)
	  tail->next = fs;
     tail = fs;
     if(!current)
	  current = fs;
     return NOERR;
}

int
includemembuf(char *buffer, char *name)
{
     struct file_str *fs;

     fs = (struct file_str *)malloc(sizeof(struct file_str));
     fs->linenum = 1;
     fs->buffer = (unsigned char *)buffer;
     fs->bufptr = 0;
     fs->fp = (FILE *)0;
     fs->filename = name;
     fs->next = current;
     if(!tail)
	  tail = fs;
     current = fs;
     return NOERR;
}

int
includefile(char *filename)
{
     struct stat statbuf;
     struct file_str *rove, *fs;

     if(stat(filename, &statbuf) == -1){
	  return FILEERR;
     }
     for(rove=current;rove;rove=rove->next)
	  if(rove->dev == statbuf.st_dev &&
	     rove->inode == statbuf.st_ino)
	       return RECURSE;
     fs = (struct file_str *)malloc(sizeof(struct file_str));
     if(!(fs->fp = fopen(filename, "r"))){
	  free(fs);
	  return FILEERR;
     }
     else {
	  fs->dev = statbuf.st_dev;
	  fs->inode = statbuf.st_ino;
	  fs->linenum = 1;
	  fs->filename = strdup(filename);
	  fs->buffer = 0;
	  fs->next = current;
	  if(!tail)
	       tail = fs;
	  current = fs;
     }
     return NOERR;
}

int
addfile(char *filename)
{
     struct stat statbuf;
     struct file_str *fs;

     if(stat(filename, &statbuf) == -1){
	  return FILEERR;
     }
     fs = (struct file_str *)malloc(sizeof(struct file_str));
     if(!(fs->fp = fopen(filename, "r"))){
	  free(fs);
	  return FILEERR;
     }
     else {
	  fs->dev = statbuf.st_dev;
	  fs->inode = statbuf.st_ino;
	  fs->linenum = 1;
	  fs->filename = strdup(filename);
	  fs->buffer = 0;
	  fs->next = 0;
	  if(tail)
	       tail->next = fs;
	  tail = fs;
	  if(!current)
	       current = fs;
     }
     return NOERR;
}
