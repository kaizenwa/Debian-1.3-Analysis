/* Utility functions.

   Copyright (C) 1993, Eric Youngdale.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#ifdef USE_VARARGS
#include <varargs.h>
#else
#include <stdarg.h>
#endif
#include "utils.h"
#include <sys/stat.h>

#ifdef __svr4__
extern char *strdup(const char *);
extern int fileno(FILE *);
#endif

struct instruction ins, ins1;
char *program = "jumpkit";

const char *version_string = "2.17";


#ifdef LONG_FILENAMES
static int tested = 0;
#endif

#ifdef USE_VARARGS
void usage(va_alist)
  va_dcl
#else
void usage(char *format, ...)
#endif
{
  va_list vp;
#ifdef USE_VARARGS
  char *format;
#endif

  fprintf(stderr, "usage: %s", program);

#ifdef USE_VARARGS
  va_start(vp);
  format = va_arg (vp, char *);
#else
  va_start(vp, format);
#endif
  vfprintf(stderr, format, vp);
  va_end(va);

  fputc('\n', stderr);

  exit(1);
}

#ifdef USE_VARARGS
void error(va_alist)
  va_dcl
#else
void error(char *format, ...)
#endif
{
  va_list vp;
#ifdef USE_VARARGS
  char *format;
#endif

  fprintf(stderr, "\n%s: ", program);

#ifdef USE_VARARGS
  va_start(vp);
  format = va_arg (vp, char *);
#else
  va_start(vp, format);
#endif
  vfprintf(stderr, format, vp);
  va_end(va);

  fputc('\n', stderr);

  exit(1);
}

#ifdef USE_VARARGS
void xsystem(va_alist)
  va_dcl
#else
void xsystem(char *format, ...)
#endif
{
  va_list vp;
  static char command[4096];	/* Don't exceed the system limits */

#ifdef USE_VARARGS
  char *format;
  va_start(vp);
  format = va_arg (vp, char *);
#else
  va_start(vp, format);
#endif
  vsprintf(command, format, vp);
  va_end(va);

  if (system(command)) {
    perror ("system");
    error("error running '%s'\n", command);
  }
}

void xfree (void *ptr)
{
  if (ptr) free (ptr);
}

void *xmalloc(size_t size)
{
  void *result = malloc(size);
  if (!result)
    error("out ot memory\n");
  return result;
}

char *xstrdup(char *string)
{
  char *result = strdup(string);
  if (!result)
    error("out of memory\n");
  return result;
}

GLOBAL *readgbls(char *filedir, char *fileroot, int flags, char * allowed)
{
  char filename[256];
  char buffer[256];
  char name[256];
  char library[256];
  char objfile[256];
  char undefs[256];
  char type;
  char * cpnt, *cpnt1;
  GLOBAL *pnt = NULL, *head = NULL;
  int nargs;
  int size;
  int index = 1;
  int n_ignore = 0;
  u_int offset = 0;
  FILE *infile;
  FILE *testfile;
  struct stat infile_stat, testfile_stat;

#ifdef LONG_FILENAMES
  if (!tested){
    char filename1[256], filename2[256];
    strcpy(filename1, filedir);
    strcat(filename1, "jump.longfilenametest1");
    if ((infile = fopen(filename1,"w"))==NULL)
	error("filename %s test failed (%s)",filename1,strerror(errno));
    fprintf(infile,"foo");
    fclose(infile);
    strcpy(filename2, filedir);
    strcat(filename2, "jump.longfilenametest2");
    if ((infile = fopen(filename2,"r"))!=NULL)
        fclose(infile);
    remove(filename1);
    if(infile){
      printf("You must recompile the tools without the -DLONG_FILENAMES\n"); 
      printf("in Makefile before these tools will work on the minix filesystem\n");
      exit(1);
    };
    tested++;
  };
#endif

  strcpy(filename, filedir);
  strcat(filename, fileroot);
  infile = fopen(filename, "r");
  if (!infile) return NULL;

  testfile = fopen(fileroot,"r");
  if(testfile){
    fstat(fileno(infile), &infile_stat);
    fstat(fileno(testfile), &testfile_stat);
    fclose(testfile);

    if(testfile_stat.st_ino != infile_stat.st_ino ||
       testfile_stat.st_dev != infile_stat.st_dev) {
      printf("The JUMP_DIR environment variable points to %s, but there is\n",
	     filename);
      printf("a file %s in the current working directory.  These files are\n",
	     fileroot);
      printf("not the same, and I have no way of telling\n");
      printf("which file you really want me to use, so I will exit, and\n");
      printf("once you have either changed JUMP_DIR or removed the file in\n");
      printf("the current working directory you can try again.\n");
      exit(1);
    };
  };

  while (1)
    {
      fgets(buffer, sizeof(buffer), infile);
      if (feof(infile)) break;

      /* Strip any text after a # sign (i.e. comments) */
      cpnt = buffer;
      while(*cpnt){
	if(*cpnt == '#') {*cpnt = 0; break; };
	cpnt++;
      };

      /* Quietly ignore any lines that have just whitespace */
      cpnt = buffer;
      while(*cpnt){
	if(*cpnt != ' ' && *cpnt != '\t') break;
	cpnt++;
      };
      if (!(*cpnt) || *cpnt == '\n') {
	n_ignore++;
	continue;
      };

      /* See if we used the -o format - if so, skip over the filename label for now */

      cpnt = strchr(buffer,':');
      cpnt1 = strchr(buffer,' ');
      library[0] = 0;

      if(cpnt && (((int) cpnt) < ((unsigned int) cpnt1))) {
	if(strchr(cpnt+1,':')) { /* New style of nm output */
	  char * cpnt2;
	  *cpnt = 0;
	  cpnt1 = strrchr(buffer,'/');
	  if(cpnt1)
	    cpnt1++;
	  else
	    cpnt1 = buffer;
	  if(strcmp(cpnt-3,".sa") == 0){
	    cpnt -= 3;
	    *cpnt = 0;
	    cpnt += 3;
	  }
	  strcpy(library, cpnt1);
	  /* OK, now get the name of the library module */
	  cpnt++;
	  cpnt1 = strchr(cpnt, ':');
	  if(cpnt1) {
	    *cpnt1 = 0;
	    strcpy(objfile, cpnt);
	    cpnt1++;
	  }
	  nargs = sscanf(cpnt1, "%x %c %s", &size, &type, name);
	  nargs += 2;
	} else {
	  nargs = sscanf(cpnt+1, "%x %c %s %s %s", &size, &type, name, library, objfile);
	  *cpnt = 0;
	  if(nargs == 3 && (cpnt1 = strrchr(buffer, '('))){
	    cpnt = cpnt1;
	    *cpnt = 0;
	    if(strcmp(cpnt-3,".sa") == 0){
	      cpnt -= 3;
	      *cpnt = 0;
	      cpnt1 = strrchr(buffer, '/');
	      if(cpnt1)
		cpnt1++;
	      else
		cpnt1 = buffer;
	      strcpy(library, cpnt1);
	    }
	  }
	}
      } else {
	nargs = sscanf(buffer, "%x %c %s %s %s %s", &size, &type, name, library, objfile, undefs);
      };

      /* We need to do something to this line */
#if 0
      nargs = sscanf(cpnt, "%x %c %s %s %s", &size, &type, name, library, objfile);
#endif
 
     if ((nargs < 4 && !(flags &JUSTNM) && strcmp(name, PLACEHOLDER)) || 
	 nargs < 3)
	error("invalid symbol in '%s', line %d\n", filename, index + n_ignore);

      if ((flags & CHECKSIZE) && (!size) && strcmp(name, PLACEHOLDER))
	error("invalid size for symbol '%s'\n", name);

      if(strchr(allowed, type) == NULL && strchr(allowed,'*') == NULL && 
	 strcmp(name, PLACEHOLDER))
	error("invalid type for symbol '%s' (wrong file, or wrong type)\n",
	      name);

      /* These symbols should always be left alone */
      if (!(flags &JUSTNM) && type == 'T' &&
	  strncmp(name,"__GLOBAL_$",10) == 0 &&
	  name[11] == '$' && (name[10] == 'I' || name[10] == 'D')) 
	error("symbol '%s' should not appear in %s\n",name,filename);

      if (!head)
	{
	  head = xmalloc(sizeof(GLOBAL));
	  pnt = head;
	}
      else
	{
	  pnt->next = xmalloc(sizeof(GLOBAL));
	  pnt = pnt->next;
	}

      pnt->next = NULL;
      pnt->index = index++;
      pnt->type = type;
      pnt->offset = offset;
      pnt->size = size;
      offset += size;
      pnt->name = xstrdup(name);
      pnt->library = xstrdup(library);
      if(nargs > 4) pnt->objfile = xstrdup(objfile);
      else pnt->objfile = "";

      if(nargs > 5) pnt->override_undefs = xstrdup(undefs);
      else pnt->override_undefs = "";
    };

  fclose(infile);
    
  return head;
}

void vfyparams(char * filedir, char * name, unsigned int text,
	       unsigned int data, unsigned int jumpsize, unsigned int gotsize,
	       int major, int minor, int force){
  char filename[1024];
  char buffer[128];
  int goof, m1, m2, pl, nargs;
  char * pnt;
  FILE * infile;
  unsigned int numb;

  strcpy(filename, filedir);

  strcat(filename,JUMP_PARAMS);

  infile = fopen(filename,"r");
  goof = 1;  /* Force a rewrite of parameters */

  if(infile){
    goof = 0;
    
    while (1==1){
      fgets(buffer, sizeof(buffer), infile);
      if(feof(infile)) break;
      pnt = strchr(buffer,'\n');
      if(pnt) *pnt = 0;
      if(strncmp(buffer, "Name=", 5) == 0) {
	if(strcmp(name, buffer+5) != 0) {
	  printf("Warning: library name changed from %s to %s\n", buffer+5, name);
	  goof++;
	};
	continue;
      };
      
      if(strncmp(buffer, "Text=0x", 7) == 0){
	sscanf(buffer+7, "%x", &numb);
	if(numb != text) {
	  goof++;
	  printf("Warning: library text offset has changed from 0x%8.8x to 0x%8.8x\n",
		 numb, text);
	};
	continue;
      };
      
      if(strncmp(buffer, "Data=0x", 7) == 0){
	sscanf(buffer+7, "%x", &numb);
	if(numb != data) {
	  goof++;
	  printf("Warning: library data offset has changed from 0x%8.8x to 0x%8.8x\n",
		 numb, data);
	};
	continue;
      };
      
      if(strncmp(buffer, "Jump=0x", 7) == 0){
	sscanf(buffer+7, "%x", &numb);
	if(numb != jumpsize) {
	  goof++;
	  printf("Warning: library jumptable size has changed from 0x%8.8x to 0x%8.8x\n",
		 numb, jumpsize);
	};
	continue;
      };
      
      if(strncmp(buffer, "GOT=0x", 6) == 0){
	sscanf(buffer+6, "%x", &numb);
	if(numb != gotsize) {
	  goof++;
	  printf("Warning: library GOT size has changed from 0x%8.8x to 0x%8.8x\n",
		 numb, gotsize);
	};
	continue;
      };
      
      if(strncmp(buffer, "Version=", 8) == 0){
	nargs = sscanf(buffer+8, "%d.%d.%d", &m1, &m2, &pl);
	m2 = m2 * 100;
	if(nargs == 3) m2 = m2 + pl;
	if(m1 != major || m2 != minor) {
	  goof++;
	  printf("Warning: library version number has changed from %d.%d to %d.%d\n",
		 m1, m2 ,major, minor);
	};
	continue;
      };
      
    };
    fclose(infile);

    if(goof && !force) {
      printf("Use -f switch to force this program to record the new parameters\n");
      printf("as the correct ones.\n");
      exit(1);
    };
  };  /* Able to open input file */

  if(goof)
    printf("Recording new library parameters.\n");

  /* Now write out the current parameters */
#if 0
  infile = fopen(filename,"w");
#else
  if ((infile = fopen(filename,"w"))==NULL)
	error("Can't record new params in %s (%s)",filename,strerror(errno));
#endif
  fprintf(infile,"Name=%s\n",name);
  fprintf(infile,"Text=0x%8.8x\n",text);
  fprintf(infile,"Data=0x%8.8x\n",data);
  fprintf(infile,"Jump=0x%8.8x\n",jumpsize);
  fprintf(infile,"GOT=0x%8.8x\n",gotsize);
  fprintf(infile,"Version=%d.%d.%d\n",major, minor/100, minor%100);
#if 0
  if(minor % 100)
  else
    fprintf(infile,"Version=%d.%d\n",major, minor/100);
#endif
  fclose(infile);
}

