/*
 * Program getsize.c.

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


/*
 * Initial version 1.0: Eric Youngdale 12/6/92
 * Gratuitous changes by David Engel: 12/12/92
 * Version 1.2: Eric Youngdale 1/31/92 - Add support for DLL
 *
 * Determine the actual size of each data variable in the program.  Spit out
 * an equivalent copy of jump.vars with actual variable sizes listed.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef linux
#include <getopt.h>
#else

#ifdef __svr4__
#include <stdlib.h>
#else
extern int optind;
extern char *optarg;
extern int getopt (int __argc, char **__argv, char *__optstring);
#endif

#endif

#include "utils.h"

char * filedir = NULL; /* location for all internal files */

FILE * gbldata = NULL;
FILE * outfile = NULL;
FILE * infile = NULL;

GLOBAL * gdhead = NULL;		/* Global symbols to be diverted */
GLOBAL * gfhead = NULL;		/* List of global functions */

GLOBAL * gfsize = NULL;		/* List of global functions */

char command[16384];
char *libname;
unsigned int origin = 0;
unsigned int major, minor;
unsigned int jumpsize = 0x1000;

void main(int argc, char ** argv)
{
  char filename[256];
  GLOBAL *gpnt;

  program = argv[0];

  fprintf(stderr,"getsize v%s\n",version_string);
  filedir = getenv("JUMP_DIR");
  if (!filedir) {
    fprintf(stderr,"Warning: JUMP_DIR environment variable not set - using cwd\n");
    filedir = "";
  } else
    if(filedir[strlen(filedir)-1] != '/') {
       char *ptr = xmalloc (strlen(filedir) + 2);
       filedir = strcpy (ptr, filedir);
       strcat(filedir, "/");
    }
		 

  gdhead = readgbls(filedir, JUMP_VARS, 0, "DCKU");
  gfhead = readgbls(filedir, JUMP_FUNCTIONS, 0, "TtU");

  /*
   * Next generate the jump table itself.
   */
  if ((outfile = fopen("size.s","w"))==NULL)
	error("Can't open size.s (%s)",strerror(errno));
  fprintf(outfile,"\t.text\n");

  /*
   * Next generate the file that contains the global variables.
   */
  for (gpnt = gdhead; gpnt; gpnt = gpnt->next)
    {
      if(strcmp(gpnt->name, PLACEHOLDER)){
	GVAR_FILENAME(filename, filedir, gpnt);
	fprintf(stderr,"%s\n",filename);
	gbldata = fopen(filename, "r");
	if (!gbldata)
	  {
	    GCMN_FILENAME(filename, filedir, gpnt);
	    fprintf(stderr,"%s\n",filename);
	    gbldata = fopen(filename, "r");
	    if (!gbldata)
	      error("no source file for symbol '%s'", gpnt->name);
	  };
	if (gbldata)
	  {
	    while (1)
	      {
		char buffer[16384];
		fgets(buffer, sizeof(buffer), gbldata);
		if (feof(gbldata))
		  break;
		fputs(buffer, outfile);
	      };
	    fprintf(outfile, "\n");
	    fclose(gbldata);
	  }
      };
    };
  fprintf(outfile,".globl __done\n__done:\n");

  fclose(outfile);

  xsystem(PREFIX "as -o size.o size.s");
  xsystem(PREFIX "nm " NO_CPLUS " size.o | grep \" T \" | sort > size.nm");
  remove("size.o");
  remove("size.s");

  if ((infile = fopen("size.nm", "r"))==NULL)
	error("Can't open size.nm (%s)", strerror(errno));
  {
    char buffer[16384];
    char name[256];
    char last_name[256];
    char type;
    int address, last_address;
    last_address =0;
    while(1==1){
      fgets(buffer, sizeof(buffer), infile);
      if (feof(infile))
	break;
      sscanf(buffer,"%x %c %s",&address, &type, name);
      if(address) {
	for (gpnt = gdhead; gpnt; gpnt = gpnt->next)
	  if(strcmp(gpnt->name, last_name) == 0) {
	    if(gpnt->size && gpnt->size < address-last_address)
	      printf("** Overflow **");
	    printf("%8.8x %c %-20s %-14s %s\n",address-last_address, gpnt->type, last_name, gpnt->library, gpnt->objfile);
	    break;
	  };
	if(!gpnt)
	  printf("%8.8x D %s\n",address-last_address, last_name);

      };
      last_address = address;
      strcpy(last_name, name);
    };
  };
  exit(0);
}
