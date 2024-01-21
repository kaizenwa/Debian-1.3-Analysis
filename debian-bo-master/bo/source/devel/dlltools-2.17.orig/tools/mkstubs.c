/*
 * Program mkstubs.c.

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
 * Build stub libraries required to link user programs to a sharable library.
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

GLOBAL *guhead = NULL;		/* List of undefined symbols needed for lib*/

static char override[256];

#define MAX_UNDEFS 16

char * shlib_version = NULL;

/* Checks to see if a given library is in liblist.  If it is, remove it from
   the list, and return 1, otherwise return 0 */

int in_library(char * liblist, char * libname){
  char * pnt;
  int len = strlen(libname);
  pnt=liblist;
  do {
    if(strncmp(pnt, libname, len) == 0 &&
       (pnt[len] == 0 || pnt[len] == ',')) {
      strcpy(pnt, pnt+len + (pnt[len] == ',' ? 1 : 0));
      /* Remove this library from the list */
      return 1;
    };
    pnt = strchr(pnt, ',');
  } while (pnt++);
  return 0;

}


void main(int argc, char ** argv) {
  char *filedir;		/* location for all internal files */
  FILE *outfile;
  GLOBAL * ghead = NULL;	/* Global symbols to be diverted */
  char *imagename = NULL;
  char *imageroot = NULL;
  unsigned int origin = 0;
  unsigned int major = 0, minor = 0, patchlevel = 0;
  unsigned int jumpsize = 0x1000;
  unsigned int gotsize = 0;
  int force = 0;
  int nargs = 0;
  unsigned int data_origin = 0;
  unsigned int got_origin = 0;
  unsigned int data_address = 0;
  unsigned int last_data_address = 0;
  int nundefs = 0;
  int count, jcount;
  char *undefs[MAX_UNDEFS];
  char stubname[32];
  char objname[32];
  char symname[32];
  char symname1[64];
  int c;
  char *cp;
  int i, j;
  GLOBAL *gpnt, *gpnt1;

  printf("mkstubs v%s\n",version_string);
  program = argv[0];

  if (argc < 2)
    usage("-l image -v version -a address -j jumpsize stublibs ...");

  /* First read the command line arguments */
  while ((c = getopt(argc, argv, "fl:v:j:g:a:u:d:")) != EOF)
    switch (c)
      {
      case 'f':
	force = 1;
	break;
      case 'l':
	imagename = optarg;
	imageroot = strrchr(imagename, '/');
	if (!imageroot)
	  imageroot = imagename;
	else
	  imageroot++;
	break;
      case 'v':
	if ((nargs = sscanf(optarg, "%d.%d.%d", &major, &minor, &patchlevel)) < 2)
	  error("invalid version number '%s'", optarg);
	minor = minor * 100;
	if(nargs == 3) minor = minor + patchlevel;
	shlib_version = optarg;
	break;
      case 'a':
	origin = strtoul(optarg, &cp, 0);
	if (*cp || origin < 0x60000000 || (origin & 0xfff))
	  error("invalid load address '%s'", optarg);
	break;
	case 'd':
	  data_origin = strtoul(optarg, &cp, 0);
	  if (*cp || data_origin < 0x60000000 || (data_origin & 3))
	    error("invalid load address '%s'", optarg);
	  break;
      case 'j':
	jumpsize = strtoul(optarg, &cp, 0);
	if (*cp || !jumpsize || (jumpsize > 0x1000 && (jumpsize & 0xfff)))
	  error("invalid jump table size '%s'", optarg);
	break;
	case 'g':
	  gotsize = strtoul(optarg, &cp, 0);
	  if (*cp || !gotsize || (gotsize > 0x1000 && (gotsize & 0xfff)))
	    error("invalid GOT table size '%s'", optarg);
	  break;
      case 'u':
	if (nundefs >= MAX_UNDEFS)
	  error("too many undefined symbols");
	undefs[nundefs++] = optarg;
	break;
      default:
	error("invalid option '-%c'", c);
	exit(1);
      }

  if (!imagename)
    error("no image file specified");
  if (!origin)
    error("no load address specified");
  if (optind >= argc)
    error("no stub libraries specified");

#if 0
  if (*imagename != '/' && *imagename != '.')
    fprintf(stderr,"Warning - non-absolute pathname specified for library\n");
#endif

  filedir = getenv("JUMP_DIR");

  if (filedir)
    printf("Reading configuration files from %s\n",filedir);
  else
    fprintf(stderr,"Warning: JUMP_DIR environment variable not set - using cwd\n");

  if (!filedir)
    filedir = "";
  else
    if(filedir[strlen(filedir)-1] != '/') {
       char *ptr = xmalloc (strlen(filedir) + 2);
       filedir = strcpy (ptr, filedir);
       strcat(filedir, "/");
    }

  vfyparams(filedir, imagename, origin, data_origin, jumpsize, gotsize, major,
	    minor , force);

  guhead = readgbls(filedir, JUMP_UNDEFS, 2, "D");
  gpnt = guhead;
  while(gpnt){
    for(i=0;i<nundefs;i++)
      if(strcmp(undefs[i], gpnt->name) == 0) break;
    if(i == nundefs) {
      printf("Automatically adding %s as an undefined symbol to the\n stub libraries.\n", gpnt->name);
      undefs[nundefs++] = gpnt->name;
    };
    gpnt = gpnt->next;
  };

  ghead = readgbls(filedir, JUMP_FUNCTIONS, 0, "TtU");


  if (ghead)
    {
      gpnt = ghead;
      while (gpnt->next)
	gpnt = gpnt->next;
      gpnt->next = readgbls(filedir, JUMP_VARS, 1, "DCKU");
    }
  else
    ghead = readgbls(filedir, JUMP_VARS, 1, "DCKU");

  sprintf(symname, "___%s_%d_%d", imageroot, major, minor);

  /* This one is used to help out mkimage */
  sprintf(symname1, "__NEEDS_SHRLIB_%s_%d", imageroot, major);

  setbuf(stdout, NULL);

  data_address = 0;
  
  for (gpnt = ghead; gpnt; gpnt = gpnt->next)
    {
      if(data_origin && (gpnt->type == 'C' || gpnt->type == 'D')) {
	gpnt->offset = data_address; /* We have to recalculate */
	data_address += gpnt->size;
      };
    };
  
  last_data_address = 0;
  
  for (gpnt = ghead; gpnt; gpnt = gpnt->next)
    {
      if(gpnt->offset + gpnt->size > last_data_address)
	last_data_address = gpnt->offset + gpnt->size;
    };
  
  for (i = optind; i < argc; i++)
    {
      char *tmpname = "mkstubs.tmp";

      sprintf(stubname, "%s.sa", argv[i]);
      remove(stubname);
      printf("%s: ", stubname);

      printf("___%s_%d_%d, ", imageroot, major, minor);

      {
	if ((outfile = fopen(tmpname, "w"))==NULL)
		error("Can't open %s (%s)",tmpname,strerror(errno));
	fprintf(outfile, ".text\n");
	fprintf(outfile, "LCSHR01:\n\t.ascii\t\"%s.so.%d\\0\"\n", imagename, major);
#ifdef DLL
	if((minor % 100) == 0)
	  fprintf(outfile, "LCSHR02:\n\t.ascii\t\"DLL Jump %d.%d\\0\"\n", major, minor/100);
	else
	  fprintf(outfile, "LCSHR02:\n\t.ascii\t\"DLL Jump %d.%dpl%d\\0\"\n", major, minor/100, minor % 100);
#else
	if((minor % 100) == 0)
	  fprintf(outfile, "LCSHR02:\n\t.ascii\t\"Jump %d.%d\\0\"\n", major, minor/100);
	else
	  fprintf(outfile, "LCSHR02:\n\t.ascii\t\"Jump %d.%dpl%d\\0\"\n", major, minor/100, minor % 100);
#endif
	fprintf(outfile, ".data\n\t.align 4\n");
	fprintf(outfile, ".globl\t%s\n%s:\n", symname, symname);
	fprintf(outfile, ".globl\t%s\n%s:\n", symname1, symname1);
	fprintf(outfile, "\t.long LCSHR01\n");
	fprintf(outfile, "\t.long LCSHR02\n");
	fprintf(outfile, "\t.long 0x%8.8x\n", origin);
	fprintf(outfile, "\t.long 0x%8.8x\n", (major << 16) + minor);
	fprintf(outfile, "\t.stabs \"___SHARED_LIBRARIES__\",25,0,0,%s\n\n", symname);
	if(gotsize) {
	  if(!data_origin)
	    fprintf(outfile, "\t__%s_CONFLICT_LIST:\n\t.long 0x%8.8x\n",
		    imageroot,origin+jumpsize); 
	  else
	    fprintf(outfile, "\t__%s_CONFLICT_LIST:\n\t.long 0x%8.8x\n",
		    imageroot,data_origin - gotsize); 
	  
	  fprintf(outfile, "\t.stabs \"__SHARABLE_CONFLICTS__\",25,0,0,__%s_CONFLICT_LIST\n",imageroot); 
	};

	/*
	 * Add a special entry here so that global constructors are correctly
	 * run for global objects in the sharable library
	 */
	  fprintf(outfile, "\t.stabs \"___CTOR_LIST__\",20,0,0,0x%x\n",
		  origin + jumpsize-8); 

	count = 2;  /* One for pointer to sharable conflict list, second for
		       first entry in GOT, which is always NULL */
	jcount = 0;
	
	got_origin = origin + jumpsize;
	if(data_origin) got_origin = data_origin - gotsize;
	
	if(gotsize)
	  for (gpnt = ghead; gpnt; gpnt = gpnt->next)
	    {
	      if(strcmp(gpnt->name, PLACEHOLDER)){
		switch(gpnt->type){
		case 'T':
#if 0
		  fprintf(outfile, "\t.globl __GOT_%s\n", gpnt->name);
		  fprintf(outfile,"\t__GOT_%s = 0x%8.8x\n\n", gpnt->name,
			  origin+jumpsize+8+(jcount*4));
#endif
		  fprintf(outfile, "\t.globl __PLT_%s\n", gpnt->name);
		  fprintf(outfile, "__PLT_%s = 0x%8.8x\n", gpnt->name,
			  origin+8*gpnt->index);
		case 't':
		case 'U':
		  break;
		default:
		  fprintf(outfile, "\t.globl __GOT_%s\n", gpnt->name);
		  fprintf(outfile,"\t__GOT_%s = 0x%8.8x\n\n", gpnt->name,
			  got_origin+(count*4));
		}; /* switch */
	      };  /* Not placeholder */
	      if (gpnt->type == 'T' || gpnt->type == 't' || gpnt->type == 'U') 
		jcount++;
	      else
		count++;
	    };
	
	fprintf(outfile, " __GOT_SIZE = 0x%8.8x\n", gotsize);
	fprintf(outfile, " __PLT_SIZE = 0x%8.8x\n", jumpsize);
	fprintf(outfile, " __LAST_DATA_ADDRESS = 0x%8.8x\n",
		(data_origin ? data_origin : origin+jumpsize+gotsize)
		+last_data_address);

	fprintf(outfile, "\n");
	fclose(outfile);
	
	sprintf(objname, "__%s.o", imageroot);
	xsystem(PREFIX "as -o %s %s", objname, tmpname);
	
	remove(tmpname);
	xsystem(PREFIX "ar qc %s %s", stubname, objname);
	remove(objname);
      };
      
      for (gpnt = ghead; gpnt; gpnt = gpnt->next)
	{
	  if(gpnt->type == 'C') continue;
	  if (in_library(gpnt->library, argv[i]) && 
	      strcmp(gpnt->name, PLACEHOLDER))
	    {
	      override[0] = 0;
	      printf("%s, ", gpnt->objfile);

	      if ((outfile = fopen(tmpname, "w"))==NULL)
		error("Can't open %s (%s)",tmpname,strerror(errno));

	      for (gpnt1 = gpnt; gpnt1; gpnt1 = gpnt1->next)
		{
		  if(gpnt1->type == 'C') continue;
		  if (gpnt != gpnt1 && 
		      (strcmp(gpnt->objfile, gpnt1->objfile) ||
		      !in_library(gpnt1->library, argv[i]))) continue;
		  
		  if(strlen(gpnt1->override_undefs))
		    strcat(override, gpnt1->override_undefs);

		  if(strcmp(gpnt1->name, PLACEHOLDER) == 0) continue;
		  if((gpnt1->type >= 'a' && gpnt1->type <= 'z') ||
			gpnt1->type == 'U') continue;

		  fprintf(outfile, "\t.globl %s\n", gpnt1->name);
		  if (gpnt1->type == 'T') {
		    fprintf(outfile, "%s = 0x%8.8x\n", gpnt1->name,
			    origin+8*gpnt1->index);
		  } else {
		    if(data_origin && gpnt1->type != 'K') {
		      fprintf(outfile,"\t%s = 0x%8.8x\n\n", gpnt1->name,
			      data_origin+gpnt1->offset);
		    } else {
		      if(data_origin)
			fprintf(outfile,"\t%s = 0x%8.8x\n\n", gpnt1->name,
				origin+jumpsize+gpnt1->offset);
		      else
			fprintf(outfile,"\t%s = 0x%8.8x\n\n", gpnt1->name,
				origin+jumpsize+gotsize+gpnt1->offset);
		    };
		  };
		};
	      fprintf(outfile, ".stabs \"%s\",0,0,0,0\n\n", symname);

	      if (strlen(override) == 0){
		for (j = 0; j < nundefs; j++)
		  fprintf(outfile, "\t.stabs \"%s\",0,0,0,0\n", undefs[j]);
	      } else {
		char * cpnt1, *cpnt2;
		cpnt1 = override;
		while (*cpnt1){
		  cpnt2 = strchr(cpnt1, ':');
		  if (cpnt2) *cpnt2 = 0;
		  fprintf(outfile, "\t.stabs \"__NEEDS_SHRLIB_%s\",0,0,0,0\n", cpnt1);
		  if(cpnt2) cpnt1 = cpnt2 + 1;
		  else *cpnt1 = 0;
		};
	      };
	      fclose(outfile);

	      sprintf(objname, "__%c%5.5d.o", gpnt->type, gpnt->index);
	      xsystem(PREFIX "as -o %s %s", objname, tmpname);
	      remove(tmpname);
	      xsystem(PREFIX "ar q %s %s", stubname, objname);
	      remove(objname);
	    }
	};

      /* Now go through and add a separate .o file for each common
	 symbol in the library.  These appear in separate .o files because
	 there can be more than one "definition" of a common symbol, but not of
	 an absolute symbol.  This essentially works around the problem */

      for (gpnt = ghead; gpnt; gpnt = gpnt->next)
	{
	  if (in_library(gpnt->library, argv[i]) && 
	      strcmp(gpnt->name, PLACEHOLDER))
	    {
	      printf("%s, ", gpnt->name);
	      if ((outfile = fopen(tmpname, "w"))==NULL)
		error("Can't open %s (%s)",tmpname,strerror(errno));

	      if(strcmp(gpnt->name, PLACEHOLDER) == 0) continue;
	      if(gpnt->type != 'C') continue;
	      
	      fprintf(outfile, "\t.globl %s\n", gpnt->name);
	      if(data_origin)
		fprintf(outfile,"\t%s = 0x%8.8x\n\n", gpnt->name,
			data_origin+gpnt->offset);
	      else
		fprintf(outfile,"\t%s = 0x%8.8x\n\n", gpnt->name,
			origin+jumpsize+gotsize+gpnt->offset);

	      fprintf(outfile, ".stabs \"%s\",0,0,0,0\n\n", symname);
	      
	      fclose(outfile);
	      sprintf(objname, "__%c%5.5d.o", gpnt->type, gpnt->index);
	      xsystem(PREFIX "as -o %s %s", objname, tmpname);
	      remove(tmpname);
	      xsystem(PREFIX "ar q %s %s", stubname, objname);
	      remove(objname);
	    };
	}

      /* OK, we are done now.  Run ranlib to fix the library and exit. */

      printf("sequencing, ");
      xsystem(PREFIX "ar s %s", stubname);

      printf("done\n");
    }

    exit(0);
}
