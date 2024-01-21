/*
 * Program mkimage.c.

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
 * Perform steps require to link image for sharable library with DLL including
 * building the jump table and arranging all of the global data such that it 
 * all appears at the same address from one version to the next.
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
char outfile_name[256];

GLOBAL * gdhead = NULL;		/* Global symbols to be diverted */
GLOBAL * gfhead = NULL;		/* List of global functions */
#ifndef IMPLIED_IMPORT
GLOBAL * gphead = NULL;		/* List of symbols to be imported */
#else
GLOBAL * gihead = NULL;		/* List of symbols to be ignored */
#endif

char command[16384];
char *libname;
unsigned int origin = 0;
unsigned int data_origin = 0; /* Optional  */
unsigned int major, minor, patchlevel;
unsigned int jumpsize = 0x1000;
unsigned int gotsize = 0;
unsigned int data_address = 0;
unsigned int size_used;

char * shlib_version = NULL;

#define OMAGIC 0407		/* ...object file or impure executable.  */
#define NMAGIC 0410		/* Code indicating pure executable.  */
#define ZMAGIC 0413		/* Code indicating demand-paged executable.  */
#define QMAGIC 0314
#define N_MAGIC(exec) ((exec).a_info & 0xffff)

/* Determines whether the linker defaults (silently) to qmagic or not */
int 
is_q_magic(void)
{
  FILE * exec;
  unsigned int magic;

#if 0
  xsystem(PREFIX "as -o foob.o /dev/null");
  xsystem(PREFIX "ld -o foob foob.o");
#else
  xsystem("echo .: |"PREFIX"as -o foob.o");
  xsystem(PREFIX "ld -o foob foob.o");
#endif
  xsystem("rm foob.o");

  exec = fopen("foob","r");
  fread(&magic, sizeof(magic), 1, exec);
  fclose(exec);
  xsystem("rm foob");
#ifdef HOST_TARGET_ENDIANESS_DIFFERS
  switch(md_chars_to_number (&magic, sizeof(magic)) & 0xffff) {
#else
  switch(magic & 0xffff) {
#endif
  case QMAGIC:
    return 1;
    break;
  case ZMAGIC:
    return 0;
    break;
  default:
    fprintf(stderr,"Binary format not a.out ZMAGIC or QMAGIC\n");
    fprintf(stderr,"Magic = %x\n", magic);
    exit(0);
  }
  fprintf(stderr,"Unable to determine default binary format\n");
  exit(0);
}


void main(int argc, char ** argv)
{
  char filename[256];
  char wrapup[65536];
  int c;
  int force;
  char *cp;
  char *libroot = NULL;
  int i;
  int debuglib = 0;
  int qmagic_library = -1;
  int nargs;
  int fixup_list;
  GLOBAL *gpnt, *gpnt1;
  wrapup[0] = 0;
  fixup_list = 0;

  printf("mkimage v%s\n",version_string);
  program = argv[0];
  force = 0;

  if (argc < 2)
    usage("-l image -v version -a address -j jumpsize linkargs ...");

  /* First read the command line arguments */
  while ((c = getopt(argc, argv, "Gfv:j:g:l:a:d:")) != EOF)
    {
      switch (c)
	{
	case 'f':
	  force = 1;
	  break;
	case 'G':
	  debuglib = 1;
	  break;
	case 'l':
	  libname = optarg;
	  libroot = strrchr(libname, '/');
	  if (!libroot)
	    libroot = libname;
	  else
	    libroot++;
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
	  if (*cp || origin < 0x60000000 || (origin & 3))
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
	default:
	  error("invalid option '-%c'", c);
	  exit(1);
	}
    }

  if (!libname)
    error("no image file specified");
  if (!origin)
    error("no load address specified");

#if 0
  if (*libname != '/' && *libname != '.')
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

  vfyparams(filedir, libname, origin, data_origin, jumpsize, gotsize, major,
	    minor , force);

  gdhead = readgbls(filedir, JUMP_VARS, CHECKSIZE, "DCKU");
  gfhead = readgbls(filedir, JUMP_FUNCTIONS, 0, "TtU");

#ifndef IMPLIED_IMPORT
  gphead = readgbls(filedir, JUMP_IMPORT, JUSTNM, "*");
#else
  gihead = readgbls(filedir, JUMP_IGNORE, 0, "*");
#endif

   for (i = optind; i < argc; i++)
     {
       if(strcmp(argv[i], "-qmagic") == 0) 
	 {
	   qmagic_library = 4096 - 32; /*number of text bytes we add in padding*/
	   origin -= 4096;
	   break;
       } else
	 if(strcmp(argv[i], "-noqmagic") == 0) 
	   {
	     qmagic_library = 0; /*number of text bytes we add in padding*/
	     break;
	   }
     }

  if (qmagic_library == -1) {
    if(is_q_magic()) {
      qmagic_library = 4096 - 32; /*number of text bytes we add in padding*/
      origin -= 4096;
    } else qmagic_library = 0;
  }

  /*
   * Next generate the jump table itself.
   */
  sprintf(outfile_name,"%s__jump.s",filedir);
  if ((outfile = fopen(outfile_name,"w"))==NULL)
	error("Can't open %s (%s)",outfile_name,strerror(errno));
  fprintf(outfile,"\t.text\n");
  if(qmagic_library)
    fprintf(outfile,"\t.fill %d,1,0\n", qmagic_library);
  fprintf(outfile,"\t.long %d\n\n", (major << 16) + minor);
  size_used = 8;

  for (gpnt = gfhead; gpnt; gpnt = gpnt->next)
    {
      fprintf(outfile, "\t.align\t3\n");
      if(strcmp(gpnt->name, PLACEHOLDER)){
	size_used += 8;
	if(gpnt->type < 'a') {
	  fprintf(outfile, "\t.globl %s\n",gpnt->name);
	  fprintf(outfile, "%s:\n",gpnt->name);
	};
	if (gpnt->type == 'U') {
	  fprintf(outfile, "\tjmp\t___libc_undefined\n\n");
	} else
	  if(!debuglib || gpnt->type >= 'a')
	    fprintf(outfile, "\tjmp\t%s__LOCAL__\n\n",gpnt->name);
	  else
	    fprintf(outfile, "\tjmp\t%s__INTRM__\n\n",gpnt->name);
      } else {
	fprintf(outfile, "\t.space 8\n");
      };
    };

  if(size_used > jumpsize - 8)
    error("Jump table overflow.");

  /*
   * Add a call to ___main in the last jump table slot.  This is used to
   * call the global constructors and destructors.
   */

  fprintf(outfile,"\t.org 0x%8.8x\n", jumpsize - 8 + qmagic_library);
  fprintf(outfile, "\tjmp\t___main\n\n");


  /*
   * Next generate the GOT that contains the pointers to global functions.
   */
  size_used = 8;
  if(gotsize){
    if(data_origin) {
      fprintf(outfile,"\t.data\n");
      fprintf(outfile,"\t.org 0\n");
      if((data_origin - gotsize) & 0xfff)
	fprintf(outfile,"\t.space %d\n",
		(data_origin - gotsize) & 0xfff);
    }
    else
      fprintf(outfile,"\t.org 0x%8.8x\n", jumpsize + qmagic_library);
    fprintf(outfile,"\t.long __SHARABLE_CONFLICTS__\n");
#if 0
    fprintf(outfile,".globl _GLOBAL_OFFSET_TABLE_\n");
#endif
    /* Store magic number used by DLL code to make sure this is a real list */

    fprintf(outfile,"_GLOBAL_OFFSET_TABLE_:\n");
    fprintf(outfile,"\t.long 0xfeeb1ed3\n");
#if 0
    for (gpnt = gfhead; gpnt; gpnt = gpnt->next)
      {
	if(strcmp(gpnt->name, PLACEHOLDER)){
	  fprintf(outfile, "\t.globl __GOT_%s\n",gpnt->name);
	  fprintf(outfile, "__GOT_%s:\n",gpnt->name);
	  fprintf(outfile,"\t.long %s\n", gpnt->name);
	} else {
	  fprintf(outfile,"\t.space 4\n");
	};
      };
    /*
     * Next generate the GOT that contains the pointers to global variables.
   */
    fprintf(outfile,"\t.org 0x%8.8x\n", jumpsize + (jumpsize >> 1));
#endif
    for (gpnt = gdhead; gpnt; gpnt = gpnt->next)
      {
	size_used += 4;
	if(strcmp(gpnt->name, PLACEHOLDER)){
	  fprintf(outfile,".globl __GOT_%s\n", gpnt->name);
	  fprintf(outfile,"__GOT_%s:\n", gpnt->name);
	  fprintf(outfile,"\t.long %s\n", gpnt->name);
	} else {
	  fprintf(outfile,"\t.space 4\n");
	};
      }
#if 0
    jumpsize += jumpsize >> 1;
#endif
  };

  if(size_used > gotsize)
    error("GOT table overflow.");

   if(data_origin)
     fprintf(outfile,"\t.text\n");

  /*
   * Next generate the file that contains the global variables.
   */
  data_address = 0;

  if(data_origin) {
    data_address = gotsize;
    if((data_origin - gotsize) & 0xfff)
      data_address = (gotsize + 0xfff) & 0xfffff000;
  };

  for (gpnt = gdhead; gpnt; gpnt = gpnt->next)
    {
      if(data_origin && gpnt->type != 'K') {
	gpnt->offset = data_address; /* We have to recalculate */
	data_address += gpnt->size;
      };
    };

  for (gpnt = gdhead; gpnt; gpnt = gpnt->next)
    {
      if(strcmp(gpnt->name, PLACEHOLDER)){
	GVAR_FILENAME(filename, filedir, gpnt);
	gbldata = fopen(filename, "r");
	if (!gbldata)
	  {
	    GCMN_FILENAME(filename, filedir, gpnt);
	    gbldata = fopen(filename, "r");
	    if (!gbldata)
	      error("no source file for symbol '%s'", gpnt->name);
	  };
	if (gbldata)
	  {
	    if(data_origin){
	      if(gpnt->type == 'K') {
		fprintf(outfile,"\t.text\n");
		fprintf(outfile,"\t.org 0x%8.8x\n", jumpsize + gpnt->offset + 
			qmagic_library);
	      } else {
		fprintf(outfile,"\t.data\n");
		fprintf(outfile,"\t.org 0x%8.8x\n", gpnt->offset);
	      };
	      } else
	      fprintf(outfile,"\t.org 0x%8.8x\n", jumpsize+gotsize+gpnt->offset+qmagic_library);
	    while (1)
	      {
		char buffer[16384];
		int i;
		fgets(buffer, sizeof(buffer), gbldata);
		if (feof(gbldata))
		  break;

		/* Make a simple consistency check (i.e. make sure the data
		   files are not scrambled */

		i = strlen(buffer);
		if(buffer[0] == '_') {
		  if(buffer[i-1] == '\n' && buffer[i-2] == ':' &&
		     strncmp(buffer, gpnt->name, i-2))
		    error("Data files mixed up for variable %s\n",gpnt->name);
		};
		    
		if(buffer[0] == '\t' && buffer[1] >= 'a' && buffer[2] <= 'z')
		  error("Machine instructions found in declaration for variable %s\n", gpnt->name);

#ifdef DLL       
		/* Now check to see if we need fixups here. */
		if (strncmp(buffer,"\t.long",6) == 0)
		  {
		    char var[256];
		    char scra[256];
#ifdef IMPLIED_IMPORT
		    char * cpnt;
#endif
		    int fixflag = 0;
		    strcpy(var, buffer+7);
		    var[strlen(var)-1] = 0; /* add trailing NULL */

		    /* We must also handle the case of _foo+3 or _foo-3 */
		    cp = strchr(var, '+');
		    if(cp) *cp =0;
		    cp = strchr(var, '-');
		    if(cp) *cp =0;

#ifdef IMPLIED_IMPORT
		    /* Check to see if this is a simple constant.  If so, then
		       pass on it */
		    cpnt = var;
		    while(*cpnt && *cpnt >= '0' && *cpnt <= '9') cpnt++;
		    
		    if((var[0] != '0' || var[1] != 'x') && *cpnt &&
		       strncmp(var,"___STV_",7) != 0) {
#endif
		    gpnt1 = gdhead; /* See if this is an exported symbol */
		    while (gpnt1)
		      {
			if (strcmp(var, gpnt1->name) == 0)
			  {
			    fixflag = 1;
			    break;
			  };
			gpnt1 = gpnt1->next;
		      };
		    if (!fixflag){
#ifdef IMPLIED_IMPORT
		      if(strncmp(var,"__GOT_",6) != 0){
			gpnt1 = gfhead;  /* Global function list */
			while (gpnt1)
			  {
			    if (strcmp(var, gpnt1->name) == 0) break;
			    gpnt1 = gpnt1->next;
			  };
			
			if(!gpnt1){
			  gpnt1 = gihead;  /* Ignore list */
			  while (gpnt1)
			    {
			      if (strcmp(var, gpnt1->name) == 0) break;
			      gpnt1 = gpnt1->next;
			    };
			};
			if(!gpnt1) fixflag = 1;
#ifdef DEBUG_IMPLIED
			if(fixflag) printf("Variable %s is assumed to be imported\n",var);
#endif
		      };
#else
		      gpnt1 = gphead; /* See if this is an imported symbol */
		      while (gpnt1)
			{
			  if (strcmp(var, gpnt1->name) == 0)
			    {
			      fixflag = 1;
			      break;
			    };
			  gpnt1 = gpnt1->next;
			};
#endif
		    };
		    if (fixflag)
		      {
			sprintf(scra, "___STD_%5.5d:\n",fixup_list);
			fputs(scra, outfile);
			if(fixup_list == 0){
			  strcpy(wrapup, ".data\n");
			  sprintf(scra, "___fixup_list:\n");
			  strcat(wrapup, scra);
			  strcat(wrapup, "\t.stabs \"__BUILTIN_FIXUPS__\",25,0,0,___fixup_list\n");
			};
			sprintf(scra, "\t.long __GOT_%s\n",var);
			if(strlen(wrapup) + strlen(scra) > sizeof(wrapup))
			  error("Variable wrapup overfilled\n");
			strcat(wrapup, scra);
			sprintf(scra, "\t.long ___STD_%5.5d\n",fixup_list++);	      
			strcat(wrapup, scra);
		      };
#ifdef IMPLIED_IMPORT
		  };
#endif
		  };
#endif  /* DLL */
		fputs(buffer, outfile);
	      };
	    fprintf(outfile, "\n");
	    fclose(gbldata);
	  }
      };
    };


#ifdef DLL
  if(fixup_list){
    fputs(wrapup, outfile);
    fputs("\t.long 0\n", outfile);
  };      
#endif

  fprintf(outfile, "\t.align\t12\n");

  /* This special symbol is needed to pass the information to crt0 about what
     symbols need to have addresses updated in the tables */
  if(gotsize)
    fprintf(outfile, "\t.stabs \"__SHARABLE_CONFLICTS__\",%d,0,0,_GLOBAL_OFFSET_TABLE_\n", (data_origin? 25: 23));

  /* If this is a debugging version of the sharable library, we add extra stuff
     here. */
  if(debuglib){
    fprintf(outfile,".text\n");
    for (gpnt = gfhead; gpnt; gpnt = gpnt->next)
      {
	fprintf(outfile, "\t.align\t3\n");
	if(strcmp(gpnt->name, PLACEHOLDER)){
	  if(gpnt->type < 'a' && gpnt->type != 'U') {
	    fprintf(outfile, "%s__LABL__:\n",gpnt->name);
	    fprintf(outfile,"\t.ascii \"Calling %s\\n\"\n", gpnt->name);
#ifdef __mc68000__
	    fprintf (outfile, "\t.even\n");
#endif
	    fprintf(outfile, "%s__INTRM__:\n",gpnt->name);
#if defined (__i386__)
	    fprintf(outfile,"\tpushl %%eax\n\tpushl %%ebx\n\tpushl"
		    " %%ecx\n\tpushl %%edx\n");
	    fprintf(outfile,"\tmovl $4,%%eax\n");
	    fprintf(outfile,"\tmovl $2,%%ebx\n");
	    fprintf(outfile,"\tmovl $%s__LABL__,%%ecx\n", gpnt->name);
	    fprintf(outfile,"\tmovl $%d,%%edx\n",strlen(gpnt->name)+9);
	    fprintf(outfile,"\tint $0x80\n");
	    fprintf(outfile,"\tpopl %%edx\n\tpopl %%ecx\n\tpopl"
		    " %%ebx\n\tpopl %%eax\n");
#elif defined (__mc68000__)
	    fprintf (outfile, "\tmoveml d2/d3,sp@-\n");
	    fprintf (outfile, "\tmovel #4,d0\n");
	    fprintf (outfile, "\tmovel #2,d1\n");
	    fprintf (outfile, "\tmovel #%s__LABL__,d2\n", gpnt->name);
	    fprintf (outfile, "\tmovel #%d,d3\n", strlen (gpnt->name) + 9);
	    fprintf (outfile, "\ttrap #0\n");
	    fprintf (outfile, "\tmoveml sp@+,d2/d3\n");
#endif
	    fprintf(outfile, "\tjmp\t%s__LOCAL__\n\n",gpnt->name);
	  };
	};
      };
    
  }
  fclose(outfile);

  xsystem(PREFIX "as -o %s__jump.o %s__jump.s", filedir, filedir);

  if(data_origin)
    sprintf(command, PREFIX "ld -x -Ttext %8.8x -Tdata %8.8x -o %s.so.%s %s__jump.o",
	    origin, (data_origin-gotsize) & 0xfffff000, libroot,
	    shlib_version, filedir);
  else
    sprintf(command, PREFIX "ld -x -Ttext %8.8x -o %s.so.%s %s__jump.o",
	    origin, libroot, shlib_version, filedir);

  for (i = optind; i < argc; i++)
    {
      strcat(command, " ");
      strcat(command, argv[i]);
    };
  printf("executing:%s\n",command);
  xsystem(command);
  xsystem("rm %s__jump.o", filedir);

  sprintf(command, PREFIX "nm " NO_CPLUS " %s.so.%s | grep \"D __NEEDS_SHRLIB_\" > %s%s; exit 0",
	  libroot, shlib_version, filedir, JUMP_UNDEFS);
  xsystem(command);

  printf("This library requires that the following shared libraries also be loaded:\n");

  sprintf(command, "cat %s%s", filedir, JUMP_UNDEFS);
  xsystem(command);

  printf("\n\n");

  exit(0);
}
