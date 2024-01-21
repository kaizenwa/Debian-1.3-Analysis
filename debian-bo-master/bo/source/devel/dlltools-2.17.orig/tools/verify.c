/*
 * Program verify.c.

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
 * Verify that the pair of shared library/stub library are consistent with each
 * other.
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

#if 0
#ifdef linux
#include <a.out.h>
#else
#include "../a.out.h"
#endif
#endif

#include <linux/a.out.h>

#include <stdio.h>

#include "utils.h"

#ifdef HOST_TARGET_ENDIANESS_DIFFERS
#include "md.h"
#endif

FILE * imagefile;
FILE * textfile1, *textfile2;


char *libname;
char * stubname;
char *libroot = NULL;
char command[1024];
char buffer[1024];
char name[1024];
char name1[1024];
struct exec header;
unsigned int base;
unsigned int unstripped;

int main(int argc, char ** argv)
{
  int i;
  int c;
  int flag;
  unsigned int address;
  char type;
  printf("verify v%s (experimental)\n",version_string);

  if (argc < 2)
    usage("-l sharableimage stublib1 stublib2....");


  while ((c = getopt(argc, argv, "l:j:")) != EOF)
    {
      switch (c)
	{
	case 'l':
	  libname = optarg;
	  libroot = strrchr(libname, '/');
	  if (!libroot)
	    libroot = libname;
	  else
	    libroot++;
	  break;
	default:
	  error("invalid option '-%c'", c);
	  exit(1);
	}
    }
  if (!libname)
    error("no sharable library specified");

  imagefile = fopen(libname,"rb");

  if(!imagefile)
    error("Unable to open %s\n",libname);

  fread((char *) &header, sizeof(header), 1, imagefile);
  fclose(imagefile);

#ifdef HOST_TARGET_ENDIANESS_DIFFERS
  md_chars_to_hdr ((unsigned char *) &header, &header);
#endif

  base = header.a_entry;  /* This is the base for the sharable image */
  unstripped = header.a_syms;  /* If stripped this is 0 */

  /* Give the canonical warning */
  printf("Used address range 0x%x-0x%x be aware ! must be unique !\n",
	 header.a_entry, header.a_entry + header.a_text + header.a_data + 
	 header.a_bss);

  if(!unstripped){
    printf("The sharable image has been stripped - it is no longer possible\n");
    printf("to verify the stubs and the image\n");
    exit(1);
  };

  /* Next, we need to get the symbols from the stub libraries  */

  strcpy(command, PREFIX "nm " NO_CPLUS " ");
  for (i = optind; i < argc; i++)
    {
      strcat(command, " ");
      strcat(command, argv[i]);
    };
  strcat(command," | grep -v \"__PLT\" | grep \" A \" |  sort | uniq > verify.stub");
  xsystem(command);

  if ((textfile1 = fopen("verify.stub","r"))==NULL)
	error("Can't open verify.stub (%s)",strerror(errno));
  while(1==1){
    fgets(buffer, sizeof(buffer), textfile1);
    if (feof(textfile1))
      break;
    sscanf(buffer,"%x %c %s",&address, &type, name);
  };
  fclose(textfile1);

  /* Now get the symbol table from the sharable image itself. */
  strcpy(command, PREFIX "nm " NO_CPLUS " ");
  strcat(command, libname);
  strcat(command, " | grep -v \" A \" | grep -v \"__STV\" | sort > verify2.tmp");
  xsystem(command);


  textfile1 = fopen("verify2.tmp","r");
  textfile2 = fopen("verify.shimg","w");
  if (!textfile1 || !textfile2)
	error("Can't perform verification (%s)",strerror(errno));
  flag = 0;

  while(1==1){
    fgets(buffer, sizeof(buffer), textfile1);
    if (feof(textfile1))
      break;
    sscanf(buffer,"%x %c %s",&address, &type, name1);
    if (strcmp(name1 + strlen(name1) - 9, "__LOCAL__") == 0) flag++;
    if (address >= header.a_text + header.a_entry - (N_MAGIC(header) == QMAGIC ? 
						     sizeof (struct exec) : 0)) flag = 0;
    if (strcmp(name1, "_GLOBAL_OFFSET_TABLE_") == 0) continue;
    if (flag == 0) fprintf(textfile2,"%8.8x A %s\n",address, name1);
    if (strcmp(name, name1) == 0) break;
  };
  fclose(textfile1);
  fclose(textfile2);
  remove("verify2.tmp");
  system("echo diff verify.stub verify.shimg > verify.out");
  system("diff verify.stub verify.shimg >> verify.out");
  remove("verify.stub");
  remove("verify.shimg");
  /* Now tell the user whether this all worked properly or not */

  if ((textfile1 = fopen("verify.out","r"))==NULL)
	error("Can't open verify.out (%s)",strerror(errno));
  fgets(buffer, sizeof(buffer), textfile1);
  if (fgets(buffer, sizeof(buffer), textfile1) != NULL && strlen(buffer) != 0) {
    printf("Caution: stub library and sharable libraries have differences\n");
    printf("See file verify.out for list of diffs\n");
  } else {
    printf("The stub library and the sharable libraries have identical symbols\n");
  };
  fclose(textfile1);
  return 0;
}


