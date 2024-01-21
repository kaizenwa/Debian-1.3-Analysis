/****************************************************************************/
/*                                                                          */
/*                           GNAT COMPILER TOOLS                            */
/*                                                                          */
/*                              G N A T C H P                               */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                             $Revision: 1.28 $                            */
/*                                                                          */
/*          Copyright (C) 1992-1997, Free Software Foundation, Inc.         */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/* This is the utility that takes the unit list output from the GNAT
 * compiler and uses it to actually write the individual files. It is
 * called as part of the gnatchop script.
 */

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef DIR_SEPARATOR
#if defined (__EMX__)
#define DIR_SEPARATOR '\\'
#else
#define DIR_SEPARATOR '/'
#endif
#endif

int open_read(char *path)
{
    return open(path, O_RDONLY | O_BINARY);
}

int open_create(char *path)
{
    return open(path, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY,
#if defined (__EMX__)
                S_IREAD | S_IWRITE);
#else
                S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
#endif
}

long file_length(int fd)
{
    /* Return the number of bytes in the named file. */
    int ret;
    struct stat statbuf;

    ret = fstat(fd, &statbuf);
    if (ret || !S_ISREG(statbuf.st_mode)) return 0L;
    return (statbuf.st_size);
}

int is_regular_file (char *name)
{
  int ret;
  struct stat statbuf;

  ret = stat(name, &statbuf);
  return (!ret && S_ISREG(statbuf.st_mode));
}

int is_directory_file (char *name)
{
  int ret;
  struct stat statbuf;

  ret = stat(name, &statbuf);
  return (!ret && S_ISDIR(statbuf.st_mode));
}

char   *optarg;				/* Global argument pointer. */

static char *scan = NULL;	        /* Private scan pointer. */

#define MAX_UNITS 1000  /* Maximum compilation units per single file */

main (int argc, char *argv [])
{
  int  fd1;
  int  i, j, n, unit;
  char *p;
  int  linenum;
  int  overwrite_option = 0;
  int  source_ref_option = 0;
  int  script_option = 0;
  int  errors = 0;
  char *ptr;
  char name [10000];
  char *names [MAX_UNITS];
  int  linenums [MAX_UNITS];
  long offsets [MAX_UNITS];
  long bytes;
  char *directory = (char *) 0;
  int  directory_len = 0;
  char *source_name;
  char *script_name;
  FILE *script;
  int c;
  extern char *optarg;
#ifdef VMS
#define optind decc$gl_optind
#endif
  extern int optind;

  /* Scan the arguments list. The arguments for gnatchop would be 
   * gnatchop [-ksw] filename [directory]
   */

  while ((c = getopt(argc, argv, "krsw")) != -1)
     switch (c) {
        case 'k':
            /* passed from k8 option to gnatchop, ignore it */
            break;
        case 's':
            script_option++;
            break;
        case 'w':
            overwrite_option++;
            break;
        case 'r':
            source_ref_option++;
            break;
        case '?':
            fprintf
              (stderr, "usage: gnatchop [-krsw] filename [directory]\n");
            exit (1);
     }

  source_name = argv [optind++];

  /* The optional last argument would be a directory name.  If it does not
   * correspond to an existing directory, issue message and abort. 
   */
  if (optind < argc) {
    directory_len = strlen (argv[optind]) + 1;
    directory = malloc (directory_len);
    strcpy (directory, argv [optind]);
    directory [directory_len] = '\0';
    if (!is_directory_file (directory)) {
      fprintf(stderr, "%s is not a valid directory\n", directory);
      exit (1);
    }
  }

  /* It is assumed that gcc is called by "gcc -gnatu -gnats -c filename" on 
   * a source file and the subsequent output is redirected as the standard
   * input to this program.  The strings are of the following forms:
   * "Config pragmas line 1, file offset 0, file name gnat.adc"
   * "Unit XXXXX (spec) line ddd, file offset ddd. file name XXXX.ads" or
   * "Unit XXXXX (body) line ddd, file offset ddd. file name XXXX.adb"
   * ...
   */

  printf("splitting %s into: \n", source_name);
  {
    /* Declare a buffer big enough to hold the offset information and then
     * read it all in with a single read operation.
     */
    int buffer_size = file_length (0);
    char buffer [buffer_size+1];

    read (0, buffer, buffer_size);
    buffer [buffer_size] = 0;
    ptr = buffer;
    for (unit=1;;unit++) {
      /* Scan for the string " line", skip over it, and read the line number.
       * if the string does not appear, it indicates that the entire string
       * has been fully processed.
       */
       ptr = strstr (ptr, " line");
       if (ptr == (char *) 0) break;
       ptr = ptr + 6;
       sscanf(ptr, "%d", &n);
       linenums [unit] = n;

      /* Scan for the word "offset", skip over it and read the actual offset
       * must be found, since we just found a ) line before it
       */
      ptr = strstr (ptr, "offset"); 
      ptr = ptr + 6;
      sscanf(ptr, "%d", &n);
      offsets [unit] = n;

      /* Scan for the words "file name", skip over them and read in the actual
       * file name to be used when writing out this compilation unit.  If a 
       * directory name was given as an argument, prepend it to the file name.
       */
      ptr = strstr (ptr, "file name"); 
      ptr = ptr + 9;
      sscanf(ptr, "%s", &name);
      names [unit] = malloc (strlen (name) + directory_len + 1);
      names [unit] [0] = '\0';
      if (directory) {
        strcpy (names [unit], directory);
        i = strlen (names [unit]);
	if (names [unit] [i-1] != DIR_SEPARATOR)
	  names [unit] [i++] = DIR_SEPARATOR;
        names [unit] [i] = '\0';
      }
      strcat (names [unit], name);
      printf("   %s \n", names [unit]);
      ptr = strstr (ptr, "\n"); 
    }
  }

  printf("\n"); 
  /* If the overwrite option is not specified check to see if any file that
   * would be written is the name of an existing file and if so print it and
   * later signal an abort after checking all files.
   */

  if (!overwrite_option) {
    for (i = 1; i < unit; i++)
      if (is_regular_file (names [i])) {
        fprintf(stderr, "%s already exists, use -w to overwrite\n", names [i]);
        errors++;
      }
      if (errors) {
        printf("no files have been written\n");
        exit (1);
      }
  }

  fd1 = open_read (source_name);
  bytes = file_length (fd1);
  offsets [unit] = bytes;

  {
    char buf2 [bytes];  /* large enough to read in entire source file. */
    int  fd2;
    int bytes_now;
    int bytes_read;
    int bytes_left;

    int has_no_corresponding_body (char *filename) 
    {
       int i;
       char *fname = strcpy (malloc (strlen(filename)+1), filename);
       fname [strlen (fname) - 1] = 'b';
       for (i = 1; i < unit; i++) {
         if (!strcmp (fname, names [i]))
           return 0;
       }
       return 1;
    }

  /*
   * Read the source file in groups of bytes given by the offsets array
   * and write each group as a separate file using the string given by the
   * names array.
   */
    for (i = 1; i < unit; i++) {
      bytes = offsets [i+1] - offsets [i];
      bytes_read = 0;
      bytes_left = bytes;
      do {
	bytes_now = read (fd1, &buf2 [bytes_read], bytes_left);
	bytes_read += bytes_now;
	bytes_left -= bytes_now;
      } while ((bytes_read < bytes) && (bytes_now > 0));
      if (bytes_read < bytes)
        buf2 [bytes_read] = 0;
      fd2 = open_create (names [i]);

      if (fd2 == -1) {
	 fprintf (stderr, "unable to open output file '%s'\n", 
		  names [i]);
	 errors++;
      } else {

	 if (source_ref_option) {
	    int j;
	    char c;
	    write (fd2, "pragma Source_Reference (", 25);
	    j = 100000;
	    while (j > 0) {
	       c = (linenums[i]/j) % 10 + '0';
	       write (fd2, &c, 1);
	       j = j / 10;
	    }

	    write (fd2, ", \"", 3);
	    write (fd2, source_name, strlen (source_name));
	    write (fd2, "\");\n", 4);
	 }

	 write (fd2, buf2, bytes_read);
	 close (fd2);
      }
    }

    if (script_option) {
       script_name = malloc (strlen (source_name) + 5);
       if (script_name == NULL) {
          printf ("Out of memory while attempting to create the compilation "
                  "script!\n");
          exit (1);
       }
       strcpy (script_name, source_name);

       /* Look for the last period in script_name  */
       ptr = strrchr (script_name, DIR_SEPARATOR);
       if (ptr == NULL)
          ptr = script_name;
       ptr = strrchr (ptr, '.');
       if (ptr == NULL) {
          ptr = script_name + strlen (script_name);
          *ptr = '.';
       }

#if defined (__EMX__)
       strcpy (++ptr, "cmd");
#elif defined (MSDOS)
       strcpy (++ptr, "bat");
#elif defined (VMS)
       strcpy (++ptr, "com");
#else
       strcpy (++ptr, "sh");
#endif

       script = fopen (script_name, "w"); 
#if !(defined (__EMX__) || defined (MSDOS) || defined (VMS))
       chmod (script_name, S_IRWXU);
#endif

       for (i = 1; i < unit; i++)
          if (strstr (names [i], ".adb")
                || has_no_corresponding_body (names [i]))
             fprintf (script,
#if defined (__EMX__) || defined (MSDOS)
                      "gcc -c %%1 %%2 %%3 %%4 %%5 %%6 %%7 %%8 %%9 %s\n",
#elif defined (VMS)
		      "$gcc -c 'p1' 'p2' 'p3' 'p4' 'p5' 'p6' 'p7' 'p8' %s\n",
#else
                      "gcc -c $* %s\n",
#endif
                      names [i]);
       printf ("script %s written\n", script_name);
    }

  }
  exit (errors);
}
