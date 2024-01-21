/*
 * mkfs		A simple generic frontend for the for the mkfs program
 *		under Linux.  See the manual page for details.
 *
 * Usage:	mkfs [-V] [-t fstype] [fs-options] device [size]
 *
 * Authors:	David Engel, <david@ods.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Ron Sommeling, <sommel@sci.kun.nl>
 */


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <limits.h>


#define VERSION		"1.9"

#ifndef DEFAULT_FSTYPE
# define DEFAULT_FSTYPE		"minix"
#endif

#define SEARCH_PATH	"PATH=/sbin:/sbin/fs.d:/sbin/fs:/etc/fs:/etc"
#define PROGNAME	"mkfs.%s"


int main(int argc, char *argv[])
{
  char progname[NAME_MAX];
  char *fstype = NULL;
  int i, more = 0, verbose = 0;

  /* Check commandline options. */
  opterr = 0;
  while ((more == 0) && ((i = getopt(argc, argv, "Vt:")) != EOF))
    switch (i) {
    case 'V':
      verbose++;
      break;
    case 't':
      fstype = optarg;
      break;
    default:
      more = 1;
      break;		/* start of specific arguments */
    }
  if (optind == argc) {
    fprintf(stderr,
      "Usage: mkfs [-V] [-t fstype] [fs-options] device [size]\n");
    return -1;
  }
  
  /* If -t wasn't specified, use the default */
  if (fstype == NULL)
    fstype = DEFAULT_FSTYPE;

  /* Set PATH and program name */
  putenv(SEARCH_PATH);
  sprintf(progname, PROGNAME, fstype);
  argv[--optind] = progname;

  if (verbose) {
    puts("mkfs version " VERSION " (" __DATE__ ")");
    i = optind;
    while (argv[i])
      printf("%s ", argv[i++]);
    printf("\n");
    if (verbose > 1)
      return 0;
  }

  /* Execute the program */
  execvp(progname, argv+optind);
  perror(progname);
  return 1;
}
