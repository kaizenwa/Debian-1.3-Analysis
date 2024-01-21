#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include <locale.h>
#include <localeinfo.h>

#include "mknumeric.h"

extern int yyparse (void);

unsigned char *codename;

struct numeric_info ninfo =
{
  "",				/* char *decimal_point */
  "",				/* char *thousands_sep */
  ""				/* char *grouping */
};

int
main (int argc, char *argv[])
{
  int i;
  unsigned char *outname = "LC_NUMERIC";

  while ((i = getopt (argc, argv, "o:")) != EOF)
    {
      switch (i)
	{
	case 'o':
	  outname = optarg;
	  break;
	}
    }

  if (argc - optind > 1)
    {
      (void) fprintf (stderr, "Usage: %s [-o out_file_name] [file]\n", argv[0]);
      return 3;
    }
  else if ((argc - optind) == 1)
    {
      if (freopen (argv[optind], "r", stdin) == NULL)
	{
	  perror (argv[optind]);
	  return 2;
	}
    }

  if (yyparse ())
    return 1;

  return !write_out (outname);
}

void
write_str (char *str, FILE * ofp)
{
  short int slen = strlen (str) + 1;

  (void) fwrite (&slen, sizeof (slen), 1, ofp);
  (void) fwrite (str, sizeof (char), slen, ofp);
}

int
write_out (outname)
     unsigned char *outname;
{
  FILE *ofp = fopen (outname, "w");

  if (ofp == NULL)
    return 0;

  write_str (ninfo.decimal_point, ofp);
  write_str (ninfo.thousands_sep, ofp);
  write_str (ninfo.grouping, ofp);
#ifndef NOGUARD
  (void) fwrite (codename, sizeof (unsigned char), strlen (codename) + 1, ofp);
#endif
  (void) fclose (ofp);
  return 1;
}
