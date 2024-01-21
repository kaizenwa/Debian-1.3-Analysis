#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include <locale.h>
#include <localeinfo.h>

#include "mkmonetary.h"

extern int yyparse (void);

unsigned char *codename;

struct monetary_info minfo =
{
  "",				/* char *int_curr_symbol */
  "",				/* char *currency_symbol */
  "",				/* char *mon_decimal_point */
  "",				/* char *mon_thousands_sep */
  "",				/* char *mon_grouping */
  "",				/* char *positive_sign */
  "",				/* char *negative_sign */
  CHAR_MAX,			/* char int_frac_digits */
  CHAR_MAX,			/* char frac_digits */
  CHAR_MAX,			/* char p_cs_precedes */
  CHAR_MAX,			/* char p_sep_by_space */
  CHAR_MAX,			/* char n_cs_precedes */
  CHAR_MAX,			/* char n_sep_by_space */
  CHAR_MAX,			/* char p_sign_posn */
  CHAR_MAX			/* char n_sign_posn */
};

int
main (int argc, char *argv[])
{
  int i;
  unsigned char *outname = "LC_MONETARY";

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

  write_str (minfo.int_curr_symbol, ofp);
  write_str (minfo.currency_symbol, ofp);
  write_str (minfo.mon_decimal_point, ofp);
  write_str (minfo.mon_thousands_sep, ofp);
  write_str (minfo.mon_grouping, ofp);
  write_str (minfo.positive_sign, ofp);
  write_str (minfo.negative_sign, ofp);
  (void) fwrite (&minfo.int_frac_digits, sizeof (unsigned char), ((&minfo.n_sign_posn) - (&minfo.int_frac_digits)) + sizeof (minfo.n_sign_posn), ofp);
#ifndef NOGUARD
  (void) fwrite (codename, sizeof (unsigned char), strlen (codename) + 1, ofp);
#endif
  (void) fclose (ofp);
  return 1;
}
