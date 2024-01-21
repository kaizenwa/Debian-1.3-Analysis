#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include <locale.h>
#include <localeinfo.h>

#include "mkctype.h"

extern int yyparse (void);

mk_pair_t *blanks;
mk_pair_t *cntrls;
mk_pair_t *digits;
mk_pair_t *hexs;
mk_pair_t *lowers;
mk_pair_t *nographs;
mk_pair_t *puncts;
mk_pair_t *spaces;
mk_pair_t *uppers;

mk_pair_t *ul_pairs;

unsigned char *codename;
unsigned char *ctab_filename = NULL;

unsigned short int x2type[UCHAR_MAX + 2] =
{
  0
};

unsigned char x2upper[UCHAR_MAX + 2] =
{
  '\xff'
};

unsigned char x2lower[UCHAR_MAX + 2] =
{
  '\xff'
};

void
plant (unsigned int pattern, mk_pair_t * node)
{
  unsigned int i;

  while (node != NULL)
    {
      for (i = node->lower; i <= node->upper; i++)
	x2type[i + 1] |= (unsigned short) pattern;
      node = node->next;
    }
}

void
mkxtab (mk_pair_t * node)
{
  unsigned int i;

  while (node != NULL)
    {
      if (!(x2type[node->lower + 1] & _ISlower))
	(void) fprintf (stderr, "warning: 0x%02x is not marked as lower (ul <0x%02x 0x%02x>)\n", node->lower, node->upper, node->lower);

      if (!(x2type[node->upper + 1] & _ISupper))
	(void) fprintf (stderr, "warning: 0x%02x is not marked as upper (ul <0x%02x 0x%02x>)\n", node->upper, node->upper, node->lower);

      x2upper[node->lower + 1] = node->upper;
      x2lower[node->upper + 1] = node->lower;
      node = node->next;
    }
}

typedef struct bbits_
  {
    unsigned short int bbits;
    unsigned char *bb_name;
  }

bbits_t;

bbits_t basic_bits[] =
{
  {_ISupper, "_ISupper"},
  {_ISlower, "_ISlower"},
  {_IScntrl, "_IScntrl"},
  {_ISdigit, "_ISdigit"},
  {_ISspace, "_ISspace"},
  {_IShex, "_IShex"},
  {_ISpunct, "_ISpunct"},
  {_NOgraph, "_NOgraph"},
  {_ISblank, "_ISblank"},
  {0, NULL}
};

void
ctab_out (void)
{
  unsigned short int i;
  FILE *fout = fopen (ctab_filename, "w");

  if (fout == NULL)
    {
      perror (ctab_filename);
      return;
    }

  fputs ("const unsigned short int __ctype_b[] = {\n", fout);
  fputs ("\t/* EOF */\t\t0", fout);

  for (i = 1; i <= UCHAR_MAX + 1; i++)
    {
      (void) fprintf (fout, ",\n\t/* 0x%02x, %d, 0%o */\t", i - 1, i - 1, i - 1);
      if (x2type[i])
	{
	  int dirty = 0;
	  bbits_t *tb = basic_bits;

	  while (tb->bbits)
	    {
	      if (x2type[i] & tb->bbits)
		{
		  if (dirty)
		    fputs ("|", fout);
		  fputs (tb->bb_name, fout);
		  dirty = 1;
		}
	      tb++;
	    }
	}
      else
	fputs ("0", fout);
    }

  fputs ("\n};\n", fout);

  fputs ("const unsigned char __ctype_tolower[] = {\n", fout);
  (void) fprintf (fout, "\t/* EOF */\t\t0x%02x", x2lower[0]);
  for (i = 1; i <= UCHAR_MAX + 1; i++)
    {
      (void) fprintf (fout, ",\n\t/* 0x%02x, %d, 0%o */\t0x%02x",
		      i - 1, i - 1, i - 1, x2lower[i]);
    }
  fputs ("\n};\n", fout);

  fputs ("const unsigned char __ctype_toupper[] = {\n", fout);
  (void) fprintf (fout, "\t/* EOF */\t\t0x%02x", x2upper[0]);
  for (i = 1; i <= UCHAR_MAX + 1; i++)
    {
      (void) fprintf (fout, ",\n\t/* 0x%02x, %d, 0%o */\t0x%02x",
		      i - 1, i - 1, i - 1, x2upper[i]);
    }
  fputs ("\n};\n", fout);

  (void) fclose (fout);
}

int
main (int argc, char *argv[])
{
  int i;
  unsigned char *outname = "LC_CTYPE";

  while ((i = getopt (argc, argv, "c:o:")) != EOF)
    {
      switch (i)
	{
	case 'c':
	  ctab_filename = optarg;
	  break;

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

  for (i = 0; i <= UCHAR_MAX; i++)
    {
      x2upper[i + 1] = x2lower[i + 1] = i;
      x2type[i + 1] = 0;
    }

  plant (_ISblank, blanks);
  plant (_IScntrl, cntrls);
  plant (_ISdigit, digits);
  plant (_IShex, hexs);
  plant (_ISlower, lowers);
  plant (_ISpunct, puncts);
  plant (_ISspace, spaces);
  plant (_ISupper, uppers);
  plant (_NOgraph, nographs);

  mkxtab (ul_pairs);

  if (ctab_filename)
    ctab_out ();

  return !write_out (outname);
}

int
write_out (outname)
     unsigned char *outname;
{
  FILE *ofp = fopen (outname, "w");

  if (ofp == NULL)
    return 0;

  (void) fwrite (x2type, sizeof (x2type), 1, ofp);
  (void) fwrite (x2lower, sizeof (x2lower), 1, ofp);
  (void) fwrite (x2upper, sizeof (x2upper), 1, ofp);
#ifndef NOGUARD
  (void) fwrite (codename, sizeof (unsigned char), strlen (codename) + 1, ofp);
#endif
  (void) fclose (ofp);
  return 1;
}
