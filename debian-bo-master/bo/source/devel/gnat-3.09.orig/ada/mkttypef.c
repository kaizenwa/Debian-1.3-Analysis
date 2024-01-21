/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             M K T T Y P E F                              */
/*                                                                          */
/*                             Utility Program                              */
/*                                                                          */
/*                            $Revision: 1.17 $                             */
/*                                                                          */
/*   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  */
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

/* Program to construct ttypef.ads from the template file ttypef.adt */

#include "hconfig.h"
#include "machmode.h"
#include "real.h"
#include <stdio.h>

/* Provide default values for all of the floating-point type widths
   (copied from c-decl.c).  */

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

/* Map attribute in template into string value.  */

#define NUM_FLOAT_SIZES 5

int float_sizes[NUM_FLOAT_SIZES] = {32, 48, 64, 96, 128};

struct {char *attr, *string[NUM_FLOAT_SIZES];} attribute_map[] =

    {{"DN",                                           /* Denorm  */
       {"True",
        "True",
        "True",
        "True",
        "True"}},

     {"EP",                                           /* Epsilon  */
       {"2#1.0#E-20",
        "2#1.0#E-39",
        "2#1.0#E-49",
        "2#1.0#E-60",
        "2#1.0#E-60"}},

     {"EX",                                           /* Emax  */
       {"84",
        "156",
        "200",
        "244",
        "244"}},

     {"FS",                                           /* First  */
       {"-16#0.FFFF_FF#E+32",
        "-16#0.FFFF_FFFF_FF#E+64",
        "-16#0.FFFF_FFFF_FFFF_F8#E+256",
        "-16#0.FFFF_FFFF_FFFF_FFFF#E+4096",
        "-16#0.FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_8#E+4096"}},

     {"LG",                                           /* Large  */
       {"16#0.FFFF_F8#E+21",
        "16#0.FFFF_FFFF_F8#E+37",
        "16#0.FFFF_FFFF_FFFF_C#E+50",
        "16#0.FFFF_FFFF_FFFF_FFF8#E+61",
        "16#0.FFFF_FFFF_FFFF_FFF8#E+61"}},

     {"LS",                                           /* Last  */
       {"16#0.FFFF_FF#E+32",
        "16#0.FFFF_FFFF_FF#E+64",
        "16#0.FFFF_FFFF_FFFF_F8#E+256",
        "16#0.FFFF_FFFF_FFFF_FFFF#E+4096",
        "16#0.FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_8#E+4096"}},

     {"MA",                                           /* Mantissa  */
       {"21",
        "35",
        "51",
        "61",
        "61"}},

     {"MM",                                           /* Machine_Mantissa  */
       {"24",
        "40",
        "53",
        "64",
        "113"}},

     {"MN",                                           /* Machine_Emin  */
       {"-125",
        "-125",
        "-1021",
        "-16381",
        "-16381"}},

     {"MO",                                           /* Machine_Overflows  */
       {"False",
        "False",
        "False",
        "False",
        "False"}},

     {"MR",                                           /* Machine_Radix  */
       {"2",
        "2",
        "2",
        "2",
        "2"}},

     {"MX",                                           /* Machine_Emax  */
       {"128",
        "128",
        "1024",
        "16384",
        "16384"}},

     {"OE",                                           /* Model_Epsilon  */
       {"2#1.0#E-23",
        "2#1.0#E-39",
        "2#1.0#E-52",
        "2#1.0#E-63",
        "2#1.0#E-113"}},

     {"OM",                                           /* Model_Mantissa  */
       {"24",
        "40",
        "53",
        "64",
        "113"}},

     {"ON",                                           /* Model_Emin  */
       {"-125",
        "-125",
        "-1021",
        "-16381",
        "-16381"}},

     {"OS",                                           /* Model_Small  */
       {"2#1.0#E-126",
        "2#1.0#E-126",
        "2#1.0#E-1022",
        "2#1.0#E-16381",
        "2#1.0#E-16381"}},

     {"RN",                                           /* Machine_Rounds  */
       {"True",
        "True",
        "True",
        "True",
        "True"}},

     {"SA",                                           /* Small */
       {"2#1.0#E-85",
        "2#1.0#E-157",
        "2#1.0#E-201",
        "2#1.0#E-245",
        "2#1.0#E-245"}},

     {"SF",                                           /* Safe_First  */
       {"-16#0.FFFF_FF#E+32",
        "-16#0.FFFF_FFFF_FF#E+64",
        "-16#0.FFFF_FFFF_FFFF_F8#E+256",
        "-16#0.FFFF_FFFF_FFFF_FFFF#E+4096",
        "-16#0.FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_8#E+4096"}},

     {"SL",                                           /* Safe_Large  */
       {"16#0.FFFF_FF#E+32",
        "16#0.FFFF_FFFF_FF#E+64",
        "16#0.FFFF_FFFF_FFFF_F8#E+256",
        "16#0.FFFF_FFFF_FFFF_FFFF#E+4096",
        "16#0.FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_8#E+4096"}},

     {"SM",                                           /* Safe_Small  */
       {"2#1.0#E-126",
        "2#1.0#E-126",
        "2#1.0#E-1022",
        "2#1.0#E-16381",
        "2#1.0#E-16381"}},

     {"SS",                                           /* Safe_Last  */
       {"16#0.FFFF_FF#E+32",
        "16#0.FFFF_FFFF_FF#E+64",
        "16#0.FFFF_FFFF_FFFF_F8#E+256",
        "16#0.FFFF_FFFF_FFFF_FFFF#E+4096",
        "16#0.FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_8#E+4096"}},

     {"SX",                                           /* Safe_Emax  */
       {"128",
        "128",
        "1024",
        "16384",
        "16384"}},

     {"SZ",                                           /* Signed_Zeros  */
       {"True",
        "True",
        "True",
        "True",
        "True"}}};

#define NUM_ATTRS sizeof attribute_map / sizeof attribute_map[0]
#define UNKNOWN_FLOAT_TYPE (-1)

struct {char *name; int size, idx;} float_type_sizes[] =
{ {"float", FLOAT_TYPE_SIZE, -1},
  {"double float", DOUBLE_TYPE_SIZE, -1},
  {"long double", LONG_DOUBLE_TYPE_SIZE, -1}};

int
main (argc, argv)
     int argc;
     char **argv;
{
  FILE *infile;
  int i, j;
  int c, exit_code = SUCCESS_EXIT_CODE;

  if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT)
    {
      fprintf (stderr, "Don't know how to handle this float format");
      exit_code = FATAL_EXIT_CODE;
    }

  for (i = 0; i < sizeof float_type_sizes / sizeof float_type_sizes[0]; i++)
    {
      for (j = 0; j < NUM_FLOAT_SIZES; j++)
	if (float_sizes[j] == float_type_sizes[i].size)
	  float_type_sizes[i].idx = j;

      if (float_type_sizes[i].idx < 0)
	{
	  fprintf (stderr,
		   "Characteristics of a %s type of size %d bits unknown.",
		   float_type_sizes[i].name, float_type_sizes[i].size);
	  exit_code = FATAL_EXIT_CODE;
	}
    }

  if (exit_code != SUCCESS_EXIT_CODE)
    {
      fprintf (stderr,
	  " -\nyou must create ttypef.ads by some other means.\n");
      exit (exit_code);
    }

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      exit (FATAL_EXIT_CODE);
    }

  /* Read the entire file doing something special when we hit a "%".  */
  while ((c = getc (infile)) != EOF)
    {
      if (c != '%')
	putchar (c);
      else
	{
	  int float_type, c1, c2;
	  char *value;

	  /* Get the prefix.  */
	  c = getc (infile);
	  switch (c)
	    {
	    case 'f': /* FLOAT  */
	    case 'F': /* SHORT_FLOAT  */
	      float_type = 0;
	      break;

	    case 'd':
	      float_type = 1; /* LONG_FLOAT  */
	      break;

	    case 'D':
	      /* LONG_LONG_FLOAT  */
	      float_type = 2;
	      break;

	    default:
	      fprintf (stderr, "Unknown prefix character '%c'\n", c);
	      exit (FATAL_EXIT_CODE);
	    }

	  /* Scan the attribute map to get the attribute.  */
	  c1 = getc (infile); c2 = getc (infile);
	  value = NULL;
	  for (i = 0; i < NUM_ATTRS; i++)
	    if (attribute_map[i].attr[0] == c1
		&& attribute_map[i].attr[1] == c2)
	      value
		= attribute_map[i].string[float_type_sizes[float_type].idx];

	  if (value == NULL)
	    {
	      fprintf (stderr, "Unknown attribute \"%c%c\"\n", c1, c2);
	      exit (FATAL_EXIT_CODE);
	    }

	  else printf ("%s", value);
	}
    }

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
