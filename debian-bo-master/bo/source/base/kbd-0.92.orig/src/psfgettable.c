/*
 * psfgettable.c
 *
 * Extract a Unicode character table from a PSF font
 *
 * Copyright (C) 1994 H. Peter Anvin
 *
 * This program may be freely copied under the terms of the GNU
 * General Public License (GPL), version 2, or at your option
 * any later version.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sysexits.h>
#include <string.h>
#include "psf.h"

typedef unsigned short unicode;
void usage(char *argv0)
{
  fprintf(stderr, "Usage: \n"
                  "        %s psffont [outfile]\n", argv0);
  exit(EX_USAGE);
}

int
main(int argc, char *argv[])
{
  FILE *in, *out;
  char *inname;
  struct psf_header psfhdr;
  int glyph;
  unicode unichar;
  int fontlen;

  if ( argc < 1 || argc > 3 )
    usage(argv[0]);

  if ( !strcmp(argv[1],"-") )
    {
      in = stdin;
      inname = "stdin";
    }
  else
    {
      in = fopen(inname = argv[1], "r");
      if ( !in )
        {
          perror(inname);
          exit(EX_NOINPUT);
        }
    }

  if ( argc < 4 || !strcmp(argv[3],"-") )
    out = stdout;
  else
    {
      out = fopen(argv[3], "w");
      if ( !out )
        {
          perror(argv[3]);
          exit(EX_CANTCREAT);
        }
    }

  if ( fread(&psfhdr, sizeof(struct psf_header), 1, in) < 1 )
    {
      fprintf(stderr, "%s: Cannot read psf header\n", inname);
      exit(EX_DATAERR);
    }
  
  if ( psfhdr.magic != PSF_MAGIC )
    {
      fprintf(stderr, "%s: Bad magic number\n", inname);
      exit(EX_DATAERR);
    }
  
  if ( psfhdr.mode > PSF_MAXMODE )
    {
      fprintf(stderr, "%s: Unknown mode number (%d)\n", inname, psfhdr.mode);
      exit(EX_DATAERR);
    }
  
  fontlen = ( psfhdr.mode & PSF_MODE512 ) ? 512 : 256;

  if ( ! (psfhdr.mode & PSF_MODEHASTAB ) )
    {
      fprintf(stderr, "%s: Font has no character table\n", inname);
      exit(EX_DATAERR);
    }

    /* Skip font data */
  if ( fseek(in, psfhdr.charsize * fontlen, SEEK_CUR) == -1)
    {
      perror(inname);
      exit(EX_DATAERR);
    }

  /* Copy table */

  fprintf(out, "#\n# Character table extracted from font %s\n#\n", inname);

  for ( glyph = 0 ; glyph < fontlen ; glyph++ )
    {
      if ( fontlen <= 256 )
	fprintf(out, "0x%02x\t", glyph);
      else
	fprintf(out, "0x%03x\t", glyph);

      while ( fread(&unichar, sizeof(unicode), 1, in),
	      unichar != PSF_SEPARATOR )
	fprintf(out, " U+%04x", unichar);

      putc('\n', out);
    }

  fclose(in);
  fclose(out);

  exit(EX_OK);
}
