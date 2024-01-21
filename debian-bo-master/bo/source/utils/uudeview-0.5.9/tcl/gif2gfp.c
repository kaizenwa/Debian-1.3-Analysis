#include <stdio.h>
#include <ctype.h>

/*
 * reads a GIF file from stdin, writes a TCL string definition of the
 * file in GFP format by the name of argv[1] to stdout.
 * see tkImgFmtGFP.c.                  fp@informatik.uni-frankfurt.de
 *
 * $Id: gif2gfp.c,v 1.1.1.1 1996/06/06 19:41:11 fp Exp $
 */

#define CPL 70

int
main (argc, argv)
	int argc;
	char *argv[];
{
  int cplc=3, ch, buf[3];

  if (argc!=2) {
    fprintf (stderr, "usage: %s string_name\n", argv[0]);
    exit (1);
  }
  buf[0] = fgetc (stdin);
  buf[1] = fgetc (stdin);
  buf[2] = fgetc (stdin);

  if (buf[0]!='G' || buf[1]!='I' || buf[2]!='F') {
    fprintf (stderr, "not a gif file\n");
    exit (1);
  }

  printf ("\
\n\
#\n\
# automatically generated %s by %s\n\
# you do not want to edit this :-)\n\
#\n\
\n\
set %s \"\\\nGFP",
	  __DATE__, argv[0], argv[1]);

  while (!feof(stdin)) {
    if ((ch = fgetc (stdin)) == EOF)
      break;
    switch (ch) {
    case '"':
    case '\\':
    case '[':
    case ']':
    case '$':
      cplc+=2;
      putchar ('\\');
      putchar (ch);
      break;
    case '\000':
    case '\001':
    case ' ':
      cplc+=8;
      printf ("\\001\\%03o", (int)(ch+1));
      break;
    default:
      if (isalnum(ch))
	putchar(ch), cplc+=1;
      else
	printf ("\\%03o", (int)ch), cplc+=4;
      break;
    }
    if (cplc>CPL)
      printf ("\\\n"), cplc=0;
  }
  printf ("\"\n\n");
  return 0;
}
