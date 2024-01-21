/*
 * NAME
 *	pfb2pfa - convert a type1 pfb file (binary) into a pfa (ASCII) 
 * SYNOPSIS
 *	pfb2pfa [-v] pfbfile [pfafile]
 * DESCRIPTION
 *	Program converts a binary MSDOS representation for a type1 
 *	PostScript font into a readable ASCII version. The MSDOS 
 *	newline (\r) is converted into the UNIX newline (\n).
 *	The output is written in a file whose name is the name that
 *	is provided on the command line or the basename of the input
 *	file plus extension ".pfa".
 *
 *	With the -v option you get some information about what the 
 *	program is doing.
 * AUTHOR
 *	Piet Tutelaers (rcpt@urc.tue.nl)
 */

#ifdef MSDOS
#define RB "rb"
#define WB "wb"
#define NEWLINE '\012'
#else
#define RB "r"
#define WB "w"
#define NEWLINE '\n'
#endif

#include <stdarg.h>
#include <stdio.h>
#include "filenames.h"

#ifdef KPSE
#include <c-auto.h>
#endif

#define HEX_PER_LINE 30

main (int argc, char *argv[])
{  int t, l, len, i;
   short c, done, verbose = 0;
   FILE *pfb, *pfa;
   char *pfbname, *pfaname, *myname = "pfb2pfa";

   /* prototypes */
   void fatal(char *fmt, ...);

   while (--argc > 0 && (*++argv)[0] == '-') {
      done=0;
      while ((!done) && (c = *++argv[0]))  /* allow -bcK like options */
      	 switch (c) {
      	 case 'v':
      	    verbose = 1; break;
      	 default:
      	    fatal("%s: %c illegal option\n", myname, c);
      	 }
      }

   if (argc < 1) fatal("Usage: %s [-v] pfbfile [pfafile]\n", myname);
   
   pfbname = argv[0]; argc--; argv++;
   if (argc < 1) pfaname = fullname(trim_basename(pfbname, ".pfb"), ".pfa");
   else if (argc == 1) pfaname = argv[0];
   else fatal("Usage: %s [-v] pfbfile [pfafile]\n", myname);

   pfb = fopen(pfbname, RB);
   if (pfb == NULL) fatal("Can't open %s\n", pfbname);
   
   pfa = fopen(pfaname, WB);
   if (pfa == NULL) fatal("Can't open %s\n", pfaname);
   
   while(!feof(pfb)) {
      if (getc(pfb) != 128)
	 fatal("%s: not a pfb file.\n", pfbname);
      t = getc(pfb);
      if (verbose) printf("Type: %d, ", t);
      switch (t) {
      case 1:
         l = getc(pfb) | getc(pfb)<<8 | getc(pfb)<<16 | getc(pfb)<<24;
         if (verbose) printf(" plain text, length %d\n", l);
	 for (i=0; i < l ; i++) {
            c = getc(pfb);
            if (c == '\r') putc(NEWLINE, pfa);
            else putc(c, pfa);
	  }
         break;
      case 2:
         l = getc(pfb) | getc(pfb)<<8 | getc(pfb)<<16 | getc(pfb)<<24;
         if (verbose) printf(" binary data, length %d\n", l);
         for(i = 0; i < l ;i++) {
	    fprintf(pfa, "%02x", getc(pfb));
	    if ((i+1) % HEX_PER_LINE == 0) putc(NEWLINE, pfa);
	 }
	 putc(NEWLINE, pfa);
         break;
      case 3:
         if (verbose) printf("End of file\n");
         exit(0);
         break;
      default:
	 fatal("Unknown field type: %d\n", t);
      }
   }
   fclose(pfa); fclose(pfb);
}

/* Give up ... */
void fatal(char *fmt, ...)
{  va_list args;

   va_start(args, fmt);
   vfprintf(stderr, fmt, args);
   va_end(args);
   exit(1);
}
