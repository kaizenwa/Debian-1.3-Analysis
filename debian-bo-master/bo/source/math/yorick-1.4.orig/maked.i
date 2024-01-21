/*
  MAKED.I
  Yorick script to genrate alphabetized listings of all help
  command documentation.

  $Id$
 */

#include "mkdoc.i"

/* assume current working directory is top level of distribution */
mkdoc, "Yorick/std.i", "Yorick/doc/std.doc";
mkdoc, "Yorick/graph.i", "Yorick/doc/graph.doc";
mkdoc, ["MathC/fft.i","MathC/matrix.i"], "Yorick/doc/math.doc";
mkdoc, "Drat/drat.i", "Yorick/doc/drat.doc";

/* go ahead and copy Yorick/include/README as a .doc also */
f= open("Yorick/include/README");
g= create("Yorick/doc/library.doc");
do {
  lines= rdline(f, 1000);
  n= sum(lines!=string(0));
  if (n) write, g, lines(1:n), format="%s\n";
} while (n==1000);
f= g= [];

quit;
