/*
   TXPATH.I
   Replacement for old meaning of text path= keyword.

   $Id$
 */
/*    Copyright (c) 1996.  The Regents of the University of California.
                    All rights reserved.  */

/* ------------------------------------------------------------------------ */

func rotext(text, path)
/* DOCUMENT plt, rotext(text,path), x, y
     return TEXT string as is (PATH=0), reversed (PATH=1), reversed and
     with a '\n' between each character, or with '\n' between characters
     (PATH=3).  When plotted, these strings correspond to the original
     meaning of the text path= keyword.  That is, successive characters
     will appear to the right, left, up, or down according to whether
     PATH is 0, 1, 2, or 3
   SEE ALSO: plt, path
 */
{
  if (path<0 || path>4) error, "path must be 0, 1, 2, or 3";
  n= strlen(text);
  if (!path || n<2) return text;
  text= (*pointer(text));
  if (path==3) text= text(1:n);
  else text= text(n:1:-1);
  if (text!=1) {
    n= array('\n', 2*n-1);
    n(1:0:2)= text;
    text= n;
  }
  return string(&text);
}

/* ------------------------------------------------------------------------ */
