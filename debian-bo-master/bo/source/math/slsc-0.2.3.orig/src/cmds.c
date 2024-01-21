/*	SC	A Spreadsheet Calculator
 *		Command routines
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *
 *		$Revision: 6.1 $
 */
#include <stdio.h>

#include <signal.h>

#ifdef BSD42
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <slang.h>

#include "sc.h"
#include "cmds.h"
#include "interp.h"
#include "lex.h"
#include "range.h"
#include "crypt.h"

#define DEFCOLDELIM ':'

static int openrow (int);
static int opencol (int);

void duprow (void)
{
   if (!openrow (currow + 1)) return;
   
   currow++; modflg++;
   for (curcol = 0; curcol <= maxcol; curcol++) 
     {
	register struct ent *p = tbl[currow - 1][curcol];
	if (p) 
	  {
	     register struct ent *n;
	     n = lookat (currow, curcol);
	     copyent ( n, p, 1, 0);
	  }
     }
   
   for (curcol = 0; curcol <= maxcol; curcol++) 
     {
	register struct ent *p = tbl[currow][curcol];
	if (p && (p -> flags & IS_VALID) && !p -> expr)
	  break;
     }
   
   if (curcol > maxcol)
     curcol = 0;
}

void dupcol (void)
{
   if (!opencol (curcol + 1)) return;
   modflg++;
   curcol++;
   
   for (currow = 0; currow <= maxrow; currow++) 
     {
	register struct ent *p = tbl[currow][curcol - 1];
	if (p) 
	  {
	     register struct ent *n;
	     n = lookat (currow, curcol);
	     copyent ( n, p, 0, 1);
	  }
     }
   for (currow = 0; currow <= maxrow; currow++) 
     {
	register struct ent *p = tbl[currow][curcol];
	if (p && (p -> flags & IS_VALID) && !p -> expr)
	  break;
     }
   if (currow > maxrow)
     currow = 0;
}

void insertrow (int arg)
{
   while (arg-- > 0) 
     {
	if (!openrow (currow)) return;
     }
}

void deleterow(int arg)
{
   sc_flush_saved();
   erase_area(currow, 0, currow + arg - 1, maxcol);
   currow += arg;
   while (--arg>=0) closerow (--currow);
   sync_refs();
}

void insertcol(int arg)
{
   while (arg-- > 0)
     {
	if (!opencol(curcol)) return;
     }
}

void deletecol(int arg)
{
   sc_flush_saved();
   erase_area(0, curcol, maxrow, curcol + arg - 1);
   curcol += arg;
   while (--arg>=0) closecol (--curcol);
   sync_refs();
}

void rowvalueize (int arg)
{
    valueize_area(currow, 0, currow + arg - 1, maxcol);
}

void colvalueize(int arg)
{
    valueize_area(0, curcol, maxrow, curcol + arg - 1);
}

void erase_area(int sr, int sc, int er, int ec)
{
    register int r, c;
    register struct ent **p;

   if (sr > er) 
     {
	r = sr; sr = er; er= r;	
     }
   
   if (sc > ec) 
     {
	c = sc; sc = ec; ec= c;	
     }

   if (sr < 0)
     sr = 0; 
   if (sc < 0)
     sc = 0;
   if (er >= MAXROWS)
     er = MAXROWS-1;
   if (ec >= MAXCOLS)
     ec = MAXCOLS-1;
   
   for (r = sr; r <= er; r++) 
     {
	for (c = sc; c <= ec; c++) 
	  {
	     p = &tbl[r][c];
	     if (*p) 
	       {
		  sc_free_ent(*p);
		  *p = 0;
	       }
	  }
     }
   Sc_Changed = 1;
   FullUpdate = 1;
}

void sc_erase_block (int r1, int c1, int r2, int c2)
{
   sc_flush_saved();
   erase_area(r1, c1, r2, c2);
   sync_refs();
}

void valueize_area(int sr, int sc, int er, int ec)
{
   register int r, c;
   register struct ent *p;

   if (sr > er) 
     {
	r = sr; sr = er; er= r;	
     }

   if (sc > ec) 
     {
	c = sc; sc = ec; ec= c;	
     }

   if (sr < 0)
     sr = 0; 
   if (sc < 0)
     sc = 0;
   if (er >= MAXROWS)
     er = MAXROWS-1;
   if (ec >= MAXCOLS)
     ec = MAXCOLS-1;
   
   for (r = sr; r <= er; r++) 
     {
	for (c = sc; c <= ec; c++) 
	  {
	     p = tbl[r][c];
	     if (p && p->expr) 
	       {
		  sc_efree(p->expr);
		  p->expr = 0;
		  p->flags &= ~IS_STREXPR;
	       }
	  }
     }
}

void pullcells (int to_insert)
{
   register struct ent *p, *n, *to_fix;
   register int deltar, deltac;
   int minrow, mincol;
   int mxrow, mxcol;
   int numrows, numcols;

   if (to_insert == 'r')
     {
	to_fix = Sc_Row_Pastebuffer;
     }
   else if (to_insert == 'c')
     {
	to_fix = Sc_Col_Pastebuffer;
     }
   else
     {
	to_fix = Sc_Block_Pastebuffer;
     }

   if (to_fix == NULL)
     {
	slsc_error ("Pastebuffer is empty!");
	return;
     }
   
   minrow = MAXROWS; 
   mincol = MAXCOLS;
   mxrow = 0;
   mxcol = 0;
   
   /* Find limits of the cells to pull (size of region) */
   for (p = to_fix; p; p = p->next) 
     {
	if (p->row < minrow)
	  minrow = p->row;
	if (p->row > mxrow)
	  mxrow = p->row;
	if (p->col < mincol)
	  mincol = p->col;
	if (p->col > mxcol)
	  mxcol = p->col;
     }
   
   numrows = mxrow - minrow + 1;
   numcols = mxcol - mincol + 1;
   deltar = currow - minrow;
   deltac = curcol - mincol;
   
   if (to_insert == 'r') 
     {
	insertrow(numrows);
	deltac = 0;
     } 
   else if (to_insert == 'c') 
     {
	insertcol(numcols);
	deltar = 0;
     }
   
   FullUpdate++;
   modflg++;
   for (p = to_fix; p; p = p->next) 
     {
	n = lookat (p->row + deltar, p->col + deltac);
	(void) sc_clearent(n);
	copyent( n, p, deltar, deltac);
	n -> flags = p -> flags & ~IS_DELETED;
     }
}

void colshow_op(void)
{
    register int i, j;
    for (i=0; i < MAXCOLS; i++)
     if (col_hidden[i]) break;
   for(j=i; j<MAXCOLS; j++) if (!col_hidden[j]) break;
   j--;
   if (i>=MAXCOLS)
     slsc_error ("No hidden columns to show");
   else 
     {
	(void) sprintf(line,"show %s:", sc_coltoa(i));
	(void) sprintf(line + strlen(line),"%s",sc_coltoa(j));
	linelim = strlen (line);
     }
}

void rowshow_op (void)
{
    register int i,j;
   for (i=0; i<MAXROWS; i++)
     if (row_hidden[i]) break;
   for(j=i; j<MAXROWS; j++) if (!row_hidden[j]) 
     {
	break;
     }
   j--;

   if (i >= MAXROWS)
     slsc_error ("No hidden rows to show");
   else 
     {
	(void) sprintf(line,"show %d:%d", i, j);
        linelim = strlen (line);
     }
}


static int is_row_empty (int r)
{
   register struct ent *p;
   int c;
   for (c = 0; c <= maxcol; c++)
     {
	if (VALID_CELL(p, r, c)) return 0;
     }
   return 1;
}

static int openrow (int rs)
{
   register    r;
   register struct ent **p;
   register    c;
   register	i;

   
   if (rs > maxrow) maxrow = rs; else maxrow++;
   
   if (maxrow >= MAXROWS)
     {
	maxrow = MAXROWS - 1;
	if ((rs >= MAXROWS) || !is_row_empty (maxrow))
	  {
	     slsc_error ("The table can't made be any longer.  Try deleting some rows.");
	     return 0;
	  }
     }
   
   for (i = maxrow; i > rs; i--) 
     {
	row_hidden[i] = row_hidden[i-1];
     }
   
   for (r = maxrow; r > rs; r--)
     {
	for (c = maxcol + 1, p = &tbl[r][0]; --c >= 0; p++)
	  {
	     if (0 != (p[0] = p[-MAXCOLS])) p[0] -> row++;
	     /* Ugly !!! */
	  }
     }
   
   p = &tbl[rs][0];
   for (c = maxcol + 1; --c >= 0;) *p++ = 0;
   FullUpdate++;
   modflg++;
   Sc_Changed = 1;
   return 1;
}

void closerow (int r)
{
   register struct ent **p;
   register c;
   register int i;

   if (r > maxrow) return;

   p = &tbl[r][0];
   for (c=maxcol+1; --c>=0; ) 
     {
	if (*p)
      	  sc_free_ent(*p);
	*p++ = 0;
     }

   for (i = r; i < MAXROWS - 1; i++) 
     {
	row_hidden[i] = row_hidden[i+1];
     }
   
   while (r<maxrow) 
     {
	for (c = maxcol+1, p = &tbl[r][0]; --c>=0; p++)
	  {
	     if (0 != (p[0] = p[MAXCOLS])) p[0]->row--;
	  }
	r++;
    }

   p = &tbl[maxrow][0];
   for (c=maxcol+1; --c>=0; ) *p++ = 0;
   maxrow--;
   FullUpdate++;
   Sc_Changed = 1;
   modflg++;
}

static int is_col_empty (int c)
{
   register struct ent *p;
   int r;
   for (r = 0; r <= maxrow; r++)
     {
	if (VALID_CELL(p, r, c)) return 0;
     }
   return 1;
}

static int opencol (int cs)
{
   register r;
   register struct ent **p, **pmin;
   int i;

   if (cs > maxcol) maxcol = cs; else maxcol++;
   
   if (maxcol >= MAXCOLS)
     {
	maxcol = MAXCOLS - 1;
	if ((cs >= MAXCOLS) || !is_col_empty (maxcol))
	  {
	     slsc_error ("The table can't made be any wider.  Try deleting some columns.");
	     return 0;
	  }
     }
   
   for (i = maxcol; i > cs; i--) 
     {
	Sc_Col_Width[i] = Sc_Col_Width[i-1];
	precision[i] = precision[i-1];
	col_hidden[i] = col_hidden[i-1];
     }
   /* Sc_Col_Width[cs] = DEFWIDTH;
    precision[i] =  DEFPREC;  */

   for (r=0; r <= maxrow; r++) 
     {
	p = &tbl[r][maxcol];
	pmin = &tbl[r][cs];
	
	while (p > pmin)
	  {
	     if (0 != (p[0] = p[-1]))  p[0]->col++;
	     p--;
	  }
	pmin[0] = 0;
    }
   FullUpdate++;
   modflg++;
   Sc_Changed = 1;
   return 1;
}

/* This has the effect of freeing all structures associated with column cs
 * and pulling all columns beyond it over.  It seems that it is the opposite
 * of opencol.
 */
void closecol (int cs)
{
    register r;
    register struct ent **p;
    register struct ent *q;
    register c;
    register lim = maxcol-cs;
    int i;

    if (lim < 0) return;

    for (r=0; r <= maxrow; r++)
     {
	if (NULL != (q = tbl[r][cs])) 
	  {
	     sc_free_ent(q);
	  }
     }
   

    for (r=0; r<=maxrow; r++) 
     {
	p = &tbl[r][cs];
	for (c=lim; --c>=0; p++)
	  if (0 != (p[0] = p[1])) p[0]->col--;
	p[0] = 0;
     }

    for (i = cs; i < MAXCOLS - 1; i++) 
     {
	Sc_Col_Width[i] = Sc_Col_Width[i+1];
	precision[i] = precision[i+1];
	col_hidden[i] = col_hidden[i+1];
    }

   maxcol--;
   FullUpdate++;
   Sc_Changed = 1;
   modflg++;
}

void doend (int rowinc, int colinc)
{
    register struct ent *p;
    int r, c;

    if (VALID_CELL(p, currow, curcol)) 
     {
	r = currow + rowinc;
	c = curcol + colinc;
	if ((r >= 0) && (r < MAXROWS) &&
	    (c >= 0) && (c < MAXCOLS) &&
	    !VALID_CELL(p, r, c)) 
	  {
	     currow = r;
	     curcol = c;
	  }
     }

   if (!VALID_CELL(p, currow, curcol)) 
     {
        switch (rowinc) 
	  {
	   case -1:
	     while (!VALID_CELL(p, currow, curcol) && currow > 0)
	       currow--;
	     break;
	   case  1:
	     while (!VALID_CELL(p, currow, curcol) && currow < MAXROWS-1)
	       currow++;
	     break;
	   case  0:
	     switch (colinc) 
	       {
		case -1:
		  while (!VALID_CELL(p, currow, curcol) && curcol > 0)
		    curcol--;
		  break;
		case  1:
		  while (!VALID_CELL(p, currow, curcol) && curcol < MAXCOLS-1)
		    curcol++;
		  break;
	       }
	     break;
	  }
	
	sc_clear_message ();	/* clear line */
	return;
     }
   
   switch (rowinc) 
     {
      case -1:
	while (VALID_CELL(p, currow, curcol) && currow > 0)
	  currow--;
	break;
      case  1:
	while (VALID_CELL(p, currow, curcol) && currow < MAXROWS-1)
	  currow++;
	break;
      case  0:
	switch (colinc) 
	  {
	   case -1:
	     while (VALID_CELL(p, currow, curcol) && curcol > 0)
	       curcol--;
	     break;
	   case  1:
	     while (VALID_CELL(p, currow, curcol) && curcol < MAXCOLS-1)
	       curcol++;
	     break;
	  }
	break;
     }
   if (!VALID_CELL(p, currow, curcol)) 
     {
	currow -= rowinc;
	curcol -= colinc;
     }
}

void doformat (int c1, int c2, int w, int p)
{
   register int i;
   
   if (w > SLtt_Screen_Cols - RESCOL - 2) 
     {
	slsc_error("Format too large - Maximum = %d", SLtt_Screen_Cols - RESCOL - 2);
	w = SLtt_Screen_Cols-RESCOL-2;
     }

   if (p > w) 
     {
	slsc_error("Precision too large");
	p = w;
     }
   
   for (i = c1; i<=c2; i++)
     Sc_Col_Width[i] = w, precision[i] = p;
   
   FullUpdate++;
   modflg++;
}

static void print_options (FILE *f)
{
   if (autocalc &&
       (propagation == 10) &&
       (calc_order == BYROWS) &&
       !numeric &&
       (prescale == 1.0) &&
       !extfunc &&
       Highlight_Cell &&
       showtop &&
       (tbl_style == 0))
     return;		/* No reason to do this */

    (void) fprintf(f, "set");
   if (!autocalc) (void) fprintf(f," !autocalc");
   if (propagation != 10) (void) fprintf(f, " iterations = %d", propagation);
   if(calc_order != BYROWS)  (void) fprintf(f, " bycols");
   if (numeric)	(void) fprintf(f, " numeric");
   if (prescale != 1.0) (void) fprintf(f, " prescale");
   if (extfunc)	(void) fprintf(f, " extfun");
   if (!Highlight_Cell) (void) fprintf(f, " !cellcur");
   if (!showtop) (void) fprintf(f, " !toprow");
   if (tbl_style)
     (void) fprintf(f, " tblstyle = %s", tbl_style == TBL ? "tbl" :
		    tbl_style == LATEX ? "latex" :
		    tbl_style == TEX ? "tex" : 
 		    tbl_style == FRAME ? "frame" : "0" );
   
   (void) fprintf(f, "\n");
}

static void sc_print_file_to_fp (FILE *f, int r0, int rn, int c0, int cn)
{
   char pline[1000];
   int fieldlen, nextcol;
   register int row, col;
   int plinelim;
   register struct ent **p;
   
   for (row = r0; row <= rn; row++) 
     {
	register c = 0;
	
	if (row_hidden[row]) continue;
	
	pline[plinelim = 0] = '\0';
	
	for (p = &tbl[row][col=c0]; col<=cn;
	     p += nextcol-col, col = nextcol, c += fieldlen) 
	  {
	     nextcol = col + 1;
	     if (col_hidden[col]) 
	       {
		  fieldlen = 0;
		  continue;
	       }

	     fieldlen = Sc_Col_Width[col];
	     if (*p) 
	       {
		  char *s;
		  
		  while (plinelim < c) pline[plinelim++] = ' ';
		  plinelim = c;
		  if ((*p)->flags&IS_VALID) 
		    {
		       (void)sprintf (pline+plinelim,"%*.*f",Sc_Col_Width[col],
				      precision[col], (*p)->v);
		       plinelim += strlen (pline+plinelim);
		    }
		  if (0 != (s = (*p)->label))
		    {
		       int slen;
		       char *start, *last;
		       register char *fp;
		       struct ent *nc;
		       
		       /* Figure out if the label slops over to a blank field */
		       
		       /* JED: I changed the test from (nextcol < cn) to
			* (nextcol < MAXCOLS)
			*/
		       slen = strlen(s);
		       while (slen > fieldlen && (nextcol < MAXCOLS) &&
			      !((nc = lookat(row,nextcol))->flags & IS_VALID) 
			      && !(nc->label))
			 {
			
			    if (!col_hidden[nextcol])
		 	      fieldlen += Sc_Col_Width[nextcol];

			    nextcol++;
			 }
		       if (slen > fieldlen) slen = fieldlen;
		    
		       /* Now justify and print */
		       start = ((*p)->flags & IS_LEFTFLUSH ? pline + c
				: pline + c + fieldlen - slen);
		       last = pline + c + fieldlen;
		       fp = plinelim < c ? pline + plinelim : pline + c;
		       while (fp < start) *fp++ = ' ';
		       while (slen--) *fp++ = *s++;
		       if (!((*p)->flags & IS_VALID) || fieldlen != Sc_Col_Width[col])
		       	 while(fp < last) *fp++ = ' ';
		       if (plinelim < fp - pline) plinelim = fp - pline;
		    }
	       }
	  }
	pline[plinelim++] = '\n';
	pline[plinelim] = 0;
	(void) fputs (pline, f);
     }
}

void printfile (char *fname, int r0, int c0, int rn, int cn)
{
   FILE *f;
   int pid;
   char ch, lin[100];

   if (strcmp(fname, curfile) == 0) 
     {
	(void) SLsmg_gotorc (0, 0);
	(void) SLsmg_erase_eol ();
	(void) sprintf (lin,
			"Confirm that you want to destroy the data base: (y,n)");
	(void) SLsmg_write_string (lin);
	(void) SLsmg_refresh ();
	ch = SLang_getkey();
	if ((ch != 'y') && (ch != 'Y'))
	  return;
     }

   f = openout(fname, &pid);

   if (f == NULL)
     {
	slsc_error ("Can't create file \"%s\"", fname);
	return;
     }
   
   sc_print_file_to_fp (f, r0, rn, c0, cn);
   closeout(f, pid);
}

void tblprintfile (char *fname, int r0, int c0, int rn, int cn)
{
   FILE *f;
   int pid;
   register row, col;
   register struct ent **p;
   char coldelim = DEFCOLDELIM;
   char ch, lin[100];
   
   if (strcmp(fname, curfile) == 0) 
     {
	(void) SLsmg_gotorc (0, 0);
	(void) SLsmg_erase_eol ();
	(void) sprintf (lin,
			"Confirm that you want to destroy the data base: (y,n)");
	(void) SLsmg_write_string (lin);
	(void) SLsmg_refresh ();
	ch = SLang_getkey();
	if ((ch != 'y') && (ch != 'Y'))
	  return;
     }
   
   f = openout(fname, &pid);
   
   if (f == NULL)
     {
	slsc_error ("Can't create file \"%s\"", fname);
	return;
     }

   if (tbl_style == TBL) 
     {
	fprintf(f,".\\\" ** %s spreadsheet output \n.TS\n",Progname);
	fprintf(f,"tab(%c);\n",coldelim);
	for (col = c0; col <= cn; col++) fprintf(f," n");
	fprintf(f, ".\n"); 
     } 
   else if (tbl_style == LATEX) 
     {
	fprintf(f,"%% ** %s spreadsheet output\n\\begin{tabular}{",Progname);
	for (col=c0;col<=cn; col++) fprintf(f,"c");
	fprintf(f, "}\n");
	coldelim = '&';
     }
   else if ( tbl_style == TEX ) 
     {
	fprintf(f,"{\t%% ** %s spreadsheet output\n\\settabs %d \\columns\n",
		Progname, cn - c0 + 1);
	coldelim = '&';
     }
   else if (tbl_style == FRAME)
     {
 	fputs ("<MIFFile 3.00> # generated by the sc spreadsheet calculator\n", f);
 	fputs ("<Tbls\n", f);
 	fputs (" <Tbl \n", f);
 	fputs ("  <TblID 1> # This table's ID is 1\n", f);
 	fputs ("  <TblFormat \n", f);
 	fputs ("   <TblTag `Format A'> # Table Format Catalog\n", f);
 	fputs ("  > # end of TblFormat\n", f);
	  {
	     int i;
	     for (i=0, col=c0; col <= cn; col++) if (!col_hidden[col]) i++;
	     fprintf(f,"  <TblNumColumns %d> # Has %d columns\n", i, i);
	  }
 	fputs ("  <TblTitleContent\n", f);
 	fputs ("   <Para\n", f);
 	fputs ("    <PgfTag `TableTitle'> # Forces lookup in Paragraph Format Catalog\n", f);
 	fputs ("    <ParaLine\n", f);
 	fprintf(f,"     <String `%s'>\n",fname);
 	fputs ("    > # end of ParaLine\n", f);
 	fputs ("   > # end of Para\n", f);
 	fputs ("  > # end of TblTitleContent\n", f);
 	fputs ("  <TblH # The heading\n", f);
 	fputs ("   <Row # The heading row\n", f);
 	for (col=c0; col <= cn; col++) 
	  {
	     if (col_hidden[col]) continue;
	     fputs ("    <Cell <CellContent <Para # Cell in column \n", f);
	     fputs ("       <PgfTag `CellHeading'> # in Paragraph Format Catalog\n", f);
	     if (r0 == 0) fprintf(f,"       <ParaLine <String `%c'>>\n",'A'+col);
	     else 
	       {
		  char *s;
		  fputs ("       <ParaLine <String `", f);
		  p = &tbl[r0][col];
		  if (*p != NULL) 
		    {
		       if ((*p)->flags & IS_VALID)
		       	 (void) fprintf (f,"%.*f",precision[col], (*p)->v);

		       if (NULL != (s = (*p)->label)) fputs (s, f);
		    }
		  fputs ("'>>\n", f);
	       }
	     fputs ("    >>> # end of Cell\n", f);
	  }
 	fputs ("   > # end of Row\n", f);
 	fputs ("  > # end of TblH\n", f);
 	fputs ("  <TblBody # The body\n", f);
 	if (r0 != 0) r0++;
     }
   
  
   for (row=r0; row<=rn; row++) 
     {
 	if (tbl_style == TEX) fputs ("\\+", f);
 	else if (tbl_style == FRAME) fputs ("   <Row # The next body row\n", f);
	
  	for (p = &tbl[row][col=c0]; col<=cn; col++, p++) 
  	  {
 	     if (col_hidden[col]) continue; /* Do not print hidden collumns */
 	     if ( tbl_style == FRAME ) 
	       {
		  fputs ("    <Cell <CellContent <Para\n", f);
		  fputs ("       <PgfTag `CellBody'> # in Paragraph Format Catalog\n", f);
		  fputs ("       <ParaLine <String `", f);
	       } 
	     
  	     if (*p) 
  	       {
  		  char *s;

		  if ((*p)->flags&IS_VALID)
		    {
		       (void) fprintf (f,"%.*f",precision[col],
				       (*p)->v);
		    }
		  if (NULL != (s = (*p)->label)) fputs (s, f);
	       }
	     
 	     if (tbl_style == FRAME) 
	       {
		  fputs ( "'>>\n", f);
		  fputs ("    >>> # end of Cell\n", f);
	       }
	     
  	     if ( col < cn )
	       {
		  if (tbl_style != FRAME) putc (coldelim, f);
	       }
	  }
	
	if ( tbl_style == LATEX ) 
	  {
	     if ( row < rn ) (void) fputs ("\\\\", f);
	  }
	else if ( tbl_style == TEX ) 
	  {
	     (void) fputs ("\\cr", f);
	  }
 	else if ( tbl_style == FRAME )
 	  {
	     fputs ("   > # end of Row\n", f);
 	  }
	
	(void) putc ('\n', f);
     }
   
   if ( tbl_style == TBL )
     (void) fprintf (f,".TE\n.\\\" ** end of %s spreadsheet output\n", Progname);
   else if ( tbl_style == LATEX )
     (void) fprintf (f,"\\end{tabular}\n%% ** end of %s spreadsheet output\n", Progname);
   else if ( tbl_style == TEX )
     (void) fprintf (f,"}\n%% ** end of %s spreadsheet output\n", Progname);
   else if ( tbl_style == FRAME ) 
     {
 	fputs ("  > # end of TblBody\n", f);
 	fputs (" ># end of Tbl\n", f);
 	fputs ("> # end of Tbls\n", f);
 	fputs ("<TextFlow <Para \n", f);
 	fputs ("  <PgfTag Body> \n", f);
 	fputs ("  <ParaLine <ATbl 1>> # Reference to table ID 1\n", f);
 	fputs (">>\n", f);
    }

   closeout(f, pid);
}

static struct enode *copye (register struct enode *e,
			    int Rdelta, int Cdelta)
{
   register struct enode *ret;
   if (e == 0) 
     {
       ret = 0;
     } 
   else if (e->op & REDUCE) 
     {
	int newrow, newcol;
	ret = (struct enode *) SLMALLOC ((unsigned) sizeof (struct enode));
	ret->op = e->op;
	newrow = (e->e.r.left.vf & FIX_ROW ? e->e.r.left.vp->row :
		  e->e.r.left.vp->row+Rdelta);
	newcol = (e->e.r.left.vf & FIX_COL ? e->e.r.left.vp->col :
		  e->e.r.left.vp->col+Cdelta);
	
	ret->e.r.left.vp = lookat (newrow, newcol);
	ret->e.r.left.vf = e->e.r.left.vf;
	newrow=e->e.r.right.vf & FIX_ROW ? e->e.r.right.vp->row :
					   e->e.r.right.vp->row+Rdelta;
	newcol=e->e.r.right.vf & FIX_COL ? e->e.r.right.vp->col :
					   e->e.r.right.vp->col+Cdelta;
	ret->e.r.right.vp = lookat (newrow, newcol);
	ret->e.r.right.vf = e->e.r.right.vf;
    } 
   else 
     {
	ret = (struct enode *) SLMALLOC ((unsigned) sizeof (struct enode));
	ret->op = e->op;
	switch (ret->op) 
	  {
	   case 'v':
	       {
		  int newrow, newcol;
		  newrow=e->e.v.vf & FIX_ROW ? e->e.v.vp->row :
		  e->e.v.vp->row+Rdelta;
		  newcol=e->e.v.vf & FIX_COL ? e->e.v.vp->col :
		  e->e.v.vp->col+Cdelta;
		  ret->e.v.vp = lookat (newrow, newcol);
		  ret->e.v.vf = e->e.v.vf;
		  break;
	       }
	   case 'k':
	     ret->e.k = e->e.k;
	     break;
	   case 'f':
	     ret->e.o.right = copye (e->e.o.right,0,0);
	     ret->e.o.left = 0;
	     break;
	   case '$':
	     ret->e.s = SLMALLOC((unsigned) strlen(e->e.s)+1);
	     (void) strcpy(ret->e.s, e->e.s);
	     break;
	   default:
	     ret->e.o.right = copye (e->e.o.right,Rdelta,Cdelta);
	     ret->e.o.left = copye (e->e.o.left,Rdelta,Cdelta);
	     break;
	  }
     }
   return ret;
}

/*
 * sync_refs and syncref are used to remove references to
 * deleted struct ents.  Note that the deleted structure must still
 * be hanging around before the call, but not referenced by an entry
 * in tbl.  Thus the sc_free_ent, fix_ent calls in sc.c
 */

void sync_refs (void)
{
   register i,j;
   register struct ent *p;
   sc_sync_ranges();
   for (i=0; i <= maxrow; i++)
     {
	for (j=0; j<=maxcol; j++) if ((p=tbl[i][j]) && p->expr)
	  syncref(p->expr);
     }
}



void syncref(struct enode *e)
{
   if (e == NULL)
     return;
   else if (e->op & REDUCE) 
     {
 	e->e.r.right.vp = lookat(e->e.r.right.vp->row, e->e.r.right.vp->col);
 	e->e.r.left.vp = lookat(e->e.r.left.vp->row, e->e.r.left.vp->col);
     } 
   else 
     {
	switch (e->op) 
	  {
	   case 'v':
	     e->e.v.vp = lookat(e->e.v.vp->row, e->e.v.vp->col);
	     break;
	   case 'k':
	     break;
	   case '$':
	     break;
	   default:
	     syncref(e->e.o.right);
	     syncref(e->e.o.left);
	     break;
	  }
     }
}

void hiderow(int arg)
{
   register int r1;
   register int r2;

   r1 = currow;
   r2 = r1 + arg - 1;
   if ((r1 < 0) || (r1 > r2)) 
     {
	slsc_error ("Invalid range");
	return;
     }
   if (r2 > MAXROWS-2) 
     {
	slsc_error ("You can't hide the last row");
	return;
     }
   FullUpdate++;
   while (r1 <= r2)
     row_hidden[r1++] = 1;
}

void hidecol (int arg)
{
   register int c1;
   register int c2;

   c1 = curcol;
   c2 = c1 + arg - 1;
   if ((c1 < 0) || (c1 > c2)) 
     {
	slsc_error ("Invalid range");
	return;
     }
   if (c2 > MAXCOLS-2) 
     {
	slsc_error ("You can't hide the last column");
	return;
     }
   FullUpdate++;
   while (c1 <= c2)
     col_hidden[c1++] = 1;
}

void showrow(int r1, int r2)
{
   if ((r1 < 0) || (r1 > r2)) 
     {
	slsc_error ("Invalid range");
	return;
     }
   if (r2 > MAXROWS-1) 
     {
	r2 = MAXROWS-1;
     }
   FullUpdate++;
   while (r1 <= r2)
     row_hidden[r1++] = 0;
}

void showcol (int c1, int c2)
{
   if ((c1 < 0) || (c1 > c2))
     {
	slsc_error ("Invalid range");
	return;
     }
   if (c2 > MAXCOLS-1) 
     {
	c2 = MAXCOLS-1;
     }
   FullUpdate++;
   while (c1 <= c2)
     col_hidden[c1++] = 0;
}

/* Open the output file, setting up a pipe if needed */

FILE *openout (char *fname, int *rpid)
{
   int pipefd[2];
   int pid;
   FILE *f;
   
   /* Skip leading blanks */
   while (*fname && (*fname == ' ')) 
     fname++;

   if (*fname != '|') 
     {		/* Open file if not pipe */
	*rpid = 0;
	return(fopen(fname, "w"));
     }
   
   fname++;				/* Skip | */
   if ( pipe (pipefd) < 0) 
     {
	slsc_error("Can't make pipe to child");
	*rpid = 0;
	return(0);
     }

   sc_reset_display ();
#ifdef VMS
   fprintf(stderr, "No son tasks available yet under VMS--sorry\n");
#else /* VMS */

   if ((pid=fork()) == 0)			  /* if child  */
     {
	(void) close (0);			  /* close stdin */
	(void) close (pipefd[1]);
	(void) dup (pipefd[0]);		  /* connect to pipe input */
	(void) signal (SIGINT, SIG_DFL);	  /* reset */
	(void) execl ("/bin/sh", "sh", "-c", fname, 0);
	exit (-127);
    }
   else				  /* else parent */
     {
	*rpid = pid;
	f = fdopen (pipefd[1], "w");
	if (f == 0)
	  {
	     (void) kill (pid, -9);
	     slsc_error ("Can't fdopen output");
	     (void) close (pipefd[1]);
	     *rpid = 0;
	     return(0);
	  }
     }
#endif /* VMS */
   return(f);
}

void closeout(FILE *f, int pid)
{
   int temp;

   (void) fclose (f);
   
   if (pid) 
     {
	while (pid != wait(&temp)) /**/;
	(void) printf("Press RETURN to continue ");
	(void) fflush(stdout);
	(void) SLang_getkey();
	sc_init_display ();
     }
}

void copyent(register struct ent *n, register struct ent *p, int dr, int dc)
{
   if ((n == NULL) || (p == NULL))
     {
	slsc_error("internal error");
	return;
     }
   
   n -> v = p -> v;
   n -> flags = p -> flags;
   n -> expr = copye (p -> expr, dr, dc);
   n -> label = 0;
   if (p -> label) 
     {
	n -> label = (char *) SLMALLOC((unsigned) (strlen (p -> label) + 1));
	(void) strcpy (n -> label, p -> label);
     }
}

void write_fd (FILE *f, int r0, int c0, int rn, int cn)
{
   register struct ent **p;
   register r, c;

   (void) fprintf (f, "# This data file was generated by the Spreadsheet ");
   (void) fprintf (f, "Calculator.\n");
   (void) fprintf (f, "# You almost certainly shouldn't edit it.\n\n");
   print_options(f);
   for (c = 0; c < MAXCOLS; c++)
     {
	if ((Sc_Col_Width[c] != DEFWIDTH) || (precision[c] != DEFPREC))
	  (void) fprintf (f, "format %s %d %d\n",sc_coltoa(c),Sc_Col_Width[c],precision[c]);
     }
   
    for (c=c0; c < cn; c++) 
     {
        if (col_hidden[c]) 
	  {
	     (void) fprintf(f, "hide %s\n", sc_coltoa(c));
	  }
     }
   for (r = r0; r <= rn; r++) 
     {
	if (row_hidden[r]) 
	  {
	     (void) fprintf(f, "hide %d\n", r);
	  }
     }

   sc_write_range(f);

   if (mdir)  (void) fprintf(f, "mdir \"%s\"\n", mdir);
   for (r=r0; r<=rn; r++) 
     {
	p = &tbl[r][c0];
	for (c=c0; c<=cn; c++, p++) if (*p) 
	  {
	     if ((*p)->label) 
	       {
		  edits(r,c);
		  (void) fprintf(f, "%s\n",line);
	       }
	     if ((*p)->flags&IS_VALID) 
	       {
		  editv (r, c);
		  (void) fprintf (f, "%s\n",line);
	       }
	  }
     }
}

int writefile (char *fname, int r0, int c0, int rn, int cn)
{
   register FILE *f;
   char save[1024];
   int pid;

#ifndef VMS
   if (Crypt) 
     {
	return (cwritefile(fname, r0, c0, rn, cn));
     }
#endif /* VMS */

   if (*fname == 0) fname = &curfile[0];
   
   (void) strcpy(save,fname);

   f = openout(fname, &pid);
   
   if (f == 0) 
     {
	slsc_error ("Can't create file \"%s\"", fname);
	return (-1);
     }

   write_fd(f, r0, c0, rn, cn);
    
   closeout(f, pid);

   if (!pid) 
     {
        (void) strcpy(curfile, save);
        modflg = 0;
        sc_message ("File \"%s\" written.",curfile);
     }
   
    return (0);
}

int Sc_File_Line_Number;
void readfile (char *fname, int eraseflg)
{
   register FILE *f;
   char save[1024];
   int save_line = Sc_File_Line_Number;
   
   if ((*fname == '*') && mdir)
     {
	(void) strcpy(save, mdir);
	*fname = '/';
	(void) strcat(save, fname);
     } 
   else 
     {
        if (*fname == 0)
	  fname = &curfile[0];
        (void) strcpy(save,fname);
     }

#ifndef VMS
   if (Crypt)  
     {
	creadfile(save, eraseflg);
	return;
     }
#endif /* VMS */

   if (eraseflg && strcmp(fname,curfile) && modcheck(" first")) return;

   f = fopen (save, "r");
   if (f == 0) 
     {
	slsc_error ("Can't read file \"%s\"", save);
	return;
     }
   
   if (eraseflg) erasedb ();

   loading++;
   Sc_File_Line_Number = 0;
   while (fgets(line,sizeof line,f)) 
     {
	linelim = 0;
	Sc_File_Line_Number++;
	if (line[0] != '#') (void) yyparse ();
     }
   --loading;
   (void) fclose (f);
   linelim = -1;
   modflg++;
   if (eraseflg) 
     {
	(void) strcpy(curfile,save);
	modflg = 0;
     }
   Sc_File_Line_Number = save_line;
   sc_eval_all ();
}

void erasedb (void)
{
   register r, c;
   for (c = 0; c <= maxcol; c++) 
     {
	Sc_Col_Width[c] = DEFWIDTH;
	precision[c] = DEFPREC;
     }

   for (r = 0; r<=maxrow; r++) 
     {
	register struct ent **p = &tbl[r][0];
	for (c=0; c++<=maxcol; p++) if (*p) 
	  {
	     if ((*p)->expr) sc_efree ((*p) -> expr);
	     if ((*p)->label) SLFREE ((char *)((*p) -> label));
	     SLFREE ((char *)(*p));
	     *p = 0;
	  }
     }
   maxrow = 0;
   maxcol = 0;
   clean_range();
   FullUpdate++;
}

void backcol (int arg)
{
   while (--arg>=0) 
     {
	if (curcol)
	  curcol--;
	else
	  {
	     slsc_error ("At column A"); break;
	  }
	while (col_hidden[curcol] && curcol)
	  curcol--;
     }
}

void forwcol(int arg)
{
    while (--arg >= 0)
     {
	if (curcol < MAXCOLS - 1)
	  curcol++;
	else
	  {
	     slsc_error ("The table can't be any wider"); break;
	  }
	while (col_hidden[curcol] && (curcol < MAXCOLS - 1))
	  curcol++;
     }
}

void forwrow (int arg)
{
   while (--arg >= 0) 
     {
	if (currow < MAXROWS - 1)
	  currow++;
	else
	  {
	     slsc_error ("The table can't be any longer"); break;
	  }
	while (row_hidden[currow] && (currow < MAXROWS - 1))
	  currow++;
     }
}

void backrow (int arg)
{
   while (--arg >= 0) 
     {
	if (currow)
	  currow--;
	else
	  {
	     slsc_error ("At row zero"); break;
	  }
	while (row_hidden[currow] && currow)
	  currow--;
     }
}


/*
 * Show a cell's label string or expression value.  May overwrite value if
 * there is one already displayed in the cell.  Created from old code in
 * update(), copied with minimal changes.
 */

void showstring (
		 char *string,	       /* to display */
		 int leftflush,	       /* or rightflush */
		 int hasvalue,	       /* is there a numeric value? */
		 int row, int col,     /* spreadsheet location */
		 int *nextcolp,	       /* value returned through it */
		 int mxcol,	       /* last column displayed? */
		 int *fieldlenp,       /* value returned through it */
		 int r, int c)	       /* screen row and column */
{
   register int nextcol  = *nextcolp;
   register int fieldlen = *fieldlenp;

   char field[1024];
   int  slen;
   char *start, *last;
   register char *fp;
   struct ent *nc;
   
   /* This figures out if the label is allowed to
    slop over into the next blank field */
   
   slen = strlen (string);
   while ((slen > fieldlen) && (nextcol <= mxcol) &&
	  !((nc = lookat (row, nextcol)) -> flags & IS_VALID) &&
	  !(nc->label)) 
     {
	if (! col_hidden [nextcol])
	  fieldlen += Sc_Col_Width [nextcol];
	
	nextcol++;
     }
   if (slen > fieldlen)
     slen = fieldlen;
   
   /* Now justify and print */
   start = leftflush ? field : field + fieldlen - slen;
   last = field+fieldlen;
   fp = field;
   while (fp < start)
     *fp++ = ' ';
   while (slen--)
     *fp++ = *string++;
   if ((! hasvalue) || (fieldlen != Sc_Col_Width[col])) while (fp < last)
     *fp++ = ' ';
   *fp = 0;
   
   SLsmg_gotorc (r, c); SLsmg_write_string (field);
   
   *nextcolp  = nextcol;
   *fieldlenp = fieldlen;
}

int sc_etype(register struct enode *e)
{
   if (e == NULL) return 0;
   switch (e->op) 
     {
      case '+': case '-': case '*': case '/': case '%': case '^':
      case '<': case '=': case '>': case '&': case '|': case 'm':
      case '~': case 'k': case INDEX:
      case REDUCE | '+': case REDUCE | '*': case REDUCE | 'a':
      case REDUCE | 'A':
      case REDUCE | 'S':
      case REDUCE | 'P':
      case REDUCE | 'D':
      case REDUCE | 'M':
      case REDUCE | 'm':
      case REDUCE | 's': case REDUCE | MAX_FUN: case REDUCE | MIN_FUN:
      case ACOS: case ASIN: case ATAN: case ATAN2: case CEIL:
      case COS: case EXP: case FABS: case FLOOR: case HYPOT:
      case LOG: case LOG10: case POW: case SIN: case SQRT:
      case TAN: case DTR: case RTD: case RND: case FV: case PV:
      case PMT: case HOUR: case MINUTE: case SECOND: case MONTH:
      case DAY: case YEAR: case NOW: case STON: case EQS:
      case LMAX: case LMIN: case NVAL: case LOOKUP:
        return (NUM);

      case O_SCONST: case '#': case DATE: case FMT: case STINDEX:
      case EXT: case SVAL: case SUBSTR:
        return (STR);
	
    case 'f':  case '?':
        return(sc_etype(e->e.o.left));
	
      case O_VAR: 
	  {
	     register struct ent *p;
	     p = e->e.v.vp;
	     if (p->expr) 
	       return(p->flags & IS_STREXPR ? STR : NUM);
	     else if (p->label)
	       return(STR);
	     else
	       return(NUM);
	  }

      default:
	return(NUM);
    }
}

/* adjust maxrow/column */
void sc_adjust_maxrow_col (void)
{
   int r = maxrow, c = maxcol;
   
   while ((r > currow) && is_row_empty (r)) r--;
   while ((c > curcol) && is_col_empty (c)) c--;
   
   maxrow = r; maxcol = c;
}
