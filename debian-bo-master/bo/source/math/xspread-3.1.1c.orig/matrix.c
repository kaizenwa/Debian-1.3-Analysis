/*	SC	A Spreadsheet Calculator
 *		Command routines
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *
 *		$Revision: 1.1 $
 *
 *		Modified by Dan Coppersmith, 5/94
 */

#include <config.h>

#include <sys/types.h>
#include <ctype.h>
#include <curses.h>

#include "sc.h"

/* next two structures move to sc.h by Bob Parbs 12-92 */
/*struct m_range_sd {             
       int ssr,ser,ssc,sec;
       int dsr,der,dsc,dec;
      };*/
/*struct m_range {
       int sr,er,sc,ec;
      };*/
struct m_range_sd *rge_sd;
struct m_range *rge_s1, *rge_s2, *rge_d;

/*#define MAXCOLS 702*/   /* temp adds by Bob Parbs 12-92 */
/*#define MAXROWS 2500*/

/***************************************************************************/
/*                                                                         */
/* The following function calculate the transpose matrix of the input one  */
/*                                                                         */
/*                                            -Fang Wang   12/91           */
/***************************************************************************/
/* REG_LEN moved to sc.h by Bob Parbs 12-92 */
/*#define REG_LEN 28  length of input range string*/
void
get_trans()      /* void added by Bob Parbs 12-92 */
{
	char s[100];
	struct m_range_sd *find_rge(), *rge_sd;
	struct ent *dv1,*dv2,*v1,*v2;
	int minsr, minsc;
	int maxsr, maxsc;
	int mindr, mindc;
	int maxdr, maxdc;
	int vr, vc;
	int r, c;

  sprintf(s, "transpose [dest_range src_range] ");
  get_str(s,REG_LEN);
  rge_sd=find_rge(s);
  /*check for input error*/
  if (rge_sd == NULL) {
	scerror("Input syntax");
	return;
   } 
	dv1=lookat(rge_sd->dsr,rge_sd->dsc);
	dv2=lookat(rge_sd->der,rge_sd->dec);
	v1=lookat(rge_sd->ssr,rge_sd->ssc);
	v2=lookat(rge_sd->ser,rge_sd->sec);
	mindr = dv1->row;
	mindc = dv1->col;
	maxdr = dv2->row;
	maxdc = dv2->col;
	if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
	if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
	maxsr = v2->row;
	maxsc = v2->col;
	minsr = v1->row;
	minsc = v1->col;
	if (minsr>maxsr) r = maxsr, maxsr = minsr, minsr = r;
	if (minsc>maxsc) c = maxsc, maxsc = minsc, minsc = c;
	/*if (maxdr >= MAXROWS  || maxdc >= MAXCOLS) {
		scerror("The table can't be any bigger");
		return;
	}*/   /* commented out by Bob Parbs 12-92 */
	/*check for the matching of size of matrices*/
	if (((maxsr-minsr) != (maxdc-mindc)) || ((maxsc-minsc) != (maxdr-mindr)))
	{
		scerror("Destination matrix range, source matrix range don't match");
		return;
	}
	erase_area(mindr, mindc, maxdr, maxdc); /*clear the des. area*/
	if (minsr == maxsr && minsc == maxsc) {
		/* Source is a single cell */
		for(vr = mindr; vr <= maxdr; vr++)
			for (vc = mindc; vc <= maxdc; vc++)
				transpose(vr, vc, minsr, minsc, maxsr, maxsc);
	} else if (minsr == maxsr) {
		/* Source is a single row */
		for (vc = mindc; vc <= maxdc; vc++)
			transpose( mindr,vc, minsr, minsc, maxsr, maxsc);
	} else if (minsc == maxsc) {
        /* Source is a single column */
		for (vr = mindr; vr <= maxdr; vr++)
			transpose(vr, mindc, minsr, minsc, maxsr, maxsc);
	} else {
		/* Everything else */
		transpose(mindr, mindc, minsr, minsc, maxsr, maxsc);
	}
}

/*#ifdef notdef  SILLY PEOPLE... */
/***************************************************************************/
/*                                                                         */
/* The following function find the range upper left corner row,upper col   */
/* and  bottom right corner row and col.                                   */
/*                                                                         */
/*                                            -Fang Wang   12/91           */
/***************************************************************************/

/* ROWLIM, COLIM moved to sc.h by Bob Parbs 12-92 */
/*#define ROWLIM 4 size of the array stores col label*/
/*#define COLIM 3 size of the array stores row label*/
struct m_range_sd *
find_rge(s)
char *s;

{ char col[COLIM],row[ROWLIM];
  int i, j, mtemp[9];
  struct m_range_sd *rge_sd;

  for (i=0;i<=8;i++)
     mtemp[i] = 0;
  j = 0;
  while (*s != '\0') {
     i = 0;
     if ((*s == ':') || (*s == ' '))  s++;
     while (((*s <= 'Z') && (*s >= 'A')) || ((*s <= 'z') && (*s >= 'a')))
          col[i++] = *(s++);
     col[i] = '\0';
     if ((strlen(col) == 0 ) || (strlen(col) > (COLIM-1)))
         return(NULL);
     mtemp[j++] = convert(26,col,strlen(col));
     i = 0;
     while ((*s<='9') && (*s>='0'))
          row[i++] = *(s++);
     row[i] = '\0';
     if ((strlen(row) == 0 ) || (strlen(row) > (ROWLIM-1)))
         return(NULL);
     mtemp[j++] = convert(10,row,strlen(row));
  } /* end of while*/
  /* altered by Bob Parbs */
  rge_sd = (struct m_range_sd *) scxmalloc(sizeof(struct m_range_sd));
  rge_sd->dsc = mtemp[0];
  rge_sd->dsr = mtemp[1];
  rge_sd->dec = mtemp[2];
  rge_sd->der = mtemp[3];
  rge_sd->ssc = mtemp[4];
  rge_sd->ssr = mtemp[5];
  rge_sd->sec = mtemp[6];
  rge_sd->ser = mtemp[7];
  return(rge_sd);
}

/***************************************************************************
 *                                                                         *
 *    Convert the col, row into integer.                                   *
 *                                     -Fang Wang  12/91                   *
 **************************************************************************/

int convert( base, s, size)
int base,size;
char s[];

{ int val, i;
  int temp;

  temp = 0;
  val = 0;
  for (i=0; (i<size) && (s[i] != '\0'); i++) {
      if (((s[i]<='Z') && (s[i]>='A')) || ((s[i]<='z') && (s[i]>='a'))) {
          if (islower(s[i]))
               s[i] = toupper(s[i]);
      val = (temp*base) + ( s[i]-'A');
      temp = temp + 1;
      }
      else if ((s[i]<='9') && (s[i]>='0'))
          val = (val*base) + (s[i]-'0');
   }
   return (val);
}
/*#endif*/

/***************************************************************************
 *                                                                         *
 *    Calculates the transpose matrix.                                     *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 **************************************************************************/


void transpose(vr, vc, minsr, minsc, maxsr, maxsc) /* void added Bob Parbs */
int vr, vc, minsr, minsc, maxsr, maxsc;
{
    register struct ent *p;
    register struct ent *n;
    register int sr, sc;
    register int dr, dc;

    for (dc=vc, sr=minsr; sr<=maxsr; sr++, dc++) 
       for (dr=vr, sc=minsc; sc<=maxsc; sc++, dr++) {
            n = lookat (dr, dc);
            (void) clearent(n);
            if ((p = *ATBL(tbl, sr, sc)) != NULL)
                copyent( n, p, dr - sr, dc - sc);
        }
}


/***************************************************************************
 *                                                                         *
 *    Add two matrices.                                                    *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/

/* TSSIZE move to sc.h Bob Parbs 12-92 */
/*#define TSSIZE 15*/
void get_add()       /* void added Bob Parbs 12-92 */
{
  char s[100];
  struct m_range *findrge(),*rge_s1,*rge_s2,*rge_d;
  struct ent *dmin,*dmax,*s1min,*s1max,*s2min,*s2max;
  int mins1r, mins1c;
  int maxs1r, maxs1c;
  int mins2r, mins2c;
  int maxs2r, maxs2c;
  int mindr, mindc;
  int maxdr, maxdc;
  int vr, vc;
  int r, c;

/*#ifdef notdef*/
  sprintf(s,"1st matrix range:");
  get_str(s,TSSIZE);
  rge_s1 = findrge(s);
  if (rge_s1 == NULL) {
     scerror("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"2nd matrix range:");
  get_str(s,TSSIZE);
  rge_s2 = findrge(s);
  if (rge_s2 == NULL) {
     scerror("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"result matrix range:");
  get_str(s,TSSIZE);
  rge_d = findrge(s);
  if (rge_d == NULL) {
     scerror("Input syntax error");
     return;
  }
/*#endif*/
  dmin=lookat(rge_d->sr,rge_d->sc);
  dmax=lookat(rge_d->er,rge_d->ec);
  s1min=lookat(rge_s1->sr,rge_s1->sc);
  s1max=lookat(rge_s1->er,rge_s1->ec);
  s2min=lookat(rge_s2->sr,rge_s2->sc);
  s2max=lookat(rge_s2->er,rge_s2->ec);
  mindr=dmin->row;
  mindc=dmin->col;
  maxdr=dmax->row;
  maxdc=dmax->col;
  if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
  if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
  maxs1r = s1max->row;
  maxs1c = s1max->col;
  mins1r = s1min->row;
  mins1c = s1min->col;
  maxs2r = s2max->row;
  maxs2c = s2max->col;
  mins2r = s2min->row;
  mins2c = s2min->col;
  if (mins1r>maxs1r) r = maxs1r, maxs1r = mins1r, mins1r = r;
  if (mins1c>maxs1c) c = maxs1c, maxs1c = mins1c, mins1c = c;
  if (mins2r>maxs2r) r = maxs2r, maxs2r = mins2r, mins2r = r;
  if (mins2c>maxs2c) c = maxs2c, maxs2c = mins2c, mins2c = c;
  /*if (maxdr >= MAXROWS  ||
         maxdc >= MAXCOLS) {
     scerror("The table can't be any bigger");
     return;
  }*/         /* commented out by Bob Parbs 12-92 */
  if (((maxs1r-mins1r) != (maxs2r-mins2r)) ||
                      ((maxs2c-mins2c) != (maxs1c-mins1c))) {
      scerror("Size of two matrices don't match, can't be added");
      return;
     }
  if (((maxs1r-mins1r) != (maxdr-mindr)) ||
                      ((maxdc-mindc) != (maxs1c-mins1c))) {
      scerror("Size of source and destination matrices don't match");
      return;
     }
  erase_area(mindr, mindc, maxdr, maxdc);
  if (mins1r == maxs1r && mins1c == maxs1c && mins2r == maxs2r &&
        mins2c == maxs2c )  {
        /* Source is a single cell */
        for(vr = mindr; vr <= maxdr; vr++)
            for (vc = mindc; vc <= maxdc; vc++)
               addmatrix(vr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else if (mins1r == maxs1r && mins2r == maxs2r) {
        /* Source is a single row */
        for (vr = mindr; vr <= maxdr; vr++)
               addmatrix(vr,mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else if (mins1c == maxs1c && mins2c == maxs2c ) {
        /* Source is a single column */
        for (vc = mindc; vc <= maxdc; vc++)
           addmatrix(mindr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else {
        /* Everything else */
        addmatrix(mindr, mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    }
}

/*#ifdef notdef  SILLY */
/***************************************************************************
 *                                                                         *
 *    Get range for when input one by one.                                 *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/
/* ROWLIM, COLIM moved to sc.h Bob Parbs 12-92 */
/*#define ROWLIM 4*/
/*#define COLIM 3*/

struct m_range *findrge(s)
char *s;

{ char col[COLIM],row[ROWLIM];
  int i, j, mtemp[5];
  struct m_range *rge;

  for (i=0;i<=4;i++)
     mtemp[i] = 0;
  j = 0;
  while (*s != '\0') {
     i = 0;
     if (*s == ':')  s++;
     while (((*s <= 'Z') && (*s >= 'A')) || ((*s <= 'z') && (*s >= 'a')))
          col[i++] = *(s++);
     col[i] = '\0';
     if ((strlen(col) == 0 ) || (strlen(col) > (COLIM-1)))
          return(NULL);
     mtemp[j++] = convert(26,col,strlen(col));
     i = 0;
     while ((*s<='9') && (*s>='0'))
          row[i++] = *(s++);
     row[i] = '\0';
     if ((strlen(row) == 0 ) || (strlen(row) > (ROWLIM-1)))
         return(NULL);
     mtemp[j++] = convert(10,row,strlen(row)); 
  } /* end of while*/
  /* altered by Bob Parbs 12-92 */
  rge = (struct m_range *) scxmalloc(sizeof(struct m_range));
  rge->sc = mtemp[0];
  rge->sr = mtemp[1];
  rge->ec = mtemp[2];
  rge->er = mtemp[3];
  return(rge);
} 
/*#endif*/

/***************************************************************************
 *                                                                         *
 *    Calculates addition of two matrices.                                 *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/

/* Function now works 12-92 Bob Parbs */

/* MAXSS no longer needed Bob Parbs 12-92 */
/*#define MAXSS 2000*/
void addmatrix(vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c)
/* void added Bob Parbs 12-92 */
int vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c;
{
    register struct ent *p1,*p2;
    register struct ent *n;
    register int s1r, s1c, s2r, s2c;
    register int dr, dc;
    /*int i,j;*/    /* no longer needed B.P. */
    /*int list1[MAXSS],list2[MAXSS];*/ /*commented out by Bob Parbs 12-92 */

    /* lists no longer needed as values added to dest as read */
    /* used to only add int parts, now adds entire entries  Bob Parbs */


    /* following loops to fill list1, list 2 no longer needed B.P. */ 
     /*for (i=0;i<=MAXSS;i++)
        {
            list1[i] = 0;
            list2[i] = 0;
        } 
     i = 0;
     for (s1r=mins1r;s1r<=maxs1r;s1r++)
        for (s1c=mins1c;s1c<=maxs1c;s1c++)
            if ((p1 = *ATBL(tbl, s1r, s1c)) && p1->flags&is_valid)
                   list1[i++] = p1->v;
     j = 0;
     for (s2r=mins2r;s2r<=maxs2r;s2r++)
        for (s2c=mins2c;s2c<=maxs2c;s2c++)
            if ((p2 = *ATBL(tbl, s2r, s2c)) && p2->flags&is_valid)
                  list2[j++] = p2->v;
     for (i=0;i<=MAXSS;i++)
          list1[i] = list1[i] + list2[i];
     i = 0;*/

     /* altered the following to add entries as read  Bob Parbs 12-92 */
     /* add much shorter now and works correctly B.P. */

     for (dr=vr, s1r=mins1r,s2r=mins2r; s1r<=maxs1r; s1r++,dr++,s2r++)
       for (dc=vc, s1c=mins1c,s2c=mins2c; s1c<=maxs1c; s1c++, dc++,s2c++) {
            n = lookat (dr, dc);
            (void) clearent(n);
            if ((p2 = *ATBL(tbl,s2r,s2c))&&(p2->flags&is_valid)){
            if ((p1 = *ATBL(tbl, s1r, s1c)) && (p1->flags&is_valid))
                {
                   copyent( n, p1, dr - s1r, dc - s1c);
                   n->v = (p1->v+p2->v);
                 }}
            }
}

/***************************************************************************
 *                                                                         *
 *    Subtraction of two matrices.                                         *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/
/* TSSIZE moved to sc.h Bob Parbs 12-92 */
/*#define TSSIZE 15*/
void get_sub()    /* void added Bob Parbs 12-92 */
{
  char s[100];
  struct m_range *findrgt(),*rge_s1,*rge_s2;
  struct ent *dmin,*dmax,*s1min,*s1max,*s2min,*s2max;
  int mins1r, mins1c;
  int maxs1r, maxs1c;
  int mins2r, mins2c;
  int maxs2r, maxs2c;
  int mindr, mindc;
  int maxdr, maxdc;
  int vr, vc;
  int r, c;

  sprintf(s,"1st matrix range:");
  get_str(s,TSSIZE);
  rge_s1 = findrge(s);
  if (rge_s1 == NULL ) {
     scerror("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"2nd matrix range:");
  get_str(s,TSSIZE);
  rge_s2 = findrge(s);
  if (rge_s2 == NULL ) {
     scerror("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"result matrix range:");
  get_str(s,TSSIZE);
  rge_d = findrge(s);
  if (rge_s2 == NULL ) {
     scerror("Input syntax error");
     return;
  }
  dmin=lookat(rge_d->sr,rge_d->sc);
  dmax=lookat(rge_d->er,rge_d->ec);
  s1min=lookat(rge_s1->sr,rge_s1->sc);
  s1max=lookat(rge_s1->er,rge_s1->ec);
  s2min=lookat(rge_s2->sr,rge_s2->sc);
  s2max=lookat(rge_s2->er,rge_s2->ec);
  mindr=dmin->row;
  mindc=dmin->col;
  maxdr=dmax->row;
  maxdc=dmax->col;
  if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
  if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
  maxs1r = s1max->row;
  maxs1c = s1max->col;
  mins1r = s1min->row;
  mins1c = s1min->col;
  maxs2r = s2max->row;
  maxs2c = s2max->col;
  mins2r = s2min->row;
  mins2c = s2min->col;
  if (mins1r>maxs1r) r = maxs1r, maxs1r = mins1r, mins1r = r;
  if (mins1c>maxs1c) c = maxs1c, maxs1c = mins1c, mins1c = c;
  if (mins2r>maxs2r) r = maxs2r, maxs2r = mins2r, mins2r = r;
  if (mins2c>maxs2c) c = maxs2c, maxs2c = mins2c, mins2c = c;
  /*if (maxdr >= MAXROWS  ||
         maxdc >= MAXCOLS) {
     scerror("The table can't be any bigger");
     return;
  }*/       /* commented out by Bob Parbs 12-92 */
  if (((maxs1r-mins1r) != (maxs2r-mins2r)) ||
                      ((maxs2c-mins2c) != (maxs1c-mins1c))) {
      scerror("Size of two matrices don't match, can't be subtracted");
      return;
     }
  if (((maxs1r-mins1r) != (maxdr-mindr)) ||
                      ((maxdc-mindc) != (maxs1c-mins1c))) {
      scerror("Size of source and destination matrices don't match");
      return;
     }
  erase_area(mindr, mindc, maxdr, maxdc);
    if (mins1r == maxs1r && mins1c == maxs1c && mins2r == maxs2r &&
        mins2c == maxs2c )  {
        /* Source is a single cell */
        for(vr = mindr; vr <= maxdr; vr++)
            for (vc = mindc; vc <= maxdc; vc++)
               submatrix(vr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else if (mins1r == maxs1r && mins2r == maxs2r) {
        /* Source is a single row */
        for (vr = mindr; vr <= maxdr; vr++)
               submatrix(vr, mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else if (mins1c == maxs1c && mins2c == maxs2c ) {
        /* Source is a single column */
        for (vc = mindc; vc <= maxdc; vc++)
           submatrix(mindr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    } else {
        /* Everything else */
        submatrix(mindr, mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    }

}

/***************************************************************************
 *                                                                         *
 *    Calculates Subtraction of two matrices.                              *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/

/* Function now works 12-92 by Bob Parbs */

/* MAXSS no longer needed, similar changes as to addmatrix Bob Parbs 12-92 */
/*#define MAXSS 2000*/
void submatrix(vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c)
/* void added Bob Parbs 12-92 */
int vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c;
{
    register struct ent *p1,*p2;
    register struct ent *n;
    register int s1r, s1c, s2r, s2c;
    register int dr, dc;
    /*int i,j;*/                       /* no longer needed B.P. */
    /*int list1[MAXSS],list2[MAXSS];*/ /* no longer needed B.P. */

    /* following loops to fill list1,list2 no longer needed Bob Parbs 12-92 */ 
    /*for (i=0;i<=MAXSS;i++)
        {
            list1[i] = 0;
            list2[i] = 0;
        }
    i = 0;
    for (s1r=mins1r;s1r<=maxs1r;s1r++)
        for (s1c=mins1c;s1c<=maxs1c;s1c++)
            if ((p1 = *ATBL(tbl, s1r, s1c)) && p1->flags&is_valid)
                   list1[i++] = p1->v;
    j = 0;
    for (s2r=mins2r;s2r<=maxs2r;s2r++)
        for (s2c=mins2c;s2c<=maxs2c;s2c++)
            if ((p2 = *ATBL(tbl, s2r, s2c)) && p2->flags&is_valid)
                  list2[j++] = p2->v;
    for (i=0;i<=MAXSS;i++)
          list1[i] = list1[i] - list2[i];
    i = 0;*/

    /* following loop modified to subtract entries as read Bob Parbs 12-92 */

    for (dr=vr, s1r=mins1r,s2r=mins2r; s1r<=maxs1r; s1r++,dr++,s2r++)
       for (dc=vc, s1c=mins1c,s2c=mins2c; s1c<=maxs1c; s1c++, dc++,s2c++) {
            n = lookat (dr, dc);
            (void) clearent(n);
            if ((p2 = *ATBL(tbl,s2r,s2c))&&(p2->flags&is_valid)){
            if ((p1 = *ATBL(tbl, s1r, s1c)) && (p1->flags&is_valid))
                {
                   copyent( n, p1, dr - s1r, dc - s1c);
                   n->v = (p1->v-p2->v);
                 }}
            }
}
/***************************************************************************
 *                                                                         *
 *    Multiplication of two matrices.                                      *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/
/* TSSIZE moved to sc.h Bob Parbs 12-92*/
/*#define TSSIZE 15*/
void get_mult()     /* void added Bob Parbs 12-92 */
{
  char s[100];
  struct m_range *findrge(),*rge_s1,*rge_s2,*rge_d;
  struct ent *dmin,*dmax,*s1min,*s1max,*s2min,*s2max;
  int mins1r, mins1c;
  int maxs1r, maxs1c;
  int mins2r, mins2c;
  int maxs2r, maxs2c;
  int mindr, mindc;
  int maxdr, maxdc;
  int vr, vc;
  int r, c;

  sprintf(s,"1st matrix range:");
  get_str(s,TSSIZE);
  rge_s1 = findrge(s);
  if (rge_s1 == NULL) {
     scerror("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"2nd matrix range:");
  get_str(s,TSSIZE);
  rge_s2 = findrge(s);
  if (rge_s2 == NULL) {
     scerror("Input syntax error");
     return;
  }
  clearlines(0,0);
  sprintf(s,"result matrix range:");
  get_str(s,TSSIZE);
  rge_d = findrge(s);
  if (rge_d == NULL) {
     scerror("Input syntax error");
     return;
  }
  dmin=lookat(rge_d->sr,rge_d->sc);
  dmax=lookat(rge_d->er,rge_d->ec);
  s1min=lookat(rge_s1->sr,rge_s1->sc);
  s1max=lookat(rge_s1->er,rge_s1->ec);
  s2min=lookat(rge_s2->sr,rge_s2->sc);
  s2max=lookat(rge_s2->er,rge_s2->ec);
  mindr=dmin->row;
  mindc=dmin->col;
  maxdr=dmax->row;
  maxdc=dmax->col;
  if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
  if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
  maxs1r = s1max->row;
  maxs1c = s1max->col;
  mins1r = s1min->row;
  mins1c = s1min->col;
  maxs2r = s2max->row;
  maxs2c = s2max->col;
  mins2r = s2min->row;
  mins2c = s2min->col;
  if (mins1r>maxs1r) r = maxs1r, maxs1r = mins1r, mins1r = r;
  if (mins1c>maxs1c) c = maxs1c, maxs1c = mins1c, mins1c = c;
  if (mins2r>maxs2r) r = maxs2r, maxs2r = mins2r, mins2r = r;
  if (mins2c>maxs2c) c = maxs2c, maxs2c = mins2c, mins2c = c;
  /*if (maxdr >= MAXROWS  ||
         maxdc >= MAXCOLS) {
     scerror("The table can't be any bigger");
     return;
  } */        /* commented out by Bob Parbs 12-92 */
  if ((maxs1c-mins1c) != (maxs2r-mins2r))
       {
           scerror("The size of two matrices doesn't match,cann't multply");
           return;
       }
  if (((maxs1r-mins1r) != (maxdr-mindr)) ||
                      ((maxdc-mindc) != (maxs2c-mins2c))) {
      scerror("Size of source and destination matrices don't match");
      return;
     }
  erase_area(mindr, mindc, maxdr, maxdc);
    if (mins1r == maxs1r && mins1c == maxs1c && mins2r == maxs2r &&
        mins2c == maxs2c )  {
       /* Source is a single cell */
        for(vr = mindr; vr <= maxdr; vr++)
            for (vc = mindc; vc <= maxdc; vc++)
              multmatrix(vr, vc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    }
/* else if ((mins2c==maxs2c) && (mins1r != maxs1r)) {
         for (vr=mindr;vr<=maxdr;vc++)
              multmatrix1(vr,mindc,mins1r,mins1c,maxs1r,maxs1c, 
                                      mins2r,mins2c,maxs2r,maxs2c);
    }
*/
 else {
        /* Everything else */
        multmatrix(mindr, mindc, mins1r, mins1c, maxs1r, maxs1c,
                                 mins2r, mins2c, maxs2r, maxs2c);
    }
}


/*
#define MINMAX 25
multmatrix1 (vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c)
int vr,vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c;
{
    register struct ent *p1,*p2;
    register struct ent *n;
    register int s1r, s1c, s2r, s2c;
    register int dr, dc;
    double sum;
    int i,j,k;
    double list1[MINMAX][MINMAX];
    int list2[MINMAX];
    int list3[MINMAX];

    for (i=0;i<MINMAX;i++)
      {
          list2[MINMAX] = 0;
          for (j=0;j<MINMAX;j++)
              list1[i][j] = 0;
      }
     i = 0;
     for (s1r=mins1r;s1r<=maxs1r;s1r++)
       {
        j = 0;
        for (s1c=mins1c;s1c<=maxs1c;s1c++)
            {
               if ((p1 = *ATBL(tbl, s1r, s1c)) && p1->flags&is_valid)
                    list1[i][j++] = p1->v;
            }
        i++;
       }
     j = 0;
     for (s2r=mins2r;s2r<=maxs2r;s2r++)
            if ((p2 = *ATBL(tbl, s2r, mins2c)) && p2->flags&is_valid)
                  list2[j++] = p2->v;
     for (i=0;i<MINMAX;i++)
        {
            sum = 0;
            for (k=0;k<MINMAX;k++)
               sum = sum + list1[i][k] * list2[k];
            list3[i++] = sum;
        }
     i = 0;
     for (dr=vr, s1r=mins1r; s1r<=maxs1r; s1r++,dr++)
         {
             n = lookat (dr, vc);
            (void) clearent(n);
            if ((p1 = *ATBL(tbl, s1r, mins1c)) && (p1->flags&is_valid))
                {
                   copyent( n, p1, dr - s1r, dc - s1c);
                   n->v = list3[i++];
                 }
       }
}


*/
/***************************************************************************
 *                                                                         *
 *    Calcultes Multiplication of two matrices.                            *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/
/* MINMAX moved to sc.h Bob Parbs 12-92 */
/*#define MINMAX 25*/
void multmatrix(vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c)
/* void added Bob Parbs 12-92 */
int vr, vc, mins1r, mins1c, maxs1r, maxs1c,mins2r,mins2c,maxs2r,maxs2c;
{
    register struct ent *p1,*p2;
    register struct ent *n;
    register int s1r, s1c, s2r, s2c;
    register int dr, dc;
    double sum;
    int i,j,k;
    double list1[MINMAX][MINMAX];
    double list2[MINMAX][MINMAX];
    double list3[MINMAX][MINMAX];

     for (i=0;i<MINMAX;i++)
       for (j=0;j<MINMAX;j++)
        {
            list1[i][j] = 0;
            list2[i][j] = 0;
            list3[i][j] = 0;
        }
     i = 0;
     for (s1r=mins1r;s1r<=maxs1r;s1r++)
       {
        j = 0;
        for (s1c=mins1c;s1c<=maxs1c;s1c++) 
               if ((p1 = *ATBL(tbl, s1r, s1c)) && p1->flags&is_valid)
                    list1[i][j++] = p1->v;
        i++;
       }
          
     i = 0;
     for (s2r=mins2r;s2r<=maxs2r;s2r++)
       {
         j = 0;
         for (s2c=mins2c;s2c<=maxs2c;s2c++)
            if ((p2 = *ATBL(tbl, s2r, s2c)) && p2->flags&is_valid)
                  list2[i][j++] = p2->v;
         i++;
       }
     for (i=0;i<MINMAX;i++)
           for (j=0;j<MINMAX;j++)
              {
                 sum = 0;
                 for (k=0;k<MINMAX;k++)
                     sum = sum + (list1[i][k]) * (list2[k][j]);
                 list3[i][j] = sum;
               }
     i = 0;
     for (dr=vr, s1r=mins1r; s1r<=maxs1r; s1r++,dr++)
       {
          j = 0;
          for (dc=vc, s1c=mins2c; s1c<=maxs2c; s1c++, dc++) {
            n = lookat (dr, dc);
            (void) clearent(n);
            if ((p1 = *ATBL(tbl, s1r, s1c)) && (p1->flags&is_valid))
                {
                   copyent( n, p1, dr - s1r, dc - s1c);
                   n->v = list3[i][j++];
                 }
            }
           i++;
       }
}


/***************************************************************************
 *                                                                         *
 *    Invert the matrix.                                                    *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/
/* REG_LEN moved to sc.h Bob Parbs 12-92 */
/*#define REG_LEN 28*/
void get_invert()     /* void added Bob Parbs 12-92 */
{
  char s[100];
  struct m_range_sd *find_rge();
  struct ent *dv1,*dv2,*v1,*v2;
  int minsr, minsc;
  int maxsr, maxsc;
  int mindr, mindc;
  int maxdr, maxdc;
  int vr, vc;
  int r, c;


  sprintf(s, "invert [dest_range src_range] ");
  get_str(s,REG_LEN);
  rge_sd=find_rge(s);
  if (rge_sd == NULL) {
      scerror("Input syntax error");
      return;
  }

/* FIXME-real stuff (use this, or something close...:-)
		maxr = e->e.o.right->e.r.right.vp -> row;
		maxc = e->e.o.right->e.r.right.vp -> col;
		minr = e->e.o.right->e.r.left.vp -> row;
		minc = e->e.o.right->e.r.left.vp -> col;
		if (minr>maxr) r = maxr, maxr = minr, minr = r;
		if (minc>maxc) c = maxc, maxc = minc, minc = c;
*/
  dv1=lookat(rge_sd->dsr,rge_sd->dsc);
  dv2=lookat(rge_sd->der,rge_sd->dec);
  v1=lookat(rge_sd->ssr,rge_sd->ssc);
  v2=lookat(rge_sd->ser,rge_sd->sec);
  mindr = dv1->row;
  mindc = dv1->col;
  maxdr = dv2->row;
  maxdc = dv2->col;
  if (mindr>maxdr)
  {	r = maxdr, maxdr = mindr, mindr = r;
  }
  if (mindc>maxdc)
  {	c = maxdc, maxdc = mindc, mindc = c;
  }
  maxsr = v2->row;
  maxsc = v2->col;
  minsr = v1->row;
  minsc = v1->col;
  if (minsr>maxsr) r = maxsr, maxsr = minsr, minsr = r;
  if (minsc>maxsc) c = maxsc, maxsc = minsc, minsc = c;
  /*if (maxdr >= MAXROWS  ||
         maxdc >= MAXCOLS) {
     scerror("The table can't be any bigger");
     return;
  }*/   /* commented out by Bob Parbs 12-92 */
  if (((maxsr-minsr) != (maxdr-mindr)) ||
                      ((maxdc-mindc) != (maxsc-minsc))) {
      scerror("Size of source and destination matrices don't match");
      return;
     }
  erase_area(mindr, mindc, maxdr, maxdc);
  if (minsr == maxsr && minsc == maxsc) {
        /* Source is a single cell */
        for(vr = mindr; vr <= maxdr; vr++)
            for (vc = mindc; vc <= maxdc; vc++)
               invertmatrix(vr, vc, minsr, minsc, maxsr, maxsc);
   } else {
        /* Everything else */
        invertmatrix(mindr, mindc, minsr, minsc, maxsr, maxsc);
   }
}

/***************************************************************************
 *                                                                         *
 *    Calcultes the inversion of the matrix.                               *
 *                                                                         *
 *                                     -Fang Wang  12/91                   *
 ***************************************************************************/
/* MAXROW, MAXCOL moved to sc.h Bob Parbs 12-92 */
/*#define MAXROW 30*/
/*#define MAXCOL 30*/
void invertmatrix(vr, vc, minsr, minsc, maxsr, maxsc)/* void added Bob Parbs*/
int vr, vc, minsr, minsc, maxsr, maxsc;
{
    register struct ent *p;
    register struct ent *q;
    register int sr, sc;
    register int dr, dc;
    double list[MAXROW][MAXCOL];
    /* changed from MAXROWS, MAXCOLS B.P. */
    int i,j,k,m,n;
    int rowlim,collim;
    double temp;
   
    for (i=0; i<((maxsr-minsr)*2+1); i++)
      for (j=0; j<((maxsc-minsc)*2+1); j++)
          list[i][j] = 0;
    rowlim = maxsr-minsr+1;
    collim = maxsc-minsc+1;
    for (i=0;i<rowlim;i++)
       for (j=0;j<collim;j++)
          if (i==j)  list[i][j] = 1;

    for (i=0,sr=minsr;(i<rowlim) && (sr<=maxsr); i++,sr++)
       for (j=collim,sc=minsc;(j<(2*collim)) && (sc<=maxsc); j++,sc++)
           if ((p = *ATBL(tbl, sr, sc)) && p->flags&is_valid)
                list[i][j] = p->v;
    for (j=collim; j<(2*collim); j++)  {
        k = j-collim;
        temp = list[k][j];
        for (i=0; i<(2*collim); i++)
            list[k][i] = list[k][i] / temp;
        for (m=0; m < rowlim; m++)
            for (n=0; n<(2*collim); n++)
               if (( m != k ) && ( n != j )) 
                  list[m][n] = list[m][n]-list[k][n]*list[m][j];
    }
    i = 0;
    for (dr=vr, sr=minsr; sr<=maxsr; sr++,dr++)
       {
          j = 0;
          for (dc=vc, sc=minsc; sc<=maxsc; sc++, dc++) {
            q = lookat (dr, dc);
            (void) clearent(q);
            if ((p = *ATBL(tbl, sr, sc)) && (p->flags&is_valid))
                {
                   copyent( q, p, dr - sr, dc - sc);
                   q->v = list[i][j++];
                 }
            }
           i++;
       }
}
/*#endif*/
