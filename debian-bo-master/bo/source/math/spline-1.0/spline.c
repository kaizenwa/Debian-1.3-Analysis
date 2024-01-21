/***************************************************************************
 * spline.c								   *
 *								           *	
 * spline does an Akima spline interpolation.                              *
 * 								           *
 * spline is (c) David Frey, 1996					   *
 *								           * 
 * This program is free software; you can redistribute it and/or modify it *
 * under the terms of the GNU General Public License as published by the   *
 * Free Software Foundation; either version 2 of the License, or (at your  *
 * option) any later version.                                              *
 *									   * 
 * This program is distributed in the hope that it will be useful, but     *
 * WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       *
 * General Public License for more details.                                *
 *									   * 
 * You should have received a copy of the GNU General Public License       *
 * along with this program; if not, write to the Free Software Foundation, *
 * Inc., 59 Temple Place Suite 330, Boston, MA 02111-1307 USA.             *
 ***************************************************************************/

/* $Id: spline.c,v 1.2 1996/02/11 22:00:00 david Rel $
 *
 * $Log: spline.c,v $
 * Revision 1.2  1996/02/11 22:00:00  david
 * Duplicated samples (those with identical abscissas) will be dropped.
 *
 * Revision 1.2  1996/02/11 21:49:20  david
 * Duplicate samples (i.e. samples with identical abscissas) will be dropped.
 *
 * Revision 1.2  1996/02/11 20:44:10  david
 * Duplicate samples will be sorted out.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>

#include "utils.h"

#define DXSTEP	     1.0
#define INTERVALLS 100

char *progname;
float  *x=NULL,  *y=NULL;		/* the values to interpolate */
float *dx=NULL, *dy=NULL;		/* x[i+1]-x[i], resp. y[i+1]-y[i] */
float  *m=NULL,  *t=NULL;		/* the slopes */
float  *B=NULL,  *C=NULL, *D=NULL;	/* the coefficients */

static struct option const long_options[] =
{
  {"auto",     optional_argument, 0, 'a'},
  {"help",     no_argument,       0, 'h'},
  {"help",     no_argument,       0, '?'},
  {"llimit",   required_argument, 0, 'l'},
  {"ulimit",   required_argument, 0, 'u'},
  {"points",   required_argument, 0, 'n'},
  {"version",  no_argument,       0, 0  },
  {"warranty", no_argument,       0, 'w'},
  {0,          0,                 0, 0  }
};

int warranty(void)		/* say that there is no warranty */
{
  printf("\
  This program is free software; you can redistribute it and/or modify it 
  under the terms of the GNU General Public License as published by the 
  Free Software Foundation; either version 2 of the License, or (at your 
  option) any later version.

  This program is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
  for more details.

  You should have received a copy of the GNU General Public License along 
  with this program; if not, write to the Free Software Foundation, Inc., 
  59 Temple Place Suite 330, Boston, MA 02111-1307 USA.\n");

  exit(0);
}

int usage(void)			/* print a short usage statement. */
{
  fprintf(stderr,"\
usage: %s [-a [xstep]][-l llimit][-u ulimit][-n points] [data-file]

  -a, --auto          supply abscissas automatically.
  -h, --help          display this help and exit.
  -l, --llimit        set lower x-limit.
  -u, --ulimit        set upper x-limit.
  -n, --points        space output points so that approx. n intervals
                      occur between the lower and upper limit.
      --version       display version information and exit.
      --warranty      display licensing terms.\n", progname);

  exit(0);
}

int insertpoint(float xv, float yv, int n)
/* insert or drop the point if duplicate */
{
  int k;

  /* search the point */
  k=0;
  while ((k < n) && (xv > x[k])) k++;

  if (k==n) /* new point */
  {
    /* We malloc the first point explicitly, on some architectures an
     * malloc(NULL,...) != realloc(...)                               */
    if (n==0)
    {
      x=xmalloc(sizeof(float)); y=xmalloc(sizeof(float));
    }
    else
    {
      x=xrealloc(x, (n+1)*sizeof(float)); y=xrealloc(y, (n+1)*sizeof(float));
    }

    x[n]=xv; y[n]=yv; n++; 
  } 
  else if (xv==x[k]) /* duplicate */
  {
    fprintf(stderr, "%s: duplicate abscissas found, "\
                    "sample (% f,% f), dropped.\n", progname, xv,yv);
  }
  else /* intermediate point. Insert it */
  {
    x=xrealloc(x, (n+1)*sizeof(float)); y=xrealloc(y, (n+1)*sizeof(float)); 

    /* Move the points above */
    memmove(&x[k+1],&x[k], (size_t)((n-k)*sizeof(float)));
    memmove(&y[k+1],&y[k], (size_t)((n-k)*sizeof(float)));

    x[k]=xv; y[k]=yv; n++;
  }
  return n;
}

int readdata(FILE *infile, float llimit, float xstep, int n)
/* Read the points and build an array with the samples 
   (through insertpoint()) */
{
  unsigned char *line=NULL;
  int quit;
  int l;

  l=0;
  do
  {
    line=getline(infile); l++;
    quit = (line==NULL) || feof(infile);
    if (!quit)
    {
      /* Skip white spaces */
      while (*line != '\0' && isspace(*line)) line++;
      if ((*line != '\0') && (*line != '#'))
      {
	int k;		       /* number of values */
	float f1, f2;	       /* floating point numbers */

	/* read point */
	k=sscanf(line, "%f %f", &f1, &f2);

	if (k==0)
	{
	  fprintf(stderr, "%s: parse error on line %d: \"%s\"\n",
		  progname, l, line);
	}
	else if (k==1)
	{
	  /* Auto xscale */
	  n=insertpoint(llimit+n*xstep,f1,n);
	}
	else
	{
	  n=insertpoint(f1,f2,n);
	}
      }
    }
  }
  while (!quit);

  return n;
}

void computelimits(int s, int e, float *llimit, float *ulimit)
/* Compute minimum and maximum of the x-values */
{
  int i;

  *llimit=MAXFLOAT; *ulimit=-MAXFLOAT;
  for (i=s; i < e; i++)
  {
    if (x[i] < *llimit) *llimit=x[i];
    if (x[i] > *ulimit) *ulimit=x[i];
  }
}

int main(int argc, char *argv[])
{
  /* 
   * These are mostly preliminary assignments, the actual values will
   * be calculated when evaluating the spline expression depending on 
   */
  int c;
  int i, k=0, p, n=0;		/* n=no of points */
  int   d=INTERVALLS;	       	/* intervall */
  float xstep=DXSTEP;		/* x-spacing */
  float llimit=0.0;		/* lower limit */
  float ulimit=0.0;		/* upper limit */
  int calclimit=1;             /* calculate lower and upper x-limits */

  progname=strrchr(argv[0], '/');
  if (progname == NULL) progname=argv[0];
  else                  progname++;

  while ((c = getopt_long(argc, argv, "a:?l:n:u:vw",
			  long_options, (int *)0)) != EOF)
  {
    switch (c)
    {
      case 0:   break;
      case 'a': xstep=strtod(optarg, NULL);
                if (xstep == 0.0) xstep=DXSTEP;
                break;
      case 'l': llimit=strtod(optarg, NULL); calclimit=0; break;
      case 'h':
      case '?': usage(); break;
      case 'n': d=atoi(optarg); break;
      case 'u': ulimit=strtod(optarg, NULL); calclimit=0; break;
      case 'v': fprintf(stderr, "%s version 1.0\n", progname); return 0; break;
      case 'w': warranty(); break;
      default : break;
    }
  }

  fprintf(stderr, "%s version 1.0. Copyright (c) 1996 David Frey\n", progname);
  fprintf(stderr, "This is free software with ABSOLUTELY NO WARRANTY.\n\n");

  if (ulimit < llimit)
  {
    float temp;

    temp=ulimit; ulimit=llimit; llimit=temp;
  }

  if (argc > optind)
  {
    for (i=optind; i < argc; i++)
    {
      FILE *infile;

      errno=0;
      infile=fopen(argv[i], "r");
      if ((infile == NULL) || (errno != 0))
      {
	fprintf(stderr, "Can't open \"%s\": %s.\n", argv[i], strerror(errno));
	exit(1);
      }

      n=readdata(infile, llimit, xstep, n);
      fclose(infile);
    }
  }
  else
    n=readdata(stdin, llimit, xstep, n);

  if (n > 0)		     /* we have data to process */
  {
    if (n==1)		     /* but only one point :( */
    {
      printf("% f % f\n", x[0], y[0]);
    }
    else if (n==2)         /* wow, two points! */
    {
      float dx, dy,m;

      dx=x[1]-x[0]; dy=y[1]-y[0]; 

      /* 3rd step: output the coefficients for the subintervalls i=2..n-4 */
      /* calculate the intermediate values */

      if (calclimit) computelimits(0,n,&llimit, &ulimit);
      xstep=(ulimit - llimit) / (d - 1);

      m=dy/dx; /* dx != 0.0 asserted by insertpoint */
      for (i=0; i < d; i++)
      {
        float xv;

	xv=llimit + i * xstep;
	printf("% f % f\n", xv, y[0]+m*xv);
      }
    }
    else                     /* now we have something to compute */
    {
      /* Leading extrapolation points,
       * the actual values will be filled in later */

      x=(float *)xrealloc(x, (n+4) * sizeof(float));
      y=(float *)xrealloc(y, (n+4) * sizeof(float));

      /* Move x[0] to x[2] */
      memmove(&x[2],&x[0], (size_t)(n*sizeof(float)));
      memmove(&y[2],&y[0], (size_t)(n*sizeof(float)));
      n += 4;

      /*
       * calculate the coefficients of the spline 
       * (the Akima interpolation itself)                      
       */

      /* allocate the arrays 
       * NB: There are some unused locations (dx[0]),
       *     but this faciliates the indexing.
       */

      dx=(float *)xmalloc(n * sizeof(float));
      dy=(float *)xmalloc(n * sizeof(float));
       m=(float *)xmalloc(n * sizeof(float));
       t=(float *)xmalloc(n * sizeof(float));

      /* a) Calculate the differences and the slopes m[i]. */

      for (i=2; i < n-3; i++)
      {
	dx[i]=x[i+1] - x[i]; dy[i]=y[i+1] - y[i]; 
	m[i] =dy[i]/dx[i]; /* dx != 0, asserted by insertpoint() */
      }

      /* b) interpolate the missing points: */

       x[1]=x[2] + x[3] - x[4];               dx[1]=x[2] - x[1];
       y[1]=dx[1] * (m[3] - 2 * m[2]) + y[2]; dy[1]=y[2] - y[1];
       m[1]=dy[1] / dx[1];

       x[0]=2 * x[2] - x[4];                  dx[0]=x[1] - x[0];
       y[0]=dx[0] * (m[2] - 2 * m[1]) + y[1]; dy[0]=y[1] - y[0];
       m[0]=dy[0] / dx[0];

      x[n-2]=x[n-3] + x[n-4] - x[n-5];
      y[n-2]=(2 * m[n-4] - m[n-5]) * (x[n-2] - x[n-3l]) + y[n-3];

      x[n-1]=2 * x[n-3] - x[n-5];
      y[n-1]=(2 * m[n-3] - m[n-4]) * (x[n-1] - x[n-2]) + y[n-2];

      for (i=n-3; i < n-1; i++)
      {
	dx[i]=x[i+1] - x[i]; dy[i]=y[i+1] - y[i];
	 m[i]=dy[i] / dx[i];
      }

      /* the first x slopes and the last y ones are extrapolated: */

      t[0]=0.0; t[1]=0.0;  /* not relevant */
      for (i=2; i < n-2; i++)
      {
	float num, den;

	num=fabs(m[i+1] - m[i]) * m[i-1] + fabs(m[i-1] - m[i-2]) * m[i];
	den=fabs(m[i+1] - m[i]) + fabs(m[i-1] - m[i-2]);

	if (den != 0.0) t[i]=num / den;
	else            t[i]=0.0;
      }

      /* c) Allocate the polynom coefficients */

      /* A=y_i */
      /* B=t_i */
         C=(float *)xmalloc(n * sizeof(float));
         D=(float *)xmalloc(n * sizeof(float));

      for (i=2; i < n-2; i++)
      {
	/* A[i]=y[i]; 
	 * B[i]=t[i]; */
  	   C[i]=(3 * m[i] - 2 * t[i] - t[i + 1]) / dx[i];
	   D[i]=(t[i] + t[i + 1] - 2 * m[i]) / (dx[i] * dx[i]);
      }

      /* 3rd step: output the coefficients for the subintervalls i=2..n-4 */
      /* calculate the intermediate values */

      if (calclimit) computelimits(2,n-2,&llimit, &ulimit);
      xstep=(ulimit - llimit) / (d - 1);

      p=2;
      for (i=0; i < d; i++)
      {
	float xv;

	xv=llimit + i*xstep;

	if (xv > x[p])
	{
	  printf("% f % f\n", x[p], y[p]); p++;
	}
	
	k=2; while (xv >= x[k]) k++;	     /* XXX */
	k--;

	if (k != p)
	{
	  printf("% f % f\n", xv,
	         y[k] + t[k] * (xv-x[k]) + 
	         C[k] * pow((xv-x[k]),2) + D[k] * pow((xv-x[k]),3));
        }
      }
    }
    free(dx); free(dy); free(m); free(t);
  }

  free(x); free(y);
  return (0);
}
