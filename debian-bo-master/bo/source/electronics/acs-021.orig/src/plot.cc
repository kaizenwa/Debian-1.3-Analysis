/* plot.c
 *$Header: /al/acs/src/RCS/plot.cc,v 11.28 96/03/03 23:08:08 al Exp $
 * (this file is a mess.  it should be redone.)
 */
#include "constant.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
#include "pixelh.h"
#include "u_probe.h"
#include "s__.h"
#include "l_lib.h"
#include "declare.h"	/* lots */
/*--------------------------------------------------------------------------*/
	void	plottr(double);
	int	plopen(int,double,double, bool);
	void	plclose(void);
	void	plclear(void);
	void	pllocate(void);
static	void	plborder(void);
static	void	calibrate(const PROBE&);
static	int	round(double);
static	void	plhead(void);
static	int	point(double,double,double,int,int,int);
static	void	plotarg(double,double,double,double,double,double,
			 double,double,double,double,double,double);
static	void	crtopen(int,int);
static	void	ycal(double,double,double,double);
static	void	xcal(void);
/*--------------------------------------------------------------------------*/
#define INDENT 8                    			/* beware mprintf!  */
#define CONSSCALE (OPT::outwidth - INDENT - 2)	 /*console scale size in chr*/
#define RESRVLINES 4
extern char e_int[];
extern struct graph *initcrt();
extern bool crtplot;		    /* flag: crt is in graphics mode        */
static bool reset;                   /* flag: new graph.  suppress 1st line  */
static bool active;                  /* flag: plotting has opened            */
static int xmode;
static double xstart, xstop;
static bool xlinswp;
static char border[MAXWIDTH+1];     /* border string (keep, repeat at end)  */
static char emptydata[MAXWIDTH+1];  /* empty data, to copy then insert data */
static const char *device;
static struct graph *c;
/*--------------------------------------------------------------------------*/
void plottr(double xx)              /* plot a data point,  transient        */
{
 if (active){
    int ii;
    double lo[PROBECOUNT+1] = {0.};
    double hi[PROBECOUNT+1] = {0.};
    double val[PROBECOUNT+1] = {0.};

    for (ii = 0;  ii < PROBE_LISTS::plot[SIM::mode].count();  ii++){
       val[ii] = PROBE_LISTS::plot[SIM::mode][ii].value();
       if (PROBE_LISTS::plot[SIM::mode][ii].range() != 0.){
	  lo[ii] = PROBE_LISTS::plot[SIM::mode][ii].lo();
	  hi[ii] = PROBE_LISTS::plot[SIM::mode][ii].hi();
       }else{
	  lo[ii] = -5.;
	  hi[ii] = 5.;
       }
    }
    if (ii <= 1)
       val[1] = NOT_VALID;
    plotarg( xx, val[0], val[1],
	 xstart, lo[0],  lo[1],
	 xstop,  hi[0],  hi[1],
	 0.,     0.,	 0. );
 }
}
/*--------------------------------------------------------------------------*/
/* plopen: begin the plot.  any type
 */
int plopen(int mode, double start, double stop, bool linswp)
{
 int fullgrid;
 if (start == stop)
     IO::ploton = false;
 device = (testcrt()) ? "CRt" : "Ascii" ;
 if (!IO::ploton){
    plclear();
    return false;
 }
 fullgrid = !(crtplot && mode==xmode && start==xstart && stop==xstop);
 xmode   = mode;
 xstart  = start;
 xstop   = stop;
 xlinswp = linswp;
 if (*device=='C')
    crtopen(mode,fullgrid);
 else if (*device=='A')
    plhead();
 else
    error(bWARNING, e_int, "plopen" );
 active = true;
 return true;
}
/*--------------------------------------------------------------------------*/
/* plclose: finish up the plot (any type)
 */
void plclose(void)
{
 if (!active)
    return;
 if (*device=='C')
    (*c->fin)();                    /* usually a stub, except for Hercules  */
 else if (*device=='A')
    plborder();
 else
    error(bWARNING, e_int, "plclose" );
 active = false;
 IO::ploton = false;
}
/*--------------------------------------------------------------------------*/
/* plclear: clear graphics mode
 */
void plclear(void)
{
 active = false;
 if (crtplot){
    crtplot = false;
    (*c->can)();
 }
}
/*--------------------------------------------------------------------------*/
/* pllocate: locate the cursor at the bottom of the plot.
 *	     select the primary color  (CRT plot)
 */
void pllocate(void)
{
 if (crtplot)
    stext( 0, c->wh+2*c->lpc, "", c->pri);
}
/*--------------------------------------------------------------------------*/
/* plborder: draw the border -- Ascii graphics
 */
static void plborder(void)
{
 mtab(  INDENT, IO::where );
 mputs( border, IO::where );
 mputc( '\n',   IO::where );
}
/*--------------------------------------------------------------------------*/
/* calibrate: calibrate the y axis.  ascii plot.
 */
static void calibrate(const PROBE& prb)
{
 static char nums[20];		/* this label string        */
 static char highs[20];		/* the last label string    */
 int cal;			/* char position within line                */
 int stop;			/* location of last label, stop printing    */
 int filled;			/* how far (characters) have been printed   */
 int numsize;			/* number of characters in this label       */
 int start;			/* starting position of this label          */
 double markno;			/* loop counter                             */
 
 double hi, lo;
 if (prb.range() == 0){
    hi = 5;
    lo = -5;
 }else{
    hi = prb.hi();
    lo = prb.lo();
 }
 double range = hi - lo;
 
 strcpy(highs, ftos(hi, "", 5, IO::formaat));
 highs[8] = '\0';					  /* trim to 8 chrs */
 /* *strchr(&highs[2],' ') = '\0'; */	    /* make the top label, and save */
 stop = OPT::outwidth - strlen(highs) - 1;   /* space for it.                */
 
 mputs(prb.label(), IO::where);
 range = hi - lo;
 filled = 0;
 for (markno = 0.;  markno < OPT::ydivisions;  markno++){
    double number = lo + range * markno/OPT::ydivisions ;
    if (fabs(number) < fabs(range)/(10.*CONSSCALE)){
       number = 0.;
    }			    		       /* label to put on this div. */
    strcpy(nums, ftos(number, "", 5, IO::formaat));
    nums[8] = '\0';					  /* trim to 8 chrs */
    numsize = strlen(nums);			 /* center it over the mark */
    cal = round(INDENT + CONSSCALE * (markno/OPT::ydivisions));
    start = cal - (numsize+1)/2;
    if (start > filled  &&  start+numsize < stop){
       mtab( start, IO::where );		    /* if it fits, print it */
       mputs( nums, IO::where );
       filled = start + numsize ;
    }
 }
 mtab(  stop,   IO::where );		      /* print the last calibration */
 mputs( highs,  IO::where );
 mputc( '\n',   IO::where );
}
/*--------------------------------------------------------------------------*/
static int round(double x)
{
 return (int)floor(x+.5);
}
/*--------------------------------------------------------------------------*/
/* plhead: begin ascii graphics
 * print opening border, calibrations, etc.
 */ 
static void plhead(void)
{
 {for (int ii = 0;  ii < PROBE_LISTS::plot[SIM::mode].count();  ii++){
    calibrate(PROBE_LISTS::plot[SIM::mode][ii]);
 }}
 {for (int ii = 0;  ii < CONSSCALE; ii++){		/* build strings */
   border[ii] = '-';
   emptydata[ii] = ' ';
 }}
 double incr = (double)CONSSCALE / OPT::ydivisions;
 for (double place = 0.;   place < (double)CONSSCALE;   place += incr){
    border[round(place)] = '+';
    emptydata[round(place)] = '.';		/* tics in emptydata */
 }
 border[CONSSCALE] = '+';			/* fix ends of the strings */
 border[CONSSCALE+1] = '\0';
 emptydata[CONSSCALE] = emptydata[0] = '|';
 emptydata[CONSSCALE+1] = '\0';
 
 plborder();					/* print the border */
}
/*--------------------------------------------------------------------------*/
/* point: return coordinate to plot in pixel #
 */
static int point(
	double yy,	/* raw data */
	double lo,
	double hi,	/* limits: both ends of the plot */
	int scale,	/* length of scale in pixels */
	int offset,	/* pixel offset of start of plot area */
	int linswp)	/* flag: linear scale (else log scale) */
{
 int place;
 
 if (linswp)
    place = round( scale*(yy-lo)/(hi-lo));
 else
    place = round( scale*(log(yy/lo))/(log(hi/lo)));
    
 if (place < 0)
    place = 0;
 if (place > scale)
    place = scale;
 return  place + offset;
}
/*--------------------------------------------------------------------------*/
/* plotarg: plot all 2 selected probes at one time, freq, etc. point.
 */
/*ARGSUSED*/
static void plotarg(
	double xx,  /* values */
	double yy,
	double zz,
	double xlo, /* lower limits */
	double ylo,
	double zlo,
	double xhi, /* upper limits */
	double yhi,
	double zhi,
	double   ,  /* fold modulus != 0 means to suppress line */
	double yf,  /* 		when the change is too big	*/
	double zf)
{
 if (*device=='A'){
    auto char adata[MAXWIDTH+1];     /* actual data. copy emptydata, insert */
    char *xxs;				     /* string representation of xx */
    memcpy((void*)adata,(void*)emptydata,MAXWIDTH); /* copy prototype */
    xxs = ftos( xx, "           ", 5, IO::formaat );
    if (zz != NOT_VALID)
       adata[point(zz,zlo,zhi,CONSSCALE,0,1)] = '+';/* zap data into string */
    adata[point(yy,ylo,yhi,CONSSCALE,0,1)] = '*';
    mprintf( IO::where, "%-8.8s%s", xxs, adata );
    mputc( '\n', IO::where );
 }else if (*device=='C'){
    static int xold, yold, zold;
    auto   int xpt,  ypt,  zpt;
    auto   int       dy,   dz;
    xpt = point( xx, xlo, xhi, c->ww, c->lm, xlinswp );
    if (reset)
       xold = xpt;

    if (zz != NOT_VALID){
       zpt = point( zz, zhi, zlo, c->wh, c->top, true     );
       if (reset)
          zold = zpt;
       dz = zpt - zold;
       if ( !zf || ((double)abs(dz) < (double)(c->wh)*.8) )
	  line( xold, zold, xpt, zpt, c->sec );
       zold = zpt;
    }
    ypt = point( yy, yhi, ylo, c->wh, c->top, true     );
    dy = ypt - yold;
    if (reset)
       yold = ypt;
    if ( !yf || ((double)abs(dy) < (double)(c->wh)*.8) )
       line( xold, yold, xpt, ypt, c->pri );
    yold = ypt;

    xold = xpt;
    reset = false;
 }else{
    error(bWARNING, e_int, "plotarg");
 }
}
/*--------------------------------------------------------------------------*/
/* crtopen: begin CRT graphics mode
 * clear screen, draw box, calibrate, etc.
 */
static void crtopen(int , int fullgrid)
{
 double lo[PROBECOUNT+1], hi[PROBECOUNT+1];
 reset = true;
 if (!fullgrid)
    return;
 crtplot = true;
 c = initcrt();
 if (!c)
    error(bERROR, e_int, "crtopen: device not installed");
 initgraph(c);
 c->lm =          INDENT     * c->ppc    ;
 c->rm = c->sw - (INDENT     * c->ppc) -2;
 c->top =         0       /* * c->lpc */ ;
 c->bm = c->sh - (RESRVLINES * c->lpc) -2;
 c->ww = c->rm - c->lm;
 c->wh = c->bm - c->top;
 box( c->lm, c->top, c->rm, c->bm, c->grid );
 for (int ii = 0;  ii < PROBE_LISTS::plot[SIM::mode].count();  ii++){
    if (PROBE_LISTS::plot[SIM::mode][ii].range() != 0){
       lo[ii] = PROBE_LISTS::plot[SIM::mode][ii].lo();
       hi[ii] = PROBE_LISTS::plot[SIM::mode][ii].hi();
    }else{
       lo[ii] = -5.;
       hi[ii] = 5.;
    }
 }
 ycal(lo[0],hi[0],lo[1],hi[1]);
 xcal();
 stext( 0, c->wh+2*c->lpc, "", c->pri);
}
/*--------------------------------------------------------------------------*/
/* ycal: calibrate the Y axis on CRT display
 */
static void ycal(double ylo, double yhi, double zlo, double zhi)
{
 double ynum, znum, yrange, zrange;
 double ratio;
 int cal;
 static char ystr[] = "           ";
 static char zstr[] = "           ";
 yrange = yhi-ylo;
 zrange = zhi-zlo;
 for (double markno = 0.;  markno < OPT::ydivisions;  markno++){
    ratio = markno / OPT::ydivisions;
    ynum = ylo + yrange * ratio ;
    znum = zlo + zrange * ratio ;
    if (fabs(ynum) < fabs(yrange)/(10.*c->wh))
       ynum = 0.;
    if (fabs(znum) < fabs(zrange)/(10.*c->wh))
       znum = 0.;
    strcpy(ystr, ftos(ynum,ystr,5,0));
    strcpy(zstr, ftos(znum,zstr,5,0));
    ystr[8] = zstr[8] = '\0';				/* trim to 8 chrs */
    cal = round(c->bm - c->wh * ratio);
    if (cal != c->bm   &&   cal != c->top)
       line( c->lm, cal, c->rm, cal, c->grid );
    if (cal < c->lpc)
       cal = c->lpc-1;
    stext( 0,              cal, ystr, c->pri);
    stext( c->rm+1*c->ppc, cal, zstr, c->sec);
 }
 strcpy(ystr, ftos(yhi,ystr,5,0));
 strcpy(zstr, ftos(zhi,zstr,5,0));
 ystr[8] = zstr[8] = '\0';				/* trim to 8 chrs */
 cal = (c->top < c->lpc) ? c->lpc-1 : c->top ;
 stext(0,              cal, ystr, c->pri);
 stext(c->rm+1*c->ppc, cal, zstr, c->sec);
}
/*--------------------------------------------------------------------------*/
/* xcal: calibrate the X axis on CRT display
 */
static void xcal(void)
{
 double xcalib, ratio;
 int cal;
 static char str[] = "           ";

 for (double markno = 0.;  markno <= OPT::xdivisions;  markno++){
    ratio = markno / OPT::xdivisions;
    xcalib = (xlinswp)
	? xstart +      (xstop-xstart) * ratio
	: xstart * pow( (xstop/xstart) , ratio );
    cal = round(c->lm + c->ww * ratio);
    if (cal != c->lm   &&   cal != c->rm)
       line(cal, c->top, cal, c->bm, c->grid);
    strcpy(str, ftos(xcalib, str, 5, 0));
    str[8] = '\0';
    stext(cal-2*c->ppc, c->wh+c->lpc, str, c->pri);
 }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
