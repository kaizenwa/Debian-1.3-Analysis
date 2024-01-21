/*$Id: d_trln.cc,v 11.34 96/03/21 22:37:35 al Exp $ -*- C++ -*-
 * Transmission line. (ideal lossless.  for now, AC only)
 */
#include "ap.h"
#include "m_matrix.h"
#include "d_trln.h"
#include "error.h"
#include "io.h"
#include "s__.h"
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
//		TRANSLINE_COMMON::TRANSLINE_COMMON();
//		DEV_TRANSLINE::DEV_TRANSLINE();
//	void	DEV_TRANSLINE::parse(CS *cmd);
// 	void	DEV_TRANSLINE::print(int,int)const;
// 	bool	DEV_TRANSLINE::dotr();
// 	void	DEV_TRANSLINE::doac();
//	void	DEV_TRANSLINE::setinitcond(CS*);
/*--------------------------------------------------------------------------*/
static const double LINLENTOL = .000001;
static const double DEFAULT_NL = 0.25;
static TRANSLINE_COMMON Default_TRANSLINE;
/*--------------------------------------------------------------------------*/
TRANSLINE_COMMON::TRANSLINE_COMMON()
{
  z0 = 0.;
  td = 0.;
  f  = 0.;
  nl = DEFAULT_NL;
  reson = 0.;
  for (int i = 0;  i < NUM_INIT_COND;  i++)
    ic[i] = 0.;
  icset = false;
}
/*--------------------------------------------------------------------------*/
DEV_TRANSLINE::DEV_TRANSLINE()
{
  devclass = TWOPORT;
  attach_common(&Default_TRANSLINE);
}
/*--------------------------------------------------------------------------*/
void DEV_TRANSLINE::parse(CS& cmd)
{
  assert(common);
  TRANSLINE_COMMON* c = new TRANSLINE_COMMON(*(const TRANSLINE_COMMON*)common);
  assert(c);

  parselabel(cmd);
  parsenodes(cmd,NUMNODES);
  cmd.stuck();
  do{
    cmd.get("Z",    &c->z0, &c->z0);
    cmd.get("Freq", &c->f,  mPOSITIVE);
    cmd.get("Nl",   &c->nl, mPOSITIVE);
    if (cmd.pmatch("Ic")) setinitcond(cmd);
  }while (cmd.more() && !cmd.stuck());
  cmd.check(bWARNING);

  if (c->nl == 0.)
    c->nl = DEFAULT_NL;
  c->reson = c->f * (.25 / c->nl);
  attach_common(c);
}
/*--------------------------------------------------------------------------*/
/* setinitcond: set initial conditions
 */
void DEV_TRANSLINE::setinitcond(CS& cmd)
{
  TRANSLINE_COMMON* c = new TRANSLINE_COMMON(*(const TRANSLINE_COMMON*)common);

  c->icset = true;
  for (int i = 0;  i < NUM_INIT_COND;  i++)
    c->ic[i] = cmd.ctof();
}
/*--------------------------------------------------------------------------*/
void DEV_TRANSLINE::print(int where, int)const
{
  const TRANSLINE_COMMON* c = (const TRANSLINE_COMMON*)common;
  printlabel(where);
  printnodes(where,NUMNODES);
  mprintf(where, "  Z0=%s  F=%s  NL=%s",
	  ftos(c->z0, "", 7, 0),
	  ftos(c->f,  "", 7, 0),
	  ftos(c->nl, "", 7, 0));
  if (c->icset){
    mprintf(where, "  IC=");
    for (int i = 0;  i < NUM_INIT_COND;  i++)
      mprintf(where, "%s ",ftos(c->ic[i],"", 7, 0));
  }
  mputc('\n', where);
}
/*--------------------------------------------------------------------------*/
/* transmission line for transient analysis
 * stub: doesn't work
 * always returns success
 */
bool DEV_TRANSLINE::dotr()
{
  assert(!isbypassed());
  error(bWARNING, "%s: no transmission line in dc or transient\n",
	printlabel());
  return converged = true;
}
/*--------------------------------------------------------------------------*/
void DEV_TRANSLINE::doac()
{
  const TRANSLINE_COMMON* c = (const TRANSLINE_COMMON*)common;
  double lenth = SIM::freq / c->reson;	/* length in quarter waves */
  double dif = lenth - floor(lenth+.5);	/* avoid divide by zero if close to */
  if (fabs(dif) < LINLENTOL){		/* resonance by tweeking a little */
    error(bDEBUG,
	  "%s: transmission line too close to resonance\n", printlabel());
    lenth = (dif<0.) ? floor(lenth+.5)-LINLENTOL : floor(lenth+.5)+LINLENTOL;
  }
  lenth *= (kPId2);	/* now in radians */
  
  COMPLEX y12 = COMPLEX(0., -1. / (c->z0 * sin(lenth)));
  COMPLEX y11 = COMPLEX(0., tan(lenth/2) / c->z0) + y12;
  
  acx.s(n[OUT1].m,n[OUT1].m) += y11; 	/* BUG: bypasses load functions */
  acx.s(n[OUT2].m,n[OUT2].m) += y11;	/* result is flags may not be   */
  acx.s(n[OUT1].m,n[OUT2].m) -= y11;	/* updated.  No problem yet in  */
  acx.s(n[OUT2].m,n[OUT1].m) -= y11; 	/* AC but this will not work in */
					/* transient.			*/
  acx.s(n[IN1].m,n[IN1].m) += y11;
  acx.s(n[IN2].m,n[IN2].m) += y11;
  acx.s(n[IN1].m,n[IN2].m) -= y11;
  acx.s(n[IN2].m,n[IN1].m) -= y11;
  
  acx.s(n[OUT1].m,n[IN1].m) -= y12;
  acx.s(n[OUT2].m,n[IN2].m) -= y12;
  acx.s(n[OUT1].m,n[IN2].m) += y12;
  acx.s(n[OUT2].m,n[IN1].m) += y12;
  
  acx.s(n[IN1].m,n[OUT1].m) -= y12;
  acx.s(n[IN2].m,n[OUT2].m) -= y12;
  acx.s(n[IN1].m,n[OUT2].m) += y12;
  acx.s(n[IN2].m,n[OUT1].m) += y12;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
