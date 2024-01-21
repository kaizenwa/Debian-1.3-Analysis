/*$Id: e_node.cc,v 11.39 96/03/30 15:56:32 al Exp $ -*- C++ -*-
 * node probes
 */
#include "ap.h"
#include "error.h"
#include "m_matrix.h"
#include "e_node.h"
#include "u_opt.h"
#include "u_probe.h"
#include "u_xprobe.h"
#include "s__.h"
#include "e_aux.h"
/*--------------------------------------------------------------------------*/
//		NODE::NODE();
//	char*	NODE::printlabel(int where=0)const;
//	double	NODE::probe_tr_num(const char*)const;
//	xprobe_t NODE::probe_ac_ext(const char*)const;
//	double	NODE::logicval()const;
/*--------------------------------------------------------------------------*/
NODE::NODE()
{
  diter = aiter = number = 0;
  family = NULL;
  finaltime = lastchange = dt = 0.;
  lv0 = lv1 = ls0 = ls1 = false;
  nodemode = mANALOG;
  quality = qBAD;
  needsanalog = false;
  failuremode = "";
}
/*--------------------------------------------------------------------------*/
char* NODE::printlabel(int /*where*/)const
{
  static char buffer[BUFLEN];
  sprintf(buffer, "%u", number);
  return buffer;
}
/*--------------------------------------------------------------------------*/
double NODE::probe_tr_num(const char* what)const
{
  CS cmd(what);
  if (cmd.pmatch("V")){
    return v0();
  }else if (cmd.pmatch("Z")){
    node_t nn;
    node_t ground;
    ground.e = ground.m = ground.t = 0;
    nn.e = number;
    nn.t = nn.e;
    nn.m = to_internal(nn.t);
    return port_impedance(nn, ground, lu, 0.);
  }else if (cmd.pmatch("Logic")){
    return logicval();
  }else if (cmd.pmatch("LAstchange")){
    return lastchange;
  }else if (cmd.pmatch("FInaltime")){
    return finaltime;
  }else if (cmd.pmatch("DIter")){
    return (double)diter;
  }else if (cmd.pmatch("AIter")){
    return (double)aiter;
  }else if (cmd.pmatch("COUNT")){
    return (double)needsanalog;
  }else{ /* bad parameter */
    return NOT_VALID;
  }
}
/*--------------------------------------------------------------------------*/
xprobe_t NODE::probe_ac_ext(const char* what)const
{
  CS cmd(what);
  xprobe_t result(0., mtMAG, 20., true);
  
  if (cmd.pmatch("V")){
    result.value = vac();
  }else if (cmd.pmatch("Z")){
    node_t nn;
    node_t ground;
    ground.e = ground.m = ground.t = 0;
    nn.e = number;
    nn.t = nn.e;
    nn.m = to_internal(nn.t);
    result.value = port_impedance(nn, ground, acx, COMPLEX(0.));
  }else{ /* bad parameter */
    result.value = COMPLEX(NOT_VALID, NOT_VALID);
    result.ok = false;
  }
  return result;
}
/*--------------------------------------------------------------------------*/
double NODE::logicval()const  /* cryptic interpretation of logic value */
{
  return (	   (2*lv0 + lv1)
	  + (.1  * (OPT::transits - quality))
	  + (.01 * (2 - nodemode)));
}
/*--------------------------------------------------------------------------*/
/* volts_limited: transient voltage, best approximation, with limiting
 */
double volts_limited(const node_t & n1, const node_t & n2)
{
  double v1 = torange(OPT::vmin, n1.v0(), OPT::vmax);
  double v2 = torange(OPT::vmin, n2.v0(), OPT::vmax);
  double vdiff = torange(-OPT::limit, v1 - v2, OPT::limit);
  if (OPT::dampstrategy & dsRANGE
  	&& (fabs(vdiff - (n1.v0() - n2.v0())) > OPT::abstol)){
    SIM::fulldamp = true;
    printf("range limit damp\n");
  }
  if (OPT::picky <= bTRACE
  	&& (fabs(vdiff - (n1.v0() - n2.v0())) > OPT::abstol)){
    error(bTRACE,"limiting done.  was (%g %g %g) now (%g %g %g)\n",
     	 n1.v0(), n2.v0(), n1.v0() - n2.v0(), v1, v2, vdiff);
  }
  return vdiff;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
