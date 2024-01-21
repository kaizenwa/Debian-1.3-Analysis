/*$Id: e_elem3.cc,v 11.28 96/03/03 23:07:38 al Exp $ -*- C++ -*-
 * top level device functions that call the specific one and
 * standard device functions to be used where nothing else is needed
 */
#include "ap.h"
#include "m_matrix.h"
#include "e_elemnt.h"
#include "e_aux.h"
#include "u_xprobe.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
//	double	  ELEMENT::tr_amps()const;
//	double	  ELEMENT::tr_outvolts()const;
//	double	  ELEMENT::tr_involts()const;
//	COMPLEX	  ELEMENT::ac_outvolts()const;
//	COMPLEX	  ELEMENT::ac_involts()const;
//	double	  ELEMENT::probe_tr_num(const char*)const;
//	xprobe_t  ELEMENT::probe_ac_ext(const char*)const;
/*--------------------------------------------------------------------------*/
double ELEMENT::tr_amps()const
{
  return loss * tr_outvolts() + m0.f1 * tr_involts() + m0.c0;
}
/*--------------------------------------------------------------------------*/
double ELEMENT::tr_outvolts()const
{
  return n[OUT1].v0() - n[OUT2].v0();
}
/*--------------------------------------------------------------------------*/
double ELEMENT::tr_involts()const
{
  if (is2port()){
    return n[IN1].v0() - n[IN2].v0();
  }else if (is1port()){
    return tr_outvolts();
  }else if (issource()){
    return 0.;
  }else{
    assert(0);
    return 0.;
  }
}
/*--------------------------------------------------------------------------*/
COMPLEX ELEMENT::ac_outvolts()const
{
  return n[OUT1].vac() - n[OUT2].vac();
}
/*--------------------------------------------------------------------------*/
COMPLEX ELEMENT::ac_involts()const
{
  if (is2port()){
    return n[IN1].vac() - n[IN2].vac();
  }else if (is1port()){
    return ac_outvolts();
  }else if (issource()){
    return 1.;
  }else{
    assert(0);
    return 1.;
  }
}
/*--------------------------------------------------------------------------*/
double ELEMENT::probe_tr_num(const char *what)const
{
  CS cmd(what);
  if (cmd.pmatch("Vo")){
    return tr_outvolts();
  }else if (cmd.pmatch("VI")){
    return tr_involts();
  }else if (cmd.pmatch("I")){
    return tr_amps();
  }else if (cmd.pmatch("P")){
    return tr_amps() * tr_outvolts();
  }else if (cmd.pmatch("PD")){
    double p = tr_amps() * tr_outvolts();
    return (p > 0.) ? p : 0.;
  }else if (cmd.pmatch("PS")){
    double p = tr_amps() * tr_outvolts();
    return (p < 0.) ? -p : 0.;
  }else if (cmd.pmatch("INput")){
    return y0.x;
  }else if (cmd.pmatch("F")){
    assert(y0.f0 != LINEAR);
    return y0.f0;
  }else if (cmd.pmatch("NV")){
    return val;
  }else if (cmd.pmatch("EV")){
    return y0.f1;
  }else if (cmd.pmatch("Y")){
    return m0.f1;
  }else if (cmd.pmatch("EIV")){
    return m0.x;
  }else if (cmd.pmatch("IOFfset")){
    return m0.c0;
  }else if (cmd.pmatch("IPassive")){
    return m0.f1 * tr_involts();
  }else if (cmd.pmatch("ILoss")){
    return loss * tr_outvolts();
//  }else if (cmd.pmatch("DIdt")){
//    double i0  = (m0.f1  * m0.x  + m0.c0);
//    double it1 = (mt1.f1 * mt1.x + mt1.c0);
//    return  (i0 - it1) / (time0 - time1);
//  }else if (cmd.pmatch("DTNew")){
//    return timef - time0;
//  }else if (cmd.pmatch("DTOld")){
//    return time0 - time1;
//  }else if (cmd.pmatch("TIMEF")){
//    return timef;
//  }else if (cmd.pmatch("TIME")){
//    return time0;
//  }else if (cmd.pmatch("TIMEO")){
//    return time1;
  }else if (cmd.pmatch("R")){
    return (m0.f1!=0.) ? 1/m0.f1 : DBL_MAX;
  }else if (cmd.pmatch("Z")){
    double parallel = m0.f1;			/* hide Borland 3.1 bug */
    return port_impedance(n[OUT1], n[OUT2], lu, parallel);
  }else if (cmd.pmatch("ZRAW")){
    return port_impedance(n[OUT1], n[OUT2], lu, 0.);
  }else{
    return NOT_VALID;
  }
}
/*--------------------------------------------------------------------------*/
xprobe_t ELEMENT::probe_ac_ext(const char *what)const
{
  CS cmd(what);
  xprobe_t result(0., mtMAG, 20., true);
  COMPLEX admittance;
  COMPLEX source;

  if (issource()){
    source = acg;
    admittance = 0.;
  }else{
    source = 0.;
    admittance = acg;
  }

  if (cmd.pmatch("V")){				/* volts */
    result.value = ac_outvolts();
  }else if (cmd.pmatch("I")){			/* amps */
    result.value = ac_involts() * admittance + source + ac_outvolts() * loss;
  }else if (cmd.pmatch("P")){			/* complex "power" */
    COMPLEX i = ac_involts() * admittance + source + ac_outvolts() * loss;
    result.value = ac_outvolts() * conj(i);
    result.dbscale = 10.;
    result.modifier = mtREAL;
  }else if (cmd.pmatch("NV")){			/* nominal value */
    result.value = val;
  }else if (cmd.pmatch("EV")){			/* effective value */
    result.value = ev;
  }else if (cmd.pmatch("Y")){			/* admittance */
    result.value = admittance;
    result.modifier = mtREAL;
  }else if (cmd.pmatch("R")){			/* complex "resistance" */
    if (admittance == 0.){
      result.value = DBL_MAX;
    }else{
      result.value = 1. / admittance;
    }
  }else if (cmd.pmatch("Z")){			/* port impedance */
    result.value = port_impedance(n[OUT1], n[OUT2], acx, admittance);
  }else if (cmd.pmatch("ZRAW")){		/* port impedance, raw */
    result.value = port_impedance(n[OUT1], n[OUT2], acx, COMPLEX(0.));

  }else{ 					/* bad parameter */
    result.value = COMPLEX(NOT_VALID, NOT_VALID);
    result.ok = false;
  }
  return result;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
