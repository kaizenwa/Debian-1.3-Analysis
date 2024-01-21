/*$Id: e_elem1.cc,v 11.34 96/03/21 22:37:54 al Exp $ -*- C++ -*-
 * Base class for elements of a circuit
 * This file contains functions to load the matrix from a circuit element
 */
#include "m_matrix.h"
#include "error.h"
#include "e_elemnt.h"
#include "u_opt.h"
#include "u_status.h"
#include "s__.h"
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
//	double	ELEMENT::dampdiff(double& v0, const double& v1, bool damp);
//	void	ELEMENT::trload_loss();
//	void	ELEMENT::trunload_loss();
//	void	ELEMENT::il_trload_source();
//	void	ELEMENT::trload_source();
//	void	ELEMENT::trunload_source();
//	void	ELEMENT::acload_source();
//	void	ELEMENT::acload_loss();
//	void	ELEMENT::trload_passive();
//	void	ELEMENT::trunload_passive();
//	void	ELEMENT::acload_passive();
//	void	ELEMENT::trload_active();
//	void	ELEMENT::trunload_active();
//	void	ELEMENT::acload_active();
/*--------------------------------------------------------------------------*/
inline double ELEMENT::dampdiff(double& v0, const double& v1, bool damp)
{
  double diff = v0 - v1;
  if (STATUS::iter[iSTEP] > 1 && damp){
    diff *= SIM::damp;
    v0 = v1 + diff;
  }
  #if defined(DO_TRACE)
    if ((diff*diff) > (v0 * v0 + OPT::abstol)){
      trace4(printlabel(), v1, v0, diff, SIM::damp);
    }
  #endif
  return (SIM::inc_mode) ? diff : v0;
}
/*--------------------------------------------------------------------------*/
void ELEMENT::trload_loss()
{
  if (!SIM::inc_mode  &&  loss != 0.){
    if (n[OUT2].m != 0){
      aa.d(n[OUT2].m,n[OUT2].m) += loss;
      if (n[OUT1].m != 0){
	aa.d(n[OUT1].m,n[OUT1].m) += loss;
	aa.m(n[OUT1].m,n[OUT2].m) -= loss;
	aa.m(n[OUT2].m,n[OUT1].m) -= loss;
      }
    }else if (n[OUT1].m != 0){
      aa.d(n[OUT1].m,n[OUT1].m) += loss;
    }
  }
}
/*--------------------------------------------------------------------------*/
void ELEMENT::trunload_loss()
{
  assert(0);
}
/*--------------------------------------------------------------------------*/
inline void ELEMENT::il_trload_source()
{
  assert((loaditer != STATUS::iter[iTOTAL])
  	&& (loaditer = STATUS::iter[iTOTAL]));

  double dc0 = dampdiff(m0.c0, m1.c0, true);
  if (dc0 != 0.){
    if (n[OUT2].m != 0)
      n[OUT2].i() += dc0;
    if (n[OUT1].m != 0)
      n[OUT1].i() -= dc0;
  }
  m1 = m0;
}
/*--------------------------------------------------------------------------*/
void ELEMENT::trload_source()
{
  il_trload_source();
}
/*--------------------------------------------------------------------------*/
void ELEMENT::trunload_source()
{
  m0.c0 = m0.f1 = 0.;
  if (SIM::inc_mode)
    SIM::inc_mode = BAD;
  trload_source();
}
/*--------------------------------------------------------------------------*/
void ELEMENT::acload_source()
{
  if (n[OUT2].m != 0){
    n[OUT2].iac() += acg;
  }
  if (n[OUT1].m != 0){
    n[OUT1].iac() -= acg;
  }
}
/*--------------------------------------------------------------------------*/
void ELEMENT::acload_loss()
{
  if (n[OUT2].m != 0){
    acx.d(n[OUT2].m,n[OUT2].m) += loss;
    if (n[OUT1].m != 0){
      acx.d(n[OUT1].m,n[OUT1].m) += loss;
      acx.m(n[OUT1].m,n[OUT2].m) -= loss;
      acx.m(n[OUT2].m,n[OUT1].m) -= loss;
    }
  }else if (n[OUT1].m != 0){
    acx.d(n[OUT1].m,n[OUT1].m) += loss;
  }
}
/*--------------------------------------------------------------------------*/
void ELEMENT::trload_passive()
{
  double df1 = dampdiff(m0.f1, m1.f1, true);
  if (df1 != 0.){
    if (n[OUT2].m != 0){
      setneedslu(OUT2);
      aa.d(n[OUT2].m,n[OUT2].m) += df1;
      if (n[OUT1].m != 0){
	setneedslu(OUT1);
	aa.d(n[OUT1].m,n[OUT1].m) += df1;
	aa.m(n[OUT1].m,n[OUT2].m) -= df1;
	aa.m(n[OUT2].m,n[OUT1].m) -= df1;
      }
    }else if (n[OUT1].m != 0){
      setneedslu(OUT1);
      aa.d(n[OUT1].m,n[OUT1].m) += df1;
    }
  }
  il_trload_source();
}
/*--------------------------------------------------------------------------*/
void ELEMENT::trunload_passive()
{
  m0.c0 = m0.f1 = 0.;
  if (SIM::inc_mode)
    SIM::inc_mode = BAD;
  trload_passive();
}
/*--------------------------------------------------------------------------*/
void ELEMENT::acload_passive()
{
  if (n[OUT2].m != 0){
    acx.d(n[OUT2].m,n[OUT2].m) += acg;
    if (n[OUT1].m != 0){
      acx.d(n[OUT1].m,n[OUT1].m) += acg;
      acx.m(n[OUT1].m,n[OUT2].m) -= acg;
      acx.m(n[OUT2].m,n[OUT1].m) -= acg;
    }
  }else if (n[OUT1].m != 0){
    acx.d(n[OUT1].m,n[OUT1].m) += acg;
  }
}
/*--------------------------------------------------------------------------*/
void ELEMENT::trload_active()
{
  double df1 = dampdiff(m0.f1, m1.f1, true);
  if (df1 != 0.){
    setneedslu(OUT1);
    setneedslu(OUT2);
    setneedslu(IN1);
    setneedslu(IN2);
    aa.s(n[OUT1].m,n[IN1].m) += df1;
    aa.s(n[OUT2].m,n[IN2].m) += df1;
    aa.s(n[OUT1].m,n[IN2].m) -= df1;
    aa.s(n[OUT2].m,n[IN1].m) -= df1;
  }
  il_trload_source();
}
/*--------------------------------------------------------------------------*/
void ELEMENT::trunload_active()
{
  m0.c0 = m0.f1 = 0.;
  if (SIM::inc_mode)
    SIM::inc_mode = BAD;
  trload_active();
}
/*--------------------------------------------------------------------------*/
void ELEMENT::acload_active()
{
  acx.s(n[OUT1].m,n[IN1].m) += acg;
  acx.s(n[OUT2].m,n[IN2].m) += acg;
  acx.s(n[OUT1].m,n[IN2].m) -= acg;
  acx.s(n[OUT2].m,n[IN1].m) -= acg;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
