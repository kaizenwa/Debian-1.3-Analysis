/*$Id: e_aux.cc,v 11.22 96/02/18 11:43:12 al Exp $ -*- C++ -*-
 * helper functions, etc., that sort of belong to circuit elements
 */
#ifndef E_AUX_CC
#define E_AUX_CC
#include "md.h"
#include "e_aux.h"
#include "m_matrix.h"
/*--------------------------------------------------------------------------*/
template <class T>
T port_impedance(node_t n1, node_t n2, BSMATRIX<T>& mat, const T& parallel)
{
  T* zapit = new T[mat.size()+2];

  for (int ii = 0;  ii < mat.size()+2;  ++ii){
    zapit[ii] = 0.;
  }
  if (n1.m != 0)
    zapit[n1.m] =  1.;
  if (n2.m != 0)
    zapit[n2.m] = -1.;

  mat.fbsub(zapit);
  T raw_z = zapit[n1.m] - zapit[n2.m];
  delete [] zapit;
  return (parallel != 0.) 
    ? 1. / ((1./raw_z)-parallel)
    : raw_z;
}
/*--------------------------------------------------------------------------*/
#if defined(MANUAL_TEMPLATES)
  template double
  port_impedance(node_t a,node_t b,BSMATRIX<double> &c, const double &d);
  template COMPLEX
  port_impedance(node_t a,node_t b,BSMATRIX<COMPLEX> &c,const COMPLEX &d);
#endif
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
