/*$Id: e_aux.h,v 11.21 96/02/02 23:19:19 al Exp $ -*- C++ -*-
 * helper functions, etc., that sort of belong to circuit elements
 */
#include "e_card.h"
#ifndef E_AUX_H
#define E_AUX_H
/*--------------------------------------------------------------------------*/
#if defined(COMPILE_TEMPLATES) || defined(ComTemP)
  #include "e_aux.cc"
#else
  template <class T>
  T port_impedance(node_t n1, node_t n2, BSMATRIX<T>& mat, const T& parallel);
#endif
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
