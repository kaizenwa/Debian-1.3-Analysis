/*$Id: e_base.h,v 11.35 96/03/22 18:10:08 al Exp $ -*- C++ -*-
 * real base for anything to do with a circuit
 */
#include "md.h"
#include "m_matrix.h"
#include "u_prblst.h"
#ifndef E_BASE_H
#define E_BASE_H
/*--------------------------------------------------------------------------*/
class xprobe_t;
/*--------------------------------------------------------------------------*/
class CKT_BASE {
private:
  static int devcount;
  int	   probes;		/* number of probes set */
protected:
	  CKT_BASE();
  virtual ~CKT_BASE();
public:
  static  BSMATRIX<double>  aa;	/* raw matrix for DC & tran */
  static  BSMATRIX<double>  lu;	/* decomposed marrix for DC & tran */
  static  BSMATRIX<COMPLEX> acx;/* raw & decomposed matrix for AC */
public:
  virtual char*	   printlabel(int =0)const{assert(0);return NULL;}
  virtual char*	   probe_txt(const char*)const;
  virtual double   probe_num(const char*)const;
  virtual double   probe_tr_num(const char*)const;
  virtual double   probe_ac_num(const char*)const;
  virtual xprobe_t probe_ac_ext(const char*)const;
	  int	   incprobes(){return ++probes;}
	  int	   decprobes(){assert(probes>0);return --probes;}
	  bool	   hasprobes()const{return probes > 0;}
  static  int	   devicecount(){return devcount;}
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
