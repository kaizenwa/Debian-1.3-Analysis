/*$Id: e_base.cc,v 11.28 96/03/03 23:07:30 al Exp $ -*- C++ -*-
 * Base class for "cards" in the circuit description file
 */
#include "l_lib.h"
#include "constant.h"
#include "error.h"
#include "io.h"
#include "io_trace.h"
#include "u_xprobe.h"
#include "e_base.h"
#include "s__.h"
#include "l_compar.h"
/*--------------------------------------------------------------------------*/
//		 CKT_BASE()::CKT_BASE();
//		 CKT_BASE()::~CKT_BASE();
//	char*	 CKT_BASE::probe_txt(const char*)const;
//	double	 CKT_BASE::probe_num(const char*)const;
	double	 CKT_BASE::probe_tr_num(const char*)const
				{assert(0);return NOT_VALID;}
//	double	 CKT_BASE::probe_ac_num(const char*)const;
	xprobe_t CKT_BASE::probe_ac_ext(const char*)const
				{assert(0);return xprobe_t();}
/*--------------------------------------------------------------------------*/
BSMATRIX<double>  CKT_BASE::aa;
BSMATRIX<double>  CKT_BASE::lu;
BSMATRIX<COMPLEX> CKT_BASE::acx;
int		  CKT_BASE::devcount = 0;
/*--------------------------------------------------------------------------*/
CKT_BASE::CKT_BASE()
{
  ++devcount;
  probes = 0;
}
/*--------------------------------------------------------------------------*/
CKT_BASE::~CKT_BASE()
{
  trace1("~CKT_BASE", probes);
  PROBE_LISTS::purge(this);
  trace1("", probes);
  assert(probes==0);
  --devcount;
}
/*--------------------------------------------------------------------------*/
char* CKT_BASE::probe_txt(const char *what)const
{
  return ftos(probe_num(what),"           ",5,IO::formaat);
}
/*--------------------------------------------------------------------------*/
double CKT_BASE::probe_num(const char *what)const
{
  if (SIM::mode == sAC){
    return probe_ac_num(what);
  }else{
    return probe_tr_num(what);
  }
}
/*--------------------------------------------------------------------------*/
double CKT_BASE::probe_ac_num(const char *what)const
{
  int length = strlen(what);
  mod_t modifier = mtNONE;
  bool want_db = false;
  char parameter[LABELEN+1];
  strcpy(parameter, what);

  if (length > 2  &&  pmatch(&parameter[length-2], "DB")){
    want_db = true;
    length -= 2;
  }
  if (length > 1){
    switch (to_lower(parameter[length-1])){
      case 'm': modifier = mtMAG;   length--;	break;
      case 'p': modifier = mtPHASE; length--;	break;
      case 'r': modifier = mtREAL;  length--;	break;
      case 'i': modifier = mtIMAG;  length--;	break;
      default:  modifier = mtNONE;		break;
    }
  }
  parameter[length] = '\0';
  
  xprobe_t xp = probe_ac_ext(parameter);
  if (!xp.ok){
    xp = probe_ac_ext(what);
  }
  if (xp.ok){
    double rvalue = 0.;
    if (modifier == mtNONE)
      modifier = xp.modifier;
    switch (modifier){
      case mtMAG:   rvalue = abs(xp.value);    	 break;
      case mtPHASE: rvalue = arg(xp.value)*RTOD; break;
      case mtREAL:  rvalue = real(xp.value);	 break;
      case mtIMAG:  rvalue = imag(xp.value);	 break;
      case mtNONE:  assert(0);			 break;
    }
    if (want_db){
      rvalue = xp.dbscale * log10(max(rvalue,VOLTMIN));
    }
    return rvalue;
  }else{
    return NOT_VALID;
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
