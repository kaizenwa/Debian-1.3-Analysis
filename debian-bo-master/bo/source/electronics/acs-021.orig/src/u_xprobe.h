/*$Id: u_xprobe.h,v 11.28 96/03/03 23:09:17 al Exp $ -*- C++ -*-
 * extended probe data
 */
#include "md.h"
#ifndef U_XPROBE_H
#define U_XPROBE_H
/*--------------------------------------------------------------------------*/
typedef enum {mtNONE, mtMAG, mtPHASE, mtREAL, mtIMAG} mod_t;
/*--------------------------------------------------------------------------*/
struct xprobe_t{
  COMPLEX value;
  mod_t modifier;
  double dbscale;
  int ok;
  
  xprobe_t() {ok=false;}
  xprobe_t(const xprobe_t& x){
    value = x.value;
    modifier = x.modifier;
    dbscale = x.dbscale;
    ok = x.ok;
  }
  xprobe_t(COMPLEX v,mod_t m,double d,int o){
    value = v;
    modifier = m;
    dbscale = d;
    ok = o;
  }
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
