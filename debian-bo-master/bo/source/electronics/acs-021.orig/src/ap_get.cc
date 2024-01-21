/*$Id: ap_get.cc,v 11.21 96/02/02 23:17:50 al Exp $ -*- C++ -*-
 * get value for matching keyword
 */
#include <assert.h>
#include <stdlib.h>
#include "ap.h"
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
//	CS &	CS::set(const char*, bool*, bool);
//	CS &	CS::set(const char*, int*, int);
//	CS &	CS::get(const char*, int*, AP_MOD=mNONE, int=0);
//	CS &	CS::get(const char*, double*, AP_MOD=mNONE, double=0);
//	CS &	CS::get(const char*, double*, double*);
/*--------------------------------------------------------------------------*/
#if !defined(BAD_BOOL)
CS & CS::set(const char* key, bool* val, bool newval)
{
  if (pmatch(key)){
    *val = newval;
    trace0("set bool");
    trace1(key, *val);
  }else{
    trace0(key);
  }
  return *this;
}
#endif
/*--------------------------------------------------------------------------*/
CS & CS::set(const char* key, int* val, int newval)
{
  if (pmatch(key)){
    *val = newval;
    trace0("set int");
    trace1(key, *val);
  }else{
    trace0(key);
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
CS & CS::get(const char* key, int* val, AP_MOD mk1, int mv1)
{
  if (pmatch(key)){
    switch(mk1){
      case mNONE:     *val = int(ctof());	break;
      case mSCALE:    *val = int(ctof()) * mv1;	break;
      case mOFFSET:   *val = int(ctof()) + mv1; break;
      case mINVERT:   *val = 1 / int(ctof());   break;
      case mPOSITIVE: *val = abs(int(ctof()));  break;
      case mOCTAL:    *val = ctoo();		break;
      case mHEX:      *val = ctox();		break;
    }
    trace0("got int");
    trace1(key, *val);
  }else{
    trace0(key);
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
CS & CS::get(const char* key, double* val, AP_MOD mk1, double mv1)
{
  if (pmatch(key)){
    *val = ctof();
    switch(mk1){
      case mNONE:                        break;
      case mSCALE:    *val *= mv1;       break;
      case mOFFSET:   *val += mv1;       break;
      case mINVERT:   *val = 1 / *val;   break;
      case mPOSITIVE: *val = fabs(*val); break;
      case mOCTAL:    
      case mHEX:      assert(0);         break;
    }
    trace0("got double");
    trace1(key, *val);
  }else{
    trace0(key);
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
CS & CS::get(const char* key, double* val1, double* val2)
{
  if (pmatch(key)){
    *val1 = ctof();
    *val2 = ctof();
    trace0("got 2 doubles");
    trace2(key, *val1, *val2);
  }else{
    trace0(key);
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
