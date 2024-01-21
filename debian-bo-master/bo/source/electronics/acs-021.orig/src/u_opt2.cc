/*$Id: u_opt2.cc,v 11.37 96/03/24 10:10:31 al Exp $ -*- C++ -*-
 * command and functions to access OPT class
 */
#include "constant.h"
#include "ap.h"
#include "error.h"
#include "io.h"
#include "u_opt.h"
#include "c_comand.h"
#include "s__.h"
#include "l_compar.h"
#include "l_lib.h"
/*--------------------------------------------------------------------------*/
//	void	OPT::command(CS& cmd);
//	bool	OPT::set(CS& cmd);
//	void	OPT::print(int where)
/*--------------------------------------------------------------------------*/
void OPT::command(CS& cmd)
{
  bool changed = set(cmd);
  if (!changed || opts){
    print(IO::mstdout);
  }
}
/*--------------------------------------------------------------------------*/
/* set:  set options from a string
 */
bool OPT::set(CS& cmd)
{
  bool changed = false;
  cmd.stuck();
  do{
    cmd.set("ACCT",	   &acct,	true);
    cmd.set("NOACCT",	   &acct,	false);
    cmd.set("LIST",	   &list,	true);
    cmd.set("NOLIST",	   &list,	false);
    cmd.set("MOD",	   &nomod,	false);
    cmd.set("NOMOD",	   &nomod,	true);
    cmd.set("PAGE",	   &page,	true);
    cmd.set("NOPAGE",	   &page,	false);
    cmd.set("NODE",	   &node,  	true);
    cmd.set("NONODE",	   &node,  	false);
    cmd.set("OPTS",	   &opts,	true);
    cmd.set("NOOPTS",	   &opts,	false);
    cmd.get("GMIN",	   &gmin,	mPOSITIVE);
    cmd.get("RELTOL",	   &reltol,	mPOSITIVE);
    cmd.get("ABSTOL",	   &abstol,	mPOSITIVE);
    cmd.get("VNTOL",	   &vntol,	mPOSITIVE);
    cmd.get("TRTOL",	   &trtol,	mPOSITIVE);
    cmd.get("CHGTOL",	   &chgtol,	mPOSITIVE);
    cmd.get("PIVTOL",	   &pivtol,	mPOSITIVE);
    cmd.get("PIVREL",	   &pivrel,	mPOSITIVE);
    cmd.get("NUMDGT",	   &numdgt);
    cmd.get("TNOM",	   &tnom,	mOFFSET, -ABS_ZERO);
    cmd.get("CPTIME",	   &cptime);
    cmd.get("LIMTIM",	   &limtim);
    cmd.get("LIMPTS",	   &limpts);
    cmd.get("LVLCOD",	   &lvlcod);
    cmd.get("LVLTIM",	   &lvltim);
    cmd.pmatch("METHOD");
    /**/ ::set(cmd, "GEAR",	  &method, mGEAR)
      || ::set(cmd, "TRAPezoidal",&method, mTRAPEZOID);
    cmd.get("MAXORD",	   &maxord);
    cmd.get("DEFL", 	   &defl,	mPOSITIVE);
    cmd.get("DEFW", 	   &defw,	mPOSITIVE);
    cmd.get("DEFAD",	   &defad,	mPOSITIVE);
    cmd.get("DEFAS",	   &defas,	mPOSITIVE);
    cmd.get("Seed",	   &seed);
    cmd.get("WCZero",	   &wczero,	mPOSITIVE);
    cmd.get("DAMPMAX",	   &dampmax,	mPOSITIVE);
    cmd.get("DAMPMIN",	   &dampmin,	mPOSITIVE);
    cmd.get("DAMPSTrategy",&dampstrategy, mOCTAL);
    cmd.get("Floor",	   &floor,	mPOSITIVE);
    cmd.get("Tempamb",	   &tempamb,	mOFFSET, -ABS_ZERO);
    cmd.get("Short",	   &shortckt,	mPOSITIVE);
    cmd.get("TRansits",	   &transits);
    cmd.get("INwidth",	   &inwidth);
    cmd.get("OUTwidth",	   &outwidth);
    cmd.get("XDivisions",  &xdivisions,	mPOSITIVE);
    cmd.get("YDivisions",  &ydivisions,	mPOSITIVE);
    /**/ ::set(cmd, "NAG",     &picky,	bNOERROR)
      || ::set(cmd, "NONAG",   &picky,	bTRACE)
      || ::set(cmd, "TRACE",   &picky,	bTRACE)
      || ::set(cmd, "NOTRACE", &picky,	bLOG)
      || ::set(cmd, "LOG",     &picky,	bLOG)
      || ::set(cmd, "NOLOG",   &picky,	bDEBUG)
      || ::set(cmd, "DEBUG",   &picky,	bDEBUG)
      || ::set(cmd, "NODEBUG", &picky,	bPICKY)
      || ::set(cmd, "PICKY",   &picky,	bPICKY)
      || ::set(cmd, "NOPICKY", &picky,	bWARNING)
      || ::set(cmd, "WARNing", &picky,	bWARNING)
      || ::set(cmd, "NOWARN",  &picky,	bDANGER)
      || ::set(cmd, "ERRor",   &picky,	bERROR)
      || ::set(cmd, "NOERRor", &picky,	bDISASTER)
      || ::set(cmd, "DISASTER",&picky,	bDISASTER);
    cmd.pmatch("ORder");
    /**/ ::set(cmd, "REVerse", &order,	oREVERSE)
      || ::set(cmd, "FORward", &order,	oFORWARD)
      || ::set(cmd, "AUTo",    &order,	oAUTO);
    cmd.pmatch("MODe");
    /**/ ::set(cmd, "ANAlog",  &mode,	mANALOG)
      || ::set(cmd, "DIGital", &mode,	mDIGITAL)
      || ::set(cmd, "MIXed",   &mode,	mMIXED);
    cmd.set("DUPcheck",	   &dupcheck,	true);
    cmd.set("NODUPcheck",  &dupcheck,	false);
    /**/ ::set(cmd, "BYPass",  &bypass,	bYES)
      || ::set(cmd, "NOBYPass",&bypass,	bNO)
      || ::set(cmd, "VBYPass", &bypass,	bVOLT);
    cmd.set("INCmode",	   &incmode,	true);
    cmd.set("NOIncmode",   &incmode,	false);
    cmd.set("LUBypasss",   &lubypass,	true);
    cmd.set("NOLUbypass",  &lubypass,	false);
    cmd.set("FBBypasss",   &fbbypass,	true);
    cmd.set("NOFBbypass",  &fbbypass,	false);
    cmd.set("TRACELoad",   &traceload,	true);
    cmd.set("NOTRACELoad", &traceload,	false);
    cmd.get("ITERMIN",	   &itermin);
    cmd.get("LIMIT",	   &limit,	mPOSITIVE);
    cmd.get("VMAX",	   &vmax);
    cmd.get("VMIN",	   &vmin);
    cmd.get("MRT",	   &dtmin,	mPOSITIVE);
    cmd.get("DTMIn",	   &dtmin,	mPOSITIVE);
    cmd.get("DTRatio",	   &dtratio,	mPOSITIVE);
    cmd.set("RSTray",	   &rstray,	true);
    cmd.set("NORSTray",	   &rstray,	false);
    cmd.set("CSTray",	   &cstray,	true);
    cmd.set("NOCSTray",	   &cstray,	false);
    cmd.get("Harmonics",   &harmonics);
    cmd.get("TRSTEPGrow",  &trstepgrow,	mPOSITIVE);
    cmd.get("TRSTEPShrink",&trstepshrink, mPOSITIVE);
    cmd.get("TRReject",	   &trreject,	mPOSITIVE);
    cmd.set("SHOWALL",	   &showall,	true);
    cmd.set("NOSHOWALL",   &showall,	false);
    cmd.get("FOOOO",	   &foooo);
    cmd.get("DIODEflags",  &diodeflags,	mOCTAL);
    cmd.get("MOSflags",	   &mosflags,	mOCTAL);
    cmd.get("ITL1",	   &itl[1]);
    cmd.get("ITL2",	   &itl[2]);
    cmd.get("ITL3",	   &itl[3]);
    cmd.get("ITL4",	   &itl[4]);
    cmd.get("ITL5",	   &itl[5]);
    cmd.get("ITL6",	   &itl[6]);
    cmd.get("ITL7",	   &itl[7]);
    cmd.get("ITL8",	   &itl[8]);
    if (cmd.stuck()){
      cmd.check(bWARNING);
      cmd.skiparg();
    }else{
      changed = true;
    }
  }while (cmd.more() && changed);

  if (changed){
    SIM::uninit();
    lowlim = 1 - reltol;
    uplim  = 1 + reltol;
    outwidth = min(abs(outwidth), MAXWIDTH);
  }
  return changed;
}
/*--------------------------------------------------------------------------*/
/* print: "print" all option values to "where"
 * string is in a form suitable for passing to set
 */
void OPT::print(int where)
{
  mprintf(where, ".options ");
  mprintf(where, " %sacct ", ((acct)?"":"no"));
  mprintf(where, " %slist ", ((list)?"":"no"));
  mprintf(where, " %smod ", ((nomod)?"no":""));
  mprintf(where, " %spage ", ((page)?"":"no"));
  mprintf(where, " %snode ", ((node)?"":"no"));
  mprintf(where, " %sopts ", ((opts)?"":"no"));
  mprintf(where, " gmin=%s ",   ftos(gmin,   "", 7, 0));
  mprintf(where, " reltol=%s ", ftos(reltol, "", 7, 0));
  mprintf(where, " abstol=%s ", ftos(abstol, "", 7, 0));
  mprintf(where, " vntol=%s ",  ftos(vntol,  "", 7, 0));
  mprintf(where, " trtol=%s ",  ftos(trtol,  "", 7, 0));
  mprintf(where, " chgtol=%s ", ftos(chgtol, "", 7, 0));
  mprintf(where, " pivtol=%s ", ftos(pivtol, "", 7, 0));
  mprintf(where, " pivrel=%s ", ftos(pivrel, "", 7, 0));
  mprintf(where, " numdgt=%d ", numdgt);
  mprintf(where, " tnom=%s ",   ftos(tnom+ABS_ZERO,   "", 7, 0));
  mprintf(where, " cptime=%d ", cptime);
  mprintf(where, " limtim=%d ", limtim);
  mprintf(where, " limpts=%d ", limpts);
  mprintf(where, " lvlcod=%d ", lvlcod);
  mprintf(where, " lvltim=%d ", lvltim);
  switch (method){
    case mTRAPEZOID: mprintf(where, " method=trapezoid "); break;
    case mGEAR:	     mprintf(where, " method=gear ");	   break;
    case mSTIFF:
    case mUNKNOWN:   assert(0);				   break;
  }
  mprintf(where, " maxord=%d ", maxord);
  for (int ii=1;  ii<ITL_COUNT;  ii++)
    mprintf(where, " itl%d=%d ", ii, itl[ii]);
  mprintf(where, " defl=%s ", ftos(defl, "", 7, 0));
  mprintf(where, " defw=%s ", ftos(defw, "", 7, 0));
  mprintf(where, " defad=%s ", ftos(defad, "", 7, 0));
  mprintf(where, " defas=%s ", ftos(defas, "", 7, 0));
  mprintf(where, " seed=%d ",   seed);
  mprintf(where, " wczero=%s ", ftos(wczero,  "", 7, 0));
  mprintf(where, " dampmax=%s ",   ftos(dampmax,    "", 7, 0));
  mprintf(where, " dampmin=%s ",   ftos(dampmin,    "", 7, 0));
  mprintf(where, " dampstrategy=%o ", dampstrategy);
  mprintf(where, " floor=%s ",  ftos(floor,   "", 7, 0));
  mprintf(where, " tempamb=%s ",ftos(tempamb+ABS_ZERO, "", 7, 0));
  mprintf(where, " short=%s ",  ftos(shortckt,"", 7, 0));
  mprintf(where, " in=%d ",     inwidth);
  mprintf(where, " out=%d ",    outwidth);
  mprintf(where, " xdivisions=%s ",  ftos(xdivisions,   "", 7, 0));
  mprintf(where, " ydivisions=%s ",  ftos(ydivisions,   "", 7, 0));
  switch (order){
    case oREVERSE: mprintf(where, " order=reverse "); break;
    case oFORWARD: mprintf(where, " order=forward "); break;
    case oAUTO:    mprintf(where, " order=auto ");    break;
  }
  switch (mode){
    case mANALOG:  mprintf(where, " mode=analog ");  break;
    case mDIGITAL: mprintf(where, " mode=digital "); break;
    case mMIXED:   mprintf(where, " mode=mixed ");   break;
  }
  mprintf(where, " transits=%d ",transits);
  mprintf(where," %sdupcheck ", ((dupcheck)?"":"no"));
  switch (bypass){
    case bNO:	mprintf(where, " nobypass "); break;
    case bYES:	mprintf(where, " bypass ");   break;
    case bVOLT:	mprintf(where, " vbypass ");  break;
  }
  mprintf(where, " %sincmode ", ((incmode)?"":"no"));    
  mprintf(where, " %slubypass ", ((lubypass)?"":"no"));    
  mprintf(where, " %sfbbypass ", ((fbbypass)?"":"no"));    
  mprintf(where, " %straceload ", ((traceload)?"":"no"));    
  mprintf(where, " itermin=%d ", itermin);
  mprintf(where, " limit=%s ",  ftos(limit,"", 7, 0));
  mprintf(where, " vmax=%s ",  ftos(vmax,"", 7, 0));
  mprintf(where, " vmin=%s ",  ftos(vmin,"", 7, 0));
  mprintf(where, " dtmin=%s ",  ftos(dtmin,"", 7, 0));
  mprintf(where, " dtratio=%s ",  ftos(dtratio,"", 7, 0));
  mprintf(where, " %srstray ", ((rstray)?"":"no"));    
  mprintf(where, " %scstray ", ((cstray)?"":"no"));    
  mprintf(where, " harmonics=%d ", harmonics);
  mprintf(where, " trstepgrow=%s ",  ftos(trstepgrow,"", 7, 0));
  mprintf(where, " trstepshrink=%s ",  ftos(trstepshrink,"", 7, 0));
  mprintf(where, " trreject=%s ",  ftos(trreject,"", 7, 0));
  mprintf(where, " diodeflags=%o ", diodeflags);
  mprintf(where, " mosflags=%o ", mosflags);
  mputc('\n',where);
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
