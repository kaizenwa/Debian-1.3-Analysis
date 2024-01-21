/*$Id: c_sim.cc,v 11.28 96/03/03 23:07:02 al Exp $ -*- C++ -*-
 * command interface functions associated with the SIM base class
 */
#include "c_comand.h"
#include "s_ac.h"
#include "s_dc.h"
#include "s_tr.h"
#include "s_fo.h"
/*--------------------------------------------------------------------------*/
void CMD::ac(CS& cmd)		{static AC ac;		ac.command(cmd);}
void CMD::dc(CS& cmd)		{static DC dc;		dc.command(cmd);}
void CMD::fourier(CS& cmd)	{static FOURIER fo;	fo.command(cmd);}
void CMD::op(CS& cmd)		{static OP op;		op.command(cmd);}
void CMD::tr(CS& cmd)		{static TRANSIENT tr;	tr.command(cmd);}
void CMD::mark(CS&)		{SIM::freezetime = true;}
void CMD::unmark(CS&)		{SIM::freezetime = false;}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
