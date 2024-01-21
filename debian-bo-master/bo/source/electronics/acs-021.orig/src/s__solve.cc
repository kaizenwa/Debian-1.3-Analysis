/*$Id: s__solve.cc,v 11.37 96/03/24 10:09:32 al Exp $ -*- C++ -*-
 * solve one step of a transient or dc analysis
 */
#include "m_matrix.h"
#include "error.h"
#include "io.h"
#include "e_node.h"
#include "u_opt.h"
#include "u_status.h"
#include "e_card.h"
#include "s__.h"
#include "declare.h"	/* solve */
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
//	bool	SIM::solve(int,int);
//	void	SIM::advance_time();
//	void	SIM::count_iterations();
//	void	SIM::set_flags();
//	void	SIM::clear_arrays();
//	void	SIM::evaluate_models();
//	void	SIM::set_damp();
//	void	SIM::load_matrix();
//	void	SIM::solve_equations();
/*--------------------------------------------------------------------------*/
static bool converged = false;
/*--------------------------------------------------------------------------*/
bool SIM::solve(int itl, int trace)
{
  converged = false;
  int convergedcount = 0;
  
  STATUS::iter[iSTEP] = 0;
  advance_time();
  
  inc_mode = (inc_mode == false) ? BAD : inc_mode;
  damp = OPT::dampmax;
  
  do{
    if (trace >= tITERATION)
      print((double)-STATUS::iter[iSTEP]);
    count_iterations();
    set_flags();
    clear_arrays();
    evaluate_models();

    if (converged){
      ++convergedcount;
    }else{
      convergedcount = 0;
    }
    if (convergedcount <= OPT::itermin){
      converged = false;
    }
      
    if (!converged || !OPT::fbbypass || damp < .99){
      set_damp();
      load_matrix();
      solve_equations();
      if (STATUS::iter[iSTEP] >= OPT::itl[OPT::TRACE])
	IO::suppresserrors = false;
    }
  }while (!converged && STATUS::iter[iSTEP]<=itl);
  return converged;
}
/*--------------------------------------------------------------------------*/
void SIM::advance_time(void)
{
  if (SIM::mode == sTRAN){
    static double last_iter_time;	
    if (SIM::time0 > last_iter_time){	/* moving forward */
      for (int ii = 1;  ii <= STATUS::total_nodes;  ii++){
	SIM::vt1[ii] = SIM::v0[ii];
      }
    }else{				/* moving backward or DC */
      /* don't save voltages.  They're wrong !*/
    }
    last_iter_time = SIM::time0;
  }
}
/* last_iter_time is initially 0 by C definition.
 * On subsequent runs it will start with an arbitrary positive value.
 * SIM::time0 starts at either 0 or the ending time of the last run.
 * In either case, (time0 > last_iter_time) is false on the first step.
 * This correctly results in "don't save voltages..."
 */
/*--------------------------------------------------------------------------*/
void SIM::count_iterations()
{
  ++STATUS::iter[iPRINTSTEP];
  ++STATUS::iter[iSTEP];
  ++STATUS::iter[mode];
  ++STATUS::iter[iTOTAL];
}
/*--------------------------------------------------------------------------*/
void SIM::set_flags()
{
  fulldamp = false;
  inc_mode = (inc_mode == BAD || STATUS::iter[iSTEP]-1==OPT::itl[OPT::TRLOW])
      ? false
      : OPT::incmode;
  bypass_ok = (damp < OPT::dampmax*OPT::dampmax) ? false : bool(OPT::bypass);
}
/*--------------------------------------------------------------------------*/
void SIM::clear_arrays(void)
{
  if (!SIM::inc_mode){			/* Clear working array */
    SIM::aa.zero();
    SIM::aa.dezero(OPT::gmin);		/* gmin fudge */
    for (int ii = 1;  ii <= SIM::aa.size();  ii++){
      SIM::i[ii] = 0.;			/* Clear new right side */
    }
  }
  loadq.Clear();
}
/*--------------------------------------------------------------------------*/
void SIM::evaluate_models()
{
  STATUS::evaluate.start();
  converged = CARD::dotr_all();
  STATUS::evaluate.stop();
}
/*--------------------------------------------------------------------------*/
void SIM::set_damp()
{
  if (STATUS::iter[iSTEP] == 2 && !converged && OPT::dampstrategy&dsINIT){
    damp = OPT::dampmin;
  }else if (STATUS::iter[iSTEP] == 1  ||  converged){
    damp = OPT::dampmax;
  }else if (fulldamp){
    damp = OPT::dampmin;
  }else{
    damp = OPT::dampmax;
  }
  trace1("", damp);
}
/*--------------------------------------------------------------------------*/
void SIM::load_matrix()
{
  STATUS::load.start();
  if (OPT::traceload && SIM::inc_mode){
    while (!loadq.IsEmpty()){
      loadq.Pop()->trload();
    }
  }else{
    loadq.Clear();
    CARD::trload_all();
  }
  STATUS::load.stop();
}
/*--------------------------------------------------------------------------*/
void SIM::solve_equations()
{
  double *vnew;
  vnew = vi1;	/* save last iter voltages by swapping arrays */
  vi1 = v0;    
  v0 = NULL;
  lu.lu_decomp(aa, bool(OPT::lubypass && SIM::inc_mode));
  lu.fbsub(vnew, i, fw);
  v0 = vnew;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
