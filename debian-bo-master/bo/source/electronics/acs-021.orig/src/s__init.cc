/* s__init.cc
 *$Header: /al/acs/src/RCS/s__init.cc,v 11.37 96/03/24 10:09:29 al Exp $
 * initialization (allocation, node mapping, etc)
 */
#include "m_matrix.h"
#include "e_node.h"
#include "e_card.h"
#include "s__.h"
#include "u_opt.h"
#include "u_status.h"
#include "error.h"
#include "l_jmpbuf.h"
/*--------------------------------------------------------------------------*/
//	 void	SIM::command_base(CS&);
//static void	SIM::init()
//	 void	SIM::reset_timers();
//static void	SIM::count_nodes();
//static void	SIM::alloc_hold_vectors();
//static void	SIM::determine_matrix_structure(const CARD *stop);
//	 void	SIM::alloc_vectors();
//static void	SIM::unalloc_vectors();
//static void	SIM::uninit();
/*--------------------------------------------------------------------------*/
extern JMP_BUF env;
extern run_mode_t run_mode;
extern NODE* nstat;
/*--------------------------------------------------------------------------*/
void SIM::command_base(CS& cmd)
{
  reset_timers();
  init();
  alloc_vectors();
  aa.allocate().dezero(OPT::gmin).setminpivot(OPT::pivtol);
  lu.allocate().dezero(OPT::gmin).setminpivot(OPT::pivtol);
  loadq.Resize(CARD::devicecount());
  JMP_BUF stash = env;
  if (!setjmp(env.p)){
    setup(cmd);
    STATUS::set_up.stop();
    if (run_mode == rEXECUTE)
      sweep();
    lu.unallocate();
    aa.unallocate();
    unalloc_vectors();
    env = stash;
    finish();
  }else{
    env = stash;
    finish();
    error(bERROR, "");
  }
  STATUS::total.stop();
}
/*--------------------------------------------------------------------------*/
/* init: allocate, set up, etc ... for any type of simulation
 * also called by status and probe for access to internals and subckts
 */
/*static*/ void SIM::init()
{
  if (!nstat){
    uninit();
    count_nodes();
    CARD::expand_all();
    map_nodes();
    alloc_hold_vectors();
    aa.reinit(STATUS::total_nodes);
    lu.reinit(STATUS::total_nodes);
    acx.reinit(STATUS::total_nodes);
    determine_matrix_structure(CARD::first());
  }
}
/*--------------------------------------------------------------------------*/
void SIM::reset_timers()
{
  STATUS::evaluate.reset();
  STATUS::load.reset();
  STATUS::lud.reset();
  STATUS::back.reset();
  STATUS::review.reset();
  STATUS::output.reset();
  STATUS::aux1.reset();
  STATUS::aux2.reset();
  STATUS::aux3.reset();
  STATUS::set_up.reset().start();
  STATUS::total.reset().start();
  STATUS::iter[mode] = 0;
  STATUS::iter[iPRINTSTEP] = 0;
}
/*--------------------------------------------------------------------------*/
/* count_nodes: count nodes in main ckt (not subckts)
 * update the variables "STATUS::total_nodes" and "STATUS::user_nodes"
 * zeros "STATUS::subckt_nodes" and "STATUS::model_nodes"
 */
/*static*/ void SIM::count_nodes()
{
  const CARD *stop;
  const CARD *brh;
  
  STATUS::user_nodes = 0;
  brh = stop = CARD::first();
  assert(exists(stop));
  do {
    if (brh->isdevice()){
      for (int ii = 0;  brh->n[ii].e != INVALIDNODE;  ii++){
	if (brh->n[ii].e > STATUS::user_nodes)
	  STATUS::user_nodes = brh->n[ii].e;
      }
    }
  } while (brh = brh->next(),  brh != stop);
  STATUS::total_nodes = STATUS::user_nodes;
  STATUS::subckt_nodes = STATUS::model_nodes = 0;
}
/*--------------------------------------------------------------------------*/
/* alloc_hold_vectors:
 * allocate space to hold data between commands.
 * for restart, convergence assistance, bias for AC, post-processing, etc.
 * must be done BEFORE deciding what array elements to allocate,
 * but after mapping
 * if they already exist, leave them alone to save data
 */
void SIM::alloc_hold_vectors()
{
  if (!nstat){
    nstat = new NODE[STATUS::total_nodes+1];
    assert(nstat);
    for (int ii=0;  ii <= STATUS::total_nodes;  ++ii){
      nstat[NODE::to_internal(ii)].number = ii;
    }
  }
  if (!vdc){
    vdc = new double[STATUS::total_nodes+1];
    assert(vdc);
    for (int ii=0;  ii <= STATUS::total_nodes;  ++ii){
      vdc[ii] = 0.;
    }
  }
}
/*--------------------------------------------------------------------------*/
/* determine_matrix_structure: 
 * scan the list and tell the matrix which entries to allocate
 * recursive for subckts
 */
void SIM::determine_matrix_structure(const CARD *stop)
{
  assert(exists(stop));
  const CARD *brh;
  brh = stop;
  do {
    if (brh->isdevice()){
      for (int ii = 0;  brh->n[ii].m != INVALIDNODE;  ii++){
	if (brh->n[ii].m != 0){
	  for (int jj = 0;  jj < ii ;  jj++){
	    aa.iwant(brh->n[ii].m,brh->n[jj].m);
	    lu.iwant(brh->n[ii].m,brh->n[jj].m);
	    acx.iwant(brh->n[ii].m,brh->n[jj].m);
	  }
	  ++nstat[brh->n[ii].m].needsanalog;
	}
      }
      if (brh->sub()){
	determine_matrix_structure(brh->sub());
      }
    }
  } while (brh = brh->next(), brh != stop);
}
/*--------------------------------------------------------------------------*/
/* alloc_vectors:
 * allocate space for the right-side vector
 * (initially current sources, on solution becomes voltages) and copies
 * used for one-time-ago and convergence checking
 * these are new with every run and are discarded after the run.
 */
void SIM::alloc_vectors()
{
  unalloc_vectors();	/* usually unnecessary. */
  assert(!ac);		/* may be needed after exception */
  assert(!i);
  assert(!v0);
  assert(!vi1);
  assert(!vt1);
  assert(!fw);
  ac = new COMPLEX[STATUS::total_nodes+1];
  i   = new double[STATUS::total_nodes+1];
  v0  = new double[STATUS::total_nodes+1];
  vi1 = new double[STATUS::total_nodes+1];
  vt1 = new double[STATUS::total_nodes+1];
  fw  = new double[STATUS::total_nodes+1];
  assert(ac);
  assert(i);
  assert(v0);
  assert(vi1);
  assert(vt1);
  assert(fw);
  for (int ii=0;  ii <= STATUS::total_nodes;  ++ii){
    ac[ii] = 0.;
    i[ii] = 0.;
    v0[ii] = 0.;
    vi1[ii] = 0.;
    vt1[ii] = 0.;
    fw[ii] = 0.;
  }
}
/*--------------------------------------------------------------------------*/
/*static*/ void SIM::unalloc_vectors()
{
  delete [] i;
  i = NULL;
  delete [] v0;
  v0 = NULL;
  delete [] vi1;
  vi1 = NULL;
  delete [] vt1;
  vt1 = NULL;
  delete [] fw;
  fw = NULL;
  delete [] ac;
  ac = NULL;
}
/*--------------------------------------------------------------------------*/
/* uninit: undo all the allocation associated with any simulation
 * called when the circuit changes after a run, so it needs a restart
 * may be called multiple times without damage to make sure it is clean
 */
/*static*/ void SIM::uninit()
{
#if !defined(SGI_CC)	/* for some reason this generates bad code on 	    */
  delete [] nstat;	/* SGI Indy with the native compiler (NCC or DCC)   */
#endif			/* The linker reports something like "bad relocation*/
  nstat = NULL;		/* entry".  This workaround causes a memory leak,   */
  aa.reinit(0);		/* but at least you can compile it.		    */
  lu.reinit(0);		/* g++ (2.6.3) does not have the problem.	    */
  acx.reinit(0);
  delete [] nm;
  nm = NULL;
  delete [] vdc;
  vdc = NULL;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
