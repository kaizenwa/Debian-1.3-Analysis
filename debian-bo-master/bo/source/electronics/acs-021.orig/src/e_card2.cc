/*$Id: e_card2.cc,v 11.28 96/03/03 23:07:32 al Exp $ -*- C++ -*-
 * Base class for "cards" in the circuit description file
 * This file contains factory methods -- to create and destroy them
 */
#include "error.h"
#include "u_opt.h"
#include "e_card.h"
#include "io_trace.h"
/*--------------------------------------------------------------------------*/
//		CARD::CARD();
//		CARD::CARD(const CARD& proto);
//		CARD::~CARD();
     generic_t*	create_extra_stuff(const generic_t*);
static	void	freelist(generic_t**);
/*--------------------------------------------------------------------------*/
/* default constructor for the base class
 * IMPLICITLY called when any object of a derived class is constructed
 */
CARD::CARD()
{
  devclass = NOTSET;
  x = NULL;
  nextcard = prevcard = stnext = stprev = this;
  subckt = parent = NULL;
  label[0] = '\0';
  for (int ii = 0;  ii <= NODESPERBRANCH;  ii++)
    nodes[ii].e = nodes[ii].t = nodes[ii].m = INVALIDNODE;
  n = nodes;
  label[0] = '\0';
  val = ic = 0.;
  method_u = mUNKNOWN;
  evaliter = 0;
  bypass = nodamp = converged = constant = false;
  trfun = NULL;
  acfun  = NULL;
  assert(y0.x == 0. && y0.f0 == 0. && y0.f1 == 0.);
  assert(y1 == y0);
  assert(y2 == y0);
}
/*--------------------------------------------------------------------------*/
/* copy constructor for the base class
 * called by copy constructor of any derived class
 * most of this only makes sense for derived objects, but it is universal
 */
CARD::CARD(const CARD& proto)
{
  devclass = proto.devclass;
  x = create_extra_stuff(proto.x);
  nextcard = prevcard = stnext = stprev = this;
  subckt = parent = NULL;
  strcpy(label, proto.label);
  for (int ii = 0;  ii <= NODESPERBRANCH;  ii++)
    nodes[ii] = proto.nodes[ii];
  n = nodes;
  val = proto.val;
  ic = proto.ic;
  method_u = proto.method_u;
  evaliter = 0;
  bypass = nodamp = converged = false;
  constant = proto.constant;
  trfun = proto.trfun;
  acfun = proto.acfun;
  assert(y0.x == 0. && y0.f0 == 0. && y0.f1 == 0.);
  assert(y1 == y0);
  assert(y2 == y0);
}
/*--------------------------------------------------------------------------*/
CARD::~CARD()
{
  unlink();
  freelist(&x);
  if (exists(subckt)){
    while (subckt != subckt->next()){
       delete subckt->next();
    }
    delete subckt;
  }
  trace1(printlabel(),1);
}
/*--------------------------------------------------------------------------*/
/* create_extra_stuff: copy the extra info for special parts (mosfets, etc)
 */
generic_t *create_extra_stuff(const generic_t *proto)
{
  if (proto){
    generic_t *x;
    x = (generic_t*)calloc(1,proto->ssize);
    if (!x)
      error(bERROR, "out of memory: extra_stuff\n");
    memcpy((void*)x, (const void*)proto, proto->ssize);
    x->x = create_extra_stuff(x->x);
    return x;
  }else{
    return NULL;
  }
}
/*--------------------------------------------------------------------------*/
/* freelist: free a (singly linked) list of structures   (recursive)
 * struct generic is an attempt to tame type clashes
 */
static void freelist(generic_t **x)
{
  if (x  &&  *x  &&  (*x)->x  &&  (*x)->x != (*x)->x->x){
    freelist(&((*x)->x));
  }
  if (x  &&  *x){	/* strictly, this is a memory leak, but in a subckt */
    //free((void*)*x);	/* it can point to a static area, leading to	    */
    *x = NULL;		/* possible core dump on exit.  commented out here  */
  }			/* as a quick patch.  This part will be removed in  */
}			/* a future release, using COMMON instead.	    */
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
