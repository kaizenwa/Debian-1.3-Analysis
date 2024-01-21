/*$Id: u_prblst.cc,v 11.38 96/03/24 18:00:43 al Exp $ -*- C++ -*-
 * probe list functions
 */
#include "ap.h"
#include "l_lib.h"
#include "error.h"
#include "io.h"
#include "u_status.h"
#include "e_node.h"
#include "e_card.h"
#include "u_probe.h"
#include "u_prblst.h"
/*--------------------------------------------------------------------------*/
//	void	PROBE_LISTS::purge(CKT_BASE*);
//		PROBELIST::PROBELIST();
//		PROBELIST::~PROBELIST();
//	PROBE& 	PROBELIST::operator[](int)const;
//	int	PROBELIST::list(const char*)const;
//	int	PROBELIST::clear(void);
//  	void	PROBELIST::operator-=(CS&);
//  	void	PROBELIST::operator-=(CKT_BASE*);
//	void	PROBELIST::operator+=(CS&);
//	void	PROBELIST::add_all_nodes(const char*);
//	void	PROBELIST::add_node_list(CS&,const char*);
//	void	PROBELIST::add_branches(CS&,const char*);
//	void	PROBELIST::init(void);
/*--------------------------------------------------------------------------*/
extern NODE* nstat;
extern const char e_int[];
/*--------------------------------------------------------------------------*/
void PROBE_LISTS::purge(CKT_BASE* brh)
{
  for (int i = 0;  i < sCOUNT; ++i){
    alarm[i] -= brh;
    plot[i]  -= brh;
    print[i] -= brh;
    store[i] -= brh;
  }
}
/*--------------------------------------------------------------------------*/
PROBELIST::PROBELIST()
{
  bag = NULL;
  probecount = 0;
}
/*--------------------------------------------------------------------------*/
PROBELIST::~PROBELIST()	/* BUG: sometimes causes segmentation fault on exit */
{			/* not really a memory leak because all PROBELIST   */
  if (bag){		/* objects are static				    */
    //delete [] bag;	/* Since default constructor sets it to NULL, and   */
    bag = NULL;		/* init allocates memory with new, setting it to    */
  }			/* a good value, and it is not set or deleted       */
}			/* anywhere else, how can it possibly have an	    */
			/* invalid non-zero value?			    */
/*--------------------------------------------------------------------------*/
PROBE& PROBELIST::operator[](int i)const
{
  static PROBE badprobe;
  if (0 <= i  &&  i < probecount){
    assert(bag);
    return bag[i];
  }else{
    error(bDANGER, e_int, "probe out of range");
    return badprobe;
  }
}
/*--------------------------------------------------------------------------*/
int PROBELIST::list(const char *label)const
{
  mprintf(IO::mstdout, "%-7s", label);
  for (int ii = 0;  ii < probecount;  ii++){
    assert(bag);
    mprintf(IO::mstdout, " %s", bag[ii].label());
    if (bag[ii].range() != 0.){
      mprintf(IO::mstdout, "(%s,%s)",
	      ftos(bag[ii].lo(),"",5,IO::formaat),
	      ftos(bag[ii].hi(),"",5,IO::formaat));
    }
  }
  mprintf(IO::mstdout, "\n");
  return probecount;
}
/*--------------------------------------------------------------------------*/
int PROBELIST::clear(void)
{
  if(bag){
    for (int ii = 0;  ii < probecount;  ii++){
      bag[ii].detach();
    }
  }
  return probecount = 0;
}
/*--------------------------------------------------------------------------*/
void PROBELIST::operator-=(CS& cmd)
{    					/* BUG: ctostr may not advance */
  int paren = 0;			/* if its 1st char is a term */
  char parameter[BUFLEN+1];		/* can cause loss of rest of str */
  int dropcount = 0;

  int mark = cmd.cursor();

  cmd.ctostr(parameter,BUFLEN,TOKENTERM);
  strcat(parameter,"(");
  paren += cmd.skiplparen();			/* device, node, etc. */
  cmd.ctostr(&(parameter[strlen(parameter)]), 
		    BUFLEN-(int)strlen(parameter), TOKENTERM);
  strcat(parameter,")");
  paren -= cmd.skiprparen();
  if (paren != 0  || !*parameter)
    cmd.warn(bWARNING);
  
  for (int ii = 0;  ii < probecount;  ii++){
    assert(bag);
    if (dropcount > 0){
      bag[ii-dropcount] = bag[ii];
    }
    if (wmatch(bag[ii].label(),parameter)){
      bag[ii].detach();
      dropcount++;
    }
  }
  if (dropcount == 0){
    cmd.warn(bWARNING,mark);
  }
  probecount -= dropcount;
}
/*--------------------------------------------------------------------------*/
void PROBELIST::operator-=(CKT_BASE *brh)
{
  assert(brh);
  int dropcount = 0;
  for (int ii = 0;  ii < probecount;  ii++){
    assert(bag);
    if (dropcount > 0){
      bag[ii-dropcount] = bag[ii];
    }
    if (bag[ii].object() == brh){
      bag[ii].detach();
      dropcount++;
    }
  }
  probecount -= dropcount;
}
/*--------------------------------------------------------------------------*/
/* add_list: add a "list" of probes, usually only one
 * 	this means possibly several probes with a single parameter
 *	like "v(r*)" meaning all resistors
 *	but not "v(r4) v(r5)" which has two parameters
 * 	it also takes care of setting the range for plot or alarm
 */

void PROBELIST::operator+=(CS& cmd)
{    					/* BUG: ctostr may not advance */
  /* if its 1st char is a term */
  /* can cause loss of rest of str */
  char what[LABELEN+1];
  int oldcount = probecount;
  int paren = 0;

  init();
  assert(bag);

  cmd.ctostr(what,LABELEN,TOKENTERM);	/* parameter */
  if (!*(what))
    cmd.warn(bWARNING);
  paren += cmd.skiplparen();			/* device, node, etc. */

  if (cmd.pmatch("NODES")){			/* all nodes */
    add_all_nodes(what);
  }else if (cmd.is_digit()){			/* listed nodes (numbered) */
    add_node_list(cmd,what);
  }else{					/* branches */
    add_branches(cmd,what);
  }

  paren -= cmd.skiprparen();
  if (paren != 0)
    cmd.warn(bWARNING);

  if (cmd.skiplparen()){			/* range for plotting */
    double lo = cmd.ctof();
    double hi = cmd.ctof();
    while (oldcount < probecount){
      bag[oldcount].limit(lo,hi);
      ++oldcount;
    }
    if (!cmd.skiprparen())
      cmd.check(bWARNING);
  }
  assert(probecount <= PROBECOUNT);
}
/*--------------------------------------------------------------------------*/
void PROBELIST::add_all_nodes(const char *what)
{
  assert(bag);
  for (int node = 1;  node <= STATUS::user_nodes;  node++){
    if (nstat[NODE::to_internal(node)].needsanalog){
      if (probecount >= PROBECOUNT){
	error(bDANGER, e_int, "too many probes");
	break;
      }else{
	bag[probecount++] = PROBE(what,node);
      }
    }
  }
}
/*--------------------------------------------------------------------------*/
/* add_node_list: add nodes to probe list
 * 	adds a range or single node
 *	a list of numbers is consumed from cmd
 */
void PROBELIST::add_node_list(CS& cmd, const char *what)
{    
  assert(bag);
  while (cmd.is_digit()){
    int mark = cmd.cursor();
    int node = cmd.ctoi();
    if (node <= STATUS::total_nodes){
      if (probecount >= PROBECOUNT){
	error(bDANGER, e_int, "too many probes");
	break;
      }else{
	bag[probecount++] = PROBE(what,node);
      }
    }else{
      cmd.warn(bWARNING,mark);
    }
  }
}
/*--------------------------------------------------------------------------*/
/* add_branches: add net elements to probe list
 * 	all matching a label with wildcards
 *	only one argument is consumed from cmd
 */
void PROBELIST::add_branches(CS& cmd, const char *what)
{
  CARD *brh;
  int mark = cmd.cursor();
  int cmax = cmd.cursor();
  
  assert(bag);
  brh = CARD::first();
  for (;;){
    cmd.reset(mark);
    brh = findbranch(cmd, brh, CARD::root());
    cmax = max(cmax, cmd.cursor());
    if (!exists(brh))
      break;
    if (probecount >= PROBECOUNT){
      error(bDANGER, e_int, "too many probes");
      break;
    }else{
      brh->incprobes();
      bag[probecount++] = PROBE(what,0,brh);
      while (exists(brh->parent)){	/* BUG: don't get lost */
	brh = brh->parent;		/* in a subckt */
      }
    }
    brh = brh->next();
  }
  cmd.reset(cmax);
  if (mark == cmax){
    cmd.check(bWARNING);
    cmd.skiparg();
  }
}
/*--------------------------------------------------------------------------*/
void PROBELIST::init(void)
{
  if (!bag)
    bag = new PROBE[PROBECOUNT+1];
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
