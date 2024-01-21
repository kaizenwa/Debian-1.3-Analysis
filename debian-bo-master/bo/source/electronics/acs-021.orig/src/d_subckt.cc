/*$Id: d_subckt.cc,v 11.35 96/03/22 18:09:59 al Exp $ -*- C++ -*-
 * subcircuit stuff
 * base class for other elements using internal subckts
 * netlist syntax:
 * device: Xxxxx <nodelist> <subckt-name>
 * model:  .subckt <subckt-name> <nodelist>
 *	   (device cards)
 *	   .ends <subckt-name>
 */
#include "ap.h"
#include "c_comand.h"
#include "d_coment.h"
#include "d_subckt.h"
#include "error.h"
#include "io.h"
#include "mode.h"
#include "declare.h"	/* newnode_subckt */
#include "l_astack.h"
/*--------------------------------------------------------------------------*/
//		MODEL_SUBCKT::MODEL_SUBCKT(const char*);
//	void	MODEL_SUBCKT::parse(CS& cmd);
// 	void	MODEL_SUBCKT::print(int,int)const;

//	void	CMD::ends(CS&);

//		DEV_SUBCKT::DEV_SUBCKT();
//		DEV_SUBCKT::DEV_SUBCKT(CONST DEV_SUBCKT& p);
//	void	DEV_SUBCKT::parse(CS& cmd);
// 	void	DEV_SUBCKT::print(int,int)const;
//	void	DEV_SUBCKT::expand();
	void    expandsubckt(CARD*,const char*);
//	double	DEV_SUBCKT::probe_tr_num(const char *what)const;
/*--------------------------------------------------------------------------*/
static struct subckt defalt = {(generic_t*)NULL, sizeof(struct subckt), 
   (generic_t*)NULL, sDEFAULT_modelname, /*more*/};
static struct smod defaltmodel = {(generic_t*)NULL, sizeof(struct smod), 
   (generic_t*)NULL, sDEFAULT_modelname, /*more*/};
static MODEL_SUBCKT modellist = MODEL_SUBCKT(sDEFAULT_modelname);

struct SSNODE {
  char  name[LABELEN+1];
  CARD* pb;
  SSNODE(const char* Name = "", CARD * c = NULL):pb(c){strcpy(name,Name);}
};
static ASTACK<SSNODE> substack(RECURSE);

int DEV_SUBCKT::Count = 0;
/*--------------------------------------------------------------------------*/
MODEL_SUBCKT::MODEL_SUBCKT(const char *name)
{
  struct smod *xx;
  x = create_extra_stuff((generic_t*)&defaltmodel);
  xx = (struct smod*)x;
  n = xx->n;
  devclass = NOTDEVICE;
  strncpy(label, name, LABELEN);
  label[LABELEN] = '\0';
  stprev = &modellist;
}
/*--------------------------------------------------------------------------*/
void MODEL_SUBCKT::parse(CS& cmd)
{
  struct smod *m;
  m = (struct smod*)x;

  if (substack.IsFull())
    error(bERROR,"%s: subckt nesting too deep\n", printlabel());

  cmd.skiparg();	/* skip known ".subckt" */
  cmd.ctostr(label, LABELEN, TOKENTERM);
  parsenodes(cmd, PORTSPERSUBCKT);

  SSNODE t(label, CARD::putbefore);
  substack << t;

  subckt = CARD::putbefore = new DEV_COMMENT;
  m->x = (generic_t*)NULL;
}
/*--------------------------------------------------------------------------*/
void MODEL_SUBCKT::print(int where, int)const
{
  CARD *brh, *stop;

  mprintf(where, ".subckt %s ", label);
  printnodes(where, PORTSPERSUBCKT);
  mprintf(where, "\n");

  brh = stop = subckt;
  if (brh){
    do {
      brh->print(where, false);
    } while (brh = brh->next(),  brh != stop);
  }
  mprintf(where, "*+ends %s\n", label);
}
/*--------------------------------------------------------------------------*/
void CMD::ends(CS& cmd)
{
  SSNODE t;
  if (substack.IsEmpty()){
    error(bWARNING, "ends not in subckt\n");
  }else{
    substack >> t;
  }
  if (cmd.more()){
    if (!cmd.pmatch(t.name)){
      error(bERROR, "ends tag [%s] does not match subckt [%s]\n",
	    cmd.tail(), t.name);
    }
  }else{
    substack.Clear();
  }
  CARD::putbefore = t.pb;
}
/*--------------------------------------------------------------------------*/
DEV_SUBCKT::DEV_SUBCKT()
{
  struct subckt *xx;
  x = create_extra_stuff((generic_t*)&defalt);
  xx = (struct subckt*)x;
  n = xx->n;
  devclass = SUBCKT;
  ++Count;
}
/*--------------------------------------------------------------------------*/
DEV_SUBCKT::DEV_SUBCKT(CONST DEV_SUBCKT& p):BASE_SUBCKT(p)
{
  struct subckt *xx;
  xx = (struct subckt*)x;
  n = xx->n;
  ++Count;
}
/*--------------------------------------------------------------------------*/
void DEV_SUBCKT::parse(CS& cmd)
{
  struct subckt *xx;
  xx = (struct subckt*)x;

  parselabel(cmd);
  parsenodes(cmd, PORTSPERSUBCKT);
  cmd.ctostr(xx->modelname, LABELEN, TOKENTERM);
}
/*--------------------------------------------------------------------------*/
void DEV_SUBCKT::print(int where, int)const
{
  struct subckt *xx;
  xx = (struct subckt*)x;
  
  printlabel(where);
  printnodes(where, PORTSPERSUBCKT);
  mprintf(where, " %s\n",    xx->modelname);
}
/*--------------------------------------------------------------------------*/
void DEV_SUBCKT::expand()
{
  struct subckt *xx;
  xx = (struct subckt*)x;
  expandsubckt(this,xx->modelname);
  if (!subckt){
    error(bERROR, "");
  }
  subckt->expand_group();
  assert(!constant); /* because I have more work to do */
}
/*--------------------------------------------------------------------------*/
void expandsubckt(CARD *brh, const char *modelname)
{
  const CARD *model;
  CARD *scan;
  CARD *stop;
  int map[NODESPERSUBCKT];
  
  model = findbranch_sametype(modelname, &modellist);
  if (!model){
    error(bDANGER,"%s: can't find subckt: %s\n",brh->printlabel(),modelname);
    brh->subckt = (CARD*)NULL;
    return;
  }
  
  for (int i = 0; i < NODESPERSUBCKT; i++) /* initialize: all nodes unused */
    map[i] = UNUSED;
  
  stop = scan = model->subckt;
  do {						 /* scan elements of subckt */
    if (scan->isdevice()){				 /* mark nodes used */
      for (int ii = 0;  scan->n[ii].e != INVALIDNODE;  ii++){
	if (scan->n[ii].e > NODESPERSUBCKT)
	  error(bERROR,"%s: too many internal nodes\n",model->printlabel());
	map[scan->n[ii].e] = USED;
      }
    }
  } while (scan = scan->next(),  scan != stop);
  
  map[0] = 0;
  for (int port = 0; model->n[port].e != INVALIDNODE; port++){ /* map ports */
    if (model->n[port].e > NODESPERSUBCKT)
      error(bERROR, "internal error: subckt node out of range: %s\n",
	    model->printlabel());
    map[model->n[port].e] = brh->n[port].t;
  }
  
  {for (int ii = 0;  ii < NODESPERSUBCKT;  ii++){
    if (map[ii] == USED){
      map[ii] = newnode_subckt();	 /* assign number to internal nodes */
    }
  }}
  
  if (!brh->subckt){
    error(bTRACE, "%s: expanding\n", brh->printlabel());
    stop = scan = model->subckt;
    do {
      if (scan->isdevice()){
	CARD *scratch;				     	     /* copy subckt */
	scratch = scan->clone();
	scratch->parent = brh;
	brh->subckt = scratch->insertbefore(brh->subckt);
      }
    } while (scan = scan->next(),  scan != stop);
  }else{
    error(bTRACE, "%s: re-expanding\n", brh->printlabel());
  }
  
  stop = scan = brh->subckt;
  do {							     /* patch nodes */
    if (scan->isdevice()){
      int ii;
      for (ii = 0;  scan->n[ii].e != INVALIDNODE;  ii++){
	assert(scan->n[ii].e >= 0);	 /* bad node? */
	assert(map[scan->n[ii].e] >= 0); /* node map? */
	scan->n[ii].t = map[scan->n[ii].e];
      }
      scan->n[ii].t = INVALIDNODE;
    }
  } while (scan = scan->next(),  scan != stop);
}
/*--------------------------------------------------------------------------*/
double DEV_SUBCKT::probe_tr_num(const char *what)const
{
  CS cmd(what);
  assert(subckt);
  
  if (cmd.pmatch("V")){
    int nn = cmd.ctoi();			/* BUG: no bounds check */
    printf("subckt probe\n");
    return n[nn+1].v0();
  }else if (cmd.pmatch("P")){
    CARD *pb, *stop;
    double power = 0.;
    stop = pb = subckt;
    do {
      power += CARD::probe(pb,"P");
    } while (pb = pb->next(), pb != stop);
    return power;
  }else if (cmd.pmatch("PD")){
    CARD *pb, *stop;
    double power = 0.;
    stop = pb = subckt;
    do {
      power += CARD::probe(pb,"PD");
    } while (pb = pb->next(), pb != stop);
    return power;
  }else if (cmd.pmatch("PS")){
    CARD *pb, *stop;
    double power = 0.;
    stop = pb = subckt;
    do {
      power += CARD::probe(pb,"PS");
    } while (pb = pb->next(), pb != stop);
    return power;
  }else{ /* bad parameter */
    return NOT_VALID;
  }
  /*NOTREACHED*/
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
