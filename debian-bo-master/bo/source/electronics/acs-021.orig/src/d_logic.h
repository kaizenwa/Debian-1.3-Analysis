/*$Id: d_logic.h,v 11.22 96/02/18 11:45:46 al Exp $ -*- C++ -*-
 * data structures and defaults for logic model.
 */
#include "e_model.h"
#include "e_elemnt.h"
#ifndef D_LOGIC_H
#define D_LOGIC_H
/*--------------------------------------------------------------------------*/
class MODEL_LOGIC : public MODEL_CARD {
friend class DEV_LOGIC;
friend	NODE*	tologic(const MODEL_LOGIC*,const node_t*);
friend	double	toanalog(const MODEL_LOGIC*,const node_t*);
public:
	MODEL_LOGIC(const char *name = "");
	MODEL_LOGIC(const MODEL_LOGIC&){assert(0);}
	CARD*	clone()CONST{return new MODEL_LOGIC(*this);}
	void	parse(CS&);
 	void	print(int,int)const;
private:
				/* ----- digital mode ----- */
  double	delay;		/* propagation delay */
				/* -- conversion parameters both ways -- */
  double	vmax;		/* nominal volts for logic 1 */
  double	vmin;		/* nominal volts for logic 0 */
  double	range;		/* vmax - vmin */
  				/* ---- D to A conversion ---- */
  double	rise;		/* rise time (time in slope) */
  double	fall;		/* fall time (time in slope) */
  double	rs;		/* series resistance -- strong */
  double	rw;		/* series resistance -- weak */
  				/* ---- A to D conversion ---- */
  double	th1;		/* threshold for 1 as fraction of range */
  double	th0;		/* threshold for 0 as fraction of range */
  				/* ---- quality judgement parameters ---- */
  double	mr;		/* margin rise - how much worse rise can be */
  double	mf;		/* margin fall - how much worse fall can be */
  double	over;		/* overshoot limit - as fraction of range */
};
/*--------------------------------------------------------------------------*/
enum {PORTSPERGATE = 10};
enum gatetype_t {lNONE, lAND, lNAND, lOR, lNOR, lXOR, lINV};
/*--------------------------------------------------------------------------*/
struct logic {
   generic_t	*x;
   size_t	ssize;
   const MODEL_Base *model;
   char 	modelname[LABELEN+1];
   node_t	n[PORTSPERGATE+1];	/* up to here must match subckt */
   					/* PORTSPERGATE <= PORTSPERSUBCKT */
   gatetype_t	type;			/* and, or, etc. */
   unsigned	incount;		/* count input nodes */
};
/*--------------------------------------------------------------------------*/
class DEV_LOGIC : public ELEMENT {
public:
	DEV_LOGIC();
	DEV_LOGIC(CONST DEV_LOGIC& p);
	~DEV_LOGIC(){--Count;}
	CARD*	clone()CONST{return new DEV_LOGIC(*this);}
	void	parse(CS&);
 	void	print(int,int)const;
	double	probe_tr_num(const char*)const;
	double	probe_ac_num(const char*)const;
	void	expand();
	bool	dotr();
	void	trload();
	void	trunload();
	void	doac();
	double	tr_review();
static	int	count(){return Count;}
private:
  smode_t	gatemode;
  static int	Count;
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
