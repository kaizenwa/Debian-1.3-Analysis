/*$Id: d_switch.h,v 11.22 96/02/18 11:45:53 al Exp $ -*- C++ -*-
 * data structures for voltage (and current) controlled switch
 */
#include "e_model.h"
#include "e_elemnt.h"
#ifndef D_SWITCH_H
#define D_SWITCH_H
/*--------------------------------------------------------------------------*/
class MODEL_SWITCH : public MODEL_CARD {
friend class SWITCH_BASE;
private:
	MODEL_SWITCH(const MODEL_SWITCH&)	{assert(0);}
public:
	MODEL_SWITCH(const char *name = "");
	CARD*	clone()CONST{return new MODEL_SWITCH(*this);}
	void	parse(CS&);
	void	print(int,int)const;
private:
  double	vt;		/* threshold voltage */
  double	vh;		/* hysteresis voltage */
  double	ron;		/* on resistance */
  double	roff;		/* off resistance */
  enum control_t {VOLTAGE, CURRENT};
  control_t	type;		/* current or voltage controlled */
};
/*--------------------------------------------------------------------------*/
class SWITCH_COMMON : public COMPONENT_COMMON {
public:
	SWITCH_COMMON(){}
	SWITCH_COMMON(const SWITCH_COMMON& p){*this = p; attachcount=0;}
};
/*--------------------------------------------------------------------------*/
class SWITCH_BASE : public ELEMENT {
protected:
	SWITCH_BASE();
	SWITCH_BASE(CONST SWITCH_BASE& p);
	void	parse_sb(CS&,int);
	void	expand_sb();
public:
	void	print(int,int)const;
	bool	dotr();
	void	trload(){trload_passive();}
	void	trunload(){trunload_passive();}
	void	doac();
protected:
  char		inputlabel[LABELEN+1];/* this is here instead of in Cswitch */
  CARD*		input;		      /* due to bad design and lazyness */
  enum	{IPRINTNODES = 2, VPRINTNODES = 4, NUMNODES = 4};
private:
  enum state_t {UNKNOWN, ON, OFF};
  state_t	ic;		/* initial state, belongs in common */
  state_t	current_state;	/* state 1 iter ago (may be bad) */
  state_t	previous_state;	/* state 1 time or step ago (known good) */
};
/*--------------------------------------------------------------------------*/
class DEV_VSWITCH : public SWITCH_BASE {
public:
	DEV_VSWITCH(){}
	DEV_VSWITCH(CONST DEV_VSWITCH& p):SWITCH_BASE(p){}
	CARD*	clone()CONST{return new DEV_VSWITCH(*this);}
	void	parse(CS& cmd){parse_sb(cmd,4);}
	void	expand(){expand_sb();}
};
/*--------------------------------------------------------------------------*/
class DEV_CSWITCH : public SWITCH_BASE {
public:
	DEV_CSWITCH(){}
	DEV_CSWITCH(CONST DEV_CSWITCH& p):SWITCH_BASE(p){}
	CARD*	clone()CONST{return new DEV_CSWITCH(*this);}
	void	parse(CS& cmd){parse_sb(cmd,2);}
	void	expand();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
