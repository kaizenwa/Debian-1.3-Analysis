/*$Id: e_card.h,v 11.34 96/03/21 22:37:48 al Exp $ -*- C++ -*-
 * branch structure type definitions
 * device types (enumeration type?) 
 */
#include "md.h"
#include "constant.h"
#include "e_base.h"
#include "e_node.h"
#include "u_status.h"
#include "u_opt.h"
#ifndef E_CARD_H
#define E_CARD_H
/*--------------------------------------------------------------------------*/
class CS;
class COMPONENT;
/*--------------------------------------------------------------------------*/
struct fpoly1_t{	/* first order polynomial	*/
   double  x;		/* the argument			*/
   double  f0;		/* the function (c0 + x*f1)	*/
   double  f1;		/* the first derivative		*/
   fpoly1_t()			{x=0.;	f0=0.;	f1=0;	}
   fpoly1_t(const fpoly1_t& p)	{x=p.x;	f0=p.f0;f1=p.f1;}
   bool operator==(const fpoly1_t& p)const
		{return (f1==p.f1 && f0==p.f0 && x==p.x);}
};
/*--------------------------------------------------------------------------*/
struct cpoly1_t{	/* first order polynomial	*/
   double  x;		/* the argument			*/
   double  c0;		/* f(x) - x*f'(x), or f0 - x*f1 */
   double  f1;		/* the first derivative		*/
   cpoly1_t()			{x=0.;	c0=0.;	f1=0;	}
   cpoly1_t(const cpoly1_t& p)	{x=p.x;	c0=p.c0;f1=p.f1;}
   bool operator==(const cpoly1_t& p)const
		{return (f1==p.f1 && c0==p.c0 && x==p.x);}
};
/*--------------------------------------------------------------------------*/
struct generic_t {
  generic_t *x;
  size_t  ssize;
  const generic_t *m;
  char    modelname[LABELEN+1];
  char	  args[1];	// a fudge.  sizeof() >= 1
};
/*--------------------------------------------------------------------------*/
class CARD : public CKT_BASE {
private:
  static CARD* rootcard;
public: // d_subckt, c_getckt
  static CARD* putbefore;
protected:
  enum devclass_t
  	{NOTSET=-1,NOTDEVICE=0,ONEPORT,TWOPORT,SOURCE,SUBCKT,OTHERDEVICE};
  devclass_t devclass;	// something to classify the devices
  int	   evaliter;	// model eval iteration number

private:
  CARD*    nextcard;	// linked list stuff
  CARD*    prevcard;
protected:
  generic_t *x;		// extra stuff
  CARD*    stnext;	// list of same type
  CARD*    stprev;
  CARD*    subckt;	// subckt expansion
public: // commons, u_prblst
  CARD*    parent;
protected:
  char     label[LABELEN+1];
public: //insert, s__init, s__map
  node_t*  n;
private:
  node_t   nodes[NODESPERBRANCH+1]; // nodes (0,1:out, 2,3:in)
public: // mos commons
  double   val;		// value, for simple parts
protected:
  double   ic;		// initial condition
  method_t method_u;	// method to use for this part per user
public: // commons
  fpoly1_t y0;		// iteration parameters, new
  fpoly1_t y1;		// iteration parameters, 1 iter ago
  fpoly1_t y2;		// iteration parameters, 2 iter ago
protected:
  double   acbias;	// dc bias for ac use
  COMPLEX  ev;		/* ac effective value (usually real)	*/
public:
  bool bypass;		// is bypassing now
  bool nodamp;		// don't apply iteration damping
  bool converged;	// convergence status
  bool constant;	// eval stays the same every iteration
//--------------------------------------------------------------------
protected:				// special evaluation functions
	void	(*trfun)(COMPONENT*);
	void	(*acfun)(COMPONENT*);
//--------------------------------------------------------------------
protected:   				// iterators.  e_card1.cc
	void	expand_group();
	bool	dotr_group();
	void	trload_group();
	void	trunload_group();
	void	doac_group();
	double	tr_review_group();
//--------------------------------------------------------------------
public:					// interface to the iterators - inline
 static	void	expand_all(){rootcard->expand_group();}
 static	bool	dotr_all(){return rootcard->dotr_group();}
 static	void	trload_all(){rootcard->trload_group();}
 static	void	trunload_all(){rootcard->trunload_group();}
 static	void	doac_all(){rootcard->doac_group();}
 static	double	tr_review_all(){return rootcard->tr_review_group();}
//--------------------------------------------------------------------
public:					// finding one of them - inline
	CARD*	next()const{return nextcard;}
	CARD*	prev()const{return prevcard;}
	CARD*	sub()const{return subckt;}
 static CARD*	root(){return rootcard;}
 static CARD*	first(){return rootcard->next();}
 static CARD*	last(){return rootcard->prev();}
//--------------------------------------------------------------------
public:					// virtuals. -- the important stuff
 virtual CARD*	clone()CONST = 0;	// either stubs or pure
 virtual void	parse(CS&) = 0;
 virtual void	print(int,int)const = 0;
 virtual void	expand(){}
 virtual bool	dotr(){return true;}
 virtual void	trload(){}
 virtual void	trunload(){}
 virtual void	doac(){}
 virtual double	tr_review(){return BIGBIG;};
//--------------------------------------------------------------------
//					// virtuals defined in base
//virtual char*	   printlabel(int where=0)const;    // not overloaded here
//virtual char*    probe_txt(const char*)const;	    // shown here only
//virtual double   probe_num(const char*)const;	    // to complete the list
//virtual double   probe_tr_num(const char*)const;
//virtual double   probe_ac_num(const char*)const;
//virtual xprobe_t probe_ac_ext(const char*)const;
//--------------------------------------------------------------------
protected:				// create and destroy.  e_card2.cc
		CARD();
		CARD(const CARD&);
public:
 virtual	~CARD();
//--------------------------------------------------------------------
public:					// list management.  e_card3.cc
	CARD*	insertbefore(CARD* before);
	CARD*	insertafter(CARD* after);
private:
	void	unlink();
//--------------------------------------------------------------------
public:					// query functions.  e_card4.cc
	char*	printlabel(int where=0)const;
 static	double	probe(const CARD*,const char*);
 	int	connects_to(int node)const;
//--------------------------------------------------------------------
protected:				// query functions. deferred inline
	bool	evaluated();
//--------------------------------------------------------------------
public:					// query functions. inline
	bool	is1port()const{return devclass==ONEPORT;}
	bool	is2port()const{return devclass==TWOPORT;}
	bool	issource()const{return devclass==SOURCE;}
	bool	issubckt()const{return devclass==SUBCKT;}
	bool	isdevice()const{return devclass>NOTDEVICE;}
//--------------------------------------------------------------------
#if !defined(NDEBUG)
protected:				// for debugging
	bool	isbypassed()const
	  	{return (parent && (parent->bypass || parent->isbypassed()));}
#endif
//--------------------------------------------------------------------
public:					// modifiers.  inline
	void	setvalue(double v){val = v;}
	void	setvaluex(double v){val = v; x = NULL;}
//--------------------------------------------------------------------
// a very bad way to do parameter passing between the device and expression
 static double initial_voltage;
 static double initial_current;
 static double initial_condition;	/* could be either */ 
//--------------------------------------------------------------------
//	friends because the translation to C++ is incomplete
  friend  CARD*  findbranch(CS&,CARD*,CARD*);
  friend  CARD*  findbranch_samescope(const char*,CARD*);
  friend  const CARD* findbranch_sametype(const char*,const CARD*);
  friend  void   expandsubckt(CARD*,const char*);  // e_subckt.h
  friend  void	 acfix(COMPONENT*);    // e_exp.h
  friend  void   trfix(COMPONENT*);
  friend  /*static*/ void  trf_max(const CARD*,double**,cpoly1_t*);  
  friend  class  DC;
  friend  class  CARDSTASH;
  friend  class  DEV_CCCS;
  friend  class  DEV_CCVS;
  friend  class  DEV_MUTUAL_L;
  friend  class  DEV_INDUCTANCE;
  friend  class  DEV_DIODE;
  friend  class  DEV_LOGIC;
  friend  class  DEV_MOS;
  friend  class  DEV_SUBCKT;
  friend  class  COMPONENT;
  friend  class  BASE_SUBCKT;
  friend  class  MODEL_Base;
};

#define exists(c)  (c)
generic_t*  create_extra_stuff(const generic_t*);
/*--------------------------------------------------------------------------*/
inline bool CARD::evaluated()
{
  if (evaliter == STATUS::iter[iTOTAL]){
    return true;
  }else{
    evaliter = STATUS::iter[iTOTAL];
    return false;
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
