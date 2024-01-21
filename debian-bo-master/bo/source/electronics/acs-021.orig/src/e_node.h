/*$Id: e_node.h,v 11.39 96/03/30 15:56:34 al Exp $ -*- C++ -*-
 * circuit node class
 */
#include "e_base.h"
#include "s__.h"
#ifndef E_NODE_H
#define E_NODE_H
/*--------------------------------------------------------------------------*/
class MODEL_LOGIC;
class xprobe_t;
/*--------------------------------------------------------------------------*/
#define NODESPERBRANCH	4
#define OUT1	0
#define OUT2	1
#define	IN1	2
#define	IN2	3
#define INVALIDNODE	-1
#define	qBAD	 (0)
#define qGOOD	 (OPT::transits)
/*--------------------------------------------------------------------------*/
class NODE : public CKT_BASE {
public:
    int		number;		/* external node number (redundant) */
    const MODEL_LOGIC *family;	/* logic family */
    int 	diter;		/* iteration of last update - digital */
    int 	aiter;		/* iteration of last update - analog */
    double	finaltime;	/* time logic transition attains final state */
    double	lastchange;	/* time of last change */
    double	dt;		/* time diff, for conversion */
    smode_t	nodemode;	/* simulation mode */
    bool	lv0;		/* old, current value */
    bool	lv1;		/* new, future value */
    bool	ls0;		/* old, current strength */
    bool	ls1;		/* new, future strength */
    int		quality;	/* quality of digital mode */
    int		needsanalog;	/* analog info requested (count/flag) */
    const char*	failuremode;
public:
		 NODE();
	char*	 printlabel(int where=0)const;
	double	 probe_tr_num(const char*)const;
	xprobe_t probe_ac_ext(const char*)const;
	double	 logicval()const;
	
 static int	 to_internal(int i){return SIM::nm[i];}
	double	 v0()const {return SIM::v0[to_internal(number)];}
	double	 vi1()const{return SIM::vi1[to_internal(number)];}
	double	 vt1()const{return SIM::vt1[to_internal(number)];}
	double	 vdc()const{return SIM::vdc[to_internal(number)];}
	COMPLEX	 vac()const{return SIM::ac[to_internal(number)];}
};
/*--------------------------------------------------------------------------*/
struct node_t {
  int m;		// mapped, after reordering
  int t;		// m == nm[t] if properly set up
  int e;		// external, user numbers, t = subs exp
  //node_t()		{m=INVALIDNODE;	t=INVALIDNODE;	e=INVALIDNODE;}
  //node_t(node_t& p)	{m=p.m;		t=p.t;		e=p.e;}
  // no constructor because of struct logic
	double	 v0()const {return SIM::v0[m];}	/* rvalues */
	double	 vi1()const{return SIM::vi1[m];}
	double	 vt1()const{return SIM::vt1[m];}
	double	 vdc()const{return SIM::vdc[m];}
	COMPLEX	 vac()const{return SIM::ac[m];}
	double&	 i()	   {return SIM::i[m];}	/* lvalues */
	COMPLEX& iac()	   {return SIM::ac[m];}
};
/*--------------------------------------------------------------------------*/
double volts_limited(const node_t & n1, const node_t & n2);
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
