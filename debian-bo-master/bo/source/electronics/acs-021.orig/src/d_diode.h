/*$Id: d_diode.h,v 11.30 96/03/17 19:21:49 al Exp $ -*- C++ -*-
 * data structures and defaults for diode model.
 */
#include "e_subckt.h"
#include "e_model.h"
#ifndef D_DIODE_H
#define D_DIODE_H
/*--------------------------------------------------------------------------*/
class DEV_DIODE;		/* this file */
class DIODE_COMMON;
class MODEL_DIODE;
class EVAL_DIODE_Yj;
class EVAL_DIODE_Cj;
/*--------------------------------------------------------------------------*/
class DEV_ADMITTANCE;		/* external */
class DEV_CAPACITANCE;
/*--------------------------------------------------------------------------*/
  enum region_t {INITOFF=-2, REVERSE=-1, UNKNOWN=0, FORWARD=1};
/*--------------------------------------------------------------------------*/
class DEV_DIODE : public BASE_SUBCKT {
friend class DEV_MOS;
friend class EVAL_DIODE_Yj;
friend class EVAL_DIODE_Cj;
public:
	DEV_DIODE();
	DEV_DIODE(CONST DEV_DIODE& p);
	~DEV_DIODE(){--Count;}
	CARD*	clone()CONST{return new DEV_DIODE(*this);}
	void	parse(CS&);
 	void	print(int,int)const;
	double	probe_tr_num(const char*)const;
	void	expand();
static	int	count(){return Count;}
private:
  region_t	region;		/* fwd, reverse, unknown */
  double	isat;		/* is adjusted for temp, etc. */
  double	gd;		/* conductance to pass to capacitor */
  DEV_ADMITTANCE*   Yj;		/* subckt elements, for probe */
  DEV_CAPACITANCE*  Cj;
  enum	{NUMNODES = 2};
  static int	Count;
};
/*--------------------------------------------------------------------------*/
class DIODE_COMMON : public COMPONENT_COMMON {
public:
	DIODE_COMMON();
	DIODE_COMMON(const DIODE_COMMON& p){*this = p; attachcount=0;}
  double	is;		/* saturation current */
  double	rs;		/* series resistance */
  double	cj;		/* zero bias jct capacitance */
  double	cjsw;		/* zero bias sidewall capacitance */
  double	area;		/* area factor */
  double	perim;		/* perimeter factor */
  double	ic;		/* initial voltage */
  bool		off;		/* flag: assume reverse biased */
  struct {
    unsigned	is:1,
		rs:1,
		cj:1,
		cjsw:1;
  } calc;
};
/*--------------------------------------------------------------------------*/
class MODEL_DIODE : public MODEL_CARD {
friend class DEV_DIODE;
friend class EVAL_DIODE_Yj;
friend class EVAL_DIODE_Cj;
private:
	MODEL_DIODE(const MODEL_DIODE&)	{assert(0);}
public:
	MODEL_DIODE(const char *name = "");
	CARD*	clone()CONST{return new MODEL_DIODE(*this);}
	void	parse(CS&);
 	void	print(int,int)const;
protected:
  double	js;  /* is  */	/* saturation current (per area) */
  double	rs;		/* ohmic resistance (per area) */
  double	n;		/* emission coefficient */
  double	tt;		/* transit time */
  double	cjo;  /* cj  */	/* zero-bias jct capacitance (per area) */
  double	pb;   /* vj  */	/* junction potential */
  double	mj;   /* m   */	/* grading coefficient */
  double	eg;		/* activation energy */
  double	xti;		/* saturation-current temp. exp. */
  double	kf;		/* flicker noise coefficient */
  double	af;		/* flicker noise exponent */
  double	fc;		/* coef for fwd bis depl cap formula */
  double	bv;		/* reverse breakdown voltage */
  double	ibv;		/* current at reverse breakdown */
   				/* non-spice extensions */
  double	cjsw;		/* zero bias sidewall cap (per perim.) */
  double	mjsw;		/* sidewall grading coefficient */
  double	fcpb;		/* fc * pb */
};
/*--------------------------------------------------------------------------*/
class EVAL_DIODE_Yj : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
class EVAL_DIODE_Cj : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
