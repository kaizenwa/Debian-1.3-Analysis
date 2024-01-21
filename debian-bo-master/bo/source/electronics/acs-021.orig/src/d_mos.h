/*$Id: d_mos.h,v 11.30 96/03/17 19:21:51 al Exp $ -*- C++ -*-
 * data structures and defaults for mos model.
 * internal units all mks (meters)
 * but some user input parameters are in cm.
 */
#include "d_diode.h"
#include "e_subckt.h"
#ifndef D_MOS_H
#define D_MOS_H
/*--------------------------------------------------------------------------*/
class DEV_MOS;			/* this file */
class MOS_COMMON;
class MODEL_MOS;
class EVAL_MOS_Ids;
class EVAL_MOS_Gmf;
class EVAL_MOS_Gmr;
class EVAL_MOS_Gds;
class EVAL_MOS_Gmbf;
class EVAL_MOS_Gmbr;
class EVAL_MOS_Cgb;
class EVAL_MOS_Cgd;
class EVAL_MOS_Cgs;
/*--------------------------------------------------------------------------*/
class DEV_ADMITTANCE;		/* external */
class DEV_DIODE;
class DEV_CAPACITANCE;
class DEV_CS;
class DEV_RESISTANCE;
class DEV_VCCS;
enum {mNUM_INIT_COND = 3};
/*--------------------------------------------------------------------------*/
class DEV_MOS : public BASE_SUBCKT {
friend class MODEL_MOS;
friend class EVAL_MOS_Ids;
friend class EVAL_MOS_Gmf;
friend class EVAL_MOS_Gmr;
friend class EVAL_MOS_Gds;
friend class EVAL_MOS_Gmbf;
friend class EVAL_MOS_Gmbr;
friend class EVAL_MOS_Cgb;
friend class EVAL_MOS_Cgd;
friend class EVAL_MOS_Cgs;
public:
	DEV_MOS();
	DEV_MOS(CONST DEV_MOS& p);
	~DEV_MOS(){--Count;}
	CARD*	clone()CONST{return new DEV_MOS(*this);}
	void	parse(CS&);
 	void	print(int,int)const;
	void	expand();
	double	probe_tr_num(const char*)const;
	bool	dotr();
static	int	count(){return Count;}
private:
	void	limit_mos(double,double,double);
private:
   double	ids;		/* iterated parameters, latest		*/
   double	gm;
   double	gds;
   double	gmb;
   double	vgs;		/* terminal voltages			*/
   double	vds;
   double	vbs;
   double	vdsat;		/* saturation voltage			*/
   double	vgst;		/* vgs - von.				*/
   double	von;		/* actual threshold voltage		*/
   bool		reversed;	/* flag: Vgs < 0, reverse s & d		*/
   bool		cutoff;		/* flag: in cut off region		*/
   bool		subthreshold;	/* flag: subthreshold region (L2 only)	*/
   bool		saturated;	/* flag: in saturation region		*/
   bool		sbfwd;		/* flag: sb diode fwd biased		*/
   bool		dbfwd;		/* flag: db diode fwd biased		*/
   bool		punchthru;	/* flag: punch thru region		*/
   node_t	drainnode;	/* internal drain node			*/
   node_t	sourcenode;	/* internal source node			*/
   DEV_RESISTANCE*  Rs;		/* subckt elements, for probe		*/
   DEV_RESISTANCE*  Rd;
   DEV_DIODE*	    Ddb;
   DEV_DIODE*	    Dsb;
   DEV_CAPACITANCE* Cgs;
   DEV_CAPACITANCE* Cgd;
   DEV_CAPACITANCE* Cgb;
   DEV_VCCS*	    Gmbf;
   DEV_VCCS*	    Gmbr;
   DEV_ADMITTANCE*  Yds;
   DEV_VCCS*	    Gmf;
   DEV_VCCS*	    Gmr;
   DEV_CS*	    Ids;
   enum	{NUMNODES = 4};
  static int	Count;
};
/*--------------------------------------------------------------------------*/
class MOS_COMMON : public COMPONENT_COMMON {
public:
	MOS_COMMON();
	MOS_COMMON(const MOS_COMMON& p){*this = p; attachcount=0;}
   double	lo;		/* drawn (optical) channel length	*/
   double	wo;		/* channel width (drawn)		*/
   double	ad_in;		/* drain area, drawn			*/
   double	as_in;		/* source area, drawn			*/
   double	pd;		/* drain perimeter			*/
   double	ps;		/* source perimeter			*/
   double	nrd;		/* drain # squares			*/
   double	nrs;		/* source # squares			*/
   double	ic[mNUM_INIT_COND];	/* initial conditions		*/
   int		off;		/* off flag				*/
   int		icset;		/* flag: initial conditions set		*/
   				/* up to here initialized		*/
   double	le;		/* actual (electrical) channel length	*/
   double	we;		/* actual (electrical) channel width	*/
   double	ad;		/* drain area, actual			*/
   double	as;		/* source area, actual			*/
   double	cgate;		/* gate to channel capacitance		*/
   double	beta;	/*?*/	/* transconductance term (kp*w/l)	*/
   double	relxj;	/*?*/
   double	eta_1;	/*?*/
   double	eta;	/*?*/
   double	eta_2;	/*?*/
   double	idsat;	/*?*/	/* drain junction saturation current	*/
   double	issat;	/*?*/	/* source junction saturation current	*/
   double	rd;	/*?*/	/* ohmic drain resistance		*/
   double	rs;	/*?*/	/* ohmic source resistance		*/
   double	sigma;	/*?*/	/* lev 3				*/
			/*?*//* indicates calculated from other params	*/
   DIODE_COMMON sb;
   DIODE_COMMON db;
};
/*--------------------------------------------------------------------------*/
class MODEL_MOS : public MODEL_DIODE {
friend class DEV_MOS;
friend class EVAL_MOS_Ids;
friend class EVAL_MOS_Gmf;
friend class EVAL_MOS_Gmr;
friend class EVAL_MOS_Gds;
friend class EVAL_MOS_Gmbf;
friend class EVAL_MOS_Gmbr;
friend class EVAL_MOS_Cgb;
friend class EVAL_MOS_Cgd;
friend class EVAL_MOS_Cgs;
public:
	MODEL_MOS(const char *name = "");
	MODEL_MOS(const MODEL_MOS&){assert(0);}
	CARD*	clone()CONST{return new MODEL_MOS(*this);}
	void	parse(CS&);
 	void	print(int,int)const;
	void	tr_eval(COMPONENT*)const;
private:
	void	eval_mos1(DEV_MOS*)const;
	void	eval_mos2(DEV_MOS*)const;
	void	eval_mos3(DEV_MOS*)const;
private:			/* input parameters */
  double	vto;		/* zero-bias threshold voltage */
  double	kp;		/* transconductance parameter */
  double	gamma;		/* bulk threshold parameter */
  double	phi;		/* surface potential */
  double	lambda;		/* channel-length modulation */
  double	rd;		/* drain ohmic resistance */
  double	rs;		/* source ohmic resistance */
  double	cbd;		/* 0-bias BD jct capacitance */
  double	cbs;		/* 0-bias BS jct capacitance */
  double	is;		/* bulk jct sat current */
  double	cgso;		/* GS overlap capacitance */
  double	cgdo;		/* GD overlap capacitance */
  double	cgbo;		/* GB overlap capacitance */
  double	rsh;		/* D & S diffusion sheet resistance */
  double	js;		/* bulk jct sat current per sq meter */
  double	tox;		/* oxide thickness */
  double	nsub;		/* substrate doping */
  double	nss;		/* surface state density */
  double	nfs;		/* fast surface state density */
  double	xj;		/* metallurgical junction depth */
  double	ld;		/* lateral diffusion */
  double	uo;		/* surface mobility */
  double	ucrit;		/* critical field mobility degradation */
  double	uexp;		/* critical field exponent in mob.deg. (mos2) */
  double	utra;		/* transverse field coefficient (not used) */
  double	vmax;		/* max drift velocity of carriers */
  double	neff;		/* total channel charge coefficient */
  double	kf;		/* flicker noise coefficient */
  double	af;		/* flicker noise exponent */
  double	delta;		/* width effect on threshold voltage */
  double	theta;		/* mobility modulation (mos3, s.s. vbp) */
  double	eta;		/* static feedback (mos3, spice shares uexp) */
  double	kappa;		/* saturation field vector (mos3, s.s. utra) */
  int		level;		/* which model to use */
  enum gate_t {gOPP = -1, gMETAL = 0, gSAME = 1};
  gate_t	tpg;		/* type of gate material */

  enum polarity_t {pP = -1, pN = 1};
  polarity_t	polarity;	/* N or P channel */

				/* calculated parameters */
  double	alpha;		/* ((2. * E_SI / Q) / nsub) */
  double	xd;		/* sqrt(alpha) */
  double	cox;		/* oxide capacitance (E_OX / tox) */
  double	vfb;		/* flat-band voltage */
  double	vbi;		/* "built-in" voltage (vfb + phi) */
  double	xwb;		/* xd * sqrt(pb) */
  double	vbp;		/* ucrit * E_SI / cox (mos2) */
  double	cfsox;		/* Q * nfs / cox */
  double	delta3;		/* level 3 version of delta */
  double	sqrt_phi;	/* sqrt(phi) */
  double	phi_sqrt_phi;	/* phi * sqrt(phi) or pow(phi,1.5) */
  struct {
    unsigned	vto:1,		/* flags: set if the parameter has been */
		kp:1,		/* calculated as opposed to entered, so */
		gamma:1,	/* it will be re-calculated as needed.  */
		phi:1,
		cj:1;
  } calc;
};
/*--------------------------------------------------------------------------*/
class EVAL_MOS_Ids : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
class EVAL_MOS_Gmf : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
class EVAL_MOS_Gmr : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
class EVAL_MOS_Gds : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
class EVAL_MOS_Gmbf : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
class EVAL_MOS_Gmbr : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
class EVAL_MOS_Cgb : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
class EVAL_MOS_Cgd : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
class EVAL_MOS_Cgs : public COMPONENT_COMMON {
public:
  	void	tr_eval(COMPONENT*d)const;
	bool	has_tr_eval()const	{return true;}
	bool	has_ac_eval()const	{return false;}
};
/*--------------------------------------------------------------------------*/
/* dimension conversions.
 * All internal distances are in meters.
 * In some cases the user data is in other units
 */
#define CM2M	(1e-2)			/*	centimeters to meters 	 */
#define CM2M2	(1e-4)			/*          ...........  squared */
#define ICM2M	(1e2)			/* inverse  ...........    	 */
#define ICM2M2	(1e4)			/* inverse  ...........  squared */
#define ICM2M3	(1e6)			/* inverse  ...........  cubed   */
#define MICRON2METER	(1e-6)		/*	microns to meters	 */
/*--------------------------------------------------------------------------*/
#define NI	(1.45e16)		/* intrinsic carrier concentration */

#define nDRAIN	n[0]	/* nodes */
#define	nGATE	n[1]
#define nSOURCE	n[2]
#define	nBULK	n[3]

#define mDEFAULT_is		1e-14	/* last resort defaults */
					/* in case can't calculate */
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
