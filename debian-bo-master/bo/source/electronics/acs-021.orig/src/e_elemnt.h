/*$Id: e_elemnt.h,v 11.30 96/03/17 19:22:00 al Exp $ -*- C++ -*-
 * branch structure type definitions
 * device types (enumeration type?)
 */
#include "e_compon.h"
#include "e_node.h"	// for setneedslu
#ifndef E_ELEMNT_H
#define E_ELEMNT_H
/*--------------------------------------------------------------------------*/
class ELEMENT : public COMPONENT {
  friend class DEV_MUTUAL_L;
  friend class DEV_CCCS;
  friend class DEV_CCVS;
protected:
	ELEMENT(){loaditer=0;loss=0.;}
	ELEMENT(CONST ELEMENT& p):COMPONENT(p)	{loaditer=0;loss=p.loss;}
	void	setneedslu(int b)		{aa.setchanged(n[b].m);}
	void	store_values()	 		{y2 = y1; y1 = y0;}
	void	reject_values() 		{y0 = y1;}

public:					// e_elem2.cc
	void	parse(CS&);
	void	print(int,int)const;
protected:
	void	parseexpr(CS&);
	void	printexpr(int)const;

private:				// e_elem1.cc
	double	dampdiff(double&, const double&, bool);
	void	il_trload_source();
protected:
	void	trload_loss();
	void	trunload_loss();
	void	trload_source();
	void	trunload_source();
	void	acload_source();
	void	acload_loss();
	void	trload_passive();
	void	trunload_passive();
	void	acload_passive();
	void	trload_active();
	void	trunload_active();
	void	acload_active();

protected:				// e_elem3.cc
	double   tr_amps()const;
	double   tr_outvolts()const;
	double   tr_involts()const;
	COMPLEX  ac_outvolts()const;
	COMPLEX  ac_involts()const;
	double   probe_tr_num(const char*)const;
	xprobe_t probe_ac_ext(const char*)const;
private:
  int      loaditer;	/* load iteration number		*/
protected:
  cpoly1_t m1;		/* matrix parameters, 1 fill ago	*/
  cpoly1_t m0;		/* matrix parameters, new		*/
  double   loss;	/* fixed shunt conductance (should be COMMON)	*/
  COMPLEX  acg;		/* ac admittance matrix values   	*/
//COMPLEX  ev;		/* ac effective value (usually real)	*/
  enum	{DEFAULT_NUMNODES = 2};
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
