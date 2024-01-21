/*$Id: s__.h,v 11.37 96/03/24 10:09:28 al Exp $ -*- C++ -*-
 * base class for simulation methods
 */
#include "md.h"
#include "u_prblst.h"
#include "e_base.h"
#include "l_astack.h"
#ifndef S___H
#define S___H
/*--------------------------------------------------------------------------*/
class CARD;
class CS;
enum worst_t
   {wNONE, wWORST, wSENS, wRAND, wMAXDC, wMINDC, wMAXAC, wMINAC, wLEAD, wLAG};
/*--------------------------------------------------------------------------*/
class SIM : public CKT_BASE {
friend class NODE;
friend class node_t;
public:
 static	double freq;		/* AC frequency to analyze at (Hertz) */
 static	COMPLEX jomega;		/* AC frequency to analyze at (radians) */
 static double time0;		/* time now */
 static double time1;		/* time at previous time step */
 static double dtmin;		/* min internal step size */
 static double temp;		/* ambient temperature */
 static double damp;		/* Newton-Raphson damping coefficient actual */
 static bool uic;		/* flag: use initial conditions (spice-like) */
 static bool bypass_ok;		/* flag: ok to bypass model evaluation */
 static int inc_mode;		/* flag: make incremental changes (3 state) */
 static bool fulldamp;		/* flag: big iter. jump. use full (min) damp */
 static int mode;		/* simulation type (AC, DC, ...) */
 static sim_phase_t phase;	/* phase of simulation (iter, init-dc,) */
 static bool freezetime;	/* flag: don't advance stored time */
 static double genout;		/* tr dc input to circuit (generator) */
 static ASTACK<CARD*> loadq;	/* pipeline of elements to load to matrix */
protected: 
 static double last_time;	/* time at which "volts" is valid */
 static	int	*nm;		/* node map (external to internal)	*/
 static	double	*i;		/* dc-tran current (i) vector		*/
 static	double	*v0;		/* dc-tran voltage, new			*/
 static	double	*vi1;		/* dc-tran voltage, 1 iter ago		*/
 static	double	*vt1;		/* dc-tran voltage, 1 time ago		*/
 static	double	*fw;		/* dc-tran fwd sub intermediate values	*/
 static	double	*vdc;		/* saved dc voltages			*/
 static	COMPLEX	*ac;		/* ac right side			*/
 
public:
	int stepno;		/* count of visible (saved) steps */
	PROBELIST alarmlist;	/* list of alarm probes */
	PROBELIST plotlist;	/* list of plot probes */
	PROBELIST printlist;	/* list of print probes */
	PROBELIST storelist;	/* list of probes to store for postproc */

private:
 virtual void	setup(CS&) = 0;
 virtual void	sweep() = 0;
 virtual void	finish(){}

 /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
public:					/* s__init.cc */
 static	void	init();
 static	void	uninit();
 /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
protected:				/* s__aux.cc */
	void	keep();
 /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
  	void	command_base(CS&);	/* s__init.cc */
	void	reset_timers();	
 static	void	alloc_hold_vectors();
 static	void	determine_matrix_structure(const CARD*);
	void	alloc_vectors();
 static	void	unalloc_vectors();
 static	void	count_nodes();
 /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
 static	void	map_nodes();		/* s__map.cc */
 static void	order_reverse();
 static void	order_forward();
 static void	order_auto();
 static void	map_list(CARD*);
 /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
	void	restore();		/* s__out.cc */
	void	out(double);
	void	head(double,double, bool,const char*);
	void	print(double);
	void	alarm();
virtual	void	store();
 /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */ 
protected:				/* s__solve.cc */
	bool	solve(int,int);
private:
	void	advance_time();
	void	count_iterations();
	void	set_flags();
	void	clear_arrays();
	void	evaluate_models();
	void	set_damp();
	void	load_matrix();
	void	solve_equations();
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
