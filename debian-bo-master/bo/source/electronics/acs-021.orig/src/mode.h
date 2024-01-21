/*$Id: mode.h,v 11.22 96/02/18 11:46:39 al Exp $ -*- C++ -*-
 * several enumerated types to identify various modes
 */
#ifndef MODE_H
#define MODE_H
/*--------------------------------------------------------------------------*/
enum smode_t   {mANALOG=1, mDIGITAL, mMIXED};

/* sim_mode   simulation types						*/
#define sNONE	   (0)	/* not doing anything, reset by cmd interpreter	*/
#define sSTATUS	   (1)	/* status command				*/
#define sSTART	   (3)	/* start of simulation modes (exclude status)	*/
#define sAC	   (3)	/* AC analysis					*/
#define sOP	   (4)	/* op command					*/
#define	sDC	   (5)	/* dc sweep command				*/
#define	sTRAN	   (6)	/* transient command				*/
#define	sFOURIER   (7)	/* fourier command				*/
#define sCOUNT	   (8)	/* number of simulation types			*/

/* iter probes (mimic sim_mode, sort of)				*/
#define iPRINTSTEP (0)	/* iterations for this printed step		*/
#define iSTEP	   (1)	/* iterations this internal step		*/
#define iTOTAL	   (2)	/* total iterations since startup		*/
#define iCOUNT	sCOUNT	/* number of iteration counters			*/

/* control probes							*/
#define cSTEPCAUSE (0)	/* what caused this time step			*/
#define cSTEPS	   (1)	/* count of hidden steps (+ this unhidden)	*/
#define cCOUNT	   (2)	/* number of control probes			*/

/* sim_phase  which of the many steps...			*/
enum sim_phase_t {
  pNONE,	/* not doing anything, reset by cmd interpreter */
  pINIT_DC,	/* initial DC analysis				*/
  pTRAN,	/* transient, in progress			*/
  pCOUNT	/* number of possible simulation phases		*/
};

/* run_mode   what to do with dot cards on reading		*/
enum run_mode_t {
  rNONE,	/* do nothing??					*/
  rEXECUTE,	/* execute now, as a command			*/
  rPRESET,	/* do set up commands now, but not simulation	*/
		/* store parameters, so bare invocation of a	*/
		/* simulation command will do it this way.	*/
  r2PASS,	/* like preset, but then do it all on "end"	*/
  rIGNORE,	/* treat as comments				*/
  rCOUNT	/* number of things you can do with dot cards	*/
};

/* causes of this step (values for control[cSTEPCAUSE])			*/
#define scUSER	   (1)	/* user requested				*/
#define scEVENTQ   (2)	/* an "event" from the queue			*/
#define scSKIP	   (3)	/* effect of "skip" parameter			*/
#define	scITER_R   (4)	/* iter count exceeds itl4			*/
#define scITER_A   (5)	/* iter count exceeds itl3			*/
#define scTE	   (6)	/* truncation error, or device stuff		*/
#define scRDT	   (7)	/* by iter count limited by 2*rdt		*/
#define	scADT	   (8)	/* by iter count limited by max(rdt, 2*adt)	*/
#define scHALF	   (9)	/* limited by half time to exact time		*/
#define scREJECT  (10)	/* rejected previous time step			*/
#define scZERO	  (20)	/* fixed zero time step				*/
#define scSMALL	  (30)	/* time step too small				*/
#define scNO_ADVANCE (100) /* after all that it still didn't advance	*/

/* trace modes -- how much diagnostics to show 				*/
enum trace_t {
  tNONE	     = 0,	/* no extended diagnostics			*/
  tUNDER     = 1,	/* show underlying analysis, important pts only	*/
  tALLTIME   = 2,	/* show every time step, including hidden 	*/
  tREJECTED  = 3,	/* show rejected time steps			*/
  tITERATION = 4,	/* show every iteration, including nonconverged	*/
  tVERBOSE   = 5	/* show extended diagnostics			*/
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
