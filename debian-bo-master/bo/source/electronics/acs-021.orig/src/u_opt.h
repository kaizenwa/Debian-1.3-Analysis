/*$Id: u_opt.h,v 11.37 96/03/24 10:10:29 al Exp $ -*- C++ -*-
 * all the options set by the .options card.
 */
#include "md.h"
#include "mode.h"
#ifndef U_OPT_H
#define U_OPT_H
/*--------------------------------------------------------------------------*/
class CS;
enum method_t {mUNKNOWN=0, mGEAR, mTRAPEZOID, mSTIFF};
enum bypass_t {bNO=0, bYES, bVOLT};
enum {dsINIT=001, dsRANGE=002, dsDEVLIMIT=004, dsDEVREGION=010, dsREVERSE=020};
/*--------------------------------------------------------------------------*/
class OPT {
public:
  void command(CS& cmd);
private:
  bool set(CS& cmd);
  void print(int where);
public:
  enum {DCBIAS=1, DCXFER=2, TRLOW=3, TRHIGH=4, TRTOTAL=5, SSTEP=6,
	WCASE=7, TRACE=8, ITL_COUNT=9};
  enum order_t {oREVERSE=1, oFORWARD, oAUTO};
public:
  static bool acct;	    // flag: print accounting info
  static bool list;	    // flag: print listing
  static bool nomod;	    // flag: suppress model printout
  static bool page;	    // flag: do page ejects
  static bool node;	    // flag: print node table
  static bool opts;	    // flag: print options
  static double gmin;	    // minimum conductance allowed
  static double reltol;	    // relative error tolerance
  static double abstol;	    // absolute current error tolerance
  static double vntol;	    // absolute voltage error tolerance
  static double trtol;	    // transient error overestimation factor
  static double chgtol;	    // charge tolerance
  static double pivtol;	    // minimum acceptable pivot
  static double pivrel;	    // max to min ratio in a column?
  static int numdgt;	    // number of digits to display
  static double tnom;	    // nominal termperature
  static int cptime;	    // max allowed cpu time (seconds)
  static int limtim;	    // amt of time to reserve for plots
  static int limpts;	    // max points to print
  static int lvlcod;	    // enum: if == 2, solve fast
  static int lvltim;	    // enum: how to control time step
  static method_t method;   // enum: integration method
  static int maxord;	    // max order of integration
  static double defl;	    // MOS default channel length
  static double defw;	    // MOS default channel width
  static double defad;	    // MOS default drain diffusion area
  static double defas;	    // MOS default source diffusion area
  
  static int seed;	    // random number seed
  static double wczero;	    // worst case zero window
  static double dampmax;    // Newton-Raphson damping coefficient max
  static double dampmin;    // Newton-Raphson damping coefficient min
  static int dampstrategy;  // bit flags, damping strategy options
  static double floor;	    // print as 0 if less than this
  static double tempamb;    // ambient temperature
  static double shortckt;   // short resistance
  static int picky;	    // error picky-ness
  static int inwidth;	    // width of input devices
  static int outwidth;	    // width of output devices
  static double xdivisions; // plot divisions, x axis
  static double ydivisions; // plot divisions, y axis
  static order_t order;	    // ordering method
  static smode_t mode;	    // mixed-mode mode preference
  static int transits;	    // number of good transitions for digital
  static bool dupcheck;	    // check for duplicates on read
  static bypass_t bypass;   // bypass model evaluation, if appropriate
  static bool incmode;	    // make incremental changes to the matrix
  static bool lubypass;	    // bypass parts of LU decomposition, if appropriate
  static bool fbbypass;	    // bypass fwd & back sub when last iter converged
  static bool traceload;    // load only elements that need it, using queue
  static int itermin;	    // forced min iteration count.
  static double limit;	    // diff voltage limit for nonlinear calculations
  static double vmax;	    // + voltage limit for nonlinear calculations
  static double vmin;	    // - voltage limit for nonlinear calculations
  static double dtmin;	    // smallest internal step in transient analysis
  static double dtratio;    // ratio of max / min dt in transient analysis
  static bool rstray;	    // include stray resistors in models
  static bool cstray;	    // include stray capacitors in models
  static int harmonics;	    // number of harmonics in fourier analysis
  static double trstepgrow; // limit of step size growth in transient analysis
  static double trstepshrink;// amt to shrink step size on convergence failure
  static double trreject;   // how bad trunc error has to be to reject a step
  static bool showall;	    // flag: show development flags
  static int foooo;	    // a reusable value to aid development
  static int diodeflags;    // convergence heuristic flags for diode
  static int mosflags;	    // convergence heuristic flags for mosfet
  
  static double lowlim;	    // 1 - reltol
  static double uplim;	    // 1 + reltol
  
  static int itl[ITL_COUNT];// 1=dc (bias) iteration limit
			    // 2=dc transfer iteration limit
			    // 3=lower transient iteration limit
			    // 4=upper transient iteration limit
			    // 5=transient total iterations allowed
			    // 6=source stepping iteration limit
			    // 7=worst case iteration limit
			    // 8=trace nonconvergence start iteration
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
