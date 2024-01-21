/*$Id: e_exp_tr.cc,v 11.32 96/03/21 00:48:51 al Exp $ -*- C++ -*-
 * Takes care of nonlinearities, behavioral modeling, etc
 * 	in transient and dc analysis.
 */
#include "e_compon.h"
#include "error.h"
#include "e_exp.h"
/*--------------------------------------------------------------------------*/
	void	trfix(COMPONENT*);
static	void	trf_ac(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_dc(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_dctran(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_frequency(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_period(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_ramp(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_time(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_tran(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_bandwidth(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_complex(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_cornerdown(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_cornerup(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_delay(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_exp(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_expterm(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_generator(const COMPONENT*,double**,cpoly1_t*);
/*static*/ void	trf_max(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_netfunc(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_notch(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_numeric(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_offset(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_polar(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_polyterm(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_pulse(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_pwl(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_sffm(const COMPONENT*,double**,cpoly1_t*);
static	void	trf_sin(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_tanh(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_ic(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_ii(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_iv(const COMPONENT*,double**,cpoly1_t*);	
static	void	trf_tempco(const COMPONENT*,double**,cpoly1_t*);
/*--------------------------------------------------------------------------*/
#define arg0 (arg[0][0])
#define arg1 (arg[0][1])
#define arg2 (arg[0][2])
#define arg3 (arg[0][3])
#define arg4 (arg[0][4])
#define arg5 (arg[0][5])
#define arg6 (arg[0][6])
#define arg7 (arg[0][7])

static bool skip;
static double periodtime;	/* time since beginning of period	    */
static double reltime;		/* time since event			    */
static double timenow;		/* time at this here sim point		    */
/*--------------------------------------------------------------------------*/
void trfix(COMPONENT* brh)
{
  cpoly1_t y;			/* return value				    */
  				/* y.c1 is the slope, y.c0 is y intercept   */
  skip = false;
  timenow = periodtime = reltime = SIM::time0;
  y.x  = brh->y0.x;
  y.f1 = brh->val;
  y.c0 = 0.;
  
  if (brh->x){
    struct expr *x;
    double *arg;
    int *key;
    
    x = (struct expr*)brh->x;
    arg = x->args->args;
    for (key = x->keys->args;  *key;  key++){
      switch (*key){
	case eAC:		trf_ac(brh,&arg,&y);		break;
	case eDC:		trf_dc(brh,&arg,&y);		break;
	case eDCTRAN:		trf_dctran(brh,&arg,&y);	break;
	case eFREQUENCY:	trf_frequency(brh,&arg,&y);	break;
	case ePERIOD:		trf_period(brh,&arg,&y);	break;
	case eRAMP:		trf_ramp(brh,&arg,&y);		break;
	case eTIME:		trf_time(brh,&arg,&y);		break;
	case eTRAN:		trf_tran(brh,&arg,&y);		break;

	case eBANDWIDTH:	trf_bandwidth(brh,&arg,&y);	break;
	case eCOMPLEX:		trf_complex(brh,&arg,&y);	break;
	case eCORNERDOWN:	trf_cornerdown(brh,&arg,&y);	break;
	case eCORNERUP:		trf_cornerup(brh,&arg,&y);	break;
	case eDELAY:		trf_delay(brh,&arg,&y);		break;
	case eEXP:		trf_exp(brh,&arg,&y);		break;
	case eEXPTERM:		trf_expterm(brh,&arg,&y);	break;
	case eGENERATOR:	trf_generator(brh,&arg,&y);	break;
	case eMAX:		trf_max(brh,&arg,&y);		break;
	case eNETFUNC:		trf_netfunc(brh,&arg,&y);	break;
	case eNOTCH:		trf_notch(brh,&arg,&y);		break;
	case eNUMERIC:		trf_numeric(brh,&arg,&y);	break;
	case eOFFSET:		trf_offset(brh,&arg,&y);	break;
	case ePOLAR:		trf_polar(brh,&arg,&y);		break;
	case ePOLYTERM:		trf_polyterm(brh,&arg,&y);	break;
	case ePULSE:		trf_pulse(brh,&arg,&y);		break;
	case ePWL:		trf_pwl(brh,&arg,&y);		break;
	case eSFFM:		trf_sffm(brh,&arg,&y);		break;
	case eSIN:		trf_sin(brh,&arg,&y);		break;
	case eTANH:		trf_tanh(brh,&arg,&y);		break;

	case eIC:		trf_ic(brh,&arg,&y);		break;
	case eII:		trf_ii(brh,&arg,&y);		break;
	case eIV:		trf_iv(brh,&arg,&y);		break;
	case eTEMPCO:		trf_tempco(brh,&arg,&y);	break;

	default:  error(bWARNING, "%s: undefined function: %d\n",
			brh->printlabel(), *key); break;
      }
    }
  }
  brh->y0.x  = y.x;
  brh->y0.f0 = y.c0 + y.x * y.f1;
  brh->y0.f1 = y.f1;
}
/*--------------------------------------------------------------------------*/
/* trf_ac:  keyword: ac
 * following args for ac analysis only
 * here : skip them
 * no args
 */
/*ARGSUSED*/
static void trf_ac(const COMPONENT* /*brh*/, double **arg, cpoly1_t* /*y*/)
{
  skip = true;
  *arg += aAC;
}
/*--------------------------------------------------------------------------*/
/* trf_dc:  keyword: dc
 * following args for dc and transient analysis
 * no args
 */
/*ARGSUSED*/
static void trf_dc(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  y->f1 = y->c0 = 0.;
  skip = false;
  *arg += aDC;
}
/*--------------------------------------------------------------------------*/
/* trf_dctran:  keyword: dctran
 * following args for dc and transient analysis
 * this does apply to initial conditions.
 * no args
 */
/*ARGSUSED*/
static void trf_dctran(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  y->f1 = y->c0 = 0.;
  skip = false;
  *arg += aDCTRAN;
}
/*--------------------------------------------------------------------------*/
/* trf_frequency:  keyword: frequency
 * the value is frequency dependent (ac only)
 * works only in ac analysis, otherwise could be non-causal.
 * arg0 = test freq
 */
/*ARGSUSED*/
static void trf_frequency(const COMPONENT* /*brh*/, double **arg, cpoly1_t* /*y*/)
{
  skip = true;
  *arg += aFREQUENCY;
}
/*--------------------------------------------------------------------------*/
/* trf_period:  keyword: period
 * the component is periodic in time
 * periodtime and reltime are reset every period
 * all time dependencies are based on reltime and periodtime
 * for sensible behavior, this key should be first.
 * implies that following args for transient analysis only.
 * arg0 = period
 */
/*ARGSUSED*/
static void trf_period(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (timenow >= 0.){ /* if transient analysis */
    y->f1 = y->c0 = 0.;
    skip = false;
    periodtime = reltime = (arg0 == 0.) ? timenow : fmod(timenow, arg0);
  }else{
    skip = true;
  }
  *arg += aPERIOD;
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void trf_ramp(const COMPONENT* brh, double **arg, cpoly1_t* /*y*/)
{
  error(bWARNING,"ramp not implemented: %s\n", brh->printlabel());
  *arg += aRAMP;
}
/*--------------------------------------------------------------------------*/
/* trf_time:  keyword: time
 * following args take effect at the given time (switch)
 * obviously, transient only.
 * arg0 = switch time
 */
/*ARGSUSED*/
static void trf_time(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (periodtime >= arg0){
    y->f1 = y->c0 = 0.;
    reltime = periodtime - arg0;
    skip = false;
  }else{
    skip = true;
  }
  *arg += aTIME;
}
/*--------------------------------------------------------------------------*/
/* trf_tran:  keyword: transient
 * following args for transient analysis only.
 * applies to the dc analysis that computes initial conditions,
 * but not the steady state dc analysis.
 * no args
 */
/*ARGSUSED*/
static void trf_tran(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (timenow >= 0.){ /* if transient analysis */
    y->f1 = y->c0 = 0.;
    skip = false;
  }else{
    skip = true;
  }
  *arg += aTRAN;
}
/*--------------------------------------------------------------------------*/
/* trf_bandwidth:  function: bandwidth
 * gain block, with a bandwidth. (ac only)
 * bad design: should be keyword instead.
 * not implemented in transient analysis
 * but could be as R-C or similar
 * arg0 = dc gain
 * arg1 = 3 db freq.
 */
static void trf_bandwidth(const COMPONENT* brh, double **arg, cpoly1_t* y)
{
  if (!skip){
    y->f1 += arg0;
    if (timenow > 0.)
      error(bPICKY,"%s: bandwidth not supported in transient analysis\n",
	    brh->printlabel());
  }
  *arg += aBANDWIDTH;
}
/*--------------------------------------------------------------------------*/
/* trf_complex:  function: complex
 * complex value, cartesian coordinates, in frequency domain (ac only)
 * not possible in transient or dc because would be non-causal
 * arg0 = real part
 * arg1 = imaginary part
 */
static void trf_complex(const COMPONENT* brh, double **arg, cpoly1_t* y)
{
  if (!skip){
    y->f1 += arg0;
    error(bPICKY, "non-causal circuit: %s\n", brh->printlabel());
  }
  *arg += aCOMPLEX;
}
/*--------------------------------------------------------------------------*/
/* trf_cornerdown:  function: cornerdown
 * piecewise linear function:
 * output is 0 for in >= arg1
 * derivative is arg0 for in <= arg1
 * arg0 = active slope
 * arg1 = break point
 */
static void trf_cornerdown(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip  &&  y->x < arg1){
    y->f1 += arg0;
    y->c0 -= arg1 * arg0;
  }
  *arg += aCORNERDOWN;
}
/*--------------------------------------------------------------------------*/
/* trf_cornerup:  function: cornerup
 * piecewise linear function:
 * output is 0 for in <= arg1
 * derivative is arg0 for in >= arg1
 * arg0 = active slope
 * arg1 = break point
 */
static void trf_cornerup(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip  &&  y->x > arg1){
    y->f1 += arg0;
    y->c0 -= arg1 * arg0;
  }
  *arg += aCORNERUP;
}
/*--------------------------------------------------------------------------*/
/* trf_delay:  function: delay
 * time delay (ac only)
 * not implemented, needs storage of past time values
 * arg0 = magnitude
 * arg1 = delay
 */
static void trf_delay(const COMPONENT* brh, double **arg, cpoly1_t* y)
{
  if (!skip){
    y->f1 += arg0;
    error(bPICKY,"%s: delay not supported in transient or analysis\n",
	  brh->printlabel());
  }
  *arg += aDELAY;
}
/*--------------------------------------------------------------------------*/
/* trf_exp:  spice compatible function: exp
 * spice source exponential function: exponential function of time
 * known bugs: defaults wrong: could divide by zero
 * arg0 = initial value
 * arg1 = pulsed value
 * arg2 = rise delay
 * arg3 = rise time const
 * arg4 = fall delay
 * arg5 = fall time const
 */
/*ARGSUSED*/
static void trf_exp(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip){
    y->f1 += arg0;
    if (reltime > arg2)
      y->f1 += (arg1 - arg0) * (1. - Exp(-(reltime-arg2)/arg3));
    if (reltime > arg4)
      y->f1 += (arg0 - arg1) * (1. - Exp(-(reltime-arg4)/arg5));
  }
  *arg += aEXP;
}
/*--------------------------------------------------------------------------*/
/* trf_expterm:  function: expterm
 * exponent term, non-integer power term (not exponential)
 * like polyterm, but for non-integers
 * only works for positive input, because negative number raised to
 * non-integer power is usually complex.
 * arg0 = coefficient
 * arg1 = exponent
 */
static void trf_expterm(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip  &&  y->x > 0.){
    double coeff;
    coeff = arg0 * pow(y->x,arg1-1);
    y->f1 += coeff * arg1;
    y->c0 += coeff * y->x * (1-arg1);
  }
  *arg += aEXPTERM;
}
/*--------------------------------------------------------------------------*/
/* trf_generator:  function: generator
 * value is derived from the "signal generator" (generator command)
 * intended for fixed sources, as circuit input.
 * component specific period determines only whether to use this or not
 * it does not actually affect the operation of the signal generator
 * arg0 = scale factor
 */
/*ARGSUSED*/
static void trf_generator(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip)
    y->f1 += arg0 * SIM::genout;
  *arg += aGENERATOR;
}
/*--------------------------------------------------------------------------*/
/* trf_max:  function: max
 * piecewise linear function: block that clips
 * bad design: should be keyword, so it can apply to the total part
 *	       should be able to specify upper and lower limits
 * arg0 = normal value
 * arg1 = output clip pt
 */
/*static*/ void trf_max(const COMPONENT* brh, double **arg, cpoly1_t* y)
{
  if (!skip  &&  arg0 != 0.){
    double clip_input;
    clip_input = arg1/arg0;
    if (fabs(y->x-brh->y1.x) < fabs(y->x-brh->y2.x)*10.){	/* normal */
      if (y->x < -clip_input)					/* -clip */
	y->c0 -= arg1;
      else if (y->x > clip_input)				/* +clip */
	y->c0 += arg1;
      else							/* linear */
	y->f1 += arg0;
    }else{					/* oscillating */
						/* don't skip middle region */
      if (y->x < -clip_input  &&  brh->y1.x < clip_input)
	y->c0 -= arg1;
      else if (y->x > clip_input  &&  brh->y1.x > -clip_input)
	y->c0 += arg1;
      else
	y->f1 += arg0;
    }
  }
  *arg += aMAX;
}
/*--------------------------------------------------------------------------*/
/* trf_netfunc:  function: netfunction
 * gain block, nth order response. (ac only)
 * not implemented in transient analysis
 * but could be as R-L-C or similar
 * arg0 = arg count
 * arg1 = dc gain
 * arg3 = order of denom
 * arg* = coefs of denom
 * arg* = coefs of numer
 * arg0 = arg count
 */
static void trf_netfunc(const COMPONENT* brh, double **arg, cpoly1_t* y)
{
  int argcount;
  argcount = (int)arg0;
  
  if (!skip){
    y->f1 += arg1;
    if (timenow > 0.)
      error(bPICKY,"%s: netfunction not supported in transient analysis\n",
	    brh->printlabel());
  }
  *arg += argcount;
}
/*--------------------------------------------------------------------------*/
/* trf_notch:  function: notch
 * piecewise linear function: crossover notch, dead zone
 * symmetric around zero
 * arg0 = normal value
 * arg1 = dead zone size
 */
static void trf_notch(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip){
    if (y->x > arg1){
      y->f1 += arg0;
      y->c0 -= arg1 * arg0;
    }else if (y->x < -arg1){
      y->f1 += arg0;
      y->c0 += arg1 * arg0;
    }
  }
  *arg += aNOTCH;
}
/*--------------------------------------------------------------------------*/
/* trf_numeric:  simple numeric argument
 * arg0 = value
 */
/*ARGSUSED*/
static void trf_numeric(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip)
    y->f1 += arg0;
  *arg += aNUMERIC;
}
/*--------------------------------------------------------------------------*/
/* trf_offset:  function: offset
 * fixed dc offset
 * bad design: should be keyword
 * arg0 = gain
 * arg1 = output offset
 */
/*ARGSUSED*/
static void trf_offset(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip){
    y->f1 += arg0;
    y->c0 += arg1 * arg0;
  }
  *arg += aOFFSET;
}
/*--------------------------------------------------------------------------*/
/* trf_polar:  function: polar
 * complex value in polar coordinates, in frequency domain (ac only)
 * not possible in transient or dc because would be non-causal
 * arg0 = magnitude
 * arg1 = phase
 */
static void trf_polar(const COMPONENT* brh, double **arg, cpoly1_t* y)
{
  if (!skip){
    y->f1 += arg0;
    error(bPICKY, "non-causal circuit: %s\n", brh->printlabel());
  }
  *arg += aPOLAR;
}
/*--------------------------------------------------------------------------*/
/* trf_polyterm:  function: polyterm
 * polynomial term, one term in a polynomial
 * power must be integer, is rounded to nearest integer
 * caution: will divide by zero with zero input and negative exponent
 * arg0 = coefficient
 * arg1 = exponent
 */
static void trf_polyterm(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip){
    int expo;
    expo = (int)floor(arg1+.5);
    if (expo == 0){
      y->c0 += arg0;
    }else{ /* (expo != 0) */
      double coeff;
      coeff = arg0 * pow(y->x,expo-1);
      y->f1 += coeff * expo;
      y->c0 += coeff * y->x * (1-expo);
    }
  }
  *arg += aPOLYTERM;
}
/*--------------------------------------------------------------------------*/
/* trf_pulse: spice compatible function: pulse
 * spice pulse function (for sources)
 * arg0 = initial value
 * arg1 = pulsed value
 * arg2 = delay time
 * arg3 = rise time
 * arg4 = fall time
 * arg5 = pulse width
 * arg6 = period
 */
/*ARGSUSED*/
static void trf_pulse(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip){
    double pw   = (arg5 == 0.) ? reltime : arg5;
    double time = reltime;
    if (arg6 != 0.){
      while (time >= arg2+arg6){
	time -= arg6;
      }
    }
    
    if (time >= arg2+arg3+pw+arg4){		/* past pulse	*/
      y->f1 += arg0;
    }else if (time >= arg2+arg3+pw){		/* falling 	*/
      double interp = (time - (arg2+arg3+pw)) / arg4;
      y->f1 += arg1 + interp * (arg0 - arg1);
    }else if (time >= arg2+arg3){		/* pulse val 	*/
      y->f1 += arg1;
    }else if (time >= arg2){			/* rising 	*/
      double interp = (time - arg2) / arg3;
      y->f1 += arg0 + interp * (arg1 - arg0);
    }else{					/* init val	*/
      y->f1 += arg0;
    }
  }
  *arg += aPULSE;
}
/*--------------------------------------------------------------------------*/
/* trf_pwl:  spice compatible function: pwl
 * "piece-wise linear" point-plotting function of time.
 * BUG: different from SPICE when first point is not zero
 * arg0 = arg count
 * arg1 = time
 * arg2 = value
 * etc.
 */
/*ARGSUSED*/
static void trf_pwl(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  int argcount = (int)(arg[0][0]);

  if (!skip){
    double lowtime = 0.;
    double lowvalue = arg[0][2];
    int i;
    for (i = 1;
    	 i < argcount  &&  reltime > arg[0][i]  &&  lowtime <= arg[0][i];
	 i += 2){
      lowtime = arg[0][i];
      lowvalue = arg[0][i+1];
    }
    
    double hitime = arg[0][i];
    double hivalue = arg[0][i+1];
    if (i < argcount  &&  hitime > lowtime){
      double ratio = (reltime - lowtime) / (hitime - lowtime);
      y->f1 += lowvalue + (hivalue - lowvalue) * ratio;
    }else{
      y->f1 += lowvalue;
    }
  }
  *arg += argcount;
}
/*--------------------------------------------------------------------------*/
/* trf_sffm:  spice compatible function: sffm
 * single frequency frequency modulation
 * arg0 = dc offset
 * arg1 = amplitude
 * arg2 = carrier freq
 * arg3 = mod index
 * arg4 = signal freq
 */
/*ARGSUSED*/
static void trf_sffm(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip){
    double mod = arg3 * sin(kPIx2*arg4*reltime);
    y->f1 += arg0 + arg1 * sin(kPIx2*arg2*reltime + mod);
  }
  *arg += aSFFM;
}
/*--------------------------------------------------------------------------*/
/* trf_sin:  spice compatible function: sin
 * sinusoidal function, mainly for sources
 * value is decaying sinusoidal function of time
 * arg0 = offset
 * arg1 = amplitude
 * arg2 = frequency
 * arg3 = delay
 * arg4 = damping
 */
/*ARGSUSED*/
static void trf_sin(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip){
    y->f1 += arg0;
    if (reltime > arg3){
      double x = arg1 * sin(kPIx2*arg2*(reltime-arg3));
      if (arg4 != 0.)
	x *= Exp(-(reltime-arg3)*arg4);
      y->f1 += x;
    }
  }
  *arg += aSIN;
}
/*--------------------------------------------------------------------------*/
/* trf_tanh:  function: tanh
 * piecewise linear function: block that clips
 * bad design: should be keyword, so it can apply to the total part
 *	       should be able to specify upper and lower limits
 * arg0 = normal value
 * arg1 = output clip pt
 */
static void trf_tanh(const COMPONENT* /*brh*/, double **arg, cpoly1_t* y)
{
  if (!skip){
    if (arg1 != 0.){
      double aa = y->x * arg0/arg1;
      double cosine = cosh(aa);
      y->f1 += arg0 / (cosine*cosine);
      y->c0 += arg1 * tanh(aa) - y->f1 * y->x;
    }
    /* else 0 */
  }
  *arg += aTANH;
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void trf_ic(const COMPONENT* /*brh*/, double **arg, cpoly1_t* /*y*/)
{
  if (!skip){
    CARD::initial_condition = arg0;
  }
  *arg += 1;
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void trf_ii(const COMPONENT* /*brh*/, double **arg, cpoly1_t* /*y*/)
{
  if (!skip){
    CARD::initial_current = arg0;
  }
  *arg += 1;
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void trf_iv(const COMPONENT* /*brh*/, double **arg, cpoly1_t* /*y*/)
{
  if (!skip){
    CARD::initial_voltage = arg0;
  }
  *arg += 1;
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void trf_tempco(const COMPONENT* /*brh*/, double **arg, cpoly1_t* /*y*/)
{
#ifdef NEVER
  tempco = arg0;
#endif
  *arg += 1;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
