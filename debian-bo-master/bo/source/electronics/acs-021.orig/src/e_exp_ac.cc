/*$Id: e_exp_ac.cc,v 11.30 96/03/17 19:21:06 al Exp $ -*- C++ -*-
 * Takes care of nonlinearities, behavioral modeling, etc. in ac analysis.
 */
#include "e_compon.h"
#include "error.h"
#include "e_exp.h"
#include "s__.h"
/*--------------------------------------------------------------------------*/
	void	acfix(COMPONENT*);
static	void	acf_ac(const COMPONENT*,double**,COMPLEX*);
static	void	acf_dc(const COMPONENT*,double**,COMPLEX*);
static	void	acf_dctran(const COMPONENT*,double**,COMPLEX*);
static	void	acf_frequency(const COMPONENT*,double**,COMPLEX*);
static	void	acf_period(const COMPONENT*,double**,COMPLEX*);
static	void	acf_ramp(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_time(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_tran(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_bandwidth(const COMPONENT*,double**,COMPLEX*);
static	void	acf_complex(const COMPONENT*,double**,COMPLEX*);
static	void	acf_cornerdown(const COMPONENT*,double**,COMPLEX*);
static	void	acf_cornerup(const COMPONENT*,double**,COMPLEX*);
static	void	acf_delay(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_exp(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_expterm(const COMPONENT*,double**,COMPLEX*);
static	void	acf_generator(const COMPONENT*,double**,COMPLEX*);
static	void	acf_max(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_netfunc(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_notch(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_numeric(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_offset(const COMPONENT*,double**,COMPLEX*);
static	void	acf_polar(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_polyterm(const COMPONENT*,double**,COMPLEX*);
static	void	acf_pulse(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_pwl(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_sffm(const COMPONENT*,double**,COMPLEX*);
static	void	acf_sin(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_tanh(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_ic(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_ii(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_iv(const COMPONENT*,double**,COMPLEX*);	
static	void	acf_tempco(const COMPONENT*,double**,COMPLEX*);
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
static double bias;
/*--------------------------------------------------------------------------*/
void acfix(COMPONENT* brh)
{
  COMPLEX & y = brh->ev;	/* return value */
  
  if (brh->issource()){
    skip = true;
    y = 0.;
  }else{
    skip = false;
    y = brh->val;
  }
  
  bias = brh->acbias;
  if (brh->x){
    struct expr *x;
    double *arg;
    int *key;
    
    x = (struct expr*)brh->x;
    arg = x->args->args;
    for (key = x->keys->args;  *key;  key++){
      switch (*key){
        case eAC:		acf_ac(brh,&arg,&y);		break;
        case eDC:		acf_dc(brh,&arg,&y);	 	break;
	case eDCTRAN:		acf_dctran(brh,&arg,&y);	break;
	case eFREQUENCY:	acf_frequency(brh,&arg,&y);	break;
	case ePERIOD:		acf_period(brh,&arg,&y);	break;
	case eRAMP:		acf_ramp(brh,&arg,&y);		break;
	case eTIME:		acf_time(brh,&arg,&y);		break;
	case eTRAN:		acf_tran(brh,&arg,&y);	 	break;

	case eBANDWIDTH:	acf_bandwidth(brh,&arg,&y);	break;
	case eCOMPLEX:		acf_complex(brh,&arg,&y);	break;
	case eCORNERDOWN:	acf_cornerdown(brh,&arg,&y);	break;
	case eCORNERUP:		acf_cornerup(brh,&arg,&y);	break;
	case eDELAY:		acf_delay(brh,&arg,&y);		break;
	case eEXP:		acf_exp(brh,&arg,&y);		break;
	case eEXPTERM:		acf_expterm(brh,&arg,&y);	break;
	case eGENERATOR:	acf_generator(brh,&arg,&y);	break;
	case eMAX:		acf_max(brh,&arg,&y);		break;
	case eNETFUNC:		acf_netfunc(brh,&arg,&y);	break;
	case eNOTCH:		acf_notch(brh,&arg,&y);		break;
	case eNUMERIC:		acf_numeric(brh,&arg,&y);	break;
	case eOFFSET:		acf_offset(brh,&arg,&y);	break;
	case ePOLAR:		acf_polar(brh,&arg,&y);		break;
	case ePOLYTERM:		acf_polyterm(brh,&arg,&y);	break;
	case ePULSE:		acf_pulse(brh,&arg,&y);		break;
	case ePWL:		acf_pwl(brh,&arg,&y);		break;
	case eSFFM:		acf_sffm(brh,&arg,&y);		break;
	case eSIN:		acf_sin(brh,&arg,&y);		break;
	case eTANH:		acf_tanh(brh,&arg,&y);		break;

	case eIC:		acf_ic(brh,&arg,&y);		break;
	case eII:		acf_ii(brh,&arg,&y);		break;
	case eIV:		acf_iv(brh,&arg,&y);		break;
	case eTEMPCO:		acf_tempco(brh,&arg,&y);	break;

	default:  error(bWARNING, "%s: undefined function: %d\n",
			brh->printlabel(), *key); break;
      }
    }
  }
}
/*--------------------------------------------------------------------------*/
/* acf_ac:  keyword: ac
 * following args for ac analysis only
 * here : do it
 * no args
 */
/*ARGSUSED*/
static void acf_ac(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  *y = 0.;
  skip = false;
  *arg += aAC;
}
/*--------------------------------------------------------------------------*/
/* acf_dc:  keyword: dc
 * following args for dc analysis only
 * here: skip them
 * no args
 */
/*ARGSUSED*/
static void acf_dc(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
  skip = true;
  *arg += aDC;
}
/*--------------------------------------------------------------------------*/
/* acf_dctran:  keyword: dctran
 * following args for dc and transient analysis
 * here: skip them
 * no args
 */
/*ARGSUSED*/
static void acf_dctran(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
  skip = true;
  *arg += aDCTRAN;
}
/*--------------------------------------------------------------------------*/
/* acf_frequency:  keyword: frequency
 * the value is frequency dependent (ac only)
 * works only in ac analysis, otherwise could be non-causal.
 * arg0 = test freq
 */
/*ARGSUSED*/
static void acf_frequency(const COMPONENT* brh, double **arg, COMPLEX* /*y*/)
{
  error(bWARNING,"frequency not implemented: %s\n", brh->printlabel());
  *arg += aFREQUENCY;
}
/*--------------------------------------------------------------------------*/
/* acf_period:  keyword: period
 * periodic in time
 * implies that following args for transient analysis only.
 * here: skip them
 * arg0 = period
 */
/*ARGSUSED*/
static void acf_period(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
  skip = true;
  *arg += aPERIOD;
}
/*--------------------------------------------------------------------------*/
/* acf_ramp: keyword: ramp
 * ramp value between plotted points
 * transient only: skip here
 */
/*ARGSUSED*/
static void acf_ramp(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
  skip = true;
  *arg += aRAMP;
}
/*--------------------------------------------------------------------------*/
/* acf_time:  keyword: time
 * following args take effect at the given time (switch)
 * obviously, transient only.
 * arg0 = switch time
 */
/*ARGSUSED*/
static void acf_time(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
  skip = true;
  *arg += aTIME;
}
/*--------------------------------------------------------------------------*/
/* acf_tran:  keyword: transient
 * following args for transient analysis only.
 * no args
 */
/*ARGSUSED*/
static void acf_tran(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
  skip = true;
  *arg += aTRAN;
}
/*--------------------------------------------------------------------------*/
/* acf_bandwidth:  function: bandwidth
 * gain block, with a bandwidth. (ac only)
 * bad design: should be keyword instead.
 * arg0 = dc gain
 * arg1 = 3 db freq.
 */
/*ARGSUSED*/
static void acf_bandwidth(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip  &&  arg1 != 0.){
    double ratio;
    double coeff;
    ratio = SIM::freq / arg1;
    coeff = arg0 / (1.+(ratio*ratio));
    *y += COMPLEX(coeff, -coeff * ratio);
  }
  *arg += aBANDWIDTH;
}
/*--------------------------------------------------------------------------*/
/* acf_complex:  function: complex
 * complex value, cartesian coordinates, in frequency domain (ac only)
 * arg0 = real part
 * arg1 = imaginary part
 */
/*ARGSUSED*/
static void acf_complex(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip)
    *y += COMPLEX(arg0, arg1);
  *arg += aCOMPLEX;
}
/*--------------------------------------------------------------------------*/
/* acf_cornerdown:  function: cornerdown
 * piecewise linear function:
 * output is 0 for bias >= arg1
 * derivative is arg0 for bias <= arg1
 * arg0 = active slope
 * arg1 = break point
 */
static void acf_cornerdown(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip  &&  bias < arg1)
    *y += arg0;
  *arg += aCORNERDOWN;
}
/*--------------------------------------------------------------------------*/
/* acf_cornerup:  function: cornerup
 * piecewise linear function:
 * output is 0 for bias <= arg1
 * derivative is arg0 for bias >= arg1
 * arg0 = active slope
 * arg1 = break point
 */
static void acf_cornerup(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip  &&  bias > arg1)
    *y += arg0;
  *arg += aCORNERUP;
}
/*--------------------------------------------------------------------------*/
/* acf_delay:  function: delay
 * time delay (ac only)
 * arg0 = magnitude
 * arg1 = delay
 */
static void acf_delay(const COMPONENT* brh, double **arg, COMPLEX* y)
{
  if (!skip){
    double ratio;
    ratio = SIM::freq * arg1;
    if (ratio > 100000.){
      error(bPICKY, "delay too long: %s\n", brh->printlabel());
      ratio = 0.;
    }
    *y += polar(arg0, -360.0 * DTOR * ratio);
  }
  *arg += aDELAY;
}
/*--------------------------------------------------------------------------*/
/* acf_exp:  spice compatible function: exp
 * spice source exponential function: exponential function of time
 * no-op in ac analysis, with a complaint
 * arg0 = initial value
 * arg1 = pulsed value
 * arg2 = rise delay
 * arg3 = rise time const
 * arg4 = fall delay
 * arg5 = fall time const
 */
/*ARGSUSED*/
static void acf_exp(const COMPONENT* brh, double **arg, COMPLEX* /*y*/)
{
  if (!skip)
    error(bDEBUG,"%s: exp not supported in ac analysis\n",brh->printlabel());
  *arg += aEXP;
}
/*--------------------------------------------------------------------------*/
/* acf_expterm:  function: expterm
 * exponent term, non-integer power term (not exponential)
 * like polyterm, but for non-integers
 * only works for positive input, because negative number raised to
 * non-integer power is usually complex.
 * arg0 = coefficient
 * arg1 = exponent
 */
static void acf_expterm(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip  &&  bias > 0.){
    double coeff;
    coeff = arg0 * pow(bias,arg1-1);
    *y += coeff * arg1;
  }
  *arg += aEXPTERM;
}
/*--------------------------------------------------------------------------*/
/* acf_generator:  function: generator
 * value is derived from the "signal generator" (generator command)
 * intended for fixed sources, as circuit input.
 * arg0 = scale factor
 */
/*ARGSUSED*/
static void acf_generator(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip)
    *y += arg0;
  *arg += aGENERATOR;
}
/*--------------------------------------------------------------------------*/
/* acf_max:  function: max
 * piecewise linear function: block that clips
 * bad design: should be keyword, so it can apply to the total part
 *	       should be able to specify upper and lower limits
 * arg0 = normal value
 * arg1 = output clip pt
 */
static void acf_max(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip  &&  arg0 != 0.){
    double clip_input;
    clip_input = arg1/arg0;
    if (bias >= -clip_input   &&   bias <= clip_input)
      *y += arg0;
  }
  *arg += aMAX;
}
/*--------------------------------------------------------------------------*/
/* acf_netfunc:  function: netfunction
 * gain block, nth order response. (ac only)
 * BUG: only partial checking for number of args
 * arg0 = arg count
 * arg1 = dc gain
 * arg2 = ref frequency
 * arg3 = order of denom
 * arg* = coefs of denom
 * arg* = coefs of numer
 */
/*ARGSUSED*/
static void acf_netfunc(const COMPONENT* brh, double **arg, COMPLEX* y)
{
  int argcount;
  argcount = (int)arg0;

  if (!skip  &&  arg1 != 0.){
    double *denomcoeff;	/* coefficients of denominator			*/
    double *numercoeff;	/* coefficients of numerator			*/
    int denomorder;	/* order of denominator (#coefs = order + 1)	*/
    int numerorder;	/* order of numerator				*/
    double wacc;	/* stash for powers of f			*/
    COMPLEX denom;	/* denominator					*/
    COMPLEX numer;	/* numerator					*/
    int ii;		/* generic loop index				*/
    double normfreq;	/* normalized frequency				*/
    double nf2;		/*	"	"	squared			*/

    normfreq = SIM::freq / ((arg2 != 0.) ? arg2 : 1./kPIx2);
    nf2 = normfreq * normfreq;

    denomorder = (int)arg3;
    numerorder = argcount - denomorder - 6;
    if (numerorder > denomorder){
      numerorder = denomorder;
      error(bWARNING, "%s: too many args\n", brh->printlabel());
    }else if (numerorder < 0){
      denomorder += numerorder;
      numerorder = 0;
      error(bWARNING, "%s: too few args\n", brh->printlabel());
    }

    denomcoeff = &(arg[0][4]);
    numercoeff = &(arg[0][5+denomorder]);

    wacc = 1.;
    denom = denomcoeff[0]; /* * wacc */
    for (ii = 2;   ii <= denomorder;   ii += 2){
      wacc *= -nf2;
      denom += denomcoeff[ii] * wacc;
    }

    wacc = normfreq;
    denom = COMPLEX(0., denomcoeff[1] * wacc);
    for (ii = 3;   ii <= denomorder;   ii += 2){
      wacc *= -nf2;
      denom += COMPLEX(0., denomcoeff[ii] * wacc);
    }

    wacc = 1.;
    numer = numercoeff[0]; /* * wacc */
    for (ii = 2;   ii <= numerorder;   ii += 2){
      wacc *= -nf2;
      numer += numercoeff[ii] * wacc;
    }

    wacc = normfreq;
    numer = COMPLEX(0., numercoeff[1] * wacc);
    for (ii = 3;   ii <= numerorder;   ii += 2){
      wacc *= -nf2;
      numer += COMPLEX(0., numercoeff[ii] * wacc);
    }
    
    *y += arg1 * numer / denom;
  }
  *arg += argcount;
}
/*--------------------------------------------------------------------------*/
/* acf_notch:  function: notch
 * piecewise linear function: crossover notch, dead zone
 * symmetric around zero
 * arg0 = normal value
 * arg1 = dead zone size
 */
static void acf_notch(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip){
    if (bias > arg1   ||   bias < -arg1)
      *y += arg0;
  }
  *arg += aNOTCH;
}
/*--------------------------------------------------------------------------*/
/* acf_numeric:  simple numeric argument
 * arg0 = value
 */
/*ARGSUSED*/
static void acf_numeric(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip)
    *y += arg0;
  *arg += aNUMERIC;
}
/*--------------------------------------------------------------------------*/
/* acf_offset:  function: offset
 * fixed dc offset
 * bad design: should be keyword
 * arg0 = gain
 * arg1 = output offset
 */
/*ARGSUSED*/
static void acf_offset(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip)
    *y += arg0;
  *arg += aOFFSET;
}
/*--------------------------------------------------------------------------*/
/* acf_polar:  function: polar
 * complex value in polar coordinates, in frequency domain (ac only)
 * arg0 = magnitude
 * arg1 = phase
 */
/*ARGSUSED*/
static void acf_polar(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip)
    *y += polar(arg0, arg1);
  *arg += aPOLAR;
}
/*--------------------------------------------------------------------------*/
/* acf_polyterm:  function: polyterm
 * polynomial term, one term in a polynomial
 * power must be integer, is rounded to nearest integer
 * caution: will divide by zero with zero input and negative exponent
 * arg0 = coefficient
 * arg1 = exponent
 */
static void acf_polyterm(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip){
    int expo;
    expo = (int)floor(arg1+.5);
    if (expo != 0){
      double coeff;
      coeff = arg0 * pow(bias,expo-1);
      *y += coeff * expo;
    }
  }
  *arg += aPOLYTERM;
}
/*--------------------------------------------------------------------------*/
/* acf_pulse: spice compatible function: pulse
 * spice pulse function (for sources)
 * no-op in ac analysis, with a complaint
 * arg0 = initial value
 * arg1 = pulsed value
 * arg2 = delay time
 * arg3 = rise time
 * arg4 = fall time
 * arg5 = pulse width
 * arg6 = period
 */
/*ARGSUSED*/
static void acf_pulse(const COMPONENT* brh, double **arg, COMPLEX* /*y*/)
{
  if (!skip)
    error(bDEBUG,"%s: pulse not supported in ac analysis\n",brh->printlabel());
  *arg += aPULSE;
}
/*--------------------------------------------------------------------------*/
/* acf_pwl:  spice compatible function: pwl
 * "piece-wise linear" point-plotting function of time.
 * no-op in ac analysis, with a complaint
 * arg0 = arg count
 * no other args matter
 */
/*ARGSUSED*/
static void acf_pwl(const COMPONENT* brh, double **arg, COMPLEX* /*y*/)
{
  if (!skip)
    error(bDEBUG,"%s: pwl not supported in ac analysis\n",brh->printlabel());
  *arg += (int)arg0;
}
/*--------------------------------------------------------------------------*/
/* acf_sffm:  spice compatible function: sffm
 * single frequency frequency modulation
 * no-op in ac analysis, with a complaint
 * arg0 = dc offset
 * arg1 = amplitude
 * arg2 = carrier freq
 * arg3 = mod index
 * arg4 = signal freq
 */
/*ARGSUSED*/
static void acf_sffm(const COMPONENT* brh, double **arg, COMPLEX* /*y*/)
{
  if (!skip)
    error(bDEBUG,"%s: sffm not supported in ac analysis\n",brh->printlabel());
  *arg += aSFFM;
}
/*--------------------------------------------------------------------------*/
/* acf_sin:  spice compatible function: sin
 * sinusoidal function, mainly for sources
 * no-op in ac analysis, with a complaint
 * arg0 = offset
 * arg1 = amplitude
 * arg2 = frequency
 * arg3 = delay
 * arg4 = damping
 */
/*ARGSUSED*/
static void acf_sin(const COMPONENT* brh, double **arg, COMPLEX* /*y*/)
{
  if (!skip)
    error(bDEBUG,"%s: sin not supported in ac analysis\n",brh->printlabel());
  *arg += aSIN;
}
/*--------------------------------------------------------------------------*/
/* acf_tanh:  function: tanh
 * for now, a copy of max
 * piecewise linear function: block that clips
 * bad design: should be keyword, so it can apply to the total part
 *	       should be able to specify upper and lower limits
 * arg0 = normal value
 * arg1 = output clip pt
 */
static void acf_tanh(const COMPONENT* /*brh*/, double **arg, COMPLEX* y)
{
  if (!skip){
    if (arg1 != 0.){
      double cosine;
      cosine = cosh(bias * arg0/arg1);
      *y += arg0 / (cosine*cosine);
    }
    /* else 0 */
  }
  *arg += aTANH;
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void acf_ic(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
  *arg += 1;
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void acf_ii(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
  *arg += 1;
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void acf_iv(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
  *arg += 1;
}
/*--------------------------------------------------------------------------*/
/*ARGSUSED*/
static void acf_tempco(const COMPONENT* /*brh*/, double **arg, COMPLEX* /*y*/)
{
#ifdef NEVER
  tempco = arg0;
#endif
  *arg += 1;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
