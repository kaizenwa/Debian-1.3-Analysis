/*$Id: constant.h,v 11.22 96/02/18 11:45:27 al Exp $ -*- C++ -*-
 * defined constants for just about everything
 */
#include "md.h"
#ifndef CONSTANT_H
#define CONSTANT_H
/*--------------------------------------------------------------------------*/
const double kPIx2 = 6.2831853071795864769252867665590057683944;
const double kPI   = 3.1415926535897932384626433832795028841972;
const double kPId2 = 1.5707963267948966192313216916397514420986;
const double DTOR  = 0.0174532925199432957692369076848861271344;
const double RTOD  = 57.2957795130823208768;
const double ONE_OVER_PI= 0.3183098861837906715377675267450287240689;
const double ABS_ZERO = -273.15;
const double E_0   = 8.854214871e-12;	/* permittivity of air 		  */
const double E_SI  = 11.7*E_0;	   /* permittivity of silicon (1.0359e-10)*/
const double E_OX  = 3.9*E_0;	   /* permittivity of oxide (3.45e-11)	  */
const double K	   = 1.3806226e-23;	/* Boltzmann's constant		  */
const double Q	   = 1.6021918e-19;	/* electronic charge		  */
const double NOT_INPUT = -(DBL_MAX)*(.8347658);	/* unlikely number	  */
const double NOT_VALID = -(DBL_MAX)*(.8547958);	/* unlikely number	  */
const double LINEAR    = -(DBL_MAX)*(.8747958);	/* unlikely number	  */
const double BIGBIG    =  (DBL_MAX)*(.9747958);	/* unlikely number	  */
const double VOLTMIN   =  1.0e-50;
const double PWRMIN    =  1.0e-100;

#define DBVOLTMIN   (20.*log10(VOLTMIN))
#define DBPWRMIN    (10.*log10(PWRMIN))
#define TOKENTERM   ",=()[]"

#define	BAD	(-1)
#define	GOOD	(0)
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
