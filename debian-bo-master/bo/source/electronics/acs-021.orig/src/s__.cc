/*$Id: s__.cc,v 11.37 96/03/24 10:09:27 al Exp $ -*- C++ -*-
 * base class for simulation methods
 */
#include "s__.h"

double	SIM::freq = 0.;
COMPLEX	SIM::jomega = 0.;
double	SIM::time0 = 0.;
double	SIM::time1 = 0.;
double	SIM::dtmin = 0.;
double	SIM::temp = 0.;
double	SIM::last_time = 0.;
double	SIM::damp = 1.0;
bool	SIM::uic = false;
bool	SIM::bypass_ok = false;
int	SIM::inc_mode = false;
bool	SIM::fulldamp = false;
int	SIM::mode = sNONE;
sim_phase_t SIM::phase = pNONE;
bool	SIM::freezetime = false;
int	*SIM::nm = NULL;
double	*SIM::i = NULL;
double	*SIM::v0 = NULL;
double	*SIM::vi1 = NULL;
double	*SIM::vt1 = NULL;
double	*SIM::fw = NULL;
double	*SIM::vdc = NULL;
COMPLEX	*SIM::ac = NULL;
double	SIM::genout = 0.;
ASTACK<CARD*> SIM::loadq(1);
