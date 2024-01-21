/*$Id: u_opt1.cc,v 11.37 96/03/24 10:10:30 al Exp $ -*- C++ -*-
 * all the options set by the .options card.
 * initialization and declaration of statics
 */
#include "md.h"
#include "error.h"
#include "u_opt.h"

bool	OPT::acct = false;
bool	OPT::list = false;
bool	OPT::nomod = false;
bool	OPT::page = false;
bool	OPT::node = false;
bool	OPT::opts = false;
double	OPT::gmin = 1e-12;
double	OPT::reltol = .001;
double	OPT::abstol = 1e-12;
double	OPT::vntol = 1e-6;
double	OPT::trtol = 7.0;
double	OPT::chgtol = 1e-14;
double	OPT::pivtol = 1e-13;
double	OPT::pivrel = 1e-3;
int	OPT::numdgt = 4;
double	OPT::tnom = 300.0;
int	OPT::cptime = 30000;
int	OPT::limtim = 2;
int	OPT::limpts = 201;
int	OPT::lvlcod = 2;
int	OPT::lvltim = 2;
method_t OPT::method = mTRAPEZOID;
int	OPT::maxord = 2;
double	OPT::defl = 100e-6;
double	OPT::defw = 100e-6;
double	OPT::defad = 0.;
double	OPT::defas = 0.;

int	OPT::seed = 1;
double	OPT::wczero = 1e-9;
double	OPT::dampmax = 1.0;
double	OPT::dampmin = 0.5;
int	OPT::dampstrategy = dsINIT|dsDEVREGION|dsREVERSE;
double	OPT::floor = 1e-20;
double	OPT::tempamb = 300.0;
double	OPT::shortckt = 10e-6;
int	OPT::picky = bPICKY;
int	OPT::inwidth = 80;
int	OPT::outwidth = 80;
double	OPT::xdivisions = 4.;
double	OPT::ydivisions = 4.;
OPT::order_t	OPT::order = oAUTO;
smode_t	OPT::mode = mMIXED;
int	OPT::transits = 2;
bool	OPT::dupcheck = false;
bypass_t OPT::bypass = bYES;
bool	OPT::incmode = true;
bool	OPT::lubypass = true;
bool	OPT::fbbypass = true;
bool	OPT::traceload = true;
int	OPT::itermin = 1;
double	OPT::limit = 1e20;
double	OPT::vmax = 30;
double	OPT::vmin = -30;
double	OPT::dtmin = 1e-12;
double	OPT::dtratio = 1e9;
bool	OPT::rstray = false;
bool	OPT::cstray = true;
int	OPT::harmonics = 9;
double	OPT::trstepgrow = 2.;
double	OPT::trstepshrink = 2.;		/* spice is fixed at 8 */
double	OPT::trreject = .5;
bool	OPT::showall = false;
int	OPT::foooo = 0;
int	OPT::diodeflags  = 0;
int	OPT::mosflags = 0;

double	OPT::lowlim = 1. - OPT::reltol;
double	OPT::uplim = 1. + OPT::reltol;

int	OPT::itl[OPT::ITL_COUNT] = { 
		100,	/* 0=dummy */
		100,	/* 1=dc (bias) iteration limit */
		50, 	/* 2=dc transfer iteration limit */
		6,	/* 3=lower transient iteration limit (spice is 4) */
		20,	/* 4=upper transient iteration limit (spice is 10) */
		5000,	/* 5=transient total iterations allowed */
		0,	/* 6=source stepping iteration limit */
		1,	/* 7=worst case iteration limit */
		99	/* 8=trace nonconvergence start iteration */
};
