/*$Id: u_statu1.cc,v 11.22 96/02/18 11:45:20 al Exp $ -*- C++ -*-
 * place to store all kinds of statistics -- initialization
 */
#include "u_status.h"

TIMER	STATUS::get("get");
TIMER	STATUS::op("op");
TIMER	STATUS::dc("dc");
TIMER	STATUS::tran("tran");
TIMER	STATUS::four("fourier");
TIMER	STATUS::ac("ac");
TIMER	STATUS::set_up("setup");
TIMER	STATUS::order("order");
TIMER	STATUS::evaluate("evaluate");
TIMER	STATUS::load("load");
TIMER	STATUS::lud("lu");
TIMER	STATUS::back("back");
TIMER	STATUS::review("review");
TIMER	STATUS::output("output");
TIMER	STATUS::overhead("overhead");
TIMER	STATUS::aux1("aux1");
TIMER	STATUS::aux2("aux2");
TIMER	STATUS::aux3("aux3");
TIMER	STATUS::total("total");
int	STATUS::user_nodes = 0;
int	STATUS::subckt_nodes = 0;
int	STATUS::model_nodes = 0;
int	STATUS::total_nodes = 0;
int	STATUS::control[cCOUNT] = {0};
int	STATUS::iter[iCOUNT] = {0};
