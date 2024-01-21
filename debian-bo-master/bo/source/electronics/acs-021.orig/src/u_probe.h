/*$Id: u_probe.h,v 11.22 96/02/18 11:46:56 al Exp $ -*- C++ -*-
 * single probe item
 */
#include "l_compar.h"
#ifndef U_PROBE_H
#define U_PROBE_H
/*--------------------------------------------------------------------------*/
class CKT_BASE;
struct xprobe_t;
/*--------------------------------------------------------------------------*/
class PROBE {
private:
  char	    what[LABELEN+1];    
  int	    node;
  CKT_BASE* brh;
  double    low,high;
public:
	PROBE(const char *What="", int Node=0, CKT_BASE *Brh=NULL,
	      double Lo=0, double Hi=0.);
	void	  limit(double Lo,double Hi){low = Lo; high = Hi;}
	PROBE &	  detach();
		  operator bool(){return *what;}
	char*	  label()const;
	double	  value()const;
	char*	  svalue()const;
	xprobe_t  xvalue()const;
	CKT_BASE* object()const	{return brh;}
	double	  lo()const	{return low;}
	double	  hi()const	{return high;}
	double	  range()const	{return high-low;}
	bool	  inrange()const{return inorder(low,value(),high);}
private:
	double	  probe_node()const;
	double	  trprobe_node()const;
	double	  acprobe_node()const;
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
