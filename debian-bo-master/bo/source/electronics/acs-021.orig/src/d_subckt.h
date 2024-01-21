/*$Id: d_subckt.h,v 11.22 96/02/18 11:45:52 al Exp $ -*- C++ -*-
 * data structures for subcircuits
 */
#include "e_subckt.h"
#ifndef D_SUBCKT_H
#define D_SUBCKT_H
/*--------------------------------------------------------------------------*/
#define UNUSED 0
#define USED -3
#define NODESPERSUBCKT 1000
#define PORTSPERSUBCKT 100

#define sDEFAULT_modelname	""
/*--------------------------------------------------------------------------*/
class DEV_SUBCKT : public BASE_SUBCKT {
public:
	DEV_SUBCKT();
	DEV_SUBCKT(CONST DEV_SUBCKT&);
	~DEV_SUBCKT(){--Count;}
	CARD*	clone()CONST{return new DEV_SUBCKT(*this);}
	void	parse(CS&);
 	void	print(int,int)const;
	double	probe_tr_num(const char*)const;
	void	expand();
static	int	count(){return Count;}
private:
  static int	Count;
};

struct subckt {
   generic_t	*x;
   size_t	ssize;
   const generic_t *m;
   char		modelname[LABELEN+1];
   node_t	n[PORTSPERSUBCKT+1];
};

class MODEL_SUBCKT : public COMPONENT {
public:
	MODEL_SUBCKT(const char *name = "");
	MODEL_SUBCKT(const MODEL_SUBCKT&){assert(0);}
	CARD*	clone()CONST{return new MODEL_SUBCKT(*this);}
	void	parse(CS&);
 	void	print(int,int)const;
};

struct smod {
   generic_t	*x;
   size_t	ssize;
   const generic_t *m;
   char		modelname[LABELEN+1];
   node_t	n[PORTSPERSUBCKT+1];
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
