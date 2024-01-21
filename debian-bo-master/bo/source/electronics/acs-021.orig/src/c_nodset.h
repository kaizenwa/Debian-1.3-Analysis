/*$Id: c_nodset.h,v 11.22 96/02/18 11:45:26 al Exp $ -*- C++ -*-
 *  structures to keep track of nodesets and ICs
 */
#include "md.h"
#ifndef NODESET_H
#define NODESET_H
/*--------------------------------------------------------------------------*/
class CS;
/*--------------------------------------------------------------------------*/
class NODESET {
private:
  NODESET *Next;
  int    Node;
  double Voltage;
public:
	NODESET(int n=0,double v=0.,NODESET* p=NULL){Node=n;Voltage=v;Next=p;}
	void	command(CS&);
	void	add_list(CS&);
	void	add1(int,double);
	void	rm_all();
	void	rm1(int);
	void	list_all()const;
	void	list1()const;
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
