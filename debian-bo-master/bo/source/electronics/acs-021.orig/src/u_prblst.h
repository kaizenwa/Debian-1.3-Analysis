/*$Id: u_prblst.h,v 11.22 96/02/18 11:46:54 al Exp $ -*- C++ -*-
 * list of probes
 */
#include "mode.h"
#ifndef U_PRBLST_H
#define U_PRBLST_H
/*--------------------------------------------------------------------------*/
class CS;
class PROBE;
class CKT_BASE;
/*--------------------------------------------------------------------------*/
class PROBELIST {
private:
  PROBE*  bag;
  int     probecount;
public:
		PROBELIST();
		~PROBELIST();
	int     count()const{return probecount;}
	PROBE&  operator[](int i)const;
	int     list(const char*)const;
	int     clear();
	void	operator-=(CS&);
	void    operator-=(CKT_BASE*);
	void    operator+=(CS&);
private:
	void    add_node_list(CS&,const char*);
	void    add_branches(CS&,const char*);
	void    add_all_nodes(const char*);
	void    init();
};
/*--------------------------------------------------------------------------*/
class PROBE_LISTS {
public:
 static PROBELIST alarm[sCOUNT]; // list of alarm probes
 static PROBELIST plot[sCOUNT];  // list of plot probes
 static PROBELIST print[sCOUNT]; // list of print probes
 static PROBELIST store[sCOUNT]; // list of probes to store for postproc
 static void purge(CKT_BASE*);
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
