/*$Id: e_storag.h,v 11.27 96/03/03 16:39:52 al Exp $ -*- C++ -*-
 * "base" class for energy storage elements (L & C)
 */
#include "e_elemnt.h"
#ifndef E_STORAGE_H
#define E_STORAGE_H
/*--------------------------------------------------------------------------*/
class STORAGE : public ELEMENT {
protected:
	STORAGE();
	STORAGE(CONST STORAGE& p);
	void	expand_pre();
	bool	advance();
protected:
  method_t method_a;	/* actual integration method (auto)	*/
  double   timef;	/* future event time			*/
  double   time0;	/* time now (y[012], m[01])		*/
  double   time1;	/* one tick ago	 (mt1, yt1)		*/
  double   time2;	/* two tick ago	 (mt2, yt2)		*/
  double   time3;	/* two tick ago	 (mt3. yt3)		*/
//fpoly1_t y0;		/* iteration parameters, new		*/
  fpoly1_t yt1;		/* iteration parameters, 1 time ago	*/
  fpoly1_t yt2;		/* iteration parameters, 2 times ago	*/
  fpoly1_t yt3;		/* iteration parameters, 3 times ago	*/
//fpoly1_t y1;		/* iteration parameters, 1 iter ago	*/
//fpoly1_t y2;		/* iteration parameters, 2 iter ago	*/
//cpoly1_t m0;		/* matrix parameters, new		*/
//cpoly1_t m1;		/* matrix parameters, 1 fill ago	*/
  cpoly1_t mt1;		/* matrix parameters, 1 time ago	*/

//COMPLEX  ev;		/* ac effective value (usually real)	*/
//COMPLEX  acg;		/* ac admittance matrix values		*/   
//double   acbias;	/* dc bias for ac use			*/
};
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
