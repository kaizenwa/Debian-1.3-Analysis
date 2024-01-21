/*$Id: m_matrix.h,v 11.23 96/02/21 12:46:21 al Exp $ -*- C++ -*-
 * Sparse matrix package
 * Bump and spike - bordered block diagonal pattern
 */
#ifndef M_MATRIX_H
#define M_MATRIX_H
/*--------------------------------------------------------------------------*/
#include <assert.h>
#include <stdlib.h>
#include "md_bool.h"
#include "md_const.h"
/*--------------------------------------------------------------------------*/
template <class T>
class BSMATRIX {
private:
  mutable bool  *Changed;	// flag: this node changed value
  int	*Lownode;	// lowest node connecting to this one
  T	*space;		// ptr to actual memory space used
  T	**rowptr;	// ptrs to col 0 of every row
  T	**colptr;	// ptrs to row 0 of every col
  T	**diaptr;	// ptrs to diagonal
  int	nzcount;	// count of non-zero elements
  int	Size;		// # of rows and columns
  T	Zero;		// always 0 but not const
  T	Trash;		// depository for row and col 0, write only
  T	MinPivot;	// minimum pivot value
public:
 		 BSMATRIX(int ss=0)	{init(ss);}
  		~BSMATRIX()		{uninit();}
  BSMATRIX<T> &	reinit(int ss=0)	{return uninit().init(ss);}
  BSMATRIX<T> &	clone(const BSMATRIX<T> &);
  BSMATRIX<T> &	iwant(int, int);
  BSMATRIX<T> &	unallocate();
  BSMATRIX<T> &	allocate();
  void		setchanged(int n, bool x = true)CONST{Changed[n] = x;}
  BSMATRIX<T> &	setminpivot(double x){MinPivot = x; return *this;}
  BSMATRIX<T> &	zero();
  BSMATRIX<T> &	dezero(T& o);
  int		size()const{return Size;}
  double 	density();
  double 	sparsity(){return 1.-density();}
private:
  BSMATRIX<T> &	uninit();
  BSMATRIX<T> &	init(int s=0);
  const T& 	d(int r, int  )const{return *(diaptr[r]);}
  const T& 	u(int r, int c)const{return colptr[c][r];}
  const T& 	l(int r, int c)const{return rowptr[r][-c];}
  T&		SubtractDotProduct(int r, int c, int d);
  T&		SubtractDotProduct(int r, int c, int d, const T& in);
  BSMATRIX<T> &	lu_partial(CONST BSMATRIX<T> &);
  BSMATRIX<T> &	lu_full(CONST BSMATRIX<T> &);
  int		lownode(int i)const{return Lownode[i];}
  bool		ischanged(int n)const{return Changed[n];}
public:
#if !defined(NO_MATRIX_DEBUG) && !defined(NDEBUG)
  T&	d(int r, int c){
	assert(diaptr);
  	assert(r == c);
	assert(0 <= r);
	assert(r <= Size);
	return *(diaptr[r]);
  }
  T&	u(int r, int c){
	assert(colptr);
	assert(Lownode);
	assert(1 <= Lownode[c]);
	assert(Lownode[c] <= r);
  	assert(r <= c);
	assert(c <= Size);
	return colptr[c][r];
  }
  T&	l(int r, int c){
	assert(rowptr);
	assert(Lownode);
	assert(1 <= Lownode[r]);
	assert(Lownode[r] <= c);
  	assert(c <= r);
	assert(r <= Size);
	return rowptr[r][-c];
  }
#else
  T&	d(int r, int  ){return *(diaptr[r]);}
  T&	u(int r, int c){return colptr[c][r];}
  T&	l(int r, int c){return rowptr[r][-c];}
#endif
  T&	m(int r, int c){return (c>=r) ? u(r,c) : l(r,c);}
  T&	s(int r, int c);
  BSMATRIX<T> &	lu_decomp(CONST BSMATRIX<T> &, bool dopartial);
  BSMATRIX<T> &	lu_decomp();
  BSMATRIX<T> &	fbsub(T* v);
  BSMATRIX<T> &	fbsub(T* x, const T* b, T* c = NULL);
};
/*--------------------------------------------------------------------------*/
#if defined(COMPILE_TEMPLATES) || defined(ComTemP)
  #include "m_matrix.cc"
#endif
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
