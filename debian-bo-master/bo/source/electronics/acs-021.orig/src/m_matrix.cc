/*$Id: m_matrix.cc,v 11.25 96/02/26 21:17:32 al Exp $ -*- C++ -*-
 * Sparse matrix package
 * Bump and spike - bordered block diagonal pattern
 */
#ifndef M_MATRIX_CC
#define M_MATRIX_CC
/*--------------------------------------------------------------------------*/
#include "u_status.h"
#include "u_opt.h"
#include "e_node.h"
#include "error.h"
#include "l_compar.h"
#include "m_matrix.h"
/*--------------------------------------------------------------------------*/
extern NODE* nstat;
/*--------------------------------------------------------------------------*/
template <class T>
BSMATRIX<T> & BSMATRIX<T>::uninit()
{
  unallocate();
  delete [] Lownode;
  Lownode = NULL;
  delete [] Changed;
  Changed = NULL;
  return *this;
}
/*--------------------------------------------------------------------------*/
template <class T>
BSMATRIX<T> & BSMATRIX<T>::init(int ss)
{
  Trash = Zero = 0.;
  nzcount = 0;
  Size = ss;
  rowptr = colptr = diaptr = NULL;
  space = NULL;
  Lownode = new int[Size+1];
  assert(Lownode);
  {for (int ii = 0;  ii <= Size;  ++ii)
    Lownode[ii] = ii;
  }
  Changed = new bool[Size+1];
  assert(Changed);
  for (int ii = 0;  ii <= Size;  ++ii)
    setchanged(ii, false);
  return *this;
}
/*--------------------------------------------------------------------------*/
/* clone: copy to self the structure of another BSMATRIX
 * this does not copy the values stored in the matrix
 */
template <class T>
BSMATRIX<T> & BSMATRIX<T>::clone(const BSMATRIX<T> & aa)
{
  reinit(aa.size());
  for (int ii = 0;  ii <= Size;  ++ii)
    Lownode[ii] = aa.lownode(ii);  
  return *this;
}
/*--------------------------------------------------------------------------*/
/* iwant: indicate that "iwant" to allocate this spot in the matrix
 */
template <class T>
BSMATRIX<T> & BSMATRIX<T>::iwant(int node1, int node2)
{
  assert(Lownode);
  assert(node1 <= Size);
  assert(node2 <= Size);
  if (node1 == 0  ||  node2 == 0){	/* node 0 is ground, and doesn't    */
    ;					/* count as a connection	    */
  }else if (node1 < Lownode[node2]){
    Lownode[node2]=node1;
  }else if (node2 < Lownode[node1]){
    Lownode[node1]=node2;
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
template <class T>
BSMATRIX<T> & BSMATRIX<T>::unallocate()
{
  assert (Zero == 0.);
  delete [] rowptr;
  delete [] colptr;
  delete [] diaptr;
  delete [] space;
  rowptr = colptr = diaptr = NULL;
  space = NULL;
  return *this;
}
/*--------------------------------------------------------------------------*/
/* allocate: really get the space to work
 */
template <class T>
BSMATRIX<T> & BSMATRIX<T>::allocate()
{
  assert(Lownode);
  nzcount = 0;
  {for (int ii = 0;   ii <= Size;   ++ii)
    nzcount += 2 * (ii - Lownode[ii]) + 1;
  }
  colptr = new T*[Size+1];
  rowptr = new T*[Size+1];
  diaptr = new T*[Size+1];
  space  = new T[nzcount];
  assert(colptr);
  assert(rowptr);
  assert(diaptr);
  zero();

  T* point = space;
  for (int ii = 0;   ii <= Size;   ++ii){
    colptr[ii] = point - Lownode[ii];
    rowptr[ii] = colptr[ii] + 2*ii;
    diaptr[ii] = colptr[ii] + ii;
    point += 2 * (ii - Lownode[ii]) + 1;
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
template <class T>
T& BSMATRIX<T>::s(int row, int col)
{
  assert(Lownode);
  assert(0 <= col);
  assert(col <= Size);
  assert(0 <= row);
  assert(row <= Size);
  if (col == row){
    return d(row,col);
  }else if (col > row){			/* above the diagonal */
    if (row == 0)
      return Trash;
    else if (row < Lownode[col])
      return Zero;
    else
      return u(row,col);
  }else{/* if (col < row) */		/* below the diagonal */
    if (col == 0)
      return Trash;
    else if (col < Lownode[row])
      return Zero;
    else
      return l(row,col);
  }
}
/*--------------------------------------------------------------------------*/
/* zero: wipe the whole array
 */
template <class T>
BSMATRIX<T> & BSMATRIX<T>::zero()
{
  assert(space);
  Trash = Zero = 0.;
  for (int ii = 0;  ii < nzcount;  ii++){
    space[ii] = Zero;
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
/* dezero: make sure(?) the diagonal is non-zero
 */
template <class T>
BSMATRIX<T> & BSMATRIX<T>::dezero(T& offset)
{
  for (int ii = 1;  ii <= Size;  ii++){
    d(ii,ii) += offset;
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
template <class T>
double BSMATRIX<T>::density()
{
  if (Size > 0){
    assert(Lownode);
    nzcount = 0;
    for (int ii = 0;   ii <= Size;   ++ii)
      nzcount += 2 * (ii - Lownode[ii]) + 1;
    return (double)(nzcount-1)/(Size*Size);
  }else{
    assert(Size == 0);
    return 0;
  }
}
/*--------------------------------------------------------------------------*/
template <class T>
T& BSMATRIX<T>::SubtractDotProduct(int rr, int cc, int dd)
{
  assert(Lownode);
  int kk = max(Lownode[rr], Lownode[cc]);
  int len = dd - kk;
  T& dot = m(rr, cc);
  if (len > 0){
    T* row = &(l(rr,kk));
    T* col = &(u(kk,cc));
    /* for (ii = kk;   ii < dd;   ++ii) */
    for (int ii = 0;   ii < len;   ++ii){
      dot -= row[-ii] * col[ii];
    }
  }
  return dot;
}
/*--------------------------------------------------------------------------*/
template <class T>
T& BSMATRIX<T>::SubtractDotProduct(int rr, int cc, int dd, const T& in)
{
  assert(Lownode);
  int kk = max(Lownode[rr], Lownode[cc]);
  int len = dd - kk;
  T& dot = m(rr, cc);
  dot = in;
  if (len > 0){
    T* row = &(l(rr,kk));
    T* col = &(u(kk,cc));
    /* for (ii = kk;   ii < dd;   ++ii) */
    for (int ii = 0;   ii < len;   ++ii){
      dot -= row[-ii] * col[ii];
    }
  }
  return dot;
}
/*--------------------------------------------------------------------------*/
template <class T>
BSMATRIX<T> & BSMATRIX<T>::lu_partial(CONST BSMATRIX<T> & aa)
{
  int prop = 0;   /* change propagation indicator */
  assert(Lownode);
  assert(aa.Lownode);
  assert(aa.size() == Size);
  STATUS::lud.start();
  for (int mm = 1;   mm <= aa.size();   ++mm){
    assert(aa.lownode(mm) == Lownode[mm]);
    int bn = Lownode[mm];
    if (aa.ischanged(mm)  ||  bn <= prop){
      aa.setchanged(mm, false);
      prop = mm;
      if (bn < mm){
	prop = mm;
	u(bn,mm) = aa.u(bn,mm) / d(bn,bn);
	for (int ii = bn+1;  ii<mm;  ii++){
	  /* u(ii,mm) = (aa.u(ii,mm) - dot(ii,mm,ii)) / d(ii,ii); */
	  SubtractDotProduct(ii,mm,ii,aa.u(ii,mm)) /= d(ii,ii);
	}
	l(mm,bn) = aa.l(mm,bn);
	for (int jj = bn+1;  jj<mm;  jj++){
	  /* l(mm,jj) = aa.l(mm,jj) - dot(mm,jj,jj); */
	  SubtractDotProduct(mm,jj,jj,aa.l(mm,jj));
	}
	/* jj == mm */{
	  /* d(mm,mm) = aa.d(mm,mm) - dot(mm,mm,mm); then test */
	  if (SubtractDotProduct(mm,mm,mm,aa.d(mm,mm)) == 0.){
	    error(bWARNING, "open circuit: internal node %u\n", mm);
	    d(mm,mm) = MinPivot;
	  }
	}
      }else{    /* bn == mm */
	d(mm,mm) = aa.d(mm,mm);
	if (d(mm,mm)==0.){
	  d(mm,mm) = MinPivot;
	}
      }
    }
  }
  STATUS::lud.stop();
  return *this;
}
/*--------------------------------------------------------------------------*/
template <class T>
BSMATRIX<T> & BSMATRIX<T>::lu_full(CONST BSMATRIX<T> & aa)
{
  assert(Lownode);
  assert(aa.Lownode);
  assert(aa.size() == Size);
  STATUS::lud.start();
  for (int mm = 1;   mm <= Size;   ++mm){
    assert(aa.lownode(mm) == Lownode[mm]);
    int bn = Lownode[mm];
    aa.setchanged(mm, false);
    if (bn < mm){
      u(bn,mm) = aa.u(bn,mm) / d(bn,bn);
      for (int ii = bn+1;  ii<mm;  ii++){
	/* u(ii,mm) = (aa.u(ii,mm) - dot(ii,mm,ii)) / d(ii,ii); */
	SubtractDotProduct(ii,mm,ii,aa.u(ii,mm)) /= d(ii,ii);
      }
      l(mm,bn) = aa.l(mm,bn);
      for (int jj = bn+1;  jj<mm;  jj++){
	/* l(mm,jj) = aa.l(mm,jj) - dot(mm,jj,jj); */
	SubtractDotProduct(mm,jj,jj,aa.l(mm,jj));
      }
      /* jj == mm */{
	/* d(mm,mm) = aa.d(mm,mm) - dot(mm,mm,mm); then test */
	if (SubtractDotProduct(mm,mm,mm,aa.d(mm,mm)) == 0.){
	  error(bWARNING, "open circuit: internal node %u\n", mm);
	  d(mm,mm) = MinPivot;
	}
      }
    }else{    /* bn == mm */
      d(mm,mm) = aa.d(mm,mm);
      if (d(mm,mm)==0.){
	d(mm,mm) = MinPivot;
      }
    }
  }
  STATUS::lud.stop();
  return *this;
}
/*--------------------------------------------------------------------------*/
template <class T>
BSMATRIX<T> & BSMATRIX<T>::lu_decomp(CONST BSMATRIX<T> & aa, bool dopartial)
{
  if (dopartial){
    return lu_partial(aa);
  }else{
    return lu_full(aa);
  }
}
/*--------------------------------------------------------------------------*/
template <class T>
BSMATRIX<T> & BSMATRIX<T>::lu_decomp()
{
  assert(Lownode);
  STATUS::lud.start();
  for (int mm = 1;   mm <= Size;   ++mm){
    int bn = Lownode[mm];
    if (bn < mm){
      u(bn,mm) /= d(bn,bn);
      for (int ii =bn+1;  ii<mm;  ii++){
	/* (m(ii,mm) -= dot(ii,mm,ii)) /= d(ii,ii); */
	SubtractDotProduct(ii,mm,ii) /= d(ii,ii);
      }
      for (int jj = bn+1;  jj<mm;  jj++){
	/* m(mm,jj) -= dot(mm,jj,jj); */
	SubtractDotProduct(mm,jj,jj);
      }
      /* jj == mm */{
	/* m(mm,mm) -= dot(mm,mm,mm); then test */
	if (SubtractDotProduct(mm,mm,mm) == 0.){
	  error(bWARNING, "open circuit: node %u\n", mm);
	  d(mm,mm) = MinPivot;
	}
      }
    }else{    /* bn == mm */
      if (d(mm,mm)==0.){
	d(mm,mm) = MinPivot;
      }
    }
  }
  STATUS::lud.stop();
  return *this;
}
/*--------------------------------------------------------------------------*/
/* fbsub: forward and back sub, shared storage
 * v = right side vector, changed in place to solution vector
 */
template <class T>
BSMATRIX<T> & BSMATRIX<T>::fbsub(T* v)
{
  assert(Lownode);
  assert(v);
  STATUS::back.start();
  {for (int ii = 1; ii <= Size; ++ii){	    /* forward substitution */
    for (int jj = Lownode[ii]; jj < ii; ++jj){
      v[ii] -= l(ii,jj) * v[jj];
    }
    v[ii] /= d(ii,ii);
  }}
  for (int jj = Size; jj > 1; --jj){	    /* back substitution    */
    for (int ii = Lownode[jj]; ii < jj; ++ii){
      v[ii] -= u(ii,jj) * v[jj];
    }
  }
  STATUS::back.stop();
  return *this;
}
/*--------------------------------------------------------------------------*/
/* fbsub: forward and back sub, separate storage
 * b = right side vector
 * c = intermediate vector after fwd sub
 * x = solution vector
 */
template <class T>
BSMATRIX<T> & BSMATRIX<T>::fbsub(T* x, const T* b, T* c)
{
  assert(Lownode);
  assert(x);
  assert(b);
  assert(c);
  STATUS::back.start();
  if (!c){
    c = x;
    if (!b)
      b = x;
  }
  {for (int ii = 1; ii <= Size; ++ii){	    /* forward substitution */
    c[ii] = b[ii];
    for (int jj = Lownode[ii]; jj < ii; ++jj){
      c[ii] -= l(ii,jj) * c[jj];
    }
    c[ii] /= d(ii,ii);
  }}
  for (int ii = Size; ii >= 1; --ii){
    x[ii] = c[ii];
    nstat[ii].aiter = STATUS::iter[iTOTAL];
  }
  for (int jj = Size; jj > 1; --jj){	    /* back substitution    */
    for (int ii = Lownode[jj]; ii < jj; ++ii){
      x[ii] -= u(ii,jj) * x[jj];
    }
  }
  STATUS::back.stop();
  return *this;
}
/*--------------------------------------------------------------------------*/
#if defined(MANUAL_TEMPLATES)
  template class BSMATRIX<double>;
  template class BSMATRIX<COMPLEX>;
#endif
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
