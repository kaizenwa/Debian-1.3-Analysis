/*$Id: l_astack.h,v 11.38 96/03/24 18:01:14 al Exp $ -*- C++ -*-
 * An array based stack class
 */
#ifndef M_ASTACK_H
#define M_ASTACK_H
#include "md_bool.h"
/*--------------------------------------------------------------------------*/
template <class T>
class ASTACK {
private:
  T	    *s;		// storage array, allocated with "new"
  int	    size;	// how big is it (capacity)
  int	    cursor;	// the first empty slot
public:
  	     ASTACK(int sz=100)	{s = new T[sz]; assert(s); cursor=0; size=sz;}
	     ~ASTACK()		{delete [] s;}
  int        Count()const	{return cursor;}
  int        Capacity()const	{return size;}
  int	     SpaceLeft()const	{return Capacity()-Count();}
  ASTACK<T>& Clear()		{cursor = 0; return *this;}
  bool       IsEmpty()const	{return Count()==0;}
  bool       IsFull()const	{return SpaceLeft()==0;}
  ASTACK<T>& Push(T x)	     {assert(!IsFull()); s[cursor++]=x; return *this;}
  ASTACK<T>& operator<<(T x)	{return Push(x);}
  ASTACK<T>& DelTop()		{assert(!IsEmpty()); --cursor; return *this;}
  T	     Pop()		{assert(!IsEmpty()); return s[--cursor];}
  ASTACK<T>& operator>>(T& x)	{x = Pop(); return *this;}
  T	     Top()		{assert(!IsEmpty()); return s[cursor-1];}
  ASTACK<T>& Resize(int sz = 0);
};
/*--------------------------------------------------------------------------*/
template <class T>
inline ASTACK<T>& ASTACK<T>::Resize(int sz)
{
  if (sz == 0){
    sz = size*2;
  }
  if (sz > size){
    T* ns = new T[sz];
    assert(ns);
    for (int i=0; i<cursor; ++i){
      ns[i] = s[i];
    }
    delete [] s;
    s = ns;
    size = sz;
  }
  return *this;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
