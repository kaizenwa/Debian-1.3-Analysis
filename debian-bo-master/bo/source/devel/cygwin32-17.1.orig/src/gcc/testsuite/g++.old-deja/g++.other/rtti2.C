// test of rtti of single inheritance and multiple inheritance classes
// Special g++ Options: -frtti

#include <std/typeinfo.h>

extern "C" {
  int printf(const char *, ...);
  void exit(int);
  void *__dynamic_cast (const type_info& from, const type_info& to,
			int require_public, void *address);
}

class X {
 public:
  int xi;
};

class Y : public X {
  short ys;
};

class Z : public Y {
  int zi;
};

Z z;
Y y;
Y *yp = &z;
X *xp = &z;
Z *zp = &z;

class A {
 public:
  int Ai;
};

class B {
 public:
  int Bi;
};

class D : public A, public B {
  int Di;
};

/*
class E : public D, public B {
  int Ei;
};
*/
class E {
  int Ei;
};

class F : public E, public D {
  int Fi;
};

D d;
A *ap = &d;
B *bp = &d;
F f;
A *aap = &f;
D *dp = &f;
B *bbp = dp;

void *vp = zp;

void error  (int i)
{
  exit(i);
}

int main ()
{
  if (typeid(z) != typeid(Z)) error(1);
  if (typeid(*yp) == typeid(Z)) error(2);
  if (typeid(*yp) == typeid(*zp)) error(3);
  if (typeid(xp) == typeid(yp)) error(4);

  xp = (X *)&y;
  if (typeid(*xp) == typeid(*yp)) error(5);
  if (typeid(*xp) == typeid(Y)) error(6);
  
  if (__dynamic_cast (typeid(*zp), typeid(*yp), 1, vp) != zp) error(12);

  if (typeid(*ap) == typeid(*bp)) error (31);
  if (typeid(*ap) == typeid(D)) error(32);

  if (typeid(*aap) == typeid(*bbp)) error(33);
  if (typeid(*dp) == typeid(*aap)) error(34);
  vp = dp;
  if ((vp=__dynamic_cast (typeid(*dp), typeid(*bbp), 1, vp)) == 0) error(41);
  else if (dp == (D *)vp) error(42);
  if ((vp=__dynamic_cast (typeid(*dp), typeid(*bp), 1, vp)) == 0) error(43);
  else if (dp == (D *)vp) error(44);
  if ((vp=__dynamic_cast (typeid(*dp), typeid(B), 1, vp)) == 0) error(45);
  else if (dp == (D *)vp) error(46);
}
