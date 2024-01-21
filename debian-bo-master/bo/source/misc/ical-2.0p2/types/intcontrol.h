/* Copyright (c) 1995 by Sanjay Ghemawat */
#ifndef _INTCONTROL_H
#define _INTCONTROL_H

// Control file for integer sets.
// XXX To save space we outlaw INT_MIN and INT_MIN+1 as valid elements

#include "config.h"

#ifdef HAVE_LIMITS_H
#include <limits.h>
static const int empty_int = INT_MIN;
static const int obsolete_int = INT_MIN + 1;
#else
static const int empty_int = -2147483648;
static const int obsolete_int = -2147483647;
#endif

class IntControl {
  public:
    static int   is_full(int);
    static int   is_empty(int);
    static int   is_obsolete(int);
    static void  make_empty(int&);
    static void  make_obsolete(int&);
    static int   hash(int);
    static int   equal(int, int);
};

inline int   IntControl::is_full(int v) {return (v > obsolete_int);}
inline int   IntControl::is_empty(int v) {return(v==empty_int);}
inline int   IntControl::is_obsolete(int v) {return(v==obsolete_int);}
inline void  IntControl::make_empty(int& v) {v = empty_int;}
inline void  IntControl::make_obsolete(int& v) {v = obsolete_int;}
inline int   IntControl::hash(int v) {return v;}
inline int   IntControl::equal(int a, int b) {return (a == b);}

#endif /* _INTCONTROL_H */
