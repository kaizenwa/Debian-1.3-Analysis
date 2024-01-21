/*$Id: l_compar.h,v 11.22 96/02/18 11:46:26 al Exp $ -*- C++ -*-
 * inline utility functions, general purpose
 */
#include "md_bool.h"
#ifndef L_UTIL_H
#define L_UTIL_H
/*--------------------------------------------------------------------------*/
#undef MAX
#undef max
inline double max(double x, double y)
   {return (x>y) ? x : y;}
inline int max(int x, int y)
   {return (x>y) ? x : y;}

#undef MIN
#undef min
inline double min(double x, double y)
   {return (x<y) ? x : y;}
inline int min(int x, int y)
   {return (x<y) ? x : y;}

inline bool uporder(double a, double b, double c)
   {return (a<=b) && (b<=c);}

inline bool inorder(double a, double b, double c)
   {return uporder(a,b,c) || uporder(c,b,a);}

inline double torange(double a, double b, double c)
   {return min(max(a,b),c);}

inline char to_upper(char c)
   {return ((islower(c))?toupper(c):c);}

inline char to_lower(char c)
   {return ((isupper(c))?tolower(c):c);}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
#endif
