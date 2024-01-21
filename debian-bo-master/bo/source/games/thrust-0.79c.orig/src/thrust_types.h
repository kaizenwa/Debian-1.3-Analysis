
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifndef THRUST_TYPES_H
#define THRUST_TYPES_H

#define incr(x,max,return) \
{ \
    (x)++; \
    if((x)>=(max)) \
      (x)=(return); \
}
#define decr(x,min,return) \
{ \
    (x)--; \
    if((x)<(min)) \
      (x)=(return)-1; \
}

#define min(x,y) \
 (((x)<(y))?(x):(y))

#define max(x,y) \
 (((x)<(y))?(y):(x))

typedef unsigned char byte;
typedef unsigned int word;
typedef unsigned long dword;

#endif /* THRUST_TYPES_H */
