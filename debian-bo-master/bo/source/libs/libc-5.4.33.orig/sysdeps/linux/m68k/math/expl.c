#define	FUNC	expl
#include <acosl.c>

long double pow10l(long double __y)
{
    long double __result;
    __asm("ftentox%.x %1, %0" : "=f" (__result) : "f" (__y));

    return __result;
}

long double pow2l(long double __y)
{
    long double __result;
    __asm("ftwotox%.x %1, %0" : "=f" (__result) : "f" (__y));

    return __result;
}
