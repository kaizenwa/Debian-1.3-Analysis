#define	FUNC	exp
#define	OP	etox
#include <acos.c>

double pow10(double __y)
{
    double __result;
    __asm("ftentox%.x %1, %0" : "=f" (__result) : "f" (__y));

    return __result;
}

double pow2(double __y)
{
    double __result;
    __asm("ftwotox%.x %1, %0" : "=f" (__result) : "f" (__y));

    return __result;
}
