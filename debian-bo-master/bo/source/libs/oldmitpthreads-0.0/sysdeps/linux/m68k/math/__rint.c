#define	FUNC	__rint
#define	OP	intr
#include <acos.c>

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__rint, rint);
#endif
