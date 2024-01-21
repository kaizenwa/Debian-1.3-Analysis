#include <signal.h>

/* Rq: sigset_t is currently an int.  */

#undef sigmask
#define	sigmask(n)		((unsigned int)1 << (((n) - 1) & (32 - 1)))

#if 0
#define	sigemptyset(s)		(*(s) = nullsmask)
#define	sigfillset(s)		(*(s) = fillset)
#define	sigaddset(s, n)		((s)->__sigbits[sigword(n)] |= sigmask(n))
#define	sigdelset(s, n)		((s)->__sigbits[sigword(n)] &= ~sigmask(n))
#define	sigismember(s, n)	(sigmask(n) & (s)->__sigbits[sigword(n)])
#define	sigisempty(s)		(!(((s)->__sigbits[0]) | ((s)->__sigbits[1])))
#endif

int
sigaddset (sigset_t *s, int n)
{
 *s |= sigmask(n);
 return 0;
}

int
sigdelset (sigset_t *s, int n)
{
 *s  &= ~sigmask(n);
 return 0;
}

int
sigemptyset (sigset_t *s)
{
 *s = 0;
 return 0;
}

int
sigfillset (sigset_t *s)
{
 *s = ~0;
 return 0;
}

int
sigismember (const sigset_t *s, int n)
{
 return sigmask(n) & *s;
}
