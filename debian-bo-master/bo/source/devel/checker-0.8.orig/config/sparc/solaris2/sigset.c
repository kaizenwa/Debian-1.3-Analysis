#include <signal.h>

#define	sigmask(n)		((unsigned int)1 << (((n) - 1) & (32 - 1)))
#define	sigword(n)		(((unsigned int)((n) - 1))>>5)

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
 s->__sigbits[sigword(n)] |= sigmask(n);
 return 0;
}

int
sigdelset (sigset_t *s, int n)
{
 s->__sigbits[sigword(n)] &= ~sigmask(n);
 return 0;
}

int
sigemptyset (sigset_t *s)
{
 s->__sigbits[0] = s->__sigbits[1] = s->__sigbits[2] = s->__sigbits[3] = 0;
 return 0;
}

int
sigfillset (sigset_t *s)
{
 s->__sigbits[0] = s->__sigbits[1] = s->__sigbits[2] = s->__sigbits[3] = ~0;
 return 0;
}

int
sigismember (const sigset_t *s, int n)
{
 return sigmask(n) & s->__sigbits[sigword(n)];
}
