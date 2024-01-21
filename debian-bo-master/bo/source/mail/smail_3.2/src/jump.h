/* @(#) jump.h,v 1.2 1992/09/20 18:45:12 tron Exp -  */

/*
 *    Copyright (C) 1992  Chip Salzenberg
 *
 * Support common to the use of setjmp/longjmp and signals.
 */

#ifndef JUMP_H
#define JUMP_H

#include <setjmp.h>
#include <signal.h>

#if defined(POSIX_OS) && !defined(NO_SIGSETJMP)

typedef struct {
  struct sigaction act;
} JUMPSIG;

#define JUMP_SETSIG(SIG,FUNC,JS)	\
   {					\
     struct sigaction act, oldact;	\
     act.sa_handler = FUNC;		\
     sigemptyset(&act.sa_mask);		\
     sigaddset(&act.sa_mask,SIG);	\
     act.sa_flags = 0;			\
     sigaction(SIG,&act,&oldact);	\
     (JS)->act = oldact;		\
   }
#define JUMP_CLEARSIG(SIG,JS)		\
   {					\
     struct sigaction oldact;		\
     oldact = (JS)->act;		\
     sigaction(SIG,&oldact,(struct sigaction *)NULL); \
   }

#define JUMP_SETJMP(BUF)		sigsetjmp(BUF, 1)
#define JUMP_LONGJMP(BUF,VAL)		siglongjmp(BUF, VAL)
#define JUMP_ENVBUF			sigjmp_buf

#else /* !POSIX_OS || NO_SIGSETJMP */

typedef struct {
    void (*func)();
} JUMPSIG;

#define JUMP_SETSIG(SIG,FUNC,JS)	{ (JS)->func = signal(SIG,FUNC); }
#define JUMP_CLEARSIG(SIG,JS)		{ signal(SIG,(JS)->func); }

#define JUMP_SETJMP(BUF)		setjmp(BUF)
#define JUMP_LONGJMP(BUF,VAL)		longjmp(BUF, VAL)
#define JUMP_ENVBUF			jmp_buf

#endif /* POSIX_OS && !NO_SIGSETJMP */

#endif /* !JUMP_H */
