/* certain braindamaged environments don't define jmp_buf as an array, so... */

struct Jbwrap {
	jmp_buf j;
};

extern Jbwrap slowbuf; /* for getting out of interrupts while performing slow i/o on BSD */

#ifndef __linux__
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);
#endif /* not __linux__ */
