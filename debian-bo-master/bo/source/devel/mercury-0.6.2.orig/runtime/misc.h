/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	AUX_H
#define	AUX_H

#ifndef SPEED

extern	void	mkframe_msg(void);
extern	void	modframe_msg(void);
extern	void	succeed_msg(void);
extern	void	succeeddiscard_msg(void);
extern	void	fail_msg(void);
extern	void	redo_msg(void);
extern	void	call_msg(/* const */ Code *proc, /* const */ Code *succcont);
extern	void	tailcall_msg(/* const */ Code *proc);
extern	void	proceed_msg(void);
extern	void	cr1_msg(Word val0, const Word *addr);
extern	void	cr2_msg(Word val0, Word val1, const Word *addr);
extern	void	incr_hp_msg(Word val, const Word *addr);
extern	void	incr_sp_msg(Word val, const Word *addr);
extern	void	decr_sp_msg(Word val, const Word *addr);
extern	void	push_msg(Word val, const Word *addr);
extern	void	pop_msg(Word val, const Word *addr);
#endif

#if !defined(SPEED) || defined(DEBUG_GOTOS)

extern	void	goto_msg(/* const */ Code *addr);
extern	void	reg_msg(void);

#endif

#ifndef SPEED

extern	void	printint(Word n);
extern	void	printstring(const char *s);
extern	void	printheap(const Word *h);
extern	void	printdetstack(const Word *s);
extern	void	printnondstack(const Word *s);
extern	void	dumpframe(/* const */ Word *);
extern	void	dumpnondstack(void);
extern	void	printlist(Word p);
extern	void	printframe(const char *);
extern	void	printregs(const char *msg);

#endif

extern	void	printlabel(/* const */ Code *w);

#if 0 /* this used to be used for some hand-coded examples */
extern	Word	do_mklist(int start, int len);
#endif

#if __GNUC__
	#define NO_RETURN __attribute__((noreturn))
#else
	#define NO_RETURN
#endif
extern	void	fatal_error(const char *msg) NO_RETURN;

void *checked_malloc(size_t n);

#endif
