/*
 * exec.h: header for exec.c 
 *
 * Written By Michael Sandrof
 *
 * Copyright(c) 1990 
 *
 * See the COPYRIGHT file, or do a HELP IRCII COPYRIGHT 
 *
 * @(#)$Id: exec.h,v 1.19 1995/09/03 13:45:16 mrg Exp $
 */

#ifndef __exec_h_
#define __exec_h_

#include <sys/types.h>

#if defined(NeXT)		/* lameness for configure/NeXT -phone */
# if !defined(_POSIX_SOURCE) && !defined(BSDWAIT)
#  define BSDWAIT
# endif /* !_POSIX_SOURCE && !BSDWAIT */
#else /* !NeXT */
# ifndef WAITSTUFF_DECLARED
#  ifdef BSDWAIT
#   ifndef WAIT3_DECLARED
struct rusage;
union wait;
int   wait3 _((union wait *, int, struct rusage *));
#   endif /* WAIT3_DECLARED */
#  else /* BSDWAIT */
#   ifndef WAITPID_DECLARED
short waitpid _((int, int *, int));
#   endif /* WAITPID_DECLARED */
#  endif /* BSDWAIT */
# endif /* WAITSTUFF_DECLARED */
#endif /* NeXT */

#ifndef WTERMSIG
# ifndef BSDWAIT /* if wait is NOT a union */
#  define WTERMSIG(status) ((status) & 0177)
# else
#  define WTERMSIG(status) status.w_T.w_Termsig
# endif
#endif

#ifndef WSTOPSIG
# ifndef BSDWAIT
#  define WSTOPSIG(status) ((status) >> 8)
# else
#  define WSTOPSIG(status) status.w_S.w_Stopsig
# endif
#endif

#ifndef WEXITSTATUS
# ifndef BSDWAIT
#  define WEXITSTATUS(status) ((status) & 0xff00) >> 8		/* dgux 5.4.1 */
# else
#  define WEXITSTATUS(status) status.w_T.w_Retcode
# endif
#endif

	int	get_child_exit _((int));
	int	check_wait_status _((int));
	void	check_process_limits _((void));
	void	do_processes _((fd_set *));
	void	set_process_bits _((fd_set *));
	int	text_to_process _((int, char *, int));
	void	clean_up_processes _((void));
	int	is_process _((char *));
	int	get_process_index _((char **));
	void	exec_server_delete _((int));
	int	is_process_running _((int));
	void	add_process_wait _((int, char *));
	void	set_wait_process _((int));
	void	close_all_exec _((void));
	int	logical_to_index _((char *));
	void	execcmd _((char *, char *, char *));

extern	char	*signals[];

#endif /* __exec_h_ */
