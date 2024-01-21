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

#include "irc_std.h"
#include <sys/types.h>

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
	void	start_process _((char *, char *, char *, char *, unsigned int, int));
	void	kill_process _((int, int));
                
	extern	char *signals[];
	
#endif /* __exec_h_ */
