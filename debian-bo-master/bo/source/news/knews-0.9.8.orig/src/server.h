/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#ifndef SERVER_H
#define SERVER_H

typedef struct SERVER	SERVER;
typedef void		(*QuitFunc)(void*);
struct SERV_ADDR;

extern SERVER	*server_create(int);
extern void	 server_free(SERVER*);
extern void	 server_close(SERVER*);
extern int	 server_open(SERVER*, struct SERV_ADDR*, int);
extern int	 server_fork(SERVER*, char*, int);
extern long	 server_write_raw(SERVER*, char*, long);
extern int	 server_write(SERVER*, char*);
extern long	 server_read_raw(SERVER*);
extern char	*server_get_line(SERVER*);
extern char	*server_get_chunk(SERVER*);
extern char	*server_read(SERVER*);
extern char	*server_read_chunk(SERVER*);
extern char	*server_comm(SERVER*, char*, int);
extern int	 server_get_fd(SERVER*);
extern void	 server_set_fd(SERVER*, int);
extern void	 server_set_bs(SERVER*, FILE*);
extern void	 server_set_quit_func(SERVER*, QuitFunc);
extern QuitFunc	 server_get_quit_func(SERVER*);
extern int	 server_aborted(SERVER*);

extern void	 nntp_quit(void*);
extern void	 nntp_just_close(void*);

#endif /* SERVER_H */
