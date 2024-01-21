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

typedef struct SERV_ADDR SERV_ADDR;

#define NNTP_PORT	119
#define FTP_PORT	 21

extern int		 do_wait(int*, int, void (*)(void*), void*);
extern int		 would_block(int, int);
extern int		 timed_out(int);
extern char		*error_string(int);
extern void		 abort_callback(Widget, XtPointer, XtPointer);

extern SERV_ADDR	*get_host(char*, unsigned short, int);
extern int		 open_socket(void);
extern int		 connect_socket(int, SERV_ADDR*);
extern int		 open_duplex(int*);
extern int		 bind_and_listen(int);
extern SERV_ADDR	*get_sock_name(int);
extern int		 do_accept(int*);
extern void		 print_addr_ftp(SERV_ADDR*, char*);

extern char		*get_mailhostname(void);

extern void		 sigusr1_handler(int);
