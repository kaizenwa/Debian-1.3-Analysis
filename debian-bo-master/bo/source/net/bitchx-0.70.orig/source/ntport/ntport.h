//
// Copyright(c) 1996 Amol Deshpande
// amold@microsoft.com
// Redistribution in source or binary form is permitted as long as you 
// retain this notice in the distribution
//
// There is no warranty, implied or otherwise, with this software. Use at your
// own risk.
// 
// See LICENSE.TXT for complete redistribution/usage conditions
//
// The original ircii code is under the copyright indicated in the source.
// 
// And finally,
// Microsoft Corporation has nothing to do with this code. 
//
// globals, typedefs and other misc stuff
//
#ifndef NTPORT_H
#define NTPORT_H

#define WIN32_LEAN_AND_MEAN
#ifndef WINSOCK_2
#include <winsock.h>
#else
#include <winsock2.h>
#endif WINSOCK_2
#include <stdlib.h>
#include <direct.h>
#include <io.h>
#include <dirent.h>

#define strlen lstrlen
#define strcmp lstrcmp
#define strcat lstrcat
#define strcpy lstrcpy
#define index strchr
#define rindex strrchr
#define bzero(a,b) memset((a),0,(b))
#define bcopy(a,b,c) memcpy((b),(a),(c))
#define access nt_access
#define pipe mypipe
#define read(a,b,c) nt_read((a),(b),(c))
#define close(a) ***CHANGE-TO-CLOSESOCKET-OR_CLOSE***((a))
#define open(a,b) nt_open((a),(b))
#define write(a,b,c) nt_write((a),(b),(c))

#define strerror(a) mystrerror((a))

#define getcwd(a,b) forward_slash_get_cwd((a),(b))

#define USE(x) ((void)x)

#define MY_SIGNAL 

#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
#define FDSETSIZE FD_SETSIZE

#undef ZCAT
#define NEED_FCHMOD



typedef unsigned long pid_t;
typedef unsigned long u_32int_t;

extern unsigned long getuid(void);
extern unsigned long getpid(void);
extern unsigned long getppid(void);
extern int nt_access(char *,int);
extern int mypipe(int []);
extern int nt_read(int,unsigned char*,int);
extern int nt_write(void*,char*,int);
extern int nt_open(char *, int);
extern unsigned long nt_pipe_has_data(int);

extern int nt_ClearEOL(void);
extern int nt_cursor_right(void);
extern int nt_cursor_left(void);
extern int nt_getsize(int * , int * ) ;

extern void nt_move_cursor(int,int);
extern void nt_put_cr(void);
extern void nt_put_nl(void);
extern void NT_ClearScreen(void);
extern void nt_term_standout_on(void) ;
extern void nt_term_standout_off(void) ;
extern void nt_term_bold_on(void) ;
extern void nt_term_bold_off(void) ;
extern void nt_term_init(void);
extern int nt_scroll(int,int,int);


extern void release_lock();
extern void get_lock();
#define RELEASE_LOCK() release_lock() //ReleaseMutex(io_sem)
#define GET_LOCK() get_lock() //assert(WAIT_FAILED!=WaitForSingleObject(io_sem,INFINITE))

extern void start_server_and_dcc_threads(int);
extern int non_thread_check_desc(void);
extern int WaitConsole(struct timeval *, HANDLE);

extern char* forward_slash_get_cwd(char * ,int) ;

extern char * mystrerror(int);

#ifdef DPRINTF
extern void dprintf(char*,...);
#else
#define dprintf (void)
#endif DPRINTF

extern void make_err_str(unsigned int ,char *,int ) ;
extern void do_nt_stop_irc(void);

extern void init_io_lock(void);

extern void say(char *,...);
extern char *get_string_var(int);
extern char *next_arg(char *,char **);
extern void add_process(char *,char*,int,int,int,int,char*,char*,unsigned
int);
extern void refresh_screen(char*,void *);


extern int wait_any_child(HANDLE*);
extern	void add_to_proc_array(HANDLE);
extern void init_proc_critter(void);


extern HANDLE nt_win_create_new_child(void*);
extern int nt_win_init(void) ;

// globals
extern DWORD gdwPlatform;
extern char empty_string[];
extern unsigned short giswin;


/* 
 * These prototypes would be in standard unix headers 
*/
extern int scandir(char *, struct dirent ***List, 
    int			 (*Selector)(),
    int			 (*Sorter)()
	);


extern void tputs (char *__string, int __nlines, void(*));//outfuntype);
extern char *tgoto (char *__cstring, int __hpos, int __vpos);

//
// This will be gotten rid of someday
#if 0
extern char *tparam (char *__ctlstring, void *__buffer, int __size,
...);

extern char *UP;
extern char *BC;

#endif 0

#endif NTPORT_H
