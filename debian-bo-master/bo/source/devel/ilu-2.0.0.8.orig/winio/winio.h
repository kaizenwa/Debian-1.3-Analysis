/*
WINIO.H
Stdio (e.g. printf) functionality for Windows - definition
Dave Maxey - 1991

(Quickly) Ported to WIN32 / WIN16 by Dan Larner - 1995
*/

#ifdef __cplusplus
extern "C" {
#endif
/* ==== STDIO.H for Windows ==== */
#include <stdarg.h>
#include <stdio.h>

#include "winiodef.h"


/* ==== Extensions ==== */

/* winio_init() must be called before any of the above listed
functions to init the i/o window. Similar arguments to WinMain(), but
we drop the cmdline pointer but add a bufsize parameter (unsigned) -
0 means default (8k). */
int winio_init(HANDLE, HANDLE, int, unsigned);

/* Makes the window inactive, and allows the user to view and play with
it until double- clicking on the system menu to close it. NEVER RETURNS. */
void winio_end(void);

/* closes the window immediately and frees up buffers */
void winio_close(void);

/* to override default title of "Console I/O" */
void winio_settitle(char *);

/* May be SYSTEM_FIXED_FONT (default), ANSI_FIXED_FONT, or OEM_FIXED_FONT */
BOOL winio_setfont(WORD);

/* To turn automatic updating of window off and on */
BOOL winio_setpaint(BOOL);

/* clear out the contents of the buffer and start over fresh */
void winio_clear(void);

/* should be used to release cpu in spells between I/O calls. A
WM_QUIT message received by it will exit the application. If that is
a problem, use the winio_onclose function below */
void winio_yield(void);

/* Returns the underlying Windows window handle to WINIO window */
HWND winio_hwnd(void);

/* ==== User definable exit routine ==== */

typedef void (* DESTROY_FUNC)(void);

/* Optional notification function; without it, there is no way for your
application to know if the user has double-clicked the system menu box */
void winio_onclose(DESTROY_FUNC);

/* ==== Utility function built on message box ==== */

BOOL winio_warn(BOOL, const char *, ...);


/* sets up the argc and argv used to call a main()
	p_argc is the address of argc
	argv is the argument array  (should declared in caller as
		char* argv [<maximum number of args + 2>]; 
		(+ 2 since argv[0] is the program name and the last argv will be NULLed)
	i_arvdimension is the dimension of argv, i. e. <maximum number of args + 2>
	pc_title is the title to give the window
	pc_progname is the program name (i.e. what argv[0] will be set to)
	pc_arg_space is where the actual argument strings can be stored and
		should be declared in the caller as
		char szArguments[<large enough to hold all the argument strings>]; 
 	Other args are what the apps winmain receives

	Note that if the command line is not null and the first char of it is
	not null, then is is assumed that the working directory is already established
	and that arguments should be taken from the command line rather than prompted for.
	Note that pc_arg_space should still be provided as this routine preseves the
	command line for (potentially) other uses by the application.

	See example at end of the winio.c file
*/
int winio_setmain(HANDLE hInstance, HANDLE hPrevInstance, 
    LPSTR lpCmdLine, int nCmdShow,  
    int* p_argc, char * argv[], int i_arvdimension,
    char* pc_title, char* pc_progname, char* pc_arg_space);


/* creates an auxillary window for a Windows application,
   that receives all stdio output. 
   First 4 arguments are as in winio_init, 
   pc_title is as in winio_settitle */
 
int winio_console(HANDLE hInstance, HANDLE hPrevInstance,
            int nCmdShow, unsigned wBufSize, char* pc_title);


#ifdef __cplusplus
}
#endif
