/*
 *
 * CopyRight Colten Edwards Oct 96
 *
 */

#ifndef _IRCTCL_H
#define _IRCTCL_H

int handle_dcc_bot _((int, char *));
int handle_tcl_chan _((int, char *, char *, char *));

typedef struct {
	char *name;
	int  (*func) ();
	int access;
	char *help;
} cmd_t;

extern cmd_t C_msg[];
extern cmd_t C_dcc[];
int check_tcl_dcc _((char *, char *, char *, int, DCC_list *));

#ifdef WANT_TCL


#include <tcl.h>
extern Tcl_Interp *interp;
void check_tcl_tand _((char *, char *, char *));
void check_tcl_msgm _((char *, char *, char *, char *, char *));
void check_tcl_pubm _((char *, char *, char *, char *));
int check_tcl_pub _((char *, char *, char *, char *));
int check_tcl_msg _((char *, char *, char *, char *, char *));
int check_tcl_ctcp _((char *, char *, char *, char *, char *, char *));


void check_tcl_join _((char *,char *, char *, char *));
void check_tcl_raw _((char *));
void check_tcl_rejoin _((char *,char *,char *,char *));
void check_tcl_split _((char *,char *,char *,char *));
void check_tcl_chat _((char *,int, char *));
int check_tcl_ctcr _((char *,char *,char *,char *,char *,char *));
void check_tcl_mode _((char *,char *,char *,char *,char *));
void check_tcl_kick _((char *,char *,char *,char *,char *,char *));
void check_tcl_nick _((char *,char *,char *,char *, char *));
void check_tcl_topc _((char *,char *,char *,char *,char *));
void check_tcl_sign _((char *, char *,char *, char *,char *));
void check_tcl_part _((char *, char *, char *, char *));
void check_tcl_input _((char *));
void check_timers _((void));
void check_utimers _((void));
void tcl_list_timer _((Tcl_Interp *, TimerList *));

void tcl_init _((void));
#endif

#endif
