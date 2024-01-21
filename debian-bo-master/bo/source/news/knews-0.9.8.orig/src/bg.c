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
#include "global.h"
#include "bg.h"
#include "codes.h"
#include "resource.h"
#include "server.h"
#include "sysdeps.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ScrList.h"

typedef enum {
    BgStateDisconn,
    BgStateConnWait,
    BgStateSentModeReader,
    BgStateSentGroup,
    BgStateSentAuthUser,
    BgStateSentAuthPass,
    BgStateBusy,
    BgStateIdle
} BgState;

static SERVER		*bg_server = NULL;
static BgProc		bg_proc    = NULL;
static BgState		bg_state   = BgStateDisconn;
static GROUP 		*bg_group  = NULL;
static long		first      = 0;
static long		last       = 0;
static long		total      = 1;
static XtInputId	input_id   = 0;
static XtIntervalId	timeout_id = 0;
static unsigned long	timeout    = 0;

static int bg_start_connect(void);
static int bg_start_auth(void);

#define BACK_OFF	1.25
#define MIN_TIMEOUT	(      200ul)
#define MID_TIMEOUT	(   2*1000ul)
#define MAX_TIMEOUT	(5*60*1000ul)

#define MAX_PROCS	2
static BgProc	procs[MAX_PROCS] = {0, };
static int	n_procs = 0;

static void put_proc(BgProc proc)
{
    int	i;

    if (n_procs >= MAX_PROCS)
	return;

    for (i = 0 ; i < n_procs ; i++)
	if (procs[i] == proc)
	    return;

    procs[n_procs++] = proc;
}

static void set_timeout(void);

static void bg_schedule(void)
{
    int	i;

    bg_state = BgStateBusy;
    for (i = 0 ; i < n_procs ; i++) {
	bg_proc = procs[i];
	if (bg_proc(bg_server)) {
	    timeout /= BACK_OFF;
	    return;
	}
    }
    bg_state = BgStateIdle;
    bg_proc = NULL;
    timeout *= BACK_OFF;
    set_timeout();
}

static void timeout_callback(XtPointer client_data, XtIntervalId *id)
{
    timeout_id = 0;
    if (bg_state == BgStateIdle)
	bg_schedule();
}

static void remove_timeout(void)
{
    if (timeout_id)
	XtRemoveTimeOut(timeout_id);
    timeout_id = 0;
}

static void set_timeout(void)
{
    remove_timeout();

    if (timeout < MIN_TIMEOUT)
	timeout = MIN_TIMEOUT;
    else if (timeout > MAX_TIMEOUT) {
	if (bg_state == BgStateIdle) {
	    bg_shutdown();
	    set_message("Closed second connection to server.", False);
	    return;
	}
	timeout = MAX_TIMEOUT;
    }

    timeout_id = XtAppAddTimeOut(app_cont, timeout, timeout_callback, NULL);
}

static void bg_read_callback(XtPointer client_data, int *fd, XtInputId *id)
{
    char	message[128];
    char	*err;
    long	i;
    int		tmp, busy;

    i = server_read_raw(bg_server);
    if (i > 0) {
	char	*buffer;

	switch (bg_state) {
	case BgStateDisconn:
	    break;
	case BgStateConnWait:
	    buffer = server_get_line(bg_server);
	    if (!buffer)
		return;
	    if (atoi(buffer) != NNTP_OK_CANPOST &&
		atoi(buffer) != NNTP_OK_NOPOST) {
		if (strlen(buffer) > 80)
		    buffer[80] = '\0';
		sprintf(message, "Failed to open second connection, "
			"message from server is: %s", buffer);
		set_message(message, True);
		break;
	    }

	    busy = global.busy;
	    if (!busy)
		global.busy = True;
	    tmp = server_write(bg_server, "MODE READER\r\n");
	    if (!busy)
		global.busy = False;
	    if (tmp == 0) {
		bg_state = BgStateSentModeReader;
		return; /* OK */
	    }
	    set_message("Failed to open second connection to server!", True);
	    break;
	case BgStateSentModeReader:
	    buffer = server_get_line(bg_server);
	    if (!buffer)
		return;
	    if (atoi(buffer) == NNTP_ERR_NEED_AUTH && bg_start_auth())
		return;
	    bg_state = BgStateIdle;
	    bg_schedule();
	    return;
	case BgStateSentGroup:
	    buffer = server_get_line(bg_server);
	    if (!buffer)
		return;
	    if (atoi(buffer) == NNTP_ERR_NEED_AUTH && bg_start_auth())
		return;
	    if (sscanf(buffer, "%d%ld%ld%ld",
		       &tmp, &total, &first, &last) != 4 ||
		tmp != NNTP_OK_GROUP) {
		if (bg_group)
		    fprintf(stderr, "bg: Failed to enter %s:  %s!\n",
			    bg_group->name, buffer);
		bg_group = NULL;
	    }
	    bg_state = BgStateIdle;
	    bg_schedule();
	    return;
	case BgStateSentAuthUser:
	    buffer = server_get_line(bg_server);
	    if (!buffer)
		return;
	    if (atoi(buffer) != NNTP_CONT_AUTH)
		fprintf(stderr,
			"bg: Couldn't authenticate second "
			"connection: %s\n", buffer);
	    /* Still expecting reply to AUTHINFO PASS */
	    bg_state = BgStateSentAuthPass;
	    return;
	case BgStateSentAuthPass:
	    buffer = server_get_line(bg_server);
	    if (!buffer)
		return;
	    if (atoi(buffer) != NNTP_OK_AUTH) {
		fprintf(stderr,
			"bg: Couldn't authenticate second "
			"connection: %s\n", buffer);
		bg_group = NULL;
	    }
	    bg_state = BgStateIdle;
	    bg_schedule();
	    return;
	case BgStateBusy:
	    if (!bg_proc(bg_server)) {
		bg_proc = NULL;
		bg_state = BgStateIdle;
		if (timeout > MID_TIMEOUT)
		    timeout = MID_TIMEOUT;
		set_timeout();
	    }
	    return;
	case BgStateIdle:
	    break;
	}
    }

    err = NULL;
    if (i < 0) {
	if (errno == EINTR || would_block(server_get_fd(bg_server), errno))
	    return; /* Should never happen... */
	perror("knews: read");
	err = error_string(errno);
    }

    if (!err)
	err = "Broken pipe";
    sprintf(message, "Second connection to server: %s!", err);
    set_message(message, True);
    bg_shutdown();
    bg_start_connect();
}

static int bg_start_connect(void)
{
    int	tmp;

    if (!bg_server)
	bg_server = server_create(-1);

    set_message("Opening second connection to server...", False);
    if (global.nntp_server[0] != '#')
	tmp = server_open(bg_server, global.serv_addr, 0);
    else
	tmp = server_fork(bg_server, NULL, 0);

    if (tmp < 0) {
	set_message("Failed to open second connection to server!", True);
	server_close(bg_server);
	return False;
    }

    input_id = XtAppAddInput(app_cont, server_get_fd(bg_server),
			     (XtPointer)XtInputReadMask,
			     bg_read_callback, NULL);
    bg_state = BgStateConnWait;

    return True;
}

void bg_nudge(BgProc proc)
{
    put_proc(proc);

    switch (bg_state) {
    case BgStateDisconn:
	bg_start_connect();
	break;
    case BgStateIdle:
	if (timeout < MID_TIMEOUT)
	    timeout /= BACK_OFF;
	else {
	    timeout = MID_TIMEOUT;
	    set_timeout();
	}
	break;
    default:
	timeout /= BACK_OFF;
	break;
    }
}

void bg_shutdown(void)
{
    bg_group = NULL;
    if (bg_server)
	server_close(bg_server);
    bg_state = BgStateDisconn;
    if (input_id)
	XtRemoveInput(input_id);
    input_id = 0;
    remove_timeout();
    if (bg_proc)
	bg_proc(NULL);
    bg_proc = NULL;
}

GROUP *bg_in_group(long *totalp, long *firstp, long *lastp)
{
    if (totalp)
	*totalp = total;
    if (firstp)
	*firstp = first;
    if (lastp)
	*lastp = last;

    return bg_group;
}

void bg_start_group(GROUP *group)
{
    char	command[512];
    int		tmp, busy;

    if (bg_state != BgStateBusy)
	return;

    bg_group = group;
    bg_state = BgStateSentGroup;
    sprintf(command, "GROUP %s\r\n", group->name);
    busy = global.busy;
    if (!busy)
	global.busy = True;
    tmp = server_write(bg_server, command);
    if (!busy)
	global.busy = False;

    if (tmp < 0) {
	fprintf(stderr, "knews: bg failed to enter %s.\n", group->name);
	bg_state = BgStateIdle;
	bg_group = NULL;
	if (bg_proc)
	    bg_proc(NULL);
	bg_proc = NULL;
    }
}

static int bg_start_auth(void)
{
    char	command[1024];
    char	*auth_info_user = res_auth_info_user();
    char	*auth_info_pass = res_auth_info_pass();
    int		tmp, busy;

    if (!auth_info_user || !auth_info_pass)
	return False;

    if (strlen(auth_info_user) > 480 || strlen(auth_info_pass) > 480) {
	fputs("knews: authInfoUser or authInfoPass too long\n", stderr);
	return False;
    }

    sprintf(command, "AUTHINFO USER %s\r\nAUTHINFO PASS %s\r\n",
	    auth_info_user, auth_info_pass);
    busy = global.busy;
    if (!busy)
	global.busy = True;
    tmp = server_write(bg_server, command);
    if (!busy)
	global.busy = False;

    if (tmp < 0) {
	bg_state = BgStateIdle;
	bg_group = NULL;
	if (bg_proc)
	    bg_proc(NULL);
	bg_proc = NULL;
	return False;
    }

    bg_state = BgStateSentAuthUser;

    return True;
}
