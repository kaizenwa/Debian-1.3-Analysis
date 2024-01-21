/*  VER 025  TAB P   $Id: exec.c,v 1.3 1996/11/22 12:31:52 src Exp $
 *
 *  run a script
 *
 *  copyright 1996 Egil Kvaleberg, egilk@sn.no
 *  the GNU General Public License applies
 */

#include "common.h"
#include "proto.h"

/* #include <sys/types.h> */
#include <fcntl.h>

/*
 *  run a program with in and out plumbed as stdin and stdout 
 */
int script(char *program, int in, int out)
{
    int sts;
    int err;
    int pid;

    if ((pid = fork()) < 0) {
	log_msg(L_ERRno,"could not fork");
	unlock_exit(1);
    }

    if (pid == 0) {
	dup2(in, 0);
	dup2(out, 1);
	/* BUG: do something better... */
	err = open("/tmp/connect.errors", O_WRONLY|O_APPEND|O_CREAT, 0644);
	if (err >= 0) dup2(err, 2);
	setuid(getuid());
	setgid(getgid());
	execl("/bin/sh", "sh", "-c", program, (char *)0);
	log_msg(L_ERRno,"could not exec /bin/sh");
	unlock_exit(1);
    }

    while (waitpid(pid, &sts, 0) < 0) {
	if (errno == EINTR)
	    continue;
	log_msg(L_ERRno,"error waiting for connect process");
	unlock_exit(1);
    }

    return sts;
}

/*
 *  start a program with in and out plumbed to FILE pair
 */
void program_open(char *program, SOCKET_D *sock)
{
    int sts;
    int err;
    int pid;
    int p_in[2];
    int p_out[2];

    if (pipe(p_in) < 0
     || pipe(p_out) < 0) {
	log_msg(L_ERRno,"could not pipe");
	unlock_exit(1);
    }
    if ((pid = fork()) < 0) {
	log_msg(L_ERRno,"could not fork");
	unlock_exit(1);
    }

    if (pid == 0) {
	/* child */
	close(p_in[1]); /* only read on this */
	close(p_out[0]); /* only write on this */
	dup2(p_in[0], 0);
	dup2(p_out[1], 1);

	/* BUG: do something better... */
	err = open("/tmp/connect.errors", O_WRONLY|O_APPEND|O_CREAT, 0644);
	if (err >= 0) dup2(err, 2);
	setuid(getuid());
	setgid(getgid());
	execl("/bin/sh", "sh", "-c", program, (char *)0);
	log_msg(L_ERRno,"could not exec /bin/sh");
	unlock_exit(1);
    } else {
	/* parent */
	close(p_in[0]); /* only write on this */
	close(p_out[1]); /* only read on this */
	sock->r_fd = p_out[0]; 
	sock->w_fd = p_in[1];  
	if (!(sock->r_str = fdopen(p_out[0],"r"))) {
	    log_msg(L_ERRno,"could not open pipe for read");
	}
	if (!(sock->w_str = fdopen(p_in[1],"w"))) {
	    log_msg(L_ERRno,"could not open pipe for write");
	}
    }

    /* do not wait for child */
}

