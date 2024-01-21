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
#include <sys/wait.h>
#include <signal.h>
#include "child.h"

#include "sysdeps.h"

#define WRITE_STDERR(msg) write(2, msg, sizeof msg - 1)

static volatile int	sighup_may_exit = True;
static volatile int	caught_sighup   = False;

static void sighup_handler(int sig)
{
    int	oerrno = errno;

    if (sighup_may_exit) {
	if (sig == 0)
	    WRITE_STDERR("exiting.\n");
	else
	    WRITE_STDERR("knews: exiting on SIGHUP!\n");
	_exit(1);
    }

    caught_sighup = True;
    WRITE_STDERR("knews: Caught SIGHUP...  [wait for it!]  ");
    errno = oerrno;
}

void block_sighup(void)
{
    sighup_may_exit = False;
}

void unblock_sighup(void)
{
    sighup_may_exit = True;
    if (caught_sighup)
	sighup_handler(0);
}

/*********************************************************************/

void sigfpe_handler(int sig)
{
    int	oerrno = errno;

    WRITE_STDERR("knews: caught SIGFPE!\n");
    errno = oerrno;
}

/*********************************************************************/

typedef struct CHILD_CONTEXT	CHILD_CONTEXT;

struct CHILD_CONTEXT {
    CHILD_CONTEXT	*next;
    pid_t		 pid;
    volatile int	 exited;
    volatile int	 status;
    void		*data;
    ChildCallback	 proc;
    char		*stderr_buf;
    int			 stderr_fd;
    XtInputId		 stderr_id;
};

static CHILD_CONTEXT	*contexts = NULL;

static int		pipes[2];
static XtInputId	pipe_id = 0;

static void sigchld_handler(int sig)
{
    int	done_some = False;
    int	oerrno = errno;
    int	tmp;

    for (;;) {
	pid_t		pid;
	int		status;
	CHILD_CONTEXT	*loop;

	do {
	    pid = waitpid(-1, &status, WNOHANG);
	} while (pid < 0 && errno == EINTR);

	if (pid <= 0)
	    break;

	for (loop = contexts ; loop ; loop = loop->next)
	    if (loop->pid == pid) {
		done_some = True;
		loop->exited = True;
		loop->status = status;
		break;
	    }
    }

    if (done_some)
	do {
	    tmp = write(pipes[1], &done_some, 1);
	} while (tmp < 0 && errno == EINTR);

    errno = oerrno;
}

#if !defined(HAVE_SIGACTION) || HAVE_SIGACTION

static void install_sig_handlers(void)
{
    struct sigaction	sig_act;
    int			flags;

    flags = 0;
#ifdef SA_RESTART
    flags |= SA_RESTART;
#endif

    sig_act.sa_handler = sigchld_handler;
    sigemptyset(&sig_act.sa_mask);
    sigaddset(&sig_act.sa_mask, SIGCHLD);
    sig_act.sa_flags = flags;
#ifdef SA_NOCLDSTOP
    sig_act.sa_flags |= SA_NOCLDSTOP;
#endif
    if (sigaction(SIGCHLD, &sig_act, NULL) < 0)
	perror("knews: sigaction");

    /*
     *  The Layout widget sometimes generates a SIGFPE. This may be fixed
     *  now, thanks to leo@marco.de, but I might as well keeps this.
     */

    sig_act.sa_handler = SIG_IGN;
    sigemptyset(&sig_act.sa_mask);
    sig_act.sa_flags = flags;
    if (sigaction(SIGPIPE, &sig_act, NULL) < 0)
	perror("knews: sigaction(SIGPIPE)");

    sig_act.sa_handler = sigfpe_handler;
    sigemptyset(&sig_act.sa_mask);
    sigaddset(&sig_act.sa_mask, SIGFPE);
    sig_act.sa_flags = flags;
    if (sigaction(SIGFPE, &sig_act, NULL) < 0)
	perror("knews: sigaction(SIGFPE)");

    sig_act.sa_handler = sighup_handler;
    sigemptyset(&sig_act.sa_mask);
    sigaddset(&sig_act.sa_mask, SIGHUP);
    sig_act.sa_flags = flags;
    if (sigaction(SIGHUP, &sig_act, NULL) < 0)
	perror("knews: sigaction(SIGHUP)");

    sig_act.sa_handler = sigusr1_handler;
    sigemptyset(&sig_act.sa_mask);
    sigaddset(&sig_act.sa_mask, SIGUSR1);
    sig_act.sa_flags = flags;
    if (sigaction(SIGUSR1, &sig_act, NULL) < 0)
	perror("knews: sigaction(SIGHUP)");
}

static void unblock_sigpipe(void)
{
    struct sigaction	sig_act;

    sig_act.sa_handler = SIG_DFL;
    sigemptyset(&sig_act.sa_mask);
    sig_act.sa_flags = 0;
    if (sigaction(SIGPIPE, &sig_act, NULL) < 0)
	perror("knews: siagction(SIGPIPE)");
}

static void unblock_sigchld(int reap)
{
    sigset_t	set;

    sigemptyset(&set);
    sigaddset(&set, SIGCHLD);
    sigprocmask(SIG_UNBLOCK, &set, NULL);
}

static void block_sigchld(void)
{
    sigset_t	set;

    sigemptyset(&set);
    sigaddset(&set, SIGCHLD);
    sigprocmask(SIG_BLOCK, &set, NULL);
}

#else /* !HAVE_SIGACTION: potentially unreliable signals */

static void install_sig_handlers(void)
{
    if (signal(SIGCHLD, sigchld_handler) == SIG_ERR)
	perror("knews: signal(SIGCHLD)");
    if (signal(SIGPIPE, SIG_IGN) == SIG_ERR)
	perror("knews: signal(SIGPIPE)");
    if (signal(SIGFPE,  sigfpe_handler) == SIG_ERR)
	perror("knews: signal(SIGFPE)");
    if (signal(SIGHUP, sighup_handler) == SIG_ERR)
	perror("knews: signal(SIGHUP)");
    if (signal(SIGUSR1, sigusr1_handler) == SIG_ERR)
	perror("knews: signal(SIGUSR1)");
}

static void unblock_sigpipe(void)
{
    if (signal(SIGPIPE, SIG_DFL) == SIG_ERR)
	perror("knews: signal(SIGPIPE)");
}

static void unblock_sigchld(int reap)
{
    if (signal(SIGCHLD, sigchld_handler) == SIG_ERR)
	perror("knews: signal(SIGCHLD)");
    if (reap)
	sigchld_handler(0);
}

static void block_sigchld(void)
{
    if (signal(SIGCHLD, SIG_DFL) == SIG_ERR)
	perror("knews: signal(SIGCHLD)");
}

#endif

static void stderr_finalize(CHILD_CONTEXT *context)
{
    if (context->stderr_fd >= 0) {
	int	i, n;

	i = strlen(context->stderr_buf);
	n = STDERR_BUFFLEN - i - 1;
	if (n > 0) {
	    char	*c = context->stderr_buf + i;

	    i = read(context->stderr_fd, c, n);
	    if (i >= 0) c[i] = '\0';
	    else c[0] = '\0';
	}

	close(context->stderr_fd);
	context->stderr_fd = -1;
    }

    if (context->stderr_id != 0) {
	XtRemoveInput(context->stderr_id);
	context->stderr_id = 0;
    }
}

static void pipe_read_callback(XtPointer client_data, int *fd, XtInputId *id)
{
    CHILD_CONTEXT	*loop, *prev;
    char		c;

    block_sigchld();

    (void)read(pipes[0], &c, 1);

    loop = contexts;
    prev = NULL;
    while (loop) {
	if (loop->exited) {
	    CHILD_CONTEXT	*next = loop->next;

	    stderr_finalize(loop);
	    if (loop->proc)
		loop->proc(loop->data,
			   loop->status,
			   loop->stderr_buf);
	    if (prev)
		prev->next = next;
	    else
		contexts = next;
	    XtFree(loop->stderr_buf);
	    XtFree((char *)loop);
	    loop = next;
	} else {
	    prev = loop;
	    loop = loop->next;
	}
    }

    unblock_sigchld(True);
}

void suspend_child_contexts(void)
{
    if (pipe_id != 0) {
	XtRemoveInput(pipe_id);
	pipe_id = 0;
    }
}

void resume_child_contexts(void)
{
    if (pipe_id == 0)
	pipe_id = XtAppAddInput(app_cont, pipes[0],
				(XtPointer)XtInputReadMask,
				pipe_read_callback, NULL);
}

void init_child_contexts(void)
{
    long	flags;

    if (pipe(pipes) < 0) {
	perror("knews: pipe");
	exit(1);
    }

    if ((flags = fcntl(pipes[0], F_GETFL)) < 0 ||
	fcntl(pipes[0], F_SETFL, flags | O_NONBLOCK) < 0 ||
	(flags = fcntl(pipes[1], F_GETFL)) < 0 ||
	fcntl(pipes[1], F_SETFL, flags | O_NONBLOCK) < 0 ||
	fcntl(pipes[0], F_SETFD, FD_CLOEXEC) < 0 ||
	fcntl(pipes[1], F_SETFD, FD_CLOEXEC) < 0) {
	perror("knews: fcntl");
	exit(1);
    }

    resume_child_contexts();
    install_sig_handlers();
}

static void stderr_read_callback(XtPointer  client_data,
				 int       *fd,
				 XtInputId *id)
{
    CHILD_CONTEXT	*context = (CHILD_CONTEXT *)client_data;
    int			n, i;

    i = strlen(context->stderr_buf);
    n = STDERR_BUFFLEN - i - 1;

    if (n > 0) {
	char	*c = context->stderr_buf + i;

	i = read(context->stderr_fd, c, n);
	if (i > 0) {
	    c[i] = '\0';
	    n -= i;
	    if (n > 0)
		return;
	} else {
	    c[0] = '\0';
	    if (i < 0) {
		perror("knews: read");
		if (   errno == EINTR
		    || errno == EAGAIN
#ifdef EWOULDBLOCK
		    || errno == EWOULDBLOCK
#endif
		    )
		    return;
	    }
	}
    }

    XtRemoveInput(context->stderr_id);
    context->stderr_id = 0;
    close(context->stderr_fd);
    context->stderr_fd = -1;
}

static CHILD_CONTEXT *create_child_context(pid_t          pid,
					   void          *data,
					   ChildCallback  proc)
{
    CHILD_CONTEXT	*temp;

    temp = (CHILD_CONTEXT *)XtMalloc(sizeof(CHILD_CONTEXT));
    temp->pid = pid;
    temp->exited = False;
    temp->status = 0;
    temp->data = data;
    temp->proc = proc;
    temp->stderr_buf = NULL;
    temp->stderr_fd = -1;
    temp->stderr_id = 0;
    temp->next = contexts;
    contexts = temp;

    return temp;
}

pid_t fork_nicely(void *data, ChildCallback proc, int use_stderr)
{
    CHILD_CONTEXT	*context;
    pid_t		pid;
    int			stderr_fd[2];

    if (!use_stderr || pipe(stderr_fd) < 0) {
	stderr_fd[0] = -1;
	stderr_fd[1] = -1;
    }

    block_sigchld();
    pid = fork();

    if (pid == 0) { /* child */
	unblock_sigchld(False);
	setsid();

	if (!freopen("/dev/null", "w", stdout))
	    perror("/dev/null");
	if (stderr_fd[0] >= 0)
	    close(stderr_fd[0]);

	if (stderr_fd[1] >= 0 && stderr_fd[1] != STDERR_FILENO)
	    if (dup2(stderr_fd[1], STDERR_FILENO) < 0)
		perror("knews: dup2");

	unblock_sigpipe();

	return 0;
    }

    /*
     * parent or error
     */

    if (stderr_fd[1] >= 0)
	close(stderr_fd[1]);

    if (pid < 0) {
	perror("knews: fork");
	if (stderr_fd[0] >= 0)
	    close(stderr_fd[0]);
	unblock_sigchld(True);
	return -1;
    }

    context = create_child_context(pid, data, proc);
    if (stderr_fd[0] >= 0) {
	long	flags;

	if (fcntl(stderr_fd[0], F_SETFD, FD_CLOEXEC) < 0)
	    perror("fcntl");
	if ((flags = fcntl(stderr_fd[0], F_GETFL)) < 0 ||
	    fcntl(stderr_fd[0], F_SETFL, flags | O_NONBLOCK) < 0) {
	    perror("knews: fcntl");
	    close(stderr_fd[0]);
	    stderr_fd[0] = -1;
	} else {
	    context->stderr_buf = XtMalloc(STDERR_BUFFLEN);
	    context->stderr_buf[0] = '\0';
	    context->stderr_fd = stderr_fd[0];
	    context->stderr_id =
		XtAppAddInput(app_cont, stderr_fd[0],
			      (XtPointer)XtInputReadMask,
			      stderr_read_callback, (XtPointer)context);
	}
    }

    unblock_sigchld(True);

    return pid;
}

pid_t wait_for_pid(pid_t pid, int *status, char *stderr_buf)
{
    CHILD_CONTEXT	*context;
    pid_t		result;

    for (context = contexts ; context ; context = context->next)
	if (context->pid == pid)
	    break;

    if (!context) {
	errno = ECHILD;
	return -1;
    }

    suspend_child_contexts();

    while (!context->exited) {
	(void)do_wait(&pipes[0], True, NULL, NULL);
	(void)read(pipes[0], &result, 1);
    }

    stderr_finalize(context);
    result = context->pid;
    if (stderr_buf)
	strcpy(stderr_buf, context->stderr_buf);
    if (status)
	*status = context->status;

    resume_child_contexts();
    pipe_read_callback(NULL, NULL, NULL);

    return result;
}

char *signal_string(int sig)
{
#define DO(x) if (sig == x) return #x
#ifdef SIGHUP
    DO(SIGHUP);
#endif
#ifdef SIGINT
    DO(SIGINT);
#endif
#ifdef SIGQUIT
    DO(SIGQUIT);
#endif
#ifdef SIGABRT
    DO(SIGABRT);
#endif
#ifdef SIGBUS
    DO(SIGBUS);
#endif
#ifdef SIGFPE
    DO(SIGFPE);
#endif
#ifdef SIGKILL
    DO(SIGKILL);
#endif
#ifdef SIGSEGV
    DO(SIGSEGV);
#endif
#ifdef SIGPIPE
    DO(SIGPIPE);
#endif
#ifdef SIGALRM
    DO(SIGALRM);
#endif
#ifdef SIGTERM
    DO(SIGTERM);
#endif
#ifdef SIGCHLD
    DO(SIGCHLD);
#endif
#ifdef SIGCONT
    DO(SIGCONT);
#endif
#ifdef SIGSTOP
    DO(SIGSTOP);
#endif
#ifdef SIGTSTP
    DO(SIGTSTP);
#endif
#ifdef SIGTTIN
    DO(SIGTTIN);
#endif
#ifdef SIGTTOU
    DO(SIGTTOU);
#endif
#ifdef SIGURG
    DO(SIGURG);
#endif
#ifdef SIGWINCH
    DO(SIGWINCH);
#endif
#ifdef SIGIO
    DO(SIGIO);
#endif
#ifdef SIGPOLL
    DO(SIGPOLL);
#endif

    return "unknown signal";
}
