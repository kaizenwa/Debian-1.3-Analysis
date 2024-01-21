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
#include "child.h"
#include "codes.h"
#include "connect.h"
#include "file.h"
#include "resource.h"
#include "server.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"

#include "sysdeps.h"

struct SERVER {
    int		fd;
    char	*buffer;
    long	curr_pos;
    long	len;
    FILE	*bs;   /* backing store */
    QuitFunc	quit_func;
    int		aborted;
};

#define BUFFERLEN (8192 - 16)

SERVER *server_create(int fd)
{
    SERVER	*server;

    server = (SERVER *)XtMalloc(sizeof *server);
    server->fd = fd;
    server->buffer = XtMalloc(BUFFERLEN + 3);
    server->len = BUFFERLEN;
    server->curr_pos = 0;
    server->bs = NULL;
    server->quit_func = NULL;
    server->buffer[0] = '\0';

    return server;
}

void server_free(SERVER *server)
{
    server_close(server);
    XtFree(server->buffer);
    server->buffer = NULL;
    server->len = 0;
    server->bs = NULL;
    server->quit_func = NULL;
    server->aborted = False;
    XtFree((char *)server);
}

void server_close(SERVER *server)
{
    server->curr_pos = 0;
    server->buffer[0] = '\0';
    server->bs = NULL;
    server->aborted = False;

    if (server->fd >= 0 && close(server->fd) < 0)
	perror("knews: close");
    server->fd = -1;
}

int server_get_fd(SERVER *server)
{
    return server->fd;
}

void server_set_fd(SERVER *server, int fd)
{
    server->fd = fd;
}

void server_set_bs(SERVER *server, FILE *bs)
{
    server->bs = bs;
}

void server_set_quit_func(SERVER *server, QuitFunc quit_func)
{
    server->quit_func = quit_func;
}

QuitFunc server_get_quit_func(SERVER *server)
{
    return server->quit_func;
}

int server_aborted(SERVER *server)
{
    return server->aborted;
}

void nntp_quit(void *data)
{
    SERVER	*server = data;

    if (server->fd < 0)
	return;

    server_write_raw(server, "\r\nQUIT\r\n", 8);
    server_close(server);
    server->aborted = True;
}

void nntp_just_close(void *data)
{
    SERVER	*server = data;

    if (server->fd < 0)
	return;

    server_close(server);
    server->aborted = True;
}

/*************************************************************************/

/*
 *  Will block iff tell > 0.
 *
 *  tell != 0   means tell about errors
 *  tell >  1   means say what you're doing
 */
int server_open(SERVER *server, struct SERV_ADDR *addr, int tell)
{
    long	tmp;

    server_close(server);
    server->fd = open_socket();
    if (server->fd < 0) {
	set_message("Error: Failed to create socket!", True);
	return -1;
    }

    if (tell > 1)
	set_message("Server contacted, waiting for response...", False);

    tmp = connect_socket(server->fd, addr);

    if (tmp < 0 && would_block(server->fd, errno)) {
	if (tell == 0)
	    return 0; /* don't block */

	tmp = 0;
	do_wait(&server->fd, True, server->quit_func, server);
	if (server->fd < 0)
	    return -1;
    }

    server->buffer[0] = '\0';
    if (tmp >= 0)
	do {
	    tmp = read(server->fd, server->buffer, server->len);
	} while (tmp < 0 && errno == EINTR);

    if (tmp >= 0) {
	server->buffer[tmp] = '\0';
	return 0;
    }

    if (would_block(server->fd, errno))
	return 0;

    perror("knews: read");

    if (tell) {
	char	message[128];
	char	*tmp = error_string(errno);

	if (!tmp)
	    tmp = "Connection failed";
	sprintf(message, "Error: %s!", tmp);
	set_message(message, True);
    }

    return -1;
}

int server_fork(SERVER *server, char *command, int tell)
{
    static char	*cmd = NULL;
    pid_t	pid;
    int		fd[2];

    if (command)
	cmd = command;

    server_close(server);

    if (open_duplex(fd) < 0) {
	if (tell)
	    set_message("Error!  Failed to open duplex connection!", True);
	return -1;
    }

    pid = fork_nicely(NULL, NULL, False);
    if (pid < 0) {
	close(fd[0]);
	close(fd[1]);
	if (tell)
	    set_message("Error!  Fork failed!", True);
	return -1;
    }

    if (pid == 0) {
	char	*c;

	c = CODE_TO_STR(NNTP_ERR_FAULT) "%d This ain't happening, man!\r\n";

	if (fd[1] != STDIN_FILENO) {
	    if (dup2(fd[1], STDIN_FILENO) != STDIN_FILENO) {
		perror("knews: dup2");
		write(STDOUT_FILENO, c, strlen(c));
		_exit(0);
	    }
	    close(fd[1]);
	}

	if (dup2(STDIN_FILENO, STDOUT_FILENO) != STDOUT_FILENO) {
	    perror("knews: dup2");
	    write(STDOUT_FILENO, c, strlen(c));
	    _exit(0);
	}

	execl(BIN_SH, "sh", "-c", command, (char *)0);
	perror("knews: execl " BIN_SH);

	write(STDOUT_FILENO, c, strlen(c));
	_exit(0);
    }

    close(fd[1]);
    server->fd = fd[0];

    return 0;
}

/*************************************************************************/

long server_write_raw(SERVER *server, char *message, long n)
{
    long	i = 0;

    if (server->fd < 0)
	return -1;

    if (n > 0)
	do {
	    i = write(server->fd, message, n);
	} while (i < 0 && errno == EINTR);

    return 0;
}

int server_write(SERVER *server, char *message)
{
    long	n = strlen(message);

    if (server->fd < 0)
	return -1;

    while (n > 0) {
	long	i;

	do {
	    i = write(server->fd, message, n);
	} while (i < 0 && errno == EINTR);

	if (i < 0) {
	    if (would_block(server->fd, errno)) {
		if (do_wait(&server->fd, False, server->quit_func, server) < 0)
		    return -1;
		continue;
	    }

	    perror("knews: write");
	    return -1;
	}

	if (i == 0)
	    return -1;

	n -= i;
	message += i;
    }

    return 0;
}

/*************************************************************************/

long server_read_raw(SERVER *server)
{
    long	n, len, i;

    n = server->curr_pos + strlen(server->buffer + server->curr_pos);
    len = server->len - n;

    if (len <= 0) {
	len += server->len;
	server->len *= 2;
	server->buffer = XtRealloc(server->buffer, server->len + 3);
    }

    i = read(server->fd, server->buffer + n, len);
    if (i >= 0)
	server->buffer[n + i] = '\0';

    return i;
}

char *server_get_line(SERVER *server)
{
    char	*c = server->buffer + server->curr_pos;
    char	*tmp;
    long	len;

    tmp = strchr(c, '\n');
    if (tmp) {
	if (*(tmp-1) == '\r')
	    *(tmp-1) = '\0';
	*tmp++ = '\0';
	server->curr_pos = tmp - server->buffer;

	if (server->bs)
	    fprintf(server->bs, "%s\r\n", c);

	return c;
    }

    len = strlen(c);
    memmove(server->buffer, c, len + 1);
    server->curr_pos = 0;

    return NULL;
}

char *server_read(SERVER *server)
{
    char	*c;

    while (!(c = server_get_line(server))) {
	long	i;

	do {
	    i = server_read_raw(server);
	} while (i < 0 && errno == EINTR);

	if (i == 0)
	    return NULL;
	if (i < 0) {
	    if (would_block(server->fd, errno)) {
		if (do_wait(&server->fd, True, server->quit_func, server) < 0)
		    return NULL;
		continue;
	    }

	    perror("knews: read");
	    return NULL;
	}
    }

    return c;
}

static char *find_crlf_dot_crlf(char *buffer)
{
    char	*c;

    if (buffer[0] == '.' && buffer[1] == '\r' && buffer[2] == '\n')
	return buffer;

    c = strstr(buffer, "\r\n.\r\n");
    if (c)
	return c + 2;

    return NULL;
}

char *server_get_chunk(SERVER *server)
{
    char	*c = server->buffer + server->curr_pos;
    char	*dot;
    long	len;

    dot = find_crlf_dot_crlf(c);
    if (dot) {
	*dot++ = '\0';
	if (*dot++ == '\r')
	    dot++;
	server->curr_pos = dot - server->buffer;
	return c;
    }

    len = strlen(c);
    memmove(server->buffer, c, len + 1);
    server->curr_pos = 0;

    return NULL;
}

char *server_read_chunk(SERVER *server)
{
    char	*c;

    if (server->fd < 0)
	return NULL;

    while (!(c = server_get_chunk(server))) {
	long	i;

	do {
	    i = server_read_raw(server);
	} while (i < 0 && errno == EINTR);

	if (i == 0)
	    return NULL;
	if (i < 0) {
	    if (would_block(server->fd, errno)) {
		if (do_wait(&server->fd, True, server->quit_func, server) < 0)
		    return NULL;
		continue;
	    }

	    perror("knews: read");
	    return NULL;
	}
    }

    return c;
}

char *server_comm(SERVER *server, char *command, int reconnect)
{
    char	*reply;
    int		i = 0;

    do {
	if (server_write(server, command) < 0 &&
	    (!reconnect || server->aborted ||
	     reconnect_server(False) < 0 ||
	     server_write(server, command) < 0))
	    return NULL;
	reply = server_read(server);
    } while ((!reply || (reply[0] == '5' &&
			 case_lhassub(reply, "timeout"))) && i++ < 1);

    if (!reply)
	return NULL;

    i = atoi(reply);

    if (i == 450) {
	char	buffer[1024];
	char	*auth_user = res_auth_info_user();
	char	*auth_pass = res_auth_info_pass();

	if (!auth_user || !auth_pass ||
	    strlen(auth_user) > 500 || strlen(auth_pass) > 500)
	    return reply;

	server_write(server, "AUTHINFO SIMPLE");
	reply = server_read(server);
	if (!reply || atoi(reply) != 350)
	    return reply;

	sprintf(buffer, "%s %s\r\n", auth_user, auth_pass);
	server_write(server, buffer);
	reply = server_read(server);
	if (!reply || atoi(reply) != 250)
	    return reply;

	server_write(server, command);
	reply = server_read(server);
    } else if (i == NNTP_ERR_NEED_AUTH) {
	char	buffer[1024];
	char	*auth_user = res_auth_info_user();
	char	*auth_pass = res_auth_info_pass();

	if (!auth_user || !auth_pass ||
	    strlen(auth_user) > 500 || strlen(auth_pass) > 500)
	    return reply;

	sprintf(buffer, "AUTHINFO USER %s\r\n", auth_user);
	server_write(server, buffer);
	reply = server_read(server);
	if (!reply || atoi(reply) != NNTP_CONT_AUTH)
	    return reply;

	sprintf(buffer, "AUTHINFO PASS %s\r\n", auth_pass);
	server_write(server, buffer);
	reply = server_read(server);
	if (!reply || atoi(reply) != NNTP_OK_AUTH)
	    return reply;

	server_write(server, command);
	reply = server_read(server);
    }

    return reply;
}
