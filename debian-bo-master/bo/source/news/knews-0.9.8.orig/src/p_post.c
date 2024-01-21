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
#include <sys/stat.h>
#include <sys/wait.h>
#include "child.h"
#include "codes.h"
#include "connect.h"
#include "decode.h"
#include "file.h"
#include "newsrc.h"
#include "p_I.h"
#include "p_attach.h"
#include "p_popup.h"
#include "p_post.h"
#include "resource.h"
#include "server.h"
#include "thread.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"

static int do_post(int fd, char **msg)
{
    char	buffer[16834];
    char	*reply;
    long	n;
    int		status;

    set_message("Sending posting request...", False);
    reply = server_comm(main_server, "POST\r\n", True);
    if (!reply) {
	*msg =
	    "Connection to server broken while sending\n"
	    " post request.  The article was not posted.";
	return -1;
    }

    status = atoi(reply);
    if (status != NNTP_CONT_POST) {
	popup_title_notice("Posting failed, message from server is",
			   reply, False);
	return False;
    }

    set_message("Sending article...", False);
    while ((n = read(fd, buffer, sizeof buffer - 1)) > 0) {
	buffer[n] = '\0';
	if (server_write(main_server, buffer) < 0) {
	    *msg =
		"Connection to server broken while sending\n"
		"article.  The article may or may not have\n"
		"been accepted by the server.";
	    return -1;
	}
    }

    if (n < 0) {
	server_close(main_server);
	*msg = "Error with temp file.  Post status unknown...";
	return -1;
    }

    set_message("Article posted, waiting for reply...", False);
    reply = server_read(main_server);
    if (!reply) {
	*msg =
	    "Connection to server broken while waiting\n"
	    "for result of posting.  The article may have\n"
	    "been posted.";
	return -1;
    }

    if (atoi(reply) == NNTP_OK_POSTED)
	return True;

    popup_title_notice("Posting failed, message from server is",
		       reply, False);
    return False;
}

int post_article(FILE *fp)
{
    int		fd = fileno(fp);
    char	*msg = NULL;
    int		ret;

    if (lseek(fd, 0, SEEK_SET) < 0) {
	perror("lseek");
	popup_title_notice(NULL,
			   "Error with temp file!  Article not posted.",
			   False);
	return False;
    }

    set_busy(True);
    server_set_quit_func(main_server, nntp_just_close);
    ret = do_post(fd, &msg);
    server_set_quit_func(main_server, nntp_quit);
    if (ret < 0) {
	popup_title_notice(NULL, msg, False);
	reconnect_server(True);
	ret = False;
    }
    unset_busy();

    return ret;
}

int post_to_agent(char *agent, FILE *fp)
{
    char	stderr_buf[STDERR_BUFFLEN];
    char	message[256];
    pid_t	pid, wpid;
    int		status;
    int		fd = fileno(fp);

    pid = fork_nicely(NULL, NULL, True);

    if (pid < 0) {
	popup_title_notice(NULL, "Fork failed!", False);
	return False;
    }

    if (pid == 0) {
	if (lseek(fd, 0, SEEK_SET) < 0) {
	    perror("knews: lseek");
	    _exit(127);
	}
	if (fd != STDIN_FILENO) {
	    if (dup2(fd, STDIN_FILENO) != STDIN_FILENO) {
		perror("knews: dup2");
		_exit(126);
	    }
	    close(fd);
	}

	execl(BIN_SH, "sh", "-c", agent, (char *)0);
	perror("knews: execl " BIN_SH);
	_exit(127);
    }

    set_busy(False);
    wpid = wait_for_pid(pid, &status, stderr_buf);
    unset_busy();

    if (wpid != pid) {
	popup_title_notice(NULL, "wpid != pid, this ain't happening!", True);
	return False;
    }

    message[0] = '\0';
    if (WIFEXITED(status)) {
	status = WEXITSTATUS(status);
	switch (status) {
	case 0:
	    break;
	case 127:
	    strcpy(message, "Failed to start ");
	    strncat(message, agent, 200);
	    break;
	default:
	    strncat(message, agent, 200);
	    strcat(message, " exited abnormally!");
	    break;
	}
    } else if (WIFSIGNALED(status)) {
	strncat(message, agent, 200);
	sprintf(message + strlen(message), " caught %s!",
		signal_string(WTERMSIG(status)));
	status = 1;
    } else {
	strncat(message, agent, 200);
	strcat(message, " exited wierdly...");
	status = 1;
    }

    if (status != 0) {
	stderr_popup(stderr_buf, 0);
	popup_title_notice(NULL, message, False);
	return False;
    }

    return True;
}

/*************************************************************************/

static void generate_mime_boundary(char *boundary)
{
    int	n;

    srand(3ul * rand() + 17ul * time(NULL));

    strcpy(boundary, "=-=-=__");
    n = strlen(boundary);
    while (n < 32)
	boundary[n++] = base64_alpha[(rand() / 13u) % 64u];
    strcpy(boundary + n, "__=-=-=");
}

static const char *skip_header(const char *c)
{
    while (*c != '\0')
	if (*c == '\n' && !IS_SPACE(*(c+1)))
	    return c + 1;
	else
	    c++;

    return c;
}

static const char *print_header(FILE *fp, const char *c)
{
    /*
     *  rfc1522 encoding of headers would go here (but it sucks).
     */
    while (*c != '\0') {
	if (*c != '\n')
	    putc(*c, fp);
	else {
	    fputs("\r\n", fp);
	    if (!IS_SPACE(*(c+1)))
		return c + 1;
	}
	c++;
    }

    return c;
}

static int do_dump(FILE *fp, PostContext *context)
{
    PostAttachment	**a = context->attachments;
    int			na = context->n_attachments;
    const char		*art = context->art;
    const char		*ct = NULL;
    const char		*cte = NULL;
    char		buf[80], *boundary = NULL;

    fputs("Mime-Version: 1.0\r\n", fp);
    if (context->flags & NEEDS_SENDER)
	fprintf(fp, "Sender: %s@%s\r\n", global.user_id, global.domain_name);

    while (*art != '\0' && *art != '\n') {
#define IS_HEADER(h) (case_lstrncmp(art, h, sizeof h - 1) == 0)
	if (IS_HEADER("mime-version:")) {
	    art = skip_header(art);
	    continue;
	} else if (IS_HEADER("content-type:")) {
	    ct = art;
	    art = skip_header(art);
	    continue;
	} else if (IS_HEADER("content-transfer-encoding:")) {
	    cte = art;
	    art = skip_header(art);
	    continue;
	}
#undef IS_HEADER

	art = print_header(fp, art);
    }

    if (*art == '\n')
	art++;

    if (!a) {
	if (ct)
	    print_header(fp, ct);
	else
	    fprintf(fp, "Content-Type: text/plain; charset=%s\r\n",
		    context->has_8bit ? context->charset : "us-ascii");
	if (cte)
	    print_header(fp, cte);
	else if (context->has_8bit)
	    fputs("Content-Transfer-Encoding: 8bit\r\n", fp);
	fprintf(fp, "\r\n");
    } else if (*art != '\0' || na != 1) {
	generate_mime_boundary(buf);
	boundary = buf;
	fprintf(fp,
		"Content-Transfer-Encoding: 8bit\r\n"
		"Content-Type: multipart/mixed;\r\n"
		"    boundary=\"%s\"\r\n"
		"\r\n",
		boundary);
	if (*art != '\0') {
	    fprintf(fp, "--%s\r\n", boundary);
	    if (cte)
		print_header(fp, cte);
	    if (ct)
		print_header(fp, ct);
	    fputs("\r\n", fp);
	}
    }

    if (*art != '\0') {
	int	bol = True;

	while (*art != '\0') {
	    if (*art == '\n')
		putc('\r', fp);
	    else if (bol && *art == '.')
		putc('.', fp);
	    putc(*art, fp);
	    bol = *art == '\n';
	    art++;
	}
	if (!bol)
	    fputs("\r\n", fp);
    }

    if (a) {
	int	i;

	for (i = 0 ; i < na ; i++) {
	    if (boundary)
		fprintf(fp, "--%s\r\n", boundary);
	    if (!print_attachment(fp, a[i]))
		return False;
	}
	if (boundary)
	    fprintf(fp, "--%s--\r\n", boundary);
    }

    fputs(".\r\n", fp);

    if (fflush(fp) < 0) {
	perror("fflush");
	return False;
    }

    return True;
}

FILE *dump_art_to_file(PostContext *context)
{
    FILE	*fp = NULL;
    char	*tmp = NULL;

    if (!context->art)
	return NULL;

    fp = create_temp_file(&tmp);
    if (!fp) {
	popup_title_notice(NULL, "Failed to create temp file!", True);
	return NULL;
    }
    if (tmp)
	unlink(tmp);

    if (!do_dump(fp, context)) {
	fclose(fp);
	fp = NULL;
    }

    return fp;
}
