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
#include "connect.h"
#include "file.h"
#include "resource.h"
#include "save.h"
#include "server.h"
#include "util.h"
#include "uudecode.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtTree.h"

#define UU_REGEXP \
	"([^ \t]+)" 			/* 1: the file name		*/ \
	"([[({ \t]|part|-)*"		/* 2: skip garbage		*/ \
	"([0-9]+)"			/* 3: which part		*/ \
	"[ \t]*(/|of)[ \t]*"		/* 4: skip separator & garbage	*/ \
	"([0-9]+)"			/* 5: number of parts		*/
#define UU_NAME_OFFSET		1
#define UU_PART_OFFSET		3
#define UU_NOPARTS_OFFSET	5
#define UU_NO_OFFSETS		6

static int uu_parse_subject(char *subject, char **file_name,
			    int *no_parts, int *part)
{
    static regex_t	re;
    static int		inited = False;
    regmatch_t		pmatch[UU_NO_OFFSETS];

    if (!inited) {
	if (regcomp(&re, UU_REGEXP, REG_EXTENDED | REG_ICASE) != 0) {
	    fputs("knews: internal error, failed to compile"
		  " regexp to use for uudecoding.", stderr);
	    regfree(&re);
	    return False;
	}
	inited = True;
    }

    if (regexec(&re, subject, UU_NO_OFFSETS, pmatch, 0) != 0)
	return False;

    if (no_parts) {
	int	i;

	*no_parts = 0;
	for (i = pmatch[UU_NOPARTS_OFFSET].rm_so ;
	     i < pmatch[UU_NOPARTS_OFFSET].rm_eo ; i++) {
	    *no_parts *= 10;
	    *no_parts += subject[i] - '0';
	}
    }

    if (part) {
	int	i;

	*part = 0;
	for (i = pmatch[UU_PART_OFFSET].rm_so ;
	     i < pmatch[UU_PART_OFFSET].rm_eo ; i++) {
	    *part *= 10;
	    *part += subject[i] - '0';
	}
    }

    if (file_name) {
	int	len;

	len = pmatch[UU_NAME_OFFSET].rm_eo - pmatch[UU_NAME_OFFSET].rm_so;
	*file_name = XtMalloc(len + 1);
	memcpy(*file_name, &subject[pmatch[UU_NAME_OFFSET].rm_so], len);
	(*file_name)[len] = '\0';
    }    

    return True;
}

static void perror_exit(const char *msg)
{
    perror(msg);
    _exit(127);
}

static void uufilter(int in, int out)
{
    char	out_buf[16384];
    SERVER	*server;
    char	*buffer;
    enum {
	UuStateSearching,
	UuStateDoing,
	UuStateSkipping
    } state = UuStateSearching;
    long	n, pos;

    server = server_create(in);

    pos = 0;
    while ((buffer = server_read(server)))
	switch (state) {
	case UuStateSearching:
	    if (strncmp(buffer, "BEGIN", 5) == 0) {
		state = UuStateDoing;
		break;
	    }

	    if (strncmp(buffer, "begin ", 6) == 0 &&
		     isdigit(buffer[6]) &&
		     isdigit(buffer[7]) &&
		     isdigit(buffer[8]))
		state = UuStateDoing;
	    /* fall through */
	    else
		break;
	case UuStateDoing:
	    if (strncmp(buffer, "END", 3) == 0) {
		state = UuStateSkipping;
		break;
	    }
	    n = strlen(buffer);
	    if (n > sizeof out_buf / 2) {
		fputs("knews: buffer overflow\n", stderr);
		_exit(1);
	    }
	    if (n + pos + 1 > sizeof out_buf) {
		if (writen(out, out_buf, pos) < 0)
		    _exit(1);
		pos = 0;
	    }
	    memcpy(out_buf + pos, buffer, n);
	    pos += n;
	    out_buf[pos++] = '\n';
	    break;
	case UuStateSkipping:
	    if (strncmp(buffer, "BEGIN", 5) == 0)
		state = UuStateDoing;
	    break;
	}

    if (writen(out, out_buf, pos) < 0)
	_exit(1);
}

static void uudecode_child(int in_fd)
{
    int		fd[2];
    char	*uu_dir     = res_uu_dir();
    char	*uu_program = res_uu_program();
    pid_t	pid;

    if (uu_dir)
	chdir_mkdir(uu_dir);

    if (lseek(in_fd, SEEK_SET, 0) < 0)
	perror_exit("lseek");

    if (uu_program) {
	if (in_fd != STDIN_FILENO)
	    if (dup2(in_fd, STDIN_FILENO) == STDIN_FILENO)
		close(in_fd);
	    else {
		perror("knews: dup2");
		_exit(127);
	    }

	execl(BIN_SH, "sh", "-c", uu_program, (char *)NULL);
	perror("knews: execl " BIN_SH);
	_exit(127);
    }

    if (pipe(fd) < 0) {
	perror("knews: pipe");
	_exit(127);
    }

    pid = fork();
    if (pid < 0) {
	perror("knews: fork");
	_exit(127);
    }

    if (pid == 0) {
	close(fd[0]);
	uufilter(in_fd, fd[1]);
	_exit(0);
    }

    close(fd[1]);

    if (fd[0] != STDIN_FILENO)
	if (dup2(fd[0], STDIN_FILENO) == STDIN_FILENO)
	    close(fd[0]);
	else {
	    perror("knews: dup2");
	    _exit(127);
	}

    execlp("uudecode", "uudecode", (char *)0);
    perror("knews: execlp(\"uudecode\")");
    _exit(127);
}

static int do_decode(ARTICLE **arts, int no_parts)
{
    char	message[128];
    FILE	*fp;
    char	*file_name;
    pid_t	pid;
    long	result[2];

    fp = create_temp_file(&file_name);
    if (!fp) {
	set_message("Failed to create temp file!", True);
	return False;
    }
    unlink(file_name);

    strcpy(message, "Saving to temp file...   ");
    set_busy(True);
    if (save_to_file(fp, message, arts, no_parts, SAVE_BODY, result) < 0) {
	reconnect_server(True);
	unset_busy();
	return False;
    }
    unset_busy();

    if (result[0] < 0) {
	set_message("Error with temp file!", True);
	fclose(fp);
	return False;
    }

    pid = fork_nicely(XtNewString("uudecode"), pipe_context_callback,
		      global.stderr_timeout >= 0);
    if (pid < 0) {
	fclose(fp);
	set_message("Failed to start 'uudecode' program!", True);
	return False;
    }

    if (pid == 0)
	uudecode_child(fileno(fp));

    fclose(fp);  /* Already flushed */
    set_message("Started uudecode program.", False);
    return True;
}

void uudecode_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    char	*file_name = NULL;
    int		is_uu, no_parts, part;
    ARTICLE	**arts;
    SUBJECT	*subj1, *subj2;

    if (global.busy || (global.mode != NewsModeGroup &&
			global.mode != NewsModeThread))
	return;

    if (global.curr_art)
	is_uu = uu_parse_subject(global.curr_art->subject->subject,
				 &file_name, &no_parts, NULL);
    else if (global.curr_subj)
	is_uu = uu_parse_subject(global.curr_subj->subject,
				 &file_name, &no_parts, NULL);
    else {
	set_message("No selected subject!", True);
	return;
    }

    if (!is_uu || no_parts <= 0 || !file_name) {
	XtFree(file_name);
	set_message("This doesn't appear to be a uuencoded article.", True);
	return;
    }

    arts = (ARTICLE **)XtMalloc((no_parts + 2) * sizeof(ARTICLE *));
    for (part = 0 ; part < no_parts + 2 ; part++)
	arts[part] = NULL;

#define FIND_PART(subj)                                                  \
    if (strstr(subj->subject, file_name) &&                              \
	uu_parse_subject(subj->subject, NULL, NULL, &part)) {            \
	if (part > no_parts) {                                           \
	    set_message("Error: I'm confused by that subject...", True); \
	    XtFree((char *)arts);                                        \
	    XtFree(file_name);                                           \
	    return;                                                      \
	}                                                                \
	if (part >= 0 && !arts[part]) {                                  \
	    ARTICLE	*art;                                            \
	    for (art = subj->thread ; art ;                              \
		 art = next_in_thread_wrap(art))                         \
		if (art->from && art->subject == subj) {                 \
		    arts[part] = art;                                    \
		    break;                                               \
		}                                                        \
	}                                                                \
    }

    subj1 = global.curr_subj;
    subj2 = subj1->prev;
    while (subj1 || subj2) {
	if (subj1) {
	    FIND_PART(subj1);
	    subj1 = subj1->next;
	}
	if (subj2) {
	    FIND_PART(subj2);
	    subj2 = subj2->prev;
	}
    }

#undef FIND_PART

    XtFree(file_name);

    for (part = 1 ; part <= no_parts ; part++)
	if (!arts[part]) {
	    XtFree((char *)arts);
	    set_message("Error: Couldn't find all parts!", True);
	    return;
	}

    if (do_decode(arts + 1, no_parts))
	for (part = 0 ; part <= no_parts ; part++) {
	    if (arts[part] && arts[part]->from && !arts[part]->read) {
		arts[part]->read = True;
		arts[part]->subject->no_unread--;
		global.curr_group->no_unread--;
		if (res_process_xrefs())
		    process_xref(arts[part]);

		if (arts[part]->pixmap != None) {
		    global.n_hot--;
		    update_subj_hot_value(arts[part]->subject);
		}
		update_subj_entry(arts[part]->subject);
		if (global.mode == NewsModeThread) {
                    ArtTreeNodeSetInner(main_widgets.arttree,
                                        (ART_TREE_NODE *)arts[part], False);
                    ArtTreeNodeSetPixmap(main_widgets.arttree,
                                         (ART_TREE_NODE *)arts[part], None);
		}
	    }
	}

    XtFree((char *)arts);
}

void action_uudecode(Widget w, XEvent *event,
		     String *params, Cardinal *no_params)
{
    uudecode_callback(w, NULL, NULL);
}
