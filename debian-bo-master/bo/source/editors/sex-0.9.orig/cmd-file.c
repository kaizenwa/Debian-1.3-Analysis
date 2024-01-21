/*
 * File:	cmd-file.c
 * Purpose:	Functions that implement file commands.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: cmd-file.c,v 1.51 1997/01/07 01:05:34 liw Exp $"
 */

#include <assert.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <publib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>

#include "anchor.h"
#include "config.h"
#include "cmd.h"
#include "buflist.h"
#include "filewin.h"
#include "selections.h"
#include "win.h"
#include "error.h"
#include "x.h"


/*
 * How much to read/write at a time.
 */
#define READ_BYTES	1024
#define WRITE_BYTES	1024



/*
 * Structure for passing around information about the command for piping
 * selection through.
 */
struct cmd {
	char *cmdline;
	int pid;
	Sbufmark *sel;
};



/*
 * Prototypes
 */

static void pipe_through_cmd(char *, long, void *);
static int start_cmd(struct cmd *, int *, int *);
static int kill_cmd(struct cmd *);
static int check_read_write(int, int);
static int read_some(int, struct dynarr *);
static int write_some(int, char *, long, long *);



/*
 * Function:	cmd_new
 * Purpose:	Create a new buffer and make it visible in window.
 */
int cmd_new(struct win *win) {
	Sbuf *buf;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	buf = create_buffer();
	if (buf == NULL)
		return -1;
	if (win_set_buf(win, buf) == -1) {
		destroy_buffer(buf);
		return -1;
	}
	return 0;
}



/*
 * Function:	cmd_next_file
 * Purpose:	Change visible buffer to the next one.
 */
int cmd_next_file(struct win *win) {
	Sbuf *buf;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	buf = buflist_next(win_buf(win));
	if (win_set_buf(win, buf) == -1)
		return -1;
	if (!sbuf_has_flags(buf, SBUF_LOADED_FLAG)) {
		if (sbuf_load(buf) == -1)
			return -1;
		sbuf_remark(win_selection(win), 0, 0);
		sbuf_mark_set_columnar(win_selection(win), 0);
	}
	return 0;
}



/*
 * Function:	cmd_prev_file
 * Purpose:	Change visible buffer to previous one.
 */
int cmd_prev_file(struct win *win) {
	Sbuf *buf;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	buf = buflist_prev(win_buf(win));
	if (win_set_buf(win, buf) == -1)
		return -1;
	if (!sbuf_has_flags(buf, SBUF_LOADED_FLAG)) {
		if (sbuf_load(buf) == -1)
			return -1;
		sbuf_remark(win_selection(win), 0, 0);
		sbuf_mark_set_columnar(win_selection(win), 0);
	}
	return 0;
}



/*
 * Function:	cmd_query_load_file
 * Purpose:	Popup dialog to ask for filename to be loaded.
 */
int cmd_query_load_file(struct win *win) {
	struct filewin *fw;
	Sbuf *buf;
	char *s;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	fw = win_filewin(win, WIN_LOAD);
	buf = win_buf(win);

	s = sbuf_get_name(buf);
	if (s == NULL)
		s = "";
	filewin_set_filename(fw, s);
	filewin_popup(fw);
	return 0;
}



/*
 * Function:	cmd_set_name_and_load_file
 * Purpose:	Set current filename from dialog and load it into buffer.
 */
int cmd_set_name_and_load_file(struct win *win) {
	struct filewin *fw;
	Sbuf *buf, *this;
	char *name, *p;
	char fullname[1024];	/* xxx boo hiss */
	size_t n;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	fw = win_filewin(win, WIN_LOAD);
	name = filewin_filename(fw);
	
	n = fnqualify(fullname, name, sizeof(fullname));
	if (n > sizeof(fullname)) {
		error(win, "Filename too long");
		return -1;
	}

	buf = this = win_buf(win);
	if (buf != NULL) {
		do {
			p = sbuf_get_name(buf);
			if (p != NULL && strcmp(p, fullname) == 0)
				return win_set_buf(win, buf);
			buf = buflist_by_name(fullname);
		} while (buf != NULL && buf != this);
	}

	buf = create_buffer();
	if (buf == NULL)
		return -1;
	if (sbuf_set_name(buf, fullname) == -1 || win_set_buf(win, buf) == -1){
		destroy_buffer(buf);
		return -1;
	}
	return cmd_load_file(win);
}



/*
 * Function:	cmd_load_file
 * Purpose:	Load file into buffer.
 */
int cmd_load_file(struct win *win) {
	Sbuf *buf;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	buf = win_buf(win);
	if (sbuf_load(buf) == -1)
		return -1;

	sbuf_remark(win_selection(win), 0, 0);
	sbuf_mark_set_columnar(win_selection(win), 0);
	win_show(win, 0);

	return 0;
}



/*
 * Function:	cmd_reload_file
 * Purpose:	Load file into buffer again.
 */
int cmd_reload_file(struct win *win) {
	Sbuf *buf;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	
	buf = win_buf(win);
	if (sbuf_is_dirty(buf)) {
		error(win, "Can't reload dirty buffer: mark unmodifed first");
		return -1;
	}
	if (sbuf_load(buf) == -1) {
		error(win, "Error loading file: %100s", strerror(errno));
		return -1;
	}

	sbuf_remark(win_selection(win), 0, 0);
	sbuf_mark_set_columnar(win_selection(win), 0);
	win_show(win, 0);

	return 0;
}



/*
 * Function:	cmd_load_selection
 * Purpose:	Load file given in selection.
 */

static void do_load(char *str, long len, void *win) {
	filewin_set_filename(win_filewin(win, WIN_LOAD), str);
	(void) cmd_set_name_and_load_file(win);
}

int cmd_load_selection(struct win *win) {
	anchor_up(win);
	cmd_prev_was_cut = 0;
	if (sel_string(win, do_load, win) == -1)
		return -1;
	return 0;
}



/*
 * Function:	cmd_save_as
 * Purpose:	Popup dialog to ask for new filename.
 */
int cmd_save_as(struct win *win) {
	struct filewin *fw;
	Sbuf *buf;
	char *s;

	cmd_prev_was_cut = 0;
	fw = win_filewin(win, WIN_SAVE);
	buf = win_buf(win);

	s = sbuf_get_name(buf);
	if (s == NULL)
		s = "";
	filewin_set_filename(fw, s);
	filewin_popup(fw);
	return 0;
}



/*
 * Function:	cmd_set_name_and_save_file
 * Purpose:	Set current filename from dialog and save it.
 */
int cmd_set_name_and_save_file(struct win *win) {
	struct filewin *fw;
	int ret;
	char fullname[1024];	/* xxx boo hiss */
	size_t n;

	cmd_prev_was_cut = 0;

	fw = win_filewin(win, WIN_SAVE);

	n = fnqualify(fullname, filewin_filename(fw), sizeof(fullname));
	if (n > sizeof(fullname)) {
		error(win, "Filename too long");
		return -1;
	}

	ret = win_set_filename(win, fullname);
	if (ret != -1)
		ret = cmd_save_file(win);
	return ret;
}



/*
 * Function:	cmd_save_file
 * Purpose:	Save buffer to file.
 */
int cmd_save_file(struct win *win) {
	Sbuf *buf;
	int bak;

	cmd_prev_was_cut = 0;

	buf = win_buf(win);
	bak = config_get_boolean(CONFIG_SAVE_BACKUP);

	if (sbuf_get_name(buf) == NULL) {
		error(win, "Error: can't save (buffer has no name)");
		return -1;
	}
	
	if (sbuf_save(buf, bak) == -1) {
		error(win, "Error: couldn't save (%s)", strerror(errno));
		sbuf_set_flags(buf, SBUF_DIRTY_FLAG);	/* not saved! */
		return -1;
	}
	return 0;
}


/*
 * Function:	cmd_fake_save
 * Purpose:	Save buffer to file.
 */
int cmd_fake_save(struct win *win) {
	cmd_prev_was_cut = 0;
	sbuf_set_dirty(win_buf(win), 0);
	return 0;
}



/*
 * Function:	cmd_save_all
 * Purpose:	Save all buffers to file.
 */
int cmd_save_all(struct win *win) {
	Sbuf *buf, *first;
	int backups;
	char *p;

	cmd_prev_was_cut = 0;
	backups = config_get_boolean(CONFIG_SAVE_BACKUP);
	buf = first = buflist_first();
	if (buf != NULL) {
		do {
			if (sbuf_is_dirty(buf) && sbuf_save(buf, backups) == -1) {
				p = sbuf_get_name(buf);
				if (p == NULL)
					p = "(untitled)";
				error(win, "Error: saving file %s", p);
				return -1;
			}
			buf = buflist_next(buf);
		} while (buf != first);
	}
	return 0;
}



/*
 * Function:	cmd_query_insert_file
 * Purpose:	Popup dialog to let user enter name of file to be inserted.
 */
int cmd_query_insert_file(struct win *win) {
	struct filewin *fw;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	fw = win_filewin(win, WIN_INSERT);
	filewin_popup(fw);
	return 0;
}



/*
 * Function:	cmd_insert_file
 * Purpose:	Replace selection with the contents of the requested file.
 */
int cmd_insert_file(struct win *win) {
	FILE *f;
	int ret;
	struct filewin *fw;
	char fullname[1024];	/* xxx boo hiss */
	size_t n;

	anchor_up(win);
	cmd_prev_was_cut = 0;

	fw = win_filewin(win, WIN_INSERT);	
	n = fnqualify(fullname, filewin_filename(fw), sizeof(fullname));
	if (n > sizeof(fullname)) {
		error(win, "Filename too long");
		return -1;
	}
	
	f = fopen(fullname, "r");
	if (f == NULL) {
		error(win, "error opening file: %.512s", strerror(errno));
		return -1;
	}

	ret = sbuf_insert_file(win_selection(win), f);
	if (ret == -1)
		error(win, "error inserting file: %.512s", strerror(errno));
	
	if (fclose(f) == EOF) {
		if (ret == 0)
			error(win, "error closing file: %.512s", 
				strerror(errno));
		ret = -1;
	}

	return ret;
}



/*
 * Function:	cmd_query_write_to
 * Purpose:	Popup dialog to let user enter file selection is to saved to.
 */
int cmd_query_write_to(struct win *win) {
	cmd_prev_was_cut = 0;
	filewin_popup(win_filewin(win, WIN_WRITE_TO));
	return 0;
}



/*
 * Function:	cmd_write_to
 * Purpose:	Write selection to the requested file.
 */
int cmd_write_to(struct win *win) {
	Sbuf *buf;
	struct filewin *fw;
	char fullname[1024];	/* xxx boo hiss */
	size_t n;

	cmd_prev_was_cut = 0;
	
	fw = win_filewin(win, WIN_WRITE_TO);	
	n = fnqualify(fullname, filewin_filename(fw), sizeof(fullname));
	if (n > sizeof(fullname)) {
		error(win, "Filename too long");
		return -1;
	}
	
	buf = win_buf(win);
	if (sbuf_write_to(win_selection(win), fullname) == -1) {
		error(win, "Error writing selection: %s", strerror(errno));
		return -1;
	}

	return 0;
}



/*
 * Function:	cmd_query_pipe
 * Purpose:	Popup dialog to let user enter name of command to pipe
 *		selection through.
 */
int cmd_query_pipe(struct win *win) {
	struct filewin *fw;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	fw = win_filewin(win, WIN_PIPE);
	filewin_popup(fw);
	return 0;
}



/*
 * Function:	cmd_pipe
 * Purpose:	Pipe selection through command.
 */
int cmd_pipe(struct win *win) {
	struct cmd *cmd;
	
	anchor_up(win);
	cmd_prev_was_cut = 0;

	cmd = malloc(sizeof(struct cmd));
	if (cmd == NULL) {
		error(win, "Error: out of memory before piping");
		return -1;
	}
	cmd->cmdline = filewin_filename(win_filewin(win, WIN_PIPE));
	cmd->sel = win_selection(win);
	return sel_string(win, pipe_through_cmd, cmd);
}



/*
 * Function:	cmd_format_selection
 * Purpose:	Pipe selection through "fmt".  Select para, if no selection.
 */
int cmd_format_selection(struct win *win) {
	struct cmd *cmd;
	
	anchor_up(win);
	cmd_prev_was_cut = 0;

	cmd = malloc(sizeof(struct cmd));
	if (cmd == NULL) {
		error(win, "Error: out of memory before piping");
		return -1;
	}

	cmd->cmdline = config_get_string(CONFIG_FMT_PROGRAM);
	cmd->sel = win_selection(win);

	if (sbuf_mark_length(cmd->sel) == 0) {
		if (cmd_select_para(win) == -1 || sel_own(win, -1) == -1) {
			free(cmd);
			return -1;
		}
	}

	if (cmd_copy(win) == -1) {
		free(cmd);
		return -1;
	}

	return sel_string(win, pipe_through_cmd, cmd);
}



/*
 * Function.	cmd_kill_buffer
 * Purpose:	Kill current buffer, exit if it was the last one.
 */
int cmd_kill_buffer(struct win *win) {
	Sbuf *buf;

	anchor_up(win);
	cmd_prev_was_cut = 0;
	buf = win_buf(win);
	if (sbuf_is_dirty(buf)) {
		error(win, "Can't kill modified buffer");
		return -1;
	}
	if (buflist_next(buf) == buf)
		return cmd_quit(win);
	if (win_change_buf_all(buf, buflist_next(buf)) == -1)
		return -1;
	destroy_buffer(buf);
	return 0;
}



/***********************************************************************
 * Local functions.
 */


/*
 * Function:	pipe_through_cmd
 */
static void pipe_through_cmd(char *sel, long len, void *arg) {
	struct cmd *cmd;
	int x, ret, fromcmd, tocmd;
	long n;
	struct dynarr output;
	
	cmd = arg;

	if (start_cmd(cmd, &tocmd, &fromcmd) == -1) {
		error(0, "Error: Can't start command to pipe through");
		return;
	}

	n = 0;	
	ret = 1;
	dynarr_init(&output, 1);

	while (tocmd != -1 || fromcmd != -1) {
		assert(n <= len);
		x = check_read_write(tocmd, fromcmd);
		if (x == -1)
			ret = -1;
		else if (x == tocmd) {
			ret = write_some(tocmd, sel, len, &n);
			if (ret == -1 || n == len) {
				(void) close(tocmd);
				tocmd = -1;
			}
		} else {
			ret = read_some(fromcmd, &output);
			if (ret <= 0) {
				(void) close(fromcmd);
				fromcmd = -1;
			}
		}
	}

	(void) kill_cmd(cmd);

	if (output.used == 0)
		ret = sbuf_strchange(cmd->sel, "", 0);
	else
		ret = sbuf_strchange(cmd->sel, output.data, output.used);
	if (ret == -1)
		error(0, "Error: Out of memory processing piped data");
		
	dynarr_free(&output);
	free(cmd);
}



/*
 * Function:	start_cmd
 * Purpose:	Start the command through which text is piped through.
 */
#define CLOSE_PIPE(p)	((void) close(p[0]), (void) close(p[1]))
static int start_cmd(struct cmd *cmd, int *tocmd, int *fromcmd) {
	int p[2], q[2];
	
	if (pipe(p) == -1)
		return -1;
	if (pipe(q) == -1) {
		CLOSE_PIPE(p);
		return -1;
	}

	cmd->pid = fork();	
	switch (cmd->pid) {
	case -1:
		CLOSE_PIPE(p);
		CLOSE_PIPE(q);
		return -1;
		
	case 0:
		if (dup2(p[0], 0) == -1 || dup2(q[1], 1) == -1)
			exit(1);
		(void) close(p[1]);
		(void) close(q[0]);
		execl("/bin/sh", "/bin/sh", "-c", cmd->cmdline, (char *) 0);
		exit(1);

	default:
		*tocmd = p[1];
		*fromcmd = q[0];
		(void) close(p[0]);
		(void) close(q[1]);
		return 0;
	}
	/*NOTREACHED*/
}



/*
 * Function:	kill_cmd
 * Purpose:	Make sure the child process is dead.
 */
static int kill_cmd(struct cmd *cmd) {
	int n, status;

	do {
		n = waitpid(cmd->pid, &status, 0);
	} while (n == -1 && errno == EAGAIN);	/* XXX man says ERESTARTSYS */
	if (n == -1) {
		error(0, "Error: %d: %.512s", errno, strerror(errno));
		return -1;
	}
	if (WIFSIGNALED(status)) {
		error(0, "Error: Child exited by signal %d", WTERMSIG(status));
		return -1;
	}
	if (!WIFEXITED(status)) {
		error(0, "Error: Child did not exit");
		return -1;
	}
	if (WEXITSTATUS(status) != 0) {
		error(0, "Error: child did not exit correctly: %d", 
			WEXITSTATUS(status));
		return -1;
	}
	return 0;
}



/*
 * Function:	check_read_write
 * Purpose:	Check if pipes can be read from or written to.
 * Return:	-1 for error, else the fd that is OK.
 * Note:	Wait reading or writing is OK.
 */
static int check_read_write(int tocmd, int fromcmd) {
	fd_set r, w;
	int n, num;
	
	if (tocmd < fromcmd)
		num = fromcmd + 1;
	else
		num = tocmd + 1;

	do {
		FD_ZERO(&r);
		FD_ZERO(&w);
		if (fromcmd != -1)
			FD_SET(fromcmd, &r);
		if (tocmd != -1)
			FD_SET(tocmd, &w);
		n = select(num, &r, &w, NULL, NULL);
	} while (n == -1 && errno == EINTR);

	if (n == -1)
		return -1;
	if (FD_ISSET(fromcmd, &r))
		return fromcmd;
	if (FD_ISSET(tocmd, &w))
		return tocmd;
	return -1;
}



/*
 * Function:	Read some data from pipe.
 * Return:	-1 for error, 0 for EOF, 1 for got data.
 */
static int read_some(int fd, struct dynarr *output) {
	int n;

	if (dynarr_resize(output, output->used + READ_BYTES) == -1) {
		error(0, "Error: out of memory");
		return -1;
	}
	
	n = read(fd, (char *) output->data + output->used, READ_BYTES);
	if (n == -1 || n == 0)
		return n;
	output->used += n;
	return 1;
}



/*
 * Function:	Write some data to pipe.
 * Return:	-1 for error, 1 for wrote data
 */
static int write_some(int fd, char *data, long max, long *written) {
	int n, nn;

	nn = WRITE_BYTES;
	if (nn > max - *written)
		nn = max - *written;

	n = write(fd, data + *written, nn);
	if (n == -1)
		return -1;
	*written += n;
	return 1;
}
