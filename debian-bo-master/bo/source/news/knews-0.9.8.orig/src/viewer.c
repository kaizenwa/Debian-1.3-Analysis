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
#include "file.h"
#include "font.h"
#include "gif.h"
#include "jpeg.h"
#include "mailcap.h"
#include "png.h"
#include "save.h"
#include "viewer.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/Dialogue.h"
#include "../Widgets/TextField.h"

typedef struct {
    char	*data;
    Pixmap	 pixmap;
    long	 len;
    char	*view_cmd;
    char	*file_name;
    int		 ref_count;
} VIEWER;

static void viewer_destroy(VIEWER *v)
{
    if (--v->ref_count > 0)
	return;

    XtFree(v->data);
    XtFree(v->view_cmd);
    XtFree(v->file_name);
    if (v->pixmap != None) {
	XFreePixmap(display, v->pixmap);
	v->pixmap = None;
    }
    v->data      = NULL;
    v->len       = 0;
    v->view_cmd  = NULL;
    v->file_name = NULL;
    v->ref_count = 1;
    XtFree((char *)v);
}

static void mime_dialogue_callback(Widget w,
				   XtPointer client_data,
				   XtPointer call_data)
{
    DialogueReport	*report = (DialogueReport *)call_data;
    VIEWER		*v = (VIEWER *)client_data;
    char		*cmd;
    long		n;
    int			fd;
    pid_t		pid;

    switch (report->reply) {
    case DialogueReplyLeft:   /* save */
	if (!report->buffer || report->buffer[0] == '\0')
	    break;

	fd = open_expand(report->buffer, O_WRONLY|O_CREAT|O_TRUNC, True);
	if (fd < 0) {
	    set_message("Error: Failed to open file!", True);
	    return;
	}

	n = writen(fd, v->data, v->len);
	close(fd);
	if (n < 0) {
	    set_message("Error: Failed to write file!", True);
	    return;
	}
	set_message("Saved OK.", False);
	break;
    case DialogueReplyMiddle: /* pipe */
	if (!report->buffer || report->buffer[0] == '\0')
	    break;

	cmd = XtNewString(report->buffer);
	pid = fork_nicely(cmd, pipe_context_callback,
			  global.stderr_timeout >= 0);
	if (pid < 0) {
	    set_message("Error: fork failed!", True);
	    XtFree(cmd);
	    return;
	}

	if (pid == 0) { /* child */
	    char	*file_name;

	    fd = create_temp_fd(&file_name);
	    if (fd < 0) {
		perror("knews: open");
		_exit(127);
	    }
	    if (writen(fd, v->data, v->len) < 0)
		_exit(127);
	    unlink(file_name);
	    if (lseek(fd, SEEK_SET, 0) < 0) {
		perror("knews: lseek");
		_exit(127);
	    }
	    if (fd != STDIN_FILENO) {
		if (dup2(fd, STDIN_FILENO) < 0) {
		    perror("knews: dup2");
		    _exit(127);
		}
		close(fd);
	    }

	    execl(BIN_SH, "sh", "-c", report->buffer, (char *)NULL);
	    perror("knews: execl " BIN_SH);
	    _exit(127);
	}

	/*
	 *  Parent.
	 */
	set_message("Pipe started.", False);
	break;
    case DialogueReplyEnter:  /* do nothing */
	/* maybe we should default to either save or pipe ? */
	return; /* don't fall through */
    case DialogueReplyTab:
	return; /* don't fall through */
    case DialogueReplyRight:
    case DialogueReplyClose:  /* cancel*/
	break;
    }

    viewer_destroy(v);
    XtDestroyWidget(w);
}

static void click_callback(Widget	w,
			   XtPointer	client_data,
			   XtPointer	call_data)
{
    VIEWER	*v       = (VIEWER *)client_data;
    int		*button  = (int *)call_data;
    char	*cmd;
    pid_t	pid;

    if (!button || *button <= 0) {
	viewer_destroy(v);
	return;
    }

    if (!v->view_cmd || *button == 3) {
	Widget	w;

	v->ref_count++;
	w = popup_dialogue("mimedialogue", "Save to file or pipe to shell:",
			   "Save", "Pipe", "Cancel", mime_dialogue_callback,
			   (XtPointer)v, XtGrabNone);
	if (v->file_name) {
	    w = DialogueGetTextField(w);
	    TextFieldSetBuffer(w, v->file_name);
	}

	return;
    }

    cmd = XtNewString(v->view_cmd);
    pid = fork_nicely(cmd, pipe_context_callback, True);

    if (pid < 0) {
	perror("knews: fork");
	set_message("Failed to start viewer!", True);
	return;
    }

    if (pid == 0) { /* child */
	char	*file_name;
	char	*view_cmd = v->view_cmd;
	int	fd;

        fd = create_temp_fd(&file_name);
	if (fd < 0) {
	    perror("knews: open");
	    _exit(127);
	}

	if (writen(fd, v->data, v->len) < 0)
	    _exit(127);

	if (strstr(v->view_cmd, "%s")) {
	    char	tmpl = 's';

	    view_cmd = expn_tmpl(v->view_cmd, 1, &tmpl, &file_name);
	} else {
	    unlink(file_name);
	    if (lseek(fd, SEEK_SET, 0) < 0) {
		perror("knews: lseek");
		_exit(127);
	    }
	    if (fd != STDIN_FILENO) {
		if (dup2(fd, STDIN_FILENO) < 0) {
		    perror("knews: dup2");
		    _exit(127);
		}
		close(fd);
	    }
	}

	execl(BIN_SH, "sh", "-c", view_cmd, (char *)NULL);
	perror("knews: execl " BIN_SH);
	_exit(127);
    }

    /* parent */
    set_message("Viewer started.", False);
}

void do_viewer(char *type,
	       char *subtype,
	       char *view_cmd,
	       char *file_name,
	       char *data,
	       long  len)
{
    VIEWER	*v;
    long	w = 0, h = 0;

    v = (VIEWER *)XtMalloc(sizeof *v);
    v->pixmap    = None;
    v->data      = data;
    v->len       = len;
    v->view_cmd  = view_cmd;
    v->file_name = file_name;
    v->ref_count = 1;

    if (global.inline_images && strcmp(type, "image") == 0)
	if (strcmp(subtype, "jpeg") == 0)
	    v->pixmap = do_jpeg(data, len, &w, &h);
	else if (strcmp(subtype, "gif") == 0)
	    v->pixmap = do_gif(data, len, &w, &h);
	else if (strcmp(subtype, "png") == 0)
	    v->pixmap = do_png(data, len, &w, &h);

    if (v->pixmap)
	ArtTextAddImage(main_widgets.text, v->pixmap, w, h, click_callback, v);
    else {
	if (!v->view_cmd)
	    ArtTextAddClickable(main_widgets.text,
				"[knews: no mailcap entry.]  Save or pipe.",
				ascii_font->header_font,
				global.clickable_pixel, click_callback, v);
	else {
	    ArtTextAddClickable(main_widgets.text, "View with:  ",
				ascii_font->header_font,
				global.clickable_pixel, click_callback, v);
	    ArtTextAppendToLast(main_widgets.text, v->view_cmd);
	}
	ArtTextAddLine(main_widgets.text, "",
		       ascii_font->body_font, global.pixel);
    }
}
