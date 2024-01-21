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
#include <X11/Shell.h>
#include <sys/wait.h>
#include "cache.h"
#include "child.h"
#include "codes.h"
#include "connect.h"
#include "file.h"
#include "save.h"
#include "server.h"
#include "tag.h"
#include "util.h"
#include "widgets.h"
#include "xutil.h"
#include "../Widgets/ArtText.h"
#include "../Widgets/FileSel.h"
#include "../Widgets/Knapp.h"
#include "../Widgets/Layout.h"
#include "../Widgets/Message.h"
#include "../Widgets/TextField.h"
#include "../Widgets/Toggle.h"
#include "../Widgets/Util.h"

typedef enum {
    SaveScopeArtwin,
    SaveScopeArticle,
    SaveScopeSubject,
    SaveScopeThread,
    SaveScopeSubthread,
    SaveScopeTagged
} SaveScope;

static struct {
    Widget	shell;
    /***/
    Widget	file_message;
    Widget	file_field;
    Widget	shell_message;
    Widget	shell_field;
    Widget	file_chooser;
    Widget	choose_knapp;
    Widget	ok_knapp;
    Widget	close_knapp;
    /***/
    Widget	bogus_from_toggle;
    Widget	bogus_subj_toggle;
    Widget	head_toggle;
    Widget	body_toggle;
    Widget	empty_toggle;
    /***/
    Widget	artwin_toggle;
    Widget	article_toggle;
    Widget	subject_toggle;
    Widget	thread_toggle;
    Widget	subthread_toggle;
    Widget	tagged_toggle;
} save_widgets;

static int	is_save = True;

/*************************************************************************/

static void set_save_label(int to_pipe)
{
    is_save = !to_pipe;
    KnappSetLabelNo(save_widgets.ok_knapp, !is_save, True);
}

static void get_email(ARTICLE *art, char *buf, int max_len)
{
    char	*c = art->from;
    char	*p1, *p2;
    int		len;

    if ((p1 = strchr(c, '<')) && (p2 = strchr(p1, '>'))) {
	len = p2 - p1 - 1;
	if (len > max_len)
	    len = max_len;
	memcpy(buf, p1 + 1, len);
	buf[len] = '\0';
	return;
    }

    p1 = strchr(c, '(');
    if (p1) {
	do {
	    p1--;
	} while (p1 > c && (*p1 == ' ' || *p1 == '\t'));

	if (p1 > c) {
	    len = p1 - c + 1;
	    if (len > max_len)
		len = max_len;
	    memcpy(buf, c, len);
	    buf[len] = '\0';
	    return;
	}
    }

    len = strlen(c);
    if (len > max_len)
	len = max_len;
    memcpy(buf, c, len);
    buf[len] = '\0';
}

static ARTICLE **get_art_list(SaveScope scope, long *n_arts)
{
    ARTICLE	*art = NULL, *top = NULL;
    ARTICLE	**result = NULL;
    SUBJECT	*subj = NULL;
    int		n, n_alloc;

    switch (scope) {
    case SaveScopeArtwin:
	result = (ARTICLE **)XtMalloc(sizeof result[0]);
	*result = NULL;
	*n_arts = 0;
	return result;
    case SaveScopeArticle:
	if (!global.curr_art)
	    set_message("No selected article!", True);
	else {
	    result = (ARTICLE **)XtMalloc(sizeof result[0]);
	    result[0] = global.curr_art;
	    *n_arts = 1;
	}
	return result;
    case SaveScopeTagged:
	{
	    ARTICLE **tagged = get_tagged_articles();

	    n_alloc = no_tagged_articles();
	    if (n_alloc <= 0) {
		set_message("No tagged articles!", True);
		return NULL;
	    }
	    result = (ARTICLE **)XtMalloc(n_alloc * sizeof result[0]);
	    for (n = 0 ; n < n_alloc ; n++)
		result[n] = tagged[n];
	    *n_arts = n;
	    return result;
	}
    case SaveScopeSubject:
	subj = global.curr_subj;
	if (!subj) {
	    set_message("No subject selected!", True);
	    return NULL;
	}
	art = subj->thread;
	break;
    case SaveScopeThread:
	if (!global.curr_subj) {
	    set_message("No thread selected!", True);
	    return NULL;
	}
	art = global.curr_subj->thread;
	break;
    case SaveScopeSubthread:
	if (!global.curr_art) {
	    set_message("No article selected!", True);
	    return NULL;
	}
	top = art = global.curr_art;
	break;
    }

    while (art && (!art->from || (subj && art->subject != subj)))
	if (top)
	    art = next_in_subthread_preorder(art, top);
	else
	    art = next_in_thread_preorder(art);

    if (!art)
	return NULL;

    n = 0;
    n_alloc = 16;
    result = (ARTICLE **)XtMalloc(n_alloc * sizeof result[0]);

    while (art) {
	if (n + 8 > n_alloc) {
	    n_alloc += 32;
	    result = (ARTICLE **)XtRealloc((char *)result,
					   n_alloc * sizeof result[0]);
	}

	result[n++] = art;

	do {
	    if (top)
		art = next_in_subthread_preorder(art, top);
	    else
		art = next_in_thread_preorder(art);
	} while (art && (!art->from || (subj && art->subject != subj)));
    }
    *n_arts = n;

    return result;
}

static char *save_article(FILE *fp, SERVER *server, ARTICLE *art, int what)
{
    char	*buffer;

    if (what & SAVE_BOGUS_FROM) {
	char	email[128];
	time_t	t = art->date;
	char	*c = ctime(&t);

	get_email(art, email, sizeof email - 1);
	fprintf(fp, "From %s %s", email, c);
    }

    if (what & SAVE_BOGUS_SUBJ)
	fprintf(fp, "Subject: %s\n", art->subject->subject);

    while ((buffer = server_read(server)) &&
	   buffer[0] != '\0' && !IS_DOT(buffer))
	if (what & SAVE_HEAD)
	    fprintf(fp, "%s\n", buffer);

    if (!buffer || IS_DOT(buffer))
	return buffer;

    if ((what & (SAVE_HEAD | SAVE_BODY)) == (SAVE_HEAD | SAVE_BODY))
	fputc('\n', fp);

    while ((buffer = server_read(server)) && !IS_DOT(buffer))
	if (what & SAVE_BODY) {
	    if ((what & SAVE_BOGUS_FROM) && memcmp(buffer, "From ", 5) == 0)
		fputc('>', fp);
	    fprintf(fp, "%s\n", buffer);
	}

    if (what & SAVE_EMPTY)
	fputc('\n', fp);

    return buffer;
}

int save_to_file(FILE *fp, char *message, ARTICLE **arts, long n,
		 int what, long *result)
{
    char	*p = message + strlen(message);
    long	i;

    result[1] = 0;

    for (i = 0 ; i < n ; i++) {
	char	*buffer;
	SERVER	*server;

	server = cache_get_server(arts[i]->no, False);
	if (!server) {
	    char	command[32];

	    server = main_server;
	    sprintf(command, "ARTICLE %ld\r\n", arts[i]->no);
	    buffer = server_comm(server, command, True);
	    if (!buffer)
		return -1;
	    if (atoi(buffer) != NNTP_OK_ARTICLE) {
		result[1]++;
		continue;
	    }
	}

	buffer = save_article(fp, server, arts[i], what);
	if (server != main_server)
	    server_free(server);
	else if (!buffer)
	    return -1;

	sprintf(p, "%ld", i + 1);
	set_message(message, False);
    }

    if (fflush(fp) < 0)
	result[0] = -1;
    else
	result[0] = n - result[1];

    return 0;
}

static int get_what(void)
{
    int	bogus_from = ToggleGet(save_widgets.bogus_from_toggle);
    int	bogus_subj = ToggleGet(save_widgets.bogus_subj_toggle);
    int	head       = ToggleGet(save_widgets.head_toggle);
    int	body       = ToggleGet(save_widgets.body_toggle);
    int	empty      = ToggleGet(save_widgets.empty_toggle);
    int	what = 0;

    if (bogus_from)
	what |= SAVE_BOGUS_FROM;
    if (bogus_subj)
	what |= SAVE_BOGUS_SUBJ;
    if (head)
	what |= SAVE_HEAD;
    if (body)
	what |= SAVE_BODY;
    if (empty)
	what |= SAVE_EMPTY;

    return what;
}

static int get_scope(void)
{
    if (ToggleGet(save_widgets.artwin_toggle))
	return SaveScopeArtwin;
    if (ToggleGet(save_widgets.article_toggle))
	return SaveScopeArticle;
    if (ToggleGet(save_widgets.subject_toggle))
	return SaveScopeSubject;
    if (ToggleGet(save_widgets.thread_toggle))
	return SaveScopeThread;
    if (ToggleGet(save_widgets.subthread_toggle))
	return SaveScopeSubthread;
    if (ToggleGet(save_widgets.tagged_toggle))
	return SaveScopeTagged;

    return SaveScopeArticle;
}

static void do_save(char *file_name, SaveScope scope, int what)
{
    FILE	*file;
    ARTICLE	**art_list = NULL;
    long	result[2], n;
    int		status, fclose_status;
    char	message[128];

    if (global.busy)
	return;

    if (global.mode != NewsModeGroup &&	global.mode != NewsModeThread) {
	set_message("Not in a newsgroup!", True);
	return;
    } else if (!file_name || file_name[0] == '\0') {
	set_message("No file specified!", True);
	return;
    } else if (!(what & (SAVE_HEAD | SAVE_BODY))) {
	set_message("Nothing to save (neither head or body selected)!", True);
	return;
    }

    art_list = get_art_list(scope, &n);
    if (!art_list)
	return;

    file = fopen_expand(file_name, "a", True);
    if (!file) {
	set_message("Failed to open file!", True);
	XtFree((char *)art_list);
	return;
    }

    sprintf(message, "Saving...   ");

    if (scope == SaveScopeArtwin) {
	status = ArtTextDumpToFile(main_widgets.text, file);
	result[0] = 1;
	result[1] = 0;
    } else {
	set_busy(True);
	status = save_to_file(file, message, art_list, n, what, result);
	unset_busy();
    }

    if ((fclose_status = fclose(file)) < 0)
	perror("knews: fclose");
    XtFree((char *)art_list);

    if (status < 0) {
	set_busy(True);
	reconnect_server(True);
	unset_busy();
	return;
    } else if (result[0] < 0 || fclose_status < 0) {
	set_message("File error!", True);
	return;
    } else if (result[1] > 0) {
	sprintf(message, "%ld articles saved, failed to get %ld articles.",
		result[0], result[1]);
	set_message(message, True);
    } else {
	sprintf(message, "%ld articles saved.", result[0]);
	set_message(message, False);
    }

    if (scope == SaveScopeTagged)
	clear_tagged_articles();
}

void pipe_context_callback(void *data, int status, char *stderr_buf)
{
    char	message[128];
    char	*cmd = data;
    int		ok = False;

    if (strlen(cmd) > 100)
	cmd[100] = '\0';

    if (WIFEXITED(status))
	switch (WEXITSTATUS(status)) {
	case 0:
	    sprintf(message, "'%s' exited ok.", cmd);
	    ok = True;
	    break;
	case 127:
	    sprintf(message, "Failed to start '%s'!", cmd);
	    break;
	default:
	    sprintf(message, "'%s' exited abnormally!", cmd);
	    break;
	}
    else if (WIFSIGNALED(status))
	sprintf(message, "'%s' caught %s!", cmd,
		signal_string(WTERMSIG(status)));
    else
	sprintf(message, "Unknown problem with '%s'!", cmd);

    if (!ok)
	stderr_popup(stderr_buf, -1);

    set_message(message, !ok);

    XtFree(cmd);
}

static void do_pipe(char *command, SaveScope scope, int what)
{
    FILE	*file = NULL;
    char	*file_name;
    ARTICLE	**art_list = NULL;
    long	result[2], n;
    int		status, fflush_status;
    char	message[128];
    pid_t	pid;
    char	*temp;

    if (global.busy)
	return;

    if (global.mode != NewsModeGroup &&	global.mode != NewsModeThread) {
	set_message("Not in a newsgroup!", True);
	return;
    } else if (!command || command[0] == '\0') {
	set_message("No command specified!", True);
	return;
    } else if (!(what & (SAVE_HEAD | SAVE_BODY))) {
	set_message("Nothing to pipe (neither head or body selected)!", True);
	return;
    }

    art_list = get_art_list(scope, &n);
    if (!art_list)
	return;

    file = create_temp_file(&file_name);
    if (!file) {
	set_message("Failed to create temporary file!", True);
	XtFree((char *)art_list);
	return;
    }
    unlink(file_name);

    sprintf(message, "Saving to temp file...   ");

    if (scope == SaveScopeArtwin) {
	status = ArtTextDumpToFile(main_widgets.text, file);
	result[0] = 1;
	result[1] = 0;
    } else {
	set_busy(True);
	status = save_to_file(file, message, art_list, n, what, result);
	unset_busy();
    }

    if ((fflush_status = fflush(file)) < 0)
	perror("knews: fflush");
    XtFree((char *)art_list);

    if (status < 0) {
	fclose(file);
	set_busy(True);
	reconnect_server(True);
	unset_busy();
	return;
    } else if (result[0] < 0 || fflush_status < 0) {
	fclose(file);
	set_message("Error with temp file!", True);
	return;
    } else if (result[1] > 0) {
	sprintf(message,
		"%ld articles saved to temp file, failed to get %ld "
		"articles.  Pipe started.",
		result[0], result[1]);
	set_message(message, True);
    } else {
	sprintf(message,
		"%ld articles saved to temp file.  Pipe started.",
		result[0]);
	set_message(message, False);
    }

    if (scope == SaveScopeTagged)
	clear_tagged_articles();

    temp = XtNewString(command);
    pid = fork_nicely(temp, pipe_context_callback,
		      global.stderr_timeout >= 0);

    if (pid < 0) {
	set_message("Fork failed!", True);
	XtFree(temp);
	fclose(file);
	return;
    }

    if (pid == 0) { /* child */
	int	fd;

	fd = fileno(file);
	if (fd != STDIN_FILENO) {
	    fd = dup2(fd, STDIN_FILENO);
	    if (fd < 0) {
		perror("knews: dup2");
		_exit(127);
	    }
	}

	if (lseek(fd, SEEK_SET, 0) < 0) {
	    perror("knews: lseek");
	    _exit(127);
	}

	execl(BIN_SH, "sh", "-c", command, (char *)NULL);
	perror("knews: execl " BIN_SH);
	_exit(127);
    }

    set_message("Pipe started.", False);    
    fclose(file);
}

static void save_callback(Widget w,
			  XtPointer client_data,
			  XtPointer call_data)
{
    char	*file_name = TextFieldGetBuffer(save_widgets.file_field);

    do_save(file_name, get_scope(), get_what());
    XtFree(file_name);
}

static void pipe_callback(Widget w,
			  XtPointer client_data,
			  XtPointer call_data)
{
    char	*command = TextFieldGetBuffer(save_widgets.shell_field);

    do_pipe(command, get_scope(), get_what());
    XtFree(command);
}

static void set_toggles_sensitive(int sens)
{
    XtSetSensitive(save_widgets.bogus_from_toggle, sens);
    XtSetSensitive(save_widgets.bogus_subj_toggle, sens);
    XtSetSensitive(save_widgets.head_toggle, sens);
    XtSetSensitive(save_widgets.body_toggle, sens);
    XtSetSensitive(save_widgets.empty_toggle, sens);
}

static void file_chooser_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{
    char	*buffer = (char *)call_data;
    Arg		arg;

    XtPopdown(w);

    if (!buffer)
	return;

    XtSetArg(arg, XtNbuffer, buffer);
    XtSetValues(save_widgets.file_field, &arg, 1);
    XtSetKeyboardFocus(save_widgets.shell, save_widgets.file_field);
    set_save_label(False);
}

static void choose_knapp_callback(Widget gw,
				  XtPointer client_data,
				  XtPointer call_data)
{
    if (!save_widgets.file_chooser) {
	Arg	args[4];

	XtSetArg(args[0], XtNcolormap, global.cmap);
	XtSetArg(args[1], XtNvisual, global.visual);
	XtSetArg(args[2], XtNdepth, global.depth);
	save_widgets.file_chooser =
	    XtCreatePopupShell("filechooser", fileSelWidgetClass,
			       main_widgets.shell, args, 3);
	XtAddCallback(save_widgets.file_chooser, XtNcallback,
		      file_chooser_callback, NULL);
    }

    XtPopup(save_widgets.file_chooser, XtGrabExclusive);
}

static void close_knapp_callback(Widget gw,
				 XtPointer client_data,
				 XtPointer call_data)
{
    XtPopdown(save_widgets.shell);
}

static void toggle_callback(Widget gw,
			    XtPointer client_data,
			    XtPointer call_data)
{
    Boolean	*set = (Boolean *)call_data;

    if (set)
	*set = !*set;
}

static void scope_toggle_callback(Widget w,
				  XtPointer client_data,
				  XtPointer call_data)
{
    Boolean	*set = (Boolean *)call_data;

    if (!set || *set)
	return;

    ToggleSet(save_widgets.artwin_toggle, False);
    ToggleSet(save_widgets.article_toggle, False);
    ToggleSet(save_widgets.subject_toggle, False);
    ToggleSet(save_widgets.thread_toggle, False);
    ToggleSet(save_widgets.subthread_toggle, False);
    ToggleSet(save_widgets.tagged_toggle, False);
    ToggleSet(w, True);
    
    *set = True;
    set_toggles_sensitive(w != save_widgets.artwin_toggle);
}

static void knapp_callback(Widget w,
			   XtPointer client_data,
			   XtPointer call_data)
{
    (is_save ? save_callback : pipe_callback)(w, client_data, call_data);
}

static void focus_callback(Widget w,
			   XtPointer client_data,
			   XtPointer call_data)
{
    if (w == save_widgets.shell_field)
	set_save_label(True);
    else if (w == save_widgets.file_field)
	set_save_label(False);
}

static void tab_callback(Widget w,
			 XtPointer client_data,
			 XtPointer call_data)
{
    if (w == save_widgets.shell_field) {
	XtSetKeyboardFocus(save_widgets.shell, save_widgets.file_field);
	set_save_label(True);
    } else if (w == save_widgets.file_field) {
	XtSetKeyboardFocus(save_widgets.shell, save_widgets.shell_field);
	set_save_label(False);
    }
}

/*************************************************************************/

static void create_save_widgets(void)
{
    Arg		args[8];
    Widget	layout, w;

    XtSetArg(args[0], XtNallowShellResize, True);
    XtSetArg(args[1], XtNinput, True);
    XtSetArg(args[2], XtNtitle, "knews: save/pipe");
    XtSetArg(args[3], XtNiconName, "save/pipe");
    XtSetArg(args[4], XtNcolormap, global.cmap);
    XtSetArg(args[5], XtNvisual, global.visual);
    XtSetArg(args[6], XtNdepth, global.depth);
    save_widgets.shell =
	XtCreatePopupShell("saveshell", topLevelShellWidgetClass,
			   main_widgets.shell, args, 7);

    save_widgets.file_chooser = NULL;

    layout =
	XtVaCreateManagedWidget("savelayout", layoutWidgetClass,
				save_widgets.shell,
				XtVaTypedArg, XtNlayout, XtRString,
#include "layouts/save.h"
				(int)sizeof(String), (void *)0);

    XtSetArg(args[0], XtNcenter, True);
    XtCreateManagedWidget("message", messageWidgetClass,
			  layout, args, 1);

    XtSetArg(args[0], XtNcenter, False);
    save_widgets.shell_message =
	XtCreateManagedWidget("shellmessage", messageWidgetClass,
			      layout, args, 1);

    XtSetArg(args[0], XtNfocusRoot, save_widgets.shell);
    XtSetArg(args[1], XtNsingleLine, True);
    save_widgets.shell_field =
	XtCreateManagedWidget("shellfield", textFieldWidgetClass,
			      layout, args, 2);
    XtAddCallback(save_widgets.shell_field, XtNcallback,
		  pipe_callback, False);
    XtAddCallback(save_widgets.shell_field, XtNfocusCallback,
		  focus_callback, NULL);
    XtAddCallback(save_widgets.shell_field, XtNtabCallback,
		  tab_callback, NULL);

    XtSetArg(args[0], XtNcenter, False);
    save_widgets.file_message =
	XtCreateManagedWidget("filemessage", messageWidgetClass,
			      layout, args, 1);

    XtSetArg(args[0], XtNfocusRoot, save_widgets.shell);
    XtSetArg(args[1], XtNsingleLine, True);
    save_widgets.file_field =
	XtCreateManagedWidget("filefield", textFieldWidgetClass,
			      layout, args, 2);
    XtAddCallback(save_widgets.file_field, XtNcallback,
		  save_callback, NULL);
    XtAddCallback(save_widgets.file_field, XtNfocusCallback,
		  focus_callback, NULL);
    XtAddCallback(save_widgets.file_field, XtNtabCallback,
		  tab_callback, NULL);

    XtSetArg(args[0], XtNresizable, False);
    save_widgets.ok_knapp =
	XtCreateManagedWidget("ok", knappWidgetClass, layout, args, 1);
    XtAddCallback(save_widgets.ok_knapp, XtNcallback,
		  knapp_callback, NULL);

    save_widgets.close_knapp =
	XtCreateManagedWidget("close", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.close_knapp, XtNcallback,
		  close_knapp_callback, NULL);

    save_widgets.choose_knapp =
	XtCreateManagedWidget("choose", knappWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.choose_knapp, XtNcallback,
		  choose_knapp_callback, NULL);

    /***/

    save_widgets.bogus_from_toggle =
	XtCreateManagedWidget("bogusfrom", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.bogus_from_toggle, XtNcallback,
		  toggle_callback, NULL);

    save_widgets.bogus_subj_toggle =
	XtCreateManagedWidget("bogussubj", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.bogus_subj_toggle, XtNcallback,
		  toggle_callback, NULL);

    save_widgets.head_toggle =
	XtCreateManagedWidget("header", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.head_toggle, XtNcallback,
		  toggle_callback, NULL);

    save_widgets.body_toggle =
	XtCreateManagedWidget("body", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.body_toggle, XtNcallback,
		  toggle_callback, NULL);

    save_widgets.empty_toggle =
	XtCreateManagedWidget("empty", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.empty_toggle, XtNcallback,
		  toggle_callback, NULL);

    /***/

    save_widgets.artwin_toggle =
	XtCreateManagedWidget("artwin", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.artwin_toggle, XtNcallback,
		  scope_toggle_callback, NULL);

    save_widgets.article_toggle =
	XtCreateManagedWidget("article", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.article_toggle, XtNcallback,
		  scope_toggle_callback, NULL);

    save_widgets.subject_toggle =
	XtCreateManagedWidget("subject", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.subject_toggle, XtNcallback,
		  scope_toggle_callback, NULL);

    save_widgets.thread_toggle =
	XtCreateManagedWidget("thread", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.thread_toggle, XtNcallback,
		  scope_toggle_callback, NULL);

    save_widgets.subthread_toggle =
	XtCreateManagedWidget("subthread", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.subthread_toggle, XtNcallback,
		  scope_toggle_callback, NULL);

    save_widgets.tagged_toggle =
	XtCreateManagedWidget("tagged", toggleWidgetClass, layout, NULL, 0);
    XtAddCallback(save_widgets.tagged_toggle, XtNcallback,
		  scope_toggle_callback, NULL);

    w = save_widgets.article_toggle;
    if (ToggleGet(save_widgets.artwin_toggle))
	w = save_widgets.artwin_toggle;
    if (ToggleGet(save_widgets.subject_toggle))
	w = save_widgets.subject_toggle;
    if (ToggleGet(save_widgets.thread_toggle))
	w = save_widgets.thread_toggle;
    if (ToggleGet(save_widgets.subthread_toggle))
	w = save_widgets.subthread_toggle;
    if (ToggleGet(save_widgets.tagged_toggle))
	w = save_widgets.tagged_toggle;
    ToggleSet(save_widgets.artwin_toggle, False);
    ToggleSet(save_widgets.article_toggle, False);
    ToggleSet(save_widgets.subject_toggle, False);
    ToggleSet(save_widgets.thread_toggle, False);
    ToggleSet(save_widgets.subthread_toggle, False);
    ToggleSet(save_widgets.tagged_toggle, False);
    ToggleSet(w, True);

    XtSetKeyboardFocus(save_widgets.shell, save_widgets.file_field);
    XtRealizeWidget(save_widgets.shell);
    XtInstallAllAccelerators(save_widgets.shell, save_widgets.shell);

    add_WM_DELETE_WINDOW_callback(save_widgets.shell,
				  close_knapp_callback, NULL);

    if (global.busy)
	set_busy_save(True);
}

void popup_save(void)
{
    if (!save_widgets.shell)
	create_save_widgets();

    XtPopup(save_widgets.shell, XtGrabNone);

    if (global.busy)
	set_busy_save(True);
}

void popdown_save(void)
{
    if (save_widgets.shell)
	XtPopdown(save_widgets.shell);
}

static void do_action(int save, String *params, Cardinal n)
{
    int		what = 0;
    char	*c;
    SaveScope	scope = SaveScopeArticle;

    if (n == 0) {
	if (save)
	    set_message("No filename specified in save/pipe action!",
			True);
	else
	    set_message("No shell command specified in save/pipe action!",
			True);
	return;
    } else if (n == 1) {
	set_message("To few arguments to save/pipe action!", True);
	return;
    }

    c = params[1];
    while (*c != '\0') {
	switch (*c++) {
	case 'f':
	case 'F':
	    what |= SAVE_BOGUS_FROM;
	    break;
	case 'S':
	case 's':
	    what |= SAVE_BOGUS_SUBJ;
	    break;
	case 'H':
	case 'h':
	    what |= SAVE_HEAD;
	    break;
	case 'B':
	case 'b':
	    what |= SAVE_BODY;
	    break;
	case 'E':
	case 'e':
	    what |= SAVE_EMPTY;
	    break;
	}
    }

    if (!(what & (SAVE_BODY|SAVE_HEAD)) ||
	!params[0] || params[0][0] == '\0') {
	if (save)
	    set_message("Nothing to save!", True);
	else
	    set_message("Nothing to pipe!", True);
	return;
    }

    if (n >= 3) {
	switch (params[2][0]) {
	case 'W':
	case 'w':
	    scope = SaveScopeArtwin;
	    break;
	case 'A':
	case 'a':
	    scope = SaveScopeArticle;
	    break;
	case 'S':
	case 's':
	    if ((params[2][1] == 'U' || params[2][1] == 'u') &&
		(params[2][2] == 'B' || params[2][2] == 'b')) {
		switch (params[2][3]) {
		case 'J':
		case 'j':
		    scope = SaveScopeSubject;
		    break;
		case 'T':
		case 't':
		    scope = SaveScopeSubthread;
		    break;
		}
	    }
	    break;
	case 'T':
	case 't':
	    switch (params[2][1]) {
	    case 'A':
	    case 'a':
		scope = SaveScopeTagged;
		break;
	    case 'H':
	    case 'h':
		scope = SaveScopeThread;
		break;
	    }
	    break;
	}
    }

    if (save)
	do_save(params[0], scope, what);
    else
	do_pipe(params[0], scope, what);
}

void action_save(Widget w, XEvent *string,
		 String *params, Cardinal *no_params)
{
    do_action(True, params, *no_params);
}

void action_pipe(Widget w, XEvent *string,
		 String *params, Cardinal *no_params)
{
    do_action(False, params, *no_params);
}

/*************************************************************************/

void set_busy_save(int busy)
{
    if (!save_widgets.shell)
	return;

    XDefineCursor(display, XtWindow(save_widgets.shell),
		  busy ? global.busy_cursor : global.cursor);
    KnappSetActive(save_widgets.ok_knapp, !busy);
}

/************************************************************************/

/*
 *  Lots of people enclose URLs in <> or "".  Also, don't allow
 *  quotes for sequrity reasons, since we're passing it to the shell.
 */

#define IS_URL_CHAR(c) (!strchr(" \t<>\"'`\\()[]{}&", (c)))
#define MIN_URL_LEN    4

void text_url_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
    ArtTextUrlReport	*report  = (ArtTextUrlReport *)call_data;
    char		*url_cmd = global.url_command;
    char		*temp;
    long		i;
    pid_t		pid;

    if (!url_cmd) {
	set_message("Error: Resource Knews.urlCommand not set!", True);
	return;
    }

    /*
     *  Very crude parser...
     */

    if (!report->sel_ok) {
	const char	*c;

	c = report->line + report->start;
	if (*c != '\0' && IS_URL_CHAR(*c)) {
	    while (*++c != '\0' && IS_URL_CHAR(*c))
		report->stop++;

	    c = report->line + report->start;
	    while (report->start > 0 && IS_URL_CHAR(*--c))
		report->start--;

	    if (report->start + 4 < report->stop &&
		strncmp(report->line + report->start, "URL:", 4) == 0)
		report->start += 4;

	    if (report->start < report->stop &&
		report->line[report->stop] == '.')
		report->stop--;
	}
    }

    if (report->stop - report->start < MIN_URL_LEN) {
	set_message("Url to short!", True);
	return;
    }

    for (i = report->start ; i <= report->stop ; i++)
	if (!IS_URL_CHAR(report->line[i])) {
	    set_message("Illegal character in URL!", True);
	    return;
	}

    report->sel_ok = True;

    temp = XtNewString("urlCommand");
    pid = fork_nicely(temp, pipe_context_callback,
		      global.stderr_timeout >= 0);

    if (pid < 0) {
	set_message("Error: fork failed!", True);
	XtFree(temp);
	return;
    }

    if (pid == 0) {
	char	*prog, *url;
	long	i, n, url_len;

	url_len = report->stop - report->start + 1;
	url = malloc(url_len + 8);
	n = 1024;
	prog = malloc(1024);
	if (!url || !prog) {
	    fputs("knews: out of memory!\n", stderr);
	    _exit(127);
	}

	memcpy(url, report->line + report->start, url_len);
	url[url_len] = '\0';

	for (i = 0 ; *url_cmd != '\0' ; url_cmd++) {
	    if (i + url_len + 8 > n) {
		n += i + url_len + 8;
		prog = realloc(prog, n);
		if (!prog) {
		    fputs("knews: out of memory!\n", stderr);
		    _exit(127);
		}
	    }

	    if (*url_cmd == '%' && *++url_cmd == 's') {
		memcpy(prog + i, url, url_len);
		i += url_len;
	    } else {
		prog[i++] = *url_cmd;
	    }
	}
	prog[i] = '\0';

	execl(BIN_SH, "sh", "-c", prog, (char *)0);
	perror("knews: execl " BIN_SH);
	_exit(127);
    }

    set_message("Executing urlCommand.", False);
}
