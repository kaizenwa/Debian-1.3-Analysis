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
#include "../Widgets/Util.h"
#include "actions.h"
#include "ahead.h"
#include "child.h"
#include "color.h"
#include "connect.h"
#include "domain.h"
#include "file.h"
#include "font.h"
#include "k_action.h"
#include "mailcap.h"
#include "misc.h"
#include "p_menu.h"
#include "procs.h"
#include "read.h"
#include "resource.h"
#include "save.h"
#include "server.h"
#include "tag.h"
#include "thread.h"
#include "uudecode.h"
#include "widgets.h"
#include "xutil.h"

#include "sysdeps.h"

struct Global	global = {0, };
XtAppContext	app_cont;
Display		*display = NULL;
struct SERVER	*main_server = NULL;

/*
 *  Icon courtesy of Matthias Schuetze
 */

static void set_icon(Widget shell)
{
    Arg	      arg[2];
    Pixmap    icon, mask;
#include "knews_icon.xbm"
#include "knews_mask.xbm"

    icon =
	XCreateBitmapFromData(display, DefaultRootWindow(display),
			      (char *)knews_icon_bits,
			      knews_icon_width, knews_icon_height);
    mask =
	XCreateBitmapFromData(display, DefaultRootWindow(display),
			      (char *)knews_mask_bits,
			      knews_mask_width, knews_mask_height);

    XtSetArg(arg[0], XtNiconPixmap, icon);
    XtSetArg(arg[1], XtNiconMask, mask);
    XtSetValues(shell, arg, 2);

#undef knews_icon_width
#undef knews_icon_height
#undef knews_mask_width
#undef knews_mask_height
}

static int disp_io_error_handler(Display *disp)
{
    char	*msg;

    block_sighup();
    perror("knews: Fatal I/O error or KillClient");
    fputs("       Updating newsrc and kill files...  ", stderr);
    msg = do_update();
    if (!msg)
	msg = "OK";
    fprintf(stderr, "%s\n", msg);
    exit(1);
}

static int disp_error_handler(Display *disp, XErrorEvent *event)
{
    char	buffer[256];
    char	number[32];

    XGetErrorText(disp, event->error_code, buffer, sizeof(buffer));
    fprintf(stderr,
	    "X Error of failed request               : %s\n",
	    buffer);

    sprintf(number, "%d", event->request_code);
    XGetErrorDatabaseText(disp, "XRequest", number, "",
			  buffer, sizeof(buffer));
    fprintf(stderr,
	    "  Major opcode of failed request        :  %d (%s)\n",
	    event->request_code, buffer);

    switch (event->error_code) {
    case BadValue:
	fprintf(stderr, "  Resource id in failed request         :  ");
	break;
    case BadAtom:
	fprintf(stderr, "  Value in failed request               :  ");
	break;
    default:
	fprintf(stderr, "  Atom id in failed request             :  ");
	break;
    }
    fprintf(stderr, "0x%lx\n", event->resourceid);

    fputs("knews: exiting!\n", stderr);
    exit(1);
}

static void init_timer(XtPointer client_data, XtIntervalId *id)
{
    if (main_widgets.second_shell)
	XtPopup(main_widgets.second_shell, XtGrabNone);
    unset_busy();
    if (global.nntp_server)
	connect_server();
    else
	res_load(NULL);
}

int main(int argc, char *argv[])
{
#ifndef DEFAULT_NNTPSERVER
#define DEFAULT_NNTPSERVER 0
#endif
#ifndef DEFAULT_EDIT_COMMAND
#define DEFAULT_EDIT_COMMAND "xterm -e vi +%i %s"
#endif
    static XtResource resource_spec[] = {
#define offset(field) XtOffsetOf(struct Global, field)
	{"nntpServer", "NntpServer", XtRString, sizeof(String),
	 offset(nntp_server), XtRImmediate, (XtPointer)DEFAULT_NNTPSERVER},
	{"configNntpServer", "ConfigNntpServer", XtRString, sizeof(String),
	 offset(config_nntp_server), XtRImmediate, (XtPointer)NULL},
	{"editCommand", "EditCommand", XtRString, sizeof(String),
	 offset(edit_command), XtRImmediate, (XtPointer)DEFAULT_EDIT_COMMAND},
	{"urlCommand", "UrlCommand", XtRString, sizeof(String),
	 offset(url_command), XtRImmediate, (XtPointer)NULL},
	{"printCommand", "PrintCommand", XtRString, sizeof(String),
	 offset(print_command), XtRImmediate, (XtPointer)NULL},
	{"needsTerminal", "NeedsTerminal", XtRString, sizeof(String),
	 offset(needs_terminal), XtRImmediate, (XtPointer)NULL},
	{"copiousOutput", "CopiousOutput", XtRString, sizeof(String),
	 offset(copious_output), XtRImmediate, (XtPointer)NULL},
	{"mailName", "MailName", XtRString, sizeof(String),
	 offset(mail_name), XtRImmediate, (XtPointer)NULL},
	{"configFile", "ConfigFile", XtRString, sizeof(String),
	 offset(config_file), XtRImmediate, (XtPointer)NULL},
	{"busyCursor", "BusyCursor", XtRCursor, sizeof(Cursor),
	 offset(busy_cursor), XtRString, (XtPointer)"watch"},
	{"cursor", "Cursor", XtRCursor, sizeof(Cursor),
	 offset(cursor), XtRString, (XtPointer)"top_left_arrow"},
	{"stderrTimeout", "StderrTimeout", XtRLong, sizeof(long),
	 offset(stderr_timeout), XtRImmediate, (XtPointer)10000},
	{"chunkSize", "ChunkSize", XtRInt, sizeof(int),
	 offset(chunk_size), XtRImmediate, (XtPointer)16},
	{"postMiscMenuSize", "PostMiscMenuSize", XtRInt, sizeof(int),
	 offset(post_misc_menu_size), XtRImmediate, (XtPointer)1},
	{"extraMenuSize", "ExtraMenuSize", XtRInt, sizeof(int),
	 offset(extra_menu_size), XtRImmediate, (XtPointer)0},
	{"typeMenuSize", "TypeMenuSize", XtRInt, sizeof(int),
	 offset(type_menu_size), XtRImmediate, (XtPointer)0},
	{"forwardMenuSize", "ForwardMenuSize", XtRInt, sizeof(int),
	 offset(forward_menu_size), XtRImmediate, (XtPointer)0},
	{"nCols", "NCols", XtRInt, sizeof(int),
	 offset(n_cols), XtRImmediate, (XtPointer)(5*5*5 - 5 + 17)},
	{"knewsVersion", "KnewsVersion", XtRString, sizeof(String),
	 offset(version), XtRImmediate, (XtPointer)NULL},
	{"separateWindows", "SeparateWindows", XtRBoolean, sizeof(Boolean),
	 offset(separate_windows), XtRImmediate, (XtPointer)False},
	{"bell", "Bell", XtRBoolean, sizeof(Boolean),
	 offset(bell), XtRImmediate, (XtPointer)True},
	{"headDebug", "Debug", XtRBoolean, sizeof(Boolean),
	 offset(head_debug), XtRImmediate, (XtPointer)False},
	{"useIcon", "UseIcon", XtRBoolean, sizeof(Boolean),
	 offset(use_icon), XtRImmediate, (XtPointer)True},
	{"defaultHotColor", "DefaultHotColor", XtRPixel, sizeof(Pixel),
	 offset(default_hot_pixel), XtRString, XtDefaultForeground},
	{"retrieveDescriptions", "RetrieveDescriptions",
	 XtRString, sizeof(String),
	 offset(retrieve_descr), XtRImmediate, (XtPointer)NULL},
	{"readActiveFile", "ReadActiveFile", XtRString, sizeof(String),
	 offset(read_active_file), XtRImmediate, (XtPointer)NULL},
	{"fillNewsrcFile", "FillNewsrcFile", XtRString, sizeof(String),
	 offset(fill_newsrc_file), XtRImmediate, (XtPointer)NULL},
	{"showNumberLines", "ShowNumberLines", XtRString, sizeof(String),
	 offset(show_number_lines), XtRImmediate, (XtPointer)NULL},
	{"keepThreadInfo", "KeepThreadInfo", XtRString, sizeof(String),
	 offset(keep_thread_info), XtRImmediate, (XtPointer)NULL},
	{"checkForNewGroups", "CheckForNewGroups", XtRString, sizeof(String),
	 offset(check_for_new_groups), XtRImmediate, (XtPointer)NULL},
	{"confirmQuit", "Confirm", XtRBoolean, sizeof(Boolean),
	 offset(confirm_quit), XtRImmediate, (XtPointer)False},
	{"confirmCatchup", "Confirm", XtRBoolean, sizeof(Boolean),
	 offset(confirm_catchup), XtRImmediate, (XtPointer)False},
	{"confirmQuitGroup", "ConfirmQuit", XtRBoolean, sizeof(Boolean),
	 offset(confirm_quit_group), XtRImmediate, (XtPointer)NULL},
	{"icaseRegexps", "IcaseRegexps", XtRBoolean, sizeof(Boolean),
	 offset(icase_regexps), XtRImmediate, (XtPointer)True},
	{"showCache", "ShowCache", XtRBoolean, sizeof(Boolean),
	 offset(show_cache), XtRImmediate, (XtPointer)False},
	{"bogusFileSystem", "BogusFileSystem", XtRBoolean, sizeof(Boolean),
	 offset(bogus_file_system), XtRImmediate, (XtPointer)False},
	{"generatePath", "GeneratePath", XtRBoolean, sizeof(Boolean),
	 offset(generate_path), XtRImmediate, (XtPointer)False},
	{"quoteEmpty", "QuoteEmpty", XtRBoolean, sizeof(Boolean),
	 offset(quote_empty), XtRImmediate, (XtPointer)True},
	{"sortGroups", "SortGroups", XtRBoolean, sizeof(Boolean),
	 offset(sort_groups), XtRImmediate, (XtPointer)False},
	{"inlineImages", "InlineImages", XtRBoolean, sizeof(Boolean),
	 offset(inline_images), XtRImmediate, (XtPointer)True},
	{"colorHack", "Hack", XtRBoolean, sizeof(Boolean),
	 offset(color_hack), XtRImmediate, (XtPointer)False},
	{"mimeForward", "MimeForward", XtRBoolean, sizeof(Boolean),
	 offset(mime_forward), XtRImmediate, (XtPointer)True},
	{"newsrcTemplate", "NewsrcTemplate", XtRString, sizeof(String),
	 offset(newsrc_templ), XtRImmediate, (XtPointer)NULL},
	{"oldNewsrcTemplate", "OldNewsrcTemplate", XtRString, sizeof(String),
	 offset(old_newsrc_templ), XtRImmediate, (XtPointer)NULL},
	{"killFileTemplate", "KillFileTemplate", XtRString, sizeof(String),
	 offset(kill_file_templ), XtRImmediate, (XtPointer)"~/.kill-%s"},
	{"groupKillFileTemplate", "GroupKillFileTemplate",
	 XtRString, sizeof(String),
	 offset(group_kill_file_templ), XtRImmediate,
	 (XtPointer)"~/.knews/%s/%g/KILL"},
	{"autoSubscribe", "AutoSubscribe", XtRString, sizeof(String),
	 offset(auto_subscribe), XtRImmediate,
	 (XtPointer)"news.answers:\nnews.newusers.questions:\n"},
	{"mimeTypes", "MimeTypes", XtRString, sizeof(String),
	 offset(mime_types), XtRImmediate, (XtPointer)NULL},
	/******/
	{"foreground", "Foreground", XtRPixel, sizeof(Pixel),
	 offset(pixel), XtRString, XtDefaultForeground},
	{"quoteColor", "Foreground", XtRPixel, sizeof(Pixel),
	 offset(quote_pixel), XtRString, XtDefaultForeground},
	{"headerColor", "Foreground", XtRPixel, sizeof(Pixel),
	 offset(header_pixel), XtRString, XtDefaultForeground},
	{"alertColor", "Foreground", XtRPixel, sizeof(Pixel),
	 offset(alert_pixel), XtRString, XtDefaultForeground},
	{"clickableColor", "ClickableColor", XtRPixel, sizeof(Pixel),
	 offset(clickable_pixel), XtRString, XtDefaultForeground},
#undef offset
    };
    static XtActionsRec actions[] = {
	{"tree-up",			action_tree_up},
	{"tree-down",			action_tree_down},
	{"tree-left",			action_tree_left},
	{"tree-right",			action_tree_right},
	{"tree-down-right",		action_tree_down_right},
	{"followup",			action_followup},
	{"reply",			action_reply},
	{"followup-and-reply",		action_followup_and_reply},
	{"post-new",			action_post_new},
	{"forward-by-mail",		action_forward_by_mail},
	{"list-up",			action_list_up},
	{"list-down",			action_list_down},
	{"tree-or-list-up",		action_tree_or_list_up},
	{"tree-or-list-down",		action_tree_or_list_down},
	{"enter-mode",			action_enter_mode},
	{"exit-mode",			action_exit_mode},
	{"tree-left-or-exit-mode",	action_tree_left_or_exit_mode},
	{"tree-right-or-enter-mode",	action_tree_right_or_enter_mode},
	{"read-article",		action_read_article},
	{"mime-hack",			action_mime_hack},
	{"goto-next-hot",		action_goto_next_hot},
	{"view-thread",			action_view_thread},
	{"mark-read-article",		action_mark_read_article},
	{"mark-read-subject",		action_mark_read_subject},
	{"mark-read-thread",		action_mark_read_thread},
	{"mark-read-subthread",		action_mark_read_subthread},
	{"mark-read-tagged",		action_mark_read_tagged},
	{"mark-read-all",		action_mark_read_all},
	{"mark-read-to-current",	action_mark_read_to_current},
	{"mark-read-non-tagged",	action_mark_read_non_tagged},
	{"mark-read-cold",		action_mark_read_cold},
	{"mark-unread-article",		action_mark_unread_article},
	{"mark-unread-subject",		action_mark_unread_subject},
	{"mark-unread-thread",		action_mark_unread_thread},
	{"mark-unread-subthread",	action_mark_unread_subthread},
	{"mark-unread-tagged",		action_mark_unread_tagged},
	{"mark-unread-all",		action_mark_unread_all},
	{"mark-unread-killed",		action_mark_unread_killed},
	{"uudecode",			action_uudecode},
	{"clear-tagged",		action_clear_tagged},
	{"save",			action_save},
	{"pipe",			action_pipe},
	{"tag-thread",			action_tag_thread},
	{"tag-subject",			action_tag_subject},
	{"untag-thread",		action_untag_thread},
	{"untag-subject",		action_untag_subject},
	{"tag-hot",			action_tag_hot},
	{"catchup",			action_catchup},
	{"unsubscribe",			action_unsubscribe},
	{"subscribe",			action_subscribe},
	{"change-size",			action_change_size},
	{"schedule-thread-ahead",	action_schedule_thread_ahead},
	{"popup-find-group",		action_popup_find_group},
	{"do-the-right-thing",		action_do_the_right_thing},
	{"kill-append",			action_kill_append},
	{"kill-prepend",		action_kill_prepend},
	{"popup-kill",			action_popup_kill},
    };
    static String fallback_resources[] = {
	"*grouplist*PreferredLines:	14",
	"*killist*PreferredLines:	14",
	"*PreferredColumns:		84",
	"*ArtTree.useLineShadows:	True",
	"*textscrbar.allowOff:		True",
	"*Knapp.Justify:		center",
	"*MenuKnapp.Justify:		center",
	"*quotetoggle.set:		True",
	"*stderr*message.center:	False",
	"*textscrbar.stepSize:		13",
	"*unreadtoggle.set:		True",
	"*postpopup*posttext*preferredLines:		8",
	"*postpopup*posttext*preferredColumns:		64",
	"*postpopup*attachlist*preferredLines:		3",
	"*postpopup*attachlist*preferredColumns:	64",
	/* colors */
	"*Foreground:		Black",
	"*Background:		Bisque",
	"*BorderColor:		Black",
	"*rubberColor:		Red",
	"*quoteColor:		Medium Blue",
	"*headerColor:		#000090",
	"*innerColor:		Red",
	"*abort.Foreground:	Red",
	/* fonts */
	"Knews.us-ascii.bodyFont:"
	"    -b&h-lucidatypewriter-medium-r-normal-*-*-120-*-*-*-*-iso8859-1",
	"Knews.us-ascii.quoteFont:"
	"    -b&h-lucidatypewriter-bold-r-normal-*-*-120-*-*-*-*-iso8859-1",
	"Knews.us-ascii.headerFont:"
	"    -b&h-lucidatypewriter-bold-r-normal-*-*-120-*-*-*-*-iso8859-1",
	"Knews.us-ascii.treeFont:"
	"    -b&h-lucidatypewriter-bold-r-normal-*-*-120-*-*-*-*-iso8859-1",
	"Knews.us-ascii.listFont:"
	"    -b&h-lucidatypewriter-medium-r-normal-*-*-120-*-*-*-*-iso8859-1",
	"*ScrList*Font:"
	"    -b&h-lucidatypewriter-medium-r-normal-*-*-120-*-*-*-*-iso8859-1",
	"*Font:"
	"    -b&h-lucidatypewriter-bold-r-normal-*-*-120-*-*-*-*-iso8859-1",
	/* translations */
	"*arttree.baseTranslations:	#augment \\n"
	"	<Btn1Down>:	set-selected()\\n"
	"	<Btn3Down>:	toggle-outer()",
	"*grouplist.baseTranslations:	#augment \\n"
	"	<Btn2Down>:	dnd-start() \\n"
	"	<Btn2Motion>:	dnd-do() \\n"
	"	<Btn2Up>:	dnd-end()",
	"*killist.baseTranslations:	#augment \\n"
	"	<Btn2Down>:	dnd-start() \\n"
	"	<Btn2Motion>:	dnd-do() \\n"
	"	<Btn2Up>:	dnd-end()",
	"*threadlist.baseTranslations:	#augment \\n"
	"	~Ctrl <Btn2Down>:	select() mark-read-thread() \\n"
	"	~Ctrl <Btn2Motion>:	select() mark-read-thread() \\n"
	"	Ctrl <Btn2Down>:	select() mark-unread-thread() \\n"
	"	<Btn3Down>:		select() tag-thread()",
	/* labels */
	"*knapplayout.knapp0.label:	Quit\\nDone",
	"*knapplayout.knapp1.label: "
	"	Connect...\\nDisconnect\\nView thread\\nBack\\nSubscribe",
	"*knapplayout.knapp2.label: "
	"	All groups\\nAll threads\\nUnsubscribe",
	"*knapplayout.misc.label:	Misc",
	"*knapplayout.post.label:	Post",
	"*knapplayout.kill.label:	Kill...",
	"*knapplayout.knapp6.label:	Update\\nCatchup",
	"*knapplayout.knapp7.label:	Read Group\\nNext unread\\nGoto group",
	"*knapplayout.knapp8.label:	Rescan\\nPrevious",
	"*knapplayout.abort.label:	Abort",
	"*knapplayout.save.label:	Save...",
	"*knapplayout.search.label:	Search...",
	"*killeditor*fieldknapp.label:	Message-Id\\nSubject\\nFrom\\nXref",
	"*killeditor*scopeknapp.label: "
	"	Article\\nSubject\\nThread\\nSubthread",
	"*killeditor*actionknapp.label:	Kill\\nHot",
	"*postpopup*post.label:		Post\\nMail\\nPost&Mail",
	"*saveshell*ok.label:		Save\\nPipe",
	/* "buffers" */
	"*fieldmessage.buffer:		Header:",
	"*scopemessage.buffer:		Scope:",
	"*groupmessage.buffer:		Group regexp:",
	"*exprmessage.buffer:		Regexp/Message-Id:",
	"*colormessage.buffer:		Color:",
	"*saveshell*message.buffer:		Save to file or pipe to shell",
	"*saveshell*shellmessage.buffer:	Shell-command:",
	"*saveshell*filemessage.buffer:		File:",
	"*searchshell*regexptitle.buffer:	Regular expression searching",
	"*searchshell*xpattitle.buffer:		XPAT wildcard searching",
	"*searchshell*regexpmessage.buffer:	Regexp:",
	"*searchshell*wildcardmessage.buffer:	Wildcard:",
	"*searchshell*headermessage.buffer:	Header:",
	"*postpopup*posttitle.buffer:		Post/Mail Manager",
	"*postpopup*attachtitle.buffer:		Attachments:",
	"*postpopup*typetitle.buffer:		Content-Type:",
	"*postpopup*descrtitle.buffer:		Content-Description:",
	"*postpopup*disptitle.buffer:		Content-Disposition:",
	"*postpopup*enctitle.buffer:		Content-Transfer-Encoding:",
	"*postpopup*nametitle.buffer:		filename=",
	"*postpopup*inlinetoggle.label:		inline",
	"*postpopup*attachtoggle.label:		attachment",
	"*postpopup*nonetoggle.label:		None",
	"*postpopup*base64toggle.label:		Base 64",
	"*postpopup*uuetoggle.label:		Uuencode",
	"*postpopup*qptoggle.label:		Quoted-printable",
	NULL,
    };
    static XrmOptionDescRec options[] = {
	{"-nntpServer",		".nntpServer",
	 XrmoptionSepArg,	(XtPointer)NULL},
	{"-ncols",		".nCols",
	 XrmoptionSepArg,	(XtPointer)NULL},
	{"-bg",			"*Background",
	 XrmoptionSepArg,	(XtPointer)NULL},
	{"-fg",			"*Foreground",
	 XrmoptionSepArg,	(XtPointer)NULL},
	{"-separate",		".separateWindows",
	 XrmoptionNoArg,	(XtPointer)"False"},
	{"+separate",		".separateWindows",
	 XrmoptionNoArg,	(XtPointer)"True"},
	{"-descriptions",	".retrieveDescriptions",
	 XrmoptionNoArg,	(XtPointer)"False"},
	{"+descriptions",	".retrieveDescriptions",
	 XrmoptionNoArg,	(XtPointer)"True"},
	{"-active",		".readActiveFile",
	 XrmoptionNoArg,	(XtPointer)"False"},
	{"+active",		".readActiveFile",
	 XrmoptionNoArg,	(XtPointer)"True"},
	{"-fill",		".fillNewsrcFile",
	 XrmoptionNoArg,	(XtPointer)"False"},
	{"+fill",		".fillNewsrcFile",
	 XrmoptionNoArg,	(XtPointer)"True"},
	{"-lines",		".showNumberLines",
	 XrmoptionNoArg,	(XtPointer)"False"},
	{"+lines",		".showNumberLines",
	 XrmoptionNoArg,	(XtPointer)"True"},
	{"-bell",		".bell",
	 XrmoptionNoArg,	(XtPointer)"False"},
	{"+bell",		".bell",
	 XrmoptionNoArg,	(XtPointer)"True"},
	{"+keep",		".keepThreadInfo",
	 XrmoptionNoArg,	(XtPointer)"Subscribed"},
	{"-keep",		".keepThreadInfo",
	 XrmoptionNoArg,	(XtPointer)"False"},
	{"-visual",		".visualClass",
	 XrmoptionSepArg,	(XtPointer)NULL},
	{"-depth",		".visualDepth",
	 XrmoptionSepArg,	(XtPointer)NULL},
	{"-install",		".installCmap",
	 XrmoptionNoArg,	(XtPointer)"True"},
	{"+icase",		".icaseRegexps",
	 XrmoptionNoArg,	(XtPointer)"True"},
	{"-icase",		".icaseRegexps",
	 XrmoptionNoArg,	(XtPointer)"False"},
	{"+images",		".inlineImages",
	 XrmoptionNoArg,	(XtPointer)"True"},
	{"-images",		".inlineImages",
	 XrmoptionNoArg,	(XtPointer)"False"},
    };
    Arg		args[8];
    char	*home = getenv("HOME");

    if (!home) {
	fputs("knews: couldn't getenv(\"HOME\")\n", stderr);
	exit(1);
    }
    if (chdir(home) < 0) {
	int	oerrno = errno;

	fprintf(stderr, "knews: couldn't chdir(%s)", home);
	errno = oerrno;
	perror(NULL);
	exit(1);
    }

    freopen("/dev/null", "r", stdin);

    srand(3ul * time(NULL) +  5ul * getuid() +
	  7ul * getpid()   + 11ul * getppid());

    main_thr = create_thread_context();
    main_server = server_create(-1);
    server_set_quit_func(main_server, nntp_quit);

#if (XtSpecificationRelease > 4)
    XtSetLanguageProc(NULL, NULL, NULL);
#endif

    XtSetTypeConverter(XtRString, XtRPixmap, cvt_string_to_pixmap,
		       NULL, 0, XtCacheAll, destroy_pixmap);
    XtSetTypeConverter(XtRString, XtRLong, cvt_string_to_long,
		       NULL, 0, XtCacheAll, NULL);
    XSetErrorHandler(disp_error_handler);
    XSetIOErrorHandler(disp_io_error_handler);

    XtToolkitInitialize();
    app_cont = XtCreateApplicationContext();
    XtAppSetFallbackResources(app_cont, fallback_resources);
    display = XtOpenDisplay(app_cont, NULL, NULL, "Knews",
			    options, XtNumber(options), &argc, argv);

    if (argc != 1) {
	int	n = strlen(argv[1]);

	if (strncmp(argv[1], "-version", n) == 0)
	    fputs("knews: version " KNEWS_VERSION " compiled "
		  "on " __DATE__ " " __TIME__ ".\n", stderr);
	else
	    fputs("knews: Bad command line arguments, "
		  "see the man-page for usage.\n", stderr);
	exit(1);
    }

    if (!display)
	exit(1);

    color_init(display);
    XtSetArg(args[0], XtNinput, True);
    XtSetArg(args[1], XtNcolormap, global.cmap);
    XtSetArg(args[2], XtNvisual, global.visual);
    XtSetArg(args[3], XtNdepth, global.depth);
    XtSetArg(args[4], XtNmappedWhenManaged, False);
    main_widgets.shell =
	XtAppCreateShell(NULL, "Knews",
#if XtSpecificationRelease >= 6
			 sessionShellWidgetClass,
#else
			 applicationShellWidgetClass,
#endif
			 display, args, 5);
    if (!main_widgets.shell)
	exit(1);

    init_child_contexts();
    XtAppAddActions(app_cont, actions, XtNumber(actions));
    XtGetApplicationResources(main_widgets.shell, (XtPointer)&global,
			      resource_spec, XtNumber(resource_spec),
			      NULL, 0);
    init_fonts(main_widgets.shell);
    mailcap_init();

    if (global.newsrc_templ && !strstr(global.newsrc_templ, "%s")) {
	fputs("knews: no %s in newsrcTemplate, ignoring it.\n", stderr);
	global.newsrc_templ = NULL;
    }

    if (global.config_file && !strstr(global.config_file, "%s")) {
	fputs("knews: no %s in configFile, ignoring it.\n", stderr);
	global.config_file = NULL;
    }
    if (!global.config_file)
	global.config_file = "~/.knews/config-%s";
    if (global.use_icon)
	set_icon(main_widgets.shell);

    fix_domain_stuff();
    res_initialize();

    if (global.nntp_server && global.nntp_server[0] != '\0')
	global.nntp_server = XtNewString(global.nntp_server);
    else {
	char	*env_var = getenv("NNTPSERVER");

#ifndef DEFAULT_DEFAULT_NNTPSERVER
#define DEFAULT_DEFAULT_NNTPSERVER 0
#endif
	if (env_var)
	    if (env_var[0] == '\0')
		global.nntp_server = NULL;
	    else
		global.nntp_server = XtNewString(env_var);
	else {
	    global.nntp_server = DEFAULT_DEFAULT_NNTPSERVER;
	    if (global.nntp_server)
		global.nntp_server = XtNewString(global.nntp_server);
	}
    }

    if (global.chunk_size <= 0)
	global.chunk_size = 16;
    if (global.post_misc_menu_size <= 0)
	global.post_misc_menu_size = 1;

    if (!global.version)
	fputs("knews: Application defaults file "
	      "not properly installed.\n", stderr);
    else if (strncmp(global.version, KNEWS_VERSION, 5) != 0)
	fputs("knews: Incompatible version of "
	      "application defaults file.\n", stderr);

    create_main_widgets();
    global.gc = DefaultGCOfScreen(XtScreen(main_widgets.shell));
    alloc_colors();
    if (fcntl(ConnectionNumber(display), F_SETFD, FD_CLOEXEC) < 0)
	perror("fcntl");

    setNewsModeDisconnected();
    set_standard_message();
    set_busy(False);
    XtAppAddTimeOut(app_cont, 500, init_timer, NULL);

    XtMapWidget(main_widgets.shell);
    XtAppMainLoop(app_cont);

    return 0;
}
