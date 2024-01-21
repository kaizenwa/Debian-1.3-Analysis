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
#undef  _POSIX_SOURCE
#define _POSIX_SOURCE   1
#undef  _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 2

#define KNEWS_VERSION		"0.9.8"

#include "../configure.h"

#ifndef BIN_SH
#  define BIN_SH "/bin/sh"
#endif

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <time.h>
#include <sys/types.h>
#include <regex.h>
#include "../Widgets/ArtTreeNode.h"

extern XtAppContext	app_cont;
extern Display		*display;

typedef enum {
    NewsModeDisconnected,
    NewsModeConnected,
    NewsModeGroup,
    NewsModeThread,
    NewsModeAllgroups,
    NewsModeSomegroups,
    NewsModeNewgroups
} NewsMode;

typedef struct subj_node {
    struct subj_node	*hash_next;
    struct subj_node	*next;
    struct subj_node	*prev;
    struct art_node	*thread;
    char		*subject;
    long		disp;
    long		no_unread;
    Pixmap		pixmap;
    unsigned short	hash_len;
    unsigned char	has_tagged;
} SUBJECT;

typedef struct art_node {
    ART_TREE_NODE	tree_data;
    struct art_node     *hash_next;
    struct art_node     *next;
    char                *msgid;
    SUBJECT             *subject;
    char                *from;
    char                *xref;
    time_t		date;
    long                no;
    Pixmap		pixmap;
    unsigned short	hash_len;
    unsigned short	lines;
#if 0
    long		bytes;
#endif
    unsigned char	read;
    unsigned char	killed;
} ARTICLE;

#define PARENT(art)	((art)->tree_data.parent)
#define CHILD1(art)	((art)->tree_data.child1)
#define SIBLING(art)	((art)->tree_data.sibling)

#define A_PARENT(art)	((ARTICLE *)PARENT(art))
#define A_CHILD1(art)	((ARTICLE *)CHILD1(art))
#define A_SIBLING(art)	((ARTICLE *)SIBLING(art))

#define REGEXP_COMPILE_FLAGS \
((global.icase_regexps ? REG_ICASE : 0) | REG_NOSUB | REG_EXTENDED)

#define HOT_PIXMAP_SIZE	8

typedef struct art_list_node {
    long                  first;
    long                  last;
    struct art_list_node  *next;
} ART_LIST_NODE;

typedef struct group {
    char		*name;
    char		*description;
    long		no_unread;
    long		first_art;
    long		last_art;
    ART_LIST_NODE	*read_arts;
    long		disp;
    char		subscribed;
    char		moderated;
    char		found_in_newsrc;
    char		ahead_flag;
} GROUP;

extern struct Global {
    String		nntp_server;
    String		config_nntp_server;
    String		config_posting_agent;
    String		edit_command;
    String		url_command;
    String		print_command;
    String		needs_terminal;
    String		copious_output;
    Cursor		cursor;
    Cursor		busy_cursor;
    String		version;
    String		mail_name;
    String		config_file;
    String		newsrc_templ;
    String		old_newsrc_templ;
    String		kill_file_templ;
    String		group_kill_file_templ;
    String		auto_subscribe;
    String		mime_types;
    String		retrieve_descr;
    String		read_active_file;
    String		fill_newsrc_file;
    String		show_number_lines;
    String		keep_thread_info;
    String		check_for_new_groups;
    String		confirm_quit_group;
    Pixel		default_hot_pixel;
    Pixel		pixel;
    Pixel		quote_pixel;
    Pixel		header_pixel;
    Pixel		alert_pixel;
    Pixel		clickable_pixel;
    long		stderr_timeout;
    int			chunk_size;
    int			post_misc_menu_size;
    int			extra_menu_size;
    int			type_menu_size;
    int			forward_menu_size;
    int			n_cols;
    Boolean		separate_windows;
    Boolean		bell;
    Boolean		head_debug;
    Boolean		use_icon;
    Boolean		confirm_quit;
    Boolean		confirm_catchup;
    Boolean		icase_regexps;
    Boolean		show_cache;
    Boolean		bogus_file_system;
    Boolean		generate_path;
    Boolean		mime_forward;
    Boolean		quote_empty;
    Boolean		sort_groups;
    Boolean		inline_images;
    Boolean		color_hack;
    /* private */
    char		*user_id;
    char		*domain_name;
    void		*serv_addr;
    GROUP		**groups;
    long		no_groups;
    long		max_groups;
    GROUP		**new_groups;
    long		no_new_groups;
    SUBJECT		*curr_subj;
    ARTICLE		*curr_art;
    GROUP		*curr_group;
    long		n_hot;
    long		n_killed;
    time_t		last_time;
    GC			gc;
    Visual		*visual;
    Colormap		cmap;
    Cardinal		depth;
    NewsMode		mode;
    unsigned int	busy;
    Boolean		posting_allowed;
    Boolean		xover_supported;
    Boolean             list_active_supported;
} global;

extern struct SERVER		*main_server;
extern struct THREAD_CONTEXT	*main_thr;
