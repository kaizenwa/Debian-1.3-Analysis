/* {{{ Copyright notice */

/* View file module for the Midnight Commander
   Copyright (C) 1994, 1995, 1996 The Free Software Foundation
   Written by: 1994, 1995 Miguel de Icaza
               1994, 1995 Janne Kukonlehto
               1995 Jakub Jelinek
               1996 Joseph M. Hinkle

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* }}} */
/* {{{ Declarations */
#include <config.h>
#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif
#include <string.h>
#include "tty.h"
#include <sys/stat.h>
#ifdef HAVE_MMAP
#   include <sys/mman.h>
#endif
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>	/* For toupper() */
#include <stdlib.h>	/* atoi() */
#include <malloc.h>
#include <errno.h>
#include <limits.h>
#include <sys/param.h>

#include "mem.h"
#include "mad.h"
#include "util.h"
#include "dlg.h"		/* Needed by widget.h */
#include "widget.h"		/* Needed for buttonbar_new */
#include "color.h"
#include "dialog.h"
#include "file.h"
#include "mouse.h"
#include "global.h"
#include "help.h"
#include "key.h"		/* For mi_getch() */
#include "layout.h"
#if defined(HAVE_RX_H) && defined(HAVE_REGCOMP)
#include <rx.h>
#else
#include "regex.h"
#endif
#include "fs.h"
#include "../vfs/vfs.h"
#include "dir.h"
#include "panel.h" /* Needed for current_panel and other_panel */
#include "win.h"
#include "main.h"		/* For the externs */
#define WANT_WIDGETS
#include "view.h"

#ifndef MAP_FILE
#define MAP_FILE 0
#endif

/* Block size for reading files in parts */
#define READ_BLOCK 8192
#define VIEW_PAGE_SIZE 8192

#ifdef IS_AIX
#   define IFAIX(x) case (x):
#else
#   define IFAIX(x)
#endif

/* Maxlimit for skipping updates */
int max_dirt_limit = 
#ifdef _OS_NT
0;
#else
10;
#endif

/* Our callback */
static int view_callback (Dlg_head *h, WView *view, int msg, int par);
static void move_forward (WView *view, int i);

/* Wrap mode */
int wrap_mode = 1;

/* Scrolling is done in pages or line increments */
int mouse_move_pages_viewer = 1;

/* Used to compute the bottom first variable */
int have_fast_cpu = 0;

int default_hex_mode = 0;
#ifdef HEX_EDIT
int default_hexedit_mode = 0;
#endif /* HEX_EDIT */
int default_magic_flag = 1;
int default_nroff_flag = 0;
int altered_hex_mode = 0;
int altered_magic_flag = 0;
int altered_nroff_flag = 0;
/* }}} */

/* "$Id: view.c,v 1.19 1995/02/21 19:07:38 miguel Exp $" */

static char hex_char[] = "0123456789ABCDEF";

/* }}} */
/* {{{ Clean-up functions */

void
close_view_file (WView *view)
{
    if (view->file != -1){
	mc_close (view->file);
	view->file = -1;
    }
}

void
free_file (WView *view)
{
    int i;
    
#ifdef HAVE_MMAP

    if (view->mmapping){
	mc_munmap (view->data, view->s.st_size);
	close_view_file (view);
    } else 
#endif /* HAVE_MMAP */
    {
	if (view->reading_pipe){
	    /* Check error messages */
	    if (!view->have_frame)
		check_error_pipe ();
	    
	    /* Close pipe */
	    pclose (view->stdfile);
	    view->stdfile = NULL;
	    
	    /* Ignore errors because we don't want to hear about broken pipe */
	    close_error_pipe (-1, NULL);
	} else
	    close_view_file (view);
    }
    /* Block_ptr may be zero if the file was a file with 0 bytes */
    if (view->growing_buffer && view->block_ptr){
	for (i = 0; i < view->blocks; i++){
	    free (view->block_ptr [i].data);
	}
	free (view->block_ptr);
    }
}

/* Both views */
void
view_done (WView *view)
{
    if (view->view_active){
	free_file (view);
	free (view->filename);
	if (view->command)
	    free (view->command);
    }
    view->view_active = 0;
    default_hex_mode = view->hex_mode;
    default_nroff_flag = view->viewer_nroff_flag;
    default_magic_flag = view->viewer_magic_flag;
}

static void view_hook (void *);

void
view_destroy (WView *view)
{
    view_done (view);
    if (view->have_frame)
	delete_hook (&select_file_hook, view_hook);
#ifdef HAVE_TK
    free (view->cache);
    free (view->color_cache);
#endif
}

static int
get_byte (WView *view, int byte_index)
{
    int page   = byte_index / VIEW_PAGE_SIZE + 1;
    int offset = byte_index % VIEW_PAGE_SIZE;
    int i, n;
    block_ptr_t *tmp;
    
    if (view->growing_buffer){
	if (page > view->blocks){
	    tmp = xmalloc (sizeof (block_ptr_t) * page, "get_byte");
	    if (view->block_ptr){
		bcopy (view->block_ptr, tmp, sizeof (block_ptr_t) *
		       view->blocks);
		free (view->block_ptr);
	    } 
	    view->block_ptr = tmp;
	    for (i = view->blocks; i < page; i++){
		char *p = malloc (VIEW_PAGE_SIZE);
		view->block_ptr [i].data = p;
		if (!p)
		    return '\n';
		if (view->stdfile != NULL)
		    n = fread (p, 1, VIEW_PAGE_SIZE, view->stdfile);
		else
		    n = mc_read (view->file, p, VIEW_PAGE_SIZE);
		if (n != -1)
		    view->bytes_read += n;
		if (view->s.st_size < view->bytes_read){
		    view->bottom_first = -1; /* Invalidate cache */
		    view->s.st_size = view->bytes_read;
		    view->last_byte = view->bytes_read;
		    if (view->reading_pipe)
			view->last_byte = view->first + view->bytes_read;
		}
		/* To force loading the next page */
		if (n == VIEW_PAGE_SIZE && view->reading_pipe){
		    view->last_byte++;
		}
	    }
	    view->blocks = page;
	}
	if (byte_index > view->bytes_read){
	    return -1;
	} else
	    return view->block_ptr [page-1].data [offset];
    } else {
    	if (byte_index >= view->last_byte)
    	    return -1;
    	else
	    return view->data [byte_index];
    }
}

#ifdef HEX_EDIT
static void
enqueue_change (struct hexedit_change_node **head, struct hexedit_change_node *node)
{
    struct hexedit_change_node *curr = *head;

    while (curr) {
        if (node->offset < curr->offset) {
            *head = node;
            node->next = curr;
            return;
        }
        head = (struct hexedit_change_node **) curr;
        curr = curr->next;
    }
    *head = node;
    node->next = curr;
}

static void move_right (WView *);

static void
put_editkey (WView *view, unsigned char key)
{
    struct hexedit_change_node *node;
    unsigned char byte_val;

    if (!view->hexedit_mode || view->growing_buffer != 0)
        return;
 
    /* Has there been a change at this position ? */
    node = view->change_list;
    while (node) { 
        if (node->offset != view->edit_cursor)
            node = node->next;
        else
            break;
    }

    if (view->view_side == view_side_left) {
        /* Hex editing */

        if (key >= '0' && key <= '9')
            key -= '0';
        else if (key >= 'A' && key <= 'F')
            key -= '7';
        else if (key >= 'a' && key <= 'f')
            key -= 'W';
        else
            return;

        if (node)
            byte_val = node->value;
        else
            byte_val = get_byte(view, view->edit_cursor);

        if (view->nib_shift == 0) {
            byte_val = (byte_val & 0x0f) | (key << 4);
        } else {
            byte_val = (byte_val & 0xf0) | (key);
        }
    } else {
        /* Text editing */
        byte_val = key;
    }
    if (!node) {
        node = (struct hexedit_change_node *)
                    xmalloc(sizeof(struct hexedit_change_node), "HexEdit");

        if (node) {
            node->offset = view->edit_cursor;
            node->value = byte_val;
            enqueue_change (&view->change_list, node);
        }
    } else {
        node->value = byte_val;
    }
    view->dirty++;
    view_update (view);
    move_right (view);
}

static void
free_change_list (WView *view)
{
    struct hexedit_change_node *n = view->change_list;

    while (n) {
        view->change_list = n->next;
        free (n);
        n = view->change_list;
    }
    view->file_dirty = 0;
    view->dirty++;
}

static void
save_edit_changes (WView *view, char *prompt)
{
    struct hexedit_change_node *node = view->change_list;
    int fp;

    if (!node)
	return;

    if (prompt) {
        query_set_sel (1);
	if (query_dialog (prompt, view->filename,
                        2 | WITH_HOTKEYS, 2, "y Yes", "n No") != 0 ) {
            free_change_list(view);
	    return;
        }
    }
    fp = open (view->filename, O_WRONLY);
    if (fp >= 0) {
        while (node) {
            lseek (fp, node->offset, SEEK_SET);
            write (fp, &node->value, 1);
            node = node->next;
        }
        close (fp);
    }
    free_change_list (view);
}

#endif /* HEX_EDIT */

static char *
set_view_init_error (WView *view, char *msg)
{
    view->growing_buffer = 0;
    view->reading_pipe   = 0;
    view->first = 0;
    if (msg){
	view->bytes_read = strlen (msg);
	return strdup (msg);
    }
    return 0;
}

/* return values: 0 for success, else points to error message */
static char *
init_growing_view (WView *view, char *name, char *filename) 
{
    view->growing_buffer = 1;

    if (name){
	view->reading_pipe = 1;
	view->s.st_size = 0;

	open_error_pipe ();
	if ((view->stdfile = popen (name, "r")) == NULL){
	    close_error_pipe (view->have_frame?-1:1, view->data);
	    return set_view_init_error (view, " Can't spawn child program ");
	}

#ifndef HAVE_XVIEW
	/* First, check if filter produced any output */
	get_byte (view, 0);
	if (view->bytes_read <= 0){
	    pclose (view->stdfile);
	    view->stdfile = NULL;
	    close_error_pipe (view->have_frame?-1:1, view->data);
	    return set_view_init_error (view, " Empty output from child filter ");
	}
#endif
    } else {
        view->stdfile = NULL;
	if ((view->file = mc_open (filename, O_RDONLY)) == -1)
	    return set_view_init_error (view, " Could not open file ");
    }
    return 0;
}

/* Load filename into core */
/* returns:
   -1 on failure.
   if (have_frame), we return success, but data points to a
   error message instead of the file buffer (quick_view feature).
*/
static char *load_view_file (WView *view, char *filename)
{
    char *cmd;

#ifndef HAVE_MMAP
    int count;
#endif

    if ((view->file = mc_open (filename, O_RDONLY)) < 0){
	set_view_init_error (view, 0);
	return (copy_strings (" Can't open file \"",
			      filename, "\"\n ",
			      unix_error_string (errno), " ", 0));

    }
    if (mc_fstat (view->file, &view->s) < 0){
	set_view_init_error (view, 0);
	close_view_file (view);
	return copy_strings (" Can't stat file \n ",
			     unix_error_string (errno), " ", 0);
    }
    if (S_ISDIR (view->s.st_mode) || S_ISSOCK (view->s.st_mode)
	|| S_ISFIFO (view->s.st_mode)){
	close_view_file (view);
	return set_view_init_error (view, " Can't view: not a regular file ");
    }

#ifdef JUST_FOR_CULTURAL_INFORMATION
    /* #ifed out because now we have growing buffers :-) */
    if (view->s.st_size < 1) {
	close_view_file (view);
	return set_view_init_error (view, " Can't view empty file ");
    }
#endif
    if (view->s.st_size == 0){
	/* Must be one of those nice files that grow (/proc) */
	close_view_file (view);
	return init_growing_view (view, 0, filename);
    }
    
    /* First, try to open a compressed file */
    if (!view->viewer_magic_flag && (is_gunzipable (view->file)) != 0){
	close_view_file (view);
	cmd = copy_strings ("gzip -dc ", "'", filename, "'", 0);
	return init_growing_view (view, cmd, filename);
    }

    /* Otherwise, the file wasn't compressed */
#ifdef HAVE_MMAP
    view->data = mc_mmap (0, view->s.st_size, PROT_READ, MAP_FILE | MAP_SHARED,
		          view->file, 0);
    if ((caddr_t) view->data == (caddr_t) -1){
	close_view_file (view);
/*	set_view_init_error (view, 0);
	return copy_strings (" Can't mmap file \n ",
			     unix_error_string (errno), " ", 0);*/
	return init_growing_view (view, 0, filename);
			     
    }
    view->first = 0;
    view->bytes_read = view->s.st_size;
    view->mmapping = 1;
    return 0;
#else
    close_view_file (view);
    return init_growing_view (view, 0, filename);
#endif
}

/* Return zero on success, -1 on failure */
int
do_view_init (WView *view, char *_command, char *_file, int start_line)
{
    char *error = 0;
    int i;

    if (view->view_active)
	view_done (view);

    /* Set up the state */
    view->block_ptr = 0;
    view->data = NULL;
    view->growing_buffer = 0;
    view->reading_pipe = 0;
    view->mmapping = 0;
    view->blocks = 0;
    view->block_ptr = 0;
    view->first = view->bytes_read = 0;

    /* Clear the markers */
    view->marker = 0;
    for (i = 0; i < 10; i++)
	view->marks [i] = 0;
    

    if (!view->have_frame){
	/* hex_mode = 0; */
	/* wrap_mode = 1; */
	view->start_col = 0;
    }
    if (_command && view->viewer_magic_flag)
	error = init_growing_view (view, _command, _file);
    else
	error = load_view_file (view, _file);

    if (error){
	if (!view->have_frame){
	    message (1, " Error ", error);
	    free (error);
	    return -1;
	}
    }

    view->view_active = 1;
    view->filename = strdup (_file);
    if (_command)
    	view->command = strdup (_command);
    else
    	view->command = 0;
    view->search_start = view->start_display = view->first;
    view->found_len = 0;
    view->start_col = 0;
    view->last_search = 0;            /* Start a new search */

    /* Special case: The data points to the error message */
    if (error){
	view->data = error;
	view->s.st_size = view->bytes_read = strlen (view->data);
    }
    view->last_byte = view->first + view->s.st_size;
    
    if (start_line > 1 && !error){
	get_byte (view, 0);
	move_forward (view, start_line - 1);
    }
#ifdef HEX_EDIT
    view->edit_cursor = view->first;
    view->file_dirty =  0;
    view->nib_shift = 0;
    view->view_side = view_side_left;
    view->change_list = NULL;
#endif /* HEX_EDIT */

    return 0;
}


/* Both views */
/* Return zero on success, -1 on failure */
int
view_init (WView *view, char *_command, char *_file, int start_line)
{
    int cols;

    if (view->have_frame)
	cols = view->widget.cols - 2;
    else
	cols = view->widget.cols;
    
    view->bottom_first = -1;
    view->bytes_per_line = 2 * (cols - 7) / 9;
    view->bytes_per_line &= 0xfffc;
    view->dirty = max_dirt_limit + 1;	/* To force refresh */
    if (!view->view_active || strcmp (_file, view->filename))
	return do_view_init (view, _command, _file, start_line);
    else
	return 0;
}

/* }}} */

/* {{{ X Window code */
#ifdef HAVE_TK
#include "tkmain.h"

/* Accepts the dim command with the width and the height in characters */
static int
tk_viewer_callback (ClientData cd, Tcl_Interp *interp, int ac, char *av[])
{
    WView *view = (WView *) cd;

    if (av [1][0] != 'd')
	return TCL_OK;
    view->widget.cols  = atoi (av [2]);
    view->widget.lines = atoi (av [3]);
    return TCL_OK;
}

void
x_create_viewer (WView *view)
{
    char *cmd;
    char *parent;
    
    /* First, check if our parent is ".", if this is the case, then
     * create a stand alone viewer, otherwise, we create a paneled
     * version of the viewer
     */
    
    if (view->have_frame){
	parent = view->widget.wcontainer;
    } else {
	parent = view->widget.parent->wdata;
    }
    cmd = tk_new_command (parent, view, tk_viewer_callback, 'v');
    
    tk_evalf ("newview %d %s %s %s", view->have_frame,
	      view->have_frame ? (char *) view->widget.wcontainer : "{}",
	      cmd+1, cmd);
}
#else
#   define x_create_viewer(x)
#endif
/* }}} */

/* {{{ Screen update functions */

#ifdef HAVE_TK
static void
view_status (WView *view)
{
    char *window = wtk_win (view->widget);
    
    if (!view->status_shown){
	view->status_shown = 0;

	tk_evalf ("view_update_info %s {%s} {%d} {%s} {%s}",
		  window, name_trunc (view->filename ? view->filename:
				      view->command ? view->command:"", 20),
		  -view->start_col, size_trunc (view->s.st_size),
		  view->growing_buffer ? "[grow]":"[]");
    }
}

static void
view_percent (WView *view, int p, int w)
{
    fprintf (stderr, "Missing tk view_percent\n");
}
#else
static void
view_percent (WView *view, int p, int w)
{
    int percent;

    percent = (view->s.st_size == 0 || view->last_byte == view->last) ? 100 :
        (p > (INT_MAX/100) ?
         p / (view->s.st_size / 100) :
	 p * 100 / view->s.st_size);

#if 0
    percent = view->s.st_size == 0 ? 100 :
	(view->last_byte == view->last ? 100 :
	 (p)*100 / view->s.st_size);
#endif
    
    widget_move (view, view->have_frame, w - 5);
    printw ("%3d%% ", percent);
}

static void
view_status (WView *view)
{
    int w = view->widget.cols - (view->have_frame * 2);
    int i;

    attrset (SELECTED_COLOR);
    widget_move (view, view->have_frame, view->have_frame);
    hline (' ', w);
    if (w > 6){
    	i = w > 24 ? 18 : w - 6;
    	printw ("File: %s", name_trunc (view->filename ? view->filename:
					view->command ? view->command:"", i));
    	if (w > 30){
    	    widget_move (view, view->have_frame, 24);
#ifdef HEX_EDIT
            if (view->hex_mode)
                printw ("Offset 0x%08x", view->edit_cursor);
            else
#endif /* HEX_EDIT */
    	    printw ("Col %d", -view->start_col);
    	}
    	if (w > 60){
	    widget_move (view, view->have_frame, 42);
	    printw ("%s bytes", size_trunc (view->s.st_size));
        }
	if (w > 70){
	    printw (" ");
	    if (view->growing_buffer)
		addstr ("  [grow]");
	}
        if (w - i > 4)
#ifdef HEX_EDIT
            if (view->hex_mode)
                view_percent (view, view->edit_cursor-view->first, w);
            else
#endif /* HEX_EDIT */
	    view_percent (view, view->start_display-view->first, w);
    }
    attrset (SELECTED_COLOR);
}
#endif

#ifdef HAVE_TK

#define DEF_COLOR       0
#define BOLD_COLOR      1
#define UNDERLINE_COLOR 2
#define MARK_COLOR      3

/* The major part of the Tk code deals with caching a line (the
 * current one) of text to avoid expensive calls to the Tk text widget
 * callback.
 *
 * We cache all this information on view->cache and the colors on
 * view->color_cache.
 *
 * FIXME: the cache does not know about the contents of the physical
 * text widget (depends on your concept of physical), so if we happen
 * to hit the case where row is decremented in the void display () routine,
 * we will end up with a clean line.
 */

static int current_color;

static void
set_color (int font)
{
    current_color = font;
}

static inline void
display_clean(WView *view, int h, int w)
{
    char *win = wtk_win (view->widget);

    tk_evalf ("cleanview %s.v.view", win);
}

static void
add_character (WView *view, int c)
{
    view->cache [view->dest] = c;
    view->color_cache [view->dest] = current_color;
}

static inline void
add_string (WView *view, char *s)
{
    while (*s)
	add_character (view, *s++);
}

static char *
get_tk_tag_name (int color)
{
    /* Those names are the names of the tags in the Tk source */
    static char *color_tag_names [] = {
	"normal", "bold", "underline", "mark"
    };

    return color_tag_names [color];
}

/*
 * Tk: Flushes the contents of view->cache to the Tk text widget
 *
 * We get the command information and call the command directly
 * to avoid escaping the view->cache contents.
 */
static void
flush_line (WView *view)
{
    char *win = wtk_win (view->widget);
    int  row = view->current_line;
    char *text_name;
    Tcl_CmdInfo info;
    int  i, prev_color;
    char str_row [30];
    char *av [5];
    int  len = strlen (view->cache);

    /* Fetch the address and clientData for the view */
    text_name = copy_strings (win, ".v.view", 0);
    Tcl_GetCommandInfo (interp, text_name, &info);

    /* Setup arguments to the command:
     * $win.v.view insert @$row,0 $view->cache
     */
    sprintf (str_row, "%d.0", row);
    i = 0;
    av [i++] = text_name;
    av [i++] = "insert";
    av [i++] = str_row;
    av [i++] = view->cache;

    /* Call the callback :-) */
    (*info.proc) (info.clientData, interp, i, av);
    bzero (view->cache, view->cache_len);

    /* Colorize the line */
    for (prev_color = 0, i = 0; i < len; i++){
	int new_color_start;
	char *color_name;
	    
	if (view->color_cache [i] == prev_color)
	    continue;

	new_color_start = i;
	
	prev_color = view->color_cache [i];
	
	for (;i < len && view->color_cache [i] == prev_color; i++)
	    ;
	
	color_name = get_tk_tag_name (prev_color);
	tk_evalf ("%s tag add %s %d.%d %d.%d",
		  text_name, color_name,
		  row, new_color_start-1,
		  row, i);
    }
    
    bzero (view->color_cache, view->cache_len);
    view->last_col = 0;
    free (text_name);
}

/* Tk: Mantains the line cache */
void
view_gotoyx (WView *view, int row, int col)
{
    if (row != view->current_line){
	fprintf (stderr, "%d\n", row);
	flush_line (view);
    }
    view->current_line = row;

    /* In case the user has resized the viewer */
    if (col > view->cache_len){
	char *new;

	new = xmalloc (col + 1, "cache");
	strcpy (new, view->cache);
	free (view->cache);
	view->cache = new;

	new = xmalloc (col + 1, "cache");
	strcpy (new, view->color_cache);
	free (view->color_cache);
	view->color_cache = new;
	
	view->cache_len = col;
    }

    view->dest = col;
    for (; view->last_col+1 < col; view->last_col++){
	view->cache [view->last_col] = ' ';
	view->color_cache [view->last_col] = current_color;
    }
    view->last_col = col;
}

#else

#define BOLD_COLOR        MARKED_COLOR
#define UNDERLINE_COLOR   VIEW_UNDERLINED_COLOR
#define MARK_COLOR        SELECTED_COLOR
#define DEF_COLOR         NORMAL_COLOR

#define set_color(font) attrset (font)


static inline void
display_clean (WView *view, int height, int width)
{
    /* FIXME: Should I use widget_erase only and repaint the box? */
    if (view->have_frame){
	int i;
	
	draw_double_box (view->widget.parent, view->widget.y, view->widget.x,
		         view->widget.lines, view->widget.cols);
	for (i = 1; i < height; i++){
	    widget_move (view, i, 1);
	    printw ("%*s", width-1, "");
	}
    } else
	widget_erase ((Widget *) view);
}

#define add_character(view,c) addch (c)
#define add_string(view,s) addstr (s)
#define view_gotoyx(v,r,c) widget_move (v,r,c)

#endif

/* Shows the file pointed to by *start_display on view_win */
static long
display (WView *view)
{
#ifdef HAVE_X
#   define  frame_shift 0
#else
    const int frame_shift = view->have_frame;
#endif
    int col = 0 + frame_shift;
    int row = 1 + frame_shift;
    int height, width;
    long from;
    int c;
    int boldflag = 0;
#ifdef HEX_EDIT
     struct hexedit_change_node *curr = view->change_list;
#endif /* HEX_EDIT */
 
    height = view->widget.lines - frame_shift;
    width = view->widget.cols - frame_shift;
    from = view->start_display;
    set_color (DEF_COLOR);

    display_clean (view, height, width);

#ifdef HEX_EDIT
    /* Find the first displayable changed byte */
    while (curr) {
        if (curr->offset < from)
            curr = curr->next;
        else
            break;
    }
#endif
    if (view->hex_mode){
        char hex_buff[10];   /* A temporary buffer for sprintf and mvwaddstr */
        int bytes;	     /* Number of bytes already printed on the line */
	
	/* Start of text column */
        int text_start = width - view->bytes_per_line - 1 + frame_shift;
	
        for (;row < height && from < view->last_byte; row++){
            /* Print the hex offset */
            sprintf (hex_buff, "%05X", (int) (from - view->first));
	    widget_move (view, row, frame_shift);
            add_string (view, hex_buff);
	    
            /* Hex dump starts from column seven */
            col = 7;
	    
            /* Each hex number is two digits */
            hex_buff[2] = 0;
            for (bytes = 0; bytes < view->bytes_per_line
		 && from < view->last_byte; bytes++, from++){
#ifdef HEX_EDIT
                /* Display and mark changed bytes */
                if (curr && from == curr->offset) {
                    c = curr->value;
                    curr = curr->next;
                    boldflag = 3;
                    set_color (7);
                } else
#endif /* HEX_EDIT */
                c = (unsigned char) get_byte (view, from);
		
	    	if (view->found_len && from >= view->search_start
		    && from < view->search_start + view->found_len){
	    	    boldflag = 1;
		    set_color (BOLD_COLOR);
	    	}
#ifdef HEX_EDIT
                /* Display the navigation cursor */
                if (from == view->edit_cursor) {
		    if (view->view_side == view_side_left){
			view->cursor_row = row;
			view->cursor_col = col;
		    }
                    boldflag = 2;
                    set_color (view->view_side == view_side_left ? 15 : 31);
                }
#endif /* HEX_EDIT */

                /* Print a hex number (sprintf is too slow) */
                hex_buff [0] = hex_char [(c >> 4)];
                hex_buff [1] = hex_char [c & 15];
		view_gotoyx (view, row, col);
                add_string (view, hex_buff);
                col += 3;
#ifdef HEX_EDIT
                /* Turn off the cursor or changed byte highlighting here */
                if (boldflag > 1)
                    set_color (DEF_COLOR);
#endif /* HEX_EDIT */
                if ((bytes & 3) == 3 && bytes + 1 < view->bytes_per_line){
#ifdef HEX_EDIT
                    /* Turn off the search highlighting */
                    if (boldflag == 1 &&
                            from == view->search_start + view->found_len - 1)
                        set_color (DEF_COLOR);
#else /* HEX_EDIT */
                
                    if (boldflag && from == view->search_start + view->found_len - 1)
                    	set_color (DEF_COLOR);
#endif /* HEX_EDIT */
		    
                    /* Hex numbers are printed in the groups of four */
                    /* Groups are separated by a vline */
		    
                    add_character (view, ' ');
                    one_vline ();
		    view_gotoyx (view, row, col + 1);
                    col += 2;
                    
                    if (boldflag && from==view->search_start+view->found_len-1)
                    	set_color (BOLD_COLOR);
		    
                }
                if (boldflag && from < view->search_start + view->found_len - 1 
                    && bytes != view->bytes_per_line - 1)
                    add_character (view, ' ');
		
                /* Print the corresponding ascii character */
		view_gotoyx (view, row, text_start + bytes);
		
                if (!is_printable (c))
                    c = '.';
#ifdef HEX_EDIT
                switch (boldflag) {
                    default:
                        break;
                    case 1:
                        set_color (BOLD_COLOR);
                        goto setcursor;
                    case 2:
                        set_color (view->view_side == view_side_left ? 31 : 15);
                        goto setcursor;
                    case 3:
                        set_color (7);

                    setcursor:
		    if (view->view_side == view_side_right){
			view->cursor_col = text_start + bytes;
			view->cursor_row = row;
		    }
		}
#endif /* HEX_EDIT */
		add_character (view, c);
		
                if (boldflag){
                    boldflag = 0;
                    set_color (DEF_COLOR);
                }
            }
        }
    } else {
        if (view->growing_buffer && from == view->last_byte)
            get_byte (view, from);
    	for (; row < height && from < view->last_byte; from++){
	    c = get_byte (view, from);
    	    if ((c == '\n') || (col == width && wrap_mode)){
       	        col = frame_shift;
       	        row++;
		if (c == '\n' || row >= height)
		    continue;
       	    }
	    if (c == '\r')
		continue;
       	    if (c == '\t'){
       	        col = ((col - frame_shift)/8)*8 + 8 + frame_shift;
       	        continue;
       	    }
	    if (view->viewer_nroff_flag && c == '\b'){
	    	if (from + 1 < view->last_byte
		    && is_printable (get_byte (view, from + 1)) &&
	    	    from > view->first
		    && is_printable (get_byte (view, from - 1)))
		{
		    if (col <= frame_shift){
		    	/* So it has to be wrap_mode - do not need to check for it */
		    	if (row == 1 + frame_shift){
		    	    from++;
		    	    continue; /* There had to be a bold character on the rightmost position
		    	    		 of the previous undisplayed line */
		    	}
		    	row--;
		    	col = width;
		    }
		    col--;
		    boldflag = 1;
		    if (get_byte (view, from - 1) == '_' && get_byte (view, from + 1) != '_')
		    	set_color (UNDERLINE_COLOR);
		    else
		    	set_color (BOLD_COLOR);
		    continue;
		}
	    }
	    if (view->found_len && from >= view->search_start
		&& from < view->search_start + view->found_len){
	    	boldflag = 1;
		set_color (MARK_COLOR);
	    }
       	    if (col >= frame_shift-view->start_col
		&& col < width-view->start_col)
	    {
		view_gotoyx (view, row, col+view->start_col);
       		if (!is_printable (c))
		    c = '.';

		add_character (view, c);
       	    } 
	    col++;
	    if (boldflag){
		boldflag = 0;
		set_color (DEF_COLOR);
	    }
        }
#ifdef HAVE_TK
	/* Tk: This flushes the last line */
	view_gotoyx (view, view->current_line+1, 0);
#endif
	
        if (view->growing_buffer && from == view->last_byte)
            get_byte (view, from);
    }
    view->last = from;
    return from;
}

void
view_place_cursor (WView *view)
{
    int shift;

    if (view->view_side == view_side_left)
	shift = view->nib_shift;
    else
	shift = 0;
    
    widget_move (&view->widget, view->cursor_row, view->cursor_col + shift);
}

void
view_update (WView *view)
{
    static int dirt_limit = 1;

    if (view->dirty > dirt_limit){
	/* Too many updates skipped -> force a update */
	display (view);
	view_status (view);
	view->dirty = 0;
	/* Raise the update skipping limit */
	dirt_limit++;
	if (dirt_limit > max_dirt_limit)
	    dirt_limit = max_dirt_limit;
    }
    if (view->dirty){
	if (is_idle ()){
	    /* We have time to update the screen properly */
	    display (view);
	    view_status (view);
	    view->dirty = 0;
	    if (dirt_limit > 1)
		dirt_limit--;
	} else {
	    /* We are busy -> skipping full update,
	       only the status line is updated */
	    view_status (view);
	}
	/* Here we had a refresh, if fast scrolling does not work
	   restore the refresh, although this should not happen */
    }
}

static inline void
my_define (Dlg_head *h, int idx, char *text,
			 void (*fn)(WView *), WView *view)
{
    define_label_data (h, (Widget *) view, idx, text, (buttonbarfn) fn, view);
}

/* }}} */
/* {{{ Movement functions */
/* If the last parameter is nonzero, it means we want get the count of lines
   from current up to the the upto position inclusive */
static long
move_forward2 (WView *view, long current, int lines, long upto)
{
    long p, q;
    int  line;
    int  col = 0;

#ifdef HEX_EDIT
    if (!upto && !view->hex_mode && view->last == view->last_byte)
        return current;
#else /* HEX_EDIT */
 
    if (!upto && view->last == view->last_byte)
    	return current;
#endif /* HEX_EDIT */

    if (view->hex_mode){
#ifdef HEX_EDIT
        p = current + lines * view->bytes_per_line;
        p = (p >= view->last_byte) ? current : p;
        if (lines == 1) {
            q = view->edit_cursor + view->bytes_per_line;
            line = q / view->bytes_per_line;
            col = (view->last_byte-1) / view->bytes_per_line;
            view->edit_cursor = (line > col) ? view->edit_cursor : q;
            view->edit_cursor = (view->edit_cursor < view->last_byte) ?
                view->edit_cursor : view->last_byte-1;
            q = current + ((LINES-2) * view->bytes_per_line);
            p = (view->edit_cursor < q) ? current : p;
        } else {
            view->edit_cursor = (view->edit_cursor < p) ?
                p : view->edit_cursor;
        }
        return p;
#else /* HEX_EDIT */
        return (p = current + lines * view->bytes_per_line) >= view->last_byte ? current : p;
#endif /* HEX_EDIT */
    } else {
    	if (upto){
    	    lines = -1;
    	    q = upto;
    	} else
    	    q = view->last_byte;
        for (line = col = 0, p = current; p < q; p++){
	    int c;
	    
	    if (lines != -1 && line >= lines)
	        return p;

	    c = get_byte (view, p);
	    
	    if (wrap_mode){
	    	if (c == '\r')
	    	    continue; /* This characters is never displayed */
	    	else if (c == '\t')
	    	    col = ((col - view->have_frame)/8)*8 +8+ view->have_frame;
		else
		    col++;
	    	if (view->viewer_nroff_flag && c == '\b'){
	    	    if (p + 1 < view->last_byte
			&& is_printable (get_byte (view, p + 1))
	    	        && p > view->first
			&& is_printable (get_byte (view, p - 1)))
		        col -= 2;
	        } else if (col == vwidth){
		    /* FIXME: the c in is_printable was a p, that is a bug,
		       I suspect I got that fix from Jakub, same applies
		       for d. */
		    int d = get_byte (view, p+2);
		    
		    if (p + 2 >= view->last_byte || !is_printable (c) || 
		        !view->viewer_nroff_flag || get_byte (view, p + 1) != '\b' || 
		        !is_printable (d)){
		        col = 0;
			
		        if (c == '\n' || get_byte (view, p+1) != '\n')
		            line++;
		    }
		} else if (c == '\n'){
	            line++;
		    col = 0;
	        }
	    } else if (c == '\n')
	        line++;
        }
        if (upto)
            return line;
    }
    return current;
}

/* returns the new current pointer */
/* Cause even the forward routine became very complex, we in the wrap_mode
   just find the nearest '\n', use move_forward2(p, 0, q) to get the count
   of lines up to there and then use move_forward2(p, something, 0), which we
   return */
static long
move_backward2 (WView *view, long current, int lines)
{
    long p, q;
    int line;

#ifdef HEX_EDIT
    if (!view->hex_mode && current == view->first)
        return current;
#else /* HEX_EDIT */
    if (current == view->first)
	return current;
#endif /* HEX_EDIT */
    
    if (view->hex_mode){
	p = current - lines * view->bytes_per_line;
#ifdef HEX_EDIT
        p = (p < view->first) ? view->first : p;
        if (lines == 1) {
            q = view->edit_cursor - view->bytes_per_line;
            view->edit_cursor = (q < view->first) ? view->edit_cursor : q;
            p = (view->edit_cursor >= current) ? current : p;
        } else {
            q = p + ((LINES-2) * view->bytes_per_line);
            view->edit_cursor = (view->edit_cursor >= q) ?
                p : view->edit_cursor;
        }
        return p;
#else /* HEX_EDIT */

        return (p < view->first) ? view->first : p;
#endif /* HEX_EDIT */
    } else {
    	if (current == view->last_byte
	    && get_byte (view, current - 1) != '\n')
	    /* There is one virtual '\n' at the end,
	       so that the last line is shown */
  	    line = 1;
  	else
  	    line = 0;
        for (q = p = current - 1; p > view->first; p--)
	    if (get_byte (view, p) == '\n')
	    	if (!wrap_mode){
	            if (line == lines)
	            	return p + 1;
	            line++;
	        } else {
	            line += move_forward2 (view, p + 1, 0, q);
	            if (line >= lines){
	            	if (line == lines)
	            	    return p + 1;
	            	else
	            	    return move_forward2 (view, p + 1, line - lines, 0);
	            }
	            q = p + 1;
	        }
    }
    return p;
}

static void
move_backward (WView *view, int i)
{
    view->search_start = view->start_display =
	move_backward2 (view, view->start_display, i);
    view->found_len = 0;
#ifdef HEX_EDIT
    view->last = view->first + ((LINES-2) * view->bytes_per_line);
#endif /* HEX_EDIT */
    view->dirty++;
}

static long
get_bottom_first (WView *view, int do_not_cache, int really)
{
    int bottom_first;

    if (!have_fast_cpu && !really)
	return INT_MAX;
    
    if (!do_not_cache && view->bottom_first != -1)
    	return view->bottom_first;

    /* Force loading */
    if (view->growing_buffer){
	int old_last_byte;

	old_last_byte = -1;
	while (old_last_byte != view->last_byte){
	    old_last_byte = view->last_byte;
	    get_byte (view, view->last_byte+VIEW_PAGE_SIZE);
	}
    }

    bottom_first = move_backward2 (view, view->last_byte, vheight - 1);
    
    if (view->hex_mode)
    	bottom_first = (bottom_first + view->bytes_per_line - 1)
	    / view->bytes_per_line * view->bytes_per_line;
    view->bottom_first = bottom_first;
    
    return view->bottom_first;
}

static void
move_forward (WView *view, int i)
{
    view->start_display = move_forward2 (view, view->start_display, i, 0);
    if (!view->reading_pipe && view->start_display > get_bottom_first (view, 0, 0))
    	view->start_display = view->bottom_first;
    view->search_start = view->start_display;
    view->found_len = 0;
#ifdef HEX_EDIT
    view->last = view->first + ((LINES-2) * view->bytes_per_line);
#endif /* HEX_EDIT */
    view->dirty++;
}


static void
move_to_top (WView *view)
{
    view->search_start = view->start_display = view->first;
    view->found_len = 0;
#ifdef HEX_EDIT
    view->last = view->first + ((LINES-2) * view->bytes_per_line);
#endif /* HEX_EDIT */
    
#ifdef HEX_EDIT
    view->nib_shift = 0;
    view->edit_cursor = view->start_display; 
#endif /* HEX_EDIT */
    
    view->dirty++;
}

static void
move_to_bottom (WView *view)
{
    view->search_start = view->start_display = get_bottom_first (view, 0, 1);
    view->found_len = 0;
#ifdef HEX_EDIT
    view->last = view->first + ((LINES-2) * view->bytes_per_line);
    view->edit_cursor = (view->edit_cursor < view->start_display) ?
        view->start_display : view->edit_cursor;
#endif /* HEX_EDIT */
    view->dirty++;
}

/* Scroll left/right the view panel functions */
static void
move_right (WView *view)
{
#ifndef HEX_EDIT
    if (wrap_mode)
	return;
    
#else /* HEX_EDIT */
    if (wrap_mode && !view->hex_mode)
        return;
    if (view->hex_mode) {
#ifdef HEX_EDIT
        view->last = view->first + ((LINES-2) * view->bytes_per_line);
#endif /* HEX_EDIT */

        if (view->hex_mode && view->view_side == view_side_left) {
            view->nib_shift = 1 - view->nib_shift;
            if (view->nib_shift == 1)
                return;
        } 
        view->edit_cursor = (++view->edit_cursor <  view->last_byte) ?
            view->edit_cursor : view->last_byte - 1;
        if (view->edit_cursor >= view->last) {
           view->edit_cursor -= view->bytes_per_line;
           move_forward(view, 1);
        }
    } else
#endif /* HEX_EDIT */
    if (--view->start_col > 0)
	view->start_col = 0;
    view->dirty++;
}

static void
move_left (WView *view)
{
#ifndef HEX_EDIT
    if (wrap_mode)
	return;
    
#else /* HEX_EDIT */
    if (wrap_mode && !view->hex_mode)
        return;
    if (view->hex_mode) {
        if (view->hex_mode && view->view_side == view_side_left) {
            view->nib_shift = 1 - view->nib_shift;
            if (view->nib_shift == 0)
                return;
        } 
        view->edit_cursor = (--view->edit_cursor < view->first) ?
            view->first : view->edit_cursor;
        if (view->edit_cursor < view->start_display) {
           view->edit_cursor += view->bytes_per_line;
           move_backward(view, 1);
        }
    } else
#endif /* HEX_EDIT */
    if (++view->start_col > 0)
	view->start_col = 0;
    view->dirty++;
}

/* }}} */
/* {{{ Search routines */

/* Case insensitive search of text in data */
static int
icase_search_p (WView *view, char *text, char *data, int nothing)
{
    int p = 0;
    char *q;

    p = (q = icase_search (text, data)) != 0; 
    if (p){
	view->found_len = strlen (text);
	view->search_start = q - data - view->found_len;
    }
    return p;
}

static char *
grow_string_buffer (char *text, int *size)
{
    char *new;
    int  old_size = *size;

    /* The grow steps */
    *size += 160;
    new = xmalloc (*size, "grow_string_buffer");
    strncpy (new, text, old_size);
    if (text)
	free (text);
    return new;
}

static char *
get_line_at (WView *view, long *p)
{
    char *buffer = 0;
    int  buffer_size, usable_size;
    int  ch;
    int  direction;
    long pos = *p;
    long i;

    direction = view->direction;
    buffer_size = usable_size = 0;
    
    i = ch = 0;
    for (;pos >= 0 && (ch = get_byte (view, pos))!= -1; pos += direction, i++){

	/* skip over all the possible zeros in the file */
	if (ch == 0 && i == 0){
	    while (pos >= 0 && ((ch = get_byte (view, pos)) != -1) && ch == 0)
		pos+= direction;
	    if (ch == -1)
		break;
	}
	if (i == usable_size){
	    buffer = grow_string_buffer (buffer, &buffer_size);
	    usable_size = buffer_size - 2;
	    buffer [0] = ' '; /* This makes possible strcpy of buffer */
	}
	buffer [i+1] = ch;
	if (ch == '\n' || !ch || ch == -1){
	    pos += direction; i++;
	    break;
	}
    }
    if (buffer){
	i--;
	buffer [0] = ' ';
	buffer [i+1] = 0;
	
	/* If we are searching backwards, reverse the string */
	if (view->direction < 0)
	    reverse_string (buffer);
    }

    *p = pos;
    return buffer;
}

/** Search status optmizations **/

/* The number of bytes between percent increments */
int  update_steps;

/* Last point where we updated the status */
long update_activate;

static void
search_update_steps (WView *view)
{
    if (view->s.st_size)
	update_steps = 40000;
    else
	update_steps = view->last_byte / 100;

    /* Do not update the percent display but every 20 ks */
    if (update_steps < 20000)
	update_steps = 20000;
}

static void
search (WView *view, char *text, int (*search)(WView *, char *, char *, int))
{
    int w = view->widget.cols - (view->have_frame * 2);
    char *s = NULL;		/*  The line we read from the view buffer */
    long p, beginning;
    int  ch;
    int isatbeg; /* Nonzero means we start search at beginning of some line */
    int found_len, search_start;
    Dlg_head *d = 0;
    int search_status;

    /* Used to keep track of where the line starts, when looking forward */
    /* is the index before transfering the line; the reverse case uses   */
    /* the position returned after the line has been read */
    long forward_line_start;
    long reverse_line_start;
    long t;
    /* Clear interrupt status */
    got_interrupt ();
    
    if (verbose){
	d = message (D_INSERT, " Search ", "Searching %s", text);
	mc_refresh ();
    }
    ch = 0;
    if (view->direction == 1){
	p = view->found_len ? view->search_start + 1 : view->search_start;
    } else {
	p = (view->found_len ? view->search_start : view->last) - 1;
    }
    beginning = p;

    isatbeg = view->found_len == 0;
    found_len = view->found_len;
    search_start = view->search_start;

    /* Compute the percent steps */
    search_update_steps (view);
    update_activate = 0;

    for (; ; isatbeg = 1, free (s)){
#ifdef HAVE_TK
	static int count;

	if ((count++ % 32) == 0)
	    tk_dispatch_all ();
	if (!d->running)
	    break;
#endif
	if (p >= update_activate){
	    update_activate += update_steps;
	    if (verbose){
		view_percent (view, p, w);
		mc_refresh ();
	    }
	    if (got_interrupt ())
		break;
	}
	forward_line_start = p;
	disable_interrupt_key ();
	s = get_line_at (view, &p);
	reverse_line_start = p;
	enable_interrupt_key ();
	if (!s)
	    break;
	
	search_status = (*search) (view, text, s + 1, match_normal);
	if (search_status < 0)
	    break;

	if (search_status == 0)
	    continue;

	/* We found the string */
	
	if (!isatbeg && !view->search_start){
	    
	    /* We do not want to match a
	     * ^ regexp when not at the real
	     * beginning of some line
	     */
	    view->found_len = found_len;
	    view->search_start = search_start;
	    if ((*search) (view, text, s, match_normal) <= 0)
		continue;
	    (*search) (view, text, s + 1, match_normal);
	}
	/* Record the position used to continue the search */
	if (view->direction == 1)
	    t = forward_line_start;
	else
	    t = reverse_line_start ? reverse_line_start + 3 : 0;
	view->search_start += t;

	if (t != beginning){
	    if (t > get_bottom_first (view, 0, 0))
		view->start_display = view->bottom_first;
	    else
		view->start_display = t;
	}
	
	free (s);
	break;
    }
    disable_interrupt_key ();
    if (verbose){
	dlg_run_done (d);
	destroy_dlg (d);
    }

    if (!s){
	message (0, " Search ", " Search string not found ");
	view->found_len = 0;
    }
}

/* Search buffer (it's size is len) in the complete buffer */
/* returns the position where the block was found or -1 if not found */
static long
block_search (WView *view, char *buffer, int len)
{
    int w = view->widget.cols - (view->have_frame * 2);
    char *d = buffer, b;
    long e;

    /* clear interrupt status */
    got_interrupt ();
    enable_interrupt_key ();
    e = view->found_len ? view->search_start + 1 : view->search_start;

    search_update_steps (view);
    update_activate = 0;
    
    for (; e < view->last_byte; e++){
	if (e >= update_activate){
	    update_activate += update_steps;
	    if (verbose){
		view_percent (view, e, w);
		mc_refresh ();
	    }
	    if (got_interrupt ())
		break;
	}
	b = get_byte (view, e);
	
	if (*d == b){
	    d++;
	} else {
	    e -= d - buffer;
	    d = buffer;
	}
	if (d - buffer == len){
	    disable_interrupt_key ();
	    return e - len;
	}
    }
    disable_interrupt_key ();
    return -1;
}

/* States of our funny recognizer */
enum {
    normal,
    inside_quotes,
    zero,
    hex1,
    hex2,
    oct1
};

/* This routine doesn't report all the user mistakes, it just ignores them */
static void
hex_search (WView *view, char *text)
{
    char buffer [120];		/* Where we hold the information */
    int  i, block_len;
    int  v = 0;
    long pos;			/* Where did we found the string */
    char *p;			/* Temporary */
    int  state = normal;	/* Initial state of the micro-scanner */
    
    /* First convert the string to a stream of bytes */
    for (i = block_len = 0; text [i] && block_len < sizeof (buffer); i++){
	switch (state){
	case inside_quotes:
	    if (text [i] == '"')
		state = normal;
	    else
		buffer [block_len++] = text [i];
	    break;

	case normal:
	    if (text [i] == '"'){
		state = inside_quotes;
		break;
	    }
	    if (text [i] == '0'){
		state = zero;
		break;
	    }
	    if (text [i] == 'x'){
		state = hex1;
		break;
	    }
	    break;

	case zero:
	    if (text [i] == 'x')
		state = hex1;
	    break;

	case hex1:
	    v = 0;
	    text [i] = toupper (text [i]);
	    if ((p = strchr (hex_char, text [i])) != 0){
		v = (p - hex_char) << 4;
		state = hex2;
	    }
	    break;

	case hex2:
	    text [i] = toupper (text [i]);
	    if ((p = strchr (hex_char, text [i])) != 0){
		v |= (p - hex_char);
		state = normal;
	    }
	    buffer [block_len++] = v;
	    break;
	}
    }
    /* Then start the search */
    pos = block_search (view, buffer, block_len);
    if (pos == -1){
	message (0, " Search ", " Search string not found ");
	view->found_len = 0;
	return;
    }
    
    view->search_start = pos + 1;
    view->found_len = block_len;
    
    /* Adjust the file offset */
    view->start_display = (pos & (~(view->bytes_per_line-1)));
    if (view->start_display > get_bottom_first (view, 0, 0))
    	view->start_display = view->bottom_first;
}

static int regexp_view_search (WView *view, char *pattern, char *string, int match_type)
{
    static regex_t r;
    static char *old_pattern = NULL;
    static int old_type;
    int startpos, found_len, len;

    if (!old_pattern || strcmp (old_pattern, pattern) || old_type != match_type){
	if (old_pattern){
	    regfree (&r);
	    free (old_pattern);
	}
	if (regcomp (&r, pattern, REG_EXTENDED|REG_NOSUB)){
	    message (1, " Error ", " Invalid regular expression ");
	    return -1;
	}
	old_pattern = strdup (pattern);
	old_type = match_type;
    }
    len = strlen (string);
    startpos = re_search (&r, string, len, 0, len, NULL);
    if (startpos < 0)
    	return 0;
    found_len = re_match (&r, string, len, startpos, NULL);
    view->found_len = found_len;
    view->search_start = startpos;
    return 1;
}

static void do_regexp_search (void *xview, char *regexp)
{
    WView *view = (WView *) xview;
    
    view->search_exp = regexp;
    search (view, regexp, regexp_view_search);
    /* Had a refresh here */
    view->dirty++;
    view_update (view);
}

static void do_normal_search (void *xview, char *text)
{
    WView *view = (WView *) xview;
    
    view->search_exp = text;
    if (view->hex_mode)
	hex_search (view, text);
    else 
	search (view, text, icase_search_p);
    /* Had a refresh here */
    view->dirty++;
    view_update (view);
}

/* }}} */
/* {{{ Mouse and keyboard handling */

/* Real view only */
static void help_cmd (void)
{
    interactive_display (LIBDIR "mc.hlp", "[Internal File Viewer]");
    /*
    view_refresh (0);
    */
}

/* Both views */
void toggle_wrap_mode (WView *view)
{
#ifdef HEX_EDIT
    if (view->hex_mode) {
        if (view->growing_buffer != 0) {
            return;
        }
        get_bottom_first (view, 1, 0);
        if (view->hexedit_mode) {
            view->view_side = 1 - view->view_side;
        } else {
            view->hexedit_mode = 1 - view->hexedit_mode;
        }
        view_labels (view);
        view->dirty++;
        view_update (view);
#else /* HEX_EDIT */
    if (view->hex_mode){
    	get_bottom_first (view, 1, 0);
#endif /* HEX_EDIT */
	return;
    } 
    wrap_mode = 1 - wrap_mode;
    get_bottom_first (view, 1, 0);
    if (wrap_mode)
	view->start_col = 0;
    else {
	if (have_fast_cpu){
	    if (view->bottom_first < view->start_display)
		view->search_start = view->start_display = view->bottom_first;
    	    view->found_len = 0;
	}
    }
    view_labels (view);
    view->dirty++;
    view_update (view);
}

/* Both views */
void
toggle_hex_mode (WView *view)
{
    view->hex_mode = 1 - view->hex_mode;

    if (view->hex_mode){
        /* Shift the line start to 0x____0 on entry, restore it for Ascii */
        view->start_save = view->start_display;
        view->start_display -= view->start_display % view->bytes_per_line;
        view->edit_cursor = view->start_display;
	view->widget.options |= W_WANT_CURSOR;
	view->widget.parent->raw = 1;
    } else {
        view->start_display = view->start_save;
	view->widget.parent->raw = 0;
	view->widget.options &= ~W_WANT_CURSOR;
    }
    altered_hex_mode = 1;
    get_bottom_first (view, 1, 0);
    view_labels (view);
    view->dirty++;
    view_update (view);
}

#ifdef HEX_EDIT
/* Both views */
void
toggle_hexedit_mode(WView *view)
{
    view->hexedit_mode = 1 - view->hexedit_mode;
}
#endif /* HEX_EDIT */

/* Both views */
void
goto_line (WView *view)
{
    char *line, prompt [100];
    int i, oldline = 1;

    for (i = view->first; i < view->start_display; i++)
	if (get_byte (view, i) == '\n')
	    oldline ++;
    sprintf (prompt, " The current line number is %d.\n"
	             " Enter the new line number:", oldline);
    line = input_dialog (" Goto line ", prompt, "");
    if (line){
	if (*line){
	    move_to_top (view);
	    move_forward (view, atoi (line) - 1);
	}
	free (line);
    }
    view->dirty++;
    view_update (view);
}

/* Both views */
void
regexp_search (WView *view, int direction)
{
    char *regexp = "";
    static char *old = 0;

#ifdef HEX_EDIT
    /* This is really an F6 key handler */
    if (view->hex_mode) {
        /* Save it without a confirmation prompt */
        save_edit_changes(view, NULL);
	return;
    }
#else /* HEX_EDIT */ 
    if (view->hex_mode)
	return;
#endif /* HEX_EDIT */ 
    
    regexp = old ? old : regexp;
    regexp = input_dialog (" Search ", " Enter regexp:", regexp);
    if ((!regexp) || (!*regexp)){
	regexp = "";
	return;
    }
    if (old)
	free (old);
    old = strdup (regexp);
#if 0
    /* Mhm, do we really need to load all the file in the core? */
    if (view->bytes_read < view->last_byte)
	get_byte (view, view->last_byte-1);/* Get the whole file in to memory */
#endif
    view->direction = direction;
    do_regexp_search (view, regexp);

    view->last_search = do_regexp_search;
}

void
regexp_search_cmd (WView *view)
{
    regexp_search (view, 1);
}

/* Both views */
void
normal_search (WView *view, int direction)
{
    static char *old;
    char *exp = "";

    exp = old ? old : exp;
    exp = input_dialog (" Search ", " Enter search string:", exp);
    if ((!exp) || (!*exp)){
	exp = "";
	return;
    }
    if (old)
	free (old);
    old = strdup (exp);
/* FIXME: add this after 2.0: free (exp) */

    view->direction = direction;
    do_normal_search (view, exp);
    view->last_search = do_normal_search;
}

void
normal_search_cmd (WView *view)
{
    normal_search (view, 1);
}

void
change_viewer (WView *view)
{
    altered_magic_flag = 1;
    if (!view->command){
    	view->viewer_magic_flag = !view->viewer_magic_flag;
    	view_labels (view);
    } else {
        char *s = strdup (view->filename);
        char *t = strdup (view->command);

        view_done (view);

	/* Is it possible to not use the growing buffers? */
	if (!(*t && !*s))
	    view->viewer_magic_flag = !view->viewer_magic_flag;
    	view_init (view, t, s, 0);
    	free (s);
    	free (t);
    	view_labels (view);
    	view->dirty++;
    	view_update (view);
    }
}

static void
change_nroff (WView *view)
{
    view->viewer_nroff_flag = !view->viewer_nroff_flag;
    altered_nroff_flag = 1;
    view_labels (view);
    view->dirty++;
    view_update (view);
}

/* Real view only */
static void
quit_cmd (WView *view)
{
    view->widget.parent->running = 0;
#ifdef HEX_EDIT
    if (view->change_list)
        save_edit_changes (view, " Save changes? ");
#endif /* HEX_EDIT */
}

/* Both views */
void
view_labels (WView *view)
{
    Dlg_head *h = view->widget.parent;
    
    define_label (h, (Widget *) view, 1, "Help", help_cmd);
    
    my_define (h, 10, "Quit", quit_cmd, view);
    my_define (h, 4, view->hex_mode ?"Ascii":"Hex", toggle_hex_mode, view);
    my_define (h, 5, "Line", goto_line, view);
    my_define (h, 6, view->hex_mode ? "Save" : "RxSrch", regexp_search_cmd, view);

#ifdef HEX_EDIT
    my_define (h, 2, view->hex_mode ? view->hexedit_mode ?
                     view->view_side == view_side_left ? "EdText" : "EdHex" :
                     view->growing_buffer ? "" : "Edit" :
                     wrap_mode ? "UnWrap" : "Wrap",
                     toggle_wrap_mode, view);
#else /* HEX_EDIT */
    my_define (h, 2, view->hex_mode ? "" : wrap_mode ? "UnWrap" : "Wrap",
	       toggle_wrap_mode, view);
 #endif /* HEX_EDIT */
   
    my_define (h, 7, view->hex_mode ? "HxSrch" : "Search",
	       normal_search_cmd, view);
    
    my_define (h, 8, view->viewer_magic_flag ? "Raw" : "Parse",
	       change_viewer, view);

    if (!view->have_frame){
	my_define (h, 9, view->viewer_nroff_flag ? "Unform" : "Format",
		   change_nroff, view);
	my_define (h, 3, "Quit", quit_cmd, view);
    }
    
    redraw_labels (h, (Widget *) view);
}

/* Both views */
static int
check_left_right_keys (WView *view, int c)
{
    if (c == KEY_LEFT)
	move_left (view);
    else if (c == KEY_RIGHT)
	move_right (view);
    else return 0;

    return 1;
}

enum { off, on };

static void
set_monitor (WView *view, int set_on)
{
    int old = view->monitor;
    
    view->monitor = set_on;
    
    if (view->monitor){
	move_to_bottom (view);
	view->bottom_first = -1;
	set_idle_proc (view->widget.parent, 1);
    } else {
	if (old)
	    set_idle_proc (view->widget.parent, 0);
    }
}

/* Both views */
static int
view_handle_key (WView *view, int c)
{
    int l;
    int prev_monitor = view->monitor;

    set_monitor (view, off);
    
#ifdef HEX_EDIT
    if (view->hex_mode) {
        switch (c) {
        case 0x09:		/* Tab key */
            view->view_side = 1 - view->view_side;
            view->dirty++;
            return 1;

        case XCTRL('a'):        /* Beginning of line */
            view->edit_cursor -= view->edit_cursor % view->bytes_per_line;
            view->dirty++;
            return 1;
    
        case XCTRL('b'):        /* Character back */
            move_left(view);
            return 1;

        case XCTRL('e'):        /* End of line */
            view->edit_cursor -= view->edit_cursor % view->bytes_per_line;
            view->edit_cursor += view->bytes_per_line - 1;
            view->dirty++;
            return 1;
    
        case XCTRL('f'):        /* Character forward */
            move_right(view);
            return 1;
        }
    
	/* Trap 0-9,A-Z,a-z for left side data entry (hex editing) */
        if (view->view_side == view_side_left){
            if ((c >= '0' && c <= '9') || 
                (c >= 'A' && c <= 'A') ||
                (c >= 'a' && c <= 'f')){
        
                put_editkey (view, c);
                return 1;
            }
        }
	
	/* Trap all printable characters for right side data entry */
	/* Also enter the value of the Enter key */
	if (view->view_side == view_side_right){
	    if ((c >= ' ' && c <= '~') || (c == '\n')){
		put_editkey(view, c);
		return 1;
	    }
	}
    }
#endif /* HEX_EDIT */
    
    if (check_left_right_keys (view, c))
	return 1;
    
    if (check_movement_keys (c, 1, vheight, view, (movefn) move_backward, (movefn) move_forward,
			     (movefn) move_to_top, (movefn) move_to_bottom)){
	return 1;
    }
    switch (c){

    case '?':
	regexp_search (view, -1);
	return 1;
	
    case '/':
	regexp_search (view, 1);
	return 1;

	/* Continue search */
    case XCTRL('s'):
    case 'n':
	if (view->last_search){
	    (*view->last_search)(view, view->search_exp);
	} else {
	    /* if not... then ask for an expression */
	    normal_search (view, 1);
	}
	return 1;

    case XCTRL('r'):
	if (view->last_search){
	    (*view->last_search)(view, view->search_exp);
	} else {
	    normal_search (view, -1);
	}
	return 1;
	
    case 'h':
        move_left (view);
        return 1;
        
    case 'j':
    case '\n':
    case 'e':
    	move_forward (view, 1);
    	return 1;
    	
    case 'd':
        move_forward (view, vheight / 2);
        return 1;
        
    case 'u':
        move_backward (view, vheight / 2);
        return 1;
    	
    case 'k':
    case 'y':
        move_backward (view, 1);
        return 1;
        
    case 'l':
        move_right (view);
        return 1;
        
    case ' ':
    case 'f':
        move_forward (view, vheight - 1);
        return 1;

    case '!':
	exec_shell ();
	return 1;
	
    case 'F':
	set_monitor (view, on);
	return 1;
	
    case 'b':
    	move_backward (view, vheight - 1);
    	return 1;
        
    case KEY_IC:
        move_backward (view, 2);
        return 1;
        
    case KEY_DC:
        move_forward (view, 2);
        return 1;

    case 'm':
	view->marks [view->marker] = view->start_display;
	return 1;

    case 'r':
	view->start_display = view->marks [view->marker];
	view->dirty++;
	return 1;
	
	/*  Use to indicate parent that we want to see the next/previous file */
	/* Only works on full screen mode */
    case XCTRL('f'):
    case XCTRL('b'):
	if (!view->have_frame)
	    view->move_dir = c == XCTRL('f') ? 1 : -1;
	/* fall */

    case 'q':
    case XCTRL('g'):
    case ESC_CHAR:
	view->view_quit = 1;
	return 1;

    }
    if (c >= '0' && c <= '9')
	view->marker = c - '0';

    /* Restore the monitor status */
    set_monitor (view, prev_monitor);
    
    /* Key not used */
    return 0;
}

/* Both views */
int
view_event (WView *view, Gpm_Event *event, int *result)
{
    *result = MOU_NORMAL;
    if (event->type & (GPM_DOWN|GPM_DRAG)){
    	if (!wrap_mode){
    	    if (event->x < view->widget.cols / 4){
    	    	move_left (view);
    	    	*result = MOU_REPEAT;
    	    	return 1;
    	    }
    	    if (event->x > 3 * vwidth / 4){
    	    	move_right (view);
    	    	*result = MOU_REPEAT;
    	    	return 1;
    	    }
	}
	if (event->y < view->widget.lines / 3){
	    if (mouse_move_pages_viewer)
	    	move_backward (view, view->widget.lines / 2 - 1);
	    else
	    	move_backward (view, 1);
	    *result = MOU_REPEAT;
	    return 1;
	}
	else if (event->y > 2 * vheight /3){
	    if (mouse_move_pages_viewer)
	    	move_forward (view, vheight / 2 - 1);
	    else
	    	move_forward (view, 1);
	    *result = MOU_REPEAT; 
	    return 1;
	}
    }
    return 0;
}

/* Real view only */
int
real_view_event (Gpm_Event *event, void *x)
{
    int result;
    
    if (view_event ((WView *) x, event, &result))
    	view_update ((WView *) x);
    return result;
}

/* }}} */
/* {{{ Window creation, destruction and a driver stub for real view */

static int
view_mode_callback (struct Dlg_head *h, int id, int msg)
{
    return default_dlg_callback (h, id, msg);
}

#ifdef HAVE_XVIEW
/* Real view only */

void
view_adjust_size (Dlg_head *unused)
{
}

int
view (char *_command, char *_file, int *move_dir_p, viewline_t start_line)
{
    int midnight_colors [4];
    int error;
    WView      *wview;

    wview = view_new (0, 0, COLS, LINES - 1, 0);

    error = view_init (wview, _command, _file, start_line);
    if (!error){
	x_view (wview);	
    }
    *move_dir_p = 0;
    return !error;
}
#else
Dlg_head   *view_dlg;

void
view_adjust_size (Dlg_head *h)
{
    WView      *view;
    WButtonBar *bar;

    /* Look up the viewer and the buttonbar, we assume only two widgets here */
    view = (WView *) find_widget_type (h, (void *) &view_callback);
    bar  = (WButtonBar *) view->widget.parent->current->next->widget;
    widget_set_size (&view->widget, 0, 0, LINES-1, COLS);
    widget_set_size (&bar->widget, LINES-1, 0, 1, COLS);
}

/* Real view only */
int
view (char *_command, char *_file, int *move_dir_p, int start_line)
{
    int midnight_colors [4];
    int error;
    WView *wview;
    WButtonBar *bar;

    
    /* Create dialog and widgets, put them on the dialog */
    view_dlg = create_dlg (0, 0, LINES, COLS, midnight_colors,
			   view_mode_callback, "[Internal File Viewer]",
			   "view",
			   DLG_NONE);

    wview = view_new (0, 0, COLS, LINES-1, 0);

    bar  = buttonbar_new (1);

    add_widget (view_dlg, wview);
    add_widget (view_dlg, bar);

    error = view_init (wview, _command, _file, start_line);
    if (move_dir_p)
	*move_dir_p = 0;

    /* Please note that if you add another widget,
     * you have to modify view_adjust_size to
     * be aware of it
     */
    if (!error){
	run_dlg (view_dlg);
	if (move_dir_p)
	    *move_dir_p = wview->move_dir;
    }
    destroy_dlg (view_dlg);
    
    return !error;
}
#endif

static void
view_hook (void *v)
{
    WView *view = (WView *) v;
    WPanel *panel;

    /* If the user is busy typing, wait until he finishes to update the
       screen */
    if (!is_idle ()){
	if (!hook_present (idle_hook, view_hook))
	    add_hook (&idle_hook, view_hook, v);
	return;
    }

    delete_hook (&idle_hook, view_hook);
    
    if (get_current_type () == view_listing)
	panel = current_panel;
    else if (get_other_type () == view_listing)
	panel = other_panel;
    else
	return;
	
    if (!S_ISREG (panel->dir.list [panel->selected].buf.st_mode))
	return;
    
    view_init (view, 0, panel->dir.list [panel->selected].fname, 0);
    display (view);
    view_status (view);
}

static int
view_callback (Dlg_head *h, WView *view, int msg, int par)
{
    int v;
    
    switch (msg){
    case WIDGET_INIT:
	x_create_viewer (view);
	if (view->have_frame)
	    add_hook (&select_file_hook, view_hook, view);
	else
	    view_labels (view);
	break;
	
    case WIDGET_DRAW:
	display (view);
	view_status (view);
	break;

    case WIDGET_CURSOR:
	if (view->hex_mode)
	    view_place_cursor (view);
	break;

    case WIDGET_KEY:
	v = view_handle_key ((WView *)view, par);
	if (view->view_quit)
	    h->running = 0;
	else {
	    view_update (view);
	}
	return v;

    case WIDGET_IDLE:
	/* This event is generated when the user is using the 'F' flag */
	view->bottom_first = -1;
	move_to_bottom (view);
	display (view);
	view_status (view);
	sleep (1);
	return 1;
	
    case WIDGET_FOCUS:
#ifdef HAVE_TK
	tk_evalf ("focus %s.v.view", wtk_win (view->widget));
#endif
	view_labels (view);
	return 1;
	
    }
    return default_proc (h, msg, par);
}

WView *
view_new (int y, int x, int cols, int lines, int is_panel)
{
    WView *view = xmalloc (sizeof (WView), "view_new");
    
    init_widget (&view->widget, y, x, lines, cols,
		 (callback_fn) view_callback,
		 (destroy_fn) view_destroy,
		 (mouse_h) real_view_event, NULL);

    view->filename = 0;
    view->view_active = 0;
    view->bottom_first = 0;
    view->start_col = 0;
    view->dirty = 0;
    view->hex_mode = default_hex_mode;
#ifdef HEX_EDIT
    view->hexedit_mode = default_hexedit_mode;
#endif /* HEX_EDIT */
    view->viewer_magic_flag = default_magic_flag;
    view->viewer_nroff_flag = default_nroff_flag;
    view->view_quit = 0;
    view->move_dir = 0;
    view->have_frame = is_panel;
    view->last_byte = -1;
    view->monitor = 0;
#ifdef HAVE_TK
    view->status_shown = 0;
    view->current_line = 1;
    view->cache_len = 80;
    view->last_col = 0;
    view->cache = xmalloc (81, "view->cache");
    view->color_cache = xmalloc (81, "view->cache");
    view->direction = 1;
    bzero (view->cache, 81);
    view->dest = 0;
#endif
    widget_want_cursor (view->widget, 0);

    return view;
}

/* }}} */
/* {{{ Emacs local variables */

/*
   Cause emacs to enter folding mode for this file:
   Local variables:
   end:
   */

/* }}} */
