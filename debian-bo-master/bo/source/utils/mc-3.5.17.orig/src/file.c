/* {{{ Copyright */

/* File managing.  Important notes on this file:
   
   About the use of dialogs in this file:
     If you want to add a new dialog box (or call a routine that pops
     up a dialog box), you have to provide a wrapper for background
     operations (ie, background operations have to up-call to the parent
     process).

     For example, instead of using the message() routine, in this
     file, you should use one of the stubs that call message with the
     proper number of arguments (ie, message_1s, message_2s and so on).

     Actually, that is a rule that should be followed by any routines
     that may be called from this module.

*/

/* File managing
   Copyright (C) 1994, 1995, 1996 The Free Software Foundation
   
   Written by: 1994, 1995       Janne Kukonlehto
               1994, 1995       Fred Leeflang
               1994, 1995, 1996 Miguel de Icaza
               1995, 1996       Jakub Jelinek

   The copy code was based in GNU's cp, and was written by:
   Torbjorn Granlund, David MacKenzie, and Jim Meyering.

   The move code was based in GNU's mv, and was written by:
   Mike Parker and David MacKenzie.

   Janne Kukonlehto added much error recovery to them for being used
   in an interactive program.

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

/* {{{ Include files */

#include <config.h>
/* Hack: the vfs code should not rely on this */
#define WITH_FULL_PATHS 1

#include <sys/types.h>
#include <dirent.h>
#include <stdio.h>
#include <errno.h>
#include "tty.h"
#include <ctype.h>
#include <malloc.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include <time.h>
#ifdef _OS_NT
#include <sys/utime.h>
#else
#include <utime.h>
#endif
#include "mad.h"
#include "regex.h"
#include "util.h"
#include "dialog.h"
#include "global.h"
/* Needed by query_replace */
#include "color.h"
#include "win.h"
#include "dlg.h"
#include "widget.h"
#define WANT_WIDGETS
#include "main.h"		/* WANT_WIDGETS-> we get the the_hint def */
#include "file.h"
#include "layout.h"
#include "widget.h"
#include "wtools.h"
#include "background.h"

/* Needed for current_panel, other_panel and WTree */
#include "dir.h"
#include "panel.h"
#include "tree.h"

#include "key.h"
#include "../vfs/vfs.h"

/* }}} */

/* rcsid [] = "$Id: file.c,v 1.12 1995/02/21 19:05:47 miguel Exp $" */
int verbose = 1;

/* Recursive operation on subdirectories */
int dive_into_subdirs = 1;

/* If running as root, preserve the original uid/gid on file copy */
int preserve_uidgid = 1;

/* If off, it gets a little scrict with dangerous operations */
int know_what_am_i_doing = 1;

int stable_symlinks = 0;

/* The next two are not static, since they are used on background.c */
/* Controls appending to files, shared with filequery.c */
int do_append = 0;

/* result from the recursive query */
int recursive_result;

/* This is a hard link cache */
struct link {
    struct link *next;
    vfs *vfs;
    dev_t dev;
    ino_t ino;
    short linkcount;
    char name[1];
};

struct link *linklist = NULL;
struct re_pattern_buffer rx;
struct re_registers regs;
static char *dest_mask = NULL;
static int op_follow_symlinks = 0;

#define WX 60
#define WY 10
#define BY 10

#define DIF_TIME(t1,t2) ((t2.tv_sec -t1.tv_sec) *1000+ \
			 (t2.tv_usec-t1.tv_usec)/1000)

static Dlg_head *op_dlg;
static char *op_names [] = { " Copy ", " Move ", " Delete " };
static int selected_button;
static int last_percentage [3];

/* Used for button result values */
enum {
    REPLACE_YES = B_USER,
    REPLACE_NO,
    REPLACE_APPEND,
    REPLACE_ALWAYS,
    REPLACE_UPDATE,
    REPLACE_NEVER,
    REPLACE_ABORT
};

static int replace_colors [4];
static Dlg_head *replace_dlg;
static char *replace_filename;
static struct stat *s_stat, *d_stat;
static int replace_result;

enum {
    RECURSIVE_YES,
    RECURSIVE_NO,
    RECURSIVE_ALWAYS,
    RECURSIVE_NEVER,
    RECURSIVE_ABORT
};

static int recursive_erase (char *s);

/* Describe the components in the panel operations window */
static WLabel *FileLabel [2];
static WLabel *FileString [2];
static WLabel *ProgressLabel [3];
static WGauge *ProgressGauge [3];

/* }}} */

/* {{{ File progress display routines */

static void update_buttons (void)
{
#ifndef HAVE_X
#define h op_dlg
    int minus = verbose ? 0 : 3;
    
    attrset ((selected_button == FILE_SKIP) ? FOCUSC : NORMALC);
    dlg_move (op_dlg, BY-minus, 14);
    addstr ("[ Skip ]");
    attrset ((selected_button == FILE_ABORT) ? FOCUSC : NORMALC);
    dlg_move (op_dlg, BY-minus, WX - 19);
    addstr ("[ Abort ]");
    mc_refresh ();
#undef h
#endif    
}

static int check_buttons (void)
{
#ifndef HAVE_X
    int c;

#if 0
    rotate_dash ();
#endif

    /* no delay please says the 1 there */
    c = get_key_code (1);
    
    if (c == ERR)
        return FILE_CONT;

    switch (c){
    case 's':
	selected_button = FILE_SKIP;
	update_buttons ();
	return selected_button;
    case 'a':
    case KEY_F(10):
    case XCTRL('g'):
    case XCTRL('c'):
    case ESC_CHAR:
	selected_button = FILE_ABORT;
	update_buttons ();
	return selected_button;
    case ' ':
    case '\n':
    case KEY_ENTER:
	return selected_button;
    case '\t':
    case KEY_LEFT:
    case KEY_RIGHT:
	selected_button = (selected_button == FILE_SKIP) ? FILE_ABORT : FILE_SKIP;
	update_buttons ();
	/* Fall through */
    default:
	return FILE_CONT;
    }
#else
#ifdef HAVE_TK
    tk_dispatch_all ();
#endif
#ifdef HAVE_XVIEW
    xv_dispatch_something ();
#endif
    if (op_dlg->running)
        return FILE_CONT;
#    ifdef ZERO
/*FIXME: I think FRAME_CMD kills the dialog whenever we click a button, so that is
    why this is ifdefed*/
    else if (op_dlg->ret_value == B_CANCEL)
        return FILE_ABORT;
    else
    	return op_dlg->ret_value;
#    else
    return FILE_ABORT;
#    endif    	
#endif
}

static int op_win_callback (struct Dlg_head *h, int id, int msg)
{
    switch (msg){
#ifndef HAVE_X    
    case DLG_DRAW:
	attrset (REVERSE_COLOR);
	dlg_erase (h);
	draw_box (h, 1, 2, h->lines-2, h->cols-4);
	update_buttons ();
	return 1;
#endif
    }
    return 0;
}

/* Used to save the hint line */
static int last_hint_line;

void create_op_win (int op)
{
    int i;
#ifdef HAVE_XVIEW    
    char *sixty = "                                                                                   ";
    char *fifteen = "               ";
#else
    char *sixty = "";
    char *fifteen = "";
#endif    

    replace_result = 0;
    recursive_result = 0;

    op_dlg = create_dlg (0, 0, WY+4, WX+4, dialog_colors,
			 op_win_callback, "", "opwin", DLG_CENTER);

    last_hint_line = the_hint->widget.y;
    if ((op_dlg->y + op_dlg->lines) > last_hint_line)
	the_hint->widget.y = op_dlg->y + op_dlg->lines+1;
	
    x_set_dialog_title (op_dlg, "");

#ifndef HAVE_X
    add_widget (op_dlg, button_new (0, 0, 0, "", 0, -1, 0, 0, NULL));
#else
    tk_new_frame (op_dlg, "b.");
    add_widgetl (op_dlg, button_new (0, 0, FILE_ABORT, "Abort", 0, -1, 0, 0, NULL),
        XV_WLAY_RIGHTOF);
    add_widgetl (op_dlg, button_new (0, 0, FILE_SKIP, "Skip", 0, -1, 0, 0, NULL),
        XV_WLAY_CENTERROW);
#endif        
    tk_new_frame (op_dlg, "2.");
    add_widgetl (op_dlg, ProgressGauge [2] = gauge_new (8, 14, 0, 100, 0, NULL), 
        XV_WLAY_RIGHTOF);
    add_widgetl (op_dlg, ProgressLabel [2] = label_new (8, 5, fifteen, NULL), 
        XV_WLAY_NEXTROW);
    tk_new_frame (op_dlg, "1.");
    add_widgetl (op_dlg, ProgressGauge [1] = gauge_new (7, 14, 0, 100, 0, NULL), 
        XV_WLAY_RIGHTOF);
    add_widgetl (op_dlg, ProgressLabel [1] = label_new (7, 5, fifteen, NULL), 
        XV_WLAY_NEXTROW);
    tk_new_frame (op_dlg, "0.");
    add_widgetl (op_dlg, ProgressGauge [0] = gauge_new (6, 14, 0, 100, 0, NULL), 
        XV_WLAY_RIGHTOF);
    add_widgetl (op_dlg, ProgressLabel [0] = label_new (6, 5, fifteen, NULL), 
        XV_WLAY_NEXTROW);
    tk_new_frame (op_dlg, "f1.");
    add_widgetl (op_dlg, FileString [1] = label_new (4, 14, sixty, NULL),
        XV_WLAY_RIGHTOF);
    add_widgetl (op_dlg, FileLabel [1] = label_new (4, 5, fifteen, NULL), 
        XV_WLAY_NEXTROW);
    tk_new_frame (op_dlg, "f0.");
    add_widgetl (op_dlg, FileString [0] = label_new (3, 14, sixty, NULL),
        XV_WLAY_RIGHTOF);
    add_widgetl (op_dlg, FileLabel [0] = label_new (3, 5, fifteen, NULL), 
        XV_WLAY_NEXTROW);
	
    /* We will manage the dialog without any help, that's why
       we have to call init_dlg */
    init_dlg (op_dlg);
    op_dlg->running = 1;
    selected_button = FILE_SKIP;
    update_buttons ();
    for (i = 0; i < 3; i++)
	last_percentage [i] = -99;
}

void destroy_op_win (void)
{
#ifdef HAVE_XVIEW
    xtoolkit_kill_dialog (op_dlg);
#endif
    dlg_run_done (op_dlg);
    destroy_dlg (op_dlg);
    the_hint->widget.y = last_hint_line;
}

static int show_no_bar (int n)
{
    if (n >= 0) {
    	label_set_text (ProgressLabel [n], "");
        gauge_show (ProgressGauge [n], 0);
    }
    return check_buttons ();
}

#ifndef HAVE_X
#define truncFileString(s) name_trunc (s, 47)
#else
#define truncFileString(s) s
#endif

static int show_source (char *s)
{
    if (s != NULL){

#ifdef WITH_FULL_PATHS
    	int i = strlen (cpanel->cwd);

	/* We remove the full path we have added before */
        if (!strncmp (s, cpanel->cwd, i)){ 
            if (s[i] == PATH_SEP)
            	s += i + 1;
        }
#endif /* WITH_FULL_PATHS */
	
	label_set_text (FileLabel [0], "Source");
	label_set_text (FileString [0], truncFileString (s));
	return check_buttons ();
    } else {
	label_set_text (FileLabel [0], "");
	label_set_text (FileString [0], "");
	return check_buttons ();
    }
}

static int show_target (char *s)
{
    if (s != NULL){
	label_set_text (FileLabel [1], "Target");
	label_set_text (FileString [1], truncFileString (s));
	return check_buttons ();
    } else {
	label_set_text (FileLabel [1], "");
	label_set_text (FileString [1], "");
	return check_buttons ();
    }
}

static int show_deleting (char *s)
{
    label_set_text (FileLabel [0], "Deleting");
    label_set_text (FileString [0], truncFileString (s));
    return check_buttons ();
}

static int show_bar (int n, long done, long total)
{
    gauge_set_value (ProgressGauge [n], (int) total, (int) done);
    gauge_show (ProgressGauge [n], 1);
    return check_buttons ();
}

static int show_file_progress (long done, long total)
{
    if (!verbose)
	return check_buttons ();
    if (total > 0){
	label_set_text (ProgressLabel [0], "File");
	return show_bar (0, done, total);
    } else
	return show_no_bar (0);
}

static int show_count_progress (long done, long total)
{
    if (!verbose)
	return check_buttons ();
    if (total > 0){
	label_set_text (ProgressLabel [1], "Count");
	return show_bar (1, done, total);
    } else
	return show_no_bar (1);
}

static int show_bytes_progress (long done, long total)
{
    if (!verbose)
	return check_buttons ();
    if (total > 0){
	label_set_text (ProgressLabel [2], "Bytes");
	return show_bar (2, done, total);
    } else
	return show_no_bar (2);
}

/* }}} */


/* {{{ Copy routines */

enum CaseConvs { NO_CONV=0, UP_CHAR=1, LOW_CHAR=2, UP_SECT=4, LOW_SECT=8 };

int convert_case (int c, enum CaseConvs *conversion)
{
    if (*conversion & UP_CHAR){
	*conversion &= ~UP_CHAR;
	return toupper (c);
    } else if (*conversion & LOW_CHAR){
	*conversion &= ~LOW_CHAR;
	return tolower (c);
    } else if (*conversion & UP_SECT){
	return toupper (c);
    } else if (*conversion & LOW_SECT){
	return tolower (c);
    } else
	return c;
}

static int transform_error = 0;
static char *do_transform_source (char *source)
{
    int j, k, l, len;
    char *fnsource = x_basename (source);
    int next_reg;
    enum CaseConvs case_conv = NO_CONV;
    static char fntarget [MC_MAXPATHLEN];
    
    len = strlen (fnsource);
    j = re_match (&rx, fnsource, len, 0, &regs);
    if (j != len) {
        transform_error = FILE_SKIP;
    	return NULL;
    }
    for (next_reg = 1, j = 0, k = 0; j < strlen (dest_mask); j++) {
        switch (dest_mask [j]) {
	case '\\':
	    j++;
	    if (! isdigit (dest_mask [j])){
		/* Backslash followed by non-digit */
		switch (dest_mask [j]){
		case 'U':
		    case_conv |= UP_SECT;
		    case_conv &= ~LOW_SECT;
		    break;
		case 'u':
		    case_conv |= UP_CHAR;
		    break;
		case 'L':
		    case_conv |= LOW_SECT;
		    case_conv &= ~UP_SECT;
		    break;
		case 'l':
		    case_conv |= LOW_CHAR;
		    break;
		case 'E':
		    case_conv = NO_CONV;
		    break;
		default:
		    /* Backslash as quote mark */
		    fntarget [k++] = convert_case (dest_mask [j], &case_conv);
		}
		break;
	    } else {
		/* Backslash followed by digit */
		next_reg = dest_mask [j] - '0';
		/* Fall through */
	    }
                
	case '*':
	    if (next_reg < 0 || next_reg >= RE_NREGS
		|| regs.start [next_reg] < 0) {
		message_1s (1, " Error ", " Invalid target mask ");
		transform_error = FILE_ABORT;
		return NULL;
	    }
	    for (l = regs.start [next_reg]; l < regs.end [next_reg]; l++)
		fntarget [k++] = convert_case (fnsource [l], &case_conv);
	    next_reg ++;
	    break;
            	
	default:
	    fntarget [k++] = convert_case (dest_mask [j], &case_conv);
	    break;
        }
    }
    fntarget [k] = 0;
    return fntarget;
}

static char *transform_source (char *source)
{
    char *s = strdup (source);
    char *q;

    /* We remove \n from the filename since regex routines would use \n as an anchor */
    /* this is just to be allowed to maniupulate file names with \n on it */
    for (q = s; *q; q++){
	if (*q == '\n')
	    *q = ' ';
    }
    q = do_transform_source (s);
    free (s);
    return q;
}

void free_linklist (void)
{
    struct link *lp, *lp2;
    
    for (lp = linklist; lp != NULL; lp = lp2){
    	lp2 = lp -> next;
    	free (lp);
    }
    linklist = NULL;
}

/* Returns 0 if the inode wasn't found in the cache and 1 if it was found
   and a hardlink was succesfully made */
int check_hardlinks (char *src_name, char *dst_name, struct stat *pstat)
{
    struct link *lp;
    vfs *my_vfs = vfs_type (src_name);
    ino_t ino = pstat->st_ino;
    dev_t dev = pstat->st_dev;
    struct stat link_stat;
    char *p;

    if (vfs_file_is_ftp (src_name))
        return 0;
    for (lp = linklist; lp != NULL; lp = lp -> next)
        if (lp->vfs == my_vfs && lp->ino == ino && lp->dev == dev){
            if (!mc_stat (lp->name, &link_stat) && link_stat.st_ino == ino &&
                link_stat.st_dev == dev && vfs_type (lp->name) == my_vfs){
                p = strchr (lp->name, 0) + 1; /* i.e. where the `name' file
            				         was copied to */
                if (vfs_type (dst_name) == vfs_type (p)){
            	    if (!mc_stat (p, &link_stat)){
            	    	if (!mc_link (p, dst_name))
            	    	    return 1;
            	    }
            	}
            }
            /* FIXME: Announce we couldn't make the hardlink */
            return 0;
        }
    lp = (struct link *) xmalloc (sizeof (struct link) + strlen (src_name) 
                                  + strlen (dst_name) + 1, "Hardlink cache");
    if (lp){
    	lp->vfs = my_vfs;
    	lp->ino = ino;
    	lp->dev = dev;
    	strcpy (lp->name, src_name);
    	p = strchr (lp->name, 0) + 1;
    	strcpy (p, dst_name);
    	lp->next = linklist;
    	linklist = lp;
    }
    return 0;
}

int copy_file_file (char *src_path, char *dst_path, int toplevel, 
    int ask_overwrite)
{
#ifndef _OS_NT
    uid_t src_uid;
    gid_t src_gid;
#endif
    char *buf = 0;
    int  buf_size = 8*1024;
    int  dest_desc = 0;
    int  source_desc;
    int  n_read;
    int  n_written;
    int  src_mode;		/* The mode of the source file */
    struct stat sb, sb2;
    struct utimbuf utb;
    int  dst_exists = 0;
    long n_read_total = 0;
    long file_size;
    int  return_status, temp_status;
    int do_remote_copy = 0;
    int appending = 0;

    return_status = FILE_RETRY;

    if (show_source (src_path) == FILE_ABORT
	|| show_target (dst_path) == FILE_ABORT)
	return FILE_ABORT;
    mc_refresh ();

 retry_dst_stat:
    if (!mc_lstat (dst_path, &sb2)){
	if (S_ISDIR (sb2.st_mode)){
	    return_status = file_error (" Cannot overwrite directory \"%s\" ",
					dst_path);
	    if (return_status == FILE_RETRY)
		goto retry_dst_stat;
	    return return_status;
	}
	dst_exists = 1;
    }

 retry_src_lstat:
    if (mc_lstat (src_path, &sb)){
	return_status = file_error (" Cannot lstat source file \"%s\" \n %s ",
				    src_path);
	if (return_status == FILE_RETRY)
	    goto retry_src_lstat;
	return return_status;
    }
    
    if (dst_exists){
	/* Destination already exists */
	if (sb.st_dev == sb2.st_dev
	    && sb.st_ino == sb2.st_ino){
	    message_3s (1, " Error ", " `%s' and `%s' are the same file. ",
		     src_path, dst_path);
	    do_refresh ();
	    return FILE_SKIP;
	}
	if (S_ISDIR (sb2.st_mode)){
	    message_2s (1, " Error ", " Cannot overwrite directory `%s' ",
		     dst_path);
	    do_refresh ();
	    return FILE_SKIP;
	}

	/* Should we replace destination? */
	if (ask_overwrite) {
	    return_status = query_replace (dst_path, &sb, &sb2);
	    if (return_status != FILE_CONT)
	        return return_status;
	}
    }

    if (!do_append) {
    
        /* Check the hardlinks */
        if (sb.st_nlink > 1 && check_hardlinks (src_path, dst_path, &sb) == 1) {
    	    /* We have made a hardlink - no more processing is necessary */
    	    return return_status;
        }
#ifndef _OS_NT    
        if (S_ISLNK (sb.st_mode) && (!toplevel || !op_follow_symlinks)){
	    char link_target [MC_MAXPATHLEN];
	    int len;

        retry_src_readlink:
	    len = mc_readlink (src_path, link_target, MC_MAXPATHLEN);
	    if (len < 0){
	        return_status = file_error
		    (" Cannot read source link \"%s\" \n %s ", src_path);
	        if (return_status == FILE_RETRY)
		    goto retry_src_readlink;
	        return return_status;
	    }
	    link_target [len] = 0;
	    if (stable_symlinks && *link_target != PATH_SEP) {
		char *p, *q, *r, *s;
		
		p = strdup (src_path);
		r = strrchr (p, PATH_SEP);
		if (r) {
		    r[1] = 0;
		    if (*dst_path == PATH_SEP)
		        q = strdup (dst_path);
		    else
		    	q = copy_strings (p, dst_path, 0);
		    r = strrchr (q, PATH_SEP);
		    if (r) {
		        r[1] = 0;
		        s = copy_strings (p, link_target, NULL);
		        strcpy (link_target, s);
		        free (s);
			s = diff_two_paths (q, link_target);
			if (s) {
			    strcpy (link_target, s);
			    free (s);
			}
		    }
		    free (q);
		}
		free (p);
	    }
	    
        retry_dst_symlink:
	    if (mc_symlink (link_target, dst_path) == 0)
	        /* Success */
	        return FILE_CONT;
	    /*
	     * if dst_exists, it is obvious that this had failed.
	     * We can delete the old symlink and try again...
	     */
	    if (dst_exists && S_ISLNK (sb2.st_mode)) {
	        if (!mc_unlink (dst_path))
	            if (mc_symlink (link_target, dst_path) == 0)
	                /* Success */
	                return FILE_CONT;
	    } 
	    return_status = file_error
		(" Cannot create target symlink \"%s\" \n %s ", dst_path);
	    if (return_status == FILE_RETRY)
		goto retry_dst_symlink;
	    return return_status;
        }
#endif /* !OS_NT */
        if (S_ISCHR (sb.st_mode) || S_ISBLK (sb.st_mode) || S_ISFIFO (sb.st_mode)
            || S_ISSOCK (sb.st_mode)){
        retry_mknod:        
            if (mc_mknod (dst_path, sb.st_mode, sb.st_rdev) < 0){
	        return_status = file_error
		    (" Cannot create special file \"%s\" \n %s ", dst_path);
	        if (return_status == FILE_RETRY)
		    goto retry_mknod;
	        return return_status;
	    }
	    /* Success */
	    return FILE_CONT;
        }
    }
    
    if (!do_append && !vfs_file_is_local (src_path) && vfs_file_is_local (dst_path)){
	
	mc_setctl (src_path, MCCTL_SETREMOTECOPY, dst_path);
    }
 retry_src_open:
    if ((source_desc = mc_open (src_path, O_RDONLY)) < 0){
	return_status = file_error
	    (" Cannot open source file \"%s\" \n %s ", src_path);
	if (return_status == FILE_RETRY)
	    goto retry_src_open;
	do_append = 0;
	return return_status;
    }

#ifndef _OS_NT
    /* Only unix uses the vfs ctls, vfs does not work on NT yet */
#define USE_VFS_CTLS 1
#endif
#ifdef USE_VFS_CTLS
    /* Before we can use the mc_ctl routine, the ftpfs code should be
     * changed so that it aborts connections as it should
     *
     * Jakub?
     */
    do_remote_copy = mc_ctl (source_desc, MCCTL_ISREMOTECOPY, 0);
#else
#define do_remote_copy 0
#define mc_ctl(a,b,c) 0
#endif
    
    if (!do_remote_copy) {
 retry_src_fstat:
        if (mc_fstat (source_desc, &sb)){
	    return_status = file_error
	        (" Cannot fstat source file \"%s\" \n %s ", src_path);
	    if (return_status == FILE_RETRY)
	        goto retry_src_fstat;
	    do_append = 0;
            goto ret;
        }
    } else {
 retry_src_rstat:
        if (mc_stat (src_path, &sb)){
	    return_status = file_error
	        (" Cannot stat source file \"%s\" \n %s ", src_path);
	    if (return_status == FILE_RETRY)
	        goto retry_src_rstat;
	    do_append = 0;
            goto ret;
        }
    }
    src_mode = sb.st_mode;
#ifndef _OS_NT
    src_uid = sb.st_uid;
    src_gid = sb.st_gid;
    utb.actime = sb.st_atime;
    utb.modtime = sb.st_mtime;
#endif
    file_size = sb.st_size;
    
    /* If the target is a symlink and we do not want to append, we should
       unlink it first... */
       
    if (dst_exists && !do_append && S_ISLNK (sb2.st_mode))
        mc_unlink (dst_path);

    /* Create the new regular file with small permissions initially,
       to not create a security hole.  */

    if (!do_remote_copy) {
 retry_dst_open:
        if ((do_append && 
            (dest_desc = mc_open (dst_path, O_WRONLY | O_APPEND)) < 0) ||
            (!do_append &&
            (dest_desc = mc_open (dst_path, O_WRONLY | O_CREAT | O_TRUNC, 0600)) < 0)) {
	    return_status = file_error
	        (" Cannot create target file \"%s\" \n %s ", dst_path);
	    if (return_status == FILE_RETRY)
	        goto retry_dst_open;
	    do_append = 0;
	    goto ret2;
        }
    }
    appending = do_append;
    do_append = 0;

    if (!do_remote_copy) {
 retry_dst_fstat:
        /* Find out the optimal buffer size.  */
        if (mc_fstat (dest_desc, &sb)){
	    return_status = file_error
	        (" Cannot fstat target file \"%s\" \n %s ", dst_path);
	    if (return_status == FILE_RETRY)
	        goto retry_dst_fstat;
	    goto ret2;
        }
        buf_size = 8*1024;

        buf = (char *) xmalloc (buf_size, "copy_file_file");
    }

    return_status = show_file_progress (0, file_size);
    mc_refresh ();
    if (return_status != FILE_CONT)
	goto ret3;

    if (!do_remote_copy)
        for (;;){
    retry_src_read:
	    n_read = mc_read (source_desc, buf, buf_size);
	    if (n_read < 0){
	        return_status = file_error
		    (" Cannot read source file \"%s\" \n %s ", src_path);
	        if (return_status == FILE_RETRY)
		    goto retry_src_read;
	        goto ret3;
	    }
	    if (n_read == 0)
	        break;

	    n_read_total += n_read;

    retry_dst_write:
	    n_written = mc_write (dest_desc, buf, n_read);
	    if (n_written < n_read){
	        return_status = file_error
		    (" Cannot write target file \"%s\" \n %s ", dst_path);
	        if (return_status == FILE_RETRY)
		    goto retry_dst_write;
	        goto ret3;
	    }
	    return_status = show_file_progress (n_read_total, file_size);
	    mc_refresh ();
	    if (return_status != FILE_CONT)
	        goto ret3;
        }
    else {
        int i, size;
	struct timeval tv_last_update;
	struct timeval tv_current;
	
	gettimeofday (&tv_last_update, (struct timezone *) NULL);
        for (i = 1; i;) {
            switch (size = mc_ctl (source_desc, MCCTL_REMOTECOPYCHUNK, 8192)) {
                case MCERR_TARGETOPEN:
                    break;
                case MCERR_READ:
                    break;
                case MCERR_WRITE:
                    break;
	        case MCERR_DATA_ON_STDIN:
		    break;
                case MCERR_FINISH:
                    i = 0;
                    break;
            }
	    if (size == MCERR_TARGETOPEN){
		message_1s (1, " Error ", " Can't open target file ");
		goto ret3;
	    }
	    if (size == MCERR_READ)
		goto ret3;

	    if (i && size != MCERR_DATA_ON_STDIN){
		n_read_total += size;

		/* Windows NT ftp servers report that files have no
		 * permissions: -------, so if we happen to have actually
		 * read something, we should fix the permissions.
		 */
		if (!(src_mode &
		      ((S_IRUSR|S_IWUSR|S_IXUSR)    /* user */
		       |(S_IXOTH|S_IWOTH|S_IROTH)  /* other */
		       |(S_IXGRP|S_IWGRP|S_IRGRP)))) /* group */
		    src_mode = S_IRUSR|S_IWUSR|S_IROTH|S_IRGRP;
	    }

	    /* Update rotating dash in case it's been a while since we updated it */
	    /* Hardcoded to 2000 miliseconds */
	    gettimeofday (&tv_current, NULL);
	    if (DIF_TIME (tv_last_update, tv_current) > 2000){
		rotate_dash ();
		tv_last_update = tv_current;
	    }
	    return_status = show_file_progress (n_read_total, file_size);
	    mc_refresh ();
	    if (return_status != FILE_CONT)
	        goto ret3;
        }
    }

 retry_dst_chmod:
    if (!appending && mc_chmod (dst_path, src_mode)){
	temp_status = file_error
	    (" Cannot chmod target file \"%s\" \n %s ", dst_path);
	if (temp_status == FILE_RETRY)
	    goto retry_dst_chmod;
	return_status = temp_status;
    }
#ifndef _OS_NT 
    if (!appending && preserve_uidgid) {
     retry_dst_chown:
        if (mc_chown (dst_path, src_uid, src_gid)){
	    temp_status = file_error
	        (" Cannot chown target file \"%s\" \n %s ", dst_path);
	    if (temp_status == FILE_RETRY)
	        goto retry_dst_chown;
	    return_status = temp_status;
        }
    }
#endif
 ret:
    if (buf)
        free (buf);
 ret0:        
 retry_dst_close:
    if (!do_remote_copy) {
        if (mc_close (dest_desc) < 0){
	    temp_status = file_error
	        (" Cannot close target file \"%s\" \n %s ", dst_path);
	    if (temp_status == FILE_RETRY)
	        goto retry_dst_close;
	    return_status = temp_status;
        }
    }
    if (!appending)
        mc_utime (dst_path, &utb);

 ret2:
 retry_src_close:
    if (mc_close (source_desc) < 0){
	temp_status = file_error
	    (" Cannot close source file \"%s\" \n %s ", src_path);
	if (temp_status == FILE_RETRY)
	    goto retry_src_close;
	if (temp_status == FILE_ABORT)
	    return_status = temp_status;
    }

    return return_status;

 ret3:
    /* Remove short file */
    mc_unlink (dst_path);
    if (do_remote_copy) {
        mc_ctl (source_desc, MCCTL_FINISHREMOTE, -1);
        goto ret0;
    }
    goto ret;
}

/*
 * I think these copy_*_* functions should have a return type.
 * anyway, this function *must* have two directories as arguments.
 */
/* FIXME: This function needs to check the return values of the
   function calls */
int copy_dir_dir (char *s, char *d, int toplevel, int move_over)
{
    struct dirent *next;
    struct stat   buf, cbuf;
    DIR    *reading;
    char   *path, *mdpath, *dest_file, *dest_dir;
    int    return_status = FILE_CONT;

    /* First get the mode of the source dir */
 retry_src_stat:
    if (mc_lstat (s, &cbuf)){
	return_status = file_error (" Cannot stat source directory \"%s\" \n %s ", s);
	if (return_status == FILE_RETRY)
	    goto retry_src_stat;
	return return_status;
    } 

/* FIXME: In this step we should do something
   in case the destination already exist */    
    /* Check the hardlinks */
    if (cbuf.st_nlink > 1 && check_hardlinks (s, d, &cbuf) == 1) {
    	/* We have made a hardlink - no more processing is necessary */
    	return return_status;
    }

/* Can someone answer: can cbuf be a link? How? */    
    if (S_ISLNK (cbuf.st_mode) && (!toplevel && dive_into_subdirs)){
        /* We want to replicate the symlink, not to copy the directory :-) */
        int result;
        
        if (!mc_stat (d, &buf) && toplevel && dive_into_subdirs){
            dest_dir = copy_strings (d, PATH_SEP_STR, x_basename (s), 0);
    	    result = copy_file_file (s, dest_dir, 1, 1);
    	    free (dest_dir);
    	    return result;
        } else
    	    return copy_file_file (s, d, 0, 1);
    }
    if (!S_ISDIR (cbuf.st_mode)){
	return_status = file_error (" Source directory \"%s\" is not a directory ", s);
	if (return_status == FILE_RETRY)
	    goto retry_src_stat;
	return return_status;
    }

    /* Now, check if the dest dir exists, if not, create it. */
    if (mc_stat (d, &buf)){
    	/* Here the dir doesn't exist : make it !*/

    	if (move_over) {
            if (mc_rename (s, d) == 0)
		return FILE_CONT;
	}
	dest_dir = copy_strings (d, 0);
    } else {
        /*
         * If the destination directory exists, we want to copy the whole
         * directory, but we only want this to happen once.
	 *
	 * Escape sequences added to the * to avoid compiler warnings.
         * so, say /bla exists, if we copy /tmp/\* to /bla, we get /bla/tmp/\*
         * or ( /bla doesn't exist )       /tmp/\* to /bla     ->  /bla/\*
         */
/* Again, I'm getting curious. Is not d already what we wanted, incl.
   masked source basename? Is not this just a relict of the past versions? 
   I'm afraid this will lead into a two level deep dive :( */         
        if (toplevel && dive_into_subdirs){
            dest_dir = copy_strings (d, PATH_SEP_STR, x_basename (s), 0);
	} else {
	    dest_dir = copy_strings (d, 0);
	    goto dont_mkdir;
	}
    }
 retry_dst_mkdir:
    if (my_mkdir (dest_dir, cbuf.st_mode | S_IRWXU)){
	return_status = file_error (" Cannot create target directory \"%s\" \n %s ", dest_dir);
	if (return_status == FILE_RETRY)
	    goto retry_dst_mkdir;
	return return_status;
    }

 dont_mkdir:
    /* open the source dir for reading */
    reading = mc_opendir (s);

    if (!reading){
	free (dest_dir);
	return return_status;
    }
    
    while ((next = mc_readdir (reading)) && return_status != FILE_ABORT){
        /*
         * Now, we don't want '.' and '..' to be created / copied at any time 
         */
        if (!strcmp (next->d_name, "."))
            continue;
        if (!strcmp (next->d_name, ".."))
           continue;

        /* get the filename and add it to the src directory */
        path = copy_strings (s, PATH_SEP_STR, next->d_name, 0);
        /* lstat is needed here, because we want even links to directories
           be replicated as links */
        mc_lstat (path, &buf);

        if (S_ISDIR (buf.st_mode)){
            mdpath = copy_strings (dest_dir, PATH_SEP_STR, next->d_name, 0);
            /*
             * From here, we just intend to recursively copy subdirs, not
             * the double functionality of copying different when the target
             * dir already exists. So, we give the recursive call the flag 0
             * meaning no toplevel.
             */
            return_status = copy_dir_dir (path, mdpath, 0, 0);
	    free (mdpath);
	} else {
	    dest_file = copy_strings (dest_dir, PATH_SEP_STR, x_basename (path), 0);
            return_status = copy_file_file (path, dest_file, 0, 1);
	    free (dest_file);
	}
        free (path);
    }
    mc_closedir (reading);
    mc_chmod (dest_dir, cbuf.st_mode);
    free (dest_dir);
    return return_status;
}

/* }}} */

/* {{{ Move routines */

static int move_file_file (char *s, char *d)
{
    struct stat src_stats, dst_stats;
    int return_status = FILE_CONT;

    if (show_source (s) == FILE_ABORT
	|| show_target (d) == FILE_ABORT)
	return FILE_ABORT;

    mc_refresh ();

 retry_src_lstat:
    if (mc_lstat (s, &src_stats) != 0){
	/* Source doesn't exist */
	return_status = file_error (" Cannot stat file \"%s\" \n %s ", s);
	if (return_status == FILE_RETRY)
	    goto retry_src_lstat;
	return return_status;
    }

    if (mc_lstat (d, &dst_stats) == 0){
	/* Destination already exists */
	if (src_stats.st_dev == dst_stats.st_dev
	    && src_stats.st_ino == dst_stats.st_ino){
	    int msize = COLS - 36;

	    if (msize < 0)
		msize = 40;
	    msize /= 2;
	    
	    message_3s (1, " Error ", " `%s' and `%s' are the same file ",
		     name_trunc (s, msize), name_trunc (d, msize) );
	    do_refresh ();
	    return FILE_SKIP;
	}
	if (S_ISDIR (dst_stats.st_mode)){
	    message_2s (1, " Error ", " Cannot overwrite directory `%s' ", d);
	    do_refresh ();
	    return FILE_SKIP;
	}

	if (confirm_overwrite){
	    return_status = query_replace (d, &src_stats, &dst_stats);
	    if (return_status != FILE_CONT)
		return return_status;
	}
	/* Ok to overwrite */
    }

 retry_rename:
    if (!do_append) {
        if (mc_rename (s, d) == 0)
	    return FILE_CONT;
    }
#if 0
/* Comparison to EXDEV seems not to work in nfs if you're moving from
   one nfs to the same, but on the server it is on two different
   filesystems. Then nfs returns EIO instead of EXDEV. 
   Hope it will not hurt if we always in case of error try to copy/delete. */
     else
    	errno = EXDEV; /* Hack to copy (append) the file and then delete it */

    if (errno != EXDEV){
	return_status = files_error (" Cannot move file \"%s\" to \"%s\" \n %s ", s, d);
	if (return_status == FILE_RETRY)
	    goto retry_rename;
	return return_status;
    }
#endif    

    /* Failed because filesystem boundary -> copy the file instead */
    if ((return_status = copy_file_file (s, d, 0, 0)) != FILE_CONT)
	return return_status;
    if ((return_status = show_source (NULL)) != FILE_CONT
	|| (return_status = show_file_progress (0, 0)) != FILE_CONT)
	return return_status;

    mc_refresh ();

 retry_src_remove:
    if (mc_unlink (s)){
	return_status = file_error (" Cannot remove file \"%s\" \n %s ", s);
	if (return_status == FILE_RETRY)
	    goto retry_src_remove;
	return return_status;
    }

    return FILE_CONT;
}

int move_dir_dir (char *s, char *d)
{
    struct stat sbuf, dbuf, destbuf;
    char *destdir;
    int return_status;
    int move_over = 0;

    if (show_source (s) == FILE_ABORT
	|| show_target (d) == FILE_ABORT)
	return FILE_ABORT;

    mc_refresh ();

    mc_stat (s, &sbuf);
    if (mc_stat (d, &dbuf))
	destdir = copy_strings (d, 0);
    else if (!dive_into_subdirs) {
	destdir = copy_strings (d, 0);
	move_over = 1;
    } else
/* And I'm not sure about this as well - shouldn't we strdup always? */    
	destdir = copy_strings (d, PATH_SEP_STR, x_basename (s), 0);

    /* Check if the user inputted an existing dir */
 retry_dst_stat:
    if (!mc_stat (destdir, &destbuf)){
	if (move_over) {
	    if ((return_status = copy_dir_dir (s, destdir, 0, 1)) != FILE_CONT)
		goto ret;
	    goto oktoret;
	} else {
	    if (S_ISDIR (destbuf.st_mode))
	        return_status = file_error (" Cannot overwrite directory \"%s\" ", destdir);
	    else
	        return_status = file_error (" Cannot overwrite file \"%s\" ", destdir);
	    if (return_status == FILE_RETRY)
	        goto retry_dst_stat;
	}
        free (destdir);
        return return_status;
    }

 retry_rename:
    if (mc_rename (s, destdir) == 0){
	return_status = FILE_CONT;
	goto ret;
    }

    if (errno != EXDEV){
	return_status = files_error (" Cannot move directory \"%s\" to \"%s\" \n %s ", s, d);
	if (return_status == FILE_RETRY)
	    goto retry_rename;
	goto ret;
    }

    /* Failed because of filesystem boundary -> copy dir instead */
    if ((return_status = copy_dir_dir (s, destdir, 0, 0)) != FILE_CONT)
	goto ret;
oktoret:
    if ((return_status = show_source (NULL)) != FILE_CONT
	|| (return_status = show_file_progress (0, 0)) != FILE_CONT)
	goto ret;

    mc_refresh ();

    return_status = recursive_erase (s);

 ret:
    free (destdir);
    return return_status;
}

/* }}} */

/* {{{ Erase routines */

static int erase_file (char *s)
{
    int return_status;

    if (show_deleting (s) == FILE_ABORT)
	return FILE_ABORT;

    mc_refresh ();

 retry_unlink:
    if (mc_unlink (s)){
	return_status = file_error (" Cannot delete file \"%s\" \n %s ", s);
	if (return_status == FILE_RETRY)
	    goto retry_unlink;
	return return_status;
    }
    return FILE_CONT;
}

static int recursive_erase (char *s)
{
    struct dirent *next;
    struct stat	buf;
    DIR    *reading;
    char   *path;
    int    return_status = FILE_CONT;

    if (!strcmp (s, ".."))
	return 1;
    
    reading = mc_opendir (s);
    
    if (!reading)
	return 1;

    while ((next = mc_readdir (reading)) && return_status == FILE_CONT){
	if (!strcmp (next->d_name, "."))
	    continue;
   	if (!strcmp (next->d_name, ".."))
	    continue;
	path = copy_strings (s, PATH_SEP_STR, next->d_name, 0);
   	if (mc_lstat (path, &buf)){
	    free (path);
	    return 1;
	} 
	if (S_ISDIR (buf.st_mode))
	    return_status = (recursive_erase (path) != FILE_CONT);
	else
	    return_status = erase_file (path);
	free (path);
    }
    mc_closedir (reading);
    if (return_status != FILE_CONT)
	return return_status;
    if (show_deleting (s) == FILE_ABORT)
	return FILE_ABORT;
    mc_refresh ();
 retry_rmdir:
    if (my_rmdir (s)){
	return_status = file_error (" Cannot remove directory \"%s\" \n %s ", s);
	if (return_status == FILE_RETRY)
	    goto retry_rmdir;
	return return_status;
    }
    return FILE_CONT;
}

int erase_dir (char *s)
{
    int error;

    if (strcmp (s, "..") == 0)
	return FILE_SKIP;

    if (strcmp (s, ".") == 0)
	return FILE_SKIP;

    if (show_deleting (s) == FILE_ABORT)
	return FILE_ABORT;
    mc_refresh ();

 retry_rmdir:
    error = my_rmdir (s);
    if (error && (errno == ENOTEMPTY || errno == EEXIST)){
	error = query_recursive (s);
	if (error == FILE_CONT)
	    return recursive_erase (s);
	else
	    return error;
    }
    if (error){
	error = file_error (" Cannot remove directory \"%s\" \n %s ", s);
	if (error == FILE_RETRY)
	    goto retry_rmdir;
	return error;
    }
    return FILE_CONT;
}

/* }}} */

/* {{{ Panel operate routines */

/* Returns currently selected file or the first marked file if there is one */
static char *get_file (struct stat *stat_buf)
{
    int i;
    WPanel *panel;
    
    if (get_current_type () == view_tree){
	WTree *tree = (WTree *)get_panel_widget (get_current_index ());
	
	mc_stat (tree->selected_ptr->name, stat_buf);
	return tree->selected_ptr->name;
    } 

    panel = cpanel;
    
    if (panel->marked){
	for (i = 0; i < panel->count; i++)
	    if (panel->dir.list [i].f.marked){
		*stat_buf = panel->dir.list [i].buf;
		return panel->dir.list [i].fname;
	    }
    } else {
	*stat_buf = panel->dir.list [panel->selected].buf;
	return panel->dir.list [panel->selected].fname;
    }
    fprintf (stderr, " Internal error: get_file \n");
    mi_getch ();
    return "";
}

static int is_wildcarded (char *p)
{
    for (; *p; p++) {
        if (*p == '*')
            return 1;
        else if (*p == '\\' && p [1] >= '1' && p [1] <= '9')
            return 1;
    }
    return 0;
}

char *file_mask_dialog (char *header, char *text, char *def_text, int only_one, int *do_background)
{
#define FMDY 13
    int source_easy_patterns = easy_patterns;
    char *source_mask, *orig_mask, *dest_dir;
    const char *error;
    struct stat buf;
    int notroot, val;
    
    QuickDialog Quick_input;
    QuickWidget quick_widgets [] = {
/* preserve UID/GID must be the first one */    
    { quick_checkbox, 3, 64, 8, FMDY, "preserve UIDs/GIDs", 'U', 0, 0,
      &preserve_uidgid, 0, XV_WLAY_BELOWCLOSE, "preserve" },
    { quick_checkbox, 42, 64, 8, FMDY, "Stable Symlinks", 'S', 0, 0,
      &stable_symlinks, 0, XV_WLAY_BELOWCLOSE, "stab-sym" },
    { quick_checkbox, 23, 64, 7, FMDY, "Dive into subdir if exists", 'D', 0, 0, 
      &dive_into_subdirs, 0, XV_WLAY_RIGHTOF, "dive" },
    { quick_checkbox, 3, 64, 7, FMDY, "follow sYmlinks", 'y', 0, 0, 
      &op_follow_symlinks, 0, XV_WLAY_BELOWOF, "follow" },
#ifdef HAVE_XVIEW
#define FMDI1 7
#define FMDI2 4
#define FMDC  6
    { quick_input, 3, 64, 6, FMDY, 0, 0, 58, 0, 
      0, 0, XV_WLAY_BELOWCLOSE, "" },
#endif
    { quick_label, 3, 64, 5, FMDY, "to:", 0, 0, 0, 0, 0, XV_WLAY_BELOWOF,"to"},
    { quick_checkbox, 37, 64, 4, FMDY, "using shell Patterns", 'p', 0, 0, 
      0/* &source_easy_patterns */, 0, XV_WLAY_BELOWCLOSE, "using-shell" },
    { quick_input, 3, 64, 3, FMDY, 0 /* default text */, 0, 58, 
      0, 0, 0, XV_WLAY_BELOWCLOSE, "input-def" },
#ifndef HAVE_XVIEW      
#define FMDI1 6
#define FMDI2 7
#define FMDC 5
    { quick_input, 3, 64, 6, FMDY, 0, 0, 58, 0, 
      0, 0, XV_WLAY_BELOWCLOSE, "input2" },
#endif      
#define FMDI0 8	  
    { quick_label, 2, 64, 2, FMDY, 0, 0, 0, 0, 0, 0, XV_WLAY_DONTCARE, "ql" },
#define SKIP 2
    { quick_button, 42, 64, 9, FMDY, "[ Cancel ]", 'C', 2, B_CANCEL, 0, 0, 0,
	  "cancel" },
#ifdef WITH_BACKGROUND
#undef SKIP
#define SKIP 3
    { quick_button, 25, 64, 9, FMDY, "[ Background ]", 'B', 2, B_USER, 0, 0, 0, "back" },
#endif
    { quick_button, 15, 64, 9, FMDY, "[ Ok ]", 'O', 2, B_ENTER, 0, 0, 0, "ok" },
    { 0 } };

    stable_symlinks = 0;
    quick_widgets [FMDC].result = &source_easy_patterns;
    quick_widgets [FMDI1].text = easy_patterns ? "*" : "^\\(.*\\)$";
    notroot = (geteuid ()) ? 1 : 0;
    Quick_input.xlen  = 64;
    Quick_input.xpos  = -1;
    Quick_input.title = header;
    Quick_input.help  = "[Mask Copy/Rename]";
    if (notroot)
	Quick_input.class = "quick_file_mask";
    else
	Quick_input.class = "quick_file_mask_root";
    quick_widgets [FMDI0].text = text;
    quick_widgets [FMDI2].text = def_text;
    quick_widgets [FMDI2].str_result = &dest_dir;
    quick_widgets [FMDI1].str_result = &source_mask;
    Quick_input.ylen  = FMDY;
    Quick_input.widgets = quick_widgets + notroot;
    if (notroot) {
        int i;
        
        preserve_uidgid = 0;
	quick_widgets [1].tkname = "uidgid";
    }

    *do_background = 0;
ask_file_mask:

    if ((val = quick_dialog_skip (&Quick_input, SKIP)) == B_CANCEL)
	return 0;

    orig_mask = source_mask;
    if (!dest_dir || !*dest_dir) {
	if (source_mask)
	    free (source_mask);
	return dest_dir;
    }
    if (source_easy_patterns) {
	source_easy_patterns = easy_patterns;
	easy_patterns = 1;
	source_mask = convert_pattern (source_mask, match_file, 1);
	source_easy_patterns = easy_patterns;
    }

    error = re_compile_pattern (source_mask, strlen (source_mask), &rx);
    if (error) {
	message_3s (1, " Error ", "Invalid source pattern \"%s\" \n %s ",
		 orig_mask, error);
	if (orig_mask)
	    free (orig_mask);
	goto ask_file_mask;
    }
    if (orig_mask)
	free (orig_mask);
    dest_mask = strrchr (dest_dir, PATH_SEP);
    if (dest_mask == NULL)
	dest_mask = dest_dir;
    else
	dest_mask++;
    orig_mask = dest_mask;
    if (!*dest_mask || (!dive_into_subdirs && !is_wildcarded (dest_mask)) ||
	(dive_into_subdirs && ((!only_one && !is_wildcarded (dest_mask)) ||
			       (only_one && !mc_stat (dest_dir, &buf) && S_ISDIR (buf.st_mode)))))
	dest_mask = strdup ("*");
    else {
	dest_mask = strdup (dest_mask);
	*orig_mask = 0;
    }
    if (!*dest_dir) {
	free (dest_dir);
	dest_dir = strdup ("./");
    }
    if (val == B_USER)
	*do_background = 1;
    return dest_dir;
}

/* Returns 1 if did change the directory structure,
   Returns 0 if user aborted */
int panel_operate (int operation, char *thedefault)
{
#ifdef WITH_FULL_PATHS
    char *source_with_path = NULL;
#else
#   define source_with_path source
#endif
    char *source = NULL;
    char *dest = NULL;
    char *temp = NULL;
    int only_one = (get_current_type () == view_tree) || (cpanel->marked <= 1);
    struct stat src_stat, dst_stat;
    int i, value;
    long marked, total;
    long count = 0, bytes = 0;
    int  dst_result;
    int  do_bg;			/* do background operation? */

    do_bg = 0;
    rx.buffer = NULL;
    if (linklist)
    	free_linklist ();
    if (get_current_type () == view_listing)
	if (!cpanel->marked && !strcmp (selection (cpanel)->fname, "..")){
	    message (1, " Error ", " Can't operate on \"..\"! ");
	    return 0;
	}
    
    if (operation < OP_COPY || operation > OP_DELETE)
	return 0;
    
    /* Generate confirmation prompt */
    
    if (!only_one){
	sprintf (cmd_buf, "%s%d %s%s ", op_names [operation], cpanel->marked,
		 (cpanel->marked == cpanel->dirs_marked) ? "directories" :
		 (cpanel->dirs_marked) ? "files/directories" : "files",
		 (operation == OP_DELETE) ? "?" : " with source mask:");
    } else {
	source = get_file (&src_stat);
	sprintf (cmd_buf,"%s%s \"%s\"%s ", op_names [operation],
	         S_ISDIR (src_stat.st_mode) ? "directory" : "file",
		 name_trunc (source, 28),
		 (operation == OP_DELETE) ? "?" : " with source mask:");
    }
    
    /* Show confirmation dialog */
    if (operation == OP_DELETE && confirm_delete){
	i = query_dialog (op_names [operation], cmd_buf,
			  3, 2, " Yes ", " No ");
	if (i != 0)
	    return 0;
    } else if (operation != OP_DELETE) {
	char *dest_dir;
	
	if (thedefault != NULL)
	    dest_dir = thedefault;
	else if (get_other_type () == view_listing)
	    dest_dir = opanel->cwd;
	else
	    dest_dir = cpanel->cwd;

	rx.buffer = (char *) xmalloc (MC_MAXPATHLEN, "mask copying");
	rx.allocated = MC_MAXPATHLEN;
	rx.translate = 0;
	dest = file_mask_dialog (op_names [operation],
				    cmd_buf, dest_dir, only_one, &do_bg);
	if (!dest) {
	    free (rx.buffer);
	    return 0;
	}
	if (!*dest){
	    free (rx.buffer);
	    free (dest);
	    return 0;
	}
    }

#ifdef WITH_BACKGROUND
    /* Did the user select to do a background operation? */
    if (do_bg){
	int v;
	
	v = do_background ();
	if (v == -1){
	    message (1, " Error ", " Sorry, I could not put the job in background ");
	}

	/* If we are the parent */
	if (v == 1){
	    vfs_force_expire (cpanel->cwd);
	    vfs_force_expire (dest);
	    return 0;
	}
    }
#endif
    /* Initialize things */
    create_op_win (operation);
    ftpfs_hint_reread (0);
    
    /* Now, let's do the job */
    /* This code is only called by the tree and panel code */
    if (only_one){
	/* One file: FIXME mc_chdir will take user out of any vfs */
	if (operation != OP_COPY && get_current_type () == view_tree)
	    mc_chdir (PATH_SEP_STR);
	
	/* The source and src_stat variables have been initialized before */
#ifdef WITH_FULL_PATHS
	source_with_path = copy_strings (cpanel->cwd, PATH_SEP_STR, source, 0);
#endif
	
	if (operation == OP_DELETE){
	    /* Delete operation */
	    if (S_ISDIR (src_stat.st_mode))
		value = erase_dir (source_with_path);
	    else
		value = erase_file (source_with_path);
	} else {
	    /* Copy or move operation */
	    temp = transform_source (source_with_path);
	    if (temp == NULL) {
		value = transform_error;
	    } else {
		temp = get_full_name (dest, temp);
		free (dest);
		dest = temp;
		temp = 0;
	        switch (operation){
	        case OP_COPY:
		    if (S_ISDIR (src_stat.st_mode))
		        value = copy_dir_dir (source_with_path, dest, 1, 0);
		    else
		        value = copy_file_file (source_with_path, dest, 1, 1);
		    break;
	        case OP_MOVE:
		    if (S_ISDIR (src_stat.st_mode))
		        value = move_dir_dir (source_with_path, dest);
		    else
		        value = move_file_file (source_with_path, dest);
		    break;
	        default:
		    value = FILE_CONT;
		    message_1s (1, " Internal failure ", " Unknown file operation ");
	        }
	    }
	} /* Copy or move operation */

	if (value == FILE_CONT)
	    unmark_files (cpanel);

    } else {
	/* Many files */

	if (operation != OP_DELETE){
	    /* Check destination for copy or move operation */
	retry_many_dst_stat:
	    dst_result = mc_stat (dest, &dst_stat);
	    if (dst_result == 0 && !S_ISDIR (dst_stat.st_mode)){
		if (file_error (" Destination \"%s\" must be a directory ", dest) == FILE_RETRY)
		    goto retry_many_dst_stat;
		goto clean_up;
	    }
	}

	/* Initialize variables for progress bars */
	marked = cpanel->marked;
	total = cpanel->total;

	/* Loop for every file */
	for (i = 0; i < cpanel->count; i++){
	    if (!cpanel->dir.list [i].f.marked)
		continue;	/* Skip the unmarked ones */
	    source = cpanel->dir.list [i].fname;
	    src_stat = cpanel->dir.list [i].buf;	/* Inefficient, should we use pointers? */
#ifdef WITH_FULL_PATHS
	    if (source_with_path)
	    	free (source_with_path);
	    source_with_path = copy_strings (cpanel->cwd, PATH_SEP_STR, source, 0);
#endif
	    if (operation == OP_DELETE){
		/* Delete operation */
		if (S_ISDIR (src_stat.st_mode))
		    value = erase_dir (source_with_path);
		else
		    value = erase_file (source_with_path);
	    } else {
		/* Copy or move operation */
		if (temp)
		    free (temp);
		temp = transform_source (source_with_path);
		if (temp == NULL) {
		    value = transform_error;
		} else {
		    temp = get_full_name (dest, temp);
		    switch (operation){
		    case OP_COPY:
		    	if (S_ISDIR (src_stat.st_mode))
			    value = copy_dir_dir (source_with_path, temp, 1, 0);
		    	else
			    value = copy_file_file (source_with_path, temp, 1, 1);
		    	break;
		    case OP_MOVE:
		    	if (S_ISDIR (src_stat.st_mode))
			    value = move_dir_dir (source_with_path, temp);
		    	else
		            value = move_file_file (source_with_path, temp);
		        break;
		    default:
		    	message_1s (1, " Internal failure ",
			         " Unknown file operation ");
		    	goto clean_up;
		    }
		}
	    } /* Copy or move operation */

	    if (value == FILE_ABORT)
		goto clean_up;
	    if (value == FILE_CONT){
		file_mark (cpanel, i, 0);
		cpanel->marked --;
		if (S_ISDIR (src_stat.st_mode))
		    cpanel->dirs_marked --;
		cpanel->total -= src_stat.st_size;
	    }
	    count ++;
	    if (show_count_progress (count, marked) == FILE_ABORT)
		goto clean_up;
	    bytes += src_stat.st_size;
	    if (verbose &&
	        show_bytes_progress (bytes, total) == FILE_ABORT)
		goto clean_up;
	    if (operation != OP_DELETE && verbose
		&& show_file_progress (0, 0) == FILE_ABORT)
		goto clean_up;
	    mc_refresh ();
	} /* Loop for every file */
    } /* Many files */

 clean_up:
    /* Clean up */
    destroy_op_win ();
    ftpfs_hint_reread (1);
    if (linklist)
    	free_linklist ();
#if WITH_FULL_PATHS
    if (source_with_path)
    	free (source_with_path);
#endif
    if (dest)
	free (dest);
    if (temp)
	free (temp);
    if (rx.buffer) {
	free (rx.buffer);
	rx.buffer = NULL;
    }
    if (dest_mask) {
	free (dest_mask);
	dest_mask = NULL;
    }

#ifdef WITH_BACKGROUND
    /* Let our parent know we are saying bye bye */
    if (we_are_background){
	vfs_shut ();
	tell_parent (MSG_CHILD_EXITING);
	exit (1);
    } 
#endif
    return 1;
}

/* }}} */

/* {{{ Query/status report routines */

static int
real_do_file_error (enum OperationMode mode, char *error)
{
    int result;
    char *msg;

    msg = mode == Foreground ? " Error " : " Background process error ";
    result = query_dialog (msg, error, 3, 3, " Skip ", " Retry ", " Abort ");

    switch (result){
    case 0:
	do_refresh ();
	return FILE_SKIP;
    case 1:
	do_refresh ();
	return FILE_RETRY;
    case 2:
    default:
	return FILE_ABORT;
    }
}

/* Report error with one file */
int
file_error (char *format, char *file)
{
    sprintf (cmd_buf, format,
	     name_trunc (file, 30), unix_error_string (errno));
    return do_file_error (cmd_buf);
}

/* Report error with two files */
int
files_error (char *format, char *file1, char *file2)
{
    sprintf (cmd_buf, format, name_trunc (file1, 15),
	     name_trunc (file2, 15), unix_error_string (errno));
    return do_file_error (cmd_buf);
}

static char *format = "Target file \"%s\" already exists!";
static int
replace_callback (struct Dlg_head *h, int Id, int Msg)
{
#ifndef HAVE_X

    switch (Msg){
    case DLG_DRAW:
	dialog_repaint (h, ERROR_COLOR, ERROR_COLOR);
	break;
    }
#endif
    return 0;
}

static void
init_replace (enum OperationMode mode)
{
    char buffer [128];
    
    replace_colors [0] = ERROR_COLOR;
    replace_colors [1] = REVERSE_COLOR;
    replace_colors [2] = ERROR_COLOR;
    replace_colors [3] = REVERSE_COLOR;
    
    replace_dlg = create_dlg (0, 0, 14, 60, replace_colors, replace_callback,
			      "[Replace]", "replace", DLG_CENTER);

    
    x_set_dialog_title (replace_dlg,
        mode == Foreground ? " File exists " : " Background process: File exists ");

#ifdef HAVE_X
#define X_TRUNC 128
#else
#define X_TRUNC 52
#endif
    sprintf (buffer, format, name_trunc (replace_filename, X_TRUNC - strlen (format)));
    add_widgetl (replace_dlg, label_new (3, 5, buffer, NULL), XV_WLAY_CENTERROW);
    
    add_widgetl (replace_dlg,
		button_new (BY + 1, 46, REPLACE_ABORT, "[ Abort ]", 'a', 2, 0, 0, NULL),
		XV_WLAY_CENTERROW);
    
    tk_new_frame (replace_dlg, "a.");
    add_widgetl (replace_dlg,
		button_new (BY - 1, 47, REPLACE_NEVER, "[ nonE ]", 'e', 5, 0, 0, NULL),
		XV_WLAY_RIGHTOF);
    add_widgetl (replace_dlg,
		button_new (BY - 1, 36, REPLACE_UPDATE, "[ Update ]", 'u', 2, 0, 0, NULL),
		XV_WLAY_RIGHTOF);
    add_widgetl (replace_dlg,
		button_new (BY - 1, 28, REPLACE_ALWAYS, "[ alL ]", 'l', 4, 0, 0, NULL),
		XV_WLAY_RIGHTOF);

    add_widgetl (replace_dlg, label_new (BY-1, 5, "Overwrite all targets?", NULL),
	        XV_WLAY_CENTERROW);        

    /* "this target..." widgets */
    tk_new_frame (replace_dlg, "p.");
    if (!S_ISDIR (d_stat->st_mode))
        add_widgetl (replace_dlg,
		    button_new (BY - 2, 43, REPLACE_APPEND, "[ apPend ]", 'p', 2, 0, 0, NULL),
		    XV_WLAY_RIGHTOF);
    add_widgetl (replace_dlg,
		button_new (BY - 2, 36, REPLACE_NO, "[ No ]", 'n', 2, 0, 0, NULL),
		XV_WLAY_RIGHTOF);
    add_widgetl (replace_dlg,
		button_new (BY - 2, 28, REPLACE_YES, "[ Yes ]", 'y', 2, 0, 0, NULL), 
		XV_WLAY_RIGHTOF);
    add_widgetl (replace_dlg, label_new (BY-2,5, "Overwrite this target?", NULL),
		 XV_WLAY_CENTERROW);
    
    tk_new_frame (replace_dlg, "i.");
    sprintf (buffer, "Target date: %s, size %d",
	     file_date (d_stat->st_mtime), (int) d_stat->st_size);
    add_widgetl (replace_dlg, label_new (6, 5, buffer, NULL), XV_WLAY_CENTERROW);
    sprintf (buffer, "Source date: %s, size %d",
	     file_date (s_stat->st_mtime), (int) s_stat->st_size);
    add_widgetl (replace_dlg, label_new (5, 5, buffer, NULL), XV_WLAY_CENTERROW);
    tk_end_frame ();
}

static int
real_query_replace (enum OperationMode mode, char *destname, struct stat *_s_stat,
		    struct stat *_d_stat)
{
    if (replace_result < REPLACE_ALWAYS){
	replace_filename = destname;
	s_stat = _s_stat;
	d_stat = _d_stat;
	init_replace (mode);
	run_dlg (replace_dlg);
	replace_result = replace_dlg->ret_value;
	if (replace_result == B_CANCEL)
	    replace_result = REPLACE_ABORT;
	destroy_dlg (replace_dlg);
    }

    switch (replace_result){
    case REPLACE_UPDATE:
	do_refresh ();
	if (s_stat->st_mtime > d_stat->st_mtime)
	    return FILE_CONT;
	else
	    return FILE_SKIP;
	
    case REPLACE_APPEND:
        do_append = 1;
    case REPLACE_YES:
    case REPLACE_ALWAYS:
	do_refresh ();
	return FILE_CONT;
    case REPLACE_NO:
    case REPLACE_NEVER:
	do_refresh ();
	return FILE_SKIP;
    case REPLACE_ABORT:
    default:
	return FILE_ABORT;
    }
}

int
real_query_recursive (enum OperationMode mode, char *s)
{
    char *confirm, *text;

    if (recursive_result < RECURSIVE_ALWAYS){
	char *msg =
	    mode == Foreground ? "\n   Directory not empty.   \n   Delete it recursively? "
	                       : "\n   Background process: Directory not empty \n   Delete it recursively? ";
	text = copy_strings (" Delete: ", name_trunc (s, 30), " ", 0);

        if (know_what_am_i_doing){
	    query_set_sel (1);
	    recursive_result = query_dialog (text, msg, 3 | WITH_HOTKEYS, 5,
					     "y Yes ", "n No ", "l alL ", "e nonE ", "a Abort ");
	} else
	    recursive_result = query_dialog (text, msg, 3 | WITH_HOTKEYS, 5, 
					     "y Yes ", "n No ", "l alL ", "e nonE ", "a Abort ");
	if (recursive_result != RECURSIVE_ABORT)
	    do_refresh ();
	free (text);
	if (!know_what_am_i_doing && (recursive_result == RECURSIVE_YES
	    || recursive_result == RECURSIVE_ALWAYS)){
	    text = copy_strings (" Please type 'yes' to confirm recursive delete of ",
				 recursive_result == RECURSIVE_YES
				 ? name_trunc (s, 20) : "all the directories", " ", 0);
	    confirm = input_dialog (mode == Foreground ? " Recursive Delete "
				                       : " Background process: Recursive Delete ",
				    text, "no");
	    do_refresh ();
	    if (!confirm || strcmp (confirm, "yes"))
		recursive_result = RECURSIVE_NEVER;
	    free (confirm);
	    free (text);
	}
    }
    switch (recursive_result){
    case RECURSIVE_YES:
    case RECURSIVE_ALWAYS:
	return FILE_CONT;
    case RECURSIVE_NO:
    case RECURSIVE_NEVER:
	return FILE_SKIP;
    case RECURSIVE_ABORT:
    default:
	return FILE_ABORT;
    }
}

#ifdef WITH_BACKGROUND
int
do_file_error (char *str)
{
    return call_1s (real_do_file_error, str);
}

int
query_recursive (char *s)
{
    return call_1s (real_query_recursive, s);
}

int
query_replace (char *destname, struct stat *_s_stat, struct stat *_d_stat)
{
    if (we_are_background)
	return parent_call (real_query_replace, 3, strlen(destname), destname,
			    sizeof (struct stat), _s_stat, sizeof(struct stat), _d_stat);
    else
	return real_query_replace (Foreground, destname, _s_stat, _d_stat);
}

#else
do_file_error (char *str)
{
    return real_do_file_error (Foreground, str);
}

int
query_recursive (char *s)
{
    return real_query_recursive (Foreground, s);
}

int
query_replace (char *destname, struct stat *_s_stat, struct stat *_d_stat)
{
    return real_query_replace (Foreground, destname, _s_stat, _d_stat);
}

#endif

/*
  Cause emacs to enter folding mode for this file:
  Local variables:
  end:
*/
