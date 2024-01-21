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
extern void	set_curr_art(ARTICLE*, int);
extern void	set_curr_subj(SUBJECT*);
extern void	set_curr_group(void);
extern void	set_tree_stuff(ARTICLE*);
extern void	set_standard_message(void);
extern void	set_busy(int);
extern void	unset_busy(void);
extern void	change_interruptible(int);
extern int	global_cleanup(int, int);
extern void	stderr_popup(char*, long);
extern void	add_rescan_timeout(void);
extern void	remove_rescan_timeout(void);
extern void	check_if_rescan_due(void);
extern void	disconnect(int);
extern char	*do_update(void);
extern void	realize_fake(ARTICLE*, char**, int);

extern Boolean	cvt_string_to_pixmap(Display*,
				     XrmValue*, Cardinal*,
				     XrmValue*, XrmValue*,
				     XtPointer*);
extern void	destroy_pixmap(XtAppContext, XrmValue*, XtPointer,
			       XrmValue*, Cardinal*);

extern Widget	popup_notice(char*, char*, char*, char*, char*, long,
			     XtCallbackProc, XtPointer, XtGrabKind);
extern Widget	popup_dialogue(char*, char*, char*, char*, char*,
			       XtCallbackProc, XtPointer, XtGrabKind);
extern void	popup_colornotice(int);
extern void	popup_regexpnotice(int, const regex_t*);
extern void	popup_title_notice(char*, char*, int);
extern Widget	create_simple_menu(Widget, char*, int, XtCallbackProc, void*);
