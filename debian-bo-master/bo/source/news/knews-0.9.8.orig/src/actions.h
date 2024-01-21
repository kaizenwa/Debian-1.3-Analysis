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
extern void action_tree_up(Widget, XEvent*, String*, Cardinal*);
extern void action_tree_down(Widget, XEvent*, String*, Cardinal*);
extern void action_tree_left(Widget, XEvent*, String*, Cardinal*);
extern void action_tree_right(Widget, XEvent*, String*, Cardinal*);
extern void action_tree_down_right(Widget, XEvent*, String*, Cardinal*);
extern void action_list_up(Widget, XEvent*, String*, Cardinal*);
extern void action_list_down(Widget, XEvent*, String*, Cardinal*);
extern void action_tree_or_list_up(Widget, XEvent*, String*, Cardinal*);
extern void action_tree_or_list_down(Widget, XEvent*, String*, Cardinal*);
extern void action_exit_mode(Widget, XEvent*, String*, Cardinal*);
extern void action_enter_mode(Widget, XEvent*, String*, Cardinal*);
extern void action_tree_left_or_exit_mode(Widget, XEvent*, String*, Cardinal*);
extern void action_tree_right_or_enter_mode(Widget, XEvent*,
					    String*, Cardinal*);
extern void action_goto_next_hot(Widget, XEvent*, String*, Cardinal*);
extern void action_view_thread(Widget, XEvent*, String*, Cardinal*);
extern void action_change_size(Widget, XEvent*, String*, Cardinal*);
extern void action_popup_find_group(Widget, XEvent*, String*, Cardinal*);
extern void action_do_the_right_thing(Widget, XEvent*, String*, Cardinal*);
