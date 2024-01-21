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
extern void update_misc_menu(NewsMode);
extern void create_misc_menu1(Widget);
extern void create_misc_menu2(Widget);
extern void popdown_msgid_dialogue(void);
extern void action_mark_read_article(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_read_subject(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_read_thread(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_read_subthread(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_read_tagged(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_read_all(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_read_to_current(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_read_non_tagged(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_read_cold(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_unread_article(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_unread_subject(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_unread_thread(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_unread_subthread(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_unread_tagged(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_unread_all(Widget, XEvent*, String*, Cardinal*);
extern void action_mark_unread_killed(Widget, XEvent*, String*, Cardinal*);
extern void action_clear_tagged(Widget, XEvent*, String*, Cardinal*);
extern void action_catchup(Widget, XEvent*, String*, Cardinal*);
extern void action_subscribe(Widget, XEvent*, String*, Cardinal*);
extern void action_unsubscribe(Widget, XEvent*, String*, Cardinal*);
extern void action_tag_hot(Widget, XEvent*, String*, Cardinal*);
