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
struct main_widgets {
    Widget	shell;
    Widget	second_shell;
    Widget	top_manager;
    Widget	top_layout;
    Widget	arttree;
    Widget	group_list;
    Widget	thread_list;
    Widget	knapp[12];
    Widget	text;
};

extern struct main_widgets main_widgets;

extern void create_main_widgets(void);
extern void setNewsModeDisconnected(void);
extern void setNewsModeConnected(void);
extern void setNewsModeGroup(int);
extern void setNewsModeThread(void);
extern void setNewsModeAllgroups(regex_t*);
extern void setNewsModeNewgroups(void);
extern void update_subj_entry(SUBJECT*);
extern void update_group_entry(GROUP*);
extern void purge_hot(Pixmap);
extern void set_message(char*, int);
