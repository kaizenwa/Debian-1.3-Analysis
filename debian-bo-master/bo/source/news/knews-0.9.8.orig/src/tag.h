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
extern void       arttree_tag_callback(Widget, XtPointer, XtPointer);
extern void       thread_list_tag_callback(Widget, XtPointer, XtPointer);
extern void       clear_tagged_articles(void);
extern ARTICLE  **get_tagged_articles(void);
extern long       no_tagged_articles(void);
extern void       mark_tagged_articles(ARTICLE*);
extern void       tag_hot_articles(void);
extern void	  untag_article(ARTICLE*);
extern void       history_push(ARTICLE*);
extern ARTICLE   *history_pop(void);
extern ARTICLE   *history_peek(void);
extern void       clear_history(void);
extern void       action_tag_thread(Widget, XEvent*, String*, Cardinal*);
extern void       action_tag_subject(Widget, XEvent*, String*, Cardinal*);
extern void       action_untag_thread(Widget, XEvent*, String*, Cardinal*);
extern void       action_untag_subject(Widget, XEvent*, String*, Cardinal*);
