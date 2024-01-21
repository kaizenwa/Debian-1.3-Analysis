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
 * SOFTWARE IS AT THE USER'S OWN RISK.
 */
extern struct KILL_NODE	*parse_kill_line(char*, int);
extern void		 fprint_kill_node(FILE*, struct KILL_NODE*, int);
extern void		 sprint_kill_node(struct KILL_NODE*, char*, long);
extern void		 free_kill_node(struct KILL_NODE*);
extern long		 apply_kill(struct KILL_NODE*);
extern void		 fix_node_pixmap(struct KILL_NODE*);
extern void		 alloc_hot_pixel(struct KILL_NODE*, int);
