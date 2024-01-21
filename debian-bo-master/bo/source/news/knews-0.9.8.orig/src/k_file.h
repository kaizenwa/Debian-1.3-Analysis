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
struct KILL_FILE;

extern void		 read_global_kill_file(void);
extern int		 update_kill_files(void);
extern void		 kill_exit_group(GROUP*);
extern void		 kill_cleanup(void);
extern void		 kill_articles(GROUP*);
extern void		 kill_edit_popup(GROUP*);
extern int		 add_kill_node(struct KILL_FILE*, int,
				       int, int, int, char*, char*, char*);
extern struct KILL_FILE	*get_kill_file(GROUP*);
