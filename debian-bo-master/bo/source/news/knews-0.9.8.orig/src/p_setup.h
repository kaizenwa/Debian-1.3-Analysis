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
struct PostContext;

extern struct PostContext	*create_post_context(int, char*);
extern void			free_post_context(struct PostContext*);
extern int			outstanding_posts(void);

extern void	post_setup(struct PostContext*, FILE*, ARTICLE*,
			   int, int, int);
extern void	append_signature(FILE*);
extern int	insert_extra_headers(FILE*, ARTICLE*);
