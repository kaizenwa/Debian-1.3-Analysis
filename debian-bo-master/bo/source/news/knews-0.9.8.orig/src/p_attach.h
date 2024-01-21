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
struct PostAttachment;

extern struct PostAttachment
		*create_attachment(char*, char*);
extern void	 free_attachment(struct PostAttachment*);
extern int	 print_attachment(FILE*, struct PostAttachment*);
extern void	 print_attach_info(struct PostAttachment*, char*);

extern int	 attach_get_enc(struct PostAttachment*);
extern int	 attach_is_inline(struct PostAttachment*);
extern char	*attach_get_type(struct PostAttachment*);
extern char	*attach_get_name(struct PostAttachment*);
extern char	*attach_get_descr(struct PostAttachment*);

extern int	 attach_set_enc(struct PostAttachment*, int, char*);
extern int	 attach_set_inline(struct PostAttachment*, int, char*);
extern int	 attach_set_type(struct PostAttachment*, char*, char*);
extern int	 attach_set_name(struct PostAttachment*, char*, char*);
extern int	 attach_set_descr(struct PostAttachment*, char*, char*);
