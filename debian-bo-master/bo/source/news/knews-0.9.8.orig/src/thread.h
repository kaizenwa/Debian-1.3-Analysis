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
#ifndef THREAD_H
#define THREAD_H

typedef struct THREAD_CONTEXT THREAD_CONTEXT;

extern THREAD_CONTEXT	*main_thr;

extern THREAD_CONTEXT	*create_thread_context(void);
extern void		 clear_thread_context(THREAD_CONTEXT*);
extern ARTICLE		*get_articles(THREAD_CONTEXT*);
extern SUBJECT		*get_subjects(THREAD_CONTEXT*);
extern void		 set_subjects(THREAD_CONTEXT*, SUBJECT*);
extern char		*get_refs(THREAD_CONTEXT*);

extern ARTICLE	*find_article(const char*, long);
extern void	 read_group(char*, int, long);
extern ARTICLE	*parse_head(long, THREAD_CONTEXT*, char*);
extern void	 fix_author(ARTICLE*, char*, int);
extern char	*thread_from_file(struct SERVER*, long);

#endif /* THREAD_H */
