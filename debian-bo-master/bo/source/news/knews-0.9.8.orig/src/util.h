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
extern ARTICLE *next_in_thread_preorder(ARTICLE*);
extern ARTICLE *next_in_subthread_preorder(ARTICLE*, ARTICLE*);
extern ARTICLE *preorder_skip_subthread(ARTICLE*);
extern ARTICLE *next_in_thread_wrap(ARTICLE*);
extern ARTICLE *next_in_subthread_wrap(ARTICLE*, ARTICLE*);
extern ARTICLE *next_in_thread_dont_wrap(ARTICLE*);
extern ARTICLE *prev_in_thread_dont_wrap(ARTICLE*);
extern void	update_subj_hot_value(SUBJECT*);
extern long	mark_subject_unread(SUBJECT*);
extern long	mark_sub_subject_unread(ARTICLE*, SUBJECT*);
extern long	mark_subject_read(SUBJECT*, int, int);
extern long	mark_sub_subject_read(ARTICLE*, SUBJECT*, int, int);
extern long	mark_subthread_read(ARTICLE*, int, int);
extern long	mark_thread_read(ARTICLE*, int, int);
extern long	mark_subject_hot(SUBJECT*, Pixmap);
extern long	mark_thread_hot(ARTICLE*, Pixmap);
extern long	mark_subthread_hot(ARTICLE*, Pixmap);
extern void	process_xref(ARTICLE*);
extern void	fake_xref(ARTICLE*, char**, int);
extern void	free_read_arts_list(GROUP*);
extern ART_LIST_NODE *create_read_arts_list(void);
extern ARTICLE *first_unread_article_with_subject(SUBJECT*);

extern void	ascii_lower(char*);
extern void	ascii_nlower(char*, long);
extern void	memcpy_lower(char*, char*, long);
extern int	case_strcmp(const char*, const char*);
extern int	case_strncmp(const char*, const char*, long);
extern int	case_lstrcmp(const char*, const char*);
extern int	case_lstrncmp(const char*, const char*, long);
extern int	case_lhassub(const char*, const char*);

#define IS_LOWER(c) \
((unsigned int)((c) - 'a') <= 'z' - 'a')
#define IS_UPPER(c) \
((unsigned int)((c) - 'A') <= 'Z' - 'A')
#define IS_ALPHA(c) \
(IS_UPPER(c) || IS_LOWER(c))
#define LOWER(u)    \
((unsigned char)(u) + ('a' - 'A'))
#define UPPER(u)    \
((unsigned char)(u) - ('a' - 'A'))
#define TO_LOWER(c)  \
(IS_UPPER(c) ? LOWER(c) : (unsigned char)(c))
#define TO_UPPER(c)  \
(IS_LOWER(c) ? UPPER(c) : (unsigned char)(c))
#define IS_DIGIT(c) \
((unsigned int)((c) - '0') < 10u)

#define IS_SPACE(c) \
((c) == ' ' || (c) == '\t')
#define IS_DOT(buffer) \
((buffer)[0] == '.' && (buffer)[1] == '\0')
