/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */

extern int search(char *, int, int);
extern int search_forward(char *);
extern int search_backward(char *);
extern int forward_search_line(char *);
extern int backward_search_line(char *);
extern int bol_fsearch(char *);
extern int bol_bsearch(char *);
extern int re_search_forward(char *);
extern int re_search_backward(char *);
extern int replace_match(char *, int *);
extern void regexp_nth_match(int *);
int insert_file_region (char *, char *, char *);
int search_file(char *, char *, int *);
extern int Case_Sensitive;

