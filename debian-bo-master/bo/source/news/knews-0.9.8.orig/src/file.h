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
struct stat;

extern int	open_mkdir(char*, int, int);
extern int	open_expand(char*, int, int);
extern FILE    *fopen_mkdir(char*, char*, int);
extern FILE    *fopen_expand(char*, char*, int);
extern int	unlink_expand(char*);
extern int	chdir_mkdir(char*);
extern int      create_temp_fd(char**);
extern FILE    *create_temp_file(char**);
extern char    *snarf_file(int, long*);
extern int	writen(int, char*, long);
