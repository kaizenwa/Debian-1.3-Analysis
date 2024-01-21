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
extern int	get_newsgroups(void);
extern int	get_newsgroups_from_newsrc(void);
extern int	get_descriptions(void);
extern char    *rescan(void);
extern int	update_newsrc(void);
extern int	check_for_new_groups(void);
extern void	parse_newsrc(int);
extern void	sort_groups(void);
extern void	calc_no_unread(GROUP*);
extern GROUP   *create_group(char*);
extern GROUP   *find_group(char*);
