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
enum {
    KillFieldMsgid   = 0,
    KillFieldSubject = 1,
    KillFieldFrom    = 2,
    KillFieldXref    = 3
};

enum {
    KillScopeArticle   = 0,
    KillScopeSubject   = 1,
    KillScopeThread    = 2,
    KillScopeSubthread = 3
};

typedef struct KILL_NODE {
    char		*expr_str;
    regex_t		*expr_re;
    char		*group_str;
    regex_t		*group_re;
    char		*color;
    Pixel		pixel;
    Pixmap		pixmap;
    unsigned int	field         : 2,
			scope         : 2,
			hot           : 1,
			expired       : 1,
			alloced_pixel : 1;
} KILL_NODE;

typedef struct KILL_FILE {
    long		  n;
    struct KILL_NODE	**nodes;
    GROUP		 *group;
    char		 *file_name;
    struct KILL_WIDGETS	 *w;
    unsigned char	  expire;
    unsigned char	  stay_up;
    unsigned char	  dirty;
} KILL_FILE;
