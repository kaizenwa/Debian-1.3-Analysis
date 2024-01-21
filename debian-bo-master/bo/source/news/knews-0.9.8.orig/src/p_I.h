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

#ifndef MAIL_COMMAND
#error "You need to define MAIL_COMMAND in configure.h"
#endif

#define POST            (1<<0)
#define MAIL            (1<<1)
#define POST_DONE       (1<<2)
#define MAIL_DONE       (1<<3)
#define OK_TO_POST      (1<<4)
#define NEEDS_SENDER    (1<<5)
#define ORIG_MAIL       (1<<6) /* article originally intended as email only */

typedef struct PostContext {
    char			*file_name;
    const char			*art;
    char			*charset;
    int				line;
    int				n_attachments;
    unsigned short		flags;
    unsigned char		busy;
    unsigned char		has_8bit;
    struct PostWidgets		*widgets;
    struct PostAttachment	**attachments;
    char			*q_str;
    char			*qq_str;
} PostContext;

typedef struct PostAttachment	PostAttachment;
