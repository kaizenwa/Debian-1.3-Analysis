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
typedef struct {
    char	*view_command;
    char	*compose;
    char	*compose_typed;
    char	*print;
    char	*edit;
    char	*test;
    char	*x11_bitmap;
    char	*description;
    char	needsterminal;
    char	copiousoutput;
    char	textualnewlines;
} MailcapData;

extern const MailcapData	*mailcap_lookup(char*, char*);
extern void	 mailcap_init(void);
extern char	*expn_tmpl(char*, int, const char*, char**);
