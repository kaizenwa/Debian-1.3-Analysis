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
    XFontStruct		*body_font;
    XFontStruct		*quote_font;
    XFontStruct		*header_font;
    XFontStruct		*list_font;
    XFontStruct		*tree_font;
    const struct DecodeFuncs	*funcs;
    int			head_enc_hack;
} MimeFont;

extern MimeFont	*default_font;
extern MimeFont	*ascii_font;

extern void	 init_fonts(Widget);
extern MimeFont	*get_font(char*);
extern void	 font_enter_group(void);
