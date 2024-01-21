/*
 * File:	font.h
 * Purpose:	Declarations for font loading
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: font.h,v 1.1.1.1 1995/12/29 11:56:39 liw Exp $"
 */

#ifndef font_h
#define font_h

#include <X11/Xlib.h>

struct font;

int font_load(struct font **, const char *);
struct font *font_get_default(void);
void font_set_default(struct font *);
int font_width(struct font *);
int font_height(struct font *);
int font_row_position(struct font *, int);
Font font_get_x_font_id(struct font *);

#endif
