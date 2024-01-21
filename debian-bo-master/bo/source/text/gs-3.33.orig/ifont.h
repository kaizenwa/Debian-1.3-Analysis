/* Copyright (C) 1989, 1991, 1993, 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* ifont.h */
/* Interpreter internal font representation */

#include "gsstruct.h"			/* for extern_st */

/* The external definition of fonts is given in the PostScript manual, */
/* pp. 91-93. */

/* The structure given below is 'client data' from the viewpoint */
/* of the library.  font-type objects (t_struct/st_font, "t_fontID") */
/* point directly to a gs_font.  */

typedef struct font_data_s {
	ref dict;			/* font dictionary object */
	ref BuildChar;
	ref BuildGlyph;
	ref Encoding;
	ref CharStrings;
	ref OtherSubrs;			/* from Private dictionary */
	ref Subrs;			/* from Private dictionary */
} font_data;
/*
 * Even though the interpreter's part of the font data actually
 * consists of refs, allocating it as refs tends to create sandbars;
 * since it is always allocated and freed as a unit, we can treat it
 * as an ordinary structure.
 */
/* st_font_data is exported for zdefault_make_font in zfont.c. */
extern_st(st_font_data);
#define public_st_font_data()	/* in zfont2.c */\
  gs_public_st_ref_struct(st_font_data, font_data, "font_data")
#define pfont_data(pfont) ((font_data *)((pfont)->client_data))
#define pfont_dict(pfont) (&pfont_data(pfont)->dict)

/* Registered encodings, for the benefit of platform fonts, `seac', */
/* and compiled font initialization. */
/* This is a t_array ref that points to the encodings. */
#define registered_Encodings_countof 4
extern ref registered_Encodings;
#define registered_Encoding(i) (registered_Encodings.value.refs[i])
#define StandardEncoding registered_Encoding(0)
