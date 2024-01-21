/*
 * File:	font.c
 * Purpose:	Implement font loading.
 * Author:	Lars Wirzenius
 * Version:	"@(#)SeX:$Id: font.c,v 1.2 1996/01/05 13:16:05 liw Exp $"
 */

#include <assert.h>
#include <stdlib.h>
#include <publib.h>

#include <X11/Intrinsic.h>

#include "font.h"
#include "x.h"
#include "error.h"



/*
 * Structure:	font
 * Purpose:	Describe a loaded font.
 * Fields:	name	name of font
 *		width	width of widest character in font (in pixels)
 *		height	height of widest character in font (in pixels)
 *		xfont	the X font descriptor
 */
struct font {
	char *name;
	int width;
	int height;
	XFontStruct *xfont;
};



/*
 * Variable:	default_font
 * Purpose:	Give the default font
 * Note:	Returned by font_get_default, set by font_set_default.
 */
static struct font *default_font = NULL;



/*
 * Function:	font_load
 * Purpose:	Load a font.
 * Arguments:	font	pointer to font descriptor that will be initialized
 *		name	name of font to load
 * Return:	-1 for failure (and set *font to NULL), 0 for success.
 */
int font_load(struct font **font, const char *name) {
	struct font *f;

	assert(font != NULL);
	assert(name != NULL);
	assert(*name != '\0');

        f = malloc(sizeof(struct font));
	if (f != NULL) {
	        f->name = strdup(name);
	        f->xfont = XLoadQueryFont(x_display(), name);
	        if (f->name != NULL && f->xfont != NULL) {
		        f->width = f->xfont->max_bounds.width;
			f->height = f->xfont->max_bounds.ascent
					+ f->xfont->max_bounds.descent;
			*font = f;
			return 0;
		}
	}

	error(NULL, "out of memory loading a font");
	
	if (f != NULL) {
		free(f->name);
		/* FIXME: should we free f->xfont also? */
		free(f);
	}
	return -1;
}	



/*
 * Function:	font_width
 * Purpose:	Return the width (in pixels) of a font.
 * Arguments:	font	pointer to font descriptor
 * Return:	width
 * Note:	This function cannot fail.
 */
int font_width(struct font *font) {
	return font->width;
}



/*
 * Function:	font_height
 * Purpose:	Return the height (in pixels) of a font.
 * Arguments:	font	pointer to font descriptor
 * Return:	height
 * Note:	This function cannot fail.
 */
int font_height(struct font *font) {
	return font->height;
}



/*
 * Function:	font_set_default
 * Purpose:	Set the default font.
 * Arguments:	font	the to set to the default font
 * Return:	nothing.
 */
void font_set_default(struct font *font) {
	assert(font != NULL);
	default_font = font;
}



/*
 * Function:	font_get_default
 * Purpose:	Return the default font.
 * Arguments:	none.
 * Return:	Pointer to the font descriptor.
 * Note:	font_set_default must be called before this function.
 */
struct font *font_get_default(void) {
	assert(default_font != NULL);
	return default_font;
}



/*
 * Function:	font_row_position
 * Purpose:	Compute the pixel row that is to be drawn at to get text
 *		rows properly aligned.
 * Arguments:	font	font to be computed for
 *		row	character row
 * Return:	pixel row
 */
int font_row_position(struct font *font, int row) {
	return font->xfont->max_bounds.ascent + row * font->height;
}




/*
 * Function:	font_get_x_font_id
 * Purpose:	Return the X font identifier.
 * Arguments:	font	the font
 * Return:	The X font identifier.
 */
Font font_get_x_font_id(struct font *font) {
	return font->xfont->fid;
}
