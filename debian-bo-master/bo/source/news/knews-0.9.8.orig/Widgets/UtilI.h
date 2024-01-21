/*
 *  Modified from Xlibint.h.   kjj.
 */

/*

Copyright (c) 1984, 1985, 1987, 1989  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/

#define CI_NONEXISTCHAR(cs) ((cs)->width == 0 &&                         \
			     ((cs)->rbearing | (cs)->lbearing |          \
			      (cs)->ascent   | (cs)->descent ) == 0)

/* 
 * CI_GET_CHAR_INFO_1D - return the charinfo struct for the indicated 8bit
 * character.  If the character is in the column and exists, then return the
 * appropriate metrics (note that fonts with common per-character metrics will
 * return min_bounds).  If none of these hold true, try again with the default
 * char.
 */
#define CI_GET_CHAR_INFO_1D(fs,col,def,cs)                               \
do {                                                                     \
    unsigned int  tmp = col - fs->min_char_or_byte2;                     \
    cs = def;                                                            \
    if (tmp <= fs->max_char_or_byte2 - fs->min_char_or_byte2)            \
	if (fs->per_char == NULL)                                        \
	    cs = &fs->min_bounds;                                        \
	else {                                                           \
	    cs = fs->per_char + tmp;                                     \
	    if (CI_NONEXISTCHAR(cs))                                     \
                cs = def;                                                \
	}                                                                \
} while (0)

#define CI_GET_DEFAULT_INFO_1D(fs,cs)                                    \
    CI_GET_CHAR_INFO_1D (fs, fs->default_char, NULL, cs)



/*
 * CI_GET_CHAR_INFO_2D - return the charinfo struct for the indicated row and 
 * column.  This is used for fonts that have more than row zero.
 */
#define CI_GET_CHAR_INFO_2D(fs,row,col,def,cs)                           \
do {                                                                     \
    unsigned int tmp1 = row - fs->min_byte1;                             \
    unsigned int tmp2 = col - fs->min_char_or_byte2;                     \
    cs = def;                                                            \
    if (tmp1 <= fs->max_byte1 - fs->min_byte1 &&                         \
	tmp2 <= fs->max_char_or_byte2 - fs->min_char_or_byte2)           \
	if (fs->per_char == NULL)                                        \
	    cs = &fs->min_bounds;                                        \
	else {                                                           \
	    cs = fs->per_char + tmp2 + tmp1 *                            \
                (fs->max_char_or_byte2 - fs->min_char_or_byte2 + 1);     \
	    if (CI_NONEXISTCHAR(cs))                                     \
                cs = def;                                                \
        }                                                                \
} while (0)

#define CI_GET_DEFAULT_INFO_2D(fs,cs)                                    \
{                                                                        \
    unsigned int r = (fs->default_char >> 8);                            \
    unsigned int c = (fs->default_char & 0xff);                          \
    CI_GET_CHAR_INFO_2D (fs, r, c, NULL, cs);                            \
}
