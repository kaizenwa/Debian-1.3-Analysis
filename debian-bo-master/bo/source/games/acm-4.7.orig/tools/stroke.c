#include <Vlib.h>
#include "VFont.h"
#include "VRoman.h"

int VFontWidthPixels (v, scale)
Viewport	*v;
int		scale; {

	return VRomanGlyph['A'].glyph_width * scale / 25600;

}

void DrawStrokeString (o, str, len, scale, trans, reverse_flag)
VPoint	 *o;
register unsigned char	*str;
register int	len;
register double	scale; 
VMatrix	*trans;
int	reverse_flag;
{

	register int	c, i, k, m, nvert;
	register VGlyphVertex *p;
	register double	x1, y1, x2, y2;
	VPoint	 t0, t1, t2, vertx[256];

	if (reverse_flag) {
		str = str + len - 1;
	}

	for ( ; len > 0; -- len) {

	    if ((c = *str) < 128) {
		k = VRomanGlyph[c].path_start;
		if (reverse_flag) {
			o->y -= VRomanGlyph[c].glyph_width * scale / 25600.0;
		}
		for (i = 0; i < VRomanGlyph[c].path_count; ++ i, ++ k) {
			p = &VRomanVertex[VRomanPath[k].vertex_start];
			nvert = 0;
			for (m=0; m < VRomanPath[k].vertex_count; ++m, ++p) {
				x1 = o->y + p->x * scale / 25600.0;
				y1 = o->z + p->y * scale / 25600.0;
				VSetPoint (t1, o->x, x1, y1);
				VTransform (&t1, trans, &t0);
				vertx[nvert++] = t0;
			}
			PrintPath (vertx, nvert);
		}

		if (reverse_flag) {
		    -- str;
		}
		else {
		    ++ str;
		    o->y += VRomanGlyph[c].glyph_width * scale / 25600.0;
		}

	    }
	}
}
