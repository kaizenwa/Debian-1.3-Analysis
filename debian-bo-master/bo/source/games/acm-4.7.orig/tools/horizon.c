#include <stdio.h>
#include <math.h>
#include <Vlib.h>
#include <VFont.h>
#include <imath.h>

#define P_i	DEGtoRAD(6.0)
#define P_o	DEGtoRAD(20.0)
#define P_f	DEGtoRAD(23.0)
#define P_ha	DEGtoRAD(28.0)

#define FONT_SCALE	0.025
#define VERT_BAR_SCALE	0.025

extern void DrawStrokeString();

VGlyphPath	paths[1024];
VGlyphVertex3	verticies[4096];

int path_count;
int vertex_count;

#ifdef notdef
void
PrintPoint (a, b, c)
double	a, b, c;
{
	printf (" { 0x%x, 0x%x, 0x%x },\n", (short) (a * UNITY) & 0xffff,
		(short) (b * UNITY) & 0xffff, (short) (c * UNITY) & 0xffff);
}
#endif

VPoint vertx[3];
int nvert;

#define PrintPoint(a,b,c) { vertx[nvert].x = (a); \
	vertx[nvert].y = (b); vertx[nvert].z = (c); ++ nvert; }


void
PrintPath (v, vcnt)
VPoint	*v;
int	vcnt;
{
	paths[path_count].vertex_count = vcnt;
	paths[path_count++].vertex_start = vertex_count;
	for (; vcnt > 0; --vcnt) {
		verticies[vertex_count].x = (short) (v->x * UNITY);
		verticies[vertex_count].y = (short) (v->y * UNITY);
		verticies[vertex_count++].z = (short) (v->z * UNITY);
		++v;
	}
}

void
DashedLine (x1, y1, z1, x2, y2, z2)
double x1, y1, z1, x2, y2, z2;
{
	VPoint	v;
	register double t, scale, seg;
	register int i;

	v.x = x2 - x1;
	v.y = y2 - y1;
	v.z = z2 - z1;

#define NSEG	3
#define ON_FRACTION	0.7

	scale = 1.0 / (ON_FRACTION * NSEG + (1.0 - ON_FRACTION) * (NSEG - 1));

	seg = ON_FRACTION * scale;

	t = 0.0;
	for (i=0; i < NSEG; ++ i) {
		nvert = 0;
		PrintPoint (x1 + v.x * t, y1 + v.y * t, z1 + v.z * t);
		PrintPoint (x1 + v.x * (t + seg), y1 + v.y * (t + seg),
			z1 + v.z * (t + seg));
		PrintPath (vertx, nvert);
		t += scale;
	}
}

main () {

	double	c, cp2, sp2, dpitch, pitch;
	double	xi, yi, xo, yo, m;
	VPoint	inner, inner_p, f, f1, y_axis, v, vert;
	VMatrix	m1, m2;
	char	spitch[16];
	int	i;

	inner.x = cos (P_i / 2.0);
	inner.y = sin (P_i / 2.0);
	inner.z = 0.0;

	y_axis.x = y_axis.z = 0.0;
	y_axis.y = 1.0;

	xi = inner.x;
	yi = inner.y;

	xo = 1.0;
	yo = tan (P_ha / 2.0);

	nvert = 0;
	PrintPoint (xi, -yi, 0.0);
	PrintPoint (xo, -yo, 0.0);
	PrintPath (vertx, nvert);

	nvert = 0;
	PrintPoint (xi, yi, 0.0);
	PrintPoint (xo, yo, 0.0);
	PrintPath (vertx, nvert);

	for (dpitch=10.0; dpitch < 81.0; dpitch += 10.0) {

		pitch = DEGtoRAD (dpitch);

		VIdentMatrix (&m1);
		VRotate (&m1, YRotation, pitch);
		VTransform (&inner, &m1, &inner_p);

		xi = inner_p.x;
		yi = inner_p.y;

		c = cos((P_o - P_i) / 2.0);
		cp2 = cos (pitch);
		cp2 = cp2 * cp2;
		sp2 = sin (pitch);
		sp2 = sp2 * sp2;

		xo = (xi * c - yi * sqrt (xi * xi * cp2 + yi * yi * cp2 -
			c * c + 2.0 * c * sp2 - sp2 * sp2) - xi * sp2) /
			(xi * xi + yi * yi);

		yo = sqrt (cp2 - (xo * xo));

		v.x = xo;
		v.y = yo;
		v.z = sin(pitch);

		VCrossProd (&v, &y_axis, &vert);
		m = sqrt (vert.x * vert.x + vert.y * vert.y + vert.z *
			vert.z);

		vert.x *= VERT_BAR_SCALE / m;
		vert.y *= VERT_BAR_SCALE / m;
		vert.z *= VERT_BAR_SCALE / m;

		nvert = 0;
		PrintPoint (xi, -yi, inner_p.z);
		PrintPoint (xo, -yo, sin(pitch));
		PrintPoint (
			xo - vert.x, -yo + vert.y, sin(pitch) - vert.z);
		PrintPath (vertx, nvert);

		nvert = 0;
		PrintPoint (xi, yi, inner_p.z);
		PrintPoint (xo, yo, sin(pitch));
		PrintPoint (
			xo - vert.x, yo + vert.y, sin(pitch) - vert.z);
		PrintPath (vertx, nvert);

		DashedLine (xi, -yi, -inner_p.z, xo, -yo, -sin(pitch));
		nvert = 0;
		PrintPoint (xo, -yo, - sin(pitch));
		PrintPoint (
			xo - vert.x, -yo + vert.y, - sin(pitch) + vert.z);
		PrintPath (vertx, nvert);

		DashedLine (xi, yi, -inner_p.z, xo, yo, -sin(pitch));
		nvert = 0;
		PrintPoint (xo, yo, - sin(pitch));
		PrintPoint (
			xo - vert.x, yo + vert.y, -sin(pitch) + vert.z);
		PrintPath (vertx, nvert);

		c = cos((P_f - P_i) / 2.0);
		cp2 = cos (pitch);
		cp2 = cp2 * cp2;
		sp2 = sin (pitch);
		sp2 = sp2 * sp2;

		f.x = (xi * c - yi * sqrt (xi * xi * cp2 + yi * yi * cp2 -
			c * c + 2.0 * c * sp2 - sp2 * sp2) - xi * sp2) /
			(xi * xi + yi * yi);

		f.y = sqrt (cp2 - (xo * xo));

		f.z = sin(pitch);

		f.x = 1.0;
		f.z = 0.0;

		sprintf (spitch, "%d", (int) (fabs(dpitch) + 0.5));
		VIdentMatrix (&m2);
		VRotate (&m2, YRotation, pitch);
		f.y += 0.02;
		f1 = f;
		DrawStrokeString (&f, spitch, 2, FONT_SCALE, &m2, 0);

		f = f1;
		f.y = -f.y;
		DrawStrokeString (&f, spitch, 2, FONT_SCALE, &m2, 1);

		VIdentMatrix (&m2);
		VRotate (&m2, YRotation,  - pitch);
		f = f1;
		DrawStrokeString (&f, spitch, 2, FONT_SCALE, &m2, 0);

		f = f1;
		f.y = -f.y;
		DrawStrokeString (&f, spitch, 2, FONT_SCALE, &m2, 1);

	}

/*
 *  Now create the output file
 */

	printf ("/*\n *  This file created by horizon.c\n */\n");
	printf ("#include <VFont.h>\n");

	printf ("VGlyphPath horizon_path [] = {\n");
	for (i=0; i<path_count; ++i)
		printf ("  { %d, %d },\n",
			paths[i].vertex_count, paths[i].vertex_start);

	printf ("};\nVGlyphVertex3 horizon_vertex [] = {\n");

	for (i=0; i<vertex_count; ++i)
		printf ("  { 0x%x, 0x%x, 0x%x },\n",
			verticies[i].x & 0xffff,
			verticies[i].y & 0xffff,
			verticies[i].z & 0xffff);

	printf ("};\n");

	exit (0);

}
