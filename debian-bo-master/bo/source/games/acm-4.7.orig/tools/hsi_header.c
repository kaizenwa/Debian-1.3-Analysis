#include <stdio.h>
#include <math.h>
#include <Vlib.h>
#include <VFont.h>
#include <imath.h>

#define DEGtoRAD(a)	(a * M_PI / 180.0)

#define MAJOR_TICK	0.93
#define MINOR_TICK	0.97
#define CHAR_BASELINE	0.78
#define CHAR_WIDTH	(0.025 * 3.9)
#define FONT_SCALE	(0.025 * 3.9)

#define LEGEND_TICK	1.1
#define PLANE_HEIGHT	0.4
#define PLANE_WING	0.25
#define PLANE_TAIL	0.13

#define CDI_WIDTH	0.08
#define CDI_HEIGHT	1.00
#define CDI_DOT_RADIUS	0.03
#define CDI_DOT_SPACE	0.25
#define SCP_HEIGHT	0.40
#define SCP_ARROW_START 0.30

#define GS_DOT_RADIUS	0.05
#define GS_DOT_SPACE	0.35
#define GS_Y		1.4

#define VOR_DIR_WIDTH	(CDI_WIDTH * 1.7)
#define VOR_DIR_HEIGHT	(CDI_WIDTH * 1.7 * 0.867)
#define VOR_DIR_BASE	0.24

extern void DrawStrokeString();

VGlyphPath	paths[1024];
VGlyphVertex3	verticies[4096];

int path_count = 0;
int vertex_count = 0;

VPoint vertx[16];
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
Circle (y, z, radius, nseg)
double	y, z, radius;
int	nseg;
{

	int	i;
	VPoint	pt, first_pt;
	double	a, incr = 360.0 / nseg;

	pt.x = 1.0;

	nvert = 0;

	for (i=0; i<nseg; ++i) {

		a = DEGtoRAD (incr * i);
		pt.y = sin(a) * radius + y;
		pt.z = cos(a) * radius + z;
		PrintPoint (pt.x, pt.y, pt.z);
		if (i == 0)
			first_pt = pt;
	}

	PrintPoint (first_pt.x, first_pt.y, first_pt.z);

	PrintPath (vertx, nvert);
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

void WriteSegmentTables (file, prefix)
FILE	*file;
char	*prefix;
{

	int	i;

	printf ("VGlyphPath %s_path [] = {\n", prefix);
	for (i=0; i<path_count; ++i)
		printf ("  { %d, %d },\n",
			paths[i].vertex_count, paths[i].vertex_start);

	printf ("};\nVGlyphVertex3 %s_vertex [] = {\n", prefix);

	for (i=0; i<vertex_count; ++i)
		printf ("  { 0x%x, 0x%x, 0x%x },\n",
			verticies[i].x & 0xffff,
			verticies[i].y & 0xffff,
			verticies[i].z & 0xffff);

	printf ("};\n\n");

	path_count = vertex_count = 0;

}

main () {

	char		*s, buf[64];
	int		i;
	double		hdg;
	VMatrix		mat;
	VPoint		f, a, b;

	for (i=0; i<72; ++ i) {

		hdg = DEGtoRAD((double) i * 5.0);

		a.x = 1.0;
		a.y = sin (hdg);
		a.z = cos (hdg);

		b = a;

		if ((i % 2) == 0) {
			b.y *= MAJOR_TICK;
			b.z *= MAJOR_TICK;
		}
		else {
			b.y *= MINOR_TICK;
			b.z *= MINOR_TICK;
		}

		nvert = 0;
		PrintPoint (a.x, a.y, a.z);
		PrintPoint (b.x, b.y, b.z);
		PrintPath (vertx, nvert);

		VIdentMatrix (&mat);
		VRotate (&mat, XRotation, - hdg);

		if ((i % 6) == 0) {

			if (i == 0) {
				s = "N";
			}
			else if (i == 18) {
				s = "E";
			}
			else if (i == 36) {
				s = "S";
			}
			else if (i == 54) {
				s = "W";
			}
			else {
				sprintf (buf, "%d", i / 2);
				s = buf;
			}

			f.x = 1.0;
			f.z = CHAR_BASELINE;

			if (strlen (s) == 1)
				f.y = - CHAR_WIDTH / 2.0;
			else
				f.y = - CHAR_WIDTH;

			DrawStrokeString (&f, s, strlen (s),
				FONT_SCALE, &mat, 0);
		}
	}

/*
 *  Now create the output file
 */

	printf ("/*\n *  This file created by heading.c\n */\n");
	printf ("#include <VFont.h>\n");

	WriteSegmentTables (stdout, "heading");

/*
 * build the legend
 */

	a.x = b.x = 1.0;

	for (i=0; i < 8; ++i) {

		hdg = DEGtoRAD((double) i * 45.0);
		a.y = sin (hdg);
		a.z = cos (hdg);
		b.y = a.y * LEGEND_TICK;
		b.z = a.z * LEGEND_TICK;

		nvert = 0;
		PrintPoint (a.x, a.y, a.z);
		PrintPoint (b.x, b.y, b.z);
		PrintPath (vertx, nvert);
	}

	nvert = 0;
	PrintPoint (1.0, CDI_WIDTH / 2.0, PLANE_HEIGHT / 2.0);
	PrintPoint (1.0, CDI_WIDTH / 2.0, - PLANE_HEIGHT / 2.0);
	PrintPoint (1.0, PLANE_TAIL, - PLANE_HEIGHT / 2.0);
	PrintPath (vertx, nvert);

	nvert = 0;
	PrintPoint (1.0, - CDI_WIDTH / 2.0, PLANE_HEIGHT / 2.0);
	PrintPoint (1.0, - CDI_WIDTH / 2.0, - PLANE_HEIGHT / 2.0);
	PrintPoint (1.0, - PLANE_TAIL, - PLANE_HEIGHT / 2.0);
	PrintPath (vertx, nvert);

	nvert = 0;
	PrintPoint (1.0, CDI_WIDTH / 2.0, PLANE_HEIGHT / 6.0);
	PrintPoint (1.0, PLANE_WING, PLANE_HEIGHT / 6.0);
	PrintPath (vertx, nvert);

	nvert = 0;
	PrintPoint (1.0, - CDI_WIDTH / 2.0, PLANE_HEIGHT / 6.0);
	PrintPoint (1.0, - PLANE_WING, PLANE_HEIGHT / 6.0);
	PrintPath (vertx, nvert);

	WriteSegmentTables (stdout, "legend");

	nvert = 0;
	PrintPoint (1.0, CDI_WIDTH / 2.0, CDI_HEIGHT / 2.0);
	PrintPoint (1.0, CDI_WIDTH / 2.0, - CDI_HEIGHT / 2.0);
	PrintPoint (1.0, - CDI_WIDTH / 2.0, - CDI_HEIGHT / 2.0);
	PrintPoint (1.0, - CDI_WIDTH / 2.0, CDI_HEIGHT / 2.0);
	PrintPoint (1.0, CDI_WIDTH / 2.0, CDI_HEIGHT / 2.0);
	PrintPath (vertx, nvert);

	WriteSegmentTables (stdout, "cdi");

	nvert = 0;
	PrintPoint (1.0, CDI_WIDTH / 2.0, CDI_HEIGHT / 2.0);
	PrintPoint (1.0, CDI_WIDTH / 2.0, CDI_HEIGHT / 2.0 + SCP_ARROW_START);
	PrintPoint (1.0, 0.0, CDI_HEIGHT / 2.0 + SCP_HEIGHT);
	PrintPoint (1.0, - CDI_WIDTH / 2.0, CDI_HEIGHT / 2.0 + SCP_ARROW_START);
	PrintPoint (1.0, - CDI_WIDTH / 2.0, CDI_HEIGHT / 2.0);
	PrintPoint (1.0, CDI_WIDTH / 2.0, CDI_HEIGHT / 2.0);
	PrintPath (vertx, nvert);

	nvert = 0;
	PrintPoint (1.0, CDI_WIDTH / 2.0, - CDI_HEIGHT / 2.0);
	PrintPoint (1.0, CDI_WIDTH / 2.0, - CDI_HEIGHT / 2.0 - SCP_ARROW_START);
	PrintPoint (1.0, -CDI_WIDTH / 2.0, - CDI_HEIGHT / 2.0 - SCP_ARROW_START);
	PrintPoint (1.0, -CDI_WIDTH / 2.0, - CDI_HEIGHT / 2.0);
	PrintPoint (1.0, CDI_WIDTH / 2.0, - CDI_HEIGHT / 2.0);
	PrintPath (vertx, nvert);

	Circle (2.0 * CDI_DOT_SPACE, 0.0, CDI_DOT_RADIUS, 8);
	Circle (- 2.0 * CDI_DOT_SPACE, 0.0, CDI_DOT_RADIUS, 8);
	Circle (CDI_DOT_SPACE, 0.0, CDI_DOT_RADIUS, 8);
	Circle (- CDI_DOT_SPACE, 0.0, CDI_DOT_RADIUS, 8);  

	WriteSegmentTables (stdout, "scp");

	Circle (GS_Y, 2.0 * GS_DOT_SPACE, GS_DOT_RADIUS, 8);
	Circle (GS_Y, - 2.0 * GS_DOT_SPACE, GS_DOT_RADIUS, 8);
	Circle (GS_Y, GS_DOT_SPACE, GS_DOT_RADIUS, 8);
	Circle (GS_Y, - GS_DOT_SPACE, GS_DOT_RADIUS, 8);

	hdg = GS_DOT_RADIUS;
	nvert = 0;
	PrintPoint (1.0, GS_Y + hdg, hdg * 0.6);
	PrintPoint (1.0, GS_Y - hdg, hdg * 0.6);
	PrintPoint (1.0, GS_Y - hdg, - hdg * 0.6);
	PrintPoint (1.0, GS_Y + hdg, - hdg * 0.6);
	PrintPoint (1.0, GS_Y + hdg, hdg * 0.6);
	PrintPath (vertx, nvert);

	WriteSegmentTables (stdout, "gs_scale");

	hdg = GS_DOT_RADIUS;
	nvert = 0;
	PrintPoint (1.0, GS_Y - hdg, hdg);
	PrintPoint (1.0, GS_Y - hdg, - hdg);
	PrintPoint (1.0, GS_Y - (4.0 * hdg), - (2.0 * hdg));
	PrintPoint (1.0, GS_Y - (4.0 * hdg), (2.0 * hdg));
	PrintPoint (1.0, GS_Y - hdg, hdg);
	PrintPath (vertx, nvert);

	WriteSegmentTables (stdout, "gs_pointer");

	nvert = 0;
	PrintPoint (1.0, 0.0, VOR_DIR_BASE + VOR_DIR_HEIGHT);
	PrintPoint (1.0, VOR_DIR_WIDTH / 2.0, VOR_DIR_BASE);
	PrintPoint (1.0, -VOR_DIR_WIDTH / 2.0, VOR_DIR_BASE);
	PrintPoint (1.0, 0.0, VOR_DIR_BASE + VOR_DIR_HEIGHT);
	PrintPath (vertx, nvert);

	WriteSegmentTables (stdout, "vor_to_from");

	exit (0);

}
