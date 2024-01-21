/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991,1992  Riley Rainey
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; version 2 dated June, 1991.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program;  if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.
 */
#include "../lib/Vlib.h"
#include <X11/Xutil.h>
#include <math.h>
#include <sys/time.h>

#define VIEW1

extern Display  *dpy;
extern Window   win;
extern GC       curGC;
extern XSizeHints xsh;
extern int	mono;

VPolygon *poly[2048];

double lastTime = 1000.0;
double frameCount = 0.0;
double minInterval = 10000.0;
double maxInterval = 0.0;
double elapsedTotal = 0.0;

static VPoint origin = { 0.0, 0.0, 0.0 };

void TimeIntervalAccounting() {

	struct	itimerval itv;
	double	thisTime, elapsedTime;

	getitimer (ITIMER_REAL, &itv);

	thisTime = (double) itv.it_value.tv_sec +
		(double) itv.it_value.tv_usec / 1000000.0;

	elapsedTime = lastTime - thisTime;

	if (elapsedTime > maxInterval)
		maxInterval = elapsedTime;

	if (elapsedTime < minInterval)
		minInterval = elapsedTime;

	elapsedTotal += elapsedTime;
	frameCount += 1;
	lastTime = thisTime;
}

void PrintStatistics () {

	printf ("Total time for %g frames was %g seconds.\n\n", frameCount,
		elapsedTotal);
	printf ("Average frame rate was %g frames per second\n",
		frameCount / elapsedTotal);
	printf ("Maximum frame rate was %g frames per second\n",
		1.0 / minInterval);
	printf ("Minimum frame rate was %g frames per second\n",
		1.0 / maxInterval);
}

VPoint	*normal;
Viewport *vx;

void placeObject (v, obj, loc, roll, pitch, yaw, poly, cnt)
Viewport *v;
VObject *obj;
VPoint  loc;
double roll;
double pitch;
double yaw;
VPolygon **poly;
int	 *cnt; {

	int	 i, j, k, offset, aspect, n;
	VPoint	 *q, tmp, center, nc;
	VMatrix	 mtx, mtx1;
	register double dist;
	VPolygon **p;

	j = *cnt;

	VIdentMatrix (&mtx);
	if (roll != 0.0)
		VRotate (&mtx, XRotation, roll);
	if (pitch != 0.0)
		VRotate (&mtx, YRotation, pitch);
	if (yaw != 0.0)
		VRotate (&mtx, ZRotation, yaw);
	VTranslatePoint (&mtx, loc);

	VMatrixMult (&mtx, &v->eyeSpace, &mtx1);

	VTransform (&obj->center, &mtx, &tmp);
	VTransform (&tmp, &v->eyeSpace, &center);
	for (i=0; i<4; ++i) {
		dist = VPointToClipPlaneDistance (&center, &(normal[i]));
		if (dist > obj->extent) {
			return;
		}
	}

	n = obj->numPolys;
	p = obj->polygon;
	if (obj->order) {
		VTransform (&origin, &mtx1, &nc);
		VTransform_ (&_VUnitVectorI, &mtx1, &obj->xaxis);
		VTransform_ (&_VUnitVectorJ, &mtx1, &obj->yaxis);
		VTransform_ (&_VUnitVectorK, &mtx1, &obj->zaxis);
		aspect = VComputeObjectAspect (obj, &nc);
#ifdef notdef
		printf ("%s: %s\n", obj->name, VGetAspectName(aspect));
#endif
		offset = aspect * n;
	}

	for (i=0; i<n; ++i) {

		if (poly[j] != (VPolygon *) NULL)
			VDestroyPolygon (poly[j]);

		poly[j] = (obj->order) ?
			VCopyPolygon(p[obj->order[offset + i]]) :
			VCopyPolygon(p[i]);
		for ((k=0, q=poly[j]->vertex); k<poly[j]->numVtces; (++k, ++q)) {
			VTransform(q, &mtx1, &tmp);
			*q = tmp;
		}
	        if (poly[j]->flags & PolyNormalValid) {
		    VTransform_ (&poly[j]->normal, &mtx1, &tmp);
		    poly[j]->normal = tmp;
		}
		++j;
	}

	*cnt = j;
}

app(background)
char *background; {

	int	i, cnt, black;
	unsigned int j, curPixel = 0;
	char	*str;
	FILE	*file;
	VObject	*obj, *ftr, *mig;
	VPoint	viewPt, centerInt, up, ftrLoc, ftrLoc2, ftrLoc3, migLoc1, migLoc2;
	Viewport *v;
	double	dist, p, migV, v1, a, migRoll;
	double	updateRate = 10.0;
	Colormap cmap;
	long	screen;
	ZInfo	z;
	VColor	*vsky;
	unsigned long skypixel;
	struct	 itimerval timeval;
	XSegment seg[2];

	str = "../../objects/rwy-and-gnd.obj";

	if ((file = fopen(str, "r")) == (FILE *) NULL) {
		perror ("Cannot open object file");
		exit (1);
	}

	if ((obj = VReadObject(file)) == (VObject *) NULL) {
		fprintf (stderr, "Error reading the object definition.\n");
		exit (1);
	}

	fclose (file);

	str = "../../objects/f16.obj";

	if ((file = fopen(str, "r")) == (FILE *) NULL) {
		perror ("Cannot open object file");
		exit (1);
	}

	if ((ftr = VReadObject(file)) == (VObject *) NULL) {
		fprintf (stderr, "Error reading the object definition.\n");
		exit (1);
	}

	fclose (file);
	

	str = "../../objects/mig29.obj";

	if ((file = fopen(str, "r")) == (FILE *) NULL) {
		perror ("Cannot open object file");
		exit (1);
	}

	if ((mig = VReadObject(file)) == (VObject *) NULL) {
		fprintf (stderr, "Error reading the object definition.\n");
		exit (1);
	}

	fclose (file);
	
	 _VDefaultWorkContext->usePixmaps = 0;

	screen = DefaultScreen (dpy);
	cmap = DefaultColormap (dpy, screen);

	v = VOpenViewport (dpy, screen, win, cmap,
		DefaultVisual (dpy, screen), UNITS_FEET,
		1.5, 1.0, xsh.width, xsh.height);

	normal = v->clipNormals;
	vx = v;

	vsky = VAllocColor ("skyblue");

	if (VBindColors (v, background) < 0) {
		fprintf (stderr, "Error in binding colors.\n");
		exit (1);
	}

	skypixel = v->pixel[vsky->cIndex];

	ftrLoc.x = 80.0;
	ftrLoc.y = -110.0;
	ftrLoc.z = -7.0;

	ftrLoc2.x = 475.0;
	ftrLoc2.y = 3.7;
	ftrLoc2.z = -7.0;

	migLoc1.z = -310.0;
	migLoc2.z = -320.0;
	migV = 370.0 / 3600.0 * 5280.0 / updateRate;	/* mig speed in fps */
	migV = migV / (2500.0 * 3.14 * 2.0); /* mig speed in rad/update */
	migRoll = 28.0 * 3.14 / 180.0;
	a = 90.0 * 3.14 / 180.0;

	v1 = 130.0 / 3600.0 * 5280.0 / updateRate;

	centerInt.x = 80.0;
	centerInt.y = -90.0;
	centerInt.z = -6.0;

	dist = -10000.0; p = 0;

	getitimer (ITIMER_REAL, &timeval);
	timeval.it_value.tv_sec = 1000;
	timeval.it_value.tv_usec = 0;
	setitimer (ITIMER_REAL, &timeval, (struct itimerval *) NULL);

	mono = 0;

	while (1) {

		if (mono == 0) {

			z.depth = MaxDepth;
			z.color = skypixel;
			FillRectangle (v->w, 0, 0, xsh.width, xsh.height, &z);

		}

		ftrLoc3.x = dist+150.0;
		ftrLoc3.y = -15.0;
		ftrLoc3.z = (dist+150.0) * 50.0 / 1000.0 - 40.0;
		if (ftrLoc3.z > -13.0)
			ftrLoc3.z = -13.0;
		viewPt.x = dist;
		viewPt.y = 15.0;
		viewPt.z = dist * 50.0 / 1000.0 - 50.0;
		if (viewPt.z > -13.0)
			viewPt.z = -13.0;
		up = viewPt;
		up.z = up.z - 1.0;
		if (viewPt.z < -14.0)
			up.y = up.y + 0.1 * sin(p);
		p = p + 0.03;
		dist = dist + v1;
		if (dist > 10000.0) {
			PrintStatistics ();
			exit (0);
		}

		migLoc1.x = 0.0 + 2200.0 * cos (a);
		migLoc1.y = -800.0 + 2200.0 * sin (a);
		migLoc2.x = 0.0 + 2225.0 * cos (a-0.04);
		migLoc2.y = -800.0 + 2225.0 * sin (a-0.04);
		a = a + migV;

/*
 *  Calculate eye space information based on our current viewpoint.
 */

#ifdef VIEW1
	VGetEyeSpace (v, viewPt, centerInt, up);
#endif
#ifdef VIEW2
	up = migLoc2;
	up.z = up.z - 1.0;
	VGetEyeSpace (v, migLoc2, viewPt, up);
#endif
#ifdef VIEW3
	up = ftrLoc;
	up.z = up.z - 1.0;
	VGetEyeSpace (v, ftrLoc, viewPt, up);
#endif

/*
 *  Now create a vector containing all polygons from the objects.
 */

	cnt = 0;
	placeObject (v, obj, origin, 0.0, 0.0, 0.0, poly, &cnt);

#ifndef VIEW3
	placeObject (v, ftr, ftrLoc, 0.0, 0.0, 90.0*3.14/180.0, poly, &cnt);
#endif
	placeObject (v, ftr, ftrLoc2, 0.0, 0.0, -3.0*3.14/180.0, poly, &cnt);
	placeObject (v, ftr, ftrLoc3, 0.0, -11.0*3.14/180.0, 0.0, poly, &cnt);
#ifndef VIEW1
	placeObject (v, ftr, viewPt, 0.0, -11.0*3.14/180.0, 0.0, poly, &cnt);
#endif
	placeObject (v, mig, migLoc1, migRoll, 0.0, a+90.0*3.14/180.0, poly, &cnt);
#ifndef VIEW2
	placeObject (v, mig, migLoc2, migRoll, 0.0, a+90.0*3.14/180.0, poly, &cnt);
#endif

	black = BlackPixel(dpy, 0);


/*
 *  First clip, then draw each polygon.
 */

	for (i=0; i<cnt; ++i) {

		if (mono)
			XSetForeground (dpy, curGC, black);

		poly[i] = VClipSidedPolygon(v, poly[i], v->clipPoly);

		if (poly[i]) {
#ifdef notdef
		    if (mono == 0 &&
			curPixel != (j=v->pixel[poly[i]->color->cIndex])) {
			XSetForeground (dpy, curGC, j);
			curPixel = j;
		    }
#endif
		    if (mono)
		        VDrawPolygon (v, win, curGC, poly[i]);
		    else
		        VFillPolygon (v, win, curGC, poly[i]);
		}

	}

	VExposeBuffer (v, curGC);

	TimeIntervalAccounting();

/*
 * Erase the un-displayed planes.
 */

#ifdef notdef
	if (mono == 0) {
		curPixel = *(v->pixel);
        	XSetForeground (dpy, curGC, curPixel);
		if ( _VDefaultWorkContext->usePixmaps == 1)
        	XFillRectangle (dpy, v->monoPixmap, curGC, 0, 0, xsh.width, xsh.height);
		else
        	XFillRectangle (dpy, win, curGC, 0, 0, xsh.width, xsh.height);
	}
#endif


        }

}
