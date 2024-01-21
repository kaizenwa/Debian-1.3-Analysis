/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991-1994  Riley Rainey
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

#include <math.h>
#include "pm.h"
#include <stdio.h>

#define scanSlope 	2.1445

void plotPrimaryTarget(), plotNormalTarget();

#define SetSegment(s, xa, ya, xb, yb) {s.x1=u->rx+(xa); \
s.x2=u->rx+(xb); s.y1=u->ry+(ya); s.y2=u->ry+(yb); }

static char *thirty = "30";

#ifdef notdef

double heading (x)
VPoint  *x; {

	double  m;

	if (x->x == 0.0 && x->y == 0.0)
		return 0.0;

	if ((m = atan2 (x->y, x->x)) < 0.0)
                return (pi * 2.0 + m);
	else
		return m;
}

#endif

/*
 *  We'll build an array that contains the amount of radar radiation that each
 *  craft can "see" coming from another player.  This is used to build each players
 *  radar threat display.
 *
 *  rval[i][j] will represent the amount of radar energy that player j sees coming from
 *  player i.
 *
 *  For neatness, the rval array has been moved to the ptbl vector.
 */

char *ItoA (n, s)
int	n;
char	*s; {

	if (abs(n) > 999) {
		sprintf (s, "%d,%3.3d", n/1000, abs(n) % 1000);
	}
	else 
		sprintf (s, "%d", n);

	return s;
}

int doRadar (c, u)
craft *c;
viewer *u; {

	register craft *p;
	register int	i, t, x, y, mono;
	register int	xc, yc;
	register double	xs, ys;
	char		*buf;
	XSegment	seg[64];
	int		ns = 0;
	static ZInfo	z;

	mono = u->v->flags & VPMono;

	t = c->curRadarTarget = getRadarTarget (c, 0);
	xc = (u->radarWidth + 1) / 2;
	yc = (u->radarWidth + 1) / 2;

/*
 *  Fill background
 */

	z.depth = --u->v->depth;
	z.color = u->v->pixel[radarBackgroundColor->cIndex];
	FillRectangle (u->v->w, u->rx, u->ry,
		u->radarWidth, u->radarWidth, &z);

/*
 *  Draw reference "thing" and the border
 */

	SetSegment (seg[ns], 0, 0, u->radarWidth-1, 0); ns ++;
	SetSegment (seg[ns], u->radarWidth-1, 0, u->radarWidth-1,
		u->radarWidth-1); ns ++;
	SetSegment (seg[ns], 0, u->radarWidth-1, 0, 0); ns ++;
	SetSegment (seg[ns], 0, u->radarWidth-1, u->radarWidth-1,
		u->radarWidth-1); ns ++;
	VDrawSegments (u->v, seg, ns, u->v->pixel[whitePixel]);

	ns = 0;
	SetSegment (seg[ns], xc, yc, xc - 5, yc + 5); ns ++;
	SetSegment (seg[ns], xc - 10, yc, xc - 5, yc + 5); ns ++;
	SetSegment (seg[ns], xc - 10, yc, xc - 18, yc); ns ++;
	SetSegment (seg[ns], xc, yc, xc + 5, yc + 5); ns ++;
	SetSegment (seg[ns], xc + 10, yc, xc + 5, yc + 5); ns ++;
	SetSegment (seg[ns], xc + 10, yc, xc + 18, yc); ns ++;

/*
 *  Radar dead?
 */

	if (isFunctioning (c, SYS_RADAR) == 0) {
		VDrawSegments (u->v, seg, ns, u->v->pixel[radarColor->cIndex]);
		return 0;
	}

/*
 *  Radar off?
 */

	if (c->radarMode <= RM_STANDBY) {
		if (c->radarMode == RM_OFF)
			buf = "RADAR OFF";
		else
			buf = "RADAR STBY";
		c->curRadarTarget = -1;
		VDrawStrokeString (u->v, u->rx + u->rftw * 4,
			u->ry + u->rfth * 3, buf, strlen(buf), u->rfth,
			&u->rz);
		return 0;
	}

/*
 *  Not a real radar mode?
 */

	if (c->radarMode == RM_ILS)
		return 0;

/*
 *  Plot reference lines
 */

	i = (u->radarWidth + 3) / 4;
	y = u->rfth + 3;
	SetSegment (seg[ns], xc, y, xc, y+4); ns ++;
	y = u->radarWidth - 10 - u->rfth;
	SetSegment (seg[ns], xc, y, xc, y+4); ns ++;

	x = xc - i;
	y = u->rfth + 6;
	SetSegment (seg[ns], x, y, x, y+4); ns ++;
	y = u->radarWidth - 10 - u->rfth;
	SetSegment (seg[ns], x, y, x, y+4); ns ++;
	VDrawStrokeString (u->v, x - u->rftw + u->rx, y + 6 + u->rfth + u->ry,
		thirty, 2, u->rfth, &u->rz);

	x = xc + i;
	y = u->rfth + 6;
	SetSegment (seg[ns], x, y, x, y+4); ns ++;
	y = u->radarWidth - 10 - u->rfth;
	SetSegment (seg[ns], x, y, x, y+4); ns ++;
	VDrawStrokeString (u->v, x-u->rftw + u->rx, y + 6 + u->rfth + u->ry,
		thirty, 2, u->rfth, &u->rz);

	i = (u->radarWidth + 3) / 4;
	x = 2;
	SetSegment (seg[ns], x, yc, x+4, yc); ns ++;
	x = u->radarWidth - 6;
	SetSegment (seg[ns], x, yc, x+4, yc); ns ++;

	x = 5 + 2 * u->rftw;
	y = yc - i;
	SetSegment (seg[ns], x, y, x+4, y); ns ++;
	x = 3;
	VDrawStrokeString (u->v, x + u->rx, y + (u->rfth+1)/2 + u->ry,
		thirty, 2, u->rfth, &u->rz);
	x = u->radarWidth - 6;
	SetSegment (seg[ns], x, y, x+4, y); ns ++;

	x = 5 + 2 * u->rftw;
	y = yc + i;
	SetSegment (seg[ns], x, y, x+4, y); ns ++;
	x = 3;
	VDrawStrokeString (u->v, x + u->rx, y + (u->rfth+1)/2 + u->ry,
		thirty, 2, u->rfth, &u->rz);
	x = u->radarWidth - 6;
	SetSegment (seg[ns], x, y, x+4, y); ns ++;

	VDrawSegments (u->v, seg, ns, u->v->pixel[radarColor->cIndex]);
	
/*
 *  Now plot all targets visible to the radar system.
 */

	u->v->w->clip.x1 = u->rx + 1;
	u->v->w->clip.y1 = u->ry + 1;
	u->v->w->clip.x2 = u->rx + u->radarWidth - 2;
	u->v->w->clip.y2 = u->ry + u->radarHeight - 2;

	for ((i=0, p=ptbl); i<MAXPLAYERS; (++i, ++p)) {
		if (c->relValid[i] > 0 && c->relPos[i].x >= 0.0) {

			xs = c->relPos[i].y / c->relPos[i].x / scanSlope;
			ys = c->relPos[i].z / c->relPos[i].x / scanSlope;
			if (fabs(xs) <= 1.0 && fabs(ys) <= 1.0) {
				x = xs * (double) xc + xc;
				y = ys * (double) yc + yc;
				if (t == i)
					plotPrimaryTarget (c, u, i, x, y);
				else
					plotNormalTarget (u, x, y);
			}
		}
	}

	u->v->w->clip.x1 = 0;
	u->v->w->clip.y1 = 0;
	u->v->w->clip.x2 = u->v->w->width - 1;
	u->v->w->clip.y2 = u->v->w->height - 1;
	return 0;
}

int isRadarVisible (c, i)
craft *c;
int	i; {

	register double d, xs, ys;
	VPoint	r1;

	if (ptbl[i].type == CT_FREE || i == c->pIndex)
		return 0;

/*
 *  Is the set on ?
 */

	if (c->radarMode <= RM_STANDBY ||
		isFunctioning (c, SYS_RADAR) == 0) {
		ptbl[i].rval[c->pIndex] = 0.0;
		return 0;
	}

/*
 *  Calculate the coordinates of the target relative to the craft's frame.
 */

	r1.x = ptbl[i].Sg.x - c->Sg.x;
	r1.y = ptbl[i].Sg.y - c->Sg.y;
	r1.z = ptbl[i].Sg.z - c->Sg.z;
	VTransform (&r1, &(c->Itrihedral), &(c->relPos[i]));

	ptbl[i].rval[c->pIndex] = 0.0;

	if (c->relPos[i].x <= 0.0)
		return 0;

	d = c->relPos[i].x * scanSlope;
	xs = c->relPos[i].y / d;
	ys = c->relPos[i].z / d;

/*
 *  if the absolute values of xs and ys are both less than 1.0, then
 *  we are painting this target with radar energy.  Estimate the value of
 *  the energy that the target sees.
 */

	if ((fabs(xs) <= 1.0) && (fabs(ys) <= 1.0)) {
		d = mag(c->relPos[i]);
#ifdef notdef
		ptbl[i].rval[c->pIndex] = c->cinfo->radarOutput / (d * d);
		if (ptbl[i].flags & FL_GND_CONTACT)
			return 0;
		else if (d >= c->cinfo->radarDRange)
			return 0;
		else if (d >= c->cinfo->radarTRange)
			return 1;
#endif
		ptbl[i].rval[c->pIndex] = 15000.0 / (d * d);
		if (d >= 80.0 * NM)
			return 0;
		else if (d >= 60.0 * NM)
			return 1;
		else
			return 2;
	}
	else
		return 0;

}

int getRadarTarget (c, new)
craft 	*c;
int	new; {

	register int	i, minID;
	register double minD, d;

	for (i=0; i<MAXPLAYERS; ++i)
		c->relValid[i] = isRadarVisible(c, i);

	if (new == 0) {
		i = c->curRadarTarget;
		if (i != -1 && c->relValid[i] != 0)
			return i;
	}

	minD = 1000.0 * NM;
	minID = -1;
	for (i=0; i<MAXPLAYERS; ++i) {
		if (c->relValid[i] == 2) {
			if (new == 1 && i == c->curRadarTarget)
				continue;
			d = mag (c->relPos[i]);
			if (d < minD) {
				minD = d;
				minID = i;
			}
		}
	}

	return minID;
}

void EmitRadarEnergy () {

	register int i, j;
	register craft	*p;

	for (i=0, p = &ptbl[0]; i<MAXPLAYERS; ++i, ++p) {
		if (p->type != CT_FREE) {
			for (j = 0; j < MAXPLAYERS; ++ j)
				(void) isRadarVisible(p, j);
		}
	}
}

void plotNormalTarget (u, x, y)
viewer *u;
int	x, y; {

	int	ns = 0;
	XSegment seg[4];

	SetSegment (seg[ns], x-2, y-2, x-2, y+2); ns ++;
	SetSegment (seg[ns], x-2, y+2, x+2, y+2); ns ++;
	SetSegment (seg[ns], x+2, y+2, x+2, y-2); ns ++;
	SetSegment (seg[ns], x+2, y-2, x-2, y-2); ns ++;

	VDrawSegments (u->v, seg, ns, u->v->pixel[radarColor->cIndex]);
}

extern double heading();

void plotPrimaryTarget (c, u, i, x, y)
craft  *c;
viewer *u;
int	i;
int	x, y; {

	int	xp, yp;
	char	s[16];
	VPoint	rel, deltaV;
	double	d, cl;

	xp = u->radarWidth - 9 * u->rftw;
	yp = u->radarWidth - 8 * u->rfth;

	FillRectangle (u->v->w, x-3 + u->rx, y-3 + u->ry, 7, 7,
		&u->rz);

/*
 *  Heading of target
 */

	sprintf (s, "  %3.3d", (int) (RADtoDEG(ptbl[i].curHeading)));
	VDrawStrokeString (u->v, xp + u->rx, yp + u->ry,
		s, strlen(s), u->rfth, &u->rz);

/*
 *  Relative heading to target.
 */

	rel.x = ptbl[i].Sg.x - c->Sg.x;
	rel.y = ptbl[i].Sg.y - c->Sg.y;
	rel.z = ptbl[i].Sg.z - c->Sg.z;
	sprintf (s, "  %3.3d R", (int) RADtoDEG(heading(&rel)));
	VDrawStrokeString (u->v, xp + u->rx, yp + (15 * u->rfth / 10) + u->ry,
		s, strlen(s), u->rfth, &u->rz);

/*
 *  Closure rate
 */

	deltaV.x = ptbl[i].Cg.x - c->Cg.x;
	deltaV.y = ptbl[i].Cg.y - c->Cg.y;
	deltaV.z = ptbl[i].Cg.z - c->Cg.z;
	d = mag (rel);
	cl = - (deltaV.x * rel.x + deltaV.y * rel.y + deltaV.z + rel.z) / d;
	c->targetDistance = d;
	c->targetClosure = cl;
	sprintf (s, "%5d C", (int)(cl / NM * 3600.0));
	VDrawStrokeString (u->v, xp + u->rx, yp + (30 * u->rfth / 10) + u->ry,
		s, strlen(s), u->rfth, &u->rz);

/*
 *  Range to target
 */

	xp = 40 * u->radarWidth / RADAR_WINDOW_WIDTH;
	yp = u->rfth + 4;	
	sprintf (s, "%d", (int) (d / NM));
	VDrawStrokeString (u->v, xp + u->rx, yp + u->ry,
		s, strlen(s), u->rfth, &u->rz);

/*
 *  Altitude of target
 */

	xp = 150 * u->radarWidth / RADAR_WINDOW_WIDTH;
	yp = u->rfth + 4;	
	sprintf (s, "%d", (int) (- ptbl[i].Sg.z / 1000.0));
	VDrawStrokeString (u->v, xp + u->rx, yp + u->ry,
		s, strlen(s), u->rfth, &u->rz);
}

/*
 *  doTEWS :  update the threat display for player i.
 */

void doTEWS (c, u)
craft	*c;
viewer	*u; {

	register int	i, x, y, hostile_found = 0;
	VPoint	rel, tmp;
	double	m, unit;

	XSetForeground (u->dpy, u->gc, u->v->flags & VPMono ?
			BlackPixel(u->v->dpy, u->v->screen) :
			u->v->pixel[blackPixel]);
	XFillRectangle (u->dpy, u->win, u->gc, u->TEWSx-u->TEWSSize/2,
		u->TEWSy-u->TEWSSize/2, u->TEWSSize, u->TEWSSize);
	XSetForeground (u->dpy, u->gc, u->v->flags & VPMono ?
			WhitePixel(u->v->dpy, u->v->screen) :
			u->v->pixel[whitePixel]);

	XDrawArc(u->dpy, u->win, u->gc, u->TEWSx-(u->TEWSSize+1)/2,
		u->TEWSy-(u->TEWSSize+1)/2, u->TEWSSize, u->TEWSSize,
		0, 360*64);

	for (i=0; i<MAXPLAYERS; ++i) {

		if (c->pIndex == i)
			continue;

		if (c->rval[i] > c->cinfo->TEWSThreshold) {
			tmp.x = ptbl[i].Sg.x - c->Sg.x;
			tmp.y = ptbl[i].Sg.y - c->Sg.y;
			tmp.z = ptbl[i].Sg.z - c->Sg.z;
			VTransform (&tmp, &(c->Itrihedral), &(rel));
			m = mag(rel);
			rel.x /= m;
			rel.y /= m;
			rel.z /= m;
			unit = sqrt (rel.x * rel.x + rel.y * rel.y);
			if (unit == 0.0) {
				rel.x = 1.0;
				rel.y = 0.0;
			}
			x = u->TEWSx + (int) (rel.y * u->TEWSSize * 0.4 / unit);
			y = u->TEWSy - (int) (rel.x * u->TEWSSize * 0.4 / unit);
			if (c->team == ptbl[i].team) {
				XDrawLine (u->dpy, u->win, u->gc,
					x-2, y-2, x-2, y+2);
				XDrawLine (u->dpy, u->win, u->gc,
					x-2, y+2, x+2, y+2);
				XDrawLine (u->dpy, u->win, u->gc,
					x+2, y+2, x+2, y-2);
				XDrawLine (u->dpy, u->win, u->gc,
					x+2, y-2, x-2, y-2);
			}
			else {
				XFillRectangle (u->dpy, u->win, u->gc,
					x-3, y-3, 7, 7);
				hostile_found = 1;
			}
		}
	}

	if (c->vl == u) {
	    if (hostile_found == 1 && u->flow[SoundLockWarning] == 0) {
	    	playContinuousSound (c, SoundLockWarning);
	    }
	    else if (hostile_found == 0 && u->flow[SoundLockWarning] != 0) {
		stopSound (c, SoundLockWarning);
	    }
	}

}

void
doDroneRadar (c)
craft *c;
{
	VPoint	deltaV, rel;
	register int	i;

	if ((i = c->curRadarTarget = getRadarTarget (c, 0)) != -1) {
		rel.x = ptbl[i].Sg.x - c->Sg.x;
		rel.y = ptbl[i].Sg.y - c->Sg.y;
		rel.z = ptbl[i].Sg.z - c->Sg.z;
		deltaV.x = ptbl[i].Cg.x - c->Cg.x;
		deltaV.y = ptbl[i].Cg.y - c->Cg.y;
		deltaV.z = ptbl[i].Cg.z - c->Cg.z;
		c->targetDistance = mag (rel);
		c->targetClosure = - (deltaV.x * rel.x +
		    deltaV.y * rel.y + deltaV.z + rel.z) / c->targetDistance;
	}
}
