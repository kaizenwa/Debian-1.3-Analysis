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
 
#include "pm.h"
#include <math.h>

/*
 *  getStick :  get stick input from mouse
 *		inputs Sa and Se range from -1.0 to 1.0.
 */

int getStick (c, u)
craft *c;
viewer *u; {

	int	rootX, rootY, x, y;
	Window	root, child;
	unsigned int mask;
	double	d, fuzz;

	if (XQueryPointer (u->dpy, u->win, &root, &child, &rootX, &rootY,
		&x, &y, &mask) == True) {

		if (x >= 0 && y >= 0 && x < u->width && y < u->height) {

			fuzz = u->scaleFactor * 8.0;
			x = x - u->xCenter;
			y = y - u->yCenter;
			d = sqrt ((double)(x*x + y*y));

			if (d > fuzz) {
				c->Sa=(double) x / (double) u->xCenter * (d-fuzz) / d;
				c->Se=(double) y / (double) u->yCenter * (d-fuzz) / d;
#ifndef LINEAR_CONTROL_RESPONSE
				if (c->Sa < 0.0)
					c->Sa =  - c->Sa * c->Sa;
				else
					c->Sa = c->Sa * c->Sa;

				if (c->Se < 0.0)
					c->Se = - c->Se * c->Se;
				else
					c->Se = c->Se * c->Se;
#endif
			}
			else {
				c->Sa = 0.0;
				c->Se = 0.0;
			}
			return 1;

		}
	}

	return 0;
}
