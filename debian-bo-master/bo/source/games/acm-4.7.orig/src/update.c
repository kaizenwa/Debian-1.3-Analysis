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
 
#include "pm.h"
#include "alarm.h"
#include <X11/Xutil.h>

#ifdef REAL_DELTA_T
#include <sys/time.h>

double lastTime = 0.0, sum = 0.0;
long frameCount=0;

#endif

double gameEmptyTime = 0.0;


extern int flightCalculations();
extern int missileCalculations();
extern void doEvents(), doViews(), flapControl(), resupplyCheck();
extern void alarmCheck();
extern void blackBoxInput(), blackBoxOutput();

int cur = 0;
static int warned = 0;

int
redraw()
{

	int	i, gameEmpty;
	craft	*p;
#ifdef REAL_DELTA_T
	struct timeval tp;
	struct timezone tzp;
	double	thisTime;
#endif

	gameEmpty = 1;

	for ((i=0, p=ptbl); i<MAXPLAYERS; (++i, ++p)) {
		if ((p->type == CT_PLANE) && !(p->flags & FL_BLACK_BOX)) {
			gameEmpty = 0;
			doEvents (p);

			if (flightCalculations (p) == 1) {
				killPlayer (p);
			}
			doWeaponUpdate (p);
			flapControl (p);
		}
	}

	for ((i=0, p=ptbl); i<MAXPLAYERS; (++i, ++p)) {
		if ((p->type == CT_DRONE) && !(p->flags & FL_BLACK_BOX)) {

			if (droneCalculations (p) == 1) {
				killPlayer (p);
			}

/*
 *  Calling flightCalculations makes drones adhere to the flight model.
 */
			if (flightCalculations (p) == 1) {
				killPlayer (p);
			}
			doWeaponUpdate (p);
			flapControl (p);
		}
	}

	for ((i=0, p=ctbl); i<MAXPLAYERS; (++i, ++p)) {
	  if (p->type == CT_FREE) continue;
	  doEvents(p);
	}

	blackBoxInput();

	for ((i=0, p=mtbl); i<MAXPROJECTILES; (++i, ++p)) {
		if (p->type == CT_MISSILE) {
			if (missileCalculations (p) == 1)
				killMissile (p);
		}
		else if (p->type == CT_CANNON) {
			if (cannonCalculations (p) == 1)
				killMissile (p);
		}
		else if (p->type == CT_EXPLOSION) {
			-- (p->flameDuration);
			if ((-- p->duration) <= 0)
				p->type = CT_FREE;
		}
	}

	lookForImpacts ();

#ifdef REAL_DELTA_T
	doViews();
#else
	if (cur++ % REDRAW_EVERY == 0)
		doViews ();
#endif

	blackBoxOutput();

	alarmCheck (deltaT);

#ifdef REAL_DELTA_T
	gettimeofday (&tp, &tzp);
	thisTime = (double) tp.tv_sec + (double) tp.tv_usec / 1000000.0;

/*
 *  Check if "thisTime" ever equals "lastTime", if so, warn that REAL_DELTA_T
 *  probably should not be used on this system.
 */

	if (thisTime == lastTime) {
		thisTime += 0.0001;
		if (warned) {
		    warned = 1;
		    fprintf (stderr,
"Hmm. We seem to have performed a complete inner loop in zero elapsed time.\n\
This seems suspicious.  Perhaps gettimeofday() has a relatively coarse\n\
granularity on this system.\n\
This server should probably be recompiled without REAL_DELTA_T defined.\n");
		}
	}

	deltaT = thisTime - lastTime;
	sum += deltaT;
#ifdef WATCH_FRAME_RATE
	frameCount++;
	if (frameCount % 100 == 0) {
		printf ("rate is %lf frames/second\n", (double)frameCount/sum );
		sum = deltaT;
		frameCount = 1;
	}
#endif

/*
 *  If the amount of real elapsed time is unreasonably long, then
 *  make it one update interval.
 */

	if (deltaT > (double)((UPDATE_INTERVAL / 1000000.0) * 4)) {
		deltaT = UPDATE_INTERVAL / 1000000.0;
	}

	halfDeltaTSquared = 0.5 * deltaT * deltaT;
	lastTime = thisTime;
#endif

	curTime += deltaT;
	if (gameEmpty) {
		if ((gameEmptyTime += deltaT) > MAX_GAME_IDLE_SECONDS) {
			exit (0);
		}
	}
	else {
		gameEmptyTime = 0.0;
	}

	return 0;

}
