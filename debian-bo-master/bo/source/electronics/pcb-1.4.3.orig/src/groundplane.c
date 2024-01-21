/*
 *                            COPYRIGHT
 *
 *  PCB, interactive printed circuit board design
 *  Copyright (C) 1994,1995,1996 Thomas Nau
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Contact addresses for paper mail and Email:
 *  Thomas Nau, Schlehenweg 15, 88471 Baustetten, Germany
 *  Thomas.Nau@rz.uni-ulm.de
 *
 */

static	char	*rcsid = "$Id: groundplane.c,v 143.1 1996/09/16 09:08:39 nau Exp $";

/* groundplane routines
 */

#include "global.h"

#include "data.h"
#include "groundplane.h"

/* ---------------------------------------------------------------------------
 * add all selected pins/vias to the groundplane
 * returns TRUE if objects are changed
 */
Boolean AddSelectedToGroundplane(void)
{
	Boolean		changed = False;

	VIA_LOOP(PCB->Data,
		if (TEST_FLAG(SELECTEDFLAG, via) && !TEST_FLAG(ONGROUNDPLANEFLAG, via))
		{
			changed = True;
			SET_FLAG(ONGROUNDPLANEFLAG, via);
		}
	);
	ALLPIN_LOOP(PCB->Data,
		if (TEST_FLAG(SELECTEDFLAG, pin) && !TEST_FLAG(ONGROUNDPLANEFLAG, pin))
		{
			changed = True;
			SET_FLAG(ONGROUNDPLANEFLAG, pin);
		}
	);
	return(changed);
}

/* ---------------------------------------------------------------------------
 * removes all selected pins/vias from the groundplane
 * returns TRUE if objects are changed
 */
Boolean RemoveSelectedFromGroundplane(void)
{
	Boolean		changed = False;

	VIA_LOOP(PCB->Data,
		if (TEST_FLAG(SELECTEDFLAG, via) && TEST_FLAG(ONGROUNDPLANEFLAG, via))
		{
			changed = True;
			CLEAR_FLAG(ONGROUNDPLANEFLAG, via);
		}
	);
	ALLPIN_LOOP(PCB->Data,
		if (TEST_FLAG(SELECTEDFLAG, pin) && TEST_FLAG(ONGROUNDPLANEFLAG, pin))
		{
			changed = True;
			CLEAR_FLAG(ONGROUNDPLANEFLAG, pin);
		}
	);
	return(changed);
}

/* ---------------------------------------------------------------------------
 * removes all pins/vias from the groundplane
 * returns TRUE if objects are changed
 */
Boolean ClearGroundplane(void)
{
	Boolean		changed = False;

	VIA_LOOP(PCB->Data,
		if (TEST_FLAG(ONGROUNDPLANEFLAG, via))
		{
			changed = True;
			CLEAR_FLAG(ONGROUNDPLANEFLAG, via);
		}
	);
	ALLPIN_LOOP(PCB->Data,
		if (TEST_FLAG(ONGROUNDPLANEFLAG, pin))
		{
			changed = True;
			CLEAR_FLAG(ONGROUNDPLANEFLAG, pin);
		}
	);
	return(changed);
}

/* ---------------------------------------------------------------------------
 * selects all memebers of the groundplane
 * returns TRUE if objects are changed
 */
Boolean SelectGroundplaneMember(void)
{
	Boolean		changed = False;

	VIA_LOOP(PCB->Data,
		if (TEST_FLAG(ONGROUNDPLANEFLAG, via) && !TEST_FLAG(SELECTEDFLAG, via))
		{
			changed = True;
			SET_FLAG(SELECTEDFLAG, via);
		}
	);
	ALLPIN_LOOP(PCB->Data,
		if (TEST_FLAG(ONGROUNDPLANEFLAG, pin) && !TEST_FLAG(SELECTEDFLAG, pin))
		{
			changed = True;
			SET_FLAG(SELECTEDFLAG, pin);
		}
	);
	return(changed);
}

