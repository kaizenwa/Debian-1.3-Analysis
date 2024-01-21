/* xpm2pixmap.h - xtron v1.1 header for xpm2pixmap.c
 *
 *   Copyright (C) 1995 Rhett D. Jacobs <rhett@hotel.canberra.edu.au>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 1, or (at your option)
 *  any later version.
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
 *  Last Modified: 16/4/95
 */

#ifndef _XPM2PIXMAP_H
#define _XPM2PIXMAP_H

#include <stdio.h>
#include <string.h>
#include <xpm.h>

#include "resource.h"

#ifndef MAX_PATH_LEN
#define MAX_PATH_LEN 1024
#endif

Pixmap ReadXPM(char *filename);

#endif
