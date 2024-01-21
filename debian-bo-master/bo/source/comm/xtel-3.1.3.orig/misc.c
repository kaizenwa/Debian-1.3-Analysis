/*	
 *   xtel - Emulateur MINITEL sous X11
 *
 *   Copyright (C) 1991-1996  Lectra Systemes & Pierre Ficheux
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
static char rcsid[] = "$Id: misc.c,v 1.3 1996/10/28 13:37:19 pierre Exp $";

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static char buf[256];

/* 
 * Construit un nom codee ISO-8859 a partir de sequences ASCII
 * ex: T\351l\351tel --> Télétel
 */
char *build_name (s)
char *s;
{
    register int i = 0, ii = 0;
    int x;
    char *ss;

    memset (buf, 0, 256);
    
    while (*(s+i)) {
	if (*(s+i) == '\\') {
	    sscanf (s+i+1, "%o", &x);
	    buf[ii++] = (char)x;
	    i += 4;
	}
	else {
	    buf[ii++] = *(s+i);
	    i++;
	}
    }
    
    ss = calloc (1, i+1);
    strcpy (ss, buf);
    
    return ss;
}

/* Retourne le token suivant */
char *next_token (s, tok)
char *s, *tok;
{
    char *pt = strtok (s, tok);
    if (pt)
	return pt;
    else 
	return "";
}
