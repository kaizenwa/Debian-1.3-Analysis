/*	
 *   xtel - Emulateur MINITEL sous X11
 *
 *   Copyright (C) 1991-1994  Lectra Systemes & Pierre Ficheux
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
static char rcsid[] = "$Id: xvdt.c,v 1.3 1996/09/22 16:56:29 pierre Exp $";

/*
 * Test du widget Videotex
 */
 
#include <stdio.h>
#include <fcntl.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>

#include "Videotex.h"

Widget toplevel, form, vdts;
XtAppContext app_context;

static void Fin (w, client_data, pevent)
Widget w;
caddr_t client_data;
XButtonEvent *pevent;
{
    exit (0);
}

static void Decode (w, client_data, pevent)
Widget w;
caddr_t client_data;
XButtonEvent *pevent;
{
    char c;
    int fd;

    fd = open (client_data, O_RDONLY);
    
    if (fd > 0) {
	XtVaSetValues (w, XtNfdConnexion, fd, NULL);
	videotexDecode (w, 12);
	while (read (fd, &c, 1) > 0)
	    videotexDecode (w, c);
	close (fd);
    }
    else {
	perror (client_data);
	exit (1);
    }
}

main (ac, av)
int ac;
char **av;
{
    if (ac == 1) {
	fprintf (stderr, "Usage: xvdt fichier_videotex\n");
	exit (1);
    }

    toplevel = XtAppInitialize(&app_context, "XVdt", NULL, 0, &ac, av, NULL, NULL, 0); 
    form = XtCreateManagedWidget ("form", formWidgetClass, toplevel, NULL, 0);
    vdts = XtCreateManagedWidget ("vdts", videotexWidgetClass, form, NULL, 0);

    XtAddEventHandler (vdts, ExposureMask, False, Decode, (caddr_t)av[1]);
    XtAddEventHandler (vdts, ButtonPressMask, False, Fin, NULL);

    XtRealizeWidget(toplevel);
    XtAppMainLoop (app_context);
}
