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
static char rcsid[] = "$Id: copyright.c,v 1.6 1996/10/04 17:03:26 pierre Exp $";

#include "xtel.h"
#include "patchlevel.h"
#include <X11/Shell.h>

#ifndef NO_XPM
#define H_COPYRIGHT	260
#else
#include "bitmaps/pf.bit"
#define H_COPYRIGHT	300
#endif
#define W_COPYRIGHT	320

#ifdef NO_NETWORK
static char *sockets = "locales";
#else
static char *sockets = "Internet";
#endif /* NO_NETWORK */
static Display *display;
static GC gc_copyright;
static int screen;
static Widget popup_copyright, forme_copyright;
static Pixmap pixmap_pf;

static XFontStruct *load_query_font (nom)
char *nom;
{
    XFontStruct *xfs;

    if ((xfs = XLoadQueryFont (display, nom)) == NULL) {
	fprintf (stderr, "fonte \"%s\" introuvable, remplacee par \"fixed\"...\n", nom);
	xfs = XLoadQueryFont (display, "fixed");
    }

    return (xfs);
}

/* Affiche une chaine */
static void affiche_chaine_centree (w, s, xfs, y)
Widget w;
char *s;
XFontStruct *xfs;
int y;
{
    int x;

    XSetFont (display, gc_copyright, xfs->fid);
    x = (W_COPYRIGHT - XTextWidth (xfs, s, strlen(s))) / 2;
    XDrawString (display, XtWindow(w), gc_copyright, x, y, s, strlen(s));
}

/* Affiche et Efface le copyright */
static void Expose_copyright (w, client_data, pevent)
Widget w;
XtPointer client_data;
XExposeEvent *pevent;
{
    char buf[256];
    XFontStruct *xfs_titre, *xfs_6x10, *xfs_9x15;
    int y = 0, on_s_en_fout;
    unsigned int w_pf, h_pf;
    Window root;

    /*
     * Affiche les infos dont la photo de l'auteur pour les filles... 
     */
    XSetFillStyle (display, gc_copyright, FillSolid);

    xfs_titre = load_query_font ("-adobe-times-bold-r-*-*-*-240-100-100-p-*-*-*");
    xfs_6x10 = load_query_font ("6x10");
    xfs_9x15 = load_query_font ("9x15");
    
    if (prototype_xtel == 0) {
	if (PATCHLEVEL != 0)
	    sprintf (buf, "XTel %d.%d.%d", version_xtel, revision_xtel, PATCHLEVEL);
	else
	    sprintf (buf, "XTel %d.%d", version_xtel, revision_xtel);
    }
    else {
	if (PATCHLEVEL != 0)
	    sprintf (buf, "XTel %d.%d.%db%d [beta]", version_xtel, revision_xtel, PATCHLEVEL, prototype_xtel);
	else
	    sprintf (buf, "XTel %d.%db%d [beta]", version_xtel, revision_xtel, prototype_xtel);
    }

    y += 30;
    affiche_chaine_centree (w, buf, xfs_titre, y);
    y += 25;
    affiche_chaine_centree (w, "par", xfs_6x10, y);
    y+= 15;

#if defined(NO_XPM)
    XSetFillStyle (display, gc_copyright, FillOpaqueStippled);
    XSetStipple (display, gc_copyright, pixmap_pf);
    XSetTSOrigin (display, gc_copyright, (W_COPYRIGHT - pf_width)/2, y);
    XFillRectangle (display, XtWindow(w), gc_copyright, (W_COPYRIGHT - pf_width)/2, y, pf_width, pf_height);
    XSetFillStyle (display, gc_copyright, FillSolid);
    y += (pf_height + 20);
#else
    XGetGeometry (display, pixmap_pf, &root, &on_s_en_fout, &on_s_en_fout, &w_pf, &h_pf, (unsigned int*)&on_s_en_fout, (unsigned int*)&on_s_en_fout);    
    XCopyArea (display, pixmap_pf, XtWindow(w), gc_copyright, 0, 0, w_pf, h_pf, (W_COPYRIGHT - w_pf)/2, y);
    y += (h_pf + 20);
#endif

    affiche_chaine_centree (w, "Pierre Ficheux", xfs_9x15, y);
    y += 20;
    affiche_chaine_centree (w, "Lectra Systèmes S.A.", xfs_9x15, y);
    y += 20;
    affiche_chaine_centree (w, "(pierre@lectra.com)", xfs_6x10, y);
    y += 30;
    sprintf (buf, "toolkit %s, sockets %s", toolkit_utilise, sockets);
    affiche_chaine_centree (w, buf, xfs_6x10, y);
    y += 20;
    affiche_chaine_centree (w, "- clic ! -", xfs_9x15, y);

    XFreeFont (display, xfs_titre);
    XFreeFont (display, xfs_6x10);
    XFreeFont (display, xfs_9x15);
}

static void Efface_copyright (w, client_data, pevent)
Widget w;
XtPointer client_data;
XButtonEvent *pevent;
{
    if (flag_copyright_affiche) {
	XtPopdown (popup_copyright);
	XtDestroyWidget (popup_copyright);
	XtDestroyWidget (forme_copyright);
	XFreeGC (display, gc_copyright);
	XFreePixmap (display, pixmap_pf);
	flag_copyright_affiche = False;
    }
}

/*
 * Fonction publique d'affichage
 */
affiche_copyright (w, classe_forme, pixmap)
Widget w;
WidgetClass classe_forme;
Pixmap pixmap;
{
    pixmap_pf = pixmap;
    popup_copyright = XtCreatePopupShell ("Information", applicationShellWidgetClass, w, NULL, 0);
    forme_copyright = XtVaCreateManagedWidget ("forme_copyright", classe_forme, popup_copyright, XtNwidth, W_COPYRIGHT, XtNheight, H_COPYRIGHT, NULL);

    XtPopup (popup_copyright, XtGrabNone);

    display = XtDisplay (forme_copyright);
    screen = DefaultScreen (display);

    /* GC pour le copyright */
    gc_copyright = XCreateGC (display, XtWindow(forme_copyright), 0, NULL); 
    XCopyGC (display, DefaultGC (display, screen), ~0, gc_copyright); 
    XSetForeground (display, gc_copyright, BlackPixel (display, screen));
    XSetBackground (display, gc_copyright, WhitePixel (display, screen));

    flag_copyright_affiche = True;

    XtAddEventHandler (forme_copyright, ButtonPressMask, False, (XtEventHandler)Efface_copyright, NULL);
    XtAddEventHandler (forme_copyright, ExposureMask, False, (XtEventHandler)Expose_copyright, (XtPointer)NULL);
}
