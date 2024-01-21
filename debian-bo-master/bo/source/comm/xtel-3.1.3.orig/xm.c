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
static char rcsid[] = "$Id: xm.c,v 1.11 1997/01/08 15:03:18 pierre Exp $";

/*
 * Couche Toolkit OSF-MOTIF
 */

#include "xtel.h"
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/RowColumn.h>
#include <Xm/SelectioB.h>
#include <Xm/FileSB.h>
#include <Xm/MessageB.h>
#include <Xm/CascadeB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>

#ifndef NO_XPM
#include <X11/xpm.h>
#include "pixmaps/pf.xpm"
#else
#include "bitmaps/pf.bit"
#endif /* NO_XPM */

#ifndef LOW_MEMORY

#include "bitmaps/unsensitiveh.xbm"
#include "bitmaps/unsensitiveb.xbm"

#include "pixmaps/rewind.xpm"
#include "pixmaps/prev.xpm"
#include "pixmaps/stop.xpm"
#include "pixmaps/play.xpm"
#include "pixmaps/next.xpm"
#include "pixmaps/disk_write.xpm"
#include "pixmaps/disk_read.xpm"
#include "pixmaps/printera.xpm"
#include "pixmaps/printer.xpm"
#include "pixmaps/door_run.xpm"
#include "pixmaps/nouveau.xpm"
#include "pixmaps/camera.xpm"
#include "pixmaps/film.xpm"
#include "pixmaps/phone.xpm"
#include "pixmaps/cassete.xpm"

#include "pixmaps/envoi.xpm"
#include "pixmaps/retour.xpm"
#include "pixmaps/annul.xpm"
#include "pixmaps/guide.xpm"
#include "pixmaps/repet.xpm"
#include "pixmaps/sommaire.xpm"
#include "pixmaps/suite.xpm"
#include "pixmaps/correc.xpm"
#include "pixmaps/cxfin.xpm"

#include "pixmaps/envoi_on.xpm"
#include "pixmaps/retour_on.xpm"
#include "pixmaps/annul_on.xpm"
#include "pixmaps/guide_on.xpm"
#include "pixmaps/repet_on.xpm"
#include "pixmaps/sommaire_on.xpm"
#include "pixmaps/suite_on.xpm"
#include "pixmaps/correc_on.xpm"
#include "pixmaps/cxfin_on.xpm"

#include "Timer.h"
#include "XpmButton.h"

#endif /* !LOW_MEMORY */

#include "bitmaps/teleph.xbm"
#include "bitmaps/aide.bit"
#include "bitmaps/lecteur.bit"
#include "bitmaps/zero.bit"
#include "bitmaps/wind.bit"
#include "bitmaps/rewind.bit"
#include "bitmaps/play.bit"
#include "bitmaps/stop.bit"

#define XmStringDefCreate(s) XmStringLtoRCreate((s),XmSTRING_DEFAULT_CHARSET)
#define K_Charge	0
#define K_Sauve		1

static char buf[256];
static Arg args[10];
static int n;
static char *toolkit = "OSF/Motif";

static Widget lecteur, compteur_lecteur_page, compteur_lecteur_caractere;
static Widget stop_lecteur, play_lecteur, bouton_lecteur;
static Widget boite_haut;
static Widget rc_lecteur, menu_protocole, bouton_aide, menu_aide;
static Widget quitte_lecteur;
static Widget compteur_lecteur;
static Widget compteur_lecteur_titre;
static Widget menu_type_arret_page;
static Widget clavier_lecteur, btn_lecteur;
static Widget fem, em, top;
static Widget forme_base, paned_base, menu_compose, menu_procedure, menu_impression;
static Widget selecteur_fichier, boite_saisie, confirmation_sortie, message_erreur;
static Widget barre_menu, menu_fichier, menu_enregistrement;


static void Ouverture_boite_saisie ();
static void Validation_selecteur_fichier ();
static void Composition_service ();
static void Appel_procedure ();
static void Ouverture_selecteur_fichier ();
static void Annulation_selecteur_fichier ();
static void Validation_boite_saisie ();
static void Annulation_boite_saisie ();
static void Ouverture_confirmation_sortie ();
static void Validation_confirmation_sortie ();
static void Annulation_confirmation_sortie ();
static void Validation_message_erreur ();
static void Affiche_menu_clavier ();
static void Valide_le_bouton_stop();
static void Affiche_copyright ();

/* A DEPLACER */
Widget la_cassete;

#if XmREVISION > 1
#define XTPOINTER	XtPointer
#else
#define XTPOINTER	caddr_t
#endif

static XtActionsRec actions_touches[] = {
    {"affiche_menu_clavier", Affiche_menu_clavier}
  };

static XrmOptionDescRec options[] = {
    {"-serveur",	"*serveur",		XrmoptionSepArg, NULL},
    {"-service",	"*nomService",		XrmoptionSepArg, NULL},
    {"-petit",		"*petiteFonte",		XrmoptionNoArg, "True"},
    {"-ng",		"*nGris",		XrmoptionNoArg, "True"},
    {"-br",		"*boutonRaccrocher",	XrmoptionNoArg, "True"},
    {"-low",		"*lowMemory",		XrmoptionNoArg, "True"},
};

/* Callback de selection du type d'arret */
static void menu_type_arret_page_cb (w, choice, call_data)
Widget w;
int choice;
XtPointer call_data;
{
    flag_arret_a_chaque_page = True;

    switch (choice) {
      case 0 :
	  flag_arret_a_chaque_page = False;
	  break;
	  
	case 1 :
	   type_de_fin_page = ARRET_FF;
	  break;

	case 2 :
	   type_de_fin_page = ARRET_SUITE;
	  break;

	case 3 :
	   type_de_fin_page = ARRET_SUITE | ARRET_FF;
	  break;
	    
	default :
	    break;
      }
}


/*
 * Creation d'une entree menu (type PushButtonWidget)
 */
static Widget create_menu_entry (parent, name, sensitive, cb, call_data)
Widget parent;
char *name;
Boolean sensitive;
void *cb;
XTPOINTER call_data;
{
    Widget entry;

    entry = XtVaCreateManagedWidget (name, xmPushButtonWidgetClass, parent, XtNsensitive, sensitive, NULL);
    XtAddCallback (entry, XmNactivateCallback, (XtCallbackProc)cb, call_data);
    
    return (entry);
}

static Widget create_bitmap_label (parent, name, bits, width, height)
Widget parent;
char *name;
char *bits;
int width, height;
{
    Widget w;
    Pixmap pixmap;
    Display *dpy = XtDisplay(parent);
    int screen = DefaultScreen (dpy);
    long bg;

    XtVaGetValues (parent, XtNbackground, &bg, NULL);
    if ((pixmap = XCreatePixmapFromBitmapData (dpy, DefaultRootWindow(dpy), bits, width, height, BlackPixel (dpy, screen), bg, DisplayPlanes(dpy, screen)))) {
	w = XtVaCreateManagedWidget (name, xmLabelWidgetClass, parent, XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap, XtNmappedWhenManaged, False, NULL);
	return (w);
    }
    else {
	fprintf (stderr, "create_bitmap_label: %s: can't create X bitmap\n", name);
	return (NULL);
    }
}

#ifndef NO_XPM
/*
 * PushButton avec Pixmap XPM
 */
static Widget create_pixmap_button (parent, name, sensitive, class, arm_cb, disarm_cb, arm_call_data, disarm_call_data, xpm_data, unsensitive_xpm_data)
Widget parent;
char *name;
Boolean sensitive;
WidgetClass class;
void *arm_cb, *disarm_cb;
XTPOINTER arm_call_data, disarm_call_data;
char *xpm_data[], *unsensitive_xpm_data[];
{
    Widget w;
    Pixmap pixmap, unsensitive_pixmap;

    XpmCreatePixmapFromData (XtDisplay(parent), DefaultRootWindow(XtDisplay(parent)), xpm_data, &pixmap, NULL, NULL);
    if (pixmap) {
	w = XtVaCreateManagedWidget (name, class, parent, XmNsensitive, sensitive, XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap, NULL);
	if (unsensitive_xpm_data) {
	    XpmCreatePixmapFromData (XtDisplay(parent), DefaultRootWindow(XtDisplay(parent)), unsensitive_xpm_data, &unsensitive_pixmap, NULL, NULL);
	    XtVaSetValues (w, XmNlabelInsensitivePixmap, unsensitive_pixmap, NULL);
	}

	if (arm_cb)
	    XtAddCallback (w, XmNarmCallback, (XtCallbackProc)arm_cb, arm_call_data);
	if (disarm_cb)
	    XtAddCallback (w, XmNdisarmCallback, (XtCallbackProc)disarm_cb, disarm_call_data);

	return (w);
    }
    else {
	fprintf (stderr, "create_pixmap_button: %s: can't create Xpm pixmap\n", name);
	return (NULL);
    }
}
#endif

#ifndef LOW_MEMORY
static Widget create_xpm_button (parent, name, class, cb, call_data, pixmap1_data, pixmap2_data, shape1_data, shape2_data, unsensitive_bitmap)
Widget parent;
char *name;
WidgetClass class;
void *cb;
XtPointer call_data;
char *pixmap1_data[], *pixmap2_data[];
char *shape1_data[], *shape2_data[];
Pixmap unsensitive_bitmap;
{
    Widget w;
    Pixmap pixmap1 = None, pixmap2 = None, shape1 = None, shape2 = None;

    if (pixmap1_data)
      XpmCreatePixmapFromData (XtDisplay(parent), DefaultRootWindow(XtDisplay(parent)), pixmap1_data, &pixmap1, &shape1, NULL);
    if (pixmap2_data)
      XpmCreatePixmapFromData (XtDisplay(parent), DefaultRootWindow(XtDisplay(parent)), pixmap2_data, &pixmap2, &shape2, NULL);

    w = XtVaCreateManagedWidget (name, class, parent, XtNpixmap1, pixmap1, XtNpixmap2, pixmap2, XtNshape1, shape1, XtNshape2, shape2, XtNunsensitiveBitmap, unsensitive_bitmap, NULL);

    if (cb)
      XtAddCallback (w, XtNcallback, (XtCallbackProc)cb, call_data);

    return (w);
}
#endif /* !LOW_MEMORY */

/*
 * PushButton avec Bitmap X
 */
static Widget create_bitmap_button (parent, name, class, arm_cb, disarm_cb, arm_call_data, disarm_call_data, bits, width, height)
Widget parent;
char *name;
WidgetClass class;
void *arm_cb, *disarm_cb;
XTPOINTER arm_call_data, disarm_call_data;
char *bits;
int width, height;
{
    Widget w;
    Pixmap pixmap;
    Display *dpy = XtDisplay(parent);
    int screen = DefaultScreen (dpy);
    long fg, bg;

    XtVaGetValues (parent, XtNbackground, &bg, XtNforeground, &fg, NULL);
    if ((pixmap = XCreatePixmapFromBitmapData (dpy, DefaultRootWindow(dpy), bits, width, height, fg, bg, DisplayPlanes(dpy, screen)))) {
	w = XtVaCreateManagedWidget (name, class, parent, XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap, XtNforeground, fg, XtNbackground, bg, NULL);
	if (arm_cb)
	    XtAddCallback (w, XmNarmCallback, (XtCallbackProc)arm_cb, arm_call_data);
	if (disarm_cb)
	    XtAddCallback (w, XmNdisarmCallback, (XtCallbackProc)disarm_cb, disarm_call_data);

	return (w);
    }
    else {
	fprintf (stderr, "create_bitmap_button: %s: can't create bitmap\n", name);
	return (NULL);
    }
}

/* 
 * Affichage du popup-menu clavier 
 */
void Affiche_menu_clavier (w, pevent, params, nb_params)
Widget w;
XButtonEvent *pevent;
String *params;
Cardinal nb_params;
{
    XmMenuPosition(menu_clavier, pevent);
    XtManageChild (menu_clavier);
}

/*
 * Initialisation du toolkit 
 */
Widget init_toolkit (pargc, argv)
int *pargc;
char **argv; 
{
#ifdef DEBUG
    printf ("Initialisation du toolkit\n");
#endif

    toolkit_utilise = toolkit;

    top = XtAppInitialize(&app_context, "XTelm", options, XtNumber(options), pargc, argv, NULL, NULL, 0);

    XtAppAddActions(app_context, actions_touches, XtNumber(actions_touches));

    return (top);
}

/*
 * Initialisation des widgets
 */
Widget init_widgets (topLevel)
Widget topLevel;
{
    register int i;
    Widget entry;
#ifndef LOW_MEMORY
    Pixmap unsensitive_pixmap_h, unsensitive_pixmap_b;
#endif

#ifdef DEBUG
    printf ("Initialisation des widgets\n");
#endif

    low_memory = rsc_xtel.lowMemory;

    /* Contenant de base */
    forme_base = XtVaCreateManagedWidget ("forme_base", xmRowColumnWidgetClass, topLevel, XmNorientation, XmVERTICAL, XmNentryAlignment, XmALIGNMENT_CENTER, NULL);


    /* Barre de menu */
    n = 0;
    XtSetArg (args[n], XmNmenuHelpWidget, bouton_aide); n++;
    barre_menu = XmCreateMenuBar (forme_base, "barre_menu", args, n);

    /* Menu "Fichier" */
    menu_fichier = XmCreatePulldownMenu (barre_menu, "menu_fichier", NULL, 0);
    menu_impression = XmCreatePulldownMenu (topLevel, "menu_impression", NULL, 0);
    XtVaCreateManagedWidget ("bouton_fichier", xmCascadeButtonWidgetClass, barre_menu, XmNsubMenuId, menu_fichier, NULL);
    create_menu_entry (menu_fichier, "bouton_sauve", True, Ouverture_selecteur_fichier, (XTPOINTER)K_Sauve);
    entry = create_menu_entry (menu_fichier, "bouton_charge", True, Ouverture_selecteur_fichier, (XTPOINTER)K_Charge);
    XtVaCreateManagedWidget ("bouton_imprime", xmCascadeButtonWidgetClass, menu_fichier, XmNsubMenuId, menu_impression, NULL);
    create_menu_entry (menu_impression, "bouton_impression_ascii", True, imprime_page_courante_ascii, NULL);
    create_menu_entry (menu_impression, "bouton_impression_videotex", True, imprime_page_courante_videotex, NULL);
    XtVaCreateManagedWidget ("separateur_fichier", xmSeparatorGadgetClass, menu_fichier, NULL);
    create_menu_entry (menu_fichier, "bouton_quitte", True, Ouverture_confirmation_sortie, NULL);

    /* Menu "Enregistrement" */
    menu_enregistrement = XmCreatePulldownMenu (barre_menu, "menu_enregistrement", NULL, 0);
    XtVaCreateManagedWidget ("bouton_enregistrement", xmCascadeButtonWidgetClass, barre_menu, XmNsubMenuId, menu_enregistrement, NULL);
    create_menu_entry (menu_enregistrement, "bouton_nouveau", True, nouvel_enregistrement, NULL);
    bouton_demarre = create_menu_entry (menu_enregistrement, "bouton_demarre", True, demarre_enregistrement, NULL);
    bouton_arrete = create_menu_entry (menu_enregistrement, "bouton_arrete", False, arrete_enregistrement, NULL);
    bouton_lecteur = create_menu_entry (menu_enregistrement, "bouton_lecteur", False, debut_lecteur, NULL);

    /* Creation du menu "Services" */
    if (nb_services != 0) {
	menu_compose = XmCreatePulldownMenu (barre_menu, "menu_compose", NULL, 0);
	bouton_compose = XtVaCreateManagedWidget ("bouton_compose", xmCascadeButtonWidgetClass, barre_menu, XmNsubMenuId, menu_compose, NULL);
	
	for (i = 0 ; i != nb_services ; i++) {
	    n = 0;
	    XtSetArg (args[n], XmNlabelString, XmStringCreate (entree_compose[i], XmSTRING_DEFAULT_CHARSET)); n++;
	    entry = XtVaCreateManagedWidget (entree_compose[i], xmPushButtonGadgetClass, menu_compose, NULL);
	    if (i == nb_services-1 && !strcmp (entree_compose[i], "Direct"))
		XtAddCallback(entry, XmNactivateCallback, Ouverture_boite_saisie, NULL);
	    else
		XtAddCallback(entry, XmNactivateCallback, Composition_service, (XTPOINTER)i);
	    
	}
    }

    /* Creation du menu Procedures */
    if (nb_procedures != 0) {
	menu_procedure = XmCreatePulldownMenu (barre_menu, "menu_procedure", NULL, 0);
	bouton_procedure = XtVaCreateManagedWidget ("bouton_procedure", xmCascadeButtonWidgetClass, barre_menu, XmNsubMenuId, menu_procedure, NULL);
	for (i = 0 ; i != nb_procedures ; i++) {
	    entry = create_menu_entry (menu_procedure, "proc", True, Appel_procedure, (XTPOINTER)i);
	    XtVaSetValues (entry, XmNlabelString, XmStringCreate (procedures[i].nom, XmSTRING_DEFAULT_CHARSET), NULL);
	}
    }

    /* Menu des protocoles */
    if (nb_protocoles) {
	menu_protocole = XmCreatePulldownMenu (barre_menu, "menu_protocole", NULL, 0);
	bouton_proto = XtVaCreateManagedWidget ("bouton_proto", xmCascadeButtonWidgetClass, barre_menu, XmNsubMenuId, menu_protocole, NULL);
	for (i = 0 ; i != nb_protocoles ; i++) {
	    entry = create_menu_entry (menu_protocole, "proto", True, Selection_telechargement, (XTPOINTER)i);
	    XtVaSetValues (entry, XmNlabelString, XmStringCreate (protocoles[i].nom, XmSTRING_DEFAULT_CHARSET), NULL);
	}

	XtVaCreateManagedWidget ("blanc_tele", xmSeparatorGadgetClass, menu_protocole, NULL);
	/* Bouton d'interruption de protocole */
	stop_tele = create_menu_entry (menu_protocole, "stop_tele", True, Stop_telechargement, (XTPOINTER)NULL);
    }

    /* Aide */
    menu_aide = XmCreatePulldownMenu (barre_menu, "menu_aide", NULL, 0);
    bouton_aide = create_bitmap_button (barre_menu, "bouton_aide", xmCascadeButtonWidgetClass, NULL, NULL, NULL, NULL, aide_bits, aide_width, aide_height);
    XtVaSetValues (bouton_aide, XmNsubMenuId, menu_aide, NULL);
    create_menu_entry (menu_aide, "a_propos", True, Affiche_copyright, NULL);

    XtVaSetValues (barre_menu, XmNmenuHelpWidget, bouton_aide, NULL);
    XtManageChild (barre_menu);

    paned_base = XtVaCreateManagedWidget ("paned_base", xmRowColumnWidgetClass, forme_base, XmNorientation, (rsc_xtel.petiteFonte ? XmHORIZONTAL : XmVERTICAL), XmNentryAlignment, XmALIGNMENT_CENTER, NULL);

#ifndef LOW_MEMORY
    if (!low_memory) {
	Pixmap pixmap_k7;

	/* Calcul des Pixmaps de masquage des boutons */
	unsensitive_pixmap_h = XCreateBitmapFromData (XtDisplay(topLevel), DefaultRootWindow(XtDisplay(topLevel)), unsensitiveh_bits, unsensitiveh_width, unsensitiveh_height);
	unsensitive_pixmap_b = XCreateBitmapFromData (XtDisplay(topLevel), DefaultRootWindow(XtDisplay(topLevel)), unsensitiveb_bits, unsensitiveb_width, unsensitiveb_height);

	if (rsc_xtel.petiteFonte)
	    boite_haut = XtVaCreateManagedWidget ("boite_haut", xmRowColumnWidgetClass, paned_base, XmNorientation, XmVERTICAL, XmNnumColumns, 2, XmNpacking, XmPACK_COLUMN, NULL);
	else
	    boite_haut = XtVaCreateManagedWidget ("boite_haut", xmRowColumnWidgetClass, paned_base, XmNorientation, XmHORIZONTAL, NULL);


	(void)create_xpm_button (boite_haut, "btn_sauve", xpmButtonWidgetClass, Ouverture_selecteur_fichier, (XTPOINTER)K_Sauve, disk_write_xpm, None, None, None, unsensitive_pixmap_h);    
	(void)create_xpm_button (boite_haut, "btn_charge", xpmButtonWidgetClass, Ouverture_selecteur_fichier, (XTPOINTER)K_Charge, disk_read_xpm, None, None, None, unsensitive_pixmap_h);    
	(void)create_xpm_button (boite_haut, "btn_imprime_ascii", xpmButtonWidgetClass, imprime_page_courante_ascii, NULL, printera_xpm, None, None, None, unsensitive_pixmap_h);    
	(void)create_xpm_button (boite_haut, "btn_imprime", xpmButtonWidgetClass, imprime_page_courante_videotex, NULL, printer_xpm, None, None, None, unsensitive_pixmap_h);    
	(void)create_xpm_button (boite_haut, "btn_nouveau", xpmButtonWidgetClass, nouvel_enregistrement, NULL, nouveau_xpm, None, None, None, unsensitive_pixmap_h);
	(void)create_xpm_button (boite_haut, "btn_enregistre", xpmButtonWidgetClass, change_enregistrement, NULL, camera_xpm, None, None, None, unsensitive_pixmap_h);
	btn_lecteur = create_xpm_button (boite_haut, "btn_lecteur", xpmButtonWidgetClass, debut_lecteur, NULL, film_xpm, None, None, None, unsensitive_pixmap_h);
	btn_direct = create_xpm_button (boite_haut, "btn_direct", xpmButtonWidgetClass, Ouverture_boite_saisie, NULL, phone_xpm, None, None, None, unsensitive_pixmap_h);
	(void)create_xpm_button (boite_haut, "btn_quitte", xpmButtonWidgetClass, Ouverture_confirmation_sortie, NULL, door_run_xpm, None, None, None, unsensitive_pixmap_h);

	/* Cassette */
	XpmCreatePixmapFromData (XtDisplay(boite_haut), DefaultRootWindow(XtDisplay(boite_haut)), cassete_xpm, &pixmap_k7, NULL, NULL);
	la_cassete = XtVaCreateManagedWidget ("la_cassete", xmLabelWidgetClass, boite_haut, XmNlabelType, XmPIXMAP, XmNmappedWhenManaged, False, XmNlabelPixmap, pixmap_k7, NULL);
	teleph = create_bitmap_label (boite_haut, "teleph", teleph_bits, teleph_width, teleph_height);
	/* Timer */
	if (!rsc_xtel.petiteFonte)
	    timer_minitel = XtVaCreateManagedWidget ("timer_minitel", timerWidgetClass, boite_haut, NULL);
    }
#endif

    /*
     * Creation du lecteur
     */
    n = 0;
    XtSetArg(args[n], XtNiconPixmap, XCreateBitmapFromData(XtDisplay(topLevel), XtScreen(topLevel)->root, lecteur_bits, lecteur_width, lecteur_height)); n++;
    XtSetArg(args[n], XmNtransient, False); n++;
    lecteur = XmCreateFormDialog (topLevel, "lecteur", args, n);
    
    n = 0;
    XtSetArg(args[n], XmNsensitive, True); n++;
    rc_lecteur = XmCreateRowColumn (lecteur, "rc_lecteur", args, n);
    XtManageChild (rc_lecteur);

    n = 0;
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
    clavier_lecteur = XmCreateRadioBox (rc_lecteur, "clavier_lecteur", args, n);
    XtManageChild (clavier_lecteur);

    /* Les boutons de commandes */
#ifndef LOW_MEMORY
    if (low_memory)
#endif /* !LOW_MEMORY */
	{
	    create_bitmap_button (clavier_lecteur, "rewind_lecteur", xmToggleButtonWidgetClass, Rewind_lecteur, Valide_le_bouton_stop, NULL, NULL, zero_bits, zero_width, zero_height);
	    create_bitmap_button (clavier_lecteur, "rewind_lecteur", xmToggleButtonWidgetClass, Rewind_lecteur, Valide_le_bouton_stop, NULL, NULL, rewind_bits, rewind_width, rewind_height);
	    stop_lecteur = create_bitmap_button (clavier_lecteur, "stop_lecteur", xmToggleButtonWidgetClass, Stop_lecteur, NULL, NULL, NULL, stop_bits, stop_width, stop_height);
	    play_lecteur = create_bitmap_button (clavier_lecteur, "play_lecteur", xmToggleButtonWidgetClass, Play_lecteur, NULL, NULL, NULL, play_bits, play_width, play_height);
	    create_bitmap_button (clavier_lecteur, "wind_lecteur", xmToggleButtonWidgetClass, Wind_lecteur, Valide_le_bouton_stop, NULL, NULL, wind_bits, wind_width, wind_height);
	}
#ifndef LOW_MEMORY
    else {
	create_pixmap_button (clavier_lecteur, "zero_lecteur", True, xmToggleButtonWidgetClass, Rewind_lecteur, Valide_le_bouton_stop, NULL, NULL, rewind_xpm, NULL);
	create_pixmap_button (clavier_lecteur, "rewind_lecteur", True, xmToggleButtonWidgetClass, Rewind_lecteur, Valide_le_bouton_stop, NULL, NULL, prev_xpm, NULL);
	stop_lecteur = create_pixmap_button (clavier_lecteur, "stop_lecteur", True, xmToggleButtonWidgetClass, Stop_lecteur, NULL, NULL, NULL, stop_xpm, NULL);
	play_lecteur = create_pixmap_button (clavier_lecteur, "play_lecteur", True, xmToggleButtonWidgetClass, Play_lecteur, NULL, NULL, NULL, play_xpm, NULL);
	create_pixmap_button (clavier_lecteur, "wind_lecteur", True, xmToggleButtonWidgetClass, Wind_lecteur, Valide_le_bouton_stop, NULL, NULL, next_xpm, NULL);
    }
#endif /* !LOW_MEMORY */

    XtVaSetValues (stop_lecteur, XmNset, True, NULL);

    /* Compteurs */
    n = 0;
    XtSetArg(args[n], XmNx, 10); n++;
    XtSetArg(args[n], XmNy, 40); n++;
    XtSetArg(args[n], XmNspacing, 10); n++;
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
    compteur_lecteur = XmCreateRowColumn (rc_lecteur, "compteur_lecteur", args, n);
    XtManageChild (compteur_lecteur);

    compteur_lecteur_titre = XmCreateLabel (compteur_lecteur, "compteur_lecteur_titre", NULL, 0);
    compteur_lecteur_caractere = XmCreateLabel (compteur_lecteur, "compteur_lecteur_caractere", NULL, 0);
    compteur_lecteur_page = XmCreateLabel (compteur_lecteur, "compteur_lecteur_page", NULL, 0);
    XtManageChild (compteur_lecteur_titre);
    XtManageChild (compteur_lecteur_caractere);
    XtManageChild (compteur_lecteur_page);

    /* Condition d'arret */
    menu_type_arret_page = XmVaCreateSimpleOptionMenu (rc_lecteur, "menu_type_arret_page", NULL, 0, 0, (XtCallbackProc)menu_type_arret_page_cb, XmVaPUSHBUTTON, NULL, 0, NULL, NULL, XmVaPUSHBUTTON, NULL, 0, NULL, NULL, XmVaPUSHBUTTON, NULL, 0, NULL, NULL, XmVaPUSHBUTTON, NULL, 0, NULL, NULL, NULL); 
    XtManageChild (menu_type_arret_page);

    /* Bouton 'Quitte' */
    n = 0;
    XtSetArg(args[n], XmNx, 10); n++;
    XtSetArg(args[n], XmNy, 120); n++;
    XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg(args[n], XmNtopWidget, rc_lecteur); n++;
    XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg(args[n], XmNbottomOffset, 10); n++;
    quitte_lecteur = XmCreatePushButton (lecteur, "quitte_lecteur", args, n);
    XtManageChild (quitte_lecteur);
    XtAddCallback(quitte_lecteur, XmNactivateCallback, (XtCallbackProc)fin_lecteur, NULL);

    /* 
     * Ecran Videotex 
     */
    entry = paned_base;
    n = 0;
    XtSetArg (args[n], XtNcommandeDeconnexion, CHAINE_COMMANDE_FIN); n++;

    if (rsc_xtel.nGris) {
	XtSetArg (args[n], XtNnGris, True);
	n++;
    }
    
    if (rsc_xtel.petiteFonte) {
	XtSetArg (args[n], XtNpetiteFonte, True);
	n++;
#ifndef LOW_MEMORY
	if (!low_memory)
	    entry = XtVaCreateManagedWidget ("pem", xmRowColumnWidgetClass, paned_base, XmNorientation, XmVERTICAL, NULL);
#endif
    }

    /* Id de ROM */
#ifdef NO_TVR
    XtSetArg (args[n], XtNenqROM, "Cv1");
#else
    XtSetArg (args[n], XtNenqROM, "Pv4");
#endif
    n++;

    fem = XmCreateFrame (entry, "f_ecran_minitel", NULL, 0);
    XtManageChild (fem);
    em = XtCreateManagedWidget ("ecran_minitel", videotexWidgetClass, fem, args, n);

#ifndef LOW_MEMORY
    /* Timer */
    if (rsc_xtel.petiteFonte && !low_memory)
	timer_minitel = XtVaCreateManagedWidget ("timer_minitel", timerWidgetClass, entry, NULL);
#endif

    /* Popup-menu du clavier Minitel */
    n = 0;
    XtSetArg (args[n], XmNwhichButton, 2); n++;
    menu_clavier = XmCreatePopupMenu (em, "menu_clavier", args, n);
    create_menu_entry (menu_clavier, "videotex", False, selection_mode_emulation, (XTPOINTER)"V");
    create_menu_entry (menu_clavier, "teleinfo_ascii", True, selection_mode_emulation, (XTPOINTER)"A");
    create_menu_entry (menu_clavier, "teleinfo_fr", True, selection_mode_emulation, (XTPOINTER)"F");
    create_menu_entry (menu_clavier, "envoi", False, commandes, (XTPOINTER)"A");
    create_menu_entry (menu_clavier, "retour", False, commandes, (XTPOINTER)"B");
    create_menu_entry (menu_clavier, "repetition", False, commandes, (XTPOINTER)"C");
    create_menu_entry (menu_clavier, "guide", False, commandes, (XTPOINTER)"D");
    create_menu_entry (menu_clavier, "annulation", False, commandes, (XTPOINTER)"E");
    create_menu_entry (menu_clavier, "sommaire", False, commandes, (XTPOINTER)"F");
    create_menu_entry (menu_clavier, "correction", False, commandes, (XTPOINTER)"G");
    create_menu_entry (menu_clavier, "suite", False, commandes, (XTPOINTER)"H");
    XtVaCreateManagedWidget ("blanc", xmSeparatorGadgetClass, menu_clavier, XmNheight, 20, NULL);
    create_menu_entry (menu_clavier, "connexion_fin", False, commandes, (XTPOINTER)"I");

    /* Bouton "Raccrocher" ? */
    if (rsc_xtel.boutonRaccrocher)
	create_menu_entry (menu_clavier, "raccrocher", False, raccrocher, NULL);

#ifndef LOW_MEMORY
    if (!low_memory) {
	boite_bas = XtVaCreateManagedWidget ("boite_bas", xmRowColumnWidgetClass, paned_base, XmNorientation, (rsc_xtel.petiteFonte ? XmVERTICAL : XmHORIZONTAL), NULL);
	
	(void)create_xpm_button (boite_bas, "btn_envoi", xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)"A", envoi_xpm, envoi_on_xpm, envoi_xpm, envoi_on_xpm, unsensitive_pixmap_b);
	(void)create_xpm_button (boite_bas, "btn_retour", xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)"B", retour_xpm, retour_on_xpm, retour_xpm, retour_on_xpm, unsensitive_pixmap_b);
	(void)create_xpm_button (boite_bas, "btn_repetition", xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)"C", repet_xpm, repet_on_xpm, repet_xpm, repet_on_xpm, unsensitive_pixmap_b);
	(void)create_xpm_button (boite_bas, "btn_guide", xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)"D", guide_xpm, guide_on_xpm, guide_xpm, guide_on_xpm, unsensitive_pixmap_b);
	(void)create_xpm_button (boite_bas, "btn_annulation", xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)"E", annul_xpm, annul_on_xpm, annul_xpm, annul_on_xpm, unsensitive_pixmap_b);
	(void)create_xpm_button (boite_bas, "btn_sommaire", xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)"F", sommaire_xpm, sommaire_on_xpm, sommaire_xpm, sommaire_on_xpm, unsensitive_pixmap_b);
	(void)create_xpm_button (boite_bas, "btn_correction", xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)"G", correc_xpm, correc_on_xpm, correc_xpm, correc_on_xpm, unsensitive_pixmap_b);
	(void)create_xpm_button (boite_bas, "btn_suite", xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)"H", suite_xpm, suite_on_xpm, suite_xpm, suite_on_xpm, unsensitive_pixmap_b);
	(void)create_xpm_button (boite_bas, "btn_connexion_fin", xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)"I", cxfin_xpm, cxfin_on_xpm, cxfin_xpm, cxfin_on_xpm, unsensitive_pixmap_b);
    }
#endif /* !LOW_MEMORY */

    return (em);
}

/*	
 * fonctions utilisees pour la gestion des menus deroulants
 */

/*
 * Traitement des selecteurs de fichiers
 */
static void Ouverture_selecteur_fichier (w, flag, call_data)
Widget w;
int *flag;
XtPointer call_data;
{
    /* Creation du selecteur */
    n = 0;
    XtSetArg (args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); n++;
    selecteur_fichier = XmCreateFileSelectionDialog (top, "selecteur_fichier", args, n);
    XtAddCallback(selecteur_fichier, XmNokCallback, Validation_selecteur_fichier, (XtPointer)flag);
    XtAddCallback(selecteur_fichier, XmNcancelCallback, Annulation_selecteur_fichier, NULL);
    XtUnmanageChild (XmSelectionBoxGetChild (selecteur_fichier, XmDIALOG_HELP_BUTTON));
    XtManageChild (selecteur_fichier);
}

static void Validation_selecteur_fichier (w, flag, call_data)
Widget w;
int flag;
XtPointer call_data;
{
    Widget text_w;

    text_w = XmSelectionBoxGetChild(selecteur_fichier, XmDIALOG_TEXT);
    if (flag == K_Sauve) {
	strcpy (nom_fichier_sauve, XmTextGetString(text_w));
	Sauve ();
	XtDestroyWidget (selecteur_fichier);
    }
    else {
	strcpy (nom_fichier_charge, XmTextGetString(text_w));
	XtDestroyWidget (selecteur_fichier);
	Charge ();
    }
}

static void Annulation_selecteur_fichier (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    XtDestroyWidget (selecteur_fichier);
}

/*
 * Traitement de la boite de saisie
 */
static void Ouverture_boite_saisie (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    /* Creation de la boite de saisie */
    n = 0;
    XtSetArg (args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); n++;
    boite_saisie = XmCreatePromptDialog (top, "boite_saisie", args, n);
    XtAddCallback(boite_saisie, XmNokCallback, Validation_boite_saisie, NULL);
    XtAddCallback(boite_saisie, XmNcancelCallback, Annulation_boite_saisie, NULL);
    XtUnmanageChild (XmSelectionBoxGetChild (boite_saisie, XmDIALOG_HELP_BUTTON));

    XtManageChild (boite_saisie);
}

static void Validation_boite_saisie (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    Widget text_w;

    text_w = XmSelectionBoxGetChild(boite_saisie, XmDIALOG_TEXT);
    init_xtel ();
    connexion_service (XmTextGetString(text_w));

    XtDestroyWidget (boite_saisie);
}

static void Annulation_boite_saisie (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    XtDestroyWidget (boite_saisie);
}

/*
 * Composition d'un service
 */
/* ARGSUSED */
static void
Composition_service (w, numero_choisi, call_data)
Widget w;
int numero_choisi;	     
XtPointer call_data;	
{
    /* composition */
#ifdef DEBUG
    printf ("service uucp %s\n", definition_services[numero_choisi].nom_uucp);
#endif

    init_xtel ();
    connexion_service (definition_services[numero_choisi].nom_uucp);
}

/*
 * Appel d'une procedure
 */
/* ARGSUSED */
static void
Appel_procedure (w, numero_procedure, call_data)
Widget w;
int numero_procedure;	     
XtPointer call_data;	
{
    /* composition */
#ifdef DEBUG
    printf ("Procedure numero %%d\n", numero_procedure);
#endif

    /*chat_courant = procedures[numero_procedure].chat;*/
    start_procedure (numero_procedure);

    init_xtel ();
    /* Si M1, on compose a la main... */
    if (!nb_services)
	connexion_service (NULL);
    else
	connexion_service (procedures[numero_procedure].service);
}

/*
 * Traitement de la boite de confirmation de sortie
 */
static void Ouverture_confirmation_sortie (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    /* Creation de la boite */
    n = 0;
    XtSetArg (args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); n++;
    confirmation_sortie = XmCreateQuestionDialog (top, "confirmation_sortie", args, n);
    XtAddCallback(confirmation_sortie, XmNokCallback, Validation_confirmation_sortie, NULL);
    XtAddCallback(confirmation_sortie, XmNcancelCallback, Annulation_confirmation_sortie, NULL);
    XtUnmanageChild (XmMessageBoxGetChild (confirmation_sortie, XmDIALOG_HELP_BUTTON));
    XtManageChild (confirmation_sortie);
}

/* ARGSUSED */
static void
Validation_confirmation_sortie (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;	
{
    Boolean flag_connexion;

    XtVaGetValues (ecran_minitel, XtNconnecte, &flag_connexion, NULL);

    /* Si on est connecte, on deconnecte et on quitte */
    if (flag_connexion) {
	write (socket_xteld, CHAINE_COMMANDE_FIN, 1);
	sortie_violente = True;
    }
    else
	ce_n_est_qu_un_au_revoir ();
}

/* ARGSUSED */
static void
Annulation_confirmation_sortie (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;	
{
    XtDestroyWidget (confirmation_sortie);
}

/*
 * Traitement de la boite d'erreur
 */ 
static void Validation_message_erreur (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    XtDestroyWidget (message_erreur);
}

/*
 * Affichage du copyright
 */
static void Affiche_copyright (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    Pixmap pixmap_pf;

    if (!flag_copyright_affiche) {
#ifndef NO_XPM
	XpmCreatePixmapFromData (XtDisplay(top), XtWindow(top), pf_xpm, &pixmap_pf, NULL, NULL);
#else
	pixmap_pf = XCreateBitmapFromData (XtDisplay(top), XtWindow(top), pf_bits, pf_width, pf_height);
#endif /* NO_XPM */
	affiche_copyright (top, xmFormWidgetClass, pixmap_pf);
    }
}

/* 
 * Affichage d'une erreur donnee par code_erreur 
 */
void affiche_erreur(s, code_erreur)
char *s;
int code_erreur;
{
    if (code_erreur != 0)
	sprintf (buf, "%s : %s", s, sys_errlist[code_erreur]);
    else {
      if (s[0] == '[')
	strcpy (buf, rsc_xtel.erreur[s[1]-'0']);
      else
	strcpy (buf, s);
    }

    /* Creation de la boite */
    n = 0;
    XtSetArg (args[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); n++;
    XtSetArg (args[n], XmNmessageString, XmStringDefCreate(buf)); n++;
    message_erreur = XmCreateErrorDialog (top, "message_erreur", args, n);
    XtAddCallback(message_erreur, XmNokCallback, Validation_message_erreur, NULL);
    XtUnmanageChild (XmMessageBoxGetChild(message_erreur, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild (XmMessageBoxGetChild(message_erreur, XmDIALOG_HELP_BUTTON));
    XtManageChild (message_erreur);
}

#ifdef NOTDEF
/* Maintenant dans bouton.c */

/* Traitement de l'enregistrement */
static void demarre_enregistrement (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    flag_enregistrement = 1;
    XtSetSensitive (bouton_demarre, False);
    XtSetSensitive (bouton_arrete, True);
#ifndef LOW_MEMORY
    if (!low_memory)
	XtMapWidget (la_cassete);
#endif
}

static void arrete_enregistrement (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    flag_enregistrement = 0;
    XtSetSensitive (bouton_demarre, True);
    XtSetSensitive (bouton_arrete, False);
#ifndef LOW_MEMORY
    if (!low_memory)
	XtUnmapWidget (la_cassete);
#endif
}

static void change_enregistrement (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    if (flag_enregistrement)
	Arrete_enregistrement (w, NULL, NULL);
    else
	Demarre_enregistrement (w, NULL, NULL);
}

static void nouvel_enregistrement (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    cpt_buffer = 0;
    taille_zone_enregistrement = 1000;
    zone_enregistrement = realloc (zone_enregistrement, taille_zone_enregistrement);
}

#endif /* NOTDEF */

/* 
 * Fonctions du lecteur 
 */
void activation_bouton_stop ()
{
    XmToggleButtonSetState (stop_lecteur, True, True);
}

/* Force le bouton STOP valide si appel de stop_rapide() */
static void Valide_le_bouton_stop (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    activation_bouton_stop ();
}

/* validation/invalidation */
void lecteur_valide (flag)
char flag;
{
    XtSetSensitive (bouton_lecteur, flag);
#ifndef LOW_MEMORY
    if (!low_memory)
	XtSetSensitive (btn_lecteur, flag);
#endif /* LOW_MEMORY */
}

/* Affiche/efface le lecteur */
void efface_lecteur ()
{
    XtUnmanageChild (lecteur);
}

void affiche_lecteur ()
{
    XtManageChild (lecteur);
}

/* Affiche les compteurs */
void affiche_compteur_lecteur_caractere ()
{
    sprintf (buf, "%05d", numero_caractere_courant);

    n = 0;
    XtSetArg (args[n], XmNlabelString, XmStringDefCreate(buf)); n++;
    XtSetValues (compteur_lecteur_caractere, args, n);
}

void affiche_compteur_lecteur_page ()
{
    sprintf (buf, "%03d", numero_page_courante);

    n = 0;
    XtSetArg (args[n], XmNlabelString, XmStringDefCreate(buf)); n++;
    XtSetValues (compteur_lecteur_page, args, n);
}

