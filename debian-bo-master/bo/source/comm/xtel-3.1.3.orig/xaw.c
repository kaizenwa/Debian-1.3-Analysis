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
static char rcsid[] = "$Id: xaw.c,v 1.17 1997/02/21 17:00:30 pierre Exp $";

/*
 * Couche Toolkit ATHENA
 */
#include "xtel.h"
#include <sys/types.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include <X11/cursorfont.h>

#ifndef LOW_MEMORY
#include "XpmButton.h"
#include "XpmToggle.h"
#include "Timer.h"

#include "bitmaps/unsensitiveh.xbm"
#include "bitmaps/unsensitiveb.xbm"

#include "pixmaps/disk_write.xpm"
#include "pixmaps/disk_read.xpm"
#include "pixmaps/printera.xpm"
#include "pixmaps/printer.xpm"
#include "pixmaps/door_run.xpm"
#include "pixmaps/nouveau.xpm"
#include "pixmaps/camera.xpm"
#include "pixmaps/phone.xpm"
#include "pixmaps/film.xpm"
#include "pixmaps/cassete.xpm"
#include "pixmaps/envoi.xpm"
#include "pixmaps/envoi_on.xpm"
#include "pixmaps/retour.xpm"
#include "pixmaps/retour_on.xpm"
#include "pixmaps/repet.xpm"
#include "pixmaps/repet_on.xpm"
#include "pixmaps/guide.xpm"
#include "pixmaps/guide_on.xpm"
#include "pixmaps/annul.xpm"
#include "pixmaps/annul_on.xpm"
#include "pixmaps/sommaire.xpm"
#include "pixmaps/sommaire_on.xpm"
#include "pixmaps/correc.xpm"
#include "pixmaps/correc_on.xpm"
#include "pixmaps/suite.xpm"
#include "pixmaps/suite_on.xpm"
#include "pixmaps/cxfin.xpm"
#include "pixmaps/cxfin_on.xpm"

#include "pixmaps/rewind.xpm"
#include "pixmaps/prev.xpm"
#include "pixmaps/stop.xpm"
#include "pixmaps/play.xpm"
#include "pixmaps/next.xpm"

#endif /* LOW_MEMORY */

#ifndef NO_XPM
#include <X11/xpm.h>
#include "pixmaps/pf.xpm"
#else
#include "bitmaps/pf.bit"
#endif /* NO_XPM */

#include "bitmaps/teleph.xbm"
#include "bitmaps/submenu.xbm"
#include "bitmaps/zero.bit"
#include "bitmaps/wind.bit"
#include "bitmaps/rewind.bit"
#include "bitmaps/play.bit"
#include "bitmaps/stop.bit"
#include "bitmaps/lecteur.bit"
#include "bitmaps/aide.bit"

#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Label.h>

#define K_Charge		0
#define K_Sauve			1
#define K_Compose		2
#define K_Fin			3
#define SAUVE            	0
#define CHARGE           	1
#define IMPRIME           	2
#define LIGNE_BLANCHE		3
#define QUITTE           	4
#define NOUVEAU            	0
#define DEMARRE           	1
#define ARRETE          	2
#define LECTEUR          	3

/* Nom des entrees du menu "Fichier" */
static String entree_fichier[] = {
    "sauve",
    "charge",
    "imprime",
    "blanc",
    "quitte",
    NULL
};


static String entree_enregistrement[] = {
    "nouveau",
    "demarre",
    "arrete",
    "lecteur",
    NULL,
};

#ifndef LOW_MEMORY
static String *pixmaps_fichier[] = {
    disk_write_xpm,
    disk_read_xpm,
    NULL,
    NULL,
    door_run_xpm
};

static String *pixmaps_minitel[] = {
    envoi_xpm,
    retour_xpm,
    repet_xpm,
    guide_xpm,
    annul_xpm,
    sommaire_xpm,
    correc_xpm,
    suite_xpm,
    NULL,
    cxfin_xpm
};

static String *pixmaps_minitel_on[] = {
    envoi_on_xpm,
    retour_on_xpm,
    repet_on_xpm,
    guide_on_xpm,
    annul_on_xpm,
    sommaire_on_xpm,
    correc_on_xpm,
    suite_on_xpm,
    NULL,
    cxfin_on_xpm
};

#endif

static void PopupSousMenu(), PopdownSousMenu();

static XtActionsRec actions[] = {
    {"PopupSousMenu", (XtActionProc)PopupSousMenu},
    {"PopdownSousMenu", (XtActionProc)PopdownSousMenu}
}; 

static char *toolkit = "Athena";
static Arg args[30];
static int n;
static char buf[256];
static Widget em, top;
static Widget bouton_enregistrement, menu_enregistrement;
static Widget bouton_fichier, menu_fichier, menu_compose, menu_procedure;
static Widget forme_base, nouveau, lecteur;
static Widget popup_sortie, popup_saisie, popup_erreur, quitte_lecteur, titre_arret_lecteur;
static Widget boite_saisie, confirmation_sortie, message_erreur;
static Widget compteur_lecteur_titre, compteur_lecteur_caractere, compteur_lecteur_page;
static Widget ok_boite_saisie;
static Widget ok_confirmation_sortie;
static Widget popup_lecteur, forme_lecteur, play_lecteur, stop_lecteur, wind_lecteur;
static Widget rewind_lecteur, zero_lecteur;
static Widget arret_lecteur_popup;
static Widget type_arret_lecteur, arret_none, arret_form_feed, arret_suite;
static Widget arret_suite_ou_ff;
static Widget menu_aide, bouton_aide;
static Widget menu_proto, paned_base;
#ifndef LOW_MEMORY
static Widget boite_haut, btn_enregistre, btn_lecteur, btn_quitte;
#endif /* LOW_MEMORY */
static Widget popup_impression, bouton_imprime;
static Boolean popup_actif;
static Cursor hand_cursor;

#ifdef NOTDEF
static void arret_a_chaque_page ();
static void type_fin_page ();
#else
extern void arret_a_chaque_page ();
extern void type_fin_page ();
#endif

static void Popdown ();
static void positionnement_widget ();
static void prepare_dialogue ();

static void Selection_menu_fichier();
static void Affiche_copyright ();
static void Selection_menu_compose();
static void Selection_menu_compose_direct();
static void Selection_menu_procedure() /*, Selection_menu_enregistrement()*/;
static void Validation_dialogue(), Validation_erreur();

static XrmOptionDescRec options[] = {
    {"-serveur",	"*serveur",		XrmoptionSepArg, NULL},
    {"-service",	"*nomService",		XrmoptionSepArg, NULL},
    {"-petit",		"*petiteFonte",		XrmoptionNoArg, "True"},
    {"-ng",		"*nGris",		XrmoptionNoArg, "True"},
    {"-br",		"*boutonRaccrocher",	XrmoptionNoArg, "True"},
    {"-low",		"*lowMemory",		XrmoptionNoArg, "True"},
};

static void menu_type_arret_page_cb (w, choice, call_data)
Widget w;
int choice;
XtPointer call_data;
{
    String value;
    flag_arret_a_chaque_page = True;

    switch (choice) {
      case 0 :
	  flag_arret_a_chaque_page = False;
	  XtVaGetValues (arret_none, XtNlabel, &value, NULL);
	  break;
	  
	case 1 :
	   type_de_fin_page = ARRET_FF;
	  XtVaGetValues (arret_form_feed, XtNlabel, &value, NULL);
	  break;

	case 2 :
	   type_de_fin_page = ARRET_SUITE;
	  XtVaGetValues (arret_suite, XtNlabel, &value, NULL);
	  break;

	case 3 :
	   type_de_fin_page = ARRET_SUITE | ARRET_FF;
	  XtVaGetValues (arret_suite_ou_ff, XtNlabel, &value, NULL);
	  break;
	  
	default :
	    return;
      }

    XtVaSetValues (type_arret_lecteur, XtNlabel, value, NULL);
}

static void
PopupSousMenu (w, pevent, params, nb_params)
Widget          w;
XCrossingEvent  *pevent;
String         *params;
Cardinal        nb_params;
{
    Dimension width, height;
    Position x, y, x_root, y_root;

    XtVaGetValues (bouton_imprime, XtNwidth, &width, XtNheight, &height, XtNx, &x, XtNy, &y, NULL);
    XtTranslateCoords (bouton_imprime, x, y, &x_root, &y_root);

    if (!popup_actif && (pevent->x > width && pevent->y > y) && pevent->y < (y + height)) {
	XtVaSetValues (popup_impression, XtNx, x_root + width + 1, XtNy, pevent->y_root, NULL);
	XtPopup (popup_impression, XtGrabNonexclusive);
	popup_actif = True;
    }
}

static void
PopdownSousMenu (w, pevent, params, nb_params)
Widget          w;
XCrossingEvent  *pevent;
String         *params;
Cardinal        nb_params;
{
    if (popup_actif) {
	XtPopdown (popup_impression);
	popup_actif = False;
    }
}

/*
 * Creation d'une entree menu
 */
static Widget create_menu_entry (parent, name, cb, call_data)
Widget parent;
char *name;
void *cb;
XtPointer call_data;
{
    Widget entry;

    entry = XtVaCreateManagedWidget (name, smeBSBObjectClass, parent, NULL);
    XtAddCallback(entry, XtNcallback, (XtCallbackProc)cb, call_data);
    
    return (entry);
}

#ifndef LOW_MEMORY
/*
 * Creation de boutons
 */
static Widget create_pixmap_button (parent, name, class, cb, call_data, pixmap1_data, pixmap2_data, shape1_data, shape2_data, unsensitive_bitmap)
Widget parent;
char *name;
WidgetClass class;
void *cb;
XtPointer call_data;
char **pixmap1_data, **pixmap2_data;
char **shape1_data, **shape2_data;
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
#endif /* LOW_MEMORY */

static Widget create_bitmap_button (parent, name, class, cb, call_data, bits, width, height)
Widget parent;
char *name;
WidgetClass class;
void *cb;
XtPointer call_data;
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
	w = XtVaCreateManagedWidget (name, class, parent, XtNbitmap, pixmap, XtNforeground, fg, XtNbackground, bg, NULL);
	if (cb)
	    XtAddCallback (w, XtNcallback, (XtCallbackProc)cb, call_data);
	return (w);
    }
    else {
	fprintf (stderr, "create_bitmap_button: %s: can't create X bitmap\n", name);
	return (NULL);
    }
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
	w = XtVaCreateManagedWidget (name, labelWidgetClass, parent, XtNbitmap, pixmap, XtNmappedWhenManaged, False, NULL);
	return (w);
    }
    else {
	fprintf (stderr, "create_bitmap_label: %s: can't create X bitmap\n", name);
	return (NULL);
    }
}

static Widget create_text_button (parent, name, class, activate_cb, activate_call_data)
Widget parent;
char *name;
WidgetClass class;
void *activate_cb;
XtPointer activate_call_data;
{
    Widget w;
    
    w = XtVaCreateManagedWidget (name, class, parent, NULL);
    if (activate_cb) 
	XtAddCallback (w, XtNcallback, (XtCallbackProc)activate_cb, activate_call_data);
    return (w);
}


/*
 * Initialisation du toolkit 
 */
Widget init_toolkit (pac, av)
int *pac;
char **av; 
{
#ifdef DEBUG
    printf ("Initialisation du toolkit\n");
#endif

    toolkit_utilise = toolkit;
    top = XtAppInitialize (&app_context, "XTel", options, XtNumber(options), pac, av, NULL, NULL, 0);

    return (top);
}

/*
 * Initialisation des widgets
 */
Widget init_widgets (topLevel)
Widget topLevel;
{
    register int i;
    Widget entry, typein;
#ifdef DEBUG
    printf ("Initialisation des widgets\n");
#endif
#ifndef LOW_MEMORY
    Pixmap unsensitive_pixmap_h, unsensitive_pixmap_b;
#endif

    low_memory = rsc_xtel.lowMemory;

    /* forme de base */
    forme_base = XtCreateManagedWidget ("forme_base", formWidgetClass, topLevel, NULL, 0);

    /*
     * Creation des menus 
     */

    /* Fichier */

    /* titre du menu */
    bouton_fichier = XtVaCreateManagedWidget("bouton_fichier", menuButtonWidgetClass, forme_base, XtNmenuName, "menu_fichier", NULL);

    /* le menu en lui meme */
    menu_fichier = XtCreatePopupShell("menu_fichier", simpleMenuWidgetClass, bouton_fichier, NULL, 0);

    /* creation des entrees */
    for (i = 0;  entree_fichier[i] != NULL ; i++) {
      String item = entree_fichier[i];

      if (i == LIGNE_BLANCHE)
	(void)XtCreateManagedWidget(item, smeLineObjectClass, menu_fichier, NULL, 0);
      else {
	entry = create_menu_entry (menu_fichier, item, Selection_menu_fichier, (XtPointer)i);
	if (i == IMPRIME) {
	    bouton_imprime = entry;
	    XtVaSetValues (entry, XtNrightBitmap, XCreateBitmapFromData (XtDisplay(topLevel), XtScreen(topLevel)->root, submenu_bits, submenu_width, submenu_height), NULL);
	}
      }
    }

    /* Selection impression ASCII/Videotex */
    popup_impression = XtCreatePopupShell ("popup_impression", simpleMenuWidgetClass, forme_base, NULL, 0);
    (void)create_menu_entry (popup_impression, "imp_ascii", (XtCallbackProc)imprime_page_courante_ascii, (XtPointer)NULL);
    (void)create_menu_entry (popup_impression, "imp_videotex", (XtCallbackProc)imprime_page_courante_videotex, (XtPointer)NULL);

    /* Enregistrement */

    /* titre du menu */
    bouton_enregistrement = XtVaCreateManagedWidget("bouton_enregistrement", menuButtonWidgetClass, forme_base,
		  XtNmenuName, "menu_enregistrement",
		  XtNfromHoriz, bouton_fichier, NULL);

    /* le menu en lui meme */
    menu_enregistrement = XtCreatePopupShell("menu_enregistrement", simpleMenuWidgetClass, bouton_enregistrement, NULL, 0);

    /* creation des entrees */
    nouveau = create_menu_entry (menu_enregistrement, entree_enregistrement[NOUVEAU],  (XtCallbackProc)nouvel_enregistrement, (XtPointer)NULL);
    bouton_demarre = create_menu_entry (menu_enregistrement, entree_enregistrement[DEMARRE],  (XtCallbackProc)demarre_enregistrement, (XtPointer)NULL);
    bouton_arrete = create_menu_entry (menu_enregistrement, entree_enregistrement[ARRETE],  (XtCallbackProc)arrete_enregistrement, (XtPointer)NULL);
    lecteur = create_menu_entry (menu_enregistrement, entree_enregistrement[LECTEUR],  (XtCallbackProc)debut_lecteur, (XtPointer)NULL);

    /* 
     *  Composition : les entree de ce menu sont creees dynamiquement a partir 
     *  du fichier "xtel.services"
     */
    if (nb_services != 0) {
	/* titre du menu */
	bouton_compose = XtVaCreateManagedWidget("bouton_compose", menuButtonWidgetClass, forme_base,
		  XtNmenuName, "menu_compose",
		  XtNfromHoriz, bouton_enregistrement, NULL);
	
	/* le menu en lui meme */
	menu_compose = XtCreatePopupShell("menu_compose", simpleMenuWidgetClass, bouton_compose, NULL, 0);

	/* creation des entrees */
	for (i = 0; i < nb_services ; i++) {
	    if (i == nb_services-1 && !strcmp (entree_compose[i], "Direct"))
	      (void)create_menu_entry (menu_compose, entree_compose[i],  (XtCallbackProc)Selection_menu_compose_direct, (XtPointer)i);
	    else
	      (void)create_menu_entry (menu_compose, entree_compose[i],  (XtCallbackProc)Selection_menu_compose, (XtPointer)i);
	}
    }

    /* 
     *  Procedures : les entree de ce menu sont creees dynamiquement a partir 
     *  du fichier ".xtelproc"
     */
    if (nb_procedures != 0) {
	/* titre du menu */
	if (nb_services == 0)
	    bouton_procedure = XtVaCreateManagedWidget("bouton_procedure", menuButtonWidgetClass, forme_base,
		  XtNmenuName, "menu_procedure",
		  XtNfromHoriz, bouton_enregistrement, NULL);
	else
	    bouton_procedure = XtVaCreateManagedWidget("bouton_procedure", menuButtonWidgetClass, forme_base,
		  XtNmenuName, "menu_procedure",
		  XtNfromHoriz, bouton_compose, NULL);
	
	/* le menu en lui meme */
	menu_procedure = XtCreatePopupShell("menu_procedure", simpleMenuWidgetClass, bouton_procedure, NULL, 0);
	
	/* creation des entrees */
	for (i = 0 ; i != nb_procedures ; i++) {
	    char nomproc[5];
	    
	    sprintf (nomproc, "p%d", i);
	    entry = create_menu_entry (menu_procedure, nomproc,  (XtCallbackProc)Selection_menu_procedure, (XtPointer)i);
	    XtVaSetValues (entry, XtNlabel, procedures[i].nom, NULL);
	}
    }

    /* Menu des protocoles */
    if (nb_protocoles) {
	/* titre du menu */
	if (nb_procedures == 0) {
	    if (nb_services == 0)
		entry = bouton_enregistrement;
	    else
		entry = bouton_compose;
	}
	else
	    entry = bouton_procedure;

	bouton_proto = XtVaCreateManagedWidget("bouton_proto", menuButtonWidgetClass, forme_base,
		  XtNmenuName, "menu_proto",
		  XtNfromHoriz, entry, XtNsensitive, False, NULL);

	/* le menu en lui meme */
	menu_proto = XtCreatePopupShell("menu_proto", simpleMenuWidgetClass, bouton_proto, NULL, 0);

	/* creation des entrees */
	for (i = 0 ; i != nb_protocoles ; i++) {
	    char nomproto[10];

	    sprintf (nomproto, "proto%d", i);
	    entry = create_menu_entry (menu_proto, nomproto, (XtCallbackProc)Selection_telechargement, (XtPointer)i);
	    XtVaSetValues (entry, XtNlabel, protocoles[i].nom, NULL);
	}

	XtCreateManagedWidget("blanc_tele", smeLineObjectClass, menu_proto, NULL, 0);	
	/* Bouton d'interruption de protocole */
	stop_tele = create_menu_entry (menu_proto, "stop_tele",  (XtCallbackProc)Stop_telechargement, (XtPointer)NULL);
    }

    /* Aide */
    /* titre du menu */
    bouton_aide = XtVaCreateManagedWidget("bouton_aide", menuButtonWidgetClass, forme_base,
		  XtNmenuName, "menu_aide",
		  XtNbitmap, XCreateBitmapFromData (XtDisplay(topLevel), XtScreen(topLevel)->root, aide_bits, aide_width, aide_height),
		  XtNfromHoriz, bouton_enregistrement, 
                  XtNhorizDistance, (rsc_xtel.petiteFonte ? 170 : 425), NULL);
	
    /* le menu en lui meme */
    menu_aide = XtCreatePopupShell("menu_aide", simpleMenuWidgetClass, bouton_aide, NULL, 0);
    (void)create_menu_entry (menu_aide, "a_propos", (XtCallbackProc)Affiche_copyright, (XtPointer)NULL);

    /* 
     * Paned contenant les boutons du haut, l'ecran minitel, les boutons du bas
     */
    paned_base = XtVaCreateManagedWidget ("paned_base", panedWidgetClass, forme_base, XtNfromVert, bouton_fichier, XtNorientation, (rsc_xtel.petiteFonte ? XtorientHorizontal : XtorientVertical), NULL);
#ifndef LOW_MEMORY
    if (!low_memory)
	boite_haut = XtCreateManagedWidget ("boite_haut", boxWidgetClass, paned_base, NULL, 0);
#endif
    /*
     * Ecran/clavier du minitel  
     */
    /* ecran */
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
	    entry = XtVaCreateManagedWidget ("pem", panedWidgetClass, paned_base, NULL);
#endif
    }

    /* Id de ROM */
#ifdef NO_TVR
    XtSetArg (args[n], XtNenqROM, "Cv1");
#else
    XtSetArg (args[n], XtNenqROM, "Pv4");
#endif
    n++;

    em = XtCreateManagedWidget ("ecran_minitel", videotexWidgetClass, entry, args, n);

#ifndef LOW_MEMORY
    if (rsc_xtel.petiteFonte && !low_memory)
	timer_minitel = XtVaCreateManagedWidget ("timer_minitel", timerWidgetClass, entry, NULL);
#endif

    /* Clavier MINITEL = popup menu */
    menu_clavier = XtCreatePopupShell ("menu_clavier", simpleMenuWidgetClass, em, NULL, 0);

#ifndef LOW_MEMORY
    if (!low_memory) {
	/* La boite du bas, le Label d'aide */
	boite_bas = XtCreateManagedWidget ("boite_bas", boxWidgetClass, paned_base, NULL, 0);
/*	aide_xpmb = XtCreateManagedWidget ("aide_xpmb", labelWidgetClass, paned_base, NULL, 0);*/
    }
#endif
    /* creation des entrees */
    for (i = 0; i < 14 ; i++) {
        String item = touches[i].nom;

        if (strcmp (item, "blanc") == 0)
	  (void)XtCreateManagedWidget(item, smeLineObjectClass, menu_clavier, NULL, 0);
	else
	  (void)create_menu_entry (menu_clavier, item, (i > 3 ? (XtCallbackProc)commandes : (XtCallbackProc)selection_mode_emulation), (XtPointer)touches[i].code);
    }

    if (rsc_xtel.boutonRaccrocher)
      (void)create_menu_entry (menu_clavier, "raccrocher", (XtCallbackProc)raccrocher, (XtPointer)NULL);

    /*
     * Popups pour les boites de dialogue 
     */
    popup_saisie = XtCreatePopupShell ("popup_saisie", transientShellWidgetClass, forme_base, NULL, 0);
    popup_sortie = XtCreatePopupShell ("popup_sortie", transientShellWidgetClass, forme_base, NULL, 0);

    /* Confirmation de sortie */
    confirmation_sortie = XtCreateManagedWidget ("confirmation_sortie", dialogWidgetClass, popup_sortie, NULL, 0);

    /* 2 boutons : Ok, Annule */
    ok_confirmation_sortie = create_text_button (confirmation_sortie, "ok_confirmation_sortie", commandWidgetClass, Validation_dialogue, (XtPointer) K_Fin);
    XawDialogAddButton (confirmation_sortie, "annule_confirmation_sortie", Popdown, popup_sortie);

    /*
     * Boite de saisie pour chargement / sauvegarde / composition
     */
    boite_saisie = XtCreateManagedWidget ("boite_saisie", dialogWidgetClass, popup_saisie, NULL, 0);	        
    ok_boite_saisie = XtCreateManagedWidget ("ok_boite_saisie", commandWidgetClass, boite_saisie, NULL, 0);
    XawDialogAddButton (boite_saisie, "annule_boite_saisie", Popdown, popup_saisie);

    /* Installation de l'accelerateur : Return <==> cliquer "Ok" */
    if ((typein = XtNameToWidget(boite_saisie, "value")) != 0) {
	XtInstallAccelerators(typein, ok_boite_saisie);
    }

    /*
     * Creation du lecteur
     */
    n = 0;
    XtSetArg(args[n], XtNiconPixmap, 
	     XCreateBitmapFromData(XtDisplay(topLevel),
				   XtScreen(topLevel)->root,
				   lecteur_bits, lecteur_width, lecteur_height)); n++;
    popup_lecteur = XtCreatePopupShell ("popup_lecteur", applicationShellWidgetClass, forme_base, args, n);
    forme_lecteur = XtCreateManagedWidget ("forme_lecteur", formWidgetClass, popup_lecteur, NULL, 0);

    /*
     * Boutons du lecteur
     */
#ifndef LOW_MEMORY
    if (low_memory)
#endif /* !LOW_MEMORY */
	{
	    zero_lecteur = create_bitmap_button (forme_lecteur, "zero_lecteur", commandWidgetClass, Zero_lecteur, (XtPointer)NULL, zero_bits, zero_width, zero_height);
	    rewind_lecteur = create_bitmap_button (forme_lecteur, "rewind_lecteur", commandWidgetClass, Rewind_lecteur, (XtPointer)NULL, rewind_bits, rewind_width, rewind_height);
	    stop_lecteur = create_bitmap_button (forme_lecteur, "stop_lecteur", toggleWidgetClass, Stop_lecteur, (XtPointer)NULL, stop_bits, stop_width, stop_height);
	    play_lecteur = create_bitmap_button (forme_lecteur, "play_lecteur", toggleWidgetClass, Play_lecteur, (XtPointer)NULL, play_bits, play_width, play_height);

	    wind_lecteur = create_bitmap_button (forme_lecteur, "wind_lecteur", commandWidgetClass, Wind_lecteur, (XtPointer)NULL, wind_bits, wind_width, wind_height);
	}
#ifndef LOW_MEMORY
    else {
	zero_lecteur = create_pixmap_button (forme_lecteur, "zero_lecteur", xpmButtonWidgetClass, (XtCallbackProc)Zero_lecteur, (XtPointer)NULL, rewind_xpm, NULL, NULL, NULL, None);
	rewind_lecteur = create_pixmap_button (forme_lecteur, "rewind_lecteur", xpmButtonWidgetClass, Rewind_lecteur, (XtPointer)NULL, prev_xpm, NULL, NULL, NULL, None);
	stop_lecteur = create_pixmap_button (forme_lecteur, "stop_lecteur", xpmToggleWidgetClass, Stop_lecteur, (XtPointer)NULL, stop_xpm, NULL, NULL, NULL, None);
	play_lecteur = create_pixmap_button (forme_lecteur, "play_lecteur", xpmToggleWidgetClass, Play_lecteur, (XtPointer)NULL, play_xpm, NULL, NULL, NULL, None);
	wind_lecteur = create_pixmap_button (forme_lecteur, "wind_lecteur", xpmButtonWidgetClass, Wind_lecteur, (XtPointer)NULL, next_xpm, NULL, NULL, NULL, None);
    }
#endif /* !LOW_MEMORY */

    XtVaSetValues (play_lecteur, XtNradioGroup, stop_lecteur, NULL);

    /* le compteur */
    compteur_lecteur_titre = XtCreateManagedWidget ("compteur_lecteur_titre", labelWidgetClass, forme_lecteur, NULL, 0);
    compteur_lecteur_caractere = XtCreateManagedWidget ("compteur_lecteur_caractere", labelWidgetClass, forme_lecteur, NULL, 0);
    compteur_lecteur_page = XtCreateManagedWidget ("compteur_lecteur_page", labelWidgetClass, forme_lecteur, NULL, 0);

    /* Arret a chaque page */
    titre_arret_lecteur = XtCreateManagedWidget ("titre_arret_lecteur", labelWidgetClass, forme_lecteur, NULL, 0);
    type_arret_lecteur = XtCreateManagedWidget ("type_arret_lecteur", labelWidgetClass, forme_lecteur, NULL, 0);
    arret_lecteur_popup = XtCreatePopupShell ("arret_lecteur_popup", simpleMenuWidgetClass, forme_lecteur, NULL, 0);
    arret_none = XtVaCreateManagedWidget ("arret_none", smeBSBObjectClass, arret_lecteur_popup, NULL);
    XtAddCallback (arret_none, XtNcallback, (XtCallbackProc)menu_type_arret_page_cb, (XtPointer)0);
    arret_form_feed = XtVaCreateManagedWidget ("arret_form_feed", smeBSBObjectClass, arret_lecteur_popup, NULL);
    XtAddCallback (arret_form_feed, XtNcallback, (XtCallbackProc)menu_type_arret_page_cb, (XtPointer)1);
    arret_suite = XtVaCreateManagedWidget ("arret_suite", smeBSBObjectClass, arret_lecteur_popup, NULL);
    XtAddCallback (arret_suite, XtNcallback, (XtCallbackProc)menu_type_arret_page_cb, (XtPointer)2);
    arret_suite_ou_ff = XtVaCreateManagedWidget ("arret_suite_ou_ff", smeBSBObjectClass, arret_lecteur_popup, NULL);
    XtAddCallback (arret_suite_ou_ff, XtNcallback, (XtCallbackProc)menu_type_arret_page_cb, (XtPointer)3);


    /* le bouton pour quitter */
    quitte_lecteur = create_text_button (forme_lecteur, "quitte_lecteur", commandWidgetClass, fin_lecteur, (XtPointer)NULL);

    XtAddEventHandler (popup_saisie, EnterWindowMask, False, (XtEventHandler)evenement_entre_widget, NULL);
    XtAddEventHandler (forme_lecteur, LeaveWindowMask, False, (XtEventHandler)sauve_l_ecran, NULL);
    XtAppAddActions (app_context, actions, XtNumber(actions));

    classe_forme_copyright = formWidgetClass;

#ifndef LOW_MEMORY
    if (!low_memory) {
      /* Calcul des Pixmaps de masquage des boutons */
      unsensitive_pixmap_h = XCreateBitmapFromData (XtDisplay(topLevel), DefaultRootWindow(XtDisplay(topLevel)), unsensitiveh_bits, unsensitiveh_width, unsensitiveh_height);
      unsensitive_pixmap_b = XCreateBitmapFromData (XtDisplay(topLevel), DefaultRootWindow(XtDisplay(topLevel)), unsensitiveb_bits, unsensitiveb_width, unsensitiveb_height);

	/*
	 * Raccourcis menu, timer
	 */
	/* "Fichier" */
	for (i = 0;  entree_fichier[i] != NULL ; i++) {
	    if (i != LIGNE_BLANCHE && i != QUITTE) {
		if (i == IMPRIME) {
		    (void)create_pixmap_button (boite_haut, "btn_imprime_ascii", xpmButtonWidgetClass,  (XtCallbackProc)imprime_page_courante_ascii, (XtPointer)NULL, printera_xpm, None, None, None, unsensitive_pixmap_h);
		    (void)create_pixmap_button (boite_haut, "btn_imprime", xpmButtonWidgetClass, (XtCallbackProc)imprime_page_courante_videotex, (XtPointer)NULL, printer_xpm, None, None, None, unsensitive_pixmap_h);
		}
		else {
		    sprintf (buf, "btn_%s", entree_fichier[i]);
		    (void)create_pixmap_button (boite_haut, buf, xpmButtonWidgetClass, Selection_menu_fichier, (XtPointer)i, pixmaps_fichier[i], None, None, None, unsensitive_pixmap_h);
		}
	    }
	}
	
	/* Enregistrement */
      sprintf (buf, "btn_%s", entree_enregistrement[NOUVEAU]);
      (void)create_pixmap_button (boite_haut, buf, xpmButtonWidgetClass, nouvel_enregistrement, (XtPointer)NULL, nouveau_xpm, None, None, None, unsensitive_pixmap_h);
      btn_enregistre = create_pixmap_button (boite_haut, "btn_enregistre", xpmButtonWidgetClass, change_enregistrement, (XtPointer)NULL, camera_xpm, None, None, None, unsensitive_pixmap_h);
      btn_lecteur = create_pixmap_button (boite_haut, "btn_lecteur", xpmButtonWidgetClass, debut_lecteur, (XtPointer)NULL, film_xpm, None, None, None, unsensitive_pixmap_h);

	/* Appel direct */
	if (nb_services >= 1 && strcmp (entree_compose[nb_services-1], "Direct") == 0) {
	    btn_direct = create_pixmap_button (boite_haut, "btn_direct", xpmButtonWidgetClass, Selection_menu_compose_direct, (XtPointer)(nb_services-1), phone_xpm, None, None, None, unsensitive_pixmap_h);
	}
	
      /* Quitter */
      btn_quitte = create_pixmap_button (boite_haut, "btn_quitte", xpmButtonWidgetClass, Selection_menu_fichier, (XtPointer)QUITTE, door_run_xpm, None, None, None, unsensitive_pixmap_h);
      la_cassete = create_pixmap_button (boite_haut, "la_cassete", xpmButtonWidgetClass, NULL, NULL, cassete_xpm, NULL, NULL, NULL, None);
      XtVaSetValues (la_cassete, XtNmappedWhenManaged, False, NULL);

      /* Creation du telephone */
      teleph = create_bitmap_label (boite_haut, "teleph", teleph_bits, teleph_width, teleph_height);

      XtVaCreateManagedWidget ("glue", coreWidgetClass, boite_haut, XtNwidth, (rsc_xtel.petiteFonte ? 90 : 20), NULL);
      
      if (!rsc_xtel.petiteFonte)
	  timer_minitel = XtVaCreateManagedWidget ("timer_minitel", timerWidgetClass, boite_haut, NULL);
	
      /* Boutons clavier Minitel */
      for (i = 4 ; i < 14 ; i++) {
	  String item = touches[i].nom;
	  
	  if (strcmp (item, "blanc") != 0) {
	      sprintf (buf, "btn_%s", item);
	      (void)create_pixmap_button (boite_bas, buf, xpmButtonWidgetClass, (XtCallbackProc)commandes, (XtPointer)touches[i].code, pixmaps_minitel[i-4], pixmaps_minitel_on[i-4], pixmaps_minitel[i-4], pixmaps_minitel_on[i-4], unsensitive_pixmap_b);
	  }	
      }

    }
#endif /* LOW_MEMORY */

    return (em);
}


/*
 * Positionnement du widget "w" par rapport au widget "r"
 */ 
static void positionnement_widget (r, w)
Widget r, w;
{
    Dimension xr, yr, xr_a, yr_a;

    XtVaGetValues (r, XtNx, &xr, XtNy, &yr, NULL);

    /* calcul des coordonnees absolues de la reference */
    XtTranslateCoords (r, (Position)xr, (Position)yr, (Position*)&xr_a, (Position*)&yr_a);

    /* fixe les coordonnees centrees */
    XtVaSetValues (w, XtNx, xr_a + 50, XtNy, yr_a + 50, NULL);
}

/*
 * Preparation du widget dialogue :
 *
 *	- fixe le callback adequat sur le bouton Ok
 *	- positionne le popup-shell
 *	- affiche...
 */
static void prepare_dialogue (ok, popup, code)
Widget ok, popup;
int code;
{
    if (!hand_cursor)
	hand_cursor = XCreateFontCursor (XtDisplay(top), XC_hand2);

    if (popup == popup_saisie)
      XtAddCallback (ok, XtNcallback, Validation_dialogue, (XtPointer) code);

    positionnement_widget (em, popup);
    XtPopup (popup, XtGrabNonexclusive);

    XDefineCursor (XtDisplay(popup), XtWindow(popup), hand_cursor);
}


/*
 * Popdown d'un menu (efface le popup-menu de l'ecran)
 */
static void
Popdown (w, popup, call_data)
Widget w, popup;
XtPointer call_data;	
{
    /*
     * Si boite de saisie, on supprime le callback du bouton "Ok"
     */
    if (popup == popup_saisie)
      XtRemoveAllCallbacks (ok_boite_saisie, XtNcallback);

    XtPopdown (popup);
/*    XSetInputFocus (XtDisplay(popup), PointerRoot, 0, CurrentTime);*/
}

/*
 * Confirmation d'une erreur : destruction du popup d'erreur et du "dialog" 
 * d'erreur
 */
static void
Validation_erreur (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;	
{
    XtPopdown (popup_erreur);
    XtDestroyWidget (popup_erreur);
    XtDestroyWidget (message_erreur);
}


/*
 * Callback du bouton "Ok" d'une boite de saisie
 */
static void Validation_dialogue (w, code, call_data)
Widget w;
int code;
XtPointer call_data;
{
    Boolean flag_connexion;
/*    XSetInputFocus (XtDisplay(w), PointerRoot, 0, CurrentTime);*/

    switch (code) {

      case K_Fin : /* quitte le programme */

	  XtVaGetValues (ecran_minitel, XtNconnecte, &flag_connexion, NULL);
	  if (flag_connexion) {
	      write (socket_xteld, CHAINE_COMMANDE_FIN, 1);
	      sortie_violente = 1;
	      break;
	  }
	  else
	    ce_n_est_qu_un_au_revoir ();

#ifdef NO_SEL_FILE

      case K_Sauve : /* sauve sur le fichier */

	XtRemoveAllCallbacks (ok_boite_saisie, XtNcallback);
	XtPopdown(popup_saisie);
	strcpy (nom_fichier_sauve, XawDialogGetValueString(boite_saisie));
	Sauve ();

	break;

      case K_Charge : /* charge un fichier */

	XtRemoveAllCallbacks (ok_boite_saisie, XtNcallback);
	XtPopdown(popup_saisie);
	strcpy (nom_fichier_charge, XawDialogGetValueString(boite_saisie));
	Charge ();

	break;

#endif /* NO_SEL_FILE */

      case K_Compose : /* composition numero direct */

	XtRemoveAllCallbacks (ok_boite_saisie, XtNcallback);
	XtPopdown(popup_saisie);
	if (strlen (XawDialogGetValueString(boite_saisie)) > 0) {
	    strcpy (numero_courant, XawDialogGetValueString(boite_saisie));
	    init_xtel ();
	    connexion_service (XawDialogGetValueString(boite_saisie));
	}
	
	break;

	default : break;
    }
}


/*
 * Traitement du menu "fichier"
 */
/* ARGSUSED */
static void
Selection_menu_fichier(w, option_choisie, garbage)
Widget w;
int option_choisie;	        /* client_data */
XtPointer garbage;		/* call_data */
{
#ifndef NO_SEL_FILE
    FILE *fp;
    extern FILE *XsraSelFile();
    String name;
#endif /* NO_SEL_FILE */

    switch (option_choisie) {

      case SAUVE :

#ifdef NO_SEL_FILE
	n = 0;
	XtSetArg (args[n], XtNlabel, rsc_xtel.string[0]); n++;
	XtSetArg (args[n], XtNvalue, nom_fichier_sauve); n++;
	XtSetValues (boite_saisie, args, n);
	prepare_dialogue (ok_boite_saisie, popup_saisie, K_Sauve);
#else
	fp = XsraSelFile (top, rsc_xtel.string[0], rsc_xtel.string[2], rsc_xtel.string[3], rsc_xtel.string[4], nom_fichier_sauve, "w", NULL, (char*)&name);
	if (fp) {
	    fclose (fp);
	    strcpy (nom_fichier_sauve, name);
	    XtFree (name);
	    Sauve ();
	}
#endif /* NO_SEL_FILE */
	break ;

      case CHARGE :

#ifdef NO_SEL_FILE
	n = 0;
	XtSetArg (args[n], XtNlabel, rsc_xtel.string[1]); n++;
	XtSetArg (args[n], XtNvalue, nom_fichier_charge); n++; 
	XtSetValues (boite_saisie, args, n);
	prepare_dialogue (ok_boite_saisie, popup_saisie, K_Charge);
#else
	fp = XsraSelFile (top, rsc_xtel.string[1], rsc_xtel.string[2], rsc_xtel.string[3], rsc_xtel.string[4], nom_fichier_charge, "r", NULL, (char*)&name);
	if (fp) {
	    fclose (fp);
	    strcpy (nom_fichier_charge, name);
	    XtFree (name);
	    Charge ();
	}
#endif /* NO_SEL_FILE */
	break ;

      case QUITTE :

	  prepare_dialogue (ok_confirmation_sortie, popup_sortie, K_Fin);
	break;

	default : 
	  break;
    }
}

/* 
 * Menu "enregistrement" 
 */
#ifdef NOTDEF
static void demarre_enregistrement ()
{
    flag_enregistrement = True;
    XtSetSensitive (demarre, False);
    XtSetSensitive (arrete, True);
#ifndef LOW_MEMORY
    if (!low_memory)
	XtMapWidget (la_cassete);
#endif
}

static void arrete_enregistrement ()
{
    flag_enregistrement = False;
    XtSetSensitive (demarre, True);
    XtSetSensitive (arrete, False);
#ifndef LOW_MEMORY
    if (!low_memory)
	XtUnmapWidget (la_cassete);
#endif
}

#ifndef LOW_MEMORY
static void cb_btn_enregistre (w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    if (flag_enregistrement)
	arrete_enregistrement ();
    else
	demarre_enregistrement ();
}
#endif /* LOW_MEMORY */



/* ARGSUSED */
static void
Selection_menu_enregistrement(w, option_choisie, garbage)
Widget w;
int option_choisie;	        /* client_data */
XtPointer garbage;		/* call_data */
{
    switch (option_choisie) {

      case NOUVEAU :

	  cpt_buffer = 0;
	  taille_zone_enregistrement = 1000;
	  zone_enregistrement = realloc (zone_enregistrement, taille_zone_enregistrement);
	  break;

	case DEMARRE :

	    demarre_enregistrement ();
	  break;

	case ARRETE :

	    arrete_enregistrement ();
	  break;

	case LECTEUR :

	    debut_lecteur ();
	  break;

	default : 
	  
	  break;
    }
}

#endif /* NOTDEF */

/*
 * Traitement du menu "compose"
 */
/* ARGSUSED */
static void
Selection_menu_compose(w, numero_choisi, garbage)
Widget w;
int numero_choisi;	     
XtPointer garbage;	
{
    /* composition */
#ifdef DEBUG
    printf ("service uucp %s\n", definition_services[numero_choisi].nom_uucp);
#endif

    init_xtel ();
    connexion_service (definition_services[numero_choisi].nom_uucp);
}

/* ARGSUSED */
static void
Selection_menu_compose_direct(w, numero_choisi, garbage)
Widget w;
int numero_choisi;	     
XtPointer garbage;	
{
    n = 0;
    XtSetArg (args[n], XtNlabel, rsc_xtel.string[5]); n++;
    XtSetArg (args[n], XtNvalue, numero_courant); n++;
    XtSetValues (boite_saisie, args, n);

    prepare_dialogue (ok_boite_saisie, popup_saisie, K_Compose);
}

/*
 * Traitement du menu "procedure"
 */
/* ARGSUSED */
static void
Selection_menu_procedure(w, numero_procedure, garbage)
Widget w;
int numero_procedure;	     
XtPointer garbage;	
{
#ifdef DEBUG
    printf ("Procedure numero %d\n", numero_procedure);
#endif

    /* Composition du service */
  /*  chat_courant = procedures[numero_procedure].chat;*/
    start_procedure (numero_procedure);

    init_xtel ();
    /* Si M1, on compose a la main... */
    if (!nb_services)
	connexion_service (NULL);
    else
	connexion_service (procedures[numero_procedure].service);
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
	affiche_copyright (top, formWidgetClass, pixmap_pf);
    }
}


/* 
 * Affichage d'une erreur donnee par code_erreur
 * 
 * Remarque: On est oblige de creer le widget a chaque fois car la largeur
 *	     d'un widget dialog est calculee a la creation...
 */
void affiche_erreur(s, code_erreur)
char *s;
int code_erreur;
{
    if (!hand_cursor)
	hand_cursor = XCreateFontCursor (XtDisplay(top), XC_hand2);

    if (code_erreur != 0)
	sprintf (buf, "%s : %s", s, sys_errlist[code_erreur]);
    else {
      if (s[0] == '[')
	strcpy (buf, rsc_xtel.erreur[s[1]-'0']);
      else
	strcpy (buf, s);
    }

    popup_erreur = XtCreatePopupShell ("popup_erreur", transientShellWidgetClass, forme_base, NULL, 0);

    /*
     * Boite d'erreur 
     */
    message_erreur = XtVaCreateManagedWidget ("message_erreur", dialogWidgetClass, popup_erreur, XtNlabel, buf, NULL);
    XawDialogAddButton (message_erreur, "ok_message_erreur", Validation_erreur, NULL);
    positionnement_widget (em, popup_erreur);
    XtPopup (popup_erreur, XtGrabNonexclusive);
    XDefineCursor (XtDisplay(popup_erreur), XtWindow(popup_erreur), hand_cursor);
}

/*
 * fonctions appelees dans lecteur.c
 */

/* validation/invalidation */

void lecteur_valide (flag)
char flag;
{
    XtSetSensitive (lecteur, flag);
#ifndef LOW_MEMORY
    if (!low_memory)
	XtSetSensitive (btn_lecteur, flag);
#endif /* LOW_MEMORY */
}

/* Affiche/efface le lecteur */

void efface_lecteur ()
{
    XtPopdown (popup_lecteur);
}

void affiche_lecteur ()
{
    XtPopup (popup_lecteur, XtGrabNone);    
}

/* Affiche les compteurs */

void affiche_compteur_lecteur_caractere ()
{
    sprintf (buf, "%05d", numero_caractere_courant);

    n = 0;
    XtSetArg (args[n], XtNlabel, buf); n++;
    XtSetValues (compteur_lecteur_caractere, args, n);
}

void affiche_compteur_lecteur_page ()
{
    sprintf (buf, "%03d", numero_page_courante);

    n = 0;
    XtSetArg (args[n], XtNlabel, buf); n++;
    XtSetValues (compteur_lecteur_page, args, n);
}

/* Force le bouton STOP valide si appel de stop_rapide() */

void activation_bouton_stop ()
{
    /* Valide le STOP, invalide la PLAY */
    n = 0;
    XtSetArg (args[n], XtNstate, False); n++;
    XtSetValues (play_lecteur, args, n);
    n = 0;
    XtSetArg (args[n], XtNstate, True); n++;
    XtSetValues (stop_lecteur, args, n);
}
