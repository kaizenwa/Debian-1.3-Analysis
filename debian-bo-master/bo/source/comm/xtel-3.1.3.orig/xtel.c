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
static char rcsid[] = "$Id: xtel.c,v 1.15 1997/01/08 15:03:40 pierre Exp $";

/*
 * programme principal 
 */

#define EXTERN
#include "xtel.h"
#include <X11/Shell.h>

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

#include "patchlevel.h"
#include "bitmaps/xtel.bit"
#include "bitmaps/teleph.xbm"
#include "bitmaps/teleph_mask.xbm"

#ifndef NO_XPM
#include <X11/xpm.h>
#include "pixmaps/xtel.xpm"
#endif /* NO_XPM */

static Cursor teleph_cursor;
static Pixmap pixmap_icone = None;
static Boolean flag_execution = False;
static char *procedure_executee = NULL;
static char *copyright_string = "(C)1991-96 LECTRA SYSTEMES & Pierre Ficheux";

/* liste des noms des boutons MINITEL */
struct definition_touche touches[] = {
                          {"videotex", "V"},
                          {"teleinfo_ascii", "A"},
                          {"teleinfo_fr", "F"},
			  {"blanc", NULL},
                          {"envoi", "A"},
			  {"retour", "B"},
			  {"repetition", "C"},
			  {"guide", "D"},
			  {"annulation",  "E"},
			  {"sommaire", "F"},
			  {"correction", "G"},
			  {"suite", "H"},
			  {"blanc", NULL},
			  {"connexion_fin", "I"},
			};

/*
 * Options (ressources) propres a XTEL
 *
 *	-serveur	*serveur		specifie la machine serveur
 *	-service	*service		specifie le service
 *	-petite		*petiteFonte		taille de la fonte
 *	-ng		*nGris			force les niveaux de gris 
 *			*commandeImpression	comme son nom l'indique
 *			*nomService		nom du service tcp
 *      -low		*lowMemory		pas de jolis boutons
 */
static Boolean defaultFalse = False;

#define XtNserveur	"serveur"
#define XtCServeur	"Serveur"

#define XtNpetiteFonte	"petiteFonte"
#define XtCPetiteFonte	"PetiteFonte"

#define XtNlowMemory	"lowMemory"
#define XtCLowMemory	"LowMemory"

#define XtNnGris	"nGris"
#define XtCNGris	"NGris"

#define XtNcommandeImpression 		"commandeImpression"
#define XtCCommandeImpression 		"CommandeImpression"

#define XtNcommandeImpressionAscii 	"commandeImpressionAscii"
#define XtCCommandeImpressionAscii 	"CommandeImpressionAscii"

#define XtNnomService			"nomService"
#define XtCNomService			"NomService"

#define XtNboutonRaccrocher		"boutonRaccrocher"
#define XtCBoutonRaccrocher		"BoutonRaccrocher"

#define XtNstring1			"string1"
#define XtNstring2			"string2"
#define XtNstring3			"string3"
#define XtNstring4			"string4"
#define XtNstring5			"string5"
#define XtNstring6			"string6"
#define XtNstring7			"string7"
#define XtNstring8			"string8"
#define XtNstring9			"string9"
#define XtNerreur1			"erreur1"

static XtResource ressources[] = {
    {
	XtNserveur,
	XtCServeur,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, serveur),
	XtRString, "ondee"
	},
    {
	XtNnGris,
	XtCNGris,
	XtRBoolean, sizeof(Boolean),
	XtOffsetOf(ressources_xtel, nGris),
	XtRBoolean, (XtPointer) &defaultFalse
	},
    {
	XtNpetiteFonte,
	XtCPetiteFonte,
	XtRBoolean, sizeof(Boolean),
	XtOffsetOf(ressources_xtel, petiteFonte),
	XtRBoolean, (XtPointer) &defaultFalse
	},
    {
	XtNcommandeImpression,
	XtCCommandeImpression,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, commandeImpression),
	XtRString, "lpr -h %s"
	},
    {
	XtNcommandeImpressionAscii,
	XtCCommandeImpressionAscii,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, commandeImpressionAscii),
	XtRString, "lpr -h %s"
	},

    {
	XtNnomService,
	XtCNomService,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, nomService),
	XtRString, "xtel"
	},
    {
	XtNboutonRaccrocher,
	XtCBoutonRaccrocher,
	XtRBoolean, sizeof(Boolean),
	XtOffsetOf(ressources_xtel, boutonRaccrocher),
	XtRBoolean, (XtPointer) &defaultFalse
	},
    {
	XtNlowMemory,
	XtCLowMemory,
	XtRBoolean, sizeof(Boolean),
	XtOffsetOf(ressources_xtel, lowMemory),
	XtRBoolean, (XtPointer) &defaultFalse
	},
    {
	XtNstring1,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, string[0]),
	XtRString, (XtPointer) "String 0"
	},
    {
	XtNstring2,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, string[1]),
	XtRString, (XtPointer) "String 1"
	},
    {
	XtNstring3,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, string[2]),
	XtRString, (XtPointer) "String 2"
	},
    {
	XtNstring4,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, string[3]),
	XtRString, (XtPointer) "String 3"
	},
    {
	XtNstring5,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, string[4]),
	XtRString, (XtPointer) "String 4"
	},
    {
	XtNstring6,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, string[5]),
	XtRString, (XtPointer) "String 5"
	},
    {
	XtNstring7,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, string[6]),
	XtRString, (XtPointer) "String 6"
	},
    {
	XtNstring8,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, string[7]),
	XtRString, (XtPointer) "String 7"
	},
    {
	XtNstring9,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, string[8]),
	XtRString, (XtPointer) "String 8"
	},
    {
	XtNerreur1,
	XtCString,
	XtRString, sizeof(String),
	XtOffsetOf(ressources_xtel, erreur[0]),
	XtRString, (XtPointer) "Erreur 0"
	}
  };

/*
 * Sauve l'ecran
 */
void sauve_l_ecran (w, pevent, params, nb_params)
Widget w;
XEvent *pevent;
String  *params;
Cardinal nb_params;
{
    videotexSauveEcran (ecran_minitel);
}

/* 
 * fonction d'init 
 */
void init_xtel ()
{
    videotexInit (ecran_minitel);
    clavier_minitel_valide (False);
    XFlush (XtDisplay(ecran_minitel));
}

/* Handler d'erreur X */
static int xtel_X_error_handler (display, err)
Display *display;
XErrorEvent *err;
{
    char msg[80];
    
    XGetErrorText (display, err->error_code, msg, 80);
    fprintf (stderr, "\n\nXTel ===> Erreur X: %s\n\n", msg);
    fprintf (stderr, "\tVerifiez votre environnement X, si ce comportement s'avere\n");
    fprintf (stderr, "\tetre un bug de XTel, executez XTel sous debugger, posez un\n");
    fprintf (stderr, "\tpoint d'arret sur la fonction \"xtel_X_error_handler\" et\n");
    fprintf (stderr, "\tenvoyez le detail de la pile d'appels a \"pierre@rd.lectra.fr\".\n\n\tMerci et joyeuses Paques.\n\n");
    
}


#ifndef NO_XPM

/* Construction d'icone XPM, pique dans xpaint ! */
static void SetIconImage(w)
Widget w;
{
    static Pixmap	icon = None;
    Window		iconWindow;
    Screen		*screen = XtScreen(w);
    Display		*dpy = XtDisplay(w);
    XpmAttributes xpma;
	
    xpma.valuemask = XpmReturnInfos;

    /*
     **  Build the icon
     */
    iconWindow = XCreateSimpleWindow(dpy, RootWindowOfScreen(screen),
				     0, 0, /* x, y */
				     1, 1, 0,
				     BlackPixelOfScreen(screen),
				     BlackPixelOfScreen(screen));
    if (icon == None) {
	XpmCreatePixmapFromData(dpy, iconWindow,
				xtel_xpm, &icon, NULL, &xpma);
	XResizeWindow(dpy, iconWindow, xpma.width, xpma.height);
    }

    XSetWindowBackgroundPixmap(dpy, iconWindow, icon);
    XtVaSetValues(w, XtNiconWindow, iconWindow, NULL);
}

#endif /* NO_XPM */

void Usage ()
{
    fprintf (stderr, "Options disponibles:\n");
    fprintf (stderr, "-ng\n\tForce le mode niveaux de gris\n");
    fprintf (stderr, "-petit\n\tSelectionne la petite fonte\n");
    fprintf (stderr, "-br\n\tValide le bouton pour raccrocher\n");
    fprintf (stderr, "-service service_xtel\n\tSelectionne le nom TCP du service XTel\n");
    fprintf (stderr, "-serveur serveur_xtel\n\tSelectionne le nom du serveur XTel\n");
    fprintf (stderr, "-low\n\tPas de jolis boutons\n");
    fprintf (stderr, "--exec-procedure, -e nom_procedure\n\tExecute une procedure\n");

    exit (1);
}

static void execute_procedure (w, proc, pevent)
Widget w;
char *proc;
XEvent *pevent;
{
    Boolean fc;
    int i;

    XtVaGetValues (w, XtNconnecte, &fc, NULL);

    if (!(fc || flag_execution)) {
	flag_execution = True;
	
	/* Composition du service */
	start_procedure ((i = get_proc_index(proc)));
	init_xtel ();
	connexion_service (procedures[i].service);
    }
}

/* Affichage/effacement du telephone de connexion */
void affiche_telephone ()
{
#ifdef LOW_MEMORY
    XDefineCursor (XtDisplay(ecran_minitel), XtWindow(ecran_minitel), teleph_cursor);
#else
    if (!rsc_xtel.lowMemory)
	XtMapWidget (teleph);
    XDefineCursor (XtDisplay(ecran_minitel), XtWindow(ecran_minitel), teleph_cursor);
#endif
}

void efface_telephone ()
{
#ifdef LOW_MEMORY
    XUndefineCursor (XtDisplay(ecran_minitel), XtWindow(ecran_minitel));
#else
    if (!rsc_xtel.lowMemory)
	XtUnmapWidget (teleph);
    XUndefineCursor (XtDisplay(ecran_minitel), XtWindow(ecran_minitel));
#endif
}

/* Creation du curseur */
void create_teleph_cursor (top)
Widget top;
{
    Pixmap shape, mask;
    XColor white, black;
    Display *dpy = XtDisplay(top);
    int screen = DefaultScreen(dpy);

    shape = XCreatePixmapFromBitmapData (dpy, DefaultRootWindow(dpy), teleph_bits, teleph_width, teleph_height, 1, 0, 1);
    mask = XCreatePixmapFromBitmapData (dpy, DefaultRootWindow(dpy), teleph_mask_bits, teleph_mask_width, teleph_mask_height, 1, 0, 1);
    XParseColor (dpy, DefaultColormap(dpy, screen), "white", &white);
    XParseColor (dpy, DefaultColormap(dpy, screen), "black", &black);
    teleph_cursor = XCreatePixmapCursor (dpy, shape, mask, &black, &white, 0, 0);
}

/*
 * Programme principal
 */
main(ac, av)
int ac;
char **av;
{
    Widget topLevel;
    unsigned char c;
    static char l;
    char buf[256];
    struct passwd *pw;
    struct stat statb;
    Boolean flag_point_xtel = False;
    int i;

    if (prototype_xtel == 0) {
	if (PATCHLEVEL != 0)
	    printf ("XTEL %d.%d.%d, Emulateur MINITEL %s\n", version_xtel, revision_xtel, PATCHLEVEL, copyright_string);
	else
	    printf ("XTEL %d.%d, Emulateur MINITEL %s\n", version_xtel, revision_xtel, copyright_string);
    }
    else {
	if (PATCHLEVEL != 0)
	    printf ("XTEL %d.%d.%db%d [beta], Emulateur MINITEL %s\n", version_xtel, revision_xtel, PATCHLEVEL, prototype_xtel, copyright_string);
	else
	    printf ("XTEL %d.%db%d [beta], Emulateur MINITEL %s\n", version_xtel, revision_xtel, prototype_xtel, copyright_string);
    }

    taille_zone_enregistrement = 1000;
    mode_emulation = MODE_VIDEOTEX;

#ifdef NO_NETWORK
    unaddr.sun_family = AF_UNIX;
    strcpy (unaddr.sun_path, XTEL_UNIX_PATH);
#endif /* NO_NETWORK */

    /*
     * Allocation de la zone d'enregistrement par defaut
     */

    if ((zone_enregistrement = malloc (taille_zone_enregistrement)) == NULL) {
	perror ("malloc");
	exit (1);
    }

    if ((topLevel = init_toolkit (&ac, av)) == NULL) {
	fprintf (stderr, "Erreur initialisation Toolkit, dommage...\n");
	exit (1);
    }

    XSetErrorHandler (xtel_X_error_handler);

    /*
     * Options annexes
     */
    i = 1;
    while (i < ac) {
	if (strcmp (av[i], "-e") * strcmp (av[i], "--exec-procedure") == 0)
	    procedure_executee = av[++i];
	else if (av[i][0] != '-') {
	    strcpy (nom_fichier_charge, av[i]);
	}
	else
	    Usage ();

	i++;
    }

    sprintf (buf, "%s-%d.%d", xtel_basename(av[0]), version_xtel, revision_xtel);
    XtVaSetValues (topLevel, XtNtitle, buf, XtNiconName, buf, NULL);

    /*
     * lecture des ressources specifiques
     */
    XtGetApplicationResources (topLevel, &rsc_xtel, ressources, XtNumber(ressources), NULL, 0);

    /* 
     * Lecture des services disponibles 
     */
#ifdef NO_NETWORK
    if ((socket_xteld = socket (AF_UNIX, SOCK_STREAM, 0))  < 0) {
	perror ("socket");
	exit (1);
    }

    if (connect (socket_xteld, (struct sockaddr *)&unaddr, sizeof(unaddr)) < 0) {
	perror ("connect");
	exit (1);
    }
#else
    if ((socket_xteld = c_clientbyname(rsc_xtel.serveur, rsc_xtel.nomService)) < 0) {
	fprintf (stderr, "Erreur de connexion au serveur XTEL...\n");
	exit (1);
    }
#endif /* NO_NETWORK

    /* Transmet le nom d'utilisateur */
    if ((pw = getpwuid(getuid())) == NULL) {
	perror ("getpwuid");
	exit(1);
    }
    l = strlen (pw->pw_name);
    write (socket_xteld, &l, 1);
    write (socket_xteld, pw->pw_name, l);

    nb_services = 0;

    for(;;) {
	write (socket_xteld, CHAINE_COMMANDE_SERVICE_SUIVANT, 1);
	read (socket_xteld, &c, 1);

	if (c == VALEUR_REPONSE_PLUS_DE_SERVICE)
	    break;

	else { /* transmet la longueur de la chaine */
	    int l = c;

	    if (l != 0) {
		read (socket_xteld, buf, l);
		buf[l] = 0;
		definition_services[nb_services].nom_service = build_name (buf);
		entree_compose[nb_services] = definition_services[nb_services].nom_service;
		write (socket_xteld, CHAINE_COMMANDE_NOM_UUCP, 1);
		read (socket_xteld, &c, 1);
		l = c;
		definition_services[nb_services].nom_uucp = (char *) calloc (1, l+1);
		read (socket_xteld, definition_services[nb_services].nom_uucp, l);
		definition_services[nb_services].nom_uucp[l] = 0;
#ifdef DEBUG
		printf ("%d %s %s\n", nb_services, definition_services[nb_services].nom_service ,definition_services[nb_services].nom_uucp);
#endif
		nb_services++;
	    }
	    else {
#ifdef DEBUG
		fprintf (stderr, "entree %d interdite\n", nb_services);
#endif
	    }
	}
    }

    close (socket_xteld);

    /*
     * Procedures et protocoles
     */
    sprintf (buf, "%s/.xtel", getenv ("HOME"));
    flag_point_xtel = (stat (buf, &statb) == 0);

    /* Procedures */

    /* Ouvre le fichier $HOME/.xtelproc */
    if (flag_point_xtel)
	sprintf (buf, "%s/.xtel/procedures", getenv ("HOME"));
    else
	sprintf (buf, "%s/.xtelproc", getenv ("HOME"));
    nb_procedures = init_procedures (buf);

    /* Verifie la procedure a executer si il y lieu */
    if (procedure_executee && get_proc_index (procedure_executee) < 0) {
	fprintf (stderr, "procedure %s inexistante :-(\n", procedure_executee);
	exit (1);
    }

    /*
     * Protocoles de telechargement
     */
    if (flag_point_xtel)
	sprintf (buf, "%s/.xtel/protocols", getenv ("HOME"));
    else
	sprintf (buf, "%s/.xtelproto", getenv ("HOME"));
    nb_protocoles = init_protocoles (buf, &protocoles[0]);


    /* 
     * Initialisation des widgets 
     */
    if ((ecran_minitel = init_widgets (topLevel)) == NULL) {
	fprintf (stderr, "Erreur initialisation widgets\n");
	exit (1);
    }

    /* Creation du curseur telephone */
    create_teleph_cursor (topLevel);

    XtRealizeWidget(topLevel);

    /*
     * Contruction de l'icone
     */
    XtVaGetValues(topLevel, XtNiconPixmap, &pixmap_icone, NULL);
    if (pixmap_icone == None) {
#ifdef NO_XPM
	XtVaSetValues (topLevel, XtNiconPixmap, XCreateBitmapFromData(XtDisplay(topLevel), XtScreen(topLevel)->root,  xtel_bits, xtel_width, xtel_height), NULL);
#else
	SetIconImage (topLevel);
#endif /* NO_XPM */
    }

    XtAddCallback (ecran_minitel, XtNmodeCallback, (XtCallbackProc)selection_mode_emulation, (XtPointer)"A");
    XtAddCallback (ecran_minitel, XtNenregCallback, (XtCallbackProc)enregistre_caractere, (XtPointer)NULL);
    init_xtel ();

    XtAddEventHandler (topLevel, LeaveWindowMask, False, (XtEventHandler)sauve_l_ecran, NULL);
    if (procedure_executee)
	XtAddEventHandler (ecran_minitel, ExposureMask, False, (XtEventHandler)execute_procedure, (XtPointer)procedure_executee);

    if (nom_fichier_charge[0]) {
	Charge ();
	debut_lecteur ();
    }

    /* attente evenements */
    XtAppMainLoop (app_context);
 }
