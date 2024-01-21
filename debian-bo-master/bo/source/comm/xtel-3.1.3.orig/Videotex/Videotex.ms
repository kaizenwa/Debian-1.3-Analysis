\"
\" Doc du widget Videotex (macros ms)
\" $Id: Videotex.ms,v 1.3 1996/10/31 13:00:03 pierre Exp $
\" 
.if n \{
.nr LL 79n
.ll 79n
.nr FL 79n
.nr LT 79n
.\}
.nr PS 11
.de Ds
.LP
.in +0.6i
.LP
..
.de De
.in -0.6i
..
.de Nf
.cs R 20
\s-2
.nf
..
.de Fi
\s+2
.fi
.cs R
..
\"
\"
.ce 7
\s+5\fBWidget Videotex\fP\s-5
.sp
Octobre 1996
.sp
\fIPierre Ficheux\fP
.sp
LECTRA SYSTEMES
.sp 0
Service Recherche et Développement
.sp 0
Z.I. Marticot 33610 CESTAS
.sp
E-mail: pierre@rd.lectra.fr
.sp 4
.NH
Présentation
.PP
Le widget \fBVideotex\fP permet de créer dans une application X utilisant la 
librairie des \fIIntrinsics\fP (Xt) un écran d'émulation vidéotex (compatible 
Minitel 2). L'émulation fonctionne sur un écran couleur, monochrome ou à niveaux de
gris.
.PP
Le mode \fI80 colonnes\fP (mode \fItélé-informatique\fP) du minitel 
n'est pas géré par le widget (il doit être géré dans l'application). On peut 
utiliser ce widget dans une application MOTIF si l'on compile avec le flag
\fI-DUSE_MOTIF\fP. La différence réside principalement dans la définition des
\fIKeySyms\fP (qui sont spéciaux sous MOTIF).
.PP
Le programme suivant (présent sur le répertoire \fIVideotex\fP) peut être considéré 
comme un émulateur minimal (décodage d'un fichier vidéotex) :
.sp
.Nf

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

.Fi
.NH
La classe Videotex
.NH 2
Introduction
.PP
Le widget Videotex implémente la classe \fBvideotexWidgetClass\fP. La structure du
widget est conforme à la structure standard des widgets Xt:
.sp
.IP "Videotex.c" 2i
le source du widget
.IP "VideotexP.h" 2i
le fichier d'en-tête \fIprivé\fP du widget
.IP "Videotex.h" 2i
le fichier d'en-tête \fIpublic\fP du widget
.sp
.PP
Le widget Vidéotex peut donc être utilisé dans une application \fIOSF-Motif\fP ou
bien \fIAthena (Xaw)\fP. 
.PP
Le widget utilise des fontes spécifiques (fontes vidéotex) fournies dans la 
distribution de \fIxtel-3.0\fP sur le repertoire \fPfonts\fI. 
.sp
.NH 2
Les ressources du widget
.PP
.IN "Panner widget" "resources"
.ps 9
.nr PS 9
.vs 11
.nr VS 11
.TS H
expand;
lw(1.5i) lw(1.5i) lw(1i) lw(2i).
_
.sp 3p
.TB
Name	Class	Type	Default Value
.sp 3p
_
.TH
.R
.sp 3p
petiteFonte	PetiteFonte	Boolean	False
nGris	NGris	Boolean	False
connecte	Connecte	Boolean	False
fdConnexion	FdConnexion	int	-1
commandeDeconnexion	CommandeDeconnexion	String	NULL
modeCallback	ModeCallback	Callback	NULL
enregCallback	EnregCallback	Callback	NULL
modeVideotex	ModeVideotex	Boolean	True
enqROM	EnqROM	String	Cv1
.sp 3p
_
.TE
.ps 11
.nr PS 11
.vs 13
.nr VS 13
.Ac
.IP petiteFonte 2i
utilisation de la fonte 8x10 points à la place de la fonte 16x20.
.sp
.IP nGris 2i
l'émulation utilise des niveaux de gris à la place des couleurs (sauf sur 
un écran monochrome bien sûr !)
.sp
.IP connecte 2i
indique au widget si il est connecté à la ligne ou non
.sp
.IP fdConnexion 2i
valeur du \fIfile-descriptor\fP Unix associé à la connexion
.sp
.IP commandeDeconnexion 2i
chaine de caractères à envoyer sur le \fIdescripteur de fichier\fP précédent 
sur une demande de déconnexion modem.
.sp
.IP modeCallback 2i
callback appelé lors du passage en mode télé-informatique
.sp
.IP enregCallback 2i
callback appelé si l'on veut enregistrer les commandes Minitel (SUITE, ENVOI, 
etc...)
.sp
.IP modeVideotex 2i
mode de fonctionnement (Vidéotex si True)
.sp
.IP enqROM 2i
identificateur à renvoyer sur un ENQROM (le widget ajoute SOH et EOT)
.sp
.NH 2
Les actions du widget
.sp
.PP
Le widget \fBVideotex\fP reconnait les actions suivantes dans le cas de 
l'utilisation du toolkit Athena :
.sp
.Nf
<Btn1Down>:	reconnaissance-commande()
<Btn3Down>:	conversion-ascii-page()
<Key>Return:	emission-commande(A)
<Key>Prior:	emission-commande(B)
<Key>Insert:	emission-commande(C)
<Key>End:	emission-commande(D)
<Key>Delete:	emission-commande(E)
<Key>Home:	emission-commande(F)
Ctrl<Key>H:	emission-commande(G)
<Key>Next:	emission-commande(H)
.Fi
.PP
Il reconnait les actions suivantes dans le cas de MOTIF :
.sp
.Nf
<Btn1Down>:		reconnaissance-commande()
<Btn3Down>:		conversion-ascii-page()
<Key>Return:		emission-commande(A)
<Key>osfPageUp:		emission-commande(B)
<Key>osfInsert:		emission-commande(C)
<Key>osfEndLine:	emission-commande(D)
<Key>osfDelete:		emission-commande(E)
<Key>osfBeginLine:	emission-commande(F)
Ctrl<Key>H:		emission-commande(G)
<Key>osfPageDown:	emission-commande(H)
.Fi
.sp
.IP reconnaissance-commande() 2i
le clic avec le bouton 1 dans une zone de l'écran Vidéotex retourne la chaine de
caractères complète pointée par la souris (on tente de reconnaitre les commandes
du Minitel \fIENVOI\fP, \fIRETOUR\fP, ...)
.sp
.IP emission_commande(chaine) 2i
permet d'associer des touches du clavier aux commandes du clavier minitel
.sp
.NH 2
Les fonctions publiques du widget
.sp
.PP
.FD 0
void videotexInit(\fIw\fP)
.br
Widget \fIw\fP;
.FN
.IP \fIw\fP 1i
spécifie le widget Videotex
.PP
Initialise l'émulation dans le widget
.sp 2
.FD 0
void videotexDecode(\fIw\fP, \fIc\fP)
.br
Widget \fIw\fP;
.br
char \fIc\fP;
.FN
.IP \fIw\fP 1i
spécifie le widget Videotex
.IP \fIc\fP 1i
le caractère à décoder
.PP
Effectue le décodage du caractère \fIc\fP dans l'écran du widget
Videotex \fIw\fP
.sp 2
.FD 0
Pixmap videotexPixmapEcranSauve (\fIw\fP)
.br
Widget \fIw\fP;
.FN
.IP \fIw\fP 1i
spécifie le widget Videotex
.PP
Retourne le \fIPixmap\fP de sauvegarde de l'écran (utilisé si le serveur X ne fait
pas de \fIbacking-store\fP)
.sp 2
.FD 0
void videotexSauveEcran (\fIw\fP)
.br
Widget \fIw\fP;
.FN
.IP \fIw\fP 1i
spécifie le widget Videotex
.PP
Force la sauvegarde de l'écran vidéotex
