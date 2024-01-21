/*
 * src/xio/strings.c, part of Pente (game program)
 * Copyright (C) 1994-1995 William Shubert.
 * See "configure.h.in" for more copyright information.
 */

#include <wms.h>

#ifdef  X11_DISP

#include <but/but.h>
#include <but/text.h>
#include <but/menu.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <abut/abut.h>
#include <abut/msg.h>
#include "../pente.h"
#include "xio.h"

/* Names of the languages...in each language! */
const char  *xioStr_langlist[XIO_LANG_COUNT][XIO_LANG_COUNT+1] = {
  {"English", "French", BUTMENU_OLEND},
  {"Anglais", "Français", BUTMENU_OLEND}};

/*
 * Fonts to use in each language.
 *   If more than one will do, put the first choice with a "/" then the next,
 *   then the next, etc.
 */
char  *xioStr_mfonts[] = {
  "-bitstream-charter-medium-r-normal--%d-*-*-*-*-*-*-*",
  "-bitstream-charter-medium-r-normal--%d-*-*-*-*-*-*-*"};
char *xioStr_bfonts[] = {
  "-bitstream-charter-black-r-normal--%d-*-*-*-*-*-*-*/"
  "-bitstream-charter-bold-r-normal--%d-*-*-*-*-*-*-*",
  "-bitstream-charter-black-r-normal--%d-*-*-*-*-*-*-*/"
  "-bitstream-charter-bold-r-normal--%d-*-*-*-*-*-*-*"};

/* Buttons in the setup window. */
char  *xioStr_pentesetup[] = {"Pente Setup", "Configuration de Pente"};
char  *xioStr_language[] = {"Language", "Langue"};
char  *xioStr_color[] = {"Color", "Couleur"};
char  *xioStr_showThink[] = {"Show thinking", XIO_COMPCHAR " visible"};
char  *xioStr_soundvol[] = {"Sound Volume", "Volume Sonore"};
char  *xioStr_off[] = {"Off", "Aucun"};
char  *xioStr_max[] = {"Max", "Max"};
char  *xioStr_transtab[] = {"Transposition Table", "Table de Transposition"};
char  *xioStr_size[] = {"Size:", "Taille:"};
char  *xioStr_autosize[] = {"Autosize", "Taille Automatique"};
char  *xioStr_netplay[] = {"Network Play", "Jeu en Réseau"};
char  *xioStr_enabled[] = {"Enabled", "Activé"};
char  *xioStr_socket[] = {"Port:", "Port:"};
char  *xioStr_disabled[] = {"Disabled", "Désactivé"};
char  *xioStr_connect[] = {"Connect", "Connexion"};
char  *xioStr_disconnect[] = {"Disconnect", "Déconnexion"};
char  *xioStr_noSockets[] = {
  "Sorry, this program was compiled on a system that doesn't support "
    "sockets.  You cannot play networked.",
  "Désolé, ce programme a été compilé sur un système ne "
    "gérant pas les sockets.  Vous ne pouvez pas jouer en réseau."};

/* Labels on the board. */
char  *xioStr_turn[] = {"Turn", "Tour"};
char  *xioStr_captures[] = {"CAPTURES", "CAPTURES"};
char  *xioStr_copyright[] = {"Copyright \251 1994-1995 William Shubert.",
			    "Copyright \251 1994-1995 William Shubert."};
char  *xioStr_nowarr[] = {"Pente comes with ABSOLUTELY NO WARRANTY and "
			     "is free software.",
			     "Pente est fourni sans AUCUNE GARANTIE et "
			     "est un logiciel gratuit"};
char  *xioStr_seehelp[] = {"Please see the \"Copying and Non-Warranty\" help "
			      "page for details.",
			    "Veuillez consulter la page d'aide \""
			    "Copie et Non-Garantie\" pour plus de détails."};

/* Button labels. */
char  *xioStr_undo[] = {"Undo", "Défaire"};
char  *xioStr_start[] = {"Start", "Débuter"};
char  *xioStr_stop[] = {"Stop", "Arrêter"};
char  *xioStr_continue[] = {"Continue", "Continuer"};
char  *xioStr_redo[] = {"Redo", "Refaire"};
char  *xioStr_setup[] = {"Setup", "Config."};
char  *xioStr_help[] = {"Help", "Aide"};
char  *xioStr_quit[] = {"Quit", "Quitter"};
char  *xioStr_ok[] = {"OK", "OK"};
char  *xioStr_cancel[] = {"Cancel", "Annuler"};
char  *xioStr_netConn[] = {"Network Connect", "Connexion Réseau"};
char  *xioStr_machine[] = {"Machine:", "Machine:"};
char  *xioStr_reject[] = {"Reject", "Rejet"};
char  *xioStr_noRemote[] = {"Sorry, Pente " VERSION " doesn't have "
			       "networking yet.  Please wait for a later "
			       "version to arrive!",
			     "Désolé, Pente " VERSION " n'a pas encore "
                               "la fonctionnalité réseau.  Veuillez "
			       "attendre une prochaine version!"};

/* Menu entries. */
char  *xioStr_player1[] = {"Player 1", "Joueur 1"};
char  *xioStr_player2[] = {"Player 2", "Joueur 2"};
char  *xioStr_human[] = {"Human", "Humain"};
char  *xioStr_remote[] = {"Remote", "Distant"};
char  *xioStr_comp1[] = {XIO_COMPCHAR " Simple",
			    XIO_COMPCHAR " Simple"};
char  *xioStr_comp2[] = {XIO_COMPCHAR " Easy",
			    XIO_COMPCHAR " Facile"};
char  *xioStr_comp3[] = {XIO_COMPCHAR " Med",
			    XIO_COMPCHAR " Moyen"};
char  *xioStr_comp4[] = {XIO_COMPCHAR " Hard",
			    XIO_COMPCHAR " Difficile"};
char  *xioStr_comp5[] = {XIO_COMPCHAR " Expert",
			    XIO_COMPCHAR " Expert"};
char  *xioStr_comp6[] = {XIO_COMPCHAR " Master",
			    XIO_COMPCHAR " Maître"};

/* Messages for message windows. */
char  *xioStr_notEnoughColors[] = {"Pente couldn't allocate enough colors.  "
				     "It will still run, but in black and "
				     "white.",
				   "Pas assez de couleurs."};

/* Messages to be used for network play. */
char  *xioStr_netOffer[] = {"Player \"%s\" wants to play pente against "
			       "you.  Accept?", "Le joueur \"%s\" souhaite "
			       "jouer à pente contre vous. Acceptez-vous ?"};
char  *xioStr_netReject[] = {"Player \"%s\" rejected your offer to play "
				"pente.", "Le joueur \"%s\" a rejeté votre "
			        "offre de jouer à pente."};
char  *xioStr_netWait[] = {"Waiting for connection to be accepted.  Press "
			      "\"Cancel\" to give up.",
			      "En attente de la validation de la connexion."
			      "  Appuyer sur \"Annuler\" pour abandonner."};
char  *xioStr_netDisconnect[] = {"Your connection with \"%s\" has been "
				    "disconnected because of \"%s\".",
				    "La connexion avec \"%s\" a été "
				    "interrompue à cause de \"%s\"."};
char  *xioStr_netBusy[] = {"Sorry, player \"%s\" is already playing a "
			      "networked game of pente.",
			      "Désolé, le joueur \"%s\" est déjà en train de "
			      "jouer une partie de pente en réseau."};
char  *xioStr_netClosed[] = {"Your opponent has closed your network "
			       "connection.",
			     "Votre adversaire a fermé la connexion."};

/* Error messages! */
char  *xioStr_errProtSocket[] = {"You can't use local port %s.  Ports up "
				    "to %d are reserved for superuser access "
				    "only.",
				    "Vous ne pouvez pas utiliser le port %s."
				    "Les ports jusqu'à %d sont réservés à "
				    "l'accès pour le super utilisateur "
				    "uniquement."};
char  *xioStr_errSockInUse[] = {"Port %s is already in use on this "
				   "machine.  If you want to play networked, "
				   "you will have to choose a different "
				   "port number.", "Le port %s est déjà "
				   "utilisé sur cette machine. Si vous "
				   "souhaitez jouer en réseau, vous devez "
				   "choisir un numéro de port différent."};
char  *xioStr_errLSockGeneric[] = {"Error opening local port %s: \"%s\".",
				      "Erreur d'ouverture du port local %s: "
				      " \"%s\"."};
char  *xioStr_errSockNumInvalid[] = {
  "\"%s\" is not a valid port.  Ports are integers in the "
    "range from 1 through %d.  The default Pente port is 15023; "
    "you should use that unless port 15023 is already in use.",
    "\"%s\" n'est pas un port valide. Les ports valides sont les "
    "entiers qui vont " "de 1 à %d.  Le port de Pente par défaut "
    "est 15023; c'est celui que vous devriez utiliser à moins que "
    "le port 15023 soit déjà utilisé."};
char  *xioStr_errRSockGeneric[] = {"Error \"%s\" occurred while opening "
				      "port %s on machine \"%s\".",
				      "L'erreur \"%s\" est survenue lors de "
				      "l'ouverture du port %s sur la "
				      "machine \"%s\"."};
char  *xioStr_errRSockRefused[] = {"Your connection to machine \"%s\" port "
				      "%s was refused.  Probably there is no "
				      "Pente running that is set up to "
				      "listen on that port.",
				      "La connexion à la machine \"%s\" sur "
				      "le port %s a été refusée. Il n'y a "
				      "probablement aucun Pente de "
				      "configuré pour écouter sur ce port."};
char  *xioStr_errHostTemp[] = {"Machine \"%s\" couldn't be found.  This may "
				  "be a temporary situation; you may want to "
				  "try again later.",
				  "La machine \"%s\" n'a pas pu être "
				  "localisée.  Il se peut que ce soit un "
				  "problème temporaire; vous "
				  "pouvez réessayer plus tard."};
char  *xioStr_errHostPerm[] = {"Machine \"%s\" couldn't be found.",
			        "Impossible de localiser la machine \"%s\"."};


/* Help */
char  *xioStr_hmenu[] = {"Help topics", "Sujets D'aide"};
char  *xioStr_phelp[] = {"Program Help", "Aide Du Programme"};
char  *xioStr_ghelp[] = {"How To Play Pente", "Comment Jouer A Pente"};
char  *xioStr_shelp[] = {"The Setup Window", "La Fenêtre De Configuration"};
char  *xioStr_nhelp[] = {"Network Play", "Jeu En Réseau"};
char  *xioStr_cphelp[] = {"About The Computer Players", "A Propos Des "
			     "Joueurs Gérés Par L'ordinateur"};
char  *xioStr_ahelp[] = {"About the Author", "A Propos De L'auteur"};
char  *xioStr_chelp[] = {"Copying and Non-Warranty", "Copie Et Non-Garantie"};

/* Help on using the program. */
static xio_tb_t  proghelp_eng[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "By Bill Shubert"},
  {butText_center, 0, DATE},
  {butText_just, 0, ""},
  {butText_just, 1, "Help on the program Pente"},
  {butText_just, 0, "   This help page explains how to use the features of "
     "Pente version " VERSION ".  At the top of this help window is a menu; "
     "click on it and drag with the mouse to pop up a list of other help "
     "pages available."},
  {butText_just, 0, ""},
  {butText_just, 0, "   To scroll the text in this help window, you may "
     "either use the slider bar to the left or right-click and drag on the "
     "text itself."},
  {butText_just, 0, ""},
  {butText_just, 0, "   The main window of Pente shows the pente board with "
     "some controls at the bottom.  The board itself looks like a regular "
     "pente board and should be mostly self-explanatory.  Simply click on an "
     "intersection to place a piece there.  To either side of "
     "the pente board are pits where captured pieces are kept.  Below the "
     "board are eight controls.  These are the player 1 selector, the player "
     "2 selector, the undo button, the redo button, the start/stop button, "
     "the setup button, the help button, and the quit button."},
  {butText_just, 0, ""},
  {butText_just, 0, "   The player 1 and player 2 selectors are menus "
     "found on the far left and far right of the controls.  "
     "When the menu is opened by pressing the left mouse button, there are "
     "several choices.  The first choice, "
     "\"Human\", lets you play that player.  For example, if both selectors "
     "are set to \"Human\", then two people can use the same computer to "
     "play pente against each other."},
  {butText_just, 0, "   The next selection in a player selector is "
     "\"Remote\".  If your Pente program is configured correctly, then this "
     "option can be used to play pente against somebody on another computer.  "
     "See the networking help page for more information on this."},
  {butText_just, 0, "   The bottom six options in the player selectors are "
     "the computer opponents, represented by a " XIO_COMPCHAR " icon and the "
     "difficulty of that opponent.  Selecting one of these players makes the "
     "computer play for that player.  For example, setting the player 1 "
     "selector to \"Human\" and the player 2 selector to \"" XIO_COMPCHAR
     " Simple\" will let you play as player 1, and make the computer play "
     "for player 2.  This particular computer opponent is very easy to beat.  "
     "The computer players are listed from easiest to beat to hardest to "
     "beat in the player selector."},
  {butText_just, 0, ""},
  {butText_just, 0, "   In the upper center of the controls is the start/"
     "stop button.  This button will start a new game when no game is "
     "being played, or stop a game in progress.  The label on the button "
     "will change to reflect it's action."},
  {butText_just, 0, "   Sometimes the game will be paused (for example, if "
     "you change players in the middle of a game).  When this happens the "
     "label on the start/stop button will change to \"Continue\".  Pressing "
     "the button then will resume the game."},
  {butText_just, 0, ""},
  {butText_just, 0, "   To either side of the start/stop button are the "
     "undo and redo buttons.  Pressing undo will take back moves, one at a "
     "time.  Pressing redo will replay moves, one at a time.  Using these "
     "buttons you can review a game to see what moves led to the current "
     "position."},
  {butText_just, 0, "   Undo can also be used to take back a move.  If you "
     "press undo until the move you want to take back is gone, then move "
     "somewhere else, the game will continue from there."},
  {butText_just, 0, ""},
  {butText_just, 0, "   The help button is in the bottom center of the "
     "controls.  Since you're reading this, you must already know how to "
     "use this button."},
  {butText_just, 0, ""},
  {butText_just, 0, "   The setup button, to the left of the help button, "
     "lets you change the configuration of Pente.  See the setup window "
     "help page for more information."},
  {butText_just, 0, ""},
  {butText_just, 0, "   The quit button, which is to the right of the help "
     "button, exits Pente immediately."},
  {butText_just, 0, ""},
  {butText_just, 0, "   There are some keyboard accelerators available as "
     "well.  Pressing \"Enter\" or \"Return\" is the same as pressing the "
     "\"OK\" button in a window.  Pressing \"H\" or \"Help\" is the same as "
     "pressing the \"Help\" button.  Pressing \"Q\" in the main window will "
     "quit the program.  Pressing \"S\" will toggle sound on and off.  "
     "The left and right arrow keys will undo and redo moves."},
  {butText_just, 0, "   If you press \"Q\" by accident and lose your game, "
     "don't worry!  Just restart Pente and your game will still be there, "
     "exactly where you left off."},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};
static xio_tb_t  proghelp_french[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "Par Bill Shubert"},
  {butText_center, 0, DATE_FRENCH},
  {butText_just, 0, ""},
  {butText_just, 1, "Aide du programme Pente"},
  {butText_just, 0, "   Cette page d'aide explique comment utiliser les "
     "caractéristiques de Pente version " VERSION ".  En haut de cette fenêtre "
     "d'aide se trouve un menu; cliquer dessus et tirer avec la "
     "souris pour accéder à une liste des pages d'aide disponibles."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Pour faire défiler le texte dans cette fenêtre "
     "d'aide, vous pouvez soit utiliser la barre de défilement à gauche, "
     "soit cliquer avec le bouton droit et tirer le texte lui-même."},
  {butText_just, 0, ""},
  {butText_just, 0, "   La fenêtre principale de Pente montre le plateau "
     "de jeu de pente comportant quelques boutons de contrôle en bas. "
     "Le plateau de jeu lui-même ressemble à un plateau de jeu de "
     "pente habituel et devrait être suffisamment explicite. "
     "Il suffit de cliquer sur une "
     "intersection pour mettre un jeton à cet endroit. On trouve de chaque "
     "côté du plateau du jeu de pente des renfoncements où les jetons "
     "capturés sont conservés. On trouve sous le plateau de jeu huit "
     "boutons de contrôle. Il s'agit du sélectionneur du joueur 1, du "
     "sélectionneur du joueur 2, du bouton pour défaire un coup, du "
     "bouton pour refaire un coup, du bouton de début/arrêt de "
     "partie, du bouton de configuration, "
     "du bouton d'aide et du bouton pour quitter."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Les sélectionneurs du joueur 1 et du joueur 2 sont "
     "des menus qui se trouvent à l'extrême gauche et à l'extrême droite "
     "des boutons de contrôle.  Une fois le menu ouvert en appuyant sur "
     "le bouton gauche de la souris, on a accès à plusieurs choix. "
     "Le premier choix \"Humain\", vous "
     "permet d'utiliser ce joueur. Par exemple, lorsque "
     "les deux sélectionneurs "
     "sont positionnés sur \"Humain\", deux personnes peuvent utiliser le "
     "même ordinateur pour jouer l'un contre l'autre."},
  {butText_just, 0, "   Le choix suivant du sélectionneur de joueur est "
     "\"Distant\". Si le programme Pente est configuré correctement cette "
     "option peut être utilisée pour jouer à pente contre quelqu'un sur "
     "un autre ordinateur.  Consultez la page d'aide concernant le réseau pour "
     "obtenir de plus amples information."},
  {butText_just, 0, "   Les six dernières options du sélectionneur de joueur "
     "sont les adversaires gérés par l'ordinateur, elles sont représentées "
     "par une icône " XIO_COMPCHAR " suivi de la difficulté de cet "
     "adversaire. La sélection d'un de ces joueurs permet de jouer contre "
     "ce joueur géré par l'ordinateur. Par exemple, si on positionne le "
     "sélectionneur du joueur 1 à \"Humain\" et le sélectionneur du joueur 2 à "
     "\"" XIO_COMPCHAR " Simple\", on jouera alors en tant que joueur 1, et "
     "l'ordinateur gérera le joueur 2. Cet adversaire là est très facile à "
     "battre. Les joueurs gérés par l'ordinateur sont affichés dans l'ordre "
     "croissant de difficulté dans le sélectionneur de joueur."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Le bouton de début/arrêt se trouve en haut et au "
     "milieu des boutons de contrôle. Ce bouton permet débuter une nouvelle "
     "partie lorsqu'aucune partie n'est en cours, ou d'arrêter un jeu en "
     "cours. Le nom de ce bouton change suivant le contexte."},
  {butText_just, 0, "   Parfois le jeu est en pause (par exemple, si vous "
     "changez les joueurs en plein milieu d'une partie). Dans ce cas, le nom "
     "du bouton de début/arrêt se change en \"Continuer\".  L'appui sur ce "
     "bouton permet alors de reprendre la partie."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Les boutons défaire et refaire se trouvent des deux "
     "côtés du bouton de début/arrêt. L'appui sur défaire permet de "
     "revenir en arrière, d'un coup à la fois. L'appui sur refaire permet "
     "de rejouer les coups, un à la fois. En utilisant ces boutons "
     "vous pouvez compulser une partie afin de voir quels coups ont "
     "conduit à la position actuelle."},
  {butText_just, 0, "   Défaire peut également être utilisé pour revenir "
     "en arrière. Si vous appuyez sur Défaire jusqu'à ce que le coup "
     "que vous souhaitez enlever ne soit plus là, puis si vous bougez "
     "le curseur autre part, alors la partie continuera depuis cette "
     "position."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Le bouton d'aide se trouve au milieu et en bas "
     "des boutons de contrôle. Puisque vous êtes en train de lire ceci, vous "
     "devez déjà savoir comment utiliser ce bouton."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Le bouton de configuration, à gauche du bouton "
     "d'aide, permet de modifier la configuration de Pente. Consultez "
     "la page d'aide de la fenêtre de configuration pour obtenir de "
     "plus amples renseignements."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Le bouton pour quitter, qui se trouve à droite "
     "du bouton d'aide, permet de sortir de Pente immédiatement."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Il existe aussi quelques raccourcis clavier. "
     "L'appui sur \"Entrée\" ou \"Retour Chariot\" revient au même qu'un "
     "appui sur le bouton \"OK\" d'une fenêtre.  L'appui sur \"H\" ou "
     "\"Aide\" revient au même qu'un appui sur le bouton \"Aide\". "
     "L'appui sur \"Q\" dans la fenêtre principale permet de sortir du "
     "programme. L'appui sur \"S\" permet de mettre ou d'enlever le son. "
     "Les touches de flèches gauche et droite permettent de défaire ou "
     "refaire les coups."},
  {butText_just, 0, "   Si vous appuyez sur \"Q\" par mégarde et perdez "
     "votre partie, pas de panique! Il suffit de relancer Pente et votre "
     "partie sera toujours là, exactement où vous en étiez."},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};

static xio_tb_t  gamehelp_eng[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "By Bill Shubert"},
  {butText_center, 0, DATE},
  {butText_just, 0, ""},
  {butText_just, 1, "How to play Pente"},
  {butText_just, 0, "   Pente is the English name of the Asian game of "
     "ni-nuki, which is itself a version of the game of go-moku.  Pente "
     "is played on a 19x19 grid with stones in two different colors.  "
     "Each player chooses one set of stones; then the players take turns "
     "placing their stones on any unoccupied intersection until one player "
     "wins."},
  {butText_just, 0, "   There are two ways to win.  If a player makes five "
     "or more stones in a straight line (across, down, or diagonally), then "
     "that player wins.  Or, if a player captures five pairs of his or her "
     "opponent's stones, that player also wins."},
  {butText_just, 0, "   Stones may be captured in pairs only.  To capture "
     "a pair of stones, a player must place one stone on either side of the "
     "pair.  For example:"},
  {butText_center, 0, XIO_PL1CHAR XIO_PL2CHAR XIO_PL2CHAR},
  {butText_just, 0, "   If another " XIO_PL1CHAR " stone is placed on the "
     "right side of this group, the two center stones will be captured.  "
     "This will count as one capture."},
  {butText_just, 0, "   That's it!"},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};
static xio_tb_t  gamehelp_french[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "Par Bill Shubert"},
  {butText_center, 0, DATE_FRENCH},
  {butText_just, 0, ""},
  {butText_just, 1, "Comment jouer à Pente"},
  {butText_just, 0, "   Pente est le nom Anglais du jeu asiatique "
     "ni-nuki, qui est lui-même une version du jeu go-moku. "
     " Pente se joue sur une grille de 19x19 cases en utilisant des "
     "pierres de deux couleurs différentes.  Chaque joueur choisit un "
     "ensemble de pierres; puis les joueurs placent à tour de rôle leurs "
     "pierres sur les intersections innocupées jusqu'à "
     "ce qu'un des joueurs gagne."},
  {butText_just, 0, "   Il y a deux façons de gagner.  Un joueur peut "
     "gagner s'il aligne (verticalement, horizontalement ou en diagonale) "
     "consécutivement cinq pierres ou plus. Un joueur peut aussi gagner en "
     "capturant cinq paires de pierres de son adversaire."},
  {butText_just, 0, "   Les pierres peuvent être uniquement capturées par "
     "paires. Pour capturer une paire de pierres, un joueur doit placer une "
     "pierre de part et d'autre de la paire. Par exemple:"},
  {butText_center, 0, XIO_PL1CHAR XIO_PL2CHAR XIO_PL2CHAR},
  {butText_just, 0, "   Si une autre " XIO_PL1CHAR " pierre est placée "
     "sur le côté droit de ce groupe, les deux pierres centrales seront "
     "capturées. Cela comptera alors pour une capture."},
  {butText_just, 0, "   Et c'est tout!"},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};

static xio_tb_t  setuphelp_eng[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "By Bill Shubert"},
  {butText_center, 0, DATE},
  {butText_just, 0, ""},
  {butText_just, 1, "Help on the setup window"},
  {butText_just, 0, "   This help page explains the features of the setup "
     "window in Pente.  To see the setup window, press the \"setup\" button "
     "on the main window."},
  {butText_just, 0, "   There are four subsections of the setup window.  "
     "These are the options section, the sound section, the "
     "transposition table section, and the networking section."},
  {butText_just, 0, ""},
  {butText_just, 0, "   The options section, in the upper left portion of "
     "the setup window, has three controls.  There is a \"Language\" menu "
     "that selects which language text messages should appear in.  Currently "
     "the only languages supported are English and French.  If you "
     "know another language and are willing to do the translations, by all "
     "means get in touch with me!  I'd like to translate this program into "
     "as many languages as possible."},
  {butText_just, 0, "   In the options section there is also a checkbox "
     "for color.  If you have a color screen, pressing this checkbox will "
     "switch between using color and black and white."},
  {butText_just, 0, "   The last control in the options section is the "
     "\"Show thinking\" check box.  If this is checked, you can see the "
     "moves that the computer is thinking about when it is playing at "
     "the \"Expert\" and \"Master\" levels.  If this is not checked, you "
     "cannot see the moves that the computer is considering."},
  {butText_just, 0, ""},
  {butText_just, 0, "   The sound section is in the lower left of the "
     "setup window.  It contains one control: a slider which sets the sound "
     "volume.  If you have sound, sliding this control will set how loud the "
     "sound effects should be.  Sliding all the way to zero will turn off "
     "all sound."},
  {butText_just, 0, ""},
  {butText_just, 0, "   The transposition table section is in the upper "
     "right of the setup window.  This is currently disabled."},
  {butText_just, 0, ""},
  {butText_just, 0, "   The network section is in the lower right of the "
     "setup window.  This is currently disabled."},
  {butText_just, 0, ""},
  {butText_just, 0, "   At the bottom of the setup window are two buttons, "
     "labeled \"OK\" and \"Help\".  Pressing \"OK\" closes the setup window "
     "and pressing \"Help\" opens this help page."},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};
static xio_tb_t  setuphelp_french[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "Par Bill Shubert"},
  {butText_center, 0, DATE_FRENCH},
  {butText_just, 0, ""},
  {butText_just, 1, "Aide sur la fenêtre de configuration"},
  {butText_just, 0, "   Cette page d'aide explique les différentes options "
     "de la fenêtre de configuration de Pente. Pour accèder à la fenêtre "
     "de configuration, appuyez sur le bouton \"Config.\" de la fenêtre "
     "principale."},
  {butText_just, 0, "   Il y a quatre parties dans la fenêtre de "
     "configuration.  On y trouve la partie des options, la partie "
     "son, la partie de la table de transposition et la partie "
     "réseau."},
  {butText_just, 0, ""},
  {butText_just, 0, "   La partie des options, en haut et à gauche de la "
     "fenêtre de configuration, contient trois options de contrôle. On trouve "
     "le menu \"Langue\" qui permet de choisir dans quelle langue "
     "les messages s'affichent. Pour l'instant, les seules langues "
     "disponibles sont l'Anglais et le Français.  Si vous connaissez une "
     "autre langue et si vous avez envie de réaliser la traduction, alors "
     "surtout contactez moi!  J'aimerais traduire mon programme dans "
     "autant de langues que possible."},
  {butText_just, 0, "   Dans la partie des options, il y a également une "
     "boîte à cocher pour la couleur. Si vous avez un moniteur couleur, lors "
     "de l'appui sur la boîte à cocher vous pouvez basculer entre la couleur "
     "et le noir et blanc."},
  {butText_just, 0, "   Le dernier bouton do contrôle de la partie des "
     "options est la boîte á cocher \"" XIO_COMPCHAR " visible\".  "
     "Si elle est cochée, vous pouvez visualiser les déplacements "
     "auxquels l'ordinateur réfléchit lorsqu'il joue aux niveaux "
     "\"Expert\" et \"Maître\".  Si elle n'est pas cochée, vous ne "
     "pouvez pas voir les déplacements que l'ordinateur est en train de "
     "considérer."},
  {butText_just, 0, ""},
  {butText_just, 0, "   La partie son se trouve en bas et à gauche "
     "de la fenêtre de configuration. On y trouve une seule option de "
     "contrôle: une barre de défilement qui permet d'ajuster le volume "
     "sonore. Si vous avez du son, en glissant le curseur vous pouvez "
     "spécifiez le volume du son. En glissant le curseur vers zéro "
     "vous pouvez arrêter tout effet sonore."},
  {butText_just, 0, ""},
  {butText_just, 0, "   La partie de la table de transposition se trouve en "
     "haut et à droite de la fenêtre de configuration. Pour l'instant, elle "
     "n'est pas en fonction."},
  {butText_just, 0, ""},
  {butText_just, 0, "   La partie réseau se trouve en bas et à droite de "
     "la fenêtre de configuration. Pour l'instant, elle "
     "n'est pas en fonction."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Tout en bas de la fenêtre de configuration, on "
     "trouve deux boutons dont le nom est \"OK\" et \"Aide\". L'appui sur "
     "\"OK\" ferme la fenêtre de configuration et l'appui sur \"Aide\" "
     "permet d'activer cette page d'aide."},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};

static xio_tb_t  network_eng[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "By Bill Shubert"},
  {butText_center, 0, DATE},
  {butText_just, 0, ""},
  {butText_just, 1, "Setting Up Network Play"},
  {butText_just, 0, "   To play pente against somebody on a different "
     "machine, both players have to start by running Pente (version 2.1.0 or "
     "later).  In the setup window, there's a control panel that lets you set "
     "your port number.  Exactly which port number you choose is pretty "
     "unimportant; they're all the same.  This is the port where your Pente "
     "program will be listening for network games.  You should probably leave "
     "it at 15023, which is the standard Pente port."},
  {butText_just, 0, "   Once both players have run Pente, decide who will "
     "connect and who will listen.  The player who listens must make sure "
     "that remote operation is turned on with the checkbox in the setup "
     "window.  Then the player who will connect can either press the "
     "\"Connect\" button in his setup window or select \"Remote\" on one of "
     "his player menus.  The connecting player then gets a window where he is "
     "asked for the name of the machine to connect to and the port.  He "
     "should type in the name of the listening player's machine and the same "
     "port than the listening player has set in his setup window (this will "
     "be 15023 unless the listening player decided to be different).  After "
     "pressing OK, the two players should be connected and ready to go!"},
  {butText_just, 0, ""},
  {butText_just, 1, "Using Network Play"},
  {butText_just, 0, "   Once the two players are connected, you should see "
     "each other's mice.  Your opponent's mouse will be upside down.  Now "
     "just set one player to \"Remote\" and the other to \"human\" and you'll "
     "be playing against each other."},
  {butText_just, 0, ""},
  {butText_just, 0, "   If a player wishes to stop using network play, they "
     "should press the \"Disconnect\" button in the setup window."},
  {butText_just, 0, ""},

  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};
static xio_tb_t  network_french[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "Par Bill Shubert"},
  {butText_center, 0, DATE_FRENCH},
  {butText_just, 0, ""},
  {butText_just, 1, "Configurer le Jeu en Réseau"},
  {butText_just, 0, "   Pour jouer à pente contre quelqu'un sur une machine "
     "différente, les deux joueurs doivent démarrer Pente (version 2.1.0 ou "
     "plus). Dans la fenêtre de configuration, on trouve un panneau de "
     "contrôle permettant de sélectionner le numéro de port. Peu importe le "
     "numéro de port que vous choisissez; ce sont tous les mêmes. Il "
     "s'agit du port sur lequel le programme Pente va se mettre à l'écoute "
     "pour les jeux en réseau. Vous devriez laisser le numéro de port à "
     "15023, c'est le port standard de Pente."},
  {butText_just, 0, "   Une fois que les deux joueurs ont lancé Pente, il "
     "faut décider qui va se connecter et qui va écouter. Le joueur "
     "qui écoute doit s'assurer que l'option de connexion "
     "est validée par une coche dans la fenêtre de configuration. "
     "Ensuite, le joueur qui se connecte "
     "peut soit appuyer sur le bouton \"Connexion\" de sa fenêtre de "
     "configuration, soit sélectionner \"Distant\" sur l'un des menus "
     "des sélectionneurs de joueur. Le joueur qui se connecte "
     "obtient alors une fenêtre où il "
     "doit saisir le nom de la machine où se connecter ainsi que le port. "
     "Le nom à saisir est celui de la machine du joueur distant et le "
     "port est le même que celui choisi par le joueur distant (ce sera "
     "15023 à moins que le joueur distant en ait décidé autrement). Après "
     "avoir appuyé sur OK, les deux joueurs devraient être connectés et "
     "prêts à jouer!"},
  {butText_just, 0, ""},
  {butText_just, 1, "Utilisation du jeu en réseau"},
  {butText_just, 0, "   Une fois que les deux joueurs sont connectés, "
     "vous devriez voir la souris de chacun des deux. La souris de votre "
     "adversaire est retournée. A présent il suffit de sélectionner l'un des "
     "joueurs comme \"Distant\" et l'autre comme \"Humain\" afin de "
     "jouer l'un contre l'autre."},
  {butText_just, 0, ""},
  {butText_just, 0, "   FRENCH If a player wishes to stop using network play, "
     "they should press the \"Disconnect\" button in the setup window."},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};
  
static xio_tb_t  computer_eng[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "By Bill Shubert"},
  {butText_center, 0, DATE},
  {butText_just, 0, ""},
  {butText_just, 1, "About the Computer Players"},
  {butText_just, 0, "   The computer players use a tree search to determine "
     "their moves.  Making a threat (such as three in a row, open on both "
     "ends) counts as good thing; captures count even better.  So pretty "
     "much they analyze all possible moves, looking for sequences that let "
     "them make lots more threats and captures than their opponents."},
  {butText_just, 0, "   Each computer player searches one move farther "
     "ahead than the previous one.  So \"" XIO_COMPCHAR " Simple\" only looks "
     "one move ahead, but \"" XIO_COMPCHAR " Master\" looks six moves ahead."},
  {butText_just, 0, "   At deeper searches, pruning is done.  I wanted "
     "to experiment with different pruning algorithms besides regular "
     "alpha-beta, but my results haven't been that great.  I prune off all "
     "sequences that last 3 or more moves with no threats or captures; and "
     "I never even consider moves that are more than 2 spaces from all other "
     "pieces on the board.  Lastly, if the computer is searching more than "
     "4 moves ahead, I do a 4 ply search then search deeper on the \"n\" most "
     "promising moves, where \"n\" is the overall search depth.  This last "
     "pruning algorithm is a lot like alpha-beta, but probably not as good."},
  {butText_just, 0, "   I plan on doing a major rewrite, replacing all "
     "this weird stuff with normal alpha-beta and installing a transposition "
     "table, then comparing the result against my current algorithms.  I "
     "suspect that this new version will be able to search deeper in less "
     "time, and thus play a better game."},
  {butText_just, 0, "   As an aside, originally I used a pattern matching "
     "algorithm instead of deep searches.  The pattern matcher trained "
     "itself using simulated annealing.  After letting the annealing "
     "algorithm run for a week or two, it played a surprisingly strong "
     "game for only one ply lookahead.  I tried training it to recognize "
     "patterns useful for a two ply search, but unfortunately the annealing "
     "algorithm was far too slow for this.  After giving up on tuning the "
     "algorithm enough for reasonable performance, I switched to the search "
     "that I use today."},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};
static xio_tb_t  computer_french[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "Par Bill Shubert"},
  {butText_center, 0, DATE_FRENCH},
  {butText_just, 0, ""},
  {butText_just, 1, "A Propos des Joueurs Gérés par l'Ordinateur"},
  {butText_just, 0, "   Les joueurs gérés par l'ordinateur utilisent une "
     "recherche d'arbre pour déterminer leurs coups. Porter une menace "
     "(telle que trois pierres alignées avec une ouverture de chaque côté) "
     "compte comme une bonne chose; les captures compte pour encore plus.  "
     "Ils analysent donc tous les coups possibles, en recherchant des "
     "séquences leur permettant de faire beaucoup plus de menaces et de "
     "captures que leurs adversaires."},
  {butText_just, 0, "   Chaque joueur géré par l'ordinateur cherche un coup "
     "en avance de plus que le précédent joueur. "
     "Donc \"" XIO_COMPCHAR " Simple\" "
     "regarde juste un coup en avance, mais \"" XIO_COMPCHAR " Maître\" "
     "regarde six coups en avance."},
  {butText_just, 0, "   Pour les recherches plus profondes, on a recours à "
     "un élagage.  Je voulais expérimenter différents algorithmes "
     "d'élagage en plus de l'alpha-beta habituel, mais mes résultats n'ont "
     "pas été si bons que ça. "
     "J'élague toutes les séquences de 3 coups ou plus sans menaces "
     "ni captures; et je ne prends jamais en considération les coups qui "
     "se trouvent à plus de 2 cases des autres jetons du plateau de jeu. "
     "Finalement, si l'ordinateur cherche plus de 4 coups en avance, je "
     "commence la recherche de 4 coups en avance  puis je cherche plus "
     "profondément dans "
     "les \"n\" plus prometteurs coups, où \"n\" est la profondeur de "
     "recherche globale. Ce dernier algorithme d'élagage ressemble "
     "beaucoup à de l'alpha-beta, mais n'est peut être pas aussi bon."},
  {butText_just, 0, "   Je projette de réaliser une grande réécriture, de "
     "remplacer toutes ces choses étranges par de l'alpha-beta normal et "
     "d'installer une table de transposition, puis de comparer le résultat "
     "avec l'algorithme que j'utilise pour le moment.  Je suppose que "
     "cette nouvelle version sera capable de chercher encore plus "
     "profondément et en moins de temps, et de jouer encore mieux."},
  {butText_just, 0, "   Un petit mot en en apparté: au départ "
     "j'utilisais un algorithme de "
     "comparaison de motif à la place de recherches en profondeur.  Le "
     "comparateur de motif s'entraînait tout seul en utilisant une "
     "heuristique. Après avoir laissé tourner l'heuristique pendant "
     "une semaine ou deux, le programme était étonnement fort pour "
     "seulement une recherche d'un coup "
     "d'avance. J'ai essayé de l'entraîner à reconnaître des motifs utiles "
     "pour une recherche de deux coups en avance, mais malheureusement "
     "l'heuristique était beaucoup trop lente. Après avoir abandonné la "
     "mise au point de l'algorithme, suffisamment pour obtenir des "
     "performances raisonnables, je suis passé à la recherche que j'utilise "
     "à présent."},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};

static xio_tb_t  author_eng[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "By Bill Shubert"},
  {butText_center, 0, DATE},
  {butText_just, 0, ""},
  {butText_just, 1, "About the author"},
  {butText_just, 0, "   Hello!  My name is Bill Shubert.  I've always loved "
     "playing pente, and computer algorithms to play human-oriented games "
     "fascinate me, so writing a computer program to play pente seemed "
     "like a natural thing to try.  Then I wanted to learn X, so I wrote "
     "the GUI to go with it.  It seems like I can always think of a way to "
     "improve this program so I'm never really done."},
  {butText_just, 0, "   Feel free to get in touch with me if you have any "
     "comments about this program!  As of the date at the top of this "
     "help page, you can contact me in these ways:"},
  {butText_left, 0, "   Email: wms@hevanet.com"},
  {butText_left, 0, "   WWW: http://www.hevanet.com/wms/"},
  {butText_left, 0, "   Work email: wms@ssd.intel.com"},
  {butText_left, 0, "   Phone: (503)223-2285"},
  {butText_just, 0, "Or send me a postcard at:"},
  {butText_left, 0, "   Bill Shubert"},
  {butText_left, 0, "   1975 NW Everett St. #301"},
  {butText_left, 0, "   Portland, OR 97209"},
  {butText_left, 0, "   USA"},
  {butText_just, 0, ""},
  {butText_just, 0, "   I'd like to thank Eric Dupas for translating all "
     "text into French."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Thanks for trying out my program.  I hope you like "
     "it!"},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};
static xio_tb_t  author_french[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "Par Bill Shubert"},
  {butText_center, 0, DATE_FRENCH},
  {butText_just, 0, ""},
  {butText_just, 1, "A Propos de l'Auteur"},
  {butText_just, 0, "   Bonjour!  Mon nom est Bill Shubert.  J'ai toujours "
     "aimé jouer à pente, et les algorithmes pour jouer "
     "à des jeux pour humains me fascinent, il m'a donc semblé naturel "
     "d'écrire un programme pour jouer à pente. "
     "Ensuite j'ai voulu apprendre X, donc j'ai écrit l'Interface Graphique "
     "Utilisateur qui fonctionne avec. J'ai toujours l'impression d'avoir "
     "une façon d'améliorer ce programme, je n'en ai donc jamais "
     "vraiment terminé."},
  {butText_just, 0, "   Vous pouvez me joindre si vous avez des commentaires "
     "concernant ce programme! A la date qui se trouve en haut de cette page "
     "d'aide, vous pouvez me contacter de plusieurs manières:"},
  {butText_left, 0, "   Email: wms@hevanet.com"},
  {butText_left, 0, "   WWW: http://www.hevanet.com/wms/"},
  {butText_left, 0, "   Email (travail): wms@ssd.intel.com"},
  {butText_left, 0, "   Téléphone: (503)223-2285"},
  {butText_just, 0, "Ou bien envoyez moi une carte postale à:"},
  {butText_left, 0, "   Bill Shubert"},
  {butText_left, 0, "   1975 NW Everett St. #301"},
  {butText_left, 0, "   Portland, OR 97209"},
  {butText_left, 0, "   USA"},
  {butText_just, 0, ""},
  {butText_just, 0, "   J'aimerais remercier Eric Dupas pour la traduction "
     "de tout le texte en Français."},
  {butText_just, 0, ""},
  {butText_just, 0, "   Merci d'essayer mon programme.  J'espère que "
     "vous l'appréciez!"},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};

static xio_tb_t  copying_eng[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "By Bill Shubert"},
  {butText_center, 0, DATE},
  {butText_just, 0, ""},
  {butText_just, 1, "Distribution"},
  {butText_just, 0, "   This program is distributed under the Gnu General "
     "Public License, version 2.  This should have been included with your "
     "copy of the source code is a file called \"COPYING\".  In any case, the "
     "license is also included below."},
  {butText_just, 0, "   Basically, there are three things that most of you "
     "will care about in this license.  One is that you can give away free "
     "copies to all your friends.  Another is that you have to be told where "
     "you can get the source code from.  You can get it from me; see the "
     "\"About the Author\" help page for how to get in touch.  The last is "
     "that there is no warranty, so if this software ruins your life don't "
     "come crying to me."},
  {butText_just, 0, "   If you want to do anything else with this software, "
     "you should read the license below for details."},
  {butText_just, 0, ""},
  {butText_center, 0, "GNU GENERAL PUBLIC LICENSE"},
  {butText_center, 0, "TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND "
     "MODIFICATION"},
  {butText_just, 0, ""},
  {butText_just, 0, "  0. This License applies to any program or other work "
     "which contains a notice placed by the copyright holder saying it may be "
     "distributed under the terms of this General Public License.  The "
     "\"Program\", below, refers to any such program or work, and a \"work "
     "based on the Program\" means either the Program or any derivative work "
     "under copyright law: that is to say, a work containing the Program or a "
     "portion of it, either verbatim or with modifications and/or translated "
     "into another language.  (Hereinafter, translation is included without "
     "limitation in the term \"modification\".)  Each licensee is addressed "
     "as \"you\"."},
  {butText_just, 0, ""},
  {butText_just, 0, "Activities other than copying, distribution and "
     "modification are not covered by this License; they are outside its "
     "scope.  The act of running the Program is not restricted, and the "
     "output from the Program is covered only if its contents constitute a "
     "work based on the Program (independent of having been made by running "
     "the Program). Whether that is true depends on what the Program does."},
  {butText_just, 0, ""},
  {butText_just, 0, "  1. You may copy and distribute verbatim copies of the "
     "Program's source code as you receive it, in any medium, provided that "
     "you conspicuously and appropriately publish on each copy an appropriate "
     "copyright notice and disclaimer of warranty; keep intact all the "
     "notices that refer to this License and to the absence of any warranty; "
     "and give any other recipients of the Program a copy of this License "
     "along with the Program."},
  {butText_just, 0, ""},
  {butText_just, 0, "You may charge a fee for the physical act of "
     "transferring a copy, and you may at your option offer warranty "
     "protection in exchange for a fee."},
  {butText_just, 0, ""},
  {butText_just, 0, "  2. You may modify your copy or copies of the Program "
     "or any portion of it, thus forming a work based on the Program, and "
     "copy and distribute such modifications or work under the terms of "
     "Section 1 above, provided that you also meet all of these "
     "conditions:"},
  {butText_just, 0, ""},
  {butText_just, 0, "    a) You must cause the modified files to carry "
     "prominent notices stating that you changed the files and the date of "
     "any change."},
  {butText_just, 0, ""},
  {butText_just, 0, "    b) You must cause any work that you distribute or "
     "publish, that in whole or in part contains or is derived from the "
     "Program or any part thereof, to be licensed as a whole at no charge to "
     "all third parties under the terms of this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "    c) If the modified program normally reads commands "
     "interactively when run, you must cause it, when started running for "
     "such interactive use in the most ordinary way, to print or display an "
     "announcement including an appropriate copyright notice and a notice "
     "that there is no warranty (or else, saying that you provide a warranty) "
     "and that users may redistribute the program under these conditions, and "
     "telling the user how to view a copy of this License.  (Exception: if "
     "the Program itself is interactive but does not normally print such an "
     "announcement, your work based on the Program is not required to print "
     "an announcement.)"},
  {butText_just, 0, ""},
  {butText_just, 0, "These requirements apply to the modified work as a "
     "whole.  If identifiable sections of that work are not derived from the "
     "Program, and can be reasonably considered independent and separate "
     "works in themselves, then this License, and its terms, do not apply to "
     "those sections when you distribute them as separate works.  But when "
     "you distribute the same sections as part of a whole which is a work "
     "based on the Program, the distribution of the whole must be on the "
     "terms of this License, whose permissions for other licensees extend to "
     "the entire whole, and thus to each and every part regardless of who "
     "wrote it."},
  {butText_just, 0, ""},
  {butText_just, 0, "Thus, it is not the intent of this section to claim "
     "rights or contest your rights to work written entirely by you; rather, "
     "the intent is to exercise the right to control the distribution of "
     "derivative or collective works based on the Program."},
  {butText_just, 0, ""},
  {butText_just, 0, "In addition, mere aggregation of another work not based "
     "on the Program with the Program (or with a work based on the Program) "
     "on a volume of a storage or distribution medium does not bring the "
     "other work under the scope of this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "  3. You may copy and distribute the Program (or a work "
     "based on it, under Section 2) in object code or executable form under "
     "the terms of Sections 1 and 2 above provided that you also do one of "
     "the following:"},
  {butText_just, 0, ""},
  {butText_just, 0, "    a) Accompany it with the complete corresponding "
     "machine-readable source code, which must be distributed under the terms "
     "of Sections 1 and 2 above on a medium customarily used for software "
     "interchange; or,"},
  {butText_just, 0, ""},
  {butText_just, 0, "    b) Accompany it with a written offer, valid for at "
     "least three years, to give any third party, for a charge no more than "
     "your cost of physically performing source distribution, a complete "
     "machine-readable copy of the corresponding source code, to be "
     "distributed under the terms of Sections 1 and 2 above on a medium "
     "customarily used for software interchange; or,"},
  {butText_just, 0, ""},
  {butText_just, 0, "    c) Accompany it with the information you received "
     "as to the offer to distribute corresponding source code.  (This "
     "alternative is allowed only for noncommercial distribution and only if "
     "you received the program in object code or executable form with such an "
     "offer, in accord with Subsection b above.)"},
  {butText_just, 0, ""},
  {butText_just, 0, "The source code for a work means the preferred form of "
     "the work for making modifications to it.  For an executable work, "
     "complete source code means all the source code for all modules it "
     "contains, plus any associated interface definition files, plus the "
     "scripts used to control compilation and installation of the executable. "
     " However, as a special exception, the source code distributed need not "
     "include anything that is normally distributed (in either source or "
     "binary form) with the major components (compiler, kernel, and so on) of "
     "the operating system on which the executable runs, unless that "
     "component itself accompanies the executable."},
  {butText_just, 0, ""},
  {butText_just, 0, "If distribution of executable or object code is made by "
     "offering access to copy from a designated place, then offering "
     "equivalent access to copy the source code from the same place counts as "
     "distribution of the source code, even though third parties are not "
     "compelled to copy the source along with the object code."},
  {butText_just, 0, ""},
  {butText_just, 0, "  4. You may not copy, modify, sublicense, or "
     "distribute the Program except as expressly provided under this License. "
     " Any attempt otherwise to copy, modify, sublicense or distribute the "
     "Program is void, and will automatically terminate your rights under "
     "this License.  However, parties who have received copies, or rights, "
     "from you under this License will not have their licenses terminated so "
     "long as such parties remain in full compliance."},
  {butText_just, 0, ""},
  {butText_just, 0, "  5. You are not required to accept this License, since "
     "you have not signed it.  However, nothing else grants you permission to "
     "modify or distribute the Program or its derivative works.  These "
     "actions are prohibited by law if you do not accept this License.  "
     "Therefore, by modifying or distributing the Program (or any work based "
     "on the Program), you indicate your acceptance of this License to do so, "
     "and all its terms and conditions for copying, distributing or modifying "
     "the Program or works based on it."},
  {butText_just, 0, ""},
  {butText_just, 0, "  6. Each time you redistribute the Program (or any "
     "work based on the Program), the recipient automatically receives a "
     "license from the original licensor to copy, distribute or modify the "
     "Program subject to these terms and conditions.  You may not impose any "
     "further restrictions on the recipients' exercise of the rights granted "
     "herein.  You are not responsible for enforcing compliance by third "
     "parties to this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "  7. If, as a consequence of a court judgment or "
     "allegation of patent infringement or for any other reason (not limited "
     "to patent issues), conditions are imposed on you (whether by court "
     "order, agreement or otherwise) that contradict the conditions of this "
     "License, they do not excuse you from the conditions of this License.  "
     "If you cannot distribute so as to satisfy simultaneously your "
     "obligations under this License and any other pertinent obligations, "
     "then as a consequence you may not distribute the Program at all.  For "
     "example, if a patent license would not permit royalty-free "
     "redistribution of the Program by all those who receive copies directly "
     "or indirectly through you, then the only way you could satisfy both it "
     "and this License would be to refrain entirely from distribution of the "
     "Program."},
  {butText_just, 0, ""},
  {butText_just, 0, "If any portion of this section is held invalid or "
     "unenforceable under any particular circumstance, the balance of the "
     "section is intended to apply and the section as a whole is intended to "
     "apply in other circumstances."},
  {butText_just, 0, ""},
  {butText_just, 0, "It is not the purpose of this section to induce you to "
     "infringe any patents or other property right claims or to contest "
     "validity of any such claims; this section has the sole purpose of "
     "protecting the integrity of the free software distribution system, "
     "which is implemented by public license practices.  Many people have "
     "made generous contributions to the wide range of software distributed "
     "through that system in reliance on consistent application of that "
     "system; it is up to the author/donor to decide if he or she is willing "
     "to distribute software through any other system and a licensee cannot "
     "impose that choice."},
  {butText_just, 0, ""},
  {butText_just, 0, "This section is intended to make thoroughly clear what "
     "is believed to be a consequence of the rest of this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "  8. If the distribution and/or use of the Program is "
     "restricted in certain countries either by patents or by copyrighted "
     "interfaces, the original copyright holder who places the Program under "
     "this License may add an explicit geographical distribution limitation "
     "excluding those countries, so that distribution is permitted only in or "
     "among countries not thus excluded.  In such case, this License "
     "incorporates the limitation as if written in the body of this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "  9. The Free Software Foundation may publish revised "
     "and/or new versions of the General Public License from time to time.  "
     "Such new versions will be similar in spirit to the present version, but "
     "may differ in detail to address new problems or concerns."},
  {butText_just, 0, ""},
  {butText_just, 0, "Each version is given a distinguishing version number.  "
     "If the Program specifies a version number of this License which applies "
     "to it and \"any later version\", you have the option of following the "
     "terms and conditions either of that version or of any later version "
     "published by the Free Software Foundation.  If the Program does not "
     "specify a version number of this License, you may choose any version "
     "ever published by the Free Software Foundation."},
  {butText_just, 0, ""},
  {butText_just, 0, "  10. If you wish to incorporate parts of the Program "
     "into other free programs whose distribution conditions are different, "
     "write to the author to ask for permission.  For software which is "
     "copyrighted by the Free Software Foundation, write to the Free Software "
     "Foundation; we sometimes make exceptions for this.  Our decision will "
     "be guided by the two goals of preserving the free status of all "
     "derivatives of our free software and of promoting the sharing and reuse "
     "of software generally."},
  {butText_just, 0, ""},
  {butText_center, 0, "NO WARRANTY"},
  {butText_just, 0, ""},
  {butText_just, 0, "  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, "
     "THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY "
     "APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT "
     "HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT "
     "WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT "
     "LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A "
     "PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE "
     "OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU "
     "ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION."},
  {butText_just, 0, ""},
  {butText_just, 0, "  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR "
     "AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO "
     "MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE "
     "LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL "
     "OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE "
     "PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING "
     "RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A "
     "FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF "
     "SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH "
     "DAMAGES."},
  {butText_just, 0, ""},
  {butText_center, 0, "END OF TERMS AND CONDITIONS"},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};

static xio_tb_t  copying_french[] = {
  {butText_center, 1, "Pente " VERSION},
  {butText_center, 0, "Par Bill Shubert"},
  {butText_center, 0, DATE_FRENCH},
  {butText_just, 0, ""},
  {butText_just, 1, "Distribution"},
  {butText_just, 0, "   This program is distributed under the Gnu General "
     "Public License, version 2.  This should have been included with your "
     "copy of the source code is a file called \"COPYING\".  In any case, the "
     "license is also included below."},
  {butText_just, 0, "   Basically, there are three things that most of you "
     "will care about in this license.  One is that you can give away free "
     "copies to all your friends.  Another is that you have to be told where "
     "you can get the source code from.  You can get it from me; see the "
     "\"About the Author\" help page for how to get in touch.  The last is "
     "that there is no warranty, so if this software ruins your life don't "
     "come crying to me."},
  {butText_just, 0, "   If you want to do anything else with this software, "
     "you should read the license below for details."},
  {butText_just, 0, ""},
  {butText_center, 0, "GNU GENERAL PUBLIC LICENSE"},
  {butText_center, 0, "TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND "
     "MODIFICATION"},
  {butText_just, 0, ""},
  {butText_just, 0, "  0. This License applies to any program or other work "
     "which contains a notice placed by the copyright holder saying it may be "
     "distributed under the terms of this General Public License.  The "
     "\"Program\", below, refers to any such program or work, and a \"work "
     "based on the Program\" means either the Program or any derivative work "
     "under copyright law: that is to say, a work containing the Program or a "
     "portion of it, either verbatim or with modifications and/or translated "
     "into another language.  (Hereinafter, translation is included without "
     "limitation in the term \"modification\".)  Each licensee is addressed "
     "as \"you\"."},
  {butText_just, 0, ""},
  {butText_just, 0, "Activities other than copying, distribution and "
     "modification are not covered by this License; they are outside its "
     "scope.  The act of running the Program is not restricted, and the "
     "output from the Program is covered only if its contents constitute a "
     "work based on the Program (independent of having been made by running "
     "the Program). Whether that is true depends on what the Program does."},
  {butText_just, 0, ""},
  {butText_just, 0, "  1. You may copy and distribute verbatim copies of the "
     "Program's source code as you receive it, in any medium, provided that "
     "you conspicuously and appropriately publish on each copy an appropriate "
     "copyright notice and disclaimer of warranty; keep intact all the "
     "notices that refer to this License and to the absence of any warranty; "
     "and give any other recipients of the Program a copy of this License "
     "along with the Program."},
  {butText_just, 0, ""},
  {butText_just, 0, "You may charge a fee for the physical act of "
     "transferring a copy, and you may at your option offer warranty "
     "protection in exchange for a fee."},
  {butText_just, 0, ""},
  {butText_just, 0, "  2. You may modify your copy or copies of the Program "
     "or any portion of it, thus forming a work based on the Program, and "
     "copy and distribute such modifications or work under the terms of "
     "Section 1 above, provided that you also meet all of these "
     "conditions:"},
  {butText_just, 0, ""},
  {butText_just, 0, "    a) You must cause the modified files to carry "
     "prominent notices stating that you changed the files and the date of "
     "any change."},
  {butText_just, 0, ""},
  {butText_just, 0, "    b) You must cause any work that you distribute or "
     "publish, that in whole or in part contains or is derived from the "
     "Program or any part thereof, to be licensed as a whole at no charge to "
     "all third parties under the terms of this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "    c) If the modified program normally reads commands "
     "interactively when run, you must cause it, when started running for "
     "such interactive use in the most ordinary way, to print or display an "
     "announcement including an appropriate copyright notice and a notice "
     "that there is no warranty (or else, saying that you provide a warranty) "
     "and that users may redistribute the program under these conditions, and "
     "telling the user how to view a copy of this License.  (Exception: if "
     "the Program itself is interactive but does not normally print such an "
     "announcement, your work based on the Program is not required to print "
     "an announcement.)"},
  {butText_just, 0, ""},
  {butText_just, 0, "These requirements apply to the modified work as a "
     "whole.  If identifiable sections of that work are not derived from the "
     "Program, and can be reasonably considered independent and separate "
     "works in themselves, then this License, and its terms, do not apply to "
     "those sections when you distribute them as separate works.  But when "
     "you distribute the same sections as part of a whole which is a work "
     "based on the Program, the distribution of the whole must be on the "
     "terms of this License, whose permissions for other licensees extend to "
     "the entire whole, and thus to each and every part regardless of who "
     "wrote it."},
  {butText_just, 0, ""},
  {butText_just, 0, "Thus, it is not the intent of this section to claim "
     "rights or contest your rights to work written entirely by you; rather, "
     "the intent is to exercise the right to control the distribution of "
     "derivative or collective works based on the Program."},
  {butText_just, 0, ""},
  {butText_just, 0, "In addition, mere aggregation of another work not based "
     "on the Program with the Program (or with a work based on the Program) "
     "on a volume of a storage or distribution medium does not bring the "
     "other work under the scope of this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "  3. You may copy and distribute the Program (or a work "
     "based on it, under Section 2) in object code or executable form under "
     "the terms of Sections 1 and 2 above provided that you also do one of "
     "the following:"},
  {butText_just, 0, ""},
  {butText_just, 0, "    a) Accompany it with the complete corresponding "
     "machine-readable source code, which must be distributed under the terms "
     "of Sections 1 and 2 above on a medium customarily used for software "
     "interchange; or,"},
  {butText_just, 0, ""},
  {butText_just, 0, "    b) Accompany it with a written offer, valid for at "
     "least three years, to give any third party, for a charge no more than "
     "your cost of physically performing source distribution, a complete "
     "machine-readable copy of the corresponding source code, to be "
     "distributed under the terms of Sections 1 and 2 above on a medium "
     "customarily used for software interchange; or,"},
  {butText_just, 0, ""},
  {butText_just, 0, "    c) Accompany it with the information you received "
     "as to the offer to distribute corresponding source code.  (This "
     "alternative is allowed only for noncommercial distribution and only if "
     "you received the program in object code or executable form with such an "
     "offer, in accord with Subsection b above.)"},
  {butText_just, 0, ""},
  {butText_just, 0, "The source code for a work means the preferred form of "
     "the work for making modifications to it.  For an executable work, "
     "complete source code means all the source code for all modules it "
     "contains, plus any associated interface definition files, plus the "
     "scripts used to control compilation and installation of the executable. "
     " However, as a special exception, the source code distributed need not "
     "include anything that is normally distributed (in either source or "
     "binary form) with the major components (compiler, kernel, and so on) of "
     "the operating system on which the executable runs, unless that "
     "component itself accompanies the executable."},
  {butText_just, 0, ""},
  {butText_just, 0, "If distribution of executable or object code is made by "
     "offering access to copy from a designated place, then offering "
     "equivalent access to copy the source code from the same place counts as "
     "distribution of the source code, even though third parties are not "
     "compelled to copy the source along with the object code."},
  {butText_just, 0, ""},
  {butText_just, 0, "  4. You may not copy, modify, sublicense, or "
     "distribute the Program except as expressly provided under this License. "
     " Any attempt otherwise to copy, modify, sublicense or distribute the "
     "Program is void, and will automatically terminate your rights under "
     "this License.  However, parties who have received copies, or rights, "
     "from you under this License will not have their licenses terminated so "
     "long as such parties remain in full compliance."},
  {butText_just, 0, ""},
  {butText_just, 0, "  5. You are not required to accept this License, since "
     "you have not signed it.  However, nothing else grants you permission to "
     "modify or distribute the Program or its derivative works.  These "
     "actions are prohibited by law if you do not accept this License.  "
     "Therefore, by modifying or distributing the Program (or any work based "
     "on the Program), you indicate your acceptance of this License to do so, "
     "and all its terms and conditions for copying, distributing or modifying "
     "the Program or works based on it."},
  {butText_just, 0, ""},
  {butText_just, 0, "  6. Each time you redistribute the Program (or any "
     "work based on the Program), the recipient automatically receives a "
     "license from the original licensor to copy, distribute or modify the "
     "Program subject to these terms and conditions.  You may not impose any "
     "further restrictions on the recipients' exercise of the rights granted "
     "herein.  You are not responsible for enforcing compliance by third "
     "parties to this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "  7. If, as a consequence of a court judgment or "
     "allegation of patent infringement or for any other reason (not limited "
     "to patent issues), conditions are imposed on you (whether by court "
     "order, agreement or otherwise) that contradict the conditions of this "
     "License, they do not excuse you from the conditions of this License.  "
     "If you cannot distribute so as to satisfy simultaneously your "
     "obligations under this License and any other pertinent obligations, "
     "then as a consequence you may not distribute the Program at all.  For "
     "example, if a patent license would not permit royalty-free "
     "redistribution of the Program by all those who receive copies directly "
     "or indirectly through you, then the only way you could satisfy both it "
     "and this License would be to refrain entirely from distribution of the "
     "Program."},
  {butText_just, 0, ""},
  {butText_just, 0, "If any portion of this section is held invalid or "
     "unenforceable under any particular circumstance, the balance of the "
     "section is intended to apply and the section as a whole is intended to "
     "apply in other circumstances."},
  {butText_just, 0, ""},
  {butText_just, 0, "It is not the purpose of this section to induce you to "
     "infringe any patents or other property right claims or to contest "
     "validity of any such claims; this section has the sole purpose of "
     "protecting the integrity of the free software distribution system, "
     "which is implemented by public license practices.  Many people have "
     "made generous contributions to the wide range of software distributed "
     "through that system in reliance on consistent application of that "
     "system; it is up to the author/donor to decide if he or she is willing "
     "to distribute software through any other system and a licensee cannot "
     "impose that choice."},
  {butText_just, 0, ""},
  {butText_just, 0, "This section is intended to make thoroughly clear what "
     "is believed to be a consequence of the rest of this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "  8. If the distribution and/or use of the Program is "
     "restricted in certain countries either by patents or by copyrighted "
     "interfaces, the original copyright holder who places the Program under "
     "this License may add an explicit geographical distribution limitation "
     "excluding those countries, so that distribution is permitted only in or "
     "among countries not thus excluded.  In such case, this License "
     "incorporates the limitation as if written in the body of this License."},
  {butText_just, 0, ""},
  {butText_just, 0, "  9. The Free Software Foundation may publish revised "
     "and/or new versions of the General Public License from time to time.  "
     "Such new versions will be similar in spirit to the present version, but "
     "may differ in detail to address new problems or concerns."},
  {butText_just, 0, ""},
  {butText_just, 0, "Each version is given a distinguishing version number.  "
     "If the Program specifies a version number of this License which applies "
     "to it and \"any later version\", you have the option of following the "
     "terms and conditions either of that version or of any later version "
     "published by the Free Software Foundation.  If the Program does not "
     "specify a version number of this License, you may choose any version "
     "ever published by the Free Software Foundation."},
  {butText_just, 0, ""},
  {butText_just, 0, "  10. If you wish to incorporate parts of the Program "
     "into other free programs whose distribution conditions are different, "
     "write to the author to ask for permission.  For software which is "
     "copyrighted by the Free Software Foundation, write to the Free Software "
     "Foundation; we sometimes make exceptions for this.  Our decision will "
     "be guided by the two goals of preserving the free status of all "
     "derivatives of our free software and of promoting the sharing and reuse "
     "of software generally."},
  {butText_just, 0, ""},
  {butText_center, 0, "NO WARRANTY"},
  {butText_just, 0, ""},
  {butText_just, 0, "  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, "
     "THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY "
     "APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT "
     "HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT "
     "WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT "
     "LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A "
     "PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE "
     "OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU "
     "ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION."},
  {butText_just, 0, ""},
  {butText_just, 0, "  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR "
     "AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO "
     "MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE "
     "LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL "
     "OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE "
     "PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING "
     "RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A "
     "FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF "
     "SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH "
     "DAMAGES."},
  {butText_just, 0, ""},
  {butText_center, 0, "END OF TERMS AND CONDITIONS"},
  {butText_just, 0, ""},
  {butText_center, 0,
     XIO_PL1CHAR XIO_PL2CHAR XIO_PL1MARKCHAR XIO_PL2CHAR XIO_PL1CHAR},
  {butText_just, 0, ""},
  {0,0,NULL}};

xio_tb_t  *xioStr_proghelp[] = {proghelp_eng, proghelp_french};
xio_tb_t  *xioStr_gamehelp[] = {gamehelp_eng, gamehelp_french};
xio_tb_t  *xioStr_setuphelp[] = {setuphelp_eng, setuphelp_french};
xio_tb_t  *xioStr_networkhelp[] = {network_eng, network_french};
xio_tb_t  *xioStr_comphelp[] = {computer_eng, computer_french};
xio_tb_t  *xioStr_abouthelp[] = {author_eng, author_french};
xio_tb_t  *xioStr_copyinghelp[] = {copying_eng, copying_french};

#endif  /* X11_DISP */
