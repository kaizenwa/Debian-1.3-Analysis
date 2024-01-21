#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <stdlib.h>
#include <malloc.h>

#define XK_MISCELLANY
#include <sys/types.h>
#include <sys/time.h>
#ifdef ISC
#include <sys/bsdtypes.h> /* Saul */
#endif
#if defined ___AIX || defined _AIX || defined __QNX__ || defined ___AIXV3 || defined AIXV3 || defined _SEQUENT_
#include <sys/select.h>
#endif

#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xproto.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/xpm.h>
#include <X11/X.h>

#include "../../../configure.h"
#include "../../../fvwm/module.h"
#include "../../../libs/fvwmlib.h"  
#include "../../../version.h"
#include "../global.h"

/* Constante de couleurs utilise dans le tableau TabColor */
#define black 0
#define white 1
#define back 2
#define fore 3
#define shad 4
#define li 5
#define True 1
#define False 0

extern char *Scrapt;
extern Atom propriete;
extern Atom type;
extern char *pixmapPath;

/* Constante pour les type de message envoie entre objets */
/* <0 valeur reserve pour les messages internes */
/* =0 message changement de valeur */
/* >0 message envoie par l'utilisateur */
#define ModifValue 0


struct XObj 
{
  Window ObjWin;		/* Fenetre contenant l'objet */
  Window *ParentWin;		/* Fenetre parent */
  Display *display;
  int Screen;
  Colormap *colormap;
  XColor TabColor[6];
  GC gc;			/* gc utilise pour les requetes: 4 octets */
  int id;			/* Numero d'id */
  int x,y;			/* Origine du bouton */
  int height,width;		/* Hauteur et largeur */
  char *title;			/* Titre */
  char *swallow;		/* SwallowExec */
  char *icon;			/* Icon */
  char *forecolor;		/* Couleur des lignes */
  char *backcolor;		/* Couleur de fond */
  char *shadcolor;		/* Couleur des lignes */
  char *licolor;		/* Couleur de fond */
  char *font;			/* Police utilisé */
  Pixmap iconPixmap;		/* Icone charge */
  Pixmap icon_maskPixmap;	/* Icone masque */
  int icon_w,icon_h;		/* Largeur et hauteur de l'icone */
  XFontStruct *xfont;
  int value;			/* Valeur courante */
  int value2;			/* Valeur minimale */
  int value3;			/* Valeur maximale */
  State state;
  void (*InitObj) (struct XObj *xobj);	/* Initialisation de l'objet */
  void (*DestroyObj) (struct XObj *xobj);	/* Destruction objet */
  void (*DrawObj) (struct XObj *xobj);	/* Dessin de l'objet */
  void (*EvtMouse) (struct XObj *xobj,XButtonEvent *EvtButton);
  void (*EvtKey) (struct XObj *xobj,XKeyEvent *EvtKey);
  void (*ProcessMsg) (struct XObj *xobj,unsigned long type,unsigned long *body);
  void *UserPtr;
};

void ChooseFunction(struct XObj *xobj,char *type);

















