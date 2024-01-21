/* Fichier .h contenant toutes les types des variables globales
qui sont utilisees par
		- l'analyseur lexical
		- l'analyseur syntaxique
*/

/* Structure qui regroupe commande et boucle conditionnelle */
typedef struct
{
 int Type;		/* Type de la commande */
 long *TabArg;		/* Tableau d'argument: id var,fnc,const,adr bloc... */
 int NbArg;
} Instr;


/* Structure bloc d'instruction */
typedef struct
{
 Instr *TabInstr;		/* Tableau d'instruction */
 int NbInstr;			/* Nombre d'instruction */
} Bloc;


/* Structure pour enregistrer l'entete du script: valeur par defaut */
typedef struct			/* Type pour la gestion X */
{
  int x,y ;			/* Origine de la fenetre */
  int height,width;		/* Hauteur et largeur */
  char *titlewin;		/* Titre */
  char *forecolor;		/* Couleur des lignes */
  char *backcolor;		/* Couleur de fond */
  char *shadcolor;		/* Couleur des lignes */
  char *licolor;		/* Couleur des lignes */
  char *font;			/* Police utilisee */
  char *icon;			/* Icone pour l'application iconisee */
  Bloc *initbloc;		/* Bloc d'iniitalisation */
} ScriptProp;

/* Les differents etats d'un objet graphique */
typedef enum { Hiden, Inactif, Actif } State;


typedef struct
{
 int NbCase;
 int *LstCase;		/* Tableau des valeurs case a verifier */
} CaseObj;

/* Structure pour enregistrer les caracteristiques des objets graphiques */
typedef struct			/* Type pour les boutons */
{
  int id;
  char *type;
  int x,y ;			/* Origine du bouton */
  int height,width;		/* Hauteur et largeur */
  char *title;			/* Titre */
  char *swallow;		/* swallowexec */
  char *icon;			/* Icon */
  char *forecolor;		/* Couleur des lignes */
  char *backcolor;		/* Couleur de fond */
  char *shadcolor;
  char *licolor;
  char *font;			/* Police utilisé */
  State state;			/* Etat du bouton:invisible, inactif et actif */
  int value;
  int value2;
  int value3;
} MyObject;

typedef MyObject TabObj[30];





