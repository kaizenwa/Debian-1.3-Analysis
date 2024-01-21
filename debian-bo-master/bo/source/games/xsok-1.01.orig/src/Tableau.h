#ifndef _Tableau_h
#define _Tableau_h

/* define any special resource names here that are not in <X11/StringDefs.h> */
#define XtNruleset	"rules"
#define XtCRuleset	"Rules"
#define XtNlevel	"level"
#define XtCLevel	"Level"
#define XtNusername	"username"
#define XtCUsername	"Username"
#define XtNxsokdir	"xsokdir"
#define XtCXsokdir	"Xsokdir"
#define XtNxpmdir	"xpmdir"
#define XtCXpmdir	"Xpmdir"
#define XtNsavedir	"xsokdir"
#define XtCSavedir	"Xsokdir"
#define XtNmessageFile	"messageFile"
#define XtCMessageFile	"MessageFile"
#define XtNkeyboardFile	"keyboardFile"
#define XtCKeyboardFile	"KeyboardFile"


/* declare specific TableauWidget class and instance datatypes */

typedef struct _TableauClassRec*	TableauWidgetClass;
typedef struct _TableauRec*		TableauWidget;

/* declare the class constant */

extern WidgetClass tableauWidgetClass;

#endif /* _Tableau_h */
