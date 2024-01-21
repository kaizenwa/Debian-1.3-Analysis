#ifndef _TableauP_h
#define _TableauP_h

#include "Tableau.h"
/* include superclass private header file */
#include <X11/CoreP.h>

/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRTableauResource "TableauResource"

typedef struct {
    int empty;
} TableauClassPart;

typedef struct _TableauClassRec {
    CoreClassPart	core_class;
    TableauClassPart	tableau_class;
} TableauClassRec;

extern TableauClassRec tableauClassRec;

typedef struct {
    /* resources */
    String rules;
    int level;
    String username;
    String xsokdir;
    String xpmdir;
    String savedir;
    String messageFile;
    String keyboardFile;
} TableauPart;

typedef struct _TableauRec {
    CorePart core;
    TableauPart tableau;
} TableauRec;

#endif /* _TableauP_h */
