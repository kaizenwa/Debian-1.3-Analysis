/*
 * gdmoTcl.c
 * (gdmotcl bedeutet nicht: Geh Du Mal Oefter Tee oder Cola Loeffeln ;-)
 *
 * Implementation of the "gdmo" command, with options like:
 * load		to parse (load) a GDMO file
 * class, package, parameter, namebinding, attribute, group, behaviour,
 * action, notification		used alone for a list of the labels
 *		used with a label for the template body
 *
 * that implements a parser and browser for GDMO files
 * (GDMO = Guidelines for the Definition of Managed Objects)
 * CCITT Rec. X.722 respectively ISO/IEC 10165-4
 *
 * Copyright (c) 1994, 1995
 *
 * M. Kernchen (email: M.Kernchen@tu-bs.de)
 * TU Braunschweig, Germany
 * Institute for Operating Systems and Computer Networks
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  The University of Braunschweig
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include "gdmo.h"

#define GDMODEBUG 1

/* interfacing with flex and bison */
extern int	reentered;
extern long	gdmo_file_pos;
extern int	lineno;
extern FILE	*yyin;		/* filedescriptor of the parsed file */

#ifdef YYDEBUG
extern int	yydebug;
#endif

char		*gdmo_file;	/* path name for the parsed file */

/*
 * Some arrays to map readable names to internal codes.
 */

static char *context_string[] = {
    "invalid", "ACTION-INFO", "ACTION-REPLY",
    "EVENT-INFO", "EVENT-REPLY", "SPECIFIC-ERROR"
};

static char *delete_string[] = {
    "invalid", "ONLY-IF-NO-CONTAINED-OBJECTS",
    "DELETES-CONTAINED-OBJECTS"
};

static char *create_string[] = {
    "invalid", "WITH-REFERENCE-OBJECT", "WITH-AUTOMATIC-INSTANCE-NAMING",  
    "WITH-REFERENCE-OBJECT WITH-AUTOMATIC-INSTANCE-NAMING"
};

static char *matches_string[] = {
    "EQUALITY", "ORDERING", "SUBSTRINGS",
    "SET-COMPARISON", "SET-INTERSECTION"
};


/*
 * Forward declarations for procedures defined later in this file:
 */

static void
Warning			_ANSI_ARGS_((char *message));

static void
ReadTextFromFile	_ANSI_ARGS_((Tcl_Interp*, text_in_file*,
				     Tcl_DString*));
static void
behav_list_result	_ANSI_ARGS_((Tcl_Interp*, behav_ref*));

static void
param_list_result	_ANSI_ARGS_((Tcl_Interp*, param_ref*));

static void
param_list_dstring	_ANSI_ARGS_((Tcl_DString*, param_ref*));

static void
attr_list_result	_ANSI_ARGS_((Tcl_Interp*, attr_ref*));

static void
attr_list_dstring	_ANSI_ARGS_((Tcl_DString*, attr_ref*));

static void
class_list_result	_ANSI_ARGS_((Tcl_Interp*, class_ref*));

static void
oid_result		_ANSI_ARGS_((Tcl_Interp*, gdmo_oid*));

static int
GdmoNotification	_ANSI_ARGS_((Tcl_Interp *interp,
				     int argc, char **argv));
static int
GdmoBehaviour		_ANSI_ARGS_((Tcl_Interp *interp,
				     int argc, char **argv));
static int
GdmoLoad		_ANSI_ARGS_((Tcl_Interp *interp,
				     int argc, char **argv));
static int
GdmoInfo		_ANSI_ARGS_((Tcl_Interp *interp,
				     int argc, char **argv));
static int 
GdmoCmd			_ANSI_ARGS_((ClientData clientData,
				     Tcl_Interp *interp,
				     int argc, char **argv));


/*===========================================================================*
 *
 * Warning() writes a warning message to stderr. This is only enabled in
 * debugging mode and used to signal missing references, which may be
 * parser bugs.
 */

static void 
Warning(message)
     char *message;
{
#ifdef GDMODEBUG
    fprintf(stderr, "gdmotcl.c: %s\n", message);
#endif
}


/*===========================================================================*
 *
 * ReadTextFromFile() as the name allready implies, reads a portion
 * from the file "path_and_pos->path" from position "path_and_pos->pos"
 * through length "path_and_pos->len". The read text will be set as the
 * result string for interp, if "dstring" is NULL, otherwise it will
 * be appended to "dstring".
 */

static void
ReadTextFromFile(interp, path_and_pos, dstring)
     Tcl_Interp	 *interp;
     text_in_file *path_and_pos;
     Tcl_DString *dstring;
{
    char	*text;		/* text, to be read from file */
    FILE	*fin = NULL;	/* filedescriptor of the actual opened file */

    text = ckalloc (path_and_pos->len + 1);
    text[0] = text[path_and_pos->len] = '\0';

    fin = fopen (path_and_pos->path, "r");
    if (!fin) {
#ifdef GDMODEBUG
	fprintf (stderr, "Warning: couldn't open \"%s\": %s\n",
		 path_and_pos->path, Tcl_PosixError (interp));
#endif
    } else {

	int error = fseek(fin, path_and_pos->pos, 0);
	if (error < 0) {		/* error */
#ifdef GDMODEBUG
	    fprintf (stderr, 
		     "Warning: failed to seek to position %ld in \"%s\": %s\n",
		     path_and_pos->pos, path_and_pos->path, 
		     Tcl_PosixError (interp));
#endif
	} else {

	    error = fread (text, path_and_pos->len, 1, fin);
	    if (error == 0) {
#ifdef GDMODEBUG
		fprintf (stderr, "couldn't read from \"%s\": %s\n",
			 path_and_pos->path, Tcl_PosixError(interp));
#endif
	    }
	}
	fclose (fin);
    }
    
    if (dstring) {
	Tcl_DStringAppendElement(dstring, text);
	ckfree(text);
    } else {
	Tcl_SetResult(interp, text, TCL_DYNAMIC);
    }
}

/*===========================================================================* 
 *
 * Append a list of behaviour-labels to the result string
 */

static void behav_list_result(interp, behav_list)
Tcl_Interp	*interp;
behav_ref	*behav_list;
{
    while (behav_list) {
	if (!behav_list->def) {
	    /* XXX probably error in bison */
	    Warning("missing pointer to definition in behaviour list"); 
	    /* Tcl_AppendElement(interp, ""); */
	} else {
	    Tcl_AppendElement(interp, behav_list->def->label->string);
	}
	behav_list = behav_list->next;
    }
}

/*
 * Append a list of parameter-labels to the result string
 */

static void param_list_result(interp, param_list)
Tcl_Interp	*interp;
param_ref	*param_list;
{
    while (param_list) {
	if (!param_list->def) {
	    /* XXX probably error in bison */
	    Warning("missing pointer to definition in parameter list"); 
	    /* Tcl_AppendElement(interp, ""); */
	} else {
	    Tcl_AppendElement(interp, param_list->def->label->string);
	}
	param_list = param_list->next;
    }
}

/*
 * Append a sublist of parameter-labels to the dynamic string "dstring"
 */

static void param_list_dstring(dstring, param_list)
Tcl_DString	*dstring;
param_ref	*param_list;
{
    Tcl_DStringStartSublist(dstring);
    while (param_list) {
	if (!param_list->def) {
	    /* XXX probably error in bison */
	    Warning("missing pointer to definition in parameter list"); 
	    /* Tcl_DStringAppendElement(dstring, ""); */
	} else {
	    Tcl_DStringAppendElement(dstring, param_list->def->label->string);
	}
	param_list = param_list->next;
    }
    Tcl_DStringEndSublist(dstring);
}

/*
 * Append a list of attribute-labels to the result string
 */

static void attr_list_result(interp, attr_list)
Tcl_Interp	*interp;
attr_ref	*attr_list;
{
    while (attr_list) {
	if (!attr_list->def) {
	    /* XXX probably error in bison */
	    Warning("missing pointer to definition in attribute list"); 
	    /* Tcl_AppendElement(interp, ""); */
	} else {
	    Tcl_AppendElement(interp, attr_list->def->label->string);
	}
	attr_list = attr_list->next;
    }
}

/*
 * Append a sublist of attribute-labels to the dynamic string "dstring"
 */

static void attr_list_dstring(dstring, attr_list)
Tcl_DString	*dstring;
attr_ref	*attr_list;
{
    Tcl_DStringStartSublist(dstring);
    while (attr_list) {
	if (!attr_list->def) {
	    /* XXX probably error in bison */
	    Warning("missing pointer to definition in attribute list"); 
	    /* Tcl_DStringAppendElement(dstring, ""); */
	} else {
	    Tcl_DStringAppendElement(dstring, attr_list->def->label->string);
	}
	attr_list = attr_list->next;
    }
    Tcl_DStringEndSublist(dstring);
}

/*
 * Append an object-identifier (a list of object-identifier-parts)
 * to the result string
 */

static void oid_result(interp, obj_id)
Tcl_Interp	*interp;
gdmo_oid	*obj_id;
{
    while (obj_id) {
	if (!obj_id->part) {
	    /* XXX probably error in bison */
	    Warning("missing pointer to object identifier part"); 
	    /* Tcl_AppendElement(interp, ""); */
	} else {
	    Tcl_AppendElement(interp, obj_id->part);
	}
	obj_id = obj_id->next;
    }
} /* oid_result() */

/*===========================================================================*
 *
 * Append a list of class-labels to the result string
 */

static void class_list_result(interp, class_list)
Tcl_Interp	*interp;
class_ref	*class_list;
{
    while (class_list) {
	if (!class_list->def) {
	    /* XXX probably error in bison */
	    Warning("missing pointer to definition in class list"); 
	    /* Tcl_AppendElement(interp, ""); */
	} else {
	    Tcl_AppendElement(interp, class_list->def->label->string);
	}
	class_list = class_list->next;
    }
} /* class_list_result() */

/*
 * Append a list of package-labels to the result string
 */

static void mand_pckg_result(interp, pckg_list)
Tcl_Interp	*interp;
class_pckg	*pckg_list;
{
    while (pckg_list) {
	if (!pckg_list->def) {
	    /* XXX fehler beim aufbau */
	    Warning("missing pointer to definition for mandatory package");
	    Tcl_AppendElement(interp, "");
	} else {
	    Tcl_AppendElement(interp, pckg_list->def->label->string);
	}
	if (pckg_list->cond) {
	    /* XXX fehler beim aufbau */
	    Warning("mandatory package with condition");
	}
	pckg_list = pckg_list->next;
    }
} /* mand_pckg_result() */

/*
 * Append a list of package-label and condition-definition pairs
 * to the result string
 */

static void cond_pckg_result(interp, pckg_list)
Tcl_Interp	*interp;
class_pckg	*pckg_list;
{
    Tcl_DString	dstring;

    Tcl_DStringInit(&dstring);
    while (pckg_list) {
	Tcl_DStringStartSublist(&dstring);
	if (!pckg_list->def) {
	    /* XXX fehler beim aufbau */
	    Warning("missing pointer to definition for conditional package");
	    Tcl_DStringAppendElement(&dstring, "");
	} else {
	    Tcl_DStringAppendElement(&dstring,
				     pckg_list->def->label->string);
	}
	if (!pckg_list->cond) {
	    /* XXX fehler beim aufbau */
	    Warning("missing condition for conditional package");
	    Tcl_DStringAppendElement(&dstring, "");
	} else {

	    /* get the condition text from file */

	    ReadTextFromFile(interp, pckg_list->cond, &dstring);
	}
	Tcl_DStringEndSublist(&dstring);
	pckg_list = pckg_list->next;
    }
    Tcl_DStringResult(interp, &dstring);
} /* cond_pckg_result() */

/*
 * option_class() 
 */

static int option_class(interp, argc, argv)
Tcl_Interp	*interp;
int		argc;
char		**argv;
{
/* XXX dar"uber ob die naechste zeile sinnvoll ist, noch gedanken machen */
    static gdmo_class	*tmp_ptr = NULL;

    if (argc == 2) {
	if (!strcmp(argv[1], "info")) {
	    Tcl_SetResult(interp,
			  "exist superior mandatory conditional oid",
			  TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[1], "class")) {
	    /* only the class option, return a list of the class_labels */
	    tmp_ptr = class_def_list;
	    while (tmp_ptr) {
		Tcl_AppendElement(interp, tmp_ptr->label->string);
		tmp_ptr = tmp_ptr->next;
	    }
	    return TCL_OK;
	}
    } else if (argc == 4) {
	/* exist superior mandatory conditional oid */
	/* return the definition of the class (argv[2]), if exists! */
/*	gdmo_class	*tmp_ptr = class_def_list; */

	if (!tmp_ptr || strcmp(tmp_ptr->label->string, argv[2])) {
	    tmp_ptr = class_def_list;
	    while (tmp_ptr && strcmp(tmp_ptr->label->string, argv[2])) {
		tmp_ptr = tmp_ptr->next;
	    }
	    if (!tmp_ptr) {
		Tcl_AppendResult(interp, "wrong arg: class \"", argv[2],
				 "\" doesn't exist!", (char *) NULL);
		return TCL_ERROR;
	    }
	}
	/* definition exists! */
	if (!strcmp(argv[3], "exist")) {
	    Tcl_SetResult(interp, tmp_ptr->only_label ? "0" : "1", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "superior")) {
	    class_list_result(interp, tmp_ptr->superclass);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "mandatory")) {
	    mand_pckg_result(interp, tmp_ptr->mand_pckg);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "conditional")) {
	    cond_pckg_result(interp, tmp_ptr->cond_pckg);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "oid")) {
	    oid_result(interp, tmp_ptr->oid);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "bad option \"", argv[3], "\": should be ",
			 "exist, superior, mandatory, conditional, oid",
			 (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " class ?name option?\"", (char *) NULL);
    return TCL_ERROR;
} /* option_class() */

/*===========================================================================*
 *
 *
 */

static void pckg_attr_result(interp, attr_list)
Tcl_Interp	*interp;
pckg_attr	*attr_list;
{
    Tcl_DString	dstring;

    Tcl_DStringInit(&dstring);
    while (attr_list) {
	Tcl_DStringStartSublist(&dstring);

	/* extract attribute_label */

	if (!attr_list->def) {
	    /* XXX fehler beim aufbau */
	    Warning("missing pointer to definition of attribute"); 
	    Tcl_DStringAppendElement(&dstring, "");
	} else {
	    Tcl_DStringAppendElement(&dstring, attr_list->def->label->string);
	}

	/* extract properties, if there! */

	Tcl_DStringStartSublist(&dstring);
	if (attr_list->property[t_replace_with_default]) {
	    Tcl_DStringAppendElement(&dstring, "REPLACE-WITH-DEFAULT");
	} else {
	    Tcl_DStringAppendElement(&dstring, "");
	}

	Tcl_DStringStartSublist(&dstring);
	if (attr_list->property[t_default_value] == 1) {
	    Tcl_DStringAppendElement(&dstring, "DEFAULT VALUE");
	    Tcl_DStringAppendElement(&dstring, attr_list->default_val->ref);
	} else if (attr_list->property[t_default_value] == 2) {
	    Tcl_DStringAppendElement(&dstring,
				     "DEFAULT VALUE DERIVATION RULE");
	    Tcl_DStringAppendElement(&dstring,
				     attr_list->default_val->behav->label->
				     string);
	}
	Tcl_DStringEndSublist(&dstring);

	Tcl_DStringStartSublist(&dstring);
	if (attr_list->property[t_initial_value] == 1) {
	    Tcl_DStringAppendElement(&dstring, "INITIAL VALUE");
	    Tcl_DStringAppendElement(&dstring, attr_list->initial_val->ref);
	} else if (attr_list->property[t_initial_value] == 2) {
	    Tcl_DStringAppendElement(&dstring,
				     "INITIAL VALUE DERIVATION RULE");
	    Tcl_DStringAppendElement(&dstring,
				     attr_list->initial_val->behav->label->
				     string);
	}
	Tcl_DStringEndSublist(&dstring);

	Tcl_DStringStartSublist(&dstring);
	if (attr_list->property[t_permitted_values]) {
	    Tcl_DStringAppendElement(&dstring, "PERMITTED VALUES");
	    Tcl_DStringAppendElement(&dstring, attr_list->permitted_values);
	}
	Tcl_DStringEndSublist(&dstring);
	
	Tcl_DStringStartSublist(&dstring);
	if (attr_list->property[t_required_values]) {
	    Tcl_DStringAppendElement(&dstring, "REQUIRED VALUES");
	    Tcl_DStringAppendElement(&dstring, attr_list->required_values);
	}
	Tcl_DStringEndSublist(&dstring);
	
	if (attr_list->property[t_get_replace] == 1) {
	    Tcl_DStringAppendElement(&dstring, "GET");
	} else if (attr_list->property[t_get_replace] == 2) {
	    Tcl_DStringAppendElement(&dstring, "REPLACE");
	} else if (attr_list->property[t_get_replace] == 3) {
	    Tcl_DStringAppendElement(&dstring, "GET-REPLACE");
	} else {
	    Tcl_DStringAppendElement(&dstring, "");
	}
	
	if (attr_list->property[t_add_remove] == 1) {
	    Tcl_DStringAppendElement(&dstring, "ADD");
	} else if (attr_list->property[t_add_remove] == 2) {
	    Tcl_DStringAppendElement(&dstring, "REMOVE");
	} else if (attr_list->property[t_add_remove] == 3) {
	    Tcl_DStringAppendElement(&dstring, "ADD-REMOVE");
	} else {
	    Tcl_DStringAppendElement(&dstring, "");
	}
	Tcl_DStringEndSublist(&dstring);
	
	/* and now the parameterlabels, if there! */

	param_list_dstring(&dstring, attr_list->param);

	Tcl_DStringEndSublist(&dstring);
	
	attr_list = attr_list->next;
    }
    Tcl_DStringResult(interp, &dstring);
} /* pckg_attr_result() */

static void pckg_group_result(interp, group_list)
Tcl_Interp	*interp;
pckg_group	*group_list;
{
    Tcl_DString	dstring;

    Tcl_DStringInit(&dstring);
    while (group_list) {
	Tcl_DStringStartSublist(&dstring);
	if (!group_list->def) {
	    /* XXX fehler beim aufbau */
	    Warning("missing pointer to definition of group"); 
	    Tcl_DStringAppendElement(&dstring, "");
	} else {
	    Tcl_DStringAppendElement(&dstring, group_list->def->label->string);
	}
	attr_list_dstring(&dstring, group_list->attr);

	Tcl_DStringEndSublist(&dstring);
	group_list = group_list->next;
    }
    Tcl_DStringResult(interp, &dstring);
} /* pckg_group_result() */

static void pckg_action_result(interp, action_list)
Tcl_Interp	*interp;
pckg_action	*action_list;
{
    Tcl_DString	dstring;

    Tcl_DStringInit(&dstring);
    while (action_list) {
	Tcl_DStringStartSublist(&dstring);
	if (!action_list->def) {
	    /* XXX fehler beim aufbau */
	    Warning("missing pointer to definition of action"); 
	    Tcl_DStringAppendElement(&dstring, "");
	} else {
	    Tcl_DStringAppendElement(&dstring,
				     action_list->def->label->string);
	}
	param_list_dstring(&dstring, action_list->param);

	Tcl_DStringEndSublist(&dstring);
	action_list = action_list->next;
    }
    Tcl_DStringResult(interp, &dstring);
}

static void pckg_notif_result(interp, notif_list)
Tcl_Interp	*interp;
pckg_notif	*notif_list;
{
    Tcl_DString	dstring;

    Tcl_DStringInit(&dstring);
    while (notif_list) {
	Tcl_DStringStartSublist(&dstring);
	if (!notif_list->def) {
	    /* XXX fehler beim aufbau */
	    Warning("missing pointer to definition of notification"); 
	    Tcl_DStringAppendElement(&dstring, "");
	} else {
	    Tcl_DStringAppendElement(&dstring, notif_list->def->label->string);
	}

	param_list_dstring(&dstring, notif_list->param);

	Tcl_DStringEndSublist(&dstring);
	notif_list = notif_list->next;
    }
    Tcl_DStringResult(interp, &dstring);
}

/*
 *
 *
 */

static int option_pckg(interp, argc, argv)
Tcl_Interp	*interp;
int		argc;
char		**argv;
{
    if (argc == 2) {
	if (!strcmp(argv[1], "info")) {
	    Tcl_SetResult(interp,
			  "exist behaviours attributes groups actions \
notifications oid",
			  TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[1], "package")) {
	    /* only the package option, return a list of the package_labels */
	    gdmo_pckg	*tmp_ptr = pckg_def_list;

	    while (tmp_ptr) {
		Tcl_AppendElement(interp, tmp_ptr->label->string);
		tmp_ptr = tmp_ptr->next;
	    }
	    return TCL_OK;
	}
    } else if (argc == 4) {
	/* return the definition of the package (argv[2]), if exists! */
	gdmo_pckg	*tmp_ptr = pckg_def_list;

	while (tmp_ptr && strcmp(tmp_ptr->label->string, argv[2])) {
	    tmp_ptr = tmp_ptr->next;
	}
	if (!tmp_ptr) {
	    Tcl_AppendResult(interp, "wrong arg: package \"", argv[2],
			     "\" doesn't exist!", (char *) NULL);
	    return TCL_ERROR;
	}
	/* definition exists! */
	if (!strcmp(argv[3], "exist")) {
	    Tcl_SetResult(interp, tmp_ptr->only_label ? "0" : "1", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "behaviours")) {
	    behav_list_result(interp, tmp_ptr->behav);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "attributes")) {
	    pckg_attr_result(interp, tmp_ptr->attr);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "groups")) {
	    pckg_group_result(interp, tmp_ptr->group);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "actions")) {
	    pckg_action_result(interp, tmp_ptr->action);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "notifications")) {
	    pckg_notif_result(interp, tmp_ptr->notif);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "oid")) {
	    oid_result(interp, tmp_ptr->oid);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "bad option \"", argv[3], "\": should be ",
			 "exist, behaviours, attributes, groups, ",
			 "actions, notifications, oid",
			 (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " package ?name option?\"", (char *) NULL);
    return TCL_ERROR;
}

/*===========================================================================*
 *
 *
 */

static int option_param(interp, argc, argv)
Tcl_Interp	*interp;
int		argc;
char		**argv;
{
    if (argc == 2) {
	if (!strcmp(argv[1], "info")) {
	    Tcl_SetResult(interp, "exist context choice behaviours oid",
			  TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[1], "parameter")) {
	    gdmo_param	*tmp_ptr = param_def_list;

	    while (tmp_ptr) {
		Tcl_AppendElement(interp, tmp_ptr->label->string);
		tmp_ptr = tmp_ptr->next;
	    }
	    return TCL_OK;
	}
    } else if (argc == 4) {
	/* return the definition of the parameter (argv[2]), if exists! */
	gdmo_param	*tmp_ptr = param_def_list;

	while (tmp_ptr && strcmp(tmp_ptr->label->string, argv[2])) {
	    tmp_ptr = tmp_ptr->next;
	}
	if (!tmp_ptr) {
	    Tcl_AppendResult(interp, "wrong arg: parameter \"", argv[2],
			     "\" doesn't exist!", (char *) NULL);
	    return TCL_ERROR;
	}
	/* definition exists! */
	if (!strcmp(argv[3], "exist")) {
	    Tcl_SetResult(interp, tmp_ptr->only_label ? "0" : "1", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "context")) {
	    if (tmp_ptr->context) {
		if (tmp_ptr->context->keyword) {
		    Tcl_AppendElement(interp, tmp_ptr->context->keyword);

		} else if (tmp_ptr->context->type != no_token) {
		    Tcl_AppendElement(interp,
				      context_string[tmp_ptr->context->type]);
		} else {	/* XXX shouldn't really happen */
		    Warning("missing context-keyword or -token"); 
/*		    Tcl_AppendElement(interp, ""); */
		}
	    } else {
		if (! tmp_ptr->only_label) {
		    /* XXX shouldn't really happen */
		    Warning("missing context");
		}
		Tcl_AppendElement(interp, "");
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "choice")) {
	    if (tmp_ptr->choice) {
		if (tmp_ptr->choice->syntax) {
		    Tcl_AppendElement(interp, tmp_ptr->choice->syntax);
		} else {
		    Tcl_AppendElement(interp, "");
		}
		if (tmp_ptr->choice->attr) {
		    Tcl_AppendElement(interp,
				      tmp_ptr->choice->attr->label->string);
		} else {
		    Tcl_AppendElement(interp, "");
		}
	    } else {
		if (! tmp_ptr->only_label) {
		    /* XXX shouldn't really happen */
		    Warning("missing syntax-or-attribute-choice"); 
		}
		Tcl_AppendElement(interp, "");
		Tcl_AppendElement(interp, "");
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "behaviours")) {
	    behav_list_result(interp, tmp_ptr->behav);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "oid")) {
	    oid_result(interp, tmp_ptr->oid);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "bad option \"", argv[3], "\": should be ",
			 "exist, context, choice, behaviours, oid",
			 (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " parameter ?name option?\"", (char *) NULL);
    return TCL_ERROR;
}

/*===========================================================================*
 *
 *
 */

static int option_namebind(interp, argc, argv)
Tcl_Interp	*interp;
int		argc;
char		**argv;
{
    if (argc == 2) {
	if (!strcmp(argv[1], "info")) {
	    Tcl_SetResult(interp,
			  "exist subordinate superior attribute behaviours \
create delete oid",
			  TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[1], "namebinding")) {
	    gdmo_namebind	*tmp_ptr = namebind_def_list;

	    while (tmp_ptr) {
		Tcl_AppendElement(interp, tmp_ptr->label->string);
		tmp_ptr = tmp_ptr->next;
	    }
	    return TCL_OK;
	}
    } else if (argc == 4) {
	/* return the definition of the namebinding (argv[2]), if exists! */
	gdmo_namebind	*tmp_ptr = namebind_def_list;

	while (tmp_ptr && strcmp(tmp_ptr->label->string, argv[2])) {
	    tmp_ptr = tmp_ptr->next;
	}
	if (!tmp_ptr) {
	    Tcl_AppendResult(interp, "wrong arg: namebinding \"", argv[2],
			     "\" doesn\'t exist!", (char *) NULL);
	    return TCL_ERROR;
	}
	/* definition exists! */
	if (!strcmp(argv[3], "exist")) {
	    Tcl_SetResult(interp, tmp_ptr->only_label ? "0" : "1", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "subordinate")) {
	    if (tmp_ptr->subclass) {
		if (tmp_ptr->subclass->def) {
		    Tcl_AppendElement(interp,
				      tmp_ptr->subclass->def->label->string);
		} else {	/* XXX shouldn't really happen */
		    Warning("missing subordinate object class");
		    Tcl_AppendElement(interp, "");
		}
		if (tmp_ptr->subclass->and_subs) {
		    Tcl_AppendElement(interp, "1");
		} else {
		    Tcl_AppendElement(interp, "0");
		}
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "superior")) {
	    if (tmp_ptr->superclass) {
		if (tmp_ptr->superclass->def) {
		    Tcl_AppendElement(interp,
				      tmp_ptr->superclass->def->label->string);
		} else {	/* XXX shouldn't really happen */
		    Warning("missing superior object class");
		    Tcl_AppendElement(interp, "");
		}
		if (tmp_ptr->superclass->and_subs) {
		    Tcl_AppendElement(interp, "1");
		} else {
		    Tcl_AppendElement(interp, "0");
		}
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "attribute")) {
	    if (tmp_ptr->attr) {
		Tcl_AppendElement(interp, tmp_ptr->attr->label->string);
	    } else {
		if (! tmp_ptr->only_label) {
		    /* XXX shouldn't really happen */
		    Warning("missing with attribute");
		}
		Tcl_AppendElement(interp, "");
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "behaviours")) {
	    behav_list_result(interp, tmp_ptr->behav);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "create")) {
	    Tcl_DString	dstring;

	    Tcl_DStringInit(&dstring);
	    if (tmp_ptr->create) {
		if (tmp_ptr->create->modifier) {
		    Tcl_DStringAppendElement(&dstring,
					     create_string[tmp_ptr->create->
							   modifier]);
		} else {
		    Tcl_DStringAppendElement(&dstring, "");
		}
		param_list_dstring(&dstring, tmp_ptr->create->param);
	    }
	    Tcl_DStringResult(interp, &dstring);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "delete")) {
	    Tcl_DString	dstring;

	    Tcl_DStringInit(&dstring);
	    if (tmp_ptr->delete) {
		if (tmp_ptr->delete->modifier) {
		    Tcl_DStringAppendElement(&dstring,
					     delete_string[tmp_ptr->delete->
							   modifier]);
		} else {
		    Tcl_DStringAppendElement(&dstring, "");
		}
		param_list_dstring(&dstring, tmp_ptr->delete->param);
	    }
	    Tcl_DStringResult(interp, &dstring);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "oid")) {
	    oid_result(interp, tmp_ptr->oid);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "bad option \"", argv[3], "\": should be ",
			 "exist, subordinate, superior, attribute, ",
			 "behaviours, create, delete, oid",
			 (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " namebinding ?name option?\"", (char *) NULL);
    return TCL_ERROR;
}

/*===========================================================================*
 *
 *
 */

static int option_attr(interp, argc, argv)
Tcl_Interp	*interp;
int		argc;
char		**argv;
{
    if (argc == 2) {
	if (!strcmp(argv[1], "info")) {
	    Tcl_SetResult(interp,
			  "exist choice matchesfor behaviours parameters oid",
			  TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[1], "attribute")) {
	    gdmo_attr	*tmp_ptr = attr_def_list;

	    while (tmp_ptr) {
		Tcl_AppendElement(interp, tmp_ptr->label->string);
		tmp_ptr = tmp_ptr->next;
	    }
	    return TCL_OK;
	}
    } else if (argc == 4) {
	/* return the definition of the attribute (argv[2]), if exists! */
	gdmo_attr	*tmp_ptr = attr_def_list;

	while (tmp_ptr && strcmp(tmp_ptr->label->string, argv[2])) {
	    tmp_ptr = tmp_ptr->next;
	}
	if (!tmp_ptr) {
	    Tcl_AppendResult(interp, "wrong arg: attribute \"", argv[2],
			     "\" doesn\'t exist!", (char *) NULL);
	    return TCL_ERROR;
	}
	/* definition exists! */
	if (!strcmp(argv[3], "exist")) {
	    Tcl_SetResult(interp, tmp_ptr->only_label ? "0" : "1", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "choice")) {
	    if (tmp_ptr->choice) {
		if (tmp_ptr->choice->attr) {
		    Tcl_AppendElement(interp,
				      tmp_ptr->choice->attr->label->string);
		} else {
		    Tcl_AppendElement(interp, "");
		}
		if (tmp_ptr->choice->syntax) {
		    Tcl_AppendElement(interp, tmp_ptr->choice->syntax);
		} else {
		    Tcl_AppendElement(interp, "");
		}
	    } else {
		if (! tmp_ptr->only_label) {
		    /* XXX shouldn't really happen */
		    Warning("missing derived-or-with-syntax-choice");
		}
		Tcl_AppendElement(interp, "");
		Tcl_AppendElement(interp, "");
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "matchesfor")) {
	    int		i;

	    for (i=0; i<5; i++) {
		if (tmp_ptr->matches_for[i]) {
		    Tcl_AppendElement(interp, matches_string[i]);
		}
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "behaviours")) {
	    behav_list_result(interp, tmp_ptr->behav);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "parameters")) {
	    param_list_result(interp, tmp_ptr->param);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "oid")) {
	    oid_result(interp, tmp_ptr->oid);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "bad option \"", argv[3], "\": should be ",
			 "exist, choice, matchesfor, behaviours, ",
			 "parameters, oid",
			 (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " attribute ?name option?\"", (char *) NULL);
    return TCL_ERROR;
}

/*===========================================================================*
 *
 *
 */

static int option_group(interp, argc, argv)
Tcl_Interp	*interp;
int		argc;
char		**argv;
{
    if (argc == 2) {
	if (!strcmp(argv[1], "info")) {
	    Tcl_SetResult(interp,
			  "exist attributes fixed description oid",
			  TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[1], "group")) {
	    gdmo_group	*tmp_ptr = group_def_list;

	    while (tmp_ptr) {
		Tcl_AppendElement(interp, tmp_ptr->label->string);
		tmp_ptr = tmp_ptr->next;
	    }
	    return TCL_OK;
	}
    } else if (argc == 4) {
	/* return the definition of the group (argv[2]), if exists! */
	gdmo_group	*tmp_ptr = group_def_list;

	while (tmp_ptr && strcmp(tmp_ptr->label->string, argv[2])) {
	    tmp_ptr = tmp_ptr->next;
	}
	if (!tmp_ptr) {
	    Tcl_AppendResult(interp, "wrong arg: group \"", argv[2],
			     "\" doesn\'t exist!", (char *) NULL);
	    return TCL_ERROR;
	}
	/* definition exists! */
	if (!strcmp(argv[3], "exist")) {
	    Tcl_SetResult(interp, tmp_ptr->only_label ? "0" : "1", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "attributes")) {
	    attr_list_result(interp, tmp_ptr->attr);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "fixed")) {
	    Tcl_SetResult(interp, tmp_ptr->fixed ? "1" : "0", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "description")) {
	    if (!tmp_ptr->description) {
		/* XXX kann spaeter entfallen!? */
/*		Tcl_SetResult(interp, "", TCL_STATIC); */
	    } else {
		ReadTextFromFile(interp, tmp_ptr->description, NULL);
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "oid")) {
	    oid_result(interp, tmp_ptr->oid);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "bad option \"", argv[3], "\": should be ",
			 "exist, attributes, fixed, description, oid",
			 (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " group ?name option?\"", (char *) NULL);
    return TCL_ERROR;
}

/*===========================================================================*
 *
 *
 */

static int option_action(interp, argc, argv)
Tcl_Interp	*interp;
int		argc;
char		**argv;
{
    if (argc == 2) {
	if (!strcmp(argv[1], "info")) {
	    Tcl_SetResult(interp,
			  "exist behaviours mode parameters infosyntax \
replysyntax oid",
			  TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[1], "action")) {
	    gdmo_action	*tmp_ptr = action_def_list;

	    while (tmp_ptr) {
		Tcl_AppendElement(interp, tmp_ptr->label->string);
		tmp_ptr = tmp_ptr->next;
	    }
	    return TCL_OK;
	}
    } else if (argc == 4) {
	/* return the definition of the action (argv[2]), if exists! */
	gdmo_action	*tmp_ptr = action_def_list;

	while (tmp_ptr && strcmp(tmp_ptr->label->string, argv[2])) {
	    tmp_ptr = tmp_ptr->next;
	}
	if (!tmp_ptr) {
	    Tcl_AppendResult(interp, "wrong arg: action \"", argv[2],
			     "\" doesn\'t exist!", (char *) NULL);
	    return TCL_ERROR;
	}
	/* definition exists! */
	if (!strcmp(argv[3], "exist")) {
	    Tcl_SetResult(interp, tmp_ptr->only_label ? "0" : "1", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "behaviours")) {
	    behav_list_result(interp, tmp_ptr->behav);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "mode")) {
	    Tcl_SetResult(interp, tmp_ptr->confirmed ? "1" : "0",
			  TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "parameters")) {
	    param_list_result(interp, tmp_ptr->param);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "infosyntax")) {
	    if (tmp_ptr->info_syntax) {
		Tcl_SetResult(interp, tmp_ptr->info_syntax, TCL_STATIC);
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "replysyntax")) {
	    if (tmp_ptr->reply_syntax) {
		Tcl_SetResult(interp, tmp_ptr->reply_syntax, TCL_STATIC);
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "oid")) {
	    oid_result(interp, tmp_ptr->oid);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "bad option \"", argv[3], "\": should be ",
			 "exist, behaviours, mode, parameters, ",
			 "infosyntax, replysyntax, oid",
			 (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " action ?name option?\"", (char *) NULL);
    return TCL_ERROR;
} /* option_action */


/*===========================================================================*
 *
 *
 */

static void notif_info_ids_result(interp, info_ids)
Tcl_Interp	  *interp;
notif_info_syntax *info_ids;
{
    Tcl_DString	dstring;

    Tcl_DStringInit(&dstring);
    if (info_ids) {
	if (!info_ids->syntax) {
	    Warning("missing information syntax");
	    Tcl_DStringAppendElement(&dstring, "");
	} else {
	    Tcl_DStringAppendElement(&dstring, info_ids->syntax);
	}
	if (!info_ids->attrids) {
	    Tcl_DStringAppendElement(&dstring, "");
	} else {
	    notif_attr_ids	*tmp_notif_ids = info_ids->attrids;

	    Tcl_DStringStartSublist(&dstring);
	    while (tmp_notif_ids) {
		Tcl_DStringStartSublist(&dstring);
		Tcl_DStringAppendElement(&dstring, tmp_notif_ids->field_name);
		Tcl_DStringAppendElement(&dstring,
					 tmp_notif_ids->def->label->string);
		Tcl_DStringEndSublist(&dstring);

		tmp_notif_ids = tmp_notif_ids->next;
	    }
	    Tcl_DStringEndSublist(&dstring);
	}
    }
    Tcl_DStringResult(interp, &dstring);
} /* notif_info_ids_result() */

/*===========================================================================*
 *
 *
 */

static int 
GdmoNotification (interp, argc, argv)
     Tcl_Interp	*interp;
     int argc;
     char **argv;
{
    if (argc == 2) {
	if (!strcmp(argv[1], "info")) {
	    Tcl_SetResult(interp,
		"exist behaviours parameters infosyntaxandids replysyntax oid",
			  TCL_STATIC);
	    return TCL_OK;
	    
	} else if (!strcmp(argv[1], "notification")) {
	    gdmo_notif *tmp_ptr = notif_def_list;
	    
	    while (tmp_ptr) {
		Tcl_AppendElement(interp, tmp_ptr->label->string);
		tmp_ptr = tmp_ptr->next;
	    }
	    return TCL_OK;
	}
    } else if (argc == 4) {
	/* return the definition of the action (argv[2]), if exists! */
	gdmo_notif	*tmp_ptr = notif_def_list;

	while (tmp_ptr && strcmp(tmp_ptr->label->string, argv[2])) {
	    tmp_ptr = tmp_ptr->next;
	}
	if (!tmp_ptr) {
	    Tcl_AppendResult(interp, "wrong arg: notification \"", argv[2],
			     "\" doesn\'t exist!", (char *) NULL);
	    return TCL_ERROR;
	}
	/* definition exists! */
	if (!strcmp(argv[3], "exist")) {
	    Tcl_SetResult(interp, tmp_ptr->only_label ? "0" : "1", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "behaviours")) {
	    behav_list_result(interp, tmp_ptr->behav);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "parameters")) {
	    param_list_result(interp, tmp_ptr->param);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "infosyntaxandids")) {
	    notif_info_ids_result(interp, tmp_ptr->syntax_and_attr_ids);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "replysyntax")) {
	    if (tmp_ptr->reply_syntax) {
		Tcl_SetResult(interp, tmp_ptr->reply_syntax, TCL_STATIC);
	    }
	    return TCL_OK;

	} else if (!strcmp(argv[3], "oid")) {
	    oid_result(interp, tmp_ptr->oid);
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "bad option \"", argv[3], "\": should be ",
			 "exist, behaviours, parameters, ",
			 "infosyntaxandids, replysyntax, oid",
			 (char *) NULL);
	return TCL_ERROR;
    }
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
		     " notification ?name option?\"", (char *) NULL);
    return TCL_ERROR;
}


/*===========================================================================*
 *
 *
 */

static int 
GdmoBehaviour (interp, argc, argv)
     Tcl_Interp	*interp;
     int argc;
     char **argv;
{
    if (argc == 2) {
	if (!strcmp(argv[1], "info")) {
	    Tcl_SetResult(interp, "exist definition", TCL_STATIC);
	    return TCL_OK;
	} else if (!strcmp(argv[1], "behaviour")) {
	    gdmo_behav	*tmp_ptr = behav_def_list;

	    while (tmp_ptr) {
		Tcl_AppendElement(interp, tmp_ptr->label->string);
		tmp_ptr = tmp_ptr->next;
	    }
	    return TCL_OK;
	}
    } else if (argc == 4) {
	gdmo_behav	*tmp_ptr = behav_def_list;

	while (tmp_ptr && strcmp(tmp_ptr->label->string, argv[2])) {
	    tmp_ptr = tmp_ptr->next;
	}
	if (!tmp_ptr) {
	    Tcl_AppendResult(interp, "wrong arg: behaviour \"", argv[2],
			     "\" doesn\'t exist!", (char *) NULL);
	    return TCL_ERROR;
	}
	if (!strcmp(argv[3], "exist")) {
	    Tcl_SetResult(interp, tmp_ptr->only_label ? "0" : "1", TCL_STATIC);
	    return TCL_OK;

	} else if (!strcmp(argv[3], "definition")) {
	    if (!tmp_ptr->description) {
		if (! tmp_ptr->only_label) {
		    Warning("missing defined as in behaviour");
		}
		Tcl_AppendElement(interp, "");
	    } else {
		ReadTextFromFile(interp, tmp_ptr->description, NULL);
	    }
	    return TCL_OK;
	}
	Tcl_AppendResult(interp, "bad option \"", argv[3], "\": should be ",
			 "exist, definition",
			 (char *) NULL);
	return TCL_ERROR;
    }

    Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
		      " behaviour ?name option?\"", (char *) NULL);
    return TCL_ERROR;
}


/*===========================================================================*
 *
 * GdmoLoad() searches for the given file in the current directory and
 * in $scotty_library/site and $scotty_library/gdmos. If successful, the 
 * file is run through the parser.
 */

static int 
GdmoLoad (interp, argc, argv)
     Tcl_Interp	*interp;
     int argc;
     char **argv;
{
    FILE *fin = NULL;
    Tcl_DString	dst;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " load path\"", (char *) NULL);
	return TCL_ERROR;
    }

    gdmo_file = Tcl_TildeSubst (interp, argv[2], &dst);
    if (!gdmo_file) {
	Tcl_DStringFree (&dst);
	return TCL_ERROR;
    }

    if (access (gdmo_file, R_OK) != 0) {

	char *libDir = Tcl_GetVar (interp, "scotty_library", TCL_GLOBAL_ONLY);
	
	if (libDir) {
	    Tcl_DStringFree (&dst);
	    Tcl_DStringAppend (&dst, libDir, -1);
	    Tcl_DStringAppend (&dst, "/site/", 6);
	    Tcl_DStringAppend (&dst, argv[2], -1);
	    gdmo_file = Tcl_DStringValue (&dst);
	}
	
	if (libDir && (access (gdmo_file, R_OK) != 0)) {
	    Tcl_DStringFree (&dst);
            Tcl_DStringAppend (&dst, libDir, -1);
            Tcl_DStringAppend (&dst, "/gdmos/", 7);
            Tcl_DStringAppend (&dst, argv[2], -1);
            gdmo_file = Tcl_DStringValue (&dst);
	}
	    
	if (libDir && (access (gdmo_file, R_OK) != 0)) {
	    Tcl_AppendResult (interp, "couldn't open GDMO file \"", argv[2],
			      "\": ", Tcl_PosixError (interp), (char *) NULL);
	    Tcl_DStringFree (&dst);
	    return TCL_ERROR;
	}
    }
    
    gdmo_file = ckstrdup (gdmo_file);
    Tcl_DStringFree (&dst);
    
    fin = fopen (gdmo_file, "r");
    if (!fin) {
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "couldn't open \"", gdmo_file, "\": ",
			 Tcl_PosixError(interp), (char *) NULL);
	ckfree (gdmo_file);
	return TCL_ERROR;
    }

#ifdef YYDEBUG
    yydebug = 1;
#endif
    
    gdmo_file_pos = -1;
    lineno = 1;
    reentered = 0;

    yyrestart(fin);
    if (yyparse()) {
	fclose (fin);
	Tcl_AppendResult(interp, "parsing GDMO file \"", gdmo_file,
			 "\" failed: probably not a GDMO file",
			 (char *) NULL);
	ckfree (gdmo_file);
	return TCL_ERROR;
    }

    fclose (fin);
    return TCL_OK;
}


/*===========================================================================*
 *
 *
 */

static int
GdmoInfo (interp, argc, argv)
     Tcl_Interp	*interp;
     int argc;
     char **argv;
{
    if (argc != 3) {
        Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " info option\"", (char *) NULL);
        return TCL_ERROR;
    }
    
    --argc;	/* pretend there is only "info" for the options */

    if (!strcmp(argv[2], "class")) {
	return option_class(interp, argc, argv);
    } else if (!strcmp(argv[2], "package")) {
	return option_pckg(interp, argc, argv);
    } else if (!strcmp(argv[2], "parameter")) {
	return option_param(interp, argc, argv);
    } else if (!strcmp(argv[2], "namebinding")) {
	return option_namebind(interp, argc, argv);
    } else if (!strcmp(argv[2], "attribute")) {
	return option_attr(interp, argc, argv);
    } else if (!strcmp(argv[2], "group")) {
	return option_group(interp, argc, argv);
    } else if (!strcmp(argv[2], "action")) {
	return option_action(interp, argc, argv);
    } else if (!strcmp(argv[2], "notification")) {
	return GdmoNotification (interp, argc, argv);
    } else if (!strcmp(argv[2], "behaviour")) {
	return GdmoBehaviour (interp, argc, argv);
    }

    Tcl_AppendResult(interp, "bad option \"", argv[2], "\": should be ",
		     "class, package, parameter, namebinding, ",
		     "attribute, group, action, notification, or behaviour",
		     (char *) NULL);
    return TCL_ERROR;
}


/*===========================================================================*
 *
 * This is the gdmo command as described in the scotty documentation.
 * It simply dispatches to the C functions implementing the options
 * understood by the gdmo command.
 */

static int 
GdmoCmd (clientData, interp, argc, argv)
     ClientData	clientData;
     Tcl_Interp	*interp;
     int argc;
     char **argv;
{
    if (argc < 2) {
        Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " option ?arg arg ...?\"", (char *) NULL);
        return TCL_ERROR;
    }

    if (!strcmp(argv[1], "load")) {
	return GdmoLoad (interp, argc, argv);
    } else if (!strcmp(argv[1], "info")) {
	return GdmoInfo (interp, argc, argv);
    } else if (!strcmp(argv[1], "class")) {
	return option_class(interp, argc, argv);
    } else if (!strcmp(argv[1], "package")) {
	return option_pckg(interp, argc, argv);
    } else if (!strcmp(argv[1], "parameter")) {
	return option_param(interp, argc, argv);
    } else if (!strcmp(argv[1], "namebinding")) {
	return option_namebind(interp, argc, argv);
    } else if (!strcmp(argv[1], "attribute")) {
	return option_attr(interp, argc, argv);
    } else if (!strcmp(argv[1], "group")) {
	return option_group(interp, argc, argv);
    } else if (!strcmp(argv[1], "action")) {
	return option_action(interp, argc, argv);
    } else if (!strcmp(argv[1], "notification")) {
	return GdmoNotification (interp, argc, argv);
    } else if (!strcmp(argv[1], "behaviour")) {
	return GdmoBehaviour (interp, argc, argv);
    }

    Tcl_AppendResult(interp, "bad option \"", argv[1], "\": should be ",
		     "load, info, class, package, parameter, namebinding, ",
		     "attribute, group, action, notification, or behaviour",
		     (char *) NULL);
    return TCL_ERROR;
}


/*===========================================================================*
 *
 * Gdmo_Init() registers the gdmo Tcl command. This is the exported 
 * public function of the gdmo extension.
 */

int 
Gdmo_Init (interp)
     Tcl_Interp *interp;
{
    Tcl_CreateCommand (interp, "gdmo", GdmoCmd,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

    return TCL_OK;

}
