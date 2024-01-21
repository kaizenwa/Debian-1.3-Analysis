/*
 * gdmoParser.y
 *
 * Implementation of a parser for GDMO files
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

%{	/* DEFINITIONS */

#include "gdmo.h"

int reentered;		/* false, if mid-action is called for the first time */

/* XXX interfacing with flex?? */
extern char	*yytext;
extern int	lineno;		/* actual line number in the parsed file */

/* XXX interfacing with gdmotcl.c */
extern char	*gdmo_file;	/* path name for the parsed file */

/*
 * Global variables to hold the linear lists of the 
 * corresponding definitions:
 */

gdmo_class	*class_def_list	= NULL;
gdmo_pckg	*pckg_def_list	= NULL;
gdmo_param	*param_def_list	= NULL;
gdmo_namebind	*namebind_def_list = NULL;
gdmo_attr	*attr_def_list	= NULL;
gdmo_group	*group_def_list	= NULL;
gdmo_behav	*behav_def_list	= NULL;
gdmo_action	*action_def_list = NULL;
gdmo_notif	*notif_def_list	= NULL;

/*
 * Forward declarations for procedures defined later in this file:
 */

static void
Redefinition            _ANSI_ARGS_((char * template, char *name));

static gdmo_class *
add_class_def		_ANSI_ARGS_((gdmo_label*, class_ref*,
				     class_pckg*, class_pckg*,
				     gdmo_oid*, int));
static gdmo_class *
check_class_def		_ANSI_ARGS_((gdmo_label*));

static gdmo_pckg *
add_pckg_def		_ANSI_ARGS_((gdmo_label*, behav_ref*,
				     pckg_attr*, pckg_group*,
				     pckg_action*, pckg_notif*,
				     gdmo_oid*, int));
static gdmo_pckg *
check_pckg_def		_ANSI_ARGS_((gdmo_label*));

static gdmo_param *
add_param_def		_ANSI_ARGS_((gdmo_label*, param_context*,
				     syntax_or_attr*, behav_ref*,
				     gdmo_oid*, int));
static gdmo_param *
check_param_def		_ANSI_ARGS_((gdmo_label*));

static gdmo_namebind *
add_namebind_def	_ANSI_ARGS_((gdmo_label*, namebind_class*,
				     namebind_class*, gdmo_attr*, behav_ref*,
				     namebind_create*, namebind_delete*,
				     gdmo_oid*, int));
static gdmo_namebind *
check_namebind_def	_ANSI_ARGS_((gdmo_label*));

static gdmo_attr *
add_attr_def		_ANSI_ARGS_((gdmo_label*, syntax_or_attr*, int*,
				     behav_ref*, param_ref*,
				     gdmo_oid*, int));
static gdmo_attr *
check_attr_def		_ANSI_ARGS_((gdmo_label*));

static gdmo_group *
add_group_def		_ANSI_ARGS_((gdmo_label*, attr_ref*, int,
				     text_in_file*, gdmo_oid*, int));
static gdmo_group *
check_group_def		_ANSI_ARGS_((gdmo_label*));

static gdmo_behav *
add_behav_def		_ANSI_ARGS_((gdmo_label*, text_in_file*,int));

static gdmo_behav *
check_behav_def		_ANSI_ARGS_((gdmo_label*));

static gdmo_action *
add_action_def		_ANSI_ARGS_((gdmo_label*, behav_ref*, int,
				     param_ref*, type_ref*,
				     type_ref*, gdmo_oid*, int));
static gdmo_action *
check_action_def	_ANSI_ARGS_((gdmo_label*));

static gdmo_notif *
add_notif_def		_ANSI_ARGS_((gdmo_label*, behav_ref*,
				     param_ref*, notif_info_syntax*,
				     type_ref*, gdmo_oid*, int));
static gdmo_notif *
check_notif_def		_ANSI_ARGS_((gdmo_label*));

/*
 * Print a warning message about a redefinition. We assume the the
 * global variables point to the actual file and line number.
 */

static void
Redefinition (template, name)
     char *template;
     char *name;
{
    fprintf (stderr, "%s:%d warning: redefinition of %s \"%s\"\n", 
             gdmo_file, lineno, template, name);
}


/* 
 * Called by yyparse if we found an error. We assume the the
 * global variables point to the actual file and line number.
 */

static void
yyerror (msg)
    char *msg;
{
    fprintf(stderr, "%s:%d: %s\n", gdmo_file, lineno, msg);
}

/*============================================================================*
 *
 * adds the definition given by the arguments to class_def_list,
 * if necessary and returns a (gdmo_class *) to the definition
 */

static gdmo_class *
add_class_def (class_label, superior, mandatory, conditional,
	       obj_id, only_label)
     gdmo_label	*class_label;
     class_ref	*superior;
     class_pckg	*mandatory, *conditional;
     gdmo_oid	*obj_id;
     int	 only_label;
{
    gdmo_class *tmp_ptr;

    if (!class_def_list) { /* first definition */
	class_def_list = (gdmo_class *) ckalloc(sizeof(gdmo_class));

	class_def_list->label = class_label;
	class_def_list->only_label = only_label;
	class_def_list->inline_def = 0;
	class_def_list->superclass = superior;
	class_def_list->mand_pckg = mandatory;
	class_def_list->cond_pckg = conditional;
	class_def_list->oid = obj_id;
	class_def_list->next = NULL;

	return class_def_list;
    }
    tmp_ptr = class_def_list;
    while (tmp_ptr->next &&
	   strcmp(tmp_ptr->label->string, class_label->string)) {
	/* while there is a next definition and this definition has not
	 * the searched label, search on
	 */
	tmp_ptr = tmp_ptr->next;
    };
    if (!tmp_ptr->next &&
	strcmp(tmp_ptr->label->string, class_label->string)) {
	/* if this is the last definition and it has not the searched label:
	 * insert new entry at the end of class_def_list
	 */
	tmp_ptr->next = (gdmo_class *) ckalloc(sizeof(gdmo_class));

	tmp_ptr = tmp_ptr->next;
	tmp_ptr->label = class_label;
	tmp_ptr->only_label = only_label;
	tmp_ptr->inline_def = 0;
	tmp_ptr->superclass = superior;
	tmp_ptr->mand_pckg = mandatory;
	tmp_ptr->cond_pckg = conditional;
	tmp_ptr->oid = obj_id;
	tmp_ptr->next = NULL;

	return tmp_ptr;
    }
    /* label already exists! */
    if (!only_label) { /* not only checking! */
	tmp_ptr->inline_def = 0;
	tmp_ptr->superclass = superior;
	tmp_ptr->mand_pckg = mandatory;
	tmp_ptr->cond_pckg = conditional;
	tmp_ptr->oid = obj_id;

	if (!tmp_ptr->only_label) {
	    Redefinition ("class", tmp_ptr->label->string);
	}
	tmp_ptr->only_label = 0;
    }
    return tmp_ptr;
}

/*
 * checks, if "class_label" is allready defined, if not, an entry is
 * created, with only a label and only_label = 1
 * returns a (gdmo_class *) to the definition of "class_label"
 */

static gdmo_class *check_class_def(class_label)
gdmo_label	*class_label;
{
    return add_class_def(class_label, NULL, NULL, NULL, NULL, 1);
}

/*============================================================================*
 *
 * see add_class_def()
 */

static gdmo_pckg *add_pckg_def(pckg_label, behav_list, attr_list, group_list,
			       action_list, notif_list, obj_id, only_label)
gdmo_label	*pckg_label;
behav_ref	*behav_list;
pckg_attr	*attr_list;
pckg_group	*group_list;
pckg_action	*action_list;
pckg_notif	*notif_list;
gdmo_oid	*obj_id;
int		only_label;
{
    gdmo_pckg *tmp_ptr;

    if (!pckg_def_list) { /* first definition */
	pckg_def_list = (gdmo_pckg *) ckalloc(sizeof(gdmo_pckg));

	pckg_def_list->label = pckg_label;
	pckg_def_list->only_label = only_label;
	pckg_def_list->inline_def = 0;
	pckg_def_list->behav = behav_list;
	pckg_def_list->attr = attr_list;
	pckg_def_list->group = group_list;
	pckg_def_list->action = action_list;
	pckg_def_list->notif = notif_list;
	pckg_def_list->oid = obj_id;
	pckg_def_list->next = NULL;

	return pckg_def_list;
    }
    tmp_ptr = pckg_def_list;
    while (tmp_ptr->next &&
	   strcmp(tmp_ptr->label->string, pckg_label->string)) {
	tmp_ptr = tmp_ptr->next;
    };
    if (!tmp_ptr->next && strcmp(tmp_ptr->label->string, pckg_label->string)) {
	/* insert new entry at the end of pckg_def_list */
	tmp_ptr->next = (gdmo_pckg *) ckalloc(sizeof(gdmo_pckg));

	tmp_ptr = tmp_ptr->next;
	tmp_ptr->label = pckg_label;
	tmp_ptr->only_label = only_label;
	tmp_ptr->inline_def = 0;
	tmp_ptr->behav = behav_list;
	tmp_ptr->attr = attr_list;
	tmp_ptr->group = group_list;
	tmp_ptr->action = action_list;
	tmp_ptr->notif = notif_list;
	tmp_ptr->oid = obj_id;
	tmp_ptr->next = NULL;

	return tmp_ptr;
    }
    if (!only_label) {
	tmp_ptr->inline_def = 0;
	tmp_ptr->behav = behav_list;
	tmp_ptr->attr = attr_list;
	tmp_ptr->group = group_list;
	tmp_ptr->action = action_list;
	tmp_ptr->notif = notif_list;
	tmp_ptr->oid = obj_id;

	if (!tmp_ptr->only_label) {
	    Redefinition ("package", tmp_ptr->label->string);
	}
	tmp_ptr->only_label = 0;
    }
    return tmp_ptr;
}

/*
 * see check_class_def()
 */

static gdmo_pckg *check_pckg_def(pckg_label)
gdmo_label	*pckg_label;
{
    return add_pckg_def(pckg_label, NULL, NULL, NULL, NULL, NULL, NULL, 1);
}

/*============================================================================*
 *
 * see add_class_def()
 */

static gdmo_param *add_param_def(param_label, context_type, syn_or_attr,
				 behav_list, obj_id, only_label)
gdmo_label	*param_label;
param_context	*context_type;
syntax_or_attr	*syn_or_attr;
behav_ref	*behav_list;
gdmo_oid	*obj_id;
int		only_label;
{
    gdmo_param *tmp_ptr;

    if (!param_def_list) { /* first definition */
	param_def_list = (gdmo_param *) ckalloc(sizeof(gdmo_param));

	param_def_list->label = param_label;
	param_def_list->only_label = only_label;
	param_def_list->inline_def = 0;
	param_def_list->context = context_type;
	param_def_list->choice = syn_or_attr;
	param_def_list->behav = behav_list;
	param_def_list->oid = obj_id;
	param_def_list->next = NULL;

	return param_def_list;
    }
    tmp_ptr = param_def_list;
    while (tmp_ptr->next &&
	   strcmp(tmp_ptr->label->string, param_label->string)) {
	tmp_ptr = tmp_ptr->next;
    };
    if (!tmp_ptr->next &&
	strcmp(tmp_ptr->label->string, param_label->string)) {
	/* insert new entry at the beginning of param_def_list */
	tmp_ptr->next = (gdmo_param *) ckalloc(sizeof(gdmo_param));

	tmp_ptr = tmp_ptr->next;
	tmp_ptr->label = param_label;
	tmp_ptr->only_label = only_label;
	tmp_ptr->inline_def = 0;
	tmp_ptr->context = context_type;
	tmp_ptr->choice = syn_or_attr;
	tmp_ptr->behav = behav_list;
	tmp_ptr->oid = obj_id;
	tmp_ptr->next = NULL;

	return tmp_ptr;
    }
    if (!only_label) {
	tmp_ptr->inline_def = 0;
	tmp_ptr->context = context_type;
	tmp_ptr->choice = syn_or_attr;
	tmp_ptr->behav = behav_list;
	tmp_ptr->oid = obj_id;

	if (!tmp_ptr->only_label) {
	    Redefinition ("parameter", tmp_ptr->label->string);
	}
	tmp_ptr->only_label = 0;
    }
    return tmp_ptr;
}

/*
 * see check_class_def()
 */

static gdmo_param *check_param_def(param_label)
gdmo_label	*param_label;
{
    return add_param_def(param_label, NULL, NULL, NULL, NULL, 1);
}

/*============================================================================*
 *
 * see add_class_def()
 */

static gdmo_namebind *add_namebind_def(namebind_label, subordinate, superior,
				       attribute, behav_list,
				       create_list, delete_list,
				       obj_id, only_label)
gdmo_label	*namebind_label;
namebind_class	*subordinate, *superior;
gdmo_attr	*attribute;
behav_ref	*behav_list;
namebind_create	*create_list;
namebind_delete	*delete_list;
gdmo_oid	*obj_id;
int		only_label;
{
    gdmo_namebind *tmp_ptr;

    if (!namebind_def_list) { /* first definition */
	namebind_def_list = (gdmo_namebind *) ckalloc(sizeof(gdmo_namebind));

	namebind_def_list->label = namebind_label;
	namebind_def_list->only_label = only_label;
	namebind_def_list->inline_def = 0;
	namebind_def_list->subclass = subordinate;
	namebind_def_list->superclass = superior;
	namebind_def_list->attr = attribute;
	namebind_def_list->behav = behav_list;
	namebind_def_list->create = create_list;
	namebind_def_list->delete = delete_list;
	namebind_def_list->oid = obj_id;
	namebind_def_list->next = NULL;

	return namebind_def_list;
    }
    tmp_ptr = namebind_def_list;
    while (tmp_ptr->next &&
	   strcmp(tmp_ptr->label->string, namebind_label->string)) {
        tmp_ptr = tmp_ptr->next;
    };
    if (!tmp_ptr->next &&
	strcmp(tmp_ptr->label->string, namebind_label->string)) {
        /* insert new entry at the end of namebind_def_list */
	tmp_ptr->next = (gdmo_namebind *) ckalloc(sizeof(gdmo_namebind));

	tmp_ptr = tmp_ptr->next;
	tmp_ptr->label = namebind_label;
	tmp_ptr->only_label = only_label;
	tmp_ptr->inline_def = 0;
	tmp_ptr->subclass = subordinate;
	tmp_ptr->superclass = superior;
	tmp_ptr->attr = attribute;
	tmp_ptr->behav = behav_list;
	tmp_ptr->create = create_list;
	tmp_ptr->delete = delete_list;
	tmp_ptr->oid = obj_id;
	tmp_ptr->next = NULL;

	return tmp_ptr;
    }
    if (!only_label) {
	tmp_ptr->inline_def = 0;
	tmp_ptr->subclass = subordinate;
	tmp_ptr->superclass = superior;
	tmp_ptr->attr = attribute;
	tmp_ptr->behav = behav_list;
	tmp_ptr->create = create_list;
	tmp_ptr->delete = delete_list;
	tmp_ptr->oid = obj_id;

	if (!tmp_ptr->only_label) {
	    Redefinition ("name binding", tmp_ptr->label->string);
	}
	tmp_ptr->only_label = 0;
    }
    return tmp_ptr;
}

/*
 * see check_class_def()
 */

static gdmo_namebind *check_namebind_def(namebind_label)
gdmo_label	*namebind_label;
{
   return add_namebind_def(namebind_label, NULL, NULL, NULL, NULL, NULL, NULL,
			   NULL, 1);
}


/*============================================================================*
 *
 * see add_class_def()
 */

static gdmo_attr *add_attr_def(attr_label, attr_or_syntax, matches_for,
			       behav_list, param_list, obj_id, only_label)
gdmo_label	*attr_label;
syntax_or_attr	*attr_or_syntax;
int		matches_for[]; /* [5] enum matches */
behav_ref	*behav_list;
param_ref	*param_list;
gdmo_oid	*obj_id;
int		only_label;
{
    gdmo_attr *tmp_ptr;

    if (!attr_def_list) { /* first definition */
	attr_def_list = (gdmo_attr *) ckalloc(sizeof(gdmo_attr));

	attr_def_list->label = attr_label;
	attr_def_list->only_label = only_label;
	attr_def_list->inline_def = 0;
	attr_def_list->choice = attr_or_syntax;
	if (matches_for) {
	    attr_def_list->matches_for[0] = matches_for[0];
	    attr_def_list->matches_for[1] = matches_for[1];
	    attr_def_list->matches_for[2] = matches_for[2];
	    attr_def_list->matches_for[3] = matches_for[3];
	    attr_def_list->matches_for[4] = matches_for[4];
	} else {
	    attr_def_list->matches_for[0] = 0;
	    attr_def_list->matches_for[1] = 0;
	    attr_def_list->matches_for[2] = 0;
	    attr_def_list->matches_for[3] = 0;
	    attr_def_list->matches_for[4] = 0;
	}
	attr_def_list->behav = behav_list;
	attr_def_list->param = param_list;
	attr_def_list->oid = obj_id;
	attr_def_list->next = NULL;

	return attr_def_list;
    }
    tmp_ptr = attr_def_list;
    while (tmp_ptr->next &&
	   strcmp(tmp_ptr->label->string, attr_label->string)) {
        tmp_ptr = tmp_ptr->next;
    };
    if (!tmp_ptr->next &&
	strcmp(tmp_ptr->label->string, attr_label->string)) {
        /* insert new entry at the beginning of attr_def_list */
	tmp_ptr->next = (gdmo_attr *) ckalloc(sizeof(gdmo_attr));

	tmp_ptr = tmp_ptr->next;
	tmp_ptr->label = attr_label;
	tmp_ptr->only_label = only_label;
	tmp_ptr->inline_def = 0;
	tmp_ptr->choice = attr_or_syntax;
	if (matches_for) {
	    tmp_ptr->matches_for[0] = matches_for[0];
	    tmp_ptr->matches_for[1] = matches_for[1];
	    tmp_ptr->matches_for[2] = matches_for[2];
	    tmp_ptr->matches_for[3] = matches_for[3];
	    tmp_ptr->matches_for[4] = matches_for[4];
	} else {
	    tmp_ptr->matches_for[0] = 0;
	    tmp_ptr->matches_for[1] = 0;
	    tmp_ptr->matches_for[2] = 0;
	    tmp_ptr->matches_for[3] = 0;
	    tmp_ptr->matches_for[4] = 0;
	}
	tmp_ptr->behav = behav_list;
	tmp_ptr->param = param_list;
	tmp_ptr->oid = obj_id;
	tmp_ptr->next = NULL;

	return tmp_ptr;
    }
    if (!only_label) {
	tmp_ptr->inline_def = 0;
	tmp_ptr->choice = attr_or_syntax;
	if (matches_for) {
	    tmp_ptr->matches_for[0] = matches_for[0];
	    tmp_ptr->matches_for[1] = matches_for[1];
	    tmp_ptr->matches_for[2] = matches_for[2];
	    tmp_ptr->matches_for[3] = matches_for[3];
	    tmp_ptr->matches_for[4] = matches_for[4];
	} else {
	    tmp_ptr->matches_for[0] = 0;
	    tmp_ptr->matches_for[1] = 0;
	    tmp_ptr->matches_for[2] = 0;
	    tmp_ptr->matches_for[3] = 0;
	    tmp_ptr->matches_for[4] = 0;
	}
	tmp_ptr->behav = behav_list;
	tmp_ptr->param = param_list;
	tmp_ptr->oid = obj_id;

	if (!tmp_ptr->only_label) {
	    Redefinition ("attribute", tmp_ptr->label->string);
	}
	tmp_ptr->only_label = 0;
    }
    return tmp_ptr;
}

/*
 * see check_class_def()
 */

static gdmo_attr *check_attr_def(attr_label)
gdmo_label	*attr_label;
{
    return add_attr_def(attr_label, NULL, NULL, NULL, NULL, NULL, 1);
}

/*============================================================================*
 *
 * see add_class_def()
 */

static gdmo_group *add_group_def(group_label, elements, fixed, description,
				 obj_id, only_label)
gdmo_label	*group_label;
attr_ref	*elements;
int		fixed;
text_in_file	*description;
gdmo_oid	*obj_id;
int		only_label;
{
    gdmo_group *tmp_ptr;
 
    if (!group_def_list) { /* first definition */
	group_def_list = (gdmo_group *) ckalloc(sizeof(gdmo_group));

	group_def_list->label = group_label;
	group_def_list->only_label = only_label;
	group_def_list->inline_def = 0;
	group_def_list->attr = elements;
	group_def_list->fixed = fixed;
	group_def_list->description = description;
	group_def_list->oid = obj_id;
	group_def_list->next = NULL;

	return group_def_list;
    }
    tmp_ptr = group_def_list;
    while (tmp_ptr->next &&
	   strcmp(tmp_ptr->label->string, group_label->string)) {
        tmp_ptr = tmp_ptr->next;
    };
    if (!tmp_ptr->next &&
	strcmp(tmp_ptr->label->string, group_label->string)) {
        /* insert new entry at the end of group_def_list */
	tmp_ptr->next = (gdmo_group *) ckalloc(sizeof(gdmo_group));

	tmp_ptr = tmp_ptr->next;
	tmp_ptr->label = group_label;
	tmp_ptr->only_label = only_label;
	tmp_ptr->inline_def = 0;
	tmp_ptr->attr = elements;
	tmp_ptr->fixed = fixed;
	tmp_ptr->description = description;
	tmp_ptr->oid = obj_id;
	tmp_ptr->next = NULL;

	return tmp_ptr;
    }
    if (!only_label) {
	tmp_ptr->inline_def = 0;
	tmp_ptr->attr = elements;
	tmp_ptr->fixed = fixed;
	tmp_ptr->description = description;
	tmp_ptr->oid = obj_id;

	if (!tmp_ptr->only_label) {
	    Redefinition ("attribute group", tmp_ptr->label->string);
	}
	tmp_ptr->only_label = 0;
    }
    return tmp_ptr;
}

/*
 * see check_class_def()
 */

static gdmo_group *check_group_def(group_label)
gdmo_label	*group_label;
{
    return add_group_def(group_label, NULL, 0, NULL, NULL, 1);
}

/*============================================================================*
 *
 * see add_class_def()
 */

static gdmo_behav *add_behav_def(behav_label, definition, only_label)
gdmo_label	*behav_label;
text_in_file	*definition;
int		only_label;
{
    gdmo_behav *tmp_ptr;

    if (!behav_def_list) { /* first definition */
	behav_def_list = (gdmo_behav *) ckalloc(sizeof(gdmo_behav));

	behav_def_list->label = behav_label;
	behav_def_list->only_label = only_label;
	behav_def_list->inline_def = 0;
	behav_def_list->description = definition;
	behav_def_list->next = NULL;

	return behav_def_list;
    }
    tmp_ptr = behav_def_list;
    while (tmp_ptr->next &&
	   strcmp(tmp_ptr->label->string, behav_label->string)) {
        tmp_ptr = tmp_ptr->next;
    };
    if (!tmp_ptr->next &&
	strcmp(tmp_ptr->label->string, behav_label->string)) {
        /* insert new entry at the beginning of behav_def_list */
	tmp_ptr->next = (gdmo_behav *) ckalloc(sizeof(gdmo_behav));

	tmp_ptr = tmp_ptr->next;
	tmp_ptr->label = behav_label;
	tmp_ptr->only_label = only_label;
	tmp_ptr->inline_def = 0;
	tmp_ptr->description = definition;
	tmp_ptr->next = NULL;

	return tmp_ptr;
    }
    if (!only_label) {
	tmp_ptr->inline_def = 0;
	tmp_ptr->description = definition;

	if (!tmp_ptr->only_label) {
	    Redefinition ("behaviour", tmp_ptr->label->string);
	}
	tmp_ptr->only_label = 0;
    }
    return tmp_ptr;
}

/*
 * see check_class_def()
 */

static gdmo_behav *check_behav_def(behav_label)
gdmo_label	*behav_label;
{
    return add_behav_def(behav_label, NULL, 1);
}

/*============================================================================*
 *
 * see add_class_def()
 */

static gdmo_action *add_action_def(action_label, behav_list, confirmed,
				   param_list, info_syntax, reply_syntax,
				   obj_id, only_label)
gdmo_label	*action_label;
behav_ref	*behav_list;
int		confirmed;
param_ref	*param_list;
type_ref	*info_syntax;
type_ref	*reply_syntax;
gdmo_oid	*obj_id;
int		only_label;
{
    gdmo_action *tmp_ptr;

    if (!action_def_list) { /* first definition */
	action_def_list = (gdmo_action *) ckalloc(sizeof(gdmo_action));

	action_def_list->label = action_label;
	action_def_list->only_label = only_label;
	action_def_list->inline_def = 0;
	action_def_list->behav = behav_list;
	action_def_list->confirmed = confirmed;
	action_def_list->param = param_list;
	action_def_list->info_syntax = info_syntax;
	action_def_list->reply_syntax = reply_syntax;
	action_def_list->oid = obj_id;
	action_def_list->next = NULL;

	return action_def_list;
    }
    tmp_ptr = action_def_list;
    while (tmp_ptr->next &&
	   strcmp(tmp_ptr->label->string, action_label->string)) {
        tmp_ptr = tmp_ptr->next;
    };
    if (!tmp_ptr->next &&
	strcmp(tmp_ptr->label->string, action_label->string)) {
        /* insert new entry at the end of action_def_list */
	tmp_ptr->next = (gdmo_action *) ckalloc(sizeof(gdmo_action));

	tmp_ptr = tmp_ptr->next;
	tmp_ptr->label = action_label;
	tmp_ptr->only_label = only_label;
	tmp_ptr->inline_def = 0;
	tmp_ptr->behav = behav_list;
	tmp_ptr->confirmed = confirmed;
	tmp_ptr->param = param_list;
	tmp_ptr->info_syntax = info_syntax;
	tmp_ptr->reply_syntax = reply_syntax;
	tmp_ptr->oid = obj_id;
	tmp_ptr->next = NULL;

	return tmp_ptr;
    }
    if (!only_label) {
	tmp_ptr->inline_def = 0;
	tmp_ptr->behav = behav_list;
	tmp_ptr->confirmed = confirmed;
	tmp_ptr->param = param_list;
	tmp_ptr->info_syntax = info_syntax;
	tmp_ptr->reply_syntax = reply_syntax;
	tmp_ptr->oid = obj_id;

	if (!tmp_ptr->only_label) {
	    Redefinition ("action", tmp_ptr->label->string);
	}
	tmp_ptr->only_label = 0;
    }
    return tmp_ptr;
}

/*
 * see check_class_def()
 */

static gdmo_action *check_action_def(action_label)
gdmo_label	*action_label;
{
    return add_action_def(action_label, NULL, 0, NULL, NULL, NULL, NULL, 1);
}

/*============================================================================*
 *
 * see add_class_def()
 */

static gdmo_notif *add_notif_def(notif_label, behav_list, param_list,
				 syntax_attr_ids, reply_syntax, obj_id,
				 only_label)
gdmo_label	*notif_label;
behav_ref	*behav_list;
param_ref	*param_list;
notif_info_syntax *syntax_attr_ids;
type_ref	*reply_syntax;
gdmo_oid	*obj_id;
int		only_label;
{
    gdmo_notif *tmp_ptr;

    if (!notif_def_list) { /* first definition */
	notif_def_list = (gdmo_notif *) ckalloc(sizeof(gdmo_notif));

	notif_def_list->label = notif_label;
	notif_def_list->only_label = only_label;
	notif_def_list->inline_def = 0;
	notif_def_list->behav = behav_list;
	notif_def_list->param = param_list;
	notif_def_list->syntax_and_attr_ids = syntax_attr_ids;
	notif_def_list->reply_syntax = reply_syntax;
	notif_def_list->oid = obj_id;
	notif_def_list->next = NULL;

	return notif_def_list;
    }
    tmp_ptr = notif_def_list;
    while (tmp_ptr->next &&
	   strcmp(tmp_ptr->label->string, notif_label->string)) {
        tmp_ptr = tmp_ptr->next;
    };
    if (!tmp_ptr->next &&
	strcmp(tmp_ptr->label->string, notif_label->string)) {
        /* insert new entry at the end of notif_def_list */
	tmp_ptr->next = (gdmo_notif *) ckalloc(sizeof(gdmo_notif));

	tmp_ptr = tmp_ptr->next;
	tmp_ptr->label = notif_label;
	tmp_ptr->only_label = only_label;
	tmp_ptr->inline_def = 0;
	tmp_ptr->behav = behav_list;
	tmp_ptr->param = param_list;
	tmp_ptr->syntax_and_attr_ids = syntax_attr_ids;
	tmp_ptr->reply_syntax = reply_syntax;
	tmp_ptr->oid = obj_id;
	tmp_ptr->next = NULL;

	return tmp_ptr;
    }
    if (!only_label) {
	tmp_ptr->inline_def = 0;
	tmp_ptr->behav = behav_list;
	tmp_ptr->param = param_list;
	tmp_ptr->syntax_and_attr_ids = syntax_attr_ids;
	tmp_ptr->reply_syntax = reply_syntax;
	tmp_ptr->oid = obj_id;

	if (!tmp_ptr->only_label) {
	    Redefinition ("notification", tmp_ptr->label->string);
	}
	tmp_ptr->only_label = 0;
    }
    return tmp_ptr;
}

/*
 * see check_class_def()
 */

static gdmo_notif *check_notif_def(notif_label)
gdmo_label	*notif_label;
{
    return add_notif_def(notif_label, NULL, NULL, NULL, NULL, NULL, 1);
}

static class_ref *add_class_list(class)
gdmo_class *class;
{
    class_ref *tmp_ptr;
    tmp_ptr = (class_ref *) ckalloc(sizeof(class_ref));

    tmp_ptr->def = class;
    tmp_ptr->next = NULL;

    return tmp_ptr;
}

static class_pckg *add_class_pckg(pckg, condition)
gdmo_pckg *pckg;
text_in_file *condition;
{
    class_pckg *tmp_ptr;
    tmp_ptr = (class_pckg *) ckalloc(sizeof(class_pckg));

    tmp_ptr->def = pckg;
    tmp_ptr->cond = condition;
    tmp_ptr->next = NULL;

    return tmp_ptr;
}

static behav_ref *add_behav_list(behav)
gdmo_behav *behav;
{
    behav_ref *tmp_ptr;
    tmp_ptr = (behav_ref *) ckalloc(sizeof(behav_ref));

    tmp_ptr->def = behav;
    tmp_ptr->next = NULL;

    return tmp_ptr;
}

static param_ref *add_param_list(param)
gdmo_param	*param;
{
    param_ref *tmp_ptr;
    tmp_ptr = (param_ref *) ckalloc(sizeof(param_ref));

    tmp_ptr->def = param;
    tmp_ptr->next = NULL;

    return tmp_ptr;
}

static pckg_group *add_pckg_group(group, attr_list)
gdmo_group	*group;
attr_ref	*attr_list;
{
    pckg_group *tmp_ptr;
    tmp_ptr = (pckg_group *) ckalloc(sizeof(pckg_group));

    tmp_ptr->def = group;
    tmp_ptr->attr = attr_list;
    tmp_ptr->next = NULL;

    return tmp_ptr;
}

static attr_ref *add_attr_list(attr)
gdmo_attr	*attr;
{
    attr_ref *tmp_ptr;
    tmp_ptr = (attr_ref *) ckalloc(sizeof(attr_ref));

    tmp_ptr->def = attr;
    tmp_ptr->next = NULL;

    return tmp_ptr;
}

static pckg_action *add_pckg_action(action, param_list)
gdmo_action	*action;
param_ref	*param_list;
{
    pckg_action *tmp_ptr;
    tmp_ptr = (pckg_action *) ckalloc(sizeof(pckg_action));

    tmp_ptr->def = action;
    tmp_ptr->param = param_list;
    tmp_ptr->next = NULL;

    return tmp_ptr;
}

static pckg_notif *add_pckg_notif(notif, param_list)
gdmo_notif	*notif;
param_ref	*param_list;
{
    pckg_notif *tmp_ptr;
    tmp_ptr = (pckg_notif *) ckalloc(sizeof(pckg_notif));

    tmp_ptr->def = notif;
    tmp_ptr->param = param_list;
    tmp_ptr->next = NULL;

    return tmp_ptr;
}

static pckg_attr *add_pckg_attr(attr, repl_default, default_val, initial_val,
				permitted_vals, required_vals,
				get_repl, add_remv, param_list)
gdmo_attr	*attr;
int		repl_default;
pckg_val_specifier *default_val, *initial_val;
type_ref	*permitted_vals, *required_vals;
int		get_repl, add_remv;
param_ref	*param_list;
{
    int i;
    pckg_attr *tmp_ptr;
    tmp_ptr = (pckg_attr *) ckalloc(sizeof(pckg_attr));

    tmp_ptr->def = attr;

    for (i=0; i<7; i++)	tmp_ptr->property[i] = 0;

    tmp_ptr->property[t_replace_with_default] = repl_default;
    tmp_ptr->default_val = default_val;
    if (default_val && default_val->ref) {
	tmp_ptr->property[t_default_value] = 1;
    } else if (default_val && default_val->behav) {
	tmp_ptr->property[t_default_value] = 2;
    }
    tmp_ptr->initial_val = initial_val;
    if (initial_val && initial_val->ref) {
	tmp_ptr->property[t_initial_value] = 1;
    } else if (initial_val && initial_val->behav) {
	tmp_ptr->property[t_initial_value] = 2;
    }
    if (permitted_vals) {
	tmp_ptr->property[t_permitted_values] = 1;
	tmp_ptr->permitted_values= permitted_vals;
    }
    if (required_vals) {
	tmp_ptr->property[t_required_values] = 1;
	tmp_ptr->required_values= required_vals;
    }
    tmp_ptr->property[t_get_replace] = get_repl;
    tmp_ptr->property[t_add_remove] = add_remv;

    tmp_ptr->param = param_list;
    tmp_ptr->next = NULL;

    return tmp_ptr;

}

static param_context *CreateContext(keyword, context)
type_ref		*keyword;
enum contextToken context;
{
    param_context *tmp_ptr;
    tmp_ptr = (param_context *) ckalloc(sizeof(param_context));

    tmp_ptr->keyword = keyword;
    tmp_ptr->type = context;

    return tmp_ptr;
}

/* static syntax_or_attr *create_syntax_or_attr(syntax, attr)
 * type_ref	*syntax;
 * gdmo_attr	*attr;
 * {
 *    syntax_or_attr *tmp_ptr;
 *    tmp_ptr = (syntax_or_attr *) ckalloc(sizeof(syntax_or_attr));
 * 
 *    tmp_ptr->syntax = syntax;
 *    tmp_ptr->attr = attr;
 * 
 *    return tmp_ptr;
 * }
 */

/* static namebind_class *create_namebind_class(class, and_subs)
 * gdmo_class *class;
 * int	and_subs;
 * {
 *    namebind_class *tmp_ptr;
 *    tmp_ptr = (namebind_class *) ckalloc(sizeof(namebind_class));
 * 
 *    tmp_ptr->def = class;
 *    tmp_ptr->and_subs = and_subs;
 * 
 *    return tmp_ptr;
 * }
 */

/* static namebind_create *create_namebind_create(modifier, param_list)
 * enum createModifier modifier;
 * param_ref	*param_list;
 * {
 *     namebind_create *tmp_ptr;
 *     tmp_ptr = (namebind_create *) ckalloc(sizeof(namebind_create));
 * 
 *    tmp_ptr->modifier = modifier;
 *    tmp_ptr->param = param_list;
 * 
 *    return tmp_ptr;
 * }
 */

/* static namebind_delete *create_namebind_delete(modifier, param_list)
 * enum deleteModifier modifier;
 * param_ref	*param_list;
 * {
 *     namebind_delete *tmp_ptr;
 *     tmp_ptr = (namebind_delete *) ckalloc(sizeof(namebind_delete));
 * 
 *     tmp_ptr->modifier = modifier;
 *     tmp_ptr->param = param_list;
 * 
 *     return tmp_ptr;
 * }
 */

/* static notif_info_syntax *create_notif_info_syntax(syntax, attrids)
 * type_ref	*syntax;
 * notif_attr_ids	*attrids;
 * {
 *     notif_info_syntax *tmp_ptr;
 *     tmp_ptr = (notif_info_syntax *) ckalloc(sizeof(notif_info_syntax));
 * 
 *     tmp_ptr->syntax = syntax;
 *     tmp_ptr->attrids = attrids;
 * 
 *     return tmp_ptr;
 * }
 */

/* static notif_attr_ids	*add_notif_attr_ids(field, attr)
 * notif_field_name *field;
 * gdmo_attr	*attr;
 * {
 *     notif_attr_ids *tmp_ptr;
 * 
 *     tmp_ptr = (notif_attr_ids *) ckalloc(sizeof(notif_attr_ids));
 *     tmp_ptr->field_name = field;
 *     tmp_ptr->def = attr;
 *     tmp_ptr->next = NULL;
 * 
 *     return tmp_ptr;
 * }
 */

%}

%union {
/* return values from flex: */
    text_in_file *text_in_file_ptr;
    char	*string_val;

/* return values from bison-rules: */
    int		val;
    int		*val_ptr; /* XXX be careful!! maybe allocation problems!! */
    gdmo_label	*template_label_ptr;
    gdmo_class	*class_def_ptr;
    gdmo_pckg	*pckg_def_ptr;
    gdmo_param	*param_def_ptr;
    gdmo_namebind *namebind_def_ptr;
    gdmo_attr	*attr_def_ptr;
    gdmo_group	*group_def_ptr;
    gdmo_behav	*behav_def_ptr;
    gdmo_action	*action_def_ptr;
    gdmo_notif	*notif_def_ptr;
    class_ref	*class_list_ptr;
    class_pckg	*class_pckg_ptr;
    behav_ref	*behav_list_ptr;
    pckg_attr	*pckg_attr_ptr;
    param_ref	*param_list_ptr;
    pckg_property *pckg_property_ptr;
    pckg_val_specifier	*pckg_val_specifier_ptr;
    type_ref	*type_ref_ptr;
    pckg_group	*pckg_group_ptr;
    attr_ref	*attr_list_ptr;
    pckg_action	*pckg_action_ptr;
    pckg_notif	*pckg_notif_ptr;
    gdmo_oid	*oid_ptr;
    param_context *param_context_ptr;
    syntax_or_attr *syntax_or_attr_ptr;
    namebind_class *namebind_class_ptr;
    namebind_create	*namebind_create_ptr;
/*     enum createModifer createModify;
 *     enum deleteModifer deleteModify;
 */
    int		createModify;
    int		deleteModify;
    namebind_delete	*namebind_delete_ptr;
    notif_attr_ids	*notif_attr_ids_ptr;
    notif_info_syntax *notif_info_syntax_ptr;
}

/* TOKENS */

/* ';' ':' ',' and all single characters are allready token!!
 * %token T_SEMICOLON
 * %token T_COLON
 * %token T_COMMA
 */

/* see lexgdmo.l
 * %token T_LIBCOMMENT
 * %token T_LIBAUTHOR
 */
%token T_LIBCOMMENT

%token T_PARAMETERS
%token T_WITH
/* see lexgdmo.l
 * %token T_WITHi	* to avoid shift/reduce conflicts *
 * %token T_WITHr	* there is a better solution, but with more rules *
 */

%token T_MANAGED
%token T_OBJECT
%token T_CLASS
%token T_DERIVED
%token T_FROM
%token T_CHARACTERIZED
%token T_BY
%token T_CONDITIONAL
%token T_PACKAGES
%token T_PRESENT
%token T_IF
%token T_REGISTERED
%token T_AS

%token T_PACKAGE
%token T_BEHAVIOUR
%token T_ATTRIBUTES
%token T_ATTRIBUTE
%token T_GROUPS
%token T_ACTIONS
%token T_NOTIFICATIONS
%token T_REPLACE_WITH_DEFAULT
%token T_DEFAULT
%token T_VALUE
%token T_INITIAL
%token T_PERMITTED
%token T_VALUES
%token T_REQUIRED
%token T_DERIVATION
%token T_RULE
%token T_GET
%token T_REPLACE
%token T_GET_REPLACE
%token T_ADD
%token T_REMOVE
%token T_ADD_REMOVE

%token T_PARAMETER
%token T_CONTEXT
%token T_ACTION_INFO
%token T_ACTION_REPLY
%token T_EVENT_INFO
%token T_EVENT_REPLY
%token T_SPECIFIC_ERROR
/* see lexgdmo.l
 * %token T_COMMON_ERROR
 */
%token T_SYNTAX

%token T_NAME
%token T_BINDING
%token T_SUBORDINATE
%token T_AND
%token T_SUBCLASSES
%token T_NAMED
%token T_SUPERIOR
%token T_CREATE
%token T_DELETE
%token T_WITH_REFERENCE_OBJECT
%token T_WITH_AUTOMATIC_INSTANCE_NAMING
%token T_ONLY_IF_NO_CONTAINED_OBJECTS
%token T_DELETES_CONTAINED_OBJECTS

%token T_MATCHES
%token T_FOR
%token T_EQUALITY
%token T_ORDERING
%token T_SUBSTRINGS
%token T_SET_COMPARISON
%token T_SET_INTERSECTION

%token T_GROUP
%token T_ELEMENTS
%token T_FIXED
%token T_DESCRIPTION

%token T_DEFINED

%token T_ACTION
%token T_MODE
%token T_CONFIRMED

%token T_INFORMATION
%token T_REPLY

%token T_NOTIFICATION
/* see lexgdmo.l
 * %token T_NON_CONFIRMED
 */
%token T_NON_CONFIRMED
%token T_IDS

/* XXX maybe allocation problems with text_in_file_ptr!! */
%token <text_in_file_ptr> T_DELIMITED_STRING
%token <string_val> T_OBJ_ID_PART T_TYPE_REFERENCE T_TYPE_REFERENCE_ID
%token <string_val> T_VALUE_REFERENCE T_TEMPLATE_LABEL
/* T_FIELD_NAME */

%type <class_def_ptr> managed_object_class class_label
%type <pckg_def_ptr> package package_label
%type <param_def_ptr> parameter parameter_label
%type <namebind_def_ptr> name_binding
%type <attr_def_ptr> attribute attribute_label with_attribute
%type <group_def_ptr> attribute_group group_label
%type <behav_def_ptr> behaviour_definition behaviour_def_label
%type <action_def_ptr> action action_label
%type <notif_def_ptr> notification notification_label
%type <class_list_ptr> derived_from class_label_list
%type <class_pckg_ptr> characterized_by package_label_list
%type <class_pckg_ptr> conditional_packages cond_package_list
%type <text_in_file_ptr> condition_definition description delimited_string
%type <behav_list_ptr> behaviour behaviour_def_label_list
%type <pckg_attr_ptr> pckg_attrs pckg_attr_list
%type <param_list_ptr> parameter_label_s parameter_s parameter_label_list
/* %type <pckg_property_ptr> propertylist */
%type <val> replace_with_default get_replace add_remove qualifier
%type <val> mode_confirmed fixed
%type <createModify> create_modifier
%type <deleteModify> delete_modifier
%type <pckg_val_specifier_ptr> default_value initial_value value_specifier
%type <type_ref_ptr> permitted_values required_values context_keyword
%type <type_ref_ptr> with_information_syntax with_reply_syntax
%type <pckg_group_ptr> attribute_group_s attribute_groups_list
%type <attr_list_ptr> attribute_label_s group_elements attribute_label_list
%type <pckg_action_ptr> action_s actions_list
%type <pckg_notif_ptr> notification_s notifications_list
%type <oid_ptr> registered_as object_identifier obj_id_parts
%type <param_context_ptr> context context_type
%type <syntax_or_attr_ptr> syntax_or_attribute_choice
%type <syntax_or_attr_ptr> derived_or_with_syntax_choice
%type <namebind_class_ptr> subordinate_class superior_class class_subclasses
%type <namebind_create_ptr> create
%type <namebind_delete_ptr> delete
%type <val_ptr> matches_for qualifier_list
%type <string_val> type_reference field_name
%type <template_label_ptr> template_label
%type <notif_attr_ids_ptr> field_and_attribute_s and_attribute_ids
%type <notif_info_syntax_ptr> with_info_syntax_and_attr_ids
%type <text_in_file_ptr> libcomment

%% /* GRAMMAR RULES */

template_list:
	template	   /* XXX eventuell auch empty!? */
      | template_list template
;

template:
	managed_object_class	{ ; }
      | package			{ ; }
      | parameter		{ ; }
      | name_binding		{ ; }
      | attribute		{ ; }
      | attribute_group		{ ; }
      | behaviour_definition	{ ; }
      | action			{ ; }
      | notification		{ ; }
      | template_label error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal TEMPLATE-NAME \"%s\" in template \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till next end of construct or template */
	{ reentered = 0; }
      | error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf, "no valid template-start \"%s\"", yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till next end of construct or template */
	{ reentered = 0; }

/* funktioniert leider nicht ganz, da gar nicht versucht wird bis zum
 * ';' zu scannen:
 *      | template_label error
 *	  {
 *	      if (!reentered) {
 *		  yyerror("missing \";\", probably no GDMO-file!");
 *	      }
 *	      ++reentered;
 *	  }
 */
/*
 *      | error
 *	  {
 *	      if (!reentered) {
 *		  yyerror("missing \";\", probably no GDMO-file!");
 *	      }
 *	      ++reentered;
 *	  }
 */
;

/*===================  MANAGED OBJECT CLASS TEMPLATE  =====================*/
managed_object_class:
	template_label T_MANAGED T_OBJECT T_CLASS
	    libcomment
	    derived_from
	    characterized_by
	    conditional_packages
	T_REGISTERED T_AS object_identifier	';'
	{ $$ = add_class_def($1, $6, $7, $8, $11, 0); }
      | template_label T_MANAGED T_OBJECT T_CLASS
	error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal CONSTRUCT-NAME \"%s\" in \
MANAGED OBJECT CLASS \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till end of construct or template */
	{
	    reentered = 0;
	    $$ = check_class_def($1); /* create a dummy entry */
	}
;

libcomment:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_LIBCOMMENT delimited_string ';'
	{ $$ = NULL; /* cause there is no use for it */ }
;

derived_from:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_DERIVED T_FROM class_label_list ';'
	{ $$ = $3; }
      | T_DERIVED T_FROM error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal class-label \"%s\" at DERIVED FROM ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

class_label_list:
	class_label
	{ $$ = add_class_list($1); }
      | class_label_list ','
	class_label
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next; 
	    $1->next = add_class_list($3);
	}
/*      | class_label_list ',' error
 *	{ * yyerrok; * }
 */
;

class_label:
	template_label
	{ $$ = check_class_def($1); }
      | managed_object_class
	{ $$ = $1; $$->inline_def = 1; }
;

characterized_by:
	/* empty cause it's optional */
	{ $$ = NULL; } /* oder so "ahnlich */
      | T_CHARACTERIZED T_BY package_label_list ';'
	{ $$ = $3; }
      | T_CHARACTERIZED T_BY error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal package-label \"%s\" at CHARACTERIZED BY ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

package_label_list:
	package_label
	{ $$ = add_class_pckg($1, NULL); }
      | package_label_list ','
	package_label
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next; 
	    $1->next = add_class_pckg($3, NULL);
	}
;

package_label:
	template_label
	{ $$ = check_pckg_def($1); }
      | package
	{ $$ = $1; $$->inline_def = 1; }
;

conditional_packages:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_CONDITIONAL T_PACKAGES cond_package_list ';'
	{ $$ = $3; }
      | T_CONDITIONAL T_PACKAGES error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal package-label \"%s\" at CONDITIONAL PACKAGES \
... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

cond_package_list: /* XXX maybe false the error recovery */
	package_label T_PRESENT T_IF condition_definition
	{ $$ = add_class_pckg($1, $4); }
      | package_label error
	{
	    yyerror("probably missing PRESENT IF in \
CONDITIONAL PACKAGES ... ;");
	    $$ = NULL;
	}
      | cond_package_list ','
	package_label T_PRESENT T_IF condition_definition
	{
	    if (!$1) {
		$$ = add_class_pckg($3, $6);
	    } else {
		$$ = $1;
		while ($1->next)	$1 = $1->next; 
		$1->next = add_class_pckg($3, $6);
	    }
	}
      | cond_package_list ',' package_label error
	{
	    yyerror("probably missing PRESENT IF in \
CONDITIONAL PACKAGES ... ;");
	    $$ = $1;
	}
;

condition_definition:
/* maybe this definition will be altered in the future */
	T_DELIMITED_STRING
	{
	    $$ = (text_in_file *) ckalloc(sizeof(text_in_file));
	    $$->path = $1->path;
	    $$->pos = $1->pos;
	    $$->len = $1->len;
	}
      | error
	{
	    char *buf;

	    buf = ckalloc(strlen(yytext) + 80);
	    sprintf(buf, "\"%s\" probably illegal delimited-string for \
condition-definition of PRESENT IF",
		    yytext);
	    yyerror(buf);
	    ckfree(buf);
	    $$ = NULL;
	}
;

object_identifier:
	'{' obj_id_parts '}'
	{ $$ = $2; }
      | '{' error '}'
	{
	    yyerror("illegal object-identifier");
	    $$ = NULL;
	}
/*
 *	| error
 *	  {
 *	      yyerror("illegal object-identifier");
 *	      $$ = NULL;
 *	  }
 */
;

obj_id_parts:
	T_OBJ_ID_PART
	{
	    $$ = (gdmo_oid *) ckalloc(sizeof(gdmo_oid));
	    $$->part = ckstrdup($1);
	    $$->next = NULL;
	}
      | obj_id_parts T_OBJ_ID_PART
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next; 
	    $1->next = (gdmo_oid *) ckalloc(sizeof(gdmo_oid));
	    $1 = $1->next;
	    $1->part = ckstrdup($2);
	    $1->next = NULL;
	}
;

package:
/*===========================  PACKAGE TEMPLATE  ============================*/
	template_label T_PACKAGE
	    libcomment
	behaviour
	pckg_attrs
	attribute_group_s
	action_s
	notification_s
	registered_as
	';'
	{ $$ = add_pckg_def($1, $4, $5, $6, $7, $8, $9, 0); }
      |	template_label T_PACKAGE
	error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal CONSTRUCT-NAME \"%s\" in PACKAGE \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till end of construct or template */
	{
	    reentered = 0;
	    $$ = check_pckg_def($1); /* create a dummy entry */
	}
;

behaviour:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_BEHAVIOUR behaviour_def_label_list ';'
	{ $$ = $2; }
      | T_BEHAVIOUR error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal behaviour-definition-label \"%s\" at \
BEHAVIOUR ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

behaviour_def_label_list:
	behaviour_def_label
	{ $$ = add_behav_list($1); }
      | behaviour_def_label_list ',' behaviour_def_label
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next;
	    $1->next = add_behav_list($3);
	}
;

behaviour_def_label:
	template_label
	{ $$ = check_behav_def($1); }
      | behaviour_definition
	{ $$ = $1; $$->inline_def = 1; }
;

pckg_attrs:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_ATTRIBUTES pckg_attr_list ';'
	{ $$ = $2; }
      | T_ATTRIBUTES error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal package-attribute \"%s\" at ATTRIBUTES ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

pckg_attr_list:
	attribute_label
	replace_with_default
	default_value
	initial_value
	permitted_values
	required_values
	get_replace
	add_remove
	parameter_label_s
	{ $$ = add_pckg_attr($1, $2, $3, $4, $5, $6, $7, $8, $9); }
      | pckg_attr_list ','
	attribute_label
 		replace_with_default
	default_value
	initial_value
	permitted_values
	required_values
	get_replace
	add_remove
	parameter_label_s
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next;
	    $1->next = add_pckg_attr($3, $4, $5, $6, $7, $8, $9,
				     $10, $11);
	}
;

attribute_label:
	template_label
	{ $$ = check_attr_def($1); }
      | attribute
	{ $$ = $1; $$->inline_def = 1; }
;

parameter_label_s:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | parameter_label_s parameter_label
	{
	    if (!$1) {
		$$ = add_param_list($2);
	    } else {
		$$ = $1;
		while ($1->next)	$1 = $1->next;
		$1->next = add_param_list($2);
	    }
	}
;

parameter_label:
	template_label
	{ $$ = check_param_def($1); }
      | parameter
	{ $$ = $1; $$->inline_def = 1; }
;

replace_with_default:
	/* empty cause it's optional */
	{ $$ = 0; /* false */}
      | T_REPLACE_WITH_DEFAULT
	{ $$ = 1; /* true */}
;

default_value:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_DEFAULT T_VALUE value_specifier
	{ $$ = $3; }
      | T_DEFAULT T_VALUE error
	{
	    yyerror("illegal value-specifier at DEFAULT VALUE");
	    $$ = NULL;
	}
;

initial_value:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_INITIAL T_VALUE value_specifier
	{ $$ = $3; }
      | T_INITIAL T_VALUE error
	{
	    yyerror("illegal value-specifier at INITIAL VALUE");
	    $$ = NULL;
	}
;

value_specifier:
	T_VALUE_REFERENCE
	{
	    $$ = (pckg_val_specifier *) ckalloc(sizeof(pckg_val_specifier));
	    $$->ref = ckstrdup($1);
	    $$->behav = NULL;
	/* $$ = create_pckg_val_specifier($1, NULL); */
	}
      | T_DERIVATION T_RULE behaviour_def_label
	{
	    $$ = (pckg_val_specifier *)
		 ckalloc(sizeof(pckg_val_specifier));
	    $$->ref = NULL;
	    $$->behav = $3;
	/* $$ = create_pckg_val_specifier(NULL, $3); */
	}
;

permitted_values:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_PERMITTED T_VALUES type_reference
	{ $$ = $3; }
      | T_PERMITTED T_VALUES error
	{
	    yyerror("illegal type-reference at PERMITTED VALUES");
	    $$ = NULL;
	}
;

required_values:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_REQUIRED T_VALUES type_reference
	{ $$ = $3; }
      | T_REQUIRED T_VALUES error
	{
	    yyerror("illegal type-reference at REQUIRED VALUES");
	    $$ = NULL;
	}
;

get_replace:
	/* empty cause it's optional */
	{ $$ = 0; /* no get, replace, or get-replace */ }
      | T_GET
	{ $$ = 1; }
      | T_REPLACE
	{ $$ = 2; }
      | T_GET_REPLACE
	{ $$ = 3; }
;

add_remove:
	/* empty cause it's optional */
	{ $$ = 0; /* no add, remove, or add-remove */ }
      | T_ADD
	{ $$ = 1; }
      | T_REMOVE
	{ $$ = 2; }
      | T_ADD_REMOVE
	{ $$ = 3; }
;

attribute_group_s:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_ATTRIBUTE T_GROUPS attribute_groups_list ';'
	{ $$ = $3; }
      | T_ATTRIBUTE T_GROUPS error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal attribute-group \"%s\" at ATTRIBUTE GROUPS \
... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

attribute_groups_list:
	group_label attribute_label_s
	{ $$ = add_pckg_group($1, $2);	}
      | attribute_groups_list ','
	group_label attribute_label_s
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next;
	    $1->next = add_pckg_group($3, $4);
	}
;

group_label:
	template_label
	{ $$ = check_group_def($1); }
      | attribute_group
	{ $$ = $1; $$->inline_def = 1; }
;

attribute_label_s:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | attribute_label_s attribute_label
	{
	    if (!$1) {
		$$ = add_attr_list($2);
	    } else {
		$$ = $1;
		while ($1->next)	$1 = $1->next;
		$1->next = add_attr_list($2);
	    }
	}
;

action_s:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_ACTIONS actions_list ';'
	{ $$ = $2; }
      | T_ACTIONS error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal action \"%s\" at ACTIONS ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

actions_list:
	action_label parameter_label_s
	{ $$ = add_pckg_action($1, $2); }
      | actions_list ','
	action_label parameter_label_s
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next;
	    $1->next = add_pckg_action($3, $4);
	}
;

action_label:
	template_label
	{ $$ = check_action_def($1); }
      | action
	{ $$ = $1; $$->inline_def = 1; }
;

notification_s:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_NOTIFICATIONS notifications_list ';'
	{ $$ = $2; }
      | T_NOTIFICATIONS error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal notification \"%s\" at NOTIFICATIONS ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

notifications_list:
	notification_label parameter_label_s
	{ $$ = add_pckg_notif($1, $2);	}
      | notifications_list ','
	notification_label parameter_label_s
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next;
	    $1->next = add_pckg_notif($3, $4);
	}
;

notification_label:
	template_label
	{ $$ = check_notif_def($1); }
      | notification
	{ $$ = $1; $$->inline_def = 1; }
;

registered_as:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_REGISTERED T_AS object_identifier
	{ $$ = $3; }
/* geschieht schon in object_identifier
 *	      | T_REGISTERED T_AS error
 *		  {
 *		      yyerror("illegal object_identifier at REGISTERED AS");
 *		      $$ = NULL;
 *		  }
 */
;

parameter:
/*==========================  PARAMETER TEMPLATE  ===========================*/
	template_label T_PARAMETER
	    libcomment
	context
	syntax_or_attribute_choice
	behaviour
	registered_as
	';'
	{ $$ = add_param_def($1, $4, $5, $6, $7, 0); }
      |	template_label T_PARAMETER
	error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal CONSTRUCT-NAME \"%s\" in PARAMETER \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till end of construct or template */
	{
	    reentered = 0;
	    $$ = check_param_def($1); /* create a dummy entry */
	}
;

context:
	T_CONTEXT context_type ';'
	{ $$ = $2; }
      | T_CONTEXT error
	{	/* XXX vorsicht mit return wert $$, da nicht optional */
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal context-type \"%s\" at CONTEXT ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

context_type:
	context_keyword
	{ $$ = CreateContext($1, no_token); }
      | T_ACTION_INFO
	{ $$ = CreateContext(NULL, t_action_info); }
      | T_ACTION_REPLY
	{ $$ = CreateContext(NULL, t_action_reply); }
      | T_EVENT_INFO
	{ $$ = CreateContext(NULL, t_event_info); }
      | T_EVENT_REPLY
	{ $$ = CreateContext(NULL, t_event_reply); }
      | T_SPECIFIC_ERROR
	{ $$ = CreateContext(NULL, t_specific_error); }
;

context_keyword:
/*		type_reference T_POINT identifier */
	T_TYPE_REFERENCE_ID
	{ $$ = ckstrdup($1); }
;

syntax_or_attribute_choice:
	T_WITH T_SYNTAX type_reference ';'
	{
	    $$ = (syntax_or_attr *) ckalloc(sizeof(syntax_or_attr));
	    $$->syntax = $3;
	    $$->attr = NULL;
	/* $$ = Createsyntax_or_attr($3, NULL); */
	}
      | T_WITH T_SYNTAX error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal type-reference \"%s\" at WITH SYNTAX ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
      | T_ATTRIBUTE attribute_label ';'
	{
	    $$ = (syntax_or_attr *) ckalloc(sizeof(syntax_or_attr));
	    $$->syntax = NULL;
	    $$->attr = $2;
	/* $$ = Createsyntax_or_attr(NULL, $2); */
	}
      | T_ATTRIBUTE error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal attribute-label \"%s\" at ATTRIBUTE ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

name_binding:
/*========================  NAME BINDING TEMPLATE  ==========================*/
	template_label T_NAME T_BINDING
	    libcomment
	subordinate_class
	superior_class
	with_attribute
	behaviour
	create
	delete
	T_REGISTERED T_AS object_identifier
	';'
	{
	    $$ = add_namebind_def($1, $5, $6, $7, $8, $9, $10, $13, 0);
	}
      | template_label T_NAME T_BINDING
	error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal CONSTRUCT-NAME \"%s\" in \
NAME BINDING \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till end of construct or template */
	{
	    reentered = 0;
	    $$ = check_namebind_def($1); /* create a dummy entry */
	}
;

subordinate_class:
	T_SUBORDINATE T_OBJECT T_CLASS class_subclasses ';'
	{ $$ = $4; }
      | T_SUBORDINATE T_OBJECT T_CLASS error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal class-label \"%s\" at SUBORDINATE OBJECT \
CLASS ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

superior_class:
	T_NAMED T_BY T_SUPERIOR T_OBJECT T_CLASS class_subclasses ';'
	{ $$ = $6; }
      | T_NAMED T_BY T_SUPERIOR T_OBJECT T_CLASS error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal class-label \"%s\" at NAMED BY SUPERIOR \
OBJECT CLASS ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

with_attribute:
	T_WITH T_ATTRIBUTE attribute_label ';'
	{ $$ = $3; }
      | T_WITH T_ATTRIBUTE error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal attribute-label \"%s\" at WITH ATTRIBUTE \
... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

class_subclasses:
	class_label
	{
	    $$ = (namebind_class *) ckalloc(sizeof(namebind_class));
	    $$->def = $1;
	    $$->and_subs = 0;
	/* $$ = Createnamebind_class($1, 0); */
	}
      | class_label T_AND T_SUBCLASSES
	{
	    $$ = (namebind_class *) ckalloc(sizeof(namebind_class));
	    $$->def = $1;
	    $$->and_subs = 1;
	/* $$ = Createnamebind_class($1, 1); */
	}
;

create:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_CREATE parameter_label_s ';'
	{
	    $$ = (namebind_create *) ckalloc(sizeof(namebind_create));
	    $$->modifier = no_create_modifier;
	    $$->param = $2;
	/* $$ = Createnamebind_create(no_create_modifier, $2); */
	}
      | T_CREATE create_modifier parameter_label_s ';'
	{
	    $$ = (namebind_create *) ckalloc(sizeof(namebind_create));
	    $$->modifier = $2;
	    $$->param = $3;
	/* $$ = Createnamebind_create($2, $3); */
	}
      | T_CREATE create_modifier ',' create_modifier
	parameter_label_s ';'
	{
	    if ($2 == $4) {
		$$ = (namebind_create *)
		     ckalloc(sizeof(namebind_create));
		$$->modifier = $2;
		$$->param = $5;
		/* $$ = Createnamebind_create($2, $5); */
	    } else {
		$$ = (namebind_create *)
		     ckalloc(sizeof(namebind_create));
		$$->modifier = both_create_modifier;
		$$->param = $5;
		/* $$ = Createnamebind_create(both, $5); */
	    }
	}
      | T_CREATE error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal create-modifier or parameter-label \"%s\" at \
CREATE ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

create_modifier:
	T_WITH_REFERENCE_OBJECT
	{ $$ = with_reference_object; }
      | T_WITH_AUTOMATIC_INSTANCE_NAMING
	{ $$ = with_automatic_instance_naming; }
;

delete:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_DELETE parameter_label_s ';'
	{
	    $$ = (namebind_delete *) ckalloc(sizeof(namebind_delete));
	    $$->modifier = no_delete_modifier;
	    $$->param = $2;
	/* $$ = Createnamebind_delete(no_delete_modifier, $2); */
	}
      | T_DELETE delete_modifier parameter_label_s ';'
	{
	    $$ = (namebind_delete *) ckalloc(sizeof(namebind_delete));
	    $$->modifier = $2;
	    $$->param = $3;
	/* $$ = Createnamebind_delete($2, $3); */
	}
      | T_DELETE error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal delete-modifier or parameter-label \"%s\" at \
DELETE ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

delete_modifier:
	T_ONLY_IF_NO_CONTAINED_OBJECTS
	{ $$ = only_if_no_contained_objects; }
      | T_DELETES_CONTAINED_OBJECTS
	{ $$ = deletes_contained_objects; }
;

attribute:
/*===========================  ATTRIBUTE LABEL  =============================*/
	template_label T_ATTRIBUTE
	    libcomment
	derived_or_with_syntax_choice
	matches_for
	behaviour
	parameter_s
	registered_as
	';'
	{ $$ = add_attr_def($1, $4, $5, $6, $7, $8, 0); }
      | template_label T_ATTRIBUTE
	error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal CONSTRUCT-NAME \"%s\" in ATTRIBUTE \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till end of construct or template */
	{
	    reentered = 0;
	    $$ = check_attr_def($1); /* create a dummy entry */
	}
;

derived_or_with_syntax_choice:
	T_DERIVED T_FROM attribute_label ';'
	{
	    $$ = (syntax_or_attr *) ckalloc(sizeof(syntax_or_attr));
	    $$->attr = $3;
	    $$->syntax = NULL;
	/* $$ = Createsyntax_or_attr($3, NULL); */
	}
      | T_DERIVED T_FROM error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal attribute-label \"%s\" at DERIVED FROM ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
      | T_WITH T_ATTRIBUTE T_SYNTAX type_reference ';'
	{
	    $$ = (syntax_or_attr *) ckalloc(sizeof(syntax_or_attr));
	    $$->attr = NULL;
	    $$->syntax = $4;
	/* $$ = Createsyntax_or_attr(NULL, $4); */
	}
      | T_WITH T_ATTRIBUTE T_SYNTAX error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal type-reference \"%s\" at WITH ATTRIBUTE \
SYNTAX ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

matches_for:
	/* empty cause it's optional */
	{
  /*		    int i; * dummy[] = { 0, 0, 0, 0, 0 }; *
   *		    for (i=0; i<5; i++);
   *			$$[i] = 0;
   */
  /*		    int tmp[] = { 0, 0, 0, 0, 0 };
   *		    $$ = tmp;
   */
	    /* is there a better way? */

	    $$ = NULL;
	};
      | T_MATCHES T_FOR qualifier_list ';'
	{ $$ = $3; }
      | T_MATCHES T_FOR error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal qualifier \"%s\" at MATCHES FOR ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

qualifier_list:
	qualifier
	{
  /*		    int tmp[] = { 0, 0, 0, 0, 0 };
   *		    tmp[$1] = 1;
   *		    $$ = tmp;
   */
	    int tmp[5];
	    int i;

	    for (i=0; i<5; i++) {
		tmp[i] = 0;
	    }
	    $$ = tmp;
	    $$[$1] = 1;
	/* $$ = add_matches_for($1); */
	}
      | qualifier_list ',' qualifier
	{ /* don't know if it works, let's try */
	    $1[$3] = 1;
	    $$ = $1;
	/* $$ = add_matches_for($3); */
	}
;

parameter_s:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_PARAMETERS parameter_label_list ';'
	{ $$ = $2; }
      | T_PARAMETERS error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal parameter-label \"%s\" at PARAMETERS ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

parameter_label_list:
	parameter_label
	{ $$ = add_param_list($1); }
      | parameter_label_list ',' parameter_label
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next;
	    $1->next = add_param_list($3);
	}
;

qualifier:
	T_EQUALITY
	{ $$ = t_equality; }
      | T_ORDERING
	{ $$ = t_ordering; }
      | T_SUBSTRINGS
	{ $$ = t_substrings; }
      | T_SET_COMPARISON
	{ $$ = t_set_comparison; }
      | T_SET_INTERSECTION
	{ $$ = t_set_intersection; }
/*	      | error no qualifier
 */
;

attribute_group:
/*=======================  ATTRIBUTE GROUP TEMPLATE  ========================*/
	template_label T_ATTRIBUTE T_GROUP
	    libcomment
	group_elements
	fixed
	description
	T_REGISTERED T_AS object_identifier
	';'
	{ $$ = add_group_def($1, $5, $6, $7, $10, 0); }
      | template_label T_ATTRIBUTE T_GROUP
	error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal CONSTRUCT-NAME \"%s\" in \
ATTRIBUTE GROUP \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till end of construct or template */
	{
	    reentered = 0;
	    $$ = check_group_def($1); /* create a dummy entry */
	}
;

group_elements:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_GROUP T_ELEMENTS attribute_label_list ';'
	{ $$ = $3; }
      | T_GROUP T_ELEMENTS error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal attribute-label \"%s\" at GROUP ELEMENTS ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

attribute_label_list:
	attribute_label
	{ $$ = add_attr_list($1); }
      | attribute_label_list ','
	attribute_label
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next;
	    $1->next = add_attr_list($3);
	}
;

fixed:
	/* empty cause it's optional */
	{ $$ = 0; }
      | T_FIXED ';'
	{ $$ = 1; }
;

description:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_DESCRIPTION delimited_string ';'
	{ $$ = $2; }
      | T_DESCRIPTION error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal delimited_string \"%s\" at DESCRIPTION ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

behaviour_definition:
/*==========================  BEHAVIOUR TEMPLATE  ===========================*/
	template_label T_BEHAVIOUR
	    libcomment
	T_DEFINED T_AS delimited_string
	';'
	{ $$ = add_behav_def($1, $6, 0); }
      | template_label T_BEHAVIOUR
	error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal CONSTRUCT-NAME \"%s\" in BEHAVIOUR \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till end of construct or template */
	{
	    reentered = 0;
	    $$ = check_behav_def($1); /* create a dummy entry */
	}
;

action:
/*===========================  ACTION TEMPLATE  =============================*/
	template_label T_ACTION
	    libcomment
	behaviour
	mode_confirmed
	parameter_s
	T_REGISTERED T_AS object_identifier
	';'
	{ $$ = add_action_def($1, $4, $5, $6, NULL, NULL, $9, 0); }
      |
	template_label T_ACTION
	    libcomment
	behaviour
	mode_confirmed
	parameter_s
	with_information_syntax
	T_REGISTERED T_AS object_identifier
	';'
	{ $$ = add_action_def($1, $4, $5, $6, $7, NULL, $10, 0); }
      |
	template_label T_ACTION
	    libcomment
	behaviour
	mode_confirmed
	parameter_s
	with_reply_syntax
	T_REGISTERED T_AS object_identifier
	';'
	{ $$ = add_action_def($1, $4, $5, $6, NULL, $7, $10, 0); }
      |
	template_label T_ACTION
	    libcomment
	behaviour
	mode_confirmed
	parameter_s
	with_information_syntax
	with_reply_syntax
	T_REGISTERED T_AS object_identifier
	';'
	{ $$ = add_action_def($1, $4, $5, $6, $7, $8, $11, 0); }
      | template_label T_ACTION
	error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal CONSTRUCT-NAME \"%s\" in ACTION \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till end of construct or template */
	{
	    reentered = 0;
	    $$ = check_action_def($1); /* create a dummy entry */
	}
;

mode_confirmed:
	/* empty cause it's optional */
	{ $$ = 0; }
      | T_MODE T_CONFIRMED ';'
	{ $$ = 1; }
      | T_MODE error ';'
	{	/* XXX nochmal besser machen */
	    yyerror("probably missing CONFIRMED at MODE ... ;");
	    $$ = 0;
	}
;

with_information_syntax:
	T_WITH T_INFORMATION T_SYNTAX type_reference ';'
	{ $$ = $4; }
      | T_WITH T_INFORMATION T_SYNTAX error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal type-reference \"%s\" at WITH INFORMATION \
SYNTAX ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

with_reply_syntax:
	T_WITH T_REPLY T_SYNTAX type_reference ';'
	{ $$ = $4; }
      | T_WITH T_REPLY T_SYNTAX error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal type-reference \"%s\" at WITH REPLY SYNTAX \
... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

notification:
/*========================  NOTIFICATION TEMPLATE  ==========================*/
	template_label T_NOTIFICATION
	    libcomment
	behaviour
	parameter_s
	    mode_non_confirmed
	T_REGISTERED T_AS object_identifier
	';'
	{ $$ = add_notif_def($1, $4, $5, NULL, NULL, $9, 0); }
      |
	template_label T_NOTIFICATION
	    libcomment
	behaviour
	parameter_s
	    mode_non_confirmed
	with_info_syntax_and_attr_ids
	T_REGISTERED T_AS object_identifier
	';'
	{ $$ = add_notif_def($1, $4, $5, $7, NULL, $10, 0); }
      |
	template_label T_NOTIFICATION
	    libcomment
	behaviour
	parameter_s
	    mode_non_confirmed
	with_reply_syntax
	T_REGISTERED T_AS object_identifier
	';'
	{ $$ = add_notif_def($1, $4, $5, NULL, $7, $10, 0); }
      |
	template_label T_NOTIFICATION
	    libcomment
	behaviour
	parameter_s
	    mode_non_confirmed
	with_info_syntax_and_attr_ids
	with_reply_syntax
	T_REGISTERED T_AS object_identifier
	';'
	{ $$ = add_notif_def($1, $4, $5, $7, $8, $11, 0); }
      |	template_label T_NOTIFICATION
	error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + strlen($1->string) + 80);
		sprintf(buf,
			"illegal CONSTRUCT-NAME \"%s\" in NOTIFICATION \"%s\"",
			yytext, $1->string);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'	/* scan till end of construct or template */
	{
	    reentered = 0;
	    $$ = check_notif_def($1); /* create a dummy entry */
	}
;

mode_non_confirmed:
	/* empty cause it's optional */
	{ }
      | T_MODE T_NON_CONFIRMED ';'
	{ }
;

with_info_syntax_and_attr_ids:
	T_WITH T_INFORMATION T_SYNTAX type_reference
	and_attribute_ids
	';'
	{
	    $$ = (notif_info_syntax *)
		ckalloc(sizeof(notif_info_syntax));
	    $$->syntax = $4;
	    $$->attrids = $5;
	/* $$ = create_notif_info_syntax($4, $5); */
	}
      | T_WITH T_INFORMATION T_SYNTAX error
	{
	    if (!reentered) {
		char	*buf;

		buf = ckalloc(strlen(yytext) + 80);
		sprintf(buf,
			"illegal type-reference or attribute-ids \"%s\" at \
WITH INFORMATION SYNTAX ... ;",
			yytext);
		yyerror(buf);
		ckfree(buf);
	    }
	    ++reentered;
	}
	';'
	{ reentered = 0; $$ = NULL; }
;

/* XXX die naechsten beiden regeln besitzen noch keine ausreichende fehler-
 *     behandlung
 */
and_attribute_ids:
	/* empty cause it's optional */
	{ $$ = NULL; }
      | T_AND T_ATTRIBUTE T_IDS field_and_attribute_s
	{ $$ = $4; }
      | T_AND T_ATTRIBUTE T_IDS error
	{
	    yyerror("illegal field-name or attribute-label at \
AND ATTRIBUTE IDS ...");
	    $$ = NULL;
	}
;

field_and_attribute_s:
	field_name attribute_label
	{
	    $$ = (notif_attr_ids *) ckalloc(sizeof(notif_attr_ids));
	    $$->field_name = $1;
	    $$->def = $2;
	    $$->next = NULL;
	/* $$ = add_notif_attr_ids($1, $2); */
	}
      | field_and_attribute_s ','
	field_name attribute_label
	{
	    $$ = $1;
	    while ($1->next)	$1 = $1->next;
	    $1->next = (notif_attr_ids *)
 				ckalloc(sizeof(notif_attr_ids));
	    $1 = $1->next;
	    $1->field_name = $3;
	    $1->def = $4;
	    $1->next = NULL;
	/* $1 = add_notif_attr_ids($3, $4); */
	}
      | field_and_attribute_s ',' error
	{
	    yyerror("illegal field-name or attribute_label at \
AND ATTRIBUTE IDS ...");
	    $$ = $1;
	}
;

field_name:
/* XXX nicht gerade optimal geloest */
	T_TEMPLATE_LABEL
	{ $$ = ckstrdup($1); }
;

template_label:
	T_TEMPLATE_LABEL
	{
	    $$ = (gdmo_label *) ckalloc(sizeof(gdmo_label));
	    $$->string = ckstrdup($1);
	    $$->standard = NULL;
	    $$->oid = NULL;
	}
/* XXX vorsicht, eigentlich nicht wirklich delimited-string, da
 *     "<standard-name>"
 * document_identifier:
 * 		'"' standard_name '"'
 * 	      | object_identifier
 * 	;
 */ 
      | delimited_string ':' T_TEMPLATE_LABEL
	{
	    $$ = (gdmo_label *) ckalloc(sizeof(gdmo_label));
	    $$->string = ckstrdup($3);
	    $$->standard = $1;
	    $$->oid = NULL;
	}
      | object_identifier ':' T_TEMPLATE_LABEL
	{
	    $$ = (gdmo_label *) ckalloc(sizeof(gdmo_label));
	    $$->string = ckstrdup($3);
	    $$->standard = NULL;
	    $$->oid = $1;
	}
/*	      | error
 *		  {
 *		      char *buf;
 *		      buf = ckalloc(strlen(yytext) + 40);
 *		      yyerror("\"%s\" not a template_label");
 *		      $$ = NULL;
 *		  }
 */
;

type_reference:
	T_TYPE_REFERENCE
	{ $$ = ckstrdup($1); }
/*	      | error
 *		  {
 *		      char *buf;
 *		      buf = ckalloc(strlen(yytext) + 40);
 *		      yyerror("\"%s\" not a type-reference");
 *		      $$ = NULL;
 *		  }
 */
;

delimited_string:
	T_DELIMITED_STRING
	{
	    $$ = (text_in_file *) ckalloc(sizeof(text_in_file));
	    $$->path = $1->path;
	    $$->pos = $1->pos;
	    $$->len = $1->len;
	}
/*      | error
 *	  {
 *	      char *buf;
 *
 *	      buf = ckalloc(strlen(yytext) + 80);
 *	      sprintf(buf, "illegal delimited-string \"%s\"", yytext);
 *	      yyerror(buf);
 *	      ckfree(buf);
 *	      $$ = NULL;
 *	  }
 */
;

/* delimited_string: * vielleicht besser schon im lex erschlagen, ok!! *
 *		text_delimiter text_string text_delimiter
 *	      | text_string  <-- das koennte noch zum problem werden!! XXX
 *	;
 *
 * text_delimiter:
 *		* hier muss ich mir wohl noch was einfallen lassen,
 *		   eventuell erschlage ich das mit einem token, geht
 *		   aber wahrscheinlich nicht, da beide gleich sein muessen *
 *		T_TEXT_DELIMITER
 *	;
 */

%% /* FUNCTIONS */
