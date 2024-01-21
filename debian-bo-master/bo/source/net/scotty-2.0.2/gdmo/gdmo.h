/*
 * gdmo.h
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

#include <scotty.h>

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

/* structures to hold the information of the corresponding definition
 * ------------------------------------------------------------------
 * components they have in common:
 * label	structure with a string that holds the name of the definition
 *              and maybe an delimited-string or an object-identifier that
 *              references the definition, if only label is present
 * only_label	indicates, if the definition body is filled out (== 0)
 * inline_def	indicates, if the definition was made inline (!= 0)
 * oid		the object identifier for the definition
 * next		pointer to the next definition, if present (!= NULL)
 */
typedef struct gdmo_class	gdmo_class;
typedef struct gdmo_pckg	gdmo_pckg;
typedef struct gdmo_param	gdmo_param;
typedef struct gdmo_namebind	gdmo_namebind;
typedef struct gdmo_attr	gdmo_attr;
typedef struct gdmo_group	gdmo_group;
typedef struct gdmo_behav	gdmo_behav;
typedef struct gdmo_action	gdmo_action;
typedef struct gdmo_notif	gdmo_notif;

/* structures to reference the corresponding definition
 * ----------------------------------------------------
 * they have two components:
 * def		pointer to the definition (see above)
 * next		pointer to the next definition reference
 */
typedef struct class_ref	class_ref;
typedef struct param_ref	param_ref;
typedef struct attr_ref		attr_ref;
typedef struct behav_ref	behav_ref;

/* structures to hold further body information of the definition
 * -------------------------------------------------------------
 * if they are used only in one definition, they start with the
 * corresponding definition name
 * if they could be part of a list, the component next is again used
 * to hold a pointer to the next element
 */
typedef struct gdmo_label	gdmo_label;
typedef struct text_in_file	text_in_file;
typedef struct gdmo_oid		gdmo_oid;       /* to avoid ambiguosness */
typedef	       char		type_ref;	/* maybe i change this XXX */
typedef struct syntax_or_attr	syntax_or_attr;

typedef struct class_pckg	class_pckg;

typedef struct pckg_attr	pckg_attr;
typedef struct pckg_property	pckg_property;
typedef struct pckg_val_specifier pckg_val_specifier;
typedef        char		pckg_val_ref;	/* maybe i change this XXX */
typedef struct pckg_group	pckg_group;
typedef struct pckg_action	pckg_action;
typedef struct pckg_notif	pckg_notif;

typedef struct param_context	param_context;

typedef struct namebind_class	namebind_class;
typedef struct namebind_delete	namebind_delete;
typedef struct namebind_create	namebind_create;

typedef struct notif_info_syntax notif_info_syntax;
typedef struct notif_attr_ids	notif_attr_ids;
typedef 	char		notif_field_name;

struct gdmo_label {
    char	*string;
    text_in_file *standard;
    gdmo_oid	*oid;
};

/* Managed Object Class: */

struct gdmo_class {
    gdmo_label	*label;
    int		only_label;
    int		inline_def;
    class_ref	*superclass;
    class_pckg	*mand_pckg; /* mandatory package has no condition! */
    class_pckg	*cond_pckg;
    gdmo_oid	*oid;
    gdmo_class	*next;
};

struct class_ref {
    gdmo_class	*def;
    class_ref	*next;
};

struct class_pckg {
    gdmo_pckg	*def;
    text_in_file *cond;
    class_pckg	*next;
};

struct text_in_file {
    char	*path;
    long	pos;
    long	len;
};

/* struct gdmo_oid {
 *    char		*oid;
    * (noch keinen plan, wie das aussieht, wohl ASN.1 XXX) *
    * FORM: 1.4.8.835.84.74.0 + strings *
    * vielleicht aber auch einfach ein text_in_file *
 * };
 */

struct gdmo_oid {
    char	*part;
    gdmo_oid	*next;
};

/* Package: */

struct gdmo_pckg {
    gdmo_label	*label;
    int		only_label;
    int		inline_def;
    behav_ref	*behav;
    pckg_attr	*attr;
    pckg_group	*group;
    pckg_action	*action;
    pckg_notif	*notif;
    gdmo_oid	*oid;
    gdmo_pckg	*next;
};

struct pckg_attr {
    gdmo_attr	*def;
    int		property[7];	/* 7 -> enum propers */
    pckg_val_specifier *default_val;
    pckg_val_specifier *initial_val;
    type_ref	*permitted_values;
    type_ref	*required_values; 
    param_ref	*param;
    pckg_attr	*next;
};

/* struct pckg_property {
 * * geht das ueberhaupt: 0, 0, 0, 0, ... *
 *    int		property[7];	* enum propers *
 *    pckg_val_ref *default_value;
 *    gdmo_behav	*default_behav;
 *    pckg_val_ref *initial_value;
 *    gdmo_behav	*initial_behav;
 *    type_ref	*permitted_values;
 *   type_ref	*required_values; 
 * *    enum getReplace	get_replace;
 * *    enum addRemove	add_remove;
 *
 * };
 */

enum propers {
    t_replace_with_default	= 0,
    t_default_value		= 1,	/* value-specifier */
    t_initial_value		= 2,	/* value-specifier */
    t_permitted_values		= 3,	/* type-reference */
    t_required_values		= 4,	/* type-reference */
    t_get_replace		= 5,
    t_add_remove		= 6
/*    t_get			= 5,
 *    t_replace			= 6,
 */
/*    t_add			= 8,
 *    t_remove			= 9,
 */
};

/* enum get_add_choice {
 *    none	= 0,
 *    first	= 1, (get bzw. add)
 *    second	= 2, (replace bzw. remove)
 *    both	= 3  (get_replace bzw. add_remove)
 * };
 */

struct pckg_val_specifier { /* XXX value-specifier eigentlich eher union */
    pckg_val_ref *ref;
    gdmo_behav	 *behav;
};

struct pckg_group {
    gdmo_group	*def;
    attr_ref	*attr;
    pckg_group	*next;
};

struct pckg_action {
     gdmo_action *def;
     param_ref	 *param;
     pckg_action *next;
};

struct pckg_notif {
    gdmo_notif	*def;
    param_ref	*param;
    pckg_notif	*next;
};

/* Parameter: */

struct gdmo_param {
    gdmo_label	*label;
    int		only_label;
    int		inline_def;
    param_context *context; /* oder so "ahnlich */
    syntax_or_attr *choice;
    behav_ref	*behav;
    gdmo_oid	*oid;
    gdmo_param	*next;
};

struct param_ref {
    gdmo_param	*def;
    param_ref	*next;
};

struct syntax_or_attr {
    type_ref	*syntax;       /* besser als */
    gdmo_attr	*attr;         /*  union ??? */
};

enum contextToken {
    no_token,
    t_action_info,
    t_action_reply,
    t_event_info,
    t_event_reply,
    t_specific_error
};

struct param_context {	/* eigentlich union */
    type_ref	*keyword; /* vielleicht text_in_file */
    enum contextToken type;
};

/* struct param_context { */
    /* sechs alternativen, davon f"unf einfach ??? */
    /* union benutzen, oder? */
/* } */

/* Name binding: */

struct gdmo_namebind {
    gdmo_label	*label;
    int		only_label;
    int		inline_def;
    namebind_class *subclass;
    namebind_class *superclass;
    gdmo_attr	*attr;
    behav_ref	*behav;
    namebind_create *create;
    namebind_delete *delete;
    gdmo_oid	*oid;
    gdmo_namebind *next;
};

enum deleteModifier {
    no_delete_modifier,
    only_if_no_contained_objects,
    deletes_contained_objects
};

struct namebind_delete {
    enum deleteModifier	modifier;
    param_ref	*param;
};

enum createModifier {
    no_create_modifier,
    with_reference_object,
    with_automatic_instance_naming,
    both_create_modifier
};

struct namebind_create {
    enum createModifier	modifier;
    param_ref	*param;
};

struct namebind_class {
    gdmo_class	*def;
    int		and_subs;
};

/* struct classRec {
 *     gdmo_class	*def;
 *     int	and_subs;
 * }
 *
 * struct createRec {
 *     modifier	crtModi;
 *      * 0,1,2,3 keiner oder a oder b oder beide *
 *     param	*param_ref;
 * }
 * struct deleteRec {
 *     modifier	delModi;
 *      * 0,1,2 keiner oder a oder b *
 *     param	*param_ref;
 * }
 */

/* Attribute: */

struct gdmo_attr {
    gdmo_label	*label;
    int		only_label;
    int		inline_def;
    syntax_or_attr *choice;
    int		matches_for[5]; /* enum matches !!! */
    /* oder (entsprechende Bit-positionen setzen in einem Byte) */
    behav_ref	*behav;
    param_ref	*param;
    gdmo_oid	*oid;
    gdmo_attr	*next;
};

struct attr_ref {
    gdmo_attr	*def;
    attr_ref	*next;
};

enum matches {
/* wahrscheinlich sind die Werte "uberfl"ussig, aber wegen matchesFor[5] da! */
    t_equality		= 0,
    t_ordering		= 1,
    t_substrings	= 2,
    t_set_comparison	= 3,
    t_set_intersection	= 4
};

/* Attribute group: */

struct gdmo_group {
    gdmo_label	*label;
    int		only_label;
    int		inline_def;
    attr_ref	*attr;
    int		fixed;
    text_in_file *description;
    gdmo_oid	*oid;
    gdmo_group	*next;
};

/* Behaviour: */

struct gdmo_behav {
    gdmo_label	*label;
    int		only_label;
    int		inline_def;
    text_in_file *description;
    gdmo_behav	*next;
};

struct behav_ref {
    gdmo_behav	*def;
    behav_ref	*next;
};

/* Action: */

struct gdmo_action {
    gdmo_label	*label;
    int		only_label;
    int		inline_def;
    behav_ref	*behav;
    int		confirmed;
    param_ref	*param;
    type_ref	*info_syntax;
    type_ref	*reply_syntax;
    gdmo_oid	*oid;
    gdmo_action	*next;
};

/* Notification: */

struct gdmo_notif {
    gdmo_label	*label;
    int		only_label;
    int		inline_def;
    behav_ref	*behav;
    param_ref	*param;
    notif_info_syntax *syntax_and_attr_ids;
    type_ref	*reply_syntax;
    gdmo_oid	*oid;
    gdmo_notif	*next;
};

struct notif_info_syntax {
    type_ref	*syntax;
    notif_attr_ids *attrids; /* optional, only present if syntax is present! */
};

struct notif_attr_ids {
    notif_field_name	*field_name;
    gdmo_attr		*def;
    notif_attr_ids	*next;
};

/*
 * Global variables to hold the linear lists of the 
 * corresponding definitions:
 */

extern gdmo_class	*class_def_list;
extern gdmo_pckg	*pckg_def_list;
extern gdmo_param	*param_def_list;
extern gdmo_namebind	*namebind_def_list;
extern gdmo_attr	*attr_def_list;
extern gdmo_group	*group_def_list;
extern gdmo_behav	*behav_def_list;
extern gdmo_action	*action_def_list;
extern gdmo_notif	*notif_def_list;



