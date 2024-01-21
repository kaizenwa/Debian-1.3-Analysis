/* Interpretation of Xconq GDL.
   Copyright (C) 1989, 1991, 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* Syntax is el cheapo Lisp. */

#include "conq.h"
#define for_both_lists(lis1,lis2,rest1,rest2)  \
   for (rest1 = (lis1), rest2 = (lis2);  \
	rest1 != lispnil && rest2 != lispnil;  \
	rest1 = cdr(rest1), rest2 = cdr(rest2))
#include "imf.h"

extern int actually_read_lisp;

static void module_and_line PARAMS ((Module *module, char *buf));
static void init_constant PARAMS ((int key, int val));
static void init_self_eval PARAMS ((int key));
static void useless_form_warning PARAMS ((Module *module, Obj *form));
static void include_module PARAMS ((Obj *form, Module *module));
static void start_conditional PARAMS ((Obj *form, Module *module));
static void start_else PARAMS ((Obj *form, Module *module));
static void end_conditional PARAMS ((Obj *form, Module *module));
static Variant *interp_variant_defns PARAMS ((Obj *lis));
static void interp_utype PARAMS ((Obj *form));
static void fill_in_utype PARAMS ((int u, Obj *list));
static int set_utype_property PARAMS ((int u, char *propname, Obj *val));
static void interp_mtype PARAMS ((Obj *form));
static void fill_in_mtype PARAMS ((int m, Obj *list));
static void interp_ttype PARAMS ((Obj *form));
static void fill_in_ttype PARAMS ((int t, Obj *list));
static void interp_table PARAMS ((Obj *form));
static void add_to_table PARAMS ((Obj *tablename, int tbl, Obj *clauses));
static void interp_one_clause PARAMS ((Obj *tablename, int tbl, int lim1, int lim2, Obj *indexes1, Obj *indexes2, Obj *values));
static void interp_variable PARAMS ((Obj *form, int isnew));
static void undefine_variable PARAMS ((Obj *form));
static void add_properties PARAMS ((Obj *form));
static int list_lengths_match PARAMS ((Obj *types, Obj *values, char *formtype, Obj *form));
static void add_to_utypes PARAMS ((Obj *types, Obj *prop, Obj *values));
static void add_to_mtypes PARAMS ((Obj *types, Obj *prop, Obj *values));
static void add_to_ttypes PARAMS ((Obj *types, Obj *prop, Obj *values));
static void interp_world PARAMS ((Obj *form));
static void interp_area PARAMS ((Obj *form));
static void fill_in_terrain PARAMS ((Obj *contents));
static void fill_in_aux_terrain PARAMS ((Obj *contents));
static void fill_in_people_sides PARAMS ((Obj *contents));
static void fill_in_features PARAMS ((Obj *contents));
static void fill_in_elevations PARAMS ((Obj *contents));
static void fill_in_cell_material PARAMS ((Obj *contents));
static void fill_in_temperatures PARAMS ((Obj *contents));
static void fill_in_winds PARAMS ((Obj *contents));
static void fill_in_clouds PARAMS ((Obj *contents));
static void fill_in_cloud_bottoms PARAMS ((Obj *contents));
static void fill_in_cloud_heights PARAMS ((Obj *contents));
static void interp_side PARAMS ((Obj *form, Side *side));
static void check_name_uniqueness PARAMS ((Side *side, char *str, char *kind));
static void merge_unit_namers PARAMS ((Side *side, Obj *lis));
static void interp_side_value_list PARAMS ((short *arr, Obj *lis));
static void interp_atkstats_list PARAMS ((Side *side, Obj *lis));
static void interp_hitstats_list PARAMS ((Side *side, Obj *lis));
static void fn_set_terrain_view PARAMS ((int x, int y, int val));
static void fn_set_aux_terrain_view PARAMS ((int x, int y, int val));
static void fn_set_terrain_view_date PARAMS ((int x, int y, int val));
static void fn_set_aux_terrain_view_date PARAMS ((int x, int y, int val));
static void fn_set_unit_view PARAMS ((int x, int y, int val));
static void fn_set_unit_view_date PARAMS ((int x, int y, int val));
static void fn_set_material_view PARAMS ((int x, int y, int val));
static void fn_set_material_view_date PARAMS ((int x, int y, int val));
static void fn_set_temp_view PARAMS ((int x, int y, int val));
static void fn_set_temp_view_date PARAMS ((int x, int y, int val));
static void fn_set_cloud_view PARAMS ((int x, int y, int val));
static void fn_set_cloud_bottom_view PARAMS ((int x, int y, int val));
static void fn_set_cloud_height_view PARAMS ((int x, int y, int val));
static void fn_set_cloud_view_date PARAMS ((int x, int y, int val));
static void fn_set_wind_view PARAMS ((int x, int y, int val));
static void fn_set_wind_view_date PARAMS ((int x, int y, int val));
static void read_view_layer PARAMS ((Side *side, Obj *contents, void (*setter)(int, int, int)));
static void read_aux_terrain_view_layer PARAMS ((Side *side, Obj *contents, void (*setter)(int, int, int)));
static void read_material_view_layer PARAMS ((Side *side, Obj *contents, void (*setter)(int, int, int)));
static void read_general_doctrine PARAMS ((Side *side, Obj *props));
static void read_utype_doctrine PARAMS ((Side *side, Obj *list));
static void interp_doctrine PARAMS ((Obj *form));
static void fill_in_doctrine PARAMS ((struct a_doctrine *doctrine, Obj *props));
static void interp_player PARAMS ((Obj *form));
static void fill_in_player PARAMS ((struct a_player *player, Obj *props));
static void interp_agreement PARAMS ((Obj *form));
static void interp_unit_defaults PARAMS ((Obj *form));
static void interp_unit PARAMS ((Obj *form));
static void interp_utype_list PARAMS ((short *arr, Obj *lis));
static void interp_utype_value_list PARAMS ((short *arr, Obj *lis));
static void interp_mtype_value_list PARAMS ((short *arr, Obj *lis));
static void interp_short_array PARAMS ((short *arr, Obj *lis, int n));
static void interp_long_array PARAMS ((long *arr, Obj *lis, int n));
static void interp_unit_act PARAMS ((Unit *unit, Obj *props));
static void interp_unit_plan PARAMS ((Unit *unit, Obj *props));
static Task *interp_task PARAMS ((Obj *form));
static Goal *interp_goal PARAMS ((Obj *form));
static void interp_namer PARAMS ((Obj *form));
static void interp_text_generator PARAMS ((Obj *form));
static void interp_scorekeeper PARAMS ((Obj *form));
static void interp_history PARAMS ((Obj *form));
static void interp_past_unit PARAMS ((Obj *form));

static void too_many_types PARAMS ((char *typename, int maxnum, Obj *name));
static void unknown_property PARAMS ((char *type, char *inst, char *name));
static void read_layer PARAMS ((Obj *contents, void (*setter)(int, int, int)));
static void read_rle PARAMS ((Obj *contents, void (*setter)(int, int, int), short *chartable));

/* This is the module from which forms are being read and
   interpreted, if they are coming from a module. */

Module *curmodule;

Obj *cond_read_stack;

/* True if game will start up in the middle of a turn. */

int midturnrestore = FALSE;

/* The count of cells that did not have valid terrain data. */

int numbadterrain = 0;

/* True if should warn about bad terrain. */

int warnbadterrain = TRUE;

char *readerrbuf;

/* This is the list of side defaults that will be applied
   to all sides read subsequently. */

Obj *side_defaults;

/* Defaults to use for filling in unit properties. */

int uxoffset = 0, uyoffset = 0;

static short default_unit_side_number = -1;

static short default_unit_origside_number = -1;

static short default_unit_cp = -1;

static short default_unit_hp = -1;

static short default_unit_cxp = -1;

static short default_unit_z = -1;

static short default_transport_id = -1;

static Obj *default_unit_hook;

static short *default_supply;

static short *default_tooling;

/* Globals used to communicate with the RLE reader. */

short layer_use_default;
int layer_default;
int layer_multiplier;
int layer_adder;
short layer_area_x, layer_area_y;
short layer_area_w, layer_area_h;

int ignore_specials;

/* This is the table of keywords. */

struct a_key {
    char *name;
    short key;
} keywordtable[] = {

#undef  DEF_KWD
#define DEF_KWD(NAME,CODE)  { NAME, CODE },

#include "keyword.def"

    { NULL, 0 }
};

/* Given a string, return the enum of the matching keyword,
   if found, else -1. */

int
keyword_code(str)
char *str;
{
    int i;

    /* (should do a binary search first, then switch to exhaustive) */
    for (i = 0; keywordtable[i].name != NULL; ++i) {
	if (strcmp(str, keywordtable[i].name) == 0)
	  return keywordtable[i].key;
    }
    return (-1);
}

char *
keyword_name(k)
enum keywords k;
{
    return keywordtable[k].name;
}

#define TYPEPROP(TYPES, N, DEFNS, I, TYPE)  \
  ((TYPE *) &(((char *) (&(TYPES[N])))[DEFNS[I].offset]))[0]

/* This is a generic syntax check and escape. */

#define SYNTAX(X,TEST,MSG)  \
  if (!(TEST)) {  \
      syntax_error((X), (MSG));  \
      return;  \
  }
  
#define SYNTAX_RETURN(X,TEST,MSG,RET)  \
  if (!(TEST)) {  \
      syntax_error((X), (MSG));  \
      return (RET);  \
  }

void  
syntax_error(x, msg)
Obj *x;
char *msg;
{
    sprintlisp(readerrbuf, x);
    read_warning("syntax error in `%s' - %s", readerrbuf, msg);
}

/* This is specifically for typechecking. */

#define TYPECHECK(PRED,X,MSG)  \
  if (!PRED(X)) {  \
      type_error((X), (MSG));  \
      return;  \
  }

#define TYPECHECK_RETURN(PRED,X,MSG,RET)  \
  if (!PRED(X)) {  \
      type_error((X), (MSG));  \
      return (RET);  \
  }

void
type_error(x, msg)
Obj *x;
char *msg;
{
    sprintlisp(readerrbuf, x);
    read_warning("type error in `%s' - %s", readerrbuf, msg);
}

/* Parse the (propertyname value) lists that most forms use. */

#define PARSE_PROPERTY(BDG,NAME,VAL)  \
  SYNTAX(BDG, (consp(BDG) && symbolp(car(BDG))), "property binding");  \
  (NAME) = c_string(car(BDG));  \
  (VAL) = cadr(BDG);

static void
module_and_line(module, buf)
Module *module;
char *buf;
{
    if (module) {
	if (module->startlineno != module->endlineno) {
	    sprintf(buf, "%s:%d-%d: ",
		    module->name, module->startlineno, module->endlineno);
	} else {
	    sprintf(buf, "%s:%d: ",
		    module->name, module->startlineno);
	}
    } else {
	buf[0] = '\0';
    }
}

static void
init_constant(key, val)
int key;
{
    Obj *sym = intern_symbol(keyword_name(key));

    setq(sym, new_number(val));
    flag_as_constant(sym);
}

static void
init_self_eval(key)
int key;
{
    Obj *sym = intern_symbol(keyword_name(key));

    setq(sym, sym);
    flag_as_constant(sym);
}

void
init_predefined_symbols()
{
    int m;

    /* Predefined constants. */
    init_constant(K_FALSE, 0);
    init_constant(K_TRUE, 1);
    init_constant(K_NON_UNIT, -1);
    init_constant(K_NON_MATERIAL, -1);
    init_constant(K_NON_TERRAIN, -1);
    init_constant(K_CELL, 0);
    init_constant(K_BORDER, 1);
    init_constant(K_CONNECTION, 2);
    init_constant(K_COATING, 3);
    init_constant(K_RIVER_X, 10);
    init_constant(K_VALLEY_X, 11);
    init_constant(K_ROAD_X, 12);
    init_constant(K_OVER_NOTHING, 0);
    init_constant(K_OVER_OWN, 1);
    init_constant(K_OVER_BORDER, 2);
    init_constant(K_OVER_ALL, 3);
    /* Random self-evaluating symbols. */
    init_self_eval(K_AND);
    init_self_eval(K_OR);
    init_self_eval(K_NOT);
    init_self_eval(K_REJECT);
    init_self_eval(K_RESET);
    init_self_eval(K_USUAL);
    /* Leave these unbound so that first ref computes correct list. */
    intern_symbol(keyword_name(K_USTAR));
    intern_symbol(keyword_name(K_MSTAR));
    intern_symbol(keyword_name(K_TSTAR));
    /* This just needs to be inited somewhere. */
    side_defaults = lispnil;
    /* Same for this. */
    /* (Can't use nummtypes because not set up yet.) */
    if (default_supply == NULL) {
	default_supply = (short *) xmalloc(MAXMTYPES * sizeof(short));
	for (m = 0; m < MAXMTYPES; ++m)
	  default_supply[m] = -1;
    }
    /* And for this. */
    if (readerrbuf == NULL)
      readerrbuf = (char *) xmalloc(BUFSIZE);
}

/* This is the basic interpreter of a form appearing in a module. */

void
interp_form(module, form)
Module *module;
Obj *form;
{
    Obj *thecar;
    char *name;

    /* Put the passed-in module into a global; for use in error messages. */
    curmodule = module;
    if (consp(form) && symbolp(thecar = car(form))) {
	name = c_string(thecar);
	if (Debug) {
	    /* If in a module, report the line number(s) of a form. */
	    if (module != NULL) {
		Dprintf("Line %d", module->startlineno);
		if (module->endlineno != module->startlineno)
		  Dprintf("-%d", module->endlineno);
	    }
	    Dprintf(": (%s ", name);
	    Dprintlisp(cadr(form));
	    if (cddr(form) != lispnil) {
		Dprintf(" ");
		Dprintlisp(caddr(form));
		if (cdr(cddr(form)) != lispnil)
		  Dprintf(" ...");
	    }
	    Dprintf(")\n");
	}
	switch (keyword_code(name)) {
	  case K_GAME_MODULE:
	    interp_game_module(form, module);
	    load_base_module(module);
	    break;
#ifndef SPECIAL
	  case K_UNIT_TYPE:
	    interp_utype(form);
	    break;
	  case K_MATERIAL_TYPE:
	    interp_mtype(form);
	    break;
	  case K_TERRAIN_TYPE:
	    interp_ttype(form);
	    break;
	  case K_TABLE:
	    interp_table(form);
	    break;
          case K_DEFINE:
	    interp_variable(form, TRUE);
	    break;
	  case K_SET:
	    interp_variable(form, FALSE);
	    break;
	  case K_UNDEFINE:
	    undefine_variable(form);
	    break;
	  case K_ADD:
	    add_properties(form);
	    break;
#endif /* n SPECIAL */
	  case K_WORLD:
	    interp_world(form);
	    break;
	  case K_AREA:
	    interp_area(form);
	    break;
	  case K_SIDE:
	    interp_side(form, NULL);
	    break;
	  case K_SIDE_DEFAULTS:
	    side_defaults = cdr(form);
	    break;
	  case K_DOCTRINE:
	    interp_doctrine(form);
	    break;
	  case K_INDEPENDENT_UNITS:
	    interp_side(form, indepside);
	    break;
	  case K_PLAYER:
	    interp_player(form);
	    break;
	  case K_AGREEMENT:
	    interp_agreement(form);
	    break;
	  case K_SCOREKEEPER:
	    interp_scorekeeper(form);
	    break;
	  case K_EVT:
	    interp_history(form);
	    break;
	  case K_EXU:
	    interp_past_unit(form);
	    break;
	  case K_BATTLE:
	    read_warning("battle objects not yet supported");
	    break;
	  case K_UNIT:
	    /* We must have some unit types! */
	    if (numutypes == 0)
	      load_default_game();
	    interp_unit(cdr(form));
	    break;
	  case K_UNIT_DEFAULTS:
	    interp_unit_defaults(cdr(form));
	    break;
	  case K_NAMER:
	    interp_namer(form);
	    break;
	  case K_TEXT:
	    interp_text_generator(form);
	    break;
	  case K_IMF:
	    interp_imf(form);
	    break;
	  case K_PALETTE:
	    interp_palette(form);
	    break;
	  case K_COLOR:
	    interp_color(form);
	    break;
	  case K_INCLUDE:
	    include_module(form, module);
	    break;
	  case K_IF:
	    start_conditional(form, module);
	    break;
	  case K_ELSE:
	    start_else(form, module);
	    break;
	  case K_END_IF:
	    end_conditional(form, module);
	    break;
	  case K_PRINT:
	    print_form(cadr(form));
	    break;
	  default:
	    if (numutypes == 0)
	      load_default_game();
	    if ((boundp(thecar) && utypep(symbol_value(thecar)))
		|| utype_from_name(name) != NONUTYPE) {
		interp_unit(form);
	    } else {
		useless_form_warning(module, form);
	    }
	}
    } else {
	useless_form_warning(module, form);
    }
}

static void
useless_form_warning(module, form)
Module *module;
Obj *form;
{
    char posbuf[BUFSIZE], buf[BUFSIZE];

    if (!actually_read_lisp)
      return;
    module_and_line(module, posbuf);
    sprintlisp(buf, form);
    init_warning("%sA useless form: %s", posbuf, buf);
}

/* Inclusion is half-module-like, not strictly textual. */

static void
include_module(form, module)
Obj *form;
Module *module;
{
    char *name;
    Obj *mname = cadr(form);
    Module *submodule;

    SYNTAX(mname, (symbolp(mname) || stringp(mname)),
	   "included module name not a string or symbol");
    name = c_string(mname);
    Dprintf("Including \"%s\" ...\n", name);
    submodule = add_game_module(name, module);
    load_game_module(submodule, TRUE);
    if (submodule->loaded) {
    	do_module_variants(submodule, cddr(form));
    } 
    Dprintf("... Done including \"%s\".\n", name);
}

static void
start_conditional(form, module)
Obj *form;
Module *module;
{
    Obj *testform, *rslt;

    testform = cadr(form);
    rslt = eval(testform);
    if (numberp(rslt) && c_number(rslt) == 1) {
	actually_read_lisp = TRUE;
    } else {
	actually_read_lisp = FALSE;
    }
}

static void
start_else(form, module)
Obj *form;
Module *module;
{
    /* should match up with cond read stack */
    actually_read_lisp = !actually_read_lisp;
}

static void
end_conditional(form, module)
Obj *form;
Module *module;
{
    /* should match up with cond read stack */
    actually_read_lisp = TRUE;
}

/* Given a list of variant-defining forms, allocate and return an
   array of variant objects. */

static Variant *
interp_variant_defns(lis)
Obj *lis;
{
    int i = 0, len;
    Obj *head;
    Variant *varray, *var;

    if (lis == lispnil)
      return NULL;
    len = length(lis);
    varray = (Variant *) xmalloc((len + 1) * sizeof(Variant));
    for (i = 0; i < len; ++i) {
	var = varray + i;
	var->id = var->dflt = var->range = var->cases = lispnil;
	head = car(lis);
	if (symbolp(head)) {
	    var->id = head;
	    var->name = c_string(var->id);
	} else if (consp(head)) {
	    if (stringp(car(head))) {
		var->name = c_string(car(head));
		head = cdr(head);
	    }
	    if (symbolp(car(head))) {
		var->id = car(head);
		if (var->name == NULL)
		  var->name = c_string(var->id);
		head = cdr(head);
	    } else if (var->name != NULL) {
	    	var->id = intern_symbol(var->name);
	    } else {
		read_warning("bad variant (#%d), continuing", i);
		var->id = new_number(i);
	    }
	    /* Pick up a default value if specified. */
	    if (!consp(car(head))) {
	    	var->dflt = car(head);
	    	head = cdr(head);
	    } else if (match_keyword(var->id, K_WORLD_SIZE)) {
	    	var->dflt = car(head);
	    	head = cdr(head);
	    } else if (match_keyword(var->id, K_REAL_TIME)) {
	    	var->dflt = car(head);
	    	head = cdr(head);
	    }
	    /* (should recognize and pick up a range spec if present) */
	    /* Case clauses are everything that's left over. */
	    var->cases = head;
	} else {
	    read_warning("bad variant (#%d), ignoring", i);
	    var->id = new_number(i);
	}
	lis = cdr(lis);
    }
    /* Terminate the array with an id that never appears otherwise. */
    varray[i].id = lispnil;
    return varray;
}

/* Digest the form defining the module as a whole. */

void
interp_game_module(form, module)
Obj *form;
Module *module;
{
    char *name = NULL, *propname, *strval = NULL;
    Obj *props = cdr(form), *bdg, *propval;

    if (module == NULL)
      return;  /* why is this here? */

    /* Collect and set the module name if supplied by this form. */
    if (stringp(car(props))) {
	name = c_string(car(props));
	props = cdr(props);
    }
    if (name != NULL) {
	if (empty_string(module->name)) {
	    module->name = name;
	} else {
	    if (strcmp(name, module->name) != 0) {
		read_warning("Module name `%s' does not match declared name `%s', ignoring declared name",
			     module->name, name);
	    }
	}
    }
    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, propval);
	if (stringp(propval))
	  strval = c_string(propval);
	switch (keyword_code(propname)) {
	  case K_TITLE:
	    module->title = strval;
	    break;
	  case K_BLURB:
	    module->blurb = strval;
	    break;
	  case K_PICTURE_NAME:
	    module->picturename = strval;
	    break;
	  case K_BASE_MODULE:
	    module->basemodulename = strval;
	    break;
	  case K_DEFAULT_BASE_MODULE:
	    module->defaultbasemodulename = strval;
	    break;
	  case K_BASE_GAME:
	    module->basegame = strval;
	    break;
	  case K_INSTRUCTIONS:
	    /* The instructions are a list of strings. */
	    module->instructions = propval;
	    break;
	  case K_VARIANTS:
	    module->variants = interp_variant_defns(cdr(bdg));
	    break;
	  case K_NOTES:
	    /* The player notes are a list of strings. */
	    module->notes = propval;
	    break;
	  case K_DESIGN_NOTES:
	    /* The design notes are a list of strings. */
	    module->designnotes = propval;
	    break;
	  case K_VERSION:
	    module->version = strval;
	    break;
	  case K_PROGRAM_VERSION:
	    module->programversion = strval;
	    break;
	  case K_ORIGINAL_MODULE:
	    module->origmodulename = strval;
	    break;
	  case K_ORIGINAL_VARIANTS:
	    module->origvariants = interp_variant_defns(cdr(bdg));
	    break;
	  case K_ORIGINAL_VERSION:
	    module->origversion = strval;
	    break;
	  default:
	    unknown_property("game module", module->name, propname);
	}
    }
    /* Should be smarter about earlier vs later versions. */
    if (!empty_string(module->programversion)
	&& strcmp(module->programversion, version_string()) != 0) {
	/* This should become some sort of alert on some systems. */
	read_warning("The module `%s' is claimed to be for Xconq version `%s', but you are actually running version `%s'",
		     module->name, module->programversion, version_string());
    }
}

/* The following code is unneeded if all the types have been compiled in. */

#ifndef SPECIAL

/* Create a new type of unit and fill in info about it. */

static void
interp_utype(form)
Obj *form;
{
    int u;
    Obj *name = cadr(form), *utype;

    TYPECHECK(symbolp, name, "unit-type name not a symbol");
    if (!canaddutype)
      read_warning("Should not be defining more unit types");
    if (numutypes < MAXUTYPES) {
	u = numutypes++;
	utype = new_utype(u);
	/* Set default values for the unit type's props first. */
	/* Any default type name shouldn't confuse the code below. */
	default_unit_type(u);
	setq(name, utype);
	/* Set the values of random props. */
	fill_in_utype(u, cddr(form));
	/* If no internal type name string given, use the defined name. */
	if (empty_string(u_internal_name(u))) {
	    set_u_internal_name(u, c_string(name));
	}
	if (empty_string(u_type_name(u))) {
	    set_u_type_name(u, u_internal_name(u));
	}
	/* If the official type name is different from the internal name,
	   make it a variable bound to the type. */ 
	if (strcmp(u_type_name(u), u_internal_name(u)) != 0) {
	    setq(intern_symbol(u_type_name(u)), utype);
	}
    } else {
	too_many_types("unit", MAXUTYPES, name);
    }
    /* Blast any cached list of types. */
    makunbound(intern_symbol(keyword_name(K_USTAR)));
    eval_symbol(intern_symbol(keyword_name(K_USTAR)));
}

/* Trudge through assorted properties, filling them in. */

static void
fill_in_utype(u, list)
int u;
Obj *list;
{
    char *propname;
    Obj *bdg, *val;

    for ( ; list != lispnil; list = cdr(list)) {
	bdg = car(list);
	PARSE_PROPERTY(bdg, propname, val);
	set_utype_property(u, propname, val);
    }
}

/* Given a unit type, property name, and a value, find the
   definition of the property and set its value. */

static int
set_utype_property(u, propname, val)
int u;
char *propname;
Obj *val;
{
    int i, found = FALSE, numval;
    char *strval;

    for (i = 0; utypedefns[i].name != NULL; ++i) {
	if (strcmp(propname, utypedefns[i].name) == 0) {
	    if (utypedefns[i].intgetter) {
		val = eval(val);
		if (!numberishp(val)) {
		    read_warning("unit type %s property %s value not a number",
				 u_type_name(u), utypedefns[i].name);
		    return TRUE;
		}
		numval = c_number(val);
		if (!between(utypedefns[i].lo, numval, utypedefns[i].hi)) {
		    read_warning("unit type %s property %s value %d not between %d and %d",
				 u_type_name(u), utypedefns[i].name, numval,
				 utypedefns[i].lo, utypedefns[i].hi);
		    /* Let it pass through, at least for now. */
		}
		TYPEPROP(utypes, u, utypedefns, i, short) = numval;
	    } else if (utypedefns[i].strgetter) {
		val = eval(val);
		if (!stringp(val)) {
		    read_warning("unit type %s property %s value not a string",
				 u_type_name(u), utypedefns[i].name);
		    return TRUE;
		}
		strval = c_string(val);
		TYPEPROP(utypes, u, utypedefns, i, char *) = strval;
	    } else {
		TYPEPROP(utypes, u, utypedefns, i, Obj *) = val;
	    }
	    found = TRUE;
	    break;
	}
    }
    if (!found)
      unknown_property("unit type", u_type_name(u), propname);
    return found;
}

/* Declare a new type of material and fill in info about it. */

static void
interp_mtype(form)
Obj *form;
{
    int m;
    Obj *name = cadr(form), *mtype;
    
    TYPECHECK(symbolp, name, "material-type name not a symbol");
    if (!canaddmtype)
      read_warning("Should not be defining more material types");
    if (nummtypes < MAXMTYPES) {
	m = nummtypes++;
	mtype = new_mtype(m);
	/* Set default values for the material type's properties first. */
	default_material_type(m);
	setq(name, mtype);
	/* Set the values of random props. */
	fill_in_mtype(m, cddr(form));
	/* If no type name string given, use the defined name. */
	if (empty_string(m_type_name(m))) {
	    set_m_type_name(m, c_string(name));
	}
    } else {
	too_many_types("material", MAXMTYPES, name);
    }
    /* Blast and remake any cached list of types. */
    makunbound(intern_symbol(keyword_name(K_MSTAR)));
    eval_symbol(intern_symbol(keyword_name(K_MSTAR)));
}

/* Go through a list of prop name/value pairs and fill in the
   material type description from them. */

static void
fill_in_mtype(m, list)
int m;
Obj *list;
{
    int i, found, numval;
    char *strval;
    Obj *bdg, *val;
    char *propname;

    for ( ; list != lispnil; list = cdr(list)) {
	bdg = car(list);
	PARSE_PROPERTY(bdg, propname, val);
	found = FALSE;
	for (i = 0; mtypedefns[i].name != NULL; ++i) {
	    if (strcmp(propname, mtypedefns[i].name) == 0) {
		if (mtypedefns[i].intgetter) {
		    val = eval(val);
		    if (!numberishp(val)) {
			read_warning("material type %s property %s value not a number",
				     m_type_name(m), mtypedefns[i].name);
			return;
		    }
		    numval = c_number(val);
		    if (!between(mtypedefns[i].lo, numval, mtypedefns[i].hi)) {
			read_warning("material type %s property %s value %d not between %d and %d",
				     m_type_name(m), mtypedefns[i].name, numval,
				     mtypedefns[i].lo, mtypedefns[i].hi);
			/* Let it pass through, at least for now. */
		    }
		    TYPEPROP(mtypes, m, mtypedefns, i, short) = numval;
		} else if (mtypedefns[i].strgetter) {
		    val = eval(val);
		    if (!stringp(val)) {
			read_warning("material type %s property %s value not a string",
				     m_type_name(m), mtypedefns[i].name);
			return;
		    }
		    strval = c_string(val);
		    TYPEPROP(mtypes, m, mtypedefns, i, char *) = strval;
		} else {
		    TYPEPROP(mtypes, m, mtypedefns, i, Obj *) = val;
		}
		found = TRUE;
		break;
	    }
	}
	if (!found)
	  unknown_property("material type", m_type_name(m), propname);
    }
}

/* Declare a new type of terrain and fill in info about it. */

static void
interp_ttype(form)
Obj *form;
{
    int t;
    Obj *name = cadr(form), *ttype;

    TYPECHECK(symbolp, name, "terrain-type name not a symbol");
    if (!canaddttype)
      read_warning("Should not be defining more terrain types");
    if (numttypes < MAXTTYPES) {
	t = numttypes++;
	ttype = new_ttype(t);
	/* Set default values for the terrain type's props first. */
	default_terrain_type(t);
	setq(name, ttype);
	/* Set the values of random properties. */
	fill_in_ttype(t, cddr(form));
	/* If no type name string given, use the defined name. */
	if (empty_string(t_type_name(t))) {
	    set_t_type_name(t, c_string(name));
	}
    } else {
	too_many_types("terrain", MAXTTYPES, name);
    }
    /* Blast and remake any cached list of all types. */
    makunbound(intern_symbol(keyword_name(K_TSTAR)));
    eval_symbol(intern_symbol(keyword_name(K_TSTAR)));
}

/* Go through a list of prop name/value pairs and fill in the
   terrain type description from them. */

static void
fill_in_ttype(t, list)
int t;
Obj *list;
{
    int i, found, numval;
    char *strval;
    char *propname;
    Obj *bdg, *val;

    for ( ; list != lispnil; list = cdr(list)) {
	bdg = car(list);
	PARSE_PROPERTY(bdg, propname, val);
	found = FALSE;
	for (i = 0; ttypedefns[i].name != NULL; ++i) {
	    if (strcmp(propname, ttypedefns[i].name) == 0) {
		if (ttypedefns[i].intgetter) {
		    val = eval(val);
		    if (!numberishp(val)) {
			read_warning("terrain type %s property %s value not a number",
				     t_type_name(t), ttypedefns[i].name);
			return;
		    }
		    numval = c_number(val);
		    if (!between(ttypedefns[i].lo, numval, ttypedefns[i].hi)) {
			read_warning("terrain type %s property %s value %d not between %d and %d",
				     t_type_name(t), ttypedefns[i].name, numval,
				     ttypedefns[i].lo, ttypedefns[i].hi);
			/* Let it pass through, at least for now. */
		    }
		    TYPEPROP(ttypes, t, ttypedefns, i, short) = numval;
		} else if (ttypedefns[i].strgetter) {
		    val = eval(val);
		    if (!stringp(val)) {
			read_warning("terrain type %s property %s value not a string",
				     t_type_name(t), ttypedefns[i].name);
			return;
		    }
		    strval = c_string(val);
		    TYPEPROP(ttypes, t, ttypedefns, i, char *) = strval;
		} else {
		    TYPEPROP(ttypes, t, ttypedefns, i, Obj *) = val;
		}
		found = TRUE;
		break;
	    }
	}
	if (!found)
	  unknown_property("terrain type", t_type_name(t), propname);
    }
    /* Recalculate the count of subtypes. */
    count_terrain_subtypes();
}

/* Fill in a table. */

static void
interp_table(form)
Obj *form;
{
    int i, found, reset = TRUE;
    Obj *formsym = cadr(form), *body = cddr(form);
    char *tablename;

    TYPECHECK(symbolp, formsym, "table name not a symbol");
    tablename = c_string(formsym);
    found = FALSE;
    for (i = 0; tabledefns[i].name != NULL; ++i) {
	if (strcmp(tablename, tabledefns[i].name) == 0) {
	    if (match_keyword(car(body), K_ADD)) {
		body = cdr(body);
		reset = FALSE;
	    }
	    allocate_table(i, reset);
	    add_to_table(formsym, i, body);
	    found = TRUE;
	    break;
	}
    }
    if (!found)
      read_warning( "Undefined table `%s'", tablename);
}

/* Given a table and a list of value-setting clauses, fill in the table. */

#define INDEXP(typ, x) \
  ((typ == UTYP) ? utypep(x) : ((typ == MTYP) ? mtypep(x) : ttypep(x)))

#define nonlist(x) (!consp(x) && x != lispnil)

#define CHECK_INDEX_1(tbl, x)  \
  if (!INDEXP(tabledefns[tbl].index1, (x))) {  \
      read_warning("table %s index 1 has wrong type",  \
		   tabledefns[tbl].name);  \
      return;  \
  }

#define CHECK_INDEX_2(tbl, x)  \
  if (!INDEXP(tabledefns[tbl].index2, (x))) {  \
      read_warning("table %s index 2 has wrong type",  \
		   tabledefns[tbl].name);  \
      return;  \
  }

#define CHECK_VALUE(tbl, x)  \
  if (!numberp(x)) {  \
      read_warning("table %s value is not a number",  \
		   tabledefns[tbl].name);  \
      return;  \
  } \
  { int checknum = c_number(x);  \
    if (!between(tabledefns[tbl].lo, checknum, tabledefns[tbl].hi)) {  \
	read_warning("table %s value %d not within bounds %d to %d",  \
		     tabledefns[tbl].name, checknum, tabledefns[tbl].lo, tabledefns[tbl].hi);  \
	return;  \
    }  \
  }

#define CHECK_LISTS(tablename, lis1, lis2)  \
  if (consp(lis2)  \
      && !list_lengths_match(lis1, lis2, "table", tablename))  {  \
      return;  \
  }


static void
add_to_table(tablename, tbl, clauses)
int tbl;
Obj *tablename, *clauses;
{
    int i, num, lim1, lim2;
    Obj *clause, *indexes1, *indexes2, *values;

    lim1 = numtypes_from_index_type(tabledefns[tbl].index1);
    lim2 = numtypes_from_index_type(tabledefns[tbl].index2);
    for ( ; clauses != lispnil; clauses = cdr(clauses)) {
	clause = car(clauses);
	switch (clause->type) {
	  case SYMBOL:
	    clause = eval_symbol(clause);
	    TYPECHECK(numberp, clause, "table clause does not eval to number");
	    /* Now treat it as a number. */
	  case NUMBER:
	    /* A constant value for the table - blast over everything. */
	    CHECK_VALUE(tbl, clause);
	    num = c_number(clause);
	    /* Make sure the table is allocated first. */
	    if (tabledefns[tbl].table == NULL)
	      allocate_table(tbl, TRUE);
	    for (i = 0; i < lim1 * lim2; ++i)
	      (*(tabledefns[tbl].table))[i] = num;
	    break;
	  case CONS:
	    /* Evaluate the three parts of a clause. */
	    indexes1 = eval(car(clause));
	    indexes2 = eval(cadr(clause));
	    values = eval(caddr(clause));
	    if (cdr(cddr(clause)) != lispnil) {
		sprintlisp(readerrbuf, clause);
		read_warning("In table `%s', extra junk at end of clause `%s', ignoring",
			     c_string(tablename), readerrbuf);
	    }
	    interp_one_clause(tablename, tbl, lim1, lim2,
			      indexes1, indexes2, values);
	    break;
	  case STRING:
	    break; /* error? */
	  default:
	    /* who knows? */
	    break;
	}
    }
}

static void
interp_one_clause(tablename, tbl, lim1, lim2, indexes1, indexes2, values)
Obj *tablename, *indexes1, *indexes2, *values;
int tbl, lim1, lim2;
{
    int i, j, num;
    Obj *tmp1, *tmp2, *in1, *in2, *value, *subvalue;

    if (nonlist(indexes1)) {
	CHECK_INDEX_1(tbl, indexes1);
	i = c_number(indexes1);
	if (nonlist(indexes2) ) {
	    CHECK_INDEX_2(tbl, indexes2);
	    j = c_number(indexes2);
	    value = values;
	    CHECK_VALUE(tbl, value);
	    num = c_number(value);
	    (*(tabledefns[tbl].table))[lim2 * i + j] = num;
	} else {
	    CHECK_LISTS(tablename, indexes2, values);
	    for_all_list(indexes2, tmp2) {
		in2 = car(tmp2);
		CHECK_INDEX_2(tbl, in2);
		j = c_number(in2);
		value = (consp(values) ? car(values) : values);
		CHECK_VALUE(tbl, value);
		num = c_number(value);
		(*(tabledefns[tbl].table))[lim2 * i + j] = num;
		if (consp(values))
		  values = cdr(values);
	    }
	}
    } else {
	CHECK_LISTS(tablename, indexes1, values);
	for_all_list(indexes1, tmp1) {
	    in1 = car(tmp1);
	    CHECK_INDEX_1(tbl, in1);
	    i = c_number(in1);
	    value = (consp(values) ? car(values) : values);
	    if (nonlist(indexes2)) {
		CHECK_INDEX_2(tbl, indexes2);
		j = c_number(indexes2);
		CHECK_VALUE(tbl, value);
		num = c_number(value);
		(*(tabledefns[tbl].table))[lim2 * i + j] = num;
	    } else {
		if (nonlist(value)) {
		    subvalue = value;
		    CHECK_VALUE(tbl, subvalue);
		    num = c_number(subvalue);
		    for_all_list(indexes2, tmp2) {
			in2 = car(tmp2);
			CHECK_INDEX_2(tbl, in2);
			j = c_number(in2);
			(*(tabledefns[tbl].table))[lim2 * i + j] = num;
		    }
		} else {
		    CHECK_LISTS(tablename, indexes2, value);
		    for_all_list(indexes2, tmp2) {
			in2 = car(tmp2);
			CHECK_INDEX_2(tbl, in2);
			j = c_number(in2);
			subvalue = car(value);
			CHECK_VALUE(tbl, subvalue);
			num = c_number(subvalue);
			(*(tabledefns[tbl].table))[lim2 * i + j] = num;
			value = cdr(value);
		    }
		}
	    }
	    if (consp(values))
	      values = cdr(values);
	}
    }
}

/* Set the binding of an existing known variable. */

static void
interp_variable(form, isnew)
Obj *form;
int isnew;
{
    int i, numval;
    Obj *var = cadr(form);
    Obj *val = eval(caddr(form));
    char *name;

    SYNTAX(form, symbolp(var), "variable is not a symbol");
    name = c_string(var);

    if (isnew) {
	if (boundp(var))
	  read_warning("Symbol `%s' has been bound already, overwriting", name);
	setq(var, val);
    } else {
	/* Look through the set of defined globals. */
	for (i = 0; vardefns[i].name != 0; ++i) {
	    if (strcmp(name, vardefns[i].name) == 0) {
		if (vardefns[i].intgetter != NULL) {
		    TYPECHECK(numberishp, val, "is not a number or type");
		    numval = c_number(val);
		    if (!between(vardefns[i].lo, numval, vardefns[i].hi)) {
			read_warning("global %s value %d not between %d and %d",
				     vardefns[i].name, numval,
				     vardefns[i].lo, vardefns[i].hi);
			/* Let it pass through, at least for now. */
		    }
		    (*(vardefns[i].intsetter))(numval);
		} else if (vardefns[i].strgetter != NULL) {
		    TYPECHECK(stringp, val, "is not a string");
		    (*(vardefns[i].strsetter))(c_string(val));
		} else if (vardefns[i].objgetter != NULL) {
		    (*(vardefns[i].objsetter))(val);
		} else {
		    abort();
		}
		return;
	    }
	}
	/* Try as a random symbol. */
	if (boundp(var)) {
	    setq(var, val);
	    return;
	}
	/* Out of luck. */
	read_warning("Can't set unknown global named `%s'", name);
    }
}

static void
undefine_variable(form)
Obj *form;
{
    Obj *var = cadr(form);

    if (!symbolp(var)) {
	read_warning("Can't undefine a non-symbol!");
	return;
    }
    makunbound(var);
}

/* General function to augment types. */

static void
add_properties(form)
Obj *form;
{
    Obj *rest, *types, *prop, *values;

    rest = cdr(form);
    types = eval(car(rest));
    rest = cdr(rest);
    prop = car(rest);
    SYNTAX(form, symbolp(prop), "not a property name in third position");
    rest = cdr(rest);
    SYNTAX(form, rest != lispnil, "no property values supplied");
    values = eval(car(rest));
    /* Complain about, but ignore, extra things. */
    if (cdr(rest) != lispnil) {
	sprintlisp(readerrbuf, form);
	read_warning("Extra junk at the end of `%s', ignoring", readerrbuf);
    }
    if (utypep(types) || (consp(types) && utypep(car(types)))) {
	add_to_utypes(types, prop, values);
    } else if (mtypep(types) || (consp(types) && mtypep(car(types)))) {
	add_to_mtypes(types, prop, values);
    } else if (ttypep(types) || (consp(types) && ttypep(car(types)))) {
	add_to_ttypes(types, prop, values);
    } else {
	sprintlisp(readerrbuf, form);
	read_warning("No types to add to in `%s'", readerrbuf);
    }
}

/* Compare a list of types with a list of values, complain if
   they don't match up. */

static int
list_lengths_match(types, values, formtype, form)
Obj *types, *values, *form;
char *formtype;
{
    if (length(types) != length(values)) {
	sprintlisp(spbuf, form);
	read_warning("Lists of differing lengths (%d vs %d) in %s `%s'",
		     length(types), length(values), formtype, spbuf);
	return FALSE;
    }
    return TRUE;
}

static void
add_to_utypes(types, prop, values)
Obj *types, *prop, *values;
{
    char *propname = c_string(prop);
    Obj *lis1, *lis2;

    if (utypep(types)) {
	set_utype_property(types->v.num, propname, values);
    } else if (consp(types)) {
	if (consp(values)) {
	    if (!list_lengths_match(types, values, "utype property", prop))
	      return;
	    for_both_lists(types, values, lis1, lis2) {
		TYPECHECK(utypep, car(lis1), "not a unit type");
		if (!set_utype_property(car(lis1)->v.num, propname, car(lis2)))
		  break;
	    }
	} else {
	    for_all_list(types, lis1) {
		TYPECHECK(utypep, car(lis1), "not a unit type");
		if (!set_utype_property(car(lis1)->v.num, propname, values))
		  break;
	    }
	}
    }
}

static void
add_to_mtypes(types, prop, values)
Obj *types, *prop, *values;
{
    Obj *lis1, *lis2;

    if (mtypep(types)) {
	fill_in_mtype(types->v.num,
		      cons(cons(prop, cons(values, lispnil)), lispnil));
    } else if (consp(types)) {
	if (consp(values)) {
	    if (!list_lengths_match(types, values, "mtype property", prop))
	      return;
	    for_both_lists(types, values, lis1, lis2) {
		TYPECHECK(mtypep, car(lis1), "not a unit type");
		fill_in_mtype(car(lis1)->v.num,
			      cons(cons(prop, cons(car(lis2), lispnil)),
				   lispnil));
	    }
	} else {
	    for_all_list(types, lis1) {
		TYPECHECK(mtypep, car(lis1), "not a material type");
		fill_in_mtype(car(lis1)->v.num,
			      cons(cons(prop, cons(values, lispnil)),
				   lispnil));
	    }
	}
    }
}

static void
add_to_ttypes(types, prop, values)
Obj *types, *prop, *values;
{
    Obj *lis1, *lis2;

    if (ttypep(types)) {
	fill_in_ttype(types->v.num,
		      cons(cons(prop, cons(values, lispnil)), lispnil));
    } else if (consp(types)) {
	if (consp(values)) {
	    if (!list_lengths_match(types, values, "ttype property", prop))
	      return;
	    for_both_lists(types, values, lis1, lis2) {
		TYPECHECK(ttypep, car(lis1), "not a terrain type");
		fill_in_ttype(car(lis1)->v.num,
			      cons(cons(prop, cons(car(lis2), lispnil)),
				   lispnil));
	    }
	} else {
	    for_all_list(types, lis1) {
		TYPECHECK(ttypep, car(lis1), "not a terrain type");
		fill_in_ttype(car(lis1)->v.num,
			      cons(cons(prop, cons(values, lispnil)),
				   lispnil));
	    }
	}
    }
}

#endif /* n SPECIAL */

/* Interpret a world-specifying form. */

static void
interp_world(form)
Obj *form;
{
    int numval;
    Obj *props, *bdg, *propval;
    char *propname;

    props = cdr(form);
    if (symbolp(car(props))) {
	/* This is the id of the world (eventually). */
	props = cdr(props);
    }
    if (numberp(car(props))) {
    	set_world_circumference(c_number(car(props)), TRUE);
	props = cdr(props);
    }
    for ( ; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, propval);
	if (numberp(propval))
	  numval = c_number(propval);
	switch (keyword_code(propname)) {
	  case K_CIRCUMFERENCE:
	    set_world_circumference(numval, TRUE);
	    break;
	  case K_DAY_LENGTH:
	    world.daylength = numval;
	    break;
	  case K_YEAR_LENGTH:
	    world.yearlength = numval;
	    break;
	  case K_AXIAL_TILT:
	    world.axialtilt = numval;
	    break;
	  default:
	    unknown_property("world", "", propname);
	}
    }
}

/* Only one area, of fixed size.  Created anew if shape/size is supplied, else
   just modified. */

static void
interp_area(form)
Obj *form;
{
    int newarea = FALSE, newwidth = 0, newheight = 0, numval;
    Obj *props, *subprop, *bdg, *propval, *rest;
    char *propname;

    props = cdr(form);
    /* (eventually this will be an id or name) */
    if (symbolp(car(props))) {
	props = cdr(props);
	newarea = TRUE;
    }
    /* Collect the width of the area. */
    if (numberp(car(props))) {
	newwidth = newheight = c_number(car(props));
    	if (area.fullwidth == 0)
	  newarea = TRUE;
	if (area.fullwidth > 0 && area.fullwidth != newwidth)
	  read_warning("weird areas - %d vs %d", area.fullwidth, newwidth);
	props = cdr(props);
    }
    /* Collect the height of the area. */
    if (numberp(car(props))) {
	newheight = c_number(car(props));
    	if (area.fullheight == 0)
	  newarea = TRUE;
	if (area.fullheight > 0 && area.fullheight != newheight)
	  read_warning("weird areas - %d vs %d", area.fullheight, newheight);
	props = cdr(props);
    }
    /* See if we're restricting ourselves to a piece of a larger area. */
    if (consp(car(props))
        && match_keyword(car(car(props)), K_RESTRICT)) {
        subprop = cdr(car(props));
        if (numberp(car(subprop))) {
	    area.fullwidth = c_number(car(subprop));
	    subprop = cdr(subprop);
	    TYPECHECK(numberp, car(subprop), "restriction parm not a number");
	    area.fullheight = c_number(car(subprop));
	    subprop = cdr(subprop);
	    TYPECHECK(numberp, car(subprop), "restriction parm not a number");
	    area.fullx = c_number(car(subprop));
	    subprop = cdr(subprop);
	    TYPECHECK(numberp, car(subprop), "restriction parm not a number");
	    area.fully = c_number(car(subprop));
        } else if (match_keyword(car(subprop), K_RESET)) {
	    area.fullwidth = area.fullheight = 0;
	    area.fullx = area.fully = 0;
        } else {
	    syntax_error(car(props), "not 4 numbers or \"reset\"");
	    return;
        }
	props = cdr(props);        
    }
    /* If this is setting the area's shape for the first time,
       actually do it. */
    if (newarea)
      set_area_shape(newwidth, newheight, TRUE);
    for ( ; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, propval);
	if (numberp(propval))
	  numval = c_number(propval);
	rest = cdr(bdg);
	switch (keyword_code(propname)) {
	  case K_WIDTH:
	    /* Note that this may fail if the height has to change at
	       at the same time. */
	    set_area_shape(numval, area.height, TRUE);
	    break;
	  case K_HEIGHT:
	    /* Note that this may fail if the width has to change at
	       at the same time. */
	    set_area_shape(area.width, numval, TRUE);
	    break;
	  case K_LATITUDE:
	    area.latitude = numval;
	    break;
	  case K_LONGITUDE:
	    area.longitude = numval;
	    break;
	  case K_CELL_WIDTH:
	    area.cellwidth = numval;
	    break;
	  case K_TERRAIN:
	    fill_in_terrain(rest);
	    break;
	  case K_AUX_TERRAIN:
	    fill_in_aux_terrain(rest);
	    break;
	  case K_PEOPLE_SIDES:
	    fill_in_people_sides(rest);
	    break;
	  case K_FEATURES:
	    fill_in_features(rest);
	    break;
	  case K_ELEVATIONS:
	    fill_in_elevations(rest);
	    break;
	  case K_MATERIAL:
	    fill_in_cell_material(rest);
	    break;
	  case K_TEMPERATURES:
	    fill_in_temperatures(rest);
	    break;
	  case K_WINDS:
	    fill_in_winds(rest);
	    break;
	  case K_CLOUDS:
	    fill_in_clouds(rest);
	    break;
	  case K_CLOUD_BOTTOMS:
	    fill_in_cloud_bottoms(rest);
	    break;
	  case K_CLOUD_HEIGHTS:
	    fill_in_cloud_heights(rest);
	    break;
	  default:
	    unknown_property("area", "", propname);
	}
    }
}

/* The general plan of reading is similar for all layers - create a blank
   layer if none allocated, then call read_layer and pass a function that will
   actually put a value into a cell of the layer.  We need to define those
   functions because most of the setters are macros, and because we can do
   some extra error checking. */

/* Read the area terrain. */

static void
fill_in_terrain(contents)
Obj *contents;
{
    /* We must have some terrain types or we're going to lose bigtime. */
    if (numttypes == 0)
      load_default_game();
    numbadterrain = 0;
    /* Make sure the terrain layer exists. */
    if (!terrain_defined())
      allocate_area_terrain();
    read_layer(contents, fn_set_terrain_at);
    if (numbadterrain > 0) {
	read_warning("%d occurrences of unknown terrain in all",
		     numbadterrain);
    }
}

/* Read a layer of auxiliary terrain. */

static void
fill_in_aux_terrain(contents)
Obj *contents;
{
    int t;
    Obj *typesym = car(contents), *typeval;

    if (symbolp(typesym) && ttypep(typeval = eval(typesym))) {
	t = c_number(typeval);
	contents = cdr(contents);
	/* Make sure aux terrain space exists, but don't overwrite. */
	allocate_area_aux_terrain(t);
	tmpttype = t;
	read_layer(contents, fn_set_aux_terrain_at);
	/* Ensure that borders and connections have all their bits
	   correctly set. */
	patch_linear_terrain(t);
    } else {
	/* not a valid aux terrain type */
    }
}

static void
fill_in_people_sides(contents)
Obj *contents;
{
    /* Make sure the people sides layer exists. */
    allocate_area_people_sides();
    read_layer(contents, fn_set_people_side_at);
}

/* This should recompute size etc of all these features too. */

static void
fill_in_features(contents)
Obj *contents;
{
    int fid;
    Obj *featspec, *flist;
    Feature *feat;

    init_features();
    for (flist = car(contents); flist != lispnil; flist = cdr(flist)) {
	featspec = car(flist);
	fid = 0;
	feat = NULL;
	if (numberp(car(featspec))) {
	    fid = c_number(car(featspec));
	    feat = find_feature(fid);
	    featspec = cdr(featspec);
	}
	if (feat == NULL) {
	    feat = create_feature(c_string(car(featspec)),
				  c_string(cadr(featspec)));
	    if (fid > 0)
	      feat->id = fid;
	} else {
	    /* complain if data doesn't match? */
	}
    }
    read_layer(cdr(contents), fn_set_raw_feature_at);
}

static void
fill_in_elevations(contents)
Obj *contents;
{
    /* Make sure the elevation layer exists. */
    allocate_area_elevations();
    read_layer(contents, fn_set_elevation_at);
}

static void
fill_in_cell_material(contents)
Obj *contents;
{
    int m;
    Obj *typesym = car(contents), *typeval;

    if (symbolp(typesym) && mtypep(typeval = eval(typesym))) {
	m = c_number(typeval);
	contents = cdr(contents);
	/* Make sure this material layer exists. */
	allocate_area_material(m);
	tmpmtype = m;
	read_layer(contents, fn_set_material_at);
    } else {
	/* not a valid material type spec, should warn */
    }
}

static void
fill_in_temperatures(contents)
Obj *contents;
{
    /* Make sure the temperature layer exists. */
    allocate_area_temperatures();
    read_layer(contents, fn_set_temperature_at);
}

static void
fill_in_winds(contents)
Obj *contents;
{
    /* Make sure the winds layer exists. */
    allocate_area_winds();
    read_layer(contents, fn_set_raw_wind_at);
}

static void
fill_in_clouds(contents)
Obj *contents;
{
    /* Make sure the clouds layer exists. */
    allocate_area_clouds();
    read_layer(contents, fn_set_raw_cloud_at);
}

static void
fill_in_cloud_bottoms(contents)
Obj *contents;
{
    /* Make sure the cloud bottoms layer exists. */
    allocate_area_cloud_bottoms();
    read_layer(contents, fn_set_raw_cloud_bottom_at);
}

static void
fill_in_cloud_heights(contents)
Obj *contents;
{
    /* Make sure the cloud heights layer exists. */
    allocate_area_cloud_heights();
    read_layer(contents, fn_set_raw_cloud_height_at);
}

/* This is like init_warning, but with a module and line(s) glued in. */
/* (This may seem like a strange position for this routine, but it's
   necessary to accommodate a Mac 16-bit PC-relative addressing limit;
   in the middle of the file, it's close enough to all of its callers.
   Nasty remarks about Mac runtime architecture to Dev:Null please.) */

void
#ifdef __STDC__
read_warning(char *str, ...)
#else
read_warning(str, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *str;
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
#endif
{
    char buf[BUFSIZE];

    module_and_line(curmodule, buf);
#ifdef __STDC__
    {
	va_list ap;

	va_start(ap, str);
	vtprintf(buf, str, ap);
	va_end(ap);
    }
#else
    tprintf(buf, str, a1, a2, a3, a4, a5, a6, a7, a8, a9);
#endif
    low_init_warning(buf);
}

/* Interpret a side spec. */

static void
interp_side(form, side)
Obj *form;
Side *side;
{
    int id = -1;
    Obj *ident = lispnil, *sym = lispnil, *props = cdr(form);

    /* See if there's an optional side identifier and pick it off. */
    if (!listp(car(props))) {
	ident = car(props);
	props = cdr(props);
    }
    if (numberp(ident)) {
	id = c_number(ident);
	side = side_n(id);
    } else if (symbolp(ident)) {
	if (boundp(ident) && numberp(symbol_value(ident))) {
	    id = c_number(symbol_value(ident));
	    side = side_n(id);
	} else {
	    sym = ident;
	}
    } else if (stringp(ident)) {
	side = find_side_by_name(c_string(ident));
    } else {
	/* We want to create a new side. */
    }
    if (symbolp(car(props))) {
	sym = car(props);
	props = cdr(props);
    }
    if (side == NULL) {
	side = create_side();
	/* Replace with the read-in id if it was given. */
	if (id >= 0)
	  side->id = id;
    }
    if (sym != lispnil) {
	/* Record the symbol as going with this side. */
	/* (should warn if existing symbol being overwritten?) */
	side->symbol = sym;
	/* Also let it eval to the side's id. */
	setq(sym, new_number(side->id));
    }
    /* Apply the current side defaults first. */
    fill_in_side(side, side_defaults, FALSE);
    /* Now fill in from the explicitly specified properties. */
    fill_in_side(side, props, FALSE);
    Dprintf("  Got side %s\n", side_desig(side));
}

/* Given a side, fill in some of its properties. */

void
fill_in_side(side, props, userdata)
Side *side;
Obj *props;
int userdata;
{
    int numval = 0, permission;
    char *propname, *strval = NULL;
    Obj *bdg, *rest, *propval;

#define CHECK_PERMISSION  \
	    if (userdata) {  \
		permission = FALSE;  \
		break;  \
	    }

    for (; props != lispnil; props = cdr(props)) {
	permission = TRUE;
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, propval);
	if (symbolp(propval))
	  propval = eval(propval);
	if (numberp(propval))
	  numval = c_number(propval);
	if (stringp(propval))
	  strval = c_string(propval);
	rest = cdr(bdg);
	switch (keyword_code(propname)) {
	  case K_NAME:
	    check_name_uniqueness(side, strval, "name");
	    side->name = strval;
	    break;
	  case K_LONG_NAME:
	    check_name_uniqueness(side, strval, "long name");
	    side->longname = strval;
	    break;
	  case K_SHORT_NAME:
	    check_name_uniqueness(side, strval, "short name");
	    side->shortname = strval;
	    break;
	  case K_NOUN:
	    check_name_uniqueness(side, strval, "noun");
	    side->noun = strval;
	    break;
	  case K_PLURAL_NOUN:
	    check_name_uniqueness(side, strval, "plural noun");
	    side->pluralnoun = strval;
	    break;
	  case K_ADJECTIVE:
	    check_name_uniqueness(side, strval, "adjective");
	    side->adjective = strval;
	    break;
          /* Several synonyms are allowed for specifying colors. */
	  case K_COLOR:
	    side->colorscheme = strval;
	    break;
	  case K_EMBLEM_NAME:
	    side->emblemname = strval;
	    break;
	  case K_UNIT_NAMERS:
	    /* Allocate space if not already done so. */
	    if (side->unitnamers == NULL)
	      side->unitnamers = (char **) xmalloc(numutypes * sizeof(char *));
	    merge_unit_namers(side, rest);
	    break;
	  case K_FEATURE_NAMERS:
	    /* (should merge instead of bashing) */
	    side->featurenamers = rest;
	    break;
	  case K_NAMES_LOCKED:
	    side->nameslocked = numval;
	    break;
	  case K_CLASS:
	    CHECK_PERMISSION;
	    side->sideclass = strval;
	    break;
	  case K_SELF_UNIT:
	    CHECK_PERMISSION;
	    side->self_unit_id = numval;
	    break;
	  case K_CONTROLLED_BY:
	    side->controlled_by_id = numval;
	    break;
	  case K_TRUSTS:
	    interp_side_value_list(side->trusts, rest);
	    break;
	  case K_TRADES:
	    interp_side_value_list(side->trades, rest);
	    break;
	  case K_START_WITH:
	    CHECK_PERMISSION;
	    if (side->startwith == NULL)
	      side->startwith = (short *) xmalloc(numutypes * sizeof(short));
	    interp_utype_value_list(side->startwith, rest);
	    break;
	  case K_NEXT_NUMBERS:
	    CHECK_PERMISSION;
	    if (side->counts == NULL)
	      side->counts = (short *) xmalloc(numutypes * sizeof(short));
	    interp_utype_value_list(side->counts, rest);
	    break;
	  case K_TECH:
	    CHECK_PERMISSION;
	    if (side->tech == NULL)
	      side->tech = (short *) xmalloc(numutypes * sizeof(short));
	    interp_utype_value_list(side->tech, rest);
	    break;
	  case K_INIT_TECH:
	    CHECK_PERMISSION;
	    if (side->inittech == NULL)
	      side->inittech = (short *) xmalloc(numutypes * sizeof(short));
	    interp_utype_value_list(side->inittech, rest);
	    break;
	  case K_DEFAULT_DOCTRINE:
	    read_general_doctrine(side, rest);
	    break;
	  case K_DOCTRINES:
	    read_utype_doctrine(side, rest);
	    break;
	  case K_DOCTRINES_LOCKED:
	    side->doctrines_locked = numval;
	    break;
	  case K_TERRAIN_VIEW:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_terrain_view);
	    break;
	  case K_TERRAIN_VIEW_DATES:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_terrain_view_date);
	    break;
	  case K_AUX_TERRAIN_VIEW:
	    CHECK_PERMISSION;
	    read_aux_terrain_view_layer(side, rest, fn_set_aux_terrain_view);
	    break;
	  case K_AUX_TERRAIN_VIEW_DATES:
	    CHECK_PERMISSION;
	    read_aux_terrain_view_layer(side, rest, fn_set_aux_terrain_view_date);
	    break;
	  case K_UNIT_VIEW:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_unit_view);
	    break;
	  case K_UNIT_VIEW_DATES:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_unit_view_date);
	    break;
	  case K_MATERIAL_VIEW:
	    CHECK_PERMISSION;
	    read_material_view_layer(side, rest, fn_set_material_view);
	    break;
	  case K_MATERIAL_VIEW_DATES:
	    CHECK_PERMISSION;
	    read_material_view_layer(side, rest, fn_set_material_view_date);
	    break;
	  case K_TEMPERATURE_VIEW:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_temp_view);
	    break;
	  case K_TEMPERATURE_VIEW_DATES:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_temp_view_date);
	    break;
	  case K_CLOUD_VIEW:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_cloud_view);
	    break;
	  case K_CLOUD_BOTTOM_VIEW:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_cloud_bottom_view);
	    break;
	  case K_CLOUD_HEIGHT_VIEW:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_cloud_height_view);
	    break;
	  case K_CLOUD_VIEW_DATES:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_cloud_view_date);
	    break;
	  case K_WIND_VIEW:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_wind_view);
	    break;
	  case K_WIND_VIEW_DATES:
	    CHECK_PERMISSION;
	    read_view_layer(side, rest, fn_set_wind_view_date);
	    break;
	  case K_ACTIVE:
	    CHECK_PERMISSION;
	    side->ingame = numval;
	    break;
	  case K_EVER_ACTIVE:
	    CHECK_PERMISSION;
	    side->everingame = numval;
	    break;
	  case K_PRIORITY:
	    CHECK_PERMISSION;
	    side->priority = numval;
	    break;
	  case K_STATUS:
	    CHECK_PERMISSION;
	    side->status = numval;
	    break;
	  case K_SCORES:
	    CHECK_PERMISSION;
	    /* The data in this slot will be interpreted later,
	       after scorekeepers exist. */
	    side->rawscores = rest;
	    break;
	  case K_WILLING_TO_DRAW:
	    side->willingtodraw = numval;
	    break;
	  case K_FINISHED_TURN:
	    side->finishedturn = numval;
	    break;
	  case K_TURN_TIME_USED:
	    CHECK_PERMISSION;
	    side->turntimeused = numval;
	    break;
	  case K_TOTAL_TIME_USED:
	    CHECK_PERMISSION;
	    side->totaltimeused = numval;
	    break;
	  case K_TIMEOUTS:
	    CHECK_PERMISSION;
	    side->timeouts = numval;
	    break;
	  case K_TIMEOUTS_USED:
	    CHECK_PERMISSION;
	    side->timeoutsused = numval;
	    break;
	  case K_ADVANTAGE:
	    CHECK_PERMISSION;
	    side->advantage = numval;
	    break;
	  case K_ADVANTAGE_MIN:
	    CHECK_PERMISSION;
	    side->minadvantage = numval;
	    break;
	  case K_ADVANTAGE_MAX:
	    CHECK_PERMISSION;
	    side->maxadvantage = numval;
	    break;
	  case K_PLAYER:
	    side->playerid = numval;
	    break;
	  case K_GAIN_COUNTS:
	    CHECK_PERMISSION;
	    interp_short_array(side->gaincounts, rest, numutypes * num_gain_reasons);
	    break;
	  case K_LOSS_COUNTS:
	    CHECK_PERMISSION;
	    interp_short_array(side->losscounts, rest, numutypes * num_loss_reasons);
	    break;
	  case K_ATTACK_STATS:
	    CHECK_PERMISSION;
	    interp_atkstats_list(side, rest);
	    break;
	  case K_HIT_STATS:
	    CHECK_PERMISSION;
	    interp_hitstats_list(side, rest);
	    break;
	  case K_AI_DATA:
	    /* The data in this slot will be interpreted when the AI is
	       ready to look at it. */
	    side->aidata = rest;
	    break;
	  case K_UI_DATA:
	    /* The data in this slot will be interpreted when the interface
	       is ready to look at it. */
	    side->uidata = rest;
	    break;
	  default:
	    unknown_property("side", side_desig(side), propname);
	}
	if (!permission) {
	    read_warning("No permission to set property `%s'", propname);
	}
    }
    /* Calculate the plural form of the noun for units if it was not
       explicitly defined. */
    if (side->noun != NULL && side->pluralnoun == NULL) {
	side->pluralnoun = copy_string(plural_form(side->noun));
    }
}

static void
check_name_uniqueness(side, str, kind)
Side *side;
char *str, *kind;
{
    if (name_in_use(side, str)) {
	init_warning("Side %s `%s' is already in use", kind, str);
    }
}

/* Given a list of (utype str) pairs, set unit namers appropriately. */

static void
merge_unit_namers(side, lis)
Side *side;
Obj *lis;
{
    int u, u2 = 0;
    Obj *rest, *elt, *types, *namer;

    for_all_list(lis, rest) {
	elt = car(rest);
	if (consp(elt)) {
	    types = eval(car(elt));
	    namer = cadr(elt);
	    if (utypep(types) && (stringp(namer) || symbolp(namer))) {
		u = c_number(types);
		side->unitnamers[u] = c_string(namer);
	    } else {
		syntax_error(elt, "garbled unit namer");
	    }
	} else if (stringp(elt) || symbolp(elt)) {
	    if (u2 < numutypes) {
		side->unitnamers[u2++] = c_string(elt);
	    } else {
		read_warning("more unit namers than unit types, ignoring extra");
	    }
	} else {
	    syntax_error(elt, "not a valid unit namer");
	}
    }
}

static void
interp_side_value_list(arr, lis)
short *arr;
Obj *lis;
{
    int s = 0;
    Obj *rest, *head;

    if (arr == NULL)
      run_error("null array for side value list?");
    for_all_list(lis, rest) {
    	head = car(rest);
	if (numberp(head)) {
	    if (s > g_sides_max())
	      break;
	    arr[s++] = c_number(head);
	} else if (symbolp(head)) {
	    int s2 = c_number(eval(head));

	    if (between(1, s2, g_sides_max()))
	      arr[s2] = TRUE;
	    else
	      read_warning("bad side spec");
	} else if (consp(head)) {
	    Obj *sidespec = car(head);
	    int s2, val2 = c_number(cadr(head));

	    if (numberp(sidespec) || symbolp(sidespec)) {
		s2 = c_number(eval(sidespec));
		if (between(1, s2, g_sides_max()))
		  arr[s2] = val2;
		else
		  read_warning("bad side spec");
	    } else if (consp(sidespec)) {
	    	read_warning("not implemented");
	    } else {
	    	read_warning("not implemented");
	    }
	} else {
	    read_warning("not implemented");
	}
    }
}

static void
interp_atkstats_list(side, lis)
Side *side;
Obj *lis;
{
    int u, u2;
    Obj *rest, *head;

    for_all_list(lis, rest) {
    	head = car(rest);
    	if (consp(head) && symbolp(car(head))) {
	    u = utype_from_symbol(car(head));
	    if (!is_unit_type(u)) {
		init_warning("non unit type");
		continue;
	    }
	    if (side->atkstats[u] == NULL)
	      side->atkstats[u] = (long *) xmalloc(numutypes * sizeof(long));
	    interp_long_array(side->atkstats[u], cdr(head), numutypes);
	}
    }
}

static void
interp_hitstats_list(side, lis)
Side *side;
Obj *lis;
{
    int u, u2;
    Obj *rest, *head;

    for_all_list(lis, rest) {
    	head = car(rest);
    	if (consp(head) && symbolp(car(head))) {
	    u = utype_from_symbol(car(head));
	    if (!is_unit_type(u)) {
		init_warning("non unit type");
		continue;
	    }
	    if (side->hitstats[u] == NULL)
	      side->hitstats[u] = (long *) xmalloc(numutypes * sizeof(long));
	    interp_long_array(side->hitstats[u], cdr(head), numutypes);
	}
    }
}

/* Helper function to init side view from rle encoding. */
/* (should move these into side.c?) */

static void
fn_set_terrain_view(x, y, val)
int x, y, val;
{
    set_terrain_view(tmpside, x, y, val);
}

static void
fn_set_terrain_view_date(x, y, val)
int x, y, val;
{
    set_terrain_view_date(tmpside, x, y, val);
}

static void
fn_set_unit_view(x, y, val)
int x, y, val;
{
    set_unit_view(tmpside, x, y, val);
}

static void
fn_set_unit_view_date(x, y, val)
int x, y, val;
{
    set_unit_view_date(tmpside, x, y, val);
}

static void
fn_set_temp_view(x, y, val)
int x, y, val;
{
    set_temperature_view(tmpside, x, y, val);
}

static void
fn_set_temp_view_date(x, y, val)
int x, y, val;
{
    set_temperature_view_date(tmpside, x, y, val);
}

static void
fn_set_cloud_view(x, y, val)
int x, y, val;
{
    set_cloud_view(tmpside, x, y, val);
}

static void
fn_set_cloud_bottom_view(x, y, val)
int x, y, val;
{
    set_cloud_bottom_view(tmpside, x, y, val);
}

static void
fn_set_cloud_height_view(x, y, val)
int x, y, val;
{
    set_cloud_height_view(tmpside, x, y, val);
}

static void
fn_set_cloud_view_date(x, y, val)
int x, y, val;
{
    set_cloud_view_date(tmpside, x, y, val);
}

static void
fn_set_wind_view(x, y, val)
int x, y, val;
{
    set_wind_view(tmpside, x, y, val);
}

static void
fn_set_wind_view_date(x, y, val)
int x, y, val;
{
    set_wind_view_date(tmpside, x, y, val);
}

static void
read_view_layer(side, contents, setter)
Side *side;
Obj *contents;
void (*setter) PARAMS ((int x, int y, int val));
{
    if (g_see_all())
      return;
    init_view(side);
    tmpside = side;
    read_layer(contents, setter);
}

static void
fn_set_aux_terrain_view(x, y, val)
int x, y, val;
{
    /* Filter anything but the basic six bits. */
    val &= 0x3f;
    set_aux_terrain_view(tmpside, x, y, tmpttype, val);
}

static void
fn_set_aux_terrain_view_date(x, y, val)
int x, y, val;
{
    set_aux_terrain_view_date(tmpside, x, y, tmpttype, val);
}

static void
read_aux_terrain_view_layer(side, contents, setter)
Side *side;
Obj *contents;
void (*setter) PARAMS ((int x, int y, int val));
{
    int t;
    Obj *typesym = car(contents), *typeval;

    if (g_see_all())
      return;
    init_view(side);
    tmpside = side;
    if (!symbolp(typesym)) {
	return;
    }
    typeval = eval(typesym);
    if (!ttypep(typeval)) {
	return;
    }
    t = c_number(typeval);
    if (!t_is_cell(t)) {
	return;
    }
    contents = cdr(contents);
    tmpttype = t;
    read_layer(contents, setter);
}

static void
fn_set_material_view(x, y, val)
int x, y, val;
{
    set_material_view(tmpside, x, y, tmpmtype, val);
}

static void
fn_set_material_view_date(x, y, val)
int x, y, val;
{
    set_material_view_date(tmpside, x, y, tmpmtype, val);
}

static void
read_material_view_layer(side, contents, setter)
Side *side;
Obj *contents;
void (*setter) PARAMS ((int x, int y, int val));
{
    int m;
    Obj *typesym = car(contents), *typeval;

    if (all_see_all)
      return;
    init_view(side);
    tmpside = side;
    if (!symbolp(typesym)) {
	return;
    }
    typeval = eval(typesym);
    if (!mtypep(typeval)) {
	return;
    }
    m = c_number(typeval);
    contents = cdr(contents);
    tmpmtype = m;
    read_layer(contents, setter);
}

static void
read_general_doctrine(side, props)
Side *side;
Obj *props;
{
    int id = -1;
    char *name = NULL;
    Obj *ident = lispnil;
    Doctrine *doctrine = NULL;

    if (props == lispnil) {
	/* (should complain) */
	return;
    }
    ident = car(props);
    props = cdr(props);
    if (numberp(ident)) {
	id = c_number(ident);
	doctrine = find_doctrine(id);
    } else if (symbolp(ident) || stringp(ident)) {
	name = c_string(ident);
	doctrine = find_doctrine_by_name(name);
    } else {
	type_error(ident, "invalid doctrine reference");
	return;
    }
    if (doctrine == NULL) {
	doctrine = new_doctrine(id);
	doctrine->name = name;
    }
    fill_in_doctrine(doctrine, props);
    side->default_doctrine = doctrine;
}

/* Read doctrine info pertaining to a particular unit type. */

static void
read_utype_doctrine(side, lis)
Side *side;
Obj *lis;
{
    short arr[MAXUTYPES];
    int u, id;
    char *name;
    Obj *item, *rest, *ulist, *props, *ident;
    Doctrine *doctrine;
    
    for_all_list(lis, rest) {
	item = car(rest);
	ulist = car(item);
	props = cdr(item);
	for_all_unit_types(u)
	  arr[u] = FALSE;
	interp_utype_list(arr, ulist);
	ident = car(props);
	props = cdr(props);
	id = 0;
	name = NULL;
	if (numberp(ident)) {
	    id = c_number(ident);
	    doctrine = find_doctrine(id);
	} else if (symbolp(ident) || stringp(ident)) {
	    name = c_string(ident);
	    doctrine = find_doctrine_by_name(name);
	} else {
	    type_error(ident, "invalid doctrine reference");
	    return;
	}
	if (doctrine == NULL) {
	    doctrine = new_doctrine(id);
	    doctrine->name = name;
	}
	fill_in_doctrine(doctrine, props);
	for_all_unit_types(u) {
	    if (arr[u]) {
		if (side->udoctrine == NULL)
		  init_doctrine(side);
		side->udoctrine[u] = doctrine;
	    }
	}
    }
}

/* Interpret a form as a doctrine. */

static void
interp_doctrine(form)
Obj *form;
{
    int id = -1;
    char *name = NULL;
    Obj *ident = lispnil, *props = cdr(form);
    Doctrine *doctrine = NULL;

    /* Pick up an optional numeric or symbolic id. */
    if (props != lispnil) {
	if (!consp(car(props))) {
	    ident = car(props);
	    props = cdr(props);
	}
    }
    /* Use the id to find an existing doctrine. */
    if (numberp(ident)) {
	id = c_number(ident);
	doctrine = find_doctrine(id);
    }
    if (symbolp(ident)) {
	name = c_string(ident);
	doctrine = find_doctrine_by_name(name);
    }
    if (doctrine == NULL) {
	doctrine = new_doctrine(id);
	doctrine->name = name;
    }
    /* Give the symbol a binding so eval'ing works. */
    if (symbolp(ident))
      setq(ident, new_string(name));
    fill_in_doctrine(doctrine, props);
    Dprintf("  Got doctrine\n");
}

static void
fill_in_doctrine(doctrine, props)
Doctrine *doctrine;
Obj *props;
{
    int numval;
    char *propname;
    Obj *bdg, *val;

    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, val);
	if (numberp(val))
	  numval = c_number(val);
	switch (keyword_code(propname)) {
	  case K_EVER_ASK_SIDE:
	    doctrine->everaskside = numval;
	    break;
	  case K_CONSTRUCTION_RUN:
	    /* Ensure that we have something to write into. */
	    /* (assumes that numutypes already frozen - always true?) */
	    if (doctrine->construction_run == NULL)
	      doctrine->construction_run =
	        (short *) xmalloc (numutypes * sizeof(short));
	    interp_utype_value_list(doctrine->construction_run, cdr(bdg));
	    break;
	  case K_LOCKED:
	    doctrine->locked = numval;
	    break;
	  default:
	    unknown_property("doctrine", "", propname);
	}
    }
}

/* Interpret a form that defines a player. */

static void
interp_player(form)
Obj *form;
{
    int id = -1;
    Obj *ident = lispnil, *props = cdr(form);
    Player *player = NULL;

    if (props != lispnil) {
	if (!consp(car(props))) {
	    ident = car(props);
	    props = cdr(props);
	}
    }
    if (numberp(ident)) {
	id = c_number(ident);
	player = find_player(id);
    }
    if (player == NULL) {
	player = add_player();
    }
    if (id > 0)
      player->id = id;
    fill_in_player(player, props);
    Dprintf("  Got player %s\n", player_desig(player));
}

static void
fill_in_player(player, props)
Player *player;
Obj *props;
{
    char *propname, *strval;
    Obj *bdg, *propval;

    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, propval);
	if (stringp(propval))
	  strval = c_string(propval);
	switch (keyword_code(propname)) {
	  case K_NAME:
	    player->name = strval;
	    break;
	  case K_CONFIG_NAME:
	    player->configname = strval;
	    break;
	  case K_DISPLAY_NAME:
	    player->displayname = strval;
	    break;
	  case K_AI_TYPE_NAME:
	    player->aitypename = strval;
	    break;
	  case K_INITIAL_ADVANTAGE:
	    player->advantage = c_number(propval);
	    break;
	  case K_PASSWORD:
	    player->password = strval;
	    break;
	  default:
	    unknown_property("player", player_desig(player), propname);
	}
    }
    canonicalize_player(player);
}

/* Create and fill in an agreement, as specified by the form. */

static void
interp_agreement(form)
Obj *form;
{
    int id = 0;
    char *propname;
    Obj *props = cdr(form), *agid, *bdg, *val;
    Agreement *ag;

    agid = car(props);
    if (numberp(agid)) {
    	id = c_number(agid);
	/* should use the number eventually */
	props = cdr(props);
    }
    if (1 /* must create a new agreement object */) {
	ag = create_agreement(id);
	/* Fill in defaults for the slots. */
	ag->state = draft;  /* default for now */
	ag->drafters = NOSIDES;
	ag->proposers = NOSIDES;
	ag->signers = NOSIDES;
	ag->willing = NOSIDES;
	ag->knownto = NOSIDES;
    }
    /* Interpret the properties. */
    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, val);
	switch (keyword_code(propname)) {
	  case K_TYPE_NAME:
	    ag->typename = c_string(val);
	    break;
	  case K_NAME:
	    ag->name = c_string(val);
	    break;
	  case K_STATE:
	    ag->state = c_number(val);
	    break;
	  case K_TERMS:
	    ag->terms = val;
	    break;
	  case K_DRAFTERS:
	    break;
	  case K_PROPOSERS:
	    break;
	  case K_SIGNERS:
	    break;
	  case K_WILLING_TO_SIGN:
	    break;
	  case K_KNOWN_TO:
	    break;
	  case K_ENFORCEMENT:
	    ag->enforcement = c_number(val);
	    break;
	    break;
	  default:
	    unknown_property("agreement", "", propname);
	}
    }
}

static void
interp_unit_defaults(form)
Obj *form;
{
    int u, m, numval, wasnum, variablelength;
    Obj *props = form, *bdg, *val;
    char *propname;

    if (match_keyword(car(props), K_RESET)) {
	/* Reset all the tweakable defaults. */
	uxoffset = 0, uyoffset = 0;
	default_unit_side_number = -1;
	default_unit_origside_number = -1;
	default_unit_cp = -1;
	default_unit_hp = -1;
	default_unit_cxp = -1;
	default_unit_z = -1;
	default_transport_id = -1;
	default_unit_hook = lispnil;
	for_all_material_types(m)
	  default_supply[m] = -1;
	canaddmtype = FALSE;
	if (default_tooling != NULL) {
	    for_all_unit_types(u)
	      default_tooling[m] = -1;
	}
	props = cdr(props);
    }
    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, val);
	numval = 0;
	wasnum = FALSE;
	if (numberp(val)) {
	    numval = c_number(val);
	    wasnum = TRUE;
	}
	variablelength = FALSE;
	/* Note that not all unit slots can get default values. */
	switch (keyword_code(propname)) {
	  case K_AT:
	    uxoffset = numval;
	    uyoffset = c_number(caddr(bdg));
	    /* The property is not really variable-length, but mollify the error check. */
	    variablelength = TRUE;
	    break;
	  case K_S:
	    if (!wasnum)
	      numval = c_number(eval(val));
	    default_unit_side_number = numval;
	    break;
	  case K_OS:
	    if (!wasnum)
	      numval = c_number(eval(val));
	    default_unit_origside_number = numval;
	    break;
	  case K_CP:
	    default_unit_cp = numval;
	    break;
	  case K_HP:
	    default_unit_hp = numval;
	    break;
	  case K_CXP:
	    default_unit_cxp = numval;
	    break;
	  case K_M:
	    interp_mtype_value_list(default_supply, cdr(bdg));
	    variablelength = TRUE;
	    break;
	  case K_TP:
	    if (default_tooling == NULL)
	      default_tooling = (short *) xmalloc(numutypes * sizeof(short));
	    interp_utype_value_list(default_tooling, cdr(bdg));
	    variablelength = TRUE;
	    break;
	  case K_IN:
	    default_transport_id = numval;
	    break;
	  case K_PLAN:
	    /* (should fill in) */
	    variablelength = TRUE;
	    break;
	  case K_Z:
	    default_unit_z = numval;
	    break;
	  case K_X:
	    default_unit_hook = cdr(bdg);
	    variablelength = TRUE;
	    break;
	  default:
	    unknown_property("unit-defaults", "", propname);
	}
	if (!variablelength && cddr(bdg) != lispnil)
	  read_warning("Extra junk in a %s property, ignoring", propname);
    }
}

/* Try to find a unit type named by the string. */
/* (should move these elsewhere) */

int
utype_from_name(str)
char *str;
{
    char *tmpstr;
    int u;
    Obj *sym;

    for_all_unit_types(u) {
	if (strcmp(str, u_type_name(u)) == 0)
	  return u;
    }
    /* Try evaluating the symbol. */
    sym = intern_symbol(str);
    if (boundp(sym) && utypep(symbol_value(sym)))
      return c_number(symbol_value(sym));
    /* Try some less common possibilities. */
    for_all_unit_types(u) {
	tmpstr = u_short_name(u);
	if (tmpstr && strcmp(str, tmpstr) == 0)
	  return u;
    }
    for_all_unit_types(u) {
	tmpstr = u_long_name(u);
	if (tmpstr && strcmp(str, tmpstr) == 0)
	  return u;
    }
    return NONUTYPE;
}

/* Try to find a unit type named by the string. */

int
utype_from_symbol(sym)
Obj *sym;
{
    char *str, *tmpstr;
    int u;

    if (boundp(sym) && utypep(symbol_value(sym)))
      return c_number(symbol_value(sym));
    str = c_string(sym);
    for_all_unit_types(u) {
	if (strcmp(str, u_type_name(u)) == 0)
	  return u;
    }
    /* Try some less common possibilities. */
    for_all_unit_types(u) {
	tmpstr = u_short_name(u);
	if (tmpstr && strcmp(str, tmpstr) == 0)
	  return u;
    }
    for_all_unit_types(u) {
	tmpstr = u_long_name(u);
	if (tmpstr && strcmp(str, tmpstr) == 0)
	  return u;
    }
    return NONUTYPE;
}

/* This creates an individual unit and fills in data about it. */

static void
interp_unit(form)
Obj *form;
{
    int u, u2, m, tp_varies, numval, wasnum, nuid = 0, variablelength;
    int nusn = -1, nuosn = -1;
    char *propname;
    Obj *head = car(form), *props = cdr(form), *bdg, *val;
    Unit *unit, *unit2;
    extern int nextid;

    Dprintf("Reading a unit from ");
    Dprintlisp(form);
    Dprintf("\n");
    if (symbolp(head)) {
	u = utype_from_symbol(head);
     	if (u != NONUTYPE) {
	    unit = create_unit(u, FALSE);
	    canaddmtype = FALSE;
	    if (unit == NULL) {
		read_warning("Failed to create a unit, skipping the form");
		return;
	    }
	} else {
	    read_warning("\"%s\" not a known unit type, skipping the form",
			 c_string(head));
	    return;
	}
    } else if (stringp(head)) {
	unit = find_unit_by_name(c_string(head));
	if (unit == NULL) {
	    read_warning("No unit named \"%s\", skipping the form",
			 c_string(head));
	    return;
    	}
    } else if (numberp(head)) {
	unit = find_unit_by_number(c_number(head));
	if (unit == NULL) {
	    read_warning("No unit numbered %d, skipping the form",
			 c_number(head));
	    return;
    	}
    }
    /* At this point we're guaranteed to have a unit to work with. */
    /* Modify the unit according to current defaults. */
    if (default_unit_side_number >= 0)
      nusn = default_unit_side_number;
    if (default_unit_origside_number >= 0)
      nuosn = default_unit_origside_number;
    if (default_unit_cp >= 0)
      unit->cp = default_unit_cp;
    if (default_unit_hp >= 0)
      unit->hp = unit->hp2 = default_unit_hp;
    if (default_unit_cxp >= 0)
      unit->cxp = default_unit_cxp;
    /* First default supplies using the generic supply initialize. */
    init_supply(unit);
    /* Then fill in anything from the unit defaults. */
    for_all_material_types(m) {
	if (default_supply[m] >= 0)
	  unit->supply[m] = default_supply[m];
    }
    /* Fill in any default toolings that might have been set up. */
    if (default_tooling != NULL) {
	tp_varies = FALSE;
	for_all_unit_types(u2) {
	    if (default_tooling[u2] > 0) {
		tp_varies = TRUE;
		break;
	    }
	}
	if (tp_varies) {
	    if (unit->tooling == NULL)
	      init_unit_tooling(unit);
	    for_all_unit_types(u2)
	      unit->tooling[u] = default_tooling[u];
	}
    }
    /* Peel off fixed-position properties, if they're supplied. */
    if (numberp(car(props))) {
	unit->prevx = c_number(car(props)) + uxoffset - area.fullx;
	props = cdr(props);
    }
    if (numberp(car(props))) {
	unit->prevy = c_number(car(props)) + uyoffset - area.fully;
	props = cdr(props);
    }
    if (props != lispnil && !consp(car(props))) {
	nusn = c_number(eval(car(props)));
	props = cdr(props);
    }
    /* Now crunch through optional stuff.  The unit's properties must *already*
       be correct. */
    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, val);
	numval = 0;
	wasnum = FALSE;
	if (numberp(val)) {
	    numval = c_number(val);
	    wasnum = TRUE;
	}
	variablelength = FALSE;
	switch (keyword_code(propname)) {
	  case K_N:
	    unit->name = c_string(val);
	    break;
	  case K_SHARP:
	    nuid = numval;
	    break;
	  case K_S:
	    if (!wasnum)
	      numval = c_number(eval(val));
	    nusn = numval;
	    break;
	  case K_OS:
	    if (!wasnum)
	      numval = c_number(eval(val));
	    nuosn = numval;
	    break;
	  case K_AT:
	    unit->prevx = numval + uxoffset;
	    unit->prevy = c_number(caddr(bdg)) + uyoffset;
	    variablelength = TRUE;
	    break;
	  case K_NB:
	    unit->number = numval;
	    break;
	  case K_CP:
	    unit->cp = numval;
	    break;
	  case K_HP:
	    /* Note if the unit is read in as damaged, we don't want
	       damage reckoning to replace with a default hp2. */
	    unit->hp = unit->hp2 = numval;
	    break;
	  case K_CXP:
	    unit->cxp = numval;
	    break;
	  case K_MO:
	    unit->morale = numval;
	    break;
	  case K_M:
	    interp_mtype_value_list(unit->supply, cdr(bdg));
	    variablelength = TRUE;
	    break;
	  case K_TP:
	    if (unit->tooling == NULL)
	      init_unit_tooling(unit);
	    interp_utype_value_list(unit->tooling, cdr(bdg));
	    variablelength = TRUE;
	    break;
	  case K_OPINIONS:
	    if (unit->opinions == NULL)
	      init_unit_opinions(unit, numsides);
	    if (unit->opinions != NULL) {
		interp_side_value_list(unit->opinions, cdr(bdg));
	    } else {
		read_warning("Unit %s cannot have opinions, ignoring attempt to set",
			     unit_desig(unit));
	    }
	    variablelength = TRUE;
	    break;
	  case K_IN:
	    /* Stash the Lisp object pointer for now - will be
	       translated to unit pointer later. */
	    unit->transport = (Unit *) val;
	    break;
	  case K_ACT:
	    interp_unit_act(unit, cdr(bdg));
	    variablelength = TRUE;
	    break;
	  case K_PLAN:
	    interp_unit_plan(unit, cdr(bdg));
	    variablelength = TRUE;
	    break;
	  case K_Z:
	    unit->z = numval;
	    break;
	  case K_APPEAR:
	    if (unit->extras == NULL)
	      init_unit_extras(unit);
	    if (numberp(val))
	      unit->extras->appear = numval;
	    variablelength = TRUE;
	    break;
	  case K_DISAPPEAR:
	    if (unit->extras == NULL)
	      init_unit_extras(unit);
	    if (numberp(val))
	      unit->extras->disappear = numval;
	    variablelength = TRUE;
	    break;
	  case K_X:
	    if (unit->extras == NULL)
	      init_unit_extras(unit);
	    unit->extras->hook = cdr(bdg);
	    variablelength = TRUE;
	    break;
	  default:
	    unknown_property("unit", unit_desig(unit), propname);
	}
	if (!variablelength && cddr(bdg) != lispnil)
	  read_warning("Extra junk in the %s property of %s, ignoring",
		       propname, unit_desig(unit));
    }
    /* If the unit id was given, assign it to the unit, avoiding
       duplication. */
    if (nuid > 0) {
    	/* If this id is already in use by some other unit, complain. */
    	unit2 = find_unit(nuid);
    	if (unit2 != NULL && unit2 != unit)
	  init_error("Id %d already in use by %s", nuid, unit_desig(unit2)); 
     	/* Guaranteed distinct, safe to use. */
	unit->id = nuid;
	/* Ensure that future random ids won't step on this one. */
	nextid = max(nextid, nuid + 1);
    }
    if (nusn >= 0) {
	/* (should check that this is an allowed side?) */
	set_unit_side(unit, side_n(nusn));
    }
    if (nuosn >= 0) {
	/* (should check that this is an allowed side?) */
	set_unit_origside(unit, side_n(nuosn));
    } else {
	set_unit_origside(unit, unit->side);
    }
    /* (should fill in hook) */
    Dprintf("  Got %s\n", unit_desig(unit));
}

static void
interp_utype_list(arr, lis)
short *arr;
Obj *lis;
{
    int u = 0;
    Obj *rest, *head;

    /* Assume that if the destination array does not exist, there is
       probably a reason, and it's not our concern. */
    if (arr == NULL)
      return;
    lis = eval(lis);
    if (!consp(lis))
      lis = cons(lis, lispnil);
    for_all_list(lis, rest) {
    	head = car(rest);
    	if (utypep(head)) {
	    arr[head->v.num] = TRUE;
	} else {
	    /* syntax error */
	}
    }
}

static void
interp_utype_value_list(arr, lis)
short *arr;
Obj *lis;
{
    int u = 0;
    Obj *rest, *head, *types, *values, *subrest, *subrest2;

    /* Assume that if the destination array does not exist, there is
       probably a reason, and it's not our concern. */
    if (arr == NULL)
      return;
    for_all_list(lis, rest) {
    	head = car(rest);
    	if (numberp(head)) {
	    if (u < numutypes) {
	    	arr[u++] = c_number(head);
	    } else {
		init_warning("too many numbers in list");
	    }
	} else if (consp(head)) {
	    types = eval(car(head));
	    values = eval(cadr(head));
	    if (utypep(types)) {
		u = types->v.num;
	    	arr[u++] = c_number(values);
	    } else if (consp(values)) {
		for_both_lists(types, values, subrest, subrest2) {
		    TYPECHECK(utypep, car(subrest), "not a unit type");
		    u = car(subrest)->v.num;
		    arr[u++] = c_number(car(subrest2));
		}
	    } else {
		for_all_list(types, subrest) {
		    TYPECHECK(utypep, car(subrest), "not a unit type");
		    u = car(subrest)->v.num;
		    arr[u++] = c_number(values);
		}
	    }
	} else {
	    /* syntax error */
	}
    }
}

static void
interp_mtype_value_list(arr, lis)
short *arr;
Obj *lis;
{
    int m = 0;
    Obj *rest, *head, *types, *values, *subrest, *subrest2;

    /* Assume that if the destination array does not exist, there is
       probably a reason, and it's not our concern. */
    if (arr == NULL)
      return;
    for_all_list(lis, rest) {
    	head = car(rest);
    	if (numberp(head)) {
	    if (m < nummtypes) {
	    	arr[m++] = c_number(head);
	    } else {
		init_warning("too many numbers in list");
	    }
	} else if (consp(head)) {
	    types = eval(car(head));
	    values = eval(cadr(head));
	    if (mtypep(types)) {
		m = types->v.num;
	    	arr[m++] = c_number(values);
	    } else if (consp(values)) {
		for_both_lists(types, values, subrest, subrest2) {
		    TYPECHECK(mtypep, car(subrest), "not a material type");
		    m = car(subrest)->v.num;
		    arr[m++] = c_number(car(subrest2));
		}
	    } else {
		for_all_list(types, subrest) {
		    TYPECHECK(mtypep, car(subrest), "not a material type");
		    m = car(subrest)->v.num;
		    arr[m++] = c_number(values);
		}
	    }
	} else {
	    /* syntax error */
	}
    }
}

static void
interp_short_array(arr, lis, n)
short *arr;
Obj *lis;
int n;
{
    int i = 0;
    Obj *rest, *head;

    /* Assume that if the destination array does not exist, there is
       probably a reason, and it's not our concern. */
    if (arr == NULL)
      return;
    for_all_list(lis, rest) {
    	head = car(rest);
    	if (numberp(head)) {
	    if (i < n) {
	    	arr[i++] = c_number(head);
	    } else {
		init_warning("too many numbers in list");
		break;
	    }
	}
    }
}

static void
interp_long_array(arr, lis, n)
long *arr;
Obj *lis;
int n;
{
    int i = 0;
    Obj *rest, *head;

    /* Assume that if the destination array does not exist, there is
       probably a reason, and it's not our concern. */
    if (arr == NULL)
      return;
    for_all_list(lis, rest) {
    	head = car(rest);
    	if (numberp(head)) {
	    if (i < n) {
	    	arr[i++] = c_number(head);
	    } else {
		init_warning("too many numbers in list");
		break;
	    }
	}
    }
}

/* Interpret a unit's action state. */

static void
interp_unit_act(unit, props)
Unit *unit;
Obj *props;
{
    int numval;
    Obj *bdg, *propval;
    char *propname;

    if (unit->act == NULL) {
	unit->act = (ActorState *) xmalloc(sizeof(ActorState));
	/* Flag the action as undefined. */
	unit->act->nextaction.type = ACTION_NONE;
    }
    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, propval);
	if (numberp(propval))
	  numval = c_number(propval);
	switch (keyword_code(propname)) {
	  case K_ACP:
	    unit->act->acp = numval;
	    break;
	  case K_ACP0:
	    unit->act->initacp = numval;
	    break;
	  case K_AA:
	    unit->act->actualactions = numval;
	    break;
	  case K_AM:
	    unit->act->actualmoves = numval;
	    break;
	  case K_A:
	    /* (should interp a spec for the next action) */
	    break;
	  default:
	    unknown_property("unit actionstate", unit_desig(unit), propname);
	}
    }
}

/* Fill in a unit's plan. */

static void
interp_unit_plan(unit, props)
Unit *unit;
Obj *props;
{
    int numval;
    Obj *bdg, *propval, *plantypesym, *trest;
    char *propname;
    Goal *goal;
    Task *task;

    if (unit->plan == NULL) {
	/* Create the plan explicitly, even if unit type doesn't allow it
	   (type might be changed later in the reading process). */
	unit->plan = (Plan *) xmalloc(sizeof(Plan));
	/* From init_unit_plan: can't call it directly, might not behave
	   right (should fix to be callable from here - problem is that
	   other unit props such as cp might not be set right yet) */
	/* Allow AIs to make this unit do things. */
	unit->plan->aicontrol = TRUE;
	/* Enable supply alarms by default. */
	unit->plan->supply_alarm = TRUE;
    }
    plantypesym = car(props);
    SYNTAX(props, symbolp(plantypesym), "plan type must be a symbol");
    unit->plan->type = lookup_plan_type(c_string(plantypesym));
    props = cdr(props);
    SYNTAX(props, numberp(car(props)), "plan creation turn must be a number");
    unit->plan->creation_turn = c_number(car(props));
    props = cdr(props);
    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, propval);
	if (numberp(propval))
	  numval = c_number(propval);
	switch (keyword_code(propname)) {
	  case K_INITIAL_TURN:
	    unit->plan->initial_turn = numval;
	    break;
	  case K_FINAL_TURN:
	    unit->plan->final_turn = numval;
	    break;
	  case K_ASLEEP:
	    unit->plan->asleep = numval;
	    break;
	  case K_RESERVE:
	    unit->plan->reserve = numval;
	    break;
	  case K_DELAYED:
	    unit->plan->delayed = numval;
	    break;
	  case K_WAIT:
	    unit->plan->waitingfortasks = numval;
	    break;
	  case K_AI_CONTROL:
	    unit->plan->aicontrol = numval;
	    break;
	  case K_SUPPLY_ALARM:
	    unit->plan->supply_alarm = numval;
	    break;
	  case K_SUPPLY_IS_LOW:
	    unit->plan->supply_is_low = numval;
	    break;
	  case K_WAIT_TRANSPORT:
	    unit->plan->waitingfortransport = numval;
	    break;
	  case K_GOAL:
	    goal = interp_goal(cdr(bdg));
	    unit->plan->maingoal = goal;
	    break;
	  case K_FORMATION:
	    goal = interp_goal(cdr(bdg));
	    unit->plan->formation = goal;
	    /* (should do after all units read in!) */
	    unit->plan->funit = find_unit(goal->args[0]);
	    break;
	  case K_TASKS:
	    for_all_list(cdr(bdg), trest) {
	    	task = interp_task(car(trest));
		if (task) {
		    /* (should add tasks in reverse order) */
		    task->next = unit->plan->tasks;
		    unit->plan->tasks = task;
		}
	    }
	    break;
	  default:
	    unknown_property("unit plan", unit_desig(unit), propname);
	}
    }
}

int
lookup_plan_type(name)
char *name;
{
    int i;
    extern char *plantypenames[];

    for (i = 0; plantypenames[i] != NULL; ++i)
      /* should get real enum */
      if (strcmp(name, plantypenames[i]) == 0)
	return i;
    return PLAN_NONE;
}

static Task *
interp_task(form)
Obj *form;
{
    int tasktype, numargs, i;
    char *argtypes;
    Obj *tasktypesym;
    Task *task;

    tasktypesym = car(form);
    SYNTAX_RETURN(form, symbolp(tasktypesym), "task type must be a symbol", NULL);
    tasktype = lookup_task_type(c_string(tasktypesym));
    task = create_task(tasktype);
    form = cdr(form);
    task->execnum = c_number(car(form));
    form = cdr(form);
    task->retrynum = c_number(car(form));
    form = cdr(form);
    argtypes = taskdefns[tasktype].argtypes;
    numargs = strlen(argtypes);
    for (i = 0; i < numargs; ++i) {
	if (form == lispnil)
	  break;
	SYNTAX_RETURN(form, numberp(car(form)), "task arg must be a number", NULL);
	task->args[i] = c_number(car(form));
	form = cdr(form);
    }
    /* Warn about unused data, but not a serious problem. */
    if (form != lispnil)
      read_warning("Excess args for task %s", task_desig(task));
    return task;
}

static Goal *
interp_goal(form)
Obj *form;
{
    int goaltype, tf, numargs, i;
    char *argtypes;
    Obj *goaltypesym;
    Goal *goal;
    Side *side;

    SYNTAX_RETURN(form, numberp(car(form)), "goal side must be a number", NULL);
    side = side_n(c_number(car(form)));
    form = cdr(form);
    SYNTAX_RETURN(form, numberp(car(form)), "goal tf must be a number", NULL);
    tf = c_number(car(form));
    form = cdr(form);
    goaltypesym = car(form);
    SYNTAX_RETURN(form, symbolp(goaltypesym), "goal type must be a symbol", NULL);
    goaltype = lookup_goal_type(c_string(goaltypesym));
    goal = create_goal(goaltype, side, tf);
    form = cdr(form);
    argtypes = goaldefns[goaltype].argtypes;
    numargs = strlen(argtypes);
    for (i = 0; i < numargs; ++i) {
	if (form == lispnil)
	  break;
	SYNTAX_RETURN(form, numberp(car(form)), "goal arg must be a number", NULL);
	goal->args[i] = c_number(car(form));
	form = cdr(form);
    }
    /* Warn about unused data, but not a serious problem. */
    if (form != lispnil)
      read_warning("Excess args for goal %s", goal_desig(goal));
    return goal;
}

/* (to goal.c?) */

int
lookup_goal_type(name)
char *name;
{
    int i;

    for (i = 0; goaldefns[i].name != NULL; ++i)
      if (strcmp(name, goaldefns[i].name) == 0)
	return i; /* should get real enum? */
    return GOAL_NO;
}

/* Make a namer from the form. */

static void
interp_namer(form)
Obj *form;
{
    Obj *id = cadr(form), *meth = car(cddr(form));

    if (symbolp(id)) {
	setq(id, make_namer(id, meth));
    }
}

static void
interp_text_generator(form)
Obj *form;
{
    Obj *id = cadr(form);

    if (symbolp(id)) {
	setq(id, lispnil);
    }
}

/* Make a scorekeeper from the given form. */

static void
interp_scorekeeper(form)
Obj *form;
{
    int id = 0;
    char *propname;
    Obj *props = cdr(form), *bdg, *propval;
    Scorekeeper *sk = NULL;

    if (numberp(car(props))) {
	id = c_number(car(props));
	props = cdr(props);
    }
    if (id > 0) {
	sk = find_scorekeeper(id);
    }
    /* Create a new scorekeeper object if necessary. */
    if (sk == NULL) {
	sk = create_scorekeeper();
	if (id > 0) {
	    sk->id = id;
	}
    }
    /* Interpret the properties. */
    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, propval);
	switch (keyword_code(propname)) {
	  case K_TITLE:
	    sk->title = c_string(propval);
	    break;
	  case K_WHEN:
	    sk->when = propval;
	    break;
	  case K_APPLIES_TO:
	    sk->who = propval;
	    break;
	  case K_KNOWN_TO:
	    sk->knownto = propval;
	    break;
	  case K_TRIGGER:
	    sk->trigger = propval;
	    break;
	  case K_DO:
	    sk->body = propval;
	    break;
	  case K_MESSAGES:
	    sk->messages = propval;
	    break;
	  case K_TRIGGERED:
	    sk->triggered = c_number(propval);
	    break;
	  case K_INITIAL:
	    sk->initial = c_number(propval);
	    break;
	  case K_NOTES:
	    sk->notes = propval;
	    break;
	  default:
	    unknown_property("scorekeeper", "??", propname);
	}
    }
}

/* Make a past unit from the form. */

static void
interp_past_unit(form)
Obj *form;
{
    int u = NONUTYPE, nid;
    char *propname;
    Obj *props, *bdg, *propval;
    PastUnit *pastunit;

    Dprintf("Reading a past unit from ");
    Dprintlisp(form);
    Dprintf("\n");
    props = cdr(form);
    if (numberp(car(props))) {
	nid = c_number(car(props));
	props = cdr(props);
    } else {
	/* (should be error) */
    }
    if (symbolp(car(props))) {
	u = utype_from_symbol(car(props));
	props = cdr(props);
    }
    if (u == NONUTYPE) {
	read_warning("bad exu");
	return;
    }
    pastunit = create_past_unit(u, nid);
    /* Peel off fixed-position properties, if they're supplied. */
    if (numberp(car(props))) {
	pastunit->x = c_number(eval(car(props)));
	props = cdr(props);
    }
    if (numberp(car(props))) {
	pastunit->y = c_number(eval(car(props)));
	props = cdr(props);
    }
    if (!consp(car(props))) {
	pastunit->side = side_n(c_number(eval(car(props))));
	props = cdr(props);
    }
    for (; props != lispnil; props = cdr(props)) {
	bdg = car(props);
	PARSE_PROPERTY(bdg, propname, propval);
	switch (keyword_code(propname)) {
	  case K_Z:
	    pastunit->z = c_number(propval);
	    break;
	  case K_N:
	    pastunit->name = c_string(propval);
	    break;
	  case K_NB:
	    pastunit->number = c_number(propval);
	    break;
	  default:
	    unknown_property("exu", "??", propname);
	}
    }
}

/* Make a historical event from the form. */

static void
interp_history(form)
Obj *form;
{
    int startdate, type, i;
    char *typename;
    SideMask observers;
    Obj *props;
    HistEvent *hevt;

    Dprintf("Reading a hist event from ");
    Dprintlisp(form);
    Dprintf("\n");
    props = cdr(form);
    /* Get the event's date. */
    if (numberp(car(props))) {
	startdate = c_number(car(props));
	props = cdr(props);
    } else {
	syntax_error(form, "bad hevt date");
	return;
    }
    /* Get the event type. */
    if (symbolp(car(props))) {
	typename = c_string(car(props));
	type = -1;
	for (i = 0; hevtdefns[i].name != NULL; ++i)
	  if (strcmp(typename, hevtdefns[i].name) == 0) {
	      type = i;
	      break;
	  }
	props = cdr(props);
    } else {
	syntax_error(form, "bad hevt type");
	return;
    }
    /* Get the bit vector of observers. */
    if (numberp(car(props))) {
	observers = c_number(car(props));
	props = cdr(props);
    } else if (symbolp(car(props))
	       && keyword_code(c_string(car(props))) == K_ALL) {
	observers = ALLSIDES;
	props = cdr(props);
    } else {
	syntax_error(form, "bad hevt observers");
	return;
    }
    hevt = create_historical_event(type);
    hevt->startdate = startdate;
    hevt->observers = observers;
    /* Read up to 4 remaining numbers. */
    i = 0;
    for (; props != lispnil && i < 4; props = cdr(props)) {
	hevt->data[i++] = c_number(car(props));
    }
    /* Insert the newly created event. */
    /* (linking code should be in its own routine) */
    hevt->next = history;
    hevt->prev = history->prev;
    history->prev->next = hevt;
    history->prev = hevt;
}

/* Designer is trying to define too many different types. */

static void
too_many_types(typename, maxnum, name)
char *typename;
int maxnum;
Obj *name;
{
    read_warning("Limited to %d types of %s", maxnum, typename);
    sprintlisp(spbuf, name);
    read_warning("(Failed to create type with name `%s')", spbuf);
}

/* Property name is unknown, either misspelled or misapplied. */

static void
unknown_property(type, inst, name)
char *type, *inst, *name;
{
    read_warning("The %s form %s has no property named %s", type, inst, name);
}

static void
read_layer(contents, setter)
Obj *contents;
void (*setter) PARAMS ((int, int, int));
{
    int i, slen, n, ix, len, usechartable = FALSE;
    char *str;
    short chartable[256];
    Obj *rest, *desc, *rest2, *subdesc, *sym, *num;

    layer_use_default = FALSE;
    layer_default = 0;
    layer_multiplier = 1;
    layer_adder = 0;
    layer_area_x = area.fullx;  layer_area_y = area.fully;
    layer_area_w = area.width;  layer_area_h = area.height;
    if (area.fullwidth > 0)
      layer_area_w = area.fullwidth;
    if (area.fullheight > 0)
      layer_area_h = area.fullheight;
    ignore_specials = FALSE;
    for_all_list(contents, rest) {
	desc = car(rest);
	if (stringp(desc)) {
	    /* Read from here to the end of the list, interpreting as
	       contents. */
	    read_rle(rest, setter, (usechartable ? chartable : NULL));
	    return;
	} else if (consp(desc) && symbolp(car(desc))) {
	    switch (keyword_code(c_string(car(desc)))) {
	      case K_CONSTANT:
		/* should set to a constant value taken from cadr */
		read_warning("Constant layers not supported yet");
		return;
	      case K_SUBAREA:
	        /* should apply data to a subarea */
		read_warning("Layer subareas not supported yet");
		break;
	      case K_XFORM:
		layer_multiplier = c_number(cadr(desc));
		layer_adder = c_number(caddr(desc));
		break;
	      case K_BY_BITS:
		break;
	      case K_BY_CHAR:
		/* Assign each char to its corresponding index. */
		/* First seed the table with a 1-1 map. */
		for (i = 0; i < 255; ++i)
		  chartable[i] = 0;
		for (i = 'a'; i <= '~'; ++i)
		  chartable[i] = i - 'a';
		for (i = ':'; i <= '['; ++i)
		  chartable[i] = i - ':' + 30;
		str = c_string(cadr(desc));
		len = strlen(str);
		for (i = 0; i < len; ++i) {
		    chartable[(int) str[i]] = i;
		    /* If special chars in by-char string, flag it. */
		    if (str[i] == '*' || str[i] == ',')
		      ignore_specials = TRUE;
		}
		usechartable = TRUE;
		break;
	      case K_BY_NAME:
		/* Work through list and match names to numbers. */
		/* First seed the table with a 1-1 map. */
		for (i = 0; i < 255; ++i)
		  chartable[i] = 0;
		for (i = 'a'; i <= '~'; ++i)
		  chartable[i] = i - 'a';
		for (i = ':'; i <= '['; ++i)
		  chartable[i] = i - ':' + 30;
		desc = cdr(desc);
		/* Support optional explicit string a la by-char. */
		if (stringp(car(desc))) {
		    str = c_string(car(desc));
		    slen = strlen(str);
		    for (i = 0; i < slen; ++i)
		      chartable[(int) str[i]] = i;
		    desc = cdr(desc);
		} else {
		    str = NULL;
		}
		i = 0;
		for (rest2 = desc; rest2 != lispnil; rest2 = cdr(rest2)) {
		    subdesc = car(rest2);
		    if (symbolp(subdesc)) {
		    	sym = subdesc;
		    	ix = i++;
		    } else if (consp(subdesc)) {
		    	sym = car(subdesc);
		    	num = cadr(subdesc);
		    	TYPECHECK(numberp, num,
				  "by-name explicit value is not a number");
		    	ix = c_number(num);
		    } else {
		    	read_warning("garbage by-name subdesc, ignoring");
		    	continue;
		    }
		    /* Eval the symbol into something resembling a value. */
		    sym = eval(sym);
		    TYPECHECK(numberishp, sym,
			      "by-name index is not a number or type");
		    n = c_number(sym);
		    chartable[(str ? str[ix] : (ix <= 29 ? ('a' + ix) : (':' + ix - 30)))] = n;
		}
		usechartable = TRUE;
		break;
	      default:
		sprintlisp(readerrbuf, desc);
		read_warning("Ignoring garbage terrain description %s",
			     readerrbuf);
	    }
	}
    }
}

/* General RLE reader.  This basically parses the run lengths and calls
   the function that records what was read. */

static void
read_rle(contents, setter, chartable)
Obj *contents;
void (*setter) PARAMS ((int, int, int));
short *chartable;
{
    char ch, *rowstr;
    int i, x, y, run, val, sawval, sawneg, sgn, x1, y1, numbadchars = 0;
    Obj *rest;

    rest = contents;
    y = layer_area_h - 1;
    while (rest != lispnil && y >= 0) {
	/* should error check ... */
	rowstr = c_string(car(rest));
	i = 0;
	x = 0;  /* depends on shape of saved data... */
	while ((ch = rowstr[i++]) != '\0' && x < layer_area_w) {
	    sawval = FALSE;
	    sawneg = FALSE;
	    if (isdigit(ch) || ch == '-') {
		if (ch == '-') {
		    sawneg = TRUE;
		    ch = rowstr[i++];
		    /* A minus sign by itself is a problem. */
		    if (!isdigit(ch))
		      goto recovery;
		}
		/* Interpret a substring of digits as a run length. */
		run = ch - '0';
		while ((ch = rowstr[i++]) != 0 && isdigit(ch)) {
		    run = run * 10 + ch - '0';
		}
		/* A '*' separates a run and a numeric value. */
		if (ch == '*' && !ignore_specials) {
		    /* A negative run length is a problem. */
		    if (sawneg)
		      goto recovery;
		    ch = rowstr[i++];
		    /* If we're seeing garbled data, skip to the next line. */
		    if (ch == '\0')
		      goto recovery;
		    /* Recognize a negative number. */
		    sgn = 1;
		    if (ch == '-') {
			val = -1;
			ch = rowstr[i++];
		    }
		    /* Interpret these digits as a value. */
		    if (isdigit(ch)) {
			val = ch - '0';
			while ((ch = rowstr[i++]) != 0 && isdigit(ch)) {
			    val = val * 10 + ch - '0';
			}
			sawval = TRUE;
			val = sgn * val;
		    } else {
			/* Some other char seen - just ignore the '*' then. */
		    }
		    /* If we're seeing garbled data, skip to the next line. */
		    if (ch == '\0')
		      goto recovery;
		}
		/* If we're seeing garbled data, skip to the next line. */
		if (ch == '\0')
		  goto recovery;
	    } else {
		run = 1;
	    }
	    if (ch == ',' && !ignore_specials) {
	    	if (!sawval) {
		    /* This was a value instead of a run length. */
		    val = run;
		    /* If it was prefixed with a minus sign originally,
		       negate the value. */
		    if (sawneg)
		      val = - val;
		    run = 1;
		} else {
		    /* Comma is just being a separator. */
		}
	    } else if (chartable != NULL) {
		val = chartable[ch];
	    } else if (between('a', ch, '~')) {
		val = ch - 'a';
	    } else if (between(':', ch, '[')) {
		val = ch - ':' + 30;
	    } else {
	    	/* Warn about strange characters. */
		++numbadchars;
		if (numbadchars <= 5) {
		    read_warning(
		     "Bad char '%c' (0x%x) in layer, using NUL instead",
				 ch, ch);
		    /* Clarify that we're not going to report all bad chars. */
		    if (numbadchars == 5)
		      read_warning(
		     "Additional bad chars will not be reported individually");
		}
		val = 0;
	    }
	    val = val * layer_multiplier + layer_adder;
	    /* Given a run of values, stuff them into the layer. */
	    while (run-- > 0) {
	    	x1 = wrapx(x - layer_area_x);  y1 = y - layer_area_y;
	    	if (in_area(x1, y1))
		  (*setter)(x1, y1, val);
		++x;
	    }
	}
      recovery:
	/* Fill-in string may be too short for this row; just leave
	   the rest of it alone, assume that somebody has assured
	   that the contents are reasonable. */
	rest = cdr(rest);
	y--;
    }
    /* Report the count of garbage chars, in case there were a great many. */
    if (numbadchars > 0)
      init_warning("A total of %d bad chars were present", numbadchars);
}
