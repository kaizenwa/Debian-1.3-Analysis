/* Xconq game module writing.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern void init_write PARAMS ((void));
extern char *shortest_escaped_name PARAMS ((int u));
extern int xmalloc_warnings;
extern int memory_exhausted;
extern Feature *featurelist;
extern Doctrine *doctrine_list;
#include "imf.h"
extern ImageFamily **recorded_imfs;
extern int num_recorded_imfs;

#define key(x) (keyword_name(x))

static void start_form PARAMS ((char *hd));
static void add_to_form PARAMS ((char *x));
static void add_num_to_form PARAMS ((int x));
static void add_num_or_dice_to_form PARAMS ((int x));
static void end_form PARAMS ((void));
static void newline_form PARAMS ((void));
static void space_form PARAMS ((void));
static char *escaped_symbol PARAMS ((char *str));
static char *escaped_string PARAMS ((char *str));
static void write_bool_prop PARAMS ((char *name, int value,
				     int dflt, int nodefaulting,
				     int addnewline));
static void write_num_prop PARAMS ((char *name, int value,
				    int dflt, int nodefaulting,
				    int addnewline));
static void write_str_prop PARAMS ((char *name, char *value,
				    char *dflt, int nodefaulting,
				    int addnewline));
static int string_not_default PARAMS ((char *str, char *dflt));
static void write_lisp_prop PARAMS ((char *name, struct a_obj *value,
				     struct a_obj *dflt, int nodefaulting,
				     int as_cdr, int addnewline));
static void write_utype_value_list PARAMS ((char *name, short *arr,
					    int dflt, int addnewline));
static void write_mtype_value_list PARAMS ((char *name, short *arr,
					    int dflt, int addnewline));
static void write_side_value_list PARAMS ((char *name, short *arr,
					   int dflt, int addnewline));
static void write_utype_string_list PARAMS ((char *name, char **arr,
					     char *dflt, int addnewline));
static void write_types PARAMS ((void));
static void write_tables PARAMS ((int compress));
static void write_type_name_list PARAMS ((int typ, int *flags, int dim));
#if 0
static void write_type_value_list PARAMS ((int typ, int *flags, int dim,
					   int (*getter)(int, int), int i));
#endif
static void write_table PARAMS ((char *name, int (*getter)(int, int),
				 int dflt, int typ1, int typ2, int compress));
static void write_world PARAMS ((void));
static void write_areas PARAMS ((Module *module));
static void write_area_terrain PARAMS ((int compress));
static void write_area_aux_terrain PARAMS ((int compress));
static void write_area_features PARAMS ((int compress));
static void write_area_elevations PARAMS ((int compress));
static void write_area_people_sides PARAMS ((int compress));
static void write_area_materials PARAMS ((int compress));
static void write_area_temperatures PARAMS ((int compress));
static void write_area_clouds PARAMS ((int compress));
static void write_area_winds PARAMS ((int compress));
static void write_globals PARAMS ((void));
static void write_scorekeepers PARAMS ((void));
static void write_doctrines PARAMS ((void));
static void write_sides PARAMS ((Module *module));
static void write_side_properties PARAMS ((Side *side));
static int fn_terrain_view PARAMS ((int x, int y));
static int fn_aux_terrain_view PARAMS ((int x, int y));
static int fn_aux_terrain_view_date PARAMS ((int x, int y));
static int fn_terrain_view_date PARAMS ((int x, int y));
static int fn_unit_view PARAMS ((int x, int y));
static int fn_unit_view_date PARAMS ((int x, int y));
static int fn_temp_view PARAMS ((int x, int y));
static int fn_temp_view_date PARAMS ((int x, int y));
static int fn_cloud_view PARAMS ((int x, int y));
static int fn_cloud_bottom_view PARAMS ((int x, int y));
static int fn_cloud_height_view PARAMS ((int x, int y));
static int fn_cloud_view_date PARAMS ((int x, int y));
static int fn_wind_view PARAMS ((int x, int y));
static int fn_wind_view_date PARAMS ((int x, int y));
static void write_side_view PARAMS ((Side *side, int compress));
static void write_one_side_view_layer PARAMS ((int propkey, int (*fn)(int x, int y)));
static void write_players PARAMS ((void));
static void write_player PARAMS ((struct a_player *player));
static void write_agreements PARAMS ((void));
static void write_agreement PARAMS ((struct a_agreement *agreement));
static void write_units PARAMS ((Module *module));
static void write_unit_properties PARAMS ((Unit *unit));
static void write_unit_act PARAMS ((Unit *unit));
static void write_unit_plan PARAMS ((Unit *unit));
static void write_task PARAMS ((Task *task));
static void write_goal PARAMS ((Goal *goal, int keyword));
static void write_rle PARAMS ((int (*datafn)(int, int), int lo, int hi,
			       int (*translator)(int), int compress));
static void write_run PARAMS ((int run, int val));
static void write_history PARAMS ((void));
static void write_past_unit PARAMS ((PastUnit *pastunit));
static void write_historical_event PARAMS ((HistEvent *hevt));
static void write_images PARAMS ((void));
static int reshaped_point PARAMS ((int x1, int y1, int *x2p, int *y2p));
static int original_point PARAMS ((int x1, int y1, int *x2p, int *y2p));

/* The pointer to the file being written to. */

static FILE *fp;

/* True if the area is to be saved to a different size than it is now. */

static int doreshape;

static Module *reshaper;

/* A pre-allocated module used for when we're saving the game and may
   not do any more allocation. */

static Module *spare_module;

static char *shortestbuf;

static char *escapedthingbuf;

static int tmpcompress;

/* Preparation and preallocation for writing. */

void
init_write()
{
    spare_module = create_game_module("spare module");
    shortestbuf = xmalloc(BUFSIZE);
    escapedthingbuf = xmalloc(BUFSIZE);
}

/* Little routines to do low-level syntax.  While these look excessive, calling fprintf
   directly would result in an object code size increase of as much as 25%. */

static void
start_form(hd)
char *hd;
{
    fprintf(fp, "(%s", hd);
}

static void
add_to_form(x)
char *x;
{
    fprintf(fp, " %s", x);
}

static void
add_num_to_form(x)
int x;
{
    fprintf(fp, " %d", x);
}

/* Write either a normal value or a dice spec, as appropriate. */

static void
add_num_or_dice_to_form(x)
int x;
{
    if (x >> 14 == 1) {
	fprintf(fp, " %dd%d+%d", (x >> 11) & 0x07, (x >> 7) & 0x0f, x & 0x7f);
    } else {
	fprintf(fp, " %d", x);
    }
}

static void
end_form()
{
    fprintf(fp, ")");
}

static void
newline_form()
{
    fprintf(fp, "\n");
}

static void
space_form()
{
    fprintf(fp, " ");
}

/* These two routines make sure that any symbols and strings can
   be read in again. */

static char *
escaped_symbol(str)
char *str;
{
    char *tmp = str;

    if (str[0] == '|' && str[strlen(str)-1] == '|')
      return str;
    while (*tmp != '\0') {
	if (((char *) strchr(" ()#\";|", *tmp)) != NULL || isdigit(str[0])) {
	    sprintf(escapedthingbuf, "|%s|", str);
	    return escapedthingbuf;
	}
	++tmp;
    }
    return str;
}

/* Note that this works correctly on NULL strings, turning them into
   strings of length 0. */

static char *
escaped_string(str)
char *str;
{
    char *tmp = str, *rslt = escapedthingbuf;

    *rslt++ = '"';
    if (str != NULL) {
	while (*tmp != 0) {
	    if (*tmp == '"') *rslt++ = '\\';
	    *rslt++ = *tmp++;
	}
    }
    *rslt++ = '"';
    *rslt = '\0';
    return escapedthingbuf;
}

static void
write_bool_prop(name, value, dflt, nodefaulting, addnewline)
char *name;
int value, dflt, nodefaulting, addnewline;
{
    if (nodefaulting || value != dflt) {
	space_form();
	start_form(name);
	add_to_form((value ? "true" : "false"));
	end_form();
	if (addnewline) {
	    newline_form();
	    space_form();
	}
    }
}

static void
write_num_prop(name, value, dflt, nodefaulting, addnewline)
char *name;
int value, dflt, nodefaulting, addnewline;
{
    if (nodefaulting || value != dflt) {
	space_form();
	start_form(name);
	/* (should check that this is always correct to use here) */
	add_num_or_dice_to_form(value);
	end_form();
	if (addnewline) {
	    newline_form();
	    space_form();
	}
    }
}

/* Handle the writing of a single string-valued property. */

static void
write_str_prop(name, value, dflt, nodefaulting, addnewline)
char *name, *value, *dflt;
int nodefaulting, addnewline;
{
    if (nodefaulting || string_not_default(value, dflt)) {
	space_form();
	start_form(name);
	add_to_form(escaped_string(value));
	end_form();
	if (addnewline) {
	    newline_form();
	    space_form();
	}
    }
}

static int
string_not_default(str, dflt)
char *str, *dflt;
{
    if (empty_string(dflt)) {
	if (empty_string(str)) {
	    return FALSE;
	} else {
	    return TRUE;
	}
    } else {
	if (empty_string(str)) {
	    return TRUE;
	} else {
	    return (strcmp(str, dflt) != 0);
	}
    }
}

static void
write_lisp_prop(name, value, dflt, nodefaulting, as_cdr, addnewline)
char *name;
Obj *value, *dflt;
int nodefaulting, as_cdr, addnewline;
{
    Obj *rest;

    /* Sanity check. */
    if (value == NULL) {
	run_warning("Property \"%s\" has a bad value NULL, ignoring", name);
	return;
    }
    if (nodefaulting || !equal(value, dflt)) {
	space_form();
	start_form(name);
	if (as_cdr && consp(value)) {
	    for_all_list(value, rest) {
	    	space_form();
	    	fprintlisp(fp, car(rest));
	    }
	} else {
	    space_form();
	    fprintlisp(fp, value);
	}
	end_form();
	if (addnewline) {
	    newline_form();
	    space_form();
	}
    }
}

static void
write_utype_value_list(name, arr, dflt, addnewline)
char *name;
short *arr;
int dflt, addnewline;
{
    int u, writeany;

    if (arr == NULL)
      return;
    writeany = FALSE;
    for_all_unit_types(u) {
	if (arr[u] != dflt) {
	    writeany = TRUE;
	    break;
	}
    }
    if (!writeany)
      return;
    space_form();
    start_form(name);
    for_all_unit_types(u) {
	add_num_to_form(arr[u]);
    }
    end_form();
    if (addnewline) {
	newline_form();
	space_form();
    }
}

static void
write_mtype_value_list(name, arr, dflt, addnewline)
char *name;
short *arr;
int dflt, addnewline;
{
    int m, writeany;

    if (nummtypes == 0 || arr == NULL)
      return;
    writeany = FALSE;
    for_all_material_types(m) {
	if (arr[m] != dflt) {
	    writeany = TRUE;
	    break;
	}
    }
    if (!writeany)
      return;
    space_form();
    start_form(name);
    for_all_material_types(m) {
	add_num_to_form(arr[m]);
    }
    end_form();
    if (addnewline) {
	newline_form();
	space_form();
    }
}

static void
write_side_value_list(name, arr, dflt, addnewline)
char *name;
short *arr;
int dflt, addnewline;
{
    int s, writeany;

    if (arr == NULL)
      return;
    writeany = FALSE;
    for (s = 0; s <= numsides; ++s) {
	if (arr[s] != dflt) {
	    writeany = TRUE;
	    break;
	}
    }
    if (!writeany)
      return;
    space_form();
    start_form(name);
    for (s = 0; s <= numsides; ++s) {
	add_num_to_form(arr[s]);
    }
    end_form();
    if (addnewline) {
	newline_form();
	space_form();
    }
}

static void
write_utype_string_list(name, arr, dflt, addnewline)
char *name;
char **arr, *dflt;
int addnewline;
{
    int u, writeany;

    if (arr == NULL)
      return;
    writeany = FALSE;
    for_all_unit_types(u) {
	if (arr[u] != dflt /* bogus, should use strcmp */) {
	    writeany = TRUE;
	    break;
	}
    }
    if (!writeany)
      return;
    space_form();
    start_form(name);
    for_all_unit_types(u) {
	add_to_form(escaped_string(arr[u]));
    }
    end_form();
    if (addnewline) {
	newline_form();
	space_form();
    }
}

/* Return the shortest properly escaped name that can be used to identify
   unit type. */

char *
shortest_escaped_name(u)
int u;
{
    char *typename = u_type_name(u);

    if (strlen(u_internal_name(u)) < strlen(typename)) {
	typename = u_internal_name(u);
    }
    sprintf(shortestbuf, "%s", escaped_symbol(typename));
    return shortestbuf;
}

/* A saved game should include everything necessary to recreate a game
   exactly.  It is important that this routine not attempt to use graphics,
   since it may be called when graphics code fails, and that it not allocate
   memory, since it may be called upon memory exhaustion.  Returns TRUE if
   that save was successful. */

int
write_entire_game_state(fname)
char *fname;
{
    Module *module;
    int rslt;

    /* Note in the history that a copy was made. */
    if (!memory_exhausted)
      record_event(H_GAME_SAVED, ALLSIDES);
    /* No additional allocation should ever happen during saving,
       so complain if it does. */
    xmalloc_warnings = TRUE;
    module = spare_module;
    /* Reset the module's contents, might have been used already. */
    memset(module, 0, sizeof(Module));
    module->name = fname;
    module->filename = fname;
    module->instructions = lispnil;
    module->notes = lispnil;
    module->designnotes = lispnil;
    module->startlineno = 1;
    module->endlineno = 1;
    module->compress_tables = TRUE;
    module->compress_layers = TRUE;
    module->def_all = TRUE;
    /* Record information about the original main module, for use after
       the game has been restored.  mainmodule should point to something,
       but it doesn't actually have to. */
    if (mainmodule != NULL) {
	module->origmodulename = mainmodule->name;
	module->origvariants = mainmodule->variants;
	module->origversion = mainmodule->version;
    }
    rslt = write_game_module(module);
    xmalloc_warnings = FALSE;
    /* Record that the game's state is accurately saved away. */
    if (rslt)
      gamestatesafe = TRUE;
    return rslt;
}

/* Given a game module telling what is in the module, write out a file
   containing the requested content.  Return true if everything went OK. */

int
write_game_module(module)
Module *module;
{
    int i;
    Obj *rest;
    Variant *var;
    Module *origmodule;

    if (module->filename == NULL) {
	/* (should be an error?) */
	return FALSE;
    }
    fp = fopen(module->filename, "w");
    if (fp != NULL) {
	/* Write the definition of this game module. */
	start_form(key(K_GAME_MODULE));
	add_to_form(escaped_string(module->name));
	newline_form();
	space_form();
	if (module->def_all) {
	    write_str_prop(key(K_TITLE), module->title,
			   "", FALSE, TRUE);
	    write_str_prop(key(K_BLURB), module->blurb,
			   "", FALSE, TRUE);
	    write_str_prop(key(K_PICTURE_NAME), module->picturename,
			   "", FALSE, TRUE);
	    write_str_prop(key(K_BASE_MODULE), module->basemodulename,
			   "", FALSE, TRUE);
	    write_str_prop(key(K_DEFAULT_BASE_MODULE), module->defaultbasemodulename,
			   "", FALSE, TRUE);
	    write_str_prop(key(K_BASE_GAME), module->basegame,
			   "", FALSE, TRUE);
	    write_str_prop(key(K_VERSION), module->version,
			   "", FALSE, TRUE);
	    write_str_prop(key(K_PROGRAM_VERSION), version_string(),
			   "", FALSE, TRUE);
	    if (module->variants) {
		start_form(key(K_VARIANTS));
		for (i = 0; module->variants[i].id != lispnil; ++i) {
		    var = &(module->variants[i]);
		    if (!empty_string(var->name)) {
			start_form(escaped_string(var->name));
			if (symbolp(var->id)) {
			    add_to_form(c_string(var->id));
			} else {
			    space_form();
			    fprintlisp(fp, var->id);
			}
		    } else if (symbolp(var->id)) {
			start_form(c_string(var->id));
		    } else {
			start_form("");
			fprintlisp(fp, var->id);
		    }
		    if (var->dflt != lispnil) {
			space_form();
			fprintlisp(fp, var->dflt);
		    }
		    for_all_list(var->cases, rest) {
			space_form();
			fprintlisp(fp, car(rest));
		    }
		    end_form();
		}
		end_form();
		newline_form();
	    }
	    /* Write out the "original variants" selected for a game.  Note that
	       since these are just used to record into scorefiles, we need only
	       the values, not full variant definitions. */
	    write_str_prop(key(K_ORIGINAL_MODULE), module->origmodulename,
			   "", FALSE, TRUE);
	    if (module->origvariants) {
		space_form();
		start_form(key(K_ORIGINAL_VARIANTS));
		for (i = 0; module->origvariants[i].id != lispnil; ++i) {
		    var = &(module->origvariants[i]);
		    space_form();
		    if (symbolp(var->id)) {
			start_form(c_string(var->id));
		    } else {
			start_form("");
			add_num_to_form(i);
		    }
		    if (var->hasintvalue) {
			add_num_to_form(var->intvalue);
		    }
		    end_form();
		}
		end_form();
		newline_form();
		space_form();
	    }
	}
	space_form();
	end_form();
	newline_form();
	newline_form();
	if (module->def_all || module->def_types)
	  write_types();
	if (module->def_all || module->def_tables)
	  write_tables(module->compress_tables);
	if (module->def_all || module->def_globals)
	  write_globals();
	if (1 /* need to suppress synthesis after reload */) {
	  start_form(key(K_SET));
	  add_to_form("synthesis-methods");
	  add_to_form("nil");
	  end_form();
	  newline_form();
	}
	if (module->def_all || module->def_scoring)
	  write_scorekeepers();
	doreshape = reshape_the_output(module);
	reshaper = module;
	if (module->def_all || module->def_world)
	  write_world();
	if (module->def_all || module->def_areas)
	  write_areas(module);
	if (module->def_all || module->def_sides)
	  write_doctrines();
	if (module->def_all || module->def_sides)
	  write_sides(module);
	if (module->def_all || module->def_players)
	  write_players();
	if (module->def_all || module->def_agreements)
	  write_agreements();
	if (module->def_all || module->def_units)
	  write_units(module);
	if (module->def_all || module->def_history)
	  write_history();
	if (module->def_all)
	  write_images();
	/* Write the game notes here (seems reasonable, no deeper reason). */
	if (module->instructions != lispnil) {
	    start_form(key(K_GAME_MODULE));
	    space_form();
	    write_lisp_prop(key(K_INSTRUCTIONS), module->instructions,
			    lispnil, FALSE, FALSE, TRUE);
	    newline_form();
	    space_form();
	    space_form();
	    end_form();
	    newline_form();
	    newline_form();
	}
	if (module->notes != lispnil) {
	    start_form(key(K_GAME_MODULE));
	    space_form();
	    write_lisp_prop(key(K_NOTES), module->notes,
			    lispnil, FALSE, FALSE, TRUE);
	    newline_form();
	    space_form();
	    space_form();
	    end_form();
	    newline_form();
	    newline_form();
	}
	if (module->designnotes != lispnil) {
	    start_form(key(K_GAME_MODULE));
	    space_form();
	    write_lisp_prop(key(K_DESIGN_NOTES), module->designnotes,
			    lispnil, FALSE, FALSE, TRUE);
	    newline_form();
	    space_form();
	    space_form();
	    end_form();
	    newline_form();
	    newline_form();
	}
	fclose(fp);
	return TRUE;
    } else {
	return FALSE;
    }
}

/* Write definitions of all the types. */

static void
write_types()
{
    int u, m, t, i, ival;
    char *typename, *sval;
    Obj *obj;

    /* (or write out all the default values first for doc, then
       only write changed values) */

    for_all_unit_types(u) {
	start_form(key(K_UNIT_TYPE));
	typename = shortest_escaped_name(u);
	add_to_form(typename);
	newline_form();
	space_form();
	for (i = 0; utypedefns[i].name != NULL; ++i) {
	    /* Don't write out props used internally only, unless debugging. */
	    if ((strncmp(utypedefns[i].name, "zz-", 3) == 0) && !Debug)
	      continue;
	    if (utypedefns[i].intgetter) {
		ival = (*(utypedefns[i].intgetter))(u);
		write_num_prop(utypedefns[i].name, ival,
			       utypedefns[i].dflt, FALSE, TRUE);
	    } else if (utypedefns[i].strgetter) {
		sval = (*(utypedefns[i].strgetter))(u);
		/* Special-case a couple possibly-redundant slots. */
		if (utypedefns[i].strgetter == u_type_name
		    && strcmp(typename, sval) == 0)
		  continue;
		if (utypedefns[i].strgetter == u_internal_name
		    && strcmp(typename, sval) == 0)
		  continue;
		write_str_prop(utypedefns[i].name, sval,
			       utypedefns[i].dfltstr, FALSE, TRUE);
	    } else {
		obj = (*(utypedefns[i].objgetter))(u);
		write_lisp_prop(utypedefns[i].name, obj,
				lispnil, FALSE, FALSE, TRUE);
	    }
	}
	space_form();
	end_form();
	newline_form();
    }
    newline_form();
    for_all_material_types(m) {
	start_form(key(K_MATERIAL_TYPE));
	add_to_form(escaped_symbol(m_type_name(m)));
	newline_form();
	space_form();
	for (i = 0; mtypedefns[i].name != NULL; ++i) {
	    /* Don't write out props used internally only, unless debugging. */
	    if ((strncmp(mtypedefns[i].name, "zz-", 3) == 0) && !Debug)
	      continue;
	    if (mtypedefns[i].intgetter) {
		ival = (*(mtypedefns[i].intgetter))(m);
		write_num_prop(mtypedefns[i].name, ival,
			       mtypedefns[i].dflt, FALSE, TRUE);
	    } else if (mtypedefns[i].strgetter) {
		sval = (*(mtypedefns[i].strgetter))(m);
		/* Special-case a a possibly-redundant slot. */
		if (mtypedefns[i].strgetter == m_type_name
		    && strcmp(typename, sval) == 0)
		  continue;
		write_str_prop(mtypedefns[i].name, sval,
			       mtypedefns[i].dfltstr, FALSE, TRUE);
	    } else {
		obj = (*(mtypedefns[i].objgetter))(m);
		write_lisp_prop(mtypedefns[i].name, obj,
				lispnil, FALSE, FALSE, TRUE);
	    }
	}
	space_form();
	end_form();
	newline_form();
    }
    newline_form();
    for_all_terrain_types(t) {
	start_form(key(K_TERRAIN_TYPE));
	add_to_form(escaped_symbol(t_type_name(t)));
	newline_form();
	space_form();
	for (i = 0; ttypedefns[i].name != NULL; ++i) {
	    /* Don't write out props used internally only, unless debugging. */
	    if ((strncmp(ttypedefns[i].name, "zz-", 3) == 0) && !Debug)
	      continue;
	    if (ttypedefns[i].intgetter) {
		ival = (*(ttypedefns[i].intgetter))(t);
		write_num_prop(ttypedefns[i].name, ival,
			       ttypedefns[i].dflt, FALSE, TRUE);
	    } else if (ttypedefns[i].strgetter) {
		sval = (*(ttypedefns[i].strgetter))(t);
		/* Special-case a a possibly-redundant slot. */
		if (ttypedefns[i].strgetter == t_type_name
		    && strcmp(typename, sval) == 0)
		  continue;
		write_str_prop(ttypedefns[i].name, sval,
			       ttypedefns[i].dfltstr, FALSE, TRUE);
	    } else {
		obj = (*(ttypedefns[i].objgetter))(t);
		write_lisp_prop(ttypedefns[i].name, obj,
				lispnil, FALSE, FALSE, TRUE);
	    }
	}
	space_form();
	end_form();
	newline_form();
    }
    newline_form();
}

/* Write definitions of all the tables. */

static void
write_tables(compress)
int compress;
{
    int tbl;

    newline_form();
    for (tbl = 0; tabledefns[tbl].name != 0; ++tbl) {
	/* Don't write out tables used internally only, unless debugging. */
	if ((strncmp(tabledefns[tbl].name, "zz-", 3) == 0) && !Debug)
	  continue;
	if (*(tabledefns[tbl].table) != NULL) {
	    write_table(tabledefns[tbl].name,
			tabledefns[tbl].getter, tabledefns[tbl].dflt,
			tabledefns[tbl].index1, tabledefns[tbl].index2, compress);
	    newline_form();
	}
    }
}

#define star_from_typ(typ)  \
  ((typ) == UTYP ? "u*" : ((typ) == MTYP ? "m*" :"t*"))

#define name_from_typ(typ, i)  \
  ((typ) == UTYP ? shortest_escaped_name(i) : ((typ) == MTYP ? m_type_name(i) : t_type_name(i)))

static void
write_type_name_list(typ, flags, dim)
int typ, *flags, dim;
{
    int j, first = TRUE, listlen = 0;

    if (flags == NULL)
      return;
    for (j = 0; j < dim; ++j)
      if (flags[j])
        ++listlen;
    if (listlen > 1)
      start_form("");
    for (j = 0; j < dim; ++j) {
	if (flags[j]) {
	    if (first)
	      first = FALSE;
	    else
	      space_form();
	    fprintf(fp, "%s", escaped_symbol(name_from_typ(typ, j)));
	}
    }
    if (listlen > 1)
      end_form();
}

#if 0
/* Write out a list of values in a table. */

static void
write_type_value_list(typ, flags, dim, getter, i)
int typ, *flags, dim, (*getter) PARAMS ((int, int)), i;
{
    int j, first = TRUE, listlen = 0;

    for (j = 0; j < dim; ++j)
      if (flags == NULL || flags[j])
        ++listlen;
    if (listlen > 1)
      start_form("");
    for (j = 0; j < dim; ++j) {
	if (flags == NULL || flags[j]) {
	    if (first)
	      first = FALSE;
	    else
	      space_form();
	    fprintf(fp, "%d", (*getter)(i, j));
	}
    }
    if (listlen > 1)
      end_form();
}
#endif

/* A simple histogram struct - count and value, that's all. */

struct histo { int count, val; };

/* Sort into *descending* order by count. */

static int
histo_compare(x, y)
CONST void *x, *y;
{
    return ((struct histo *) y)->count - ((struct histo *) x)->count;
}

/* Write out a single table.  Only write it if it contains non-default
   values, and try to find runs of constant value, since tables can be
   really large, but often have constant areas within them. */

static void
write_table(name, getter, dflt, typ1, typ2, compress)
char *name;
int (*getter) PARAMS ((int, int)), dflt, typ1, typ2, compress;
{
    int i, j, k, colvalue, constcol, next;
    int numrandoms, nextrowdiffers, writeconst;
    int sawfirst, constrands, constval;
    int dim1 = numtypes_from_index_type(typ1);
    int dim2 = numtypes_from_index_type(typ2);
    struct histo mostcommon[200]; /* needs to be more than MAXxTYPES */
    int indexes1[200], randoms[200];

    start_form(key(K_TABLE));
    add_to_form(name);
    fprintf(fp, "  ; %d", dflt);
    if (!compress) {
	/* Write every value separately. */
	for (i = 0; i < dim1; ++i) {
	    newline_form();
	    space_form();
	    space_form();
	    start_form(escaped_symbol(name_from_typ(typ1, i)));
	    add_to_form(star_from_typ(typ2));
	    space_form();
	    start_form("");
	    for (j = 0; j < dim2; ++j) {
		add_num_to_form((*getter)(i, j));
	    }
	    end_form();
	    end_form();
	}
    } else if (dim1 <= dim2) {
        /* Analyze the table by rows. */
	for (k = 0; k < dim1; ++k)
	  indexes1[k] = FALSE;
	for (i = 0; i < dim1; ++i) {
	    /* First see if this row has all the same values as the next. */
	    indexes1[i] = TRUE;
	    nextrowdiffers = FALSE;
	    if (i < dim1 - 1) {
	    	for (j = 0; j < dim2; ++j) {
	    	    if ((*getter)(i, j) != (*getter)(i + 1, j)) {
	    	    	nextrowdiffers = TRUE;
	    	    	break;
	    	    }
	    	}
	    } else {
	    	/* The last row is *always* "different". */
	    	nextrowdiffers = TRUE;
	    }
	    /* (should look at *all* rows to find matching rows before
	       dumping one) */
	    if (nextrowdiffers) {
		/* Make a histogram of all the values in this row. */
		mostcommon[0].count = 1;
		mostcommon[0].val = (*getter)(i, 0);
		next = 1;
		for (j = 0; j < dim2; ++j) {
		    for (k = 0; k < next; ++k) {
			if (mostcommon[k].val == (*getter)(i, j)) {
			    ++(mostcommon[k].count);
			    break;
			}
		    }
		    if (k == next) {
			mostcommon[next].count = 1;
			mostcommon[next].val = (*getter)(i, j);
			++next;
		    }
		}
		if (next == 1 && mostcommon[0].val == dflt) {
		    /* Entire row(s) is/are just the default table value. */
		} else {
		    writeconst = FALSE;
		    numrandoms = 0;
		    if (next == 1) {
			/* Only one value in the row(s). */
			writeconst = TRUE;
		    } else {
			qsort(mostcommon, next, sizeof(struct histo),
			      histo_compare);
			if (mostcommon[0].count >= (3 * dim2) / 4) {
			    /* The most common value in this row(s) is not the only value,
			       but it is worth writing into a separate clause. */
			    writeconst = TRUE;
			    for (j = 0; j < dim2; ++j) {
				/* Flag the other values as needing to be
				   written separately. */
				randoms[j] =
				  (mostcommon[0].val != (*getter)(i, j));
				if (randoms[j])
				  ++numrandoms;
			    }
			} else {
			    /* Flag all in the row as randoms. */
			    for (j = 0; j < dim2; ++j) {
				randoms[j] = TRUE;
				++numrandoms;
			    }
			}
		    }
		    /* Write out the most common value (if non-default) in the row(s),
		       expressing it with a clause that applies the value
		       to the entire row(s). */
		    if (writeconst && mostcommon[0].val != dflt) {
			newline_form();
			space_form();
			space_form();
			start_form("");
			write_type_name_list(typ1, indexes1, dim1);
			add_to_form(star_from_typ(typ2));
			add_num_to_form(mostcommon[0].val);
			end_form();
		    }
		    /* Now override the most common value with any
		       exceptions. */
		    if (numrandoms > 0) {
			constrands = TRUE;
			sawfirst = FALSE;
			for (j = 0; j < dim2; ++j) {
			    if (randoms[j]) {
			        if (!sawfirst) {
				    constval = (*getter)(i, j);
				    sawfirst = TRUE;
			        }
			        if (sawfirst && constval != (*getter)(i, j)) {
				    constrands = FALSE;
				    break;
			        }
			    }
			}
			if (constrands) {
			    newline_form();
			    space_form();
			    space_form();
			    start_form("");
			    write_type_name_list(typ1, indexes1, dim1);
			    space_form();
			    write_type_name_list(typ2, randoms, dim2);
			    add_num_to_form(constval);
			    end_form();
			} else {
			    /* We have a group of rows with varying data
			       in the columns; write a separate row. */
			    for (j = 0; j < dim2; ++j) {
				if (randoms[j]) {
				    newline_form();
				    space_form();
				    space_form();
				    start_form("");
				    write_type_name_list(typ1, indexes1, dim1);
				    add_to_form(escaped_symbol(name_from_typ(typ2, j)));
				    add_num_to_form((*getter)(i, j));
				    end_form();
				}
			    }
			}
		    }
		}
		/* Reset the row flags in preparation for the next group
		   of rows whose contents match each other. */
		for (k = 0; k < dim1; ++k)
		  indexes1[k] = FALSE;
	    }
	}
    } else {
        /* Analyze the table by columns. */
        /* Don't work as hard to optimize; this case should be uncommon,
	   since there are usually more types of units than
	   materials or terrain. */
	for (j = 0; j < dim2; ++j) {
	    constcol = TRUE;
	    colvalue = (*getter)(0, j);
	    for (i = 0; i < dim1; ++i) {
		if ((*getter)(i, j) != colvalue) {
		    constcol = FALSE;
		    break;
		}
	    }
	    if (!constcol || colvalue != dflt) {
		newline_form();
		space_form();
		space_form();
		start_form(star_from_typ(typ1));
		add_to_form(escaped_symbol(name_from_typ(typ2, j)));
		/* Write out either a single constant value or a list of
		   varying values, as appropriate. */
		if (constcol) {
		    add_num_to_form(colvalue);
		} else {
		    space_form();
		    start_form("");
		    for (i = 0; i < dim1; ++i) {
			add_num_to_form((*getter)(i, j));
		    }
		    end_form();
		}
		end_form();
	    }
	}
    }
    newline_form();
    space_form();
    space_form();
    end_form();
    newline_form();
}

/* Write info about the whole world. */

static void
write_world()
{
    newline_form();
    start_form(key(K_WORLD));
    /* K_CIRCUMFERENCE always written. */
    add_num_to_form((doreshape ? reshaper->final_circumference : world.circumference));
    write_num_prop(key(K_DAY_LENGTH), world.daylength, 1, FALSE, FALSE);
    write_num_prop(key(K_YEAR_LENGTH), world.yearlength, 1, FALSE, FALSE);
    write_num_prop(key(K_AXIAL_TILT), world.axialtilt, 0, FALSE, FALSE);
    end_form();
    newline_form();
}

/* Write info about the area in the world.  This code uses run-length encoding
   to reduce the size of each written layer as much as possible.  Note
   also that each layer is written as a separate form, so that the Lisp
   reader doesn't have to read really large forms back in. */

static void
write_areas(module)
Module *module;
{
    int all = module->def_all, compress = module->compress_layers;

    newline_form();
    /* Write the basic dimensions. */
    start_form(key(K_AREA));
    /* K_WIDTH, K_HEIGHT always written. */
    add_num_to_form((doreshape ? reshaper->final_width : area.width));
    add_num_to_form((doreshape ? reshaper->final_height : area.height));
    /* Write all the scalar properties. */
    write_num_prop(key(K_LATITUDE), area.latitude, 0, 0, FALSE);
    write_num_prop(key(K_LONGITUDE), area.longitude, 0, 0, FALSE);
    write_num_prop(key(K_CELL_WIDTH), area.cellwidth, 0, 0, FALSE);
    end_form();
    newline_form();
    /* Write the area's layers, each as a separate form. */
    if (all || module->def_area_terrain)
      write_area_terrain(compress);
    if (all || module->def_area_terrain)
      write_area_aux_terrain(compress);
    if (all || module->def_area_misc)
      write_area_features(compress);
    if (all || module->def_area_misc)
      write_area_elevations(compress);
    if (all || module->def_area_misc)
      write_area_people_sides(compress);
    if (all || module->def_area_material)
      write_area_materials(compress);
    if (all || module->def_area_weather)
      write_area_temperatures(compress);
    if (all || module->def_area_weather)
      write_area_clouds(compress);
    if (all || module->def_area_weather)
      write_area_winds(compress);
}

static void
write_area_terrain(compress)
int compress;
{
    int t;

    start_form(key(K_AREA));
    space_form();
    start_form(key(K_TERRAIN));
    newline_form();
    space_form();
    start_form(key(K_BY_NAME));
    for_all_terrain_types(t) {
	/* Break the list into groups of 5 per line. */
    	if (t % 5 == 0) {
	    newline_form();
	    space_form();
	    space_form();
	    space_form();
	}
	space_form();
	start_form(escaped_symbol(t_type_name(t)));
	add_num_to_form(t);
	end_form();
    }
    end_form();
    newline_form();
    write_rle(fn_terrain_at, 0, numttypes - 1, NULL, compress);
    space_form();
    space_form();
    end_form();
    end_form();
    newline_form();
}

static void
write_area_aux_terrain(compress)
int compress;
{
    int t;

    for_all_terrain_types(t) {
	if (aux_terrain_defined(t)) {
	    start_form(key(K_AREA));
	    space_form();
	    start_form(key(K_AUX_TERRAIN));
	    add_to_form(escaped_symbol(t_type_name(t)));
	    newline_form();
	    tmpttype = t;
	    write_rle(fn_aux_terrain_at, 0, 127, NULL, compress);
	    space_form();
	    space_form();
	    end_form();
	    end_form();
	    newline_form();
	}
    }
}

static void
write_area_features(compress)
int compress;
{
    Feature *feature;

    if (featurelist == NULL || !features_defined())
      return;
    start_form(key(K_AREA));
    space_form();
    start_form(key(K_FEATURES));
    space_form();
    start_form("");
    newline_form();
    /* Dump out the list of features first. */
    for (feature = featurelist; feature != NULL; feature = feature->next) {
	space_form();
	space_form();
	space_form();
	start_form("");
	add_num_to_form(feature->id);
	add_to_form(escaped_string(feature->typename));
	add_to_form(escaped_string(feature->name));
	end_form();
	newline_form();
    }
    space_form();
    space_form();
    end_form();
    newline_form();
    /* Now record which features go with which cells. */
    write_rle(fn_feature_at, 0, -1, NULL, compress);
    space_form();
    space_form();
    end_form();
    end_form();
    newline_form();
}

static int
fn_elevation_at_offset(x, y)
int x, y;
{
    return (elev_at(x, y) - area.minelev);
}

static void
write_area_elevations(compress)
int compress;
{
    if (!elevations_defined())
      return;
    start_form(key(K_AREA));
    space_form();
    start_form(key(K_ELEVATIONS));
    if (area.minelev != 0) {
	space_form();
	start_form(key(K_XFORM));
	add_num_to_form(1);
	add_num_to_form(area.minelev);
	end_form();
    }
    newline_form();
    write_rle(fn_elevation_at_offset, minelev - area.minelev, maxelev, NULL, compress);
    space_form();
    space_form();
    end_form();
    end_form();
    newline_form();
}

/* Write out people sides for the area. */

static void
write_area_people_sides(compress)
int compress;
{
    Side *side;

    if (!people_sides_defined())
      return;
    start_form(key(K_AREA));
    space_form();
    start_form(key(K_PEOPLE_SIDES));
    newline_form();
    space_form();
    space_form();
    start_form(key(K_BY_NAME));
    for_all_sides(side) {
	/* Break the list into groups of 5 per line. */
	if (side_number(side) % 5 == 0) {
	    newline_form();
	    space_form();
	    space_form();
	    space_form();
	}
	space_form();
	start_form("");
	add_num_to_form(side_number(side)); /* should be symbol */
	add_num_to_form(side_number(side));
	end_form();
    }
    end_form();
    newline_form();
    /* Value of NOBODY is guaranteed to be the largest valid. */
    write_rle(fn_people_side_at, 0, NOBODY, NULL, compress);
    space_form();
    space_form();
    end_form();
    end_form();
    newline_form();
}

static void
write_area_materials(compress)
int compress;
{
    int m;

    if (any_cell_materials_defined()) {
	for_all_material_types(m) {
	    if (cell_material_defined(m)) {
		start_form(key(K_AREA));
		space_form();
		start_form(key(K_MATERIAL));
		add_to_form(escaped_symbol(m_type_name(m)));
		newline_form();
		tmpmtype = m;
		write_rle(fn_material_at, 0, 127, NULL, compress);
		end_form();
		end_form();
		newline_form();
	    }
	}
    }
}

static void
write_area_temperatures(compress)
int compress;
{
    if (!temperatures_defined())
      return;
    start_form(key(K_AREA));
    space_form();
    start_form(key(K_TEMPERATURES));
    newline_form();
    write_rle(fn_temperature_at, 0, 9999, NULL, compress);
    space_form();
    space_form();
    end_form();
    end_form();
    newline_form();
}

static void
write_area_clouds(compress)
int compress;
{
    if (clouds_defined()) {
	start_form(key(K_AREA));
	space_form();
	start_form(key(K_CLOUDS));
	newline_form();
	write_rle(fn_raw_cloud_at, 0, 127, NULL, compress);
	space_form();
	space_form();
	end_form();
	end_form();
	newline_form();
    }
    if (cloud_bottoms_defined()) {
	start_form(key(K_AREA));
	space_form();
	start_form(key(K_CLOUD_BOTTOMS));
	newline_form();
	write_rle(fn_raw_cloud_bottom_at, 0, 9999, NULL, compress);
	space_form();
	space_form();
	end_form();
	end_form();
	newline_form();
    }
    if (cloud_heights_defined()) {
	start_form(key(K_AREA));
	space_form();
	start_form(key(K_CLOUD_HEIGHTS));
	newline_form();
	write_rle(fn_raw_cloud_height_at, 0, 9999, NULL, compress);
	space_form();
	space_form();
	end_form();
	end_form();
	newline_form();
    }
}

static void
write_area_winds(compress)
int compress;
{
    if (winds_defined()) {
	start_form(key(K_AREA));
	space_form();
	start_form(key(K_WINDS));
	newline_form();
	write_rle(fn_raw_wind_at, -32767, 32767, NULL, compress);
	space_form();
	space_form();
	end_form();
	end_form();
	newline_form();
    }
}

/* Write the globals.  The "complete" flag forces all values out, even
   if they match the compiled-in defaults. */

static void
write_globals()
{
    int i, complete = FALSE;
    time_t now;

    /* Snapshot realtime values. */
    time(&now);
    set_g_elapsed_time(idifftime(now, game_start_in_real_time));

    newline_form();

    for (i = 0; vardefns[i].name != 0; ++i) {
	if (vardefns[i].intgetter != NULL) {
	    if (complete || (*(vardefns[i].intgetter))() != vardefns[i].dflt) {
		start_form(key(K_SET));
		add_to_form(vardefns[i].name);
		add_num_to_form((*(vardefns[i].intgetter))());
		end_form();
		newline_form();
	    }
	} else if (vardefns[i].strgetter != NULL) {
	    if (complete || string_not_default((*(vardefns[i].strgetter))(), vardefns[i].dfltstr)) {
		start_form(key(K_SET));
		add_to_form(vardefns[i].name);
		add_to_form(escaped_string((*(vardefns[i].strgetter))()));
		end_form();
		newline_form();
	    }
	} else if (vardefns[i].objgetter != NULL) {
	    if (complete
		|| (vardefns[i].dfltfn == NULL
		    && (*(vardefns[i].objgetter))() != lispnil)) {
		if ((*(vardefns[i].objgetter))() != NULL) {
		    start_form(key(K_SET));
		    add_to_form(vardefns[i].name);
		    space_form();
		    /* Suppress evaluation upon readin. */
		    fprintf(fp, "'");
		    fprintlisp(fp, (*(vardefns[i].objgetter))());
		    end_form();
		    newline_form();
		} else {
		    fprintf(fp, "; %s was unbound?\n", vardefns[i].name);
		}
	    }
	} else {
	    abort();
	}
    }
}

/* Write all the scorekeepers. */

static void
write_scorekeepers()
{
    Scorekeeper *sk;

    for_all_scorekeepers(sk) {
	start_form(key(K_SCOREKEEPER));
	add_num_to_form(sk->id);
	newline_form();
	space_form();
	write_str_prop(key(K_TITLE), sk->title, "", FALSE, TRUE);
	write_lisp_prop(key(K_WHEN), sk->when, lispnil, FALSE, FALSE, TRUE);
	write_lisp_prop(key(K_APPLIES_TO), sk->who, lispnil, FALSE, FALSE, TRUE);
	write_lisp_prop(key(K_KNOWN_TO), sk->who, lispnil, FALSE, FALSE, TRUE);
	write_lisp_prop(key(K_TRIGGER), sk->trigger, lispnil, FALSE, FALSE, TRUE);
	write_lisp_prop(key(K_DO), sk->body, lispnil, FALSE, FALSE, TRUE);
	write_num_prop(key(K_TRIGGERED), sk->triggered, 0, FALSE, TRUE); 
	write_num_prop(key(K_INITIAL), sk->initial, -10001, FALSE, TRUE); 
	write_lisp_prop(key(K_NOTES), sk->notes, lispnil, FALSE, FALSE, TRUE);
	space_form();
	end_form();
	newline_form();
    }
}

static void
write_doctrines()
{
    Doctrine *doc;

    for (doc = doctrine_list; doc != NULL; doc = doc->next) {
	start_form(key(K_DOCTRINE));
	if (doc->name)
	  add_to_form(escaped_symbol(doc->name));
	else
	  add_num_to_form(doc->id);
	newline_form();
	space_form();
	write_num_prop(key(K_EVER_ASK_SIDE), doc->everaskside, 0, FALSE, TRUE); 
	write_utype_value_list(key(K_CONSTRUCTION_RUN), doc->construction_run, 0, TRUE);
	write_num_prop(key(K_LOCKED), doc->locked, 0, FALSE, TRUE); 
	space_form();
	end_form();
	newline_form();
    }
}

/* Write declarations of all the sides. */

static void
write_sides(module)
Module *module;
{
    Side *side;

    fprintf(fp, "\n; %d sides\n", numsides);
    Dprintf("Will try to write %d sides ...\n", numsides);
    for_all_sides(side) {
	start_form(key(K_SIDE));
	add_num_to_form(side->id);
	if (symbolp(side->symbol))
	  add_to_form(escaped_symbol(c_string(side->symbol)));
	newline_form();
	space_form();
	write_side_properties(side);
	space_form();
	end_form();
	newline_form();
	if (module->def_all || module->def_side_views)
	  write_side_view(side, module->compress_layers);
	Dprintf("  Wrote side %s\n", side_desig(side));
    }
    start_form(key(K_INDEPENDENT_UNITS));
    newline_form();
    write_side_properties(indepside);
    space_form();
    end_form();
    newline_form();
    Dprintf("  Wrote independent unit properties\n");
    Dprintf("... Done writing sides\n");
}

/* Write random properties of a side. */

static void
write_side_properties(side)
Side *side;
{
    int i, u, u2, anyudoctrines;

    write_str_prop(key(K_NAME), side->name, "", FALSE, TRUE);
    write_str_prop(key(K_LONG_NAME), side->longname, "", FALSE, TRUE);
    write_str_prop(key(K_SHORT_NAME), side->shortname, "", FALSE, TRUE);
    write_str_prop(key(K_NOUN), side->noun, "", FALSE, TRUE);
    write_str_prop(key(K_PLURAL_NOUN), side->pluralnoun, "", FALSE, TRUE);
    write_str_prop(key(K_ADJECTIVE), side->adjective, "", FALSE, TRUE);
    write_str_prop(key(K_COLOR), side->colorscheme, "", FALSE, TRUE);
    write_str_prop(key(K_EMBLEM_NAME), side->emblemname, "", FALSE, TRUE);
    write_utype_string_list(key(K_UNIT_NAMERS), side->unitnamers, "", TRUE);
    write_str_prop(key(K_CLASS), side->sideclass, "", FALSE, TRUE);
    write_bool_prop(key(K_ACTIVE), side->ingame, 1, FALSE, TRUE);
    write_bool_prop(key(K_EVER_ACTIVE), side->everingame, 1, FALSE, TRUE);
    write_num_prop(key(K_PRIORITY), side->priority, 0, FALSE, TRUE);
    write_num_prop(key(K_STATUS), side->status, 0, FALSE, TRUE);
    write_num_prop(key(K_TURN_TIME_USED), side->turntimeused, 0, FALSE, TRUE);
    write_num_prop(key(K_TOTAL_TIME_USED), side->totaltimeused, 0, FALSE, TRUE);
    write_num_prop(key(K_TIMEOUTS), side->timeouts, 0, FALSE, TRUE);
    write_num_prop(key(K_TIMEOUTS_USED), side->timeoutsused, 0, FALSE, TRUE);
    write_num_prop(key(K_FINISHED_TURN), side->finishedturn, 0, FALSE, TRUE);
    write_num_prop(key(K_WILLING_TO_DRAW), side->willingtodraw, 0, FALSE, TRUE);
    write_num_prop(key(K_ADVANTAGE), side->advantage, 1, FALSE, TRUE);
    write_num_prop(key(K_ADVANTAGE_MIN), side->minadvantage, 1, FALSE, TRUE);
    write_num_prop(key(K_ADVANTAGE_MAX), side->maxadvantage, 9999, FALSE, TRUE);
    write_num_prop(key(K_CONTROLLED_BY), side_number(side->controlled_by), 0, FALSE, TRUE);
    write_num_prop(key(K_SELF_UNIT), (side->self_unit ? side->self_unit->id : 0), 0, FALSE, TRUE);
    write_num_prop(key(K_PLAYER), (side->player ? side->player->id : 0), 0, FALSE, TRUE);
    write_num_prop(key(K_DEFAULT_DOCTRINE),
    		   (side->default_doctrine ? side->default_doctrine->id : 0), 0, FALSE, TRUE);
    anyudoctrines = FALSE;
    for_all_unit_types(u) {
	if (side->udoctrine[u] != side->default_doctrine) {
	    anyudoctrines = TRUE;
	    break;
	}
    }
    if (anyudoctrines) {
	space_form();
	start_form(key(K_DOCTRINES));
	for_all_unit_types(u) {
	    if (side->udoctrine[u] != side->default_doctrine) {
		space_form();
		start_form(escaped_symbol(u_type_name(u)));
		if (side->udoctrine[u]->name)
		  add_to_form(escaped_symbol(side->udoctrine[u]->name));
		else
		  add_num_to_form(side->udoctrine[u]->id);
		end_form();
	    }
	}
	end_form();
	newline_form();
	space_form();
    }
    write_side_value_list(key(K_TRUSTS), side->trusts, FALSE, TRUE);
    write_side_value_list(key(K_TRADES), side->trades, FALSE, TRUE);
    write_utype_value_list(key(K_START_WITH), side->startwith, 0, TRUE);
    write_utype_value_list(key(K_NEXT_NUMBERS), side->counts, 0, TRUE);
    write_utype_value_list(key(K_TECH), side->tech, 0, TRUE);
    write_utype_value_list(key(K_INIT_TECH), side->inittech, 0, TRUE);
    if (side->scores) {
	space_form();
	start_form(key(K_SCORES));
	for (i = 0; i < numscores; ++i) {
	    add_num_to_form(side->scores[i]);
	}
	end_form();
	newline_form();
	space_form();
    }
    /* Write out statistics. */
    if (side->gaincounts != NULL) {
	space_form();
	start_form(key(K_GAIN_COUNTS));
	for (i = 0; i < numutypes * num_gain_reasons; ++i) {
	    add_num_to_form(side->gaincounts[i]);
	}
	end_form();
	newline_form();
	space_form();
    }
    if (side->losscounts != NULL) {
	space_form();
	start_form(key(K_LOSS_COUNTS));
	for (i = 0; i < numutypes * num_loss_reasons; ++i) {
	    add_num_to_form(side->losscounts[i]);
	}
	end_form();
	newline_form();
	space_form();
    }
    if (side->atkstats != NULL) {
	space_form();
	start_form(key(K_ATTACK_STATS));
	for_all_unit_types(u) {
	    if (side->atkstats[u] != NULL) {
		newline_form();
		space_form();
		space_form();
		space_form();
		start_form(u_type_name(u));
		for_all_unit_types(u2) {
		    add_num_to_form(side_atkstats(side, u, u2));
		}
		end_form();
	    }
	}
	end_form();
	newline_form();
	space_form();
    }
    if (side->hitstats != NULL) {
	space_form();
	start_form(key(K_HIT_STATS));
	for_all_unit_types(u) {
	    if (side->hitstats[u] != NULL) {
		newline_form();
		space_form();
		space_form();
		space_form();
		start_form(u_type_name(u));
		for_all_unit_types(u2) {
		    add_num_to_form(side_hitstats(side, u, u2));
		}
		end_form();
	    }
	}
	end_form();
	newline_form();
	space_form();
    }
    /* Have the AI paste its useful state into distinct element of
       side->aidata. */
    if (side_has_ai(side)) {
	ai_save_state(side);
    }
    write_lisp_prop(key(K_AI_DATA), side->aidata, lispnil, FALSE, TRUE, TRUE);
    /* Have the interface paste its useful state into distinct element
       of side->uidata. */
    if (side_has_display(side)) {
	ui_save_state(side);
    }
    write_lisp_prop(key(K_UI_DATA), side->uidata, lispnil, FALSE, TRUE, TRUE);
}

/* Write about what has been seen in the area. */

/* (should have option to spec symbolic dict of sides and units) */

static int
fn_terrain_view(x, y)
int x, y;
{
    return terrain_view(tmpside, x, y);
}

static int
fn_terrain_view_date(x, y)
int x, y;
{
    return terrain_view_date(tmpside, x, y);
}

static int
fn_aux_terrain_view(x, y)
int x, y;
{
    return aux_terrain_view(tmpside, x, y, tmpttype);
}

static int
fn_aux_terrain_view_date(x, y)
int x, y;
{
    return aux_terrain_view_date(tmpside, x, y, tmpttype);
}

static int
fn_unit_view(x, y)
int x, y;
{
    return unit_view(tmpside, x, y);
}

static int
fn_unit_view_date(x, y)
int x, y;
{
    return unit_view_date(tmpside, x, y);
}

static int
fn_material_view(x, y)
int x, y;
{
    return material_view(tmpside, x, y, tmpmtype);
}

static int
fn_material_view_date(x, y)
int x, y;
{
    return material_view_date(tmpside, x, y, tmpmtype);
}

static int
fn_temp_view(x, y)
int x, y;
{
    return temperature_view(tmpside, x, y);
}

static int
fn_temp_view_date(x, y)
int x, y;
{
    return temperature_view_date(tmpside, x, y);
}

static int
fn_cloud_view(x, y)
int x, y;
{
    return cloud_view(tmpside, x, y);
}

static int
fn_cloud_bottom_view(x, y)
int x, y;
{
    return cloud_bottom_view(tmpside, x, y);
}

static int
fn_cloud_height_view(x, y)
int x, y;
{
    return cloud_height_view(tmpside, x, y);
}

static int
fn_cloud_view_date(x, y)
int x, y;
{
    return cloud_view_date(tmpside, x, y);
}

static int
fn_wind_view(x, y)
int x, y;
{
    return wind_view(tmpside, x, y);
}

static int
fn_wind_view_date(x, y)
int x, y;
{
    return wind_view_date(tmpside, x, y);
}

static void
write_side_view(side, compress)
Side *side;
int compress;
{
    int t, m;

    /* View layers are not defined if see-all is in effect. */
    if (all_see_all)
      return;
    tmpside = side;
    tmpcompress = compress;
    write_one_side_view_layer(K_TERRAIN_VIEW, fn_terrain_view);
    if (!g_see_terrain_always()) {
	if (side->terrview)
	  write_one_side_view_layer(K_TERRAIN_VIEW_DATES, fn_terrain_view_date);
	if (numcelltypes < numttypes) {
	  for_all_terrain_types(t) {
	    if (!t_is_cell(t)) {
		tmpttype = t;
		if (side->auxterrview[t])
		  write_one_side_view_layer(K_AUX_TERRAIN_VIEW, fn_aux_terrain_view);
		if (side->auxterrviewdate[t])
		  write_one_side_view_layer(K_AUX_TERRAIN_VIEW_DATES, fn_aux_terrain_view_date);
	    }
	  }
        }
    }
    if (0 /* have material views */) {
	for_all_material_types(m) {
	    if (0) {
		tmpmtype = m;
		if (side->materialview[m])
		  write_one_side_view_layer(K_MATERIAL_VIEW, fn_material_view);
		if (side->materialviewdate[m])
		  write_one_side_view_layer(K_MATERIAL_VIEW_DATES, fn_material_view_date);
	    }
	}
    }
    write_one_side_view_layer(K_UNIT_VIEW, fn_unit_view);
    write_one_side_view_layer(K_UNIT_VIEW_DATES, fn_unit_view_date);
    if (!g_see_weather_always()) {
      if (any_temp_variation) {
	write_one_side_view_layer(K_TEMPERATURE_VIEW, fn_temp_view);
	write_one_side_view_layer(K_TEMPERATURE_VIEW_DATES, fn_temp_view_date);
      }
      if (any_clouds) {
	write_one_side_view_layer(K_CLOUD_VIEW, fn_cloud_view);
	write_one_side_view_layer(K_CLOUD_BOTTOM_VIEW, fn_cloud_bottom_view);
	write_one_side_view_layer(K_CLOUD_HEIGHT_VIEW, fn_cloud_height_view);
	write_one_side_view_layer(K_CLOUD_VIEW_DATES, fn_cloud_view_date);
      }
      if (any_wind_variation) {
	write_one_side_view_layer(K_WIND_VIEW, fn_wind_view);
	write_one_side_view_layer(K_WIND_VIEW_DATES, fn_wind_view_date);
      }
    }
}

static void
write_one_side_view_layer(propkey, fn)
int propkey, (*fn) PARAMS ((int x, int y));
{
    newline_form();
    start_form(key(K_SIDE));
    add_num_to_form(tmpside->id);
    space_form();
    start_form(key(propkey));
    newline_form();
    write_rle(fn, -32767, 32767, NULL, tmpcompress);
    space_form();
    space_form();
    end_form();
    end_form();
    newline_form();
}

static void
write_players()
{
    Side *side;

    Dprintf("Will try to write players ...\n");
    for_all_sides(side) {
	if (side->player != NULL) {
	    write_player(side->player);
	    Dprintf("Wrote player %s,\n", player_desig(side->player));
	}
    }
    Dprintf("... Done writing players.\n");
}

static void
write_player(player)
Player *player;
{
    start_form(key(K_PLAYER));
    add_num_to_form(player->id);
    newline_form();
    space_form();
    write_str_prop(key(K_NAME), player->name, "", FALSE, TRUE);
    write_str_prop(key(K_CONFIG_NAME), player->configname, "", FALSE, TRUE);
    write_str_prop(key(K_DISPLAY_NAME), player->displayname, "", FALSE, TRUE);
    write_str_prop(key(K_AI_TYPE_NAME), player->aitypename, "", FALSE, TRUE);
    space_form();
    end_form();
    newline_form();
}

static void
write_agreements()
{
    Agreement *agreement;

    for (agreement = agreementlist; agreement != NULL; agreement = agreement->next) {
	write_agreement(agreement);
    }
}

static void
write_agreement(agreement)
Agreement *agreement;
{
    start_form(key(K_AGREEMENT));
    add_num_to_form(agreement->id);
    newline_form();
    space_form();
    write_str_prop(key(K_TYPE_NAME), agreement->typename, "", FALSE, TRUE);
    write_str_prop(key(K_NAME), agreement->name, "", FALSE, TRUE);
    write_num_prop(key(K_STATE), agreement->state, 0, FALSE, TRUE);
    write_lisp_prop(key(K_TERMS), agreement->terms, lispnil, FALSE, FALSE, TRUE);
    write_num_prop(key(K_DRAFTERS), agreement->drafters, 0, FALSE, TRUE);
    write_num_prop(key(K_PROPOSERS), agreement->proposers, 0, FALSE, TRUE);
    write_num_prop(key(K_SIGNERS), agreement->signers, 0, FALSE, TRUE);
    write_num_prop(key(K_WILLING_TO_SIGN), agreement->willing, 0, FALSE, TRUE);
    write_num_prop(key(K_KNOWN_TO), agreement->knownto, 0, FALSE, TRUE);
    write_num_prop(key(K_ENFORCEMENT), agreement->enforcement, 0, FALSE, TRUE);
    space_form();
    end_form();
    newline_form();
}

/* Should write out "unit groups" with dict prepended, then can use with
   multiple games */

/* Write the unit section of a game module. */

static void
write_units(module)
Module *module;
{
    int x0, y0, x, y, numtowrite;
    Unit *unit;
    Side *loopside;

    /* Make sure no dead units get saved. */
    flush_dead_units();
    /* Make a consistent ordering. */
    sort_units();
    numtowrite = 0;
    for_all_sides_plus_indep(loopside) {
	for_all_side_units(loopside, unit) {
	    if (alive(unit))
	      ++numtowrite;
	}
    }
    fprintf(fp, "; %d units\n", numtowrite);
    /* Need to write out the defaults being assumed subsequently. */
    /* maybe use those in postprocessing. */
    start_form(key(K_UNIT_DEFAULTS));
    end_form();
    newline_form();
    Dprintf("Writing %d units ...\n", numliveunits);
    for_all_sides_plus_indep(loopside) {
	for_all_side_units(loopside, unit) {
	    if (alive(unit)) {
		/* K_AT always written */
		/* K_S always written */
		/* If the unit will appear later, must write out that
		   later position, possibly mapped to a new place if
		   the map is being reshaped. */
		if (unit->cp < 0 && unit_appear_turn(unit) >= 0) {
		    x0 = (- unit->prevx);  y0 = (- unit->prevy);
		} else {
		    x0 = unit->x;  y0 = unit->y;
		}
		if (doreshape) {
		    reshaped_point(x0, y0, &x, &y);
		} else {
		    x = x0;  y = y0;
		}
		/* If these were negative values made positive for the
		   purposes of reshaping, make them negative again. */
		if (unit->cp < 0 && unit_appear_turn(unit) >= 0) {
		    x = (- x);  y = (- y);
		}
		start_form(shortest_escaped_name(unit->type));
		add_num_to_form(x);
		add_num_to_form(y);
		/* (should be able to write sides symbolically) */
		add_num_to_form(side_number(unit->side));
		write_num_prop(key(K_Z), unit->z, 0, FALSE, FALSE);
		write_str_prop(key(K_N), unit->name, NULL, FALSE, FALSE);
		write_num_prop(key(K_OS), side_number(unit->origside), side_number(unit->side), FALSE, FALSE);
		/* Maybe write the unit's id. */
		if (module->def_all || module->def_unit_ids || unit->occupant)
		  write_num_prop(key(K_SHARP), unit->id, 0, FALSE, FALSE);
		/* Need this to get back into the right transport. */
		if (unit->transport)
		  write_num_prop(key(K_IN), unit->transport->id, 0, FALSE, FALSE);
		/* Write optional info about the units. */
		if (module->def_all || module->def_unit_props)
		  write_unit_properties(unit);
		if (module->def_all || module->def_unit_acts)
		  write_unit_act(unit);
		if (module->def_all || module->def_unit_plans)
		  write_unit_plan(unit);
		/* close the unit out */
		end_form();
		newline_form();
		Dprintf("Wrote %s\n", unit_desig(unit));
	    }
	}
	newline_form();
    }
    Dprintf("... Done writing units.\n");
}

/* Write random properties, but only if they have non-default values. */

static void
write_unit_properties(unit)
Unit *unit;
{
    write_num_prop(key(K_NB), unit->number, 0, FALSE, FALSE);
    write_num_prop(key(K_HP), unit->hp, u_hp(unit->type), FALSE, FALSE);
    write_num_prop(key(K_CP), unit->cp, u_cp(unit->type), FALSE, FALSE);
    write_num_prop(key(K_CXP), unit->cxp, 0, FALSE, FALSE);
    write_num_prop(key(K_MO), unit->morale, 0, FALSE, FALSE);
    write_utype_value_list(key(K_TP), unit->tooling, 0, FALSE);
    write_side_value_list(key(K_OPINIONS), unit->opinions, FALSE, FALSE);
    write_mtype_value_list(key(K_M), unit->supply, 0, FALSE);
    write_num_prop("point-value", unit_point_value(unit), -1, FALSE, FALSE);
    write_num_prop(key(K_APPEAR), unit_appear_turn(unit), -1, FALSE, FALSE);
    /* (should do appear x,y also here?) */
    write_num_prop(key(K_DISAPPEAR), unit_disappear_turn(unit), -1, FALSE, FALSE);
    write_lisp_prop(key(K_X), unit_hook(unit), lispnil, FALSE, TRUE, FALSE);
}

/* Write out the unit's current actor state. */

static void
write_unit_act(unit)
Unit *unit;
{
    int acp = u_acp(unit->type), atype, i, slen;
    ActorState *act = unit->act;

    /* Actor state is kind of meaningless for dead units. */
    if (!alive(unit))
      return;
    if (act != NULL
	&& (act->acp != acp
	    || act->initacp != acp
	    || act->nextaction.type != ACTION_NONE)) {
	if (1) {
	   newline_form();
	   space_form();
	}
	space_form();
	start_form(key(K_ACT));
	if (act->acp != acp)
	  write_num_prop(key(K_ACP), act->acp, acp, FALSE, FALSE);
	if (act->initacp != acp)
	  write_num_prop(key(K_ACP0), act->initacp, acp, FALSE, FALSE);
	if (act->nextaction.type != ACTION_NONE) {
	    atype = act->nextaction.type;
	    space_form();
	    start_form(key(K_A));
	    add_to_form(actiondefns[atype].name);
	    slen = strlen(actiondefns[atype].argtypes);
    	    for (i = 0; i < slen; ++i)
	      add_num_to_form(act->nextaction.args[i]);
    	    if (act->nextaction.actee != 0) {
    	    	space_form();
		add_num_to_form(act->nextaction.actee);
    	    }
    	    end_form();
	}
	end_form();
    }
}

/* Write out the unit's current plan. */

static void
write_unit_plan(unit)
Unit *unit;
{
    Task *task;
    Plan *plan = unit->plan;

    /* The plan is kind of meaningless for dead units. */
    if (!alive(unit))
      return;
    if (plan) {
	if (1) {
	    newline_form();
	    space_form();
	}
    	space_form();
    	start_form(key(K_PLAN));
    	add_to_form(plantypenames[plan->type]);
	add_num_to_form(plan->creation_turn);
	write_num_prop(key(K_INITIAL_TURN), plan->initial_turn, 0, FALSE, FALSE);
	write_num_prop(key(K_FINAL_TURN), plan->final_turn, 0, FALSE, FALSE);
	write_bool_prop(key(K_ASLEEP), plan->asleep, FALSE, FALSE, FALSE);
	write_bool_prop(key(K_RESERVE), plan->reserve, FALSE, FALSE, FALSE);
	write_bool_prop(key(K_DELAYED), plan->delayed, FALSE, FALSE, FALSE);
	write_bool_prop(key(K_WAIT), plan->waitingfortasks, FALSE, FALSE, FALSE);
	write_bool_prop(key(K_AI_CONTROL), plan->aicontrol, TRUE, FALSE, FALSE);
	write_bool_prop(key(K_SUPPLY_ALARM), plan->supply_alarm, TRUE, FALSE, FALSE);
	write_bool_prop(key(K_SUPPLY_IS_LOW), plan->supply_is_low, FALSE, FALSE, FALSE);
	write_bool_prop(key(K_WAIT_TRANSPORT), plan->waitingfortransport, FALSE, FALSE, FALSE);
	if (plan->maingoal)
	  write_goal(plan->maingoal, K_GOAL);
	if (plan->formation)
	  write_goal(plan->formation, K_FORMATION);
	if (plan->tasks) {
    	    space_form();
    	    start_form(key(K_TASKS));
    	    for_all_tasks(plan, task) {
	    	space_form();
	    	write_task(task);
	    }
	    end_form();
	}
	end_form();
    }
}

static void
write_task(task)
Task *task;
{
    int i, numargs;
    char *argtypes;

    /* (should make this into an "is_task_type" test) */
    if (!between(TASK_NONE, task->type, /*TASK_LAST*/ TASK_SENTRY)) {
	run_warning("Bad task type %d while writing, skipping it", task->type);
	return;
    }
    start_form(taskdefns[task->type].name);
    add_num_to_form(task->execnum);
    add_num_to_form(task->retrynum);
    argtypes = taskdefns[task->type].argtypes;
    numargs = strlen(argtypes);
    for (i = 0; i < numargs; ++i)
      add_num_to_form(task->args[i]);
    end_form();
}

static void
write_goal(goal, keyword)
Goal *goal;
int keyword;
{
    int i, numargs;
    char *argtypes;

    space_form();
    start_form(key(keyword));
    add_num_to_form(side_number(goal->side));
    add_num_to_form(goal->tf);
    add_to_form(goaldefns[goal->type].name);
    argtypes = goaldefns[goal->type].argtypes;
    numargs = strlen(argtypes);
    for (i = 0; i < numargs; ++i)
      add_num_to_form(goal->args[i]);
    end_form();
}

/* Write all the historical events recorded so far. */

static void
write_history()
{
    PastUnit *pastunit;
    HistEvent *hevt;

    /* Write all the past units that might be mentioned in events.  These
       should already be sorted by id. */
    for (pastunit = past_unit_list; pastunit != NULL; pastunit = pastunit->next)
      write_past_unit(pastunit);
    newline_form();
    /* Now write all the events, doing the first one separately so as to
       simplify testing for the end of the history list (which is circular). */
    write_historical_event(history);
    for (hevt = history->next; hevt != history; hevt = hevt->next)
      write_historical_event(hevt);
    newline_form();
}

static void
write_past_unit(pastunit)
PastUnit *pastunit;
{
    start_form(key(K_EXU));
    add_num_to_form(pastunit->id);
    add_to_form(shortest_escaped_name(pastunit->type));
    add_num_to_form(pastunit->x);
    add_num_to_form(pastunit->y);
    add_num_to_form(side_number(pastunit->side));
    write_num_prop(key(K_Z), pastunit->z, 0, FALSE, FALSE);
    write_str_prop(key(K_N), pastunit->name, NULL, FALSE, FALSE);
    write_num_prop(key(K_NB), pastunit->number, 0, FALSE, FALSE);
    end_form();
    newline_form();
}

static void
write_historical_event(hevt)
HistEvent *hevt;
{
    int i;
    char *descs;

    /* Might be reasons not to write this event. */
    if (hevt->startdate < 0)
      return;
    start_form(key(K_EVT));
    add_num_to_form(hevt->startdate);
    add_to_form(hevtdefns[hevt->type].name);
    if (hevt->observers == ALLSIDES)
      add_to_form(key(K_ALL));
    else
      add_num_to_form(hevt->observers);
    descs = hevtdefns[hevt->type].datadescs;
    for (i = 0; descs[i] != '\0'; ++i) {
	switch (descs[i]) {
	  case 'm':
	  case 'n':
	  case 's':
	  case 'S':
	  case 'u':
	  case 'U':
	  case 'x':
	  case 'y':
	    add_num_to_form(hevt->data[i]);
	    break;
	  default:
	    run_warning("'%c' is not a recognized history data desc char", descs[i]);
	    break;
	}
    }
    end_form();
    newline_form();
}

/* The comparison function for the image list just does "strcmp" order
   and *requires* that all image families be named and named uniquely. */

static void sort_all_recorded_images PARAMS ((void));

static int
image_name_compare(imf1, imf2)
#ifdef THINK_C
const
#endif
void *imf1, *imf2;
{
    return strcmp((*((ImageFamily **) imf1))->name,
		  (*((ImageFamily **) imf2))->name);
}

static void
sort_all_recorded_images()
{
    qsort(&(recorded_imfs[0]), num_recorded_imfs, sizeof(ImageFamily *), image_name_compare);
}

static void
write_images()
{
    int i;

    if (recorded_imfs == NULL || num_recorded_imfs == 0)
      return;
    sort_all_recorded_images();
    for (i = 0; i < num_recorded_imfs; ++i) {
	write_imf(fp, recorded_imfs[i]);
    }
}

/* This is a generalized routine to do run-length-encoding of area layers.
   It uses hook fns to acquire data at a point and an optional translator to
   do any last-minute fixing.  It can use either a char or numeric encoding,
   depending on the expected range of values. */

static void
write_rle(datafn, lo, hi, translator, compress)
int (*datafn) PARAMS ((int, int)), lo, hi, (*translator) PARAMS ((int)), compress;
{
    int width, height, x, y, x0, y0, run, runval, val, trval;
    int numbad = 0;

    width = area.width;  height = area.height;
    if (doreshape) {
	width = reshaper->final_width;  height = reshaper->final_height;
    }
    for (y = height-1; y >= 0; --y) {
	space_form();
	space_form();
	fprintf(fp, "\"");
	run = 0;
	x0 = 0;  y0 = y;
	if (doreshape)
	  original_point(0, y, &x0, &y0);
	val = (*datafn)(x0, y0);
	/* Zero out anything not in the world, unless reshaping. */
	if (!doreshape && !in_area(x0, y0))
	  val = 0;
	/* Check that the data falls within bounds, clip if not. */
	if (lo <= hi && !between(lo, val, hi) && in_area(x0, y0)) {
	    ++numbad;
	    if (val < lo)
	      val = lo;
	    if (val > hi)
	      val = hi;
	}
	runval = val;
	for (x = 0; x < width; ++x) {
	    x0 = x;  y0 = y;
	    if (doreshape)
	      original_point(x, y, &x0, &y0);
	    val = (*datafn)(x0, y0);
	    /* Zero out anything not in the world, unless reshaping. */
	    if (!doreshape && !in_area(x0, y0))
	      val = 0;
	    /* Check that the data falls within bounds, clip if not. */
	    if (lo <= hi && !between(lo, val, hi) && in_area(x0, y0)) {
		++numbad;
		if (val < lo)
		  val = lo;
		if (val > hi)
		  val = hi;
	    }
	    if (val == runval && compress) {
		run++;
	    } else {
		trval = (translator != NULL ? (*translator)(runval) : runval);
		write_run(run, trval);
		/* Start a new run. */
		runval = val;
		run = 1;
	    }
	}
	/* Finish off the row. */
	trval = (translator != NULL ? (*translator)(val) : val);
	write_run(run, trval);
	fprintf(fp, "\"");
	newline_form();
    }
    if (numbad > 0) {
	run_warning("%d values not between %d and %d", numbad, lo, hi);
    }
}

/* Write a single run, using the most compact encoding possible.
   0 - 29 is 'a' - '~', 30 - 63 is ':' - '[' */ 

static void
write_run(run, val)
int run, val;
{
    if (run > 1) {
	fprintf(fp, "%d", run);
	if (!between(0, val, 63))
	  fprintf(fp, "*");
    }
    if (between(0, val, 29)) {
	fprintf(fp, "%c", val + 'a');
    } else if (between(30, val, 63)) {
	fprintf(fp, "%c", val - 30 + ':');
    } else {
	fprintf(fp, "%d,", val);
    }
}

/* Compute and return the corresponding point in an area being reshaped. */

static int
reshaped_point(x1, y1, x2p, y2p)
int x1, y1, *x2p, *y2p;
{
    *x2p = (((x1 - reshaper->subarea_x) * reshaper->final_subarea_width )
	    / reshaper->subarea_width ) + reshaper->final_subarea_x;
    *y2p = (((y1 - reshaper->subarea_y) * reshaper->final_subarea_height)
	    / reshaper->subarea_height) + reshaper->final_subarea_y;
    return TRUE;
}

static int
original_point(x1, y1, x2p, y2p)
int x1, y1, *x2p, *y2p;
{
    *x2p = (((x1 - reshaper->final_subarea_x) * reshaper->subarea_width )
	    / reshaper->final_subarea_width ) + reshaper->subarea_x;
    *y2p = (((y1 - reshaper->final_subarea_y) * reshaper->subarea_height)
	    / reshaper->final_subarea_height) + reshaper->subarea_y;
    return inside_area(*x2p, *y2p);
}

