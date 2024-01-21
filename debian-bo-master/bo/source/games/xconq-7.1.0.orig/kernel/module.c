/* Game modules for Xconq.
   Copyright (C) 1991, 1992, 1993, 1994 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"

extern char *readerrbuf;

static void do_one_variant PARAMS ((Module *module, struct a_variant *var, Obj *varsetdata));

/* List of all known modules. Their descriptions can co-exist in memory,
   even if their contents cannot. */

Module *modulelist;

/* The main module defining the game in effect. */

Module *mainmodule;

char *moduledesigbuf = NULL;

/* Empty out the list of modules. */

void
clear_game_modules()
{
    modulelist = mainmodule = NULL;
}

/* Create a brand-new game module. */

Module *
create_game_module(name)
char *name;
{
    Module *module = (Module *) xmalloc(sizeof(Module));

    /* Fill in nonzero slots. */
    /* The module's name must never be NULL. */
    if (name == NULL)
      name = "";
    module->name = name;
    module->instructions = lispnil;
    module->notes = lispnil;
    module->designnotes = lispnil;
    module->startlineno = 1;
    module->endlineno = 1;
    /* Add to front of module list. */
    module->next = modulelist;
    modulelist = module;
    return module;
}

Module *
find_game_module(name)
char *name;
{
    Module *module;

    if (name != NULL) {
	for_all_modules(module) {
	    if (module->name && strcmp(name, module->name) == 0)
	      return module;
	}
    }
    return NULL;
}


/* Produce a module of the given name, either by finding it or creating it. */

Module *
get_game_module(name)
char *name;
{
    Module *module = find_game_module(name);

    if (module != NULL)
      return module;
    return create_game_module(name);
}

/* Make a game module for the given name and maybe bolt it into the include
   list of another module. */

Module *
add_game_module(name, includer)
char *name;
Module *includer;
{
    Module *module = get_game_module(name), *other;

    if (includer) {
	/* Add to the end of the list of include files. */
	if (includer->include == NULL) {
	    includer->include = includer->lastinclude = module;
	} else {
	    for (other = includer->include; other != NULL; other = other->nextinclude) {
		/* already here, just return it. */
		if (module == other)
		  return module;
	    }
	    includer->lastinclude->nextinclude = module;
	    includer->lastinclude = module;
	}
    } else {
	/* an error? */
    }
    return module;
}

/* Display game module info to a side. */

void
describe_game_modules(arg, key, buf)
int arg;
char *key, *buf;
{
    if (mainmodule != NULL) {
	/* First put out basic module info. */
	describe_game_module_aux(buf, mainmodule, 0);
	/* Now do the lengthy module notes (with no indentation). */
	describe_module_notes(buf, mainmodule);
    } else {
	sprintf(buf, "(No game module information is available.)");
    }
}

/* Recurse down through included modules to display docs on each.
   Indents each file by inclusion level.  Note that modules cannot
   be loaded more than once, so each will be described only once here. */

void   
describe_game_module_aux(buf, module, level)
char *buf;
Module *module;
int level;
{
    int i;
    char indentbuf[100];
    Module *submodule;

    indentbuf[0] = '\0';
    for (i = 0; i < level; ++i) {
	strcat(indentbuf, "  ");
    }
    tprintf(buf, "%s\"%s\"", indentbuf,
	    (module->title ? module->title : module->name));
    if (module->title == NULL) {
	tprintf(buf, " (\"%s\")", module->name);
    }
    if (module->version != NULL) {
	tprintf(buf, " (version \"%s\")", module->version);
    }
    tprintf(buf, "\n");
    tprintf(buf, "%s          %s\n",
	    indentbuf,
	    (module->blurb ? module->blurb : "(no description)"));
    if (module->notes != lispnil) {
	tprintf(buf, "%s          (See notes below)\n", indentbuf);
    }
    /* Now describe any included modules. */
    for_all_includes(module, submodule) {
	describe_game_module_aux(buf, submodule, level + 1);
    }
}

/* Dump the module player's and designer's notes into the given buffer. */

void
describe_module_notes(buf, module)
char *buf;
Module *module;
{
    Module *submodule;

    if (module->notes != lispnil) {
	tprintf(buf, "\nNotes to \"%s\":\n", module->name);
	append_notes(buf, module->notes);
    }
#ifdef DESIGNERS
    /* Only show design notes if any designers around. */
    if (numdesigners > 0 && module->designnotes != lispnil) {
	tprintf(buf, "\nDesign Notes to \"%s\":\n", module->name);
	append_notes(buf, module->designnotes);
    }
#endif /* DESIGNERS */
    for_all_includes(module, submodule) {
	describe_module_notes(buf, submodule);
    }
}

/* Sometimes we find ourselves lacking a game to provide meaning and
   context for interpretation; this routine loads the standard game
   (or a specified alternative default) immediately, but only makes
   it the main module if none defined. */

void
load_default_game()
{
    extern char *standard_game_name;
    char *defaultname = standard_game_name;
    Module *module, *module2;

    /* If we have a different default module, use it instead. */
    if (mainmodule != NULL
	&& !empty_string(mainmodule->defaultbasemodulename)) {
    	defaultname = mainmodule->defaultbasemodulename;
    }
    module = get_game_module(defaultname);
    if (mainmodule == NULL)
      mainmodule = module;
    load_game_description(module);
    /* Recurse one level of default base module. */
    /* (should recurse indefinitely, or not?) */
    if (!empty_string(module->defaultbasemodulename)) {
	module2 = get_game_module(module->defaultbasemodulename);
	load_game_module(module2, TRUE);
    }
    load_game_module(module, TRUE);
}

/* Attempt to read just the first form in a module and use it as a
   description of the module.  Return true if this worked, false
   otherwise. */

int
load_game_description(module)
Module *module;
{
    Obj *form, *thecar;
    char *name;

    if (open_module(module, FALSE)) {
	if ((form = read_form(module->fp,
			      &(module->startlineno),
			      &(module->endlineno)))
	    != lispeof) {
	    if (consp(form) && symbolp(thecar = car(form))) {
		name = c_string(thecar);
		if (keyword_code(name) == K_GAME_MODULE) {
		    interp_game_module(form, module);
		    close_module(module);
		    /* Note that module is still not considered "loaded". */
		    return TRUE;
		}
	    }
	}
    }
    return FALSE;
}

/* Game files can live in library directories or somewhere else.  This
   function tries to find a file, open it, and load the contents. */

void
load_game_module(module, dowarn)
Module *module;
int dowarn;
{
    char ch;

    if (open_module(module, dowarn)) {
	if (module->fp) {
	    /* Peek at the first character - was 'X' in old format files. */
	    ch = getc(module->fp);
	    ungetc(ch, module->fp);
	    if (ch == 'X') {
		init_error("\"%s\" is probably an obsolete Xconq file; in any case, it cannot be used.",
			   module->filename);
	    } else {
		/* New format, read it all. */
		read_forms(module);
	    }
	} else {
	    /* (should be able to read from contents string) */
	}
	/* We're done, can close. */
	close_module(module);
	/* Mark the module as having been loaded - note that this will happen
	   even if there were horrible errors. */
	module->loaded = TRUE;
	/* If the turn number has been set explicitly to a positive value,
	   assume that a saved game is being restored into the middle of the turn. */
	if (g_turn() > 0)
	  midturnrestore = TRUE;
	/* If the random state has been set explicitly to a nonnegative value,
	   use it to reseed the generator. */
	if (g_random_state() >= 0)
	  init_xrandom(g_random_state());
	/* Make all the cross-references right. */
	patch_object_references();
	/* Are all the types staked down now? */
	if (!canaddutype && !canaddmtype && !canaddttype) {
	    typesdefined = TRUE;
	    /* (should also record this as a sort of base module?) */
	}
    }
}

void
load_base_module(module)
Module *module;
{
    char *basename = module->basemodulename;
    Module *basemodule;

    if (!empty_string(basename)) {
	basemodule = find_game_module(basename);
	if (basemodule == NULL)
	  basemodule = add_game_module(basename, module);
	if (basemodule->loaded) {    
	    Dprintf("Base module `%s' already loaded.\n", basename);
	} else {
	    Dprintf("Loading base module `%s' ...\n", basename);
	    load_game_module(basemodule, FALSE);
	    Dprintf("... Done loading `%s'.\n", basename);
	}
    }
}

/* Given a module, attempt to open it. */

int
open_module(module, dowarn)
Module *module;
int dowarn;
{
    FILE *fp = NULL;

    /* Don't open more than once. */
    if (module->open && dowarn) {
	init_warning("Module \"%s\" is already open, ignoring attempt to reopen",
		     module->name);
	return FALSE;
    }
    /* Don't open if already loaded. */
    if (module->loaded && dowarn) {
	init_warning("Module \"%s\" is already loaded, ignoring attempt to reload",
		     module->name);
	return FALSE;
    }
    if (module->contents) {
	/* Uninterpreted contents already available, init the ptr. */
	module->sp = module->contents;
	Dprintf("Reading module \"%s\" from string ...\n", module->name);
    } else if ((fp = open_module_library_file(module)) != NULL) {
	/* Found the module in a library directory. */
	Dprintf("Reading module \"%s\" from library file \"%s\" ...\n",
		module->name, module->filename);
	module->fp = fp;
    } else if ((fp = open_module_explicit_file(module)) != NULL) {
	Dprintf("Reading module \"%s\" from file \"%s\" ...\n",
		module->name, module->filename);
	module->fp = fp;
    } else {
	if (dowarn) {
	    if (module->name) {
	    	init_warning("Can't find module \"%s\" anywhere",
			     module->name);
	    } else {
	    	init_warning("Can't find unnamed module anywhere");
	    }
	}
	return FALSE;
    }
    /* It worked, mark this module as open. */
    module->open = TRUE;
    return TRUE;
}

/* Read info about a side's preferences and setup. */

/* This assumes one form only, probably too restrictive. */
/* should read all the forms, use the relevant ones. */
/* (how does this interact with other defaults?) */
/* (should be delayed until player can confirm it...) */

/* (update to work like other module stuff? then can use resources etc) */
/* (fix so that correct name can be found reliably) */

int
load_side_config(side)
Side *side;
{
#if 0
    FILE *fp;
    Obj *config;
    Module *module;

    /* (should incorp config name somehow, also be sys-dependent) */
    module = create_game_module(side->player->name);

    if ((module->fp = fopen(module->filename, "r")) != NULL) {
	if ((config = read_form(module->fp,
				&(module->startlineno),
				&(module->endlineno)))
	    != lispeof) {
	    /* interpret the config */
	    Dprintf("Interpreting %s config form", side_desig(side));
	    Dprintlisp(config);
	    Dprintf("\n");
	    fill_in_side(side, config, TRUE);
	} else {
	    /* no config form in the file */
	}
    } else {
	init_warning("Module \"%s\" could not be opened", module->name);
	/* Not a disaster, keep going */
    }
#endif
    return FALSE;
}

/* Read an entire file, attempting to pick up objects in it. */

/* (does this interp game-module form twice if description previously
   loaded?) */

void
read_forms(module)
Module *module;
{
    Obj *form;

    Dprintf("Trying to read a new format file \"%s\"...\n", module->name);
    while ((form = read_form(module->fp,
			     &(module->startlineno),
			     &(module->endlineno)))
	   != lispeof) {
	interp_form(module, form);
    }
    /* Clean up after any print forms that might have been done. */
    end_printing_forms();
    Dprintf("... Done reading \"%s\".\n", module->name);
}

/* Interpret the given list of variants. */

void
do_module_variants(module, lis)
Module *module;
Obj *lis;
{
    int i, found;
    Obj *restset, *varset;
    Variant *var;

    if (module->variants == NULL)
      return; /* error? */
    for_all_list(lis, restset) {
	varset = car(restset);
	found = FALSE;
	for (i = 0; module->variants[i].id != lispnil; ++i) {
	    var = &(module->variants[i]);
	    if (equal(car(varset), var->id)) {
		do_one_variant(module, var, cdr(varset));
		found = TRUE;
	    }
	}
	if (!found) {
	    read_warning("Mystifying variant");
	}
    }
    /* Now implement all the defaults. */
    for (i = 0; module->variants[i].id != lispnil; ++i) {
	var = &(module->variants[i]);
	if (!var->used)
	  do_one_variant(module, var, lispnil);
    }
}

static void
do_one_variant(module, var, varsetdata)
Module *module;
Variant *var;
Obj *varsetdata;
{
    int val, caseval;
    int width = 0, height = 0, circumference, latitude, longitude;
    int rtime, rtimeperturn, rtimeperside;
    char *vartypename = c_string(var->id);
    Obj *restcases, *headcase, *rest, *filler, *rawval, *rawcaseval;

    if (Debug) {
	if (readerrbuf == NULL)
	  readerrbuf = (char *) xmalloc(BUFSIZE);
    	sprintlisp(readerrbuf, varsetdata);
    	Dprintf("Module %s variant %s being set to `%s'\n",
	    	module_desig(module), vartypename, readerrbuf);
    }
    switch (keyword_code(vartypename)) {
      case K_WORLD_SEEN:
	val = (varsetdata == lispnil ?
	       (var->dflt == lispnil ? FALSE : c_number(eval(var->dflt))) :
	       c_number(eval(car(varsetdata))));
	var->hasintvalue = TRUE;
	var->intvalue = val;
	set_g_terrain_seen(val);
	break;
      case K_SEE_ALL:
	val = (varsetdata == lispnil ?
	       (var->dflt == lispnil ? FALSE : c_number(eval(var->dflt))) :
	       c_number(eval(car(varsetdata))));
	var->hasintvalue = TRUE;
	var->intvalue = val;
	set_g_see_all(val);
	break;
      case K_SEQUENTIAL:
	val = (varsetdata == lispnil ?
	       (var->dflt == lispnil ? FALSE : c_number(eval(var->dflt))) :
	       c_number(eval(car(varsetdata))));
	var->hasintvalue = TRUE;
	var->intvalue = val;
	set_g_use_side_priority(val);
	break;
      case K_WORLD_SIZE:
      	filler = lispnil;
	if (varsetdata != lispnil) {
	    filler = varsetdata;
	} else if (var->dflt != lispnil) {
	    filler = var->dflt;
	}
	/* Pick the width and height out of the list. */
	if (filler != lispnil) {
	    width = c_number(eval(car(filler)));
	    filler = cdr(filler);
	}
	if (filler != lispnil) {
	    height = c_number(eval(car(filler)));
	    filler = cdr(filler);
	} else {
	    height = width;
	}
	/* Pick up a circumference if given. */
	if (filler != lispnil) {
	    circumference = c_number(eval(car(filler)));
	    set_world_circumference(circumference, TRUE);
	    filler = cdr(filler);
	}
	/* This is more useful after the circumference has been set. */
	if (width > 0 && height > 0)
	  set_area_shape(width, height, TRUE);
	/* Pick up latitude and longitude if given. */
	if (filler != lispnil) {
	    latitude = c_number(eval(car(filler)));
	    /* (should use a setter routine?) */
	    area.latitude = latitude;
	    filler = cdr(filler);
	}
	if (filler != lispnil) {
	    longitude = c_number(eval(car(filler)));
	    /* (should use a setter routine?) */
	    area.longitude = longitude;
	    filler = cdr(filler);
	}
	/* (should record settings somehow) */
	break;
      case K_REAL_TIME:
      	filler = lispnil;
	if (varsetdata != lispnil) {
	    filler = varsetdata;
	} else if (var->dflt != lispnil) {
	    filler = var->dflt;
	}
	if (filler != lispnil) {
	    rtime = c_number(eval(car(filler)));
	    filler = cdr(filler);
	} else {
	    rtime = 0;
	}
	if (filler != lispnil) {
	    rtimeperside = c_number(eval(car(filler)));
	    filler = cdr(filler);
	} else {
	    rtimeperside = 0;
	}
	if (filler != lispnil) {
	    rtimeperturn = c_number(eval(car(filler)));
	    filler = cdr(filler);
	} else {
	    rtimeperturn = 0;
	}
	/* If the values were specified, tweak the official
	   realtime globals. */
	if (rtime > 0)
	  set_g_rt_for_game(rtime);
	if (rtimeperside > 0)
	  set_g_rt_per_side(rtimeperside);
	if (rtimeperturn > 0)
	  set_g_rt_per_turn(rtimeperturn);
	/* (should record settings somehow) */
	break;
      default:
	/* This is the general case. */
	if (varsetdata != lispnil) {
	    rawval = car(varsetdata);
	} else if (var->dflt != lispnil) {
	    rawval = var->dflt;
	} else {
	    rawval = lispnil;
	}
	/* Always evaluate the raw value of the variant. */
	rawval = eval(rawval);
	if (numberp(rawval)) {
	    val = c_number(rawval);
	    var->hasintvalue = TRUE;
	    var->intvalue = val;
	} else {
	    val = FALSE;
	}
	for_all_list(var->cases, restcases) {
	    headcase = car(restcases);
	    rawcaseval = eval(car(headcase));
	    if (numberp(rawcaseval))
	      caseval = c_number(rawcaseval);
	    if (numberp(rawcaseval) && caseval == val) {
		for_all_list(cdr(headcase), rest) {
		    interp_form(module, car(rest));
	    	}
	    }
	}
	/* Clean up after printing, might have been print forms in variant. */
	end_printing_forms();
	break;
    }
    /* Flag the variant as having been specified. */
    var->used = TRUE;
}

void
init_module_reshape(module)
Module *module;
{
    /* Seed all the reshaping parameters with reasonable values. */
    module->maybe_reshape = TRUE;
    module->subarea_width = area.width;
    module->subarea_height = area.height;
    module->subarea_x = module->subarea_y = 0;
    module->final_subarea_width = area.width;
    module->final_subarea_height = area.height;
    module->final_subarea_x = module->final_subarea_y = 0;
    module->final_width = area.width;  module->final_height = area.height;
    module->final_circumference = world.circumference;
    module->fill_type = 0;
}

/* This is true if any actual reshaping is required. */

int
reshape_the_output(module)
Module *module;
{
    return (module->maybe_reshape
	    && (module->subarea_width != area.width
		|| module->subarea_height != area.height
		|| module->subarea_x != 0
		|| module->subarea_y != 0
		|| module->final_subarea_width != area.width
		|| module->final_subarea_height != area.height
		|| module->final_subarea_x != 0
		|| module->final_subarea_y != 0
		|| module->final_width != area.width
		|| module->final_height != area.height
		|| module->final_circumference != world.circumference));
}

/* Check if the proposed reshape will actually work. */

int
valid_reshape(module)
Module *module;
{
    /* (should check hexagon shaping) */
    if (module->subarea_width > area.width
	|| module->subarea_height > area.height)
      return FALSE;
    /* (should check other offsets) */
    if (module->final_width < 3 || module->final_height < 3)
      return FALSE;
    return TRUE;
}

/* Close the module. */

void
close_module(module)
Module *module;
{
    if (module->sp) {
	module->sp = NULL;
    }
    if (module->fp) {
	fclose(module->fp);
	module->fp = NULL;
    }
    module->open = FALSE;
}

/* Return a description of the module. */

char *
module_desig(module)
Module *module;
{
    if (moduledesigbuf == NULL)
      moduledesigbuf = xmalloc(BUFSIZE);
    sprintf(moduledesigbuf, "module %s (%s)",
	    module->name, (module->title ? module->title : "no title"));
    return moduledesigbuf;
}

/* (random code below, should be sent to better places) */

#ifdef DEBUGGING
/* Use this to ensure that everything is cool. */

void
doublecheck_state(side)
Side *side;
{
    Unit *unit;

    for_all_units(unit) {
	if (unit->x < 0 || unit->x >= area.width ||
	    unit->y <= 0 || unit->y >= (area.height - 1) ||
	    unit->hp <= 0) {
	    Dprintf("%s off map hp %d", unit_desig(unit), unit->hp);
	}
    }
}
#endif /* DEBUGGING */

void set_u_internal_name(u, s) int u; char *s; { utypes[u].iname = s; }
void set_u_type_name(u, s) int u; char *s; { utypes[u].name = s; }
void set_m_type_name(m, s) int m; char *s; { mtypes[m].name = s; }
void set_t_type_name(t, s) int t; char *s; { ttypes[t].name = s; }

/* If a special symbol, we might not have to fail. */

int
lazy_bind(sym)
Obj *sym;
{
    int u, m, t;
    Obj *value;

    switch (keyword_code(c_string(sym))) {
      case K_USTAR:
	value = lispnil;
	/* Since consing glues onto the front, iterate backwards
	   through the types. */
	for (u = numutypes - 1; u >= 0; --u) {
	    value = cons(new_utype(u), value);
	}
	break;
      case K_MSTAR:
	value = lispnil;
	for (m = nummtypes - 1; m >= 0; --m) {
	    value = cons(new_mtype(m), value);
	}
	break;
      case K_TSTAR:
	value = lispnil;
	for (t = numttypes - 1; t >= 0; --t) {
	    value = cons(new_ttype(t), value);
	}
	break;
      default:
	return FALSE;
    }
    setq(sym, value);
    return TRUE;
}

int
coerce_to_side_id(x)
Obj *x;
{
    if (numberp(x)) {
	return c_number(x);
    }
    return 0;
}

Side *
coerce_to_side(x)
Obj *x;
{
    return side_n(coerce_to_side_id(x));
}

int
coerce_to_unit_id(x)
Obj *x;
{
    if (numberp(x)) {
	return c_number(x);
    }
    return 0;
}

Unit *
coerce_to_unit(x)
Obj *x;
{
    return find_unit(coerce_to_unit_id(x));
}
