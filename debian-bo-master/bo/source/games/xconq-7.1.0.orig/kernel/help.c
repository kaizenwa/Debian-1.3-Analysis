/* Help support for Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

/* This is basically support code for interfaces, which handle the
   actual help interaction themselves. */

/* This file must also be translated (mostly) for non-English Xconq. */

#include "conq.h"
extern void notify_instructions PARAMS ((void));

static void describe_help_system PARAMS ((int arg, char *key, char *buf));
static void describe_synth_run PARAMS ((char *buf, int methkey));
static int histogram_compare PARAMS ((const void *h1, const void *h2));

/* The first help node in the chain. */

HelpNode *first_help_node;

/* The last help node. */

HelpNode *last_help_node;

/* The help node with copying and copyright info. */

HelpNode *copying_help_node;

/* The help node with (non-)warranty info. */

HelpNode *warranty_help_node;

HelpNode *default_prev_help_node;

/* Create the initial help node and link it to itself.  Subsequent nodes will be
   inserted later, after a game has been loaded. */

void
init_help()
{
    /* Note that we can't use add_help_node to set up the first help node. */
    first_help_node = create_help_node();
    first_help_node->key = "help system";
    first_help_node->fn = describe_help_system;
    first_help_node->prev = first_help_node->next = first_help_node;
    last_help_node = first_help_node;
    copying_help_node = add_help_node("copyright", describe_copyright, 0, first_help_node);
    warranty_help_node = add_help_node("warranty", describe_warranty, 0, copying_help_node);
    /* Set the place for new nodes to appear normally. */
    default_prev_help_node = copying_help_node;
    add_help_node("news", describe_news, 0, NULL);
}

/* This function creates the actual set of help nodes for the kernel. */

void
create_game_help_nodes()
{
    int u, m, t;
    char *name, *longname;
    HelpNode *node;

    add_help_node("game design", describe_game_design, 0, NULL);
    add_help_node("modules", describe_game_modules, 0, NULL);
    add_help_node("scoring", describe_scorekeepers, 0, NULL);
    add_help_node("game setup", describe_setup, 0, NULL);
    for_all_unit_types(u) {
	longname = u_long_name(u);
	if (!empty_string(longname)) {
	    sprintf(spbuf, "%s (%s)", longname, u_type_name(u));
	    name = copy_string(spbuf);
	} else {
	    name = u_type_name(u);
	}
	node = add_help_node(name, describe_utype, u, NULL);
	node->nclass = utypenode;
    }
    for_all_material_types(m) {
	node = add_help_node(m_type_name(m), describe_mtype, m, NULL);
	node->nclass = mtypenode;
    }
    for_all_terrain_types(t) {
	node = add_help_node(t_type_name(t), describe_ttype, t, NULL);
	node->nclass = ttypenode;
    }
    add_help_node("general concepts", describe_concepts, 0, NULL);
    /* Invalidate any existing topics node. */
    first_help_node->text = NULL;
}

/* Create an empty help node. */

HelpNode *
create_help_node()
{
    HelpNode *node = (HelpNode *) xmalloc(sizeof(HelpNode));

    node->key = NULL;
    node->fn = NULL;
    node->nclass = miscnode;
    node->arg = 0;
    node->text = NULL;
    node->prev = node->next = NULL;
    return node;
}

/* Add a help node after the given node. */

HelpNode *
add_help_node(key, fn, arg, prevnode)
char *key;
void (*fn)();
int arg;
HelpNode *prevnode;
{
    HelpNode *node, *nextnode;

    if (empty_string(key))
      run_error("empty help key");
    node = create_help_node();
    node->key = key;
    node->fn = fn;
    node->arg = arg;
    if (prevnode == NULL)
      prevnode = default_prev_help_node->prev;
    nextnode = prevnode->next;
    node->prev = prevnode;
    node->next = nextnode;
    prevnode->next = node;
    nextnode->prev = node;
    /* Might need to fix last help node. */
    last_help_node = first_help_node->prev;
    return node;
}

/* Given a string and node, find the next node whose key matches. */

HelpNode *
find_help_node(node, str)
HelpNode *node;
char *str;
{
    HelpNode *tmp;

    /* Note that the search wraps around. */
    for (tmp = node->next; tmp != node; tmp = tmp->next) {
    	if (strcmp(tmp->key, str) == 0)
    	  return tmp;
    	if (strstr(tmp->key, str) != NULL)
    	  return tmp;
    }
    return NULL;
}

/* Return the string containing the text of the help node, possibly
   computing it first. */

char *
get_help_text(node)
HelpNode *node;
{
    int allocsize;

    if (node != NULL) {
	/* Maybe calculate the text to display. */
	if (node->text == NULL) {
	    if (node->fn != NULL) {
		/* (should allow for variable-size allocation) */
		allocsize = 5000;
		if (node->fn == describe_copyright)
		  allocsize = 30000;
	    	node->text = xmalloc(allocsize);
		if (node->text != NULL) {
		    /* Make buffer into an empty string, so that the
		       description function can just start appending. */
		    (node->text)[0] = '\0';
		    node->textend = 0;
		    node->textsize = allocsize;
		    (*(node->fn))(node->arg, node->key, node->text);
		    node->textend = strlen(node->text);
		} else {
		    /* Ran out of memory... (would never get here though!) */
		}
	    } else {
		/* Generate a default message if nothing to compute help. */
		sprintf(spbuf, "%s: No info available.", node->key);
		node->text = copy_string(spbuf);
		node->textend = strlen(node->text);
	    }
	}
	return node->text;
    } else {
	return NULL;
    }
}

static void
describe_help_system(arg, key, buf)
int arg;
char *key, *buf;
{
    strcat(buf, "This is the header node of the Xconq help system.\n");
    strcat(buf, "Go forward or backward from here to see the online help.\n");
}

/* Create a raw list of help topics by just iterating through all the nodes,
   except for the topics node itself. */

void
describe_topics(arg, key, buf)
int arg;
char *key, *buf;
{
    HelpNode *topics, *tmp;

    topics = find_help_node(first_help_node, "topics");
    /* Unlikely that we'll call this without the topics node existing
       already, but just in case... */
    if (topics == NULL)
      return;
    for (tmp = topics->next; tmp != topics; tmp = tmp->next) {
	tprintf(buf, "%s", tmp->key);
	tprintf(buf, "\n");
    }
}

/* Get the news file and put it into text buffer. */

void
describe_news(arg, key, buf)
int arg;
char *key, *buf;
{
    FILE *fp;

    fp = fopen(news_filename(), "r");
    if (fp != NULL) {
	tprintf(buf, "XCONQ NEWS\n\n");
	while (fgets(spbuf, BUFSIZE-1, fp) != NULL) {
	    tprintf(buf, "%s", spbuf);
	}
	fclose(fp);
    } else {
	tprintf(buf, "(no news)");
    }
}

void
describe_concepts(arg, key, buf)
int arg;
char *key, *buf;
{
    tprintf(buf, "Hit points (hp) represent the overall condition of ");
    tprintf(buf, "the unit.");
    tprintf(buf, "\n");
    tprintf(buf, "Action points (acp) are what a unit needs to be able ");
    tprintf(buf, "to do anything at all.");
    tprintf(buf, "\n");
}

/* Spit out all the general game_design parameters in a readable fashion. */

void
describe_game_design(arg, key, buf)
int arg;
char *key, *buf;
{
    int u, m, t;
    
    /* Replicate title and blurb? (should put title at head of windows, and pages if printed) */
    tprintf(buf, "This game includes %d unit types and %d terrain types",
	    numutypes, numttypes);
    if (nummtypes > 0) {
	tprintf(buf, ", along with %d material types", nummtypes);
    }
    tprintf(buf, ".\n");
    if (g_sides_min() == g_sides_max()) {
    	tprintf(buf, "Exactly %d sides may play.\n", g_sides_min());
    } else {
    	tprintf(buf, "From %d up to %d sides may play.\n", g_sides_min(), g_sides_max());
    }
    tprintf(buf, "\n");
    tprintf(buf, "Player advantages may range from %d to %d, defaulting to %d.\n",
	    g_advantage_min(), g_advantage_max(), g_advantage_default());
    tprintf(buf, "\n");
    if (g_see_all()) {
	tprintf(buf, "Everything is always seen by all sides.\n");
    } else {
    	if (g_see_terrain_always()) {
	    tprintf(buf, "Terrain view is always accurate once seen.\n");
    	}
    	if (g_see_weather_always()) {
	    tprintf(buf, "Weather view is always accurate once terrain seen.\n");
    	}
    	if (g_terrain_seen()) {
	    tprintf(buf, "World terrain is already seen by all sides.\n");
    	}
    }
    tprintf(buf, "\n");
    if (g_last_turn() < 9999) {
	tprintf(buf, "Game can go for up to %d turns", g_last_turn());
	if (g_extra_turn() > 0) {
	    tprintf(buf, ", with %d%% chance of additional turn thereafter.", g_extra_turn());
	}
	tprintf(buf, ".\n");
    }
    if (g_rt_for_game() > 0) {
	tprintf(buf, "Entire game can last up to %d minutes.\n",
		g_rt_for_game() / 60);
    }
    if (g_rt_per_turn() > 0) {
	tprintf(buf, "Each turn can last up to %d minutes.\n",
		g_rt_per_turn() / 60);
    }
    if (g_rt_per_side() > 0) {
	tprintf(buf, "Each side gets a total %d minutes to act.\n",
		g_rt_per_side() / 60);
    }
    if (g_units_in_game_max() >= 0) {
	tprintf(buf, "Limited to no more than %d units in all.\n", g_units_in_game_max());
    }
    if (g_units_per_side_max() >= 0) {
	tprintf(buf, "Limited to no more than %d units per side.\n", g_units_per_side_max());
    }
    /* (should list out random event types?) */
    tprintf(buf, "\n");
    tprintf(buf, "Lowest possible temperature is %d, at an elevation of %d.\n",
	    g_temp_floor(), g_temp_floor_elev());
    tprintf(buf, "\nUnit Types:\n");
    for_all_unit_types(u) {
	tprintf(buf, "  %s", u_type_name(u));
	if (!empty_string(u_help(u)))
	  tprintf(buf, " (%s)", u_help(u));
	tprintf(buf, "\n");
#ifdef DESIGNERS
	/* Show designers a bit more. */
	if (numdesigners > 0) {
	    tprintf(buf, "    [");
	    if (!empty_string(u_uchar(u)))
	      tprintf(buf, "char '%s'", u_uchar(u));
	    else
	      tprintf(buf, "no char");
	    if (!empty_string(u_image_name(u)))
	      tprintf(buf, ", image \"%s\"", u_image_name(u));
	    if (!empty_string(u_color(u)))
	      tprintf(buf, ", color \"%s\"", u_color(u));
	    if (!empty_string(u_generic_name(u)))
	      tprintf(buf, ", generic name \"%s\"", u_generic_name(u));
	    if (u_desc_format(u) != lispnil) {
	        tprintf(buf, ", special format");
	    }
	    tprintf(buf, "]\n");
	}
#endif /* DESIGNERS */
    }
    tprintf(buf, "\nTerrain Types:\n");
    for_all_terrain_types(t) {
	tprintf(buf, "  %s", t_type_name(t));
	if (!empty_string(t_help(t)))
	  tprintf(buf, " (%s)", t_help(t));
	tprintf(buf, "\n");
#ifdef DESIGNERS
	/* Show designers a bit more. */
	if (numdesigners > 0) {
	    tprintf(buf, "    [");
	    if (!empty_string(t_char(t)))
	      tprintf(buf, "char '%s'", t_char(t));
	    else
	      tprintf(buf, "no char");
	    if (!empty_string(t_image_name(t)))
	      tprintf(buf, ", image \"%s\"", t_image_name(t));
	    if (!empty_string(t_color(t)))
	      tprintf(buf, ", color \"%s\"", t_color(t));
	    if (t_desc_format(t) != lispnil) {
	        tprintf(buf, ", special format");
	    }
	    tprintf(buf, "]\n");
	}
#endif /* DESIGNERS */
    }
    if (nummtypes > 0) {
	tprintf(buf, "\nMaterial Types:\n");
	for_all_material_types(m) {
	    tprintf(buf, "  %s", m_type_name(m));
	    if (!empty_string(m_help(m)))
	      tprintf(buf, " (%s)", m_help(m));
	    tprintf(buf, "\n");
#ifdef DESIGNERS
	    /* Show designers a bit more. */
	    if (numdesigners > 0) {
		tprintf(buf, "    [");
		if (!empty_string(m_char(m)))
		  tprintf(buf, "char '%s'", m_char(m));
		else
		  tprintf(buf, "no char");
		if (!empty_string(m_image_name(m)))
		  tprintf(buf, ", image \"%s\"", m_image_name(m));
		if (!empty_string(m_color(m)))
		  tprintf(buf, ", color \"%s\"", m_color(m));
		if (m_desc_format(m) != lispnil) {
		    tprintf(buf, ", special format");
		}
		tprintf(buf, "]\n");
	    }
#endif /* DESIGNERS */
	}
    }
}

int any_mp_to_enter_unit PARAMS ((int u));
int any_mp_to_leave_unit PARAMS ((int u));

int
any_mp_to_enter_unit(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_mp_to_enter(u, u2) != 0) return TRUE;
    }
    return FALSE;
}

int
any_mp_to_leave_unit(u)
int u;
{
    int u2;
	
    for_all_unit_types(u2) {
	if (uu_mp_to_leave(u, u2) != 0) return TRUE;
    }
    return FALSE;
}

void fraction_desc PARAMS ((char *buf, int n));

void
fraction_desc(buf, n)
char *buf;
int n;
{
    sprintf(buf, "%d.%d", n / 100, n % 100);
}

/* Full details on the given type of unit. */

/* (The defaults should come from the *.def defaults!!) */

void
describe_utype(u, key, buf)
int u;
char *key, *buf;
{
    char sidetmpbuf[BUFSIZE];
    int m, first;
    Side *side;

    append_help_phrase(buf, u_help(u));
    if (u_point_value(u) > 0) {
	tprintf(buf, "     (point value %d)\n", u_point_value(u));
    }
    if (u_can_be_self(u)) {
    	tprintf(buf, "Can be self-unit");
    	if (u_self_changeable(u))
	  tprintf(buf, "; side may choose another to be self-unit");
    	if (u_self_resurrects(u))
	  tprintf(buf, "; if dies, another becomes self-unit");
    	tprintf(buf, ".\n");
    }
    if (u_possible_sides(u) != lispnil) {
	tprintf(buf, "Possible sides (in this game): ");
	first = TRUE;
	for_all_sides_plus_indep(side) {
	    if (type_allowed_on_side(u, side)) {
		if (first)
		  first = FALSE;
		else
		  tprintf(buf, ", ");
		tprintf(buf, "%s", shortest_side_title(side, sidetmpbuf));
	    }
    	}
    	tprintf(buf, ".\n");
    }
    if (u_type_in_game_max(u) >= 0) {
	tprintf(buf, "At most %d allowed in a game.\n", u_type_in_game_max(u));
    }
    if (u_type_per_side_max(u) >= 0) {
	tprintf(buf, "At most %d allowed on each side in a game.\n", u_type_per_side_max(u));
    }
    if (u_acp(u) > 0) {
	    tprintf(buf, "Gets %d action points each turn", u_acp(u));
	    if (u_acp_min(u) != 0) {
		tprintf(buf, ", can go down to %d acp", u_acp_min(u));
	    }
	    if (u_acp_max(u) != -1) {
		tprintf(buf, ", can go up to %d acp", u_acp_max(u));
	    }
	    if (u_free_acp(u) != 0) {
		tprintf(buf, ", %d free", u_free_acp(u));
	    }
	    tprintf(buf, ".\n");
    } else {
	tprintf(buf, "Does not act.\n");
    }
    if (!u_direct_control(u)) {
	tprintf(buf, "Cannot be controlled directly by side.\n");
    }
    if (u_speed(u) > 0) {
	tprintf(buf, "Speed (mp/acp ratio) is %d.%d cells/acp.\n",
		u_speed(u) / 100, u_speed(u) % 100);
	if (u_speed_min(u) != 0 || u_speed_max(u) != 9999) {
	    tprintf(buf, "Speed variation limited to between %d.%d and %d.%d.\n",
		    u_speed_min(u) / 100, u_speed_min(u) % 100,
		    u_speed_max(u) / 100, u_speed_max(u) % 100);
	}
	if (u_speed_wind_effect(u) != lispnil) {
	    tprintf(buf, "Wind affects speed.\n");
	}
	if (u_speed_damage_effect(u) != lispnil) {
	    tprintf(buf, "Damage affects speed.\n");
	}
	ut_table_row_desc(spbuf, u, ut_mp_to_enter, NULL);
	tprintf(buf, "MP to enter cell: %s.\n", spbuf);
	ut_table_row_desc(spbuf, u, ut_mp_to_leave, NULL);
	tprintf(buf, "MP to leave cell: %s.\n", spbuf);
	if (any_mp_to_enter_unit(u)) {
	    uu_table_row_desc(spbuf, u, uu_mp_to_enter, NULL);
	    tprintf(buf, "MP to enter unit: %s.\n", spbuf);
	}
	if (any_mp_to_leave_unit(u)) {
	    uu_table_row_desc(spbuf, u, uu_mp_to_leave, NULL);
	    tprintf(buf, "MP to leave unit: %s.\n", spbuf);
	}
	if (u_mp_to_leave_world(u) >= 0) {
	    tprintf(buf, "%d MP to leave the world entirely.\n", u_mp_to_leave_world(u));
	}
	if (u_free_mp(u) > 0) {
	    tprintf(buf, "Gets up to %d free MP if needed to move.\n", u_free_mp(u));
	}
	if (u_acp_to_move(u) > 0) {
	    tprintf(buf, "Uses %d ACP to move.\n", u_acp_to_move(u));
	} else {
	    tprintf(buf, "Cannot move by self.\n");
	}
    } else {
	tprintf(buf, "Does not move.\n");
    }
    tprintf(buf, "Hit Points: %d.", u_hp_max(u));
    if (u_parts(u) > 1) {
	tprintf(buf, "  Parts: %d.", u_parts(u));
    }
    if (u_hp_recovery(u) != 0) {
	tprintf(buf, "  Recovers by %d.%d HP each turn.",
		u_hp_recovery(u) / 100, u_hp_recovery(u) % 100);
    }
    tprintf(buf, "\n");
    if (u_capacity(u) != 0) {
	tprintf(buf, "Generic capacity for units is %d.\n",
		u_capacity(u));
    }
    if (u_cxp_max(u) != 0) {
	tprintf(buf, "Combat experience max: %d.\n", u_cxp_max(u));
    }
    if (u_cp(u) != 1) {
	tprintf(buf, "Construction points to complete: %d.\n", u_cp(u));
    }
    if (u_tech_to_see(u) != 0) {
	tprintf(buf, "Tech to see: %d.\n", u_tech_to_see(u));
    }
    if (u_tech_to_own(u) != 0) {
	tprintf(buf, "Tech to own: %d.\n", u_tech_to_own(u));
    }
    if (u_tech_to_use(u) != 0) {
	tprintf(buf, "Tech to use: %d.\n", u_tech_to_use(u));
    }
    if (u_tech_to_build(u) != 0) {
	tprintf(buf, "Tech to build: %d.\n", u_tech_to_build(u));
    }
    if (u_tech_max(u) != 0) {
	tprintf(buf, "Tech max: %d.\n", u_tech_max(u));
    }
    if (u_tech_max(u) != 0 && u_tech_per_turn_max(u) != 9999) {
	tprintf(buf, "Tech increase per turn max: %d.\n", u_tech_per_turn_max(u));
    }
    if (u_tech_from_ownership(u) != 0) {
	tprintf(buf, "Tech guaranteed by ownership: %d.\n", u_tech_from_ownership(u));
    }
    if (u_tech_leakage(u) != 0) {
	tprintf(buf, "Tech leakage: %d.\n", u_tech_leakage(u));
    }
    if (u_acp(u) > 0
        && type_can_research(u) > 0
        ) {
        tprintf(buf, "\nResearch:\n");
        uu_table_row_desc(spbuf, u, uu_acp_to_research, NULL);
	tprintf(buf, "ACP to research: %s.\n", spbuf);
        uu_table_row_desc(spbuf, u, uu_tech_per_research, fraction_desc);
	tprintf(buf, "  Tech gained: %s.\n", spbuf);
    }
    if (u_acp(u) > 0
        && (type_can_create(u) > 0
            || type_can_complete(u) > 0
        )) {
        tprintf(buf, "\nConstruction:\n");
        if (type_can_create(u) > 0) {
	    uu_table_row_desc(spbuf, u, uu_acp_to_create, NULL);
	    tprintf(buf, "ACP to create: %s.\n", spbuf);
 	    uu_table_row_desc(spbuf, u, uu_create_range, NULL);
 	    tprintf(buf, "  Creation distance max: %s.\n", spbuf);
 	    uu_table_row_desc(spbuf, u, uu_creation_cp, NULL);
 	    tprintf(buf, "  Completeness upon creation: %s.\n", spbuf);
	}
        if (type_can_complete(u) > 0) {
	    uu_table_row_desc(spbuf, u, uu_acp_to_build, NULL);
	    tprintf(buf, "ACP to build: %s.\n", spbuf);
 	    uu_table_row_desc(spbuf, u, uu_cp_per_build, NULL);
 	    tprintf(buf, "  Completeness added per build: %s.\n", spbuf);
        }
        if (u_cp_per_self_build(u) > 0) {
	    tprintf(buf, "Can finish building self at %d cp, will add %d cp per action.\n",
		    u_cp_to_self_build(u), u_cp_per_self_build(u));
        }
        /* Toolup help. */
        if (type_can_toolup(u)) {
	    uu_table_row_desc(spbuf, u, uu_acp_to_toolup, NULL);
	    tprintf(buf, "ACP to toolup: %s.\n", spbuf);
	    uu_table_row_desc(spbuf, u, uu_tp_per_toolup, NULL);
	    tprintf(buf, "  TP/toolup action: %s.\n", spbuf);
	    /* (should put these with type beING built...) */
	    uu_table_row_desc(spbuf, u, uu_tp_to_build, NULL);
	    tprintf(buf, "  TP to build: %s.\n", spbuf);
	    uu_table_row_desc(spbuf, u, uu_tp_max, NULL);
	    tprintf(buf, "  TP max: %s.\n", spbuf);
        }
        
    }
    if (u_acp(u) > 0
        && (u_acp_to_fire(u) > 0
        || type_can_attack(u) > 0
        || u_acp_to_detonate(u) > 0
        )) {
	tprintf(buf, "\nCombat:\n");
	if (type_can_attack(u) > 0) {
	    uu_table_row_desc(spbuf, u, uu_acp_to_attack, NULL);
	    tprintf(buf, "Can attack (ACP %s).\n", spbuf);
	    if (uu_table_row_not_default(u, uu_attack_range, 1)) {
		uu_table_row_desc(spbuf, u, uu_attack_range, NULL);
		tprintf(buf, "Attack range is %s.\n", spbuf);
		uu_table_row_desc(spbuf, u, uu_attack_range_min, NULL);
		tprintf(buf, "Attack range min is %s.\n", spbuf);
	    }
	}
   	if (u_acp_to_fire(u) > 0) {
	    tprintf(buf, "Can fire (%d ACP), at ranges", u_acp_to_fire(u));
	    if (u_range_min(u) > 0) {
		tprintf(buf, " from %d", u_range_min(u));
	    }
	    tprintf(buf, " up to %d", u_range(u));
	    tprintf(buf, ".\n");
	}
	if (type_can_capture(u) > 0) {
	    uu_table_row_desc(spbuf, u, uu_acp_to_capture, NULL);
	    tprintf(buf, "Can capture (ACP %s).\n", spbuf);
	    uu_table_row_desc(spbuf, u, uu_capture, NULL);
	    tprintf(buf, "Chance to capture: %s.\n", spbuf);
	    if (uu_table_row_not_default(u, uu_indep_capture, -1)) {
		uu_table_row_desc(spbuf, u, uu_indep_capture, NULL);
		tprintf(buf, "Chance to capture indep: %s.\n", spbuf);
	    }
	}
	if (u_acp_to_detonate(u) > 0) {
	    tprintf(buf, "Can detonate self (%d ACP)", u_acp_to_detonate(u));
	    if (u_hp_per_detonation(u) < u_hp_max(u)) {
		tprintf(buf, ", losing %d HP per detonation",
			u_hp_per_detonation(u));
	    }
	    tprintf(buf, ".\n");
	    if (u_detonate_on_death(u)) {
		tprintf(buf, "%d%% chance to detonate if mortally hit in combat.\n",
			u_detonate_on_death(u));
	    }
	    uu_table_row_desc(spbuf, u, uu_detonate_on_hit, NULL);
	    tprintf(buf, "Chance to detonate upon being hit: %s", spbuf);
	    tprintf(buf, ".\n");
	    uu_table_row_desc(spbuf, u, uu_detonate_on_capture, NULL);
	    tprintf(buf, "Chance to detonate upon capture: %s", spbuf);
	    tprintf(buf, ".\n");
	    uu_table_row_desc(spbuf, u, uu_detonation_range, NULL);
	    tprintf(buf, "Range of detonation effect is %s", spbuf);
	    tprintf(buf, ".\n");
	}
	uu_table_row_desc(spbuf, u, uu_hit, NULL);
	tprintf(buf, "Hit chances are %s", spbuf);
	tprintf(buf, ".\n");
	uu_table_row_desc(spbuf, u, uu_damage, dice_desc);
	tprintf(buf, "Damages are %s", spbuf);
	tprintf(buf, ".\n");
   	if (u_acp_to_fire(u) > 0) {
	    /* (should describe fire damage if different) */
   	}
    }
    if (uu_table_row_not_default(u, uu_protection, 100)) {
	uu_table_row_desc(spbuf, u, uu_protection, NULL);
	tprintf(buf, "Protection of occupants/transport is %s.\n", spbuf);
    }
    if (uu_table_row_not_default(u, uu_retreat_chance, 0)) {
	uu_table_row_desc(spbuf, u, uu_retreat_chance, NULL);
	tprintf(buf, "Chance to retreat from combat is %s.\n", spbuf);
    }
    if (u_wrecked_type(u) != NONUTYPE) {
    	tprintf(buf, "Becomes %s when destroyed.\n",
		u_type_name(u_wrecked_type(u)));
    }
    if (u_acp(u) > 0
        && (u_acp_to_change_side(u) > 0
            || u_acp_to_disband(u) > 0
            || u_acp_to_transfer_part(u) > 0
            )) {
	tprintf(buf, "\nOther Actions:\n");
	if (u_acp_to_change_side(u) > 0) {
	    tprintf(buf, "Can be given to another side (%d ACP).\n",
		    u_acp_to_change_side(u));
	}
	if (u_acp_to_disband(u) > 0) {
	    tprintf(buf, "Can be disbanded (%d ACP)", u_acp_to_disband(u));
	    if (u_hp_per_disband(u) < u_hp_max(u)) {
	    	tprintf(buf, ", losing %d HP per action", u_hp_per_disband(u));
	    }
	    tprintf(buf, ".\n"); 
	}
	if (u_acp_to_transfer_part(u) > 0) {
	    tprintf(buf, "Can transfer parts (%d ACP).\n",
		    u_acp_to_transfer_part(u));
	}
	if (uu_table_row_not_default(u, uu_acp_to_repair, 0)) {
	    uu_table_row_desc(spbuf, u, uu_acp_to_repair, NULL);
	    tprintf(buf, "ACP to repair is %s.\n", spbuf);
	    uu_table_row_desc(spbuf, u, uu_repair, NULL);
	    tprintf(buf, "Repair performance is %s.\n", spbuf);
	    uu_table_row_desc(spbuf, u, uu_hp_to_repair, NULL);
	    tprintf(buf, "Min HP to repair is %s.\n", spbuf);
	}
    }
    if (!g_see_all()) {
    	tprintf(buf, "\nVision:\n");
	tprintf(buf, "%d%% chance to be seen at outset of game.\n",
		u_already_seen(u));
	tprintf(buf, "%d%% chance to be seen at outset of game if independent.\n",
		u_already_seen_indep(u));
	if (u_see_always(u))
	  tprintf(buf, "Always seen if terrain has been seen.\n");
	/* (should put other _see_ things here) */
	switch (u_vision_range(u)) {
	  case -1:
	    tprintf(buf, "Can never see other units.\n");
	    break;
	  case 0:
	    tprintf(buf, "Can see other units at own location.\n");
	    break;
	  case 1:
	    /* Default range, no need to say anything. */
	    break;
	  default:
	    tprintf(buf, "Can see units up to %d cells away.\n", u_vision_range(u));
	    break;
	}
	if (u_vision_range(u) >= 0) {
	  uu_table_row_desc(spbuf, u, uu_see_at, NULL);
	  tprintf(buf, "Chance to see if in same cell is %s", spbuf);
	  tprintf(buf, ".\n");
	}
	if (u_vision_range(u) >= 1) {
	  uu_table_row_desc(spbuf, u, uu_see_adj, NULL);
	  tprintf(buf, "Chance to see if adjacent is %s", spbuf);
	  tprintf(buf, ".\n");
	}
	if (u_vision_range(u) >= 2) {
	  uu_table_row_desc(spbuf, u, uu_see, NULL);
	  tprintf(buf, "Chance to see in general is %s", spbuf);
	  tprintf(buf, ".\n");
	}
    }
    if (nummtypes > 0) {
	tprintf(buf, "\nMaterial Handling:\n");
	for_all_material_types(m) {
	    tprintf(buf, "  %s", m_type_name(m));
	    if (um_base_production(u, m) > 0) {
		tprintf(buf, ", %d base production", um_base_production(u, m));
	    }
	    if (um_storage_x(u, m) > 0) {
		tprintf(buf, ", %d storage", um_storage_x(u, m));
		if (um_initial(u, m) > 0) {
		    tprintf(buf, " (%d initially)", min(um_initial(u, m), um_storage_x(u, m)));
		}
	    }
	    if (um_base_consumption(u, m) > 0) {
		tprintf(buf, ", %d base consumption", um_base_consumption(u, m));
	    }
	    if (um_inlength(u, m) > 0) {
		tprintf(buf, ", receive from %d cells away", um_inlength(u, m));
	    }
	    if (um_outlength(u, m) > 0) {
		tprintf(buf, ", send up to %d cells away", um_outlength(u, m));
	    }
	    tprintf(buf, "\n");
	}
    }
    /* (should display weather interaction here) */
    if (u_spy_chance(u) > 0 /* and random event in use */) {
	tprintf(buf, "%d%% chance to spy, on units up to %d away.",
		u_spy_chance(u), u_spy_range(u));
    }
    if (u_revolt(u) > 0 /* and random event in use */) {
	tprintf(buf, "%d.%d%% chance of revolt.\n",
		u_revolt(u) / 100, u_revolt(u) % 100);
    }
    /* Display the designer's notes for this type. */
    if (u_notes(u) != lispnil) {
	tprintf(buf, "\nNotes:\n");
	append_notes(buf, u_notes(u));
    }
}

void
describe_mtype(m, key, buf)
int m;
char *key, *buf;
{
    append_help_phrase(buf, m_help(m));
    if (m_people(m) > 0) {
	tprintf(buf, "1 of this represents %d individuals.", m_people(m));
    }
    /* Display the designer's notes for this type. */
    if (m_notes(m) != lispnil) {
	tprintf(buf, "\nNotes:\n");
	append_notes(buf, m_notes(m));
    }
}

void
describe_ttype(t, key, buf)
int t;
char *key, *buf;
{
    int m, ct;

    append_help_phrase(buf, t_help(t));
    switch (t_subtype(t)) {
      case cellsubtype:
	break;
      case bordersubtype:
	tprintf(buf, " (a border type)\n");
	break;
      case connectionsubtype:
	tprintf(buf, " (a connection type)\n");
	break;
      case coatingsubtype:
	tprintf(buf, " (a coating type)\n");
	break;
    }
    tprintf(buf, "Generic unit capacity is %d.\n", t_capacity(t));
    if (minelev != maxelev /* should be "elevscanvary" */) {
	if (t_elev_min(t) == t_elev_max(t)) {
	    tprintf(buf, "Elevation is always %d.\n",
		    t_elev_min(t));
	} else {
	    tprintf(buf, "Elevations fall between %d and %d.\n",
		    t_elev_min(t), t_elev_max(t));
	}
    }
    if (t_thickness(t) > 0) {
	tprintf(buf, "Thickness is %d.\n", t_thickness(t));
    }
    if (any_temp_variation) {
	if (t_temp_min(t) == t_temp_max(t)) {
	    tprintf(buf, "Temperature is always %d.\n",
		    t_temp_min(t));
	} else {
	    tprintf(buf, "Temperatures fall between %d and %d, averaging %d.\n",
		    t_temp_min(t), t_temp_max(t), t_temp_avg(t));
	}
    }
    if (any_wind_variation) {
	if (t_wind_force_min(t) == t_wind_force_max(t)) {
	    tprintf(buf, "Wind force is always %d.\n",
		    t_wind_force_min(t));
	} else {
	    tprintf(buf, "Wind forces fall between %d and %d, averaging %d.\n",
		    t_wind_force_min(t), t_wind_force_max(t), t_wind_force_avg(t));
	}
	if (t_wind_force_variability(t) > 0) {
	    tprintf(buf, "%d%% chance each turn that wind force will change.\n",
		    t_wind_force_variability(t));
	}
	if (t_wind_variability(t) > 0) {
	    tprintf(buf, "%d%% chance each turn that wind direction will change.\n",
		    t_wind_variability(t));
	}
    }
    if (any_clouds) {
	if (t_clouds_min(t) == t_clouds_max(t)) {
	    tprintf(buf, "Cloud cover is always %d.\n",
		    t_clouds_min(t));
	} else {
	    tprintf(buf, "Cloud cover falls between %d and %d\n",
		    t_clouds_min(t), t_clouds_max(t));
	}
    }
    /* Display relationships with materials. */
    if (nummtypes > 0) {
	for_all_material_types(m) {
	    if (tm_storage_x(t, m) > 0) {
	    	tprintf(buf, "Can store up to %d %s", tm_storage_x(t, m), m_type_name(m));
	    	tprintf(buf, " (normally starts game with %d)",
	    		min(tm_initial(t, m), tm_storage_x(t, m)));
	    	tprintf(buf, ".\n");
	    }
	    if (tm_production(t, m) > 0 || tm_consumption(t, m) > 0) {
		tprintf(buf, " Produces %d and consumes %d each turn.\n",
			tm_production(t, m), tm_consumption(t, m));
	    }
	}
    }
    /* Display relationships with any coating terrain types. */
    if (numcoattypes > 0) {
	tprintf(buf, "Coatings:\n");
    	for_all_terrain_types(ct) {
	    if (t_is_coating(ct)) {
		tprintf(buf, "%s coats, depths %d up to %d",
			t_type_name(ct), tt_coat_min(ct, t), tt_coat_max(ct, t));
	    }
    	}
    }
    /* (should display damaged and exhaustion types) */
    /* Display the designer's notes for this type. */
    if (t_notes(t) != lispnil) {
	tprintf(buf, "\nNotes:\n");
	append_notes(buf, t_notes(t));
    }
}

void
describe_scorekeepers(arg, key, buf)
int arg;
char *key, *buf;
{
    int i = 1;
    Scorekeeper *sk;

    if (scorekeepers == NULL) {
	tprintf(buf, "No scores are being kept.");
    } else {
	for_all_scorekeepers(sk) {
	    if (numscorekeepers > 1) {
		tprintf(buf, "%d.  ", i++);
	    }
	    if (symbolp(sk->body)
		&& match_keyword(sk->body, K_LAST_SIDE_WINS)) {
		tprintf(buf, "The last side left in the game wins.");
		/* (should mention point values also) */
	    } else {
		tprintf(buf, "(an indescribably complicated scorekeeper)");
	    }
	    tprintf(buf, "\n");
	}
    }
}

/* List each synthesis method and its parameters. */

void
describe_setup(arg, key, buf)
int arg;
char *key, *buf;
{
    int u, methkey;
    Obj *synthlist, *methods, *method;
    
    tprintf(buf, "Synthesis done when setting up this game:\n");
    synthlist = g_synth_methods();
    for (methods = synthlist; methods != lispnil; methods = cdr(methods)) {
	method = car(methods);
	if (symbolp(method)) {
	    methkey = keyword_code(c_string(method));
	    switch (methkey) {
	      case K_MAKE_COUNTRIES:
		tprintf(buf, "\nCountries:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		tprintf(buf, "  %d cells across, between %d and %d cells apart.\n",
			2 * g_radius_min() + 1,
			g_separation_min(), g_separation_max());
		if (t_property_not_default(t_country_min, 0)) {
		    t_property_desc(spbuf, t_country_min, NULL);
		    tprintf(buf, "  Minimum terrain in each country: %s.\n", spbuf);
		}
		if (t_property_not_default(t_country_max, -1)) {
		    t_property_desc(spbuf, t_country_max, NULL);
		    tprintf(buf, "  Maximum terrain in each country: %s.\n", spbuf);
		}
		if (u_property_not_default(u_start_with, 0)) {
		    u_property_desc(spbuf, u_start_with, NULL);
		    tprintf(buf, "  Start with: %s.\n", spbuf);
		}
		if (u_property_not_default(u_indep_near_start, 0)) {
		    u_property_desc(spbuf, u_indep_near_start, NULL);
		    tprintf(buf, "  Independents nearby: %s.\n", spbuf);
		}
		tprintf(buf, "  Favored terrain:\n");
		for_all_unit_types(u) {
		    if (u_start_with(u) > 0 || u_indep_near_start(u)) {
			tprintf(buf, "  %s:", u_type_name(u));
			ut_table_row_desc(spbuf, u, ut_favored, NULL);
			tprintf(buf, " %s\n", spbuf);
		    }
		}
		if (g_radius_max() != 0) {
		    tprintf(buf, "Country growth:\n");
		    if (g_radius_max() == -1) {
			tprintf(buf, "  Up to entire world");
		    } else {
			tprintf(buf, "  Up to %d cells across", 2 * g_radius_max() + 1);
		    }
		    tprintf(buf, ", %d chance to stop if blocked.\n", g_growth_stop());
		    if (t_property_not_default(t_country_growth, 100)) {
			t_property_desc(spbuf, t_country_max, NULL);
			tprintf(buf, "  Growth chance, by terrain: %s.\n", spbuf);
		    }
		    if (t_property_not_default(t_country_takeover, 0)) {
			t_property_desc(spbuf, t_country_takeover, NULL);
			tprintf(buf, "  Takeover chance, by terrain: %s.\n", spbuf);
		    }
		    if (u_property_not_default(u_unit_growth, 0)) {
			u_property_desc(spbuf, u_unit_growth, NULL);
			tprintf(buf, "  Chance for additional unit: %s.\n", spbuf);
		    }
		    if (u_property_not_default(u_indep_growth, 0)) {
			u_property_desc(spbuf, u_indep_growth, NULL);
			tprintf(buf, "  Chance for additional independent unit: %s.\n", spbuf);
		    }
		    if (u_property_not_default(u_unit_takeover, 0)) {
			u_property_desc(spbuf, u_unit_takeover, NULL);
			tprintf(buf, "  Chance to take over units: %s.\n", spbuf);
		    }
		    if (u_property_not_default(u_indep_takeover, 0)) {
			u_property_desc(spbuf, u_indep_takeover, NULL);
			tprintf(buf, "  Chance to take over independent unit: %s.\n", spbuf);
		    }
		    if (u_property_not_default(u_country_units_max, -1)) {
			u_property_desc(spbuf, u_country_units_max, NULL);
			tprintf(buf, "  Maximum units in country: %s.\n", spbuf);
		    }
		    if (t_property_not_default(t_country_people, 0)) {
			t_property_desc(spbuf, t_country_people, NULL);
			tprintf(buf, "  People takeover chance, by terrain: %s.\n", spbuf);
		    }
		}
		break;
	      case K_MAKE_EARTHLIKE_TERRAIN:
		tprintf(buf, "\nEarthlike terrain:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		break;
	      case K_MAKE_FRACTAL_PTILE_TERRAIN:
		tprintf(buf, "\nFractal percentile terrain:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		tprintf(buf, "  Alt blobs density %d, size %d, height %d\n",
			g_alt_blob_density(), g_alt_blob_size(), g_alt_blob_height());
		tprintf(buf, "    %d smoothing passes\n", g_alt_smoothing());
		t_property_desc(spbuf, t_alt_min, NULL);
		tprintf(buf, "    Lower percentiles: %s.\n", spbuf);
		t_property_desc(spbuf, t_alt_max, NULL);
		tprintf(buf, "    Upper percentiles: %s.\n", spbuf);
		tprintf(buf, "  Wet blobs density %d, size %d, height %d\n",
			g_wet_blob_density(), g_wet_blob_size(), g_wet_blob_height());
		tprintf(buf, "    %d smoothing passes\n", g_wet_smoothing());
		t_property_desc(spbuf, t_wet_min, NULL);
		tprintf(buf, "    Lower percentiles: %s.\n", spbuf);
		t_property_desc(spbuf, t_wet_max, NULL);
		tprintf(buf, "    Upper percentiles: %s.\n", spbuf);
		break;
	      case K_MAKE_INDEPENDENT_UNITS:
		tprintf(buf, "\nIndependent units:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		/* (should show indep density and people) */
		break;
	      case K_MAKE_INITIAL_MATERIALS:
		tprintf(buf, "\nMaterials:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		/* (should show unit and terrain initial supply) */
		break;
	      case K_MAKE_MAZE_TERRAIN:
		tprintf(buf, "\nMaze terrain:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		break;
	      case K_MAKE_RANDOM_DATE:
		tprintf(buf, "\nRandom date:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		break;
	      case K_MAKE_RANDOM_TERRAIN:
		tprintf(buf, "\nRandom terrain:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		break;
	      case K_MAKE_RIVERS:
		tprintf(buf, "\nRivers:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		if (t_property_not_default(t_river_chance, 0)) {
		    t_property_desc(spbuf, t_river_chance, NULL);
		    tprintf(buf, "  Chance to be river source: %s.\n", spbuf);
		    if (g_river_sink_terrain() != NONTTYPE)
		      tprintf(buf, "  Sink is %s.\n", t_type_name(g_river_sink_terrain()));
		    else
		      tprintf(buf, "  No special sink terrain type.\n");
		}
		break;
	      case K_MAKE_ROADS:
		tprintf(buf, "\nRoads:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		tprintf(buf, "  Chance to run:\n");
		for_all_unit_types(u) {
		    if (uu_table_row_not_default(u, uu_road_chance, 0)) {
			tprintf(buf, "  %s:", u_type_name(u));
			uu_table_row_desc(spbuf, u, uu_road_chance, NULL);
			tprintf(buf, " %s\n", spbuf);
		    }

		}
		/* (should include routing terrain preferences) */
		break;
	      case K_MAKE_WEATHER:
		tprintf(buf, "\nWeather:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		break;
	      case K_NAME_GEOGRAPHICAL_FEATURES:
		tprintf(buf, "\nNames for geographical features:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		break;
	      case K_NAME_UNITS_RANDOMLY:
		tprintf(buf, "\nNames for units:");
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		break;
	      default:
		tprintf(buf, "\n%s:", c_string(method));
		describe_synth_run(buf, methkey);
		tprintf(buf, "\n");
		break;
	    }
	} else if (consp(method)) {
	}
    }
}

static void
describe_synth_run(buf, methkey)
char *buf;
int methkey;
{
    int calls, runs;
  
    if (get_synth_method_uses(methkey, &calls, &runs)) {
	if (calls > 0) {
	    if (calls == 1 && runs == 1) {
		tprintf(buf, " (was run)");
	    } else if (calls == 1 && runs == 0) {
		tprintf(buf, " (was not run)");
	    } else {
		tprintf(buf, " (was called %d times, was run %d times)",
			calls, runs);
	    }
	} else {
	    tprintf(buf, " (not attempted)");
	}
    } else {
	tprintf(buf, " (???)");
    }
}

/* This describes a command (from cmd.def et al) in a way that all
   interfaces can use. */

void
describe_command (ch, name, help, onechar, buf)
int ch, onechar;
char *name, *help, *buf;
{
    if (onechar && ch != '\0') {
	if (ch < ' ' || ch > '~') { 
	    sprintf(buf+strlen(buf), "'^%c' ", (ch ^ 0x40));
	} else {
	    sprintf(buf+strlen(buf), " '%c' ", ch);
	}
    } else if (!onechar && ch == '\0') {
	strcat(buf, "\"");
	strcat(buf, name);
	strcat(buf, "\"");
    } else
      return;
    strcat(buf, " ");
    strcat(buf, help);
    strcat(buf, "\n");
}

int
u_property_not_default(fn, dflt)
int (*fn) PARAMS ((int i));
int dflt;
{
    int u, val;

    for_all_unit_types(u) {
	val = (*fn)(u);
	if (val != dflt)
	  return TRUE;
    }
    return FALSE;
}

int
t_property_not_default(fn, dflt)
int (*fn) PARAMS ((int i));
int dflt;
{
    int t, val;

    for_all_terrain_types(t) {
	val = (*fn)(t);
	if (val != dflt)
	  return TRUE;
    }
    return FALSE;
}

int
uu_table_row_not_default(u, fn, dflt)
int u, dflt;
int (*fn) PARAMS ((int i, int j));
{
    int u2, val2;

    for_all_unit_types(u2) {
	val2 = (*fn)(u, u2);
	if (val2 != dflt)
	  return TRUE;
    }
    return FALSE;
}

int
ut_table_row_not_default(u, fn, dflt)
int u, dflt;
int (*fn) PARAMS ((int i, int j));
{
    int t, val2;

    for_all_terrain_types(t) {
	val2 = (*fn)(u, t);
	if (val2 != dflt)
	  return TRUE;
    }
    return FALSE;
}

int
um_table_row_not_default(u, fn, dflt)
int u, dflt;
int (*fn) PARAMS ((int i, int j));
{
    int m, val2;

    for_all_material_types(m) {
	val2 = (*fn)(u, m);
	if (val2 != dflt)
	  return TRUE;
    }
    return FALSE;
}

struct histo {
    int val, num;
};

/* This compare will sort histogram entries in *reverse* order
   (most common values first). */

static int
histogram_compare(h1, h2)
CONST void *h1, *h2;
{
    if (((struct histo *) h2)->num != ((struct histo *) h1)->num) {
    	return ((struct histo *) h2)->num - ((struct histo *) h1)->num;
    } else {
    	return ((struct histo *) h2)->val - ((struct histo *) h1)->val;
    }
}

void
u_property_desc(buf, fn, formatter)
char *buf;
int (*fn) PARAMS ((int i));
void (*formatter) PARAMS ((char *buf, int val));
{
    int val, u, val2, constant = TRUE, found;
    int i, numentries, first;
    struct histo histogram[MAXUTYPES];

    /* Compute a histogram of all the values for the given property. */
    numentries = 0;
    val = (*fn)(0);
    histogram[numentries].val = val;
    histogram[numentries].num = 1;
    ++numentries;
    for_all_unit_types(u) {
	val2 = (*fn)(u);
	if (val2 == val) {
	    ++(histogram[0].num);
	} else {
	    constant = FALSE;
	    found = FALSE;
	    for (i = 1; i < numentries; ++i) {
		if (val2 == histogram[i].val) {
		    ++(histogram[i].num);
		    found = TRUE;
		    break;
		}
	    }
	    if (!found) {
		histogram[numentries].val = val2;
		histogram[numentries].num = 1;
		++numentries;
	    }
	}
    }
    /* The constant table/row case is easily disposed of. */
    if (constant) {
	if (formatter == NULL) {
	    sprintf(buf, "%d for all types", val);
    	} else {
	    (*formatter)(buf, val);
	    strcat(buf, " for all types");
    	}
    	return;
    }
    /* Not a constant row; sort the histogram and compose a description. */
    qsort(histogram, numentries, sizeof(struct histo), histogram_compare);
    if (histogram[0].num * 2 >= numutypes) {
    	if (formatter == NULL) {
	    sprintf(buf, "%d by default", histogram[0].val);
    	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[0].val);
	    sprintf(buf, "%s by default", subbuf);
    	}
    	i = 1;
    } else {
	buf[0] = '\0';
    	i = 0;
    }
    for (; i < numentries; ++i) {
	if (i > 0)
	  tnprintf(buf, BUFSIZE, ", ");
	if (formatter == NULL) {
	    tnprintf(buf, BUFSIZE, "%d for ", histogram[i].val);
	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[i].val);
	    tnprintf(buf, BUFSIZE, "%s for ", subbuf);
	}
	first = TRUE;
	for_all_unit_types(u) {
	    if ((*fn)(u) == histogram[i].val) {
		if (!first)
		  tnprintf(buf, BUFSIZE, ",");
		else
		  first = FALSE;
		tnprintf(buf, BUFSIZE, "%s", u_type_name(u));
	    }
	}
    }
}

void
t_property_desc(buf, fn, formatter)
char *buf;
int (*fn) PARAMS ((int i));
void (*formatter) PARAMS ((char *buf, int val));
{
    int val, t, val2, constant = TRUE, found;
    int i, numentries, first;
    struct histo histogram[MAXUTYPES];

    /* Compute a histogram of all the values for the given property. */
    numentries = 0;
    val = (*fn)(0);
    histogram[numentries].val = val;
    histogram[numentries].num = 1;
    ++numentries;
    for_all_terrain_types(t) {
	val2 = (*fn)(t);
	if (val2 == val) {
	    ++(histogram[0].num);
	} else {
	    constant = FALSE;
	    found = FALSE;
	    for (i = 1; i < numentries; ++i) {
		if (val2 == histogram[i].val) {
		    ++(histogram[i].num);
		    found = TRUE;
		    break;
		}
	    }
	    if (!found) {
		histogram[numentries].val = val2;
		histogram[numentries].num = 1;
		++numentries;
	    }
	}
    }
    /* The constant table/row case is easily disposed of. */
    if (constant) {
	if (formatter == NULL) {
	    sprintf(buf, "%d for all types", val);
    	} else {
	    (*formatter)(buf, val);
	    strcat(buf, " for all types");
    	}
    	return;
    }
    /* Not a constant row; sort the histogram and compose a description. */
    qsort(histogram, numentries, sizeof(struct histo), histogram_compare);
    if (histogram[0].num * 2 >= numttypes) {
    	if (formatter == NULL) {
	    sprintf(buf, "%d by default", histogram[0].val);
    	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[0].val);
	    sprintf(buf, "%s by default", subbuf);
    	}
    	i = 1;
    } else {
	buf[0] = '\0';
    	i = 0;
    }
    for (; i < numentries; ++i) {
	if (i > 0)
	  tnprintf(buf, BUFSIZE, ", ");
	if (formatter == NULL) {
	    tnprintf(buf, BUFSIZE, "%d for ", histogram[i].val);
	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[i].val);
	    tnprintf(buf, BUFSIZE, "%s for ", subbuf);
	}
	first = TRUE;
	for_all_terrain_types(t) {
	    if ((*fn)(t) == histogram[i].val) {
		if (!first)
		  tnprintf(buf, BUFSIZE, ",");
		else
		  first = FALSE;
		tnprintf(buf, BUFSIZE, "%s", t_type_name(t));
	    }
	}
    }
}

void
uu_table_row_desc(buf, u, fn, formatter)
char *buf;
int u;
int (*fn) PARAMS ((int i, int j));
void (*formatter) PARAMS ((char *buf, int val));
{
    int val = (*fn)(u, 0), val2, u2, constant = TRUE, found;
    int i, numentries, first;
    struct histo histogram[MAXUTYPES];

    /* Compute a histogram of all the values in the row of the table. */
    numentries = 0;
    histogram[numentries].val = val;
    histogram[numentries].num = 1;
    ++numentries;
    for_all_unit_types(u2) {
	val2 = (*fn)(u, u2);
	if (val2 == val) {
	    ++(histogram[0].num);
	} else {
	    constant = FALSE;
	    found = FALSE;
	    for (i = 1; i < numentries; ++i) {
		if (val2 == histogram[i].val) {
		    ++(histogram[i].num);
		    found = TRUE;
		    break;
		}
	    }
	    if (!found) {
		histogram[numentries].val = val2;
		histogram[numentries].num = 1;
		++numentries;
	    }
	}
    }
    /* The constant table/row case is easily disposed of. */
    if (constant) {
	if (formatter == NULL) {
	    sprintf(buf, "%d for all types", val);
    	} else {
	    (*formatter)(buf, val);
	    strcat(buf, " for all types");
    	}
    	return;
    }
    /* Not a constant row; sort the histogram and compose a description. */
    qsort(histogram, numentries, sizeof(struct histo), histogram_compare);
    if (histogram[0].num * 2 >= numutypes) {
    	if (formatter == NULL) {
	    sprintf(buf, "%d by default", histogram[0].val);
    	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[0].val);
	    sprintf(buf, "%s by default", subbuf);
    	}
    	i = 1;
    } else {
	buf[0] = '\0';
    	i = 0;
    }
    for (; i < numentries; ++i) {
	if (i > 0) tnprintf(buf, BUFSIZE, ", ");
	if (formatter == NULL) {
	    tnprintf(buf, BUFSIZE, "%d vs ", histogram[i].val);
	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[i].val);
	    tnprintf(buf, BUFSIZE, "%s vs ", subbuf);
	}
	first = TRUE;
	for_all_unit_types(u2) {
	    if ((*fn)(u, u2) == histogram[i].val) {
		if (!first) tnprintf(buf, BUFSIZE, ",");  else first = FALSE;
		tnprintf(buf, BUFSIZE, "%s", u_type_name(u2));
	    }
	}
    }
}

void
ut_table_row_desc(buf, u, fn, formatter)
char *buf;
int u;
int (*fn) PARAMS ((int i, int j));
void (*formatter) PARAMS ((char *buf, int val));
{
    int val = (*fn)(u, 0), val2, t, constant = TRUE, found;
    int i, numentries, first;
    struct histo histogram[MAXUTYPES];

    /* Compute a histogram of all the values in the row of the table. */
    numentries = 0;
    histogram[numentries].val = val;
    histogram[numentries].num = 1;
    ++numentries;
    for_all_terrain_types(t) {
	val2 = (*fn)(u, t);
	if (val2 == val) {
	    ++(histogram[0].num);
	} else {
	    constant = FALSE;
	    found = FALSE;
	    for (i = 1; i < numentries; ++i) {
		if (val2 == histogram[i].val) {
		    ++(histogram[i].num);
		    found = TRUE;
		    break;
		}
	    }
	    if (!found) {
		histogram[numentries].val = val2;
		histogram[numentries].num = 1;
		++numentries;
	    }
	}
    }
    /* The constant table/row case is easily disposed of. */
    if (constant) {
	if (formatter == NULL) {
	    sprintf(buf, "%d for all types", val);
    	} else {
	    (*formatter)(buf, val);
	    strcat(buf, " for all types");
    	}
    	return;
    }
    /* Not a constant row; sort the histogram and compose a description. */
    qsort(histogram, numentries, sizeof(struct histo), histogram_compare);
    if (histogram[0].num * 2 >= numttypes) {
    	if (formatter == NULL) {
	    sprintf(buf, "%d by default", histogram[0].val);
    	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[0].val);
	    sprintf(buf, "%s by default", subbuf);
    	}
    	i = 1;
    } else {
	buf[0] = '\0';
    	i = 0;
    }
    for (; i < numentries; ++i) {
	if (i > 0)
	  tnprintf(buf, BUFSIZE, ", ");
	if (formatter == NULL) {
	    tnprintf(buf, BUFSIZE, "%d vs ", histogram[i].val);
	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[i].val);
	    tnprintf(buf, BUFSIZE, "%s vs ", subbuf);
	}
	first = TRUE;
	for_all_terrain_types(t) {
	    if ((*fn)(u, t) == histogram[i].val) {
		if (!first)
		  tnprintf(buf, BUFSIZE, ",");
		else
		  first = FALSE;
		tnprintf(buf, BUFSIZE, "%s", t_type_name(t));
	    }
	}
    }
}

void
um_table_row_desc(buf, u, fn, formatter)
char *buf;
int u;
int (*fn) PARAMS ((int i, int j));
void (*formatter) PARAMS ((char *buf, int val));
{
    int val = (*fn)(u, 0), val2, m, constant = TRUE, found;
    int i, numentries, first;
    struct histo histogram[MAXUTYPES];

    /* Compute a histogram of all the values in the row of the table. */
    numentries = 0;
    histogram[numentries].val = val;
    histogram[numentries].num = 1;
    ++numentries;
    for_all_material_types(m) {
	val2 = (*fn)(u, m);
	if (val2 == val) {
	    ++(histogram[0].num);
	} else {
	    constant = FALSE;
	    found = FALSE;
	    for (i = 1; i < numentries; ++i) {
		if (val2 == histogram[i].val) {
		    ++(histogram[i].num);
		    found = TRUE;
		    break;
		}
	    }
	    if (!found) {
		histogram[numentries].val = val2;
		histogram[numentries].num = 1;
		++numentries;
	    }
	}
    }
    /* The constant table/row case is easily disposed of. */
    if (constant) {
	if (formatter == NULL) {
	    sprintf(buf, "%d for all types", val);
    	} else {
	    (*formatter)(buf, val);
	    strcat(buf, " for all types");
    	}
    	return;
    }
    /* Not a constant row; sort the histogram and compose a description. */
    qsort(histogram, numentries, sizeof(struct histo), histogram_compare);
    if (histogram[0].num * 2 >= nummtypes) {
    	if (formatter == NULL) {
	    sprintf(buf, "%d by default", histogram[0].val);
    	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[0].val);
	    sprintf(buf, "%s by default", subbuf);
    	}
    	i = 1;
    } else {
	buf[0] = '\0';
    	i = 0;
    }
    for (; i < numentries; ++i) {
	if (i > 0)
	  tnprintf(buf, BUFSIZE, ", ");
	if (formatter == NULL) {
	    tnprintf(buf, BUFSIZE, "%d vs ", histogram[i].val);
	} else {
	    char subbuf[40];

	    (*formatter)(subbuf, histogram[i].val);
	    tnprintf(buf, BUFSIZE, "%s vs ", subbuf);
	}
	first = TRUE;
	for_all_material_types(m) {
	    if ((*fn)(u, m) == histogram[i].val) {
		if (!first)
		  tnprintf(buf, BUFSIZE, ",");
		else
		  first = FALSE;
		tnprintf(buf, BUFSIZE, "%s", m_type_name(m));
	    }
	}
    }
}

/* A simple table-printing utility. Blanks out default values so they don't
   clutter the table. */
/* (not currently used anywhere?) */

void
append_number(buf, value, dflt)
char *buf;
int value, dflt;
{
    if (value != dflt) {
	sprintf(tmpbuf, "%5d ", value);
	tprintf(buf, "%s", tmpbuf);
    } else {
	tprintf(buf, "      ");
    }
}

void
append_help_phrase(buf, phrase)
char *buf, *phrase;
{
    if (empty_string(phrase))
      return;
    tprintf(buf, "-- ");
    tprintf(buf, "%s", phrase);
    tprintf(buf, " --\n");
}

void
append_notes(buf, notes)
char *buf;
Obj *notes;
{
    char *notestr;
    Obj *rest;

    if (stringp(notes)) {
	notestr = c_string(notes);
	if (strlen(notestr) > 0) { 
	    strcat(buf, notestr);
	    strcat(buf, " ");
	} else {
	    strcat(buf, "\n");
	}
    } else if (consp(notes)) {
	for (rest = notes; rest != lispnil; rest = cdr(rest)) {
	    append_notes(buf, car(rest));
	}
    } else {
	/* error? */
    }
}

void
notify_instructions()
{
    Obj *instructions = mainmodule->instructions, *rest;

    if (instructions != lispnil) {
	if (stringp(instructions)) {
	    notify_all("%s", c_string(instructions));
	} else if (consp(instructions)) {
	    for (rest = instructions; rest != lispnil; rest = cdr(rest)) {
		if (stringp(car(rest))) {
		    notify_all("%s", c_string(car(rest)));
		} else {
		    /* (should probably warn about this case too) */
		}
	    }
	} else {
	    run_warning("Instructions are of wrong type");
	}
    } else {
	notify_all("(no instructions supplied)");
    }
}

/* Print the news file onto the console if there is anything to print. */

void
print_any_news()
{
    FILE *fp;

    fp = open_library_file(news_filename());
    if (fp != NULL) {
	printf("\n                              XCONQ NEWS\n\n");
	while (fgets(spbuf, BUFSIZE-1, fp) != NULL) {
	    fputs(spbuf, stdout);
	}
	/* Add another blank line, to separate from init printouts. */
	printf("\n");
	fclose(fp);
    }
}

/* Generate a readable description of the game (design) being played. */
/* This works by writing out appropriate help nodes, along with some
   indexing material.  This does *not* do interface-specific help,
   such as commands. */

void
print_game_description_to_file(fp)
FILE *fp;
{
    HelpNode *node;

    /* (need to work on which nodes to dump out) */
    for (node = first_help_node; node != first_help_node; node = node->next) {
	get_help_text(node);
	if (node->text != NULL) {
	    fprintf(fp, "\014\n%s\n", node->key);
	    fprintf(fp, "%s\n", node->text);
	}
    }
}
