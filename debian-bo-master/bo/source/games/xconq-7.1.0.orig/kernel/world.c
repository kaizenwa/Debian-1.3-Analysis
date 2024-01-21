/* Worlds and areas in Xconq.
   Copyright (C) 1987, 1988, 1989, 1991, 1992, 1993, 1994, 1995, 1996
   Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
extern int num_features PARAMS ((void));

/* The main world structure. */

World world;

/* The current area of the world. */

Area area;

/* This is the number of terrain types that can fill a cell. */

int numcelltypes = 0;

/* This is the number of terrain types that can be border terrain. */

int numbordtypes = 0;

/* This is the number of types that can be connections. */

int numconntypes = 0;

/* This is the number of types that can be coatings. */

int numcoattypes = 0;

/* (Features could also be used to implement the region 
   labeling scheme now used only in the machine player) */

Feature *featurelist = NULL;

Feature *last_feature = NULL;

/* Feature id 0 means no geographical feature defined. */

int nextfid = 1;

int minelev;
int maxelev;

int mintemp;
int maxtemp;

int any_temp_variation = FALSE;

int any_temp_variation_in_layer = FALSE;

int minwindforce;
int maxwindforce;

int any_wind_variation = FALSE;

int any_wind_variation_in_layer = FALSE;

int minclouds;
int maxclouds;

int any_clouds = FALSE;

/* This is true if it is ever possible to have quantities of material
   in cells. */

int any_materials_in_terrain = FALSE;

static Feature *tmpfeature;

static int tmpint;

static int tmpint2, tmpint3;

/* Clean out all the world and area data. */

void
init_world()
{
    memset(&world, 0, sizeof(World));
    /* These default values effectively disable day and year effects. */
    world.daylength = 1;
    world.yearlength = 1;
    /* Init the current (default) area. */
    memset(&area, 0, sizeof(Area));
    /* Note especially that area width and height dimensions are now zero. */
}

int
set_world_circumference(circum, warn)
int circum, warn;
{
    /* All world circumferences are valid, no checks necessary. */
    world.circumference = circum;
    world.daylight_width = ((50 * world.circumference) / 100) / 2;
    world.twilight_width = ((60 * world.circumference) / 100) / 2;
    area.xwrap = (world.circumference == area.width);
    return TRUE;
}

int
set_area_shape(width, height, warn)
int width, height, warn;
{
    if (!valid_area_shape(width, height, warn))
      return FALSE;
    area.width = width;  area.height = height;
    area.halfheight = area.height / 2;
    area.maxdim = max(area.width, area.height);
    area.xwrap = (world.circumference == area.width);
    area.numcells = area.width * area.height;
    if (!area.xwrap)
      area.numcells = (area.numcells * 3) / 4;
    return TRUE;
}

int
valid_area_shape(width, height, warn)	
int width, height, warn;
{
    if (width < MINWIDTH || height < MINHEIGHT) {
	if (warn)
	  init_warning("area is %dx%d, too small", width, height);
    	return FALSE;
    }
    if (width != world.circumference && width * 2 < height) {
	if (warn)
	  init_warning("hexagon area is %dx%d, impossible dimensions",
		       width, height);
    	return FALSE;
    }
    return TRUE;
}

void
check_area_shape()
{
    if (area.width == 0 || area.height == 0)
      run_error("0x0 area");
    if (!valid_area_shape(area.width, area.height, TRUE))
      run_error("sorry, this is a fatal error here");
}

/* Calculate globals that we can use later to decide if particular
   classes of calculations need to be done, such as weather changes. */

void
calculate_world_globals()
{
    int t, m;

    /* This is a last chance to set a default world size.
       Will usually be set already, by players or by module. */
    if (area.width <= 0 && area.height <= 0)
      set_area_shape(DEFAULTWIDTH, DEFAULTHEIGHT, 0);
    minelev = t_elev_min(0);
    maxelev = t_elev_max(0);
    mintemp = t_temp_min(0);
    maxtemp = t_temp_max(0);
    minwindforce = t_wind_force_min(0);
    maxwindforce = t_wind_force_max(0);
    minclouds = t_clouds_min(0);
    maxclouds = t_clouds_max(0);
    for_all_terrain_types(t) {
	if (t_elev_min(t) < minelev)
	  minelev = t_elev_min(t);
	if (t_elev_max(t) > maxelev)
	  maxelev = t_elev_max(t);
	if (t_temp_min(t) < mintemp)
	  mintemp = t_temp_min(t);
	if (t_temp_max(t) > maxtemp)
	  maxtemp = t_temp_max(t);
	if (t_wind_force_min(t) < maxwindforce)
	  minwindforce = t_wind_force_min(t);
	if (t_wind_force_max(t) > maxwindforce)
	  maxwindforce = t_wind_force_max(t);
	if (t_clouds_min(t) < minclouds)
	  minclouds = t_clouds_min(t);
	if (t_clouds_max(t) > maxclouds)
	  maxclouds = t_clouds_max(t);	
	/* Decide if materials can ever be accumulated in cells. */
	for_all_material_types(m) {
	    if (tm_storage_x(t, m) > 0)
	      any_materials_in_terrain = TRUE;
	}
    }
    if (mintemp != maxtemp)
      any_temp_variation = TRUE;
    if (minwindforce != maxwindforce)
      any_wind_variation = TRUE;
    /* (should figure out what this is supposed to mean really) */
    if (minwindforce != maxwindforce)
      any_wind_variation_in_layer = TRUE;
    if (minclouds != maxclouds)
      any_clouds = TRUE;
}

void
count_terrain_subtypes()
{
    int t;

    numcelltypes = numbordtypes = numconntypes = numcoattypes = 0;
    for_all_terrain_types(t) {
	switch (t_subtype(t)) {
	  case cellsubtype:
	    ++numcelltypes;
	    break;
	  case bordersubtype:
	    ++numbordtypes;
	    break;
	  case connectionsubtype:
	    ++numconntypes;
	    break;
	  case coatingsubtype:
	    ++numcoattypes;
	    break;
	}
    }
}

void
final_init_world()
{
    int x, y, el, count, sum, rowcount, rowsum, rowavg;

    area.minelev = area.maxelev = area.avgelev = 0;
    if (elevations_defined()) {
	area.maxelev = area.minelev = elev_at(area.width / 2, area.height / 2);
	/* In order to avoid overflowing by summing all elevations at
	   once, we average along a row, then average the row averages. */
	count = sum = 0;
	for (y = 0; y < area.height; y++) {
	    rowcount = rowsum = 0;
	    for (x = 0; x < area.width; x++) {
		if (in_area(x, y)) {
		    el = elev_at(x, y);
		    if (el < area.minelev)
		      area.minelev = el;
		    if (el > area.maxelev)
		      area.maxelev = el;
		    ++rowcount;
		    rowsum += el;
		}
	    }
	    rowavg = rowsum / rowcount;
	    ++count;
	    sum += rowavg;
	}
	area.avgelev = sum / count;
    }
    /* Calculate extent and position of geographical features. */
    compute_all_feature_centroids();
}

/* Make space for an area's terrain, and for the unit cache. */

/* This can be postponed until it is actually needed. */

void
allocate_area_terrain()
{
    check_area_shape();
    /* Get rid of old stuff maybe. (is this desirable?) */
    if (area.terrain != NULL) {
	free((char *) area.terrain);
	free((char *) area.units);
    }
    /* Allocate the basic terrain layer. */
    /* It doesn't matter what ttype 0 is, we're guaranteed that it
       will be defined eventually. */
    area.terrain = malloc_area_layer(char);
    /* Allocate and null out the unit cache. */
    area.units = malloc_area_layer(Unit *);
}

/* Set up the auxiliary terrain layer of the area. */

void
allocate_area_aux_terrain(t)
int t;
{
    if (!any_aux_terrain_defined()) {
	area.auxterrain = (char **) xmalloc(numttypes * sizeof(char *));
    }
    if (!aux_terrain_defined(t)) {
	area.auxterrain[t] = malloc_area_layer(char);
    }
}

/* Allocate some number of scratch layers.  These are used as temporaries
   in calculations, etc. */

void
allocate_area_scratch(n)
int n;
{
    check_area_shape();
    if (n >= 1 && !area.tmp1) {
	area.tmp1 = malloc_area_layer(short);
    }
    if (n >= 2 && !area.tmp2) {
	area.tmp2 = malloc_area_layer(short);
    }
    if (n >= 3 && !area.tmp3) {
	area.tmp3 = malloc_area_layer(short);
    }
    if (n >= 4) {
	run_error("can't allocate more than 3 scratch layers");
    }
}

/* Should have something to free scratch layers up also maybe. */

/* Allocate and init the elevation layer. */

void
allocate_area_elevations()
{
    if (!elevations_defined()) {
	check_area_shape();
	area.elevations = malloc_area_layer(short);
    }
}

/* Allocate and init the temperature layer. */

void
allocate_area_temperatures()
{
    if (!temperatures_defined()) {
	check_area_shape();
	area.temperature = malloc_area_layer(short);
    }
    /* We'll need one scratch layer too. */
    allocate_area_scratch(1);
}

/* Allocate a layer indicating the side of the people living in each cell. */

void
allocate_area_people_sides()
{
    if (!people_sides_defined()) {
	check_area_shape();
	area.peopleside = malloc_area_layer(char);
	/* NOBODY != 0, so need to blast it over the layer. */
	memset(area.peopleside, NOBODY, area.width * area.height);
    }
}

/* Set up a cell material layer of the area. */

void
allocate_area_material(m)
int m;
{
    check_area_shape();
    if (!any_cell_materials_defined()) {
	area.materials = (short **) xmalloc(nummtypes * sizeof(short *));
    }
    if (!cell_material_defined(m)) {
	area.materials[m] = malloc_area_layer(short);
    }
}

void
allocate_area_clouds()
{
    if (!clouds_defined()) {
	check_area_shape();
	area.clouds = malloc_area_layer(short);
    }
}

void
allocate_area_cloud_altitudes()
{
    allocate_area_cloud_bottoms();
    allocate_area_cloud_heights();
}

void
allocate_area_cloud_bottoms()
{
    if (!cloud_bottoms_defined()) {
	check_area_shape();
	area.cloudbottoms = malloc_area_layer(short);
    }
}

void
allocate_area_cloud_heights()
{
    if (!cloud_heights_defined()) {
	check_area_shape();
	area.cloudheights = malloc_area_layer(short);
    }
}

void
allocate_area_winds()
{
    if (!winds_defined()) {
	check_area_shape();
	area.winds = malloc_area_layer(short);
    }
}

int
fn_terrain_at(x, y)
int x, y;
{
    return terrain_at(x, y);
}

int
fn_aux_terrain_at(x, y)
int x, y;
{
    return aux_terrain_at(x, y, tmpttype);
}

int
fn_feature_at(x, y)
int x, y;
{
    return raw_feature_at(x, y);
}

int
fn_elevation_at(x, y)
int x, y;
{
    return elev_at(x, y);
}

int
fn_people_side_at(x, y)
int x, y;
{
    return people_side_at(x, y);
}

int
fn_material_at(x, y)
int x, y;
{
    return material_at(x, y, tmpmtype);
}

int
fn_temperature_at(x, y)
int x, y;
{
    return temperature_at(x, y);
}

int
fn_raw_cloud_at(x, y)
int x, y;
{
    return raw_cloud_at(x, y);
}

int
fn_raw_cloud_bottom_at(x, y)
int x, y;
{
    return raw_cloud_bottom_at(x, y);
}

int
fn_raw_cloud_height_at(x, y)
int x, y;
{
    return raw_cloud_height_at(x, y);
}

int
fn_raw_wind_at(x, y)
int x, y;
{
    return raw_wind_at(x, y);
}

void
fn_set_terrain_at(x, y, val)
int x, y, val;
{
    extern warnbadterrain, numbadterrain;

    /* It's important not to put bad values into the terrain layer. */
    if (!is_terrain_type(val)) {
    	/* Only warn the first few times, just count thereafter. */
    	if (warnbadterrain && numbadterrain < 10) {
	    read_warning("Unknown terrain type (%d) at %d,%d; substituting %s",
			 val, x, y, t_type_name(0));
	}
	val = 0;
	++numbadterrain;
    }
    set_terrain_at(x, y, val);
}

void
fn_set_aux_terrain_at(x, y, val)
int x, y, val;
{
    /* Filter anything but the basic six bits. */
    val &= 0x3f;
    set_aux_terrain_at(x, y, tmpttype, val);
}

void
fn_set_people_side_at(x, y, val)
int x, y, val;
{
    set_people_side_at(x, y, val);
}

void
fn_set_raw_feature_at(x, y, val)
int x, y, val;
{
    set_raw_feature_at(x, y, val);
}

void
fn_set_elevation_at(x, y, val)
int x, y, val;
{
    set_elev_at(x, y, val);
}

void
fn_set_material_at(x, y, val)
int x, y, val;
{
    set_material_at(x, y, tmpmtype, val);
}

void
fn_set_temperature_at(x, y, val)
int x, y, val;
{
    set_temperature_at(x, y, val);
}

void
fn_set_raw_wind_at(x, y, val)
int x, y, val;
{
    set_raw_wind_at(x, y, val);
}

void
fn_set_raw_cloud_at(x, y, val)
int x, y, val;
{
    set_raw_cloud_at(x, y, val);
}

void
fn_set_raw_cloud_bottom_at(x, y, val)
int x, y, val;
{
    set_raw_cloud_bottom_at(x, y, val);
}

void
fn_set_raw_cloud_height_at(x, y, val)
int x, y, val;
{
    set_raw_cloud_height_at(x, y, val);
}

/* Change the terrain of the cell at the given location to the given
   type of terrain, and update everything affected by the change. */

void
change_terrain_type(x, y, t2)
int x, y, t2;
{
    int t, curview, newview, update, u2, nu2;
    HistEventType hevttype;
    Unit *unit2;
    Side *side;

    t = terrain_at(x, y);
    set_terrain_at(x, y, t2);
    /* Let everybody see this change. */
    for_all_sides(side) {
	update = FALSE;
	if (side->see_all) {
	    update = TRUE;
	} else if (cover(side, x, y) > 0) {
	    /* Always update our knowledge of the terrain. */
	    curview = terrain_view(side, x, y);
	    newview = buildtview(t2);
	    set_terrain_view(side, x, y, newview);
	    if (!g_see_terrain_always())
	      set_terrain_view_date(side, x, y, g_turn());
	    if (newview != curview)
	      update = TRUE;
	}
	if (update) {
	    update_cell_display(side, x, y, TRUE);
	}
    }
    /* There might be catastrophic consequences for any units here. */
    for_all_stack(x, y, unit2) {
	u2 = unit2->type;
	if (ut_vanishes_on(u2, t2)
	    && !can_occupy_conn(unit2, unit2->x, unit2->y, unit2->z)) {
	    hevttype = H_UNIT_VANISHED;
	    kill_unit(unit2, hevttype);
	} else if (ut_wrecks_on(u2, t2)
		   && !can_occupy_conn(unit2, unit2->x, unit2->y, unit2->z)) {
	    if (u_wrecked_type(u2) == NONUTYPE) {
		/* Occupants always die if the wrecked unit disappears. */
		hevttype = H_UNIT_WRECKED;
		kill_unit(unit2, hevttype);
	    } else {
		/* Change the unit's type. */
		hevttype = H_UNIT_WRECKED;
		change_unit_type(unit2, u_wrecked_type(u2), hevttype);
		nu2 = unit2->type;
		/* Restore to default hp for the new type. */
		unit2->hp = unit2->hp2 = u_hp(nu2);
		/* Get rid of occupants if now overfull. */
		eject_excess_occupants(unit2);
		/* Maybe make it go away, taking remaining unlucky
                   occupants with. */
		if (ut_vanishes_on(nu2, t2)) {
		    hevttype = H_UNIT_VANISHED;
		    kill_unit(unit2, hevttype);
		}
	    }
	}
	if (active_display(unit2->side)) {
	    if (hevttype == H_UNIT_VANISHED) {
		notify(unit2->side, "Terrain change from %s to %s causes %s to vanish",
		       t_type_name(t), t_type_name(t2),
		       unit_handle(unit2->side, unit2));
	    } else if (hevttype == H_UNIT_WRECKED) {
		notify(unit2->side, "Terrain change from %s to %s wrecks %s",
		       t_type_name(t), t_type_name(t2),
		       unit_handle(unit2->side, unit2));
	    }
	}
    }
}

/* Generalized area search routine.  It starts in the immediately adjacent
   cells and expands outwards.  The basic structure is to examine successive
   "rings" out to the max distance; within each ring, we must scan each of
   six faces (picking a random one to start with) by iterating along that
   face, in a direction 120 degrees from the direction out to one corner of
   the face.  Draw a picture if you want to understand it... */

/* Incr is normally one.  It is set to area_size to search on areas
   instead of cells. */

/* Note that points far outside the map may be generated, but the predicate
   will not be called on them.  It may be applied to the same point several
   times, however, if the distance is enough to wrap around the area. */

/* This needs to be changed to understand different world shapes. */

int
search_around(x0, y0, maxdist, pred, rxp, ryp, incr)
int x0, y0, maxdist, (*pred) PARAMS ((int, int)), *rxp, *ryp, incr;
{
    int clockwise, dist, dd, d, dir, x1, y1, i, dir2, x, y, xw;

    maxdist = max(min(maxdist, area.width), min(maxdist, area.height));
    clockwise = (flip_coin() ? 1 : -1);
    for (dist = 1; dist <= maxdist; dist += incr) {
	dd = random_dir();
	for_all_directions(d) {
	    dir = (d + dd) % NUMDIRS;
	    x1 = x0 + dist * dirx[dir];
	    y1 = y0 + dist * diry[dir];
	    for (i = 0; i < dist; ++i) {
		dir2 = opposite_dir(dir + clockwise);
		x = x1 + i * dirx[dir2] * incr;
		y = y1 + i * diry[dir2] * incr;
		xw = wrapx(x);
		if (inside_area(x, y) && (*pred)(xw, y)) {
		    *rxp = xw;  *ryp = y;
		    return TRUE;
		}
	    }
	}
    }
    return FALSE;
}

int
search_and_apply(x0, y0, maxdist, pred, rxp, ryp, incr, fn, num)
int x0, y0, maxdist, *rxp, *ryp, incr, num;
int (*pred) PARAMS ((int, int));
void (*fn) PARAMS ((int, int));
{
    int clockwise, dist, x0w, dd, d, dir, x1, y1, i, dir2, x, y, xw;

    maxdist = max(min(maxdist, area.width), min(maxdist, area.height));
    clockwise = (flip_coin() ? 1 : -1);
    if (maxdist >= 0) {
	x0w = wrapx(x0);
	if (inside_area(x0w, y0) && (*pred)(x0w, y0)) {
	    *rxp = x0w;  *ryp = y0;
	    (*fn)(x0w, y0);
	    if (--num <= 0)
	      return TRUE;
	}
    }
    for (dist = 1; dist <= maxdist; dist += incr) {
	dd = random_dir();
	for_all_directions(d) {
	    dir = (d + dd) % NUMDIRS;
	    x1 = x0 + dist * dirx[dir];
	    y1 = y0 + dist * diry[dir];
	    for (i = 0; i < dist; ++i) {
		dir2 = opposite_dir(dir + clockwise);
		x = x1 + i * dirx[dir2] * incr;
		y = y1 + i * diry[dir2] * incr;
		if (between(0, y, area.height-1)) {
		    xw = wrapx(x);
		    if (inside_area(x, y) && (*pred)(xw, y)) {
			*rxp = xw;  *ryp = y;
			(*fn)(xw, y);
			if (--num <= 0)
			  return TRUE;
		    }
		}
	    }
	}
    }
    return FALSE;
}

/* Apply a function to every cell within the given radius, being careful (for
   both safety and efficiency reasons) not to go past edges.  Note that the
   distance is inclusive, and that distance of 0 means x0,y0 only.  Also,
   if the distance is greater than either map dimension, this routine still
   operates on a correct intersection with the area.  */

int stop_apply;

void
apply_to_area(x0, y0, dist, fn)
int x0, y0, dist;
void (*fn) PARAMS ((int, int));
{
    int x, y, x1, y1, x2, y2;

    dist = min(dist, area.maxdim);
    y1 = y0 - dist;
    y2 = y0 + dist;
    stop_apply = FALSE;
    for (y = y1; y <= y2; ++y) {
	if (between(1, y, area.height-2)) {
	    /* Compute endpoints of row, but don't wrap or loop will confuse */
	    x1 = x0 - (y < y0 ? (y - y1) : dist);
	    x2 = x0 + (y > y0 ? (y2 - y) : dist);
	    for (x = x1; x <= x2; ++x) {
		/* not real efficient, sigh... */
		if (in_area(wrapx(x), y)) {
		    ((*fn)(wrapx(x), y));
		    /* Backdoor for early termination. */
		    if (stop_apply)
		      return;
		}
	    }
	}
    }
}

void
apply_to_area_plus_edge(x0, y0, dist, fn)
int x0, y0, dist;
void (*fn) PARAMS ((int, int));
{
    int x, y, x1, y1, x2, y2;

    dist = min(dist, area.maxdim);
    y1 = y0 - dist;
    y2 = y0 + dist;
    for (y = y1; y <= y2; ++y) {
	if (between(0, y, area.height-1)) {
	    /* Compute endpoints of row, but don't wrap or loop will confuse */
	    x1 = x0 - (y < y0 ? (y - y1) : dist);
	    x2 = x0 + (y > y0 ? (y2 - y) : dist);
	    for (x = x1; x <= x2; ++x) {
		/* not real efficient, sigh... */
		if (in_area(wrapx(x), y)) {
		    ((*fn)(wrapx(x), y));
		}
	    }
	}
    }
}

void
apply_to_ring(x0, y0, distmin, distmax, fn)
int x0, y0, distmin, distmax;
void (*fn) PARAMS ((int, int));
{
    int dist, x, y, x1, y1, x2, y2;

    dist = min(distmax, area.maxdim);
    y1 = y0 - dist;
    y2 = y0 + dist;
    for (y = y1; y <= y2; ++y) {
	if (between(1, y, area.height-2)) {
	    /* Compute endpoints of row, but don't wrap or loop will confuse */
	    x1 = x0 - (y < y0 ? (y - y1) : dist);
	    x2 = x0 + (y > y0 ? (y2 - y) : dist);
	    for (x = x1; x <= x2; ++x) {
		/* not real efficient, sigh... */
		if (in_area(wrapx(x), y) && distance(x, y, x0, y0) >= distmin) {
		    ((*fn)(wrapx(x), y));
		}
	    }
	}
    }
}

/* Apply the function to the hexagon bounded by w,h. */

void
apply_to_hexagon(x0, y0, w2, h2, fn)
int x0, y0, w2, h2;
void (*fn) PARAMS ((int, int));
{
    int x, y, x1, y1, x2, y2;

    y1 = limit(y0 - h2);
    y2 = limit(y0 + h2);
    for (y = y1; y <= y2; ++y) {
	if (between(0, y, area.height-1)) {  /* always true? */
	    /* Compute endpoints of row, but don't wrap or loop will confuse */
	    x1 = x0 - w2 + (y < y0 ? (y0 - y) : 0);
	    x2 = x0 + w2 - (y > y0 ? (y - y0) : 0);
	    for (x = x1; x <= x2; ++x) {
		/* not real efficient, sigh... */
		if (in_area(wrapx(x), y)) {
		    ((*fn)(wrapx(x), y));
		}
	    }
	}
    }
}

/* Apply a function all along a path. */

void
apply_to_path(fx, fy, tx, ty, dirtest, dirsort, fn, shortest)
int fx, fy, tx, ty, shortest;
int (*dirtest) PARAMS ((int x, int y, int dir));
int (*dirsort) PARAMS ((int x, int y, int *dirchoices, int numchoices));
int (*fn) PARAMS ((int x, int y, int dir, int j, int numchoices));
{
    int i = 500, j, x = fx, y = fy, moved;
    int dx, dxa, dy, dirchoices[NUMDIRS], axis, hextant, tmp, sig;
    int d1, d2, d3, d4;
    int numchoices, shortestnumchoices;

    while (!(x == tx && y == ty) && i-- > 0 /* safety */) {
	dx = tx - x;  dy = ty - y;
	/* If in a wrapping world, choose the shortest of directions. */
	if (area.xwrap) {
	    dxa = (tx + area.width) - fx;
	    if (ABS(dx) > ABS(dxa))
	      dx = dxa;
	    dxa = (tx - area.width) - fx;
	    if (ABS(dx) > ABS(dxa))
	      dx = dxa;
	}
	/* Figure out the axis or hextant of this delta. */
	axis = hextant = -1;
	/* Decode the delta values. */
	if (dx == 0) {
	    axis = (dy > 0 ? NORTHEAST : SOUTHWEST);
	} else if (dy == 0) {
	    axis = (dx > 0 ? EAST : WEST);
	} else if (dx == (0 - dy)) {
	    axis = (dy > 0 ? NORTHWEST : SOUTHEAST);
	} else if (dx > 0) {
	    hextant = (dy > 0 ? EAST :
		       (ABS(dx) > ABS(dy) ? SOUTHEAST : SOUTHWEST));
	} else {
	    hextant = (dy < 0 ? WEST :
		       (ABS(dx) > ABS(dy) ? NORTHWEST : NORTHEAST));
	}
	numchoices = 0;
	/* On an axis, there's no choice. */
	if (axis >= 0) {
	    d1 = d2 = axis;
	    if (dirtest == NULL || (*dirtest)(x, y, d1))
	      dirchoices[numchoices++] = axis;
	}
	/* Two choices in the middle of a hextant. */
	if (hextant >= 0) {
	    d1 = left_dir(hextant);
	    d2 = hextant;
	    if (dirtest == NULL || (*dirtest)(x, y, d1))
	      dirchoices[numchoices++] = d1;
	    if (dirtest == NULL || (*dirtest)(x, y, d2))
	      dirchoices[numchoices++] = d2;
	}
	/* Sort the choices if requested. */
	if (numchoices > 1 && dirsort != NULL)
	  numchoices = (*dirsort)(x, y, dirchoices, numchoices);
	/* Try each of the directions. */
	if (!inside_area(x, y))
	  return;
	moved = FALSE;
	for (j = 0; j < numchoices; ++j) {
	    sig = (*fn)(x, y, dirchoices[j], j, numchoices);
	    if (sig > 0) {
		/* It's cool - go with this dir. */
		/* Jump along to the new spot on the path. */
		x += dirx[dirchoices[j]];  y += diry[dirchoices[j]];
		moved = TRUE;
		/* Out of the loop. */
		break;
	    } else if (sig < 0) {
		/* Stop applying to the path right now. */
		return;
	    } else {
		/* Try another. */
	    }
	}
	/* If we don't have to pick a shortest path, we have two more
	   directions to try if we need to. */
	if (!moved && !shortest) {
	    shortestnumchoices = numchoices;
	    d3 = left_dir(d1);
	    d4 = right_dir(d2);
	    if (dirtest == NULL || (*dirtest)(x, y, d3))
	      dirchoices[numchoices++] = d3;
	    if (dirtest == NULL || (*dirtest)(x, y, d4))
	      dirchoices[numchoices++] = d4;
	    if (numchoices > shortestnumchoices + 1 && dirsort != NULL)
	      (*dirsort)(x, y, dirchoices + shortestnumchoices, numchoices - shortestnumchoices);
	    /* Try each of the directions. */
	    for (j = shortestnumchoices; j < numchoices; ++j) {
		sig = (*fn)(x, y, dirchoices[j], j, numchoices - shortestnumchoices);
		if (sig > 0) {
		    /* It's cool - go with this dir. */
		    /* Jump along to the new spot on the path. */
		    x += dirx[dirchoices[j]];  y += diry[dirchoices[j]];
		    /* Out of the loop. */
		    break;
		} else if (sig < 0) {
		    /* Stop applying to the path right now. */
		    return;
		} else {
		    /* Try another. */
		}
	    }
	}
    }
}

#if 0 /* currently unused */
/* Find a path between the two given points. */

/* The chooser function gets passed an small array for directions;
   it is expected to fill it with directions sorted in order of
   preference, and to return the number of directions it found. */

/* (the chooser also needs to respect already-marked cells) */
/* (marking should account for all directions in?) */
/* (should return a "figure of merit" sometimes) */
/* (main prog should test vicinity of dest, might not be reachable
   anyway, but maybe should have "reach within n cells") */

int
find_path(fx, fy, tx, ty, chooser, maxwps, waypoints, numwpsp)
int fx, fy, tx, ty, maxwps, *numwpsp;
int (*chooser) PARAMS ((int, int, int, int, int *));
Waypoint *waypoints;
{
    int ndirs, trythese[NUMDIRS], i;
    int x1, y1, x2, y2;

    if (fx == fy && tx == ty) {
	return TRUE;
    }
    ndirs = (*chooser)(x1, y1, x2, y2, trythese);
    if (ndirs == 0) {
	/* We're totally blocked. */
	return FALSE;
    } else {
	for (i = 0; i < ndirs; ++i) {
	    /* try this direction with find_path_aux */
	}
    }
    return FALSE;
}
#endif

#if 0	    
/* Find the type of the border on the given side of the given hex. */

int
border_at(x, y, dir, t)
int x, y, dir, t;
{
    int bord;

    if (!inside_area(x, y)
	|| !t_is_border(t)
	|| !aux_terrain_defined(t))
      return FALSE;
    bord = aux_terrain_at(x, y, t);
    return (bord & (1 << dir));
}
#endif

/* For now, set a bit on both sides of a border. */

void
set_border_at(x, y, dir, t, onoff)
int x, y, dir, t, onoff;
{
    int ox, oy, bord, obord;
    int odir = opposite_dir(dir);

    if (!t_is_border(t))
      return;
    if (!point_in_dir(x, y, dir, &ox, &oy)) {
	run_warning("border on outside of world at %d,%d, can't set", x, y);
	return;
    }
    allocate_area_aux_terrain(t);
    onoff = (onoff ? 1 : 0);  /* make sure it's one bit */
    bord = aux_terrain_at(x, y, t);
    bord = ((onoff << dir) | (bord & ~(1 << dir)));
    set_aux_terrain_at(x, y, t, bord);
    /* Go to the other cell and tweak its border bits. */
    obord = aux_terrain_at(ox, oy, t);
    obord = ((onoff << odir) | (obord & ~(1 << odir)));
    set_aux_terrain_at(ox, oy, t, obord);
}

#if 0
/* Find the type of the connection on the given side of the given cell. */

int
connection_at(x, y, dir, t)
int x, y, dir, t;
{
    int conn;

    if (!inside_area(x, y)
	|| !t_is_connection(t)
	|| !aux_terrain_defined(t))
      return FALSE;
    conn = aux_terrain_at(x, y, t);
    return (conn & (1 << dir));
}
#endif

/* For now, set a bit on both sides of a connection. */

void
set_connection_at(x, y, dir, t, onoff)
int x, y, dir, t, onoff;
{
    int ox, oy, conn, oconn;
    int odir = opposite_dir(dir);

    if (!t_is_connection(t))
      return;
    if (!point_in_dir(x, y, dir, &ox, &oy)) {
	run_warning("connection to outside of world at %d,%d, can't set", x, y);
	return;
    }
    allocate_area_aux_terrain(t);
    onoff = (onoff ? 1 : 0);  /* make sure it's one bit */
    conn = aux_terrain_at(x, y, t);
    conn = ((onoff << dir) | (conn & ~(1 << dir)));
    set_aux_terrain_at(x, y, t, conn);
    /* Go to the other cell and tweak its connection bits. */
    oconn = aux_terrain_at(ox, oy, t);
    oconn = ((onoff << odir) | (oconn & ~(1 << odir)));
    set_aux_terrain_at(ox, oy, t, oconn);
}

/* If there might be any inconsistencies in borders or connections,
   this fixes them.  Basically this just detects if a bit is set on
   either side, and sets the bits on both sides if so. */

void
patch_linear_terrain(t)
int t;
{
    int x, y, dir, x1, y1;
	
    if (t_is_border(t)) {
	for_all_cells(x, y) {
	    /* This test is a hack to save some time.  If a cell has no border
	       flags in any direction, then either it has no borders or else it
	       will be fixed up later on, when an adjacent cell is patched. */
	    if (aux_terrain_at(x, y, t) != 0) {
		for_all_directions(dir) {
		    if (border_at(x, y, dir, t)
			&& point_in_dir(x, y, dir, &x1, &y1)
			&& !border_at(x1, y1, opposite_dir(dir), t))
		      set_border_at(x, y, dir, t, TRUE);
		}
	    }
	}
    } else if (t_is_connection(t)) {
	for_all_cells(x, y) {
	    if (aux_terrain_at(x, y, t) != 0) {
		for_all_directions(dir) {
		    if (connection_at(x, y, dir, t)
			&& point_in_dir(x, y, dir, &x1, &y1)
			&& !connection_at(x1, y1, opposite_dir(dir), t))
		      set_connection_at(x, y, dir, t, TRUE);
		}
	    }
	}
    }
}

/* Make space to record named features. */

void
init_features()
{
    if (features_defined())
      return;
    featurelist = last_feature = NULL;
    area.features = malloc_area_layer(short);
    /* No need to fill layer with default value, as long as
       feature 0 means "no feature". */
}

Feature *
create_feature(typename, name)
char *typename, *name;
{
    Feature *newfeature = (Feature *) xmalloc(sizeof(Feature));

    newfeature->id = nextfid++;
    newfeature->typename = typename;
    newfeature->name = name;
    /* Add to the end of the feature list. */
    if (last_feature != NULL) {
    	last_feature->next = newfeature;
    } else {
    	featurelist = newfeature;
    }
    last_feature = newfeature;
    return newfeature;
}

Feature *
find_feature(fid)
int fid;
{
    Feature *feature;

    for (feature = featurelist; feature != NULL; feature = feature->next) {
	if (feature->id == fid)
	  return feature;
    }
    return NULL;
}

Feature *
feature_at(x, y)
int x, y;
{
    int fid;

    if (!features_defined())
      return NULL;

    fid = raw_feature_at(x, y);
    if (fid == 0) {
	return NULL;
    } else {
	return find_feature(fid);
    }
}

void
set_feature_type_name(feature, typename)
Feature *feature;
char *typename;
{
    if (feature == NULL)
      return;
    feature->typename = copy_string(typename);
    /* (should ping all displays) */
}

void
set_feature_name(feature, name)
Feature *feature;
char *name;
{
    if (feature == NULL)
      return;
    feature->name = copy_string(name);
    /* (should ping all displays) */
}

void
destroy_feature(feature)
Feature *feature;
{
    Feature *tmp, *prev = NULL;

    if (feature == NULL)
      return; /* (should be error?) */
    if (feature == featurelist)
      featurelist = feature->next;
    else {
	for (tmp = featurelist; tmp != NULL; tmp = tmp->next) {
    	    if (tmp == feature) {
		if (prev != NULL)
		  prev->next = tmp->next;
		break;
	    }
	}
	prev = tmp;
    }
    /* (should dealloc also?) */
}

/* (should be able to call this from designer tools) */

void
renumber_features()
{
    int newfid = 1, maxoldfid = 0, x, y;
    short *newlabels;
    Feature *feature;

    if (!features_defined())
      return;
    for (feature = featurelist; feature != NULL; feature = feature->next) {
    	maxoldfid = max(maxoldfid, feature->id);
    	feature->relabel = newfid++;
    }
    if (maxoldfid > 1000)
      return; /* don't risk it */
    newlabels = (short *) xmalloc((maxoldfid + 1) * sizeof(short));
    for (feature = featurelist; feature != NULL; feature = feature->next) {
    	newlabels[feature->id] = feature->relabel;
    }
    for_all_cells(x, y) {
    	set_raw_feature_at(x, y, newlabels[raw_feature_at(x, y)]);
    }
    for (feature = featurelist; feature != NULL; feature = feature->next) {
    	feature->id = feature->relabel;
    }
}

void
compute_all_feature_centroids()
{
    int x, y;
    Feature *feature;

    /* Only do this if features and a feature layer to work with. */
    if (featurelist == NULL)
      return;
    if (!features_defined())
      return;
    /* Clear out the features. */
    for (feature = featurelist; feature != NULL; feature = feature->next) {
	feature->size = 0;
	feature->x = feature->y = 0;
    }
    for_all_cells(x, y) {
	feature = feature_at(x, y);
	if (feature != NULL) {
	    ++(feature->size);
	    feature->x += x;  feature->y += y;
	}
    }
    for (feature = featurelist; feature != NULL; feature = feature->next) {
	if (feature->size > 0) {
	    feature->x = feature->x / feature->size;
	    feature->y = feature->y / feature->size;
	}
    }
}

void
compute_feature_centroid(feature)
Feature *feature;
{
    int x, y, fid;

    feature->size = 0;
    feature->x = feature->y = 0;
    for_all_cells(x, y) {
	fid = raw_feature_at(x, y);
	if (feature->id == fid) {
	    feature->x += x;  feature->y += y;
	}
    }
    if (feature->size > 0) {
	feature->x = feature->x / feature->size;
	feature->y = feature->y / feature->size;
    }
}

int 
num_features()
{
    int f = 0, x, y;

    if (!features_defined())
      return 0;

    for_all_cells(x, y) {
	f = max(f, raw_feature_at(x, y));
    }
    return f;
}

/* Compute the coords of a point in the given direction. */

int
point_in_dir(x, y, dir, xp, yp)
int x, y, dir, *xp, *yp;
{
    *xp = wrapx(x + dirx[dir]);  *yp = y + diry[dir];
    return (in_area(*xp, *yp));
}

int
interior_point_in_dir(x, y, dir, xp, yp)
int x, y, dir, *xp, *yp;
{
    *xp = wrapx(x + dirx[dir]);  *yp = y + diry[dir];
    return (inside_area(*xp, *yp));
}

int
point_in_dir_n(x, y, dir, n, xp, yp)
int x, y, dir, n, *xp, *yp;
{
    *xp = wrapx(x + n * dirx[dir]);  *yp = y + n * diry[dir];
    return (in_area(*xp, *yp));
}

int
interior_point_in_dir_n(x, y, dir, n, xp, yp)
int x, y, dir, n, *xp, *yp;
{
    *xp = wrapx(x + n * dirx[dir]);  *yp = y + n * diry[dir];
    return (inside_area(*xp, *yp));
}

/* Return a random point guaranteed inside the area. */

int
random_point(xp, yp)
int *xp, *yp;
{
    int tries = 500;

    while (tries-- > 0) {
	*xp = xrandom(area.width);  *yp = xrandom(area.height - 2) + 1;
	if (inside_area(*xp, *yp))
	  return TRUE;
    }
    return FALSE;
}

/* Return a random point guaranteed on the edge of the area. */

int
random_edge_point(xp, yp)
int *xp, *yp;
{
    int tries = 500, ratio, val;

    while (tries-- > 0) {
	if (area.xwrap) {
	    *xp = xrandom(area.width);
	    *yp = (flip_coin() ? 0 : area.height - 1);
	} else {
	    ratio = ((area.width - area.halfheight) * 100) / (area.width + area.halfheight);
	    if (probability(ratio)) {
		/* Pick along top or bottom edge. */
		if (flip_coin()) {
		    *xp = xrandom(area.width - area.halfheight) + area.halfheight;
		    *yp = 0;
		} else {
		    *xp = xrandom(area.width - area.halfheight);
		    *yp = area.height - 1;
		}
	    } else {
		/* Pick along right or left side. */
		if (flip_coin()) {
		    /* Pick along right side. */
		    if (flip_coin()) {
			/* Pick along upper right side. */
			val = xrandom(area.halfheight);
			*xp = area.width - val;
			*yp = area.halfheight + val;
		    } else {
			/* Pick along lower right side. */
			*xp = area.width;
			*yp = xrandom(area.halfheight);
		    }
		} else {
		    /* Pick along left side. */
		    if (flip_coin()) {
			/* Pick along upper left side. */
			*xp = 0;
			*yp = area.halfheight + xrandom(area.halfheight);
		    } else {
			/* Pick along lower left side. */
			val = xrandom(area.halfheight);
			*xp = val;
			*yp = area.halfheight - val;
		    }
		}
	    }
	}
	if (in_area(*xp, *yp) && !inside_area(*xp, *yp))
	  return TRUE;
    }
    return FALSE;
}

/* Return a random point guaranteed to be within a given radius of
   a given point. */

int
random_point_near(cx, cy, radius, xp, yp)
int cx, cy, radius, *xp, *yp;
{
    int tries = 500;

    if (radius <= 0)
      return FALSE;
    while (tries-- > 0) {
	*xp = cx + xrandom(2 * radius + 1) - radius;
	*yp = cy + xrandom(2 * radius + 1) - radius;
	if (inside_area(*xp, *yp)
	    && distance(cx, cy, *xp, *yp) <= radius)
	      return TRUE;
    }
    return FALSE;
}

/* Return a random point guaranteed to be within a given radius of
   a given point. */

int
random_point_in_area(cx, cy, rx, ry, xp, yp)
int cx, cy, rx, ry, *xp, *yp;
{
    int tries = 500;

    while (tries-- > 0) {
	*xp = cx + xrandom(2 * rx + 1) - rx;
	*yp = cy + xrandom(2 * ry + 1) - ry;
	if (inside_area(*xp, *yp)
	    && distance(cx, cy, *xp, *yp) <= max(rx - ry, ry - rx))
	  return TRUE;  /* (should fix test?) */
    }
    return FALSE;
}

/* Generic warning that a terrain subtype is incorrect. */

void
terrain_subtype_warning(context, t)
char *context;
int t;
{
    run_warning("In %s: Garbage t%d (%s) subtype %d",
		context, t, t_type_name(t), t_subtype(t));
}

/* Given a vector, return the direction that best approximates it. */

int
approx_dir(dx, dy)
int dx, dy;
{
    if (dx == 0) {
	if (dy == 0)
	  return -1; /* should flag so can use special cursor */
	if (dy > 0)
	  return NORTHEAST;
	return SOUTHWEST;
    } else if (dx > 0) {
    	/* Check for the axes first. */
	if (dy == 0)
	  return EAST;
	if (dy == (-dx))
	  return SOUTHEAST;
	if (dy > dx)
	  return NORTHEAST;
	if ((-dy) <= dx / 2)
	  return EAST;
	if ((-dy) < dx * 2)
	  return SOUTHEAST;
	return SOUTHWEST;
    } else {
    	/* Check for the axes first. */
	if (dy == 0)
	  return WEST;
    	if (dy == (-dx))
    	  return NORTHWEST;
	if (dy > (-dx) * 2)
	  return NORTHEAST;
	if (dy >= (-dx) / 2)
	  return NORTHWEST;
	if (dy > dx)
	  return WEST;
	return SOUTHWEST;
    }
}

/* Computing distance in a hexagonal system is a little peculiar, since it's
   sometimes just delta x or y, and other times is the sum.  Basically there
   are six formulas to compute distance, depending on the direction between
   the two points.  If the area wraps, this routine reports the shortest
   distance. */

int
distance(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    int dx = x2 - x1, dy = y2 - y1;

    if (area.xwrap) {
    	/* Choose the shortest way around a cylinder. */
    	dx = (dx < 0 ? (dx < 0 - area.width / 2 ? area.width + dx : dx)
	        	 : (dx > area.width / 2 ? dx - area.width : dx));
    }
    if (dx >= 0) {
	if (dy >= 0) {
	    return (dx + dy);
	} else if ((0 - dy) <= dx) {
	    return dx;
	} else {
	    return (0 - dy);
	}
    } else {
	if (dy <= 0) {
	    return (0 - (dx + dy));
	} else if (dy <= (0 - dx)) {
	    return (0 - dx);
	} else {
	    return dy;
	}
    }
}

int
world_distance(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    int dx = x2 - x1, dy = y2 - y1;

    /* Choose the shortest way around the world. */
    dx = (dx < 0 ? (dx < 0 - world.circumference / 2 ? world.circumference + dx : dx)
	         : (dx > world.circumference / 2 ? dx - world.circumference : dx));
    if (dx >= 0) {
	if (dy >= 0) {
	    return (dx + dy);
	} else if ((0 - dy) <= dx) {
	    return dx;
	} else {
	    return (0 - dy);
	}
    } else {
	if (dy <= 0) {
	    return (0 - (dx + dy));
	} else if (dy <= (0 - dx)) {
	    return (0 - dx);
	} else {
	    return dy;
	}
    }
}

/* Find the direction matching the given x and y, return -1 if no
   match.  Callers should be careful to test for this! */

int
closest_dir(x, y)
int x, y;
{
    int dir;

    for_all_directions(dir) {
	if (dirx[dir] == x && diry[dir] == y)
	  return dir;
    }
    return -1;
}

/* should put date-handling code here? */

#ifdef DESIGNERS

/* Cell painting. */

static void
paint_cell_1(x, y)
int x, y;
{
    /* Only do anything if we're actually changing to a different type. */
    if (terrain_at(x, y) != tmpttype) {
	set_terrain_at(x, y, tmpttype);
	see_exact(tmpside, x, y);
    }
}

void
paint_cell(side, x, y, r, t)
Side *side;
int x, y, r, t;
{
    tmpside = side;
    tmpttype = t;
    apply_to_area_plus_edge(x, y, r, paint_cell_1);
}

void
paint_border(side, x, y, dir, t, mode)
Side *side;
int x, y, dir, t, mode;
{
    int oldbord;

    if (!inside_area(x, y))
      return;
    allocate_area_aux_terrain(t);
    oldbord = border_at(x, y, dir, t);
    set_border_at(x, y, dir, t, (mode < 0 ? !oldbord : mode));
    if (oldbord != border_at(x, y, dir, t)) {
        /* We do both a see_exact and update because see_exact doesn't
           account for linear terrain changes. */
	see_exact(side, x, y);
	update_cell_display(side, x, y, TRUE);
	see_exact(side, x+dirx[dir], y+diry[dir]);
	update_cell_display(side, x+dirx[dir], y+diry[dir], TRUE);
    }
}

void
paint_connection(side, x, y, dir, t, mode)
Side *side;
int x, y, dir, t, mode;
{
    int oldconn;

    if (!inside_area(x, y))
      return;
    allocate_area_aux_terrain(t);
    oldconn = connection_at(x, y, dir, t);
    set_connection_at(x, y, dir, t, (mode < 0 ? !oldconn : mode));
    if (oldconn != connection_at(x, y, dir, t)) {
        /* We do both a see_exact and update because see_exact doesn't
           account for linear terrain changes. */
	see_exact(side, x, y);
	update_cell_display(side, x, y, TRUE);
	see_exact(side, x+dirx[dir], y+diry[dir]);
	update_cell_display(side, x+dirx[dir], y+diry[dir], TRUE);
    }
}

/* Coating painting. */

static void
paint_coating_1(x, y)
int x, y;
{
    int olddepth = aux_terrain_at(x, y, tmpttype);

    if (olddepth != tmpint) {
	set_aux_terrain_at(x, y, tmpttype, tmpint);
	see_exact(tmpside, x, y);
    }
}

void
paint_coating(side, x, y, r, t, depth)
Side *side;
int x, y, r, t, depth;
{
    allocate_area_aux_terrain(t);
    tmpside = side;
    tmpttype = t;
    tmpint = depth;
    apply_to_area_plus_edge(x, y, r, paint_coating_1);
}

/* Painting of people sides. */

static void
paint_people_1(x, y)
int x, y;
{
    int oldpeop = people_side_at(x, y);

    if (oldpeop != tmpint) {
	set_people_side_at(x, y, tmpint);
	see_exact(tmpside, x, y);
    }
}

void
paint_people(side, x, y, r, s)
Side *side;
int x, y, r, s;
{
    allocate_area_people_sides();
    tmpside = side;
    tmpint = s;
    apply_to_area(x, y, r, paint_people_1);
}

/* Painting of geographical features. */

static void
paint_feature_1(x, y)
int x, y;
{
    int oldfid = raw_feature_at(x, y);
    Feature *oldfeature;

    if (oldfid != tmpint) {
	set_raw_feature_at(x, y, tmpint);
	++(tmpfeature->size);
	see_exact(tmpside, x, y);
    	if (oldfid != 0) {
	    oldfeature = find_feature(oldfid);
	    if (oldfeature != NULL)
	      --(oldfeature->size);
	}
    }
}

void
paint_feature(side, x, y, r, f)
Side *side;
int x, y, r, f;
{
    Feature *newfeature;

    init_features();
    newfeature = find_feature(f);
    if (newfeature != NULL) {
	tmpside = side;
	tmpfeature = newfeature;
	tmpint = f;
	apply_to_area(x, y, r, paint_feature_1);
    }
}

/* Painting of terrain elevations. */

static void
paint_elev_1(x, y)
int x, y;
{
    int n, t = terrain_at(x, y), oldelev = elev_at(x, y);

    /* Clip desired elevation to what's allowed for the terrain here. */
    n = max(t_elev_min(t), min(tmpint, t_elev_max(t)));
    if (n != oldelev) {
	set_elev_at(x, y, n);
	see_exact(tmpside, x, y);
    }
}

void
paint_elevation(side, x, y, r, elev)
Side *side;
int x, y, r, elev;
{
    allocate_area_elevations();
    tmpside = side;
    tmpint = elev;
    apply_to_area_plus_edge(x, y, r, paint_elev_1);
}

static void
paint_temp_1(x, y)
int x, y;
{
    int n, t = terrain_at(x, y), oldtemp = temperature_at(x, y);
    
    n = max(t_temp_min(t), min(tmpint, t_temp_max(t)));
    if (n != oldtemp) {
	set_temperature_at(x, y, n);
	see_exact(tmpside, x, y);
    }
}

void
paint_temperature(side, x, y, r, temp)
Side *side;
int x, y, r, temp;
{
    allocate_area_temperatures();
    tmpside = side;
    tmpint = temp;
    apply_to_area_plus_edge(x, y, r, paint_temp_1);
}

static void
paint_material_1(x, y)
int x, y;
{
    int oldm = material_at(x, y, tmpmtype);

    if (oldm != tmpint) {
	set_material_at(x, y, tmpmtype, tmpint);
	see_exact(tmpside, x, y);
    }
}

void
paint_material(side, x, y, r, m, amt)
Side *side;
int x, y, r, m, amt;
{
    allocate_area_material(m);
    tmpside = side;
    tmpmtype = m;
    tmpint = amt;
    apply_to_area_plus_edge(x, y, r, paint_material_1);
}

/* Cloud painting is more complicated because up to three separate
   layers are involved. */

static void
paint_clouds_1(x, y)
int x, y;
{
    int oldcl = raw_cloud_at(x, y);
    int oldbot = raw_cloud_bottom_at(x, y);
    int oldhgt = raw_cloud_height_at(x, y);
    int changed = FALSE;

    if (oldcl != tmpint) {
	set_raw_cloud_at(x, y, tmpint);
	changed = TRUE;
    }
    if (oldbot != tmpint2) {
	set_raw_cloud_bottom_at(x, y, tmpint2);
	changed = TRUE;
    }
    if (oldhgt != tmpint3) {
	set_raw_cloud_height_at(x, y, tmpint3);
	changed = TRUE;
    }
    if (changed)
      see_exact(tmpside, x, y);
}

void
paint_clouds(side, x, y, r, cloudtype, bot, hgt)
Side *side;
int x, y, r, cloudtype, bot, hgt;
{
    allocate_area_clouds();
    /* (should not always do altitudes) */
    allocate_area_cloud_altitudes();
    tmpside = side;
    tmpint = cloudtype;
    tmpint2 = bot;
    tmpint3 = hgt;
    apply_to_area_plus_edge(x, y, r, paint_clouds_1);
}

static void
paint_winds_1(x, y)
int x, y;
{
    int oldw = raw_wind_at(x, y);

    if (oldw != tmpint) {
	set_raw_wind_at(x, y, tmpint);
	see_exact(tmpside, x, y);
    }
}

void
paint_winds(side, x, y, r, dir, force)
Side *side;
int x, y, r, dir, force;
{
    allocate_area_winds();
    tmpside = side;
    tmpint = force << 3 | dir;
    apply_to_area_plus_edge(x, y, r, paint_winds_1);
}

#endif /* DESIGNERS */
