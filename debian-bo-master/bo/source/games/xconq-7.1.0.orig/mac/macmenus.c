/* Menus and commands for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#include "conq.h"
#include "print.h"
#include "macconq.h"
extern void enable_command(void);
extern TEHandle command_text;
extern TEHandle run_length_text;

void
query_position_modally(int mode)

{
	if (map_modal == NO_MODAL) {
		map_modal = mode;
	} else {
		beep();
	}
}

void
set_position_modally()
{
	modal_map = map_from_window(FrontWindow());
	GetMouse(&modal_point);
	position_set_modally = TRUE;
}

#undef DEF_CMD
#define DEF_CMD(letter,name,args,FN,help) extern void FN(void);

#include "cmd.def"

#include "maccmd.def"

extern int do_one_give(Unit *unit);
extern int do_one_take(Unit *unit);
extern int do_one_return(Unit *unit);
extern int do_one_detonate(Unit *unit);
extern int do_one_repair(Unit *unit);
extern int do_one_give_unit(Unit *unit);
extern int do_one_detach(Unit *unit);
extern int do_one_disband(Unit *unit);

extern int do_one_clear_plan(Unit *unit);
extern int do_one_dir_move(Unit *unit);
extern int do_one_dir_multiple_move(Unit *unit);
extern int do_one_set_name(Unit *unit);

static void sanitize_for_menu(char *str, Str255 outstr);

static void resign_the_game(int forced);

static void te_cut(TEHandle hdl);
static void te_copy(TEHandle hdl);
static void te_paste(TEHandle hdl);

static void do_page_setup_mi(void);
static void do_print_mi(void);
static void do_select_all_mi(void);
static void do_find_previous_mi(void);
static void do_find_next_mi(void);
static void do_find_location_mi(void);
static void do_find_unit_by_name_mi(void);
static void do_find_selected_mi(void);
static void do_closeup_mi(void);
static void do_move_mi(void);
static void do_patrol_mi(void);
static void do_wake_mi(void);
static void do_sleep_mi(void);
static void do_done_moving_mi(void);
static void do_construction_mi(void);
static void do_repair_mi(void);
static void do_attack_mi(void);
static void do_overrun_mi(void);
static void do_fire_mi(void);
static void do_fire_into_mi(void);
static void do_take_mi(void);
static void do_detonate_mi(void);
static void do_give_unit_mi(int mi);
static void do_disband_mi(void);
static void moveonclick_mi(void);
static void autoselect_mi(void);

static void adjust_menu_item(MenuHandle menu, int item, int value);
extern void enable_commands_for_unit(MenuHandle menu, Unit *unit);

int forcedtoresign;

int modal_construction = FALSE;

WindowPtr window_behind_construction;

extern Feature *featurelist;

#ifdef THINK_C
#include <profile.h>
#endif

int Profile = FALSE;

#ifdef DESIGNERS
#define side_may_select(unit) (in_play(unit) && ((unit)->side == dside || dside->designer))

#define valid_selection(unit) (in_play(unit) && ((unit)->side == dside || dside->designer))

#define menus_tweakable() ((!beforestart && !endofgame) || (gameinited && dside->designer))
#else /* n DESIGNERS */
#define side_may_select(unit) (in_play(unit) && ((unit)->side == dside))

#define valid_selection(unit) (in_play(unit) && ((unit)->side == dside))

#define menus_tweakable() ((!beforestart && !endofgame) || (gameinited))
#endif /* DESIGNERS */

#define MAXWINDOWS 100

char *cursavename = NULL;

MenuHandle mapviewmenu = nil;
MenuHandle listviewmenu = nil;
MenuHandle closeupviewmenu = nil;

MenuHandle utypemenu = nil;
MenuHandle ttypemenu = nil;
MenuHandle mtypemenu = nil;
MenuHandle sidemenu = nil;
MenuHandle aitypemenu = nil;

MenuHandle featuremenu = nil;
MenuHandle optterrmenu = nil;

WindowPtr *winmenuwins;

int numwindows;

Map *worldmap = NULL;

WindowPtr worldmapwin = nil;

/* Set up all the menus, at least in their basic form. */

void
init_menus()
{
	int i;
	Handle menubar;
	MenuHandle menu;

	menubar = GetNewMBar(mbMain);
	SetMenuBar(menubar);
	/* Add the DAs etc as usual. */
	menu = GetMHandle(mApple);
	if (menu != nil)
	  AddResMenu(menu, 'DRVR');
	/* Get the different versions of the view menu. */
	mapviewmenu = GetMenu(mViewMap);
	listviewmenu = GetMenu(mViewList);
	closeupviewmenu = GetMenu(mViewCloseup);
	/* Init submenus and popups. */
	menu = GetMenu(mViewFontSizes);
	if (menu != nil)
	  InsertMenu(menu, -1);
	menu = GetMenu(mMagnifications);
	if (menu != nil)
	  InsertMenu(menu, -1);
	menu = GetMenu(mViewWeather);
	if (menu != nil)
	  InsertMenu(menu, -1);
	sidemenu = GetMenu(mSides);
	if (sidemenu != nil)
	  InsertMenu(sidemenu, -1);
	utypemenu = GetMenu(mUnitTypes);
	if (utypemenu != nil)
	  InsertMenu(utypemenu, -1);
	mtypemenu = GetMenu(mMaterialTypes);
	if (mtypemenu != nil)
	  InsertMenu(mtypemenu, -1);
	ttypemenu = GetMenu(mTerrainTypes);
	if (ttypemenu != nil)
	  InsertMenu(ttypemenu, -1);
	aitypemenu = GetMenu(mAITypes);
	if (aitypemenu != nil)
	  InsertMenu(aitypemenu, -1);
	featuremenu = GetMenu(mFeatures);
	if (featuremenu != nil)
	  InsertMenu(featuremenu, -1);
	optterrmenu = GetMenu(mOptTerrainTypes);
	if (optterrmenu != nil)
	  InsertMenu(optterrmenu, -1);
	menu = GetMenu(mViewAngles);
	if (menu != nil)
	  InsertMenu(menu, -1);
	/* Init the support for the Windows menu. */
	winmenuwins = (WindowPtr *) xmalloc(MAXWINDOWS * sizeof(WindowPtr));
	for (i = 0; i < MAXWINDOWS; ++i)
	  winmenuwins[i] = nil;
	numwindows = 0;
	/* Done fiddling with menus, draw them. */
	DrawMenuBar();
}

/* Add a menu item that can be used to go to the given window.  Set the window title
   here too, ensures that the two match. */

void
add_window_menu_item(char *name, WindowPtr win)
{
	MenuHandle menu;
	Str255 tmpstr;

	if (numwindows < MAXWINDOWS) {
		c2p(name, tmpstr);
		SetWTitle(win, tmpstr);
		winmenuwins[numwindows++] = win;
		menu = GetMHandle(mWindows);
		if (menu != nil)
		  AppendMenu(menu, tmpstr);
	}
}

/* Remove the menu item that can be used to select a given window. */

void
remove_window_menu_item(WindowPtr win)
{
	int i, found = FALSE, next = 0;
	Str255 tmpstr;
	MenuHandle menu;

	/* Search for the window and remove it from the array of windows
	   that have menu items. */
	for (i = 0; i < numwindows; ++i) {
		if (!found && winmenuwins[i] == win) {
			found = TRUE;
		}
		if (found && i < numwindows - 1) {
			/* Shift the other windows down. */
			winmenuwins[i] = winmenuwins[i + 1];
		}
	}
	/* This routine gets called for all windows, so get out of here
	   if this window is not one of those in the menu. */
	if (!found)
	  return;
	--numwindows;
	/* Trash the existing window menu. */
	menu = GetMHandle(mWindows);
	if (menu != nil) {
		DeleteMenu(mWindows);
		ReleaseResource((Handle) menu);
	}
	/* Build a new version starting with the resource. */
	menu = GetMenu(mWindows);
	if (menu != nil) {
		for (i = 0; i < numwindows; ++i) {
			GetWTitle(winmenuwins[i], tmpstr);
			AppendMenu(menu, tmpstr);
		}
		/* Glue a view menu on after the windows menu. */
		if (GetMHandle(mViewMap) != nil)
		  next = mViewMap;
		if (GetMHandle(mViewList) != nil)
		  next = mViewList;
		InsertMenu(menu, next);
	}
	DrawMenuBar();
}

/* Fill up the side menu. */

void
build_side_menu()
{
	char *title;
	Side *side2;
	Str255 tmpstr;

	if (sidemenu != nil && CountMItems(sidemenu) < 1) {
		for_all_sides(side2) {
			title = shortest_side_title(side2, spbuf);
			sanitize_for_menu(title, tmpstr);
			AppendMenu(sidemenu, tmpstr);
			EnableItem(sidemenu, side_number(side2));
		}
		if (1 /* independent units possible in this game */) {
			AppendMenu(sidemenu, "\pindependent");
		}
	}
}

void
update_side_menu(Side *side2)
{
	char *title;
	Str255 tmpstr;

	title = shortest_side_title(side2, spbuf);
	sanitize_for_menu(title, tmpstr);
	SetMenuItemText(sidemenu, side_number(side2), tmpstr);
}

void
build_unit_type_menu()
{
	int u;
	Str255 tmpstr;

	if (utypemenu != nil && CountMItems(utypemenu) < 1) {
		for_all_unit_types(u) {
			sanitize_for_menu(u_type_name(u), tmpstr);
			AppendMenu(utypemenu, tmpstr);
			EnableItem(utypemenu, u + 1);
		}
	}
}

void
build_material_type_menu()
{
	int m;
	Str255 tmpstr;

	if (mtypemenu != nil && CountMItems(mtypemenu) < 1 && nummtypes > 0) {
		for_all_material_types(m) {
			sanitize_for_menu(m_type_name(m), tmpstr);
			AppendMenu(mtypemenu, tmpstr);
			EnableItem(mtypemenu, m + 1);
		}
	}
}

void
build_terrain_type_menu()
{
	int t;
	Str255 tmpstr;

	if (ttypemenu != nil && CountMItems(ttypemenu) < 1) {
		for_all_terrain_types(t) {
			sanitize_for_menu(t_type_name(t), tmpstr);
			AppendMenu(ttypemenu, tmpstr);
			EnableItem(ttypemenu, t);
		}
	}
}

void
build_ai_type_menu()
{
	if (aitypemenu != nil && CountMItems(aitypemenu) < 1) {
		AppendMenu(aitypemenu, "\pNone");
		EnableItem(aitypemenu, 1);
		AppendMenu(aitypemenu, "\pMplayer");
		EnableItem(aitypemenu, 2);
	}
}

void
build_feature_menu()
{
	int i = 1;
	char *name;
	Str255 tmpstr;
	Feature *feature;
	MenuHandle oldmenu, newmenu = nil;

	if (featuremenu != nil) {
		if (CountMItems(featuremenu) >= 1) {
			/* Trash the existing feature menu. */
			oldmenu = GetMHandle(mFeatures);
			if (oldmenu != nil) {
				DeleteMenu(mFeatures);
				ReleaseResource((Handle) oldmenu);
				/* Build a new version starting with the resource. */
				if ((newmenu = GetMenu(mFeatures)) != nil) {
					featuremenu = newmenu;
				}
			}
		}
		AppendMenu(featuremenu, "\pNo Feature");
		EnableItem(featuremenu, i++);
    	for (feature = featurelist; feature != NULL; feature = feature->next) {
    		name = feature_desc(feature, spbuf);
			sanitize_for_menu(name, tmpstr);
    		AppendMenu(featuremenu, tmpstr);
    		EnableItem(featuremenu, i++);
    	}
    	if (newmenu != nil) {
    		InsertMenu(featuremenu, -1);
    	}
	}
}

void
build_optional_terrain_type_menu()
{
	int t;
	Str255 tmpstr;

	if (optterrmenu != nil && CountMItems(optterrmenu) < 1) {
		for_all_terrain_types(t) {
			if (!t_is_cell(t)) {
				sanitize_for_menu(t_type_name(t), tmpstr);
				AppendMenu(optterrmenu, tmpstr);
				EnableItem(optterrmenu, t);
			}
		}
		if (CountMItems(optterrmenu) < 1)
		  optterrmenu = nil;
	}
}

/* Alter the feature menu to reflect a changed or new feature. */

void
update_feature_menu(Feature *feature)
{
	char *desc;
	Str255 tmpstr;

    desc = feature_desc(feature, spbuf);
	sanitize_for_menu(desc, tmpstr);
	if (feature->id + 1 > CountMItems(featuremenu)) {
		AppendMenu(featuremenu, tmpstr);
		EnableItem(featuremenu, feature->id + 1);
	} else {
		SetMenuItemText(featuremenu, feature->id + 1, tmpstr);
	}
}

/* This removes chars that are specially recognized by the Menu Manager.
   Has to be done so that strange game-defined names don't scramble the
   menus; innocuous because this affects only the appearance in the menu. */

static void
sanitize_for_menu(char *str, Str255 outstr)
{
	int i = 0;

	/* Replace special chars with blanks. */
	for (i = 0; str[i] != '\0' && i < 255; ++i) {
		switch (str[i]) {
			case ';':
			case '!':
			case '<':
			case '/':
			case '(':
			/* I don't think closing parens are special, but since the
			   (presumed) matching open paren is gone, might as well get
			   rid of the close also. */
			case ')':
				outstr[i + 1] = ' ';
				break;
			default:
				outstr[i + 1] = str[i];
				break;
		}
	}
	outstr[i + 1] = '\0';
	/* Replace a leading hyphen with an underscore. */
	if (outstr[1] == '-')
	  outstr[1] = '_';
	outstr[0] = i;
}

/* Decipher and do a menu command. */

void
do_menu_command(long which)
{
	short menuid, menuitem, daRefNum;
	Str255 daname;
	WindowPtr win;
	Map *map;

	menuid = HiWord(which);
	menuitem = LoWord(which);
	DGprintf("menu %d, item %d\n", menuid, menuitem);
	map = map_from_window(FrontWindow());	
	switch (menuid) {
		case mApple:
			switch (menuitem) {
				case miAppleAbout:
					do_about_box();
					break;
				case miAppleHelp:
					help_dialog(NULL);
					break;
				case miAppleInstructions:
					instructions_dialog();
					break;
				default:
					GetItem(GetMHandle(mApple), menuitem, daname);
					daRefNum = OpenDeskAcc(daname);
			}
			break;
		case mFile:
			switch (menuitem) {
				case miFileNew:
					/* (should reset state of everything first) */
					new_game_dialog();
					break;
				case miFileOpen:
					/* (should reset state of everything first) */
					open_game_dialog();
					break;
				case miFileConnect:
					connect_game_dialog();
					break;
				case miFileSave:
					save_the_game(FALSE, FALSE);
					break;
				case miFileSaveAs:	
					save_the_game(TRUE, FALSE);
					break;
				case miFilePreferences:
					set_preferences();
					break;
				case miFilePageSetup:
					do_page_setup_mi();
					break;
				case miFilePrintWindow:
					do_print_mi();
					break;
				case miFileResign:
					do_resign();
					break;
				case miFileQuit:
					quit_the_game();
					break;
			}
			break;
		case mEdit:
			if (SystemEdit(menuitem - 1))
			  break;
			switch (menuitem)  {
				case miEditCut:
					if (FrontWindow() == commandwin) {
						te_cut(command_text);
					} else if(FrontWindow() == constructionwin) {
						te_cut(run_length_text);
					}
					break;
				case miEditCopy:
					if (FrontWindow() == commandwin) {
						te_copy(command_text);
					} else if(FrontWindow() == constructionwin) {
						te_copy(run_length_text);
					}
					break;
				case miEditPaste:
					if (FrontWindow() == commandwin) {
						te_paste(command_text);
					} else if (FrontWindow() == constructionwin) {
						te_paste(run_length_text);
					}
					break;
				case miEditClear:
					if (FrontWindow() == commandwin) {
						TEDelete(command_text);						
					} else if (FrontWindow() == constructionwin) {
						TEDelete(run_length_text);						
					}
					break;
				case miEditSelectAll:
					do_select_all_mi();
					break;
#ifdef DESIGNERS
				case miEditDesign:
					do_design();
					break;
#endif
			}
			break;
		case mFind:
			switch (menuitem) {
				case miFindPrevious:
					do_find_previous_mi();
					break;
				case miFindNext:
					do_find_next_mi();
					break;
				case miFindLocation:
					do_find_location_mi();
					break;
				case miFindUnitByName:
					do_find_unit_by_name_mi();
					break;
				case miFindSelected:
					do_find_selected_mi();
					break;
			}
			break;
		case mPlay:
			switch (menuitem) {
				case miPlayCloseup:
					do_closeup_mi();
					break;
				case miPlayMove:
					query_position_modally(MOVE_TO_MODAL);
					break;
				case miPlaySetFormation:
					query_position_modally(SET_FORMATION_MODAL);
					break;
				case miPlayReturn:
					do_return();
					break;
				case miPlayWake:
					do_wake();
					break;
				case miPlaySleep:
					do_sleep();
					break;
				case miPlayReserve:
					do_reserve();
					break;
				case miPlayBuild:
					do_build();
					break;
				case miPlayRepair:
					do_repair_mi();
					break;
				case miPlayAttack:
					do_attack_mi();
					break;
				case miPlayOverrun:
					do_overrun_mi();
					break;
				case miPlayFire:
					query_position_modally(FIRE_MODAL);
					break;
				case miPlayFireInto:
					do_fire_into_mi();
					break;
				case miPlayDetonate:
					do_detonate_mi();
					break;
				case miPlayTake:
					do_take_mi();
					break;
				case miPlayDrop:
					break;
				case miPlayGive:
					/* has submenu mSides */
					break;
				case miPlayDetach:
					do_detach();
					break;
				case miPlayDisband:
					do_disband();
					break;
				case miPlayAddTerrain:
					query_position_modally(ADD_TERRAIN_MODAL);
					break;
				case miPlayRemoveTerrain:
					query_position_modally(REMOVE_TERRAIN_MODAL);
					break;
				case miPlayRename:
					do_name();
					break;
			}
			break;
		case mSides:
			do_give_unit_mi(menuitem);
			break;
		case mSide:
			switch (menuitem) {
				case miSideCloseup:
					break;
				case miSideFinishedTurn:
					finish_turn(dside);
					break;
				case miSideMoveOnClick:
					moveonclick_mi();
					break;
				case miSideAutoSelect:
					autoselect_mi();
					break;
				case miSideAutoFinish:
					/* Toggle auto-finish for turns. */
					set_autofinish(dside, !dside->autofinish);
					break;
				case miSideSound:
					playsounds = !playsounds;
					ui_update_state();
					save_preferences();
					break;
				case miSideRename:
					side_rename_dialog(dside);
					break;
			}
			break;
		case mWindows:
			switch (menuitem) {
				case miWindowsGame:
					if (gamewin == nil) {
						create_game_window();
					}
					ShowWindow(gamewin);
					SelectWindow(gamewin);
					break;
				case miWindowsNotices:
					if (noticewin == nil) {
						create_notice_window();
					}
					ShowWindow(noticewin);
					SelectWindow(noticewin);
					break;
				case miWindowsHistory:
					if (historywin == nil) {
						create_history_window();
					}
					ShowWindow(historywin);
					SelectWindow(historywin);
					break;
				case miWindowsConstruction:
					enable_construction();
					break;
				case miWindowsCommand:
					enable_command();
					break;
				/* should have agreements list etc handling here also */
				case miWindowsScores:
					scores_dialog();
					break;
				case miWindowsNewList:
					create_list();
					break;
				case miWindowsNewMap:
					create_map(5 /* should be a pref */);
					break;
				case miWindowsWorldMap:
					if (worldmapwin == nil) {
						int power, winwid = 200;

						for (power = 0; power < NUMPOWERS; ++power) {
							if (area.width * hws[power] <= winwid
								&& area.width * hws[power+1] > winwid) break;
						}
						worldmap = create_map(power);
						worldmap->drawothermaps = TRUE;
						/* (should be in survey mode too?) */
						worldmapwin = worldmap->window;
					}
					ShowWindow(worldmapwin);
					SelectWindow(worldmapwin);
					break;
				default:
					win = winmenuwins[menuitem - miWindowsFirst];
					if (win != nil) {
						SelectWindow(win);
					}
					break;
			}
			break;
		case mViewMap:
			if (map == NULL) break;
			switch (menuitem)  {
				case miViewCloser:
					magnify_map(map, 1);
					break;
				case miViewFarther:
					magnify_map(map, -1);
					break;
				case miViewNames:
					toggle_map_names(map);
					break;
				case miViewPeople:
					toggle_map_people(map);
					break;
				case miViewElevations:
					toggle_map_elevations(map);
					break;
				case miViewDaylight:
					toggle_map_lighting(map);
					break;
				case miViewCoverage:
					toggle_map_coverage(map);
					break;
				case miViewGrid:
					toggle_map_grid(map);
					break;
				case miViewTopline:
					toggle_map_topline(map);
					break;
				case miViewTopunit:
					toggle_map_topunit(map);
					break;
				case miViewOtherMaps:
					toggle_map_other_maps(map);
					break;
			}
			break;
		case mViewWeather:
			if (map == NULL) break;
			switch (menuitem)  {
				case miWeatherTemperature:
					toggle_map_temperature(map);
					break;
				case miWeatherWinds:
					toggle_map_winds(map);
					break;
				case miWeatherClouds:
					toggle_map_clouds(map);
					break;
				case miWeatherStorms:
					toggle_map_storms(map);
					break;
			}
			break;
		case mViewAngles:
			if (map == NULL) break;
			switch (menuitem)  {
				case miAngle15:
					set_view_angle(map->vp, 15);
					force_map_update(map);
					break;
				case miAngle30:
					set_view_angle(map->vp, 30);
					force_map_update(map);
					break;
				case miAngle90:
					set_view_angle(map->vp, 90);
					force_map_update(map);
					break;
			}
			break;
		case mViewList:
			switch (menuitem)  {
				case miViewByType:
					set_list_sorting(list_from_window(FrontWindow()), bytype, menuitem);
					break;
				case miViewByName:
					set_list_sorting(list_from_window(FrontWindow()), byname, menuitem);
					break;
				case miViewBySide:
					set_list_sorting(list_from_window(FrontWindow()), byside, menuitem);
					break;
				case miViewByActOrder:
					set_list_sorting(list_from_window(FrontWindow()), byactorder, menuitem);
					break;
				case miViewByLocation:
					set_list_sorting(list_from_window(FrontWindow()), bylocation, menuitem);
					break;
				case miViewIconSize:
					toggle_list_large_icons(list_from_window(FrontWindow()));
					break;
			}
			break;
		case mAITypes:
			switch (menuitem) {
				case 1:
					if (side_has_ai(dside)) {
						set_side_ai(dside, NULL);
					}
					break;
				case 2:
					if (!side_has_ai(dside)) {
						set_side_ai(dside, "mplayer");
					}
					break;
			}
			break;
		case mMagnifications:
			if (map == NULL) break;
			set_map_mag(map, menuitem - 1);
			break;
		case mMaterialTypes:
			if (map == NULL) break;
			toggle_map_materials(map, menuitem - 1);
			break;
		case mOptTerrainTypes:
			if (map == NULL) break;
			toggle_map_aux_terrain(map, map->auxterraintypes[menuitem]);
			break;
	}
	HiliteMenu(0);
}

/* Display the "About..." box. */

void
do_about_box()
{
	short ditem;
	Str255 tmpstr;
	WindowPtr win;
	PicHandle pic;
	short itemtype;  Handle itemhandle;  Rect itemrect;

	win = GetNewDialog(dAbout, NULL, (DialogPtr) -1L);
	/* Fill in the kernel's version and copyright. */
	GetDItem(win, diAboutVersion, &itemtype, &itemhandle, &itemrect);
	c2p(version_string(), tmpstr);
	SetIText(itemhandle, tmpstr);
	GetDItem(win, diAboutCopyright, &itemtype, &itemhandle, &itemrect);
	c2p(copyright_string(), tmpstr);
	SetIText(itemhandle, tmpstr);
	/* Substitute a color picture if we've got a color/gray screen. */
	if (hasColorQD) {
		GetDItem(win, diAboutPicture, &itemtype, &itemhandle, &itemrect);
		if ((pic = (PicHandle) GetResource('PICT', pSplashColor)) != nil) {
			SetDItem(win, diAboutPicture, itemtype, (Handle) pic, &itemrect);
		}
	}
	ShowWindow(win);
	SelectWindow(win);
	draw_default_button(win, diAboutOK);
	SetCursor(&QD(arrow));
	ModalDialog(NULL, &ditem);
	/* Just return, no matter what input. */
	HideWindow(win);
	DisposDialog(win);
}

/* This routine does both "save" and "save as". */

void
save_the_game(int askname, int quitting)
{
	Point pnt;
	char namebuf[256];
	Str255 tmpstr;
    SFReply reply;

#ifdef DESIGNERS
	/* Do the selective save only if we're not shutting down Xconq. */
	if (dside->designer && !quitting) {
		designer_save_dialog();
		return;
	}
#endif
	if (askname || cursavename == NULL) {
		if (cursavename == NULL)
		  cursavename = saved_game_filename();
		/* Collect the file and path to save to. */
		SetPt(&pnt, 100, 100);
		c2p(cursavename, tmpstr);
		SFPutFile(pnt, "\p", tmpstr, /*(DlgHookProcPtr)*/ nil, &reply);
		if (!reply.good)
		  return;
		/* Make the location of the file be the current volume. */
		SetVol(reply.fName, reply.vRefNum);
		p2c(((char *) reply.fName), namebuf);
		cursavename = copy_string(namebuf);
	}
	SetCursor(*watchcursor);
	write_entire_game_state(cursavename);
	set_game_file_type(cursavename);
	SetCursor(&QD(arrow));
}

/*THPrint printrecordhandle = nil;*/

void
maybe_init_print()
{
/*	if (printrecordhandle == nil) {
		printrecordhandle = (THPrint) NewHandle(sizeof(TPrint));
		PrOpen();
		PrintDefault(printrecordhandle);
	} */
}

void
do_page_setup_mi()
{
	maybe_init_print();
/*	PrStlDialog(printrecordhandle); */
}

/* Handle printing.  What gets printed depends on which window is in front. */

static void
do_print_mi()
{
	Map *map;
	List *list;

	maybe_init_print();
/*	if (!PrJobDialog(printrecordhandle)) return; */
	if ((map = map_from_window(FrontWindow())) != NULL) {
		print_map(map);
	} else if ((list = list_from_window(FrontWindow())) != NULL) {
		print_list(list);
	} else {
		/* (should at least be able to print help) */
	}
}

static void
resign_the_game(int forced)
{
	Str255 tmpstr;

	sprintf(tmpbuf, "Other players");
	c2p(tmpbuf, tmpstr);
	ParamText(tmpstr, "\p", "\p", "\p");
	switch (CautionAlert(aResignGame, nil)) {
		case aiResignGameOK:
			forcedtoresign = forced;
			resign_game(dside, NULL);
			break;
		case aiResignGameWillingToDraw:
			set_willing_to_draw(dside, TRUE);
			break;
		case aiResignGameCancel:
			break;
	}
}

/* Attempt to quit. */

void
quit_the_game()
{
	int confirmresign = FALSE;

#ifdef DESIGNERS
	/* If we're designing, need only confirm the save before quitting;
	   no resignation requirement. */
	if (dside->designer) {
		switch (CautionAlert(aDesignerQuitGame, nil)) {
			case aiDesignerQuitGameOK:
				SetCursor(*watchcursor);
				save_the_game(FALSE, TRUE);
				SetCursor(&QD(arrow));
				/* Fall through to exit. */
			case aiDesignerQuitGameDontSave:
				ExitToShell();
				break;
			case aiDesignerQuitGameCancel:
				break;
		}
		return;
	}
#endif /* DESIGNERS */
	if (endofgame) {
		/* Game is already over, nothing to save, just leave. */
		ExitToShell();
	} else if (!gamestatesafe || !interfacestatesafe) {
		switch (CautionAlert(aQuitGame, nil)) {
			case aiQuitGameOK:
				if (all_others_willing_to_save(dside)) {
					SetCursor(*watchcursor);
					save_the_game(FALSE, TRUE);
					SetCursor(&QD(arrow));
					ExitToShell();
				} else {
					set_willing_to_save(dside, TRUE);
					/* (should warn that not all were willing to save) */
				}
				break;
			case aiQuitGameDontSave:
				if (all_others_willing_to_quit(dside)) {
					set_willing_to_draw(dside, TRUE);
				} else {
					/* Can only get out by resigning. */
					confirmresign = TRUE;
				}
				break;
			case aiQuitGameCancel:
				break;
		}
	} else {
		/* (should still confirm that we're not avoiding our fate?) */
		ExitToShell();
	}
	if (confirmresign) {
		resign_the_game(TRUE);
	}
}

static void
te_cut(TEHandle hdl)
{
	if (ZeroScrap() == noErr) {
		TECut(hdl);
		if (TEToScrap() != noErr)
		  ZeroScrap();
	}
}

static void
te_copy(TEHandle hdl)
{
	if (ZeroScrap() == noErr) {
		TECopy(hdl);
		if (TEToScrap() != noErr)
		  ZeroScrap();
	}
}

static void
te_paste(TEHandle hdl)
{
	if (TEFromScrap() == noErr) {
		TEPaste(hdl);
	}
}

static void
do_select_all_mi()
{
	Map *map;
	Unit *unit;

	if (FrontWindow() == commandwin) {
		/* (should add this) */
	} else if (FrontWindow() == constructionwin) {
		/* (should add this) */
	} else if ((map = map_from_window(FrontWindow())) != NULL) {
		for_all_units(unit) {
			if (side_may_select(unit)) {
				select_unit_on_map(map, unit);
			}
		}
		/* Do all the drawing at once. */
		draw_selections(map);
	} else if (0 /* is list */) {
	}
}

static void
do_find_previous_mi()
{
	Map *map;
	
	if ((map = map_from_window(FrontWindow())) != NULL) {
		select_previous_unit(map);
	}
}

static void
do_find_next_mi()
{
	Map *map;
	
	if ((map = map_from_window(FrontWindow())) != NULL) {
		select_next_mover(map);
	}
}

/* (should ask for x,y coords, focus/center on that position) */

static void
do_find_location_mi()
{
	Map *map;

	beep();	
	if ((map = map_from_window(FrontWindow())) != NULL) {
	}
}

/* (should ask for a textual name/select from a gazetteer, scroll to that unit) */

static void
do_find_unit_by_name_mi()
{
	Map *map;
	
	beep();	
	if ((map = map_from_window(FrontWindow())) != NULL) {
	}
}

static void
do_find_selected_mi()
{
	Map *map;
	List *list;
	UnitCloseup *closeup;
	
	if ((map = map_from_window(FrontWindow())) != NULL) {
		int i;
		Unit *unit;

		/* Scroll to the first valid selected unit we find. */
		for (i = 0; i < map->numselections; ++i) {
			unit = map->selections[i];
			if (in_play(unit)) {
				scroll_to_unit(map, unit);
				return;
			}
		}
		beep();
	} else if ((list = list_from_window(FrontWindow())) != NULL) {
		scroll_to_selected_unit_in_list(list);
	} else if ((closeup = unit_closeup_from_window(FrontWindow())) != NULL) {
		if (closeup->unit != NULL)
		  scroll_best_map_to_unit(closeup->unit);
	} else if (constructionwin == FrontWindow()) {
		scroll_to_selected_construction_unit();
	} else {
		/* nothing to do */
	}
}

/* Menu items/commands for unit play action. */

void
apply_to_all_selected(int (*fn)(Unit *unit), int beepfailure)
{
	int i, rslt, numcould = 0, numnot = 0;
	Map *map;  List *list;  UnitCloseup *closeup;
	Unit *unit;

	if (fn == NULL)
	  return;
	if ((map = map_from_window(FrontWindow())) != NULL) {
		for (i = 0; i < map->numselections; ++i) {
			unit = map->selections[i];
			if (in_play(unit)) {
				rslt = (*fn)(unit);
				if (rslt)
				  ++numcould;
				else
				  ++numnot;
			}
		}
	} else if ((list = list_from_window(FrontWindow())) != NULL) {
		if ((unit = (Unit *) selected_unit_in_list(list)) != NULL) {
			rslt = (*fn)(unit);
			if (rslt)
			  ++numcould;
			else
			  ++numnot;
		}
	} else if ((closeup = unit_closeup_from_window(FrontWindow())) != NULL) {
		if ((unit = closeup->unit) != NULL) {
			rslt = (*fn)(unit);
			if (rslt)
			  ++numcould;
			else
			  ++numnot;
		}
	}
	/* If everybody that was asked to do this couldn't, beep once. */
	if (beepfailure && numcould == 0 && numnot > 0)
	  beep();
}

static void
do_closeup_mi()
{
	int i;
	Map *map;  List *list;  UnitCloseup *unitcloseup;
	Unit *unit;
	
	if ((map = map_from_window(FrontWindow())) != NULL) {
		for (i = 0; i < map->numselections; ++i) {
			if ((unit = map->selections[i]) != NULL) {
				if ((unitcloseup = find_unit_closeup(unit)) != NULL) {
					ShowWindow(unitcloseup->window);
					SelectWindow(unitcloseup->window);
				} else {
					create_unit_closeup(unit);
				}
			}
		}
	} else if ((list = list_from_window(FrontWindow())) != NULL) {
		if ((unit = (Unit *) selected_unit_in_list(list)) != NULL) {
			if ((unitcloseup = find_unit_closeup(unit)) != NULL) {
				ShowWindow(unitcloseup->window);
				SelectWindow(unitcloseup->window);
			} else {
				create_unit_closeup(unit);
			}
		}
	} else if ((unitcloseup = unit_closeup_from_window(FrontWindow())) != NULL) {
		if ((unit = unitcloseup->unit) != NULL) {
			force_update(unitcloseup->window);
		}
	}
}

static void
do_move_mi()
{
	do_move_to();
}

static void
do_repair_mi()
{
	apply_to_all_selected(do_one_repair, 0);	
}

static int
do_one_repair(Unit *unit)
{
	return TRUE;
}

/* (should find one unit to attack, do modal dialog if != 1) */

static void
do_attack_mi()
{
	int i;
	Map *map;
	Unit *atker, *other;
	
	if ((map = map_from_window(FrontWindow())) != NULL) {
		for (i = 0; i < map->numselections; ++i) {
			if ((atker = map->selections[i]) != NULL) {
				for_all_units(other) {
					if (other != atker
						&& other->side != atker->side
						&& distance(atker->x, atker->y, other->x, other->y) == 1) {
							prep_attack_action(atker, atker, other, 100);
							break;
					}
				}
			}
		}
	}
}

void
do_attack_command()
{
	int i, numcould = 0, numnot = 0;
	Point target;
	Map *map;
	Unit *atker, *other;
	
	if ((map = map_from_window(FrontWindow())) != NULL) {
		GetMouse(&target);
		m_nearest_unit(map, target.h, target.v, &other);
		if (other != NULL) {
			for (i = 0; i < map->numselections; ++i) {
				if ((atker = map->selections[i]) != NULL) {
					if (other != atker
						&& other->side != atker->side) {
						if (distance(atker->x, atker->y, other->x, other->y) <= 1
							&& valid(check_attack_action(atker, atker, other, 100))) {
							prep_attack_action(atker, atker, other, 100);
							++numcould;
						} else if (valid(check_fire_at_action(atker, atker, other, -1))) {
							prep_fire_at_action(atker, atker, other, -1);
							++numcould;
						} else {
							++numnot;
						}
					} else {
					}
				}
			}
		} else {
			/* (should let units fire into empty cell) */
		}
	}
	/* If nobody could do the action, beep once. */
	if (numcould == 0 && numnot > 0) beep();
}

static void
do_overrun_mi()
{
	/* (should loop until mouse clicked in map) */
	beep();
}

int
do_fire_command()
{
	int i, numcould = 0, numnot = 0;
	Point target;
	Map *map;
	Unit *atker, *other;
	
	if ((map = map_from_window(FrontWindow())) != NULL) {
		GetMouse(&target);
		m_nearest_unit(map, target.h, target.v, &other);
		if (other != NULL) {
			for (i = 0; i < map->numselections; ++i) {
				if ((atker = map->selections[i]) != NULL && valid_selection(atker)) {
					if (other != atker
						&& other->side != atker->side
						&& valid(check_fire_at_action(atker, atker, other, -1))) {
						prep_fire_at_action(atker, atker, other, -1);
						++numcould;
					} else {
						++numnot;
					}
				}
			}
		} else {
			/* Just error out.  To bombard a cell, the player must do a fire-into
			   command.  It might be useful to add an option that automatically tries
			   firing into if no targets are visible, but if a player was trying to
			   fire at a unit but got the cell slightly wrong, then the fire-into will
			   happen anyway and the player loses the acp. */
			beep();
		}
	}
	/* If nobody could do the action, beep once. */
	if (numcould == 0 && numnot > 0) {
		beep();
		return FALSE;
	} else {
		return TRUE;
	}
}

static void
do_fire_into_mi()
{
	if (map_modal == NO_MODAL) {
		map_modal = FIRE_INTO_MODAL;
	}
}

int
do_fire_into_command()
{
	int x, y, i, numcould = 0, numnot = 0;
	Point target;
	Map *map;
	Unit *atker;
	
	if ((map = map_from_window(FrontWindow())) != NULL) {
		GetMouse(&target);
		if (m_nearest_cell(map, target.h, target.v, &x, &y)) {
			for (i = 0; i < map->numselections; ++i) {
				if ((atker = map->selections[i]) != NULL && valid_selection(atker)) {
					if (valid(check_fire_into_action(atker, atker, x, y, 0, -1))) {
						prep_fire_into_action(atker, atker, x, y, 0, -1);
						++numcould;
					} else {
						++numnot;
					}
				}
			}
		} else {
			beep();
		}
	}
	/* If nobody could do the action, beep once. */
	if (numcould == 0 && numnot > 0) {
		beep();
		return FALSE;
	} else {
		return TRUE;
	}
}

/* (should decide how to detonate based on selections and modal input) */

int
do_one_detonate(Unit *unit)
{
    if (valid(check_detonate_action(unit, unit, unit->x, unit->y, unit->z))) {
		prep_detonate_action(unit, unit, unit->x, unit->y, unit->z);
		return TRUE;
	}
	return FALSE;
}

static void
do_detonate_mi()
{
	apply_to_all_selected(do_one_detonate, TRUE);
}

/* This command samples the current mouse to get the desired detonate location. */

void
do_detonate_command()
{
	int i, x, y;
	Point detpoint;
	Map *map;
	Unit *unit;
	
	if ((map = map_from_window(FrontWindow())) != NULL) {
		GetMouse(&detpoint);
		m_nearest_cell(map, detpoint.h, detpoint.v, &x, &y);
		for (i = 0; i < map->numselections; ++i) {
			if ((unit = map->selections[i]) != NULL) {
				prep_detonate_action(unit, unit, x, y, unit->z);
			}
		}
	}
}

int
do_one_give(Unit *unit)
{
	give_supplies(unit, NULL, NULL);
	return 0;
}

Side *tmpcmdside;

int
do_one_give_unit(Unit *unit)
{
	if (unit->side == tmpcmdside)
	  return FALSE;
#ifdef DESIGNERS
	if (dside->designer) {
		designer_change_side(unit, tmpcmdside);
		return TRUE;
	}
#endif /* DESIGNERS */
	prep_change_side_action(unit, unit, tmpcmdside);
	return TRUE;
}

static void
do_give_unit_mi(int mi)
{
	Side *side = side_n(mi); /* note that side_n returns NULL for mi == numsides */
	
	apply_to_all_selected(do_one_give_unit, TRUE);
}

static void
moveonclick_mi()
{
	Map *frontmap = map_from_window(FrontWindow());

	if (frontmap) {
		frontmap->moveonclick = !frontmap->moveonclick;
		/* (should force updates of map windows to reflect?) */
	} else {
		defaultmoveonclick = !defaultmoveonclick;
	}
}

int
do_one_take(Unit *unit)
{
	take_supplies(unit, NULL, NULL);
	return TRUE;
}

static void
do_take_mi()
{
	apply_to_all_selected(do_one_take, TRUE);
}

static void
autoselect_mi()
{
	Map *frontmap = map_from_window(FrontWindow());
	extern Unit *curunit;

	if (frontmap) {
		frontmap->autoselect = !frontmap->autoselect;
		frontmap->curunit = NULL; /* maybe not a good idea? */
		/* (should force updates of map windows to reflect?) */
	} else {
		defaultautoselect = !defaultautoselect;
	}
}

static void
adjust_menu_item(MenuHandle menu, int item, int value)
{
	if (value)
	  EnableItem(menu, item);
	else
	  DisableItem(menu, item);
}

/* The state of menu items changes to reflect selected units, etc. */

void
adjust_menus()
{
	int i, m, t, numwins, menus_useful, numitems, has_text, offset;
	MenuHandle menu;
	Map *frontmap;
	List *frontlist;
	UnitCloseup *frontcloseup;
	Unit *unit;
	WindowPtr frontwin;

	frontwin = FrontWindow();
	frontmap = map_from_window(frontwin);
	frontlist = list_from_window(frontwin);
	frontcloseup = unit_closeup_from_window(frontwin);
	menus_useful =
		(gamedefined
		 && ((!beforestart && !endofgame)
		 	 || (gameinited
#ifdef DESIGNERS
		 	     && dside->designer
#endif /* DESIGNERS */
		 	     )));
	/* Certain menus need to be built after the game is running. */
	if (menus_useful) {
		build_material_type_menu();
		build_side_menu();
		build_ai_type_menu();
	}
#if 0
	if (is_da_window(FrontWindow())) {
		if ((menu = GetMHandle(mEdit)) != nil) {
		}
		return;
	}
#endif
	menu = GetMHandle(mApple);
	if (menu != nil) {
		adjust_menu_item(menu, miAppleInstructions, gamedefined);
	}
	menu = GetMHandle(mFile);
	if (menu != nil) {
		/* Availability of file menu items depends on whether we're in a game. */
		adjust_menu_item(menu, miFileNew, !gamedefined);
		adjust_menu_item(menu, miFileOpen, !gamedefined);
		adjust_menu_item(menu, miFileConnect, !gamedefined);
		adjust_menu_item(menu, miFileSave, menus_useful);
		adjust_menu_item(menu, miFileSaveAs, menus_useful);
		adjust_menu_item(menu, miFilePrintWindow, (frontmap != NULL || frontlist != NULL));
		adjust_menu_item(menu, miFileResign, menus_useful && dside->ingame);
	}
	menu = GetMHandle(mEdit);
	if (menu != nil) {
		/* Edit menu is always available, but individual items may be disabled. */
		adjust_menu_item(menu, miEditUndo, FALSE);
		has_text = FALSE;
		if (frontwin == commandwin) {
			has_text = ((*command_text)->selStart < (*command_text)->selEnd);
		} else if (frontwin == constructionwin) {
			has_text = ((*run_length_text)->selStart < (*run_length_text)->selEnd);
		}
		adjust_menu_item(menu, miEditCut, has_text);
		adjust_menu_item(menu, miEditCopy, has_text);
		adjust_menu_item(menu, miEditPaste, (GetScrap(nil, 'TEXT', (long *) &offset) > 0));
		adjust_menu_item(menu, miEditClear, has_text);
		adjust_menu_item(menu, miEditSelectAll, (frontmap != NULL || frontlist != NULL || has_text));
		if (menus_useful) {
#ifdef DESIGNERS
			CheckItem(menu, miEditDesign, dside->designer);
#else
			DisableItem(menu, miEditDesign);
#endif /* DESIGNERS */
		}
	}
	menu = GetMHandle(mFind);
	if (menu != nil) {
		if (gameinited) {
			EnableItem(menu, 0);
			adjust_menu_item(menu, miFindPrevious, menus_useful);
			adjust_menu_item(menu, miFindNext, menus_useful);
			adjust_menu_item(menu, miFindLocation, menus_useful);
			adjust_menu_item(menu, miFindUnitByName, TRUE);
			adjust_menu_item(menu, miFindSelected, (frontmap != NULL || frontlist != NULL || frontcloseup != NULL || constructionwin == frontwin));
		} else {
			/* We're not even in a game yet, turn entire menu off. */
			DisableItem(menu, 0);
		}
	}
	menu = GetMHandle(mPlay);
	if (menu != nil) {
		if (gameinited) {
			EnableItem(menu, 0);
			/* Disable everything first, then selectively re-enable. */
			DisableItem(menu, miPlayCloseup);
			DisableItem(menu, miPlayMove);
			DisableItem(menu, miPlaySetFormation);
			DisableItem(menu, miPlayReturn);
			DisableItem(menu, miPlayWake);
			DisableItem(menu, miPlaySleep);
			DisableItem(menu, miPlayReserve);
			DisableItem(menu, miPlayDelay);
			DisableItem(menu, miPlayBuild);
			DisableItem(menu, miPlayRepair);
			DisableItem(menu, miPlayAttack);
			DisableItem(menu, miPlayOverrun);
			DisableItem(menu, miPlayFire);
			DisableItem(menu, miPlayFireInto);
			DisableItem(menu, miPlayDetonate);
			DisableItem(menu, miPlayTake);
			DisableItem(menu, miPlayDrop);
			DisableItem(menu, miPlayGive);
			DisableItem(menu, miPlayDetach);
			DisableItem(menu, miPlayDisband);
			DisableItem(menu, miPlayAddTerrain);
			DisableItem(menu, miPlayRemoveTerrain);
			DisableItem(menu, miPlayRename);
			/* Note that command enabling is accumulative for all units. */
			if (frontmap != NULL) {
				for (i = 0; i < frontmap->numselections; ++i) {
					if ((unit = frontmap->selections[i]) != NULL) {
						enable_commands_for_unit(menu, unit);
					}
				}
			} else if (frontlist != NULL) {
				if ((unit = (Unit *) selected_unit_in_list(frontlist)) != NULL) {
					enable_commands_for_unit(menu, unit);
				}
			} else if (frontcloseup != NULL) {
				if ((unit = frontcloseup->unit) != NULL) {
					enable_commands_for_unit(menu, unit);
				}
			}
		} else {
			/* We're not even in a valid game state yet, turn entire menu off. */
			DisableItem(menu, 0);
		}
	}
	menu = GetMHandle(mSide);
	if (menu != nil) {
		if (menus_useful) {
			EnableItem(menu, 0);
			CheckItem(menu, miSideMoveOnClick,
					  (frontmap ? frontmap->moveonclick : defaultmoveonclick));
			CheckItem(menu, miSideAutoSelect,
					  (frontmap ? frontmap->autoselect : defaultautoselect));
			CheckItem(menu, miSideAutoFinish, dside->autofinish);
			CheckItem(menu, miSideSound, playsounds);
			if (dside->nameslocked) {
				DisableItem(menu, miSideRename);
			} else {
				EnableItem(menu, miSideRename);
			}
		} else {
			/* We're not even in a game yet, turn entire menu off. */
			DisableItem(menu, 0);
		}
	}
	menu = GetMHandle(mWindows);
	if (menu != nil) {
		if (gameinited) {
			EnableItem(menu, 0);
			/* Every item is always enabled. (should be, anyway) */
			CheckItem(menu, miWindowsGame,
				(gamewin && ((WindowPeek) gamewin)->visible));
			CheckItem(menu, miWindowsNotices,
				(noticewin && ((WindowPeek) noticewin)->visible));
			CheckItem(menu, miWindowsHistory,
				(historywin && ((WindowPeek) historywin)->visible));
			CheckItem(menu, miWindowsCommand,
				(commandwin && ((WindowPeek) commandwin)->visible));
			if (any_construction_possible()) {
				EnableItem(menu, miWindowsConstruction);
				CheckItem(menu, miWindowsConstruction,
					(constructionwin && ((WindowPeek) constructionwin)->visible));
			} else {
				DisableItem(menu, miWindowsConstruction);
			}
			numwins = CountMItems(menu) - miWindowsFirst;
			for (i = 0; i < numwins; ++i) {
				CheckItem(menu, i + miWindowsFirst, (winmenuwins[i] == frontwin));
			}
		} else {
			/* We're not even in a game yet, turn entire menu off. */
			DisableItem(menu, 0);
		}
	}
	/* If a map window is frontmost, install and adjust the map view menu. */
	if (frontmap != NULL) {
		/* Delete the list view menu if that's what's installed currently. */
		menu = GetMHandle(mViewList);
		if (menu != nil)
		  DeleteMenu(mViewList);
		/* Make sure the map view menu is installed (is always at the end). */
		InsertMenu(mapviewmenu, 0);
		menu = GetMHandle(mViewMap);
		if (menu != nil) {
			EnableItem(menu, 0);
			if (frontmap->vp->power == 0) {
				DisableItem(menu, miViewFarther);
			} else {
				EnableItem(menu, miViewFarther);
			}
			if (frontmap->vp->power == NUMPOWERS-1) {
				DisableItem(menu, miViewCloser);
			} else {
				EnableItem(menu, miViewCloser);
			}
			EnableItem(menu, miViewGrid);
			CheckItem(menu, miViewGrid, frontmap->drawgrid);
			EnableItem(menu, miViewTopline);
			CheckItem(menu, miViewTopline, (frontmap->toplineh > 0));
			EnableItem(menu, miViewTopunit);
			CheckItem(menu, miViewTopunit, (frontmap->topunith > 0));
			EnableItem(menu, miViewNames);
			CheckItem(menu, miViewNames, frontmap->drawnames);
			if (people_sides_defined()) {
				EnableItem(menu, miViewPeople);
				CheckItem(menu, miViewPeople, frontmap->drawpeople);
			} else {
				DisableItem(menu, miViewPeople);
			}
			if (elevations_defined()) {
				EnableItem(menu, miViewElevations);
				CheckItem(menu, miViewElevations, frontmap->drawelevations);
			} else {
				DisableItem(menu, miViewElevations);
			}
			if (temperatures_defined() || winds_defined() || clouds_defined()) {
				EnableItem(menu, miViewWeather);
			} else {
				DisableItem(menu, miViewWeather);
			}
			if (nummtypes > 0) {
				EnableItem(menu, miViewMaterials);
				/* (should do checkmark if anything in submenu is on) */
			} else {
				DisableItem(menu, miViewMaterials);
			}
			if (numbordtypes + numconntypes + numcoattypes > 0) {
				EnableItem(menu, miViewTerrain);
			} else {
				DisableItem(menu, miViewTerrain);
			}
			if (world.daylength != 1) {
				EnableItem(menu, miViewDaylight);
				CheckItem(menu, miViewDaylight, frontmap->drawlighting);
			} else {
				DisableItem(menu, miViewDaylight);
			}
			if (dside->coverage) {
				EnableItem(menu, miViewCoverage);
				CheckItem(menu, miViewCoverage, frontmap->drawcover);
			} else {
				DisableItem(menu, miViewCoverage);
			}
			if (nummaps > 1) {
				EnableItem(menu, miViewOtherMaps);
				CheckItem(menu, miViewOtherMaps, frontmap->drawothermaps);
			} else {
				DisableItem(menu, miViewOtherMaps);
			}
			menu = GetMHandle(mViewWeather);
			if (menu != nil) {
				EnableItem(menu, 0);
				if (temperatures_defined()) {
					EnableItem(menu, miWeatherTemperature);
					CheckItem(menu, miWeatherTemperature, frontmap->drawtemperature);
				} else {
					DisableItem(menu, miWeatherTemperature);
				}
				if (winds_defined()) {
					EnableItem(menu, miWeatherWinds);
					CheckItem(menu, miWeatherWinds, frontmap->drawwinds);
				} else {
					DisableItem(menu, miWeatherWinds);
				}
				if (clouds_defined()) {
					EnableItem(menu, miWeatherClouds);
					CheckItem(menu, miWeatherClouds, frontmap->drawclouds);
				} else {
					DisableItem(menu, miWeatherClouds);
				}
				/* should define what this means */
				DisableItem(menu, miWeatherStorms);
/*				CheckItem(menu, miWeatherStorms, frontmap->drawstorms); */
			}
		}
	} else if (frontlist != NULL) {
		/* Do the list view menu similarly. */
		menu = GetMHandle(mViewMap);
		if (menu != nil)
		  DeleteMenu(mViewMap);
		/* Make sure the menu is actually installed (is always at the end). */
		InsertMenu(listviewmenu, 0);
		menu = GetMHandle(mViewList);
		if (menu != nil) {
			EnableItem(menu, 0);
			if (1 /* at least one unit in list */) {
				EnableItem(menu, miViewByType);
				CheckItem(menu, miViewByType, (frontlist->mainsortmi == miViewByType));
				EnableItem(menu, miViewByName);
				CheckItem(menu, miViewByName, (frontlist->mainsortmi == miViewByName));
				EnableItem(menu, miViewBySide);
				CheckItem(menu, miViewBySide, (frontlist->mainsortmi == miViewBySide));
				EnableItem(menu, miViewByActOrder);
				CheckItem(menu, miViewByActOrder, (frontlist->mainsortmi == miViewByActOrder));
				EnableItem(menu, miViewByLocation);
				CheckItem(menu, miViewByLocation, (frontlist->mainsortmi == miViewByLocation));
				/* should mark the secondary sort? */
				EnableItem(menu, miViewWithTransport);
				DisableItem(menu, miViewWithCommander);
				EnableItem(menu, miViewIconSize);
				CheckItem(menu, miViewIconSize, frontlist->largeicons);
			} else {
				DisableItem(menu, miViewByType);
				DisableItem(menu, miViewByName);
				DisableItem(menu, miViewBySide);
				DisableItem(menu, miViewByActOrder);
				DisableItem(menu, miViewByLocation);
				DisableItem(menu, miViewWithTransport);
				DisableItem(menu, miViewWithCommander);
				DisableItem(menu, miViewIconSize);
			}
		}
	} else {
		/* For any other window, disable any or all of the view menus, as needed. */
		menu = GetMHandle(mViewList);
		if (menu != nil)
		  DisableItem(menu, 0);
		menu = GetMHandle(mViewMap);
		if (menu != nil)
		  DisableItem(menu, 0);
	}
	menu = GetMHandle(mMagnifications);
	if (menu != nil) {
		/* Always on. */
		EnableItem(menu, 0);
		if (frontmap != NULL) {
			for (i = 0; i < NUMPOWERS; ++i) {
				CheckItem(menu, i + 1, (i == frontmap->vp->power));
			}
		}
	}
	menu = GetMHandle(mMaterialTypes);
	if (menu != nil) {
		/* Always on, if any material types defined. */
		if (nummtypes > 0) {
			EnableItem(menu, 0);
			if (frontmap != NULL) {
				for_all_material_types(m) {
					if (any_cell_materials_defined() && cell_material_defined(m)) {
						EnableItem(menu, m + 1);
						CheckItem(menu, m + 1, frontmap->drawmaterials[m]);
					} else {
					    CheckItem(menu, i, FALSE);
						DisableItem(menu, m + 1);
					}
				}
			}
		} else {
			DisableItem(menu, 0);
		}
	}
	menu = GetMHandle(mOptTerrainTypes);
	if (menu != nil) {
		if (any_aux_terrain_defined()) {
			EnableItem(menu, 0);
			if (frontmap != NULL) {
				numitems = CountMItems(menu);
				for (i = 1; i <= numitems; ++i) {
					t = frontmap->auxterraintypes[i];
					if (any_aux_terrain_defined() && aux_terrain_defined(t)) {
						EnableItem(menu, i);
					    CheckItem(menu, i, frontmap->drawauxterrain[t]);
					} else {
					    CheckItem(menu, i, FALSE);
						DisableItem(menu, i);
					}
				}
			}
		} else {
			DisableItem(menu, 0);
		}
	}
	menu = GetMHandle(mAITypes);
	if (menu != nil) {
		/* Always on. */
		EnableItem(menu, 0);
		CheckItem(menu, 1, (!side_has_ai(dside)));
		CheckItem(menu, 2, (side_has_ai(dside)));
	}
	/* Everything has been tweaked, redraw the menu bar. */
	DrawMenuBar();
}

/* Tweak the command menu according to what the unit can do. */
/* (some of these should have the menu text tweaked also, terms weird sometimes) */

static void
enable_commands_for_unit(MenuHandle menu, Unit *unit)
{
	int u = unit->type;

	/* If we can select the unit at all, we can always get a closeup of it. */
	EnableItem(menu, miPlayCloseup);
	/* (how to do checkmarks if multiple units selected? use '-' as per HIG) */
	CheckItem(menu, miPlayCloseup, (find_unit_closeup(unit) != NULL));
	/* If we don't actually have any control over the unit, or the game is over,
	   nothing more to do. */
	if (!side_controls_unit(dside, unit) || endofgame)
	  return;
	if (can_move_at_all(unit)) {
		EnableItem(menu, miPlayMove);
		/* also check for places to return to? */
		EnableItem(menu, miPlayReturn);
	}
	if (unit->plan) {
		EnableItem(menu, miPlaySetFormation);
		EnableItem(menu, miPlayWake);
		CheckItem(menu, miPlayWake, !unit->plan->asleep);
		EnableItem(menu, miPlaySleep);
		CheckItem(menu, miPlaySleep, unit->plan->asleep);
		EnableItem(menu, miPlayReserve);
		CheckItem(menu, miPlayReserve, unit->plan->reserve);
		EnableItem(menu, miPlayDelay);
		CheckItem(menu, miPlayDelay, unit->plan->delayed);
	}
	if (can_create(unit) || can_complete(unit) || can_research(unit)) {
		EnableItem(menu, miPlayBuild);
	}
	if (can_repair(unit)) {
		EnableItem(menu, miPlayRepair);
	}
	if (can_attack(unit)) {
		EnableItem(menu, miPlayAttack);
		EnableItem(menu, miPlayOverrun);
	}
	if (can_fire(unit)) {
		EnableItem(menu, miPlayFire);
		EnableItem(menu, miPlayFireInto);
	}
	if (can_detonate(unit)) {
		EnableItem(menu, miPlayDetonate);
	}
	if (1 /*dside->designer*/) {
		EnableItem(menu, miPlayGive);
	}
	if (side_can_disband(dside, unit)) {
		EnableItem(menu, miPlayDisband);
	}
#if 0
	/* change-type menu item not yet defined */ 
	if (can_change_type(unit)) {
		EnableItem(menu, miPlayChangeType);
	}
#endif
	if (can_add_terrain(unit)) {
		EnableItem(menu, miPlayAddTerrain);
	}
	if (can_remove_terrain(unit)) {
		EnableItem(menu, miPlayRemoveTerrain);
	}
	if (1 /* !unitnameslocked */) {
		EnableItem(menu, miPlayRename);
	}
}
