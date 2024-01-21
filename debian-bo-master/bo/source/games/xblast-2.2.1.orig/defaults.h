/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th 1997
 * started August 1993
 *
 * File: xdefaults.h 
 * default settings for xblast
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public Licences as published
 * by the Free Software Foundation; either version 2; or (at your option)
 * any later version
 *
 * This program is distributed in the hope that it will entertaining,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILTY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Publis License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */


#ifndef _XDEFAULTS_H
#define _XDEFAULTS_H
/*
 * file for setups and defaults
 */
static char *file_appl_def   ="/usr/lib/X11/app-defaults/XBlast";
static char *file_setup      ="/.xblast";
static char *file_setup_dir  ="/.xblast-setups";
/*
 * fallbacks for game config
 */
static BMEntry arg_settings[] = {
  { D_TeamMode,              "None" },
#ifdef XBLAST_SOUND
  { D_SoundServer,           "stereo" },
#endif
  /* terminator */
  { NULL, NULL },
};

/*
 * fallbacks for game setup
 */
static BMEntry default_settings[] = {
  /* default player names */
  { D_Player1,               "Olli" },
  { D_Player2,               "Norbert" },
  { D_Player3,               "Rodi" },
  { D_Player4,               "Harald" },
  { D_Player5,               "Susi" },
  { D_Player6,               "Garth" },
  /* game configs */
  { D_StartingLevel,         "0" },
  { D_NumberOfLives,         "3" },
  { D_NumberOfVictories,     "5" },
  { D_FrameRate,             "20" },
  { D_Random,                "off" },
  { D_AllowColorMode,        "true" },
  { D_ForceOverride,         "false" },
  { D_Fork,                  "true" },
  { D_BellSound,             "true" },
  { D_PrintStat,             "false" },
  { D_LoadDefaults,          "false" },
  { D_SaveDefaults,          "false" },
  /* all levels selected */
  { D_UseLevel,              "true" },
  /* default player strings */
  { D_PlayerWinGame,         "CONGRATULATIONS" },
  /* default colors player 1 */
  { D_PlayerHelmetColor1,    "Turquoise" },
  { D_PlayerFaceColor1,      "LightSalmon" },
  { D_PlayerBodyColor1,      "White" },
  { D_PlayerHandsFeetColor1, "Gray75" },
  { D_PlayerArmsLegsColor1,  "Gray25" },
  { D_PlayerBackpackColor1,  "RoyalBlue" },
  /* default colors player 2 */
  { D_PlayerHelmetColor2,    "MidnightBlue" },
  { D_PlayerFaceColor2,      "LightSalmon" },
  { D_PlayerBodyColor2,      "NavyBlue" },
  { D_PlayerHandsFeetColor2, "Red" },
  { D_PlayerArmsLegsColor2,  "RoyalBlue" },
  { D_PlayerBackpackColor2,  "Gold" },
  /* default colors player 3 */
  { D_PlayerHelmetColor3,    "Red" },
  { D_PlayerFaceColor3,      "LightSalmon" },
  { D_PlayerBodyColor3,      "Red" },
  { D_PlayerHandsFeetColor3, "ForestGreen" },
  { D_PlayerArmsLegsColor3,  "IndianRed" },
  { D_PlayerBackpackColor3,  "DarkSeaGreen" },
  /* default colors player 4 */
  { D_PlayerHelmetColor4,    "Yellow" },
  { D_PlayerFaceColor4,      "LightSalmon" },
  { D_PlayerBodyColor4,      "SpringGreen" },
  { D_PlayerHandsFeetColor4, "OrangeRed" },
  { D_PlayerArmsLegsColor4,  "LightYellow" },
  { D_PlayerBackpackColor4,  "RoyalBlue" },
  /* default colors player 5 */
  { D_PlayerHelmetColor5,    "DeepPink" },
  { D_PlayerFaceColor5,      "LightSalmon" },
  { D_PlayerBodyColor5,      "Orchid" },
  { D_PlayerHandsFeetColor5, "SpringGreen" },
  { D_PlayerArmsLegsColor5,  "RoyalBlue" },
  { D_PlayerBackpackColor5,  "DeepPink" },
  /* default colors player 6 */
  { D_PlayerHelmetColor6,    "OliveDrab" },
  { D_PlayerFaceColor6,      "LightSalmon" },
  { D_PlayerBodyColor6,      "DarkOliveGreen" },
  { D_PlayerHandsFeetColor6, "OrangeRed" },
  { D_PlayerArmsLegsColor6,  "DarkKhaki" },
  { D_PlayerBackpackColor6,  "Firebrick" },
#ifdef DEBUG
  /* debugging resources */
  { D_NoTitle,               "False" },
#endif
  /* terminator */
  { NULL,                 NULL },
};

/*
 * fallbacks for display setup 
 */
static BMEntry disp_default_setting[] = {
  /* color mode */
  { D_ColorMode,        "true" },
  { D_Override,         "false" },
  /* fonts */
#ifdef FORCE_USE_FIXED_FONTS
  { D_LargeFont, "fixed"},
  { D_MediumFont, "fixed"},
  { D_SmallFont, "fixed"},
#else
  { D_LargeFont,        "-*-helvetica-bold-r-*-*-24-*-*-*-*-*-iso8859-*" },
  { D_MediumFont,       "-*-helvetica-bold-r-*-*-18-*-*-*-*-*-iso8859-*" },
  { D_SmallFont,        "-*-helvetica-bold-r-*-*-14-*-*-*-*-*-iso8859-*" },
#endif
  /* colors */
  { D_TitleColor1,      "SpringGreen" },
  { D_TitleColor2,      "Cyan" },
  { D_LightTextColor1,  "Yellow" },
  { D_LightTextColor2,  "Gold" },
  { D_DarkTextColor1,   "Black" },
  { D_DarkTextColor2,   "MidnightBlue" },
  { D_StatusLedColor,   "SpringGreen" },
  { D_StatusForeground, "LightSteelBlue" },
  { D_StatusBackground, "Black" },
  { D_ExplosionColor1,  "OrangeRed" },
  { D_ExplosionColor2,  "LightYellow" },
  { D_ExplosionColor3,  "White" },
  { D_BombColor,        "DarkSlateGray" },
  /* single player keys */
  { D_SinglePlayerPauseKey,       "KP_Subtract P" },
  { D_SinglePlayerUpKey,          "KP_8 KP_Up Up T" },
  { D_SinglePlayerLeftKey,        "KP_4 KP_Left Left F" },
  { D_SinglePlayerDownKey,        "KP_2 KP_Down Down v B" },
  { D_SinglePlayerRightKey,       "KP_6 KP_Right Right H" },
  { D_SinglePlayerStopKey,        "KP_5 KP_Begin Begin G" },
  { D_SinglePlayerBombKey,        "KP_0 KP_Insert Insert space" },
  { D_SinglePlayerSpecialKey,     "Return KP_Add Tab" },
  { D_SinglePlayerAbortKey,       "KP_Multiply A" },
  { D_SinglePlayerAbortCancelKey, "KP_Divide Z" },
  /* right player keys */
  { D_RightPlayerPauseKey,       "KP_Subtract" },
  { D_RightPlayerUpKey,          "KP_8 KP_Up Up" },
  { D_RightPlayerLeftKey,        "KP_4 KP_Left Left" },
  { D_RightPlayerDownKey,        "KP_2 KP_Down Down" },
  { D_RightPlayerRightKey,       "KP_6 KP_Right Right" },
  { D_RightPlayerStopKey,        "KP_5 KP_Begin Begin" },
  { D_RightPlayerBombKey,        "KP_0 KP_Insert Insert" },
  { D_RightPlayerSpecialKey,     "Return KP_Add" },
  { D_RightPlayerAbortKey,       "KP_Multiply" },
  { D_RightPlayerAbortCancelKey, "KP_Divide" },
  /* left player keys */
  { D_LeftPlayerPauseKey,       "P" },
  { D_LeftPlayerUpKey,          "T" },
  { D_LeftPlayerLeftKey,        "F" },
  { D_LeftPlayerDownKey,        "V B" },
  { D_LeftPlayerRightKey,       "H" },
  { D_LeftPlayerStopKey,        "G" },
  { D_LeftPlayerBombKey,        "space" },
  { D_LeftPlayerSpecialKey,     "Tab" },
  { D_LeftPlayerAbortKey,       "A" },
  { D_LeftPlayerAbortCancelKey, "Y" },
  /* terminator */
  { NULL, NULL },
};

/* 
 * command line args for xrm data base 
 */
#ifndef DEBUG
#ifdef XBLAST_SOUND
static int arg_table_entries = 42;
#else
static int arg_table_entries = 39;
#endif
#else
#ifdef XBLAST_SOUND
static int arg_table_entries = 44;
#else
static int arg_table_entries = 42;
#endif
#endif
static XrmOptionDescRec arg_table[] = {
  { "-display", W_Display,              XrmoptionSepArg,    (caddr_t) NULL}, 
  { "-xrm",     NULL,                   XrmoptionResArg,    (caddr_t) NULL}, 
  { "-single",  W_TeamMode,             XrmoptionNoArg,     (caddr_t) "None"}, 
  { "-team",    W_TeamMode,             XrmoptionNoArg,     (caddr_t) "Team"}, 
  { "-double",  W_TeamMode,             XrmoptionNoArg,     (caddr_t) "Double"}, 
  { "-l",       W_StartingLevel,        XrmoptionSepArg,    (caddr_t) NULL},
  { "-L",       W_NumberOfLives,        XrmoptionSepArg,    (caddr_t) NULL},
  { "-v",       W_NumberOfVictories,    XrmoptionSepArg,    (caddr_t) NULL},
  { "-f",       W_FrameRate,            XrmoptionSepArg,    (caddr_t) NULL},
  { "-F",       W_Fork,                 XrmoptionNoArg,     (caddr_t) "false"},
  { "+F",       W_Fork,                 XrmoptionNoArg,     (caddr_t) "true"},
  { "-r",       W_Random,               XrmoptionNoArg,     (caddr_t) "on"},
  { "+r",       W_Random,               XrmoptionNoArg,     (caddr_t) "off"},
  { "-rp",      W_RandomPlayerPosition, XrmoptionNoArg,     (caddr_t) "on"},
  { "+rp",      W_RandomPlayerPosition, XrmoptionNoArg,     (caddr_t) "off"},  
  { "-rl",      W_RandomLevelOrder,     XrmoptionNoArg,     (caddr_t) "on"},
  { "+rl",      W_RandomLevelOrder,     XrmoptionNoArg,     (caddr_t) "off"},
  { "-bw",      W_AllowColorMode,       XrmoptionNoArg,     (caddr_t) "false"},
  { "+bw",      W_AllowColorMode,       XrmoptionNoArg,     (caddr_t) "true"},
  { "-wm",      W_ForceOverride,        XrmoptionNoArg,     (caddr_t) "true"},
  { "+wm",      W_ForceOverride,        XrmoptionNoArg,     (caddr_t) "false"},
  { "-q",       W_BellSound,            XrmoptionNoArg,     (caddr_t) "false"},
  { "+q",       W_BellSound,            XrmoptionNoArg,     (caddr_t) "true"},
  { "-P",       W_PrintStat,            XrmoptionNoArg,     (caddr_t) "true"},
  { "+P",       W_PrintStat,            XrmoptionNoArg,     (caddr_t) "false"},
  { "-D",       W_LoadDefaults,         XrmoptionNoArg,     (caddr_t) "true"},
  { "+D",       W_LoadDefaults,         XrmoptionNoArg,     (caddr_t) "false"},
  { "-D=",      W_LoadNamedDefaults,    XrmoptionStickyArg, (caddr_t) NULL},
  { "-S=",      W_SaveNamedDefaults,    XrmoptionStickyArg, (caddr_t) NULL},
  { "-S",       W_SaveDefaults,         XrmoptionNoArg,     (caddr_t) "true" },
  { "+S",       W_SaveDefaults,         XrmoptionNoArg,     (caddr_t) "false"},
  { "-?",       W_PrintHelp,            XrmoptionNoArg,     (caddr_t) "true"},
  { "-h",       W_PrintHelp,            XrmoptionNoArg,     (caddr_t) "true"},
  { "-s",       W_PrintLevels,          XrmoptionNoArg,     (caddr_t) "true"},
  { "-stcl",    W_PrintLevelsTcl,       XrmoptionNoArg,     (caddr_t) "true"},
#ifdef XBLAST_SOUND
  /* option for sound server */
  { "-stereo",  W_SoundServer,          XrmoptionNoArg,     (caddr_t) "stereo" },
  { "-mono",    W_SoundServer,          XrmoptionNoArg,     (caddr_t) "mono" },
  { "-nosound", W_SoundServer,          XrmoptionNoArg,     (caddr_t) "none" },
#endif
#ifdef DEBUG
  /* options for debugging */
  { "-notitle", W_NoTitle,              XrmoptionNoArg,     (caddr_t) "true" },
  { "-title",   W_NoTitle,              XrmoptionNoArg,     (caddr_t) "false" },
#endif
  /* option to be ignored by parseCommand */
  { "-u",       NULL,                   XrmoptionSkipLine,  (caddr_t) NULL}, 
  { "-U",       NULL,                   XrmoptionSkipLine,  (caddr_t) NULL}, 
  { "+u",       NULL,                   XrmoptionSkipLine,  (caddr_t) NULL}, 
  { "+U",       NULL,                   XrmoptionSkipLine,  (caddr_t) NULL}, 
};

#endif
/*
 * end of file xdefaults.h
 */
