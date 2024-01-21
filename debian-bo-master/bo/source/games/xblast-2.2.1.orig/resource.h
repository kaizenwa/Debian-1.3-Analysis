/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 26th, 1997
 * started August 1993
 *
 * File: xreource.h 
 * string definitons for resources
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


#ifndef _XRESOURCE_H
#define _XRESOURCE_H
/*
 * resource string constants
 *
 * W_ is the suffix for entries used for parseCommand
 * D_ is the suffix for entries used for default settings
 * N_ is the suffix for the correct name of the entry
 * C_ is the suffix for the correct class of the entry
 */

/* display names */
#define W_Display ".Display"
#define N_AtDisplay1  "xblast.display1"
#define N_AtDisplay2  "xblast.display2"
#define N_AtDisplay3  "xblast.display3"
#define N_AtDisplay4  "xblast.display4"
#define N_AtDisplay5  "xblast.display5"
#define N_AtDisplay6  "xblast.display6"
static char *N_AtDisplay[MAX_PLAYER] = {
  N_AtDisplay1,
  N_AtDisplay2,
  N_AtDisplay3,
  N_AtDisplay4,
  N_AtDisplay5,
  N_AtDisplay6,
};
#define D_AtDisplay1  "XBlast.display1"
#define D_AtDisplay2  "XBlast.display2"
#define D_AtDisplay3  "XBlast.display3"
#define D_AtDisplay4  "XBlast.display4"
#define D_AtDisplay5  "XBlast.display5"
#define D_AtDisplay6  "XBlast.display6"
static char *D_AtDisplay[MAX_PLAYER] = {
  D_AtDisplay1,
  D_AtDisplay2,
  D_AtDisplay3,
  D_AtDisplay4,
  D_AtDisplay5,
  D_AtDisplay6,
};
#define C_AtDisplay "XBlast.Display"
/* player names */
#define N_Player1 "xblast.player1"
#define N_Player2 "xblast.player2"
#define N_Player3 "xblast.player3"
#define N_Player4 "xblast.player4"
#define N_Player5 "xblast.player5"
#define N_Player6 "xblast.player6"
#define N_PlayerS "xblast.singlePlayer"
#define N_PlayerR "xblast.rightPlayer"
#define N_PlayerL "xblast.leftPlayer"

static char *N_Player[MAX_PLAYER+3] = {
  N_Player1,
  N_Player2,
  N_Player3,
  N_Player4,
  N_Player5,
  N_Player6,
  N_PlayerS,
  N_PlayerR,
  N_PlayerL,
};
#define D_Player1 "XBlast.player1"
#define D_Player2 "XBlast.player2"
#define D_Player3 "XBlast.player3"
#define D_Player4 "XBlast.player4"
#define D_Player5 "XBlast.player5"
#define D_Player6 "XBlast.player6"
static char *D_Player[MAX_PLAYER] = {
  D_Player1,
  D_Player2,
  D_Player3,
  D_Player4,
  D_Player5,
  D_Player6,
};
#define C_Player "XBlast.Player"
/* number of players */
#define W_NumberOfPlayers ".numberOfPlayers"
#define N_NumberOfPlayers "xblast.numberOfPlayers"
#define C_NumberOfPlayers "XBlast.NumberOfPlayers"
/* team mode */
#define W_TeamMode        ".teamMode"
#define D_TeamMode        "XBlast.teamMode"
#define N_TeamMode        "xblast.teamMode"
#define C_TeamMode        "XBlast.TeamMode"
/* starting level */
#define W_StartingLevel  ".startingLevel"
#define D_StartingLevel  "XBlast.startingLevel"
#define N_StartingLevel  "xblast.startingLevel"
#define C_StartingLevel  "XBlast.StartingLevel"
/* number of lives */
#define W_NumberOfLives  ".numberOfLives"
#define D_NumberOfLives  "XBlast.numberOfLives"
#define N_NumberOfLives  "xblast.numberOfLives"
#define C_NumberOfLives  "XBlast.NumberOfLives"
/* number of victories */
#define W_NumberOfVictories  ".numberOfVictories"
#define D_NumberOfVictories  "XBlast.numberOfVictories"
#define N_NumberOfVictories  "xblast.numberOfVictories"
#define C_NumberOfVictories  "XBlast.NumberOfVictories"
/* frame rate */
#define W_FrameRate  ".frameRate"
#define D_FrameRate  "XBlast.frameRate"
#define N_FrameRate  "xblast.frameRate"
#define C_FrameRate  "XBlast.FrameRate"
/* random modes */
#define W_Random                ".Random"
#define D_Random                "XBlast.Random"
#define C_Random                "XBlast.Random"
#define W_RandomPlayerPosition  ".randomPlayerPosition"
#define N_RandomPlayerPosition  "xblast.randomPlayerPosition"
#define W_RandomLevelOrder      ".randomLevelOrder"
#define N_RandomLevelOrder      "xblast.randomLevelOrder"
/* toggle color support */
#define W_AllowColorMode  ".allowColorMode"
#define D_AllowColorMode  "XBlast.allowColorMode"
#define N_AllowColorMode  "xblast.allowColorMode"
#define C_AllowColorMode  "XBlast.AllowColorMode"
/* force override window manager */
#define W_ForceOverride ".forceOverride"
#define D_ForceOverride "XBlast.forceOverride"
#define N_ForceOverride "xblast.forceOverride"
#define C_ForceOverride "XBlast.ForceOverride"
/* fork mode */
#define W_Fork ".fork"
#define D_Fork "XBlast.fork"
#define N_Fork "xblast.fork"
#define C_Fork "XBlast.Fork"
/* sound using bell */
#define W_BellSound  ".bellSound"
#define D_BellSound  "XBlast.bellSound"
#define N_BellSound  "xblast.bellSound"
#define C_BellSound  "XBlast.BellSound"
/* select levels to play */
#define D_UseLevel "XBlast.UseLevel"
#define C_UseLevel "XBlast.UseLevel"
/* print statistics for tcl */
#define W_PrintStat  ".printStat"
#define D_PrintStat  "XBlast.printStat"
#define N_PrintStat  "xblast.printStat"
#define C_PrintStat  "XBlast.PrintStat"
/* load defaults  */
#define W_LoadDefaults  ".loadDefaults"
#define D_LoadDefaults  "XBlast.loadDefaults"
#define N_LoadDefaults  "xblast.loadDefaults"
#define C_LoadDefaults  "XBlast.LoadDefaults"
/* save defaults */
#define W_SaveDefaults  ".saveDefaults"
#define D_SaveDefaults  "XBlast.saveDefaults"
#define N_SaveDefaults  "xblast.saveDefaults"
#define C_SaveDefaults  "XBlast.SaveDefaults"
/* load named defaults  */
#define W_LoadNamedDefaults  ".loadNamedDefaults"
#define D_LoadNamedDefaults  "XBlast.loadNamedDefaults"
#define N_LoadNamedDefaults  "xblast.loadNamedDefaults"
#define C_LoadNamedDefaults  "XBlast.LoadNamedDefaults"
/* save named defaults */
#define W_SaveNamedDefaults  ".saveNamedDefaults"
#define D_SaveNamedDefaults  "XBlast.saveNamedDefaults"
#define N_SaveNamedDefaults  "xblast.saveNamedDefaults"
#define C_SaveNamedDefaults  "XBlast.SaveNamedDefaults"
/* print usage and options */
#define W_PrintHelp  ".printHelp"
#define N_PrintHelp  "xblast.printHelp"
#define C_PrintHelp  "XBlast.PrintHelp"
/* print a list of levels */
#define W_PrintLevels  ".printLevels"
#define N_PrintLevels  "xblast.printLevels"
#define C_PrintLevels  "XBlast.PrintLevels"
/* print a list of levels for tkXBlast */
#define W_PrintLevelsTcl  ".printLevelsTcl"
#define N_PrintLevelsTcl  "xblast.printLevelsTcl"
#define C_PrintLevelsTcl  "XBlast.PrintLevelsTcl"

#ifdef DEBUG
/*
 * resource for debugging XBlast
 */
#define W_NoTitle ".noTitle"
#define D_NoTitle "XBlast.noTitle"
#define N_NoTitle "xblast.noTitle"
#define C_NoTitle "XBlast.NoTitle"
#endif

/*
 * display resources
 */
/* color mode */
#define N_ColorMode "xblast.colorMode"
#define D_ColorMode "XBlast.colorMode"
#define C_ColorMode "XBlast.ColorMode"
/* override window manager */
#define N_Override "xblast.override"
#define D_Override "Xblast.override"
#define C_Override "Xblast.Override"
/* fonts */
#define N_LargeFont  "xblast.largeFont"
#define D_LargeFont  "XBlast.largeFont"
#define C_LargeFont  "XBlast.Font"
#define N_MediumFont "xblast.mediumFont"
#define D_MediumFont "XBlast.mediumFont"
#define C_MediumFont "XBlast.Font"
#define N_SmallFont  "xblast.smallFont"
#define D_SmallFont  "XBlast.smallFont"
#define C_SmallFont  "XBlast.Font"
/* title background color 1 */
#define N_TitleColor1 "xblast.titleColor1"
#define D_TitleColor1 "XBlast.titleColor1"
#define C_TitleColor1 "XBlast.Background"
/* title bachground color 2 */
#define N_TitleColor2 "xblast.titleColor2"
#define D_TitleColor2 "XBlast.titleColor2"
#define C_TitleColor2 "XBlast.Background"
/* colors for light text */
#define N_LightTextColor1 "xblast.textBox.lightColor1"
#define N_LightTextColor2 "xblast.textBox.lightColor2"
#define D_LightTextColor1 "XBlast.textBox.lightColor1"
#define D_LightTextColor2 "XBlast.textBox.lightColor2"
#define C_LightTextColor  "XBlast.TextBox.Foreground"
/* colors for dark text */
#define N_DarkTextColor1  "xblast.textBox.darkColor1"
#define N_DarkTextColor2  "xblast.textBox.darkColor2"
#define D_DarkTextColor1  "XBlast.textBox.darkColor1"
#define D_DarkTextColor2  "XBlast.textBox.darkColor2"
#define C_DarkTextColor   "XBlast.TextBox.Background"
/* colors for status bar */
#define N_StatusLedColor   "xblast.statusBar.ledColor"
#define N_StatusForeground "xblast.statusBar.foreground"
#define N_StatusBackground "xblast.statusBar.background"
#define D_StatusLedColor   "XBlast.statusBar.ledColor"
#define D_StatusForeground "XBlast.statusBar.foreground"
#define D_StatusBackground "XBlast.statusBar.background"
#define C_StatusLedColor   "XBlast.StatusBar.Foreground"
#define C_StatusForeground "XBlast.StatusBar.Foreground"
#define C_StatusBackground "XBlast.StatusBar.Background"
/* colors for explosions */
#define N_ExplosionColor1  "xblast.explosionColor1"
#define N_ExplosionColor2  "xblast.explosionColor2"
#define N_ExplosionColor3  "xblast.explosionColor3"
#define D_ExplosionColor1  "XBlast.explosionColor1"
#define D_ExplosionColor2  "XBlast.explosionColor2"
#define D_ExplosionColor3  "XBlast.explosionColor3"
#define C_ExplosionColor1  "XBlast.ExplosionColor"
#define C_ExplosionColor2  "XBlast.ExplosionColor"
#define C_ExplosionColor3  "XBlast.ExplosionColor"
/* color for bombs */
#define N_BombColor "xblast.bombColor"
#define C_BombColor "XBlast.BombColor"
#define D_BombColor "XBlast.bombColor"

/* 
 * player resources
 */
/* win game message */
#define D_PlayerWinGame "XBlast.Player.winGameMsg"
/* win level messages */
static char *D_PlayerWinLevel[MAX_PLAYER] = {
  "XBlast.player1.winLevelMsg",
  "XBlast.player2.winLevelMsg",
  "XBlast.player3.winLevelMsg",
  "XBlast.player4.winLevelMsg",
  "XBlast.player5.winLevelMsg",
  "XBlast.player6.winLevelMsg",
};
static char *N_PlayerWinLevel[MAX_PLAYER+3] = {
  "xblast.player1.winLevelMsg",
  "xblast.player2.winLevelMsg",
  "xblast.player3.winLevelMsg",
  "xblast.player4.winLevelMsg",
  "xblast.player5.winLevelMsg",
  "xblast.player6.winLevelMsg",
  "xblast.singlePlayer.winLevelMsg",
  "xblast.rightPlayer.winLevelMsg",
  "xblast.leftPlayer.winLevelMsg",
};
#define C_PlayerWinLevel "XBlast.Player.WinLevelMsg"
/* win game messages */
static char *N_PlayerWinGame[MAX_PLAYER+3] = {
  "xblast.player1.winGameMsg",
  "xblast.player2.winGameMsg",
  "xblast.player3.winGameMsg",
  "xblast.player4.winGameMsg",
  "xblast.player5.winGameMsg",
  "xblast.player6.winGameMsg",
  "xblast.singlePlayer.winGameMsg",
  "xblast.rightPlayer.winGameMsg",
  "xblast.leftPlayer.winGameMsg",
};
#define C_PlayerWinGame "XBlast.Player.WinGameMsg"
/* lose leve messages */
static char *N_PlayerLoseLevel[MAX_PLAYER+3] = {
  "xblast.player1.loseLevelMsg",
  "xblast.player2.loseLevelMsg",
  "xblast.player3.loseLevelMsg",
  "xblast.player4.loseLevelMsg",
  "xblast.player5.loseLevelMsg",
  "xblast.player6.loseLevelMsg",
  "xblast.singlePlayer.loseLevelMsg",
  "xblast.rightPlayer.loseLevelMsg",
  "xblast.leftPlayer.loseLevelMsg",
};
#define C_PlayerLoseLevel "XBlast.Player.LoseLevelMsg"
/* lose life messages */
static char *N_PlayerLoseLife[MAX_PLAYER+3] = {
  "xblast.player1.loseLifeMsg",
  "xblast.player2.loseLifeMsg",
  "xblast.player3.loseLifeMsg",
  "xblast.player4.loseLifeMsg",
  "xblast.player5.loseLifeMsg",
  "xblast.player6.loseLifeMsg",
  "xblast.singlePlayer.loseLifeMsg",
  "xblast.rightPlayer.loseLifeMsg",
  "xblast.leftPlayer.loseLifeMsg",
};
#define C_PlayerLoseLife "XBlast.Player.LoseLifeMsg"
/* gloat messages */
static char *N_PlayerGloat[MAX_PLAYER+3] = {
  "xblast.player1.gloatMsg",
  "xblast.player2.gloatMsg",
  "xblast.player3.gloatMsg",
  "xblast.player4.gloatMsg",
  "xblast.player5.gloatMsg",
  "xblast.player6.gloatMsg",
  "xblast.singlePlayer.gloatMsg",
  "xblast.rightPlayer.gloatMsg",
  "xblast.leftPlayer.gloatMsg",
};
#define C_PlayerGloat "XBlast.Player.GloatMsg"
/* Welcome messages */
static char *N_PlayerWelcome[MAX_PLAYER+3] = {
  "xblast.player1.welcomeMsg",
  "xblast.player2.welcomeMsg",
  "xblast.player3.welcomeMsg",
  "xblast.player4.welcomeMsg",
  "xblast.player5.welcomeMsg",
  "xblast.player6.welcomeMsg",
  "xblast.singlePlayer.welcomeMsg",
  "xblast.rightPlayer.welcomeMsg",
  "xblast.leftPlayer.welcomeMsg",
};
#define C_PlayerWelcome "XBlast.Player.WelcomeMsg"
/* Abort messages */
static char *D_PlayerAbort[MAX_PLAYER] = {
  "XBlast.player1.abortMsg",
  "XBlast.player2.abortMsg",
  "XBlast.player3.abortMsg",
  "XBlast.player4.abortMsg",
  "XBlast.player5.abortMsg",
  "XBlast.player6.abortMsg",
};
static char *N_PlayerAbort[MAX_PLAYER+3] = {
  "xblast.player1.abortMsg",
  "xblast.player2.abortMsg",
  "xblast.player3.abortMsg",
  "xblast.player4.abortMsg",
  "xblast.player5.abortMsg",
  "xblast.player6.abortMsg",
  "xblast.singlePlayer.abortMsg",
  "xblast.rightPlayer.abortMsg",
  "xblast.leftPlayer.abortMsg",
};
#define C_PlayerAbort "XBlast.Player.AbortMsg"
/* abort cancel messages */
static char *D_PlayerAbortCancel[MAX_PLAYER] = {
  "XBlast.player1.abortCancelMsg",
  "XBlast.player2.abortCancelMsg",
  "XBlast.player3.abortCancelMsg",
  "XBlast.player4.abortCancelMsg",
  "XBlast.player5.abortCancelMsg",
  "XBlast.player6.abortCancelMsg",
};
static char *N_PlayerAbortCancel[MAX_PLAYER+3] = {
  "xblast.player1.abortCancelMsg",
  "xblast.player2.abortCancelMsg",
  "xblast.player3.abortCancelMsg",
  "xblast.player4.abortCancelMsg",
  "xblast.player5.abortCancelMsg",
  "xblast.player6.abortCancelMsg",
  "xblast.singlePlayer.abortCancelMsg",
  "xblast.rightPlayer.abortCancelMsg",
  "xblast.leftPlayer.abortCancelMsg",
};
#define C_PlayerAbortCancel "XBlast.Player.AbortCancelMsg"
/* player 1 color */
#define D_PlayerHelmetColor1    "XBlast.player1.helmetColor"
#define D_PlayerFaceColor1      "XBlast.player1.faceColor"
#define D_PlayerBodyColor1      "XBlast.player1.bodyColor"
#define D_PlayerHandsFeetColor1 "XBlast.player1.handsFeetColor"
#define D_PlayerArmsLegsColor1  "XBlast.player1.armsLegsColor"
#define D_PlayerBackpackColor1  "XBlast.player1.backpackColor"
/* player 2 color */
#define D_PlayerHelmetColor2    "XBlast.player2.helmetColor"
#define D_PlayerFaceColor2      "XBlast.player2.faceColor"
#define D_PlayerBodyColor2      "XBlast.player2.bodyColor"
#define D_PlayerHandsFeetColor2 "XBlast.player2.handsFeetColor"
#define D_PlayerArmsLegsColor2  "XBlast.player2.armsLegsColor"
#define D_PlayerBackpackColor2  "XBlast.player2.backpackColor"
/* player 3 color */
#define D_PlayerHelmetColor3    "XBlast.player3.helmetColor"
#define D_PlayerFaceColor3      "XBlast.player3.faceColor"
#define D_PlayerBodyColor3      "XBlast.player3.bodyColor"
#define D_PlayerHandsFeetColor3 "XBlast.player3.handsFeetColor"
#define D_PlayerArmsLegsColor3  "XBlast.player3.armsLegsColor"
#define D_PlayerBackpackColor3  "XBlast.player3.backpackColor"
/* player 4 color */
#define D_PlayerHelmetColor4    "XBlast.player4.helmetColor"
#define D_PlayerFaceColor4      "XBlast.player4.faceColor"
#define D_PlayerBodyColor4      "XBlast.player4.bodyColor"
#define D_PlayerHandsFeetColor4 "XBlast.player4.handsFeetColor"
#define D_PlayerArmsLegsColor4  "XBlast.player4.armsLegsColor"
#define D_PlayerBackpackColor4  "XBlast.player4.backpackColor"
/* player 5 color */
#define D_PlayerHelmetColor5    "XBlast.player5.helmetColor"
#define D_PlayerFaceColor5      "XBlast.player5.faceColor"
#define D_PlayerBodyColor5      "XBlast.player5.bodyColor"
#define D_PlayerHandsFeetColor5 "XBlast.player5.handsFeetColor"
#define D_PlayerArmsLegsColor5  "XBlast.player5.armsLegsColor"
#define D_PlayerBackpackColor5  "XBlast.player5.backpackColor"
/* player 6 color */
#define D_PlayerHelmetColor6    "XBlast.player6.helmetColor"
#define D_PlayerFaceColor6      "XBlast.player6.faceColor"
#define D_PlayerBodyColor6      "XBlast.player6.bodyColor"
#define D_PlayerHandsFeetColor6 "XBlast.player6.handsFeetColor"
#define D_PlayerArmsLegsColor6  "XBlast.player6.armsLegsColor"
#define D_PlayerBackpackColor6  "XBlast.player6.backpackColor"
/* all player color names */ 
static char *N_PlayerHelmetColor[MAX_PLAYER+3] = {
  "xblast.player1.helmetColor",
  "xblast.player2.helmetColor",
  "xblast.player3.helmetColor",
  "xblast.player4.helmetColor",
  "xblast.player5.helmetColor",
  "xblast.player6.helmetColor",
  "xblast.singlePlayer.helmetColor",
  "xblast.rightPlayer.helmetColor",
  "xblast.leftPlayer.helmetColor",
};
static char *N_PlayerFaceColor[MAX_PLAYER+3] = {
  "xblast.player1.faceColor",
  "xblast.player2.faceColor",
  "xblast.player3.faceColor",
  "xblast.player4.faceColor",
  "xblast.player5.faceColor",
  "xblast.player6.faceColor",
  "xblast.singlePlayer.faceColor",
  "xblast.rightPlayer.faceColor",
  "xblast.leftPlayer.faceColor",
};
static char *N_PlayerBodyColor[MAX_PLAYER+3] = {
  "xblast.player1.bodyColor",
  "xblast.player2.bodyColor",
  "xblast.player3.bodyColor",
  "xblast.player4.bodyColor",
  "xblast.player5.bodyColor",
  "xblast.player6.bodyColor",
  "xblast.singlePlayer.bodyColor",
  "xblast.rightPlayer.bodyColor",
  "xblast.leftPlayer.bodyColor",
};
static char *N_PlayerHandsFeetColor[MAX_PLAYER+3] = {
  "xblast.player1.handsFeetColor",
  "xblast.player2.handsFeetColor",
  "xblast.player3.handsFeetColor",
  "xblast.player4.handsFeetColor",
  "xblast.player5.handsFeetColor",
  "xblast.player6.handsFeetColor",
  "xblast.singlePlayer.handsFeetColor",
  "xblast.rightPlayer.handsFeetColor",
  "xblast.leftPlayer.handsFeetColor",
};
static char *N_PlayerArmsLegsColor[MAX_PLAYER+3] = {
  "xblast.player1.armsLegsColor",
  "xblast.player2.armsLegsColor",
  "xblast.player3.armsLegsColor",
  "xblast.player4.armsLegsColor",
  "xblast.player5.armsLegsColor",
  "xblast.player6.armsLegsColor",
  "xblast.singlePlayer.armsLegsColor",
  "xblast.rightPlayer.armsLegsColor",
  "xblast.leftPlayer.armsLegsColor",
};
static char *N_PlayerBackpackColor[MAX_PLAYER+3] = {
  "xblast.player1.backpackColor",
  "xblast.player2.backpackColor",
  "xblast.player3.backpackColor",
  "xblast.player4.backpackColor",
  "xblast.player5.backpackColor",
  "xblast.player6.backpackColor",
  "xblast.singlePlayer.backpackColor",
  "xblast.rightPlayer.backpackColor",
  "xblast.leftPlayer.backpackColor",
};

/* player color class */
#define C_PlayerColor  "XBlast.Player.Background"
#endif

/* player attributes array */
static char **N_PlayerAttributes[] = {
  /* name */
  N_Player,
  /* messages */
  N_PlayerWinLevel,
  N_PlayerWinGame,
  N_PlayerLoseLevel,
  N_PlayerLoseLife,
  N_PlayerGloat,
  N_PlayerWelcome,
  N_PlayerAbort,
  N_PlayerAbortCancel,
  /* colors */
  N_PlayerHelmetColor,
  N_PlayerFaceColor,
  N_PlayerBodyColor,
  N_PlayerHandsFeetColor,
  N_PlayerArmsLegsColor,
  N_PlayerBackpackColor,
  /* terminator */
  NULL,
};

/* player attributes array */
static char *C_PlayerAttributes[] = {
  /* name */
  C_Player,
  /* messages */
  C_PlayerWinLevel,
  C_PlayerWinGame,
  C_PlayerLoseLevel,
  C_PlayerLoseLife,
  C_PlayerGloat,
  C_PlayerWelcome,
  C_PlayerAbort,
  C_PlayerAbortCancel,
  /* colors */
  C_PlayerColor,
  C_PlayerColor,
  C_PlayerColor,
  C_PlayerColor,
  C_PlayerColor,
  C_PlayerColor,
  /* terminator */
  NULL,
};

/* player key resources */

/* single player */
#define D_SinglePlayerPauseKey       "XBlast.singlePlayer.pauseKey"
#define D_SinglePlayerUpKey          "XBlast.singlePlayer.upKey"
#define D_SinglePlayerLeftKey        "XBlast.singlePlayer.leftKey"
#define D_SinglePlayerDownKey        "XBlast.singlePlayer.downKey"
#define D_SinglePlayerRightKey       "XBlast.singlePlayer.rightKey"
#define D_SinglePlayerStopKey        "XBlast.singlePlayer.stopKey"
#define D_SinglePlayerBombKey        "XBlast.singlePlayer.bombKey"
#define D_SinglePlayerSpecialKey     "XBlast.singlePlayer.specialKey"
#define D_SinglePlayerAbortKey       "XBlast.singlePlayer.abortKey"
#define D_SinglePlayerAbortCancelKey "XBlast.singlePlayer.abortCancelKey"
/* right player */
#define D_RightPlayerPauseKey       "XBlast.rightPlayer.pauseKey"
#define D_RightPlayerUpKey          "XBlast.rightPlayer.upKey"
#define D_RightPlayerLeftKey        "XBlast.rightPlayer.leftKey"
#define D_RightPlayerDownKey        "XBlast.rightPlayer.downKey"
#define D_RightPlayerRightKey       "XBlast.rightPlayer.rightKey"
#define D_RightPlayerStopKey        "XBlast.rightPlayer.stopKey"
#define D_RightPlayerBombKey        "XBlast.rightPlayer.bombKey"
#define D_RightPlayerSpecialKey     "XBlast.rightPlayer.specialKey"
#define D_RightPlayerAbortKey       "XBlast.rightPlayer.abortKey"
#define D_RightPlayerAbortCancelKey "XBlast.rightPlayer.abortCancelKey"
/* left player */
#define D_LeftPlayerPauseKey       "XBlast.leftPlayer.pauseKey"
#define D_LeftPlayerUpKey          "XBlast.leftPlayer.upKey"
#define D_LeftPlayerLeftKey        "XBlast.leftPlayer.leftKey"
#define D_LeftPlayerDownKey        "XBlast.leftPlayer.downKey"
#define D_LeftPlayerRightKey       "XBlast.leftPlayer.rightKey"
#define D_LeftPlayerStopKey        "XBlast.leftPlayer.stopKey"
#define D_LeftPlayerBombKey        "XBlast.leftPlayer.bombKey"
#define D_LeftPlayerSpecialKey     "XBlast.leftPlayer.specialKey"
#define D_LeftPlayerAbortKey       "XBlast.leftPlayer.abortKey"
#define D_LeftPlayerAbortCancelKey "XBlast.leftPlayer.abortCancelKey"

/* all player key names */
static char* N_PlayerPauseKey[3] = {
  "xblast.singlePlayer.pauseKey",
  "xblast.rightPlayer.pauseKey",
  "xblast.leftPlayer.pauseKey",
};
static char* N_PlayerUpKey[3] = {
  "xblast.singlePlayer.upKey",
  "xblast.rightPlayer.upKey",
  "xblast.leftPlayer.upKey",
};
static char* N_PlayerLeftKey[3] = {
  "xblast.singlePlayer.leftKey",
  "xblast.rightPlayer.leftKey",
  "xblast.leftPlayer.leftKey",
};
static char* N_PlayerRightKey[3] = {
  "xblast.singlePlayer.rightKey",
  "xblast.rightPlayer.rightKey",
  "xblast.leftPlayer.rightKey",
};
static char* N_PlayerDownKey[3] = {
  "xblast.singlePlayer.downKey",
  "xblast.rightPlayer.downKey",
  "xblast.leftPlayer.downKey",
};
static char* N_PlayerStopKey[3] = {
  "xblast.singlePlayer.stopKey",
  "xblast.rightPlayer.stopKey",
  "xblast.leftPlayer.stopKey",
};
static char* N_PlayerBombKey[3] = {
  "xblast.singlePlayer.bombKey",
  "xblast.rightPlayer.bombKey",
  "xblast.leftPlayer.bombKey",
};
static char* N_PlayerSpecialKey[3] = {
  "xblast.singlePlayer.specialKey",
  "xblast.rightPlayer.specialKey",
  "xblast.leftPlayer.specialKey",
};
static char* N_PlayerAbortKey[3] = {
  "xblast.singlePlayer.abortKey",
  "xblast.rightPlayer.abortKey",
  "xblast.leftPlayer.abortKey",
};
static char* N_PlayerAbortCancelKey[3] = {
  "xblast.singlePlayer.abortCancelKey",
  "xblast.rightPlayer.abortCancelKey",
  "xblast.leftPlayer.abortCancelKey",
};

/* classes for keys */
#define C_PlayerPauseKey       "XBlast.Player.PauseKey"
#define C_PlayerUpKey          "XBlast.Player.UpKey"
#define C_PlayerLeftKey        "XBlast.Player.LeftKey"
#define C_PlayerRightKey       "XBlast.Player.RightKey"
#define C_PlayerDownKey        "XBlast.Player.DownKey"
#define C_PlayerStopKey        "XBlast.Player.StopKey"
#define C_PlayerBombKey        "XBlast.Player.BombKey"
#define C_PlayerSpecialKey     "XBlast.Player.SpecialKey"
#define C_PlayerAbortKey       "XBlast.Player.AbortKey"
#define C_PlayerAbortCancelKey "XBlast.Player.AbortCancelKey"

#ifdef XBLAST_SOUND
/*
 * sound server resources
 */
#define W_SoundServer ".soundServer"
#define D_SoundServer "XBlast.soundServer"
#define N_SoundServer "xblast.soundServer"
#define C_SoundServer "XBlast.SoundServer"
#endif

/*
 * end of file
 */




