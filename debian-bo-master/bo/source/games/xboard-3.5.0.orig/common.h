/*
 * common.h -- Common definitions for X and Windows NT versions of XBoard
 * $Id: common.h,v 1.41 1996/12/19 22:42:52 mann Exp $
 *
 * Copyright 1991 by Digital Equipment Corporation, Maynard, Massachusetts.
 * Enhancements Copyright 1992-95 Free Software Foundation, Inc.
 *
 * The following terms apply to Digital Equipment Corporation's copyright
 * interest in XBoard:
 * ------------------------------------------------------------------------
 * All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * ------------------------------------------------------------------------
 *
 * The following terms apply to the enhanced version of XBoard distributed
 * by the Free Software Foundation:
 * ------------------------------------------------------------------------
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 * ------------------------------------------------------------------------
 */

#ifndef _COMMON
#define _COMMON

/* Begin compatibility grunge  */

#if defined(__STDC__) || defined(WIN32) || defined(_amigados)
#define	P(args)	args
typedef void *VOIDSTAR;
#else
#define P(args)		()
typedef char *VOIDSTAR;
#endif

#ifdef WIN32
typedef char Boolean;
typedef char *String;
#define popen _popen
#define pclose _pclose

#else
#ifdef _amigados        /*  It is important, that these types have  */
typedef int Boolean;    /*  a length of 4 bytes each, as we are     */
typedef char *String;   /*  using ReadArgs() for argument parsing.  */
#ifdef _DCC
FILE *popen(const char *, const char *);
int pclose(FILE *);
#endif

#else
#include <X11/Intrinsic.h>
#endif
#endif


#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

/* End compatibility grunge */


#define BOARD_SIZE		8
#define DROP_RANK               -3
#define MAX_MOVES		1000
#define MSG_SIZ			512
#define DIALOG_SIZE		256
#define STAR_MATCH_N            16
#define MOVE_LEN		32
#define TIME_CONTROL		"5"	/* in minutes */
#define TIME_DELAY_QUOTE	"1.0"	/* seconds between moves */
#define TIME_DELAY              ((float) 1.0)
#define MOVES_PER_SESSION	40	/* moves per TIME_CONTROL */
#define TIME_INCREMENT          -1      /* if >= 0, MOVES_PER_SESSION unused */
#define WhiteOnMove(move)	(((move) % 2) == 0)
#define ICS_HOST                "chessclub.com"
#define ICS_PORT	        "5000"
#define ICS_COMM_PORT           ""
#define FIRST_HOST		"localhost"
#define SECOND_HOST		"localhost"
#define TELNET_PROGRAM          "telnet"
#define MATCH_MODE		"False"
#define INIT_STRING		"new\nbeep\nrandom\neasy\n"
#define WHITE_STRING		"white\ngo\n"
#define BLACK_STRING		"black\ngo\n"
#define DEFAULT_SIZE            "Large"
#define WHITE_PIECE_COLOR	"#FFFFCC"
#define BLACK_PIECE_COLOR	"#202020"
#define LIGHT_SQUARE_COLOR	"#C8C365"
#define DARK_SQUARE_COLOR	"#77A26D"
#define BELLCHAR                '\007'
#define NULLCHAR                '\000'
#define COLOR_SHOUT             "green"
#define COLOR_SSHOUT            "green,black,1"
#define COLOR_CHANNEL1          "cyan"
#define COLOR_CHANNEL           "cyan,black,1"
#define COLOR_KIBITZ            "magenta,black,1"
#define COLOR_TELL              "yellow,black,1"
#define COLOR_CHALLENGE         "red,black,1"
#define COLOR_NORMAL            "default"

typedef enum {
    BeginningOfGame, MachinePlaysWhite, MachinePlaysBlack,
	AnalyzeMode, AnalyzeFile, TwoMachinesPlay,
    EditGame, PlayFromGameFile, EndOfGame, EditPosition,
    IcsIdle, IcsPlayingWhite, IcsPlayingBlack, IcsObserving,
    IcsExamining
  } GameMode;

typedef enum {
    WhitePawn, WhiteKnight, WhiteBishop, WhiteRook, WhiteQueen, WhiteKing,
    BlackPawn, BlackKnight, BlackBishop, BlackRook, BlackQueen, BlackKing,
    EmptySquare,
    ClearBoard, WhitePlay, BlackPlay /*for use on EditPosition menus*/
  } ChessSquare;

typedef ChessSquare Board[BOARD_SIZE][BOARD_SIZE];

typedef enum {
    WhiteKingSideCastle = 1, WhiteQueenSideCastle,
    WhiteKingSideCastleWild, WhiteQueenSideCastleWild,
    WhitePromotionKnight, WhitePromotionBishop,
    WhitePromotionRook, WhitePromotionQueen,
    BlackPromotionKnight, BlackPromotionBishop,
    BlackPromotionRook, BlackPromotionQueen,
    BlackKingSideCastle, BlackQueenSideCastle,
    BlackKingSideCastleWild, BlackQueenSideCastleWild,
    WhiteCapturesEnPassant, BlackCapturesEnPassant,
    WhiteDrop, BlackDrop, NormalMove,
    WhiteWins, BlackWins, GameIsDrawn, GameUnfinished,
    GNUChessGame, XBoardGame, MoveNumberOne, 
    BadMove, Comment, AmbiguousMove, PositionDiagram, ElapsedTime, PGNTag
  } ChessMove;

typedef struct {
#if !defined(_amigados)
    char *whitePieceColor;
    char *blackPieceColor;
    char *lightSquareColor;
    char *darkSquareColor;
#else
    int whitePieceColor;
    int blackPieceColor;
    int lightSquareColor;
    int darkSquareColor;
#endif
    int movesPerSession;
    int timeIncrement;
    char *initString;
    char *whiteString;
    char *blackString;
    char *firstChessProgram;
    char *secondChessProgram;
    Boolean noChessProgram;
    char *firstHost;
    char *secondHost;
    char *bitmapDirectory;
    char *remoteShell;
    char *remoteUser;
    float timeDelay;
    char *timeControl;
    Boolean icsActive;
    char *icsHost;
    char *icsPort;
    char *icsCommPort;  /* if set, use serial port instead of tcp host/port */
    char *icsLogon;     /* Hack to permit variable logon scripts. */
    Boolean icsInputBox;
    Boolean useTelnet;
    char *telnetProgram;
    char *gateway;
    char *loadGameFile;
    int loadGameIndex;      /* game # within file */
    char *saveGameFile;
    Boolean autoSaveGames;
    char *loadPositionFile;
    int loadPositionIndex;  /* position # within file */
    char *savePositionFile;
    Boolean matchMode;
    Boolean monoMode;
    Boolean debugMode;
    Boolean clockMode;
    char *boardSize;
    Boolean Iconic;
    char *searchTime;
    int searchDepth;
    Boolean showCoords;
    char *clockFont;
    char *messageFont;
    char *coordFont;
    Boolean ringBellAfterMoves;
    Boolean autoCallFlag;
    Boolean flipView;
    char *cmailGameName;
    Boolean alwaysPromoteToQueen;
    Boolean oldSaveStyle;
    Boolean quietPlay;
    Boolean showThinking;
    Boolean periodicUpdates;
    Boolean autoObserve;
    Boolean autoComment;
    Boolean getMoveList;
    Boolean checkLegality;
/**** These are used only by xboard: ********/
    int borderXoffset;
    int borderYoffset;
    Boolean titleInWindow;
/********************************************/
/**** Currently used only by WinBoard: ******/
    Boolean localEdit;
/********************************************/
    Boolean zippyTalk;
    Boolean zippyPlay;
    int flashCount;		/* Number of times to flash */
    int flashRate;		/* Flashes per second */
    Boolean clickClick;	/* If True, use click-click instead of
			   click-drag-release to move pieces */
    char *xpmDirectory; /* Path to XPM files to use */
    int msLoginDelay;  /* Delay per character (in msec) while sending
			  ICS logon script */

    Boolean colorize;	/* If True, use the following colors to color text */
    /* Strings for colors, as "fg, bg, bold" */
    char *colorShout;
    char *colorSShout;
    char *colorChannel1;
    char *colorChannel;
    char *colorKibitz;
    char *colorTell;
    char *colorChallenge;
    char *colorNormal;
} AppData, *AppDataPtr;

extern AppData appData;

typedef struct {
    /* PGN 7-tag info */
    char *event;
    char *site;
    char *date;
    char *round;
    char *white;
    char *black;
    ChessMove result;
    /* Additional info */
    char *fen;          /* NULL or FEN for starting position; input only */
    char *resultDetails;
    char *timeControl;
    char *extraTags;    /* NULL or "[Tag \"Value\"]\n", etc. */
    /* These are set from the ICS game-start messages, or from PGN tags */ 
    int whiteRating;
    int blackRating;
  /* OK - This is a kludge, so I'd better document it:
     The player ratings show up before the game creation
     message on the ICSes, so when the game is actually created,
     the ratings get zeroed out.
     If keep_ratings == 1, then ClearGameInfo() does NOT
     clear the ratings. If keep_ratings == 0, it DOES clear
     the ratings. ClearGameInfo() is the only routine that
     honors this, the others simply zero it out. */
    int keepRatings;
} GameInfo;


#endif
