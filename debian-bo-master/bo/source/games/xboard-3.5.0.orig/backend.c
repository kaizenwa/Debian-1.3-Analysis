/*
 * backend.c -- Common back end for X and Windows NT versions of
 * XBoard $Id: backend.c,v 1.78 1997/01/01 23:40:46 mann Exp $
 *
 * Copyright 1991 by Digital Equipment Corporation, Maynard, Massachusetts.
 * Enhancements Copyright 1992-96 Free Software Foundation, Inc.
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
 *
 * See the file ChangeLog for a revision history.  */

#include <config.h>

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <math.h>

#if STDC_HEADERS
# include <stdlib.h>
# include <string.h>
#else /* not STDC_HEADERS */
# if HAVE_STRING_H
#  include <string.h>
# else /* not HAVE_STRING_H */
#  include <strings.h>
# endif /* not HAVE_STRING_H */
#endif /* not STDC_HEADERS */

#if HAVE_SYS_FCNTL_H
# include <sys/fcntl.h>
#else /* not HAVE_SYS_FCNTL_H */
# if HAVE_FCNTL_H
#  include <fcntl.h>
# endif /* HAVE_FCNTL_H */
#endif /* not HAVE_SYS_FCNTL_H */

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#if defined(_amigados) && !defined(__GNUC__)
struct timezone {
    int tz_minuteswest;
    int tz_dsttime;
};
extern int gettimeofday(struct timeval *, struct timezone *);
#endif

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#include "common.h"
#include "frontend.h"
#include "backend.h"
#include "parser.h"
#include "moves.h"
#ifdef ZIPPY
# include "zippy.h"
#endif
#include "backendz.h"

/* A point in time */
typedef struct {
    long sec;  /* Assuming this is >= 32 bits */
    int ms;    /* Assuming this is >= 16 bits */
} TimeMark;

/* Search stats from chessprogram */
typedef struct {
  char movelist[MSG_SIZ]; /* Last PV we were sent */
  int depth;              /* Current search depth */
  int nr_moves;           /* Total nr of root moves */
  int moves_left;         /* Moves remaining to be searched */
  int nodes;              /* # of nodes searched */
  int time;               /* Search time (seconds * 10) */
  int score;              /* Score * 100 */
  int got_only_move;      /* If last msg was "(only move)" */
  int got_fail;           /* 0 - nothing, 1 - got "--", 2 - got "++" */
  int ok_to_send;         /* handshaking between send & recv */
  int line_is_book;       /* 1 if movelist is book moves */
  int seen_stat;          /* 1 if we've seen the stat01: line */
} ChessProgramStats;

int establish P((void));
void read_from_player P((InputSourceRef isr, char *buf, int count, int error));
void read_from_ics P((InputSourceRef isr, char *buf, int count, int error));
void SendToICS P((char *s));
void SendToICSDelayed P((char *s, long msdelay));
void SendMoveToICS P((ChessMove moveType, int fromX, int fromY,
		      int toX, int toY));
Boolean ParseMachineMove P((char *machineMove, int moveNum,
			    ChessMove *moveType, int *fromX, int *fromY,
			    int *toX, int *toY, char *promoChar));
void InitPosition P((int redraw));
void SendCurrentBoard P((ProcRef pr));
void SendBoard P((ProcRef pr, Board board));
void FinishUserMove P((ChessMove moveType, int toX, int toY));
void HandleMachineMove P((char *message, InputSourceRef isr));
void LoadGameLoop P((void));
int LoadGameOneMove P((void));
int LoadGameFromFile P((char *filename, int n, char *title, int useList));
int LoadPositionFromFile P((char *filename, int n, char *title));
int SaveGameToFile P((char *filename));
int SavePositionToFile P((char *filename));
void ApplyMove P((ChessMove *moveType, int fromX, int fromY,
		  int toX, int toY, Board board));
void MakeMove P((ChessMove *moveType, int fromX, int fromY,
		 int toX, int toY));
void BackwardInner P((int target));
void ForwardInner P((int target));
void GameEnds P((ChessMove result, char *resultDetails, int whosays));
void Reset P((int redraw));
void EditPositionDone P((void));
void PrintOpponents P((FILE *fp));
void PrintPosition P((FILE *fp, int move));
void InitChessProgram P((char *hostName, char *programName,
			 ProcRef *pr, InputSourceRef *isr, int *sendTime));
void SendToProgram P((char *message, ProcRef pr));
void ReceiveFromProgram P((InputSourceRef isr, char *buf, int count, int error));
void SendTimeControl P((ProcRef pr, int mps, long tc, int inc, int sd,
			int st));
void SendTimeRemaining P((ProcRef pr));
void SendMoveToProgram P((ChessMove moveType, int fromX, int fromY, int toX,
			  int toY, ProcRef firstProgramPR, int sendTime));
void Attention P((ProcRef pr));
void ResurrectChessProgram P((void));
void DisplayComment P((int moveNumber, char *text));
void DisplayMove P((int moveNumber));
void DisplayAnalysis P((void));

void ParseGameHistory P((char *game));
void ParseBoard12 P((char *string));
void StartClocks P((void));
void DisplayBothClocks P((void));
void SwitchClocks P((void));
void StopClocks P((void));
void ResetClocks P((void));
char *PGNDate P((void));
void SetGameInfo P((void));
char *PositionToFEN P((int move));
Boolean ParseFEN P((Board board, int *blackPlaysFirst, char *fen));
int RegisterMove P((void));
void MakeRegisteredMove P((void));
void TruncateGame P((void));
int looking_at P((char *, int *, char *));
void CopyPlayerNameIntoFileName P((char **, char *));
char *SavePart P((char *));
int SaveGameOldStyle P((FILE *));
int SaveGamePGN P((FILE *));
char *StrSave P((char *));
void GetTimeMark P((TimeMark *));
long SubtractTimeMarks P((TimeMark *, TimeMark *));
void CheckFlags P((void));
long NextTickLength P((long));
void CheckTimeControl P((void));
void show_bytes P((FILE *, char *, int));
char *CmailMsg P((void));
int string_to_rating P((char *str));

extern int tinyLayout;
static ChessProgramStats programStats;

/* States for ics_getting_history */
#define H_FALSE 0
#define H_REQUESTED 1
#define H_GOT_REQ_HEADER 2
#define H_GOT_UNREQ_HEADER 3
#define H_GETTING_MOVES 4

/* whosays values for GameEnds */
#define GE_ICS 0
#define GE_GNU 1
#define GE_PLAYER 2
#define GE_FILE 3

/* Maximum number of games in a cmail message */
#define CMAIL_MAX_GAMES 20

/* Different types of move when calling RegisterMove */
#define CMAIL_MOVE   0
#define CMAIL_RESIGN 1
#define CMAIL_DRAW   2
#define CMAIL_ACCEPT 3

/* Different types of result to remember for each game */
#define CMAIL_NOT_RESULT 0
#define CMAIL_OLD_RESULT 1
#define CMAIL_NEW_RESULT 2

/* Fake up flags for now, as we aren't keeping track of castling
   availability yet */
#define FakeFlags(index) \
    (((((index) % 2) == 0) ? F_WHITE_ON_MOVE : 0) | F_ALL_CASTLE_OK)

FILE *gameFileFP, *fromUserFP, *toUserFP, *debugFP;

char cmailMove[CMAIL_MAX_GAMES][MOVE_LEN], cmailMsg[MSG_SIZ];
char bookOutput[MSG_SIZ], thinkOutput[MSG_SIZ], lastHint[MSG_SIZ],
  thinkOutput1[MSG_SIZ];

int currentMove = 0, forwardMostMove = 0, backwardMostMove = 0,
  pauseExamForwardMostMove = 0,
  nCmailGames = 0, nCmailResults = 0, nCmailMovesRegistered = 0,
  cmailMoveRegistered[CMAIL_MAX_GAMES], cmailResult[CMAIL_MAX_GAMES],
  cmailMsgLoaded = FALSE, cmailMailedMove = FALSE,
  cmailOldMove = -1, firstMove = TRUE, flipView = FALSE,
  blackPlaysFirst = FALSE, startedFromSetupPosition = FALSE,
  searchTime = 0, pausing = FALSE, pauseExamInvalid = FALSE,
  whiteFlag = FALSE, blackFlag = FALSE,
  firstMaybePondering = FALSE, secondMaybePondering = FALSE, 
  ics_user_moved = 0, ics_gamenum = -1, ics_getting_history = H_FALSE,
  matchMode = FALSE, hintRequested = FALSE, bookRequested = FALSE,
  cmailMoveType[CMAIL_MAX_GAMES], ics_bughouse = FALSE;
long ics_basetime, ics_increment = 0, ics_clock_paused = 0;
ProcRef firstProgramPR = NoProc, secondProgramPR = NoProc, icsPR = NoProc,
  lastMsgPR = NoProc, cmailPR = NoProc;
InputSourceRef firstProgramISR = NULL, secondProgramISR = NULL,
  telnetISR = NULL, fromUserISR = NULL, cmailISR = NULL;
int firstSendTime = 2, secondSendTime = 2;  /* 0=don't, 1=do, 2=test */
char timeTestStr[MSG_SIZ];
GameMode gameMode = BeginningOfGame, lastGameMode = BeginningOfGame;
char moveList[MAX_MOVES][MOVE_LEN], parseList[MAX_MOVES][MOVE_LEN * 2];
char *commentList[MAX_MOVES], *cmailCommentList[CMAIL_MAX_GAMES];
char white_holding[32], black_holding[32];
TimeMark lastNodeCountTime;
long lastNodeCount=0;
int isCraftyRunning, craftyMajorVersion, craftyMinorVersion;
char craftyProgramName[MSG_SIZ];
int have_sent_ICS_logon = 0;
long whiteTimeRemaining, blackTimeRemaining, timeControl;
long timeRemaining[2][MAX_MOVES];
     
static int fgShout, bgShout, attrShout;
static int fgSShout, bgSShout, attrSShout;
static int fgChannel1, bgChannel1, attrChannel1;
static int fgChannel, bgChannel, attrChannel;
static int fgKibitz, bgKibitz, attrKibitz;
static int fgTell, bgTell, attrTell;
static int fgChallenge, bgChallenge, attrChallenge;
static int fgNormal, bgNormal, attrNormal;

GameInfo gameInfo;

AppData appData;

Board boards[MAX_MOVES], initialPosition = {
    { WhiteRook, WhiteKnight, WhiteBishop, WhiteQueen,
	WhiteKing, WhiteBishop, WhiteKnight, WhiteRook },
    { WhitePawn, WhitePawn, WhitePawn, WhitePawn,
	WhitePawn, WhitePawn, WhitePawn, WhitePawn },
    { EmptySquare, EmptySquare, EmptySquare, EmptySquare,
	EmptySquare, EmptySquare, EmptySquare, EmptySquare },
    { EmptySquare, EmptySquare, EmptySquare, EmptySquare,
	EmptySquare, EmptySquare, EmptySquare, EmptySquare },
    { EmptySquare, EmptySquare, EmptySquare, EmptySquare,
	EmptySquare, EmptySquare, EmptySquare, EmptySquare },
    { EmptySquare, EmptySquare, EmptySquare, EmptySquare,
	EmptySquare, EmptySquare, EmptySquare, EmptySquare },
    { BlackPawn, BlackPawn, BlackPawn, BlackPawn,
	BlackPawn, BlackPawn, BlackPawn, BlackPawn },
    { BlackRook, BlackKnight, BlackBishop, BlackQueen,
	BlackKing, BlackBishop, BlackKnight, BlackRook }
};

static char *cnames[9] = { "black", "red", "green", "yellow", "blue",
			     "magenta", "cyan", "white" };

/* String is: "fg, bg, attr". Which is 0, 1, 2 */
static int parse_color( str, which )
     char *str;
     int which;
{
    char *p, buf[100], *d;
    int i;
  
    if ( appData.debugMode )
      printf("parse_color:%s:\n", str );
    
    if ( strlen( str ) > 99 )	/* watch bounds on buf */
      return -1;

    p = str;
    d = buf;
    for( i=0; i<which; ++i ) {
	p = strchr( p, ',' );
	if ( !p )
	  return -1;
	++p;
    }

    /* Could be looking at something like:
       black, , 1
       .. in which case we want to stop on a comma also */
    while( *p && *p != ',' && !isalpha(*p) && !isdigit(*p) )
      ++p;

    if ( *p == ',' )
      {
	if ( appData.debugMode )
	  printf("item %d is empty\n", which );
	
	return -1;	/* Use default for empty field */
      }
    
    if ( which == 2 || isdigit(*p) )
      return atoi( p );
 
    while( *p && isalpha( *p ) )
      *(d++) = *(p++);

    *d = 0;

    for( i=0; i<8; ++i ) {
	if ( !StrCaseCmp( buf, cnames[i] ) )
	  return which? (i+40) : (i+30);
    }
    if (!StrCaseCmp(buf, "default")) return -1;

    fprintf(stderr, "ERROR Color `%s' not matched.\n", buf );
    return -2;
}

static int parse_cpair( fg, bg, attr, str )
     int *fg, *bg, *attr;
     char *str;
{
    if ( (*fg=parse_color( str, 0 )) == -2 ) {
	fprintf(stderr, "ERROR Can't parse foreground color in `%s'\n", str );
	return -1;
    }

    /* bg and attr are optional */
    if ( (*bg=parse_color( str, 1 )) < 0 || 
	(*attr = parse_color( str, 2 )) < 0 ) {
	*attr = 0;
    }
    return 0;
}

/* Convert str to a rating. Checks for special cases of "----",
   "++++", etc. Also strips ()'s */
int string_to_rating( str )
  char *str;
{
  while( *str && !isdigit( *str ) )
    ++str;

  if ( !*str )
    return 0;	/* One of the special "no rating" cases */
  else
    return atoi( str );
}

void InitBackEnd1()
{
    int matched, min, sec;

    isCraftyRunning = 0;
    strcpy( craftyProgramName, "??? Unknown Program ???" );

    /* Init programStats */
    programStats.movelist[0] = 0;
    programStats.depth = 0;
    programStats.nr_moves = 0;
    programStats.moves_left = 0;
    programStats.nodes = 0;
    programStats.time = 100;
    programStats.score = 0;
    programStats.got_only_move = 0;
    programStats.got_fail = 0;
    programStats.ok_to_send = 1;
    programStats.line_is_book = 0;
    programStats.seen_stat = 0;
	
    /*
     * Initialize game list
     */
    ListNew(&gameList);


    /*
     * Internet chess server status
     */
    if (appData.icsActive) {
	appData.matchMode = FALSE;
#ifdef ZIPPY	
	appData.noChessProgram = !appData.zippyPlay;
#else
	appData.zippyPlay = FALSE;
	appData.zippyTalk = FALSE;
	appData.noChessProgram = TRUE;
#endif
    } else {
	appData.zippyTalk = appData.zippyPlay = FALSE;
    }

    /*
     * Parse timeControl resource
     */
    if (!ParseTimeControl(appData.timeControl)) {
	/* Can't use DisplayFatalError yet; need more initialization */
	fprintf(stderr, "Bad timeControl option %s", appData.timeControl);
	exit(2);
    }
    if (appData.icsActive) timeControl = 0;
    
    /*
     * Parse searchTime resource
     */
    if (*appData.searchTime != NULLCHAR) {
	matched = sscanf(appData.searchTime, "%d:%d", &min, &sec);
	if (matched == 1) {
	    searchTime = min * 60;
	} else if (matched == 2) {
	    searchTime = min * 60 + sec;
	} else {
	    /* Can't use DisplayFatalError yet; need more initialization */
	    fprintf(stderr, "Bad searchTime option %s", appData.searchTime);
	    exit(2);
	}
    }
    
    if (appData.icsActive) {
	appData.clockMode = TRUE;
    } else if ((*appData.searchTime != NULLCHAR) ||
	       (appData.searchDepth > 0) ||
	       appData.noChessProgram) {
	appData.clockMode = FALSE;
	firstSendTime = secondSendTime = 0;
    }

#ifdef ZIPPY
    ZippyInit();
#endif

    /* Parse colors, if colorizing */
    if ( appData.colorize == True )
      {
	  if ( parse_cpair( &fgShout, &bgShout, &attrShout,
			   appData.colorShout ) < 0 ||
	      parse_cpair( &fgSShout, &bgSShout, &attrSShout,
			  appData.colorSShout ) < 0 ||
	      parse_cpair( &fgChannel1, &bgChannel1, &attrChannel1,
			  appData.colorChannel1 ) < 0  ||
	      parse_cpair( &fgChannel, &bgChannel, &attrChannel,
			  appData.colorChannel ) < 0  ||
	      parse_cpair( &fgKibitz, &bgKibitz, &attrKibitz,
			  appData.colorKibitz ) < 0 ||
	      parse_cpair( &fgTell, &bgTell, &attrTell,
			  appData.colorTell ) < 0 ||
	      parse_cpair( &fgChallenge, &bgChallenge, &attrChallenge,
			  appData.colorChallenge ) < 0  ||
	      parse_cpair( &fgNormal, &bgNormal, &attrNormal,
			  appData.colorNormal ) < 0 )
	    {
		fprintf( stderr, "ERROR Unable to parse color strings. Disabling colorization.\n");
		appData.colorize = False;
	    }
      }
}

int ParseTimeControl(tc)
     char *tc;
{
    int matched, min, sec;

    matched = sscanf(tc, "%d:%d", &min, &sec);
    if (matched == 1) {
	timeControl = min * 60 * 1000;
    } else if (matched == 2) {
	timeControl = (min * 60 + sec) * 1000;
    } else {
	return FALSE;
    }
    return TRUE;
}

void InitBackEnd2()
{
    char buf[MSG_SIZ];
    int err;

    if (appData.icsActive) {
	err = establish();
	if (err != 0) {
	    if (*appData.icsCommPort != NULLCHAR) {
		sprintf(buf, "Could not open comm port %s",  
			appData.icsCommPort);
	    } else {
		sprintf(buf, "Could not connect to host %s, port %s",  
			appData.icsHost, appData.icsPort);
	    }
	    DisplayFatalError(buf, err, 1);
	    return;
	}
	SetICSMode();
	telnetISR = AddInputSource(icsPR, FALSE, read_from_ics);
	fromUserISR = AddInputSource(NoProc, FALSE, read_from_player);
    } else if (appData.noChessProgram) {
	SetNCPMode();
    } else {
	SetGNUMode();
    }

    if (*appData.cmailGameName != NULLCHAR) {
	SetCmailMode();
	OpenLoopback(&cmailPR);
	cmailISR = AddInputSource(cmailPR, FALSE, CmailSigHandlerCallBack);
    }
    
    if (appData.matchMode) {
	/* Set up machine vs. machine match */
	if (appData.noChessProgram) {
	    DisplayFatalError("Can't have a match with no chess programs",
			      0, 2);
	    return;
	}
	Reset(TRUE);
	matchMode = TRUE;
	if (*appData.loadGameFile != NULLCHAR) {
	    if (!LoadGameFromFile(appData.loadGameFile,
				  appData.loadGameIndex,
				  appData.loadGameFile, FALSE)) {
		DisplayFatalError("Bad game file", 0, 1);
		return;
	    }
	} else if (*appData.loadPositionFile != NULLCHAR) {
	    if (!LoadPositionFromFile(appData.loadPositionFile,
				      appData.loadPositionIndex,
				      appData.loadPositionFile)) {
		DisplayFatalError("Bad position file", 0, 1);
		return;
	    }
	}
	TwoMachinesEvent();
    } else if (*appData.cmailGameName != NULLCHAR) {
	/* Set up cmail mode */
	ReloadCmailMsgEvent(TRUE);
    } else {
	/* Set up other modes */
	Reset(TRUE);
	if (*appData.loadGameFile != NULLCHAR) {
	    (void) LoadGameFromFile(appData.loadGameFile,
				    appData.loadGameIndex,
				    appData.loadGameFile, TRUE);
	} else if (*appData.loadPositionFile != NULLCHAR) {
	    (void) LoadPositionFromFile(appData.loadPositionFile,
					appData.loadPositionIndex,
					appData.loadPositionFile);
	}
    }
}

/*
 * Establish will establish a contact to a remote host.port.
 * Sets icsPR to a ProcRef for a process (or pseudo-process)
 *  used to talk to the host.
 * Returns 0 if okay, error code if not.
 */
int establish()
{
    char buf[MSG_SIZ];

    if (*appData.icsCommPort != NULLCHAR) {
	/* Talk to the host through a serial comm port */
	return OpenCommPort(appData.icsCommPort, &icsPR);

    } else if (*appData.gateway != NULLCHAR) {
	if (*appData.remoteShell == NULLCHAR) {
	    /* Use the rcmd protocol to run telnet program on a gateway host */
	    sprintf(buf, "%s %s %s",
		    appData.telnetProgram, appData.icsHost, appData.icsPort);
	    return OpenRcmd(appData.gateway, appData.remoteUser, buf, &icsPR);

	} else {
	    /* Use the rsh program to run telnet program on a gateway host */
	    if (*appData.remoteUser == NULLCHAR) {
		sprintf(buf, "%s %s %s %s %s", appData.remoteShell,
			appData.gateway, appData.telnetProgram,
			appData.icsHost, appData.icsPort);
	    } else {
		sprintf(buf, "%s -l %s %s %s %s %s",
			appData.remoteShell, appData.remoteUser,
			appData.gateway, appData.telnetProgram,
			appData.icsHost, appData.icsPort);
	    }
	    return StartChildProcess(buf, &icsPR);

	}
    } else if (appData.useTelnet) {
	return OpenTelnet(appData.icsHost, appData.icsPort, &icsPR);

    } else {
	/* TCP socket interface differs somewhat between
	   Unix and NT; handle details in the front end.
	*/
	return OpenTCP(appData.icsHost, appData.icsPort, &icsPR);
    }
}

void show_bytes(fp, buf, count)
     FILE *fp;
     char *buf;
     int count;
{
    while (count--) {
      if (*buf < 040 || *(unsigned char *) buf > 0177) {
	    fprintf(fp, "\\%03o", *buf & 0xff);
	} else {
	    putc(*buf, fp);
	}
	buf++;
    }
    fflush(fp);
}

void read_from_player(isr, message, count, error)
     InputSourceRef isr;
     char *message;
     int count;
     int error;
{
    int outError, outCount;
    static int eofCount = 0;

    if (count > 0) {
	if (appData.debugMode) {
	    fprintf(debugFP, "Sending to ICS: ");
	    show_bytes(debugFP, message, count);
	}
	outCount = OutputToProcess(icsPR, message, count, &outError);
	eofCount = 0;
	if (outCount < count) {
	    RemoveInputSource(isr);
	    DisplayFatalError("Error writing to ICS", outError, 1);
	}
    } else if (count < 0) {
	RemoveInputSource(isr);
	DisplayFatalError("Error reading from keyboard", error, 1);
    } else if (eofCount++ > 0) {
	RemoveInputSource(isr);
	DisplayFatalError("Got end of file from keyboard", 0, 0);
    }
}

void
SendToICS(s)
     char *s;
{
    int count, outCount, outError;

    if (icsPR == NULL) return;

    count = strlen(s);
    if (appData.debugMode) {
	fprintf(debugFP, "Sending to ICS: ");
	show_bytes(debugFP, s, count);
    }
    outCount = OutputToProcess(icsPR, s, count, &outError);
    if (outCount < count) {
	DisplayFatalError("Error writing to ICS", outError, 1);
    }
}

/* This is used for sending logon scripts to the ICS. Sending
   without a delay causes problems when using timestamp on ICC
   (at least on my machine). */
void
SendToICSDelayed(s,msdelay)
     char *s;
     long msdelay;
{
    int count, outCount, outError;

    if (icsPR == NULL) return;

    count = strlen(s);
    if (appData.debugMode) {
	fprintf(debugFP, "Sending to ICS: ");
	show_bytes(debugFP, s, count);
    }
    outCount = OutputToProcessDelayed(icsPR, s, count, &outError,
				      msdelay);
    if (outCount < count) {
	DisplayFatalError("Error writing to ICS", outError, 1);
    }
}


/* Remove all highlighting escape sequences in s
   Also deletes any suffix starting with '(' 
*/
char *
StripHighlightAndTitle(s)
     char *s;
{
    static char retbuf[MSG_SIZ];
    char *p = retbuf;

    while (*s != NULLCHAR) {
	while (*s == '\033') {
	    while (*s != NULLCHAR && !isalpha(*s)) s++;
	    if (*s != NULLCHAR) s++;
	}
	while (*s != NULLCHAR && *s != '\033') {
	    if (*s == '(') {
		*p = NULLCHAR;
		return retbuf;
	    }
	    *p++ = *s++;
	}
    }
    *p = NULLCHAR;
    return retbuf;
}

/* Remove all highlighting escape sequences in s */
char *
StripHighlight(s)
     char *s;
{
    static char retbuf[MSG_SIZ];
    char *p = retbuf;

    while (*s != NULLCHAR) {
	while (*s == '\033') {
	    while (*s != NULLCHAR && !isalpha(*s)) s++;
	    if (*s != NULLCHAR) s++;
	}
	while (*s != NULLCHAR && *s != '\033') {
	    *p++ = *s++;
	}
    }
    *p = NULLCHAR;
    return retbuf;
}

static int leftover_start = 0, leftover_len = 0;
char star_match[STAR_MATCH_N][MSG_SIZ];

/* Test whether pattern is present at &buf[*index]; if so, return TRUE,
   advance *index beyond it, and set leftover_start to the new value of
   *index; else return FALSE.  If pattern contains the character '*', it
   matches any sequence of characters not containing '\r', '\n', or the
   character following the '*' (if any), and the matched sequence(s) are
   copied into star_match.
*/
int
looking_at(buf, index, pattern)
     char *buf;
     int *index;
     char *pattern;
{
    char *bufp = &buf[*index], *patternp = pattern;
    int star_count = 0;
    char *matchp = star_match[0];
    
    for (;;) {
	if (*patternp == NULLCHAR) {
	    *index = leftover_start = bufp - buf;
	    *matchp = NULLCHAR;
	    return TRUE;
	}
	if (*bufp == NULLCHAR) return FALSE;
	if (*patternp == '*') {
	    if (*bufp == *(patternp + 1)) {
		*matchp = NULLCHAR;
		matchp = star_match[++star_count];
		patternp += 2;
		bufp++;
		continue;
	    } else if (*bufp == '\n' || *bufp == '\r') {
		patternp++;
		if (*patternp == NULLCHAR)
		  continue;
		else
		  return FALSE;
	    } else {
		*matchp++ = *bufp++;
		continue;
	    }
	}
	if (*patternp != *bufp) return FALSE;
	patternp++;
	bufp++;
    }
}

/*-- Game start info cache: --*/
int gs_gamenum;
char gs_kind[MSG_SIZ];
/*----------------------------*/

static void con_color( fg, bg, attr )
     int fg, bg, attr;
{
    if (bg > 0) {
	if (fg > 0) {
	    fprintf(toUserFP, "\033[0;%d;%d;%dm", attr, fg, bg);
	} else {
	    fprintf(toUserFP, "\033[0;%d;%dm", attr, bg);
	}
    } else {
	if (fg > 0) {
	    fprintf(toUserFP, "\033[0;%d;%dm", attr, fg);
	} else {
	    fprintf(toUserFP, "\033[0;%dm", attr);
	}
    }
    fflush(toUserFP);
}

static int loggedOn = FALSE;

void read_from_ics(isr, data, count, error)
     InputSourceRef isr;
     char *data;
     int count;
     int error;
{
#define BUF_SIZE 8192
#define STARTED_NONE 0
#define STARTED_MOVES 1
#define STARTED_BOARD 2
#define STARTED_OBSERVE 3
#define STARTED_HOLDINGS 4
    
    static int started = STARTED_NONE;
    static char parse[20000];
    static int parse_pos;
    static char buf[BUF_SIZE + 1];
    static int firstTime = TRUE, intfSet = FALSE;
    static int savingComment = FALSE;
    static coloring = FALSE;
    /* When parsing a Creating: message, we have to save one of the
       players names & both ratings and figure out who is who
       when the game starts.
       !! I THINK we can store this in the gameInfo directly,
       but some funny stuff may happen with clearing it,
       so we'll do this for now */
    static char player1Name[128];
    
    char str[500];
    int i, oldi;
    int buf_len;
    int next_out;
    int tkind, reject, n;

    if (count > 0) {
	/* If last read ended with a partial line that we couldn't parse,
	   prepend it to the new read and try again. */
	if (leftover_len > 0) {
	    for (i=0; i<leftover_len; i++)
	      buf[i] = buf[leftover_start + i];
	}

	/* Copy in new characters, removing nulls and \r's */
	buf_len = leftover_len;
	for (i = 0; i < count; i++) {
	    if (data[i] != NULLCHAR && data[i] != '\r')
	      buf[buf_len++] = data[i];
	}

	buf[buf_len] = NULLCHAR;
	next_out = leftover_len;
	leftover_start = 0;
	
	i = 0;
	while (i < buf_len) {
	    
#define TN_WILL 0373
#define TN_WONT 0374
#define TN_DO   0375
#define TN_DONT 0376
#define TN_IAC  0377
#define TN_ECHO 0001
#define TN_SGA  0003
#define TN_PORT 23

	    /* Deal with part of the TELNET option negotiation
	       protocol.  We refuse to do anything beyond the
	       defaults, except that we allow the WILL ECHO option,
	       which ICS uses to turn off password echoing when we are
	       directly connected to it.  We must reject this option
	       when we are talking to a real telnet server (port 23),
	       because most telnet servers want to go into
	       character-by-character mode and handle all the
	       echoing. */
	    if (buf_len - i >= 3 && (unsigned char) buf[i] == TN_IAC) {
		static int remoteEchoOption = FALSE; /* telnet ECHO option */
		unsigned char reply[4];
		oldi = i;
		reply[0] = TN_IAC;
		reply[3] = NULLCHAR;
		switch ((unsigned char) buf[++i]) {
		  case TN_WILL:
		    if (appData.debugMode)
		      fprintf(debugFP, "\n<WILL ");
		    switch (reply[2] = (unsigned char) buf[++i]) {
		      case TN_ECHO:
			if (appData.debugMode)
			  fprintf(debugFP, "ECHO ");
			/* Reply only if this is a change, according
			   to the protocol rules. */
			if (remoteEchoOption) break;
			if (atoi(appData.icsPort) == TN_PORT) {
			    reply[1] = TN_DONT;
			    if (appData.debugMode)
			      fprintf(debugFP, ">DONT ECHO ");
			} else {
			    EchoOff();
			    reply[1] = TN_DO;
			    if (appData.debugMode)
			      fprintf(debugFP, ">DO ECHO ");
			    remoteEchoOption = TRUE;
			}
			SendToICS((char *) reply);
			break;
		      default:
			if (appData.debugMode)
			  fprintf(debugFP, "%d ", reply[2]);
			/* Whatever this is, we don't want it. */
			reply[1] = TN_DONT;
			if (appData.debugMode)
			  fprintf(debugFP, ">DONT %d ", reply[2]);
			SendToICS((char *) reply);
			break;
		    }
		    break;
		  case TN_WONT:
		    if (appData.debugMode)
		      fprintf(debugFP, "\n<WONT ");
		    switch (reply[2] = (unsigned char) buf[++i]) {
		      case TN_ECHO:
			if (appData.debugMode)
			  fprintf(debugFP, "ECHO ");
			/* Reply only if this is a change, according
			   to the protocol rules. */
			if (!remoteEchoOption) break;
			EchoOn();
			reply[1] = TN_DONT;
			if (appData.debugMode)
			  fprintf(debugFP, ">DONT ECHO ");
			remoteEchoOption = FALSE;
			SendToICS((char *) reply);
			break;
		      default:
			if (appData.debugMode)
			  fprintf(debugFP, "%d ", (unsigned char) buf[i]);
			/* Whatever this is, it must already be turned
			   off, because we never agree to turn on
			   anything non-default, so according to the
			   protocol rules, we don't reply. */
			break;
		    }
		    break;
		  case TN_DO:
		    if (appData.debugMode)
		      fprintf(debugFP, "\n<DO ");
		    switch (reply[2] = (unsigned char) buf[++i]) {
		      default:
			/* Whatever this is, we refuse to do it. */
			reply[1] = TN_WONT;
			if (appData.debugMode)
			  fprintf(debugFP, "%d >WONT %d ", reply[2], reply[2]);
			SendToICS((char *) reply);
			break;
		    }
		    break;
		  case TN_DONT:
		    if (appData.debugMode)
		      fprintf(debugFP, "\n<DONT ");
		    switch (reply[2] = (unsigned char) buf[++i]) {
		      default:
			if (appData.debugMode)
			  fprintf(debugFP, "%d ", reply[2]);
			/* Whatever this is, we are already not doing
			   it, because we never agree to do anything
			   non-default, so according to the protocol
			   rules, we don't reply. */
			break;
		    }
		    break;
		  case TN_IAC:
		    if (appData.debugMode)
		      fprintf(debugFP, "\n<IAC ");
		    /* Doubled IAC; pass it through */
		    i--;
		    break;
		  default:
		    if (appData.debugMode)
		      fprintf(debugFP, "\n<%d ", (unsigned char) buf[i]);
		    /* Drop all other telnet commands on the floor */
		    break;
		}
		if (oldi > next_out)
		  fwrite(&buf[next_out], oldi - next_out, 1, toUserFP);
		if (++i > next_out)
		  next_out = i;
		continue;
	    }
		
	    if (loggedOn && !intfSet) {
		extern char* programVersion;
		sprintf(str, "set interface %s\n", programVersion);
		SendToICS(str);
		intfSet = TRUE;
	    }

            /* Kludge to deal with rcmd protocol */
	    if (firstTime && looking_at(buf, &i, "\001*")) {
		DisplayFatalError(&buf[1], 0, 1);
		continue;
	    } else {
	        firstTime = FALSE;
	    }

#ifdef ZIPPY
	    if (appData.zippyTalk || appData.zippyPlay) {
		if (ZippyControl(buf, &i)) continue;
		if (appData.zippyTalk && ZippyConverse(buf, &i)) continue;
		if (appData.zippyPlay && ZippyMatch(buf, &i)) continue;
	    } 
#endif
	    oldi = i;
	    if (looking_at(buf, &i, "* s-shouts: *") ||
	        looking_at(buf, &i, "* c-shouts: *") ) {
		if (appData.colorize) {
		    if (oldi > next_out) {
			fwrite(&buf[next_out], oldi - next_out, 1, toUserFP);
			fflush(toUserFP);
			next_out = oldi;
		    }
		    con_color(fgSShout, bgSShout, attrSShout);
		    coloring = TRUE;
		}
		/* Ignore shouts */
		loggedOn = TRUE;
		continue;
	    }

	    if (looking_at(buf, &i, "--->")) {
		loggedOn = TRUE;
		continue;
	    }

	    if (looking_at(buf, &i, "* shouts: *") ||
		looking_at(buf, &i, "--> * *")) {
		if (appData.colorize) {
		    if (oldi > next_out) {
			fwrite(&buf[next_out], oldi - next_out, 1, toUserFP);
			fflush(toUserFP);
			next_out = oldi;
		    }
		    con_color(fgShout, bgShout, attrShout);
		    coloring = TRUE;
		}
		/* Ignore shouts */
		loggedOn = TRUE;
		continue;
	    }

	    if (looking_at( buf, &i, "Challenge:")) {
		if (appData.colorize) {
		    if (oldi > next_out) {
			fwrite(&buf[next_out], oldi - next_out, 1, toUserFP);
			fflush(toUserFP);
			next_out = oldi;
		    }
		    con_color( fgChallenge, bgChallenge, attrChallenge );
		    coloring = TRUE;
		}
		loggedOn = TRUE;
		continue;
	    }
		
	    if ((tkind = 1, looking_at(buf, &i, "* tells you: *")) ||
		looking_at(buf, &i, "* says: *") ||
		/* Make messages the same color as tells */
		looking_at(buf, &i, "--* (*): *" ) ||
		(tkind = 2, looking_at(buf, &i, "* whispers: *")) ||
		looking_at(buf, &i, "* kibitzes: *") ||
		(tkind = 3, looking_at(buf, &i, "*)(*): *")) ||
		looking_at(buf, &i, "*(*): *") ||
		(tkind = 4, looking_at(buf, &i, "*)(*)(*): *"))) {

	      /* Disambiguate tells from allobs & admin comments */
	      reject = 0;
	      if ( tkind == 3 ) {
		/* Make sure we don't match allobs */
		if ( !strncmp( "Observing", buf, 9 ) )
		  reject = 1;
		/* Make sure we don't match admin comments --
		   star_match[1] will be date/time instead of
		   channel # */
		else if ( strchr( star_match[1], ' ' ) )
		  reject = 1;
	      }

		if (appData.colorize && !reject) {
		    if (oldi > next_out) {
			fwrite(&buf[next_out], oldi - next_out, 1, toUserFP);
			fflush(toUserFP);
			next_out = oldi;
		    }
		    switch (tkind) {
		      case 1:
			con_color(fgTell, bgTell, attrTell);
			break;
		      case 2:
			con_color(fgKibitz, bgKibitz, attrKibitz);
			break;
		      case 3:
		      case 4:
			if (strcmp(star_match[tkind - 2], "1") == 0) {
			    con_color(fgChannel1, bgChannel1, attrChannel1);
			} else {
			    con_color(fgChannel, bgChannel, attrChannel);
			}
			break;
		    }
		    coloring = TRUE;
		}
		loggedOn = TRUE;
		if (started == STARTED_NONE && appData.autoComment &&
		    (gameMode == IcsObserving || gameMode == IcsPlayingWhite ||
		     gameMode == IcsPlayingBlack)) {
		    memcpy(parse, &buf[oldi], i - oldi);
		    parse[i - oldi] = NULLCHAR;
		    AppendComment(forwardMostMove, StripHighlight(parse));
		    savingComment = TRUE;
		} else {
		    savingComment = FALSE;
		}
		continue;
	    }

	    if (looking_at(buf, &i, "\\   *")) {
		if (savingComment) {
		    sprintf(parse, "\\   %s", star_match[0]);
		    AppendComment(forwardMostMove, parse);
		}
		continue;
	    }

	    if (looking_at(buf, &i, "Black Strength :") ||
		looking_at(buf, &i, "<<< style 10 board >>>") ||
		looking_at(buf, &i, "<10>") ||
		looking_at(buf, &i, "#@#")) {
		/* Wrong board style */
		loggedOn = TRUE;
		SendToICS("set style 12\n");
    	        SendToICS("refresh\n");
		continue;
	    }
	    
	    if (!have_sent_ICS_logon && looking_at(buf, &i, "login:")) {
		ICSInitScript();
		have_sent_ICS_logon = 1;
		continue;
	    }
	      
	    if (looking_at(buf, &i, "\n<12> ") ||
		looking_at(buf, &i, "<12> ")) {
		loggedOn = TRUE;
		if (oldi > next_out) {
		    fwrite(&buf[next_out], oldi - next_out, 1, toUserFP);
		    fflush(toUserFP);
		}
		next_out = i;
		started = STARTED_BOARD;
		parse_pos = 0;
		continue;
	    }

	    if ((started == STARTED_NONE && looking_at(buf, &i, "\n<b1> ")) ||
		looking_at(buf, &i, "<b1> ")) {
		if (oldi > next_out) {
		    fwrite(&buf[next_out], oldi - next_out, 1, toUserFP);
		    fflush(toUserFP);
		}
		next_out = i;
		started = STARTED_HOLDINGS;
		parse_pos = 0;
		continue;
	    }

	    if (looking_at(buf, &i, "* *vs. * *--- *")) {
		loggedOn = TRUE;
		/* Header for a move list -- first line */

		/* Save player ratings into gameInfo */
		gameInfo.whiteRating = string_to_rating( star_match[1] );
		gameInfo.blackRating = string_to_rating( star_match[3] );
		gameInfo.keepRatings = 1;
		if ( appData.debugMode )
		  printf("IGOT-HEADER: W=%d B=%d\n", gameInfo.whiteRating,
		       gameInfo.blackRating );
		
		switch (ics_getting_history) {
		  case H_FALSE:
		    switch (gameMode) {
		      case IcsIdle:
		      case BeginningOfGame:
			/* User typed "moves" or "oldmoves" while we
			   were idle.  Pretend we asked for these
			   moves and soak them up so user can step
			   through them and/or save them.
			   */
			Reset(FALSE);
			gameMode = IcsObserving;
			ModeHighlight();
			ics_gamenum = -1;
			ics_getting_history = H_GOT_REQ_HEADER;
			break;
		      case EditGame: /*?*/
		      case EditPosition: /*?*/
			/* Should above feature work in these modes too? */
			/* For now it doesn't */
			ics_getting_history = H_GOT_UNREQ_HEADER;
			break;
		      default:
			ics_getting_history = H_GOT_UNREQ_HEADER;
			break;
		    }
		    break;
		  case H_REQUESTED:
		    /* Is this the right one? */
		    if (strcmp(gameInfo.white, star_match[0]) == 0 &&
			strcmp(gameInfo.black, star_match[2]) == 0) {
			/* All is well */
			ics_getting_history = H_GOT_REQ_HEADER;
		    }
		    break;
		  case H_GOT_REQ_HEADER:
		  case H_GOT_UNREQ_HEADER:
		  case H_GETTING_MOVES:
		    /* Should not happen */
		    DisplayError("Error gathering move list: two headers", 0);
		    ics_getting_history = H_FALSE;
		    break;
		}
		continue;
	    }

	    if (looking_at(buf, &i,
	      "* * match, initial time: * minutes, increment: * seconds.")) {
		/* Header for a move list -- second line */
		/* Initial board will follow if this is a wild game */
		/* Again, we might want to save some of these fields later */
		/* For now we do nothing with them. */
		continue;
	    }

	    if (looking_at(buf, &i, "Move  ")) {
		/* Beginning of a move list */
		switch (ics_getting_history) {
		  case H_FALSE:
		    /* Normally should not happen */
		    /* Maybe user hit reset while we were parsing */
		    break;
		  case H_REQUESTED:
		  case H_GETTING_MOVES:
		    /* Should not happen */
		    DisplayError("Error gathering move list: no header", 0);
		    ics_getting_history = H_FALSE;
		    break;
		  case H_GOT_REQ_HEADER:
		    ics_getting_history = H_GETTING_MOVES;
		    started = STARTED_MOVES;
		    parse_pos = 0;
		    if (oldi > next_out) {
			fwrite(&buf[next_out], oldi - next_out, 1, toUserFP);
			fflush(toUserFP);
		    }
		    break;
		  case H_GOT_UNREQ_HEADER:
		    ics_getting_history = H_FALSE;
		    break;
		}
		continue;
	    }				
	    
	    if ((coloring && looking_at(buf, &i, "*% ")) ||
		looking_at(buf, &i, "% ") ||
		(started == STARTED_MOVES && looking_at(buf, &i, ": "))) {
		if (coloring) {
		    if (oldi > next_out) {
			fwrite(&buf[next_out], oldi - next_out, 1, toUserFP);
			fflush(toUserFP);
			next_out = oldi;
		    }
		    con_color(fgNormal, bgNormal, attrNormal);
		    coloring = FALSE;
		}
		savingComment = FALSE;
		switch (started) {
		  case STARTED_MOVES:
		    started = STARTED_NONE;
		    parse[parse_pos] = NULLCHAR;
		    ParseGameHistory(parse);
		    if (gameMode == IcsObserving && ics_gamenum == -1) {
			/* Moves came from oldmoves or moves command
			   while we weren't doing anything else.
			   */
			currentMove = forwardMostMove;
			flipView = appData.flipView;
			DrawPosition(FALSE, boards[currentMove]);
			DisplayBothClocks();
			sprintf(str, "%s vs. %s",
				gameInfo.white, gameInfo.black);
			DisplayTitle(str);
			gameMode = IcsIdle;
		    } else {
			/* Moves were history of an active game */
			if (gameInfo.resultDetails != NULL) {
			    free(gameInfo.resultDetails);
			    gameInfo.resultDetails = NULL;
			}
		    }
		    DisplayMove(currentMove - 1);
		    next_out = i;
		    ics_getting_history = H_FALSE;
		    break;

		  case STARTED_OBSERVE:
		    started = STARTED_NONE;
		    SendToICS("refresh\n");
		    break;

		  default:
		    break;
		}
		continue;
	    }
	    
	    if ((started == STARTED_MOVES || started == STARTED_BOARD ||
		 started == STARTED_HOLDINGS) && i >= leftover_len) {
		/* Accumulate characters in move list or board */
		parse[parse_pos++] = buf[i];
	    }
	    
	    /* Start of game messages.  Mostly we detect start of game
	       when the first board image arrives.  On some versions
	       of the ICS, though, we need to do a "refresh" after starting
	       to observe in order to get the current board right away. */
	    if (looking_at(buf, &i, "Adding game * to observation list")) {
		started = STARTED_OBSERVE;
		continue;
	    }

	    /* Handle auto-observe */
	    if (appData.autoObserve &&
		(gameMode == IcsIdle || gameMode == BeginningOfGame) &&
		looking_at(buf, &i, "Game notification: * (*) vs. * (*)")) {
		char *player;
		/* Choose the player that was highlighted, if any. */
		if (star_match[0][0] == '\033' ||
		    star_match[1][0] != '\033') {
		    player = star_match[0];
		} else {
		    player = star_match[2];
		}
		sprintf(str, "observe %s\n", StripHighlightAndTitle(player));
		SendToICS(str);
		/* Save ratings from notify string */
		gameInfo.whiteRating = string_to_rating( star_match[1] );
		gameInfo.blackRating = string_to_rating( star_match[3] );
		gameInfo.keepRatings = 1;

		if ( appData.debugMode )
		  printf("IGOT-NOTIFY: W=%d, B=%d\n", gameInfo.whiteRating,
			 gameInfo.blackRating);
		
		continue;
	    }

	    /* Deal with automatic examine mode after a game,
	       and with IcsObserving -> IcsExamining transition */
	    if (looking_at(buf, &i, "Entering examine mode for game *.") ||
		looking_at(buf, &i, "has made you an examiner of game *.")) {

		int gamenum = atoi(star_match[0]);
		if ((gameMode == IcsIdle || gameMode == IcsObserving) &&
		    gamenum == ics_gamenum) {
		    /* We were already playing or observing this game;
		       no need to refetch history */
		    gameMode = IcsExamining;
		    if (pausing) {
			pauseExamForwardMostMove = forwardMostMove;
		    } else if (currentMove < forwardMostMove) {
			ForwardInner(forwardMostMove);
		    }
		} else {
		    /* I don't think this case really can happen */
		    SendToICS("refresh\n");
		}
		continue;
	    }    
	    
	    /* Error messages */
	    if (ics_user_moved) {
		if (looking_at(buf, &i, "Illegal move") ||
		    looking_at(buf, &i, "Not a legal move") ||
		    looking_at(buf, &i, "Your king is in check") ||
		    looking_at(buf, &i, "It isn't your turn") ||
		    looking_at(buf, &i, "It is not your move")) {
		    /* Illegal move */
		    ics_user_moved = 0;
		    if (forwardMostMove > backwardMostMove) {
			currentMove = --forwardMostMove;
			DisplayError("Illegal move\n(rejected by ICS)", 0);
			DrawPosition(FALSE, boards[currentMove]);
			DisplayMove(currentMove - 1);
			SwitchClocks();
		    }
		    continue;
		}
	    }

	    if (looking_at(buf, &i, "still have time") ||
		looking_at(buf, &i, "not out of time")) {
		/* We must have called his flag a little too soon */
		whiteFlag = blackFlag = FALSE;
		continue;
	    }

	    if (looking_at(buf, &i, "added * seconds to") ||
		looking_at(buf, &i, "seconds were added to")) {
		/* Update the clocks */
		SendToICS("refresh\n");
		continue;
	    }

	    if (!ics_clock_paused && looking_at(buf, &i, "clock paused")) {
		ics_clock_paused = TRUE;
		StopClocks();
		continue;
	    }

	    if (ics_clock_paused && looking_at(buf, &i, "clock resumed")) {
		ics_clock_paused = FALSE;
		StartClocks();
		continue;
	    }

	    /* Grab player ratings from the Creating: message.
	     Note we have to check for the special case when
	     the ICS inserts things like [white] or [black]. */
	    if (looking_at(buf, &i, "Creating: * (*)* * (*)") ||
		looking_at(buf, &i, "Creating: * (*) [*] * (*)")) {
	      /* star_matches:
		 0      player 1 name (not necessarily white)
		 1	player 1 rating
		 2	empty, white, or black (IGNORED)
		 3	player 2 name (not necessarily black)
		 4      player 2 rating

		 The names/ratings are sorted out when the game
		 actually starts (below).
		 */
	      if ( appData.debugMode )
		printf("IGOT-CREATING:%s@%s[%s]:%s@%s\nASSUMING `%s' is WHITE\n"
		       , star_match[0], star_match[1], star_match[2],
		       star_match[3], star_match[4], star_match[0] );

	      /* Save these now - May have to swap them when the game
		 starts (assume for now player1 is white) */
	      gameInfo.whiteRating = string_to_rating( star_match[1] );
	      gameInfo.blackRating = string_to_rating( star_match[4] );
	      gameInfo.keepRatings = 1;

	      strcpy( player1Name, star_match[0] );
	      continue;
	    }
	    
	    /* Improved generic start/end-of-game messages */
	    if (looking_at(buf, &i, "{Game * (* vs. *) *}*")) {
		/* star_match[0] is the game number */
		/*           [1] is the white player's name */
		/*           [2] is the black player's name */
		/* For end-of-game: */
		/*           [3] is the reason for the game end */
		/*           [4] is a PGN end game-token, preceded by " " */
		/* For start-of-game: */
		/*           [3] begins with "Creating" or "Continuing" */
		/*           [4] is " *" or empty (don't care). */
		int gamenum = atoi(star_match[0]);
		char *why = star_match[3];
		char *endtoken = star_match[4];
		ChessMove endtype = (ChessMove) 0;

                /* Game start messages */
		if (strncmp(why, "Creating ", 9) == 0 ||
		    strncmp(why, "Continuing ", 11) == 0) {
		    gs_gamenum = gamenum;
		    strcpy(gs_kind, strchr(why, ' ') + 1);
		    /* Is player1 actually black? */
		    if (strcmp(player1Name, star_match[2]) == 0) {
		      if (appData.debugMode)
			printf("SWAPPING-RATINGS\n");
		      
		      /* Yep, swap the ratings */
		      n = gameInfo.whiteRating;
		      gameInfo.whiteRating = gameInfo.blackRating;
		      gameInfo.blackRating = n;
		    }
		    strcpy(player1Name, "");
#ifdef ZIPPY
		    ZippyGameStart(star_match[1], star_match[2]);
#endif /*ZIPPY*/
		    continue;
		}

		/* Game end messages */
		if (gameMode == IcsIdle || gameMode == BeginningOfGame ||
		    ics_gamenum != gamenum) {
		    continue;
		}
		while (endtoken[0] == ' ') endtoken++;
		switch (endtoken[0]) {
		  case '*':
		  default:
		    endtype = GameUnfinished;
		    break;
		  case '0':
		    endtype = BlackWins;
		    break;
		  case '1':
		    if (endtoken[1] == '/')
		      endtype = GameIsDrawn;
		    else
		      endtype = WhiteWins;
		    break;
		}
		GameEnds(endtype, why, GE_ICS);
		continue;
	    }

	    if (looking_at(buf, &i, "Removing game * from observation") ||
		looking_at(buf, &i, "Game * (*) has no examiners")) {
		if (gameMode == IcsObserving &&
		    atoi(star_match[0]) == ics_gamenum)
		  {
		      StopClocks();
		      gameMode = IcsIdle;
		      ics_gamenum = -1;
		      ics_user_moved = FALSE;
		  }
		continue;
	    }

	    if (looking_at(buf, &i, "You are no longer examining game *.")) {
		if (gameMode == IcsExamining &&
		    atoi(star_match[0]) == ics_gamenum)
		  {
		      gameMode = IcsIdle;
		      ics_gamenum = -1;
		      ics_user_moved = FALSE;
		  }
		continue;
	    }

	    /* Advance leftover_start past any newlines we find,
	       so only partial lines can get reparsed */
	    if (looking_at(buf, &i, "\n")) {
		if (started == STARTED_BOARD) {
		    started = STARTED_NONE;
		    parse[parse_pos] = NULLCHAR;
		    ParseBoard12(parse);
		    ics_user_moved = 0;
		    /* Usually suppress following prompt */
		    if (!(forwardMostMove == 0 && gameMode == IcsExamining)) {
			if (looking_at(buf, &i, "*% ")) {
			    savingComment = FALSE;
			}
		    }
		    next_out = i;
		} else if (started == STARTED_HOLDINGS) {
		    int gamenum;
		    started = STARTED_NONE;
		    parse[parse_pos] = NULLCHAR;
		    if (sscanf(parse, " game %d", &gamenum) == 1 &&
			gamenum == ics_gamenum) {
			ics_bughouse = TRUE;
			sscanf(parse, "game %d white %s black %s",
			       &gamenum, white_holding, black_holding);
			if (tinyLayout) {
			    sprintf(str, "%s-%s %s-%s",
				    white_holding, black_holding,
				    gameInfo.white, gameInfo.black);
			} else {
			sprintf(str, "%s %s vs. %s %s",
				gameInfo.white, white_holding,
				gameInfo.black, black_holding);
			}
			DisplayTitle(str);
		    }
		    /* Suppress following prompt */
		    if (looking_at(buf, &i, "*% ")) {
			savingComment = FALSE;
		    }
		    next_out = i;
		}
		continue;
	    }

	    i++;		/* skip unparsed character and loop back */
	}
	
	if (started != STARTED_MOVES && started != STARTED_BOARD &&
	    started != STARTED_HOLDINGS && i > next_out) {
	    fwrite(&buf[next_out], i - next_out, 1, toUserFP);
	    fflush(toUserFP);
	    next_out = i;
	}
	
	leftover_len = buf_len - leftover_start;
	/* if buffer ends with something we couldn't parse,
	   reparse it after appending the next read */
	
    } else if (count == 0) {
	extern char *programName;
	RemoveInputSource(isr);
	/*** User probably typed "quit", so don't do this:
	  DisplayFatalError("Connection closed by ICS", 0, 0);
	  ***/
	fprintf(stderr, "%s: Connection closed by ICS\n", programName);
	ExitEvent(0);
    } else {
	RemoveInputSource(isr);
	DisplayFatalError("Error reading from ICS", error, 1);
    }
}


/* Board style 12 looks like this:

<12> r-b---k- pp----pp ---bP--- ---p---- q------- ------P- P--Q--BP -----R-K W -1 0 0 0 0 0 0 paf MaxII 0 2 12 21 25 234 174 24 Q/d7-a4 (0:06) Qxa4 0

 * The "<12> " is stripped before it gets to this routine.
 * The trailing 0 (flip state) is a recent addition, and FICS doesn't have it.
 * Additional trailing fields may be added in the future.
 */

#define PATTERN "%72c%c%d%d%d%d%d%d%d%s%s%d%d%d%d%d%d%d%d%s%s%s%d"

#define RELATION_OBSERVING_PLAYED    0
#define RELATION_OBSERVING_STATIC   -2   /* examined, oldmoves, or smoves */
#define RELATION_PLAYING_MYMOVE      1
#define RELATION_PLAYING_NOTMYMOVE  -1
#define RELATION_EXAMINING           2
#define RELATION_ISOLATED_BOARD     -3

void ParseBoard12(string)
     char *string;
{ 
    GameMode newGameMode;
    int gamenum, newGame, relation, basetime, increment, ics_flip = 0;
    int j, k, n, moveNum, white_stren, black_stren, white_time, black_time;
    int double_push, castle_ws, castle_wl, castle_bs, castle_bl, irrev_count;
    char to_play, board_chars[72];
    char move_str[500], str[500], elapsed_time[500];
    char black[32], white[32];
    Board board;
    
    newGame = FALSE;

    if (appData.debugMode)
      fprintf(debugFP, "Parsing board: %s\n", string);

    move_str[0] = NULLCHAR;
    elapsed_time[0] = NULLCHAR;
    n = sscanf(string, PATTERN, board_chars, &to_play, &double_push,
	       &castle_ws, &castle_wl, &castle_bs, &castle_bl, &irrev_count,
	       &gamenum, white, black, &relation, &basetime, &increment,
	       &white_stren, &black_stren, &white_time, &black_time,
	       &moveNum, str, elapsed_time, move_str, &ics_flip);

    if (n < 22) {
	sprintf(str, "Failed to parse board string:\n\"%s\"", string);
	DisplayError(str, 0);
	return;
    }

    /* Convert the move number to internal form */
    moveNum = (moveNum - 1) * 2;
    if (to_play == 'B') moveNum++;

    switch (relation) {
      case RELATION_OBSERVING_PLAYED:
      case RELATION_OBSERVING_STATIC:
	if (gamenum == -1) {
	    /* Old ICC buglet */
	    relation = RELATION_OBSERVING_STATIC;
	}
	newGameMode = IcsObserving;
	break;
      case RELATION_PLAYING_MYMOVE:
      case RELATION_PLAYING_NOTMYMOVE:
	newGameMode =
	  ((relation == RELATION_PLAYING_MYMOVE) == (to_play == 'W')) ?
	    IcsPlayingWhite : IcsPlayingBlack;
	break;
      case RELATION_EXAMINING:
	newGameMode = IcsExamining;
	break;
      case RELATION_ISOLATED_BOARD:
      default:
	/* Just display this board.  If user was doing something else,
	   we will forget about it until the next board comes. */ 
	newGameMode = IcsIdle;
	break;
    }
    
    /* Modify behavior for initial board display on move listing
       of wild games.
    */
    switch (ics_getting_history) {
      case H_FALSE:
      case H_REQUESTED:
	break;
      case H_GOT_REQ_HEADER:
	/* This is the initial position of the current game */
	gamenum = ics_gamenum;
	moveNum = 0; /* old ICS bug workaround */
	newGameMode = gameMode;
	/* Kludge to keep "first board of a new game" code from
	   being executed: */ 
	relation = RELATION_OBSERVING_STATIC;
	break;
      case H_GOT_UNREQ_HEADER:
	/* This is an initial board that we don't want */
	return;
      case H_GETTING_MOVES:
	/* Should not happen */
	DisplayError("Error gathering move list: extra board", 0);
	ics_getting_history = H_FALSE;
	return;
    }

    /* Take action if this is the first board of a new game, or of a
       different game than is currently being displayed.  */
    if (gamenum != ics_gamenum || newGameMode != gameMode ||
	relation == RELATION_ISOLATED_BOARD) {

	/* Forget the old game and get the history (if any) of the new one */
	Reset(FALSE);
	newGame = TRUE;
	if (gamenum == -1) {
	    newGameMode = IcsIdle;
	} else if (moveNum > 0 && newGameMode != IcsIdle &&
		   appData.getMoveList) {
	    /* Need to get game history */
	    ics_getting_history = H_REQUESTED;
	    sprintf(str, "moves %d\n", gamenum);
	    SendToICS(str);
	}

	/* Initially flip the board to have black on the bottom if playing
	   black or if the ICS flip flag is set, but let the user change
	   it with the Flip View button. */
	flipView = (newGameMode == IcsPlayingBlack) || ics_flip;

	/* Done with values from previous mode; copy in new ones */
	gameMode = newGameMode;
	ModeHighlight();
	ics_gamenum = gamenum;
	if (gamenum == gs_gamenum) {
	    int klen = strlen(gs_kind);
	    if (gs_kind[klen - 1] == '.') gs_kind[klen - 1] = NULLCHAR;
	    sprintf(str, "ICS %s", gs_kind);
	    gameInfo.event = StrSave(str);
	} else {
	    gameInfo.event = StrSave("ICS game");
	}
	gameInfo.site = StrSave(appData.icsHost);
	gameInfo.date = PGNDate();
	gameInfo.round = StrSave("-");
	gameInfo.white = StrSave(white);
	gameInfo.black = StrSave(black);
	ics_basetime = basetime * 60 * 1000; /* convert to ms */
	ics_increment = increment * 1000;    /* convert to ms */
	if (increment == 0)
	  sprintf(str, "%d", basetime * 60);
	else
	  sprintf(str, "%d+%d", basetime * 60, increment);
	gameInfo.timeControl = StrSave(str);

	/* Silence shouts if requested */
	if (appData.quietPlay &&
	    (gameMode == IcsPlayingWhite || gameMode == IcsPlayingBlack)) {
	    SendToICS("set shout 0\n");
	}
    }

    /* Throw away game result if anything actually changes in examine mode */
    if (gameMode == IcsExamining && !newGame) {
	gameInfo.result = GameUnfinished;
	if (gameInfo.resultDetails != NULL) {
	    free(gameInfo.resultDetails);
	    gameInfo.resultDetails = NULL;
	}
    }

    /* In pausing && IcsExamining mode, we ignore boards coming
       in if they are in a different variation than we are. */
    if (pauseExamInvalid) return;
    if (pausing && gameMode == IcsExamining) {
	if (moveNum <= pauseExamForwardMostMove) {
	    pauseExamInvalid = TRUE;
	    forwardMostMove = pauseExamForwardMostMove;
	    return;
	}
    }

    /* Parse the board */
    for (k = 0; k < 8; k++)
      for (j = 0; j < 8; j++)
	board[k][j] = CharToPiece(board_chars[(7-k)*9 + j]);
    CopyBoard(boards[moveNum], board);
    if (moveNum == 0) {
	startedFromSetupPosition =
	  !CompareBoards(board, initialPosition);
    }
    
    if (ics_getting_history == H_GOT_REQ_HEADER) {
	/* This was an initial position from a move list, not
	   the current position */
	return;
    }

    /* Update currentMove and known move number limits */
    if (newGame) {
	forwardMostMove = backwardMostMove = currentMove = moveNum;
    } else if (moveNum == forwardMostMove + 1 || moveNum == forwardMostMove
	       || (moveNum < forwardMostMove && moveNum >= backwardMostMove)) {
	forwardMostMove = moveNum;
	if (!pausing || currentMove > forwardMostMove)
	  currentMove = forwardMostMove;
    } else {
	/* New part of history that is not contiguous with old part */ 
	if (pausing && gameMode == IcsExamining) {
	    pauseExamInvalid = TRUE;
	    forwardMostMove = pauseExamForwardMostMove;
	    return;
	}
	forwardMostMove = backwardMostMove = currentMove = moveNum;
	if (gameMode == IcsExamining && moveNum > 0 && appData.getMoveList) {
	    ics_getting_history = H_REQUESTED;
	    sprintf(str, "moves %d\n", gamenum);
	    SendToICS(str);
	}
    }

    /* Update the clocks */
    timeRemaining[0][moveNum] = whiteTimeRemaining = white_time * 1000;
    timeRemaining[1][moveNum] = blackTimeRemaining = black_time * 1000;

#ifdef ZIPPY
    if (appData.zippyPlay && newGame &&
	gameMode != IcsObserving && gameMode != IcsIdle &&
	gameMode != IcsExamining)
      ZippyFirstBoard(moveNum, basetime, increment);
#endif

    /* Put the move on the move list, first converting
       to canonical algebraic form. */
    if (moveNum > 0) {
	ChessMove moveType;
	int fromX, fromY, toX, toY;
	char promoChar;

	fromX = fromY = toX = toY = -1;
	if (moveNum <= backwardMostMove) {
	    /* We don't know what the board looked like before
	       this move.  Punt. */
	    strcpy(parseList[moveNum - 1], move_str);
	    strcat(parseList[moveNum - 1], " ");
	    strcat(parseList[moveNum - 1], elapsed_time);
	} else if (ParseMachineMove(move_str, moveNum - 1, &moveType,
				    &fromX, &fromY, &toX, &toY, &promoChar)) {
	    moveType = CoordsToAlgebraic(boards[moveNum - 1],
					 FakeFlags(moveNum - 1), EP_UNKNOWN,
					 fromY, fromX, toY, toX, promoChar,
					 parseList[moveNum-1]);
	    switch (MateTest(boards[moveNum], FakeFlags(moveNum), EP_UNKNOWN)){
	      case MT_NONE:
	      case MT_STALEMATE:
	      default:
		break;
	      case MT_CHECK:
		strcat(parseList[moveNum - 1], "+");
		break;
	      case MT_CHECKMATE:
		strcat(parseList[moveNum - 1], "#");
		break;
	    }
	    strcat(parseList[moveNum - 1], " ");
	    strcat(parseList[moveNum - 1], elapsed_time);
	} else if (strcmp(move_str, "none") == 0) {
	    /* Again, we don't know what the board looked like;
	       this is really the start of the game. */
	    /* !!What should Zippy do if this happens? */
	    /* !!Actually, I don't think this can happen currently. */
	    parseList[moveNum - 1][0] = NULLCHAR;
	    backwardMostMove = moveNum;
	    startedFromSetupPosition = TRUE;
	} else {
	    /* Move from ICS was illegal!?  Punt. */
	    if (appData.checkLegality && appData.debugMode) {
		sprintf(str, "Illegal move \"%s\" from ICS", move_str);
		DisplayError(str, 0);
	    }
	    strcpy(parseList[moveNum - 1], move_str);
	    strcat(parseList[moveNum - 1], " ");
	    strcat(parseList[moveNum - 1], elapsed_time);
	}

#ifdef ZIPPY
	/* Send move to chess program */
	if (appData.zippyPlay && !newGame) {
	    if ((gameMode == IcsPlayingWhite && WhiteOnMove(moveNum)) ||
		(gameMode == IcsPlayingBlack && !WhiteOnMove(moveNum))) {

		if (fromX == -1) {
		    sprintf(str, "Couldn't parse move \"%s\" from ICS",
			    move_str);
		    DisplayError(str, 0);
		} else {
		    SendMoveToProgram(moveType, fromX, fromY, toX, toY,
				      firstProgramPR, firstSendTime);
		    if (firstMove) {
			firstMove = FALSE;
			SendToProgram(appData.whiteString, firstProgramPR);
		    }
		}
	    }
	}
#endif
    }
    
    /* Start the clocks */
    appData.clockMode = !(ics_basetime == 0 && ics_increment == 0);
    if (gameMode == IcsIdle ||
	relation == RELATION_OBSERVING_STATIC ||
	relation == RELATION_EXAMINING ||
	ics_clock_paused)
      DisplayBothClocks();
    else
      StartClocks();

    /* Display opponents and material strengths */
    if (!ics_bughouse) {
	if (tinyLayout) {
	    /* Make it easier to see strength in tiny layout */
	    sprintf(str, "(%d-%d) %s-%s",
		    white_stren, black_stren, gameInfo.white, gameInfo.black);
	} else {
	    basetime = increment = 0;
	    sscanf( gameInfo.timeControl, "%d+%d", &basetime, &increment );
	    sprintf(str, "%s (%d) vs. %s (%d) tc={%d %d}", 
		    gameInfo.white, white_stren, gameInfo.black, black_stren,
		    basetime/60, increment );
	}
	DisplayTitle(str);
    }
    
    /* Display the board */
    if (!pausing) {
	DrawPosition(FALSE, boards[currentMove]);
	DisplayMove(moveNum - 1);
	if (appData.ringBellAfterMoves && !ics_user_moved)
	  RingBell();
    }
}

void
GetMoveListEvent()
{
    char buf[MSG_SIZ];
    if (appData.icsActive && gameMode != IcsIdle && ics_gamenum > 0) {
	ics_getting_history = H_REQUESTED;
	sprintf(buf, "moves %d\n", ics_gamenum);
	SendToICS(buf);
    }
}

void AnalysisPeriodicEvent( force )
	 int force;
{
  if ( ((programStats.ok_to_send == 0 || programStats.line_is_book)
	   && !force) || appData.periodicUpdates==False )
	return;

  /* Send . command to Crafty to collect stats */
  SendToProgram( ".\n", firstProgramPR );

  /* Don't send another until we get a response (this makes
     us stop sending to old Crafty's which don't understand
	 the "." command (sending illegal cmds resets node count & time,
	 which looks bad)) */
  programStats.ok_to_send = 0;
}

void
SendMoveToICS(moveType, fromX, fromY, toX, toY)
     ChessMove moveType;
     int fromX, fromY, toX, toY;
{
    char user_move[MSG_SIZ];
    char promo_char;

    switch (moveType) {
      default:
	DisplayError("Internal error; bad moveType", 0);
	break;
      case WhiteKingSideCastle:
      case BlackKingSideCastle:
      case WhiteQueenSideCastleWild:
      case BlackQueenSideCastleWild:
	sprintf(user_move, "o-o\n");
	break;
      case WhiteQueenSideCastle:
      case BlackQueenSideCastle:
      case WhiteKingSideCastleWild:
      case BlackKingSideCastleWild:
	sprintf(user_move, "o-o-o\n");
	break;
      case WhitePromotionQueen:
      case BlackPromotionQueen:
      case WhitePromotionRook:
      case BlackPromotionRook:
      case WhitePromotionBishop:
      case BlackPromotionBishop:
      case WhitePromotionKnight:
      case BlackPromotionKnight:
#if 0 
	sprintf(user_move, "%c%c%c%c=%c\n",
		'a' + fromX, '1' + fromY, 'a' + toX, '1' + toY,
		PieceToChar(PromoPiece(moveType)));
#else /* temporary code for FICS compatibility */
	promo_char = ToLower(PieceToChar(PromoPiece(moveType)));
	if (promo_char == 'n') promo_char = 'k';  /* for ICC (!) */
	sprintf(user_move, "promote %c\n%c%c%c%c\n",
		promo_char, 'a' + fromX, '1' + fromY, 'a' + toX, '1' + toY);
#endif
	break;
      case WhiteDrop:
      case BlackDrop:
	sprintf(user_move, "%c@%c%c\n",
		ToUpper(PieceToChar((ChessSquare) fromX)),
		'a' + toX, '1' + toY);
	break;
      case NormalMove:
      case WhiteCapturesEnPassant:
      case BlackCapturesEnPassant:
	sprintf(user_move, "%c%c%c%c\n",
		'a' + fromX, '1' + fromY, 'a' + toX, '1' + toY);
	break;
    }
    SendToICS(user_move);
}

void ProcessICSInitScript(f)
     FILE *f;
{
    char buf[MSG_SIZ];

    while (fgets(buf, MSG_SIZ, f)) {
	SendToICSDelayed(buf,(long)appData.msLoginDelay);
    }

    fclose(f);
}


/* Parser for moves from gnuchess or ICS */
Boolean ParseMachineMove(machineMove, moveNum,
			 moveType, fromX, fromY, toX, toY, promoChar)
     char *machineMove;
     int moveNum;
     ChessMove *moveType;
     int *fromX, *fromY, *toX, *toY;
     char *promoChar;
{       
    *moveType = yylexstr(moveNum, machineMove);
    switch (*moveType) {
      case WhitePromotionQueen:
      case BlackPromotionQueen:
      case WhitePromotionRook:
      case BlackPromotionRook:
      case WhitePromotionBishop:
      case BlackPromotionBishop:
      case WhitePromotionKnight:
      case BlackPromotionKnight:
      case NormalMove:
      case WhiteCapturesEnPassant:
      case BlackCapturesEnPassant:
      case WhiteKingSideCastle:
      case WhiteQueenSideCastle:
      case BlackKingSideCastle:
      case BlackQueenSideCastle:
      case WhiteKingSideCastleWild:
      case WhiteQueenSideCastleWild:
      case BlackKingSideCastleWild:
      case BlackQueenSideCastleWild:
	*fromX = currentMoveString[0] - 'a';
	*fromY = currentMoveString[1] - '1';
	*toX = currentMoveString[2] - 'a';
	*toY = currentMoveString[3] - '1';
	*promoChar = currentMoveString[4];
	return TRUE;

      case WhiteDrop:
      case BlackDrop:
	*fromX = *moveType == WhiteDrop ?
	  (int) CharToPiece(ToUpper(currentMoveString[0])) :
	  (int) CharToPiece(ToLower(currentMoveString[0]));
	*fromY = DROP_RANK;
	*toX = currentMoveString[2] - 'a';
	*toY = currentMoveString[3] - '1';
	*promoChar = NULLCHAR;
	return TRUE;

      case BadMove:
	/* bug?  Try parsing as coordinate notation. */
	*fromX = machineMove[0] - 'a';
	*fromY = machineMove[1] - '1';
	*toX = machineMove[2] - 'a';
	*toY = machineMove[3] - '1';
	*promoChar = machineMove[4];
	if (*fromX < 0 || *fromX > 7 || *fromY < 0 || *fromY > 7 ||
	    *toX < 0 || *toX > 7 || *toY < 0 || *toY > 7)
	  *fromX = *fromY = *toX = *toY = 0;
	return FALSE;

      case AmbiguousMove:
      case (ChessMove) 0:	/* end of file */
      case ElapsedTime:
      case Comment:
      case PGNTag:
      case WhiteWins:
      case BlackWins:
      case GameIsDrawn:
      default:
	/* bug? */
	*fromX = *fromY = *toX = *toY = 0;
	*promoChar = NULLCHAR;
	return FALSE;
    }
}


void InitPosition(redraw)
     int redraw;
{
    currentMove = forwardMostMove = backwardMostMove = 0;
    CopyBoard(boards[0], initialPosition);
    if (redraw)
      DrawPosition(FALSE, boards[currentMove]);
}

void SendCurrentBoard(pr)
     ProcRef pr;
{
    SendBoard(pr, boards[currentMove]);
}

void SendBoard(pr, board)
     ProcRef pr;
     Board board;
{
    char message[MSG_SIZ];
    ChessSquare *bp;
    int i, j;
    
    SendToProgram("edit\nwhite\n", pr);  /* "white" is for crafty */
    SendToProgram("#\n", pr);
    for (i = BOARD_SIZE - 1; i >= 0; i--) {
	bp = &board[i][0];
	for (j = 0; j < BOARD_SIZE; j++, bp++) {
	    if ((int) *bp < (int) BlackPawn) {
		sprintf(message, "%c%c%c\n", PieceToChar(*bp), 
		    'a' + j, '1' + i);
		SendToProgram(message, pr);
	    }
	}
    }
    
    SendToProgram("c\n", pr);
    for (i = BOARD_SIZE - 1; i >= 0; i--) {
	bp = &board[i][0];
	for (j = 0; j < BOARD_SIZE; j++, bp++) {
	    if (((int) *bp != (int) EmptySquare)
		&& ((int) *bp >= (int) BlackPawn)) {
		sprintf(message, "%c%c%c\n", ToUpper(PieceToChar(*bp)),
			'a' + j, '1' + i);
		SendToProgram(message, pr);
	    }
	}
    }
    
    SendToProgram(".\n", pr);
}


void SendMoveToProgram(moveType, fromX, fromY, toX, toY, programPR, sendTime)
     ChessMove moveType;
     int fromX, fromY, toX, toY;
     ProcRef programPR;
     int sendTime;
{
    char user_move[MSG_SIZ];
    char promoChar = ToLower(PieceToChar(PromoPiece(moveType)));

    if (promoChar == '.')
      sprintf(user_move, "%c%c%c%c\n",
	      'a' + fromX, '1' + fromY, 'a' + toX, '1' + toY);
    else
      sprintf(user_move, "%c%c%c%c%c\n",
	      'a' + fromX, '1' + fromY, 'a' + toX, '1' + toY,
	      promoChar);
    
    if (sendTime)
      SendTimeRemaining(programPR);
    SendToProgram(user_move, programPR);
    strcpy(moveList[currentMove - 1], user_move);  /* note side-effect */
}


int IsPromotion(fromX, fromY, toX, toY)
     int fromX, fromY, toX, toY;
{
    return gameMode != EditPosition &&
      fromX >=0 && fromY >= 0 && toX >= 0 && toY >= 0 &&
	((boards[currentMove][fromY][fromX] == WhitePawn && toY == 7) ||
	 (boards[currentMove][fromY][fromX] == BlackPawn && toY == 0));
}


int OKToStartUserMove(x, y)
     int x, y;
{
    ChessSquare from_piece;
    int white_piece;

    if (matchMode) return FALSE;
    if (gameMode == EditPosition) return TRUE;

    if (x >= 0 && y >= 0)
      from_piece = boards[currentMove][y][x];
    else
      from_piece = EmptySquare;

    if (from_piece == EmptySquare) return FALSE;

    white_piece = (int)from_piece >= (int)WhitePawn &&
                  (int)from_piece <= (int)WhiteKing;

    switch (gameMode) {
      case PlayFromGameFile:
      case AnalyzeFile:
      case TwoMachinesPlay:
      case EndOfGame:
	return FALSE;

      case IcsObserving:
      case IcsIdle:
	return FALSE;

      case MachinePlaysWhite:
      case IcsPlayingBlack:
	if (appData.zippyPlay) return FALSE;
	if (white_piece) {
	    DisplayError("You are playing Black", 0);
	    return FALSE;
	}
	break;

      case MachinePlaysBlack:
      case IcsPlayingWhite:
	if (appData.zippyPlay) return FALSE;
	if (!white_piece) {
	    DisplayError("You are playing White", 0);
	    return FALSE;
	}
	break;

      case EditGame:
	if (cmailMsgLoaded && (currentMove < cmailOldMove)) {
	    /* Editing correspondence game history */
	    /* Could disallow this or prompt for confirmation */
	    cmailOldMove = -1;
	}
	if (currentMove < forwardMostMove) {
	    /* Discarding moves */
	    /* Could prompt for confirmation here,
	       but I don't think that's such a good idea */
	    forwardMostMove = currentMove;
	}
	break;

      case BeginningOfGame:
	if (appData.icsActive) return FALSE;
	if (!appData.noChessProgram) {
	    if (!white_piece) {
		DisplayError("You are playing White", 0);
		return FALSE;
	    }
	}
	break;

      default:
      case IcsExamining:
	break;
    }
    if (currentMove != forwardMostMove && gameMode != AnalyzeMode
		&& gameMode != AnalyzeFile ) {
	DisplayError("Displayed position is not current", 0);
	return FALSE;
    }
    return TRUE;
}

FILE *lastLoadGameFP = NULL;
int lastLoadGameNumber = 0;
int lastLoadGameUseList = FALSE;
char lastLoadGameTitle[MSG_SIZ];
ChessMove lastLoadGameStart = (ChessMove) 0;

void UserMoveEvent(fromX, fromY, toX, toY, promoChar)
     int fromX, fromY, toX, toY;
     int promoChar;
{
    ChessMove moveType;
    
    if (fromX < 0 || fromY < 0) return;
    if ((fromX == toX) && (fromY == toY)) {
	return;
    }
	
    if (toX < 0 || toY < 0) {
	if (gameMode == EditPosition && (toX == -2 || toY == -2)) {
	    boards[0][fromY][fromX] = EmptySquare;
	    DrawPosition(FALSE, boards[currentMove]);
	}
	return;
    }
	
    if (gameMode == EditPosition) {
	boards[0][toY][toX] = boards[0][fromY][fromX];
	boards[0][fromY][fromX] = EmptySquare;
	DrawPosition(FALSE, boards[currentMove]);
	return;
    }
	
    /* Check if the user is playing in turn.  This is complicated because we
       let the user "pick up" a piece before it is his turn.  So the piece he
       tried to pick up may have been captured by the time he puts it down!
       Therefore we use the color the user is supposed to be playing in this
       test, not the color of the piece that is currently on the starting
       square---except in EditGame mode, where the user is playing both
       sides; fortunately there the capture race can't happen.  (It can
       now happen in IcsExamining mode, but that's just too bad.  The user
       will get a somewhat confusing message in that case.)
    */
    if ( gameMode != AnalyzeMode && gameMode != AnalyzeFile ) {
	if (gameMode == MachinePlaysWhite || gameMode == IcsPlayingBlack ||
	    ((gameMode == EditGame || gameMode == BeginningOfGame ||
	      gameMode == IcsExamining) && 
	     (int) boards[currentMove][fromY][fromX] >= (int) BlackPawn &&
	     (int) boards[currentMove][fromY][fromX] <= (int) BlackKing)) {
	    /* User is moving for Black */
	    if (WhiteOnMove(currentMove)) {
		DisplayError("It is White's turn", 0);
		return;
	    }
	} else {
	    /* User is moving for White */
	    if (!WhiteOnMove(currentMove)) {
		DisplayError("It is Black's turn", 0);
		return;
	    }
	}
    }

    if (appData.checkLegality) {
	moveType = LegalityTest(boards[currentMove], FakeFlags(currentMove),
				EP_UNKNOWN, fromY, fromX, toY, toX, promoChar);
	if (moveType == BadMove) {
	    DisplayError("Illegal move", 0);
	    return;
	}
    } else {
	moveType = PromoCharToMoveType(WhiteOnMove(currentMove), promoChar);
    }

    /* Ok, now we know that the move is good, so we can kill
       the previous line in Analysis Mode */
    if ( gameMode == AnalyzeMode && currentMove < forwardMostMove )
      forwardMostMove = currentMove;
	  
    /* If we need the chess program but it's dead, restart it */
    ResurrectChessProgram();

    /* A user move restarts a paused game*/
    if (pausing)
      PauseEvent();
    
    thinkOutput[0] = NULLCHAR;
    MakeMove(&moveType, fromX, fromY, toX, toY);

    if (appData.icsActive) {
	if (gameMode == IcsPlayingWhite || gameMode == IcsPlayingBlack ||
	    gameMode == IcsExamining) {
	    SendMoveToICS(moveType, fromX, fromY, toX, toY);
	    ics_user_moved = 1;
	}
    } else {
	SendMoveToProgram(moveType, fromX, fromY, toX, toY,
			  firstProgramPR, firstSendTime);

	if (currentMove == cmailOldMove + 1) {
	    cmailMoveType[lastLoadGameNumber - 1] = CMAIL_MOVE;
	}
    }
    
    switch (gameMode) {
      case BeginningOfGame:
	if (appData.noChessProgram)
	  lastGameMode = gameMode = EditGame;
	else
	  lastGameMode = gameMode = MachinePlaysBlack;
	SetGameInfo();
	ModeHighlight();
	break;

      case EditGame:
	switch (MateTest(boards[currentMove], FakeFlags(currentMove),
			 EP_UNKNOWN)) {
	  case MT_NONE:
	  case MT_CHECK:
	    break;
	  case MT_CHECKMATE:
	    if (WhiteOnMove(currentMove)) {
		GameEnds(BlackWins, "Black mates", GE_PLAYER);
	    } else {
		GameEnds(WhiteWins, "White mates", GE_PLAYER);
	    }
	    break;
	  case MT_STALEMATE:
	    GameEnds(GameIsDrawn, "Stalemate", GE_PLAYER);
	    break;
	}
	break;

      case MachinePlaysBlack:
      case MachinePlaysWhite:
      default:
	break;
    }
}

void HandleMachineMove(message, isr)
     char *message;
     InputSourceRef isr;
{
    char machineMove[MSG_SIZ], buf1[MSG_SIZ], buf2[MSG_SIZ];
	char realname[MSG_SIZ];
    int fromX, fromY, toX, toY;
    ChessMove moveType;
    char promoChar;
    char *p;
    
    if (strncmp(message, "warning:", 8) == 0) {
	DisplayError(message, 0);
	return;
    }
    
    /*
     * If chess program startup fails, exit with an error message.
     * Attempts to recover here are futile.
     */
    if ((StrStr(message, "unknown host") != NULL)
	|| (StrStr(message, "No remote directory") != NULL)
	|| (StrStr(message, "not found") != NULL)
	|| (StrStr(message, "No such file") != NULL)
	|| (StrStr(message, "can't alloc") != NULL)
	|| (StrStr(message, "Permission denied") != NULL)) {
	if (isr != secondProgramISR) {
	    firstMaybePondering = FALSE;
	    sprintf(buf1,
		"Failed to start chess program %s on %s: %s\n",
		appData.firstChessProgram, appData.firstHost, message);
	} else {
	    secondMaybePondering = FALSE;
	    sprintf(buf1,
		"Failed to start second chess program %s on %s: %s\n",
		appData.secondChessProgram, appData.secondHost, message);
	}
	RemoveInputSource(isr);
	DisplayFatalError(buf1, 0, 1);
	return;
    }
    
    /*
     * If the move is illegal, cancel it and redraw the board.
     * We don't accept "illegal move" messages (which Crafty
     * produces), because many crafty versions have some weird
     * bug that makes them print bogus "illegal move" messages
     * after some perfectly legal moves.
     */
#if CRAFTY_FIXED
    if (strncmp(message + 1, "llegal move", 11) == 0 &&
#else
    if (strncmp(message, "Illegal move", 12) == 0 &&
#endif
	!StrStr(message, "bogus") && !StrStr(message, "name") &&
	!StrStr(message, "draw") && !StrStr(message, "rating")) {
	if (isr == firstProgramISR)
	  firstMaybePondering = FALSE;
	else
	  secondMaybePondering = FALSE;

	if (StrStr(message, "time") || StrStr(message, "otim") ||
	    StrStr(message, timeTestStr)) {
	    if (isr == firstProgramISR) {
		/* First program doesn't have "time" or "otim" command */
		firstSendTime = 0;
		return;
	    } else if (isr == secondProgramISR) {
		/* Second program doesn't have "time" or "otim" command */
		secondSendTime = 0;
		return;
	    }
	}
	if (forwardMostMove <= backwardMostMove) return;
	if (WhiteOnMove(forwardMostMove) !=
	    ( (gameMode == MachinePlaysWhite && isr == firstProgramISR) ||
	      (gameMode == TwoMachinesPlay && isr == secondProgramISR) )) {
	    /* Bogus complaint about gnuchess's own hint move; gnuchess bug */
#ifndef ZIPPY
	    sprintf(buf1, "%s chess program gave bogus error message:\n %s",
		    isr == firstProgramISR ? "first" : "second", message);
	    DisplayFatalError(buf1, 0, -1); /* don't exit */
#endif /*ZIPPY*/
	    return;
	}
	if (pausing) PauseEvent();
	if (gameMode == PlayFromGameFile) {
	    /* Stop reading this game file */
	    gameMode = EditGame;
	    ModeHighlight();
	}
	currentMove = --forwardMostMove;
	SwitchClocks();
	sprintf(buf1, "Illegal move \"%s\"\n(rejected by %schess program)",
		parseList[currentMove],
		isr == firstProgramISR ? "first " :
		(isr == secondProgramISR ? "second " : ""));
	DisplayError(buf1, 0);
	
	DrawPosition(FALSE, boards[currentMove]);
	return;
    }
    
    if (strncmp(message, "time", 4) == 0 && StrStr(message, "CHESS")) {
	/* Program has a broken "time" command that
	   outputs a string not ending in newline.
	   Don't use it. */
	if (isr == firstProgramISR) firstSendTime = 0;
	if (isr == secondProgramISR) secondSendTime = 0;
    }
    
    if (strncmp(message, "Hint:", 5) == 0) {
	sscanf(message, "Hint: %s", lastHint);
	if (hintRequested) {
	    hintRequested = FALSE;
	    if (ParseMachineMove(lastHint, forwardMostMove, &moveType,
				 &fromX, &fromY, &toX, &toY, &promoChar)) {
		moveType =
		  CoordsToAlgebraic(boards[forwardMostMove],
				    FakeFlags(forwardMostMove), EP_UNKNOWN,
				    fromY, fromX, toY, toX, promoChar, buf1);
		sprintf(buf2, "Hint: %s", buf1);
		DisplayInformation(buf2);
	    } else {
		/* Hint move was illegal!? */
		sprintf(buf1, "Illegal hint move \"%s\"\nfrom %schess program",
			lastHint, isr == firstProgramISR ? "first " :
			(isr == secondProgramISR ? "second " : ""));
		DisplayError(buf1, 0);
	    }
	}
	return;
    }
    
    /*
     * win, lose or draw
     */
    if (strncmp(message, "White", 5) == 0) {
	GameEnds(WhiteWins, "White mates", GE_GNU);
	return;
    } else if (strncmp(message, "Black", 5) == 0) {
	GameEnds(BlackWins, "Black mates", GE_GNU);
	return;
    } else if (strncmp(message, "opponent mates!", 15) == 0) {
	switch (gameMode) {
	  case MachinePlaysBlack:
	    GameEnds(WhiteWins, "White mates", GE_GNU);
	    break;
	  case MachinePlaysWhite:
	    GameEnds(BlackWins, "Black mates", GE_GNU);
	    break;
	  case TwoMachinesPlay:
	    if (isr == firstProgramISR)
	      GameEnds(WhiteWins, "White mates", GE_GNU);
	    else
	      GameEnds(BlackWins, "Black mates", GE_GNU);
	    break;
	  default:
	    /* can't happen */
	    break;
	}
	return;
    } else if (strncmp(message, "computer mates!", 15) == 0) {
	switch (gameMode) {
	  case MachinePlaysBlack:
	    GameEnds(BlackWins, "Black mates", GE_GNU);
	    break;
	  case MachinePlaysWhite:
	    GameEnds(WhiteWins, "White mates", GE_GNU);
	    break;
	  case TwoMachinesPlay:
	    if (isr == firstProgramISR)
	      GameEnds(BlackWins, "Black mates", GE_GNU);
	    else
	      GameEnds(WhiteWins, "White mates", GE_GNU);
	    break;
	  default:
	    /* can't happen */
	    break;
	}
	return;
    } else if (strncmp(message, "Draw", 4) == 0) {
#ifdef ZIPPY
	if (appData.zippyPlay) {
	    /* If this was a draw by repetition or the
	       50-move rule, we have to claim it.
	       */
	    SendToICS("draw\n");
	}
#endif
	GameEnds(GameIsDrawn, "Draw", GE_GNU);
	return;
    }    

    if (appData.showThinking) {
	int plylev, mvleft, mvtot, curscore, time, nodes;
	char plyext;
	int ignore = FALSE;
	int prefixHint = FALSE;

	switch (gameMode) {
	  case MachinePlaysBlack:
	  case IcsPlayingBlack:
	    if (WhiteOnMove(forwardMostMove)) prefixHint = TRUE;
	    break;
	  case MachinePlaysWhite:
	  case IcsPlayingWhite:
	    if (!WhiteOnMove(forwardMostMove)) prefixHint = TRUE;
	    break;
	  case AnalyzeMode:
	  case AnalyzeFile:
		break;
	  case TwoMachinesPlay:
	    if ((isr == firstProgramISR) == WhiteOnMove(forwardMostMove))
	      ignore = TRUE;
	    break;
	  default:
	    ignore = TRUE;
	    break;
	}

	if (!ignore) {
	    if (sscanf(message, "%d%c %d %d %d\n",
		       &plylev, &plyext, &curscore, &time, &nodes) == 5) {
		  programStats.depth = plylev;
		  programStats.nodes = nodes;
		  programStats.time = time;
		  programStats.score = curscore;
		  strcpy(programStats.movelist, &message[27]);
		  programStats.got_only_move = 0;

		  if ( programStats.seen_stat )
			programStats.ok_to_send = 1;
		  
		  if ( strchr( programStats.movelist, '(' ) != NULL )
			{
			  programStats.line_is_book = 1;
			  programStats.nr_moves = 0;
			  programStats.moves_left = 0;
			}
		  else
			programStats.line_is_book = 0;
		  
		sprintf(thinkOutput, "depth=%d %s%+.2f  %s%s%s",
				plylev, 
			(gameMode == TwoMachinesPlay ?
			 (isr == firstProgramISR ? "B" : "W") : ""),
			((double) curscore) / 100.0,
			prefixHint ? lastHint : "",
			prefixHint ? "  " : "",
			&message[27] );

		if (currentMove == forwardMostMove || gameMode==AnalyzeMode ||
			gameMode==AnalyzeFile ) {
		  DisplayMove(currentMove - 1);
		  DisplayAnalysis();
		}
		return;
	    }
		/* crafty (9.25+) will say "(only move) <move>" if only 1 legal move */
		else if ((p=StrStr(message, "(only move)")) != NULL ) {
		  sscanf(p, "(only move) %s", buf1 );
		  sprintf(thinkOutput, "%s (only move)", buf1 );
		  sprintf(programStats.movelist, "%s (only move)", buf1 );
		  programStats.depth = 1;
		  programStats.nr_moves = 1;
		  programStats.moves_left = 1;
		  programStats.nodes = 1;
		  programStats.time = 1;
		  programStats.got_only_move = 1;

		  /* Not really, but we also use this member to
			 mean "line isn't going to change" (Crafty
			 isn't searching, so stats won't change) */
		  programStats.line_is_book = 1;
		  
		if (currentMove == forwardMostMove || gameMode==AnalyzeMode ||
			gameMode == AnalyzeFile ) {
		  DisplayMove(currentMove - 1);
		  DisplayAnalysis();
		}
		return;
	    }
		/* The stat01: line is from Crafty (9.29+) in response
		   to the "." command */
		else if (sscanf(message,"stat01: %d %d %d %d %d",
						&time, &nodes, &plylev, &mvleft,
						&mvtot ) == 5 )
		  {
			programStats.seen_stat = 1;

			if ( programStats.got_only_move || appData.periodicUpdates==False )
			  return;

			programStats.depth = plylev;
			programStats.time = time;
			programStats.nodes = nodes;
			programStats.moves_left = mvleft;
			programStats.nr_moves = mvtot;
			programStats.ok_to_send = 1;
			DisplayAnalysis();
			return;
		  }
		/* Crafty 9.29+ outputs this */
		else if (strncmp(message,"++",2) == 0) {
		  programStats.got_fail = 2;
		return;
		}
		/* Crafty 9.29+ outputs this */
		else if (strncmp(message,"--",2) == 0) {
		  programStats.got_fail = 1;
		  return;
		}
		else if (thinkOutput[0] != NULLCHAR &&
		       strncmp(message,
			       "                           ", 27) == 0) {
		strcat(thinkOutput, &message[26]);
		strcpy( programStats.movelist, &message[26] );
		if (currentMove == forwardMostMove || gameMode==AnalyzeMode ||
			gameMode == AnalyzeFile ) {
		  DisplayMove(currentMove - 1);
		  DisplayAnalysis();
		}
		return;
	    }
	}
    }

    if (bookRequested) {
	if (message[0] == '\t' || message[0] == ' ') {
	    /* Part of the book output is here; append it */
	    strcat(bookOutput, message);
	    strcat(bookOutput, "  \n");
	    return;
	} else if (bookOutput[0] != NULLCHAR) {
	    /* All of book output has arrived; display it */
	    char *p = bookOutput;
	    while (*p != NULLCHAR) {
		if (*p == '\t') *p = ' ';
		p++;
	    }
	    DisplayInformation(bookOutput);
	    bookRequested = FALSE;
	    /* Fall through to parse the current output */
	}
    }

    /* Extract Crafty version number */
    if (!strncmp(message,"kibitz",6) &&
	sscanf(message, "kibitz Hello from %s v%d.%d",
	       realname, &craftyMajorVersion,
	       &craftyMinorVersion ) == 3 )
      {
	  isCraftyRunning = 1;
	  sprintf( craftyProgramName, "%s %d.%d",
		  realname, craftyMajorVersion, craftyMinorVersion );
      }
    
    /* Argh! RH changed the "Hello" message with the Jakarta
       version of Crafty. Oh well... deal with it :) */
    if (!strncmp(message,"kibitz",6) &&
	StrStr(message,"(Jakarta)") != NULL )
      {
	  isCraftyRunning = 1;
	  strcpy( craftyProgramName, "Crafty <Jakarta>" );
	  craftyMajorVersion = 10;
	  craftyMinorVersion = 18;
      }
	    
    /* rmh:  allow "whisper" command to be sent directly to ics
       from the chess program so that it can let observers know
       what's going on.  also allow "resign" ,"draw" and kibitz since
       crafty can resign and will soon offer a draw, and it can now
       announce mate to its opponent.
       Note - We don't allow these to be sent until we are logged
       on. Crafty kibitzes have been known to interfere with the
       login process. */
    if ( loggedOn == TRUE ) {
    if (!strncmp(message,"whisper",7) || !strncmp(message,"resign",6) ||
	  !strncmp(message,"kibitz",6)  || !strncmp(message,"draw",4) ||
	  !strncmp(message,"tell",3)) {
	SendToICS(message);
	SendToICS("\n");
	return;
    }
    }

    /*
     * Now either the output is a machine move, or it's noise.
     */
    if (StrStr(message, "...") == NULL) return; /* noise */
    sscanf(message, "%s %s %s", buf1, buf2, machineMove);
    if (machineMove[0] == NULLCHAR) return; /* more noise */

    hintRequested = FALSE;
    bookRequested = FALSE;
    if (isr == firstProgramISR)
      firstMaybePondering = TRUE;
    else
      secondMaybePondering = TRUE;
    if (firstSendTime == 2 && isr == firstProgramISR) firstSendTime = 1;
    if (secondSendTime == 2 && isr == secondProgramISR) secondSendTime = 1;

    if (!ParseMachineMove(machineMove, forwardMostMove, &moveType,
			  &fromX, &fromY, &toX, &toY, &promoChar)) {
	/* Machine move was illegal!?  Give message and continue. */
	sprintf(buf1, "Illegal move \"%s\" from machine",
		machineMove);
	DisplayError(buf1, 0);
    }
    
    /* currentMoveString is set as a side-effect of ParseMachineMove */
    strcpy(machineMove, currentMoveString);
    strcat(machineMove, "\n");
    strcpy(moveList[forwardMostMove], machineMove);
    
    if (!pausing)
      currentMove = forwardMostMove;  /*display latest move*/
    
    MakeMove(&moveType, fromX, fromY, toX, toY);
    
    if (!pausing && appData.ringBellAfterMoves)
      RingBell();
    
#ifdef ZIPPY
    if (gameMode == IcsPlayingWhite || gameMode == IcsPlayingBlack) {
	SendMoveToICS(moveType, fromX, fromY, toX, toY);
	ics_user_moved = 1;
    }
#endif

    if (gameMode == TwoMachinesPlay) {
	if (WhiteOnMove(forwardMostMove)) {
	    if (secondSendTime) 
	      SendTimeRemaining(secondProgramPR);
	    SendToProgram(machineMove, secondProgramPR);
	    if (firstMove) {
		firstMove = FALSE;
		SendToProgram(appData.whiteString, secondProgramPR);
	    }
	} else {
	    if (firstSendTime)
	      SendTimeRemaining(firstProgramPR);
	    SendToProgram(machineMove, firstProgramPR);
	    if (firstMove) {
		firstMove = FALSE;
		SendToProgram(appData.blackString, firstProgramPR);
	    }
	}
    }
}


/* Parse a game score from the character string "game", and
   record it as the history of the current game.  The game
   score is NOT assumed to start from the standard position. 
   The display is not updated in any way.
   */
void ParseGameHistory(game)
     char *game;
{
    ChessMove moveType;
    int fromX, fromY, toX, toY, boardIndex;
    char promoChar;
    char *p, *q;
    char buf[MSG_SIZ];

    if (appData.debugMode)
      fprintf(debugFP, "Parsing game history: %s\n", game);

    gameInfo.event = StrSave("ICS game");
    gameInfo.site = StrSave(appData.icsHost);
    gameInfo.date = PGNDate();
    gameInfo.round = StrSave("-");

    /* Parse out names of players */
    while (*game == ' ') game++;
    p = buf;
    while (*game != ' ') *p++ = *game++;
    *p = NULLCHAR;
    gameInfo.white = StrSave(buf);
    while (*game == ' ') game++;
    p = buf;
    while (*game != ' ' && *game != '\n') *p++ = *game++;
    *p = NULLCHAR;
    gameInfo.black = StrSave(buf);

    /* Parse moves */
    boardIndex = 0;
    yynewstr(game);
    for (;;) {
	yyboardindex = boardIndex;
	moveType = (ChessMove) yylex();
	switch (moveType) {
	  case WhitePromotionQueen:
	  case BlackPromotionQueen:
	  case WhitePromotionRook:
	  case BlackPromotionRook:
	  case WhitePromotionBishop:
	  case BlackPromotionBishop:
	  case WhitePromotionKnight:
	  case BlackPromotionKnight:
	  case NormalMove:
	  case WhiteCapturesEnPassant:
	  case BlackCapturesEnPassant:
	  case WhiteKingSideCastle:
	  case WhiteQueenSideCastle:
	  case BlackKingSideCastle:
	  case BlackQueenSideCastle:
	  case WhiteKingSideCastleWild:
	  case WhiteQueenSideCastleWild:
	  case BlackKingSideCastleWild:
	  case BlackQueenSideCastleWild:
	    fromX = currentMoveString[0] - 'a';
	    fromY = currentMoveString[1] - '1';
	    toX = currentMoveString[2] - 'a';
	    toY = currentMoveString[3] - '1';
	    promoChar = currentMoveString[4];
	    break;
	  case WhiteDrop:
	  case BlackDrop:
	    fromX = moveType == WhiteDrop ?
	      (int) CharToPiece(ToUpper(currentMoveString[0])) :
		(int) CharToPiece(ToLower(currentMoveString[0]));
	    fromY = DROP_RANK;
	    toX = currentMoveString[2] - 'a';
	    toY = currentMoveString[3] - '1';
	    promoChar = NULLCHAR;
	    break;
	  case BadMove:
	    /* bug? */
	    sprintf(buf, "Bad move in ICS output: \"%s\"", yy_text);
	    DisplayError(buf, 0);
	    return;
	  case AmbiguousMove:
	    /* bug? */
	    sprintf(buf, "Ambiguous move in ICS output: \"%s\"", yy_text);
	    DisplayError(buf, 0);
	    return;
	  case (ChessMove) 0:	/* end of file */
	    if (boardIndex < backwardMostMove) {
		/* Oops, gap.  How did that happen? */
		return;
	    }
	    backwardMostMove = 0;
	    if (boardIndex > forwardMostMove) {
		forwardMostMove = boardIndex;
	    }
	    return;
	  case ElapsedTime:
	    if (boardIndex > 0) {
		strcat(parseList[boardIndex-1], " ");
		strcat(parseList[boardIndex-1], yy_text);
	    }
	    continue;
	  case Comment:
	  case PGNTag:
	  default:
	    /* ignore */
	    continue;
	  case WhiteWins:
	  case BlackWins:
	  case GameIsDrawn:
	  case GameUnfinished:
	    if (gameMode == IcsExamining) {
		if (boardIndex < backwardMostMove) {
		    /* Oops, gap.  How did that happen? */
		    return;
		}
		backwardMostMove = 0;
		return;
	    }
	    gameInfo.result = moveType;
	    p = strchr(yy_text, '{');
	    if (p == NULL) p = strchr(yy_text, '(');
	    if (p == NULL) {
		p = yy_text;
		if (p[0] == '0' || p[0] == '1' || p[0] == '*') p = "";
	    } else {
		q = strchr(p, *p == '{' ? '}' : ')');
		if (q != NULL) *q = NULLCHAR;
		p++;
	    }
	    gameInfo.resultDetails = StrSave(p);
	    continue;
	}
	if (boardIndex >= forwardMostMove &&
	    !(gameMode == IcsObserving && ics_gamenum == -1)) {
	    backwardMostMove = 0;
	    return;
	}
	(void) CoordsToAlgebraic(boards[boardIndex], FakeFlags(boardIndex),
				 EP_UNKNOWN, fromY, fromX, toY, toX, promoChar,
				 parseList[boardIndex]);
	CopyBoard(boards[boardIndex + 1], boards[boardIndex]);
	boardIndex++;
	ApplyMove(&moveType, fromX, fromY, toX, toY,
		  boards[boardIndex]);
	switch (MateTest(boards[boardIndex],
			 FakeFlags(boardIndex), EP_UNKNOWN)) {
	  case MT_NONE:
	  case MT_STALEMATE:
	  default:
	    break;
	  case MT_CHECK:
	    strcat(parseList[boardIndex - 1], "+");
	    break;
	  case MT_CHECKMATE:
	    strcat(parseList[boardIndex - 1], "#");
	    break;
	}
    }
}


/* Apply a move to the given board.  Oddity: moveType is ignored on
   input unless the move is seen to be a pawn promotion, in which case
   moveType tells us what to promote to.
*/
void ApplyMove(moveType, fromX, fromY, toX, toY, board)
     ChessMove *moveType;
     int fromX, fromY, toX, toY;
     Board board;
{
    if (fromY == 0 && fromX == 4
	&& board[fromY][fromX] == WhiteKing
	&& toY == 0 && toX == 6) {
	*moveType = WhiteKingSideCastle;
 	board[fromY][fromX] = EmptySquare;
 	board[toY][toX] = WhiteKing;
 	board[fromY][7] = EmptySquare;
 	board[toY][5] = WhiteRook;
    } else if (fromY == 0 && fromX == 4
	       && board[fromY][fromX] == WhiteKing
	       && toY == 0 && toX == 2) {
	*moveType = WhiteQueenSideCastle;
 	board[fromY][fromX] = EmptySquare;
 	board[toY][toX] = WhiteKing;
 	board[fromY][0] = EmptySquare;
 	board[toY][3] = WhiteRook;
    } else if (fromY == 0 && fromX == 3
 	&& board[fromY][fromX] == WhiteKing
 	&& toY == 0 && toX == 5) {
 	*moveType = WhiteKingSideCastleWild;
 	board[fromY][fromX] = EmptySquare;
 	board[toY][toX] = WhiteKing;
 	board[fromY][7] = EmptySquare;
 	board[toY][4] = WhiteRook;
    } else if (fromY == 0 && fromX == 3
 	       && board[fromY][fromX] == WhiteKing
 	       && toY == 0 && toX == 1) {
 	*moveType = WhiteQueenSideCastleWild;
 	board[fromY][fromX] = EmptySquare;
 	board[toY][toX] = WhiteKing;
 	board[fromY][0] = EmptySquare;
 	board[toY][2] = WhiteRook;
    } else if (fromY == 6
	       && board[fromY][fromX] == WhitePawn
	       && toY == 7) {
	/* white pawn promotion */
	board[7][toX] = PromoPiece(*moveType);
	if (board[7][toX] == EmptySquare) {
	    board[7][toX] = WhiteQueen;
	    *moveType = WhitePromotionQueen;
	}
	board[6][fromX] = EmptySquare;
    } else if ((fromY == 4)
	       && (toX != fromX)
	       && (board[fromY][fromX] == WhitePawn)
	       && (board[toY][toX] == EmptySquare)) {
	*moveType = WhiteCapturesEnPassant;
	board[fromY][fromX] = EmptySquare;
	board[toY][toX] = WhitePawn;
	board[toY - 1][toX] = EmptySquare;
   } else if (fromY == 7 && fromX == 4
	       && board[fromY][fromX] == BlackKing
	       && toY == 7 && toX == 6) {
	*moveType = BlackKingSideCastle;
 	board[fromY][fromX] = EmptySquare;
 	board[toY][toX] = BlackKing;
 	board[fromY][7] = EmptySquare;
 	board[toY][5] = BlackRook;
   } else if (fromY == 7 && fromX == 4
	       && board[fromY][fromX] == BlackKing
	       && toY == 7 && toX == 2) {
	*moveType = BlackQueenSideCastle;
 	board[fromY][fromX] = EmptySquare;
 	board[toY][toX] = BlackKing;
 	board[fromY][0] = EmptySquare;
 	board[toY][3] = BlackRook;
    } else if (fromY == 7 && fromX == 3
 	       && board[fromY][fromX] == BlackKing
 	       && toY == 7 && toX == 5) {
 	*moveType = BlackKingSideCastleWild;
 	board[fromY][fromX] = EmptySquare;
 	board[toY][toX] = BlackKing;
 	board[fromY][7] = EmptySquare;
 	board[toY][4] = BlackRook;
    } else if (fromY == 7 && fromX == 3
 	       && board[fromY][fromX] == BlackKing
 	       && toY == 7 && toX == 1) {
 	*moveType = BlackQueenSideCastleWild;
 	board[fromY][fromX] = EmptySquare;
 	board[toY][toX] = BlackKing;
 	board[fromY][0] = EmptySquare;
 	board[toY][2] = BlackRook;
    } else if (fromY == 1
	       && board[fromY][fromX] == BlackPawn
	       && toY == 0) {
	/* black pawn promotion */
	board[0][toX] = PromoPiece(*moveType);
	if (board[0][toX] == EmptySquare) {
	    board[0][toX] = BlackQueen;
	    *moveType = BlackPromotionQueen;
	}
	board[1][fromX] = EmptySquare;
    } else if ((fromY == 3)
	       && (toX != fromX)
	       && (board[fromY][fromX] == BlackPawn)
	       && (board[toY][toX] == EmptySquare)) {
	*moveType = BlackCapturesEnPassant;
	board[fromY][fromX] = EmptySquare;
	board[toY][toX] = BlackPawn;
	board[toY + 1][toX] = EmptySquare;
    } else if (fromY == DROP_RANK) {
	board[toY][toX] = (ChessSquare) fromX;
    } else {
	*moveType = NormalMove;
	board[toY][toX] = board[fromY][fromX];
	board[fromY][fromX] = EmptySquare;
    }
}

/*
 * MakeMove() displays moves.  If they are illegal, GNU chess will detect
 * this and send an Illegal move message.  XBoard will then retract the move.
 *
 * Oddity: moveType is ignored on input unless the move is seen to be a
 * pawn promotion, in which case moveType tells us what to promote to.
 */
void MakeMove(moveType, fromX, fromY, toX, toY)
     ChessMove *moveType;
     int fromX, fromY, toX, toY;
{
    forwardMostMove++;
    if (commentList[forwardMostMove] != NULL) {
	free(commentList[forwardMostMove]);
	commentList[forwardMostMove] = NULL;
    }
    CopyBoard(boards[forwardMostMove], boards[forwardMostMove - 1]);
    ApplyMove(moveType, fromX, fromY, toX, toY,
	      boards[forwardMostMove]);
    gameInfo.result = GameUnfinished;
    if (gameInfo.resultDetails != NULL) {
	free(gameInfo.resultDetails);
	gameInfo.resultDetails = NULL;
    }
    (void) CoordsToAlgebraic(boards[forwardMostMove - 1],
			     FakeFlags(forwardMostMove - 1), EP_UNKNOWN,
			     fromY, fromX, toY, toX,
			     ToLower(PieceToChar(PromoPiece(*moveType))),
			     parseList[forwardMostMove - 1]);
    switch (MateTest(boards[forwardMostMove],
		     FakeFlags(forwardMostMove), EP_UNKNOWN)){
      case MT_NONE:
      case MT_STALEMATE:
      default:
	break;
      case MT_CHECK:
	strcat(parseList[forwardMostMove - 1], "+");
	break;
      case MT_CHECKMATE:
	strcat(parseList[forwardMostMove - 1], "#");
	break;
    }
    if (!pausing || gameMode == PlayFromGameFile || gameMode == AnalyzeFile) {
	currentMove = forwardMostMove;
    }

    SwitchClocks();
    timeRemaining[0][forwardMostMove] = whiteTimeRemaining;
    timeRemaining[1][forwardMostMove] = blackTimeRemaining;

    if (gameMode == PlayFromGameFile ?
	matchMode || (appData.timeDelay == 0 && !pausing) : pausing) return;

    DisplayMove(currentMove - 1);
    DrawPosition(FALSE, boards[currentMove]);
}

void InitChessProgram(hostName, programName, pr, isr, sendTime)
     char *hostName, *programName;
     ProcRef *pr;
     InputSourceRef *isr;
     int *sendTime;
{
    char cmdLine[MSG_SIZ];
    char buf[MSG_SIZ];
    int err;
    
    if (appData.noChessProgram) return;
    
    if (*appData.searchTime != NULLCHAR) {
	/* With gnuchess this option works only on the command line */
	sprintf(cmdLine, "%s %d", programName, searchTime);
    } else {
	strcpy(cmdLine, programName);
    }

    if (strcmp(hostName, "localhost") == 0) {
	err = StartChildProcess(cmdLine, pr);
    } else if (*appData.remoteShell == NULLCHAR) {
	err = OpenRcmd(hostName, appData.remoteUser, cmdLine, pr);
    } else {
	if (*appData.remoteUser == NULLCHAR) {
	    sprintf(buf, "%s %s %s", appData.remoteShell, hostName, cmdLine);
	} else {
	    sprintf(buf, "%s -l %s %s %s", appData.remoteShell,
		    appData.remoteUser, hostName, cmdLine);
	}
	err = StartChildProcess(buf, pr);
    }
    
    if (err != 0) {
	sprintf(buf, "Startup failure on '%s'", programName);
	DisplayFatalError(buf, err, -1); /* don't exit */
	*pr = NoProc;
	*isr = NULL;
	return;
    }
    
    *isr = AddInputSource(*pr, TRUE, ReceiveFromProgram);

    hintRequested = FALSE;
    bookRequested = FALSE;
    if (*isr == firstProgramISR)
      firstMaybePondering = FALSE;
    else
      secondMaybePondering = FALSE;
    SendToProgram(appData.initString, *pr);
    SendTimeControl(*pr, appData.movesPerSession, timeControl,
		    appData.timeIncrement, appData.searchDepth,
		    searchTime);
    if (appData.showThinking) {
        SendToProgram("post\n", *pr);
    }
    
    if (*sendTime == 2) {
	/* Does program have "time" and "otim" commands? */
	char buf[MSG_SIZ];
	sprintf(timeTestStr, "%ld", timeControl/10);
	sprintf(buf, "time %s\notim %s\nbogus\n", timeTestStr, timeTestStr);
	/* "bogus" is a kludge to work around a gnuchess bug;
	   some versions do not send a newline at the end of
	   their response to the time command.  No known version 
	   of gnuchess actually implements a "bogus" command, but
	   the error message it provokes ends with a newline! */
	SendToProgram(buf, *pr);
    }
}


void GameEnds(result, resultDetails, whosays)
     ChessMove result;
     char *resultDetails;
     int whosays;
{
    char *quit = "quit\n";
    int dummy;

    /* If we're loading the game from a file, stop */
    (void) StopLoadGameTimer();
    if (gameFileFP != NULL) {
	gameFileFP = NULL;
    }

    /* If this is an ICS game, only ICS can really say it's done;
       if not, anyone can. */
    if (!((gameMode == IcsPlayingWhite || gameMode == IcsPlayingBlack
	   || gameMode == IcsObserving || gameMode == IcsExamining)
	  && whosays != GE_ICS)) {
	StopClocks();
    
	if (resultDetails != NULL) {
	    gameInfo.result = result;
	    gameInfo.resultDetails = StrSave(resultDetails);

	    if (currentMove == forwardMostMove)
	      DisplayMove(currentMove - 1);
    
	    if (forwardMostMove != 0) {
		if (gameMode != PlayFromGameFile && gameMode != EditGame) {
		    if (*appData.saveGameFile != NULLCHAR) {
			SaveGameToFile(appData.saveGameFile);
		    } else if (appData.autoSaveGames) {
			AutoSaveGame();
		    }
		    if (*appData.savePositionFile != NULLCHAR) {
			SavePositionToFile(appData.savePositionFile);
		    }
		}
	    }
	}

	if (appData.icsActive) {
	    if (appData.quietPlay &&
		(gameMode == IcsPlayingWhite ||
		 gameMode == IcsPlayingBlack)) {
		SendToICS("set shout 1\n");
	    }
	    gameMode = IcsIdle;
	    ics_user_moved = FALSE;
	} else if (gameMode != EditGame) {
	    lastGameMode = gameMode;
	    gameMode = EndOfGame;
	}
	pausing = FALSE;
	ModeHighlight();
    }

    if (appData.noChessProgram) return;
    if (result == GameUnfinished) return;

    /* Kill off chess programs */
    if (firstProgramISR != NULL)
      RemoveInputSource(firstProgramISR);
    firstProgramISR = NULL;
    
    if (firstProgramPR != NoProc) {
	InterruptChildProcess(firstProgramPR);
	OutputToProcess(firstProgramPR, quit, strlen(quit), &dummy);
	DestroyChildProcess(firstProgramPR);
    }
    firstProgramPR = NoProc;
    
    if (secondProgramISR != NULL)
      RemoveInputSource(secondProgramISR);
    secondProgramISR = NULL;
    
    if (secondProgramPR != NoProc) {
	InterruptChildProcess(secondProgramPR);
	OutputToProcess(secondProgramPR, quit, strlen(quit), &dummy);
	DestroyChildProcess(secondProgramPR);
    }
    secondProgramPR = NoProc;
    
    if (matchMode) {
	exit(0);
    }
}


void ResurrectChessProgram()
     /* The chess program may have exited.
        If so, restart it and feed it all the moves made so far. */
{
    int i;
    
    if (appData.noChessProgram || firstProgramPR != NoProc) return;
    
    InitChessProgram(appData.firstHost, appData.firstChessProgram,
		     &firstProgramPR, &firstProgramISR, &firstSendTime);
    SendToProgram("force\n", firstProgramPR);
    
    if (startedFromSetupPosition) {
	if (backwardMostMove % 2 == 1)
	  SendToProgram("a2a3\n", firstProgramPR);
	SendBoard(firstProgramPR, boards[backwardMostMove]);
    }
    
    for (i = backwardMostMove; i < currentMove; i++) {
	SendToProgram(moveList[i], firstProgramPR);
    }
    
    if (!firstSendTime) {
	/* can't tell gnuchess what its clock should read,
	   so we bow to its notion. */
	ResetClocks();
	timeRemaining[0][currentMove] = whiteTimeRemaining;
	timeRemaining[1][currentMove] = blackTimeRemaining;
    }
}

/*
 * Button procedures
 */
void Reset(redraw)
     int redraw;
{
    int i;

    pausing = pauseExamInvalid = FALSE;
    flipView = appData.flipView;
    startedFromSetupPosition = blackPlaysFirst = FALSE;
    firstMove = TRUE;
    whiteFlag = blackFlag = FALSE;
    firstMaybePondering = FALSE;
    secondMaybePondering = FALSE;
    thinkOutput[0] = NULLCHAR;
    lastHint[0] = NULLCHAR;
    ClearGameInfo(&gameInfo);
    ics_user_moved = ics_clock_paused = FALSE;
    ics_getting_history = H_FALSE;
    ics_gamenum = -1;
    ics_bughouse = FALSE;
    white_holding[0] = black_holding[0] = NULLCHAR;
    
    ResetFrontEnd();

    GameEnds((ChessMove) 0, NULL, GE_PLAYER);
    lastGameMode = gameMode = BeginningOfGame;
    ModeHighlight();
    InitPosition(redraw);
    for (i = 0; i < MAX_MOVES; i++) {
	if (commentList[i] != NULL) {
	    free(commentList[i]);
	    commentList[i] = NULL;
	}
    }
    ResetClocks();
    timeRemaining[0][0] = whiteTimeRemaining;
    timeRemaining[1][0] = blackTimeRemaining;
    InitChessProgram(appData.firstHost, appData.firstChessProgram,
		     &firstProgramPR, &firstProgramISR, &firstSendTime);
    DisplayTitle("");
    DisplayMessage("", "");
}


void LoadGameLoop()
{
    for (;;) {
	if (!LoadGameOneMove())
	  return;
	if (matchMode || appData.timeDelay == 0)
	  continue;
	if (appData.timeDelay < 0 || gameMode == AnalyzeFile )
	  return;
        StartLoadGameTimer((long)(1000.0 * appData.timeDelay));
	break;
    }
}

int LoadGameOneMove()
{
    int fromX = 0, fromY = 0, toX = 0, toY = 0, done, editAfterDone;
    ChessMove moveType;
    char move[MSG_SIZ];
    char *p, *q;
    
    if (gameFileFP == NULL)
      return FALSE;
    
    if (gameMode != PlayFromGameFile && gameMode != AnalyzeFile ) {
	gameFileFP = NULL;
	return FALSE;
    }
    
    yyboardindex = forwardMostMove;
    moveType = (ChessMove) yylex();
    
    done = FALSE;
    editAfterDone = TRUE;
    switch (moveType) {
      case Comment:
	if (appData.debugMode) 
	  fprintf(debugFP, "Parsed Comment: %s\n", yy_text);
	p = yy_text;
	if (*p == '{' || *p == '[' || *p == '(') {
	    p[strlen(p) - 1] = NULLCHAR;
	    p++;
	}
	while (*p == '\n') p++;
	if (!matchMode && (pausing || appData.timeDelay != 0)) {
	    DisplayComment(currentMove - 1, p);
	}
	AppendComment(currentMove, p);
	return TRUE;

      case WhiteCapturesEnPassant:
      case BlackCapturesEnPassant:
      case WhitePromotionQueen:
      case BlackPromotionQueen:
      case WhitePromotionRook:
      case BlackPromotionRook:
      case WhitePromotionBishop:
      case BlackPromotionBishop:
      case WhitePromotionKnight:
      case BlackPromotionKnight:
      case NormalMove:
      case WhiteKingSideCastle:
      case WhiteQueenSideCastle:
      case BlackKingSideCastle:
      case BlackQueenSideCastle:
      case WhiteKingSideCastleWild:
      case WhiteQueenSideCastleWild:
      case BlackKingSideCastleWild:
      case BlackQueenSideCastleWild:
	if (appData.debugMode)
	  fprintf(debugFP, "Parsed %s into %s\n",
		  yy_text, currentMoveString);
	fromX = currentMoveString[0] - 'a';
	fromY = currentMoveString[1] - '1';
	toX = currentMoveString[2] - 'a';
	toY = currentMoveString[3] - '1';
	break;

      case WhiteDrop:
      case BlackDrop:
	if (appData.debugMode)
	  fprintf(debugFP, "Parsed %s into %s\n",
		  yy_text, currentMoveString);
	fromX = moveType == WhiteDrop ?
	  (int) CharToPiece(ToUpper(currentMoveString[0])) :
	    (int) CharToPiece(ToLower(currentMoveString[0]));
	fromY = DROP_RANK;
	toX = currentMoveString[2] - 'a';
	toY = currentMoveString[3] - '1';
	break;

      case WhiteWins:
      case BlackWins:
      case GameIsDrawn:
      case GameUnfinished:
	if (appData.debugMode)
	  fprintf(debugFP, "Parsed game end: %s\n", yy_text);
	p = strchr(yy_text, '{');
	if (p == NULL) p = strchr(yy_text, '(');
	if (p == NULL) {
	    p = yy_text;
	    if (p[0] == '0' || p[0] == '1' || p[0] == '*') p = "";
	} else {
	    q = strchr(p, *p == '{' ? '}' : ')');
	    if (q != NULL) *q = NULLCHAR;
	    p++;
	}
	GameEnds(moveType, p, GE_FILE);
	done = TRUE;
	editAfterDone = (moveType == GameUnfinished);
	if (cmailMsgLoaded) {
	    flipView = WhiteOnMove(currentMove);
	    if (moveType == GameUnfinished) flipView = !flipView;
	    if (appData.debugMode)
	      fprintf(debugFP, "Setting flipView to %d\n", flipView) ;
	}
	break;

      case (ChessMove) 0:  /* end of file */
	if (appData.debugMode)
	  fprintf(debugFP, "Parser hit end of file\n");
	switch (MateTest(boards[currentMove], FakeFlags(currentMove),
			 EP_UNKNOWN)) {
	  case MT_NONE:
	  case MT_CHECK:
	    DisplayMessage("", "End of game file");
	    break;
	  case MT_CHECKMATE:
	    if (WhiteOnMove(currentMove)) {
		GameEnds(BlackWins, "Black mates", GE_FILE);
	    } else {
		GameEnds(WhiteWins, "White mates", GE_FILE);
	    }
	    editAfterDone = FALSE;
	    break;
	  case MT_STALEMATE:
	    GameEnds(GameIsDrawn, "Stalemate", GE_FILE);
	    editAfterDone = FALSE;
	    break;
	}
	done = TRUE;
	break;

      case MoveNumberOne:
	if (lastLoadGameStart == GNUChessGame) {
	    /* GNUChessGames have numbers, but they aren't move numbers */
	    if (appData.debugMode)
	      fprintf(debugFP, "Parser ignoring: '%s' (%d)\n",
		      yy_text, (int) moveType);
	    return LoadGameOneMove(); /* tail recursion */
	}
	/* else fall thru */

      case XBoardGame:
      case GNUChessGame:
      case PGNTag:
	/* Reached start of next game in file */
	if (appData.debugMode)
	  fprintf(debugFP, "Parsed start of next game: %s\n", yy_text);
	switch (MateTest(boards[currentMove], FakeFlags(currentMove),
			 EP_UNKNOWN)) {
	  case MT_NONE:
	  case MT_CHECK:
	    DisplayMessage("", "End of game");
	    break;
	  case MT_CHECKMATE:
	    if (WhiteOnMove(currentMove)) {
		GameEnds(BlackWins, "Black mates", GE_FILE);
	    } else {
		GameEnds(WhiteWins, "White mates", GE_FILE);
	    }
	    editAfterDone = FALSE;
	    break;
	  case MT_STALEMATE:
	    GameEnds(GameIsDrawn, "Stalemate", GE_FILE);
	    editAfterDone = FALSE;
	    break;
	}
	done = TRUE;
	break;

      case PositionDiagram: /* should not happen; ignore */
      case ElapsedTime:	/* ignore */
	if (appData.debugMode)
	  fprintf(debugFP, "Parser ignoring: '%s' (%d)\n",
		  yy_text, (int) moveType);
	return LoadGameOneMove(); /* tail recursion */

      default:
      case BadMove:
	if (appData.debugMode)
	  fprintf(debugFP, "Parsed BadMove: %s\n", yy_text);
	sprintf(move, "Bad move: %d.%s%s",
		(forwardMostMove / 2) + 1,
		WhiteOnMove(forwardMostMove) ? " " : ".. ", yy_text);
	DisplayError(move, 0);
	done = TRUE;
	break;

      case AmbiguousMove:
	if (appData.debugMode)
	  fprintf(debugFP, "Parsed AmbiguousMove: %s\n", yy_text);
	sprintf(move, "Ambiguous move: %d.%s%s",
		(forwardMostMove / 2) + 1,
		WhiteOnMove(forwardMostMove) ? " " : ".. ", yy_text);
	DisplayError(move, 0);
	done = TRUE;
	break;
    }
    
    if (done) {
	if (appData.matchMode || (appData.timeDelay == 0 && !pausing)) {
	    DrawPosition(FALSE, boards[currentMove]);
	    DisplayBothClocks();
	    if (!appData.matchMode && commentList[currentMove] != NULL)
	      DisplayComment(currentMove - 1, commentList[currentMove]);
	}
	if (editAfterDone) {
	    lastGameMode = gameMode;
	    gameMode = EditGame;
	    ModeHighlight();
	}
        (void) StopLoadGameTimer();
	gameFileFP = NULL;
	cmailOldMove = forwardMostMove;
	return FALSE;
    } else {
	strcat(currentMoveString, "\n");
	strcpy(moveList[forwardMostMove], currentMoveString);
	SendToProgram(currentMoveString, firstProgramPR);
	
	thinkOutput[0] = NULLCHAR;
	MakeMove(&moveType, fromX, fromY, toX, toY);
	
	return TRUE;
    }
}

/* Load the nth game from the given file */
int LoadGameFromFile(filename, n, title, useList)
     char *filename;
     int n;
     char *title;
     /*Boolean*/ int useList;
{
    FILE *f;
    char buf[MSG_SIZ];

    if (strcmp(filename, "-") == 0) {
	f = stdin;
	title = "stdin";
    } else {
	f = fopen(filename, "r");
	if (f == NULL) {
	    sprintf(buf, "Can't open \"%s\"", filename);
	    DisplayError(buf, errno);
	    return FALSE;
	}
    }
    if (fseek(f, 0, 0) == -1) {
	/* f is not seekable; probably a pipe */
	if (n == 0) n = 1;
	useList = FALSE;
    }
    if (useList && n == 0) {
	int error = GameListBuild(f);
	if (error) {
	    DisplayError("Cannot build game list", error);
	} else if (!ListEmpty(&gameList) &&
		   ((ListGame *) gameList.tailPred)->number > 1) {
	    GameListPopUp(f, title);
	    return TRUE;
	}
	GameListDestroy();
	n = 1;
    }
    return LoadGame(f, n, title, FALSE);
}


void MakeRegisteredMove()
{
    ChessMove moveType;
    
    if (cmailMoveRegistered[lastLoadGameNumber - 1]) {
    	switch (cmailMoveType[lastLoadGameNumber - 1]) {
    	  case CMAIL_MOVE:
    	  case CMAIL_DRAW:
	    if (appData.debugMode)
	      fprintf(debugFP, "Restoring %s for game %d\n",
		      cmailMove[lastLoadGameNumber - 1], lastLoadGameNumber);
    
	    moveType =
	      PromoCharToMoveType(WhiteOnMove(currentMove),
				  cmailMove[lastLoadGameNumber - 1][4]);
	    thinkOutput[0] = NULLCHAR;
	    strcpy(moveList[currentMove], cmailMove[lastLoadGameNumber - 1]);
	    MakeMove(&moveType,
		     cmailMove[lastLoadGameNumber - 1][0] - 'a',
		     cmailMove[lastLoadGameNumber - 1][1] - '1',
		     cmailMove[lastLoadGameNumber - 1][2] - 'a',
		     cmailMove[lastLoadGameNumber - 1][3] - '1');
	      
	    switch (MateTest(boards[currentMove], FakeFlags(currentMove),
			     EP_UNKNOWN)) {
	      case MT_NONE:
	      case MT_CHECK:
		break;
    		
	      case MT_CHECKMATE:
		if (WhiteOnMove(currentMove)) {
		    GameEnds(BlackWins, "Black mates", GE_PLAYER);
		} else {
		    GameEnds(WhiteWins, "White mates", GE_PLAYER);
		}
		break;
    		
	      case MT_STALEMATE:
		GameEnds(GameIsDrawn, "Stalemate", GE_PLAYER);
		break;
	    }

	    break;
	    
	  case CMAIL_RESIGN:
	    if (WhiteOnMove(currentMove)) {
		GameEnds(BlackWins, "White resigns", GE_PLAYER);
	    } else {
		GameEnds(WhiteWins, "Black resigns", GE_PLAYER);
	    }
	    break;
	    
	  case CMAIL_ACCEPT:
	    GameEnds(GameIsDrawn, "Draw agreed", GE_PLAYER);
	    break;
	      
	  default:
	    break;
	}
    }

    return;
}

/* Wrapper around LoadGame for use when a Cmail message is loaded */
int CmailLoadGame(f, gameNumber, title, useList)
     FILE *f;
     int gameNumber;
     char *title;
     int useList;
{
    int retVal;

    if (gameNumber > nCmailGames) {
	DisplayError("No more games in this message", 0);
	return FALSE;
    }
    if (f == lastLoadGameFP) {
	int offset = gameNumber - lastLoadGameNumber;
	if (offset == 0) {
	    cmailMsg[0] = NULLCHAR;
	    if (cmailMoveRegistered[lastLoadGameNumber - 1]) {
		cmailMoveRegistered[lastLoadGameNumber - 1] = FALSE;
		nCmailMovesRegistered--;
	    }
	    cmailMoveType[lastLoadGameNumber - 1] = CMAIL_MOVE;
	    if (cmailResult[lastLoadGameNumber - 1] == CMAIL_NEW_RESULT) {
		cmailResult[lastLoadGameNumber - 1] = CMAIL_NOT_RESULT;
	    }
	} else {
	    if (! RegisterMove()) return FALSE;
	}
    }

    retVal = LoadGame(f, gameNumber, title, useList);

    /* Make move registered during previous look at this game, if any */
    MakeRegisteredMove();

    if (cmailCommentList[lastLoadGameNumber - 1] != NULL) {
	commentList[currentMove]
	  = StrSave(cmailCommentList[lastLoadGameNumber - 1]);
	DisplayComment(currentMove - 1, commentList[currentMove]);
    }

    return retVal;
}

/* Support for LoadNextGame, LoadPreviousGame, ReloadSameGame */
int ReloadGame(offset)
     int offset;
{
    int gameNumber = lastLoadGameNumber + offset;
    if (lastLoadGameFP == NULL) {
	DisplayError("No game has been loaded yet", 0);
	return FALSE;
    }
    if (gameNumber <= 0) {
	DisplayError("Can't back up any further", 0);
	return FALSE;
    }
    if (cmailMsgLoaded) {
	return CmailLoadGame(lastLoadGameFP, gameNumber,
			     lastLoadGameTitle, lastLoadGameUseList);
    } else {
	return LoadGame(lastLoadGameFP, gameNumber,
			lastLoadGameTitle, lastLoadGameUseList);
    }
}


/* Load the nth game from open file f */
int LoadGame(f, gameNumber, title, useList)
     FILE *f;
     int gameNumber;
     char *title;
     int useList;
{
    ChessMove cm;
    char buf[MSG_SIZ];
    int gn = gameNumber;

	if (gameMode != BeginningOfGame && gameMode != AnalyzeFile ) {
	Reset(FALSE);
    }

    gameFileFP = f;
    if (lastLoadGameFP != NULL && lastLoadGameFP != f) {
	fclose(lastLoadGameFP);
    }

    if (useList) {
	ListGame *lg = (ListGame *) ListElem(&gameList, gameNumber-1);
	
	if (lg) {
	    fseek(f, lg->offset, 0);
	    GameListHighlight(gameNumber);
	    gn = 1;
	}
	else {
	    DisplayError("Game number out of range", 0);
	    return FALSE;
	}
    } else {
	GameListDestroy();
	if (fseek(f, 0, 0) == -1) {
	    if (f == lastLoadGameFP ?
		gameNumber == lastLoadGameNumber + 1 :
		gameNumber == 1) {
		gn = 1;
	    } else {
		DisplayError("Can't seek on game file", 0);
		return FALSE;
	    }
	}
    }
    lastLoadGameFP = f;
    lastLoadGameNumber = gameNumber;

    lastLoadGameUseList = useList;

    yynewfile(f);

    if (*title != NULLCHAR) {
	if (gameNumber > 1) {
	    sprintf(buf, "%s %d", title, gameNumber);
	    DisplayTitle(buf);
	} else {
	    DisplayTitle(title);
	}
    }
	if ( gameMode != AnalyzeFile )
	  {
    lastGameMode = gameMode = PlayFromGameFile;
    ModeHighlight();
	  }

    InitPosition(FALSE);
    StopClocks();

	if ( gameMode != AnalyzeFile ) {
    if (firstProgramPR == NoProc) {
	InitChessProgram(appData.firstHost, appData.firstChessProgram,
			 &firstProgramPR, &firstProgramISR, &firstSendTime);
    } else {
	SendToProgram(appData.initString, firstProgramPR);
	SendTimeControl(firstProgramPR, appData.movesPerSession,
			timeControl, appData.timeIncrement,
			appData.searchDepth, searchTime);
    }
    SendToProgram("force\n", firstProgramPR);
    }
	else
	  SendToProgram("new\n", firstProgramPR );
    
    /*
     * Skip the first gn-1 games in the file.
     * Also skip over anything that precedes an identifiable 
     * start of game marker, to avoid being confused by 
     * garbage at the start of the file.  Currently 
     * recognized start of game markers are the move number "1",
     * the pattern "gnuchess .* game", the pattern
     * "^[#;%] [^ ]* game file", and a PGN tag block.  
     * A game that starts with one of the latter two patterns
     * will also have a move number 1, possibly
     * following a position diagram.
     */
    cm = lastLoadGameStart = (ChessMove) 0;
    yyskipmoves = TRUE;
    while (gn > 0) {
	yyboardindex = forwardMostMove;
	cm = (ChessMove) yylex();
	yyskipmoves = FALSE;
	switch (cm) {
	  case (ChessMove) 0:
	    if (cmailMsgLoaded) {
		nCmailGames = CMAIL_MAX_GAMES - gn;
	    } else {
		Reset(TRUE);
		DisplayError("Game not found in file", 0);
	    }
	    yyskipmoves = FALSE;
	    return FALSE;

	  case GNUChessGame:
	  case XBoardGame:
	    gn--;
	    lastLoadGameStart = cm;
	    break;
	    
	  case MoveNumberOne:
	    switch (lastLoadGameStart) {
	      case GNUChessGame:
	      case XBoardGame:
	      case PGNTag:
		break;
	      case MoveNumberOne:
	      case (ChessMove) 0:
		gn--;   /* count this game */
		lastLoadGameStart = cm;
		break;
	      default:
		/* impossible */
		break;
	    }
	    break;

	  case PGNTag:
	    switch (lastLoadGameStart) {
	      case GNUChessGame:
	      case PGNTag:
	      case MoveNumberOne:
	      case (ChessMove) 0:
		gn--;   /* count this game */
		lastLoadGameStart = cm;
		break;
	      case XBoardGame:
		lastLoadGameStart = cm;  /* game counted already */
		break;
	      default:
		/* impossible */
		break;
	    }
	    if (gn > 0) {
		do {
		    yyboardindex = forwardMostMove;
		    cm = (ChessMove) yylex();
		} while (cm == PGNTag || cm == Comment);
	    }
	    break;

	  case WhiteWins:
	  case BlackWins:
	  case GameIsDrawn:
 	    if (cmailMsgLoaded && (CMAIL_MAX_GAMES == lastLoadGameNumber)) {
		if (   cmailResult[CMAIL_MAX_GAMES - gn - 1]
		    != CMAIL_OLD_RESULT) {
		    nCmailResults ++ ;
		    cmailResult[  CMAIL_MAX_GAMES
				- gn - 1] = CMAIL_OLD_RESULT;
		}
	    }
	    break;

	  default:
	    break;
	}
    }
    yyskipmoves = FALSE;
    
    if (appData.debugMode)
      fprintf(debugFP, "Parsed game start '%s' (%d)\n", yy_text, (int) cm);

    if (cm == XBoardGame) {
	/* Skip any header junk before position diagram and/or move 1 */
	for (;;) {
	    yyboardindex = forwardMostMove;
	    cm = (ChessMove) yylex();

	    if (cm == (ChessMove) 0 ||
		cm == GNUChessGame || cm == XBoardGame) {
		/* Empty game; pretend end-of-file and handle later */
		cm = (ChessMove) 0;
		break;
	    }

	    if (cm == MoveNumberOne || cm == PositionDiagram ||
		cm == PGNTag || cm == Comment)
	      break;
	}
    } else if (cm == GNUChessGame) {
	if (gameInfo.event != NULL) {
	    free(gameInfo.event);
	}
	gameInfo.event = StrSave(yy_text);
    }	

    while (cm == PGNTag) {
	if (appData.debugMode) 
	  fprintf(debugFP, "Parsed PGNTag: %s\n", yy_text);
	ParsePGNTag(yy_text, &gameInfo);
	yyboardindex = forwardMostMove;
	cm = (ChessMove) yylex();
	/* Handle comments interspersed among the tags */
	while (cm == Comment) {
	    char *p;
	    if (appData.debugMode) 
	      fprintf(debugFP, "Parsed Comment: %s\n", yy_text);
	    p = yy_text;
	    if (*p == '{' || *p == '[' || *p == '(') {
		p[strlen(p) - 1] = NULLCHAR;
		p++;
	    }
	    while (*p == '\n') p++;
	    AppendComment(currentMove, p);
	    yyboardindex = forwardMostMove;
	    cm = (ChessMove) yylex();
	}
    }

    if (gameInfo.event != NULL) {
	char *tags = PGNTags(&gameInfo);
	TagsPopUp(tags, CmailMsg());
	free(tags);
    } else {
	/* Make something up, but don't display it now */
	SetGameInfo();
	TagsPopDown();
    }

    if (cm == PositionDiagram) {
	int i, j;
	char *p;
	Board initial_position;

	if (appData.debugMode)
	      fprintf(debugFP, "Parsed PositionDiagram: %s\n", yy_text);

	if (gameInfo.fen == NULL) {
	    p = yy_text;
	    for (i = BOARD_SIZE - 1; i >= 0; i--)
	      for (j = 0; j < BOARD_SIZE; p++)
		switch (*p) {
		  case '[':
		  case '-':
		  case ' ':
		  case '\t':
		  case '\n':
		  case '\r':
		    break;
		  default:
		    initial_position[i][j++] = CharToPiece(*p);
		    break;
		}
	    while (*p == ' ' || *p == '\t' ||
		   *p == '\n' || *p == '\r') p++;
	
	    if (strncmp(p, "black", strlen("black"))==0)
	      blackPlaysFirst = TRUE;
	    else
	      blackPlaysFirst = FALSE;
	    startedFromSetupPosition = TRUE;
	
	    if (blackPlaysFirst) {
		currentMove = forwardMostMove = backwardMostMove = 1;
		CopyBoard(boards[0], initial_position);
		CopyBoard(boards[1], initial_position);
		strcpy(moveList[0], "");
		strcpy(parseList[0], "");
		timeRemaining[0][1] = whiteTimeRemaining;
		timeRemaining[1][1] = blackTimeRemaining;
		SendToProgram("a2a3\n", firstProgramPR);
		SendCurrentBoard(firstProgramPR);
		if (commentList[0] != NULL) {
		    commentList[1] = commentList[0];
		    commentList[0] = NULL;
		}
	    } else {
		currentMove = forwardMostMove = backwardMostMove = 0;
		CopyBoard(boards[0], initial_position);
		SendCurrentBoard(firstProgramPR);
	    }
	}
	yyboardindex = forwardMostMove;
	cm = (ChessMove) yylex();
    }

    if (gameInfo.fen != NULL) {
	Board initial_position;
	startedFromSetupPosition = TRUE;
	if (!ParseFEN(initial_position, &blackPlaysFirst, gameInfo.fen)) {
	    Reset(TRUE);
	    DisplayError("Bad FEN position in file", 0);
	    return FALSE;
	}
	if (blackPlaysFirst) {
	    currentMove = forwardMostMove = backwardMostMove = 1;
	    CopyBoard(boards[0], initial_position);
	    CopyBoard(boards[1], initial_position);
	    strcpy(moveList[0], "");
	    strcpy(parseList[0], "");
	    timeRemaining[0][1] = whiteTimeRemaining;
	    timeRemaining[1][1] = blackTimeRemaining;
	    SendToProgram("a2a3\n", firstProgramPR);
	    SendCurrentBoard(firstProgramPR);
	    if (commentList[0] != NULL) {
		commentList[1] = commentList[0];
		commentList[0] = NULL;
	    }
	} else {
	    currentMove = forwardMostMove = backwardMostMove = 0;
	    CopyBoard(boards[0], initial_position);
	    SendCurrentBoard(firstProgramPR);
	}
	free(gameInfo.fen);
	gameInfo.fen = NULL;
    }

    while (cm == Comment) {
	char *p;
	if (appData.debugMode) 
	  fprintf(debugFP, "Parsed Comment: %s\n", yy_text);
	p = yy_text;
	if (*p == '{' || *p == '[' || *p == '(') {
	    p[strlen(p) - 1] = NULLCHAR;
	    p++;
	}
	while (*p == '\n') p++;
	AppendComment(currentMove, p);
	yyboardindex = forwardMostMove;
	cm = (ChessMove) yylex();
    }

    if (cm == (ChessMove) 0 || cm == WhiteWins || cm == BlackWins ||
	cm == GameIsDrawn || cm == GameUnfinished) {
	DisplayMessage("", "No moves in game");
	if (cmailMsgLoaded) {
	    if (appData.debugMode)
	      fprintf(debugFP, "Setting flipView to %d.\n", FALSE);
	    flipView = FALSE;
	}
	DrawPosition(FALSE, boards[currentMove]);
	DisplayBothClocks();
	lastGameMode = gameMode;
	gameMode = EditGame;
	ModeHighlight();
	gameFileFP = NULL;
	cmailOldMove = 0;
	return TRUE;
    }

    if (cm != MoveNumberOne && cm != GNUChessGame) {
	char buf[MSG_SIZ];
	Reset(TRUE);
	sprintf(buf, "Unexpected token in game: '%s' (%d)",
		yy_text, (int) cm);
	DisplayError(buf, 0);
	return FALSE;
    }

    if (commentList[currentMove] != NULL) {
	if (!matchMode && (pausing || appData.timeDelay != 0)) {
	    DisplayComment(currentMove - 1, commentList[currentMove]);
	}
	if (!matchMode && appData.timeDelay != 0) {
	    DrawPosition(FALSE, boards[currentMove]);
	    if (appData.timeDelay > 0)
	      StartLoadGameTimer((long)(1000.0 * appData.timeDelay));
	    return TRUE;
	}
    }
    if (!matchMode && appData.timeDelay != 0)
      DrawPosition(FALSE, boards[currentMove]);

    LoadGameLoop();
    
    if ( gameMode == AnalyzeFile )
	  programStats.ok_to_send = 1;
	
    return TRUE;
}

/* Load the nth position from the given file */
int LoadPositionFromFile(filename, n, title)
     char *filename;
     int n;
     char *title;
{
    FILE *f;
    char buf[MSG_SIZ];

    if (strcmp(filename, "-") == 0) {
	return LoadPosition(stdin, n, "stdin");
    } else {
	f = fopen(filename, "r");
	if (f == NULL) {
	    sprintf(buf, "Can't open \"%s\"", filename);
	    DisplayError(buf, errno);
	    return FALSE;
	} else {
	    return LoadPosition(f, n, title);
	}
    }
}

/* Load the nth position from the given open file, and close it */
int LoadPosition(fp, positionNumber, title)
     FILE *fp;
     int positionNumber;
     char *title;
{
    char *p, line[MSG_SIZ];
    Board initial_position;
    int i, j, fenMode;
    
    if (gameMode != BeginningOfGame) {
	Reset(TRUE);
    }

    if (positionNumber > 1) {
	sprintf(line, "%s %d", title, positionNumber);
	DisplayTitle(line);
    } else {
	DisplayTitle(title);
    }
    lastGameMode = gameMode = EditGame;
    ModeHighlight();
    startedFromSetupPosition = TRUE;
    
    if (firstProgramPR == NoProc)
      InitChessProgram(appData.firstHost, appData.firstChessProgram,
		       &firstProgramPR, &firstProgramISR, &firstSendTime);
    
    /* Negative position number means to seek to that byte offset */
    if (positionNumber < 0) {
	fseek(fp, -positionNumber, 0);
	positionNumber = 1;
    }
    /* See if this file is FEN or old-style xboard */
    if (fgets(line, MSG_SIZ, fp) == NULL) {
	Reset(TRUE);
	DisplayError("Position not found in file", 0);
	return FALSE;
    }
    switch (line[0]) {
      case '#':  case 'x':
      default:
	fenMode = FALSE;
	break;
      case 'p':  case 'n':  case 'b':  case 'r':  case 'q':  case 'k':
      case 'P':  case 'N':  case 'B':  case 'R':  case 'Q':  case 'K':
      case '1':  case '2':  case '3':  case '4':  case '5':  case '6':
      case '7':  case '8':
	fenMode = TRUE;
	break;
    }

    if (positionNumber >= 2  /* don't look for '#' if positionNumber == 1 */) {
	if (fenMode || line[0] == '#') positionNumber--;
	while (positionNumber > 0) {
	    /* skip postions before number positionNumber */
	    if (fgets(line, MSG_SIZ, fp) == NULL) {
		Reset(TRUE);
		DisplayError("Position not found in file", 0);
		return FALSE;
	    }
	    if (fenMode || line[0] == '#') positionNumber--;
	}
    }

    if (fenMode) {
	if (!ParseFEN(initial_position, &blackPlaysFirst, line)) {
	    Reset(TRUE);
	    DisplayError("Bad FEN position in file", 0);
	    return FALSE;
	}
    } else {
	(void) fgets(line, MSG_SIZ, fp);
	(void) fgets(line, MSG_SIZ, fp);
    
	for (i = BOARD_SIZE - 1; i >= 0; i--) {
	    (void) fgets(line, MSG_SIZ, fp);
	    for (p = line, j = 0; j < BOARD_SIZE; p++) {
		if (*p == ' ')
		  continue;
		initial_position[i][j++] = CharToPiece(*p);
	    }
	}
    
	blackPlaysFirst = FALSE;
	if (!feof(fp)) {
	    (void) fgets(line, MSG_SIZ, fp);
	    if (strncmp(line, "black", strlen("black"))==0)
	      blackPlaysFirst = TRUE;
	}
    }
    fclose(fp);
    
    if (blackPlaysFirst) {
	CopyBoard(boards[0], initial_position);
	strcpy(moveList[0], "");
	strcpy(parseList[0], "");
	currentMove = forwardMostMove = backwardMostMove = 1;
	CopyBoard(boards[1], initial_position);
	SendToProgram("force\na2a3\n", firstProgramPR);
	SendCurrentBoard(firstProgramPR);
	DisplayMessage("", "Black to play");
    } else {
	currentMove = forwardMostMove = backwardMostMove = 0;
	CopyBoard(boards[0], initial_position);
	SendCurrentBoard(firstProgramPR);
	SendToProgram("force\n", firstProgramPR);
	DisplayMessage("", "White to play");
    }
    
    ResetClocks();
    timeRemaining[0][1] = whiteTimeRemaining;
    timeRemaining[1][1] = blackTimeRemaining;

    DrawPosition(FALSE, boards[currentMove]);
    
    return TRUE;
}


void CopyPlayerNameIntoFileName(dest, src)
     char **dest, *src;
{
    while (*src != NULLCHAR && *src != ',') {
	if (*src == ' ') {
	    *(*dest)++ = '_';
	    src++;
	} else {
	    *(*dest)++ = *src++;
	}
    }
}

char *DefaultFileName(ext)
     char *ext;
{
    static char def[MSG_SIZ];
    char *p;

    if (gameInfo.white != NULL && gameInfo.white[0] != '-') {
	p = def;
	CopyPlayerNameIntoFileName(&p, gameInfo.white);
	*p++ = '-';
	CopyPlayerNameIntoFileName(&p, gameInfo.black);
	*p++ = '.';
	strcpy(p, ext);
    } else {
	def[0] = NULLCHAR;
    }
    return def;
}

/* Save the current game to the given file */
int SaveGameToFile(filename)
     char *filename;
{
    FILE *f;
    char buf[MSG_SIZ];

    if (strcmp(filename, "-") == 0) {
	return SaveGame(toUserFP, 0, NULL);
    } else {
	f = fopen(filename, "a");
	if (f == NULL) {
	    sprintf(buf, "Can't open \"%s\"", filename);
	    DisplayError(buf, errno);
	    return FALSE;
	} else {
	    return SaveGame(f, 0, NULL);
	}
    }
}

char *SavePart(str)
     char *str;
{
    static char buf[MSG_SIZ];
    char *p;
    
    p = strchr(str, ' ');
    if (p == NULL) return str;
    strncpy(buf, str, p - str);
    buf[p - str] = NULLCHAR;
    return buf;
}

#define PGN_MAX_LINE 75

int SaveGamePGN(f)
     FILE *f;
{
    int i, offset, linelen, newblock;
    time_t tm;
    char *movetext;
    char numtext[32];
    int movelen, numlen, blank;
    
    tm = time((time_t *) NULL);
    
    PrintPGNTags(f, &gameInfo);
    
    if (backwardMostMove > 0 || startedFromSetupPosition) {
	fprintf(f, "[FEN \"%s\"]\n[SetUp \"1\"]\n",
		PositionToFEN(backwardMostMove));
	fprintf(f, "\n{--------------\n");
	PrintPosition(f, backwardMostMove);
	fprintf(f, "--------------}\n");
    } else {
	fprintf(f, "\n");
    }

    i = backwardMostMove;
    offset = backwardMostMove & (~1L);  /* output move numbers start at 1 */
    linelen = 0;
    newblock = TRUE;

    while (i < forwardMostMove) {
	/* Print comments preceding this move */
	if (commentList[i] != NULL) {
	    if (linelen > 0) fprintf(f, "\n");
	    fprintf(f, "{\n%s}\n", commentList[i]);
	    linelen = 0;
	    newblock = TRUE;
	}

	/* Format move number */
        if ((i % 2) == 0) {
	    sprintf(numtext, "%d.", (i - offset)/2 + 1);
	} else {
	    if (newblock) {
		sprintf(numtext, "%d...", (i - offset)/2 + 1);
	    } else {
		numtext[0] = NULLCHAR;
	    }
	}
	numlen = strlen(numtext);
	newblock = FALSE;

	/* Print move number */
	blank = linelen > 0 && numlen > 0;
	if (linelen + (blank ? 1 : 0) + numlen > PGN_MAX_LINE) {
	    fprintf(f, "\n");
	    linelen = 0;
	    blank = 0;
	}
	if (blank) {
	    fprintf(f, " ");
	    linelen++;
	}
	fprintf(f, numtext);
	linelen += numlen;

	/* Get move */
        movetext = SavePart(parseList[i]);
	movelen = strlen(movetext);

	/* Print move */
	blank = linelen > 0 && movelen > 0;
	if (linelen + (blank ? 1 : 0) + movelen > PGN_MAX_LINE) {
	    fprintf(f, "\n");
	    linelen = 0;
	    blank = 0;
	}
	if (blank) {
	    fprintf(f, " ");
	    linelen++;
	}
	fprintf(f, movetext);
	linelen += movelen;

	i++;
    }
    
    /* Start a new line */
    if (linelen > 0) fprintf(f, "\n");

    /* Print comments after last move */
    if (commentList[i] != NULL) {
	fprintf(f, "{\n%s}\n", commentList[i]);
    }

    /* Print result */
    if (gameInfo.resultDetails != NULL &&
	gameInfo.resultDetails[0] != NULLCHAR) {
	fprintf(f, "{%s} %s\n\n", gameInfo.resultDetails,
		PGNResult(gameInfo.result));
    } else {
	fprintf(f, "%s\n\n", PGNResult(gameInfo.result));
    }

    fclose(f);
    return TRUE;
}

int SaveGameOldStyle(f)
     FILE *f;
{
    int i, offset;
    time_t tm;
    extern char *programName;
    
    tm = time((time_t *) NULL);
    
    fprintf(f, "# %s game file -- %s", programName, ctime(&tm));
    PrintOpponents(f);
    
    if (backwardMostMove > 0 || startedFromSetupPosition) {
	fprintf(f, "\n[--------------\n");
	PrintPosition(f, backwardMostMove);
	fprintf(f, "--------------]\n");
    } else {
	fprintf(f, "\n");
    }

    i = backwardMostMove;
    offset = backwardMostMove & (~1L);  /* output move numbers start at 1 */

    while (i < forwardMostMove) {
	if (commentList[i] != NULL) {
	    fprintf(f, "[%s]\n", commentList[i]);
	}

	if ((i % 2) == 1) {
	    fprintf(f, "%d. ...  %s\n", (i - offset)/2 + 1, parseList[i]);
	    i++;
	} else {
	    fprintf(f, "%d. %s  ", (i - offset)/2 + 1, parseList[i]);
	    i++;
	    if (commentList[i] != NULL) {
		fprintf(f, "\n");
		continue;
	    }
	    if (i >= forwardMostMove) {
		fprintf(f, "\n");
		break;
	    }
	    fprintf(f, "%s\n", parseList[i]);
	    i++;
	}
    }
    
    if (commentList[i] != NULL) {
	fprintf(f, "[%s]\n", commentList[i]);
    }

    /* This isn't really the old style, but it's close enough */
    if (gameInfo.resultDetails != NULL &&
	gameInfo.resultDetails[0] != NULLCHAR) {
	fprintf(f, "%s (%s)\n\n", PGNResult(gameInfo.result),
		gameInfo.resultDetails);
    } else {
	fprintf(f, "%s\n\n", PGNResult(gameInfo.result));
    }

    fclose(f);
    return TRUE;
}

/* Save the current game to open file f and close the file */
int SaveGame(f, dummy, dummy2)
     FILE *f;
     int dummy;
     char *dummy2;
{
    if (appData.oldSaveStyle)
      return SaveGameOldStyle(f);
    else
      return SaveGamePGN(f);
}

/* Save the current position to the given file */
int SavePositionToFile(filename)
     char *filename;
{
    FILE *f;
    char buf[MSG_SIZ];

    if (strcmp(filename, "-") == 0) {
	return SavePosition(toUserFP, 0, NULL);
    } else {
	f = fopen(filename, "a");
	if (f == NULL) {
	    sprintf(buf, "Can't open \"%s\"", filename);
	    DisplayError(buf, errno);
	    return FALSE;
	} else {
	    SavePosition(f, 0, NULL);
	    return TRUE;
	}
    }
}

/* Save the current position to the given open file and close the file */
int SavePosition(f, dummy, dummy2)
     FILE *f;
     int dummy;
     char *dummy2;
{
    time_t tm;
    extern char *programName;
    char *fen;
    
    if (appData.oldSaveStyle) {
	tm = time((time_t *) NULL);
    
	fprintf(f, "# %s position file -- %s", programName, ctime(&tm));
	PrintOpponents(f);
	fprintf(f, "[--------------\n");
	PrintPosition(f, currentMove);
	fprintf(f, "--------------]\n");
    } else {
	fen = PositionToFEN(currentMove);
	fprintf(f, "%s\n", fen);
	free(fen);
    }
    fclose(f);
    return TRUE;
}

void ReloadCmailMsgEvent(unregister)
     int unregister;
{
    static char *inFilename = NULL;
    static char *outFilename;
    int i;
    struct stat inbuf, outbuf;
    int status;
    
    /* Any registered moves are unregistered if unregister is set, */
    /* i.e. invoked by the signal handler */
    if (unregister) {
	for (i = 0; i < CMAIL_MAX_GAMES; i ++) {
	    cmailMoveRegistered[i] = FALSE;
	    if (cmailCommentList[i] != NULL) {
		free(cmailCommentList[i]);
		cmailCommentList[i] = NULL;
	    }
	}
	nCmailMovesRegistered = 0;
    }

    Reset(TRUE);
    
    for (i = 0; i < CMAIL_MAX_GAMES; i ++) {
	cmailResult[i] = CMAIL_NOT_RESULT;
    }
    nCmailResults = 0;

    if (inFilename == NULL) {
	/* Because the filenames are static they only get malloced once  */
	/* and they never get freed                                      */
	inFilename = (char *) malloc(strlen(appData.cmailGameName) + 9);
	sprintf(inFilename, "%s.game.in", appData.cmailGameName);

	outFilename = (char *) malloc(strlen(appData.cmailGameName) + 5);
	sprintf(outFilename, "%s.out", appData.cmailGameName);
    }
    
    status = stat(outFilename, &outbuf);
    if (status < 0) {
	cmailMailedMove = FALSE;
    } else {
	status = stat(inFilename, &inbuf);
	cmailMailedMove = (inbuf.st_mtime < outbuf.st_mtime);
    }
    
    /* LoadGameFromFile(CMAIL_MAX_GAMES) with cmailMsgLoaded == TRUE
       counts the games, notes how each one terminated, etc.

       It would be nice to remove this kludge and instead gather all
       the information while building the game list.  (And to keep it
       in the game list nodes instead of having a bunch of fixed-size
       parallel arrays.)  Note this will require getting each game's
       termination from the PGN tags, as the game list builder does
       not process the game moves.  --mann
    */
    cmailMsgLoaded = TRUE;
    LoadGameFromFile(inFilename, CMAIL_MAX_GAMES, "", FALSE);
    
    /* Load first game in the file or popup game menu */
    LoadGameFromFile(inFilename, 0, appData.cmailGameName, TRUE);

    return;
}

int RegisterMove()
{
    FILE *f;
    char string[MSG_SIZ];

    if (   cmailMailedMove
	|| (cmailResult[lastLoadGameNumber - 1] == CMAIL_OLD_RESULT)) {
	return TRUE;				  /* Allow free viewing  */
    }

    /* Unregister move to ensure that we don't leave RegisterMove        */
    /* with the move registered when the conditions for registering no   */
    /* longer hold                                                       */
    if (cmailMoveRegistered[lastLoadGameNumber - 1]) {
	cmailMoveRegistered[lastLoadGameNumber - 1] = FALSE;
	nCmailMovesRegistered --;

	if (cmailCommentList[lastLoadGameNumber - 1] != NULL) 
	{
	    free(cmailCommentList[lastLoadGameNumber - 1]);
	    cmailCommentList[lastLoadGameNumber - 1] = NULL;
	}
    }

    if (cmailOldMove == -1) {
	DisplayError("You have edited the game history.\nUse Reload Same Game and make your move again.", 0);
	return FALSE;
    }

    if (currentMove > cmailOldMove + 1) {
	DisplayError("You have entered too many moves.\nBack up to the correct position and try again.", 0);
	return FALSE;
    }

    if (currentMove < cmailOldMove) {
        DisplayError("Displayed position is not current.\nStep forward to the correct position and try again.", 0);
        return FALSE;
    }

    if (forwardMostMove > currentMove) {
	/* Silently truncate extra moves */
	TruncateGame();
    }

    if (   (currentMove == cmailOldMove + 1)
	|| (   (currentMove == cmailOldMove)
	    && (   (cmailMoveType[lastLoadGameNumber - 1] == CMAIL_ACCEPT)
		|| (cmailMoveType[lastLoadGameNumber - 1] == CMAIL_RESIGN)))) {
	if (gameInfo.result != GameUnfinished) {
	    cmailResult[lastLoadGameNumber - 1] = CMAIL_NEW_RESULT;
	}

	if (commentList[currentMove] != NULL) {
	    cmailCommentList[lastLoadGameNumber - 1]
	      = StrSave(commentList[currentMove]);
	}
	strcpy(cmailMove[lastLoadGameNumber - 1], moveList[currentMove - 1]);

	if (appData.debugMode)
	  fprintf(debugFP, "Saving %s for game %d\n",
		  cmailMove[lastLoadGameNumber - 1], lastLoadGameNumber);

	sprintf(string,
		"%s.game.out.%d", appData.cmailGameName, lastLoadGameNumber);
	
	f = fopen(string, "w");
	if (appData.oldSaveStyle) {
	    SaveGameOldStyle(f);
	    fclose(f);
	    
	    sprintf(string, "%s.pos.out", appData.cmailGameName);
	    f = fopen(string, "w");
	    SavePosition(f, 0, NULL);
	    fclose(f);
	} else {
	    fprintf(f, "{--------------\n");
	    PrintPosition(f, currentMove);
	    fprintf(f, "--------------}\n\n");
	    
	    SaveGame(f, 0, NULL);
	    fclose(f);
	}
	
	cmailMoveRegistered[lastLoadGameNumber - 1] = TRUE;
	nCmailMovesRegistered ++;
    } else if (nCmailGames == 1) {
	DisplayError("You have not made a move yet.", 0);
	return FALSE;
    }

    return TRUE;
}
    
void MailMoveEvent()
{
    static char *partCommandString = "cmail -xv%s -remail -game %s 2>&1";
    FILE *commandOutput;
    char buffer[MSG_SIZ], msg[MSG_SIZ], string[MSG_SIZ];
    int nBytes = 0; /*  Suppress warnings on uninitialized variables    */
    int nBuffers;
    int i;
    int archived;
    char *arcDir;

    if (! cmailMsgLoaded) {
	DisplayError("The cmail message is not loaded.\nUse Reload CMail Message and make your move again.", 0);
	return;
    }

    if (nCmailGames == nCmailResults) {
        DisplayError("No unfinished games", 0);
	return;
    }

#ifdef CMAIL_PROHIBIT_REMAIL
    if (cmailMailedMove) {
	sprintf(msg, "You have already mailed a move.\nWait until a move arrives from your opponent.\nTo resend the same move, type\n\"cmail -remail -game %s\"\non the command line.", appData.cmailGameName);
        DisplayError(msg, 0);
	return;
    }
#endif

    if (! (cmailMailedMove || RegisterMove())) return;
    
    if (   cmailMailedMove
	|| (nCmailMovesRegistered + nCmailResults == nCmailGames)) {
	sprintf(string, partCommandString,
		appData.debugMode ? " -v" : "", appData.cmailGameName);
	commandOutput = popen(string, "r");

	if (commandOutput == NULL) {
	    DisplayError("Failed to invoke cmail", 0);
	} else {
	    for (nBuffers = 0; (! feof(commandOutput)); nBuffers ++) {
		nBytes = fread(buffer, 1, MSG_SIZ - 1, commandOutput);
	    }
	    if (nBuffers > 1) {
		(void) memcpy(msg, buffer + nBytes, MSG_SIZ - nBytes - 1);
		(void) memcpy(msg + MSG_SIZ - nBytes - 1, buffer, nBytes);
		nBytes = MSG_SIZ - 1;
	    } else {
		(void) memcpy(msg, buffer, nBytes);
	    }
	    *(msg + nBytes) = '\0';		  /* \0 for end-of-string*/

	    if(StrStr(msg, "Mailed cmail message to ") != NULL) {
		cmailMailedMove = TRUE;		  /* Prevent >1 moves    */

		archived = TRUE;
		for (i = 0; i < nCmailGames; i ++) {
		    if (cmailResult[i] == CMAIL_NOT_RESULT) {
			archived = FALSE;
		    }
		}
		if (   archived
		    && (   (arcDir = (char *) getenv("CMAIL_ARCDIR"))
		        != NULL)) {
		    sprintf(buffer, "%s/%s.%s.archive",
			    arcDir,
			    appData.cmailGameName,
			    gameInfo.date);
		    LoadGameFromFile(buffer, 1, buffer, FALSE);
		    cmailMsgLoaded = FALSE;
		}
	    }

	    DisplayInformation(msg);
	    pclose(commandOutput);
	}
    } else {
	if ((*cmailMsg) != '\0') {
	    DisplayInformation(cmailMsg);
	}
    }

    return;
}

char *CmailMsg()
{
    int  prependComma = 0;
    char number[5];
    char string[MSG_SIZ];  /* Space for game-list */
    int  i;
    
    if (!cmailMsgLoaded) return "";

    if (cmailMailedMove) {
	sprintf(cmailMsg, "Waiting for reply from opponent\n");
    } else {
	/* Create a list of games left */
	sprintf(string, "[");
	for (i = 0; i < nCmailGames; i ++) {
	    if (! (   cmailMoveRegistered[i]
		   || (cmailResult[i] == CMAIL_OLD_RESULT))) {
		if (prependComma) {
		    sprintf(number, ",%d", i + 1);
		} else {
		    sprintf(number, "%d", i + 1);
		    prependComma = 1;
		}
		
		strcat(string, number);
	    }
	}
	strcat(string, "]");

	if (nCmailMovesRegistered + nCmailResults == 0) {
	    switch (nCmailGames) {
	      case 1:
		sprintf(cmailMsg,
			"Still need to make move for game\n");
		break;
		
	      case 2:
		sprintf(cmailMsg,
			"Still need to make moves for both games\n");
		break;
		
	      default:
		sprintf(cmailMsg,
			"Still need to make moves for all %d games\n",
			nCmailGames);
		break;
	    }
	} else {
	    switch (nCmailGames - nCmailMovesRegistered - nCmailResults) {
	      case 1:
		sprintf(cmailMsg,
			"Still need to make a move for game %s\n",
			string);
		break;
		
	      case 0:
		if (nCmailResults == nCmailGames) {
		    sprintf(cmailMsg, "No unfinished games\n");
		} else {
		    sprintf(cmailMsg, "Ready to send mail\n");
		}
		break;
		
	      default:
		sprintf(cmailMsg,
			"Still need to make moves for games %s\n",
			string);
	    }
	}
    }

    return cmailMsg;
}

void ResetGameEvent()
{
    Reset(TRUE);
    cmailMsgLoaded = FALSE;
    if (appData.icsActive)
      SendToICS("refresh\n");
}

static int exitrecur = 0;

void ExitEvent(status)
     int status;
{
    if (exitrecur++ > 0) exit(status);  /* error during exit processing */
    if (appData.colorize) con_color(-1, -1, 0);

    if (icsPR != NoProc) {
	DestroyChildProcess(icsPR);
    }
    /* Save game if resource set and not already saved by GameEnds() */
    if (gameInfo.resultDetails == NULL && forwardMostMove > 0) {
	if (*appData.saveGameFile != NULLCHAR) {
	    SaveGameToFile(appData.saveGameFile);
	} else if (appData.autoSaveGames) {
	    AutoSaveGame();
	}
	if (*appData.savePositionFile != NULLCHAR) {
	    SavePositionToFile(appData.savePositionFile);
	}
    }
    GameEnds((ChessMove) 0, NULL, GE_PLAYER);

    exit(status);
}

void PauseEvent()
{
    if (pausing) {
	pausing = FALSE;
	ModeHighlight();
	if (gameMode == MachinePlaysWhite ||
	    gameMode == MachinePlaysBlack) {
	    StartClocks();
	} else {
	    DisplayBothClocks();
	}
	if (gameMode == PlayFromGameFile) {
	    if (appData.timeDelay >= 0) LoadGameLoop();
	} else if (gameMode == IcsExamining && pauseExamInvalid) {
	    Reset(FALSE);
	    SendToICS("refresh\n");
	} else if (currentMove < forwardMostMove) {
	    ForwardInner(forwardMostMove);
	}
	pauseExamInvalid = FALSE;
    } else {
	switch (gameMode) {
	  default:
	    return;
	  case IcsExamining:
	    pauseExamForwardMostMove = forwardMostMove;
	    pauseExamInvalid = FALSE;
	    /* fall through */
	  case IcsObserving:
	  case IcsPlayingWhite:
	  case IcsPlayingBlack:
	    pausing = TRUE;
	    ModeHighlight();
	    return;
	  case PlayFromGameFile:
	    (void) StopLoadGameTimer();
	    pausing = TRUE;
	    ModeHighlight();
	    break;
	  case BeginningOfGame:
	    if (appData.icsActive) return;
	    /* else fall through */
	  case MachinePlaysWhite:
	  case MachinePlaysBlack:
	  case TwoMachinesPlay:
	    if (forwardMostMove == 0)
	      return;		/* don't pause if no one has moved */
	    if ((gameMode == MachinePlaysWhite &&
		 !WhiteOnMove(forwardMostMove)) ||
		(gameMode == MachinePlaysBlack &&
		 WhiteOnMove(forwardMostMove))) {
		StopClocks();
	    }
	    pausing = TRUE;
	    ModeHighlight();
	    break;
	}
    }
}

void EditCommentEvent()
{
    char title[MSG_SIZ];

    if (currentMove < 1 || parseList[currentMove - 1][0] == NULLCHAR) {
	strcpy(title, "Edit comment");
    } else {
	sprintf(title, "Edit comment on %d.%s%s", (currentMove - 1) / 2 + 1,
		WhiteOnMove(currentMove - 1) ? " " : ".. ",
		parseList[currentMove - 1]);
    }

    EditCommentPopUp(currentMove, title, commentList[currentMove]);
}


void EditTagsEvent()
{
    char *tags = PGNTags(&gameInfo);
    EditTagsPopUp(tags);
    free(tags);
}

void AnalyzeModeEvent()
{
    if (appData.noChessProgram || gameMode == AnalyzeMode )
      return;

    if (gameMode == PlayFromGameFile || gameMode == TwoMachinesPlay ||
	gameMode == EndOfGame)
      EditGameEvent();
    if (gameMode == EditPosition) EditPositionDone();
   
    if (gameMode == EditGame) forwardMostMove = currentMove;
    ResurrectChessProgram();	/* in case it isn't running */

    if ( (gameMode==EditPosition && blackPlaysFirst == TRUE) ||
	(gameMode != EditPosition && !WhiteOnMove( currentMove )) )
      SendToProgram("black\n", firstProgramPR );
    else
      SendToProgram("white\n", firstProgramPR );

    if ( gameMode != AnalyzeFile )
      SendToProgram( "noise 1000\nanalyze\n", firstProgramPR );
	
    lastGameMode = gameMode = AnalyzeMode;
    pausing = FALSE;
    ModeHighlight();
    SetGameInfo();

    StartClocks();
    StartAnalysisClock();
    GetTimeMark( &lastNodeCountTime );
    lastNodeCount = 0;
}

void AnalyzeFileEvent()
{
    if (appData.noChessProgram || gameMode == AnalyzeFile )
      return;

    if (gameMode == PlayFromGameFile || gameMode == TwoMachinesPlay ||
	gameMode == EndOfGame)
      EditGameEvent();
    if (gameMode == EditPosition) EditPositionDone();
   
    if (gameMode == EditGame) forwardMostMove = currentMove;
    ResurrectChessProgram();	/* in case it isn't running */

    lastGameMode = gameMode = AnalyzeFile;
    pausing = FALSE;
    ModeHighlight();
    SetGameInfo();
    if ( blackPlaysFirst == TRUE )
      SendToProgram("black\nnoise 1000\nanalyze\n", firstProgramPR );
    else
      SendToProgram("white\nnoise 1000\nanalyze\n", firstProgramPR );	  
    StartClocks();
    StartAnalysisClock();
    GetTimeMark( &lastNodeCountTime );
    lastNodeCount = 0;
}

void MachineWhiteEvent()
{
    if (appData.noChessProgram || (gameMode == MachinePlaysWhite))
      return;
    if (gameMode == PlayFromGameFile || gameMode == TwoMachinesPlay ||
	gameMode == EndOfGame)
      EditGameEvent();
    if (gameMode == EditPosition) EditPositionDone();
    if (gameMode == AnalyzeMode || gameMode == AnalyzeFile )
	  ExitAnalyzeMode();
   
    if (!WhiteOnMove(gameMode == EditGame ? currentMove : forwardMostMove)) {
	DisplayError("It is not White's turn", 0);
	return;
    }
    if (gameMode == EditGame) forwardMostMove = currentMove;
    ResurrectChessProgram(); /* in case it isn't running */

    lastGameMode = gameMode = MachinePlaysWhite;
    pausing = FALSE;
    ModeHighlight();
    SetGameInfo();
    SendToProgram(appData.whiteString, firstProgramPR);
    StartClocks();
}

void MachineBlackEvent()
{
    if (appData.noChessProgram || (gameMode == MachinePlaysBlack))
      return;
    if (gameMode == PlayFromGameFile || gameMode == TwoMachinesPlay ||
	gameMode == EndOfGame)
      EditGameEvent();
    if (gameMode == EditPosition) EditPositionDone();
    if (gameMode == AnalyzeMode || gameMode == AnalyzeFile )
	  ExitAnalyzeMode();
    
    if (WhiteOnMove(gameMode == EditGame ? currentMove : forwardMostMove)) {
	DisplayError("It is not Black's turn", 0);
	return;
    }
    if (gameMode == EditGame) forwardMostMove = currentMove;
    ResurrectChessProgram(); /* in case it isn't running */

    lastGameMode = gameMode = MachinePlaysBlack;
    pausing = FALSE;
    ModeHighlight();
    SetGameInfo();
    SendToProgram(appData.blackString, firstProgramPR);
    StartClocks();
}


void TwoMachinesEvent()
{
    int i;
    
    if (appData.noChessProgram) return;
    
    switch (gameMode) {
      case TwoMachinesPlay:
	return;
      case MachinePlaysWhite:
      case MachinePlaysBlack:
      case BeginningOfGame:
      case PlayFromGameFile:
      case EndOfGame:
	EditGameEvent();
	if (gameMode != EditGame) return;
	break;
      case EditPosition:
	EditPositionDone();
	break;
      case AnalyzeMode:
      case AnalyzeFile:
	ExitAnalyzeMode();
	break;
      case EditGame:
      default:
	break;
    }
    
    forwardMostMove = currentMove;
    ResurrectChessProgram(); /* in case first program isn't running */

    InitChessProgram(appData.secondHost, appData.secondChessProgram,
		     &secondProgramPR, &secondProgramISR, &secondSendTime);
    if (startedFromSetupPosition) {
	if (blackPlaysFirst) {
	    SendToProgram("force\na2a3\n", secondProgramPR);
	    SendBoard(secondProgramPR, boards[backwardMostMove]);
	} else {
	    SendBoard(secondProgramPR, boards[backwardMostMove]);
	    SendToProgram("force\n", secondProgramPR);
	}
    } else {
	SendToProgram("force\n", secondProgramPR);
    }
    for (i = backwardMostMove; i < forwardMostMove; i++) {
	SendToProgram(moveList[i], secondProgramPR);
    }
    lastGameMode = gameMode = TwoMachinesPlay;
    pausing = FALSE;
    ModeHighlight();
    SetGameInfo();
    firstMove = TRUE;
    if (WhiteOnMove(forwardMostMove))
      SendToProgram(appData.whiteString, secondProgramPR);
    else
      SendToProgram(appData.blackString, firstProgramPR);
	
    if (!firstSendTime || !secondSendTime) {
	ResetClocks();
	timeRemaining[0][forwardMostMove] = whiteTimeRemaining;
	timeRemaining[1][forwardMostMove] = blackTimeRemaining;
    }
    StartClocks();
}


void IcsClientEvent()
{
    if (!appData.icsActive) return;
    switch (gameMode) {
      case IcsPlayingWhite:
      case IcsPlayingBlack:
      case IcsObserving:
      case IcsIdle:
      case BeginningOfGame:
      case IcsExamining:
	return;

      case EditGame:
	break;

      case EditPosition:
	EditPositionDone();
	break;

      case AnalyzeMode:
      case AnalyzeFile:
	ExitAnalyzeMode();
	break;
	
      default:
	EditGameEvent();
	break;
    }

    gameMode = IcsIdle;
    ModeHighlight();
    return;
}


void EditGameEvent()
{
    int i;
    
    switch (gameMode) {
      case MachinePlaysWhite:
	if (WhiteOnMove(forwardMostMove)) {
	    DisplayError("Wait until your turn,\nor select Move Now", 0);
	    return;
	}
	SendToProgram("force\n", firstProgramPR);
	break;
      case MachinePlaysBlack:
	if (!WhiteOnMove(forwardMostMove)) {
	    DisplayError("Wait until your turn,\nor select Move Now", 0);
	    return;
	}
	SendToProgram("force\n", firstProgramPR);
	break;
      case BeginningOfGame:
	SendToProgram("force\n", firstProgramPR);
	break;
      case PlayFromGameFile:
        (void) StopLoadGameTimer();
	if (gameFileFP != NULL) {
	    gameFileFP = NULL;
	}
	break;
      case EditPosition:
	EditPositionDone();
	break;
      case AnalyzeMode:
      case AnalyzeFile:
	ExitAnalyzeMode();
	break;
      case TwoMachinesPlay:
	GameEnds((ChessMove) 0, NULL, GE_PLAYER);
	ResurrectChessProgram();
	break;
      case EndOfGame:
	ResurrectChessProgram();
	break;
      case IcsPlayingBlack:
      case IcsPlayingWhite:
	DisplayError("Warning: You are still playing a game", 0);
	break;
      case IcsObserving:
	DisplayError("Warning: You are still observing a game", 0);
	break;
      case IcsExamining:
	DisplayError("Warning: You are still examining a game", 0);
	break;
      case IcsIdle:
	break;
      case EditGame:
      default:
	return;
    }
    
    pausing = FALSE;
    StopClocks();

    if (gameMode == MachinePlaysWhite ||
	gameMode == MachinePlaysBlack ||
	gameMode == TwoMachinesPlay ||
	gameMode == PlayFromGameFile) {
	i = forwardMostMove;
	while (i > currentMove) {
	    SendToProgram("undo\n", firstProgramPR);
	    i--;
	}
	whiteTimeRemaining = timeRemaining[0][currentMove];
	blackTimeRemaining = timeRemaining[1][currentMove];
	DisplayBothClocks();
	if (whiteFlag || blackFlag) {
	    whiteFlag = blackFlag = 0;
	}
	DisplayTitle("");
    }		
    
    lastGameMode = gameMode = EditGame;
    ModeHighlight();
    SetGameInfo();
}


void EditPositionEvent()
{
    if (gameMode == EditPosition) {
	EditGameEvent();
	return;
    }
    
    EditGameEvent();
    if (gameMode != EditGame) return;
    
    lastGameMode = gameMode = EditPosition;
    ModeHighlight();
    SetGameInfo();
    if (currentMove > 0)
      CopyBoard(boards[0], boards[currentMove]);
    
    blackPlaysFirst = !WhiteOnMove(currentMove);
    ResetClocks();
    currentMove = forwardMostMove = backwardMostMove = 0;
}

void ExitAnalyzeMode()
{
  SendToProgram( "exit\n", firstProgramPR );
  AnalysisPopDown();
}

void EditPositionDone()
{
    startedFromSetupPosition = TRUE;
    SendToProgram(appData.initString, firstProgramPR);
    SendTimeControl(firstProgramPR, appData.movesPerSession,
		    timeControl, appData.timeIncrement,
		    appData.searchDepth, searchTime);
    if (blackPlaysFirst) {
	strcpy(moveList[0], "");
	strcpy(parseList[0], "");
	currentMove = forwardMostMove = backwardMostMove = 1;
	CopyBoard(boards[1], boards[0]);
	SendToProgram("force\na2a3\n", firstProgramPR);
	SendCurrentBoard(firstProgramPR);
	DisplayTitle("");
    } else {
	currentMove = forwardMostMove = backwardMostMove = 0;
	SendCurrentBoard(firstProgramPR);
	SendToProgram("force\n", firstProgramPR);
	DisplayTitle("");
    }
    timeRemaining[0][forwardMostMove] = whiteTimeRemaining;
    timeRemaining[1][forwardMostMove] = blackTimeRemaining;
    lastGameMode = gameMode = EditGame;
    ModeHighlight();
}

/* Pause for `ms' milliseconds */
void TimeDelay( ms )
	 long ms;
{
  TimeMark m1, m2;

  GetTimeMark( &m1 );

  do {
	GetTimeMark( &m2 );
  } while( SubtractTimeMarks( &m2, &m1 ) < ms );
}

void SendMultiLineToICS( buf )
	 char *buf;
{
  char temp[MSG_SIZ+1], *p;
  int len;

  len = strlen( buf );
  if ( len > MSG_SIZ )
	len = MSG_SIZ;
  
  strncpy( temp, buf, len );
  temp[len] = 0;

  p = temp;
  while( *p ) {
	if ( *p == '\n' || *p == '\r' )
	  *p = ' ';
	++p;
  }

  SendToICS( temp );
  SendToICS( "\n" );

  /* Echo to terminal */
  fprintf( stderr, "%s\n", temp );
}

void SetWhiteToPlayEvent()
{
    if (gameMode != EditPosition) return;
    blackPlaysFirst = FALSE;
    DisplayBothClocks(); /* works because currentMove is 0 */
}

void SetBlackToPlayEvent()
{
    if (gameMode != EditPosition) return;
    blackPlaysFirst = TRUE;
    currentMove = 1;	/* kludge */
    DisplayBothClocks();
    currentMove = 0;
}

void EditPositionMenuEvent(selection, x, y)
     ChessSquare selection;
     int x, y;
{
    char buf[MSG_SIZ];

    if (gameMode != EditPosition && gameMode != IcsExamining) return;

    switch (selection) {
      case ClearBoard:
	for (x = 0; x < BOARD_SIZE; x++)
	  for (y = 0; y < BOARD_SIZE; y++) {
	      if (gameMode == IcsExamining) {
		  if (boards[currentMove][y][x] != EmptySquare) {
		      sprintf(buf, "x@%c%c\n", 'a' + x, '1' + y);
		      SendToICS(buf);
		  }
	      } else /* gameMode == EditPosition */ {
		  boards[0][y][x] = EmptySquare;
	      }
	  }
	if (gameMode == EditPosition)
	  DrawPosition(FALSE, boards[0]);
	break;

      case WhitePlay:
	SetWhiteToPlayEvent();
	break;

      case BlackPlay:
	SetBlackToPlayEvent();
	break;

      case EmptySquare:
	if (gameMode == IcsExamining) {
	    sprintf(buf, "x@%c%c\n", 'a' + x, '1' + y);
	    SendToICS(buf);
	} else {
	    boards[0][y][x] = EmptySquare;
	    DrawPosition(FALSE, boards[0]);
	}
	break;

      default:
        if (gameMode == IcsExamining) {
	    sprintf(buf, "%c@%c%c\n",
		    PieceToChar(selection), 'a' + x, '1' + y);
	    SendToICS(buf);
	} else {
	    boards[0][y][x] = selection;
	    DrawPosition(FALSE, boards[0]);
	}
	break;
    }
}


void DropMenuEvent(selection, x, y)
     ChessSquare selection;
     int x, y;
{
    ChessMove moveType;

    switch (gameMode) {
      case IcsPlayingWhite:
	if (!WhiteOnMove(currentMove)) {
	    DisplayError("It is Black's turn", 0);
	    return;
	}
	moveType = WhiteDrop;
	break;
      case IcsPlayingBlack:
	if (WhiteOnMove(currentMove)) {
	    DisplayError("It is White's turn", 0);
	    return;
	}
	selection = (ChessSquare) ((int) selection
				   + (int) BlackPawn - (int) WhitePawn);
	moveType = BlackDrop;
	break;
      default:
	return;
    }

    if (boards[currentMove][y][x] != EmptySquare) {
	DisplayError("That square is occupied", 0);
    } else {
	if (pausing) PauseEvent();
	MakeMove(&moveType, (int) selection, DROP_RANK, x, y);
	SendMoveToICS(moveType, (int) selection, DROP_RANK, x, y);
	ics_user_moved = 1;
    }
}

void AcceptEvent()
{
    /* Accept a pending offer of any kind from opponent */
    
    if (appData.icsActive) {
	SendToICS("accept\n");
    } else if (cmailMsgLoaded) {
	if (currentMove == cmailOldMove &&
	    commentList[cmailOldMove] != NULL &&
	    StrStr(commentList[cmailOldMove], WhiteOnMove(cmailOldMove) ?
		   "Black offers a draw" : "White offers a draw")) {
	    TruncateGame();
	    GameEnds(GameIsDrawn, "Draw agreed", GE_PLAYER);
	    cmailMoveType[lastLoadGameNumber - 1] = CMAIL_ACCEPT;
	} else {
	    DisplayError("There is no pending offer on this move", 0);
	    cmailMoveType[lastLoadGameNumber - 1] = CMAIL_MOVE;
	}
    } else {
	/* Currently GNU Chess doesn't make offers at all */
    }
}

void DeclineEvent()
{
    /* Decline a pending offer of any kind from opponent */
    
    if (appData.icsActive) {
	SendToICS("decline\n");
    } else if (cmailMsgLoaded) {
	if (currentMove == cmailOldMove &&
	    commentList[cmailOldMove] != NULL &&
	    StrStr(commentList[cmailOldMove], WhiteOnMove(cmailOldMove) ?
		   "Black offers a draw" : "White offers a draw")) {
#ifdef NOTDEF
	    AppendComment(cmailOldMove, "Draw declined");
	    DisplayComment(cmailOldMove - 1, "Draw declined");
#endif /*NOTDEF*/
	} else {
	    DisplayError("There is no pending offer on this move", 0);
	}
    } else {
	/* Currently GNU Chess doesn't make offers at all */
    }
}

void CallFlagEvent()
{
    /* Call your opponent's flag (claim a win on time) */
    if (appData.icsActive) {
	SendToICS("flag\n");
    } else {
	switch (gameMode) {
	  default:
	    return;
	  case MachinePlaysWhite:
	    if (whiteFlag) {
		if (blackFlag)
		  GameEnds(GameIsDrawn, "Both players ran out of time",
			   GE_PLAYER);
		else
		  GameEnds(BlackWins, "Black wins on time", GE_PLAYER);
	    } else {
		DisplayError("Your opponent is not out of time", 0);
	    }
	    break;
	  case MachinePlaysBlack:
	    if (blackFlag) {
		if (whiteFlag)
		  GameEnds(GameIsDrawn, "Both players ran out of time",
			   GE_PLAYER);
		else
		  GameEnds(WhiteWins, "White wins on time", GE_PLAYER);
	    } else {
		DisplayError("Your opponent is not out of time", 0);
	    }
	    break;
	}
    }
}

void DrawEvent()
{
    /* Offer draw or accept pending draw offer from opponent */
    
    if (appData.icsActive) {
	/* Note: tournament rules require draw offers to be
	   made after you make your move but before you punch
	   your clock.  Currently ICS doesn't let you do that;
	   instead, you immediately punch your clock after making
	   a move, but you can offer a draw at any time. */
	
	SendToICS("draw\n");
    } else if (cmailMsgLoaded) {
	if (currentMove == cmailOldMove &&
	    commentList[cmailOldMove] != NULL &&
	    StrStr(commentList[cmailOldMove], WhiteOnMove(cmailOldMove) ?
		   "Black offers a draw" : "White offers a draw")) {
	    GameEnds(GameIsDrawn, "Draw agreed", GE_PLAYER);
	    cmailMoveType[lastLoadGameNumber - 1] = CMAIL_ACCEPT;
	} else if (currentMove == cmailOldMove + 1) {
	    char *offer = WhiteOnMove(cmailOldMove) ?
	      "White offers a draw" : "Black offers a draw";
	    AppendComment(currentMove, offer);
	    DisplayComment(currentMove - 1, offer);
	    cmailMoveType[lastLoadGameNumber - 1] = CMAIL_DRAW;
	} else {
	    DisplayError("You must make your move before offering a draw", 0);
	    cmailMoveType[lastLoadGameNumber - 1] = CMAIL_MOVE;
	}
    } else {
	/* Currently GNU Chess doesn't offer or accept draws */
    }
}

void AdjournEvent()
{
    /* Offer Adjourn or accept pending Adjourn offer from opponent */
    
    if (appData.icsActive) {
	SendToICS("adjourn\n");
    } else {
	/* Currently GNU Chess doesn't offer or accept Adjourns */
    }
}


void AbortEvent()
{
    /* Offer Abort or accept pending Abort offer from opponent */
    
    if (appData.icsActive) {
	SendToICS("abort\n");
    } else {
	GameEnds(GameUnfinished, "Game aborted", GE_PLAYER);
    }
}

void ResignEvent()
{
    /* Resign.  You can do this even if it's not your turn. */
    
    if (appData.icsActive) {
	SendToICS("resign\n");
    } else {
	switch (gameMode) {
	  case MachinePlaysWhite:
	    GameEnds(WhiteWins, "Black resigns", GE_PLAYER);
	    break;
	  case MachinePlaysBlack:
	    GameEnds(BlackWins, "White resigns", GE_PLAYER);
	    break;
	  case EditGame:
	    if (cmailMsgLoaded) {
		TruncateGame();
		if (WhiteOnMove(cmailOldMove)) {
		    GameEnds(BlackWins, "White resigns", GE_PLAYER);
		} else {
		    GameEnds(WhiteWins, "Black resigns", GE_PLAYER);
		}
		cmailMoveType[lastLoadGameNumber - 1] = CMAIL_RESIGN;
	    }
	    break;
	  default:
	    break;
	}
    }
}


void StopObservingEvent()
{
    /* Stop observing current games */
    SendToICS("unobserve\n");
}

void StopExaminingEvent()
{
    /* Stop observing current game */
    SendToICS("unexamine\n");
}

void ForwardInner(target)
     int target;
{
    int limit;

    if (gameMode == EditPosition)
      return;
    
    if (gameMode == PlayFromGameFile && !pausing)
      PauseEvent();
    
    if (gameMode == IcsExamining && pausing)
      limit = pauseExamForwardMostMove;
    else
      limit = forwardMostMove;

    if (currentMove >= limit) {
	if (gameFileFP != NULL)
	  (void) LoadGameOneMove();
	return;
    }
    
    if (target > limit) target = limit;
    if (gameMode == EditGame || gameMode == AnalyzeMode ||
		gameMode == AnalyzeFile ) {
	while (currentMove < target) {
	    SendToProgram(moveList[currentMove++], firstProgramPR);
	}
    } else {
	currentMove = target;
    }
    
    if (gameMode == EditGame || gameMode == EndOfGame) {
	whiteTimeRemaining = timeRemaining[0][currentMove];
	blackTimeRemaining = timeRemaining[1][currentMove];
    }
    DisplayBothClocks();
    DisplayMove(currentMove - 1);
    DrawPosition(FALSE, boards[currentMove]);
    if (commentList[currentMove] != NULL) {
	DisplayComment(currentMove - 1, commentList[currentMove]);
    }
}


void ForwardEvent()
{
    if (gameMode == IcsExamining && !pausing) {
	SendToICS("forward\n");
    } else {
	ForwardInner(currentMove + 1);
    }
}

void ToEndEvent()
{
    if (gameMode == IcsExamining && !pausing) {
	SendToICS("forward 999999\n");
    } else {
	ForwardInner(forwardMostMove);
    }
}

void BackwardInner(target)
     int target;
{
    if ((currentMove <= backwardMostMove) || (gameMode == EditPosition))
      return;
    
    if (gameMode == PlayFromGameFile && !pausing)
      PauseEvent();
    
    if (gameMode == EditGame || gameMode==AnalyzeMode ||
		gameMode == AnalyzeFile ) {
	while (currentMove > target) {
	    SendToProgram("undo\n", firstProgramPR);
	    currentMove--;
	}
    } else {
	currentMove = target;
    }
    
    if (gameMode == EditGame || gameMode == EndOfGame) {
	whiteTimeRemaining = timeRemaining[0][currentMove];
	blackTimeRemaining = timeRemaining[1][currentMove];
    }
    DisplayBothClocks();
    DisplayMove(currentMove - 1);
    DrawPosition(FALSE, boards[currentMove]);
    if (commentList[currentMove] != NULL) {
	DisplayComment(currentMove - 1, commentList[currentMove]);
    }
}

void BackwardEvent()
{
    if (gameMode == IcsExamining && !pausing) {
	SendToICS("backward\n");
    } else {
	BackwardInner(currentMove - 1);
    }
}

void ToStartEvent()
{
  if ( gameMode == AnalyzeMode || gameMode == AnalyzeFile ) {

	/* We do backwardInner() manually in Analysis mode to optimize */
	
	SendToProgram( "new\n", firstProgramPR );
	currentMove=backwardMostMove;
    DisplayBothClocks();
    DisplayMove(currentMove - 1);
    DrawPosition(FALSE, boards[currentMove]);
    if (commentList[currentMove] != NULL) {
	  DisplayComment(currentMove - 1, commentList[currentMove]);
	}
	return;
  }
  
    if (gameMode == IcsExamining && !pausing) {
	SendToICS("backward 999999\n");
    } else {
	BackwardInner(backwardMostMove);
    }
}

void RevertEvent()
{
    if (gameMode != IcsExamining) {
	DisplayError("You are not examining a game", 0);
	return;
    }
    if (pausing) {
	DisplayError("You can't revert while pausing", 0);
	return;
    }
    SendToICS("revert\n");
}

void RetractMoveEvent()
{
    switch (gameMode) {
      case MachinePlaysWhite:
      case MachinePlaysBlack:
	if (WhiteOnMove(forwardMostMove) == (gameMode == MachinePlaysWhite)) {
	    DisplayError("Wait until your turn,\nor select Move Now", 0);
	    return;
	}
	if (forwardMostMove < 2) return;
	currentMove = forwardMostMove = forwardMostMove - 2;
	whiteTimeRemaining = timeRemaining[0][currentMove];
	blackTimeRemaining = timeRemaining[1][currentMove];
	DisplayBothClocks();
	DisplayMove(currentMove - 1);
	DrawPosition(FALSE, boards[currentMove]);
	SendToProgram("remove\n", firstProgramPR);
	break;

      case BeginningOfGame:
      default:
	break;

      case IcsPlayingWhite:
      case IcsPlayingBlack:
	if (WhiteOnMove(forwardMostMove) == (gameMode == IcsPlayingWhite)) {
	    SendToICS("takeback 2\n");
	} else {
	    SendToICS("takeback 1\n");
	}
	break;
    }
}

void MoveNowEvent()
{
    switch (gameMode) {
      case MachinePlaysWhite:
	if (!WhiteOnMove(forwardMostMove)) {
	    DisplayError("It is your turn", 0);
	    return;
	}
	InterruptChildProcess(firstProgramPR);
	break;
      case MachinePlaysBlack:
	if (WhiteOnMove(forwardMostMove)) {
	    DisplayError("It is your turn", 0);
	    return;
	}
	InterruptChildProcess(firstProgramPR);
	break;
      case TwoMachinesPlay:
	if (WhiteOnMove(forwardMostMove)) {
	    InterruptChildProcess(secondProgramPR);
	} else {
	    InterruptChildProcess(firstProgramPR);
	}
	break;
      case BeginningOfGame:
      default:
	break;
    }
}

void TruncateGameEvent()
{
    EditGameEvent();
    if (gameMode != EditGame) return;
    TruncateGame();
}

void TruncateGame()
{
    if (forwardMostMove > currentMove) {
	if (gameInfo.resultDetails != NULL) {
	    free(gameInfo.resultDetails);
	    gameInfo.resultDetails = NULL;
	    gameInfo.result = GameUnfinished;
	}
	forwardMostMove = currentMove;
    }
}

void HintEvent()
{
    if (appData.noChessProgram) return;
    switch (gameMode) {
      case MachinePlaysWhite:
	if (WhiteOnMove(forwardMostMove)) {
	    DisplayError("Wait until your turn", 0);
	    return;
	}
	break;
      case BeginningOfGame:
      case MachinePlaysBlack:
	if (!WhiteOnMove(forwardMostMove)) {
	    DisplayError("Wait until your turn", 0);
	    return;
	}
	break;
      default:
	DisplayError("No hint available", 0);
	return;
    }
    SendToProgram("hint\n", firstProgramPR);
    hintRequested = TRUE;
}

void BookEvent()
{
    if (appData.noChessProgram) return;
    switch (gameMode) {
      case MachinePlaysWhite:
	if (WhiteOnMove(forwardMostMove)) {
	    DisplayError("Wait until your turn", 0);
	    return;
	}
	break;
      case BeginningOfGame:
      case MachinePlaysBlack:
	if (!WhiteOnMove(forwardMostMove)) {
	    DisplayError("Wait until your turn", 0);
	    return;
	}
	break;
      case EditPosition:
	EditPositionDone();
	break;
      case TwoMachinesPlay:
	return;
      default:
	break;
    }
    SendToProgram("bk\n", firstProgramPR);
    bookOutput[0] = NULLCHAR;
    bookRequested = TRUE;
}

void AboutGameEvent()
{
    char *tags = PGNTags(&gameInfo);
    TagsPopUp(tags, CmailMsg());
    free(tags);
}

/* end button procedures */

void PrintPosition(fp, move)
     FILE *fp;
     int move;
{
    int i, j;
    
    for (i = BOARD_SIZE - 1; i >= 0; i--) {
	for (j = 0; j < BOARD_SIZE; j++) {
	    fprintf(fp, "%c", PieceToChar(boards[move][i][j]));
	    fputc(j == BOARD_SIZE - 1 ? '\n' : ' ', fp);
	}
    }
    if ((gameMode == EditPosition) ? !blackPlaysFirst : (move % 2 == 0))
      fprintf(fp, "white to play\n");
    else
      fprintf(fp, "black to play\n");
}

void PrintOpponents(fp)
     FILE *fp;
{
    if (gameInfo.white != NULL) {
	fprintf(fp, "\t%s vs. %s\n", gameInfo.white, gameInfo.black);
    } else {
	fprintf(fp, "\n");
    }
}

void SetGameInfo()
{
    char buf[MSG_SIZ];

    /* This routine is used only for certain modes */
    ClearGameInfo(&gameInfo);

    switch (gameMode) {
      case MachinePlaysWhite:
      case MachinePlaysBlack:
      case TwoMachinesPlay:
	gameInfo.event = StrSave("GNU Chess game");
	gameInfo.site = StrSave(HostName());
	gameInfo.date = PGNDate();
	gameInfo.round = StrSave("-");
	if (strcmp(appData.firstHost, "localhost") != 0) {
	    sprintf(buf, "%s@%s",
		    appData.firstChessProgram, appData.firstHost);
	} else {
	    strcpy(buf, appData.firstChessProgram);
	}
	if (gameMode == MachinePlaysWhite) {
	    gameInfo.white = StrSave(buf);
	    gameInfo.black = StrSave(UserName());
	} else if (gameMode == MachinePlaysBlack) {
	    gameInfo.white = StrSave(UserName());
	    gameInfo.black = StrSave(buf);
	} else /* gameMode == TwoMachinesPlay */ {
	    gameInfo.black = StrSave(buf);
	    if (strcmp(appData.secondHost, "localhost") != 0) {
		sprintf(buf, "%s@%s",
			appData.secondChessProgram, appData.secondHost);
		gameInfo.white = StrSave(buf);
	    } else {
		gameInfo.white = StrSave(appData.secondChessProgram);
	    }
	}
	if (appData.timeIncrement == 0) {
	    sprintf(buf, "%ld", timeControl/1000);
	} else if (appData.timeIncrement > 0) {
	    sprintf(buf, "%ld+%d",
		    timeControl/1000, appData.timeIncrement);
	} else {
	    sprintf(buf, "%d/%ld", appData.movesPerSession, timeControl/1000);
	}
	gameInfo.timeControl = StrSave(buf);
	break;

      case EditGame:
	gameInfo.event = StrSave("Edited game");
	gameInfo.site = StrSave(HostName());
	gameInfo.date = PGNDate();
	gameInfo.round = StrSave("-");
	gameInfo.white = StrSave("-");
	gameInfo.black = StrSave("-");
	break;

      case EditPosition:
	gameInfo.event = StrSave("Edited position");
	gameInfo.site = StrSave(HostName());
	gameInfo.date = PGNDate();
	gameInfo.round = StrSave("-");
	gameInfo.white = StrSave("-");
	gameInfo.black = StrSave("-");
	break;

      case IcsPlayingWhite:
      case IcsPlayingBlack:
      case IcsObserving:
      case IcsExamining:
	break;

      case PlayFromGameFile:
	gameInfo.event = StrSave("Game from non-PGN file");
	gameInfo.site = StrSave(HostName());
	gameInfo.date = PGNDate();
	gameInfo.round = StrSave("-");
	gameInfo.white = StrSave("?");
	gameInfo.black = StrSave("?");
	break;

      default:
	break;
    }
}

void ReplaceComment(index, text)
     int index;
     char *text;
{
    int len;

    while (*text == '\n') text++;
    len = strlen(text);
    while (len > 0 && text[len - 1] == '\n') len--;

    if (commentList[index] != NULL)
      free(commentList[index]);

    if (len == 0) {
	commentList[index] = NULL;
	return;
    }
    commentList[index] = (char *) malloc(len + 2);
    strncpy(commentList[index], text, len);
    commentList[index][len] = '\n';
    commentList[index][len + 1] = NULLCHAR;
}

void AppendComment(index, text)
     int index;
     char *text;
{
    int oldlen, len;
    char *old;

    while (*text == '\n') text++;
    len = strlen(text);
    while (len > 0 && text[len - 1] == '\n') len--;

    if (len == 0) return;

    if (commentList[index] != NULL) {
	old = commentList[index];
	oldlen = strlen(old);
	commentList[index] = (char *) malloc(oldlen + len + 2);
	strcpy(commentList[index], old);
	free(old);
	strncpy(&commentList[index][oldlen], text, len);
	commentList[index][oldlen + len] = '\n';
	commentList[index][oldlen + len + 1] = NULLCHAR;
    } else {
	commentList[index] = (char *) malloc(len + 2);
	strncpy(commentList[index], text, len);
	commentList[index][len] = '\n';
	commentList[index][len + 1] = NULLCHAR;
    }
}

void SendToProgram(message, pr)
     char *message;
     ProcRef pr;
{
    int count, outCount, error;
    char *which;
    char buf[MSG_SIZ];

    if (pr == NULL) return;
    Attention(pr);
    lastMsgPR = pr;
    which = (pr == firstProgramPR) ? "first" : "second";
    
    if (appData.debugMode)
      fprintf(debugFP, "Sending to %s: %s", which, message);
    
    count = strlen(message);
    outCount = OutputToProcess(pr, message, count, &error);
    if (outCount < count) {
	sprintf(buf, "Error writing to %s chess program", which);
	DisplayFatalError(buf, error, 1);
    }
}

void ReceiveFromProgram(isr, message, count, error)
     InputSourceRef isr;
     char *message;
     int count;
     int error;
{
    char *end_str, *name, *which;
    char buf[MSG_SIZ];

    if (count <= 0) {
	if (isr == firstProgramISR) {
	    which = "first";
	    name = appData.firstChessProgram;
	} else if (isr == secondProgramISR) {
	    which = "second";
	    name = appData.secondChessProgram;
	} else {
	    return;
	}
	if (count == 0) {
	    sprintf(buf,
		    "Error: %s chess program (%s) exited unexpectedly",
		    which, name);
	    RemoveInputSource(isr);
	    DisplayFatalError(buf, 0, -1); /* don't exit */
	} else {
	    sprintf(buf,
		    "Error reading from %s chess program (%s)",
		    which, name);
	    RemoveInputSource(isr);
	    DisplayFatalError(buf, error, -1); /* don't exit */
	}
	GameEnds((ChessMove) 0, NULL, GE_PLAYER);
	return;
    }
    
    if ((end_str = strchr(message, '\r')) != NULL)
      *end_str = NULLCHAR;
    if ((end_str = strchr(message, '\n')) != NULL)
      *end_str = NULLCHAR;
    
    if (appData.debugMode) {
	fprintf(debugFP, "Received from %s: %s\n",
		isr == firstProgramISR ? "first" : "second", message);
    }
    HandleMachineMove(message, isr);
}


void
SendTimeControl(pr, mps, tc, inc, sd, st)
     ProcRef pr;
     int mps, inc, sd, st;
     long tc;
{
    char buf[MSG_SIZ];
    int seconds = (tc / 1000) % 60;

    if (st > 0) {
	/* This works only for Crafty.  For gnuchessx, we set
	   search time on the command line in InitChessProgram */
	sprintf(buf, "st %d\n", st);
    } else if (sd > 0) {
	/* Search to given depth in unlimited time */
	/* Send both gnuchessx command (depth) and Crafty command (sd) */
	sprintf(buf, "level 1 9999\ndepth\n%d\nsd %d\n", sd, sd);
    } else {
	if (inc >= 0)
	  mps = 0;
	else
	  inc = 0;

	if (seconds == 0) {
	    /* Note gnuchess bug -- minutes:seconds doesn't work.
	       Maybe it will be fixed in the future, so just avoid :seconds
	       when seconds is 0. */
	    sprintf(buf, "level %d %ld %d\n", mps, tc/60000, inc);
	} else {
	    sprintf(buf, "level %d %ld:%02d %d\n", mps, tc/60000,
		    seconds, inc);
	}
    }
    SendToProgram(buf, pr);
    SendTimeRemaining(pr);

    /* note kludge: "bogus" command forces gnuchessx to print out
       something that ends with a newline.  This is an error message
       in all known versions of gnuchess, but that's OK. */ 
    SendToProgram("bogus\n", pr);
}

void SendTimeRemaining(pr)
     ProcRef pr;
{
    char message[MSG_SIZ];
    long time, otime;

    /* Note: this routine must be called when the clocks are stopped
       or when they have *just* been set or switched; otherwise
       it will be off by the time since the current tick started.
    */
    if (WhiteOnMove(forwardMostMove)) {
	time = whiteTimeRemaining / 10;
	otime = (blackTimeRemaining - ics_increment) / 10;
    } else {
	time = blackTimeRemaining / 10;
	otime = (whiteTimeRemaining - ics_increment) / 10;
    }
    if (time <= 0) time = 1;
    if (otime <= 0) otime = 1;
    
    sprintf(message, "time %ld\notim %ld\n", time, otime);
    SendToProgram(message, pr);
}

void PeriodicUpdatesEvent(newState)
	 int newState;
{
  if ( newState==appData.periodicUpdates )
	return;

  appData.periodicUpdates=newState;

  /* Display type changes, so update it now */
  DisplayAnalysis();

  /* Get the ball rolling again... */
  if ( newState==True )
	AnalysisPeriodicEvent(1);
}

void ShowThinkingEvent(newState)
     int newState;
{
    if (newState == appData.showThinking) return;
    switch (gameMode) {
      case MachinePlaysWhite:
	if (WhiteOnMove(forwardMostMove)) {
	    DisplayError("Wait until your turn", 0);
	    return;
	}
	break;
      case BeginningOfGame:
      case MachinePlaysBlack:
	if (!WhiteOnMove(forwardMostMove)) {
	    DisplayError("Wait until your turn", 0);
	    return;
	}
	break;
      case EditPosition:
	EditPositionDone();
	break;
      case TwoMachinesPlay:
	return;
      default:
	break;
    }
    if (newState) {
	SendToProgram("post\n", firstProgramPR);
    } else {
	SendToProgram("nopost\n", firstProgramPR);
	thinkOutput[0] = NULLCHAR;
    }
    appData.showThinking = newState;
}

void DisplayMove(moveNumber)
     int moveNumber;
{
    char message[MSG_SIZ];
    char res[MSG_SIZ];
	char cpThinkOutput[MSG_SIZ];

	strcpy( cpThinkOutput, thinkOutput );
	if ( strchr( cpThinkOutput, '\n' ) )
	  *strchr( cpThinkOutput, '\n' ) = 0;

    if (moveNumber == forwardMostMove - 1 &&
	gameInfo.resultDetails != NULL) {
	if (gameInfo.resultDetails[0] == NULLCHAR) {
	    sprintf(res, " %s", PGNResult(gameInfo.result));
	} else {
	    sprintf(res, " {%s} %s",
		    gameInfo.resultDetails, PGNResult(gameInfo.result));
	}
    } else {
	res[0] = NULLCHAR;
    }
    
    if (moveNumber < 0 || parseList[moveNumber][0] == NULLCHAR) {
	DisplayMessage(res,
		       moveNumber == forwardMostMove - 1 ? cpThinkOutput : "");
    } else {
	sprintf(message, "%d.%s%s%s", moveNumber / 2 + 1,
		WhiteOnMove(moveNumber) ? " " : ".. ",
		parseList[moveNumber], res);
	DisplayMessage(message,
		       moveNumber == forwardMostMove - 1 ? cpThinkOutput : "");
    }
}

void DisplayAnalysisText( text )
	 char *text;
{
  char buf[MSG_SIZ];

  if ( !isCraftyRunning || (craftyMajorVersion < 9) ||
	  (craftyMajorVersion == 9 && craftyMinorVersion < 25 ) )
	return;

  if ( gameMode == AnalyzeMode || gameMode == AnalyzeFile )
	{
	  sprintf( buf, "Analysis (%s)", craftyProgramName );
	  AnalysisPopUp( buf, text );
    }
}

static int only_one_move( char *str )
{
  while( *str && isspace( *str ) ) 	++str;
  while( *str && !isspace( *str ) ) ++str;

  if ( !*str )
	return 1;

  while( *str && isspace( *str ) ) ++str;
  if ( !*str )
	return 1;

  return 0;
}

void DisplayAnalysis()
{
  char buf[MSG_SIZ];
  float nps;
  char *xtra[] = { "", " (--)", " (++)" };
  int h, m, s;
  
  if ( programStats.time == 0 )
	programStats.time = 1;
  
  if ( programStats.got_only_move )
	strcpy( buf, programStats.movelist );
  else
	{
	  nps = ((float)programStats.nodes) /
		(((float)programStats.time)/100.0);

	  s = programStats.time/100.0;
	  h = (s / (60*60));
	  s = s - h*60*60;
	  m = (s/60);
	  s = s - m*60;

	  if ( programStats.moves_left > 0 && appData.periodicUpdates==True )
		sprintf( buf, "depth=%d %d/%d %+.2f %s%s\nNodes: %d NPS: %d\nTime: %02d:%02d:%02d",
				programStats.depth,
				programStats.nr_moves-programStats.moves_left,
				programStats.nr_moves, ((float)programStats.score)/100.0,
				programStats.movelist,
				only_one_move(programStats.movelist)?
				xtra[programStats.got_fail] : "",
				programStats.nodes, (int)nps, h, m, s );
	  else
		sprintf( buf, "depth=%d %+.2f %s%s\nNodes: %d NPS: %d\nTime: %02d:%02d:%02d",
				programStats.depth,
				((float)programStats.score)/100.0,
				programStats.movelist,
				only_one_move(programStats.movelist)?
				xtra[programStats.got_fail] : "",
				programStats.nodes, (int)nps, h, m, s );	  
    }
  
  DisplayAnalysisText( buf );
}

void DisplayComment(moveNumber, text)
     int moveNumber;
     char *text;
{
    char title[MSG_SIZ];

    if (moveNumber < 0 || parseList[moveNumber][0] == NULLCHAR) {
	strcpy(title, "Comment");
    } else {
	sprintf(title, "Comment on %d.%s%s", moveNumber / 2 + 1,
		WhiteOnMove(moveNumber) ? " " : ".. ",
		parseList[moveNumber]);
    }

    CommentPopUp(title, text);
}

/* This routine sends a ^C interrupt to gnuchess, to awaken it if it
 * might be busy thinking on our time.  It can be omitted if your
 * gnuchess is configured to stop thinking immediately on any user
 * input.  However, that gnuchess feature depends on the FIONREAD
 * ioctl, which does not work properly on some flavors of Unix.
 */
void Attention(pr)
     ProcRef pr;
{
#if defined(ATTENTION)
    if (appData.noChessProgram || (pr == NoProc)) return;
    switch (gameMode) {
      case MachinePlaysWhite:
      case MachinePlaysBlack:
      case TwoMachinesPlay:
      case IcsPlayingWhite:
      case IcsPlayingBlack:
	if (forwardMostMove > backwardMostMove + 1) {
	    if (pr == firstProgramPR && !firstMaybePondering) return;
	    if (pr == secondProgramPR && !secondMaybePondering) return;
	    if (appData.debugMode)
	      fprintf(debugFP, "Interrupting %s\n",
		      pr == firstProgramPR ? "first" : "second");
	    InterruptChildProcess(pr);
	    if (pr == firstProgramPR)
	      firstMaybePondering = FALSE;
	    else 
	      secondMaybePondering = FALSE;
	}
	break;
      default:
	break;
    }
#endif /*ATTENTION*/
}

void CheckFlags()
{
    if (whiteTimeRemaining <= 0) {
	if (!whiteFlag) {
	    whiteFlag = TRUE;
	    if (appData.icsActive) {
		if (appData.autoCallFlag &&
		    gameMode == IcsPlayingBlack && !blackFlag)
		  SendToICS("flag\n");
	    } else {
		if (blackFlag)
		  DisplayTitle("Both flags fell");
		else
		  DisplayTitle("White's flag fell");
	    }
	}
    }
    if (blackTimeRemaining <= 0) {
	if (!blackFlag) {
	    blackFlag = TRUE;
	    if (appData.icsActive) {
		if (appData.autoCallFlag &&
		    gameMode == IcsPlayingWhite && !whiteFlag)
		  SendToICS("flag\n");
	    } else {
		if (whiteFlag)
		  DisplayTitle("Both flags fell");
		else
		  DisplayTitle("Black's flag fell");
	    }
	}
    }
}

void CheckTimeControl()
{
    if (!appData.clockMode || appData.icsActive ||
	gameMode == PlayFromGameFile || forwardMostMove == 0) return;

    if (appData.timeIncrement >= 0) {
	if (WhiteOnMove(forwardMostMove)) {
	    blackTimeRemaining += appData.timeIncrement * 1000;
	} else {
	    whiteTimeRemaining += appData.timeIncrement * 1000;
	}
	return;
    }
    /*
     * add time to clocks when time control is achieved
     */
    if ((forwardMostMove % (appData.movesPerSession * 2)) == 0) {
	whiteTimeRemaining += timeControl;
	blackTimeRemaining += timeControl;
    }
}

void DisplayBothClocks()
{
    DisplayWhiteClock(whiteTimeRemaining, WhiteOnMove(currentMove));
    DisplayBlackClock(blackTimeRemaining, !WhiteOnMove(currentMove));
}


/* Timekeeping seems to be a portability nightmare.  I think everyone
   has ftime(), but I'm really not sure, so I'm including some ifdefs
   to use other calls if you don't.  Clocks will be less accurate if
   you have neither ftime nor gettimeofday.
*/

/* Get the current time as a TimeMark */
void GetTimeMark(tm)
     TimeMark *tm;
{
#if HAVE_GETTIMEOFDAY

    struct timeval timeVal;
    struct timezone timeZone;

    gettimeofday(&timeVal, &timeZone);
    tm->sec = (long) timeVal.tv_sec; 
    tm->ms = (int) (timeVal.tv_usec / 1000L);

#else /*!HAVE_GETTIMEOFDAY*/
#if HAVE_FTIME

#include <sys/timeb.h>
    struct timeb timeB;

    ftime(&timeB);
    tm->sec = (long) timeB.time;
    tm->ms = (int) timeB.millitm;

#else /*!HAVE_FTIME && !HAVE_GETTIMEOFDAY*/
    tm->sec = (long) time(NULL);
    tm->ms = 0;
#endif
#endif
}

/* Return the difference in milliseconds between two
   time marks.  We assume the difference will fit in a long!
*/
long SubtractTimeMarks(tm2, tm1)
     TimeMark *tm2, *tm1;
{
    return 1000L*(tm2->sec - tm1->sec) +
           (long) (tm2->ms - tm1->ms);
}


/*
 * Code to manage the game clocks.
 *
 * In tournament play, black starts the clock and then white makes a move.
 * We give the human user a slight advantage if he is playing white---the
 * clocks don't run until he makes his first move, so it takes zero time.
 * Also, we don't account for network lag, so we could get out of sync
 * with GNU Chess's clock -- but then, referees are always right.  
 */

static TimeMark tickStartTM;
static long intendedTickLength;

long NextTickLength(timeRemaining)
     long timeRemaining;
{
    long nominalTickLength, nextTickLength;

    if (timeRemaining > 0L && timeRemaining <= 1000L)
      nominalTickLength = 100L;
    else
      nominalTickLength = 1000L;
    nextTickLength = timeRemaining % nominalTickLength;
    if (nextTickLength <= 0) nextTickLength += nominalTickLength;

    return nextTickLength;
}

/* Stop clocks and reset to a fresh time control */
void ResetClocks() 
{
    (void) StopClockTimer();
    whiteTimeRemaining = timeControl;
    blackTimeRemaining = timeControl;
    if (whiteFlag || blackFlag) {
	DisplayTitle("");
	whiteFlag = blackFlag = FALSE;
    }
    DisplayBothClocks();
}
	
#define FUDGE 25 /* 25ms = 1/40 sec; should be plenty even for 50 Hz clocks */

/* Decrement running clock by amount of time that has passed */
void DecrementClocks()
{
    long timeRemaining;
    long lastTickLength, fudge;
    TimeMark now;

    if (!appData.clockMode) return;
	if (gameMode==AnalyzeMode || gameMode == AnalyzeFile ) return;
	
    GetTimeMark(&now);

    lastTickLength = SubtractTimeMarks(&now, &tickStartTM);

    /* Fudge if we woke up a little too soon */
    fudge = intendedTickLength - lastTickLength;
    if (fudge < 0 || fudge > FUDGE) fudge = 0;

    if (WhiteOnMove(forwardMostMove)) {
	timeRemaining = whiteTimeRemaining -= lastTickLength;
	DisplayWhiteClock(whiteTimeRemaining - fudge,
			  WhiteOnMove(currentMove));
    } else {
	timeRemaining = blackTimeRemaining -= lastTickLength;
	DisplayBlackClock(blackTimeRemaining - fudge,
			  !WhiteOnMove(currentMove));
    }
	
    CheckFlags();
	
    tickStartTM = now;
    intendedTickLength = NextTickLength(timeRemaining - fudge) + fudge;
    StartClockTimer(intendedTickLength);
}

	
/* A player has just moved, so stop the previously running
   clock and (if in clock mode) start the other one.
   We redisplay both clocks in case we're in ICS mode, because
   ICS gives us an update to both clocks after every move.
*/
void SwitchClocks()
{
    long lastTickLength;
    TimeMark now;

    GetTimeMark(&now);

    if (StopClockTimer() && appData.clockMode) {
	lastTickLength = SubtractTimeMarks(&now, &tickStartTM);
	if (WhiteOnMove(forwardMostMove)) {
	    whiteTimeRemaining -= lastTickLength;
	} else {
	    blackTimeRemaining -= lastTickLength;
	}
	CheckFlags();
    }
    CheckTimeControl();
    if (!((gameMode == PlayFromGameFile) &&
	  (matchMode || (appData.timeDelay == 0 && !pausing)))) {
	DisplayBothClocks();
    }

    if (!appData.clockMode) return;

    switch (gameMode) {
      case MachinePlaysBlack:
      case MachinePlaysWhite:
      case BeginningOfGame:
	if (pausing) return;
	break;

      case EditGame:
      case PlayFromGameFile:
      case IcsExamining:
	return;

      default:
	break;
    }

    tickStartTM = now;
    intendedTickLength = NextTickLength(WhiteOnMove(forwardMostMove) ?
      whiteTimeRemaining : blackTimeRemaining);
    StartClockTimer(intendedTickLength);
}
	

/* Stop both clocks */
void StopClocks()
{	
    long lastTickLength;
    TimeMark now;

    if (!StopClockTimer()) return;
    if (!appData.clockMode) return;

    GetTimeMark(&now);

    lastTickLength = SubtractTimeMarks(&now, &tickStartTM);
    if (WhiteOnMove(forwardMostMove)) {
	whiteTimeRemaining -= lastTickLength;
	DisplayWhiteClock(whiteTimeRemaining, WhiteOnMove(currentMove));
    } else {
	blackTimeRemaining -= lastTickLength;
	DisplayBlackClock(blackTimeRemaining, !WhiteOnMove(currentMove));
    }
    CheckFlags();
}
	
/* Start clock of player on move.  Time may have been reset, so
   if clock is already running, stop and restart it. */
void StartClocks()
{
    (void) StopClockTimer(); /* in case it was running already */
    DisplayBothClocks();
    CheckFlags();

    if (!appData.clockMode) return;
	if (gameMode == AnalyzeMode || gameMode == AnalyzeFile ) return;

    GetTimeMark(&tickStartTM);
    intendedTickLength = NextTickLength(WhiteOnMove(forwardMostMove) ?
      whiteTimeRemaining : blackTimeRemaining);
    StartClockTimer(intendedTickLength);
}

char *TimeString(ms)
     long ms;
{
    long second, minute, hour, day;
    char *sign = "";
    static char buf[32];
    
    if (ms > 0 && ms <= 900) {
	/* convert milliseconds to tenths, rounding up */
	sprintf(buf, " 0.%1ld ", (ms+99L)/100L);
	return buf;
    }

    /* convert milliseconds to seconds, rounding up */
    /* use floating point to avoid strangeness of integer division
       with negative dividends on many machines */
    second = (long) floor(((double) (ms + 999L)) / 1000.0);

    if (second < 0) {
	sign = "-";
	second = -second;
    }
    
    day = second / (60 * 60 * 24);
    second = second % (60 * 60 * 24);
    hour = second / (60 * 60);
    second = second % (60 * 60);
    minute = second / 60;
    second = second % 60;
    
    if (day > 0)
      sprintf(buf, " %s%ld:%02ld:%02ld:%02ld ",
	      sign, day, hour, minute, second);
    else if (hour > 0)
      sprintf(buf, " %s%ld:%02ld:%02ld ", sign, hour, minute, second);
    else
      sprintf(buf, " %s%2ld:%02ld ", sign, minute, second);
    
    return buf;
}


/*
 * This is necessary because some C libraries aren't ANSI C compliant yet.
 */
char *StrStr(string, match)
     char *string, *match;
{
    int i, length;
    
    length = strlen(match);
    
    for (i = strlen(string) - length; i >= 0; i--, string++)
      if (!strncmp(match, string, length))
	return string;
    
    return NULL;
}

#ifndef _amigados
int StrCaseCmp(s1, s2)
     char *s1, *s2;
{
    char c1, c2;
    
    for (;;) {
	c1 = ToLower(*s1++);
	c2 = ToLower(*s2++);
	if (c1 > c2) return 1;
	if (c1 < c2) return -1;
	if (c1 == NULLCHAR) return 0;
    }
}


int ToLower(c)
     int c;
{
    return isupper(c) ? tolower(c) : c;
}


int ToUpper(c)
     int c;
{
    return islower(c) ? toupper(c) : c;
}
#endif /* !_amigados	*/

char *StrSave(s)
     char *s;
{
    char *ret;

    if ((ret = (char *) malloc(strlen(s) + 1))) {
    strcpy(ret, s);
    }
    return ret;
}

char *StrSavePtr(s, savePtr)
    char *s, **savePtr;
{
    if (*savePtr) {
	free(*savePtr);
    }
    if ((*savePtr = (char *) malloc(strlen(s) + 1))) {
	strcpy(*savePtr, s);
    }
    return(*savePtr);
}

char *PGNDate()
{
    time_t clock;
    struct tm *tm;
    char buf[MSG_SIZ];

    clock = time((time_t *)NULL);
    tm = localtime(&clock);
    sprintf(buf, "%04d.%02d.%02d",
	    tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday);
    return StrSave(buf);
}


char *PositionToFEN(move)
     int move;
{
    int i, j, fromX, fromY, toX, toY;
    int whiteToPlay;
    char buf[128];
    char *p, *q;
    int emptycount;

    whiteToPlay = (gameMode == EditPosition) ?
      !blackPlaysFirst : (move % 2 == 0);
    p = buf;

    /* Piece placement data */
    for (i = BOARD_SIZE - 1; i >= 0; i--) {
	emptycount = 0;
	for (j = 0; j < BOARD_SIZE; j++) {
	    if (boards[move][i][j] == EmptySquare) {
		emptycount++;
	    } else {
		if (emptycount > 0) {
		    *p++ = '0' + emptycount;
		    emptycount = 0;
		}
		*p++ = PieceToChar(boards[move][i][j]);
	    }
	}
	if (emptycount > 0) {
	    *p++ = '0' + emptycount;
	    emptycount = 0;
	}
	*p++ = '/';
    }
    *(p - 1) = ' ';

    /* Active color */
    *p++ = whiteToPlay ? 'w' : 'b';
    *p++ = ' ';

    /* !!We don't keep track of castling availability, so fake it */
    q = p;
    if (boards[move][0][4] == WhiteKing) {
	if (boards[move][0][7] == WhiteRook) *p++ = 'K';
	if (boards[move][0][0] == WhiteRook) *p++ = 'Q';
    }
    if (boards[move][7][4] == BlackKing) {
	if (boards[move][7][7] == BlackRook) *p++ = 'k';
	if (boards[move][7][0] == BlackRook) *p++ = 'q';
    }	    
    if (q == p) *p++ = '-';
    *p++ = ' ';

    /* En passant target square */
    if (move > backwardMostMove) {
	fromX = moveList[move - 1][0] - 'a';
	fromY = moveList[move - 1][1] - '1';
	toX = moveList[move - 1][2] - 'a';
	toY = moveList[move - 1][3] - '1';
	if (fromY == (whiteToPlay ? 6 : 1) &&
	    toY == (whiteToPlay ? 4 : 3) &&
	    boards[move][toY][toX] == (whiteToPlay ? BlackPawn : WhitePawn) &&
	    fromX == toX) {
	    /* 2-square pawn move just happened */
	    *p++ = toX + 'a';
	    *p++ = whiteToPlay ? '6' : '3';
	} else {
	    *p++ = '-';
	}
    } else {
	*p++ = '-';
    }

    /* !!We don't keep track of halfmove clock for 50-move rule */
    strcpy(p, " 0 ");
    p += 3;

    /* Fullmove number */
    sprintf(p, "%d", (move / 2) + 1);
    
    return StrSave(buf);
}

Boolean ParseFEN(board, blackPlaysFirst, fen)
     Board board;
     int *blackPlaysFirst;
     char *fen;
{
    int i, j;
    char *p;
    int emptycount;

    p = fen;

    /* Piece placement data */
    for (i = BOARD_SIZE - 1; i >= 0; i--) {
	j = 0;
	while (j < BOARD_SIZE) {
	    if (isdigit(*p)) {
		emptycount = *p++ - '0';
		if (j + emptycount > BOARD_SIZE) return FALSE;
		while (emptycount--) board[i][j++] = EmptySquare;
	    } else {
		board[i][j++] = CharToPiece(*p++);
	    }
	}
	if (*p != '/' && *p != ' ') return FALSE;
	p++;
    }

    /* Active color */
    switch (*p) {
      case 'w':
	*blackPlaysFirst = FALSE;
	break;
      case 'b': 
	*blackPlaysFirst = TRUE;
	break;
      default:
	return FALSE;
    }
	
    /* !!We ignore the rest of the FEN notation */
    return TRUE;
}
