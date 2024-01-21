/*
 * backend.h -- Interface exported by XBoard back end
 * $Id: backend.h,v 1.27 1996/11/17 05:57:41 mann Exp $
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
#include "lists.h"

extern GameMode gameMode;
extern int pausing, cmailMsgLoaded, flipView, ics_bughouse;
extern FILE *fromUserFP, *toUserFP, *debugFP;

void TimeDelay P((long ms));
void SendMultiLineToICS P(( char *text ));
void AnalysisPeriodicEvent P((int force));
void SetWhiteToPlayEvent P((void));
void SetBlackToPlayEvent P((void));
void InitBackEnd1 P((void));
void InitBackEnd2 P((void));
int IsPromotion P((int fromX, int fromY, int toX, int toY));
int OKToStartUserMove P((int x, int y));
void Reset P((int redraw));
void ResetGameEvent P((void));
int LoadGame P((FILE *f, int n, char *title, int useList));
int CmailLoadGame P((FILE *f, int n, char *title, int useList));
int ReloadGame P((int offset));
int SaveGame P((FILE *f, int dummy, char *dummy2));
int LoadPosition P((FILE *f, int n, char *title));
int SavePosition P((FILE *f, int dummy, char *dummy2));
void EditPositionEvent P((void));
void FlipViewEvent P((void));
void MachineWhiteEvent P((void));
void MachineBlackEvent P((void));
void TwoMachinesEvent P((void));
void EditGameEvent P((void));
void IcsClientEvent P((void));
void ForwardEvent P((void));
void BackwardEvent P((void));
void ToEndEvent P((void));
void ToStartEvent P((void));
void RevertEvent P((void));
void RetractMoveEvent P((void));
void MoveNowEvent P((void));
void TruncateGameEvent P((void));
void PauseEvent P((void));
void CallFlagEvent P((void));
void AcceptEvent P((void));
void DeclineEvent P((void));
void DrawEvent P((void));
void AbortEvent P((void));
void AdjournEvent P((void));
void ResignEvent P((void));
void StopObservingEvent P((void));
void StopExaminingEvent P((void));
void ShowThinkingEvent P((int newState));
void PeriodicUpdatesEvent P((int newState));
void HintEvent P((void));
void BookEvent P((void));
void AboutGameEvent P((void));
void ExitEvent P((int status));
char *DefaultFileName P((char *));
void UserMoveEvent P((int fromX, int fromY, int toX, int toY, int promoChar));
void DecrementClocks P((void));
char *TimeString P((long millisec));
void LoadGameLoop P((void));
void DisplayBothClocks P((void));
void EditPositionMenuEvent P((ChessSquare selection, int x, int y));
void DropMenuEvent P((ChessSquare selection, int x, int y));
int ParseTimeControl P((char *tc));
void ProcessICSInitScript P((FILE * f));
void EditCommentEvent P((void));
void ReplaceComment P((int index, char *text));
int ReplaceTags P((char *tags, GameInfo *gi));/* returns nonzero on error */
void AppendComment P((int index, char *text));
void ReloadCmailMsgEvent P((int unregister));
void MailMoveEvent P((void));
void EditTagsEvent P((void));
void GetMoveListEvent P((void));
void ExitAnalyzeMode P((void));
void AnalyzeModeEvent P((void));
void AnalyzeFileEvent P((void));

char *StrStr P((char *string, char *match));
char *StrSave P((char *s));
char *StrSavePtr P((char *s, char **savePtr));

#ifndef _amigados
int StrCaseCmp P((char *s1, char *s2));
int ToLower P((int c));
int ToUpper P((int c));
#else
#define StrCaseCmp Stricmp  /*  Use utility.library functions   */
#include <proto/utility.h>
#endif

extern GameInfo gameInfo;



/* pgntags.c prototypes
 */
char *PGNTags P((GameInfo *));
void PrintPGNTags P((FILE *f, GameInfo *));
int ParsePGNTag P((char *, GameInfo *));
char *PGNResult P((ChessMove result));


/* gamelist.c prototypes
 */
/* A game node in the double linked list of games.
 */
typedef struct _ListGame {
    ListNode node;
    int number;
    unsigned long offset;   /*  Byte offset of game within file.     */
    GameInfo gameInfo;      /*  Note that some entries may be NULL. */
} ListGame;
 
extern List gameList;
void ClearGameInfo P((GameInfo *));
int GameListBuild P((FILE *));
void GameListInitGameInfo P((GameInfo *));
char *GameListLine P((int, GameInfo *));

extern char* StripHighlight P((char *));  /* returns static data */
extern char* StripHighlightAndTitle P((char *));  /* returns static data */
