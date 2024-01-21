/*
 * frontend.h -- Interface exported by all XBoard front ends
 * $Id: frontend.h,v 1.18 1996/11/17 05:57:41 mann Exp $
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

#ifndef _FRONTEND
#define _FRONTEND

#include <stdio.h>

void ModeHighlight P((void));
void SetICSMode P((void));
void SetGNUMode P((void));
void SetNCPMode P((void));
void SetCmailMode P((void));
void DisplayTitle P((String title));
void DisplayMessage P((String message, String extMessage));
void DisplayError P((String message, int error));
void DisplayFatalError P((String message, int error, int status));
void DisplayInformation P((String message));
void DrawPosition P((int fullRedraw, Board board));
void ResetFrontEnd P((void));
void CommentPopUp P((String title, String comment));
void CommentPopDown P((void));
void EditCommentPopUp P((int index, String title, String text));

void RingBell P((void));
void EchoOn P((void));
void EchoOff P((void));
void Raw P((void));

char *UserName P((void));
char *HostName P((void));

int ClockTimerRunning P((void));
int StopClockTimer P((void));
void StartClockTimer P((long millisec));
void DisplayWhiteClock P((long timeRemaining, int highlight));
void DisplayBlackClock P((long timeRemaining, int highlight));

int LoadGameTimerRunning P((void));
int StopLoadGameTimer P((void));
void StartLoadGameTimer P((long millisec));
void AutoSaveGame P((void));

typedef VOIDSTAR ProcRef;
#define NoProc ((ProcRef) 0)
int StartChildProcess P((char *cmdLine, ProcRef *pr));
void DestroyChildProcess P((ProcRef pr));
void InterruptChildProcess P((ProcRef pr));

int OpenTelnet P((char *host, char *port, ProcRef *pr));
int OpenTCP P((char *host, char *port, ProcRef *pr));
int OpenCommPort P((char *name, ProcRef *pr));
int OpenLoopback P((ProcRef *pr));
int OpenRcmd P((char *host, char *user, char *cmd, ProcRef *pr));

typedef VOIDSTAR InputSourceRef;
typedef void (*InputCallback) P((InputSourceRef isr, char *buf, int count, int error));
InputSourceRef AddInputSource P((ProcRef pr, int lineByLine, InputCallback func));
void RemoveInputSource P((InputSourceRef isr));

int OutputToProcess P((ProcRef pr, char *message, int count, int *outError));
int OutputToProcessDelayed P((ProcRef pr, char *message, int count,
			      int *outError, long msdelay));

void CmailSigHandlerCallBack P((InputSourceRef isr, char *buf, int count, int error));

extern ProcRef cmailPR;

void GameListPopUp P((FILE *fp, char *filename));
void GameListPopDown P((void));
void GameListHighlight P((int index));
void GameListDestroy P((void));

void EditTagsPopUp P((char *tags));
void TagsPopUp P((char *tags, char *msg));
void TagsPopDown P((void));

void ICSInitScript P((void));
void StartAnalysisClock P((void));
void AnalysisPopUp P((char *title, char *label));
void AnalysisPopDown P((void));

#endif
