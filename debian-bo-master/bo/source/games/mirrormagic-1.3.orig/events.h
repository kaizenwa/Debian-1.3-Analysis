/***********************************************************
*  Mirror Magic II -- McDuffins Revenge                    *
*----------------------------------------------------------*
*  ©1994 Artsoft Development                               *
*        Holger Schemel                                    *
*        33659 Bielefeld-Senne                             *
*        Telefon: (0521) 493245                            *
*        eMail: aeglos@valinor.ms.sub.org                  *
*               aeglos@uni-paderborn.de                    *
*               q99492@pbhrzx.uni-paderborn.de             *
*----------------------------------------------------------*
*  events.h                                                *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#ifndef EVENTS_H
#define EVENTS_H

#include "main.h"

void EventLoop(void);
void ClearEventQueue(void);
void SleepWhileUnmapped(void);

void HandleExposeEvent(XExposeEvent *);
void HandleButtonEvent(XButtonEvent *);
void HandleMotionEvent(XMotionEvent *);
void HandleKeyEvent(XKeyEvent *);
void HandleNoXEvent(void);
void HandleButton(int, int, int);

#endif
