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
*  game.h                                                  *
*                                                          *
*  Letzte Aenderung: 29.09.1994                            *
***********************************************************/

#ifndef GAME_H
#define GAME_H

#include "main.h"

BOOL CreateNewLevelFile(void);
BOOL CreateNewScoreFile(void);
BOOL CreateNewNamesFile(void);
void LoadLevel(int);
void LoadScore(int);
BOOL ConvertNames(void);
void LoadPlayerInfo(void);
void SaveLevel(int);
void SaveScore(int);
void SavePlayerInfo(void);
void GetPlayerConfig(void);
void DrawLevel(void);
void InitGame(void);
void NextRay(int, int);
void NextDam(int, int);
void ScanLaser(void);
void DrawLaser(int, int);
BOOL Objekt(void);
BOOL Edge(void);
BOOL Polarizer(void);
BOOL Block(void);
BOOL LaserGun(void);
BOOL Receiver(void);
BOOL Walls(void);
BOOL Walls2(void);
void Bang(int, int, int);
void ClickElement(int, int, int);
void RotateMirror(int);
BOOL ObjHit(int, int, int);
void DeletePacMan(int, int);
void GameWon(void);
BOOL NewHiScore(void);
void ColorCycling(void);
int GameActions(int, int, int);
void MovePacMen(void);

#endif





