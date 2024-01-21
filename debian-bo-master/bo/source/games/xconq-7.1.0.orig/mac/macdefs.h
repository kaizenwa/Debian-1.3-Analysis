/* Definitions of all the resources for the Mac interface to Xconq.
   Copyright (C) 1992, 1993, 1994, 1995, 1996 Stanley T. Shebs.

Xconq is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.  See the file COPYING.  */

#define XconqSignature 'XCNQ'

/* The menu bar. */

#define mbMain 128

/* Menus. */

#define mApple 128
#define mFile 129
#define mEdit 130
#define mFind 131
#define mPlay 132
#define mSide 133
#define mWindows 134
#define mViewMap 135

#define mViewList 235
#define mViewCloseup 335

#define mSides 200
#define mViewWeather 201
#define mMaterialTypes 202
#define mViewFontSizes 203
#define mTerrainTypes 204
#define mUnitTypes 205
#define mAITypes 206
#define mAgreements 208
#define mMagnifications 209

#define mFeatures 212
#define mOptTerrainTypes 213
#define mViewAngles 214

/* Menu items. */

#define miAppleAbout 1
#define miAppleHelp 2
#define miAppleInstructions 3

#define miFileNew 1
#define miFileOpen 2
/* 3 */
#define miFileConnect 4
/* 5 */
#define miFileSave 6
#define miFileSaveAs 7
/* 8 */
#define miFilePreferences 9
/* 10 */
#define miFilePageSetup 11
#define miFilePrintWindow 12
/* 13 */
#define miFileResign 14
#define miFileQuit 15

#define miEditUndo 1
/* 2 */
#define miEditCut 3
#define miEditCopy 4
#define miEditPaste 5
#define miEditClear 6
/* 7 */
#define miEditSelectAll 8
/* 9 */
#define miEditDesign 10

#define miFindPrevious 1
#define miFindNext 2
/* 3 */
#define miFindLocation 4
#define miFindUnitByName 5
/* 6 */
#define miFindSelected 7

#define miPlayCloseup 1
/* 2 */
#define miPlayMove 3
#define miPlayReturn 4
#define miPlaySetFormation 5
/* 6 */
#define miPlayWake 7
#define miPlaySleep 8
#define miPlayReserve 9
#define miPlayDelay 10
/* 11 */
#define miPlayBuild 12
#define miPlayRepair 13
/* 14 */
#define miPlayAttack 15
#define miPlayOverrun 16
#define miPlayFire 17
#define miPlayFireInto 18
#define miPlayDetonate 19
/* 20 */
#define miPlayTake 21
#define miPlayDrop 22
#define miPlayGive 23
#define miPlayDetach 24
#define miPlayDisband 25
/* 26 */
#define miPlayAddTerrain 27
#define miPlayRemoveTerrain 28
/* 29 */
#define miPlayRename 30

#define miSideCloseup 1
#define miSideFinishedTurn 2
/* 3 */
#define miSideMoveOnClick 4
#define miSideAutoSelect 5
#define miSideAutoFinish 6
/* 7 */
#define miSideSound 8
/* 9 */
/* 10 is AI Type submenu */
/* 11 */
#define miSideDoctrine 12
/* 13 */
#define miSideRename 14

#define miWindowsGame 1
#define miWindowsNotices 2
#define miWindowsHistory 3
#define miWindowsConstruction 4
#define miWindowsCommand 5
#define miWindowsAgreements 6
#define miWindowsScores 7
/* 8 */
#define miWindowsNewMap 9
#define miWindowsNewList 10
/* 11 */
#define miWindowsWorldMap 12
/* 13 */
#define miWindowsFirst 14

#define miViewCloser 1
#define miViewFarther 2
#define miViewMags 3
#define miViewAngle 4
/* 5 */
#define miViewNames 6
#define miViewElevations 7
#define miViewPeople 8
#define miViewWeather 9
#define miViewMaterials 10
#define miViewTerrain 11
#define miViewDaylight 12
#define miViewCoverage 13
/* 14 */
#define miViewGrid 15
#define miViewTopline 16
#define miViewTopunit 17
#define miViewOtherMaps 18
#define miViewFontSize 19

#define miViewByType 1
#define miViewByName 2
#define miViewBySide 3
#define miViewByActOrder 4
#define miViewByLocation 5
/* 6 */
#define miViewWithTransport 7
#define miViewWithCommander 8
/* 9 */
#define miViewIconSize 10

#define miAngle15 1
#define miAngle30 2
#define miAngle90 3

#define miWeatherTemperature 1
#define miWeatherWinds 2
#define miWeatherClouds 3
#define miWeatherStorms 4

/* Dialogs. */

#define dSplash 128

#define diSplashNew 1
#define diSplashOpen 2
#define diSplashConnect 3
#define diSplashQuit 4
#define diSplashVersion 5
#define diSplashCopyright 6
#define diSplashPicture 7

#define dNewGame 129

#define diNewGameOK 1
#define diNewGameCancel 2
#define diNewGameList 3
#define diNewGameBlurb 4
#define diNewGamePicture 5
/* 6 is a fixed pict */

#define dPlayerSetup 130

#define diPlayerSetupOK 1
#define diPlayerSetupCancel 2
#define diPlayerSetupList 3
#define diPlayerSetupAdd 4
#define diPlayerSetupRemove 5
#define diPlayerSetupAdvantage 6
#define diPlayerSetupRename 7
#define diPlayerSetupAI 8
#define diPlayerSetupRemote 9
#define diPlayerSetupExchange 10

#define dVariants 131

#define diVariantsOK 1
#define diVariantsCancel 2
#define diVariantsText 3
#define diVariantsWorldSeen 4
#define diVariantsSeeAll 5
#define diVariantsSequential 6
#define diVariantsFirstCheckBox 7
/* and next 6 also */
#define diVariantsFirstSlider 14
/* and next 2 also */
#define diVariantsWorldSize 17
#define diVariantsRealTime 18
#define diVariantsMoreVariants 19
#define diVariantsHelp 20

#define dProgress 132

#define diProgressText 1
#define diProgressBar 2
#define diProgressCancel 3

#define dAbout 133

#define diAboutOK 1
#define diAboutVersion 2
#define diAboutCopyright 3
#define diAboutPicture 4

#define dWorldShape 134

#define diWorldShapeOK 1
#define diWorldShapeCancel 2
#define diWorldShapePicture 3
#define diWorldShapeCircumference 4
#define diWorldShapeWidth 5 
#define diWorldShapeHeight 6
#define diWorldShapeLatitude 7
#define diWorldShapeLongitude 8
#define diWorldShapeIcon 17

#define dRealTime 135

#define diRealTimeOK 1
#define diRealTimeCancel 2
#define diRealTimeForGame 3
#define diRealTimePerSide 4
#define diRealTimePerTurn 5

#define dMoreVariants 136

#define diMoreVariantsOK 1
#define diMoreVariantsCancel 2

#define dPreferences 150

#define diPrefsOK 1
#define diPrefsCancel 2
#define diPrefsGrid 3
#define diPrefsNames 4
#define diPrefsCheckpoint 5
#define diPrefsEvery 6
#define diPrefsInterval 7
#define diPrefsTurns 8
#define diPrefsStatistics 9

#define dSideRename 201

#define diSideRenameOK 1
#define diSideRenameCancel 2
#define diSideRenameRandom 3
#define diSideRenameName 4
#define diSideRenameFullName 5
#define diSideRenameAcronym 6
#define diSideRenameNoun 7
#define diSideRenamePluralNoun 8
#define diSideRenameAdjective 9
#define diSideRenameEmblemName 10
#define diSideRenameColorScheme 11

#define dRename 202

#define diRenameOK 1
#define diRenameCancel 2
#define diRenameRandom 3
#define diRenameName 4
#define diRenameText 5

#define dFeatureRename 203

#define diFeatureRenameOK 1
#define diFeatureRenameCancel 2
#define diFeatureRenameType 3
#define diFeatureRenameName 4

/*
#define dCommand 204

#define diCommandOK 1
#define diCommandCancel 2
#define diCommandText 3
*/

#define dMessage 205

#define diMessageOK 1
#define diMessageCancel 2
#define diMessageText 3

#define dMessageReceive 206

#define diMessageReceiveOK 1
#define diMessageReceiveText 2

#define dBuildPlan 301

#define diBuildPlanOK 1
#define diBuildPlanCancel 2
#define diBuildPlanUnit 3
#define diBuildPlanCurNext 4
#define diBuildPlanNumber 5
#define diBuildPlanCurType 6

#define dMultiBuild 302

#define diMultiBuildBuild 1
#define diMultiBuildUnitList 2
#define diMultiBuildTypeList 3
#define diMultiBuildRunLength 4
/* 5 */
#define diMultiBuildCurType 6
#define diMultiBuildNextType 7

#define dDesignerPalette 500

#define diDesignerPaletteTPal 1
#define diDesignerPaletteUPal 2
#define diDesignerPaletteExplanation 3
#define diDesignerPaletteSide 4

#define dDesignerSave 501

#define diDesignerSaveOK 1
#define diDesignerSaveCancel 2
#define diDesignerSaveName 3
#define diDesignerSaveModule 4
#define diDesignerSaveTypes 5
#define diDesignerSaveTables 6
#define diDesignerSaveGlobals 7
#define diDesignerSaveWorld 8
#define diDesignerSaveAreas 9
#define diDesignerSaveAreaTerrain 26
#define diDesignerSaveAreaMisc 27
#define diDesignerSaveAreaWeather 28
#define diDesignerSaveAreaMaterial 29
#define diDesignerSaveAreas 9
#define diDesignerSaveAreas 9
#define diDesignerSaveSides 10
#define diDesignerSavePlayers 11
#define diDesignerSaveUnits 12
#define diDesignerSaveScoring 13
#define diDesignerSaveHistory 14
#define diDesignerSaveReshape 15
#define diDesignerSaveCompress 16
#define diDesignerSaveSideNames 17
#define diDesignerSaveSideProps 18
#define diDesignerSaveSideViews 19
#define diDesignerSaveUnitProps 20
#define diDesignerSaveUnitMoves 21
#define diDesignerSaveUnitPlans 22
#define diDesignerSaveDoctrine 25
#define diDesignerSaveUnitIds 30

#define dDesignerReshape 502

#define diDesignerReshapeOK 1
#define diDesignerReshapeCancel 2
#define diDesignerReshapeOrigWidth 3
#define diDesignerReshapeOrigHeight 4
#define diDesignerReshapeOrigWorld 5
#define diDesignerReshapeOrigSubWidth 6
#define diDesignerReshapeOrigSubHeight 7
#define diDesignerReshapeOrigSubX 8
#define diDesignerReshapeOrigSubY 9
#define diDesignerReshapeOutputSubWidth 10
#define diDesignerReshapeOutputSubHeight 11
#define diDesignerReshapeOutputSubX 12
#define diDesignerReshapeOutputSubY 13
#define diDesignerReshapeOutputWidth 14
#define diDesignerReshapeOutputHeight 15
#define diDesignerReshapeOutputWorld 16
#define diDesignerReshapeFillTerrain 17

#define dInstructions 600

#define diInstructionsTitle 1
#define diInstructionsHelp 2
#define diInstructionsText 3

#define dUnitTypeDesc 601

#define diUnitTypeDescIcon 1
#define diUnitTypeDescName 2
#define diUnitTypeDescHelp 3
#define diUnitTypeDescACP 5
#define diUnitTypeDescMP 7
#define diUnitTypeDescAvail 8
#define diUnitTypeDescNotes 9

#define dMaterialTypeDesc 602

#define diMaterialTypeDescIcon 1
#define diMaterialTypeDescName 2
#define diMaterialTypeDescHelp 3
#define diMaterialTypeDescPeople 5
#define diMaterialTypeDescAvail 7
#define diMaterialTypeDescNotes 8

#define dTerrainTypeDesc 603

#define diTerrainTypeDescIcon 1
#define diTerrainTypeDescName 2
#define diTerrainTypeDescHelp 3
#define diTerrainTypeDescElevMin 5
#define diTerrainTypeDescElevMax 7
#define diTerrainTypeDescNotes 8
#define diTerrainTypeDescTempMin 10
#define diTerrainTypeDescTempMax 12
#define diTerrainTypeDescCapacity 14
#define diTerrainTypeDescAvail 15

#define dGameModuleDesc 604

#define dWinGame 900

#define diWinGameQuit 1
#define diWinGameContinue 2

#define dLoseGame 901

#define diLoseGameQuit 1
#define diLoseGameContinue 2

#define dGameOver 902

#define diGameOverQuit 1
#define diGameOverContinue 2

/* Alerts. */

#define aInitWarning 1000

#define aiInitWarningOK 1
#define aiInitWarningText 4

#define aInitError 1001

#define aiInitErrorOK 1
#define aiInitErrorText 3

#define aRunWarning 1002

#define aiRunWarningOK 1
#define aiRunWarningText 4

#define aRunError 1003

#define aiRunErrorOK 1
#define aiRunErrorText 3

#define aImagesMissing 1004

#define aQuitGame 2000

#define aiQuitGameOK 1
#define aiQuitGameCancel 2
#define aiQuitGameDontSave 3

#define aResignGame 2001

#define aiResignGameOK 1
#define aiResignGameCancel 2
#define aiResignGameWillingToDraw 3
#define aiResignGameText 4

#define aOutOfGame 2002

#define aConfirmResign 2004

#define aiConfirmResignResign 1
#define aiConfirmResignCancel 2

#define aConfirmDesign 3001

#define aiConfirmDesignOK 1
#define aiConfirmDesignCancel 2

#define aDesignerQuitGame 3002

#define aiDesignerQuitGameOK 1
#define aiDesignerQuitGameCancel 2
#define aiDesignerQuitGameDontSave 3

/* Generic windows. */

#define wGame 128
#define wDesign 129
#define wMap 130
#define wList 131
#define wUnitCloseup 132
#define wHistory 133
#define wConstruction 134
#define wHelp 135
#define wNotice 136
#define wCommand 137
#define wScores 138

/* Controls. */

#define cConstructButton 128
#define cResearchButton 129
#define cRunLengthPopup 210

#define cTopicsButton 131
#define cHelpButton 132
#define cPrevButton 133
#define cNextButton 134
#define cBackButton 135

#define cFeatureAddButton 401
#define cFeatureRemoveButton 402
#define cFeatureEditButton 403

#define cCommandDoButton 501

/* Cursors. */

#define cOpenCross 154
#define cAllMove 155
#define cNoMove 156
#define cGrayArrow 157

#define cParens1 128
#define NUMcParens 6
#define cSynth1 139
#define NUMcSynth 8
#define cMove1 148
#define NUMcMoves 6

#define cCell 201
#define cUnit 202
#define cPeople 203
#define cMaterial 204
#define cFeature 205
#define cElevation 206
#define cTemperature 207
#define cClouds 208
#define cWinds 209

#define cBord 291
#define cConn 292
#define cCoat 293

/* Patterns. */

#define pMarchingAnts 128

/* Pictures. */

#define pEmptyPict 128
#define pMapControlsBL 129
#define pMapControlsTL 130
#define pUpDownPicture 131

#define pSplashBW 2000
#define pSplashColor 2001
#define pNewGameDecor 2002

/* Strings. */

#define sFilenames 128

#define siPreferences 1
#define siLibFolder 2
#define siLibMacFolder 3
#define siResources 4
#define siImages 5
#define siSounds 6
#define siImfDir 7
#define siNews 8
#define siSavedGame 9
#define siCheckpoint 10
#define siErrorSave 11
#define siStatistics 12

/* Small icons. */

#define sicnWinds0 2100

#define sicnMiss 2200
#define sicnHit  2201
#define sicnKill 2202
