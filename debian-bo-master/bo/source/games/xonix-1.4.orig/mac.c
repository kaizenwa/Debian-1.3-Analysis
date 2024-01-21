/* Hey emacs, this is -*- Mode: C++; tab-width: 4 -*- 						*/

#ifdef USE_MAC
/*
 * mac.c
 *
 * Macintosh interface for the xonix game
 *
 * mac.c,v 1.18 1995/09/08 09:51:27 j Exp
 */

/*
 * Copyright (c) 1995
 *   Torsten Schoenitz <torsten_schoenitz@bonnie.heep.sax.de>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE DEVELOPERS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE DEVELOPERS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


/* ------------------------------------------------------------------------ */
/* Die #include-Statements binden die Interface-Dateien der benoetigten     */
/* Manager in den Quelltext ein.                                            */
/* ------------------------------------------------------------------------ */

#include "xonix.h"
#include <stdlib.h>         /* Wegen der Zufaelligkeiten                    */
#include <string.h>
#include <Sound.h>
#include <Folders.h>
#include <Packages.h>

/* ------------------------------------------------------------------------ */
/* Resource-IDs.                                                            */
/* ------------------------------------------------------------------------ */

#define BASE_RES_ID         400             /* Basis-Ressource-ID           */

#define rMenuBar            BASE_RES_ID     /* MBAR-Resource.               */
#define rPrefs				BASE_RES_ID		/* Preferences					*/
#define rAboutDialog        BASE_RES_ID     /* "Ueber xonix..."-Dialog      */
#define rEndOfGameDialog	BASE_RES_ID+2	/* Spielende-Dialog				*/
#define rNoRamDialog		BASE_RES_ID+3	/* Speicher reicht nicht		*/
#define rKeyPrefsDialog		BASE_RES_ID+4	/* Tastenbelegung				*/
#define rHallOfFameDialog	BASE_RES_ID+5	/* HighScore-Dialog				*/
#define rCongratulation		BASE_RES_ID+6	/* Gratulation und Namen-Eingabe*/
#define rWinnersList		BASE_RES_ID+7	/* HighScore-Liste				*/
#define rHints1				BASE_RES_ID+8	/* Tips & Hinweise 1			*/
#define rHints2				BASE_RES_ID+9	/* Tips & Hinweise 2			*/
#define rBackgroundInfo		BASE_RES_ID+10	/* Hintergründiges				*/

/* Menu - IDs */

#define mApple              BASE_RES_ID     /* "Apple"-Menue                */
#define mFile               BASE_RES_ID+1   /* "Ablage"-Menue               */
#define mEdit               BASE_RES_ID+2   /* "Bearbeiten"-Menue           */
#define mPrefs				BASE_RES_ID+3	/* "Einstellungen"-Menue		*/
#define mInfos				BASE_RES_ID+4	/* "Infos"-Menue				*/

/* "Apple"-Menuepunkte.  */

#define iAbout              1               /* "Ueber xonix...".            */

/* "Ablage"-Menuepunkte. */

#define iNew                1               /* "Neu".                       */
#define iOpen               2               /* "Oeffnen..".                 */
#define iClose              4               /* "Schliessen".                */
#define iSave               5               /* "Sichern".                   */
#define iSaveAs             6               /* "Sichern unter...".          */
#define iPageSetup          7               /* "Seitenformat...".           */
#define iPrint              9               /* "Drucken...".                */
#define iQuit               11              /* "Beenden".                   */

/* "Bearbeiten"-Menuepunkte. */

#define iUndo               1               /* "Widerrufen".                */
#define iCut                3               /* "Ausschneiden".              */
#define iCopy               4               /* "Kopieren".                  */
#define iPaste              5               /* "Einsetzen".                 */
#define iClear              6               /* "Loeschen".                  */
#define iClipBoard          8               /* "Zwischenablage zeigen".     */

/* "Einstellungen"-Menuepunkte. */

#define iKeyPrefs			1				/* "Tastenbelegung".			*/
#define iSound				2				/* "Sound EIN" bzw. "Sound AUS"	*/
#define iPictures			3				/* "Bilder EIN" / "Bilder AUS"	*/
#define iShowHighScore		4				/* "Zeige HighScore"			*/
#define iResetHighScore		5				/* "Lösche HighScore"			*/

/* "Infos"-Menueunkte. */

#define iHints				1				/* "Tips & Hinweise".			*/
#define iBackgroundInfo		2				/* "Hintergündiges".			*/

/* Dialog-Items */

#define iMyName				3				/* Name zum Eintragen in HOF	*/
#define iWinnerName			4				/* Item-Nr. 1.Platz in HOF		*/

/* PICT-Ressourcen */

#define rScoreLabel			BASE_RES_ID+6	/* Label fuer Punkte-Anzeige	*/
#define rLevelLabel			BASE_RES_ID+7	/* Label fuer Level-Anzeige		*/
#define rPercentLabel		BASE_RES_ID+8	/* Label fuer Fuellstand-Anzeige*/
#define rPlayerLabel		BASE_RES_ID+9	/* Label fuer Player-Anzeige	*/

#define rDigit0				BASE_RES_ID+10	/* Die Digits der 7-Segment-	*/
#define rDigit1				rDigit0+1		/* Anzeige						*/
#define rDigit2				rDigit0+2
#define rDigit3				rDigit0+3
#define rDigit4				rDigit0+4
#define rDigit5				rDigit0+5
#define rDigit6				rDigit0+6
#define rDigit7				rDigit0+7
#define rDigit8				rDigit0+8
#define rDigit9				rDigit0+9

/* ppat-Ressourcen */

#define rEmptyPattern		BASE_RES_ID		/* Pattern für EMPTY			*/
#define rFilledPattern		BASE_RES_ID+1	/* Pattern für FILLED			*/
#define rRunnerPattern		BASE_RES_ID+2	/* Pattern für RUNNER			*/
#define rFlyerPattern		BASE_RES_ID+3	/* Pattern für FLYER			*/
#define rEaterPattern		BASE_RES_ID+4	/* Pattern für EATER			*/
#define rWayPattern			BASE_RES_ID+5	/* Pattern für WAY				*/
#define rStatusPattern		BASE_RES_ID+20	/* Pattern für Statusanzeige	*/

/* CICN-Ressourcen */

#define rRunnerIcon			BASE_RES_ID+2
#define rFlyerIcon			BASE_RES_ID+3
#define rRunnerIconPlain	BASE_RES_ID+4

/* snd-Ressourcen */

#define rEndOfGameSound		BASE_RES_ID
#define rRunnerDieSound		BASE_RES_ID+1

/* STR-Ressourcen */

#define rHighScore			BASE_RES_ID+200
#define rDefaultHighScore	BASE_RES_ID+230

/* STR#-Ressourcen */

#define STRING_LIST			BASE_RES_ID
#define PREF_STRING_INDEX	1

/* KBRS-Ressourcen */
#define rKeyboardPrefs		BASE_RES_ID

#define HILITE_DELAY		8L
#define PICTURES_OFFSET		0
#define HIGHSCORE_ITEMS		10
#define NO_PICTURES_OFFSET	10
#define MAX_PIXEL_DEPTH     8
#define NIL_POINTER         0L
#define NO_FLAGS            0L
#define VISIBLE             TRUE
#define INVISIBLE           FALSE
#define NO_GOAWAY           TRUE
#define MOVE_TO_FRONT       (WindowPtr) -1L

#define STATUS_BORDER_WIDTH		5
#define STATUS_BORDER_HEIGHT	2
#define DIGIT_SPACE				2

#define CR_CHAR				0x0d
#define ENTER_CHAR			0x03
#define TAB_CHAR			0x09
#define ESC_CHAR			0x1b
#define LEFT_CHAR			0x1c
#define RIGHT_CHAR			0x1d
#define UP_CHAR				0x1e
#define DOWN_CHAR			0x1f


#define OK_BUTTON		1
#define CANCEL_BUTTON	2
#define ENDE_BUTTON		2
#define NOCHMAL_BUTTON	1
#define WEITER_BUTTON	1
#define KEY_UP_ITEM		3
#define KEY_RIGHT_ITEM	4
#define KEY_DOWN_ITEM	5
#define KEY_LEFT_ITEM	6
#define SOUND_ITEM		7
#define PICTURES_ITEM	8


/* ------------------------------------------------------------------------ */
/* Deklaration der globalen Variablen.                                      */
/* ------------------------------------------------------------------------ */

EventRecord         gEvent;                 /* Die Variable gEvent enthaelt */
                                            /* den aktuellen EventRecord    */
                                            /* (Informationen ueber den auf-*/
                                            /* getretenen Event).           */
Boolean				gGotEvent,              /* Wenn ein neuer Event vor-	*/
                                            /* liegt wird gGotEvent auf		*/
                                            /* TRUE gesetzt.                */
                    gSoundOn;				/* Wir machen den Ton			*/
MenuHandle          gAppleMenu;

char			    gKeyUp, 				/* Taste "hoch"					*/
					gKeyDown,				/* Taste "runter"				*/
					gKeyRight,				/* Taste "rechts"				*/
					gKeyLeft,				/* Taste "links"				*/
					gKeyPause;				/* Taste "Chef kommt !"			*/

GWorldPtr           gMyGWorld;              /* Offscreen-Area               */

WindowPtr           gTheCWindow;            /* Das Window fuer alles        */

Rect                gWorldRect,				/* Offscreen-Bereich			*/
					gLevelLabelRect,		/* Labels in der Statusanzeige	*/
					gPercentLabelRect,
					gPlayerLabelRect,
					gScoreLabelRect;

PicHandle			gDigits[10];			/* Die Digits der 7-Segment-	*/
											/* Anzeige						*/

PicHandle			gLevelLabel,			/* Labels der Statusanzeige		*/
					gPercentLabel,
					gPlayerLabel,
					gScoreLabel;

int					gDigitWidth,			/* Fuer das Arrangieren der		*/
					gDigitHeight;			/* Statusanzeige noetig			*/
					
PixPatHandle		gFilledPat,				/* Pattern für FILLED			*/
					gEmptyPat,				/* Pattern für EMPTY			*/
					gRunnerPat,
					gFlyerPat,
					gEaterPat,
					gWayPat,
					gStatusPat;
					
CIconHandle			gFlyerIcon,
					gRunnerIcon;
					
int					gPatternOffset;			/* Zum Wechsel für Patterns		*/

short				gActiveItem;			/* Aktiviertes Tastenfeld		*/

hgScoreEntry		gHighScoreEntry[10];	/* HighScoreListe				*/

short				gApplRefNum;			/* Referenz-Nr. Resource-Fork	*/
											/* von MacXonix	selbst			*/
short				gPrefsRefNum;			/* Selbiges fuer Prefs-File		*/

/* ------------------------------------------------------------------------ */
/* Forward-Declarations.                                                    */
/* ------------------------------------------------------------------------ */

void Set_WindowEnv (WindowPtr theWindow);
void Set_DrawingEnv (WindowPtr theWindow);
void InitStatusArea (void);
void InitMyWindow (void);
void InitKeyBoard (void);
void InitStatusDisplay (void);
void InitFigures (void);
void DrawStatusBackground (void);
void EndOfGameSound (void);
Boolean Do_KeyPrefs (void);
void DrawCompleteInside (void);
void LoadFiguresPattern (void);
void GetKeyString (Str255 myString, char myChar);
void ShowWinnersList (void);
void ResetWinnersList (void);
void ShowHints (void);
short ShowMessage (short dialogID, short endItem1, short endItem2);
void InitPreferences (void);
short OpenPrefsFile (void);

    
/* ------------------------------------------------------------------------ */
/* MakeGWorld erzeugt das Offscreen-Area fuer das "hintergruendige" Zeichnen*/
/* bevor der Bildschirm die Neuigkeiten erfaehrt.                           */
/* ------------------------------------------------------------------------ */

void MakeGWorld (void)
{   
    GDHandle    oldGD;
    GWorldPtr   oldGW;
    QDErr       errorCode;
       
    SetRect (&gWorldRect, 0, 0, 
			FIELD_WIDTH, 
			FIELD_HEIGHT + 2 * STATUS_BORDER_HEIGHT + gDigitHeight);

    GetGWorld (&oldGW, &oldGD);
    
    errorCode = NewGWorld (&gMyGWorld, MAX_PIXEL_DEPTH, &gWorldRect,
                            NIL_POINTER, NIL_POINTER, NO_FLAGS);
    if (errorCode != noErr)
    {
	  ExitXonix (1);
    }
       
    LockPixels (gMyGWorld -> portPixMap);
    SetGWorld (gMyGWorld, NIL_POINTER);
    
	DrawCompleteInside ();

    DrawCompleteBorder ();
    
	DrawStatusBackground ();

    SetGWorld (oldGW, oldGD);
    UnlockPixels (gMyGWorld -> portPixMap);
}   


/* ------------------------------------------------------------------------ */
/* Do_DragWindow ermoeglicht das Verschieben des Fensters.                  */
/* ------------------------------------------------------------------------ */

void Do_DragWindow (WindowPtr theWindow)
{
    if (theWindow != FrontWindow ())
        SelectWindow (theWindow);

    SetPort (theWindow);
    DragWindow (theWindow, gEvent.where, &qd.screenBits.bounds);
}   


/* ------------------------------------------------------------------------ */
/* Is_DAWindow ueberprueft, ob ein Fenster zu einem Schreibtischprogramm    */
/* gehoert. Diese Funktion wird fuer die Kompatibilitaet zu System 6 + 		*/
/* Finder in bezug auf DAs benoetigt.                                       */
/* ------------------------------------------------------------------------ */

short Is_DAWindow (WindowPtr theWindow)
{
    short daRefNum;
    
    daRefNum = ((WindowPeek) theWindow) -> windowKind;
    if (daRefNum < 0)                       /* Wenn windowKind des          */
                                            /* WindowRecords negativ        */
        return daRefNum;                    /* ist, dann gehoert das        */
                                            /* Fenster zu einem DA.         */
    else
        return 0;
}


/* ------------------------------------------------------------------------ */
/* Do_Close wird aufgerufen, wenn der Benutzer das aktuelle (das vordere)   */
/* Fenster schliessen will.                                                 */
/* ------------------------------------------------------------------------ */

Boolean Do_Close (void)
{
    short           itemHit,
                    daRefNum;
    WindowPtr       theWindow;
    Str255          title;
    
    theWindow = FrontWindow ();             /* Wenn das vordere Fenster zu  */
                                            /* einem Schreibtischprogramm   */
    daRefNum = Is_DAWindow (theWindow);     /* gehoert und das Programm		*/
                                            /* unter System 6 im Finder		*/
                                            /* laeuft, sollte das DA beendet*/
    if (daRefNum)                           /* werden, indem CloseDeskAcc 	*/
                                            /* aufgerufen wird.             */
    {                                       /* Dies ist aus Kompatbilitaets-*/
                                            /* gruenden in das Programm auf-*/
                                            /* genommen worden.             */
        CloseDeskAcc (daRefNum);            /* Wenn xonix unter Multifinder	*/
                                            /* bzw. System 7 laeuft, ueber- */
                                            /* nimmt der DA-Handler die Kon-*/
                                            /* trolle, wenn ein DA vorne ist*/
    }
    
    else
    {
        HideWindow (gTheCWindow);           /* Fenster wegblenden           */
    }
    
    gRun = FALSE;                           /* keine Animation mehr         */
    
    return true;
}   

/* ------------------------------------------------------------------------ */
/* Do_CloseWindow reagiert auf einen Klick in die Close-Box des Fensters.   */
/* ------------------------------------------------------------------------ */

void Do_CloseWindow (WindowPtr theWindow)
{
    if (TrackGoAway (theWindow, gEvent.where))
        Do_Close ();
}

/* ------------------------------------------------------------------------ */
/* Do_AppleMenu reagiert auf die Auswahl eines Menuepunktes aus dem "Apple"-*/
/* Menue, indem das ausgewaehlte Schreibtischprogramm gestartet wird oder	*/
/* der "ueber xonix..."-Dialog erzeugt wird.                               	*/
/* ------------------------------------------------------------------------ */

void Do_AppleMenu (short menuItem)
{
    short       daRefNum;
    Str255      daName;
    
    switch (menuItem)
    {
        case iAbout:                        /* "Ueber xonix..."-Menuepunkt. */
            ShowMessage (rAboutDialog, OK_BUTTON, OK_BUTTON);
            break;

        default:                            /* Alle anderen Menue punkte	*/
                                            /* sind DAs. Hole den DA-Namen  */
            GetItem (GetMHandle (mApple), menuItem, daName);
            daRefNum = OpenDeskAcc (daName);    /* Oeffne das DA            */
            break;
    }
}   


/* ------------------------------------------------------------------------ */
/* Do_Quit reagiert auf die Auswahl des "Beenden"-Menuepunktes, indem alle  */
/* geoeffneten Fenster geschlossen werden und das Terminationskriterium     */
/* gesetzt wird.                                                            */
/* ------------------------------------------------------------------------ */

void Do_Quit (void)
{
    gQuit = true;                      /* Ich will hier raus !!!       */
        
    if (gTheCWindow)
    	CloseWindow (gTheCWindow);

    Do_Close ();                            /* Das vordere Fenster schlies-	*/
    										/* sen.							*/

}   

/* ------------------------------------------------------------------------ */
/* Do_FileMenu koordiniert die Reaktion auf die Auswahl eines Menuepunktes  */
/* aus dem "Ablage"-Menue.                                                  */
/* ------------------------------------------------------------------------ */

void Do_FileMenu (short menuItem)
{
    switch (menuItem)
    {
        case iNew:                          /* "Neu".                       */
            Do_New ();
            break;

        case iOpen:                         /* "Oeffnen...".                */
            break;

        case iClose:                        /* "Schliessen".                */
            Do_Close ();
            break;

        case iSave:                         /* "Sichern".                   */
            break;

        case iSaveAs:                       /* "Sichern unter...".          */
            break;

        case iQuit:                         /* "Beenden".                   */
            Do_Quit ();
            break;
    }
}   


/* ------------------------------------------------------------------------ */
/* Do_PrefsMenu verwaltet den Menüpunkt "Einstellungen"						*/
/* ------------------------------------------------------------------------ */

void Do_PrefsMenu (short menuItem)
{
	switch (menuItem)
	{
		case iKeyPrefs:						/* "Tastenbelegung".			*/
			if ((Do_KeyPrefs ()) == TRUE)	/* Neustart notwendig ?			*/
			{
				Do_Close();
				Do_New();
			}
			break;
		case iSound:
			if (gSoundOn == TRUE)			/* Sound ist ein und muß aus-	*/
			{								/* geschaltet werden			*/
				gSoundOn = FALSE;
			}
			else
			{
				gSoundOn = TRUE;
			}						
			break;
		case iPictures:
			if (gPatternOffset == PICTURES_OFFSET)	/* Bilder verwendet		*/
			{
				gPatternOffset = NO_PICTURES_OFFSET;
			}
			else
			{
				gPatternOffset = PICTURES_OFFSET;
			}
			
			LoadFiguresPattern ();
			
			Do_Close ();
			Do_New ();
			
			break;
		case iResetHighScore:
			ResetWinnersList ();
		case iShowHighScore:
			ShowWinnersList ();
			break;
	}
}


/* ------------------------------------------------------------------------ */
/* Do_InfoMenu verwaltet den Menüpunkt "Infos"								*/
/* ------------------------------------------------------------------------ */

void Do_InfoMenu (short menuItem)
{
	switch (menuItem)
	{
		case iHints:
			ShowHints ();
			break;
		case iBackgroundInfo:
			ShowMessage (rBackgroundInfo, OK_BUTTON, OK_BUTTON);
			break;
	}
}

/* ------------------------------------------------------------------------ */
/* Do_MenuCommand entscheidet anhand der Menu-ID des Menues, zu dem der     */
/* ausgewaehlte Menuepunkt gehoert, welche Routine aufgerufen wird.         */
/* ------------------------------------------------------------------------ */

void Do_MenuCommand (long   choice)
{
    short       menuID,
                menuItem;
   
    /* Das Hi-Word des Ergebniswertes von MenuSelect oder MenuKey (der      */
    /* choice) enthaelt die Menu-ID des Menues, zu dem der ausgewaehlte     */
    /* Menuepunkt gehoert. Das Lo-Word beinhaltet die Menuepunktnummer des  */
    /* ausgewaehlten Menuepunktes.                                          */
    
    menuID = HiWord (choice);
    menuItem = LoWord (choice);
    
    switch (menuID)
    {
        case mApple:                        /* "Apple".                     */
            Do_AppleMenu (menuItem);
            break;

        case mFile:                         /* "Ablage".                    */
            Do_FileMenu (menuItem);
            break;

        case mEdit:                         /* "Bearbeiten".                */
            SystemEdit (menuItem -1);       /* Kompatibilitaet zu DA`s unter*/
            break;                          /* System 6.x + Finder.         */
            
        case mPrefs:						/* "Einstellungen"				*/
        	Do_PrefsMenu (menuItem);
        	break;
        	
        case mInfos:						/* "Infos"						*/
        	Do_InfoMenu (menuItem);
        	break;
        
    }
    HiliteMenu (0);                         /* Hervorhebung des ausgew.		*/
                                            /* Menuepunktes aufheben.       */
}

/* ------------------------------------------------------------------------ */
/* Adjust_Menus wird aufgerufen, bevor der Benutzer einen Menuepunkt aus-   */
/* waehlen kann. Diese Funktion sorgt dafuer, dass dem Benutzer nur die		*/
/* Menuepunkte zur Verfuegung stehen, die in der aktuellen Programmsituation*/
/* verwendet werden koennen.                                                */
/* ------------------------------------------------------------------------ */

void Adjust_Menus (void)
{
    MenuHandle      theMenu;
    WindowPtr       theWindow;
    
    /* "Ablage"-Menuepunkte*/
    
    theMenu = GetMHandle (mFile);           /* Handle auf Menue holen.      */
    
    DisableItem (theMenu, iOpen);
    DisableItem (theMenu, iSave);
    DisableItem (theMenu, iSaveAs);
    
    if (gRun == FALSE)                      /* Es laeuft noch nix           */
    {
      EnableItem (theMenu, iNew);           /* aktiviere Menuepunkte        */
                                            /* "Neu" und "Oeffnen...".      */
        DisableItem (theMenu, iClose);
    }
    else
    {
        DisableItem (theMenu, iNew);        /* Ansonsten deaktivieren       */
        EnableItem (theMenu, iClose);
    }
       
    if (Is_DAWindow (FrontWindow ()))       /* Vorderes Fenster DA ?        */
    {
        DisableItem (theMenu, iNew);        /* Saemtliche Menuepunkte des   */
                                            /* "Ablage"-Menues              */
        DisableItem (theMenu, iOpen);       /* deaktivieren (System 6.x +   */
                                            /* Finder).                     */
        EnableItem (theMenu, iClose);       /* Nur "Schliessen" aktiv       */
                                            /* (zum Beenden des DAs).       */
        DisableItem (theMenu, iSave);
        DisableItem (theMenu, iSaveAs);
    }
    
    theMenu = GetMHandle (mPrefs);			/* Handle auf Voreinst.-Menue	*/
    
    if (gSoundOn)							/* Es zwitschert schon			*/
    {
    	SetItem (theMenu, iSound, "\pSound AUS");
    }
    else
    {
    	SetItem (theMenu, iSound, "\pSound EIN");
    }
    
    if (gPatternOffset == PICTURES_OFFSET)	/* Bilder werden verwendet		*/
    {
    	SetItem (theMenu, iPictures, "\pBilder AUS");
    }
    else
    {
    	SetItem (theMenu, iPictures, "\pBilder EIN");
    }
}   

/* ------------------------------------------------------------------------ */
/* Do_MouseDown entscheidet anhand des Ergebniswertes von FindWindow,		*/
/* welche Aktionsroutine aufgerufen wird.                                   */
/* ------------------------------------------------------------------------ */

void Do_MouseDown (void)
{
    short           part;
    WindowPtr       theWindow;
    
    /* FindWindow untersucht die Position des Mausklicks und stellt fest, in*/
    /* welchem Teil des Bildschirms die Koordinaten liegen.                 */

    part = FindWindow (gEvent.where, &theWindow);
    switch (part)
    {
        case inSysWindow:                       /* Fenster eines DA`s       */
            SystemClick (&gEvent, theWindow);   /* Kompatibilitaet zu       */
                                                /* System 6.x + Finder.     */
            break;

        case inDrag:                            /* Fenster verschieben      */
            Do_DragWindow (theWindow);
            break;
                   
        case inGoAway:                          /* Fenster schliessen.      */
            Do_CloseWindow (theWindow);
            break;

        case inMenuBar:                         /* Menueleiste geklickt.    */
            Adjust_Menus ();                    /* Verfuegbare Menuepunkte  */
                                                /* aktivieren, nichtver-    */
                                                /* fuegbare deaktivieren.   */
            Do_MenuCommand (MenuSelect (gEvent.where));
                                                /* MenuSelect laesst den    */
                                                /* Benutzer einen Menuepunkt*/
                                                /* auswaehlen.              */
            break;
    }
}   

/* ------------------------------------------------------------------------ */
/* Do_KeyDown ueberprueft, ob es sich bei dem KeyDown-Event um einen Menue- */
/* kurzbefehl handelt (Befehlstaste + normale Taste).                       */
/* ------------------------------------------------------------------------ */

void Do_KeyDown (void)
{
    char            key;
    
    /* Das modifiers-Feld des EventRecords enthaelt die Information, ob bei	*/
    /* Betaetigung der Taste die Befehlstaste gedrueckt war. Die vordefi-	*/
    /* nierte Maske cmdKey kann verwendet werden, um das entsprechende Bit	*/
    /* des modifiers-Feldes auszumaskieren.                                 */

    if (gEvent.modifiers & cmdKey)
    {
        key = gEvent.message & charCodeMask;    /* ASCII-Code aus dem 		*/
                                                /* message-Feld des 		*/
                                                /* EventRecords.     		*/
        Adjust_Menus ();                        /* Verfuegbare Menuepunkte  */
                                                /* aktivieren, nichtver-    */
                                                /* fuegbare deaktivieren.   */
        Do_MenuCommand (MenuKey (key));         /* Menukey sucht den Menue- */
                                                /* punkt mit dem gesuchten  */
                                                /* Menuekurzbefehl.         */
    }
        key = gEvent.message & charCodeMask;

		if (key == gKeyLeft)
		{
			gMyRunner.dx = - RATIO;
            gMyRunner.dy = 0;
		}
		else if (key == gKeyRight)
		{
			gMyRunner.dx = RATIO;
            gMyRunner.dy = 0;
		}
		else if (key == gKeyUp)
		{
			gMyRunner.dy = - RATIO;
            gMyRunner.dx = 0;
		}
		else if (key == gKeyDown)
		{
			gMyRunner.dy = RATIO;
            gMyRunner.dx = 0;
		}
		else if (key == gKeyPause)
		{
			if (gRun == TRUE)
			{
				gRun = FALSE;
				HideWindow (gTheCWindow);
			}
			else
			{
				gRun = TRUE;
				ShowWindow (gTheCWindow);
			}
		}   
}  


/* ------------------------------------------------------------------------ */
/* Set_DrawingEnv macht das aktuelle Window zur Zeichenumgebung             */
/* ------------------------------------------------------------------------ */

void Set_DrawingEnv (WindowPtr  theWindow)
{
    SetPort (theWindow);
}   


/* ------------------------------------------------------------------------ */
/* Set_WindowEnv setzt den Koordinatensystemursprung wieder auf die ur-		*/
/* spruengliche Position (0,0) zurueck und sorgt dafuer, dass die Clipping-	*/
/* Region den gesamten Fensterinhalt (inlusive Scrollbars) beschreibt.      */
/* ------------------------------------------------------------------------ */

void Set_WindowEnv (WindowPtr   theWindow)
{
    SetPort (theWindow);
    SetOrigin (0, 0);
    ClipRect (&theWindow->portRect);
}   

/* ------------------------------------------------------------------------ */
/* Do_Update reagiert auf einen Update-Event, indem der Zeichenbereich auf  */
/* den neuzuzeichnenden Bereich beschraenkt und die gesamte Grafik gezeich-	*/
/* net wird.                                                                */
/* ------------------------------------------------------------------------ */

void Do_Update (void)
{
    WindowPtr   theWindow;
    
    GWorldEntry ();
    
    theWindow = (WindowPtr) gEvent.message; /* Das Universalfeld message	*/
                                            /* verweist auf das neuzuzeich-	*/
                                            /* nende Fenster.               */
    Set_WindowEnv (theWindow);              /* Koordinatensystem, Clipping- */
                                            /* Region auf "Normalzustand".  */
    BeginUpdate (theWindow);                /* Die Visible-Region (visRgn)  */
                                            /* wird auf den neuzuzeichnenden*/
                                            /* Bereich eingeschraenkt.      */
    Set_DrawingEnv (theWindow);             /* Zeichenbereich einschraenken,*/
                                            /* Koordinatensystemursprung	*/
                                            /* verschieben.                 */
	Set_WindowEnv (theWindow);              /* Koordinatensystem, Clipping- */
                                            /* Region auf "Normalzustand".  */
	GWorldExit ();
    EndUpdate (theWindow);                  /* Visible-Region wird wieder	*/
                                            /* auf die urspruengliche Form  */
                                            /* zurueckgesetzt.              */
}


/* ------------------------------------------------------------------------ */
/* Do_Activate reagiert auf einen Activate-Event, indem das Fenster an den  */
/* neuen Aktivierungszustand (aktiv oder inaktiv) angepasst wird.           */
/* ------------------------------------------------------------------------ */

void Do_Activate (void)
{
    WindowPtr       theWindow;
    
    theWindow = (WindowPtr) gEvent.message; /* Das Universalfeld message	*/
                                            /* verweist auf das betroffene	*/
                                            /* Fenster.                     */
    SetPort (theWindow);
    
    if (gEvent.modifiers & activeFlag)      /* Activate-/ Deactivate-Event ?*/
    {
        /* Activate-Event.*/

        SetCursor (&qd.arrow);              /* Der normale Pfeilcursor.     */
    }
    else
    {
        /* DeActivate-Event.*/

    }
}   

/* ------------------------------------------------------------------------ */
/* Do_Event entscheidet anhand des Event-Typs, welche Event-Behandlungs-    */
/* routine aufgerufen wird.                                                 */
/* ------------------------------------------------------------------------ */

void Do_Event (void)
{
    switch (gEvent.what)
    {
        case mouseDown:                     /* MouseDown-Event.             */
            Do_MouseDown ();
            break;

        case keyDown:                       /* KeyDown-Event.               */
        case autoKey:
            Do_KeyDown ();
            break;

        case updateEvt:                     /* Update-Event.                */
            Do_Update ();
            break;
   
        case activateEvt:                   /* Activate-Event.              */
            Do_Activate ();
            break;
    }
}   


/* ------------------------------------------------------------------------ */
/* Init_ToolBox initialisiert die benoetigten ToolBox-Manager.              */
/* ------------------------------------------------------------------------ */

void Init_ToolBox (void)
{
    InitGraf ((Ptr) &qd.thePort);           /* QuickDraw.                   */
    InitFonts ();                           /* Font-Manager.                */
    InitWindows ();                         /* Window-Manager.              */
    InitMenus ();                           /* Menu-Manager.                */
    TEInit ();                              /* TextEdit.                    */
    InitDialogs (NULL);                     /* Dialog-Manager.              */
}   


/* ------------------------------------------------------------------------ */
/* Make_Menus laedt und installiert die Menueleiste.                        */
/* ------------------------------------------------------------------------ */

void Make_Menus (void)
{
    Handle  menuBar;
    
    menuBar = GetNewMBar (rMenuBar);
    SetMenuBar (menuBar);
    gAppleMenu = GetMHandle (mApple);
    
    AddResMenu (gAppleMenu, 'DRVR');
    DrawMenuBar();
}   


/* ------------------------------------------------------------------------ */
/* Initialize setzt die globalen Variablen auf ihre Ausgangswerte und       */
/* koordiniert den weiteren Initialisierungsprozess.                        */
/* ------------------------------------------------------------------------ */

void Initialize (void)
{
    gQuit = false;                          /* Terminationskriterium.       */
    
    Init_ToolBox ();                        /* Initialisiere QuickDraw,     */
                                            /* Window-Manager etc.          */
    Make_Menus ();                          /* Erzeuge Menueleiste.         */
    
    SetCursor (&qd.arrow);                  /* Der normale Pfeilkursor      */
    
	InitStatusArea ();						/* Initialisiere Statusfeld		*/    

	InitPreferences ();						/* Initialisierung Tasten,		*/
											/* Bild-/Tonverwendung,			*/
											/* HighScore-Tabelle			*/
	
	InitStatusDisplay ();					/* Initialisierung Statusanzeige*/

	InitFigures ();							/* Initialisierung der Spiel-	*/
											/* elemente						*/
    MakeGWorld ();                          /* Offscreen-Area anlegen       */
    
	InitMyWindow ();						/* Anlegen Spielfenster			*/
    
    Do_New ();                              /* Starte neues Spiel           */
}   


/* ------------------------------------------------------------------------ */
/* InitStatusArea legt das Verwaltungsfeld an                 				*/
/* ------------------------------------------------------------------------ */

void InitStatusArea (void)
{
	int i;

    i = H_STEPS * V_STEPS;                  /* Groesse der Spielflaeche     */
    
    gMyStatusArea = NewPtr (i);             /* Statusfeld anfordern         */
       
    if (gMyStatusArea == NIL_POINTER)       /* Es reicht nicht aus !!       */
    {
	 	ExitXonix (1);
    }
}


/* ------------------------------------------------------------------------ */
/* InitMyWindow legt das Hauptwindow fuer das Spiel an						*/
/* ------------------------------------------------------------------------ */

void InitMyWindow (void)
{
	Rect r;

    SetRect (&r, 
			WINDOW_START_X,
			WINDOW_START_Y,
            WINDOW_START_X + FIELD_WIDTH, 
			WINDOW_START_Y + FIELD_HEIGHT + 2 * STATUS_BORDER_HEIGHT +
				gDigitHeight);

    gTheCWindow = NewCWindow (NIL_POINTER, &r, "\pXonix", VISIBLE,
                                noGrowDocProc, MOVE_TO_FRONT,
                                NO_GOAWAY, NIL_POINTER);
                                
    SetPort (gTheCWindow);                  /* Spielfeld-Window anlegen     */
}


/* ------------------------------------------------------------------------ */
/* InitKeyBoard legt die Default-Einstellung der Tasten fest				*/
/* ------------------------------------------------------------------------ */

void InitKeyBoard (void)
{
	kbPrefsHandle myKbHandle;
	
	myKbHandle = (kbPrefsHandle) Get1Resource ('KBRS', rKeyboardPrefs);
    
    gKeyUp		= (**myKbHandle).upKey;
    gKeyDown	= (**myKbHandle).downKey;
    gKeyRight	= (**myKbHandle).rightKey;
    gKeyLeft	= (**myKbHandle).leftKey;
    gKeyPause	= (**myKbHandle).pauseKey;
}

/* ------------------------------------------------------------------------ */
/* InitPreferences liest das Preferences-File aus dem entsprechenden 		*/
/* Systemordner und initialisiert damit die globalen Variablen für			*/
/* Sound- und Bildverwendung, Tastaturbelegung bzw. die HighScore-Tabelle	*/
/* Wird kein File gefunden, wird ein neues File mit Defaultwerten erzeugt.	*/
/* ------------------------------------------------------------------------ */

void InitPreferences (void)
{
	gApplRefNum = CurResFile ();
	
	gPrefsRefNum = OpenPrefsFile ();
	
	UseResFile (gPrefsRefNum);
	
	InitKeyBoard ();
	
	UseResFile (gApplRefNum);
}

/* ------------------------------------------------------------------------ */
/* InitStatusDisplay initialisiert alle Elemente des Statusdisplays			*/
/* ------------------------------------------------------------------------ */

void InitStatusDisplay (void)
{
	int i, space;
	int levelLabelWidth, percentLabelWidth, playerLabelWidth, scoreLabelWidth;
	int labelHeight;

	for (i = 0; i < 10; i ++)				/* Pict-Ressourcen laden		*/
	{
		gDigits[i] = GetPicture (rDigit0 + i);
	}

	gLevelLabel		= GetPicture (rLevelLabel);
	gPercentLabel	= GetPicture (rPercentLabel);
	gPlayerLabel	= GetPicture (rPlayerLabel);
	gScoreLabel		= GetPicture (rScoreLabel);

	gDigitWidth		= (**(gDigits[0])).picFrame.right - 
						(**(gDigits[0])).picFrame.left;
	gDigitHeight	= (**(gDigits[0])).picFrame.bottom - 
						(**(gDigits[0])).picFrame.top;

	levelLabelWidth		= (**(gLevelLabel)).picFrame.right -
							(**(gLevelLabel)).picFrame.left;
	percentLabelWidth	= (**(gPercentLabel)).picFrame.right -
							(**(gPercentLabel)).picFrame.left;
	playerLabelWidth	= (**(gPlayerLabel)).picFrame.right -
							(**(gPlayerLabel)).picFrame.left;
	scoreLabelWidth		= (**(gScoreLabel)).picFrame.right -
							(**(gScoreLabel)).picFrame.left;

	labelHeight		= (**(gLevelLabel)).picFrame.bottom - 
						(**(gLevelLabel)).picFrame.top;

	/* Pict-Ressourcen sind geladen, nun kann arrangiert werden */

	space = FIELD_WIDTH - levelLabelWidth - 2 * gDigitWidth - DIGIT_SPACE
						- percentLabelWidth - 2 * gDigitWidth - DIGIT_SPACE
						- playerLabelWidth - 2 * gDigitWidth - DIGIT_SPACE
						- scoreLabelWidth - 6 * gDigitWidth - 5 * DIGIT_SPACE
						- 2 * STATUS_BORDER_WIDTH;

	space /= 3;			/* 4 Anzeigen sind es, also 3 Zwischenraeume */
	
	SetRect (&gLevelLabelRect,
			STATUS_BORDER_WIDTH,
			FIELD_HEIGHT + STATUS_BORDER_HEIGHT,
			STATUS_BORDER_WIDTH + levelLabelWidth,
			FIELD_HEIGHT + STATUS_BORDER_HEIGHT + labelHeight);

	SetRect (&gPercentLabelRect,
			STATUS_BORDER_WIDTH + levelLabelWidth + 2 * gDigitWidth + DIGIT_SPACE 
				+ space,
			FIELD_HEIGHT + STATUS_BORDER_HEIGHT,
			STATUS_BORDER_WIDTH + levelLabelWidth + 2 * gDigitWidth + DIGIT_SPACE 
				+ space + percentLabelWidth,
			FIELD_HEIGHT + STATUS_BORDER_HEIGHT + labelHeight);

	SetRect (&gPlayerLabelRect,
			STATUS_BORDER_WIDTH + levelLabelWidth + 2 * gDigitWidth + DIGIT_SPACE 
				+ space + percentLabelWidth + 2 * gDigitWidth + DIGIT_SPACE + space,
			FIELD_HEIGHT + STATUS_BORDER_HEIGHT,
			STATUS_BORDER_WIDTH + levelLabelWidth + 2 * gDigitWidth + DIGIT_SPACE 
				+ space + percentLabelWidth + 2 * gDigitWidth + DIGIT_SPACE + space
				+ playerLabelWidth,
			FIELD_HEIGHT + STATUS_BORDER_HEIGHT + labelHeight);
			
	SetRect (&gScoreLabelRect,
			STATUS_BORDER_WIDTH + levelLabelWidth + 2 * gDigitWidth + DIGIT_SPACE 
				+ space + percentLabelWidth + 2 * gDigitWidth + DIGIT_SPACE + space
				+ playerLabelWidth + 2 * gDigitWidth + DIGIT_SPACE + space,
			FIELD_HEIGHT + STATUS_BORDER_HEIGHT,
			STATUS_BORDER_WIDTH + levelLabelWidth + 2 * gDigitWidth + DIGIT_SPACE
				+ space + percentLabelWidth + 2 * gDigitWidth + DIGIT_SPACE + space
				+ playerLabelWidth + 2 * gDigitWidth + DIGIT_SPACE + space 
				+ scoreLabelWidth,
			FIELD_HEIGHT + STATUS_BORDER_HEIGHT + labelHeight);


}

/* ------------------------------------------------------------------------ */
/* CopyDefaultHighScoreList kopiert die String-Resourcen der Applikation in */
/* das Preferences-File														*/
/* ------------------------------------------------------------------------ */

void CopyDefaultHighScoreList (short fileRefNum)
{
	StringHandle	myStringHandle;
	short			resID;
	ResType			resType;
	Str255			resName;
	short			resAttributes;
	short			i;
	
	for (i = 0; i < HIGHSCORE_ITEMS * 3; i ++)
	{
		UseResFile (gApplRefNum);
		
		myStringHandle = (StringHandle) Get1Resource ('STR ', 
													rDefaultHighScore + i);
		if (myStringHandle)
		{
			GetResInfo (myStringHandle, &resID, &resType, resName);
			resAttributes = GetResAttrs (myStringHandle);
			DetachResource (myStringHandle);
			
			UseResFile (fileRefNum);
			
			AddResource (myStringHandle, 'STR ', rHighScore + i, resName);
			SetResAttrs (myStringHandle, resAttributes);
			ChangedResource (myStringHandle);
			WriteResource (myStringHandle);
			
			ReleaseResource (myStringHandle);
		}
	}	
}

/* ------------------------------------------------------------------------ */
/* CopyDefaultKeyboardPrefs kopiert die Tastaturbelegungs-Resource in das 	*/
/* uebergebene Resource-File												*/
/* ------------------------------------------------------------------------ */

void CopyDefaultKeyboardPrefs (short fileRefNum)
{
	kbPrefsHandle myKbHandle;
	short			resID;
	ResType			resType;
	Str255			resName;
	short			resAttributes;
	
	UseResFile (gApplRefNum);
	
	myKbHandle = (kbPrefsHandle) Get1Resource ('KBRS', rKeyboardPrefs);
	
	if (myKbHandle)
	{
			GetResInfo (myKbHandle, &resID, &resType, resName);
			resAttributes = GetResAttrs (myKbHandle);
			DetachResource (myKbHandle);
			
			UseResFile (fileRefNum);
			
			AddResource (myKbHandle, 'KBRS', rKeyboardPrefs, resName);
			SetResAttrs (myKbHandle, resAttributes);
			ChangedResource (myKbHandle);
			WriteResource (myKbHandle);
			
			ReleaseResource (myKbHandle);
	}
}

/* ------------------------------------------------------------------------ */
/* CreateNewPrefsFile erzeugt bei Nichtvorhandensein eines Preferences-		*/
/* Files ein Solches														*/
/* ------------------------------------------------------------------------ */

short CreateNewPrefsFile (FSSpec prefFSSpec)
{
	short	fileRefNum;
	Handle	appHandle;
	short	resID;
	ResType	resType;
	Str255	resName;
	short	resAttributes;
	
	FSpCreateResFile (&prefFSSpec, 'RSED', 'rsrc', iuSystemScript);
	
	fileRefNum = FSpOpenResFile (&prefFSSpec, fsCurPerm);
	
	CopyDefaultHighScoreList (fileRefNum);
	CopyDefaultKeyboardPrefs (fileRefNum);
		
	UseResFile (gApplRefNum);
	
	return fileRefNum;
}

/* ------------------------------------------------------------------------ */
/* OpenPrefsFile oeffnet das Preferences-File								*/
/* ------------------------------------------------------------------------ */

short OpenPrefsFile (void)
{
	short	fileRefNum;
	short	volRef;
	long	dirID;
	FSSpec	prefFSSpec;
	Str255	prefFileName;
	
	GetIndString (prefFileName, STRING_LIST, PREF_STRING_INDEX);
	
	FindFolder (kOnSystemDisk, kPreferencesFolderType, kDontCreateFolder,
				&volRef, &dirID);
				
	FSMakeFSSpec (volRef, dirID, prefFileName, &prefFSSpec);
	
	fileRefNum = FSpOpenResFile (&prefFSSpec, fsCurPerm);
	
	if (fileRefNum == -1)
		fileRefNum = CreateNewPrefsFile (prefFSSpec);
		
	return fileRefNum;
}

/* ------------------------------------------------------------------------ */
/* LoadFiguresPattern laedt die Pattern aller Figuren						*/
/* ------------------------------------------------------------------------ */

void LoadFiguresPattern (void)
{
    gFilledPat	= GetPixPat (rFilledPattern + gPatternOffset);
    gEmptyPat	= GetPixPat (rEmptyPattern + gPatternOffset);
    gRunnerPat	= GetPixPat (rRunnerPattern + gPatternOffset);
    gFlyerPat	= GetPixPat (rFlyerPattern + gPatternOffset);
    gEaterPat	= GetPixPat (rEaterPattern + gPatternOffset);
    gWayPat		= GetPixPat (rWayPattern + gPatternOffset);
    
    if (gPatternOffset == PICTURES_OFFSET)
    {
    	gRunnerIcon = GetCIcon (rRunnerIcon);
    }
    else
    {
    	gRunnerIcon	= GetCIcon (rRunnerIconPlain);
    }
}


/* ------------------------------------------------------------------------ */
/* InitFigures initialisiert die Spielelemente (Farbzuweisung, Pattern usw.)*/
/* ------------------------------------------------------------------------ */

void InitFigures (void)
{    
    gPatternOffset = NO_PICTURES_OFFSET;
        
    gStatusPat	= GetPixPat (rStatusPattern);
    
    gFlyerIcon	= GetCIcon (rFlyerIcon);

    LoadFiguresPattern ();
    
    gSoundOn = TRUE;
}


/* ------------------------------------------------------------------------ */
/* DrawRunnerToGWorld zeichnet den Runner an der aktuellen Position in		*/
/* GWorld																	*/
/* ------------------------------------------------------------------------ */
    
void DrawRunnerToGWorld (void)
{   
    Rect    r;

    r.top = gMyRunner.y * EATER_SIZE;
    r.bottom = r.top + gMyRunner.sy;
    r.left = gMyRunner.x * EATER_SIZE; 
    r.right = r.left + gMyRunner.sx;

 	PlotCIcon (&r, gRunnerIcon);
}  


/* ------------------------------------------------------------------------ */
/* DrawWayToGWorld zeichnet die Runner-Spur an der aktuellen Position       */
/* in GWorld                                                                */
/* ------------------------------------------------------------------------ */
   
void DrawWayToGWorld (void)
{  
    Rect    r;
   
    r.top = gMyRunner.y * EATER_SIZE;
    r.bottom = r.top + gMyRunner.sy;
    r.left = gMyRunner.x * EATER_SIZE;
    r.right = r.left + gMyRunner.sx;
   
    FillCRect (&r, gWayPat);
}  


/* ------------------------------------------------------------------------ */
/* DrawEmptyToGWorld zeichnet ein freies Feld (Runner-Size) in GWorld       */
/* ------------------------------------------------------------------------ */
   
void DrawEmptyToGWorld (int xPos, int yPos)
{
    Rect    r;

    r.top = yPos * EATER_SIZE;
    r.bottom = r.top + RUNNER_SIZE;
    r.left = xPos * EATER_SIZE;
    r.right = r.left + RUNNER_SIZE;

    FillCRect (&r, gEmptyPat);     
}

/* ------------------------------------------------------------------------ */
/* DrawFlyerToGWorld zeichnet einen Flyer in GWorld                         */
/* ------------------------------------------------------------------------ */
   
void DrawFlyerToGWorld (int xPos, int yPos)
{
    Rect    r;

    r.top = yPos * EATER_SIZE;
    r.bottom = r.top + FLYER_SIZE;
    r.left = xPos * EATER_SIZE;
    r.right = r.left + FLYER_SIZE;

    if (gPatternOffset == PICTURES_OFFSET)
    {
    	PlotCIcon (&r, gFlyerIcon);
    }
    else
    {
    	FillCRect (&r, gFlyerPat);
    }
}

/* ------------------------------------------------------------------------ */
/* DrawEaterToGWorld zeichnet einen Eater in GWorld                         */
/* ------------------------------------------------------------------------ */
   
void DrawEaterToGWorld (int xPos, int yPos)
{
    Rect    r;

    r.top = yPos * EATER_SIZE;
    r.bottom = r.top + EATER_SIZE;
    r.left = xPos * EATER_SIZE;
    r.right = r.left + EATER_SIZE;

    FillCRect (&r, gEaterPat);
}

/* ------------------------------------------------------------------------ */
/* DrawFilledToGWorld zeichnet ein Randfeld in Groesse eines Runners in 	*/
/* GWorld																	*/
/* ------------------------------------------------------------------------ */

void DrawFilledToGWorld (int xPos, int yPos)
{
    Rect    r;

    r.top = yPos * EATER_SIZE;
    r.bottom = r.top + RUNNER_SIZE;
    r.left = xPos * EATER_SIZE;
    r.right = r.left + RUNNER_SIZE;

    FillCRect (&r, gFilledPat);     
}


/* ------------------------------------------------------------------------ */
/* DrawSmallFilledToGWorld zeichnet ein gefuelltes Feld in Groesse eines	*/
/* Eaters nach GWorld                                                       */
/* ------------------------------------------------------------------------ */

void DrawSmallFilledToGWorld (int xPos, int yPos)
{
    Rect    r;

    r.top = yPos * EATER_SIZE;
    r.bottom = r.top + EATER_SIZE;
    r.left = xPos * EATER_SIZE;
    r.right = r.left + EATER_SIZE;
   
    FillCRect (&r, gFilledPat);
}  


/* ------------------------------------------------------------------------ */
/* DrawCompleteBorder zeichnet den gesamten Rahmen um das Spielfeld neu     */
/* ------------------------------------------------------------------------ */

void DrawCompleteBorder (void)
{
	int i;
	
	for (i = 0; i < H_STEPS; i += RATIO)
	{
		DrawFilledToGWorld (i, 0);
		DrawFilledToGWorld (i, V_STEPS - RATIO);
	}
	
	for (i = RATIO; i < V_STEPS - RATIO; i += RATIO)
	{
		DrawFilledToGWorld (0, i);
		DrawFilledToGWorld (H_STEPS - RATIO, i);
	}
}


/* ------------------------------------------------------------------------ */
/* DrawCompleteInside zeichnet das Innere des Spielfelds neu     			*/
/* ------------------------------------------------------------------------ */

void DrawCompleteInside (void)
{
    Rect    r;

    r.top = RATIO * EATER_SIZE;
    r.bottom = (V_STEPS - RATIO) * EATER_SIZE;
    r.left = RATIO * EATER_SIZE;
    r.right = (H_STEPS - RATIO) * EATER_SIZE;

    FillCRect (&r, gEmptyPat);    
}


/* ------------------------------------------------------------------------ */
/* DrawStatusBackground zeichnet den Hintergrund der Statusanzeige			*/
/* ------------------------------------------------------------------------ */

void DrawStatusBackground (void)
{
    Rect    r;
    PicHandle Ph;
   
    r.top = FIELD_HEIGHT;
    r.bottom = r.top + 2 * STATUS_BORDER_HEIGHT + gDigitHeight;
    r.left = 0;
    r.right = FIELD_WIDTH;
   
	FillCRect (&r, gStatusPat);
	    
    DrawPicture (gLevelLabel, &gLevelLabelRect);
	DrawPicture (gPercentLabel, &gPercentLabelRect);
	DrawPicture (gPlayerLabel, &gPlayerLabelRect);
	DrawPicture (gScoreLabel, &gScoreLabelRect);
}


/* ------------------------------------------------------------------------ */
/* Draw2DigitsToGWorld zeichnet eine zweistellige Ziffer in das ueber-		*/
/* gebene Rechteck															*/
/* ------------------------------------------------------------------------ */

void Draw2DigitsToGWorld (int num, Rect *r)
{
	int value;

	value = num / 10;
	r -> left	= r -> right + DIGIT_SPACE;
	r -> right	= r -> left + gDigitWidth;
	r -> bottom	= r -> top + gDigitHeight; 

	DrawPicture (gDigits[value], r);

	value = num % 10;
	r -> left	+= gDigitWidth + DIGIT_SPACE;
	r -> right	+= gDigitWidth + DIGIT_SPACE;

	DrawPicture (gDigits[value], r);

}


/* ------------------------------------------------------------------------ */
/* ScorePercentage zeigt den aktuellen Fuellstand in Prozent an				*/
/* ------------------------------------------------------------------------ */
   
void ScorePercentage (int num)
{
	Rect r;

	r = gPercentLabelRect;

	Draw2DigitsToGWorld (num, &r);

	return;
}


/* ------------------------------------------------------------------------ */
/* ScoreLevel zeigt das aktuelle Level an									*/
/* ------------------------------------------------------------------------ */
   
void ScoreLevel (int num)
{
	Rect r;

	r = gLevelLabelRect;

	Draw2DigitsToGWorld (num, &r);

	return;
}


/* ------------------------------------------------------------------------ */
/* ScoreRunner zeigt die Anzahl der verbliebenen Runner an 					*/
/* ------------------------------------------------------------------------ */
   
void ScoreRunner (int num)
{
	Rect r;

	r = gPlayerLabelRect;

	Draw2DigitsToGWorld (num, &r);

	return;
}


/* ------------------------------------------------------------------------ */
/* ScorePoints zeigt die aktuelle Punktzahl an								*/
/* ------------------------------------------------------------------------ */

void ScorePoints (unsigned int points)
{
	Rect r;
	
	r = gScoreLabelRect;

	Draw2DigitsToGWorld ((points / 10000) % 100, &r);
	
	Draw2DigitsToGWorld ((points % 10000) / 100, &r);
	
	Draw2DigitsToGWorld ((points % 10000) % 100, &r);
	
}

/* ----------------------------------------------------------------------------	*/
/* Die folgenden kleinen Routinen unterstützen die Arbeit mit Dialog-Items		*/
/* ----------------------------------------------------------------------------	*/

/* Hole Text eines Dialog-Items */

static void GetDText(DialogPtr dlg, short item, Str255 str)
{
	short type;
	Handle hitem;
	Rect box;

	GetDItem(dlg, item, &type, &hitem, &box);
	GetIText(hitem, str);
}

/* Setze Text eines Dialog-Items */

static void SetDText(DialogPtr dlg, short item, const Str255 str)
{
	short type;
	Handle hitem;
	Rect box;

	GetDItem(dlg, item, &type, &hitem, &box);
	SetIText(hitem, str);
}

/* Hole numerischen Wert eines Dialog-Items */

static long GetDNum(DialogPtr dlg, short item)
{
	long num;
	Str255 str;
	
	GetDText(dlg, item, str);
	StringToNum(str, &num);
	return(num);
}

/* Setze numerischen Wert eines Dialog-Items */

static void SetDNum(DialogPtr dlg, short item, long num)
{
	Str255 str;

	NumToString(num, str);
	SetDText(dlg, item, str);
}

/* Hole ein ControlHandle für ein Item */

static ControlHandle GetDControl(DialogPtr dlg, short item)
{
	short type;
	Handle hitem;
	Rect box;

	GetDItem(dlg, item, &type, &hitem, &box);
	return((ControlHandle) hitem);
}

static void SetItemValue (DialogPtr dlg, short itemNo, short value)
{
	short itemType;
	Handle item;
	Rect box;
	
	GetDItem (dlg, itemNo, &itemType, &item, &box);
	SetCtlValue ((ControlHandle) item, value);
}


/* ------------------------------------------------------------------------ */
/* ShowHints zeigt den "Tips & Hinweise"-Dialog								*/
/* ------------------------------------------------------------------------ */

void ShowHints (void)
{
	if (ShowMessage (rHints1, ENDE_BUTTON, WEITER_BUTTON) == WEITER_BUTTON)
	{
		ShowMessage (rHints2, OK_BUTTON, OK_BUTTON);	
	}
}


/* ------------------------------------------------------------------------ */
/* CheckHighScore ueberprueft, ob die aktuelle Punktzahl fuer einen Rang 	*/
/* unter den ersten 10 ausreicht											*/
/* ------------------------------------------------------------------------ */

Boolean CheckHighScore (void)
{
	StringHandle	myStringHandle;
	int				lastHighScore;

	UseResFile (gPrefsRefNum);
		
	myStringHandle = (StringHandle) Get1Resource ('STR ', 
									rHighScore + 2 * HIGHSCORE_ITEMS - 1);
									
	if (myStringHandle)
		StringToNum (*myStringHandle, &lastHighScore);
	
	UseResFile (gApplRefNum);
	
	if (lastHighScore < gHighScore)
	{
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}


/* ------------------------------------------------------------------------ */
/* SetHighScoreLine setzt die Dialogelemente der übergebenen Platznummer	*/
/* ------------------------------------------------------------------------ */

void SetHighScoreLine (short number, DialogPtr theDialog)
{
	StringHandle	myStringHandle;
	
    myStringHandle = (StringHandle) Get1Resource ('STR ', 
						rHighScore + number + 2 * HIGHSCORE_ITEMS);
	if (myStringHandle)
		SetDText (theDialog, iWinnerName + number + 2 * HIGHSCORE_ITEMS,
			 		*myStringHandle);
			 			
    myStringHandle = (StringHandle) Get1Resource ('STR ', 
						rHighScore + number + HIGHSCORE_ITEMS);
	if (myStringHandle)
		SetDText (theDialog, iWinnerName + number + HIGHSCORE_ITEMS,
			 		*myStringHandle);

    myStringHandle = (StringHandle) Get1Resource ('STR ', 
						rHighScore + number);
	if (myStringHandle)
		SetDText (theDialog, iWinnerName + number, *myStringHandle);		
}


/* ------------------------------------------------------------------------ */
/* DeleteHighScoreEntry löscht den Eintrag mit der übergebenen Nummer		*/
/* ------------------------------------------------------------------------ */

void DeleteHighScoreEntry (short number)
{
	StringHandle	myStringHandle;
	
	myStringHandle = (StringHandle) Get1Resource ('STR ', 
							rHighScore + number - 1);
	if (myStringHandle)
		RmveResource (myStringHandle);
	
	myStringHandle = (StringHandle) Get1Resource ('STR ', 
							rHighScore + number + HIGHSCORE_ITEMS - 1);
	if (myStringHandle)
		RmveResource (myStringHandle);
		
	myStringHandle = (StringHandle) Get1Resource ('STR ', 
							rHighScore + number + 2 * HIGHSCORE_ITEMS - 1);
	if (myStringHandle)
		RmveResource (myStringHandle);
	
}

/* ------------------------------------------------------------------------ */
/* ShowWinnersList zeigt die "Hall of Fame"									*/
/* ------------------------------------------------------------------------ */

void ShowWinnersList (void)
{
	DialogPtr		theDialog;
	short			itemHit, i;
	
    theDialog = GetNewDialog (rWinnersList, NULL, (WindowPtr) -1);
    
    UseResFile (gPrefsRefNum);
    
    for (i = 0; i < HIGHSCORE_ITEMS; i ++)
    {
		SetHighScoreLine (i, theDialog);
    }
	
	UseResFile (gApplRefNum);
	
	ShowWindow (theDialog);
	
    do 
    {
        ModalDialog (NULL, &itemHit);
    } 
    while (itemHit != OK_BUTTON);

    DisposeDialog (theDialog);
}

/* ------------------------------------------------------------------------ */
/* ResetWinnersList löscht alle Eintraege in der "Hall of Fame" und gene-	*/
/* riert 10 Standard-Eintraege												*/
/* ------------------------------------------------------------------------ */

void ResetWinnersList (void)
{
	StringHandle	myStringHandle;
	int				i;
	
	UseResFile (gPrefsRefNum);
	
	for (i = 0; i < HIGHSCORE_ITEMS; i ++)
	{
		DeleteHighScoreEntry (i + 1);
	}
	
	CopyDefaultHighScoreList (gPrefsRefNum);
	
	UseResFile (gApplRefNum);
}

/* ------------------------------------------------------------------------ */
/* ModifyWinnersList modifiziert die STR - Ressourcen der "Hall of Fame" 	*/
/* und traegt den neuen Namen ein											*/
/* ------------------------------------------------------------------------ */

void ModifyWinnersList (Str255 newName, int newPoints)
{
	StringHandle	myStringHandle, myNameHandle, myPointsHandle, myLevelHandle;
	int				i, points;
	Str255			myPointsString, myLevelString;
	
	UseResFile (gPrefsRefNum);
	
	DeleteHighScoreEntry (HIGHSCORE_ITEMS);
		
	for (i = HIGHSCORE_ITEMS - 2, points = newPoints - 1;
		 (i >= 0) && (points < newPoints); i --)
	{
		myStringHandle = (StringHandle) Get1Resource ('STR ', 
										rHighScore + HIGHSCORE_ITEMS + i);
									
		StringToNum (*myStringHandle, &points);
		
		if (points < newPoints)
		{
			SetResInfo (myStringHandle, rHighScore + HIGHSCORE_ITEMS + i + 1,
						 NULL);
			WriteResource (myStringHandle);
			
			myStringHandle = (StringHandle) Get1Resource ('STR ',
														 rHighScore + i);
			SetResInfo (myStringHandle, rHighScore + i + 1, NULL);
			WriteResource (myStringHandle);
			
			myStringHandle = (StringHandle) Get1Resource ('STR ',
										rHighScore + 2 * HIGHSCORE_ITEMS + i);
			SetResInfo (myStringHandle, rHighScore + i + 1 + 2 * HIGHSCORE_ITEMS, 
						NULL);
			WriteResource (myStringHandle);

		}			
	}
		
	if ((i == -1) && (points < newPoints))
		i ++;
	else
		i += 2;

	myNameHandle = NewString (newName);
	if (myNameHandle)
	{
		memcpy (*myNameHandle, newName, (*newName) + 1);
		AddResource (myNameHandle, 'STR ', rHighScore + i, NULL);
		WriteResource (myNameHandle);
	}
	
	NumToString ((long) newPoints, myPointsString);
	myPointsHandle = NewString (myPointsString);
	if (myPointsHandle)
	{
		memcpy (*myPointsHandle, myPointsString, (*myPointsString) + 1);
		AddResource (myPointsHandle, 'STR ',
					 rHighScore + HIGHSCORE_ITEMS + i, NULL);
		WriteResource (myPointsHandle);
	}
	
	NumToString ((long) gLevel, myLevelString);
	myLevelHandle = NewString (myLevelString);
	if (myLevelHandle)
	{
		memcpy (*myLevelHandle, myLevelString, (*myLevelString) + 1);
		AddResource (myLevelHandle, 'STR ',
					 rHighScore + 2 * HIGHSCORE_ITEMS + i, NULL);
		WriteResource (myLevelHandle);
	}
	
	if (myPointsHandle)
		DetachResource (myPointsHandle);
	
	if (myNameHandle)
		DetachResource (myNameHandle);
	
	if (myStringHandle)
		DetachResource (myStringHandle);
		
	if (myLevelHandle)
		DetachResource (myLevelHandle);
	
	UseResFile (gApplRefNum);
	
	return;
}

/* ------------------------------------------------------------------------ */
/* InsertToHallOfFame zeigt den HallOfFame-Dialog und trägt den Namen des	*/
/* Spielers an der entsprechenden Stelle ein								*/
/* ------------------------------------------------------------------------ */

Boolean InsertToHallOfFame (void)
{
    short       	itemHit, i;
    DialogPtr   	theDialog;
    Str255			myName, myPoints;
    StringHandle	myStringHandle, myNameHandle, myPointsHandle;
    int				HighScore;

    EndOfGameSound ();
    
    theDialog = GetNewDialog (rCongratulation, NULL, (WindowPtr) -1);
    
    FlushEvents (everyEvent, 0);
    
    SelIText (theDialog, iMyName, 0, 32767);
	
    do 
    {
        ModalDialog (NULL, &itemHit);
    } 
    while (itemHit != OK_BUTTON);
    
    GetDText (theDialog, iMyName, myName);
    
    DisposeDialog (theDialog);
    
    ModifyWinnersList (myName, gHighScore);
    
    theDialog = GetNewDialog (rHallOfFameDialog, NULL, (WindowPtr) -1);
    
    UseResFile (gPrefsRefNum);
    
    for (i = 0; i < HIGHSCORE_ITEMS; i ++)
    {
		SetHighScoreLine (i, theDialog);
    }
    
    UseResFile (gApplRefNum);

	ShowWindow (theDialog);

    do 
    {
        ModalDialog (NULL, &itemHit);
    } 
    while ((itemHit != ENDE_BUTTON) && (itemHit != NOCHMAL_BUTTON));

    DisposeDialog (theDialog);
    
    if (itemHit == ENDE_BUTTON)
    	return TRUE;
    else
    	return FALSE;
}


/* ------------------------------------------------------------------------ */
/* GameOverDialog zeigt ein Dialog-Window und liefert TRUE zurueck, wenn der*/
/* OK-Button gedrueckt wurde. Wurde der Nochmal-Button gedrückt, erfolgt	*/
/* die Rückkehr mit FALSE zurück.											*/
/* ------------------------------------------------------------------------ */

Boolean GameOverDialog (void)
{
    EndOfGameSound ();

    if (ShowMessage (rEndOfGameDialog, ENDE_BUTTON, 
    				NOCHMAL_BUTTON) == ENDE_BUTTON)
    	return TRUE;
    else
    	return FALSE;
}


/* ------------------------------------------------------------------------ */
/* SetKeyItem gibt im aktivierten Item den text aus und setzt die neue 		*/
/* Tastenzuordnung															*/
/* ------------------------------------------------------------------------ */

void SetKeyItem (DialogPtr theDialog, char theChar, Str255 text)
{
	SetDText (theDialog, gActiveItem, text);
	SelIText (theDialog, gActiveItem, 0, 32767);
	
	switch (gActiveItem)
	{
		case KEY_UP_ITEM:	
			gKeyUp = theChar;
			break;
			
		case KEY_DOWN_ITEM:
			gKeyDown = theChar;
			break;
			
		case KEY_LEFT_ITEM:
			gKeyLeft = theChar;
			break;
			
		case KEY_RIGHT_ITEM:
			gKeyRight = theChar;
			break;
	}
	
}


/* ------------------------------------------------------------------------ */
/* RefModalFilter übernimmt die Sonderbehandlung von Funktionstasten bei	*/
/* Eingabe der gewünschten Tastenbelegung									*/
/* ------------------------------------------------------------------------ */

static pascal Boolean RefModalFilter(DialogPtr theDialog, 
									EventRecord *theEvent, 
									short *theItem)
{
	unsigned char	theChar;
	Boolean returnFlag;
	long	finalTicks;
	Str255	myString;
	
	returnFlag = FALSE;
	
	switch (theEvent -> what)
	{
		case keyDown:
		case autoKey:
			theChar = (theEvent -> message) & charCodeMask;
			
			switch (theChar)
			{
				case CR_CHAR:							/* Carriage Return	*/				
				case ENTER_CHAR:						/* Enter			*/
					*theItem = OK_BUTTON;
					returnFlag = TRUE;
					HiliteControl (GetDControl (theDialog, OK_BUTTON), 
									OK_BUTTON);
					Delay (HILITE_DELAY, &finalTicks);
					HiliteControl (GetDControl (theDialog, OK_BUTTON), 0);					
					break;
				
				case ESC_CHAR:							/* ESC				*/
					*theItem = CANCEL_BUTTON;
					returnFlag = TRUE;
					HiliteControl (GetDControl (theDialog, CANCEL_BUTTON), 
									CANCEL_BUTTON);
					Delay (HILITE_DELAY, &finalTicks);
					HiliteControl (GetDControl (theDialog, CANCEL_BUTTON), 0);					
					break;
				
				case UP_CHAR:							/* UpArrow			*/
				case RIGHT_CHAR:						/* RightArrow		*/
				case DOWN_CHAR:							/* DownArrow		*/
				case LEFT_CHAR:							/* LeftArrow		*/
					GetKeyString (myString, theChar);
					SetKeyItem (theDialog, theChar, myString);
					returnFlag = TRUE;
					break;
									
				default:
					SelIText (theDialog, gActiveItem, 0, 32767);
		
					if (theChar != TAB_CHAR)	/* TAB wird nicht genutzt */
					{
						switch (gActiveItem)
						{
							case KEY_UP_ITEM:	
								gKeyUp = theChar;
								break;
			
							case KEY_DOWN_ITEM:
								gKeyDown = theChar;
								break;
			
							case KEY_LEFT_ITEM:
								gKeyLeft = theChar;
								break;
			
							case KEY_RIGHT_ITEM:
								gKeyRight = theChar;
								break;
						}
					}
					break;
			}
		
		default:
			break;
	}
	
	return returnFlag;
}


/* ------------------------------------------------------------------------ */
/* GetKeyString wandelt den uebergebenen char-Code in einen entsprechenden	*/
/* Textstring (mit Uebersetzung der Zeichen unter 0x20)						*/
/* ------------------------------------------------------------------------ */

void GetKeyString (Str255 myString, char myChar)
{
	int i;
	StringHandle		myStringHandle;

	if (isgraph (myChar))							/* "normales Zeichen" 	*/
	{
		*myString = 1;
		*(myString + 1) = myChar; 
	}
	else if (iscntrl (myChar))						/* Steuerzeichen		*/
	{
		myStringHandle = (StringHandle) Get1Resource ('STR ', 
														BASE_RES_ID+myChar);
		if (myStringHandle)
		{
			for (i = 0; i <= **myStringHandle; i ++)
			{
				*(myString + i) = *(*myStringHandle + i);
			}
		}
	}
}


/* ------------------------------------------------------------------------ */
/* SetCurrentKeyStrings traegt die aktuellen Tastenbezeichnungen in die		*/
/* jeweiligen Dialog-Items ein												*/
/* ------------------------------------------------------------------------ */

void SetCurrentKeyStrings (DialogPtr theDialog)
{
	Str255	myString;
	
	GetKeyString (myString, gKeyUp);
	SetDText (theDialog, KEY_UP_ITEM, myString);
	GetKeyString (myString, gKeyDown);
	SetDText (theDialog, KEY_DOWN_ITEM, myString);	
	GetKeyString (myString, gKeyLeft);
	SetDText (theDialog, KEY_LEFT_ITEM, myString);	
	GetKeyString (myString, gKeyRight);
	SetDText (theDialog, KEY_RIGHT_ITEM, myString);
}
 
/* ------------------------------------------------------------------------ */
/* Do_KeyPrefs erledigt die Einstellung der Steuertasten für die Bewegungen */
/* des Runners sowie Sound- und Darstellungsumschaltung. Ist ein Spielneu-	*/
/* start notwendig, wird TRUE zurueckgegeben								*/
/* ------------------------------------------------------------------------ */

Boolean Do_KeyPrefs (void)
{	
	short 			itemHit;
	DialogPtr		theDialog;
	int				itemType;
	Rect			itemRect;
	Handle			itemHandle;
	char			leftBack, rightBack, upBack, downBack;
	Boolean			picturesBack, soundBack;
	kbPrefsHandle	myKbHandle;
		
	leftBack		= gKeyLeft;
	rightBack		= gKeyRight;
	upBack			= gKeyUp;
	downBack		= gKeyDown;
	picturesBack	= gPatternOffset;
	soundBack		= gSoundOn;
	
	theDialog = GetNewDialog (rKeyPrefsDialog, NULL, MOVE_TO_FRONT);	

	SetCurrentKeyStrings (theDialog);
	
	SetItemValue (theDialog, SOUND_ITEM, gSoundOn);
	
	if (gPatternOffset == PICTURES_OFFSET)
		SetItemValue (theDialog, PICTURES_ITEM, 1);
	else
		SetItemValue (theDialog, PICTURES_ITEM, 0);
	
	gActiveItem = KEY_UP_ITEM;

	SelIText (theDialog, gActiveItem, 0, 32767);
																	
	do
	{
		ModalDialog (RefModalFilter, &itemHit);
		
		switch (itemHit)								
		{
			case KEY_UP_ITEM:
			case KEY_RIGHT_ITEM:
			case KEY_DOWN_ITEM:
			case KEY_LEFT_ITEM:
				gActiveItem = itemHit;		
				SelIText (theDialog, itemHit, 0, 32767);
				break;
				
			case SOUND_ITEM:
				gSoundOn = 1 - gSoundOn;
				SetItemValue (theDialog, SOUND_ITEM, gSoundOn);
				break;
				
			case PICTURES_ITEM:
				if (gPatternOffset == PICTURES_OFFSET)
				{
					gPatternOffset = NO_PICTURES_OFFSET;
					SetItemValue (theDialog, PICTURES_ITEM, 1);
				}
				else
				{
					gPatternOffset = PICTURES_OFFSET;
					SetItemValue (theDialog, PICTURES_ITEM, 0);
				}
	
				LoadFiguresPattern ();

				break;
		}                                        
													
	}
	while ((itemHit != CANCEL_BUTTON) && (itemHit != OK_BUTTON));
	
	if (itemHit == CANCEL_BUTTON)				/* Evtl. Aenderungen verwerfen	*/
	{
		gKeyLeft		= leftBack;
		gKeyRight		= rightBack;
		gKeyUp			= upBack;
		gKeyDown		= downBack;
		gPatternOffset	= picturesBack;
		gSoundOn		= soundBack;
	}
	
	UseResFile (gPrefsRefNum);
	
	myKbHandle = (kbPrefsHandle) Get1Resource ('KBRS', rKeyboardPrefs);
	    
    (**myKbHandle).upKey	= gKeyUp;
    (**myKbHandle).downKey	= gKeyDown;
    (**myKbHandle).rightKey	= gKeyRight;
    (**myKbHandle).leftKey	= gKeyLeft;
    (**myKbHandle).pauseKey	= gKeyPause;
    
    ChangedResource (myKbHandle);
    WriteResource (myKbHandle);

	UseResFile (gApplRefNum);
	
	DisposeDialog (theDialog);
	
	if (picturesBack == gPatternOffset)			/* "Bilder" nicht geaendert	*/
		return FALSE;
	else
		return TRUE;
}

/* ------------------------------------------------------------------------ */
/* ShowMessage zeigt einen modalen Dialog entsprechend der uebergebenen		*/
/* Resource-ID und liefert die Item-Nr. des aktivierten Items zum Beenden	*/																
/* ------------------------------------------------------------------------ */

short ShowMessage (short dialogID, short endItem1, short endItem2)
{
    short       itemHit;
    DialogPtr   theDialog;
    
    theDialog = GetNewDialog (dialogID, NULL, (WindowPtr) -1);
                                            
    do 
    {
        ModalDialog (NULL, &itemHit);
    } 
    while ((itemHit != endItem1) && (itemHit != endItem2));

    DisposeDialog (theDialog);
    
    return itemHit;
}

/* ------------------------------------------------------------------------ */
/* RunnerDieSound spielt einen Sound, wenn ein Runner geopfert wurde		*/
/* ------------------------------------------------------------------------ */

void RunnerDieSound (void)
{
	Handle	soundHandle;
	
	if (gSoundOn && (soundHandle = GetResource ('snd ', rRunnerDieSound)))
		SndPlay (0L, soundHandle, FALSE);
}


/* ------------------------------------------------------------------------ */
/* EndOfGameSound spielt einen Sound, wenn alle Runner verspielt wurden		*/
/* ------------------------------------------------------------------------ */

void EndOfGameSound (void)
{
	Handle	soundHandle;
	
	if (gSoundOn && (soundHandle = GetResource ('snd ', rEndOfGameSound)))
		SndPlay (0L, soundHandle, FALSE);
}


/* ------------------------------------------------------------------------ */
/* Das Hauptprogramm beinhaltet die Main-Event-Loop, welche solange laeuft, */
/* bis der Benutzer das Fenster schliesst und dadurch das Terminations-     */
/* kriterium gesetzt wird.                                                  */
/* ------------------------------------------------------------------------ */
            
void main (void)
    
{   
    Initialize ();                          /* Initialisierung.             */

    while (!gQuit)                          /* Main-Event-Loop.             */
    {
        if (gRun)                           /* Laeuft ein Spiel?            */
            Animate ();                     /* Alle Figuren bewegen         */

        gGotEvent = WaitNextEvent (everyEvent,  /* Wir nehmen jeden Event.  */
                                    &gEvent,    /* Hier steht hinterher     */
                                                /* der Event drin.          */
                                    3,          /* 2 Ticks fuer Hinter-     */
                                                /* grundapplikationen.      */
                                    NULL);      /* Noch keine Mouse-Region  */
                                                /* (MouseMoved-Events).     */
        if (gGotEvent)                          /* Liegt ein Event vor?     */
            Do_Event ();                        /* Sorge fuer Abarbeitung   */
        
        if (gEndOfGame == TRUE)					/* QUIT gedrückt oder Player*/
        {										/* sind alle				*/
        
        	if (CheckHighScore () == TRUE)		/* Unter den ersten 10 ?	*/
        	{
        		gQuit = InsertToHallOfFame ();	/* Eintragen in Hall Of Fame*/
        	}
        	else
        	{
        		gQuit = GameOverDialog ();		/* Es hat nicht gereicht	*/
        	}
        	
        	if (gQuit == FALSE)
        	{
        		Do_New ();						/* Versuchen wir´s nochmal	*/
        	}
        }			
    }    
}   


/* ------------------------------------------------------------------------	*/
/* ExitXonix beendet Xonix und gibt gegebenenfalls eine notwendige Message	*/
/* aus																		*/
/* ------------------------------------------------------------------------	*/

void ExitXonix (int status)
{
	switch (status)
	{
		case 1:									/* Speicher reicht nicht	*/
				ShowMessage (rNoRamDialog, OK_BUTTON, OK_BUTTON);
				break;
				
		default:
				break;
	}
	
	if (gMyStatusArea)
		DisposePtr (gMyStatusArea);
	
	if (gMyGWorld)
		DisposePtr (gMyGWorld);	
		
	ExitToShell ();
}

#endif /* USE_MAC */
