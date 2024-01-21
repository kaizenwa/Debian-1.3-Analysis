/* $Id: vbox.h,v 1.1 1996/06/27 15:58:09 root Exp $ */
 
#ifndef _VBOX_H
#define _VBOX_H 1

#include <sys/types.h>
#include <unistd.h>
#include <limits.h>

/** Defines **************************************************************/

#define AttrSet						wattrset
#define Print							mvwprintw
#define Update							wrefresh
#define HidePanel						hide_panel
#define ShowPanel						show_panel
#define Free							free
#define Keypress						ungetch

#define Clear(X)						{ wclear(X); wrefresh(X); }

#define MAX_OPEN_PANELS				(10)

#define MAXNAMELEN					(20)
#define MAXCALLERIDLEN				(20)
#define MAXUSERNAMELEN				(32)
#define MAXCOMMANDLEN				(10240)

#define VBOX_MODE_LIST				(0)
#define VBOX_MODE_TIMER				(1)

#define VOICE_COMPRESSION_BASE	963.75		  /* Basis für Kompression	*/
#define VOICE_COMPRESSION_RATE	8000				 /* Basis bei ULAW/ALAW	*/

/** Variables ************************************************************/

struct MessageLine
{
	char		New;													/* Neue Nachricht	*/
	char		Del;															/* Gelöscht	*/
	char		Date[14];									/* Datum (YYMMDDMMHHSS)	*/
	char		Time[6];									/* Aufgezeichnete Sekunden	*/
	char		Name[MAXNAMELEN + 1];						/* Name des Anrufers	*/
	char		ID[MAXCALLERIDLEN + 1];						  /* ID des Anrufers	*/
	char		Filename[NAME_MAX + 1];				/* Dateiname der Nachricht	*/
};

#endif /* _VBOX_H */
