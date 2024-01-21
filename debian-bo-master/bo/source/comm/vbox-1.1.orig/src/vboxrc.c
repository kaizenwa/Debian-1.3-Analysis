/*
 * $Id: vboxrc.c,v 1.4 1996/07/19 08:33:53 root Exp $
 *
 * $Log: vboxrc.c,v $
 * Revision 1.4  1996/07/19 08:33:53  root
 * o Neue Option für die Sektion [CALLERID] eingebaut.
 *
 * Revision 1.3  1996/07/16 15:21:11  root
 * o Funktion zum auswerten der Tage in .vboxrc eingebaut.
 *
 * Revision 1.2  1996/07/15 10:02:50  root
 * o Kleine Änderungen an den Headern.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <pwd.h>
#include <unistd.h>
#include <ctype.h>
#include <time.h>
#include <fnmatch.h>

#include "vboxrc.h"
#include "vboxgetty.h"
#include "log.h"
#include "settings.h"

/** Prototypes ***********************************************************/

static boolean ReturnLine(char *, int);
static boolean GotoSection(char *);
static boolean ParseTime(char *);
static boolean ParseDays(char *);

/** Variables ************************************************************/

static FILE *VBoxRC	= NULL;
static long	 LineRC	= -1;

static struct DebugModes Debugging[] =
{
	{ "FATAL"	, L_FATAL	},
	{ "ERROR"	, L_ERROR	},
	{ "ERRORS"	, L_ERROR	},
	{ "WARN"		, L_WARN		},
	{ "WARNING"	, L_WARN		},
	{ "WARNINGS", L_WARN		},
	{ "INFO"		, L_INFO		},
	{ "INFOS"	, L_INFO		},
	{ "DEBUG"	, L_DEBUG	},
	{ "JUNK"		, L_JUNK		},
	{ NULL		, -1			}
};

static char *Weekday[] =
{
	"SO" , "MO" , "DI" , "MI" , "DO" , "FR" , "SA" ,
	"SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT",

	NULL
};


/*************************************************************************
 ** OpenVBoxRC():																			**
 *************************************************************************/

FILE *OpenVBoxRC(uid_t UID)
{
	struct passwd *Pass;

	char File[PATH_MAX + 1];

	if (!VBoxRC)
	{
		if ((Pass = getpwuid(UID)))
		{
			strncpy(File, Pass->pw_dir, PATH_MAX);
			strncat(File, "/", PATH_MAX - strlen(File));
			strncat(File, VBOXRC, PATH_MAX - strlen(File));

			if ((VBoxRC = fopen(File, "r")))
			{
				LineRC = 0;

				LogLine(L_DEBUG, "User's vboxrc \"%s\" opened.\n", File);
			}
			else LogLine(L_WARN, "Can't open \"%s\".\n", File);
		}
		else LogLine(L_ERROR, "Can't get passwd entry for uid %ld.\n", UID);
	}
	else
	{
		LogLine(L_DEBUG, "Rewinding \"%s\".\n", File);

		rewind(VBoxRC);

		LineRC = 0;
	}

	return(VBoxRC);
}

/*************************************************************************
 ** CloseVBoxRC():																		**
 *************************************************************************/

void CloseVBoxRC(void)
{
	if (VBoxRC)
	{
		fclose(VBoxRC);
		
		VBoxRC = NULL;
		LineRC = -1;
	}
}

/*************************************************************************
 ** ReturnLine():	Gibt eine Zeile aus dem vboxrc zurück. Leere Zeilen	**
 **					oder Kommentarzeilen werden überlesen. Kommentare am	**
 **					Ende der Zeile werden entfernt. Rückgabe ist TRUE,		**
 **					wenn sich eine gültige Zeile im Buffer befindet, oder	**
 **					FALSE.																**
 *************************************************************************/

static boolean ReturnLine(char *Line, int Len)
{
	char *Comment;
	int	i;

	if (VBoxRC)
	{
		while (fgets(Line, Len, VBoxRC))
		{
			LineRC++;

			Line[strlen(Line) - 1] = 0;

			if ((*Line == 0) || (*Line == '#')) continue;

			if ((Comment = strchr(Line, '#'))) *Comment = 0;

			if (strlen(Line) > 0)
			{
				for (i = (strlen(Line) - 1); i >= 0; i--)
				{
					if (!isspace(Line[i])) break;

					Line[i] = 0;
				}
			}

			True();
		}
	}

	False();
}

/*************************************************************************
 ** GotoSection():	Spoolt das vboxrc bis zu einer bestimmten Sektion.	**
 *************************************************************************/

static boolean GotoSection(char *Need)
{
	char Line[MAXVBOXRCLINELEN + 1];

	while (ReturnLine(Line, MAXVBOXRCLINELEN))
	{
		if (strcasecmp(Line, Need) == 0) True();
	}

	False();
}

/*************************************************************************
 ** ParseTime():	Zerlegt einen String mit einzelnen Zeitangaben und		**
 **					untersuche, ob die aktuelle Stunde im angegebenen		**
 **					Rahmen liegt. Rückgabe ist TRUE oder FALSE.				**
 *************************************************************************/

static boolean ParseTime(char *Time)
{
	struct tm *Localtime;

	char		 TimeFlags[24];
	int	 	 i;
	int		 b;
	int		 e;
	char		*Beg;
	char		*End;
	time_t	 Currenttime;

	if (strcmp(Time, "*") == 0) True();
	if (strcmp(Time, "-") == 0) False();

	for (i = 0; i < 24; i++) TimeFlags[i] = 0;

	Currenttime = time(NULL);

	if (!(Localtime = localtime(&Currenttime)))
	{
		LogLine(L_ERROR, "Can't get localtime()...\n");
		
		False();
	}

	for (i = 0; i < strlen(Time); i++)
	{
		if ((!isdigit(Time[i])) && (Time[i] != ',') && (Time[i] != '-') && (Time[i] != ';'))
		{
			LogLine(L_ERROR, "Error in time string \"%s\" in line #%ld.\n", Time, LineRC);

			False();
		}
	}

	Beg = strtok(Time, ",;");

	while (Beg)
	{
		if ((End = strchr(Beg, '-')))
		{
			*End++ = 0;

			if ((*End == 0) || (*Beg == 0))
			{
				LogLine(L_ERROR, "Bad time range in line #%ld (%s-%s).\n", LineRC, Beg, End);

				False();
			}
		}
		else End = Beg;

		b = atoi(Beg);
		e = atoi(End);

		if ((b < 0) || (b > 23) || (e < 0) || (e > 23))
		{
			LogLine(L_ERROR, "Bad time range in line #%ld (%s-%s).\n", LineRC, Beg, End);

			False();
		}

		if (b <= e)
		{
			LogLine(L_JUNK, "Time range %d-%d (%s-%s).\n", b, e, Beg, End);

			for (i = b; i <= e; i++) TimeFlags[i] = 1;
		}
		else
		{
			LogLine(L_JUNK, "1th time range %d-%d (%s-%s).\n", b, 23, Beg, End);
			LogLine(L_JUNK, "2nd time range %d-%d (%s-%s).\n", 0,  e, Beg, End);

			for (i = b; i < 24; i++) TimeFlags[i] = 1;
			for (i = 0; i <= e; i++) TimeFlags[i] = 1;
		}

		Beg = strtok(NULL, ",:");
	}

	if (TimeFlags[Localtime->tm_hour] == 1) True();

	False();
}

/*************************************************************************
 ** ParseDays():	Zerlegt einen String mit Tagesangaben und untersucht	**
 **					ob der aktuelle Tag in diesem Rahmen liegt.				**
 *************************************************************************/

static boolean ParseDays(char *Days)
{
	struct tm *Localtime;

	int	 	 i;
	int		 d;
	char		*Beg;
	time_t	 Currenttime;

	if (strcmp(Days, "*") == 0) True();
	if (strcmp(Days, "-") == 0) False();

	Currenttime = time(NULL);

	if (!(Localtime = localtime(&Currenttime)))
	{
		LogLine(L_ERROR, "Can't get localtime()...\n");
		
		False();
	}

	for (i = 0; i < strlen(Days); i++)
	{
		if ((!isalpha(Days[i])) && (Days[i] != ',') && (Days[i] != ';'))
		{
			LogLine(L_ERROR, "Error in day string \"%s\" in line #%ld.\n", Days, LineRC);

			False();
		}
	}

	Beg = strtok(Days, ",;");

	while (Beg)
	{
		d = 0;
		i = 0;

		while (Weekday[i])
		{
			if (strcasecmp(Weekday[i], Beg) == 0)
			{
				if (d == Localtime->tm_wday)
				{
					LogLine(L_JUNK, "Weekday \"%s\" [%d] (current is %d; match).\n", Beg, d, Localtime->tm_wday);

					True();
				}
				else
				{
					LogLine(L_JUNK, "Weekday \"%s\" [%d] (current is %d; don't match).\n", Beg, d, Localtime->tm_wday);
				}
			}

			d++;
			i++;
			
			if (d >= 7) d = 0;
		}

		Beg = strtok(NULL, ",:");
	}

	False();
}

/*************************************************************************
 ** GetRingsToWait():	Liefert die Anzahl der RING's zurück, die be-	**
 **							nötigt werden, damit der Anruf beantwortet		**
 **							wird. Bei einem Fehler wird 0 zurückgegeben.		**
 *************************************************************************/

long GetRingsToWait(uid_t UID)
{
	char	Line[MAXVBOXRCLINELEN + 1];
	long	Need;
	char *Time;
	char *Ring;
	char *Days;
	char *Stop;

	Need = 0;

	if (OpenVBoxRC(UID))
	{
		LogLine(L_DEBUG, "Searching for number of rings to answer phone...\n");

		if (GotoSection("[RINGS]"))
		{
			while (ReturnLine(Line, MAXVBOXRCLINELEN))
			{
				if ((*Line == '[') || (*Line == ']')) break;

				Time = strtok(Line, "\t ");
				Days = strtok(NULL, "\t ");
				Ring = strtok(NULL, "\t ");

					/* Fix für die neue Konfiguration: Wenn die 3. Spalte	*/
					/* nicht angegeben ist werden keine Tage ausgewertet!	*/

				if ((!Ring) && (Days))
				{
					LogLine(L_WARN, "The configuration was changed since version 1.0 - Please update your %s!\n", VBOXRC);

					Ring = Days;
					Days = "*";
				}

				if ((!Time) || (!Ring) || (!Days))
				{
					LogLine(L_ERROR, "Error in vboxrc line #%ld.\n", LineRC);
					
					continue;
				}

				LogLine(L_JUNK, "Test time \"%s\" and days \"%s\" (%s rings)...\n", Time, Days, Ring);

				if ((ParseTime(Time)) && (ParseDays(Days)))
				{
					Need = (long)strtoul(Ring, &Stop, 10);

					if (*Stop != 0)
					{
						LogLine(L_ERROR, "Bad number of rings in line #%ld.\n", LineRC);

						Need = 0;
					}
					else break;
				}
			}
		}
		else LogLine(L_WARN, "Section \"[RINGS]\" not found.\n");

		CloseVBoxRC();
	}

	if (Need <= 0)
	{
		LogLine(L_WARN, "Number of rings not found - Call will not be answered.\n");
	}
	else LogLine(L_DEBUG, "Call will be answered after %ld rings...\n", Need);

	return(Need);
}

/*************************************************************************
 ** GetNameFromCallerID():	Trägt den Besitzernamen einer CallerID oder	**
 **								"*** Unknown ***" in den Buffer ein.			**
 *************************************************************************/

void GetNameFromCallerID(uid_t UID, char *ID, char *Goto, char *Name, int Len)
{
	char	Line[MAXVBOXRCLINELEN + 1];
	char *Dummy;
	char *Owner;
	char *OwnID;
	char *OwnAS;

	strncpy(Name, "*** Unknown ***", Len);
	strncpy(Goto, "Standard", Len);

	if (OpenVBoxRC(UID))
	{
		LogLine(L_DEBUG, "Searching owner for CallerID \"%s\"...\n", ID);

		if (GotoSection("[CALLERID]"))
		{
			while (ReturnLine(Line, MAXVBOXRCLINELEN))
			{
				if ((*Line == '[') || (*Line == ']')) break;

				Dummy = Line;
				OwnID	= NULL;
				OwnAS = NULL;
				Owner = NULL;

				while (isspace(*Dummy)) Dummy++;

				if ((*Dummy) && (Dummy)) OwnID = strsep(&Dummy, "\t ");

				while (isspace(*Dummy)) Dummy++;

				if ((*Dummy) && (OwnID)) OwnAS = strsep(&Dummy, "\t ");

				while (isspace(*Dummy)) Dummy++;

				if ((*Dummy) && (OwnAS)) Owner = Dummy;

				if ((!Owner) || (!OwnAS) || (!OwnID))
				{
					LogLine(L_ERROR, "Error in vboxrc line #%ld.\n", LineRC);

					continue;
				}

				if (fnmatch(OwnID, ID, FNM_NOESCAPE | FNM_PERIOD) == 0)
				{
					strncpy(Name, Owner, Len);
					strncpy(Goto, OwnAS, Len);

					if (*Goto == '*') strncpy(Goto, Name, Len);
					if (*Goto == '-') strncpy(Goto, "Standard", Len);
					
					break;
				}
			}
		}
		else LogLine(L_WARN, "Section \"[CALLERID]\" not found.\n");

		CloseVBoxRC();
	}

	LogLine(L_DEBUG, "Found owner \"%s\" [%s].\n", Name, Goto);
}

/*************************************************************************
 ** GetOwnerSettings():																	**
 *************************************************************************/

boolean GetOwnerSettings(uid_t UID, char *Goto, char *Owner, struct UserSection *Section)
{
	char	Temp[MAXVBOXRCLINELEN + 4];
	char	Line[MAXVBOXRCLINELEN + 1];
	long	Secs;
	char *Time;
	char *Days;
	char *Text;
	char *Strg;
	char *Stop;
	char	Correct = FALSE;

	GetUserSectionDefaults(Section);

	if (OpenVBoxRC(UID))
	{
		LogLine(L_DEBUG, "Parsing settings for \"%s\" [%s]...\n", Owner, Goto);

		sprintf(Temp, "[%s]", Goto);

		if (GotoSection(Temp))
		{
			while (ReturnLine(Line, MAXVBOXRCLINELEN))
			{
				if ((*Line == '[') || (*Line == ']')) break;

				Time = strtok(Line, "\t ");
				Days = strtok(NULL, "\t ");
				Text = strtok(NULL, "\t ");
				Strg = strtok(NULL, "\t ");

				if ((!Time) || (!Text) || (!Strg) || (!Days))
				{
					LogLine(L_ERROR, "Error in vboxrc line #%ld.\n", LineRC);

					continue;
				}

				LogLine(L_JUNK, "Test time \"%s\" and days \"%s\" (%s; %s secs)...\n", Time, Days, Text, Strg);

				if ((ParseTime(Time)) && (ParseDays(Days)))
				{
					Secs = (long)strtoul(Strg, &Stop, 10);

					if ((*Stop != 0) || (Secs < 0))
					{
						LogLine(L_ERROR, "Bad record time in line #%ld.\n", LineRC);

						break;
					}

					Section->Recordtime = Secs;

					strncpy(Section->StandardMessage, Text, NAME_MAX);

					while ((Text = strtok(NULL, "\t ")))
					{
						LogLine(L_JUNK, "Found Flag \"%s\"...\n", Text);

						if (strcasecmp(Text, "NOANSWER"    ) == 0) Section->Answer = FALSE;
						if (strcasecmp(Text, "NORECORD"    ) == 0) Section->Record = FALSE;
						if (strcasecmp(Text, "NOTIMEOUTMSG") == 0) Section->PlayTimeout = FALSE;
						if (strcasecmp(Text, "NOBEEPMSG"   ) == 0) Section->PlayBeep = FALSE;
						if (strcasecmp(Text, "NOSTDMSG"    ) == 0) Section->PlayMessage = FALSE;
					}

					Correct = TRUE;

					break;
				}
			}
		}
		else LogLine(L_WARN, "Section \"[%s]\" not found.\n", Goto);

		CloseVBoxRC();
	}

	if (!Correct) LogLine(L_WARN, "No (or not all) settings found - using defaults...\n");

	LogLine(L_DEBUG, "Settings: Message \"%s\".\n", Section->StandardMessage);
	LogLine(L_DEBUG, "Settings: Record time %ld seconds.\n", Section->Recordtime);
	
	if (!Section->Answer     ) LogLine(L_DEBUG, "Settings: Don't answer the call.\n");
	if (!Section->Record     ) LogLine(L_DEBUG, "Settings: Don't record a message.\n");
	if (!Section->PlayMessage) LogLine(L_DEBUG, "Settings: Don't play the standard message.\n");
	if (!Section->PlayBeep   ) LogLine(L_DEBUG, "Settings: Don't play the beep message.\n");
	if (!Section->PlayTimeout) LogLine(L_DEBUG, "Settings: Don't play the timeout message.\n");

	return(Correct);
}

/*************************************************************************
 ** GetUserSectionDefaults():															**
 *************************************************************************/

void GetUserSectionDefaults(struct UserSection *Section)
{
	Section->Recordtime	= DEFAULT_RECORD_TIME;
	Section->Answer		= TRUE;
	Section->Record		= TRUE;
	Section->PlayMessage	= TRUE;
	Section->PlayBeep		= TRUE;
	Section->PlayTimeout	= TRUE;

	strncpy(Section->StandardMessage, "standard.msg", NAME_MAX);
}


/*************************************************************************
 ** GetDebugMode():																		**
 *************************************************************************/

int GetDebugMode(uid_t UID)
{
	char	Line[MAXVBOXRCLINELEN + 1];
	char *Word;
	int	Level;
	int	i;

	Level = GetLogLevel();

	if (OpenVBoxRC(UID))
	{
		LogLine(L_DEBUG, "Searching debug levels...\n");

		if (GotoSection("[DEBUG]"))
		{
			Level = L_FATAL;

			while (ReturnLine(Line, MAXVBOXRCLINELEN))
			{
				if ((*Line == '[') || (*Line == ']')) break;

				Word = strtok(Line, "\t +|");
				
				while (Word)
				{
					i = 0;
					
					while (Debugging[i].Name)
					{
						if (strcasecmp(Debugging[i].Name, Word) == 0)
						{
							LogLine(L_JUNK, "Found debug level \"%s\" (%d).\n", Debugging[i].Name, Debugging[i].Mode);

							Level |= Debugging[i].Mode;

							break;
						}

						i++;
					}

					Word = strtok(NULL, "\t +|");
				}
			}
		}
		else LogLine(L_WARN, "Section \"[DEBUG]\" not found.\n");

		CloseVBoxRC();
	}

	return(Level);
}
