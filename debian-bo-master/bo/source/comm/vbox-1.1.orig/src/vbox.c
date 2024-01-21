/*
 * $Id: vbox.c,v 1.3 1996/07/19 08:53:31 root Exp $
 *
 * $Log: vbox.c,v $
 * Revision 1.3  1996/07/19 08:53:31  root
 * o Kleine �nderungen.
 *
 * Revision 1.2  1996/07/15 10:02:50  root
 * o Kleine �nderungen an den Headern.
 *
 */

#include <ncurses.h>
#include <panel.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <getopt.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <pwd.h>
#include <dirent.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <signal.h>
#include <netinet/in.h>

#include "vbox.h"
#include "settings.h"
#include "log.h"
#include "vboxrc.h"
#include "defines.h"
#include "voice.h"
#include "utils.h"

/** Prototypes ***********************************************************/

static char		*GetMessageList(void);
static void		 FreeMessageList(void);
static long		 GetNumMessages(void);
static void	 	 PrintMessageList(WINDOW *, int, int);
static void	 	 PrintMessageLine(WINDOW *, int, long, char);
static void		 PrintNumMessages(WINDOW *);
static void		 DropPermissions(uid_t, gid_t);
static void		 DrawMainScreen(void);
static void		 CalculateTime(char *, size_t, char *);
static void		 Exit(int);
static int	 	 Main(void);
static int		 TimerMain(void);
static void		 ListviewPoint(void);
static void		 PrintStatus(void);
static void		 ActivateParkMode(void);
static void		 SetVolume(int);
static void	 	 StopFunc(int);
static void		 UserFunc(int);
static void		 TogglePower(void);
static void		 TimerTogglePower(WINDOW *, int);
static void		 Beep(void);
static void		 PlayMessage(struct MessageLine *);
static void		 Information(struct MessageLine *);
static void		 Usage(void);
static void		 DrawDigit(WINDOW *, int, unsigned char);
static void		 Help(void);
static void		 HelpTimer(void);
static void		 HelpTimeout(WINDOW *, int, int);
static void		 DeleteMessages(void);

static int		 InitNCurses(char *Device);
static void		 ExitNCurses(void);
static void		 CreateColorTable(void);
static PANEL	*OpenPanel(int, int, int, int, chtype, chtype);
static void		 ClosePanel(PANEL *);
static void		 HLine(WINDOW *, int, int, chtype, int);
static void		 VLine(WINDOW *, int, int, chtype, int);
static void		 Window(WINDOW *, int, int, int, int, chtype, chtype);
static chtype	 Color(int);
static void		 Scroll(WINDOW *, int);
static void		 Button(WINDOW *, int, int, char *);

/** Variables ************************************************************/

static char		*MessageList			= NULL;
static long		 NumMessages			= 0;
static char		 RedrawMessageList	= FALSE;
static time_t	 IncomingTimeM			= 0;
static time_t	 IncomingTimeC			= 0;
static char		*Basename				= NULL;
static char		 Mono						= FALSE;
static int		 RescanTimer			= 0;
static int		 VBoxMode				= VBOX_MODE_LIST;
static int		 CurrListPosY			= 0;
static long		 CurrListLine			= 0;
static float	 ListviewPointPosY	= 0;
static char		 ViewMode				= FALSE;
static char		 NoQuit					= FALSE;
static char		 ParkMode				= FALSE;
static int		 Volume					= 10;

static char MessageDir[PATH_MAX + 1];
static char UserName[MAXUSERNAMELEN + 1];
static char	UserHome[PATH_MAX + 1];

static char *MonthNames[] =
{
	"???", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
	"Sep", "Oct", "Nov", "Dec"
};

static char *NumberTable[] =
{
	"1-34567", "----5-7", "123-56-", "123-5-7", "-2-45-7",
	"1234--7", "1234-67", "1---5-7", "1234567", "12345-7"
};

static struct option Arguments[] =
{
	{ "device"		, required_argument, NULL, 'd' },
	{ "username"	, required_argument, NULL, 'u' },
	{ "update"		, required_argument, NULL, 'r' },
	{ "volume"		, required_argument, NULL, 'v' },
	{ "mono"			, no_argument		 , NULL, 'm' },
	{ "timer"		, no_argument		 , NULL, 't' },
	{ "viewmode"	, no_argument		 , NULL, 'o' },
	{ "noquit"     , no_argument      , NULL, 'q' },
	{ "help"			, no_argument		 , NULL, 'h' },
	{ NULL			, 0					 , NULL,  0  }
};

static FILE		*NCursesO	= NULL;
static FILE		*NCursesI	= NULL;
static SCREEN	*Init			= NULL;

static PANEL	*Panels[MAX_OPEN_PANELS];

/*************************************************************************
 ** The magic main...																	**
 *************************************************************************/

void main(int argc, char **argv)
{
	struct passwd *User;

	char	 Device[NAME_MAX + 1];
	uid_t	 UID;
	gid_t  GID;
	int	 Opts;

	Basename = argv[0];
	ViewMode	= FALSE;
	VBoxMode	= VBOX_MODE_LIST;
	NoQuit	= FALSE;

	UID = getuid();
	GID = getgid();

	*Device		= 0;
	*MessageDir	= 0;

	while ((Opts = getopt_long(argc, argv, "u:d:r:v:hmtoq", Arguments, (int *)0)) != EOF)
	{
		switch (Opts)
		{
			case 'q':
				NoQuit = TRUE;
				break;

			case 'o':
				ViewMode = TRUE;
				break;

			case 't':
				VBoxMode = VBOX_MODE_TIMER;
				break;

			case 'r':
				RescanTimer = atoi(optarg);
				break;

			case 'v':
				Volume = atoi(optarg);
				break;

			case 'm':
				Mono = TRUE;
				break;

			case 'u':
				if ((UID == 0) && (GID == 0))
				{
					if ((User = getpwnam(optarg)))
					{
						UID = User->pw_uid;
						GID = User->pw_gid;

						ViewMode = TRUE;
					}
					else
					{
						fprintf(stderr, "%s: can't switch to user %s.\n", Basename, optarg);
						
						Exit(10);
					}
				}
				else
				{
					fprintf(stderr, "%s: only root can switch to other users.\n", Basename);
					
					Exit(10);
				}

				break;

			case 'd':
				strncpy(Device, optarg, NAME_MAX);
				break;

			case 'h':
			default:
				Usage();
				break;
		}
	}

	if ((User = getpwuid(UID)))
	{
		strncpy(UserName, User->pw_name, MAXUSERNAMELEN);
		strncpy(UserHome, User->pw_dir, PATH_MAX);

		strcpy(MessageDir, SPOOLDIR);
		strcat(MessageDir, "/");
		strcat(MessageDir, User->pw_name);
		strcat(MessageDir, "/incoming");
	}
	else
	{
		fprintf(stderr, "%s: can't get settings for uid %d.\n", Basename, UID);

		Exit(10);
	}

	if (chdir(MessageDir) != 0)
	{
		fprintf(stderr, "%s: can't change to directory '%s'.\n", Basename, MessageDir);
		
		Exit(5);
	}

	if (!*Device) strcpy(Device, "/dev/tty");

	if (RescanTimer < 0) RescanTimer = 0;

	if ((Volume < 0) || (Volume > 100)) Volume = 10;

	if (InitNCurses(Device))
	{
		signal(SIGQUIT, SIG_IGN);
		signal(SIGINT , SIG_IGN);

		DropPermissions(UID, GID);

		while (TRUE)
		{
			if (VBoxMode == VBOX_MODE_LIST)
			{
				FreeMessageList();
				DrawMainScreen();
				GetMessageList();

				if (Main() == -1) break;

				VBoxMode = VBOX_MODE_TIMER;
			}
			else
			{
				FreeMessageList();
				GetNumMessages();

				if (TimerMain() == -1) break;

				VBoxMode = VBOX_MODE_LIST;
			}
		}
	}

	ExitNCurses();
	Exit(0);
}

/*************************************************************************
 ** Exit():	Beendet das Programm.													**
 *************************************************************************/

static void Exit(int RC)
{
	FreeMessageList();

	exit(RC);
}

/*************************************************************************
 ** Information():	Zeigt Informationen �ber eine Nachricht an.			**
 *************************************************************************/

static void Information(struct MessageLine *Line)
{
	struct VoiceHeader Header;
	struct stat Status;

	char *CompressionName[] =
	{
		"Unknown", "Unknown", "ADPCM-2", "ADPCM-3", "ADPCM-4", "ALAW", "ULAW"
	};

	PANEL *Panel;
	int	 FD;
	int	 Compression;

	int W = 49;
	int H = 13;

	if (ViewMode)
	{
		Beep();
		
		return;
	}

	if (stat(Line->Filename, &Status) != 0) return;
	
	if ((FD = open(Line->Filename, O_RDONLY)) == -1) return;

	if (read(FD, &Header, sizeof(struct VoiceHeader)) != sizeof(struct VoiceHeader))
	{
		CloseFD(FD);
		
		return;
	}

	CloseFD(FD);

	Compression = (int)ntohs(Header.Compression);

	if ((Compression < 2) || (Compression > 6)) Compression = 0;

	if ((Panel = OpenPanel(-1, -1, W, H, Color(4) | A_BOLD, Color(5) | A_BOLD)))
	{
		AttrSet(Panel->win, Color(4) | A_BOLD);

		Print(Panel->win, 1, 2, "Number     : %-30.30s", Line->ID);
		Print(Panel->win, 2, 2, "Caller     : %-30.30s", Line->Name);
		Print(Panel->win, 3, 2, "Date       : %2.2s-%2.2s-%2.2s %2.2s:%2.2s:%2.2s", &Line->Date[4], &Line->Date[2], &Line->Date[0], &Line->Date[6], &Line->Date[8], &Line->Date[10]);
		Print(Panel->win, 4, 2, "Record time: %5.5s", Line->Time);
		Print(Panel->win, 5, 2, "Filename   : %-30.30s", Line->Filename);
		Print(Panel->win, 6, 2, "Compression: %s", CompressionName[Compression]);
		Print(Panel->win, 7, 2, "Type       : %-30.30s", Header.Modem);
		Print(Panel->win, 8, 2, "Size       : %ld", Status.st_size);

		Button(Panel->win, 21, H - 3, "  _Ok  ");

		HelpTimeout(Panel->win, W, H);
		ClosePanel(Panel);
	}
}

/*************************************************************************
 ** Help():	Listenhilfe.																**
 *************************************************************************/

static void Help(void)
{
	PANEL *Panel;

	int W = 46;
	int H = 16;
	
	if ((Panel = OpenPanel(-1, -1, W, H, Color(4) | A_BOLD, Color(5) | A_BOLD)))
	{
		AttrSet(Panel->win, Color(4) | A_BOLD);

		Print(Panel->win,  1, 2, "RETURN .................... Play message");
		Print(Panel->win,  2, 2, "D ...... Mark message as delete/undelete");
		Print(Panel->win,  3, 2, "N .......... Mark message as read/unread");
		Print(Panel->win,  4, 2, "R .................. Rescan message list");
		Print(Panel->win,  5, 2, "+/- ..................... Volume up/down");
		Print(Panel->win,  6, 2, "S .................. Answer calls on/off");
		Print(Panel->win,  7, 2, "I .................. Message information");
		Print(Panel->win,  8, 2, "P .............. Activate line park mode");
		Print(Panel->win,  9, 2, "T/L ...................... Timer display");
		Print(Panel->win, 10, 2, "H ................................. Help");
		Print(Panel->win, 11, 2, "Q ................................. Quit");

		Button(Panel->win, 20, H - 3, "  _Ok  ");

		HelpTimeout(Panel->win, W, H);
		ClosePanel(Panel);
	}
}

/*************************************************************************
 ** HelpTimer():	Timerhilfe.															**
 *************************************************************************/

static void HelpTimer(void)
{
	PANEL *Panel;

	int W = 35;
	int H = 11;
	
	if ((Panel = OpenPanel(-1, -1, W, H, Color(4) | A_BOLD, Color(5) | A_BOLD)))
	{
		AttrSet(Panel->win, Color(4) | A_BOLD);

		Print(Panel->win, 1, 2, "R ....... Rescan message list");
		Print(Panel->win, 2, 2, "S ....... Answer calls on/off");
		Print(Panel->win, 3, 2, "P ... Activate line park mode");
		Print(Panel->win, 4, 2, "L .............. List display");
		Print(Panel->win, 5, 2, "H ...................... Help");
		Print(Panel->win, 6, 2, "Q ...................... Quit");

		Button(Panel->win, 14, H - 3, "  _Ok  ");

		HelpTimeout(Panel->win, W, H);
		ClosePanel(Panel);
	}
}

/*************************************************************************
 ** HelpTimeout():	Timeoutfunktion f�r die Hilfsfenster.					**
 *************************************************************************/

static void HelpTimeout(WINDOW *Win, int W, int H)
{
	int Timer;
	int K;

	cbreak();
	noecho();
	scrollok(Win, FALSE);
	keypad(Win, TRUE);
	wtimeout(Win, 1000);
	curs_set(0);

	AttrSet(Win, Color(4) | A_BOLD);
	Update(Win);

	Timer = 90;

	while (Timer > 0)
	{
		switch ((K = wgetch(Win)))
		{
			case ERR:
				Print(Win, H - 2, W - 6, "%2d", Timer--);
				Update(Win);
				break;

			case 'O':
			case 'o':
			case '\r':
			case '\n':
				Timer = 0;
				break;

			default:
				Timer = 90;
				break;
		}
	}
}

/*************************************************************************
 ** TimerMain():	Hauptschleife (Timer).											**
 *************************************************************************/

static int TimerMain(void)
{
	PANEL *Panel;
	int	 Done;
	int	 K;
	int	 W = 26;

	Clear(stdscr);

	if (!(Panel = OpenPanel(-1, -1, W, 14, Color(1) | A_BOLD, Color(1) | A_BOLD))) return(-1);

	cbreak();
	noecho();
	scrollok(Panel->win, FALSE);
	keypad(Panel->win, TRUE);
	curs_set(0);

	if (RescanTimer > 0) wtimeout(Panel->win, RescanTimer * 1000);

	if ((strlen(UserName) + 2) <= W)
	{
		AttrSet(Panel->win, Color(1) | A_BOLD);

		Print(Panel->win, 0, ((W - 2 - strlen(UserName)) / 2), " %s ", UserName);
	}

	TimerTogglePower(Panel->win, FALSE);

	Done = 0;

	while (Done == 0)
	{
		PrintNumMessages(Panel->win);

		switch ((K = wgetch(Panel->win)))
		{
			case 'S':
			case 's':
				TimerTogglePower(Panel->win, TRUE);
				break;

			case 'H':
			case 'h':
				HelpTimer();
				break;
				
			case 27:
				K = wgetch(Panel->win);
				break;

			case 'R':
			case 'r':
			case ERR:
				GetNumMessages();
				break;

			case 'P':
			case 'p':
				ActivateParkMode();
				break;
			
			case 'Q':
			case 'q':
				if (!NoQuit) Done = -1;
				break;

			case 'L':
			case 'l':
				Done = 1;
				break;
		}
	}

	ClosePanel(Panel);

	return(Done);
}

/*************************************************************************
 ** TimerTogglePower():																	**
 *************************************************************************/

static void TimerTogglePower(WINDOW *Win, int Toggle)
{
	if (Toggle) TogglePower();

	if (FileExist(UserHome, ".vboxstop"))
	{
		AttrSet(Win, Color(10) | A_REVERSE | A_BOLD);

		Print(Win, 12, 2, " OFF ");
	}
	else
	{
		AttrSet(Win, Color(1) | A_BOLD);

		HLine(Win, 2, 12, ACS_HLINE, 5);
	}

	Update(Win);
}

/*************************************************************************
 ** DrawDigit():	Zeichnet eine Nummer zwischen 0 und 9.						**
 *************************************************************************/

static void DrawDigit(WINDOW *Win, int X, unsigned char Number)
{
	char *Table	= NULL;
	int	Digit;
	int	Y;
	int	i;

	Digit = (int)(Number - '0');
	
	if ((Digit < 0) || (Digit > 9)) Digit = 0;

	Table = NumberTable[Digit];

	for (i = 0; i < strlen(Table); i++)
	{
		Y = 1;

		switch (Table[i])
		{
			case '1':	HLine(Win, X + 2, Y     , ACS_BLOCK, 5);
							break;

			case '2':	HLine(Win, X + 2, Y +  5, ACS_BLOCK, 5);
							break;

			case '3':	HLine(Win, X + 2, Y + 10, ACS_BLOCK, 5);
							break;
					
			case '4':	VLine(Win, X    , Y + 1, ACS_BLOCK, 4);
							VLine(Win, X + 1, Y + 1, ACS_BLOCK, 4);
							break;

			case '5':	VLine(Win, X + 7, Y + 1, ACS_BLOCK, 4);
							VLine(Win, X + 8, Y + 1, ACS_BLOCK, 4);
							break;

			case '6':	VLine(Win, X    , Y + 6, ACS_BLOCK, 4);
							VLine(Win, X + 1, Y + 6, ACS_BLOCK, 4);
							break;

			case '7':	VLine(Win, X + 7, Y + 6, ACS_BLOCK, 4);
							VLine(Win, X + 8, Y + 6, ACS_BLOCK, 4);
							break;
		}
	}
}

/*************************************************************************
 ** PrintNumMessages():	Gibt Anzahl der ungelesenen Nachrichten aus.		**
 *************************************************************************/

static void PrintNumMessages(WINDOW *Win)
{
	int	Messages;
	char	Number[4];

	if (RedrawMessageList)
	{
		Messages = NumMessages;

		if (Messages <  0) Messages = 0;
		if (Messages > 99) Messages = 99;

		sprintf(Number, "%02d", Messages);

		if (!Mono)
		{
			AttrSet(Win, Color(3) | A_BOLD);
		
			DrawDigit(Win, 02, '8');
			DrawDigit(Win, 13, '8');
		}

		AttrSet(Win, Color(6) | A_BOLD);
		
		if (Number[0] != '0') DrawDigit(Win, 02, Number[0]);
		if (Number[1] != '*') DrawDigit(Win, 13, Number[1]);

		Update(Win);
	}

	RedrawMessageList = FALSE;
}

/*************************************************************************
 ** DeleteMessages():																	**
 *************************************************************************/

static void DeleteMessages(void)
{
	struct MessageLine *Line;

	PANEL *Panel;
	long	 i;
	long	 j;
	int	 K;
	int	 Timer;

	for (i = 0, j = 0; i < NumMessages; i++)
	{
		Line = (struct MessageLine *)(MessageList + (sizeof(struct MessageLine) * i));

		if (Line->Del) j++;
	}
	
	if ((!j) || (ViewMode)) return;

	Panel = OpenPanel(-1, -1, 32, 6, Color(4) | A_BOLD, Color(5) | A_BOLD);

	if (!Panel) return;

	cbreak();
	noecho();
	scrollok(Panel->win, FALSE);
	keypad(Panel->win, TRUE);
	wtimeout(Panel->win, 1000);
	curs_set(0);

	Button(Panel->win,  3, 3, "  _Yes  ");
	Button(Panel->win, 21, 3, "  _No  ");

	AttrSet(Panel->win, Color(4) | A_BOLD);

	Print(Panel->win, 1, 2, "Delete all marked entries?");

	Update(Panel->win);

	Timer	= 90;

	while (Timer > 0)
	{
		switch ((K = wgetch(Panel->win)))
		{
			case ERR:
				Print(Panel->win, 4, 26, "%2d", Timer--);
				Update(Panel->win);
				break;

			case 'N':
			case 'n':
			case '\r':
			case '\n':
				Timer = 0;
				break;

			case 'Y':
			case 'y':
				for (i = 0; i < NumMessages; i++)
				{
					Line = (struct MessageLine *)(MessageList + (sizeof(struct MessageLine) * i));

					if (Line->Del) DeleteFile(Line->Filename, NULL);
				}

				Timer = 0;

				break;
		}
	}

	ClosePanel(Panel);
}

/*************************************************************************
 ** Main():	Hauptschleife (Liste)													**
 *************************************************************************/

static int Main(void)
{
	struct MessageLine *Line;

	PANEL *Panel;
	int	 K;
	int	 W;
	int	 H;
	int	 Done = 0;
	
	H = LINES - 9;
	W = COLS - 10;

	if (!(Panel = OpenPanel(4, 5, W, H, 0, 0))) return(-1);

	cbreak();
	noecho();
	wsetscrreg(Panel->win, 1, H);
	scrollok(Panel->win, FALSE);
	keypad(Panel->win, TRUE);
	curs_set(0);

	CurrListPosY = 0;
	CurrListLine = 0;

	HidePanel(Panel);
	PrintMessageList(Panel->win, W, H);
	ShowPanel(Panel);

	if (RescanTimer > 0) wtimeout(Panel->win, RescanTimer * 1000);

	StopFunc(-1);
	UserFunc(-1);

	if (FileExist(UserHome, ".vboxpark")) ActivateParkMode();

	while (Done == 0)
	{
		PrintMessageList(Panel->win, W, H);

		Line = (struct MessageLine *)(MessageList + (sizeof(struct MessageLine) * CurrListLine));

		switch ((K = wgetch(Panel->win)))
		{
			case 27:
				K = wgetch(Panel->win);
				break;
				
			case 'H':
			case 'h':
				Help();
				break;
				
			case 'I':
			case 'i':
				if ((NumMessages > 0) && (MessageList)) Information(Line);
				break;

			case KEY_DOWN:
				if (CurrListLine < (NumMessages - 1))
				{
					if (CurrListPosY < H - 1)
					{
						PrintMessageLine(Panel->win, CurrListPosY++, CurrListLine++, FALSE);
						PrintMessageLine(Panel->win, CurrListPosY  , CurrListLine  , TRUE );
					}
					else
					{
						PrintMessageLine(Panel->win, CurrListPosY, CurrListLine++, FALSE);
						Scroll(Panel->win, 1);
						PrintMessageLine(Panel->win, CurrListPosY, CurrListLine, TRUE);
					}
				}
				break;

			case KEY_UP:
				if (CurrListLine > 0)
				{
					if (CurrListPosY > 0)
					{
						PrintMessageLine(Panel->win, CurrListPosY--, CurrListLine--, FALSE);
						PrintMessageLine(Panel->win, CurrListPosY  , CurrListLine  , TRUE );
					}
					else
					{
						PrintMessageLine(Panel->win, CurrListPosY, CurrListLine--, FALSE);
						Scroll(Panel->win, -1);
						PrintMessageLine(Panel->win, CurrListPosY, CurrListLine, TRUE);
					}
				}
				break;

			case 'P':
			case 'p':
				ActivateParkMode();
				break;
				
			case 'R':
			case 'r':
			case ERR:
				GetMessageList();
				break;

			case '+':
			case '-':
				SetVolume(K);
				break;

			case 'S':
			case 's':
				TogglePower();
				break;

			case '\r':
			case '\n':
				if ((NumMessages > 0) && (MessageList))
				{
					PlayMessage(Line);
					PrintMessageLine(Panel->win, CurrListPosY, CurrListLine, TRUE);
				}
				break;

			case 'T':
			case 't':
			case 'L':
			case 'l':
				Done = 1;
				break;
			
			case 'Q':
			case 'q':
				if (!NoQuit)
				{
					Done = -1;
					DeleteMessages();
				}
				else Beep();
				break;

			case 'D':
			case 'd':
				if ((!ViewMode) && (NumMessages > 0) && (MessageList))
				{
					Line->Del = (Line->Del ? FALSE : TRUE);

					PrintMessageLine(Panel->win, CurrListPosY, CurrListLine, TRUE);
					Keypress(KEY_DOWN);
				}
				else Beep();

				break;

			case 'N':
			case 'n':
				if ((!ViewMode) && (NumMessages > 0) && (MessageList))
				{
					if (Line->New)
					{
						chmod(Line->Filename, S_IRUSR | S_IWUSR | S_IXUSR);
						chmod(Line->Filename, S_IRUSR | S_IWUSR | S_IXUSR);

						Line->New = FALSE;
					}
					else
					{
						chmod(Line->Filename, S_IRUSR | S_IWUSR);
						chmod(Line->Filename, S_IRUSR | S_IWUSR);

						Line->New = TRUE;
					}

					PrintMessageLine(Panel->win, CurrListPosY, CurrListLine, TRUE);
					Keypress(KEY_DOWN);
				}
				else Beep();

				break;
		}
	}

	ClosePanel(Panel);

	return(Done);
}

/*************************************************************************
 ** ActivateParkMode():	Aktiviert den Parkmodus.								**
 *************************************************************************/

static void ActivateParkMode(void)
{
	PANEL *Panel;
	int	 K;
	int	 Timer;

	if (!TouchFile(UserHome, ".vboxpark")) return;

	ParkMode = TRUE;

	if (VBoxMode == VBOX_MODE_LIST) PrintStatus();

	Panel = OpenPanel(-1, -1, 34, 6, Color(4) | A_BOLD, Color(5) | A_BOLD);

	if (Panel)
	{
		Button(Panel->win, 12, 3, " _Cancel ");

		AttrSet(Panel->win, Color(4) | A_BOLD);
		
		Print(Panel->win, 1, 2, "Line parking mode activated!");

		cbreak();
		noecho();
		keypad(Panel->win, TRUE);
		curs_set(0);
		wtimeout(Panel->win, 1000);

		Timer	= 90;

		while (Timer > 0)
		{
			switch ((K = wgetch(Panel->win)))
			{
				case ERR:
					Print(Panel->win, 4, 28, "%2d", Timer--);
					Update(Panel->win);

					if (!FileExist(UserHome, ".vboxpark")) Timer = 0;

					break;

				case 'C':
				case 'c':
				case '\r':
				case '\n':
					Timer = 0;
					break;
			}
		}

		ClosePanel(Panel);
	}

	DeleteFile(UserHome, ".vboxpark");

	ParkMode = FALSE;

	if (VBoxMode == VBOX_MODE_LIST) PrintStatus();
}

/*************************************************************************
 ** PrintMessageList():	Zeigt Liste mit Nachrichten an.						**
 *************************************************************************/

static void PrintMessageList(WINDOW *Win, int W, int H)
{
	int i;

	if (RedrawMessageList)
	{
		AttrSet(Win, Color(8));

		for (i = 0; i < H; i++) HLine(Win, 0, i, ' ', COLS - 10);

		if ((MessageList) && (NumMessages > 0))
		{
			for (i = 0; i < NumMessages; i++)
			{
				if (i >= H) break;

				PrintMessageLine(Win, i, i, FALSE);
			}

			CurrListPosY = 0;
			CurrListLine = 0;

			PrintMessageLine(Win, CurrListPosY, CurrListLine, TRUE);
		}

		PrintStatus();
	}

	RedrawMessageList = FALSE;
}

/*************************************************************************
 ** PrintMessageLine():	Zeigt einen Eintrag der Liste normal oder sel-	**
 **							ektiert an.													**
 *************************************************************************/

static void PrintMessageLine(WINDOW *Win, int Y, long E, char Selected)
{
	struct MessageLine *Line;

	char	MS[4];
	int	MN;

	AttrSet(Win, (Selected ? (Color(10) | A_REVERSE) : Color(8)) | A_BOLD);

	HLine(Win, 0, Y, ' ', COLS - 10);

	Line = (struct MessageLine *)(MessageList + (sizeof(struct MessageLine) * E));

	sprintf(MS, "%2.2s", &Line->Date[2]);
	
	MN = atoi(MS);
	
	if ((MN < 1) || (MN > 12)) MN = 0;

	if (Line->New) Print(Win, Y, 0, "N");
	if (Line->Del) Print(Win, Y, 0, "D");

	Print(Win, Y,  2, "%2.2s-%3.3s-%2.2s", &Line->Date[4], MonthNames[MN], &Line->Date[ 0]);
	Print(Win, Y, 12, "%2.2s:%2.2s:%2.2s", &Line->Date[6], &Line->Date[8], &Line->Date[10]);

	Print(Win, Y, 22, "%5.5s", Line->Time);
	Print(Win, Y, 29, "%-20.20s", Line->Name);
	Print(Win, Y, 50, "%20.20s", Line->ID);

	if (Selected)
	{
		ListviewPoint();

		Update(Win);
	}
}

/*************************************************************************
 ** SetVolume():	Setzt die Lautst�rke zum spielen der Nachrichten.		**
 *************************************************************************/

static void SetVolume(int K)
{
	if (!ViewMode)
	{
		if ((K == '+') && (Volume < 100))
		{
			Volume++;
			
			PrintStatus();
		}

		if ((K == '-') && (Volume > 0))
		{
			Volume--;
		
			PrintStatus();
		}
	}
	else Beep();
}

/*************************************************************************
 ** PrintStatus():	Zeigt Statusmeldungen an (Messages, Viewmode...).	**
 *************************************************************************/

static void PrintStatus(void)
{
	char Temp[80];

	if (VBoxMode != VBOX_MODE_LIST) return;

	AttrSet(stdscr, Color(9) | A_BOLD);
	
	HLine(stdscr, 3, LINES - 2, ACS_HLINE, COLS - 6);
	HLine(stdscr, 3, 2, ACS_HLINE, COLS - 6);

	if (ParkMode)
	{
		AttrSet(stdscr, Color(10) | A_REVERSE | A_BOLD | A_BLINK);
		
		Print(stdscr, LINES - 2, 4, " PARKMODE ");
	}

	if (ViewMode)
	{
		AttrSet(stdscr, Color(10) | A_REVERSE | A_BOLD);
		
		Print(stdscr, 2, 4, " VIEWMODE ");
	}

	if (FileExist(UserHome, ".vboxstop"))
	{
		AttrSet(stdscr, Color(10) | A_REVERSE | A_BOLD);

		Print(stdscr, 2, COLS - 9, " OFF ");
	}

	AttrSet(stdscr, Color(11) | A_BOLD);

	HLine(stdscr, 4, LINES - 3, ' ', COLS - 8);

	sprintf(Temp, "MESSAGES: %ld", NumMessages);

	Print(stdscr, LINES - 3, COLS - 4 - strlen(Temp), "%s", Temp);

	if (!ViewMode)
	{
		Print(stdscr, LINES - 3, 4, "VOLUME:            %d", Volume);

		AttrSet(stdscr, Color(1) | A_REVERSE);
	
		HLine(stdscr, 12, LINES - 3, ACS_BOARD, 10);
		HLine(stdscr, 12, LINES - 3, ACS_BLOCK, (Volume / 10));
	}

	Update(stdscr);
}

/*************************************************************************
 ** DropPermissions():	Setzt die Permissions auf einen Benutzer zu-		**
 **							r�ck.															**
 *************************************************************************/

static void DropPermissions(uid_t UID, gid_t GID)
{
	setuid(UID);
	setgid(GID);
	seteuid(UID);
	setegid(GID);
}

/*************************************************************************
 ** CalculateTime():	Berechnet die Aufnahmezeit anhand der Dateil�nge.	**
 *************************************************************************
 ** Bei der Kalkulation wird davon ausgegangen, da� ca. jedes 257. Byte	**
 ** ein DLE ist. Das ergibt eine Basis von 963.75 f�r die kleinste Kom-	**
 ** pression (CELP).																		**
 *************************************************************************/

static void CalculateTime(char *Name, size_t Size, char *Time)
{
	struct VoiceHeader Header;

	int	FD;
	short Compression;
	float MsgBase;
	int	Secs;
	int	Hour;
	int	Mins;

	strcpy(Time, "??:??");

	if ((FD = open(Name, O_RDONLY)))
	{
		if (read(FD, &Header, sizeof(struct VoiceHeader)) == sizeof(struct VoiceHeader))
		{
			Compression = ntohs(Header.Compression);

			switch (Compression)
			{
				case 2:
					MsgBase = VOICE_COMPRESSION_BASE * 2;			 /* ADPCM-2	*/
					break;
					
				case 3:
					MsgBase = VOICE_COMPRESSION_BASE * 3;			 /* ADPCM-3	*/
					break;
				
				case 4:
					MsgBase = VOICE_COMPRESSION_BASE * 4;			 /* ADPCM-4	*/
					break;
				
				default:
					MsgBase = VOICE_COMPRESSION_RATE;			/* ULAW & ALAW	*/
					break;
			}

			Secs = (int)(Size / MsgBase);

			if (Secs <    0) Secs = 0;							 /* 00:00 Minuten	*/
			if (Secs > 5999) Secs = 5999;						 /* 99:59 Minuten	*/

			Hour = (Secs / 60);
			Mins = Secs - (Hour * 60);

			sprintf(Time, "%02d:%02d", Hour, Mins);
		}

		close(FD);
	}
}

/*************************************************************************
 ** DrawMainScreen():	Zeichnet den Hauptbildschirm.							**
 *************************************************************************/

static void DrawMainScreen(void)
{
	int	i;
	char	Title[74];

	wclear(stdscr);

	AttrSet(stdscr, Color(2));

	for (i = 0; i < LINES; i++) HLine(stdscr, 0, i, ACS_BOARD, COLS);

	AttrSet(stdscr, Color(1) | A_REVERSE);
	
	HLine(stdscr, 0, 0, ' ', COLS);

	if (strlen(MessageDir) > 49)
	{
		strcpy(Title, "...");
		strcat(Title, &MessageDir[(strlen(MessageDir) - 46)]);
	}
	else strcpy(Title, MessageDir);

	Print(stdscr, 0, 1, "VBox Version %s", VERSION);
	Print(stdscr, 0, COLS - 8 - strlen(Title), "%s", Title);
	HLine(stdscr, COLS - 7, 0, ACS_VLINE, 1);
	HLine(stdscr, COLS - 10 - strlen(Title), 0, ACS_VLINE, 1);

	Button(stdscr, COLS - 5, 0, "_Help");

	Window(stdscr, 2, 2, COLS - 2, LINES - 2, Color(8) | A_BOLD, Color(9) | A_BOLD);

	AttrSet(stdscr, Color(11) | A_BOLD);

	Print(stdscr, 3, 4, "* DATE                 TIME  CALLER");
	Print(stdscr, 3, COLS - 12, "NUMBER %%");

	HLine(stdscr, 4, 4, ACS_HLINE, COLS - 8);
	HLine(stdscr, 4, LINES - 4, ACS_HLINE, COLS - 8);

	AttrSet(stdscr, Color(1) | A_REVERSE);

	VLine(stdscr, COLS - 5, 5, ACS_BOARD, LINES - 9);
	HLine(stdscr, COLS - 5, 5, ACS_UARROW, 1);
	HLine(stdscr, COLS - 5, LINES - 5, ACS_DARROW, 1);
}

/*************************************************************************
 ** LogLine(): 																 [Dummy] **
 *************************************************************************/

void LogLine(int Level, char *Fmt, ...)
{
}

/*************************************************************************
 ** SetLogLevel():															 [Dummy] **
 *************************************************************************/

void SetLogLevel(int Level)
{
}

/*************************************************************************
 ** GetLogLevel():															 [Dummy] **
 *************************************************************************/

int GetLogLevel(void)
{
	return(0);
}

/*************************************************************************
 ** Beep():																					**
 *************************************************************************/

static void Beep(void)
{
	beep();
	wrefresh(stdscr);
}

/*************************************************************************
 ** StopFunc():																			**
 *************************************************************************/

static void StopFunc(int Sig)
{
	if (Sig != -1)
	{
		ExitNCurses();
		Exit(Sig);
	}

	alarm(0);
	signal(SIGALRM , SIG_IGN);
	signal(SIGINT	, SIG_IGN);
	signal(SIGQUIT	, SIG_IGN);
	signal(SIGHUP	, StopFunc);
	signal(SIGTERM	, StopFunc);
}

/*************************************************************************
 ** UserFunc():																			**
 *************************************************************************/

static void UserFunc(int Nr)
{
	if (Nr != -1)
	{
		signal(SIGUSR1	, SIG_IGN);
		signal(SIGUSR2 , SIG_IGN);
		signal(SIGHUP	, SIG_IGN);

		switch (Nr)
		{
			case SIGUSR1:
				ViewMode = TRUE;
				break;
			
			case SIGUSR2:
				ViewMode = FALSE;
				break;

			case SIGHUP:
				ActivateParkMode();
				break;
		}

		PrintStatus();
	}

	signal(SIGUSR1	, UserFunc);
	signal(SIGUSR2 , UserFunc);
	signal(SIGHUP	, UserFunc);
}

/*************************************************************************
 ** Usage():																				**
 *************************************************************************/

static void Usage(void)
{
	fprintf(stderr, "\nVBox Version %s - (C) 1996 Michael 'Ghandi' Herold\n\n", VERSION);
	fprintf(stderr, "Usage: %s [ OPTION ] [ OPTION ] [ OPTION ] [ ... ]\n\n", Basename);
	fprintf(stderr, "-d, --device NAME     Device NAME for input/output (default /dev/tty).\n");
	fprintf(stderr, "-u, --username NAME   Shows messages for user NAME (force viewmode).\n");
	fprintf(stderr, "-r, --update SECS     Updates the display of messages all SECS seconds.\n");
	fprintf(stderr, "-m, --mono            Switchs to mono display.\n");
	fprintf(stderr, "-o, --viewmode        Forces viewmode (no commands allowed).\n");
	fprintf(stderr, "-t, --timer           Forces timer (counter) display.\n");
	fprintf(stderr, "-q, --noquit          Disables the quit function.\n");
	fprintf(stderr, "-v, --volume VOL      Sets default volume to VOL (range 0-100).\n\n");

	Exit(5);
}

/*************************************************************************
 ** SortMessageList():	Sortierfunktion f�r qsort().							**
 *************************************************************************/

static int SortMessageList(const void *A, const void *B)
{
	struct MessageLine *LineA = (struct MessageLine *)A;
	struct MessageLine *LineB = (struct MessageLine *)B;

	return(strcmp(LineB->Date, LineA->Date));
}

/*************************************************************************
 ** FreeMessageList():	Gibt die Liste mit Nachrichten wieder frei.		**
 *************************************************************************/

static void FreeMessageList(void)
{
	if (MessageList) Free(MessageList);
	
	MessageList			= NULL;
	NumMessages			= 0;
	IncomingTimeM		= 0;
	IncomingTimeC		= 0;
	RedrawMessageList	= TRUE;
}

/*************************************************************************
 ** GetMessageList():	Durchsucht das Incoming-Verzeichnis und erstel-	**
 **							lt eine Liste der Nachrichten.						**
 *************************************************************************
 ** [Return]				Zeiger auf Speicher der die Liste enth�lt oder	**
 **							NULL.															**
 *************************************************************************/

static char *GetMessageList(void)
{
	struct dirent *DirEnt;
	struct stat Status;
	struct MessageLine *Line;

	char	 Goto[MAXNAMELEN + 1];

	char	*List			= NULL;
	char	*Temp			= NULL;
	char	*Name			= NULL;
	DIR	*Dir			= NULL;
	long   Messages	= 0;

		/* Untersuchen, ob sich seit dem letzten Zugriff auf das	*/
		/* Verzeichniss etwas ge�ndert hat.								*/

	if (stat(MessageDir, &Status) != 0) return(MessageList);

	if (Status.st_mtime == IncomingTimeM)
	{
		if (Status.st_ctime == IncomingTimeC) return(MessageList);
	}

	IncomingTimeM = Status.st_mtime;
	IncomingTimeC = Status.st_ctime;

		/* Incoming-Verzeichnis durchsuchen und alle passenden Nach-	*/
		/* richten in die neue Liste eintragen.								*/

	if (!(Dir = opendir(MessageDir))) return(MessageList);

	while ((DirEnt = readdir(Dir)))
	{
		Name = DirEnt->d_name;

		if ((strcmp(Name, ".") != 0) && (strcmp(Name, "..") != 0))
		{
			if ((isdigit(*Name)) && (Name[12] == '-'))
			{
				if (stat(Name, &Status) == 0)
				{
					if (Status.st_mode & S_IRUSR)
					{
						if ((Temp = realloc(List, (sizeof(struct MessageLine) * (Messages + 1)))))
						{
							List = Temp;
							Line = (struct MessageLine *)(List + (sizeof(struct MessageLine) * Messages));

							strncpy(Line->Date, Name, 12);
							strncpy(Line->Name, "*** Unknown ***", MAXNAMELEN);
							strncpy(Line->ID, &Name[13], MAXCALLERIDLEN);
							strncpy(Line->Filename, Name, NAME_MAX);

							Line->New = TRUE;
							Line->Del = FALSE;
							
							if (Status.st_mode & S_IXUSR) Line->New = FALSE;

							GetNameFromCallerID(getuid(), Line->ID, Goto, Line->Name, MAXNAMELEN);

							CalculateTime(Line->Filename, Status.st_size, Line->Time);

							Messages++;
						}
					}
					else
					{
						IncomingTimeM = 0;
						IncomingTimeC = 0;
					}
				}
			}
		}
	}

	closedir(Dir);

		/* Wenn die neue Liste erzeugt werden konnte, wird die alte	*/
		/* freigegeben und durch die neue ersetzt.						*/

	if (MessageList) Free(MessageList);

	MessageList			= List;
	NumMessages			= Messages;
	RedrawMessageList	= TRUE;

	qsort(List, Messages, sizeof(struct MessageLine), SortMessageList);

	return(MessageList);
}

/*************************************************************************
 ** GetNumMessagest():	Ermittelt die Anzahl der ungelesenen Nachricht-	**
 **							en.															**
 *************************************************************************/

static long GetNumMessages(void)
{
	struct dirent *DirEnt;
	struct stat Status;

	char *Name		= NULL;
	DIR  *Dir		= NULL;
	long	Messages = 0;

		/* Untersuchen, ob sich seit dem letzten Zugriff auf das	*/
		/* Verzeichniss etwas ge�ndert hat.								*/

	if (stat(MessageDir, &Status) != 0) return(NumMessages);

	if (Status.st_mtime == IncomingTimeM)
	{
		if (Status.st_ctime == IncomingTimeC) return(NumMessages);
	}

	IncomingTimeM = Status.st_mtime;
	IncomingTimeC = Status.st_ctime;

		/* Incoming-Verzeichnis durchsuchen und alle passenden Nach-	*/
		/* richten in die neue Liste eintragen.								*/

	if (!(Dir = opendir(MessageDir))) return(NumMessages);

	while ((DirEnt = readdir(Dir)))
	{
		Name = DirEnt->d_name;

		if ((strcmp(Name, ".") != 0) && (strcmp(Name, "..") != 0))
		{
			if ((isdigit(*Name)) && (Name[12] == '-'))
			{
				if (stat(Name, &Status) == 0)
				{
					if (Status.st_mode & S_IRUSR)
					{
						if (Status.st_mode & S_IXUSR) continue;

						Messages++;
					}
					else
					{
						IncomingTimeM = 0;
						IncomingTimeC = 0;
					}
				}
			}
		}
	}

	closedir(Dir);

	NumMessages			= Messages;
	RedrawMessageList	= TRUE;

	return(Messages);
}

/*************************************************************************
 ** ListviewPoint():																		**
 *************************************************************************/

static void ListviewPoint(void)
{
	float	H;
	float P;
	float E;
	int	Y;

	H = (float) LINES - 11;
	P = (float) CurrListLine;
	E = (float) NumMessages;

	if ((MessageList) && (NumMessages > 1))
	{
		AttrSet(stdscr, Color(1) | A_REVERSE);

		HLine(stdscr, COLS - 5, ListviewPointPosY + 6, ACS_BOARD, 1);

		Y = (int)((P / (E / H)));

		if (CurrListLine == (NumMessages - 1)) Y = LINES - 12;

		AttrSet(stdscr, Color(10) | A_BOLD);

		HLine(stdscr, COLS - 5, Y + 6, ACS_BLOCK, 1);

		ListviewPointPosY = Y;

		Update(stdscr);
	}
	else ListviewPointPosY = 0;
}

/*************************************************************************
 ** PlayMessage():	Spielt eine Nachricht.										**
 *************************************************************************/

static void PlayMessage(struct MessageLine *Line)
{
	PANEL	*Panel;
	char	 Command[MAXCOMMANDLEN + 1];

	if ((!ViewMode) && (MessageList) && (NumMessages > 0))
	{
		sprintf(Command, "%s \"%s/%s\" \"%d\"", VBOXPLAYCMD, MessageDir, Line->Filename, Volume);

		Panel = OpenPanel(-1, -1, 27, 6, Color(4) | A_BOLD, Color(5) | A_BOLD);

		if (Panel)
		{
			AttrSet(Panel->win, Color(4) | A_BOLD);
		
			Print(Panel->win, 1, 2, "       PLAYING       ");
			Print(Panel->win, 3, 2, "Press CTRL-C to stop!");

			Update(Panel->win);
		}

		signal(SIGINT, SIG_DFL);
		system(Command);

		if (Panel) ClosePanel(Panel);

		StopFunc(-1);

		Line->New = FALSE;

		chmod(Line->Filename, S_IRUSR | S_IWUSR | S_IXUSR);
		chmod(Line->Filename, S_IRUSR | S_IWUSR | S_IXUSR);
	}
	else Beep();
}

/*************************************************************************
 ** TogglePower():	Schaltet Anrufbeantworter ein oder aus.				**
 *************************************************************************/

static void TogglePower(void)
{
	if (!ViewMode)
	{
		if (FileExist(UserHome, ".vboxstop"))
		{
			DeleteFile(UserHome, ".vboxstop");
		}
		else TouchFile(UserHome, ".vboxstop");
	}
	else Beep();

	PrintStatus();
}

/*************************************************************************
 **																							** 
 ** 										G U I												**
 **																							**
 *************************************************************************/

/*************************************************************************
 ** InitNCurses():	Initialisiert die Funktionen von ncurses und er-	**
 **						zeugt den Hauptbildschirm.									**
 *************************************************************************
 ** Device				Name des Devices das als Ein- & Ausgabe benutzt		**
 **						werden soll.													**
 **																							**
 ** [Return]			TRUE oder FALSE.												**
 *************************************************************************/

static int InitNCurses(char *Device)
{
	int i;

	for (i = 0; i < MAX_OPEN_PANELS; i++) Panels[i] = NULL;

	if ((NCursesI = fopen(Device, "r")))
	{
		if ((NCursesO = fopen(Device, "w")))
		{
			if ((Init = newterm(NULL, NCursesO, NCursesI)))
			{
				if (set_term(Init))
				{
					if ((LINES >= 24) && (COLS >= 80))
					{
						CreateColorTable();
					
						True();
					}
					else fprintf(stderr, "%s: minimal terminal dimensions must be 80x24.\n", Basename);
				}
				else fprintf(stderr, "%s: can't setup default screen.\n", Basename);

				delscreen(Init);
				
				Init = NULL;
			}
			else fprintf(stderr, "%s: can't initialize screen.\n", Basename);

			CloseStream(NCursesO);
		}
		else fprintf(stderr, "%s: can't open '%s' for output.\n", Basename, Device);

		CloseStream(NCursesI);
	}
	else fprintf(stderr, "%s: can't open '%s' for input.\n", Basename, Device);

	False();
}

/*************************************************************************
 ** ExitNCurses(): Beendet NCurses-Initialisierung.							**
 *************************************************************************/

static void ExitNCurses(void)
{
	int i;
	
	for (i = 0; i < MAX_OPEN_PANELS; i++)
	{
		if (Panels[i] != NULL) ClosePanel(Panels[i]);
	}

	wclear(stdscr);
	wrefresh(stdscr);
	endwin();

	if (Init) delscreen(Init);

	if (NCursesI) fclose(NCursesI);
	if (NCursesO) fclose(NCursesI);
}

/*************************************************************************
 ** CreateColorTable():	Initialisiert die Farbtabelle von ncurses.		**
 *************************************************************************/

static void CreateColorTable(void)
{
	if ((start_color() != ERR) && (!Mono))
	{
		init_pair(1 , COLOR_WHITE , COLOR_BLACK ); /* Main: Titelzeile		*/
		init_pair(2 , COLOR_WHITE , COLOR_BLUE  ); /* Main: Hintergrund	*/
		init_pair(3 , COLOR_BLACK , COLOR_BLACK ); /* Main: Schatten		*/
		init_pair(4 , COLOR_WHITE , COLOR_RED   ); /* Note: Hintergrund	*/
		init_pair(5 , COLOR_YELLOW, COLOR_RED   ); /* Note: Rahmen			*/
		init_pair(6 , COLOR_GREEN , COLOR_BLACK ); /* Time: Nummern			*/
		init_pair(8 , COLOR_YELLOW, COLOR_BLUE  ); /* List: Hintergrund	*/
		init_pair(9 , COLOR_WHITE , COLOR_BLUE  ); /* List: Rahmen			*/
		init_pair(10, COLOR_RED   , COLOR_YELLOW); /* List: Selektiert		*/
		init_pair(11, COLOR_CYAN  , COLOR_BLUE  ); /* List: Beschreibung	*/
	}
}

/*************************************************************************
 ** HLine():	Zeichnet eine horizontale Linie.									**
 *************************************************************************/

static void HLine(WINDOW *Window, int X, int Y, chtype C, int Len)
{
	if ((X != -1) && (Y != -1)) wmove(Window, Y, X);

	whline(Window, C, Len);
}

/*************************************************************************
 ** VLine():	Zeichnet eine vertikale Linie.									**
 *************************************************************************/

static void VLine(WINDOW *Window, int X, int Y, chtype C, int Len)
{
	if ((X != -1) && (Y != -1)) wmove(Window, Y, X);

	wvline(Window, C, Len);
}

/*************************************************************************
 ** Window():	Zeichnet ein Fenster.												**
 *************************************************************************/

static void Window(WINDOW *Window, int X, int Y, int W, int H, chtype B, chtype R)
{
	int i;
	int s;

		/* Fenster mit stdscr und allen Panels in der globalen Liste	*/
		/* �berschreiben.																*/

	overwrite(stdscr, Window);

	for (i = 0; i < MAX_OPEN_PANELS; i++)
	{
		if ((Panels[i]) && (Panels[i]->win != Window))
		{
			overwrite(Panels[i]->win, Window);
		}
	}

		/* Hintergrund */

	if (B != 0)
	{
		AttrSet(Window, B);

		for (i = 0; i < H - 2; i++) HLine(Window, X, Y + i, ' ', W - 2);
	}

		/* Rahmen */
	
	if (R != 0)
	{
		AttrSet(Window, R);
	
		HLine(Window, X, Y, ACS_HLINE, W - 2);
		HLine(Window, X, Y + H - 2, ACS_HLINE, W - 2);
		VLine(Window, X, Y, ACS_VLINE, H - 1);
		VLine(Window, X + W - 3, Y, ACS_VLINE, H - 1);
		HLine(Window, X, Y, ACS_ULCORNER, 1);
		HLine(Window, X + W - 3, Y, ACS_URCORNER, 1);
		HLine(Window, X, Y + H - 2, ACS_LLCORNER, 1);
		HLine(Window, X + W - 3, Y + H - 2, ACS_LRCORNER, 1);

			/* Schatten */

		s = (Mono ? 0 : 3);

		mvwchgat(Window, Y + H - 1, X + 2, W - 2, A_BOLD | A_ALTCHARSET, s, NULL);

		for (i = 0; i < H - 2; i++)
		{
			mvwchgat(Window, Y + 1 + i, X + W - 2, 2, A_BOLD | A_ALTCHARSET, s, NULL);
		}
	}
}

/*************************************************************************
 ** OpenPanel():	�ffnet ein Window als Panel.									**
 *************************************************************************/

static PANEL *OpenPanel(int X, int Y, int W, int H, chtype B, chtype R)
{
	WINDOW	*Win = NULL;
	PANEL		*Pan = NULL;
	int		 i;

	if ((W > COLS) || (H > LINES)) return(NULL);

	if (X == -1)
	{
		X = ((COLS - W) / 2);
		
		if ((X + W + 2) < COLS) X += 2;
	}
	
	if (Y == -1)
	{
		Y = ((LINES - H) / 2);
		
		if ((Y + H + 2) < LINES) Y++;
	}

	if ((Win = newwin(H, W, Y, X)))
	{
		if ((Pan = new_panel(Win)))
		{
			top_panel(Pan);
			update_panels();

			Window(Pan->win, 0, 0, W, H, B, R);

			for (i = 0; i < MAX_OPEN_PANELS; i++)
			{
				if (Panels[i] == NULL)
				{
					Panels[i] = Pan;
					
					break;
				}
			}

			return(Pan);
		}

		delwin(Win);
	}

	return(NULL);
}

/*************************************************************************
 ** ClosePanel():	Schlie�t ein Panel und sein Window.							**
 *************************************************************************/

static void ClosePanel(PANEL *Panel)
{
	WINDOW *Win;
	int	  i;

	if (Panel)
	{
		for (i = 0; i < MAX_OPEN_PANELS; i++)
		{
			if (Panels[i] == Panel)
			{
				Panels[i] = NULL;
				
				break;
			}
		}

		Win = Panel->win;
		
		del_panel(Panel);
		delwin(Win);
		update_panels();
		wrefresh(stdscr);
	}
}

/*************************************************************************
 ** Color():																				**
 *************************************************************************/

static chtype Color(int Pair)
{
	if (!Mono) return(COLOR_PAIR(Pair) | A_ALTCHARSET);

	return(COLOR_PAIR(0) | A_ALTCHARSET);
}

/*************************************************************************
 ** Scroll():																				**
 *************************************************************************/

static void Scroll(WINDOW *Win, int N)
{
	scrollok(Win, TRUE);
	wscrl(Win, N);
	scrollok(Win, FALSE);
}

/*************************************************************************
 ** Button():																				**
 *************************************************************************/

static void Button(WINDOW *Win, int X, int Y, char *Text)
{
	chtype Attr;
	int	 i;

	wmove(Win, Y, X);
	
	for (i = 0; i < strlen(Text); i++)
	{
		if (Text[i] == '_')
		{
			Attr = (Color(4) | A_REVERSE);

			if (Mono) Attr |= A_UNDERLINE;

			i++;
		}
		else Attr = (Color(1) | A_REVERSE);

		AttrSet(Win, Attr);

		wprintw(Win, "%c", Text[i]);
	}
}
