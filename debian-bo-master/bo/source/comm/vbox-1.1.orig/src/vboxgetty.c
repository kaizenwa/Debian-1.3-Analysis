/*
 * $Id: vboxgetty.c,v 1.5 1996/07/25 13:07:26 root Exp $
 *
 * $Log: vboxgetty.c,v $
 * Revision 1.5  1996/07/25 13:07:26  root
 * *** empty log message ***
 *
 * Revision 1.4  1996/07/19 08:53:32  root
 * o Kleine Änderungen.
 *
 * Revision 1.3  1996/07/16 13:03:41  root
 * o Optionen -p und -a zum Starten eines Programms nach jeder aufgenommenen
 *   Nachricht eingebaut.
 *
 * o Option -v zum Anzeigen der Versionsnummer eingebaut.
 *
 * Revision 1.2  1996/07/15 10:00:53  root
 * o Funktion FreeSpace() eingebaut. Damit kann jetzt abgefragt werden, ob
 *   noch genug Platz auf der Partition mit dem Spoolverzeichnis ist.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>
#include <pwd.h>
#include <errno.h>
#include <getopt.h>
#include <sys/vfs.h>
              
#include "vboxgetty.h"
#include "modem.h"
#include "log.h"
#include "settings.h"
#include "lock.h"
#include "voice.h"
#include "vboxrc.h"
#include "utils.h"

/** Variables ************************************************************/

static int	 Loglevel;
static int	 RingsNeeded;
static char	*Basename;
static char *MailProgram;
static char *MailAddress;

char	VBoxPID[PATH_MAX + 1];
char	VBoxLockname[PATH_MAX + 1];
char	VBoxLogname[PATH_MAX + 1];
char	VBoxDevicetext[NAME_MAX + 1];
char	VBoxDevicename[PATH_MAX + 1];
char	InitCommand[MAXINITCOMMANDLEN + 1];
char	CallerID[MAXCALLERIDLEN + 1];
char	Username[MAXUSERNAMELEN + 1];
char	UserHome[PATH_MAX + 1];
uid_t	UID;
gid_t	GID;

static struct option Arguments[] =
{
	{ "device"  , required_argument, NULL, 'd' },
	{ "speed"	, required_argument, NULL, 's' },
	{ "username", required_argument, NULL, 'u' },
	{ "program"	, required_argument, NULL, 'p' },
	{ "address" , required_argument, NULL, 'a' },
	{ "init"		, required_argument, NULL, 'i' },
	{ "version"	, no_argument		 , NULL, 'v' },
	{ "help"		, no_argument      , NULL, 'h' },
	{ NULL		, 0                , NULL,  0  }
};

/** Prototypes ***********************************************************/

static boolean	InitVBox(int);
static void		ExitVBox(int);
static void		BlockSignals(void);
static void		Main(void);
static boolean	MkDir(char *, char *);
static void		Usage(void);
static void		Version(void);
static boolean	FreeSpace(unsigned long);
static void		SigChild(int);

/*************************************************************************
 ** The magic main...																	**
 *************************************************************************/

void main(int argc, char **argv)
{
	int Opts;

	Basename = argv[0];

	Loglevel 	= L_FATAL | L_ERROR | L_WARN;
	FlowControl	= MODEM_FLOW_HARD;
	Speed			= 38400;
	ToggleDTR	= TOGGLEDTR;
	TogglePause	= TOGGLEDTRTIME;
	RingsNeeded	= 10;
	UID			= -1;
	GID			= -1;
	MailProgram	= NULL;
	MailAddress	= NULL;

	*VBoxPID				= 0;
	*VBoxLockname		= 0;
	*VBoxLogname		= 0;
	*VBoxDevicetext	= 0;
	*VBoxDevicename	= 0;
	*CallerID			= 0;
	*Username			= 0;
	*InitCommand		= 0;

		/* Argumente eintragen. */

	while ((Opts = getopt_long(argc, argv, "p:a:d:s:u:i:hv", Arguments, (int *)0)) != EOF)
	{
		switch (Opts)
		{
			case 'v':
				Version();
				break;

			case 's':
				Speed = (long)StrToNumber(optarg, 38400);
				break;
		
			case 'd':
				CopyString(VBoxDevicename, optarg, PATH_MAX);
				break;
			
			case 'u':
				CopyString(Username, optarg, MAXUSERNAMELEN);
				break;
			
			case 'i':
				CopyString(InitCommand, optarg, MAXINITCOMMANDLEN);
				break;

			case 'p':
				MailProgram = optarg;
				break;
				
			case 'a':
				MailAddress = optarg;
				break;

			case 'h':
			default:
				Usage();
				break;
		}
	}

	if (!*InitCommand) strcpy(InitCommand, "ATZ");

		/* `vbox' initialisieren. */

	if (!InitVBox(Loglevel)) ExitVBox(NORMALEXIT + 20);

		/* Signalhändler installieren. */

	signal(SIGHUP	, ExitVBox);
	signal(SIGINT	, ExitVBox);
	signal(SIGTERM	, ExitVBox);
	signal(SIGHUP	, ExitVBox);
	signal(SIGCHLD , SIG_IGN );

		/* Hauptschleife */

	while (TRUE)
	{
		if (ModemOpenDevice(VBoxDevicename))
		{
			Main();

			ModemCloseDevice();
		}
		else sleep(10);
	}

	ExitVBox(NORMALEXIT + 0);
}

/*************************************************************************
 ** SigChild():																			**
 *************************************************************************/

static void SigChild(int s)
{
	int	Status;
	pid_t	PID;

	PID = wait(&Status);

	signal(SIGCHLD, SigChild);

	if (PID != -1)
	{
		LogLine(L_JUNK, "Got signal from child %ld (exit status %d).\n", (long)PID, WEXITSTATUS(Status));
	}
}

/*************************************************************************
 ** Version():																				**
 *************************************************************************/

static void Version(void)
{
	fprintf(stderr, "%s version %s\n", Basename, VERSION);
	
	exit(NORMALEXIT + 5);
}

/*************************************************************************
 ** Usage():																				**
 *************************************************************************/

static void Usage(void)
{
	fprintf(stderr, "\n");
	fprintf(stderr, "Usage: %s OPTION OPTION OPTION [...]\n", Basename);
	fprintf(stderr, "\n");
	fprintf(stderr, "-s, --speed SPEED    Sets modem speed to SPEED (default: 38400).\n");
	fprintf(stderr, "-d, --device NAME    Device name to use (eg. /dev/ttyI5) [required].\n");
	fprintf(stderr, "-u, --username NAME  Name of the user to spool voice messages for [required].\n");
	fprintf(stderr, "-i, --init INIT      Sets modem initialize command to INIT (default: \"ATZ\").\n");
	fprintf(stderr, "-p, --program PROG   Starts program PROG after each recorded message.\n");
	fprintf(stderr, "-a, --address NAME   Address for the -p option (default: name given with -u).\n");
	fprintf(stderr, "-v, --version        Displays the version number.\n");
	fprintf(stderr, "-h, --help           Displays this help message.\n");
	fprintf(stderr, "\n");

	UnlockPID();
	CloseLog();

	exit(NORMALEXIT + 5);
}

/*************************************************************************
 ** InitVBox():																			**
 *************************************************************************/

static boolean InitVBox(int Level)
{
	struct passwd *User;

	char *Temp;

	SetLogLevel(Level);

		/* Name des aktuellen Devices ermitteln (ohne den Pfad) und alle	*/
		/* Log- und Lockdateinamen erzeugen.										*/

	if (!*VBoxDevicename)
	{
		fprintf(stderr, "%s: can't spool voice mails without a device.\n", Basename);

		Usage();
	}

	if ((Temp = rindex(VBoxDevicename, '/')))
	{
		Temp++;

		CopyString(VBoxDevicetext, Temp, NAME_MAX);
	}
	else CopyString(VBoxDevicetext, VBoxDevicename, NAME_MAX);

	if (strncmp(VBoxDevicetext, "tty", 3) != 0)
	{
		fprintf(stderr, "%s: Device name must be a tty device (eg. ttyI5).\n", Basename);

		Usage();
	}

	sprintf(VBoxPID		, PIDFILE , VBoxDevicetext);
	sprintf(VBoxLogname	, LOGFILE , VBoxDevicetext);
	sprintf(VBoxLockname	, LOCKFILE, VBoxDevicetext);

		/* Programm locken. `vbox' erzeugt eine PID Datei für das	*/
		/* aktuelle Device und lockt diese 'exclusiv', sodaß ein		*/
		/* erneuter Start mit dem gleichen Device nicht möglich		*/
		/* ist.																		*/

	if (!LockPID())
	{
		fprintf(stderr, "%s: can't create \"%s\".\n", Basename, VBoxPID);
		
		False();
	}

		/* Logdatei für das aktuelle Device öffnen. */

	OpenLog(VBoxLogname);

		/* UID und GID des aktuellen Benutzers aus /etc/passwd holen	*/
		/* und in die globalen Variablen eintragen. Alle Verzeichnisse	*/
		/* und Dateien werden mit diesen Permissions versehen!			*/

	if (!*Username)
	{
		fprintf(stderr, "%s: can't spool voice mails without a username.\n", Basename);

		Usage();
	}

	if (!(User = getpwnam(Username)))
	{
		fprintf(stderr, "%s: user %s doesn't exists.\n", Basename, Username);
		
		False();
	}

	UID = User->pw_uid;
	GID = User->pw_gid;

	CopyString(UserHome, User->pw_dir, PATH_MAX);

		/* Verzeichnisse für den Benutzer erzeugen. MkDir() kehrt nur	*/
		/* mit einem Fehler zurück, wenn das Verzeichnis nicht exis-	*/
		/* tiert hat und nicht erzeugt werden konnte.						*/

	if (!MkDir(Username, 		  "")) False();
	if (!MkDir(Username, "incoming")) False();
	if (!MkDir(Username, "messages")) False();

		/* Ok */

	SetLogLevel(GetDebugMode(UID));

	LogLine(L_INFO, "Voice spooling for user %s (uid %d)...\n", Username, UID);

	if (MailProgram)
	{
		if (!MailAddress) MailAddress = Username;

		LogLine(L_INFO, "Recorded messages will be parsed with '%s' for '%s'.\n", MailProgram, MailAddress);
	}

	True();
}

/*************************************************************************
 ** MailMessage():																		**
 *************************************************************************/

void MailMessage(char *Name, char *ID)
{
	long	Size;
	char *Line;
	int	r;

	if ((!MailProgram) || (!MailAddress)) return;

	Size = strlen(Name) + strlen(ID) + strlen(MailProgram) + strlen(MailAddress) + 100;

	if (!(Line = malloc(Size)))
	{
		LogLine(L_ERROR, "Can't allocate %ld bytes for notification program.\n", Size);

		return;
	}

	sprintf(Line, "%s %s %s %s >/dev/console 2>&1 </dev/null", MailProgram, Name, ID, MailAddress);

	LogLine(L_DEBUG, "Execute \"%s\"...\n", Line);

	switch (fork())
	{
		case 0:
			close(0);
			close(1);
			close(2);
			setpgrp();
                                             			
			if ((r = system(Line)) != 0)
			{
				LogLine(L_ERROR, "Executing notification program failed!\n");
			}

			exit(0);

		case -1:
			LogLine(L_ERROR, "Can't fork process for the notification program.\n");
			break;
	}

	free(Line);
}

/*************************************************************************
 ** MkDir():																				**
 *************************************************************************/

static boolean MkDir(char *Name, char *Dir)
{
	char NewDir[PATH_MAX + 1];

	strcpy(NewDir, SPOOLDIR);
	strcat(NewDir, "/");
	strcat(NewDir, Name);

	if ((Dir) && (*Dir))
	{
		strcat(NewDir, "/");
		strcat(NewDir, Dir);
	}

	if (mkdir(NewDir, S_IRUSR | S_IWUSR | S_IXUSR) != 0)
	{
		if (errno != EEXIST)
		{
			LogLine(L_ERROR, "Can't create directory \"%s\" (%s)\n", NewDir, strerror(errno));

			return(FALSE);
		}
		else LogLine(L_DEBUG, "Directory \"%s\" exists.\n", NewDir);
	}
	else LogLine(L_DEBUG, "Directory \"%s\" created.\n", NewDir);

	chown(NewDir, UID, GID);
	chown(NewDir, UID, GID);

	return(TRUE);
}

/*************************************************************************
 ** ExitVBox():																			**
 *************************************************************************/

static void ExitVBox(int RC)
{
	BlockSignals();

	if (RC < NORMALEXIT) LogLine(L_FATAL, "Got signal %d -- shutdown...\n", RC);

	ModemCloseDevice();

	UnlockPID();
	UnlockDevice();

	LogLine(L_INFO, "---------------------- End session -----------------------\n");

	CloseLog();

	exit(RC);
}

/*************************************************************************
 ** BlockSignals():																		**
 *************************************************************************/

static void BlockSignals(void)
{
	signal(SIGHUP		, SIG_IGN);
	signal(SIGINT		, SIG_IGN);
	signal(SIGQUIT		, SIG_IGN);
	signal(SIGILL		, SIG_IGN);
	signal(SIGTRAP		, SIG_IGN);
	signal(SIGABRT		, SIG_IGN);
	signal(SIGUNUSED	, SIG_IGN);
	signal(SIGUSR1		, SIG_IGN);
	signal(SIGUSR2		, SIG_IGN);
	signal(SIGPIPE		, SIG_IGN);
	signal(SIGALRM		, SIG_IGN);
	signal(SIGTERM		, SIG_IGN);
	signal(SIGSTKFLT	, SIG_IGN);
	signal(SIGCHLD		, SIG_IGN);
	signal(SIGTSTP		, SIG_IGN);
	signal(SIGTTIN		, SIG_IGN);
	signal(SIGTTOU		, SIG_IGN);
	signal(SIGIO		, SIG_IGN);
	signal(SIGXCPU		, SIG_IGN);
	signal(SIGXFSZ		, SIG_IGN);
	signal(SIGVTALRM	, SIG_IGN);
	signal(SIGPROF		, SIG_IGN);
	signal(SIGWINCH	, SIG_IGN);
}

/*************************************************************************
 ** Main():																					**
 *************************************************************************/

void Main(void)
{
	int Status;

	Status = STATE_INITIALIZE;

	while (Status != STATE_LEAVE)
	{
		switch (Status)
		{
				/* Das benutzte Device ist von einem anderen Prozeß ge-	*/
				/* lockt (oder noch vom eigenen?).								*/

			case STATE_DIALOUT:
			
				LogLine(L_DEBUG, "Device ist busy (dialout mode) - try again...\n");

				SleepSecs(5);
				
				if (LockDevice()) Status = STATE_INITIALIZE;
				
				break;

				/* Das Modem wird mit dem Initstring initialisiert und da-	*/
				/* nach in den Voicemodus versetzs (+FCLASS=8).					*/

			case STATE_INITIALIZE:

				LogLine(L_INFO, "Initialize modem (voice mode)...\n");

				if (LockDevice())
				{					
					if (ModemCommand(InitCommand, "OK") > 0)
					{
						if (ModemCommand("AT+FCLASS=8", "VCON|OK") > 0)
						{
							Status = STATE_WAITING;
						}
						else SleepSecs(2);
					}
					else SleepSecs(2);
				}
				else Status = STATE_DIALOUT;

				break;

				/* Das Modem wartet auf Anrufe. Nach MODEM_WAIT_TIMEOUT	*/
				/* Sekunden wird überprüft, ob das Modem noch auf An-		*/
				/* fragen reagiert.													*/

			case STATE_WAITING:

				signal(SIGCHLD, SigChild);

				ModemFlushFull();
				UnlockDevice();

				if (ModemWait(MODEM_WAIT_TIMEOUT))
				{
					Status = STATE_RING;
				}
				else Status = STATE_CHECKMODEM;

				break;

				/* Ein eingehender Anruf wurde bemerkt. Wenn noch genug	*/
				/* Platz auf der Festplatte ist, wird der Anruf nach ge-	*/
				/* nügend gefundenen RING's entgegengenommen.				*/

			case STATE_RING:

				if (LockDevice())
				{
					Status = STATE_INITIALIZE;

					RingsNeeded = GetRingsToWait(UID);

					if (ModemGetRings(RingsNeeded))
					{
						if (FreeSpace(NEED_FREE_DISK_SPACE))
						{
							if (ModemCommand("ATA", "VCON|CONNECT") > 0)
							{
								ModemSetNoCarrier(FALSE);
								VoiceMode();
							
								Status = STATE_LEAVE;
							}
						}
						else LogLine(L_FATAL, "Not enough free disk space!\n");
					}
				}
				else Status = STATE_DIALOUT;

				break;

				/* Nachdem der MODEM_WAIT_TIMEOUT abgelaufen ist, wird	*/
				/* ein Kommando an das Modem geschickt um zu sehen ob es	*/
				/* noch reagiert.														*/

			case STATE_CHECKMODEM:

				LogLine(L_DEBUG, "Checking modem (timeout)...\n");

				Status = STATE_LEAVE;

				if (LockDevice())
				{
					if (ModemCommand("AT", "OK") > 0) Status = STATE_WAITING;
				}
				else Status = STATE_DIALOUT;

				break;
		}
	}

	UnlockDevice();
}

/*************************************************************************
 ** FreeSpace():																			**
 *************************************************************************/

static boolean FreeSpace(unsigned long Bytes)
{
	struct statfs Status;

	long NeedBlocks;

	LogLine(L_DEBUG, "Checking free disc space for '%s'...\n", SPOOLDIR);

	if (Bytes <= 0)
	{
		LogLine(L_WARN, "Free disc space check disabled.\n");

		True();
	}

	if (statfs(SPOOLDIR, &Status) == 0)
	{
		if ((NeedBlocks = (Bytes / Status.f_bsize)) < 1) NeedBlocks = 1;

		LogLine(L_JUNK, "%ld blocks (%ld bytes) available - need %ld.\n", Status.f_bfree, Status.f_bsize, NeedBlocks);

		if (Status.f_bfree >= NeedBlocks) True();

		LogLine(L_WARN, "Not enough free blocks available!\n");
	}
	else LogLine(L_ERROR, "Can't get infos about the disc space!");

	False();
}

/*************************************************************************
 ** StrToNumber():																		**
 *************************************************************************/
 
unsigned long StrToNumber(char *Text, unsigned long Default)
{
	char				*Stop;
	unsigned long	 Number;

	Number = strtoul(Text, &Stop, 10);

	if (*Stop != 0)
	{
		LogLine(L_WARN, "StrToNumber() can't convert \"%s\".\n", Text);

		Number = Default;
	}

	return(Number);
}
