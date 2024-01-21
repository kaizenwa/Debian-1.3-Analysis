/*
 * $Id: voice.c,v 1.7 1996/07/25 13:07:26 root Exp $
 *
 * $Log: voice.c,v $
 * Revision 1.7  1996/07/25 13:07:26  root
 * *** empty log message ***
 *
 * Revision 1.6  1996/07/19 08:53:33  root
 * o Kleine Änderungen.
 *
 * Revision 1.5  1996/07/16 16:30:44  root
 * o Abfrage eingebaut, damit im Messagenamen kein / angegeben werden
 *   kann.
 *
 * Revision 1.4  1996/07/15 10:13:29  root
 * o Einige eincompilierten Werte durch defines ersetzt.
 *
 * o Unterstützung für den isdn4linux Modem-Header eingebaut.
 *
 * Revision 1.3  1996/07/15 10:01:43  root
 * o Unterstützung für die neuen Flags im .vboxrc eingebaut.
 *
 */

#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <limits.h>
#include <errno.h>
#include <time.h>

#include "vboxgetty.h"
#include "modem.h"
#include "voice.h"
#include "log.h"
#include "settings.h"
#include "vboxrc.h"
#include "defines.h"
#include "utils.h"

/** Variables ************************************************************/

static int	VoiceActionStatus	= VOICE_ACTION_OK;
static long Savetime				= DEFAULT_RECORD_TIME;
static int	StatusHandleDLE	= ST_NO_INPUT;
static int	ParkCheck			= TRUE;

static char Touchtones[TOUCHTONE_BUFFER_LEN + 1];

static char *Compressions[] =
{
	"-unknown-", "-unknown-", "ADPCM-2", "ADPCM-3", "ADPCM-4", "ALAW", "ULAW"
};

/** Prototypes ***********************************************************/

static short	GetCompression(void);
static boolean SetCompression(short);
static void		SetVoiceHeader(struct VoiceHeader *);
static int		RecordMessage(int);
static int		PlayMessage(char *);
static void		HandleDLE(char);
static void		HandleTouchTone(char);

/*************************************************************************
 ** VoiceMode():																			**
 *************************************************************************/

boolean VoiceMode(void)
{
	struct UserSection Section;

	char CallerName[33];
	char CallerGoto[33];

		/* Eigentümer der gefundenen CallerID ermitteln. Wenn es keine	*/
		/* gültige CallerID war, oder der Eigentümer nicht gefunden		*/
		/* werden konnte, wird "*** Unknown ***" zurückgegeben.			*/

	GetNameFromCallerID(UID, CallerID, CallerGoto, CallerName, 32);

		/* Wenn die Einstellungen nicht gefunden werden konnten, oder	*/
		/* ein Fehler aufgetreten ist, wird die Voreinstellung benut-	*/
		/* zt.																			*/

	GetOwnerSettings(UID, CallerGoto, CallerName, &Section);

	Savetime = Section.Recordtime;

	if (Savetime < 0) Savetime = DEFAULT_RECORD_TIME;

	signal(SIGCHLD, SIG_IGN);

	*Touchtones = 0;

	ParkCheck = TRUE;

	if (Section.PlayMessage)
	{
		if (PlayMessage(Section.StandardMessage) == -1) True();
	}

	if ((Savetime > 0) && (Section.PlayBeep))
	{
		if (PlayMessage("beep.msg") == -1) True();
	}

	if ((Savetime > 0) && (Section.Record))
	{
		if (RecordMessage(Section.PlayTimeout) == 0)
		{
			PlayMessage("panic.msg");
		
			False();
		}
	}

	True();
}

/*************************************************************************
 ** PlayMessage():	Spielt eine Nachricht über das Telefon ab.			**
 *************************************************************************
 ** Text				Name der Datei die gespielt werden soll.					**
 **																							**
 ** <Return>		 0 - Error															**
 **                1 - Ok																**
 **               -1 - Hangup															**
 *************************************************************************/

static int PlayMessage(char *Text)
{
	struct VoiceHeader Header;

	char	File[PATH_MAX + 1];
	char	LineIn[MODEM_BUFFER_LEN + 1];
	char	LineOut[MODEM_BUFFER_LEN + MODEM_BUFFER_LEN + 1];
	int	FD;
	short	Compression;
	TIO	TermIO;
	TIO	SaveIO;
	int	i;
	int	BytesIn;
	int	BytesOut;
	int	Written;
	char *UseText;

	if (*Touchtones != '*') *Touchtones = 0;

		/* Abfrage, damit kein Slash (/) in die Messagenamen eingebaut	*/
		/* werden kann.																*/

	if ((UseText = rindex(Text, '/')))
	{
		UseText++;

		LogLine(L_WARN, "Found slash (/) in message name - use \"%s\" as new name.\n", UseText);
	}
	else UseText = Text;

	strcpy(File, SPOOLDIR);
	strcat(File, "/");
	strcat(File, Username);
	strcat(File, "/messages/");
	strcat(File, UseText);

	LogLine(L_DEBUG, "Playing \"%s\"...\n", File);

		/* FlowControl des Modems auf XON (ausgehend) setzen. Bei einem	*/
		/* Fehler wird abgebrochen.													*/

	if (!ModemGetTermIO(&TermIO))
	{
		LogLine(L_ERROR, "Can't get terminal I/O settings (voice).\n");

		False();
	}
	
	SaveIO = TermIO;

	ModemSetFlowControl(&TermIO, MODEM_FLOW_XON_OUT);

	if (!ModemSetTermIO(&TermIO))
	{
		LogLine(L_ERROR, "Can't set terminal I/O settings (XON/XOFF).\n");

		ModemSetTermIO(&SaveIO);
		False();
	}

		/* Angegebene Nachricht öffnen und den Header auf ID, Kompression	*/
		/* und Modemtyp prüfen.															*/

	if ((FD = open(File, O_RDONLY)) == -1)
	{
		LogLine(L_WARN, "Can't open \"%s\".\n", File);

		ModemSetTermIO(&SaveIO);
		False();
	}

	if (read(FD, &Header, sizeof(struct VoiceHeader)) != sizeof(struct VoiceHeader))
	{
		LogLine(L_ERROR, "Can't read voice header from \"%s\".\n", File);

		CloseFD(FD);
		ModemSetTermIO(&SaveIO);
		False();
	}

	if (strncmp(Header.Magic, "RMD1", 4) != 0)
	{
		LogLine(L_ERROR, "No raw modem data header found.\n");

		CloseFD(FD);
		ModemSetTermIO(&SaveIO);
		False();
	}	

	if ((strcmp(Header.Modem, VOICE_MODEM) != 0) && (strcmp(Header.Modem, VOICE_LISDN) != 0))
	{
		LogLine(L_ERROR, "Wrong modem type found.\n");
		
		CloseFD(FD);
		ModemSetTermIO(&SaveIO);
		False();
	}

	Compression = ntohs(Header.Compression);

	if (!SetCompression(Compression))
	{
		LogLine(L_ERROR, "Can't set voice audio compression or line mode.\n");
		
		ModemSetTermIO(&SaveIO);
		CloseFD(FD);
		False();
	}

		/* Wenn jetzt kein Carrier mehr anliegt, wird die Nachricht	*/
		/* nicht gespielt.														*/

	if (ModemGetNoCarrier())
	{
		ModemSetTermIO(&SaveIO);
		CloseFD(FD);
		Cancel();
	}

		/* Ansonsten Modem in den Voice-Play-Modus versetzen und die	*/
		/* Nachricht spielen. AT+VTX liefert ein NO ANSWER wenn kein	*/
		/* Carrier mehr anliegt (gibt es beim ZyXEL nicht)!				*/

	if (ModemCommand("AT+VTX", "CONNECT") == 0)
	{
		LogLine(L_ERROR, "Can't start voice play mode.\n");
		
		ModemSetTermIO(&SaveIO);
		CloseFD(FD);
		False();
	}

	VoiceActionStatus	= VOICE_ACTION_OK;
	StatusHandleDLE	= ST_NO_INPUT;

	while (VoiceActionStatus == VOICE_ACTION_OK)
	{
			/* Daten vom Modem in den Buffer lesen. Wenn keine Daten mehr	*/
			/* vorhanden sind, Schleife abbrechen.									*/

		if ((BytesIn = read(FD, LineIn, MODEM_BUFFER_LEN)) <= 0) break;

			/* Eingelesene Daten durchlaufen und alle <DLE> in <DLE><DLE>	*/
			/* umwandeln?!																	*/

		BytesOut = 0;

		for (i = 0; i < BytesIn; i++)
		{
			LineOut[BytesOut] = LineIn[i];

			if (LineOut[BytesOut++] == DLE) LineOut[BytesOut++] = DLE;
		}
                         
			/* Daten so lange schreiben, bis sie zu Ende sind oder ein	*/
			/* Fehler aufgetreten ist.												*/

		errno		= 0;
		Written	= 0;

		LogLine(L_JUNK, "Play: <DATA %d incoming; ", BytesIn);

		if (ModemGetNoCarrier() == FALSE)
		{
			LogText(L_JUNK, "%d outgoing>\n", BytesOut);

			while (((Written += ModemRawWrite(&LineOut[Written], BytesOut - Written)) != BytesOut) && (errno == 0));
		}
		else LogText(L_JUNK, "nothing outgoing>\n");

		if ((Written != BytesOut) || (errno != 0))
		{
			LogLine(L_ERROR, "Could only write %d of %d bytes (%s).\n", Written, BytesOut, strerror(errno));

			VoiceActionStatus = VOICE_ACTION_STOP;
		}

		if ((FileExist(UserHome, ".vboxpark")) && (!ModemGetNoCarrier()) && (ParkCheck))
		{
			VoiceActionStatus = VOICE_ACTION_PARK;
			
			LogLine(L_DEBUG, "File \".vboxpark\" found - try to parking line...\n");
		}

			/* Eingegende Daten vom Modem prüfen. Wenn ein <DLE><DC4> er-	*/
			/* kannt wird, wird die Message nicht mehr weiter gespielt.		*/

		while ((ModemCheckInput()) && (VoiceActionStatus == VOICE_ACTION_OK))
		{
			LogLine(L_JUNK, "Checking modem input...\n");

			if ((BytesIn = ModemRawRead(LineIn, MODEM_BUFFER_LEN)) > 0)
			{
				if (ModemGetNoCarrier() == TRUE)
				{
					LogLine(L_DEBUG, "Modem returns \"NO CARRIER\" - Stop playing...\n");

					VoiceActionStatus = VOICE_ACTION_HANGUP;
				}
				else
				{
					LogLine(L_JUNK, "Handle <DLE> sequences in %d bytes.\n", BytesIn);

					for (i = 0; i < BytesIn; i++) HandleDLE(LineIn[i]);

						/* Wenn sich jetzt in den gemerkten Touchtones ein	*/
						/* '#' bedindet, wird das Spielen gestoppt.			*/

					for (i = 0; i < strlen(Touchtones); i++)
					{
						if (Touchtones[i] == '#')
						{
							LogLine(L_DEBUG, "Found touchtone '#' -- stop playing...\n");
		
							VoiceActionStatus = VOICE_ACTION_STOP;
							
							break;
						}
					}
				}
			}
			else LogLine(L_ERROR, "Can't read input from modem.\n");
		}
	}

	CloseFD(FD);
	ModemPause(500);

		/* Wenn der Carrier noch besteht, ein <DLE><ETX> ans Modem	*/
		/* schicken. Der Emulator antwortet dann mit einem "VCON".	*/

	if (ModemGetNoCarrier() == FALSE)
	{
		sprintf(LineOut, "%c%c%c%c", DLE, DC4, DLE, ETX);

		LogLine(L_JUNK, "Play: <DLE><DC4>\n");
		LogLine(L_JUNK, "Play: <DLE><ETX>\n");
                    
		ModemRawWrite(LineOut, strlen(LineOut));

		sprintf(LineOut, "VCON|%c%c", DLE, ETX);

		ModemCommand("\r",  LineOut);
	}

	ModemSetTermIO(&SaveIO);

	ModemCommand("AT", "OK|VCON");
	ModemCommand("AT", "OK");

	if ((FileExist(UserHome, ".vboxpark")) && (!ModemGetNoCarrier()) && (ParkCheck))
	{
		ParkCheck = FALSE;

		if (PlayMessage("parkline.msg") == -1) Cancel();

			/* Kommando zum parken der Leitung an das Modem schicken. Wenn	*/
			/* korrekt geparkt wurde, wird '.vboxpark' gelöscht.				*/

		DeleteFile(UserHome, ".vboxpark");

		VoiceActionStatus = VOICE_ACTION_HANGUP;
	}

	if ((VoiceActionStatus == VOICE_ACTION_HANGUP) || (ModemGetNoCarrier()))
	{
		Cancel();
	}

	True();
}

/*************************************************************************
 ** RecordMessage():	Speichert die gesprochenen Audiodaten in eine		**
 **						Datei.															**
 *************************************************************************
 ** [Return]		1  - Ok																**
 **               0  - Error															**
 **               -1 - Hangup															**
 *************************************************************************/

static int RecordMessage(int PlayTimeout)
{
	struct VoiceHeader Header;
	struct tm *Localtime;

	TIO		TermIO;
	TIO		SaveIO;
	int		FD;
	char		LineIn[MODEM_BUFFER_LEN + 1];
	char		LineOut[MODEM_BUFFER_LEN + 1];
	char		Temp[30];
	char		Name[PATH_MAX + 1];
	int		BytesIn;
	int		BytesOut;
	int		HaveDLE		= FALSE;
	int		HaveTimeout	= FALSE;
	int		i;                              
	time_t	TimeStamp;

	if (*Touchtones != '*') *Touchtones = 0;

		/* Name der Nachricht zum Speichern erzeugen. Als Name wird	*/
		/* <Datum + Zeit>-<CallerID> benutzt.								*/

	TimeStamp = time(NULL);
	Localtime = localtime(&TimeStamp);

	if (strftime(Temp, 28, "%y%m%d%H%M%S-", Localtime) != 13)
	{
		LogLine(L_ERROR, "Can't create name for recording.\n");
		
		False();
	}

	if (!*CallerID) strcpy(CallerID, "0");

	strcpy(Name, SPOOLDIR);
	strcat(Name, "/");
	strcat(Name, Username);
	strcat(Name, "/incoming/");
	strcat(Name, Temp);
	strcat(Name, CallerID);

	LogLine(L_DEBUG, "Recording \"%s\"...\n", Name);

		/* Einstellungen des Devices holen, FlowControl und Kompression	*/
		/* setzen.																			*/

	if (!ModemGetTermIO(&TermIO))
	{
		LogLine(L_ERROR, "Can't get terminal I/O settings (voice).\n");

		False();
	}
	
	SaveIO = TermIO;

	ModemSetFlowControl(&TermIO, MODEM_FLOW_XON_IN);

	TermIO.c_cc[VMIN]	 = ((MODEM_BUFFER_LEN > 0xFF) ? 0xFF : MODEM_BUFFER_LEN);
	TermIO.c_cc[VTIME] = 1;

	if (!ModemSetTermIO(&TermIO))
	{
		LogLine(L_ERROR, "Can't set terminal I/O settings (XON/XOFF).\n");

		False();
	}

	if (!SetCompression(GetCompression()))
	{
		LogLine(L_ERROR, "Can't set voice audio compressen or line mode.\n");

		ModemSetTermIO(&SaveIO);
		False();
	}

		/* Datei zum Speichern öffnen. Diese wird mit '--w-------' ge-	*/
		/* öffnet und erst zu '-rw-------' gesetzt, wenn die Daten ge-	*/
		/* schrieben wurden.															*/

	if ((FD = open(Name, O_WRONLY | O_CREAT | O_TRUNC, S_IWUSR)) == -1)
   {
   	LogLine(L_ERROR, "Can't open \"%s\".\n", Name);

		ModemSetTermIO(&SaveIO);
		False();
	}

		/* Datei zurücksetzen, mit der UID und GID des Benutzers	ver-	*/ 
		/* sehen und den Header schreiben.										*/

	truncate(Name, 0);

	chown(Name, UID, GID);
	chown(Name, UID, GID);

	SetVoiceHeader(&Header);

	if (write(FD, &Header, sizeof(struct VoiceHeader)) != sizeof(struct VoiceHeader))
	{
		LogLine(L_ERROR, "Can't write raw voice audio header.\n");

		ModemSetTermIO(&SaveIO);
		CloseFD(FD);
		Delete(Name);
		False();
	}

		/* Wenn kein Carrier mehr anliegt, wird die Aufzeichnung ge-	*/
		/* stoppt. Ansonsten wird mit dem abspeichern der Daten begon-	*/
		/* nen.																			*/

	if (ModemGetNoCarrier() == TRUE)
	{
		ModemSetTermIO(&SaveIO);
		CloseFD(FD);
		Delete(Name);
		Cancel();
	}

	if (ModemCommand("AT+VRX", "CONNECT") == 0)
	{
		LogLine(L_ERROR, "Can't start record mode.\n");

		ModemSetTermIO(&SaveIO);
		CloseFD(FD);
		Delete(Name);
		False();
	}

		/* Solange Daten aufzeichnen, bis der Timeout abgelaufen ist oder	*/
		/* der Anrufer aufgelegt hat.													*/

	StatusHandleDLE	= ST_NO_INPUT;
	VoiceActionStatus	= VOICE_ACTION_OK;

	ModemSetTimeout(Savetime);

	while (VoiceActionStatus == VOICE_ACTION_OK)
	{
		if ((BytesIn = ModemRawRead(LineIn, MODEM_BUFFER_LEN)) <= 0)
		{
			LogLine(L_ERROR, "Could not read audio data from modem.\n");

			VoiceActionStatus = VOICE_ACTION_STOP;
		}

		BytesOut = 0;
                         
		for (i = 0; ((i < BytesIn) && (VoiceActionStatus == VOICE_ACTION_OK)); i++)
		{
			if (HaveDLE)
			{
				HaveDLE = FALSE;
                              
				switch (LineIn[i])
				{
					case DLE:
						LineOut[BytesOut++] = DLE;
						break;

					case ETX:
						LogLine(L_JUNK, "Record: <DLE><ETX>\n");

						VoiceActionStatus = VOICE_ACTION_STOP;

						break;

					default:
						HandleDLE(DLE);
						HandleDLE(LineIn[i]);
						break;
				}
			}
			else
			{
				if (LineIn[i] == DLE)
				{
					HaveDLE = TRUE;
				}
				else LineOut[BytesOut++] = LineIn[i];
			}
		}

		if (BytesOut > 0)
		{
			LogLine(L_JUNK, "Record: <DATA %d incoming; %d outgoing>\n", BytesIn, BytesOut);
                         
			write(FD, LineOut, BytesOut);
		}

			/* Wenn ein Timeout zustande kam, <DLE><ETX> ans Modem	*/
			/* schicken. Im normalen Datenstrom sollte dann die Ant-	*/
			/* wort <DLE><ETX> gefunden werden.								*/

		if ((ModemGetTimeout()) || (VoiceActionStatus != VOICE_ACTION_OK))
		{
			if (!HaveTimeout)
			{
				if (!ModemGetNoCarrier())
				{
						/* Der Aufnahmemodus bricht ab, wenn ein anderes Zei-	*/
						/* chen als XON und XOFF ans Modem geschickt wird.		*/

					sprintf(LineIn, "%c%c%c%c", DLE, DC4, DLE, ETX);

					ModemRawWrite(LineIn, strlen(LineIn));
				}
	
				HaveTimeout = TRUE;
			}
			else VoiceActionStatus = VOICE_ACTION_STOP;
		}

		if (VoiceActionStatus == VOICE_ACTION_OK)
		{
			if ((FileExist(UserHome, ".vboxpark")) && (ParkCheck))
			{
				VoiceActionStatus = VOICE_ACTION_PARK;
				
				LogLine(L_DEBUG, "File \".vboxpark\" found - try to parking line...\n");
			}
	
			if (ModemGetNoCarrier()) VoiceActionStatus = VOICE_ACTION_HANGUP;
		}
	}

	ModemSetTimeout(0);
	CloseFD(FD);
	ModemSetTermIO(&SaveIO);

		/* Permissions der aufgenommenen Nachricht auf '-rw-------'	*/
		/* setzen.																	*/

	chmod(Name, S_IRUSR | S_IWUSR);
	chmod(Name, S_IRUSR | S_IWUSR);

		/* Nach beendigung der Aufnahme sollte das Modem mit einem	*/
		/* "VCON" antworten. Dieses "VCON" kann sich aber auch be-	*/
		/* reits im Buffer befinden (*muß* also nicht erscheinen).	*/

	ModemCommand("AT", "OK|VCON");
	ModemCommand("AT", "OK");

		/* Aufgezeichnete Nachricht mit einem Skript weiterverar-	*/
		/* beiten.																	*/

	MailMessage(Name, CallerID);

	if ((FileExist(UserHome, ".vboxpark")) && (!ModemGetNoCarrier()) && (ParkCheck))
	{
		ParkCheck = FALSE;

		if (PlayMessage("parkline.msg") == -1) Cancel();

			/* Kommando zum parken der Leitung an das Modem schicken. Wenn	*/
			/* korrekt geparkt wurde, wird '.vboxpark' gelöscht.				*/

		DeleteFile(UserHome, ".vboxpark");
	}

	if ((PlayTimeout) && (HaveTimeout) && (!ModemGetNoCarrier()))
	{
		if (PlayMessage("timeout.msg") == -1) Cancel();
	}

	if ((VoiceActionStatus == VOICE_ACTION_HANGUP) || (ModemGetNoCarrier()))
	{
		Cancel();
	}

	True();
}

/*************************************************************************
 ** GetCompression():																	**
 *************************************************************************/

static short GetCompression(void)
{
	short C;
	
	C = VOICE_COMPRESSION_MODE;

	if ((C < MIN_COMPRESSION_NUM) || (C > MAX_COMPRESSION_NUM)) C = 3;

	return(C);
}

/*************************************************************************
 ** SetCompression():																	**
 *************************************************************************/

static boolean SetCompression(short C)
{
	char Command[64];

	sprintf(Command, "AT+VSM=%d+VLS=2", C);

	LogLine(L_DEBUG, "Setting voice compression \"%s\"...\n", Compressions[C]);

	return(ModemCommand(Command, "OK|VCON"));
}

/*************************************************************************
 ** SetVoiceHeader():																	**
 *************************************************************************/

static void SetVoiceHeader(struct VoiceHeader *Header)
{
	memset(Header, 0, sizeof(struct VoiceHeader));

	strcpy(Header->Magic, VOICE_MAGIC);
	strcpy(Header->Modem, VOICE_MODEM);

	Header->Compression = htons(GetCompression());
}

/*************************************************************************
 ** HandleDLE(): Verarbeitet die DLE-Sequencen bei der Aufnahme.			**
 *************************************************************************/

static void HandleDLE(char Byte)
{
	LogLine(L_JUNK, "DLE: Got character ");
	LogChar(L_JUNK, Byte, FALSE);
	LogText(L_JUNK, " from modem (%s).\n", ((StatusHandleDLE == ST_NO_INPUT) ? "have no DLE" : "have DLE"));

	switch (StatusHandleDLE)
	{
		case ST_NO_INPUT:
               
			switch (Byte)
			{
				case DLE:
					StatusHandleDLE = ST_GOT_DLE;
					break;

				case XON:
					LogLine(L_WARN, "DLE: Received XON.\n");
					break;

				case XOFF:
					LogLine(L_WARN, "DLE: Received XOFF.\n");
					break;

				case NL:
				case CR:
					break;

				default:
					LogLine(L_ERROR, "DLE: Illegal modem answer ");
					LogChar(L_ERROR, Byte, FALSE);
					LogText(L_ERROR, ".\n");
					break;
			}
                                   
			return;

		case ST_GOT_DLE:
          
			switch (Byte)
			{
				case DC4:
					LogLine(L_DEBUG, "Found <DLE><DC4> -- Stop current action...\n");

					VoiceActionStatus = VOICE_ACTION_STOP;

					break;

				case ETX:
					LogLine(L_DEBUG, "Found <DLE><ETX> -- Cancel current action...\n");

					VoiceActionStatus = VOICE_ACTION_STOP;

					break;

					/* MFV-Ton 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, * oder # er-	*/
					/* kannt (Touchtone).											*/
	
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
				case '*':
				case '#':
					HandleTouchTone(Byte);
					break;

					/* Besetztzeichen erkannt */

				case 'b':
					LogLine(L_DEBUG, "Found busytone - Hangup...\n");

					VoiceActionStatus = VOICE_ACTION_HANGUP;

					break;
					
					/* T.30 Faxrufton erkannt */

				case 'c':
					LogLine(L_DEBUG, "Found T.30 faxtone - Cancel current action...\n");

					VoiceActionStatus = VOICE_ACTION_STOP;

					break;
					
					/* Datenträger eines Modems erkannt */
					
				case 'e':
					break;

					/* Wählton erkannt */

				case 'd':
					break;

					/* Quiet (Ruhe) erkannt. Das Modem hat erkannt, daß zu-	*/
					/* nächst Sprachdaten angekommen sind und eine Stille		*/
					/* folgte, die länger war als durch AT+VSD definiert.		*/

				case 'q':
					LogLine(L_DEBUG, "Found quiet - Cancel current action...\n");

					VoiceActionStatus = VOICE_ACTION_STOP;

					break;

					/* Silence (Stille) erkannt. Das Modem hat bei Beginn	*/
					/* des Sprachmodus keine Sprachdaten erkannt, aber es	*/
					/* wurde eine Stille erkannt, die länger war als dur-	*/
					/* ch AT+VSD definiert.											*/

				case 's':
					LogLine(L_DEBUG, "Found silence - Cancel current action...\n");

					VoiceActionStatus = VOICE_ACTION_STOP;

					break;

				default:
					LogLine(L_ERROR, "DLE: Illeagal <DLE> shielded code ");
					LogChar(L_ERROR, Byte, FALSE);
					LogText(L_ERROR, ".\n");
					break;
			}

			StatusHandleDLE = ST_NO_INPUT;
			return;
	}
}

/*************************************************************************
 ** HandleTouchTone():																	**
 *************************************************************************/

static void HandleTouchTone(char Tone)
{
	char Temp[2];

	if (Tone == '*') *Touchtones = 0;

	if (strlen(Touchtones) < TOUCHTONE_BUFFER_LEN)
	{
		Temp[0] = Tone;
		Temp[1] = 0;
		
		strcat(Touchtones, Temp);

		if ((Tone == '#') && (*Touchtones == '*'))
		{
			LogLine(L_INFO, "Touchtone sequence \"%s\" found.\n", Touchtones);
		}
	}
}
