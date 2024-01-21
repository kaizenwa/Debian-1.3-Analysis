/*
 *                            COPYRIGHT
 *
 *  PCB, interactive printed circuit board design
 *  Copyright (C) 1994,1995,1996 Thomas Nau
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Contact addresses for paper mail and Email:
 *  Thomas Nau, Schlehenweg 15, 88471 Baustetten, Germany
 *  Thomas.Nau@rz.uni-ulm.de
 *
 */

static	char	*rcsid = "$Id: file.c,v 143.1 1996/09/16 09:08:36 nau Exp $";

/* file save, load, merge ... routines
 * getpid() needs a cast to (int) to get rid of compiler warnings
 * on several architectures
 */

#include <stdlib.h>
#include <pwd.h>
#include <time.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>

#include "global.h"

#include "create.h"
#include "crosshair.h"
#include "data.h"
#include "dialog.h"
#include "error.h"
#include "file.h"
#include "mymem.h"
#include "misc.h"
#include "parse_l.h"
#include "remove.h"
#include "set.h"

#if !defined(HAS_ATEXIT) && !defined(HAS_ON_EXIT)
/* ---------------------------------------------------------------------------
 * some local identifiers for OS without an atexit() or on_exit()
 * call
 */
static	char	TMPFilename[80];
#endif

/* ---------------------------------------------------------------------------
 * some local prototypes
 */
static	void	PrintQuotedString(FILE *, char *);
static	void	WritePCBInfoHeader(FILE *);
static	void	WritePCBDataHeader(FILE *);
static	void	WritePCBFontData(FILE *);
static	void	WritePCBViaData(FILE *);
static	void	WritePCBElementData(FILE *);
static	void	WritePCBLayerData(FILE *, Cardinal);
static	int		WritePCB(FILE *);
static	int		WritePCBFile(char *);
static	int		WritePCBPipe(char *);

/* ---------------------------------------------------------------------------
 * opens a file and check if it exists
 */
FILE *CheckAndOpenFile(char *Filename,
	Boolean Confirm, Boolean AllButton, Boolean *WasAllButton)
{
	FILE		*fp = NULL;
	struct stat	buffer;
	char		message[MAXPATHLEN+80];

	if (Filename && *Filename)
	{
		if (!stat(Filename, &buffer) && Confirm)
		{
			sprintf(message, "file '%s' exists, use anyway?", Filename);
			*WasAllButton = False;
			switch(ConfirmReplaceFileDialog(message, AllButton))
			{
				case ALL_BUTTON:	*WasAllButton = True; break;
				case CANCEL_BUTTON: return(NULL);
			}
		}
		if ((fp = fopen(Filename, "w")) == NULL)
			OpenErrorMessage(Filename);
	}
	return(fp);
}

/* ---------------------------------------------------------------------------
 * opens a file for saving connection data
 */
FILE *OpenConnectionDataFile(void)
{
	char	*filename;
	Boolean	result;		/* not used */

	filename = GetUserInput("enter filename to save connection data:", "");
	return(CheckAndOpenFile(filename, True, False, &result));
}

/* ---------------------------------------------------------------------------
 * save PCB
 */
int SavePCB(char *Filename)
{
	int		retcode;
	char	*copy;

	if (!(retcode = WritePCBPipe(Filename)))
	{
			/* thanks to Nick Bailey for the bug-fix;
			 * first of all make a copy of the passed filename because
			 * it might be identical to 'PCB->Filename'
			 */
		copy = MyStrdup(Filename, "SavePCB()");
		SaveFree(PCB->Filename);
		PCB->Filename = copy;
		SetChangedFlag(False);
	}
	return(retcode);
}

/* ---------------------------------------------------------------------------
 * load PCB
 * parse the file with enabled 'PCB mode' (see parser)
 * if successful, update some other stuff
 */
int LoadPCB(char *Filename)
{
	PCBTypePtr	newPCB = CreateNewPCB(False);
	int			save;

		/* new data isn't added to the undo list */
	if (!ParsePCB(newPCB, Filename))
	{
		RemovePCB(PCB);
		PCB = newPCB;
		ResetStackAndVisibility();

			/* update cursor location */
		Crosshair.X = MIN(PCB->CursorX, (Position) PCB->MaxWidth);
		Crosshair.Y = MIN(PCB->CursorY, (Position) PCB->MaxHeight);

			/* update zoom information. Presetting it to a not allowed
			 * value makes sure that the routine is forced to
			 * initialize some things
			 * SetZoom() also calls CenterDisplay()
			 */
		save = PCB->Zoom;
		PCB->Zoom = MAX_ZOOM+1;
		SetZoom(save);

			/* update cursor confinement and output area (scrollbars) */
		ChangePCBSize(PCB->MaxWidth, PCB->MaxHeight);

			/* create default font if necessary */
		if (!PCB->Font.Valid)
		{
			Message("file '%s' has no font information\n"
				"  default font will be used\n", Filename);
			CreateDefaultFont();
		}

			/* clear 'changed flag' */
		SetChangedFlag(False);
		PCB->Filename = MyStrdup(Filename, "LoadPCB()");
		UpdateSettingsOnScreen();
		return(0);
	}

		/* release unused memory */
	RemovePCB(newPCB);
	return(1);
}

/* ---------------------------------------------------------------------------
 * writes the quoted string created by another subroutine
 */
static void PrintQuotedString(FILE *FP, char *S)
{
	static	DynamicStringType	ds;

	CreateQuotedString(&ds, S);
	fputs(ds.Data, FP);
}

/* ---------------------------------------------------------------------------
 * writes layout header information
 * date, UID and name of user
 */
static void WritePCBInfoHeader(FILE *FP)
{
	struct passwd	*pwentry;
	struct hostent	*hostentry;
	time_t			currenttime;
	char			hostname[256];

		/* write some useful comments */
	currenttime = time(NULL);
	pwentry = getpwuid(getuid());
	fprintf(FP, "# release: %s "RELEASE"\n", Progname);
	fprintf(FP, "# date:    %s", asctime(localtime(&currenttime)));
	fprintf(FP, "# user:    %s (%s)\n", pwentry->pw_name, pwentry->pw_gecos);
	if (gethostname(hostname, 255) != -1)
	{
		hostentry = gethostbyname(hostname); 
		fprintf(FP, "# host:    %s\n",hostentry ? hostentry->h_name : hostname);
	}
}

/* ---------------------------------------------------------------------------
 * writes data header
 * the name of the PCB, cursor location, zoom and grid
 * layergroups and some flags
 */
static void WritePCBDataHeader(FILE *FP)
{
	Cardinal	group,
				entry;

	fputs("\nPCB(", FP);
	PrintQuotedString(FP, EMPTY(PCB->Name));
	fprintf(FP, " %i %i)\n\n",
		(int) PCB->MaxWidth, (int) PCB->MaxHeight);
	fprintf(FP, "Grid(%i %i %i)\n",
		(int) PCB->Grid, (int) PCB->GridOffsetX, (int) PCB->GridOffsetY);
	fprintf(FP, "Cursor(%i %i %i)\n",
		(int) Crosshair.X, (int) Crosshair.Y, PCB->Zoom);
	fprintf(FP, "Flags(0x%04x)\n", (int) PCB->Flags);
	fputs("Groups(\"", FP);
	for (group = 0; group < MAX_LAYER; group++)
		if (PCB->LayerGroups.Number[group])
		{
			for (entry = 0; entry < PCB->LayerGroups.Number[group]; entry++)
			{
				switch(PCB->LayerGroups.Entries[group][entry])
				{
					case MAX_LAYER +COMPONENT_LAYER:
						fputc('c', FP);
						break;

					case MAX_LAYER +SOLDER_LAYER:
						fputc('s', FP);
						break;

					default:
						fprintf(FP, "%d",
							PCB->LayerGroups.Entries[group][entry]+1);
						break;
				}
				if (entry != PCB->LayerGroups.Number[group]-1)
					fputc(',', FP);
			}
			if (group != MAX_LAYER-1)
				fputc(':', FP);
		}
	fputs("\")\n\n", FP);
}

/* ---------------------------------------------------------------------------
 * writes font data of non empty symbols
 */
static void WritePCBFontData(FILE *FP)
{
	Cardinal	i, j;
	LineTypePtr	line;
	FontTypePtr	font;

	for (font = &PCB->Font, i = 0; i <= MAX_FONTPOSITION; i++)
	{
		if (!font->Symbol[i].Valid)
			continue;

		if (isprint(i))
			fprintf(FP, "Symbol('%c' %i)\n(\n",
				(char) i, (int) font->Symbol[i].Delta);
		else
			fprintf(FP, "Symbol(%i %i)\n(\n", i, (int) font->Symbol[i].Delta);

		line = font->Symbol[i].Line;
		for (j = font->Symbol[i].LineN; j; j --, line++)
			fprintf(FP, "\tSymbolLine(%i %i %i %i %i)\n",
				(int) line->Point1.X, (int) line->Point1.Y,
				(int) line->Point2.X, (int) line->Point2.Y,
				(int) line->Thickness);
		fputs(")\n", FP);
	}
}

/* ---------------------------------------------------------------------------
 * writes via data
 */
static void WritePCBViaData(FILE *FP)
{
		/* write information about vias */
	VIA_LOOP(PCB->Data,
		fprintf(FP, "Via(%i %i %i %i ",
			(int) via->X, (int) via->Y,
			(int) via->Thickness, (int) via->DrillingHole);
		PrintQuotedString(FP, EMPTY(via->Name));
		fprintf(FP, " 0x%04x)\n", (int) via->Flags);

	);

}

/* ---------------------------------------------------------------------------
 * writes element data
 */
static void WritePCBElementData(FILE *FP)
{
	ELEMENT_LOOP(PCB->Data,
			/* only non empty elements */
		if (!element->LineN && !element->PinN &&
			!element->ArcN)
			continue;

				/* the coordinates and text-flags are the same for
				 * both names of an element
				 */
		fprintf(FP, "\nElement(0x%04x ", (int) element->Flags);
		PrintQuotedString(FP, EMPTY(DESCRIPTION_NAME(element)));
		fputc(' ', FP);
		PrintQuotedString(FP, EMPTY(NAMEONPCB_NAME(element)));
		fputc(' ', FP);
		PrintQuotedString(FP, EMPTY(VALUE_NAME(element)));
		fprintf(FP, " %i %i %i %i 0x%04x)\n(\n",
				(int) DESCRIPTION_TEXT(element).X,
				(int) DESCRIPTION_TEXT(element).Y,
				(int) DESCRIPTION_TEXT(element).Direction,
				(int) DESCRIPTION_TEXT(element).Scale,
					(int) DESCRIPTION_TEXT(element).Flags);

		PIN_LOOP(element,
			fprintf(FP, "\tPin(%i %i %i %i ",
				(int) pin->X, (int) pin->Y,
				(int) pin->Thickness, (int) pin->DrillingHole);
			PrintQuotedString(FP, EMPTY(pin->Name));
			fprintf(FP, " 0x%04x)\n", (int) pin->Flags);
		);
		PAD_LOOP(element,
			fprintf(FP, "\tPad(%i %i %i %i %i ",
				(int) pad->Point1.X, (int) pad->Point1.Y,
				(int) pad->Point2.X, (int) pad->Point2.Y,
				(int) pad->Thickness);
			PrintQuotedString(FP, EMPTY(pad->Name));
			fprintf(FP, " 0x%04x)\n", (int) pad->Flags);
		);

		ELEMENTLINE_LOOP(element,
			fprintf(FP, "\tElementLine (%i %i %i %i %i)\n",
				(int) line->Point1.X, (int) line->Point1.Y,
				(int) line->Point2.X, (int) line->Point2.Y,
				(int) line->Thickness);
		);

		ARC_LOOP(element,
			fprintf(FP, "\tElementArc (%i %i %i %i %i %i %i)\n",
				(int) arc->X, (int) arc->Y,
				(int) arc->Width, (int) arc->Height,
				(int) arc->StartAngle, (int) arc->Delta,
				(int) arc->Thickness);
		);

		fprintf(FP, "\tMark (%i %i)\n)\n",
			(int) element->MarkX, (int) element->MarkY);
	);
}

/* ---------------------------------------------------------------------------
 * writes layer data
 */
static void WritePCBLayerData(FILE *FP, Cardinal Number)
{
	LayerTypePtr	layer = &PCB->Data->Layer[Number];

		/* write information about non empty layers */
	if (layer->LineN || layer->TextN || layer->PolygonN || 
		(layer->Name && *layer->Name))
	{
		fprintf(FP, "Layer(%i ", (int) Number+1);
		PrintQuotedString(FP, EMPTY(layer->Name));
		fputs(")\n(\n", FP);

		LINE_LOOP(layer,
			fprintf(FP, "\tLine(%i %i %i %i %i 0x%04x)\n",
				(int) line->Point1.X, (int) line->Point1.Y,
				(int) line->Point2.X, (int) line->Point2.Y,
				(int) line->Thickness, (int) line->Flags);
		);
		TEXT_LOOP(layer,
			fprintf(FP, "\tText(%i %i %i %i ",
				(int) text->X, (int) text->Y,
				(int) text->Direction, (int) text->Scale);
			PrintQuotedString(FP, EMPTY(text->TextString));
			fprintf(FP, " 0x%04x)\n", (int) text->Flags);
		);
		POLYGON_LOOP(layer,
			{
				int	i = 0;

				fprintf(FP, "\tPolygon(0x%04x)\n\t(", polygon->Flags);
				POLYGONPOINT_LOOP(polygon,
					{
						if (i++ % 5 == 0)
							fputs("\n\t\t", FP);
						fprintf(FP, "(%i %i) ",
							(int) point->X, (int) point->Y);
					}
				);
				fputs("\n\t)\n", FP);
			}
		);

		fputs(")\n", FP);
	}
}

/* ---------------------------------------------------------------------------
 * writes PCB to file
 */
static int WritePCB(FILE *FP)
{
	Cardinal	i;

	WritePCBInfoHeader(FP);
	WritePCBDataHeader(FP);
	WritePCBFontData(FP);
	WritePCBViaData(FP);
	WritePCBElementData(FP);
	for (i = 0; i < MAX_LAYER; i++)
		WritePCBLayerData(FP, i);

	return(STATUS_OK);
}

/* ---------------------------------------------------------------------------
 * writes PCB to file
 */
static int WritePCBFile(char *Filename)
{
	FILE	*fp;
	int		result;

	if ((fp = fopen(Filename, "w")) == NULL)
	{
		OpenErrorMessage(Filename);
		return(STATUS_ERROR);
	}
	result = WritePCB(fp);
	fclose(fp);
	return(result);
}

/* ---------------------------------------------------------------------------
 * writes PCB to pipe using the command defined by Settings.SaveCommand
 * %f are replaced by the passed filename
 */
static int WritePCBPipe(char *Filename)
{
			FILE				*fp;
			int					result;
			char				*p;
	static  DynamicStringType	command;

		/* setup commandline */
	DSClearString(&command);
	for (p = Settings.SaveCommand; *p; p++)
	{
			/* copy character if not special or add string to command */
		if (!(*p == '%' && *(p+1) == 'f'))
			DSAddCharacter(&command, *p);
		else
		{
			DSAddString(&command, Filename);

				/* skip the character */
			p++;
		}
	}
	DSAddCharacter(&command, '\0');

	if ((fp = popen(command.Data, "w")) == NULL)
	{
		PopenErrorMessage(command.Data);
		return(STATUS_ERROR);
	}
	result = WritePCB(fp);
	return(pclose(fp) ? STATUS_ERROR : result);
}

#if !defined(HAS_ATEXIT) && defined(HAS_ON_EXIT)
/* ---------------------------------------------------------------------------
 * just a glue function for systems with on_exit()
 */
void GlueEmergencySave(int status, caddr_t arg)
{
	EmergencySave();
}
#endif

/* ---------------------------------------------------------------------------
 * saves the layout in a temporary file
 * this is used for fatal errors and does not call the program specified
 * in 'saveCommand' for savety reasons
 */
void SaveInTMP(void)
{
	char	filename[80];

		/* memory might have been released before this function is called */
	if (PCB && PCB->Changed)
	{
		sprintf(filename, EMERGENCY_NAME, (int) getpid());
		Message("trying to save your layout in '%s'\n", filename);
		WritePCBFile(filename);
	}
}

/* ---------------------------------------------------------------------------
 * front-end for 'SaveInTMP()'
 * justs makes sure that the routine is only called once
 */
void EmergencySave(void)
{
	static	Boolean	already_called = False;

	if (!already_called)
	{
			/* in case of an emergency it's necessary to use the original
			 * error descriptor
			 */
		RestoreStderr();
		SaveInTMP();
		already_called = True;
	}
}

/* ---------------------------------------------------------------------------
 * creates backup file
 */
void Backup(void)
{
	char    filename[80];

	sprintf(filename, BACKUP_NAME, (int) getpid());
	WritePCBFile(filename);
}

#if !defined(HAS_ATEXIT) && !defined(HAS_ON_EXIT)
/* ---------------------------------------------------------------------------
 * makes a temporary copy of the data. This is useful for systems which
 * doesn't support calling functions on exit. We use this to save the data
 * before LEX and YACC functions are called because they are able to abort
 * the program.
 */
void SaveTMPData(void)
{
	sprintf(TMPFilename, EMERGENCY_NAME, (int) getpid());
	WritePCBFile(TMPFilename);
}

/* ---------------------------------------------------------------------------
 * removes the temporary copy of the data file
 */
void RemoveTMPData(void)
{
	unlink(TMPFilename);
}
#endif

/* ---------------------------------------------------------------------------
 * read contents of the library description file
 */
int ReadLibraryContents(void)
{
	static	char				*command = NULL;
			char				inputline[MAX_LIBRARY_LINE_LENGTH+1];
			FILE				*resultFP;
			LibraryMenuTypePtr	menu = Library.Menu;
			LibraryEntryTypePtr	entry;

		/* release old command string */
	MyFree(&command);
	command = EvaluateFilename(Settings.LibraryContentsCommand,
		Settings.LibraryPath, Settings.LibraryFilename, NULL);

		/* open a pipe to the output of the command */
	if ((resultFP = popen(command, "r")) == NULL)
	{
		PopenErrorMessage(command);
		return(1);
	}

		/* the library contents are seperated by colons;
		 * template : package : name : description
		 */
	while(fgets(inputline, MAX_LIBRARY_LINE_LENGTH, resultFP))
	{
		size_t	len = strlen(inputline);

			/* check for maximum linelength */
		if (len)
		{
			len--;
			if (inputline[len] != '\n')
				Message("linelength (%i) exceeded; following characters will be ignored\n", MAX_LIBRARY_LINE_LENGTH);
			else
				inputline[len] = '\0';
		}

			/* if the line defines a menu */
		if (!strncmp(inputline, "TYPE=", 5))
		{
			menu = GetLibraryMenuMemory();
			menu->Name = MyStrdup(UNKNOWN(&inputline[5]),
				"ReadLibraryDescription()");
		}
		else
		{
				/* allocate a new menu entry if not already done */
			if (!menu)
			{
				menu = GetLibraryMenuMemory();
				menu->Name = MyStrdup(UNKNOWN((char *) NULL),
					"ReadLibraryDescription()");
			}
			entry = GetLibraryEntryMemory(menu);
			entry->AllocatedMemory = MyStrdup(inputline,
				"ReadLibraryDescription()");

				/* now break the line into pieces separated by colons */
			if ((entry->Template = strtok(entry->AllocatedMemory, ":")) != NULL)
				if ((entry->Package= strtok(NULL, ":")) != NULL)
					if ((entry->Value = strtok(NULL, ":")) != NULL)
						entry->Description = strtok(NULL, ":");

				/* create the list entry */
			len = strlen(EMPTY(entry->Value)) +
				strlen(EMPTY(entry->Description)) +3;
			entry->ListEntry = MyCalloc(len, sizeof(char),
			 	"ReadLibraryDescription()");
			sprintf(entry->ListEntry,
				"%s, %s", EMPTY(entry->Value), EMPTY(entry->Description));
		}
	}

	return(pclose(resultFP));
}
