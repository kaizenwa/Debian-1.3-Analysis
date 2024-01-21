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

static	char	*rcsid = "$Id: command.c,v 143.1 1996/09/16 09:08:29 nau Exp $";

/* executes commands from user
 */

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "global.h"

#include "buffer.h"
#include "command.h"
#include "data.h"
#include "dialog.h"
#include "error.h"
#include "file.h"
#include "fileselect.h"
#include "mymem.h"
#include "misc.h"
#include "set.h"

/* ---------------------------------------------------------------------------
 * some local types
 */
typedef int		(*FunctionTypePtr)(void);

typedef struct
{
	char			*Text;			/* commandstring */
	FunctionTypePtr	Function;		/* function pointer */
} CommandType, *CommandTypePtr;

/* ---------------------------------------------------------------------------
 * local prototypes
 */
static	int		CommandLoadElementToBuffer(void);
static	int		CommandLoadLayoutToBuffer(void);
static	int		CommandLoadLayout(void);
static	int		CommandSaveLayout(void);
static	int		CommandSaveLayoutAndQuit(void);
static	int		CommandQuit(void);
static	int		SetArgcArgv(char *);

/* ---------------------------------------------------------------------------
 * some local identifiers
 */
static	CommandType	Command[] = {
	{ "le",	CommandLoadElementToBuffer },
	{ "m",	CommandLoadLayoutToBuffer },
	{ "s",	CommandSaveLayout },
	{ "w",	CommandSaveLayout },
	{ "wq",	CommandSaveLayoutAndQuit },
	{ "q",	CommandQuit },
	{ "l",	CommandLoadLayout }};

static	char	**argv;					/* command-line of arguments */
static	int		argc;

/* ---------------------------------------------------------------------------
 * quits the application after confirming
 * syntax: q
 */
static int CommandQuit(void)
{
	if (argc != 1)
	{
		Message("Usage: q\n  quits the application\n");
		return(1);
	}
	if (!PCB->Changed || ConfirmDialog("OK to lose data ?"))
		QuitApplication();

	return(0);	
}

/* ---------------------------------------------------------------------------
 * loads an element into the current buffer
 * syntax: le [name]
 */
static int CommandLoadElementToBuffer(void)
{
	char	*filename;

	switch(argc)
	{
		case 1:		/* open fileselect box */
			filename = FileSelectBox("load element to buffer:", NULL,
				Settings.ElementPath);
			if (filename && LoadElementToBuffer(PASTEBUFFER, filename, True))
				SetMode(PASTEBUFFER_MODE);
			break;
			
		case 2:		/* filename is passed in commandline */
			filename = argv[1];
			if (filename && LoadElementToBuffer(PASTEBUFFER, filename, True))
				SetMode(PASTEBUFFER_MODE);
			break;

		default:	/* usage */
			Message(False,"Usage: le [name]\n  loads element data to buffer\n");
			return(1);
	}
	return(0);
}

/* ---------------------------------------------------------------------------
 * loads a layout into the current buffer
 * syntax: m [name]
 */
static int CommandLoadLayoutToBuffer(void)
{
	char	*filename;

	switch(argc)
	{
		case 1:		/* open fileselect box */
			filename = FileSelectBox("load layout to buffer:", NULL,
				Settings.FilePath);
			if (filename && LoadLayoutToBuffer(PASTEBUFFER, filename))
				SetMode(PASTEBUFFER_MODE);
			break;
			
		case 2:		/* filename is passed in commandline */
			filename = argv[1];
			if (filename && LoadLayoutToBuffer(PASTEBUFFER, filename))
				SetMode(PASTEBUFFER_MODE);
			break;

		default:	/* usage */
			Message("Usage: m [name]\n  loads layout data to buffer\n");
			return(1);
	}
	return(0);
}

/* ---------------------------------------------------------------------------
 * saves the layout data and quits
 */
static int CommandSaveLayoutAndQuit(void)
{
	if (argc != 1 && argc != 2)
	{
		Message("Usage: wq [name]\n  saves layout data and quits\n");
		return(1);
	}
	if (!CommandSaveLayout())
	{
		if (!PCB->Changed || ConfirmDialog("OK to lose data ?"))
			QuitApplication();
		return(0);
	}
	return(1);
}

/* ----------------------------------------------------------------------
 * saves layout data
 * syntax: l name
 */
static int CommandSaveLayout(void)
{
	char	*filename;

	switch(argc)
	{
		case 1:			/* query name if necessary */
			if (!PCB->Filename)
			{
				filename = FileSelectBox("save layout as:", NULL,
					Settings.FilePath);
				if (filename)
					SavePCB(filename);
			}
			else
				SavePCB(PCB->Filename);
			break;

		case 2:
			SavePCB(argv[1]);
			break;

		default:
			Message("Usage: s [name] | w [name]\n  saves layout data\n");
			return(1);
	}
	return(0);
}

/* ----------------------------------------------------------------------
 * loads layout data
 * syntax: l [name]
 */
static int CommandLoadLayout(void)
{
	char	*filename;

	switch(argc)
	{
		case 1:		/* open fileselect box */
			filename = FileSelectBox("load file:", NULL, Settings.FilePath);
			if (!filename)
				return(0);
			break;
			
		case 2:		/* filename is passed in commandline */
			filename = argv[1];
			break;

		default:	/* usage */
			Message("Usage: l [name]\n  loads layout data\n");
			return(1);
	}

	if (!PCB->Changed || ConfirmDialog("OK to override layout data?"))
		LoadPCB(filename);
	return(0);
}

/* ----------------------------------------------------------------------
 * split commandline and fill argv/argc
 */
static int SetArgcArgv(char *Line)
{
	static	int		maxcount = 0;
			char	*p;

	for(argc = 0, p = strtok(Line, " \t;"); p; p = strtok(NULL, " \t;"))
	{
			/* allocate more memory */
		if (argc >= maxcount)
		{
			maxcount += 20;
			argv = (char **) MyRealloc(argv, maxcount*sizeof(char *),
				"SetArgcArgv()");
		}
		argv[argc++] = p;
	}
	return(argc);
}

/* ---------------------------------------------------------------------------
 * command is passed in, memory is already allocated
 */
void ExecuteUserCommand(char *CommandLine)
{
	int		i;

	if (SetArgcArgv(CommandLine))
	{
			/* scan command list */
		for (i = 0; i < ENTRIES(Command); i++)
			if (!strcmp(Command[i].Text, argv[0]))
				break;
		if (i == ENTRIES(Command))
		{
			Message("following commands are supported:\n"
				"  l  [file]       load layout\n"
				"  le [file]       load element to buffer\n"
				"  m  [file]       load layout to buffer (merge)\n"
				"  q               quits the application\n"
				"  s  [file]       save layout\n"
				"  w  [file]       save layout\n"
				"  wq [file]       save layout and quit\n");
		}
		else
			if (Command[i].Function())
				XBell(Dpy, Settings.Volume);
	}
	else
		XBell(Dpy, Settings.Volume);
}

