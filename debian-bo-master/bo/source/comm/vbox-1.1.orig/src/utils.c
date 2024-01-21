/*
 * $Id: utils.c,v 1.2 1996/07/15 10:02:49 root Exp $
 *
 * $Log: utils.c,v $
 * Revision 1.2  1996/07/15 10:02:49  root
 * o Kleine Änderungen an den Headern.
 *
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <limits.h>

#include "utils.h"
#include "defines.h"

/*************************************************************************
 ** FileExist(): Prüft ob eine Datei existiert.									**
 *************************************************************************/

int FileExist(char *Path, char *File)
{
	char Name[PATH_MAX + 1];
	
	if (Path)
	{
		strncpy(Name, Path, PATH_MAX);
	
		if (File)
		{
			strncat(Name, "/", PATH_MAX - strlen(Name));
			strncat(Name, File, PATH_MAX - strlen(Name));
		}
		
		if (access(Name, F_OK) == 0) True();
	}
	
	False();
}

/*************************************************************************
 ** DeleteFile():																			**
 *************************************************************************/

void DeleteFile(char *Path, char *File)
{
	char Name[PATH_MAX + 1];
	
	if (Path)
	{
		strncpy(Name, Path, PATH_MAX);
	
		if (File)
		{
			strncat(Name, "/", PATH_MAX - strlen(Name));
			strncat(Name, File, PATH_MAX - strlen(Name));
		}
		
		unlink(Name);
		unlink(Name);
	}
}

/*************************************************************************
 ** TouchFile():																			**
 *************************************************************************/

int TouchFile(char *Path, char *File)
{
	char	Name[PATH_MAX + 1];
	int	FD;
	
	if (Path)
	{
		strncpy(Name, Path, PATH_MAX);
	
		if (File)
		{
			strncat(Name, "/", PATH_MAX - strlen(Name));
			strncat(Name, File, PATH_MAX - strlen(Name));
		}

		if ((FD = open(Name, O_WRONLY | O_CREAT | O_TRUNC, S_IWUSR | S_IRUSR)) == -1) False();

		CloseFD(FD);
		
		True();
	}
	
	False();
}

/*************************************************************************
 ** CopyString():																			**
 *************************************************************************/

void CopyString(char *Destination, char *Source, int Len)
{
	strncpy(Destination, Source, Len);

	Destination[Len] = 0;
}
