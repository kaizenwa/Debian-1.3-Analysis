/*
 *  Trash - the OffiX waste basket
 *  Copyright (C) 1996  César Crusius
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
 */
 
/* tab size = 8 */

#include "Trash.h"
#include <X11/StringDefs.h>
#include <libit/errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

struct stat JunkStat;

extern Widget InfoLabel;

/************************************************************************
 * 
 * InitTrashDir
 * 
 * Verify the existence of trash directory and the index file. Try to
 * create them if they do not exist. Return zero on error. On succes,
 * change the current directory to TrashDir.
 * 
 ************************************************************************/
int
InitTrashDir(char *TrashDir)
{
	char DestDir[MAXPATHLEN];
	int ErrStatus;

	/* Get the user's home directory */
	if(getenv("HOME")==NULL)
	{	fprintf(stderr,"Trash: Could not get HOME environment.\n");
		return 0;
	}
	sprintf(DestDir,"%s/%s",getenv("HOME"),TrashDir);
	
	/* Verify the existence of the trash directory. *
	 * Try to create if it does not exist           */
	ErrStatus=lstat(DestDir,&JunkStat);
	if(ErrStatus)
	{	fprintf(stderr,"Trash: Creating directory %s...\n",DestDir);
		ErrStatus=mkdir(DestDir, 0700);
		if(ErrStatus)
		{	fprintf(stderr,"Trash: Could not create!\n");
			return 0;
		}
	}
	
	/* Change the current directory */
	ErrStatus=chdir(DestDir);
	if(ErrStatus)
	{	fprintf(stderr,"Trash: could not chdir to trash dir!\n");
		return 0;
	}
	
	/* Verify the existence of the trash index. *
	 * Try to create if it does not exist       */
	ErrStatus=lstat("index.db",&JunkStat);
	if(ErrStatus)
	{	fprintf(stderr,"Trash: Creating trash index...\n");
		ErrStatus=open("index.db", O_WRONLY|O_CREAT|O_EXCL, 0600);
		if(ErrStatus==-1 || close(ErrStatus))
		{	fprintf(stderr,"Trash: Could not create!\n");
			return 0;
		}
	}

	/* Now we finally can return "1" */
	return 1;
}

/************************************************************************
 * 
 * Read trash index
 * 
 * Initalizes the files list. Recevies an empty list.
 * File syntax : #/dir/name
 * 		
 * 		#	number of the file in the trash directory
 * 		dir	original directory of the file
 * 		name	orignal name of the file
 * 
 ************************************************************************/
void
ReadTrashIndex(dndSortedList *List)
{
	FILE *IndexFile;
	char IndexLine[MAXPATHLEN];

	/* We can assume we are in the trash directory 	*
	 * and that the index file exists.		*/
	IndexFile=fopen("index.db","r");
	
	/* Now read the contents */
	for(fgets(IndexLine,MAXPATHLEN,IndexFile);
	    !feof(IndexFile);
	    fgets(IndexLine,MAXPATHLEN,IndexFile))
	{
		char *Index;
		*strchr(IndexLine,'\n')='\0';
		if(IndexLine[0]=='\0') continue;
		Index=strchr(IndexLine,'/');
		*Index='\0';
		
		/* If the file does not exist don't add it to the list */
		if(lstat(IndexLine,&JunkStat)) continue;

		/* Split the file name into dir+name */
		char *Name=Index;
		while(strchr(Name+1,'/')!=NULL) Name=strchr(Name+1,'/');
		*Index='/'; *Name='\0'; Name++;
		
		/* Create the new list entry */
		TrashEntry *NewEntry=new TrashEntry;
		NewEntry->FileID=atol(IndexLine);
		strcpy(NewEntry->OrigDir,Index);
		strcpy(NewEntry->OrigName,Name);
		List->add(NewEntry);
	}

	/* Done */
	fclose(IndexFile);
}

void
WriteTrashIndex(dndSortedList *List)
{
	FILE *IndexFile;

	/* We can assume we are in the trash directory 	*
	 * and that the index file exists.		*/
	IndexFile=fopen("index.db","w");
	
	/* Let's write it */
	TrashEntry *entry=(TrashEntry*)(List->getfirst());
	while(entry!=NULL)
	{
		fprintf(IndexFile,"%ld%s/%s\n",
			entry->FileID,
			entry->OrigDir,
			entry->OrigName);
		entry=(TrashEntry*)(List->getnext());
	}

	/* Done */
	fclose(IndexFile);
}

int
TrashMove(char *OldPath,char *NewPath)
{
	char CmdBuf[2*MAXPATHLEN+13];

	if(lstat(OldPath,&JunkStat)) return 1;
	if(!lstat(NewPath,&JunkStat)) return 0;
	
	int ErrStatus=rename(OldPath,NewPath);
	
	if (ErrStatus && errno==EXDEV)
	{
		sprintf(CmdBuf,"cp -r '%s' '%s'",OldPath,NewPath);
		if ((ErrStatus=system(CmdBuf))==0)
			if((ErrStatus=!TrashDelete(OldPath))!=0)
				TrashDelete(NewPath);
	}

	return (ErrStatus==0);
}

int
TrashDelete(char *OldPath)
{
	char Command[MAXPATHLEN];
	
	if(lstat(OldPath,&JunkStat)) return 1;
	
	sprintf(Command,"rm -rf %s",OldPath);
	int ErrStatus=system(Command);
	
	return (!ErrStatus);
}

#include <X11/Xaw/List.h>

static char **StringList;

void
UpdateFileList(Widget w,dndSortedList *List)
{
	if(StringList!=NULL)	delete StringList;
	StringList=NULL;
	
	dndListIndex ListSize=List->size();
	if(ListSize==0)
	{
		StringList=new char*;
		StringList[0]=NULL;
		XawListChange(w,StringList,0,0,False);
		WriteTrashIndex(List);
		XtVaSetValues(InfoLabel,XtNlabel, "File name:\nDirectory:",0);
		return;
	}
	
	StringList=new char*[ListSize];
	
	dndListIndex index=0;
	TrashEntry  *entry=(TrashEntry*)(List->getfirst());
	while(entry!=NULL)
	{
		StringList[index++]=entry->OrigName;
		entry=(TrashEntry*)(List->getnext());
	}
	
	XawListChange(w,StringList,(int)ListSize,0,False);
	WriteTrashIndex(List);
	XtVaSetValues(InfoLabel,XtNlabel, "File name:\nDirectory:",0);
}	
