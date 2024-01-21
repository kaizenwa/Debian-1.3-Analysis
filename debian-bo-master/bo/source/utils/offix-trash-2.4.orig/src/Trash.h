#ifndef __TRASHh__
#define __TRASHh__

#include <sys/param.h>
#include "SortedList.h"
#include <OffiX/DragAndDrop.h>

/* Trash directory entry */
struct TrashEntry
{
	long FileID;
	char OrigDir[MAXPATHLEN];
	char OrigName[MAXPATHLEN];
};

/* Files.cc */
int  InitTrashDir(char *);
void ReadTrashIndex(dndSortedList *);
void WriteTrashIndex(dndSortedList *);
int  TrashMove(char *,char *);
int  TrashDelete(char *);
void UpdateFileList(Widget,dndSortedList *);

/* Icon.cc */
void ReadIcons();
void UpdateIcon(int);

#endif
