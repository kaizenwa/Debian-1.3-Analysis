//===============================================================
// vfilesel.cxx - vFileSelect class functions - X11R5
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vfilesel.h>		// our header
#include <v/vnotice.h>		// we use notice
#include <v/vicon.h>		// for icons

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
//#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

// Define static data of the class

const int fsMSG = 1;
const int fsLFN = 2;
const int fsFN = 3;
const int fsLCD = 4;
const int fsCD = 5;
const int fsLFLT = 6;
const int fsFLT = 7;
const int fsLIST = 8;
const int fsSEL = 9;
const int fsHOME = 10;
const int fsSHOWDOT = 11;

#define FilterIndex 11
#define FileListIndex 12
#define LS "ls -1aLF"

    static char* filterList[] =		// The default filter list
      {
	"*", 0
      };

    static char* emptyList[] =
      {
	"./",
	"../",
	0
      };

#define home_width 24
#define home_height 16
static unsigned char home_bits[] = {
   0x00, 0x80, 0x1f, 0x00, 0x40, 0x20, 0x00, 0x20, 0x40, 0xff, 0xff, 0xff,
   0x01, 0x00, 0x80, 0x01, 0x00, 0x80, 0x01, 0x00, 0xb0, 0xf1, 0x30, 0x98,
   0xf9, 0x19, 0x8c, 0x99, 0x1f, 0x86, 0x0d, 0x0f, 0x83, 0x01, 0x80, 0x81,
   0x01, 0xc0, 0x80, 0x01, 0x00, 0x80, 0x01, 0x00, 0x80, 0xff, 0xff, 0xff};

    static vIcon home(&home_bits[0], home_height, home_width); // use icon for home

    static CommandObject FileSelectDialog[] =
      {
        {C_Label, fsMSG, 0, "Select File",NoList,
                CA_MainMsg,isSens,NoFrame, 0,0},

	{C_Label, fsLFN, 0, "File:", NoList,CA_None,isSens,NoFrame, 0,fsMSG},
        {C_TextIn, fsFN, 0, "", NoList,
                CA_Large,isSens,NoFrame,fsLFN,fsMSG},

	{C_Label, fsLCD, 0, " Dir:", NoList,CA_None,isSens,NoFrame, 0,fsLFN},
        {C_Label, fsCD, 0, ".", NoList,
                CA_None,isSens,NoFrame,fsLCD,fsLFN},

	{C_Label, fsLFLT, 0, "Filter:", NoList,CA_None,isSens,NoFrame,
		0,fsLCD},

	{C_Blank, 99, 0, " ",NoList,CA_None,isSens,NoFrame,0,fsLFLT},


	{C_IconButton, fsHOME, fsHOME, "Home",(void*)&home, CA_None,
                isSens,NoFrame,99,99},
        {C_Button, fsSEL, fsSEL, " Select ", NoList,CA_None,
                isSens,NoFrame,99,fsHOME},
        {C_Button, M_Cancel, M_Cancel, " Cancel ", NoList,CA_None,
                isSens,NoFrame,99,fsSEL},
        {C_Button, M_OK, M_OK, " OK ", NoList,CA_DefaultButton,
                isSens,NoFrame,99,M_Cancel},


	{C_ComboBox,fsFLT,0,"ComboBox",(void*)filterList,	// index 10
		 CA_None,isSens,NoFrame,fsLFLT,fsCD},

	{C_List, fsLIST, 0, "List", (void*)emptyList,	// index 11
		 CA_None,isSens,NoFrame,fsFLT,fsCD},

	{C_CheckBox,fsSHOWDOT,0,"Show \".hidden\"",NoList,CA_None,isSens,
		NoFrame,fsFLT,fsLIST},

        {C_EndOfList,0,0,0,0,CA_None,0,0,0}
      };


  static int dirsort(const char**, const char**);
  static int wild_match(const char* , const char*);

//======================>>> vFileSelect::FileSelect <<<=======================
  int vFileSelect::FileSelect(const char* msg, char* filename, 
	const int maxlen, char** filter, int& filterIndex)
  {
    //	Show the file selection dialog.
    //	returns 0 on cancel, 1 otherwise

    int ans;
    char temp[maxFileLen];

    if (!filename)
	return 0;

    _showHidden = 0;
    origFile[0] = 0;
    fileList[0] = 0;

    // If the user supplies a filter, use it. Otherwise, use the default.

    if (filter != 0 && *filter != 0)
      {
	_curFilter = filterIndex;	// which filter we used
	filterPtr = filter;		// so we can change
	fileFilter = filter[_curFilter];
	FileSelectDialog[FilterIndex].itemList = (void *)filter;
	FileSelectDialog[FilterIndex].retVal = _curFilter;
      }
    else
      {
	_curFilter = 0;
	filterPtr = filterList;		// so we can change
	fileFilter = filterList[0];
      }

    // Fill this in dynamically so we can have dynamic heap array

    FileSelectDialog[FileListIndex].itemList = (void *)fileList;

    GetDirList(".", fileFilter, fileList, maxFiles);

    curSel = -1;		// No file selected

    if (!_cmdsAdded)		// Only once
      {
	AddDialogCmds(FileSelectDialog);	// Set up standard dialog
	_cmdsAdded = 1;
      }

    // Read the current directory

    // Note: this SetString won't work on Windows...

    if (getcwd(temp,maxFileLen-1))	// get the current directory
	SetString(fsCD,temp);

    strcpy(curDir,temp);		// preload input with name

    if (*filename)		// provided a base name
	strcpy(origFile,filename);

    if (*filename != 0)
      {
	SetString(fsFN,filename);	// the filename provided
      }
    else
      {
	strcpy(temp,curDir);		// set to curdir
	if (strcmp(temp,"/") != 0)
	    strcat(temp,"/");
	SetString(fsFN,temp);
      }

    (void) ShowModalDialog(msg, ans);	// show and wait

    if (ans != M_Cancel)
      {
	(void) GetTextIn(fsFN, filename, maxlen);
	filterIndex = _curFilter;	// copy back out
      }

    return ans != M_Cancel;
  }

//======================>>> vFileSelect::FileSelect <<<=======================
  int vFileSelect::FileSelectSave(const char* msg, char* filename, 
	const int maxlen, char** filter, int& filterIndex)
  {
    return FileSelect(msg, filename, maxlen, filter, filterIndex);
  }

//====================>>> vFileSelect::DialogCommand <<<=======================
  void vFileSelect::DialogCommand(ItemVal id, ItemVal val, CmdType ctype)
  {
    char temp[maxFileLen];

    switch (id)
      {
	case fsFLT:		// new filter
	  {
	    if (*filterPtr[val] == 0)
		break;
	    _curFilter = val;
	    fileFilter = filterPtr[_curFilter];
	    GetDirList(".", fileFilter, fileList, maxFiles);
	    SetValue(fsLIST,0,ChangeList);
	    curSel = -1;		// No file selected
	    break;
	  }

	case fsLIST:		// item selected
	  {
	    if (val != curSel)
	      {
		curSel = val;
		break;
	      }
	    // else fall through to fsSEL!
	  }

	// WARNING - fall through from previous case!

	case fsSEL:		// Do select
	  {
	    if (curSel < 0)	// Make sure a valid selection
		break;
	    strcpy(temp,fileList[curSel]);
	    int len = strlen(temp);
	    if (temp[len-1] == '/')	// a directory!
	      {
		temp[len-1] = 0;	// kill the /
		GetDirList(temp, fileFilter, fileList, maxFiles);
		if (getcwd(curDir,maxFileLen-1))	// get the current directory
		    SetString(fsCD,curDir);
		else
		  {
		    SetString(fsCD,".");
		    strcpy(curDir,".");
		  }
		SetValue(fsLIST,0,ChangeList);
		strcpy(temp,curDir);
		if (strcmp(temp,"/") != 0)
		    strcat(temp,"/");
		if (*origFile)
		    strcat(temp,origFile);
		SetString(fsFN,temp);
		curSel = -1;		// No file selected
	      }
	    else
	      {
		// Build up the whole name, including the directory.
		strcpy(temp,curDir);	// the directory
		if (strcmp(temp,"/") != 0)
		    strcat(temp,"/");	// needs trailing /
		strcat(temp,fileList[curSel]); // the file
		strcpy(origFile,fileList[curSel]); // the file
		SetString(fsFN,temp);
	      }
	    break;
	  }

	case fsHOME:		// change to home directory
	  {
	    char *hp = getenv("HOME");

	    if (hp == 0)
		strcpy(temp,".");
	    else
		strcpy(temp,hp);
	    // now change to HOME
	    GetDirList(temp, fileFilter, fileList, maxFiles);
	    if (getcwd(curDir,maxFileLen-1))	// get the current directory
		SetString(fsCD,curDir);
	    else
	      {
		SetString(fsCD,".");
		strcpy(curDir,".");
	      }
	    SetValue(fsLIST,0,ChangeList);
	    strcpy(temp,curDir);
	    if (strcmp(temp,"/") != 0)
		strcat(temp,"/");
	    SetString(fsFN,temp);
	    curSel = -1;		// No file selected
	    break;
	  }

	case fsSHOWDOT:
 	  {
	    _showHidden = val;		// need to repaint
	    GetDirList(".", fileFilter, fileList, maxFiles);
	    SetValue(fsLIST,0,ChangeList);
	    curSel = -1;		// No file selected
	    break;
	  }

	case M_OK:
	  {
	    (void) GetTextIn(fsFN, temp, maxFileLen-1);
	    if (temp[strlen(temp)-1] == '/')	// no name picked!
	      {
		vNoticeDialog note(this);
		if (strlen(temp) < maxFileLen - 30)
		    strcat(temp,"\nis not a valid file name.");
		else
		    strcpy(temp,"You must specify a valid file name.");
		note.Notice(temp);
		id = 0;		// don't allow OK to exit
		break;			// break if invalid
	      }
	    // else fall through to default
	  }

	// WARNING - fall through from previous case!
	default:
	  {
	    vModalDialog::DialogCommand(id,val,ctype);
	    break;
	  }
      }

    if (id == M_OK || id == M_Cancel)
	CloseDialog();
  }

//====================>>> vFileSelect::GetDirList <<<=======================
  void vFileSelect::GetDirList(const char *dir, const char* wild, 
	char** filelist, const int max)
  {
    DIR *dirp;		// for reading a directory

    struct dirent *e;

    char fn[200];

    char** fl;

    struct stat	statbuf;		// for check

    // make sure we can do this
    if (stat(dir, &statbuf) < 0 || (statbuf.st_mode & S_IFDIR) == 0)
	return;

    if ((dirp = opendir(dir)) == NULL)
	return;

    if (chdir(dir) != 0)		// change to new directory
      {
	SysDebug1(BadVals,"Unable to chdir to %s\n",dir);
	return;
      }

    // ok - will build a new list, so it is safe to kill the old one.
    for (fl = filelist ; *fl ; ++fl)	// free the space
      {
	delete [] *fl;
	*fl = 0;
      }

    fl = filelist;

    int ix;
    for (ix = 0 ; ix < max ; )
      {
	if ((e = readdir(dirp)) == NULL)	// read entry
	    break;

	if (strlen(e->d_name) >= 199)		// just ignore too long names
	    continue;
	else
	    strcpy(fn,e->d_name);		// make a copy

	if (strcmp(fn,".") == 0)		// skip the "."
	  {
	    continue;
	  }
	if (strcmp(fn,"..") == 0)		// special entry
	  {
	    fl[ix] = new char[4];
	    strcpy(fl[ix],"../");		// make it look like ../
	    ix++;				// bump list counter
	    continue;
	  }
	if (!_showHidden && *fn == '.')		// hide hidden?
	  {
	    continue;
	  }
	if (stat(fn, &statbuf) < 0)		// need to stat for type
	  {
	    fl[ix] = new char[strlen(fn)+1];	// default action
	    strcpy(fl[ix], fn);
	    ix++;				// bump list counter
	    continue;
	  }
	// OK, to here, then have a stat check file type
	if ((statbuf.st_mode & S_IFDIR) != 0)	// this is a dir
	  {
	    fl[ix] = new char[strlen(fn)+2];	// space for name + /
	    strcpy(fl[ix], fn);
	    strcat(fl[ix],"/");
	    ix++;				// bump list counter
	    continue;
	  }
	// this would allow files only, but we will show all for now
	// if ((statbuf.st_mode & (S_IFMT & ~S_IFLNK)) == 0)
	else if (wild_match(fn,wild))
	  {
	    fl[ix] = new char[strlen(fn)+1];	// space for name + /
	    strcpy(fl[ix], fn);
	    ix++;				// bump list counter
	    continue;
	  }
      }

    fl[ix] = 0;		// mark end of the list

    closedir(dirp);	// close directory

    // sort the list
    qsort(fl, ix, sizeof(char *),
	(int (*)(const void *,const void *))dirsort);

  }

//==========================>>> wild_match <<<==============================
  static int wild_match(const char* file, const char* wild)
  {
    // compares list of file suffixes in form .x or *.x  e.g.:"*.c *.h"

    char curpat[100];
    int fileLen = strlen(file);

    if (wild == 0 || wild[0] == 0 || strcmp(wild,"*") == 0)
	return 1;

    char* pat = curpat;			// copy to current pattern

    for (const char *cp = wild ; ; )		// scan all suffixes
      {
	if (*cp == '*')			// ingore *'s - should be at front
	  {
	    ++cp;			// next char
	    continue;			// ignore the *
	  }
	// look for separator - means we've build a suffix to check
	if (*cp == ' ' || *cp == '\t' || *cp == 0 || *cp == ';')
	  {
	    *pat = 0;			// end pattern string
	    int patLen = strlen(curpat);  // length of pattern
	    if (fileLen < patLen)	// gotta be >=
	      {
		if (*cp == 0)		// out of matches, return 0
		    return 0;
	      }
	    // a match is possible
	    else if (strcmp(&file[fileLen-patLen],curpat) == 0)
		return 1;

	    pat = curpat;		// start over
	    while (*cp == ' ' || *cp == '\t' || *cp == ';' )
		++cp;			// skip whitespace
	    // at beginning of next pattern
	    if (*cp == 0)			// all done?
		break;
	  }
	else
	    *pat++ = *cp++;		// copy pattern
      }
    return 0;
  }

//==========================>>> dirsort <<<==============================
  static int dirsort(const char **a, const char **b)
  {
    // utility function to sort dir entries
    const char* astr = *a;
    const char* bstr = *b;
    const char* aend = astr + strlen(astr) - 1;
    const char* bend = bstr + strlen(bstr) - 1;

    if (strncmp(astr, "../", 3) == 0)	// this goes first
	return -1;
    if (strncmp(bstr, "../", 3) == 0)
	return  1;

    if (*aend == '/' && *bend != '/')	// directories are next
	return -1;
    if (*aend != '/' && *bend == '/')
	return  1;
    return strcmp(astr, bstr);		// just normal order
  }
