//===============================================================
// vfilesel.h - reply modal dialog
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#ifndef VFILESEL_H
#define VFILESEL_H
#include <v/vmodald.h>

#define maxFiles 400
#define maxFileLen 200

    class vApp;
    class vFileSelect : protected vModalDialog
      {
      public:		//---------------------------------------- public
	vFileSelect(vBaseWindow* bw, char* title = "File Select") : 
	    vModalDialog(bw,title) { _cmdsAdded = 0; }
	vFileSelect(vApp* aw, char* title = "File Select") : 
	    vModalDialog(aw,title) { _cmdsAdded = 0; }
	~vFileSelect() {}

	int FileSelect(const char* msg, char* filename, 
	    const int maxlen, char** filter, int& filterIndex);
	int FileSelectSave(const char* msg, char* filename, 
	    const int maxlen, char** filter, int& filterIndex);

      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private
	int curSel;
	int _cmdsAdded;
	int _curFilter;
	int _showHidden;

	const char* fileFilter;
	char **filterPtr;		// ptr to filter list
	char curDir[maxFileLen];	// to hold current dir
	char origFile[maxFileLen];	// to hold original file
	char* fileList[maxFiles];	// the list of files

	virtual void DialogCommand(ItemVal id, ItemVal val, CmdType ctype);

	void GetDirList(const char *dir, const char* wild, 
	    char** filelist, const int max);

      };
#endif
