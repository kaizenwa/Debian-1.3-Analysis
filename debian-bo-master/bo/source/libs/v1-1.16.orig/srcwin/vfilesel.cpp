//===============================================================
// vfilesel.cxx - vFileSelect class functions - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vwin32.h>		// for Win 32 stuff
#include <v/vfilesel.h>		// our header
#include <v/vapp.h>
#include <v/vbasewin.h>

// Define static data of the class


    static char* defaultFilter =		// The default filter list
	"All files(*.*)|*.*|";


//===================>>> vFileSelect::vFileSelect <<<=======================
  vFileSelect::vFileSelect(vBaseWindow* bw, char* title) // constructor
  {
     _parentHWND = bw->winHwnd();	// track parent's HWND
     init();
  }

//===================>>> vFileSelect::vFileSelect <<<=======================
  vFileSelect::vFileSelect(vApp* aw, char* title)
  {
     _parentHWND = aw->winHwnd();	// track parent's HWND
     init();
  }

//===================>>> vFileSelect::init <<<=======================
  void vFileSelect::init()
  {
    memset(&ofn,0,sizeof(OPENFILENAME));	// zap entire thing
    ofn.lStructSize = sizeof(OPENFILENAME);
    ofn.hwndOwner = _parentHWND;
  }

//======================>>> vFileSelect::FileSelect <<<=======================
  int vFileSelect::FileSelect(const char* msg, char* filename,
	const int maxlen, char** filter, int& filterIndex)
  {
    //	Show the file selection dialog.
    //	returns 0 on cancel, 1 otherwise

    char fp[256];

    int filterI = filterIndex;

    filename[0] = 0;

    buildFilter(fp,filter,filterI);	// build a proper filter string

    ofn.lpstrFilter = fp;
    ofn.nFilterIndex = filterI;
    ofn.lpstrFile = filename;
    ofn.nMaxFile = maxlen;
    ofn.lpstrTitle = msg;
    ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST |
		OFN_HIDEREADONLY;

    if (::GetOpenFileName(&ofn))		// Call Windows dialog
      {
	strcpy(filename, (char*)ofn.lpstrFile);
	filterIndex = ofn.nFilterIndex - 1;
	return 1;
      }


    return 0;
  }


//======================>>> vFileSelect::FileSelectSave <<<=======================
  int vFileSelect::FileSelectSave(const char* msg, char* filename,
	const int maxlen, char** filter, int& filterIndex)
  {
    //	Show the file selection dialog.
    //	returns 0 on cancel, 1 otherwise

    char fp[256];

    int filterI = filterIndex;

    filename[0] = 0;

    buildFilter(fp,filter,filterI);	// build a proper filter string

    ofn.lpstrFilter = fp;
    ofn.nFilterIndex = filterI;
    ofn.lpstrFile = filename;
    ofn.nMaxFile = maxlen;
    ofn.lpstrTitle = msg;
    ofn.Flags = OFN_OVERWRITEPROMPT | OFN_HIDEREADONLY;

    if (::GetSaveFileName(&ofn))		// Call Windows dialog
      {
	strcpy(filename, (char*)ofn.lpstrFile);
	filterIndex = ofn.nFilterIndex - 1;
	return 1;
      }

    filename[0] = 0;
    return 0;
  }

//======================>>> vFileSelect::buildFilter <<<=======================
  void vFileSelect::buildFilter(char* fbuff, char** filter, int& fi)
  {
    if (filter == 0 || *filter[0] == 0)
      {
       strcpy(fbuff,defaultFilter);
       fi = 1;
      }
    else			// copy the filter over
      {
	*fbuff = 0;
	for (int ix = 0 ; filter[ix] != 0 && *filter[ix] != 0 ; ++ix)
	  {
            if (strlen(filter[ix])*2 + strlen(fbuff) > 255)
		break;		// safety bailout

	    // Since the V model doesn't have the description/filter
	    // organization, we will simply use the filter as the name

	    strcat(fbuff, filter[ix]); strcat(fbuff,"|");
	    strcat(fbuff, filter[ix]); strcat(fbuff,"|");
	  }
	fi = fi + 1;	// 1 based, not 0 based
      }

    // Fixup the filter now -- replacing | with 0 makes it what Windows needs

    for (char *cp = fbuff ; *cp ; ++cp)
      {
	if (*cp == '|')
	    *cp = 0;
      }

   }
