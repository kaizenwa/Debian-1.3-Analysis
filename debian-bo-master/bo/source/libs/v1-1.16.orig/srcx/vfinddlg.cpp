//===============================================================
// vFind.cxx - vFindDialog class functions - X11R5
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vfinddlg.h>           // our header
#include <v/vicon.h>           // for icon

// Define static data of the class

    static CommandObject FindDialog[] =
      {
	{C_Label, 1, 1, "Find?",NoList,
		CA_None,isSens,NoFrame, 0,0},
        {C_TextIn, 2, 2, "", NoList,
		CA_Large,isSens,NoFrame,1,0},
	{C_Button, M_OK, M_OK, "  Find  ", NoList,CA_DefaultButton,
		isSens,NoFrame,2,0},
	{C_CheckBox, 3, 0,"Case Sensitive",NoList,CA_None,
		isSens,NoFrame,1,2},
        {C_Button, M_Cancel, M_Cancel, " Cancel ", NoList,CA_None,
		isSens,NoFrame,2,2},
        {C_EndOfList,0,0,0,0,CA_None,0,0,0}
      };

//======================>>> vFindDialog::AskFindPat <<<=======================
  int vFindDialog::AskFindPat(char* reply, const int maxlen, int& caseSens)
  {
    //	Show a message, wait for a reply
    //	no important return

    int ans;

    if (!reply)
	return 0;

    if (!added)
      {
	FindDialog[1].title = reply;
	FindDialog[3].retVal = caseSens;
	AddDialogCmds(FindDialog);		// Set up standard dialog
	added = 1;
      }

    if (*reply)
	SetString(2,reply);
    SetValue(3,caseSens,Value);
    (void) ShowModalDialog("", ans);	// show and WAIT

    reply[0] = 0;

    if (ans != M_Cancel)
      {
	(void) GetTextIn(2, reply, maxlen);
	caseSens = GetValue(3);
      }

   return ans == M_OK;
  }

//====================>>> vFindDialog::DialogCommand <<<=======================
  void vFindDialog::DialogCommand(ItemVal id, ItemVal val, CmdType ctype)
  {
    vModalDialog::DialogCommand(id,val,ctype);
//    if (id == M_OK || id == M_Cancel)
//	CloseDialog();
  }
// --------------------------------------------------------------------- 
