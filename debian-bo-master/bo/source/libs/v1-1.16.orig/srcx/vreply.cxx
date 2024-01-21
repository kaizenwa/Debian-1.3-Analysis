//===============================================================
// vreply.cxx - vReplyDialog class functions - X11R5
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vreply.h>           // our header
#include <v/vicon.h>           // for icon

// Define static data of the class

#define prompt_width 32
#define prompt_height 32
static unsigned char prompt_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0xf8, 0xff, 0xff, 0x1f, 0x04, 0x00, 0x00, 0x20,
   0xe4, 0xff, 0xff, 0x27, 0x14, 0x00, 0x00, 0x28, 0x14, 0xc0, 0x01, 0x28,
   0x14, 0xe0, 0x03, 0x28, 0x14, 0x30, 0x06, 0x28, 0x14, 0x30, 0x06, 0x28,
   0x14, 0x00, 0x03, 0x28, 0x14, 0x80, 0x01, 0x28, 0x14, 0xc0, 0x00, 0x28,
   0x14, 0xc0, 0x00, 0x28, 0x14, 0xc0, 0x00, 0x28, 0x14, 0xc0, 0x00, 0x28,
   0x14, 0x00, 0x00, 0x28, 0x14, 0xc0, 0x00, 0x28, 0x14, 0x00, 0x00, 0x28,
   0xe4, 0xff, 0xff, 0x27, 0x04, 0x00, 0x00, 0x20, 0xf8, 0xff, 0xff, 0x1f,
   0xfe, 0xff, 0xff, 0x7f, 0x02, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x40,
   0x02, 0x00, 0xe0, 0x47, 0x02, 0x00, 0x00, 0x40, 0xfa, 0xff, 0xff, 0x5f,
   0xae, 0xaa, 0xaa, 0x6a, 0x56, 0x55, 0x55, 0x55, 0xaa, 0xaa, 0xaa, 0x6a,
   0xfe, 0xff, 0xff, 0x7f, 0x00, 0x00, 0x00, 0x00};

    static vIcon prompt(&prompt_bits[0], prompt_height, prompt_width);

    static CommandObject ReplyDialog[] =
      {
	{C_Icon,  10, 0, "Reply?", (void*)&prompt, CA_None, isSens,NoFrame,0,0},
        {C_Label, 1, 1, "Reply?",NoList,
                CA_MainMsg,isSens,NoFrame, 10,0},
        {C_TextIn, 2, 2, "", NoList,
                CA_Large,isSens,NoFrame,0,10},
        {C_Button, M_Cancel, M_Cancel, " Cancel ", NoList,CA_None,
                isSens,NoFrame,0,2},
        {C_Button, M_OK, M_OK, " OK ", NoList,CA_DefaultButton,
                isSens,NoFrame,M_Cancel,2},
        {C_EndOfList,0,0,0,0,CA_None,0,0,0}
      };

//======================>>> vReplyDialog::Reply <<<=======================
  int vReplyDialog::Reply(const char* msg, char* reply, const int maxlen)
  {
    //	Show a message, wait for a reply
    //	no important return

    int ans;

    if (!added)
      {
	AddDialogCmds(ReplyDialog);		// Set up standard dialog
	added = 1;
      }

    (void) ShowModalDialog(msg, ans);	// show and wait

    reply[0] = 0;

    if (ans != M_Cancel)
	(void) GetTextIn(2, reply, maxlen);

    return ans;
  }

//====================>>> vReplyDialog::DialogCommand <<<=======================
  void vReplyDialog::DialogCommand(ItemVal id, ItemVal val, CmdType ctype)
  {
    vModalDialog::DialogCommand(id,val,ctype);
//    if (id == M_OK || id == M_Cancel)
//	CloseDialog();
  }
// --------------------------------------------------------------------- 
