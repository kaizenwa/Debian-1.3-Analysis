//	vedcmdw.h:	Header for vedcmdw class
//=======================================================================

#ifndef vedCMDW_H
#define vedCMDW_H

#include <v/vcmdwin.h>	// So we can use vCmdWindow
#include <v/vmenu.h>	// For the menu pane
#include <v/vutil.h>	// For V Utilities
#include <v/vcmdpane.h> // command pane
#include <v/vstatusp.h>	// For the status pane
#include <v/vfont.h>	// for fonts

#ifdef vDEBUG
#include <v/vdebug.h>
#endif

#include "vedcnv.h"	// vedCanvasPane

    class vedCmdWindow : public vCmdWindow
      {
	friend int AppMain(int, char**);	// allow AppMain access

      public:		//---------------------------------------- public
	vedCmdWindow(char*, int, int);
	virtual ~vedCmdWindow();
	virtual void WindowCommand(ItemVal id, ItemVal val, CmdType cType);
	virtual void KeyIn(vKey keysym, unsigned int shift);

	void ChangeLoc(long line, int col);
	void ChangeInsMode(int IsInsMode);
	void StatusMessage(char *msg);
	void ErrorMsg(char *str);
	int OpenFile(char* name);
	int CheckClose(int ask = 1);

      protected:	//--------------------------------------- protected

      private:		//--------------------------------------- private

	// Standard elements
	vMenuPane* vedMenu;		// For the menu bar
	vedTextEditor* vedCanvas;	// For the canvas
	vCommandPane* vedCmdPane;	// for the command pane
	vStatusPane* vedStatus;		// For the status bar
        vFont vedFont;			// for the font

	// Dialogs associated with CmdWindow

	char fileName[200];


      };
#endif
