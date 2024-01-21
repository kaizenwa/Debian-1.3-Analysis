//===============================================================
// vlabelc.cxx	- label Cmd - Windows
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vwin32.h>		// for Win 32 stuff
#include <v/vlabelc.h>	// our definitions
#include <v/vcmdprnt.h>	// a command parent
#include <v/vutil.h>	// utility
#include <v/vicon.h>	// icons

//=====================>>> vLabelCmd::vLabelCmd <<<=======================
  vLabelCmd::vLabelCmd(vCmdParent* dp, CommandObject* dc):
	    vCmd(dp, dc)
  {
    initialize();
  }

//=====================>>> vLabelCmd::~vLabelCmd <<<=======================
  vLabelCmd::~vLabelCmd()
  {

    SysDebug(Destructor,"vLabelCmd::~vLabelCmd() destructor\n")

  }

//=====================>>> vLabelCmd::initialize <<<=======================
  void vLabelCmd::initialize(void)
  {
    // build a button command for use in a parent window

    CopyToLocal();			// Make local copies of CmdObject

    SysDebug(Constructor,"vLabelCmd::vLabelCmd() constructor\n")

    if (dlgCmd->cmdType == C_Icon && _itemList != 0)	// icon
      {
	initIcon();
	return;
     }

    long style = SS_LEFTNOWORDWRAP;	// default for a label


    if (!(dlgCmd->attrs & CA_Hidden))	// Check for Hidden
	style |= WS_VISIBLE;

    if (dlgCmd->size > 0)		// may reset title!
      {
	_w = dlgCmd->size;
      }
    else if (dlgCmd->attrs & CA_MainMsg && vLblLen(_title) < 20)
      {
	 _w = 88;
      }
    else
	_w = LabelWidth(_title) + 8;		// set my width

    if (_parentWin->paneType() == P_Status)
      {
	_h = 9;
	_parentWin->SetPosition(_x, _y, _w, _h + 2, dlgCmd->cFrame,
	     dlgCmd->cRightOf, dlgCmd->cBelow);
	_y += 2;	// make look more centered
      }
    else
      {
        _h = 11;				// default height
	_parentWin->SetPosition(_x, _y, _w, _h + 3, dlgCmd->cFrame,
	     dlgCmd->cRightOf, dlgCmd->cBelow);
	_y += 3;	// make look more centered

      }

    _CtrlOffset = _parentWin->AddDlgControl(_x, _y , _w, _h, _cmdId,
	   style, "STATIC", _title, sizeof(vLabelCmd*), (LPBYTE)this);
  }


//=====================>>> vLabelCmd::initIcon <<<=======================
  void vLabelCmd::initIcon(void)
  {
    _ip = (vIcon *) _itemList;	// to access bitmap
    long style = BS_OWNERDRAW;

     if (!(dlgCmd->attrs & CA_Hidden))	// Check for Hidden
	style |= WS_VISIBLE;

    _h = _ip->height / 2 + 6;
#ifdef _WIN32
    _w = _ip->width / 2 + 14;
#else
    _w = _ip->width / 2 + 6;
#endif
   
    _parentWin->SetPosition(_x, _y, _w, _h, dlgCmd->cFrame,
	     dlgCmd->cRightOf, dlgCmd->cBelow);

    _CtrlOffset = _parentWin->AddDlgControl(_x, _y , _w, _h, _cmdId,
	   style, "BUTTON", "", sizeof(vLabelCmd*), (LPBYTE)this);

  }

//================>>> vLabelCmd::SetCmdVal <<<============================
  void vLabelCmd::SetCmdVal(ItemVal val, ItemSetType st)
  {
    SysDebug1(Misc,"vLabelCmd::SetCmdVal(val:%d)\n",val)

    HWND myHwnd = GetMyHwnd(_cmdId);
    if (st == Sensitive)		// used so icons won't get
      {                                 // focus -- icons use ownerdraw,
	_Sensitive = val;	// which behave like buttons, not
	::EnableWindow(myHwnd, val);    // static labels -- this fixes that
      }
    else if (st == Hidden)		// hide or unhide
      {
	if (val)
	  {
	    ::ShowWindow(myHwnd,SW_HIDE);
	  }
	else
	  {
	    ::ShowWindow(myHwnd,SW_SHOW);
	  }
      }

  }

//================>>> vLabelCmd::SetCmdStr <<<============================
  void vLabelCmd::SetCmdStr(char* str)
  {

    SysDebug1(Misc,"vLabelCmd::SetCmdStr(str:%s)\n",str)

    ::SetDlgItemText(_parentWin->getParent(),_cmdId, str);
    _title = str;
  }

//================>>> vLabelCmd::MEASUREITEM <<<============================
  int vLabelCmd::MEASUREITEM(int id, MEASUREITEMSTRUCT* mis)
  {
    mis->CtlType = ODT_BUTTON;
    mis->CtlID = id;
    mis->itemWidth = _w;
    mis->itemHeight = _h;
    
    return 1;
  }

//================>>> vLabelCmd::DRAWITEM <<<============================
  int vLabelCmd::DRAWITEM(int id, DRAWITEMSTRUCT* dis)
  {
    switch (dis->itemAction)
      {
	case ODA_DRAWENTIRE:	// redraw entire control
	  {
            POINT ptSize, ptOrg;

	    // This gets called after the CTLCOLOR message has been
	    // processed in the dialog/commandpane procs.
	    // We are using LTGRAY_BRUSH for dialogs and command bars.
	    // We need to draw the icon properly on the background.
	    // We will use a null pen, and the same background. We have
	    // to use left-1, top-1, right+1, and bottom+1 to have the
	    // interior of the rectangle in the icon's area. We end up
	    // with a white line otherwise. First, fill gray background

	    HPEN oldpen = (HPEN) ::SelectObject(dis->hDC, ::GetStockObject(NULL_PEN));
	    HBRUSH oldbrush = (HBRUSH) ::SelectObject(dis->hDC, theApp->_BarBrush);
	    (void)::Rectangle(dis->hDC,dis->rcItem.left-1, dis->rcItem.top-1,
		dis->rcItem.right+1, dis->rcItem.bottom+1);

	    ::SelectObject(dis->hDC, ::GetStockObject(BLACK_BRUSH)); // draw icon black

	    HDC hdcMem = ::CreateCompatibleDC(dis->hDC);
	    HBITMAP hbm = _ip->GetIconHBM(hdcMem);

	    ::SelectObject(hdcMem, hbm);
	    ::SetMapMode(hdcMem, ::GetMapMode(dis->hDC));

	    ptSize.x = _ip->width;
	    ptSize.y = _ip->height;

	    ptOrg.x = 0;
	    ptOrg.y = 0;

	    ::SetBkColor(dis->hDC, RGB(255,255,255));

	    // Now BitBlt the icon BMP into the drawing canvas, leaving
	    // background untouched, and the brush color to draw 1 bits

	    ::BitBlt(dis->hDC, dis->rcItem.left, dis->rcItem.top, ptSize.x,
	       ptSize.y, hdcMem, ptOrg.x, ptOrg.y, 0xE20746L);
	    ::DeleteDC(hdcMem);

            ::SelectObject(dis->hDC, oldpen);	// restore hdc
	    ::SelectObject(dis->hDC, oldbrush);

            return 1;
	  }

	case ODA_SELECT:	// indicated selected
	  {
	    return 1;		// no op for icon label
	  }

	case ODA_FOCUS:		// gets focus
          {
	    return 1;		// no op for icon label
	  }
      }
    return 0;
  }

//===================>>> vLabelCmd::vCmdCallback <<<=======================
  void vLabelCmd::CmdCallback(int id, WORD codeNotify)
  {
   // No Op for a label - the icon button may generate one of these!
  }
