//===============================================================
// vdialog.cxx - vDialog class - Windows
//                                                                        
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vwin32.h>		// for Win 32 stuff
#include <v/vdialog.h>		// our header
#include <v/vapp.h>
#include <v/vthislst.h>		// For this list
#include <v/vclabelc.h>

#define SCROLL_CRACKER

// Define static data of the class
  static BOOL CALLBACK DlgProcCB(HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam);

  static vThisList _thisList;	// hide this in this file

  vDialog* vDialog::_curModal = 0;	// V:1.13


#ifndef _WIN32
typedef void DLGTEMPLATE;
#endif
//===================>>> vChkWinDlgMsgs <<<=======================
  int vChkWinDlgMsgs(MSG *msg)
  {
    // Need this to check if a message should be processed
    // by the dialog message processor instead of the general loop.

    for (vDialog* w = (vDialog*) _thisList.GetFirstThis() ; w != 0 ;
		  w = (vDialog*) _thisList.GetNextThis())
      {
	if (::IsDialogMessage(w->HwndDialog(),msg))
	    return 1;
      }

    return 0;
  }

//===================>>> vDialog::vDialog <<<=======================
  vDialog::vDialog(const vDialog& d) : vBaseWindow(d),
    vCmdParent(P_Dialog)
  {
    vSysError("vDialog - V semantics do not support copy constructors!");
    _oldModal = 0;		// V:1.13
  }

//=================>>> vDialog::~vDialog <<<=======================
  vDialog::~vDialog()
  {

    SysDebug(Destructor,"vDialog::~vDialog() destructor\n")

    _IsDisplayed = 0;		// The dialog is not up

    if (_hTemplate != 0)
      {
	::GlobalFree(_hTemplate);
	_hTemplate = 0;
      }

    closeBaseWindow();		// close this window
    _thisList.Delete((ThisId)_wDialog);	// free the this ptr
  }

//===================>>> vDialog::vDialog <<<=======================
  vDialog::vDialog(vBaseWindow* creator, int modal, char* title) :
    vBaseWindow(title), vCmdParent(P_Dialog)		// constructor
  {
     _parentHWND = creator->winHwnd();	// track parent's HWND
     init(modal,title);			// finish construction
  }

//===================>>> vDialog::vDialog <<<=======================
  vDialog::vDialog(vApp* creator, int modal, char* title) :
    vBaseWindow(title), vCmdParent(P_Dialog)		// constructor
  {
     _parentHWND = creator->winHwnd();	// track parent's HWND
     init(modal,title);			// finish construction
  }

//===================>>> vDialog::vDialog <<<=======================
  void vDialog::init(int modal, char* title)
  {
    // Initialize dialog box

    _modal = modal;		// settable modal

    SysDebug(Constructor,"vDialog::vDialog() constructor\n")

    _wType = DIALOG;		// a dialog

    _dialogType = aDialog;	// This is a dialog

    _IsDisplayed = 0;		// The dialog is not up

    _wDialog = 0;		// haven't created the dialog yet...

    
    // Now, build the initial in-memory dialog template using
    // Windows HGLOBAL handle _hTemplate.

    int buildOK;
    // Using 8,helv makes it ok to use 8 in calculating control sizes!
    if (_modal)
	buildOK = CreateDlgTemplate(
	   DS_MODALFRAME | WS_CAPTION | WS_VISIBLE | WS_POPUP  | DS_SETFONT,
	   10, 25, 1, 1, "", "", _name, 8, "Helv" );
    else
	buildOK = CreateDlgTemplate(
	    WS_CAPTION | WS_VISIBLE | WS_POPUP  | DS_SETFONT,
	    10, 25, 1, 1, "", "", _name,  8, "Helv" );
    if (!buildOK)
	vSysError("vDialog - Unable to build dialog template");

    _DefaultButton = 0;	// No default button described
    _FirstTextIn = 0;	// No text in field
  }

//===================>>> vDialog::AddDialogCmds <<<========================
  void vDialog::AddDialogCmds(CommandObject* cList)	// add commands
  {
    // This is called to add commands from the supplied
    // CommandObject list.

    vCmd* defButton = 0;
    DlgCmdList* curCmd;

    // scan the entire list

    for (int ix = 0 ; cList && (cList[ix].cmdType != C_EndOfList) ; ++ix)
      {
	curCmd = new DlgCmdList;		// get a new cell

	curCmd->nextDCL = _cmdList;		// add in at front
	_cmdList = curCmd;
	curCmd->cmdP = 0;			// not added yet

	curCmd->cmdP = AddCmd(&cList[ix]);

	// Track default button

	if (cList[ix].attrs & CA_DefaultButton)
	  {
	     defButton = curCmd->cmdP;
	  }

	if (cList[ix].cmdType == C_TextIn)
	  {
 	    if (_FirstTextIn == 0)
		_FirstTextIn = curCmd->cmdP;
	  }
      }

    if (defButton)			// we have a default
      {
	_DefaultButton = defButton;	// Track the default button

      }

    DoneAddingControls();		// all finished with controls
  }

//===================>>> vDialog::AddDialogCmdObj <<<========================
  void vDialog::AddDialogCmdObj(CommandObject* Cmd, vCmd* CmdInstance)
  {
    // This is called to add an already instantiated command object to the
    // CommandObject list.

    DlgCmdList* curCmd = new DlgCmdList;		// get a new cell

    curCmd->nextDCL = _cmdList;		// add in at front of list

    _cmdList = curCmd;	

    curCmd->cmdP = CmdInstance;		// point to the object

    // Track default button

    if (Cmd->attrs & CA_DefaultButton)
      {
	_DefaultButton = CmdInstance;	// This is the default button
	// This lets us use Enter to activate the default button.
      }
  }

//====================>>> vDialog::CancelDialog <<<=======================
  void vDialog::CancelDialog(void)
  {
    // Cancel selected - reset all values to original values


    SysDebug(CmdEvents,"vDialog::CancelDialog()\n")

    for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)
      {
	(cc->cmdP)->ResetItemValue();
      }

    // And close the dialog

    CloseDialog();
  }

//====================>>> vDialog::CloseDialog <<<=======================
  void vDialog::CloseDialog(void)
  {

    SysDebug(Build,"vDialog::CloseDialog()\n")

    _IsDisplayed = 0;		// The dialog is not up

    if (!_modal)		// destroy if not modal
	::DestroyWindow(_wDialog);
    _thisList.Delete((ThisId)_wDialog);	// free the this ptr
  }

//====================>>> vDialog::DialogCommand <<<=======================
  void vDialog::DialogCommand(ItemVal id, ItemVal retval, CmdType ctype)
  {
    // After the user has selected a command from the dialog,
    // this routine is called with the value


    SysDebug2(CmdEvents,"vDialog::DialogCommand(id:%d, val:%d)\n",id, retval)

    switch (id)			// We will do some things depending on value
      {
	case M_Cancel:
	    CancelDialog();
	    break;

	case M_Done:		// default, close dialog
	case M_OK:
	  {
	    CloseDialog();
	    break;
	  }
      }
  }

//====================>>> vDialog::DialogDisplayed <<<=======================
  void vDialog::DialogDisplayed()
  {
  }

//==================>>> vDialog::ProcessCmd <<<=======================
  void vDialog::ProcessCmd(ItemVal id, ItemVal rv, CmdType ct)
  {
    // simple interface between command objects and the
    // inherited vCmdParent ProcessCmd to the CommandObject which does the work

    DialogCommand(id, rv, ct);
  }

//====================>>> vDialog::SetDialogTitle <<<=======================
  void vDialog::SetDialogTitle(char* title)
  {
    ::SetWindowText(_wDialog, title);
  }

//================>>> vDialog::GetDialogPosition <<<========================
  void vDialog::GetDialogPosition(int& left, int& top, int& width, int& height)
  {
    RECT rct;

    ::GetWindowRect(_wDialog, &rct);

    left = rct.left;
    top = rct.top;
    width = rct.right - rct.left;
    height = rct.bottom - rct.top;

  }

//====================>>> vDialog::SetDialogPosition <<<=======================
  void vDialog::SetDialogPosition(int left, int top)
  {
    RECT ar;

    ::GetWindowRect(theApp->winClientHwnd(), &ar);

    (void)::SetWindowPos(_wDialog,HWND_TOP,left+ar.left,top+ar.top,
			 0,0,SWP_NOSIZE);
  }

//====================>>> vDialog::ShowDialog <<<=======================
  void vDialog::ShowDialog(char* msg)
  {
    //	Show the dialog with the default message

    //  We will control the position of the dialog box based on
    //	the parent's location. The user can move the dialog later.

    SysDebug1(Build,"vDialog::ShowDialog(%s)\n",msg)

    // First, get the values from our parent

    if (_IsDisplayed)
	return;				// make this a no-op

    // set the message field -- have to move to the dlgproc init case

    if (msg && *msg)
      {
	for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)
	  {
	    if (((cc->cmdP)->dlgCmd)->attrs & CA_MainMsg)
	      {
		 ((cc->cmdP)->dlgCmd)->title = msg;	// repoint to the message
	      }
	  }
      }

    // Since the dialog isn't necessarily re-created each time it is
    // popped up, we need to loop through now to set the original values

    for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)
      {
	(cc->cmdP)->SaveItemValue();
      }

    // Now ready to popup the dialog
    _IsDisplayed = 1;			// The dialog is up
    const DLGTEMPLATE * ptr = (const DLGTEMPLATE *)GlobalLock(_hTemplate);
    if (_modal)
      {
	(void) ::DialogBoxIndirectParam((HINSTANCE)theApp->vHandle(), 
#ifdef _WIN32
					ptr,
#else
					_hTemplate,
#endif
					_parentHWND, (DLGPROC) DlgProcCB, LPARAM(this));
	_IsDisplayed = 0;
      }
    else
      {
	_wDialog = 0;
	(void) ::CreateDialogIndirectParam((HINSTANCE)theApp->vHandle(),
					   ptr,
					   _parentHWND, 
					   (DLGPROC) DlgProcCB,
					   LPARAM(this)
					   );
      }
    ::GlobalUnlock(_hTemplate); // Unlock the template here
  }

// ---------------------------------------------------------------------
//====================>>> DlgProc <<<=======================
static BOOL CALLBACK DlgProcCB(HWND hDlg, UINT uMsg,
			       WPARAM wParam, LPARAM lParam)
{
  vDialog* useThis;

  if (uMsg == WM_INITDIALOG) // first time!
    {
      useThis = (vDialog*)lParam;
      _thisList.Add((ThisId)hDlg, (void*)useThis);
    }
  else
    useThis = (vDialog*)_thisList.GetThis((ThisId)hDlg);
  if (!useThis)
    return 0;


  return useThis->DynamicDlgProc(hDlg, uMsg, wParam, lParam);
}

//====================>>> vDialog::DynamicDlgProc <<<=======================
  int vDialog::DynamicDlgProc(HWND hDlg, UINT uMsg,
			    WPARAM wParam, LPARAM lParam)
  {
    int fProcessed = 1;

    // This code handles nested modals, a no no....   (V:1.13)

   if (_curModal != 0 && _curModal != this)		// V:1.13
     {
	//::BringWindowToTop(_curModal->myHwnd());
	return 0;
     }
  
  switch (uMsg)
    {
    case WM_INITDIALOG:
      {
	// Changes here must have equivlalents in the vcmdpane.cpp code
	_wDialog = hDlg;
        _myHWND = hDlg;		// V:1.13
	
	for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)
	  {
	    vCmd* cp = cc->cmdP;
	    if ((cp->dlgCmd)->cmdType == C_CheckBox && (cp->dlgCmd)->retVal)
	      cp->SetCmdVal(1,Checked);

	    if ((cp->dlgCmd)->cmdType == C_RadioButton && (cp->dlgCmd)->retVal)
	      cp->SetCmdVal(1,Checked);
	    
	    if ((cp->dlgCmd)->cmdType == C_Slider || (cp->dlgCmd)->cmdType == C_Spinner)
	      cp->SetCmdVal(cp->GetCmdValue((cp->dlgCmd)->cmdId),Value);
	    
	    if ((cp->dlgCmd)->attrs & CA_MainMsg)
	      {
		cp->SetCmdStr((cp->dlgCmd)->title);
	      }
	    if ((cp->dlgCmd)->attrs & CA_DefaultButton)
	      {
		::SetFocus(GetDlgItem(_wDialog, (cp->dlgCmd)->cmdId)); // Set to default
		fProcessed = 0;						       // return 0 if we call SetFocus
	      }
	    if ((cp->dlgCmd)->cmdType == C_TextIn)
	      {
		::SetFocus(GetDlgItem(_wDialog, (cp->dlgCmd)->cmdId)); // Set to default
		fProcessed = 0;						       // return 0 if we call SetFocus
	      }
	    
	    if (!(cp->dlgCmd)->Sensitive) // Make insensitive if it was
	      {
		cp->SetCmdVal(0,Sensitive);
	      }
	    if ((cp->dlgCmd)->attrs & CA_Hidden) // Hide it!
	      {
		cp->SetCmdVal(1,Hidden);
	      }
	    if ((cp->dlgCmd)->cmdType == C_List || (cp->dlgCmd)->cmdType == C_ComboBox
		 || (cp->dlgCmd)->cmdType == C_Spinner)
	      {
		int curval = cp->GetCmdValue((cp->dlgCmd)->cmdId);
		cp->SetCmdVal(curval,ChangeList);
	      }
	    if ((cp->dlgCmd)->cmdType == C_Icon || (cp->dlgCmd)->cmdType == C_Frame
	       || (cp->dlgCmd)->cmdType == C_ToggleFrame)
	      {
		cp->SetCmdVal(0,Sensitive); // Trick to avoid getting focus
	      }
	  }

        DialogDisplayed();
	
	break;
      }
      
    case WM_COMMAND:
      {
	switch (GET_WM_COMMAND_CMD(wParam, lParam)) // Have to ignore EN messages
	  {
	  case EN_ERRSPACE:
	  case EN_HSCROLL:
	  case EN_KILLFOCUS:
	  case EN_MAXTEXT:
	  case EN_SETFOCUS:
	  case EN_UPDATE:
	  case EN_VSCROLL:
	    return 0;
	    
	  }
	vCmd* UseThis = (vCmd *) getThisFromId((ItemVal)GET_WM_COMMAND_ID(wParam, lParam));

	if (!UseThis)
	  {
	    fProcessed = 0; break;
	  }

	UseThis->CmdCallback(GET_WM_COMMAND_ID(wParam, lParam),
			     (WORD)GET_WM_COMMAND_CMD(wParam, lParam));
      }
      break;
      
      
    case WM_HSCROLL:
    case WM_VSCROLL:
      {
	int id = ::GetWindowID(GET_WM_HSCROLL_HWND(wParam, lParam));

	vCmd* UseThis = (vCmd *) getThisFromId(id);
	int code = GET_WM_HSCROLL_CODE(wParam,lParam);

	if (!UseThis || code == SB_ENDSCROLL)
	  {
	    fProcessed = 0; break;
	  }

	UseThis->CmdCallback(code,
			     (WORD)GET_WM_HSCROLL_POS(wParam, lParam));
	break;
      }
#ifdef _WIN32      
    case WM_CTLCOLORSTATIC:
	  {
	    vCmd* UseThis = (vCmd *)
	      getThisFromId(::GetDlgCtrlID(GET_WM_CTLCOLOR_HWND(wParam, lParam, uMsg)));
	    if (UseThis && (UseThis->dlgCmd)->cmdType == C_ColorLabel)
	      {
		::SetTextColor(GET_WM_CTLCOLOR_HDC(wParam, lParam, uMsg),
			       ((vColorLabelCmd*)UseThis)->_color.pixel());
	      }
	  }
	  // flow into next case!	  
    case WM_CTLCOLORBTN:
	  ::SetBkColor(GET_WM_CTLCOLOR_HDC(wParam, lParam, uMsg),
		       ::GetSysColor(COLOR_BTNFACE));
	  return (int)(theApp->_BarBrush);
    case WM_CTLCOLORDLG:	  
	  return (int )(theApp->_BarBrush);
#else
    case WM_CTLCOLOR:
      switch (HIWORD(lParam))
	{
	case CTLCOLOR_STATIC:	// Special check for color lbl
	  {
	    vCmd* UseThis = (vCmd *)
	      getThisFromId(::GetDlgCtrlID((HWND)LOWORD(lParam)));
	    if (UseThis && (UseThis->dlgCmd)->cmdType == C_ColorLabel)
	      {
		::SetTextColor((HDC)wParam,
			       ((vColorLabelCmd*)UseThis)->_color.pixel());
	      }
	  }
	  
	  // flow into next case!
	  
	case CTLCOLOR_BTN:
	  ::SetBkColor((HDC)wParam, ::GetSysColor(COLOR_BTNFACE));
	  return (int)(theApp->_BarBrush);
	  
	case CTLCOLOR_DLG:
	  return (int )(theApp->_BarBrush);
	}
      return 0;
#endif
      
      // Following two are for BS_OWNERDRAW buttons, which include
      // icons, iconbuttons, and maybe others
      
    case WM_MEASUREITEM:
      {
	if (!_IsDisplayed)
	  return 0;
	ItemVal mid = (ItemVal)GET_WM_COMMAND_ID(wParam, lParam);
	vCmd* UseThis = (vCmd *) getThisFromId(mid);
	if (!UseThis)
	  return 0;
	return UseThis->MEASUREITEM((int)mid, (MEASUREITEMSTRUCT*)lParam);
      }
      
    case WM_DRAWITEM:
      {
	if (!_IsDisplayed)
	  return 0;
	ItemVal did = (ItemVal)GET_WM_COMMAND_ID(wParam, lParam);
	vCmd* UseThis = (vCmd *) getThisFromId(did);
	if (!UseThis)
	  return 0;
	return UseThis->DRAWITEM((int)did, (DRAWITEMSTRUCT*)lParam);
      }

    default:
      fProcessed = FALSE; break;
    }
  return fProcessed;
}
