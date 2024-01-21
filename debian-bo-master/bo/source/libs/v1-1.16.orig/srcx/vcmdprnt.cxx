//===============================================================
// vcmdprnt.cxx - vCmdParent Class
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vcmdprnt.h>		// our include

#include <v/vbtncmd.h>		// ButtonCmd
#include <v/vcbtncmd.h>		// ColorButtonCmd
#include <v/vclabelc.h>		// Color label
#include <v/vlabelc.h>		// LabelCmd
#include <v/vframec.h>		// FrameCmd
#include <v/vchkboxc.h>		// CheckBoxCmd
#include <v/vradioc.h>		// RadioButtonCmd
#include <v/vtextinc.h>		// TextInCmd
// V:1.14 #include <v/vtextedc.h>		// TextEdCmd
#include <v/vtextc.h>		// TextCmd
#include <v/vlistc.h>		// ListCmd
#include <v/vcomboc.h>		// ComboBoxCmd
#include <v/vspinc.h>		// SpinnerCmd
#include <v/vsliderc.h>		// SliderCmd
#include <v/vprogrsc.h>		// ProgressCmd

//======================>>> vCmdParent::vCmdParent <<<============================
  vCmdParent::vCmdParent()
  {
    _cmdList = 0;
  }

//======================>>> vCmdParent::~vCmdParent <<<============================
  vCmdParent::~vCmdParent()
  {
    SysDebug(Destructor,"vCmdParent::~vCmdParent() destructor\n")

    // need to delete everything we added

    DlgCmdList* tmp;

    for (DlgCmdList* cc = _cmdList ; cc != 0  ; )
      {
	delete cc->cmdP;	// delete the cmd object
	tmp = cc;		// don't forget where we were
	cc = cc->nextDCL;	// for next time
	delete tmp;		// delete the list cell
      }	
  }

//======================>>> vCmdParent::AddCmd <<<============================
  vCmd* vCmdParent::AddCmd(CommandObject* cmd)
  {
    // Private method to add a single command to a dialog object.

    SysDebug2(Build,"vCmdParent::AddCmd() - %s(id:%d)\n",cmd->title,cmd->cmdId)

    vCmd* cmdPtr;

    switch (cmd->cmdType)		// depends on type
      {
	case C_IconButton:		// a command button Icon
	case C_ToggleIconButton:	// a toggle command button Icon
	case C_ToggleButton:		// a toggle command button
	case C_Button:			// Button
	  {
	    cmdPtr = new vButtonCmd(this, cmd);
	    break;
	  }

	case C_ColorButton:		// ColorButton
	  {
	    cmdPtr = new vColorButtonCmd(this, cmd);
	    break;
	  }

	case C_ColorLabel:		// ColorButton
	  {
	    cmdPtr = new vColorLabelCmd(this, cmd);
	    break;
	  }

	case C_CheckBox:		// Checked Item
	  {
	    cmdPtr = new vCheckBoxCmd(this, cmd);
	    break;
	  }

	case C_ComboBox:		// Popup combo list
	  {
	    cmdPtr = new vComboBoxCmd(this, cmd);
	    break;
	  }

	case C_Frame:			// General purpose frame
	case C_ToggleFrame:		// Toggle frame
	  {
	    cmdPtr = new vFrameCmd(this, cmd);
	    break;
	  }

	case C_Icon:			// a display only Icon
	case C_Blank:			// to help RightOfs, Belows work
	case C_BoxedLabel:		// Boxed text label
	case C_Label:			// Regular text label
	  {
	    cmdPtr = new vLabelCmd(this, cmd);
	    break;
	  }

	case C_List:			// List of items (scrollable)
	  {
	    cmdPtr = new vListCmd(this, cmd);
	    break;
	  }

	case C_ProgressBar:		// Progress bar
	  {
	    cmdPtr = new vProgressCmd(this, cmd);
	    break;
	  }

	case C_RadioButton:		// radio button
	  {
	    cmdPtr = new vRadioButtonCmd(this, cmd);
	    break;
	  }

	case C_Slider:			// slider
	  {
	    cmdPtr = new vSliderCmd(this, cmd);
	    break;
	  }

	case C_Text:			// Text output field
	  {
	    cmdPtr = new vTextCmd(this, cmd);
	    break;
	  }

//V:1.14	case C_TextEdit:		// Text edit field
//V:1.14	  {
//V:1.14	    cmdPtr = new vTextEdCmd(this, cmd);
//V:1.14	    break;
//V:1.14	  }

	case C_TextIn:			// Text input field
	  {
	    cmdPtr = new vTextInCmd(this, cmd);
	    break;
	  }

	case C_Spinner:		// Value Box list
	  {
	    cmdPtr = new vSpinnerCmd(this, cmd);
	    break;
	  }

	default:			// unknown!
	  {
 SysDebug2(BadVals,"vCmdParent::AddCmd(id:%d, title:%s) ** Unknown DialogCommand\n",cmd->cmdId,cmd->title);
	    cmd->title = "?? Bad CommandObject ??";
	    cmdPtr = new vLabelCmd(this, cmd);
	    break;
	  }
      }

    return cmdPtr;
  }

//====================>>> vCmdParent::HasId <<<=========================
  int vCmdParent::HasId(ItemVal id)
  {
    // We need this one for panes to work correctly

    for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)
      {
	if (((cc->cmdP)->dlgCmd)->cmdId == id)
	  {
	    return 1;
	  }
      }
    return 0;
  }

//====================>>> vCmdParent::GetValue <<<=========================
  int vCmdParent::GetValue(ItemVal id)
  {
    for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)
      {
	if (((cc->cmdP)->dlgCmd)->cmdId == id)
	  {
	    return (cc->cmdP)->GetCmdValue(id);  // return the value
	  }
      }
    return 0;
  }

//====================>>> vCmdParent::GetTextIn <<<=========================
  int vCmdParent::GetTextIn(ItemVal id, char* strout, int maxlen)
  {
    // recover the string from TextIn id, up to maxlen
    for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)
      {
	if (((cc->cmdP)->dlgCmd)->cmdId == id)
	  {
	    // need to cast to a vTextInCmd so we can use getstr
	    vTextInCmd* tip = (vTextInCmd*) cc->cmdP;

	    return tip->GetTextIn(id, strout, maxlen);  // return the value
	  }
      }

    *strout = 0;
    return 0;
  }

//====================>>> vCmdParent::SetFrameChildren <<<======================
  void vCmdParent::SetFrameChildren(ItemVal frameId, int frameVal)
  {
    // Set all children of given frame to hidden or not hidden
    // Do this recursively to be sure to get them all.

    // Scan list, setting all children that use this frame

    for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)  // Find the frame
      {
	CommandObject* cop = (cc->cmdP)->dlgCmd;
	if (cop->cFrame == frameId)			// A child of ours
	  {
            // Hide control if not turning on (frameVal is opposite of Hidden)
	    SetValue(cop->cmdId, !frameVal, Hidden);
	    if (cop->cmdType == C_Frame || cop->cmdType == C_ToggleFrame)
		SetFrameChildren(cop->cmdId, frameVal); // Get other chilren
	  }
      }
  }

//====================>>> vCmdParent::getParent <<<=======================
  Widget vCmdParent::getParent(ItemVal id)
  {
    //	If the object id has a frame specified, find it and return
    //	its widget.  Otherwist, the parent is the dialog.

    Widget tmp;

    if (id == NoFrame)
      {
	return _wDialog;
      }
    else
      {
	tmp = getWidgetFromId(id);
	if (tmp == 0)
	  {
	    vSysWarning("Cannot find Frame parent.");
	    return _wDialog;
	  }
	else
	    return tmp;
      }
  }

//==================>>> vCmdParent::getWidgetFromId <<<=======================
  Widget vCmdParent::getWidgetFromId(ItemVal find_id)
  {
    // search the list of cmds for id, return its object

    DlgCmdList* cc;

    if (find_id == 0)			// no op
	return 0;

    // search the list
    for (cc = _cmdList->nextDCL ; cc != 0  ; cc = cc->nextDCL)
      {
	if (((cc->cmdP)->dlgCmd)->cmdId == find_id)
	  {
	    if ( (cc->cmdP)->wBox == 0)  // use the surround box if there
		return (cc->cmdP)->wCmd;
	    else
		return (cc->cmdP)->wBox;
	  }
      }
    
    return 0;				// failed to find it
  }

//====================>>> vCmdParent::SetValue <<<=======================
  void vCmdParent::SetValue(ItemVal id, ItemVal val, ItemSetType setType)
  {

    SysDebug2(Misc,"vCmdParent::SetValue(id:%d, val:%d)\n",id,val)

    for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)
      {
	if (((cc->cmdP)->dlgCmd)->cmdId == id)
	  {
	    (cc->cmdP)->SetCmdVal(val,setType);  // return the value
	    return;
	  }
      }
  }

//====================>>> vCmdParent::SetString <<<=======================
  void vCmdParent::SetString(ItemVal id, char* str)
  {

    SysDebug2(Misc,"vCmdParent::SetString(id:%d, str:%s)\n",id,str)

    for (DlgCmdList* cc = _cmdList ; cc != 0  ; cc = cc->nextDCL)
      {
	if (((cc->cmdP)->dlgCmd)->cmdId == id)
	  {
	    (cc->cmdP)->SetCmdStr(str);  // return the value
	    return;
	  }
      }

  }
