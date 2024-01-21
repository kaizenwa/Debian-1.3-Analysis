//===============================================================
// vlistc.cxx	- ListCmd
//
// Copyright (C) 1995,1996  Bruce E. Wampler
//
// This file is part of the V C++ GUI Framework, and is covered
// under the terms of the GNU Library General Public License,
// Version 2. This library has NO WARRANTY. See the source file
// vapp.cxx for more complete information about license terms.
//===============================================================

#include <v/vxutil.h>	// Motif/Athena mapping
#include <v/vlistc.h>	// our definitions
#include <v/vcmdprnt.h>	// a command parent
#include <v/vapp.h>
#include <v/vctlclrs.h>

extern "C"
{
#include <X11/Xlib.h>

#ifdef Motif
#include <Xm/ScrollBar.h>
#include <Xm/RowColumn.h>
#include <Xm/List.h>
#else
#include <X11/Xaw/Form.h>
#ifdef USE3D
#include <v/awscl3d.h>
#define sbWidgetClass scrollbar3dWidgetClass
#define sbThickness 15
#else
#include <X11/Xaw/Scrollbar.h>
#define sbWidgetClass scrollbarWidgetClass
#define sbThickness 10
#endif
#include <X11/Xaw/List.h>
#endif

}

#ifdef Motif
#define Nsensitive XmNsensitive
#else
#define Nsensitive XtNsensitive
#endif

    static char* EmptyItem = "";	// null item for short lists

//=================>>> vListCmd::vListCmd <<<=======================
  vListCmd::vListCmd(vCmdParent* dp, CommandObject* dc) :
	    vCmd(dp, dc)
  {
    // Create bitmaps if need to

    _fscale = 0.0;
    _scale = 0;
    _maxWidth = 10;
    initialize();			// and initialize
  }

//=======================>>> vListCmd::~vListCmd <<<=======================
  vListCmd::~vListCmd()
  {

    SysDebug(Constructor,"vListCmd::~vListCmd() Destructor\n")

  }

//=====================>>> vListCmd::initialize <<<=======================
  void vListCmd::initialize(void)
  {

#ifndef Motif	// Athena
    XtCallbackRec scrollProcList[] =
      {
	{ CScrollProcCB, this },
	{ (XtCallbackProc) 0, (XtPointer) 0}
      };

    XtCallbackRec jumpProcList[] =
      {
	{ CJumpProcCB, this },
	{ (XtCallbackProc) 0, (XtPointer) 0}
      };
#endif

    Dimension listHeight;

    SysDebug(Constructor,"vListCmd::vListCmd() constructor\n")

    CopyToLocal();		// make local copies

    // First, setup the list

    for (int ix = 0 ; ix < MAX_LIST ; ++ix)
	_workList[ix] = 0;

    SetupList();

    // build a button command for use in a parent window

    Widget WfHoriz = _parentWin->getWidgetFromId(dlgCmd->cRightOf);
    Widget WfVert = _parentWin->getWidgetFromId(dlgCmd->cBelow);

    // We will put a box around the list and the scrollbar

    int map = !(dlgCmd->attrs & CA_Hidden);

#ifdef Motif
    wBox = XtVaCreateManagedWidget(
	"vListFrame",
	xmRowColumnWidgetClass,
	wParent,			// parent
	XmNorientation, XmHORIZONTAL,
	Nbelow(WfVert)			// Form constratints
	NrightOf(WfHoriz)
	XmNspacing,0,
	XmNmappedWhenManaged, map,
        NULL);
#else
    wBox = XtVaCreateManagedWidget(
	"vListFrame",		// name
	formWidgetClass,	// class
	wParent,		// parent
	XtNorientation, XtEhorizontal,
	Nbelow(WfVert)		// Form constratints
	NrightOf(WfHoriz)
	XtNborderWidth,1,	// border around list and scrollbar
	XtNdefaultDistance, 1,
	XtNresizable, True,
	XtNmappedWhenManaged, map,
	XtNbackground, _vControlFace,
	NULL);
#endif

    _maxWidth = (dlgCmd->attrs & CA_Large) ? (_maxWidth * 3)/2 
					      :	_maxWidth;

#ifdef Motif
    wCmd = XtVaCreateManagedWidget(
	"vList",			// name
	xmListWidgetClass,	// class
	wBox,			// parent
	XmNitemCount, MAX_LIST,	// always MAX_LIST strings
	XmNitems, (XmString)_workList,	// the work list
	XmNwidth, _maxWidth + 20,
	XmNrecomputeSize,0,
	XmNlistSizePolicy, XmCONSTANT,
	XmNvisibleItemCount,MAX_LIST,
	NULL);
    // add callback for action
    XtAddCallback(wCmd, XmNsingleSelectionCallback, CListCmdCB, (XtPointer)this);
    XtAddCallback(wCmd, XmNbrowseSelectionCallback, CListCmdCB, (XtPointer)this);
#else
    wCmd = XtVaCreateManagedWidget(
	"vList",			// name
	listWidgetClass,	// class
	wBox,			// parent
	XtNdefaultColumns, 1,	// 1 column
	XtNforceColumns, True,	// force columns
	XtNlist, _workList,	// the work list
	XtNlongest, _maxWidth,	// widest string
	XtNnumberStrings, MAX_LIST,	// always MAX_LIST strings
	XtNverticalList, TRUE,	// we are a vertical list
	XtNrowSpacing, 1,
	XtNresizable, True,
	NULL);
    // add callback for action
    XtAddCallback(wCmd, XtNcallback, CListCmdCB, (XtPointer)this);
#endif
    
    // Create the scroll bar

    XtVaGetValues(wCmd, XtNheight, &listHeight, NULL);

#ifdef Motif
    _wScroll = XtVaCreateManagedWidget(
	"vProgressBar",         // name
	xmScrollBarWidgetClass, // class
	wBox,                   // parent
	XmNorientation, XmVERTICAL,
	XmNminimum,0,           // go from 0 to 100
	XmNmaximum,100,
	XmNheight, listHeight,
	XmNwidth, 15,
	XmNborderWidth, 0,      // no border
	NULL);

    XtAddCallback(_wScroll, XmNvalueChangedCallback, CJumpProcCB, (XtPointer)this);
    XtAddCallback(_wScroll, XmNincrementCallback, CScrollProcCB, (XtPointer)this);
    XtAddCallback(_wScroll, XmNdecrementCallback, CScrollProcCB, (XtPointer)this);
    
#else
    Dimension min_thumb = 10;

    _wScroll = XtVaCreateManagedWidget(
	"vListScroll",		// name
	sbWidgetClass,	// class
	wBox,			// parent
	XtNscrollProc, scrollProcList,	// callback for scrolling
	XtNjumpProc, jumpProcList,	// callback for scrolling
	XtNminimumThumb,min_thumb,
	XtNthickness, sbThickness,	// how wide
	XtNlength,listHeight,	// high as list
	XtNfromHoriz, wCmd,
	XtNresizable, TRUE,
#ifdef USE3D
        XtNbackground, _vControlFace, 
        XtNscrollbar3dBackground, _vControlBG,
#endif  
	NULL);

#endif

    SetScroll(_ScrlShown, _ScrlTop);

    // Need to set to insensitive if it is set insensitive
    if (!_Sensitive)
      {
	XtVaSetValues(wCmd,		// the widget to set
	    Nsensitive, 0,		// change to insensitive
	    NULL);
	XtVaSetValues(_wScroll,		// the widget to set
	    Nsensitive, 0,		// change to insensitive
	    NULL);
      }

  }

//====================>>> vListCmd::DoScroll <<<==========================
  void vListCmd::DoScroll(int cnt)
  {
    // Scroll the list by cnt lines

    int newTop;
    int maxMap = _numItems - MAX_LIST;

    if (_numItems <= MAX_LIST)	// not big enough to scroll
	return;

    newTop = _workMapsTo + cnt;	// where we would go
    if (cnt > 0)		// scrolling up
      {
	if (_workMapsTo == maxMap)	// already at bottom
	    return;
	_workMapsTo = (newTop > maxMap) ? maxMap : newTop;
      }
    else
      {
	if (_workMapsTo == 0)		// already at top
	    return;
	_workMapsTo = (newTop < 0) ? 0 : newTop;
      }

    // Now redraw list
    for (int ix = 0 ; ix < MAX_LIST ; ++ix)
      {
#ifdef Motif
	if ((XmString)_workList[ix] != 0)		// free the string space
	  {
	    XmStringFree((XmString)_workList[ix]);
	  }
	_workList[ix] = (char*)XmStringCreateLocalized(_fullList[ix + _workMapsTo]);
#else
	_workList[ix] = _fullList[ix + _workMapsTo];
#endif
      }

#ifdef Motif
    XmListReplaceItemsPosUnselected(wCmd, (XmString*)_workList, MAX_LIST, 1);
    XtVaSetValues(wCmd, 
	XmNwidth, _maxWidth+20,	// widest string
	NULL);
#else
    XawListChange(wCmd, _workList, MAX_LIST, _maxWidth, True);
#endif

    // See if the currently selected item is on the list, and rehighlight
    int newSel = _curSelection - _workMapsTo;

    if (newSel >= 0 && newSel < MAX_LIST)
      {
#ifdef Motif
	XmListSelectPos(wCmd, newSel+1, 0);
#else
	XawListHighlight(wCmd, newSel);
#endif
      }
    // _workMapsTo varies from 0 to _numItems - MAX_LIST
    // _ScrlTop varies from 0 to 100 - _ScrlShown

    // Use _scale to transform the current _workMapsTo to a _ScrlTop

    if (_workMapsTo == (_numItems - MAX_LIST))	// Account for integer scaling
	_ScrlTop = 100 - _ScrlShown;

    if (_fscale != 0.0)
	_ScrlTop = (int)((float)_workMapsTo * _fscale);
    else if (_scale != 0)
	_ScrlTop = _workMapsTo * _scale;
    else
	_ScrlTop = (int) ((float)_workMapsTo * _scale);

    SetScroll(_ScrlShown, _ScrlTop);
  }


//==================>>> vListCmd::GetCmdValue <<<=========================
  int vListCmd::GetCmdValue(ItemVal id)
  {
    if (id != _cmdId)
	return -1;
    return _curSelection;
  }

//=====================>>> vListCmd::SetCmdVal <<<=========================
  void vListCmd::SetCmdVal(ItemVal val, ItemSetType st)
  {

    SysDebug2(Misc,"vListCmd::SetCmdVal(val:%d, type:%d)\n",val,st)

    if (st == ChangeList || st == ChangeListPtr)
      {
	if (st == ChangeListPtr)
	    _itemList = dlgCmd->itemList;

	int oldMax = _maxWidth;		// track current max width

	SetupList();			// resetup the list
	if (oldMax > _maxWidth)
	    _maxWidth = oldMax;		// don't let it get narrower

	// turn them all off!
#ifdef Motif
	XmListReplaceItemsPosUnselected(wCmd, (XmString*)_workList, MAX_LIST, 1);
	XtVaSetValues(wCmd, 
	    XmNwidth, _maxWidth+20,	// widest string
	    NULL);
#else
	XawListChange(wCmd, _workList, MAX_LIST, _maxWidth, True);
#endif
	SetScroll(_ScrlShown, _ScrlTop);
	SetCmdVal(val,Value);
	return;

      }
    else if (st == Hidden)		// hide or unhide
      {
	if (val)
	  {
	    XtUnmapWidget(wBox);	// unmap this widget
	  }
	else
	  {
	    XtMapWidget(wBox);	// unmap this widget
	  }
      }
    else if (st == Value)
      {
	if (val >= _numItems )
	    return;

	if (val < 0)		// unselect
	  {
	    _curSelection = -1;
	    // turn them all off!
#ifdef Motif
	    XmListDeselectAllItems(wCmd);
#else
	    XawListChange(wCmd, _workList, MAX_LIST, _maxWidth, True);
#endif
	    return;
	  }

SetValue:
	// Now set appropriate _curSelection, scroll, and highlight

	_curSelection = val;	// change the current value

	// if the selection is already on the screen, then highlight it
	// See if the currently selected item is on the list, and rehighlight

	int newSel = _curSelection - _workMapsTo;

	if (newSel >= 0 && newSel < MAX_LIST)
	  {
#ifdef Motif
	    XmListDeselectAllItems(wCmd);
	    XmListSelectPos(wCmd, newSel+1, 0);
#else
	    XawListChange(wCmd, _workList, MAX_LIST, _maxWidth, True);
	    XawListHighlight(wCmd, newSel);
#endif
	  }
	else			// have to scroll
	  {
	    DoScroll(_curSelection - _workMapsTo);  // change what is displayed
	  }

	// Need to set explicitly if not yet realized
	if (!XtIsRealized(wCmd))
	  {
#ifdef Motif
	    XtVaSetValues(wCmd, 
		XmNwidth, _maxWidth+20,	// widest string
		NULL);
#else
	    XtVaSetValues(wCmd, 
		XtNlongest, _maxWidth,	// widest string
		NULL);
#endif
	  }
      }
    else if (st == Sensitive)
      {
	_Sensitive = val;
	XtVaSetValues(wCmd,		// the widget to set
	    Nsensitive, val,		// change to val
	    NULL);
	XtVaSetValues(_wScroll,		// the widget to set
	    Nsensitive, val,		// change to val
	    NULL);
      }
  }

//====================>>> vListCmd::ListCmdCB <<<=======================
  void vListCmd::ListCmdCB(int index)
  {
    _curSelection = index + _workMapsTo;  // change the current selection

    if (_curSelection >= _numItems )
	_curSelection = -1;

    if (!(dlgCmd->attrs & CA_NoNotify))	// Notify on each selection?
	_parentWin->ProcessCmd(_cmdId, _curSelection, dlgCmd->cmdType);
  }

//====================>>> vListCmd::JumpProcCB <<<=======================
  void vListCmd::JumpProcCB(int percent)
  {
    int shown = _ScrlShown;
    int min_top;
    int newMap;

    min_top = 100 - shown;

    if (percent > min_top)	// have to fix it
      {
	SetScroll(shown, min_top);
	percent = min_top;
      }

    // See if we have changed anything
    if (_fscale != 0.0)
      {
	newMap = (int)((float)percent / _fscale) + 1;
	if (newMap == 1)	// Kludge to get to 0
	    newMap = 0;
      }
    else if (_scale == 0)		// nothing to change
      {
	return;
      }
    else
	newMap = (int)((float)percent / _scale);

    if (newMap != _workMapsTo)		// we've changed the screen
      {
	DoScroll(newMap - _workMapsTo);  // change what is displayed
      }
  }

//====================>>> vListCmd::SetScroll <<<=======================
  void vListCmd::SetScroll(int Shown, int Top)
  {
#ifdef Motif
    XmScrollBarSetValues(_wScroll,Top,Shown,1,1,0);
#else		// Athena
    Arg args[3];		// Used to pass float value

    union
      {
	XtArgVal arg_value;
	float float_value;
      } shown_value, top_value;

    shown_value.float_value = float (Shown / 100.0);
    top_value.float_value = float (Top / 100.0);

    if (sizeof (float) > sizeof (XtArgVal))
      {
	XtSetArg(args[0], XtNtopOfThumb, &top_value);
	XtSetArg(args[1], XtNshown, &shown_value);
      }
    else
      {
	XtSetArg(args[0], XtNtopOfThumb, top_value.arg_value);
	XtSetArg(args[1], XtNshown, shown_value.arg_value);
      }

    XtSetValues(_wScroll, args, 2);		// change it!
#endif
  }

//====================>>> vListCmd::ScrollProcCB <<<=======================
  void vListCmd::ScrollProcCB(int position)
  {
    if (position  > 0)		// scroll list up one line
	DoScroll(1);
    else
	DoScroll(-1);		// scroll list down one line

  }

extern "C"
{
//============================>>> CListCmdCB <<<=============================
  void CListCmdCB(Widget w, XtPointer This, XtPointer lr)
  {
#ifdef Motif
    // need to adjust Motif's indexing from 1 to our indexing from 0
    XmListCallbackStruct* xml = (XmListCallbackStruct*) lr;
    ((vListCmd*)This)->ListCmdCB(xml->item_position - 1);
#else
    ((vListCmd*)This)->ListCmdCB(((XawListReturnStruct*)lr)->list_index);
#endif
  }

//============================>>> CJumpProcCB <<<=============================
  void CJumpProcCB(Widget w, XtPointer This, XtPointer pc_ptr)
  {
#ifdef Motif
    XmScrollBarCallbackStruct* mscroll = 
	(XmScrollBarCallbackStruct*) pc_ptr;

    ((vListCmd*)This)->JumpProcCB(mscroll->value);

#else

    float percent = *(float*)pc_ptr;	// get the percent back

    ((vListCmd*)This)->JumpProcCB((int)(percent*100.));
#endif
  }

//============================>>> CScollProcCB <<<=============================
  void CScrollProcCB(Widget w, XtPointer This, XtPointer position)
  {
#ifdef Motif
    XmScrollBarCallbackStruct* mscroll = 
	(XmScrollBarCallbackStruct*) position;

    if (mscroll->reason == XmCR_INCREMENT)
	((vListCmd*)This)->ScrollProcCB(1);
    else
	((vListCmd*)This)->ScrollProcCB(-1);
#else
    int pos = (int)position;
    ((vListCmd*)This)->ScrollProcCB(pos);
#endif
  }
}

//====================>>> vListCmd::SetupList <<<=======================
  void vListCmd::SetupList(void)
  {
    // Set up the list for use

    int width, len;

    // First, count how many items are in the supplied list

    _workMapsTo = 0;
    _curSelection = -1;

    _fullList = (char**)_itemList;		// list

    for ( _numItems = 0 ; _fullList && _fullList[_numItems] != 0 ; ++_numItems)
      {
	if (_numItems < MAX_LIST)	// copy only first MAX_LIST
	  {
#ifdef Motif
	    if (_workList[_numItems] != 0)		// free the string space
	      {
		XmStringFree((XmString)_workList[_numItems]);
	      }
	    _workList[_numItems] = (char *) XmStringCreateLocalized(_fullList[_numItems]);
#else
	    _workList[_numItems] = _fullList[_numItems];
#endif
	  }

	len = strlen(_fullList[_numItems]);	// strlen
	width = XTextWidth(theApp->_XDefaultFont,
		_fullList[_numItems], len);
	if (width > _maxWidth)
	    _maxWidth = width;		// track largest so far
      }

    // Note that at this point _numItems is how many items are
    // in the user supplied list. We can use this number to null
    // out the rest of the displayed list.
    if (_numItems < MAX_LIST)
      {
	for (int ix = _numItems ; ix < MAX_LIST ; ++ix )
	  {
#ifdef Motif
	    if (_workList[ix] != 0)		// free the string space
	      {
		XmStringFree((XmString)_workList[ix]);
	      }
	    _workList[ix] = (char*) XmStringCreateLocalized(EmptyItem);
#else
	    _workList[ix] = EmptyItem;
#endif
	  }
      }

    // Now set the scrollbar size limits
    if (_numItems > MAX_LIST)
      {
	_ScrlShown = (MAX_LIST * 100) / _numItems;
      }
    else
	_ScrlShown = 100;

    if (_numItems > MAX_LIST)
      {
	_fscale = 0.0;
	_scale = (100 - _ScrlShown) / (_numItems - MAX_LIST);
	if (_scale == 0)
	  {
	    _fscale = ((100. - (float)_ScrlShown) / ((float)_numItems - (float)MAX_LIST));
  	  }
      }
    else
	_scale = 0;		// will leave top at 0

    _ScrlTop =  0;
  }
