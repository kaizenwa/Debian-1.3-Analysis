"""Module wpy_tk.py
This module is necessary for wpy.py when running on a Unix system
using tkintermodule and Tk.  Put it on your Python path.  A Python
version with Tk version 4.0 is required.  Your app should import
only wpy.py, and wpy.py will import this module."""
# This is the file "wpy_tk.py" for tkinter.  It contains all the code which is
# highly dependent on the native OS GUI.  Your app should NOT import it, but
# should import wpy.py instead.
# It is Copyright (C) 1994-95, but is freely distributable.  See the
# copyright notice in the file "copyrite.jim".  THERE ARE
# NO WARRANTIES AT ALL, USE AT YOUR OWN RISK.
# See the copyright notice in copyright.jim.
# Comments to:  Jim Ahlstrom        jim@interet.com.
#
# You could access Tk from your app by using wpy_tk.WpyTk and
# wpy_tk.WpyTkCall, but then you would lose portability to NT.
#
# This layer must send events to the handlers in wpy.py and the app.  See
# functions with name WpytkHandle*.
#
# This version is based on Tk and uses "tkinter", a C interface to
# Tcl/Tk which is included as part of Python.
#/* tkintermodule.c -- Interface to libtk.a and libtcl.a.
#   Copyright (C) 1994 Steen Lumholt */
#
# The Tk version used is 4.0b3, April 1995.

import wpy
import tkinter
import wpycon
import sys, os, string

__version__ = wpytkVersion = "0.41"

wpytkUnsupported = "This feature is not supported using Tk"

WpyTk = None		# Tk environment.
wpytkActiveMDIChild = None	# Currently active MDI child frame
wpytkSequence = 0	# Used to generate unique names.
wpytkNamesToPhys = {}	# Dictionary to translate names to class instances.
			# Some names are deliberately omitted.
wpytkCanvasBd = 2	# Size of canvas border in pixels.
wpytkCanvasRelief = "raised"	# Relief (style) of canvas border.

# Masks for the "state" field of an X event.
_wpytkStateShift	= 1
_wpytkStateControl	= 4

def WpytkTkerror(err):
# Do not display dialog box for Tk background errors.
  pass

def WpytkExit(code="0"):
# Call Python exit instead Tk exit.
  cancel = 0
  for templ in wpy.wpyApp.wpyChildList:
    for doc in templ.wpyChildList[:]:
      if not _WpyPhysOkToClose(doc):
        cancel = 1
        break
      doc.OnCloseDocument()
  if not cancel:
    wpy.wpyApp.ExitInstance()
    sys.exit(string.atoi(code))

def WpytkHandleChar(tkname, keyname, keynum, keyascii):
  if len(keyascii) != 1:
    return
  try:
    frame = wpytkNamesToPhys[tkname]
  except (KeyError):
    return
  if frame.wpyFlags & wpy.wpyFlagsEdit:
    parent = frame.wpyParent
    if parent.wpyFlags & wpy.wpyFlagsDialog:
      if keyascii == "\r":
        if frame.wpyClassType != wpy.CMultiEdit:
          parent.OnEnter()
      elif keyascii == "\033":
        parent.OnEsc()
    else:
      frame.OnChar(keyascii, 0)
    return
  if frame.wpyFlags & wpy.wpyFlagsDialog:
    if keyascii == "\r":
      frame.OnEnter()
    elif keyascii == "\033":
      frame.OnEsc()
    return
  if frame.wpyFlags & wpy.wpyFlagsView:
    frame.OnChar(keyascii, 0)
    return
  try:
    view = frame.wpyChildList[0]
  except (AttributeError, IndexError):
    return
  if view.wpyFlags & wpy.wpyFlagsView:
    view.OnChar(keyascii, 0)
  #BUG: flags not returned, extended keys not handled

def WpytkHandleButton(tkname):
# Handle button press events.
  WpyTkCall("update")	# to show the button as "up"
  wpytkNamesToPhys[tkname].WpyPhysOnButton()

def WpytkHandleMenu(tkname):
# Handle menu command
  wpytkNamesToPhys[tkname].WpyPhysOnCommand()

def WpytkHandleDestroy(tkname):
  pass #print "Destroy", tkname

def WpytkHandleClose(tkname):	# BUG
  try:
    self = wpytkNamesToPhys[tkname]
  except KeyError:
    return
  # self.WpyPhysOnClose(None)

# wpytkUpdateRect
# None means there is no update rectangle
# wpytkUpdateRect == self means the whole view is invalid
# otherwise, it is a valid rectangle

def WpyTest(self, DC):	# Return the update rectangle (test version)
  if self.wpytkUpdateRect == None or self.wpytkUpdateRect == self:
    return 0, 0, 999999, 999999
  r = self.wpytkUpdateRect
  return r.wpyLocX, r.wpyLocY, r.wpyLocX + r.wpySizeX, r.wpyLocY + r.wpySizeY

def WpytkHandleDraw(tkname):
  self = wpytkNamesToPhys[tkname]
  if self.wpytkUpdateRect == None or self.wpytkUpdateRect == self:
    # Clear the entire canvas of drawing items
    WpyTkCall(tkname, "delete", "dc")
  else:
    # Clear items within the update rect
    x1 = self.wpytkUpdateRect.wpyLocX
    y1 = self.wpytkUpdateRect.wpyLocY
    x2 = x1 + self.wpytkUpdateRect.wpySizeX
    y2 = y1 + self.wpytkUpdateRect.wpySizeY
    WpyTkCall(tkname, "addtag", "delete_this", "enclosed", x1, y1, x2, y2)
    WpyTkCall(tkname, "delete", "delete_this")
  a, b, w, h = GetClientRect(self)
  self.WpyPhysOnDraw(self.wpyDC, 0, w, h)
  self.wpyphysDrawing = 0
  self.wpytkUpdateRect = None

def WpytkHandleMDIMain():
  if not wpytkActiveMDIChild:
    WpyTkCall("wm", "deiconify", ".")

previous_focus = None
def WpytkHandleFocusIn(tkname, detail):
  try:
    self = wpytkNamesToPhys[tkname]
  except KeyError:
    return
  if self.wpyFlags & wpy.wpyFlagsFrame:
    global previous_focus
    self.WpyPhysOnSetFocus(previous_focus)
    previous_focus = self
    if self.wpyClassType == wpy.CMDIChildWnd:
      global wpytkActiveMDIChild
      wpytkActiveMDIChild = self
      wpy.wpyApp.wpyMainFrame.RecalcLayout()
  elif self.wpyFlags & wpy.wpyFlagsDialog:
    global previous_focus
    self.WpyPhysOnSetFocus(previous_focus)
    previous_focus = self

def WpytkHandleFocusOut(tkname, detail):
  if tkname[-5:] == "popup":	# loss of focus in popup menu
    WpyTkCall("destroy", tkname)		# destroy popup
  try:
    self = wpytkNamesToPhys[tkname]
  except KeyError:
    return
  if self.wpyFlags & wpy.wpyFlagsFrame:
    self.WpyPhysOnKillFocus(None)

# Make a dictionary for converting X mouse states to NT.
MOUSE_MASK = _wpytkStateShift + _wpytkStateControl + 0x0700
MOUSE_FLAGS = {}
for a in (0, 0), (_wpytkStateShift, wpycon.MK_SHIFT):
  for b in (0, 0), (_wpytkStateControl, wpycon.MK_CONTROL):
    for c in (0, 0), (0x0100, wpycon.MK_LBUTTON):
      for d in (0, 0), (0x0200, wpycon.MK_MBUTTON):
        for e in (0, 0), (0x0400, wpycon.MK_RBUTTON):
          state = a[0] + b[0] + c[0] + d[0] + e[0]
          flags = a[1] + b[1] + c[1] + d[1] + e[1]
          MOUSE_FLAGS[state] = flags

def WpytkHandleMouse(tkname, eventtype, button, locx, locy, state):
# Handle mouse events.  These can only be sent to a window with wpyFlagsMouse.
  try:
    self = wpytkNamesToPhys[tkname]
  except KeyError:
    return
  if not self.wpyFlags & wpy.wpyFlagsMouse:
    return
  x   = string.atoi(locx)
  y   = string.atoi(locy)
  #while not self.wpyFlags & wpy.wpyFlagsWINDOW:	# Find the window
  #  if self.wpyParent == None:		# We are up to "App".
  #    return
  #  x = x + self.wpyLocX	# Convert to parent coordinates.
  #  y = y + self.wpyLocY
  #  self = self.wpyParent
  state = string.atoi(state)
  flags = MOUSE_FLAGS[state & MOUSE_MASK]
  if eventtype == "m":		# move
    self.OnMouseMove(x, y, flags)
  elif eventtype == "p":	# press
    if button == "1":
      self.OnLButtonDown(x, y, flags)
    elif button == "3":
      self.OnRButtonDown(x, y, flags)
  elif eventtype == "r":	# release
    if button == "1":
      self.OnLButtonUp(x, y, flags)
    elif button == "3":
      self.OnRButtonUp(x, y, flags)
  elif eventtype == "d":	# double click
    if button == "1":
      self.OnLButtonDblClk(x, y, flags)
    elif button == "3":
      self.OnRButtonDblClk(x, y, flags)

def WpytkHandleConfigure(tkname, x, y, w, h, send):
# Handle Configure events.  These happen on resize or window move.
# Only used for toplevel windows since the user can not resize child windows.
# Only the size is used, so we can generate a size event on a resize by the user.
# NOTE: Window location is invalid (unless "send" is "1" ???).
  try:
    self = wpytkNamesToPhys[tkname]
  except KeyError:
    return
  if not self.wpyFlags & wpy.wpyFlagsView:
    return
  w = string.atoi(w)
  h = string.atoi(h)
  #print "configure", tkname, self, x, y, w, h, send
  if w != self.wpytkOrigSizeX or h != self.wpytkOrigSizeY:
    self.wpytkOrigSizeX = w
    self.wpytkOrigSizeY = h
    self.WpyPhysOnSize(0, w, h)
    self.wpyParent.RecalcLayout()

def WpytkHandleScroll(tkname, cmd, amt, units = None):
  self = wpytkNamesToPhys[tkname]
  if cmd == "scroll":
    if units == "units":	# one unit
      pos = self.wpyScrollPos + string.atoi(amt)
    else:			# one page
      pos = self.wpyScrollPos + string.atoi(amt) * self.wpyScrollWinSize
  else:				# moveto
    pos = int(string.atof(amt) * self.wpyScrollSize + 0.5)
  self.SetScrollPos(pos)
  self.WpyPhysOnScroll(self.wpyScrollPos)

def WpytkHandleVScroll(tkname, *args):
  apply(WpyTkCall, (tkname, "yview") + args)
  #return
  self = wpytkNamesToPhys[tkname]
  pos = string.atof(string.split(WpyTkCall(tkname, "yview"))[0])
  pos = int(pos * self.wpyVScrollSize + 0.5)
  self.WpyPhysOnVScroll(pos)

def WpytkHandleHScroll(tkname, *args):
  apply(WpyTkCall, (tkname, "xview") + args)
  #return
  self = wpytkNamesToPhys[tkname]
  pos = string.atof(string.split(WpyTkCall(tkname, "xview"))[0])
  pos = int(pos * self.wpyHScrollSize + 0.5)
  self.WpyPhysOnHScroll(pos)

def WpytkHandleTimer(tkname):
  try:
    tup = wpytkNamesToPhys[tkname]
    del wpytkNamesToPhys[tkname]
  except KeyError:
    pass
  else:
    apply(wpy.wpyApp.WpyphysTimerProc, tup)

def WpytkPostCommand(tkname):
  self = wpytkNamesToPhys[tkname]
  grayed = wpycon.MF_GRAYED
  skip = wpycon.MF_SEPARATOR
  index = 0	# Must be "1" if tearoff menus are used
  for item in self.wpyChildList:
    flags = item.wpyphysMenuFlags
    if item.wpyphysTkMenuFlags != flags:
      item.wpyphysTkMenuFlags = flags
      if not flags & skip:
        if flags & grayed:
          WpyTkCall(tkname, "entryconfigure", index, "-state", "disabled")
        else:
          WpyTkCall(tkname, "entryconfigure", index, "-state", "normal")
    index = index + 1

#####################################################
###  Start of supported wpy_nt methods

def AddDocTemplate(templ):
  pass

def AddString(self, string):	# Add an item to a list box
  if '\t' in string:	# Support for tabs
    sizeb, h = GetTextExtent(self, "          ")	# size of blank
    sizeb = sizeb / 10
    tabstop = self.wpyCharSizeX * 8
    s = ""
    for ch in string:
      if ch != '\t':
        s = s + ch
      else:
        width, h = GetTextExtent(self, s)
        newwidth = width / tabstop * tabstop + tabstop
        nblanks = (newwidth - width + sizeb / 2) / sizeb
        s = s + " " * nblanks
    WpyTkCall(self.wpytkName, "insert", "end", s)
  else:
    WpyTkCall(self.wpytkName, "insert", "end", string)

def AfxMessageBox(text, style = 0):	# Global function
  if style   & wpycon.MB_DEFBUTN1:
    defl = 1
  elif style & wpycon.MB_DEFBUTN2:
    defl = 2
  else:
    defl = 0
  butn = style & wpycon.MB_TYPEMASK
  if butn   == wpycon.MB_OK:
    wpy.CWpyDialog("", text, None, defl, "OK").DoModal()
    return "OK"
  elif butn == wpycon.MB_ABORTRETRYIGNORE:
    buttons = ("Abort", "Retry", "Ignore")
    ret = wpy.CTkIgnEscDialog("", text, None, defl, buttons).DoModal()
  elif butn == wpycon.MB_OKCANCEL:
    buttons = ("OK", "Cancel")
    ret = wpy.CWpyDialog("", text, None, defl, buttons).DoModal()
    if ret == None:
      ret = 1
  elif butn == wpycon.MB_RETRYCANCEL:
    buttons = ("Retry", "Cancel")
    ret = wpy.CWpyDialog("", text, None, defl, buttons).DoModal()
    if ret == None:
      ret = 1
  elif butn == wpycon.MB_YESNO:
    buttons = ("Yes", "No")
    ret = wpy.CTkIgnEscDialog("", text, None, defl, buttons).DoModal()
  elif butn == wpycon.MB_YESNOCANCEL:
    buttons = ("Yes", "No", "Cancel")
    ret = wpy.CWpyDialog("", text, None, defl, buttons).DoModal()
    if ret == None:
      ret = 2
  return buttons[ret]

def WpyAppendEditView(self, text):
  WpyTkCall(self.wpytkName, "insert", "end", text)

def AppendMenu(self, id, flags, index):
  if index >= 0:
    list = self.wpyParent.wpyChildList
    if index < len(list):
      WpytkMenuCreate(self, index, 0)
    else:
      WpytkMenuCreate(self, None, 0)

_OldCursorList = []
def BeginWaitCursor():
  old_cursor  = string.split(WpyTkCall(".", "configure", "-cursor"))[4]
  _OldCursorList.append(old_cursor)
  WpyTkCall(".", "configure", "-cursor", "watch")
  WpyTkCall("update")
  WpyTkCall("after", "idle", "EndWaitCursor")

def CButtonCreate(self):
  name = WpytkNewName(self, None)
  command = ("HandleButton", name)
  if self.wpyClassType == wpy.CPushButton:
    WpyTkCall("button", name,"-text", self.wpyText, "-command", command)
    self.wpytkMyRect = None
    if self.wpyParent.wpyDefaultButton == self:
      self.wpytkRectSize = 2
      WpytkDrawRect(self)
  elif self.wpyClassType == wpy.CCheckButton:
    self.wpyTkIvar = "gvar%d" % wpytkSequence
    WpyTkCall("checkbutton", name,"-text", self.wpyText,
       "-command", command, "-variable", self.wpyTkIvar)
    if self.wpyCheckValue:
      WpyTk.globalsetvar(self.wpyTkIvar, "1")
    else:
      WpyTk.globalsetvar(self.wpyTkIvar, "0")
  elif self.wpyClassType == wpy.CRadioButton:
    try:
      varname = self.wpyGroup.wpyTkVarName
    except AttributeError:
      varname = "gvar%d" % wpytkSequence
      WpyTk.globalsetvar(varname, name)
      self.wpyGroup.wpyTkVarName = varname
    self.wpyTkVarName = varname
    WpyTkCall("radiobutton", name, "-text", self.wpyText,
       "-command", command, "-variable", varname, "-value", name)
  WpytkCtrlColor(self)
  self.wpytkCanvasId = WpyTkCall(self.wpyParent.wpytkName,
      "create", "window", int(self.wpyLocX), int(self.wpyLocY),
      "-width", int(self.wpySizeX), "-height", int(self.wpySizeY),
      "-window", name, "-anchor", "nw")

def CDialogCreate(self, parent):
  _DoDialog(self, parent, 0)

def CEditCreate(self):
  name = WpytkNewName(self, None)
  if self.wpyPasswordChar:
    WpyTkCall("entry", name, "-width", "1", "-show", self.wpyPasswordChar)
  else:
    WpyTkCall("entry", name, "-width", "1")
  WpytkCtrlColor(self)
  self.wpytkCanvasId = WpyTkCall(self.wpyParent.wpytkName,
      "create", "window", int(self.wpyLocX), int(self.wpyLocY),
      "-width", int(self.wpySizeX), "-height", int(self.wpySizeY),
      "-window", name, "-anchor", "nw")

def CFontCreate(self, family, height, weight):	# Create a Tk font name
  f = family[0]
  if f == 'r':
    face = "-*-times-"
  elif f == 's':
    face = "-*-helvetica-"
  elif f == 'm':
    face = "-*-courier-"
  if weight >= wpycon.FW_SEMIBOLD:
    face = face + "bold-"
  else:
    face = face + "medium-"
  if string.find(family, "italic") >= 0:
    face = face + "i-normal--*-"
  else:
    face = face + "r-normal--*-"
  if height == 0:
    height = self.wpyCharSizeY / self.wpyOnePoint
  height = int(height * 10 + 0.5)
  self.wpytkFontName = face + `height` + "-*-*-*-*-*-*"

def CImageCreate(self):
  if self.wpyFileName:
    try:
      name = WpyTkCall("image", "create",  "photo",
        "-file", self.wpyFileName)
      if name:
        self.wpytkName = name
        w = string.atoi(WpyTkCall("image", "width", name))
        h = string.atoi(WpyTkCall("image", "height", name))
        return w, h
    except tkinter.TclError:
      pass
  return (0, 0)

def ResetContent(self):
  WpyTkCall(self.wpytkName, "delete", "0", "end")
  ListBoxRecalc(self)

def CListBoxCreate(self):
  self.wpyFlags = self.wpyFlags + wpy.wpyFlagsMouse
  height = self.wpySizeY / self.wpyLineHeight
  lines = len(self.wpyListBoxItems)
  name = WpytkNewName(self, None, 0)
  self.wpytkScrollName = scrollname = name + ".vscroll"
  self.wpytkName = lboxname = name + ".lbox"
  wpytkNamesToPhys[lboxname] = self
  WpyTkCall("frame", name, "-bd", "0")
  t = (scrollname, "set")
  WpyTkCall("listbox", lboxname, "-width", "1",
      "-height", int(height), "-yscrollcommand", t)
  WpytkCtrlColor(self)
  self.wpySizeY = string.atoi(WpyTkCall( "winfo", "reqheight", lboxname))
  self.wpytkCanvasId = WpyTkCall(self.wpyParent.wpytkName,
      "create", "window", int(self.wpyLocX), int(self.wpyLocY),
      "-width", int(self.wpySizeX), "-height", int(self.wpySizeY),
      "-window", name, "-anchor", "nw")
  t = (lboxname, "yview")
  WpyTkCall("scrollbar", scrollname, "-command", t, "-width", self.wpyScrollWidth)
  ListBoxRecalc(self)

def ListBoxRecalc(self):
  name = self.wpytkName
  scrollname = self.wpytkScrollName
  WpyTkCall("pack", "forget", name)
  WpyTkCall("pack", "forget", scrollname)
  height = self.wpySizeY / self.wpyLineHeight
  lines = len(self.wpyListBoxItems)
  if lines > height:	# Add vertical scroll bar
    WpyTkCall("pack", scrollname, "-side", "right", "-fill", "y")
  WpyTkCall("pack", name, "-side", "left", "-expand", "1", "-fill", "both")

def WpytkCtrlColor(self):
  if hasattr(self, "wpyBrush"):
    color = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    WpyTkCall(self.wpytkName, "configure", "-bg", color)
  if hasattr(self, "wpyTextColor"):
    color = "#%2.2x%2.2x%2.2x" % self.wpyTextColor
    WpyTkCall(self.wpytkName, "configure", "-fg", color)

def CMenuCreate(self, flags):
  pass

def CMDIFrameWndCreate(frame):
  global wpytkSequence
  global wpytkNamesToPhys
  frame.wpytkOrigSizeX = 0
  frame.wpytkOrigSizeY = 0
  frame.wpytkName  = "."
  frame.wpytkTopName = "."
  wpytkNamesToPhys["."] = frame
  t = ("HandleClose", ".")
  WpyTkCall("wm", "protocol", ".", "WM_DELETE_WINDOW", t)
  frame.wpyFrameSizeX = frame.wpyFrameSizeY = 0		# Not used.
  frame.wpytkTopSizeX = -1
  #WpyTkCall("wm", "title", frame.wpytkTopName, frame.wpyText)
  frame.OnCreate(None)
  menu = frame.wpyMenu
  if menu != None:	# Menu for main frame when there are no child frames
    menu.wpytkCreated = 1
    menuframe = ".menuf"
    WpyTkCall("frame", menuframe, "-relief", wpytkCanvasRelief,
           "-bd", wpytkCanvasBd)
    menu.wpytkMenuName = menuframe
    menu.wpytkName = menuframe
    for item in menu.wpyChildList:
      buttonname = WpytkNewName(item, None)
      menuname = item.wpytkMenuName = buttonname + ".menu%d" % wpytkSequence
      if item.wpyEnabled and item.wpyChildList:
        state = "normal"
      else:
        state = "disabled"
      WpyTkCall("menubutton", buttonname, "-text", item.wpyText,
           "-menu", menuname, "-state", state)
      wpytkNamesToPhys[menuname] = item
      t = ("PostCommand", menuname)
      WpyTkCall("menu", menuname, "-postcommand", t, "-tearoff", "0")
      if item.wpyRightSide:
        WpyTkCall("pack", buttonname, "-side", "right")
      else:
        WpyTkCall("pack", buttonname, "-side", "left")
      for obj in item.wpyChildList:
        WpytkMenuCreate(obj)
  status = frame.wpyStatusLine
  if status != None:
    name = ".status"
    status.wpytkName = name
    WpyTkCall("frame", name, "-relief", wpytkCanvasRelief,
           "-bd", wpytkCanvasBd)
    pane0 = name + ".pane0"
    WpyTkCall("label", pane0, "-text", status.wpyText, "-anchor", "w")
    WpyTkCall("pack", pane0, "-in", name,
          "-side", "left", "-fill", "x")
  #if frame.wpySizeX > 0 and frame.wpySizeY > 0:
  #  t = "%dx%d" % (frame.wpySizeX, frame.wpySizeY)
  #else:
  #  t = "%dx%d" % (frame.wpyScreenSizeX/2, frame.wpyScreenSizeY/2)
  #WpyTkCall("wm", "geometry", ".", t)
  frame.RecalcLayout()
  WpyTkCall("update")
  WpyTkCall("after", "idle", "HandleMDIMain")

def CMultiEditCreate(self):
  name = WpytkNewName(self, None)
  if self.wpyPasswordChar:
    WpyTkCall("text", name, "-width", "1", "-show", self.wpyPasswordChar)
  else:
    WpyTkCall("text", name, "-width", "1")
  WpytkCtrlColor(self)
  self.wpytkCanvasId = WpyTkCall(self.wpyParent.wpytkName,
      "create", "window", int(self.wpyLocX), int(self.wpyLocY),
      "-width", int(self.wpySizeX), "-height", int(self.wpySizeY),
      "-window", name, "-anchor", "nw")

def CScrollBarCreate(self):
  name = WpytkNewName(self, None)
  t = ("HandleScroll", name)
  if self.wpyStyle & wpycon.SBS_VERT:
    WpyTkCall("scrollbar", name, "-command", t, "-width", self.wpySizeX)
  else:
    WpyTkCall("scrollbar", name, "-command", t, "-width", self.wpySizeY, \
          "-orient", "horizontal")
  WpytkCtrlColor(self)
  self.wpytkCanvasId = WpyTkCall(self.wpyParent.wpytkName,
      "create", "window", int(self.wpyLocX), int(self.wpyLocY),
      "-width", int(self.wpySizeX), "-height", int(self.wpySizeY),
      "-window", name, "-anchor", "nw")

def CStaticCreate(self):
  name = WpytkNewName(self, None)
  if self.wpyClassType == wpy.CMessage:
    WpyTkCall("message", name, "-text", self.wpyText, \
        "-aspect", int(100 * self.wpyAspect + 50))
  elif self.wpyClassType == wpy.CLabel:
    WpyTkCall("label", name, "-text", self.wpyText, "-anchor", "w")
  WpytkCtrlColor(self)
  self.wpytkCanvasId = WpyTkCall(self.wpyParent.wpytkName,
      "create", "window", int(self.wpyLocX), int(self.wpyLocY),
      "-width", int(self.wpySizeX), "-height", int(self.wpySizeY),
      "-window", name, "-anchor", "nw")

def EndWaitCursor():
  if _OldCursorList:
    old_cursor = _OldCursorList[-1]
    del _OldCursorList[-1]
    if old_cursor == "{}":
      old_cursor = ""
    WpyTkCall(".", "configure", "-cursor", old_cursor)
    WpyTkCall("update")

def DestroyMenu(self):
  try:
    if WpyTkCall("winfo", "exists", self.wpytkName) == "1":
      WpyTkCall("destroy", self.wpytkName)
  except TclError:
    pass

def DestroyWindow(self):
  try:
    WpyTkCall("destroy", self.wpytkTopName)
  except AttributeError:
    WpyTkCall("destroy", self.wpytkName)

def DoModal(self, parent):
  return _DoDialog(self, parent, 1)

def _DoDialog(self, parent, is_modal):
  # If modal, this will not return until the user destroys the window.
  # Make sure there is a way to do that !!!
  global wpytkSequence
  global wpytkNamesToPhys
  wpytkSequence = wpytkSequence + 1
  name = ".obj%d" % wpytkSequence
  self.wpytkTopName = name
  wpytkNamesToPhys[name] = self
  WpyTkCall("toplevel", name)
  WpyTkCall("wm", "withdraw", name)
  oldFocus = win = WpyTkCall("focus")
  if not win:
    win = "."
  t = WpyTkCall("winfo", "toplevel", win)
  WpyTkCall("wm", "transient", name, t)
  if self.wpyLocX <= 0 and self.wpyLocY <= 0:
    space = wpy.wpyApp.wpySizeX / 14
    x = string.atoi(WpyTkCall("winfo", "rootx", win)) + space
    y = string.atoi(WpyTkCall("winfo", "rooty", win)) + space
    self.wpyLocX = x
    self.wpyLocY = y
  t = "+%d+%d" % (self.wpyLocX, self.wpyLocY)
  WpyTkCall("wm", "geometry", name, t)
  t = ("HandleClose", name)
  WpyTkCall("wm", "protocol", name, "WM_DELETE_WINDOW", t)
  wpytkSequence = wpytkSequence + 1
  name = name + ".obj%d" % wpytkSequence
  self.wpytkName = name
  WpyTkCall("canvas", name)
  wpytkNamesToPhys[name] = self
  WpyTkCall("wm", "title", self.wpytkTopName, self.wpyText)
  WpyTkCall(self.wpytkName, "configure",
       "-width", self.wpySizeX, "-height", self.wpySizeY)
  if hasattr(self, "wpyBrush"):
    color = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    WpyTkCall(self.wpytkName, "configure", "-bg", color)
  WpyTkCall("pack", self.wpytkName, "-in", self.wpytkTopName,
        "-expand", "1", "-fill", "both")
  WpyTkCall("wm", "deiconify", self.wpytkTopName)
  if self.OnInitDialog() != 0:
    WpyTkCall("focus", self.wpytkTopName)
  if is_modal:
    WpyTkCall("grab", "set", self.wpytkTopName)
    WpyTkCall("tkwait", "window", self.wpytkTopName)
    WpyTkCall("focus", oldFocus)

def DrawMenuBar(self):
  pass

def EditClear(self):
  WpyTkCall(self.wpytkName, "delete", "sel.first", "sel.last")

def EditClearAll(self):
  WpyTkCall(self.wpytkName, "delete", "1.0", "end")

def EnableMenuItem(self, flag):
  if flag & wpycon.MF_GRAYED:
    WpyTkCall(self.wpytkName, "configure", "-state", "disabled")
  else:
    WpyTkCall(self.wpytkName, "configure", "-state", "normal")

def EnableWindow(self, enable):
  if self.wpyFlags & wpy.wpyFlagsWINDOW:
    pass
  else:
    if enable:
      WpyTkCall(self.wpytkName, "configure", "-state", "normal")
    else:
      WpyTkCall(self.wpytkName, "configure", "-state", "disabled")
  return not self.wpyEnabled

def EndDialog(self, result):
  WpyTkCall("destroy", self.wpytkTopName)

def FileDlgDoModal(self, parent):
  return wpy.CTkFileDlg(self.wpyOpen, self.wpyDefaultName,
               self.wpyFilter).DoModal(parent)

def GetActiveFrameDV():
  global wpytkActiveMDIChild
  frame = wpytkActiveMDIChild
  if frame == None:
    frame = wpy.wpyApp.wpyMainFrame
    if frame.wpyClassType == wpy.CMDIFrameWnd:
      return None, None, None
  view = frame.wpyChildList[0]
  return frame, frame.wpyParent, view

def GetClientRect(self):
  name = self.wpytkName	# BUG
  WpyTkCall("update")
  w  = string.atoi(WpyTkCall( "winfo", "width", name))
  h = string.atoi(WpyTkCall( "winfo", "height", name))
  return 0, 0, w, h

def GetDeviceCaps(DC, flag):
  if flag == wpycon.HORZSIZE:
    t = WpyTkCall("winfo", "screenmmwidth", ".")
    return string.atoi(t)
  elif flag == wpycon.HORZRES:
    t = WpyTkCall("winfo", "screenwidth", ".")
    return string.atoi(t)
  else:
    raise wpytkUnsupported

def GetDeviceScrollPosition(self):
  x = int(string.atof(WpyTkCall(self.wpytkName, "canvasx", "0"))) + wpytkCanvasBd
  y = int(string.atof(WpyTkCall(self.wpytkName, "canvasy", "0"))) + wpytkCanvasBd
  return (x, y)

def GetDrawnObject(self, x, y):
  list = string.split(WpyTkCall(self.wpytkName, "find", "overlapping", x, y, x + 1, y + 1))
  for id in list:
    try:
      obj = self.wpytkIdToItem[id]
      if obj.wpyClassType != wpy.CDrawnLine:
        return obj
    except KeyError:
      pass

def GetIndexXY(self, view, x, y):
  index = WpyTkCall(view.wpytkName, "index", self.wpytkId, "@%d,%d" % (x, y))
  if index:
    return string.atoi(index)
  return 0

def GetLine(self, line_num):
  i = "%d" % (line_num + 1)
  return WpyTkCall(self.wpytkName, "get", i + ".0", i + ".end")

def GetLineCount(self):
  return string.atoi(string.splitfields(WpyTkCall
       (self.wpytkName, "index", "end"), ".")[0]) - 1

def GetSel(self):
  l = string.split(WpyTkCall
       (self.wpytkName, "tag", "ranges", "sel"))
  if l:
    x = string.splitfields(l[0], ".")
    l1 = string.atoi(x[0]) - 1
    c1 = string.atoi(x[1])
    x = string.splitfields(l[1], ".")
    l2 = string.atoi(x[0]) - 1
    c2 = string.atoi(x[1])
    return (l1, c1, l2, c2)
  else:
    l = string.splitfields(WpyTkCall
       (self.wpytkName, "index", "insert"), ".")
    l1 = string.atoi(l[0]) - 1
    c1 = string.atoi(l[1])
    return (l1, c1, l1, c1)

def GetSystemMetrics(flag):
  if flag == wpycon.SM_CXSCREEN:
    t = WpyTkCall("winfo", "screenwidth", ".")
    return string.atoi(t)
  elif flag == wpycon.SM_CYSCREEN:
    t = WpyTkCall("winfo", "screenheight", ".")
    return string.atoi(t)
  else:
    raise wpytkUnsupported

def GetTextExtent(wnd, text):
  WpyTkCall("label", ".m10", "-text", text, "-bd", "0")
  x  = string.atoi(WpyTkCall( "winfo", "reqwidth", ".m10"))
  y = string.atoi(WpyTkCall( "winfo", "reqheight", ".m10"))
  WpyTkCall("destroy", ".m10")
  return x, y

def GetWindowRect(self):
  name = self.wpytkName	# BUG
  x  = string.atoi(WpyTkCall( "winfo", "x", name))
  y = string.atoi(WpyTkCall( "winfo", "y", name))
  w  = string.atoi(WpyTkCall( "winfo", "width", name))
  h = string.atoi(WpyTkCall( "winfo", "height", name))
  return x, y, x + w, y + h

def GetWindowText(self):
  if self.wpyFlags & wpy.wpyFlagsEdit:
    if self.wpyClassType == wpy.CEdit:
      return WpyTkCall(self.wpytkName, "get")
    else:	# CMultiEdit
      return WpyTkCall(self.wpytkName, "get", "1.0", "end")
  else:
    return self.wpyText

def InvalidateRect(self, rect, bool):
  if not self.wpyFlags & wpy.wpyFlagsView:
    return
  if self.wpyClassType == wpy.CEditView:
    return
  if rect == None:	# Invalidate whole view
    self.wpytkUpdateRect = self
    if self.wpyClassType == wpy.CScrollView:
      self.wpytkIdToItem = {}	# Reset dictionary of drawn objects
  elif self.wpytkUpdateRect == self:	# Whole view is invalidated
    pass
  else:
    if self.wpytkUpdateRect:		# Add area to update rect
      if self.wpytkUpdateRect.wpyLocX > rect.wpyLocX:
         self.wpytkUpdateRect.wpyLocX = rect.wpyLocX
      if self.wpytkUpdateRect.wpyLocY > rect.wpyLocY:
         self.wpytkUpdateRect.wpyLocY = rect.wpyLocY
      x2 = rect.wpyLocX + rect.wpySizeX
      y2 = rect.wpyLocY + rect.wpySizeY
      if self.wpytkUpdateRect.wpyLocX + self.wpytkUpdateRect.wpySizeX < x2:
        self.wpytkUpdateRect.wpySizeX = x2 - self.wpytkUpdateRect.wpyLocX
      if self.wpytkUpdateRect.wpyLocY + self.wpytkUpdateRect.wpySizeY < x2:
        self.wpytkUpdateRect.wpySizeY = y2 - self.wpytkUpdateRect.wpyLocY
    else:			# Copy area to update rect
      self.wpytkUpdateRect = wpy.CRect()
      self.wpytkUpdateRect.wpyFlags = self.wpyFlags
      self.wpytkUpdateRect.wpyLocX  = rect.wpyLocX
      self.wpytkUpdateRect.wpyLocY  = rect.wpyLocY
      self.wpytkUpdateRect.wpySizeX = rect.wpySizeX
      self.wpytkUpdateRect.wpySizeY = rect.wpySizeY
  if not self.wpyphysDrawing:
    self.wpyphysDrawing = 1
    t = ("HandleDraw", self.wpytkName)
    WpyTkCall("after", "idle", t)

def KillTimer(id):
  WpyTkCall("after", "cancel", id)

def LineLength(self, line_num):
  i = "%d" % (line_num + 1)
  return string.atoi(string.splitfields(WpyTkCall
       (self.wpytkName, "index", i + ".end"), ".")[1])

def WpytkMainLoop():
# Main event loop: wait for and react to events.
  WpyTk.mainloop()

def MoveWindow(self):
  try:		# errors are OK, the window might not exist yet.
    name = self.wpytkName
    canvas = self.wpyParent.wpytkName
  except AttributeError:
    return
  if self.wpyFlags & wpy.wpyFlagsWINDOW:
    pass
  else:
    if self.wpyVisible:
      WpyTkCall(canvas, "coords", self.wpytkCanvasId,
           int(self.wpyLocX), int(self.wpyLocY))
      WpyTkCall(canvas, "itemconfigure", self.wpytkCanvasId,
           "-width", int(self.wpySizeX), "-height", int(self.wpySizeY))
    if self.wpytkRectSize:
      WpytkDrawRect(self)

def _WpyPhysOkToClose(doc):
  if doc.wpyModifiedFlag:
    text = "Save changes to %s ?" % doc.wpyFileName
    ret = wpy.CWpyDialog("", text, 99, -1, "Yes", "No", "Cancel").DoModal()
    if ret == 0:
      doc.OnSaveDocument(doc.wpyFileName)
      return 1
    elif ret == 1:
      return 1
    else:
      return 0
  else:
    return 1

def _WpyPhysOkToReplace(path):
  text = "%s already exists.\nDo you want to replace it?" % path
  ret = wpy.CWpyDialog("", text, 99, -1, "Yes", "No").DoModal()
  if ret == 0:
    return 1
  return 0

def OnCloseDocument(self):
  global wpytkActiveMDIChild
  self.DeleteContents()
  self.SetModifiedFlag(0)
  for view in self.wpyViewList[:]:
    view.WpyPhysOnDestroy()
  newchild = 0
  for frame in self.wpyChildList[:]:
    frame.WpyPhysOnDestroy()
    WpyTkCall("destroy", frame.wpytkTopName)
    if frame == wpytkActiveMDIChild:
      newchild = 1
  self.wpyParent.wpyChildList.remove(self)
  if newchild:
    main = wpy.wpyApp.wpyMainFrame
    list = main.wpyWindowList
    if list:
      wpytkActiveMDIChild = list[0]
    else:
      wpytkActiveMDIChild = None
      WpyTkCall("after", "idle", "HandleMDIMain")
    main.RecalcLayout()

def OnFileNew(templ):
  return _OnFileNewOrOpen(templ, None)

def OnFileOpen(templ):
  filename = wpy.CTkFileDlg(1).DoModal()
  if filename:
    return _OnFileNewOrOpen(templ, filename)
  else:
    return None, None, None, None

def _OnFileNewOrOpen(templ, filename):
  if templ.wpyClassType == wpy.CSingleDocTemplate:
    main = wpy.wpyApp.wpyMainFrame
    if main == None:
      doc = templ.wpyDocument(templ)
      frame = templ.wpyFrame(templ, doc)
      view = templ.wpyView(templ, doc, frame)
      WpytkCreateFrame(templ, doc, frame, view)
      if filename:
        doc.OnOpenDocument(filename)
      else:
        doc.OnNewDocument()
      WpyTkCall("update")
      view.OnInitialUpdate()
      view.InvalidateRect()
      return templ, doc, frame, view
    else:
      doc = main.wpyParent
      if filename:
        doc.OnOpenDocument(filename)
      else:
        doc.OnNewDocument()
      WpyTkCall("update")
      view = doc.wpyViewList[0]
      view.OnInitialUpdate()
      view.InvalidateRect()
      return templ, None, None, None
  else:
    global wpytkActiveMDIChild
    doc = templ.wpyDocument(templ)
    frame = templ.wpyFrame(templ, doc)
    view = templ.wpyView(templ, doc, frame)
    if not wpytkActiveMDIChild:
      WpyTkCall("wm", "withdraw", ".")
    wpytkActiveMDIChild = frame
    WpytkCreateFrame(templ, doc, frame, view)
    if filename:
      doc.OnOpenDocument(filename)
    else:
      doc.OnNewDocument()
    WpyTkCall("update")
    view.OnInitialUpdate()
    view.InvalidateRect()
    return templ, doc, frame, view

def OnNewDocument(self):
  self.DeleteContents()
  self.SetModifiedFlag(0)

def OnWindowNew(templ):
  global wpytkActiveMDIChild
  doc = wpytkActiveMDIChild.wpyParent
  frame = templ.wpyFrame(templ, doc)
  view = templ.wpyView(templ, doc, frame)
  wpytkActiveMDIChild = frame
  WpytkCreateFrame(templ, doc, frame, view)
  WpyTkCall("update")
  view.OnInitialUpdate()
  view.InvalidateRect()
  return templ, doc, frame, view

def RecalcLayout(frame, notify, view = None):
  if _Tk_RecalcLayout(frame, notify, view, 0):
    _Tk_RecalcLayout(frame, notify, view, 1)
  
def _Tk_RecalcLayout(frame, notify, view = None, doit):
  slavelist = string.split(WpyTkCall("pack", "slaves", frame.wpytkTopName))
  newslavelist = []
  if doit:
    for slave in slavelist:
      WpyTkCall("pack", "forget", slave)
  if view == None:
    view = frame.wpyChildList
    if view:
      view = view[0]
    else:
      view = None
  menu = frame.wpyMenu
  if menu != None:
    name = menu.wpytkName
    if doit:
      WpyTkCall("pack", name, "-in", frame.wpytkTopName,
          "-side", "top", "-fill", "x")
    else:
      newslavelist.append(name)
  status = frame.wpyStatusLine
  if status != None and status.wpyVisible:
    name = status.wpytkName
    if doit:
      WpyTkCall("pack", name, "-in", frame.wpytkTopName,
          "-side", "bottom", "-fill", "x")
    else:
      newslavelist.append(name)
  if view != None and (view.wpyHScrollSize > view.wpyHScrollWinSize
           or view.wpyClassType == wpy.CEditView):
    name = view.wpytkHScrollName
    if doit:
      WpyTkCall("pack", name, "-in", frame.wpytkTopName,
       "-side", "bottom", "-fill", "x")
    else:
      newslavelist.append(name)
  if view != None and (view.wpyVScrollSize > view.wpyVScrollWinSize
           or view.wpyClassType == wpy.CEditView):
    name = view.wpytkVScrollName
    if doit:
      WpyTkCall("pack", name, "-in", frame.wpytkTopName,
         "-side", "right", "-fill", "y")
    else:
      newslavelist.append(name)
  if frame.wpytkName != ".":
    name = frame.wpytkName
    if doit:
      WpyTkCall("pack", name, "-in", frame.wpytkTopName,
      "-expand", "1", "-fill", "both")
    else:
      newslavelist.append(name)
  if view != None:
    name = view.wpytkName
    if doit:
      WpyTkCall("pack", name, "-in", frame.wpytkName,
      "-expand", "1", "-fill", "both")
  if not doit:
    if slavelist != newslavelist:
      return 1

def WpyRecordApp(self):
# Called when user gets an WpyApp instance.  Only one WpyApp instance allowed.
  global WpyTk
  global WpyTkCall
  self.wpytkName = "app"
  wpytkNamesToPhys["app"] = self
  self.wpytkBaseName = os.path.basename(sys.argv[0])
  if self.wpytkBaseName[-3:] == ".py":
    self.wpytkBaseName = self.wpytkBaseName[:-3]
  WpyTk = tkinter.create(None, self.wpytkBaseName, "Tk", 0)
  WpyTkCall = WpyTk.call
  self.WpyTk = WpyTk
  WpyTk.createcommand("tkerror", WpytkTkerror)
  WpyTk.createcommand("exit", WpytkExit)
  WpyTk.createcommand("EndWaitCursor",	EndWaitCursor)
  WpyTk.createcommand("HandleButton",	WpytkHandleButton)
  WpyTk.createcommand("HandleMenu",	WpytkHandleMenu)
  WpyTk.createcommand("HandleChar",	WpytkHandleChar)
  WpyTk.createcommand("HandleClose",	WpytkHandleClose)
  WpyTk.createcommand("HandleDraw",	WpytkHandleDraw)
  WpyTk.createcommand("HandleMDIMain",	WpytkHandleMDIMain)
  WpyTk.createcommand("HandleDestroy",	WpytkHandleDestroy)
  WpyTk.createcommand("HandleConfigure",	WpytkHandleConfigure)
  WpyTk.createcommand("HandleFocusIn",	WpytkHandleFocusIn)
  WpyTk.createcommand("HandleFocusOut",	WpytkHandleFocusOut)
  WpyTk.createcommand("HandleMouse",	WpytkHandleMouse)
  WpyTk.createcommand("HandleScroll",	WpytkHandleScroll)
  WpyTk.createcommand("HandleVScroll",	WpytkHandleVScroll)
  WpyTk.createcommand("HandleHScroll",	WpytkHandleHScroll)
  WpyTk.createcommand("HandleTimer",	WpytkHandleTimer)
  WpyTk.createcommand("PostCommand",	WpytkPostCommand)
  # Try to read in a Tk option file.
  for dir in sys.path:
    try:
      file = os.path.join(dir, "wpy_tk.dfl")
      fd = open(file, "r")
      fd.close()
      WpyTkCall("option", "readfile", file)
    except IOError:
      pass
  # Exit if user hits Control-C.
  t = ("exit", "3")
  WpyTkCall("bind", "all", "<Control-c>", t)
  t = ("HandleChar", "%W", "%K", "%N", "%A")
  WpyTkCall("bind", "all", "<Key>", t)
  t = ("HandleMouse", "%W", "p", "%b", "%x", "%y", "%s")
  WpyTkCall("bind", "all", "<ButtonPress>", t)
  t = ("HandleMouse", "%W", "r", "%b", "%x", "%y", "%s")
  WpyTkCall("bind", "all", "<ButtonRelease>", t)
  t = ("HandleMouse", "%W", "d", "%b", "%x", "%y", "%s")
  WpyTkCall("bind", "all", "<Double-Button>", t)
  t = ("HandleMouse", "%W", "m", "%b", "%x", "%y", "%s")
  WpyTkCall("bind", "all", "<Shift-Motion>", t)
  WpyTkCall("bind", "all", "<Control-Motion>", t)
  WpyTkCall("bind", "all", "<Button1-Motion>", t)
  WpyTkCall("bind", "all", "<Button2-Motion>", t)
  WpyTkCall("bind", "all", "<Button3-Motion>", t)
  t = ("HandleFocusIn",  "%W", "%d")
  WpyTkCall("bind", "all", "<FocusIn>", t)
  t = ("HandleFocusOut", "%W", "%d")
  WpyTkCall("bind", "all", "<FocusOut>", t)
  #t = ("HandleDestroy", "%W")
  #WpyTkCall("bind", "all", "<Destroy>", t)
  # WpyApp is not a visible object.  The name "." will be assigned
  #   to the first top-level window created.
  WpyTkCall("wm", "withdraw", ".")
  # Get some control and text metrics.
  WpyTkCall("label", ".m10", "-text", "0000000000", "-bd", "0")
  self.wpyCharSizeX  = string.atoi(WpyTkCall(
              "winfo", "reqwidth", ".m10")) / 10
  self.wpyCharSizeY = string.atoi(WpyTkCall(
              "winfo", "reqheight", ".m10"))
  WpyTkCall("destroy", ".m10")
  global wpytkCanvasBd
  global wpytkCanvasRelief
  WpyTkCall("canvas", ".c1")
  wpytkCanvasBd  = string.split(
            WpyTkCall(".c1", "configure", "-bd"))[4]
  wpytkCanvasBd  = string.atoi(wpytkCanvasBd)
  wpytkCanvasRelief = string.split(
            WpyTkCall(".c1", "configure", "-relief"))[4]
  WpyTkCall("destroy", ".c1")
  # Get screen size, WpyApp has the size of the screen.
  self.wpyFrameSizeX = self.wpyFrameSizeY = 0
  self.wpyLocX = self.wpyLocY = 0
  t = WpyTkCall("winfo", "pixels", ".", "100c")
  self.wpyOneMeter = string.atoi(t)

def ReleaseCapture():
  win = wpy.wpyMouseCaptureWindow
  if win != None:
    WpyTkCall("grab", "release", win.wpytkName)

def ReplaceSel(self, text):
  if WpyTkCall(self.wpytkName, "tag", "ranges", "sel"):
    WpyTkCall(self.wpytkName, "delete", "sel.first", "sel.last")
  WpyTkCall(self.wpytkName, "insert", "insert", text)

def PostMessage(self, msg, wparam, lparam):
  return SendMessage(self, msg, wparam, lparam)

def SendMessage(self, msg, wparam, lparam):
  if msg == wpycon.WM_COMMAND:
    if wparam == wpycon.ID_APP_EXIT:
      WpytkExit()
      return
  raise wpytkUnsupported

def SetCapture(self):
  WpyTkCall("grab", "set", self.wpytkName)

def SetCheck(self, checked):
  if self.wpyCreated:
    if self.wpyClassType == wpy.CCheckButton:
      WpyTk.globalsetvar(self.wpyTkIvar, checked)
    elif self.wpyClassType == wpy.CRadioButton:
      if checked:
        WpyTk.globalsetvar(self.wpyGroup.wpyTkVarName, self.wpytkName)

def SetCurSel(self, index):	# Set the selection in a list box
  self.wpySelected = index
  if self.wpyCreated:
    if index >= 0:
      WpyTkCall(self.wpytkName, "selection", "set", index)
    else:
      WpyTkCall(self.wpytkName, "selection", "clear", "0", "end")

def SetFocus(self):
  ret = WpyTkCall("focus")
  if ret:
    ret = wpytkNamesToPhys[ret]
  else:
    ret = None
  WpyTkCall("focus", self.wpytkName)
  return ret

def SetFont(self, font, redraw):
  WpyTkCall(self.wpytkName, "configure", "-font", font.wpytkFontName)

def SetModifiedFlag(self, value):
  try:
    if self.wpyViewList[0].wpyClassType == wpy.CEditView:
      self.wpyModifiedFlag = 1	# Tk edit view can't tell if it is modified.
  except:
    pass

def SetPaneText(self, index, text, update):
  name = self.wpytkName + ".pane" + `index`
  WpyTkCall(name, "configure", "-text", text)

def SetReadOnly(self, readonly):
  if readonly:
    WpyTkCall(self.wpytkName, "configure", "-state", "disabled")
  else:
    WpyTkCall(self.wpytkName, "configure", "-state", "normal")

def SetScrollPosSb(self, pos, redraw):
  if self.wpyScrollSize > 0:
    f1 = float(pos) / self.wpyScrollSize
    f2 = float(pos + self.wpyScrollWinSize) / self.wpyScrollSize
    WpyTkCall(self.wpytkName, "set", f1, f2)

def SetScrollRangeSb(self, min, max, redraw):
  SetScrollPosSb(self, self.wpyScrollPos, redraw)

def SetScrollSizes(self):
  t = "0 0 %d %d" % (self.wpyHScrollSize, self.wpyVScrollSize)
  WpyTkCall(self.wpytkName, "configure", "-scrollregion", t)
  self.wpyParent.RecalcLayout()

def SetSel(self, l1, c1, l2, c2, noscroll):
  if self.wpyClassType == wpy.CEditView:
    p1 = "%d.%d" % (l1 + 1, c1)
    p2 = "%d.%d" % (l2 + 1, c2)
    if p1 == p2:
      WpyTkCall(self.wpytkName, "mark", "set", "insert", p1)
      if not noscroll:
        WpyTkCall(self.wpytkName, "see", p1)
    else:
      WpyTkCall(self.wpytkName, "tag", "add", "sel", p1, p2)
      if not noscroll:
        WpyTkCall(self.wpytkName, "see", p1)
        WpyTkCall(self.wpytkName, "see", p2)
  else:		# Class type CEdit
    if c1 < c2:
      WpyTkCall(self.wpytkName, "selection", "range", c1, c2)
    WpyTkCall(self.wpytkName, "icursor", c2)

def WpySetStatusText(self, text):
  pass

def SetTimer(tup, delay):
  global wpytkSequence
  wpytkSequence = wpytkSequence + 1
  name = "timer%d" % wpytkSequence
  wpytkNamesToPhys[name] = tup
  t = ("HandleTimer", name)
  if delay <= 0:
    return WpyTkCall("after", "idle", t)
  else:
    return WpyTkCall("after", delay, t)

def SetTitle(self, title):	# Set document title.
  for frame in self.wpyChildList:
    WpyTkCall("wm", "title", frame.wpytkTopName, title)

def SetWindowPos(self, flag):
  if self.wpyFlags & wpy.wpyFlagsWINDOW:
    if self.wpySizeX > 0 and self.wpySizeY > 0:
      #WpyTkCall(self.wpytkName, "configure",
      #   "-width", self.wpySizeX, "-height", self.wpySizeY)
      t = "%dx%d" % (self.wpySizeX, self.wpySizeY)
      WpyTkCall("wm", "geometry", self.wpytkTopName, t)
  else:
    if self.wpyVisible:
      canvas = self.wpyParent.wpytkName
      if flag & SWP_NOSIZE:
        WpyTkCall(canvas, "coords", self.wpytkCanvasId,
           int(self.wpyLocX), int(self.wpyLocY))
      elif flag & SWP_NOMOVE:
        WpyTkCall(canvas, "itemconfigure", self.wpytkCanvasId,
           "-width", int(self.wpySizeX), "-height", int(self.wpySizeY))
      else:
        WpyTkCall(canvas, "coords", self.wpytkCanvasId,
           int(self.wpyLocX), int(self.wpyLocY))
        WpyTkCall(canvas, "itemconfigure", self.wpytkCanvasId,
           "-width", int(self.wpySizeX), "-height", int(self.wpySizeY))
    if self.wpytkRectSize:
      WpytkDrawRect(self)

def SetWindowText(self, text):
  if self.wpyFlags & wpy.wpyFlagsEdit:
    if self.wpyClassType == wpy.CEdit:
      WpyTkCall(self.wpytkName, "delete", "0", "end")
      WpyTkCall(self.wpytkName, "insert", "0", self.wpyText)
    else:	# CMultiEdit
      WpyTkCall(self.wpytkName, "delete", "1.0", "end")
      WpyTkCall(self.wpytkName, "insert", "end", text)
  elif not self.wpyFlags & wpy.wpyFlagsWINDOW:
    WpyTkCall(self.wpytkName, "configure", "-text", text)

def ShowWindow(self, visible):
  canvasid = self.wpytkCanvasId
  if canvasid != None:
    if visible == wpycon.SW_HIDE:
      WpyTkCall(self.wpyParent.wpytkName, "coords", canvasid,
           "32000", "32000")
    else:
      canvas = self.wpyParent.wpytkName
      WpyTkCall(canvas, "coords", canvasid,
           int(self.wpyLocX), int(self.wpyLocY))
      WpyTkCall(canvas, "itemconfigure", canvasid,
           "-width", int(self.wpySizeX), "-height", int(self.wpySizeY))
  if self.wpytkRectSize:
    WpytkDrawRect(self)
  return self.wpyVisible

def TrackPopupMenu(self, parent):
  self.wpyParent = parent
  menu = self.wpytkMenuName = self.wpytkName = parent.wpytkName + ".popup"
  wpytkNamesToPhys[menu] = self
  t = ("PostCommand", menu)
  WpyTkCall("menu", menu, "-postcommand", t, "-tearoff", "0")
  for obj in self.wpyChildList:
    WpytkMenuCreate(obj)
  x = WpyTkCall("winfo", "rootx", parent.wpytkName)
  y = WpyTkCall("winfo", "rooty", parent.wpytkName)
  x = string.atoi(x)
  y = string.atoi(y)
  x = x + self.wpyLocX	# BUG
  y = y + self.wpyLocY
  WpyTkCall("tk_popup", menu, int(x), int(y))
  WpyTkCall("tkwait", "window", menu)
  return 1

##################################################
## Start of special Tk support functions

def WpytkCreateFrame(templ, doc, frame, view):
  global wpytkSequence
  global wpytkNamesToPhys
  frame.wpytkOrigSizeX = view.wpytkOrigSizeX = 0
  frame.wpytkOrigSizeY = view.wpytkOrigSizeY = 0
  if wpytkNamesToPhys.has_key("."):
    wpytkSequence = wpytkSequence + 1
    topname = ".obj%d" % wpytkSequence
    WpyTkCall("toplevel", topname)
    framename = topname + ".cframe"
    canvas = framename + ".canvas"
  else:
    topname = "."
    framename = ".cframe"
    canvas = framename + ".canvas"
  view.wpytkName = canvas
  frame.wpytkName = framename
  frame.wpytkTopName = topname
  wpytkNamesToPhys[framename] = frame
  wpytkNamesToPhys[topname] = frame
  wpytkNamesToPhys[canvas] = view
  WpyTkCall("wm", "withdraw", topname)
  t = ("HandleClose", topname)
  WpyTkCall("wm", "protocol", topname, "WM_DELETE_WINDOW", t)
  if frame.wpyClassType == wpy.CMDIChildWnd:
    WpyTkCall("wm", "group", topname, ".")
  frame.wpyFrameSizeX = frame.wpyFrameSizeY = 0		# Not used.
  frame.wpytkTopSizeX = -1
  WpyTkCall("frame", framename, "-bd", wpytkCanvasBd,
         "-relief", wpytkCanvasRelief)
  if view.wpyClassType == wpy.CEditView:
    WpyTkCall("text", canvas, "-bd", "0", "-wrap", "none")
  else:
    WpyTkCall("canvas", canvas, "-bd", "0")
  if hasattr(view, "wpyBrush"):
    color = "#%2.2x%2.2x%2.2x" % view.wpyBrush.wpyColor
    WpyTkCall(canvas, "configure", "-bg", color)
  #if frame.wpyHasResize:
  #  WpyTkCall("wm", "minsize", frame.wpytkTopName, 1, 1)
  frame.OnCreate(None)
  WpyTkCall("wm", "title", frame.wpytkTopName, templ.wpyText)
  menu = templ.wpyMenu
  frame.wpyMenu = menu
  if menu != None:
    menu.wpytkCreated = 1
    menuframe = frame.wpytkTopName
    if menuframe == ".":
      menuframe = ".menu"
    else:
      menuframe = menuframe + ".menu"
    WpyTkCall("frame", menuframe, "-relief", wpytkCanvasRelief,
           "-bd", wpytkCanvasBd)
    menu.wpytkMenuName = menuframe
    menu.wpytkName = menuframe
    for item in menu.wpyChildList:
      buttonname = WpytkNewName(item, None)
      menuname = item.wpytkMenuName = buttonname + ".menu%d" % wpytkSequence
      if item.wpyEnabled and item.wpyChildList:
        state = "normal"
      else:
        state = "disabled"
      WpyTkCall("menubutton", buttonname, "-text", item.wpyText,
           "-menu", menuname, "-state", state)
      wpytkNamesToPhys[menuname] = item
      t = ("PostCommand", menuname)
      WpyTkCall("menu", menuname, "-postcommand", t, "-tearoff", "0")
      if item.wpyRightSide:
        WpyTkCall("pack", buttonname, "-side", "right")
      else:
        WpyTkCall("pack", buttonname, "-side", "left")
      for obj in item.wpyChildList:
        WpytkMenuCreate(obj)
  status = frame.wpyStatusLine
  if status != None:
    name = frame.wpytkTopName
    if name == ".":
      name = ".status"
    else:
      name = name + ".status"
    status.wpytkName = name
    WpyTkCall("frame", name, "-relief", wpytkCanvasRelief,
           "-bd", wpytkCanvasBd)
    pane0 = name + ".pane0"
    WpyTkCall("label", pane0, "-text", status.wpyText, "-anchor", "w")
    WpyTkCall("pack", pane0, "-in", name,
          "-side", "left", "-fill", "x")
  view.wpytkHScrollName = None
  view.wpytkVScrollName = None
  view.OnCreate(None)
  if view.wpyVScrollSize or view.wpyHScrollSize:
    if view.wpyClassType != wpy.CEditView:
      t = "0 0 %d %d" % (view.wpyHScrollSize, view.wpyVScrollSize)
      WpyTkCall(canvas, "configure", "-scrollregion", t)
  # Create horizontal scroll bar
  name = frame.wpytkTopName
  if name == '.':
    framename = ".scrframe"
  else:
    framename = name + ".scrframe"
  name = framename + ".hscroll"
  box =  framename + ".scrbox"
  view.wpytkHScrollName = framename
  WpyTkCall("frame", framename, "-bd", "0")
  t = ("HandleHScroll", view.wpytkName)
  #t = (view.wpytkName, "xview")
  WpyTkCall("scrollbar", name, "-command", t, "-orient", "horizontal")
  t = (name, "set")
  WpyTkCall(view.wpytkName, "configure", "-xscrollcommand", t)
  h = string.atoi(WpyTkCall("winfo", "reqheight", name))
  WpyTkCall("frame", box, "-width", h, "-height", h)
  WpyTkCall("pack", box, "-in", framename, "-side", "right")
  WpyTkCall("pack", name, "-in", framename,
       "-side", "left", "-expand", "1", "-fill", "x")
  # Create vertical scroll bar
  name = frame.wpytkTopName
  if name == '.':
    name = ".vscroll"
  else:
    name = name + ".vscroll"
  view.wpytkVScrollName = name
  t = ("HandleVScroll", view.wpytkName)
  #t = (view.wpytkName, "yview")
  WpyTkCall("scrollbar", name, "-command", t)
  t = (name, "set")
  WpyTkCall(view.wpytkName, "configure", "-yscrollcommand", t)
  #
  if view.wpySizeX > 0 and view.wpySizeY > 0:
    if view.wpyClassType == wpy.CEditView:
      doc.SetModifiedFlag()	# Tk edit view can't tell if it is modified.
      w = view.wpySizeX / view.wpyCharSizeX
      h = view.wpySizeY / view.wpyCharSizeY
      WpyTkCall(view.wpytkName, "configure",
         "-width", w, "-height", h)
    else:
      WpyTkCall(view.wpytkName, "configure",
         "-width", view.wpySizeX, "-height", view.wpySizeY)
  RecalcLayout(frame, 0, view)
  if frame.wpyVisible:
    WpyTkCall("wm", "deiconify", frame.wpytkTopName)
  else:
    WpyTkCall("wm", "withdraw", frame.wpytkTopName)
  t = ("HandleConfigure", "%W", "%x", "%y", "%w", "%h", "%E")
  WpyTkCall("bind", frame.wpytkTopName, "<Configure>", t)

def WpytkDrawRect(self):
  canvas = self.wpyParent.wpytkName
  if self.wpytkMyRect != None:
    WpyTkCall(canvas, "delete", self.wpytkMyRect)
    self.wpytkMyRect = None
  if self.wpyVisible:
    i1 = (self.wpytkRectSize + 1) / 2
    self.wpytkMyRect = WpyTkCall(canvas, "create", "rectangle",
          self.wpyLocX - i1, self.wpyLocY - i1,
          self.wpyLocX + self.wpySizeX + i1,
          self.wpyLocY + self.wpySizeY + i1,
          "-width", self.wpytkRectSize)

def WpytkListBoxSizer(self):
  self.wpyScrollWidth = self.wpyCharSizeX * 3 / 2
  w = h = 0
  for i in self.wpyListBoxItems:
    l = len(i)
    if w < l: w = l
    h = h + 1
  if h == 0:
    w = 20
    h = 10
  w = w + 2
  WpyTkCall("listbox", ".listbox", "-width", w, "-height", h)
  self.wpySizeX  = string.atoi(WpyTkCall( "winfo", "reqwidth", ".listbox"))
  self.wpySizeY  = string.atoi(WpyTkCall( "winfo", "reqheight", ".listbox"))
  self.wpyLineHeight = self.wpySizeY / h
  WpyTkCall("destroy", ".listbox")

def WpyMenuItemDestroy(self, index):
  WpyTkCall(self.wpyParent.wpytkMenuName, "delete", index)

def WpytkMenuCreate(self, index = None, doAll = 1):
  flags = self.wpyphysMenuFlags
  self.wpyphysTkMenuFlags = flags
  if flags & wpycon.MF_GRAYED:
    state = "disabled"
  else:
    state = "normal"
  WpytkNewName(self, None)
  pmenu = self.wpyParent.wpytkMenuName
  if   flags & wpycon.MF_POPUP:
    menu = self.wpytkMenuName = pmenu + ".menu%d" % wpytkSequence
    if index == None:
      WpyTkCall(pmenu, "add", "cascade", "-state", state,
        "-label", self.wpyText, "-menu", menu)
    else:
      WpyTkCall(pmenu, "insert", index, "cascade", "-state", state,
        "-label", self.wpyText, "-menu", menu)
    wpytkNamesToPhys[menu] = self
    t = ("PostCommand", menu)
    WpyTkCall("menu", menu, "-postcommand", t, "-tearoff", "0")
  elif flags & wpycon.MF_SEPARATOR:
    if index == None:
      WpyTkCall(pmenu, "add", "separator")
    else:
      WpyTkCall(pmenu, "insert", index, "separator")
  elif flags & wpycon.WPY_MF_CHECKBOX:
    try:
      var = self.wpyTkIvar
    except AttributeError:
      var = "gvar%d" % wpytkSequence
      self.wpyTkIvar = var
    t = ("HandleMenu", self.wpytkName)
    if index == None:
      WpyTkCall(pmenu, "add", "checkbutton", "-state", state,
        "-label", self.wpyText, "-variable", var, "-command", t)
    else:
      WpyTkCall(pmenu, "insert", index, "checkbutton", "-state", state,
        "-label", self.wpyText, "-variable", var, "-command", t)
    if self.wpyCheckValue:
      WpyTk.globalsetvar(var, "1")
    else:
      WpyTk.globalsetvar(var, "0")
  elif flags & wpycon.WPY_MF_RADIOBUTTON:
    try:
      var = self.wpyGroup.wpyTkVarName
    except AttributeError:
      var = "gvar%d" % wpytkSequence
      self.wpyGroup.wpyTkVarName = var
    self.wpyTkVarName = var
    try:
      value = self.wpyTkVarValue
    except AttributeError:
      value = "val%d" % wpytkSequence
      self.wpyTkVarValue = value
    if self.wpyRadioSelected == self:
      WpyTk.globalsetvar(var, value)
    t = ("HandleMenu", self.wpytkName)
    if index == None:
      WpyTkCall(pmenu, "add", "radiobutton", "-label", self.wpyText, "-state", state,
        "-variable", var, "-value", value, "-command", t)
    else:
      WpyTkCall(pmenu, "insert", index, "radiobutton", "-label", self.wpyText, "-state", state,
        "-variable", var, "-value", value, "-command", t)
  else:
    t = ("HandleMenu", self.wpytkName)
    if index == None:
      WpyTkCall(pmenu, "add", "command", "-state", state,
        "-label", self.wpyText, "-command", t)
    else:
      WpyTkCall(pmenu, "insert", index, "command", "-state", state,
        "-label", self.wpyText, "-command", t)
  if doAll:
    for obj in self.wpyChildList:
      WpytkMenuCreate(obj)

def WpytkMessageSizer(self):
  WpyTkCall("message", ".m10", "-text", self.wpyText, \
        "-aspect", int(100 * self.wpyAspect + 50))
  self.wpySizeX  = string.atoi(WpyTkCall( "winfo", "reqwidth", ".m10"))
  self.wpySizeY  = string.atoi(WpyTkCall( "winfo", "reqheight", ".m10"))
  WpyTkCall("destroy", ".m10")

def WpytkNewName(self, name = None, dict = 1):
  global wpytkSequence
  global wpytkNamesToPhys
  wpytkSequence = wpytkSequence + 1
  if name == None:
    name = self.wpyParent.wpytkName	# parent name
  if name == "app":
    name = ".obj%d" % wpytkSequence
  elif name == ".":
    name = ".obj%d" % wpytkSequence
  else:
    name = name + ".obj%d" % wpytkSequence
  self.wpytkName = name
  if dict:
    wpytkNamesToPhys[name] = self
  return name

#######################################################
## Start of Tk classes

_wpytkPasteBuf = ""
class TkMenuHandlersApp:
  def WpymenuHelpAbout(self, control):
    wpy.AboutWin().DoModal()

class TkMenuHandlersDoc:
  def WpymenuFileClose(self, control):
    if _WpyPhysOkToClose(self):
      self.OnCloseDocument()
  def WpymenuFileSave(self, control):
    self.OnSaveDocument(self.wpyFileName)
  def WpymenuFileSaveas(self, control):
    d = wpy.CTkFileDlg(0, self.wpyFileName).DoModal()
    if d:
      self.OnSaveDocument(d)

class TkMenuHandlersView:
  def WpymenuEditClear(self, control):
    if WpyTkCall(self.wpytkName, "tag", "ranges", "sel"):
      self.Clear()
    self.wpyDocument.SetModifiedFlag()
  def WpymenuEditCopy(self, control):
    global _wpytkPasteBuf
    if WpyTkCall(self.wpytkName, "tag", "ranges", "sel"):
      _wpytkPasteBuf = WpyTkCall(self.wpytkName, "get", "sel.first", "sel.last")
  def WpymenuEditCut(self, control):
    global _wpytkPasteBuf
    if WpyTkCall(self.wpytkName, "tag", "ranges", "sel"):
      _wpytkPasteBuf = WpyTkCall(self.wpytkName, "get", "sel.first", "sel.last")
      WpyTkCall(self.wpytkName, "delete", "sel.first", "sel.last")
    self.wpyDocument.SetModifiedFlag()
  def WpymenuEditPaste(self, control):
    if WpyTkCall(self.wpytkName, "tag", "ranges", "sel"):
      WpyTkCall(self.wpytkName, "delete", "sel.first", "sel.last")
    WpyTkCall(self.wpytkName, "insert", "insert", _wpytkPasteBuf)
    self.wpyDocument.SetModifiedFlag()
  def WpymenuEditSelectall(self, control):
    WpyTkCall(self.wpytkName, "tag", "add", "sel", "1.0", "end")
  def WpymenuFilePrint(self, control):
    name = wpy.CTkFileDlg(0, "printer.out").DoModal()
    if name:
      if control.wpytkPrinter:
        apply(WpyTkCall, (self.wpytkName, "postscript", "-file", name) + tuple(control.wpytkPrinter))
      else:
        WpyTkCall(self.wpytkName, "postscript", "-file", name)
