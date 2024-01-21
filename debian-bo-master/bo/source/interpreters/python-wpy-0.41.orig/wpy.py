"""Module wpy.py
This module is meant to be imported by applications using the portable
GUI development library.  It contains the class library and methods.
The code in this module is intended to be mostly independent of
the operating system GUI.  The modules wpy_tk.py, wpy_nt, etc. contain
those dependencies, and this module uses them."""
#
# This is the file "wpy.py".  It contains all the class definitions,
# globals, etc. for Windowing Python.  Your app should import it.
# It is Copyright (C) 1994-1995, but is freely distributable.  THERE ARE
# NO WARRANTIES AT ALL, USE AT YOUR OWN RISK.
# See the copyright notice in copyrite.jim.
# Comments to:  Jim Ahlstrom        jim@interet.com.
#
# The code in this module is intended to be mostly independent of
# the operating system GUI.  The modules wpy_tk.py, wpy_nt, etc. contain
# those dependencies, and this module uses them.
#
# All widgets have a "text".  Even if this is not needed, it can be
# used for debugging.  If a method is essentially identical to its MFC
# version, the name is identical.  All instance variables start with "wpy" and
# methods different from MFC methods start with "Wpy" to avoid
# name conflicts with your app.  The names "wpyphys" in upper/lower case
# are used by wpy_*.py and are considered private.

# Class hierarchy:
#
# The class hierarchy is identical to that of Microsoft Foundation Classes
# version 3.0, with some additions.  An abbreviated hierarchy is:
#  CObject
#    CDC
#    CMenu
#    CCmdTarget
#      CWinThread
#        CWinApp
#      CDocTemplate
#        CSingleDocTemplate
#        CMultiDocTemplate
#      CDocument
#      CWnd
#        CView
#          CScrollView
#          CEditView
#      CFrameWnd
#        CMDIFrameWnd
#        CMDIChildWnd
#      CControlBar
#        CStatusBar
#      CButton


# import types # not available in rexec
import math
import string
import sys
import os
import regex
import wpycon	# Contains constant values from MFC header files.

# This tries to figure out what type of system you have, and then import the
# right module.  The various modules contain all the code which is highly
# dependent on the native OS GUI.  (Thanks to Guido for the code).
def _WpyAvail(name):
  # (Can't use sys.builtin_module_names because of dynamic loading)
  try:
    exec "import " + name
    return 1
  except ImportError:
    return 0

wpyImplementNT = 0	# Use to test which physical implementation is used.
wpyImplementTK = 0

try:			# See if we are in a Netscape Plugin module
  import nspi
  NetscapePlugin = 1
except:
  NetscapePlugin = 0

_wpy = None	# Use "_wpy" as the module name containing OS functions.
if _WpyAvail('wpy_nt'):
  global wpyImplementNT
  global _wpy
  wpyImplementNT = 1
  import wpy_nt
  _wpy = wpy_nt
  class TkMenuHandlersApp:
    pass
  class TkMenuHandlersDoc:
    pass
  class TkMenuHandlersView:
    pass
elif _WpyAvail('tkinter'):
  global wpyImplementTK
  global _wpy
  wpyImplementTK = 1
  import wpy_tk
  _wpy = wpy_tk
  from wpy_tk import TkMenuHandlersApp
  from wpy_tk import TkMenuHandlersDoc
  from wpy_tk import TkMenuHandlersView
else:
  raise ImportError, "No wpy physical implementation found"

# Global variables
__version__ = wpyVersion = "0.41"
wpyApp = None	# Equal to the single CWinApp instance
wpyMouseCaptureWindow = None
wpyDoModalResult = None
wpyIdleList = []

# Errors
#wpyBadChildWindow = "Child window must have another window as parent"
#wpyNoApp = "Must call CWinApp first.  TopLevel is child of app."
TemplateFrameError = "Wrong type of frame for this template"
OnlyOneTemplate = "Only one template is allowed"
NoMainFrame = "You must create a main frame first"
BadEndDialog = "Please destroy a modeless dialog with Destroy()"

# Defined flag bits.
wpyFlagsEdit	= 0x0001	# Instance is an edit control
wpyFlagsDialog	= 0x0002	# Instance is a dialog window
wpyFlagsMouse	= 0x0004	# Send mouse events to this window
wpyFlagsView	= 0x0008	# Instance is a view
wpyFlagsFrame	= 0x0010	# Instance is a frame window
wpyFlagsModal	= 0x0020	# Instance is active as a modal dialog
wpyFlagsWINDOW	= wpyFlagsDialog + wpyFlagsFrame + wpyFlagsView

#############################################################

# Standard Python WPY code which is identical for
# all supported hardware/OS platforms.

# Start of geometry management functions:

class WpyGeometry:
  def WpyMakeEqualSize(self, *args):
  # Handy function to make all sizes equal to the largest.
  # Call with arguments and/or lists and/or tuples.
  # It only works one level of lists down (no list of lists).
  # The "self" is not used.
    x = y = 0
    for obj in args:
      if type(obj) == type([]) or type(obj) == type(()):
        for item in obj:
            if x < item.wpySizeX: x = item.wpySizeX
            if y < item.wpySizeY: y = item.wpySizeY
      else:
          if x < obj.wpySizeX: x = obj.wpySizeX
          if y < obj.wpySizeY: y = obj.wpySizeY
    for obj in args:
      if type(obj) == type([]) or type(obj) == type(()):
        for item in obj:
            item.wpySizeX = x
            item.wpySizeY = y
      else:
          obj.wpySizeX = x
          obj.wpySizeY = y

  def WpyMakeEqualSpaceX(self, loc1, loc2, pos, anchor, objs):
  # Call with a tuple or list of objects.  Space "objs" equally in
  # the X direction in "self" between relative positions loc1 and
  # loc2, and place the "anchor" y-position at "pos".  loc1 and loc2
  # and pos must be between 0 and 1.
    tot = 0
    for ob in objs:
      tot = tot + ob.wpySizeX + ob.wpyFrameSizeX
    refsize = (loc2 - loc1) * self.wpySizeX
    offset = max(0.0, (refsize - tot) / float(len(objs) + 1))
    x = loc1 * self.wpySizeX + offset
    pos = pos * self.wpySizeY
    for ob in objs:
      obsizey = ob.wpySizeY + ob.wpyFrameSizeY
      ch = anchor[0]
      if ch == "s":
        y = pos - obsizey
      elif ch != "n":
        y = pos - obsizey / 2
      else:
        y = pos
      ob.wpyLocX = int(x)
      ob.wpyLocY = int(y)
      ob.MoveWindow()
      x = x + ob.wpySizeX + offset

  def WpyPlace(self, ref, locx, locy, anchor = "nw"):
  # Place self at locx,locy relative to reference window ref.
  # If ref is None, then locx,locy is in pixels.  Otherwise, locx
  # equal to 0/1.000 means the left/right edge of ref.  The
  # "anchor" means the point of self to place at the indicated position.
  # All sizes and locations refer to the client area of the window.
    if ref == None:
      mylocx = locx
      mylocy = locy
    elif ref.wpyFlags & wpyFlagsWINDOW:	# Is this a window?
      mylocx  = ref.wpySizeX * locx
      mylocy  = ref.wpySizeY * locy
    else:			# Not a window; transform coords.
      mylocx  = ref.wpyLocX +\
                 ref.wpySizeX * locx
      mylocy  = ref.wpyLocY +\
                 ref.wpySizeY * locy
    selfsizex = self.wpySizeX + self.wpyFrameSizeX
    selfsizey = self.wpySizeY + self.wpyFrameSizeY
    ch = anchor[0]
    if ch == "s":
      mylocy = mylocy - selfsizey
    elif ch != "n":
      mylocy = mylocy - selfsizey / 2
    ch = anchor[-1]
    if ch == "e":
      mylocx = mylocx - selfsizex
    elif ch != "w":
      mylocx = mylocx - selfsizex / 2
    self.wpyLocX = int(mylocx)
    self.wpyLocY = int(mylocy)
    self.MoveWindow()

  def WpyGlue(self, ref, locx, locy, anchor, szx, szy):
  # Set the size of window "self" to szx,szy if the sizes are not None.
  # Then glue the point "anchor" of window "self" to the window "ref"
  # at the location locx,locy in ref.
  # All sizes and locations refer to the OUTSIDE of the windows.
    refsizex = ref.wpySizeX + ref.wpyFrameSizeX
    refsizey = ref.wpySizeY + ref.wpyFrameSizeY
    if ref == None:
      mylocx = locx
      mylocy = locy
      if szx != None:
        self.wpySizeX = int(szx - self.wpyFrameSizeX)
      if szy != None:
        self.wpySizeY = int(szy - self.wpyFrameSizeY)
    elif ref.wpyFlags & wpyFlagsWINDOW:	# Is this a window?
      mylocx  = refsizex * locx
      mylocy  = refsizey * locy
      if szx != None:
        self.wpySizeX = int(refsizex * szx - self.wpyFrameSizeX)
      if szy != None:
        self.wpySizeY = int(refsizey * szy - self.wpyFrameSizeY)
    else:			# Not a window; transform coords.
      mylocx  = ref.wpyLocX + refsizex * locx
      mylocy  = ref.wpyLocY + refsizey * locy
      if szx != None:
        self.wpySizeX = int(refsizex * szx - self.wpyFrameSizeX)
      if szy != None:
        self.wpySizeY = int(refsizey * szy - self.wpyFrameSizeY)
    selfsizex = self.wpySizeX + self.wpyFrameSizeX
    selfsizey = self.wpySizeY + self.wpyFrameSizeY
    ch = anchor[0]
    if ch == "s":
      mylocy = mylocy - selfsizey
    elif ch != "n":
      mylocy = mylocy - selfsizey / 2
    ch = anchor[-1]
    if ch == "e":
      mylocx = mylocx - selfsizex
    elif ch != "w":
      mylocx = mylocx - selfsizex / 2
    self.wpyLocX = int(mylocx)
    self.wpyLocY = int(mylocy)
    self.MoveWindow()

  def WpyShrinkWrap(self):
  # Size an object to just fit all its children, and the point (0,0).
  # Assumes that all children fit in their parents.
    x1 = y1 = x2 = y2 = 0
    for object in self.wpyChildList:
      x = object.wpyLocX
      y = object.wpyLocY
      if x1 > x: x1 = x
      if y1 > y: y1 = y
      x = x + object.wpySizeX # + object.wpyFrameSizeX
      y = y + object.wpySizeY # + object.wpyFrameSizeY
      if x2 < x: x2 = x
      if y2 < y: y2 = y
    self.wpySizeX = x2 - x1
    self.wpySizeY = y2 - y1
    self.MoveWindow()

# End of geometry management functions.
#############################################################

# Global functions:

def AfxMessageBox(text, style = 0):
  # Display text in a modal dialog box.
  return _wpy.AfxMessageBox(text, style)

# Mouse capture functions:
def GetCapture():
  return wpyMouseCaptureWindow	# return window with capture
def ReleaseCapture():
  global wpyMouseCaptureWindow
  _wpy.ReleaseCapture()	# return BOOL success
  wpyMouseCaptureWindow = None
  return 1

# Timer functions.  Delay can be zero to call when idle.
def SetTimer(tup, delay = 0):	# Set timer to call "apply(tup)" after delay
  if delay <= 0 and wpyImplementNT:
    wpyIdleList.append(tup)
    return tup
  else:
    return _wpy.SetTimer(tup, delay)	# return timer id
def KillTimer(id):	# Kill timer identified with "id"
  if type(id) == type (()): #types.TupleType:
    try:
      wpyIdleList.remove(id)
    except:
      pass
  else:
    _wpy.KillTimer(id)

def WpyTreeDestroy(self):
  # Destroy a tree of objects based on wpyChildList[] and wpyParent.
  for obj in self.wpyChildList:
    WpyTreeDestroy(obj)
  self.wpyParent = None
  self.wpyChildList = None

############  Start of MFC Class hierarchy   ########

class CObject:
  wpyApp = None
  wpyScreenSizeX = 0
  wpyScreenSizeY = 0
  wpyCharSizeX = 9
  wpyCharSizeY = 14
  wpyClassType = None
  wpyOneMeter = 0
  wpyOnePoint = 0
  wpyMenu = None
  wpySizeX = wpySizeY = 0
  wpyLocX  = wpyLocY = 0
  wpyFrameSizeX = wpyFrameSizeY = 0
  wpyFlags = 0
  wpyHandler = None
  wpyCreated = 0
  wpytkCreated = 0
  wpyAnchor = "nw"
  wpyVisible = 1
  wpyEnabled = 1
  wpytkRectSize = 0
  def WpyCommandRouter(self):
    handler = self.wpyHandler
    parent = self.wpyParent
    if handler == None:
      pass
    elif parent and parent.wpyFlags & wpyFlagsDialog:
      try:
        exec "func = parent." + handler
      except AttributeError:
        try:
          exec "func = parent.wpyParent." + handler
        except (AttributeError, TypeError):
          try:
            exec "func = self.wpyApp." + handler
          except AttributeError:
            return
      func(self)
    #elif parent.wpyFlags & wpyFlagsView:
    #  frame, doc, view = GetActiveFrameDV()
    #  try:
    #    exec "view." + handler +"(self)"
    #  except AttributeError:
    #    try:
    #      exec "doc." + handler +"(self)"
    #    except AttributeError:
    #      pass
    else:
      frame, doc, view = wpyApp.GetActiveFrameDV()
      try:
        exec "func = view." + handler
      except AttributeError:
        try:
          exec "func = doc." + handler
        except AttributeError:
          try:
            exec "func = frame." + handler
          except AttributeError:
            try:
              exec "func = self.wpyApp." + handler
            except AttributeError:
              try:
                exec "func = self.wpyApp.wpyMainFrame." + handler
              except AttributeError:
                try:
                  exec "func = self." + handler
                except AttributeError:
                  #if self.wpyMessageID:
                  #  wpyApp.wpyMainFrame.SendMessage(wpycon.WM_COMMAND, self.wpyMessageID, 0)
                  return
      func(self)
  def PtInRect(self, x, y):
    "Return 0/1 if point (x, y) is within the object's rectangle"
    if x >= self.wpyLocX and \
       y >= self.wpyLocY and \
       x <  self.wpyLocX + self.wpySizeX and \
       y <  self.wpyLocY + self.wpySizeY:
        return 1
    return 0

class CMenu(CObject):
  wpyphysMenuFlags = 0
  wpyRightSide = 0
  def __init__(self):
    self.wpyChildList = []
    self.wpyText = "menu"
    self.wpyParent = None
    self.wpyFlags = 0
  def Create(self, flags = 0):
    if not self.wpyCreated:
      self.wpyCreated = 1
      _wpy.CMenuCreate(self, flags)
      for obj in self.wpyChildList:
        obj.WpyPrivateMakeMenu()
    return self
  def Destroy(self):	# MFC
    # Since the same menu can be used on multiple frames,
    #   we must keep a reference count.
    if self.wpyCreated > 0:
      self.wpyCreated = self.wpyCreated - 1
      if self.wpyCreated == 0:
        _wpy.DestroyMenu(self)
        # WpyTreeDestroy(self)
  def EnableMenuItem(self, enabled):	# MFC: enable the menu item
    self.wpyEnabled = enabled
    if self.wpyParent == None:	# refers to whole menu bar
      for obj in self.wpyChildList:
        obj.EnableMenuItem(enabled)
    else:
      if enabled:
        flag = wpycon.MF_ENABLED
        self.wpyphysMenuFlags = self.wpyphysMenuFlags & ~ wpycon.MF_GRAYED
      else:
        flag = wpycon.MF_GRAYED
        self.wpyphysMenuFlags = self.wpyphysMenuFlags | wpycon.MF_GRAYED
      parent = self.wpyParent
      if parent.wpyParent == None:	# Look for top-level menu item
        if parent.wpyCreated:
          _wpy.EnableMenuItem(self, flag)
  #def SetMenuText(self, text):
  #  _wpy.WpySetMenuText(self, text)
  #  self.WpyPhysDrawMenuBar()
  def TrackPopupMenu(self, parent):	# MFC put up a popup menu
    self.Create(wpycon.MF_POPUP)
    ret = _wpy.TrackPopupMenu(self, parent)
    self.Destroy()
    return ret	# BOOL success
  def WpyMakeHandlerName(self):
    h = ""
    obj = self
    charset = string.letters + string.digits
    while obj.wpyParent != None:
      # Convert to one upper case, rest lower case, all alphanumeric
      t = ""
      for ch in string.upper(obj.wpyText[0]) + string.lower(obj.wpyText[1:]):
        if ch in charset:
          t = t + ch
      h = t + h
      obj = obj.wpyParent
    self.wpyHandler = "OnMenu" + h
  def WpyPrivateMakeMenu(self, index = -1):
    if self.wpyChildList:
      #print "Create popup ", self.wpyText, " append to ", self.wpyParent.wpyText
      _wpy.CMenuCreate(self, wpycon.MF_POPUP)
      for obj in self.wpyChildList:
        obj.WpyPrivateMakeMenu()
      self.Create()
      x = self.wpyphysMenuFlags = wpycon.MF_POPUP
      _wpy.AppendMenu(self, 0, x, index)
    else:
      #print "Create item  ", self.wpyText, " append to ", self.wpyParent.wpyText
      self.Create()
      h = self.wpyHandler
      id = 0
      if h != None and h[0:7] == "Wpymenu":
        id = self.wpyMessageID
      if self.wpyParent.wpyParent == None:	# Item on the menu bar
        _wpy.AppendMenu(self, id, self.wpyphysMenuFlags | wpycon.MF_GRAYED, index)
      else:
        _wpy.AppendMenu(self, id, self.wpyphysMenuFlags, index)

class CDrawnArc(CObject):
  # Represents an arc, pie or chord drawn to a view and DC
  def __init__(self, x, y, w, h, style, start, extent, pen, brush):
    self.wpyType = "a"
    self.wpyLocX = x
    self.wpyLocY = y
    self.wpySizeX = w
    self.wpySizeY = h
    self.wpyStyle = style
    self.wpyStart = start
    self.wpyExtent = extent
    self.wpyPen = pen
    self.wpyBrush = brush
  def WpyRedraw(self, view):	# Only used by Tk, not NT
    pencolor = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    brushcolor = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    wpy_tk.WpyTkCall(view.wpytkName, "itemconfigure", self.wpytkId,
         "-outline", pencolor, "-width", self.wpyPen.wpyWidth,
         "-fill", brushcolor)
    wpy_tk.WpyTkCall(view.wpytkName, "coords", self.wpytkId,
         int(self.wpyLocX), int(self.wpyLocY),
         int(self.wpyLocX + self.wpySizeX), int(self.wpyLocY + self.wpySizeY))

class CDrawnEllipse(CObject):
  # Represents an ellipse drawn to a view and DC
  def __init__(self, x, y, w, h, pen, brush):
    self.wpyType = "e"
    self.wpyLocX = x
    self.wpyLocY = y
    self.wpySizeX = w
    self.wpySizeY = h
    self.wpyPen = pen
    self.wpyBrush = brush
  def WpyRedraw(self, view):	# Only used by Tk, not NT
    pencolor = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    brushcolor = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    wpy_tk.WpyTkCall(view.wpytkName, "itemconfigure", self.wpytkId,
         "-outline", pencolor, "-width", self.wpyPen.wpyWidth,
         "-fill", brushcolor)
    wpy_tk.WpyTkCall(view.wpytkName, "coords", self.wpytkId,
         int(self.wpyLocX), int(self.wpyLocY),
         int(self.wpyLocX + self.wpySizeX), int(self.wpyLocY + self.wpySizeY))

class CDrawnLine(CObject):
  # Represents a line drawn to a view and DC
  def __init__(self, x1, y1, x2, y2, pen):
    self.wpyType = "l"
    self.wpyPen = pen
    self.wpyStartX = x1
    self.wpyStartY = y1
    self.wpyEndX = x2
    self.wpyEndY = y2
    self.WpySetBoundingBox()
  def WpySetBoundingBox(self):
    x1 = self.wpyStartX
    y1 = self.wpyStartY
    x2 = self.wpyEndX
    y2 = self.wpyEndY
    if x2 < x1:
      self.wpyLocX = x2
      self.wpySizeX = x1 - x2 + 1
    else:
      self.wpyLocX = x1
      self.wpySizeX = x2 - x1 + 1
    if y2 < y1:
      self.wpyLocY = y2
      self.wpySizeY = y1 - y2 + 1
    else:
      self.wpyLocY = y1
      self.wpySizeY = y2 - y1 + 1
  def WpyRedraw(self, view):	# Only used by Tk, not NT
    self.WpySetBoundingBox()
    color = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    wpy_tk.WpyTkCall(view.wpytkName, "itemconfigure", self.wpytkId,
         "-width", self.wpyPen.wpyWidth, "-fill", color)
    wpy_tk.WpyTkCall(view.wpytkName, "coords", self.wpytkId,
         int(self.wpyStartX), int(self.wpyStartY),
         int(self.wpyEndX), int(self.wpyEndY))

class CDrawnPolygon(CObject):
  # Represents a closed polygon drawn to a view and DC
  def __init__(self, points, pen, brush):
    self.wpyType = "p"
    x1 = x2 = points[0][0]
    y1 = y2 = points[0][1]
    for p in points:
      x = p[0]
      y = p[1]
      x1 = min(x1, x)
      x2 = max(x2, x)
      y1 = min(y1, y)
      y2 = max(y2, y)
    self.wpyLocX = x1
    self.wpyLocY = y1
    self.wpySizeX = x2 - x1
    self.wpySizeY = y2 - y1
    self.wpyPoints = points
    self.wpyPen = pen
    self.wpyBrush = brush
  def WpyRedraw(self, view):	# Only used by Tk, not NT
    pencolor = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    brushcolor = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    wpy_tk.WpyTkCall(view.wpytkName, "itemconfigure", self.wpytkId,
         "-outline", pencolor, "-width", self.wpyPen.wpyWidth,
         "-fill", brushcolor)

class CDrawnRectangle(CObject):
  "Represents a rectangle drawn to a view and DC"
  def __init__(self, x, y, w, h, pen, brush):
    self.wpyType = "r"
    self.wpyLocX = x
    self.wpyLocY = y
    self.wpySizeX = w
    self.wpySizeY = h
    self.wpyPen = pen
    self.wpyBrush = brush
  def WpyRedraw(self, view):	# Only used by Tk, not NT
    pencolor = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    brushcolor = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    wpy_tk.WpyTkCall(view.wpytkName, "itemconfigure", self.wpytkId,
         "-outline", pencolor, "-width", self.wpyPen.wpyWidth,
         "-fill", brushcolor)
    wpy_tk.WpyTkCall(view.wpytkName, "coords", self.wpytkId,
         int(self.wpyLocX), int(self.wpyLocY),
         int(self.wpyLocX + self.wpySizeX), int(self.wpyLocY + self.wpySizeY))

class CDrawnText(CObject):
  "Represents text drawn to a view and DC"
  def __init__(self, x, y, w, h, text, font, color):
    self.wpyType = "t"
    self.wpyLocX = x
    self.wpyLocY = y
    self.wpySizeX = w
    self.wpySizeY = h
    self.wpyText = text
    self.wpyFont = font
    self.wpyTextColor = color
  def GetIndexXY(self, view, x, y):
    # Return the text index closest to the view point (x, y).
    # NOTE:  Only works for a single text line, not multi-line formatted text.
    return _wpy.GetIndexXY(self, view, x, y)
  def WpyRedraw(self, view):	# Only used by Tk, not NT
    color = "#%2.2x%2.2x%2.2x" % self.wpyTextColor
    wpy_tk.WpyTkCall(view.wpytkName, "itemconfigure", self.wpytkId,
      "-text", self.wpyText, "-font", self.wpyFont.wpytkFontName,
      "-fill", color)
    wpy_tk.WpyTkCall(view.wpytkName, "coords", self.wpytkId,
         int(self.wpyLocX), int(self.wpyLocY))

class CDrawnImage(CObject):
  # Represents an image drawn to a view and DC.
  def __init__(self, x, y, image):
    self.wpyType = "i"
    self.wpyLocX = x
    self.wpyLocY = y
    self.wpySizeX = image.wpySizeX
    self.wpySizeY = image.wpySizeY
    self.wpyImage = image
  def WpyRedraw(self, view):	# Only used by Tk, not NT
    wpy_tk.WpyTkCall(view.wpytkName, "coords", self.wpytkId,
         int(self.wpyLocX), int(self.wpyLocY))

class CImage(CObject):
# Represents a bitmap or photo object
  def __init__(self, filename = None):
    self.wpySizeX = 0
    self.wpySizeY = 0
    self.wpyFileName = filename
  def __del__(self):
    if wpyImplementNT:
      _wpy.DestroyObject(self)
  def Create(self):
    self.wpySizeX, self.wpySizeY = _wpy.CImageCreate(self)
    return self

class WpyCommonCDC(CObject):
  def Chord(self, x, y, w, h, start, extent):
    return self.Arc(x, y, w, h, start, extent, "chord")
  def Circle(self, x, y, radius):	# Draw a circle
    w = radius * 2 + 1
    return self.Ellipse(x - radius, y - radius, w, w)
  def Pie(self, x, y, w, h, start, extent):
    return self.Arc(x, y, w, h, start, extent, "pieslice")
  def SelectObject(self, obj):		# MFC: Select object
    if obj != None:
      if wpyImplementNT:
        _wpy.SelectObject(self, obj)
      cl = obj.wpyClassType
      if cl == CBrush:
        ret = self.wpyBrush
        self.wpyBrush = obj
        return ret		# return previous object
      elif cl == CFont:
        ret = self.wpyFont
        self.wpyFont = obj
        return ret		# return previous object
      elif cl == CPen:
        ret = self.wpyPen
        self.wpyPen = obj
        return ret		# return previous object
  def SetTextColor(self, rgb = (0, 0, 0)):
    if wpyImplementNT:
      _wpy.SetTextColor(self, rgb[0], rgb[1], rgb[2])
    ret = self.wpyTextColor
    self.wpyTextColor = rgb
    return ret

class CDC_Tk(WpyCommonCDC):
# Device Context for Tk.  Windows has its own version.
  def __init__(self, view):
    self.wpyParent = view
    self.wpyPen = CPen().Create()
    self.wpyBrush = CBrush().Create()
    self.wpyFont = CFont().Create()
    self.wpyTextColor = (0, 0, 0)
  def Arc(self, x, y, w, h, start, extent, style = "arc"):
    o = CDrawnArc(x, y, w, h, style, start, extent, self.wpyPen, self.wpyBrush)
    pencolor = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    brushcolor = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    id = wpy_tk.WpyTkCall(self.wpyParent.wpytkName, "create", "arc",
         x, y, x + w, y + h, "-tags", "dc",
         "-style", style, "-start", start, "-extent", extent,
         "-outline", pencolor, "-width", self.wpyPen.wpyWidth,
         "-fill", brushcolor)
    o.wpytkId = id
    self.wpyParent.wpytkIdToItem[id] = o
    return o
  def DrawImage(self, image, x, y):
    name = self.wpyParent.wpytkName
    id = wpy_tk.WpyTkCall(name, "create", "image", x, y, "-tags", "dc",
      "-image", image.wpytkName, "-anchor", "nw")
    o = CDrawnImage(x, y, image)
    o.wpytkId = id
    self.wpyParent.wpytkIdToItem[id] = o
    return o
  def DrawText(self, text, x, y, max_width = 0, justify = "left"):
    name = self.wpyParent.wpytkName
    color = "#%2.2x%2.2x%2.2x" % self.wpyTextColor
    id = wpy_tk.WpyTkCall(name, "create", "text", x, y, "-text", text, "-tags", "dc",
      "-anchor", "nw", "-justify", justify, "-width", max_width,
      "-font", self.wpyFont.wpytkFontName, "-fill", color)
    l = string.split(wpy_tk.WpyTkCall(name, "bbox", id))
    x1 = string.atoi(l[0])
    y1 = string.atoi(l[1])
    x2 = string.atoi(l[2])
    y2 = string.atoi(l[3])
    o = CDrawnText(x, y, x2 - x1, y2 - y1, text, self.wpyFont, self.wpyTextColor)
    o.wpytkId = id
    self.wpyParent.wpytkIdToItem[id] = o
    return o
  def Ellipse(self, x, y, w, h):	# MFC: Draw an ellipse
    o = CDrawnEllipse(x, y, w, h, self.wpyPen, self.wpyBrush)
    pencolor = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    brushcolor = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    id = wpy_tk.WpyTkCall(self.wpyParent.wpytkName, "create", "oval",
         x, y, x + w, y + h, "-tags", "dc",
         "-outline", pencolor, "-width", self.wpyPen.wpyWidth,
         "-fill", brushcolor)
    o.wpytkId = id
    self.wpyParent.wpytkIdToItem[id] = o
    return o
  def GetTextExtent(self, text):	# Like MFC, but can be called for a window too
    name = self.wpyParent.wpytkName
    id = wpy_tk.WpyTkCall(name, "create", "text", 0, 0, "-text", text,
      "-anchor", "nw", "-justify", "left", "-width", 0,
      "-font", self.wpyFont.wpytkFontName)
    l = string.split(wpy_tk.WpyTkCall(name, "bbox", id))
    wpy_tk.WpyTkCall(name, "delete", id)
    x1 = string.atoi(l[0])
    y1 = string.atoi(l[1])
    x2 = string.atoi(l[2])
    y2 = string.atoi(l[3])
    return (x2 - x1, y2 - y1)
  def LineTo(self, x, y):
    o = CDrawnLine(self.wpyX, self.wpyY, x, y, self.wpyPen)
    color = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    id = wpy_tk.WpyTkCall(self.wpyParent.wpytkName, "create", "line", self.wpyX, self.wpyY,
         x, y, "-tags", "dc", "-width", self.wpyPen.wpyWidth, "-fill", color)
    o.wpytkId = id
    self.wpyParent.wpytkIdToItem[id] = o
    self.wpyX = x
    self.wpyY = y
    return o
  def MoveTo(self, x, y):
    self.wpyX = x
    self.wpyY = y
  def Polygon(self, points):
    o = CDrawnPolygon(points, self.wpyPen, self.wpyBrush)
    pencolor = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    brushcolor = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    args = (self.wpyParent.wpytkName, "create", "polygon")
    for p in points:
      args = args + (`int(p[0])`, `int(p[1])`)
    args = args + ("-tags", "dc",
         "-outline", pencolor, "-width", self.wpyPen.wpyWidth,
         "-fill", brushcolor)
    #id = wpy_tk.WpyTkCall(self.wpyParent.wpytkName, "create", "polygon",
    #     l, "-tags", "dc",
    #     "-outline", pencolor, "-width", self.wpyPen.wpyWidth,
    #     "-fill", brushcolor)
    apply(wpy_tk.WpyTkCall, args)
    o.wpytkId = id
    self.wpyParent.wpytkIdToItem[id] = o
    return o
  def Rectangle(self, x, y, w, h):	# MFC: Draw a rectangle
    o = CDrawnRectangle(x, y, w, h, self.wpyPen, self.wpyBrush)
    pencolor = "#%2.2x%2.2x%2.2x" % self.wpyPen.wpyColor
    brushcolor = "#%2.2x%2.2x%2.2x" % self.wpyBrush.wpyColor
    id = wpy_tk.WpyTkCall(self.wpyParent.wpytkName, "create", "rectangle",
         x, y, x + w, y + h, "-tags", "dc",
         "-outline", pencolor, "-width", self.wpyPen.wpyWidth,
         "-fill", brushcolor)
    o.wpytkId = id
    self.wpyParent.wpytkIdToItem[id] = o
    return o

class CDC(WpyCommonCDC):
# Device Context for Windows.  Tk has its own version.
  def __init__(self, view):
    self.wpyParent = view
    self.wpyphysDC = 0
    self.wpyBrush = CBrush().CreateStockObject(wpycon.WHITE_BRUSH)
    self.wpyFont = CFont().Create()
    self.wpyPen = CPen().CreateStockObject(wpycon.BLACK_PEN)
    self.wpyX = self.wpyY = 0
    self.wpyTextColor = (0, 0, 0)
  def Arc(self, x, y, w, h, start, extent, style = "arc"):
    o = CDrawnArc(x, y, w, h, style, start, extent, self.wpyPen, self.wpyBrush)
    _wpy.Arc(self, o)
    _wpy.WpyRegisterDraw(self.wpyParent, o)
    return o
  def DrawImage(self, image, x, y):
    o = CDrawnImage(x, y, image)
    _wpy.DrawImage(self, o)
    _wpy.WpyRegisterDraw(self.wpyParent, o)
    return o
  def DrawText(self, text, x, y, max_width = 0, justify = "left"):
    flags = wpycon.DT_NOPREFIX + wpycon.DT_EXPANDTABS + wpycon.DT_WORDBREAK
    if justify == "left":
      flags = flags + wpycon.DT_LEFT
    elif justify == "right":
      flags = flags + wpycon.DT_RIGHT
    else:
      flags = flags + wpycon.DT_CENTER
    o = CDrawnText(x, y, -1, -1, text, self.wpyFont, self.wpyTextColor)
    o.wpyDrawFlags = flags
    o.wpyMaxWidth = max_width
    (w, h) = _wpy.DrawText(self, o)	# returns the size
    o.wpySizeX = w
    o.wpySizeY = h
    _wpy.WpyRegisterDraw(self.wpyParent, o)
    return o
  def Ellipse(self, x, y, w, h):	# MFC: Draw an ellipse
    o = CDrawnEllipse(x, y, w, h, self.wpyPen, self.wpyBrush)
    _wpy.Ellipse(self, o)
    _wpy.WpyRegisterDraw(self.wpyParent, o)
    return o
  def GetTextExtent(self, text):	# Like MFC, but can be called for a window too
    return _wpy.GetTextExtent(self, text)	# text size (x,y)
  def LineTo(self, x, y):		# MFC: Draw line
    o = CDrawnLine(self.wpyX, self.wpyY, x, y, self.wpyPen)
    self.wpyX = x
    self.wpyY = y
    _wpy.LineTo(self, o)
    _wpy.WpyRegisterDraw(self.wpyParent, o)
    return o
  def MoveTo(self, x, y):		# MFC: Move to point
    self.wpyX = x
    self.wpyY = y
  def Polygon(self, points):
    o = CDrawnPolygon(points, self.wpyPen, self.wpyBrush)
    _wpy.Polygon(self, o)
    _wpy.WpyRegisterDraw(self.wpyParent, o)
    return o
  def Rectangle(self, x, y, w, h):	# MFC: Draw a rectangle
    o = CDrawnRectangle(x, y, w, h, self.wpyPen, self.wpyBrush)
    _wpy.Rectangle(self, o)
    _wpy.WpyRegisterDraw(self.wpyParent, o)
    return o

class CGdiObject(CObject):
  def __del__(self):
    if wpyImplementNT:
      _wpy.DestroyObject(self)
  def CreateStockObject(self, index):	# Only works for Windows, not Tk
    _wpy.CreateStockObject(self, index)
    return self

class CBrush(CGdiObject):
  wpyClassName = "Brush"
  def __init__(self, rgb = (255,255,255)):
    self.wpyClassType = CBrush
    self.wpyphysBrush = 0
    self.wpyColor = rgb
  def Create(self):
    if wpyImplementNT:
      _wpy.CBrushCreate(self)
    self.wpyCreated = 1
    return self

class CFont(CGdiObject):
  wpyClassName = "Font"
  def __init__(self, family = "swiss", height = 0, weight = wpycon.FW_NORMAL):
    self.wpyClassType = CFont
    self.wpyFamily = family
    self.wpyHeight = height
    self.wpyWeight = weight
  def Create(self):
    self.wpyOneMeter = self.wpyOneMeter		# Make local copy
    _wpy.CFontCreate(self, self.wpyFamily, self.wpyHeight, self.wpyWeight)
    self.wpyCreated = 1
    return self

class CPen(CGdiObject):
  wpyClassName = "Pen"
  def __init__(self, width = 1, rgb = (0,0,0), style = wpycon.PS_SOLID):
    self.wpyClassType = CPen
    self.wpyphysPen = 0
    self.wpyWidth = width
    self.wpyColor = rgb
    self.wpyStyle = style
  def Create(self):
    if wpyImplementNT:
      _wpy.CPenCreate(self)
    self.wpyCreated = 1
    return self

class CCmdTarget(CObject):
  def BeginWaitCursor(self):
    _wpy.BeginWaitCursor()
  def EndWaitCursor(self):
    _wpy.EndWaitCursor()

class CWinThread(CCmdTarget):
  pass

class CWinApp(CWinThread, TkMenuHandlersApp):
# Represents the whole application.  It has the size of the screen.
# It is not a visible object itself.  There is one and only one instance.
  def __init__(self):
    global wpyApp
    wpyApp = self
    self.wpyText  = "Main App"
    self.wpyParent = None
    self.wpyChildList = []
    self.wpyVisible = 0
    self.wpyMainFrame = None
    self.wpyFrameDocView = None
    _wpy.WpyRecordApp(self)
    self.WpyPhysAppMetrics()
    self.wpyNetscapeUrlDict = {}	# connects URL's with CNetscapeStream's
    CObject.wpyApp = self
    CObject.wpyScreenSizeX = self.wpySizeX
    CObject.wpyScreenSizeY = self.wpySizeY
    CObject.wpyCharSizeX = self.wpyCharSizeX
    CObject.wpyCharSizeY = self.wpyCharSizeY
    CObject.wpyOneMeter = self.wpyOneMeter
    CObject.wpyOnePoint = self.wpyOneMeter * 0.000352778
    self.InitInstance()
    if wpyImplementTK:
      _wpy.WpytkMainLoop()
  def AddDocTemplate(self, template):	# MFC: (void) add document template
    if len(self.wpyChildList):
      raise OnlyOneTemplate
    template.wpyParent = self
    template.wpyChildList = []
    self.wpyChildList.append(template)
    if not NetscapePlugin:	# Netscape pre-registers its classes
      _wpy.AddDocTemplate(template)
  def GetActiveFrame(self):
    frame, doc, view = self.GetActiveFrameDV()
    return frame
  def GetActiveFrameDV(self):
    if self.wpyFrameDocView:
      return self.wpyFrameDocView
    else:
      return _wpy.GetActiveFrameDV()
  def FileOpen(self):		# MFC: Open an existing document
    templ = self.wpyChildList[0]
    newmenu = None
    if templ.wpyMenuClass != None:
      if templ.wpyClassType == CMultiDocTemplate:	# Make another menu
        newmenu = templ.wpyMenu = templ.wpyMenuClass().Create()
      elif templ.wpyMenu == None:		# Make the only menu
        templ.wpyMenu = templ.wpyMenuClass().Create()
    t, doc, frame, view =  _wpy.OnFileOpen(templ)
    if self.wpyMainFrame == None:
      if templ.wpyClassType == CSingleDocTemplate:
        self.wpyMainFrame = frame
      else:
        raise NoMainFrame
    if frame == None:
      if newmenu:
        newmenu.Destroy()
    else:
      frame.wpyMenu = templ.wpyMenu
      frame.wpyText = templ.wpyText
    if wpyImplementNT:
      frame, doc, view = self.GetActiveFrameDV()
      doc.SetTitle(doc.wpyText)	# seems to be needed for MFC SDI
  def FileNew(self):		# MFC: Open a new empty document
    if NetscapePlugin:
      return
    templ = self.wpyChildList[0]
    if templ.wpyMenuClass != None:
      if templ.wpyClassType == CMultiDocTemplate:	# Make another menu
        templ.wpyMenu = templ.wpyMenuClass().Create()
      elif templ.wpyMenu == None:		# Make the only menu
        templ.wpyMenu = templ.wpyMenuClass().Create()
    t, doc, frame, view =  _wpy.OnFileNew(templ)
    if self.wpyMainFrame == None:
      if templ.wpyClassType == CSingleDocTemplate:
        self.wpyMainFrame = frame
      else:
        raise NoMainFrame
    if frame != None:
      frame.wpyMenu = templ.wpyMenu
      frame.wpyText = templ.wpyText
  def WindowNew(self):		# MFC: Open a new view on the document
    templ = self.wpyChildList[0]
    frame, doc, view = wpyApp.GetActiveFrameDV()
    if templ.wpyMenuClass != None:
      if templ.wpyClassType == CMultiDocTemplate:	# Use the menu from this document
        m = templ.wpyMenu = doc.wpyChildList[0].wpyMenu
        m.wpyCreated = m.wpyCreated + 1		# Record another use of the menu
      elif templ.wpyMenu == None:		# Make the only menu
        templ.wpyMenu = templ.wpyMenuClass().Create()
    t, x, frame, view =  _wpy.OnWindowNew(templ)
    frame.wpyMenu = templ.wpyMenu
    frame.wpyText = templ.wpyText	# BUG
  def PyRun_InteractiveLoop(self, text):
    _wpy.PyRun_InteractiveLoop(text)
  def RequestViewSize(self):	# calculate frame size for a specified view size
    frame = self.wpyParent	# BUG
    x1, y1, x2, y2 = _wpy.CalcWindowRect(frame, view)
    frame.wpySizeX = x2 - x1
    frame.wpySizeY = y2 - y1
    _wpy.SetWindowPos(frame, wpycon.SWP_NOMOVE)
    x1, y1, x2, y2 = _wpy.GetClientRect(view)
    frame.wpySizeX = frame.wpySizeX + view.wpySizeX - x2
    frame.wpySizeY = frame.wpySizeY + view.wpySizeY - y2
    _wpy.SetWindowPos(frame, wpycon.SWP_NOMOVE)
  def Exit(self, event = None):
  # Just a handy exit function in event handler format.
    self.wpyMainFrame.PostMessage(wpycon.WM_COMMAND, wpycon.ID_APP_EXIT, 0)
  def ExitInstance(self):
    pass
  def WpyPhysOnIdle(self):	# called by NT (not tk) when idle
    if wpyIdleList:
      t = wpyIdleList[0]
      del wpyIdleList[0]
      self.WpyphysTimerProc(t[0], t[1])
      if wpyIdleList:
        return 1	# need more idle-time processing
  def WpyPhysExitInstance(self):
    self.ExitInstance()
  def WpyPhysAppMetrics(self):
    # Get some text metrics.
    x, y = _wpy.GetTextExtent(None, "0000000000")
    self.wpyCharSizeX = (x + 5) / 10
    self.wpyCharSizeY = y
    # Get screen size, WpyApp has the size of the screen.
    self.wpyFrameSizeX = self.wpyFrameSizeY = 0
    self.wpySizeX = _wpy.GetSystemMetrics(wpycon.SM_CXSCREEN)
    self.wpySizeY = _wpy.GetSystemMetrics(wpycon.SM_CYSCREEN)
    self.wpyOneMeter = self.wpySizeX * 1000 / _wpy.GetDeviceCaps(None, wpycon.HORZSIZE)
    self.wpyLocX = self.wpyLocY = 0
  def WpyphysTimerProc(self, func, args, dict = {}):
    apply(func, args)	# BUG: add dict
  def WpyReturnCRect(self, x, y, w, h, flags = 0):	# Make a rectangle (used in C++)
    r = CRect()
    r.wpyLocX = x
    r.wpyLocY = y
    r.wpySizeX = w
    r.wpySizeY = h
    r.wpyFlags = flags
    return r
  # Start of Netscape plugin API
  def NPP_New(self, type, mode, args, saved):
    pass
  def WpyNPP_InitialSetWindow(self, rect, hWnd):
    templ = self.wpyChildList[0]
    doc = templ.wpyDocument(templ)
    frame = templ.wpyFrame(templ, doc)
    if frame.wpyClassType != CFrameWnd:
      raise TemplateFrameError
    self.wpyMainFrame = frame
    view = templ.wpyView(templ, doc, frame)
    view.wpyDrawable = 0
    view.wpyLocX = rect.wpyLocX
    view.wpyLocY = rect.wpyLocY
    view.wpySizeX = rect.wpySizeX
    view.wpySizeY = rect.wpySizeY
    self.wpyFrameDocView = frame, doc, view
    _wpy.WpyNetscapeFileNew(templ, doc, frame, view, hWnd)
    view.OnCreate(None)
    rect.wpyFlags = view.wpyFlags
    view.OnSize(rect)
    view.OnInitialUpdate()
    view.wpyDrawable = 1
    view.InvalidateRect()
    return view
  def NPP_SetWindow(self, rect):
    view = self.wpyFrameDocView[2]
    if rect.wpySizeX != view.wpySizeX or rect.wpySizeY != view.wpySizeY:
      rect.wpyLocX = rect.wpyLocY = 0
      rect.wpyFlags = view.wpyFlags
      view.OnSize(rect)
    view.wpyLocX = rect.wpyLocX
    view.wpyLocY = rect.wpyLocY
    view.wpySizeX = rect.wpySizeX
    view.wpySizeY = rect.wpySizeY
  def NPP_Destroy(self):
    pass  # Return a string to save
  def WpyNPP_Destroy(self):
    ret = self.NPP_Destroy()
    self.ExitInstance()
    frame, doc, view = self.wpyFrameDocView
    _wpy.WpyNetscapeFileDestroy(doc, frame, view)
    return ret
  def WpyNPP_NewStream(self, url):
    stream = self.wpyNetscapeUrlDict[url]
    return stream
  def NPN_PostURL(self, url, window, buf, file):
    return nspi.NPN_PostURL(self, url, window, buf, file)
  def NPN_NewStream(self, type):
    return nspi.NPN_NewStream(self, type)
  def NPN_Write(self, buffer):
    return nspi.NPN_Write(self, buffer)
  def NPN_DestroyStream(self, reason):
    return nspi.NPN_DestroyStream(self, reason)
  def NPN_Status(self, message):
    return nspi.NPN_Status(self, message)

class CNetscapeStream(CObject):
  def NPN_GetURL(self, url, window = ""):
    self.wpyURL = url
    self.wpyApp.wpyNetscapeUrlDict[url] = self
    return nspi.NPN_GetURL(self.wpyApp, url, window)
  def NPP_NewStream(self, type, seekable):
    pass	# can return wpycon.NP_ASFILE
  def NPP_WriteReady(self):
    return 30000	# number of bytes to send
  def NPP_Write(self, buffer, offset):
    pass	# can return -1 to abort
  def NPP_DestroyStream(self, reason):
    del self.wpyApp.wpyNetscapeUrlDict[self.wpyURL]
  def NPP_StreamAsFile(self, filename):
    pass

class CDocument(CCmdTarget, TkMenuHandlersDoc):
  def __init__(self, templ):
    self.wpyChildList = []
    self.wpyViewList = []
    self.wpyText = ""
    self.wpyParent = templ
    templ.wpyChildList.append(self)
    self.wpyModifiedFlag = 0
    self.wpyFileName = "untitled"
    self.wpyCreated = 1
  def DeleteContents(self):
    pass
  def OnCloseDocument(self):
    _wpy.OnCloseDocument(self)
  def OnNewDocument(self):
    ret = _wpy.OnNewDocument(self)	# BOOL success
    self.SetTitle(self.wpyParent.wpyText)
    return ret
  def OnOpenDocument(self, filename):
    f = open(filename)
    self.wpyFileName = filename
    self.DeleteContents()
    self.SetModifiedFlag()
    self.SerializeIn(f)
    f.close()
    self.SetModifiedFlag(0)
    self.SetTitle(self.wpyParent.wpyText + " - " + filename)
  def OnSaveDocument(self, filename):
    f = open(filename, "w")
    self.SerializeOut(f)
    f.close()
    self.SetModifiedFlag(0)
  def SetModifiedFlag(self, value = 1):	# MFC: set document flag to modified
    self.wpyModifiedFlag = value
    _wpy.SetModifiedFlag(self, value)
  def SetTitle(self, title):
    self.wpyText = title
    _wpy.SetTitle(self, title)
  def UpdateAllViews(self, sender, update):
    for view in self.wpyViewList:
      if view != sender:
        view.OnUpdate(sender, update)

class CWnd(CCmdTarget, WpyGeometry):
  wpytkCanvasId = None
  def DestroyWindow(self):
    _wpy.DestroyWindow(self)
  def DrawMenuBar(self):	# MFC
    _wpy.DrawMenuBar(self)
  def EnableWindow(self, enable):
    self.wpyEnabled = enable
    if enable:
      return _wpy.EnableWindow(self, 1)	# BOOL true if was disabled, 0 if
    else:					# window was enabled or error
      return _wpy.EnableWindow(self, 0)
  def GetClientRect(self):
    rect = CRect()
    rect.wpyFlags = self.wpyFlags
    rect.wpyLocX, rect.wpyLocY, rect.wpySizeX, rect.wpySizeY =\
         _wpy.GetClientRect(self)
    return rect		# a CRect giving size (location is 0,0) of client area
  def GetDC(self):	# MFC: Get the device context for this window
    if wpyImplementNT:
      _wpy.GetDC(self)
    return self.wpyDC
  def GetTextExtent(self, text):	# Like MFC, but called for a window
    return _wpy.GetTextExtent(self, text)	# text size (x,y)
  def GetWindowRect(self):
    rect = CRect()
    rect.wpyFlags = self.wpyFlags
    rect.wpyLocX, rect.wpyLocY, rect.wpySizeX, rect.wpySizeY =\
         _wpy.GetWindowRect(self)
    # returns x1, y1, x2, y2.  Convert to sizes.
    rect.wpySizeX = rect.wpySizeX - rect.wpyLocX
    rect.wpySizeY = rect.wpySizeY - rect.wpyLocY
    return rect	# a CRect giving size and location of window in screen
  def GetWindowText(self):
    return _wpy.GetWindowText(self)
  def InvalidateRect(self):
    _wpy.InvalidateRect(self, None, 1)
  def MoveWindow(self):
    _wpy.MoveWindow(self)
  def MoveWindowLoc(self):
    _wpy.SetWindowPos(self, wpycon.SWP_NOSIZE)
  def MoveWindowSize(self):
    _wpy.SetWindowPos(self, wpycon.SWP_NOMOVE)
  def OnChar(self, char, flags):
    if char == '\003':
      wpyApp.Exit()
  def OnLButtonDblClk(self, x, y, flags):
    pass
  def OnLButtonDown(self, x, y, flags):
    pass
  def OnLButtonUp(self, x, y, flags):
    pass
  def OnRButtonDblClk(self, x, y, flags):
    pass
  def OnRButtonDown(self, x, y, flags):
    pass
  def OnRButtonUp(self, x, y, flags):
    pass
  def OnListBox(self, control):
    pass
  def OnListBoxDbl(self, control):
    pass
  def OnMouseMove(self, x, y, flags):
    pass
  def PostMessage(self, msg, wparam = 0, lparam = 0):
    return _wpy.PostMessage(self, msg, wparam, lparam)
  def ReleaseDC(self, DC):	# MFC: release the device context
    if wpyImplementNT:
      _wpy.ReleaseDC(self, DC)	# BOOL success
    return 1
  def SendMessage(self, msg, wparam = 0, lparam = 0):
    return _wpy.SendMessage(self, msg, wparam, lparam)
  def SetCapture(self):			# Capture the mouse events
    global wpyMouseCaptureWindow	# Must call ReleaseCapture when done
    ret = wpyMouseCaptureWindow
    wpyMouseCaptureWindow = self
    _wpy.SetCapture(self)	# return window which had capture, or None
    return ret
  def SetFocus(self):
    return _wpy.SetFocus(self)
  def SetFont(self, font, redraw = 1):
    self.wpyFont = font	# Create a reference to prevent destruction of font.
    _wpy.SetFont(self, font, redraw)
  def SetWindowText(self, text):
    self.wpyText = text
    _wpy.SetWindowText(self, text)
  def ShowWindow(self, show):
    if show:	# return BOOL window was previously visible
      ret = _wpy.ShowWindow(self, wpycon.SW_SHOW)
    else:
      ret = _wpy.ShowWindow(self, wpycon.SW_HIDE)
    self.wpyVisible = show
    return ret
  ## Event handlers:
  def OnCreate(self, event):
    pass
  def OnDestroy(self):
    pass
  def WpyPhysOnDestroy(self):
    self.OnDestroy()
  def OnKillFocus(self, new_focus):
    pass
  def WpyPhysOnKillFocus(self, new_focus):
    self.OnKillFocus(new_focus)
  def OnHScroll(self, event):
    pass
  def OnScroll(self, control):
    pass
  def OnSetFocus(self, previous):
    pass
  def WpyPhysOnSetFocus(self, previous):
    self.OnSetFocus(previous)
  def OnSize(self, rect):
    pass
  def OnVScroll(self, event):
    pass
  def WpyPhysOnSize(self, flags, sizex, sizey):
    rect = CRect()
    rect.wpyFlags = self.wpyFlags
    rect.wpyLocX = rect.wpyLocY = 0
    rect.wpySizeX = sizex
    rect.wpySizeY = sizey
    self.OnSize(rect)
  ## Special (non-MFC) methods
  def WpyButtonSizer(self, checks = 0):
  # Create a default size for buttons.
    self.wpyLocX  = self.wpyLocY  = 0
    x, y = _wpy.GetTextExtent(None, self.wpyText)
    self.wpySizeX = x + y
    self.wpySizeY = y + y / 2
    self.wpyFrameSizeX = self.wpyFrameSizeY = 0
    if checks:		# Need extra space for checkmarks
      if wpyImplementNT:
        self.wpySizeX = self.wpySizeX + 3
      else:
        self.wpySizeX = self.wpySizeX + y
  def WpyLabelSizer(self):
  # Create a default size for edits and labels.
    self.wpyLocX  = self.wpyLocY  = 0
    x, y = _wpy.GetTextExtent(None, self.wpyText)
    self.wpySizeX = x
    self.wpySizeY = y
  def WpyMakeButtonName(self):
    # Set up a handler name equal to the alphanumeric chars in the text.
    t = ""
    charset = string.letters + string.digits
    for ch in self.wpyText:
      if ch in charset:
        t = t + ch
    self.wpyHandler = "OnButton" + t

class CRect(CWnd):
# General purpose rectangle.
  def __init__(self):
    pass

class CChildWnd(CWnd):
  def __init__(self, parent, text = ""):
    self.wpyChildList = []
    self.wpyParent = parent
    self.wpyText = text
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE | wpycon.WS_DLGFRAME
    self.wpySizeX = self.wpySizeY = 100
  def Create(self):
    _wpy.CWndCreate(self)
    return self

class CFrameWnd(CWnd):
  def __init__(self, templ, doc):
    self.wpyText = None
    self.wpyMenu = None
    self.wpyChildList = []
    self.wpyFlags = wpyFlagsFrame
    self.wpyStatusLine = None
    self.wpyParent = doc
    doc.wpyChildList.append(self)
    self.wpyCreated = 1
  def GetActiveDocument(self):	# BUG - should call direct
    frame, doc, view = wpyApp.GetActiveFrameDV()
    return doc		# the active document
  def GetActiveView(self):	# MFC get the active view
    frame, doc, view = wpyApp.GetActiveFrameDV()
    return view		# the active view
  def RecalcLayout(self, notify = 1):
    _wpy.RecalcLayout(self, notify)
  ### Event handlers
  def WpyPhysOnDestroy(self):
    self.OnDestroy()
    if self.wpyMenu:
      self.wpyMenu.Destroy()
      self.wpyMenu = None
    self.wpyParent.wpyChildList.remove(self)
CFrameWnd.wpyClassType = CFrameWnd

class CMDIFrameWnd(CFrameWnd):
  def __init__(self):
    self.wpyText = None
    self.wpyMenu = None
    self.wpyChildList = []
    self.wpyStatusLine = None
    self.wpyParent = wpyApp
    self.wpyCreated = 1
    self.wpyFlags = 0
    self.wpyWindowList = []
  def Create(self):
    self.wpyParent = None
    wpyApp.wpyMainFrame = self
    menu = self.wpyMenu
    if menu != None:
      menu.Create()
    _wpy.CMDIFrameWndCreate(self)
    self.wpyCreated = 1
    return self
  def WpyPhysOnDestroy(self):
    self.OnDestroy()
    if self.wpyMenu:
      self.wpyMenu.Destroy()
      self.wpyMenu = None
CMDIFrameWnd.wpyClassType = CMDIFrameWnd

class CMDIChildWnd(CFrameWnd):
  def __init__(self, templ, doc):
    CFrameWnd.__init__(self, templ, doc)
    main = wpyApp.wpyMainFrame
    main.wpyWindowList.insert(0, self)
    if len(main.wpyWindowList) == 1 and wpyImplementTK:
      main.RecalcLayout()
  def WpyPhysOnDestroy(self):
    self.OnDestroy()
    if self.wpyMenu:
      self.wpyMenu.Destroy()
      self.wpyMenu = None
    self.wpyParent.wpyChildList.remove(self)
    wpyApp.wpyMainFrame.wpyWindowList.remove(self)
CMDIChildWnd.wpyClassType = CMDIChildWnd

class CDocTemplate(CCmdTarget):
  pass

class CSingleDocTemplate(CDocTemplate):
  wpyClassName = "Single"
  def __init__(self, doc, frame, view, menu = None):
    if frame.wpyClassType != CFrameWnd:
      raise TemplateFrameError
    self.wpyClassType = CSingleDocTemplate
    self.wpyText = "Untitled"
    self.wpyDocument = doc
    self.wpyFrame = frame
    self.wpyView = view
    self.wpyMenuClass = menu

class CMultiDocTemplate(CDocTemplate):
  wpyClassName = "Multi"
  def __init__(self, doc, frame, view, menu = None):
    if frame.wpyClassType != CMDIChildWnd:
      raise TemplateFrameError
    self.wpyClassType = CMultiDocTemplate
    self.wpyText = "Untitled"
    self.wpyDocument = doc
    self.wpyFrame = frame
    self.wpyView = view
    self.wpyMenuClass = menu

class CControlBar(CWnd):
  pass

class CStatusBar(CControlBar):
  def __init__(self, parent, text):
    self.wpyChildList = []
    self.wpyParent = parent
    self.wpyText = text
  def Create(self):
    return self
  def SetPaneText(self, index, text, update = 1):
    if index == 0:
      _wpy.WpySetStatusText(self.wpyParent, text)	# Fix bug (?) in MFC
    return _wpy.SetPaneText(self, index, text, update)
  def SetWindowText(self, text):
    self.wpyText = text
    self.SetPaneText(0, text, 1)
    
# Start of controls:

class CPushButton(CWnd):
# A button the user can press with the mouse.  You must specify
# wpyLocX and wpyLocY yourself, but a default size is made.
# Parent must be specified, and must be a dialog or view.
  def __init__(self, parent, text):
    self.wpyClassType = CPushButton
    self.wpyChildList = []
    self.wpyText  = text
    self.wpyParent = parent
    self.wpyAnchor = "center"
    self.WpyButtonSizer()
    self.WpyMakeButtonName()
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE
  def Create(self):
    self.wpyParent.wpyChildList.append(self)
    if self.wpyParent.wpyDefaultButton == self:
      self.wpyStyle = self.wpyStyle | wpycon.BS_DEFPUSHBUTTON
    _wpy.CButtonCreate(self)
    self.wpyCreated = 1
    self.EnableWindow(self.wpyEnabled)
    self.ShowWindow(self.wpyVisible)
    return self
  def WpyPhysOnButton(self):
    # Route the button notification to various targets.
    self.WpyCommandRouter()

class CCheckButton(CWnd):
# A check button the user can press with the mouse.  You must specify
# wpyLocX and wpyLocY yourself, but a default size is made.
# The wpyCheckValue is 1/0 for selected/unselected.
  def __init__(self, parent, text):
    self.wpyClassType = CCheckButton
    self.wpyText  = text
    self.wpyParent = parent
    self.wpyChildList = []
    self.wpyCheckValue = 0
    self.wpyAnchor = "center"
    self.WpyButtonSizer(1)
    self.WpyMakeButtonName()
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE | wpycon.BS_CHECKBOX
    if hasattr(parent, "wpyBrush"):
      self.wpyBrush = parent.wpyBrush
  def Create(self):
    self.wpyParent.wpyChildList.append(self)
    _wpy.CButtonCreate(self)
    self.wpyCreated = 1
    if self.wpyCheckValue:
      _wpy.SetCheck(self, 1)
    else:
      _wpy.SetCheck(self, 0)
    self.EnableWindow(self.wpyEnabled)
    self.ShowWindow(self.wpyVisible)
    return self
  def WpyPhysOnButton(self):
    if self.wpyCheckValue:
      x = self.wpyCheckValue = 0
    else:
      x = self.wpyCheckValue = 1
    _wpy.SetCheck(self, x)
    self.WpyCommandRouter()
  def SetCheck(self, check = 1):
    if check:
      x = self.wpyCheckValue = 1
    else:
      x = self.wpyCheckValue = 0
    _wpy.SetCheck(self, x)
    
class CLabel(CWnd):
# A single line of text, not interactive.
  def __init__(self, parent, text):
    self.wpyClassType = CLabel
    self.wpyText  = text
    self.wpyParent = parent
    self.wpyChildList = []
    self.WpyLabelSizer()
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE | wpycon.SS_LEFT
    if hasattr(parent, "wpyBrush"):
      self.wpyBrush = parent.wpyBrush
  def Create(self):
    self.wpyParent.wpyChildList.append(self)
    _wpy.CStaticCreate(self)
    return self

class CMessage(CWnd):
# A multi-line text message with justification, not interactive.
  def __init__(self, parent, text, aspect = 3.2):
    self.wpyClassType = CMessage
    self.wpyText  = text
    self.wpyParent = parent
    self.wpyAspect = aspect
    self.wpyChildList = []
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE | wpycon.SS_LEFT
    if hasattr(parent, "wpyBrush"):
      self.wpyBrush = parent.wpyBrush
    if wpyImplementTK:
      _wpy.WpytkMessageSizer(self)
    else:	# BUG: newline does not restart justification
      x, y = _wpy.GetTextExtent(None, self.wpyText)
      if y:
        n = int(math.sqrt(x * 1.1 / (aspect * y)) + 0.9) # Number of lines
        self.wpySizeX = int(aspect * n * y)
        self.wpySizeY = y
      else:
        self.wpySizeX = 50
        self.wpySizeY = 10
      self.wpyFrameSizeX = self.wpyFrameSizeY = 0
      n = 1
      here = 0
      start = 0
      blank = 0
      copy = ""
      for ch in self.wpyText:
        if ch == ' ':
          x, y = _wpy.GetTextExtent(None, self.wpyText[start:here])
          if x > self.wpySizeX:
            copy = copy + self.wpyText[start:blank] + '\n'
            n = n + 1
            start = blank + 1
            blank = here + 1
          else:
            blank = here
        here = here + 1
      x, y = _wpy.GetTextExtent(None, self.wpyText[start:])
      if x > self.wpySizeX:
        copy = copy + self.wpyText[start:blank] + '\n'
        n = n + 1
        copy = copy + self.wpyText[blank + 1:] + '\n'
      else:
        copy = copy + self.wpyText[start:] + '\n'
      self.wpySizeY = self.wpySizeY * n
      self.wpyText = copy
  def Create(self):
    self.wpyParent.wpyChildList.append(self)
    _wpy.CStaticCreate(self)
    return self

class CRadioButton(CWnd):
# A radio button the user can press with the mouse.
# The first radio button has no "group" argument, and is default "ON".
# Subsequent radio buttons have a group argument of the first button.
# All such radio buttons are grouped together and have self.wpyGroup equal
# to the class instance of the first button.
# The class instance of the selected button is available
# from any button as self.wpyRadioSelected.
  def __init__(self, parent, text, group = None):
    self.wpyClassType = CRadioButton
    self.wpyText  = text
    self.wpyParent = parent
    self.wpyChildList = []
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE | wpycon.BS_RADIOBUTTON
    if hasattr(parent, "wpyBrush"):
      self.wpyBrush = parent.wpyBrush
    if group == None:
      self.wpyGroup = self
      self.wpyGroupList = [self]
      self.wpyRadioSelected = self	# this button is ON, the rest are OFF
    else:
      self.wpyGroup = group
      group.wpyGroupList.append(self)
      self.wpyRadioSelected = group
    self.wpyAnchor = "center"
    self.WpyButtonSizer(1)
    self.WpyMakeButtonName()
  def WpyRadioButton(self, text):
    return WpyRadioButton(text, self.wpyParent, self)
  def Create(self):
    self.wpyParent.wpyChildList.append(self)
    _wpy.CButtonCreate(self)
    self.wpyCreated = 1
    if self.wpyRadioSelected == self:
      _wpy.SetCheck(self, 1)
    else:
      _wpy.SetCheck(self, 0)
    self.EnableWindow(self.wpyEnabled)
    self.ShowWindow(self.wpyVisible)
    return self
  def WpyPhysOnButton(self):
    self.SetCheck()
    self.WpyCommandRouter()
  def SetCheck(self, check = 1):
    if check:
      for b in self.wpyGroup.wpyGroupList:
        b.wpyRadioSelected = self
        _wpy.SetCheck(b, 0)
      _wpy.SetCheck(self, 1)


class CScrollBar(CWnd):
# A scroll bar control.  Not used for scroll bar on window.
  def __init__(self, parent, text):
    self.wpyClassType = CScrollBar
    self.wpyText  = text	# Must start with "v" or "h"
    self.wpyParent = parent
    self.wpyChildList = []
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE
    size = self.wpyCharSizeY * 14 / 10
    if text[0] == "v":
      self.wpySizeX = size
      self.wpySizeY = 50
      self.wpyAnchor = "n"
    else:
      self.wpySizeX = 50
      self.wpySizeY = size
      self.wpyAnchor = "w"
    self.wpyScrollSize = 0	# Total size of the scroll control.
    self.wpyScrollWinSize = 1	# Size which fits in the window.
    self.wpyScrollPos = 0	# Index of the first visible line in the window.
  def Create(self):
    self.wpyParent.wpyChildList.append(self)
    if self.wpyText[0] == "v":
      self.wpyStyle = self.wpyStyle | wpycon.SBS_VERT
    else:
      self.wpyStyle = self.wpyStyle | wpycon.SBS_HORZ
    _wpy.CScrollBarCreate(self)
    self.SetScrollRange(1)
    return self
  def SetScrollPos(self, pos, redraw = 1):	# MFC
    ret = self.wpyScrollPos
    max = self.wpyScrollSize - self.wpyScrollWinSize
    if pos > max:
      pos = max
    if pos < 0:
      pos = 0
    self.wpyScrollPos = pos
    _wpy.SetScrollPosSb(self, pos, redraw)	# return old pos, or 0
    return ret
  def SetScrollRange(self, redraw = 1):	# Scroll model differs from MFC
    min = 0
    max = self.wpyScrollSize - self.wpyScrollWinSize
    _wpy.SetScrollRangeSb(self, min, max, 0)
    self.SetScrollPos(self.wpyScrollPos, redraw)
  def WpyPhysOnScroll(self, pos):
    self.wpyScrollPos = pos
    self.wpyParent.OnScroll(self)
    
class CListBox(CWnd):
# A list box control.
  def __init__(self, parent, text, items, max_sizey = None):
    self.wpyClassType = CListBox
    self.wpyParent = parent
    self.wpyText  = text
    self.wpyChildList = []
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE | wpycon.LBS_HASSTRINGS \
       | wpycon.LBS_NOTIFY | wpycon.WS_VSCROLL | wpycon.LBS_USETABSTOPS
    self.wpySelected = -1	# currently selected list box item, or -1
    self.wpyListBoxItems = items
    # return a size which fits the list box items
    if wpyImplementTK:
      _wpy.WpytkListBoxSizer(self)
    else:
      charx, self.wpyLineHeight = _wpy.GetTextExtent(None, "M")
      self.wpyScrollWidth = charx * 2
      if items:
        w = h = 0
        for i in items:
          x, y = _wpy.GetTextExtent(None, i)
          if w < x: w = x
          h = h + y
        self.wpySizeX = w + charx
        self.wpySizeY = h
      else:	# size is 10 lines, 20 chars
        self.wpySizeX = charx * 20
        self.wpySizeY = self.wpyLineHeight * 10
    if max_sizey != None:
      if max_sizey <= 0 or self.wpySizeY > max_sizey:
        self.wpySizeX = self.wpySizeX + self.wpyScrollWidth
        self.wpySizeY = ((max_sizey + 4) / self.wpyLineHeight) * self.wpyLineHeight
  def Create(self):
    self.wpyParent.wpyChildList.append(self)
    _wpy.CListBoxCreate(self)
    self.wpyCreated = 1
    for i in self.wpyListBoxItems:
      _wpy.AddString(self, i)
    self.SetCurSel(self.wpySelected)
    return self
  def WpySetItems(self, items, select = None):
    self.wpyListBoxItems = items
    _wpy.ResetContent(self)
    for i in items:
      _wpy.AddString(self, i)
    if select == None:
      self.SetCurSel(-1)
    else:
      self.SetCurSel(select)
  def SetCurSel(self, index):
    # set selected item to index; index == -1 for no selection.
    self.wpySelected = index
    _wpy.SetCurSel(self, index)
  def WpyPhysOnListBox(self, index):
    # Called by Windows when the list box selection changes.
    # "index" is the index (0, 1, ...) of the new selection, or -1
    self.wpySelected = index
    self.wpyParent.OnListBox(self)
  def WpyPhysOnListBoxDbl(self, index):
    # Called by Windows when the user double clicks an item.
    self.wpySelected = index
    self.wpyParent.OnListBoxDbl(self)
  def OnLButtonUp(self, x, y, flags):
    # Called by Tk on mouse event.
    self.wpySelected = string.atoi(wpy_tk.WpyTkCall(self.wpytkName, "curselection"))
    self.wpyParent.OnListBox(self)
  def OnLButtonDblClk(self, x, y, flags):
    # Called by Tk on mouse event.
    self.wpySelected = string.atoi(wpy_tk.WpyTkCall(self.wpytkName, "curselection"))
    self.wpyParent.OnListBoxDbl(self)

class CEdit(CWnd):
# A single-line edit control.
  def __init__(self, parent, text, nchars = None):
    self.wpyParent = parent
    self.wpyChildList = []
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE | \
          wpycon.ES_AUTOHSCROLL | wpycon.WS_TABSTOP
    self.wpyFlags = wpyFlagsEdit
    self.wpyPasswordChar = None
    self.wpyClassType = CEdit
    if nchars != None:
      self.wpyText = "0" * nchars
      self.WpyLabelSizer()
    elif text:
      self.wpyText  = text
      self.WpyLabelSizer()
    else:
      self.wpyText = "0000000000"
      self.WpyLabelSizer()
    self.wpySizeY = self.wpySizeY * 14 / 10
    self.wpyText  = text
  def Create(self):
    self.wpyParent.wpyChildList.append(self)
    if self.wpyPasswordChar:
      self.wpyStyle = self.wpyStyle | wpycon.ES_PASSWORD
      _wpy.CEditCreate(self)
      if wpyImplementNT:
        _wpy.SetPasswordChar(self, ord(self.wpyPasswordChar))
    else:
      _wpy.CEditCreate(self)
    self.wpyCreated = 1
    self.SetWindowText(self.wpyText)
    return self
  def SetSel(self, c1, c2):
     # Set the selection.  Set the cursor if c1==c2.
    _wpy.SetSel(self, 0, c1, 0, c2, 0)

class CMultiEditMeth:
  # Methods applicable to multi-line edit control and CEditView.
  def Clear(self):
    _wpy.EditClear(self)
  def ClearAll(self):
    _wpy.EditClearAll(self)
  def WpyAppend(self, text):	# Write text to the end of the view
    _wpy.WpyAppendEditView(self, text)
  def EnableWindow(self, enable):
    ret = self.wpyEnabled
    self.wpyEnabled = enable
    if enable:
      _wpy.SetReadOnly(self, 0)
    else:
      _wpy.SetReadOnly(self, 1)
    return ret
  def GetLine(self, line_num):
    return _wpy.GetLine(self, line_num)	# return string
  def GetLineCount(self):
    return _wpy.GetLineCount(self)	# return 1, or number of lines in control
  def GetSel(self):		# return (l1, c1, l2, c2) of selection
    return _wpy.GetSel(self)	# if no selection, return (l1, c1, l1, c1) of cursor
  def LineLength(self, line_num):
    return _wpy.LineLength(self, line_num)	# the line length
  def ReplaceSel(self, text):	# Replace the selection with text.
    _wpy.ReplaceSel(self, text)	# If no selection, insert at cursor.
  def SerializeInRaw(self, file):
    s = file.readline()
    if s:
      self.WpyAppend(s)
      if string.find(s, "\r") < 0:
        self.wpyLineEndReturn = 0
      else:
        self.wpyLineEndReturn = 1
      while 1:
        s = file.readline()
        if not s:
          break
        self.WpyAppend(s)
  def SerializeOutRaw(self, file):
    if self.wpyLineEndReturn:
      lineend = "\r\n"
    else:
      lineend = "\n"
    n = self.GetLineCount()
    for i in range(n - 1):
      s = self.GetLine(i)
      file.write(s + lineend)
    s = self.GetLine(n + 1)
    if s:
      file.write(s + lineend)
  def SetSel(self, l1, c1, l2, c2, no_scroll = 0):	# Set the selection.  Set the
    _wpy.SetSel(self, l1, c1, l2, c2, no_scroll)	# cursor if l1==l2 and c1==c2.

class CMultiEdit(CWnd, CMultiEditMeth):
# A multi-line edit control.
  def __init__(self, parent, text, nchars = None, nlines = 4):
    self.wpyParent = parent
    self.wpyChildList = []
    self.wpyStyle = wpycon.WS_CHILD | wpycon.WS_VISIBLE | wpycon.ES_AUTOHSCROLL | \
         wpycon.ES_MULTILINE | wpycon.ES_AUTOVSCROLL
    self.wpyFlags = wpyFlagsEdit
    self.wpyPasswordChar = None
    self.wpyClassType = CMultiEdit
    if nchars != None:
      self.wpyText = "0" * nchars
      self.WpyLabelSizer()
    elif text:
      self.wpyText  = text
      self.WpyLabelSizer()
    else:
      self.wpyText = "0000000000"
      self.WpyLabelSizer()
    self.wpyText  = text
    self.wpySizeY = self.wpySizeY * nlines
    self.wpyHScrollSize = self.wpyVScrollSize = 0
  def Create(self):
    self.wpyParent.wpyChildList.append(self)
    if self.wpyHScrollSize:
      self.wpyStyle = self.wpyStyle | wpycon.WS_HSCROLL
    if self.wpyVScrollSize:
      self.wpyStyle = self.wpyStyle | wpycon.WS_VSCROLL
    if self.wpyPasswordChar:
      self.wpyStyle = self.wpyStyle | wpycon.ES_PASSWORD
      _wpy.CMultiEditCreate(self)
      if wpyImplementNT:
        _wpy.SetPasswordChar(self, ord(self.wpyPasswordChar))
    else:
      _wpy.CMultiEditCreate(self)
    self.SetWindowText(self.wpyText)
    self.wpyCreated = 1
    return self

# end of controls.

# Start of windows.

class CDialog(CWnd):
  def __init__(self, parent, text):
    self.wpyChildList = []
    self.wpyText  = text
    self.wpyParent = parent
    # parent.wpyChildList.append(self)
    self.wpyFlags = wpyFlagsDialog
    self.wpyDefaultButton = None
  def DoModal(self, parent = None):	# MFC: return the exit value of a modal dialog
    global wpyDoModalResult
    wpyDoModalResult = None
    self.wpyFlags = self.wpyFlags + wpyFlagsModal
    if parent:
      ret = _wpy.DoModal(self, parent)
    else:
      ret = _wpy.DoModal(self, self.wpyParent)
    self.wpyFlags = self.wpyFlags & ~wpyFlagsModal
    # ret is 0 if user pressed a button, 1 for Enter, 2 for esc or close box
    return wpyDoModalResult
  def EndDialog(self, result):	# MFC: end a modal dialog with value "result"
    global wpyDoModalResult
    wpyDoModalResult = result
    if self.wpyFlags & wpyFlagsModal:
      _wpy.EndDialog(self, 0)
    else:
      raise BadEndDialog
  def Create(self):
    # Create a modeless dialog (not used for modal)
    _wpy.CDialogCreate(self, self.wpyParent)
    return self
  def Destroy(self):
    # Destroy a modeless dialog or modal dialog.
    if self.wpyFlags & wpyFlagsModal:
      self.EndDialog(None)
    else:
      self.DestroyWindow()
  def OnEnter(self):	# Called if user presses Enter
    if self.wpyDefaultButton:
      self.wpyDefaultButton.WpyCommandRouter()
  def OnEsc(self):	# Called if user presses ESC
    self.Destroy()

class CFileDialogOpen:
  def __init__(self, name = None, filter = ("All Files", "*")):
    self.wpyChildList = []
    self.wpyText  = "File Dialog"
    self.wpyParent = None
    self.wpyDefaultName = name
    self.wpyFilter = filter
    if self.__class__ == CFileDialogOpen:
      self.wpyOpen = 1
    else:
      self.wpyOpen = 0
  def DoModal(self, parent = None):
    return _wpy.FileDlgDoModal(self, parent)

class CFileDialogSave(CFileDialogOpen):
  pass

class CView(CWnd, TkMenuHandlersView):
  def __init__(self, templ, doc, frame):
    self.wpyDocument = doc
    self.wpyParent = frame
    frame.wpyChildList.append(self)
    doc.wpyViewList.insert(0, self)
    self.wpyChildList = []
    self.wpyText = "View"
    self.wpyFlags = wpyFlagsView + wpyFlagsMouse
    self.wpyDefaultButton = None
    self.wpyDestroyedByClose = 0
    self.wpyMenuBar = None
    self.wpyHasResize = 0
    self.wpyHScrollSize = self.wpyVScrollSize = 0
    self.wpyHScrollWinSize = self.wpyVScrollWinSize = 1
    self.wpyHScrollPos = self.wpyVScrollPos = 0
    self.wpyphysDrawing = 0
    self.wpyCreated = 1
    self.wpyDrawable = 1
    if wpyImplementNT:
      self.wpyDC = CDC(self)
    else:
      self.wpyDC = CDC_Tk(self)
  def WpyPhysOnDraw(self, DC, isprinting, w, h):
    if DC and self.wpyDrawable:
      if isprinting and wpyImplementNT:
        x, y = _wpy.GetTextExtent(DC, "0000000000")
        DC.wpyCharSizeX = (x + 5) / 10
        DC.wpyCharSizeY = y
        DC.wpyOneMeter = _wpy.GetDeviceCaps(DC, wpycon.HORZRES) * 1000.0 \
             / _wpy.GetDeviceCaps(None, wpycon.HORZSIZE)
        DC.wpyOnePoint = DC.wpyOneMeter * 0.000352778
      else:
        DC.wpyCharSizeX = self.wpyCharSizeX
        DC.wpyCharSizeY = self.wpyCharSizeY
        DC.wpyOneMeter = self.wpyOneMeter
        DC.wpyOnePoint = self.wpyOnePoint
      DC.wpySizeX = w
      DC.wpySizeY = h
      DC.wpyFlags = self.wpyFlags
      DC.wpyLocX  = DC.wpyLocY = 0
      DC.wpyIsPrinting = isprinting
      self.OnDraw(DC)
  def OnDraw(self, DC):
    pass
  def WpyPhysOnDestroy(self):
    self.OnDestroy()
    self.wpyDocument.wpyViewList.remove(self)
    self.wpyParent.wpyChildList.remove(self)
    self.wpyParent = None
    self.wpyDC = None
  def OnInitialUpdate(self):
    pass
  def WpyPhysOnHScroll(self, pos):
    self.wpyHScrollPos = pos
    self.OnHScroll(None)
  def WpyPhysOnVScroll(self, pos):
    self.wpyVScrollPos = pos
    self.OnVScroll(None)
  def OnUpdate(self, sender, update):
    self.InvalidateRect()

class CScrollView(CView):
  wpyClassName = "Scroll"
  def __init__(self, templ, doc, frame):
    CView.__init__(self, templ, doc, frame)
    self.wpyClassType = CScrollView
    self.wpyMouseMove = 0
    self.wpySizeX = self.wpySizeY = 0
    self.wpytkIdToItem = {}	# Translate tk canvas id to drawn object
  def GetDrawnObject(self, x, y):
    "return the single drawn object located at view coords (x, y), or None"
    return _wpy.GetDrawnObject(self, x, y)
  def GetDeviceScrollPosition(self):
    return _wpy.GetDeviceScrollPosition(self)
  def SetScrollSizes(self):	# MFC but abbreviated
    _wpy.SetScrollSizes(self)
  def WpyPhysOnSize(self, flags, sizex, sizey):
    rect = CRect()
    rect.wpyFlags = self.wpyFlags
    rect.wpyLocX = rect.wpyLocY = 0
    rect.wpySizeX = self.wpyHScrollWinSize = sizex
    rect.wpySizeY = self.wpyVScrollWinSize = sizey
    self.OnSize(rect)
  def Redraw(self, obj):
    if wpyImplementNT:
      _wpy.WpyRedraw(self, obj)
    else:	# Tk
      obj.WpyRedraw(self)
  def DestroyDrawn(self, obj):
    if wpyImplementNT:
      _wpy.WpyDestroyDrawn(self, obj)
    else:	# Tk
      wpy_tk.WpyTkCall(self.wpytkName, "delete", obj.wpytkId)

class CEditView(CView, CMultiEditMeth):
  wpyClassName = "Edit"
  def __init__(self, templ, doc, frame):
    CView.__init__(self, templ, doc, frame)
    self.wpyClassType = CEditView
    #if wpyApp == None:
    #  raise wpyNoApp
    #if parent == None:
    #  parent = wpyApp
    self.wpyVScrollSize = 1
    self.wpyHScrollSize = 1
    self.wpyMouseMove = 0
    # self.wpySizeX = max(100, (len(text) + 8) * self.wpyCharSizeX)
    self.wpySizeX = self.wpyCharSizeX * 70
    self.wpySizeY = self.wpyCharSizeY * 20
    if wpyImplementNT:
      self.wpyLineEndReturn = 1
    else:
      self.wpyLineEndReturn = 0

wpyphysTkMenuEnabled = 1
if wpyImplementTK:
  wpyphysTkMenuEnabled = 0	# disable unavailable items

  class AboutWin(CDialog):	# The standard dialog box
    def __init__(self):
      CDialog.__init__(self, None,
          "Python wpy version %s" % wpyVersion)
      self.b = CPushButton(self, "OK")
      self.m = CMessage(self, "This is a demonstration of version %s"
        % wpyVersion +
        """ of a proposed Python GUI abstraction tool.\
    Please direct comments and complaints to:\
   Jim Ahlstrom        jim@interet.com""")
      self.wpySizeX = self.m.wpySizeX * 1.2
      self.wpySizeY = self.m.wpySizeY * 1.2 + self.b.wpySizeY * 3
      self.b.wpySizeX = self.b.wpySizeX * 3
    def OnInitDialog(self):
      rect = self.GetClientRect()
      self.m.WpyPlace(rect, 0.5, 0.1, "n")
      self.b.WpyPlace(rect, 0.5, 0.8, "center")
      self.m.Create()
      self.b.Create()
    def OnButtonOK(self, control):
      self.EndDialog(0)

  class CTkFileDlg(CDialog):
    def __init__(self, open, filename = None, filter = ("All Files", "*")):
      self.wpyOpen = open
      if open:
        CDialog.__init__(self, None, "Open")
      else:
        CDialog.__init__(self, None, "Save As")
      if filename:
        self.dir, self.file = os.path.split(filename)
        if not os.path.isdir(self.dir):
          self.dir = os.getcwd()
      else:
        self.dir = os.getcwd()
        self.file = ""
      self.MakeRegex(filter[1])
    def MakeRegex(self, pattern):
      self.filter = pattern
      f = ""	# Set up a regex for file names
      for ch in pattern:
        if ch == "*":
          f = f + ".*"
        elif ch == ".":
          f = f + "\."
        elif ch == "?":
          f = f + "."
        else:
          f = f + ch
      self.regex = regex.compile(f)
    def OnInitDialog(self):
      border = self.wpyCharSizeX * 3
      if self.wpyOpen:
        lf = CLabel(self, "Open File: ")
      else:
        lf = CLabel(self, "Save File: ")
      ld = CLabel(self, "Directory: ")
      lx = CLabel(self, "Filter:")
      self.WpyMakeEqualSize(lf, ld, lx)
      self.win_file = CEdit(self, self.file, 0)
      self.label_dir = CLabel(self, self.dir)
      self.label_filter = CLabel(self, self.filter)
      self.label_filter.wpySizeX = self.wpyCharSizeX * 70
      lf.wpyLocX = lf.wpyLocY = border
      self.win_file.WpyPlace(lf, 1.0, 0.5, "w")
      ld.WpyPlace(lf, 0.0, 1.5, "nw")
      lx.WpyPlace(ld, 0.0, 1.1, "nw")
      self.label_dir.WpyPlace(ld, 1.0, 0.0, "nw")
      self.label_filter.WpyPlace(lx, 1.0, 0.0, "nw")
      lbox = self.filebox = CListBox(self, "file", [], 0)
      x = lbox.wpySizeX = wpyApp.wpySizeX * 20 / 100
      y = lbox.wpySizeY = wpyApp.wpySizeY * 30 / 100
      lbox.WpyPlace(lx, 0.0, 1.5, "nw")
      dbox = self.dirbox = CListBox(self, "directory", [], 0)
      dbox.wpySizeX = x
      dbox.wpySizeY = y
      dbox.WpyPlace(lbox, 1.0, 0.0, "nw")
      dbox.wpyLocX = dbox.wpyLocX + border
      ok = CPushButton(self, "OK")
      cancel = CPushButton(self, "  Cancel  ")
      self.WpyMakeEqualSize(ok, cancel)
      self.wpyDefaultButton = ok
      ok.WpyPlace(dbox, 1.0, 0.0, "nw")
      ok.wpyLocX = ok.wpyLocX + border
      cancel.WpyPlace(ok, 0.0, 1.5, "nw")
      x = ok.wpyLocX + ok.wpySizeX
      self.win_file.wpySizeX = x - self.win_file.wpyLocX
      self.label_dir.wpySizeX = x - self.label_dir.wpyLocX
      lf.Create()
      ld.Create()
      lx.Create()
      self.win_file.Create()
      self.label_dir.Create()
      self.label_filter.Create()
      ok.Create()
      cancel.Create()
      lbox.Create()
      dbox.Create()
      self.setlist()
      try:
        i = lbox.wpyListBoxItems.index(self.file)
        lbox.SetCurSel(i)
      except:
        pass
      self.wpySizeX = ok.wpyLocX + ok.wpySizeX + border
      self.wpySizeY = lbox.wpyLocY + lbox.wpySizeY + border
      self.MoveWindowSize()
    def OnButtonOK(self, control):
      s = self.win_file.GetWindowText()
      s = string.strip(s)
      path = os.path.join(self.dir, s)
      if os.path.isfile(path):
        if not self.wpyOpen:
          if _wpy._WpyPhysOkToReplace(path):
            self.EndDialog(path)
        else:
          self.EndDialog(path)
      elif "*" in s or "?" in s:
        self.MakeRegex(s)
        self.setlist()
        self.label_filter.SetWindowText(self.filter)
      else:
        if not self.wpyOpen:
          self.EndDialog(path)
    def setlist(self):
      filelist = []
      dirlist = [".."]
      for i in os.listdir(self.dir):
        if i == "." or i == "..":
          continue	# Python 1.2 used to include these, 1.3 does not.
        path = os.path.join(self.dir, i)
        if os.path.isdir(path):
          dirlist.append(i)
        else:
          if self.regex.match(i) == len(i):
            filelist.append(i)
      filelist.sort()
      dirlist.sort()
      self.filebox.WpySetItems(filelist)
      self.dirbox.WpySetItems(dirlist)
    def OnButtonCancel(self, control):
      self.EndDialog(None)
    def OnListBox(self, control):
      if control.wpyText == "file":
        lbox = self.filebox
        s = lbox.wpyListBoxItems[lbox.wpySelected]
        s = string.strip(s)
        self.win_file.SetWindowText(s)
      else:
        lbox = self.dirbox
        s = lbox.wpyListBoxItems[lbox.wpySelected]
        s = string.strip(s)
        if s == "..":
          self.dir, x = os.path.split(self.dir)
          self.label_dir.SetWindowText(self.dir)
          self.win_file.SetWindowText("")
          self.setlist()
        elif s == ".":
          pass
        else:
          path = os.path.join(self.dir, s)
          self.dir = path
          self.label_dir.SetWindowText(path)
          self.win_file.SetWindowText("")
          self.setlist()
    def OnListBoxDbl(self, control):
      self.OnButtonOK(control)

##############################################################
#
# Standard Python WPY code which is identical for
# all supported hardware/OS platforms.

class CWpyDialog(CDialog):
  def __init__(self, title, text, aspect, default, *args):
    CDialog.__init__(self, None, title)
    self.buttons = []
    n = 0
    if type(args[0]) == type(""):
      a = args
    else:
      a = args[0]
    for butn in a:
      b = CPushButton(self, butn)
      b.wpyHandler = "_wpy_dialog_handler"
      b.index = n
      n = n + 1
      self.buttons.append(b)
    if default >= 0:
      self.wpyDefaultButton = self.buttons[default]
    self.WpyMakeEqualSize(self.buttons)
    butnX = self.buttons[0].wpySizeX
    butnY = self.buttons[0].wpySizeY
    x1, y = self.GetTextExtent(text)
    x2 = butnX * n * 1.3
    if aspect == None:
      aspect = max(3.2, float(x2 * x2) / ((x1 + x2) * y))
    self.msg = CMessage(self, text, aspect)
    self.wpySizeX = max(self.msg.wpySizeX * 1.2, x2)
    self.wpySizeY = self.msg.wpySizeY + butnY * 4
  def OnInitDialog(self):
    rect = self.GetClientRect()
    butnX = self.buttons[0].wpySizeX
    butnY = self.buttons[0].wpySizeY
    self.msg.WpyPlace(rect, 0.5, 0.1, "n")
    y = float(rect.wpySizeY + self.msg.wpySizeY + self.msg.wpyLocY) / 2 / rect.wpySizeY
    rect.WpyMakeEqualSpaceX(0, 1, y, "center", self.buttons)
    self.msg.Create()
    for b in self.buttons:
      b.Create()
  def _wpy_dialog_handler(self, control):
    self.EndDialog(control.index)

class CTkIgnEscDialog(CWpyDialog):	# Ignore Esc Key
  def OnEsc(self):
    pass

# Start of items in menus.

class WpyMenuItem(CMenu):
# An abstract class - menu items.
  def __init__(self, parent, text):
    self.wpyParent = parent
    self.wpyText = text
    self.wpyChildList = []
    parent.wpyChildList.append(self)
    self.WpyMakeHandlerName()
  def Destroy(self):
  # Remove the item from the menu
    list = self.wpyParent.wpyChildList
    index = list.index(self)
    del list[index]
    _wpy.WpyMenuItemDestroy(self, index)
  def Insert(self, index=999):
  # Insert a new item at the position "index"
    list = self.wpyParent.wpyChildList
    if index < len(list):
      list.insert(index, self)
    else:
      list.append(self)
    self.WpyPrivateMakeMenu(index)

class CMenuButton(WpyMenuItem):
  def Create(self):
    if self.wpyHandler == None:
      self.wpyEnabled = 0
    style = wpycon.MF_STRING
    if not self.wpyEnabled:
      style = style + wpycon.MF_GRAYED
    self.wpyphysMenuFlags = style
    return self
  def WpyPhysOnCommand(self):
    self.WpyCommandRouter()

class CMenuCheck(WpyMenuItem):
# A menu item which selects/unselects an option.
# The wpyCheckValue is 1/0 for checked/unchecked.
  def __init__(self, parent, text):
    WpyMenuItem.__init__(self, parent, text)
    self.wpyCheckValue = 0
  def Create(self):
    if self.wpyHandler == None:
      self.wpyEnabled = 0
    style = wpycon.MF_STRING
    if not self.wpyEnabled:
      style = style + wpycon.MF_GRAYED
    if self.wpyCheckValue:
      style = style + wpycon.MF_CHECKED
    style = style + wpycon.WPY_MF_CHECKBOX
    self.wpyphysMenuFlags = style
    return self
  def WpyPhysDoCheck(self):
    self.SetCheck(not self.wpyCheckValue)
  def WpyPhysOnCommand(self):
    self.WpyPhysDoCheck()
    self.WpyCommandRouter()
  def SetCheck(self, check = 1):
    if check:
      self.wpyphysMenuFlags = self.wpyphysMenuFlags | wpycon.MF_CHECKED
      self.wpyCheckValue = 1
    else:
      self.wpyphysMenuFlags = self.wpyphysMenuFlags & ~ wpycon.MF_CHECKED
      self.wpyCheckValue = 0

class CMenuRadio(WpyMenuItem):
# A menu item which selects/unselects one of a group of options.
# See CRadioButton.
  def __init__(self, parent, text, group = None):
    WpyMenuItem.__init__(self, parent, text)
    if group == None:
      self.wpyGroup = self
      self.wpyGroupList = [self]
      self.wpyRadioSelected = self	# this button is ON, the rest are OFF
    else:
      self.wpyGroup = group
      group.wpyGroupList.append(self)
      self.wpyRadioSelected = group
  def Create(self):
    if self.wpyHandler == None:
      self.wpyEnabled = 0
    style = wpycon.MF_STRING
    if not self.wpyEnabled:
      style = style + wpycon.MF_GRAYED
    if self.wpyRadioSelected == self:
      style = style + wpycon.MF_CHECKED
    style = style + wpycon.WPY_MF_RADIOBUTTON
    self.wpyphysMenuFlags = style
    return self
  def WpyPhysOnCommand(self):
    self.SetCheck()
    self.WpyCommandRouter()
  def SetCheck(self, check = 1):
    if check:
      for b in self.wpyGroup.wpyGroupList:
        b.wpyRadioSelected = self
        b.wpyphysMenuFlags = b.wpyphysMenuFlags & ~ wpycon.MF_CHECKED
      self.wpyphysMenuFlags = self.wpyphysMenuFlags | wpycon.MF_CHECKED

class CMenuLine(WpyMenuItem):
# A menu item which just draws a line between other items.  Not interactive.
  def __init__(self, parent):
    WpyMenuItem.__init__(self, parent, "menuline")
  def Create(self):
    self.wpyphysMenuFlags = style = wpycon.MF_SEPARATOR
    return self

# End of pop-up menu items.

# Start of standard menu items:

class MenuLine(CMenuLine):	# Just an alias for CMenuLine
  def __init__(self, parent):
    CMenuLine.__init__(self, parent)

# Standard menu buttons for "File":
class MenuFile(CMenuButton):
  def __init__(self, parent, text = "File"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Commands to open/close/save the document"
class MenuFileNew(CMenuButton):
  def __init__(self, parent, text = "New"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Create a new document"
    self.wpyHandler = "wpy FileNew"
  def WpyPhysOnCommand(self):
    if self.wpyHandler == "wpy FileNew":
      wpyApp.FileNew()
    else:
      self.WpyCommandRouter()
class MenuFileOpen(CMenuButton):
  def __init__(self, parent, text = "Open..."):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Open an existing document"
    self.wpyHandler = "wpy FileOpen"
  def WpyPhysOnCommand(self):
    if self.wpyHandler == "wpy FileOpen":
      wpyApp.FileOpen()
    else:
      self.WpyCommandRouter()
class MenuFileClose(CMenuButton):
  def __init__(self, parent, text = "Close"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Close the active document"
    self.wpyMessageID = wpycon.ID_FILE_CLOSE
    self.wpyHandler = "WpymenuFileClose"
class MenuFileSave(CMenuButton):
  def __init__(self, parent, text = "Save"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Save the active document"
    self.wpyMessageID = wpycon.ID_FILE_SAVE
    self.wpyHandler = "WpymenuFileSave"
class MenuFileSaveas(CMenuButton):
  def __init__(self, parent, text = "Save As..."):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Save the active document with a new name"
    self.wpyMessageID = wpycon.ID_FILE_SAVE_AS
    self.wpyHandler = "WpymenuFileSaveas"
class MenuFilePrint(CMenuButton):
  def __init__(self, parent, text = "Print"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Print the current document"
    self.wpyMessageID = wpycon.ID_FILE_PRINT
    self.wpyHandler = "WpymenuFilePrint"
class MenuFilePrintersetup(CMenuButton):
  def __init__(self, parent, text = "Printer setup"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Change printer settings"
    self.wpyMessageID = wpycon.ID_FILE_PRINT_SETUP
    self.wpyHandler = "WpymenuFilePrintersetup"
    self.wpyEnabled = wpyphysTkMenuEnabled
class MenuFileExit(CMenuButton):
  def __init__(self, parent, text = "Exit"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Quit the application"
    self.wpyHandler = "wpy FileExit"
  def WpyPhysOnCommand(self):
    if self.wpyHandler == "wpy FileExit":
      wpyApp.Exit()
    else:
      self.WpyCommandRouter()

# Standard menu buttons for "Edit":
class MenuEdit(CMenuButton):
  def __init__(self, parent, text = "Edit"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Commands to edit the document"
class MenuEditUndo(CMenuButton):
  def __init__(self, parent, text = "Undo"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Undoes the last change"
    self.wpyMessageID = wpycon.ID_EDIT_UNDO
    self.wpyHandler = "WpymenuEditUndo"
    self.wpyEnabled = wpyphysTkMenuEnabled
class MenuEditRedo(CMenuButton):
  def __init__(self, parent, text = "Redo"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Redoes the previously undone action"
    self.wpyMessageID = wpycon.ID_EDIT_REDO
    self.wpyHandler = "WpymenuEditRedo"
    self.wpyEnabled = wpyphysTkMenuEnabled
class MenuEditCut(CMenuButton):
  def __init__(self, parent, text = "Cut"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Removes the selection and puts it on the Clipboard"
    self.wpyMessageID = wpycon.ID_EDIT_CUT
    self.wpyHandler = "WpymenuEditCut"
class MenuEditCopy(CMenuButton):
  def __init__(self, parent, text = "Copy"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Copies the selection and puts it on the Clipboard"
    self.wpyMessageID = wpycon.ID_EDIT_COPY
    self.wpyHandler = "WpymenuEditCopy"
class MenuEditPaste(CMenuButton):
  def __init__(self, parent, text = "Paste"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Inserts Clipboard contents at the insertion point"
    self.wpyMessageID = wpycon.ID_EDIT_PASTE
    self.wpyHandler = "WpymenuEditPaste"
class MenuEditDelete(CMenuButton):
  def __init__(self, parent, text = "Delete"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Erases the selection"
    self.wpyMessageID = wpycon.ID_EDIT_CLEAR
    self.wpyHandler = "WpymenuEditClear"
class MenuEditSelectall(CMenuButton):
  def __init__(self, parent, text = "Select all"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Selects the entire document"
    self.wpyMessageID = wpycon.ID_EDIT_SELECT_ALL
    self.wpyHandler = "WpymenuEditSelectall"

# Standard menu buttons for "View":
class MenuView(CMenuButton):
  def __init__(self, parent, text = "View"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Commands to change what is viewed"
class MenuViewStatusbar(CMenuCheck):
  def __init__(self, parent, text = "Status bar"):
    CMenuCheck.__init__(self, parent, text)
    self.wpyCheckValue = 1
    self.wpyMessage = "Show or hide the status bar"
    self.wpyHandler = "wpy ViewStatusbar"
  def WpyPhysOnCommand(self):
    self.WpyPhysDoCheck()
    if self.wpyHandler == "wpy ViewStatusbar":
      frame = self.wpyApp.wpyMainFrame
      status = frame.wpyStatusLine
      if status != None:
        status.ShowWindow(self.wpyCheckValue)
        frame.RecalcLayout()
    else:
      self.WpyCommandRouter()

# Standard menu buttons for "Window":
class MenuWindow(CMenuButton):
  def __init__(self, parent, text = "Window"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Commands to arrange the windows"
class MenuWindowNewwindow(CMenuButton):
  def __init__(self, parent, text = "New Window"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Open another window for the active document"
    self.wpyHandler = "wpy WindowNew"
  def WpyPhysOnCommand(self):
    if self.wpyHandler == "wpy WindowNew":
      wpyApp.WindowNew()
    else:
      self.WpyCommandRouter()
class MenuWindowCascade(CMenuButton):
  def __init__(self, parent, text = "Cascade"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Arrange windows so they overlap"
    self.wpyMessageID = wpycon.ID_WINDOW_CASCADE
    self.wpyHandler = "WpymenuWindowCascade"
    self.wpyEnabled = wpyphysTkMenuEnabled
class MenuWindowTilehorz(CMenuButton):
  def __init__(self, parent, text = "Tile Horizontally"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Arrange windows as non-overlapping tiles"
    self.wpyMessageID = wpycon.ID_WINDOW_TILE_HORZ
    self.wpyHandler = "WpymenuWindowTilehorz"
    self.wpyEnabled = wpyphysTkMenuEnabled
class MenuWindowTilevert(CMenuButton):
  def __init__(self, parent, text = "Tile Vertically"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Arrange windows as non-overlapping tiles"
    self.wpyMessageID = wpycon.ID_WINDOW_TILE_VERT
    self.wpyHandler = "WpymenuWindowTilevert"
    self.wpyEnabled = wpyphysTkMenuEnabled
class MenuWindowArrangeicons(CMenuButton):
  def __init__(self, parent, text = "Arrange Icons"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Arrange icons at the bottom of the window"
    self.wpyMessageID = wpycon.ID_WINDOW_ARRANGE
    self.wpyHandler = "WpymenuWindowArrange"
    self.wpyEnabled = wpyphysTkMenuEnabled

# Standard menu buttons for "Help":
class MenuHelp(CMenuButton):
  def __init__(self, parent, text = "Help"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Commands to display program information"
class MenuHelpAbout(CMenuButton):
  def __init__(self, parent, text = "About"):
    CMenuButton.__init__(self, parent, text)
    self.wpyMessage = "Display program information, version number and copyright"
    self.wpyMessageID = wpycon.ID_APP_ABOUT
    self.wpyHandler = "WpymenuHelpAbout"
