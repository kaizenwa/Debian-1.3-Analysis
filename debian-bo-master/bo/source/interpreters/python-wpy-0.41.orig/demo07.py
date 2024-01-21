#! /usr/local/bin/python

import wpy, wpycon
import htmllib, string

class CFormatter:
  def __init__(self, view, DC):
    self.nospace = 1
    self.view = view
    self.DC = DC
    self.outtext = ""
    self.in_anchor = 0
    self.height = 0
    self.pixels = self.y = self.chars = self.indent = self.tempindent = 0
    self.font = wpy.CFont("roman").Create()
    DC.SelectObject(self.font)
    x, self.font.sizey = DC.GetTextExtent("W" * 20)
    x = self.font.sizex = (x + 19) / 20	# Largest character size
    r = view.GetClientRect()
    self.width = r.wpySizeX - x
  def setfont(self, font):
    if self.font != font:
      self.outpartial()
    if font.wpyCreated:
      self.DC.SelectObject(font)
    else:
      font.Create()
      self.DC.SelectObject(font)
      x, font.sizey = self.DC.GetTextExtent("W" * 20)
      font.sizex = (x + 19) / 20	# Largest character size
    self.font = font
  def flush(self):
    self.outline()
  def do_anchor(self, obj):
    f = self.parser
    a = self.in_anchor
    if a < 0:
      a = -a
      obj.anchor_has_href = 0
    else:
      obj.anchor_has_href = 1
    a = a - 1
    obj.anchorname	= f.anchornames[a]
    obj.anchor		= f.anchors[a]
    obj.anchortype	= f.anchortypes[a]
  def outpartial(self):
    "Output a partial line"
    if self.outtext:
      o = self.DC.DrawText(self.outtext, self.indent + self.tempindent, self.y)
      if self.in_anchor:
        self.do_anchor(o)
      w = o.wpySizeX
      self.height = o.wpySizeY
      self.pixels = 0
      self.chars = 0
      self.outtext = ""
      self.tempindent = self.tempindent + w
  def outline(self):
    "Output the rest of the line (or all of it)"
    if self.outtext:
      o = self.DC.DrawText(self.outtext, self.indent + self.tempindent, self.y)
      if self.in_anchor:
        self.do_anchor(o)
      h = o.wpySizeY
      self.y = self.y + h
      self.height = 0
      self.pixels = 0
      self.chars = 0
      self.outtext = ""
      self.view.wpyVScrollSize = self.y + h + 50
      self.nospace = 1
    else:
      self.y = self.y + self.height
      self.height = 0
    self.tempindent = 0
  def setleftindent(self, indent):
    self.indent = indent * self.DC.wpyOnePoint
  def needvspace(self, space):
    self.outline()
    self.y = self.y + self.font.sizey * space
    self.nospace = 1
  def addword(self, word, space):
    word = string.strip(word)
    length = len(word)
    if length:
      self.nospace = 0
    elif self.nospace:
      return
    # The current line is self.outtext, and its size is self.pixels pixels plus
    #   self.chars characters.
    if self.indent + self.tempindent + self.pixels + \
          (self.chars + length) * self.font.sizex < self.width:
      # Word fits easily
      self.outtext = self.outtext + word + " " * space
      self.chars = self.chars + length + space
    else:
      w, h = self.DC.GetTextExtent(self.outtext + word)
      i = self.indent + self.tempindent + w
      if self.indent + self.tempindent + w > self.width:
        self.outline()
        self.outtext = word + " " * space
        self.chars = length + space
        self.pixels = 0
      else:
        self.outtext = self.outtext + word + " " * space
        self.pixels = w
        self.chars = space
  def setjust(self, just):
    pass
  def bgn_anchor(self, id):
    self.outpartial()
    self.oldcolor = self.DC.SetTextColor((0, 0, 200))
    self.in_anchor = id
  def end_anchor(self, id):
    self.outpartial()
    self.DC.SetTextColor(self.oldcolor)
    self.in_anchor = 0

class CStylesheet:
    stdfontset = [
        wpy.CFont("roman", 18),
        wpy.CFont("roman-italic", 18),
        wpy.CFont("roman", 18, wpycon.FW_BOLD),
        wpy.CFont("modern", 18)
    ]
    h1fontset = [
        wpy.CFont("swiss", 48),
    ]
    h2fontset = [
        wpy.CFont("swiss", 36),
    ]
    h3fontset = [
        wpy.CFont("swiss", 24),
    ]
    # Indents are expressed in printers points.
    stdindent = 12
    ddindent = 24
    ulindent = 36
    h1indent = 12
    h2indent = 12
    literalindent = 30

class MyDocument(wpy.CDocument):
  def DeleteContents(self):
    self.cur_file = None
  def OnOpenDocument(self, filename):
    self.DeleteContents()
    self.wpyFileName = filename
    self.cur_file = filename
    self.SetModifiedFlag(0)
    self.SetTitle(self.wpyParent.wpyText + " - " + filename)

class MyView(wpy.CScrollView):
  def OnCreate(self, event):
    frame = self.wpyParent
    frame.wpySizeX = frame.wpyScreenSizeX * 8 / 10
    frame.wpySizeY = frame.wpySizeX / 2
    frame.MoveWindowSize()
  def OnSize(self, event):
    #Re-parse the file if the window size changes
    self.InvalidateRect()
  def OnInitialUpdate(self):
    #Make sure there is always a scroll bar
    self.wpyVScrollSize = self.wpyScreenSizeY
    self.SetScrollSizes()
  def OnDraw(self, DC):
    doc = self.wpyDocument
    if not doc.cur_file:
      return
    self.BeginWaitCursor()
    formatter = CFormatter(self, DC)
    parser = htmllib.AnchoringParser(formatter, CStylesheet)
    formatter.parser = parser
    file = open(doc.cur_file)
    parser.feed(file.read())
    parser.close()
    file.close()
    self.EndWaitCursor()
    self.wpyVScrollSize = max(self.wpyVScrollSize, self.wpyScreenSizeY)
    self.SetScrollSizes()
  def OnLButtonDown(self, x, y, flags):
    (x0, y0) = self.GetDeviceScrollPosition()
    x = x + x0
    y = y + y0
    drawn = self.GetDrawnObject(x, y)
    if drawn and hasattr(drawn, "anchorname"):
      print "href ", drawn.anchor

class MyMenu(wpy.CMenu):
  def __init__(self):
    wpy.CMenu.__init__(self)
    file = wpy.MenuFile(self)
    help = wpy.MenuHelp(self)

    wpy.MenuFileNew(file)
    wpy.MenuFileOpen(file)
    wpy.MenuFileSave(file)
    wpy.MenuFileSaveas(file)
    wpy.MenuLine(file)
    wpy.MenuFileExit(file)

    wpy.CMenuButton(help, "Help")
    wpy.MenuLine(help)
    wpy.MenuHelpAbout(help)
      
class MyApp(wpy.CWinApp):
  def InitInstance(self):
    templ = wpy.CSingleDocTemplate(MyDocument, wpy.CFrameWnd, MyView, MyMenu)
    templ.wpyText = "Python HTML Viewer"
    self.AddDocTemplate(templ)
    self.FileNew()
    try:
      parser = htmllib.AnchoringParser
    except AttributeError:
      wpy.AfxMessageBox("I'm sorry.  This demo only works with Python 1.2.")
      self.Exit()
  def OnMenuHelpHelp(self, control):
    wpy.AfxMessageBox("This is very simple html viewer.",
      wpycon.MB_OK | wpycon.MB_ICONINFORMATION)

# Start the application, respond to events.
app = MyApp()
