# This is "wpyinter.py".
# This file is run when no file is specified on the Python
# command line.  It supports an interactive console session.

import sys
import string
import wpy

exec_dict = {}

class MyDocument(wpy.CDocument):
  def DeleteContents(self):
    list = self.wpyViewList
    if list:
      view = self.wpyViewList[0]
      view.ClearAll()
  def SerializeIn(self, file):
    view = self.wpyViewList[0]
    view.SerializeInRaw(file)
  def SerializeOut(self, file):
    view = self.wpyViewList[0]
    view.SerializeOutRaw(file)

class MyView(wpy.CEditView):
  def write(self, text):
    self.WpyAppend(text)
  def OnCreate(self, event):
    sys.stdout = self
    sys.stderr = self
    f = wpy.CFont("modern")
    f.Create()
    self.SetFont(f, 1)
  def OnMenuFileRunselection(self, control):
    last_line = self.GetLineCount() - 1
    l1, c1, l2, c2 = self.GetSel()
    if l1 == l2 and c1 == c2:	# No selection, select the whole text window
      l1 = 0
      l2 = last_line
    text = ""
    for i in range(l1, l2 + 1):
      text = text + self.GetLine(i) + "\n"
    text = text + "\n"
    line_length = self.LineLength(last_line)
    self.SetSel(last_line, line_length, last_line, line_length, 1)	# Move to EOF
    self.WpyAppend("\n")
    exec text in exec_dict

class MyMenu(wpy.CMenu):
  def __init__(self):
    wpy.CMenu.__init__(self)
    file = wpy.MenuFile(self)
    edit = wpy.MenuEdit(self)
    help = wpy.MenuHelp(self)

    wpy.MenuFileNew(file)
    wpy.MenuFileOpen(file)
    wpy.MenuFileSave(file)
    wpy.MenuFileSaveas(file)
    wpy.MenuLine(file)
    wpy.CMenuButton(file, "Run selection")
    wpy.MenuLine(file)
    wpy.MenuFileExit(file)

    wpy.MenuEditUndo(edit)
    wpy.MenuEditRedo(edit)
    wpy.MenuLine(edit)
    wpy.MenuEditCut(edit)
    wpy.MenuEditCopy(edit)
    wpy.MenuEditPaste(edit)
    wpy.MenuEditDelete(edit)
    wpy.MenuEditSelectall(edit)

    wpy.CMenuButton(help, "Help")
    wpy.MenuLine(help)
    wpy.MenuHelpAbout(help)
      
class MyApp(wpy.CWinApp):
  def InitInstance(self):
    templ = wpy.CSingleDocTemplate(MyDocument, wpy.CFrameWnd, MyView, MyMenu)
    templ.wpyText = "Python Interactive Window"
    self.AddDocTemplate(templ)
    self.FileNew()
  def OnMenuHelpHelp(self, control):
    wpy.AfxMessageBox("This is an interactive window for Python statements.  " + \
       """First compose and edit your Python program.  """ + \
       "Then select the lines you want to execute with the mouse, " + \
       "and select File/Run to execute them.  Do not run other windowing " + \
       "programs from this window.  It is just for console-mode programs.")

# Start the application, respond to events.
app = MyApp()
