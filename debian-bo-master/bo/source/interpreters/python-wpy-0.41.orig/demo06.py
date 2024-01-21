#! /usr/local/bin/python

from wpy import *

class MyDocument(CDocument):
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

class MyView(CEditView):
  def OnCreate(self, event):
    f = CFont("modern")
    f.Create()
    self.SetFont(f, 1)

class MyMenu(CMenu):
  def __init__(self):
    CMenu.__init__(self)
    file = MenuFile(self)
    edit = MenuEdit(self)
    help = MenuHelp(self)

    MenuFileNew(file)
    MenuFileOpen(file)
    MenuFileSave(file)
    MenuFileSaveas(file)
    MenuLine(file)
    MenuFileExit(file)

    MenuEditUndo(edit)
    MenuEditRedo(edit)
    MenuLine(edit)
    MenuEditCut(edit)
    MenuEditCopy(edit)
    MenuEditPaste(edit)
    MenuEditDelete(edit)
    MenuEditSelectall(edit)

    CMenuButton(help, "Help")
    MenuLine(help)
    MenuHelpAbout(help)
      
class MyApp(CWinApp):
  def InitInstance(self):
    templ = CSingleDocTemplate(MyDocument, CFrameWnd, MyView, MyMenu)
    templ.wpyText = "Python Edit View"
    self.AddDocTemplate(templ)
    self.FileNew()
  def OnMenuHelpHelp(self, control):
    AfxMessageBox("This is very simple text editor.")

# Start the application, respond to events.
app = MyApp()
