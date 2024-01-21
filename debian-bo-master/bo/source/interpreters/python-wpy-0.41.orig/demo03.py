#! /usr/local/bin/python

from wpy import *

# A Python function that generates dialog boxes with a text message
# and any number of buttons.  See Ousterhout, Tcl and the Tk Toolkit,
# Figs. 27.2-3, pp. 269-270.

class My_dialog(CDialog):
  def __init__(self, title, text, bitmap, default, *args):
    CDialog.__init__(self, None, title)
    self.buttons = []
    n = 0
    for butn in args:
      n = n + 1
      b = CPushButton(self, butn)
      b.wpyHandler = "OnButtonDialog"
      b.index = n
      self.buttons.append(b)
    if default >= 0:
      self.wpyDefaultButton = self.buttons[default]
    self.WpyMakeEqualSize(self.buttons)
    butnX = self.buttons[0].wpySizeX
    butnY = self.buttons[0].wpySizeY
    x1, y = self.GetTextExtent(text)
    x2 = butnX * n * 1.3
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
  def OnButtonDialog(self, control):
    self.EndDialog(control.index)

class MyView(CScrollView):
  def OnCreate(self, event):
    self.start = CPushButton(self, "Press Here To Start")
    self.start.wpyHandler = "OnButtonStart"
    self.endit = CPushButton(self, "Exit")
    self.WpyMakeEqualSize(self.start, self.endit)
    self.start.Create()
    self.endit.Create()
  def OnSize(self, rect):
    self.start.WpyPlace(rect, 0.5, 0.3333, "center")
    self.endit.WpyPlace(rect, 0.5, 0.6667, "center")
  def OnButtonStart(self, control):
    win = My_dialog('Not Responding',
	       "The file server isn't responding right now; "
	       "I'll keep trying.",
	       '',
	       0,
	       'OK')
    print "User pressed button", win.DoModal()
    win = My_dialog('File Modified',
	       'File "tcl.h" has been modified since '
	       'the last time it was saved. '
	       'Do you want to save it before exiting the application?',
	       'warning',
	       2,
	       'Save File',
	       'Discard Changes',
	       'Return To Editor')
    print "User pressed button", win.DoModal()
      
class MyApp(CWinApp):
  def InitInstance(self):
    templ = CSingleDocTemplate(CDocument, CFrameWnd, MyView)
    templ.wpyText = "Dialog demo from Mr. Ousterhout's Book p269"
    self.AddDocTemplate(templ)
    self.FileNew()
  def OnButtonExit(self, control):
    self.Exit()

if __name__ == '__main__':
  MyApp()
