#! /usr/local/bin/python
# This a minimal demo program for a proposed Python window
# system.  It will run unchanged on any system which has a "wpy.py".
#
# "Wpy" is a completely object-oriented system.  You should create your
# own classes derived from "Wpy" classes to use it.  You must have
# at least an Application class and a View class.  This demo also
# uses a Button class to close the window.

from wpy import *	# Standard module name on all platforms, but
			# contents varies for Motif, Tk, MSW, OS2, etc.
class MyView(CScrollView):
  def OnCreate(self, event):
    b = CPushButton(self, "Quit")
    b.Create()
    self.button = b
  def OnSize(self, rect):
    self.button.WpyPlace(rect, 0.5, 0.5, "center")

class MyApp(CWinApp):
  def InitInstance(self):
    templ = CSingleDocTemplate(CDocument, CFrameWnd, MyView)
    templ.wpyText = "Usual Hello World Demo"
    self.AddDocTemplate(templ)
    self.FileNew()
  def OnButtonQuit(self, control):
    self.Exit()

# Start the application, respond to events.
app = MyApp()
