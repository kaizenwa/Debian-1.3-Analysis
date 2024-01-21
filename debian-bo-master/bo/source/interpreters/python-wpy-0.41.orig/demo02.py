#! /usr/local/bin/python
# This program will run unchanged on any system which has a "wpy.py".

from wpy import *

class my_view(CScrollView):
  t1 = "Please press OK again to change the size of the window."
  t2 = "Please press OK to change the size of the window."
  t3 = "  Try changing the size with the window decorations too."
  def OnCreate(self, event):
    self.button1 = CPushButton(self, "OK")
    self.button2 = CPushButton(self, "Quit")
    self.WpyMakeEqualSize(self.button1, self.button2)
    self.button1.Create()
    self.button2.Create()
    self.msg = CMessage(self, self.t2 + self.t3)
    self.msg.Create()
    self.toggle = 0
  def make_msg(self):	# Toggle message text.
    if self.toggle:
      self.msg.SetWindowText(self.t1)
    else:
      self.msg.SetWindowText(self.t2 + self.t3)
  def OnSize(self, rect):
    self.button1.WpyPlace(rect, 0.3333, 0.70, "center")
    self.button2.WpyPlace(rect, 0.6667, 0.70, "center")
    self.msg.WpyPlace(rect, 0.5, 0.1, "n")
  def OnButtonOK(self, control):
# An "OK" button: Toggles between two window sizes.
    frame = self.wpyParent
    rect = frame.GetWindowRect()
    frame.wpySizeY = rect.wpySizeY	# Keep same Y size
    msg_size = self.msg.wpySizeX
    if self.toggle:
      self.toggle = 0
      frame.wpySizeX = msg_size * 1.5	# Change X size
    else:
      self.toggle = 1
      frame.wpySizeX = msg_size * 2
    self.make_msg()			# Change message
    frame.MoveWindowSize()
    
class MyApp(CWinApp):
  def InitInstance(self):
    templ = CSingleDocTemplate(CDocument, CFrameWnd, my_view)
    templ.wpyText = "Usual Hello World Demo"
    self.AddDocTemplate(templ)
    self.FileNew()
  def OnButtonQuit(self, control):
    self.Exit()

# Start the application, respond to events.
app = MyApp()
