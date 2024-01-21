# This is a Netscape plugin test program.

import wpy, wpycon

class MyView(wpy.CScrollView):
  def OnCreate(self, event):
    b = wpy.CPushButton(self, "Get URL")
    b.Create()
    self.button = b
  def OnSize(self, rect):
    self.button.WpyPlace(rect, 0.5, 0.5, "center")
  def OnDraw(self, DC):
    DC.DrawText("Hello World", 10, 10)

class MyStream(wpy.CNetscapeStream):
  def NPP_NewStream(self, type, seekable):
    self.button.EnableWindow(0)
    return wpycon.NP_ASFILE
  def NPP_StreamAsFile(self, filename):
    print "Got URL", filename
    self.button.EnableWindow(1)
  def NPP_DestroyStream(self, reason):
    wpy.CNetscapeStream.NPP_DestroyStream(self, reason)
    print "Stream ended with status", reason
    self.button.EnableWindow(1)

class MyApp(wpy.CWinApp):
  def InitInstance(self):
    templ = wpy.CSingleDocTemplate(wpy.CDocument, wpy.CFrameWnd, MyView)
    templ.wpyText = "Usual Hello World Demo"
    self.AddDocTemplate(templ)
    self.FileNew()
  def OnButtonGetURL(self, control):
    url = "http://www.python.org/"
    s = MyStream()
    s.button = control
    s.NPN_GetURL(url)


# Start the application, respond to events.
app = MyApp()
