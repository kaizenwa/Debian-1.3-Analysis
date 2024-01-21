#! /usr/local/bin/python

from wpy import *

# Global variables:
menu_en_1 = menu_en_2 = menu_en_3 = None
menu_options = None

class My_app(CWinApp):
  def InitInstance(self):
    templ = CSingleDocTemplate(CDocument, My_frame, My_view, My_menu)
    templ.wpyText = "Demonstration of Controls"
    self.AddDocTemplate(templ)
    self.FileNew()
  def OnMenuAbout(self, control):
    AboutWin().DoModal()
  def OnButtonQuit(self, control):
    self.Exit()

class My_frame(CFrameWnd):
  def __init__(self, templ, doc):
    CFrameWnd.__init__(self, templ, doc)
    self.wpyStatusLine = CStatusBar(self, "")
  def OnButtonDisableOptions(self, control):
    if menu_options.wpyEnabled:
      menu_options.EnableMenuItem(0)
      control.SetWindowText("Enable Options")
    else:
      menu_options.EnableMenuItem(1)
      control.SetWindowText("Disable Options")
    self.DrawMenuBar()
  def OnMenuOptionsStatusline(self, control):
    self.wpyStatusLine.ShowWindow(control.wpyCheckValue)
    self.RecalcLayout(1)

class My_view(CScrollView):
  def OnCreate(self, event):
    self.wpyHScrollSize = self.wpyScreenSizeX
    self.wpyVScrollSize = self.wpyScreenSizeY
    # Make labels:
    self.label1 = CLabel(self, "Use menu to enable buttons").Create()
    self.label2 = CLabel(self, "Window xxx, yyy").Create()
    self.label3 = CLabel(self, "Scrollbar location xxx").Create()
    self.label4 = CLabel(self, "Window scroll location xx, yy").Create()
    # Make buttons:
    self.butn1 = b = CPushButton(self, "Disable Options").Create()
    b.EnableWindow(menu_en_1.wpyCheckValue)
    self.butn2 = b = CPushButton(self, "Hide Text").Create()
    b.EnableWindow(menu_en_2.wpyCheckValue)
    self.butn3 = b = CPushButton(self, "Dialog").Create()
    b.EnableWindow(menu_en_3.wpyCheckValue)
    self.butn4 = CPushButton(self, "Quit").Create()
    self.butn1.WpyMakeEqualSize(self.butn1, self.butn2, self.butn3, self.butn4)
    # Make control status.
    self.radio = 0
    self.checks = (0, 0, 0)
    self.entrytext = "initial text  "
    self.scrollpos = 0
    self.color = "None"
    self.wpySizeY = self.label1.wpySizeY * 10
    self.wpySizeX = self.wpySizeY * 24 / 10
  def OnSize(self, rect):
    self.label2.SetWindowText(\
        "Window%4d,%4d  " % (rect.wpySizeX, rect.wpySizeY))
    obj = self.label1
    leading = obj.wpySizeY
    xspace = leading
    x = x0 = leading * 2
    y = y0 = leading
    obj.WpyPlace(None, x, y, "nw")
    y = y + leading
    self.label2.WpyPlace(None, x, y, "nw")
    x = rect.wpySizeX * 0.6
    y = y0
    self.label3.WpyPlace(None, x, y, "nw")
    y = y + leading
    self.label4.WpyPlace(None, x, y, "nw")
    y = y + leading * 2

    x = x0
    y0 = y
    leading = self.butn1.wpySizeY * 3 / 2
    self.butn1.WpyPlace(None, x, y, "nw")
    self.x0 = x + self.butn1.wpySizeX + leading
    self.y0 = y
    y = y + leading
    self.butn2.WpyPlace(None, x, y, "nw")
    y = y + leading
    self.butn3.WpyPlace(None, x, y, "nw")
    y = y + leading
    self.butn4.WpyPlace(None, x, y, "nw")
    self.MakeLabel4()
  def OnDraw(self, DC):
    import wpy
    x = self.x0
    y = self.y0
    f = CFont().Create()
    o = DC.SelectObject(f)
    o = None
    o = DC.DrawText("Hello World", x, y)
    y = y + o.wpySizeY
    f = CFont("swiss", 35).Create()
    DC.SelectObject(f)
    o = DC.DrawText("Hello World", x, y)
    xmax = x + o.wpySizeX
    y = y + o.wpySizeY
    size = DC.wpyOneMeter * 0.005
    x = self.x0
    b = CBrush((200, 0, 0)).Create()
    DC.SelectObject(b)
    DC.Rectangle(x, y, size, size)
    x1 = x
    x = x + size + size
    b = CBrush((0, 200, 0)).Create()
    DC.SelectObject(b)
    DC.Rectangle(x, y, size, size)
    x = x + size + size
    b = CBrush((0, 0, 200)).Create()
    DC.SelectObject(b)
    DC.Rectangle(x, y, size, size)
    y = y + size * 2
    for i in range(1, 6, 2):
      pen = CPen(i, (0, i * 40, 0)).Create()
      DC.SelectObject(pen)
      DC.MoveTo(x1, y)
      o = DC.LineTo(x, y)
      y = y + size
    size = size * 2
    x = x1
    pen = CPen(1, (255, 0, 0)).Create()
    DC.SelectObject(pen)
    DC.Arc(x, y, size, size, 20, 140)
    x = x + size * 12 / 10
    DC.Pie(x, y, size, size, 20, 140)
    x = x + size * 12 / 10
    DC.Chord(x, y, size, size, 20, 140)
    x = x + size * 12 / 10
    DC.Polygon([(x, y), (x + size, y + size), (x + size*2, y), (x + size, y - size)])
    photo = CImage("demo04.gif").Create()
    if photo.wpySizeX:
      DC.DrawImage(photo, xmax + DC.wpyOneMeter * 0.005, self.y0)
    else:
      AfxMessageBox("Could not open file demo04.gif.")
  def OnInitialUpdate(self):
    self.MakeStatusText()
    self.label3.SetWindowText("Scrollbar location%3d  " % self.scrollpos)
    self.SetScrollSizes()
  def OnButtonHideText(self, control):
    lab = self.label1
    if lab.wpyVisible:
      lab.ShowWindow(0)
      control.SetWindowText("Show Text")
    else:
      lab.ShowWindow(1)
      control.SetWindowText("Hide Text")
  def OnButtonDialog(self, control):
    d = ControlDlg(self, "Python wpy version %s" % wpyVersion)
    if d.DoModal() == 0:	# User pressed "OK"
      self.label3.SetWindowText("Scrollbar location%3d  " % self.scrollpos)
      self.MakeStatusText()
  def OnMenuFileShowpopup(self, control):
    pop = CMenu()
    rect = self.GetWindowRect()
    pop.wpyLocX = rect.wpyLocX + rect.wpySizeX / 2
    pop.wpyLocY = rect.wpyLocY + rect.wpySizeY / 2
    i1  = CMenuButton(pop, "Button 1")
    i2  = CMenuButton(pop, "Button 2")
    i3  = CMenuButton(pop, "Button 3")
    i1.wpyHandler = i2.wpyHandler = i3.wpyHandler = "OnMenuPopup"
    pop.TrackPopupMenu(self.wpyParent)
  def OnMenuPopup(self, control):
    print "Got popup command for", control.wpyText
  def OnMenuButtonsEnableButton1(self, control):
    self.butn1.EnableWindow(control.wpyCheckValue)
  def OnMenuButtonsEnableButton2(self, control):
    self.butn2.EnableWindow(control.wpyCheckValue)
  def OnMenuButtonsEnableButton3(self, control):
    self.butn3.EnableWindow(control.wpyCheckValue)
  def OnMenuButtonsVisibleButton1(self, control):
    self.butn1.ShowWindow(control.wpyCheckValue)
  def OnMenuButtonsVisibleButton2(self, control):
    self.butn2.ShowWindow(control.wpyCheckValue)
  def OnMenuButtonsVisibleButton3(self, control):
    self.butn3.ShowWindow(control.wpyCheckValue)
  def MakeStatusText(self):
    s = "Buttons: Radio %d" % (self.radio + 1)
    n = 1
    for obj in self.checks:
      if obj:
        s = s + ", check %d" % n
      n = n + 1
    s = s + ".   Selected color: %s" % self.color
    s = s + ".   Edit text: %s." % self.entrytext
    self.wpyParent.wpyStatusLine.SetWindowText(s)
  def MakeLabel4(self):
    self.label4.SetWindowText("Window scroll location%3d,%3d  " % \
          (self.wpyHScrollPos, self.wpyVScrollPos))
  def OnVScroll(self, event):
    self.MakeLabel4()
  def OnHScroll(self, event):
    self.MakeLabel4()

class AboutWin(CDialog):	# The dialog box.
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

class ControlDlg(CDialog):	# The dialog box showing controls.
  def OnInitDialog(self):
    view = self.wpyParent
    xmax = 0
    ymax = 0
    # Make radio buttons:
    self.radio1	= r1 = CRadioButton(self, "Radio 1", None).Create()
    self.radio2	= r2 = CRadioButton(self, "Radio 2", r1).Create()
    self.radio3	= r3 = CRadioButton(self, "Radio 3", r1).Create()
    self.radios = [r1, r2, r3]
    self.radios[view.radio].SetCheck()
    self.radio1.wpyHandler = \
      self.radio2.wpyHandler = \
      self.radio3.wpyHandler = \
          None
    self.radio1.WpyMakeEqualSize(self.radio1, self.radio2, self.radio3)
    leading = self.radio1.wpySizeY * 1.1
    x = y = x0 = y0 = leading
    self.radio1.WpyPlace(None, x, y, "nw")
    y = y + leading
    self.radio2.WpyPlace(None, x, y, "nw")
    y = y + leading
    self.radio3.WpyPlace(None, x, y, "nw")
    xmax = max(xmax, x + self.radio3.wpySizeX)
    ymax = max(ymax, y + self.radio3.wpySizeY)
    # Make check buttons:
    self.check1	= CCheckButton(self, "Check 1").Create()
    self.check2	= CCheckButton(self, "Check 2").Create()
    self.check3	= CCheckButton(self, "Check 3").Create()
    l = view.checks
    if l[0]:
      self.check1.SetCheck()
    if l[1]:
      self.check2.SetCheck()
    if l[2]:
      self.check3.SetCheck()
    self.check1.wpyHandler = \
      self.check2.wpyHandler = \
      self.check3.wpyHandler = \
        "OnButtonCheck123"
    self.check1.WpyMakeEqualSize(self.check1, self.check2, self.check3)
    x = x + self.radio1.wpySizeX + leading
    y = y0
    self.check1.WpyPlace(None, x, y, "nw")
    y = y + leading
    self.check2.WpyPlace(None, x, y, "nw")
    y = y + leading
    self.check3.WpyPlace(None, x, y, "nw")
    ymax = max(ymax, y + self.check3.wpySizeY)
    # Make listbox control
    s = self.listbox = CListBox(self, "colors", 
       ["Green", "Blue", "Red", "Purple", "Orange",
          "Black", "White", "Yellow"], 0)
    s.wpySizeY = ymax
    x = s.wpyLocX = x + self.check1.wpySizeX + leading
    y = s.wpyLocY = y0
    s.Create()
    if view.color == "None":
      s.SetCurSel(-1)
    else:
      s.SetCurSel(s.wpyListBoxItems.index(view.color))
    xmax = max(xmax, x + s.wpySizeX)
    ymax = max(ymax, y + s.wpySizeY)
    ymax = ymax + leading
    # Make edit control.
    label = CLabel(self, "Edit: ")
    label.wpyLocX = x0
    label.wpyLocY = y0 = ymax
    label.Create()
    edit = self.edit = CEdit(self, view.entrytext, 20)
    edit.wpyBrush = CBrush((150, 0, 0)).Create()
    edit.wpyTextColor = (100, 200, 100)
    edit.wpySizeX = edit.wpyCharSizeX * 8
    edit.WpyPlace(label, 1.0, 0.5, "w")
    edit.Create()
    edit.SetFocus()
    edit.SetSel(0, 999)
    ymax = max(ymax, edit.wpyLocY + edit.wpySizeY)
    # Make scroll bar control:
    label = self.scroll_label = CLabel(self, "%3d " % view.scrollpos)
    label.wpyLocX = edit.wpyLocX + edit.wpySizeX + leading * 2
    label.wpyLocY = y0
    label.Create()
    scroll = self.scroll = CScrollBar(self, "h")
    scroll.wpyScrollSize = 20
    scroll.wpyScrollWinSize = 5
    scroll.wpyScrollPos = view.scrollpos
    scroll.WpyPlace(label, 1.0, 0.5, "w")
    self.scroll.wpySizeX = xmax - scroll.wpyLocX
    scroll.Create()
    # Add two buttons.
    ok =   CPushButton(self, "OK")
    self.wpyDefaultButton = ok
    ok.Create()
    cancel = CPushButton(self, "Cancel").Create()
    ok.WpyMakeEqualSize(ok, cancel)
    # Set dialog box size and place buttons.
    self.wpySizeX = xmax + leading * 2
    self.wpySizeY = ymax + ok.wpySizeY * 3
    y = float(ymax + self.wpySizeY) / 2 / self.wpySizeY
    ok.WpyPlace(self, 0.3333, y, "center")
    cancel.WpyPlace(self, 0.6667, y, "center")
    # Size sets total size, allow for top border
    self.wpySizeY = self.wpySizeY + ok.wpySizeY
    self.MoveWindowSize()
    return 0	# Indicate that focus was set
  def OnScroll(self, control):
    self.scroll_label.SetWindowText("%d " % control.wpyScrollPos)
  def OnButtonOK(self, control):
    self.wpyParent.entrytext = self.edit.GetWindowText()
    self.wpyParent.scrollpos = self.scroll.wpyScrollPos
    self.wpyParent.radio = self.radios.index(self.radio1.wpyRadioSelected)
    self.wpyParent.checks = (self.check1.wpyCheckValue,
         self.check2.wpyCheckValue, self.check3.wpyCheckValue)
    index = self.listbox.wpySelected
    if index >= 0:
      self.wpyParent.color = self.listbox.wpyListBoxItems[index]
    else:
      self.wpyParent.color = "None"
    self.EndDialog(0)
  def OnButtonCancel(self, control):
    self.EndDialog(None)

class My_menu(CMenu):
  def __init__(self):
    CMenu.__init__(self)
    global menu_options
    file	= MenuFile(self)	# Using pre-defined menu buttons
    edit	= MenuEdit(self)
    buttons	= CMenuButton(self, "Buttons")
    menu_options= CMenuButton(self, "Options")
    help	= MenuHelp(self)

    # file items:
    save	= MenuFileSave(file)
    save.wpyHandler = None
    saveas	= MenuFileSaveas(file)
    saveas.wpyHandler = None
    CMenuLine(file)
    showpop	= CMenuButton(file, "Show Popup")
    showpop.wpyMessage = "Demonstration of floating popup menu"
    CMenuLine(file)
    exit	= MenuFileExit(file)

    # edit items:
    edit.EnableMenuItem(0)

    # button items:
    msg_en = "Enable/disable buttons one to three"
    msg_v  = "Show/hide buttons one to three"
    buttonEnable	= CMenuButton(buttons, "Enable")
    buttonEnable.wpyMessage = msg_en
    buttonVisible	= CMenuButton(buttons, "Visible")
    buttonVisible.wpyMessage = msg_v
    # button enable items:
    global menu_en_1, menu_en_2, menu_en_3
    menu_en_1	= CMenuCheck(buttonEnable, "Button 1")
    menu_en_1.wpyCheckValue = 1
    menu_en_1.wpyMessage = msg_en
    menu_en_2	= CMenuCheck(buttonEnable, "Button 2")
    menu_en_2.wpyCheckValue = 0
    menu_en_2.wpyMessage = msg_en
    menu_en_3	= CMenuCheck(buttonEnable, "Button 3")
    menu_en_3.wpyCheckValue = 0
    menu_en_3.wpyMessage = msg_en
    # button visible items:
    b1	= CMenuCheck(buttonVisible, "Button 1")
    b1.wpyCheckValue = 1
    b1.wpyMessage = msg_v
    b2	= CMenuCheck(buttonVisible, "Button 2")
    b2.wpyCheckValue = 1
    b2.wpyMessage = msg_v
    b3	= CMenuCheck(buttonVisible, "Button 3")
    b3.wpyCheckValue = 1
    b3.wpyMessage = msg_v

    # options items:
    statusline  = CMenuCheck(menu_options, "Status line")
    statusline.wpyCheckValue = 1
    statusline.wpyMessage = "Show/hide the status line"
    CMenuLine(menu_options)
    b1	= CMenuButton(menu_options, "Button 1")
    b2	= CMenuButton(menu_options, "Button 2")
    b1.wpyMessage = b2.wpyMessage = "Nothing happens"
    CMenuLine(menu_options)
    c1	= CMenuCheck(menu_options, "Check 1")
    c2	= CMenuCheck(menu_options, "Check 2")
    c2.wpyCheckValue = 1
    c1.wpyMessage = c2.wpyMessage = "Nothing happens.  Just a demonstration of checks."
    CMenuLine(menu_options)
    r1	= CMenuRadio(menu_options, "Radio 1")
    r2	= CMenuRadio(menu_options, "Radio 2", r1)
    r3	= CMenuRadio(menu_options, "Radio 3", r1)
    r1.wpyMessage = r2.wpyMessage = r3.wpyMessage = \
      "Nothing happens.  Just a demonstration of radio buttons."

    # help items:
    a = MenuHelpAbout(help)
    a.wpyHandler = "OnMenuAbout"

# Start the application, respond to events.
My_app()
