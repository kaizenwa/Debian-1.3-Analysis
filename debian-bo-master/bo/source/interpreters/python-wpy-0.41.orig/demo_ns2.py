#! /usr/local/bin/python

import string, regex
import wpy, wpycon

# Make some brushes to draw points.
whiteBrush = wpy.CBrush((255, 255, 255)).Create()
ignoreBrush = wpy.CBrush((127, 127, 127)).Create()	# ignored
regularBrush = wpy.CBrush((255, 255, 255)).Create()	# regular
selIgnoreBrush = wpy.CBrush((255, 0, 0)).Create()	# selected ignored
selRegularBrush = wpy.CBrush((0, 255, 0)).Create()	# selected regular
menu_undo_enabled = 0

# points are represented by a list [x, y, censor, descr], where (x, y) are
# the coordinates, descr is the descriptive text, and censor is:
#  censor == 0    Normal point.
#  censor == 1    Censored (ignored) point.
#  censor == 2    A line in the file starting with "#", a comment.
#  censor == 3    A bit of text written to location (x, y) on the graph.

class GraphDocument(wpy.CDocument):
  def DeleteContents(self):	# Reset to six points.
    self.points = []
    self.points.append([0, 0.1, 0, "Point 1"])
    self.points.append([1, 1.1, 0, "Point 2"])
    self.points.append([2, 2.5, 0, "Point 3"])
    self.points.append([3, 3.2, 0, "Point 4"])
    self.points.append([4, 4.1, 0, "Point 5"])
    self.points.append([5, 5.2, 0, "Point 6"])
    if self.wpyViewList:
      view = self.wpyViewList[0]
      view.SetSelected(None)
      view.label_eqn.SetWindowText("")
  def OnMenuDeletepoint(self, control):
    view = self.wpyViewList[0]
    point = view.selected_obj.point
    self.changed_point = point
    self.changed_index = -1
    global menu_undo_enabled
    menu_undo_enabled = 1
    self.points.remove(point)
    self.SetModifiedFlag()
    view.current_point = None
    self.UpdateAllViews(None, None)
  def OnMenuUndo(self, control):
    view = self.wpyViewList[0]
    if self.changed_index < 0:	# point was deleted
      self.points.append(self.changed_point)
    else:	# point was changed
      self.points[self.changed_index] = self.changed_point
    global menu_undo_enabled
    menu_undo_enabled = 0
    view.current_point = self.changed_point
    self.UpdateAllViews(None, None)
  def OnMenuAddcoords(self, control):
    view = self.wpyViewList[0]
    dialog = PointEditor(view, "Add a Point")
    dialog.point = [0.000, 0.000, 0, ""]
    point = dialog.DoModal()
    if point:
      self.points.append(point)
      self.SetModifiedFlag()
      view.current_point = point
      self.UpdateAllViews(None, None)
  def OnMenuChangepoint(self, control):
    view = self.wpyViewList[0]
    dialog = PointEditor(view, "Edit the Selected Point")
    dialog.point = view.selected_obj.point
    point = dialog.DoModal()
    self.ChangePoint(point)
  def ChangePoint(self, point):
    if point:
      view = self.wpyViewList[0]
      self.changed_point = view.selected_obj.point
      self.changed_index = self.points.index(view.selected_obj.point)
      self.points[self.changed_index] = point
      global menu_undo_enabled
      menu_undo_enabled = 1
      self.SetModifiedFlag()
      view.current_point = point
      self.UpdateAllViews(None, None)
  def SerializeIn(self, file):		# Read the file.
    # Regular expression for (int, int, zero or one or three, text)
    regex_read = regex.compile("\([-.0-9]+\) +\([-.0-9]+\) +\([013]\) +\(.*\)")
    while 1:
      text = file.readline()
      if not text:
        break
      text = string.strip(text)
      if text == "":		# Allow blank line
        point = [None, None, 2, ""]	# Special format of comment
        self.points.append(point)
      elif text[0] == "#":	# Comment
        point = [None, None, 2, text]	# Special format of comment
        self.points.append(point)
      elif regex_read.match(text) == len(text):
        desc = regex_read.group(4)
        if desc == None:
          desc = ""
        point = [string.atof(regex_read.group(1)),
                 string.atof(regex_read.group(2)),
                 string.atoi(regex_read.group(3)),
                 desc]
        self.points.append(point)
      else:
        ans = wpy.AfxMessageBox("Failed to read point: %s" % text,
              wpycon.MB_OKCANCEL | wpycon.MB_ICONEXCLAMATION)
        if ans == "Cancel":
          break
  def SerializeOut(self, file):		# Write the file.
    for point in self.points:
      if point[2] == 2:	# comment
        file.write(point[3] + "\n")
      else:
        file.write("%f %f %d %s\n" % tuple(point))
  def FitPoints(self):		# Calculate least squares line.
    n = 0
    sxx = sx = sxy = sy = 0.0
    for point in self.points:
      if point[2] == 0:	# Ignore comments and censored points
        n = n + 1
        x = point[0]
        y = point[1]
        sx = sx + x
        sy = sy + y
        sxx = sxx + x * x
        sxy = sxy + x * y
    if n < 2:	# Not enough points for a line.
      return None, None
    else:
      d = n * sxx - sx * sx
      a = (sxx * sy - sx * sxy) / d
      b = (n * sxy - sx * sy) / d
      return a, b

class GraphView(wpy.CScrollView):	# The view.
  def OnCreate(self, event):
    self.selected_obj = None
    self.current_point = None
    self.X0 = 0.0
    self.Y0 = 0.0
    self.ScaleX = 1.0
    self.ScaleY = 1.0
    self.lower_margin = 0.0	# In meters
    self.moving = None
    # Create controls describing the point
    self.label_point = [wpy.CLabel(self, "x:"), wpy.CLabel(self, "0000000000"),
      wpy.CLabel(self, "y:"), wpy.CLabel(self, "0000000000"),
      wpy.CCheckButton(self, "Censored"),
      wpy.CLabel(self, "Descrip:"), wpy.CLabel(self, "x" * 80)]
    for o in self.label_point:
      o.wpyBrush = whiteBrush
      o.Create()
    self.label_point[4].EnableWindow(0)		# Disable button
    o = self.label_fit = wpy.CLabel(self, "Fit:")
    o.wpyBrush = whiteBrush
    o.Create()
    o = self.label_eqn = wpy.CLabel(self, "x" * 80)
    o.wpyBrush = whiteBrush
    o.Create()
    o.SetWindowText("")
  def OnInitialUpdate(self):
    self.wpyDocument.DeleteContents()
  def OnSize(self, rect):	# Initial size or resize of window.
    chx = x = self.wpyCharSizeX
    chy = self.wpyCharSizeY
    y = rect.wpySizeY - chy
    self.label_fit.WpyPlace(None, x, y, "w")
    self.label_eqn.WpyPlace(self.label_fit, 1.3, 0.5, "w")
    y = y - chy
    for o in self.label_point:
      o.WpyPlace(None, x, y, "w")
      x = x + o.wpySizeX + chx
    self.lower_margin = float(chy * 3) / self.wpyOneMeter	# In meters
    self.InvalidateRect()
  def SetSelected(self, new):	# Change selected point.
    old = self.selected_obj	# Previous selected object
    if old:			# Redraw old point.
      if old.point[2] == 0:	# Point is not ignored
        old.wpyBrush = regularBrush
        self.Redraw(old)
      elif old.point[2] == 1:	# Point is ignored
        old.wpyBrush = ignoreBrush
        self.Redraw(old)
      elif old.point[2] == 3:	# Point is text
        old.wpyTextColor = (0, 0, 0)
        self.Redraw(old)
    if new:			# Draw new point
      p = new.point
      self.current_point = p
      t = "%.4g" % p[0]
      self.label_point[1].SetWindowText(t)	# x
      t = "%.4g" % p[1]
      self.label_point[3].SetWindowText(t)	# y
      self.label_point[4].SetCheck(p[2])	# censored
      self.label_point[6].SetWindowText(p[3])	# description
      if p[2] == 0:
        self.label_point[4].EnableWindow(1)	# Enable censor button
        new.wpyBrush = selRegularBrush
        self.Redraw(new)
      elif p[2] == 1:
        self.label_point[4].EnableWindow(1)	# Enable censor button
        new.wpyBrush = selIgnoreBrush
        self.Redraw(new)
      elif p[2] == 3:
        new.wpyTextColor = selRegularBrush.wpyColor
        self.Redraw(new)
    else:			# No point is selected
      self.current_point = None
      self.label_point[1].SetWindowText("")	# x
      self.label_point[3].SetWindowText("")	# y
      self.label_point[4].SetCheck(0)		# censored
      self.label_point[4].EnableWindow(0)	# Disable censor button
      self.label_point[6].SetWindowText("")	# description
    self.selected_obj = new
  def OnDraw(self, DC):		# Draw the view.
    doc = self.wpyDocument
    Xmin = None
    for point in doc.points:	# Get min and max points, calculate scale
      if point[2] == 2:	# Point is a #comment
        continue
      elif point[2] == 3:	# Point is comment at (x, y)
        x = point[0] * DC.wpySizeX	# coords are relative to the screen
        y = point[1] * DC.wpySizeY
        o = DC.DrawText(point[3], x, y)	# draw text
        o.point = point
        continue
      if Xmin == None:
        Xmin = Xmax = point[0]
        Ymin = Ymax = point[1]
      else:
        x = point[0]
        y = point[1]
        Xmin = min(Xmin, x)
        Ymin = min(Ymin, y)
        Xmax = max(Xmax, x)
        Ymax = max(Ymax, y)
    if Xmin == None:	# empty list
      Xmin = Ymin = 0
      Xmax = Ymax = 1
    if Xmin == Xmax:	# single point?
      Xmax = Xmin + 1
    if Ymin == Ymax:	# single point?
      Ymax = Ymin + 1
    cm = DC.wpyOneMeter / 100
    ScreenXmin = cm	# determine size of graph, leave margin at bottom
    ScreenXmax = DC.wpySizeX - cm / 2
    ScreenYmin = cm / 2
    ScreenYmax = DC.wpySizeY - self.lower_margin * DC.wpyOneMeter
    if Xmin == Xmax:
      ScaleX = 1.0
    else:
      ScaleX = float(ScreenXmax - ScreenXmin) / (Xmax - Xmin)
    if Ymin == Ymax:	# Y-axis is reversed.
      ScaleY = - 1.0
    else:
      ScaleY = - float(ScreenYmax - ScreenYmin) / (Ymax - Ymin)
    X0 = ScreenXmin - ScaleX * Xmin
    X1 = ScreenXmax - ScaleX * Xmax
    Y0 = ScreenYmin - ScaleY * Ymax
    Y1 = ScreenYmax - ScaleY * Ymin
    # Draw the x axis at y=0 if it fits.
    if Y0 >= ScreenYmin and Y0 <= ScreenYmax:
      DC.MoveTo(ScreenXmin, Y0)
      DC.LineTo(ScreenXmax, Y0)
    else:
      DC.MoveTo(ScreenXmin, ScreenYmax)
      DC.LineTo(ScreenXmax, ScreenYmax)
    # Draw the y axis at x=0 if it fits.
    if X0 >= ScreenXmin and X0 <= ScreenXmax:
      DC.MoveTo(X0, ScreenYmin)
      DC.LineTo(X0, ScreenYmax)
    else:
      DC.MoveTo(ScreenXmin, ScreenYmin)
      DC.LineTo(ScreenXmin, ScreenYmax)
    # Draw each point in the list unless it is a comment.
    self.X0 = X0
    self.Y0 = Y0
    self.ScaleX = ScaleX
    self.ScaleY = ScaleY
    self.ScreenXmin = ScreenXmin
    self.ScreenXmax = ScreenXmax
    DC.SelectObject(regularBrush)
    radius = max(DC.wpyOneMeter / 1000 * 2, 4)
    selected_obj = None
    for point in doc.points:
      if point[2] >= 2:	# Point is comment
        continue
      x = point[0]
      y = point[1]
      x = int(X0 + ScaleX * x)	# convert to screen coords.
      y = int(Y0 + ScaleY * y)
      if point[2] == 1:	# Point is ignored
        DC.SelectObject(ignoreBrush)
        o = DC.Circle(x, y, radius)
        DC.SelectObject(regularBrush)
      else:
        o = DC.Circle(x, y, radius)
      o.point = point
      if point == self.current_point:
        selected_obj = o
    self.DrawGraph(DC)
    self.SetSelected(selected_obj)
  def DrawGraph(self, DC):	# Draw the graph
    eqn = self.label_eqn.GetWindowText()
    if not eqn:
      return
    # draw a straight line from x-min to x-max.
    screenx = self.ScreenXmin
    if self.ScaleX != 0:	# convert to point coords.
      x = (screenx - self.X0) / self.ScaleX
    else:
      x = self.X0
    y = eval(eqn)	# Python expression is evaluated
    screeny = int(self.Y0 + self.ScaleY * y)	# convert to screen coords.
    DC.MoveTo(screenx, screeny)
    screenx = self.ScreenXmax
    if self.ScaleX != 0:	# convert to point coords.
      x = (screenx - self.X0) / self.ScaleX
    else:
      x = self.X0
    y = eval(eqn)	# Python expression is evaluated
    screeny = int(self.Y0 + self.ScaleY * y)	# convert to screen coords.
    DC.LineTo(screenx, screeny)
  def ScreenToPoint(self, x, y):	# Convert screen coords to the point coords.
    if self.ScaleX != 0:
      a = (x - self.X0) / self.ScaleX
    else:
      a = self.X0
    if self.ScaleY != 0:
      b = (y - self.Y0) / self.ScaleY
    else:
      b = self.Y0
    return a, b
  def OnLButtonDown(self, x, y, flags):
    if self.moving:	# We are dragging a point.
      return
    drawn = self.GetDrawnObject(x, y)
    if drawn and hasattr(drawn, "point"):	# Select the point
      self.SetSelected(drawn)
    else:			# Unselect point.
      self.SetSelected(None)
  def OnLButtonUp(self, x, y, flags):
    if self.moving:	# If we are dragging a point, stop.
      obj = self.moving
      point = [0, 0, obj.point[2], obj.point[3]]
      if obj.point[2] < 2:	# point is a regular point
        radius = max(self.wpyOneMeter / 1000 * 2, 4)
        x = obj.wpyLocX + radius
        y = obj.wpyLocY + radius
        point[0], point[1] = self.ScreenToPoint(x, y)
      else:	# point is text, coords are relative to screen
        rect = self.GetClientRect()	# coords of text are relative to screen
        point[0] = float(obj.wpyLocX) / rect.wpySizeX
        point[1] = float(obj.wpyLocY) / rect.wpySizeY
      self.wpyDocument.ChangePoint(point)	# Fix point at new location.
      wpy.ReleaseCapture()
      self.moving = None
      self.InvalidateRect()
  def OnRButtonDown(self, x, y, flags):
    if self.moving:	# Are we dragging a point?
      return
    self.OnLButtonDown(x, y, flags)	# Select point
    pop = wpy.CMenu()		# Now put up a popup menu.
    rect = self.GetWindowRect()
    pop.wpyLocX = rect.wpyLocX + x
    pop.wpyLocY = rect.wpyLocY + y
    pop.x = x
    pop.y = y
    if self.selected_obj:	# See if a point is selected
      wpy.CMenuButton(pop, "Change point")
      wpy.CMenuButton(pop, "Move point")
      wpy.CMenuButton(pop, "Delete point")
      pop.TrackPopupMenu(self)
    else:
      o = wpy.CMenuButton(pop, "Undo")
      o.EnableMenuItem(menu_undo_enabled)
      wpy.CMenuLine(pop)
      wpy.CMenuButton(pop, "Add text")
      wpy.CMenuButton(pop, "Add point")
      wpy.CMenuButton(pop, "Add coords")
      wpy.CMenuButton(pop, "Fit expr")
      wpy.CMenuLine(pop)
      wpy.CMenuButton(pop, "Help")
      pop.TrackPopupMenu(self)
  def OnRButtonUp(self, x, y, flags):
    self.OnLButtonUp(x, y, flags)
  def OnMenuAddtext(self, control):
    dialog = PointEditor(self, "Add text to the graph")
    rect = self.GetClientRect()	# coords of text are relative to screen
    x = float(control.wpyParent.x) / rect.wpySizeX
    y = float(control.wpyParent.y) / rect.wpySizeY
    dialog.point = [x, y, 3, ""]
    point = dialog.DoModal()
    if point:
      doc = self.wpyDocument
      doc.points.append(point)
      self.InvalidateRect()
  def OnMenuMovepoint(self, control):
    self.moving = self.selected_obj
    self.SetCapture()
  def OnMouseMove(self, x, y, flags):
    if self.moving:	# If we are dragging a point, move it.
      obj = self.moving
      radius = max(self.wpyOneMeter / 1000 * 2, 4)
      obj.wpyLocX = x - radius
      obj.wpyLocY = y - radius
      obj.wpyBrush = selRegularBrush
      self.Redraw(obj)
  def OnMenuAddpoint(self, control):
    doc = self.wpyDocument
    n = len(doc.points)
    if n == 0:
      point = [0, 0, 0, ""]
    elif n == 1:
      point = [1, 1, 0, ""]
    else:
      x = control.wpyParent.x
      y = control.wpyParent.y
      x, y = self.ScreenToPoint(x, y)
      point = [x, y, 0, ""]
    doc.points.append(point)
    self.current_point = point
    self.InvalidateRect()
  def OnButtonCensored(self, control):
    if self.selected_obj:
      self.selected_obj.point[2] = control.wpyCheckValue
      self.SetSelected(self.selected_obj)	# To change the color
  def OnMenuFitexpr(self, control):
    a, b = self.wpyDocument.FitPoints()
    if a != None:
      self.label_eqn.SetWindowText("%.8g + %.8g * x" % (a, b))
      self.InvalidateRect()
    else:
      self.label_eqn.SetWindowText("")

class PointEditor(wpy.CDialog):	# The point editor dialog box.
  def OnInitDialog(self):
    ch = self.wpyCharSizeX
    y = x = ch * 2
    space = self.wpyCharSizeY * 20 / 10
    self.wpySizeX = self.wpyScreenSizeX / 2
    self.wpySizeY = space * 8
    label_x = wpy.CLabel(self, "X:").Create()
    label_y = wpy.CLabel(self, "Y:").Create()
    label_d = wpy.CLabel(self, "Descr:").Create()
    label_x.WpyMakeEqualSize(label_x, label_y, label_d)
    label_x.WpyPlace(None, x, y, "w")
    label_y.WpyPlace(None, x, y + space, "w")
    label_d.WpyPlace(None, x, y + space * 2, "w")
    x = label_x.wpyLocX + label_x.wpySizeX + ch
    point = self.point
    self.edit_x = edit_x = wpy.CEdit(self, `point[0]`).Create()
    self.edit_y = edit_y = wpy.CEdit(self, `point[1]`).Create()
    self.censor = point[2]
    self.edit_d = edit_d = wpy.CEdit(self, point[3]).Create()
    edit_x.wpySizeX = edit_y.wpySizeX = edit_d.wpySizeX = self.wpySizeX - x
    edit_x.WpyPlace(None, x, y, "w")
    edit_y.WpyPlace(None, x, y + space, "w")
    edit_d.WpyPlace(None, x, y + space * 2, "w")
    ok     = wpy.CPushButton(self, "OK").Create()
    cancel = wpy.CPushButton(self, "Cancel").Create()
    self.wpyDefaultButton = ok
    ok.WpyMakeEqualSize(ok, cancel)
    self.wpySizeX = self.wpySizeX + ch * 3	# Right margin
    self.WpyMakeEqualSpaceX(0, 1, 0.70, "center", (ok, cancel))
    self.MoveWindowSize()
  def OnButtonOK(self, control):
    try:
      x = string.atof(self.edit_x.GetWindowText())
    except ValueError:
      wpy.AfxMessageBox("Improper value for X")
      return
    try:
      y = string.atof(self.edit_y.GetWindowText())
    except ValueError:
      wpy.AfxMessageBox("Improper value for Y")
      return
    d = self.edit_d.GetWindowText()
    point = [x, y, self.censor, d]
    self.EndDialog(point)
  def OnButtonCancel(self, control):
    self.EndDialog(None)

class GraphApp(wpy.CWinApp):
  def InitInstance(self):
    templ = wpy.CSingleDocTemplate(GraphDocument, wpy.CFrameWnd, GraphView)
    templ.wpyText = "Graph Demo"
    self.AddDocTemplate(templ)
    self.FileNew()
  def OnMenuHelp(self, control):
    wpy.AfxMessageBox("""This program \
displays points and a fitted graph of the points.  \
The features are: read/write points and text to file, enter points from \
keyboard or mouse, mark point as censored (ignored), add text to graph, \
select/edit/move/delete/display attributes of points or text, \
edit undo, calculate and display least squares line fit, print graph.""",
    wpycon.MB_ICONINFORMATION)
  def OnMenuFileformat(self, control):
    wpy.AfxMessageBox("""The \
file format is lines of: "x-coord y-coord censor description", where censor is \
zero for a normal point, one for a censored (ignored) point \
or three for text placed on the graph.  A line starting with "#" is \
a comment. Please see the example file demo08.dat.  For example:

# Start of my file.
0.1234 0.4567  0   This is a description of the point.""",
    wpycon.MB_ICONINFORMATION)

# Start the application, respond to events.
app = GraphApp()
