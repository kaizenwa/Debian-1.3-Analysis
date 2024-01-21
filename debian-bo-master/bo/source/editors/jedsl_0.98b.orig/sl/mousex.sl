% mousex.sl		-*- SLang -*-
%
% Mouse routines for use inside an XTerm
% (and for Linux Console running selection/gpm?)
%
% Mouse actions:
% LEFT:
%	If a region is marked, unmark it.
%	Move the point to this new location.
%
% MIDDLE:
%	If a region is marked, copy it to the pastebuffer.
%	Otherwise, paste contents of the pastebuffer at the current point.
%
% RIGHT:
%	Extend the region to this point.
%
% Ctrl-MIDDLE:
%	If a region is marked, cut it to the pastebuffer.
%
% Status line actions:
%	LEFT:	switch window to a different buffer
%	MIDDLE:	split the window
%	RIGHT:	delete the window
%
% Cut/Paste Tips:
%   1.	LEFT click at the beginning of the region, drag to the end of the
%	region and release.
%
%   2. 	RIGHT click to extend the marked region.
%
%   3.	MIDDLE click to COPY the marked region to the pastebuffer.
%	or
%	Ctrl-MIDDLE click to KILL the marked region to the pastebuffer.
%
%   4.	MIDDLE click to paste the contents of the pastebuffer at the
%	current point (use Step 1 to set the current point).
%
% Notes:
%	Since XTerm normally uses Ctrl+MouseButton to activate popup menus,
%	you can use ^C+MouseButton to simulate a Ctrl+MouseButton event
%	-- i.e. press and release ^C before a mouse button event.
%
%	To temporarily override XTerm mouse reporting -- and get the normal
%	mouse selection behaviour -- hold down Shift or Mod1 (Meta, Alt) while
%	performing the mouse action.
%
%	Note that the mouse bindings use copy_region/kill_region/yank, while
%	the key bindings typically use the yankpop equivalents so, in effect,
%	the mouse gets its own kill buffer.

% mj olesen

variable Mouse_Event_Type = 0, Mouse_X = 0, Mouse_Y = 0;
variable Mouse_Button = 3;	% start with ButtonRelease event

setkey (".0 mouse_event",	"\e[M");	% MouseButton
setkey (".'c'mouse_event",	"^C\e[M");	% simulate Ctrl+MouseButton
setkey (".'m'mouse_event",	"\e\e[M");	% simulate Meta+MouseButton
setkey (".'s'mouse_event",	"\e[\e[M");	% simulate Shift+MouseButton

% hooks to properly restore selection state (See $JED_ROOT/doc/mouse.txt)
#if$TERM xterm			% X11 mouse reporting
define suspend_hook () { tt_send ("\e[?1000l"); }
define exit_hook () { tt_send ("\e[?1000l"); exit_jed (); }
define resume_hook () { tt_send ("\e[?1000h"); }
#else				% X10 mouse reporting
define suspend_hook () { tt_send ("\e[?9l"); }
define exit_hook () { tt_send ("\e[?9l"); exit_jed (); }
define resume_hook () { tt_send ("\e[?9h"); }
#endif
resume_hook ();			% enable mouse


% a hook into mouse-based menus (someday)
define mousex_menu (x)
{
   menu_main_cmds ();		% make it simple for now
}

define mousex_report (status)
{
   Sprintf ("Button <%d>: ", Mouse_Button, 1);
   if (status)
     "Status line";
   else
     Sprintf ("col,row = %d,%d  Top,Rows = %d,%d",
	      Mouse_X, Mouse_Y, window_info('t'), window_info('r'), 4);
   flush (strcat ());
}

% missed release -- don't redefine
define mousex_3 (status) {}

% modifiers: 's' = Shift (4), 'm' = Meta (8), 'c' = Control (16)
define mouse_event (Mod)
{
   variable n, status = 0;

   n = getkey () - 040;		% button number + Modifiers
   Mouse_X = getkey () - 040;		% Column
   Mouse_Y = getkey () - 040;		% Row

   % NB: xterm and rxvt use Shift and Meta to override mouse reporting
   % and xterm use Ctrl for popup menus.
   if ((n & 3) == 3)		% release event
     {
	Mouse_Event_Type = 0;	% use Mouse_Button from last ButtonPress
     }
   else
     {
	Mouse_Event_Type = 1;
	Mouse_Button = (n & 3);
	switch (Mod)		% convert to logical masks
	  { case 'c': 16}	% fake Ctrl+MouseButton
	  { case 'm': 8 }	% fake Meta+MouseButton
	  { case 's': 4 }	% fake Shift+MouseButton
	  { pop ();   0 }	% unadulterated
	Mod = ();
	Mouse_Button = Mouse_Button | Mod;
     }

   % error (Sprintf ("Button <%d>: x,y: %d,%d", Mouse_Button, x, y, 3));

   n = nwindows ();
   while (n)
     {
	variable top = window_info('t');
	variable bot = window_info('r') + top;

	if (Mouse_Y == bot)
	  {
	     status = 1;
	     break;
	  }
	else if ((Mouse_Y >= top) and (Mouse_Y < bot))
	  {
	     Mouse_Y += 1 + what_line () - (top + window_line());
	     break;
	  }
	otherwindow ();
	n--;
     }

   !if (n)
     {
	if (Mouse_Y == 1)		% Mouse on top status line
	  {
	     if (Mouse_Event_Type)
	       mousex_menu (Mouse_Event_Type, Mouse_X);
	     return;
	  }
	Mouse_Button = 3;
	!if (Mouse_Event_Type) error ("Mouse not in a window");
	emacs_escape_x ();
	return;
     }

   loop (n) otherwindow ();
   Mouse_X += window_info('c') - 1;
   n = nwindows () - n;
   loop (n) otherwindow ();

   if (not(Mouse_Event_Type) or status)
     EXIT_BLOCK { Mouse_Button = 3; }

   variable fn = Sprintf ("mousex_%d", Mouse_Button, 1);

   !if (Mouse_Event_Type) {
      if (n) return;		% must be the same window
      fn = strcat (fn, "U");
   }

   if (is_defined (fn) > 0)
     {
	status;
	eval(strcat (".", fn));	% Use RPN--- no need to parse it
     }
   else
     {
	if (Mouse_Event_Type)
	  mousex_report (status);	% Unbound ... just report
     }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% service functions
define mouse_next_buffer ()
{
   variable n, buf, cbuf = whatbuf ();

   n = buffer_list ();		% buffers on stack
   loop (n)
     {
	buf = ();
	n--;
	if (buf[0] == ' ') continue;
	if (buffer_visible (buf)) continue;
	sw2buf (buf);
	pop_n (n);
	return;
     }
   error ("All buffers are visible.");
}

% move to a new x, y location with/without extending the region
define mouse_goto (moveto)
{
   if (moveto)
     pop_mark_0 ();
   else !if (is_visible_mark ())
     push_visible_mark ();

   if (Mouse_Y == what_line ())
     {
	variable col = what_column ();	% deal with going past end-of-line
	if (col == goto_column_best_try (Mouse_X))  % didn't move
	  pop_mark (0);
     }
   else
     {
	goto_line (Mouse_Y);
	() = goto_column_best_try (Mouse_X);
     }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% normal mouse event bindings, redefine as desired.
%
% mouse_?, mouse_?U (`U' means UP -- Button Release)
%   `?'	= 0-2	= None  + l/m/r Button
%	= 4-6	= Shift + l/m/r Button
%	= 8-10	= Meta  + l/m/r Button
%	= 16-18	= Ctrl  + l/m/r Button

% Left
define mousex_0 (status)
{
   if (status)
     mouse_next_buffer ();
   else
     mouse_goto (1);
}

% Left-Release
define mousex_0U (status)
{
   !if (status) mouse_goto (0);
}

% Middle
define mousex_1 (status)
{
   if (status)
     splitwindow ();
   else if (markp ())
     {
	call ("copy_region");
	flush ("region copied");
     }
   else
     call ("yank");
}

% Right
define mousex_2 (status)
{
   if (status)
     call ("delete_window");
   else
     mouse_goto (0);
}

% Ctrl-Middle
define mousex_17 (status)
{
   if (status)
     return;
   else if (markp ())
     {
	call ("kill_region");
	flush ("region killed");
     }
}

% Other possible bindings
#iftrue
% Ctrl-Left
define mousex_16 (status)
{
   if (status)
     enable_top_status_line (1);
   else
     {
	goto_line (Mouse_Y);
	recenter (0);
     }
}

% Ctrl-Right
define mousex_18 (status)
{
   if (status)
     enable_top_status_line (0);
   else
     mousex_report (status);
}
#endif
