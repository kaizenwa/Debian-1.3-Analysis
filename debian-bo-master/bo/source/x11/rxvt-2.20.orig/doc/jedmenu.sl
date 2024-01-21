%--------------------------------*-SLang-*--------------------------------
% An example of using the rxvt menuBar for the JED editor
#if$TERM xterm*
define toggle_menu () { tt_send ("\e[?10t"); }
define menucmd (str) { tt_send (Sprintf ("\e]10;%s\a", str, 1)); }
define menu_clear () { tt_send ("\e]10;-/\a\e]10;=\a"); }

define menu_title (str) { menucmd (strcat ("=", str)); }

define menu (str) { menucmd (strcat ("+", str)); }
define rm_menu (str) { menucmd (strcat ("-", str)); }

define menuitem (str,ev) { menucmd (Sprintf ("+{%s} %s", str, ev, 2)); }
define rm_menuitem (str) { menucmd (Sprintf ("-{%s}", str, 1)); }

define menuFn ()
{
   variable ch, cmd;

   cmd = Null_String;
   forever
     {
	ch = getkey ();
	if (ch == '\r') break;
	cmd = strcat (cmd, char (ch));
     }
   eval (cmd);
}
local_setkey ("menuFn", "\e[m");	% menu

define menu_eval (str,ev) { menuitem (str, strncat ("\\e[m", ev, "\\r", 3)); }
define menukey (str, key) { menucmd (Sprintf ("+{%s}{%s}", str, key, 2)); }

define separator () { menucmd ("+{-}"); }
define rm_separator () { menucmd ("-{-}"); }

% no other reasonable way to stack hooks?
!if (is_defined ("resume_esc")) variable resume_esc = Null_String;
!if (is_defined ("suspend_esc")) variable suspend_esc = Null_String;

% enable menus and permit 80/132 column toggle
resume_esc = strcat (resume_esc, "\e[?10;40h");
suspend_esc = strcat (suspend_esc, "\e[?10;40l");

define resume_hook () { tt_send (resume_esc); }
define suspend_hook ()  { tt_send (suspend_esc); }
define exit_hook () { suspend_hook (); menu_clear (); exit_jed (); }

%-------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% possibly useful things for the JED editor -- assuming Emacs bindings
menu_clear ();		% clear menus before construction

% format _jed_version xyyzz	into x.yy-zz
menu_title (Sprintf ("Jed %d.%d-%d (rxvt-%%v)",
		     (_jed_version/10000),
		     ((_jed_version mod 10000)/100),
		     (_jed_version mod 100),
		     3));

% some convenient arrows
menucmd ("<b>\\E[m<l>bskip_word<u>backward_paragraph<d>forward_paragraph<r>skip_word<e>\\r");

menu ("/File");
menukey ("Open",	"^X^F");
menukey ("Save",	"^X^W");
menukey ("Save Buffers","^Xs");
menukey ("Insert File",	"^Xi");
separator ();
menukey ("Shell Cmd",	"M-!");
separator ();
menukey ("Exit",	"^X^C");

menu ("/Edit");
menukey ("Undo",	"^_");
% menukey ("Redo",	"^G^_");
separator ();
menukey ("Cut",		"^W");
menukey ("Copy",	"M-W");
menukey ("Paste",       "^Y");

menu ("/Search");
menukey ("Forward",	"^S");
menukey ("Backward",	"^R");
menukey ("Replace",	"M-%");
separator ();
menu ("Regexp");
menukey ("Forward",	"M-^S");
menukey ("Backward",	"M-^R");
menu_eval ("Replace",	"query_replace_match");
menu ("../../");

menu ("Buffers");
menukey ("Kill",	"^Xk");
menukey ("List",	"^X^B");
menukey ("Switch",	"^Xb");
separator ();
menu ("Modes");
menu_eval ("C",	"c_mode");
menu_eval ("SLang",	"slang_mode");
menu_eval ("None",	"no_mode");
menu_eval ("LaTeX",	"latex_mode");
menu_eval ("Text",	"text_mode");
menu_eval ("Fortran",	"fortran_mode");
menu ("../../");

menu ("/Window");
menukey ("Delete",	"^X0");
menukey ("One",		"^X1");
menukey ("Split",	"^X2");
menukey ("Other",	"^Xo");
separator ();
menukey ("Recenter",	"^L");
separator ();
menu ("Color Schemes");
menu_eval ("White-on-Black",	"set_color_scheme(\"white-on-black\")");
menu_eval ("Black-on-White",	"set_color_scheme(\"black-on-white\")");
menu_eval ("White-on-default-Black","set_color_scheme(\"white-on-default-black\")");
menu_eval ("Black-on-default-White","set_color_scheme(\"black-on-default-white\")");
menu ("../");
menu ("../");

menu ("/Utils");
menu_eval ("Bufed",	"bufed");
menu_eval ("Dired",	"dired");
menu_eval ("Mail",	"mail");
menu_eval ("Rmail",	"rmail");
separator ();
menu_eval ("EvalBuffer",	"evalbuffer");
menu_eval ("Trim-Buffer",	"trim_buffer");
menu ("../");

menu ("/Terminal/Font");
menuitem ("Normal",	"^@\\E]50;#\\a");
menuitem ("Larger",	"^@\\E]50;#+\\a");
menuitem ("Smaller",	"^@\\E]50;#-\\a");
separator ();
menuitem ("Font 1",	"^@\\E]50;#1\\a");
menuitem ("Font 2",	"^@\\E]50;#2\\a");
menuitem ("Font 3",	"^@\\E]50;#3\\a");
menuitem ("Font 4",	"^@\\E]50;#4\\a");
menu ("../Screen");
menuitem ("Reverse Video",	"^@\\E[?5t");
menuitem ("Toggle Width",	"^@\\E[?3t");
menuitem ("Cursor",		"^@\\E[?25t");
menuitem ("Switch",		"^@\\E[?47t");
separator ();
menuitem ("menuBar",	"^@\\E[?10t");
menuitem ("scrollBar",	"^@\\E[?30t");
menu	 ("../Keys");
menuitem ("XTerm",	"^@\\E[?35t");
menuitem ("Backspace",	"^@\\E[?36t");
menuitem ("Cursor",	"^@\\E[?1t");
menuitem ("KeyPad",	"^@\\E[?66t");
menu	 ("../");
#if$COLORTERM rxvt-xpm
separator ();
menu ("Background/Pixmap");
menuitem ("None",	"^@\\E]20;\\a");
separator ();
% load .xpm files
menuitem ("Wingdogs",	"^@\\E]20;wingdogs\\a");
menuitem ("Mona",	"^@\\E]20;mona\\a");

menu ("../Scaling");
menuitem ("Full",	"^@\\E]20;;100;?\\a");
menuitem ("Tiled",	"^@\\E]20;;0\\a");
separator ();
menuitem ("400/25%",	"^@\\E]10;<b>\\^@\\\\E]20;;<l>400x0<r>25x0<u>0x400<d>0x25<e>;?\\^G\\a");
menuitem ("250/40%",	"^@\\E]10;<b>\\^@\\\\E]20;;<l>250x0<r>40x0<u>0x250<d>0x40<e>;?\\^G\\a");
menuitem ("200/50%",	"^@\\E]10;<b>\\^@\\\\E]20;;<l>200x0<r>50x0<u>0x200<d>0x50<e>;?\\^G\\a");
menuitem ("125/80%",	"^@\\E]10;<b>\\^@\\\\E]20;;<l>125x0<r>80x0<u>0x125<d>0x80<e>;?\\^G\\a");
menu ("../Position");
menuitem ("5%}{5%",	"^@\\E]10;<b>\\^@\\\\E]20;;<l>-5+0<r>+5+0<u>+0-5<d>+0+5<e>;?\\^G\\a");
menuitem ("10%}{10%",	"^@\\E]10;<b>\\^@\\\\E]20;;<l>-10+0<r>+10+0<u>+0-10<d>+0+10<e>;?\\^G\\a");
menuitem ("centre",	"^@\\E]20;;=+50+50;?\\a");
menu ("../../");
#endif	% COLORTERM
separator ();
menuitem ("Version",	"^@^[[8n");

menu ("/?");
menukey ("Info",	"^X?i");
menukey ("Man",		"^X?m");
separator ();
menukey ("Apropos",	"^X?a");
menukey ("Show Key",	"^X?k");
menukey ("Where Is",	"^X?w");
menu ("/");

resume_hook ();			% enable menu, mouse, etc
#endif	% xterm*
%%%%%%%%%%%%%%%%%%%%%%%%%%% end-of-file (SLang) %%%%%%%%%%%%%%%%%%%%%%%%%%
