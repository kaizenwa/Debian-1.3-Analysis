% bytecomp.sl	-*- SLang -*-
%
% !! WARNING: It is not a good idea to invoke this file directly.
%             Instead load it via preparse.sl.  This may be performed
%             in batch mode as:
%
%      jed -batch -n -l preparse
%
%

!if (is_defined ("Preprocess_Only"))
{
   variable Preprocess_Only = 0;
}

define jed_byte_compile_file (f)
{
   variable file;

   file = expand_jedlib_file(f);
   if (strlen (file))
     {
	flush (strcat ("Processing ", file));
	byte_compile_file (file, Preprocess_Only);
     }
   else flush (strcat (f, " not found"));
}

!if (is_defined ("__load__bytecomp__only__"))
{
   $0 = _stkdepth ();

   % list of file to byte compile:
#ifdef UNIX VMS
   "mail.sl"; 
   "iso-latin.sl";
#endif
#ifdef UNIX
   "rmail.sl";
   "sendmail.sl";
   "mailalias.sl";
   "mime.sl";
#endif
#ifdef MSDOS OS2
   "dos437.sl"; "dos850.sl";  "brief.sl";
#endif
#ifdef XWINDOWS MOUSE
   "mouse.sl";
#endif

#ifdef HAS_LINE_ATTR
   "folding.sl";
#endif
   
   if (is_defined ("KILL_ARRAY_SIZE")) 
     {
	"yankpop.sl";
	"register.sl";
     }
   "wmark.sl";
   "occur.sl";
   "javamode.sl";
   "modehook.sl";
   "nroff.sl";
   "html.sl";
   "idl.sl";
   "shmode.sl";
   "mousex.sl";
   "abbrev.sl";
   "abbrmisc.sl";
   "dabbrev.sl";
   "mutekeys.sl";
   "bookmark.sl";
   "replace.sl";
   "srchmisc.sl";
   "tex.sl";
   "bibtex.sl";
   "latex.sl";
   "latex209.sl";
   "ltx-math.sl";
   "binary.sl";
   "isearch.sl";
   "rot13.sl";
   "tabs.sl";
   "untab.sl";
   "jedhelp.sl";
   "ctags.sl";
   "compile.sl";
   "menu.sl";
   "dired.sl";
   "util.sl";
   "syntax.sl";
   "tmisc.sl";
   "cmisc.sl";
   "misc.sl";
   "help.sl";
   "cal.sl";
   "man.sl";
   "fortran.sl";
   "dcl.sl";
   "shell.sl";
   "most.sl";
   "info.sl";
   "ispell.sl";
   "sort.sl";
   "regexp.sl";
   "wordstar.sl";
   "buf.sl";
   "emacsmsc.sl";
   "cmode.sl";
   "slmode.sl";
   "search.sl";
   "linux.sl";
   "mini.sl";
   "edt.sl";
   "emacs.sl";
   "site.sl";
   
   $0 = _stkdepth () - $0;
   loop ($0)
     {
	jed_byte_compile_file (());
     }
   if (BATCH) exit_jed();
};

