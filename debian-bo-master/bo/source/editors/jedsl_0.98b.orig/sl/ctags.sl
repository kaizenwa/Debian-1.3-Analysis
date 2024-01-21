% tags.sl	-*- SLang -*-
%
% read a tags file produced by ctags/etags programs
%
% By default, the file "tags" is used.  However, setting the variable
% `Tag_File' as in
%	global_variable Tag_File = "mytag.file";
% will override this.

!if (is_defined ("Tag_File"))
{
   variable Tag_File = "tags";
}

% ctags format:
%  function-name\tfilename\t/^function-prototype/
%  typedef-name\tfilename\tline-number
define ctags_find (tag)
{
   variable n, file, proto, msg = "Tag file needs updated?";

   !if ((n = re_fsearch (strncat ("^", tag, "\t+\\([^\t]+\\)\t+", 3))), n)
     error (msg);
   file = regexp_nth_match (1);

   go_right (--n, n);
   if (looking_at ("/^"))
     {
	go_right (2);
	push_mark (); eol (); bskip_chars ("\\\\$/");
	proto = str_replace_all (bufsubstr (), "\\/", "/");
	n = 0;
     }
   else
     {
	push_mark ();
	eol ();
	n = integer (bufsubstr ());
     }

   !if (read_file (file)) error ("File not found.");

   if (n)
     {
	goto_line (n);
	return;
     }

   bob ();
   !if (bol_fsearch (proto))
     {
        () = fsearch (tag);
        message (msg);
     }
   % message (Sprintf ("Tag: <%s>", proto, 1));
}

% etags format:
%  ^L
%  filename,some-number
%  [function-type] function-name ^?line-name,some-number
define etags_find (tag)
{
   variable file, line, msg = "Tag file needs updated?";

   !if (re_fsearch (strcat (tag, "[\t ]+\x7F\\(\\d+\\),")))
     error (msg);
   line = integer (regexp_nth_match (1));

   () = bol_bsearch (char (014));	% previous ^L
   go_down_1 ();
   push_mark (); skip_chars ("^,");
   file = bufsubstr ();

   !if (read_file (file)) error ("File not found.");
   goto_line (line);
}

define find_tag ()
{
   variable tag = "0-9A-Z_a-z", tbuf = " *tags*", cbuf = whatbuf ();
#ifdef VMS
   tag = strcat (tag, "$");
#endif
   push_spot ();
   skip_white ();
   bskip_chars (tag);
   push_mark ();
   skip_chars (tag);
   tag = bufsubstr ();		% leave on the stack
   pop_spot ();

   tag = strtrim (read_mini ("Find tag:", tag, Null_String));

   !if (strlen (tag)) return;	% later I will treat this better
   !if (bufferp (tbuf), setbuf (tbuf))
     {
        if (insert_file (Tag_File) < 0)
          error ("File tags not found!");
     }

   bob ();
   if (looking_at_char (014))	% if first char is ^L (etags)
     etags_find (tag);
   else
     ctags_find (tag);
   pop2buf (whatbuf ());
   pop2buf (cbuf);
}
