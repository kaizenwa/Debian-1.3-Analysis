%
% Info reader for JED
%

variable Info_This_Filename = Null_String;
variable Info_This_Filedir = Null_String;

#ifndef VMS
% returns compression extension if file is compressed or "" if not 
define info_is_compressed (file)
{
   variable exts, ext, n;
   exts = ".Z,.z,.gz";
   n = 0;
   while (ext = extract_element(exts, n, ','), strlen(ext))
     {
	if (1 == file_status(strcat(file, ext))) break;
	n++;
     }
   ext;
}
#endif


define info_make_file_name (file)
{
   variable n, dir, dirfile, df, df_low;
#ifndef VMS
   variable cext;    % compressed extension
   cext = Null_String;
#endif
   n = 0;
   forever 
     {
        %
        % Try to find requested file in remembered directory.
        %
	dirfile = expand_filename(dircat(Info_This_Filedir, file));
	if (1 == file_status(dirfile)) break;
	
	dir = extract_element(Info_Directory, n, ',');
	df = expand_filename(dircat(dir,file));

	% try with first with info extension
#ifdef VMS
	dirfile = strcat (df, "info");  % VMS adds a '.' upon expansion
#else
	dirfile = strcat (df, ".info");
#endif

	
	if (1 == file_status(dirfile)) break;
#ifndef VMS
	cext = info_is_compressed(dirfile);
	if (strlen(cext)) break;
#endif
	df_low = expand_filename(dircat(dir,strlow(file)));
	
#ifdef VMS
	dirfile = strcat (df_low, "info");  % VMS adds a '.' upon expansion
#else
	dirfile = strcat (df_low, ".info");
#endif
	
	if (1 == file_status(dirfile)) break;
#ifndef VMS
	cext = info_is_compressed(dirfile);
	if (strlen(cext)) break;
#endif

#ifndef MSDOS
 	% try next with inf extension, since .info causes problems on FAT
	% In addition, Unix and VMS distributions may have been derived from
	% PC 8+3 distributions.
 	dirfile = strcat (df_low, ".inf");
	
 	if (1 == file_status(dirfile)) break;
#endif % Not MSDOS
#ifndef VMS
 	cext = info_is_compressed(dirfile);
 	if (strlen(cext)) break;
#endif
	
% repeat without extension
	
	dirfile = df;
	
	if (1 == file_status(dirfile)) break;
#ifndef VMS
	cext = info_is_compressed(dirfile);
	if (strlen(cext)) break;
#endif
	dirfile = df_low;
	if (1 == file_status(dirfile)) break;
#ifdef UNIX
	cext = info_is_compressed(dirfile);
	if (strlen(cext)) break;
#endif

	!if (strlen(dir)) error (strcat("Info file not found: ", file));
	++n;
     }
   
   (Info_This_Filedir, ) = parse_filename(dirfile);
   
#ifndef VMS
   cext;
#endif
   dirfile;
}


define info_find_file (file)
{
   variable dirfile, flags, buf, dir;
#ifndef VMS
   variable ext;
#endif
  
   dirfile = info_make_file_name(file);
#ifndef VMS
   =ext;
#endif
   
   setbuf("*Info*");
   set_readonly(0);
   widen(); erase_buffer();

#ifndef VMS
   if (strlen(ext))
     {
	% use zcat to uncompress it
# ifdef UNIX
	run_shell_cmd(Sprintf("zcat -f %s%s", dirfile, ext, 2));
# else
 	run_shell_cmd(Sprintf("gzip -dc %s%s", dirfile, ext, 2));
# endif
     }
   else
#endif
   insert_file(dirfile);
   
   pop(); 			       %  This also pops the return value of 
   				       %  run_shell_cmd
   
   bob();
   Info_This_Filename = dirfile;
   set_readonly(1);
   set_buffer_modified_flag(0);
   set_mode("Info", 1);
   use_keymap("Infomap");
   set_status_line(" Jed Info: %f   (%m%n)   Press '?' for help.    (%p)", 0);
   ( , dir, buf, flags) = getbuf_info();
   setbuf_info(extract_filename(Info_This_Filename), dir, buf, flags);
   % if (strcmp(whatbuf(), "*Info*")) error ("Not info!! in find file");
}


define info_find_node_split_file();  % extern

variable Info_Split_File_Buffer;  Info_Split_File_Buffer = Null_String;
variable Info_Split_Filename;  Info_Split_Filename = Null_String;

define info_search_marker(dir)
{
   variable mark, pnt, search_fun;
   mark = "\x1F";
   if (dir > 0) search_fun = &fsearch; else search_fun = &bsearch;
   push_mark();
   forever 
     {
	if (not(search_fun(mark)))
	  {
	     pop_mark_1 ();
	     return(0);
	  }
	if (bolp()) break;
	pnt = POINT;
	bol(); skip_chars("\x01-\x1E ");
	go_right_1 ();
	pnt = POINT - pnt;
	if ((pnt == 1) and (eolp() or looking_at_char('\xC'))) break;
	if (dir > 0) eol(); else bol();
     }
   pop_mark_0 ();
   return (1);
}

define info_find_node_this_file (the_node)
{
   variable node, len, fnd;
   CASE_SEARCH = 0;
   node = strcat("Node: ", the_node);
   len = strlen(node);
   widen(); bob();
   forever
     {
	% some of this could/should be replaced by a regular expression:
	% !if (re_fsearch("^[\t ]*\x1F")) ....
	
	!if (info_search_marker(1))
	  {
	     % dont give up, maybe this is a split file
	     !if (strlen(Info_Split_File_Buffer)) 
	     error(strcat ("Marker not found. ", node));
	     setbuf(Info_Split_File_Buffer);
	     info_find_node_split_file(the_node);
	     return;
	  }
	go_down_1 (); % bol();  --- already implicit
	if (ffind(node))
	  {
	     % is this really it?  ---
	     go_right(len);
	     if (eolp() or looking_at_char(',') or looking_at_char('\t')) break;
	  }
	
	eol ();
     }
   
   push_mark();
   if (info_search_marker(1)) go_up_1(); else eob();
   narrow();
   bob();
}


define info_find_node_split_file (node)
{
   variable tag, tagpos, pos, pos_len, tag_len, buf, file;
   variable re;
   buf = " *Info*";
  
   !if (bufferp(buf), setbuf(buf)) 
     {
	insbuf("*Info*");
     }
   
   widen();
      
   % make this re safe 
   tag = str_quote_string (node, "\\^$[]*.+?", '\\');
   
   tag = strcat("Node: ", tag);
   eob();
  
   
   %!if (bol_bsearch(tag)) error("tag not found.");
   %go_right(strlen(tag));
   %skip_chars(" \t\x7F");
   
   re = strcat(tag, "[\t \x7F]\\d+[ \t]*$");
   
   !if (re_bsearch(re)) error ("tag not found");
   eol ();
   bskip_chars(" \t");
   push_mark(); bskip_chars ("0-9");
   tagpos = bufsubstr();  % see comment about DOS below
   tag_len = strlen(tagpos);
  
   bob ();
   bol_fsearch("Indirect:"); pop();
   push_mark();
   !if (info_search_marker(1)) eob();
   narrow();
   bob();
   forever
     {
	!if (down_1 ()) break;
	% bol(); --- implicit in down
	!if (ffind(": ")) break;
	go_right(2);
	
	% This will not work on DOS with 16 bit ints.  Do strcmp instead.
	push_mark_eol(); pos = bufsubstr(); 
	pos_len = strlen(pos);
	if (tag_len > pos_len) continue;
	if (tag_len < pos_len) break;
	% now ==
	if (strcmp(tagpos, pos) < 0) break;
     }
   
   Info_Split_File_Buffer = Null_String;
   go_up_1 (); bol();
   push_mark();
   () = ffind(": ");
   widen();
   file = bufsubstr();

   info_find_file(file);
   info_find_node_this_file(node);
   Info_Split_File_Buffer = buf;
}



define info_narrow()
{
   if (strcmp(whatbuf(), "*Info*")) return;
   push_spot();  push_spot();
   () = info_search_marker(-1);
   go_down_1 (); push_mark();
   pop_spot();
   if (info_search_marker(1)) go_up_1 (); else eob();
   narrow();
   pop_spot();
}


  % stack for last position 

!if (is_defined("Info_Stack_Line"))
{
   variable Info_Stack_Depth; Info_Stack_Depth = 0;
   variable Info_Stack_File, Info_Stack_Line;
   Info_Stack_File = create_array('s', 16, 2, 2);
   Info_Stack_Line = create_array('i', 16, 1);
}


define info_push_position(file, split, line)
{
   variable i, j;
   
   if (Info_Stack_Depth == 16)
     {
        --Info_Stack_Depth;
	for (i = 1; i < 16; i++)
	  { 
	     j = i - 1;
	     Info_Stack_File[j, 0] = Info_Stack_File[i, 0];
	     Info_Stack_File[j, 1] = Info_Stack_File[i, 1];
	     Info_Stack_Line[j] = Info_Stack_Line[i];
	  }
     }
   
   Info_Stack_File[Info_Stack_Depth, 0] = file;
   Info_Stack_File[Info_Stack_Depth, 1] = split;
   Info_Stack_Line[Info_Stack_Depth] = line;
   ++Info_Stack_Depth;
}


define info_record_position ()
{
   variable i, file;
  
   if (strcmp(whatbuf(), "*Info*")) return;
   widen();
   file = Null_String;
   
   if (strlen (Info_Split_File_Buffer)) file = Info_Split_Filename;
   info_push_position(Info_This_Filename, file, what_line());
   info_narrow();
}




define info_find_node(node)
{
   variable the_node, file, n, len;
   n = 0;
  
   info_record_position();
   ERROR_BLOCK 
     {
	info_mode ();
     }
   
   len = strlen(node);
  % if it looks like (file)node, extract file, node
  
   if (is_substr(node, "(") == 1) n = is_substr(node, ")");
  
   if (n)
     {
	the_node = node;
	node = substr(the_node, n + 1, strlen(node));
	the_node = strsub(the_node, n, 0);  % truncate string
	file = substr(the_node, 2, n);
	if (bufferp(Info_Split_File_Buffer)) delbuf(Info_Split_File_Buffer);
	Info_Split_File_Buffer = Null_String;
	info_find_file(file);
     }
   
   !if (strlen(node)) node = "Top";
   widen();
   push_spot_bob ();
   !if (info_search_marker(1)) error("Marker not found.");
   go_down_1 ();
  
   if (looking_at("Indirect:"), pop_spot())
     {
	Info_Split_Filename = Info_This_Filename;
	info_find_node_split_file(node);
     }
   else info_find_node_this_file(node);
}



% If buffer has a menu, point is put on line after menu marker if argument
% is non-zero, otherwise leave point as is.
% signals error if no menu.
define info_find_menu(save)
{
   variable menu;
   menu = "* Menu:";
   push_spot(); eob();

   !if (bol_bsearch(menu))
     {
	pop_spot();
	error ("Node has no menu.");
     } 
   !if (save) 
     {
	pop_spot();
	return;
     }
   
   eol(); go_right_1 ();
   push_mark(); pop_spot(); pop_mark_1 ();
}



% Move move the cursor to the start of the next nearest menu item or
% note reference in this node if possible.
%
define info_next_xref ()
{
   push_mark (); go_right_1 ();
   if (re_fsearch("[*].*:")) exchange_point_and_mark ();
   pop_mark_1 ();
}
%
% Move move the cursor to the start of the previous nearest menu item or
% note reference in this node if possible.
%
define info_prev_xref ()
{
   push_mark (); go_left_1 ();
   if (re_bsearch("[*].*:")) exchange_point_and_mark ();
   pop_mark_1 ();
}


% menu references
define info_menu ()
{
   variable node, colons, colon, menu;
   node = Null_String;
   colon = ":"; colons = "::";
   menu = "* Menu:";
  
   info_find_menu(0);
  
   push_spot_bol ();
   if (looking_at("* ") and ffind(colon))
     {
	push_mark();
	bol(); go_right(2);
	node = strcat (bufsubstr(), colon);
     }
   
   pop_spot();
   !if (strlen (node) and (LAST_CHAR == '\r'))
      node = read_mini("Menu item:", node, Null_String);

   eob();
   bol_bsearch(menu); pop();
  
   !if (bol_fsearch(strcat("* ", node))) error ("Menu Item not found.");
   !if (ffind(colon)) error ("Corrupt File?");
   if (looking_at(colons))
     {
	push_mark();
	bol(); go_right(2);
     }
   else
     {
        go_right_1 ();
        skip_white();
        push_mark();
	if (looking_at_char('('))
	  {
	     () = ffind_char (')');
	  }
	% comma, tab, '.', or newline terminates
	skip_chars("\d032-\d043\d045\d047-\d255"); 
	 
        bskip_chars(" ");
     }
   info_find_node(bufsubstr(()));
}

define info_find_dir() 
{
   info_find_node ("(DIR)top");
}



define info_up ()
{   
   bob();
   !if (ffind("Up: ")) 
     {
	info_find_dir ();
	return;
     }
   
   go_right(4); push_mark();
   % comma, tab, or newline terminates
   skip_chars("\d032-\d043\d045-\d255");
   bskip_chars(" ");
   info_find_node(bufsubstr(()));
}

define info_prev()
{
   variable n;  n = 10;
   bob();
   !if (ffind("Previous: "))
     {
	!if (ffind("Prev: ")) error ("Node has no PREVIOUS");
	n = 6;
     }
   
   go_right(n); push_mark();
   skip_chars("\d032-\d043\d045-\d255");
   bskip_chars(" ");
   info_find_node(bufsubstr(()));
}

  
define info_next ()
{   
   bob();
   !if (ffind("Next: ")) error ("Node has no NEXT.");
   go_right(6); push_mark();
   % comma, tab, or newline terminates
   skip_chars("\d032-\d043\d045-\d255");
   bskip_chars(" ");
   info_find_node(bufsubstr(()));
}
  
define info_quick_help()
{
  message("q:quit,  h:tutorial,  SPC:next screen,  DEL:prev screen,  m:menu,  s:search");
}

  
$2 = "Infomap";
!if (keymap_p($2))
{
   make_keymap($2);
   definekey("info_quick_help",		"?", $2);
   definekey("info_tutorial",		"h", $2);
   definekey("info_tutorial",		"H", $2);
   definekey("info_menu",		"^M", $2);
   definekey("info_menu",		"M", $2);
   definekey("info_menu",		"m", $2);
   
   definekey("info_next_xref",		"^I", $2);
#ifdef MSDOS
   definekey("info_prev_xref",		"^@^O", $2);
#endif
   
   definekey("info_next",		"N", $2);
   definekey("info_next",		"n", $2);
   definekey("info_prev",		"P", $2);
   definekey("info_prev",		"p", $2);
   definekey("info_up",			"U", $2);
   definekey("info_up",			"u", $2);
   definekey("page_down",		" ", $2);
   definekey("page_up",			"^?", $2);
   definekey("bob",			"B",  $2);
   definekey("bob",			"b",  $2);
   definekey("info_goto_node",		"G", $2);
   definekey("info_goto_node",		"g", $2);
   definekey("info_quit",		"q",  $2);
   definekey("info_quit",		"Q",  $2);
   definekey("info_goto_last_position",	"l",  $2);
   definekey("info_goto_last_position",	"L",  $2);
   definekey("info_search",		"S",  $2);
   definekey("info_search",		"s",  $2);
   definekey("info_search",		"/",  $2);
   definekey("info_follow_reference",	"f",  $2);
   definekey("info_follow_reference",	"F",  $2);
   definekey("info_find_dir",		"D",  $2);
   definekey("info_find_dir",		"d",  $2);
   _for (1, 9, 1)
     {
	=$1;
	definekey("info_menu_number", string($1), $2);
     } 
}

define info_goto_last_position()
{
   variable split_file, file, n, i;
   
   if (Info_Stack_Depth == 0) return;
  
   --Info_Stack_Depth;
   i = Info_Stack_Depth;

   split_file = Info_Stack_File[i, 1];
   file = Info_Stack_File[i, 0];
   n = Info_Stack_Line[i];
  
   if (not(strcmp(file, Info_This_Filename)) and bufferp("*Info*"))
     {
        widen();
        goto_line(n); bol();
        info_narrow();
        return;
     }
    
   if (strlen(split_file))
     {
	setbuf(" *Info*");
	set_readonly(0);
	widen();
	erase_buffer();
#ifndef VMS
 	variable ext = info_is_compressed(split_file);
 	if (strlen(ext))
 	  {
	     % use zcat to uncompress it
#ifdef UNIX
 	     run_shell_cmd(Sprintf("zcat -f %s%s", split_file, ext, 2));
#else
 	     run_shell_cmd(Sprintf("gzip -dc %s%s", split_file, ext, 2));
#endif
 	  }
 	else
#endif
	insert_file (split_file); 
	
	pop ();
	setbuf ("*Info*");
     } 
    
   !if (strlen(file)) return;
   info_find_file(file);
   goto_line(n); bol();
   info_narrow();
}

define info_mode ()
{
   variable ibuf; ibuf = "*Info*";
   if (Info_Stack_Depth) info_goto_last_position ();
   !if (bufferp(ibuf)) info_find_dir();
   pop2buf(ibuf);
   onewindow();
   runhooks ("info_mode_hook");
}


define info_quit ()
{
   info_record_position();
   widen();
   delbuf("*Info*");
}


define info_goto_node()
{
   info_find_node (read_mini("Node:", Null_String, Null_String));
}


define info_search ()
{
   variable this_line, this_file, str, err_str, file, wline, ifile, ext;
   err_str = "String not found.";
    
   str = read_mini("Re-Search:", LAST_SEARCH, Null_String);
   !if (strlen(str)) return;
   save_search_string(str);
   widen(); go_right_1 (); 
   if (re_fsearch(str)) 
     {
	info_narrow();
	return;
     }
   
   %
   %  Not found.  Look to see if this is split.
   %
   !if (strlen(Info_Split_File_Buffer))
     {
	info_narrow();
	error (err_str);
     }
   
   this_file = Info_This_Filename;
   this_line = what_line();
   wline = window_line(); %need this so state can be restored after a failure.
  
  
   setbuf(Info_Split_File_Buffer); widen(); bob();
   bol_fsearch("Indirect:"); pop();
   push_mark();
   if (info_search_marker(1)) go_up_1 (); else eob();
   narrow();
   bob();
   bol_fsearch(extract_filename(this_file)); pop();
  
   while (down_1 ())
     {
	% bol(); --- implicit
	push_mark();
	
	!if (ffind_char (':')) {pop_mark_0 ();  break; } 
	file = bufsubstr();
	flush(strcat("Searching ", file));
	ifile = info_make_file_name(file);
#ifdef UNIX OS2
	=ext;
	if (strlen(ext))
	  {
	     setbuf(" *Info*zcat*"); erase_buffer();
#ifdef UNIX
  	     () = run_shell_cmd(Sprintf("zcat -f %s%s | grep -ci '%s'", ifile, ext, str, 3));
#else
 	     () = run_shell_cmd(Sprintf("gzip -dc %s%s | grep -ci '%s'", ifile, ext, str, 3));
#endif
	     bob();
	     if (looking_at_char ('0'))
	       {
		  delbuf(whatbuf());
		  setbuf(Info_Split_File_Buffer);
		  continue;
	       }
	     setbuf(Info_Split_File_Buffer);
	  }
	else
#endif
	!if (search_file(ifile, str, 1))
	  {
	     setbuf(Info_Split_File_Buffer);
	     continue;
	  }
			 
	info_find_file(file);
	pop(fsearch(str));
	info_narrow();
	info_push_position(this_file, Info_Split_Filename, this_line);
	return;
     }
  
   widen();
   info_find_file (this_file);
   goto_line(this_line); eol();
   info_narrow();
   recenter(wline);
   error (err_str);
}

define info_looking_at (ref)
{
   variable n;
   variable word;
   
   push_spot ();
   EXIT_BLOCK
     {
	pop_spot ();
     }
   ref = strcompress (ref, " ");
   
   n = 0;
   while (word = extract_element (ref, n,  ' '), strlen (word))
     {
	n++;
	skip_chars (" \t\n");
	!if (looking_at (word)) return 0;
	go_right (strlen (word));
     }
   1;
}




define info_follow_reference ()
{
   variable colon, colons, note, err, item, node, ref;
   colon = ":"; colons = "::";
   note = "*Note";
   err = "No cross references.";
   
   push_spot();
   !if (fsearch(note))
     {
	!if (bsearch(note))
	  {
	     pop_spot();
	     error(err);
	  }
     }
   pop_spot();
  
   ref = read_mini("Follow *Note", Null_String, Null_String);
   push_spot_bob ();
   forever
     {
	!if (fsearch(note))
	  {
	     pop_spot();
	     error ("Bad reference.");
	  }
	go_right (5);  skip_chars (" \t\n");
	% skip_white();
	% if (eolp()) 
	%  {
	%     go_right_1 (); skip_white();
	%  }
	if (info_looking_at(ref)) break;
     }
   
   push_mark();
   pop_spot();
   %info_record_position
   pop_mark_1 ();
   push_spot();
  
   !if (fsearch(colon))
     {
	pop_spot(); error ("Corrupt File?");
     }
   
   if (looking_at(colons))
     {
        push_mark();
        pop_spot();
        node = bufsubstr();
     }
   else
     {
        go_right_1 ();
        skip_white();
	if (eolp())
	  {
	     go_right_1 ();
	     skip_white();
	  }
        push_mark();
	if (looking_at_char('(')) () = ffind_char (')');
	% comma, tab, '.', or newline terminates
	skip_chars("\d032-\d043\d045\d047-\d255");
       
        bskip_chars(" ");
	node = bufsubstr(());
        pop_spot();
     }
   info_find_node(node);
}



define info_menu_number ()
{
   variable node;  node = Null_String;
   variable colon, colons; 
   colons = "::"; colon = ":";
   variable n;
  
   n = LAST_CHAR;
   if ((n < '1') or (n > '9')) return (beep());
   n -= '0';
  
   info_find_menu(1);

   while (n)
     { 
	!if (bol_fsearch("* ")) return (beep());
	if (ffind(colon)) --n; else eol();
     }
   
   if (looking_at(colons))
     {
        push_mark();
	bol(); go_right(2);
     }
   else
     {
	go_right_1 ();  skip_white();  push_mark();
	if (looking_at_char('('))
          {
	     () = ffind_char (')');
	  }
	% comma, tab, '.', or newline terminates
	skip_chars("\d032-\d043\d045\d047-\d255");
	bskip_chars(" ");
     }
   info_find_node(bufsubstr(()));
}



define info_tutorial()
{
   info_find_node("(info)help");
}

define info_reader ()
{
   info_mode ();
   variable f = "exit_jed";
   local_setkey (f,		"q");
   local_setkey (f,		"Q");
}
