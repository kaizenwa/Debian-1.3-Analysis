%  This file presents some simple examples of extending the newsreader.  
%  See README.slang for a more complete discussion of the functions involved.

message ("reading ~/.slrn.sl file.");

% Some key macros
% I like the refresh groups command to always return to the beginning of
% the group buffer

define refresh_groups_bob ()
{
   call ("refresh_groups");
   call ("bob");
}
definekey ("refresh_groups_bob", "G", "group");


define article_mode_hook ()
{
   variable sorting_method = 7;
   variable author_display = 2;
   
   if (is_substr (current_newsgroup (), "binaries")
       or is_substr (current_newsgroup (), "pictures"))
     {
	sorting_method = 3;
	author_display = 0;
     }
   
   set_integer_variable ("sorting_method", sorting_method);
   set_integer_variable ("author_display", author_display);
}

define startup_hook ()
{
   !if (strcmp (server_name (), "uhog"))
     {
	set_integer_variable ("lines_per_update", 20);
	set_integer_variable ("read_active", 0);
     }
}

% This function pipes the currently selected article to my paging program
define pipe_to_most ()
{
   pipe_article ("most");
}
definekey ("pipe_to_most", "&", "article");

%
%  Here are some article searching functions
%

variable Last_Art_Search_Str = "";
define search_newsgroup ()
{
   variable str;
   variable flags;
   
   str = read_mini ("Search for regexp: ", Last_Art_Search_Str, "");
   !if (strlen (str))
     return;

   Last_Art_Search_Str = str;
   
   uncollapse_threads ();
   
   do
     {
	flags = get_header_flags ();
	
	if (re_search_article (str))
	  {
	     pop ();
	     return;
	  }
	
	set_header_flags (flags);
	
	call ("hide_article");
	update ();
     }
   while (header_down (1));
   
   error ("Not found.");
}


define re_search_article_cmd ()
{
   variable str = read_mini ("Re-search: ", Last_Art_Search_Str, "");
   !if (strlen (str))
     return;
   
   Last_Art_Search_Str = str;
   
   if (re_search_article (str))
     {
	pop ();			       %  line matched on stack
	return;
     }
   
   error ("Not found.");
}

definekey ("re_search_article_cmd", "/", "article");

% This function prompts for a search string and then calls Digital's
% AltaVista search engine.
define altavista_search ()
{
   variable browser = "lynx";
   variable url = "http://www.altavista.digital.com/";
   variable cgi = "cgi-bin/query?pg=q&what=news&fmt=.&q=";
   variable str, new_str, len, ch, chlo, i;
   variable cmd;
   
   str = strcat ("+newsgroups:", current_newsgroup ());
   str = strcat (str, " +");
   
   str = read_mini ("AltaVista Search", "", str);
   
   len = strlen (str);
   !if (len) return;
   
   new_str = "";
   for (i = 0; i < len; i++)
     {
	ch = str [i];
	chlo = ch | 0x20;
	if ((chlo > 'z') or (chlo < 'a'))
	  ch = Sprintf ("%%%02X", ch, 1);
	else 
	  ch = char (ch);
	
	new_str = strcat (new_str, ch);
     }
   
   cmd = Sprintf ("%s '%s%s%s'", browser, url, cgi, new_str, 4);
   () = system (cmd);
}


% 
% This function pops up a selection box
%
define easy_search ()
{
   variable rsp;
   rsp = get_select_box_response ("NewsGroup Searches",   %  title
				  "AltaVista (WWW)",
				  "search articles",
				  2);

   switch (rsp)
     { case 0: altavista_search (); }
     { case 1: search_newsgroup (); }
}

definekey ("easy_search", "$", "article");


% This will allow me to execute S-Lang commands from within slrn.
define execute_slang ()
{
   variable str;
   
   str = read_mini ("S-Lang> ",  "", "");
   !if (strlen (str))
     return;
   
   eval (str);
}

definekey ("execute_slang", "^Kd", "group");
definekey ("execute_slang", "^Kd", "article");


% These functions may facilitate nn style reading

define tag_header ()
{
   set_header_flags (get_header_flags () | HEADER_TAGGED);
}

define untag_header ()
{
   set_header_flags (get_header_flags () & ~(HEADER_TAGGED));
}

define header_number_hook ()
{
   tag_header ();
}


define nn_like_function ()
{
   call ("catchup_all");
   set_prefix_argument (1);
   call ("toggle_header_tag");	       % If used with prefix, it untags all  
   call ("art_xpunge");		       % remove read articles
   call ("art_header_bob");	       % goto top of list
   call ("scroll_dn");		       % read article
}

definekey ("nn_like_function", "+", "article");

define read_prev_tagged ()
{
   !if (prev_tagged_header ())
     error ("No more tagged headers.");
   
   untag_header ();
   call ("scroll_dn");
}

define read_next_tagged ()
{
   !if (next_tagged_header ())
     error ("No more tagged headers.");
   
   untag_header ();
   call ("scroll_dn");
}

   
definekey ("read_next_tagged", "]", "article");
definekey ("read_prev_tagged", "[", "article");


variable Last_Search_Str = "";
define re_subject_search_forward ()
{
   variable str;
   
   !if (header_down (1))
     return;
   
   str = read_mini ("Subject re-search fwd: ", Last_Search_Str, "");
   
   !if (strlen (str))
     return;
   
   !if (re_fsearch_subject (str))
     error ("Not found.");
}

definekey ("re_subject_search_forward", "s", "article");   

define tag_via_subject_regexp ()
{
   variable str;
   variable count = 0;
   
   str = read_mini ("Tag subjects pattern", "", "");
   !if (strlen (str))
     return;

   call ("mark_spot");
   
   uncollapse_threads ();
   while (re_fsearch_subject (str))
     {
	tag_header ();
	count++;
	!if (header_down (1))
	  break;
     }
   collapse_threads ();

   call ("exchange_mark");
   message (Sprintf ("%d headers marked.", count, 1));
}

definekey ("tag_via_subject_regexp", "%", "article");

