
% Simple mappings
definekey ("art_bob", "^R", "article");
definekey ("art_eob", "$", "article");

define tin_art_next_unread_or_group ()
{
   if (get_header_flags () & HEADER_READ)
     {
	!if (header_next_unread ())
	  {
	     header_bob ();
	     if (get_header_flags () & HEADER_READ)
	       {
		  !if (header_next_unread ())
		    {
		       !if (is_article_visible ())
			 {
			    art_quit ();
			    tin_group_next_unread ();
			    return;
			 }
		       
		       art_hide_article_window ();
		       return;
		    }
	       }
	  }
     }
   art_select_article ();
}
definekey ("tin_art_next_unread_or_group", "\t", "article");


define tin_art_quit ()
{
   if (is_article_visible ())
     {
	art_hide_article_window ();
	return;
     }
   
   art_quit ();
}
definekey ("tin_art_quit", "q", "article");
#ifndef OS2
definekey ("tin_art_quit", "\e[D", "article");   %  left arrow
definekey ("tin_art_quit", "\eOD", "article");
definekey ("tin_art_quit", "^(kl)", "article");
definekey ("art_select_article", "\e[C", "article");   %  right arrow
definekey ("art_select_article", "\eOC", "article");
definekey ("art_select_article", "^(kr)", "article");
#else
definekey ("tin_art_quit", "\xE0K", "article");   %  left arrow
definekey ("art_select_article", "\xE0M", "article");   %  right arrow
#endif

define tin_space_key_cmd ()
{
   !if (is_article_visible ())
     {
	ERROR_BLOCK
	  {
	     _clear_error ();
	     header_bob ();
	  }
	call ("pagedn");
	return;
     }
   
   call ("article_pagedn");
}
definekey ("tin_space_key_cmd", " ", "article");

define tin_art_catchup_quit ()
{
   call ("catchup_all");
   % Provide visual feedback that catchup worked
   update ();
   art_quit ();
}

define tin_art_catchup_next ()
{
   tin_art_catchup_quit ();
   tin_group_next_unread ();
}
definekey ("tin_art_catchup_quit", "c", "article");
definekey ("tin_art_catchup_next", "C", "article");

