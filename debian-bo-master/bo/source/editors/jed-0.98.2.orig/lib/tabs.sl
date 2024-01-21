%%
%%  Tab routines for JED
%%


!if (is_defined("Tab_Stops"))
{
   variable Tab_Stops;
   Tab_Stops = create_array('i', 20, 1);
   
   %% The following defines the tab stops to be 8 column:
   _for (0, 19, 1) 
     { =$1; 
	Tab_Stops[$1] = $1 * 8 + 1;
     }
}

  
define tab_to_tab_stop ()
{
   variable c, goal, i;
   c = what_column ();
  
   _for (0, 19, 1)
     {
	=i;
	goal = Tab_Stops[i];
	if (goal > c) break;
     }
   
   insert_spaces (goal - c);
}

setkey ("tab_to_tab_stop", "^I");

variable _Tabs_Buffer;

$1 = "*TabsEdit*";
!if (keymap_p($1))
{
   make_keymap ($1);
   undefinekey ("^C", $1);
   definekey ("tabs_install_tab_stops", "^C^C", $1);
}

%% emacs like edit tab stops
define edit_tab_stops ()
{
   variable tbuf, i;
   _Tabs_Buffer = whatbuf();
   tbuf = "*TabsEdit*";
 
   sw2buf(tbuf); erase_buffer();
   TAB = 0;
      
   use_keymap(tbuf);
   _for (0, 19, 1) 
     {
	=i;
	goto_column (Tab_Stops[i]);
	insert_char ('T');
     }
   newline ();
   _for (1, 13, 1)
     {
	=i;
	goto_column (10 * i);
	insert (string(i));
     }
   newline ();
   
   loop (13) insert ("1234567890");
   
   insert ("\nTo install changes, type control-c control-c.");
   bob ();
   set_buffer_modified_flag (0);
   set_overwrite (1);
}


define tabs_install_tab_stops ()
{
   variable i;
   bob ();
   _for (0, 19, 1)
     { =i;
	skip_white ();
	if (eolp()) break;
	Tab_Stops[i] = what_column ();
	go_right_1 ();
     }
   
   _for (i, 19, 1) { =i; Tab_Stops[i] = 0;}
   set_buffer_modified_flag (0);
   delbuf (whatbuf ());
   sw2buf ( _Tabs_Buffer);
}
