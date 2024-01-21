variable Wmenu_Modes_Popup;
 

define simple_menu()
{
   variable menubar;
   variable file_popup, edit_popup, search_popup, buffers_popup, 
     modes_popup, help_popup, windows_popup;

   destroy_menubar();
   file_popup = create_popup_menu();
   ERROR_BLOCK 
      {
	 destroy_menu(file_popup);
      }
   append_menu_item(file_popup, "Open\t^X^F", 0x1, "find_file");
   append_menu_item(file_popup, "Save\t^X^W", 0x2, "write_buffer");
   append_menu_item(file_popup, "Save Buffers\t^X^s", 0x3, "save_buffers");
   append_menu_item(file_popup, "Insert File\t^Xi", 0x4, "insert_file");
   append_separator(file_popup);
   append_menu_item(file_popup, "Shell Cmd", 0x5, "do_shell_cmd");
   append_separator(file_popup);
   append_menu_item(file_popup, "Exit\t^X^C", 0x6, "exit_jed");

   edit_popup = create_popup_menu();
   ERROR_BLOCK 
      {
	 destroy_menu(file_popup);
	 destroy_menu(edit_popup);
      }
   append_menu_item(edit_popup, "Undo\t^Xu", 10, "undo");
   append_separator(edit_popup);
   append_menu_item(edit_popup, "Cut", 11, "kill_region");
   append_menu_item(edit_popup, "Copy", 12, "copy_region");
   append_menu_item(edit_popup, "Paste", 13, "yank");
   
   search_popup = create_popup_menu();
   ERROR_BLOCK 
      {
	 destroy_menu(file_popup);
	 destroy_menu(edit_popup);
	 destroy_menu(search_popup);
      }
   append_menu_item(search_popup, "Search Forward", 20, "search_forward");
   append_menu_item(search_popup, "Search Backward", 21, "search_backward");
   append_menu_item(search_popup, "Replace", 23, "replace_cmd");
   
   buffers_popup = create_popup_menu();
   ERROR_BLOCK 
      {
	 destroy_menu(file_popup);
	 destroy_menu(edit_popup);
	 destroy_menu(search_popup);
	 destroy_menu(buffers_popup);
      }
   append_menu_item(buffers_popup, "Kill", 30, "kill_buffer");
   append_menu_item(buffers_popup, "Switch To", 31, "switch_to_buffer");
   append_menu_item(buffers_popup, "List Buffers", 32, "list_buffers");
   Wmenu_Modes_Popup = create_popup_menu();
   ERROR_BLOCK 
      {
	 destroy_menu(file_popup);
	 destroy_menu(edit_popup);
	 destroy_menu(search_popup);
	 destroy_menu(buffers_popup);
	 destroy_menu(Wmenu_Modes_Popup);
      }
   append_menu_item(Wmenu_Modes_Popup, "C Mode", 40, "c_mode");
   append_menu_item(Wmenu_Modes_Popup, "Text Mode", 41, "text_mode");
   append_menu_item(Wmenu_Modes_Popup, "No Mode", 42, "no_mode");
   append_menu_item(Wmenu_Modes_Popup, "Fortran Mode", 43, "fortran_mode");
   append_popup_menu(buffers_popup, "Modes", Wmenu_Modes_Popup);
   
   windows_popup = create_popup_menu();
   ERROR_BLOCK 
      {
	 destroy_menu(file_popup);
	 destroy_menu(edit_popup);
	 destroy_menu(search_popup);
	 destroy_menu(buffers_popup);
	 destroy_menu(Wmenu_Modes_Popup);
	 destroy_menu(windows_popup);
      }
   append_menu_item(windows_popup, "One Window", 50, "one_window");
   append_menu_item(windows_popup, "Split Window", 51, "split_window");
   append_menu_item(windows_popup, "Other Window", 52, "other_window");
   append_menu_item(windows_popup, "Delete Window", 53, "delete_window");
   append_separator(windows_popup);
   append_menu_item(windows_popup, "Redraw", 54, "redraw");
   
   help_popup = create_popup_menu();
   ERROR_BLOCK 
      {
	 destroy_menu(file_popup);
	 destroy_menu(edit_popup);
	 destroy_menu(search_popup);
	 destroy_menu(buffers_popup);
	 destroy_menu(Wmenu_Modes_Popup);
	 destroy_menu(windows_popup);
	 destroy_menu(help_popup);
      }
   append_menu_item(help_popup, "Show Key", 60, "showkey");
   append_menu_item(help_popup, "Where Is Command", 61, "where_is");
   
   menubar = get_menubar();
   append_popup_menu(menubar, "&File", file_popup);
   append_popup_menu(menubar, "&Edit", edit_popup);
   append_popup_menu(menubar, "&Search", search_popup);
   append_popup_menu(menubar, "&Buffers", buffers_popup);
   append_popup_menu(menubar, "&Windows", windows_popup);
   append_popup_menu(menubar, "&Help", help_popup);
   
   set_init_popup_callback("init_popup");
   redraw_menubar();
}

define init_popup(hpopup)
{
   variable mode;
   
   if (hpopup == Wmenu_Modes_Popup)
      {
	 (, mode) = what_mode();

	 
	 check_menu_item(Wmenu_Modes_Popup, 40, mode == 2);   %  C mode
	 check_menu_item(Wmenu_Modes_Popup, 41, mode == 1);   %  Text mode
	 check_menu_item(Wmenu_Modes_Popup, 42, mode == 0);   %  No mode
	 check_menu_item(Wmenu_Modes_Popup, 43, mode == 16);   %  Fortran mode
      }
}


