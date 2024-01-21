%%   Buffer routines for Jed.  Functions included here are:
%%    
%%     list_buffers  : pop up a list of buffers
%%     save_buffers  : saves buffers that are associated with a file
%%                     with no user intervention
%%     recover_file  : restore buffer from autosave file.
%%
%%
define list_buffers ()
{
   variable i, j, tmp, this, name, flags, flag_chars, skip;
   skip = 0;
   if (prefix_argument(-1) == -1) skip = 1;
   tmp = "*BufferList*";
   this = whatbuf();
   pop2buf(tmp);
   set_readonly(0);
   erase_buffer();
   insert("U:Undo O:Overwrite R:Readonly D:Disk File Changed, A:Autosave, M:Modified\n");
   insert("C:CRmode, B:Binary File, K:Not backed up, N:No autosave\n\n");
   
   flag_chars = "CBKN-UORDAM";
   
   insert ("\t\tBuffer Name");
   goto_column(45); insert("Dir/File\n");
   
   loop (buffer_list())
     {
	name = ();
	if (skip and int(name) == 32) continue;   %% internal buffers begin with a space
	setbuf(name);
	flags = getbuf_info();    % more on stack
	setbuf(tmp);
	bol();
	i = 0x400; j = 0;
	while (i)
	  {
	     if (flags & i) flag_chars[j]; else '-';
	     insert_char (());
	     i = i shr 1; j++;
	  }
	insert_char(9);
	insert(()); %% buffer name
	goto_column(45);
	!if (eolp())
	  {
	     eol(); insert_single_space();
	  }
	
	insert(()); insert(());               %% dir/file
	newline();
     }
   bob();
   set_buffer_modified_flag(0);
   set_readonly (1);
   pop2buf(this);
}



%!% Prototype: Void save_buffers ();
%!% Save all modified buffers that are associated with a file without
%!% user intervention.
define save_buffers ()
{
   variable file, dir, flags, buf, ch;
   
   loop (buffer_list())
     { =buf;
	ch = int(buf);
	if ((ch == 32) or (ch == '*')) continue;  %% internal buffer or special
      
	setbuf(buf);
	getbuf_info(); 	=flags; pop(); =dir; =file;
      
	!if (strlen(file)) continue;        %% no file assciated with it
	if (flags & 1)
	  {
	     !if (write_buffer(strcat(dir, file)))  
	        error (strcat ("Error writing buffer ", buf));
	  }
     }  
}

      
     
%% write region to file
define write_region()
{
   variable file;
   !if (markp) error("Set Mark first!");
   file = read_file_from_mini("File:");
   write_region_to_file(file);
}


define append_region ()
{
   variable file;
   !if (markp) error("Set Mark first!");
   file = read_file_from_mini("Append to File:");
   if (-1 == append_region_to_file(file)) error ("Append failed.");
}

;%% restores buffer from autosave file.
define recover_file ()
{
   variable flags, file, dir, as, buf;
   
   getbuf_info();  =flags; pop(); =dir; =file;
   !if (strlen(file)) error("Buffer not associated with a file.");
   as = make_autosave_filename (dir, file);
   if (file_status(as) != 1)
    {
       error (strcat(as, " not readable."));
    }
    
   buf = whatbuf();
   as;
   if (file_time_compare(as, strcat(dir, file)))
     {
        " more recent. Use it";
     }
   else " not recent. Use it";
   
   if (get_yes_no(strcat((),())) > 0)
     {
	what_line();
	setbuf(buf);
	erase_buffer();
	insert_file(as); pop();
	goto_line();
     } 
}
	   


  
