% misc functions that should be in site.sl but they are not because we want
% jed to start up fast and they are not always needed.


%!% Prototype: String make_tmp_buffer_name (String base);
%!% Generates a unique buffer name using the string 'base' for the beginning
%!% of the name.  The buffer name is returned.  The buffer is not created.
define make_tmp_buffer_name (tmp)
{
   variable n = 0, buf;
   variable t = time ();
   
   tmp = strcat (tmp, time);
   do 
     {
	buf = Sprintf("%s%d", tmp, n, 2);
	n++;
     }
   while (bufferp(buf));
   buf;
}

define misc_do_write_to_file (str, file, write_function)
{
   variable ret = -1;   
   variable buf = make_tmp_buffer_name (Null_String);
   variable cbuf = whatbuf ();
   
   setbuf (buf);
   insert (str);
   set_buffer_modified_flag (0);
   push_mark ();
   bob ();
#iffalse   
   ERROR_BLOCK 
     {
	_clear_error ();
     }
#endif
   ret = write_function (file);
   
   setbuf (cbuf);
   delbuf (buf);
   ret;
}

%!% Prototype: Integer append_string_to_file (String str, String file);
%!% The string 'str' is appended to file 'file'.  This function returns -1
%!% upon failure or the number of lines written upon success.
%!% See append_region_to_file for more information.
define append_string_to_file (str, file)
{
   misc_do_write_to_file (str, file, &append_region_to_file);
}

%!% Prototype: Integer write_string_to_file (String str, String file);
%!% The string 'str' is written to file 'file'.  This function returns -1
%!% upon failure or the number of lines written upon success.
%!% This function does not modify a buffer visiting the file.
define write_string_to_file (str, file)
{
   misc_do_write_to_file (str, file, &write_region_to_file);
}
