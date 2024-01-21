define filter_region ()
{
   variable cmd, file;

   check_region (1);		       %  spot pushed
   () = dupmark ();
   
   ERROR_BLOCK
     {
	pop_mark (0);
	pop_spot ();
     }
   
   do
     {
	cmd = read_mini ("Filter command:", Null_String, Null_String);
     }
   while (not (strlen (cmd)));

   file = make_tmp_file ("/tmp/jedfilter");

   ERROR_BLOCK
     {
	pop_spot ();
	() = delete_file (file);
     }
   () = pipe_region (Sprintf ("%s > %s", cmd, file, 2));
   
   if (-1 == insert_file (file))
     error ("Error encountered running filter.");

   del_region ();
   
   EXECUTE_ERROR_BLOCK;
}

