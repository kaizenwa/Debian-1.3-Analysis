% File that can be loaded by the 'simple' executable.  

define put_prompt ()
{
   fputs ("Enter filename or 'quit'> ", stdout); pop ();
   fflush (stdout); pop ();
}

% This is a trick--- simple if you understand S-Lang.
define printf (n)
{
   fputs (Sprintf (n), stdout); pop ();
   fflush (stdout); pop ();
}


define main ()
{
   variable file;
   variable vals = "dev,ino,mode,nlink,uid,gid,rdev,size,atime,mtime,ctime";
   variable val, v, i;

   while (put_prompt(), fgets (stdin) > 0)
     {
	file = ();
	
	% Strip '\n' and whitespace off the end of the file
	file = strtrim (file);
	
	if ((0 == strcmp (file, "quit"))
	    or (0 == strlen (file))) 
	  quit ();
	
	if (stat_file (file))
	  {
	     printf ("Unable to stat %s\n", file, 1);
	     continue;
	  }
	
	% stat succeeds return information about the file.
	printf ("Stat system call for %s yields:\n", file, 1);
	for (i = 0; i < 11; i++)
	  {
	     val = extract_element (vals, i, ',');
	     v = stat_struct (val);
	     if (i > 7) printf ("\t%s:\t%d (%s)\n", val, v, unix_ctime(v), 3);
	     else printf ("\t%s:\t%d\n", val, v, 2);
	  }
     }
}

main ();
   
