

!if (is_defined ("Abbrev_File"))
{
   variable Abbrev_File;
   $1 = getenv ("JED_HOME");
   !if (strlen ($1))
     {
	$1 = getenv ("HOME");
     }
   
#ifdef VMS
   if (0 == strlen ($1))
     {
	Abbrev_File = "SYS$LOGIN:abbrevs.sl";
     }
   else Abbrev_File = dircat ($1, Abbrev_File);
#else
   
#ifdef MSDOS OS2
   Abbrev_File = "abbrevs.sl";
#else
   Abbrev_File = ".abbrevs.sl";
#endif
   Abbrev_File = dircat ($1, Abbrev_File);
#endif % VMS 
}

if (file_status (Abbrev_File) > 0) pop (evalfile (Abbrev_File));

define set_abbrev_mode (val)
{
   getbuf_info ();
   if (val) () | 0x800; else () & ~(0x800);
   setbuf_info(());
}

define abbrev_mode ()
{
   variable flags = getbuf_info() xor 0x800;
   variable msg = "Abbrev mode OFF";
   setbuf_info(flags);
   if (flags & 0x800) msg = "Abbrev mode ON";
   message (msg);
}


