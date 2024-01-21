% Simple register facility.  This provides up to 128 pastebuffers

variable Register_Buffer_Arrays = create_array ('s', 128, 1);
define reg_copy_to_register ()
{
   !if (markp ()) error ("No region defined.");
   flush ("Enter Register:");
   
   Register_Buffer_Arrays [getkey () & 0x7F] = bufsubstr ();
   message (Null_String);
}

define reg_insert_register ()
{
   flush ("Enter Register:");
   insert (Register_Buffer_Arrays [getkey () & 0x7F]);
   message (Null_String);
}

