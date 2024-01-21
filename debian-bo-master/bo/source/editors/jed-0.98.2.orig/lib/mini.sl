%
%  Recall previous commands in MiniBuffer
%

!if (is_defined ("Mini_Previous_Lines"))
{
   variable Mini_Previous_Lines, Mini_Last_Line, Mini_Next_Line, Mini_First_Line;
   Mini_Previous_Lines = create_array ('s', 12, 1);
   _for (0, 11, 1) 
     {
	=$1;
	Mini_Previous_Lines[$1] = Null_String;
     }
}
Mini_Last_Line = 0;
Mini_First_Line = 0;
Mini_Next_Line = 0;

define mini_use_this_line ()
{
   erase_buffer ();
   insert (Mini_Previous_Lines[Mini_Next_Line]);
}


define next_mini_command ()
{
   variable line;

   if (Mini_Next_Line == Mini_Last_Line) 
     {
	error ("End of list!");
     }
   Mini_Next_Line++;
   if (Mini_Next_Line == 12)  Mini_Next_Line = 0;
   mini_use_this_line ();
}

define prev_mini_command ()
{
   variable line;
   
   line = Mini_Next_Line;
   if (Mini_First_Line == Mini_Next_Line)
     {
	Mini_Next_Line = line;
	error ("Top of list!");
     }
   Mini_Next_Line--;
   if (Mini_Next_Line < 0)  Mini_Next_Line = 11;
   mini_use_this_line ();
}

define mini_exit_minibuffer ()
{
   bol_skip_white ();
   !if (eolp ())
     {
	Mini_Previous_Lines[Mini_Last_Line] = line_as_string ();
	Mini_Last_Line++;
	if (Mini_Last_Line == 12) Mini_Last_Line = 0;
	if (Mini_Last_Line == Mini_First_Line)
	  {
	     Mini_First_Line++;
	     if (Mini_First_Line == 12) Mini_First_Line = 0;
	  }
     } 
   Mini_Next_Line = Mini_Last_Line;
   Mini_Previous_Lines[Mini_Last_Line] = Null_String;
   call ("exit_mini");
}
