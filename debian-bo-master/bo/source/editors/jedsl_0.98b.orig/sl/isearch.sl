% Here is a new version of isearch.sl reworked by John Burnell <johnb@whio.grace.cri.nz>.
% This one is said to be more emacs-like.
%% isearch.sl
%% Routines here perform incremental forward/backward searches
%% 
%% ESCAPE key aborts the search
%% ENTER key finds next match
%% DELETE key deletes last character entered and returns to previous point
%%
%%  "isearch_forward"	"^FIf"   setkey  %(sample bindings)
%%  "isearch_backward"	"^FIb"   setkey

variable Isearch_Last_Search = Null_String;

%% This code fragment checks to see what the isearch keys are bound to
variable Isearch_Forward_Char = 19;   %^S
variable Isearch_Backward_Char = 18;  %^R

define get_bound_key (search_func, default)
{
   variable n, key;
   if (n = which_key (search_func), n)  
     {
	key = (); n--;
	if (strlen (key) == 2) 
	  {
	     if (key [0] == '^') 
	       return (key [1] - '@');
	  }
     }
   return default;
}

Isearch_Forward_Char = get_bound_key ("isearch_forward", 19);
Isearch_Backward_Char = get_bound_key ("isearch_backward", 18);

define isearch_simple_search (dir)
{
   variable fun, prompt;
   
   prompt = "Search:";
   fun = &fsearch;
   
   if (dir < 0)
     {
	prompt = "Search Backwards:";
	fun = &bsearch;
     }
   
   Isearch_Last_Search = read_mini (prompt, Isearch_Last_Search, Null_String);
   
   !if (fun (Isearch_Last_Search))
     error ("Not Found.");
}

define isearch_del (str)
{
   variable n = strlen (str);
   if (n) str = substr (str, 1, n - 1);
   str;
}


define isearch_dir (dir)
{
   variable prompt, str = Null_String;
   variable c, m, first = 1;
   m = 1;
   
  % This is tricky.  I am leaving a 1 or a 0 on the stack which indicates
  % whether or not a character is attached to a mark.  The number of ones
  % and zeros on the stack should match the value of m.
  % These 0s and 1s are used when unwinding the search stack
  % Since a mark was pushed and not attached to a character, push 0
  
   push_mark ();
   0;
   ERROR_BLOCK { loop (m) { pop (); pop_mark (0);}} %make sure we pop marks
  
   forever {
      if (dir == 1)
      	prompt = "Isearch forward: ";
      else
      	prompt = "Isearch Backward: ";
      message (strcat (prompt, str));
      push_spot ();
      if ((dir > 0) and looking_at (str))
	{
	   go_right (strlen (str));
	}
      update (0);
      pop_spot ();
            
      c = getkey();
      switch (c)
	{ (() == 27) and first : isearch_simple_search (dir); break;}
        { case Isearch_Forward_Char :       % ^S
	   push_mark (); ++m; 0;             % mark not attached to char!
	   if (dir < 0) {
	      dir = 1;
	   } else {
	      go_right_1 ();
	      !if (strlen (str)) str = Isearch_Last_Search;
	   }
        }  
        { case Isearch_Backward_Char :  %^R
	   push_mark (); 0; ++m;         % mark not attached to char!
	   if (dir > 0) {   
	      dir = -1;
	      c = ' ';                      % use this to fool test (*) below
	   } else  {
	      !if (strlen (str)) str = Isearch_Last_Search;
	   }
        }
        { case 127 : 
		  
	  if (m) {
	    if (()) { 
	      if (strlen (str)) str = isearch_del (str); % delete char
	    }
	    --m; pop_mark (1);     % top of stack already popped
	  }
	  continue;
        }
        { case 7 : % ^G go back to start
	   loop (m - 1) {pop (); pop_mark (0)}  % pop marks
	   pop ();
	   pop_mark (1);                      % go to start of search
	   beep ();
	   return;
	}
	{
	 case '\r' and first:
	   loop (m)
	     {
		pop ();
		pop_mark (0);
	     }
	   m = 0;
	   if (dir > 0) return search_forward ();
	   else return search_backward ();
	}
	   
        { () < 32 : break;}                       % terminate search
	{ pop (); str = strcat (str, char (c));     % any other char
	   1; push_mark(); ++m;                      % push 1 and mark
	}

      first = 0;

% test (*), see ^R switch above
      if ((dir < 0) and (m > 1) and looking_at (str) and (c >= ' ')) 
	continue;
   
      if (dir == 1) fsearch (str);
      else bsearch (str);
      !if (())
	{
	   flush (strcat (str, " not found."));
	   beep ();
	   () = input_pending (10); 
	   if (() == 1) str = isearch_del (str);
	   --m; pop_mark (0);     % top of stack already popped in test
	} 
   }
  
   EXECUTE_ERROR_BLOCK;
   if (strlen (str)) 
     Isearch_Last_Search = str;
   if (dir == 1) 
     go_right (strlen (str));
   message ("Done.");

   flush_input ();
}
   
define isearch_forward  {isearch_dir (1);}
define isearch_backward {isearch_dir (-1);}


