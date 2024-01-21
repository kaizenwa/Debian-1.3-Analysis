% dcl mode--  Special mode to facilitate editing of DCL files on VMS systems.
%

. ( [goal] 3 =goal
 
.   push_spot
.   {up_1}
.    {
.       eol_trim
.       bol_skip_white 
.       '$' looking_at_char
.          { "$\t " skip_chars
. 	   '!' looking_at_char {what_column =goal break} !if
. 	 } 
. 	 {
. 	   '!' looking_at_char 
. 	      {
	         % This takes care of running text following something
		 % like type sys$input
. 	         push_spot
. 	         up_1 eol bolp not and
. 		   {trim go_left_1
. 		    '-' looking_at_char 
. 		      { pop_spot what_column =goal break} !if
. 		   } if
. 		 pop_spot
. 	      } !if
. 	 } else
.    }
.   while
  
.   {"then" looking_at {2 +=goal 1} {0} else}
.   {"else" looking_at {2 +=goal 1} {0} else}
.   orelse pop  % orelse puts final value on stack
.   pop_spot
  
.   bol "$\t " skip_chars
.   {"else" looking_at {2 -=goal 1} {0} else}
.   {"endif" looking_at {2 -=goal 1} {0} else}
.   orelse pop
.   goal 1 > {goal}{3} else
. ) dcl_get_ind


. (
.    [goal]  
.    push_spot
.    dcl_get_ind =goal
   
.    push_spot
.    up_1 {eol_trim bolp {1 go_left '-' looking_at_char {2 +=goal} if} !if} if
.    pop_spot
   
.    bol "\t $" skip_chars
.    '!' looking_at_char 
.      { "\t " bskip_chars trim goal what_column - whitespace} 
.    !if 
.    pop_spot
.    skip_white
. )  dcl_indent


. ( [cont p] 0 =cont
.   bobp bolp '$' looking_at_char and or {"$ \n" insert 1 left pop return} if
.   trim
.   push_spot 
  % If line does not start with '$', assume something like 
  % type sys$input going on and do not give dollar
.   push_spot bol
.   '$' looking_at_char not up_1 and
.     {
.        bolp eol_trim
.        1 go_left
.        what_char '-' != =cont
.     } 
.   if
.   pop_spot
  
.   cont {
.     POINT =p "sys$input" bfind 
.       { 9 right pop 
.         ':' what_char == {1 right pop} if 
. 	POINT p == =cont
.       } if
.     p =POINT
.   } !if
  
.   bolp {1 left pop 
.         '-' looking_at_char { 1 =cont} if
.        } 
.       !if
.   pop_spot
.   newline
.   cont {'$' insert_char} !if
.   dcl_indent
. ) dcl_newline
	    

. "DCL_Mode" is_defined 
.    {  
.       [DCL_Mode] "DCL" =DCL_Mode
.       DCL_Mode make_keymap
.       "dcl_newline" "^M" DCL_Mode definekey
.       "newline_and_indent"  "^[^M" DCL_Mode definekey
.       "dcl_indent" "^I"  DCL_Mode definekey
.       "self_insert_cmd" "\"" DCL_Mode definekey
.       "self_insert_cmd" "'" DCL_Mode definekey
.    }
.   !if

. ( "DCL" use_keymap "dcl" 4 set_mode ) dcl_mode

