------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T Y L E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.21 $                             --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This version of the Style package implements the standard GNAT style
--  checking rules. For documentation of these rules, see comments on the
--  individual procedures.

with Atree;  use Atree;
with Casing; use Casing;
with Csets;  use Csets;
with Errout; use Errout;
with Namet;  use Namet;
with Output; use Output;
with Scn;    use Scn;
with Scans;  use Scans;
with Sinfo;  use Sinfo;
with Sinput; use Sinput;

package body Style is

   Indentation : constant := 3;
   --  Number of columns for each indentation level

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Error_Space_Not_Allowed (S : Source_Ptr);
   --  Posts an error message indicating that a space is not allowed
   --  at the given source location.

   procedure Error_Space_Required (S : Source_Ptr);
   --  Posts an error message indicating that a space is required at
   --  the given source location.

   procedure Require_Following_Space;
   pragma Inline (Require_Following_Space);
   --  Require token to be followed by white space. Used only if in GNAT
   --  style checking mode.

   procedure Require_Preceding_Space;
   pragma Inline (Require_Preceding_Space);
   --  Require token to be preceded by white space. Used only if in GNAT
   --  style checking mode.

   -----------------------
   -- Body_With_No_Spec --
   -----------------------

   --  In GNAT style check mode, all subprograms must have specs

   procedure Body_With_No_Spec (N : Node_Id) is
   begin
      Error_Msg_N ("(style): subprogram body has no previous spec", N);
   end Body_With_No_Spec;

   ----------------------
   -- Check_Abs_Or_Not --
   ----------------------

   --  In GNAT style check mode, ABS or NOT must be followed by a space

   procedure Check_Abs_Not is
   begin
      if Source (Scan_Ptr) > ' ' then
         Error_Space_Required (Scan_Ptr);
      end if;
   end Check_Abs_Not;

   -----------------
   -- Check_Arrow --
   -----------------

   --  In GNAT style check mode, an arrow must be surrounded by spaces

   procedure Check_Arrow is
   begin
      Require_Preceding_Space;
      Require_Following_Space;
   end Check_Arrow;

   --------------------------
   -- Check_Attribute_Name --
   --------------------------

   --  In GNAT style mode, attribute names must be mixed case, i.e. start
   --  with an upper case letter, and otherwise lower case, except after
   --  an underline character.

   procedure Check_Attribute_Name (Reserved : Boolean) is
   begin
      if Determine_Token_Casing /= Mixed_Case then

         --  For now, warning only in the reserved word case ???

         if Reserved then
            Error_Msg_SC ("(style) bad capitalization, mixed case required");
         else
            Error_Msg_SC ("(style) bad capitalization, mixed case required");
         end if;
      end if;
   end Check_Attribute_Name;

   ---------------
   -- Check_Box --
   ---------------

   --  In GNAT style check mode, a box must be preceded by a space or by
   --  a left parenthesis. Spacing checking on the surrounding tokens takes
   --  care of the remaining checks.

   procedure Check_Box is
   begin
      if Prev_Token /= Tok_Left_Paren then
         Require_Preceding_Space;
      end if;
   end Check_Box;

   ---------------------------
   -- Check_Binary_Operator --
   ---------------------------

   --  In GNAT style check mode, binary operators other than exponentiation
   --  require a preceding and following space characters.

   procedure Check_Binary_Operator is
   begin
      Require_Preceding_Space;
      Require_Following_Space;
   end Check_Binary_Operator;

   -----------------
   -- Check_Colon --
   -----------------

   --  In GNAT style check mode, a colon must be surrounded by spaces

   procedure Check_Colon is
   begin
      Require_Preceding_Space;
      Require_Following_Space;
   end Check_Colon;

   -----------------------
   -- Check_Colon_Equal --
   -----------------------

   --  In GNAT style check mode, colon equal must be surrounded by spaces

   procedure Check_Colon_Equal is
   begin
      Require_Preceding_Space;
      Require_Following_Space;
   end Check_Colon_Equal;

   -----------------
   -- Check_Comma --
   -----------------

   --  In GNAT style check mode, a comma must be either the first
   --  token on a line, or be preceded by a blank. It must also
   --  always be followed by a blank.

   procedure Check_Comma is
   begin
      if Token_Ptr > First_Non_Blank_Location
        and then Source (Token_Ptr - 1) = ' '
      then
         Error_Space_Not_Allowed (Token_Ptr - 1);
      end if;

      if Source (Scan_Ptr) > ' ' then
         Error_Space_Required (Scan_Ptr);
      end if;
   end Check_Comma;

   -------------------
   -- Check_Comment --
   -------------------

   --  In GNAT style check mode, we have several requirements on comments.
   --  Comments that are not at the start of a line merely require at least
   --  one space after the second minus, there is no other required. For
   --  comments at the start of a line, either two blanks appear after the
   --  second minus, or as special cases, a row of minuses, or a line starting
   --  with two blanks and a minus and ending with a blank and two minuses is
   --  permitted. To see the reason for these special exceptions, look at the
   --  box that precedes this procedure!

   procedure Check_Comment is
      S : Source_Ptr;

   begin
      --  Can never have a non-blank character preceding the first minus

      if Scan_Ptr > Source_First (Current_Source_File)
        and then Source (Scan_Ptr - 1) > ' '
      then
         Error_Msg_S ("(style) space required");
         return;
      end if;

      --  For a comment that is not at the start of the line, the only
      --  requirement is that we cannot have a non-blank character after
      --  the second minus sign.

      if Scan_Ptr /= First_Non_Blank_Location then
         if Source (Scan_Ptr + 2) > ' ' then
            Error_Msg ("(style) space required", Scan_Ptr + 2);
         end if;

         return;

      --  Case of a comment that is at the start of a line

      else
         --  First check, must be in appropriately indented column

         if Start_Column rem Indentation /= 0 then
            Error_Msg_S ("(style) bad column");
            return;
         end if;

         --  If we are not followed by a blank, then the only allowed case is
         --  when the entire line is made up of minus signs (case of a box
         --  comment, or in the trivial case, of a -- comment all on its own
         --  on a line, which is also permissible)

         if Source (Scan_Ptr + 2) /= ' ' then
            S := Scan_Ptr + 2;

            while Source (S) >= ' ' loop
               if Source (S) /= '-' then
                  Error_Space_Required (Scan_Ptr + 2);
                  return;
               end if;

               S := S + 1;
            end loop;

         --  If we are followed by a blank, then the comment is OK if the
         --  character following this blank is another blank or a format
         --  effector.

         elsif Source (Scan_Ptr + 3) <= ' ' then
            return;

         --  Here is the case where we only have one blank after the two minus
         --  signs, which is an error unless the line ends with two blanks, the
         --  case of a box comment.

         else
            S := Scan_Ptr + 3;

            while Source (S) not in Line_Terminator loop
               S := S + 1;
            end loop;

            if Source (S - 1) /= '-' or else Source (S - 2) /= '-' then
               Error_Space_Required (Scan_Ptr + 3);
            end if;
         end if;
      end if;
   end Check_Comment;

   -------------------
   -- Check_Dot_Dot --
   -------------------

   --  In GNAT style check mode, dot dot must be surrounded by spaces

   procedure Check_Dot_Dot is
   begin
      Require_Preceding_Space;
      Require_Following_Space;
   end Check_Dot_Dot;

   -----------------------------------
   -- Check_Exponentiation_Operator --
   -----------------------------------

   --  No spaces are required for the ** operator in GNAT style check mode

   procedure Check_Exponentiation_Operator is
   begin
      null;
   end Check_Exponentiation_Operator;

   --------------
   -- Check_HT --
   --------------

   --  Horizontal tab characters are not allowed in GNAT style check mode

   procedure Check_HT is
   begin
      Error_Msg_S ("(style) horizontal tab not allowed");
   end Check_HT;

   ----------------------
   -- Check_Identifier --
   ----------------------

   procedure Check_Identifier (Ref : Node_Id; Def : Node_Id) is
      SRef : Source_Ptr := Sloc (Ref);
      SDef : Source_Ptr := Sloc (Def);
      TRef : Source_Buffer_Ptr;
      TDef : Source_Buffer_Ptr;

   begin
      --  Only do the check if both identifiers come from the source

      if Comes_From_Source (Ref)
        and then Comes_From_Source (Def)
      then
         TRef := Source_Text (Get_Source_File_Index (SRef));
         TDef := Source_Text (Get_Source_File_Index (SDef));

         for J in 1 .. Length_Of_Name (Chars (Ref)) loop
            if TRef (SRef) /= TDef (SDef) then

               --  Ignore the case where one is an operator and the other
               --  is not, this is a phenomenon from rewriting of operators
               --  as functions, and is to be ignored. The exact case that
               --  occurs is something like <= being compared with "<="

               if (TRef (SRef) = '"' and then TDef (SDef) /= '"')
                     or else
                  (TDef (SDef) = '"' and then TRef (SRef) /= '"')
               then
                  return;

               else
                  Error_Msg_Node_1 := Def;
                  Error_Msg
                    ("(style) bad identifier casing, should be&", SRef);
                  return;
               end if;
            end if;

            SRef := SRef + 1;
            SDef := SDef + 1;
         end loop;
      end if;
   end Check_Identifier;

   -----------------------
   -- Check_Indentation --
   -----------------------

   --  In GNAT style check mode, a new statement or declaration is required
   --  to start in a column that is a multiple of the indentiation amount.

   procedure Check_Indentation is
   begin
      if Token_Ptr = First_Non_Blank_Location
        and then Start_Column rem Indentation /= 0
      then
         Error_Msg_SC ("(style) bad indentation");
      end if;
   end Check_Indentation;

   ----------------------
   -- Check_Left_Paren --
   ----------------------

   --  In GNAT style check mode, a left paren must not be preceded by an
   --  identifier character or digit (a separating space is required) and
   --  may never be followed by a space.

   procedure Check_Left_Paren is
   begin
      if Token_Ptr > Source_First (Current_Source_File)
        and then Identifier_Char (Source (Token_Ptr - 1))
      then
         Error_Space_Required (Token_Ptr - 1);
      end if;

      if Source (Scan_Ptr) = ' ' then
         Error_Space_Not_Allowed (Scan_Ptr);
      end if;
   end Check_Left_Paren;

   ---------------------------
   -- Check_Line_Terminator --
   ---------------------------

   --  In GNAT style check mode, a line may not have trailing spaces

   procedure Check_Line_Terminator is
      S : Source_Ptr;

   begin
      if Scan_Ptr > First_Non_Blank_Location then
         if Source (Scan_Ptr - 1) = ' ' then
            S := Scan_Ptr - 1;

            while Source (S - 1) = ' ' loop
               S := S - 1;
            end loop;

            Error_Msg ("(style) trailing spaces not permitted", S);
         end if;
      end if;
   end Check_Line_Terminator;

   -----------------------
   -- Check_Pragma_Name --
   -----------------------

   --  In GNAT style mode, pragma names must be mixed case, i.e. start
   --  with an upper case letter, and otherwise lower case, except after
   --  an underline character.

   procedure Check_Pragma_Name is
   begin
      if Determine_Token_Casing /= Mixed_Case then
         Error_Msg_SC ("(style) bad capitalization, mixed case required");
      end if;
   end Check_Pragma_Name;

   -----------------------
   -- Check_Right_Paren --
   -----------------------

   --  In GNAT style check mode, a right paren must never be preceded by
   --  a space unless it is the initial non-blank character on the line.

   procedure Check_Right_Paren is
   begin
      if Token_Ptr > First_Non_Blank_Location
        and then Source (Token_Ptr - 1) = ' '
      then
         Error_Space_Not_Allowed (Token_Ptr - 1);
      end if;
   end Check_Right_Paren;

   ---------------------
   -- Check_Semicolon --
   ---------------------

   --  In GNAT style check mode, a preceding space is not permitted,
   --  and a following space is required.

   procedure Check_Semicolon is
   begin
      if Scan_Ptr > Source_First (Current_Source_File)
        and then Source (Token_Ptr - 1) = ' '
      then
         Error_Space_Not_Allowed (Token_Ptr - 1);

      elsif Source (Scan_Ptr) > ' ' then
         Error_Space_Required (Scan_Ptr);
      end if;
   end Check_Semicolon;

   ----------------
   -- Check_Then --
   ----------------

   --  In GNAT style check mode, we do not permit a THEN to stand on its own
   --  on a line unless the condition spreads over more than a single line,
   --  i.e. the THEN may not appear on the line immediately after the IF.

   procedure Check_Then (If_Loc : Source_Ptr) is
   begin
      if Get_Line_Number (Token_Ptr) = Get_Line_Number (If_Loc) + 1 then
         Error_Msg_SC ("(style) misplaced THEN");
      end if;
   end Check_Then;

   -------------------------------
   -- Check_Unary_Plus_Or_Minus --
   -------------------------------

   --  In GNAT style check mode, a unary plus or minus must not be followed
   --  by a space.

   procedure Check_Unary_Plus_Or_Minus is
   begin
      if Source (Scan_Ptr) = ' ' then
         Error_Space_Not_Allowed (Scan_Ptr);
      end if;
   end Check_Unary_Plus_Or_Minus;

   ------------------------
   -- Check_Vertical_Bar --
   ------------------------

   --  In GNAT style check mode, a vertical bar must be surrounded by spaces

   procedure Check_Vertical_Bar is
   begin
      Require_Preceding_Space;
      Require_Following_Space;
   end Check_Vertical_Bar;

   -----------------------------
   -- Error_Space_Not_Allowed --
   -----------------------------

   procedure Error_Space_Not_Allowed (S : Source_Ptr) is
   begin
      Error_Msg ("(style) space not allowed", S);
   end Error_Space_Not_Allowed;

   --------------------------
   -- Error_Space_Required --
   --------------------------

   procedure Error_Space_Required (S : Source_Ptr) is
   begin
      Error_Msg ("(style) space required", S);
   end Error_Space_Required;

   ------------
   -- No_End --
   ------------

   --  In GNAT style check mode, we always require the name of a subprogram
   --  or package to be present on the END, so this is an unconditional error.

   procedure No_End (Name : Node_Id) is
   begin
      Error_Msg_Node_1 := Name;
      Error_Msg_SP ("(style) `END &` required");
   end No_End;

   ----------------------------
   -- Non_Lower_Case_Keyword --
   ----------------------------

   --  In GNAT style check mode, reserved keywords must be be spelled in all
   --  lower case (excluding keywords range, access, delta and digits used as
   --  attribute designators). This is therefore an unconditional error.

   procedure Non_Lower_Case_Keyword is
   begin
      Error_Msg_SC ("(style) reserved words must be all lower case");
   end Non_Lower_Case_Keyword;

   -----------------------------
   -- Require_Following_Space --
   -----------------------------

   procedure Require_Following_Space is
   begin
      if Source (Scan_Ptr) > ' ' then
         Error_Space_Required (Scan_Ptr);
      end if;
   end Require_Following_Space;

   -----------------------------
   -- Require_Preceding_Space --
   -----------------------------

   procedure Require_Preceding_Space is
   begin
      if Token_Ptr > Source_First (Current_Source_File)
        and then Source (Token_Ptr - 1) > ' '
      then
         Error_Space_Required (Token_Ptr - 1);
      end if;
   end Require_Preceding_Space;

   -------------------------
   -- Set_Max_Line_Length --
   -------------------------

   --  In GNAT style check mode, the maximum line length is 79

   procedure Set_Max_Line_Length (N : in out Nat) is
   begin
      N := 79;
   end Set_Max_Line_Length;

end Style;
