------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              C O M P E R R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.40 $                             --
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

--  This package contains routines called when a fatal internal compiler
--  error is detected. Calls to these routines cause termination of the
--  current compilation with appropriate error output.

with Atree;    use Atree;
with Debug;    use Debug;
with Errout;   use Errout;
with Gnatvsn;
with Osint;    use Osint;
with Output;   use Output;
with Sinput;   use Sinput;
with Sprint;   use Sprint;
with Sdefault; use Sdefault;
with Treepr;   use Treepr;

with System.Assertions; use System.Assertions;

package body Comperr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Repeat_Char (Char : Character; Col : Nat; After : Character);
   --  Output Char until current column is at or past Col, and then output
   --  the character given by After (if column is already past Col on entry,
   --  then the effect is simply to output the After character).

   --------------------
   -- Compiler_Abort --
   --------------------

   procedure Compiler_Abort (X : String; Code : Integer := 0) is

      procedure End_Line;
      --  Add blanks up to column 76, and then a final vertical bar

      procedure End_Line is
      begin
         Repeat_Char (' ', 76, '|');
         Write_Eol;
      end End_Line;

   --  Start of processing for Compiler_Abort

   begin
      --  If errors have already occured, then we guess that the abort may
      --  well be caused by previous errors, and we don't make too much fuss
      --  about it, since we want to let the programmer fix the errors first.

      --  Debug flag K disables this behavior (useful for debugging)

      if Errors_Detected /= 0 and then not Debug_Flag_K then
         raise Unrecoverable_Error;

      --  Otherwise give message with details of the abort'

      else
         Set_Standard_Error;
         Write_Char ('+');
         Repeat_Char ('=', 29, 'G');
         Write_Str ("NAT BUG DETECTED");
         Repeat_Char ('=', 76, '+');
         Write_Eol;

         if Sloc (Fatal_Error_Node) <= Standard_Location
           or else Sloc (Fatal_Error_Node) = No_Location
         then
            Write_Str ("| No source file position information available");
            End_Line;
         else
            Write_Str ("| Error detected at ");
            Write_Location (Sloc (Fatal_Error_Node));
            End_Line;
         end if;

         Write_Str
           ("| Please submit bug report by email to report@gnat.com");
         End_Line;

         Write_Str
           ("| Use a subject line meaningful to you and us to track the bug");
         End_Line;

         Write_Str
           ("| Include full sources in ASCII in a format " &
            "compatible with gnatchop");
         End_Line;

         Write_Str
           ("| First line of sources must be marked by an Ada " &
            "-- comment line");
         End_Line;

         Write_Str
           ("| Last line of sources must be last line of " &
            "email message (no signature!)");
         End_Line;

         Write_Str
           ("| See gnatinfo.txt file for more info on procedure " &
            "for submitting bugs");
         End_Line;

         Write_Str ("| ");
         Write_Str (Gnatvsn.GNAT_Version_String);
         Write_Str (" (");

         --  Output target name, deleting junk final reverse slash

         if Target_Name.all (Target_Name.all'Last) = '\'
           or else Target_Name.all (Target_Name.all'Last) = '/'
         then
            Write_Str (Target_Name.all (1 .. Target_Name.all'Last - 1));
         else
            Write_Str (Target_Name.all);
         end if;

         Write_Str (") ");

         Write_Str (X);

         if Code /= 0 then
            Write_Str (", Code=");
            Write_Int (Int (Code));
         end if;

         if Assert_Msg_Length /= 0 then
            Write_Str (" at ");

            if Assert_Msg (Assert_Msg_Length) = Ascii.NUL then
               Assert_Msg_Length := Assert_Msg_Length - 1;
            end if;

            Write_Str (Assert_Msg (1 .. Assert_Msg_Length));
         end if;

         End_Line;

         Write_Char ('+');
         Repeat_Char ('=', 76, '+');
         Write_Eol;

         --  Otherwise output additional diagnostic information and terminate
         --  with a compilation abandoned message, but don't abort, instead
         --  raise Unrecoverable_Error to generate compilation abandoned msg.

         if Debug_Flag_3 then
            Write_Eol;
            Write_Eol;
            Print_Tree_Node (Fatal_Error_Node);
            Write_Eol;
         end if;

         Set_Standard_Output;
         Tree_Dump;
         Source_Dump;
         raise Unrecoverable_Error;
      end if;

   end Compiler_Abort;

   -----------------
   -- Repeat_Char --
   -----------------

   procedure Repeat_Char (Char : Character; Col : Nat; After : Character) is
   begin
      while Column < Col loop
         Write_Char (Char);
      end loop;

      Write_Char (After);
   end Repeat_Char;

end Comperr;
