------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         I N T E R F A C E S . C                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with System;
with Unchecked_Conversion;

package body Interfaces.C is

   --  The following bodies are temporary, see documentation in spec ???

   function To_C (Item : Character) return char is
   begin
      return Character_To_char (Item);
   end To_C;

   function To_Ada (Item : char) return Character is
   begin
      return char_To_Character (Item);
   end To_Ada;

   function To_C (Item : in Wide_Character) return wchar_t is
   begin
      return Wide_Character_To_wchar_t (Item);
   end To_C;

   function To_Ada (Item : in wchar_t) return Wide_Character is
   begin
      return wchar_t_To_Wide_Character (Item);
   end To_Ada;

   -----------------------
   -- Is_Nul_Terminated --
   -----------------------

   --  Case of char_array

   function Is_Nul_Terminated (Item : in char_array) return Boolean is
   begin
      for J in Item'Range loop
         if Item (J) = nul then
            return True;
         end if;
      end loop;

      return False;
   end Is_Nul_Terminated;

   --  Case of wchar_array

   function Is_Nul_Terminated (Item : in wchar_array) return Boolean is
   begin
      for J in Item'Range loop
         if Item (J) = wide_nul then
            return True;
         end if;
      end loop;

      return False;
   end Is_Nul_Terminated;

   ------------
   -- To_Ada --
   ------------

   --  Convert char_array to String (function form)

   function To_Ada
     (Item     : in char_array;
      Trim_Nul : in Boolean := True)
      return     String
   is
      Count : Natural;
      From  : size_t;

   begin
      if Trim_Nul then
         From := Item'First;

         loop
            exit when Item (From) = nul;

            if From = Item'Last then
               raise Terminator_Error;
            else
               From := From + 1;
            end if;
         end loop;

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      declare
         subtype Return_Type is String (1 .. Count);
         type Return_Type_Ptr is access Return_Type;
         function To_Return_Type_Ptr is
           new Unchecked_Conversion (System.Address, Return_Type_Ptr);

      begin
         return To_Return_Type_Ptr (Item'Address).all;
      end;
   end To_Ada;

   --  Convert char_array to String (procedure form)

   procedure To_Ada
     (Item       : in char_array;
      Target     : out String;
      Count      : out Natural;
      Trim_Nul   : in Boolean := True)
   is
      From   : size_t;

   begin
      if Trim_Nul then
         From := Item'First;
         loop
            exit when Item (From) = nul;

            if From = Item'Last then
               raise Terminator_Error;
            else
               From := From + 1;
            end if;
         end loop;

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      if Count > Target'Length then
         raise Constraint_Error;

      else
         From := Item'First;
         for To in Target'Range loop
            Target (To) := Character (Item (From));
            From := From + 1;
         end loop;
      end if;

   end To_Ada;

   --  Convert wchar_array to Wide_String (function form)

   function To_Ada
     (Item     : in wchar_array;
      Trim_Nul : in Boolean := True)
      return     Wide_String
   is
      Count : Natural;
      From  : size_t;

   begin
      if Trim_Nul then
         From := Item'First;

         loop
            exit when Item (From) = wide_nul;

            if From = Item'Last then
               raise Terminator_Error;
            else
               From := From + 1;
            end if;
         end loop;

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      declare
         subtype Return_Type is Wide_String (1 .. Count);
         type Return_Type_Ptr is access Return_Type;
         function To_Return_Type_Ptr is
           new Unchecked_Conversion (System.Address, Return_Type_Ptr);

      begin
         return To_Return_Type_Ptr (Item'Address).all;
      end;
   end To_Ada;

   --  Convert wchar_array to Wide_String (procedure form)

   procedure To_Ada
     (Item       : in wchar_array;
      Target     : out Wide_String;
      Count      : out Natural;
      Trim_Nul   : in Boolean := True)
   is
      From   : size_t;

   begin
      if Trim_Nul then
         From := Item'First;
         loop
            exit when Item (From) = wide_nul;

            if From = Item'Last then
               raise Terminator_Error;
            else
               From := From + 1;
            end if;
         end loop;

         Count := Natural (From - Item'First);

      else
         Count := Item'Length;
      end if;

      if Count > Target'Length then
         raise Constraint_Error;

      else
         From := Item'First;
         for To in Target'Range loop
            Target (To) := Wide_Character (Item (From));
            From := From + 1;
         end loop;
      end if;

   end To_Ada;

   ----------
   -- To_C --
   ----------

   --  Convert String to char_array (function form)

   function To_C
     (Item       : in String;
      Append_Nul : in Boolean := True)
      return       char_array
   is
   begin
      --  If appending null, we have to make a copy

      if Append_Nul then
         declare
            Target : char_array (0 .. Item'Length);
            To     : size_t;

         begin
            To := 0;
            for From in Item'Range loop
               Target (To) := char (Item (From));
               To := To + 1;
            end loop;

            Target (Item'Length) := nul;
            return Target;
         end;

      --  If not appending null, we can use unchecked conversion to return
      --  the result, since we know in GNAT there is structural equivalence.

      else
         declare
            subtype Return_Type is char_array (0 .. Item'Length - 1);
            type Return_Type_Ptr is access Return_Type;
            function To_Return_Type_Ptr is
              new Unchecked_Conversion (System.Address, Return_Type_Ptr);

         begin
            return To_Return_Type_Ptr (Item'Address).all;
         end;
      end if;
   end To_C;

   --  Convert String to char_array (procedure form)

   procedure To_C
     (Item       : in String;
      Target     : out char_array;
      Count      : out size_t;
      Append_Nul : in  Boolean := True)
   is
      To : size_t;

   begin
      if Target'Length < Item'Length then
         raise Constraint_Error;

      else
         To := Target'First;
         for From in Item'Range loop
            Target (To) := char (Item (From));
            To := To + 1;
         end loop;

         Count := Item'Length;

         if Append_Nul then
            if To > Target'Last then
               raise Constraint_Error;
            else
               Target (To) := nul;
               Count := Count + 1;
            end if;
         end if;
      end if;
   end To_C;

   --  Convert Wide_String to wchar_array (function form)

   function To_C
     (Item       : in Wide_String;
      Append_Nul : in Boolean := True)
      return       wchar_array
   is
   begin
      --  If appending null, we have to make a copy

      if Append_Nul then
         declare
            Target : wchar_array (0 .. Item'Length);
            To     : size_t;

         begin
            To := 0;
            for From in Item'Range loop
               Target (To) := wchar_t (Item (From));
               To := To + 1;
            end loop;

            Target (Item'Length) := wide_nul;
            return Target;
         end;

      --  If not appending null, we can use unchecked conversion to return
      --  the result, since we know in GNAT there is structural equivalence.

      else
         declare
            subtype Return_Type is wchar_array (0 .. Item'Length - 1);
            type Return_Type_Ptr is access Return_Type;
            function To_Return_Type_Ptr is
              new Unchecked_Conversion (System.Address, Return_Type_Ptr);

         begin
            return To_Return_Type_Ptr (Item'Address).all;
         end;
      end if;
   end To_C;

   --  Convert Wide_String to wchar_array (procedure form)

   procedure To_C
     (Item       : in Wide_String;
      Target     : out wchar_array;
      Count      : out size_t;
      Append_Nul : in  Boolean := True)
   is
      To : size_t;

   begin
      if Target'Length < Item'Length then
         raise Constraint_Error;

      else
         To := Target'First;
         for From in Item'Range loop
            Target (To) := wchar_t (Item (From));
            To := To + 1;
         end loop;

         Count := Item'Length;

         if Append_Nul then
            if To > Target'Last then
               raise Constraint_Error;
            else
               Target (To) := wide_nul;
               Count := Count + 1;
            end if;
         end if;
      end if;
   end To_C;

end Interfaces.C;
