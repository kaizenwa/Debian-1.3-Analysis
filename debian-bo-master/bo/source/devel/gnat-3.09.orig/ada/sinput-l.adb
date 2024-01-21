------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.12 $                             --
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

with Alloc;
with Atree;  use Atree;
with Debug;  use Debug;
with Einfo;  use Einfo;
with Namet;  use Namet;
with Opt;
with Osint;  use Osint;
with Output; use Output;
with Scans;  use Scans;
with Sinfo;  use Sinfo;
with System; use System;

with Unchecked_Conversion;

package body Sinput.L is

   -------------------------------
   -- Adjust_Instantiation_Sloc --
   -------------------------------

   procedure Adjust_Instantiation_Sloc (N : Node_Id; A : Sloc_Adjustment) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      --  We only do the adjustment if the value is between the appropriate
      --  low and high values. It is not clear that this should ever not be
      --  the case, but in practice there seem to be some nodes that get
      --  copied twice, and this is a defence against that happening.

      if A.Lo <= Loc and then Loc <= A.Hi then
         Set_Sloc (N, Loc + A.Adjust);
      end if;
   end Adjust_Instantiation_Sloc;

   --------------------------------
   -- Complete_Source_File_Entry --
   --------------------------------

   procedure Complete_Source_File_Entry is

      function realloc
        (P        : Lines_Table_Ptr;
         New_Size : Int)
         return     Lines_Table_Ptr;
      pragma Import (C, realloc);

      CSF : constant Source_File_Index := Current_Source_File;
      Max : constant Pos := Source_File.Table (CSF).Num_Source_Lines;

   begin
      --  Release allocated storage that is no longer needed

      Source_File.Table (CSF).Lines_Table :=
        realloc
          (Source_File.Table (CSF).Lines_Table,
           Max * (Lines_Table_Type'Component_Size / System.Storage_Unit));
      Source_File.Table (CSF).Lines_Table_Max := Max;

      --  Set checksum

      Source_File.Table (CSF).Source_Checksum := Checksum;

   end Complete_Source_File_Entry;

   ---------------------------------
   -- Create_Instantiation_Source --
   ---------------------------------

   procedure Create_Instantiation_Source
     (Inst_Node   : Entity_Id;
      Template_Id : Entity_Id;
      A           : out Sloc_Adjustment)
   is
      Dnod : constant Node_Id := Declaration_Node (Template_Id);
      Xold : Source_File_Index;
      Xnew : Source_File_Index;

   begin
      Xold := Get_Source_File_Index (Sloc (Template_Id));
      A.Lo := Source_File.Table (Xold).Source_First;
      A.Hi := Source_File.Table (Xold).Source_Last;

      Source_File.Increment_Last;
      Xnew := Source_File.Last;

      Source_File.Table (Xnew)               := Source_File.Table (Xold);
      Source_File.Table (Xnew).Instantiation := Sloc (Inst_Node);
      Source_File.Table (Xnew).Template      := Xold;

      --  Now we need to compute the new values of Source_First, Source_Last
      --  and adjust the source file pointer to have the correct virtual
      --  origin for the new range of values.

      Source_File.Table (Xnew).Source_First :=
        Source_File.Table (Xnew - 1).Source_Last + 1;

      A.Adjust := Source_File.Table (Xnew).Source_First - A.Lo;
      Source_File.Table (Xnew).Source_Last := A.Hi + A.Adjust;

      Source_File.Table (Xnew).Sloc_Adjust :=
        Source_File.Table (Xold).Sloc_Adjust - A.Adjust;

      if Debug_Flag_L then
         Write_Eol;
         Write_Str ("*** Create instantiation source for ");

         if Was_Originally_Stub (Dnod) then
            Write_Str ("subunit ");

         elsif Ekind (Template_Id) = E_Generic_Package then
            if Nkind (Dnod) = N_Package_Body then
               Write_Str ("body of package ");
            else
               Write_Str ("spec of package ");
            end if;

         elsif Ekind (Template_Id) = E_Function then
            Write_Str ("body of function ");

         elsif Ekind (Template_Id) = E_Procedure then
            Write_Str ("body of procedure ");

         elsif Ekind (Template_Id) = E_Generic_Function then
            Write_Str ("spec of function ");

         elsif Ekind (Template_Id) = E_Generic_Procedure then
            Write_Str ("spec of procedure ");

         else
            pragma Assert (False); null;
         end if;

         Write_Name (Chars (Template_Id));
         Write_Eol;

         Write_Str ("  new source index = ");
         Write_Int (Int (Xnew));
         Write_Eol;

         Write_Str ("  copying from file name = ");
         Write_Name (File_Name (Xold));
         Write_Eol;

         Write_Str ("  old source index = ");
         Write_Int (Int (Xold));
         Write_Eol;

         Write_Str ("  old lo = ");
         Write_Int (Int (A.Lo));
         Write_Eol;

         Write_Str ("  old hi = ");
         Write_Int (Int (A.Hi));
         Write_Eol;

         Write_Str ("  new lo = ");
         Write_Int (Int (Source_File.Table (Xnew).Source_First));
         Write_Eol;

         Write_Str ("  new hi = ");
         Write_Int (Int (Source_File.Table (Xnew).Source_Last));
         Write_Eol;

         Write_Str ("  adjustment factor = ");
         Write_Int (Int (A.Adjust));
         Write_Eol;

         Write_Str ("  instantiation location: ");
         Write_Location (Sloc (Inst_Node));
         Write_Eol;
      end if;

      --  For a given character in the source, a higher subscript will be
      --  used to access the instantiation, which means that the virtual
      --  origin must have a corresponding lower value. We compute this
      --  new origin by taking the address of the appropriate adjusted
      --  element in the old array. Since this adjusted element will be
      --  at a negative subscript, we must suppress checks.

      declare
         pragma Suppress (All_Checks);

         function To_Source_Buffer_Ptr is new
           Unchecked_Conversion (Address, Source_Buffer_Ptr);

      begin
         Source_File.Table (Xnew).Source_Text :=
           To_Source_Buffer_Ptr
             (Source_File.Table (Xold).Source_Text (-A.Adjust)'Address);
      end;

   end Create_Instantiation_Source;

   ----------------------
   -- Load_Source_File --
   ----------------------

   function Load_Source_File
     (N    : File_Name_Type)
      return Source_File_Index
   is
      Src  : Source_Buffer_Ptr;
      Lptr : Lines_Table_Ptr;
      X    : Source_File_Index;
      Lo   : Source_Ptr;
      Hi   : Source_Ptr;

   begin
      for J in 1 .. Source_File.Last loop
         if Source_File.Table (J).File_Name = N then
            return J;
         end if;
      end loop;

      --  Here we must build a new entry in the file table

      Source_File.Increment_Last;
      X := Source_File.Last;

      if X = Source_File.First then
         Lo := First_Source_Ptr;
      else
         Lo := Source_File.Table (X - 1).Source_Last + 1;
      end if;

      Read_Source_File (N, Lo, Hi, Src);

      if Src = null then
         Source_File.Decrement_Last;
         return No_Source_File;

      else
         if Debug_Flag_L then
            Write_Eol;
            Write_Str ("*** Build source file table entry, Index = ");
            Write_Int (Int (X));
            Write_Str (", file name = ");
            Write_Name (N);
            Write_Eol;
            Write_Str ("  lo = ");
            Write_Int (Int (Lo));
            Write_Eol;
            Write_Str ("  hi = ");
            Write_Int (Int (Hi));
            Write_Eol;

            Write_Str ("  first 10 chars -->");

            declare
               procedure Wchar (C : Character);
               --  Writes character or ? for control character

               procedure Wchar (C : Character) is
               begin
                  if C < ' ' or C in Ascii.DEL .. Character'Val (16#9F#) then
                     Write_Char ('?');
                  else
                     Write_Char (C);
                  end if;
               end Wchar;

            begin
               for J in Lo .. Lo + 9 loop
                  Wchar (Src (J));
               end loop;

               Write_Str ("<--");
               Write_Eol;

               Write_Str ("  last 10 chars  -->");

               for J in Hi - 10 .. Hi - 1 loop
                  Wchar (Src (J));
               end loop;

               Write_Str ("<--");
               Write_Eol;

               if Src (Hi) /= EOF then
                  Write_Str ("  error: no EOF at end");
                  Write_Eol;
               end if;
            end;
         end if;

         declare
            S : Source_File_Record renames Source_File.Table (X);

         begin
            S.File_Name         := N;
            S.Reference_Name    := N;
            S.Full_File_Name    := Full_Source_Name;
            S.Full_Ref_Name     := Full_Source_Name;
            S.Line_Offset       := 0;
            S.Source_Text       := Src;
            S.Source_First      := Lo;
            S.Source_Last       := Hi;
            S.Time_Stamp        := Current_Source_File_Stamp;
            S.Source_Checksum   := 0;
            S.Lines_Table       := null;
            S.Num_Source_Lines  := 1;
            S.Keyword_Casing    := Unknown;
            S.Identifier_Casing := Unknown;
            S.Instantiation     := No_Location;
            S.Template          := No_Source_File;
            S.Sloc_Adjust       := 0;

            Alloc_Lines_Table (X, Opt.Table_Factor * Alloc.Lines_Initial);
            S.Lines_Table (1) := Lo;
         end;

         return X;
      end if;
   end Load_Source_File;

end Sinput.L;
