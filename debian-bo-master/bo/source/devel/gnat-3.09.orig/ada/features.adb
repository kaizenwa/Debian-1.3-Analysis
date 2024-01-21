------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F E A T U R E S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.17 $                             --
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
with Csets;    use Csets;
with Hostparm;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Sinput;   use Sinput;
with Uname;    use Uname;
with Table;

with GNAT.Heap_Sort_A; use GNAT.Heap_Sort_A;

package body Features is

   --  Data structures used to record feature references. Note that the entry
   --  with index zero is used only as a temporary for the sort routine.

   type Feature_Ref is record
      F : Feature_Name;
      L : Source_Ptr;
   end record;

   package Feature_List is new Table (
     Table_Component_Type => Feature_Ref,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Feature_List_Initial,
     Table_Increment      => Alloc.Feature_List_Increment,
     Table_Name           => "Feature_List");

   --  Data structures used to record with'ed units. Note that the entry
   --  with index zero is used only as a temporary for the sort routine.

   type With_Ref is record
      U : Unit_Name_Type;
      L : Source_Ptr;
   end record;

   package With_List is new Table (
     Table_Component_Type => With_Ref,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.With_List_Initial,
     Table_Increment      => Alloc.With_List_Increment,
     Table_Name           => "With_List");

   --  Other global data

   Multiple_Files : Boolean := False;
   --  Set to True if references for more than one file are

   Last_Index : Source_File_Index := No_Source_File;
   --  Set to source table index of last file for which a reference was output.
   --  Used in multiple file case only, not set or read otherwise.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Lt_Feature (Op1, Op2 : Natural) return Boolean;
   --  Comparison routine for comparing Feature_List table entries

   function Lt_Slocs (Op1, Op2 : Source_Ptr) return Boolean;
   --  Comparison routine used to compare two Sloc values to determine
   --  the order in which the references should be output in the table.

   function Lt_Units (Op1, Op2 : Unit_Name_Type) return Boolean;
   --  Comparison routine for comparing two unit numbers, by alphabetical
   --  comparison of the corresponding unit names.

   function Lt_With (Op1, Op2 : Natural) return Boolean;
   --  Comparison routine for comparing Feature_List table entries

   procedure Move_Feature (From : Natural; To : Natural);
   --  Move routine for sorting the Feature_List table

   procedure Move_With (From : Natural; To : Natural);
   --  Move routine for sorting the With_List table

   procedure Write_Ref (L : Source_Ptr);
   --  Writes a single reference dealing with lining up columns nicely

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      Index : Natural;

   begin
      if not Features_On then
         return;
      end if;

      Write_Eol;

      --  Output features list

      if Feature_List.Last = 0 then
         Write_Str ("No use of Ada 95 features recorded");
         Write_Eol;

      else
         --  If entries present, first sort them

         Sort (Feature_List.Last, Move_Feature'Access, Lt_Feature'Access);

         --  Then remove duplicate entries, which can arise from multiple
         --  recording of the same use in different parts of the compiler.

         declare
            N : Natural := 1;

         begin
            for J in 2 .. Feature_List.Last loop
               if Feature_List.Table (J) /= Feature_List.Table (J - 1) then
                  N := N + 1;
                  Feature_List.Table (N) := Feature_List.Table (J);
               end if;
            end loop;

            Feature_List.Set_Last (N);
         end;

         --  Now generate output listing

         Write_Str ("Use of Ada 95 Features");

         --  Loop through features in table

         Index := 1;

         Features_Loop : loop
            declare
               F : constant Feature_Name := Feature_List.Table (Index).F;
               S : String                := Feature_Name'Image (F);

            begin
               Write_Eol;
               Write_Eol;
               Write_Str (Code_Names (F));
               Write_Char (' ');

               for J in 2 .. S'Length loop
                  if S (J) = '_' then
                     S (J) := ' ';
                  else
                     S (J) := Fold_Lower (S (J));
                  end if;
               end loop;

               Write_Str (S);
               Write_Eol;

               if not Multiple_Files then
                  Write_Str ("  ");
               end if;

               Last_Index := No_Source_File;

               --  Loop through entries for single feature

               Ref_Loop : loop
                  exit Features_Loop when Index > Feature_List.Last;
                  exit Ref_Loop when Feature_List.Table (Index).F /= F;
                  Write_Ref (Feature_List.Table (Index).L);
                  Index := Index + 1;
               end loop Ref_Loop;
            end;
         end loop Features_Loop;
      end if;

      --  Output with'ed unit table use table

      return;
      --  ??? for now, next section not implemented yet

      Write_Eol;
      Write_Eol;

      if With_List.Last = 0 then
         Write_Str ("No use of Ada 95 Library Units Recorded");
         Write_Eol;
         Write_Eol;

      else
         --  If entries present, first sort them

         Sort (With_List.Last, Move_With'Access, Lt_With'Access);

         --  Then remove duplicate entries, which can arise from multiple
         --  recording of the same use in different parts of the compiler.

         declare
            N : Natural := 1;

         begin
            for J in 2 .. With_List.Last loop
               if With_List.Table (J) /= With_List.Table (J - 1) then
                  N := N + 1;
                  With_List.Table (N) := With_List.Table (J);
               end if;
            end loop;

            With_List.Set_Last (N);
         end;

         --  Now generate output listing

         Write_Str ("Use of Ada 95 Library Units Recorded");
         Write_Eol;
         Write_Str ("------------------------------------");
         Write_Eol;

         --  Loop through with'ed units in table

         Index := 1;

         With_Loop : loop
            declare
               U : constant Unit_Name_Type := With_List.Table (Index).U;

            begin
               Write_Eol;
               Write_Unit_Name (U);
               Write_Char (' ');
               Last_Index := No_Source_File;

               --  Loop through entries for single with'ed unit

               Ref_With_Loop : loop
                  exit With_Loop when Index > With_List.Last;
                  exit Ref_With_Loop when With_List.Table (Index).U /= U;
                  Write_Ref (With_List.Table (Index).L);
                  Index := Index + 1;
               end loop Ref_With_Loop;
            end;
         end loop With_Loop;
      end if;

   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if Xref_Flag_9 then
         Features_On := True;
         Feature_List.Init;
         With_List.Init;

         --  Allocate zero index entries at the start of the tables (used by
         --  sort routine as temporaries, not otherwise used for real entries)

         Feature_List.Increment_Last;
         With_List.Increment_Last;
      end if;
   end Initialize;

   ----------------
   -- Lt_Feature --
   ----------------

   function Lt_Feature (Op1, Op2 : Natural) return Boolean is
   begin
      if Feature_List.Table (Op1).F /= Feature_List.Table (Op2).F then
         return Feature_List.Table (Op1).F < Feature_List.Table (Op2).F;
      else
         return
           Lt_Slocs (Feature_List.Table (Op1).L, Feature_List.Table (Op2).L);
      end if;
   end Lt_Feature;

   --------------
   -- Lt_Slocs --
   --------------

   function Lt_Slocs (Op1, Op2 : Source_Ptr) return Boolean is
      Op1_Unit : constant Unit_Name_Type :=
                   Unit_Name (Get_Sloc_Unit_Number (Op1));
      Op2_Unit : constant Unit_Name_Type :=
                   Unit_Name (Get_Sloc_Unit_Number (Op2));

   begin
      if Op1_Unit = Op2_Unit then
         return Op1 < Op2;
      else
         return Lt_Units (Op1_Unit, Op2_Unit);
      end if;
   end Lt_Slocs;

   --------------
   -- Lt_Units --
   --------------

   function Lt_Units (Op1, Op2 : Unit_Name_Type) return Boolean is
      Op1_Name     : String (1 .. Hostparm.Max_Name_Length);
      Op1_Name_Len : Natural;
      Op2_Name     : String renames Name_Buffer;
      Op2_Name_Len : Natural renames Name_Len;

   begin
      Get_Name_String (Op1);
      Op1_Name (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
      Op1_Name_Len := Name_Len;
      Get_Name_String (Op2);

      for J in 1 .. Name_Len loop
         if Op1_Name (J) /= Op2_Name (J) then
            return Op1_Name (J) < Op2_Name (J);
         end if;
      end loop;

      return Op1_Name_Len < Op2_Name_Len;
   end Lt_Units;

   -------------
   -- Lt_With --
   -------------

   function Lt_With (Op1, Op2 : Natural) return Boolean is
      Op1_Unit : constant Unit_Name_Type := With_List.Table (Op1).U;
      Op2_Unit : constant Unit_Name_Type := With_List.Table (Op2).U;

   begin
      if Op1_Unit /= Op2_Unit then
         return Lt_Units (Op1_Unit, Op2_Unit);
      else
         return Lt_Slocs (With_List.Table (Op1).L, With_List.Table (Op2).L);
      end if;
   end Lt_With;

   ------------------
   -- Move_Feature --
   ------------------

   procedure Move_Feature (From : Natural; To : Natural) is
   begin
      Feature_List.Table (To) := Feature_List.Table (From);
   end Move_Feature;

   ---------------
   -- Move_With --
   ---------------

   procedure Move_With (From : Natural; To : Natural) is
   begin
      With_List.Table (To) := With_List.Table (From);
   end Move_With;

   ------------------
   -- Note_Feature --
   ------------------

   procedure Note_Feature (F : Feature_Name; Loc : Source_Ptr) is
   begin
      if Features_On
        and then Loc in Source_Text (Source_Index (Main_Unit))'Range
      then
         Feature_List.Increment_Last;
         Feature_List.Table (Feature_List.Last) := (F => F, L => Loc);
      end if;
   end Note_Feature;

   ---------------
   -- Note_With --
   ---------------

   procedure Note_With (U : Unit_Name_Type; Loc : Source_Ptr) is
   begin
      if Loc in Source_Text (Source_Index (Main_Unit))'Range then
         With_List.Increment_Last;
         With_List.Table (With_List.Last) := (U => U, L => Loc);
      end if;
   end Note_With;

   ---------------
   -- Write_Ref --
   ---------------

   procedure Write_Ref (L : Source_Ptr) is
      Lin : Logical_Line_Number;
      Col : Column_Number;

   begin
      --  For multiple file case, new line if file name changes

      if Multiple_Files then
         if Last_Index = No_Source_File
           or else L not in Source_Text (Last_Index)'Range
         then
            Write_Eol;
            Last_Index := Get_Source_File_Index (L);
            Write_Str ("  ");
            Write_Name (File_Name (Last_Index));
            Write_Eol;
            Write_Str ("  ");
         end if;
      end if;

      --  Start new line if current line is full

      if Column > 72 then
         Write_Eol;

         if Multiple_Files then
            Write_Str ("    ");
         else
            Write_Str ("  ");
         end if;
      end if;

      --  We do a bit of padding on the line and column number so that in
      --  the most usual cases, the references line up nicely in columns

      Lin := Get_Line_Number (L);

      if Lin < 1000 then
         Write_Char (' ');

         if Lin < 100 then
            Write_Char (' ');

            if Lin < 10 then
               Write_Char (' ');
            end if;
         end if;
      end if;

      Write_Int (Int (Lin));
      Col := Get_Column_Number (L);
      Write_Char ('(');

      if Col < 10 then
         Write_Char ('0');
      end if;

      Write_Int (Int (Col));
      Write_Char (')');
      Write_Char (' ');
   end Write_Ref;

end Features;
