------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  L I B                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.61 $                             --
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

with Atree;   use Atree;
with Einfo;   use Einfo;
with Fname;   use Fname;
with Namet;   use Namet;
with Namet;   use Namet;
with Output;  use Output;
with Sinfo;   use Sinfo;
with Sinput;  use Sinput;
with Stand;   use Stand;
with Stringt; use Stringt;
with Uname;   use Uname;

package body Lib is

   --------------------------------------------
   -- Access Functions for Unit Table Fields --
   --------------------------------------------

   function Cunit (U : Unit_Number_Type) return Node_Id is
   begin
      return Units.Table (U).Cunit;
   end Cunit;

   function Cunit_Entity (U : Unit_Number_Type) return Entity_Id is
   begin
      return Units.Table (U).Cunit_Entity;
   end Cunit_Entity;

   function Error_Location (U : Unit_Number_Type) return Source_Ptr is
   begin
      return Units.Table (U).Error_Location;
   end Error_Location;

   function Expected_Unit (U : Unit_Number_Type) return Unit_Name_Type is
   begin
      return Units.Table (U).Expected_Unit;
   end Expected_Unit;

   function Fatal_Error (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Fatal_Error;
   end Fatal_Error;

   function Generate_Code (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Generate_Code;
   end Generate_Code;

   function Ident_String (U : Unit_Number_Type) return String_Id is
   begin
      return Units.Table (U).Ident_String;
   end Ident_String;

   function Loading (U : Unit_Number_Type) return Boolean is
   begin
      return Units.Table (U).Loading;
   end Loading;

   function Main_Priority (U : Unit_Number_Type) return Int is
   begin
      return Units.Table (U).Main_Priority;
   end Main_Priority;

   function Source_Index (U : Unit_Number_Type) return Source_File_Index is
   begin
      return Units.Table (U).Source_Index;
   end Source_Index;

   function Unit_File_Name (U : Unit_Number_Type) return File_Name_Type is
   begin
      return Units.Table (U).Unit_File_Name;
   end Unit_File_Name;

   function Unit_Name (U : Unit_Number_Type) return Unit_Name_Type is
   begin
      return Units.Table (U).Unit_Name;
   end Unit_Name;

   ------------------------------------------
   -- Subprograms to Set Unit Table Fields --
   ------------------------------------------

   procedure Set_Cunit (U : Unit_Number_Type; N : Node_Id) is
   begin
      Units.Table (U).Cunit := N;
   end Set_Cunit;

   procedure Set_Cunit_Entity (U : Unit_Number_Type; E : Entity_Id) is
   begin
      Units.Table (U).Cunit_Entity := E;
   end Set_Cunit_Entity;

   procedure Set_Error_Location (U : Unit_Number_Type; W : Source_Ptr) is
   begin
      Units.Table (U).Error_Location := W;
   end Set_Error_Location;

   procedure Set_Fatal_Error (U : Unit_Number_Type; B : Boolean := True) is
   begin
      Units.Table (U).Fatal_Error := True;
   end Set_Fatal_Error;

   procedure Set_Generate_Code (U : Unit_Number_Type; B : Boolean := True) is
   begin
      Units.Table (U).Generate_Code := B;
   end Set_Generate_Code;

   procedure Set_Ident_String (U : Unit_Number_Type; S : String_Id) is
   begin
      Units.Table (U).Ident_String := S;
   end Set_Ident_String;

   procedure Set_Loading (U : Unit_Number_Type; B : Boolean := True) is
   begin
      Units.Table (U).Loading := B;
   end Set_Loading;

   procedure Set_Main_Priority (U : Unit_Number_Type; P : Int) is
   begin
      Units.Table (U).Main_Priority := P;
   end Set_Main_Priority;

   procedure Set_Unit_Name (U : Unit_Number_Type; N : Unit_Name_Type) is
   begin
      Units.Table (U).Unit_Name := N;
   end Set_Unit_Name;

   ----------------------------
   -- Entity_Is_In_Main_Unit --
   ----------------------------

   function Entity_Is_In_Main_Unit (E : Entity_Id) return Boolean is
      S : Entity_Id;

   begin
      S := Scope (E);

      while S /= Standard_Standard loop
         if S = Main_Unit_Entity then
            return True;
         elsif Ekind (S) = E_Package and then Is_Child_Unit (S) then
            return False;
         else
            S := Scope (S);
         end if;
      end loop;

      return False;
   end Entity_Is_In_Main_Unit;

   --------------------------
   -- Get_Sloc_Unit_Number --
   --------------------------

   function Get_Sloc_Unit_Number
     (S    : Source_Ptr)
      return Unit_Number_Type
   is
      Source_File : Source_File_Index := Get_Source_File_Index (S);

   begin
      Source_File := Get_Source_File_Index (S);
      while Template (Source_File) /= No_Source_File loop
         Source_File := Template (Source_File);
      end loop;

      for U in Units.First .. Units.Last loop
         if Source_Index (U) = Source_File then
            return U;
         end if;
      end loop;

      --  If not in the table, must be the main source unit, and we just
      --  have not got it put into the table yet.

      return Main_Unit;
   end Get_Sloc_Unit_Number;

   ---------------------------
   -- Get_Cunit_Unit_Number --
   ---------------------------

   function Get_Cunit_Unit_Number (N : Node_Id) return Unit_Number_Type is
   begin
      for U in Units.First .. Units.Last loop
         if Cunit (U) = N then
            return U;
         end if;
      end loop;

      --  If not in the table, must be the main source unit, and we just
      --  have not got it put into the table yet.

      return Main_Unit;
   end Get_Cunit_Unit_Number;

   ----------------------------------
   -- Get_Cunit_Entity_Unit_Number --
   ----------------------------------

   function Get_Cunit_Entity_Unit_Number
     (E    : Entity_Id)
      return Unit_Number_Type
   is
   begin
      for U in Units.First .. Units.Last loop
         if Cunit_Entity (U) = E then
            return U;
         end if;
      end loop;

      --  If not in the table, must be the main source unit, and we just
      --  have not got it put into the table yet.

      return Main_Unit;
   end Get_Cunit_Entity_Unit_Number;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Linker_Option_Lines.Init;
      Load_Stack.Init;
      Units.Init;
   end Initialize;

   ---------------
   -- Is_Loaded --
   ---------------

   function Is_Loaded (Uname : Unit_Name_Type) return Boolean is
   begin
      for Unum in Units.First .. Units.Last loop
         if Uname = Unit_Name (Unum) then
            return True;
         end if;
      end loop;

      return False;
   end Is_Loaded;

   ---------------
   -- Last_Unit --
   ---------------

   function Last_Unit return Unit_Number_Type is
   begin
      return Units.Last;
   end Last_Unit;

   ----------
   -- List --
   ----------

   procedure List is separate;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Linker_Option_Lines.Locked := True;
      Load_Stack.Locked := True;
      Units.Locked := True;
      Linker_Option_Lines.Release;
      Load_Stack.Release;
      Units.Release;
   end Lock;

   ---------------
   -- Num_Units --
   ---------------

   function Num_Units return Nat is
   begin
      return Int (Units.Last) - Int (Main_Unit) + 1;
   end Num_Units;

   ----------
   -- Sort --
   ----------

   procedure Sort (Tbl : in out Unit_Ref_Table) is separate;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      Units.Tree_Read;
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Units.Tree_Write;
   end Tree_Write;

   -----------------
   -- Version_Get --
   -----------------

   function Version_Get (U : Unit_Number_Type) return Word_Hex_String is
   begin
      return Get_Hex_String (Units.Table (U).Version);
   end Version_Get;

end Lib;
