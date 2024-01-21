------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . L O A D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.57 $                             --
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

with Atree;    use Atree;
with Debug;    use Debug;
with Errout;   use Errout;
with Fname;    use Fname;
with Osint;    use Osint;
with Output;   use Output;
with Par;
with Restrict; use Restrict;
with Scn;      use Scn;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with Uname;    use Uname;

package body Lib.Load is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Spec_Is_Irrelevant
     (Spec_Unit : Unit_Number_Type;
      Body_Unit : Unit_Number_Type)
      return      Boolean;
   --  The Spec_Unit and Body_Unit parameters are the unit numbers of the
   --  spec file that corresponds to the main unit which is a body. This
   --  function determines if the spec file is irrelevant and will be
   --  overridden by the body as described in RM 10.1.4(4). See description
   --  in "Special Handling of Subprogram Bodies" for further details.

   procedure Write_Dependency_Chain;
   --  This procedure is used to generate error message info lines that
   --  trace the current dependency chain when a load error occurs.

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Fname : File_Name_Type;

   begin
      Units.Init;
      Load_Stack.Init;
      Load_Stack.Increment_Last;
      Load_Stack.Table (Load_Stack.Last) := Main_Unit;

      --  Initialize unit table entry for Main_Unit. Note that we don't know
      --  the unit name yet, that gets filled in when the parser parses the
      --  main unit, at which time a check is made that it matches the main
      --  file name, and then the Unit_Name field is set. The Cunit and
      --  Cunit_Entity fields also get filled in later by the parser.

      Units.Increment_Last;
      Fname := Next_Main_Source;

      Units.Table (Main_Unit).Unit_File_Name := Fname;

      if Fname /= No_File then
         Units.Table (Main_Unit).Cunit           := Empty;
         Units.Table (Main_Unit).Cunit_Entity    := Empty;
         Units.Table (Main_Unit).Def_Func_Count  := 0;
         Units.Table (Main_Unit).Error_Location  := No_Location;
         Units.Table (Main_Unit).Expected_Unit   := No_Name;
         Units.Table (Main_Unit).Fatal_Error     := False;
         Units.Table (Main_Unit).Generate_Code   := False;
         Units.Table (Main_Unit).Loading         := True;
         Units.Table (Main_Unit).Ident_String    := No_String;
         Units.Table (Main_Unit).Main_Priority   := Default_Main_Priority;
         Units.Table (Main_Unit).Serial_Number   := 0;
         Units.Table (Main_Unit).Serial_Ref_Unit := Main_Unit;
         Units.Table (Main_Unit).Source_Index    := Load_Source_File (Fname);
         Units.Table (Main_Unit).Unit_Name       := No_Name;
         Units.Table (Main_Unit).Version         :=
           Source_Checksum (Units.Table (Main_Unit).Source_Index);
      end if;
   end Initialize;

   ------------------------
   -- Initialize_Version --
   ------------------------

   procedure Initialize_Version (U : Unit_Number_Type) is
   begin
      Units.Table (U).Version := Source_Checksum (Source_Index (U));
   end Initialize_Version;

   ------------------------
   -- Spec_Is_Irrelevant --
   ------------------------

   function Spec_Is_Irrelevant
     (Spec_Unit : Unit_Number_Type;
      Body_Unit : Unit_Number_Type)
      return      Boolean
   is
      Sunit : constant Node_Id := Cunit (Spec_Unit);
      Bunit : constant Node_Id := Cunit (Body_Unit);

   begin
      --  The spec is irrelevant if the body is a subprogram body, and the
      --  spec is other than a subprogram spec or generic subprogram spec.
      --  Note that the names must be the same, we don't need to check that,
      --  because we already know that from the fact that the file names are
      --  the same.

      return
         Nkind (Unit (Bunit)) = N_Subprogram_Body
           and then Nkind (Unit (Sunit)) /= N_Subprogram_Declaration
           and then Nkind (Unit (Sunit)) /= N_Generic_Subprogram_Declaration;

   end Spec_Is_Irrelevant;

   ---------------
   -- Load_Unit --
   ---------------

   function Load_Unit
     (Uname     : Unit_Name_Type;
      Required  : Boolean;
      Enode     : Node_Id;
      Corr_Body : Unit_Number_Type := No_Unit)
      return      Unit_Number_Type
   is
      Calling_Unit : Unit_Number_Type;
      Unum         : Unit_Number_Type;
      Fname        : File_Name_Type := Get_File_Name (Uname);
      Src_Ind      : Source_File_Index;
      Discard      : List_Id;

   begin
      if Debug_Flag_L then
         Write_Eol;
         Write_Str ("*** Load request for unit: ");
         Write_Unit_Name (Uname);

         if Required then
            Write_Str (" (Required = True)");
         else
            Write_Str (" (Required = False)");
         end if;

         Write_Eol;
      end if;

      --  Capture error location if it is for the main unit. The idea is to
      --  post errors on the main unit location, not the most recent unit.

      if Present (Enode)
        and then Get_Sloc_Unit_Number (Sloc (Enode)) = Main_Unit
      then
         Load_Msg_Sloc := Sloc (Enode);
      end if;

      --  If we are generating error messages, then capture calling unit

      if Present (Enode) then
         Calling_Unit := Get_Sloc_Unit_Number (Sloc (Enode));
      end if;

      --  See if we already have an entry for this unit

      Unum := Main_Unit;

      while Unum <= Units.Last loop
         exit when Uname = Units.Table (Unum).Unit_Name;
         Unum := Unum + 1;
      end loop;

      --  Whether or not the entry was found, Unum is now the right value,
      --  since it is one more than Units.Last (i.e. the index of the new
      --  entry we will create) in the not found case.

      --  A special check is necessary in the unit not found case. If the unit
      --  is not found, but the file in which it lives has already been loaded,
      --  then we have the problem that the file does not contain the unit that
      --  is needed. We simply treat this as a file not found condition.

      if Unum > Units.Last then
         for J in Units.First .. Units.Last loop
            if Fname = Units.Table (J).Unit_File_Name then
               if Debug_Flag_L then
                  Write_Str ("  file does not contain unit, Unit_Number = ");
                  Write_Int (Int (Unum));
                  Write_Eol;
                  Write_Eol;
               end if;

               if Present (Enode) then

                  if Is_Predefined_File_Name (Fname) then
                     Error_Msg_Name_1 := Uname;
                     Error_Msg
                       ("% is not a language defined unit", Load_Msg_Sloc);
                  else
                     Error_Msg_Name_1 := Fname;
                     Error_Msg_Unit_1 := Uname;
                     Error_Msg
                       ("File{ does not contain unit$", Load_Msg_Sloc);
                  end if;

                  Write_Dependency_Chain;
                  raise Unrecoverable_Error;

               else
                  return No_Unit;
               end if;
            end if;
         end loop;
      end if;

      --  If we are proceeding with load, then make load stack entry

      Load_Stack.Increment_Last;
      Load_Stack.Table (Load_Stack.Last) := Unum;

      --  Case of entry already in table

      if Unum <= Units.Last then

         --  Here is where we check for a circular dependency, which is
         --  an attempt to load a unit which is currently in the process
         --  of being loaded. We do *not* care about a circular chain that
         --  leads back to a body, because this kind of circular dependence
         --  legitimately occurs (e.g. two package bodies that contain
         --  inlined subprogram referenced by the other).

         if Loading (Unum)
           and then (Is_Spec_Name (Units.Table (Unum).Unit_Name)
                       or else Acts_As_Spec (Units.Table (Unum).Cunit))
         then
            if Debug_Flag_L then
               Write_Str ("  circular dependency encountered");
               Write_Eol;
            end if;

            if Present (Enode) then
               Error_Msg ("circular unit dependency", Load_Msg_Sloc);
               Write_Dependency_Chain;
               raise Unrecoverable_Error;
            else
               Load_Stack.Decrement_Last;
               return No_Unit;
            end if;
         end if;

         if Debug_Flag_L then
            Write_Str ("  unit already in file table, Unit_Number = ");
            Write_Int (Int (Unum));
            Write_Eol;
         end if;

         Load_Stack.Decrement_Last;
         return Unum;

      --  File is not already in table, so try to open it

      else
         if Debug_Flag_L then
            Write_Str ("  attempt unit load, Unit_Number = ");
            Write_Int (Int (Unum));
            Write_Eol;
         end if;

         Src_Ind := Load_Source_File (Fname);

         --  Make a partial entry in the file table, used even in the file not
         --  found case to print the dependency chain including the last entry

         Units.Increment_Last;
         Units.Table (Unum).Unit_Name := Uname;

         --  File was found

         if Src_Ind /= No_Source_File then

            Units.Table (Unum).Cunit          := Empty;
            Units.Table (Unum).Cunit_Entity   := Empty;
            Units.Table (Unum).Def_Func_Count := 0;
            Units.Table (Unum).Error_Location := Sloc (Enode);
            Units.Table (Unum).Expected_Unit  := Uname;
            Units.Table (Unum).Fatal_Error    := False;
            Units.Table (Unum).Generate_Code  := False;
            Units.Table (Unum).Ident_String   := No_String;
            Units.Table (Unum).Main_Priority  := Default_Main_Priority;
            Units.Table (Unum).Serial_Number  := 0;
            Units.Table (Unum).Source_Index   := Src_Ind;
            Units.Table (Unum).Unit_File_Name := Fname;
            Units.Table (Unum).Version        := Source_Checksum (Src_Ind);

            if Corr_Body = No_Unit then
               Units.Table (Unum).Serial_Ref_Unit := Unum;
            else
               Units.Table (Unum).Serial_Ref_Unit := Corr_Body;
            end if;

            --  Parse the new unit

            Set_Loading (Unum, True);
            Initialize_Scanner (Unum, Source_Index (Unum));
            Discard := Par (Configuration_Pragmas => False);
            Set_Loading (Unum, False);

            --  If spec is irrelevant, then post errors and quit

            if Corr_Body /= No_Unit
              and then Spec_Is_Irrelevant (Unum, Corr_Body)
            then
               Error_Msg_Name_1 := Unit_File_Name (Corr_Body);
               Error_Msg
                 ("cannot compile subprogram in file {!",
                  Load_Msg_Sloc);
               Error_Msg_Name_1 := Unit_File_Name (Unum);
               Error_Msg
                 ("incorrect spec in file { must be removed first!",
                  Load_Msg_Sloc);
               return No_Unit;
            end if;

            --  If loaded unit had a fatal error, then caller inherits it!

            if Units.Table (Unum).Fatal_Error
              and then Present (Enode)
            then
               Units.Table (Calling_Unit).Fatal_Error := True;
            end if;

            --  Remove load stack entry and return the entry in the file table

            Load_Stack.Decrement_Last;
            return Unum;

         --  Case of file not found

         else
            if Debug_Flag_L then
               Write_Str ("  file was not found, load failed");
               Write_Eol;
            end if;

            --  Generate message if unit required

            if Required and then Present (Enode) then

               if Is_Predefined_File_Name (Fname) then
                  Error_Msg_Name_1 := Uname;
                  Error_Msg
                    ("% is not a predefined library unit", Load_Msg_Sloc);

               else
                  Error_Msg_Name_1 := Fname;
                  Error_Msg ("file{ not found", Load_Msg_Sloc);
               end if;

               Write_Dependency_Chain;
               raise Unrecoverable_Error;

            --  If unit not required, remove load stack entry and the junk
            --  file table entry, and return No_Unit to indicate not found,

            else
               Load_Stack.Decrement_Last;
               Units.Decrement_Last;
               return No_Unit;
            end if;
         end if;
      end if;
   end Load_Unit;

   ------------------------
   -- Make_Instance_Unit --
   ------------------------

   --  If the unit is an instance, it appears as a package declaration, but
   --  contains both declaration and body of the instance. The body becomes
   --  the main unit of the compilation, and the declaration is inserted
   --  at the end of the unit table. The main unit now has the name of a
   --  body, which is constructed from the name of the original spec,
   --  and is attached to the compilation node of the original unit. The
   --  declaration has been attached to a new compilation unit node, and
   --  code will have to be generated for it.

   procedure Make_Instance_Unit (N : Node_Id) is
      Sind : constant Source_File_Index := Source_Index (Main_Unit);

   begin
      Units.Increment_Last;

      Units.Table (Units.Last)               := Units.Table (Main_Unit);
      Units.Table (Units.Last).Cunit         := Library_Unit (N);
      Units.Table (Units.Last).Generate_Code := True;

      Units.Table (Main_Unit).Cunit          := N;
      Units.Table (Main_Unit).Unit_Name      :=
        Get_Body_Name (Get_Unit_Name (Unit (Library_Unit (N))));
      Units.Table (Main_Unit).Version        := Source_Checksum (Sind);
   end Make_Instance_Unit;

   --------------------
   -- Version_Update --
   --------------------

   procedure Version_Update (U : Node_Id; From : Node_Id) is
      Unum  : constant Unit_Number_Type := Get_Cunit_Unit_Number (U);
      Fnum  : constant Unit_Number_Type := Get_Cunit_Unit_Number (From);

   begin
      Units.Table (Unum).Version :=
        Units.Table (Unum).Version
          xor
        Source_Checksum (Source_Index (Fnum));
   end Version_Update;

   ----------------------------
   -- Write_Dependency_Chain --
   ----------------------------

   procedure Write_Dependency_Chain is
   begin
      --  The dependency chain is only written if it is at least two entries
      --  deep, otherwise it is trivial (the main unit depending on a unit
      --  that it obviously directly depends on).

      if Load_Stack.Last - 1 > Load_Stack.First then
         for U in Load_Stack.First .. Load_Stack.Last - 1 loop
            Error_Msg_Unit_1 := Unit_Name (Load_Stack.Table (U));
            Error_Msg_Unit_2 := Unit_Name (Load_Stack.Table (U + 1));
            Error_Msg ("$ depends on $!", Load_Msg_Sloc);
         end loop;
      end if;
   end Write_Dependency_Chain;

end Lib.Load;
