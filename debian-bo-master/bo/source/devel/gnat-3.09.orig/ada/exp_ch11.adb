------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ C H 1 1                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.11 $                             --
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
with Einfo;    use Einfo;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Util; use Exp_Util;
with Sinfo;    use Sinfo;
with Sem;      use Sem;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Tbuild;   use Tbuild;
with Rtsfind;  use Rtsfind;
with Uintp;    use Uintp;

package body Exp_Ch11 is

   ---------------------------------------------
   -- Expand_N_Handled_Sequence_Of_Statements --
   ---------------------------------------------

   procedure Expand_N_Handled_Sequence_Of_Statements (N : Node_Id) is
      Loc      : Source_Ptr;
      Undefer  : Node_Id;
      Handler  : Node_Id;
      Handlers : constant List_Id := Exception_Handlers (N);

   begin
      --  Add a call to Abort_Undefer to the front of all exception handlers.
      --  Abortion is deferred as part of raising an exception, to prevent
      --  cleanup activities from being aborted.

      if Present (Handlers) then
         Handler := First_Non_Pragma (Handlers);
         while Present (Handler) loop
            Loc := Sloc (Handler);
            Undefer :=
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (RTE (RE_Abort_Undefer), Loc),
                Parameter_Associations => Empty_List);

            Prepend_To (Statements (Handler), Undefer);
            Analyze (Undefer);
            Handler := Next_Non_Pragma (Handler);
         end loop;
      end if;

      --  The following code needs commenting ???

      if Nkind (Parent (N)) /= N_Package_Body
        and then Nkind (Parent (N)) /= N_Accept_Statement
        and then not Has_Pending_Instantiations (Current_Scope)
      then
         Expand_Cleanup_Actions (Parent (N));
      else
         Set_First_Real_Statement (N, First (Statements (N)));
      end if;
   end Expand_N_Handled_Sequence_Of_Statements;

   ------------------------------------
   -- Expand_N_Exception_Declaration --
   ------------------------------------

   --  Generates:
   --     exceptE : constant String := "A.B.EXCEP";   -- static data
   --     except : exception_data :=  (
   --                    Handled_By_Other => False,
   --                    C1               => 'A',
   --                    C2               => 'd',
   --                    C3               => 'a',
   --                    Name_Length      => exceptE'Length
   --                    Full_Name        => exceptE'Address
   --                    Htable_Ptr       => null);

   --  (protecting test only needed if not at library level)
   --
   --     exceptF : Boolean := True --  static data
   --     if exceptF then
   --        exceptF := False;
   --        Register_Exception (except'Unchecked_Access);
   --     end if;

   procedure Expand_N_Exception_Declaration (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Id      : constant Entity_Id  := Defining_Identifier (N);
      L       : List_Id             := New_List;
      Flag_Id : Entity_Id;

      Name_Exname : constant Name_Id := New_External_Name (Chars (Id), 'E');
      Exname      : constant Node_Id :=
                      Make_Defining_Identifier (Loc, Name_Exname);

   begin
      --  Definition of the external name: nam : constant String := "A.B.NAME";

      Insert_Action (N,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Exname,
          Constant_Present    => True,
          Object_Definition   => New_Reference_To (Standard_String, Loc),
          Expression => Make_String_Literal (Loc, Full_Qualified_Name (Id))));

      Set_Is_Statically_Allocated (Exname);

      --  Create the aggregate list for type Standard.Exception_Type:
      --  Handled_By_Other component: False

      Append_To (L, New_Reference_To (Standard_False, Loc));

      --  C1 component: 'A'

      Append_To (L,
        Make_Character_Literal (Loc, Name_uA, Get_Char_Code ('A')));

      --  C2 component: 'd'

      Append_To (L,
        Make_Character_Literal (Loc, Name_D,  Get_Char_Code ('d')));

      --  C3 component: 'a'

      Append_To (L,
        Make_Character_Literal (Loc, Name_A,  Get_Char_Code ('a')));

      --  Name_Length component: Nam'Length

      Append_To (L,
        Make_Attribute_Reference (Loc,
          Prefix         => New_Reference_To (Exname, Loc),
          Attribute_Name => Name_Length));

      --  Full_Name component: Standard.A_Char!(Nam'Address)

      Append_To (L, Unchecked_Convert_To (Standard_A_Char,
        Make_Attribute_Reference (Loc,
          Prefix         => New_Reference_To (Exname, Loc),
          Attribute_Name => Name_Address)));

      --  Htable_Ptr component: null

      Append_To (L, Make_Null (Loc));

      Set_Expression (N, Make_Aggregate (Loc, Expressions => L));
      Analyze_And_Resolve (Expression (N), Etype (Id));

      --  Register_Exception (except'Unchecked_Access);

      L := New_List (
             Make_Procedure_Call_Statement (Loc,
               Name => New_Reference_To (RTE (RE_Register_Exception), Loc),
               Parameter_Associations => New_List (
                 Unchecked_Convert_To (RTE (RE_Exception_Data_Ptr),
                   Make_Attribute_Reference (Loc,
                     Prefix         => New_Reference_To (Id, Loc),
                     Attribute_Name => Name_Unrestricted_Access)))));

      if not Is_Library_Level_Entity (Id) then
         Flag_Id :=  Make_Defining_Identifier (Loc,
                       New_External_Name (Chars (Id), 'F'));

         Insert_Action (N,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Flag_Id,
             Object_Definition   => New_Reference_To (Standard_Boolean, Loc),
             Expression          => New_Reference_To (Standard_True, Loc)));

         Set_Is_Statically_Allocated (Flag_Id);

         Append_To (L,
           Make_Assignment_Statement (Loc,
             Name       => New_Reference_To (Flag_Id, Loc),
             Expression => New_Reference_To (Standard_False, Loc)));

         Insert_After_And_Analyze (N,
           Make_If_Statement (Loc,
             Condition       => New_Reference_To (Flag_Id, Loc),
             Then_Statements => L));

      else
         Insert_List_After_And_Analyze (N, L);
      end if;

   end Expand_N_Exception_Declaration;

end Exp_Ch11;
