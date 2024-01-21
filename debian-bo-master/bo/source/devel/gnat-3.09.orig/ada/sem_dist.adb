------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.134 $                            --
--                                                                          --
--          Copyright (C) 1992-1997, Free Software Foundation, Inc.         --
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
with Casing;   use Casing;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Elists;   use Elists;
with Exp_Dist; use Exp_Dist;
with Exp_TSS;  use Exp_TSS;
with Fname;    use Fname;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Namet;    use Namet;
with Osint;    use Osint;
with Opt;      use Opt;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Sprint;   use Sprint;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Uname;    use Uname;

package body Sem_Dist is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Unchecked_Conversion (C_Unit : Node_Id);
   --  Load, analyze and add the package Unchecked_Conversion
   --  to the context clauses of the enclosing library unit

   procedure Check_Categorization_Dependencies
     (Unit_Entity     : Entity_Id;
      Depended_Entity : Entity_Id;
      Info_Node       : Node_Id);
   --  This procedure checks that the categorization of a lib unit and that
   --  of the depended unit satisfy dependency restrictions.
   --  What is the info-Node param, need more documentation ???

   procedure Check_Non_Static_Default_Expr (Type_Def : Node_Id);
   --  Iterate through the component list of a record definition, check
   --  that no component is declared with a non-static default value.

   procedure RAS_E_Dereference (Pref : Node_Id);
   --  Handles explicit dereference of Remote Access to Subprograms.

   function Build_Parent_Full_Name (P : Node_Id)  return Node_Id;
   --  Build prefix of child unit name. Recurse if needed.

   function Build_Unit_Full_Name (U : Node_Id) return Node_Id;
   --  If the unit is a child unit, build name with all ancestors. otherwise,
   --  returns a new reference to the unit name.

   function Full_Qualified_Name (E : Entity_Id) return String_Id;
   --  returns the full qualified name of the entity in lower case.

   function Get_Name_Id (Name : String) return Name_Id;
   --  Given a string, return the Name_Id that represent the string

   function Has_Pragma_All_Calls_Remote (L : List_Id) return Boolean;
   --  Return true if L contains a pragma All_Calls_Remote node.

   function Static_Discriminant_Expr (L : List_Id) return Boolean;
   --  Iterate through the list of discriminants to check if any of them
   --  contains non-static default expression, which is a violation in
   --  a preelaborated library unit.

   procedure Rewrite_Or_Replace (N : Node_Id; New_Node : Node_Id);
   --  In a number of situations, we rewrite a node and if we are in stub
   --  generation mode, we want the stub output to include this rewriting.
   --  Since stub generation uses the original tree, this means we need
   --  to do a Replace_Substitute_Tree if we are generating stubs, but if
   --  we are not generating stubs, then semantic accuracy of the original
   --  tree is properly preserved by using Rewrite_Substitute_Tree.

   ------------------------------
   -- Add_Unchecked_Conversion --
   ------------------------------

   procedure Add_Unchecked_Conversion (C_Unit : Node_Id) is
      Contexts : List_Id := Context_Items (C_Unit);
      Lib_Unit : Node_Id;
      Withn    : Node_Id;
      Uname    : Unit_Name_Type;
      Unum     : Unit_Number_Type;
      UEntity  : Entity_Id;
      Withed   : Boolean := False;
      Context  : Node_Id;

      procedure Failure (S : String);
      --  Internal procedure called if an error occurs. The parameter
      --  is a detailed error message that is to be given

      procedure Failure (S : String) is
      begin
         Set_Standard_Error;

         Write_Str ("fatal error: runtime library configuration error");
         Write_Eol;
         Write_Char ('"');
         Write_Name (Get_File_Name (Uname));
         Write_Str (""" (");
         Write_Str (S);
         Write_Char (')');
         Write_Eol;
         Set_Standard_Output;
         raise Unrecoverable_Error;
      end Failure;

   --  Start of processing for Add_Unchecked_Conversion

   begin
      Name_Buffer (1 .. 22) := "unchecked_conversion%s";
      Name_Len := 22;
      Uname := Name_Find;
      Unum := Load_Unit (Uname, False, Empty);

      if Unum = No_Unit then
         Failure ("unit not found");
      elsif Fatal_Error (Unum) then
         Failure ("parser errors");
      end if;

      --  Make sure that the unit is analyzed

      if not Analyzed (Cunit (Unum)) then
         Semantics (Cunit (Unum));

         if Fatal_Error (Unum) then
            Failure ("semantic errors");
         end if;
      end if;

      Lib_Unit := Unit (Cunit (Unum));

      UEntity := Defining_Entity (Lib_Unit);

      --  Add to the context clause

      if Present (Contexts) then
         Context := First (Contexts);
         while Present (Context) and not Withed loop
            Withed :=
              Nkind (Context) = N_With_Clause
                and then
              Defining_Entity (Unit (Library_Unit (Context))) = UEntity;
            Context := Next (Context);
         end loop;
      end if;

      if not Withed then
         Withn :=
           Make_With_Clause (Standard_Location,
             Name => New_Reference_To (UEntity, Standard_Location));
         Set_Library_Unit       (Withn, Cunit (Unum));
         Set_Corresponding_Spec (Withn, UEntity);
         Set_First_Name         (Withn, True);
         Set_Implicit_With      (Withn, True);
         Mark_Rewrite_Insertion (Withn);
         Prepend (Withn, Contexts);
      end if;
   end Add_Unchecked_Conversion;

   ----------------------
   -- Append_System_PI --
   ----------------------

   procedure Append_System_PI (N : Node_Id; L : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      procedure Append_Items (Items : List_Id);
      --  Given Items, a list of visible declarations or following pragmas
      --  of L, append System.Partition_Interface to the context items
      --  of N if any of there is Remote_Call_Interface or Remote_Types
      --  pragma in the list.

      ------------------
      -- Append_Items --
      ------------------

      procedure Append_Items (Items : List_Id) is
         Nd : Node_Id;

      begin
         if Present (Items) then
            Nd := First (Items);
            while Present (Nd) loop

               --  Search ends when non-pragma is met since they appear first

               exit when Nkind (Nd) /= N_Pragma;

               if Chars (Nd) = Name_Remote_Call_Interface
                 or else Chars (Nd) = Name_Remote_Types
               then
                  Append_To (Context_Items (N),
                    Make_With_Clause (Loc,
                      Make_Selected_Component (Loc,
                        Prefix        =>
                          Make_Identifier (Loc, Name_System),
                        Selector_Name =>
                          Make_Identifier (Loc, Name_Partition_Interface))));
               end if;

               Nd := Next (Nd);
            end loop;
         end if;
      end Append_Items;

   --  Start processing of Append_System_PI

   begin
      if Nkind (Unit (L)) = N_Package_Declaration then
         Append_Items (Visible_Declarations (Specification (Unit (L))));
         Append_Items (Pragmas_After (L));
      end if;
   end Append_System_PI;

   -----------------------
   -- Append_System_RPC --
   -----------------------

   procedure Append_System_RPC (N : Node_Id) is
      Decls  : constant List_Id := Visible_Declarations
                                     (Specification (Unit (N)));

      S      : constant Source_Ptr := Sloc (N);
      Items  : List_Id := Context_Items (N);
      F      : List_Id := Pragmas_After (N);
      Decl   : Node_Id;
      Item   : Node_Id;

      procedure Append_Items (N : in out Node_Id);
      --  Given N, first node in a list (visible declarations or following
      --  pragmas) append to the list context items "with System.Rpc" if
      --  unit is either RCI or remote types.

      ------------------
      -- Append_Items --
      ------------------

      procedure Append_Items (N : in out Node_Id) is
      begin
         while Present (N) and then Nkind (N) = N_Pragma loop

            if Chars (N) = Name_Remote_Call_Interface
              or else Chars (N) = Name_Remote_Types
            then
               Item := Make_With_Clause (S,
                 Make_Selected_Component (S,
                   Prefix        => Make_Identifier (S, Name_System),
                   Selector_Name => Make_Identifier (S, Name_Rpc)));

               if Present (Items) then
                  Append (Item, Items);
               else
                  Items := New_List (Item);
               end if;
            end if;

            N := Next (N);
         end loop;
      end Append_Items;

   --  Start processing of Append_System_Rpc

   begin
      if not Present (Decls) then
         return;
      end if;

      Decl := First (Decls);
      Append_Items (Decl);

      if not Present (F) then
         return;
      end if;

      Decl := First (F);
      Append_Items (Decl);
   end Append_System_RPC;

   ----------------------------
   -- Build_Parent_Full_Name --
   ----------------------------

   function Build_Parent_Full_Name (P : Node_Id) return Node_Id is
      Loc   : Source_Ptr := Sloc (P);
      P_Ref : Node_Id := New_Reference_To (Defining_Entity (P), Loc);

   begin
      if No (Parent_Spec (P)) then
         return P_Ref;
      else
         return
           Make_Selected_Component (Loc,
             Prefix => Build_Parent_Full_Name (Unit (Parent_Spec (P))),
             Selector_Name => P_Ref);
      end if;
   end Build_Parent_Full_Name;


   --------------------------
   -- Build_Unit_Full_Name --
   --------------------------

   function Build_Unit_Full_Name (U : Node_Id) return Node_Id is
      Loc    : Source_Ptr := Sloc (U);
      U_Name : Entity_Id  := Defining_Entity (U);
      Result : Node_Id;

   begin
      if No (Parent_Spec (U)) then
         return New_Reference_To (U_Name, Loc);

      else
         Result :=
           Make_Expanded_Name (Loc,
             Chars => Chars (U_Name),
             Prefix => Build_Parent_Full_Name (Unit (Parent_Spec (U))),
             Selector_Name => New_Reference_To (U_Name, Loc));

         Set_Entity (Result, U_Name);
         Set_Etype (Result, Etype (U_Name));
         return Result;
      end if;
   end Build_Unit_Full_Name;

   ---------------------------------------
   -- Check_Categorization_Dependencies --
   ---------------------------------------

   procedure Check_Categorization_Dependencies
     (Unit_Entity     : Entity_Id;
      Depended_Entity : Entity_Id;
      Info_Node       : Node_Id)
   is
      N                  : Node_Id := Info_Node;
      Depended_Unit_Node : Node_Id;

      type Categorization is
         (Pure, Shared_Passive, Remote_Types,
           Remote_Call_Interface, Pre_Elaborated, Normal);

      Unit_Category : Categorization;
      With_Category : Categorization;

      function Get_Categorization (E : Entity_Id) return Categorization;
      --  To simplify checking code below.

      function Get_Categorization (E : Entity_Id) return Categorization is
      begin
         if Is_Preelaborated (E) then
            return Pre_Elaborated;
         elsif Is_Pure (E) then
            return Pure;
         elsif Is_Shared_Passive (E) then
            return Shared_Passive;
         elsif Is_Remote_Types (E) then
            return Remote_Types;
         elsif Is_Remote_Call_Interface (E) then
            return Remote_Call_Interface;
         else
            return Normal;
         end if;
      end Get_Categorization;

   begin
      if Nkind (Info_Node) = N_With_Clause then

         --  Compilation unit node of withed unit.

         Depended_Unit_Node := Library_Unit (Info_Node);

      else
         --  Parent spec compilation unit node.

         Depended_Unit_Node := Info_Node;
      end if;

      --  KLUDGE ALERT !!!

      --  Right now, there is junk code in sem_prag that deliberately
      --  does not set the Is_Preelaborated flag for generic units,
      --  but that's wrong, since it means that we have trouble with
      --  this check with we "with" a preelaborated unit. Probably there
      --  is similar junk for the other cases.

      --  The reason for the junk code was to stop problems at the
      --  instantiation level, but this should be cured in other ways
      --  (e.g. not copying these flags to the instantiation).

      --  For now, we just disconnect the dependency checks for generic units

      if Ekind (Depended_Entity) = E_Generic_Procedure
           or else
         Ekind (Depended_Entity) = E_Generic_Function
           or else
         Ekind (Depended_Entity) = E_Generic_Package
      then
         return;
      end if;

      Unit_Category := Get_Categorization (Unit_Entity);
      With_Category := Get_Categorization (Depended_Entity);

      if With_Category > Unit_Category then

         --  System.Partition_Interface and System.Rpc are with'ed in
         --  processing remote access to subprogram type by RCI and remote
         --  types units to generate fat pointer type. Since they are not
         --  categorized (not an error, by the way), we will get a dependency
         --  violation if we don't skip checking at this point.

         if Unit_Category = Remote_Types
           and then Chars (Depended_Entity) = Name_Rpc
           and then Present (Scope (Depended_Entity))
           and then Chars (Scope (Depended_Entity)) = Name_System
         then
            null;

         elsif Unit_Category = Remote_Call_Interface
           and then (Chars (Depended_Entity) = Name_Rpc
                      and then Present (Scope (Depended_Entity))
           and then Chars (Scope (Depended_Entity)) = Name_System)
         then
            null;

         elsif Unit_Category = Remote_Call_Interface
           and then Chars (Depended_Entity) = Name_Partition_Interface
         then
            null;

         else
            Error_Msg_NE ("current unit cannot depend on&"
              & " (wrong categorization)", N, Depended_Entity);
         end if;
      end if;

   end Check_Categorization_Dependencies;

   -----------------------------------
   -- Check_Non_Static_Default_Expr --
   -----------------------------------

   procedure Check_Non_Static_Default_Expr (Type_Def : Node_Id) is
      Component_Decl : Node_Id;

   begin
      --  Check that component declarations do not involve:

      --    a. a non-static default expression, where the object is
      --       declared to be default initialized.

      --    b. a dynamic Itype (discriminants and constraints)

      if Null_Present (Type_Def) then
         return;
      else
         Component_Decl := First (Component_Items (Component_List (Type_Def)));
      end if;

      while Present (Component_Decl)
        and then Nkind (Component_Decl) = N_Component_Declaration
      loop
         if Present (Expression (Component_Decl))
           and then Nkind (Expression (Component_Decl)) /= N_Null
           and then not Is_Static_Expression (Expression (Component_Decl))
         then
            Error_Msg_N
              ("non-static expression in declaration in preelaborated unit",
               Component_Decl);

         --  Fix this later!

         --  elsif Has_Dynamic_Itype (Component_Decl) then
         --     Error_Msg_N
         --       ("dynamic type discriminant," &
         --        " constraint in preelaborated unit",
         --        Component_Decl);
         end if;

         Component_Decl := Next (Component_Decl);
      end loop;
   end Check_Non_Static_Default_Expr;

   --------------------------------------
   -- CW_Remote_Extension_Add_Receiver --
   --------------------------------------

   procedure CW_Remote_Extension_Add_Receiver (N : Node_Id) is
      PN : constant Node_Id := Parent (N);
      LU : Node_Id;
      PD : Node_Id;
      SP : Node_Id;
      BL : List_Id;
      LN : Node_Id;

      procedure Add_Receiver (L : List_Id);
      --  In case there is a classwide type remote extension (check spec
      --  for definition) on the list, append a receiver for such type
      --  (extension)

      procedure Add_Receiver (L : List_Id) is
         Decl : Node_Id;

      begin
         if not Present (L) then
            return;
         end if;

         Decl := First (L);

         while Present (Decl) loop

            if Is_Class_Wide_Type_Remote_Extension (Decl) then

               if not Is_Remote_Call_Interface (Defining_Identifier
                 (Decl))
               then

                  --  Add to BL (package body declaration list) the
                  --  receiver subprogram for the type (extension)

                  null; --  ??? To be updated soon
               end if;

            end if;

            Decl := Next (Decl);
         end loop;
      end Add_Receiver;

   --  Start of processing CW_Remote_Extension_Add_Receiver

   begin
      if Nkind (PN) /= N_Compilation_Unit then
         return;
      end if;

      LU := Library_Unit (PN);

      if not Present (LU) then
         return;
      end if;

      PD := Unit (LU);

      if Nkind (PD) /= N_Package_Declaration then
         return;
      end if;

      SP := Specification (PD);
      BL := Declarations (N);

      LN := Last (BL);
      Add_Receiver (Visible_Declarations (SP));
      Add_Receiver (Private_Declarations (SP));
      Add_Receiver (BL);

   end CW_Remote_Extension_Add_Receiver;

   -------------------------------
   -- Enclosing_Lib_Unit_Entity --
   -------------------------------

   function Enclosing_Lib_Unit_Entity return Entity_Id is
      Unit_Entity : Entity_Id := Current_Scope;

   begin
      --  Look for enclosing library unit entity by following scope links.
      --  Equivalent to, but faster than indexing through the scope stack.

      while (Present (Scope (Unit_Entity))
        and then Scope (Unit_Entity) /= Standard_Standard)
        and not Is_Child_Unit (Unit_Entity)
      loop
         Unit_Entity := Scope (Unit_Entity);
      end loop;

      return Unit_Entity;
   end Enclosing_Lib_Unit_Entity;

   -----------------------------
   -- Enclosing_Lib_Unit_Node --
   -----------------------------

   function Enclosing_Lib_Unit_Node (N : Node_Id) return Node_Id is
      Current_Node : Node_Id := N;

   begin
      while Present (Current_Node)
        and then Nkind (Current_Node) /= N_Compilation_Unit
      loop
         Current_Node := Parent (Current_Node);
      end loop;

      if Nkind (Current_Node) /= N_Compilation_Unit then
         return Empty;
      end if;

      return Current_Node;
   end Enclosing_Lib_Unit_Node;

   -------------------------
   -- Full_Qualified_Name --
   -------------------------

   function Full_Qualified_Name (E : Entity_Id) return String_Id is
      Ent         : Entity_Id := E;
      Parent_Name : String_Id := No_String;

   begin
      --  Deals properly with child units

      if Nkind (Ent) = N_Defining_Program_Unit_Name then
         Ent := Defining_Identifier (Ent);
      end if;

      --  Compute recursively the qualification. Only "Standard" has no scope.

      if Present (Scope (Scope (Ent))) then
         Parent_Name := Full_Qualified_Name (Scope (Ent));
      end if;

      --  Every entity should have a name except some expanded blocks
      --  don't bother about those.

      if Chars (Ent) = No_Name then
         return Parent_Name;
      end if;

      --  Add a period between Name and qualification

      if Parent_Name /= No_String then
         Start_String (Parent_Name);
         Store_String_Char (Get_Char_Code ('.'));

      else
         Start_String;
      end if;

      --  Generates the entity name in upper case

      Get_Name_String (Chars (Ent));
      Set_Casing (All_Lower_Case);
      Store_String_Chars (Name_Buffer (1 .. Name_Len));
      return End_String;
   end Full_Qualified_Name;

   --------------------------
   -- Generate_Stubs_Files --
   --------------------------

   procedure Generate_Stubs_Files (N : Node_Id) is
      Unit_Node : Node_Id := Unit (N);
      Nd        : Node_Id;

      procedure Output_Stubs_File (Stubs_Node : Node_Id);
      --  Create the source file for a stubs node

      procedure Output_Stubs_File (Stubs_Node : Node_Id) is
      begin
         Stub_Output_Start;
         Sprint_Node_Pure_Ada (Stubs_Node);
         Write_Eol;
         Stub_Output_Stop;
      end Output_Stubs_File;

   --  Start of processing for Generate_Stubs_Files

   begin
      if Distribution_Stub_Mode = Generate_Caller_Stub_Body then
         if Nkind (Unit_Node) = N_Package_Declaration then
            if Is_Shared_Passive
              (Defining_Unit_Name (Specification (Unit_Node)))
            then
               Nd := Build_Passive_Partition_Stub (N, Is_Client => True);
            else
               Init_Names;
               Nd := Build_Calling_Stubs_Bodies_Cunit (N);
            end if;

            Output_Stubs_File (Nd);

         else
            Error_Msg_N ("Specification file expected from command line",
              Unit_Node);
         end if;

      elsif Distribution_Stub_Mode = Generate_Receiver_Stub_Body then
         if Nkind (Unit_Node) = N_Package_Declaration
           and then Is_Shared_Passive
             (Defining_Unit_Name (Specification (Unit_Node)))
         then
            Nd := Build_Passive_Partition_Stub (N, Is_Client => False);
         else
            Init_Names;
            Nd := Build_Receiving_Stubs_Bodies_Cunit (N);
         end if;

         Output_Stubs_File (Nd);
      end if;

   end Generate_Stubs_Files;

   -----------------
   -- Get_Name_Id --
   -----------------

   function Get_Name_Id (Name : String) return Name_Id is
   begin
      Name_Len := Name'Length;
      Name_Buffer (1 ..  Name_Len) := Name;
      return Name_Find;
   end Get_Name_Id;

   ---------------------------------
   -- Has_Pragma_All_Calls_Remote --
   ---------------------------------

   function Has_Pragma_All_Calls_Remote (L : List_Id) return Boolean is
      Decl : Node_Id;

   begin
      if Present (L) then
         Decl := First (L);
         while Present (Decl)
           and then (Nkind (Decl) /= N_Pragma
                      or else Chars (Decl) /= Name_All_Calls_Remote)
         loop
            Decl := Next (Decl);
         end loop;

         if Present (Decl) then
            return True;
         end if;
      end if;

      return False;
   end Has_Pragma_All_Calls_Remote;

   ---------------------------
   -- In_Preelaborated_Unit --
   ---------------------------

   function In_Preelaborated_Unit return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  There are no constraints on body of remote_call_interface or
      --  remote_types packages..

      return (Unit_Entity /= Standard_Standard)
        and then (Is_Preelaborated (Unit_Entity)
                    or else Is_Pure (Unit_Entity)
                    or else Is_Shared_Passive (Unit_Entity)
                    or else ((Is_Remote_Types (Unit_Entity)
                              or else Is_Remote_Call_Interface (Unit_Entity))
                       and then Ekind (Unit_Entity) = E_Package
                       and then Unit_Kind /= N_Package_Body
                       and then not In_Package_Body (Unit_Entity)
                       and then not In_Instance));
   end In_Preelaborated_Unit;

   ------------------
   -- In_Pure_Unit --
   ------------------

   function In_Pure_Unit return Boolean is
   begin
      return Is_Pure (Current_Scope);
   end In_Pure_Unit;

   -----------------------------------
   -- In_Remote_Call_Interface_Unit --
   -----------------------------------

   function In_Remote_Call_Interface_Unit return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  There are no restrictions on the body of an RCI unit.

      return Is_Remote_Call_Interface (Unit_Entity)
        and then Ekind (Unit_Entity) = E_Package
        and then Unit_Kind /= N_Package_Body
        and then not In_Package_Body (Unit_Entity)
        and then not In_Instance;
   end In_Remote_Call_Interface_Unit;

   --------------------------
   -- In_Remote_Types_Unit --
   --------------------------

   function In_Remote_Types_Unit return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;
      Unit_Kind   : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));

   begin
      --  There are no restrictions on the body of a Remote Types unit.

      return Is_Remote_Types (Unit_Entity)
        and then Ekind (Unit_Entity) = E_Package
        and then Unit_Kind /= N_Package_Body
        and then not In_Package_Body (Unit_Entity)
        and then not In_Instance;
   end In_Remote_Types_Unit;

   ----------------------------
   -- In_Shared_Passive_Unit --
   ----------------------------

   function In_Shared_Passive_Unit return Boolean is
      Unit_Entity : constant Entity_Id := Current_Scope;

   begin
      return Is_Shared_Passive (Unit_Entity);
   end In_Shared_Passive_Unit;

   ---------------------------------------
   -- In_Subprogram_Task_Protected_Unit --
   ---------------------------------------

   function In_Subprogram_Task_Protected_Unit return Boolean is
      E : Entity_Id;
      K : Entity_Kind;

   begin
      --  The following is to verify that a declaration is inside
      --  subprogram, generic subprogram, task unit, protected unit.
      --  Used to validate if a lib. unit is Pure. RM 10.2.1(16).

      --  Use scope chain to check successively outer scopes

      E := Current_Scope;
      loop
         K := Ekind (E);

         if        K = E_Procedure
           or else K = E_Function
           or else K = E_Generic_Procedure
           or else K = E_Generic_Function
           or else K = E_Task_Type
           or else K = E_Task_Subtype
           or else K = E_Protected_Type
           or else K = E_Protected_Subtype
         then
            return True;

         elsif E = Standard_Standard then
            return False;
         end if;

         E := Scope (E);
      end loop;

   end In_Subprogram_Task_Protected_Unit;

   ------------------------
   -- In_Subprogram_Unit --
   ------------------------

   function In_Subprogram_Unit return Boolean is
      E : Entity_Id;
      K : Entity_Kind;

   begin
      --  Use scope chain to check successively outer scopes

      E := Current_Scope;
      loop
         K := Ekind (E);

         if        K = E_Procedure
           or else K = E_Function
           or else K = E_Generic_Procedure
           or else K = E_Generic_Function
         then
            return True;

         elsif E = Standard_Standard then
            return False;
         end if;

         E := Scope (E);
      end loop;

   end In_Subprogram_Unit;

   -----------------------------------------
   -- Is_Class_Wide_Type_Remote_Extension --
   -----------------------------------------

   function Is_Class_Wide_Type_Remote_Extension (N : Node_Id) return Boolean is
      Derived  : Entity_Id;
      Root_Ty  : Entity_Id;
      Contexts : List_Id;
      Item     : Node_Id;
      Item_Ety : Entity_Id;
      RACW     : Entity_Id;
      DD       : Node_Id;

      function Compare_Root_W_RACW (E : Entity_Id) return Boolean;
      --  Return True if the list containing input entity E has a
      --  remote access to classwide type and whose designated type is
      --  the root abstract type of the Derived type

      function Compare_Root_W_RACW (E : Entity_Id) return Boolean is
         Remote_Access : Entity_Id := E;

      begin
         while Present (Remote_Access) loop
            if Is_Remote_Access_To_Class_Wide_Type (Remote_Access) then
               DD := Designated_Type (Remote_Access);

               --  Test if the designated type of this Remote-Access-To-
               --  Classwide-type is the Root abstract type of the
               --  derived type.

               if Etype (DD) = Root_Ty then
                  return True;
               end if;
            end if;

            Remote_Access := Next_Entity (Remote_Access);
         end loop;

         return False;
      end Compare_Root_W_RACW;

   --  Start of processing for Is_Class_Wide_Type_Remote_Extension

   begin
      if Nkind (N) /= N_Full_Type_Declaration then
         return False;
      end if;

      if Nkind (Type_Definition (N)) /= N_Derived_Type_Definition then
         return False;
      end if;

      Derived := Defining_Identifier (N);

      if not Is_Limited_Record (Derived) then
         return False;
      end if;

      if not Is_Tagged_Type (Derived) then
         return False;
      end if;

      Root_Ty := Etype (Derived);
      Contexts := Context_Items (Cunit (Current_Sem_Unit));

      if not Present (Contexts) then
         return False;
      end if;

      Item := First (Contexts);

      while Present (Item) loop

         if Nkind (Item) = N_With_Clause then
            Item_Ety := Entity (Name (Item));

            if Is_Remote_Call_Interface (Item_Ety) then
               RACW := First_Entity (Item_Ety);

               if Compare_Root_W_RACW (RACW) then
                  return True;
               end if;
            end if;
         end if;

         Item := Next (Item);
      end loop;

      --  For compiler generated classwide extensions "object_stub" in
      --  an RCI unit (spec and body)

      if Is_Remote_Call_Interface (Derived) then
         RACW := First_Entity (Scope (Derived));

         if Compare_Root_W_RACW (RACW) then
            return True;
         end if;
      end if;

      return False;
   end Is_Class_Wide_Type_Remote_Extension;

   -----------------------------------------
   -- Is_Remote_Access_To_Class_Wide_Type --
   -----------------------------------------

   function Is_Remote_Access_To_Class_Wide_Type
     (E    : Entity_Id)
      return Boolean
   is
      DD : Node_Id;
      ED : Node_Id;

   begin
      --  This type entity would have been set Is_Remote_Call_Interface
      --  during the type declaration in case it is inside an RCI unit.
      --  This type entity would have been set Is_Remote_Types during
      --  the type declaration in case it is inside a Remote_Types unit.

      if not Is_Remote_Call_Interface (E)
        and then not Is_Remote_Types (E)
      then
         return False;
      end if;

      if Ekind (E) = E_General_Access_Type then
         DD := Designated_Type (E);
         ED := Parent (Etype (DD));

         if Nkind (ED) = N_Private_Type_Declaration
           and then Limited_Present (ED)
           and then Ekind (DD) = E_Class_Wide_Type
         then
            return True;
         end if;
      end if;

      return False;
   end Is_Remote_Access_To_Class_Wide_Type;

   -----------------------------------------
   -- Is_Remote_Access_To_Subprogram_Type --
   -----------------------------------------

   function Is_Remote_Access_To_Subprogram_Type
     (E    : Entity_Id)
      return Boolean
   is
   begin
      --  This type entity would have been set Is_Remote_Call_Interface
      --  during the type declaration in case it is inside an RCI unit.
      --  This type entity would have been set Is_Remote_Types during
      --  the type declaration in case it is inside a Remote_Types unit.

      return Ekind (E) = E_Access_Subprogram_Type
        and then (Is_Remote_Call_Interface (E)
                   or else Is_Remote_Types (E));
   end Is_Remote_Access_To_Subprogram_Type;

   --------------------------
   -- Process_Partition_ID --
   --------------------------

   procedure Process_Partition_ID (N : Node_Id) is
      Loc            : constant Source_Ptr := Sloc (N);
      Ety            : Entity_Id;
      Nd             : Node_Id;
      Get_Pt_Id      : Node_Id;
      Get_Pt_Id_Call : Node_Id;
      Prefix_String  : String_Id;
      Typ            : constant Entity_Id := Etype (N);

   begin
      Ety := Entity (Prefix (N));

      --  In case prefix is not a library unit entity, get the entity
      --  of library unit.

      while (Present (Scope (Ety))
        and then Scope (Ety) /= Standard_Standard)
        and not Is_Child_Unit (Ety)
      loop
         Ety := Scope (Ety);
      end loop;

      Nd := Enclosing_Lib_Unit_Node (N);

      --  Retrieve the proper function to call.

      if Is_Remote_Call_Interface (Ety) then
         Get_Pt_Id := New_Occurrence_Of
           (RTE (RE_Get_Active_Partition_Id), Loc);

      elsif Is_Shared_Passive (Ety) then
         Get_Pt_Id := New_Occurrence_Of
           (RTE (RE_Get_Passive_Partition_Id), Loc);

      else
         Get_Pt_Id := New_Occurrence_Of
           (RTE (RE_Get_Local_Partition_Id), Loc);
      end if;

      --  Get and store the String_Id corresponding to the name of the
      --  library unit whose Partition_Id is needed

      Get_Unit_Name_String (Get_Unit_Name (Get_Declaration_Node (Ety)));
      Name_Len := Name_Len - 7;
      --  Remove seven last character ("(spec)" or " (body)").

      Start_String;
      Store_String_Chars (Name_Buffer (1 .. Name_Len));
      Prefix_String := End_String;

      --  Build the function call which will replace the attribute

      if Is_Remote_Call_Interface (Ety) or Is_Shared_Passive (Ety) then

         Get_Pt_Id_Call :=
           Make_Function_Call (Loc,
             Name => Get_Pt_Id,
             Parameter_Associations =>
               New_List (Make_String_Literal (Loc, Prefix_String)));

      else
         Get_Pt_Id_Call := Make_Function_Call (Loc, Get_Pt_Id);

      end if;

      --  Replace the attribute node by a conversion of the function call
      --  to the target type.

      Rewrite_Substitute_Tree (N, Convert_To (Typ, Get_Pt_Id_Call));
      Analyze_And_Resolve (N, Typ);

   end Process_Partition_ID;

   -------------------------------------------
   -- Process_Remote_Access_Subprogram_Type --
   -------------------------------------------

   procedure Process_Remote_Access_Subprogram_Type (N : Node_Id) is
      Id : constant Entity_Id := Defining_Entity (N);
      Vi : constant List_Id   := Visible_Declarations (N);
      Pr : constant List_Id   := Private_Declarations (N);
      Fl : List_Id := New_List;
      Pl : List_Id := New_List;
      Tl : List_Id := New_List;
      Sp : Node_Id;
      Df : Node_Id;

      procedure Build_Lists (L : List_Id);
      --  Given input list L, seperate declarations into three lists, one
      --  access type list, one function specification list and one
      --  procedure specification list

      -----------------
      -- Build_Lists --
      -----------------

      procedure Build_Lists (L : List_Id) is
         Decl : Node_Id := First (L);

      begin
         while Present (Decl) loop
            if Nkind (Decl) = N_Subprogram_Declaration then
               Sp := Specification (Decl);

               if Nkind (Sp) = N_Procedure_Specification then
                  Append (Sp, Pl);

               elsif Nkind (Sp) = N_Function_Specification then
                  Append (Sp, Fl);
               end if;

            elsif Nkind (Decl) = N_Full_Type_Declaration then
               Df := Type_Definition (Decl);

               if Present (Df) then
                  if Nkind (Df) = N_Access_Procedure_Definition
                    or else Nkind (Df) = N_Access_Function_Definition
                  then
                     Append (Df, Tl);
                  end if;
               end if;
            end if;

            Decl := Next (Decl);
         end loop;
      end Build_Lists;

   --  Start processing of Process_Remote_Access_Subprogram_Type

   begin
      if Present (Vi) then
         Build_Lists (Vi);
      end if;

      if Present (Pr) then
         Build_Lists (Pr);
      end if;

      --  Return if no remote access to subprogram type declaration

      if not Present (Tl) then
         return;
      end if;

      return;
   end Process_Remote_Access_Subprogram_Type;

   ----------------------------------
   -- Process_Remote_AST_Attribute --
   ----------------------------------

   procedure Process_Remote_AST_Attribute
     (N        : Node_Id;
      New_Type : Entity_Id)
   is
      Loc                   : constant Source_Ptr := Sloc (N);
      Remote_Subp           : Entity_Id;
      Tick_Access_Conv_Call : Node_Id;
      Remote_Subp_Decl      : Node_Id;
      RAS_Decl              : Node_Id;
      Decl                  : Node_Id;
      RS_Pkg_Specif         : Node_Id;
      RS_Pkg_E              : Entity_Id;
      RAS_Pkg_E             : Entity_Id;
      RAS_Type              : Entity_Id;
      RAS_Name              : Name_Id;
      Async_E               : Entity_Id;
      Subp_Id               : Int := 1;
      Attribute_Subp        : Entity_Id;

   --  Start processing of Process_Remote_AST_Attribute

   begin

      --  check if we have to expand the access attribute

      if Ekind (New_Type) /= E_Record_Type
        or else No (Corresponding_Remote_Type (New_Type))
        or else Distribution_Stub_Mode = No_Stubs
        or else not Is_Entity_Name (Prefix (N))
      then
         return;
      else
         Remote_Subp := Entity (Prefix (N));
         RAS_Type  := Corresponding_Remote_Type (New_Type);
         RAS_Name  := Chars (RAS_Type);
         RAS_Decl := Parent (RAS_Type);
         Attribute_Subp := TSS (New_Type, Name_uRAS_Access);

         if No (Attribute_Subp) then
            Add_RAST_Features (RAS_Decl);
            Attribute_Subp := TSS (New_Type, Name_uRAS_Access);
         end if;
      end if;

      RAS_Pkg_E  := Defining_Entity (Parent (RAS_Decl));
      Remote_Subp_Decl := Get_Declaration_Node (Remote_Subp);

      if Nkind (Remote_Subp_Decl) = N_Subprogram_Body then
         Remote_Subp := Corresponding_Spec (Remote_Subp_Decl);
         Remote_Subp_Decl := Get_Declaration_Node (Remote_Subp);
      end if;

      RS_Pkg_Specif := Parent (Remote_Subp_Decl);
      RS_Pkg_E := Defining_Entity (RS_Pkg_Specif);

      --  Compute the subprogram Id

      Decl := Prev (Remote_Subp_Decl);

      while Present (Decl)
        and then Nkind (Decl) = N_Subprogram_Declaration
        and then Comes_From_Source (Decl)
      loop
         Subp_Id := Subp_Id + 1;
         Decl := Prev (Decl);
      end loop;

      if Ekind (Remote_Subp) = E_Procedure
        and then Is_Asynchronous (Remote_Subp)
      then
         Async_E := Standard_True;
      else
         Async_E := Standard_False;
      end if;

      Tick_Access_Conv_Call :=
        Make_Function_Call (Loc,
          Name => New_Occurrence_Of (Attribute_Subp, Loc),
          Parameter_Associations =>
            New_List (
              Relocate_Node (N),
              Make_String_Literal (Loc, Full_Qualified_Name (RS_Pkg_E)),
              Make_Integer_Literal (Loc, UI_From_Int (Subp_Id)),
              New_Reference_To (Async_E, Loc)));

      Rewrite_Or_Replace (N, Tick_Access_Conv_Call);
      Analyze (N);

   end Process_Remote_AST_Attribute;

   ------------------------------------
   -- Process_Remote_AST_Declaration --
   ------------------------------------

   procedure Process_Remote_AST_Declaration (N : Node_Id) is
      Loc                   : constant Source_Ptr := Sloc (N);
      User_Type             : constant Node_Id := Defining_Identifier (N);
      Distributed_Name      : constant Name_Id := Get_Name_Id ("distributed");
      Origin_Name           : constant Name_Id := Get_Name_Id ("origin");
      Receiver_Name         : constant Name_Id := Get_Name_Id ("receiver");
      Ras_Name              : constant Name_Id := Get_Name_Id ("ras");
      Pkg_Name              : constant Name_Id := Get_Name_Id ("pkg_name");
      Subp_Id_Name          : constant Name_Id := Get_Name_Id ("subp_id");
      Async_Name            : constant Name_Id := Get_Name_Id ("async");
      Param_Specs           : List_Id := New_List;
      Fat_Type              : constant Entity_Id :=
                                Make_Defining_Identifier
                                  (Loc, Chars (User_Type));
      New_Type_Decl         : Node_Id;

   begin

      --  We add a record type declaration for the equivalent fat pointer type.

      New_Type_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Fat_Type,
          Discriminant_Specifications =>
            New_List (
              Make_Discriminant_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc, Distributed_Name),
                Discriminant_Type =>
                  New_Reference_To (Standard_Boolean, Loc),
                Expression =>
                  New_Reference_To (Standard_False, Loc))),

          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List =>
                Make_Component_List (Loc,
                  Component_Items => New_List,
                  Variant_Part    =>
                    Make_Variant_Part (Loc,
                      Name => Make_Identifier (Loc, Distributed_Name),
                      Variants =>

                        New_List (
                          Make_Variant (Loc,
                            Discrete_Choices =>
                              New_List (
                                New_Reference_To (Standard_True, Loc)),
                            Component_List =>
                              Make_Component_List (Loc,
                                Component_Items =>

                                  New_List (
                                    Make_Component_Declaration (Loc,
                                      Defining_Identifier =>
                                        Make_Defining_Identifier (Loc,
                                          Chars => Origin_Name),
                                      Subtype_Indication =>
                                        New_Reference_To
                                          (Standard_Integer,
                                           Loc)),

                                    Make_Component_Declaration (Loc,
                                      Defining_Identifier =>
                                        Make_Defining_Identifier (Loc,
                                          Chars => Receiver_Name),
                                      Subtype_Indication =>
                                        New_Reference_To
                                          (RTE (RE_Address), Loc)),

                                    Make_Component_Declaration (Loc,
                                      Defining_Identifier =>
                                        Make_Defining_Identifier (Loc,
                                          Chars => Subp_Id_Name),
                                      Subtype_Indication =>
                                        New_Reference_To
                                          (Standard_Natural,
                                           Loc)),

                                    Make_Component_Declaration (Loc,
                                      Defining_Identifier =>
                                        Make_Defining_Identifier (Loc,
                                          Chars => Async_Name),
                                      Subtype_Indication =>
                                        New_Reference_To
                                          (Standard_Boolean,
                                           Loc))))),
                          Make_Variant (Loc,
                            Discrete_Choices =>
                              New_List (
                                New_Reference_To (Standard_False, Loc)),
                            Component_List =>
                              Make_Component_List (Loc,
                                Component_Items =>

                                  New_List (
                                    Make_Component_Declaration (Loc,
                                      Defining_Identifier =>
                                        Make_Defining_Identifier (Loc,
                                          Chars => Ras_Name),
                                      Subtype_Indication =>
                                        New_Occurrence_Of
                                          (User_Type, Loc))))))))));

      Insert_After (N, New_Type_Decl);
      Set_Equivalent_Type (User_Type, Fat_Type);
      Set_Corresponding_Remote_Type (Fat_Type, User_Type);

   end Process_Remote_AST_Declaration;

   -----------------------
   -- RAS_E_Dereference --
   -----------------------

   procedure RAS_E_Dereference (Pref : Node_Id) is
      Loc             : constant Source_Ptr := Sloc (Pref);
      Call_Node       : Node_Id;
      New_Type        : constant Entity_Id := Etype (Pref);
      RAS             : constant Entity_Id :=
                          Corresponding_Remote_Type (New_Type);
      RAS_Decl        : constant Node_Id   := Parent (RAS);
      Explicit_Deref  : constant Node_Id   := Parent (Pref);
      Deref_Subp_Call : constant Node_Id   := Parent (Explicit_Deref);
      Deref_Proc      : Entity_Id;
      Params          : List_Id;

   begin
      if Nkind (Deref_Subp_Call) = N_Procedure_Call_Statement then
         Params := Parameter_Associations (Deref_Subp_Call);

         if Present (Params) then
            Prepend (Pref, Params);
         else
            Params := New_List (Pref);
         end if;

      elsif Nkind (Deref_Subp_Call) = N_Indexed_Component then

         Params := Expressions (Deref_Subp_Call);

         if Present (Params) then
            Prepend (Pref, Params);
         else
            Params := New_List (Pref);
         end if;

      else
         --  context is not a call.

         return;
      end if;

      Deref_Proc := TSS (New_Type, Name_uRAS_Dereference);

      if No (Deref_Proc) then
         Add_RAST_Features (RAS_Decl);
         Deref_Proc := TSS (New_Type, Name_uRAS_Dereference);
      end if;

      if Ekind (Deref_Proc) = E_Function then
         Call_Node :=
           Make_Function_Call (Loc,
              Name => New_Occurrence_Of (Deref_Proc, Loc),
              Parameter_Associations => Params);
      else
         Call_Node :=
           Make_Procedure_Call_Statement (Loc,
              Name => New_Occurrence_Of (Deref_Proc, Loc),
              Parameter_Associations => Params);
      end if;

      Rewrite_Or_Replace (Deref_Subp_Call, Call_Node);
      Analyze (Deref_Subp_Call);
   end RAS_E_Dereference;


   ------------------------------
   -- Remote_AST_E_Dereference --
   ------------------------------

   function Remote_AST_E_Dereference (P : Node_Id) return Boolean
   is
      ET       : constant Entity_Id  := Etype (P);
   begin
      --  Perform the changes only on original dereferences.

      if Comes_From_Source (P)
        and then Is_Record_Type (ET)
        and then (Is_Remote_Call_Interface (ET)
                   or else Is_Remote_Types (ET))
        and then Present (Corresponding_Remote_Type (ET))
        and then (Nkind (Parent (Parent (P))) = N_Procedure_Call_Statement
                   or else Nkind (Parent (Parent (P))) = N_Indexed_Component)
        and then Distribution_Stub_Mode /= No_Stubs
      then
         RAS_E_Dereference (P);
         return True;
      else
         return False;
      end if;
   end Remote_AST_E_Dereference;

   ------------------------------
   -- Remote_AST_I_Dereference --
   ------------------------------

   function Remote_AST_I_Dereference (P : Node_Id) return Boolean
   is
      ET     : constant Entity_Id  := Etype (P);
      Deref  : Node_Id;
   begin

      if Comes_From_Source (P)
        and then (Is_Remote_Call_Interface (ET)
                   or else Is_Remote_Types (ET))
        and then Present (Corresponding_Remote_Type (ET))
        and then Distribution_Stub_Mode /= No_Stubs
      then
         Deref := Make_Explicit_Dereference (Sloc (P),
           Prefix => Relocate_Node (P));
         Rewrite_Substitute_Tree (P, Deref);
         Set_Etype (P, ET);
         RAS_E_Dereference (Prefix (P));
         return True;
      end if;

      return False;
   end Remote_AST_I_Dereference;

   ------------------------
   -- Rewrite_Or_Replace --
   ------------------------

   procedure Rewrite_Or_Replace (N : Node_Id; New_Node : Node_Id) is
   begin
      if Distribution_Stub_Mode = Generate_Receiver_Stub_Body
        or else Distribution_Stub_Mode = Generate_Caller_Stub_Body
      then
         Replace_Substitute_Tree (N, New_Node);
      else
         Rewrite_Substitute_Tree (N, New_Node);
      end if;
   end Rewrite_Or_Replace;

   -------------------------------------
   -- Set_Categorization_From_Pragmas --
   -------------------------------------

   procedure Set_Categorization_From_Pragmas (N : Node_Id) is
      P : constant Node_Id := Parent (N);

   begin

      --  Deal with categorization pragmas in Pragmas of Compilation_Unit.
      --  The purpose is to set categorization flags before analyzing the
      --  unit itself, so as to diagnose violations of categorization as
      --  we process each declaration, even though the pragma appears after
      --  the unit.

      if Nkind (P) /= N_Compilation_Unit then
         return;
      end if;

      if Present (Pragmas_After (P)) then
         declare
            Pragma_Node : Node_Id := First (Pragmas_After (P));

         begin
            while Present (Pragma_Node) loop

               --  Skip implicit types that may have been introduced by
               --  previous analysis.

               if Nkind (Pragma_Node) = N_Pragma then

                  case Get_Pragma_Id (Chars (Pragma_Node)) is
                     when Pragma_All_Calls_Remote   |
                       Pragma_Preelaborate          |
                       Pragma_Pure                  |
                       Pragma_Remote_Call_Interface |
                       Pragma_Remote_Types          |
                       Pragma_Shared_Passive        => Analyze (Pragma_Node);
                     when others                    => null;
                  end case;
               end if;

               Pragma_Node := Next (Pragma_Node);
            end loop;
         end;
      end if;
   end Set_Categorization_From_Pragmas;

   ---------------------------------
   -- Should_Declare_Partition_ID --
   ---------------------------------

   function Should_Declare_Partition_ID (L : List_Id) return Boolean is
      Nd : Node_Id := First (L);
      Ch : Name_Id;
      Na : Node_Id := Defining_Unit_Name (Parent (L));

   begin
      while Present (Nd) loop
         if Nkind (Nd) = N_Pragma then
            Ch := Chars (Nd);

            if Ch = Name_Preelaborate
              or else Ch = Name_Remote_Call_Interface
              or else Ch = Name_Shared_Passive
              or else Ch = Name_Remote_Types
            then
               return True;

            elsif Ch = Name_Pure then
               return False;
            end if;
         end if;

         Nd := Next (Nd);
      end loop;

      --  This is a non-categorizaed library unit

      if Nkind (Na) = N_Defining_Program_Unit_Name
        and then Nkind (Name (Na)) = N_Identifier
        and then Chars (Name (Na)) = Name_System
        and then Nkind (Defining_Identifier (Na)) = N_Defining_Identifier
        and then Chars (Defining_Identifier (Na)) = Name_Rpc
      then
         return True;
      end if;

      return False;
   end Should_Declare_Partition_ID;

   ------------------------------
   -- Static_Discriminant_Expr --
   ------------------------------

   function Static_Discriminant_Expr (L : List_Id) return Boolean is
      Discriminant_Spec : Node_Id;

   begin
      Discriminant_Spec := First (L);
      while Present (Discriminant_Spec) loop
         if Present (Expression (Discriminant_Spec))
           and then not Is_Static_Expression (Expression (Discriminant_Spec))
         then
            return False;
         end if;

         Discriminant_Spec := Next (Discriminant_Spec);
      end loop;

      return True;
   end Static_Discriminant_Expr;

   --------------------------------------
   -- Validate_Access_Type_Declaration --
   --------------------------------------

   procedure Validate_Access_Type_Declaration (T : Entity_Id; N : Node_Id) is
      Def : constant Node_Id := Type_Definition (N);

   begin
      case Nkind (Def) is
         when N_Access_To_Subprogram_Definition =>

            --  A pure library_item must not contain the declaration of a
            --  named access type, except within a subprogram, generic
            --  subprogram, task unit, or protected unit (RM 10.2.1(16)).

            if Comes_From_Source (T)
               and then In_Pure_Unit
               and then not In_Subprogram_Task_Protected_Unit
            then
               Error_Msg_N ("named access type not allowed in pure unit", T);
            end if;

            --  Set Is_Remote_Call_Interface flag on entity to allow easy
            --  checks later on for required validations of RCI units. This
            --  is only done for entities that are in the original source.

            if Comes_From_Source (T)
              and then In_Remote_Call_Interface_Unit
            then
               Set_Is_Remote_Call_Interface (T);
            end if;

            --  Set Is_Remote_Types flag on entity to allow easy
            --  checks later on for required validations of such units. This
            --  is only done for entities that are in the original source.

            if Comes_From_Source (T)
              and then In_Remote_Types_Unit
            then
               Set_Is_Remote_Types (T);
            end if;

         when N_Access_To_Object_Definition =>

            if Comes_From_Source (T)
              and then In_Pure_Unit
              and then not In_Subprogram_Task_Protected_Unit
            then
               Error_Msg_N
                 ("named access type not allowed in pure unit", T);
            end if;

            --  Check for RCI unit type declaration. It should not contain
            --  the declaration of an access-to-object type unless it is a
            --  general access type that designates a class-wide limited
            --  private type. There are also constraints about the primitive
            --  subprograms of the class-wide type.

            Validate_RCI_Access_Object_Type_Declaration (T);

            --  Check for shared passive unit type declaration. It should
            --  not contain the declaration of access to class wide type,
            --  access to task type and access to protected type with entry.

            Validate_SP_Access_Object_Type_Decl (T);

            --  Set Is_Remote_Types flag on entity to allow easy
            --  checks later on for required validations of such units. This
            --  is only done for entities that are in the original source.

            if Comes_From_Source (T)
              and then In_Remote_Types_Unit
            then
               Set_Is_Remote_Types (T);
            end if;

         when others => null;

      end case;

   end Validate_Access_Type_Declaration;

   ----------------------------------------
   -- Validate_Categorization_Dependency --
   ----------------------------------------

   procedure Validate_Categorization_Dependency
     (N : Node_Id;
      E : Entity_Id)
   is
      K : constant Node_Kind := Nkind (N);
      P : constant Node_Id   := Parent (N);

   begin
      --  Validate library unit only

      if Nkind (P) /= N_Compilation_Unit then
         return;
      end if;

      --  Body of RCI unit does not need validation.

      if Is_Remote_Call_Interface (E)
        and then (Nkind (N) = N_Package_Body
                   or else Nkind (N) = N_Subprogram_Body)
      then
         return;
      end if;

      --  Process with clauses

      declare
         Item             : Node_Id;
         Entity_Of_Withed : Entity_Id;

      begin
         Item := First (Context_Items (P));

         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
              and then not Implicit_With (Item)
            then
               Entity_Of_Withed := Entity (Name (Item));
               Check_Categorization_Dependencies (E, Entity_Of_Withed, Item);
            end if;

            Item := Next (Item);
         end loop;
      end;

      --  Child depends on parent therefore parent should also
      --  be categorized and satify the dependecy hierarchy.

      --  Check if N is a child spec.

      if (K in N_Generic_Declaration              or else
          K in N_Generic_Instantiation            or else
          K in N_Generic_Renaming_Declaration     or else
          K =  N_Package_Declaration              or else
          K =  N_Package_Renaming_Declaration     or else
          K =  N_Subprogram_Declaration           or else
          K =  N_Subprogram_Renaming_Declaration)
        and then Present (Parent_Spec (N))
      then
         declare
            Parent_Lib_U  : constant Node_Id   := Parent_Spec (N);
            Parent_Kind   : constant Node_Kind :=
                              Nkind (Unit (Parent_Lib_U));
            Parent_Entity : Entity_Id;

         begin
            if        Parent_Kind =  N_Package_Instantiation
              or else Parent_Kind =  N_Procedure_Instantiation
              or else Parent_Kind =  N_Function_Instantiation
              or else Parent_Kind =  N_Package_Renaming_Declaration
              or else Parent_Kind in N_Generic_Renaming_Declaration
            then
               Parent_Entity := Defining_Entity (Unit (Parent_Lib_U));

            else
               Parent_Entity :=
                 Defining_Entity (Specification (Unit (Parent_Lib_U)));
            end if;

            Check_Categorization_Dependencies (E, Parent_Entity, N);

            --  Verify that public child of an RCI library unit
            --  must also be an RCI library unit (RM E.2.3(15)).

            if Is_Remote_Call_Interface (Parent_Entity)
              and then not Private_Present (P)
              and then not Is_Remote_Call_Interface (E)
            then
               Error_Msg_N
                 ("public child of rci unit must also be rci unit", N);
               return;
            end if;
         end;
      end if;

   end Validate_Categorization_Dependency;

   ------------------------------
   -- Validate_Non_Static_Call --
   ------------------------------

   procedure Validate_Non_Static_Call (N : Node_Id) is
   begin
      if Comes_From_Source (N)
        and then not In_Subprogram_Unit
        and then In_Preelaborated_Unit
      then
         --  In case non-static call is inside a record type declaration
         --  we should skip the error message

         if Nkind (Parent (N)) = N_Component_Declaration then
            return;
         end if;

         Error_Msg_N ("non-static call not allowed in preelaborated unit", N);
      end if;
   end Validate_Non_Static_Call;

   --------------------------------------
   -- Validate_Null_Statement_Sequence --
   --------------------------------------

   procedure Validate_Null_Statement_Sequence (N : Node_Id) is
      Item : Node_Id;

   begin
      if In_Preelaborated_Unit then
         Item := First (Statements (Handled_Statement_Sequence (N)));

         while Present (Item) loop
            if Nkind (Item) /= N_Label
              and then Nkind (Item) /= N_Null_Statement
            then
               Error_Msg_N
                 ("statements not allowed in preelaborated unit", Item);
               exit;
            end if;

            Item := Next (Item);
         end loop;
      end if;
   end Validate_Null_Statement_Sequence;

   ---------------------------------
   -- Validate_Object_Declaration --
   ---------------------------------

   procedure Validate_Object_Declaration
     (N   : Node_Id;
      Id  : Entity_Id;
      E   : Node_Id;
      Odf : Node_Id;
      T   : Entity_Id)
   is
   begin
      --  Verify that any access to subprogram object does not have in its
      --  subprogram profile access type parameters or limited parameters
      --  without Read and Write attributes (E.2.3(13)).

      Validate_RCI_Subprogram_Declaration (N);

      --  Check that if we are in preelaborated elaboration code, then we
      --  do not have an instance of a default initialized private, task or
      --  protected object declaration which would violate (RM 10.2.1(9)).
      --  Note that constants are never default initialized (and the test
      --  below also filters out deferred constants). A variable is default
      --  initialized if it does *not* have an initialization expression.

      --  Filter out cases that are not declaration of a variable from source.

      if Nkind (N) /= N_Object_Declaration
        or else Constant_Present (N)
        or else not Comes_From_Source (Id)
      then
         return;
      end if;

      if In_Preelaborated_Unit
        and then not In_Subprogram_Unit
      then
         if No (E) then
            declare
               Ent : Entity_Id;

            begin
               --  Object decl. that is of record type and has no default expr.
               --  should check if there is any non-static default expression
               --  in component decl. of the record type decl.

               if Is_Record_Type (T) then
                  if Nkind (Parent (T)) = N_Full_Type_Declaration then
                     Check_Non_Static_Default_Expr
                       (Type_Definition (Parent (T)));

                  elsif Nkind (Odf) = N_Subtype_Indication then
                     Check_Non_Static_Default_Expr (Type_Definition
                       (Parent (Entity (Subtype_Mark (Odf)))));
                  end if;
               end if;

               --  Similarly, array whose component type is record of component
               --  declarations with default expression that is non-static
               --  is a violation.

               if Is_Array_Type (T) then
                  if Nkind (Parent (T)) = N_Full_Type_Declaration then
                     declare
                        Comp_Type : Entity_Id := Component_Type (T);

                     begin
                        while Is_Array_Type (Comp_Type) loop
                           Comp_Type := Component_Type (Comp_Type);
                        end loop;

                        if Is_Record_Type (Comp_Type) then
                           if Nkind (Parent (Comp_Type)) =
                             N_Full_Type_Declaration
                           then
                              Check_Non_Static_Default_Expr
                                  (Type_Definition (Parent (Comp_Type)));
                           end if;
                        end if;
                     end;
                  end if;
               end if;

               if Is_Private_Type (Id)
                 or else
                   (Is_Access_Type (T)
                     and then
                       Depends_On_Private (Designated_Type (T)))
                 or else Depends_On_Private (T)
               then
                  Error_Msg_N
                    ("private object not allowed in preelaborated unit", N);
                  return;

               --  Access to Task or Protected type

               elsif Nkind (Odf) = N_Identifier
                 and then Present (Etype (Odf))
                 and then Is_Access_Type (Etype (Odf))
               then
                  Ent := Designated_Type (Etype (Odf));

               elsif Nkind (Odf) = N_Identifier then
                  Ent := Entity (Odf);

               elsif Nkind (Odf) = N_Subtype_Indication then
                  Ent := Etype (Subtype_Mark (Odf));

               elsif
                  Nkind (Odf) = N_Constrained_Array_Definition
               then
                  Ent := Component_Type (T);

               else
                  return;
               end if;

               if Is_Task_Type (Ent)
                 or else (Is_Protected_Type (Ent) and then Has_Entries (Ent))
               then
                  Error_Msg_N
                    ("concurrent object not allowed in preelaborated unit",
                     N);
                  return;

               end if;
            end;
         end if;

         --  Evaluation of discriminant default expr. is done when obj.
         --  is created. And it has to be static expr.

         if Is_Record_Type (Etype (Id)) then
            declare
               ET  : constant Entity_Id := Etype (Id);
               EE  : constant Entity_Id := Etype (Etype (Id));
               PEE : Node_Id;

            begin
               if Has_Discriminants (ET)
                 and then Present (EE)
               then
                  PEE := Parent (EE);

                  if Nkind (PEE) = N_Full_Type_Declaration
                    and then not Static_Discriminant_Expr
                                  (Discriminant_Specifications (PEE))
                  then
                     Error_Msg_N
                       ("non-static discriminant in preelaborated unit",
                        PEE);
                  end if;
               end if;
            end;
         end if;

         --  Similarly, array whose component type is record of component
         --  declarations with discriminant expression that is non-static
         --  is a violation.

         if Is_Array_Type (T) then
            if Nkind (Parent (T)) = N_Full_Type_Declaration then
               declare
                  Comp_Type : Entity_Id := Component_Type (T);

               begin
                  while Is_Array_Type (Comp_Type) loop
                     Comp_Type := Component_Type (Comp_Type);
                  end loop;

                  if Is_Record_Type (Comp_Type)
                    and then Has_Discriminants (Comp_Type)
                    and then
                      Nkind (Parent (Comp_Type)) = N_Full_Type_Declaration
                    and then not Static_Discriminant_Expr
                      (Discriminant_Specifications (Parent (Comp_Type)))
                  then
                     Error_Msg_N
                       ("non-static discriminant in preelaborated unit",
                        Comp_Type);
                  end if;
               end;
            end if;
         end if;

      end if;

      --  A pure library_item must not contain the declaration of any
      --  variable except within  a subprogram, generic subprogram, task
      --  unit or protected unit (RM 10.2.1(16)).

      if In_Pure_Unit
        and then not In_Subprogram_Task_Protected_Unit
      then
         Error_Msg_N ("declaration of variable not allowed in pure unit", N);

      --  The visible part of an RCI library unit must not contain the
      --  declaration of a variable (RM E.1.3(9))

      elsif In_Remote_Call_Interface_Unit then
         Error_Msg_N ("declaration of variable not allowed in rci unit", N);

      --  The visible part of a Shared Passive library unit must not contain
      --  the declaration of a variable (RM E.2.2(7))

      elsif In_Remote_Types_Unit then
         Error_Msg_N
           ("variable declaration not allowed in remote types unit", N);
      end if;

   end Validate_Object_Declaration;

   --------------------------------
   --  Validate_RCI_Declarations --
   --------------------------------

   procedure Validate_RCI_Declarations (P : Entity_Id) is
      E : Entity_Id;

   begin

      E := First_Entity (P);

      while Present (E) loop
         if Comes_From_Source (E) then

            if Is_Limited_Type (E) then
               Error_Msg_N
                 ("Limited type not allowed in rci unit", Parent (E));

            elsif Ekind (E) = E_Generic_Function
              or else Ekind (E) = E_Generic_Package
              or else Ekind (E) = E_Generic_Procedure
            then
               Error_Msg_N ("generic declaration not allowed in rci unit",
                 Parent (E));

            elsif (Ekind (E) = E_Function
                    or else Ekind (E) = E_Procedure)
              and then Is_Inlined (E)
            then
               Error_Msg_N
                 ("inlined subprogram not allowed in rci unit", Parent (E));

            elsif Ekind (E) = E_Package then
               Validate_RCI_Declarations (E);
            end if;
         end if;

         E := Next_Entity (E);
      end loop;
   end Validate_RCI_Declarations;

   -------------------------------------------------
   -- Validate_RCI_Access_Object_Type_Declaration --
   -------------------------------------------------

   procedure Validate_RCI_Access_Object_Type_Declaration (T : Entity_Id) is
      Direct_Designated_Type : Entity_Id;
      Desig_Type         : Entity_Id;
      Primitive_Subprograms  : Elist_Id;
      Type_Decl              : Node_Id;
      Subprogram             : Elmt_Id;
      Subprogram_Node        : Node_Id;
      Profile                : List_Id;
      Param_Spec             : Node_Id;
      Param_Type             : Entity_Id;
      Limited_Type_Decl      : Node_Id;

   begin
      --  We are called from Analyze_Type_Declaration, and the Nkind
      --  of the given node is N_Access_To_Object_Definition.

      if not Comes_From_Source (T)
        or else not In_Remote_Call_Interface_Unit
      then
         return;
      end if;

      --  Check RCI unit type declaration. It should not contain the
      --  declaration of an access-to-object type unless it is a
      --  general access type that designates a class-wide limited
      --  private type. There are also constraints about the primitive
      --  subprograms of the class-wide type (RM E.2.3(14)).

      if Ekind (T) /= E_General_Access_Type
        or else Ekind (Designated_Type (T)) /= E_Class_Wide_Type
      then
         Error_Msg_N
           ("Access type in RCI unit must be general access-to-class-wide",
            T);
         return;
      end if;

      Direct_Designated_Type := Designated_Type (T);

      Desig_Type := Etype (Direct_Designated_Type);
      Type_Decl       := Parent (Desig_Type);

      if Nkind (Type_Decl) /= N_Private_Type_Declaration
        or else not Limited_Present (Type_Decl)
        or else Primitive_Operations (Desig_Type) = No_Elist
      then
         Error_Msg_N
           ("designated type must be limited private and have primitive ops",
            T);
         return;
      end if;

      Primitive_Subprograms := Primitive_Operations (Desig_Type);
      Subprogram            := First_Elmt (Primitive_Subprograms);

      while Subprogram /= No_Elmt loop
         Subprogram_Node := Node (Subprogram);

         if not Comes_From_Source (Subprogram_Node) then
            goto Next_Subprogram;
         end if;

         Profile := Parameter_Specifications (Parent (Subprogram_Node));

         --  Profile must exist, otherwise not primitive operation

         Param_Spec := First (Profile);

         while Present (Param_Spec) loop

            --  Now find out if this parameter is a controlling parameter

            Param_Type := Parameter_Type (Param_Spec);

            if Nkind (Param_Type) = N_Identifier
              and then Etype (Param_Type) = Desig_Type
            then
               --  It is indeed a controlling parameter, and since it's not
               --  an access parameter, this is a violation.

               Error_Msg_N
                 ("not access control parameter in rci unit", Param_Spec);

            elsif Nkind (Param_Type) = N_Access_Definition
              and then Subtype_Mark (Param_Type) = Desig_Type
            then
               --  It is indeed controlling parameter but since it's an
               --  access parameter, this is not a violation.

               null;

            elsif
              Is_Limited_Type (Etype (Defining_Identifier (Param_Spec)))
            then
               --  Not a controlling parameter, so type must have Read
               --  and Write attributes.

               if Nkind (Param_Type) = N_Identifier
                 and then Nkind (Parent (Etype (Param_Type))) =
                          N_Private_Type_Declaration
               then
                  Param_Type := Etype (Param_Type);
                  Limited_Type_Decl := Parent (Param_Type);


                  if No (TSS (Param_Type, Name_uRead))
                    or else No (TSS (Param_Type, Name_uWrite))
                  then
                     Error_Msg_N
                       ("limited formal must have read/write attributes",
                         Param_Spec);
                  end if;
               end if;
            end if;

            --  Check next parameter in this subprogram

            Param_Spec  := Next (Param_Spec);
         end loop;

         <<Next_Subprogram>>
            Subprogram := Next_Elmt (Subprogram);
      end loop;

      --  Now this is an RCI unit access-to-class-wide-limited-private type
      --  declaration. Set the type entity to be Is_Remote_Call_Interface to
      --  optimize later checks by avoiding tree traversal to find out if this
      --  entity is inside an RCI unit.

      Set_Is_Remote_Call_Interface (T);

   end Validate_RCI_Access_Object_Type_Declaration;

   -----------------------------------------
   -- Validate_RCI_Subprogram_Declaration --
   -----------------------------------------

   procedure Validate_RCI_Subprogram_Declaration (N : Node_Id) is
      K           : Node_Kind := Nkind (N);
      Profile     : List_Id;
      Id          : Node_Id;
      Param_Spec  : Node_Id;
      Param_Type  : Entity_Id;
      Type_Decl   : Node_Id;
      Error_Node  : Node_Id := N;

   begin
      --  There are two possible cases in which this procedure is called:

      --    1. called from Analyze_Subprogram_Declaration.
      --    2. called from Validate_Object_Declaration (access to subprogram).

      if not In_Remote_Call_Interface_Unit then
         return;
      end if;

      if K = N_Subprogram_Declaration then
         Profile := Parameter_Specifications (Specification (N));

      elsif K = N_Object_Declaration then
         Id := Defining_Identifier (N);

         if Nkind (Id) = N_Defining_Identifier
           and then Nkind (Parent (Etype (Id))) = N_Full_Type_Declaration
           and then Ekind (Etype (Id)) = E_Access_Subprogram_Type
         then
            Profile :=
              Parameter_Specifications (Type_Definition (Parent (Etype (Id))));
         else
            return;
         end if;
      end if;

      --  Iterate through the parameter specification list, checking that
      --  no access parameter and no limited type paramter in the list.

      if Present (Profile) then
         Param_Spec := First (Profile);

         while Present (Param_Spec) loop
            Param_Type := Etype (Defining_Identifier (Param_Spec));
            Type_Decl  := Parent (Param_Type);

            if Ekind (Param_Type) = E_Anonymous_Access_Type then

               if K = N_Subprogram_Declaration then
                  Error_Node := Param_Spec;
               end if;

               --  Report error only if declaration is in source program.

               if Comes_From_Source
                 (Defining_Unit_Name (Specification (N)))
               then
                  Error_Msg_N
                    ("subprogram in rci unit cannot have access parameter",
                      Error_Node);
               end if;

            --  For limited private type parameter, we check only the
            --  private declaration and ignore full type declaration.

            elsif Is_Limited_Type (Param_Type)
              and then Nkind (Type_Decl) = N_Private_Type_Declaration
            then

               --  A limited parameter is legal only if user-specified
               --  Read and Write attributes exist for it.

               if No (TSS (Param_Type, Name_uRead))
                 or else No (TSS (Param_Type, Name_uWrite))
               then

                  if K = N_Subprogram_Declaration then
                     Error_Node := Param_Spec;
                  end if;

                  Error_Msg_N
                    ("limited parameter in rci unit "
                       & "must have read/write attributes ", Error_Node);
               end if;
            end if;

            Param_Spec  := Next (Param_Spec);
         end loop;
      end if;
   end Validate_RCI_Subprogram_Declaration;

   -----------------------------------------------
   -- Validate_Remote_Access_To_Class_Wide_Type --
   -----------------------------------------------

   procedure Validate_Remote_Access_To_Class_Wide_Type (N : Node_Id) is
      K  : constant Node_Kind := Nkind (N);
      PK : constant Node_Kind := Nkind (Parent (N));
      E  : Entity_Id;

   begin
      --  This subprogram enforces the checks in (RM E.2.2(8)) for
      --  certain uses of class-wide limited private types.

      --    Storage_Pool and Storage_Size are not defined for such types
      --
      --    The expected type of allocator must not not be such a type.

      --    The actual parameter of generic instantiation must not
      --    be such a type.

      --  On entry, there are four cases

      --    1. called from sem_attr Analyze_Attribute where attribute
      --       name is either Storage_Pool or Storage_Size.

      --    2. called from exp_ch4 Expand_N_Allocator

      --    3. called from sem_ch12 Analyze_Associations

      --    4. called from sem_ch4 Analyze_Explicit_Dereference

      if not Present (N)
        or else Distribution_Stub_Mode = Compile_Receiver_Stub_Spec
        or else Distribution_Stub_Mode = Compile_Caller_Stub_Spec
      then
         return;
      end if;

      if K = N_Attribute_Reference then
         E := Etype (Prefix (N));

         if Is_Remote_Access_To_Class_Wide_Type (E) then
            Error_Msg_N ("incorrect attribute of remote operand", N);
            return;
         end if;

      elsif K = N_Allocator then
         E := Etype (N);

         if Is_Remote_Access_To_Class_Wide_Type (E) then
            Error_Msg_N ("incorrect expected remote type of allocator", N);
            return;
         end if;

      elsif K = N_Identifier then
         E := Entity (N);

         if Is_Remote_Access_To_Class_Wide_Type (E) then
            Error_Msg_N ("incorrect remote type generic actual", N);
            return;
         end if;

      --  This subprogram also enforces the checks in E.2.2(13).
      --  A value of such type must not be explicitly dereferenced
      --  unless in a dispatching call.

      elsif K = N_Explicit_Dereference then
         E := Etype (Prefix (N));

         if Is_Remote_Access_To_Class_Wide_Type (E)
           and then PK /= N_Procedure_Call_Statement
           and then PK /= N_Function_Call
         then
            --  The following is to let the compiler generated tags check
            --  pass through without error message. This is a bit kludgy
            --  isn't there some better way of making this exclusion ???

            if (PK = N_Selected_Component
                 and then Present (Parent (Parent (N)))
                 and then Nkind (Parent (Parent (N))) = N_Op_Ne)
              or else (PK = N_Unchecked_Type_Conversion
                        and then Present (Parent (Parent (N)))
                        and then
                          Nkind (Parent (Parent (N))) = N_Selected_Component)
            then
               return;
            end if;

            --  The following is to let the compiler generated membership
            --  check and type conversion pass through without error message.

            if (PK = N_Not_In
                 and then Present (Parent (Parent (N)))
                 and then Nkind (Parent (Parent (N))) = N_If_Statement)
              or else (PK = N_Indexed_Component
                        and then Present (Parent (Parent (N)))
                        and then
                          Nkind (Parent (Parent (N))) = N_Selected_Component)
            then
               return;
            end if;

            Error_Msg_N ("incorrect remote type dereference", N);
         end if;
      end if;
   end Validate_Remote_Access_To_Class_Wide_Type;

   ------------------------------------------
   -- Validate_Remote_Type_Type_Conversion --
   ------------------------------------------

   procedure Validate_Remote_Type_Type_Conversion (N : Node_Id) is
      S  : constant Entity_Id := Etype (N);
      E  : constant Entity_Id := Etype (Expression (N));

   begin
      --  This test is required in the case where a conversion apears
      --  inside a normal package, it does not necessarily have to be
      --  inside an RCI, Remote_Types unit (RM E.2.2(9,12)).

      if Is_Remote_Access_To_Subprogram_Type (E)
        and then not Is_Remote_Access_To_Subprogram_Type (S)
      then
         Error_Msg_N ("incorrect conversion of remote operand", N);
         return;

      elsif Is_Remote_Access_To_Class_Wide_Type (E)
        and then not Is_Remote_Access_To_Class_Wide_Type (S)
      then
         Error_Msg_N ("incorrect conversion of remote operand", N);
         return;
      end if;
   end Validate_Remote_Type_Type_Conversion;

   -------------------------------
   -- Validate_RT_RAT_Component --
   -------------------------------

   procedure Validate_RT_RAT_Component (N : Node_Id) is
      Spec     : constant Node_Id   := Specification (N);
      Name_U   : constant Entity_Id := Defining_Entity (Spec);
      Visible  : constant List_Id   := Visible_Declarations (Spec);
      Privat   : constant List_Id   := Private_Declarations (Spec);
      Is_RT    : constant Boolean   := Is_Remote_Types (Name_U);
      Ty_Decl  : Node_Id;
      P_Id     : Entity_Id;
      F_Id     : Entity_Id;
      Comp_Lst : List_Id;
      Comp_Itm : Node_Id;
      Comp_Typ : Entity_Id;
      Clause   : Node_Id;
      Found_R  : Boolean;
      Found_W  : Boolean;

   begin
      --  By the time we get here, the unit is already analyzed and it's
      --  safe to test if unit is Remote_Types from unit name.

      if not Is_RT then
         return;
      end if;

      --  Now, violation happens when a visible private type has a
      --  non-visible remote access type component without read/write

      if not Present (Visible)
        or else not Present (Privat)
      then
         return;
      end if;

      Ty_Decl := First (Visible);
      while Present (Ty_Decl) loop
         if Nkind (Ty_Decl) = N_Private_Type_Declaration then

            --  Private Identifier and Full view Identifier

            P_Id := Defining_Identifier (Ty_Decl);
            F_Id := Full_View (P_Id);

            if Ekind (F_Id) /= E_Record_Type then
               return;
            end if;

            --  Now get to the full view definition component items

            if Null_Present (Type_Definition (Parent (F_Id))) then
               goto Next_Type;
            else
               Comp_Lst := Component_Items (Component_List (Type_Definition
                             (Parent (F_Id))));
            end if;

            --  Check if any component is a violation

            Comp_Itm := First (Comp_Lst);
            while Present (Comp_Itm) loop
               if Nkind (Subtype_Indication (Comp_Itm))
                  = N_Subtype_Indication
               then
                  Comp_Typ :=
                    Entity (Subtype_Mark (Subtype_Indication (Comp_Itm)));
               else
                  Comp_Typ :=
                    Entity (Subtype_Indication (Comp_Itm));
               end if;

               Found_R := False;
               Found_W := False;

               --  Candidates are private access types

               if Is_Private (Comp_Typ)
                 and then Ekind (Comp_Typ) = E_Access_Type
               then
                  Clause := First (Privat);

                  while Present (Clause) loop

                     --  Check if read/write present

                     if Nkind (Clause) = N_Attribute_Definition_Clause
                       and then Entity (Name (Clause)) = Comp_Typ
                     then
                        if Chars (Clause) = Name_Read then
                           Found_R := True;
                        elsif  Chars (Clause) = Name_Write then
                           Found_W := True;
                        end if;
                     end if;

                     Clause := Next (Clause);
                  end loop;

                  --  Both read and write have to be defined

                  if not Found_R
                    or else not Found_W
                  then
                     Error_Msg_N
                     ("invalid component type in remote types unit",
                       Comp_Itm);
                  end if;
               end if;
               Comp_Itm := Next (Comp_Itm);
            end loop;
         end if;

         <<Next_Type>>
            Ty_Decl := Next (Ty_Decl);
      end loop;

   end Validate_RT_RAT_Component;

   -----------------------------------------
   -- Validate_SP_Access_Object_Type_Decl --
   -----------------------------------------

   procedure Validate_SP_Access_Object_Type_Decl (T : Entity_Id) is
      Direct_Designated_Type : Entity_Id;

      function Has_Entry_Declarations (E : Entity_Id) return Boolean;
      --  Return true if the protected type designated by T has
      --  entry declarations.

      function Has_Entry_Declarations (E : Entity_Id) return Boolean is
         Ety : Entity_Id;

      begin
         if Nkind (Parent (E)) = N_Protected_Type_Declaration then
            Ety := First_Entity (E);
            while Present (Ety) loop
               if Ekind (Ety) = E_Entry then
                  return True;
               end if;

               Ety := Next (Ety);
            end loop;
         end if;

         return False;
      end Has_Entry_Declarations;

   --  Start of processing for Validate_SP_Access_Object_Type_Decl

   begin
      --  We are called from Sem_Ch3.Analyze_Type_Declaration, and the
      --  Nkind of the given entity is N_Access_To_Object_Definition.

      if not Comes_From_Source (T)
        or else not In_Shared_Passive_Unit
        or else In_Subprogram_Task_Protected_Unit
      then
         return;
      end if;

      --  Check Shared Passive unit. It should not contain the declaration
      --  of an access-to-object type whose designated type is a class-wide
      --  type, task type or protected type with entry (RM E.2.1(7)).

      Direct_Designated_Type := Designated_Type (T);

      if Ekind (Direct_Designated_Type) = E_Class_Wide_Type then
         Error_Msg_N
           ("invalid access-to-class-wide type in shared passive unit", T);
         return;

      elsif Ekind (Direct_Designated_Type) in Task_Kind then
         Error_Msg_N
           ("invalid access-to-task type in shared passive unit", T);
         return;

      elsif Ekind (Direct_Designated_Type) in Protected_Kind
        and then Has_Entry_Declarations (Direct_Designated_Type)
      then
         Error_Msg_N
           ("invalid access-to-protected type in shared passive unit", T);
         return;
      end if;
   end Validate_SP_Access_Object_Type_Decl;

   ---------------------------------
   -- Validate_Static_Object_Name --
   ---------------------------------

   procedure Validate_Static_Object_Name (N : Node_Id) is

      function Is_Primary (N : Node_Id) return Boolean;
      --  Determine whether node is syntactically a primary in an expression.

      function Is_Primary (N : Node_Id) return Boolean is
         K : constant Node_Kind := Nkind (Parent (N));

      begin
         case K is

            when N_Op | N_In | N_Not_In =>
               return True;

            when N_Aggregate | N_Index_Or_Discriminant_Constraint =>
               return True;

            when N_Attribute_Reference =>
               return Attribute_Name (Parent (N)) /= Name_Address
                 and then Attribute_Name (Parent (N)) /= Name_Access
                 and then Attribute_Name (Parent (N)) /= Name_Unchecked_Access;

            when N_Indexed_Component =>
               return (N /= Prefix (Parent (N))
                 or else Is_Primary (Parent (N)));

            when N_Qualified_Expression =>
               return Is_Primary (Parent (N));

            when N_Assignment_Statement | N_Object_Declaration =>
               return (N = Expression (Parent (N)));

            when N_Selected_Component =>
               return Is_Primary (Parent (N));

            when others =>
               return False;
         end case;
      end Is_Primary;

   --  Start of processing for Validate_Static_Object_Name

   begin

      if not In_Preelaborated_Unit
        or else not Comes_From_Source (N)
        or else In_Subprogram_Unit
      then
         return;

      --  Filter out cases where primary is default in a component
      --  declaration, discriminant specification, or actual in a record
      --  type initialization call.

      --  Initialization call of internal types.

      elsif Nkind (Parent (N)) = N_Procedure_Call_Statement then

         if Present (Parent (Parent (N)))
           and then Nkind (Parent (Parent (N))) = N_Freeze_Entity
         then
            return;
         end if;

         if Nkind (Name (Parent (N))) = N_Identifier
           and then not Comes_From_Source (Entity (Name (Parent (N))))
         then
            return;
         end if;
      end if;

      --  Error if the name is a primary in an expression. The parent must not
      --  be an operator, or a selected component or an indexed component that
      --  is itself a primary. Entities that are actuals do not need to be
      --  checked, because the call itself will be diagnosed.

      if (Ekind (Entity (N)) = E_Variable
           or else
            (not Is_Static_Expression (N)
              and then Ekind (Entity (N)) = E_Constant))
      and then Is_Primary (N)
      then
         Error_Msg_N ("non-static object name in preelaborated unit", N);
      end if;
   end Validate_Static_Object_Name;

end Sem_Dist;
