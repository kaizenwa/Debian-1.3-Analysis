------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P_ D I S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.59 $                             --
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
with Exp_TSS;  use Exp_TSS;
with Fname;    use Fname;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Namet;    use Namet;
with Output;   use Output;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Dist; use Sem_Dist;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Sprint;   use Sprint;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Uname;    use Uname;

package body Exp_Dist is

   ------------------------------------------
   --  Global constants for external names --
   ------------------------------------------

   Stream_Name                        : Name_Id;
   Item_Name                          : Name_Id;
   Params_Name                        : Name_Id;
   Result_Name                        : Name_Id;
   RPC_Receiver_Name                  : Name_Id;
   Params_Stream_Type_Name            : Name_Id;
   Do_Rpc_Name                        : Name_Id;
   Do_Apc_Name                        : Name_Id;
   Exceptions_Name                    : Name_Id;
   Exception_Occurrence_Name          : Name_Id;
   Null_Occurrence_Name               : Name_Id;
   Reraise_Occurrence_Name            : Name_Id;
   Subprogram_Id_Name                 : Name_Id;
   Get_Active_Partition_Id_Name       : Name_Id;
   Get_RCI_Package_Receiver_Name      : Name_Id;
   Get_Local_Partition_Id_Name        : Name_Id;
   Get_Passive_Partition_Id_Name      : Name_Id;
   Register_Receiving_Stub_Name       : Name_Id;
   Root_Stream_Type_Name              : Name_Id;
   Stream_Element_Count_Name          : Name_Id;
   Partition_Interface_Name           : Name_Id;
   RCI_Cache_Name                     : Name_Id;
   RCI_Info_Name                      : Name_Id;

   -----------------------------------------
   -- Global constants for internal names --
   -----------------------------------------


   Stream_In_Name : Name_Id;
   --  Name for stream input

   Stream_Out_Name : Name_Id;
   --  Name for stream output

   Except_Name : Name_Id;
   --  Name for exception occurence

   Returned_Val_Name : Name_Id;
   --  Name for value returned by a function

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Racw_Stubs
     (Vis_Decl      : Node_Id;
      Pkg_Bdy_Decls : List_Id;
      Stubs_CItems  : List_Id;
      Last_Racw_Num : Int);
   --  Builds and adds the calling stubs bodies for the primitive operations
   --  of a racw type to the calling stubs package body declarations. Vis_Decl
   --  is the declaration node of the racw type.

   procedure Add_System_Rpc (C_Unit : Node_Id);
   --  Adds implicit with for system_rpc. Also appends system.rpc to the use
   --  clause.

   procedure Add_With_Clause
     (Nam    : in Node_Id;
      CItems : in out List_Id);
   --  Adds with clause for Nam to the specified context item list if not
   --  already present.

   procedure Append_Nat_To_String (S : String; V : Nat);
   --  Stores in the name buffer the result of the concatenation
   --  S & Nat'image (N)

   procedure Build_Calling_Stubs_Pkg_Body
     (Pkg_Decl       : in Node_Id;
      Last_Stub_Num  : in out Int;
      Last_Racw_Num  : in out Int;
      Last_Ras_Num   : in out Int;
      Stubs_CItems   : in List_Id;
      Stubs_Pkg_Body : out Node_Id);
   --  This procedure builds the calling stubs package body for a given
   --  package declaration. Last_Stub_Num is the number given to the last
   --  stub built. Last_Racw_Num is the number given to the last remote access
   --  to class wide encountered. Last_Ras_Num is the number given to the last
   --  remote-access-to-subprogram type encountered. Pkg_Decl is the
   --  declaration node of a RCI package or the node of a package declaration
   --  appearing in the visible part of a RCI package declaration.

   procedure Build_Receiving_Stubs_Pkg_Body
     (Unit_Node      : in Node_Id;
      Last_Racw_Num  : in out Int;
      Last_Ras_Num   : in out Int;
      Stubs_CItems   : in List_Id;
      Stubs_Pkg_Body : out Node_Id);
   --  Builds the receiving stubs package body unit node. Unit_Node
   --  is either the declaration node of a RCI package which requires no body
   --  or a RCI package body node. Last_Racw_Num is the number given to the
   --  last remote access to class wide encountered. Last_Ras_Num is the
   --  number given to the last remote-access-to-subprogram type encountered.

   --  functions used to build the most used external name nodes.

   function Ada_Streams (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component ada.streams

   function Ada_Exceptions (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component ada.exceptions

   function System_PInterface (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.partition_interface

   function System_Rpc (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.rpc

   function SP_RCI_Info (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.partition_interface.-
   --  rci_info

   function SP_Subprogram_Id (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.partition_interface.-
   --  subprogram_id

   function SP_Get_Local_Partition_Id (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.partition_interface.-
   --  get_local_partition_id

   function SP_Register_Receiving_Stub
    (Loc  : Source_Ptr)
     return Node_Id;
   --  returns the selected component system.partition_interface.-
   --  register_server_elaboration

   function SR_Partition_ID (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.rpc.partition_id

   function SR_RPC_Receiver (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.rpc.rpc_receiver

   function SR_Params_Stream_Type (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.rpc.params_stream_type

   function SR_Do_Rpc (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.rpc.do_rpc

   function SR_Do_Apc (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component system.rpc.do_apc;

   function RC_Get_Active_Partition_Id (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component rci_cache.get_active_partition_id

   function RC_Get_RCI_Package_Receiver (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component rci_cache.get_rci_package_receiver

   function AE_Exception_Occurrence (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component ada.exceptions.exception_occurrence

   function AE_Null_Occurrence (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component ada.exceptions.null_occurrence

   function AE_Reraise_Occurrence (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component ada.exceptions.reraise_occurrence

   function AS_Root_Stream_Type (Loc : Source_Ptr) return Node_Id;
   --  returns the selected component ada.streams.root_stream_type

   function Full_Etype_Name (N : Node_Id; Ref : Node_Id) return Node_Id;
   --  If N has an Etype whose enclosing library unit is the same as that of
   --  Ref then returns the full name of the Etype of N. Otherwise return N.

   function Build_Parent_Full_Name (P : Node_Id)  return Node_Id;
   --  Build prefix of child unit name. Recurse if needed.

   function Build_Unit_Full_Name (U : Node_Id) return Node_Id;
   --  If the unit is a child unit, build name with all ancestors. otherwise,
   --  returns a new reference to the unit name.

   function Full_Qualified_Name_Node (E : Entity_Id) return Node_Id;
   --  returns a node with the full qualified name of the entity.

   function Get_Name_Id (Name : String) return Name_Id;
   --  Returns the corresponding Name_Id for a given string;

   function Get_Pkg_Name_String_Id (Decl_Node : Node_Id) return String_Id;
   --  Gets the full name of a package. Its parameter is the declaration
   --  node of the package.

   function Get_String_Id (Val : String) return String_Id;
   --  Returns the corresponding String_Id for a given string

   function Has_Pragma_RCI (L : List_Id) return Boolean;
   --  Return true if L contains a pragma Remote_Call_Interface node.

   function Has_Unknown_Size (E : Entity_Id) return Boolean;
   --  returns true if the type E is unconstrained or has unknown
   --  discriminants.

   function Is_RCI_Pkg_Decl_Cunit (Cunit : Node_Id) return Boolean;
   --  Returns true if the unit of Cunit is an RCI package declaration;
   --  the parameter is supposed to be a compilation unit node;

   function AStub_Param_Specs (Loc : Source_Ptr) return List_Id;
   --  returns a new parameter specification list for an asynchronous
   --  receiving stub.

   function NStub_Param_Specs (Loc : Source_Ptr) return List_Id;
   --  returns a new parameter specification list for a normal receiving stub

   function Racw_Read_Spec
     (Loc       : Source_Ptr;
      Racw_Type : Entity_Id)
      return      Node_Id;
   --  Builds a read operation specification for a given racw entity.

   function Racw_Write_Spec
     (Loc       : Source_Ptr;
      Racw_Type : Entity_Id)
      return       Node_Id;
   --  Builds a write operation specification for a given racw entity.

   function Ras_Input_Spec
     (Loc      : Source_Ptr;
      Ras_Type : Entity_Id)
      return      Node_Id;
   --  Builds an Input operation specification for a given ras type entity.

   function Ras_Output_Spec
     (Loc      : Source_Ptr;
      Ras_Type : Entity_Id)
      return       Node_Id;
   --  Builds an Output operation specification for a given ras type entity.

   function Ras_Read_Spec
     (Loc      : Source_Ptr;
      Ras_Type : Entity_Id)
      return      Node_Id;
   --  Builds a read operation specification for a given ras type entity.

   function Ras_Write_Spec
     (Loc      : Source_Ptr;
      Ras_Type : Entity_Id)
      return       Node_Id;
   --  Builds a write operation specification for a given ras type entity.

   function Ras_Proc_Spec
     (Loc       : Source_Ptr;
      Ras_Type  : Entity_Id;
      Proc_Name : Name_Id)
      return      Node_Id;
   --  Common processing for the previous three procedures.

   ------------------
   --  Add_Racw_RW --
   ------------------

   procedure Add_Racw_RW (N : Node_Id) is
      Loc       : Source_Ptr := Sloc (N);
      Racw_Type : Entity_Id  := Defining_Identifier (N);
      L         : List_Id    := New_List;

   begin
      --  Initialize names. This is important when we are in stubs
      --  compilation mode and one of the unit withed by the
      --  stubs package body also withes the RCI unit beeing compiled.
      --  Should be generalized ???

      Init_Names;

      --  Append read procedure spec

      Append_To (L,
        Make_Subprogram_Declaration (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Make_Defining_Identifier (Loc, Name_Read),
              Parameter_Specifications => New_List (

                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Stream_Name),
                  Parameter_Type =>
                    Make_Access_Definition (Loc,
                      Subtype_Mark =>
                        Make_Attribute_Reference (Loc,
                          Prefix =>
                            New_Reference_To (RTE (RE_Root_Stream_Type), Loc),
                          Attribute_Name => Name_Class))),

                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Item_Name),
                  Out_Present => True,
                  Parameter_Type =>
                    New_Reference_To (Racw_Type, Loc))))));

      --  Append read attribute representation clause

      Append_To (L,
        Make_Attribute_Definition_Clause (Loc,
          Name => New_Reference_To (Racw_Type, Loc),
          Chars => Name_Read,
          Expression => Make_Identifier (Loc, Name_Read)));

      --  Append write procedure spec

      Append_To (L,
        Make_Subprogram_Declaration (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Make_Defining_Identifier (Loc, Name_Write),
              Parameter_Specifications => New_List (

                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Stream_Name),
                  Parameter_Type =>
                    Make_Access_Definition (Loc,
                      Subtype_Mark =>
                        Make_Attribute_Reference (Loc,
                          Prefix =>
                            New_Reference_To (RTE (RE_Root_Stream_Type), Loc),
                          Attribute_Name => Name_Class))),

                Make_Parameter_Specification (Loc,
                  Defining_Identifier =>
                    Make_Defining_Identifier (Loc, Item_Name),
                  Parameter_Type =>
                    New_Reference_To (Racw_Type, Loc))))));

      --  Append write attribute representation clause

      Append_To (L,
        Make_Attribute_Definition_Clause (Loc,
          Name => New_Reference_To (Racw_Type, Loc),
          Chars => Name_Write,
          Expression => Make_Identifier (Loc, Name_Write)));

      --  Insert newly built nodes in the tree

      Insert_List_After (N, L);

   end Add_Racw_RW;

   --------------------
   -- Add_Racw_Stubs --
   --------------------

   procedure Add_Racw_Stubs
     (Vis_Decl      : Node_Id;
      Pkg_bdy_Decls : List_Id;
      Stubs_CItems  : List_Id;
      Last_Racw_Num : Int)
   is
      --  Information needed from the input declaration

      Loc                : Source_Ptr := Sloc (Vis_Decl);
      Racw_Type          : Entity_Id  := Defining_Identifier (Vis_Decl);
      Root_Type          : Entity_Id  := Etype (Designated_Type (
                                                Racw_Type));
      Root_Type_Decl     : Node_Id := Parent (Root_Type);
      Root_Pkg_Spec      : Node_Id := Parent (Root_Type_Decl);
      Root_Pkg_Decl      : Node_Id := Parent (Root_Pkg_Spec);
      Root_Pkg           : Node_Id := Defining_Unit_Name (Root_Pkg_Spec);
      Root_Pkg_Vis_Decls : List_Id := Visible_Declarations (Root_Pkg_Spec);
      Root_Pkg_CItems    : List_Id := Context_Items (
                                        Enclosing_Lib_Unit_Node (
                                          Root_Pkg_Decl));

      CItems : List_Id := New_List;
      CItem  : Node_Id;
      Async  : Boolean := Is_Asynchronous (Racw_Type);
      Decl   : Node_Id;

      --  List of local names needed

      Receiving_Stub_Name : constant Name_Id := Get_Name_Id ("receiving");
      Origin_Name         : constant Name_Id := Get_Name_Id ("origin");
      Receiver_Name       : constant Name_Id := Get_Name_Id ("receiver");
      Addr_Name           : constant Name_Id := Get_Name_Id ("addr");
      P_Name              : constant Name_Id := New_Internal_Name ('P');
      F_Name              : constant Name_Id := New_Internal_Name ('F');
      R_Name              : constant Name_Id := New_Internal_Name ('R');
      A_Name              : constant Name_Id := New_Internal_Name ('A');
      V_Name              : constant Name_Id := New_Internal_Name ('V');

      Object_Stub_Name        : constant Name_Id :=
                                  New_External_Name (
                                    Related_Id   =>
                                      Get_Name_Id ("object_stub"),
                                    Suffix       => 'T',
                                    Suffix_Index => Last_Racw_Num);

      Object_Stub_Access_Name : constant Name_Id :=
                                  New_External_Name (
                                    Related_Id   =>
                                      Get_Name_Id ("object_stub_access"),
                                  Suffix       => 'T',
                                  Suffix_Index => Last_Racw_Num);

      --  Variable for object stub and access to object stub declarations

      Obj_Stub_Decl        : Node_Id;
      Acc_To_Obj_Stub_Decl : Node_Id;

      --  Variable for primitive operation stubs

      Racw_CStub_Spec : Node_Id;
      Racw_CStub_Body : Node_Id;
      Racw_RStub_Spec : Node_Id;
      Racw_RStub_Body : Node_Id;

      --  Features for the dispatching receiver

      Dispatcher_Name  : Name_Id;
      Dispatcher_Spec  : Node_Id;
      Dispatcher_Body  : Node_Id;
      Param_Assocs     : List_Id := New_List;
      Dispatcher_Decls : List_Id := New_List;
      Case_Stmt_Alts   : List_Id := New_List;
      Prim_Op_Num_Name : Name_Id := New_Internal_Name ('N');

      --  Number used to identify a primitive operation of Root_Type.

      Prim_Op_Num : Int := 0;

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Build_Async_Calling_Stub_Body
        (Vis_Decl : Node_Id)
         return  Node_Id;
      --  Builds the body of the calling stub for an asynchronous racw
      --  procedure

      function Build_Async_Receiving_Stub_Body
        (Vis_Decl : Node_Id)
         return     Node_Id;
      --  Builds the body node of the receiving stub for an asynchronous
      --  Racw procedure

      function Build_Calling_Stub_Body (Vis_Decl : Node_Id) return Node_Id;
      --  Builds the body of the calling stub for a primitive operation of
      --  a racw type.

      function Build_Receiving_Stub_Body
        (Vis_Decl : Node_Id)
         return     Node_Id;
      --  Builds the body node of the receiving stub for a regular Racw
      --  subprogram.

      function Find_Disp_Param_Spec (L : List_Id) return Node_Id;
      --  Scans the parameter specification list L and returns the first
      --  parameter specification node whose parameter type is the Racw
      --  type. Returns Empty if the list contains no such parameter type.

      function Has_Access_To_Root_Type (L : List_Id) return Boolean;
      --  Returns true if at list one of the parameter types in the
      --  parameter specification list L is an access definition to
      --  Root type.

      function Is_Disp_Param_Spec (Param_Spec : Node_Id) return Boolean;
      --  Returns true if the parameter specification is an access
      --  definition to the root type.

      -----------------------------------
      -- Build_Async_Calling_Stub_Body --
      -----------------------------------

      function Build_Async_Calling_Stub_Body
        (Vis_Decl : Node_Id)
         return     Node_Id
      is
         --  Information needed from the input parameter

         Subp_Spec       : Node_Id := Specification (Vis_Decl);
         Param_Specs     : List_Id := Parameter_Specifications (Subp_Spec);
         Subp_Name       : Node_Id := Defining_Unit_Name (Subp_Spec);
         Disp_Param_Spec : Node_Id := Find_Disp_Param_Spec (Param_Specs);
         Param_Spec      : Node_Id;
         Param_Type      : Node_Id;

         --  Building new entities for the local identifiers

         Stream_In   : Entity_Id;
         Object_Stub : Entity_Id;
         Origin      : Entity_Id;
         Receiver    : Entity_Id;
         Addr        : Entity_Id;

         --  Features for the stub body to create

         Stmts            : List_Id := New_List;
         Write_Stmts      : List_Id := New_List;
         Decls            : List_Id := New_List;
         Stub_Param_Specs : List_Id := New_List;
         Disp_Params      : List_Id := New_List;
         Stub_Param_Spec  : Node_Id;
         First_Disp_Param : Node_Id;
         Next_Disp_Param  : Node_Id;
         Cont_Arg_Test    : Node_Id;
         Stub_Spec        : Node_Id;
         Stub_Body        : Node_Id;
         Stream_Decl      : Node_Id;

      begin
         --  Initialization of the local entities

         Stream_In   :=
           Make_Defining_Identifier (Loc, Stream_In_Name);

         Object_Stub :=
           Make_Defining_Identifier (Loc, Object_Stub_Name);

         Origin      :=
           Make_Defining_Identifier (Loc, Origin_Name);

         Receiver    :=
           Make_Defining_Identifier (Loc, Receiver_Name);

         Addr        :=
           Make_Defining_Identifier (Loc, Addr_Name);

         --  Build and append stream input declaration to the list of
         --  declarations of the stub body

         Stream_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Stream_In,
             Object_Definition =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => SR_Params_Stream_Type (Loc),

               Constraint =>
                 Make_Index_Or_Discriminant_Constraint (Loc,
                   Constraints =>
                     New_List (Make_Integer_Literal (Loc, Uint_0)))));

         Set_Aliased_Present (Stream_Decl);
         Append (Stream_Decl, Decls);

         --  Build and append the write statement for the Receiver, to the
         --  list of statements of the stub body

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => SR_RPC_Receiver (Loc),
             Attribute_Name => Name_Write,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               Make_Selected_Component (Loc,
                 Prefix =>
                   Make_Identifier (Loc,
                     Chars => Chars (
                       Defining_Identifier (Disp_Param_Spec))),
                 Selector_Name => New_Reference_To (Receiver, Loc)))));

         --  Write statement for the subprogram identifier

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => SP_Subprogram_Id (Loc),
             Attribute_Name => Name_Write,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               --  Type conversion necessary ???

               --  Make_Type_Conversion (Loc,
                  --  SP_Subprogram_Id (loc)),

               Make_Integer_Literal (Loc, UI_From_Int (Prim_Op_Num)))));

         --  Append the write statements for the in parameters

         if Param_Specs /= No_List then
            Param_Spec := First (Param_Specs);
            while Present (Param_Spec) loop

               if Is_Disp_Param_Spec (Param_Spec) then

                  Append_To (Disp_Params,
                    Make_Identifier (Loc,
                      Chars =>
                        Chars (Defining_Identifier (Param_Spec))));

                  Append_To (Stub_Param_Specs,
                    Make_Parameter_Specification (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc,
                          Chars =>
                            Chars (Defining_Identifier (Param_Spec))),

                      Parameter_Type =>
                        Make_Access_Definition (Loc,
                          Subtype_Mark =>
                            New_Reference_To (Object_Stub, Loc)),

                      Expression =>
                        Copy_Original_Tree (Expression (Param_Spec))));

                  Append_To (Write_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix => Make_Identifier (Loc, Name_System),
                          Selector_Name =>
                            Make_Identifier (Loc, Name_Address)),

                      Attribute_Name => Name_Write,

                      Expressions =>
                        New_List (
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Reference_To (Stream_In, Loc),
                            Attribute_Name => Name_Unchecked_Access),

                          Make_Selected_Component (Loc,
                            Prefix =>
                              Make_Identifier (Loc,
                                Chars => Chars (
                                  Defining_Identifier (Param_Spec))),

                            Selector_Name =>
                              Make_Identifier (Loc, Addr_Name)))));

               else
                  Param_Type :=
                    Full_Etype_Name
                      (Parameter_Type (Param_Spec),
                       Vis_Decl);
                  Stub_Param_Spec := Copy_Original_Tree (Param_Spec);
                  Set_Parameter_Type (Stub_Param_Spec, Param_Type);
                  Append_To (Stub_Param_Specs, Stub_Param_Spec);

                  if Has_Unknown_Size (Etype
                    (Parameter_Type (Param_Spec)))
                  then
                     Append_To (Stmts,
                       Make_Attribute_Reference (Loc,
                         Prefix => Copy_Original_Tree (Param_Type),
                         Attribute_Name => Name_Output,

                         Expressions =>
                           New_List (
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Reference_To (Stream_In, Loc),
                               Attribute_Name => Name_Unchecked_Access),
                             Make_Identifier (Loc,
                               Chars => Chars (
                                 Defining_Identifier (Param_Spec))))));

                  else
                     Append_To (Write_Stmts,
                       Make_Attribute_Reference (Loc,
                         Prefix => Copy_Original_Tree (Param_Type),
                         Attribute_Name => Name_Write,

                         Expressions =>
                           New_List (
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Reference_To (Stream_In, Loc),
                               Attribute_Name => Name_Unchecked_Access),
                             Make_Identifier (Loc,
                               Chars => Chars (
                                 Defining_Identifier (Param_Spec))))));
                  end if;
               end if;

               Param_Spec := Next (Param_Spec);
            end loop;
         end if;

         Append_List (Write_Stmts, Stmts);

         --  Append Do_Apc call to the list of statements

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => SR_Do_Apc (Loc),
             Parameter_Associations => New_List (
               Make_Selected_Component (Loc,
                 Prefix =>
                   Make_Identifier (Loc,
                     Chars => Chars (
                       Defining_Identifier (Disp_Param_Spec))),
                 Selector_Name => New_Reference_To (Origin, Loc)),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access))));

         --  Build and insert controlling argument tests

         if Disp_Params /= No_List then

            First_Disp_Param := First (Disp_Params);

            if Present (First_Disp_Param)
              and then Present (Next (First_Disp_Param))
            then
               Next_Disp_Param := Next (First_Disp_Param);

               Cont_Arg_Test :=
                 Make_Op_Eq (Loc,

                   Left_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix =>
                         Make_Identifier (Loc,
                           Chars => Chars (First_Disp_Param)),
                       Selector_Name => New_Reference_To (Origin, Loc)),

                   Right_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix =>
                         Make_Identifier (Loc,
                           Chars => Chars (Next_Disp_Param)),
                       Selector_Name => New_Reference_To (Origin, Loc)));

               Next_Disp_Param := Next (Next_Disp_Param);

               while Present (Next_Disp_Param) loop

                  Cont_Arg_Test :=
                    Make_Op_And (Loc,
                      Left_Opnd => Cont_Arg_Test,

                      Right_Opnd =>
                        Make_Op_Eq (Loc,

                          Left_Opnd =>
                            Make_Selected_Component (Loc,
                              Prefix =>
                                Make_Identifier (Loc,
                                  Chars => Chars (First_Disp_Param)),
                              Selector_Name =>
                                New_Reference_To (Origin, Loc)),

                          Right_Opnd =>
                            Make_Selected_Component (Loc,
                              Prefix =>
                                Make_Identifier (Loc,
                                  Chars => Chars (Next_Disp_Param)),
                              Selector_Name =>
                                New_Reference_To (Origin, Loc))));

                  Next_Disp_Param := Next (Next_Disp_Param);
               end loop;

               Stmts := New_List (
                 Make_If_Statement (Loc,
                   Condition => Cont_Arg_Test,
                   Then_Statements => Stmts,
                   Else_Statements => New_List (
                     Make_Raise_Statement (Loc,
                       Name =>
                         New_Occurrence_Of
                           (Standard_Constraint_Error, Loc)))));
            end if;
         end if;

         --  Build the stub specification node

         Stub_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name =>
               Copy_Original_Tree (Defining_Unit_Name (Subp_Spec)),
             Parameter_Specifications => Stub_Param_Specs);

         --  Build the stub body node

         Stub_Body :=
           Make_Subprogram_Body (Loc,
             Specification => Stub_Spec,
             Declarations => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Stmts));

         return Stub_Body;

      end Build_Async_Calling_Stub_Body;

      -------------------------------------
      -- Build_Async_Receiving_Stub_Body --
      -------------------------------------

      function Build_Async_Receiving_Stub_Body
        (Vis_Decl : Node_Id)
         return     Node_Id
      is
         --  Information needed from the input declaration

         Subp_Spec   : Node_Id := Specification (Vis_Decl);
         Param_Specs : List_Id := Parameter_Specifications (Subp_Spec);
         Subp_Name   : Node_Id := Defining_Unit_Name (Subp_Spec);
         Param_Spec  : Node_Id;
         Param       : Entity_Id;
         Param_Type  : Node_Id;

         --  New entities for the local identifiers

         Params      : Entity_Id;

         --  Features for the stub body to create

         Decls             : List_Id := New_List;
         Stmts             : List_Id := New_List;
         Hss               : Node_Id;
         Stub_Spec         : Node_Id;
         Stub_Body         : Node_Id;
         Param_List        : List_Id := New_List;
         Param_Read_Stmts  : List_Id := New_List;

      begin
         --  Initialization of the external entities

         Params :=
           Make_Defining_Identifier (Loc, Params_Name);

         --  Build the stub specification node

         Stub_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name =>
               Make_Identifier (Loc,
                 Chars =>
                   New_External_Name (
                     Related_Id   => Receiving_Stub_Name,
                     Suffix       => 'S',
                     Suffix_Index => Prim_Op_Num)),

             Parameter_Specifications => AStub_Param_Specs (Loc));

         --  Build the stub body node

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Identifier (Loc, Addr_Name),
             Object_Definition =>
               Make_Selected_Component (Loc,
                 Prefix => Make_Identifier (Loc, Name_System),
                 Selector_Name => Make_Identifier (Loc, Name_Address))));

         if Param_Specs /= No_List then
            Param_Spec := First (Param_Specs);
            while Present (Param_Spec) loop

               Param_Type := Parameter_Type (Param_Spec);
               Param :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
               Append (Param, Param_List);

               if Is_Disp_Param_Spec (Param_Spec) then

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        New_Reference_To (Param, Loc),
                      Object_Definition =>
                        Make_Identifier (Loc, Chars (Racw_Type))));

                  Append_To (Param_Read_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix => Make_Identifier (Loc, Name_System),
                          Selector_Name =>
                            Make_Identifier (Loc, Name_Address)),
                      Attribute_Name => Name_Read,

                      Expressions =>
                        New_List (
                          Make_Identifier (Loc, Params_Name),
                          Make_Identifier (Loc, Addr_Name))));

                  Append_To (Param_Read_Stmts,
                    Make_Assignment_Statement (Loc,
                      Name => Make_Identifier (Loc, Chars (Param)),
                      Expression =>
                        Make_Function_Call (Loc,
                          Name => Make_Identifier (Loc, F_Name),
                          Parameter_Associations => New_List (
                            Make_Identifier (Loc, Addr_Name)))));

               elsif Has_Unknown_Size (Etype (Param_Type)) then

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        New_Reference_To (Param, Loc),
                      Object_Definition =>
                        Full_Etype_Name (Param_Type, Vis_Decl),

                      Expression =>
                        Make_Function_Call (Loc,
                          Name => Make_Attribute_Reference (Loc,
                            Prefix =>
                              Full_Etype_Name (Param_Type, Vis_Decl),
                            Attribute_Name => Name_Input),

                          Parameter_Associations =>
                            New_List (
                              Make_Identifier (Loc, Params_Name)))));

               else
                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        New_Reference_To (Param, Loc),
                      Object_Definition =>
                        Full_Etype_Name (Param_Type, Vis_Decl)));

                  Append_To (Param_Read_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix => Copy_Original_Tree (Param_Type),
                      Attribute_Name => Name_Read,

                      Expressions =>
                        New_List (
                          Make_Identifier (Loc, Chars (Params)),
                          Make_Identifier (Loc, Chars (Param)))));
               end if;

               Param_Spec := Next (Param_Spec);
            end loop;
         end if;

         Stmts := Param_Read_Stmts;

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               Make_Selected_Component (Loc,
                 Prefix => Build_Unit_Full_Name (Root_Pkg_Decl),
                 Selector_Name => Copy_Original_Tree (Subp_Name)),
             Parameter_Associations => Param_List));

         Hss :=
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => Stmts,
             Exception_Handlers => New_List (
               Make_Exception_Handler (Loc,
                 Exception_Choices => New_List (Make_Others_Choice (Loc)),
                 Statements => New_List (Make_Null_Statement (Loc)))));

         Stub_Body :=
           Make_Subprogram_Body (Loc,
             Specification => Stub_Spec,
             Declarations  => Decls,
             Handled_Statement_Sequence => Hss);

         return Stub_Body;
      end Build_Async_Receiving_Stub_Body;

      -------------------------------
      -- Build_Receiving_Stub_Body --
      -------------------------------

      function Build_Receiving_Stub_Body
        (Vis_Decl : Node_Id)
         return     Node_Id
      is
         --  Information needed from the input declaration

         Subp_Spec   : Node_Id := Specification (Vis_Decl);
         Param_Specs : List_Id := Parameter_Specifications (Subp_Spec);
         Subp_Name   : Node_Id := Defining_Unit_Name (Subp_Spec);
         Param_Spec  : Node_Id;
         Param       : Entity_Id;
         Param_Type  : Node_Id;

         --  New entities for the local identifiers

         Params        : Entity_Id;
         Result        : Entity_Id;
         Returned_Val  : Entity_Id;
         Except        : Entity_Id;

         --  Features for the stub body to create

         Decls             : List_Id := New_List;
         Stmts             : List_Id := New_List;
         Hss               : Node_Id;
         Stub_Spec         : Node_Id;
         Stub_Body         : Node_Id;
         Param_List        : List_Id := New_List;
         Param_Read_Stmts  : List_Id := New_List;
         Param_Write_Stmts : List_Id := New_List;

      begin
         --  Initialization of the local entities

         Params := Make_Defining_Identifier (Loc, Params_Name);
         Result := Make_Defining_Identifier (Loc, Result_Name);
         Returned_Val := Make_Defining_Identifier (Loc, Returned_Val_Name);
         Except := Make_Defining_Identifier (Loc, Except_Name);

         --  Build the stub specification node

         Stub_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name =>
               Make_Identifier (Loc,
                 Chars =>
                   New_External_Name (
                     Related_Id => Receiving_Stub_Name,
                     Suffix       => 'S',
                     Suffix_Index => Prim_Op_Num)),
             Parameter_Specifications => NStub_Param_Specs (Loc));

         --  Build the stub body node

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Identifier (Loc, Addr_Name),
             Object_Definition =>
               Make_Selected_Component (Loc,
                 Prefix => Make_Identifier (Loc, Name_System),
                 Selector_Name => Make_Identifier (Loc, Name_Address))));

         if Param_Specs /= No_List then
            Param_Spec := First (Param_Specs);

            while Present (Param_Spec) loop
               Param :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
               Param_Type := Parameter_Type (Param_Spec);

               Append (Param, Param_List);

               if Is_Disp_Param_Spec (Param_Spec) then

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        New_Reference_To (Param, Loc),
                      Object_Definition   =>
                        Make_Identifier (Loc, Chars (Racw_Type))));

                  Append_To (Param_Read_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix => Make_Identifier (Loc, Name_System),
                           Selector_Name =>
                             Make_Identifier (Loc, Name_Address)),
                      Attribute_Name => Name_Read,

                      Expressions =>
                        New_List (
                          Make_Identifier (Loc, Params_Name),
                          Make_Identifier (Loc, Addr_Name))));

                  Append_To (Param_Read_Stmts,
                    Make_Assignment_Statement (Loc,
                      Name => Make_Identifier (Loc, Chars (Param)),
                      Expression =>
                        Make_Function_Call (Loc,
                          Name => Make_Identifier (Loc, F_Name),
                          Parameter_Associations => New_List (
                            Make_Identifier (Loc, Addr_Name)))));

               else
                  if Has_Unknown_Size (Etype (Param_Type)) then

                     Append_To (Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier =>
                           New_Reference_To (Param, Loc),

                         Object_Definition =>
                           Full_Etype_Name (Param_Type, Vis_Decl),

                         Expression =>
                           Make_Function_Call (Loc,
                             Name =>
                               Make_Attribute_Reference (Loc,
                                 Prefix =>
                                   Full_Etype_Name (Param_Type, Vis_Decl),
                                 Attribute_Name => Name_Input),
                             Parameter_Associations =>
                               New_List (
                                 Make_Identifier (Loc, Params_Name)))));

                  else
                     Append_To (Decls,
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => New_Reference_To (Param, Loc),
                         Object_Definition =>
                           Full_Etype_Name (Param_Type, Vis_Decl)));

                     if In_Present (Param_Spec) or else
                       not Out_Present (Param_Spec)
                     then

                        Append_To (Param_Read_Stmts,
                          Make_Attribute_Reference (Loc,
                            Prefix =>
                              Full_Etype_Name (Param_Type, Vis_Decl),
                            Attribute_Name => Name_Read,

                            Expressions =>
                              New_List (
                                Make_Identifier (Loc, Chars (Params)),
                                Make_Identifier (Loc, Chars (Param)))));
                     end if;

                  end if;

                  if Out_Present (Param_Spec) then

                     if Has_Unknown_Size (Etype (Param_Type)) then

                        Append_To (Param_Write_Stmts,
                          Make_Attribute_Reference (Loc,
                            Prefix =>
                              Full_Etype_Name (Param_Type, Vis_Decl),
                            Attribute_Name => Name_Output,

                            Expressions =>
                              New_List (
                                Make_Identifier (Loc, Chars (Result)),
                                Make_Identifier (Loc, Chars (Param)))));

                     else
                        Append_To (Param_Write_Stmts,
                          Make_Attribute_Reference (Loc,
                            Prefix =>
                              Full_Etype_Name (Param_Type, Vis_Decl),
                            Attribute_Name => Name_Write,

                            Expressions =>
                              New_List (
                                Make_Identifier (Loc, Chars (Result)),
                                Make_Identifier (Loc, Chars (Param)))));
                     end if;
                  end if;
               end if;

               Param_Spec := Next (Param_Spec);
            end loop;
         end if;

         --  Add the declaration for the value returned by a function

         if Nkind (Subp_Spec) = N_Function_Specification
           and then not Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec)))
         then
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Returned_Val,
                Object_Definition =>
                  Full_Etype_Name (Subtype_Mark (Subp_Spec), Vis_Decl)));
         end if;

         Stmts := Param_Read_Stmts;

         if Nkind (Subp_Spec) = N_Function_Specification then

            if Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec))) then

               Append_To (Stmts,

                 Make_Block_Statement (Loc,
                   Declarations => New_List (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => Returned_Val,
                       Object_Definition =>
                         Full_Etype_Name (Subtype_Mark (Subp_Spec), Vis_Decl),

                       Expression =>
                         Make_Function_Call (Loc,
                           Name =>
                             Make_Selected_Component (Loc,
                               Prefix =>
                                 Build_Unit_Full_Name (Root_Pkg_Decl),
                               Selector_Name =>
                                 Copy_Original_Tree (Subp_Name)),
                           Parameter_Associations => Param_List))),

                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (
                         Make_Attribute_Reference (Loc,
                           Prefix => AE_Exception_Occurrence (Loc),
                           Attribute_Name => Name_Write,

                           Expressions => New_List (
                             New_Reference_To (Result, Loc),
                             AE_Null_Occurrence (Loc))),

                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             Full_Etype_Name
                               (Subtype_Mark (Subp_Spec),
                                Vis_Decl),
                           Attribute_Name => Name_Output,

                           Expressions =>
                             New_List (
                               Make_Identifier (Loc, Chars (Result)),
                               Make_Identifier (Loc,
                                 Chars => Chars (Returned_Val))))))));

            else
               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name => Make_Identifier (Loc, Chars (Returned_Val)),
                   Expression =>
                     Make_Function_Call (Loc,
                       Name =>
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Build_Unit_Full_Name (Root_Pkg_Decl),
                           Selector_Name => Copy_Original_Tree (Subp_Name)),
                       Parameter_Associations => Param_List)));

               Append_To (Stmts,
                 Make_Attribute_Reference (Loc,
                   Prefix => AE_Exception_Occurrence (Loc),
                   Attribute_Name => Name_Write,

                   Expressions => New_List (
                     New_Reference_To (Result, Loc),
                     AE_Null_Occurrence (Loc))));
            end if;

         else
            Append_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix => Build_Unit_Full_Name (Root_Pkg_Decl),
                    Selector_Name => Copy_Original_Tree (Subp_Name)),
                Parameter_Associations => Param_List));

            Append_To (Stmts,
              Make_Attribute_Reference (Loc,
                Prefix => AE_Exception_Occurrence (Loc),
                Attribute_Name => Name_Write,

                Expressions => New_List (
                  New_Reference_To (Result, Loc),
                  AE_Null_Occurrence (Loc))));
         end if;

         Append_List (Param_Write_Stmts, Stmts);

         if Nkind (Subp_Spec) = N_Function_Specification
           and then not Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec)))
         then
            Append_To (Stmts,
              Make_Attribute_Reference (Loc,
                Prefix =>
                  Full_Etype_Name
                    (Subtype_Mark (Subp_Spec),
                     Vis_Decl),
                Attribute_Name => Name_Write,

                Expressions =>
                  New_List (
                    Make_Identifier (Loc, Chars (Result)),
                    Make_Identifier (Loc, Chars (Returned_Val)))));
         end if;

         Hss :=
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => Stmts,
             Exception_Handlers => New_List (
               Make_Exception_Handler (Loc,
                 Choice_Parameter => Make_Identifier (Loc, Chars (Except)),
                 Exception_Choices => New_List (Make_Others_Choice (Loc)),

                 Statements => New_List (
                   Make_Attribute_Reference (Loc,
                     Prefix => AE_Exception_Occurrence (Loc),
                     Attribute_Name => Name_Write,

                     Expressions =>
                       New_List (
                         New_Reference_To (Result, Loc),
                         New_Reference_To (Except, Loc)))))));

         Stub_Body :=
           Make_Subprogram_Body (Loc,
             Specification => Copy_Original_Tree (Stub_Spec),
             Declarations => Decls,
             Handled_Statement_Sequence => Hss);

         return Stub_Body;
      end Build_Receiving_Stub_Body;

      -----------------------------
      -- Build_Calling_Stub_Body --
      -----------------------------

      function Build_Calling_Stub_Body
        (Vis_Decl : Node_Id)
         return     Node_Id
      is
         --  Information needed from the input parameter

         Subp_Spec       : Node_Id := Specification (Vis_Decl);
         Param_Specs     : List_Id := Parameter_Specifications (Subp_Spec);
         Subp_Name       : Node_Id := Defining_Unit_Name (Subp_Spec);
         Disp_Param_Spec : Node_Id := Find_Disp_Param_Spec (Param_Specs);
         Param_Spec      : Node_Id;
         Param_Type      : Node_Id;

         --  Building new entities for the local identifiers

         Stream_In            : Entity_Id;
         Stream_Out           : Entity_Id;
         Returned_Val         : Entity_Id;
         Except               : Entity_Id;
         Object_Stub          : Entity_Id;
         Origin               : Entity_Id;
         Receiver             : Entity_Id;
         Addr                 : Entity_Id;

         --  Features for the stub body to create

         Stmts            : List_Id := New_List;
         Write_Stmts      : List_Id := New_List;
         Output_Stmts     : List_Id := New_List;
         Decls            : List_Id := New_List;
         Stub_Param_Specs : List_Id := New_List;
         Disp_Params      : List_Id := New_List;
         Stub_Param_Spec  : Node_Id;
         First_Disp_Param : Node_Id;
         Next_Disp_Param  : Node_Id;
         Cont_Arg_Test    : Node_Id;
         Stub_Spec        : Node_Id;
         Stub_Body        : Node_Id;

         --  Variable for the declaration node of a stream

         Stream_Decl   : Node_Id;

      begin
         --  Initialization of the external entities

         Stream_In              :=
           Make_Defining_Identifier (Loc, Stream_In_Name);
         Stream_Out             :=
           Make_Defining_Identifier (Loc, Stream_Out_Name);
         Returned_Val           :=
           Make_Defining_Identifier (Loc, Returned_Val_Name);
         Except                 :=
           Make_Defining_Identifier (Loc, Except_Name);
         Object_Stub            :=
           Make_Defining_Identifier (Loc, Object_Stub_Name);
         Origin                 :=
           Make_Defining_Identifier (Loc, Origin_Name);
         Receiver               :=
           Make_Defining_Identifier (Loc, Receiver_Name);
         Addr                   :=
           Make_Defining_Identifier (Loc, Addr_Name);

         --  Build and append stream input declaration to the list of
         --  declarations of the stub body

         Stream_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Stream_In,
             Object_Definition =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => SR_Params_Stream_Type (Loc),

               Constraint =>
                 Make_Index_Or_Discriminant_Constraint (Loc,
                   Constraints =>
                     New_List (Make_Integer_Literal (Loc, Uint_0)))));

         Set_Aliased_Present (Stream_Decl);
         Append (Stream_Decl, Decls);

         --  Build and append stream output declaration to the list of
         --  declarations of the stub body

         Stream_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Stream_Out,

             Object_Definition =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => SR_Params_Stream_Type (Loc),

                 Constraint =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints =>
                       New_List (Make_Integer_Literal (Loc, Uint_0)))));

         Set_Aliased_Present (Stream_Decl);
         Append (Stream_Decl, Decls);

         --  Append the declaration for the exeption occurrence

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Except,
             Object_Definition => AE_Exception_Occurrence (Loc)));

         --  Append the declaration for the returned value in the
         --  case of a function

         if Nkind (Subp_Spec) = N_Function_Specification
           and then not Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec)))
         then
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Returned_Val,
                Object_Definition =>
                  Full_Etype_Name (Subtype_Mark (Subp_Spec), Vis_Decl)));
         end if;

         --  Build and append the write statement for the Receiver, to the
         --  list of statements of the stub body

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => SR_RPC_Receiver (Loc),
             Attribute_Name => Name_Write,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               Make_Selected_Component (Loc,
                 Prefix =>
                   New_Reference_To (
                     Defining_Identifier (Disp_Param_Spec),
                     Loc),
                 Selector_Name =>
                   New_Reference_To (Receiver, Loc)))));

         --  Write statement for the subprogram identifier

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => SP_Subprogram_Id (Loc),
             Attribute_Name => Name_Write,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               --  Type conversion necessary ???

               --  Make_Type_Conversion (Loc,
                  --  SP_Subprogram_Id (loc),

               Make_Integer_Literal (Loc, UI_From_Int (Prim_Op_Num)))));

         --  Append the write statements for the in parameters
         --  and the read statements for the out parameters

         if Param_Specs /= No_List then

            Param_Spec := First (Param_Specs);

            while Present (Param_Spec) loop

               if Is_Disp_Param_Spec (Param_Spec) then

                  Append_To (Disp_Params,
                    Make_Identifier (Loc,
                      Chars =>
                        Chars (Defining_Identifier (Param_Spec))));

                  Append_To (Stub_Param_Specs,
                    Make_Parameter_Specification (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc,
                          Chars =>
                            Chars (Defining_Identifier (Param_Spec))),

                      Parameter_Type =>
                        Make_Access_Definition (Loc,
                          Subtype_Mark =>
                            New_Reference_To (Object_Stub, Loc)),

                      Expression =>
                        Copy_Original_Tree (Expression (Param_Spec))));

                  --  we only add write operation since mode is "in"
                  --  as we have an access definition,

                  Append_To (Write_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix => Make_Identifier (Loc, Name_System),
                          Selector_Name =>
                            Make_Identifier (Loc, Name_Address)),
                      Attribute_Name => Name_Write,

                      Expressions =>
                        New_List (
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Reference_To (Stream_In, Loc),
                            Attribute_Name => Name_Unchecked_Access),

                          Make_Selected_Component (Loc,
                            Prefix =>
                              Make_Identifier (Loc,
                                Chars => Chars (
                                  Defining_Identifier (Param_Spec))),
                            Selector_Name =>
                              Make_Identifier (Loc, Addr_Name)))));

               else
                  Stub_Param_Spec := Copy_Original_Tree (Param_Spec);
                  Param_Type :=
                    Full_Etype_Name
                      (Parameter_Type (Param_Spec),
                       Vis_Decl);
                  Set_Parameter_Type (Stub_Param_Spec, Param_Type);
                  Append_To (Stub_Param_Specs, Stub_Param_Spec);

                  if Has_Unknown_Size (Etype
                    (Parameter_Type (Param_Spec)))
                  then
                     Append_To (Stmts,
                       Make_Attribute_Reference (Loc,
                         Prefix => Copy_Original_Tree (Param_Type),
                         Attribute_Name => Name_Output,

                         Expressions =>
                           New_List (
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Reference_To (Stream_In, Loc),
                               Attribute_Name => Name_Unchecked_Access),

                             Make_Identifier (Loc,
                               Chars => Chars (
                                 Defining_Identifier (Param_Spec))))));

                  elsif In_Present (Param_Spec)
                    or else not Out_Present (Param_Spec)
                  then
                     Append_To (Write_Stmts,
                       Make_Attribute_Reference (Loc,
                         Prefix => Copy_Original_Tree (Param_Type),
                         Attribute_Name => Name_Write,

                         Expressions =>
                           New_List (
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Reference_To (Stream_In, Loc),
                               Attribute_Name => Name_Unchecked_Access),
                             Make_Identifier (Loc,
                               Chars => Chars (
                                 Defining_Identifier (Param_Spec))))));
                  end if;

                  if Out_Present (Param_Spec) then

                     if Has_Unknown_Size (Etype (
                       Parameter_Type (Param_Spec)))
                     then
                        Append_To (Output_Stmts,
                          Make_Assignment_Statement (Loc,

                            Name =>
                              Make_Identifier (Loc,
                                Chars =>
                                  Chars (Defining_Identifier (Param_Spec))),

                            Expression =>
                              Make_Function_Call (Loc,
                                Name =>
                                  Make_Attribute_Reference (Loc,
                                    Prefix => Copy_Original_Tree (Param_Type),
                                    Attribute_Name => Name_Input),

                                Parameter_Associations => New_List (
                                  Make_Attribute_Reference (Loc,
                                    Prefix =>
                                      New_Reference_To (Stream_Out, Loc),
                                    Attribute_Name =>
                                      Name_Unchecked_Access)))));

                     else
                        Append_To (Output_Stmts,
                          Make_Attribute_Reference (Loc,
                            Prefix => Copy_Original_Tree (Param_Type),
                            Attribute_Name => Name_Read,

                            Expressions =>
                              New_List (
                                Make_Attribute_Reference (Loc,
                                  Prefix =>
                                    New_Reference_To (Stream_Out, Loc),
                                  Attribute_Name =>
                                    Name_Unchecked_Access),

                                Make_Identifier (Loc,
                                  Chars => Chars (
                                    Defining_Identifier (Param_Spec))))));
                     end if;
                  end if;
               end if;

               Param_Spec := Next (Param_Spec);
            end loop;
         end if;

         --  append the write statement list to the list of statements

         Append_List (Write_Stmts, Stmts);

         --  append do_rpc call to the list of statements

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => SR_Do_Rpc (Loc),
             Parameter_Associations => New_List (
               Make_Selected_Component (Loc,
                 Prefix =>
                   New_Reference_To (
                     Defining_Identifier (Disp_Param_Spec),
                     Loc),

                 Selector_Name =>
                   New_Reference_To (Origin, Loc)),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_Out, Loc),
                 Attribute_Name => Name_Unchecked_Access))));

         --  Append the read operation for the exception occurrence
         --  and the call to Reraise_Occurence

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => AE_Exception_Occurrence (Loc),
             Attribute_Name => Name_Read,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_Out, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               New_Reference_To (Except, Loc))));

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => AE_Reraise_Occurrence (Loc),
             Parameter_Associations => New_List (
               New_Reference_To (Except, Loc))));

         --  Append the read statement for the returned value, and
         --  the return statement in the case of a function

         if Nkind (Subp_Spec) = N_Function_Specification then

            if Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec))) then

               Append_To (Output_Stmts,
                 Make_Return_Statement (Loc,
                   Expression =>
                     Make_Function_Call (Loc,
                       Name =>
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             Full_Etype_Name
                               (Subtype_Mark (Subp_Spec),
                                Vis_Decl),
                           Attribute_Name => Name_Input),

                       Parameter_Associations => New_List (
                         Make_Attribute_Reference (Loc,
                           Prefix => New_Reference_To (Stream_Out, Loc),
                           Attribute_Name => Name_Unchecked_Access)))));

            else
               Append_To (Output_Stmts,
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     Full_Etype_Name
                       (Subtype_Mark (Subp_Spec),
                        Vis_Decl),
                   Attribute_Name => Name_Read,

                   Expressions => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix => New_Reference_To (Stream_Out, Loc),
                       Attribute_Name => Name_Unchecked_Access),

                     New_Reference_To (Returned_Val, Loc))));

               --  Append a return statement; the returned value is the one
               --  previously read from the stream output

               Append_To (Output_Stmts,
                 Make_Return_Statement (Loc,
                   Expression => New_Reference_To (Returned_Val, Loc)));
            end if;
         end if;

         --  Append the statements for the out parameters and
         --  the returned value in the case of a function

         if not Is_Empty_List (Output_Stmts) then
            Append_List (Output_Stmts, Stmts);
         end if;

         --  Build and insert controlling argument tests

         if Disp_Params /= No_List then

            First_Disp_Param := First (Disp_Params);

            if Present (First_Disp_Param)
              and then Present (Next (First_Disp_Param))
            then
               Next_Disp_Param := Next (First_Disp_Param);

               Cont_Arg_Test :=
                 Make_Op_Eq (Loc,

                   Left_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix =>
                         Make_Identifier (Loc,
                           Chars => Chars (First_Disp_Param)),
                       Selector_Name => New_Reference_To (Origin, Loc)),

                   Right_Opnd =>
                     Make_Selected_Component (Loc,
                       Prefix =>
                         Make_Identifier (Loc,
                           Chars => Chars (Next_Disp_Param)),
                       Selector_Name => New_Reference_To (Origin, Loc)));

               Next_Disp_Param := Next (Next_Disp_Param);

               while Present (Next_Disp_Param) loop

                  Cont_Arg_Test :=
                    Make_Op_And (Loc,
                      Left_Opnd => Cont_Arg_Test,

                      Right_Opnd =>
                        Make_Op_Eq (Loc,

                          Left_Opnd =>
                            Make_Selected_Component (Loc,
                              Prefix =>
                                Make_Identifier (Loc,
                                  Chars => Chars (First_Disp_Param)),
                              Selector_Name =>
                                New_Reference_To (Origin, Loc)),

                          Right_Opnd =>
                            Make_Selected_Component (Loc,
                              Prefix =>
                                Make_Identifier (Loc,
                                  Chars => Chars (Next_Disp_Param)),
                              Selector_Name =>
                                New_Reference_To (Origin, Loc))));

                  Next_Disp_Param := Next (Next_Disp_Param);
               end loop;

               Stmts := New_List (
                 Make_If_Statement (Loc,
                   Condition => Cont_Arg_Test,
                   Then_Statements => Stmts,
                   Else_Statements => New_List (
                     Make_Raise_Statement (Loc,
                       Name =>
                         New_Occurrence_Of
                           (Standard_Constraint_Error, Loc)))));
            end if;
         end if;

         --  Build the stub specification node

         if Nkind (Subp_Spec) = N_Function_Specification then
            Stub_Spec :=
              Make_Function_Specification (Loc,
                Defining_Unit_Name =>
                  Copy_Original_Tree (Defining_Unit_Name (Subp_Spec)),
                Parameter_Specifications => Stub_Param_Specs,
                Subtype_Mark =>
                  Full_Etype_Name (Subtype_Mark (Subp_Spec), Vis_Decl));

         else
            Stub_Spec :=
              Make_Procedure_Specification (Loc,
                Defining_Unit_Name =>
                  Copy_Original_Tree (Defining_Unit_Name (Subp_Spec)),
                Parameter_Specifications => Stub_Param_Specs);
         end if;

         --  Build the stub body node

         Stub_Body :=
           Make_Subprogram_Body (Loc,
              Specification => Stub_Spec,
              Declarations  => Decls,
              Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements => Stmts));

         return Stub_Body;

      end Build_Calling_Stub_Body;

      ---------------------------
      -- Find_Disp_Param_Spec --
      ---------------------------

      function Find_Disp_Param_Spec (L : List_Id) return Node_Id is
         Param_Spec : Node_Id;

      begin
         if Present (L) then
            Param_Spec := First (L);
            while Present (Param_Spec)
              and then not Is_Disp_Param_Spec (Param_Spec)
            loop
               Param_Spec := Next (Param_Spec);
            end loop;

            return Param_Spec;

         else
            return Empty;
         end if;
      end Find_Disp_Param_Spec;

      -----------------------------
      -- Has_Access_To_Root_Type --
      -----------------------------

      function Has_Access_To_Root_Type (L : List_Id) return Boolean is
      begin
         return Present (Find_Disp_Param_Spec (L));
      end Has_Access_To_Root_Type;

      ------------------------
      -- Is_Disp_Param_Spec --
      ------------------------

      function Is_Disp_Param_Spec (Param_Spec : Node_Id) return Boolean is
      begin
         return
           Nkind (Parameter_Type (Param_Spec)) = N_Access_Definition
           and then Etype (Subtype_Mark (Parameter_Type (Param_Spec)))
           = Root_Type;
      end Is_Disp_Param_Spec;

   --  Start of processing for Add_Racw_Stubs

   begin
      --  Initialize the dispatcher name variable

      Append_Nat_To_String ("dispatcher_receiver", Last_Racw_Num);
      Dispatcher_Name := Name_Find;

      --  Build and prepend the declaration of the object stub type

      Obj_Stub_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Object_Stub_Name),

          Type_Definition =>
            Make_Derived_Type_Definition (Loc,
              Subtype_Indication =>
                Make_Selected_Component (Loc,
                  Prefix => Build_Unit_Full_Name (Root_Pkg_Decl),
                  Selector_Name =>
                    Make_Identifier (Loc, Chars (Root_Type))),

              Record_Extension_Part =>
                Make_Record_Definition (Loc,
                  Component_List =>
                    Make_Component_List (Loc,
                      Component_Items => New_List (

                        Make_Component_Declaration (Loc,
                          Defining_Identifier =>
                            Make_Defining_Identifier (Loc, Origin_Name),
                          Subtype_Indication => SR_Partition_ID (Loc)),

                        Make_Component_Declaration (Loc,
                          Defining_Identifier =>
                            Make_Defining_Identifier (Loc, Receiver_Name),
                          Subtype_Indication => SR_RPC_Receiver (Loc)),

                        Make_Component_Declaration (Loc,
                          Defining_Identifier =>
                            Make_Defining_Identifier (Loc, Addr_Name),
                          Subtype_Indication =>
                          Make_Selected_Component (Loc,
                              Prefix => Make_Identifier (Loc, Name_System),
                              Selector_Name =>
                                Make_Identifier (Loc, Name_Address))))))));

      Prepend (Obj_Stub_Decl, Pkg_Bdy_Decls);

      --  Build and insert the declaration of the access to object_stub type

      Acc_To_Obj_Stub_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Object_Stub_Access_Name),

          Type_Definition =>
            Make_Access_To_Object_Definition (Loc,
              Subtype_Indication =>
                Make_Identifier (Loc, Object_Stub_Name)));

      Insert_After (Obj_Stub_Decl, Acc_To_Obj_Stub_Decl);

      --  Build and add the body of read procedure for the racw type

      Append_To (Pkg_Bdy_Decls,
        Make_Subprogram_Body (Loc,

          Specification => Racw_Read_Spec (Loc, Racw_Type),

          Declarations => New_List (
            Make_Object_Declaration (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, P_Name),
              Object_Definition => SR_Partition_ID (Loc)),

            Make_Object_Declaration (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, R_Name),
              Object_Definition => SR_RPC_Receiver (Loc)),

            Make_Object_Declaration (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, A_Name),
              Object_Definition =>
                Make_Selected_Component (Loc,
                  Prefix => Make_Identifier (Loc, Name_System),
                  Selector_Name =>
                    Make_Identifier (Loc, Name_Address))),


            Make_Object_Declaration (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, V_Name),
              Object_Definition =>
                Make_Identifier (Loc, Object_Stub_Access_Name)),

            Make_Function_Instantiation (Loc,
              Defining_Unit_Name => Make_Identifier (Loc, F_Name),
              Name => Make_Identifier (Loc, Name_Unchecked_Conversion),
              Generic_Associations => New_List (
                Make_Generic_Association (Loc,
                  Explicit_Generic_Actual_Parameter =>
                    Make_Selected_Component (Loc,
                      Prefix => Make_Identifier (Loc, Name_System),
                      Selector_Name =>
                        Make_Identifier (Loc, Name_Address))),

                Make_Generic_Association (Loc,
                  Explicit_Generic_Actual_Parameter =>
                    New_Reference_To (Racw_Type, Loc))))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Attribute_Reference (Loc,
                  Prefix => SR_Partition_ID (Loc),
                  Attribute_Name => Name_Read,

                  Expressions => New_List (
                    Make_Identifier (Loc, Stream_Name),
                    Make_Identifier (Loc, P_Name))),

                Make_Attribute_Reference (Loc,
                  Prefix => SR_RPC_Receiver (Loc),
                  Attribute_Name => Name_Read,

                  Expressions => New_List (
                    Make_Identifier (Loc, Stream_Name),
                    Make_Identifier (Loc, R_Name))),

                Make_Attribute_Reference (Loc,
                  Prefix =>
                    Make_Selected_Component (Loc,
                      Prefix =>
                        Make_Identifier (Loc, Name_System),
                      Selector_Name =>
                        Make_Identifier (Loc, Name_Address)),
                  Attribute_Name => Name_Read,

                  Expressions => New_List (
                    Make_Identifier (Loc, Stream_Name),
                    Make_Identifier (Loc, A_Name))),

                Make_If_Statement (Loc,
                  Condition =>
                    Make_Op_Eq (Loc,
                      Left_Opnd =>
                        Make_Function_Call (Loc,
                          Name => SP_Get_Local_Partition_Id (Loc)),

                      Right_Opnd =>
                        Make_Identifier (Loc, P_Name)),

                  Then_Statements => New_List (
                    Make_Assignment_Statement (Loc,
                      Name => Make_Identifier (Loc, Item_Name),
                      Expression =>
                        Make_Function_Call (Loc,
                          Name => Make_Identifier (Loc, F_Name),
                          Parameter_Associations => New_List (
                            Make_Identifier (Loc, A_Name))))),

                  Else_Statements => New_List (
                    Make_Assignment_Statement (Loc,
                      Name => Make_Identifier (Loc, V_Name),
                      Expression =>
                        Make_Allocator (Loc,
                          Expression =>
                            Make_Identifier (Loc, Object_Stub_Name))),

                    Make_Assignment_Statement (Loc,
                      Name =>
                        Make_Selected_Component (Loc,
                          Prefix => Make_Identifier (Loc, V_Name),
                          Selector_Name =>
                            Make_Identifier (Loc, Origin_Name)),

                      Expression =>
                        Make_Identifier (Loc, P_Name)),

                    Make_Assignment_Statement (Loc,
                      Name =>
                        Make_Selected_Component (Loc,
                          Prefix => Make_Identifier (Loc, V_Name),
                          Selector_Name =>
                            Make_Identifier (Loc, Receiver_Name)),

                      Expression =>
                        Make_Identifier (Loc, R_Name)),

                    Make_Assignment_Statement (Loc,
                      Name =>
                        Make_Selected_Component (Loc,
                          Prefix => Make_Identifier (Loc, V_Name),
                          Selector_Name =>
                            Make_Identifier (Loc, Addr_Name)),

                      Expression =>
                        Make_Identifier (Loc, A_Name)),

                    Make_Assignment_Statement (Loc,
                      Name => Make_Identifier (Loc, Item_Name),
                      Expression =>
                        Make_Type_Conversion (Loc,
                          Subtype_Mark => New_Reference_To (Racw_Type, Loc),
                          Expression =>
                            Make_Identifier (Loc, V_Name)))))))));

      --  Build and add write procedure body for the racw type.

      Append_To (Pkg_Bdy_Decls,

        Make_Subprogram_Body (Loc,

          Specification => Racw_Write_Spec (Loc, Racw_Type),

          Declarations => New_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_If_Statement (Loc,
                  Condition =>
                    Make_Not_In (Loc,
                      Left_Opnd =>
                        Make_Explicit_Dereference (Loc,
                          Prefix => Make_Identifier (Loc, Item_Name)),

                      Right_Opnd =>
                        Make_Identifier (Loc, Object_Stub_Name)),

                  Then_Statements => New_List (
                    Make_Attribute_Reference (Loc,
                      Prefix => SR_Partition_ID (Loc),
                      Attribute_Name => Name_Write,

                      Expressions => New_List (
                        Make_Identifier (Loc, Stream_Name),
                        Make_Function_Call (Loc,
                          Name => SP_Get_Local_Partition_Id (Loc)))),

                    Make_Attribute_Reference (Loc,
                      Prefix => SR_RPC_Receiver (Loc),
                      Attribute_Name => Name_Write,

                      Expressions => New_List (
                        Make_Identifier (Loc, Stream_Name),
                        Make_Attribute_Reference (Loc,
                          Prefix =>
                            Make_Identifier (Loc, Dispatcher_Name),
                          Attribute_Name => Name_Unrestricted_Access))),

                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix =>
                            Make_Identifier (Loc, Name_System),
                          Selector_Name =>
                            Make_Identifier (Loc, Name_Address)),
                          Attribute_Name => Name_Write,

                      Expressions => New_List (
                        Make_Identifier (Loc, Stream_Name),
                        Make_Attribute_Reference (Loc,
                          Prefix =>
                            Make_Explicit_Dereference (Loc,
                              Prefix =>
                                Make_Identifier (Loc, Item_Name)),
                          Attribute_Name => Name_Address)))),

                   Else_Statements => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix => SR_Partition_ID (Loc),
                       Attribute_Name => Name_Write,

                       Expressions => New_List (
                         Make_Identifier (Loc, Stream_Name),
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Make_Type_Conversion (Loc,
                               Subtype_Mark =>
                                 Make_Identifier (Loc, Object_Stub_Name),
                               Expression =>
                                 Make_Explicit_Dereference (Loc,
                                   Prefix =>
                                     Make_Identifier (Loc, Item_Name))),
                           Selector_Name =>
                             Make_Identifier (Loc, Origin_Name)))),

                     Make_Attribute_Reference (Loc,
                       Prefix => SR_RPC_Receiver (Loc),
                       Attribute_Name => Name_Write,

                       Expressions => New_List (
                         Make_Identifier (Loc, Stream_Name),
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Make_Type_Conversion (Loc,
                               Subtype_Mark =>
                                 Make_Identifier (Loc, Object_Stub_Name),
                               Expression =>
                                 Make_Explicit_Dereference (Loc,
                                   Prefix =>
                                     Make_Identifier (Loc, Item_Name))),

                           Selector_Name =>
                             Make_Identifier (Loc, Receiver_Name)))),

                     Make_Attribute_Reference (Loc,
                       Prefix =>
                         Make_Selected_Component (Loc,
                           Prefix => Make_Identifier (Loc, Name_System),
                           Selector_Name =>
                             Make_Identifier (Loc, Name_Address)),
                       Attribute_Name => Name_Write,

                       Expressions => New_List (
                         Make_Identifier (Loc, Stream_Name),
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Make_Type_Conversion (Loc,
                               Subtype_Mark =>
                                 Make_Identifier (Loc, Object_Stub_Name),
                               Expression =>
                                 Make_Explicit_Dereference (Loc,
                                   Prefix =>
                                     Make_Identifier (Loc, Item_Name))),
                           Selector_Name =>
                             Make_Identifier (Loc, Addr_Name))))))))));

      --  Build the calling stubs

      if Present (Root_Pkg_Vis_Decls) then
         Decl := First (Root_Pkg_Vis_Decls);
         while Present (Decl) loop

            --  Build and append primitive operation calling stubs

            if (Nkind (Decl) = N_Subprogram_Declaration or
                Nkind (Decl) = N_Abstract_Subprogram_Declaration)
              and then Has_Access_To_Root_Type (Parameter_Specifications
                (Specification (Decl)))
            then
               Prim_Op_Num := Prim_Op_Num + 1;

               --  Generate regular or asynchronous stubs if pragma
               --  asynchronous applies to the racw type

               if Async and then
                 Nkind (Specification (Decl)) = N_Procedure_Specification
               then
                  Racw_CStub_Body := Build_Async_Calling_Stub_Body (Decl);
                  Racw_RStub_Body := Build_Async_Receiving_Stub_Body (Decl);
                  Param_Assocs :=
                    New_List (Make_Identifier (Loc, Params_Name));

               else
                  Racw_CStub_Body := Build_Calling_Stub_Body (Decl);
                  Racw_RStub_Body := Build_Receiving_Stub_Body (Decl);
                  Param_Assocs :=
                    New_List (
                      Make_Identifier (Loc, Params_Name),
                      Make_Identifier (Loc, Result_Name));
               end if;

               Racw_CStub_Spec := Specification (Racw_CStub_Body);
               Racw_RStub_Spec := Specification (Racw_RStub_Body);

               --  Insert the declaration of the calling stub after the
               --  declaration of the access to object stub

               Insert_After (Acc_To_Obj_Stub_Decl,
                 Make_Subprogram_Declaration (Loc,
                   Specification => Copy_Separate_Tree (Racw_CStub_Spec)));

               --  Append the calling stub body to the package body
               --  declarations

               Append (Racw_CStub_Body, Pkg_Bdy_Decls);

               --  Append the receiving stub body to the dispatching
               --  receiver declarations

               Append (Racw_RStub_Body, Dispatcher_Decls);

               --  Append a case statement alternative for the
               --  primitive operation.

               Append_To (Case_Stmt_Alts,
                 Make_Case_Statement_Alternative (Loc,
                   Discrete_Choices => New_List (
                     Make_Integer_Literal (Loc, UI_From_Int (Prim_Op_Num))),

                   Statements => New_List (
                     Make_Procedure_Call_Statement (Loc,
                       Name => Copy_Original_Tree (
                         Defining_Unit_Name (Racw_RStub_Spec)),
                       Parameter_Associations => Param_Assocs))));
            end if;

            Decl := Next (Decl);
         end loop;
      end if;

      --  Complete dispatching receiver building

      Prepend_To (Dispatcher_Decls,
        Make_Function_Instantiation (Loc,
          Defining_Unit_Name => Make_Identifier (Loc, F_Name),
          Name => Make_Identifier (Loc, Name_Unchecked_Conversion),
          Generic_Associations => New_List (
            Make_Generic_Association (Loc,
              Explicit_Generic_Actual_Parameter =>
                  Make_Selected_Component (Loc,
                    Prefix => Make_Identifier (Loc, Name_System),
                    Selector_Name => Make_Identifier (Loc, Name_Address))),

            Make_Generic_Association (Loc,
              Explicit_Generic_Actual_Parameter =>
                New_Reference_To (Racw_Type, Loc)))));

      Append_To (Case_Stmt_Alts,
        Make_Case_Statement_Alternative (Loc,
          Discrete_Choices => New_List (Make_Others_Choice (Loc)),
          Statements => New_List (Make_Null_Statement (Loc))));

      Dispatcher_Spec :=
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name =>
            Make_Identifier (Loc, Dispatcher_Name),
          Parameter_Specifications => NStub_Param_Specs (Loc));

      Append_To (Dispatcher_Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Prim_Op_Num_Name),
          Object_Definition => SP_Subprogram_Id (Loc)));

      Dispatcher_Body :=
        Make_Subprogram_Body (Loc,
          Specification => Dispatcher_Spec,

          Declarations  => Dispatcher_Decls,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Attribute_Reference (Loc,
                  Prefix => SP_Subprogram_Id (Loc),
                  Attribute_Name => Name_Read,

                  Expressions => New_List (
                    Make_Identifier (Loc, Params_Name),
                    Make_Identifier (Loc, Prim_Op_Num_Name))),
                    Make_Case_Statement (Loc,
                      Expression =>
                        Make_Identifier (Loc, Prim_Op_Num_Name),
                      Alternatives => Case_Stmt_Alts))));

      Prepend (Dispatcher_Body, Pkg_Bdy_Decls);

      --  Add with clauses to the stubs context items

      if Present (Root_Pkg_CItems) then
         CItem := First (Root_Pkg_CItems);

         while Present (CItem) loop
            if Nkind (CItem) = N_With_Clause
              and then Comes_From_Source (CItem)
            then
               Add_With_Clause
                 (Copy_Original_Tree (Name (CItem)),
                  CItems);
            end if;

            CItem := Next (CItem);
         end loop;

         Append_List (CItems, Stubs_CItems);
      end if;
   end Add_Racw_Stubs;

   -----------------------
   -- Add_RAST_Features --
   -----------------------

   procedure Add_RAST_Features (Vis_Decl : Node_Id) is
      --  Information needed from the input declaration

      Loc           : Source_Ptr := Sloc (Vis_Decl);
      Ras_Type      : Entity_Id  := Defining_Identifier (Vis_Decl);
      Fat_Type      : constant Entity_Id := Equivalent_Type (Ras_Type);
      Old_Name      : constant Name_Id   := Chars (Ras_Type);
      Origin_Name   : constant Name_Id   := Get_Name_Id ("origin");
      Receiver_Name : constant Name_Id   := Get_Name_Id ("receiver");
      Subp_Id_Name  : constant Name_Id   := Get_Name_Id ("subp_id");
      Async_Name    : constant Name_Id   := Get_Name_Id ("async");
      New_Name      : constant Name_Id   := Chars (Fat_Type);

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Build_RAST_Deref_Subp_Body (N : Node_Id);
      --  Build subprogram that implements dereference for a given RAST.

      procedure Build_Tick_Access_Conv_Body (N : Node_Id);
      --  Build the function that converts access attribute into fat pointer
      --  for a remote subprogram.

      procedure RAS_Input_Body;
      --  Build the body of the Input stream subprogram for the RAS type.

      procedure RAS_Output_Body;
      --  Build the body of the Output stream subprogram for the RAS type.

      --------------------------------
      -- Build_RAST_Deref_Subp_Body --
      --------------------------------

      procedure Build_RAST_Deref_Subp_Body (N : Node_Id) is

         --  Information needed from the input declaration node;

         Type_Def         : Node_Id := Type_Definition (N);

         --  Building new entities for the local identifiers

         Stream_In        : Entity_Id;
         Stream_Out       : Entity_Id;
         Returned_Val     : Entity_Id;
         Except           : Entity_Id;

         --  Features for the dereference subpprogram body to create

         Param_Specs      : List_Id;
         Param_Spec       : Node_Id;
         Deref_Subp_Spec  : Node_Id;
         Deref_Subp_Body  : Node_Id;
         Deref_Subp_Decls : List_Id := New_List;
         Deref_Subp_Stmts : List_Id := New_List;
         Write_Stmts      : List_Id := New_List;
         Output_Stmts     : List_Id := New_List;
         Then_Stmts       : List_Id := New_List;
         Else_Stmts       : List_Id := New_List;


         --  Variable for the declaration node of a stream

         Stream_Decl   : Node_Id;

         --  Start processing for Build_RAST_Deref_Subp_Body

      begin

         --  Initialization of the local entities

         Stream_In    :=
           Make_Defining_Identifier (Loc, Stream_In_Name);
         Stream_Out   :=
           Make_Defining_Identifier (Loc, Stream_Out_Name);
         Returned_Val :=
           Make_Defining_Identifier (Loc, Returned_Val_Name);
         Except       :=
           Make_Defining_Identifier (Loc, Except_Name);

         Stream_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Stream_In,
             Object_Definition =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc),

               Constraint =>
                 Make_Index_Or_Discriminant_Constraint (Loc,
                   Constraints =>
                     New_List (Make_Integer_Literal (Loc, Uint_0)))));

         Set_Aliased_Present (Stream_Decl);
         Append (Stream_Decl, Deref_Subp_Decls);

         --  Build and append stream output declaration to the list of
         --  declarations

         Stream_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Stream_Out,

             Object_Definition =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc),

                 Constraint =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints =>
                       New_List (Make_Integer_Literal (Loc, Uint_0)))));

         Set_Aliased_Present (Stream_Decl);
         Append (Stream_Decl, Deref_Subp_Decls);

         --  Append the declaration for the exeption occurrence

         Append_To (Deref_Subp_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Except,
             Object_Definition =>
               New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc)));

         --  Append the declaration for the returned value in the
         --  case of a function

         if Nkind (Type_Def) = N_Access_Function_Definition
           and then not Has_Unknown_Size (Etype (Subtype_Mark (Type_Def)))
         then
            Append_To (Deref_Subp_Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Returned_Val,
                Object_Definition =>
                  New_Occurrence_Of
                    (Entity (Subtype_Mark (Type_Def)), Loc)));
         end if;

         --  Build and append the write statement for the Receiver, to the
         --  list of statements

         Append_To (Deref_Subp_Stmts,
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (RTE (RE_RPC_Receiver), Loc),
                 Attribute_Name => Name_Write,

               Expressions => New_List (
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Reference_To (Stream_In, Loc),
                   Attribute_Name => Name_Unchecked_Access),

                 Unchecked_Convert_To (RTE (RE_RPC_Receiver),
                    Make_Selected_Component (Loc,
                       Prefix =>
                         Make_Identifier (Loc, Name_Pointer),
                       Selector_Name =>
                         Make_Identifier (Loc, Receiver_Name))))));

         --  Write statement for the subprogram identifier

         Append_To (Deref_Subp_Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Standard_Integer, Loc),
             Attribute_Name => Name_Write,

           Expressions => New_List (
             Make_Attribute_Reference (Loc,
               Prefix => New_Reference_To (Stream_In, Loc),
               Attribute_Name => Name_Unchecked_Access),

             Make_Selected_Component (Loc,
               Prefix => Make_Identifier (Loc, Name_Pointer),
               Selector_Name => Make_Identifier (Loc, Subp_Id_Name)))));

         --  Append the write statements for the in parameters
         --  and the read statements for the out parameters

         Param_Specs := New_List_Copy (Parameter_Specifications (Type_Def));

         --  Append the write statements for the in parameters
         --  and the read statements for the out parameters

         if Param_Specs /= No_List then

            Param_Spec := First (Param_Specs);

            while Present (Param_Spec) loop

               Set_Defining_Identifier (Param_Spec,
                 New_Copy (Defining_Identifier (Param_Spec)));

               if Has_Unknown_Size (Etype
                 (Parameter_Type (Param_Spec)))
               then

                  Append_To (Deref_Subp_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix => Parameter_Type (Param_Spec),
                      Attribute_Name => Name_Output,

                    Expressions =>
                      New_List (
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Reference_To (Stream_In, Loc),
                          Attribute_Name => Name_Unchecked_Access),

                        Make_Identifier (Loc,
                          Chars =>
                            Chars (Defining_Identifier (Param_Spec))))));

               elsif In_Present (Param_Spec)
                 or else not Out_Present (Param_Spec)
               then
                  Append_To (Write_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix => Parameter_Type (Param_Spec),
                        Attribute_Name => Name_Write,

                      Expressions =>
                        New_List (
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Reference_To (Stream_In, Loc),
                            Attribute_Name => Name_Unchecked_Access),
                          Make_Identifier (Loc,
                            Chars =>
                              Chars (Defining_Identifier (Param_Spec))))));
               end if;

               if Out_Present (Param_Spec) then

                  --  Read operation are within an if statement and
                  --  are thus appended to a then statement list which
                  --  will be used later to build the if statement.

                  if Has_Unknown_Size (Etype (
                    Parameter_Type (Param_Spec)))
                  then
                     Append_To (Output_Stmts,
                       Make_Assignment_Statement (Loc,
                         Name =>
                           Make_Identifier (Loc,
                             Chars =>
                               Chars (Defining_Identifier (Param_Spec))),

                         Expression =>
                               Make_Attribute_Reference (Loc,
                                 Prefix => Parameter_Type (Param_Spec),
                                 Attribute_Name => Name_Input,

                               Expressions => New_List (
                                 Make_Attribute_Reference (Loc,
                                   Prefix =>
                                     New_Reference_To (Stream_Out, Loc),
                                   Attribute_Name =>
                                     Name_Unchecked_Access)))));

                  else
                     Append_To (Output_Stmts,
                       Make_Attribute_Reference (Loc,
                         Prefix => Parameter_Type (Param_Spec),
                         Attribute_Name => Name_Read,

                       Expressions =>
                         New_List (
                           Make_Attribute_Reference (Loc,
                             Prefix => New_Reference_To (Stream_Out, Loc),
                             Attribute_Name => Name_Unchecked_Access),

                           Make_Identifier (Loc,
                             Chars =>
                               Chars (Defining_Identifier (Param_Spec))))));
                  end if;
               end if;

               Param_Spec := Next (Param_Spec);
            end loop;
         end if;

         Append_List (Write_Stmts, Deref_Subp_Stmts);

         --  Append Do_Apc call to Then Statements

         Append_To (Then_Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Do_Apc), Loc),
             Parameter_Associations => New_List (

               Make_Type_Conversion (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_Partition_ID), Loc),
                 Expression =>
                   Make_Selected_Component (Loc,
                     Prefix =>
                       Make_Identifier (Loc, Name_Pointer),
                     Selector_Name => Make_Identifier (Loc, Origin_Name))),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access))));

         --  Append Do_Rpc call to Else Statements

         Append_To (Else_Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Do_Rpc), Loc),
             Parameter_Associations => New_List (

               Make_Type_Conversion (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (RTE (RE_Partition_ID), Loc),
                 Expression =>
                   Make_Selected_Component (Loc,
                     Prefix =>
                       Make_Identifier (Loc, Name_Pointer),
                     Selector_Name => Make_Identifier (Loc, Origin_Name))),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_Out, Loc),
                 Attribute_Name => Name_Unchecked_Access))));

         --  Append the read operation for the exception occurrence
         --  and the call to Reraise_Occurence

         Append_To (Else_Stmts,
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc),
                 Attribute_Name => Name_Read,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_Out, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               New_Reference_To (Except, Loc))));

         Append_To (Else_Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Reraise_Occurrence), Loc),
             Parameter_Associations => New_List (
               New_Reference_To (Except, Loc))));

         --  Append the read statement for the returned value, and
         --  the return statement in the case of a function

         if Nkind (Type_Def) = N_Access_Function_Definition then

            if Has_Unknown_Size (Etype (Subtype_Mark (Type_Def))) then

               Append_To (Output_Stmts,
                 Make_Return_Statement (Loc,
                   Expression =>
                         Make_Attribute_Reference (Loc,
                           Prefix => Subtype_Mark (Type_Def),
                           Attribute_Name => Name_Input,

                           Expressions => New_List (
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Reference_To (Stream_Out, Loc),
                               Attribute_Name => Name_Unchecked_Access)))));

            else
               Append_To (Output_Stmts,
                     Make_Attribute_Reference (Loc,
                       Prefix =>
                         New_Occurrence_Of
                           (Entity (Subtype_Mark (Type_Def)),  Loc),
                       Attribute_Name => Name_Read,

                       Expressions => New_List (
                         Make_Attribute_Reference (Loc,
                           Prefix => New_Reference_To (Stream_Out, Loc),
                           Attribute_Name => Name_Unchecked_Access),

                         New_Reference_To (Returned_Val, Loc))));

               --  Append a return statement; the returned value is the one
               --  previously read from the stream output

               Append_To (Output_Stmts,
                 Make_Return_Statement (Loc,
                   Expression => New_Reference_To (Returned_Val, Loc)));
            end if;
         end if;

         --  Append statements for the out parameters and
         --  the returned value in the case of a function

         if not Is_Empty_List (Output_Stmts) then
            Append_List (Output_Stmts, Else_Stmts);
         end if;

         --  If the subprogram is a procedure, its body contains a test
         --  to check whether the call is asynchronous.

         if Nkind (Type_Definition (N)) = N_Access_Procedure_Definition then
            Append_To (Deref_Subp_Stmts,
              Make_If_Statement (Loc,
                Condition =>
                  Make_Selected_Component (Loc,
                    Prefix => Make_Identifier (Loc, Name_Pointer),
                    Selector_Name => Make_Identifier (Loc, Async_Name)),
                Then_Statements => Then_Stmts,
                Else_Statements => Else_Stmts));
         else
            Append_List_To (Deref_Subp_Stmts, Else_Stmts);
         end if;

         --  Check for parameterless subprograms.

         if not Present (Param_Specs) then
            Param_Specs := Empty_List;
         end if;

         Param_Spec := Make_Parameter_Specification (Loc,
           Defining_Identifier => Make_Defining_Identifier (Loc, Name_Pointer),
           Parameter_Type      => New_Occurrence_Of (Fat_Type, Loc));

         Prepend (Param_Spec, Param_Specs);

         if Nkind (Type_Def) = N_Access_Function_Definition then

            Deref_Subp_Spec :=
              Make_Function_Specification (Loc,
                Defining_Unit_Name       =>
                  Make_Defining_Identifier (Loc, Name_uRAS_Dereference),
                Parameter_Specifications => Param_Specs,
                Subtype_Mark =>
                   New_Occurrence_Of
                     (Entity (Subtype_Mark (Type_Definition (N))), Loc));

         elsif Nkind (Type_Definition (N)) = N_Access_Procedure_Definition then

            Deref_Subp_Spec :=
              Make_Procedure_Specification (Loc,
                Defining_Unit_Name       =>
                  Make_Defining_Identifier (Loc, Name_uRAS_Dereference),
                Parameter_Specifications => Param_Specs);
         end if;

         Deref_Subp_Body :=
           Make_Subprogram_Body (Loc,
             Specification => Deref_Subp_Spec,
             Declarations  => Deref_Subp_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Deref_Subp_Stmts));

         Set_TSS (Fat_Type, Defining_Unit_Name (Deref_Subp_Spec));

      end Build_RAST_Deref_Subp_Body;

      ---------------------------------
      -- Build_Tick_Access_Conv_Body --
      ---------------------------------

      procedure Build_Tick_Access_Conv_Body (N : Node_Id) is
         Pkg_Name      : constant Name_Id := Get_Name_Id ("pkg_name");
         Var_Name      : constant Name_Id := New_Internal_Name ('V');
         TA_Name       : constant Name_Id := Get_Name_Id ("tick_access");
         TA_Conv_Spec  : Node_Id;
         TA_Conv_Body  : Node_Id;
         TA_Conv_Stmts : List_Id;
         TA_Conv_Decls : List_Id;

         --  Start processing for Build_Tick_Access_Conv_Body

      begin

         TA_Conv_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name =>
               Make_Defining_Identifier (Loc, Name_uRAS_Access),

             Parameter_Specifications =>
               New_List (
                 Make_Parameter_Specification (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc, TA_Name),
                   Parameter_Type =>
                     New_Occurrence_Of (Ras_Type, Loc)),

                 Make_Parameter_Specification (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc, Pkg_Name),
                   Parameter_Type =>
                     New_Reference_To (Standard_String, Loc)),

                 Make_Parameter_Specification (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc, Subp_Id_Name),
                   Parameter_Type =>
                     New_Reference_To (Standard_Natural, Loc)),

                 Make_Parameter_Specification (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc, Async_Name),
                   Parameter_Type =>
                     New_Reference_To (Standard_Boolean, Loc))),

             Subtype_Mark => New_Occurrence_Of (Fat_Type, Loc));

         TA_Conv_Decls :=
           New_List (
             Make_Object_Declaration (Loc,
               Defining_Identifier =>
                 Make_Defining_Identifier (Loc, Var_Name),
               Object_Definition =>
                 Make_Subtype_Indication (Loc,
                   Subtype_Mark => New_Occurrence_Of (Fat_Type, Loc),
                   Constraint   =>
                     Make_Index_Or_Discriminant_Constraint (Loc,
                       Constraints => New_List (
                         New_Reference_To (Standard_True, Loc))))));

         TA_Conv_Stmts :=
           New_List (
             Make_Assignment_Statement (Loc,
               Name =>
                 Make_Selected_Component (Loc,
                   Prefix =>
                     Make_Identifier (Loc, Var_Name),
                   Selector_Name =>
                     Make_Identifier (Loc, Origin_Name)),

               Expression =>
                 Make_Type_Conversion (Loc,
                   Subtype_Mark =>
                     New_Occurrence_Of (Standard_Integer, Loc),
                   Expression   =>
                     Make_Function_Call (Loc,
                       Name =>
                             New_Occurrence_Of
                               (RTE (RE_Get_Active_Partition_Id), Loc),
                       Parameter_Associations =>
                         New_List (Make_Identifier (Loc, Pkg_Name))))),

                     Make_Assignment_Statement (Loc,
                       Name =>
                         Make_Selected_Component (Loc,
                           Prefix =>
                             Make_Identifier (Loc, Var_Name),
                           Selector_Name =>
                             Make_Identifier (Loc, Receiver_Name)),

                        Expression =>
                          Unchecked_Convert_To (RTE (RE_Address),
                            Make_Function_Call (Loc,
                              Name =>
                                New_Occurrence_Of
                                  (RTE (RE_Get_RCI_Package_Receiver), Loc),
                              Parameter_Associations =>
                              New_List (Make_Identifier (Loc, Pkg_Name))))),

             Make_Assignment_Statement (Loc,
               Name =>
                 Make_Selected_Component (Loc,
                   Prefix =>
                     Make_Identifier (Loc, Var_Name),
                   Selector_Name =>
                     Make_Identifier (Loc, Subp_Id_Name)),
               Expression =>
                 Make_Identifier (Loc, Subp_Id_Name)),

             Make_Assignment_Statement (Loc,
               Name =>
                 Make_Selected_Component (Loc,
                   Prefix =>
                     Make_Identifier (Loc, Var_Name),
                   Selector_Name =>
                     Make_Identifier (Loc, Async_Name)),
               Expression =>
                 Make_Identifier (Loc, Async_Name)),

             Make_Return_Statement (Loc,
               Expression =>
                 Make_Identifier (Loc, Var_Name)));

         TA_Conv_Body :=
           Make_Subprogram_Body (Loc,
             Specification => TA_Conv_Spec,
             Declarations  => TA_Conv_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => TA_Conv_Stmts));

         Set_TSS (Fat_Type, Defining_Unit_Name (TA_Conv_Spec));

      end Build_Tick_Access_Conv_Body;

      --------------------
      -- RAS_Input_Body --
      --------------------

      procedure RAS_Input_Body is
         Var_Name   : constant Name_Id := New_Internal_Name ('V');
         Subp_Decls : List_Id;
         Subp_Stmts : List_Id;
         Subp_Body  : Node_Id;

         --  Start processing for RAS_Input_Body
      begin
         Subp_Decls := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               Make_Defining_Identifier (Loc, Var_Name),
             Object_Definition =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => New_Occurrence_Of (Fat_Type, Loc),
                 Constraint   =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints => New_List (
                       New_Reference_To (Standard_True, Loc))))));

         Subp_Stmts := New_List (

           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Standard_Integer, Loc),
             Attribute_Name => Name_Read,

             Expressions =>
               New_List (
                 Make_Identifier (Loc, Stream_Name),
                 Make_Selected_Component (Loc,
                   Prefix => Make_Identifier (Loc, Var_Name),
                   Selector_Name => Make_Identifier (Loc, Origin_Name)))),

           Make_Attribute_Reference (Loc,
             Prefix =>
               New_Occurrence_Of (RTE (RE_Address), Loc),
             Attribute_Name => Name_Read,

             Expressions =>
               New_List (
                 Make_Identifier (Loc, Stream_Name),
                 Make_Selected_Component (Loc,
                   Prefix => Make_Identifier (Loc, Var_Name),
                   Selector_Name => Make_Identifier (Loc, Receiver_Name)))),

           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (Standard_Natural, Loc),
             Attribute_Name => Name_Read,

             Expressions =>
               New_List (
                 Make_Identifier (Loc, Stream_Name),
                 Make_Selected_Component (Loc,
                   Prefix => Make_Identifier (Loc, Var_Name),
                   Selector_Name => Make_Identifier (Loc, Subp_Id_Name)))),

           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (Standard_Boolean, Loc),
             Attribute_Name => Name_Read,

             Expressions =>
               New_List (
                 Make_Identifier (Loc, Stream_Name),
                 Make_Selected_Component (Loc,
                   Prefix => Make_Identifier (Loc, Var_Name),
                   Selector_Name => Make_Identifier (Loc, Async_Name)))),

           Make_Return_Statement (Loc,
             Expression => Make_Identifier (Loc, Var_Name)));

         Subp_Body :=
           Make_Subprogram_Body (Loc,
             Specification => Ras_Input_Spec (Loc, Ras_Type),
             Declarations => Subp_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Subp_Stmts));

         Set_TSS (Fat_Type, Defining_Unit_Name (Specification (Subp_Body)));

      end RAS_Input_Body;

      ---------------------
      -- RAS_Output_Body --
      ---------------------

      procedure RAS_Output_Body is
         Subp_Stmts : List_Id;
         Subp_Body  : Node_Id;

         --  Start processing for RAS_Output_Body

      begin
         Subp_Stmts := New_List (

           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Standard_Integer, Loc),
             Attribute_Name => Name_Write,

             Expressions =>
               New_List (
                 Make_Identifier (Loc, Stream_Name),
                 Make_Selected_Component (Loc,
                   Prefix => Make_Identifier (Loc, Item_Name),
                   Selector_Name => Make_Identifier (Loc, Origin_Name)))),

           Make_Attribute_Reference (Loc,
             Prefix =>
               New_Occurrence_Of (RTE (RE_Address), Loc),
             Attribute_Name => Name_Write,

             Expressions =>
               New_List (
                 Make_Identifier (Loc, Stream_Name),
                 Make_Selected_Component (Loc,
                   Prefix => Make_Identifier (Loc, Item_Name),
                   Selector_Name => Make_Identifier (Loc, Receiver_Name)))),

           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (Standard_Natural, Loc),
             Attribute_Name => Name_Write,

             Expressions =>
               New_List (
                 Make_Identifier (Loc, Stream_Name),
                 Make_Selected_Component (Loc,
                   Prefix => Make_Identifier (Loc, Item_Name),
                   Selector_Name => Make_Identifier (Loc, Subp_Id_Name)))),

           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (Standard_Boolean, Loc),
             Attribute_Name => Name_Write,

             Expressions =>
               New_List (
                 Make_Identifier (Loc, Stream_Name),
                 Make_Selected_Component (Loc,
                   Prefix => Make_Identifier (Loc, Item_Name),
                   Selector_Name => Make_Identifier (Loc, Async_Name)))));

         Subp_Body :=
           Make_Subprogram_Body (Loc,
             Specification => Ras_Output_Spec (Loc, Ras_Type),
             Declarations => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Subp_Stmts));

         Set_TSS (Fat_Type, Defining_Unit_Name (Specification (Subp_Body)));

      end RAS_Output_Body;

      --  Start processing for Add_RAST_Features

   begin
      Init_Names;
      Build_RAST_Deref_Subp_Body  (Vis_Decl);
      Build_Tick_Access_Conv_Body (Vis_Decl);
      RAS_Input_Body;
      RAS_Output_Body;

   end Add_RAST_Features;

   ---------------------
   -- Add_With_Clause --
   ---------------------

   procedure Add_With_Clause
     (Nam    : in Node_Id;
      CItems : in out List_Id)
   is
      Loc            : Source_Ptr := Sloc (Nam);
      Withed         : Boolean := False;
      Context        : Node_Id;

   begin
      if CItems /= No_List then
         Context := First (CItems);

         --  Check if Nam is already in the with clause

         while Present (Context) and not Withed loop
            if Nkind (Context) = N_With_Clause then
               Withed := Designate_Same_Unit (Name (Context), Nam);
            end if;

            Context := Next (Context);
         end loop;

         --  Add to the context clause list if not withed

         if not Withed then
            Prepend (Make_With_Clause (Loc, Nam), CItems);
         end if;
      else

         --  Build a new context item list

         CItems := New_List (
           Make_With_Clause (Loc, Nam));
      end if;
   end Add_With_Clause;

   --------------------------
   -- Append_Nat_To_String --
   --------------------------

   procedure Append_Nat_To_String (S : String; V : Nat) is

      -----------------------
      -- Local subprograms --
      -----------------------

      procedure Add_Nat_To_Name_Buffer (V : Nat);
      --  Add decimal representation of given value to the end of the string
      --  currently stored in Name_Buffer, incrementing Name_Len as required.

      ----------------------------
      -- Add_Nat_To_Name_Buffer --
      ----------------------------

      procedure Add_Nat_To_Name_Buffer (V : Nat) is
      begin
         if V >= 10 then
            Add_Nat_To_Name_Buffer (V / 10);
         end if;

         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) :=
           Character'Val (Character'Pos ('0') + V rem 10);
      end Add_Nat_To_Name_Buffer;

      --  Start of processing for Append_Nat_To_String

   begin
      Name_Len := S'Length;
      Name_Buffer (1 .. Name_Len) := S;
      Add_Nat_To_Name_Buffer (V);

   end Append_Nat_To_String;

   --------------------
   -- Add_System_Rpc --
   --------------------

   procedure Add_System_Rpc (C_Unit : Node_Id) is
      Contexts   : List_Id := Context_Items (C_Unit);
      Lib_Unit   : Node_Id;
      Withn      : Node_Id;
      Use_Clause : Node_Id := Empty;
      Uname      : Unit_Name_Type;
      Unum       : Unit_Number_Type;
      UEntity    : Entity_Id;
      Withed     : Boolean := False;
      Context    : Node_Id;
      Name_Node  : Node_Id;

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

   --  Start of processing for Add_System_Rpc

   begin
      Name_Buffer (1 .. 12) := "system.rpc%s";
      Name_Len := 12;
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
      UEntity   := Defining_Entity (Lib_Unit);
      Name_Node := Defining_Unit_Name (Specification (Lib_Unit));

      --  Add to the context clause

      Withn :=
        Make_With_Clause (Standard_Location,
          Name => New_Reference_To (UEntity, Standard_Location));
      Set_Library_Unit          (Withn, Cunit (Unum));
      Set_Corresponding_Spec    (Withn, UEntity);
      Set_First_Name            (Withn, True);
      Set_Implicit_With         (Withn, True);
      Mark_Rewrite_Insertion (Withn);

      if No (Contexts) then
         Set_Context_Items (C_Unit,
           New_List (
             Withn,
             Make_Use_Package_Clause (Standard_Location,
               Names => New_List (
                 New_Reference_To (UEntity, Standard_Location)))));

      else
         --  Do a search for the Use_Clause

         Context := First (Contexts);
         while Present (Context) and Use_Clause = Empty loop
            if Nkind (Context) = N_Use_Package_Clause then
               Use_Clause := Context;

            else
               Context := Next (Context);
            end if;
         end loop;

         Prepend (Withn, Contexts);

         if Present (Use_Clause) then

            if Present (Names (Use_Clause)) then
               Append_To (Names (Use_Clause),
                 New_Reference_To (UEntity, Standard_Location));
            else
               Set_Names (Use_Clause,
                 New_List (New_Reference_To (UEntity, Standard_Location)));
            end if;

         else
            Append_To (Contexts,
              Make_Use_Package_Clause (Standard_Location,
                New_List (New_Reference_To (UEntity, Standard_Location))));
         end if;
      end if;
   end Add_System_Rpc;

   -----------------
   -- Ada_Streams --
   -----------------

   function Ada_Streams (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => Make_Identifier (Loc, Name_Ada),
          Selector_Name => Make_Identifier (Loc, Name_Streams));
   end Ada_Streams;

   --------------------
   -- Ada_Exceptions --
   --------------------

   function Ada_Exceptions (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => Make_Identifier (Loc, Name_Ada),
          Selector_Name => Make_Identifier (Loc, Exceptions_Name));
   end Ada_Exceptions;

   ----------------
   -- System_Rpc --
   ----------------

   function System_Rpc (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => Make_Identifier (Loc, Name_System),
          Selector_Name => Make_Identifier (Loc, Name_Rpc));
   end System_Rpc;

   -----------------------
   -- System_PInterface --
   -----------------------

   function System_PInterface (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => Make_Identifier (Loc, Name_System),
          Selector_Name => Make_Identifier (Loc, Partition_Interface_Name));
   end System_PInterface;

   ---------------------
   -- SR_Partition_ID --
   ---------------------

   function SR_Partition_ID (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => System_Rpc (Loc),
          Selector_Name => Make_Identifier (Loc, Name_Partition_ID));
   end SR_Partition_ID;

   ---------------------
   -- SR_RPC_Receiver --
   ---------------------

   function SR_RPC_Receiver (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => System_Rpc (Loc),
          Selector_Name => Make_Identifier (Loc, RPC_Receiver_Name));
   end SR_RPC_Receiver;

   ---------------------------
   -- SR_Params_Stream_Type --
   ---------------------------

   function SR_Params_Stream_Type (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => System_Rpc (Loc),
          Selector_Name => Make_Identifier (Loc, Params_Stream_Type_Name));
   end SR_Params_Stream_Type;

   ---------------
   -- SR_Do_Rpc --
   ---------------

   function SR_Do_Rpc (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => System_Rpc (Loc),
          Selector_Name => Make_Identifier (Loc, Do_Rpc_Name));
   end SR_Do_Rpc;

   ---------------
   -- SR_Do_Apc --
   ---------------

   function SR_Do_Apc (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => System_Rpc (Loc),
          Selector_Name => Make_Identifier (Loc, Do_Apc_Name));
   end SR_Do_Apc;

   -----------------------------
   -- AE_Exception_Occurrence --
   -----------------------------

   function AE_Exception_Occurrence (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => Ada_Exceptions (Loc),
          Selector_Name => Make_Identifier (Loc, Exception_Occurrence_Name));
   end AE_Exception_Occurrence;

   ------------------------
   -- AE_Null_Occurrence --
   ------------------------

   function AE_Null_Occurrence (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => Ada_Exceptions (Loc),
          Selector_Name => Make_Identifier (Loc, Null_Occurrence_Name));
   end AE_Null_Occurrence;

   ---------------------------
   -- AE_Reraise_Occurrence --
   ---------------------------

   function AE_Reraise_Occurrence (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => Ada_Exceptions (Loc),
          Selector_Name => Make_Identifier (Loc, Reraise_Occurrence_Name));
   end AE_Reraise_Occurrence;

   -------------------------
   -- AS_Root_Stream_Type --
   -------------------------

   function AS_Root_Stream_Type (Loc : Source_Ptr) return Node_Id is
   begin
      return
         Make_Selected_Component (Loc,
           Prefix => Ada_Streams (Loc),
           Selector_Name => Make_Identifier (Loc, Root_Stream_Type_Name));
   end AS_Root_Stream_Type;

   -----------------
   -- SP_RCI_Info --
   -----------------

   function SP_RCI_Info (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => System_PInterface (Loc),
          Selector_Name => Make_Identifier (Loc, RCI_Info_Name));
   end SP_RCI_Info;

   ----------------------
   -- SP_Subprogram_id --
   ----------------------

   function SP_Subprogram_Id (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => System_PInterface (Loc),
          Selector_Name => Make_Identifier (Loc, Subprogram_Id_Name));
   end SP_Subprogram_Id;

   --------------------------------
   -- SP_Get_Local_Partition_Id --
   --------------------------------

   function SP_Get_Local_Partition_Id (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => System_PInterface (Loc),
          Selector_Name => Make_Identifier (Loc, Get_Local_Partition_Id_Name));
   end SP_Get_Local_Partition_Id;

   --------------------------------
   -- SP_Register_Receiving_Stub --
   --------------------------------

   function SP_Register_Receiving_Stub
     (Loc  : Source_Ptr)
      return Node_Id
   is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => System_PInterface (Loc),
          Selector_Name =>
            Make_Identifier (Loc, Register_Receiving_Stub_Name));
   end SP_Register_Receiving_Stub;

   --------------------------------
   -- RC_Get_Active_Partition_Id --
   --------------------------------

   function RC_Get_Active_Partition_Id (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => Make_Identifier (Loc, RCI_Cache_Name),
          Selector_Name =>
            Make_Identifier (Loc, Get_Active_Partition_Id_Name));
   end RC_Get_Active_Partition_Id;

   ---------------------------------
   -- RC_Get_RCI_Package_Receiver --
   ---------------------------------

   function RC_Get_RCI_Package_Receiver (Loc : Source_Ptr) return Node_Id is
   begin
      return
        Make_Selected_Component (Loc,
          Prefix => Make_Identifier (Loc, RCI_Cache_Name),
          Selector_Name =>
            Make_Identifier (Loc, Get_RCI_Package_Receiver_Name));
   end RC_Get_RCI_Package_Receiver;

   -----------------------
   -- AStub_Param_Specs --
   -----------------------

   function AStub_Param_Specs (Loc : Source_Ptr) return List_Id is
   begin
      return
        New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Params_Name),
            Parameter_Type =>
              Make_Access_Definition (Loc,
                Subtype_Mark => SR_Params_Stream_Type (Loc))));
   end AStub_Param_Specs;

   -----------------------
   -- NStub_Param_Specs --
   -----------------------

   function NStub_Param_Specs (Loc : Source_Ptr) return List_Id is
   begin
      return
        New_List (
          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Params_Name),
            Parameter_Type =>
              Make_Access_Definition (Loc,
                Subtype_Mark => SR_Params_Stream_Type (Loc))),

          Make_Parameter_Specification (Loc,
            Defining_Identifier => Make_Defining_Identifier (Loc, Result_Name),
            Parameter_Type =>
              Make_Access_Definition (Loc,
                Subtype_Mark => SR_Params_Stream_Type (Loc))));
   end NStub_Param_Specs;

   ----------------------------
   -- Build_Parent_Full_Name --
   ----------------------------

   function Build_Parent_Full_Name (P : Node_Id) return Node_Id is
      Loc   : constant Source_Ptr := Sloc (P);
      P_Ref : constant Node_Id    :=
                New_Reference_To (Defining_Entity (P), Loc);

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

   ----------------------------------
   -- Build_Passive_Partition_Stub --
   ----------------------------------

   function Build_Passive_Partition_Stub
     (Comp      : Node_Id;
      Is_Client : Boolean := False)
      return      Node_Id
   is
      Loc          : Source_Ptr;
      P            : Node_Id := Unit (Comp);
      Address_Id   : Entity_Id;
      Base_Address : Entity_Id;
      Cleanup_Id   : Entity_Id;
      Comp_Unit    : Node_Id;
      Decl         : Node_Id;
      Decls        : List_Id := Visible_Declarations (Specification (P));
      Object_Id    : Entity_Id;
      Prev_Id      : Entity_Id;
      Prev_Address : Entity_Id;
      New_Decls    : List_Id := New_List;
      New_Decl     : Node_Id;
      Pack_Decl    : Node_Id;
      Pack_Id      : Entity_Id;
      Shm_Cleanup  : Name_Id   := Get_Name_Id ("Cleanup");
      Shm_Id       : Name_Id   := Get_Name_Id ("Shm_Id");
      Shm_Serv     : Name_Id   := Get_Name_Id ("Shm_services");
      Shm_Types    : Name_Id   := Get_Name_Id ("Shm_types");
      Stor_Elem    : Name_Id   := Get_Name_Id ("Storage_elements");
      S_Create     : Name_Id   := Get_Name_Id ("Create");
      S_Map        : Name_Id   := Get_Name_Id ("Map");
      S_Look       : Name_Id   := Get_Name_Id ("Lookup");
      S_Reg        : Name_Id   := Get_Name_Id ("Register_f");
      S_Next_Addr  : Name_Id   := Get_Name_Id ("Next_address");
      Tmp_Id       : Entity_Id;
      Pkg_String   : String_Id := Get_Pkg_Name_String_Id (P);

      function Build_With_Clauses return List_Id;
      --  Add with_clauses for system and for shared memory utilities to
      --  the given context of the unit.

      procedure Emit_Address_Decl;
      --  Build declaration for constant that holds address of current object.

      procedure Emit_Address_Clause;
      --  Build address representation clause for current object.

      procedure Emit_Base_Address_Decl;
      --  Build declaration for Base address of passive partition, used
      --  as origin to compute addresses of all objects within.

      procedure Emit_Pack_Id;
      --  Emit declaration for Id used to establish shared memory address.

      -----------------------
      -- Emit_Address_Decl --
      -----------------------

      procedure Emit_Address_Decl is
         Offset   : Node_Id;

      begin
         --  _Object1_Address : constant System.Address :=
         --     Shm_Services.Next_Address
         --        (Previous_Address, Previous_Object'Size);

         if Present (Prev_Id) then
            Offset :=
              Make_Function_Call (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix        => Make_Identifier (Loc, Shm_Serv),
                    Selector_Name => Make_Identifier (Loc, S_Next_Addr)),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Prev_Address, Loc),
                  Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Prev_Id, Loc),
                     Attribute_Name => Name_Size)));

         else
            Offset := New_Occurrence_Of (Base_Address, Loc);
         end if;

         Append_To (New_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Address_Id,
             Constant_Present    => True,
             Object_Definition   =>
               Make_Selected_Component (Loc,
                  Prefix        => Make_Identifier (Loc, Name_System),
                  Selector_Name => Make_Identifier (Loc, Name_Address)),
             Expression => Offset));
      end Emit_Address_Decl;

      -------------------------
      -- Emit_Address_Clause --
      -------------------------

      procedure Emit_Address_Clause is
      begin
         Append_To (New_Decls,
            Make_Attribute_Definition_Clause (Loc,
              Name => New_Occurrence_Of (Object_Id, Loc),
              Chars => Name_Address,
              Expression => New_Occurrence_Of (Address_Id, Loc)));
      end Emit_Address_Clause;

      ----------------------------
      -- Emit_Base_Address_Decl --
      ----------------------------

      procedure Emit_Base_Address_Decl is
      begin
         Append_To (New_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Base_Address,
             Object_Definition =>
               Make_Selected_Component (Loc,
                  Prefix        => Make_Identifier (Loc, Name_System),
                  Selector_Name => Make_Identifier (Loc, Name_Address)),
             Expression =>
               Make_Function_Call (Loc,
                 Name =>
                   Make_Selected_Component (Loc,
                     Prefix        => Make_Identifier (Loc, Shm_Serv),
                     Selector_Name => Make_Identifier (Loc, S_Map)),
                 Parameter_Associations =>
                   New_List (New_Occurrence_Of (Pack_Id, Loc)))));
      end Emit_Base_Address_Decl;

      ------------------
      -- Emit_Pack_Id --
      ------------------

      procedure Emit_Pack_Id is
         Expr : Node_Id;
         Decl : Node_Id;

      begin
         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Pack_Id,
             Object_Definition =>
               Make_Selected_Component (Loc,
                  Prefix        => Make_Identifier (Loc, Shm_Types),
                  Selector_Name => Make_Identifier (Loc, Shm_Id)));

         if Is_Client then
            Expr :=
              Make_Function_Call (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix        => Make_Identifier (Loc, Shm_Serv),
                    Selector_Name => Make_Identifier (Loc, S_Look)),
                Parameter_Associations =>
                  New_List (Make_String_Literal (Loc, Pkg_String)));
         else
            Expr :=
              Make_Function_Call (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix        => Make_Identifier (Loc, Shm_Serv),
                    Selector_Name => Make_Identifier (Loc, S_Create)),
                Parameter_Associations =>
                  New_List
                   (Make_Integer_Literal (Loc, UI_From_Int (4096))));
         end if;

         Set_Expression (Decl, Expr);
         Append_To (New_Decls, Decl);
      end Emit_Pack_Id;

      ------------------------
      -- Build_With_Clauses --
      ------------------------

      --  Builds: with system, shm_types, shm_elaboration;

      function Build_With_Clauses return List_Id is
         Clauses : List_Id := New_List_Copy (Context_Items (Parent (P)));

      begin
         Add_With_Clause
          (Make_Selected_Component (Loc,
            Prefix        => Make_Identifier (Loc, Name_System),
            Selector_Name => Make_Identifier (Loc, Stor_Elem)),
          Clauses);
         Add_With_Clause (Make_Identifier (Loc, Shm_Serv), Clauses);
         Add_With_Clause (Make_Identifier (Loc, Shm_Types), Clauses);
         Add_With_Clause (Make_Identifier (Loc, Name_System), Clauses);

         return Clauses;
      end Build_With_Clauses;

   --  Start of processing for Build_Passive_Partition_Stub

   begin
      Loc := Sloc (P);
      Pack_Id :=
        New_Internal_Entity (E_Constant, Current_Scope, Loc, 'P');

      Base_Address :=
        New_Internal_Entity (E_Constant, Current_Scope, Loc, 'B');

      Emit_Pack_Id;
      Emit_Base_Address_Decl;
      Prev_Id := Empty;

      Decl := First (Decls);

      while Present (Decl) loop
         Loc := Sloc (Decl);

         if Nkind (Decl) = N_Object_Declaration then
            Object_Id := Defining_Identifier (Decl);
            Address_Id :=
              New_Internal_Entity (E_Constant, Current_Scope, Loc, 'A');
            Emit_Address_Decl;
            New_Decl := New_Copy (Decl);

            if Is_Client then
               Set_Expression (New_Decl, Empty);
            end if;

            Append_To (New_Decls, New_Decl);
            Emit_Address_Clause;
            Prev_Id      := Object_Id;
            Prev_Address := Address_Id;

         elsif Is_Client then
            --  Other declarations are added to the client stub as they are.
            --  Semantic checks already guarantee that the declarations
            --  are legal for a shared passive partition.

            Append (New_Copy_Tree (Decl), New_Decls);
         end if;

         Decl := Next (Decl);
      end loop;

      if not Is_Client then

         --  Register to the name server

         Tmp_Id :=
           New_Internal_Entity (E_Constant, Current_Scope, Loc, 'T');

         Append_To (New_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tmp_Id,
             Constant_Present    => True,
             Object_Definition   =>
               Make_Selected_Component (Loc,
                 Prefix        => Make_Identifier (Loc, Name_System),
                 Selector_Name => Make_Identifier (Loc, Name_Address)),
             Expression =>
               Make_Function_Call (Loc,
                 Name =>
                   Make_Selected_Component (Loc,
                     Prefix => Make_Identifier (Loc, Shm_Serv),
                     Selector_Name => Make_Identifier (Loc, S_Reg)),
                 Parameter_Associations =>
                   New_List (
                     Make_String_Literal (Loc, Pkg_String),
                     New_Occurrence_Of (Pack_Id, Loc)))));

         --  Insert Cleanup Object

         Cleanup_Id :=
           New_Internal_Entity (E_Constant, Current_Scope, Loc, 'C');

         Append_To (New_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Cleanup_Id,
             Object_Definition   =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   Make_Selected_Component (Loc,
                     Prefix        => Make_Identifier (Loc, Shm_Serv),
                     Selector_Name => Make_Identifier (Loc, Shm_Cleanup)),
                 Constraint =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints => New_List (Pack_Id)))));

      end if;

      Pack_Decl := Make_Package_Declaration (Loc,
         Specification =>
           Make_Package_Specification (Loc,
             Defining_Unit_Name =>
               Make_Defining_Identifier (Loc,
                 Chars => Chars (Defining_Unit_Name (Specification (P)))),
             Visible_Declarations => New_Decls));

      Comp_Unit :=
        Make_Compilation_Unit (Loc,
          Context_Items => Build_With_Clauses,
          Unit => Pack_Decl);

      return Comp_Unit;
   end Build_Passive_Partition_Stub;

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
             Chars  => Chars (U_Name),
             Prefix => Build_Parent_Full_Name (Unit (Parent_Spec (U))),
             Selector_Name => New_Reference_To (U_Name, Loc));

         Set_Entity (Result, U_Name);
         return Result;
      end if;
   end Build_Unit_Full_Name;

   ---------------------
   -- Full_Etype_Name --
   ---------------------

   function Full_Etype_Name (N : Node_Id; Ref : Node_Id) return Node_Id is
   begin
      if Nkind (N) in N_Has_Etype
        and then
          Enclosing_Lib_Unit_Node (Etype (N)) /= Enclosing_Lib_Unit_Node (Ref)
      then
         return Full_Qualified_Name_Node (Etype (N));
      end if;
      return N;
   end Full_Etype_Name;

   ------------------------------
   -- Full_Qualified_Name_Node --
   ------------------------------

   function Full_Qualified_Name_Node (E : Entity_Id) return Node_Id is
      Loc         : Source_Ptr := Sloc (E);
      Ent         : Entity_Id := E;
      Parent_Name : Node_Id := Empty;

   begin
      --  Deals properly with child units

      if Nkind (Ent) = N_Defining_Program_Unit_Name then
         Ent := Defining_Identifier (Ent);
      end if;

      --  Compute recursively the qualification. Only "Standard" has no scope.

      if Present (Scope (Scope (Ent))) then
         Parent_Name := Full_Qualified_Name_Node (Scope (Ent));
      end if;

      --  Every entity should have a name except some expanded blocks
      --  don't bother about those.

      if Chars (Ent) = No_Name then
         return Parent_Name;
      end if;

      --  return the full name node

      if Present (Parent_Name) then
         return
           Make_Selected_Component (Loc,
             Prefix => Parent_Name,
             Selector_Name =>
               Make_Defining_Identifier (Loc, Chars (Ent)));
      else
         return Make_Identifier (Loc, Chars (Ent));
      end if;
   end Full_Qualified_Name_Node;

   -----------------
   -- Get_Name_Id --
   -----------------

   function Get_Name_Id (Name : String) return Name_Id is
   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      return Name_Find;
   end Get_Name_Id;

   ----------------------------
   -- Get_Pkg_Name_string_id --
   ----------------------------

   function Get_Pkg_Name_String_Id (Decl_Node : Node_Id) return String_Id is
      Unit_Name_Id : Unit_Name_Type := Get_Unit_Name (Decl_Node);

   begin
      Get_Unit_Name_String (Unit_Name_Id);

      --  Remove seven last character ("(spec)" or " (body)").

      Name_Len := Name_Len - 7;
      return Get_String_Id (Name_Buffer (1 .. Name_Len));
   end Get_Pkg_Name_String_Id;

   -------------------
   -- Get_String_Id --
   -------------------

   function Get_String_Id (Val : String) return String_Id is
   begin
      Start_String;
      Store_String_Chars (Val);
      return End_String;
   end Get_String_Id;

   --------------------
   -- Has_Pragma_RCI --
   --------------------

   function Has_Pragma_RCI (L : List_Id) return Boolean is
      Decl : Node_Id;

   begin
      if Present (L) then
         Decl := First (L);
         while Present (Decl)
           and then (Nkind (Decl) /= N_Pragma
                      or else Chars (Decl) /= Name_Remote_Call_Interface)
         loop
            Decl := Next (Decl);
         end loop;

         if Present (Decl) then
            return True;
         end if;
      end if;

      return False;
   end Has_Pragma_RCI;

   ----------------------
   -- Has_Unknown_Size --
   ----------------------

   function Has_Unknown_Size (E : Entity_Id) return Boolean is
   begin
      return Has_Unknown_Discriminants (E) or else
        ((Is_Array_Type (E) or Is_Record_Type (E) or Is_String_Type (E))
         and then not Is_Constrained (E));
   end Has_Unknown_Size;

   ---------------------------------
   -- Is_ACW_Limited_Private_Type --
   ---------------------------------

   function Is_ACWLP_Type (E : Entity_Id) return Boolean is
      DD : Node_Id;
      ED : Node_Id;
   begin
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
   end Is_ACWLP_Type;

   ---------------------------
   -- Is_RCI_Pkg_Decl_Cunit --
   ---------------------------

   function Is_RCI_Pkg_Decl_Cunit (Cunit : Node_Id) return Boolean is
      The_Unit : constant Node_Id := Unit (Cunit);

   begin
      if Nkind (The_Unit) /= N_Package_Declaration then
         return false;
      end if;

      if Analyzed (The_Unit) then
         return Is_Remote_Call_Interface (Defining_Entity (The_Unit));
      end if;

      return
        (Has_Pragma_RCI (Visible_Declarations (Specification (The_Unit)))
          or else Has_Pragma_RCI (Pragmas_After (Cunit)));
   end Is_RCI_Pkg_Decl_Cunit;

   -----------------------------
   -- Is_RCI_Pkg_Spec_Or_Body --
   -----------------------------

   function Is_RCI_Pkg_Spec_Or_Body (Cunit : Node_Id) return Boolean is
   begin
      return Is_RCI_Pkg_Decl_Cunit (Cunit)
        or else
         (Nkind (Unit (Cunit)) = N_Package_Body
           and then Is_RCI_Pkg_Decl_Cunit (Library_Unit (Cunit)));
   end Is_RCI_Pkg_Spec_Or_Body;

   --------------------------------------
   -- Build_Calling_Stubs_Bodies_Cunit --
   --------------------------------------

   function Build_Calling_Stubs_Bodies_Cunit
     (RCI_Cunit : Node_Id)
      return      Node_Id
   is
      --  Features needed from the input compilation unit

      Loc                 : Source_Ptr := Sloc (RCI_Cunit);
      RCI_Decl            : Node_Id    := Unit (RCI_Cunit);
      RCI_Spec            : Node_Id    := Specification (RCI_Decl);
      RCI_Pkg_Name        : Node_Id    := Defining_Unit_Name (RCI_Spec);
      RCI_Pkg_Name_String : String_Id  := Get_Pkg_Name_String_Id (RCI_Decl);

      --  Features for the stub package body to create

      Package_Body        : Node_Id;
      Package_Body_CItems : List_Id;
      Package_Body_Decls  : List_Id := New_List;

      --  List of local names needed

      Receiver_Name         : Name_Id := Get_Name_Id ("receiver");
      Partition_Name        : Name_Id := Get_Name_Id ("partition");
      Done_Name             : Name_Id := Get_Name_Id ("done");
      Boolean_Name          : Name_Id := Get_Name_Id ("boolean");
      True_Name             : Name_Id := Get_Name_Id ("true");
      False_Name            : Name_Id := Get_Name_Id ("false");
      Get_RCI_Data_Name     : Name_Id := Get_Name_Id ("get_rci_data");
      Set_RCI_Data_Name     : Name_Id := Get_Name_Id ("set_rci_data");
      Elaborated_Name       : Name_Id := Get_Name_Id ("elaborated");
      In_Progress_Name      : Name_Id := Get_Name_Id ("in_progress");
      Active_Partition_Name : Name_Id := Get_Name_Id ("active_partition");
      Package_Receiver_Name : Name_Id := Get_Name_Id ("package_receiver");

      --  A number is given to each subprogram which is callable remotely;
      --  it will be used together with the Package Id to compute the
      --  corresponding Service_ID.

      Subp_Num : Int := 0;
      Racw_Num : Int := 0;
      Ras_Num  : Int := 0;

   --  Start of processing for Build_Calling_Stubs_Cunits

   begin
      --  Build the context items

      Package_Body_CItems := New_List (
        Make_With_Clause (Loc, Ada_Streams (Loc)),
        Make_With_Clause (Loc, Ada_Exceptions (Loc)),
        Make_With_Clause (Loc, System_Rpc (Loc)),
        Make_With_Clause (Loc, System_PInterface (Loc)));

      --  Build the stub subprogram bodies

      Build_Calling_Stubs_Pkg_Body
        (RCI_Decl,
         Subp_Num,
         Racw_Num,
         Ras_Num,
         Package_Body_CItems,
         Package_Body);

      Package_Body_Decls := Declarations (Package_Body);

      --  Build and prepend the declaration of the protected object used for
      --  elaboration control

      Prepend_To (Package_Body_Decls,
        Make_Package_Instantiation (Loc,
          Defining_Unit_Name =>
            Make_Defining_Identifier (Loc, RCI_Cache_Name),
          Name               =>
            SP_RCI_Info (Loc),
          Generic_Associations =>
            New_List (Make_String_Literal
                      (Loc,
                       Get_Pkg_Name_String_Id
                       (Defining_Unit_Name
                         (Specification (Unit (RCI_Cunit))))))));

      --  Add unchecked_conversion to the context clause and add a
      --  use type clause for System.RPC.Partition_ID

      if Racw_Num /= 0 or else Ras_Num /= 0 then
         Add_With_Clause (Make_Identifier (Loc, Name_Unchecked_Conversion),
           Package_Body_CItems);
         Prepend_To (Package_Body_Decls,
           Make_Use_Type_Clause (Loc,
             Subtype_Marks => New_List (SR_Partition_ID (Loc))));
      end if;

      --  Build the compilation unit for the calling stubs package body

      return
        Make_Compilation_Unit (Loc,
          Context_Items => Package_Body_CItems,
          Unit => Package_Body);

   end Build_Calling_Stubs_Bodies_Cunit;

   ----------------------------------
   -- Build_Calling_Stubs_Pkg_Body --
   ----------------------------------

   procedure Build_Calling_Stubs_Pkg_Body
     (Pkg_Decl       : in Node_Id;
      Last_Stub_Num  : in out Int;
      Last_Racw_Num  : in out Int;
      Last_Ras_Num   : in out Int;
      Stubs_CItems   : in List_Id;
      Stubs_Pkg_Body : out Node_Id)
   is
      --  Features needed from the input declaration

      Loc       : Source_Ptr := Sloc (Pkg_Decl);
      Spec      : Node_Id    := Specification (Pkg_Decl);
      Vis_Decls : List_Id    := Visible_Declarations (Spec);
      Pkg_Name  : Node_Id    := Defining_Unit_Name (Spec);
      Vis_Decl  : Node_Id;

      --  Features for the stub package body to create

      Package_Body_Decls   : List_Id := New_List;
      Inner_Stubs_Pkg_Body : Node_Id;

      -----------------------
      -- Local Subprograms --
      -----------------------

      function Build_Async_Calling_Stub_Body
        (Vis_Decl : Node_Id)
         return     Node_Id;
      --  Builds the body of the calling stub for an asynchronous remote call
      --  interface subprogram. The input parameter is supposed to be the
      --  non-empty declaration node of the subprogram.

      function Build_Calling_Stub_Body (Vis_Decl : Node_Id) return Node_Id;
      --  Builds the body of the calling stub for a non-asynchronous remote
      --  call interface subprogram. The input parameter is supposed to be
      --  the non-empty declaration node of the subprogram.

      -----------------------------------
      -- Build_Async_Calling_Stub_Body --
      -----------------------------------

      function Build_Async_Calling_Stub_Body (Vis_Decl : Node_Id)
        return Node_Id
      is
         --  Information needed from the input parameter

         Subp_Spec   : Node_Id := Specification (Vis_Decl);
         Param_Specs : List_Id := Parameter_Specifications (Subp_Spec);
         Subp_Name   : Node_Id := Defining_Unit_Name (Subp_Spec);
         Param_Spec  : Node_Id;

         --  Building new entities for the local identifiers

         Stream_In              : Entity_Id;

         --  Features for the stub body to create

         Stmts       : List_Id := New_List;
         Write_Stmts : List_Id := New_List;
         Decls       : List_Id := New_List;
         Stub_Body   : Node_Id;

         Stream_Decl   : Node_Id;
         --  Variable for the declaration node of a stream

      begin
         --  Initialization of the local entities

         Stream_In :=
           Make_Defining_Identifier (Loc, Stream_In_Name);

         --  Build and append stream input declaration to the list of
         --  declarations of the stub body

         Stream_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => New_Reference_To (Stream_In, Loc),
             Object_Definition =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => SR_Params_Stream_Type (Loc),

               Constraint =>
                 Make_Index_Or_Discriminant_Constraint (Loc,
                   Constraints =>
                     New_List (Make_Integer_Literal (Loc, Uint_0)))));

         Set_Aliased_Present (Stream_Decl);
         Append (Stream_Decl, Decls);

         --  Build and append the write statement for the Package_Receiver, to
         --  the list of statements of the stub body

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => SR_RPC_Receiver (Loc),
             Attribute_Name => Name_Write,

             Expressions => New_List (

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               Make_Function_Call (Loc,
                 Name => RC_Get_RCI_Package_Receiver (Loc)))));

         --  Write statement for the subprogram identifier

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => SP_Subprogram_Id (Loc),
             Attribute_Name => Name_Write,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               --  Type conversion necessary ???

               --  Make_Type_Conversion (Loc,
                  --  SP_Subprogram_Id (Loc),

               Make_Integer_Literal (Loc, UI_From_Int (Last_Stub_Num)))));

         --  Append the write statements for the in parameters

         if Param_Specs /= No_List then
            Param_Spec := First (Param_Specs);
            while Present (Param_Spec) loop

               if Has_Unknown_Size (Etype (Parameter_Type (Param_Spec))) then

                  Append_To (Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix => Parameter_Type (Param_Spec),
                      Attribute_Name => Name_Output,

                      Expressions =>
                        New_List (
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Reference_To (Stream_In, Loc),
                            Attribute_Name => Name_Unchecked_Access),

                          Make_Identifier (Loc,
                            Chars =>
                              Chars (Defining_Identifier (Param_Spec))))));

               else
                  Append_To (Write_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix => Parameter_Type (Param_Spec),
                      Attribute_Name => Name_Write,

                      Expressions =>
                        New_List (
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Reference_To (Stream_In, Loc),
                            Attribute_Name => Name_Unchecked_Access),

                          Make_Identifier (Loc,
                            Chars =>
                              Chars (Defining_Identifier (Param_Spec))))));
               end if;

               Param_Spec := Next (Param_Spec);
            end loop;
         end if;

         Append_List (Write_Stmts, Stmts);

         --  append do_apc call to the list of statements

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => SR_Do_Apc (Loc),
             Parameter_Associations => New_List (
               Make_Function_Call (Loc,
                 Name => RC_Get_Active_Partition_Id (Loc)),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access))));

         --  Build the stub body node

         Stub_Body :=
           Make_Subprogram_Body (Loc,
             Specification => Copy_Original_Tree (Subp_Spec),
             Declarations => Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => Stmts));

         return Stub_Body;

      end Build_Async_Calling_Stub_Body;

      ----------------------------
      -- Build_Calling_Stub_Body --
      ----------------------------

      function Build_Calling_Stub_Body (Vis_Decl : Node_Id) return Node_Id is

         --  Information needed from the input parameter

         Subp_Spec   : Node_Id := Specification (Vis_Decl);
         Param_Specs : List_Id := Parameter_Specifications (Subp_Spec);
         Subp_Name   : Node_Id := Defining_Unit_Name (Subp_Spec);
         Param_Spec  : Node_Id;

         --  Building new entities for the local identifiers

         Stream_In              : Entity_Id;
         Stream_Out             : Entity_Id;
         Returned_Val           : Entity_Id;
         Except                 : Entity_Id;

         --  Features for the stub body to create

         Stmts        : List_Id := New_List;
         Write_Stmts  : List_Id := New_List;
         Output_Stmts : List_Id := New_List;
         Decls        : List_Id := New_List;
         Stub_Body    : Node_Id;

         --  Variable for the declaration node of a stream

         Stream_Decl   : Node_Id;

      begin
         --  Initialization of the local entities

         Stream_In    :=
           Make_Defining_Identifier (Loc, Stream_In_Name);

         Stream_Out   :=
           Make_Defining_Identifier (Loc, Stream_Out_Name);

         Returned_Val :=
           Make_Defining_Identifier (Loc, Returned_Val_Name);

         Except       :=
           Make_Defining_Identifier (Loc, Except_Name);

         --  Build and append stream input declaration to the list of
         --  declarations of the stub body

         Stream_Decl :=
            Make_Object_Declaration (Loc,
              Defining_Identifier => New_Reference_To (Stream_In, Loc),
              Object_Definition =>
                Make_Subtype_Indication (Loc,
                  Subtype_Mark => SR_Params_Stream_Type (Loc),

                Constraint =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints =>
                      New_List (Make_Integer_Literal (Loc, Uint_0)))));

         Set_Aliased_Present (Stream_Decl);
         Append (Stream_Decl, Decls);

         --  Build and append stream output declaration to the list of
         --  declarations of the stub body

         Stream_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier =>
               New_Reference_To (Stream_Out, Loc),

             Object_Definition =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => SR_Params_Stream_Type (Loc),

                 Constraint =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints =>
                       New_List (Make_Integer_Literal (Loc, Uint_0)))));

         Set_Aliased_Present (Stream_Decl);
         Append (Stream_Decl, Decls);

         --  Append the declaration for the exeption occurrence

         Append_To (Decls,
            Make_Object_Declaration (Loc,
              Defining_Identifier => Except,
              Object_Definition => AE_Exception_Occurrence (Loc)));

         --  Append the declaration for the returned value in the
         --  case of a function

         if Nkind (Subp_Spec) = N_Function_Specification
           and then not Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec)))
         then
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Returned_Val,
                Object_Definition =>
                  Copy_Original_Tree (Subtype_Mark (Subp_Spec))));
         end if;

         --  Build and append the write statement for the Package_Receiver, to
         --  the list of statements of the stub body

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => SR_RPC_Receiver (Loc),
             Attribute_Name => Name_Write,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               Make_Function_Call (Loc,
                 Name => RC_Get_RCI_Package_Receiver (Loc)))));

         --  Write statement for the subprogram identifier

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => SP_Subprogram_Id (Loc),
             Attribute_Name => Name_Write,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               --  Type conversion necessary ???

               --  Make_Type_Conversion (Loc,
                  --  SP_Subprogram_Id (Loc),

               Make_Integer_Literal (Loc, UI_From_Int (Last_Stub_Num)))));

         --  Append the write statements for the in parameters
         --  and the read statements for the out parameters

         if Param_Specs /= No_List then

            Param_Spec := First (Param_Specs);

            while Present (Param_Spec) loop

               if Has_Unknown_Size (Etype
                 (Parameter_Type (Param_Spec)))
               then

                  Append_To (Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix => Parameter_Type (Param_Spec),
                      Attribute_Name => Name_Output,

                      Expressions =>
                        New_List (
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Reference_To (Stream_In, Loc),
                            Attribute_Name => Name_Unchecked_Access),

                          Make_Identifier (Loc,
                            Chars =>
                              Chars (Defining_Identifier (Param_Spec))))));

               elsif In_Present (Param_Spec)
                 or else not Out_Present (Param_Spec)
               then
                  Append_To (Write_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix => Parameter_Type (Param_Spec),
                      Attribute_Name => Name_Write,

                      Expressions =>
                        New_List (
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Reference_To (Stream_In, Loc),
                            Attribute_Name => Name_Unchecked_Access),
                          Make_Identifier (Loc,
                            Chars =>
                              Chars (Defining_Identifier (Param_Spec))))));
               end if;

               if Out_Present (Param_Spec) then

                  --  Read operation are within an if statement and
                  --  are thus appended to a then statement list which
                  --  will be used later to build the if statement.

                  if Has_Unknown_Size (Etype (
                    Parameter_Type (Param_Spec)))
                  then
                     Append_To (Output_Stmts,
                       Make_Assignment_Statement (Loc,

                         Name =>
                           Make_Identifier (Loc,
                             Chars =>
                               Chars (Defining_Identifier (Param_Spec))),

                         Expression =>
                           Make_Function_Call (Loc,
                             Name =>
                               Make_Attribute_Reference (Loc,
                                 Prefix => Parameter_Type (Param_Spec),
                                 Attribute_Name => Name_Input),

                             Parameter_Associations => New_List (
                               Make_Attribute_Reference (Loc,
                                 Prefix =>
                                   New_Reference_To (Stream_Out, Loc),
                                 Attribute_Name =>
                                   Name_Unchecked_Access)))));

                  else
                     Append_To (Output_Stmts,
                       Make_Attribute_Reference (Loc,
                         Prefix => Parameter_Type (Param_Spec),
                         Attribute_Name => Name_Read,

                         Expressions =>
                           New_List (
                             Make_Attribute_Reference (Loc,
                               Prefix => New_Reference_To (Stream_Out, Loc),
                               Attribute_Name => Name_Unchecked_Access),

                             Make_Identifier (Loc,
                               Chars =>
                                 Chars (Defining_Identifier (Param_Spec))))));
                  end if;
               end if;

               Param_Spec := Next (Param_Spec);
            end loop;
         end if;

         --  Append the write statement list to the list of statements

         Append_List (Write_Stmts, Stmts);

         --  Append Do_Rpc call to the list of statements

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => SR_Do_Rpc (Loc),
             Parameter_Associations => New_List (

               Make_Function_Call (Loc,
                 Name => RC_Get_Active_Partition_Id (Loc)),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_In, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_Out, Loc),
                 Attribute_Name => Name_Unchecked_Access))));

         --  Append the read operation for the exception occurrence
         --  and the call to Reraise_Occurrence

         Append_To (Stmts,
           Make_Attribute_Reference (Loc,
             Prefix => AE_Exception_Occurrence (Loc),
             Attribute_Name => Name_Read,

             Expressions => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Stream_Out, Loc),
                 Attribute_Name => Name_Unchecked_Access),

               New_Reference_To (Except, Loc))));

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => AE_Reraise_Occurrence (Loc),
               Parameter_Associations => New_List (
                 New_Reference_To (Except, Loc))));

         --  Append the read statement for the returned value, and
         --  the return statement in the case of a function

         if Nkind (Subp_Spec) = N_Function_Specification then

            if Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec))) then

               Append_To (Output_Stmts,
                 Make_Return_Statement (Loc,
                   Expression =>
                     Make_Function_Call (Loc,
                       Name =>
                         Make_Attribute_Reference (Loc,
                           Prefix => Subtype_Mark (Subp_Spec),
                           Attribute_Name => Name_Input),

                       Parameter_Associations => New_List (
                         Make_Attribute_Reference (Loc,
                           Prefix => New_Reference_To (Stream_Out, Loc),
                           Attribute_Name => Name_Unchecked_Access)))));

            else
               Append_To (Output_Stmts,
                 Make_Attribute_Reference (Loc,
                   Prefix => Copy_Original_Tree (Subtype_Mark (Subp_Spec)),
                   Attribute_Name => Name_Read,

                   Expressions => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix => New_Reference_To (Stream_Out, Loc),
                       Attribute_Name => Name_Unchecked_Access),

                     New_Reference_To (Returned_Val, Loc))));

               --  Append a return statement; the returned value is the one
               --  previously read from the stream output

               Append_To (Output_Stmts,
                 Make_Return_Statement (Loc,
                   Expression => New_Reference_To (Returned_Val, Loc)));
            end if;
         end if;

         --  Append the statements for the out parameters and
         --  the returned value in the case of a function

         if not Is_Empty_List (Output_Stmts) then
            Append_List (Output_Stmts, Stmts);
         end if;

         --  Build the stub body node

         Stub_Body :=
            Make_Subprogram_Body (Loc,
              Specification => Copy_Original_Tree (Subp_Spec),
              Declarations  => Decls,
              Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements => Stmts));

         return Stub_Body;

      end Build_Calling_Stub_Body;

   --  Start of processing for Build_Calling_Stubs

   begin
      --  Build the stub subprogram bodies

      if Vis_Decls /= No_List then
         Vis_Decl := First (Vis_Decls);

         while Present (Vis_Decl) loop
            if Comes_From_Source (Vis_Decl) then
               if Nkind (Vis_Decl) = N_Subprogram_Declaration then
                  Last_Stub_Num := Last_Stub_Num + 1;

                  if Nkind (Specification (Vis_Decl)) =
                              N_Procedure_Specification
                    and then Is_Asynchronous (Defining_Entity (Vis_Decl))
                  then
                     Append_To (Package_Body_Decls,
                       Build_Async_Calling_Stub_Body (Vis_Decl));

                  else
                     Append_To (Package_Body_Decls,
                       Build_Calling_Stub_Body (Vis_Decl));
                  end if;

               elsif Nkind (Vis_Decl) = N_Package_Declaration then

                  Build_Calling_Stubs_Pkg_Body
                    (Vis_Decl,
                     Last_Stub_Num,
                     Last_Racw_Num,
                     Last_Ras_Num,
                     Stubs_CItems,
                     Inner_Stubs_Pkg_Body);

                  Append (Inner_Stubs_Pkg_Body, Package_Body_Decls);

               elsif Nkind (Vis_Decl) = N_Full_Type_Declaration then
                  if Is_Remote_Access_To_Class_Wide_Type
                       (Defining_Identifier (Vis_Decl))
                  then
                     Last_Racw_Num := Last_Racw_Num + 1;
                     Add_Racw_Stubs (
                       Vis_Decl,
                       Package_Body_Decls,
                       Stubs_CItems,
                       Last_Racw_Num);

                  elsif Is_Remote_Access_To_Subprogram_Type
                          (Defining_Identifier (Vis_Decl))
                  then
                     Last_Ras_Num := Last_Ras_Num + 1;
                     Add_RAST_Features (Vis_Decl);
                  end if;
               else
                  --  See all the other features which are used
                  --  (types appearing directly or through a with, etc.)

                  null;
               end if;
            end if;

            Vis_Decl := Next (Vis_Decl);
         end loop;
      end if;

      --  Build the package body node

      Stubs_Pkg_Body :=
        Make_Package_Body (Loc,
          Defining_Unit_Name => Copy_Original_Tree (Pkg_Name),
          Declarations => Package_Body_Decls);

   end Build_Calling_Stubs_Pkg_Body;

   ----------------
   -- Init_Names --
   ----------------

   procedure Init_Names is
   begin

      --  External names

      Stream_Name                  := Get_Name_Id ("stream");
      Item_Name                    := Get_Name_Id ("item");
      Params_Name                  := Get_Name_Id ("params");
      Result_Name                  := Get_Name_Id ("result");
      RPC_Receiver_Name            := Get_Name_Id ("rpc_receiver");
      Params_Stream_Type_Name      := Get_Name_Id ("params_stream_type");
      Do_Rpc_Name                  := Get_Name_Id ("do_rpc");
      Do_Apc_Name                  := Get_Name_Id ("do_apc");
      Exceptions_Name              := Get_Name_Id ("exceptions");
      Exception_Occurrence_Name    := Get_Name_Id ("exception_occurrence");
      Null_Occurrence_Name         := Get_Name_Id ("null_occurrence");
      Reraise_Occurrence_Name      := Get_Name_Id ("reraise_occurrence");
      Subprogram_Id_Name           := Get_Name_Id ("subprogram_id");
      Get_Local_Partition_Id_Name  := Get_Name_Id ("get_local_partition_id");
      Get_Active_Partition_Id_Name := Get_Name_Id ("get_active_partition_id");
      Root_Stream_Type_Name        := Get_Name_Id ("root_stream_type");
      Stream_Element_Count_Name    := Get_Name_Id ("stream_element_count");
      Partition_Interface_Name     := Get_Name_Id ("partition_interface");
      Get_Passive_Partition_Id_Name
                                   := Get_Name_Id ("get_passive_partition_id");
      Get_RCI_Package_Receiver_Name
                                   := Get_Name_Id ("get_rci_package_receiver");
      Register_Receiving_Stub_Name := Get_Name_Id ("register_receiving_stub");
      RCI_Cache_Name               := Get_Name_Id ("rci_cache");
      RCI_Info_Name                := Get_Name_Id ("rci_info");

      --  Internal names

      Stream_In_Name    := New_Internal_Name ('S');
      Stream_Out_Name   := New_Internal_Name ('S');
      Returned_Val_Name := New_Internal_Name ('R');
      Except_Name       := New_Internal_Name ('E');

   end Init_Names;

   ----------------------------------------
   -- Build_Receiving_Stubs_Bodies_Cunit --
   ----------------------------------------

   function Build_Receiving_Stubs_Bodies_Cunit
    (RCI_Cunit : Node_Id)
     return      Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (RCI_Cunit);
      Unit_Node : constant Node_Id    := Unit (RCI_Cunit);

      --  Features for the compilation unit to create

      Pkg_Body_CItems : List_Id := No_List;
      Pkg_Body        : Node_Id;
      Stubs_Cunit     : Node_Id;
      Racw_Num        : Int := 0;
      Ras_Num         : Int := 0;

   --  Start of processing for Build_Receiving_Stubs_Bodies_Cunit

   begin
      --  Build context items for the package body

      if Nkind (Unit_Node) = N_Package_Body then
         Pkg_Body_CItems :=
           New_List_Copy (Context_Items (RCI_Cunit));
      end if;

      --  Add Ada.Exceptions, System.Rpc and System.Partition_interface
      --  to the withed list of the stub bodies package

      Add_With_Clause (Ada_Exceptions (Loc), Pkg_Body_CItems);
      Add_With_Clause (Ada_Streams (Loc), Pkg_Body_CItems);
      Add_With_Clause (System_Rpc (Loc), Pkg_Body_CItems);
      Add_With_Clause (System_PInterface (Loc), Pkg_Body_CItems);

      --  Build the package body

      Build_Receiving_Stubs_Pkg_Body
        (Unit_Node,
         Racw_Num,
         Ras_Num,
         Pkg_Body_CItems,
         Pkg_Body);

      --  Add unchecked_conversion to the context clause and
      --  add a use type clause for System.RPC.Partition_ID

      if Racw_Num /= 0 or else Ras_Num /= 0 then
         Add_With_Clause (Make_Identifier (Loc, Name_Unchecked_Conversion),
           Pkg_Body_CItems);
         Prepend_To (Declarations (Pkg_Body),
           Make_Use_Type_Clause (Loc,
             Subtype_Marks => New_List (SR_Partition_ID (Loc))));
      end if;

      --  Build the compilation unit for the receiving stubs package spec

      Stubs_Cunit :=
        Make_Compilation_Unit (Loc,
          Context_Items => Pkg_Body_CItems,
          Unit          => Pkg_Body);

      return Stubs_Cunit;
   end Build_Receiving_Stubs_Bodies_Cunit;

   ------------------------------------
   -- Build_Receiving_Stubs_Pkg_Body --
   ------------------------------------

   procedure Build_Receiving_Stubs_Pkg_Body
     (Unit_Node      : in Node_Id;
      Last_Racw_Num  : in out Int;
      Last_Ras_Num   : in out Int;
      Stubs_CItems   : in List_Id;
      Stubs_Pkg_Body : out Node_Id)
   is
      --  Features needed from the input body

      Loc      : Source_Ptr := Sloc (Unit_Node);
      Body_Hss : Node_Id;
      Spec     : Node_Id;

      --  Features needed from the corresponding specification

      Pkg_Decl   : Node_Id;
      Pkg_Name   : Node_Id;
      Vis_Decls  : List_Id;
      Priv_Decls : List_Id;

      --  Features for the receiving stubs package body to create

      Pkg_Body_Decls  : List_Id := New_List;
      Pkg_Body_Ss     : List_Id := No_List;
      Pkg_Body_Ehs    : List_Id := No_List;
      Elab_Stmt       : Node_Id;
      Stub_Body       : Node_Id;
      Param_Assocs    : List_Id;
      Pkg_Name_String : String_Id;

      --  This is used to build the local receiver procedure

      Receiver_Spec  : Node_Id;
      Receiver_Body  : Node_Id;
      Receiver_Decls : List_Id := New_List;
      Case_Stmt_Alts : List_Id := New_List;

      --  List of local names needed

      Receiving_Stub_Name       : Name_Id;
      Package_Rpc_Receiver_Name : Name_Id;

      Subp_Num_Name             : Name_Id := New_Internal_Name ('N');
      --  Name of the variable used to get the subprogram
      --  identifier from the stream output

      Subp_Num      : Int := 0;
      --  A number  is  given to  each subprogram which  is callable remotely;
      --  it will be used together with the  Package Id to compute the
      --  corresponding Service_ID.

      function Build_Async_Receiving_Stub_Body
        (Vis_Decl : Node_Id; Prefix : Node_Id)
         return Node_Id;
      --  Builds the body node of the receiving stub for an asynchronous
      --  procedure

      function Build_Receiving_Stub_Body (Vis_Decl : Node_Id; Prefix : Node_Id)
        return Node_Id;
      --  Builds the body node of the receiving stub for a regular subprogram.

      procedure Build_Receiving_Stubs
        (Pkg_Decl : Node_Id;
         Pkg_Body : Node_Id;
         Prefix   : Node_Id);
      --  Builds the receiving stubs for the subprograms of the specified list

      -------------------------------------
      -- Build_Async_Receiving_Stub_Body --
      -------------------------------------

      function Build_Async_Receiving_Stub_Body
        (Vis_Decl : Node_Id; Prefix : Node_Id)
         return Node_Id
      is
         --  Information needed from the input declaration

         Subp_Spec   : Node_Id := Specification (Vis_Decl);
         Param_Specs : List_Id := Parameter_Specifications (Subp_Spec);
         Subp_Name   : Node_Id := Defining_Unit_Name (Subp_Spec);
         Param_Spec  : Node_Id;
         Param       : Entity_Id;
         Param_Type  : Node_Id;

         --  New entities for the local identifiers

         Params      : Entity_Id;

         --  Features for the stub body to create

         Decls             : List_Id := New_List;
         Stmts             : List_Id := New_List;
         Hss               : Node_Id;
         Stub_Spec         : Node_Id;
         Stub_Body         : Node_Id;
         Param_List        : List_Id := New_List;
         Param_Read_Stmts  : List_Id := New_List;

      begin
         --  Initialization of the external entities

         Params :=
           Make_Defining_Identifier (Loc, Params_Name);

         --  Build the stub specification node

         Stub_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name =>
               Make_Identifier (Loc,
                 Chars =>
                   New_External_Name (
                     Related_Id   => Receiving_Stub_Name,
                     Suffix       => 'S',
                     Suffix_Index => Subp_Num)),
             Parameter_Specifications => AStub_Param_Specs (Loc));

         --  Build the stub body node

         if Param_Specs /= No_List then
            Param_Spec := First (Param_Specs);
            while Present (Param_Spec) loop

               Param_Type := Parameter_Type (Param_Spec);
               Param :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
               Append (Param, Param_List);

               --  For the moment we suppose that we have
               --  no access definition as parameter type ???

               if Has_Unknown_Size (Etype (Param_Type)) then

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        New_Reference_To (Param, Loc),
                      Object_Definition => Copy_Original_Tree (Param_Type),

                      Expression =>
                        Make_Function_Call (Loc,
                          Name => Make_Attribute_Reference (Loc,
                            Prefix => Copy_Original_Tree (Param_Type),
                            Attribute_Name => Name_Input),

                          Parameter_Associations =>
                            New_List (
                              Make_Identifier (Loc, Chars (Params))))));

               else
                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        New_Reference_To (Param, Loc),
                      Object_Definition => Copy_Original_Tree (Param_Type)));

                  Append_To (Param_Read_Stmts,
                    Make_Attribute_Reference (Loc,
                      Prefix => Copy_Original_Tree (Param_Type),
                      Attribute_Name => Name_Read,

                      Expressions =>
                        New_List (
                          Make_Identifier (Loc, Chars (Params)),
                          Make_Identifier (Loc, Chars (Param)))));
               end if;

               Param_Spec := Next (Param_Spec);
            end loop;
         end if;

         Stmts := Param_Read_Stmts;

         if Prefix /= Empty then
            Subp_Name :=
               Make_Selected_Component (Loc,
                  Prefix => Prefix,
                  Selector_Name => Subp_Name);
         end if;

         Append_To (Stmts,
           Make_Procedure_Call_Statement (Loc,
             Name => Copy_Original_Tree (Subp_Name),
             Parameter_Associations => Param_List));

         Hss :=
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => Stmts,
             Exception_Handlers => New_List (
               Make_Exception_Handler (Loc,
                 Exception_Choices => New_List (Make_Others_Choice (Loc)),
                 Statements => New_List (Make_Null_Statement (Loc)))));

         Stub_Body :=
           Make_Subprogram_Body (Loc,
             Specification => Copy_Original_Tree (Stub_Spec),
             Declarations => Decls,
             Handled_Statement_Sequence => Hss);

         return Stub_Body;
      end Build_Async_Receiving_Stub_Body;

      -------------------------------
      -- Build_Receiving_Stub_Body --
      -------------------------------

      function Build_Receiving_Stub_Body (Vis_Decl : Node_Id; Prefix : Node_Id)
        return Node_Id
      is
         --  Information needed from the input declaration

         Subp_Spec   : Node_Id := Specification (Vis_Decl);
         Param_Specs : List_Id := Parameter_Specifications (Subp_Spec);
         Subp_Name   : Node_Id := Defining_Unit_Name (Subp_Spec);
         Param_Spec  : Node_Id;
         Param       : Entity_Id;
         Param_Type  : Node_Id;

         --  New entities for the local identifiers

         Params        : Entity_Id;
         Result        : Entity_Id;
         Returned_Val  : Entity_Id;
         Except        : Entity_Id;

         --  Features for the stub body to create

         Decls             : List_Id := New_List;
         Stmts             : List_Id := New_List;
         Hss               : Node_Id;
         Stub_Spec         : Node_Id;
         Stub_Body         : Node_Id;
         Param_List        : List_Id := New_List;
         Param_Read_Stmts  : List_Id := New_List;
         Param_Write_Stmts : List_Id := New_List;

      begin
         --  Initialization of the local entities

         Params :=
           Make_Defining_Identifier (Loc, Params_Name);

         Result :=
           Make_Defining_Identifier (Loc, Result_Name);

         Returned_Val :=
           Make_Defining_Identifier (Loc, Returned_Val_Name);

         Except :=
           Make_Defining_Identifier (Loc, Except_Name);

         --  Build the stub specification node

         Stub_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name =>
               Make_Identifier (Loc,
                 Chars =>
                   New_External_Name (
                     Related_Id => Receiving_Stub_Name,
                     Suffix       => 'S',
                     Suffix_Index => Subp_Num)),
             Parameter_Specifications => NStub_Param_Specs (Loc));

         --  Build the stub body node

         if Param_Specs /= No_List then
            Param_Spec := First (Param_Specs);
            while Present (Param_Spec) loop
               Param :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
               Param_Type := Parameter_Type (Param_Spec);

               --  For the moment we suppose that we have
               --  no access definition as parameter type ???

               Append (Param, Param_List);

               if Has_Unknown_Size (Etype (Param_Type)) then

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        New_Reference_To (Param, Loc),

                      Object_Definition =>
                        Copy_Original_Tree (Param_Type),

                      Expression =>
                        Make_Function_Call (Loc,
                          Name =>
                            Make_Attribute_Reference (Loc,
                              Prefix => Copy_Original_Tree (Param_Type),
                              Attribute_Name => Name_Input),
                          Parameter_Associations =>
                            New_List (
                              Make_Identifier (Loc, Chars (Params))))));

               else
                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        New_Reference_To (Param, Loc),
                      Object_Definition => Copy_Original_Tree (Param_Type)));

                  if In_Present (Param_Spec) or else
                     not Out_Present (Param_Spec)
                  then

                     Append_To (Param_Read_Stmts,
                       Make_Attribute_Reference (Loc,
                         Prefix => Copy_Original_Tree (Param_Type),
                         Attribute_Name => Name_Read,

                         Expressions =>
                           New_List (
                             Make_Identifier (Loc, Chars (Params)),
                             Make_Identifier (Loc, Chars (Param)))));
                  end if;
               end if;

               if Out_Present (Param_Spec) then

                  if Has_Unknown_Size (Etype (Param_Type)) then

                     Append_To (Param_Write_Stmts,
                       Make_Attribute_Reference (Loc,
                         Prefix => Copy_Original_Tree (Param_Type),
                         Attribute_Name => Name_Output,

                         Expressions =>
                           New_List (
                             Make_Identifier (Loc, Chars (Result)),
                             Make_Identifier (Loc, Chars (Param)))));

                  else
                     Append_To (Param_Write_Stmts,
                       Make_Attribute_Reference (Loc,
                         Prefix => Copy_Original_Tree (Param_Type),
                         Attribute_Name => Name_Write,

                         Expressions =>
                           New_List (
                             Make_Identifier (Loc, Chars (Result)),
                             Make_Identifier (Loc, Chars (Param)))));
                  end if;
               end if;

               Param_Spec := Next (Param_Spec);
            end loop;
         end if;

         --  Add the declaration for the value returned by a function

         if Nkind (Subp_Spec) = N_Function_Specification
           and then not Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec)))
         then
            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Returned_Val,
                Object_Definition =>
                  Copy_Original_Tree (Subtype_Mark (Subp_Spec))));
         end if;

         Stmts := Param_Read_Stmts;

         if Prefix /= Empty then
            Subp_Name :=
               Make_Selected_Component (Loc,
                  Prefix => Prefix,
                  Selector_Name => Subp_Name);
         end if;

         if Nkind (Subp_Spec) = N_Function_Specification then

            if Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec))) then

               Append_To (Stmts,

                 Make_Block_Statement (Loc,
                   Declarations => New_List (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => Returned_Val,
                       Object_Definition =>
                         Copy_Original_Tree (Subtype_Mark (Subp_Spec)),

                       Expression =>
                         Make_Function_Call (Loc,
                           Name => Copy_Original_Tree (Subp_Name),
                           Parameter_Associations => Param_List))),

                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (
                         Make_Attribute_Reference (Loc,
                           Prefix => AE_Exception_Occurrence (Loc),
                           Attribute_Name => Name_Write,

                           Expressions => New_List (
                             New_Reference_To (Result, Loc),
                             AE_Null_Occurrence (Loc))),

                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             Copy_Original_Tree (Subtype_Mark (Subp_Spec)),
                           Attribute_Name => Name_Output,

                           Expressions =>
                             New_List (
                               Make_Identifier (Loc, Chars (Result)),
                               Make_Identifier (Loc,
                                 Chars => Chars (Returned_Val))))))));
            else
               Append_To (Stmts,
                 Make_Assignment_Statement (Loc,
                   Name => Make_Identifier (Loc, Chars (Returned_Val)),
                   Expression =>
                     Make_Function_Call (Loc,
                       Name => Copy_Original_Tree (Subp_Name),
                       Parameter_Associations => Param_List)));

               Append_To (Stmts,
                 Make_Attribute_Reference (Loc,
                   Prefix => AE_Exception_Occurrence (Loc),
                   Attribute_Name => Name_Write,

                   Expressions => New_List (
                     New_Reference_To (Result, Loc),
                     AE_Null_Occurrence (Loc))));
            end if;

         else
            Append_To (Stmts,
              Make_Procedure_Call_Statement (Loc,
                Name => Copy_Original_Tree (Subp_Name),
                Parameter_Associations => Param_List));

            Append_To (Stmts,
              Make_Attribute_Reference (Loc,
                Prefix => AE_Exception_Occurrence (Loc),
                Attribute_Name => Name_Write,

                Expressions => New_List (
                  New_Reference_To (Result, Loc),
                  AE_Null_Occurrence (Loc))));
         end if;

         Append_List (Param_Write_Stmts, Stmts);

         if Nkind (Subp_Spec) = N_Function_Specification
           and then not Has_Unknown_Size (Etype (Subtype_Mark (Subp_Spec)))
         then
            Append_To (Stmts,
              Make_Attribute_Reference (Loc,
                Prefix => Copy_Original_Tree (Subtype_Mark (Subp_Spec)),
                Attribute_Name => Name_Write,

                Expressions =>
                  New_List (
                    Make_Identifier (Loc, Chars (Result)),
                    Make_Identifier (Loc, Chars (Returned_Val)))));
         end if;

         Hss :=
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => Stmts,
             Exception_Handlers => New_List (
               Make_Exception_Handler (Loc,
                 Choice_Parameter => Make_Identifier (Loc, Chars (Except)),
                 Exception_Choices => New_List (Make_Others_Choice (Loc)),
                 Statements => New_List (
                   Make_Attribute_Reference (Loc,
                     Prefix => AE_Exception_Occurrence (Loc),
                     Attribute_Name => Name_Write,

                     Expressions =>
                       New_List (
                         New_Reference_To (Result, Loc),
                         New_Reference_To (Except, Loc)))))));

         Stub_Body :=
           Make_Subprogram_Body (Loc,
             Specification => Copy_Original_Tree (Stub_Spec),
             Declarations => Decls,
             Handled_Statement_Sequence => Hss);

         return Stub_Body;
      end Build_Receiving_Stub_Body;

      ---------------------------
      -- Build_Receiving_Stubs --
      ---------------------------

      procedure Build_Receiving_Stubs
        (Pkg_Decl : Node_Id;
         Pkg_Body : Node_Id;
         Prefix   : Node_Id)
      is
         Specif          : Node_Id := Specification (Pkg_Decl);
         Decls           : List_Id := Visible_Declarations (Specif);
         Subpackage_Body : Node_Id;
         Decl            : Node_Id;
         Spec            : Node_Id;
         New_Prefix      : Node_Id;

      begin
         if Decls /= No_List then
            Decl := First (Decls);

            while Present (Decl) loop
               if Comes_From_Source (Decl) then
                  if Nkind (Decl) = N_Subprogram_Declaration then
                     Subp_Num := Subp_Num + 1;

                     if Nkind (Specification (Decl)) =
                                              N_Procedure_Specification
                       and then Is_Asynchronous (Defining_Entity (Decl))
                     then
                        Stub_Body :=
                          Build_Async_Receiving_Stub_Body (Decl, Prefix);
                        Append (Stub_Body, Receiver_Decls);
                        Param_Assocs :=
                          New_List (Make_Identifier (Loc, Params_Name));

                     else
                        Stub_Body := Build_Receiving_Stub_Body (Decl, Prefix);
                        Append (Stub_Body, Receiver_Decls);
                        Param_Assocs :=
                          New_List (
                            Make_Identifier (Loc, Params_Name),
                            Make_Identifier (Loc, Result_Name));
                     end if;

                     Append_To (Case_Stmt_Alts,
                       Make_Case_Statement_Alternative (Loc,
                         Discrete_Choices => New_List (
                           Make_Integer_Literal (Loc, UI_From_Int (Subp_Num))),

                         Statements => New_List (
                           Make_Procedure_Call_Statement (Loc,
                             Name => Copy_Original_Tree (Defining_Unit_Name
                               (Specification (Stub_Body))),
                             Parameter_Associations => Param_Assocs))));

                  elsif Nkind (Decl) = N_Package_Declaration then

                     Spec := Specification (Decl);

                     if Prefix /= Empty then
                        New_Prefix :=
                          Make_Selected_Component (Loc,
                            Prefix => Prefix,
                              Selector_Name =>
                                Copy_Original_Tree (
                                  Defining_Unit_Name (Spec)));
                     else
                        New_Prefix := Defining_Unit_Name (Spec);
                     end if;

                     if not Present (Corresponding_Body (Decl)) then
                        Subpackage_Body :=
                          Make_Package_Body (Loc,
                            Defining_Unit_Name =>
                              Copy_Original_Tree (Defining_Unit_Name (Specif)),
                            Declarations => New_List);
                        Append (Subpackage_Body, Declarations (Pkg_Body));
                     end if;

                     Build_Receiving_Stubs (Decl, Subpackage_Body, New_Prefix);

                  elsif Nkind (Decl) = N_Full_Type_Declaration then
                     if Is_Remote_Access_To_Class_Wide_Type (
                          Defining_Identifier (Decl))
                     then
                        Last_Racw_Num := Last_Racw_Num + 1;
                        Add_Racw_Stubs
                          (Decl,
                           Declarations (Pkg_Body),
                           Stubs_CItems,
                           Last_Racw_Num);
                     elsif Is_Remote_Access_To_Subprogram_Type (
                          Defining_Identifier (Decl))
                     then
                        Last_Ras_Num := Last_Ras_Num + 1;
                        Add_RAST_Features (Decl);
                     end if;
                  else
                     --  All the other cases will be seen later

                     null;
                  end if;
               end if;

               Decl := Next (Decl);
            end loop;
         end if;

      end Build_Receiving_Stubs;

   --  Start of processing for Build_Receiving_Stubs_Pkg_Body

   begin
      if Nkind (Unit_Node) = N_Package_Body then
         Pkg_Decl := Get_Declaration_Node (Corresponding_Spec (Unit_Node));
         Spec     := Specification (Pkg_Decl);

      else
         --  In this case we have a spec for which no body is required

         Pkg_Decl := Unit_Node;
         Spec := Specification (Unit_Node);
      end if;

      --  Initialization of features needed from the specification

      Loc         := Sloc (Spec);
      Vis_Decls   := Visible_Declarations (Spec);
      Priv_Decls  := Private_Declarations (Spec);
      Pkg_Name    := Defining_Unit_Name (Spec);

      --  Initialization of names

      Package_Rpc_Receiver_Name := Get_Name_Id ("package_rpc_receiver");
      Receiving_Stub_Name       := Get_Name_Id ("receiving");
      Pkg_Name_String           := Get_Pkg_Name_String_Id (Unit_Node);

      --  Initialize the declarative part of the package body to build
      --  using the original body declarative part.

      if Nkind (Unit_Node) = N_Package_Body then
         Stubs_Pkg_Body := Unit_Node;
      else
         Stubs_Pkg_Body :=
           Make_Package_Body (Loc,
             Defining_Unit_Name => Copy_Original_Tree (Pkg_Name),
             Declarations => New_List);
      end if;

      --  Build and append the receiving stub body to the stubs package body

      Build_Receiving_Stubs (Pkg_Decl, Stubs_Pkg_Body, Empty);

      Append_To (Case_Stmt_Alts,
        Make_Case_Statement_Alternative (Loc,
          Discrete_Choices => New_List (Make_Others_Choice (Loc)),
          Statements => New_List (Make_Null_Statement (Loc))));

      Receiver_Spec :=
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name =>
            Make_Identifier (Loc, Package_Rpc_Receiver_Name),
          Parameter_Specifications => NStub_Param_Specs (Loc));

      Append_To (Receiver_Decls,
         Make_Object_Declaration (Loc,
           Defining_Identifier =>
             Make_Defining_Identifier (Loc, Subp_Num_Name),
           Object_Definition => SP_Subprogram_Id (Loc)));

      Receiver_Body :=
        Make_Subprogram_Body (Loc,
          Specification => Receiver_Spec,

          Declarations  => Receiver_Decls,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Attribute_Reference (Loc,
                  Prefix => SP_Subprogram_Id (Loc),
                  Attribute_Name => Name_Read,

                  Expressions => New_List (
                    Make_Identifier (Loc, Params_Name),
                    Make_Identifier (Loc, Subp_Num_Name))),
                    Make_Case_Statement (Loc,
                      Expression =>
                        Make_Identifier (Loc, Subp_Num_Name),
                      Alternatives => Case_Stmt_Alts))));

      Append (Receiver_Body, Declarations (Stubs_Pkg_Body));

      --  Build the package body node

      Elab_Stmt :=
        Make_Procedure_Call_Statement (Loc,
           Name => SP_Register_Receiving_Stub (Loc),
           Parameter_Associations => New_List (
             Make_String_Literal (Loc, Pkg_Name_String),
             Make_Attribute_Reference (Loc,
               Prefix => Make_Identifier (Loc, Package_Rpc_Receiver_Name),
               Attribute_Name => Name_Unrestricted_Access),
             Make_Attribute_Reference (Loc,
               Prefix => Copy_Original_Tree (Pkg_Name),
               Attribute_Name => Name_Version)));

      Body_Hss := Handled_Statement_Sequence (Stubs_Pkg_Body);

      if  Present (Body_Hss) then
         if Present (Statements (Body_Hss)) then
            Append (Elab_Stmt, Statements (Body_Hss));
         else
            Set_Statements (Body_Hss, New_List (Elab_Stmt));
         end if;
      else
         Set_Handled_Statement_Sequence (Stubs_Pkg_Body,
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements => New_List (Elab_Stmt)));
      end if;

   end Build_Receiving_Stubs_Pkg_Body;

   --------------------
   -- Racw_Read_Spec --
   --------------------

   function Racw_Read_Spec
     (Loc       : Source_Ptr;
      Racw_Type : Entity_Id)
      return      Node_Id
   is
   begin
      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name => Make_Defining_Identifier (Loc, Name_Read),
          Parameter_Specifications => New_List (

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Stream_Name),
              Parameter_Type =>
                Make_Access_Definition (Loc,
                  Subtype_Mark =>
                    Make_Attribute_Reference (Loc,
                      Prefix => AS_Root_Stream_Type (Loc),
                      Attribute_Name => Name_Class))),

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Item_Name),
              Out_Present => True,
              Parameter_Type =>
                New_Reference_To (Racw_Type, Loc))));
   end Racw_Read_Spec;

   ---------------------
   -- Racw_Write_Spec --
   ---------------------

   function Racw_Write_Spec
     (Loc       : Source_Ptr;
      Racw_Type : Entity_Id)
      return      Node_Id
   is
   begin
      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name => Make_Defining_Identifier (Loc, Name_Write),
          Parameter_Specifications => New_List (

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Stream_Name),

              Parameter_Type =>
                Make_Access_Definition (Loc,
                  Subtype_Mark =>
                    Make_Attribute_Reference (Loc,
                      Prefix => AS_Root_Stream_Type (Loc),
                      Attribute_Name => Name_Class))),

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Item_Name),
              Parameter_Type =>
                New_Reference_To (Racw_Type, Loc))));
   end Racw_Write_Spec;

   --------------------
   -- Ras_Input_Spec --
   --------------------

   function Ras_Input_Spec
     (Loc      : Source_Ptr;
      Ras_Type : Entity_Id)
      return      Node_Id
   is
      Fat_Type : constant Entity_Id := Equivalent_Type (Ras_Type);
   begin
      return
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Make_Defining_Identifier (Loc, Name_Input),
          Parameter_Specifications => New_List (

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Stream_Name),
              Parameter_Type =>
                Make_Access_Definition (Loc,
                  Subtype_Mark =>
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        New_Occurrence_Of (RTE (RE_Root_Stream_Type), Loc),
                      Attribute_Name => Name_Class)))),

          Subtype_Mark => New_Occurrence_Of (Fat_Type, Loc));
   end Ras_Input_Spec;

   ---------------------
   -- Ras_Output_Spec --
   ---------------------

   function Ras_Output_Spec
     (Loc      : Source_Ptr;
      Ras_Type : Entity_Id)
      return      Node_Id
   is
   begin
      return Ras_Proc_Spec (Loc, Ras_Type,  Name_Output);
   end Ras_Output_Spec;

   -------------------
   -- Ras_Read_Spec --
   -------------------

   function Ras_Read_Spec
     (Loc      : Source_Ptr;
      Ras_Type : Entity_Id)
      return      Node_Id
   is
   begin
      return Ras_Proc_Spec (Loc, Ras_Type,  Name_Read);
   end Ras_Read_Spec;

   --------------------
   -- Ras_Write_Spec --
   --------------------

   function Ras_Write_Spec
     (Loc      : Source_Ptr;
      Ras_Type : Entity_Id)
      return      Node_Id
   is
   begin
      return Ras_Proc_Spec (Loc, Ras_Type,  Name_Write);
   end Ras_Write_Spec;

   -------------------
   -- Ras_Proc_Spec --
   -------------------

   function Ras_Proc_Spec
     (Loc       : Source_Ptr;
      Ras_Type  : Entity_Id;
      Proc_Name : Name_Id)
      return      Node_Id
   is
      Fat_Type : Entity_Id := Equivalent_Type (Ras_Type);
   begin
      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name => Make_Defining_Identifier (Loc, Proc_Name),
          Parameter_Specifications => New_List (

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Stream_Name),

              Parameter_Type =>
                Make_Access_Definition (Loc,
                  Subtype_Mark =>
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        New_Occurrence_Of (RTE (RE_Root_Stream_Type), Loc),
                      Attribute_Name => Name_Class))),

            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, Item_Name),
              Parameter_Type => New_Occurrence_Of (Fat_Type, Loc))));
   end Ras_Proc_Spec;

end Exp_Dist;
