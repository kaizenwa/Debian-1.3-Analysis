------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ P R A G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.325 $                            --
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

--  This unit contains the semantic processing for all pragmas, both language
--  and implementation defined. For most pragmas, the parser only does the
--  most basic job of checking the syntax, so Sem_Prag also contains the code
--  to complete the syntax checks. Certain pragmas are handled partially or
--  completely by the parser (see Par.Prag for further details).

with Atree;    use Atree;
with Casing;   use Casing;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Util; use Exp_Util;
with Features; use Features;
with Fname;    use Fname;
with Hostparm; use Hostparm;
with Lib;      use Lib;
with Lib.Writ; use Lib.Writ;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Eval; use Sem_Eval;
with Sem_Intr; use Sem_Intr;
with Sem_Mech; use Sem_Mech;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Uname;    use Uname;

package body Sem_Prag is

   ----------------------------------------------
   -- Common Handling of Import-Export Pragmas --
   ----------------------------------------------

   --  In the following section, a number of Import_xxx and Export_xxx
   --  pragmas are defined by GNAT. These are compatible with the DEC
   --  pragmas of the same name, and all have the following common
   --  form and processing:

   --  pragma Export_xxx
   --        [Internal                 =>] LOCAL_NAME,
   --     [, [External                 =>] EXTERNAL_SYMBOL]
   --     [, other optional parameters   ]);

   --  pragma Import_xxx
   --        [Internal                 =>] LOCAL_NAME,
   --     [, [External                 =>] EXTERNAL_SYMBOL]
   --     [, other optional parameters   ]);

   --   EXTERNAL_SYMBOL ::=
   --     IDENTIFIER
   --   | static_string_EXPRESSION

   --  The internal LOCAL_NAME designates the entity that is imported or
   --  exported, and must refer to an entity in the current declarative
   --  part (as required by the rules for LOCAL_NAME).

   --  The external linker name is designated by the External parameter
   --  if given, or the Internal parameter if not (if there is no External
   --  parameter, the External parameter is a copy of the Internal name).

   --  If the External parameter is given as a string, then this string
   --  is treated as an external name (exactly as though it had been given
   --  as an External_Name parameter for a normal Import pragma).

   --  If the External parameter is given as an identifier (or there is no
   --  External parameter, so that the Internal identifier is used), then
   --  the external name is the characters of the identifier, translated
   --  to all upper case letters for OpenVMS versions of GNAT, and to all
   --  lower case letters for all other versions

   --  Note: the external name specified or implied by any of these special
   --  Import_xxx or Export_xxx pragmas override an external or link name
   --  specified in a previous Import or Export pragma.

   --  Note: these and all other DEC-compatible GNAT pragmas allow full
   --  use of named notation, following the standard rules for subprogram
   --  calls, i.e. parameters can be given in any order if named notation
   --  is used, and positional and named notation can be mixed, subject to
   --  the rule that all positional parameters must appear first.

   --  Note: All these pragmas are implemented exactly following the DEC
   --  design and implementation and are intended to be fully compatible
   --  with the use of these pragmas in the DEC Ada compiler.

   -------------------------------------
   -- Local Subprograms and Variables --
   -------------------------------------

   function Is_Generic_Subprogram (Id : Entity_Id) return Boolean;
   --  Return True if Id is a generic procedure or a function

   function Get_Base_Subprogram (Def_Id : Entity_Id) return Entity_Id;
   --  If Def_Id refers to a renamed subprogram, then the base subprogram
   --  (the original one, following the renaming chain) is returned.
   --  Otherwise the entity is returned unchanged. Should be in Einfo???

   Locking_Policy_Sloc          : Source_Ptr := No_Location;
   Queuing_Policy_Sloc          : Source_Ptr := No_Location;
   Task_Dispatching_Policy_Sloc : Source_Ptr := No_Location;
   --  These global variables remember the location of a previous locking,
   --  queuing or task dispatching policy pragma, so that appropriate error
   --  messages can be generated for inconsistent pragmas. Note that it is
   --  fine that these are global locations, because the check for consistency
   --  is over the entire program.

   --------------------
   -- Analyze_Pragma --
   --------------------

   procedure Analyze_Pragma (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Prag_Id : Pragma_Id;

      Pragma_Exit : exception;
      --  This exception is used to exit pragma processing completely. It
      --  is used when an error is detected, and in other situations where
      --  it is known that no further processing is required.

      Arg_Count : Int;
      --  Number of pragma argument associations

      Arg1 : Node_Id;
      Arg2 : Node_Id;
      Arg3 : Node_Id;
      Arg4 : Node_Id;
      --  First four pragma arguments (pragma argument association nodes,
      --  or Empty if the corresponding argument does not exist).

      procedure Check_Ada_83_Warning;
      --  Issues a warning message for the current pragma if operating in Ada
      --  83 mode (used for language pragmas that are not a standard part of
      --  Ada 83). This procedure does not raise Error_Pragma. Also notes use
      --  of 95 pragma.

      procedure Check_Arg_Count (Required : Nat);
      --  Check argument count for pragma is equal to given parameter.
      --  If not, then issue an error message and raise Pragma_Exit.

      --  Note: all routines whose name is Check_Arg_Is_xxx take an
      --  argument Arg which can either be a pragma argument association,
      --  in which case the check is applied to the expression of the
      --  association or an expression directly.

      procedure Check_Arg_Is_Identifier (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is an
      --  identifier. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Integer_Literal (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is an
      --  integer literal. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Library_Level_Local_Name (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it has the
      --  proper syntactic form for a local name and meets the semantic
      --  requirements for a local name. The local name is analyzed as
      --  part of the processing for this call. In addition, the local
      --  name is required to represent an entity at the library level.

      procedure Check_Arg_Is_Local_Name (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it has the
      --  proper syntactic form for a local name and meets the semantic
      --  requirements for a local name. The local name is analyzed as
      --  part of the processing for this call.

      procedure Check_Arg_Is_Locking_Policy (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is a valid
      --  locking policy name. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_One_Of (Arg : Node_Id; N1, N2 : Name_Id);
      procedure Check_Arg_Is_One_Of (Arg : Node_Id; N1, N2, N3 : Name_Id);
      --  Check the specified argument Arg to make sure that it is an
      --  identifier whose name matches either N1 or N2 (or N3 if present).
      --  If not then give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Queuing_Policy (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is a valid
      --  queuing policy name. If not give error and raise Pragma_Exit.

      procedure Check_Arg_Is_Static_Expression
        (Arg : Node_Id;
         Typ : Entity_Id);
      --  Check the specified argument Arg to make sure that it is a static
      --  expression of the given type (i.e. it will be analyzed and resolved
      --  using this type, which can be any valid argument to Resolve, e.g.
      --  Any_Integer is OK). If not, given error and raise Pragma_Exit.

      procedure Check_Arg_Is_String_Literal (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is a
      --  string literal. If not give error and raise Pragma_Exit

      procedure Check_Arg_Is_Task_Dispatching_Policy (Arg : Node_Id);
      --  Check the specified argument Arg to make sure that it is a valid
      --  valid task dispatching policy name. If not give error and raise
      --  Pragma_Exit.

      procedure Check_At_Least_N_Arguments (N : Nat);
      --  Check there are at least N arguments present

      procedure Check_At_Most_N_Arguments (N : Nat);
      --  Check there are no more than N arguments present

      procedure Check_First_Subtype (Arg : Node_Id);
      --  Checks that Arg, whose expression is an entity name referencing
      --  a subtype, does not reference a type that is not a first subtype.

      procedure Check_In_Main_Program;
      --  Common checks for pragmas that appear within a main program
      --  (Priority, Main_Storage, Time_Slice).

      procedure Check_Interrupt_Handler;
      --  Common proessing for first argument of pragma Interrupt_Handler
      --  or pragma Attach_Handler.

      procedure Check_Is_In_Decl_Part_Or_Package_Spec;
      --  Check that pragma appears in a declarative part, or in a package
      --  specification, i.e. that it does not occur in a statement sequence
      --  in a body.

      procedure Check_No_Identifier (Arg : Node_Id);
      --  Checks that the given argument does not have an identifier. If
      --  an identifier is present, then an error message is issued, and
      --  Pragma_Exit is raised.

      procedure Check_No_Identifiers;
      --  Checks that none of the arguments to the pragma has an identifier.
      --  If any argument has an identifier, then an error message is issued,
      --  and Pragma_Exit is raised.

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id);
      --  Checks if the given argument has an identifier, and if so, requires
      --  it to match the given identifier name. If there is a non-matching
      --  identifier, then an error message is given and Error_Pragmas raised.

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : String);
      --  Checks if the given argument has an identifier, and if so, requires
      --  it to match the given identifier name. If there is a non-matching
      --  identifier, then an error message is given and Error_Pragmas raised.
      --  In this version of the procedure, the identifier name is given as
      --  a string with lower case letters.

      procedure Check_Static_Constraint (Constr : Node_Id);
      --  Constr is a constraint from an N_Subtype_Indication node from a
      --  component constraint in an Unchecked_Union type. This routine checks
      --  that the constraint is static as required by the restrictions for
      --  Unchecked_Union.

      procedure Check_Valid_Configuration_Pragma;
      --  Legality checks for placement of a configuration pragma

      procedure Check_Valid_Library_Unit_Pragma;
      --  Legality checks for library unit pragmas. A special case arises for
      --  pragmas in generic instances that come from copies of the original
      --  library unit pragmas in the generic templates. In the case of other
      --  than library level instantiations these can appear in contexts which
      --  would normally be invalid (they only apply to the original template
      --  and to library level instantiations).

      procedure Error_Pragma (Msg : String);
      --  Outputs error message for current pragma. The message contains an %
      --  that will be replaced with the pragma name, and the flag is placed
      --  on the pragma itself. Pragma_Exit is then raised.

      procedure Error_Pragma_Arg (Msg : String; Arg : Node_Id);
      --  Outputs error message for current pragma. The message may contain
      --  an % that will be replaced with the pragma name. The parameter Arg
      --  may either be a pragma argument association, in which case the flag
      --  is placed on the expression of this association, or an expression,
      --  in which case the flag is placed directly on the expression. The
      --  message is placed using Error_Msg_N, so the message may also contain
      --  an & insertion character which will reference the given Arg value.
      --  After placing the message, Pragma_Exit is raised.

      procedure Error_Pragma_Arg_Ident (Msg : String; Arg : Node_Id);
      --  Outputs error message for current pragma. The message may contain
      --  contains an % that will be replaced with the pragma name. The
      --  parameter Arg must be a pragma argument association with a non-empty
      --  identifier (i.e. its Chars field must be set), and the error message
      --  is placed on the identifier. The message is placed using Error_Msg_N
      --  so the message may also contain an & insertion character which will
      --  reference the identifier. After placing the message, Pragma_Exit
      --  is raised.

      function Find_Lib_Unit_Name return Entity_Id;
      --  Used for a library unit pragma to find the entity to which the
      --  library unit pragma applies, returns the entity found.

      procedure Find_Program_Unit_Name (Id : Node_Id);
      --  If the pragma is a compilation unit pragma, the id must denote the
      --  compilation unit in the same compilation, and the pragma must appear
      --  in the list of preceding or trailing pragmas. If it is a program
      --  unit pragma that is not a compilation unit pragma, then the
      --  identifier must be visible.

      type Name_List is array (Natural range <>) of Name_Id;
      type Args_List is array (Natural range <>) of Node_Id;
      procedure Gather_Associations
        (Names : Name_List;
         Args  : out Args_List);
      --  This procedure is used to gather the arguments for a pragma that
      --  permits arbitrary ordering of parameters using the normal rules
      --  for named and positional parameters. The Names argument is a list
      --  of Name_Id values that corresponds to the allowed pragma argument
      --  association identifiers in order. The result returned in Args is
      --  a list of corresponding expressions that are the pragma arguments.
      --  Note that this is a list of expressions, not of pragma argument
      --  associations (Gather_Associations has completely checked all the
      --  optional identifiers when it returns). An entry in Args is Empty
      --  on return if the corresponding argument is not present. Since all
      --  pragmas allowing reordering are GNAT defined pragmas, this routine
      --  also makes the required call to GNAT_Pragma.

      procedure GNAT_Pragma;
      --  Called for all GNAT defined pragmas to note the use of the feature,
      --  and also check the relevant restriction (No_Implementation_Pragmas).

      function Is_Before_First_Decl
        (Pragma_Node : Node_Id;
         Decls       : List_Id)
         return        Boolean;
      --  Return True if Pragma_Node is before the first declarative item in
      --  Decls where Decls is the list of declarative items.

      function Is_Configuration_Pragma return Boolean;
      --  Deterermines if the placement of the current pragma is appropriate
      --  for a configuration pragma (precedes the current compilation unit)

      procedure Pragma_Misplaced;
      --  Issue fatal error message for misplaced pragma

      procedure Pragma_Not_Implemented;
      --  Issue warning message for unimplemented pragma

      procedure Process_Atomic_Shared_Volatile;
      --  Common processing for pragmas Atomic, Shared, Volatile. Note that
      --  Shared is an obsolete Ada 83 pragma, treated as being identical
      --  in effect to pragma Atomic.

      procedure Process_Convention (C : out Convention_Id; E : out Entity_Id);
      --  Common procesing for Convention, Interface, Import and Export.
      --  Checks first two arguments of pragma, and sets the appropriate
      --  convention value in the specified entity or entities. On return
      --  C is the convention, E is the referenced entity.

      procedure Process_Extended_Import_Export_Exception_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Form     : Node_Id;
         Arg_Code     : Node_Id);
      --  Common processing for the pragmas Import_Object, Common_Object
      --  and Psect_Object. The three arguments correspond to the three
      --  named parameters of the pragmas. An argument is empty if the
      --  corresponding parameter is not present in the pragma.

      procedure Process_Extended_Import_Export_Object_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Size     : Node_Id);
      --  Common processing for the pragmas Import_Object, Common_Object
      --  and Psect_Object. The three arguments correspond to the three
      --  named parameters of the pragmas. An argument is empty if the
      --  corresponding parameter is not present in the pragma.

      procedure Process_Extended_Import_Export_Internal_Arg
        (Arg_Internal : Node_Id := Empty);
      --  Common processing for all extended Import and Export pragmas. The
      --  argument is the pragma parameter for the Internal argument. If
      --  Arg_Internal is empty or inappropriate, an error message is posted.
      --  Otherwise, on normal return, the Entity_Field of Arg_Internal is
      --  set to identify the referenced entity.

      procedure Process_Extended_Import_Export_Subprogram_Pragma
        (Arg_Internal                 : Node_Id;
         Arg_External                 : Node_Id;
         Arg_Parameter_Types          : Node_Id;
         Arg_Result_Type              : Node_Id := Empty;
         Arg_Mechanism                : Node_Id;
         Arg_Result_Mechanism         : Node_Id := Empty;
         Arg_First_Optional_Parameter : Node_Id := Empty);
      --  Common processing for all extended Import and Export pragmas
      --  applying to subprograms. The caller omits any arguments that do
      --  not apply to the pragma in question (for example, Arg_Result_Type
      --  can be non-Empty only in the Import_Function and Export_Function
      --  cases). The argument names correspond to the allowed pragma
      --  association identifiers.

      procedure Process_Generic_List;
      --  Common processing for Share_Generic and Inline_Generic

      procedure Process_Import_Or_Interface;
      --  Common processing for Import of Interface

      procedure Process_Interface_Name
        (Subprogram_Def : Entity_Id;
         Ext_Arg        : Node_Id;
         Link_Arg       : Node_Id);
      --  Given the last two arguments of pragma Import, pragma Export, or
      --  pragma Interface_Name, performs validity checks and sets the
      --  Interface_Name field of the given subprogram entity to the
      --  appropriate external or link name, depending on the arguments
      --  given. Ext_Arg is always present, but Link_Arg may be missing.
      --  Note that Ext_Arg may represent the Link_Name if Link_Arg is
      --  missing, and appropriate named notation is used for Ext_Arg.
      --  If neither Ext_Arg nor Link_Arg is present, the interface name
      --  is set to the default from the subprogram name.

      procedure Process_Suppress_Unsuppress (Sense : Boolean);
      --  Common processing for Suppress and Unsuppress. The boolean parameter
      --  Sense is True for the Suppress case, False for the Unsuppress case.

      procedure Set_Extended_Import_Export_External_Name
        (Internal_Ent : Entity_Id;
         Arg_External : Node_Id);
      --  Common processing for all extended import export pragmas. The first
      --  argument, Internal_Ent, is the internal entity, which has already
      --  been checked for validity by the caller. Arg_External is from the
      --  Import or Export pragma, and may be null if no External parameter
      --  was present. If Arg_External is present and is a non-null string
      --  (a null string is treated as the default), then the Interface_Name
      --  field of Internal_Ent is set appropriately.

      procedure Set_Mechanism_Value (Ent : Entity_Id; Mech_Name : Node_Id);
      --  Mech is a parameter passing mechanism (see Import_Function syntax
      --  for MECHANISM_NAME). This routine checks that the mechanism argument
      --  has the right form, and if not issues an error message. If the
      --  argument has the right form then the Mechanism field of Ent is
      --  set appropriately.

      function Set_Pragma_Arg (Arg : Node_Id) return Node_Id;
      --  All the routines that check pragma arguments take either a pragma
      --  argument association (in which case the expression of the argument
      --  association is checked), or the expression directly. The function
      --  Set_Pragma_Arg is a utility used to deal with these two cases. If
      --  Arg is a pragma argument association node, then its expression is
      --  returned, otherwise Arg is returned unchanged.

      --------------------------
      -- Check_Ada_83_Warning --
      --------------------------

      procedure Check_Ada_83_Warning is
      begin
         GNAT_Pragma;

         if Ada_83 and then Comes_From_Source (N) then
            Error_Msg_N ("(Ada 83) pragma& is non-standard?", N);
         end if;
      end Check_Ada_83_Warning;

      ---------------------
      -- Check_Arg_Count --
      ---------------------

      procedure Check_Arg_Count (Required : Nat) is
      begin
         if Arg_Count /= Required then
            Error_Pragma ("wrong number of arguments for pragma%");
         end if;
      end Check_Arg_Count;

      -----------------------------
      -- Check_Arg_Is_Identifier --
      -----------------------------

      procedure Check_Arg_Is_Identifier (Arg : Node_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         if Nkind (Argx) /= N_Identifier then
            Error_Pragma_Arg
              ("argument for pragma% must be identifier", Argx);
         end if;
      end Check_Arg_Is_Identifier;

      ----------------------------------
      -- Check_Arg_Is_Integer_Literal --
      ----------------------------------

      procedure Check_Arg_Is_Integer_Literal (Arg : Node_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         if Nkind (Argx) /= N_Integer_Literal then
            Error_Pragma_Arg
              ("argument for pragma% must be integer literal", Argx);
         end if;
      end Check_Arg_Is_Integer_Literal;

      -------------------------------------------
      -- Check_Arg_Is_Library_Level_Local_Name --
      -------------------------------------------

      --  LOCAL_NAME ::=
      --    DIRECT_NAME
      --  | DIRECT_NAME'ATTRIBUTE_DESIGNATOR
      --  | library_unit_NAME

      procedure Check_Arg_Is_Library_Level_Local_Name (Arg : Node_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Local_Name (Arg);

         if not Is_Library_Level_Entity (Entity (Expression (Arg))) then
            Error_Pragma_Arg
              ("argument for pragma% must be library level entity", Arg);
         end if;
      end Check_Arg_Is_Library_Level_Local_Name;

      -----------------------------
      -- Check_Arg_Is_Local_Name --
      -----------------------------

      --  LOCAL_NAME ::=
      --    DIRECT_NAME
      --  | DIRECT_NAME'ATTRIBUTE_DESIGNATOR
      --  | library_unit_NAME

      procedure Check_Arg_Is_Local_Name (Arg : Node_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         if Nkind (Argx) not in N_Direct_Name
           and then (Nkind (Argx) /= N_Selected_Component
                      or else Nkind (Selector_Name (Argx)) /= N_Identifier)
           and then (Nkind (Argx) /= N_Attribute_Reference
                      or else Present (Expressions (Argx))
                      or else Nkind (Prefix (Argx)) /= N_Identifier)
         then
            Error_Pragma_Arg ("argument for pragma% must be local name", Argx);
         end if;

         Analyze (Argx);

         if Is_Entity_Name (Argx)
           and then Scope (Entity (Argx)) /= Current_Scope
         then
            Error_Pragma_Arg
              ("pragma% argument must be in same declarative part", Arg);
         end if;
      end Check_Arg_Is_Local_Name;

      ---------------------------------
      -- Check_Arg_Is_Locking_Policy --
      ---------------------------------

      procedure Check_Arg_Is_Locking_Policy (Arg : Node_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if not Is_Locking_Policy_Name (Chars (Argx)) then
            Error_Pragma_Arg
              ("argument of pragma% is not valid locking policy name",
               Argx);
         end if;
      end Check_Arg_Is_Locking_Policy;

      -------------------------
      -- Check_Arg_Is_One_Of --
      -------------------------

      procedure Check_Arg_Is_One_Of (Arg : Node_Id; N1, N2 : Name_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if Chars (Argx) /= N1 and then Chars (Argx) /= N2 then
            Error_Msg_Name_2 := N1;
            Error_Msg_Name_3 := N2;
            Error_Pragma_Arg ("argument for pragma% must be% or%", Argx);
         end if;
      end Check_Arg_Is_One_Of;

      procedure Check_Arg_Is_One_Of
        (Arg        : Node_Id;
         N1, N2, N3 : Name_Id)
      is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if Chars (Argx) /= N1
           and then Chars (Argx) /= N2
           and then Chars (Argx) /= N3
         then
            Error_Pragma_Arg ("invalid argument for pragma%", Argx);
         end if;
      end Check_Arg_Is_One_Of;

      ---------------------------------
      -- Check_Arg_Is_Queuing_Policy --
      ---------------------------------

      procedure Check_Arg_Is_Queuing_Policy (Arg : Node_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if not Is_Queuing_Policy_Name (Chars (Argx)) then
            Error_Pragma_Arg
              ("argument of pragma% is not valid queuing policy name",
               Argx);
         end if;
      end Check_Arg_Is_Queuing_Policy;

      ------------------------------------
      -- Check_Arg_Is_Static_Expression --
      ------------------------------------

      procedure Check_Arg_Is_Static_Expression
        (Arg : Node_Id;
         Typ : Entity_Id)
      is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         Analyze_And_Resolve (Argx, Typ);

         if Is_OK_Static_Expression (Argx) then
            return;

         elsif Etype (Argx) = Any_Type then
            raise Pragma_Exit;

         elsif not Is_Static_Expression (Argx) then
            Error_Pragma_Arg
              ("argument for pragma% must be a static expression", Argx);

         --  Static expression which raised constraint error. This is
         --  an error that has already been flagged, so just return.

         else
            raise Pragma_Exit;
         end if;

      end Check_Arg_Is_Static_Expression;

      ---------------------------------
      -- Check_Arg_Is_String_Literal --
      ---------------------------------

      procedure Check_Arg_Is_String_Literal (Arg : Node_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         if Nkind (Argx) /= N_String_Literal then
            Error_Pragma_Arg
              ("argument for pragma% must be string literal", Argx);
         end if;

      end Check_Arg_Is_String_Literal;

      ------------------------------------------
      -- Check_Arg_Is_Task_Dispatching_Policy --
      ------------------------------------------

      procedure Check_Arg_Is_Task_Dispatching_Policy (Arg : Node_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         Check_Arg_Is_Identifier (Argx);

         if not Is_Task_Dispatching_Policy_Name (Chars (Argx)) then
            Error_Pragma_Arg
              ("argument of pragma% is not valid task dispatching policy name",
                Argx);
         end if;
      end Check_Arg_Is_Task_Dispatching_Policy;

      --------------------------------
      -- Check_At_Least_N_Arguments --
      --------------------------------

      procedure Check_At_Least_N_Arguments (N : Nat) is
      begin
         if Arg_Count < N then
            Error_Pragma ("too few arguments for pragma%");
         end if;
      end Check_At_Least_N_Arguments;

      -------------------------------
      -- Check_At_Most_N_Arguments --
      -------------------------------

      procedure Check_At_Most_N_Arguments (N : Nat) is
         Arg : Node_Id;

      begin
         if Arg_Count > N then
            Arg := Arg1;

            for J in 1 .. N loop
               Arg := Next (Arg);
               Error_Pragma_Arg ("too many arguments for pragma%", Arg);
            end loop;
         end if;
      end Check_At_Most_N_Arguments;

      -------------------------
      -- Check_First_Subtype --
      -------------------------

      procedure Check_First_Subtype (Arg : Node_Id) is
         Argx : constant Node_Id := Set_Pragma_Arg (Arg);

      begin
         if not Is_First_Subtype (Entity (Argx)) then
            Error_Pragma_Arg
              ("pragma% cannot apply to subtype", Argx);
         end if;
      end Check_First_Subtype;

      ---------------------------
      -- Check_In_Main_Program --
      ---------------------------

      procedure Check_In_Main_Program is
         P : constant Node_Id := Parent (N);

      begin
         --  Must be at outer level

         if Nkind (P) /= N_Subprogram_Body
           or else Nkind (Parent (P)) /= N_Compilation_Unit
         then
            Error_Pragma
              ("% pragma allowed only in outer level subprogram");

         elsif Present (Parameter_Specifications (Specification (P))) then
            Error_Msg_Name_1 := Chars (N);
            Error_Msg_N
              ("?pragma% is only effective in main program", N);
         end if;
      end Check_In_Main_Program;

      -----------------------------
      -- Check_Interrupt_Handler --
      -----------------------------

      procedure Check_Interrupt_Handler is
         Arg1_X : constant Node_Id := Expression (Arg1);

      begin
         Analyze (Arg1_X);

         if not Is_Entity_Name (Arg1_X) then
            Error_Pragma_Arg
              ("argument of pragma% must be entity name", Arg1);
         end if;

         declare
            Prot_Proc : constant Entity_Id := Entity (Arg1_X);
            Prot_Type : constant Entity_Id := Scope (Prot_Proc);

         begin
            if Ekind (Prot_Proc) /= E_Procedure
              or else Ekind (Prot_Type) /= E_Protected_Type
            then
               Error_Pragma_Arg
                 ("argument of pragma% must be protected procedure",
                  Arg1);
            end if;

            if not Is_Library_Level_Entity (Prot_Type) then
               Error_Pragma_Arg
                 ("pragma% requires library level entity", Arg1);
            end if;

            if Present (First_Formal (Prot_Proc)) then
               Error_Pragma_Arg
                 ("argument of pragma% must be parameterless procedure",
                  Arg1);
            end if;

            if Parent (N) /=
                 Protected_Definition (Parent (Prot_Type))
            then
               Error_Pragma ("pragma% must be in protected definition");
            end if;


            Set_Is_Interrupt_Handler  (Prot_Proc);

            if Prag_Id = Pragma_Interrupt_Handler then
               Set_Has_Interrupt_Handler (Prot_Type);
            end if;
         end;
      end Check_Interrupt_Handler;

      -------------------------------------------
      -- Check_Is_In_Decl_Part_Or_Package_Spec --
      -------------------------------------------

      procedure Check_Is_In_Decl_Part_Or_Package_Spec is
         P : Node_Id;

      begin
         P := Parent (N);

         loop
            if No (P) then
               exit;

            elsif Nkind (P) = N_Handled_Sequence_Of_Statements then
               exit;

            elsif Nkind (P) = N_Package_Specification then
               return;

            elsif Nkind (P) = N_Block_Statement then
               return;

            --  Note: the following tests seem a little peculiar, because
            --  they test for bodies, but if we were in the statement part
            --  of the body, we would already have hit the handled statement
            --  sequence, so the only way we get here is by being in the
            --  declarative part of the body.

            elsif Nkind (P) = N_Subprogram_Body
              or else Nkind (P) = N_Package_Body
              or else Nkind (P) = N_Task_Body
              or else Nkind (P) = N_Entry_Body
            then
               return;
            end if;

            P := Parent (P);
         end loop;

         Error_Pragma ("pragma% is not in declarative part or package spec");

      end Check_Is_In_Decl_Part_Or_Package_Spec;

      -------------------------
      -- Check_No_Identifier --
      -------------------------

      procedure Check_No_Identifier (Arg : Node_Id) is
      begin
         if Chars (Arg) /= No_Name then
            Error_Pragma_Arg_Ident
              ("pragma% does not permit identifier& here", Arg);
         end if;
      end Check_No_Identifier;

      --------------------------
      -- Check_No_Identifiers --
      --------------------------

      procedure Check_No_Identifiers is
         Arg_Node : Node_Id;

      begin
         if Arg_Count > 0 then
            Arg_Node := Arg1;

            while Present (Arg_Node) loop
               Check_No_Identifier (Arg_Node);
               Arg_Node := Next (Arg_Node);
            end loop;
         end if;
      end Check_No_Identifiers;

      -------------------------------
      -- Check_Optional_Identifier --
      -------------------------------

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id) is
      begin
         if Present (Arg) and then Chars (Arg) /= No_Name then
            if Chars (Arg) /= Id then
               Error_Msg_Name_1 := Chars (N);
               Error_Msg_Name_2 := Id;
               Error_Msg_N ("pragma% argument expects identifier%", Arg);
               raise Pragma_Exit;
            end if;
         end if;
      end Check_Optional_Identifier;

      procedure Check_Optional_Identifier (Arg : Node_Id; Id : String) is
      begin
         Name_Buffer (1 .. Id'Length) := Id;
         Name_Len := Id'Length;
         Check_Optional_Identifier (Arg, Name_Find);
      end Check_Optional_Identifier;

      -----------------------------
      -- Check_Static_Constraint --
      -----------------------------

      --  Note: for convenience in writing this procedure, in addition to
      --  the officially (i.e. by spec) allowed argument which is always
      --  a constraint, it also allows ranges and discriminant associations.

      procedure Check_Static_Constraint (Constr : Node_Id) is

         procedure Require_Static (E : Node_Id);
         --  Require given expression to be static expression

         procedure Require_Static (E : Node_Id) is
         begin
            if not Is_OK_Static_Expression (E) then
               Error_Msg_N
                 ("non-static constraint not allowed in Unchecked_Union", E);
               raise Pragma_Exit;
            end if;
         end Require_Static;

      --  Start of processing for Check_Static_Constraint

      begin
         case Nkind (Constr) is
            when N_Discriminant_Association =>
               Require_Static (Expression (Constr));

            when N_Range =>
               Require_Static (Low_Bound (Constr));
               Require_Static (High_Bound (Constr));

            when N_Attribute_Reference =>
               Require_Static (Type_Low_Bound  (Etype (Prefix (Constr))));
               Require_Static (Type_High_Bound (Etype (Prefix (Constr))));

            when N_Range_Constraint =>
               Check_Static_Constraint (Range_Expression (Constr));

            when N_Index_Or_Discriminant_Constraint =>
               declare
                  IDC : Entity_Id := First (Constraints (Constr));

               begin
                  while Present (IDC) loop
                     Check_Static_Constraint (IDC);
                     IDC := Next (IDC);
                  end loop;
               end;

            when others =>
               null;
         end case;
      end Check_Static_Constraint;

      --------------------------------------
      -- Check_Valid_Configuration_Pragma --
      --------------------------------------

      --  A configuration pragma must appear in the context clause of
      --  a compilation unit, at the start of the list (i.e. only other
      --  pragmas may precede it).

      procedure Check_Valid_Configuration_Pragma is
      begin
         if not Is_Configuration_Pragma then
            Error_Pragma ("incorrect placement for configuration pragma%");
         end if;
      end Check_Valid_Configuration_Pragma;

      -------------------------------------
      -- Check_Valid_Library_Unit_Pragma --
      -------------------------------------

      procedure Check_Valid_Library_Unit_Pragma is
         Plist       : List_Id;
         Parent_Node : Node_Id;
         Unit_Name   : Entity_Id;
         Valid       : Boolean := True;
         Unit_Kind   : Node_Kind;
         Unit_Node   : Node_Id;
         Sindex      : Source_File_Index;

      begin
         if not Is_List_Member (N) then
            Pragma_Misplaced;
            Valid := False;

         else
            Plist := List_Containing (N);
            Parent_Node := Parent (Plist);

            if Parent_Node = Empty then
               Pragma_Misplaced;

            --  Case of pragma appearing after a compilation unit. In this
            --  case it must have an argument with the corresponding name
            --  and must be part of the following pragmas of its parent.

            elsif Nkind (Parent_Node) = N_Compilation_Unit then
               if Plist /= Pragmas_After (Parent_Node) then
                  Pragma_Misplaced;

               elsif Arg_Count = 0 then
                  Error_Pragma
                    ("argument required if outside compilation unit");

               else
                  Check_No_Identifiers;
                  Check_Arg_Count (1);
                  Unit_Node := Unit (Parent_Node);
                  Unit_Kind := Nkind (Unit_Node);

                  Analyze (Expression (Arg1));

                  if        Unit_Kind = N_Generic_Subprogram_Declaration
                    or else Unit_Kind = N_Subprogram_Declaration
                  then
                     Unit_Name := Defining_Entity (Unit_Node);

                  elsif     Unit_Kind = N_Function_Instantiation
                    or else Unit_Kind = N_Package_Instantiation
                    or else Unit_Kind = N_Procedure_Instantiation
                  then
                     Unit_Name := Defining_Entity (Unit_Node);

                  else
                     Unit_Name := Cunit_Entity (Current_Sem_Unit);
                  end if;

                  if Chars (Unit_Name) /=
                     Chars (Entity (Expression (Arg1)))
                  then
                     Error_Pragma_Arg
                       ("pragma% argument is not current unit name", Arg1);
                  end if;

                  if Ekind (Unit_Name) = E_Package
                    and then Present (Renamed_Entity (Unit_Name))
                  then
                     Error_Pragma ("pragma% not allowed for renamed package");
                  end if;
               end if;

            --  Here we check for the generic instantiation case. We detect
            --  this by noting that the Sloc on the node does not belong to
            --  the current compilation unit.

            else
               Sindex := Source_Index (Current_Sem_Unit);

               if Loc < Source_First (Sindex)
                 or else Loc > Source_Last (Sindex)
               then
                  Rewrite_Substitute_Tree (N, Make_Null_Statement (Loc));
                  raise Pragma_Exit;

               --  If before first declaration, the pragma applies to the
               --  enclosing unit, and the name if present must be this name.

               elsif Is_Before_First_Decl (N, Plist) then
                  Unit_Node := Get_Declaration_Node (Current_Scope);
                  Unit_Kind := Nkind (Unit_Node);

                  if Nkind (Parent (Unit_Node)) /= N_Compilation_Unit then
                     Pragma_Misplaced;

                  elsif Unit_Kind = N_Package_Body
                    or else
                      (Unit_Kind = N_Subprogram_Body
                         and then not Acts_As_Spec (Unit_Node))
                  then
                     Pragma_Misplaced;

                  elsif Arg_Count > 0 then
                     Analyze (Expression (Arg1));

                     if Entity (Expression (Arg1)) /= Current_Scope then
                        Error_Pragma_Arg
                          ("name in pragma% must be enclosing unit", Arg1);
                     end if;

                  --  It is legal to have no argument in this context

                  else
                     return;
                  end if;

               --  Error if not before first declaration. This is because a
               --  library unit pragma argument must be the name of a library
               --  unit (RM 10.1.5(7)), but the only names permitted in this
               --  context are (RM 10.1.5(6)) names of subprogram declarations,
               --  generic subprogram declarations or generic instantiations.

               else
                  Error_Pragma
                    ("pragma% misplaced, must be before first declaration");
               end if;
            end if;
         end if;

      end Check_Valid_Library_Unit_Pragma;

      ------------------
      -- Error_Pragma --
      ------------------

      procedure Error_Pragma (Msg : String) is
      begin
         Error_Msg_Name_1 := Chars (N);
         Error_Msg_N (Msg, N);
         Rewrite_Substitute_Tree (N, Make_Null_Statement (Loc));
         raise Pragma_Exit;
      end Error_Pragma;

      ----------------------
      -- Error_Pragma_Arg --
      ----------------------

      procedure Error_Pragma_Arg (Msg : String; Arg : Node_Id) is
      begin
         Error_Msg_Name_1 := Chars (N);

         if Nkind (Arg) = N_Pragma_Argument_Association then
            Error_Msg_N (Msg, Expression (Arg));
         else
            Error_Msg_N (Msg, Arg);
         end if;

         raise Pragma_Exit;
      end Error_Pragma_Arg;

      ----------------------------
      -- Error_Pragma_Arg_Ident --
      ----------------------------

      procedure Error_Pragma_Arg_Ident (Msg : String; Arg : Node_Id) is
      begin
         Error_Msg_Name_1 := Chars (N);
         Error_Msg_N (Msg, Arg);
         raise Pragma_Exit;
      end Error_Pragma_Arg_Ident;

      ------------------------
      -- Find_Lib_Unit_Name --
      ------------------------

      function Find_Lib_Unit_Name return Entity_Id is
      begin
         --  Return inner compilation unit entity, for case of nested
         --  categorization pragmas. This happens in generic unit.

         if Nkind (Parent (N)) = N_Package_Specification
           and then Defining_Entity (Parent (N)) /= Current_Scope
         then
            return Defining_Entity (Parent (N));

         else
            return Current_Scope;
         end if;
      end Find_Lib_Unit_Name;

      ----------------------------
      -- Find_Program_Unit_Name --
      ----------------------------

      procedure Find_Program_Unit_Name (Id : Node_Id) is
         Unit_Name : Entity_Id;
         Unit_Kind : Node_Kind;
         P         : constant Node_Id := Parent (N);

      begin
         if Nkind (P) = N_Compilation_Unit then
            Unit_Kind := Nkind (Unit (P));

            if Unit_Kind = N_Subprogram_Declaration
              or else Unit_Kind = N_Package_Declaration
              or else Unit_Kind in N_Generic_Declaration
            then
               Unit_Name := Defining_Entity (Unit (P));

               if Chars (Id) = Chars (Unit_Name) then
                  Set_Entity (Id, Unit_Name);
                  Set_Etype (Id, Etype (Unit_Name));
               else
                  Set_Etype (Id, Any_Type);
                  Error_Pragma
                    ("cannot find program unit referenced by pragma%");
               end if;

            else
               Set_Etype (Id, Any_Type);
               Error_Pragma ("pragma% inapplicable to this unit");
            end if;

         else
            Analyze (Id);
         end if;

      end Find_Program_Unit_Name;

      -------------------------
      -- Gather_Associations --
      -------------------------

      procedure Gather_Associations
        (Names : Name_List;
         Args  : out Args_List)
      is
         Arg : Node_Id;

      begin
         GNAT_Pragma;

         --  Initialize all parameters to Empty

         for J in Args'Range loop
            Args (J) := Empty;
         end loop;

         --  That's all we have to do if there are no argument associations

         if No (Pragma_Argument_Associations (N)) then
            return;
         end if;

         --  Otherwise first deal with any positional parameters present

         Arg := First (Pragma_Argument_Associations (N));

         for Index in Args'Range loop
            exit when No (Arg) or else Chars (Arg) /= No_Name;
            Args (Index) := Expression (Arg);
            Arg := Next (Arg);
         end loop;

         --  Positional parameters all processed, if any left, then we
         --  have too many positional parameters.

         if Present (Arg) and then Chars (Arg) = No_Name then
            Error_Pragma_Arg
              ("too many positional associations for pragma%", Arg);
         end if;

         --  Process named parameters if any are present

         while Present (Arg) loop
            if Chars (Arg) = No_Name then
               Error_Pragma_Arg
                 ("positional association cannot follow named association",
                  Arg);

            else
               for Index in Names'Range loop
                  if Names (Index) = Chars (Arg) then
                     if Present (Args (Index)) then
                        Error_Pragma_Arg
                          ("duplicate argument association for pragma%", Arg);
                     else
                        Args (Index) := Expression (Arg);
                        exit;
                     end if;
                  end if;

                  if Index = Names'Last then
                     Error_Pragma_Arg_Ident
                       ("pragma% does not allow & argument", Arg);
                  end if;
               end loop;
            end if;

            Arg := Next (Arg);
         end loop;
      end Gather_Associations;

      -----------------
      -- GNAT_Pragma --
      -----------------

      procedure GNAT_Pragma is
      begin
         Note_Feature (Implementation_Dependent_Pragmas, Loc);
         Check_Restriction (No_Implementation_Pragmas, N);
      end GNAT_Pragma;

      --------------------------
      -- Is_Before_First_Decl --
      --------------------------

      function Is_Before_First_Decl
        (Pragma_Node : Node_Id;
         Decls       : List_Id)
         return        Boolean
      is
         Item            : Node_Id := First (Decls);

      begin
         --  Only other pragmas can come before this pragma

         loop
            if No (Item) or else Nkind (Item) /= N_Pragma then
               return False;

            elsif Item = Pragma_Node then
               return True;
            end if;

            Item := Next (Item);
         end loop;

      end Is_Before_First_Decl;

      -----------------------------
      -- Is_Configuration_Pragma --
      -----------------------------

      --  A configuration pragma must appear in the context clause of
      --  a compilation unit, at the start of the list (i.e. only other
      --  pragmas may precede it).

      function Is_Configuration_Pragma return Boolean is
         Lis : constant List_Id := List_Containing (N);
         Par : constant Node_Id := Parent (N);
         Prg : Node_Id;

      begin
         --  If no parent, then we are in the configuration pragma file,
         --  so the placement is definitely appropriate.

         if No (Par) then
            return True;

         --  Otherwise we must be in the context clause of a compilation unit
         --  and the only thing allowed before us in the context list is more
         --  configuration pragmas.

         elsif Nkind (Par) = N_Compilation_Unit
           and then Context_Items (Par) = Lis
         then
            Prg := First (Lis);

            loop
               if Prg = N then
                  return True;
               elsif Nkind (Prg) /= N_Pragma then
                  return False;
               end if;

               Prg := Next (Prg);
            end loop;

         else
            return False;
         end if;

      end Is_Configuration_Pragma;

      ----------------------
      -- Pragma_Misplaced --
      ----------------------

      procedure Pragma_Misplaced is
      begin
         Error_Pragma ("incorrect placement of pragma%");
      end Pragma_Misplaced;

      ----------------------------
      -- Pragma_Not_Implemented --
      ----------------------------

      procedure Pragma_Not_Implemented is
      begin
         Error_Pragma ("pragma% not implemented?");
      end Pragma_Not_Implemented;

      ------------------------------------
      -- Process Atomic_Shared_Volatile --
      ------------------------------------

      procedure Process_Atomic_Shared_Volatile is
         E_Id : Node_Id;
         E    : Entity_Id;
         D    : Node_Id;
         K    : Node_Kind;

      begin
         GNAT_Pragma;
         Check_Ada_83_Warning;
         Check_No_Identifiers;
         Check_Arg_Count (1);
         Check_Arg_Is_Local_Name (Arg1);
         E_Id := Expression (Arg1);

         if Etype (E_Id) = Any_Type then
            return;
         end if;

         E := Entity (E_Id);
         D := Declaration_Node (E);
         K := Nkind (D);

         if Is_Type (E) then
            if Rep_Item_Too_Early (E, N)
                 or else
               Rep_Item_Too_Late (E, N)
            then
               return;
            else
               Check_First_Subtype (Arg1);
            end if;

            if Prag_Id /= Pragma_Volatile then
               Set_Is_Atomic (E);
               Set_Is_Atomic (Implementation_Type (E));
            end if;

            Set_Is_Volatile (E);
            Set_Is_Volatile (Implementation_Type (E));

         elsif K = N_Object_Declaration
           or else (K = N_Component_Declaration
                     and then Original_Record_Component (E) = E)
         then
            if Rep_Item_Too_Late (E, N) then
               return;
            end if;

            if Prag_Id /= Pragma_Volatile then
               Set_Is_Atomic (E);
            end if;

            Set_Is_Volatile (E);

         else
            Error_Pragma_Arg
              ("inappropriate entity for pragma%", Arg1);
         end if;
      end Process_Atomic_Shared_Volatile;

      ------------------------
      -- Process_Convention --
      ------------------------

      procedure Process_Convention
        (C : out Convention_Id;
         E : out Entity_Id)
      is
         Id        : Node_Id;
         E1        : Entity_Id;
         Comp_Unit : Unit_Number_Type;
         Cname     : Name_Id;

         procedure Set_Convention_From_Pragma (E : Entity_Id);
         --  Set convention in entity E, and also flag that the entity has a
         --  convention pragma. If entity is for a private or incomplete type,
         --  also set convention and flag on underlying type. This procedure
         --  also deals with the special case of C_Pass_By_Copy convention.

         procedure Set_Convention_From_Pragma (E : Entity_Id) is
         begin
            Set_Convention (E, C);
            Set_Has_Convention_Pragma (E);

            if Is_Incomplete_Or_Private_Type (E) then
               Set_Convention            (Underlying_Type (E), C);
               Set_Has_Convention_Pragma (Underlying_Type (E), True);
            end if;

            --  If the entity is a record type, then check for special case of
            --  C_Pass_By_Copy, which is treated the same as C except that the
            --  special record flag is set.

            if Cname = Name_C_Pass_By_Copy then
               if Is_Record_Type (E) then
                  Set_C_Pass_By_Copy (Base_Type (E));
               elsif Is_Incomplete_Or_Private_Type (E)
                 and then Is_Record_Type (Underlying_Type (E))
               then
                  Set_C_Pass_By_Copy (Base_Type (Underlying_Type (E)));
               end if;
            end if;

         end Set_Convention_From_Pragma;

      --  Start of processing for Process_Convention

      begin
         Check_At_Least_N_Arguments (2);
         Check_Arg_Is_Identifier (Arg1);
         Check_Optional_Identifier (Arg1, Name_Convention);
         Cname := Chars (Expression (Arg1));

         --  C_Pass_By_Copy is treated as a synonym for convention C
         --  (this is tested again below to set the critical flag)

         if Cname = Name_C_Pass_By_Copy then
            C := Convention_C;

         --  Otherwise we must have something in the standard convention list

         elsif Is_Convention_Name (Cname) then
            C := Get_Convention_Id (Chars (Expression (Arg1)));

         --  In DEC VMS, it seems that there is an undocumented feature
         --  that any unrecognized convention is treated as the default,
         --  which for us is convention C. It does not seem so terrible
         --  to do this unconditionally, silently in the VMS case, and
         --  with a warning in the non-VMS case.

         else
            if not (OpenVMS or Debug_Flag_M) then
               Error_Msg_N
                 ("?unrecognized convention name, C assumed",
                  Expression (Arg1));
            end if;

            C := Convention_C;
         end if;

         Check_Arg_Is_Local_Name (Arg2);
         Check_Optional_Identifier (Arg2, Name_Entity);

         Id := Expression (Arg2);

         --  The following if is highly suspicious. It was derived from
         --  the code in 1.181 which handles intrinsic quite separately.
         --  It does not work to do Analyze (Id) for the case of an
         --  operator symbol to which pragma Convention Intrinsic is
         --  applied, so presumably this code is wrong for specifying
         --  a foreign convention for an operator ???

         if C = Convention_Intrinsic then
            Find_Program_Unit_Name (Id);
         else
            Analyze (Id);

            if not Is_Entity_Name (Id) then
               Error_Pragma_Arg ("entity name required", Arg2);
            end if;
         end if;

         E := Entity (Id);

         if Is_Subprogram (E)
           and then Nkind (Parent (Declaration_Node (E))) = N_Subprogram_Body
         then
            Error_Pragma
              ("pragma% requires separate spec and must come before body");
         end if;

         if Etype (E) = Any_Type
           or else Rep_Item_Too_Early (E, N)
         then
            raise Pragma_Exit;
         else
            E := Underlying_Type (E);
         end if;

         if Rep_Item_Too_Late (E, N) then
            raise Pragma_Exit;
         end if;

         if Has_Convention_Pragma (E) then
            Error_Pragma_Arg
              ("at most one Convention/Export/Import pragma is allowed", Arg2);

         elsif Convention (E) = Convention_Protected
           or else Ekind (Scope (E)) = E_Protected_Type
         then
            Error_Pragma_Arg
              ("a protected operation cannot be given a different convention",
                Arg2);
         end if;

         --  For Intrinsic, a subprogram is required

         if C = Convention_Intrinsic
           and then not Is_Subprogram (E)
           and then not Is_Generic_Subprogram (E)
         then
            Error_Pragma_Arg
              ("second argument of pragma% must be a subprogram", Arg2);
         end if;

         --  For Stdcall, a subprogram, variable or subprogram type is required

         if C = Convention_Stdcall
           and then not Is_Subprogram (E)
           and then not Is_Generic_Subprogram (E)
           and then Ekind (E) /= E_Variable
           and then not
             (Is_Access_Type (E)
              and then Ekind (Designated_Type (E)) = E_Subprogram_Type)
         then
            Error_Pragma_Arg
              ("second argument of pragma% must be subprogram (type)",
               Arg2);
         end if;

         if not Is_Subprogram (E)
           and then not Is_Generic_Subprogram (E)
         then
            Set_Convention_From_Pragma (E);

            if Is_Type (E) then

               Check_First_Subtype (Arg2);
               Set_Convention_From_Pragma (Base_Type (E));

               --  For subprograms, we must set the convention on the
               --  internally generated directly designated type as well.

               if Ekind (E) = E_Access_Subprogram_Type then
                  Set_Convention_From_Pragma (Directly_Designated_Type (E));
               end if;
            end if;

         --  For the subprogram case, set proper convention for all homonyms
         --  in same compilation unit.
         --  Is the test of compilation unit really necessary ???
         --  What about subprogram renamings here???

         else
            Comp_Unit := Get_Sloc_Unit_Number (Sloc (E));
            Set_Convention_From_Pragma (E);

            E1 := E;
            loop
               E1 := Homonym (E1);
               exit when No (E1) or else Scope (E1) /= Current_Scope;

               --  Note: below we are missing a check for Rep_Item_Too_Late.
               --  That is deliberate, we cannot chain the rep item on more
               --  than one Rep_Item chain, to be fixed later ???

               if Comp_Unit = Get_Sloc_Unit_Number (Sloc (E1)) then
                  Set_Convention_From_Pragma (E1);
               end if;
            end loop;
         end if;

      end Process_Convention;

      -----------------------------------------------------
      -- Process_Extended_Import_Export_Exception_Pragma --
      -----------------------------------------------------

      procedure Process_Extended_Import_Export_Exception_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Form     : Node_Id;
         Arg_Code     : Node_Id)
      is
         Def_Id   : Entity_Id;
         Code_Val : Uint;

      begin
         Process_Extended_Import_Export_Internal_Arg (Arg_Internal);
         Def_Id := Entity (Arg_Internal);

         if Ekind (Def_Id) /= E_Exception then
            Error_Pragma_Arg
              ("pragma% must refer to declared exception", Arg_Internal);
         end if;

         Set_Extended_Import_Export_External_Name (Def_Id, Arg_External);

         if Present (Arg_Form) then
            Check_Arg_Is_One_Of (Arg_Form, Name_Ada, Name_VMS);
         end if;

         if Present (Arg_Form)
           and then Chars (Arg_Form) = Name_Ada
         then
            null;
         else
            Set_Is_VMS_Exception (Def_Id);
         end if;

         if Present (Arg_Code) then
            if not Is_VMS_Exception (Def_Id) then
               Error_Pragma_Arg
                 ("Code option for pragma% not allowed for Ada case",
                  Arg_Code);
            end if;

            Check_Arg_Is_Static_Expression (Arg_Code, Any_Integer);
            Code_Val := Expr_Value (Arg_Code);

            if not UI_Is_In_Int_Range (Code_Val) then
               Error_Pragma_Arg
                 ("Code option for pragma% must be in 32-bit range",
                  Arg_Code);

            else
               Set_Exception_Code (Def_Id, Code_Val);
            end if;
         end if;

      end Process_Extended_Import_Export_Exception_Pragma;

      --------------------------------------------------
      -- Process_Extended_Import_Export_Object_Pragma --
      --------------------------------------------------

      procedure Process_Extended_Import_Export_Object_Pragma
        (Arg_Internal : Node_Id;
         Arg_External : Node_Id;
         Arg_Size     : Node_Id)
      is
         Def_Id   : Entity_Id;

      begin
         Process_Extended_Import_Export_Internal_Arg (Arg_Internal);
         Def_Id := Entity (Arg_Internal);

         if Ekind (Def_Id) /= E_Constant
           and then Ekind (Def_Id) /= E_Variable
         then
            Error_Pragma_Arg
              ("pragma% must designate an object", Arg_Internal);
         end if;

         if Rep_Item_Too_Late (Def_Id, N) then
            raise Pragma_Exit;
         end if;

         Set_Extended_Import_Export_External_Name (Def_Id, Arg_External);

         if Present (Arg_Size)
           and then Nkind (Arg_Size) /= N_Identifier
           and then Nkind (Arg_Size) /= N_String_Literal
         then
            Error_Pragma_Arg
              ("pragma% Size argument must be identifier or string literal",
               Arg_Size);
         end if;

         if Prag_Id = Pragma_Export_Object then
            Set_Is_Exported (Def_Id);

         else
            Set_Is_Imported (Def_Id);

            if Prag_Id /= Pragma_Import_Object then
               Set_Is_Common_Object (Def_Id);
            end if;
         end if;

      end Process_Extended_Import_Export_Object_Pragma;

      -------------------------------------------------
      -- Process_Extended_Import_Export_Internal_Arg --
      -------------------------------------------------

      procedure Process_Extended_Import_Export_Internal_Arg
        (Arg_Internal : Node_Id := Empty)
      is
      begin
         GNAT_Pragma;

         if No (Arg_Internal) then
            Error_Pragma ("Internal parameter required for pragma%");
         end if;

         if Nkind (Arg_Internal) = N_Identifier then
            null;

         elsif Nkind (Arg_Internal) = N_Operator_Symbol
           and then (Prag_Id = Pragma_Import_Function
                       or else
                     Prag_Id = Pragma_Export_Function)
         then
            null;

         else
            Error_Pragma_Arg
              ("wrong form for Internal parameter for pragma%", Arg_Internal);
         end if;

         Check_Arg_Is_Local_Name (Arg_Internal);

      end Process_Extended_Import_Export_Internal_Arg;

      ------------------------------------------------------
      -- Process_Extended_Import_Export_Subprogram_Pragma --
      ------------------------------------------------------

      procedure Process_Extended_Import_Export_Subprogram_Pragma
        (Arg_Internal                 : Node_Id;
         Arg_External                 : Node_Id;
         Arg_Parameter_Types          : Node_Id;
         Arg_Result_Type              : Node_Id := Empty;
         Arg_Mechanism                : Node_Id;
         Arg_Result_Mechanism         : Node_Id := Empty;
         Arg_First_Optional_Parameter : Node_Id := Empty)
      is
         Ent       : Entity_Id;
         Def_Id    : Entity_Id;
         Hom_Id    : Entity_Id;
         Formal    : Entity_Id;
         Ambiguous : Boolean;
         Match     : Boolean;
         Dval      : Node_Id;

         function Same_Type (Ptype, Formal : Entity_Id) return Boolean;
         --  Determines if Ptype references the type of Formal. Note that
         --  only the base types need to match according to the spec.

         function Same_Type (Ptype, Formal : Entity_Id) return Boolean is
         begin
            Find_Type (Ptype);

            if not Is_Entity_Name (Ptype)
              or else Entity (Ptype) = Any_Type
            then
               raise Pragma_Exit;
            end if;

            return Base_Type (Entity (Ptype)) = Base_Type (Etype (Formal));
         end Same_Type;

      --  Start of processing for
      --  Process_Extended_Import_Export_Subprogram_Pragma

      begin
         Process_Extended_Import_Export_Internal_Arg (Arg_Internal);
         Hom_Id := Entity (Arg_Internal);
         Ent := Empty;
         Ambiguous := False;

         --  Loop through homonyms (overloadings) of Hom_Id

         while Present (Hom_Id) loop
            Def_Id := Get_Base_Subprogram (Hom_Id);

            --  Treat only subprograms that are not derived subprograms.
            --  We don't treat derived subprograms, since it is the parent
            --  subprogram which is actually called and must be imported.

            if not Is_Subprogram (Def_Id)
              or else Scope (Def_Id) /= Current_Scope
              or else (Is_Overloadable (Def_Id)
                         and then Present (Alias (Def_Id)))
            then
               null;

            else
               Match := True;

               --  Pragma cannot apply to subprogram body

               if Is_Subprogram (Def_Id)
                 and then
                   Nkind (Parent
                     (Declaration_Node (Def_Id))) = N_Subprogram_Body
               then
                  Error_Pragma
                    ("pragma% requires separate spec"
                      & " and must come before body");
               end if;

               --  Test result type if given, note that the result type
               --  parameter can only be present for the function cases.

               if Present (Arg_Result_Type)
                 and then not Same_Type (Arg_Result_Type, Def_Id)
               then
                  Match := False;

               --  Test parameter types if given. Note that this parameter
               --  has not been analyzed (and must not be, since it is
               --  semantic nonsense), so we get it as the parser left it.

               elsif Present (Arg_Parameter_Types) then
                  Check_Matching_Types : declare
                     Formal : Entity_Id;
                     Ptype  : Node_Id;

                  begin
                     Formal := First_Formal (Def_Id);

                     if Nkind (Arg_Parameter_Types) = N_Null then
                        if Present (Formal) then
                           Match := False;
                        end if;

                     --  A list of one type, e.g. (List) is parsed as
                     --  a parenthesized expression.

                     elsif Nkind (Arg_Parameter_Types) /= N_Aggregate
                       and then Paren_Count (Arg_Parameter_Types) = 1
                     then
                        if No (Formal)
                          or else Present (Next_Formal (Formal))
                        then
                           Match := False;
                        else
                           Match := Same_Type (Arg_Parameter_Types, Formal);
                        end if;

                     --  A list of more than one type is parsed as a aggregate

                     elsif Nkind (Arg_Parameter_Types) = N_Aggregate
                       and then Paren_Count (Arg_Parameter_Types) = 0
                     then
                        Ptype := First (Expressions (Arg_Parameter_Types));

                        while Present (Ptype) or else Present (Formal) loop
                           if No (Ptype)
                             or else No (Formal)
                             or else not Same_Type (Ptype, Formal)
                           then
                              Match := False;
                              exit;
                           else
                              Formal := Next_Formal (Formal);
                              Ptype := Next (Ptype);
                           end if;
                        end loop;

                     --  Anything else is of the wrong form

                     else
                        Error_Pragma_Arg
                          ("wrong form for Parameter_Types parameter",
                           Arg_Parameter_Types);
                     end if;
                  end Check_Matching_Types;
               end if;

               --  Match is now False if the entry we found did not match
               --  either a supplied Parameter_Types or Result_Types argument

               if Match then
                  if No (Ent) then
                     Ent := Def_Id;

                  --  Ambiguous case, the flag Ambiguous shows if we already
                  --  detected this and output the initial messages.

                  else
                     if not Ambiguous then
                        Ambiguous := True;
                        Error_Msg_Name_1 := Chars (N);
                        Error_Msg_N
                          ("pragma% does not uniquely identify subprogram!",
                           N);
                        Error_Msg_Sloc := Sloc (Ent);
                        Error_Msg_N ("matching subprogram at #!", N);
                        Ent := Empty;
                     end if;

                     Error_Msg_Sloc := Sloc (Def_Id);
                     Error_Msg_N ("matching subprogram at #!", N);
                  end if;
               end if;
            end if;

            Hom_Id := Homonym (Hom_Id);
         end loop;

         --  See if we found an entry

         if No (Ent) then
            if not Ambiguous then
               if Is_Generic_Subprogram (Entity (Arg_Internal)) then
                  Error_Pragma
                    ("pragma% cannot be given for generic subprogram");

               else
                  Error_Pragma
                    ("pragma% does not identify local subprogram");
               end if;
            end if;

            return;
         end if;

         --  Import pragmas must be be for imported entities

         if (Prag_Id = Pragma_Import_Function
               or else
             Prag_Id = Pragma_Import_Procedure
               or else
             Prag_Id = Pragma_Import_Valued_Procedure)
         then
            if not Is_Imported (Ent) then
               Error_Pragma
                 ("pragma Import or Interface must precede pragma%");
            end if;

         --  For the Export cases, the pragma Export is sufficient to set
         --  the entity as exported, if it is not exported already. We
         --  leave the default Ada convention in this case.

         else
            Set_Is_Exported (Ent);
         end if;

         --  Special processing for Valued_Procedure cases

         if Prag_Id = Pragma_Import_Valued_Procedure
           or else
            Prag_Id = Pragma_Export_Valued_Procedure
         then
            Formal := First_Formal (Ent);

            if No (Formal) then
               Error_Pragma
                 ("at least one parameter required for pragma%");

            elsif Ekind (Formal) /= E_Out_Parameter then
               Error_Pragma
                 ("first parameter must have mode out for pragma%");

            else
               Set_Is_Valued_Procedure (Ent);
            end if;
         end if;

         Set_Extended_Import_Export_External_Name (Ent, Arg_External);

         --  Process Result_Mechanism argument if present. We have already
         --  checked that this is only allowed for the function case.

         if Present (Arg_Result_Mechanism) then
            Set_Mechanism_Value (Ent, Arg_Result_Mechanism);
         end if;

         --  Process Mechanism parameter if present. Note that this parameter
         --  is not analyzed, and must not be analyzed since it is semantic
         --  nonsense, so we get it in exactly as the parser left it.

         if Present (Arg_Mechanism) then

            declare
               Formal : Entity_Id;
               Massoc : Node_Id;
               Mname  : Node_Id;
               Choice : Node_Id;

            begin
               --  A single mechanism association without a formal parameter
               --  name is parsed as a parenthesized expression. All other
               --  cases are parsed as aggregates, so we rewrite the single
               --  parameter case as an aggregate for consistency.

               if Nkind (Arg_Mechanism) /= N_Aggregate
                 and then Paren_Count (Arg_Mechanism) = 1
               then
                  Rewrite_Substitute_Tree (Arg_Mechanism,
                    Make_Aggregate (Sloc (Arg_Mechanism),
                      Expressions => New_List (
                        Relocate_Node (Arg_Mechanism))));
               end if;

               --  Case of only mechanism name given, applies to all formals

               if Nkind (Arg_Mechanism) /= N_Aggregate then
                  Formal := First_Formal (Ent);
                  while Present (Formal) loop
                     Set_Mechanism_Value (Formal, Arg_Mechanism);
                     Formal := Next_Formal (Formal);
                  end loop;

               --  Case of list of mechanism associations given

               else
                  if Null_Record_Present (Arg_Mechanism) then
                     Error_Pragma_Arg
                       ("inappropriate form for Mechanism parameter",
                        Arg_Mechanism);
                  end if;

                  --  Deal with positional ones first

                  Formal := First_Formal (Ent);
                  if Present (Expressions (Arg_Mechanism)) then
                     Mname := First (Expressions (Arg_Mechanism));

                     while Present (Mname) loop
                        if No (Formal) then
                           Error_Pragma_Arg
                             ("too many mechanism associations", Mname);
                        end if;

                        Set_Mechanism_Value (Formal, Mname);
                        Formal := Next_Formal (Formal);
                        Mname := Next (Mname);
                     end loop;
                  end if;

                  --  Deal with named entries

                  if Present (Component_Associations (Arg_Mechanism)) then
                     Massoc := First (Component_Associations (Arg_Mechanism));

                     while Present (Massoc) loop
                        Choice := First (Choices (Massoc));

                        if Nkind (Choice) /= N_Identifier
                          or else Present (Next (Choice))
                        then
                           Error_Pragma_Arg
                             ("incorrect form for mechanism association",
                              Massoc);
                        end if;

                        Formal := First_Formal (Ent);
                        loop
                           if No (Formal) then
                              Error_Pragma_Arg
                                ("parameter name & not present", Choice);
                           end if;

                           if Chars (Choice) = Chars (Formal) then
                              Set_Mechanism_Value
                                (Formal, Expression (Massoc));
                              exit;
                           end if;

                           Formal := Next_Formal (Formal);
                        end loop;

                        Massoc := Next (Massoc);
                     end loop;
                  end if;
               end if;
            end;
         end if;

         --  Process First_Optional_Parameter argument if present. We have
         --  already checked that this is only allowed for the Import case.

         if Present (Arg_First_Optional_Parameter) then
            if Nkind (Arg_First_Optional_Parameter) /= N_Identifier then
               Error_Pragma_Arg
                 ("first optional parameter must be formal parameter name",
                  Arg_First_Optional_Parameter);
            end if;

            Formal := First_Formal (Ent);
            loop
               if No (Formal) then
                  Error_Pragma_Arg
                    ("specified formal parameter& not found",
                     Arg_First_Optional_Parameter);
               end if;

               exit when Chars (Formal) =
                         Chars (Arg_First_Optional_Parameter);

               Formal := Next_Formal (Formal);
            end loop;

            Set_First_Optional_Parameter (Ent, Formal);

            --  Check specified and all remaining formals have right form

            while Present (Formal) loop
               if Ekind (Formal) /= E_In_Parameter then
                  Error_Msg_NE
                    ("optional formal& is not of mode in!",
                     Arg_First_Optional_Parameter, Formal);

               else
                  Dval := Default_Value (Formal);

                  if not Present (Dval) then
                     Error_Msg_NE
                       ("optional formal& does not have default value!",
                        Arg_First_Optional_Parameter, Formal);

                  elsif Compile_Time_Known_Value_Or_Aggr (Dval) then
                     null;

                  else
                     Error_Msg_NE
                       ("default value for optional formal& is non-static!",
                        Arg_First_Optional_Parameter, Formal);
                  end if;
               end if;

               Set_Is_Optional_Parameter (Formal);
               Formal := Next_Formal (Formal);
            end loop;
         end if;

      end Process_Extended_Import_Export_Subprogram_Pragma;

      --------------------------
      -- Process_Generic_List --
      --------------------------

      procedure Process_Generic_List is
         Arg : Node_Id;
         Exp : Node_Id;

      begin
         GNAT_Pragma;
         Check_No_Identifiers;
         Check_At_Least_N_Arguments (1);

         Arg := Arg1;
         while Present (Arg) loop
            Exp := Expression (Arg);
            Analyze (Exp);

            if not Is_Entity_Name (Exp)
              or else
                (not Is_Generic_Instance (Entity (Exp))
                  and then
                 not Is_Generic_Unit (Entity (Exp)))
            then
               Error_Pragma_Arg
                 ("pragma% argument must be name of generic unit/instance",
                  Arg);
            end if;

            Arg := Next (Arg);
         end loop;
      end Process_Generic_List;

      ---------------------------------
      -- Process_Import_Or_Interface --
      ---------------------------------

      procedure Process_Import_Or_Interface is
         C      : Convention_Id;
         Def_Id : Entity_Id;
         Hom_Id : Entity_Id;

      begin
         Process_Convention (C, Def_Id);
         Kill_Size_Check_Code (Def_Id);
         Note_Possible_Modification (Expression (Arg2));

         if Ekind (Def_Id) = E_Variable then

            --  Initialization is not allowed for imported variable
            --  The No_Location is used to mark the default initialization
            --  of access types

            --  Use of No_Location here is really ugly???

            if Present (Expression (Parent (Def_Id)))
               and then Sloc (Expression (Parent (Def_Id))) /= No_Location
            then
               Error_Msg_Sloc := Sloc (Def_Id);
               Error_Pragma_Arg
                 ("no initialization allowed for declaration of& #", Arg2);

            else
               Set_Is_Imported (Def_Id);
               Set_Is_Public (Def_Id);
               Process_Interface_Name (Def_Id, Arg3, Arg4);
            end if;

         elsif Is_Subprogram (Def_Id)
           or else Is_Generic_Subprogram (Def_Id)
         then
            --  If name is overloaded, pragma applies to all the
            --  denoted entities in the same declarative part.

            Hom_Id := Def_Id;

            while Present (Hom_Id) loop
               Def_Id := Get_Base_Subprogram (Hom_Id);

               --  Ignore inherited subprograms because the pragma will
               --  apply to the parent operation, which is the one called.

               if Is_Overloadable (Def_Id)
                 and then Present (Alias (Def_Id))
               then
                  null;

               --  What exactly is the following test for ???

               elsif
                 Parent (Get_Declaration_Node (Def_Id)) /= Parent (N)
               then
                  exit;

               else
                  Set_Is_Imported (Def_Id);

                  --  If Import intrinsic, set intrinsic flag
                  --  and verify that it is known as such.

                  if C = Convention_Intrinsic then
                     Set_Is_Intrinsic_Subprogram (Def_Id);
                     Check_Intrinsic_Subprogram
                       (Def_Id, Expression (Arg2));
                  end if;

                  --  All interfaced procedures need an external
                  --  symbol created for them since they are
                  --  always referenced from another object file.

                  Set_Is_Public (Def_Id);
                  Set_Has_Completion (Def_Id);
                  Process_Interface_Name (Def_Id, Arg3, Arg4);
               end if;

               Hom_Id := Homonym (Hom_Id);
            end loop;

         else
            Error_Pragma_Arg
              ("second argument of pragma% must be subprogram or variable",
               Arg2);
         end if;

         --  If this pragma applies to a compilation unit, then the unit,
         --  which is a subprogram, does not require (or allow) a body.
         --  We also do not need to elaborate imported procedures.

         if Nkind (Parent (N)) = N_Compilation_Unit then
            Set_Body_Required    (Parent (N), False);
            Set_Has_No_Elab_Code (Parent (N), False);
         end if;

      end Process_Import_Or_Interface;

      ----------------------------
      -- Process_Interface_Name --
      ----------------------------

      procedure Process_Interface_Name
        (Subprogram_Def : Entity_Id;
         Ext_Arg        : Node_Id;
         Link_Arg       : Node_Id)
      is
         Ext_Nam    : Node_Id;
         Link_Nam   : Node_Id;
         String_Val : String_Id;

         procedure Check_Form_Of_Interface_Name (SN : Node_Id);
         --  SN is a string literal node for an interface name. This routine
         --  performs some minimal checks that the name is reasonable. In
         --  particular that no spaces or other obviously incorrect characters
         --  appear. This is only a warning, since any characters are allowed.

         procedure Check_Form_Of_Interface_Name (SN : Node_Id) is
            S  : constant String_Id := Strval (Expr_Value_S (SN));
            SL : constant Nat       := String_Length (S);
            C  : Char_Code;

         begin
            if SL = 0 then
               Error_Msg_N ("interface name cannot be null string", SN);
            end if;

            for J in 1 .. SL loop
               C := Get_String_Char (S, J);

               if not In_Character_Range (C)
                 or else Get_Character (C) = ' '
                 or else Get_Character (C) = ','
               then
                  Error_Msg_N
                    ("?interface name contains illegal character", SN);
               end if;
            end loop;
         end Check_Form_Of_Interface_Name;

      --  Start of processing for Process_Interface_Name

      begin
         if No (Link_Arg) then
            if No (Ext_Arg) then
               return;

            elsif Chars (Ext_Arg) = No_Name
              or else Chars (Ext_Arg) = Name_External_Name
            then
               Ext_Nam  := Expression (Ext_Arg);
               Link_Nam := Empty;

            else
               Ext_Nam  := Empty;
               Link_Nam := Expression (Ext_Arg);
            end if;

         else
            Ext_Nam  := Expression (Ext_Arg);
            Link_Nam := Expression (Link_Arg);
         end if;

         --  Check expressions for external name and link name are static

         if Present (Ext_Nam) then
            Check_Arg_Is_Static_Expression (Ext_Nam, Standard_String);
            Check_Form_Of_Interface_Name (Ext_Nam);
         end if;

         if Present (Link_Nam) then
            Check_Arg_Is_Static_Expression (Link_Nam, Standard_String);
            Check_Form_Of_Interface_Name (Link_Nam);
         end if;

         --  If there is no link name, just set the external name

         if No (Link_Nam) then
            Set_Interface_Name (Subprogram_Def, Expr_Value_S (Ext_Nam));

         --  For the Link_Name case, the given literal is preceded by an
         --  asterisk, which indicates to GCC that the given name should
         --  be taken literally, and in particular that no prepending of
         --  underlines should occur, even in systems where this is the
         --  normal default.

         else
            Start_String;
            Store_String_Char (Get_Char_Code ('*'));
            String_Val := Strval (Expr_Value_S (Link_Nam));

            for J in 1 .. String_Length (String_Val) loop
               Store_String_Char (Get_String_Char (String_Val, J));
            end loop;

            Link_Nam :=
              Make_String_Literal (Sloc (Link_Nam), End_String);

            Set_Interface_Name (Subprogram_Def, Link_Nam);
         end if;
      end Process_Interface_Name;

      ---------------------------------
      -- Process_Suppress_Unsuppress --
      ---------------------------------

      procedure Process_Suppress_Unsuppress (Sense : Boolean) is
         C         : Check_Id;
         E_Id      : Node_Id;
         E         : Entity_Id;
         Effective : Boolean;

         procedure Suppress_Unsuppress_Echeck (E : Entity_Id; C : Check_Id);
         --  Used to suppress a single check on the given entity

         procedure Suppress_Unsuppress_Echeck (E : Entity_Id; C : Check_Id) is
         begin
            --  First set appropriate suppress flags in the entity

            case C is
               when Access_Check =>
                  Effective := Suppress_Access_Checks (E);
                  Set_Suppress_Access_Checks (E, Sense);

               when Accessibility_Check =>
                  Effective := Suppress_Accessibility_Checks (E);
                  Set_Suppress_Accessibility_Checks (E, Sense);

               when Discriminant_Check =>
                  Effective := Suppress_Discriminant_Checks  (E);
                  Set_Suppress_Discriminant_Checks (E, Sense);

               when Division_Check =>
                  Effective := Suppress_Division_Checks (E);
                  Set_Suppress_Division_Checks (E, Sense);

               when Elaboration_Check =>
                  Effective := Suppress_Elaboration_Checks (E);
                  Set_Suppress_Elaboration_Checks (E, Sense);

               when Index_Check =>
                  Effective := Suppress_Index_Checks (E);
                  Set_Suppress_Index_Checks (E, Sense);

               when Length_Check =>
                  Effective := Suppress_Length_Checks (E);
                  Set_Suppress_Length_Checks (E, Sense);

               when Overflow_Check =>
                  Effective := Suppress_Overflow_Checks (E);
                  Set_Suppress_Overflow_Checks (E, Sense);

               when Range_Check =>
                  Effective := Suppress_Range_Checks (E);
                  Set_Suppress_Range_Checks (E, Sense);

               when Storage_Check =>
                  Effective := Suppress_Storage_Checks (E);
                  Set_Suppress_Storage_Checks (E, Sense);

               when Tag_Check =>
                  Effective := Suppress_Tag_Checks (E);
                  Set_Suppress_Tag_Checks (E, Sense);

               when All_Checks =>
                  Suppress_Unsuppress_Echeck (E, Access_Check);
                  Suppress_Unsuppress_Echeck (E, Accessibility_Check);
                  Suppress_Unsuppress_Echeck (E, Discriminant_Check);
                  Suppress_Unsuppress_Echeck (E, Division_Check);
                  Suppress_Unsuppress_Echeck (E, Elaboration_Check);
                  Suppress_Unsuppress_Echeck (E, Index_Check);
                  Suppress_Unsuppress_Echeck (E, Length_Check);
                  Suppress_Unsuppress_Echeck (E, Overflow_Check);
                  Suppress_Unsuppress_Echeck (E, Range_Check);
                  Suppress_Unsuppress_Echeck (E, Storage_Check);
                  Suppress_Unsuppress_Echeck (E, Tag_Check);
            end case;

            --  If the entity is not declared in the current scope, then we
            --  make an entry in the Entity_Suppress table so that the flag
            --  will be removed on exit. This entry is only made if the
            --  suppress did something (i.e. the flag was not already set).

            if Effective and then Scope (E) /= Current_Scope then
               Entity_Suppress.Increment_Last;
               Entity_Suppress.Table
                 (Entity_Suppress.Last).Entity := E;
               Entity_Suppress.Table
                 (Entity_Suppress.Last).Check  := C;
            end if;
         end Suppress_Unsuppress_Echeck;

      --  Start of processing for Process_Suppress_Unsuppress

      begin
         --  Suppress/Unsuppress can appear as a configuration pragma,
         --  or in a declarative part or a package spec (RM 11.5(5))

         if not Is_Configuration_Pragma then
            Check_Is_In_Decl_Part_Or_Package_Spec;
         end if;

         Check_At_Least_N_Arguments (1);
         Check_At_Most_N_Arguments (2);
         Check_No_Identifier (Arg1);
         Check_Arg_Is_Identifier (Arg1);

         if not Is_Check_Name (Chars (Expression (Arg1))) then
            Error_Msg
              ("argument of pragma% is not valid check name",
                Sloc (Expression (Arg1)));

         else
            C := Get_Check_Id (Chars (Expression (Arg1)));
         end if;

         if Arg_Count = 1 then
            case C is
               when Access_Check =>
                  Scope_Suppress.Access_Checks := Sense;

               when Accessibility_Check =>
                  Scope_Suppress.Accessibility_Checks := Sense;

               when Discriminant_Check =>
                  Scope_Suppress.Discriminant_Checks := Sense;

               when Division_Check =>
                  Scope_Suppress.Division_Checks := Sense;

               when Elaboration_Check =>
                  Scope_Suppress.Elaboration_Checks := Sense;

               when Index_Check =>
                  Scope_Suppress.Index_Checks := Sense;

               when Length_Check =>
                  Scope_Suppress.Length_Checks := Sense;

               when Overflow_Check =>
                  Scope_Suppress.Overflow_Checks := Sense;

               when Range_Check =>
                  Scope_Suppress.Range_Checks := Sense;

               when Storage_Check =>
                  Scope_Suppress.Storage_Checks := Sense;

               when Tag_Check =>
                  Scope_Suppress.Tag_Checks := Sense;

               when All_Checks =>
                  Scope_Suppress := (others => Sense);

            end case;

         --  Case of two arguments present, where the check is
         --  suppressed for a specified entity (given as the second
         --  argument of the pragma)

         else
            Check_Optional_Identifier (Arg2, Name_On);
            E_Id := Expression (Arg2);
            Analyze (E_Id);

            if not Is_Entity_Name (E_Id) then
               Error_Pragma_Arg
                 ("second argument of pragma% must be entity name", Arg2);
            end if;

            E := Entity (E_Id);

            if E = Any_Id then
               return;
            else
               loop
                  Suppress_Unsuppress_Echeck (E, C);
                  exit when No (Homonym (E));
                  E := Homonym (E);
               end loop;
            end if;
         end if;

      end Process_Suppress_Unsuppress;

      ----------------------------------------------
      -- Set_Extended_Import_Export_External_Name --
      ----------------------------------------------

      procedure Set_Extended_Import_Export_External_Name
        (Internal_Ent : Entity_Id;
         Arg_External : Node_Id)
      is
         Old_Name : constant Node_Id := Interface_Name (Internal_Ent);
         New_Name : Node_Id;

      begin
         if No (Arg_External) then
            return;

         elsif Nkind (Arg_External) = N_String_Literal then
            if String_Length (Strval (Arg_External)) = 0 then
               return;
            else
               New_Name := Arg_External;
            end if;

         elsif Nkind (Arg_External) = N_Identifier then
            New_Name := Get_Default_External_Name (Arg_External);

         else
            Error_Pragma_Arg
              ("incorrect form for External parameter for pragma%",
               Arg_External);
         end if;

         --  If we already have an external name set (by a prior normal
         --  Import or Export pragma), then the external names must match

         if Present (Interface_Name (Internal_Ent)) then
            declare
               S1 : constant String_Id := Strval (Old_Name);
               S2 : constant String_Id := Strval (New_Name);

               procedure Mismatch;
               --  Called if names do not match

               procedure Mismatch is
               begin
                  Error_Msg_Sloc := Sloc (Old_Name);
                  Error_Pragma_Arg
                    ("external name does not match that given #",
                     Arg_External);
               end Mismatch;

            begin
               if String_Length (S1) /= String_Length (S2) then
                  Mismatch;

               else
                  for J in 1 .. String_Length (S1) loop
                     if Get_String_Char (S1, J) /= Get_String_Char (S2, J) then
                        Mismatch;
                     end if;
                  end loop;
               end if;
            end;

         --  Otherwise set the given name

         else
            Set_Interface_Name (Internal_Ent, New_Name);
         end if;

      end Set_Extended_Import_Export_External_Name;

      -------------------------
      -- Set_Mechanism_Value --
      -------------------------

      --  Note: the mechanism name has not been analyzed (and cannot indeed
      --  be analyzed, since it is semantic nonsense), so we get it in the
      --  exact form created by the parser.

      procedure Set_Mechanism_Value (Ent : Entity_Id; Mech_Name : Node_Id) is
         Class : Node_Id;
         Param : Node_Id;

         procedure Bad_Class;
         --  Signal bad descriptor class name

         procedure Bad_Mechanism;
         --  Signal bad mechanism name

         procedure Bad_Class is
         begin
            Error_Pragma_Arg ("unrecognized descriptor class name", Class);
         end Bad_Class;

         procedure Bad_Mechanism is
         begin
            Error_Pragma_Arg ("unrecognized mechanism name", Mech_Name);
         end Bad_Mechanism;

      --  Start of processing for Set_Mechanism_Value

      begin
         if Mechanism (Ent) /= Default_Mechanism then
            Error_Msg_NE
              ("mechanism for & has already been set", Mech_Name, Ent);
         end if;

         --  MECHANISM_NAME ::= value | reference | descriptor

         if Nkind (Mech_Name) = N_Identifier then
            if Chars (Mech_Name) = Name_Value then
               Set_Mechanism (Ent, By_Copy);
               return;

            elsif Chars (Mech_Name) = Name_Reference then
               Set_Mechanism (Ent, By_Reference);
               return;

            elsif Chars (Mech_Name) = Name_Descriptor then
               Check_VMS (Mech_Name);
               Set_Mechanism (Ent, By_Descriptor);
               return;

            elsif Chars (Mech_Name) = Name_Copy then
               Error_Pragma_Arg
                 ("bad mechanism name, Value assumed", Mech_Name);

            else
               Bad_Mechanism;
            end if;

         --  MECHANISM_NAME ::= descriptor (CLASS_NAME)
         --  CLASS_NAME     ::= ubs | ubsb | uba | s | sb | a | nca

         --  Note: this form is parsed as an indexed component

         elsif Nkind (Mech_Name) = N_Indexed_Component then
            Class := First (Expressions (Mech_Name));

            if Nkind (Prefix (Mech_Name)) /= N_Identifier
              or else Chars (Prefix (Mech_Name)) /= Name_Descriptor
              or else Present (Next (Class))
            then
               Bad_Mechanism;
            end if;

         --  MECHANISM_NAME ::= descriptor (Class => CLASS_NAME)
         --  CLASS_NAME     ::= ubs | ubsb | uba | s | sb | a | nca

         --  Note: this form is parsed as a function call

         elsif Nkind (Mech_Name) = N_Function_Call then

            Param := First (Parameter_Associations (Mech_Name));

            if Nkind (Name (Mech_Name)) /= N_Identifier
              or else Chars (Name (Mech_Name)) /= Name_Descriptor
              or else Present (Next (Param))
              or else No (Selector_Name (Param))
              or else Chars (Selector_Name (Param)) /= Name_Class
            then
               Bad_Mechanism;
            else
               Class := Explicit_Actual_Parameter (Param);
            end if;

         else
            Bad_Mechanism;
         end if;

         --  Fall through here with Class set to descriptor class name

         Check_VMS (Mech_Name);

         if Nkind (Class) /= N_Identifier then
            Bad_Class;

         elsif Chars (Class) = Name_UBS then
            Set_Mechanism (Ent, By_Descriptor_UBS);

         elsif Chars (Class) = Name_UBSB then
            Set_Mechanism (Ent, By_Descriptor_UBSB);

         elsif Chars (Class) = Name_UBA then
            Set_Mechanism (Ent, By_Descriptor_UBA);

         elsif Chars (Class) = Name_S then
            Set_Mechanism (Ent, By_Descriptor_S);

         elsif Chars (Class) = Name_SB then
            Set_Mechanism (Ent, By_Descriptor_SB);

         elsif Chars (Class) = Name_A then
            Set_Mechanism (Ent, By_Descriptor_A);

         elsif Chars (Class) = Name_NCA then
            Set_Mechanism (Ent, By_Descriptor_NCA);

         else
            Bad_Class;
         end if;

      end Set_Mechanism_Value;

      --------------------
      -- Set_Pragma_Arg --
      --------------------

      function Set_Pragma_Arg (Arg : Node_Id) return Node_Id is
      begin
         if Nkind (Arg) = N_Pragma_Argument_Association then
            return Expression (Arg);
         else
            return Arg;
         end if;
      end Set_Pragma_Arg;

   --  Start of processing for Analyze_Pragma

   begin
      if not Is_Pragma_Name (Chars (N)) then
         Error_Pragma ("unrecognized pragma%!?");
      else
         Prag_Id := Get_Pragma_Id (Chars (N));
      end if;

      --  Preset arguments

      Arg1 := Empty;
      Arg2 := Empty;
      Arg3 := Empty;
      Arg4 := Empty;

      if Present (Pragma_Argument_Associations (N)) then
         Arg1 := First (Pragma_Argument_Associations (N));

         if Present (Arg1) then
            Arg2 := Next (Arg1);

            if Present (Arg2) then
               Arg3 := Next (Arg2);

               if Present (Arg3) then
                  Arg4 := Next (Arg3);
               end if;
            end if;
         end if;
      end if;

      --  Count number of arguments

      declare
         Arg_Node : Node_Id;

      begin
         Arg_Count := 0;
         Arg_Node := Arg1;

         while Present (Arg_Node) loop
            Arg_Count := Arg_Count + 1;
            Arg_Node := Next (Arg_Node);
         end loop;
      end;

      --  An enumeration type defines the pragmas that are supported by the
      --  implementation. Get_Pragma_Id (in package Prag) transorms a name
      --  into the corresponding enumeration value for the following case.

      case Prag_Id is

         -----------------
         -- Abort_Defer --
         -----------------

         --  pragma Abort_Defer;

         when Pragma_Abort_Defer =>
            GNAT_Pragma;
            Check_Arg_Count (0);

            --  The only required semantic processing is to check the
            --  placement. This pragma must appear at the start of the
            --  statement sequence of a handled sequence of statements.

            if Nkind (Parent (N)) /= N_Handled_Sequence_Of_Statements
              or else N /= First (Statements (Parent (N)))
            then
               Pragma_Misplaced;
            end if;

         ------------
         -- Ada_83 --
         ------------

         --  pragma Ada_83;

         --  Note: this pragma also has some specific processing in Par.Prag
         --  because we want to set the Ada 83 mode switch during parsing.

         when Pragma_Ada_83 =>
            GNAT_Pragma;
            Ada_83 := True;
            Ada_95 := False;
            Check_Arg_Count (0);

         ------------
         -- Ada_95 --
         ------------

         --  pragma Ada_95;

         --  Note: this pragma also has some specific processing in Par.Prag
         --  because we want to set the Ada 83 mode switch during parsing.

         when Pragma_Ada_95 =>
            GNAT_Pragma;
            Ada_83 := False;
            Ada_95 := True;
            Check_Arg_Count (0);

         ----------------------
         -- All_Calls_Remote --
         ----------------------

         --  pragma All_Calls_Remote [(library_package_NAME)];

         when Pragma_All_Calls_Remote => All_Calls_Remote : declare
            Lib_Entity : Entity_Id;

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;
            Lib_Entity := Find_Lib_Unit_Name;

            --  This pragma should only apply to a RCI unit (RM E.2.3(23)).

            if Present (Lib_Entity)
              and then not Debug_Flag_U
            then
               if not Is_Remote_Call_Interface (Lib_Entity) then
                  Error_Pragma ("pragma% only apply to rci unit");

               --  Set flag for entity of the library unit

               else
                  Set_Has_All_Calls_Remote (Lib_Entity);
               end if;

            end if;
         end All_Calls_Remote;

         --------------
         -- Annotate --
         --------------

         --  pragma Annotate (IDENTIFIER {, ARG);
         --  ARG ::= NAME | EXPRESSION

         when Pragma_Annotate => Annotate : begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_Arg_Is_Identifier (Arg1);

            declare
               Arg : Node_Id := Arg2;
               Exp : Node_Id;

            begin
               while Present (Arg) loop
                  Exp := Expression (Arg);
                  Analyze (Exp);

                  if Is_Entity_Name (Exp) then
                     null;

                  elsif Nkind (Exp) = N_String_Literal then
                     Resolve (Exp, Standard_String);

                  elsif Is_Overloaded (Exp) then
                     Error_Pragma_Arg ("ambiguous argument for pragma%", Exp);

                  else
                     Resolve (Exp, Etype (Exp));
                  end if;

                  Arg := Next (Arg);
               end loop;
            end;
         end Annotate;

         ------------
         -- Assert --
         ------------

         --  pragma Assert (Boolean_EXPRESSION);

         when Pragma_Assert =>
            GNAT_Pragma;
            Check_No_Identifiers;

            if Arg_Count > 1 then
               Check_Arg_Count (2);
               Check_Arg_Is_Static_Expression (Arg2, Standard_String);
            end if;

            if Assertions_Enabled
              or else Operating_Mode /= Generate_Code
            then
               Analyze_And_Resolve (Expression (Arg1), Standard_Boolean);
            end if;

         ---------------
         -- AST_Entry --
         ---------------

         --  pragma AST_Entry (entry_IDENTIFIER);

         when Pragma_AST_Entry => AST_Entry : declare
            Ent : Node_Id;

         begin
            GNAT_Pragma;
            Check_VMS (N);
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Local_Name (Arg1);
            Ent := Entity (Expression (Arg1));

            --  Note: the implementation of the AST_Entry pragma could handle
            --  the entry family case fine, but for now we are consistent with
            --  the DEC rules, and do not allow the pragma, which of course
            --  has the effect of also forbidding the attribute.

            if Ekind (Ent) /= E_Entry then
               Error_Pragma_Arg
                 ("pragma% argument must be simple entry name", Arg1);

            elsif Is_AST_Entry (Ent) then
               Error_Pragma_Arg
                 ("duplicate % pragma for entry", Arg1);

            elsif Has_Homonym (Ent) then
               Error_Pragma_Arg
                 ("pragma% argument cannot specify overloaded entry", Arg1);

            else
               declare
                  FF : constant Entity_Id := First_Formal (Ent);

               begin
                  if Present (FF) then
                     if Present (Next_Formal (FF)) then
                        Error_Pragma_Arg
                          ("entry for pragma% can have only one argument",
                           Arg1);

                     elsif Parameter_Mode (FF) /= E_In_Parameter then
                        Error_Pragma_Arg
                          ("entry parameter for pragma% must have mode IN",
                           Arg1);
                     end if;
                  end if;
               end;

               Set_Is_AST_Entry (Ent);
            end if;
         end AST_Entry;

         ------------------
         -- Asynchronous --
         ------------------

         --  pragma Asynchronous (LOCAL_NAME);

         when Pragma_Asynchronous => Asynchronous : declare
            F      : Boolean;
            Nm     : Entity_Id;
            C_Ent  : Entity_Id;
            L      : List_Id;
            S      : Node_Id;
            N      : Node_Id;
            Formal : Entity_Id;

         begin
            Check_Ada_83_Warning;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);

            if Debug_Flag_U then
               return;
            end if;

            C_Ent := Cunit_Entity (Current_Sem_Unit);
            Analyze (Expression (Arg1));
            Nm := Entity (Expression (Arg1));

            if not Is_Remote_Call_Interface (C_Ent)
              and then not Is_Remote_Types (C_Ent)
            then
               --  This pragma should only appear in an RCI or Remote Types
               --  unit (RM E.4.1(4))

               Error_Pragma ("pragma% not in rci or remote types unit");

            elsif not Is_Remote_Call_Interface (Nm)
              and then not Is_Remote_Types (C_Ent)
            then
               --  The argument should be declared in RCI or Remote Types
               --  unit (RM E.4.1(4))

               Error_Pragma_Arg
                 ("pragma% argument not in 'R'C'I/Remote_Types unit", Arg1);
            end if;

            if Ekind (Nm) = E_Procedure
              and then Nkind (Parent (Nm)) = N_Procedure_Specification
            then
               L := Parameter_Specifications (Parent (Nm));

               if not Present (L) then
                  Set_Is_Asynchronous (Nm);
                  return;
               end if;

               --  The formals should be of mode IN (RM E.4.1(6))

               S := First (L);
               while Present (S) loop
                  Formal := Defining_Identifier (S);

                  if Nkind (Formal) = N_Defining_Identifier
                    and then Ekind (Formal) /= E_In_Parameter
                  then
                     Error_Pragma_Arg
                       ("pragma% remote procedure with mode IN only"
                       , Arg1);
                  end if;

                  S := Next (S);
               end loop;

               Set_Is_Asynchronous (Nm);
               return;

            elsif Ekind (Nm) = E_Access_Subprogram_Type then
               N := Declaration_Node (Nm);

               if Nkind (N) = N_Full_Type_Declaration
                 and then Nkind (Type_Definition (N)) =
                                     N_Access_Procedure_Definition
               then
                  L := Parameter_Specifications (Type_Definition (N));

                  if not Present (L) then
                     Set_Is_Asynchronous (Nm);
                     return;
                  end if;

                  --  The formals should be of mode IN (RM E.4.1(7))

                  S := First (L);
                  while Present (S) loop
                     Formal := Defining_Identifier (S);

                     if Nkind (Formal) = N_Defining_Identifier
                       and then Ekind (Formal) /= E_In_Parameter
                     then
                        Error_Pragma_Arg
                          ("pragma% remote procedure with mode IN only",
                            Arg1);
                     end if;

                     S := Next (S);
                  end loop;

                  Set_Is_Asynchronous (Nm);

               else
                  Error_Pragma_Arg
                    ("pragma% remote access-to-procedure type only",
                    Arg1);
               end if;

            else
               --  Access-to-class-wide type

               Set_Is_Asynchronous (Nm);
            end if;

         end Asynchronous;

         ------------
         -- Atomic --
         ------------

         --  pragma Atomic (LOCAL_NAME);

         when Pragma_Atomic =>
            Process_Atomic_Shared_Volatile;

         -----------------------
         -- Atomic_Components --
         -----------------------

         --  pragma Atomic_Components (array_LOCAL_NAME);

         --  This processing is shared by Volatile_Components

         when Pragma_Atomic_Components   |
              Pragma_Volatile_Components =>

         Atomic_Components : declare
            E_Id : Node_Id;
            E    : Entity_Id;
            D    : Node_Id;
            K    : Node_Kind;

         begin
            GNAT_Pragma;
            Check_Ada_83_Warning;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Expression (Arg1);

            if Etype (E_Id) = Any_Type then
               return;
            end if;

            E := Entity (E_Id);

            if Rep_Item_Too_Early (E, N)
                 or else
               Rep_Item_Too_Late (E, N)
            then
               return;
            end if;

            D := Declaration_Node (E);
            K := Nkind (D);

            if (K = N_Full_Type_Declaration and then Is_Array_Type (E))
              or else
                ((Ekind (E) = E_Constant or else Ekind (E) = E_Variable)
                   and then Nkind (D) = N_Object_Declaration
                   and then Nkind (Object_Definition (D)) =
                                       N_Constrained_Array_Definition)
            then
               --  For consistency, always set these flags on the underlying
               --  base type if E is an object. The test above verifies that
               --  it is safe to do this.

               if Nkind (D) = N_Object_Declaration then
                  E := Base_Type (Etype (E));
               end if;

               if Prag_Id = Pragma_Atomic_Components then
                  Set_Has_Atomic_Components (Base_Type (E));
               end if;

               Set_Has_Volatile_Components (Base_Type (E));

            else
               Error_Pragma_Arg ("inappropriate entity for pragma%", Arg1);
            end if;
         end Atomic_Components;

         --------------------
         -- Attach_Handler --
         --------------------

         --  pragma Attach_Handler (handler_NAME, EXPRESSION);

         when Pragma_Attach_Handler =>
            Check_Ada_83_Warning;
            Check_No_Identifiers;
            Check_Arg_Count (2);
            Check_Interrupt_Handler;
            Analyze_And_Resolve (Expression (Arg2), RTE (RE_Interrupt_Id));

         --------------------
         -- C_Pass_By_Copy --
         --------------------

         --  pragma C_Pass_By_Copy ([Max_Size =>] static_integer_EXPRESSION);

         when Pragma_C_Pass_By_Copy => C_Pass_By_Copy : declare
            Arg : Node_Id;
            Val : Uint;

         begin
            GNAT_Pragma;
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Max_Size);

            Arg := Expression (Arg1);
            Check_Arg_Is_Static_Expression (Arg, Any_Integer);

            Val := Expr_Value (Arg);

            if Val <= 0 then
               Error_Pragma_Arg
                 ("maximum size for pragma% must be positive", Arg1);

            elsif UI_Is_In_Int_Range (Val) then
               Default_C_Record_Mechanism := UI_To_Int (Val);

            --  If a giant value is given, Int'Last will do well enough.
            --  If sometime someone complains that a record larger than
            --  two gigabytes is not copied, we will worry about it then!

            else
               Default_C_Record_Mechanism := Mechanism_Type'Last;
            end if;
         end C_Pass_By_Copy;

         -------------------
         -- Common_Object --
         -------------------

         --  pragma Common_Object (
         --        [Internal =>] LOCAL_NAME,
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         when Pragma_Common_Object => Common_Object : declare
            Args  : Args_List (1 .. 3);
            Names : Name_List (1 .. 3) := (
                      Name_Internal,
                      Name_External,
                      Name_Size);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Size     : Node_Id renames Args (3);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Object_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Size     => Size);
         end Common_Object;

         ----------------------------
         -- Complex_Representation --
         ----------------------------

         --  pragma Complex_Representation ([Entity =>] LOCAL_NAME);

         when Pragma_Complex_Representation => Complex_Representation : declare
            E_Id : Entity_Id;
            E    : Entity_Id;
            Ent  : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Max_Size);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Expression (Arg1);

            if Etype (E_Id) = Any_Type then
               return;
            end if;

            E := Entity (E_Id);

            if not Is_Record_Type (E) then
               Error_Pragma_Arg
                 ("argument for pragma% must be record type", Arg1);
            end if;

            Ent := First_Entity (E);

            if No (Ent)
              or else No (Next_Entity (Ent))
              or else Present (Next_Entity (Next_Entity (Ent)))
              or else not Is_Floating_Point_Type (Etype (Ent))
              or else Etype (Ent) /= Etype (Next_Entity (Ent))
            then
               Error_Pragma_Arg
                 ("record for pragma% must have two fields of same fpt type",
                  Arg1);

            else
               Set_Has_Complex_Representation (Base_Type (E));
            end if;
         end Complex_Representation;

         -------------------------
         -- Component_Alignment --
         -------------------------

         --  pragma Component_Alignment (
         --        [Form =>] ALIGNMENT_CHOICE
         --     [, [Name =>] type_LOCAL_NAME]);
         --
         --   ALIGNMENT_CHOICE ::=
         --     Component_Size
         --   | Component_Size_4
         --   | Storage_Unit
         --   | Default

         when Pragma_Component_Alignment => Component_AlignmentP : declare
            Args  : Args_List (1 .. 2);
            Names : Name_List (1 .. 2) := (
                      Name_Form,
                      Name_Name);

            Form : Node_Id renames Args (1);
            Name : Node_Id renames Args (2);

            Atype   : Component_Alignment_Kind;
            Type_Id : Node_Id;
            Typ     : Entity_Id;

         begin
            Gather_Associations (Names, Args);

            if No (Form) then
               Error_Pragma ("missing Form argument for pragma%");
            end if;

            Check_Arg_Is_Identifier (Form);

            --  Get proper alignment, note that Default = Component_Size
            --  on all machines we have so far, and we want to set this
            --  value rather than the default value to indicate that it
            --  has been explicitly set (and thus will not get overridden
            --  by the default component alignment for the current scope)

            if Chars (Form) = Name_Component_Size then
               Atype := Calign_Component_Size;

            elsif Chars (Form) = Name_Component_Size_4 then
               Atype := Calign_Component_Size_4;

            elsif Chars (Form) = Name_Default then
               Atype := Calign_Component_Size;

            elsif Chars (Form) = Name_Storage_Unit then
               Atype := Calign_Storage_Unit;

            else
               Error_Pragma_Arg
                 ("invalid Form parameter for pragma%", Form);
            end if;

            --  Case with no name, supplied, affects scope table entry

            if No (Name) then
               Scope_Stack.Table
                 (Scope_Stack.Last).Component_Alignment_Default := Atype;

            --  Case of name supplied

            else
               Check_Arg_Is_Local_Name (Name);
               Find_Type (Name);
               Typ := Entity (Name);

               if Typ = Any_Type
                 or else Rep_Item_Too_Early (Typ, N)
               then
                  return;
               else
                  Typ := Underlying_Type (Typ);
               end if;

               if not Is_Record_Type (Typ)
                 and then not Is_Array_Type (Typ)
               then
                  Error_Pragma_Arg
                    ("Name parameter of pragma% must identify record or " &
                     "array type", Name);
               end if;

               --  An explicit Component_Alignment pragma overrides an
               --  implicit pragma Pack, but not an explicit one.

               if not Has_Pragma_Pack (Base_Type (Typ)) then
                  Set_Is_Packed (Base_Type (Typ), False);
                  Set_Component_Alignment (Base_Type (Typ), Atype);
               end if;
            end if;

         end Component_AlignmentP;

         ----------------
         -- Controlled --
         ----------------

         --  pragma Controlled (first_subtype_LOCAL_NAME);

         when Pragma_Controlled => Controlled : declare
            Arg : Node_Id;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);
            Arg := Expression (Arg1);

            if not Is_Entity_Name (Arg)
              or else not Is_Access_Type (Entity (Arg))
            then
               Error_Pragma_Arg ("pragma% requires access type", Arg1);
            else
               Set_Has_Pragma_Controlled (Base_Type (Entity (Arg)));
            end if;
         end Controlled;

         ----------------
         -- Convention --
         ----------------

         --  pragma Convention ([Convention =>] convention_IDENTIFIER,
         --    [Entity =>] LOCAL_NAME);

         when Pragma_Convention => Convention : declare
            C : Convention_Id;
            E : Entity_Id;

         begin
            Note_Feature (New_Representation_Pragmas, Loc);
            Check_Ada_83_Warning;
            Check_Arg_Count (2);
            Process_Convention (C, E);
         end Convention;

         ---------------
         -- CPP_Class --
         ---------------

         --  pragma CPP_Class ([Entity =>] LOCAL_NAME)

         when Pragma_CPP_Class => CPP_Class : declare
            Arg         : Node_Id;
            Typ         : Entity_Id;
            Default_DTC : Entity_Id := Empty;
            VTP_Type    : constant Entity_Id  := RTE (RE_Vtable_Ptr);
            C           : Entity_Id;
            Tag_C       : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            Arg := Expression (Arg1);
            Analyze (Arg);

            if Etype (Arg) = Any_Type then
               return;
            end if;

            if not Is_Entity_Name (Arg)
              or else not Is_Type (Entity (Arg))
            then
               Error_Pragma_Arg ("pragma% requires a type mark", Arg1);
            end if;

            Typ := Entity (Arg);

            if not Is_Record_Type (Typ) then
               Error_Pragma_Arg ("pragma% applicable to a record, "
                 & "tagged record or record extension", Arg1);
            end if;

            Default_DTC := First_Component (Typ);
            while Present (Default_DTC)
              and then Etype (Default_DTC) /= VTP_Type
            loop
               Default_DTC := Next_Component (Default_DTC);
            end loop;

            if not Is_Tagged_Type (Typ) and then Present (Default_DTC) then
               Error_Pragma_Arg
                 ("only tagged records can contain vtable pointers", Arg1);

            elsif Is_Tagged_Type (Typ)
              and then Typ = Root_Type (Typ)
              and then No (Default_DTC)
            then
               Error_Pragma_Arg
                 ("a cpp_class must contain a vtable pointer", Arg1);
            else
               Set_Is_CPP_Class (Typ);
               Set_Is_Limited_Record (Typ);
               Set_Is_Tag (Default_DTC);
               Set_DT_Entry_Count (Default_DTC, No_Uint);

               --  Since a CPP type has no direct link to its associated tag
               --  most tags checks cannot be performed

               Set_Suppress_Tag_Checks (Typ);
               Set_Suppress_Tag_Checks (Class_Wide_Type (Typ));

               if Typ = Root_Type (Typ) then

                  --  Get rid of the _tag component which is only useful for
                  --  regular tagged types

                  Tag_C := Tag_Component (Typ);
                  C := First_Entity (Typ);

                  if C = Tag_C then
                     Set_First_Entity (Typ, Next_Entity (Tag_C));

                  else
                     while Next_Entity (C) /= Tag_C loop
                        C := Next_Entity (C);
                     end loop;

                     Set_Next_Entity (C, Next_Entity (Tag_C));
                  end if;
               end if;
            end if;
         end CPP_Class;

         ---------------------
         -- CPP_Constructor --
         ---------------------

         --  pragma CPP_Constructor ([Entity =>] LOCAL_NAME);

         when Pragma_CPP_Constructor => CPP_Constructor : declare
            Id     : Entity_Id;
            Def_Id : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            Id := Expression (Arg1);
            Find_Program_Unit_Name (Id);

            --  If we did not find the name, we are done

            if Etype (Id) = Any_Type then
               return;
            end if;

            Def_Id := Entity (Id);

            if Ekind (Def_Id) = E_Function
              and then Is_Class_Wide_Type (Etype (Def_Id))
              and then Is_CPP_Class (Etype (Etype (Def_Id)))
            then
               --  What the heck is this??? this pragma allows only 1 arg

               if Arg_Count >= 2 then
                  Check_At_Most_N_Arguments (3);
                  Process_Interface_Name (Def_Id, Arg2, Arg3);
               end if;

               if No (Parameter_Specifications (Parent (Def_Id))) then
                  Set_Has_Completion (Def_Id);
                  Set_Is_Constructor (Def_Id);
               else
                  Unimplemented (Arg1, "non-default constructors");
               end if;

            else
               Error_Pragma_Arg
                 ("pragma% requires function returning a 'C'P'P_Class type",
                   Arg1);
            end if;
         end CPP_Constructor;

         --------------------
         -- CPP_Destructor --
         --------------------

         --  pragma CPP_Destructor ([Entity =>] LOCAL_NAME);

         when Pragma_CPP_Destructor =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);
            Pragma_Not_Implemented;

         -----------------
         -- CPP_Virtual --
         -----------------

         --  pragma CPP_Virtual
         --      [Entity =>]       LOCAL_NAME
         --    [ [Vtable_Ptr =>]   LOCAL_NAME,
         --      [Position =>]     static_integer_EXPRESSION]);

         when Pragma_CPP_Virtual => CPP_Virtual : declare
            Arg      : Node_Id;
            Typ      : Entity_Id;
            Subp     : Entity_Id;
            VTP_Type : constant Entity_Id  := RTE (RE_Vtable_Ptr);
            DTC      : Entity_Id;
            V        : Uint;

         begin
            GNAT_Pragma;

            if Arg_Count = 3 then
               Check_Optional_Identifier (Arg2, Name_Vtable_Ptr);
               Check_Optional_Identifier (Arg3, Name_Entry_Count);

            else
               Check_Arg_Count (1);
            end if;

            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);

            --  First argument must be a subprogram name

            Arg := Expression (Arg1);
            Find_Program_Unit_Name (Arg);

            if Etype (Arg) = Any_Type then
               return;
            else
               Subp := Entity (Arg);
            end if;

            if not (Is_Subprogram (Subp)
                     and then Is_Dispatching_Operation (Subp))
            then
               Error_Pragma_Arg
                 ("pragma% must reference a primitive operation", Arg1);
            end if;

            Typ := Find_Dispatching_Type (Subp);

            --  If only one Argument defaults are :
            --    . DTC_Entity is the default Vtable pointer
            --    . DT_Position will be set at the freezing point

            if Arg_Count = 1 then
               Set_DTC_Entity (Subp, Tag_Component (Typ));
               return;
            end if;

            --  Second argument is a component name of type Vtable_Ptr

            Arg := Expression (Arg2);

            if Nkind (Arg) /= N_Identifier then
               Error_Msg_NE ("must be a& component name", Arg, Typ);
               raise Pragma_Exit;
            end if;

            DTC := First_Component (Typ);
            while Present (DTC) and then Chars (DTC) /= Chars (Arg) loop
               DTC := Next_Component (DTC);
            end loop;

            if No (DTC) then
               Error_Msg_NE ("must be a& component name", Arg, Typ);
               raise Pragma_Exit;

            elsif Etype (DTC) /= VTP_Type then
               Wrong_Type (Arg, VTP_Type);
               return;
            end if;

            --  Third argument is an integer (DT_Position)

            Arg := Expression (Arg3);
            Analyze_And_Resolve (Arg, Any_Integer);

            if not Is_Static_Expression (Arg) then
               Error_Pragma_Arg
                 ("third argument of pragma% must be a static expression",
                  Arg3);

            else
               V := Expr_Value (Expression (Arg3));

               if V <= 0 then
                  Error_Pragma_Arg
                    ("third argument of pragma% must be positive",
                     Arg3);

               else
                  Set_DTC_Entity (Subp, DTC);
                  Set_DT_Position (Subp, V);
               end if;
            end if;
         end CPP_Virtual;

         ----------------
         -- CPP_Vtable --
         ----------------

         --  pragma CPP_Vtable (
         --    [Entity =>]       LOCAL_NAME
         --    [Vtable_Ptr =>]   LOCAL_NAME,
         --    [Entry_Count =>]  static_integer_EXPRESSION);

         when Pragma_CPP_Vtable => CPP_Vtable : declare
            Arg           : Node_Id;
            Typ           : Entity_Id;
            Already_a_Tag : Boolean := False;
            Comp          : Entity_Id := Empty;
            VTP_Type      : constant Entity_Id  := RTE (RE_Vtable_Ptr);
            DTC           : Entity_Id;
            V             : Uint;
            Elmt          : Elmt_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (3);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, Name_Vtable_Ptr);
            Check_Optional_Identifier (Arg3, Name_Entry_Count);
            Check_Arg_Is_Local_Name (Arg1);

            --  First argument is a record type name

            Arg := Expression (Arg1);
            Analyze (Arg);

            if Etype (Arg) = Any_Type then
               return;
            else
               Typ := Entity (Arg);
            end if;

            if not (Is_Type (Typ) and then Is_CPP_Class (Typ)) then
               Error_Pragma_Arg ("'C'P'P_Class type expected", Arg1);
            end if;

            --  Second argument is a component name of type Vtable_Ptr

            Arg := Expression (Arg2);

            if Nkind (Arg) /= N_Identifier then
               Error_Msg_NE ("must be a& component name", Arg, Typ);
               raise Pragma_Exit;
            end if;

            DTC := First_Component (Typ);
            while Present (DTC) and then Chars (DTC) /= Chars (Arg) loop
               DTC := Next_Component (DTC);
            end loop;

            if No (DTC) then
               Error_Msg_NE ("must be a& component name", Arg, Typ);
               raise Pragma_Exit;

            elsif Etype (DTC) /= VTP_Type then
               Wrong_Type (DTC, VTP_Type);
               return;

            --  If it is the first pragma Vtable, This becomes the default tag

            elsif (not Is_Tag (DTC))
              and then DT_Entry_Count (Tag_Component (Typ)) = No_Uint
            then
               Set_Is_Tag (Tag_Component (Typ), False);
               Set_Is_Tag (DTC, True);
               Set_DT_Entry_Count (DTC, No_Uint);
            end if;

            --  Those pragmas must appear before any primitive operation
            --  definition (except inherited ones) otherwise the default
            --  may be wrong

            Elmt := First_Elmt (Primitive_Operations (Typ));
            while Present (Elmt) loop
               if No (Alias (Node (Elmt))) then
                  Error_Msg_Sloc := Sloc (Node (Elmt));
                  Error_Pragma
                    ("pragma% must appear before this primitive operation");
               end if;

               Elmt := Next_Elmt (Elmt);
            end loop;

            --  Third argument is an integer (DT_Entry_Count)

            Arg := Expression (Arg3);
            Analyze_And_Resolve (Arg, Any_Integer);

            if not Is_Static_Expression (Arg) then
               Error_Pragma_Arg
                 ("entry count for pragma% must be a static expression", Arg3);

            else
               V := Expr_Value (Expression (Arg3));

               if V <= 0 then
                  Error_Pragma_Arg
                    ("entry count for pragma% must be positive", Arg3);
               else
                  Set_DT_Entry_Count (DTC, V);
               end if;
            end if;

         end CPP_Vtable;

         -----------
         -- Debug --
         -----------

         --  pragma Debug (PROCEDURE_CALL_STATEMENT);

         when Pragma_Debug => Debug : begin
            GNAT_Pragma;

            --  If assertions are enabled, or if we are doing semantic checking
            --  only, then rewrite the pragma with its corresponding procedure
            --  call and then analyze the call.

            if Assertions_Enabled
              or else Operating_Mode /= Generate_Code
            then
               Rewrite_Substitute_Tree
                 (N, Relocate_Node (Debug_Statement (N)));
               Analyze (N);

            --  Otherwise simply rewrite with a null statement and do not even
            --  analyze the argument of the pragma.

            else
               Rewrite_Substitute_Tree (N, Make_Null_Statement (Loc));
            end if;
         end Debug;

         -------------------
         -- Discard_Names --
         -------------------

         --  pragma Discard_Names [([On =>] LOCAL_NAME)];

         when Pragma_Discard_Names => Discard_Names : declare
            E_Id : Entity_Id;
            E    : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Ada_83_Warning;

            --  Deal with configuration pragma case
            --  For now, ignored ???

            if Arg_Count = 0 and then Is_Configuration_Pragma then
               return;

            --  Otherwise, check correct appropriate context

            else
               Check_Is_In_Decl_Part_Or_Package_Spec;

               --  For now, ignore the case of no parameter present ???

               if Arg_Count = 0 then
                  return;

               else
                  Check_Arg_Count (1);
                  Check_Optional_Identifier (Arg1, Name_On);
                  Check_Arg_Is_Local_Name (Arg1);
                  E_Id := Expression (Arg1);

                  if Etype (E_Id) = Any_Type then
                     return;
                  else
                     E := Entity (E_Id);
                  end if;

                  if (Is_First_Subtype (E)
                       and then (Is_Enumeration_Type (E)
                                  or else Is_Tagged_Type (E)))
                    or else Ekind (E) = E_Exception
                  then
                     Set_Discard_Names (E);
                  else
                     Error_Pragma_Arg
                       ("inappropriate entity for pragma%", Arg1);
                  end if;
               end if;
            end if;
         end Discard_Names;

         ---------------
         -- Elaborate --
         ---------------

         --  pragma Elaborate (library_unit_NAME {, library_unit_NAME});

         when Pragma_Elaborate => Elaborate : declare
            Plist       : List_Id;
            Parent_Node : Node_Id;
            Arg         : Node_Id;
            Citem       : Node_Id;

         begin
            --  Pragma must be in context items list of a compilation unit

            if not Is_List_Member (N) then
               Pragma_Misplaced;
               return;

            else
               Plist := List_Containing (N);
               Parent_Node := Parent (Plist);

               if Parent_Node = Empty
                 or else Nkind (Parent_Node) /= N_Compilation_Unit
                 or else Context_Items (Parent_Node) /= Plist
               then
                  Pragma_Misplaced;
                  return;
               end if;
            end if;

            --  In Ada 83 mode, there can be no items following it in the
            --  context list except other pragmas and implicit with clauses
            --  (e.g. those added by use of Rtsfind). In Ada 95 mode, this
            --  placement rule does not apply.

            if Ada_83 and then Comes_From_Source (N) then
               Citem := Next (N);

               while Present (Citem) loop
                  if Nkind (Citem) = N_Pragma
                    or else (Nkind (Citem) = N_With_Clause
                              and then Implicit_With (Citem))
                  then
                     null;
                  else
                     Error_Pragma
                       ("(Ada 83) pragma% must be at end of context clause");
                  end if;

                  Citem := Next (Citem);
               end loop;
            end if;

            --  Finally, the arguments must all be units mentioned in a with
            --  clause in the same context clause. Note we already checked
            --  (in Par.Prag) that the arguments are either identifiers or

            Arg := Arg1;
            Outer : while Present (Arg) loop
               Citem := First (Plist);

               Inner : while Citem /= N loop
                  if Nkind (Citem) = N_With_Clause
                    and then Same_Name (Name (Citem), Expression (Arg))
                  then
                     Set_Elaborate_Present (Citem, True);
                     exit Inner;
                  end if;

                  Citem := Next (Citem);
               end loop Inner;

               if Citem = N then
                  Error_Pragma_Arg
                    ("argument of pragma% is not with'ed unit", Arg);
               end if;

               Arg := Next (Arg);
            end loop Outer;
         end Elaborate;

         -------------------
         -- Elaborate_All --
         -------------------

         --  pragma Elaborate_All (library_unit_NAME {, library_unit_NAME});

         when Pragma_Elaborate_All => Elaborate_All : declare
            Plist       : List_Id;
            Parent_Node : Node_Id;
            Arg         : Node_Id;
            Citem       : Node_Id;

         begin
            Check_Ada_83_Warning;

            --  Pragma must be in context items list of a compilation unit

            if not Is_List_Member (N) then
               Pragma_Misplaced;
               return;

            else
               Plist := List_Containing (N);
               Parent_Node := Parent (Plist);

               if Parent_Node = Empty
                 or else Nkind (Parent_Node) /= N_Compilation_Unit
                 or else Context_Items (Parent_Node) /= Plist
               then
                  Pragma_Misplaced;
                  return;
               end if;
            end if;

            --  Note: unlike pragma Elaborate, pragma Elaborate_All does not
            --  have to appear at the end of the context clause, but may
            --  appear mixed in with other items.

            --  Final check: the arguments must all be units mentioned in
            --  a with clause in the same context clause. Note that we
            --  already checked (in Par.Prag) that all the arguments are
            --  either identifiers or selected components.

            Arg := Arg1;
            Outr : while Present (Arg) loop
               Citem := First (Plist);

               Innr : while Citem /= N loop
                  if Nkind (Citem) = N_With_Clause
                    and then Same_Name (Name (Citem), Expression (Arg))
                  then
                     Set_Elaborate_All_Present (Citem, True);
                     exit Innr;
                  end if;

                  Citem := Next (Citem);
               end loop Innr;

               if Citem = N then
                  Error_Pragma_Arg
                    ("argument of pragma% is not with'ed unit", Arg);
               end if;

               Arg := Next (Arg);
            end loop Outr;
         end Elaborate_All;

         --------------------
         -- Elaborate_Body --
         --------------------

         --  pragma Elaborate_Body [( library_unit_NAME )];

         when Pragma_Elaborate_Body => Elaborate_Body : declare
            Plist      : List_Id;
            Cunit_Node : Node_Id;

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;
            Cunit_Node := Cunit (Current_Sem_Unit);
            Set_Elaborate_Body_Present (Cunit_Node, True);
            Set_Body_Required (Cunit_Node, True);
         end Elaborate_Body;

         ------------
         -- Export --
         ------------

         --  pragma Export (
         --    [   Convention    =>] convention_IDENTIFIER,
         --    [   Entity        =>] local_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Export => Export : declare
            C      : Convention_Id;
            Def_Id : Entity_Id;

         begin
            Check_Ada_83_Warning;
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (4);
            Note_Feature (New_Representation_Pragmas, Loc);
            Process_Convention (C, Def_Id);
            Note_Possible_Modification (Expression (Arg2));
            Process_Interface_Name (Def_Id, Arg3, Arg4);

            if not Is_Public (Def_Id) then
               Error_Pragma_Arg ("internal entity cannot be exported", Arg2);
            end if;

            Set_Is_Exported (Def_Id);
         end Export;

         ----------------------
         -- Export_Exception --
         ----------------------

         --  pragma Export_Exception (
         --        [Internal         =>] LOCAL_NAME,
         --     [, [External         =>] EXTERNAL_SYMBOL,]
         --     [, [Form     =>] Ada | VMS]
         --     [, [Code     =>] static_integer_EXPRESSION]);

         when Pragma_Export_Exception => Export_Exception : declare
            Args  : Args_List (1 .. 4);
            Names : Name_List (1 .. 4) := (
                      Name_Internal,
                      Name_External,
                      Name_Form,
                      Name_Code);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Form     : Node_Id renames Args (3);
            Code     : Node_Id renames Args (4);

         begin
            if Inside_A_Generic then
               Error_Pragma ("pragma% cannot be used for generic entities");
            end if;

            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Exception_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Form     => Form,
              Arg_Code     => Code);

         end Export_Exception;

         ---------------------
         -- Export_Function --
         ---------------------

         --  pragma Export_Function (
         --        [Internal         =>] LOCAL_NAME,
         --     [, [External         =>] EXTERNAL_SYMBOL,]
         --     [, [Parameter_Types  =>] (PARAMETER_TYPES)]
         --     [, [Result_Type      =>] SUBTYPE_MARK]
         --     [, [Mechanism        =>] MECHANISM]
         --     [, [Result_Mechanism =>] MECHANISM_NAME]);

         when Pragma_Export_Function => Export_Function : declare
            Args  : Args_List (1 .. 6);
            Names : Name_List (1 .. 6) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Result_Type,
                      Name_Mechanism,
                      Name_Result_Mechanism);

            Internal         : Node_Id renames Args (1);
            External         : Node_Id renames Args (2);
            Parameter_Types  : Node_Id renames Args (3);
            Result_Type      : Node_Id renames Args (4);
            Mechanism        : Node_Id renames Args (5);
            Result_Mechanism : Node_Id renames Args (6);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal         => Internal,
              Arg_External         => External,
              Arg_Parameter_Types  => Parameter_Types,
              Arg_Result_Type      => Result_Type,
              Arg_Mechanism        => Mechanism,
              Arg_Result_Mechanism => Result_Mechanism);
         end Export_Function;

         -------------------
         -- Export_Object --
         -------------------

         --  pragma Export_Object (
         --        [Internal =>] LOCAL_NAME,
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         when Pragma_Export_Object => Export_Object : declare
            Args  : Args_List (1 .. 3);
            Names : Name_List (1 .. 3) := (
                      Name_Internal,
                      Name_External,
                      Name_Size);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Size     : Node_Id renames Args (3);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Object_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Size     => Size);
         end Export_Object;

         ----------------------
         -- Export_Procedure --
         ----------------------

         --  pragma Export_Procedure (
         --        [Internal         =>] LOCAL_NAME,
         --     [, [External         =>] EXTERNAL_SYMBOL,]
         --     [, [Parameter_Types  =>] (PARAMETER_TYPES)]
         --     [, [Mechanism        =>] MECHANISM]);

         when Pragma_Export_Procedure => Export_Procedure : declare
            Args  : Args_List (1 .. 4);
            Names : Name_List (1 .. 4) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism);

            Internal        : Node_Id renames Args (1);
            External        : Node_Id renames Args (2);
            Parameter_Types : Node_Id renames Args (3);
            Mechanism       : Node_Id renames Args (4);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal        => Internal,
              Arg_External        => External,
              Arg_Parameter_Types => Parameter_Types,
              Arg_Mechanism       => Mechanism);
         end Export_Procedure;

         -------------------
         -- Extend_System --
         -------------------

         --  pragma Extend_System ([Name =>] Identifier);

         when Pragma_Extend_System => Extend_System : declare
         begin
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Name);
            Check_Arg_Is_Identifier (Arg1);

            Get_Name_String (Chars (Expression (Arg1)));

            if Name_Len > 4
              and then Name_Buffer (1 .. 4) = "aux_"
            then
               if Present (System_Extend_Pragma_Arg) then
                  if Chars (Expression (Arg1)) =
                     Chars (Expression (System_Extend_Pragma_Arg))
                  then
                     null;
                  else
                     Error_Msg_Sloc := Sloc (System_Extend_Pragma_Arg);
                     Error_Pragma ("pragma% conflicts with that at#");
                  end if;

               else
                  System_Extend_Pragma_Arg := Arg1;
               end if;
            else
               Error_Pragma ("incorrect name for pragma%, must be Aux_xxx");
            end if;
         end Extend_System;

         -----------------------------
         -- Export_Valued_Procedure --
         -----------------------------

         --  pragma Export_Valued_Procedure (
         --        [Internal         =>] LOCAL_NAME,
         --     [, [External         =>] EXTERNAL_SYMBOL,]
         --     [, [Parameter_Types  =>] (PARAMETER_TYPES)]
         --     [, [Mechanism        =>] MECHANISM]);

         when Pragma_Export_Valued_Procedure =>
         Export_Valued_Procedure : declare
            Args  : Args_List (1 .. 4);
            Names : Name_List (1 .. 4) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism);

            Internal        : Node_Id renames Args (1);
            External        : Node_Id renames Args (2);
            Parameter_Types : Node_Id renames Args (3);
            Mechanism       : Node_Id renames Args (4);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal        => Internal,
              Arg_External        => External,
              Arg_Parameter_Types => Parameter_Types,
              Arg_Mechanism       => Mechanism);
         end Export_Valued_Procedure;

         --------------------------
         -- Float_Representation --
         --------------------------

         --  pragma Float_Representation (VAX_Float | IEEE_Float);

         when Pragma_Float_Representation =>
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifier (Arg1);
            Check_Arg_Is_One_Of (Arg1, Name_VAX_Float, Name_IEEE_Float);

            if Chars (Expression (Arg1)) = Name_VAX_Float then
               Check_VMS (Arg1);
               Opt.Float_Format := VAX;

            else
               Opt.Float_Format := IEEE;
            end if;

         -----------
         -- Ident --
         -----------

         --  pragma Ident (STRING_LITERAL)

         when Pragma_Ident => Ident : declare
            Str : String_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_String_Literal (Arg1);
            Check_Is_In_Decl_Part_Or_Package_Spec;
            Str := Strval (Expression (Arg1));

            if (String_Length (Str) > 31) then
               Error_Pragma_Arg
                 ("argument for pragma% is too long, maximum is 31", Arg1);
            end if;

            declare
               GP : Node_Id := Parent (Parent (N));

            begin
               if Nkind (GP) = N_Package_Declaration then
                  GP := Parent (GP);
               end if;

               --  If we have a compilation unit, then record the ident
               --  value, checking for improper duplication.

               if Nkind (GP) = N_Compilation_Unit then
                  if Ident_String (Current_Sem_Unit) /= No_String then
                     Error_Pragma ("duplicate% pragma not permitted");
                  else
                     Set_Ident_String (Current_Sem_Unit, Str);
                  end if;

               --  For subunits, we just ignore the Ident, since in GNAT
               --  these are not separate object files, and hence not
               --  separate units in the unit table.

               elsif Nkind (GP) = N_Subunit then
                  null;

               --  Otherwise we have a misplaced pragma Ident

               else
                  Error_Pragma ("pragma% only allowed at outer level");
               end if;
            end;
         end Ident;

         ------------
         -- Import --
         ------------

         --  pragma Import (
         --    [   Convention    =>] convention_IDENTIFIER,
         --    [   Entity        =>] local_NAME
         --    [, [External_Name =>] static_string_EXPRESSION ]
         --    [, [Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Import =>
            Check_Ada_83_Warning;
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (4);
            Process_Import_Or_Interface;

         ----------------------
         -- Import_Exception --
         ----------------------

         --  pragma Import_Exception (
         --        [Internal         =>] LOCAL_NAME,
         --     [, [External         =>] EXTERNAL_SYMBOL,]
         --     [, [Form     =>] Ada | VMS]
         --     [, [Code     =>] static_integer_EXPRESSION]);

         when Pragma_Import_Exception => Import_Exception : declare
            Args  : Args_List (1 .. 4);
            Names : Name_List (1 .. 4) := (
                      Name_Internal,
                      Name_External,
                      Name_Form,
                      Name_Code);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Form     : Node_Id renames Args (3);
            Code     : Node_Id renames Args (4);

         begin
            Gather_Associations (Names, Args);

            if Present (External) and then Present (Code) then
               Error_Pragma
                 ("cannot give both External and Code options for pragma%");
            end if;

            Process_Extended_Import_Export_Exception_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Form     => Form,
              Arg_Code     => Code);

         end Import_Exception;

         ---------------------
         -- Import_Function --
         ---------------------

         --  pragma Import_Function (
         --        [Internal                 =>] LOCAL_NAME,
         --     [, [External                 =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types          =>] (PARAMETER_TYPES)]
         --     [, [Result_Type              =>] SUBTYPE_MARK]
         --     [, [Mechanism                =>] MECHANISM]
         --     [, [Result_Mechanism         =>] MECHANISM_NAME]
         --     [, [First_Optional_Parameter =>] IDENTIFIER]);

         when Pragma_Import_Function => Import_Function : declare
            Args  : Args_List (1 .. 7);
            Names : Name_List (1 .. 7) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Result_Type,
                      Name_Mechanism,
                      Name_Result_Mechanism,
                      Name_First_Optional_Parameter);

            Internal                 : Node_Id renames Args (1);
            External                 : Node_Id renames Args (2);
            Parameter_Types          : Node_Id renames Args (3);
            Result_Type              : Node_Id renames Args (4);
            Mechanism                : Node_Id renames Args (5);
            Result_Mechanism         : Node_Id renames Args (6);
            First_Optional_Parameter : Node_Id renames Args (7);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal                 => Internal,
              Arg_External                 => External,
              Arg_Parameter_Types          => Parameter_Types,
              Arg_Result_Type              => Result_Type,
              Arg_Mechanism                => Mechanism,
              Arg_Result_Mechanism         => Result_Mechanism,
              Arg_First_Optional_Parameter => First_Optional_Parameter);
         end Import_Function;

         -------------------
         -- Import_Object --
         -------------------

         --  pragma Import_Object (
         --        [Internal =>] LOCAL_NAME,
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         when Pragma_Import_Object => Import_Object : declare
            Args  : Args_List (1 .. 3);
            Names : Name_List (1 .. 3) := (
                      Name_Internal,
                      Name_External,
                      Name_Size);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Size     : Node_Id renames Args (3);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Object_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Size     => Size);
         end Import_Object;

         ----------------------
         -- Import_Procedure --
         ----------------------

         --  pragma Import_Procedure (
         --        [Internal                 =>] LOCAL_NAME,
         --     [, [External                 =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types          =>] (PARAMETER_TYPES)]
         --     [, [Mechanism                =>] MECHANISM]
         --     [, [First_Optional_Parameter =>] IDENTIFIER]);

         when Pragma_Import_Procedure => Import_Procedure : declare
            Args  : Args_List (1 .. 5);
            Names : Name_List (1 .. 5) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism,
                      Name_First_Optional_Parameter);

            Internal                 : Node_Id renames Args (1);
            External                 : Node_Id renames Args (2);
            Parameter_Types          : Node_Id renames Args (3);
            Mechanism                : Node_Id renames Args (4);
            First_Optional_Parameter : Node_Id renames Args (5);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal                 => Internal,
              Arg_External                 => External,
              Arg_Parameter_Types          => Parameter_Types,
              Arg_Mechanism                => Mechanism,
              Arg_First_Optional_Parameter => First_Optional_Parameter);
         end Import_Procedure;

         -----------------------------
         -- Import_Valued_Procedure --
         -----------------------------

         --  pragma Import_Valued_Procedure (
         --        [Internal                 =>] LOCAL_NAME,
         --     [, [External                 =>] EXTERNAL_SYMBOL]
         --     [, [Parameter_Types          =>] (PARAMETER_TYPES)]
         --     [, [Mechanism                =>] MECHANISM]
         --     [, [First_Optional_Parameter =>] IDENTIFIER]);

         when Pragma_Import_Valued_Procedure =>
         Import_Valued_Procedure : declare
            Args  : Args_List (1 .. 5);
            Names : Name_List (1 .. 5) := (
                      Name_Internal,
                      Name_External,
                      Name_Parameter_Types,
                      Name_Mechanism,
                      Name_First_Optional_Parameter);

            Internal                 : Node_Id renames Args (1);
            External                 : Node_Id renames Args (2);
            Parameter_Types          : Node_Id renames Args (3);
            Mechanism                : Node_Id renames Args (4);
            First_Optional_Parameter : Node_Id renames Args (5);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Subprogram_Pragma (
              Arg_Internal                 => Internal,
              Arg_External                 => External,
              Arg_Parameter_Types          => Parameter_Types,
              Arg_Mechanism                => Mechanism,
              Arg_First_Optional_Parameter => First_Optional_Parameter);
         end Import_Valued_Procedure;

         ------------
         -- Inline --
         ------------

         --  pragma Inline ( NAME {, NAME} );

         when Pragma_Inline => Inline : declare
            Assoc    : Node_Id;
            Decl     : Node_Id;
            Subp_Id  : Node_Id;
            Subp     : Entity_Id;

            procedure Make_Inline (Subp : Entity_Id);
            --  Subp is the defining unit name of the subprogram
            --  declaration. Set the flag, as well as the flag in the
            --  corresponding boy, if there is one present.

            procedure Make_Inline (Subp : Entity_Id) is
               Kind : Entity_Kind := Ekind (Subp);

            begin
               if Etype (Subp) = Any_Type then
                  return;

               --  The referenced entity must either be the enclosing entity,
               --  or an entity declared within the current open scope.

               elsif Present (Scope (Subp))
                 and then Scope (Subp) /= Current_Scope
                 and then Subp /= Current_Scope
               then
                  Pragma_Misplaced;
                  return;
               end if;

               --  Processing for procedure, operator or function

               if Kind = E_Procedure
                 or else Kind = E_Function
                 or else Kind = E_Operator
               then
                  Set_Is_Inlined (Subp, True);

                  Decl := Parent (Parent (Subp));

                  if Nkind (Decl) = N_Subprogram_Declaration
                    and then Present (Corresponding_Body (Decl))
                  then
                     Set_Is_Inlined (Corresponding_Body (Decl), True);
                  end if;

               --  Don't do anything for a generic procedure or generic
               --  function. The instance will be marked inlined as
               --  required during the compilation of the instance.

               elsif Kind = E_Generic_Procedure
                 or else Kind = E_Generic_Function
               then
                  null;

               --  Literals are by definition inlined.

               elsif Kind = E_Enumeration_Literal then
                  null;

               --  Anything else is an error

               else
                  Error_Pragma_Arg
                    ("expect subprogram name for pragma%", Assoc);
               end if;
            end Make_Inline;

         begin
            Check_No_Identifiers;
            Check_At_Least_N_Arguments (1);
            Assoc := Arg1;

            while Present (Assoc) loop
               Subp_Id := Expression (Assoc);
               Analyze (Subp_Id);
               Subp := Entity (Subp_Id);

               if Subp = Any_Id then
                  null;
               else
                  Make_Inline (Subp);

                  while Present (Homonym (Subp))
                    and then Scope (Homonym (Subp)) = Current_Scope
                  loop
                     Make_Inline (Homonym (Subp));
                     Subp := Homonym (Subp);
                  end loop;
               end if;

               Assoc := Next (Assoc);
            end loop;
         end Inline;

         --------------------
         -- Inline_Generic --
         --------------------

         --  pragma Inline_Generic (NAME {, NAME});

         when Pragma_Inline_Generic =>
            Process_Generic_List;

         ----------------------
         -- Inspection_Point --
         ----------------------

         --  pragma Inspection_Point [(object_NAME {, object_NAME})];

         when Pragma_Inspection_Point => Inspection_Point : declare
            Arg : Node_Id;
            Exp : Node_Id;

         begin
            if Arg_Count > 0 then
               Arg := Arg1;
               loop
                  Exp := Expression (Arg);
                  Analyze (Exp);

                  if not Is_Entity_Name (Exp)
                    or else not Is_Object (Entity (Exp))
                  then
                     Error_Pragma_Arg ("object name required", Arg);
                  end if;

                  Arg := Next (Arg);
                  exit when No (Arg);
               end loop;
            end if;
         end Inspection_Point;

         ---------------
         -- Interface --
         ---------------

         --  pragma Interface (
         --    convention_IDENTIFIER,
         --    local_NAME );

         when Pragma_Interface =>
            GNAT_Pragma;
            Check_Arg_Count (2);
            Check_No_Identifiers;
            Process_Import_Or_Interface;

         --------------------
         -- Interface_Name --
         --------------------

         --  pragma Interface_Name (
         --    [  Entity        =>] local_NAME
         --    [,[External_Name =>] static_string_EXPRESSION ]
         --    [,[Link_Name     =>] static_string_EXPRESSION ]);

         when Pragma_Interface_Name => Interface_Name : declare
            Id     : Node_Id;
            Def_Id : Entity_Id;
            Hom_Id : Entity_Id;
            Found  : Boolean;

         begin
            GNAT_Pragma;
            Check_At_Least_N_Arguments (2);
            Check_At_Most_N_Arguments  (3);
            Id := Expression (Arg1);
            Analyze (Id);

            if not Is_Entity_Name (Id) then
               Error_Pragma_Arg
                 ("first argument for pragma% must be entity name", Arg1);
            elsif Etype (Id) = Any_Type then
               return;
            else
               Def_Id := Entity (Id);
            end if;

            --  Special DEC-compatible processing for the object case,
            --  forces object to be imported.

            if Ekind (Def_Id) = E_Variable then
               Kill_Size_Check_Code (Def_Id);
               Note_Possible_Modification (Id);

               --  Initialization is not allowed for imported variable
               --  The No_Location is used to mark the default
               --  initialization of access types

               --  Use of No_Location here is really ugly???

               if Present (Expression (Parent (Def_Id)))
                  and then
                    Sloc (Expression (Parent (Def_Id))) /= No_Location
               then
                  Error_Msg_Sloc := Sloc (Def_Id);
                  Error_Pragma_Arg
                    ("no initialization allowed for declaration of& #",
                     Arg2);

               else
                  Set_Is_Imported (Def_Id);
                  Set_Is_Public (Def_Id);
                  Process_Interface_Name (Def_Id, Arg2, Arg3);
               end if;

            --  Otherwise must be subprogram

            elsif not Is_Subprogram (Def_Id) then
               Error_Pragma_Arg
                 ("argument of pragma% is not subprogram", Arg1);

            else
               Check_At_Most_N_Arguments (3);
               Hom_Id := Def_Id;
               Found := False;

               --  Loop through homonyms

               loop
                  Def_Id := Get_Base_Subprogram (Hom_Id);

                  if Is_Imported (Def_Id) then
                     Process_Interface_Name (Def_Id, Arg2, Arg3);
                     Found := True;
                  end if;

                  Hom_Id := Homonym (Hom_Id);

                  exit when No (Hom_Id)
                    or else Scope (Hom_Id) /= Current_Scope;
               end loop;

               if not Found then
                  Error_Pragma_Arg
                    ("argument of pragma% is not imported subprogram",
                     Arg1);
               end if;
            end if;
         end Interface_Name;

         -----------------------
         -- Interrupt_Handler --
         -----------------------

         --  pragma Interrupt_Handler (handler_NAME);

         when Pragma_Interrupt_Handler =>
            Check_Ada_83_Warning;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Interrupt_Handler;

         ------------------------
         -- Interrupt_Priority --
         ------------------------

         --  pragma Interrupt_Priority [(EXPRESSION)];

         when Pragma_Interrupt_Priority => Interrupt_Priority : declare
            P   : constant Node_Id := Parent (N);
            Arg : Node_Id;

         begin
            Check_Ada_83_Warning;

            if Arg_Count /= 0 then
               Arg := Expression (Arg1);
               Check_Arg_Count (1);
               Check_No_Identifiers;

               --  Set In_Default_Expression for per-object case???

               Analyze_And_Resolve (Arg, Standard_Integer);
               if Expander_Active then
                  Rewrite_Substitute_Tree (Arg,
                    Convert_To (RTE (RE_Interrupt_Priority), Arg));
               end if;
            end if;

            if Nkind (P) /= N_Task_Definition
              and then Nkind (P) /= N_Protected_Definition
            then
               Pragma_Misplaced;
               return;

            elsif Has_Priority_Pragma (P) then
               Error_Pragma ("duplicate pragma% not allowed");

            else
               Set_Has_Priority_Pragma (P, True);
            end if;
         end Interrupt_Priority;

         ------------------
         -- Linker_Alias --
         ------------------

         --  pragma Linker_Alias (
         --      [Entity =>]  LOCAL_NAME
         --      [Alias  =>]  static_string_EXPRESSION);

         when Pragma_Linker_Alias =>
            GNAT_Pragma;
            Check_Arg_Count (2);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, Name_Alias);
            Check_Arg_Is_Library_Level_Local_Name (Arg1);
            Check_Arg_Is_Static_Expression (Arg2, Standard_String);

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Entity (Expression (Arg1)), N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Entity (Expression (Arg1)));
            end if;

         --------------------
         -- Linker_Options --
         --------------------

         --  pragma Linker_Options [string_EXPRESSION]

         when Pragma_Linker_Options =>
            Check_Ada_83_Warning;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Static_Expression (Arg1, Standard_String);
            Store_Linker_Option_String
              (Strval (Expr_Value_S (Expression (Arg1))));

         --------------------
         -- Linker_Section --
         --------------------

         --  pragma Linker_Section (
         --      [Entity  =>]  LOCAL_NAME
         --      [Section =>]  static_string_EXPRESSION);

         when Pragma_Linker_Section =>
            GNAT_Pragma;
            Check_Arg_Count (2);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Optional_Identifier (Arg2, Name_Section);
            Check_Arg_Is_Library_Level_Local_Name (Arg1);
            Check_Arg_Is_Static_Expression (Arg2, Standard_String);

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Entity (Expression (Arg1)), N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Entity (Expression (Arg1)));
            end if;

         ----------
         -- List --
         ----------

         --  pragma List (On | Off)

         --  There is nothing to do here, since we did all the processing
         --  for this pragma in Par.Prag (so that it works properly even in
         --  syntax only mode)

         when Pragma_List =>
            null;

         --------------------
         -- Locking_Policy --
         --------------------

         --  pragma Locking_Policy (policy_IDENTIFIER);

         when Pragma_Locking_Policy => declare
            LP : Character;

         begin
            Check_Ada_83_Warning;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Locking_Policy (Arg1);
            Check_Valid_Configuration_Pragma;
            Get_Name_String (Chars (Expression (Arg1)));
            LP := Fold_Upper (Name_Buffer (1));

            if Locking_Policy /= ' '
              and then Locking_Policy /= LP
            then
               Error_Msg_Sloc := Locking_Policy_Sloc;
               Error_Pragma ("locking policy incompatible with policy#");
            else
               Locking_Policy := LP;
               Locking_Policy_Sloc := Loc;
            end if;
         end;

         ----------------
         -- Long_Float --
         ----------------

         --  pragma Long_Float (D_Float | G_Float);

         when Pragma_Long_Float =>
            Check_VMS (N);
            Check_Valid_Configuration_Pragma;
            Check_Arg_Count (1);
            Check_No_Identifier (Arg1);
            Check_Arg_Is_One_Of (Arg1, Name_D_Float, Name_G_Float);

            if Chars (Expression (Arg1)) = Name_D_Float then
               Opt.Float_Format_Long := D_Float;
            else
               Opt.Float_Format_Long := G_Float;
            end if;

         -----------------------
         -- Machine_Attribute --
         -----------------------

         --  pragma Machine_Attribute (
         --    [Entity         =>] LOCAL_NAME,
         --    [Attribute_Name =>] static_string_EXPRESSION
         --  [,[Info           =>] static_string_EXPRESSION] );

         when Pragma_Machine_Attribute => Machine_Attribute : declare
            Id     : Entity_Id;
            Def_Id : Entity_Id;

         begin
            GNAT_Pragma;

            if Arg_Count = 3 then
               Check_Optional_Identifier (Arg3, "info");
               Check_Arg_Is_Static_Expression (Arg3, Standard_String);
            else
               Check_Arg_Count (2);
            end if;

            Check_Arg_Is_Local_Name (Arg1);
            Check_Optional_Identifier (Arg2, Name_Attribute_Name);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Static_Expression (Arg2, Standard_String);
            Def_Id := Entity (Expression (Arg1));

            if Is_Access_Type (Def_Id) then
               Def_Id := Designated_Type (Def_Id);
            end if;

            if Rep_Item_Too_Early (Def_Id, N) then
               return;
            end if;

            Def_Id := Underlying_Type (Def_Id);

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Def_Id, N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Entity (Expression (Arg1)));
            end if;
         end Machine_Attribute;

         ------------------
         -- Main_Storage --
         ------------------

         --  pragma Main_Storage
         --   (MAIN_STORAGE_OPTION [, MAIN_STORAGE_OPTION]);

         when Pragma_Main_Storage => Main_Storage : declare
            Args  : Args_List (1 .. 2);
            Names : Name_List (1 .. 2) := (
                      Name_Working_Storage,
                      Name_Top_Guard);

            Nod : Node_Id;

         begin
            Gather_Associations (Names, Args);

            for J in 1 .. 2 loop
               if Present (Args (J)) then
                  Check_Arg_Is_Static_Expression (Args (J), Any_Integer);
               end if;
            end loop;

            Check_In_Main_Program;

            Nod := Next (N);
            while Present (Nod) loop
               if Nkind (Nod) = N_Pragma
                 and then Chars (Nod) = Name_Main_Storage
               then
                  Error_Msg_Name_1 := Chars (N);
                  Error_Msg_N ("duplicate pragma% not permitted", Nod);
               end if;

               Nod := Next (Nod);
            end loop;

         end Main_Storage;

         -----------------
         -- Memory_Size --
         -----------------

         --  pragma Memory_Size (NUMERIC_LITERAL)

         when Pragma_Memory_Size =>

            --  Memory size is simply ignored

            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Integer_Literal (Arg1);

         ---------------
         -- No_Return --
         ---------------

         --  pragma No_Return (procedure_LOCAL_NAME);

         when Pragma_No_Return => declare
            Id    : Node_Id;
            E     : Entity_Id;
            Found : Boolean;

         begin
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Local_Name (Arg1);
            Id := Expression (Arg1);
            Analyze (Id);

            if not Is_Entity_Name (Id) then
               Error_Pragma_Arg ("entity name required", Arg1);
            end if;

            if Etype (Id) = Any_Type then
               raise Pragma_Exit;
            end if;

            E := Entity (Id);

            Found := False;
            while Present (E)
              and then Scope (E) = Current_Scope
            loop
               if Ekind (E) = E_Procedure
                 or else Ekind (E) = E_Generic_Procedure
               then
                  Set_No_Return (E);
                  Found := True;
               end if;

               E := Homonym (E);
            end loop;

            if not Found then
               Error_Pragma ("no procedures found for pragma%");
            end if;
         end;

         -----------------------
         -- Normalize_Scalars --
         -----------------------

         --  pragma Normalize_Scalars;

         when Pragma_Normalize_Scalars =>
            Check_Ada_83_Warning;
            Check_Arg_Count (0);
            Check_Valid_Configuration_Pragma;
            Normalize_Scalars := True;

         --------------
         -- Optimize --
         --------------

         --  pragma Optimize (Time | Space);

         --  The actual check for optimize is done in Gigi. Note that this
         --  pragma does not actually change the optimization setting, it
         --  simply checks that it is consistent with the pragma.

         when Pragma_Optimize =>
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_One_Of (Arg1, Name_Time, Name_Space, Name_Off);

         ----------
         -- Pack --
         ----------

         --  pragma Pack (first_subtype_LOCAL_NAME);

         when Pragma_Pack => Pack : declare
            Assoc   : Node_Id := Arg1;
            Type_Id : Node_Id := Expression (Assoc);
            Typ     : Entity_Id;
            Ctyp    : Entity_Id;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);

            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type
              or else Rep_Item_Too_Early (Typ, N)
            then
               return;
            else
               Typ := Underlying_Type (Typ);
            end if;

            if not Is_Array_Type (Typ) and then not Is_Record_Type (Typ) then
               Error_Pragma ("pragma% must specify array or record type");
            end if;

            Check_First_Subtype (Arg1);

            if Has_Pragma_Pack (Typ) then
               Error_Pragma ("duplicate pragma%, only one allowed");

            --  Array type. We set the Has_Pragma_Pack flag, and Is_Packed,
            --  but not Has_Non_Standard_Rep, because we don't actually know
            --  till freeze time if the array can have packed representation.
            --  That's because in the general case we do not know enough about
            --  the component type until it in turn is frozen, which certainly
            --  happens before the array type is frozen, but not necessarily
            --  till that point (i.e. right now it may be unfrozen).

            elsif Is_Array_Type (Typ) then
               if not Rep_Item_Too_Late (Typ, N) then
                  Set_Is_Packed       (Base_Type (Typ));
                  Set_Has_Pragma_Pack (Base_Type (Typ));
               end if;

            --  Record type. For record types, the pack is always effective

            else -- Is_Record_Type (Typ)
               if not Rep_Item_Too_Late (Typ, N) then
                  Set_Has_Pragma_Pack      (Base_Type (Typ));
                  Set_Is_Packed            (Base_Type (Typ));
                  Set_Has_Non_Standard_Rep (Base_Type (Typ));
               end if;
            end if;
         end Pack;

         ----------
         -- Page --
         ----------

         --  pragma Page;

         --  There is nothing to do here, since we did all the processing
         --  for this pragma in Par.Prag (so that it works properly even in
         --  syntax only mode)

         when Pragma_Page =>
            null;

         -------------
         -- Passive --
         -------------

         --  pragma Passive [(PASSIVE_FORM)];

         --   PASSIVE_FORM ::= Semaphore | No

         when Pragma_Passive =>
            GNAT_Pragma;

            if Nkind (Parent (N)) /= N_Task_Definition then
               Error_Pragma ("pragma% must be within task definition");
            end if;

            if Arg_Count /= 0 then
               Check_Arg_Count (1);
               Check_Arg_Is_One_Of (Arg1, Name_Semaphore, Name_No);
            end if;

         ------------------
         -- Preelaborate --
         ------------------

         --  pragma Preelaborate [(library_unit_NAME)];

         --  Set the flag Is_Preelaborated of program unit name entity

         when Pragma_Preelaborate => Preelaborate : declare
            Ent : Entity_Id;
            Pa  : Node_Id   := Parent (N);
            Pk  : Node_Kind := Nkind (Pa);

         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;
            Ent := Find_Lib_Unit_Name;

            --  This filters out pragmas inside generic parent then
            --  show up inside instantiation

            if Present (Ent)
              and then not (Pk = N_Package_Specification
                             and then Present (Generic_Parent (Pa)))
            then
               if not Debug_Flag_U then
                  Set_Is_Preelaborated (Ent);
               end if;
            end if;
         end Preelaborate;

         --------------
         -- Priority --
         --------------

         --  pragma Priority (EXPRESSION);

         when Pragma_Priority => Priority : declare
            P   : constant Node_Id := Parent (N);
            Arg : Node_Id;

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);

            Arg := Expression (Arg1);
            Analyze_And_Resolve (Arg, Standard_Integer);

            --  Subprogram case

            if Nkind (P) = N_Subprogram_Body then
               Check_In_Main_Program;

               --  Must be static

               if not Is_Static_Expression (Arg) then
                  Error_Pragma_Arg
                    ("main subprogram priority is not static", Arg1);

               --  If constraint error, then we already signalled an error

               elsif Raises_Constraint_Error (Arg) then
                  null;

               --  Otherwise check in range

               else
                  declare
                     Val : constant Uint := Expr_Value (Arg);

                  begin
                     if Val < 0 or else Val > Ttypes.System_Max_Priority then
                        Error_Pragma_Arg
                          ("main subprogram priority is out of range", Arg1);
                     end if;
                  end;
               end if;

               Set_Main_Priority
                 (Current_Sem_Unit, UI_To_Int (Expr_Value (Arg)));

            --  Task or Protected, must be of type Integer

            elsif (Nkind (P) = N_Protected_Definition
                     or else Nkind (P) = N_Task_Definition)
            then
               if Expander_Active then
                  Rewrite_Substitute_Tree (Arg,
                    Convert_To (RTE (RE_Any_Priority), Arg));
               end if;

            --  Anything else is incorrect

            else
               Pragma_Misplaced;
            end if;

            if Has_Priority_Pragma (P) then
               Error_Pragma ("duplicate pragma% not allowed");
            else
               Set_Has_Priority_Pragma (P, True);
            end if;

         end Priority;

         ------------------
         -- Psect_Object --
         ------------------

         --  pragma Psect_Object (
         --        [Internal =>] LOCAL_NAME,
         --     [, [External =>] EXTERNAL_SYMBOL]
         --     [, [Size     =>] EXTERNAL_SYMBOL]);

         when Pragma_Psect_Object => Psect_Object : declare
            Args  : Args_List (1 .. 3);
            Names : Name_List (1 .. 3) := (
                      Name_Internal,
                      Name_External,
                      Name_Size);

            Internal : Node_Id renames Args (1);
            External : Node_Id renames Args (2);
            Size     : Node_Id renames Args (3);

         begin
            Gather_Associations (Names, Args);
            Process_Extended_Import_Export_Object_Pragma (
              Arg_Internal => Internal,
              Arg_External => External,
              Arg_Size     => Size);
         end Psect_Object;

         ----------
         -- Pure --
         ----------

         --  pragma Pure [(library_unit_NAME)];

         when Pragma_Pure => Pure : declare
         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;
            Set_Is_Pure (Find_Lib_Unit_Name);
         end Pure;

         -------------------
         -- Pure_Function --
         -------------------

         --  pragma Pure_Function ([Entity =>] function_LOCAL_NAME);

         when Pragma_Pure_Function => Pure_Function : declare
            E_Id   : Node_Id;
            E      : Entity_Id;
            Def_Id : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Local_Name (Arg1);
            E_Id := Expression (Arg1);

            if Error_Posted (E_Id) then
               return;
            end if;

            --  Loop through homonyms (overloadings) of referenced entity

            E := Entity (E_Id);
            while Present (E) loop
               Def_Id := Get_Base_Subprogram (E);

               if Ekind (Def_Id) /= E_Function
                 and then Ekind (Def_Id) /= E_Generic_Function
                 and then Ekind (Def_Id) /= E_Operator
               then
                  Error_Pragma_Arg ("pragma% requires a function name", Arg1);
               end if;

               Set_Is_Pure (Def_Id);
               E := Homonym (E);
            end loop;
         end Pure_Function;

         --------------------
         -- Queuing_Policy --
         --------------------

         --  pragma Queuing_Policy (policy_IDENTIFIER);

         when Pragma_Queuing_Policy => declare
            QP : Character;

         begin
            Check_Ada_83_Warning;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Queuing_Policy (Arg1);
            Check_Valid_Configuration_Pragma;
            Get_Name_String (Chars (Expression (Arg1)));
            QP := Fold_Upper (Name_Buffer (1));

            if Queuing_Policy /= ' '
              and then Queuing_Policy /= QP
            then
               Error_Msg_Sloc := Queuing_Policy_Sloc;
               Error_Pragma ("queuing policy incompatible with policy#");
            else
               Queuing_Policy := QP;
               Queuing_Policy_Sloc := Loc;
            end if;
         end;

         ---------------------------
         -- Remote_Call_Interface --
         ---------------------------

         --  pragma Remote_Call_Interface [(library_unit_NAME)];

         when Pragma_Remote_Call_Interface => Remote_Call_Interface : declare
         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;
            Set_Is_Remote_Call_Interface (Find_Lib_Unit_Name);
         end Remote_Call_Interface;

         ------------------
         -- Remote_Types --
         ------------------

         --  pragma Remote_Types [(library_unit_NAME)];

         when Pragma_Remote_Types => Remote_Types : declare
         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;
            Set_Is_Remote_Types (Find_Lib_Unit_Name);
         end Remote_Types;

         ------------------
         -- Restrictions --
         ------------------

         --  pragma Restrictions (RESTRICTION {, RESTRICTION});

         --  RESTRICTION ::=
         --    restriction_IDENTIFIER
         --  | restriction_parameter_IDENTIFIER => EXPRESSION

         when Pragma_Restrictions => Restrictions_Pragma : declare
            Arg   : Node_Id;
            R_Id  : Restriction_Id;
            RP_Id : Restriction_Parameter_Id;
            Id    : Name_Id;
            Expr  : Node_Id;

         begin
            Check_Ada_83_Warning;
            Check_At_Least_N_Arguments (1);

            Any_Restrictions := True;
            Arg := Arg1;

            while Present (Arg) loop
               Id := Chars (Arg);
               Expr := Expression (Arg);

               --  Case of no restriction identifier

               if Id = No_Name then
                  if Nkind (Expr) /= N_Identifier then
                     Error_Pragma_Arg
                       ("invalid form for restriction", Arg);

                  else
                     R_Id := Get_Restriction_Id (Chars (Expr));

                     if R_Id = Not_A_Restriction_Id then
                        Error_Pragma_Arg
                          ("invalid restriction identifier", Arg);
                     else
                        Restrictions (R_Id) := True;
                     end if;
                  end if;

               --  Case of restriction identifier present

               else
                  RP_Id := Get_Restriction_Parameter_Id (Id);
                  Analyze_And_Resolve (Expr, Any_Integer);

                  if RP_Id = Not_A_Restriction_Parameter_Id then
                     Error_Pragma_Arg
                       ("invalid restriction parameter identifier", Arg);

                  elsif not Is_OK_Static_Expression (Expr)
                    or else not Is_Integer_Type (Etype (Expr))
                    or else Expr_Value (Expr) < 0
                  then
                     Error_Pragma_Arg
                       ("value must be non-negative static integer", Arg);

                  else
                     Restriction_Parameters (RP_Id) :=
                       Expr_Value (Expr);
                  end if;
               end if;

               Arg := Next (Arg);
            end loop;
         end Restrictions_Pragma;

         ----------------
         -- Reviewable --
         ----------------

         --  pragma Reviewable;

         when Pragma_Reviewable =>
            Check_Ada_83_Warning;
            Check_Arg_Count (0);

         -------------------
         -- Share_Generic --
         -------------------

         --  pragma Share_Generic (NAME {, NAME});

         when Pragma_Share_Generic =>
            Process_Generic_List;

         ------------
         -- Shared --
         ------------

         --  pragma Shared (LOCAL_NAME);

         when Pragma_Shared =>
            Process_Atomic_Shared_Volatile;

         --------------------
         -- Shared_Passive --
         --------------------

         --  pragma Shared_Passive [(library_unit_NAME)];

         --  Set the flag Is_Shared_Passive of program unit name entity

         when Pragma_Shared_Passive => Shared_Passive : declare
         begin
            Check_Ada_83_Warning;
            Check_Valid_Library_Unit_Pragma;
            Set_Is_Shared_Passive (Find_Lib_Unit_Name);
         end Shared_Passive;

         ----------------------
         -- Source_File_Name --
         ----------------------

         --  pragma Source_File_Name (
         --    [UNIT_NAME =>] unit_NAME,
         --    [BODY_FILE_NAME | SPEC_FILE_NAME] => STRING_LITERAL);

         --  No processing here. Processing was completed during parsing,
         --  since we need to have file names set as early as possible.
         --  Units are loaded well before semantic processing starts.

         --  The only processing we defer to this point is the check
         --  for correct placement.

         when Pragma_Source_File_Name =>
            Check_Valid_Configuration_Pragma;

         ----------------------
         -- Source_Reference --
         ----------------------

         --  pragma Source_Reference (INTEGER_LITERAL [, STRING_LITERAL]);

         --  Nothing to do, all processing completed in Par.Prag, since we
         --  need the information for possible parser messages that are output

         when Pragma_Source_Reference =>
            null;

         ------------------
         -- Storage_Size --
         ------------------

         --  pragma Storage_Size (EXPRESSION);

         when Pragma_Storage_Size => Storage_Size : declare
            P : constant Node_Id := Parent (N);

         begin
            Check_No_Identifiers;
            Check_Arg_Count (1);

            --  Set In_Default_Expression for per-object case???

            Analyze_And_Resolve (Expression (Arg1), Any_Integer);

            if Nkind (P) /= N_Task_Definition then
               Pragma_Misplaced;
               return;

            else
               if Has_Storage_Size_Pragma (P) then
                  Error_Pragma ("duplicate pragma% not allowed");
               else
                  Set_Has_Storage_Size_Pragma (P, True);
               end if;
            end if;
         end Storage_Size;

         ------------------
         -- Storage_Unit --
         ------------------

         --  pragma Storage_Unit (NUMERIC_LITERAL);

         --  Only permitted argument is System'Storage_Unit value

         when Pragma_Storage_Unit =>
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Integer_Literal (Arg1);

            if Intval (Expression (Arg1)) /=
              UI_From_Int (Ttypes.System_Storage_Unit)
            then
               Error_Msg_Uint_1 := Intval (Expression (Arg1));
               Error_Pragma_Arg
                 ("the only allowed argument for pragma% is ^", Arg1);
            end if;

         --------------
         -- Subtitle --
         --------------

         --  pragma Subtitle ([Subtitle =>] STRING_LITERAL)

         when Pragma_Subtitle =>
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Subtitle);
            Check_Arg_Is_String_Literal (Arg1);

         --------------
         -- Suppress --
         --------------

         --  pragma Suppress (IDENTIFIER [, [On =>] NAME]);

         when Pragma_Suppress =>
            Process_Suppress_Unsuppress (True);

         ------------------
         -- Suppress_All --
         ------------------

         --  pragma Suppress_All;

         --  The only check made here is that the pragma appears in the
         --  proper place, i.e. following a compilation unit. If indeed
         --  it appears in this context, then the parser has already
         --  inserted an equivalent pragma Suppress (All_Checks) to get
         --  the required effect.

         when Pragma_Suppress_All =>
            Check_Arg_Count (0);

            if Nkind (Parent (N)) /= N_Compilation_Unit
              or else not Is_List_Member (N)
              or else List_Containing (N) /= Pragmas_After (Parent (N))
            then
               Error_Pragma
                 ("misplaced pragma%, must follow compilation unit");
            end if;

         -----------------
         -- System_Name --
         -----------------

         --  pragma System_Name (DIRECT_NAME);

         --  Syntax check: one argument, which must be the identifier GNAT
         --  or the identifier GCC, no other identifiers are acceptable.

         when Pragma_System_Name =>
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_One_Of (Arg1, Name_Gcc, Name_Gnat);

         -----------------------------
         -- Task_Dispatching_Policy --
         -----------------------------

         --  pragma Task_Dispatching_Policy (policy_IDENTIFIER);

         when Pragma_Task_Dispatching_Policy => declare
            DP : Character;

         begin
            Check_Ada_83_Warning;
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_Arg_Is_Task_Dispatching_Policy (Arg1);
            Check_Valid_Configuration_Pragma;
            Get_Name_String (Chars (Expression (Arg1)));
            DP := Fold_Upper (Name_Buffer (1));

            if Task_Dispatching_Policy /= ' '
              and then Task_Dispatching_Policy /= DP
            then
               Error_Msg_Sloc := Task_Dispatching_Policy_Sloc;
               Error_Pragma
                 ("task dispatching policy incompatible with policy#");
            else
               Task_Dispatching_Policy := DP;
               Task_Dispatching_Policy_Sloc := Loc;
            end if;
         end;

         --------------
         -- Task_Info --
         --------------

         --  pragma Task_Info (EXPRESSION);

         when Pragma_Task_Info => Task_Info : declare
            P : constant Node_Id := Parent (N);

         begin
            GNAT_Pragma;

            if Nkind (P) /= N_Task_Definition then
               Error_Pragma ("pragma% must appear in task definition");
            end if;

            Check_No_Identifiers;
            Check_Arg_Count (1);

            Analyze_And_Resolve (Expression (Arg1), RTE (RE_Task_Info_Type));

            if Etype (Expression (Arg1)) = Any_Type then
               return;
            end if;

            if Has_Task_Info_Pragma (P) then
               Error_Pragma ("duplicate pragma% not allowed");
            else
               Set_Has_Task_Info_Pragma (P, True);
            end if;

         end Task_Info;

         ------------------
         -- Task_Storage --
         ------------------

         --  pragma Task_Storage (
         --     [Task_Type =>] LOCAL_NAME,
         --     [Top_Guard =>] static_integer_EXPRESSION);

         when Pragma_Task_Storage => Task_Storage : declare
            Args  : Args_List (1 .. 2);
            Names : Name_List (1 .. 2) := (
                      Name_Task_Type,
                      Name_Top_Guard);

            Task_Type : Node_Id renames Args (1);
            Top_Guard : Node_Id renames Args (2);

            Ent : Entity_Id;

         begin
            Gather_Associations (Names, Args);
            Check_Arg_Is_Local_Name (Task_Type);
            Check_Arg_Is_Static_Expression (Top_Guard, Any_Integer);

            Ent := Entity (Task_Type);

            if not Is_Task_Type (Ent) then
               Error_Pragma_Arg
                 ("argument for pragma% must be task type", Task_Type);
            end if;

            Check_First_Subtype (Task_Type);

            if Rep_Item_Too_Late (Ent, N) then
               raise Pragma_Exit;
            end if;

         end Task_Storage;

         ----------------
         -- Time_Slice --
         ----------------

         --  pragma Time_Slice (static_duration_EXPRESSION);

         when Pragma_Time_Slice => Time_Slice : declare
            Val : Ureal;
            Nod : Node_Id;

         begin
            Check_Arg_Count (1);
            Check_No_Identifiers;
            Check_In_Main_Program;
            Check_Arg_Is_Static_Expression (Arg1, Standard_Duration);

            if not Error_Posted (Arg1) then
               Nod := Next (N);
               while Present (Nod) loop
                  if Nkind (Nod) = N_Pragma
                    and then Chars (Nod) = Name_Time_Slice
                  then
                     Error_Msg_Name_1 := Chars (N);
                     Error_Msg_N ("duplicate pragma% not permitted", Nod);
                  end if;

                  Nod := Next (Nod);
               end loop;
            end if;

            --  Process only if in main unit

            if Get_Sloc_Unit_Number (Loc) = Main_Unit then
               Opt.Time_Slice_Set := True;
               Val := Expr_Value_R (Expression (Arg1));

               if Val <= Ureal_0 then
                  Opt.Time_Slice_Value := 0;

               elsif Val > UR_From_Uint (UI_From_Int (1000)) then
                  Opt.Time_Slice_Value := 1_000_000_000;

               else
                  Opt.Time_Slice_Value :=
                    UI_To_Int (UR_To_Uint (Val * UI_From_Int (1_000_000)));
               end if;
            end if;
         end Time_Slice;

         -----------
         -- Title --
         -----------

         --  pragma Title (TITLING_OPTION [, TITLING OPTION]);

         --   TITLING_OPTION ::=
         --     [Title =>] STRING_LITERAL
         --   | [Subtitle =>] STRING_LITERAL

         when Pragma_Title => Title : declare
            Args  : Args_List (1 .. 2);
            Names : Name_List (1 .. 2) := (
                      Name_Title,
                      Name_Subtitle);

         begin
            Gather_Associations (Names, Args);

            for J in 1 .. 2 loop
               if Present (Args (J)) then
                  Check_Arg_Is_String_Literal (Args (J));
               end if;
            end loop;
         end Title;

         ---------------------
         -- Unchecked_Union --
         ---------------------

         --  pragma Unchecked_Union (first_subtype_LOCAL_NAME)

         when Pragma_Unchecked_Union => Unchecked_Union : declare
            Assoc   : Node_Id := Arg1;
            Type_Id : Node_Id := Expression (Assoc);
            Typ     : Entity_Id;
            Discr   : Entity_Id;
            Tdef    : Node_Id;
            Clist   : Node_Id;
            Vpart   : Node_Id;
            Comp    : Node_Id;
            Variant : Node_Id;

         begin
            GNAT_Pragma;
            Check_No_Identifiers;
            Check_Arg_Count (1);
            Check_Arg_Is_Local_Name (Arg1);

            Find_Type (Type_Id);
            Typ := Entity (Type_Id);

            if Typ = Any_Type
              or else Rep_Item_Too_Early (Typ, N)
            then
               return;
            else
               Typ := Underlying_Type (Typ);
            end if;

            if Rep_Item_Too_Late (Typ, N) then
               return;
            end if;

            Check_First_Subtype (Arg1);

            --  Note remaining cases are references to a type in the current
            --  declarative part. If we find an error, we post the error on
            --  the relevant type declaration at an appropriate point.

            if not Is_Record_Type (Typ) then
               Error_Msg_N ("Unchecked_Union must be record type", Typ);
               return;

            elsif Is_Tagged_Type (Typ) then
               Error_Msg_N ("Unchecked_Union must not be tagged", Typ);
               return;

            elsif Is_Limited_Type (Typ) then
               Error_Msg_N
                 ("Unchecked_Union must not be limited record type", Typ);
               return;

            else
               if not Has_Discriminants (Typ) then
                  Error_Msg_N
                    ("Unchecked_Union must have one discriminant", Typ);
                  return;
               end if;

               Discr := First_Discriminant (Typ);

               if Present (Next_Discriminant (Discr)) then
                  Error_Msg_N
                    ("Unchecked_Union must have exactly one discriminant",
                     Next_Discriminant (Discr));
                  return;
               end if;

               if No (Discriminant_Default_Value (Discr)) then
                  Error_Msg_N
                    ("Unchecked_Union discriminant must have default value",
                     Discr);
               end if;

               Tdef  := Type_Definition (Declaration_Node (Typ));
               Clist := Component_List (Tdef);

               if No (Clist) or else No (Variant_Part (Clist)) then
                  Error_Msg_N
                    ("Unchecked_Union must have variant part",
                     Tdef);
                  return;
               end if;

               Vpart := Variant_Part (Clist);

               if Is_Non_Empty_List (Component_Items (Clist)) then
                  Error_Msg_N
                    ("components before variant not allowed " &
                     "in Unchecked_Union",
                     First (Component_Items (Clist)));
               end if;

               Variant := First (Variants (Vpart));
               while Present (Variant) loop
                  Clist := Component_List (Variant);

                  if Present (Variant_Part (Clist)) then
                     Error_Msg_N
                       ("Unchecked_Union may not have nested variants",
                        Variant_Part (Clist));
                  end if;

                  if not Is_Non_Empty_List (Component_Items (Clist)) then
                     Error_Msg_N
                       ("Unchecked_Union may not have empty component list",
                        Variant);
                     return;
                  end if;

                  Comp := First (Component_Items (Clist));

                  if Nkind (Comp) = N_Component_Declaration then

                     if Present (Expression (Comp)) then
                        Error_Msg_N
                          ("default initialization not allowed " &
                           "in Unchecked_Union",
                           Expression (Comp));
                     end if;

                     declare
                        Sindic : constant Node_Id :=
                                   Subtype_Indication (Comp);

                     begin
                        if Nkind (Sindic) = N_Subtype_Indication then
                           Check_Static_Constraint (Constraint (Sindic));
                        end if;
                     end;
                  end if;

                  if Present (Next (Comp)) then
                     Error_Msg_N
                       ("Unchecked_Union variant can have only one component",
                        Next (Comp));
                  end if;

                  Variant := Next (Variant);
               end loop;
            end if;

            Set_Is_Unchecked_Union           (Typ, True);
            Set_Suppress_Discriminant_Checks (Typ, True);
            Set_Convention                   (Typ, Convention_C);

            Set_Has_Unchecked_Union (Base_Type (Typ), True);
            Set_Is_Unchecked_Union  (Base_Type (Typ), True);

         end Unchecked_Union;

         ------------------------
         -- Unimplemented_Unit --
         ------------------------

         --  pragma Unimplemented_Unit;

         --  Note: this only gives an error if we are generating code,
         --  or if we are in a generic library unit (where the pragma
         --  appears in the body, not in the spec).

         when Pragma_Unimplemented_Unit => Unimplemented_Unit : declare
            Cunitent : Entity_Id := Cunit_Entity (Get_Sloc_Unit_Number (Loc));
            Ent_Kind : Entity_Kind := Ekind (Cunitent);

         begin
            GNAT_Pragma;
            Check_Arg_Count (0);

            if Operating_Mode = Generate_Code
              or else Ent_Kind = E_Generic_Function
              or else Ent_Kind = E_Generic_Procedure
              or else Ent_Kind = E_Generic_Package
            then
               Get_Name_String (Chars (Cunitent));
               Set_Casing (Mixed_Case);
               Write_Str (Name_Buffer (1 .. Name_Len));
               Write_Str (" is not implemented");
               Write_Eol;
               raise Unrecoverable_Error;
            end if;
         end Unimplemented_Unit;

         ----------------
         -- Unsuppress --
         ----------------

         --  pragma Unsuppress (IDENTIFIER [, [On =>] NAME]);

         when Pragma_Unsuppress =>
            Process_Suppress_Unsuppress (False);

         --------------
         -- Volatile --
         --------------

         --  pragma Volatile (LOCAL_NAME);

         when Pragma_Volatile =>
            Process_Atomic_Shared_Volatile;

         -------------------------
         -- Volatile_Components --
         -------------------------

         --  pragma Volatile_Components (array_LOCAL_NAME);

         --  Volatile is handled by the same circuit as Atomic_Components

         --------------
         -- Warnings --
         --------------

         --  pragma Warnings (On | Off, [LOCAL_NAME])

         when Pragma_Warnings =>
            GNAT_Pragma;
            Check_At_Least_N_Arguments (1);
            Check_At_Most_N_Arguments (2);
            Check_No_Identifiers;

            --  One argument case was processed by parser in Par.Prag

            if Arg_Count /= 1 then
               Check_Arg_Is_One_Of (Arg1, Name_On, Name_Off);
               Check_Arg_Count (2);
               Check_Arg_Is_Local_Name (Arg2);

               declare
                  E_Id : Node_Id;
                  E    : Entity_Id;

               begin
                  E_Id := Expression (Arg2);
                  Analyze (E_Id);

                  if not Is_Entity_Name (E_Id) then
                     Error_Pragma_Arg
                       ("second argument of pragma% must be entity name",
                        Arg2);
                  end if;

                  E := Entity (E_Id);

                  if E = Any_Id then
                     return;
                  else
                     loop
                        Set_Warnings_Off (E,
                          (Chars (Expression (Arg1)) = Name_Off));
                        exit when No (Homonym (E));
                        E := Homonym (E);
                     end loop;
                  end if;
               end;
            end if;

         -------------------
         -- Weak_External --
         -------------------

         --  pragma Weak_External ([Entity =>] LOCAL_NAME);

         when Pragma_Weak_External => Weak_External : declare
            Ent : Entity_Id;

         begin
            GNAT_Pragma;
            Check_Arg_Count (1);
            Check_Optional_Identifier (Arg1, Name_Entity);
            Check_Arg_Is_Library_Level_Local_Name (Arg1);
            Ent := Entity (Expression (Arg1));

            if Rep_Item_Too_Early (Ent, N) then
               return;
            else
               Ent := Underlying_Type (Ent);
            end if;

            --  The only processing required is to link this item on to the
            --  list of rep items for the given entity. This is accomplished
            --  by the call to Rep_Item_Too_Late (when no error is detected
            --  and False is returned).

            if Rep_Item_Too_Late (Ent, N) then
               return;
            else
               Set_Has_Gigi_Rep_Item (Ent);
            end if;
         end Weak_External;

      end case;

   exception
      when Pragma_Exit => null;

   end Analyze_Pragma;

   -------------------------
   -- Get_Base_Subprogram --
   -------------------------

   function Get_Base_Subprogram (Def_Id : Entity_Id) return Entity_Id is
      Result : Entity_Id;

   begin
      Result := Def_Id;

      --  Follow subprogram renaming chain

      while Is_Subprogram (Result)
        and then
          Nkind (Parent (Declaration_Node (Result))) =
            N_Subprogram_Renaming_Declaration
        and then Present (Alias (Result))
      loop
         Result := Alias (Result);
      end loop;

      return Result;
   end Get_Base_Subprogram;

   ---------------------------
   -- Is_Generic_Subprogram --
   ---------------------------

   function Is_Generic_Subprogram (Id : Entity_Id) return Boolean is
   begin
      return  Ekind (Id) = E_Generic_Procedure
        or else Ekind (Id) = E_Generic_Function;
   end Is_Generic_Subprogram;

   ------------------------------
   -- Is_Pragma_String_Literal --
   ------------------------------

   --  This function returns true if the corresponding pragma argument is
   --  a static string expression. These are the only cases in which string
   --  literals can appear as pragma arguments. We also allow a string
   --  literal as the first argument to pragma Assert (although it will
   --  of course always generate a type error).

   function Is_Pragma_String_Literal (Par : Node_Id) return Boolean is
      Pragn : constant Node_Id := Parent (Par);
      Assoc : constant List_Id := Pragma_Argument_Associations (Pragn);
      Pname : constant Name_Id := Chars (Pragn);
      Argn  : Natural;
      N     : Node_Id;

   begin
      Argn := 1;
      N := First (Assoc);
      loop
         exit when N = Par;
         Argn := Argn + 1;
         N := Next (N);
      end loop;

      if Pname = Name_Assert then
         return True;

      elsif Pname = Name_Export then
         return Argn > 2;

      elsif Pname = Name_Ident then
         return Argn = 1;

      elsif Pname = Name_Import then
         return Argn > 2;

      elsif Pname = Name_Interface_Name then
         return Argn > 1;

      elsif Pname = Name_Linker_Alias then
         return Argn = 2;

      elsif Pname = Name_Linker_Section then
         return Argn = 2;

      elsif Pname = Name_Machine_Attribute then
         return Argn = 2;

      elsif Pname = Name_Source_File_Name then
         return True;

      elsif Pname = Name_Source_Reference then
         return Argn = 2;

      elsif Pname = Name_Title then
         return True;

      elsif Pname = Name_Subtitle then
         return True;

      else
         return False;
      end if;

   end Is_Pragma_String_Literal;

   --------------------------------------
   -- Process_Compilation_Unit_Pragmas --
   --------------------------------------

   procedure Process_Compilation_Unit_Pragmas (N : Node_Id) is
   begin
      --  A special check for pragma Suppress_All. This is a strange DEC
      --  pragma, strange because it comes at the end of the unit. If we
      --  have a pragma Suppress_All in the Pragmas_After of the current
      --  unit, then we insert a pragma Suppress (All_Checks) at the start
      --  of the context clause to ensure the correct processing.

      --  We have to do that here, instead of the more natural

      if Present (Pragmas_After (N)) then
         declare
            P : Node_Id;

         begin
            P := First (Pragmas_After (N));
            while Present (P) loop
               if Chars (P) = Name_Suppress_All then
                  Prepend_To (Context_Items (N),
                    Make_Pragma (Sloc (P),
                      Chars => Name_Suppress,
                      Pragma_Argument_Associations => New_List (
                        Make_Pragma_Argument_Association (Sloc (P),
                          Expression =>
                            Make_Identifier (Sloc (P),
                              Chars => Name_All_Checks)))));
                  exit;
               end if;

               P := Next (P);
            end loop;
         end;
      end if;
   end Process_Compilation_Unit_Pragmas;

end Sem_Prag;
