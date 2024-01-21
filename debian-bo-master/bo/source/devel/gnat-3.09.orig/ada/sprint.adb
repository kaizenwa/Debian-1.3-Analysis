------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S P R I N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.171 $                            --
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

with Atree;   use Atree;
with Casing;  use Casing;
with Debug;   use Debug;
with Einfo;   use Einfo;
with Itypes;  use Itypes;
with Lib;     use Lib;
with Namet;   use Namet;
with Nlists;  use Nlists;
with Output;  use Output;
with Rtsfind; use Rtsfind;
with Sinfo;   use Sinfo;
with Snames;  use Snames;
with Stand;   use Stand;
with Stringt; use Stringt;
with Uintp;   use Uintp;
with Uname;   use Uname;
with Urealp;  use Urealp;

package body Sprint is

   Indent : Int := 0;
   --  Number of columns for current line output indentation

   Indent_Annull_Flag : Boolean := False;
   --  Set True if subsequent Write_Indent call to be ignored, gets reset
   --  by this call, so it is only active to suppress a single indent call.

   Dump_Original_Only : Boolean;
   --  Set True if the -do (dump original tree) flag is set

   Dump_Generated_Only : Boolean;
   --  Set True if the -dg (dump generated tree) flag is set

   Line_Limit : constant := 72;
   --  Limit value for chopping long lines

   Freeze_Indent : Int := 0;
   --  Keep track of freeze indent level (controls blank lines before
   --  procedures within expression freeze actions)

   Pure_Ada : Boolean := False;
   --  True if Sprint_Node_Pure_Ada was called

   -----------------------
   --  Local Procedures --
   -----------------------

   procedure Col_Check (N : Nat);
   --  Check that at least N characters remain on current line, and if not,
   --  then start an extra line with two characters extra indentation for
   --  continuing text on the next line.

   procedure Indent_Annull;
   --  Causes following call to Write_Indent to be ignored. This is used when
   --  a higher level node wants to stop a lower level node from starting a
   --  new line, when it would otherwise be inclined to do so (e.g. the case
   --  of an accept statement called from an accept alternative with a guard)

   procedure Indent_Begin;
   --  Increase indentation level

   procedure Indent_End;
   --  Decrease indentation level

   procedure Process_TFAI_RR_Flags (Nod : Node_Id);
   --  Given a divide, multiplication or division node, check the flags
   --  Treat_Fixed_As_Integer and Rounded_Flags, and if set, output the
   --  appropriate special syntax characters (# and @).

   procedure Sprint_Bar_List (List : List_Id);
   --  Print the given list with items separated by vertical bars

   procedure Sprint_Node_Actual (Node : Node_Id);
   --  This routine prints its node argument. It is a lower level routine than
   --  Sprint_Node, in that it does not bother about rewritten trees.

   procedure Write_Ekind (E : Entity_Id);
   --  Write the String corresponding to the Ekind without "E_".

   procedure Write_Id (N : Node_Id);
   --  Write Chars field of given node

   function Write_Identifiers (Node : Node_Id) return Boolean;
   --  Handle node where the grammar has a list of defining identifiers, but
   --  the tree has a separate declaration for each identifier. Handles the
   --  printing of the defining identifier, and returns True if the type and
   --  initialization information is to be printed, False if it is to be
   --  skipped (the latter case happens when printing defining identifiers
   --  other than the first in the original tree output case).

   procedure Write_Implicit_Def (E : Entity_Id);
   --  Write the definition of the implicit type E according to its Ekind

   procedure Write_Indent;
   --  Start a new line and write indentation spacing

   function Write_Indent_Identifiers (Node : Node_Id) return Boolean;
   --  Like Write_Identifiers except that each new printed declaration
   --  is at the start of a new line.

   procedure Write_Indent_Str (S : String);
   --  Start a new line and write indent spacing followed by given string

   procedure Write_Name_With_Col_Check (N : Name_Id);
   --  Write name (using Write_Name) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.

   procedure Write_Operator_Symbol_With_Col_Check (N : Name_Id);
   --  Write operator symbol that corresponds to the Name_Id, for example
   --  Name_Op_Eq and "=", with initial column check, and possible initial
   --  Write_Indent (to get new line) if current line is too full.

   procedure Write_Param_Specs (N : Node_Id);
   --  Output parameter specifications for node (which is either a function
   --  or procedure specification with a Parameter_Specifications field)

   procedure Write_Rewrite_Str (S : String);
   --  Writes out a string (typically containing { or }) for a node created
   --  by rewriting the tree. Suppressed if Debug_Flag_G is set, since in this
   --  case we don't specially mark nodes created by rewriting).

   procedure Write_Str_With_Col_Check (S : String);
   --  Write string (using Write_Str) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.

   procedure Write_Uint_With_Col_Check (U : Uint; Format : UI_Format);
   --  Write Uint (using UI_Write) with initial column check, and possible
   --  initial Write_Indent (to get new line) if current line is too full.
   --  The format parameter determines the output format (see UI_Write).

   procedure Write_Ureal_With_Col_Check (U : Ureal);
   --  Write Ureal (using same output format as UR_Write) with column checks
   --  and a possible initial Write_Indent (to get new line) if current line
   --  is too full.

   ---------------
   -- Col_Check --
   ---------------

   procedure Col_Check (N : Nat) is
   begin
      if N + Column > Line_Limit then
         Write_Indent_Str ("  ");
      end if;
   end Col_Check;

   -------------------
   -- Indent_Annull --
   -------------------

   procedure Indent_Annull is
   begin
      Indent_Annull_Flag := True;
   end Indent_Annull;

   ------------------
   -- Indent_Begin --
   ------------------

   procedure Indent_Begin is
   begin
      Indent := Indent + 3;
   end Indent_Begin;

   ----------------
   -- Indent_End --
   ----------------

   procedure Indent_End is
   begin
      Indent := Indent - 3;
   end Indent_End;

   --------
   -- PG --
   --------

   procedure PG (Node : Node_Id) is
   begin
      Dump_Generated_Only := True;
      Dump_Original_Only := False;
      Sprint_Node (Node);
      Write_Eol;
   end PG;

   --------
   -- PO --
   --------

   procedure PO (Node : Node_Id) is
   begin
      Dump_Generated_Only := False;
      Dump_Original_Only := True;
      Sprint_Node (Node);
      Write_Eol;
   end PO;

   ---------------------------
   -- Process_TFAI_RR_Flags --
   ---------------------------

   procedure Process_TFAI_RR_Flags (Nod : Node_Id) is
   begin
      if Treat_Fixed_As_Integer (Nod) then
         pragma Assert (not Pure_Ada);
         Write_Char ('#');
      end if;

      if Rounded_Result (Nod) then
         pragma Assert (not Pure_Ada);
         Write_Char ('@');
      end if;
   end Process_TFAI_RR_Flags;

   --------
   -- PS --
   --------

   procedure PS (Node : Node_Id) is
   begin
      Dump_Generated_Only := False;
      Dump_Original_Only := False;
      Sprint_Node (Node);
      Write_Eol;
   end PS;

   -----------------
   -- Source_Dump --
   -----------------

   procedure Source_Dump is
      Max_Unit : Unit_Number_Type;

      procedure Underline;
      --  Put underline under string we just printed

      procedure Underline is
         Col : constant Int := Column;

      begin
         Write_Eol;

         while Col > Column loop
            Write_Char ('-');
         end loop;

         Write_Eol;
      end Underline;

   --  Start of processing for Tree_Dump.

   begin
      Dump_Generated_Only := Debug_Flag_G;
      Dump_Original_Only  := Debug_Flag_O;

      --  Note that we turn off the tree dump flags immediately, before
      --  starting the dump. This avoids generating two copies of the dump
      --  if an abort occurs after printing the dump, and more importantly,
      --  avoids an infinite loop if an abort occurs during the dump.

      if Debug_Flag_Z then
         Debug_Flag_Z := False;
         Write_Eol;
         Write_Eol;
         Write_Str ("Source recreated from tree of Standard (spec)");
         Underline;
         Sprint_Node (Standard_Package_Node);
         Write_Eol;
         Write_Eol;
      end if;

      if Debug_Flag_S or Debug_Flag_G or Debug_Flag_O then
         Debug_Flag_G := False;
         Debug_Flag_O := False;
         Debug_Flag_S := False;

         if Debug_Flag_F then
            Max_Unit := Last_Unit;
         else
            Max_Unit := Main_Unit;
         end if;

         for U in Main_Unit .. Max_Unit loop
            Write_Str ("Source recreated from tree for ");
            Write_Unit_Name (Unit_Name (U));
            Underline;
            Sprint_Node (Cunit (U));
            Write_Eol;
            Write_Eol;
         end loop;
      end if;
   end Source_Dump;

   ---------------------
   -- Sprint_Bar_List --
   ---------------------

   procedure Sprint_Bar_List (List : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Sprint_Node (Node);
            Node := Next (Node);
            exit when Node = Empty;
            Write_Str (" | ");
         end loop;
      end if;
   end Sprint_Bar_List;

   -----------------------
   -- Sprint_Comma_List --
   -----------------------

   procedure Sprint_Comma_List (List : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Sprint_Node (Node);
            Node := Next (Node);
            exit when Node = Empty;

            if not Is_Rewrite_Insertion (Node)
              or else not Dump_Original_Only
            then
               Write_Str (", ");
            end if;

         end loop;
      end if;
   end Sprint_Comma_List;

   --------------------------
   -- Sprint_Indented_List --
   --------------------------

   procedure Sprint_Indented_List (List : List_Id) is
   begin
      Indent_Begin;
      Sprint_Node_List (List);
      Indent_End;
   end Sprint_Indented_List;

   -----------------
   -- Sprint_Node --
   -----------------

   procedure Sprint_Node (Node : Node_Id) is
   begin
      if Is_Rewrite_Insertion (Node) then
         if not Dump_Original_Only then
            Write_Rewrite_Str ("{");
            Sprint_Node_Actual (Node);
            Write_Rewrite_Str ("}");
         end if;

      elsif Is_Rewrite_Substitution (Node) then
         if Dump_Generated_Only then
            Sprint_Node_Actual (Node);
         elsif Dump_Original_Only then
            Sprint_Node_Actual (Original_Node (Node));
         else
            Sprint_Node_Actual (Original_Node (Node));
            Write_Rewrite_Str ("{");
            Sprint_Node_Actual (Node);
            Write_Rewrite_Str ("}");
         end if;

      else
         Sprint_Node_Actual (Node);
      end if;
   end Sprint_Node;

   ------------------------
   -- Sprint_Node_Actual --
   ------------------------

   procedure Sprint_Node_Actual (Node : Node_Id)
   is
   begin
      if Node = Empty then
         return;
      end if;

      for J in 1 .. Paren_Count (Node) loop
         Write_Str_With_Col_Check ("(");
      end loop;

      case Nkind (Node) is

         when N_Abort_Statement =>
            Write_Indent_Str ("abort ");
            Sprint_Comma_List (Names (Node));
            Write_Char (';');

         when N_Abortable_Part =>
            Sprint_Indented_List (Statements (Node));

         when N_Abstract_Subprogram_Declaration =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Str_With_Col_Check (" is abstract;");

         when N_Accept_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));

            if Present (Condition (Node)) then
               Write_Indent;
               Write_Str_With_Col_Check ("when ");
               Sprint_Node (Condition (Node));
               Write_Str (" => ");
               Indent_Annull;
            end if;

            Sprint_Node (Accept_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Accept_Statement =>
            Write_Indent_Str ("accept ");
            Write_Id (Entry_Direct_Name (Node));

            if Present (Entry_Index (Node)) then
               Write_Str_With_Col_Check (" (");
               Sprint_Node (Entry_Index (Node));
               Write_Char (')');
            end if;

            Write_Param_Specs (Node);

            if Present (Handled_Statement_Sequence (Node)) then
               Write_Str_With_Col_Check (" do");
               Sprint_Node (Handled_Statement_Sequence (Node));
               Write_Indent_Str ("end ");
               Write_Id (Entry_Direct_Name (Node));
            end if;

            Write_Char (';');

         when N_Access_Definition =>
            Write_Str_With_Col_Check ("access ");
            Sprint_Node (Subtype_Mark (Node));

         when N_Access_Function_Definition =>
            Write_Str_With_Col_Check ("access ");

            if Protected_Present (Node) then
               Write_Str_With_Col_Check ("protected ");
            end if;

            Write_Str_With_Col_Check ("function");
            Write_Param_Specs (Node);
            Write_Str_With_Col_Check (" return ");
            Sprint_Node (Subtype_Mark (Node));

         when N_Access_Procedure_Definition =>
            Write_Str_With_Col_Check ("access ");

            if Protected_Present (Node) then
               Write_Str_With_Col_Check ("protected ");
            end if;

            Write_Str_With_Col_Check ("procedure");
            Write_Param_Specs (Node);

         when N_Access_To_Object_Definition =>
            Write_Str_With_Col_Check ("access ");

            if All_Present (Node) then
               Write_Str_With_Col_Check ("all ");
            elsif Constant_Present (Node) then
               Write_Str_With_Col_Check ("constant ");
            end if;

            Sprint_Node (Subtype_Indication (Node));

         when N_And_Then =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" and then ");
            Sprint_Node (Right_Opnd (Node));

         when N_At_Clause =>
            Write_Indent_Str ("for ");
            Write_Id (Identifier (Node));
            Write_Str_With_Col_Check (" use at ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Aggregate =>
            if Null_Record_Present (Node) then
               Write_Str_With_Col_Check ("(null record)");

            else
               Write_Str_With_Col_Check ("(");

               if Present (Expressions (Node)) then
                  Sprint_Comma_List (Expressions (Node));

                  if Present (Component_Associations (Node)) then
                     Write_Str (", ");
                  end if;
               end if;

               if Present (Component_Associations (Node)) then
                  Sprint_Comma_List (Component_Associations (Node));
               end if;

               Write_Char (')');
            end if;

         when N_Mod_Clause =>
            Sprint_Node_List (Pragmas_Before (Node));
            Write_Str_With_Col_Check ("at mod ");
            Sprint_Node (Expression (Node));

         when N_Allocator =>
            Write_Str_With_Col_Check ("new ");
            Sprint_Node (Expression (Node));

            if Present (Storage_Pool (Node)) then
               pragma Assert (not Pure_Ada);
               Write_Str_With_Col_Check ("[storage_pool = ");
               Sprint_Node (Storage_Pool (Node));
               Write_Char (']');
            end if;

         when N_Assignment_Statement =>
            Write_Indent;
            Sprint_Node (Name (Node));
            Write_Str (" := ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Asynchronous_Select =>
            Write_Indent_Str ("select");
            Indent_Begin;
            Sprint_Node (Triggering_Alternative (Node));
            Indent_End;
            Write_Indent_Str ("then abort");
            Sprint_Node (Abortable_Part (Node));
            Write_Indent_Str ("end select;");

         when N_Attribute_Definition_Clause =>
            Write_Indent_Str ("for ");
            Sprint_Node (Name (Node));
            Write_Char (''');
            Write_Name_With_Col_Check (Chars (Node));
            Write_Str_With_Col_Check (" use ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Attribute_Reference =>
            if Is_Procedure_Attribute_Name (Attribute_Name (Node)) then
               Write_Indent;
            end if;

            Sprint_Node (Prefix (Node));
            Write_Char (''');
            Write_Name_With_Col_Check (Attribute_Name (Node));
            Sprint_Paren_Comma_List (Expressions (Node));

            if Is_Procedure_Attribute_Name (Attribute_Name (Node)) then
               Write_Char (';');
            end if;

         when N_Block_Statement =>
            Write_Indent;

            if Present (Identifier (Node))
              and then (not Has_Created_Identifier (Node)
                          or else not Dump_Original_Only)
            then
               Write_Rewrite_Str ("{");
               Write_Id (Identifier (Node));
               Write_Str (" : ");
               Write_Rewrite_Str ("}");
            end if;

            if Present (Declarations (Node)) then
               Write_Str_With_Col_Check ("declare");
               Sprint_Indented_List (Declarations (Node));
               Write_Indent;
            end if;

            Write_Str_With_Col_Check ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_Str ("end");

            if Present (Identifier (Node))
              and then (not Has_Created_Identifier (Node)
                          or else not Dump_Original_Only)
            then
               Write_Rewrite_Str ("{");
               Write_Char (' ');
               Write_Id (Identifier (Node));
               Write_Rewrite_Str ("}");
            end if;

            Write_Char (';');

         when N_Case_Statement =>
            Write_Indent_Str ("case ");
            Sprint_Node (Expression (Node));
            Write_Str (" is");
            Sprint_Indented_List (Alternatives (Node));
            Write_Indent_Str ("end case;");

         when N_Case_Statement_Alternative =>
            Write_Indent_Str ("when ");
            Sprint_Bar_List (Discrete_Choices (Node));
            Write_Str (" => ");
            Sprint_Indented_List (Statements (Node));

         when N_Character_Literal =>
            if Column > 70 then
               Write_Indent_Str ("  ");
            end if;

            Write_Char (''');
            Write_Char_Code (Char_Literal_Value (Node));
            Write_Char (''');

         when N_Code_Statement =>
            Write_Indent;
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Compilation_Unit =>
            Sprint_Node_List (Context_Items (Node));

            if Private_Present (Node) then
               Write_Indent_Str ("private ");
               Indent_Annull;
            end if;

            Sprint_Node (Unit (Node));
            Sprint_Opt_Node_List (Pragmas_After (Node));

         when N_Component_Association =>
            Sprint_Bar_List (Choices (Node));
            Write_Str (" => ");
            Sprint_Node (Expression (Node));

         when N_Component_Clause =>
            Write_Indent;
            Sprint_Node (Component_Name (Node));
            Write_Str (" at ");
            Sprint_Node (Position (Node));
            Write_Char (' ');
            Write_Str_With_Col_Check ("range ");
            Sprint_Node (First_Bit (Node));
            Write_Str (" .. ");
            Sprint_Node (Last_Bit (Node));
            Write_Char (';');

         when N_Component_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_Str (" : ");

               if Aliased_Present (Node) then
                  Write_Str_With_Col_Check ("aliased ");
               end if;

               Sprint_Node (Subtype_Indication (Node));

               if Present (Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Expression (Node));
               end if;

               Write_Char (';');
            end if;

         when N_Component_List =>
            if Null_Present (Node) then
               Indent_Begin;
               Write_Indent_Str ("null");
               Write_Char (';');
               Indent_End;

            else
               Sprint_Indented_List (Component_Items (Node));
               Sprint_Node (Variant_Part (Node));
            end if;

         when N_Concat_Multiple =>
            declare
               Expr : Node_Id;

            begin
               pragma Assert (not Pure_Ada);
               Expr := First (Expressions (Node));

               loop
                  Sprint_Node (Expr);
                  Expr := Next (Expr);
                  exit when No (Expr);
                  Write_Str (" && ");
               end loop;
            end;

         when N_Conditional_Entry_Call =>
            Write_Indent_Str ("select");
            Indent_Begin;
            Sprint_Node (Entry_Call_Alternative (Node));
            Indent_End;
            Write_Indent_Str ("else");
            Sprint_Indented_List (Else_Statements (Node));
            Write_Indent_Str ("end select;");

         when N_Conditional_Expression =>
            declare
               Condition : constant Node_Id := First (Expressions (Node));
               Then_Expr : constant Node_Id := Next (Condition);
               Else_Expr : constant Node_Id := Next (Then_Expr);

            begin
               pragma Assert (not Pure_Ada);
               Write_Str_With_Col_Check ("(if ");
               Sprint_Node (Condition);
               Write_Str_With_Col_Check (" then ");
               Sprint_Node (Then_Expr);
               Write_Str_With_Col_Check (" else ");
               Sprint_Node (Else_Expr);
               Write_Char (')');
            end;

         when N_Constrained_Array_Definition =>
            Write_Str_With_Col_Check ("array ");
            Sprint_Paren_Comma_List (Discrete_Subtype_Definitions (Node));
            Write_Str (" of ");

            if Aliased_Present (Node) then
               Write_Str_With_Col_Check ("aliased ");
            end if;

            Sprint_Node (Subtype_Indication (Node));

         when N_Decimal_Fixed_Point_Definition =>
            Write_Str_With_Col_Check ("digits ");
            Sprint_Node (Digits_Expression (Node));
            Write_Str_With_Col_Check (" delta ");
            Sprint_Node (Delta_Expression (Node));
            Sprint_Opt_Node (Real_Range_Specification (Node));

         when N_Defining_Character_Literal =>
            Write_Name_With_Col_Check (Chars (Node));

         when N_Defining_Identifier =>
            Write_Id (Node);

         when N_Defining_Operator_Symbol =>
            if Pure_Ada then
               Write_Operator_Symbol_With_Col_Check (Chars (Node));
            else
               Write_Name_With_Col_Check (Chars (Node));
            end if;

         when N_Defining_Program_Unit_Name =>
            Sprint_Node (Name (Node));
            Write_Char ('.');
            Write_Id (Defining_Identifier (Node));

         when N_Delay_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));

            if Present (Condition (Node)) then
               Write_Indent;
               Write_Str_With_Col_Check ("when ");
               Sprint_Node (Condition (Node));
               Write_Str (" => ");
               Indent_Annull;
            end if;

            Sprint_Node (Delay_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Delay_Relative_Statement =>
            Write_Indent_Str ("delay ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Delay_Until_Statement =>
            Write_Indent_Str ("delay until ");
            Sprint_Node (Expression (Node));
            Write_Char (';');

         when N_Delta_Constraint =>
            Write_Str_With_Col_Check ("delta ");
            Sprint_Node (Delta_Expression (Node));
            Sprint_Opt_Node (Range_Constraint (Node));

         when N_Derived_Type_Definition =>
            if Abstract_Present (Node) then
               Write_Str_With_Col_Check ("abstract ");
            end if;

            Write_Str_With_Col_Check ("new ");
            Sprint_Node (Subtype_Indication (Node));

            if Present (Record_Extension_Part (Node)) then
               Write_Str_With_Col_Check (" with ");
               Sprint_Node (Record_Extension_Part (Node));
            end if;

         when N_Designator =>
            Sprint_Node (Name (Node));
            Write_Char ('.');
            Write_Id (Identifier (Node));

         when N_Digits_Constraint =>
            Write_Str_With_Col_Check ("digits ");
            Sprint_Node (Digits_Expression (Node));
            Sprint_Opt_Node (Range_Constraint (Node));

         when N_Discriminant_Association =>
            if Present (Selector_Names (Node)) then
               Sprint_Bar_List (Selector_Names (Node));
               Write_Str (" => ");
            end if;

            Sprint_Node (Expression (Node));

         when N_Discriminant_Specification =>
            if Write_Identifiers (Node) then
               Write_Str (" : ");
               Sprint_Node (Discriminant_Type (Node));

               if Present (Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Expression (Node));
               end if;
            end if;

         when N_Elsif_Part =>
            Write_Indent_Str ("elsif ");
            Sprint_Node (Condition (Node));
            Write_Str_With_Col_Check (" then");
            Sprint_Indented_List (Then_Statements (Node));

         when N_Empty =>
            null;

         when N_Entry_Body =>
            Write_Indent_Str ("entry ");
            Write_Id (Defining_Identifier (Node));
            Sprint_Node (Entry_Body_Formal_Part (Node));
            Write_Str_With_Col_Check (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_Str ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_Str ("end ");
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Entry_Body_Formal_Part =>
            if Present (Entry_Index_Specification (Node)) then
               Write_Str_With_Col_Check (" (");
               Sprint_Node (Entry_Index_Specification (Node));
               Write_Char (')');
            end if;

            Write_Param_Specs (Node);
            Write_Str_With_Col_Check (" when ");
            Sprint_Node (Condition (Node));

         when N_Entry_Call_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));
            Sprint_Node (Entry_Call_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Entry_Call_Statement =>
            Write_Indent;
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Parameter_Associations (Node));
            Write_Char (';');

         when N_Entry_Declaration =>
            Write_Indent_Str ("entry ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discrete_Subtype_Definition (Node)) then
               Write_Str_With_Col_Check (" (");
               Sprint_Node (Discrete_Subtype_Definition (Node));
               Write_Char (')');
            end if;

            Write_Param_Specs (Node);
            Write_Char (';');

         when N_Entry_Index_Specification =>
            Write_Str_With_Col_Check ("for ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" in ");
            Sprint_Node (Discrete_Subtype_Definition (Node));

         when N_Enumeration_Representation_Clause =>
            Write_Indent_Str ("for ");
            Write_Id (Identifier (Node));
            Write_Str_With_Col_Check (" use ");
            Sprint_Node (Array_Aggregate (Node));
            Write_Char (';');

         when N_Enumeration_Type_Definition =>

            --  Skip attempt to print Literals field if it's not there and
            --  we are in package Standard (case of Character, which is
            --  handled specially (without an explicit literals list).

            if Sloc (Node) > Standard_Location
              or else Present (Literals (Node))
            then
               Sprint_Paren_Comma_List (Literals (Node));
            end if;

         when N_Error =>
            Write_Str_With_Col_Check ("<error>");

         when N_Exception_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_Str_With_Col_Check (" : exception;");
            end if;

         when N_Exception_Handler =>
            Write_Indent_Str ("when ");

            if Present (Choice_Parameter (Node)) then
               Sprint_Node (Choice_Parameter (Node));
               Write_Str (" : ");
            end if;

            Sprint_Bar_List (Exception_Choices (Node));
            Write_Str (" => ");
            Sprint_Indented_List (Statements (Node));

         when N_Exception_Renaming_Declaration =>
            Write_Indent;
            Sprint_Node (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" : exception renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Exit_Statement =>
            Write_Indent_Str ("exit");
            Sprint_Opt_Node (Name (Node));

            if Present (Condition (Node)) then
               Write_Str_With_Col_Check (" when ");
               Sprint_Node (Condition (Node));
            end if;

            Write_Char (';');

         when N_Explicit_Dereference =>
            Sprint_Node (Prefix (Node));
            Write_Str (".all");

         when N_Extension_Aggregate =>
            Write_Str_With_Col_Check ("(");
            Sprint_Node (Ancestor_Part (Node));
            Write_Str_With_Col_Check (" with ");

            if Null_Record_Present (Node) then
               Write_Str_With_Col_Check ("null record");
            else
               if Present (Expressions (Node)) then
                  Sprint_Comma_List (Expressions (Node));

                  if Present (Component_Associations (Node)) then
                     Write_Str (", ");
                  end if;
               end if;

               if Present (Component_Associations (Node)) then
                  Sprint_Comma_List (Component_Associations (Node));
               end if;
            end if;

            Write_Char (')');

         when N_Floating_Point_Definition =>
            Write_Str_With_Col_Check ("digits ");
            Sprint_Node (Digits_Expression (Node));
            Sprint_Opt_Node (Real_Range_Specification (Node));

         when N_Formal_Decimal_Fixed_Point_Definition =>
            Write_Str_With_Col_Check ("delta <> digits <>");

         when N_Formal_Derived_Type_Definition =>
            Write_Str_With_Col_Check ("new ");
            Sprint_Node (Subtype_Mark (Node));

            if Private_Present (Node) then
               Write_Str_With_Col_Check (" with private");
            end if;

         when N_Formal_Discrete_Type_Definition =>
            Write_Str_With_Col_Check ("<>");

         when N_Formal_Floating_Point_Definition =>
            Write_Str_With_Col_Check ("digits <>");

         when N_Formal_Modular_Type_Definition =>
            Write_Str_With_Col_Check ("mod <>");

         when N_Formal_Object_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_Str (" : ");

               if In_Present (Node) then
                  Write_Str_With_Col_Check ("in ");
               end if;

               if Out_Present (Node) then
                  Write_Str_With_Col_Check ("out ");
               end if;

               Sprint_Node (Subtype_Mark (Node));

               if Present (Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Expression (Node));
               end if;

               Write_Char (';');
            end if;

         when N_Formal_Ordinary_Fixed_Point_Definition =>
            Write_Str_With_Col_Check ("delta <>");

         when N_Formal_Package_Declaration =>
            Write_Indent_Str ("with package ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" is new ");
            Sprint_Node (Name (Node));
            Write_Str_With_Col_Check (" (<>);");

         when N_Formal_Private_Type_Definition =>
            if Abstract_Present (Node) then
               Write_Str_With_Col_Check ("abstract ");
            end if;

            if Tagged_Present (Node) then
               Write_Str_With_Col_Check ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_Str_With_Col_Check ("limited ");
            end if;

            Write_Str_With_Col_Check ("private");

         when N_Formal_Signed_Integer_Type_Definition =>
            Write_Str_With_Col_Check ("range <>");

         when N_Formal_Subprogram_Declaration =>
            Write_Indent_Str ("with ");
            Sprint_Node (Specification (Node));

            if Box_Present (Node) then
               Write_Str_With_Col_Check (" is <>");
            elsif Present (Default_Name (Node)) then
               Write_Str_With_Col_Check (" is ");
               Sprint_Node (Default_Name (Node));
            end if;

            Write_Char (';');

         when N_Formal_Type_Declaration =>
            Write_Indent_Str ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Sprint_Paren_Comma_List
                 (Discriminant_Specifications (Node));
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_With_Col_Check ("(<>)");
            end if;

            Write_Str_With_Col_Check (" is ");
            Sprint_Node (Formal_Type_Definition (Node));
            Write_Char (';');

         when N_Free_Statement =>
            pragma Assert (not Pure_Ada);
            Write_Str_With_Col_Check ("free ");
            Sprint_Node (Expression (Node));

         when N_Freeze_Entity =>
            if not Dump_Original_Only then
               pragma Assert (not Pure_Ada);
               Write_Indent;
               Write_Rewrite_Str ("{");
               Write_Str_With_Col_Check ("freeze ");
               Write_Id (Entity (Node));
               Write_Str (" [");

               if No (Actions (Node)) then
                  Write_Char (']');

               else
                  Freeze_Indent := Freeze_Indent + 1;
                  Sprint_Indented_List (Actions (Node));
                  Freeze_Indent := Freeze_Indent - 1;
                  Write_Indent_Str ("]");
                  Write_Rewrite_Str ("}");
               end if;
            end if;

         when N_Full_Type_Declaration =>
            Write_Indent_Str ("type ");
            Write_Id (Defining_Identifier (Node));
            Sprint_Opt_Paren_Comma_List
              (Discriminant_Specifications (Node));
            Write_Str_With_Col_Check (" is ");
            Sprint_Node (Type_Definition (Node));
            Write_Char (';');

         when N_Function_Call =>
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Parameter_Associations (Node));

         when N_Function_Instantiation =>
            Write_Indent_Str ("function ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" is new ");
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Generic_Associations (Node));
            Write_Char (';');

         when N_Function_Specification =>
            Write_Str_With_Col_Check ("function ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Param_Specs (Node);
            Write_Str_With_Col_Check (" return ");
            Sprint_Node (Subtype_Mark (Node));

         when N_Generic_Association =>
            if Present (Selector_Name (Node)) then
               Sprint_Node (Selector_Name (Node));
               Write_Str (" => ");
            end if;

            Sprint_Node (Explicit_Generic_Actual_Parameter (Node));

         when N_Generic_Function_Renaming_Declaration =>
            Write_Indent_Str ("generic function ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Generic_Package_Declaration =>
            Write_Indent;
            Write_Indent_Str ("generic ");
            Sprint_Indented_List (Generic_Formal_Declarations (Node));
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Generic_Package_Renaming_Declaration =>
            Write_Indent_Str ("generic package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Generic_Procedure_Renaming_Declaration =>
            Write_Indent_Str ("generic procedure ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Generic_Subprogram_Declaration =>
            Write_Indent;
            Write_Indent_Str ("generic ");
            Sprint_Indented_List (Generic_Formal_Declarations (Node));
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Goto_Statement =>
            Write_Indent_Str ("goto ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Handled_Sequence_Of_Statements =>
            Sprint_Indented_List (Statements (Node));

            if Present (Exception_Handlers (Node)) then
               Write_Indent_Str ("exception");
               Indent_Begin;
               Sprint_Node_List (Exception_Handlers (Node));
               Indent_End;
            end if;

            if Present (Identifier (Node)) then
               pragma Assert (not Pure_Ada);
               Write_Indent_Str ("at end");
               Indent_Begin;
               Write_Indent;
               Sprint_Node (Identifier (Node));
               Write_Char (';');
               Indent_End;
            end if;

         when N_Identifier =>
            Write_Id (Node);

         when N_If_Statement =>
            Write_Indent_Str ("if ");
            Sprint_Node (Condition (Node));
            Write_Str_With_Col_Check (" then");
            Sprint_Indented_List (Then_Statements (Node));
            Sprint_Opt_Node_List (Elsif_Parts (Node));

            if Present (Else_Statements (Node)) then
               Write_Indent_Str ("else");
               Sprint_Indented_List (Else_Statements (Node));
            end if;

            Write_Indent_Str ("end if;");

         when N_Implicit_Label_Declaration =>
            if not Dump_Original_Only then
               pragma Assert (not Pure_Ada);
               Write_Indent;
               Write_Rewrite_Str ("{");
               Write_Id (Defining_Identifier (Node));
               Write_Str (" : ");
               Write_Str_With_Col_Check ("label");
               Write_Rewrite_Str ("}");
            end if;

         when N_In =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" in ");
            Sprint_Node (Right_Opnd (Node));

         when N_Incomplete_Type_Declaration =>
            Write_Indent_Str ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Sprint_Paren_Comma_List
                 (Discriminant_Specifications (Node));
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_With_Col_Check ("(<>)");
            end if;

            Write_Char (';');

         when N_Index_Or_Discriminant_Constraint =>
            Sprint_Paren_Comma_List (Constraints (Node));

         when N_Indexed_Component =>
            Sprint_Node (Prefix (Node));
            Sprint_Opt_Paren_Comma_List (Expressions (Node));

         when N_Integer_Literal =>
            if Print_In_Hex (Node) then
               Write_Uint_With_Col_Check (Intval (Node), Hex);
            else
               Write_Uint_With_Col_Check (Intval (Node), Auto);
            end if;

         when N_Interpretation =>
            pragma Assert (not Pure_Ada);
            Write_Indent_Str ("interpretation ");
            Sprint_Node (Etype (Node));

            if Present (Entity (Node)) then
               Write_Str (", ");
               Sprint_Node (Entity (Node));
            end if;

         when N_Iteration_Scheme =>
            if Present (Condition (Node)) then
               Write_Str_With_Col_Check ("while ");
               Sprint_Node (Condition (Node));
            else
               Write_Str_With_Col_Check ("for ");
               Sprint_Node (Loop_Parameter_Specification (Node));
            end if;

            Write_Char (' ');

         when N_Itype_Reference =>
            Write_Indent_Str ("reference ");
            Write_Id (Itype (Node));

         when N_Label =>
            Write_Indent_Str ("<<");
            Write_Id (Identifier (Node));
            Write_Str (">>");

         when N_Loop_Parameter_Specification =>
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" in ");

            if Reverse_Present (Node) then
               Write_Str_With_Col_Check ("reverse ");
            end if;

            Sprint_Node (Discrete_Subtype_Definition (Node));

         when N_Loop_Statement =>
            Write_Indent;

            if Present (Identifier (Node))
              and then (not Has_Created_Identifier (Node)
                          or else not Dump_Original_Only)
            then
               Write_Rewrite_Str ("{");
               Write_Id (Identifier (Node));
               Write_Str (" : ");
               Write_Rewrite_Str ("}");
               Sprint_Node (Iteration_Scheme (Node));
               Write_Str_With_Col_Check ("loop");
               Sprint_Indented_List (Statements (Node));
               Write_Indent_Str ("end loop ");
               Write_Rewrite_Str ("{");
               Write_Id (Identifier (Node));
               Write_Rewrite_Str ("}");
               Write_Char (';');

            else
               Sprint_Node (Iteration_Scheme (Node));
               Write_Str_With_Col_Check ("loop");
               Sprint_Indented_List (Statements (Node));
               Write_Indent_Str ("end loop;");
            end if;

         when N_Modular_Type_Definition =>
            Write_Str_With_Col_Check ("mod ");
            Sprint_Node (Expression (Node));

         when N_Not_In =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" not in ");
            Sprint_Node (Right_Opnd (Node));

         when N_Null =>
            Write_Str_With_Col_Check ("null");

         when N_Null_Statement =>
            Write_Indent_Str ("null;");

         when N_Number_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_Str_With_Col_Check (" : constant ");
               Write_Str (" := ");
               Sprint_Node (Expression (Node));
               Write_Char (';');
            end if;

         when N_Object_Declaration =>
            if Write_Indent_Identifiers (Node) then
               Write_Str (" : ");

               if Aliased_Present (Node) then
                  Write_Str_With_Col_Check ("aliased ");
               end if;

               if Constant_Present (Node) then
                  Write_Str_With_Col_Check ("constant ");
               end if;

               Sprint_Node (Object_Definition (Node));

               if Present (Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Expression (Node));
               end if;

               Write_Char (';');
            end if;

         when N_Object_Renaming_Declaration =>
            Write_Indent;
            Sprint_Node (Defining_Identifier (Node));
            Write_Str (" : ");
            Sprint_Node (Subtype_Mark (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Op_Abs =>
            Write_Str ("abs ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Add =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" + ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_And =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" and ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Concat =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" & ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Divide =>
            Sprint_Node (Left_Opnd (Node));
            Write_Char (' ');
            Process_TFAI_RR_Flags (Node);
            Write_Str ("/ ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Eq =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" = ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Expon =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" ** ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Ge =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" >= ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Gt =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" > ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Le =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" <= ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Lt =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" < ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Minus =>
            Write_Str ("-");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Mod =>
            Sprint_Node (Left_Opnd (Node));

            if Treat_Fixed_As_Integer (Node) then
               pragma Assert (not Pure_Ada);
               Write_Str (" #mod ");
            else
               Write_Str (" mod ");
            end if;

            Sprint_Node (Right_Opnd (Node));

         when N_Op_Multiply =>
            Sprint_Node (Left_Opnd (Node));
            Write_Char (' ');
            Process_TFAI_RR_Flags (Node);
            Write_Str ("* ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Ne =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" /= ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Not =>
            Write_Str ("not ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Or =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" or ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Plus =>
            Write_Str ("+");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Rem =>
            Sprint_Node (Left_Opnd (Node));

            if Treat_Fixed_As_Integer (Node) then
               pragma Assert (not Pure_Ada);
               Write_Str (" #rem ");
            else
               Write_Str (" rem ");
            end if;

            Sprint_Node (Right_Opnd (Node));

         when N_Op_Shift =>
            pragma Assert (not Pure_Ada);
            Write_Id (Node);
            Write_Char ('!');
            Write_Str_With_Col_Check ("(");
            Sprint_Node (Left_Opnd (Node));
            Write_Str (", ");
            Sprint_Node (Right_Opnd (Node));
            Write_Char (')');

         when N_Op_Subtract =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" - ");
            Sprint_Node (Right_Opnd (Node));

         when N_Op_Xor =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" xor ");
            Sprint_Node (Right_Opnd (Node));

         when N_Operator_Symbol =>
            if Pure_Ada then
               Write_Operator_Symbol_With_Col_Check (Chars (Node));
            else
               Write_Name_With_Col_Check (Chars (Node));
            end if;

         when N_Ordinary_Fixed_Point_Definition =>
            Write_Str_With_Col_Check ("delta ");
            Sprint_Node (Delta_Expression (Node));
            Sprint_Opt_Node (Real_Range_Specification (Node));

         when N_Or_Else =>
            Sprint_Node (Left_Opnd (Node));
            Write_Str (" or else ");
            Sprint_Node (Right_Opnd (Node));

         when N_Others_Choice =>
            Write_Str_With_Col_Check ("others");

         when N_Package_Body =>
            Write_Indent;
            Write_Indent_Str ("package body ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str (" is");
            Sprint_Indented_List (Declarations (Node));

            if Present (Handled_Statement_Sequence (Node)) then
               Write_Indent_Str ("begin");
               Sprint_Node (Handled_Statement_Sequence (Node));
            end if;

            Write_Indent_Str ("end ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Char (';');

         when N_Package_Body_Stub =>
            Write_Indent_Str ("package_body ");
            Sprint_Node (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" is separate;");

         when N_Package_Declaration =>
            Write_Indent;
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Package_Instantiation =>
            Write_Indent;
            Write_Indent_Str ("package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str (" is new ");
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Generic_Associations (Node));
            Write_Char (';');

         when N_Package_Renaming_Declaration =>
            Write_Indent_Str ("package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Package_Specification =>
            Write_Str_With_Col_Check ("package ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str (" is");
            Sprint_Indented_List (Visible_Declarations (Node));

            if Present (Private_Declarations (Node)) then
               Write_Indent_Str ("private");
               Sprint_Indented_List (Private_Declarations (Node));
            end if;

            Write_Indent_Str ("end ");
            Sprint_Node (Defining_Unit_Name (Node));

         when N_Parameter_Association =>
            Sprint_Node (Selector_Name (Node));
            Write_Str (" => ");
            Sprint_Node (Explicit_Actual_Parameter (Node));

         when N_Parameter_Specification =>
            if Write_Identifiers (Node) then
               Write_Str (" : ");

               if In_Present (Node) then
                  Write_Str_With_Col_Check ("in ");
               end if;

               if Out_Present (Node) then
                  Write_Str_With_Col_Check ("out ");
               end if;

               Sprint_Node (Parameter_Type (Node));

               if Present (Expression (Node)) then
                  Write_Str (" := ");
                  Sprint_Node (Expression (Node));
               end if;
            end if;

         when N_Pragma =>
            Write_Indent_Str ("pragma ");
            Write_Name_With_Col_Check (Chars (Node));

            if Present (Pragma_Argument_Associations (Node)) then
               Sprint_Opt_Paren_Comma_List
                 (Pragma_Argument_Associations (Node));
            end if;

            Write_Char (';');

         when N_Pragma_Argument_Association =>
            if Chars (Node) /= No_Name then
               Write_Name_With_Col_Check (Chars (Node));
               Write_Str (" => ");
            end if;

            Sprint_Node (Expression (Node));

         when N_Private_Type_Declaration =>
            Write_Indent_Str ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Sprint_Paren_Comma_List
                 (Discriminant_Specifications (Node));
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_With_Col_Check ("(<>)");
            end if;

            Write_Str (" is ");

            if Tagged_Present (Node) then
               Write_Str_With_Col_Check ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_Str_With_Col_Check ("limited ");
            end if;

            Write_Str_With_Col_Check ("private;");

         when N_Private_Extension_Declaration =>
            Write_Indent_Str ("type ");
            Write_Id (Defining_Identifier (Node));

            if Present (Discriminant_Specifications (Node)) then
               Sprint_Paren_Comma_List
                 (Discriminant_Specifications (Node));
            elsif Unknown_Discriminants_Present (Node) then
               Write_Str_With_Col_Check ("(<>)");
            end if;

            Write_Str_With_Col_Check (" is new ");
            Sprint_Node (Subtype_Indication (Node));
            Write_Str_With_Col_Check (" with private;");

         when N_Procedure_Call_Statement =>
            Write_Indent;
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Parameter_Associations (Node));
            Write_Char (';');

         when N_Procedure_Instantiation =>
            Write_Indent_Str ("procedure ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Str_With_Col_Check (" is new ");
            Sprint_Node (Name (Node));
            Sprint_Opt_Paren_Comma_List (Generic_Associations (Node));
            Write_Char (';');

         when N_Procedure_Specification =>
            Write_Str_With_Col_Check ("procedure ");
            Sprint_Node (Defining_Unit_Name (Node));
            Write_Param_Specs (Node);

         when N_Protected_Body =>
            Write_Indent_Str ("protected body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_Str ("end ");
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Protected_Body_Stub =>
            Write_Indent_Str ("protected body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" is separate;");

         when N_Protected_Definition =>
            Sprint_Indented_List (Visible_Declarations (Node));

            if Present (Private_Declarations (Node)) then
               Write_Indent_Str ("private");
               Sprint_Indented_List (Private_Declarations (Node));
            end if;

            Write_Indent_Str ("end ");

         when N_Protected_Type_Declaration =>
            Write_Indent_Str ("protected type ");
            Write_Id (Defining_Identifier (Node));
            Sprint_Opt_Paren_Comma_List
              (Discriminant_Specifications (Node));
            Write_Str (" is");
            Sprint_Node (Protected_Definition (Node));
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Qualified_Expression =>
            Sprint_Node (Subtype_Mark (Node));
            Write_Char (''');
            Sprint_Node (Expression (Node));

         when N_Raise_Constraint_Error =>
            pragma Assert (not Pure_Ada);

            --  This node can be used either as a subexpression or as a
            --  statement form. The following test is a reasonably reliable
            --  way to distinguish the two cases.

            if Is_List_Member (Node)
              and then Nkind (Parent (Node)) not in N_Subexpr
            then
               Write_Indent;
            end if;

            Write_Str_With_Col_Check ("[constraint_error");

            if Present (Condition (Node)) then
               Write_Str_With_Col_Check (" when ");
               Sprint_Node (Condition (Node));
            end if;

            Write_Char (']');

         when N_Raise_Statement =>
            Write_Indent_Str ("raise ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Range =>
            Sprint_Node (Low_Bound (Node));
            Write_Str (" .. ");
            Sprint_Node (High_Bound (Node));

         when N_Range_Constraint =>
            Write_Str_With_Col_Check ("range ");
            Sprint_Node (Range_Expression (Node));

         when N_Real_Literal =>
            Write_Ureal_With_Col_Check (Realval (Node));

         when N_Real_Range_Specification =>
            Write_Str_With_Col_Check ("range ");
            Sprint_Node (Low_Bound (Node));
            Write_Str (" .. ");
            Sprint_Node (High_Bound (Node));

         when N_Record_Definition =>
            if Abstract_Present (Node) then
               Write_Str_With_Col_Check ("abstract ");
            end if;

            if Tagged_Present (Node) then
               Write_Str_With_Col_Check ("tagged ");
            end if;

            if Limited_Present (Node) then
               Write_Str_With_Col_Check ("limited ");
            end if;

            if Null_Present (Node) then
               Write_Str_With_Col_Check ("null record");

            else
               Write_Str_With_Col_Check ("record");
               Sprint_Node (Component_List (Node));
               Write_Indent_Str ("end record");
            end if;

         when N_Record_Representation_Clause =>
            Write_Indent_Str ("for ");
            Sprint_Node (Identifier (Node));
            Write_Str_With_Col_Check (" use record ");

            if Present (Mod_Clause (Node)) then
               Sprint_Node (Mod_Clause (Node));
            end if;

            Sprint_Indented_List (Component_Clauses (Node));
            Write_Indent_Str ("end record;");

         when N_Reference =>
            pragma Assert (not Pure_Ada);
            Sprint_Node (Prefix (Node));
            Write_Str_With_Col_Check ("'reference");

         when N_Requeue_Statement =>
            Write_Indent_Str ("requeue ");
            Sprint_Node (Name (Node));

            if Abort_Present (Node) then
               Write_Str_With_Col_Check (" with abort");
            end if;

            Write_Char (';');

         when N_Return_Statement =>
            if Present (Expression (Node)) then
               Write_Indent_Str ("return ");
               Sprint_Node (Expression (Node));
               Write_Char (';');
            else
               Write_Indent_Str ("return;");
            end if;

         when N_Selective_Accept =>
            Write_Indent_Str ("select");

            declare
               Alt_Node : Node_Id;

            begin
               Alt_Node := First (Select_Alternatives (Node));
               loop
                  Indent_Begin;
                  Sprint_Node (Alt_Node);
                  Indent_End;
                  Alt_Node := Next (Alt_Node);
                  exit when No (Alt_Node);
                  Write_Indent_Str ("or");
               end loop;
            end;

            if Present (Else_Statements (Node)) then
               Write_Indent_Str ("else");
               Sprint_Indented_List (Else_Statements (Node));
            end if;

            Write_Indent_Str ("end select;");

         when N_Signed_Integer_Type_Definition =>
            Write_Str_With_Col_Check ("range ");
            Sprint_Node (Low_Bound (Node));
            Write_Str (" .. ");
            Sprint_Node (High_Bound (Node));

         when N_Single_Protected_Declaration =>
            Write_Indent_Str ("protected ");
            Write_Id (Defining_Identifier (Node));
            Write_Str (" is");
            Sprint_Node (Protected_Definition (Node));
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Single_Task_Declaration =>
            Write_Indent_Str ("task ");
            Write_Id (Defining_Identifier (Node));

            if Present (Task_Definition (Node)) then
               Write_Str (" is");
               Sprint_Node (Task_Definition (Node));
               Write_Id (Defining_Identifier (Node));
            end if;

            Write_Char (';');

         when N_Selected_Component | N_Expanded_Name =>
            Sprint_Node (Prefix (Node));
            Write_Char ('.');
            Sprint_Node (Selector_Name (Node));

         when N_Slice =>
            Sprint_Node (Prefix (Node));
            Write_Str_With_Col_Check (" (");
            Sprint_Node (Discrete_Range (Node));
            Write_Char (')');

         when N_String_Literal =>
            if String_Length (Strval (Node)) + Column > 75 then
               Write_Indent_Str ("  ");
            end if;

            Write_String_Table_Entry (Strval (Node));

         when N_Subprogram_Body =>
            if Freeze_Indent = 0 then
               Write_Indent;
            end if;

            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Str (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_Str ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_Str ("end ");
            Sprint_Node
              (Defining_Unit_Name (Specification (Node)));
            Write_Char (';');

            if Is_List_Member (Node)
              and then Present (Next (Node))
              and then Nkind (Next (Node)) /= N_Subprogram_Body
            then
               Write_Indent;
            end if;

         when N_Subprogram_Body_Stub =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Str_With_Col_Check (" is separate;");

         when N_Subprogram_Declaration =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Char (';');

         when N_Subprogram_Renaming_Declaration =>
            Write_Indent;
            Sprint_Node (Specification (Node));
            Write_Str_With_Col_Check (" renames ");
            Sprint_Node (Name (Node));
            Write_Char (';');

         when N_Subtype_Declaration =>
            Write_Indent_Str ("subtype ");
            Write_Id (Defining_Identifier (Node));
            Write_Str (" is ");
            Sprint_Node (Subtype_Indication (Node));
            Write_Char (';');

         when N_Subtype_Indication =>
            Sprint_Node (Subtype_Mark (Node));
            Write_Char (' ');
            Sprint_Node (Constraint (Node));

         when N_Subunit =>
            Write_Indent_Str ("separate (");
            Sprint_Node (Name (Node));
            Write_Char (')');
            Write_Eol;
            Sprint_Node (Proper_Body (Node));

         when N_Task_Body =>
            Write_Indent_Str ("task body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str (" is");
            Sprint_Indented_List (Declarations (Node));
            Write_Indent_Str ("begin");
            Sprint_Node (Handled_Statement_Sequence (Node));
            Write_Indent_Str ("end ");
            Write_Id (Defining_Identifier (Node));
            Write_Char (';');

         when N_Task_Body_Stub =>
            Write_Indent_Str ("task body ");
            Write_Id (Defining_Identifier (Node));
            Write_Str_With_Col_Check (" is separate;");

         when N_Task_Definition =>
            Sprint_Indented_List (Visible_Declarations (Node));

            if Present (Private_Declarations (Node)) then
               Write_Indent_Str ("private");
               Sprint_Indented_List (Private_Declarations (Node));
            end if;

            Write_Indent_Str ("end ");

         when N_Task_Type_Declaration =>
            Write_Indent_Str ("task type ");
            Write_Id (Defining_Identifier (Node));
            Sprint_Opt_Paren_Comma_List
              (Discriminant_Specifications (Node));
            if Present (Task_Definition (Node)) then
               Write_Str (" is");
               Sprint_Node (Task_Definition (Node));
               Write_Id (Defining_Identifier (Node));
            end if;

            Write_Char (';');

         when N_Terminate_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));

            Write_Indent;

            if Present (Condition (Node)) then
               Write_Str_With_Col_Check ("when ");
               Sprint_Node (Condition (Node));
               Write_Str (" => ");
            end if;

            Write_Str_With_Col_Check ("terminate;");
            Sprint_Node_List (Pragmas_After (Node));

         when N_Timed_Entry_Call =>
            Write_Indent_Str ("select");
            Indent_Begin;
            Sprint_Node (Entry_Call_Alternative (Node));
            Indent_End;
            Write_Indent_Str ("or");
            Indent_Begin;
            Sprint_Node (Delay_Alternative (Node));
            Indent_End;
            Write_Indent_Str ("end select;");

         when N_Triggering_Alternative =>
            Sprint_Node_List (Pragmas_Before (Node));
            Sprint_Node (Triggering_Statement (Node));
            Sprint_Node_List (Statements (Node));

         when N_Type_Conversion =>
            Sprint_Node (Subtype_Mark (Node));
            Col_Check (4);

            if Conversion_OK (Node) then
               pragma Assert (not Pure_Ada);
               Write_Char ('?');
            end if;

            if Float_Truncate (Node) then
               pragma Assert (not Pure_Ada);
               Write_Char ('^');
            end if;

            if Rounded_Result (Node) then
               pragma Assert (not Pure_Ada);
               Write_Char ('@');
            end if;

            Write_Char ('(');
            Sprint_Node (Expression (Node));
            Write_Char (')');

         when N_Unchecked_Expression =>
            Col_Check (10);
            Write_Str ("`(");
            Sprint_Node (Expression (Node));
            Write_Char (')');

         when N_Unchecked_Type_Conversion =>
            pragma Assert (not Pure_Ada);
            Sprint_Node (Subtype_Mark (Node));
            Write_Char ('!');
            Write_Str_With_Col_Check ("(");
            Sprint_Node (Expression (Node));
            Write_Char (')');

         when N_Unconstrained_Array_Definition =>
            Write_Str_With_Col_Check ("array (");

            declare
               Node1 : Node_Id := First (Subtype_Marks (Node));
            begin
               loop
                  Sprint_Node (Node1);
                  Write_Str_With_Col_Check (" range <>");
                  Node1 := Next (Node1);
                  exit when Node1 = Empty;
                  Write_Str (", ");
               end loop;
            end;

            Write_Str (") of ");

            if Aliased_Present (Node) then
               Write_Str_With_Col_Check ("aliased ");
            end if;

            Sprint_Node (Subtype_Indication (Node));

         when N_Unused_At_Start | N_Unused_At_End =>
            Write_Indent_Str ("***** Error, unused node encountered *****");
            Write_Eol;

         when N_Use_Package_Clause =>
            Write_Indent_Str ("use ");
            Sprint_Comma_List (Names (Node));
            Write_Char (';');

         when N_Use_Type_Clause =>
            Write_Indent_Str ("use type ");
            Sprint_Comma_List (Subtype_Marks (Node));
            Write_Char (';');

         when N_Validate_Unchecked_Conversion =>
            Write_Indent_Str ("validate unchecked_conversion (");
            Sprint_Node (Source_Type (Node));
            Write_Str (", ");
            Sprint_Node (Target_Type (Node));
            Write_Str (");");

         when N_Variant =>
            Write_Indent_Str ("when ");
            Sprint_Bar_List (Discrete_Choices (Node));
            Write_Str (" => ");
            Sprint_Node (Component_List (Node));

         when N_Variant_Part =>
            Indent_Begin;
            Write_Indent_Str ("case ");
            Sprint_Node (Name (Node));
            Write_Str (" is ");
            Sprint_Indented_List (Variants (Node));
            Write_Indent_Str ("end case");
            Indent_End;

         when N_With_Clause =>

            --  Special test, if we are dumping the original tree only,
            --  then we want to eliminate the bogus with clauses that
            --  correspond to the non-existent children of Text_IO.

            if Dump_Original_Only
              and then Is_Text_IO_Kludge_Unit (Name (Node))
            then
               null;

            --  Normal case, output the with clause

            else
               if First_Name (Node) or else not Dump_Original_Only then
                  Write_Indent_Str ("with ");
               else
                  Write_Str (", ");
               end if;

               Sprint_Node (Name (Node));

               if Last_Name (Node) or else not Dump_Original_Only then
                  Write_Char (';');
               end if;
            end if;

      end case;

      for J in 1 .. Paren_Count (Node) loop
         Write_Char (')');
      end loop;

   end Sprint_Node_Actual;

   ----------------------
   -- Sprint_Node_List --
   ----------------------

   procedure Sprint_Node_List (List : List_Id) is
      Node : Node_Id;

   begin
      if Is_Non_Empty_List (List) then
         Node := First (List);

         loop
            Sprint_Node (Node);
            Node := Next (Node);
            exit when Node = Empty;
         end loop;
      end if;
   end Sprint_Node_List;

   --------------------------
   -- Sprint_Node_Pure_Ada --
   --------------------------

   procedure Sprint_Node_Pure_Ada (Node : Node_Id) is
      Saved_Dump_Original_Only : constant Boolean := Dump_Original_Only;

   begin
      Pure_Ada := True;
      Dump_Original_Only := True;
      Sprint_Node (Node);
      Dump_Original_Only := Saved_Dump_Original_Only;
      Pure_Ada := False;
   end Sprint_Node_Pure_Ada;

   ---------------------
   -- Sprint_Opt_Node --
   ---------------------

   procedure Sprint_Opt_Node (Node : Node_Id) is
   begin
      if Present (Node) then
         Write_Char (' ');
         Sprint_Node (Node);
      end if;
   end Sprint_Opt_Node;

   --------------------------
   -- Sprint_Opt_Node_List --
   --------------------------

   procedure Sprint_Opt_Node_List (List : List_Id) is
   begin
      if Present (List) then
         Sprint_Node_List (List);
      end if;
   end Sprint_Opt_Node_List;

   ---------------------------------
   -- Sprint_Opt_Paren_Comma_List --
   ---------------------------------

   procedure Sprint_Opt_Paren_Comma_List (List : List_Id) is
   begin
      if Is_Non_Empty_List (List) then
         Write_Char (' ');
         Sprint_Paren_Comma_List (List);
      end if;
   end Sprint_Opt_Paren_Comma_List;

   -----------------------------
   -- Sprint_Paren_Comma_List --
   -----------------------------

   procedure Sprint_Paren_Comma_List (List : List_Id) is
      N           : Node_Id;
      Node_Exists : Boolean := False;

   begin

      if Is_Non_Empty_List (List) then

         if Dump_Original_Only then
            N := First (List);

            while Present (N) loop

               if not Is_Rewrite_Insertion (N) then
                  Node_Exists := True;
                  exit;
               end if;

               N := Next (N);
            end loop;

            if not Node_Exists then
               return;
            end if;
         end if;

         Write_Str_With_Col_Check ("(");
         Sprint_Comma_List (List);
         Write_Char (')');
      end if;
   end Sprint_Paren_Comma_List;

   -----------------
   -- Write_Ekind --
   -----------------

   procedure Write_Ekind (E : Entity_Id) is
      S : constant String := Entity_Kind'Image (Ekind (E));

   begin
      Name_Len := S'Length;
      Name_Buffer (1 .. Name_Len) := S;
      Set_Casing (Mixed_Case);
      Write_Str_With_Col_Check (Name_Buffer (1 .. Name_Len));
   end Write_Ekind;

   --------------
   -- Write_Id --
   --------------

   procedure Write_Id (N : Node_Id) is
   begin
      Write_Name_With_Col_Check (Chars (N));
   end Write_Id;

   -----------------------
   -- Write_Identifiers --
   -----------------------

   function Write_Identifiers (Node : Node_Id) return Boolean is
   begin
      Sprint_Node (Defining_Identifier (Node));

      --  The remainder of the declaration must be printed unless we are
      --  printing the original tree and this is not the last identifier

      return
         not Dump_Original_Only or else not More_Ids (Node);

   end Write_Identifiers;

   ------------------------
   -- Write_Implicit_Def --
   ------------------------

   procedure Write_Implicit_Def (E : Entity_Id) is
      Ind : Node_Id;

   begin
      case Ekind (E) is
         when E_Array_Subtype =>
            Write_Str_With_Col_Check ("subtype ");
            Write_Id (E);
            Write_Str_With_Col_Check (" is ");
            Write_Id (Base_Type (E));
            Write_Str_With_Col_Check (" (");

            Ind := First_Index (E);

            while Present (Ind) loop
               Sprint_Node (Ind);
               Ind := Next_Index (Ind);

               if Present (Ind) then
                  Write_Str (", ");
               end if;
            end loop;

            Write_Str (");");

         when E_Signed_Integer_Subtype | E_Enumeration_Subtype =>
            Write_Str_With_Col_Check ("subtype ");
            Write_Id (E);
            Write_Str (" is ");
            Write_Id (Etype (E));
            Write_Str_With_Col_Check (" range ");
            Sprint_Node (Scalar_Range (E));
            Write_Str (";");

         when others =>
            Write_Str_With_Col_Check ("type ");
            Write_Id (E);
            Write_Str_With_Col_Check (" is <");
            Write_Ekind (E);
            Write_Str (">;");
      end case;

   end Write_Implicit_Def;

   ------------------
   -- Write_Indent --
   ------------------

   procedure Write_Indent is
   begin
      if Indent_Annull_Flag then
         Indent_Annull_Flag := False;
      else
         Write_Eol;
         for I in 1 .. Indent loop
            Write_Char (' ');
         end loop;
      end if;
   end Write_Indent;

   ------------------------------
   -- Write_Indent_Identifiers --
   ------------------------------

   function Write_Indent_Identifiers (Node : Node_Id)
     return Boolean is
   begin
      --  We need to start a new line for every node, except in the case
      --  where we are printing the original tree and this is not the first
      --  defining identifier in the list.

      if not Dump_Original_Only or else not Prev_Ids (Node) then
         Write_Indent;

      --  If printing original tree and this is not the first defining
      --  identifier in the list, then the previous call to this procedure
      --  printed only the name, and we add a comma to separate the names.

      else
         Write_Str (", ");
      end if;

      Sprint_Node (Defining_Identifier (Node));

      --  The remainder of the declaration must be printed unless we are
      --  printing the original tree and this is not the last identifier

      return
         not Dump_Original_Only or else not More_Ids (Node);

   end Write_Indent_Identifiers;

   ----------------------
   -- Write_Indent_Str --
   ----------------------

   procedure Write_Indent_Str (S : String) is
   begin
      Write_Indent;
      Write_Str (S);
   end Write_Indent_Str;

   -------------------------------
   -- Write_Name_With_Col_Check --
   -------------------------------

   procedure Write_Name_With_Col_Check (N : Name_Id) is
   begin
      Get_Name_String (N);
      Write_Str_With_Col_Check (Name_Buffer (1 .. Name_Len));
   end Write_Name_With_Col_Check;

   ------------------------------------------
   -- Write_Operator_Symbol_With_Col_Check --
   ------------------------------------------

   procedure Write_Operator_Symbol_With_Col_Check (N : Name_Id) is
   begin
      case N is
         when Name_Op_Abs =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := 'a';
            Name_Buffer (3) := 'b';
            Name_Buffer (4) := 's';
            Name_Buffer (5) := '"';
            Name_Len := 5;

         when Name_Op_And =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := 'a';
            Name_Buffer (3) := 'n';
            Name_Buffer (4) := 'd';
            Name_Buffer (5) := '"';
            Name_Len := 5;

         when Name_Op_Mod =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := 'm';
            Name_Buffer (3) := 'o';
            Name_Buffer (4) := 'd';
            Name_Buffer (5) := '"';
            Name_Len := 5;

         when Name_Op_Not =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := 'n';
            Name_Buffer (3) := 'o';
            Name_Buffer (4) := 't';
            Name_Buffer (5) := '"';
            Name_Len := 5;

         when Name_Op_Or =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := 'o';
            Name_Buffer (3) := 'r';
            Name_Buffer (4) := '"';
            Name_Len := 4;

         when Name_Op_Rem =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := 'r';
            Name_Buffer (3) := 'e';
            Name_Buffer (4) := 'm';
            Name_Buffer (5) := '"';
            Name_Len := 5;

         when Name_Op_Xor =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := 'x';
            Name_Buffer (3) := 'o';
            Name_Buffer (4) := 'r';
            Name_Buffer (5) := '"';
            Name_Len := 5;

         when Name_Op_Eq =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '=';
            Name_Buffer (3) := '"';
            Name_Len := 3;

         when Name_Op_Ne =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '/';
            Name_Buffer (3) := '=';
            Name_Buffer (4) := '"';
            Name_Len := 4;

         when Name_Op_Lt =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '<';
            Name_Buffer (3) := '"';
            Name_Len := 3;

         when Name_Op_Le =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '<';
            Name_Buffer (3) := '=';
            Name_Buffer (4) := '"';
            Name_Len := 4;

         when Name_Op_Gt =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '>';
            Name_Buffer (3) := '"';
            Name_Len := 3;

         when Name_Op_Ge =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '>';
            Name_Buffer (3) := '=';
            Name_Buffer (4) := '"';
            Name_Len := 4;

         when Name_Op_Add =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '+';
            Name_Buffer (3) := '"';
            Name_Len := 3;

         when Name_Op_Subtract =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '-';
            Name_Buffer (3) := '"';
            Name_Len := 3;

         when Name_Op_Concat =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '&';
            Name_Buffer (3) := '"';
            Name_Len := 3;

         when Name_Op_Multiply =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '*';
            Name_Buffer (3) := '"';
            Name_Len := 3;

         when Name_Op_Divide =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '/';
            Name_Buffer (3) := '"';
            Name_Len := 3;

         when Name_Op_Expon =>
            Name_Buffer (1) := '"';
            Name_Buffer (2) := '*';
            Name_Buffer (3) := '*';
            Name_Buffer (4) := '"';
            Name_Len := 4;

         when others =>
            Get_Name_String (N);
      end case;

      Write_Str_With_Col_Check (Name_Buffer (1 .. Name_Len));
   end Write_Operator_Symbol_With_Col_Check;

   -----------------------
   -- Write_Param_Specs --
   -----------------------

   procedure Write_Param_Specs (N : Node_Id) is
      Specs  : List_Id;
      Spec   : Node_Id;
      Formal : Node_Id;

   begin
      Specs := Parameter_Specifications (N);

      if Present (Specs) then
         Write_Str_With_Col_Check (" (");
         Spec := First (Specs);

         loop
            Sprint_Node (Spec);
            Formal := Defining_Identifier (Spec);
            Spec := Next (Spec);
            exit when Spec = Empty;

            --  Add semicolon, unless we are printing original tree and the
            --  next specification is part of a list (but not the first
            --  element of that list)

            if not Dump_Original_Only or else not Prev_Ids (Spec) then
               Write_Str ("; ");
            end if;
         end loop;

         --  Write out any extra formals

         while Present (Extra_Formal (Formal)) loop
            Formal := Extra_Formal (Formal);
            Write_Str ("; ");
            Write_Name_With_Col_Check (Chars (Formal));
            Write_Str (" : ");
            Write_Name_With_Col_Check (Chars (Etype (Formal)));
         end loop;

         Write_Char (')');
      end if;
   end Write_Param_Specs;

   --------------------------
   -- Write_Rewrite_Str --
   --------------------------

   procedure Write_Rewrite_Str (S : String) is
   begin
      if not Dump_Generated_Only then
         if S'Length = 1 and then S (1) = '}' then
            Write_Char ('}');
         else
            Write_Str_With_Col_Check (S);
         end if;
      end if;
   end Write_Rewrite_Str;

   ------------------------------
   -- Write_Str_With_Col_Check --
   ------------------------------

   procedure Write_Str_With_Col_Check (S : String) is
   begin
      if Int (S'Last) + Column > Line_Limit then
         Write_Indent_Str ("  ");

         if S (1) = ' ' then
            Write_Str (S (2 .. S'Length));
         else
            Write_Str (S);
         end if;

      else
         Write_Str (S);
      end if;
   end Write_Str_With_Col_Check;

   -------------------------------
   -- Write_Uint_With_Col_Check --
   -------------------------------

   procedure Write_Uint_With_Col_Check (U : Uint; Format : UI_Format) is
   begin
      Col_Check (UI_Decimal_Digits_Hi (U));
      UI_Write (U, Format);
   end Write_Uint_With_Col_Check;

   --------------------------------
   -- Write_Ureal_With_Col_Check --
   --------------------------------

   procedure Write_Ureal_With_Col_Check (U : Ureal) is
      D : constant Uint := Denominator (U);
      N : constant Uint := Numerator (U);

   begin
      Col_Check
        (UI_Decimal_Digits_Hi (D) + UI_Decimal_Digits_Hi (N) + 4);
      UR_Write (U);
   end Write_Ureal_With_Col_Check;

end Sprint;
