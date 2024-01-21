------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . C H 1 0                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.85 $                             --
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

with Fname; use Fname;
with Uname; use Uname;

separate (Par)
package body Ch10 is

   --  Local functions, used only in this chapter

   function P_Context_Clause    return List_Id;
   function P_Subunit           return Node_Id;

   function Set_Location return Source_Ptr;
   --  The current compilation unit starts with Token at Token_Ptr. This
   --  function determines the corresponding source location for the start
   --  of the unit, including any preceding comment lines.

   procedure Unit_Display (Cunit : Node_Id; Loc : Source_Ptr);
   --  This procedure is used to generate a line of output for the a unit in
   --  the source program. Cunit is the node for the compilation unit, and
   --  Loc is the source location for the start of the unit in the source
   --  file (which is not necessarily the Sloc of the Cunit node). This
   --  output is written to the standard output file for use by gnatchop.

   -------------------------
   -- 10.1.1  Compilation --
   -------------------------

   --  COMPILATION ::= {COMPILATION_UNIT}

   --  There is no specific parsing routine for a compilation, since we only
   --  permit a single compilation in a source file, so there is no explicit
   --  occurrence of compilations as such (our representation of a compilation
   --  is a series of separate source files).

   ------------------------------
   -- 10.1.1  Compilation unit --
   ------------------------------

   --  COMPILATION_UNIT ::=
   --    CONTEXT_CLAUSE LIBRARY_ITEM
   --  | CONTEXT_CLAUSE SUBUNIT

   --  LIBRARY_ITEM ::=
   --    private LIBRARY_UNIT_DECLARATION
   --  | LIBRARY_UNIT_BODY
   --  | [private] LIBRARY_UNIT_RENAMING_DECLARATION

   --  LIBRARY_UNIT_DECLARATION ::=
   --    SUBPROGRAM_DECLARATION | PACKAGE_DECLARATION
   --  | GENERIC_DECLARATION    | GENERIC_INSTANTIATION

   --  LIBRARY_UNIT_RENAMING_DECLARATION ::=
   --    PACKAGE_RENAMING_DECLARATION
   --  | GENERIC_RENAMING_DECLARATION
   --  | SUBPROGRAM_RENAMING_DECLARATION

   --  LIBRARY_UNIT_BODY ::= SUBPROGRAM_BODY | PACKAGE_BODY

   --  Error recovery: cannot raise Error_Resync. If an error occurs, tokens
   --  are skipped up to the next possible beginning of a compilation unit.

   --  Note: if only configuration pragmas are found, Empty is returned

   --  Note: in syntax-only mode, it is possible for P_Compilation_Unit
   --  to return strange things that are not really compilation units.
   --  This is done to help out gnatchop when it is faced with nonsense.

   function P_Compilation_Unit return Node_Id is
      Scan_State         : Saved_Scan_State;
      Body_Node          : Node_Id;
      Specification_Node : Node_Id;
      Unit_Node          : Node_Id;
      Comp_Unit_Node     : Node_Id;
      Name_Node          : Node_Id;
      Item               : Node_Id;
      Private_Sloc       : Source_Ptr := No_Location;
      Config_Pragmas     : List_Id;

      Cunit_Error_Flag   : Boolean := False;
      --  This flag is set True if we have to scan for a compilation unit
      --  token. It is used to ensure clean termination in such cases by
      --  not insisting on being at the end of file, and, in the sytax only
      --  case by not scanning for additional compilation units.

      Cunit_Location : Source_Ptr;
      --  Location of unit for unit identification output (List_Unit option)

   begin
      Num_Library_Units := Num_Library_Units + 1;

      --  Set location of the compilation unit if unit list option set
      --  and we are in syntax check only mode

      if List_Units and then Operating_Mode = Check_Syntax then
         Cunit_Location := Set_Location;
      end if;

      --  Scan out any configuration pragmas

      Config_Pragmas := No_List;

      if Num_Library_Units = 1 then
         while Token = Tok_Pragma loop
            Save_Scan_State (Scan_State);
            Item := P_Pragma;

            if Item = Error
              or else Chars (Item) > Last_Configuration_Pragma_Name
            then
               Restore_Scan_State (Scan_State);
               exit;
            end if;

            if Config_Pragmas = No_List then
               Config_Pragmas := Empty_List;

               if Operating_Mode = Check_Syntax and then List_Units then
                  Write_Str
                    ("Config pragmas line 1, file offset 0, " &
                     "file name gnat.adc");
                  Write_Eol;
               end if;
            end if;

            Append (Item, Config_Pragmas);
            Cunit_Location := Set_Location;
         end loop;
      end if;

      --  Establish compilation unit node and scan context items

      Comp_Unit_Node := New_Node (N_Compilation_Unit, No_Location);
      Set_Cunit (Current_Source_Unit, Comp_Unit_Node);
      Set_Context_Items (Comp_Unit_Node, P_Context_Clause);

      if Present (Config_Pragmas) then

         --  Check for case of only configuration pragmas present

         if Token = Tok_EOF
           and then Is_Empty_List (Context_Items (Comp_Unit_Node))
         then
            if Operating_Mode = Check_Syntax then
               return Empty;

            else
               Item := First (Config_Pragmas);
               Error_Msg_N
                 ("cannot compile configuration pragmas with gcc", Item);
               Error_Msg_N
                 ("use gnatchop to process configuration pragmas!", Item);
               raise Unrecoverable_Error;
            end if;

         --  Otherwise configuration pragmas are simply prepended to the
         --  context of the current unit.

         else
            Append_List (Context_Items (Comp_Unit_Node), Config_Pragmas);
            Set_Context_Items (Comp_Unit_Node, Config_Pragmas);
         end if;
      end if;

      --  Check for PRIVATE. Note that for the moment we allow this in
      --  Ada_83 mode, since we do not yet know if we are compiling a
      --  predefined unit, and if we are then it would be allowed anyway.

      if Token = Tok_Private then
         Private_Sloc := Token_Ptr;
         Set_Keyword_Casing (Current_Source_File, Determine_Token_Casing);
         if Style_Check then Style.Check_Indentation; end if;

         Save_Scan_State (Scan_State); -- at PRIVATE
         Note_Feature (Private_Child, Token_Ptr);
         Scan; -- past PRIVATE

         if Token = Tok_Separate then
            Error_Msg_SP ("cannot have private subunits!");

         elsif Token = Tok_Package then
            Scan; -- past PACKAGE

            if Token = Tok_Body then
               Restore_Scan_State (Scan_State); -- to PRIVATE
               Error_Msg_SC ("cannot have private package body!");
               Scan; -- ignore PRIVATE

            else
               Restore_Scan_State (Scan_State); -- to PRIVATE
               Scan; -- past PRIVATE
               Set_Private_Present (Comp_Unit_Node, True);
            end if;

         elsif Token = Tok_Procedure
           or else Token = Tok_Function
           or else Token = Tok_Generic
         then
            Set_Private_Present (Comp_Unit_Node, True);
         end if;
      end if;

      --  Loop to find our way to a compilation unit token

      loop
         exit when Token in Token_Class_Cunit and then Token /= Tok_With;

         exit when Bad_Spelling_Of (Tok_Package)
           or else Bad_Spelling_Of (Tok_Function)
           or else Bad_Spelling_Of (Tok_Generic)
           or else Bad_Spelling_Of (Tok_Separate)
           or else Bad_Spelling_Of (Tok_Procedure);

         --  Allow task and protected for nice error recovery purposes

         exit when Token = Tok_Task
           or else Token = Tok_Protected;

         if Token = Tok_With then
            Error_Msg_SC ("misplaced WITH");
            Append_List (P_Context_Clause, Context_Items (Comp_Unit_Node));

         elsif Bad_Spelling_Of (Tok_With) then
            Append_List (P_Context_Clause, Context_Items (Comp_Unit_Node));

         else
            Error_Msg_SC ("compilation unit expected");
            Cunit_Error_Flag := True;
            Resync_Cunit;

            --  If we are at an end of file, then just quit, the above error
            --  message was complaint enough.

            if Token = Tok_EOF then
               return Error;
            end if;
         end if;
      end loop;

      --  We have a compilation unit token, so that's a reasonable choice for
      --  determining the standard casing convention used for keywords in case
      --  it hasn't already been done on seeing a WITH or PRIVATE.

      Set_Keyword_Casing (Current_Source_File, Determine_Token_Casing);
      if Style_Check then Style.Check_Indentation; end if;

      --  Remaining processing depends on particular type of compilation unit

      if Token = Tok_Package then

         --  A common error is to omit the body keyword after package. We can
         --  often diagnose this early on (before getting loads of errors from
         --  contained subprogram bodies), by knowing that that the file we
         --  are compiling has a name that requires a body to be found.

         --  However, we do not do this check if we are operating in syntax
         --  checking only mode, because in that case there may be multiple
         --  units in the same file, and the file name is not a reliable guide.

         Save_Scan_State (Scan_State);
         Scan; -- past Package keyword

         if Token /= Tok_Body
           and then Operating_Mode /= Check_Syntax
           and then
             Get_Expected_Unit_Type
               (File_Name (Current_Source_File)) = Expect_Body
         then
            Error_Msg_BC ("keyword BODY expected here [see file name]");
            Restore_Scan_State (Scan_State);
            Set_Unit (Comp_Unit_Node, P_Package (Pf_Pbod));
         else
            Restore_Scan_State (Scan_State);
            Set_Unit (Comp_Unit_Node, P_Package (Pf_Decl_Gins_Pbod_Rnam));
         end if;

      elsif Token = Tok_Generic then
         Set_Unit (Comp_Unit_Node, P_Generic);

      elsif Token = Tok_Separate then
         Set_Unit (Comp_Unit_Node, P_Subunit);

      elsif Token = Tok_Procedure
        or else Token = Tok_Function
      then
         Set_Unit (Comp_Unit_Node, P_Subprogram (Pf_Decl_Gins_Pbod_Rnam));

         --  A little bit of an error recovery check here. If we just scanned
         --  a subprogram declaration (as indicated by an SIS entry being
         --  active), then if the following token is BEGIN or an identifier,
         --  or a token which can reasonably start a declaration but cannot
         --  start a compilation unit, then we assume that the semicolon in
         --  the declaration should have been IS.

         if SIS_Entry_Active then

            if Token = Tok_Begin
               or else Token = Tok_Identifier
               or else Token in Token_Class_Deckn
            then
               Push_Scope_Stack;
               Scope.Table (Scope.Last).Etyp := E_Name;
               Scope.Table (Scope.Last).Sloc := SIS_Sloc;
               Scope.Table (Scope.Last).Ecol := SIS_Ecol;
               Scope.Table (Scope.Last).Lreq := False;
               SIS_Entry_Active := False;

               --  If we had a missing semicolon in the declaration, then
               --  change the message to from <missing ";"> to <missing "is">

               if SIS_Missing_Semicolon_Message /= No_Error_Msg then
                  Change_Error_Text     -- Replace: "missing "";"" "
                    (SIS_Missing_Semicolon_Message, "missing IS");

               --  Otherwise we saved the semicolon position, so complain

               else
                  Error_Msg (""";"" should be IS", SIS_Semicolon_Sloc);
               end if;

               Body_Node := Unit (Comp_Unit_Node);
               Specification_Node := Specification (Body_Node);
               Change_Node (Body_Node, N_Subprogram_Body);
               Set_Specification (Body_Node, Specification_Node);
               Parse_Decls_Begin_End (Body_Node);
               Set_Unit (Comp_Unit_Node, Body_Node);
            end if;

         --  If we scanned a subprogram body, make sure we did not have private

         elsif Private_Sloc /= No_Location
           and then Nkind (Unit (Comp_Unit_Node)) /= N_Function_Instantiation
           and then Nkind (Unit (Comp_Unit_Node)) /= N_Procedure_Instantiation
         then
            Error_Msg ("cannot have private subprogram body", Private_Sloc);

         --  P_Subprogram can yield an abstract subprogram, but this cannot
         --  be a compilation unit. Treat as a subprogram declaration.

         elsif
           Nkind (Unit (Comp_Unit_Node)) = N_Abstract_Subprogram_Declaration
         then
            Error_Msg_N
              ("compilation unit cannot be abstract subprogram",
                 Unit (Comp_Unit_Node));

            Unit_Node :=
              New_Node (N_Subprogram_Declaration, Sloc (Comp_Unit_Node));
            Set_Specification (Unit_Node,
              Specification (Unit (Comp_Unit_Node)));
            Set_Unit (Comp_Unit_Node, Unit_Node);
         end if;

      --  Otherwise we have TASK. This is not really an acceptable token,
      --  but we accept it to improve error recovery.

      elsif Token = Tok_Task then
         Scan; -- Past TASK

         if Token = Tok_Type then
            Error_Msg_SP
              ("task type cannot be used as compilation unit");
         else
            Error_Msg_SP
              ("task declaration cannot be used as compilation unit");
         end if;

         --  If in check syntax mode, accept the task anyway. This is done
         --  particularly to improve the behavior of GNATCHOP in this case.

         if Operating_Mode = Check_Syntax then
            Set_Unit (Comp_Unit_Node, P_Task);

         --  If not in syntax only mode, treat this as horrible error

         else
            Cunit_Error_Flag := True;
            return Error;
         end if;

      elsif Token = Tok_Protected then
         Scan; -- Past PROTECTED

         if Token = Tok_Type then
            Error_Msg_SP
              ("protected type cannot be used as compilation unit");
         else
            Error_Msg_SP
              ("protected declaration cannot be used as compilation unit");
         end if;

         --  If in check syntax mode, accept protected anyway. This is done
         --  particularly to improve the behavior of GNATCHOP in this case.

         if Operating_Mode = Check_Syntax then
            Set_Unit (Comp_Unit_Node, P_Protected);

         --  If not in syntax only mode, treat this as horrible error

         else
            Cunit_Error_Flag := True;
            return Error;
         end if;

      --  Anything else is an internal error!

      else
         null;
         pragma Assert (False);
      end if;

      --  Here is where locate the compilation unit entity. This is a little
      --  tricky, since it is buried in various places.

      Unit_Node := Unit (Comp_Unit_Node);

      --  Only try this if we got an OK unit!

      if Unit_Node /= Error then
         if Nkind (Unit_Node) = N_Subunit then
            Unit_Node := Proper_Body (Unit_Node);
         end if;

         if Nkind (Unit_Node) in N_Generic_Declaration then
            Unit_Node := Specification (Unit_Node);
         end if;

         if Nkind (Unit_Node) = N_Package_Declaration
           or else Nkind (Unit_Node) = N_Subprogram_Declaration
           or else Nkind (Unit_Node) = N_Subprogram_Body
           or else Nkind (Unit_Node) = N_Subprogram_Renaming_Declaration
         then
            Unit_Node := Specification (Unit_Node);

         elsif Nkind (Unit_Node) = N_Subprogram_Renaming_Declaration then
            Note_Feature (Library_Unit_Renaming, Sloc (Unit_Node));

            if Ada_83 then
               Error_Msg_N
                 ("(Ada 83) library unit renaming not allowed", Unit_Node);
            end if;
         end if;

         if Nkind (Unit_Node) = N_Task_Body
           or else Nkind (Unit_Node) = N_Protected_Body
           or else Nkind (Unit_Node) = N_Task_Type_Declaration
           or else Nkind (Unit_Node) = N_Protected_Type_Declaration
           or else Nkind (Unit_Node) = N_Single_Task_Declaration
           or else Nkind (Unit_Node) = N_Single_Protected_Declaration
         then
            Name_Node := Defining_Identifier (Unit_Node);
         else
            Name_Node := Defining_Unit_Name (Unit_Node);
         end if;

         Set_Sloc (Comp_Unit_Node, Sloc (Name_Node));

         --  Set Entity field in file table. Easier now that we have name!
         --  Note that this is also skipped if we had a bad unit

         if Nkind (Name_Node) = N_Defining_Program_Unit_Name then
            Set_Cunit_Entity
              (Current_Source_Unit, Defining_Identifier (Name_Node));
         else
            Set_Cunit_Entity (Current_Source_Unit, Name_Node);
         end if;

         Set_Unit_Name
           (Current_Source_Unit, Get_Unit_Name (Unit (Comp_Unit_Node)));

      --  If we had a bad unit, make sure the fatal flag is set in the file
      --  table entry, since this is surely a fatal error and also set our
      --  flag to inhibit the requirement that we be at end of file.

      else
         Cunit_Error_Flag := True;
         Set_Fatal_Error (Current_Source_Unit);
      end if;

      --  Clear away any missing semicolon indication, we are done with that
      --  unit, so what's done is done, and we don't want anything hanging
      --  around from the attempt to parse it!

      SIS_Entry_Active := False;

      --  Scan out pragmas after unit

      Set_Pragmas_After (Comp_Unit_Node, P_Pragmas_Opt);

      --  Cancel effect of any outstanding pragma Warnings (Off)

      Set_Warnings_Mode_On (Scan_Ptr);

      --  Ada 83 error checks

      if Ada_83 then

         --  Check we did not with any child units

         Item := First (Context_Items (Comp_Unit_Node));

         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
              and then Nkind (Name (Item)) /= N_Identifier
            then
               Error_Msg_N ("(Ada 83) child units not allowed", Item);
            end if;

            Item := Next (Item);
         end loop;

         --  Check that we did not have a PRIVATE keyword present

         if Private_Present (Comp_Unit_Node) then
            Error_Msg
              ("(Ada 83) private units not allowed", Private_Sloc);
         end if;
      end if;

      --  If no serious error, then output possible unit information line
      --  for gnatchop if we are in syntax only, list units mode.

      if not Cunit_Error_Flag
        and then List_Units
        and then Operating_Mode = Check_Syntax
      then
         Unit_Display (Comp_Unit_Node, Cunit_Location);
      end if;

      --  And now we should be at the end of file, except that if we had to
      --  scan for a compilation unit, then we don't check this, since it
      --  seems in practice to often make things worse, and we already gave
      --  a serious error message.

      if Token /= Tok_EOF and then not Cunit_Error_Flag then

         --  If we are not at end of file, then fatal error unless we are
         --  syntax checking only mode, where we do allow additional units
         --  by making a recursive call to this routine. Skip this message
         --  if we already had some fatal error.

         if Operating_Mode = Check_Syntax then
            return P_Compilation_Unit;

         else
            if not Fatal_Error (Current_Source_Unit) then

               if Token in Token_Class_Cunit then
                  Error_Msg_SC
                    ("end of file expected, " &
                     "file can have only one compilation unit");

               else
                  Error_Msg_SC ("end of file expected");
               end if;
            end if;

            return Error;
         end if;

      --  This is the normal return

      else
         return Comp_Unit_Node;
      end if;

   exception

      --  An error resync is a serious bomb, so indicate result unit no good

      when Error_Resync =>
         Set_Fatal_Error (Current_Source_Unit);
         return Error;

   end P_Compilation_Unit;

   --------------------------
   -- 10.1.1  Library Item --
   --------------------------

   --  Parsed by P_Compilation_Unit (10.1.1)

   --------------------------------------
   -- 10.1.1  Library Unit Declaration --
   --------------------------------------

   --  Parsed by P_Compilation_Unit (10.1.1)

   ------------------------------------------------
   -- 10.1.1  Library Unit Renaming Declaration  --
   ------------------------------------------------

   --  Parsed by P_Compilation_Unit (10.1.1)

   -------------------------------
   -- 10.1.1  Library Unit Body --
   -------------------------------

   --  Parsed by P_Compilation_Unit (10.1.1)

   ------------------------------
   -- 10.1.1  Parent Unit Name --
   ------------------------------

   --  Parsed (as a name) by its parent construct

   ----------------------------
   -- 10.1.2  Context Clause --
   ----------------------------

   --  CONTEXT_CLAUSE ::= {CONTEXT_ITEM}

   --  CONTEXT_ITEM ::= WITH_CLAUSE | USE_CLAUSE

   --  WITH_CLAUSE ::=
   --    with library_unit_NAME {,library_unit_NAME};

   --  Error recovery: Cannot raise Error_Resync

   function P_Context_Clause return List_Id is
      Item_List  : List_Id;
      With_Node  : Node_Id;
      First_Flag : Boolean;

   begin
      Item_List := New_List;

      --  Get keyword casing from WITH keyword in case not set yet

      if Token = Tok_With then
         Set_Keyword_Casing (Current_Source_File, Determine_Token_Casing);
      end if;

      --  Loop through context items

      loop
         if Style_Check then Style.Check_Indentation; end if;

         --  Gather any pragmas appearing in the context clause

         P_Pragmas_Opt (Item_List);

         --  Processing for WITH clause

         if Token = Tok_With then
            Scan; -- past WITH
            First_Flag := True;

            --  Loop through names in one with clause, generating a separate
            --  N_With_Clause node for each nam encountered.

            loop
               With_Node := New_Node (N_With_Clause, Token_Ptr);
               Append (With_Node, Item_List);

               --  Note that we allow with'ing of child units, even in Ada 83
               --  mode, since presumably if this is not desired, then the
               --  compilation of the child unit itself is the place where
               --  such an "error" should be caught.

               Set_Name (With_Node, P_Qualified_Simple_Name);
               Set_First_Name (With_Node, First_Flag);
               First_Flag := False;
               exit when Token /= Tok_Comma;
               Scan; -- past comma
            end loop;

            Set_Last_Name (With_Node, True);
            TF_Semicolon;


         --  Processing for USE clause

         elsif Token = Tok_Use then
            Append (P_Use_Clause, Item_List);

         --  Anything else is end of context clause

         else
            exit;
         end if;
      end loop;

      return Item_List;
   end P_Context_Clause;

   --------------------------
   -- 10.1.2  Context Item --
   --------------------------

   --  Parsed by P_Context_Clause (10.1.2)

   -------------------------
   -- 10.1.2  With Clause --
   -------------------------

   --  Parsed by P_Context_Clause (10.1.2)

   -----------------------
   -- 10.1.3  Body Stub --
   -----------------------

   --  Subprogram stub parsed by P_Subprogram (6.1)
   --  Package stub parsed by P_Package (7.1)
   --  Task stub parsed by P_Task (9.1)
   --  Protected stub parsed by P_Protected (9.4)

   ----------------------------------
   -- 10.1.3  Subprogram Body Stub --
   ----------------------------------

   --  Parsed by P_Subprogram (6.1)

   -------------------------------
   -- 10.1.3  Package Body Stub --
   -------------------------------

   --  Parsed by P_Package (7.1)

   ----------------------------
   -- 10.1.3  Task Body Stub --
   ----------------------------

   --  Parsed by P_Task (9.1)

   ---------------------------------
   -- 10.1.3  Protected Body Stub --
   ---------------------------------

   --  Parsed by P_Protected (9.4)

   ---------------------
   -- 10.1.3  Subunit --
   ---------------------

   --  SUBUNIT ::= separate (PARENT_UNIT_NAME) PROPER_BODY

   --  PARENT_UNIT_NAME ::= NAME

   --  The caller has checked that the initial token is SEPARATE

   --  Error recovery: cannot raise Error_Resync

   function P_Subunit return Node_Id is
      Subunit_Node : Node_Id;
      Body_Node    : Node_Id;

   begin
      Subunit_Node := New_Node (N_Subunit, Token_Ptr);
      Body_Node := Error; -- in case no good body found
      Scan; -- past SEPARATE;

      T_Left_Paren;
      Set_Name (Subunit_Node, P_Qualified_Simple_Name);
      T_Right_Paren;

      if Token = Tok_Semicolon then
         Error_Msg_SC ("unexpected semicolon ignored");
         Scan;
      end if;

      if Token = Tok_Function or else Token = Tok_Procedure then
         Body_Node := P_Subprogram (Pf_Pbod);

      elsif Token = Tok_Package then
         Body_Node := P_Package (Pf_Pbod);

      elsif Token = Tok_Protected then
         Scan; -- past PROTECTED

         if Token = Tok_Body then
            Body_Node := P_Protected;
         else
            Error_Msg_AP ("BODY expected");
            return Error;
         end if;

      elsif Token = Tok_Task then
         Scan; -- past TASK

         if Token = Tok_Body then
            Body_Node := P_Task;
         else
            Error_Msg_AP ("BODY expected");
            return Error;
         end if;

      else
         Error_Msg_SC ("proper body expected");
         return Error;
      end if;

      Set_Proper_Body  (Subunit_Node, Body_Node);
      return Subunit_Node;

   end P_Subunit;

   ------------------
   -- Set_Location --
   ------------------

   function Set_Location return Source_Ptr is
      Physical : Boolean;
      Loc      : Source_Ptr;

   begin
      if Prev_Token = No_Token then
         return Source_First (Current_Source_File);

      else
         Loc := Prev_Token_Ptr;
         loop
            exit when Loc = Token_Ptr;

            if Source (Loc) in Line_Terminator then
               Skip_Line_Terminators (Loc, Physical);
               exit when Physical;
            end if;

            Loc := Loc + 1;
         end loop;

         return Loc;
      end if;
   end Set_Location;

   ------------------
   -- Unit_Display --
   ------------------

   procedure Unit_Display (Cunit : Node_Id; Loc : Source_Ptr) is
      Unum : constant Unit_Number_Type    := Get_Cunit_Unit_Number (Cunit);
      Sind : constant Source_File_Index   := Source_Index (Unum);
      Line : constant Logical_Line_Number := Get_Line_Number (Loc);
      Unam : constant Unit_Name_Type      := Unit_Name (Unum);

   begin
      if List_Units then
         Write_Str ("Unit ");
         Write_Unit_Name (Unit_Name (Unum));
         Write_Str (" line ");
         Write_Int (Int (Line));

         Write_Str (", file offset ");
         Write_Int (Int (Loc) - Int (Source_Text (Sind)'First));

         Write_Str (", file name ");
         Write_Name (Get_File_Name (Unam));
         Write_Eol;
      end if;
   end Unit_Display;

end Ch10;
