------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 0                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.255 $                            --
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
with Einfo;    use Einfo;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Dist; use Exp_Dist;
with Fname;    use Fname;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Lib.Writ; use Lib.Writ;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Dist; use Sem_Dist;
with Sem_Prag; use Sem_Prag;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Uname;    use Uname;

package body Sem_Ch10 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Context (N : Node_Id);
   --  Analyzes items in the context clause of compilation unit

   procedure Check_Private_Child_Unit (N : Node_Id);
   --  If a with_clause mentions a private child unit, the compilation
   --  unit must be a member of the same family, as described in 10.1.2 (8).

   procedure Check_Stub_Level (N : Node_Id);
   --  Verify that a stub is declared immediately within a compilation unit,
   --  and not in  an inner frame.

   procedure Implicit_With_On_Parent (Child_Unit : Node_Id; N : Node_Id);
   --  When a child unit appears in a context clause,  the implicit with on
   --  parents is made explicit, and with clauses are inserted in the context
   --  clause after the one for the child. In addition, if the main unit is a
   --  child unit, implicit withs are also added for all its ancestors. N is
   --  the compilation unit whose list of context items receives the implicit
   --  with clauses.

   procedure Install_Context (N : Node_Id);
   --  Installs the entities from the context clause of the given compilation
   --  unit into the visibility chains. This is done before analyzing a unit.
   --  For a child unit, install context of parents as well.

   procedure Install_Context_Clauses (N : Node_Id);
   --  Subsidiary to previous one. Process only with_ and use_clauses for
   --  current unit and its library unit if any.

   procedure Install_Withed_Unit (With_Clause : Node_Id);
   --  If the unit is not a child unit, make unit immediately visible.
   --  The caller ensures that the unit is not already currently installed.

   procedure Install_Parents (Lib_Unit : Node_Id; Is_Private : Boolean);
   --  This procedure establishes the context for the compilation of a child
   --  unit. If Lib_Unit is a child library spec then the context of the parent
   --  is installed, and the parent itself made immediately visible, so that
   --  the child unit is processed in the declarative region of the parent.
   --  Install_Parents makes a recursive call to itself to ensure that all
   --  parents are loaded in the nested case. If Lib_Unit is a library body,
   --  the only effect of Install_Parents is to install the private decls of
   --  the parents, because the visible parent declarations will have been
   --  installed as part of the context of the corresponding spec.

   procedure Install_Siblings (U_Name : Entity_Id; N : Node_Id);
   --  In the compilation of a child unit, a child of any of the  ancestor
   --  units is directly visible if it is visible, because the parent is in
   --  an enclosing scope. Iterate over context to find child units of U_Name
   --  or of some ancestor of it.

   function Is_Child_Spec (Lib_Unit : Node_Id) return Boolean;
   --  Lib_Unit is a library unit which may be a spec or a body. Is_Child_Spec
   --  returns True if Lib_Unit is a library spec which is a child spec, i.e.
   --  a library spec that has a parent. If the call to Is_Child_Spec returns
   --  True, then Parent_Spec (Lib_Unit) is non-Empty and points to the
   --  compilation unit for the parent spec.
   --  Lib_Unit can also be a subprogram body that acts as its own spec. If
   --  the Parent_Spec is  non-empty, this is also a child unit.

   procedure Remove_Context (N : Node_Id);
   --  Removes the entities from the context clause of the given compilation
   --  unit from the visibility chains. This is done on exit from a unit as
   --  part of cleaning up the visibility chains for the caller. A special
   --  case is that the call from the Main_Unit can be ignored, since at the
   --  end of the main unit the visibility table won't be needed in any case.
   --  For a child unit, remote parents and their context as well.

   procedure Remove_Context_Clauses (N : Node_Id);
   --  Subsidiary of previous one. Remove use_ and with_clauses.

   procedure Remove_Parents (Lib_Unit : Node_Id);
   --  Remove_Parents checks if Lib_Unit is a child spec. If so then the parent
   --  contexts established by the corresponding call to Install_Parents are
   --  removed. Remove_Parents contains a recursive call to itself to ensure
   --  that all parents are removed in the nested case.

   procedure Remove_Withed_Unit (Unit_Name : Entity_Id);
   --  This procedure makes the given unit not visible.

   procedure Analyze_Proper_Body (N : Node_Id);
   --  Common processing for subprogram stubs and package stubs. Once the
   --  subunit name is established, load and analyze.

   ------------------------------
   -- Analyze_Compilation_Unit --
   ------------------------------

   procedure Analyze_Compilation_Unit (N : Node_Id) is
      The_Unit      : constant Node_Id := Unit (N);
      Lib_Unit      : Node_Id := Library_Unit (N);
      Spec_Id       : Node_Id;
      Main_Cunit    : constant Node_Id := Cunit (Main_Unit);
      Par_Spec_Name : Unit_Name_Type;
      Unum          : Unit_Number_Type;

   begin
      Process_Compilation_Unit_Pragmas (N);

      --  If we are compiling in some distribution mode, establish
      --  global flags to control generation of internal types and
      --  stub files.

      if N = Library_Unit (Main_Cunit)
         and then (Distribution_Stub_Mode = Compile_Caller_Stub_Spec
                     or else
                   Distribution_Stub_Mode = Compile_Receiver_Stub_Spec)
      then
         Init_Names;
      end if;

      --  If the unit is a subunit whose parent has not been analyzed (which
      --  indicates that the main unit is a subunit, either the current one or
      --  one of its descendents) then the subunit is compiled as part of the
      --  analysis of the parent, which we proceed to do. Basically this gets
      --  handled from the top down and we don't want to do anything at this
      --  level (i.e. this subunit will be handled on the way down from the
      --  parent), so at this level we immediately return. If the subunit
      --  ends up not analyzed, it means that the parent did not contain a
      --  stub for it.

      if Nkind (The_Unit) = N_Subunit
        and then not Analyzed (Lib_Unit)
      then
         Semantics (Lib_Unit);

         if not Analyzed (Proper_Body (The_Unit)) then
            Error_Msg_N ("missing stub for subunit", N);
         end if;

         return;
      end if;

      --  Analyze context (this will call Sem recursively for with'ed units)

      Analyze_Context (N);

      --  If the unit is a package body, the spec is already loaded and must
      --  be analyzed first, before we analyze the body.

      if Nkind (The_Unit) = N_Package_Body then

         Semantics (Lib_Unit);

         --  Verify that the library unit is a package declaration.

         if Nkind (Unit (Lib_Unit)) /= N_Package_Declaration
              and then
            Nkind (Unit (Lib_Unit)) /= N_Generic_Package_Declaration
         then
            Error_Msg_N ("no legal package declaration for package body", N);
            return;

         --  Otherwise, the entity in the declaration is visible. Update
         --  the version to reflect dependence of this body on the spec.

         else
            Spec_Id := Defining_Entity (Unit (Lib_Unit));
            Set_Is_Immediately_Visible (Spec_Id, True);
            Version_Update (N, Lib_Unit);
         end if;

      --  If the unit is a subprogram body, then we similarly need to analyze
      --  its spec. However, things are a little simpler in this case, because
      --  here, this analysis is done only for error checking and consistency
      --  purposes, so there's nothing else to be done.

      elsif Nkind (The_Unit) = N_Subprogram_Body then
         if Acts_As_Spec (N) then

            --  If the subprogram body is a child unit, we must create a
            --  declaration for it, in order to properly load the parent(s).
            --  After this, the original unit does not acts as a spec, because
            --  there is an explicit one. If this  unit appears in a context
            --  clause, then an implicit with on the parent will be added when
            --  installing the context. If this is the main unit, there is no
            --  Unit_Table entry for the declaration, (It has the unit number
            --  of the main unit) and code generation is unaffected.

            Unum := Get_Cunit_Unit_Number (N);
            Par_Spec_Name := Get_Parent_Spec_Name (Unit_Name (Unum));

            if Par_Spec_Name /= No_Name then
               Unum := Load_Unit (Par_Spec_Name, True, N);

               if Unum /= No_Unit then

                  --  Build subprogram declaration and attach parent unit to it

                  declare
                     Loc : constant Source_Ptr := Sloc (N);

                  begin
                     Lib_Unit :=
                       Make_Compilation_Unit (Loc,
                         Context_Items => Empty_List,
                         Unit =>
                           Make_Subprogram_Declaration (Sloc (N),
                             Specification =>
                               Copy_Separate_Tree (Specification (The_Unit))));

                     Set_Library_Unit (N, Lib_Unit);
                     Set_Parent_Spec (Unit (Lib_Unit), Cunit (Unum));
                     Semantics (Lib_Unit);
                     Set_Acts_As_Spec (N, False);
                  end;
               end if;
            end if;

         else
            Semantics (Lib_Unit);
            Version_Update (N, Lib_Unit);
         end if;

      --  If it is a subprogram declaration it does not need an elaboration
      --  procedure. A renamed package also needs no elaboration procedure.

      elsif Nkind (The_Unit) = N_Subprogram_Declaration
        or else Nkind (The_Unit) = N_Package_Renaming_Declaration
      then
         Set_Has_No_Elab_Code (N, True);
      end if;

      --  If it is a child unit, the parent must be elaborated first
      --  and we update version, since we are dependent on our parent.

      if Is_Child_Spec (The_Unit) then
         Semantics (Parent_Spec (The_Unit));
         Version_Update (N, Parent_Spec (The_Unit));
      end if;

      --  With the analysis done, install the context. Note that we can't
      --  install the context from the with clauses as we analyze them,
      --  because each with clause must be analyzed in a clean visibility
      --  context, so we have to wait and install them all at once.

      Install_Context (N);

      --  All components of the context: with-clauses, library unit, ancestors
      --  if any, (and their context)  are analyzed and installed. Now analyze
      --  the unit itself, which is either a package, subprogram spec or body.

      Analyze (The_Unit);

      --  Treat compilation unit pragmas that appear after the library unit

      if Present (Pragmas_After (N)) then
         declare
            Prag_Node : Node_Id := First (Pragmas_After (N));

         begin
            while Present (Prag_Node) loop
               Analyze (Prag_Node);
               Prag_Node := Next (Prag_Node);
            end loop;
         end;
      end if;

      --  Generate distribution stub files if requested and no error

      if N = Main_Cunit
        and then (Distribution_Stub_Mode = Generate_Receiver_Stub_Body
                    or else
                  Distribution_Stub_Mode = Generate_Caller_Stub_Body)
        and then not Fatal_Error (Main_Unit)
        and then (Is_RCI_Pkg_Spec_Or_Body (N)
                   or else (Nkind (The_Unit) = N_Package_Declaration
                     and then
                       Is_Shared_Passive
                         (Defining_Unit_Name (Specification (The_Unit)))))
      then
         Generate_Stubs_Files (N);
      end if;

      --  Last step is to deinstall the context we just installed
      --  as well as the unit just compiled.

      Remove_Context (N);

      if Nkind (The_Unit) = N_Package_Declaration
        or else Nkind (The_Unit) in N_Generic_Declaration
        or else Nkind (The_Unit) = N_Package_Renaming_Declaration
      then
         Remove_Withed_Unit (Defining_Entity (The_Unit));

      elsif Nkind (The_Unit) = N_Package_Body
        or else (Nkind (The_Unit) = N_Subprogram_Body
                  and then not Acts_As_Spec (N))
      then
         --  Bodies that are not the main unit are compiled if they
         --  are generic or contain generic or inlined units. Their
         --  analysis brings in the context of the corresponding spec
         --  (unit declaration) which must be removed as well, to
         --  return the compilation environment to its proper state.

         Remove_Context (Lib_Unit);
      end if;

      --  If this is the main unit and we are generating code, we must
      --  check that all generic units in the context have a body if they
      --  need it, even if they have not been instantiated. In  the absence
      --  of .ali files for generic units, we must force the load of the body,
      --  just to produce the proper error if the body is absent. We skip this
      --  verification if the main unit itself is generic.

      if Get_Cunit_Unit_Number (N) = Main_Unit
        and then Operating_Mode = Generate_Code
        and then Expander_Active
      then
         --  Indicate that the main unit is now analyzed, to catch possible
         --  circularities between it and generic bodies. Remove main unit
         --  from visibility. This might seem superfluous, but the main unit
         --  must not be visible in the generic body expansions that follow.

         Set_Analyzed (N, True);
         Set_Is_Immediately_Visible (Cunit_Entity (Main_Unit), False);

         declare
            Item : Node_Id;
            Nam  : Entity_Id;
            Un   : Unit_Number_Type;

         begin
            Item := First (Context_Items (N));

            while Present (Item) loop

               if Nkind (Item) = N_With_Clause
                  and then not Implicit_With (Item)
               then
                  Nam := Entity (Name (Item));

                  if (Ekind (Nam) = E_Generic_Procedure
                       and then not Is_Intrinsic_Subprogram (Nam))
                    or else (Ekind (Nam) = E_Generic_Function
                              and then not Is_Intrinsic_Subprogram (Nam))
                    or else (Ekind (Nam) = E_Generic_Package
                              and then Unit_Requires_Body (Nam))
                  then
                     if Present (Renamed_Object (Nam)) then
                        Un :=
                           Load_Unit
                             (Get_Body_Name (Get_Unit_Name
                               (Get_Declaration_Node
                                 (Renamed_Object (Nam)))), False, N);
                     else
                        Un :=
                          Load_Unit
                            (Get_Body_Name (Get_Unit_Name (Item)), False, N);
                     end if;

                     if Un = No_Unit then
                        Error_Msg_NE
                          ("body of generic unit& not found", Item, Nam);
                        exit;

                     elsif not Analyzed (Cunit (Un))
                       and then Un /= Main_Unit
                     then
                        Semantics (Cunit (Un));
                     end if;
                  end if;
               end if;

               Item := Next (Item);
            end loop;
         end;
      end if;

      --  Final step, for main unit, check that we are not semantically
      --  dependent on a unit forbidden by some restriction pragma.

      if Any_Restrictions and then Current_Sem_Unit = Main_Unit then
         for U in Main_Unit + 1 .. Last_Unit loop
            Check_Restricted_Unit (Unit_Name (U), Sloc (N));
         end loop;
      end if;

   end Analyze_Compilation_Unit;

   ----------------------------
   -- Analyze_Task_Body_Stub --
   ----------------------------

   procedure Analyze_Task_Body_Stub (N : Node_Id) is
      Nam : Entity_Id := Current_Entity_In_Scope (Defining_Identifier (N));
      Loc : constant Source_Ptr := Sloc (N);

   begin
      Check_Stub_Level (N);

      --  First occurence of name may have been as an incomplete type.

      if Present (Nam) and then Ekind (Nam) = E_Incomplete_Type then
         Nam := Full_View (Nam);
      end if;

      if No (Nam)
        or else not Is_Task_Type (Etype (Nam))
      then
         Error_Msg_N ("missing specification for task body", N);
      else
         Set_Has_Completion (Etype (Nam));
         Analyze_Proper_Body (N);

         --  Set elaboration flag to indicate that entity is callable.
         --  This cannot be done in the expansion of the body  itself,
         --  because the proper body is not in a declarative part. This
         --  is only done if expansion is active, because the context
         --  may be generic and the flag not defined yet.

         if Expander_Active then
            Insert_After (N,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Identifier (Loc,
                    New_External_Name (Chars (Etype (Nam)), 'E')),
                 Expression => New_Reference_To (Standard_True, Loc)));
         end if;

      end if;
   end Analyze_Task_Body_Stub;

   ---------------------
   -- Analyze_Context --
   ---------------------

   procedure Analyze_Context (N : Node_Id) is
      Item : Node_Id;

   begin
      --  Loop through context items

      Item := First (Context_Items (N));
      while Present (Item) loop

         --  For with clause, analyze the with clause, and then update
         --  the version, since we are dependent on a unit that we with.

         if Nkind (Item) = N_With_Clause then
            Analyze (Item);

            if not Implicit_With (Item) then
               Version_Update (N, Library_Unit (Item));
            end if;

         --  Analyze pragmas

         elsif Nkind (Item) = N_Pragma then
            Analyze (Item);

         --  But skip use clauses at this stage, since we don't want to do
         --  any installing of potentially use visible entities until we
         --  we actually install the complete context (in Install_Context).
         --  Otherwise things can get installed in the wrong context.

         else
            null;
         end if;

         Item := Next (Item);
      end loop;
   end Analyze_Context;

   -------------------------------
   -- Analyze_Package_Body_Stub --
   -------------------------------

   procedure Analyze_Package_Body_Stub (N : Node_Id) is
      Id   : constant Entity_Id := Defining_Identifier (N);
      Nam  : Entity_Id;

   begin
      --  The package declaration must be in the current declarative part.

      Check_Stub_Level (N);
      Nam := Current_Entity_In_Scope (Id);

      if No (Nam)
        or else
          (Ekind (Nam) /= E_Package and then Ekind (Nam) /= E_Generic_Package)
      then
         Error_Msg_N ("missing specification for package stub", N);

      else
         --  Indicate that the body of the package exists. If we are doing
         --  only semantic analysis, the stub stands for the body. If we are
         --  generating code, the existence of the body will be confirmed
         --  when we load the proper body.

         Set_Has_Completion (Nam);
         Analyze_Proper_Body (N);
      end if;
   end Analyze_Package_Body_Stub;

   -------------------------
   -- Analyze_Proper_Body --
   -------------------------

   --  If the subunit is already loaded, it means that the main unit
   --  is a subunit, and that the current unit is one of its parents
   --  which was being analyzed to provide the needed context for the
   --  analysis of the subunit. In this case we analyze the subunit and
   --  continue with the parent, without looking a subsequent subunits.

   procedure Analyze_Proper_Body (N : Node_Id) is
      Subunit_Name      : constant Unit_Name_Type := Get_Unit_Name (N);
      Unum              : Unit_Number_Type;
      Subunit_Not_Found : Boolean := False;

   begin
      if Is_Loaded (Subunit_Name) then

         --  If the proper body is already linked to the stub node,
         --  the stub is in a generic unit and just needs analyzing.

         if Present (Library_Unit (N)) then
            Set_Corresponding_Stub (Unit (Library_Unit (N)), N);
            Analyze_Subunit (Library_Unit (N));

         --  Otherwise we must load the subunit and link to it

         else
            --  Load the subunit, this must work, since we originally
            --  loaded the subunit earlier on. So this will not really
            --  load it, just give access to it.

            Unum := Load_Unit (Subunit_Name, True, N);

            --  And analyze the subunit in the parent context (note that we
            --  do not call Semantics, since that would remove the parent
            --  context). Because of this, we have to manually reset the
            --  compiler state to Analyzing since it got destroyed by Load.

            Compiler_State := Analyzing;
            Set_Corresponding_Stub (Unit (Cunit (Unum)), N);
            Analyze_Subunit (Cunit (Unum));
            Set_Library_Unit (N, Cunit (Unum));
         end if;

      --  If the main unit is a subunit, then we are just performing semantic
      --  analysis on that subunit, and any other subunits of any parent unit
      --  should be ignored, except that a stub may provide a declaration.

      elsif Nkind (Unit (Cunit (Main_Unit))) = N_Subunit
        and then Subunit_Name /= Unit_Name (Main_Unit)
        and then not Xref_Analyze
      then
         if Nkind (N) = N_Subprogram_Body_Stub then
            Analyze_Subprogram_Body (N);
         end if;

         return;

      --  If the subunit is not already loaded, and we are generating code,
      --  then this is the case where compilation started from the parent,
      --  and we are generating code for an entire subunit tree. In that
      --  case we definitely need to load the subunit.

      --  In order to continue the analysis with the rest of the parent,
      --  and other subunits, we load the unit without requiring its
      --  presence, and emit a warning if not found, rather than terminating
      --  the compilation abruptly, as for other missing file problems.

      elsif Operating_Mode = Generate_Code or else Xref_Analyze then

         --  If the proper body is already linked to the stub node,
         --  the stub is in a generic unit and just needs analyzing.

         --  We update the version. Although we are not technically
         --  semantically dependent on the subunit, given our approach
         --  of macro substitution of subunits, it makes sense to
         --  include it in the version identification.

         if Present (Library_Unit (N)) then
            Set_Corresponding_Stub (Unit (Library_Unit (N)), N);
            Analyze_Subunit (Library_Unit (N));
            Version_Update (Cunit (Main_Unit), Library_Unit (N));

         --  Otherwise we must load the subunit and link to it

         else
            Unum := Load_Unit (Subunit_Name, False, N);

            if Operating_Mode = Generate_Code
              and then Unum = No_Unit
            then
               Error_Msg_Name_1 := Subunit_Name;
               Error_Msg_Name_2 := Get_File_Name (Subunit_Name);
               Error_Msg_N
                 ("subunit% in file{ not found!?", N);
               Subunits_Missing := True;
               Subunit_Not_Found := True;

               if Nkind (N) = N_Subprogram_Body_Stub then
                  Analyze_Subprogram_Body (N);
                  return;
               end if;
            end if;

            --  Load_Unit may reset Compiler_State, since it may have been
            --  necessary to parse an additional units, so we make sure
            --  that we reset it to the Analyzing state.

            Compiler_State := Analyzing;

            if Unum /= No_Unit and then not Fatal_Error (Unum) then

               if Debug_Flag_L then
                  Write_Str ("*** Loaded subunit from stub. Analyze");
                  Write_Eol;
               end if;

               declare
                  Comp_Unit : constant Node_Id := Cunit (Unum);

               begin
                  --  Check for child unit instead of subunit

                  if Nkind (Unit (Comp_Unit)) /= N_Subunit then
                     Error_Msg_N
                       ("expected SEPARATE subunit, found child unit",
                        Cunit_Entity (Unum));
                     raise Unrecoverable_Error;
                  end if;

                  --  OK, we have a subunit, so go ahead and analyze it

                  Set_Corresponding_Stub (Unit (Comp_Unit), N);
                  Analyze_Subunit (Comp_Unit);
                  Set_Library_Unit (N, Comp_Unit);

                  --  We update the version. Although we are not technically
                  --  semantically dependent on the subunit, given our
                  --  approach of macro substitution of subunits, it makes
                  --  sense to include it in the version identification.

                  Version_Update (Cunit (Main_Unit), Comp_Unit);
               end;

            else
               --  If the subunit corresponding to the stub has not
               --  been found, then in an analyze called by gnatf, we
               --  avoid messages about missing bodies for procedures
               --  and functions. We similarly avoid these messages if
               --  the subunit file was not found.

               if Xref_Analyze or Subunit_Not_Found then

                  case Nkind (N) is

                     when N_Subprogram_Body_Stub =>
                        declare
                           Spec      : constant Node_Id := Specification (N);
                           Spec_Node : Entity_Id;
                           Subp      : Entity_Id;

                        begin
                           Subp := Analyze_Spec (Spec);
                           Spec_Node := Find_Corresponding_Spec (N);
                        end;

                     when others =>
                        null;
                  end case;

               end if;

            end if;
         end if;

         --  The remaining case is when the subunit is not already loaded and
         --  we are not generating code. In this case we are just performing
         --  semantic analysis on the parent, and we are not interested in
         --  the subunit. For subprograms, analyze the stub as a body. For
         --  other entities the stub has already been marked as completed.

      else
         if Nkind (N) = N_Subprogram_Body_Stub then
            Analyze_Subprogram_Body (N);
         end if;
      end if;

   end Analyze_Proper_Body;

   ----------------------------------
   -- Analyze_Protected_Body_Stub --
   ----------------------------------

   procedure Analyze_Protected_Body_Stub (N : Node_Id) is
      Nam : Entity_Id := Current_Entity_In_Scope (Defining_Identifier (N));
      Loc : constant Source_Ptr := Sloc (N);

   begin
      Check_Stub_Level (N);

      --  First occurence of name may have been as an incomplete type.

      if Present (Nam) and then Ekind (Nam) = E_Incomplete_Type then
         Nam := Full_View (Nam);
      end if;

      if No (Nam)
        or else not Is_Protected_Type (Etype (Nam))
      then
         Error_Msg_N ("missing specification for Protected body", N);
      else
         Set_Has_Completion (Etype (Nam));
         Analyze_Proper_Body (N);
      end if;
   end Analyze_Protected_Body_Stub;

   ----------------------------------
   -- Analyze_Subprogram_Body_Stub --
   ----------------------------------

   --  A subprogram body stub can appear with or without a previous
   --  specification. If there is one, the analysis of the body will
   --  find it and verify conformance.  The formals appearing in the
   --  specification of the stub play no role, except for requiring
   --  an additional conformance check. However, if we are performing
   --  semantic checks only, the stub must be analyzed like a body,
   --  because it may be the declaration of the subprogram.

   procedure Analyze_Subprogram_Body_Stub (N : Node_Id) is
      Decl : Node_Id;

   begin
      Check_Stub_Level (N);

      --  Verify that the identifier for the stub is unique within this
      --  declarative part.

      if Nkind (Parent (N)) = N_Block_Statement
        or else Nkind (Parent (N)) = N_Package_Body
        or else Nkind (Parent (N)) = N_Subprogram_Body
      then
         Decl := First (Declarations (Parent (N)));

         while Present (Decl)
           and then Decl /= N
         loop
            if Nkind (Decl) = N_Subprogram_Body_Stub
              and then (Chars (Defining_Unit_Name (Specification (Decl)))
                      = Chars (Defining_Unit_Name (Specification (N))))
            then
               Error_Msg_N ("identifier for stub is not unique", N);
            end if;

            Decl := Next (Decl);
         end loop;
      end if;

      if Errors_Detected > 0 then
         Analyze_Subprogram_Body (N);
      else
         Analyze_Proper_Body (N);
      end if;

   end Analyze_Subprogram_Body_Stub;

   ---------------------
   -- Analyze_Subunit --
   ---------------------

   --  A subunit is compiled either by itself (for semantic checking)
   --  or as part of compiling the parent (for code generation). In
   --  either case, by the time we actually process the subunit, the
   --  parent has already been installed and analyzed. The node N is
   --  a compilation unit, whose context needs to be treated here,
   --  because we come directly here from the parent without calling
   --  Analyze_Compilation_Unit.

   --  The compilation context includes the explicit context of the
   --  subunit, and the context of the parent, together with the parent
   --  itself. In order to compile the current context, we remove the
   --  one inherited from the parent, in order to have a clean visibility
   --  table. We restore the parent context before analyzing the proper
   --  body itself. On exit, we remove only the explicit context of the
   --  subunit.

   procedure Analyze_Subunit (N : Node_Id) is
      Lib_Unit   : constant Node_Id   := Library_Unit (N);
      Par_Unit   : constant Entity_Id := Current_Scope;

      Lib_Spec   : Node_Id := Library_Unit (Lib_Unit);
      Use_Clause : Node_Id;

      procedure Re_Install_Parents (L : Node_Id);
      --  Recursive procedure to restore scope of all ancestors of subunit,
      --  from outermost in. If parent is not a subunit, the call to install
      --  context installs context of spec and (if parent is a child unit)
      --  the context of its parents as well. It is confusing that parents
      --  should be treated differently in both cases, but the semantics are
      --  just not identical.

      procedure Re_Install_Use_Clauses;
      --  As part of the removal of the parent scope, the use clauses are
      --  removed, to be reinstalled when the context of the subunit has
      --  been analyzed.

      ------------------------
      -- Re_Install_Parents --
      ------------------------

      procedure Re_Install_Parents (L : Node_Id) is
      begin
         if Nkind (Unit (L)) = N_Subunit then
            Re_Install_Parents (Library_Unit (L));
         end if;

         Install_Context (L);
      end Re_Install_Parents;

      ----------------------------
      -- Re_Install_Use_Clauses --
      ----------------------------

      procedure Re_Install_Use_Clauses is
         U : Node_Id;

      begin
         while Present (Use_Clause) loop
            U := Use_Clause;
            Use_Clause := Next_Use_Clause (U);

            if Nkind (U) = N_Use_Package_Clause then
               Analyze_Use_Package (U);
            else
               Analyze_Use_Type (U);
            end if;
         end loop;
      end Re_Install_Use_Clauses;

   --  Start of processing for Analyze_Subunit

   begin
      if not Is_Empty_List (Context_Items (N)) then

         --  Save current use clauses.

         Use_Clause := Scope_Stack.Table (Scope_Stack.Last).First_Use_Clause;
         Pop_Scope;
         Remove_Context (Lib_Unit);

         --  If the parent is a package body, remove the context of the spec
         --  as well. If it is a subprogram body, verify first that there is
         --  a spec for it. If the parent is a subunit, Lib_Spec is its
         --  parent, whose context must also be removed, together with that
         --  of further ancestors.

         if Present (Lib_Spec) then
            Remove_Context (Lib_Spec);

            while Nkind (Unit (Lib_Spec)) = N_Subunit loop
               Lib_Spec := Library_Unit (Lib_Spec);
               Remove_Context (Lib_Spec);
            end loop;
         end if;

         Analyze_Context (N);

         Re_Install_Parents (Lib_Unit);
         New_Scope (Par_Unit);

         --  If the context includes a child unit of the parent of the
         --  subunit, the parent will have been removed from visibility,
         --  after compiling that cousin in the context. The visibility
         --  of the parent must be restored now. This also applies if the
         --  context includes another subunit of the same parent which in
         --  turn includes a child unit in its context.

         if Ekind (Par_Unit) = E_Package then
            if not Is_Immediately_Visible (Par_Unit)
              or else (Present (First_Entity (Par_Unit))
                        and then not Is_Immediately_Visible
                         (First_Entity (Par_Unit)))
            then
               Set_Is_Immediately_Visible   (Par_Unit);
               Install_Visible_Declarations (Par_Unit);
               Install_Private_Declarations (Par_Unit);
            end if;
         end if;

         Re_Install_Use_Clauses;
         Install_Context (N);
      end if;

      Analyze (Proper_Body (Unit (N)));
      Remove_Context (N);

   end Analyze_Subunit;

   -------------------------
   -- Analyze_With_Clause --
   -------------------------

   --  Analyze the declaration of a unit in  a with clause. At end,
   --  label the with clause with the defining entity for the unit.

   procedure Analyze_With_Clause (N : Node_Id) is
      Unit_Kind : constant Node_Kind := Nkind (Unit (Library_Unit (N)));
      E_Name    : Entity_Id;

   begin
      Semantics (Library_Unit (N));

      if Unit_Kind in N_Generic_Declaration then

         --  Semantic analysis of a generic unit is performed on a copy of
         --  the original tree. Retrieve the entity on  which semantic info
         --  actually appears.

         E_Name := Defining_Entity (Unit (Library_Unit (N)));

      --  Note: in the following test, Unit_Kind is the original Nkind, but
      --  in the case of an instantiation, the call to Semantics above will
      --  have replaced the unit by its instantiated version.

      elsif Unit_Kind = N_Package_Instantiation
        and then Nkind (Unit (Library_Unit (N))) = N_Package_Body
      then
         --  Instantiation node is replaced with body of instance.
         --  Unit name is defining unit name in corresponding spec.

         E_Name := Corresponding_Spec (Unit (Library_Unit (N)));

      elsif Unit_Kind = N_Procedure_Instantiation
        or else Unit_Kind = N_Function_Instantiation
      then
         --  Instantiation node is replaced with a package that contains
         --  renaming declarations and instance itself. The subprogram
         --  specification is the last declaration in the package spec.

         E_Name :=
           Defining_Entity
             (Last (Visible_Declarations
                     (Specification (Unit (Library_Unit (N))))));

      elsif Unit_Kind = N_Package_Renaming_Declaration
        or else Unit_Kind in N_Generic_Renaming_Declaration
      then
         E_Name := Defining_Entity (Unit (Library_Unit (N)));

      elsif Unit_Kind = N_Subprogram_Body
        and then Nkind (Name (N)) = N_Selected_Component
        and then not Acts_As_Spec (Library_Unit (N))
      then

         --  For a child unit that has no spec, one has been created and
         --  analyzed. The entity required is that of the spec.

         E_Name := Corresponding_Spec (Unit (Library_Unit (N)));

      else
         E_Name := Defining_Entity (Unit (Library_Unit (N)));
      end if;

      if Nkind (Name (N)) = N_Selected_Component then

         --  Child unit in a with clause

         Change_Selected_Component_To_Expanded_Name (Name (N));
      end if;

      Set_Entity_With_Style_Check (Name (N), E_Name);

   end Analyze_With_Clause;

   ------------------------------
   -- Check_Private_Child_Unit --
   ------------------------------

   procedure Check_Private_Child_Unit (N : Node_Id) is
      Lib_Unit   : constant Node_Id := Unit (N);
      Item       : Node_Id;
      Curr_Unit  : Entity_Id;
      Sub_Parent : Node_Id;
      Priv_Child : Entity_Id;
      Par_Lib    : Entity_Id;
      Par_Spec   : Node_Id;

      function Is_Private_Library_Unit (Unit : Entity_Id) return Boolean;
      --  Returns true if and only if the library unit is declared with
      --  an explicit designation of private.

      function Is_Private_Library_Unit (Unit : Entity_Id) return Boolean is
      begin
         return Private_Present (Parent (Get_Declaration_Node (Unit)));
      end Is_Private_Library_Unit;

   --  Start of processing for Check_Private_Child_Unit

   begin
      if Nkind (Lib_Unit) = N_Package_Body
        or else Nkind (Lib_Unit) = N_Subprogram_Body
      then
         Curr_Unit := Defining_Entity (Unit (Library_Unit (N)));
         Par_Lib   := Curr_Unit;

      elsif Nkind (Lib_Unit) = N_Subunit then

         --  The parent is itself a body. The parent entity is to be found
         --  in the corresponding spec.

         Sub_Parent := Library_Unit (N);
         Curr_Unit  := Defining_Entity (Unit (Library_Unit (Sub_Parent)));
         Par_Lib    := Curr_Unit;

      else
         Curr_Unit := Defining_Entity (Lib_Unit);

         Par_Lib := Curr_Unit;
         Par_Spec  := Parent_Spec (Lib_Unit);

         if No (Par_Spec) then
            Par_Lib := Empty;
         else
            Par_Lib := Defining_Entity (Unit (Par_Spec));
         end if;
      end if;

      Item := First (Context_Items (N));
      while Present (Item) loop

         if Nkind (Item) = N_With_Clause
            and then not Implicit_With (Item)
            and then Is_Private_Descendant (Entity (Name (Item)))
         then
            Priv_Child := Entity (Name (Item));

            declare
               Curr_Parent  : Entity_Id := Par_Lib;
               Child_Parent : Entity_Id := Scope (Priv_Child);
               Prv_Ancestor : Entity_Id := Child_Parent;
               Curr_Private : Boolean   := Is_Private_Library_Unit (Curr_Unit);

            begin
               --  If the child unit is a public child then locate
               --  the nearest private ancestor; Child_Parent will
               --  then be set to the parent of that ancestor.

               if not Is_Private_Library_Unit (Priv_Child) then
                  while Present (Prv_Ancestor)
                    and then not Is_Private_Library_Unit (Prv_Ancestor)
                  loop
                     Prv_Ancestor := Scope (Prv_Ancestor);
                  end loop;

                  if Present (Prv_Ancestor) then
                     Child_Parent := Scope (Prv_Ancestor);
                  end if;
               end if;

               while Present (Curr_Parent)
                 and then Curr_Parent /= Standard_Standard
                 and then Curr_Parent /= Child_Parent
               loop
                  Curr_Private :=
                    Curr_Private or else Is_Private_Library_Unit (Curr_Parent);
                  Curr_Parent := Scope (Curr_Parent);
               end loop;

               if not Present (Curr_Parent) then
                  Curr_Parent := Standard_Standard;
               end if;

               if Curr_Parent /= Child_Parent then

                  if Ekind (Priv_Child) = E_Generic_Package
                    and then Chars (Priv_Child) in Text_IO_Package_Name
                    and then Chars (Scope (Scope (Priv_Child))) = Name_Ada
                  then
                     Error_Msg_NE
                       ("& is a nested package, not a compilation unit",
                       Name (Item), Priv_Child);
                  else

                     Error_Msg_N
                       ("unit in with clause is private child unit!", Item);
                     Error_Msg_NE
                       ("current unit must also have parent&!",
                        Item, Child_Parent);
                  end if;

               elsif not Curr_Private
                 and then Nkind (Lib_Unit) /= N_Package_Body
                 and then Nkind (Lib_Unit) /= N_Subprogram_Body
               then
                  Error_Msg_NE
                    ("current unit must also be private descendant of&",
                     Item, Child_Parent);
               end if;
            end;
         end if;

         Item := Next (Item);
      end loop;

   end Check_Private_Child_Unit;

   ----------------------
   -- Check_Stub_Level --
   ----------------------

   procedure Check_Stub_Level (N : Node_Id) is
      Par  : constant Node_Id   := Parent (N);
      Kind : constant Node_Kind := Nkind (Par);

   begin
      if (Kind = N_Package_Body
           or else Kind = N_Subprogram_Body
           or else Kind = N_Task_Body
           or else Kind = N_Protected_Body)

        and then (Nkind (Parent (Par)) = N_Compilation_Unit
                   or else Nkind (Parent (Par)) = N_Subunit)
      then
         null;
      else
         Error_Msg_N ("stub cannot appear in an inner scope", N);
      end if;
   end Check_Stub_Level;

   ---------------------
   -- Install_Context --
   ---------------------

   procedure Install_Context (N : Node_Id) is
      Lib_Unit : Node_Id := Unit (N);

   begin
      Install_Context_Clauses (N);

      if Is_Child_Spec (Lib_Unit) then
         Install_Parents (Lib_Unit, Private_Present (Parent (Lib_Unit)));
      end if;
   end Install_Context;

   -----------------------------
   -- Install_Context_Clauses --
   -----------------------------

   procedure Install_Context_Clauses (N : Node_Id) is
      Lib_Unit      : Node_Id := Unit (N);
      Item          : Node_Id;
      Uname_Node    : Entity_Id;
      Unit_Num      : constant Unit_Number_Type := Get_Cunit_Unit_Number (N);
      Check_Private : Boolean := False;
      Decl_Node     : Node_Id;
      Lib_Parent    : Entity_Id;

   begin
      --  Loop through context clauses to find the with/use clauses

      Item := First (Context_Items (N));
      while Present (Item) loop

         if Nkind (Item) = N_With_Clause
           and then not Implicit_With (Item)
         then
            Uname_Node := Entity (Name (Item));

            if Is_Private_Descendant (Uname_Node) then
               Check_Private := True;
            end if;

            Install_Withed_Unit (Item);

            Decl_Node := Get_Declaration_Node (Uname_Node);

            --  If the unit is a subprogram instance, it appears nested
            --  within a package that carries the parent information.

            if Is_Generic_Instance (Uname_Node)
              and then Ekind (Uname_Node) /= E_Package
            then
               Decl_Node := Parent (Parent (Decl_Node));
            end if;

            if Is_Child_Spec (Decl_Node) then
               Implicit_With_On_Parent (Decl_Node, N);

            elsif Nkind (Decl_Node) = N_Subprogram_Body
              and then not Acts_As_Spec (Parent (Decl_Node))
              and then Is_Child_Spec (Unit (Library_Unit (Parent (Decl_Node))))
            then
               Implicit_With_On_Parent
                 (Unit (Library_Unit (Parent (Decl_Node))), N);
            end if;

         elsif Nkind (Item) = N_Use_Package_Clause then
            Analyze_Use_Package (Item);

         elsif Nkind (Item) = N_Use_Type_Clause then
            Analyze_Use_Type (Item);
         end if;

         Item := Next (Item);
      end loop;

      if Is_Child_Spec (Lib_Unit) then

         --  The unit also has implicit withs on its own parents.

         if No (Context_Items (N)) then
            Set_Context_Items (N, New_List);
         end if;

         Implicit_With_On_Parent (Lib_Unit, N);
      end if;

      --  If the unit is a body, the context of the specification must also
      --  be installed.

      if Nkind (Lib_Unit) = N_Package_Body
        or else (Nkind (Lib_Unit) = N_Subprogram_Body
                  and then not Acts_As_Spec (N))
      then
         Install_Context (Library_Unit (N));

         if Is_Child_Spec (Unit (Library_Unit (N))) then

            --  If the unit is the body of a public child unit, the private
            --  declarations of the parent must be made visible. If the child
            --  unit is private, the private declarations have been installed
            --  already in the call to Install_Parents for the spec. Installing
            --  private declarations must be done for all ancestors of public
            --  child units. In addition, sibling units mentioned in the
            --  context clause of the body are directly visible.

            declare
               Lib_Spec : Node_Id := Unit (Library_Unit (N));
               P        : Node_Id;
               P_Name   : Entity_Id;

            begin
               while Is_Child_Spec (Lib_Spec) loop
                  P := Unit (Parent_Spec (Lib_Spec));

                  if not (Private_Present (Parent (Lib_Spec))) then
                     P_Name := Defining_Entity (P);
                     Install_Private_Declarations (P_Name);
                     Set_Use (Private_Declarations (Specification (P)));
                  end if;

                  Lib_Spec := P;
               end loop;
            end;
         end if;

         --  For a package body, children in context are immediately visible

         Install_Siblings (Defining_Entity (Unit (Library_Unit (N))), N);
      end if;

      if Nkind (Lib_Unit) = N_Generic_Package_Declaration
        or else Nkind (Lib_Unit) = N_Generic_Subprogram_Declaration
        or else Nkind (Lib_Unit) = N_Package_Declaration
        or else Nkind (Lib_Unit) = N_Subprogram_Declaration
      then
         if Is_Child_Spec (Lib_Unit) then
            Lib_Parent := Defining_Entity (Unit (Parent_Spec (Lib_Unit)));
            Set_Is_Private_Descendant
              (Defining_Entity (Lib_Unit),
               Is_Private_Descendant (Lib_Parent)
                 or else Private_Present (Parent (Lib_Unit)));

         else
            Set_Is_Private_Descendant
              (Defining_Entity (Lib_Unit),
               Private_Present (Parent (Lib_Unit)));
         end if;
      end if;

      if Check_Private then
         Check_Private_Child_Unit (N);
      end if;
   end Install_Context_Clauses;

   -----------------------------
   -- Implicit_With_On_Parent --
   -----------------------------

   procedure Implicit_With_On_Parent (
     Child_Unit : Node_Id;
     N          : Node_Id)

   is
      Loc    : constant Source_Ptr := Sloc (N);
      P      : constant Node_Id    := Parent_Spec (Child_Unit);
      P_Unit : constant Node_Id    := Unit (P);

      P_Name : Entity_Id := Defining_Entity (P_Unit);
      Withn  : Node_Id;

      function Build_Unit_Name return Node_Id;
      --  If the unit is a child unit, build qualified name with all
      --  ancestors.

      function Build_Ancestor_Name (P : Node_Id)  return Node_Id;
      --  Build prefix of child unit name. Recurse if needed.

      function Build_Unit_Name return Node_Id is
         Result : Node_Id;

      begin
         if No (Parent_Spec (P_Unit)) then
            return New_Reference_To (P_Name, Loc);
         else
            Result :=
              Make_Expanded_Name (Loc,
                Chars  => Chars (P_Name),
                Prefix => Build_Ancestor_Name (Unit (Parent_Spec (P_Unit))),
                Selector_Name => New_Reference_To (P_Name, Loc));
            Set_Entity (Result, P_Name);
            return Result;
         end if;
      end Build_Unit_Name;

      function Build_Ancestor_Name (P : Node_Id) return Node_Id is
         P_Ref : Node_Id := New_Reference_To (Defining_Entity (P), Loc);

      begin
         if No (Parent_Spec (P)) then
            return P_Ref;
         else
            return
              Make_Selected_Component (Loc,
                Prefix => Build_Ancestor_Name (Unit (Parent_Spec (P))),
                Selector_Name => P_Ref);
         end if;
      end Build_Ancestor_Name;

   --  Start of processing for Implicit_With_On_Parent

   begin
      Withn := Make_With_Clause (Loc, Name => Build_Unit_Name);

      Set_Library_Unit          (Withn, P);
      Set_Corresponding_Spec    (Withn, P_Name);
      Set_First_Name            (Withn, True);
      Set_Implicit_With         (Withn, True);

      --  Node is placed at the beginning of the context items, so that
      --  subsequent use clauses on the parent can be validated.

      Prepend (Withn, Context_Items (N));
      Mark_Rewrite_Insertion (Withn);
      Install_Withed_Unit (Withn);

      if Is_Child_Spec (P_Unit) then
         Implicit_With_On_Parent (P_Unit, N);
      end if;
   end Implicit_With_On_Parent;

   -------------------------
   -- Install_Withed_Unit --
   -------------------------

   procedure Install_Withed_Unit (With_Clause : Node_Id) is
      Unit_Name : constant Entity_Id := Entity (Name (With_Clause));
      P         : constant Entity_Id := Scope (Unit_Name);

   begin
      if P /= Standard_Standard then

         --  If the unit is not analyzed after analysis of the with clause,
         --  and it is an instantiation, then it awaits a body and is the main
         --  unit. Its appearance in the context of some other unit indicates
         --  a circular dependency (DEC suite perversity).

         if not Analyzed (Unit_Name)
           and then Nkind (Parent (Unit_Name)) = N_Package_Instantiation
         then
            Error_Msg_N
              ("instantiation depends on itself", Name (With_Clause));

         elsif not Is_Visible_Child_Unit (Unit_Name) then
            Set_Is_Visible_Child_Unit (Unit_Name);

            --  The parent unit may have been installed already, and
            --  may have appeared in  a use clause.

            if In_Use (Scope (Unit_Name)) then
               Set_Is_Potentially_Use_Visible (Unit_Name);
            end if;

            Set_Context_Installed (With_Clause);
         end if;

      elsif not Is_Immediately_Visible (Unit_Name) then
         Set_Is_Immediately_Visible (Unit_Name);
         Set_Context_Installed (With_Clause);
      end if;

   end Install_Withed_Unit;

   -----------------------
   -- Load_Needed_Body --
   -----------------------

   --  N is a generic unit named in a with clause, or else it is
   --  a unit that contains a generic unit or an inlined function.
   --  In order to perform an instantiation, the body of the unit
   --  must be present. If the unit itself is generic, we assume
   --  that an instantiation follows, and  load and analyze the body
   --  unconditionally. This forces analysis of the spec as well.
   --  If the unit is not generic, but contains a generic unit, it
   --  is loaded on demand, at the point of instantiation (see ch12).

   procedure Load_Needed_Body (N : Node_Id) is
      Body_Name : Unit_Name_Type;
      Unum      : Unit_Number_Type;

   begin
      Body_Name := Get_Body_Name (Get_Unit_Name (Unit (N)));
      Unum := Load_Unit (Body_Name, True, N);
      Compiler_State := Analyzing; -- reset after load

      if Unum /= No_Unit
        and then not Fatal_Error (Unum)
      then
         if Debug_Flag_L then
            Write_Str ("*** Loaded generic body");
            Write_Eol;
         end if;

         Semantics (Cunit (Unum));
      end if;
   end Load_Needed_Body;

   ---------------------
   -- Install_Parents --
   ---------------------

   procedure Install_Parents (Lib_Unit : Node_Id; Is_Private : Boolean) is
      P      : Node_Id;
      E_Name : Entity_Id;
      P_Name : Entity_Id;
      P_Spec : Node_Id;

   begin
      P := Unit (Parent_Spec (Lib_Unit));
      P_Name := Defining_Entity (P);

      if Ekind (P_Name) = E_Generic_Package
        and then Nkind (Lib_Unit) /= N_Generic_Subprogram_Declaration
        and then Nkind (Lib_Unit) /= N_Generic_Package_Declaration
        and then Nkind (Lib_Unit) not in N_Generic_Renaming_Declaration
      then
         Error_Msg_N
           ("child of a generic package must be a generic unit", Lib_Unit);

      elsif Ekind (P_Name) /= E_Generic_Package
        and then Ekind (P_Name) /= E_Package
      then
         Error_Msg_N
           ("Parent unit must be package or generic package", Lib_Unit);
         raise Unrecoverable_Error;

      elsif Present (Renamed_Object (P_Name)) then
         Error_Msg_N ("parent unit cannot be a renaming", Lib_Unit);
         raise Unrecoverable_Error;

      elsif Nkind (Original_Node (P)) = N_Package_Instantiation
        and then Nkind (Lib_Unit) not in N_Renaming_Declaration
        and then Nkind (Lib_Unit) not in N_Generic_Instantiation
      then
         Error_Msg_N
           ("child of an instance must be an instance or renaming", Lib_Unit);
      end if;

      --  This is the recursive call that ensures all parents are loaded

      if Is_Child_Spec (P) then
         Install_Parents (P, Is_Private);
      end if;

      --  Now we can install the context for this parent

      Install_Context_Clauses (Parent_Spec (Lib_Unit));
      Install_Siblings (P_Name, Parent (Lib_Unit));

      --  The child unit is in the declarative region of the parent. The
      --  parent must therefore appear in the scope stack and be visible,
      --  as when compiling the corresponding body. If the child unit is
      --  private or it is a package body, private declarations must be
      --  accessible as well. Use declarations in the parent must also
      --  be installed. Finally, other child units of the same parent that
      --  are in the context are immediately visible.

      --  Find entity for compilation unit, and set its private descendant
      --  status as needed.

      E_Name := Defining_Entity (Lib_Unit);

      Set_Is_Child_Unit (E_Name);

      Set_Is_Private_Descendant (E_Name,
         Is_Private_Descendant (P_Name)
           or else Private_Present (Parent (Lib_Unit)));

      P_Spec := Specification (Get_Declaration_Node (P_Name));
      New_Scope (P_Name);

      --  Save current visibility of unit.

      Scope_Stack.Table (Scope_Stack.Last).Previous_Visibility :=
        Is_Immediately_Visible (P_Name);
      Set_Is_Immediately_Visible (P_Name);
      Install_Visible_Declarations (P_Name);
      Set_Use (Visible_Declarations (P_Spec));

      if Is_Private
        or else Private_Present (Parent (Lib_Unit))
      then
         Install_Private_Declarations (P_Name);
         Set_Use (Private_Declarations (P_Spec));
      end if;
   end Install_Parents;

   ----------------------
   -- Install_Siblings --
   ----------------------

   procedure Install_Siblings (U_Name : Entity_Id; N : Node_Id) is
      Item : Node_Id;
      Id   : Entity_Id;

      function Is_Ancestor (E : Entity_Id) return Boolean;
      --  Determine whether the scope of a child unit is an ancestor of
      --  the current unit.
      --  Shouldn't this be somewhere more general ???

      function Is_Ancestor (E : Entity_Id) return Boolean is
         Par : Entity_Id;

      begin
         Par := U_Name;

         while Present (Par)
           and then Par /= Standard_Standard
         loop

            if Par = E then
               return True;
            end if;

            Par := Scope (Par);
         end loop;

         return False;
      end Is_Ancestor;

   --  Start of processing for Install_Siblings

   begin
      --  Iterate over explicit with clauses, and check whether the
      --  scope of each entity is an ancestor of the current unit.

      Item := First (Context_Items (N));

      while Present (Item) loop

         if Nkind (Item) = N_With_Clause
           and then not Implicit_With (Item)
         then
            Id := Entity (Name (Item));

            if Is_Child_Unit (Id)
              and then Is_Ancestor (Scope (Id))
            then
               Set_Is_Immediately_Visible (Id);
            end if;
         end if;

         Item := Next (Item);
      end loop;
   end Install_Siblings;

   -------------------
   -- Is_Child_Spec --
   -------------------

   function Is_Child_Spec (Lib_Unit : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (Lib_Unit);

   begin
      return (K in N_Generic_Declaration              or else
              K in N_Generic_Instantiation            or else
              K in N_Generic_Renaming_Declaration     or else
              K =  N_Package_Declaration              or else
              K =  N_Package_Renaming_Declaration     or else
              K =  N_Subprogram_Declaration           or else
              K =  N_Subprogram_Renaming_Declaration)
        and then Present (Parent_Spec (Lib_Unit));
   end Is_Child_Spec;

   --------------------
   -- Remove_Parents --
   --------------------

   procedure Remove_Parents (Lib_Unit : Node_Id) is
      P      : Node_Id;
      P_Name : Entity_Id;
      E      : Entity_Id;
      Vis    : constant Boolean :=
                 Scope_Stack.Table (Scope_Stack.Last).Previous_Visibility;

   begin
      if Is_Child_Spec (Lib_Unit) then
         P := Unit (Parent_Spec (Lib_Unit));
         P_Name := Defining_Entity (P);

         Remove_Context_Clauses (Parent_Spec (Lib_Unit));
         End_Package_Scope (P_Name);
         Set_Is_Immediately_Visible (P_Name, Vis);

         --  Remove from visibility the siblings as well, which are directly
         --  visible while the parent is in scope.

         E := First_Entity (P_Name);

         while Present (E) loop

            if Is_Child_Unit (E) then
               Set_Is_Immediately_Visible (E, False);
            end if;

            E := Next_Entity (E);
         end loop;

         Set_In_Package_Body (P_Name, False);

         --  This is the recursive call to remove the context of any
         --  higher level parent. This recursion ensures that all parents
         --  are removed in the reverse order of their installation.

         Remove_Parents (P);
      end if;
   end Remove_Parents;

   --------------------
   -- Remove_Context --
   --------------------

   procedure Remove_Context (N : Node_Id) is
      Lib_Unit : constant Node_Id := Unit (N);
   begin
      --  If this is a child unit, first remove the parent units.

      if Is_Child_Spec (Lib_Unit) then
         Remove_Parents (Lib_Unit);
      end if;

      Remove_Context_Clauses (N);
   end Remove_Context;

   ----------------------------
   -- Remove_Context_Clauses --
   ----------------------------

   procedure Remove_Context_Clauses (N : Node_Id) is
      Lib_Unit  : constant Node_Id := Unit (N);
      Item      : Node_Id;
      Unit_Name : Entity_Id;

   begin

      --  Loop through context items and undo with_clauses and use_clauses.

      Item := First (Context_Items (N));

      while Present (Item) loop

         --  We are interested only in with clauses which got installed
         --  on entry, as indicated by their Context_Installed flag set

         if Nkind (Item) = N_With_Clause
            and then Context_Installed (Item)
         then
            --  Remove items from one with'ed unit

            Unit_Name := Entity (Name (Item));
            Remove_Withed_Unit (Unit_Name);
            Set_Context_Installed (Item, False);

         elsif Nkind (Item) = N_Use_Package_Clause then
            End_Use_Package (Item);

         elsif Nkind (Item) = N_Use_Type_Clause then
            End_Use_Type (Item);
         end if;

         Item := Next (Item);
      end loop;

   end Remove_Context_Clauses;

   ------------------------
   -- Remove_Withed_Unit --
   ------------------------

   procedure Remove_Withed_Unit (Unit_Name : Entity_Id) is
      P : Entity_Id := Scope (Unit_Name);

   begin

      if Debug_Flag_I then
         Write_Str ("remove withed unit ");
         Write_Name (Chars (Unit_Name));
         Write_Eol;
      end if;

      if P /= Standard_Standard then
         Set_Is_Visible_Child_Unit (Unit_Name, False);
      end if;

      Set_Is_Potentially_Use_Visible (Unit_Name, False);
      Set_Is_Immediately_Visible     (Unit_Name, False);

   end Remove_Withed_Unit;

end Sem_Ch10;
