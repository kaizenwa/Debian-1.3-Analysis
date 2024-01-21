------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T 1 D R V                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.55 $                             --
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
with Back_End;
with Comperr;
with Csets;    use Csets;
with Debug;    use Debug;
with Elists;
with Errout;   use Errout;
with Features;
with Fname;    use Fname;
with Frontend;
with Gnatvsn;  use Gnatvsn;
with Inline;
with Lib;      use Lib;
with Lib.Writ; use Lib.Writ;
with Namet;    use Namet;
with Nlists;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Par;
with Sem;
with Sinfo;    use Sinfo;
with Sinput.L; use Sinput.L;
with Snames;
with Sprint;   use Sprint;
with Stringt;
with System.Assertions;
with Tree_Gen;
with Treepr;   use Treepr;
with Types;    use Types;
with Uintp;
with Uname;    use Uname;
with Urealp;
with Usage;

procedure Gnat1drv is
   Main_Unit_Node : Node_Id;
   --  Compilation unit node for main unit

   Main_Kind : Node_Kind;
   --  Kind of main compilation unit node.

   Original_Operating_Mode : Operating_Mode_Type;
   --  Save operating type specified by options

   Call_Back_End : Boolean;
   --  Flag indicating whether we call the backend to generate code

begin
   --  This inner block is set up to catch assertion errors and constraint
   --  errors. Since the code for handling these errors can cause another
   --  exception to be raised (namely Unrecoverable_Error), we need two
   --  nested blocks, so that the outer one handles unrecoverable error.

   begin
      Osint.Initialize (Compiler);
      Osint.Scan_Compiler_Args;
      Osint.Add_Default_Search_Dirs;
      Sinput.Initialize;
      Lib.Initialize;
      Sem.Initialize;
      Csets.Initialize;
      Uintp.Initialize;
      Urealp.Initialize;
      Errout.Initialize;
      Namet.Initialize;
      Snames.Initialize;
      Stringt.Initialize;
      Features.Initialize;
      Inline.Initialize;

      if (Verbose_Mode or Full_List)
        and then (not Debug_Flag_7)
      then
         Write_Eol;
         Write_Str ("GNAT ");
         Write_Str (Gnat_Version_String);
         Write_Str (" Copyright 1991-1996 Free Software Foundation, Inc.");
         Write_Eol;
      end if;

      Original_Operating_Mode := Operating_Mode;
      Frontend;
      Main_Unit_Node := Cunit (Main_Unit);
      Main_Kind := Nkind (Unit (Main_Unit_Node));

      --  Check for suspicious or incorrect body present.

      --  Kludge alert: Note that at the moment we suppress this check in
      --  GNAT (-gnatg) mode, because our build process involves "legitimate"
      --  cases of junk bodies being present because the new GNULLI encounters
      --  old GNULLI bodies and must ignore them. ???

      if not Opt.GNAT_Mode
        and then
          ((Main_Kind = N_Package_Declaration
             and then not Body_Required (Main_Unit_Node))
           or else Main_Kind = N_Package_Renaming_Declaration
           or else Main_Kind = N_Subprogram_Renaming_Declaration
           or else Nkind (Original_Node (Unit (Main_Unit_Node)))
                           in N_Generic_Instantiation)
      then
         declare
            Sname : constant Unit_Name_Type := Unit_Name (Main_Unit);
            Fname : File_Name_Type;

         begin
            if Is_Body_Name (Sname) then
               Fname := Get_File_Name (Sname);
            else
               Fname := Get_File_Name (Get_Body_Name (Sname));
            end if;

            if Load_Source_File (Fname) /= No_Source_File then
               Error_Msg_Name_1 := Sname;

               --  Ada 83 case of a package body being ignored. This is not
               --  an error as far as the Ada 83 RM is concerned, but it is
               --  almost certainly not what is wanted so output a warning.

               if Main_Kind = N_Package_Declaration
                 and then Ada_83
                 and then Operating_Mode = Generate_Code
               then
                  Error_Msg_N
                    ("package % does not require a body?!", Main_Unit_Node);
                  Error_Msg_Name_1 := Fname;
                  Error_Msg_N
                    ("body in file{?! will be ignored", Main_Unit_Node);

               --  Ada 95 cases of a body file present when no body is
               --  permitted. This we consider to be an error

               else
                  if Nkind (Original_Node (Unit (Main_Unit_Node)))
                      in N_Generic_Instantiation
                  then
                     Error_Msg_N
                       ("generic instantiation for % does not allow a body",
                        Main_Unit_Node);

                  elsif Main_Kind = N_Package_Declaration then
                     Error_Msg_N
                       ("package % does not allow a body!",
                        Main_Unit_Node);
                  else
                     Error_Msg_N
                       ("renaming declaration for % does not allow a body!",
                        Main_Unit_Node);
                  end if;

                  Error_Msg_Name_1 := Fname;
                  Error_Msg_N
                    ("remove incorrect body in file{!", Main_Unit_Node);
               end if;
            end if;
         end;
      end if;

      --  Exit if compilation errors detected

      if Errors_Detected /= 0 then
         Errout.Finalize;
         Namet.Finalize;
         Features.Finalize;
         Exit_Program (E_Errors);
      end if;

      --  Case of no code required to be generated, exit indicating no error

      if Original_Operating_Mode /= Generate_Code then
         Errout.Finalize;
         Tree_Gen;
         Namet.Finalize;
         Features.Finalize;
         return;
      end if;

      --  All remaining cases are cases in which the user requested that code
      --  be generated (i.e. no -gnatc or -gnats switch was used). Check if
      --  we can in fact satisfy this request.

      --  Cannot generate code if someone has turned off code generation
      --  for any reason at all. We will try to figure out a reason below.

      if Operating_Mode /= Generate_Code then
         Call_Back_End := False;

      --  Cannot generate code if in stub generation mode

      elsif Distribution_Stub_Mode = Generate_Receiver_Stub_Body
           or else
         Distribution_Stub_Mode = Generate_Caller_Stub_Body
      then
         Call_Back_End := False;

      --  We can generate code for a subprogram body unless its corresponding
      --  subprogram spec is a generic delaration. Note that the check for
      --  No (Library_Unit) here is a defensive check that should not be
      --  necessary, since the Library_Unit field should be set properly.

      elsif Main_Kind = N_Subprogram_Body
        and then not Subunits_Missing
        and then (No (Library_Unit (Main_Unit_Node))
                   or else Nkind (Unit (Library_Unit (Main_Unit_Node))) /=
                                          N_Generic_Subprogram_Declaration)
      then
         Call_Back_End := True;

      --  We can generate code for a package body unless its corresponding
      --  package spec is a generic declaration. As described above, the
      --  check for No (LIbrary_Unit) is a defensive check.

      elsif Main_Kind = N_Package_Body
        and then not Subunits_Missing
        and then (No (Library_Unit (Main_Unit_Node))
           or else Nkind (Unit (Library_Unit (Main_Unit_Node))) /=
                      N_Generic_Package_Declaration)
      then
         Call_Back_End := True;

      --  We can generate code for a package declaration or a subprogram
      --  declaration only if it does not required a body.

      elsif (Main_Kind = N_Package_Declaration
               or else
             Main_Kind = N_Subprogram_Declaration)
        and then not Body_Required (Main_Unit_Node)
      then
         Call_Back_End := True;

      --  Compilation units that are renamings do not require bodies,
      --  so we can generate code for them.

      elsif Main_Kind = N_Package_Renaming_Declaration
        or else Main_Kind = N_Subprogram_Renaming_Declaration
      then
         Call_Back_End := True;

      --  In all other cases (specs which have bodies, generics, and bodies
      --  where subunits are missing), we cannot generate code and we generate
      --  a warning message. Note that generic instantiations are gone at this
      --  stage since they have been replaced by their instances.

      else
         Call_Back_End := False;
      end if;

      --  At this stage Call_Back_End is set to indicate if the backend
      --  should be called to generate code. If it is not set, then code
      --  generation has been turned off, even though code was requested
      --  by the original command. This is not an error from the user
      --  point of view, but it is an error from the point of view of
      --  the gcc driver, so we must exit with an error status.

      --  We generate an informative message (from the gcc point of view,
      --  it is an error message, but from the users point of view this
      --  is not an error, just a consequence of compiling something that
      --  cannot generate code.

      if not Call_Back_End then
         Write_Str ("No code generated for ");
         Write_Str ("file ");
         Write_Name (Unit_File_Name (Main_Unit));

         if Subunits_Missing then
            Write_Str (" (missing subunits)");

         elsif Main_Kind = N_Subunit then
            Write_Str (" (subunit)");

         elsif Main_Kind = N_Package_Body
           or else Main_Kind = N_Subprogram_Body
         then
            Write_Str (" (generic unit)");

         elsif Main_Kind = N_Subprogram_Declaration then
            Write_Str (" (subprogram spec)");

         --  Only other case is a package spec

         else
            Write_Str (" (package spec)");
         end if;

         Write_Eol;
         Errout.Finalize;
         Tree_Gen;
         Namet.Finalize;

         --  Exit program with error indication, to kill object file

         Exit_Program (E_No_Code);
      end if;

      --  Lock tables that backend is not supposed to touch

      Atree.Lock;
      Elists.Lock;
      Fname.Lock;
      Inline.Lock;
      Lib.Lock;
      Namet.Lock;
      Nlists.Lock;
      Sem.Lock;
      Sinput.Lock;
      Stringt.Lock;

      --  Here we will call the backend to generate code

      Set_Generate_Code (Main_Unit);

      --  If we have a corresponding spec, then we need object
      --  code for the spec unit as well

      if Nkind (Unit (Main_Unit_Node)) in N_Unit_Body
        and then not Acts_As_Spec (Main_Unit_Node)
      then
         Set_Generate_Code
           (Get_Cunit_Unit_Number (Library_Unit (Main_Unit_Node)));
      end if;

      --  Here we call the backend to generate the output code

      Back_End;

      --  Once the backend is complete, we unlock the names table. This
      --  call allows a few extra entries, needed for example for the file
      --  name for the library file output.

      Namet.Unlock;

      --  Now we complete output of errors, the tree and the features list.
      --  These are delayed till now, since it is perfectly possible for
      --  gigi to generate errors, modify the tree (in particular by setting
      --  flags indicating that elaboration is required), and to register
      --  the use of special features.

      Errout.Finalize;
      Tree_Gen;
      Features.Finalize;

      --  Only write the library if the backend did not generate any error
      --  messages. Otherwise signal errors to the driver program so that
      --  there will be no attempt to generate an object file.

      if Errors_Detected /= 0 then
         Exit_Program (E_Errors);
      end if;

      Lib.Writ.Write_Library_Info;
      Namet.Finalize;

   exception
      --  Handle fatal internal compiler errors

      when System.Assertions.Assert_Failure =>
         Comperr.Compiler_Abort ("Assert_Failure");

      when Constraint_Error =>
         Comperr.Compiler_Abort ("Constraint_Error");

      when Program_Error =>
         Comperr.Compiler_Abort ("Program_Error");

      when Storage_Error =>
         Set_Standard_Error;
         Write_Str ("insufficient memory for compiler");
         Write_Eol;
         raise Unrecoverable_Error;
   end;

--  The outer exception handles an unrecoverable error

exception
   when Unrecoverable_Error =>
      Errout.Finalize;
      Set_Standard_Error;
      Write_Str ("compilation abandoned");
      Write_Eol;
      Set_Standard_Output;

      Tree_Dump;
      Source_Dump;
      Exit_Program (E_Errors);

end Gnat1drv;
