------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T F D R V                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.41 $                             --
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

with Csets;    use Csets;
with Comperr;
with Debug;    use Debug;
with Errout;   use Errout;
with Features;
with Frontend;
with Gnatvsn;
with Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Par;
with Sem_Type;
with Snames;
with Sprint;
with Stringt;
with System.Assertions;
with Treepr;
with Types;    use Types;
with Uintp;
with Urealp;
with Usage;
with Xref;

procedure Gnatfdrv is

   Total_Warnings : Nat := 0;
   --  Counts total warnings in all files

   Total_Errors : Nat := 0;
   --  Counts total errors in all files

begin
   --  The following package initializations are done once for the complete
   --  set of main source files. It is in particular important that the
   --  names table not be reinitialized between compilations, since we use
   --  name table indexes in the source file table.

   Osint.Initialize (Compiler);
   Osint.Scan_Compiler_Args;
   Osint.Add_Default_Search_Dirs;

   --  Either we are in syntax only mode (when calling gnatchop) or
   --  we want to perform semantic checks for Xref.

   if Operating_Mode = Generate_Code then
      Operating_Mode := Check_Semantics;
   end if;

   Xref.Initialize;
   Csets.Initialize;
   Uintp.Initialize;
   Urealp.Initialize;
   Namet.Initialize;
   Snames.Initialize;
   Stringt.Initialize;
   Features.Initialize;
   Errout.Initialize;

   if (Verbose_Mode or Full_List)
     and then (not Debug_Flag_7)
   then
      Write_Eol;
      Write_Str ("GNATF ");
      Write_Str (Gnatvsn.Gnat_Version_String);
      Write_Str (" Copyright 1991-1996 Free Software Foundation, Inc.");
      Write_Eol;
   end if;

   --  Output usage information if no files

   if not More_Source_Files then
      Usage;
      Exit_Program (E_Fatal);
   end if;

   --  Loop through files

   while More_Source_Files loop

      --  The outer block is here to handle an unrecoverable error if one
      --  is signalled (by raising the Unrecoverable_Error exception).

      begin
         --  The inner block is here to handle an assert error or constraint
         --  error. We need the nested blocks because the handling of these
         --  exceptions can end up raising an Unrecoverable_Error exception.

         begin
            Frontend;

            --  Update total error counts

            Total_Warnings := Total_Warnings + Warnings_Detected;
            Total_Errors   := Total_Errors + Errors_Detected;

            --  Let the Xref gather what it needs if there are no errors. We
            --  do not attempt to gather cross-reference info if errors occur.
            --  or if we are in syntax check only mode

            exit when Total_Errors > 0;



            Xref.Gather_Xref_Info (Lib.Cunit (Main_Unit));

            --  We don't reinitialize the names table for each file, since, as
            --  noted above, name table indices are used in the source file
            --  table and must not change from one compilation to another.
            --  However, it is necessary to reset the associated entity
            --  information, since that gets invalidated by destroying the
            --  tree for each new file.

            Namet.Reset_Name_Table;
            Sem_Type.Init_Interp_Tables;

         --  Exception handler catches fatal internal errors

         exception

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

      --  This is the handler for the outer block

      exception
         when Unrecoverable_Error =>
            Total_Warnings := Total_Warnings + Warnings_Detected;
            Total_Errors := Total_Errors + Errors_Detected;
            Errout.Finalize;
            Set_Standard_Error;
            Write_Str ("compilation of ");
            Write_Name (Lib.Unit_File_Name (Main_Unit));
            Write_Str (" abandoned");
            Write_Eol;
            Set_Standard_Output;
            Treepr.Tree_Dump;
            Sprint.Source_Dump;
            Exit_Program (E_Errors);
      end;

   end loop;

   if Total_Errors = 0 then
      Xref.Finalize;
   end if;

   Errout.Finalize;
   Features.Finalize;
   Namet.Finalize;

   --  All done. Set proper exit status

   if Total_Errors > 0 then
      Exit_Program (E_Errors);
   else
      Exit_Program (E_Success);
   end if;

end Gnatfdrv;
