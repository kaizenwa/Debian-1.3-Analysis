------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D G E N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.68 $                             --
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

with ALI;      use ALI;
with Binde;    use Binde;
with Hostparm;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Types;    use Types;
with Sdefault; use Sdefault;

with GNAT.OS_Lib;

package body Bindgen is

   Statement_Buffer : String (1 .. 1000);
   --  Buffer used for constructing output statements

   With_Finalization : Boolean := False;
   --  Flag which indicates whether the program use finalization
   --  (presence of the unit System.Finalization_Implementation)

   With_Tasking : Boolean := False;

   Default_Object_Dir : String_Ptr := Object_Dir_Default_Name;
   Default_Object_Len : Integer := Default_Object_Dir.all'Length;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Gen_Adainit (Main : Boolean);
   --  Generates the Adainit procedure. Main is true if the main program is
   --  Ada.

   procedure Gen_Adafinal;
   --  Generate the Adafinal procedure as required by thr RM

   procedure Gen_Elab_Calls;
   --  Generate sequence of elaboration calls

   procedure Gen_Main_Program_File;
   --  Generate lines for output file in main program case

   procedure Gen_Non_Main_Program_File;
   --  Generate lines for output file in non-main program case

   procedure List_Object_Files_Options;
   --  Output a comment containing a list of the full names of the object
   --  files to be linked and the list of linker options supplised by
   --  Linker_Options pragmas in the source.

   procedure List_Versions;
   --  Output series of definitions for unit versions

   ---------------------
   -- Gen_Output_File --
   ---------------------

   procedure Gen_Output_File (Filename : String) is
   begin
      Create_Binder_Output (Filename);

      if Bind_Main_Program then
         Gen_Main_Program_File;
      else
         Gen_Non_Main_Program_File;
      end if;

      Close_Binder_Output;
   end Gen_Output_File;

   --------------------
   -- Gen_Elab_Calls --
   --------------------

   procedure Gen_Elab_Calls is
      L   : Natural;

   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Unit.Table (Elab_Order.Table (E)).Uname);

         --  if the program uses finalization we must make sure to finalize
         --  global objects too at the end of the program.

         if Name_Buffer (1 .. 34) = "system.finalization_implementation" then
            With_Finalization := True;
         end if;

         if Name_Buffer (1 .. 21) = "system.tasking.stages" then
            With_Tasking := True;
         end if;

         --  Generate elaboration call if elaboration needed

         if not Unit.Table (Elab_Order.Table (E)).No_Elab then
            Statement_Buffer (1 .. 3) := "   ";

            --  Copy the unit name (and replace '.' by '__' for child unit)

            L := 4;

            for J in 1 .. Name_Len - 2 loop
               if Name_Buffer (J) /= '.' then
                  Statement_Buffer (L) := Name_Buffer (J);
                  L := L + 1;
               else
                  Statement_Buffer (L .. L + 1) := "__";
                  L := L + 2;
               end if;
            end loop;

            --  Complete call to elaboration routine

            Statement_Buffer (L .. L + 6) := "___elab";
            Statement_Buffer (L + 7) := Name_Buffer (Name_Len);
            Statement_Buffer (L + 8 .. L + 11) := " ();";
            L := L + 11;
            Write_Binder_Info (Statement_Buffer (1 .. L));
         end if;
      end loop;
   end Gen_Elab_Calls;

   -----------------
   -- Gen_Adainit --
   -----------------

   procedure Gen_Adainit (Main : Boolean) is
   begin

      Write_Binder_Info ("void adainit ()");
      Write_Binder_Info ("{");

      if not Main then

         --  The flag ada__init_flag is used to ensure that only the first
         --  call to adainit has an effect (RM B.1(39)). It is false (zero)
         --  before the first call, and true thereafter.

         Write_Binder_Info ("   static int ada__init_flag = 0;");
         Write_Binder_Info ("   if (ada__init_flag) return;");
         Write_Binder_Info ("   ada__init_flag++;");
      end if;

      Gen_Elab_Calls;
      Write_Binder_Info ("}");
   end Gen_Adainit;

   ------------------
   -- Gen_Adafinal --
   ------------------

   procedure Gen_Adafinal is
   begin
      Write_Binder_Info ("void adafinal () {");

      if With_Tasking then
         Write_Binder_Info ("   system__tasking__stages"
           & "__finalize_global_tasks ();");

      elsif With_Finalization then
         Write_Binder_Info ("   system__finalization_implementation"
           & "__finalize_global_list ();");
      end if;

      Write_Binder_Info ("}");
   end Gen_Adafinal;

   ---------------------------
   -- Gen_Main_Program_File --
   ---------------------------

   procedure Gen_Main_Program_File is
      Ctr : Integer;

      procedure Set_Int (N : Int);
      --  Set given value in decimal in Statement_Buffer with no spaces
      --  starting at the Ctr position and updating Ctr past the value.
      --  A negative sign is output for a negative value.

      procedure Set_Main_Program_Name;
      --  Given the main program name in Name_Buffer (length in Name_Len)
      --  generate the name of the routine to be used in the call. The
      --  name is generated starting at Ctr, and Ctr is updated past it.

      procedure Set_Int (N : Int) is
      begin
         if N < 0 then
            Statement_Buffer (Ctr) := '-';
            Ctr := Ctr + 1;
            Set_Int (-N);
         else
            if N > 9 then
               Set_Int (N / 10);
            end if;

            Statement_Buffer (Ctr) :=
              Character'Val (N mod 10 + Character'Pos ('0'));
            Ctr := Ctr + 1;
         end if;
      end Set_Int;

      procedure Set_Main_Program_Name is
      begin
         --  Note that name has %b on the end which we ignore

         --  Output initial _ada_ if no dots in name

         for J in 1 .. Name_Len - 1 loop
            if J = Name_Len - 1 then
               Statement_Buffer (Ctr .. Ctr + 4) := "_ada_";
               Ctr := Ctr + 5;

            else
               exit when Name_Buffer (J) = '.';
            end if;
         end loop;

         --  Copy name, changing dots to double underscores

         for J in 1 .. Name_Len - 2 loop
            if Name_Buffer (J) = '.' then
               Statement_Buffer (Ctr) := '_';
               Statement_Buffer (Ctr + 1) := '_';
               Ctr := Ctr + 2;
            else
               Statement_Buffer (Ctr) := Name_Buffer (J);
               Ctr := Ctr + 1;
            end if;
         end loop;
      end Set_Main_Program_Name;

   --  Start of processing for Gen_Main_Program_File

   begin
      --  Generate __main_priority

      Statement_Buffer (1 .. 23) := "int  __main_priority = ";
      Ctr := 24;
      Set_Int (ALIs.Table (ALIs.First).Main_Priority);
      Statement_Buffer (Ctr) := ';';
      Write_Binder_Info (Statement_Buffer (1 .. Ctr));

      --  Generate time slice value, -1 means it was not set

      Statement_Buffer (1 .. 24) := "int  __time_slice_val = ";
      Ctr := 25;
      Set_Int (ALIs.Table (ALIs.First).Time_Slice_Value);
      Statement_Buffer (Ctr) := ';';
      Write_Binder_Info (Statement_Buffer (1 .. Ctr));

      --  Generate locking policy value (blank = none, otherwise set to the
      --  first character, in upper case, of the policy name).

      Statement_Buffer (1 .. 28) := "char __locking_policy = ' ';";
      Statement_Buffer (26) := Locking_Policy;
      Write_Binder_Info (Statement_Buffer (1 .. 28));

      --  Generate queuing policy value (blank = none, otherwise set to the
      --  first character, in upper case, of the policy name).

      Statement_Buffer (1 .. 28) := "char __queuing_policy = ' ';";
      Statement_Buffer (26) := Queuing_Policy;
      Write_Binder_Info (Statement_Buffer (1 .. 28));

      --  Generate task dispatching policy value (blank = none, otherwise set
      --  to the first character, in upper case, of the policy name).

      Statement_Buffer (1 .. 37) := "char __task_dispatching_policy = ' ';";
      Statement_Buffer (35) := Task_Dispatching_Policy;
      Write_Binder_Info (Statement_Buffer (1 .. 37));

      Write_Binder_Info ("");

      --  Write argv/argc stuff

      Write_Binder_Info ("extern int gnat_argc;");
      Write_Binder_Info ("extern char **gnat_argv;");
      Write_Binder_Info ("extern char **gnat_envp;");
      Write_Binder_Info ("extern int gnat_exit_status;");

      --  Generate adainit and adafinal

      Gen_Adainit (Main => True);
      Gen_Adafinal;

      --  Generate main

      Write_Binder_Info ("int main (argc, argv, envp)");
      Write_Binder_Info ("    int argc;");
      Write_Binder_Info ("    char **argv;");
      Write_Binder_Info ("    char **envp;");
      Write_Binder_Info ("{");
      Write_Binder_Info ("   gnat_argc = argc;");
      Write_Binder_Info ("   gnat_argv = argv;");
      Write_Binder_Info ("   gnat_envp = envp;");
      Write_Binder_Info (" ");

      Write_Binder_Info ("   __gnat_initialize();");
      Write_Binder_Info ("   adainit();");
      Write_Binder_Info (" ");

      --  Output main program name

      Get_Name_String (Unit.Table (First_Unit_Entry).Uname);

      --  Main program is procedure case

      if ALIs.Table (ALIs.First).Main_Program = Proc then
         Statement_Buffer (1 .. 3) := "   ";
         Ctr := 4;
         Set_Main_Program_Name;
         Statement_Buffer (Ctr .. Ctr + 3) := " ();";
         Write_Binder_Info (Statement_Buffer (1 .. Ctr + 3));

      --  Main program is function case

      else -- ALIs.Table (ALIs_First).Main_Program = Func
         Statement_Buffer (1 .. 11) := "   return (";
         Ctr := 12;
         Set_Main_Program_Name;
         Statement_Buffer (Ctr .. Ctr + 4) := " ());";
         Write_Binder_Info (Statement_Buffer (1 .. Ctr + 4));
      end if;

      Write_Binder_Info (" ");
      Write_Binder_Info ("   adafinal();");
      Write_Binder_Info ("   __gnat_finalize();");

      Write_Binder_Info ("   exit (gnat_exit_status);");
      Write_Binder_Info ("}");
      List_Versions;
      List_Object_Files_Options;
   end Gen_Main_Program_File;

   -------------------------------
   -- Gen_Non_Main_Program_File --
   -------------------------------

   procedure Gen_Non_Main_Program_File is
   begin
      --  Generate dummy __main_priority function

      begin
         Write_Binder_Info ("int");
         Write_Binder_Info ("__main_priority ()");
         Write_Binder_Info ("{");
         Write_Binder_Info (" return -1;");
         Write_Binder_Info ("}");
      end;

      --  Generate dummy time slice value, -1 means it was not set

      Write_Binder_Info ("int __time_slice = -1;");

      --  Generate Adainit and Adafinal

      Gen_Adainit (Main => False);
      Gen_Adafinal;

      --  Output version and object file information

      List_Versions;
      List_Object_Files_Options;
   end Gen_Non_Main_Program_File;

   -------------------------------
   -- List_Object_Files_Options --
   -------------------------------

   procedure List_Object_Files_Options is
      Sptr : Natural;

   begin
      Write_Binder_Info ("/* BEGIN Object file/option list");

      for E in Elab_Order.First .. Elab_Order.Last loop

         --  If not spec that has an associated body, then generate a
         --  comment giving the name of the corresponding object file,
         --  except that we always skip shared passive units.

         if Unit.Table (Elab_Order.Table (E)).Utype /= Is_Spec
           and then not Unit.Table (Elab_Order.Table (E)).Shared_Passive
         then
            Get_Name_String
              (ALIs.Table
                (Unit.Table (Elab_Order.Table (E)).My_ALI).Ofile_Full_Name);

            --  If the presence of an object file is necessary or if it
            --  exists, then use it.

            if not Hostparm.Exclude_Missing_Objects
              or else
                GNAT.OS_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len))
            then
               Write_Binder_Info (Name_Buffer (1 .. Name_Len));
            end if;
         end if;
      end loop;

      --  Write linker options

      Sptr := 0;
      for J in 1 .. Linker_Options.Last loop
         if Linker_Options.Table (J) = Ascii.Nul then
            Write_Binder_Info (Statement_Buffer (1 .. Sptr));
            Sptr := 0;
         else
            Sptr := Sptr + 1;
            Statement_Buffer (Sptr) := Linker_Options.Table (J);
         end if;
      end loop;

      Write_Binder_Info ("   END Object file/option list */");
   end List_Object_Files_Options;

   -------------------
   -- List_Versions --
   -------------------

   --  This routine generates a line of the form:

   --    unsigned unam = 0xhhhhhhhh;

   --  for each unit, where unam is the unit name suffixed by either B or
   --  S for body or spec, with dots replaced by double underscores.

   procedure List_Versions is
      Sptr : Natural;

   begin
      for U in Unit.First .. Unit.Last loop
         Statement_Buffer (1 .. 9) := "unsigned ";
         Sptr := 10;

         Get_Name_String (Unit.Table (U).Uname);

         for K in 1 .. Name_Len loop
            if Name_Buffer (K) = '.' then
               Statement_Buffer (Sptr) := '_';
               Sptr := Sptr + 1;
               Name_Buffer (K) := '_';

            elsif Name_Buffer (K) = '%' then
               exit;
            end if;

            Statement_Buffer (Sptr) := Name_Buffer (K);
            Sptr := Sptr + 1;
         end loop;

         if Name_Buffer (Name_Len) = 's' then
            Statement_Buffer (Sptr) := 'S';
         else
            Statement_Buffer (Sptr) := 'B';
         end if;

         Sptr := Sptr + 1;
         Statement_Buffer (Sptr .. Sptr + 4) := " = 0x";
         Sptr := Sptr + 5;
         Statement_Buffer (Sptr .. Sptr + 7) := Unit.Table (U).Version;
         Statement_Buffer (Sptr + 8) := ';';
         Write_Binder_Info (Statement_Buffer (1 .. Sptr + 8));
      end loop;

   end List_Versions;

end Bindgen;
