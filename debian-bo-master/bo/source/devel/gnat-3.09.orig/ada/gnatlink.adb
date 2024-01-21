------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T L I N K                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.14 $                              --
--                                                                          --
--             Copyright (C) 1996 Free Software Foundation, Inc.            --
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

--  Gnatlink usage: please consult the gnat documentation

with Ada.Command_Line;     use Ada.Command_Line;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Gnatvsn;              use Gnatvsn;
with Interfaces.C_Streams; use Interfaces.C_Streams;
with Osint;                use Osint;
with Output;               use Output;
with System;               use System;
with Table;

procedure Gnatlink is

   package Linker_Options is new Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Linker_Options");

   package Linker_Objects is new Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Linker_Objects");

   package Binder_Options is new Table (
     Table_Component_Type => String_Access,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1, -- equals low bound of Argument_List for Spawn
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Gnatlink.Binder_Options");

   Gcc        : constant String := "gcc";

   Read_Mode  : constant String := "r" & Ascii.Nul;
   Write_Mode : constant String := "w" & Ascii.Nul;

   Begin_Info : constant String := "/* BEGIN Object file/option list";
   End_Info   : constant String := "   END Object file/option list */";

   Gcc_Path        : String_Access;
   Linker_Path     : String_Access;

   Output_Filename : String_Access;
   Ali_Filename    : String_Access;
   Binder_Src_File : String_Access;
   Binder_Obj_File : String_Access;

   Debug_Flag_Present : Boolean := False;
   Verbose_Mode       : Boolean := False;

   function Base_Name (File_Name : in String) return String;
   --  Return just the filename part without the extension (if present).

   procedure Delete (Name : in String);
   --  Wrapper to unlink as status is ignored by this application.

   procedure Exit_With_Error (Error : in String);
   --  Output Error and exit program with a fatal condition.

   procedure Process_Args;
   --  Go through all the arguments and build option tables.

   procedure Process_Binder_File (Name : in String);
   --  Reads the binder file and extracts linker arguments.

   procedure Write_Usage;
   --  Show user the program options.

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (File_Name : in String) return String is
      Findex1 : Natural;
      Findex2 : Natural;

   begin
      Findex1 := File_Name'First;

      --  The file might be specified by a full path name. However,
      --  we want the path to be stripped away.  In addition to the
      --  default directory_separator allow the '/' to act as separator
      --  since this is allowed in MS-DOS and OS2 ports.

      for J in reverse File_Name'Range loop
         if File_Name (J) = Directory_Separator
           or else File_Name (J) = '/'
         then
            Findex1 := J + 1;
            exit;
         end if;
      end loop;

      Findex2 := File_Name'Last;
      while Findex2 > Findex1
        and then File_Name (Findex2) /=  '.'
      loop
         Findex2 := Findex2 - 1;
      end loop;

      if Findex2 = Findex1 then
         Findex2 := File_Name'Last + 1;
      end if;

      return File_Name (Findex1 .. Findex2 - 1);

   end Base_Name;

   ------------
   -- Delete --
   ------------

   procedure Delete (Name : in String) is
      Status : int;

   begin
      Status := unlink (Name'Address);
   end Delete;

   ---------------------
   -- Exit_With_Error --
   ---------------------

   procedure Exit_With_Error (Error : in String) is
   begin
      Write_Str (Command_Name);
      Write_Str (": ");
      Write_Str (Error);
      Write_Eol;
      Exit_Program (E_Fatal);
   end Exit_With_Error;

   ------------------
   -- Process_Args --
   ------------------

   procedure Process_Args is
      Next_Arg : Integer;

   begin
      Binder_Options.Increment_Last;
      Binder_Options.Table (Binder_Options.Last) := new String'("-c");

      Next_Arg := 1;

      loop
         exit when Next_Arg > Argument_Count;

         Process_One_Arg : declare
            Arg : String := Argument (Next_Arg);

         begin

            if Arg'Length /= 0
              and then (Arg (1) = Switch_Character or else Arg (1) = '-')
            then
               if Arg (2) = 'g'
                 and then (Arg'Length = 2
                           or else Arg (3) in '0' .. '3')
               then
                  Debug_Flag_Present := True;

                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                   new String'(Arg);

                  Binder_Options.Increment_Last;
                  Binder_Options.Table (Binder_Options.Last) :=
                    Linker_Options.Table (Linker_Options.Last);

               elsif Arg'Length = 2 then
                  case Arg (2) is
                     when 'b' =>
                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                          new String'(Arg);

                        Binder_Options.Increment_Last;
                        Binder_Options.Table (Binder_Options.Last) :=
                          Linker_Options.Table (Linker_Options.Last);

                        Next_Arg := Next_Arg + 1;

                        if Next_Arg > Argument_Count then
                           Exit_With_Error ("Missing argument for -b");
                        end if;

                        Get_Machine_Name : declare
                           Name_Arg : String_Access :=
                                        new String'(Argument (Next_Arg));

                        begin
                           Linker_Options.Increment_Last;
                           Linker_Options.Table (Linker_Options.Last) :=
                             Name_Arg;

                           Binder_Options.Increment_Last;
                           Binder_Options.Table (Binder_Options.Last) :=
                             Name_Arg;

                        end Get_Machine_Name;

                     when 'o' =>
                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                         new String'(Arg);

                        Next_Arg := Next_Arg + 1;

                        if Next_Arg > Argument_Count then
                           Exit_With_Error ("Missing argument for -o");
                        end if;

                        Output_Filename := new String'(Argument (Next_Arg));

                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                          Output_Filename;

                     when 'v' =>

                        --  Support "double" verbose mode.  Second -v
                        --  gets sent to the linker and binder phases.

                        if Verbose_Mode then
                           Linker_Options.Increment_Last;
                           Linker_Options.Table (Linker_Options.Last) :=
                            new String'(Arg);

                           Binder_Options.Increment_Last;
                           Binder_Options.Table (Binder_Options.Last) :=
                             Linker_Options.Table (Linker_Options.Last);

                        else
                           Verbose_Mode := True;

                        end if;

                     when others =>
                        Linker_Options.Increment_Last;
                        Linker_Options.Table (Linker_Options.Last) :=
                         new String'(Arg);

                  end case;

               elsif Arg (2) = 'B' then
                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                   new String'(Arg);

                  Binder_Options.Increment_Last;
                  Binder_Options.Table (Binder_Options.Last) :=
                    Linker_Options.Table (Linker_Options.Last);

               elsif Arg = "-gnatlink" then

                  Next_Arg := Next_Arg + 1;

                  if Next_Arg > Argument_Count then
                     Exit_With_Error ("Missing argument for -gnatlink");
                  end if;

                  Linker_Path :=
                    GNAT.OS_Lib.Locate_Exec_On_Path (Argument (Next_Arg));

                  if Linker_Path = null then
                     Exit_With_Error
                       ("Could not locate linker: " & Argument (Next_Arg));
                  end if;

               else
                  --  Send all multi-character switches not recognized as
                  --  a special case by gnatlink to the linker/loader stage.

                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                   new String'(Arg);
               end if;

            else
               if Arg'Length > 4
                 and then Arg (Arg'Last - 3 .. Arg'Last) = ".ali"
               then
                  if Ali_Filename = null then
                     Ali_Filename := new String'(Arg);
                  else
                     Exit_With_Error
                       ("Sorry - cannot handle more than one ALI file");
                  end if;

               elsif Arg'Length > Get_Object_Suffix.all'Length
                 and then Arg
                   (Arg'Last - Get_Object_Suffix.all'Length + 1 .. Arg'Last)
                    = Get_Object_Suffix.all
               then
                  Linker_Objects.Increment_Last;
                  Linker_Objects.Table (Linker_Objects.Last) :=
                   new String'(Arg);

               else
                  Linker_Options.Increment_Last;
                  Linker_Options.Table (Linker_Options.Last) :=
                   new String'(Arg);
               end if;

            end if;

         end Process_One_Arg;

         Next_Arg := Next_Arg + 1;

      end loop;

   end Process_Args;

   -------------------------
   -- Process_Binder_File --
   -------------------------

   procedure Process_Binder_File (Name : in String) is
      Fd        : FILEs;
      Next_Line : String (1 .. 1000);
      Nlast     : Integer;
      Status    : int;

      procedure Get_Next_Line;
      --  Read the next line from the binder file without the line
      --  terminator.

      procedure Get_Next_Line is
         Fchars : chars;

      begin
         Fchars := fgets (Next_Line'Address, Next_Line'Length, Fd);

         if Fchars = System.Null_Address then
            Exit_With_Error ("Error reading binder output");
         end if;

         Nlast := Next_Line'First;
         while Nlast <= Next_Line'Last
           and then Next_Line (Nlast) /= Ascii.Lf
           and then Next_Line (Nlast) /= Ascii.Cr
         loop
            Nlast := Nlast + 1;
         end loop;

         Nlast := Nlast - 1;
      end Get_Next_Line;

   --  Start of processing for Process_Binder_File

   begin

      Fd := fopen (Name'Address, Read_Mode'Address);

      if Fd = NULL_Stream then
         Exit_With_Error ("Failed to open binder output");
      end if;

      loop
         Get_Next_Line;
         exit when Next_Line (Next_Line'First .. Nlast) = Begin_Info;
      end loop;

      loop
         Get_Next_Line;
         exit when Next_Line (Next_Line'First .. Nlast) = End_Info;

         Linker_Objects.Increment_Last;
         Linker_Objects.Table (Linker_Objects.Last) :=
           new String'(Next_Line (Next_Line'First .. Nlast));
      end loop;

      Status := fclose (Fd);

   end Process_Binder_File;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage is
   begin
      Write_Str ("Usage: ");
      Write_Str (Command_Name);
      Write_Str (" 'name'.ali");
      Write_Eol;
      Write_Str ("      [-o exec_name]     -- by default it is 'name'");
      Write_Eol;
      Write_Str ("      [-v]               -- verbose mode");
      Write_Eol;
      Write_Str ("      [-gnatlink name]   -- full name for the linker (gcc)");
      Write_Eol;
      Write_Str ("      [list of objects]  -- non Ada binaries");
      Write_Eol;
      Write_Str ("      [linker options]   -- other options for the linker");
      Write_Eol;
   end Write_Usage;

--  Start of processing for Gnatlink

begin -- Gnatlink

   if Argument_Count = 0 then
      Write_Usage;
      Exit_Program (E_Fatal);
   end if;

   Process_Args;

   --  Locate all the necessary programs and verify required files
   --  are present.

   Gcc_Path := GNAT.OS_Lib.Locate_Exec_On_Path (Gcc);

   if Gcc_Path = null then
      Exit_With_Error ("Couldn't locate " & Gcc);
   end if;

   if Linker_Path = null then
      Linker_Path := Gcc_Path;
   end if;

   if Ali_Filename = null then
      Exit_With_Error ("Required 'name'.ali not present.");
   end if;

   if not Is_Regular_File (Ali_Filename.all) then
      Exit_With_Error (Ali_Filename.all & " not found.");
   end if;

   if Verbose_Mode then
      Write_Eol;
      Write_Str ("GNATLINK ");
      Write_Str (Gnat_Version_String);
      Write_Str (" Copyright 1996 Free Software Foundation, Inc.");
      Write_Eol;
   end if;

   --  If there wasn't an output specified, then use the base name of
   --  the .ali filename.

   if Output_Filename = null then

      Output_Filename
        := new String'(Base_Name (Ali_Filename.all)
                       & Get_Debuggable_Suffix.all);

      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) :=
       new String'("-o");

      Linker_Options.Increment_Last;
      Linker_Options.Table (Linker_Options.Last) :=
       new String'(Output_Filename.all);

   end if;

   --  Transform the .ali filename into the binder output filename.

   Make_Binder_Filenames : declare
      Fname     : String  := Base_Name (Ali_Filename.all);
      Fname_Len : Integer := Fname'Length;

      function Get_Maximum_File_Name_Length return Integer;
      pragma Import (C, Get_Maximum_File_Name_Length,
                        "Get_Maximum_File_Name_Length");

      Maximum_File_Name_Length : Integer
        := Get_Maximum_File_Name_Length;

   begin
      Binder_Src_File := new String'("b_" & Fname & ".c");

      --  If the length of the binder object file becomes too long due to
      --  the addition of the "b_" prefix, then truncate it.
      --  This is only an issue on VMS, where Get_Maximum_File_Name_Length
      --  returns the maximum object file length, which is less
      --  than the actual maximum file name length.

      if Maximum_File_Name_Length > 0
        and then Fname_Len > Maximum_File_Name_Length - 2
      then
         Fname_Len := Maximum_File_Name_Length - 2;
      end if;

      Binder_Obj_File := new String'("b_" & Fname (1 .. Fname_Len)
                                      & Get_Object_Suffix.all);

      if Fname_Len /= Fname'Length then
         Binder_Options.Increment_Last;
         Binder_Options.Table (Binder_Options.Last) := new String'("-o");
         Binder_Options.Increment_Last;
         Binder_Options.Table (Binder_Options.Last) := Binder_Obj_File;
      end if;

   end Make_Binder_Filenames;

   Process_Binder_File (Binder_Src_File.all & Ascii.Nul);

   --  Compile the binder file. This is fast, so might as well do it always

   Bind_Step : declare
      Success : Boolean;
      Args    : Argument_List (1 .. Binder_Options.Last + 1);

   begin
      for J in Binder_Options.First .. Binder_Options.Last loop
         Args (J) := Binder_Options.Table (J);
      end loop;

      Args (Args'Last) := Binder_Src_File;

      if Verbose_Mode then
         Write_Str (Base_Name (Gcc_Path.all));

         for J in Args'Range loop
            Write_Str (" ");
            Write_Str (Args (J).all);
         end loop;

         Write_Eol;
      end if;

      GNAT.OS_Lib.Spawn (Gcc_Path.all, Args, Success);

      if not Success then
         Exit_Program (E_Fatal);
      end if;
   end Bind_Step;

   --  This is the very last argument to add to the linker objects.

   Linker_Objects.Increment_Last;
   Linker_Objects.Table (Linker_Objects.Last) := new String'("-lgnat");

   --  Now, actually link the program.

   Link_Step : declare
      Success  : Boolean;
      Num_Args : constant Integer :=
                  (Linker_Options.Last - Linker_Options.First + 1) +
                  (Linker_Objects.Last - Linker_Objects.First + 1);
      Args     : Argument_List (1 .. Num_Args + 1);
      Index    : Integer := Args'First;

   begin
      Args (Index) := Binder_Obj_File;

      for J in Linker_Objects.First .. Linker_Objects.Last loop
         Index := Index + 1;
         Args (Index) := Linker_Objects.Table (J);
      end loop;

      for J in Linker_Options.First .. Linker_Options.Last loop
         Index := Index + 1;
         Args (Index) := Linker_Options.Table (J);
      end loop;

      if Verbose_Mode then
         Write_Str (Base_Name (Linker_Path.all));

         for J in Args'Range loop
            Write_Str (" ");
            Write_Str (Args (J).all);
         end loop;

         Write_Eol;
      end if;

      GNAT.OS_Lib.Spawn (Linker_Path.all, Args, Success);

      if not Success then
         Exit_Program (E_Fatal);
      end if;

   end Link_Step;

   --  Only keep the binder output file and it's associated object
   --  file if compiling with the -g option.  These files are only
   --  useful if debugging.

   if not Debug_Flag_Present then
      Delete (Binder_Src_File.all & Ascii.Nul);
      Delete (Binder_Obj_File.all & Ascii.Nul);
   end if;

   Exit_Program (E_Success);

exception
   when others =>
      Exit_With_Error ("INTERNAL ERROR. Please report.");
end Gnatlink;
