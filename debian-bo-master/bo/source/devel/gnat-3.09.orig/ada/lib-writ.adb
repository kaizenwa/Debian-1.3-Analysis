------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . W R I T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.77 $                             --
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
with Fname;    use Fname;
with Hostparm;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Gnatvsn;  use Gnatvsn;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Stringt;  use Stringt;
with System;   use System;
with Uname;    use Uname;

package body Lib.Writ is

   ------------------------------
   -- Increment_Def_Func_Count --
   ------------------------------

   function Increment_Def_Func_Count return Nat is
      Ref_Unit : constant Unit_Number_Type :=
                   Units.Table (Current_Sem_Unit).Serial_Ref_Unit;

      TSN : Int renames Units.Table (Ref_Unit).Def_Func_Count;

   begin
      TSN := TSN + 1;
      return TSN;
   end Increment_Def_Func_Count;

   -----------------------------
   -- Increment_Serial_Number --
   -----------------------------

   function Increment_Serial_Number return Nat is
      TSN : Int renames Units.Table (Current_Sem_Unit).Serial_Number;

   begin
      TSN := TSN + 1;
      return TSN;
   end Increment_Serial_Number;

   --------------------------------
   -- Store_Linker_Option_String --
   --------------------------------

   procedure Store_Linker_Option_String (S : String_Id) is
   begin
      Linker_Option_Lines.Increment_Last;
      Linker_Option_Lines.Table (Linker_Option_Lines.Last) := S;
   end Store_Linker_Option_String;

   ------------------------
   -- Write_Library_Info --
   ------------------------

   procedure Write_Library_Info is

      -----------------------------------
      -- Format of Library Information --
      -----------------------------------

      --  This section  describes the format of the library information that is
      --  associated with object files. The exact method of this association is
      --  potentially implementation dependent and is described and implemented
      --  in package From the point of view of the description here, all
      --  we need to know is that the information is represented as a string of
      --  characters that is somehow associated with an object file, and can be
      --  retrieved. If no library information exists for a given object file,
      --  then we take this as equivalent to the non-existence of the object
      --  file, as if source file has not been previously compiled.

      --  The library information is written as a series of lines of the form:

      --    Key_Character parameter parameter ...

      --  The first two lines in the file identify the library output version
      --  and standard version (these are required to be consistent across the
      --  entire set of compilation units).

      --    V "xxxxxxxxxxxxxxxx"
      --
      --      This line indicates the library output version, as defined in
      --      Gnatvsn. It ensures that separate object modules of a program are
      --      consistent. It has to be changed if anything changes which would
      --      affect successful binding of separately compiled modules.
      --      Examples of such changes are modifications in the format of the
      --      library info described in this package, or modifications to
      --      calling sequences, or to the way that data is represented.

      --    S "xxxxxxxxxxxxxxxx"
      --
      --      This line contains information regarding types declared in
      --      packages Standard, System as stored in Gnatvsn.Standard_Version.
      --      The purpose is, on systems where for example the size of Integer
      --      can be set by command line switches, to ensure that all units in
      --      a program are compiled with a consistent set of options.

      --  The next line is present only for a unit that can be a main program
      --  It has the form:

      --    M type [priority] [T=time-slice]

      --      The type parameter is either P for a parameterless procedure,
      --      or F for a function returning a value of integral type (the
      --      latter is for writing a main program that returns an exit status)
      --      The priority parameter is present only if there was a valid
      --      pragma Priority in the corresponding unit to set the main task
      --      priority. It is an unsigned decimal integer. The time slice
      --      parameter is present only if there was a valid pragma Time_Slice
      --      in the corresponding unit (it is an unsigned decimal integer
      --      in the range 0 .. 10**9 giving the time slice in milliseconds).
      --
      --      Note: it would be neater if the priority parameter had the
      --      format P=priority, but it does not seem worth while to create
      --      a bootstrap path problem etc by changing the format that much.

      --    A argument

      --      One of these lines appears for each of the arguments present
      --      in the call to the gnat1 program. This can be used if it is
      --      necessary to reconstruct this call (e.g. for fix and continue)

      --      Note: as of 3.00, still not activated ???

      --    P [L=x] [Q=x] [T=x]

      --      Present if the unit uses tasking directly or indirectly and
      --      has one or more valid xxx_Policy pragmas that apply to the unit.
      --      The arguments are as follows:
      --
      --        L=x  Indicates that a valid Locking_Policy pragma applies to
      --             the unit, where x is the first character (upper case) of
      --             the policy name (e.g. 'C' for Ceiling_Locking)
      --
      --        Q=x  Indicates that a valid Queuing_Policy pragma applies to
      --             the unit, where x is the first character (upper case) of
      --             the policy name (e.g. 'P' for Priority_Queuing).
      --
      --        T=x  Indicates that a valid Task_Dispatching_Policy pragma
      --             applies to the unit, where x is the first character
      --             (upper case) of the policy name (e.g. 'F' for
      --             FIFO_Within_Priorities).

      --      Note that language defined units never output a P line (all
      --      language defined units must correctly handle all possible cases).
      --      These values are checked for consistency by the binder and then
      --      copied to the generated binder output file.

      --  Following these header lines, a set of information lines appears for
      --  each compilation unit that appears in the corresponding object file.
      --  In particular, when a package body or subprogram body is compiled,
      --  there will be two sets of information, one for the spec and one for
      --  the body. with the entry for the body appearing first. This is the
      --  only case in which a single ALI file contains more than one unit (in
      --  particular note that subunits do *not* count as compilation units for
      --  this purpose, and generate no library information, since they are
      --  inlined).

      --  The lines for each compilation unit have the following form.

      --    U unit-name source-name version <<attributes>>
      --
      --      This line identifies the unit to which this section of the
      --      library information file applies. The first three parameters are
      --      the unit name in internal format, as described in package Uname,
      --      and the name of the source file containing the unit.
      --
      --      Version is the version given as eight hexadecimal characters
      --      with upper case letters. This value is the exclusive or of the
      --      source checksums of the unit and all its semantically dependent
      --      units.
      --
      --      The <<attributes>> are a series of two letter codes indicating
      --      information about the unit:
      --
      --         EB  Unit has pragma Elaborate_Body
      --
      --         NE  Unit has no elaboration routine. All subprogram bodies
      --             and specs are in this category. Package bodies and specs
      --             may or may not have NE set, depending on whether or not
      --             elaboration code is required. Set if Has_No_Elab_Code
      --             flag is set in the N_Compilation_Unit node.
      --
      --         PK  Unit is package, rather than a subprogram
      --
      --         PU  Unit has pragma Pure
      --
      --         PR  Unit has pragma Preelaborate
      --
      --         RC  Unit has pragma Remote_Call_Interface
      --
      --         RT  Unit has pragma Remote_Types
      --
      --         SP  Unit has pragma Shared_Passive.
      --
      --         SU  Unit is a subprogram, rather than a package
      --
      --      The attributes may appear in any order, separated by spaces.

      --    W unit-name [source-name lib-name [E] [EA]]
      --
      --      One of these lines is present for each unit that is mentioned in
      --      an explicit with clause by the current unit. The first parameter
      --      is the unit name in internal format. The second parameter is the
      --      file name of the file that must be compiled to compile this unit
      --      (which is usually the file for the body, except for packages
      --      which have no body). The third parameter is the file name of the
      --      library information file that contains the results of compiling
      --      this unit. The E and EA parameters are present if the pragmas
      --      Elaborate and Elaborate_All respectively apply to this unit. In
      --      the case of generic units, only the first parameter is present,
      --      since generic units do not need to be compiled, and generate no
      --      library information. Note that the elaborate pragmas can be given
      --      for generic units, but they are ignored.

      --  Following the unit information is an optional series of lines that
      --  indicates the usage of pragma Library_Unit. For each appearence of
      --  pragma Library_Unit in any of the units for which unit lines are
      --  present, a line of the form:

      --    L "string"

      --  where string is the string from the unit line enclosed in quotes.
      --  Within the quotes the following can occur:

      --    7-bit graphic characters other than " or {
      --    "" (indicating a single " character)
      --    {hh} indicating a character whose code is hex hh

      --  For further details, see Stringt.Write_String_Table_Entry. Note that
      --  wide characters in the form {hhhh} cannot be produced, since pragma
      --  Linker_Option accepts only String, not Wide_String.

      --  Finally at the end of the ali file is a series of lines that
      --  indicates the source files on which the compiled units depend. This
      --  is used by the binder for consistency checking.

      --    D source-name time-stamp checksum optional-comments

      --  The time-stamp field contains the time stamp of the corresponding
      --  source file. See types.ads for details on time stamp representation.

      --  The checksum is an 8-hex digit representation of the source file
      --  checksum, with letters given in upper case.

      --  The optional comments field, if present, must be separated from the
      --  checksum by at least one blank. Currently the optional-comments
      --  field is not used.

      --  Note: blank lines are ignored when the library information is read,
      --  and separate sections of the file are separated by blank lines to
      --  ease readability. Blanks between fields are also ignored.

      ----------------
      -- Local Data --
      ----------------

      Info_Buffer : String (1 .. 2 * Hostparm.Max_Name_Length + 64);
      --  Info_Buffer used to prepare lines of library output

      Info_Buffer_Len : Natural;
      --  Number of characters stored in Info_Buffer

      Info_Buffer_Col : Natural;
      --  Column number of next character to be written (can be different from
      --  Info_Buffer_Len because of tab characters written by Write_Info_Tab)

      With_Flags : array (Units.First .. Units.Last) of Boolean;
      --  Array of flags used to show which units are with'ed

      Elab_Flags : array (Units.First .. Units.Last) of Boolean;
      --  Array of flags used to show which units have pragma Elaborate set

      Elab_All_Flags : array (Units.First .. Units.Last) of Boolean;
      --  Array of flags used to show which units have pragma Elaborate All set

      -----------------------
      -- Local Subprograms --
      -----------------------

      procedure Collect_Withs (Cunit : Node_Id);
      --  Collect with lines for entries in the context clause of the
      --  given compilation unit, Cunit.

      procedure Write_Info_Char (C : Character);
      pragma Inline (Write_Info_Char);
      --  Adds one character to Info_Buffer

      procedure Write_Info_Hex (W : Word);
      --  Writes out 8 hex digits (lower case letters), corresponding to the
      --  value of the parameter Word.

      procedure Write_Info_Initiate (Key : Character);
      --  Initiates write of new line to info file, the parameter is the
      --  keyword character for the line.

      procedure Write_Info_Nat (N : Nat);
      --  Adds image of N to Info_Buffer with no leading or trailing blanks

      procedure Write_Info_Name (Name : Name_Id);
      --  Adds characters of Name to Info_Buffer

      procedure Write_Info_Str (Val : String);
      --  Adds characters of Val to Info_Buffer surrounded by quotes

      procedure Write_Info_Tab (Col : Natural);
      --  Tab out with blanks and HT's to column Col. If already at or past
      --  Col, writes a single blank, so that we do get a required field
      --  separation.

      procedure Write_Info_Terminate;
      --  Terminate output of info line built in Info_Buffer

      procedure Write_Unit_Information (Unit_Num : Unit_Number_Type);
      --  Write out the library information for one unit for which code is
      --  generated (includes unit line and with lines).

      procedure Write_With_Lines;
      --  Write out with lines collected by calls to Collect_Withs

      -------------------
      -- Collect_Withs --
      -------------------

      procedure Collect_Withs (Cunit : Node_Id) is
         Item : Node_Id;
         Unum : Unit_Number_Type;

      begin
         Item := First (Context_Items (Cunit));
         while Present (Item) loop

            if Nkind (Item) = N_With_Clause then
               Unum := Get_Cunit_Unit_Number (Library_Unit (Item));
               With_Flags (Unum) := True;

               if Elaborate_Present (Item) then
                  Elab_Flags (Unum) := True;
               end if;

               if Elaborate_All_Present (Item) then
                  Elab_All_Flags (Unum) := True;
               end if;
            end if;

            Item := Next (Item);
         end loop;
      end Collect_Withs;

      ---------------------
      -- Write_Info_Char --
      ---------------------

      procedure Write_Info_Char (C : Character) is
      begin
         Info_Buffer_Len := Info_Buffer_Len + 1;
         Info_Buffer (Info_Buffer_Len) := C;
         Info_Buffer_Col := Info_Buffer_Col + 1;
      end Write_Info_Char;

      --------------------
      -- Write_Info_Hex --
      --------------------

      procedure Write_Info_Hex (W : Word) is
         H : constant array (Word range 0 .. 15) of Character :=
                                                         "0123456789abcdef";
         V : Word := W;

      begin
         for J in reverse Info_Buffer_Len + 1 .. Info_Buffer_Len + 8 loop
            Info_Buffer (J) := H (V mod 16);
            V := V / 16;
         end loop;

         Info_Buffer_Len := Info_Buffer_Len + 8;
         Info_Buffer_Col := Info_Buffer_Col + 8;
      end Write_Info_Hex;

      -------------------------
      -- Write_Info_Initiate --
      -------------------------

      procedure Write_Info_Initiate (Key : Character) is
      begin
         Info_Buffer_Len := 0;
         Info_Buffer_Col := 1;
         Write_Info_Char (Key);
         Write_Info_Char (' ');
      end Write_Info_Initiate;

      --------------------
      -- Write_Info_Nat --
      --------------------

      procedure Write_Info_Nat (N : Nat) is
      begin
         if N > 9 then
            Write_Info_Nat (N / 10);
         end if;

         Write_Info_Char (Character'Val (N mod 10 + Character'Pos ('0')));
      end Write_Info_Nat;

      ---------------------
      -- Write_Info_Name --
      ---------------------

      procedure Write_Info_Name (Name : Name_Id) is
      begin
         Get_Name_String (Name);
         Info_Buffer (Info_Buffer_Len + 1 .. Info_Buffer_Len + Name_Len) :=
           Name_Buffer (1 .. Name_Len);
         Info_Buffer_Len := Info_Buffer_Len + Name_Len;
         Info_Buffer_Col := Info_Buffer_Col + Name_Len;
      end Write_Info_Name;

      --------------------
      -- Write_Info_Str --
      --------------------

      procedure Write_Info_Str (Val : String) is
      begin
         Info_Buffer (Info_Buffer_Len + 1 .. Info_Buffer_Len + Val'Length)
                                                                     := Val;
         Info_Buffer_Len := Info_Buffer_Len + Val'Length;
         Info_Buffer_Col := Info_Buffer_Col + Val'Length;
      end Write_Info_Str;

      --------------------
      -- Write_Info_Tab --
      --------------------

      procedure Write_Info_Tab (Col : Natural) is
         Next_Tab : Natural;

      begin
         if Col <= Info_Buffer_Col then
            Write_Info_Str ("  ");
         else
            loop
               Next_Tab := 8 * ((Info_Buffer_Col - 1) / 8) + 8 + 1;
               exit when Col < Next_Tab;
               Write_Info_Char (Ascii.HT);
               Info_Buffer_Col := Next_Tab;
            end loop;

            while Info_Buffer_Col < Col loop
               Write_Info_Char (' ');
            end loop;
         end if;
      end Write_Info_Tab;

      --------------------------
      -- Write_Info_Terminate --
      --------------------------

      procedure Write_Info_Terminate is
      begin
         --  Delete any trailing blanks

         while Info_Buffer_Len > 0
           and then Info_Buffer (Info_Buffer_Len) = ' '
         loop
            Info_Buffer_Len := Info_Buffer_Len - 1;
         end loop;

         Write_Library_Info (Info_Buffer (1 .. Info_Buffer_Len));
         Info_Buffer_Len := 0;
      end Write_Info_Terminate;

      ----------------------------
      -- Write_Unit_Information --
      ----------------------------

      procedure Write_Unit_Information (Unit_Num : Unit_Number_Type) is
         Ukind : constant Node_Kind := Nkind (Unit (Cunit (Unit_Num)));
         Pnode : Node_Id;

      begin
         Write_Info_Initiate ('U');
         Write_Info_Name (Unit_Name (Unit_Num));
         Write_Info_Tab (25);
         Write_Info_Name (Unit_File_Name (Unit_Num));

         Write_Info_Tab (49);
         Write_Info_Str (Version_Get (Unit_Num));

         if Is_Preelaborated (Cunit_Entity (Unit_Num)) then
            Write_Info_Str ("  PR");
         end if;

         if Has_No_Elab_Code (Cunit (Unit_Num)) then
            Write_Info_Str ("  NE");
         end if;

         if Elaborate_Body_Present (Cunit (Unit_Num)) then
            Write_Info_Str ("  EB");
         end if;

         if Is_Pure (Cunit_Entity (Unit_Num)) then
            Write_Info_Str ("  PU");
         end if;

         if Is_Remote_Call_Interface (Cunit_Entity (Unit_Num)) then
            Write_Info_Str ("  RC");
         end if;

         if Is_Remote_Types (Cunit_Entity (Unit_Num)) then
            Write_Info_Str ("  RT");
         end if;

         if Is_Shared_Passive (Cunit_Entity (Unit_Num)) then
            Write_Info_Str ("  SP");
         end if;

         if Ukind = N_Subprogram_Declaration
           or else Ukind = N_Subprogram_Body
         then
            Write_Info_Str ("  SU");

         elsif Ukind = N_Package_Declaration
           or else Ukind = N_Package_Body
         then
            Write_Info_Str ("  PK");
         end if;

         Write_Info_Terminate;

         --  Generate with lines, first those that are directly with'ed

         for J in With_Flags'Range loop
            With_Flags (J) := False;
            Elab_Flags (J) := False;
            Elab_All_Flags (J) := False;
         end loop;

         Collect_Withs (Cunit (Unit_Num));

         --  For a body, we must also check for any subunits which belong to
         --  us and which have context clauses of their own, since these
         --  with'ed units our part of our elaboration dependencies.

         if Nkind (Unit (Cunit (Unit_Num))) in N_Unit_Body then
            for S in Units.First .. Units.Last loop

               --  We are only interested in subunits

               if Nkind (Unit (Cunit (S))) = N_Subunit then
                  Pnode := Library_Unit (Cunit (S));

                  --  Find ultimate parent of the subunit

                  while Nkind (Unit (Pnode)) = N_Subunit loop
                     Pnode := Library_Unit (Pnode);
                  end loop;

                  --  See if it belongs to us, and if so, include it's with's

                  if Pnode = Cunit (Unit_Num) then
                     Collect_Withs (Cunit (S));
                  end if;
               end if;
            end loop;
         end if;

         Write_With_Lines;
      end Write_Unit_Information;

      ----------------------
      -- Write_With_Lines --
      ----------------------

      procedure Write_With_Lines is
         With_Table : Unit_Ref_Table (1 .. Pos (Units.Last - Units.First + 1));
         Num_Withs  : Int := 0;
         Cunit      : Node_Id;
         Uname      : Unit_Name_Type;
         Fname      : File_Name_Type;

      begin
         --  Loop to build the with table. A with on the main unit itself
         --  is ignored (AARM 10.2(14a)). Such a with-clause can occur if
         --  the main unit is a subprogram with no spec, and a subunit of
         --  it unecessarily withs the parent.

         for J in Units.First + 1 .. Units.Last loop
            if With_Flags (J) then
               Num_Withs := Num_Withs + 1;
               With_Table (Num_Withs) := J;
            end if;
         end loop;

         --  Sort and output the table

         Sort (With_Table (1 .. Num_Withs));

         for J in 1 .. Num_Withs loop
            Cunit := Units.Table (With_Table (J)).Cunit;
            Uname := Units.Table (With_Table (J)).Unit_Name;
            Fname := Units.Table (With_Table (J)).Unit_File_Name;

            Write_Info_Initiate ('W');
            Write_Info_Name (Uname);

            --  Now we need to figure out the names of the files that contain
            --  the with'ed unit. These will usually be the files for the body,
            --  except except in the case of a package that has no body, as
            --  indicated by the Body_Required flag in the compilation unit
            --  node not being set. No names are output for a generic unit.

            if Nkind (Unit (Cunit)) not in N_Generic_Declaration
              and then Nkind (Unit (Cunit)) not in
                                      N_Generic_Renaming_Declaration
            then
               Write_Info_Tab (25);

               if Body_Required (Cunit) then
                  Write_Info_Name (Get_File_Name (Get_Body_Name (Uname)));
                  Write_Info_Tab (49);
                  Write_Info_Name
                    (Lib_File_Name (Get_File_Name (Get_Body_Name (Uname))));
               else
                  Write_Info_Name (Fname);
                  Write_Info_Tab (49);
                  Write_Info_Name (Lib_File_Name (Fname));
               end if;

               if Elab_Flags (With_Table (J)) then
                  Write_Info_Str ("  E");
               end if;

               if Elab_All_Flags (With_Table (J)) then
                  Write_Info_Str ("  EA");
               end if;
            end if;
            Write_Info_Terminate;
         end loop;
      end Write_With_Lines;

      ----------
      -- Writ --
      ----------

   begin
      Create_Output_Library_Info;

      --  Output version line

      Write_Info_Initiate ('V');
      Write_Info_Char ('"');
      Write_Info_Str (Library_Version);
      Write_Info_Char ('"');
      Write_Info_Terminate;

      --  Output standard version line

      Write_Info_Initiate ('S');
      Write_Info_Char ('"');
      Write_Info_Str (Standard_Version);
      Write_Info_Char ('"');
      Write_Info_Terminate;

      --  Output main program line if this is acceptable main program

      declare
         U : constant Node_Id := Unit (Units.Table (Main_Unit).Cunit);
         S : Node_Id;

      begin
         if Nkind (U) = N_Subprogram_Body
           or else (Nkind (U) = N_Package_Body
                      and then
                        (Nkind (Original_Node (U)) = N_Function_Instantiation
                           or else
                         Nkind (Original_Node (U)) =
                                                  N_Procedure_Instantiation))
         then
            --  If the unit is a subprogram instance, the entity for the
            --  subprogram is the last visible one in the package spec,
            --  appearing after the renamings for the generic actuals.

            if Nkind (U) = N_Package_Body then
               S := Specification (Last (Visible_Declarations
                           (Specification
                             (Unit (Library_Unit (Parent (U)))))));
            else
               S := Specification (U);
            end if;

            if not Present (Parameter_Specifications (S)) then
               if Nkind (S) = N_Procedure_Specification then
                  Write_Info_Initiate ('M');
                  Write_Info_Char ('P');

               else
                  declare
                     Nam : Node_Id := Defining_Unit_Name (S);

                  begin
                     --  if it is a child unit, get its simple name.

                     if Nkind (Nam) = N_Defining_Program_Unit_Name then
                        Nam := Defining_Identifier (Nam);
                     end if;

                     if Is_Integer_Type (Etype (Nam)) then
                        Write_Info_Initiate ('M');
                        Write_Info_Char ('F');
                     end if;
                  end;
               end if;

               if Main_Priority (Main_Unit) /= Default_Main_Priority then
                  Write_Info_Char (' ');
                  Write_Info_Nat (Main_Priority (Main_Unit));
               end if;

               if Opt.Time_Slice_Set then
                  Write_Info_Char (' ');
                  Write_Info_Char ('T');
                  Write_Info_Char ('=');
                  Write_Info_Nat (Opt.Time_Slice_Value);
               end if;

               Write_Info_Terminate;
            end if;
         end if;
      end;

      --  Output tasking policy line if needed

      if Tasking_Used
        and then not Is_Predefined_File_Name (Unit_File_Name (Main_Unit))
        and then (Queuing_Policy /= ' '
                    or else
                  Locking_Policy /= ' '
                    or else
                  Task_Dispatching_Policy /= ' ')
      then
         Write_Info_Initiate ('P');

         if Locking_Policy /= ' ' then
            Write_Info_Str  ("L=");
            Write_Info_Char (Locking_Policy);
            Write_Info_Char (' ');
         end if;

         if Queuing_Policy /= ' ' then
            Write_Info_Str  ("Q=");
            Write_Info_Char (Queuing_Policy);
            Write_Info_Char (' ');
         end if;

         if Task_Dispatching_Policy /= ' ' then
            Write_Info_Str  ("T=");
            Write_Info_Char (Task_Dispatching_Policy);
            Write_Info_Char (' ');
         end if;

         Write_Info_Terminate;
      end if;

      --  Loop through file table to output information for all units for which
      --  we have generated code, as marked by the Generate_Code flag.

      for Unit in Units.First .. Units.Last loop
         if Units.Table (Unit).Generate_Code then
            Write_Info_Terminate; -- blank line
            Write_Unit_Information (Unit);
         end if;
      end loop;

      Write_Info_Terminate; -- blank line

      --  Output linker option lines

      for J in 1 .. Linker_Option_Lines.Last loop
         declare
            S : constant String_Id := Linker_Option_Lines.Table (J);
            C : Character;

         begin
            Write_Info_Initiate ('L');
            Write_Info_Char ('"');

            for J in 1 .. String_Length (S) loop
               C := Get_Character (Get_String_Char (S, J));

               if C in Character'Val (16#20#) .. Character'Val (16#7E#)
                 and then C /= '{'
               then
                  Write_Info_Char (C);
               end if;

               if C = '"' then
                  Write_Info_Char (C);
               end if;
            end loop;

            Write_Info_Char ('"');
            Write_Info_Terminate;
         end;
      end loop;

      --  Prepare to output the source dependency lines

      declare
         Sdep_Table : Unit_Ref_Table (1 .. Pos (Units.Last - Units.First + 1));
         --  Keeps track of sdep entries

         Num_Sdep : Nat := 0;
         --  Number of active entries in Sdep_Table

         Sind : Source_File_Index;
         --  Index of corresponding source file

      begin
         for Unit in Units.First .. Units.Last loop
            Num_Sdep := Num_Sdep + 1;
            Sdep_Table (Num_Sdep) := Unit;
         end loop;

         Lib.Sort (Sdep_Table (1 .. Num_Sdep));

         for J in 1 .. Num_Sdep loop
            Sind := Units.Table (Sdep_Table (J)).Source_Index;
            Write_Info_Initiate ('D');
            Write_Info_Name (File_Name (Sind));
            Write_Info_Tab (25);
            Write_Info_Str (String (Time_Stamp (Sind)));
            Write_Info_Char (' ');
            Write_Info_Str (Get_Hex_String (Source_Checksum (Sind)));
            Write_Info_Terminate;
         end loop;
      end;

      Close_Output_Library_Info;

   end Write_Library_Info;

end Lib.Writ;
