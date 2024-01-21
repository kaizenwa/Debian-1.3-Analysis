------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F R O N T E N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.35 $                             --
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
with Elists;
with Errout;
with Fname;
with Inline;   use Inline;
with Lib;      use Lib;
with Lib.Load;
with Namet;    use Namet;
with Nlists;
with Opt;      use Opt;
with Output;   use Output;
with Par;
with Rtsfind;
with Sprint;
with Scn;      use Scn;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with CStand;
with Treepr;
with Types;    use Types;
with Usage;

procedure Frontend is
begin
   --  Carry out package initializations. These are initializations which
   --  might logically be performed at elaboration time, were it not for
   --  the fact that we may be doing things more than once in the big loop
   --  over files. Like elaboration, the order in which these calls are
   --  made is in some cases important. For example, Lib cannot be
   --  initialized until Namet, since it uses names table entries.

   Rtsfind.Initialize;
   Atree.Initialize;
   Nlists.Initialize;
   Elists.Initialize;
   Lib.Load.Initialize;
   Sem_Ch8.Initialize;
   Fname.Initialize;

   --  Create package Standard

   CStand.Create_Standard;

   --  Initialize the scanner. Note that we do this after the call to
   --  Create_Standard, which uses the scanner in its processing of
   --  floating-point bounds.

   Initialize_Scanner (Main_Unit, Source_Index (Main_Unit));

   --  Output header if in verbose mode or full list mode

   if Verbose_Mode or Full_List then
      Write_Eol;

      if Operating_Mode = Generate_Code then
         Write_Str ("Compiling: ");
      else
         Write_Str ("Checking: ");
      end if;

      Write_Name (Full_File_Name (Current_Source_File));

      if not Debug_Flag_7 then
         Write_Str (" (source file time stamp: ");
         Write_Time_Stamp (Current_Source_File);
         Write_Char (')');
      end if;

      Write_Eol;
   end if;

   --  Here we call the parser to parse the compilation unit (or units in
   --  the check syntax mode, but in that case we won't go on to the
   --  semantics in any case).

   declare
      Discard : List_Id;

   begin
      Discard := Par (Configuration_Pragmas => False);
   end;

   --  The main unit is now loaded, and subunits of it can be loaded,
   --  without reporting spurious loading circularities.

   Set_Loading (Main_Unit, False);

   --  Now on to the semantics. We skip the semantics if we are in syntax
   --  only mode, or if we encountered a fatal error during the parsing.

   if Operating_Mode /= Check_Syntax
     and then not Fatal_Error (Main_Unit)
   then
      --  Reset Operating_Mode to Check_Semantics for subunits. We cannot
      --  actually generate code for subunits, so we suppress expansion.
      --  This also corrects certain problems that occur if we try to
      --  incorporate subunits at a lower level.

      if Operating_Mode = Generate_Code
         and then Nkind (Unit (Cunit (Main_Unit))) = N_Subunit
      then
         Operating_Mode := Check_Semantics;
      end if;

      --  Analyze (and possibly expand) main unit

      Scope_Suppress := Suppress_Options;
      Semantics (Cunit (Main_Unit));

      --  Instantiate generic bodies and inlined bodies

      Instantiate_Bodies;

      if Inline_Active then
         Analyze_Inlined_Bodies;
      end if;

      if List_Units then
         Lib.List;
      end if;
   end if;

   Treepr.Tree_Dump;
   Sprint.Source_Dump;

end Frontend;
