------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T C M D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.17 $                             --
--                                                                          --
--            Copyright (C) 1996 Free Software Foundation, Inc.             --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;

with System.Storage_Elements; use System.Storage_Elements;

with Hostparm; use Hostparm;
--  Used to determine if we are in VMS or not for error message purposes

with GNAT.OS_Lib;             use GNAT.OS_Lib;

procedure GNATCmd is

   ------------------
   -- SWITCH TABLE --
   ------------------

   --  The switch tables contain an entry for each switch recognized by the
   --  command processor. The syntax of entries is as follows:

   --    SWITCH_STRING ::= "/ command-qualifier-name TRANSLATION"

   --    TRANSLATION ::=
   --      DIRECT_TRANSLATION
   --    | DIRECTORIES_TRANSLATION
   --    | FILE_TRANSLATION
   --    | NUMERIC_TRANSLATION
   --    | OPTIONS_TRANSLATION
   --    | COMMANDS_TRANSLATION

   --    DIRECT_TRANSLATION      ::= space UNIX_SWITCHES
   --    DIRECTORIES_TRANSLATION ::= =* UNIX_SWITCH *
   --    FILE_TRANSLATION        ::= =@ UNIX_SWITCH @
   --    NUMERIC_TRANSLATION     ::= =# UNIX_SWITCH #
   --    OPTIONS_TRANSLATION     ::= =OPTION {space OPTION}
   --    COMMANDS_TRANSLATION    ::= =? ARGS space command-name

   --    UNIX_SWITCHES ::= UNIX_SWITCH {, UNIX_SWITCH}

   --    UNIX_SWITCH ::= unix-switch-string | !unix-switch-string

   --    OPTION ::= option-name space UNIX_SWITCHES

   --    ARGS ::= -cargs | -bargs | -largs

   --  Here command-qual is the name of the switch recognized by the GNATCmd.
   --  This is always given in upper case in the templates, although in the
   --  actual commands, either upper or lower case is allowed.

   --  The unix-switch-string always starts with a minus, and has no commas
   --  or spaces in it. Case is significant in the unix switch string. If a
   --  unix switch string is preceded by the not sign (!) it means that the
   --  effect of the corresponding command qualifer is to remove any previous
   --  occurrence of the given switch in the command line.

   --  The DIRECTORIES_TRANSLATION format is used where a list of directories
   --  is given. This possible corresponding formats recognized by GNATCmd are
   --  as shown by the following example for the case of PATH

   --    PATH=direc
   --    PATH=(direc,direc,direc,direc)

   --  When more than one directory is present for the DIRECTORIES case, then
   --  multiple instances of the corresponding unix switch are generated,
   --  with the file name being substituted for the occurrence of *.

   --  The FILE_TRANSLATION format is similar except that only a single
   --  file is allowed, not a list of files, and only one unix switch is
   --  generated as a result.

   --  The NUMERIC_TRANSLATION format is similar to the FILE_TRANSLATION case
   --  except that the parameter is a decimal integer in the range 0 to 999.

   --  For the OPTIONS_TRANSLATION case, GNATCmd similarly permits one or
   --  more options to appear (although only in some cases does the use of
   --  multiple options make logical sense). For example, taking the
   --  case of ERRORS for GCC, the following are all allowed:

   --    /ERRORS=BRIEF
   --    /ERRORS=(FULL,VERBOSE)
   --    /ERRORS=(BRIEF IMMEDIATE)

   --  If no option is provided (e.g. just /ERRORS is written), then the
   --  first option in the list is the default option. For /ERRORS this
   --  is NORMAL, so /ERRORS with no option is equivalent to /ERRORS=NORMAL.

   --  The COMMANDS_TRANSLATION case is only used for gnatmake, to correspond
   --  to the use of -cargs, -bargs and -largs (the ARGS string as indicated
   --  is one of these three possibilities). The name given by COMMAND is the
   --  corresponding command name to be used to interprete the switches to be
   --  passed on. Switches of this type set modes, e.g. /COMPILER_SWITCHES
   --  sets the mode so that all subsequent switches, up to another switch
   --  with COMMANDS_TRANSLATION apply to the corresponding commands issued
   --  by the make utility. For example

   --    /COMPILER_SWITCHES /LIST /BINDER_SWITCHES /MAIN
   --    /COMPILER_SWITCHES /NOLIST /COMPILE_CHECKS=SYNTAX

   --  Clearly these switches must come at the end of the list of switches
   --  since all subsequent switches apply to an issued command.

   --  For the DIRECT_TRANSLATION case, an implicit additional entry is
   --  created by prepending NO to the name of the qualifer, and then
   --  inverting the sense of the UNIX_SWITCHES string. For example,
   --  given the entry:

   --     "/LIST -gnatl"

   --  An implicit entry is created:

   --     "/NOLIST !-gnatl"

   --  In the case where, a ! is already present, inverting the sense of the
   --  switch means removing it.

   subtype S is String;
   --  A synonym to shorten the table

   type String_Ptr is access constant String;
   --  String pointer type used throughout

   type Switches is array (Natural range <>) of String_Ptr;
   --  Type used for array of swtiches

   type Switches_Ptr is access constant Switches;

   ----------------------------
   -- Switches for GNAT BIND --
   ----------------------------

   S_Bind_Dep    : aliased constant S := "/ELABORATION_DEPENDENCIES "      &
                                            "-e";

   S_Bind_Horrid : aliased constant S := "/PESSIMISTIC_ELABORATION_ORDER " &
                                            "-h";

   S_Bind_Elab   : aliased constant S := "/ORDER_OF_ELABORATION "          &
                                            "-l";

   S_Bind_Main   : aliased constant S := "/MAIN "                          &
                                            "!-n";

   S_Bind_Rdsrc  : aliased constant S := "/READ_SOURCES="                  &
                                            "ALL "                         &
                                               "-s "                       &
                                            "NONE "                        &
                                               "-x "                       &
                                            "AVAILABLE "                   &
                                               "!-x,!-s";

   S_Bind_Nosrc  : aliased constant S := "/NOREAD_SOURCES "                &
                                            "-x";

   S_Bind_Stamp  : aliased constant S := "/TIME_STAMP_CHECK "              &
                                            "!-t";

   S_Bind_Warn   : aliased constant S := "/WARNINGS="                      &
                                            "NORMAL "                      &
                                               "!-ws,!-we "                &
                                            "SUPPRESS "                    &
                                               "-ws "                      &
                                            "ERROR "                       &
                                               "-we";

   S_Bind_Nowarn : aliased constant S := "/NOWARNINGS "                    &
                                            "-ws";

   S_Bind_Rep    : aliased constant S := "/REPORT_ERRORS="                 &
                                            "VERBOSE "                     &
                                               "-v "                       &
                                            "BRIEF "                       &
                                               "-b "                       &
                                            "DEFAULT "                     &
                                               "!-b,!-v";

   S_Bind_Norep  : aliased constant S := "/NOREPORT_ERRORS "               &
                                            "!-b,!-v";

   S_Bind_Verb   : aliased constant S := "/VERBOSE "                       &
                                            "-v";

   S_Bind_Curdir : aliased constant S := "/CURRENT_DIRECTORY_SEARCH "      &
                                            "!-I-";

   S_Bind_Lpath  : aliased constant S := "/LIBRARY_SEARCH=*"               &
                                            "-aOdir*";

   S_Bind_Spath  : aliased constant S := "/SOURCE_SEARCH=*"                &
                                            "-aIdir*";

   S_Bind_Path   : aliased constant S := "/SEARCH=*"                       &
                                            "-I*";

   S_Bind_Output : aliased constant S := "/EXECUTABLE=@"                   &
                                            "-o@";

   S_Bind_Noexec : aliased constant S := "/NOEXECUTABLE "                  &
                                            "-c";

   S_Bind_Debug  : aliased constant S := "/DEBUG="                         &
                                            "TRACEBACK "                   &
                                               "-g2 "                      &
                                            "ALL "                         &
                                               "-g3 "                      &
                                            "NONE "                        &
                                               "-g0 "                      &
                                            "SYMBOLS "                     &
                                               "-g1 "                      &
                                            "NOSYMBOLS "                   &
                                               "!-g1 "                     &
                                            "LINK "                        &
                                               "-g3 "                      &
                                            "NOTRACEBACK "                 &
                                               "!-g2";

   S_Bind_Nodug  : aliased constant S := "/NODEBUG "                       &
                                            "!-g";

   Bind_Switches : aliased constant Switches := (
     S_Bind_Dep    'Access,
     S_Bind_Horrid 'Access,
     S_Bind_Elab   'Access,
     S_Bind_Main   'Access,
     S_Bind_Rdsrc  'Access,
     S_Bind_Nosrc  'Access,
     S_Bind_Stamp  'Access,
     S_Bind_Warn   'Access,
     S_Bind_Nowarn 'Access,
     S_Bind_Rep    'Access,
     S_Bind_Norep  'Access,
     S_Bind_Verb   'Access,
     S_Bind_Curdir 'Access,
     S_Bind_Lpath  'Access,
     S_Bind_Spath  'Access,
     S_Bind_Path   'Access,
     S_Bind_Output 'Access,
     S_Bind_Noexec 'Access,
     S_Bind_Debug  'Access);

   ----------------------------
   -- Switches for GNAT CHOP --
   ----------------------------

   S_Chop_Flimit : aliased constant S := "/FILE_NAME_MAX_LENGTH=#"         &
                                            "-k#";

   S_Chop_Ref    : aliased constant S := "/REFERENCE "                     &
                                            "-r";

   S_Chop_Script : aliased constant S := "/SCRIPT "                        &
                                            "-s";

   S_Chop_Over   : aliased constant S := "/OVERWRITE "                     &
                                            "-w";

   Chop_Switches : aliased constant Switches := (
     S_Chop_Flimit 'Access,
     S_Chop_Ref    'Access,
     S_Chop_Script 'Access,
     S_Chop_Over   'Access);

   -------------------------------
   -- Switches for GNAT COMPILE --
   -------------------------------

   S_GCC_Rep     : aliased constant S := "/REPORT_ERRORS="                 &
                                            "VERBOSE "                     &
                                               "-gnatv "                   &
                                            "BRIEF "                       &
                                               "-gnatb "                   &
                                            "FULL "                        &
                                               "-gnatf "                   &
                                            "IMMEDIATE "                   &
                                               "-gnate "                   &
                                            "DEFAULT "                     &
                                               "!-gnatb,!-gnatv";

   S_GCC_Norep   : aliased constant S := "/NOREPORT_ERRORS "               &
                                            "!-gnatb,!-gnatv";

   S_GCC_Verb    : aliased constant S := "/VERBOSE "                       &
                                            "-v";

   S_GCC_Check   : aliased constant S := "/SYNTAX_ONLY "                   &
                                            "-gnats";

   S_GCC_Noload  : aliased constant S := "/NOLOAD "                        &
                                            "-gnatc";

   S_GCC_Ident   : aliased constant S := "/IDENTIFIER_CHARACTER_SET="      &
                                             "DEFAULT "                    &
                                                "-gnati1 "                 &
                                             "1 "                          &
                                                "-gnati1 "                 &
                                             "2 "                          &
                                                "-gnati2 "                 &
                                             "3 "                          &
                                                "-gnati3 "                 &
                                             "4 "                          &
                                                "-gnati4 "                 &
                                             "PC "                         &
                                                "-gnatip "                 &
                                             "PC850 "                      &
                                                "-gnati8 "                 &
                                             "FULL_UPPER "                 &
                                                "-gnatif "                 &
                                             "NO_UPPER "                   &
                                                "-gnatin "                 &
                                             "WIDE "                       &
                                                "-gnatiw";

   S_GCC_Noident : aliased constant S := "/NOIDENTIFIER_CHARACTER_SET "    &
                                             "-gnati1";

   S_GCC_Wide    : aliased constant S := "/WIDE_CHARACTER_ENCODING="       &
                                             "BRACKETS "                   &
                                                "-gnatWb "                 &
                                             "NONE "                       &
                                                "-gnatWn "                 &
                                             "HEX "                        &
                                                "-gnatWh "                 &
                                             "UPPER "                      &
                                                "-gnatWu "                 &
                                             "SHIFT_JIS "                  &
                                                "-gnatWs "                 &
                                             "EUC "                        &
                                                "-gnatWe";

   S_GCC_Nowide  : aliased constant S := "/NOWIDE_CHARACTER_ENCODING "     &
                                            "-gnatWn";

   S_GCC_Flimit  : aliased constant S := "/FILE_NAME_MAX_LENGTH=#"         &
                                            "-gnatk#";

   S_GCC_List    : aliased constant S := "/LIST "                          &
                                            "-gnatl";

   S_GCC_Elimit  : aliased constant S := "/ERROR_LIMIT=#"                  &
                                            "-gnatm#";

   S_GCC_Noelim  : aliased constant S := "/NOERROR_LIMIT "                 &
                                            "-gnatm999";

   S_GCC_Inline  : aliased constant S := "/INLINE_SUBPROGRAMS "            &
                                            "-gnatn,-O3";

   S_GCC_Checks  : aliased constant S := "/CHECKS="                        &
                                             "FULL "                       &
                                                "-gnato,!-gnatp "          &
                                             "ASSERTIONS "                 &
                                                "-gnata "                  &
                                             "DEFAULT "                    &
                                                "!-gnato,!-gnatp "         &
                                             "SUPPRESS_ALL "               &
                                                "-gnatp,!-gnato";

   S_GCC_Nochks  : aliased constant S := "/NOCHECKS "                      &
                                             "-gnatp,!-gnato";

   S_GCC_Trys    : aliased constant S := "/TRY_SEMANTICS "                 &
                                            "-gnatq";

   S_GCC_Style   : aliased constant S := "/STYLE_CHECKS="                  &
                                            "REFERENCE_MANUAL "            &
                                               "-gnatr "                   &
                                            "NONE "                        &
                                               "!-gnatg,!-gnatr "          &
                                            "GNAT "                        &
                                               "-gnatg";

   S_GCC_Nostyle : aliased constant S := "/NOSTYLE_CHECKS "                &
                                            "!-gnatg,!-gnatr";

   S_GCC_Tree    : aliased constant S := "/TREE_OUTPUT "                   &
                                            "-gnatt";

   S_GCC_Units   : aliased constant S := "/UNITS_LIST "                    &
                                            "-gnatu";

   S_GCC_Warn    : aliased constant S := "/WARNINGS="                      &
                                            "DEFAULT "                     &
                                               "!-gnatws,!-gnatwe "        &
                                            "SUPPRESS "                    &
                                               "-gnatws "                  &
                                            "ERROR "                       &
                                               "-gnatwe "                  &
                                            "UNINITIALIZED "               &
                                               "-Wuninitialized "          &
                                            "ALL_GCC "                     &
                                               "-Wall";

   S_GCC_Nowarn  : aliased constant S := "/NOWARNINGS "                    &
                                            "-gnatws";

   S_GCC_Feat    : aliased constant S := "/FEATURES "                      &
                                               "-gnatx9";

   S_GCC_Dist    : aliased constant S := "/DISTRIBUTION_STUBS="            &
                                            "RECEIVER "                    &
                                               "-gnatzr "                  &
                                            "SENDER "                      &
                                               "-gnatzs";

   S_GCC_Nodist  : aliased constant S := "/NODISTRIBUTION_STUBS "          &
                                            "!-gnatzr,!-gnatzs";

   S_GCC_Ada_83  : aliased constant S := "/83 "                            &
                                            "-gnat83";

   S_GCC_Ada_95  : aliased constant S := "/95 "                            &
                                            "!-gnat83";

   S_GCC_Debug   : aliased constant S := "/DEBUG="                         &
                                            "TRACEBACK "                   &
                                               "-g2 "                      &
                                            "ALL "                         &
                                               "-g3 "                      &
                                            "NONE "                        &
                                               "-g0 "                      &
                                            "SYMBOLS "                     &
                                               "-g1 "                      &
                                            "NOSYMBOLS "                   &
                                               "!-g1 "                     &
                                            "LINK "                        &
                                               "-g3 "                      &
                                            "NOTRACEBACK "                 &
                                               "!-g2";

   S_GCC_Nodebug : aliased constant S := "/NODEBUG "                       &
                                            "!-g";

   S_GCC_Opt     : aliased constant S := "/OPTIMIZE="                      &
                                            "ALL "                         &
                                               "-O2,!-O0,!-O1,!-O3 "       &
                                            "NONE "                        &
                                               "-O0,!-O1,!-O2,!-O3 "       &
                                            "SOME "                        &
                                               "-O1,!-O0,!-O2,!-O3 "       &
                                            "DEVELOPMENT "                 &
                                               "-O1,!-O0,!-O2,!-O3 "       &
                                            "UNROLL_LOOPS "                &
                                               "-funroll-loops "           &
                                            "INLINING "                    &
                                               "-O3,-gnatn,!-O0,!-O1,!-O2";

   S_GCC_Noopt   : aliased constant S := "/NOOPTIMIZE "                    &
                                            "-O0,!-O1,!-O2,!-O3";

   S_GCC_Asm     : aliased constant S := "/ASM "                           &
                                            "-S,!-c";

   S_GCC_Path    : aliased constant S := "/SEARCH=*"                       &
                                            "-I*";

   S_GCC_Curdir  : aliased constant S := "/CURRENT_DIRECTORY_SEARCH "      &
                                            "!-I-";

   GCC_Switches : aliased constant Switches := (
     S_GCC_Rep     'Access,
     S_GCC_Norep   'Access,
     S_GCC_Verb    'Access,
     S_GCC_Check   'Access,
     S_GCC_Noload  'Access,
     S_GCC_Ident   'Access,
     S_GCC_Noident 'Access,
     S_GCC_Wide    'Access,
     S_GCC_Nowide  'Access,
     S_GCC_Flimit  'Access,
     S_GCC_List    'Access,
     S_GCC_Elimit  'Access,
     S_GCC_Noelim  'Access,
     S_GCC_Inline  'Access,
     S_GCC_Checks  'Access,
     S_GCC_Nochks  'Access,
     S_GCC_Trys    'Access,
     S_GCC_Style   'Access,
     S_GCC_Nostyle 'Access,
     S_GCC_Tree    'Access,
     S_GCC_Units   'Access,
     S_GCC_Warn    'Access,
     S_GCC_Nowarn  'Access,
     S_GCC_Feat    'Access,
     S_GCC_Dist    'Access,
     S_GCC_Nodist  'Access,
     S_GCC_Ada_83  'Access,
     S_GCC_Ada_95  'Access,
     S_GCC_Debug   'Access,
     S_GCC_Nodebug 'Access,
     S_GCC_Opt     'Access,
     S_GCC_Noopt   'Access,
     S_GCC_Asm     'Access,
     S_GCC_Path    'Access,
     S_GCC_Curdir  'Access);

   ----------------------------
   -- Switches for GNAT LINK --
   ----------------------------

   S_Link_Output : aliased constant S := "/EXECUTABLE=@"                   &
                                            "-o@";

   S_Link_Verb   : aliased constant S := "/VERBOSE "                       &
                                            "-v";

   S_Link_Debug  : aliased constant S := "/DEBUG="                         &
                                            "ALL "                         &
                                               "-g3 "                      &
                                            "NONE "                        &
                                               "-g0 "                      &
                                            "TRACEBACK "                   &
                                               "-g2 "                      &
                                            "NOTRACEBACK "                 &
                                               "-g0";

   S_Link_Other  : aliased constant S := "/<other> "                       &
                                            "--for-linker=";

   Link_Switches : aliased constant Switches := (
      S_Link_Output 'Access,
      S_Link_Verb   'Access,
      S_Link_Debug  'Access,
      S_Link_Other  'Access);

   ----------------------------
   -- Switches for GNAT MAKE --
   ----------------------------

   S_Make_All    : aliased constant S := "/ALL_FILES "                     &
                                            "-a";

   S_Make_Nolink : aliased constant S := "/NOLINK "                        &
                                            "-c";

   S_Make_Force  : aliased constant S := "/FORCE_COMPILE "                 &
                                            "-f";

   S_Make_Jobs   : aliased constant S := "/PROCESSES=#"                    &
                                            "-j#";

   S_Make_Nojobs : aliased constant S := "/NOPROCESSES "                   &
                                            "-j1";

   S_Make_Cont   : aliased constant S := "/CONTINUE_ON_ERROR "             &
                                            "-k";

   S_Make_Dep    : aliased constant S := "/DEPENDENCIES_LIST "             &
                                            "-M";

   S_Make_Objchk : aliased constant S := "/DO_OBJECT_CHECK "               &
                                            "-n";

   S_Make_Out    : aliased constant S := "/EXECUTABLE=@"                   &
                                            "-o@";

   S_Make_Quiet  : aliased constant S := "/QUIET "                         &
                                            "-q";

   S_Make_Reason : aliased constant S := "/REASONS "                       &
                                            "-v";

   S_Make_Skip   : aliased constant S := "/SKIP_MISSING=*"                 &
                                            "-aL*";

   S_Make_Curdir : aliased constant S := "/CURRENT_DIRECTORY_SEARCH "      &
                                            "!-I-";

   S_Make_Lib    : aliased constant S := "/LIBRARY_SEARCH=*"               &
                                            "-L*";

   S_Make_Source : aliased constant S := "/SOURCE_SEARCH=*"                &
                                            "-aI*";

   S_Make_Conss  : aliased constant S := "/CONDITIONAL_SOURCE_SEARCH=*"    &
                                            "-A*";

   S_Make_Obj    : aliased constant S := "/OBJECT_SEARCH=*"                &
                                            "-aO*";

   S_Make_Path   : aliased constant S := "/SEARCH=*"                       &
                                            "-I*";

   S_Make_Comp   : aliased constant S := "/COMPILER_SWITCHES=?"            &
                                            "-cargs COMPILE";

   S_Make_Bind   : aliased constant S := "/BINDER_SWITCHES=?"              &
                                            "-bargs BIND";

   S_Make_Link   : aliased constant S := "/LINKER_SWITCHES=?"              &
                                            "-largs LINK";

   S_Make_Verb   : aliased constant S := "/VERBOSE "                       &
                                            "-v";

   Make_Switches : aliased constant Switches := (
     S_Make_All    'Access,
     S_Make_Nolink 'Access,
     S_Make_Force  'Access,
     S_Make_Jobs   'Access,
     S_Make_Nojobs 'Access,
     S_Make_Cont   'Access,
     S_Make_Dep    'Access,
     S_Make_Objchk 'Access,
     S_Make_Out    'Access,
     S_Make_Quiet  'Access,
     S_Make_Reason 'Access,
     S_Make_Skip   'Access,
     S_Make_Curdir 'Access,
     S_Make_Lib    'Access,
     S_Make_Source 'Access,
     S_Make_Conss  'Access,
     S_Make_Obj    'Access,
     S_Make_Path   'Access,
     S_Make_Comp   'Access,
     S_Make_Bind   'Access,
     S_Make_Link   'Access,
     S_Make_Verb   'Access);

   ----------------------------------
   -- Switches for GNAT PREPROCESS --
   ----------------------------------

   S_Prep_Com    : aliased constant S := "/COMMENTS "                      &
                                            "-c,!-b";

   S_Prep_Blank  : aliased constant S := "/BLANK_LINES "                   &
                                            "-b";

   S_Prep_Remove : aliased constant S := "/REMOVE "                        &
                                            "!-b,!-c";

   S_Prep_Ref    : aliased constant S := "/REFERENCE "                     &
                                            "-r";

   S_Prep_Ver    : aliased constant S := "/VERSION "                       &
                                            "-v";

   S_Prep_Sym    : aliased constant S := "/SYMBOLS "                       &
                                            "-s";

   S_Prep_Verb   : aliased constant S := "/VERBOSE "                       &
                                            "-v";

   Prep_Switches : aliased constant Switches := (
     S_Prep_Com    'Access,
     S_Prep_Blank  'Access,
     S_Prep_Remove 'Access,
     S_Prep_Ref    'Access,
     S_Prep_Ver    'Access,
     S_Prep_Sym    'Access,
     S_Prep_Verb   'Access);

   ----------------------------
   -- Switches for GNAT XREF --
   ----------------------------

   S_XRF_Rep     : aliased constant S := "/REPORT_ERRORS="                 &
                                            "VERBOSE "                     &
                                               "-gnatv "                   &
                                            "BRIEF "                       &
                                               "-gnatb "                   &
                                            "FULL "                        &
                                               "-gnatf "                   &
                                            "IMMEDIATE "                   &
                                               "-gnate "                   &
                                            "NORMAL "                      &
                                               "!-gnatb,!-gnatv";

   S_XRF_Norep   : aliased constant S := "/NOREPORT_ERRORS "               &
                                            "!-gnatb,!-gnatv";

   S_XRF_Verb    : aliased constant S := "/VERBOSE "                       &
                                            "-v";

   S_XRF_Check   : aliased constant S := "/SYNTAX_ONLY "                   &
                                            "-gnats";

   S_XRF_Ident   : aliased constant S := "/IDENTIFIER_CHARACTER_SET="      &
                                             "DEFAULT "                    &
                                                "-gnati1 "                 &
                                             "1 "                          &
                                                "-gnati1 "                 &
                                             "2 "                          &
                                                "-gnati2 "                 &
                                             "3 "                          &
                                                "-gnati3 "                 &
                                             "4 "                          &
                                                "-gnati4 "                 &
                                             "PC "                         &
                                                "-gnatip "                 &
                                             "PC850 "                      &
                                                "-gnati8 "                 &
                                             "FULL_UPPER "                 &
                                                "-gnatif "                 &
                                             "NO_UPPER "                   &
                                                "-gnatin "                 &
                                             "WIDE "                       &
                                                "-gnatiw";

   S_XRF_Noident : aliased constant S := "/NOIDENTIFIER_CHARACTER_SET "    &
                                             "-gnati1";

   S_XRF_Wide    : aliased constant S := "/WIDE_CHARACTER_ENCODING="       &
                                             "BRACKETS "                   &
                                                "-gnatWb "                 &
                                             "NONE "                       &
                                                "-gnatWn "                 &
                                             "HEX "                        &
                                                "-gnatWh "                 &
                                             "UPPER "                      &
                                                "-gnatWu "                 &
                                             "SHIFT_JIS "                  &
                                                "-gnatWs "                 &
                                             "EUC "                        &
                                                "-gnatWe";

   S_XRF_Nowide  : aliased constant S := "/NOWIDE_CHARACTER_ENCODING "     &
                                            "-gnatWn";

   S_XRF_Flimit  : aliased constant S := "/FILE_NAME_MAX_LENGTH=#"         &
                                            "-gnatk#";

   S_XRF_List    : aliased constant S := "/LIST "                          &
                                            "-gnatl";

   S_XRF_Elimit  : aliased constant S := "/ERROR_LIMIT=#"                  &
                                            "-gnatm#";

   S_XRF_Noelim  : aliased constant S := "/NOERROR_LIMIT "                 &
                                            "-gnatm999";

   S_XRF_Trys    : aliased constant S := "/TRY_SEMANTICS "                 &
                                            "-gnatq";

   S_XRF_Style   : aliased constant S := "/STYLE_CHECKS="                  &
                                            "REFERENCE_MANUAL "            &
                                               "-gnatr "                   &
                                            "NONE "                        &
                                               "!-gnatg,!-gnatr "          &
                                            "GNAT "                        &
                                               "-gnatg";

   S_XRF_Nostyle : aliased constant S := "/NOSTYLE_CHECKS "                &
                                            "!-gnatg,!-gnatr";

   S_XRF_Units   : aliased constant S := "/UNITS_LIST "                    &
                                            "-gnatu";

   S_XRF_Warn    : aliased constant S := "/WARNINGS="                      &
                                            "NORMAL "                      &
                                               "!-gnatws,!-gnatwe "        &
                                            "SUPPRESS "                    &
                                               "-gnatws "                  &
                                            "ERROR "                       &
                                               "-gnatwe";

   S_XRF_Nowarn  : aliased constant S := "/NOWARNINGS "                    &
                                            "-gnatws";

   S_XRF_Level   : aliased constant S := "/XREF="                          &
                                            "ALL "                         &
                                               "-gnatx5 "                  &
                                            "NONE "                        &
                                               "-gnatx0 "                  &
                                            "UNUSED_WITHS "                &
                                               "-gnatx1 "                  &
                                            "UNUSED_ENTITIES "             &
                                               "-gnatx2 "                  &
                                            "UNUSED "                      &
                                               "-gnatx2 "                  &
                                            "COMPILED "                    &
                                               "-gnatx3 "                  &
                                            "BODIES "                      &
                                               "-gnatx4 "                  &
                                            "GLOBAL_FILE "                 &
                                               "-gnatx6";

   S_XRF_Feat    : aliased constant S := "/FEATURES "                      &
                                            "-gnatx9";


   S_XRF_Ada_83  : aliased constant S := "/83 "                            &
                                            "-gnat83";

   S_XRF_Ada_95  : aliased constant S := "/95 "                            &
                                            "!-gnat83";

   S_XRF_Path    : aliased constant S := "/SEARCH=*"                       &
                                            "-I*";

   S_XRF_Curdir  : aliased constant S := "/CURRENT_DIRECTORY_SEARCH "      &
                                            "!-I-";

   XRF_Switches : aliased constant Switches := (
     S_XRF_Rep     'Access,
     S_XRF_Norep   'Access,
     S_XRF_Verb    'Access,
     S_XRF_Check   'Access,
     S_XRF_Ident   'Access,
     S_XRF_Noident 'Access,
     S_XRF_Wide    'Access,
     S_XRF_Nowide  'Access,
     S_XRF_Flimit  'Access,
     S_XRF_List    'Access,
     S_XRF_Elimit  'Access,
     S_XRF_Noelim  'Access,
     S_XRF_Trys    'Access,
     S_XRF_Style   'Access,
     S_XRF_Nostyle 'Access,
     S_XRF_Units   'Access,
     S_XRF_Warn    'Access,
     S_XRF_Nowarn  'Access,
     S_XRF_Level   'Access,
     S_XRF_Feat    'Access,
     S_XRF_Ada_83  'Access,
     S_XRF_Ada_95  'Access,
     S_XRF_Path    'Access,
     S_XRF_Curdir  'Access);

   -------------------
   -- COMMAND TABLE --
   -------------------

   --  The command table contains an entry for each command recognized by
   --  GNATCmd. The entries are represented by an array of records.

   type Command_Entry is record

      Cname : String_Ptr;
      --  Command name for GNAT xxx command

      Usage : String_Ptr;
      --  A usage string, used for error messages

      Unixcmd  : String_Ptr;
      --  Corresponding Unix command

      Switches : Switches_Ptr;
      --  Pointer to array of switch strings

      Minfile : Natural;
      --  Minimum number of file/directory parameters for command

      Maxfile : Natural;
      --  Maximum number of file/directory parameters for command (zero means
      --  that an arbitrary number of such parameters may appear).

      Defext : String (1 .. 3);
      --  Default extension. If non-blank, then this extension is supplied by
      --  default as the extension for any file parameter which does not have
      --  an extension already.

   end record;

   Command_List : array (Natural range <>) of Command_Entry := (

      (Cname    => new S'("BIND"),
       Usage    => new S'("GNAT BIND file[.ali] /qualifiers"),
       Unixcmd  => new S'("gnatbind"),
       Switches => Bind_Switches'Access,
       Minfile  => 1,
       Maxfile  => 0,
       Defext   => "ali"),

      (Cname    => new S'("CHOP"),
       Usage    => new S'("GNAT CHOP file [directory] /qualifiers"),
       Unixcmd  => new S'("gnatchop"),
       Switches => Chop_Switches'Access,
       Minfile  => 1,
       Maxfile  => 2,
       Defext   => "   "),

      (Cname    => new S'("COMPILE"),
       Usage    => new S'("GNAT COMPILE file file .. file /qualifiers"),
       Unixcmd  => new S'("gcc -c"),
       Switches => GCC_Switches'Access,
       Minfile  => 1,
       Maxfile  => 0,
       Defext   => "   "),

      (Cname    => new S'("LINK"),
       Usage    => new S'("GNAT LINK file[.ali] [extra objs] /qualifiers"),
       Unixcmd  => new S'("gnatlink"),
       Switches => Link_Switches'Access,
       Minfile  => 1,
       Maxfile  => 0,
       Defext   => "ali"),

      (Cname    => new S'("MAKE"),
       Usage    =>
         new S'("GNAT MAKE file /qualifiers (includes COMPILE /qualifiers)"),
       Unixcmd  => new S'("gnatmake"),
       Switches => Make_Switches'Access,
       Minfile  => 1,
       Maxfile  => 1,
       Defext   => "   "),

      (Cname    => new S'("PREPROCESS"),
       Usage    => new S'("GNAT PREPROCESS ifile ofile dfile /qualifiers"),
       Unixcmd  => new S'("gnatprep"),
       Switches => Prep_Switches'Access,
       Minfile  => 3,
       Maxfile  => 3,
       Defext   => "   "),

      (Cname    => new S'("XREF"),
       Usage    => new S'("GNAT XREF file file .. file /qualifiers"),
       Unixcmd  => new S'("gnatf -x5"),
       Switches => XRF_Switches'Access,
       Minfile  => 1,
       Maxfile  => 0,
       Defext   => "   ")
   );

   -------------------------
   -- INTERNAL STRUCTURES --
   -------------------------

   --  The switches and commands are defined by strings in the previous
   --  section so that they are easy to modify, but internally, they are
   --  kept in a more conveniently accessible form described in this
   --  section.

   --  Commands, command qualifers and options have a similar common format
   --  so that searching for matching names can be done in a common manner.

   type Item_Id is (Id_Command, Id_Switch, Id_Option);

   type Translation_Type is
     (T_Direct, T_Directories, T_File, T_Numeric, T_Options, T_Commands,
      T_Other);

   type Item (Id : Item_Id);
   type Item_Ptr is access all Item;

   subtype Command_Item is Item (Id_Command);
   subtype Switch_Item  is Item (Id_Switch);
   subtype Option_Item  is Item (Id_Option);

   type Item (Id : Item_Id) is record
      Name : String_Ptr;
      --  Name of the command, switch (with slash) or option

      Next : Item_Ptr;
      --  Pointer to next item on list, always has the same Id value

      Unix_String : String_Ptr;
      --  Corresponding Unix string. For a command, this is the unix command
      --  name and possible default switches. For a switch or option it is
      --  the unix switch string.

      case Id is

         when Id_Command =>

            Switches : Item_Ptr;
            --  Pointer to list of switch items for the command, linked
            --  through the Next fields with null terminating the list.

            Usage : String_Ptr;
            --  Usage information, used only for errors and the default
            --  list of commands output.

            Minfile : Natural;
            --  Minimum number of file/directory parameters for command

            Maxfile : Natural;
            --  Maximum number of file/directory parameters for command
            --  (zero means that an arbitrary number are allowed).

            Defext : String (1 .. 3);
            --  Default extension. If non-blank, then this extension is
            --  supplied by default as the extension for any file parameter
            --  which does not have an extension already.

         when Id_Switch =>

            Translation : Translation_Type;
            --  Type of switch translation. For all cases, except Options,
            --  this is the only field needed, since the Unix translation
            --  is found in Unix_String.

            Options : Item_Ptr;
            --  For the Options case, this field is set to point to a list
            --  of options item (for this case Unix_String is null in the
            --  main switch item). The end of the list is marked by null.

         when Id_Option =>

            null;
            --  No special fields needed, since Name and Unix_String are
            --  sufficient to completely described an option.

      end case;
   end record;

   ----------------------------------
   -- Declarations for GNATCMD use --
   ----------------------------------

   Commands : Item_Ptr;
   --  Pointer to head of list of command items, one for each command, with
   --  the end of the list marked by a null pointer.

   Last_Command : Item_Ptr;
   --  Pointer to last item in Commands list

   Normal_Exit : exception;
   --  Raise this exception for normal program termination

   Error_Exit : exception;
   --  Raise this exception if error detected

   Errors : Natural := 0;
   --  Count errors detected

   Command : Item_Ptr;
   --  Pointer to command item for current command

   Make_Commands_Active : Item_Ptr := null;
   --  Set to point to Command entry for COMPILE, BIND, or LINK as appropriate
   --  if a COMMANDS_TRANSLATION switch has been encountered while processing
   --  a MAKE Command.

   Buffer : String (1 .. 4096) := (others => ' ');
   --  Buffer used to build Unix style command

   Ptr : Natural := 0;
   --  Pointer to last used location in buffer

   File_Count : Natural := 0;
   --  Number of file/directory arguments so far

   Display_Command : Boolean := False;
   --  Set true if /? switch causes display of generated command

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Invert_Sense (S : String) return String_Ptr;
   --  Given a unix switch string S, computes the inverse (adding or
   --  removing ! characters as required), and returns a pointer to
   --  the allocated result on the heap.

   function Match (S1, S2 : String) return Boolean;
   --  Determines whether S1 and S2 match. This is a case insensitive match.

   function Match_Prefix (S1, S2 : String) return Boolean;
   --  Determines whether S1 matches a prefix of S2. This is also a case
   --  insensitive match (for example Match ("AB","abc") is True).

   function Matching_Name
     (S     : String;
      Itm   : Item_Ptr;
      Quiet : Boolean := False)
      return  Item_Ptr;
   --  Determines if the item list headed by Itm and threaded through the
   --  Next fields (with null marking the end of the list), contains an
   --  entry that uniquely matches the given string. The match is case
   --  insensitive and permits unique abbreviation. If the match succeeds,
   --  then a pointer to the matching item is returned. Otherwise, an
   --  appropriate error message is written. Note that the discriminant
   --  of Itm is used to determine the appropriate form of this message.
   --  Quiet is normally False as shown, if it is set to True, then no
   --  error message is generated in a not found situation (null is still
   --  returned to indicate the not-found situation).

   function OK_Integer (S : String) return Boolean;
   --  Checks that S is a string of digits, returning True if all digits,
   --  False if empty or a non-digit is present.

   procedure Place (C : Character);
   --  Place a single character in the buffer, updating Ptr

   procedure Place (S : String);
   --  Place a string character in the buffer, updating Ptr

   procedure Place_Lower (S : String);
   --  Place string in buffer, forcing letters to lower case, updating Ptr

   procedure Place_Unix_Switches (S : String_Ptr);
   --  Given a unix switch string, place corresponding switches in Buffer,
   --  updating Ptr appropriatelly. Note that in the case of use of ! the
   --  result may be to remove a previously placed switch.

   procedure Validate_Command_Or_Option (N : String_Ptr);
   --  Check that N is a valid command or option name, i.e. that it is of the
   --  form of an Ada identifier with upper case letters and underscores.

   procedure Validate_Unix_Switch (S : String_Ptr);
   --  Check that S is a valid switch string as described in the syntax
   --  for the switch table item UNIX_SWITCH.

   ------------------
   -- Invert_Sense --
   ------------------

   function Invert_Sense (S : String) return String_Ptr is
      Sinv : String (1 .. S'Length * 2);
      --  Result (for sure long enough)

      Sinvp : Natural := 0;
      --  Pointer to output string

   begin
      for Sp in S'Range loop
         if Sp = S'First or else S (Sp - 1) = ',' then
            if S (Sp) = '!' then
               null;
            else
               Sinv (Sinvp + 1) := '!';
               Sinv (Sinvp + 2) := S (Sp);
               Sinvp := Sinvp + 2;
            end if;

         else
            Sinv (Sinvp + 1) := S (Sp);
            Sinvp := Sinvp + 1;
         end if;
      end loop;

      return new String'(Sinv (1 .. Sinvp));
   end Invert_Sense;

   -----------
   -- Match --
   -----------

   function Match (S1, S2 : String) return Boolean is
      Dif : constant Integer := S2'First - S1'First;

   begin

      if S2 = "/<other>" then
         return True;

      elsif S1'Length /= S2'Length then
         return False;

      else
         for J in S1'Range loop
            if To_Lower (S1 (J)) /= To_Lower (S2 (J + Dif)) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end Match;

   -------------------
   -- Matching_Name --
   -------------------

   function Matching_Name
     (S     : String;
      Itm   : Item_Ptr;
      Quiet : Boolean := False)
      return  Item_Ptr
   is
      P1, P2 : Item_Ptr;

      procedure Err;
      --  Little procedure to output command/qualifier/option as appropriate
      --  and bump error count.

      procedure Err is
      begin
         if Quiet then
            return;
         end if;

         Errors := Errors + 1;

         case Itm.Id is
            when Id_Command =>
               Put (Standard_Error, "command");

            when Id_Switch =>
               if OpenVMS then
                  Put (Standard_Error, "qualifier");
               else
                  Put (Standard_Error, "switch");
               end if;

            when Id_Option =>
               Put (Standard_Error, "option");

         end case;

         Put (Standard_Error, ": ");
         Put (Standard_Error, S);

      end Err;

   --  Start of processing for Matching_Name

   begin
      --  If exact match, that's the one we want

      P1 := Itm;
      while P1 /= null loop
         if Match (S, P1.Name.all) then
            return P1;
         else
            P1 := P1.Next;
         end if;
      end loop;

      --  Now check for prefix matches

      P1 := Itm;
      while P1 /= null loop
         if not Match_Prefix (S, P1.Name.all) then
            P1 := P1.Next;

         else
            --  Here we have found one matching prefix, so see if there is
            --  another one (which is an ambiguity)

            P2 := P1.Next;
            while P2 /= null loop
               if Match_Prefix (S, P2.Name.all) then
                  if not Quiet then
                     Put (Standard_Error, "ambiguous ");
                     Err;
                     Put (Standard_Error, " (matches ");
                     Put (Standard_Error, P1.Name.all);

                     while P2 /= null loop
                        if Match_Prefix (S, P2.Name.all) then
                           Put (Standard_Error, ',');
                           Put (Standard_Error, P2.Name.all);
                        end if;

                        P2 := P2.Next;
                     end loop;

                     Put_Line (Standard_Error, ")");
                  end if;

                  return null;
               end if;

               P2 := P2.Next;
            end loop;

            --  If we fall through that loop, then there was only one match

            return P1;
         end if;
      end loop;

      --  If we fall through outer loop, there was no match

      if not Quiet then
         Put (Standard_Error, "unrecognized ");
         Err;
         New_Line (Standard_Error);
      end if;

      return null;
   end Matching_Name;

   ------------------
   -- Match_Prefix --
   ------------------

   function Match_Prefix (S1, S2 : String) return Boolean is
   begin
      if S1'Length > S2'Length then
         return False;

      else
         return Match (S1, S2 (S2'First .. S2'First + S1'Length - 1));
      end if;
   end Match_Prefix;

   ----------------
   -- OK_Integer --
   ----------------

   function OK_Integer (S : String) return Boolean is
   begin
      if S'Length = 0 then
         return False;

      else
         for J in S'Range loop
            if not Is_Digit (S (J)) then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end OK_Integer;

   -----------
   -- Place --
   -----------

   procedure Place (C : Character) is
   begin
      Ptr := Ptr + 1;
      Buffer (Ptr) := C;
   end Place;

   procedure Place (S : String) is
   begin
      for J in S'Range loop
         Place (S (J));
      end loop;
   end Place;

   -----------------
   -- Place_Lower --
   -----------------

   procedure Place_Lower (S : String) is
   begin
      for J in S'Range loop
         Place (To_Lower (S (J)));
      end loop;
   end Place_Lower;

   -------------------------
   -- Place_Unix_Switches --
   -------------------------

   procedure Place_Unix_Switches (S : String_Ptr) is
      P1, P2, P3 : Natural;
      Remove     : Boolean;
      Slen       : Natural;

   begin
      P1 := S'First;
      while P1 <= S'Last loop
         if S (P1) = '!' then
            P1 := P1 + 1;
            Remove := True;
         else
            Remove := False;
         end if;

         P2 := P1;
         pragma Assert (S (P1) = '-');
         while P2 < S'Last and then S (P2 + 1) /= ',' loop
            P2 := P2 + 1;
         end loop;

         --  Switch is now in S (P1 .. P2)

         Slen := P2 - P1 + 1;

         if Remove then
            P3 := 2;
            while P3 <= Ptr - Slen loop
               if Buffer (P3) = ' '
                 and then Buffer (P3 + 1 .. P3 + Slen) = S (P1 .. P2)
                 and then (P3 + Slen = Ptr
                             or else
                           Buffer (P3 + Slen + 1) = ' ')
               then
                  Buffer (P3 .. Ptr - Slen - 1) :=
                    Buffer (P3 + Slen + 1 .. Ptr);
                  Ptr := Ptr - Slen - 1;

               else
                  P3 := P3 + 1;
               end if;
            end loop;

         else
            Place (' ');
            Place (S (P1 .. P2));
         end if;

         P1 := P2 + 2;
      end loop;
   end Place_Unix_Switches;

   --------------------------------
   -- Validate_Command_Or_Option --
   --------------------------------

   procedure Validate_Command_Or_Option (N : String_Ptr) is
   begin
      pragma Assert (N'Length > 0);
      for J in N'Range loop
         if N (J) = '_' then
            pragma Assert (N (J - 1) /= '_');
            null;
         else
            pragma Assert (Is_Upper (N (J)) or else Is_Digit (N (J)));
            null;
         end if;
      end loop;
   end Validate_Command_Or_Option;

   --------------------------
   -- Validate_Unix_Switch --
   --------------------------

   procedure Validate_Unix_Switch (S : String_Ptr) is
   begin
      pragma Assert (S (S'First) = '-' or else S (S'First) = '!');
      for J in S'First + 1 .. S'Last loop
         pragma Assert (S (J) /= ' ');

         if S (J) = '!' then
            pragma Assert (S (J - 1) = ',' and then S (J + 1) = '-');
            null;
         end if;
      end loop;
   end Validate_Unix_Switch;

-------------------------------------
-- Start of processing for GNATCmd --
-------------------------------------

begin
   --  First we must preprocess the string form of the command and options
   --  list into the internal form that we use.

   for C in Command_List'Range loop

      declare
         Command : Item_Ptr := new Command_Item;

         Last_Switch : Item_Ptr;
         --  Last switch in list

      begin
         --  Link new command item into list of commands

         if Last_Command = null then
            Commands := Command;
         else
            Last_Command.Next := Command;
         end if;

         Last_Command := Command;

         --  Fill in fields of new command item

         Command.Name        := Command_List (C).Cname;
         Command.Usage       := Command_List (C).Usage;
         Command.Unix_String := Command_List (C).Unixcmd;
         Command.Minfile     := Command_List (C).Minfile;
         Command.Maxfile     := Command_List (C).Maxfile;
         Command.Defext      := Command_List (C).Defext;

         Validate_Command_Or_Option (Command.Name);

         --  Process the switch list

         for S in Command_List (C).Switches'Range loop
            declare
               SS : constant String_Ptr := Command_List (C).Switches (S);

               P  : Natural := SS'First;
               Sw : Item_Ptr := new Switch_Item;

               Last_Opt : Item_Ptr;
               --  Pointer to last option

            begin
               --  Link new switch item into list of switches

               if Last_Switch = null then
                  Command.Switches := Sw;
               else
                  Last_Switch.Next := Sw;
               end if;

               Last_Switch := Sw;

               --  Process switch string, first get name

               while SS (P) /= ' ' and SS (P) /= '=' loop
                  P := P + 1;
               end loop;

               Sw.Name := new String'(SS (SS'First .. P - 1));

               --  Direct translation case

               if SS (P) = ' ' then
                  Sw.Translation := T_Direct;
                  Sw.Unix_String := new String'(SS (P + 1 .. SS'Last));
                  Validate_Unix_Switch (Sw.Unix_String);

                  if SS (P - 1) = '>' then
                     Sw.Translation := T_Other;

                  --  Create the inverted case (/NO ..)

                  elsif SS (SS'First + 1 .. SS'First + 2) /= "NO" then
                     Sw := new Switch_Item;
                     Last_Switch.Next := Sw;
                     Last_Switch := Sw;

                     Sw.Name
                       := new String'("/NO" & SS (SS'First + 1 .. P - 1));
                     Sw.Translation := T_Direct;
                     Sw.Unix_String := Invert_Sense (SS (P + 1 .. SS'Last));
                     Validate_Unix_Switch (Sw.Unix_String);
                  end if;

               --  Directories translation case

               elsif SS (P + 1) = '*' then
                  pragma Assert (SS (SS'Last) = '*');
                  Sw.Translation := T_Directories;
                  Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                  Validate_Unix_Switch (Sw.Unix_String);

               --  File translation case

               elsif SS (P + 1) = '@' then
                  pragma Assert (SS (SS'Last) = '@');
                  Sw.Translation := T_File;
                  Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                  Validate_Unix_Switch (Sw.Unix_String);

               --  Numeric translation case

               elsif SS (P + 1) = '#' then
                  pragma Assert (SS (SS'Last) = '#');
                  Sw.Translation := T_Numeric;
                  Sw.Unix_String := new String'(SS (P + 2 .. SS'Last - 1));
                  Validate_Unix_Switch (Sw.Unix_String);

               --  Commands translation case

               elsif SS (P + 1) = '?' then
                  Sw.Translation := T_Commands;
                  Sw.Unix_String := new String'(SS (P + 2 .. SS'Last));

               --  Options translation case

               else
                  Sw.Translation := T_Options;
                  Sw.Unix_String := new String'("");

                  P := P + 1; -- bump past =
                  while P <= SS'Last loop
                     declare
                        Opt : Item_Ptr := new Option_Item;
                        Q   : Natural;

                     begin
                        --  Link new option item into options list

                        if Last_Opt = null then
                           Sw.Options := Opt;
                        else
                           Last_Opt.Next := Opt;
                        end if;

                        Last_Opt := Opt;

                        --  Fill in fields of new option item

                        Q := P;
                        while SS (Q) /= ' ' loop
                           Q := Q + 1;
                        end loop;

                        Opt.Name := new String'(SS (P .. Q - 1));
                        Validate_Command_Or_Option (Opt.Name);

                        P := Q + 1;
                        Q := P;

                        while Q <= SS'Last and then SS (Q) /= ' ' loop
                           Q := Q + 1;
                        end loop;

                        Opt.Unix_String := new String'(SS (P .. Q - 1));
                        Validate_Unix_Switch (Opt.Unix_String);
                        P := Q + 1;
                     end;
                  end loop;
               end if;
            end;
         end loop;
      end;
   end loop;

   --  If no parameters, give complete list of commands

   if Argument_Count = 0 then
      Put_Line ("List of available commands, qualifiers, and options");

      while Commands /= null loop
         New_Line;
         Put (Commands.Usage.all);
         Set_Col (53);
         Put_Line (Commands.Unix_String.all);

         declare
            Sw : Item_Ptr := Commands.Switches;

         begin
            while Sw /= null loop
               Put ("   ");
               Put (Sw.Name.all);

               case Sw.Translation is

                  when T_Other =>
                     Set_Col (53);
                     Put_Line (Sw.Unix_String.all & "<other>");

                  when T_Direct =>
                     Set_Col (53);
                     Put_Line (Sw.Unix_String.all);

                  when T_Directories =>
                     Put ("=(direc,direc,..direc)");
                     Set_Col (53);
                     Put (Sw.Unix_String.all);
                     Put (" direc ");
                     Put (Sw.Unix_String.all);
                     Put_Line (" direc ...");

                  when T_File =>
                     Put ("=file");
                     Set_Col (53);
                     Put (Sw.Unix_String.all);
                     Put_Line (" file ");

                  when T_Numeric =>
                     Put ("=nnn");
                     Set_Col (53);
                     Put (Sw.Unix_String.all);
                     Put_Line ("nnn");

                  when T_Commands =>
                     Put (" (switches for ");
                     Put (Sw.Unix_String (
                          Sw.Unix_String'First + 7 .. Sw.Unix_String'Last));
                     Put (')');
                     Set_Col (53);
                     Put (Sw.Unix_String (
                          Sw.Unix_String'First .. Sw.Unix_String'First + 5));
                     Put_Line (" switches");

                  when T_Options =>
                     declare
                        Opt : Item_Ptr := Sw.Options;

                     begin
                        Put_Line ("=(option,option..)");

                        while Opt /= null loop
                           Put ("      ");
                           Put (Opt.Name.all);

                           if Opt = Sw.Options then
                              Put (" (D)");
                           end if;

                           Set_Col (53);
                           Put_Line (Opt.Unix_String.all);
                           Opt := Opt.Next;
                        end loop;
                     end;

               end case;

               Sw := Sw.Next;
            end loop;
         end;

         Commands := Commands.Next;
      end loop;

      raise Normal_Exit;
   end if;

   --  We have arguments, so the first one must be a command name

   Command := Matching_Name (Argument (1), Commands);

   if Command = null then
      raise Error_Exit;
   end if;

   Place (Command.Unix_String.all);

   for C in 2 .. Argument_Count loop

      declare
         Argv    : String := Argument (C);
         Arg_Idx : Integer := Argv'First;

         function Get_Arg_End (Argv : String; Arg_Idx : Integer)
            return Integer;

         function Get_Arg_End (Argv : String; Arg_Idx : Integer)
            return Integer
         is
         begin
            for I in Arg_Idx + 1 .. Argv'Last loop
               if Argv (I) = '/' then
                  return I - 1;
               end if;
            end loop;

            return Argv'Last;
         end Get_Arg_End;

      begin

         loop

            declare
               Next_Arg_Idx : Integer := Get_Arg_End (Argv, Arg_Idx);
               Arg          : String := Argv (Arg_Idx .. Next_Arg_Idx);

            begin

               Arg_Idx := Next_Arg_Idx + 1;

               --  Special handling for internal debugging switch /?

               if Arg = "/?" then
                  Display_Command := True;

               --  Copy -switch unchanged

               elsif Arg (Arg'First) = '-' then
                  Place (' ');
                  Place (Arg);

               --  Copy quoted switch with quotes stripped

               elsif Arg (Arg'First) = '"' then
                  if Arg (Arg'Last) /= '"' then
                     Put (Standard_Error, "misquoted argument: ");
                     Put_Line (Standard_Error, Arg);
                     Errors := Errors + 1;

                  else
                     Put (Arg (Arg'First + 1 .. Arg'Last - 1));
                  end if;

               --  File or Directory argument

               elsif Arg (Arg'First) /= '/' then
                  File_Count := File_Count + 1;
                  Place (' ');
                  Place_Lower (Arg);

                  if Command.Defext /= "   " then
                     if Arg'Length < 5 or else Buffer (Ptr - 3) /= '.' then
                        Place ('.');
                        Place (Command.Defext);
                     end if;
                  end if;

               --  Switch argument

               else
                  declare
                     Sw   : Item_Ptr;
                     SwP  : Natural;
                     P2   : Natural;
                     Endp : Natural;
                     Opt  : Item_Ptr;

                  begin
                     SwP := Arg'First;
                     while SwP < Arg'Last and then Arg (SwP + 1) /= '=' loop
                        SwP := SwP + 1;
                     end loop;

                     --  At this point, the switch name is in
                     --  Arg (Arg'First..SwP) and if that is not the whole
                     --  switch, then there is an equal sign at
                     --  Arg (SwP + 1) and the rest of Arg is what comes
                     --  after the equal sign.

                     --  If make commands are active, see if we have another
                     --  COMMANDS_TRANSLATION switch belonging to gnatmake.

                     if Make_Commands_Active /= null then
                        Sw :=
                          Matching_Name
                            (Arg (Arg'First .. SwP),
                             Command.Switches,
                             Quiet => True);

                        if Sw /= null and then Sw.Translation = T_Commands then
                           null;

                        else
                           Sw :=
                             Matching_Name
                               (Arg (Arg'First .. SwP),
                                Make_Commands_Active.Switches,
                                Quiet => False);
                        end if;

                     --  For case of GNAT MAKE, if we cannot find the switch,
                     --  then see if it is a recognized compiler switch
                     --  instead, and if so process the compiler switch.

                     elsif Command.Name.all = "MAKE" then
                        Sw :=
                          Matching_Name
                            (Arg (Arg'First .. SwP),
                             Command.Switches,
                             Quiet => True);

                        if Sw = null then
                           Sw :=
                             Matching_Name
                               (Arg (Arg'First .. SwP),
                                Matching_Name ("COMPILE", Commands).Switches,
                                Quiet => False);
                        end if;

                     --  For all other cases, just search the relevant command

                     else
                        Sw :=
                          Matching_Name
                            (Arg (Arg'First .. SwP),
                             Command.Switches,
                             Quiet => False);

                     end if;

                     if Sw /= null then
                        case Sw.Translation is

                           when T_Direct =>
                              Place_Unix_Switches (Sw.Unix_String);

                           when T_Directories =>
                              if SwP + 1 > Arg'Last then
                                 Put (Standard_Error,
                                      "missing directories for: ");
                                 Put_Line (Standard_Error, Arg);
                                 Errors := Errors + 1;

                              elsif Arg (SwP + 2) /= '(' then
                                 SwP := SwP + 2;
                                 Endp := Arg'Last;

                              elsif Arg (Arg'Last) /= ')' then
                                 Put (Standard_Error,
                                      "incorrectly parenthesized argument: ");
                                 Put_Line (Standard_Error, Arg);

                              else
                                 SwP := SwP + 3;
                                 Endp := Arg'Last - 1;
                              end if;

                              while SwP <= Endp loop
                                 P2 := SwP;

                                 while P2 < Endp
                                       and then Arg (P2 + 1) /= ','
                                 loop
                                    P2 := P2 + 1;
                                 end loop;

                                 Place_Unix_Switches (Sw.Unix_String);
                                 Place_Lower (Arg (SwP .. P2));
                                 SwP := P2 + 2;
                              end loop;

                           when T_File =>
                              if SwP + 1 > Arg'Last then
                                 Put (Standard_Error, "missing file for: ");
                                 Put_Line (Standard_Error, Arg);
                                 Errors := Errors + 1;
                              else
                                 Place_Unix_Switches (Sw.Unix_String);
                                 Place (' ');
                                 Place_Lower (Arg (SwP + 2 .. Arg'Last));
                              end if;

                           when T_Numeric =>
                              if OK_Integer (Arg (SwP + 2 .. Arg'Last)) then
                                 Place_Unix_Switches (Sw.Unix_String);
                                 Place (Arg (SwP + 2 .. Arg'Last));

                              else
                                 Put (Standard_Error, "argument for ");
                                 Put (Standard_Error, Sw.Name.all);
                                 Put_Line (Standard_Error, "must be numeric");
                                 Errors := Errors + 1;
                              end if;

                           when T_Commands =>

                              --  Output -largs/-bargs/-cargs

                              Place (' ');
                              Place (Sw.Unix_String
                                      (Sw.Unix_String'First ..
                                       Sw.Unix_String'First + 5));

                              --  Set source of new commands, also setting this
                              --  non-null indicates that we are in the special
                              --  commands mode for processing the -xargs case.

                              Make_Commands_Active :=
                                Matching_Name
                                  (Sw.Unix_String
                                    (Sw.Unix_String'First + 7 ..
                                     Sw.Unix_String'Last),
                                   Commands);

                           when T_Options =>
                              if SwP + 1 > Arg'Last then
                                 Place_Unix_Switches (Sw.Options.Unix_String);
                                 SwP := Endp + 1;

                              elsif Arg (SwP + 2) /= '(' then
                                 SwP := SwP + 2;
                                 Endp := Arg'Last;

                              elsif Arg (Arg'Last) /= ')' then
                                 Put (Standard_Error,
                                      "incorrectly parenthesized argument: ");
                                 Put_Line (Standard_Error, Arg);
                                 SwP := Endp + 1;

                              else
                                 SwP := SwP + 3;
                                 Endp := Arg'Last - 1;
                              end if;

                              while SwP <= Endp loop
                                 P2 := SwP;

                                 while P2 < Endp
                                       and then Arg (P2 + 1) /= ','
                                 loop
                                    P2 := P2 + 1;
                                 end loop;

                                 --  Option name is in Arg (SwP .. P2)

                                 Opt := Matching_Name (Arg (SwP .. P2),
                                                       Sw.Options);

                                 if Opt /= null then
                                    Place_Unix_Switches (Opt.Unix_String);
                                 end if;

                                 SwP := P2 + 2;
                              end loop;

                           when T_Other =>
                              Place_Unix_Switches
                                (new String'(Sw.Unix_String.all
                                             & Arg (Arg'First .. SwP)));

                        end case;
                     end if;
                  end;
               end if;
            end;

            exit when Arg_Idx > Argv'Last;

         end loop;
      end;
   end loop;

   if Display_Command then
      Put (Standard_Error, "generated command -->");
      Put (Standard_Error, Buffer (1 .. Ptr));
      Put (Standard_Error, "<--");
      New_Line (Standard_Error);
      raise Normal_Exit;
   end if;

   if File_Count < Command.Minfile
      or else (Command.Maxfile /= 0 and then File_Count > Command.Maxfile)
   then
      Put (Standard_Error, "usage: ");
      Put_Line (Standard_Error, Command.Usage.all);
      Errors := Errors + 1;
   end if;

   if Errors > 0 then
      raise Error_Exit;
   else
      --  Prepare arguments for a call to spawn

      declare
         Pname_Ptr : Natural;
         Args      : Argument_List (1 .. 500);
         Nargs     : Natural;
         P1, P2    : Natural;
         Success   : Boolean;
         Exec_Path : String_Access;

      begin
         Pname_Ptr := 1;

         while Pname_Ptr < Ptr and then Buffer (Pname_Ptr + 1) /= ' ' loop
            Pname_Ptr := Pname_Ptr + 1;
         end loop;

         P1 := Pname_Ptr + 2;

         Nargs := 0;
         while P1 <= Ptr loop
            if Buffer (P1) = ' ' then
               P1 := P1 + 1;
            else
               Nargs := Nargs + 1;
               P2 := P1;

               while P2 < Ptr and then Buffer (P2 + 1) /= ' ' loop
                  P2 := P2 + 1;
               end loop;

               Args (Nargs) := new String'(Buffer (P1 .. P2));
               P1 := P2 + 2;
            end if;
         end loop;

         Exec_Path := Locate_Exec_On_Path (Buffer (1 .. Pname_Ptr));

         if Exec_Path = null then
            Put_Line (Standard_Error,
                      "Couldn't locate " & Buffer (1 .. Pname_Ptr));
            raise Error_Exit;
         end if;

         Spawn (Exec_Path.all, Args (1 .. Nargs), Success);
      end;

      raise Normal_Exit;
   end if;

exception
   when Error_Exit =>
      Set_Exit_Status (Failure);

   when Normal_Exit =>
      Set_Exit_Status (Success);

end GNATCmd;
