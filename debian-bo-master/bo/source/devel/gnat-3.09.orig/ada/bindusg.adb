------------------------------------------------------------------------------
--                                                                          --
--                        GBIND BINDER COMPONENTS                           --
--                                                                          --
--                             B I N D U S G                                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                            $Revision: 1.26 $                             --
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

with Osint;  use Osint;
with Output; use Output;

procedure Bindusg is

   procedure Write_Switch_Char;
   --  Write two spaces followed by appropriate switch character

   procedure Write_Switch_Char is
   begin
      Write_Str ("  ");
      Write_Char (Switch_Character);
   end Write_Switch_Char;

--  Start of processing for Bindusg

begin
   --  Usage line

   Write_Str ("Usage: ");
   Write_Program_Name;
   Write_Char (' ');
   Write_Str ("switches lfile");
   Write_Eol;
   Write_Eol;

   --  Line for -aO switch

   Write_Switch_Char;
   Write_Str ("aOdir  Specify library files search path");
   Write_Eol;

   --  Line for -aI switch

   Write_Switch_Char;
   Write_Str ("aIdir  Specify source files search path");
   Write_Eol;

   --  Line for -b switch

   Write_Switch_Char;
   Write_Str ("b      Generate brief messages to std");
   Write_Str ("err even if verbose mode set");
   Write_Eol;

   --  Line for -c switch

   Write_Switch_Char;
   Write_Str ("c      Check only, no generation of b");
   Write_Str ("inder output file");
   Write_Eol;

   --  Line for -e switch

   Write_Switch_Char;
   Write_Str ("e      Output complete list of elabor");
   Write_Str ("ation order dependencies");
   Write_Eol;

   --  Line for -h switch

   Write_Switch_Char;
   Write_Str ("h      Horrible (worst-case) elaborat");
   Write_Str ("ion order");
   Write_Eol;

   --  Line for -I switch

   Write_Switch_Char;
   Write_Str ("Idir   Specify library and source files search path");
   Write_Eol;

   --  Line for -I- switch

   Write_Switch_Char;
   Write_Str ("I-     Don't look for sources & library files");
   Write_Str (" in default directory");
   Write_Eol;

   --  Line for -l switch

   Write_Switch_Char;
   Write_Str ("l      Output chosen elaboration order");
   Write_Eol;

   --  Line for -m switch

   Write_Switch_Char;
   Write_Str ("mnnn   Limit number of detected error");
   Write_Str ("s to nnn (1-999)");
   Write_Eol;

   --  Line for -n switch

   Write_Switch_Char;
   Write_Str ("n      No main program");
   Write_Eol;

   --  Line for -o switch

   Write_Switch_Char;
   Write_Str ("o file give the output file name (default is b_xxx.c) ");
   Write_Eol;

   --  Line for -s switch

   Write_Switch_Char;
   Write_Str ("s      Require all source files to be");
   Write_Str (" present");
   Write_Eol;

   --  Line for -t switch

   Write_Switch_Char;
   Write_Str ("t      Ignore time stamp and other consistency errors");
   Write_Eol;

   --  Line for -v switch

   Write_Switch_Char;
   Write_Str ("v      Verbose mode. Error messages,");
   Write_Str ("header, summary output to stdout");
   Write_Eol;

   --  Lines for -w switch

   Write_Switch_Char;
   Write_Str ("wx     Warning mode. (x=s/e for supp");
   Write_Str ("ress/treat as error)");
   Write_Eol;

   --  Line for -x switch

   Write_Switch_Char;
   Write_Str ("x      Exclude source files (check ob");
   Write_Str ("ject consistency only)");
   Write_Eol;
   --  Line for sfile

   Write_Str ("  lfile   Library file names");
   Write_Eol;

end Bindusg;
