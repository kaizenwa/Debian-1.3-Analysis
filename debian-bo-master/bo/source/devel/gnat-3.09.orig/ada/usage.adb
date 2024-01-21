------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               U S A G E                                  --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision: 1.59 $                              --
--                                                                          --
--          Copyright (C) 1992-1997, Free Software Foundation, Inc.         --
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

with System.WCh_Con; use System.WCh_Con;

procedure Usage is

   procedure Write_Switch_Char;
   --  Output two spaces followed by default switch character

   procedure Write_Switch_Char is
   begin
      Write_Str ("  ");
      Write_Char (Switch_Character);
   end Write_Switch_Char;

--  Start of processing for Usage

begin
   --  Usage line

   Write_Str ("Usage: ");
   Write_Program_Name;
   Write_Char (' ');
   Write_Str ("switches sfile");
   Write_Eol;
   Write_Eol;

   --  Line for -a switch

   Write_Switch_Char;
   Write_Str ("a      Assertions enabled. Pragma Ass");
   Write_Str ("ert and pragma Debug to be activated");
   Write_Eol;

   --  Line for -b switch

   Write_Switch_Char;
   Write_Str ("b      Generate brief messages to std");
   Write_Str ("err even if verbose mode set");
   Write_Eol;

   --  Line for -c switch

   Write_Switch_Char;
   Write_Str ("c      Check syntax and semantics onl");
   Write_Str ("y (no code generation attempted)");
   Write_Eol;

   --  Line for -e switch

   Write_Switch_Char;
   Write_Str ("e      Error messages generated immed");
   Write_Str ("iately, not saved up till end");
   Write_Eol;

   --  Line for -f switch

   Write_Switch_Char;
   Write_Str ("f      Full errors. Multiple errors/l");
   Write_Str ("ine, all undefined references");
   Write_Eol;

   --  Line for -g switch

   Write_Switch_Char;
   Write_Str ("g      GNAT style checks enabled");
   Write_Eol;

   --  Line for -i switch

   Write_Switch_Char;
   Write_Str ("i?     Identifier char set (?=1/2/3/4");
   Write_Str ("/8/p/f/n/w)");
   Write_Eol;

   --  Line for -k switch

   Write_Switch_Char;
   Write_Str ("knnn   Limit file names to nnn charac");
   Write_Str ("ters (k = krunch)");
   Write_Eol;

   --  Line for -l switch

   Write_Switch_Char;
   Write_Str ("l      Output full source listing wit");
   Write_Str ("h embedded error messages");
   Write_Eol;

   --  Line for -m switch

   Write_Switch_Char;
   Write_Str ("mnnn   Limit number of detected error");
   Write_Str ("s to nnn (1-999)");
   Write_Eol;

   --  Line for -n switch

   Write_Switch_Char;
   Write_Str ("n      Inlining of subprograms (apply ");
   Write_Str ("pragma Inline across units)");
   Write_Eol;

   --  Line for -N switch

   Write_Switch_Char;
   Write_Str ("N      Aggressive inlining of subprograms");
   Write_Str ("(regardless of individual Inline pragmas)");
   Write_Eol;

   --  Line for -o switch

   Write_Switch_Char;
   Write_Str ("o      Enable optional checks (overfl");
   Write_Str ("ow, stack check, elaboration checks");
   Write_Eol;

   --  Line for -p switch

   Write_Switch_Char;
   Write_Str ("p      Suppress all checks");
   Write_Eol;

   --  Lines for -q switch

   Write_Switch_Char;
   Write_Str ("q      Don't quit, try semantics, eve");
   Write_Str ("n if parse errors");
   Write_Eol;

   --  Line for -r switch

   Write_Switch_Char;
   Write_Str ("r      Reference manual column layout");
   Write_Str (" required");
   Write_Eol;

   --  Lines for -s switch

   Write_Switch_Char;
   Write_Str ("s      Syntax check only");
   Write_Eol;

   --  Lines for -t switch

   Write_Switch_Char;
   Write_Str ("t      Tree output file to be generated");
   Write_Eol;

   --  Line for -T switch

   Write_Switch_Char;
   Write_Str ("Tnnn   All compiler tables start at nnn");
   Write_Str (" times their usual starting size");
   Write_Eol;

   --  Line for -u switch

   Write_Switch_Char;
   Write_Str ("u      List units for this compilatio");
   Write_Str ("n");
   Write_Eol;

   --  Line for -v switch

   Write_Switch_Char;
   Write_Str ("v      Verbose mode. Full error outpu");
   Write_Str ("t with source lines to stdout");
   Write_Eol;

   --  Lines for -w switch

   Write_Switch_Char;
   Write_Str ("w?     Warning mode. (?=s/e for suppr");
   Write_Str ("ess/treat as error)");
   Write_Eol;

   --  Line for -W switch

   Write_Switch_Char;
   Write_Str ("W?     Wide character encoding method");
   Write_Str (" (?=");

   for J in WC_Encoding_Method loop
      Write_Char (WC_Encoding_Letters (J));

      if J = WC_Encoding_Method'Last then
         Write_Char (')');
      else
         Write_Char ('/');
      end if;
   end loop;

   Write_Eol;

   --  Lines for -x switch

   Write_Switch_Char;
   Write_Str ("x?     Cross-reference level and swit");
   Write_Str ("ches (?=1/2/3/4/5/9/b/s)");
   Write_Eol;

   --  Lines for -z switch

   Write_Switch_Char;
   Write_Str ("z?     Distribution stub generation (");
   Write_Str ("r/s for receiver/sender stubs)");
   Write_Eol;

   --  Line for -83 switch

   Write_Switch_Char;
   Write_Str ("83     Enforce Ada 83 restrictions");
   Write_Eol;

   --  Line for sfile

   Write_Str ("  sfile   Source file names");
   Write_Eol;

end Usage;
