------------------------------------------------------------------------------

--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               B C H E C K                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.22 $                             --
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

with ALI;     use ALI;
with Binderr; use Binderr;
with Namet;   use Namet;
with Opt;     use Opt;
with Osint;   use Osint;
with Types;   use Types;

package body Bcheck is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Accumulate_Checksum (C : Character; Csum : in out Word);
   pragma Inline (Accumulate_Checksum);
   --  This routine accumulates the checksum given character C. During the
   --  scanning of a source file, this routine is called with every character
   --  in the source, excluding blanks, and all control characters (except
   --  that ESC is included in the checksum). Upper case letters not in string
   --  literals are folded by the caller. See Sinput spec for the documentation
   --  of the checksum algorithm. Note: checksum values are only used if we
   --  generate code, so it is not necessary to worry about making the right
   --  sequence of calls in any error situation.

   function Get_File_Checksum (Fname : Name_Id) return Word;
   --  Compute checksum for the given file. As far as possible, this circuit
   --  computes exactly the same value computed by the compiler, but it does
   --  not matter if it gets it wrong in marginal cases, since the only result
   --  is to miss some smart recompilation cases, correct functioning is not
   --  affecte by a mis-computation. Returns an impossible checksum value,
   --  with the upper bit set, if the file is missing or has an error.

   -------------------------
   -- Accumulate_Checksum --
   -------------------------

   procedure Accumulate_Checksum (C : Character; Csum : in out Word) is
   begin
      Csum := Csum + Csum + Character'Pos (C);

      if Csum > 16#8000_0000# then
         Csum := (Csum + 1) and 16#7FFF_FFFF#;
      end if;
   end Accumulate_Checksum;

   --------------------
   -- Check_Versions --
   --------------------

   procedure Check_Versions is
   begin
      for A in ALIs.First .. ALIs.Last loop

         if ALIs.Table (A).Ver /= ALIs.Table (ALIs.First).Ver then
            Error_Msg_Name_1 := Unit.Table (ALIs.Table (A).First_Unit).Sfile;
            Error_Msg_Name_2 :=
              Unit.Table (ALIs.Table (ALIs.First).First_Unit).Sfile;

            if Ignore_Time_Stamp_Errors then
               Error_Msg
                 ("?% and % compiled with different GNAT versions");
            else
               Error_Msg
                 ("% and % compiled with different GNAT versions");
            end if;
         end if;

         if ALIs.Table (A).Std /= ALIs.Table (ALIs.First).Std then
            Error_Msg_Name_1 := Unit.Table (ALIs.Table (A).First_Unit).Sfile;
            Error_Msg_Name_2 :=
              Unit.Table (ALIs.Table (ALIs.First).First_Unit).Sfile;

            if Ignore_Time_Stamp_Errors then
               Error_Msg
                 ("?% and % compiled with different versions of Standard");
            else
               Error_Msg
                 ("% and % compiled with different versions of Standard");
            end if;
         end if;
      end loop;
   end Check_Versions;

   -------------------------------------
   -- Check_Configuration_Consistency --
   -------------------------------------

   procedure Check_Configuration_Consistency is
      P1, P2 : Character;

   begin
      --  Check consistent queuing policy

      if Queuing_Policy /= ' ' then
         Qouter : for A1 in ALIs.First .. ALIs.Last loop
            P1 := ALIs.Table (A1).Queuing_Policy;
            if P1 /= ' ' then
               for A2 in ALIs.First .. ALIs.Last loop
                  P2 := ALIs.Table (A2).Queuing_Policy;
                  if P2 /= ' ' then
                     if P1 /= P2 then
                        Error_Msg_Name_1 :=
                          Unit.Table (ALIs.Table (A1).First_Unit).Sfile;
                        Error_Msg_Name_2 :=
                          Unit.Table (ALIs.Table (A2).First_Unit).Sfile;

                        if Ignore_Time_Stamp_Errors then
                           Error_Msg
                             ("?% and % compiled with different " &
                              "queuing policies");
                        else
                           Error_Msg
                             ("% and % compiled with different " &
                              "queuing policies");
                        end if;

                        exit Qouter;
                     end if;
                  end if;
               end loop;
            end if;
         end loop Qouter;
      end if;

      --  Check consistent locking policy

      --  Note: this is unnecessary code as long as only one locking policy
      --  is defined, but it is in place so that if locking policies are
      --  added then they will be checked for consistency.

      if Locking_Policy /= ' ' then
         Louter : for A1 in ALIs.First .. ALIs.Last loop
            P1 := ALIs.Table (A1).Locking_Policy;
            if P1 /= ' ' then
               for A2 in ALIs.First .. ALIs.Last loop
                  P2 := ALIs.Table (A2).Locking_Policy;
                  if P2 /= ' ' then
                     if P1 /= P2 then
                        Error_Msg_Name_1 :=
                          Unit.Table (ALIs.Table (A1).First_Unit).Sfile;
                        Error_Msg_Name_2 :=
                          Unit.Table (ALIs.Table (A2).First_Unit).Sfile;

                        if Ignore_Time_Stamp_Errors then
                           Error_Msg
                             ("?% and % compiled with different " &
                              "locking policies");
                        else
                           Error_Msg
                             ("% and % compiled with different " &
                              "locking policies");
                        end if;

                        exit Louter;
                     end if;
                  end if;
               end loop;
            end if;
         end loop Louter;
      end if;

      --  Check consistent task dispatching policy

      --  Note: this is unnecessary code as long as only one task dispatching
      --  policy is defined, but it is in place so that if task dispatching
      --  policies are added then they will be checked for consistency.

      if Task_Dispatching_Policy /= ' ' then
         Touter : for A1 in ALIs.First .. ALIs.Last loop
            P1 := ALIs.Table (A1).Task_Dispatching_Policy;
            if P1 /= ' ' then
               for A2 in ALIs.First .. ALIs.Last loop
                  P2 := ALIs.Table (A2).Task_Dispatching_Policy;
                  if P2 /= ' ' then
                     if P1 /= P2 then
                        Error_Msg_Name_1 :=
                          Unit.Table (ALIs.Table (A1).First_Unit).Sfile;
                        Error_Msg_Name_2 :=
                          Unit.Table (ALIs.Table (A2).First_Unit).Sfile;

                        if Ignore_Time_Stamp_Errors then
                           Error_Msg
                             ("?% and % compiled with different " &
                              "task dispatching policies");
                        else
                           Error_Msg
                             ("% and % compiled with different " &
                              "task dispatching policies");
                        end if;

                        exit Touter;
                     end if;
                  end if;
               end loop;
            end if;
         end loop Touter;
      end if;

   end Check_Configuration_Consistency;

   -----------------------
   -- Check_Consistency --
   -----------------------

   procedure Check_Consistency is
      Src : Source_Id;
      --  Source file Id for this Sdep entry

   begin
      --  First, we go through the source table to see if there are any cases
      --  in which we should go after source files and compute checksums of
      --  the source files. We need to do this for any file for which we have
      --  mismatching time stamps and (so far) matching checksums.

      for S in Source.First .. Source.Last loop

         --  If all time stamps for a file match, then there is nothing to
         --  do, since we will not be checking checksums in that case anyway

         if Source.Table (S).All_Timestamps_Match then
            null;

         --  If we did not find the source file, then we can't compute its
         --  checksum anyway. Note that when we have a time stamp mismatch,
         --  we try to find the source file unconditionally (i.e. if
         --  Check_Source_Files is False).

         elsif not Source.Table (S).Source_Found then
            null;

         --  If we already have non-matching or missing checksums, then no
         --  need to try going after source file, since we won't trust the
         --  checksums in any case.

         elsif not Source.Table (S).All_Checksums_Match then
            null;

         --  Now we have the case where we have time stamp mismatches, and
         --  the source file is around, but so far all checksums match. This
         --  is the case where we need to compute the checksum from the source
         --  file, since otherwise we would ignore the time stamp mismatches,
         --  and that is wrong if the checksum of the source does not agree
         --  with the checksums in the ALI files.

         elsif Check_Source_Files then
            if Source.Table (S).Checksum /=
               Get_File_Checksum (Source.Table (S).Sfile)
            then
               Source.Table (S).All_Checksums_Match := False;
            end if;
         end if;
      end loop;

      --  Loop through ALI files

      ALIs_Loop : for A in ALIs.First .. ALIs.Last loop

         --  Loop through Sdep entries in one ALI file

         Sdep_Loop : for D in
           ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep
         loop
            Src := Source_Id (Get_Name_Table_Info (Sdep.Table (D).Sfile));

            --  If the time stamps match, or all checksums match, then we
            --  are OK, otherwise we have a definite error.

            if Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
              and then not Source.Table (Src).All_Checksums_Match
            then
               Error_Msg_Name_1 := ALIs.Table (A).Sfile;
               Error_Msg_Name_2 := Sdep.Table (D).Sfile;

               --  Two styles of message, depending on whether or not
               --  the updated file is the one that must be recompiled

               if Error_Msg_Name_1 = Error_Msg_Name_2 then
                  if Ignore_Time_Stamp_Errors then
                     Error_Msg
                        ("?% has been modified and should be recompiled");
                  else
                     Error_Msg
                       ("% has been modified and must be recompiled");
                  end if;

               else
                  if Ignore_Time_Stamp_Errors then
                     Error_Msg
                       ("?% should be recompiled (% has been modified)");
                  else
                     Error_Msg
                       ("% must be recompiled (% has been modified)");
                  end if;
               end if;

               if (not Ignore_Time_Stamp_Errors) and Verbose_Mode then
                  declare
                     Msg : constant String := "file % has time stamp ";
                     Buf : String (1 .. Msg'Length + Time_Stamp_Length);

                  begin
                     Buf (1 .. Msg'Length) := Msg;
                     Buf (Msg'Length + 1 .. Buf'Length) :=
                       String (Source.Table (Src).Stamp);
                     Error_Msg_Name_1 := ALIs.Table (A).Sfile;
                     Error_Msg (Buf);

                     Buf (Msg'Length + 1 .. Buf'Length) :=
                       String (Sdep.Table (D).Stamp);
                     Error_Msg_Name_1 := Sdep.Table (D).Sfile;
                     Error_Msg (Buf);
                  end;
               end if;

               --  Exit from the loop through Sdep entries once we find one
               --  that does not match.

               exit Sdep_Loop;
            end if;

         end loop Sdep_Loop;
      end loop ALIs_Loop;
   end Check_Consistency;

   -----------------------
   -- Get_File_Checksum --
   -----------------------

   function Get_File_Checksum (Fname : Name_Id) return Word is
      Src  : Source_Buffer_Ptr;
      Hi   : Source_Ptr;
      Csum : Word;
      Ptr  : Source_Ptr;

      Bad : exception;
      --  Raised if file not found, or file format error

      use Ascii;
      --  Make control characters visible

      procedure Free_Source;
      --  Free source file buffer

      procedure Free_Source is
         procedure free (Arg : Source_Buffer_Ptr);
         pragma Import (C, free, "free");

      begin
         free (Src);
      end Free_Source;

   --  Start of processing for Get_File_Checksum

   begin
      Read_Source_File (Fname, 0, Hi, Src);

      --  If we cannot find the file, then return an impossible checksum,
      --  impossible becaues checksums have the high order bit zero, so
      --  that checksums do not match.

      if Src = null then
         raise Bad;
      end if;

      Csum := 0;
      Ptr := 0;

      loop
         case Src (Ptr) is

            --  Spaces and formatting information are ignored in checksum

            when ' ' | CR | LF | VT | FF | HT =>
               Ptr := Ptr + 1;

            --  EOF is ignored unless it is the last character

            when EOF =>
               if Ptr = Hi then
                  Free_Source;
                  return Csum;
               else
                  Ptr := Ptr + 1;
               end if;

            --  Non-blank characters that are included in the checksum

            when '#' | '&' | '*' | ':' | '(' | ',' | '.' | '=' | '>' |
                 '<' | ')' | '/' | ';' | '|' | '!' | '+' | '_' |
                 '0' .. '9' | 'a' .. 'z'
            =>
               Accumulate_Checksum (Src (Ptr), Csum);
               Ptr := Ptr + 1;

            --  Upper case letters, fold to lower case

            when 'A' .. 'Z' =>
               Accumulate_Checksum
                 (Character'Val (Character'Pos (Src (Ptr)) + 32), Csum);
               Ptr := Ptr + 1;

            --  Left bracket, really should do wide character thing here,
            --  but for now, don't bother.

            when '[' =>
               raise Bad;

            --  Minus, could be comment

            when '-' =>
               if Src (Ptr + 1) = '-' then
                  Ptr := Ptr + 2;

                  while Src (Ptr) >= ' ' or else Src (Ptr) = HT loop
                     Ptr := Ptr + 1;
                  end loop;

               else
                  Accumulate_Checksum ('-', Csum);
                  Ptr := Ptr + 1;
               end if;

            --  String delimited by double quote

            when '"' =>
               Accumulate_Checksum ('"', Csum);

               loop
                  Ptr := Ptr + 1;
                  exit when Src (Ptr) = '"';

                  if Src (Ptr) < ' ' then
                     raise Bad;
                  end if;

                  Accumulate_Checksum (Src (Ptr), Csum);
               end loop;

               Accumulate_Checksum ('"', Csum);
               Ptr := Ptr + 1;

            --  String delimited by percent

            when '%' =>
               Accumulate_Checksum ('%', Csum);

               loop
                  Ptr := Ptr + 1;
                  exit when Src (Ptr) = '%';

                  if Src (Ptr) < ' ' then
                     raise Bad;
                  end if;

                  Accumulate_Checksum (Src (Ptr), Csum);
               end loop;

               Accumulate_Checksum ('%', Csum);
               Ptr := Ptr + 1;

            --  Quote, could be character constant

            when ''' =>
               Accumulate_Checksum (''', Csum);

               if Src (Ptr + 2) = ''' then
                  Accumulate_Checksum (Src (Ptr + 1), Csum);
                  Accumulate_Checksum (''', Csum);
                  Ptr := Ptr + 3;

               --  Otherwise assume attribute char. We should deal with wide
               --  character cases here, but that's hard, so forget it.

               else
                  Ptr := Ptr + 1;
               end if;

            --  Upper half character, more to be done here, we should worry
            --  about folding Latin-1, folding other character sets, and
            --  dealing with the nasty case of upper half wide encoding.

            when Upper_Half_Character =>
               Accumulate_Checksum (Src (Ptr), Csum);
               Ptr := Ptr + 1;

            --  Escape character, we should do the wide character thing here,
            --  but for now, do not bother.

            when ESC =>
               raise Bad;

            --  Invalid control characters

            when NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS  | SO  |
                 SI  | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN |
                 EM  | FS  | GS  | RS  | US  | DEL
            =>
               raise Bad;

            --  Invalid graphic characters

            when '$' | '?' | '@' | '`' | '\' |
                 '^' | '~' | ']' | '{' | '}'
            =>
               raise Bad;

         end case;
      end loop;

   exception
      when Bad =>
         Free_Source;
         return 16#FFFF_FFFF#;

   end Get_File_Checksum;

end Bcheck;
