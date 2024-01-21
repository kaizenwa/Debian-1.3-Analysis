------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E R R O U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.151 $                            --
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

--  Warning! Error messages can be generated during Gigi processing by direct
--  calls to error message routines, so it is essential that the processing
--  in this body be consistent with the requirements for the Gigi processing
--  environment, and that in particular, no disallowed table expansion is
--  allowed to occur.

with Atree;    use Atree;
with Casing;   use Casing;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Expander; use Expander;
with Fname;    use Fname;
with Hostparm;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Scans;    use Scans;
with Sem_Util; use Sem_Util;
with Sinput;   use Sinput;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Uintp;    use Uintp;
with Uname;    use Uname;

package body Errout is

   Max_Msg_Length : constant := 80 + 2 * Hostparm.Max_Line_Length;
   --  Maximum length of error message. The addition of Max_Line_Length
   --  ensures that two insertion tokens of maximum length can be accomodated.

   Msg_Buffer : String (1 .. Max_Msg_Length);
   --  Buffer used to prepare error messages

   Msglen : Integer;
   --  Number of characters currently stored in the message buffer

   Flag_Source : Source_File_Index;
   --  Source file index for source file where error is being posted

   Is_Warning_Msg : Boolean;
   --  Set by Set_Msg_Text to indicate if current message is warning message

   Is_Unconditional_Msg : Boolean;
   --  Set by Set_Msg_Text to indicate if current message is unconditional

   Cur_Msg : Error_Msg_Id;
   --  Id of most recently posted error message

   Current_Error_Source_File : Source_File_Index;
   --  Id of current messages. Used to post file name when unit changes. This
   --  is initialized to Main_Source at the start of a compilation, which means
   --  that no file names will be output unless there are errors in units
   --  other than the main unit.

   Manual_Quote_Mode : Boolean;
   --  Set True in manual quotation mode

   List_Pragmas_Index : Int;
   --  Index into List_Pragmas table

   List_Pragmas_Mode : Boolean;
   --  Starts True, gets set False by pragma List (Off), True by List (On)

   Suppress_Message : Boolean;
   --  A flag used to suppress certain obviously redundant messages (i.e.
   --  those referring to a node whose type is Any_Type). This suppression
   --  is effective only if All_Errors_Mode is off.

   Kill_Message : Boolean;
   --  A flag used to kill weird messages (e.g. those containing uninterpreted
   --  implicit type references) if we have already seen at least one message
   --  already. The idea is that we hope the weird message is a junk cascaded
   --  message that should be suppressed.

   Suppress_Instance_Location : Boolean := False;
   --  Normally, if a # location in a message references a location within
   --  a generic template, then a note is added giving the location of the
   --  instantiation. If this variable is set True, then this note is not
   --  output. This is used for internal processing for the case of an
   --  illegal instantiation. See Error_Msg routine for further details.

   Continuation : Boolean;
   --  Indicates if current message is a continuation. Intialized from the
   --  Msg_Cont parameter in Error_Msg_Internal and then set True if a \
   --  insertion character is encountered.

   -----------------------------------
   -- Error Message Data Structures --
   -----------------------------------

   --  The error messages are stored as a linked list of error message objects
   --  sorted into ascending order by the source location (Sloc). Each object
   --  records the text of the message and its source location.

   --  The following record type and table are used to represent error
   --  messages, with one entry in the table being allocated for each message.

   type Error_Msg_Object is record
      Text : String_Ptr;
      --  Text of error message, fully expanded with all insertions

      Next : Error_Msg_Id;
      --  Pointer to next message in error chain

      Sfile : Source_File_Index;
      --  Source table index of source file. In the case of an error that
      --  refers to a template, always references the original template
      --  not an instantiation copy.

      Sptr : Source_Ptr;
      --  Flag pointer. In the case of an error that refers to a template,
      --  always references the original template, not an instantiation copy.

      Line : Logical_Line_Number;
      --  Line number for error message. Used in some cases to suppress
      --  errors appearing on the same line.

      Col : Column_Number;
      --  Column number for error message

      Warn : Boolean;
      --  True if warning message (i.e. insertion character ? appeared)

      Uncond : Boolean;
      --  True if unconditional message (i.e. insertion character ! appeared)

      Msg_Cont : Boolean;
      --  This is used for logical messages that are composed of multiple
      --  individual messages. For messages that are not part of such a
      --  group, or that are the first message in such a group. Msg_Cont
      --  is set to False. For subsequent messages in a group, Msg_Cont
      --  is set to True. This is used to make sure that such a group of
      --  messages is either suppressed or retained as a group (e.g. in
      --  the circuit that deletes identical messages).

   end record;

   package Errors is new Table (
     Table_Component_Type => Error_Msg_Object,
     Table_Index_Type     => Error_Msg_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 200,
     Table_Increment      => 200,
     Table_Name           => "Error");

   Error_Msgs : Error_Msg_Id;
   --  The list of error messages

   --------------------------
   -- Warning Mode Control --
   --------------------------

   --  Pragma Warnings allows warnings to be turned off for a specified
   --  region of code, and the following tabl is the data structure used
   --  to keep track of these regions.

   --  It contains pairs of source locations, the first being the start
   --  location for a warnings off region, and the second being the end
   --  location. When a pragma Warnings (Off) is encountered, a new entry
   --  is established extending from the location of the pragma to the
   --  end of the current source file. A subsequent pragma Warnings (On)
   --  adjusts the end point of this entry appropriately.

   --  If all warnings are suppressed by comamnd switch, then there is a
   --  dummy entry (put there by Errout.Initialize) at the start of the
   --  table which covers all possible Source_Ptr values. Note that the
   --  source pointer values in this table always reference the original
   --  template, not an instantiation copy, in the generic case.

   type Warnings_Entry is record
      Start : Source_Ptr;
      Stop  : Source_Ptr;
   end record;

   package Warnings is new Table (
     Table_Component_Type => Warnings_Entry,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Warnings");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Debug_Output (N : Node_Id);
   --  Called from Error_Msg_N and Error_Msg_NE to generate line of debug
   --  output giving node number (of node N) if the debug X switch is set.

   function Duplicate_Messages (M1, M2 : Error_Msg_Id) return Boolean;
   --  This function is passed the Id values of two error messages. A value
   --  of true is returned only if M1 and M2 represent identical messages.
   --  For this to be true, both M1 and M2 must represent non-continuation
   --  messages with identical text, and must either have no following
   --  continuation messages, or identical sequences of continuations.

   procedure Error_Msg_Internal
     (Msg           : String;
      Flag_Location : Source_Ptr;
      Msg_Cont      : Boolean);
   --  This is like Error_Msg, except that Flag_Location is known not to be
   --  a location within a instantiation of a generic template. The outer
   --  level routine, Error_Msg, takes care of dealing with the generic case.
   --  Msg_Cont is set True to indicate that the message is a continuation of
   --  a previous message. This means that it must have the same Flag_Location
   --  as the previous message.

   function OK_Node (N : Node_Id) return Boolean;
   --  Determines if a node is an OK node to place an error message on (return
   --  True) or if the error message should be suppressed (return False). A
   --  message is suppressed if the node already has an error posted on it,
   --  or if it refers to an Etype that has an error posted on it, or if
   --  it references an Entity that has an error posted on it.

   procedure Output_Error_Msgs (E : in out Error_Msg_Id);
   --  Output source line, error flag, and text of stored error message and
   --  all subsequent messages for the same line and unit. On return E is
   --  set to be one higher than the last message output.

   procedure Output_Line_Number (L : Logical_Line_Number);
   --  Output a line number as six digits (with leading zeroes suppressed),
   --  followed by a period and a blank (note that this is 8 characters which
   --  means that tabs in the source line will not get messed up). A line
   --  number of zero is output as eight blanks (this is the way we list the
   --  line number of a Source_Reference pragma at the start of the file).

   procedure Output_Msg_Text (E : Error_Msg_Id);
   --  Outputs characters of text in the text of the error message E, excluding
   --  any final exclamation point. Note that no end of line is output, the
   --  caller is responsible for adding the end of line.

   procedure Output_Source_Line
     (L     : Logical_Line_Number;
      Sfile : Source_File_Index;
      Errs  : Boolean);
   --  Outputs text of source line L, in file S, together with preceding line
   --  number, as described above for Output_Line_Number. The Errs parameter
   --  indicates if there are errors attached to the line, which forces
   --  listing on, even in the presence of pragma List (Off).

   procedure Set_Msg_Blank;
   --  Sets a single blank in the message if the preceding character is a
   --  non-blank character other than a left parenthesis. Has no effect if
   --  manual quote mode is turned on.

   procedure Set_Msg_Blank_Conditional;
   --  Sets a single blank in the message if the preceding character is a
   --  non-blank character other than a left parenthesis or quote. Has no
   --  effect if manual quote mode is turned on.

   procedure Set_Msg_Char (C : Character);
   --  Add a single character to the current message. This routine does not
   --  check for special insertion characters (they are just treated as text
   --  characters if they occur).

   procedure Set_Msg_Insertion_Column;
   --  Handle column number insertion (@ insertion character)

   procedure Set_Msg_Insertion_Name;
   --  Handle name insertion (% insertion character)

   procedure Set_Msg_Insertion_Line_Number (Loc, Flag : Source_Ptr);
   --  Handle line number insertion (# insertion character). Loc is the
   --  location to be referenced, and Flag is the location at which the
   --  flag is posted (used to determine whether to add "in file xxx")

   procedure Set_Msg_Insertion_Node;
   --  Handle node (name from node) insertion (& insertion character)

   procedure Set_Msg_Insertion_Reserved_Name;
   --  Handle insertion of reserved word name (* insertion character).

   procedure Set_Msg_Insertion_Reserved_Word
     (Text : String;
      J    : in out Integer);
   --  Handle reserved word insertion (upper case letters). The Text argument
   --  is the current error message input text, and J is an index which on
   --  entry points to the first character of the reserved word, and on exit
   --  points past the last character of the reserved word.

   procedure Set_Msg_Insertion_Type_Reference (Flag : Source_Ptr);
   --  Handle type reference (right brace insertion character). Flag is the
   --  location of the flag, which is provided for the internal call to
   --  Set_Msg_Insertion_Line_Number,

   procedure Set_Msg_Insertion_Uint;
   --  Handle Uint insertion (^ insertion character)

   procedure Set_Msg_Insertion_Unit_Name;
   --  Handle unit name insertion ($ insertion character)

   procedure Set_Msg_Insertion_File_Name;
   --  Handle file name insertion (left brace insertion character)

   procedure Set_Msg_Int (Line : Int);
   --  Set the decimal representation of the argument in the error message
   --  buffer with no leading zeroes output.

   procedure Set_Msg_Name_Buffer;
   --  Output name from Name_Buffer, with surrounding quotes unless manual
   --  quotation mode is in effect.

   procedure Set_Msg_Node (Node : Node_Id);
   --  Add the sequence of characters for the name associated with the
   --  given node to the current message.

   procedure Set_Msg_Quote;
   --  Set quote if in normal quote mode, nothing if in manual quote mode

   procedure Set_Msg_Str (Text : String);
   --  Add a sequence of characters to the current message. This routine does
   --  not check for special insertion characters (they are just treated as
   --  text characters if they occur).

   procedure Set_Msg_Text (Text : String; Flag : Source_Ptr);
   --  Add a sequence of characters to the current message. The characters may
   --  be one of the special insertion characters (see documentation in spec).
   --  Flag is the location at which the error is to be posted, which is used
   --  to determine whether or not the # insertion needs a file name. The
   --  variables Msg_Buffer, Msglen, Is_Warning_Msg, and Is_Unconditional_Msg
   --  are set on return.

   procedure Set_Posted (N : Node_Id);
   --  Sets the Error_Posted flag on the given node, and all its parents
   --  that are subexpressions and then on the parent non-subexpression
   --  construct that contains the original expression (this reduces the
   --  number of cascaded messages)

   -----------------------
   -- Change_Error_Text --
   -----------------------

   procedure Change_Error_Text (Error_Id : Error_Msg_Id; New_Msg : String) is
      Save_Next : Error_Msg_Id;
      Err_Id    : Error_Msg_Id := Error_Id;

   begin
      Set_Msg_Text (New_Msg, Errors.Table (Error_Id).Sptr);
      Errors.Table (Error_Id).Text := new String'(Msg_Buffer (1 .. Msglen));

      --  If in immediate error message mode, output modified error message now
      --  This is just a bit tricky, because we want to output just a single
      --  message, and the messages we modified is already linked in. We solve
      --  this by temporarily resetting its forward pointer to empty.

      if Immediate_Errors then
         Save_Next := Errors.Table (Error_Id).Next;
         Errors.Table (Error_Id).Next := No_Error_Msg;
         Write_Eol;
         Output_Source_Line
           (Errors.Table (Error_Id).Line, Errors.Table (Error_Id).Sfile, True);
         Output_Error_Msgs (Err_Id);
         Errors.Table (Error_Id).Next := Save_Next;
      end if;
   end Change_Error_Text;

   ------------------
   -- Debug_Output --
   ------------------

   procedure Debug_Output (N : Node_Id) is
   begin
      if Debug_Flag_1 then
         Write_Str ("*** following error message posted on node id = #");
         Write_Int (Int (N));
         Write_Str (" ***");
         Write_Eol;
      end if;
   end Debug_Output;

   ------------------------
   -- Duplicate_Messages --
   ------------------------

   function Duplicate_Messages (M1, M2 : Error_Msg_Id) return Boolean is
      L1, L2 : Error_Msg_Id;
      N1, N2 : Error_Msg_Id;

   begin
      --  Both messages must be non-continuation messages

      if Errors.Table (M1).Msg_Cont or else Errors.Table (M2).Msg_Cont then
         return False;
      end if;

      --  Definitely not equal if message text does not match

      if Errors.Table (M1).Text.all /= Errors.Table (M2).Text.all then
         return False;
      end if;

      --  Same text. See if all continuations are also identical

      L1 := M1;
      L2 := M2;

      loop
         N1 := Errors.Table (L1).Next;
         N2 := Errors.Table (L2).Next;

         --  If one of the message sequenes has run out, the other must too

         if N1 = No_Error_Msg or else (not Errors.Table (N1).Msg_Cont) then
            return
              N2 = No_Error_Msg
                or else
              (not Errors.Table (N2).Msg_Cont);
         end if;

         --  Otherwise both message sequences are continued

         if Errors.Table (N1).Text.all /= Errors.Table (N2).Text.all then
            return False;

         else
            L1 := N1;
            L2 := N2;
         end if;
      end loop;
   end Duplicate_Messages;

   ---------------
   -- Error_Msg --
   ---------------

   --  Error_Msg posts a flag at the given location, except that if the
   --  Flag_Location points within a generic template and corresponds
   --  to an instantiation of this generic template, then the actual
   --  message will be posted on the generic instantiation, along with
   --  additional messages referencing the generic declaration.

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr) is

      Sindex : constant Source_File_Index :=
                 Get_Source_File_Index (Flag_Location);

      Already_Flagged : Boolean := False;
      --  Gets reset to true if a message has already been posted at
      --  the given Flag_Location. Used to avoid bogus instantiation
      --  messages as described below.

      Orig_Loc : Source_Ptr;
      --  Original location of Flag_Location (i.e. location in original
      --  template in instantiation case, otherwise unchanged).

   begin
      --  It is a fatal error to issue an error message when scanning from
      --  the internal source buffer (see Sinput for further documentation)

      pragma Assert (Source /= Internal_Source_Ptr);

      --  The idea at this stage is that we have two kinds of messages.

      --  First, we have those that are to be placed as requested at
      --  Flag_Location. This includes messages that have nothing to
      --  do with generics, and also messages placed on generic templates
      --  that reflect an error in the template itself. For such messages
      --  we simply call Error_Msg_Internal to place the message in the
      --  requested location.

      if Instantiation (Sindex) = No_Location then
         Error_Msg_Internal (Msg, Flag_Location, False);
         return;
      end if;

      --  If we are trying to flag an error in an instantiation, we may have
      --  a generic contract violation. What we generate in this case is:

      --     instantiation error at ...
      --     original error message

      --  All these messages are posted at the location of the top level
      --  instantiation. If there are nested instantiations, then the
      --  instantiation error message can be repeated, pointing to each
      --  of the relevant instantiations.

      --  However, before we do this, we need to worry about the case where
      --  indeed we are in an instantiation, but the message is a warning
      --  message. In this case, it almost certainly a warning for the
      --  template itself and so it is posted on the template.

      for M in Msg'Range loop
         if Msg (M) = '?' then
            Error_Msg_Internal (Msg, Flag_Location, False);
            return;
         end if;
      end loop;

      --  Second, we need to worry about the case where there was a real error
      --  in the template, and we are getting a repeat of this error in the
      --  instantiation. We don't want to complain about the instantiation
      --  in this case, since we have already flagged the template.

      --  To deal with this case, just see if we have posted a non-warning
      --  message at the template location. If so, assume that the current
      --  message is redundant. There could be cases in which this is not
      --  a correct assumption, but it is not terrible to lose a message,
      --  about an incorrect instantiation, given that we have flagged an
      --  illegality in the template itself.

      Orig_Loc := Original_Location (Flag_Location);

      for Err in Errors.First .. Errors.Last loop
         if not Errors.Table (Err).Warn
           and then Errors.Table (Err).Sptr = Orig_Loc
         then
            return;
         end if;
      end loop;

      --  OK, this is the case where we have an instantiation error, and
      --  we need to generate the error on the instantiation, rather than
      --  on the template. First, see if we have posted this exact error
      --  before, and if so suppress it. It is not so easy to use the main
      --  list of errors for this, since they have already been split up
      --  according to the processing below. Consequently we use an auxiliary
      --  data structure that just records these types of messages (it will
      --  never have very many entries).

      declare
         Actual_Error_Loc : Source_Ptr;
         --  Location of outer level instantiation in instantiation case, or
         --  just a copy of Flag_Location in the normal case. This is the
         --  location where all error messages will actually be posted.

         Save_Error_Msg_Sloc : constant Source_Ptr := Error_Msg_Sloc;
         --  Save possible location set for caller's message. We need to
         --  use Error_Msg_Sloc for the location of the instantiation error
         --  but we have to preserve a possible original value.

         X : Source_File_Index;

         Msg_Cont_Status : Boolean;
         --  Used to label continuation lines in instantiation case with
         --  proper Msg_Cont status.

      begin
         X := Get_Source_File_Index (Flag_Location);

         --  Loop to find highest level instantiation, where all error
         --  messages will be placed.

         while Instantiation (X) /= No_Location loop
            Actual_Error_Loc := Instantiation (X);
            X := Get_Source_File_Index (Actual_Error_Loc);
         end loop;

         --  Since we are generating the messages at the instantiation
         --  point in any case, we do not want the references to the
         --  bad lines in the instance to be annotated with the location
         --  of the instantiation.

         Suppress_Instance_Location := True;
         Msg_Cont_Status := False;

         --  Loop to generate instantiation messages

         Error_Msg_Sloc := Flag_Location;
         X := Get_Source_File_Index (Flag_Location);

         while Instantiation (X) /= No_Location loop
            Error_Msg_Internal
               ("instantiation error #",
                Actual_Error_Loc, Msg_Cont_Status);
            Error_Msg_Sloc := Instantiation (X);
            X := Get_Source_File_Index (Error_Msg_Sloc);
            Msg_Cont_Status := True;
         end loop;

         Suppress_Instance_Location := False;
         Error_Msg_Sloc := Save_Error_Msg_Sloc;

         --  Here we output the original message on the outer instantiation

         Error_Msg_Internal (Msg, Actual_Error_Loc, Msg_Cont_Status);
      end;
   end Error_Msg;

   ------------------------
   -- Error_Msg_Internal --
   ------------------------

   procedure Error_Msg_Internal
     (Msg           : String;
      Flag_Location : Source_Ptr;
      Msg_Cont      : Boolean)
   is
      Next_Msg : Error_Msg_Id;
      --  Pointer to next message at insertion point

      Prev_Msg : Error_Msg_Id;
      --  Pointer to previous message at insertion point

      Temp_Msg : Error_Msg_Id;

      Orig_Loc : constant Source_Ptr := Original_Location (Flag_Location);

   begin
      Continuation := Msg_Cont;
      Suppress_Message := False;
      Kill_Message := False;
      Set_Msg_Text (Msg, Orig_Loc);

      --  Return without doing anything if message is suppressed

      if Suppress_Message
        and not All_Errors_Mode
        and not (Msg (Msg'Last) = '!')
      then
         return;
      end if;

      --  Return without doing anything if message is killed and this
      --  is not the first error message. The philosophy is that if we
      --  get a weird error message and we already have had a message,
      --  then we hope the weird message is a junk cascaded message

      if Kill_Message
        and then not All_Errors_Mode
        and then Errors_Detected /= 0
      then
         return;
      end if;

      --  Immediate return if warning message and warnings are suppressed

      if Is_Warning_Msg then
         for J in Warnings.First .. Warnings.Last loop
            if Warnings.Table (J).Start <= Orig_Loc
              and then Orig_Loc <= Warnings.Table (J).Stop
            then
               Cur_Msg := No_Error_Msg;
               return;
            end if;
         end loop;
      end if;

      --  Otherwise build error message object for new message

      Errors.Increment_Last;
      Cur_Msg := Errors.Last;
      Errors.Table (Cur_Msg).Text     := new String'(Msg_Buffer (1 .. Msglen));
      Errors.Table (Cur_Msg).Next     := No_Error_Msg;
      Errors.Table (Cur_Msg).Sptr     := Orig_Loc;
      Errors.Table (Cur_Msg).Sfile    := Get_Source_File_Index (Orig_Loc);
      Errors.Table (Cur_Msg).Line     := Get_Line_Number (Orig_Loc);
      Errors.Table (Cur_Msg).Col      := Get_Column_Number (Orig_Loc);
      Errors.Table (Cur_Msg).Warn     := Is_Warning_Msg;
      Errors.Table (Cur_Msg).Uncond   := Is_Unconditional_Msg;
      Errors.Table (Cur_Msg).Msg_Cont := Continuation;

      --  If immediate errors mode set, output error message now. Also output
      --  now if the -d1 debug flag is set (so node number message comes out
      --  just before actual error message)

      if Immediate_Errors or else Debug_Flag_1 then
         Write_Eol;
         Output_Source_Line (Errors.Table (Cur_Msg).Line,
           Errors.Table (Cur_Msg).Sfile, True);
         Temp_Msg := Cur_Msg;
         Output_Error_Msgs (Temp_Msg);

      --  If not in immediate errors mode, then we insert the message in the
      --  error chain for later output by Finalize. The messages are sorted
      --  first by unit (main unit comes first), and within a unit by source
      --  location (earlier flag location first in the chain).

      else
         Prev_Msg := No_Error_Msg;
         Next_Msg := Error_Msgs;

         while Next_Msg /= No_Error_Msg loop
            exit when
              Errors.Table (Cur_Msg).Sfile < Errors.Table (Next_Msg).Sfile;

            if Errors.Table (Cur_Msg).Sfile =
                 Errors.Table (Next_Msg).Sfile
            then
               exit when Orig_Loc < Errors.Table (Next_Msg).Sptr;
            end if;

            Prev_Msg := Next_Msg;
            Next_Msg := Errors.Table (Next_Msg).Next;
         end loop;

         --  The possible insertion point for the new message is after Prev_Msg
         --  and before Next_Msg. However, there are some cases in which we do
         --  not insert the message on the grounds that it is redundant with
         --  respect to the previous message. We only consider deleting the
         --  message if it is for the same line and unit as the previous one.

         if Prev_Msg /= No_Error_Msg
           and then Errors.Table (Prev_Msg).Line =
                                             Errors.Table (Cur_Msg).Line
           and then Errors.Table (Prev_Msg).Sfile =
                                             Errors.Table (Cur_Msg).Sfile
         then
            --  Delete a complete duplicate message (i.e. same error text
            --  at same position). Such duplicate messages are typically
            --  lexical messages from tokens that are rescanned. Note that
            --  such complete duplicates are deleted even if All_Errors
            --  mode is set on, since they can't possibly give any useful
            --  information under any circumstances.

            --  A special case of identity of error messages occurs when
            --  the previous message is of the form "xxx, instance at yyy",
            --  and the new message is simply "xxx". In this case we prefer
            --  to retain only the instance message.

            --  Note we actually do this in two places, once here, but that
            --  catches only adjacent messages, and again in Finalize, where
            --  we catch any remaining instances of duplicated messages.

            --  Note: do not delete messages following an instantiation
            --  error, because we need to keep those properly associated
            --  with the instantiation error messages.

            Check_Duplicate : declare

               function Same_Error (Prv, Cur : String_Ptr) return Boolean;
               --  See if current message is same error as previous, defined
               --  as either an identical message, or the same message but
               --  lacking an "instance at xxx" tag.

               function Same_Error (Prv, Cur : String_Ptr) return Boolean is
                  Cur_Len : constant Integer := Cur'Length;
                  Prv_Len : constant Integer := Prv'Length;

               begin
                  return
                    Prv.all = Cur.all
                      or else
                        (Prv_Len - 10 > Cur_Len
                           and then
                         Cur.all = Prv.all (1 .. Cur_Len)
                           and then
                         Prv (Cur_Len + 1 .. Cur_Len + 10) = ", instance");
               end Same_Error;

            --  Start of processing for Check_Duplicate

            begin
               if Errors.Table (Prev_Msg).Col = Errors.Table (Cur_Msg).Col
                 and then
                   Same_Error
                     (Errors.Table (Prev_Msg).Text,
                      Errors.Table (Cur_Msg).Text)
               then
                  return;
               end if;
            end Check_Duplicate;

            --  Remaining case is where we are parsing and we are not in
            --  all errors mode (in semantics, don't delete any messages)

            if not All_Errors_Mode and then Compiler_State = Parsing then

               --  Don't delete unconditional messages

               if not Errors.Table (Cur_Msg).Uncond then

                  --  Don't delete if prev msg is warning and new msg is
                  --  an error. This is because we don't want a real error
                  --  masked by a warning. In all other cases (that is parse
                  --  errors for the same line that are not unconditional)
                  --  we do delete the message. This helps to avoid
                  --  junk extra messages from cascaded parsing errors

                  if not Errors.Table (Prev_Msg).Warn
                    or else Errors.Table (Cur_Msg).Warn
                  then
                     --  All tests passed, delete the message by simply
                     --  returning without any further processing.

                     return;
                  end if;
               end if;
            end if;
         end if;

         --  Come here if message is to be inserted in the error chain

         if Prev_Msg = No_Error_Msg then
            Error_Msgs := Cur_Msg;
         else
            Errors.Table (Prev_Msg).Next := Cur_Msg;
         end if;

         Errors.Table (Cur_Msg).Next := Next_Msg;
      end if;

      --  Bump appropriate statistics count

      if Errors.Table (Cur_Msg).Warn
        and then Warning_Mode /= Treat_As_Error
      then
         Warnings_Detected := Warnings_Detected + 1;

      else
         Errors_Detected := Errors_Detected + 1;

         --  Turn off code generation if not done already

         if Operating_Mode = Generate_Code then
            Operating_Mode := Check_Semantics;
            Expander_Active := False;
         end if;

         --  Set the fatal error flag in the unit table unless we are
         --  in Try_Semantics mode. This stops the semantics from being
         --  performed if we find a parser error. This is skipped if we
         --  are currently dealing with the configuration pragma file.

         if not Try_Semantics
           and then Current_Source_Unit /= No_Unit
         then
            Set_Fatal_Error (Get_Sloc_Unit_Number (Orig_Loc));
         end if;
      end if;

      --  Terminate if max errors reached

      if Errors_Detected + Warnings_Detected = Maximum_Errors then
         raise Unrecoverable_Error;
      end if;

   end Error_Msg_Internal;

   -----------------
   -- Error_Msg_S --
   -----------------

   procedure Error_Msg_S (Msg : String) is
   begin
      Error_Msg (Msg, Scan_Ptr);
   end Error_Msg_S;

   ------------------
   -- Error_Msg_AP --
   ------------------

   procedure Error_Msg_AP (Msg : String) is
      S1 : Source_Ptr;
      C  : Character;

   begin
      --  If we had saved the Scan_Ptr value after scanning the previous
      --  token, then we would have exactly the right place for putting
      --  the flag immediately at hand. However, that would add at least
      --  two instructions to a Scan call *just* to service the possibility
      --  of an Error_Msg_AP call. So instead we reconstruct that value.

      --  We have two possibilities, start with Prev_Token_Ptr and skip over
      --  the current token, which is made harder by the possibility that this
      --  token may be in error, or start with Token_Ptr and work backwards.
      --  We used to take the second approach, but it's hard because of
      --  comments, and harder still because things that look like comments
      --  can appear inside strings. So now we take the first approach.

      --  Note: in the case where there is no previous token, Prev_Token_Ptr
      --  is set to Source_First, which is a reasonable position for the
      --  error flag in this situation.

      S1 := Prev_Token_Ptr;
      C := Source (S1);

      --  If the previous token is a string literal, we need a special approach
      --  since there may be white space inside the literal and we don't want
      --  to stop on that white space.

      if Prev_Token = Tok_String_Literal then
         loop
            S1 := S1 + 1;

            if Source (S1) = C then
               S1 := S1 + 1;
               exit when Source (S1) /= C;
            elsif Source (S1) in Line_Terminator then
               exit;
            end if;
         end loop;

      --  Character literal also needs special handling

      elsif Prev_Token = Tok_Char_Literal then
         S1 := S1 + 3;

      --  Otherwise we search forward for the end of the current token, marked
      --  by a line terminator, white space, a comment symbol or if we bump
      --  into the following token (i.e. the current token)

      else
         while Source (S1) not in Line_Terminator
           and then Source (S1) /= ' '
           and then Source (S1) /= Ascii.HT
           and then (Source (S1) /= '-' or else Source (S1 + 1) /= '-')
           and then S1 /= Token_Ptr
         loop
            S1 := S1 + 1;
         end loop;
      end if;

      --  S1 is now set to the location for the flag

      Error_Msg (Msg, S1);

   end Error_Msg_AP;

   ------------------
   -- Error_Msg_BC --
   ------------------

   procedure Error_Msg_BC (Msg : String) is
   begin
      --  If we are at end of file, post the flag after the previous token

      if Token = Tok_EOF then
         Error_Msg_AP (Msg);

      --  If we are at start of file, post the flag at the current token

      elsif Token_Ptr = Source_First (Current_Source_File) then
         Error_Msg_SC (Msg);

      --  If the character before the current token is a space or a horizontal
      --  tab, then we place the flag on this character (in the case of a tab
      --  we would really like to place it in the "last" character of the tab
      --  space, but that it too much trouble to worry about).

      elsif Source (Token_Ptr - 1) = ' '
         or else Source (Token_Ptr - 1) = Ascii.HT
      then
         Error_Msg (Msg, Token_Ptr - 1);

      --  If there is no space or tab before the current token, then there is
      --  no room to place the flag before the token, so we place it on the
      --  token instead (this happens for example at the start of a line).

      else
         Error_Msg (Msg, Token_Ptr);
      end if;
   end Error_Msg_BC;

   ------------------
   -- Error_Msg_SC --
   ------------------

   procedure Error_Msg_SC (Msg : String) is
   begin
      --  If we are at end of file, post the flag after the previous token

      if Token = Tok_EOF then
         Error_Msg_AP (Msg);

      --  For all other cases the message is posted at the current token
      --  pointer position

      else
         Error_Msg (Msg, Token_Ptr);
      end if;
   end Error_Msg_SC;

   ------------------
   -- Error_Msg_SP --
   ------------------

   procedure Error_Msg_SP (Msg : String) is
   begin
      --  Note: in the case where there is no previous token, Prev_Token_Ptr
      --  is set to Source_First, which is a reasonable position for the
      --  error flag in this situation

      Error_Msg (Msg, Prev_Token_Ptr);
   end Error_Msg_SP;

   -----------------
   -- Error_Msg_N --
   -----------------

   procedure Error_Msg_N (Msg : String; N : Node_Id) is
   begin
      if All_Errors_Mode
        or else Msg (Msg'Last) = '!'
        or else OK_Node (N)
      then
         Debug_Output (N);
         Error_Msg_Node_1 := N;
         Error_Msg (Msg, Sloc (N));
      end if;

      if not Is_Warning_Msg then
         Set_Posted (N);
      end if;
   end Error_Msg_N;

   ------------------
   -- Error_Msg_NE --
   ------------------

   procedure Error_Msg_NE (Msg : String; N : Node_Id; E : Entity_Id) is
   begin
      if All_Errors_Mode
        or else Msg (Msg'Last) = '!'
        or else OK_Node (N)
      then
         Debug_Output (N);
         Error_Msg_Node_1 := E;
         Error_Msg (Msg, Sloc (N));
      end if;

      if not Is_Warning_Msg then
         Set_Posted (N);
      end if;
   end Error_Msg_NE;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      Cur      : Error_Msg_Id;
      Prv      : Error_Msg_Id;
      Nxt      : Error_Msg_Id;
      E, F     : Error_Msg_Id;
      Err_Flag : Boolean;
      L        : Logical_Line_Number;
      Delete   : Boolean;

   begin
      --  Eliminate any duplicated error messages from the list. This is
      --  done after the fact to avoid problems with Change_Error_Text.

      Cur := Error_Msgs;
      Prv := No_Error_Msg;

      while Cur /= No_Error_Msg loop
         Nxt := Errors.Table (Cur).Next;

         --  Check for duplicate messages

         Delete := False;
         F := Nxt;

         while F /= No_Error_Msg
           and then Errors.Table (F).Sptr = Errors.Table (Cur).Sptr
         loop
            if Duplicate_Messages (Cur, F) then
               Delete := True;
               exit;
            else
               F := Errors.Table (F).Next;
            end if;
         end loop;

         --  If deletion signalled, delete message and all its continuations

         if Delete then

            --  On entry to the loop, Cur points to a message to delete

            loop
               Nxt := Errors.Table (Cur).Next;

               if Prv = No_Error_Msg then
                  Error_Msgs := Nxt;
               else
                  Errors.Table (Prv).Next := Nxt;
               end if;

               Cur := Nxt;

               exit when Cur = No_Error_Msg
                 or else not Errors.Table (Cur).Msg_Cont;

            end loop;

         --  If no deletion, move Cur to next message

         else
            Prv := Cur;
            Cur := Nxt;
         end if;
      end loop;

      --  Brief Error mode

      if Brief_Output or (not Full_List and not Verbose_Mode) then
         E := Error_Msgs;
         Set_Standard_Error;

         while E /= No_Error_Msg loop
            Write_Name (Reference_Name (Errors.Table (E).Sfile));
            Write_Char (':');
            Write_Int (Int (Errors.Table (E).Line));
            Write_Char (':');

            if Errors.Table (E).Col < 10 then
               Write_Char ('0');
            end if;

            Write_Int (Int (Errors.Table (E).Col));
            Write_Str (": ");
            Output_Msg_Text (E);
            Write_Eol;
            E := Errors.Table (E).Next;
         end loop;

         Set_Standard_Output;
      end if;

      --  Full source listing case

      if Full_List then
         List_Pragmas_Index := 1;
         List_Pragmas_Mode := True;
         E := Error_Msgs;
         Write_Eol;

         --  First list initial main source file with its error messages

         for N in 1 .. Num_Source_Lines (Main_Source) loop
            L := Physical_To_Logical (N, Main_Source);

            Err_Flag :=
              E /= No_Error_Msg
                and then Errors.Table (E).Line = L
                and then Errors.Table (E).Sfile = Main_Source;

            Output_Source_Line (L, Main_Source, Err_Flag);

            if Err_Flag then
               Output_Error_Msgs (E);

               if not Debug_Flag_2 then
                  Write_Eol;
               end if;
            end if;

         end loop;

         --  Then output errors, if any, for subsidiary units

         while E /= No_Error_Msg
           and then Errors.Table (E).Sfile /= Main_Source
         loop
            Write_Eol;
            Output_Source_Line
              (Errors.Table (E).Line, Errors.Table (E).Sfile, True);
            Output_Error_Msgs (E);
         end loop;
      end if;

      --  Verbose mode (error lines only with error flags)

      if Verbose_Mode and not Full_List then
         E := Error_Msgs;

         --  Loop through error lines

         while E /= No_Error_Msg loop
            Write_Eol;
            Output_Source_Line
              (Errors.Table (E).Line, Errors.Table (E).Sfile, True);
            Output_Error_Msgs (E);
         end loop;
      end if;

      --  Output error summary if verbose or full list mode

      if Verbose_Mode or else Full_List then

         --  Extra blank line if error messages or source listing were output

         if Errors_Detected + Warnings_Detected > 0 or else Full_List then
            Write_Eol;
         end if;

         --  Message giving total number of lines

         Write_Str (" ");
         Write_Int (Num_Source_Lines (Main_Source));

         if Num_Source_Lines (Main_Source) = 1 then
            Write_Str (" line: ");
         else
            Write_Str (" lines: ");
         end if;

         --  Message giving number of errors detected. This normally goes to
         --  Standard_Output. The exception is when brief mode is not set,
         --  verbose mode (or full list mode) is set, and there are errors.
         --  In this case we send the message to standard error to make sure
         --  that *something* appears on standard error in an error situation.

         if Errors_Detected + Warnings_Detected /= 0
           and then not Brief_Output
           and then (Verbose_Mode or Full_List)
         then
            Set_Standard_Error;
         end if;

         if Errors_Detected = 0 then
            Write_Str ("No errors");

         elsif Errors_Detected = 1 then
            Write_Str ("1 error");

         else
            Write_Int (Errors_Detected);
            Write_Str (" errors");
         end if;

         if Warnings_Detected = 1 then
            Write_Str (", 1 warning");

         elsif Warnings_Detected > 1 then
            Write_Str (", ");
            Write_Int (Warnings_Detected);
            Write_Str (" warnings");
         end if;

         Write_Eol;
         Set_Standard_Output;
      end if;

      if Maximum_Errors /= 0
        and then Errors_Detected + Warnings_Detected = Maximum_Errors
      then
         Set_Standard_Error;
         Write_Str ("fatal error: maximum errors reached");
         Write_Eol;
         Set_Standard_Output;
      end if;

   end Finalize;

   ----------------
   -- Get_Msg_Id --
   ----------------

   function Get_Msg_Id return Error_Msg_Id is
   begin
      return Cur_Msg;
   end Get_Msg_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Errors.Init;
      Error_Msgs := No_Error_Msg;
      Errors_Detected := 0;
      Warnings_Detected := 0;
      Cur_Msg := No_Error_Msg;
      Current_Error_Source_File := Main_Source;
      List_Pragmas.Init;

      --  Initialize warnings table, if all warnings are suppressed, supply
      --  an initial dummy entry covering all possible source locations.

      Warnings.Init;

      if Warning_Mode = Suppress then
         Warnings.Increment_Last;
         Warnings.Table (Warnings.Last).Start := Source_Ptr'First;
         Warnings.Table (Warnings.Last).Stop  := Source_Ptr'Last;
      end if;

   end Initialize;

   -------------
   -- OK_Node --
   -------------

   function OK_Node (N : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (N);

   begin
      if Error_Posted (N) then
         return False;

      elsif K in N_Has_Etype
        and then Present (Etype (N))
        and then Error_Posted (Etype (N))
      then
         return False;

      elsif (K in N_Op
              or else K = N_Attribute_Reference
              or else K = N_Character_Literal
              or else K = N_Expanded_Name
              or else K = N_Identifier
              or else K = N_Operator_Symbol)
        and then Present (Entity (N))
        and then Error_Posted (Entity (N))
      then
         return False;
      else
         return True;
      end if;
   end OK_Node;

   -----------------------
   -- Output_Error_Msgs --
   -----------------------

   procedure Output_Error_Msgs (E : in out Error_Msg_Id) is
      P : Source_Ptr;
      T : Error_Msg_Id;
      Flag_Num : Pos;
      Mult_Flags : Boolean := False;

   begin
      --  Figure out if we will place more than one error flag on this line

      T := E;
      while T /= No_Error_Msg
        and then Errors.Table (T).Line = Errors.Table (E).Line
        and then Errors.Table (T).Sfile = Errors.Table (E).Sfile
      loop
         if Errors.Table (T).Sptr > Errors.Table (E).Sptr then
            Mult_Flags := True;
         end if;

         T := Errors.Table (T).Next;
      end loop;

      --  Output the error flags. The circuit here makes sure that the tab
      --  characters in the original line are properly accounted for. The
      --  eight blanks at the start are to match the line number.

      if not Debug_Flag_2 then
         Write_Str ("        ");
         P := Line_Start (Errors.Table (E).Sptr);
         Flag_Num := 1;

         --  Loop through error messages for this line to place flags

         T := E;
         while T /= No_Error_Msg
           and then Errors.Table (T).Line = Errors.Table (E).Line
           and then Errors.Table (T).Sfile = Errors.Table (E).Sfile
         loop

            --  Loop to output blanks till current flag position

            while P < Errors.Table (T).Sptr loop
               if Source_Text (Errors.Table (T).Sfile) (P) = Ascii.HT then
                  Write_Char (Ascii.HT);
               else
                  Write_Char (' ');
               end if;

               P := P + 1;
            end loop;

            --  Output flag (unless already output, this happens if more
            --  than one error message occurs at the same flag position).

            if P = Errors.Table (T).Sptr then
               if (Flag_Num = 1 and then not Mult_Flags)
                 or else Flag_Num > 9
               then
                  Write_Char ('|');
               else
                  Write_Char (Character'Val (Character'Pos ('0') + Flag_Num));
               end if;

               P := P + 1;
            end if;

            T := Errors.Table (T).Next;
            Flag_Num := Flag_Num + 1;
         end loop;

         Write_Eol;
      end if;

      --  Now output the error messages

      T := E;

      while T /= No_Error_Msg
        and then Errors.Table (T).Line = Errors.Table (E).Line
        and then Errors.Table (T).Sfile = Errors.Table (E).Sfile

      loop
         Write_Str ("        >>> ");
         Output_Msg_Text (T);

         if Debug_Flag_2 then
            while Column < 74 loop
               Write_Char (' ');
            end loop;

            Write_Str (" <<<");
         end if;

         Write_Eol;
         T := Errors.Table (T).Next;
      end loop;

      E := T;
   end Output_Error_Msgs;

   ------------------------
   -- Output_Line_Number --
   ------------------------

   procedure Output_Line_Number (L : Logical_Line_Number) is
      D     : Int;       -- next digit
      C     : Character; -- next character
      Z     : Boolean;   -- flag for zero suppress
      N, M  : Int;       -- temporaries

   begin
      if L = 0 then
         Write_Str ("        ");

      else
         Z := False;
         N := Int (L);

         M := 100_000;
         while M /= 0 loop
            D := Int (N / M);
            N := N rem M;
            M := M / 10;

            if D = 0 then
               if Z then
                  C := '0';
               else
                  C := ' ';
               end if;
            else
               Z := True;
               C := Character'Val (D + 48);
            end if;

            Write_Char (C);
         end loop;

         Write_Str (". ");
      end if;
   end Output_Line_Number;

   ---------------------
   -- Output_Msg_Text --
   ---------------------

   procedure Output_Msg_Text (E : Error_Msg_Id) is
   begin
      if Errors.Table (E).Warn then
         Write_Str ("warning: ");

      elsif Hostparm.Tag_Errors then
         Write_Str ("error: ");
      end if;

      Write_Str (Errors.Table (E).Text.all);
   end Output_Msg_Text;

   ------------------------
   -- Output_Source_Line --
   ------------------------

   procedure Output_Source_Line
     (L     : Logical_Line_Number;
      Sfile : Source_File_Index;
      Errs  : Boolean)
   is
      S : Source_Ptr;
      C : Character;

      Line_Number_Output : Boolean := False;
      --  Set True once line number is output

   begin
      if Sfile /= Current_Error_Source_File then
         Write_Str ("==============Error messages for source file: ");
         Write_Name (Full_Ref_Name (Sfile));
         Write_Eol;
         Current_Error_Source_File := Sfile;
      end if;

      if Errs or List_Pragmas_Mode then
         Output_Line_Number (L);
         Line_Number_Output := True;
      end if;

      S := Line_Start (L, Sfile);

      loop
         C := Source_Text (Sfile) (S);
         exit when C = Ascii.LF or else C = Ascii.CR or else C = EOF;

         --  Deal with matching entry in List_Pragmas table

         if Full_List
           and then List_Pragmas_Index <= List_Pragmas.Last
           and then S = List_Pragmas.Table (List_Pragmas_Index).Ploc
         then

            case List_Pragmas.Table (List_Pragmas_Index).Ptyp is
               when Page =>
                  Write_Char (C);

                  --  Ignore if on line with errors so that error flags
                  --  get properly listed with the error line .


                  if not Errs then
                     Write_Char (Ascii.FF);
                  end if;

               when List_On =>
                  List_Pragmas_Mode := True;

                  if not Line_Number_Output then
                     Output_Line_Number (L);
                     Line_Number_Output := True;
                  end if;

                  Write_Char (C);

               when List_Off =>
                  Write_Char (C);
                  List_Pragmas_Mode := False;
            end case;

            List_Pragmas_Index := List_Pragmas_Index + 1;

         --  Normal case (no matching entry in List_Pragmas table)

         else
            if Errs or List_Pragmas_Mode then
               Write_Char (C);
            end if;
         end if;

         S := S + 1;
      end loop;

      if Line_Number_Output then
         Write_Eol;
      end if;
   end Output_Source_Line;

   -------------------
   -- Set_Msg_Blank --
   -------------------

   procedure Set_Msg_Blank is
   begin
      if Msglen > 0
        and then Msg_Buffer (Msglen) /= ' '
        and then Msg_Buffer (Msglen) /= '('
        and then not Manual_Quote_Mode
      then
         Set_Msg_Char (' ');
      end if;
   end Set_Msg_Blank;

   -------------------------------
   -- Set_Msg_Blank_Conditional --
   -------------------------------

   procedure Set_Msg_Blank_Conditional is
   begin
      if Msglen > 0
        and then Msg_Buffer (Msglen) /= ' '
        and then Msg_Buffer (Msglen) /= '('
        and then Msg_Buffer (Msglen) /= '"'
        and then not Manual_Quote_Mode
      then
         Set_Msg_Char (' ');
      end if;
   end Set_Msg_Blank_Conditional;

   ------------------
   -- Set_Msg_Char --
   ------------------

   procedure Set_Msg_Char (C : Character) is
   begin

      --  The check for message buffer overflow is needed to deal with cases
      --  where insertions get too long (in particular a child unit name can
      --  be very long).

      if Msglen < Max_Msg_Length then
         Msglen := Msglen + 1;
         Msg_Buffer (Msglen) := C;
      end if;
   end Set_Msg_Char;

   ------------------------------
   -- Set_Msg_Insertion_Column --
   ------------------------------

   procedure Set_Msg_Insertion_Column is
   begin
      if RM_Column_Check then
         Set_Msg_Str (" in column ");
         Set_Msg_Int (Int (Error_Msg_Col) + 1);
      end if;
   end Set_Msg_Insertion_Column;

   ---------------------------------
   -- Set_Msg_Insertion_File_Name --
   ---------------------------------

   procedure Set_Msg_Insertion_File_Name is
   begin
      if Error_Msg_Name_1 = No_Name then
         null;

      elsif Error_Msg_Name_1 = Error_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Set_Msg_Blank;
         Get_Decoded_Name_String (Error_Msg_Name_1);
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;

      --  The following assignments ensure that the second and third percent
      --  insertion characters will correspond to the Error_Msg_Name_2 and
      --  Error_Msg_Name_3 as required.

      Error_Msg_Name_1 := Error_Msg_Name_2;
      Error_Msg_Name_2 := Error_Msg_Name_3;

   end Set_Msg_Insertion_File_Name;

   -----------------------------------
   -- Set_Msg_Insertion_Line_Number --
   -----------------------------------

   procedure Set_Msg_Insertion_Line_Number (Loc, Flag : Source_Ptr) is
      Sindex_Loc  : Source_File_Index;
      Sindex_Flag : Source_File_Index;

   begin
      Set_Msg_Blank;

      if Loc = No_Location then
         Set_Msg_Str ("at unknown location");

      elsif Loc <= Standard_Location then
         Set_Msg_Str ("in package Standard");

         if Loc = Standard_Ascii_Location then
            Set_Msg_Str (".Ascii");
         end if;

      else
         --  Add "at file-name:" if reference is to other than the source
         --  file in which the error message is placed. Note that we check
         --  full file names, rather than just the source indexes, to
         --  deal with generic instantiations from the current file.

         Sindex_Loc  := Get_Source_File_Index (Loc);
         Sindex_Flag := Get_Source_File_Index (Flag);

         if Full_File_Name (Sindex_Loc) /= Full_File_Name (Sindex_Flag) then
            Set_Msg_Str ("at ");
            Get_Decoded_Name_String
              (Reference_Name (Get_Source_File_Index (Loc)));
            Set_Msg_Name_Buffer;
            Set_Msg_Char (':');

         --  If in current file, add text "at line "

         else
            Set_Msg_Str ("at line ");
         end if;

         --  Output line number for reference

         Set_Msg_Int (Int (Get_Line_Number (Loc)));

         --  Deal with the instantiation case. We may have a reference to,
         --  e.g. a type, that is declared within a generic template, and
         --  what we are really referring to is the occurrence in an instance.
         --  In this case, the line number of the instantiation is also of
         --  interest, and we add a notation:

         --    , instance at xxx

         --  where xxx is a line number output using this same routine (and
         --  the recursion can go further if the instantiation is itself in
         --  a generic template).

         --  The flag location passed to us in this situation is indeed the
         --  line number within the template, but as described in Sinput.L
         --  (file sinput-l.ads, section "Handling Generic Instantiations")
         --  we can retrieve the location of the instantiation itself from
         --  this flag location value.

         --  Note: this processing is suppressed if Suppress_Instance_Location
         --  is set True. This is used to prevent redundant annotations of the
         --  location of the instantiation in the case where we are placing
         --  the messages on the instantiation in any case.

         if Instantiation (Sindex_Loc) /= No_Location
           and then not Suppress_Instance_Location
         then
            Set_Msg_Str (", instance ");
            Set_Msg_Insertion_Line_Number (Instantiation (Sindex_Loc), Flag);
         end if;
      end if;
   end Set_Msg_Insertion_Line_Number;

   ----------------------------
   -- Set_Msg_Insertion_Name --
   ----------------------------

   procedure Set_Msg_Insertion_Name is
   begin
      if Error_Msg_Name_1 = No_Name then
         null;

      elsif Error_Msg_Name_1 = Error_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Set_Msg_Blank_Conditional;
         Get_Decoded_Name_String (Error_Msg_Name_1);

         --  Remove %s or %b at end. These come from unit names. If the
         --  caller wanted the (unit) or (body), then they would have used
         --  the $ insertion character. Certainly no error message should
         --  ever have %b or %s explicitly occurring.

         if Name_Len > 2
           and then Name_Buffer (Name_Len - 1) = '%'
         then
            Name_Len := Name_Len - 2;
         end if;

         --  If operator name or character literal name, just print it as is
         --  Also print as is if it ends in a right paren (case of x'val(nnn))

         if Name_Buffer (1) = '"'
           or else Name_Buffer (1) = '''
           or else Name_Buffer (Name_Len) = ')'
         then
            Set_Msg_Name_Buffer;

         --  Else output with surrounding quotes in proper casing mode

         else
            Set_Casing (Identifier_Casing (Flag_Source), Mixed_Case);
            Set_Msg_Quote;
            Set_Msg_Name_Buffer;
            Set_Msg_Quote;
         end if;
      end if;

      --  The following assignments ensure that the second and third percent
      --  insertion characters will correspond to the Error_Msg_Name_2 and
      --  Error_Msg_Name_3 as required.

      Error_Msg_Name_1 := Error_Msg_Name_2;
      Error_Msg_Name_2 := Error_Msg_Name_3;

   end Set_Msg_Insertion_Name;

   ----------------------------
   -- Set_Msg_Insertion_Node --
   ----------------------------

   procedure Set_Msg_Insertion_Node is
   begin
      Suppress_Message :=
        Error_Msg_Node_1 = Error
          or else Error_Msg_Node_1 = Any_Type;

      if Error_Msg_Node_1 = Empty then
         Set_Msg_Blank_Conditional;
         Set_Msg_Str ("<empty>");

      elsif Error_Msg_Node_1 = Error then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      elsif Error_Msg_Node_1 = Standard_Void_Type then
         Set_Msg_Blank;
         Set_Msg_Str ("procedure name");

      else
         Set_Msg_Blank_Conditional;

         --  Skip quotes for operator case

         if Nkind (Error_Msg_Node_1) in N_Op then
            Set_Msg_Node (Error_Msg_Node_1);

         else
            Set_Msg_Quote;
            Set_Msg_Node (Error_Msg_Node_1);
            Set_Msg_Quote;
         end if;
      end if;

      --  The following assignment ensures that a second ampersand insertion
      --  character will correspond to the Error_Msg_Node_2 parameter.

      Error_Msg_Node_1 := Error_Msg_Node_2;

   end Set_Msg_Insertion_Node;

   -------------------------------------
   -- Set_Msg_Insertion_Reserved_Name --
   -------------------------------------

   procedure Set_Msg_Insertion_Reserved_Name is
   begin
      Set_Msg_Blank_Conditional;
      Get_Name_String (Error_Msg_Name_1);
      Set_Msg_Quote;
      Set_Casing (Keyword_Casing (Flag_Source), All_Lower_Case);
      Set_Msg_Name_Buffer;
      Set_Msg_Quote;
   end Set_Msg_Insertion_Reserved_Name;

   -------------------------------------
   -- Set_Msg_Insertion_Reserved_Word --
   -------------------------------------

   procedure Set_Msg_Insertion_Reserved_Word
     (Text : String;
      J    : in out Integer)
   is
   begin
      Set_Msg_Blank_Conditional;
      Name_Len := 0;

      while J <= Text'Last and then Text (J) in 'A' .. 'Z' loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Text (J);
         J := J + 1;
      end loop;

      Set_Casing (Keyword_Casing (Flag_Source), All_Lower_Case);
      Set_Msg_Quote;
      Set_Msg_Name_Buffer;
      Set_Msg_Quote;
   end Set_Msg_Insertion_Reserved_Word;

   --------------------------------------
   -- Set_Msg_Insertion_Type_Reference --
   --------------------------------------

   procedure Set_Msg_Insertion_Type_Reference (Flag : Source_Ptr) is
   begin
      Set_Msg_Blank;

      if Error_Msg_Node_1 = Standard_Void_Type then
         Set_Msg_Str ("procedure name instead of function");
         return;

      elsif Error_Msg_Node_1 = Standard_Exception_Type then
         Set_Msg_Str ("an exception");
         return;

      elsif     Error_Msg_Node_1 = Any_Access
        or else Error_Msg_Node_1 = Any_Boolean
        or else Error_Msg_Node_1 = Any_Character
        or else Error_Msg_Node_1 = Any_Composite
        or else Error_Msg_Node_1 = Any_Discrete
        or else Error_Msg_Node_1 = Any_Fixed
        or else Error_Msg_Node_1 = Any_Integer
        or else Error_Msg_Node_1 = Any_Modular
        or else Error_Msg_Node_1 = Any_Numeric
        or else Error_Msg_Node_1 = Any_Real
        or else Error_Msg_Node_1 = Any_Scalar
        or else Error_Msg_Node_1 = Any_String
      then
         Get_Decoded_Name_String (Chars (Error_Msg_Node_1));
         Set_Msg_Name_Buffer;
         return;

      elsif Error_Msg_Node_1 = Universal_Real then
         Set_Msg_Str ("type universal real");
         return;

      elsif Error_Msg_Node_1 = Universal_Integer then
         Set_Msg_Str ("type universal integer");
         return;

      elsif Error_Msg_Node_1 = Universal_Fixed then
         Set_Msg_Str ("type universal fixed");
         return;
      end if;

      --  Special case of anonymous array

      if Nkind (Error_Msg_Node_1) in N_Entity
        and then Is_Array_Type (Error_Msg_Node_1)
        and then Present (Related_Array_Object (Error_Msg_Node_1))
      then
         Set_Msg_Str ("type of ");
         Set_Msg_Node (Related_Array_Object (Error_Msg_Node_1));
         Set_Msg_Str (" declared");
         Set_Msg_Insertion_Line_Number
           (Sloc (Related_Array_Object (Error_Msg_Node_1)), Flag);
         return;
      end if;

      --  If we fall through, it is not a special case, so first output
      --  the name of the type, preceded by private for a private type

      if Is_Private_Type (Error_Msg_Node_1) then
         Set_Msg_Str ("private type ");
      else
         Set_Msg_Str ("type ");
      end if;

      --  Types in Standard are displayed as "Standard.name"

      if Sloc (Error_Msg_Node_1) <= Standard_Location then
         Set_Msg_Quote;
         Set_Msg_Str ("Standard.");
         Set_Msg_Node (Error_Msg_Node_1);
         Set_Msg_Quote;

      else
         declare
            Loc      : constant Source_Ptr       := Sloc (Error_Msg_Node_1);
            Unit_Num : constant Unit_Number_Type := Get_Sloc_Unit_Number (Loc);

         begin
            --  Types in other language defined units are displayed as
            --  "package-name.type-name"

            if Is_Predefined_File_Name (Unit_File_Name (Unit_Num)) then
               Get_Decoded_Name_String (Unit_Name (Unit_Num));
               Name_Len := Name_Len - 2;
               Set_Msg_Quote;
               Set_Casing (Mixed_Case);
               Set_Msg_Name_Buffer;
               Set_Msg_Char ('.');
               Set_Casing (Mixed_Case);
               Set_Msg_Node (Error_Msg_Node_1);
               Set_Msg_Quote;

            --  All other types display as "type name" defined at line xxx
            --  possibly qualified if qualification is requested.

            else
               declare
                  procedure Set_Qualification (N : Int; E : Entity_Id);
                  --  Little recursive procedure to set N levels of
                  --  scope qualification for given entity.

                  procedure Set_Qualification (N : Int; E : Entity_Id) is
                  begin
                     if N = 0 then
                        return;

                     else
                        Set_Qualification (N - 1, Scope (E));
                        Set_Msg_Node (Scope (E));
                        Set_Msg_Char ('.');
                     end if;
                  end Set_Qualification;

               begin
                  Set_Msg_Quote;
                  Set_Qualification (Error_Msg_Qual_Level, Error_Msg_Node_1);
                  Set_Msg_Node (Error_Msg_Node_1);
                  Set_Msg_Quote;
                  Set_Msg_Str (" defined");
                  Set_Msg_Insertion_Line_Number
                     (Sloc (Error_Msg_Node_1), Flag);
               end;
            end if;
         end;
      end if;

   end Set_Msg_Insertion_Type_Reference;

   ----------------------------
   -- Set_Msg_Insertion_Uint --
   ----------------------------

   procedure Set_Msg_Insertion_Uint is
   begin
      Set_Msg_Blank;
      UI_Image (Error_Msg_Uint_1);

      for J in 1 .. UI_Image_Length loop
         Set_Msg_Char (UI_Image_Buffer (J));
      end loop;

      --  The following assignment ensures that a second carret insertion
      --  character will correspond to the Error_Msg_Uint_2 parameter.

      Error_Msg_Uint_1 := Error_Msg_Uint_2;
   end Set_Msg_Insertion_Uint;

   ---------------------------------
   -- Set_Msg_Insertion_Unit_Name --
   ---------------------------------

   procedure Set_Msg_Insertion_Unit_Name is
   begin
      if Error_Msg_Unit_1 = No_Name then
         null;

      elsif Error_Msg_Unit_1 = Error_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Get_Unit_Name_String (Error_Msg_Unit_1);
         Set_Msg_Blank;
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;

      --  The following assignment ensures that a second percent insertion
      --  character will correspond to the Error_Msg_Unit_2 parameter.

      Error_Msg_Unit_1 := Error_Msg_Unit_2;

   end Set_Msg_Insertion_Unit_Name;

   -----------------
   -- Set_Msg_Int --
   -----------------

   procedure Set_Msg_Int (Line : Int) is
   begin
      if Line > 9 then
         Set_Msg_Int (Line / 10);
      end if;

      Set_Msg_Char (Character'Val (Character'Pos ('0') + (Line rem 10)));
   end Set_Msg_Int;

   -------------------------
   -- Set_Msg_Name_Buffer --
   -------------------------

   procedure Set_Msg_Name_Buffer is
   begin
      for J in 1 .. Name_Len loop
         Set_Msg_Char (Name_Buffer (J));
      end loop;
   end Set_Msg_Name_Buffer;

   ------------------
   -- Set_Msg_Node --
   ------------------

   procedure Set_Msg_Node (Node : Node_Id) is
      Ent         : Entity_Id;
      Old_Ent     : Entity_Id;
      Mchar       : Character;
      Derived     : Boolean := False;
      Class_Flag  : Boolean := False;
      Nam         : Name_Id;

      procedure Kill_Type;
      --  If message buffer ends with " type ", then remove the last five
      --  characters. This is used to avoid a duplication of "type" in the
      --  text of the message with "type" generated by the special tests
      --  below (e.g. we don't want "expected type type derived from ..")

      procedure Kill_Type is
      begin
         if Msglen > 4
           and then Msg_Buffer (Msglen - 4 .. Msglen) = "type "
           and then (Msglen = 5 or else Msg_Buffer (Msglen - 5) = ' ')
         then
            Msglen := Msglen - 5;
         end if;
      end Kill_Type;

   --  Start of processing for Set_Msg_Node

   begin
      if Nkind (Node) = N_Designator then
         Set_Msg_Node (Name (Node));
         Set_Msg_Char ('.');
         Set_Msg_Node (Identifier (Node));
         return;

      elsif Nkind (Node) = N_Defining_Program_Unit_Name then
         Set_Msg_Node (Name (Node));
         Set_Msg_Char ('.');
         Set_Msg_Node (Defining_Identifier (Node));
         return;

      elsif Nkind (Node) = N_Selected_Component then
         Set_Msg_Node (Prefix (Node));
         Set_Msg_Char ('.');
         Set_Msg_Node (Selector_Name (Node));
         return;
      end if;

      --  The only remaining possibilities are identifiers, defining
      --  identifiers, pragmas, and pragma argument associations, i.e.
      --  nodes that have a Chars field.

      --  Internal names generally represent something gone wrong. An exception
      --  is the case of internal type names, where we try to find a reasonable
      --  external representation for the external name

      if Is_Internal_Name (Chars (Node))
        and then
          ((Is_Entity_Name (Node)
                          and then Present (Entity (Node))
                          and then Is_Type (Entity (Node)))
              or else
           (Nkind (Node) = N_Defining_Identifier and then Is_Type (Node)))
      then
         if Nkind (Node) = N_Identifier then
            Ent := Entity (Node);
         else
            Ent := Node;
         end if;

         --  Undo placement of a quote, since we will put it back later

         Mchar := Msg_Buffer (Msglen);

         if Mchar = '"' then
            Msglen := Msglen - 1;
         end if;

         --  The loop here deals with recursive types, we are trying to
         --  find a related entity that is not an implicit type. Note
         --  that the check with Old_Ent stops us from getting "stuck".
         --  Also, we don't output the "type derived from" message more
         --  than once in the case where we climb up multiple levels.

         loop
            Old_Ent := Ent;

            --  Implicit access type, use directly designated type

            if Is_Access_Type (Ent) then
               Set_Msg_Str ("access to ");
               Ent := Directly_Designated_Type (Ent);

            --  Classwide type

            elsif Is_Class_Wide_Type (Ent) then
               Class_Flag := True;
               Ent := Root_Type (Ent);

            --  Use base type if this is a subtype

            elsif Ent /= Base_Type (Ent) then
               Kill_Type;
               Set_Msg_Str ("subtype of ");
               Ent := Base_Type (Ent);

            --  If this is a base type with a first named subtype, use the
            --  first named subtype instead. This is not quite accurate in
            --  all cases, but it makes too much noise to be accurate and
            --  add 'Base in all cases. Note that we only do this is the
            --  first named subtype is not itself an internal name. This
            --  avoids the obvious loop (subtype->basetype->subtype) which
            --  would otherwise occur!)

            elsif Present (Freeze_Node (Ent))
              and then Present (First_Subtype_Link (Freeze_Node (Ent)))
              and then
                not Is_Internal_Name
                      (Chars (First_Subtype_Link (Freeze_Node (Ent))))
            then
               Ent := First_Subtype_Link (Freeze_Node (Ent));

            --  Otherwise use root type

            else
               if not Derived then
                  Kill_Type;
                  Set_Msg_Str ("type derived from ");
                  Derived := True;
               end if;

               Ent := Etype (Ent);
            end if;

            --  If we are stuck in a loop, get out and settle for the
            --  internal name after all.

            exit when Ent = Old_Ent;

            --  Get out if we finally found a non-internal name to use

            exit when not Is_Internal_Name (Chars (Ent));
         end loop;

         if Mchar = '"' then
            Set_Msg_Char ('"');
         end if;

         Nam := Chars (Ent);

      --  For any other internal names, we settle for using the name

      else
         Nam := Chars (Node);
      end if;

      --  If we still have an internal name, then set to kill the message
      --  if it is not the first message (we really try hard not to show
      --  the dirty laundry of the implementation to the compiler user!)

      if Is_Internal_Name (Nam) then
         Kill_Message := True;
      end if;

      --  At this stage, the name to output is in Nam

      Get_Decoded_Name_String (Nam);

      --  If we have any of the names from standard that start with the
      --  characters "any " (e.g. Any_Type), then kill the message since
      --  almost certainly it is a junk cascaded message.

      if Name_Len > 4
        and then Name_Buffer (1 .. 4) = "any "
      then
         Kill_Message := True;
      end if;

      --  Now we have to set the proper case. If we have a source location
      --  then do a check to see if the name in the source is the same name
      --  as the name in the Names table, except for possible differences
      --  in case, which is the case when we can copy from the source.

      declare
         Src_Loc : constant Source_Ptr := Sloc (Error_Msg_Node_1);
         Sbuffer : Source_Buffer_Ptr;
         Ref_Ptr : Integer;
         Src_Ptr : Source_Ptr;

      begin
         Ref_Ptr := 1;
         Src_Ptr := Src_Loc;

         --  Determine if the reference we are dealing with corresponds
         --  to text at the point of the error reference. This will often
         --  be the case for simple identifier references, and is the case
         --  where we can copy the spelling from the source.

         if Src_Loc /= No_Location
           and then Src_Loc > Standard_Location
         then
            Sbuffer := Source_Text (Get_Source_File_Index (Src_Loc));

            while Ref_Ptr <= Name_Len loop
               exit when
                 Fold_Lower (Sbuffer (Src_Ptr)) /=
                 Fold_Lower (Name_Buffer (Ref_Ptr));
               Ref_Ptr := Ref_Ptr + 1;
               Src_Ptr := Src_Ptr + 1;
            end loop;
         end if;

         --  If we get through the loop without a mismatch, then output
         --  the name the way it is spelled in the source program

         if Ref_Ptr > Name_Len then
            Src_Ptr := Src_Loc;

            for J in 1 .. Name_Len loop
               Name_Buffer (J) := Sbuffer (Src_Ptr);
               Src_Ptr := Src_Ptr + 1;
            end loop;

         --  Otherwise set the casing using the default identifier casing

         else
            Set_Casing (Identifier_Casing (Flag_Source), Mixed_Case);
         end if;
      end;

      Set_Msg_Name_Buffer;

      --  Add 'Class if class wide type

      if Class_Flag then
         Set_Msg_Char (''');
         Get_Name_String (Name_Class);
         Set_Casing (Identifier_Casing (Flag_Source), Mixed_Case);
         Set_Msg_Name_Buffer;
      end if;
   end Set_Msg_Node;

   -------------------
   -- Set_Msg_Quote --
   -------------------

   procedure Set_Msg_Quote is
   begin
      if not Manual_Quote_Mode then
         Set_Msg_Char ('"');
      end if;
   end Set_Msg_Quote;

   -----------------
   -- Set_Msg_Str --
   -----------------

   procedure Set_Msg_Str (Text : String) is
   begin
      for J in Text'Range loop
         Set_Msg_Char (Text (J));
      end loop;
   end Set_Msg_Str;

   ------------------
   -- Set_Msg_Text --
   ------------------

   procedure Set_Msg_Text (Text : String; Flag : Source_Ptr) is
      C : Character;         -- Current character
      P : Natural;           -- Current index;

   begin
      Manual_Quote_Mode := False;
      Is_Warning_Msg := False;
      Is_Unconditional_Msg := False;
      Msglen := 0;
      Flag_Source := Get_Source_File_Index (Flag);
      P := Text'First;

      while P <= Text'Last loop
         C := Text (P);
         P := P + 1;

         --  Check for insertion character

         if C = '%' then
            Set_Msg_Insertion_Name;

         elsif C = '$' then
            Set_Msg_Insertion_Unit_Name;

         elsif C = '{' then
            Set_Msg_Insertion_File_Name;

         elsif C = '}' then
            Set_Msg_Insertion_Type_Reference (Flag);

         elsif C = '*' then
            Set_Msg_Insertion_Reserved_Name;

         elsif C = '&' then
            Set_Msg_Insertion_Node;

         elsif C = '#' then
            Set_Msg_Insertion_Line_Number (Error_Msg_Sloc, Flag);

         elsif C = '\' then
            Continuation := True;

         elsif C = '@' then
            Set_Msg_Insertion_Column;

         elsif C = '^' then
            Set_Msg_Insertion_Uint;

         elsif C = '`' then
            Manual_Quote_Mode := not Manual_Quote_Mode;
            Set_Msg_Char ('"');

         elsif C = '!' then
            Is_Unconditional_Msg := True;

         elsif C = '?' then
            Is_Warning_Msg := True;

         elsif C = ''' then
            Set_Msg_Char (Text (P));
            P := P + 1;

         --  Upper case letter (start of reserved word if 2 or more)

         elsif C in 'A' .. 'Z'
           and then P <= Text'Last
           and then Text (P) in 'A' .. 'Z'
         then
            P := P - 1;
            Set_Msg_Insertion_Reserved_Word (Text, P);

         --  Normal character with no special treatment

         else
            Set_Msg_Char (C);
         end if;

      end loop;
   end Set_Msg_Text;

   ----------------
   -- Set_Posted --
   ----------------

   procedure Set_Posted (N : Node_Id) is
      P : Node_Id;

   begin

      --  We always set Error_Posted on the node itself

      Set_Error_Posted (N);

      --  If it is a subexpression, then set Error_Posted on parents
      --  up to and including the first non-subexpression construct

      P := N;
      loop
         P := Parent (P);
         Set_Error_Posted (P);
         exit when Nkind (P) not in N_Subexpr;
      end loop;
   end Set_Posted;

   ---------------------------
   -- Set_Warnings_Mode_Off --
   ---------------------------

   procedure Set_Warnings_Mode_Off (Loc : Source_Ptr) is
   begin
      --  Don't bother with entries from instantiation copies, since we
      --  will already have a copy in the template, which is what matters

      if Instantiation (Get_Source_File_Index (Loc)) /= No_Location then
         return;
      end if;

      --  If last entry in table already covers us, this is a redundant
      --  pragma Warnings (Off) and can be ignored. This also handles the
      --  case where all warnings are suppressed by command line switch.

      if Warnings.Last >= Warnings.First
        and then Warnings.Table (Warnings.Last).Start <= Loc
        and then Loc <= Warnings.Table (Warnings.Last).Stop
      then
         return;

      --  Otherwise establish a new entry, extending from the location of
      --  the pragma to the end of the current source file. This ending
      --  point will be adjusted by a subsequent pragma Warnings (On).

      else
         Warnings.Increment_Last;
         Warnings.Table (Warnings.Last).Start := Loc;
         Warnings.Table (Warnings.Last).Stop :=
           Source_Last (Current_Source_File);
      end if;
   end Set_Warnings_Mode_Off;

   --------------------------
   -- Set_Warnings_Mode_On --
   --------------------------

   procedure Set_Warnings_Mode_On (Loc : Source_Ptr) is
   begin
      --  Don't bother with entries from instantiation copies, since we
      --  will already have a copy in the template, which is what matters

      if Instantiation (Get_Source_File_Index (Loc)) /= No_Location then
         return;
      end if;

      --  Nothing to do unless command line switch to suppress all warnings
      --  is off, and the last entry in the warnings table covers this
      --  pragma Warnings (On), in which case adjust the end point.

      if (Warnings.Last >= Warnings.First
           and then Warnings.Table (Warnings.Last).Start <= Loc
           and then Loc <= Warnings.Table (Warnings.Last).Stop)
        and then Warning_Mode /= Suppress
      then
         Warnings.Table (Warnings.Last).Stop := Loc;
      end if;
   end Set_Warnings_Mode_On;

end Errout;
