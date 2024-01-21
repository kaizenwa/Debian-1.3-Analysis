------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B I N D E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.23 $                             --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc. --
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

with Binderr; use Binderr;
with Butil;   use Butil;
with Debug;   use Debug;
with Namet;   use Namet;
with Opt;     use Opt;
with Output;  use Output;

package body Binde is

   --  The following data structures are used to represent the graph that is
   --  used to determine the elaboration order (using a topological sort).

   --  The following structures are used to record successors. If A is a
   --  successor of B in this table, it means that A must be elaborated
   --  before B is elaborated.

   type Successor_Id is new Nat;
   --  Identification of single successor entry

   No_Successor : constant Successor_Id := 0;
   --  Used to indicate end of list of successors

   type Succ_Reason is (Withed, Elab, Elab_All, Spec_First);
   --  Reason for existence of successor link

   type Successor_Link is record
      Before : Unit_Id;
      --  Predecessor unit

      After : Unit_Id;
      --  Successor unit

      Next : Successor_Id;
      --  Next successor on this list

      Reason : Succ_Reason;
      --  Reason for this link

      Reason_Unit : Unit_Id;
      --  For Reason = Elab, or Elab_All, records the unit containing the
      --  pragma leading to the link.
   end record;

   Succ_First : constant := 1;

   package Succ is new Table (
     Table_Component_Type => Successor_Link,
     Table_Index_Type     => Successor_Id,
     Table_Low_Bound      => Succ_First,
     Table_Initial        => 500,
     Table_Increment      => 200,
     Table_Name           => "Succ");

   --  A Unit_Node record is built for each active unit

   type Unit_Node_Record is record

      Successors : Successor_Id;
      --  Pointer to list of links for successor nodes

      Num_Pred : Int;
      --  Number of predecessors for this unit. Normally non-negative, but
      --  can go negative in the case of units chosen by the diagnose error
      --  procedure (when cycles are being removed from the graph).

      Nextnp : Unit_Id;
      --  Forward pointer for list of units with no predecessors

      Elab_Order : Nat;
      --  Position in elaboration order (zero = not placed yet)

      Visited : Boolean;
      --  Used in computing transitive closure for elaborate all and
      --  also in locating cycles and paths in the diagnose routines.

      Elab_Position : Natural;
      --  Initialized to zero. Set non-zero when a unit is chosen and
      --  placed in the elaboration order. The value represents the
      --  ordinal position in the elaboration order.

   end record;

   package UNR is new Table (
     Table_Component_Type => Unit_Node_Record,
     Table_Index_Type     => Unit_Id,
     Table_Low_Bound      => First_Unit_Entry,
     Table_Initial        => 500,
     Table_Increment      => 200,
     Table_Name           => "UNR");

   No_Pred : Unit_Id;
   --  Head of list of items with no predecessors

   Num_Left : Int;
   --  Number of entries not yet dealt with

   Cur_Unit : Unit_Id;
   --  Current unit, set by Gather_Dependencies, and picked up in Build_Link
   --  to set the Reason_Unit field of the created dependency link.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Better_Choice (U1, U2 : Unit_Id) return Boolean;
   --  U1 and U2 are both permitted candidates for selection as the next unit
   --  to be elaborated. This function determines whether U1 is a better choice
   --  than U2, i.e. should be elaborated in preference to U2, based on a set
   --  of heuristics that establish a friendly and predictable order (see body
   --  for details). The result is True if U1 is a better choice than U2, and
   --  False if it is a worse choice, or there is no preference between them.

   procedure Build_Link (Before, After : Unit_Id; R : Succ_Reason);
   --  Establish a successor link, Before must be elaborated before After,
   --  and the reason for the link is R.

   procedure Choose (Chosen : Unit_Id);
   --  Chosen is the next entry chosen in the elaboration order. This
   --  procedure updates all data structures appropriately.

   function Corresponding_Body (U : Unit_Id) return Unit_Id;
   --  Given a unit which is a spec for which there is a separate body,
   --  return the unit id of the body. It is an error to call this routine
   --  with a unit that is not a spec, or which does not have a separate body.

   function Corresponding_Spec (U : Unit_Id) return Unit_Id;
   --  Given a unit which is a body for which there is a separate spec,
   --  return the unit id of the spec. It is an error to call this routine
   --  with a unit that is not a body, or which does not have a separate spec.

   procedure Diagnose_Elaboration_Problem;
   --  Called when no elaboration order can be found, either because of a
   --  cycle, or because of an Elaborate_Body pragma that can't be satisfied.
   --  Outputs an appropriate error message, and then abandons the bind.

   procedure Elab_All_Links (Before, After : Unit_Id);
   --  Used to compute the transitive closure of elaboration links for an
   --  Elaborate_All pragma. Unit After has a pragma Elaborate_All, and
   --  unit Before must be previously elaborated. First a link is built
   --  making sure that unit Before is elaborated before After, then a
   --  recursive call ensures that we also build links for any units needed
   --  by Before (i.e. these units must also be elaborated before After).

   procedure Elab_Error_Msg (S : Successor_Id);
   --  Given a successor link, outputs an error message of the form
   --  "& must be elaborated before & ..." where ... is the reason.

   procedure Gather_Dependencies;
   --  Compute dependencies, building the Succ and UNR tables

   function Unit_Id_Of (Uname : Unit_Name_Type) return Unit_Id;
   --  This function uses the Info field set in the names table to obtain
   --  the unit Id of a unit, given its name id value.

   function Worse_Choice (U1, U2 : Unit_Id) return Boolean;
   --  This is like Better_Choice, and has the same interface, but returns
   --  true if U1 is a worse choice than U2 in the sense of the -h (horrible
   --  elaboration order) switch. We still have to obey Ada rules, so it is
   --  not quite the direct inverse of Better_Choice.

   procedure Write_Dependencies;
   --  Write out dependencies (called only if appropriate option is set)

   -------------------
   -- Better_Choice --
   -------------------

   function Better_Choice (U1, U2 : Unit_Id) return Boolean is

      function Body_Unit (U : Unit_Id) return Boolean;
      --  Determines if given unit is a body

      function Waiting_Body (U : Unit_Id) return Boolean;
      --  Determines if U is a waiting body, defined as a body which has
      --  not been elaborated, but whose spec has been elaborated.

      function Body_Unit (U : Unit_Id) return Boolean is
      begin
         return Unit.Table (U).Utype = Is_Body
           or else Unit.Table (U).Utype = Is_Body_Only;
      end Body_Unit;

      function Waiting_Body (U : Unit_Id) return Boolean is
      begin
         return Unit.Table (U).Utype = Is_Body and then
            UNR.Table (Corresponding_Spec (U)).Elab_Position /= 0;
      end Waiting_Body;

   --  Start of processing for Better_Choice

   --  Note: the checks here are applied in sequence, and the ordering is
   --  significant (i.e. the more important criteria are applied first).

   begin
      --  Prefer a waiting body to any other case

      if Waiting_Body (U1) and not Waiting_Body (U2) then
         return True;

      elsif Waiting_Body (U2) and not Waiting_Body (U1) then
         return False;

      --  Prefer a predefined unit to a non-predefined unit

      elsif Unit.Table (U1).Predefined and not Unit.Table (U2).Predefined then
         return True;

      elsif Unit.Table (U2).Predefined and not Unit.Table (U1).Predefined then
         return False;

      --  Prefer a body to a spec

      elsif Body_Unit (U1) and not Body_Unit (U2) then
         return True;

      elsif Body_Unit (U2) and not Body_Unit (U1) then
         return False;

      --  If both are waiting bodies, then prefer the one whose spec is
      --  more recently elaborated. Consider the following:

      --     spec of A
      --     spec of B
      --     body of A or B?

      --  The normal waiting body preference would have placed the body of
      --  A before the spec of B if it could. Since it could not, there it
      --  must be the case that A depends on B. It is therefore a good idea
      --  to put the body of B first.

      elsif Waiting_Body (U1) and then Waiting_Body (U2) then
         return
           UNR.Table (Corresponding_Spec (U1)).Elab_Position >
           UNR.Table (Corresponding_Spec (U2)).Elab_Position;

      --  Otherwise decide on the basis of alphabetical order

      else
         return Uname_Less (Unit.Table (U1).Uname, Unit.Table (U2).Uname);
      end if;
   end Better_Choice;

   ----------------
   -- Build_Link --
   ----------------

   procedure Build_Link (Before, After : Unit_Id; R : Succ_Reason) is
   begin
      Succ.Increment_Last;
      Succ.Table (Succ.Last).Before      := Before;
      Succ.Table (Succ.Last).After       := After;
      Succ.Table (Succ.Last).Next        := UNR.Table (Before).Successors;
      UNR.Table (Before).Successors      := Succ.Last;
      Succ.Table (Succ.Last).Reason      := R;
      Succ.Table (Succ.Last).Reason_Unit := Cur_Unit;
      UNR.Table (After).Num_Pred         := UNR.Table (After).Num_Pred + 1;
   end Build_Link;

   ------------
   -- Choose --
   ------------

   Num_Chosen : Natural := 0;
   --  Number of units chosen

   procedure Choose (Chosen : Unit_Id) is
      S : Successor_Id;
      U : Unit_Id;

   begin
      if Debug_Flag_C then
         Write_Str ("Choosing Unit ");
         Write_Unit_Name (Unit.Table (Chosen).Uname);
         Write_Eol;
      end if;

      --  Add to elaboration order. Note that units having no elaboration
      --  code are not treated specially yet. The special casing of this
      --  is in Bindgen, where Gen_Elab_Calls skips over them. Meanwhile
      --  we need them here, because the object file list is also driven
      --  by the contents of the Elab_Order table.

      Elab_Order.Increment_Last;
      Elab_Order.Table (Elab_Order.Last) := Chosen;

      --  Remove from No_Pred list. This is a little inefficient and may
      --  be we should doubly link the list, but it will do for now!

      if No_Pred = Chosen then
         No_Pred := UNR.Table (Chosen).Nextnp;

      else
         --  Note that we just ignore the situation where it does not
         --  appear in the No_Pred list, this happens in calls from the
         --  Diagnose_Elaboration_Problem routine, where cycles are being
         --  removed arbitrarily from the graph.

         U := No_Pred;
         while U /= No_Unit_Id loop
            if UNR.Table (U).Nextnp = Chosen then
               UNR.Table (U).Nextnp := UNR.Table (Chosen).Nextnp;
               exit;
            end if;

            U := UNR.Table (U).Nextnp;
         end loop;
      end if;

      --  For all successors, decrement the number of predecessors, and
      --  if it becomes zero, then add to no predecessor list.

      S := UNR.Table (Chosen).Successors;

      while S /= No_Successor loop
         U := Succ.Table (S).After;
         UNR.Table (U).Num_Pred := UNR.Table (U).Num_Pred - 1;

         if Debug_Flag_N then
            Write_Str ("  decrementing Num_Pred for unit ");
            Write_Unit_Name (Unit.Table (U).Uname);
            Write_Str (" new value = ");
            Write_Int (Int (UNR.Table (U).Num_Pred));
            Write_Eol;
         end if;

         if UNR.Table (U).Num_Pred = 0 then
            UNR.Table (U).Nextnp := No_Pred;
            No_Pred := U;
         end if;

         S := Succ.Table (S).Next;
      end loop;

      --  All done, adjust number of units left count and set elaboration pos

      Num_Left := Num_Left - 1;
      Num_Chosen := Num_Chosen + 1;
      UNR.Table (Chosen).Elab_Position := Num_Chosen;
   end Choose;

   ------------------------
   -- Corresponding_Body --
   ------------------------

   --  Currently if the body and spec are separate, then they appear as
   --  two separate units in the same ALI file, with the body appearing
   --  first and the spec appearing second.

   function Corresponding_Body (U : Unit_Id) return Unit_Id is
   begin
      pragma Assert (Unit.Table (U).Utype = Is_Spec);
      return U - 1;
   end Corresponding_Body;

   ------------------------
   -- Corresponding_Spec --
   ------------------------

   --  Currently if the body and spec are separate, then they appear as
   --  two separate units in the same ALI file, with the body appearing
   --  first and the spec appearing second.

   function Corresponding_Spec (U : Unit_Id) return Unit_Id is
   begin
      pragma Assert (Unit.Table (U).Utype = Is_Body);
      return U + 1;
   end Corresponding_Spec;

   ----------------------------------
   -- Diagnose_Elaboration_Problem --
   ----------------------------------

   procedure Diagnose_Elaboration_Problem is
      U : Unit_Id;

      function Find_Path (Ufrom, Uto : Unit_Id; ML : Nat) return Boolean;
      --  Recursive routine used to find a path from node Ufrom to node Uto.
      --  If a path exists, returns True and outputs an appropriate set of
      --  error messages giving the path. Also calls Choose for each of the
      --  nodes so that they get removed from the remaining set. There are
      --  two cases of calls, either Ufrom = Uto for an attempt to find a
      --  cycle, or Ufrom is a spec and Uto the corresponding body for the
      --  case of an unsatisfiable Elaborate_Body pragma. ML is the minimum
      --  acceptable length for a path.

      ---------------
      -- Find_Path --
      ---------------

      function Find_Path (Ufrom, Uto : Unit_Id; ML : Nat) return Boolean is

         function Find_Link (U : Unit_Id; PL : Nat) return Boolean;
         --  This is the inner recursive routine, it determines if a path
         --  exists from U to Uto, and if so returns True and outputs the
         --  appropriate set of error messages. PL is the path length

         ---------------
         -- Find_Link --
         ---------------

         function Find_Link (U : Unit_Id; PL : Nat) return Boolean is
            S : Successor_Id;

         begin
            --  Recursion ends if we are at terminating node and the path
            --  is sufficiently long, generate error message and return True.

            if U = Uto and then PL >= ML then
               if Ufrom = Uto then
                  Error_Msg ("elaboration circularity detected");
               else
                  Error_Msg_Name_1 := Unit.Table (Ufrom).Uname;
                  Error_Msg ("Elaborate_Body for unit & cannot be satisfied");
               end if;

               Choose (U);
               return True;

            --  All done if already visited, otherwise mark as visited

            elsif UNR.Table (U).Visited then
               return False;

            --  Otherwise mark as visited and look at all successors

            else
               UNR.Table (U).Visited := True;

               S := UNR.Table (U).Successors;
               while S /= No_Successor loop
                  if Find_Link (Succ.Table (S).After, PL + 1) then
                     Elab_Error_Msg (S);
                     Choose (U);
                     return True;
                  end if;

                  S := Succ.Table (S).Next;
               end loop;

               --  Falling through means this does not lead to a path

               return False;
            end if;
         end Find_Link;

      --  Start of processing for Find_Path

      begin
         --  Initialize all non-chosen nodes to not visisted yet

         for U in Unit.First .. Unit.Last loop
            UNR.Table (U).Visited := UNR.Table (U).Elab_Position /= 0;
         end loop;

         --  Now try to find the path

         return Find_Link (Ufrom, 0);
      end Find_Path;

   --  Start of processing for Diagnose_Elaboration_Error

   begin
      --  First try for paths of length > 1 from units which are in No_Pred
      --  but have not been chosen. These are specs with elaborate_body
      --  pragmas whose corresponding bodies cannot be chosen yet.

      U := No_Pred;
      while U /= No_Unit_Id loop
         if Find_Path (U, Corresponding_Body (U), 2) then
            raise Unrecoverable_Error;
         end if;

         U := UNR.Table (U).Nextnp;
      end loop;

      --  If we failed in that attempt, try to find cycles starting with any
      --  of the remaining nodes that have not yet been chosen. There must
      --  be at least one (there is some reason we are being called!)

      for U in Unit.First .. Unit.Last loop
         if UNR.Table (U).Elab_Position = 0 then
            if Find_Path (U, U, 1) then
               raise Unrecoverable_Error;
            end if;
         end if;
      end loop;

      --  We should never get here, since we were called for some reason,
      --  and we should have found and eliminated at least one bad path.

      pragma Assert (False);

   end Diagnose_Elaboration_Problem;

   --------------------
   -- Elab_All_Links --
   --------------------

   procedure Elab_All_Links (Before, After : Unit_Id) is
   begin
      if UNR.Table (Before).Visited then
         return;
      end if;

      --  Build the direct link for Before

      UNR.Table (Before).Visited := True;
      Build_Link (Before, After, Elab_All);

      --  Process all units with'ed by Before recursively

      for W in
        Unit.Table (Before).First_With .. Unit.Table (Before).Last_With
      loop

         --  Skip if no ALI file for this with, happens with generics now,
         --  will happen only with specialized generics like unchecked
         --  stuff when we finally fix generics???

         if Withs.Table (W).Afile /= No_File then
            Elab_All_Links (Unit_Id_Of (Withs.Table (W).Uname), After);
         end if;
      end loop;

      --  Process corresponding body, if there is one

      if Unit.Table (Before).Utype = Is_Spec then
         Elab_All_Links (Corresponding_Body (Before), After);
      end if;
   end Elab_All_Links;

   --------------------
   -- Elab_Error_Msg --
   --------------------

   procedure Elab_Error_Msg (S : Successor_Id) is
   begin
      Error_Msg_Name_1 := Unit.Table (Succ.Table (S).Before).Uname;
      Error_Msg_Name_2 := Unit.Table (Succ.Table (S).After).Uname;
      Error_Msg_Info ("  & must be elaborated before &");

      Error_Msg_Name_1 := Unit.Table (Succ.Table (S).Reason_Unit).Uname;

      case Succ.Table (S).Reason is
         when Withed =>
            Error_Msg_Info
              ("     reason: with clause");

         when Elab =>
            Error_Msg_Info
              ("     reason: pragma Elaborate in unit &");

         when Elab_All =>
            Error_Msg_Info
              ("     reason: pragma Elaborate_All in unit &");

         when Spec_First =>
            Error_Msg_Info
              ("     reason: spec always elaborated before body");
      end case;
   end Elab_Error_Msg;

   ---------------------
   -- Find_Elab_Order --
   ---------------------

   procedure Find_Elab_Order is
      U           : Unit_Id;
      Best_So_Far : Unit_Id;

   begin
      Succ.Init;
      Num_Left := Int (Unit.Last - Unit.First + 1);

      --  Initialize unit table for elaboration control

      for U in Unit.First .. Unit.Last loop
         UNR.Increment_Last;
         UNR.Table (UNR.Last).Successors    := No_Successor;
         UNR.Table (UNR.Last).Num_Pred      := 0;
         UNR.Table (UNR.Last).Nextnp        := No_Unit_Id;
         UNR.Table (UNR.Last).Elab_Order    := 0;
         UNR.Table (UNR.Last).Elab_Position := 0;
      end loop;

      --  Gather dependencies and output them if option set

      Gather_Dependencies;

      --  Output elaboration dependencies if option is set

      if Elab_Dependency_Output then
         Write_Dependencies;
      end if;

      --  Initialize the no predecessor list

      No_Pred := No_Unit_Id;

      for U in UNR.First .. UNR.Last loop
         if UNR.Table (U).Num_Pred = 0 then
            UNR.Table (U).Nextnp := No_Pred;
            No_Pred := U;
         end if;
      end loop;

      --  OK, now we determine the elaboration order proper. All we do is to
      --  select the best choice from the no predecessor list until all the
      --  nodes have been chosen.

      Outer : loop
         --  If there are no nodes with predecessors, then either we are
         --  done, as indicated by Num_Left being set to zero, or we have
         --  a circularity. In the latter case, diagnose the circularity,
         --  removing it from the graph and continue

         Get_No_Pred : while No_Pred = No_Unit_Id loop
            exit Outer when Num_Left < 1;
            Diagnose_Elaboration_Problem;
         end loop Get_No_Pred;

         U := No_Pred;
         Best_So_Far := No_Unit_Id;

         --  Loop to choose best entry in No_Pred list

         No_Pred_Search : loop
            if Debug_Flag_N then
               Write_Str ("  considering choice of ");
               Write_Unit_Name (Unit.Table (U).Uname);
               Write_Eol;

               if Unit.Table (U).Elaborate_Body then
                  Write_Str
                    ("    Elaborate_Body = True, Num_Pred for body = ");
                  Write_Int
                    (Int (UNR.Table (Corresponding_Body (U)).Num_Pred));
               else
                  Write_Str
                    ("    Elaborate_Body = False");
               end if;

               Write_Eol;
            end if;

            --  We must ignore entries that have Pragma Elaborate_Body set
            --  unless the corresponding body has a Num_Pred of one (i.e.
            --  it is waiting only for its spec to be chosen, so that we
            --  can choose it immediate after choosing the spec).

            if not Unit.Table (U).Elaborate_Body
               or else UNR.Table (Corresponding_Body (U)).Num_Pred = 1
            then
               --  This is a candididate to be considered for choice

               if Best_So_Far = No_Unit_Id
                 or else ((not Horrible_Elab_Order)
                            and then Better_Choice (U, Best_So_Far))
                 or else (Horrible_Elab_Order
                            and then Worse_Choice (U, Best_So_Far))
               then
                  if Debug_Flag_N then
                     Write_Str ("    tentatively chosen (best so far)");
                     Write_Eol;
                  end if;

                  Best_So_Far := U;
               end if;
            end if;

            U := UNR.Table (U).Nextnp;
            exit No_Pred_Search when U = No_Unit_Id;
         end loop No_Pred_Search;

         --  If no candididate chosen, it means that all the items with
         --  No_Pred set to zero are instances of specs with Elaborate_Body
         --  set where the bodies cannot be elaborated yet. In this case,
         --  we diagnose the elaboration problem and continue the main loop.

         if Best_So_Far = No_Unit_Id then
            Diagnose_Elaboration_Problem;

         --  Otherwise choose the best candidate found

         else
            Choose (Best_So_Far);

            --  If we just chose a spec with Elaborate_Body set, then we
            --  must immediately elaborate the spec, before any other units.

            if Unit.Table (Best_So_Far).Elaborate_Body then
               pragma Assert
                 (UNR.Table (Corresponding_Body (Best_So_Far)).Num_Pred = 0);
               Choose (Corresponding_Body (Best_So_Far));
            end if;
         end if;
      end loop Outer;

   end Find_Elab_Order;

   -------------------------
   -- Gather_Dependencies --
   -------------------------

   procedure Gather_Dependencies is
      Withed_Unit : Unit_Id;

   begin
      --  Loop through all units

      for U in Unit.First .. Unit.Last loop
         Cur_Unit := U;

         --  If there is a body and a spec, then spec must be elaborated first
         --  Note that the corresponding spec immediately follows the body

         if Unit.Table (U).Utype = Is_Body then
            Build_Link (Corresponding_Spec (U), U, Spec_First);
         end if;

         --  Process WITH references for this unit ignoring generic units

         for W in Unit.Table (U).First_With .. Unit.Table (U).Last_With loop
            if Withs.Table (W).Sfile /= No_File then
               Withed_Unit :=
                 Unit_Id (Unit_Id_Of (Withs.Table (W).Uname));

               --  Pragma Elaborate case. We must build a link for the withed
               --  unit itself, and also the corresponding body if there is one

               --  However, skip this processing if there is no ALI file for
               --  the WITH entry, because this means it is a generic (even
               --  when we fix the generics so that an ALI file is present,
               --  we probably still will have no ALI file for unchecked
               --  and other special cases).

               if Withs.Table (W).Elaborate
                 and then Withs.Table (W).Afile /= No_File
               then
                  Build_Link (Withed_Unit, U, Withed);

                  if Unit.Table (Withed_Unit).Utype = Is_Spec then
                     Build_Link
                      (Corresponding_Body (Withed_Unit), U, Elab);
                  end if;

               --  Pragma Elaborate_All case, for this we use the recursive
               --  Elab_All_Links procedure to establish the links.

               elsif Withs.Table (W).Elaborate_All then

                  --  Reset flags used to stop multiple visits to a given node

                  for Uref in UNR.First .. UNR.Last loop
                     UNR.Table (Uref).Visited := False;
                  end loop;

                  --  Now establish all the links we need

                  Elab_All_Links (Withed_Unit, U);

               --  Case of normal WITH with no elaboration pragmas, just
               --  build the single link to the directly referenced unit

               else
                  Build_Link (Withed_Unit, U, Withed);
               end if;
            end if;
         end loop;
      end loop;
   end Gather_Dependencies;

   ----------------
   -- Unit_Id_Of --
   ----------------

   function Unit_Id_Of (Uname : Unit_Name_Type) return Unit_Id is
      Info : constant Int := Get_Name_Table_Info (Uname);

   begin
      pragma Assert (Info /= 0 and then Unit_Id (Info) /= No_Unit_Id);
      return Unit_Id (Info);
   end Unit_Id_Of;

   ------------------
   -- Worse_Choice --
   ------------------

   function Worse_Choice (U1, U2 : Unit_Id) return Boolean is

      function Body_Unit (U : Unit_Id) return Boolean;
      --  Determines if given unit is a body

      function Waiting_Body (U : Unit_Id) return Boolean;
      --  Determines if U is a waiting body, defined as a body which has
      --  not been elaborated, but whose spec has been elaborated.

      function Body_Unit (U : Unit_Id) return Boolean is
      begin
         return Unit.Table (U).Utype = Is_Body
           or else Unit.Table (U).Utype = Is_Body_Only;
      end Body_Unit;

      function Waiting_Body (U : Unit_Id) return Boolean is
      begin
         return Unit.Table (U).Utype = Is_Body and then
            UNR.Table (Corresponding_Spec (U)).Elab_Position /= 0;
      end Waiting_Body;

   --  Start of processing for Worse_Choice

   --  Note: the checks here are applied in sequence, and the ordering is
   --  significant (i.e. the more important criteria are applied first).

   begin
      --  If either unit is predefined, then use Better_Choice, since the
      --  language requires that predefined units not mess up in the choice
      --  of elaboration order.

      if Unit.Table (U1).Predefined or else Unit.Table (U2).Predefined then
         return Better_Choice (U1, U2);

      --  Prefer anything else to a waiting body (!)

      elsif Waiting_Body (U1) and not Waiting_Body (U2) then
         return False;

      elsif Waiting_Body (U2) and not Waiting_Body (U1) then
         return True;

      --  Prefer a spec to a body (!)

      elsif Body_Unit (U1) and not Body_Unit (U2) then
         return False;

      elsif Body_Unit (U2) and not Body_Unit (U1) then
         return True;

      --  If both are waiting bodies, then prefer the one whose spec is
      --  less recently elaborated. Consider the following:

      --     spec of A
      --     spec of B
      --     body of A or B?

      --  The normal waiting body preference would have placed the body of
      --  A before the spec of B if it could. Since it could not, there it
      --  must be the case that A depends on B. It is therefore a good idea
      --  to put the body of B last so that if there is an elaboration order
      --  problem, we will find it (that's what horrible order is about)

      elsif Waiting_Body (U1) and then Waiting_Body (U2) then
         return
           UNR.Table (Corresponding_Spec (U1)).Elab_Position <
           UNR.Table (Corresponding_Spec (U2)).Elab_Position;

      --  Otherwise decide on the basis of alphabetical order. We do not try
      --  to reverse the usual choice here, since it can cause cancelling
      --  errors with the other inversions.

      else
         return Uname_Less (Unit.Table (U1).Uname, Unit.Table (U2).Uname);
      end if;
   end Worse_Choice;

   ------------------------
   -- Write_Dependencies --
   ------------------------

   procedure Write_Dependencies is
      Col : Positive;

   begin
      Write_Eol;
      Write_Str
        ("                 ELABORATION ORDER DEPENDENCIES");
      Write_Eol;
      Write_Eol;
      Write_Str
        ("Elaborate                   Before                      Because");
      Write_Eol;
      Write_Str
        ("---------                   ------                      -------");
      Write_Eol;

      for S in Succ_First .. Succ.Last loop
         Write_Unit_Name (Unit.Table (Succ.Table (S).Before).Uname);
         Col := Name_Len + 1;

         if Col >= 29 then
            Write_Eol;
            Col := 1;
         end if;

         while Col < 29 loop
            Write_Char (' ');
            Col := Col + 1;
         end loop;

         Write_Unit_Name (Unit.Table (Succ.Table (S).After).Uname);
         Col := Col + Name_Len;

         if Col >= 57 then
            Write_Eol;
            Col := 1;
         end if;

         while Col < 57 loop
            Write_Char (' ');
            Col := Col + 1;
         end loop;

         case Succ.Table (S).Reason is
            when Withed      => Write_Str ("with");
            when Elab        => Write_Str ("Elaborate");
            when Elab_All    => Write_Str ("Elaborate_All");
            when Spec_First  => Write_Str ("spec first");
         end case;

         Write_Eol;
      end loop;

      Write_Eol;
   end Write_Dependencies;

end Binde;
