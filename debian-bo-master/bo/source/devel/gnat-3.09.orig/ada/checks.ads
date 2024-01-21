------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C H E C K S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.44 $                             --
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

--  Package containing routines used to deal with runtime checks. These
--  routines are used both by the semantics and by the expander. In some
--  cases, checks are enabled simply by setting flags for gigi, and in
--  other cases the code for the check is expanded.

--  The approach used for range and length checks, in regards to suppressed
--  checks, is to attempt to detect at compilation time that a constraint
--  error will occur. If this is detected a warning or error is issued and the
--  offending expression or statement replaced with a constraint error node.
--  This always occurs whether checks are suppressed or not.  Dynamic range
--  checks are, of course, not inserted if checks are suppressed.

with Types; use Types;

package Checks is

   function Access_Checks_Suppressed        (E : Entity_Id) return Boolean;
   function Accessibility_Checks_Suppressed (E : Entity_Id) return Boolean;
   function Discriminant_Checks_Suppressed  (E : Entity_Id) return Boolean;
   function Division_Checks_Suppressed      (E : Entity_Id) return Boolean;
   function Elaboration_Checks_Suppressed   (E : Entity_Id) return Boolean;
   function Index_Checks_Suppressed         (E : Entity_Id) return Boolean;
   function Length_Checks_Suppressed        (E : Entity_Id) return Boolean;
   function Overflow_Checks_Suppressed      (E : Entity_Id) return Boolean;
   function Range_Checks_Suppressed         (E : Entity_Id) return Boolean;
   function Storage_Checks_Suppressed       (E : Entity_Id) return Boolean;
   function Tag_Checks_Suppressed           (E : Entity_Id) return Boolean;
   --  These functions check to see if the named check is suppressed,
   --  either by an active scope suppress setting, or because the check
   --  has been specifically suppressed for the given entity. If no entity
   --  is relevant for the current check, then Empty is used as an argument.
   --  Note: the reason we insist on specifying Empty is to force the
   --  caller to think about whether there is any relevant entity that
   --  should be checked.

   procedure Apply_Access_Check (N : Node_Id; Typ : Entity_Id);
   --  Determines whether an expression node should be flagged as needing
   --  a runtime access check. If the node requires such a check, the
   --  Do_Access_Check flag is turned on.

   procedure Apply_Accessibility_Check (N : Node_Id; Typ : Entity_Id);
   --  Given a name N denoting an access parameter, emits a run-time
   --  accessibility check (if necessary), checking that the level of
   --  the object denoted by the access parameter is not deeper than the
   --  level of the type Typ. Program_Error is raised if the check fails.

   procedure Apply_Array_Size_Check (N : Node_Id; Typ : Entity_Id);
   --  N is the node for an object declaration that declares an object of
   --  array type Typ. This routine generates, if necessary, a check that
   --  the size of the array is not too large, raising Storage_Error if so.

   procedure Apply_Arithmetic_Overflow_Check (N : Node_Id);
   --  Given a binary arithmetic operator (+ - *) expand a software integer
   --  overflow check using range checks on a larger checking type or a call
   --  to an appropriate runtime routine. This is used for all three operators
   --  for the signed integer case, and for +/- in the fixed-point case. The
   --  check is expanded only if Software_Overflow_Checking is enabled and
   --  Do_Overflow_Check is set on node N. Note that divide is handled
   --  separately using Apply_Arithmetic_Divide_Overflow_Check.

   procedure Apply_Arithmetic_Divide_Overflow_Check (N : Node_Id);
   --  Given a binary arithmetic division operator, expand a software integer
   --  overflow check, checking for the one bad case with is division of the
   --  largest negative number by minus one. The check is expanded only if
   --  Software_Overflow_Checking is enabled and Do_Overflow_Check is set
   --  on node N. Note that division by zero is handled separately using
   --  the Apply_Zero_Divide_Check procedure.

   procedure Apply_Constraint_Check
     (N          : Node_Id;
      Typ        : Entity_Id;
      No_Sliding : Boolean := False);
   --  Top-level procedure, calls all the others depending on the class of Typ.
   --  Checks that expression N verifies the constraint of type Typ. No_Sliding
   --  is only relevant for constrained array types, id set to true, it
   --  checks that indexes are in range.

   procedure Apply_Discriminant_Check
     (N   : Node_Id;
      Typ : Entity_Id;
      Lhs : Node_Id := Empty);
   --  Given an expression N of a discriminated type, or of an access type
   --  whose designated type is a discriminanted type, generates a check to
   --  ensure that the expression can be converted to the subtype given as
   --  the second parameter. Lhs is empty except in the case of assignments,
   --  where the target object may be needed to determine the subtype to
   --  check against (such as the cases of unconstrained formal parameters
   --  and unconstrained aliased objects). For the case of unconstrained
   --  formals, the check is peformed only if the corresponding actual is
   --  constrained, i.e., whether Lhs'Constrained is True.

   function Build_Discriminant_Checks
     (N     : Node_Id;
      T_Typ : Entity_Id)
      return  Node_Id;
   --  Subsidiary routine for Apply_Discriminant_Check. Builds the expression
   --  that compares discriminants of the expression with discriminants of the
   --  type. Also used directly for membership tests (see Exp_Ch4.Expand_N_In).

   procedure Apply_Subscript_Conversion_Checks (N : Node_Id);
   --  Add necessary type conversions to the subscripts of formal parameters
   --  that are unconstrained arrays or access to them and also for subscripts
   --  of packed arrays to enforce index checks. The node N is an indexed
   --  component node whose prefix is not an access type (i.e. the caller
   --  is responsible for rewriting an access type prefix as an explicit
   --  dereference before the call).

   procedure Apply_Zero_Divide_Check (N : Node_Id);
   --  The node kind is N_Op_Divide, N_Op_Mod, or N_Op_Rem. An appropriate
   --  check is generated to ensure that the right operand is non-zero. Note
   --  that in the divide case, but not in the other two cases, overflow can
   --  still occur with a non-zero divisor when the largest negative number
   --  is divided by minus one, but we do not check for that case here. The
   --  check is generated only if Do_Overflow_Check is set on the node N.

   procedure Apply_Type_Conversion_Checks (N : Node_Id);
   --  N is an N_Type_Conversion node. A type conversion actually involves
   --  two sorts of checks. The first check is the checks that ensures that
   --  the operand in the type conversion fits onto the base type of the
   --  subtype it is being converted to (see RM 4.6 (28)-(50)). The second
   --  check is there to ensure that once the operand has been converted to
   --  a value of the target type, this converted value meets the
   --  constraints imposed by the target subtype (see RM 4.6 (51)).

   procedure Apply_Universal_Integer_Attribute_Checks (N : Node_Id);
   --  The argument N is an attribute reference node intended for processing
   --  by gigi. The attribute is one that returns a universal integer, but
   --  the attribute reference node is currently typed with the expected
   --  result type. This routine deals with range and overflow checks needed
   --  to make sure that the universal result is in range.

   -----------------------------
   -- Length and Range Checks --
   -----------------------------

   --  In the following procedures, three arguments are given, the expression,
   --  to be checked against the target type, and the source type. The source
   --  type is normally taken from Etype (Expr), unless an explicit Source_Typ
   --  parameter is given, in which case it supplies the source type. The
   --  target type is always given explicitly.

   --  The only case in which the caller supplies an explicit Source_Typ is
   --  for the case of Out and In_Out parameters, where, for the conversion
   --  on return (the Out direction), the types must be reversed. This is
   --  handled by the caller, and is the only reason we have the option to
   --  pass in Source_Typ explicitly.

   procedure Apply_Length_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty);
   --  This procedure builds a sequence of declarations to do a length check
   --  that checks if the lengths of the two arrays Target_Typ and source type
   --  are the same. The resulting actions are inserted at Node using a call
   --  to Insert_Actions.
   --
   --  For access types, the Directly_Designated_Type is retrieved and
   --  processing continues as enumerated above, with a guard against
   --  null values.
   --
   --  Note: calls to Apply_Length_Check currently never supply an explicit
   --  Source_Typ parameter, but Apply_Length_Check takes this parameter and
   --  processes it as described above for consistency with the other routines
   --  in this section.

   procedure Apply_Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty);
   --  For an Node of kind N_Range, constructs a range check action that
   --  tests first that the range is not null and then that the range
   --  is contained in the Target_Typ range.
   --
   --  For scalar types, constructs a range check action that first tests that
   --  the expression is contained in the Target_Typ range. The difference
   --  between this and Apply_Scalar_Range_Check is that the latter generates
   --  the actual checking code in gigi against the Etype of the expression.
   --
   --  For constrained array types, construct series of range check actions
   --  to check that each Expr range is properly contained in the range of
   --  Target_Typ.
   --
   --  For a type conversion to an unconstrained array type, constructs
   --  a range check action to check that the bounds of the source type
   --  are within the constraints imposed by the Target_Typ.
   --
   --  For access types, the Directly_Designated_Type is retrieved and
   --  processing continues as enumerated above, with a guard against
   --  null values.
   --
   --  The source type is used by type conversions to unconstrained array
   --  types to retrive the corresponding bounds.

   procedure Apply_Static_Length_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty);
   --  Tries to determine statically whether the two array types source type
   --  and Target_Typ have the same length. If it can be determined at compile
   --  time that they do not, then an N_Raise_Constraint_Error node replaces
   --  Expr, and a warning message is issued.

   procedure Apply_Scalar_Range_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Fixed_Int  : Boolean   := False);
   --  For scalar types, determines whether an expression node should be
   --  flagged as needing a runtime range check. If the node requires such
   --  a check, the Do_Range_Check flag is turned on. The Fixed_Int flag
   --  if set causes any fixed-point values to be treated as though they
   --  were discrete values (i.e. the underlying integer value is used).

   type Check_Result is private;
   --  Type used to return result of Range_Check call, for later use in
   --  call to Insert_Range_Checks procedure.

   procedure Append_Range_Checks
     (Checks       : Check_Result;
      Stmts        : List_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr;
      Flag_Node    : Node_Id);
   --  Called to append range checks as returned by a call to Range_Check.
   --  Stmts is a list to which either the dynamic check is appended or
   --  the raise Constraint_Error statement is appended (for static checks).
   --  Static_Sloc is the Sloc at which the raise CE node points,
   --  Flag_Node is used as the node at which to set the Has_Dynamic_Check
   --  flag. Checks_On is a boolean value that says if range and index checking
   --  is on or not.

   procedure Insert_Range_Checks
     (Checks       : Check_Result;
      Node         : Node_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr := No_Location;
      Flag_Node    : Node_Id    := Empty;
      Do_Before    : Boolean    := False);
   --  Called to insert range checks as returned by a call to Range_Check.
   --  Node is the node after which either the dynamic check is inserted or
   --  the raise Constraint_Error statement is inserted (for static checks).
   --  Suppress_Typ is the type to check to determine if checks are suppressed.
   --  Static_Sloc, if passed, is the Sloc at which the raise CE node points,
   --  otherwise Sloc (Node) is used. The Has_Dynamic_Check flag is normally
   --  set at Node. If Flag_Node is present, then this is used instead as the
   --  node at which to set the Has_Dynamic_Check flag. Normally the check is
   --  inserted after, if Do_Before is True, the check is inserted before
   --  Node.

   function Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Warn_Node  : Node_Id   := Empty)
      return       Check_Result;
   --  Like Apply_Range_Check, except it does not modify anything. Instead
   --  it returns an encapsulated result of the check operations for later
   --  use in a call to Insert_Range_Checks. If Warn_Node is non-empty, its
   --  Sloc is used, in the static case, for the generated warning or error.
   --  Additionally, it is used rather than Expr (or Low/High_Bound of Expr)
   --  in constructing the check.

private

   type Check_Result is array (Positive range 1 .. 2) of Node_Id;
   --  There are two cases for the result returned by Range_Check:
   --
   --    For the static case the result is one or two nodes that should cause
   --    a Constraint_Error. Typically these will include Expr itself or the
   --    direct descendents of Expr, such as Low/High_Bound (Expr)). It is the
   --    responsibility of the caller to rewrite and substitute the nodes with
   --    N_Raise_Constraint_Error nodes.
   --
   --    For the non-static case a single N_Raise_Constraint_Error node
   --    with a non-empty Condition field is returned.
   --
   --  Unused entries in Check_Result, if any, are simply set to Empty
   --  For external clients, the required processing on this result is
   --  achieved using the Insert_Range_Checks routine.

   pragma Inline (Access_Checks_Suppressed);
   pragma Inline (Accessibility_Checks_Suppressed);
   pragma Inline (Discriminant_Checks_Suppressed);
   pragma Inline (Division_Checks_Suppressed);
   pragma Inline (Elaboration_Checks_Suppressed);
   pragma Inline (Index_Checks_Suppressed);
   pragma Inline (Length_Checks_Suppressed);
   pragma Inline (Overflow_Checks_Suppressed);
   pragma Inline (Range_Checks_Suppressed);
   pragma Inline (Storage_Checks_Suppressed);
   pragma Inline (Tag_Checks_Suppressed);

   pragma Inline (Apply_Length_Check);
   pragma Inline (Apply_Range_Check);
   pragma Inline (Apply_Static_Length_Check);
end Checks;
