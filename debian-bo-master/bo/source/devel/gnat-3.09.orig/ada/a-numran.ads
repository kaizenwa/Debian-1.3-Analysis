------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                  A D A . N U M E R I C S . R A N D O M                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
--                                                                          --
--        Copyright (C) 1992,1993,1994 Free Software Foundation, Inc.       --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides the basic support used for the Float_Random and
--  Discrete_Random children of Ada.Numerics. In this package, the generator
--  state is exposed for access but this is a private package so no problem.

private package Ada.Numerics.Random is

   --  All the arithmetic is done using a base type that is at least
   --  32 bits long (one's complement or two's complement, does not
   --  matter). It also must be at least as large as integer.

   type Int_Range is range
      Long_Integer'Min (Long_Integer (Integer'First), (-2 ** 31) + 1) ..
      Long_Integer'Max (Long_Integer (Integer'Last),  (+2 ** 31) - 1);

   type Int is new Int_Range'Base;
   subtype Nat is Int range 0 .. Int'Last;
   --  The types that we will actually use

   --  The following declarations define the type State, which is used as the
   --  underlying type for both Generator and State in the user level packages

   Larger_Lag  : constant := 25;
   Smaller_Lag : constant := 11;
   --  Lag values used for modular accessing of the state vector

   type Lag_Range is mod Larger_Lag;
   --  Range of larger lag is length of state vector

   type State_Vector is array (Lag_Range) of Float;

   type State is record
      Lagged_Outputs : State_Vector;
      Borrow         : Float;
      R, S           : Lag_Range;
   end record;

   subtype Uniformly_Distributed is Float range 0.0 .. 1.0;
   --  Range of random numbers

   procedure Random (S : in out State; U : out Uniformly_Distributed);
   --  Obtain next random number, updating State

   function Make_State (Starter : Int := 3E+7) return State;
   --  Build a state from the given integer value. The default value for
   --  Int is used to build the default starting generator configurations.

   procedure Reset (S : out State; Initiator : in Integer);
   --  Set state from given integer value

   procedure Reset (S : out State);
   --  Set state from current time

   Max_Image_Width : constant := (24 + 1) * 25 + 24;
   --  The 24 + 1 is for one floating point value plus a comma, and there
   --  are up to Larger_Lag number of these, followed by one additional value

   function Image (S : State)  return String;
   --  Convert state to canonical string format (see body for format)

   function Value (S : String) return State;
   --  Convert string returned by previous image call back to State

end Ada.Numerics.Random;
