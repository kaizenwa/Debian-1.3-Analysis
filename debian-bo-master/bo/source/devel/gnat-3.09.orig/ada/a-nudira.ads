------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--         A D A . N U M E R I C S . D I S C R E T E _ R A N D O M          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;

generic
   type Result_Subtype is (<>);

package Ada.Numerics.Discrete_Random is

   --  Basic facilities.

   type Generator is limited private;

   function Random (Gen : Generator) return Result_Subtype;

   procedure Reset (Gen : Generator);
   procedure Reset (Gen : Generator; Initiator : Integer);

   --  Advanced facilities.

   type State is private;

   procedure Save  (Gen : Generator; To_State   : out State);
   procedure Reset (Gen : Generator; From_State : State);

   Max_Image_Width : constant := 80;

   function Image (Of_State    : State)  return String;
   function Value (Coded_State : String) return State;

private
   subtype Int is Interfaces.Integer_32;
   subtype Rst is Result_Subtype;

   type Flt is digits 14;

   RstF : constant Flt := Flt (Rst'Pos (Rst'First));
   RstL : constant Flt := Flt (Rst'Pos (Rst'Last));

   Offs : constant Flt := RstF + 0.5;

   K1   : constant := 94_833_359;
   K1F  : constant := 94_833_359.0;
   K2   : constant := 47_416_679;
   K2F  : constant := 47_416_679.0;
   Scal : constant Flt := (RstL - RstF + 1.0) / (K1F * K2F);

   type State is record
      X1  : Int := Int (2999 ** 2);
      X2  : Int := Int (1439 ** 2);
      P   : Int := K1;
      Q   : Int := K2;
      FP  : Flt := K1F;
      Scl : Flt := Scal;
   end record;

   type Generator is limited record
      Gen_State : State;
   end record;

end Ada.Numerics.Discrete_Random;
