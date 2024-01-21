------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--            A D A . N U M E R I C S . F L O A T _ R A N D O M             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.10 $                             --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces;

package Ada.Numerics.Float_Random is

   --  Basic facilities

   type Generator is limited private;

   subtype Uniformly_Distributed is Float range 0.0 .. 1.0;

   function Random (Gen : Generator) return Uniformly_Distributed;

   procedure Reset (Gen : Generator);
   procedure Reset (Gen : Generator; Initiator : Integer);

   --  Advanced facilities

   type State is private;

   procedure Save  (Gen : Generator; To_State   : out State);
   procedure Reset (Gen : Generator; From_State : State);

   Max_Image_Width : constant := 80;

   function Image (Of_State :    State)  return String;
   function Value (Coded_State : String) return State;

private
   type Int is new Interfaces.Integer_32;
   type Flt is digits 14;

   K1   : constant := 94_833_359;
   K1F  : constant := 94_833_359.0;
   K2   : constant := 47_416_679;
   K2F  : constant := 47_416_679.0;
   Scal : constant := 1.0 / (K1F * K2F);

   type State is record
      X1  : Int := 2999 ** 2;     --  Square mod p
      X2  : Int := 1439 ** 2;      --  Square mod q
      P   : Int := K1;             --  see Blum, Blum & Shub
      Q   : Int := K2;             --  see Blum, Blum & Shub
      X   : Int := 1;              --  see Blum, Blum & Shub
      Scl : Flt := Scal;
   end record;

   type Generator is limited record
      Gen_State : State;
   end record;

end Ada.Numerics.Float_Random;
