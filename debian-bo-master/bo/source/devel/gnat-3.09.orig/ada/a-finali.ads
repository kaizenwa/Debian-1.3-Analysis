------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     A D A . F I N A L I Z A T I O N                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with System.Finalization_Root;

package Ada.Finalization is
pragma Preelaborate (Finalization);

   type Controlled is abstract tagged private;

   procedure Initialize (Object : in out Controlled);
   procedure Adjust     (Object : in out Controlled);
   procedure Finalize   (Object : in out Controlled);

   type Limited_Controlled is abstract tagged limited private;

   procedure Initialize (Object : in out Limited_Controlled);
   procedure Finalize   (Object : in out Limited_Controlled);

private
   package SFR renames System.Finalization_Root;

   type Controlled is abstract new SFR.Root_Controlled with null record;

   function "=" (A, B : Controlled) return Boolean;
   --  Need to be defined explictly because we don't want to compare the
   --  hidden pointers

   type Limited_Controlled is
     abstract new SFR.Root_Controlled with null record;

end Ada.Finalization;
