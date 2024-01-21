------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       A D A . I N T E R R U P T S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.10 $                              --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with System;
with Interfaces.C.System_Constants;

package Ada.Interrupts is

   type Interrupt_ID is new integer
     range 0 .. Interfaces.C.System_Constants.NSIG;

   type Parameterless_Handler is access protected procedure;

   function Is_Reserved (Interrupt : Interrupt_ID)
      return Boolean;

   function Is_Attached (Interrupt : Interrupt_ID)
      return Boolean;

   function Current_Handler (Interrupt : Interrupt_ID)
      return Parameterless_Handler;

   procedure Attach_Handler
      (New_Handler : Parameterless_Handler;
       Interrupt   : Interrupt_ID);

   procedure Exchange_Handler
      (Old_Handler : out Parameterless_Handler;
       New_Handler : Parameterless_Handler;
       Interrupt   : Interrupt_ID);

   procedure Detach_Handler
      (Interrupt : Interrupt_ID);

   function Reference (Interrupt : Interrupt_ID)
      return System.Address;

end Ada.Interrupts;
