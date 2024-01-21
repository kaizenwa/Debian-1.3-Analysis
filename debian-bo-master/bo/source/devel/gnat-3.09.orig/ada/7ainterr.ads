------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       A D A . I N T E R R U P T S                        --
--                                                                          --
--                                 S p e c                                  --
--                         (Version for new GNARL)                          --
--                                                                          --
--                            $Revision: 1.12 $                             --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with System.Interrupts;
--  used for Ada_Interrupt_ID

package Ada.Interrupts is

   --  Made definition of Interrupt_ID depend on System.OS_Interface.
   --  rather than being a visible target-specific type.
   --  This made the file a-interr.ads target-independent.
   --  The target-specific declaration of Interrupt_ID is *not* in
   --  System.Interrupts, because that contains operations on Interrupt_IDs
   --  that we do not want to inherit and make visible here.
   --  It is in System.OS_Interface because operations on this type
   --  will need visibility of other target-specific info. that is only
   --  visible there.
   --  Made non-renamed subprograms in line, for efficiency.

   type Interrupt_ID is new System.Interrupts.Ada_Interrupt_ID;

   type Parameterless_Handler is access protected procedure;

   function Is_Reserved (Interrupt : Interrupt_ID)
      return Boolean;
   pragma Inline (Is_Reserved);

   function Is_Attached (Interrupt : Interrupt_ID)
      return Boolean;
   pragma Inline (Is_Attached);

   function Current_Handler (Interrupt : Interrupt_ID)
      return Parameterless_Handler;
   pragma Inline (Current_Handler);

   procedure Attach_Handler
      (New_Handler : Parameterless_Handler;
       Interrupt   : Interrupt_ID);
   pragma Inline (Attach_Handler);

   procedure Exchange_Handler
      (Old_Handler : out Parameterless_Handler;
       New_Handler : Parameterless_Handler;
       Interrupt   : Interrupt_ID);
   pragma Inline (Exchange_Handler);

   procedure Detach_Handler
      (Interrupt : Interrupt_ID);
   pragma Inline (Detach_Handler);

   function Reference (Interrupt : Interrupt_ID)
      return System.Address;

end Ada.Interrupts;
