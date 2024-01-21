------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--         A D A . S Y N C H R O N O U S _ T A S K _ C O N T R O L          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------


with System;

package Ada.Synchronous_Task_Control is

   type Suspension_Object is limited private;

   procedure Set_True (S : in out Suspension_Object);

   procedure Set_False (S : in out Suspension_Object);

   function Current_State (S : Suspension_Object) return Boolean;

   procedure Suspend_Until_True (S : in out Suspension_Object);

private

   --  ??? Suspension_Object is implemented as a record with a single
   --      protected object field.  This is to program around a bug
   --      in GNAT, which does not like limited private types
   --      to be completed with a protected object.  Such a definition
   --      will compile, but its use kills the compiler.

   --  ??? Using a protected object is overkill; suspension could be
   --      implemented more efficiently.

   protected type Suspension_PO is
      entry Wait;
      procedure Set_False;
      procedure Set_True;
      function Get_Open return Boolean;
      entry Wait_Exception;

   private
      Open : Boolean := False;
   end Suspension_PO;

   type Suspension_Object is record
      Suspend : Suspension_PO;
   end record;

end Ada.Synchronous_Task_Control;
