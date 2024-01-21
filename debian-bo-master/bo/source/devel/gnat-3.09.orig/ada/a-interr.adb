------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                         A D A . I N T E R R U P T S                      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.6 $                             --
--                                                                          --
--      Copyright (C) 1991,1992,1993,1994,1995 Florida State University     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

with System.Interrupts;
package body Ada.Interrupts is

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Interrupt : Interrupt_ID) return Boolean renames
     System.Interrupts.Is_Reserved;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached (Interrupt : Interrupt_ID) return Boolean renames
     System.Interrupts.Is_Attached;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (Interrupt : Interrupt_ID)
     return Parameterless_Handler renames System.Interrupts.Current_Handler;

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler
     (New_Handler : in Parameterless_Handler;
      Interrupt   : in Interrupt_ID) is
   begin
      System.Interrupts.Attach_Handler (New_Handler, Interrupt);
   end Attach_Handler;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : in Parameterless_Handler;
      Interrupt   : in Interrupt_ID) is
   begin
      System.Interrupts.Exchange_Handler (Old_Handler, New_Handler, Interrupt);
   end Exchange_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   procedure Detach_Handler (Interrupt : in Interrupt_ID) is
   begin
      System.Interrupts.Detach_Handler (Interrupt);
   end Detach_Handler;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Interrupt_ID) return System.Address
     renames System.Interrupts.Reference;

end Ada.Interrupts;
