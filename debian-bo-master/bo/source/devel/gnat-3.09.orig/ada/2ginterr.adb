------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                     S Y S T E M . I N T E R R U P T S                    --
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

--  This is the Irix version of this package.

with Ada.Task_Identification;
with Ada.Interrupts;
with Ada.Interrupts.Names;

with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Pthreads;

with System.Storage_Elements;
with System.Task_Primitives;

with Unchecked_Conversion;

with System.Tasking;
with System.Tasking.Utilities;
with System.Tasking.Rendezvous;

package body System.Interrupts is

   use Tasking;
   use Ada.Interrupts;

   package C        renames Interfaces.C;
   package Pthreads renames Interfaces.C.Pthreads;
   package INTR     renames Ada.Interrupts;
   package INTNAM   renames Ada.Interrupts.Names;

   function To_System is new Unchecked_Conversion
     (Ada.Task_Identification.Task_ID, Task_ID);

   use type Ada.Interrupts.Interrupt_ID;
   use type Ada.Interrupts.Parameterless_Handler;
   use type C.Int;

   type Handler_Kind is (Unknown, Task_Entry, Protected_Procedure);

   type Handler_Desc is -- (Kind : Handler_Kind := Unknown) is
      record
         Kind   : Handler_Kind := Unknown;
         T      : Task_ID;
         E      : Task_Entry_Index;
         H      : Parameterless_Handler;
         Static : Boolean;
      end record;

--         case Kind is
--            when Unknown =>
--               null;
--            when Task_Entry =>
--               T : Task_ID;
--               E : Task_Entry_Index;
--            when Protected_Procedure =>
--               H      : Parameterless_Handler;
--               Static : Boolean;
--         end case;
--      end record;

   task type Server_Task (Interrupt : Interrupt_ID) is
      pragma Interrupt_Priority (System.Interrupt_Priority'Last);
   end Server_Task;

   type Server_Task_Access is access Server_Task;

   Usable_Interrupts   : array (Ada.Interrupts.Interrupt_ID) of Boolean;
   Attached_Interrupts : array (Ada.Interrupts.Interrupt_ID) of Boolean;
   Handlers            : array (Ada.Interrupts.Interrupt_ID) of Task_ID;
   Descriptors         : array (Ada.Interrupts.Interrupt_ID) of Handler_Desc;
   Interrupt_Count     : array (Ada.Interrupts.Interrupt_Id) of Integer
     := (others => 0);

   pragma Volatile_Components (Interrupt_Count);

   procedure Signal_Handler (Sig : Ada.Interrupts.Interrupt_ID);
   --  Type and Head, Tail of the list containing Registered Interrupt
   --  Handlers. These definitions are used to register the handlers
   --  specified by the pragma Interrupt_Handler.

   --
   --  Handler Registration:
   --

   type Registered_Handler;
   type R_Link is access all Registered_Handler;

   type Registered_Handler is record
      H :    System.Address := System.Null_Address;
      Next : R_Link := null;
   end record;

   Registered_Handlers : R_Link := null;

   function Is_Registered (Handler : Parameterless_Handler) return boolean;
   procedure Unimplemented (Str : String);

   procedure Signal_Handler (Sig : Ada.Interrupts.Interrupt_ID) is
      Handler : Task_ID renames Handlers (Sig);
   begin
      if Handler /= null then
         Interrupt_Count (Sig) := Interrupt_Count (Sig) + 1;
         System.Task_Primitives.Cond_Signal (Handler.Cond);
      end if;
   end Signal_Handler;

   type Handler_Ptr is access procedure (Sig : Ada.Interrupts.Interrupt_ID);

   function TISR is new Unchecked_Conversion
     (Handler_Ptr, Pthreads.Isr_Address);

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Boolean is
   begin
      return not Usable_Interrupts (Interrupt);
   end Is_Reserved;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Boolean is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      return Attached_Interrupts (Interrupt);
   end Is_Attached;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler (Interrupt : Ada.Interrupts.Interrupt_ID)
     return Ada.Interrupts.Parameterless_Handler is
   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      if Descriptors (Interrupt).Kind = Protected_Procedure then
         return Descriptors (Interrupt).H;
      else
         return null;
      end if;

   end Current_Handler;

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler
     (New_Handler : Ada.Interrupts.Parameterless_Handler;
      Interrupt   : Ada.Interrupts.Interrupt_ID;
      Static      : boolean := false) is

      New_Task : Server_Task_Access;

   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      if Handlers (Interrupt) = null then
         New_Task := new Server_Task (Interrupt);
         Handlers (Interrupt) := To_System (New_Task.all'Identity);
         if (Pthreads.Intr_Attach (C.Int (Interrupt),
                                   TISR (Signal_Handler'Access))
             = Pthreads.FUNC_ERR) then
            raise Program_Error;
         end if;
      end if;

--    Descriptors (Interrupt) :=
--      (Kind => Protected_Procedure,
--       H => New_Handler,
--       Static => Static);

      Descriptors (Interrupt).Kind := Protected_Procedure;
      Descriptors (Interrupt).H := New_Handler;
      Descriptors (Interrupt).Static := Static;

      Attached_Interrupts (Interrupt) := True;

   end Attach_Handler;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   procedure Exchange_Handler
     (Old_Handler : out Ada.Interrupts.Parameterless_Handler;
      New_Handler : Ada.Interrupts.Parameterless_Handler;
      Interrupt   : Ada.Interrupts.Interrupt_ID;
      Static      : boolean := false) is

   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      Old_Handler := Current_Handler (Interrupt);
      Attach_Handler (New_Handler, Interrupt, Static);

   end Exchange_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   procedure Detach_Handler
     (Interrupt : Ada.Interrupts.Interrupt_ID;
      Static    : boolean := false) is
   begin

      if Descriptors (Interrupt).Kind = Task_Entry then
         raise Program_Error;
      end if;

      if not Static and then Descriptors (Interrupt).Static then
         raise Program_Error;
      end if;

      Attached_Interrupts (Interrupt) := False;
--    Descriptors (Interrupt) := (Kind => Unknown);
      Descriptors (Interrupt).Kind := Unknown;

      if (Pthreads.Intr_Attach (C.Int (Interrupt), null)
          = Pthreads.FUNC_ERR) then
         raise Program_Error;
      end if;

   end Detach_Handler;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Ada.Interrupts.Interrupt_ID)
     return System.Address is

      Signal : System.Address :=
        System.Storage_Elements.To_Address
          (System.Storage_Elements.Integer_Address (Interrupt));

   begin
      if Is_Reserved (Interrupt) then
      --  Only usable Interrupts can be used for binding it to an Entry.
         raise Program_Error;
      end if;
      return Signal;
   end Reference;

   --------------------------------
   -- Register_Interrupt_Handler --
   --------------------------------

   procedure Register_Interrupt_Handler
     (Handler_Addr : System.Address) is
   begin
      Registered_Handlers :=
       new Registered_Handler'(H => Handler_Addr, Next => Registered_Handlers);
   end Register_Interrupt_Handler;

   -------------------
   -- Is_Registered --
   -------------------

   function Is_Registered (Handler : Parameterless_Handler) return boolean is
      Ptr : R_Link := Registered_Handlers;

      type Fat_Ptr is record
         Object_Addr  : System.Address;
         Handler_Addr : System.Address;
      end record;

      function To_Fat_Ptr is new Unchecked_Conversion
        (Parameterless_Handler, Fat_Ptr);

      Fat : Fat_Ptr;

   begin
      Fat := To_Fat_Ptr (Handler);

      while Ptr /= null loop

         if Ptr.H = Fat.Handler_Addr then
            return True;
         end if;

         Ptr := Ptr.Next;
      end loop;

      return False;
   end Is_Registered;


   -----------------------------
   -- Bind_Interrupt_To_Entry --
   -----------------------------

   procedure Bind_Interrupt_To_Entry
     (T       : System.Tasking.Task_ID;
      E       : System.Tasking.Task_Entry_Index;
      Int_Ref : System.Address) is

      Interrupt   : constant Interrupt_ID :=
        Interrupt_ID (Storage_Elements.To_Integer (Int_Ref));

      New_Task : Server_Task_Access;

   begin
      if Is_Reserved (Interrupt) then
         raise Program_Error;
      end if;

      if Handlers (Interrupt) = null then
         New_Task := new Server_Task (Interrupt);
         Handlers (Interrupt) := To_System (New_Task.all'Identity);
         if (Pthreads.Intr_Attach (C.Int (Interrupt),
                                   TISR (Signal_Handler'Access))
             = Pthreads.FUNC_ERR) then
            raise Program_Error;
         end if;
      end if;

--    Descriptors (Interrupt) :=
--      (Kind => Task_Entry,
--       T => T,
--       E => E);

      Descriptors (Interrupt).Kind := Task_Entry;
      Descriptors (Interrupt).T := T;
      Descriptors (Interrupt).E := E;

      Attached_Interrupts (Interrupt) := True;

   end Bind_Interrupt_To_Entry;

   ------------------------------
   -- Detach_Interrupt_Entries --
   ------------------------------

   procedure Detach_Interrupt_Entries (T : Tasking.Task_ID) is
   begin
      for I in Ada.Interrupts.Interrupt_ID loop
         if not Is_Reserved (I) then
            if Descriptors (I).Kind = Task_Entry and then
              Descriptors (I).T = T then
               Attached_Interrupts (I) := False;
--             Descriptors (I) := (Kind => Unknown);
               Descriptors (I).Kind := Unknown;

               if (Pthreads.Intr_Attach (C.Int (I), null)
                   = Pthreads.FUNC_ERR) then
                  raise Program_Error;
               end if;

            end if;
         end if;
      end loop;
   end Detach_Interrupt_Entries;

   procedure Unimplemented (Str : String) is

      procedure Put_Character (C : Integer);
      pragma Import (C, Put_Character, "putchar");

      procedure Prog_Exit (Status : Integer);
      pragma Import (C, Prog_Exit, "exit");

   begin
      for I in Str'Range loop
         Put_Character (Character'Pos (Str (I)));
      end loop;
      Put_Character (Character'Pos (Ascii.Lf));
      Prog_Exit (-1);
   end Unimplemented;



   ---------------------
   -- Block_Interrupt --
   ---------------------

   procedure Block_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) is
   begin
      Unimplemented ("Block_Interrupt");
   end Block_Interrupt;

   -----------------------
   -- Unblock_Interrupt --
   -----------------------

   procedure Unblock_Interrupt (Interrupt : Ada.Interrupts.Interrupt_ID) is
   begin
      Unimplemented ("Ada Signal Handlers");
   end Unblock_Interrupt;

   ----------------
   -- Is_Blocked --
   ----------------

   function Is_Blocked (Interrupt : Ada.Interrupts.Interrupt_ID)
      return boolean is
   begin
      Unimplemented ("Is_Blocked");
      return false;
   end Is_Blocked;

   task body Server_Task is
      Desc  : Handler_Desc renames Descriptors (Interrupt);
      Self  : Task_ID := System.Tasking.Self;
      Error : Boolean;
      Temp  : Parameterless_Handler;
   begin
      System.Tasking.Utilities.Make_Independent;
      loop
         while Interrupt_Count (Interrupt) > 0 loop
            Interrupt_Count (Interrupt) := Interrupt_Count (Interrupt) - 1;
            begin
               case Desc.Kind is
                  when Unknown =>
                     null;
                  when Task_Entry =>
                     System.Tasking.Rendezvous.Call_Simple
                       (Desc.T, Desc.E, Null_Address);
                  when Protected_Procedure =>
                     Temp := Desc.H;
                     Temp.all;
               end case;
            exception
               when others => null;
            end;
         end loop;

         System.Task_Primitives.Write_Lock (Self.L, Error);
         System.Task_Primitives.Cond_Wait (Self.Cond, Self.L);
         System.Task_Primitives.Unlock (Self.L);

      end loop;
   end Server_Task;

begin

   Usable_Interrupts  :=
        (INTNAM.SIGILL  => False,  -- Maps to Program_Error
         INTNAM.SIGABRT => False,  -- ???
         INTNAM.SIGFPE  => False,  -- Maps to Constraint_Error
         INTNAM.SIGSEGV => False,  -- Maps to Constraint_Error or Storage_Error
         INTNAM.SIGKILL => False,  -- Can't be caught or ignored
         INTNAM.SIGSYS  => False,  -- Maps to Program_Error
         INTNAM.SIGBUS  => False,  -- Maps to Constraint_Error
         INTNAM.SIGALRM => False,  -- Used for ada delay operations
         INTNAM.SIG32 .. Ada.Interrupts.Interrupt_ID'Last => False,
--                                 -- Reserved for kernel usage
         others  => True);

end System.Interrupts;
