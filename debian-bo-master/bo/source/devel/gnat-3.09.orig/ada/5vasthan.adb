------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                  S Y S T E M . A S T _ H A N D L I N G                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $                              --
--                                                                          --
--            Copyright (C) 1996 Free Software Foundation, Inc.             --
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

--  This is the OpenVMS/Alpha version.

with System; use System;

with System.Machine_Code;
with System.Storage_Elements;

with System.Tasking;
with System.Tasking.Rendezvous;
with System.Tasking_Soft_Links;
with System.Tasking.Utilities;

with System.Task_Primitives;
with System.Task_Primitives.Operations;

--  with Ada.Finalization;
--  removed, because of problem with controlled attribute ???

with Ada.Task_Attributes;
with Ada.Task_Identification;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body System.AST_Handling is

   package ATID renames Ada.Task_Identification;

   package ST   renames System.Tasking;
   package STSL renames System.Tasking_Soft_Links;
   package STR  renames System.Tasking.Rendezvous;
   package STU  renames System.Tasking.Utilities;

   package SSE  renames System.Storage_Elements;
   package STPO renames System.Task_Primitives.Operations;

   ---------------------------------
   -- AST_Handler Data Structures --
   ---------------------------------

   --  As noted in the private part of the spec of System.Aux_DEC, the
   --  AST_Handler type is simply a pointer to a procedure that takes
   --  a single address width parameter. The following is a local copy
   --  of that definition.

   --  We need our own copy because we need to get our hands on this
   --  and we cannot see the private part of System.Aux_DEC. We don't
   --  want to be a child of Aux_Dec because of complications resulting
   --  from the use of pragma Extend_System. We will use unchecked
   --  conversions between the two versions of the declarations.

   type AST_Handler is access procedure (Param : Address);

   --  However, this declaration is somewhat misleading, since the values
   --  referenced by AST_Handler values (all produced in this package by
   --  calls to Create_AST_Handler) are highly stylized.

   --  The first point is that in VMS/Alpha, procedure pointers do not in
   --  fact point to code, but rather to a 48-byte procedure descriptor.
   --  So a value of type AST_Handler is in fact a pointer to one of these
   --  48-byte descriptors.

   type Descriptor_Type is new SSE.Storage_Array (1 .. 48);
   for  Descriptor_Type'Alignment use Standard'Maximum_Alignment;
   type Descriptor_Ref is access all Descriptor_Type;

   --  Normally, there is only one such descriptor for a given procedure, but
   --  it works fine to make a copy of the single allocated descriptor, and
   --  use the copy itself, and we take advantage of this in the design here.
   --  The idea is that AST_Handler values will all point to a record with the
   --  following structure:

   --  Note: When we say it works fine, there is one delicate point, which
   --  is that the code for the AST procedure itself requires the original
   --  descriptor address.  We handle this by saving the orignal descriptor
   --  address in this structure and restoring in Process_AST.

   type AST_Handler_Data is record
      Descriptor              : Descriptor_Type;
      Original_Descriptor_Ref : Descriptor_Ref;
      Taskid                  : ATID.Task_Id;
      Entryno                 : Natural;
   end record;

   type AST_Handler_Data_Ref is access all AST_Handler_Data;

   function To_AST_Handler is new Ada.Unchecked_Conversion
     (AST_Handler_Data_Ref, System.Aux_DEC.AST_Handler);

   function To_AST_Data_Handler_Ref is new Ada.Unchecked_Conversion
     (System.Aux_DEC.AST_Handler, AST_Handler_Data_Ref);

   function To_AST_Data_Handler_Ref is new Ada.Unchecked_Conversion
     (AST_Handler, AST_Handler_Data_Ref);

   --  Each time Create_AST_Handler is called, a new value of this record
   --  type is created, containing a copy of the procedure descriptor for
   --  the routine used to handle all AST's (Process_AST), and the Task_Id
   --  and entry number parameters identifying the task entry involved.

   --  The AST_Handler value returned is a pointer to this record. Since
   --  the record starts with the procedure descriptor, it can be used
   --  by the system in the normal way to call the procedure. But now
   --  when the procedure gets control, it can determine the address of
   --  the procedure descriptor used to call it (since the ABI specifies
   --  that this is left sitting in register r27 on entry), and then use
   --  that address to retrieve the Task_Id and entry number so that it
   --  knows on which entry to queue the AST request.

   --  The next issue is where are these records placed. Since we intend
   --  to pass pointers to these records to asynchronous system service
   --  routines, they have to be on the heap, which means we have to worry
   --  about when to allocate them and deallocate them.

   --  We solve this problem by introducing a task attribute that points to
   --  a vector, indexed by the entry number, of AST_Handler_Data records
   --  for a given task. The pointer itself is a controlled object allowing
   --  us to write a finalization routine that frees the referenced vector.

   --  An entry in this vector is either initialized (Entryno non-zero) and
   --  can be used for any subsequent reference to the same entry, or it is
   --  unused, marked by the Entryno value being zero.

   type AST_Handler_Vector is array (Natural range <>) of AST_Handler_Data;
   type AST_Handler_Vector_Ref is access all AST_Handler_Vector;
   procedure Free is new Ada.Unchecked_Deallocation
     (Object => AST_Handler_Vector,
      Name   => AST_Handler_Vector_Ref);

--  type AST_Vector_Ptr is new Ada.Finalization.Controlled with record
--  removed due to problem with controlled attribute, consequence is that
--  we have a memory leak if a task that has AST attribute entries is
--  terminated. ???

   type AST_Vector_Ptr is record
      Vector : AST_Handler_Vector_Ref;
   end record;

   procedure Finalize (Object : in out AST_Vector_Ptr);
   --  Used to get rid of allocated AST_Vector's

   AST_Vector_Init : AST_Vector_Ptr;
   --  Initial value, treated as constant, Vector will be null.

   package AST_Attribute is new Ada.Task_Attributes
     (Attribute     => AST_Vector_Ptr,
      Initial_Value => AST_Vector_Init);

   use AST_Attribute;

   -----------------------
   -- AST Service Queue --
   -----------------------

   --  The following global data structures are used to queue pending
   --  AST requests. When an AST is signalled, the AST service routine
   --  Process_AST is called, and it makes an entry in this structure.

   type AST_Instance is record
      Taskid  : ATID.Task_Id;
      Entryno : Natural;
      Param   : System.Address;
   end record;
   --  The Taskid and Entryno indicate the entry on which this AST is to
   --  be queued, and Param is the parameter provided from the AST itself.

   AST_Service_Queue_Size  : constant := 256;
   AST_Service_Queue_Limit : constant := 250;
   type AST_Service_Queue_Index is mod AST_Service_Queue_Size;
   --  Index used to refer to entries in the circular buffer which holds
   --  active AST_Instance values. The upper bound reflects the maximum
   --  number of AST instances that can be stored in the buffer. Since
   --  these entries are immediately serviced by the high priority server
   --  task that does the actual entry queuing, it is very unusual to have
   --  any significant number of entries simulaneously queued.

   AST_Service_Queue : array (AST_Service_Queue_Index) of AST_Instance;
   pragma Volatile_Components (AST_Service_Queue);
   --  The circular buffer used to store active AST requests.

   AST_Service_Queue_Put : AST_Service_Queue_Index := 0;
   AST_Service_Queue_Get : AST_Service_Queue_Index := 0;
   pragma Atomic (AST_Service_Queue_Put);
   pragma Atomic (AST_Service_Queue_Get);
   --  These two variables point to the next slots in the AST_Service_Queue
   --  to be used for putting a new entry in and taking an entry out. This
   --  is a circular buffer, so these pointers wrap around. If the two values
   --  are equal the buffer is currently empty. The pointers are atomic to
   --  ensure proper synchronization between the single producer (namely the
   --  Process_AST procedure), and the single consumer (the AST_Service_Task).

   --------------------------------
   -- AST Server Task Structures --
   --------------------------------

   --  The basic approach is that when an AST comes in, a call is made to
   --  the Process_AST procedure. It queues the request in the service queue
   --  and then wakes up an AST server task to perform the actual call to the
   --  required entry. We use this intermediate server task, since the AST
   --  procedure itself cannot wait to return, and we need some caller for
   --  the rendezvous so that we can use the normal rendezvous mechanism.

   --  It would work to have only one AST server task, but then we would lose
   --  all overlap in AST processing, and furthermore, we could get priority
   --  inversion effects resulting in starvation of AST requests.

   --  We therefore maintain a small pool of AST server tasks. We adjust
   --  the size of the pool dynamically to reflect traffic, so that we have
   --  a sufficient number of server tasks to avoid starvation.

   Max_AST_Servers : constant Natural := 16;
   --  Maximum number of AST server tasks that can be allocated

   Num_AST_Servers : Natural := 0;
   --  Number of AST server tasks currently active

   Num_Waiting_AST_Servers : Natural := 0;
   --  This is the number of AST server tasks that are either waiting for
   --  work, or just about to go to sleep and wait for work.

   Is_Waiting : array (1 .. Max_AST_Servers) of Boolean := (others => False);
   --  An array of flags showing which AST server tasks are currently waiting

   AST_Task_Ids : array (1 .. Max_AST_Servers) of ST.Task_Id;
   --  Task Id's of allocated AST server tasks

   task type AST_Server_Task (Num : Natural) is
      pragma Priority (Priority'Last);
   end AST_Server_Task;
   --  Declaration for AST server task. This task has no entries, it is
   --  controlled by sleep and wakeup calls at the task primitives level.

   type AST_Server_Task_Ptr is access all AST_Server_Task;
   --  Type used to allocate server tasks

   function To_Integer is new Ada.Unchecked_Conversion
     (ATID.Task_Id, Integer);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Allocate_New_AST_Server;
   --  Allocate an additional AST server task

   procedure Process_AST (Param : System.Address);
   --  This is the central routine for processing all AST's, it is referenced
   --  as the code address of all created AST_Handler values. See detailed
   --  description in body to understand how it works to have a single such
   --  procedure for all AST's even though it does not get any indication of
   --  the entry involved passed as an explicit parameter. The single explicit
   --  parameter Param is the parameter passed by the system with the AST.

   -----------------------------
   -- Allocate_New_AST_Server --
   -----------------------------

   procedure Allocate_New_AST_Server is
      Dummy : AST_Server_Task_Ptr;

   begin
      if Num_AST_Servers = Max_AST_Servers then
         return;

      else
         --  Note: it is safe to increment Num_AST_Servers immediately, since
         --  no one will try to activate this task until it indicates that it
         --  is sleeping by setting its entry in Is_Waiting to True.

         Num_AST_Servers := Num_AST_Servers + 1;
         Dummy := new AST_Server_Task (Num_AST_Servers);
      end if;
   end Allocate_New_AST_Server;

   ---------------------
   -- AST_Server_Task --
   ---------------------

   task body AST_Server_Task is
      Taskid  : ATID.Task_Id;
      Entryno : Natural;
      Param   : aliased Address;
      Self_Id : constant ST.Task_Id := ST.Self;

      pragma Volatile (Param);

   begin
      --  By making this task independent of master, when the process
      --  goes away, the Interrupt_Manager will terminate gracefully.

      STU.Make_Independent;

      --  Record our task Id for access by Process_AST

      AST_Task_Ids (Num) := Self_Id;

      --  Note: this entire task operates with the main task lock set, except
      --  when it is sleeping waiting for work, or busy doing a rendezvous
      --  with an AST server. This lock protects the data structures that
      --  are shared by multiple instances of the server task.

      STSL.Lock_Task;

      --  This is the main infinite loop of the task. We go to sleep and
      --  wait to be woken up by Process_AST when there is some work to do.

      loop
         Num_Waiting_AST_Servers := Num_Waiting_AST_Servers + 1;

         STSL.Unlock_Task;
         Is_Waiting (Num) := True;
         STPO.Sleep (Self_Id);
         STPO.Unlock (Self_Id);

         --  We are awake, there is something to do!

         STSL.Lock_Task;
         Num_Waiting_AST_Servers := Num_Waiting_AST_Servers - 1;

         --  Loop here to service outstanding requests. We are always
         --  locked on entry to this loop.

         while AST_Service_Queue_Get /= AST_Service_Queue_Put loop
            Taskid  := AST_Service_Queue (AST_Service_Queue_Get).Taskid;
            Entryno := AST_Service_Queue (AST_Service_Queue_Get).Entryno;
            Param   := AST_Service_Queue (AST_Service_Queue_Get).Param;

            AST_Service_Queue_Get := AST_Service_Queue_Get + 1;

            --  This is a manual expansion of the normal call simple code

            declare
               type AA is access all Address;
               P : AA := Param'Access;

               function To_ST_Task_Id is new Ada.Unchecked_Conversion
                 (ATID.Task_Id, ST.Task_Id);

            begin
               STSL.Unlock_Task;
               STR.Call_Simple
                 (Acceptor           => To_ST_Task_Id (Taskid),
                  E                  => ST.Task_Entry_Index (Entryno),
                  Uninterpreted_Data => P'Address);
               STSL.Lock_Task;
            end;
         end loop;
      end loop;

   end AST_Server_Task;

   ------------------------
   -- Create_AST_Handler --
   ------------------------

   function Create_AST_Handler
     (Taskid  : ATID.Task_Id;
      Entryno : Natural)
      return    System.Aux_DEC.AST_Handler
   is
      Attr_Ref : Attribute_Handle;
      Temp_Vec : AST_Handler_Vector_Ref;

      Process_AST_Ptr : constant AST_Handler := Process_AST'Access;
      --  Reference to standard procedure descriptor for Process_AST

      function To_Descriptor_Ref is new Ada.Unchecked_Conversion
        (AST_Handler, Descriptor_Ref);

      Original_Descriptor_Ref : Descriptor_Ref :=
                                  To_Descriptor_Ref (Process_AST_Ptr);

   begin
      if ATID.Is_Terminated (Taskid) then
         raise Program_Error;
      end if;

      Attr_Ref := Reference (Taskid);

      --  Allocate another server if supply is getting low

      if Num_Waiting_AST_Servers < 2 then
         Allocate_New_AST_Server;
      end if;

      --  No point in creating more if we have zillions waiting to
      --  be serviced.

      while AST_Service_Queue_Put - AST_Service_Queue_Get
         > AST_Service_Queue_Limit
      loop
         delay 0.01;
      end loop;

      --  If no AST vector allocated, or the one we have is too short, then
      --  allocate one of right size and initialize all entries except the
      --  one we will use to unused. Note that the assignment automatically
      --  frees the old allocated table if there is one.

      if Attr_Ref.Vector = null
        or else Attr_Ref.Vector'Length < Entryno
      then
         Attr_Ref.Vector := new AST_Handler_Vector (1 .. Entryno);

         for E in 1 .. Entryno loop
            Attr_Ref.Vector (E).Descriptor :=
              Original_Descriptor_Ref.all;
            Attr_Ref.Vector (E).Original_Descriptor_Ref :=
              Original_Descriptor_Ref;
            Attr_Ref.Vector (E).Taskid  := Taskid;
            Attr_Ref.Vector (E).Entryno := E;
         end loop;
      end if;

      return To_AST_Handler (Attr_Ref.Vector (Entryno)'Unrestricted_Access);
   end Create_AST_Handler;

   ----------------------------
   -- Expand_AST_Packet_Pool --
   ----------------------------

   procedure Expand_AST_Packet_Pool
     (Requested_Packets : in Natural;
      Actual_Number     : out Natural;
      Total_Number      : out Natural)
   is
   begin
      --  Not yet fully implemented, just say we added none and return how
      --  many total.

      Actual_Number := 0;
      Total_Number := AST_Service_Queue_Size;
   end Expand_AST_Packet_Pool;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out AST_Vector_Ptr) is
   begin
      Free (Object.Vector);
   end Finalize;

   -----------------
   -- Process_AST --
   -----------------

   procedure Process_AST (Param : Address) is

      Handler_Data_Ptr : AST_Handler_Data_Ref;
      --  This variable is set to the address of the descriptor through
      --  which Process_AST is called. Since the descriptor is part of
      --  an AST_Handler value, this is also the address of this value,
      --  from which we can obtain the task and entry number information.

   begin
      System.Machine_Code.Asm
        (Template => "addl $27,0,%0",
         Outputs  => AST_Handler_Data_Ref'Asm_Output ("=r", Handler_Data_Ptr),
         Volatile => True);

      System.Machine_Code.Asm
        (Template => "ldl $27,%0",
         Inputs  => Descriptor_Ref'Asm_Input
           ("m", Handler_Data_Ptr.Original_Descriptor_Ref),
         Volatile => True);

      AST_Service_Queue (AST_Service_Queue_Put) := AST_Instance'
        (Taskid  => Handler_Data_Ptr.Taskid,
         Entryno => Handler_Data_Ptr.Entryno,
         Param   => Param);

      AST_Service_Queue_Put := AST_Service_Queue_Put + 1;

      --  Need to wake up processing task. If there is no waiting server
      --  then we have temporarily run out, but things should still be
      --  OK, since one of the active ones will eventually pick up the
      --  service request queued in the AST_Service_Queue.

      for J in 1 .. Num_AST_Servers loop
         if Is_Waiting (J) then
            Is_Waiting (J) := False;
            STPO.Wakeup (AST_Task_Ids (J));
            exit;
         end if;
      end loop;
   end Process_AST;

end System.AST_Handling;
