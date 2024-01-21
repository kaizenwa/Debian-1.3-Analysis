------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--         S Y S T E M . T A S K I N G . T A S K _ A T T R I B U T E S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.5 $                             --
--                                                                          --
--             Copyright (C) 1995,1996 Florida State University             --
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

with System.Task_Primitives;
with System.Tasking.Initialization;
with System.Tasking.Utilities;
with Unchecked_Conversion;

package body System.Tasking.Task_Attributes is

   use System.Tasking,
       System.Task_Primitives,
       System.Tasking.Initialization,
       System.Tasking.Utilities;

   function To_Access_Node is new Unchecked_Conversion
     (Access_Address, Access_Node);
   --  Tetch pointer to indirect attribute list

   function To_Access_Address is new Unchecked_Conversion
     (Access_Node, Access_Address);
   --  Store pointer to indirect attribute list

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Instance) is
      Lock_Result : Boolean;

   begin
      Defer_Abortion;
      Write_Lock (All_Attrs_L, Lock_Result);
      pragma Assert
        (not Lock_Result or else
         System.Tasking.Utilities.Runtime_Assert_Shutdown
         ("Locking error"));

      --  Remove this instantiation from the list of all instantiations.

      declare
         P : Access_Instance;
         Q : Access_Instance := All_Attributes;

      begin
         while Q /= null and then Q /= X'Unchecked_Access loop
            P := Q; Q := Q.Next;
         end loop;

         pragma Assert (Q /= null or else
           System.Tasking.Utilities.Runtime_Assert_Shutdown
             ("Ran off end of All_Attributes list"));

         if P = null then
            All_Attributes := Q.Next;
         else
            P.Next := Q.Next;
         end if;
      end;

      if X.Index /= 0 then

         --  Free location of this attribute, for reuse.

         In_Use := In_Use and not (2**Natural (X.Index));

         --  There is no need for finalization in this case,
         --  since controlled types are too big to fit in the TCB.

      else
         --  Remove nodes for this attribute from the lists of
         --  all tasks, and deallocate the nodes.
         --  Deallocation does finalization, if necessary.

         Write_Lock
           (System.Tasking.Initialization.All_Tasks_L, Lock_Result);
         pragma Assert (not Lock_Result or else
            System.Tasking.Utilities.Runtime_Assert_Shutdown
              ("Attribute finalization locking error"));

         declare
            C    : System.Tasking.Task_ID :=
                     System.Tasking.Initialization.All_Tasks_List;
            P, Q : Access_Node;

         begin
            while C /= null loop
               Write_Lock (C.L, Lock_Result);

               pragma Assert (not Lock_Result or else
                 System.Tasking.Utilities.Runtime_Assert_Shutdown
                   ("Locking error"));

               Q := To_Access_Node (C.Indirect_Attributes);
               while Q /= null
                 and then Q.Instance /= X'Unchecked_Access
               loop
                  P := Q;
                  Q := Q.Next;
               end loop;

               if Q /= null then
                  if P = null then
                     C.Indirect_Attributes := To_Access_Address (Q.Next);
                  else
                     P.Next := Q.Next;
                  end if;

                  X.Deallocate.all (Q);
               end if;

               Unlock (C.L);
               C := C.All_Tasks_Link;
            end loop;
         end;

         Unlock (System.Tasking.Initialization.All_Tasks_L);
      end if;

      Unlock (All_Attrs_L);
      Undefer_Abortion;

   exception
      when others => null;
         pragma Assert
           (System.Tasking.Utilities.Runtime_Assert_Shutdown
             ("Exception in task attribute instance finalization"));
   end Finalize;

   -------------------------
   -- Finalize Attributes --
   -------------------------

   --  This is to be called from inside System.Task_Stages.Leave_Task.
   --  It relies on the caller holding T.L write-lock on entry.

   procedure Finalize_Attributes (T : Task_ID) is
      P : Access_Node;
      Q : Access_Node := To_Access_Node (T.Indirect_Attributes);

   begin
      --  Deallocate all the indirect attributes of this task.

      while Q /= null loop
         P := Q;
         Q := Q.Next; P.Instance.Deallocate.all (P);
      end loop;

      T.Indirect_Attributes := null;

   exception
      when others => null;
         pragma Assert (
           Utilities.Runtime_Assert_Shutdown (
           "Exception in per-task attributes finalization"));
   end Finalize_Attributes;

   ---------------------------
   -- Initialize Attributes --
   ---------------------------

   --  This is to be called by System.Task_Stages.Create_Task.
   --  It relies on their being no concurrent access to this TCB,
   --  so it does not defer abortion or lock T.L.

   procedure Initialize_Attributes (T : Task_ID) is
      Lock_Result : Boolean;
      P           : Access_Instance;
      Q           : Access_Node;

   begin
      Write_Lock (All_Attrs_L, Lock_Result);
      pragma Assert
        (not Lock_Result or else
         System.Tasking.Utilities.Runtime_Assert_Shutdown ("Locking error"));

      --  Initialize all the direct-access attributes of this task.

      P := All_Attributes;
      while P /= null loop
         if P.Index /= 0 then
            T.Direct_Attributes (P.Index) :=
              System.Storage_Elements.To_Address (P.Initial_Value);
         end if;

         P := P.Next;
      end loop;

      Unlock (All_Attrs_L);

   exception
      when others => null;
         pragma Assert (
           Utilities.Runtime_Assert_Shutdown
             ("Exception in per-task attributes initialization"));
   end Initialize_Attributes;

begin
   System.Task_Primitives.Initialize_Lock
     (System.Any_Priority'Last, All_Attrs_L);
end System.Tasking.Task_Attributes;
