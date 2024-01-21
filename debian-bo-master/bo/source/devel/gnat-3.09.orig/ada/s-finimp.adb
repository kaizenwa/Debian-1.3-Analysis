------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--    S Y S T E M . F I N A L I Z A T I O N _ I M P L E M E N T A T I O N   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.26 $                             --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with System.Tasking_Soft_Links;

package body System.Finalization_Implementation is

   use Ada.Exceptions;
   use System.Finalization_Root;
   use System.Tasking_Soft_Links;

   package SSE renames System.Storage_Elements;
   use type SSE.Storage_Offset;

   procedure Raise_From_Finalize
     (P     : Finalizable_Ptr;
      E_Occ : Exception_Occurrence);
   --  Deal with an exception raised during finalization of a list. The
   --  exception is represented by its occurrence E_Occ and P is a pointer
   --  to the list of element not yet finalized.

   --------------------------
   -- Attach_To_Final_List --
   --------------------------

   procedure Attach_To_Final_List
     (L       : in out Finalizable_Ptr;
      Obj     : in out Finalizable;
      Nb_Link : Short_Short_Integer)
   is
   begin
      if Nb_Link = 1 then
         Obj.Next         := L;
         L                := Obj'Unchecked_Access;

      elsif Nb_Link = 2 then

         --  Attachement is protected against multi-threaded access

         Lock_Task.all;
         Obj.Next    := L.Next;
         Obj.Prev    := L.Next.Prev;
         L.Next.Prev := Obj'Unchecked_Access;
         L.Next      := Obj'Unchecked_Access;
         Unlock_Task.all;
      end if;
   end Attach_To_Final_List;


   -----------------------------
   -- Detach_From_Final_List --
   -----------------------------

   procedure Detach_From_Final_List (Obj : in out Finalizable) is
   --  We know that the detach object is neither at the beginning nor at the
   --  end of the list, thank's to the dummy First and Last Elements

   begin
      Lock_Task.all;
      Obj.Next.Prev := Obj.Prev;
      Obj.Prev.Next := Obj.Next;
      Unlock_Task.all;
   end Detach_From_Final_List;

   --------------------------
   --  Raise_From_Finalize --
   --------------------------

   procedure Raise_From_Finalize
     (P     : Finalizable_Ptr;
      E_Occ : Exception_Occurrence)
   is
      Msg : constant String := Exception_Message (E_Occ);

   begin
      Finalize_List (P);

      if Msg = "" then
         Raise_Exception (
           E       => Program_Error'Identity,
           Message => "exception "
           & Exception_Name (E_Occ) & " raised during finalization");
      else
         Raise_Exception (Program_Error'Identity, Msg);
      end if;
   end Raise_From_Finalize;

   -------------------
   -- Finalize_List --
   -------------------

   procedure Finalize_List (L : Finalizable_Ptr) is
      P     : Finalizable_Ptr := L;
      Q     : Finalizable_Ptr;

   begin
      while P /= null loop
         Q := P.Next;
         Finalize (P.all);
         P := Q;
      end loop;

   exception
      when E_Occ : others => Raise_From_Finalize (Q, E_Occ);
   end Finalize_List;

   --------------------------
   -- Finalize_Global_List --
   --------------------------

   procedure Finalize_Global_List is
   begin
      Finalize_List (Global_Final_List);
   end Finalize_Global_List;

   ------------------
   -- Finalize_One --
   ------------------

   procedure Finalize_One (Obj : in out  Finalizable) is
   begin
      Detach_From_Final_List (Obj);
      Finalize (Root_Controlled'Class (Obj));

   exception
      when E_Occ : others => Raise_From_Finalize (null, E_Occ);
   end Finalize_One;

   ----------------------------------
   -- Record_Controller Management --
   ----------------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Limited_Record_Controller) is
   begin
      null;
   end Initialize;

   procedure Initialize (Object : in out Record_Controller) is
   begin
      Object.My_Address := Object'Address;
   end Initialize;

   -------------
   --  Adjust --
   -------------

   procedure Adjust (Object : in out Record_Controller) is

      My_Offset : constant SSE.Storage_Offset :=
                    Object.My_Address - Object'Address;

      P : Finalizable_Ptr;

      procedure Ptr_Adjust (Ptr : in out Finalizable_Ptr);
      --  Substract the offset to the pointer

      procedure Reverse_Adjust (P : Finalizable_Ptr);
      --  Ajust the components in the reverse order in which they are stored
      --  on the finalization list. (Adjust and Finalization are not done in
      --  the same order)

      procedure Ptr_Adjust (Ptr : in out Finalizable_Ptr) is
         function To_Addr is
           new Ada.Unchecked_Conversion (Finalizable_Ptr, Address);

         function To_Ptr is
           new Ada.Unchecked_Conversion (Address, Finalizable_Ptr);

      begin
         if Ptr /= null then
            Ptr := To_Ptr (To_Addr (Ptr) - My_Offset);
         end if;
      end Ptr_Adjust;

      procedure Reverse_Adjust (P : Finalizable_Ptr) is
      begin
         if P /= null then
            Ptr_Adjust (P.Next);
            Reverse_Adjust (P.Next);
            Adjust (P.all);
         end if;
      end Reverse_Adjust;

   begin

      --  Adjust the components and their finalization pointers next

      Ptr_Adjust (Object.F);
      Reverse_Adjust (Object.F);

      --  then Adjust the object itself

      Object.My_Address := Object'Address;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize   (Object : in out Limited_Record_Controller) is
   begin
      Finalize_List (Object.F);
   end Finalize;

end System.Finalization_Implementation;
