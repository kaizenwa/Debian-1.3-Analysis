------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . D E B U G _ P O O L S                     --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                            $Revision: 1.4 $                              --
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

with Unchecked_Conversion;
with Gnat.Htable;

package body System.Debug_Pools is

   package SSE renames System.Storage_Elements;
   use type SSE.Storage_Offset;
   use type SSE.Integer_Address;

   Accessing_Not_Allocated_Storage : exception;
   Accessing_Reallocated_Storage   : exception;
   Accessing_Deallocated_Storage   : exception;
   Freeing_Not_Allocated_Storage   : exception;

   --  Definition of a H-table storing the status of each storage chunck
   --  used by this pool

   type State is (Not_Allocated, Deallocated, Allocated);
   type Status is record
      Stat : State;
      Siz  : SSE.Storage_Count;
   end record;

   type Header is range 1 .. 503;
   function H (F : Address) return Header;

   package Table is new GNAT.Htable.Simple_Htable (
     Header_Num => Header,
     Element    => Status,
     No_Element => Status'(Not_Allocated, 0),
     Key        => Address,
     Hash       => H,
     Equal      => "=");

   -------
   -- H --
   -------

   function H (F : Address) return Header is
   begin
      return Header (1 +
         (SSE.To_Integer (F) mod SSE.Integer_Address (Header'Last)));
   end H;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : out Address;
      Size_In_Storage_Elements : in SSE.Storage_Count;
      Alignment                : in SSE.Storage_Count)
   is
      function Malloc (Size : SSE.Storage_Count) return System.Address;
      pragma Import (C, Malloc, "__gnat_malloc");

   begin
      Storage_Address := Malloc (Size_In_Storage_Elements);

      if Storage_Address = Null_Address then
         raise Storage_Error;
      else
         Table.Set (Storage_Address,
           Status'(Allocated, Size_In_Storage_Elements));
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : in Address;
      Size_In_Storage_Elements : in SSE.Storage_Count;
      Alignment                : in SSE.Storage_Count)
   is
      procedure Free (Address : System.Address; Siz : SSE.Storage_Count);
      --  Faked free, that reset all the deallocated storage to "DEADBEEF"

      procedure Free (Address : System.Address; Siz : SSE.Storage_Count) is
         DB : constant SSE.Storage_Array (0 .. 3) :=
                (16#DE#, 16#AD#, 16#BE#, 16#EF#);
         DB_Index : SSE.Storage_Offset := DB'First;

         subtype Dead_Memory is SSE.Storage_Array (1 .. Siz);
         type Mem_Ptr is access all Dead_Memory;

         function From_Ptr is
           new Unchecked_Conversion (System.Address, Mem_Ptr);

      begin
         for J in Dead_Memory'Range loop
            From_Ptr (Address) (J) := DB (DB_Index);
            DB_Index := (DB_Index + 1) mod (DB'Last + 1);
         end loop;
      end Free;

      S : Status := Table.Get (Storage_Address);

   begin
      case S.Stat is
         when  Not_Allocated |
               Deallocated   =>
            raise Freeing_Not_Allocated_Storage;

         when Allocated =>

--  ??? Bug in Gigi, the size given by Deallocate is not always coherent
--      with the allocated size

--            if S.Siz /= Size_In_Storage_Elements then
--               raise Freeing_Not_Allocated_Storage;
--            else
               Free (Storage_Address, Size_In_Storage_Elements);
               Table.Set (Storage_Address, Status'(Deallocated, 0));
--            end if;
      end case;
   end Deallocate;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (Pool : Debug_Pool) return SSE.Storage_Count is
   begin
      return SSE.Storage_Count'Last;
   end Storage_Size;

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference
     (Pool                     : in out Debug_Pool;
      Storage_Address          : in Address;
      Size_In_Storage_Elements : in SSE.Storage_Count;
      Alignment                : in SSE.Storage_Count)
   is
      S : Status := Table.Get (Storage_Address);

   begin
      case S.Stat is
         when  Not_Allocated =>
            raise Accessing_Not_Allocated_Storage;

         when Deallocated =>
            raise Accessing_Deallocated_Storage;

         when Allocated =>
            if S.Siz /= Size_In_Storage_Elements then
               raise  Accessing_Reallocated_Storage;
            end if;
      end case;
   end Dereference;
end System.Debug_Pools;
