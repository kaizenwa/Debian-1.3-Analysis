------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--            S Y S T E M . P A R T I T I O N _ I N T E R F A C E           --
--                                                                          --
--                                  B o d y                                 --
--                   (Dummy body for non-distributed case)                  --
--                                                                          --
--                             $Revision: 1.7 $                             --
--                                                                          --
--          Copyright (C) 1995,1996 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

package body System.Partition_Interface is

   function getpid return Integer;
   pragma Import (C, getpid);

   ----------------------
   -- Elaboration_Type --
   ----------------------

   protected body Elaboration_Type is

      ------------------
      -- Get_RCI_Data --
      ------------------

      entry Get_RCI_Data
        (Receiver  : out System.RPC.RPC_Receiver;
         Partition : out System.RPC.Partition_ID;
         Done      : out Boolean)
      when True is
      begin
         Receiver  := null;
         Partition := Get_Local_Partition_ID;
         Done      := True;
      end Get_RCI_Data;

      ------------------
      -- Set_RCI_Data --
      ------------------

      procedure Set_RCI_Data
        (RCI_Name  : Unit_Name_Access;
         Receiver  : System.RPC.RPC_Receiver;
         Partition : System.RPC.Partition_ID)
      is
      begin
         null;
      end Set_RCI_Data;

      ---------------------------
      -- Initiate_Invalidation --
      ---------------------------

      procedure Initiate_Invalidation is
      begin
         null;
      end Initiate_Invalidation;

      ---------------------------
      -- Complete_Invalidation --
      ---------------------------

      procedure Complete_Invalidation is
      begin
         null;
      end Complete_Invalidation;

      ------------------
      -- Get_RCI_Unit --
      ------------------

      function Get_RCI_Unit return Unit_Name_Access is
      begin
         return null;
      end Get_RCI_Unit;

   end Elaboration_Type;

   -----------------------------
   -- Get_Active_Partition_Id --
   -----------------------------

   function Get_Active_Partition_ID
     (RCI_Unit : in Unit_Name)
      return     System.RPC.Partition_ID
   is
   begin
      return Get_Local_Partition_ID;
   end Get_Active_Partition_ID;

   -----------------------------
   -- Get_Active_Partition_ID --
   -----------------------------

   function Get_Active_Partition_ID
     (RCI_Unit    : in Unit_Name_Access;
      Elaboration : in Elaboration_Access)
      return        System.Rpc.Partition_ID
   is
   begin
      return Get_Local_Partition_ID;
   end Get_Active_Partition_ID;

   ------------------------
   -- Get_Active_Version --
   ------------------------

   function Get_Active_Version (RCI_Unit : in Unit_Name) return String is
   begin
      return "";
   end Get_Active_Version;

   ----------------------------
   -- Get_Local_Partition_Id --
   ----------------------------

   function Get_Local_Partition_ID return System.RPC.Partition_ID is
   begin
      return System.RPC.Partition_ID (getpid mod 7);
   end Get_Local_Partition_ID;

   ------------------------------
   -- Get_Passive_Partition_ID --
   ------------------------------

   function Get_Passive_Partition_ID
     (RCI_Unit : Unit_Name)
      return     System.RPC.Partition_ID
   is
   begin
      return Get_Local_Partition_ID;
   end Get_Passive_Partition_ID;

   ------------------------------
   -- Get_RCI_Package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (RCI_Unit : in Unit_Name)
      return     System.RPC.RPC_Receiver
   is
   begin
      return null;
   end Get_RCI_Package_Receiver;

   ------------------------------
   -- Get_RCI_Package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (RCI_Unit    : in Unit_Name_Access;
      Elaboration : in Elaboration_Access)
      return        System.RPC.RPC_Receiver
   is
   begin
      return null;
   end Get_RCI_Package_Receiver;

   -------------------------------
   -- Invalidate_Receiving_Stub --
   -------------------------------

   procedure Invalidate_Receiving_Stub
     (RCI_Unit  : in Unit_Name_Access;
      Partition : in RPC.Partition_ID)
   is
   begin
      null;
   end Invalidate_Receiving_Stub;

   --------------
   -- RCI_Info --
   --------------

   package body RCI_Info is

      -----------------------------
      -- Get_Active_Partition_ID --
      -----------------------------

      function Get_Active_Partition_ID return System.RPC.Partition_ID is
      begin
         return Get_Local_Partition_ID;
      end Get_Active_Partition_ID;

      ------------------------------
      -- Get_RCI_Package_Receiver --
      ------------------------------

      function Get_RCI_Package_Receiver return System.RPC.RPC_Receiver is
      begin
         return null;
      end Get_RCI_Package_Receiver;

   end RCI_Info;

   ---------------------------
   -- Register_Calling_Stub --
   ---------------------------

   procedure Register_Calling_Stub
     (Partition    : in RPC.Partition_ID;
      Elaboration  : in Elaboration_Access)
   is
   begin
      null;
   end Register_Calling_Stub;

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (RCI_Unit : in Unit_Name;
      Receiver : in RPC.RPC_Receiver;
      Version  : in String := "")
   is
   begin
      null;
   end Register_Receiving_Stub;

end System.Partition_Interface;
