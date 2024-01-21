------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           S Y S T E M . R P C                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.14 $                             --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  Note: this is a dummy implementation which does not support distribution.
--  The GLADE distribution package includes a replacement for this file which
--  has a different private

with Ada.Streams;
with System.Storage_Elements;

package System.RPC is

   type Partition_ID is range 0 .. 63; --  temporary declaration

   Communication_Error : exception;

   type Params_Stream_Type
     (Initial_Size : Ada.Streams.Stream_Element_Count) is new
       Ada.Streams.Root_Stream_Type with private;

   procedure Read
     (Stream : in out Params_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Params_Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array);

   --  Synchronous call

   procedure Do_RPC
     (Partition  : in Partition_ID;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type);

   --  Asynchronous call

   procedure Do_APC
     (Partition  : in Partition_ID;
      Params     : access Params_Stream_Type);

   --  The handler for incoming RPCs.

   type RPC_Receiver is
     access procedure
       (Params     : access Params_Stream_Type;
        Result     : access Params_Stream_Type);

   procedure Establish_RPC_Receiver (
      Partition : in Partition_ID;
      Receiver  : in RPC_Receiver);

private

   type Params_Stream_Type
     (Initial_Size : Ada.Streams.Stream_Element_Count) is new
       Ada.Streams.Root_Stream_Type with null record;

end System.RPC;
