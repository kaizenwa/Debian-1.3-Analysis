------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                          A D A . S T R E A M S                           --
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


package Ada.Streams is
pragma Pure (Streams);

   type Root_Stream_Type is abstract tagged limited private;

   type Stream_Element is mod 2 ** Standard'Storage_Unit;

   type Stream_Element_Offset is range
     -(2 ** (Standard'Address_Size - 1)) ..
     +(2 ** (Standard'Address_Size - 1)) - 1;

   subtype Stream_Element_Count is
      Stream_Element_Offset range 0 .. Stream_Element_Offset'Last;

   type Stream_Element_Array is
      array (Stream_Element_Offset range <>) of Stream_Element;

   procedure Read
     (Stream : in out Root_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is abstract;

   procedure Write
     (Stream : in out Root_Stream_Type;
      Item   : in Stream_Element_Array)
   is abstract;

private

   type Root_Stream_Type is abstract tagged limited null record;

end Ada.Streams;
