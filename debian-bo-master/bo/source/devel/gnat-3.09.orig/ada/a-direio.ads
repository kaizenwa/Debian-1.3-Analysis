------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                        A D A . D I R E C T _ I O                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.15 $                             --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------


with Ada.IO_Exceptions;
with System.Direct_IO;
with Interfaces.C_Streams;

generic
   type Element_Type is private;

package Ada.Direct_IO is

   type File_Type is limited private;

   type File_Mode is (In_File, Inout_File, Out_File);

   --  The following representation clause allows the use of unchecked
   --  conversion for rapid translation between the File_Mode type
   --  used in this package and System.File_IO.

   for File_Mode use
     (In_File     => 0,   -- System.Fole_IO.File_Mode'Pos (In_File)
      Inout_File  => 1,   -- System.File_IO.File_Mode'Pos (Inout_File);
      Out_File    => 2);  -- System.File_IO.File_Mode'Pos (Out_File)

   type Count is new System.Direct_IO.Count;

   subtype Positive_Count is Count range 1 .. Count'Last;

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (File : in out File_Type;
      Mode : in File_Mode := Inout_File;
      Name : in String := "";
      Form : in String := "");

   procedure Open
     (File : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "");

   procedure Close  (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset  (File : in out File_Type; Mode : in File_Mode);
   procedure Reset  (File : in out File_Type);

   function Mode (File : in File_Type) return File_Mode;
   function Name (File : in File_Type) return String;
   function Form (File : in File_Type) return String;

   function Is_Open (File : in File_Type) return Boolean;

   ---------------------------------
   -- Input and Output Operations --
   ---------------------------------

   procedure Read
     (File : in File_Type;
      Item : out Element_Type;
      From : in Positive_Count);

   procedure Read
     (File : in File_Type;
      Item : out Element_Type);

   procedure Write
     (File : in File_Type;
      Item : in Element_Type;
      To   : in Positive_Count);

   procedure Write
     (File : in File_Type;
      Item : in Element_Type);

   procedure Set_Index (File : in File_Type; To : in Positive_Count);

   function Index (File : in File_Type) return Positive_Count;
   function Size  (File : in File_Type) return Count;

   function End_Of_File (File : in File_Type) return Boolean;

   ----------------
   -- Exceptions --
   ----------------

   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error   : exception renames IO_Exceptions.Mode_Error;
   Name_Error   : exception renames IO_Exceptions.Name_Error;
   Use_Error    : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error    : exception renames IO_Exceptions.End_Error;
   Data_Error   : exception renames IO_Exceptions.Data_Error;

private
   type File_Type is new System.Direct_IO.File_Type;

   Bytes : constant Interfaces.C_Streams.size_t :=
             Interfaces.C_Streams.size_t
               ((Element_Type'Size + System.Storage_Unit - 1) /
                                                System.Storage_Unit);
   --  Size of an element in storage units

   pragma Inline (Close);
   pragma Inline (Create);
   pragma Inline (Delete);
   pragma Inline (End_Of_File);
   pragma Inline (Form);
   pragma Inline (Index);
   pragma Inline (Is_Open);
   pragma Inline (Mode);
   pragma Inline (Name);
   pragma Inline (Open);
   pragma Inline (Read);
   pragma Inline (Reset);
   pragma Inline (Set_Index);
   pragma Inline (Size);
   pragma Inline (Write);

end Ada.Direct_IO;
