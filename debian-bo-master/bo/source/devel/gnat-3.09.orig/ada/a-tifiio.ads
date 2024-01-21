------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                 A D A . T E X T _ I O . F I X E D _ I O                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  In Ada 95, the package Ada.Text_IO.Fixed_IO is a subpackage of Text_IO.
--  This is for compatibility with Ada 83. In GNAT we make it a child package
--  to avoid loading the necessary code if Fixed_IO is not instantiated. See
--  routine Rtsfind.Text_IO_Kludge for a description of how we patch up the
--  difference in semantics so that it is invisible to the Ada programmer.

private generic
   type Num is delta <>;

package Ada.Text_IO.Fixed_IO is

   Default_Fore : Field := Num'Fore;
   Default_Aft  : Field := Num'Aft;
   Default_Exp  : Field := 0;

   procedure Get
     (File  : in File_Type;
      Item  : out Num;
      Width : in Field := 0);

   procedure Get
     (Item  : out Num;
      Width : in Field := 0);

   procedure Put
     (File : in File_Type;
      Item : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp);

   procedure Put
     (Item : in Num;
      Fore : in Field := Default_Fore;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp);

   procedure Get
     (From : in String;
      Item : out Num;
      Last : out Positive);

   procedure Put
     (To   : out String;
      Item : in Num;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp);

private
   pragma Inline (Get);
   pragma Inline (Put);

end Ada.Text_IO.Fixed_IO;
