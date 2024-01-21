------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--             A D A . W I D E _ T E X T _ I O . F L O A T _ I O            --
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

--  In Ada 95, the package Ada.Wide_Text_IO.Float_IO is a subpackage
--  of Wide_Text_IO. In GNAT we make it a child package to avoid loading
--  the necessary code if Float_IO is not instantiated. See the routine
--  Rtsfind.Text_IO_Kludge for a description of how we patch up the
--  difference in semantics so that it is invisible to the Ada programmer.

private generic
   type Num is digits <>;

package Ada.Wide_Text_IO.Float_IO is

   Default_Fore : Field := 2;
   Default_Aft  : Field := Num'Digits - 1;
   Default_Exp  : Field := 3;

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
     (From : in Wide_String;
      Item : out Num;
      Last : out Positive);

   procedure Put
     (To   : out Wide_String;
      Item : in Num;
      Aft  : in Field := Default_Aft;
      Exp  : in Field := Default_Exp);

end Ada.Wide_Text_IO.Float_IO;
