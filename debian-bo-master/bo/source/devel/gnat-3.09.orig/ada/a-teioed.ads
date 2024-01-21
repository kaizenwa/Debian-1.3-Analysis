------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                  A D A . T E X T _ I O . E D I T I N G                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $                              --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

package Ada.Text_IO.Editing is

   type Picture is private;

   function Valid
     (Pic_String      : in String;
      Blank_When_Zero : in Boolean := False)
      return            Boolean;

   function To_Picture
     (Pic_String      : in String;
      Blank_When_Zero : in Boolean := False)
      return            Picture;

   function Pic_String      (Pic : in Picture) return String;
   function Blank_When_Zero (Pic : in Picture) return Boolean;

   Max_Picture_Length : constant := 64;

   Picture_Error : exception;

   Default_Currency   : constant String    := "$";
   Default_Fill       : constant Character := ' ';
   Default_Separator  : constant Character := ',';
   Default_Radix_Mark : constant Character := '.';

   generic
      type Num is delta <> digits <>;
      Default_Currency   : in String := Editing.Default_Currency;
      Default_Fill       : in Character := Editing.Default_Fill;
      Default_Separator  : in Character := Editing.Default_Separator;
      Default_Radix_Mark : in Character := Editing.Default_Radix_Mark;

   package Decimal_Output is

      function Length
        (Pic      : in Picture;
         Currency : in String := Default_Currency)
         return     Natural;

      function Valid
        (Item     : Num;
         Pic      : in Picture;
         Currency : in String := Default_Currency)
         return     Boolean;

      function Image
        (Item       : Num;
         Pic        : in Picture;
         Currency   : in String    := Default_Currency;
         Fill       : in Character := Default_Fill;
         Separator  : in Character := Default_Separator;
         Radix_Mark : in Character := Default_Radix_Mark)
         return       String;

      procedure Put
        (File       : in Ada.Text_IO.File_Type;
         Item       : Num;
         Pic        : in Picture;
         Currency   : in String    := Default_Currency;
         Fill       : in Character := Default_Fill;
         Separator  : in Character := Default_Separator;
         Radix_Mark : in Character := Default_Radix_Mark);

      procedure Put
        (Item       : Num;
         Pic        : in Picture;
         Currency   : in String    := Default_Currency;
         Fill       : in Character := Default_Fill;
         Separator  : in Character := Default_Separator;
         Radix_Mark : in Character := Default_Radix_Mark);

      procedure Put
        (To         : out String;
         Item       : Num;
         Pic        : in Picture;
         Currency   : in String    := Default_Currency;
         Fill       : in Character := Default_Fill;
         Separator  : in Character := Default_Separator;
         Radix_Mark : in Character := Default_Radix_Mark);

   end Decimal_Output;

private

   MAX_PICSIZE      : constant := 50;
   MAX_MONEYSIZE    : constant := 10;
   Invalid_Position : constant := -1;

   subtype Pic_Index is Natural range 0 .. MAX_PICSIZE;

   type Picture_Record (Length : Pic_Index := 0) is record
      Expanded : String (1 .. Length);
   end record;

   type Format_Record is record
      Picture              : Picture_Record;
      --  Read only

      Blank_When_Zero      : Boolean;
      --  Read/write

      Original_BWZ         : Boolean;

      --  The following components get written

      Star_Fill            : Boolean := False;

      Radix_Position       : Integer := Invalid_Position;

      Sign_Position,
      Second_Sign          : Integer := Invalid_Position;

      Start_Float,
      End_Float            : Integer := Invalid_Position;

      Start_Currency,
      End_Currency         : Integer := Invalid_Position;

      Max_Leading_Digits   : Integer := 0;

      Max_Trailing_Digits  : Integer := 0;

      Max_Currency_Digits  : Integer := 0;

      Floater              : Character := '!';
      --  Initialized to illegal value

   end record;

   type Picture is record
      Contents : Format_Record;
   end record;

   type Number_Attributes is record
      Negative     : Boolean := False;

      Has_Fraction : Boolean := False;

      Start_Of_Int,
      End_Of_Int,
      Start_Of_Fraction,
      End_Of_Fraction : Integer := Invalid_Position;    -- invalid value
   end record;

   function Parse_Number_String (Str : String) return Number_Attributes;
   --  Assumed format is 'IMAGE or Fixed_IO.Put format (depends on no
   --  trailing blanks...)

   procedure Precalculate (Pic : in out Format_Record);
   --  Precalculates fields from the user supplied data

   function Format_Number
     (Pic                 : Format_Record;
      Number              : String;
      Currency_Symbol     : String;
      Fill_Character      : Character;
      Separator_Character : Character;
      Radix_Point         : Character)
      return                String;
   --  Formats number according to Pic

   function Expand (Picture : in String) return String;

end Ada.Text_IO.Editing;
