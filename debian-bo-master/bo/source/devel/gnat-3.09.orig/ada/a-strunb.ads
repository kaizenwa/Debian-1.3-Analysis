------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                A D A . S T R I N G S . U N B O U N D E D                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.14 $                              --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Maps;
with Ada.Finalization;
with Ada.Streams;

package Ada.Strings.Unbounded is
pragma Preelaborate (Unbounded);

   type Unbounded_String is private;

   Null_Unbounded_String : constant Unbounded_String;

   function Length (Source : Unbounded_String) return Natural;

   type String_Access is access all String;

   procedure Free (X : in out String_Access);

   --------------------------------------------------------
   -- Conversion, Concatenation, and Selection Functions --
   --------------------------------------------------------

   function To_Unbounded_String (Source : String)     return Unbounded_String;
   function To_Unbounded_String (Length : in Natural) return Unbounded_String;

   function To_String (Source : Unbounded_String) return String;

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : in Unbounded_String);

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : in String);

   procedure Append
     (Source   : in out Unbounded_String;
      New_Item : in Character);

   function "&" (Left, Right : Unbounded_String) return Unbounded_String;

   function "&"
     (Left  : in Unbounded_String;
      Right : in String)
      return  Unbounded_String;

   function "&"
     (Left  : in String;
      Right : in Unbounded_String)
      return  Unbounded_String;

   function "&"
     (Left  : in Unbounded_String;
      Right : in Character)
      return  Unbounded_String;

   function "&"
     (Left  : in Character;
      Right : in Unbounded_String)
      return  Unbounded_String;

   function Element
     (Source : in Unbounded_String;
      Index  : in Positive)
      return   Character;

   procedure Replace_Element
     (Source : in out Unbounded_String;
      Index  : in Positive;
      By     : Character);

   function Slice
     (Source : in Unbounded_String;
      Low    : in Positive;
      High   : in Natural)
      return   String;

   function "=" (Left, Right : in Unbounded_String) return Boolean;

   function "="
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function "="
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   function "<" (Left, Right : in Unbounded_String) return Boolean;

   function "<"
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function "<"
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   function "<=" (Left, Right : in Unbounded_String) return Boolean;

   function "<="
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function "<="
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   function ">" (Left, Right : in Unbounded_String) return Boolean;

   function ">"
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function ">"
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   function ">=" (Left, Right : in Unbounded_String) return Boolean;

   function ">="
     (Left  : in Unbounded_String;
      Right : in String)
      return  Boolean;

   function ">="
     (Left  : in String;
      Right : in Unbounded_String)
      return  Boolean;

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source   : in Unbounded_String;
      Pattern  : in String;
      Going    : in Direction := Forward;
      Mapping  : in Maps.Character_Mapping := Maps.Identity)
      return     Natural;

   function Index
     (Source   : in Unbounded_String;
      Pattern  : in String;
      Going    : in Direction := Forward;
      Mapping  : in Maps.Character_Mapping_Function)
      return     Natural;

   function Index
     (Source : in Unbounded_String;
      Set    : in Maps.Character_Set;
      Test   : in Membership := Inside;
      Going  : in Direction  := Forward)
      return   Natural;

   function Index_Non_Blank
     (Source : in Unbounded_String;
      Going  : in Direction := Forward)
      return   Natural;

   function Count
     (Source  : in Unbounded_String;
      Pattern : in String;
      Mapping : in Maps.Character_Mapping := Maps.Identity)
      return    Natural;

   function Count
     (Source   : in Unbounded_String;
      Pattern  : in String;
      Mapping  : in Maps.Character_Mapping_Function)
      return     Natural;

   function Count
     (Source : in Unbounded_String;
      Set    : in Maps.Character_Set)
      return   Natural;

   procedure Find_Token
     (Source : in Unbounded_String;
      Set    : in Maps.Character_Set;
      Test   : in Membership;
      First  : out Positive;
      Last   : out Natural);

   ------------------------------------
   -- String Translation Subprograms --
   ------------------------------------

   function Translate
     (Source  : in Unbounded_String;
      Mapping : in Maps.Character_Mapping)
      return    Unbounded_String;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : Maps.Character_Mapping);

   function Translate
     (Source  : in Unbounded_String;
      Mapping : in Maps.Character_Mapping_Function)
      return    Unbounded_String;

   procedure Translate
     (Source  : in out Unbounded_String;
      Mapping : in Maps.Character_Mapping_Function);

   ---------------------------------------
   -- String Transformation Subprograms --
   ---------------------------------------

   function Replace_Slice
     (Source : in Unbounded_String;
      Low    : in Positive;
      High   : in Natural;
      By     : in String)
      return   Unbounded_String;

   procedure Replace_Slice
     (Source   : in out Unbounded_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in String);

   function Insert
     (Source   : in Unbounded_String;
      Before   : in Positive;
      New_Item : in String)
      return     Unbounded_String;

   procedure Insert
     (Source   : in out Unbounded_String;
      Before   : in Positive;
      New_Item : in String);

   function Overwrite
     (Source   : in Unbounded_String;
      Position : in Positive;
      New_Item : in String)
      return     Unbounded_String;

   procedure Overwrite
     (Source    : in out Unbounded_String;
      Position  : in Positive;
      New_Item  : in String);

   function Delete
     (Source  : in Unbounded_String;
      From    : in Positive;
      Through : in Natural)
      return    Unbounded_String;

   procedure Delete
     (Source  : in out Unbounded_String;
      From    : in Positive;
      Through : in Natural);

   function Trim
     (Source : in Unbounded_String;
      Side   : in Trim_End)
      return   Unbounded_String;

   procedure Trim
     (Source : in out Unbounded_String;
      Side   : in Trim_End);

   function Trim
     (Source : in Unbounded_String;
      Left   : in Maps.Character_Set;
      Right  : in Maps.Character_Set)
      return   Unbounded_String;

   procedure Trim
     (Source : in out Unbounded_String;
      Left   : in Maps.Character_Set;
      Right  : in Maps.Character_Set);

   function Head
     (Source : in Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space)
      return   Unbounded_String;

   procedure Head
     (Source : in out Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space);

   function Tail
     (Source : in Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space)
      return   Unbounded_String;

   procedure Tail
     (Source : in out Unbounded_String;
      Count  : in Natural;
      Pad    : in Character := Space);

   function "*"
     (Left  : in Natural;
      Right : in Character)
      return  Unbounded_String;

   function "*"
     (Left  : in Natural;
      Right : in String)
      return  Unbounded_String;

   function "*"
     (Left  : in Natural;
      Right : in Unbounded_String)
      return  Unbounded_String;

private

   --  The subprograms are declared before the full view of the type so
   --  they don't become primitive opertions.

   procedure Unbounded_String_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : Unbounded_String);

   procedure Unbounded_String_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Unbounded_String);

   function Unbounded_String_Input
     (Stream : access Ada.Streams.Root_Stream_Type'Class)
      return   Unbounded_String;

   package AF renames Ada.Finalization;

   type Unbounded_String is new AF.Controlled with record
      Reference : String_Access;
   end record;

   for Unbounded_String'Write  use Unbounded_String_Write;
   for Unbounded_String'Read   use Unbounded_String_Read;
   for Unbounded_String'Output use Unbounded_String_Write;
   for Unbounded_String'Input  use Unbounded_String_Input;

   --  Note: no need for a separate Output procedure, Read has the good
   --  profile and semantic.

   procedure Initialize (Object : in out Unbounded_String);
   procedure Adjust     (Object : in out Unbounded_String);
   procedure Finalize   (Object : in out Unbounded_String);

   Null_Unbounded_String : constant Unbounded_String :=
     (AF.Controlled with Reference => new String'(""));

end Ada.Strings.Unbounded;
