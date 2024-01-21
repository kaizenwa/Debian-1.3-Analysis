------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             X R E F _ T A B                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.116 $                            --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Atree;   use Atree;
with Csets;   use Csets;
with Errout;  use Errout;
with Gnatvsn;
with Lib;     use Lib;
with Namet;   use Namet;
with Osint;   use Osint;
with Sinfo;   use Sinfo;
with Sinput;  use Sinput;
with Stand;   use Stand;

package body Xref_Tab is

   Header_Full : constant String := "%%";
   Header_Stub : constant String := "--";
   --  String indicating if a file in a required unit (A file given in
   --  argument to GNATF) or an auxiliary unit (A file loaded during
   --  compilation)

   Line_Length : constant Natural := 79;
   --  Where does this wierd value of 79 come from ???

   Entity_Indent : constant Integer :=  0;
   --  Defines the indentation of lines containing declarations of entities.

   Reference_Indent : constant Integer :=  1;
   --  Defines the indentation of lines containing the reference list to
   --  an entity

   Indent : Integer;
   --  Indentation of the current line depending on the line : entity
   --  definition or reference list

   Too_Long_Indent : constant Integer :=  1;
   --  These four constants should be transferred into types.ads ???
   --  or possibly user parametrized in some way ???
   --  Also, they should be documented ???
   --  Used for the formatted output of Write_Buffer. Used how???

   Line_Too_Long : Boolean := False;
   --  To signal a previous truncated line.
   --  The indent then changes to be Too_Long_Indent spaces larger.

   Buffer : String (1 .. Line_Length + 1);
   --  The buffer variable used to build formatted output (not NUL terminated)
   --  Do we handle identifiers longer than this buffer properly???

   Buffer_Length : Natural := 0;
   --  The current length of Buffer. Points to last character in Buffer

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Char_To_Buffer (The_Char : Character);
   --  Appends the given Char to the end of Buffer. Used for calling
   --  Unix_Write, which accepts text only in form of a Str.

   procedure Add_Nat_To_Buffer (Number : Nat);
   --  Append given integer value to end of buffer

   procedure Add_Str_To_Buffer (Append : String);
   --  Appends the given string to the end of Buffer.

   procedure Add_Tabs_To_Buffer;
   --  Addsd tabulations between the nae of a file and its time stamp.

   function Scope_Path (The_Entity : Entity_Id) return String_Ptr;
   --  Returns the path string of the given entity. A path string consists of
   --  the name of the entity followed by the hierarchical scopes. The scope
   --  entities are separated by a point. However the first separator changes
   --  '#' if the entity is not accessible from outside the unit.

   procedure Write_Entity_Info (The_Entity : Entity_Acc);
   --  Writes all the informations concerning the entity : path,
   --  Place of declaration, Entity kind

   procedure Write_Path (The_Entity : Entity_Acc);
   --  Places the path string of the given entity in Buffer.
   --  A path string consists of the name of the entity followed
   --  by the hierarchical scopes.

   procedure Write_Place_Of_Declaration (The_Entity : Entity_Acc);
   --  Places the declaration string of the given entity in Buffer.
   --  A declaration string consists of the file name in
   --  which the entity is declared followed by the line number.

   procedure Write_Type (Text : String);
   --  Places the Entity_Kind string of a given Entity_Kind'Image in Buffer.
   --  The first two characters get cut ("E_");

   procedure Write_Unit_Info
     (Header_Line  : String;
      Current_Etbl : Entity_Table_Acc);
   --  Writes the information line about the unit : Kind, Unit_Name, Status
   --  and File_Name.

   ------------------------
   -- Add_Char_To_Buffer --
   ------------------------

   procedure Add_Char_To_Buffer (The_Char : Character) is
   begin
      if Buffer_Length = 0 then

         --  Do the correct indention.

         for J in 1 .. Indent loop
            Buffer (J) := ' ';
         end loop;

         Buffer_Length := Indent;

         --  Ignore a leading space.

         if The_Char /= ' ' then
            Buffer (Buffer_Length + 1) := The_Char;
            Buffer_Length := Buffer_Length + 1;
         end if;

      elsif Buffer_Length + 1 > Line_Length then
         if not Line_Too_Long then
            Line_Too_Long := True;
            Indent := Indent + Too_Long_Indent;
         end if;

         Write_Xref_Info (Buffer (1 .. Buffer_Length));
         Buffer_Length := 0;
         Add_Char_To_Buffer (The_Char);

      else
         Buffer (Buffer_Length + 1) := The_Char;
         Buffer_Length := Buffer_Length + 1;

      end if;
   end Add_Char_To_Buffer;

   -----------------------
   -- Add_Nat_To_Buffer --
   -----------------------

   procedure Add_Nat_To_Buffer (Number : Nat) is
      Nat_Str : String (1 .. 10);
      First   : Integer := Nat_Str'Last + 1;
      Num     : Nat := Number;
   begin
      while Num >= 10 loop
         First := First - 1;
         Nat_Str (First) := Character'Val ((Num mod 10) + 48);
         Num := Num / 10;
      end loop;

      First := First - 1;
      Nat_Str (First) := Character'Val ((Num mod 10) + 48);

      Add_Str_To_Buffer (Nat_Str (First .. Nat_Str'Last));

   end Add_Nat_To_Buffer;

   -----------------------
   -- Add_Str_To_Buffer --
   -----------------------

   procedure Add_Str_To_Buffer (Append : String) is
   begin
      if Buffer_Length = 0 then

         --  Do the correct indention.

         for J in 1 .. Indent loop
            Buffer (J) := ' ';
         end loop;

         Buffer_Length := Indent;
      end if;

      if Buffer_Length + Append'Length <= Line_Length then

         --  All OK: no new line!

         Buffer (Buffer_Length + 1 .. Buffer_Length + Append'Length) :=
            Append (Append'First .. Append'Last);
         Buffer_Length := Buffer_Length + Append'Length;

      elsif Append'Length > Line_Length - Indent - Too_Long_Indent then

         --  New line and truncation of the string.

         if Buffer_Length > Indent then
            Write_Xref_Info (Buffer (1 .. Buffer_Length));
            Buffer_Length := 0;
         end if;

         Add_Str_To_Buffer (Append
           (Append'First .. Append'First + Line_Length - Indent - 1));
         Write_Xref_Info (Buffer (1 .. Buffer_Length));
         Buffer_Length := 0;

         if not Line_Too_Long then
            Line_Too_Long := True;
            Indent := Indent + Too_Long_Indent;
            Add_Str_To_Buffer (Append
              (Append'First + Line_Length - Indent + 1 .. Append'Last));
         else
            Add_Str_To_Buffer (Append
              (Append'First + Line_Length - Indent .. Append'Last));
         end if;

      else
         --  Only new line!

         if not Line_Too_Long then
            Line_Too_Long := True;
            Indent := Indent + Too_Long_Indent;
         end if;

         Write_Xref_Info (Buffer (1 .. Buffer_Length));
         Buffer_Length := 0;
         Add_Str_To_Buffer (Append);

      end if;
   end Add_Str_To_Buffer;

   ------------------------
   -- Add_Tabs_To_Buffer --
   ------------------------

   procedure Add_Tabs_To_Buffer is
      Tabs_Col : constant := 25;
      Next_Tab : Natural := Buffer_Length + 1;

   begin
      if Next_Tab > Tabs_Col then
         Add_Char_To_Buffer (' ');
      else
         loop
            Next_Tab := 8 * ((Next_Tab - 1) / 8) + 8 + 1;
            exit when Next_Tab > Tabs_Col;
            Add_Char_To_Buffer (Ascii.HT);
         end loop;

         while Next_Tab < Tabs_Col loop
            Add_Char_To_Buffer (' ');
         end loop;
      end if;

   end Add_Tabs_To_Buffer;

   procedure Write_Entity_Info (The_Entity : Entity_Acc) is

   begin
      Indent := Entity_Indent;
      Write_Path (The_Entity);

      if Entity_Info_In_Xref then
         Write_Type (Entity_Kind'Image (The_Entity.Entity_Type));
      end if;

      Write_Place_Of_Declaration (The_Entity);
   end Write_Entity_Info;

   ----------------
   -- Write_Path --
   ----------------

   procedure Write_Path (The_Entity : Entity_Acc) is
   begin
      Add_Str_To_Buffer (The_Entity.Scope_Path.all);
   end Write_Path;

   --------------------------------
   -- Write_Place_Of_Declaration --
   --------------------------------

   procedure Write_Place_Of_Declaration (The_Entity : Entity_Acc) is
   begin
      Add_Char_To_Buffer (' ');
      Add_Nat_To_Buffer (Int (The_Entity.Line_Number));
      Add_Char_To_Buffer (':');
      Add_Nat_To_Buffer (Int (The_Entity.Col_Number));
      Add_Char_To_Buffer (' ');

      if The_Entity.Real_Line /= No_Line_Number
        and then The_Entity.Real_Col /= No_Column_Number
      then
         Add_Nat_To_Buffer (Int (The_Entity.Real_Line));
         Add_Char_To_Buffer (':');
         Add_Nat_To_Buffer (Int (The_Entity.Real_Col));
         Add_Char_To_Buffer (' ');
      end if;
   end Write_Place_Of_Declaration;

   ----------------
   -- Write_Type --
   ----------------

   procedure Write_Type (Text : String) is
      LC_Text : String := Text (Text'First + 2 .. Text'Last);
   begin
      Add_Char_To_Buffer (' ');

      for J in LC_Text'Range loop
         LC_Text (J) :=  Fold_Lower (LC_Text (J));
      end loop;

      Add_Str_To_Buffer (LC_Text);

   end Write_Type;


   ---------------------
   -- Write_Unit_Info --
   ---------------------

   procedure Write_Unit_Info
     (Header_Line  : String;
      Current_Etbl : Entity_Table_Acc)
   is
   begin

      Indent := Entity_Indent;
      Write_Xref_Info (Buffer (1 .. Buffer_Length));
      Buffer_Length := 0;
      Add_Str_To_Buffer (Header_Line);
      Add_Char_To_Buffer (' ');
      Add_Str_To_Buffer (Current_Etbl.File_Name.all);

      --  We write the time stamp of the file only if we ghenerate a single
      --  xref file. In the other cases, file names and time stamps are written
      --  at the beginning of the file

      if Global_Xref_File then
         Add_Tabs_To_Buffer;
         Add_Str_To_Buffer (String (Current_Etbl.Time_Stamp));
         Add_Char_To_Buffer (' ');
         Add_Str_To_Buffer (Header_Line);
      end if;
      Write_Xref_Info (Buffer (1 .. Buffer_Length));
      Buffer_Length := 0;

   end Write_Unit_Info;

   ----------------
   -- Add_Entity --
   ----------------

   procedure Add_Entity
     (To_Etbl     : in     Entity_Table_Acc;
      Entity_Node : in     Entity_Id;
      New_Entity  : in out Entity_Acc)
   is
      The_Kind     : constant Entity_Kind := Ekind (Entity_Node);
      Parent_Node  : constant Node_Id     := Parent (Entity_Node);
      Spec_Node    : Node_Id;

      Grand_Parent : Node_Id;
      --  Bad name, as it is not always the grand parent, try to describe
      --  it abstractly (i.e. what is it used for) rather than just saying
      --  what it is ???

   begin
      Namet.Get_Name_String (Chars (Entity_Node));

      New_Entity := new An_Entity;
      --  new An_Entity reads very awkwardly, find a better name
      --  for An_Entity ???

      New_Entity.Chars       := new String'(Name_Buffer (1 .. Name_Len));

      New_Entity.Entity_Node := Entity_Node;
      New_Entity.Entity_Type := The_Kind;
      New_Entity.Entity_Char := Chars (Entity_Node);
      New_Entity.Entity_Sloc := Sloc (Entity_Node);

      if Include_Inlined
        and then not All_Info_In_Xref
        and then not To_Etbl.Has_Inlined
        and then To_Etbl.Status in Spec_Status
      then
         if ((The_Kind = E_Procedure or else The_Kind = E_Function)
               and then Is_Inlined (Entity_Node))
           or else Nkind (Parent (Parent_Node)) in N_Generic_Declaration
           or else Nkind (Parent_Node) in N_Generic_Renaming_Declaration
         then
            To_Etbl.Has_Inlined := True;
         end if;
      end if;

      New_Entity.Line_Number := Get_Line_Number (Sloc (Entity_Node));
      New_Entity.Col_Number  := Get_Column_Number (Sloc (Entity_Node));

      if Nkind (Parent_Node) = N_Private_Type_Declaration
        or else Nkind (Parent_Node) = N_Private_Extension_Declaration
        or else Nkind (Parent_Node) = N_Incomplete_Type_Declaration
        or else (Nkind (Parent_Node) = N_Object_Declaration
                  and then Constant_Present (Parent_Node)
                  and then No (Expression (Parent_Node)))
      then
         declare
            Full_View_Node : Entity_Id := Full_View (Entity_Node);

         begin
            if Full_View_Node /= Empty then
               New_Entity.Real_Line :=
                 Get_Line_Number (Sloc (Full_View_Node));
               New_Entity.Real_Col  :=
                 Get_Column_Number (Sloc (Full_View_Node));
            end if;
         end;
      end if;

      New_Entity.Scope_Path := Scope_Path (Entity_Node);


      if Scope (Entity_Node) < Last_Standard_Node_Id
        or else Nkind (Parent (Scope (Entity_Node))) = N_Package_Specification
      then
         New_Entity.Is_Direct := True;
      end if;

      if (To_Etbl.Length = 0) then
         To_Etbl.First_Entity := New_Entity;
         To_Etbl.Last_Entity  := New_Entity;
      else
         To_Etbl.Last_Entity.Next_Entity := New_Entity;
         To_Etbl.Last_Entity             := New_Entity;
      end if;

      To_Etbl.Length := To_Etbl.Length + 1;

      --  We give no warnings if certain nodes have no references:
      --
      --  1. an enumeration literal
      --  2. a record component
      --  3. a package name in a package body or body stub
      --  4. a subprogram name or its parameters in a subprogram body
      --     or body stub which does not act as a spec.

      --  We do this because certain hidden references (e.g. within a
      --  range construct or an aggregate) don't appear in our reference
      --  list or because an identifier always points to the subprogram
      --  name in the in the spec.

      if (The_Kind = E_Void
          and then Nkind (Parent_Node) not in N_Generic_Instantiation)
        or else The_Kind = E_Enumeration_Literal
        or else Nkind (Parent_Node) = N_Component_Declaration
        or else Nkind (Parent_Node) = N_Loop_Parameter_Specification
        or else Nkind (Parent_Node) in N_Package_Body .. N_Task_Body
        or else Nkind (Parent_Node) in N_Body_Stub
        or else (The_Kind in Subprogram_Kind
                  and then Nkind (Parent (Parent_Node)) = N_Subprogram_Body
                  and then Corresponding_Spec (Parent (Parent_Node)) /= Empty)
      then
         New_Entity.Give_Warning := False;
      end if;

      if The_Kind in Formal_Kind then

         --  We don't give warnings for parameters of Access Subprograms
         --  or parameters of accept statement
         --  But we guve warnings for Entry declaration

         Grand_Parent := Parent (Parent_Node);

         if Nkind (Parent (Grand_Parent)) = N_Subprogram_Declaration
           or else Nkind (Grand_Parent) = N_Entry_Declaration
           or else Nkind (Parent (Grand_Parent)) =
                     N_Generic_Subprogram_Declaration
         then
            null;

         elsif (Nkind (Parent (Grand_Parent)) = N_Subprogram_Body
                  and then
                Corresponding_Spec (Parent (Grand_Parent)) /= Empty)
           or else Nkind (Parent (Grand_Parent)) = N_Subprogram_Body_Stub
           or else Nkind (Grand_Parent) = N_Access_Function_Definition
           or else Nkind (Grand_Parent) = N_Access_Procedure_Definition
           or else Nkind (Grand_Parent) = N_Accept_Statement
           or else (Nkind (Grand_Parent) = N_Entry_Body_Formal_Part
                    and then Entry_Index_Specification (Grand_Parent) /=
                               Empty)
           or else (Nkind (Grand_Parent) /= N_Entry_Body_Formal_Part
                      and then
                    Is_Overloadable (Defining_Unit_Name (Grand_Parent))
                      and then
                    Is_Imported (Defining_Unit_Name (Grand_Parent)))
         then
            New_Entity.Give_Warning := False;
         end if;

         --  These three lines need to be commented out becasue
         --  they will always raise an Assertion_Failure exception
         --  since Defining_Unit_Name returns a node whose Nkind
         --  is N_Defining_Unit_Name whereas Einfo.Is_Internal
         --  can only be applied to nodes whose Nkind is N_Entity ???

         --  if Einfo.Is_Internal (Defining_Unit_Name
         --    (Grand_Parent)) then
         --     New_Entity.Is_Internal := True;
         --  end if;

      end if;

      --  When we are adding an entity which is defined in a subprogram body
      --  we set the flag Is_Direct to True if this subprogrm is inlined
      --  or a generic defined in the spec.

      if Include_Inlined and then not All_Info_In_Xref
        and then (To_Etbl.Status = A_Body or else To_Etbl.Status = Sub_Body)
      then

         if To_Etbl.Status = A_Body then
            Spec_Node := Library_Unit (To_Etbl.Top_Node);
         else
            Spec_Node := Library_Unit (Library_Unit (To_Etbl.Top_Node));
         end if;

         if Nkind (Unit (Spec_Node)) = N_Generic_Package_Declaration then
            New_Entity.Is_Direct := True;

         elsif (Nkind (Parent (Parent_Node)) = N_Subprogram_Body
            or else Nkind (Parent (Parent_Node)) = N_Subprogram_Declaration)
         then
            --  If the entity defined is a subprogram then we must check if
            --  the definition appears in another subprogram.

            if Nkind (Parent_Node) = N_Function_Specification
              or else Nkind (Parent_Node) = N_Procedure_Specification
            then
               Grand_Parent := Parent (Parent (Parent_Node));
            else
               Grand_Parent := Parent (Parent_Node);
            end if;

            --  If the declaration appears in a subprogram, then we check if
            --  this subprogram is is an inlimed or a generic.

            if Nkind (Grand_Parent) = N_Subprogram_Body
              and then Corresponding_Spec (Grand_Parent) /= Empty
              and then
                (Nkind (Parent (Parent (Corresponding_Spec (Grand_Parent)))) =
                                     N_Generic_Subprogram_Declaration
                  or else Is_Inlined (Corresponding_Spec (Grand_Parent)))

            then
               --  We check that the inlined or generic subprogram appears
               --  in a spec. We don't mind of inlined or generic subprograms
               --  which are defined in bodies for example which aren't
               --  exported.

               declare
                  Spec_Node : Node_Id := Corresponding_Spec (Grand_Parent);

               begin
                  if Nkind (Parent (Parent (Parent (Spec_Node)))) =
                                              N_Package_Specification
                  then
                     New_Entity.Is_Direct := True;
                  end if;
               end;
            end if;
         end if;
      end if;

   end Add_Entity;

   --------------
   -- Add_Etbl --
   --------------

   procedure Add_Etbl
     (First_Etbl  : in out Entity_Table_Acc;
      Last_Etbl   : in out Entity_Table_Acc;
      Unit_Number : in     Unit_Number_Type;
      New_Etbl    : in out Entity_Table_Acc)
   is
      Unit_Node    : Node_Id := Unit (Cunit (Unit_Number));

      Etbl_Tmp     : Entity_Table_Acc := First_Etbl;
      --  To store the current entity table within the search loop.

      Found        : Boolean := False;

      Current_File : File_Name_Type :=
                       Full_File_Name (Source_Index (Unit_Number));

   begin
      --  We look if the entity table is already in the list.

      Namet.Get_Name_String (Current_File);

      while not Found loop

         if (Etbl_Tmp = null) then

            --  In this case we add the entity table to our list.

            New_Etbl := new Entity_Table;
            New_Etbl.Unit_Number := Unit_Number;
            New_Etbl.Next_Etbl := null;
            New_Etbl.File_Name := new String'(Name_Buffer (1 .. Name_Len));
            Namet.Get_Name_String (Unit_Name (Unit_Number));
            New_Etbl.Unit_Name := new String'(Name_Buffer (1 .. Name_Len - 2));
            New_Etbl.Time_Stamp := Source_File_Stamp (Current_File);

            if Name_Buffer (Name_Len) = 's' then
               New_Etbl.Status := A_Spec;
            elsif Acts_As_Spec (Cunit (Unit_Number)) then
               New_Etbl.Status := Body_As_Spec;
            else
               New_Etbl.Status := A_Body;
            end if;

            case Nkind (Unit_Node) is
               when N_Subprogram_Declaration |  N_Subprogram_Body |
                    N_Subprogram_Body_Stub =>
                  Unit_Node := Specification (Unit_Node);

                  case Nkind (Unit_Node) is
                     when N_Procedure_Specification =>
                        New_Etbl.Kind := Proc;
                     when N_Function_Specification =>
                        New_Etbl.Kind := Func;
                     when others =>
                        New_Etbl.Kind := Unknown;
                  end case;

               when N_Package_Declaration | N_Package_Instantiation |
                    N_Package_Body        | N_Package_Body_Stub =>
                  New_Etbl.Kind := Pack;

               when N_Generic_Declaration | N_Function_Instantiation |
                    N_Procedure_Instantiation =>
                  New_Etbl.Kind := Genr;

               when N_Task_Body_Stub =>
                  New_Etbl.Kind := Tsk;

               when N_Subunit =>
                  New_Etbl.Kind   := Subunit;
                  New_Etbl.Status := Sub_Body;

               when others =>
                  New_Etbl.Kind := Unknown;

            end case;

            New_Etbl.Top_Node := Cunit (Unit_Number);

            if (First_Etbl = null) then
               First_Etbl := New_Etbl;
               Last_Etbl  := New_Etbl;
            else
               Last_Etbl.Next_Etbl := New_Etbl;
               Last_Etbl           := New_Etbl;
            end if;

            Found := True;

         else
            if (Etbl_Tmp.File_Name.all = Name_Buffer (1 .. Name_Len)) then

               --  In this case we update only the top Node_Id.

               Etbl_Tmp.Top_Node     := Cunit (Unit_Number);
               Etbl_Tmp.Xref_Written := False;
               Etbl_Tmp.Unit_Number  := Unit_Number;
               New_Etbl := Etbl_Tmp;

               Found := True;

            else
               Etbl_Tmp := Etbl_Tmp.Next_Etbl;
            end if;

         end if;
      end loop;

   end Add_Etbl;


   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference
     (To_Entity :  Entity_Acc;
      New_Etbl  :  Entity_Table_Acc;
      New_Ref   :  Node_Id)
   is
      R_Tmp    : Ref_Acc;

      New_Sloc : Source_Ptr := Sloc (New_Ref);

   begin
      if To_Entity /= null then

         R_Tmp := new Ref;
         R_Tmp.Ref_Node    := New_Ref;
         R_Tmp.Sloc        := New_Sloc;

         if Nkind (New_Ref) /= N_Expanded_Name then
            R_Tmp.Line_Number := Get_Line_Number (Sloc (New_Ref));
            R_Tmp.Col_Number  := Get_Column_Number (Sloc (New_Ref));
         else
            R_Tmp.Line_Number :=
              Get_Line_Number (Sloc (Selector_Name (New_Ref)));
            R_Tmp.Col_Number :=
              Get_Column_Number (Sloc (Selector_Name (New_Ref)));
         end if;

         R_Tmp.Etbl        := New_Etbl;

         if To_Entity.First_Ref = null then
            To_Entity.First_Ref := R_Tmp;
            To_Entity.Last_Ref  := R_Tmp;
         else
            To_Entity.Last_Ref.Next_Ref := R_Tmp;
            To_Entity.Last_Ref          := R_Tmp;
         end if;

         if Nkind (Parent (New_Ref)) = N_Pragma_Argument_Association then
            R_Tmp.Is_Pragma := True;

         else
            To_Entity.Length := To_Entity.Length + 1;

            if Nkind (Parent (New_Ref)) = N_With_Clause then
               if Elaborate_Present (Parent (New_Ref)) then
                  R_Tmp.Is_An_Elaborated_With_Clause := True;
               end if;
            end if;
         end if;

      end if;
   end Add_Reference;


   --------------
   -- Add_With --
   --------------

   procedure Add_With
     (To_Etbl  : Entity_Table_Acc;
      New_Etbl : Entity_Table_Acc;
      Is_Implicit : Boolean := False)
   is
      W_Tmp : With_Acc := To_Etbl.First_With;
      --  To store the current values within the search loop.

      Found  : Boolean  := False;

   begin
      if W_Tmp = null then

         --  No With_Clause yet!

         To_Etbl.First_With             := new With_Clause;
         To_Etbl.First_With.Withed_Etbl := New_Etbl;
         To_Etbl.First_With.Is_Implicit := Is_Implicit;

         --  Set the field Prev_Msgs to Done to avoid that
         --  the Xref checks if Implicit withed units are useful or not.
         if Is_Implicit then
            To_Etbl.First_With.Prev_Msgs := Done;
         end if;
      else
         --  Look for New_Etbl, if not in the list creat a new With_Clause!

         if W_Tmp.Withed_Etbl = New_Etbl then
            Found := True;
         end if;

         while W_Tmp.Next_With /= null and then not Found loop
            W_Tmp := W_Tmp.Next_With;

            if W_Tmp.Withed_Etbl = New_Etbl then
               Found := True;
            end if;
         end loop;

         if not Found then
            W_Tmp.Next_With             := new With_Clause;
            W_Tmp.Next_With.Withed_Etbl := New_Etbl;
            W_Tmp.Next_With.Is_Implicit := Is_Implicit;

            --  Set the field Prev_Msgs to Done to avoid that
            --  the Xref checks if Implicit withed units are useful or not.
            if Is_Implicit then
               W_Tmp.Next_With.Prev_Msgs := Done;
            end if;

         end if;

      end if;
   end Add_With;

   --------------------------
   -- Clear_And_Mark_Xrefs --
   --------------------------

   procedure Clear_And_Mark_Xrefs
     (Home_Etbl   : Entity_Table_Acc;
      Target_Etbl : Entity_Table_Acc;
      First_Pass  : Boolean;
      In_Xref     : Boolean;
      Count_Marks : Boolean := True)
    is
      E_Tmp                : Entity_Acc := Home_Etbl.First_Entity;
      R_Tmp                : Ref_Acc;
      Old_Marks            : Natural;
      Is_Used_In_Elaborate : Boolean := False;
      First_Reference      : Boolean := True;
      Write_Unit           : Boolean := True;

      With_Clauses         : With_Acc;

   begin

      if Count_Marks then
         Target_Etbl.Marked := False;
      end if;

      while E_Tmp /= null loop

         if First_Pass and then Count_Marks then
            E_Tmp.Marks := 0;
         end if;

         First_Reference := True;

         Old_Marks := E_Tmp.Marks;
         R_Tmp := E_Tmp.First_Ref;
         Is_Used_In_Elaborate := False;

         while (R_Tmp /= null) loop

            if In_Xref and then not Home_Etbl.Xref_Written
              and then R_Tmp.Etbl = Target_Etbl and then First_Pass then
               if Xref_Flag
                 and then not Global_Xref_File
                 and then
                   ((E_Tmp.Entity_Type not in Formal_Kind
                             and then E_Tmp.Entity_Type /= E_Discriminant)
                     or else All_Info_In_Xref)
               then
                  if Write_Unit then
                     if Home_Etbl.RU then
                        Write_Unit_Info (Header_Full, Home_Etbl);
                     else
                        Write_Unit_Info (Header_Stub, Home_Etbl);
                     end if;

                     Write_Unit := False;
                  end if;


                  if First_Reference then

                     Write_Entity_Info (E_Tmp);

                     Write_Xref_Info (Buffer (1 .. Buffer_Length));
                     Buffer_Length := 0;

                     Indent := Reference_Indent;

                     Add_Char_To_Buffer ('{');
                     First_Reference := False;

                  else
                     Indent := Reference_Indent;

                     Add_Char_To_Buffer (' ');
                  end if;

                  Add_Nat_To_Buffer (Int (R_Tmp.Line_Number));
                  Add_Char_To_Buffer (':');
                  Add_Nat_To_Buffer (Int (R_Tmp.Col_Number));
               end if;
            end if;

            if R_Tmp.Etbl = Target_Etbl
              and then not R_Tmp.Is_Pragma
              and then (First_Pass or else not R_Tmp.Marked)
              and then Count_Marks
            then

               E_Tmp.Marks := E_Tmp.Marks + 1;
               R_Tmp.Marked := True;

               if not Is_Used_In_Elaborate then
                  Is_Used_In_Elaborate := R_Tmp.Is_An_Elaborated_With_Clause;
               end if;

            else
               if First_Pass and then Count_Marks then
                  R_Tmp.Marked := False;
               end if;
            end if;


            R_Tmp := R_Tmp.Next_Ref;
         end loop;

         if Xref_Flag and then not First_Reference then
            Add_Char_To_Buffer ('}');
            Write_Xref_Info (Buffer (1 .. Buffer_Length));
            Buffer_Length := 0;
         end if;

         --  We mark the target entity table to signal that there are
         --  some cross references found.

         --  We don't consider the first entity (always referenced in the
         --  with clause) except if the target entity table is a subprogram,
         --  (in this case we're only able to reference the first entity).

         if E_Tmp.Marks /= Old_Marks
           and then not Target_Etbl.Marked
         then
            if E_Tmp /= Home_Etbl.First_Entity
              or else Home_Etbl.Kind in Proc .. Genr
                  --  Above explicit reference to range is improper ???
                  --  Introduce proper subtype at point of declaration ???

              or else Is_Used_In_Elaborate
            then
               Target_Etbl.Marked := True;
            end if;
         end if;

         E_Tmp := E_Tmp.Next_Entity;
      end loop;

      Home_Etbl.Xref_Written := True;

      --  Special treatment for Text_IO and Wide_Text_IO. Needed
      --  because of the kludge used for nested generic packages.
      --  See Rtsfind.Text_IO_Kludge for details.

      if not Target_Etbl.Marked
        and then Home_Etbl.First_Entity /= null
        and then
          (Home_Etbl.First_Entity.Chars.all = "text_io"
            or else
           Home_Etbl.First_Entity.Chars.all = "wide_text_io")
      then
         With_Clauses := Target_Etbl.First_With;
         while With_Clauses /= null loop
            if With_Clauses.Is_Implicit then
               Target_Etbl.Marked := True;
               exit;
            end if;

            With_Clauses := With_Clauses.Next_With;
         end loop;
      end if;

   end Clear_And_Mark_Xrefs;

   ------------------
   -- Delete_Table --
   ------------------

   procedure Delete_Table (Old_Etbl : Entity_Table_Acc) is
   begin
      null;
   end Delete_Table;

   -----------------
   -- Entity_Node --
   -----------------

   function Entity_Node (The_Entity : Entity_Acc) return Entity_Id is
   begin
      if The_Entity = null then
         return Empty;
      else
         return The_Entity.Entity_Node;
      end if;
   end Entity_Node;

   -----------------
   -- Entity_Type --
   -----------------

   function Entity_Type (The_Entity : Entity_Acc) return Entity_Kind is
   begin
      if The_Entity = null then
         return E_Void;
      else
         return The_Entity.Entity_Type;
      end if;
   end Entity_Type;

   -----------
   -- First --
   -----------

   function First (The_Etbl : Entity_Table_Acc) return Entity_Id is
   begin
      return The_Etbl.First_Entity.Entity_Node;
   end First;

   -----------
   -- First --
   -----------

   function First (The_Entity : Entity_Acc) return Ref_Acc is
   begin
      return The_Entity.First_Ref;
   end First;

   ------------------
   -- Give_Warning --
   ------------------

   function Give_Warning (The_Entity : Entity_Acc) return Boolean is
   begin
      return The_Entity.Give_Warning;
   end Give_Warning;

   ---------------
   -- In_E_List --
   ---------------

   function In_E_List
     (The_Etbl   : Entity_Table_Acc;
      The_Entity : Entity_Id)
      return       Entity_Acc
   is
      E_Tmp : Entity_Acc;
      --  To store the current entity within the search loop.

   begin
      E_Tmp := The_Etbl.First_Entity;

      while E_Tmp /= null
        and then E_Tmp.Entity_Node /= The_Entity
      loop
         E_Tmp := E_Tmp.Next_Entity;
      end loop;

      return E_Tmp;
   end In_E_List;

   -----------------
   -- In_Ref_List --
   -----------------

   function In_Ref_List
     (The_Entity : Entity_Acc;
      The_Ref    : Node_Id)
      return       Boolean
   is
      R_Tmp : Ref_Acc;
      --  To store the current reference within the search loop.

   begin
      if The_Entity = null then
         return False;

      else
         R_Tmp := The_Entity.First_Ref;

         while R_Tmp /= null
           and then R_Tmp.Ref_Node /= The_Ref loop
            R_Tmp := R_Tmp.Next_Ref;
         end loop;

         if R_Tmp = null then
            return False;
         else
            return True;
         end if;

      end if;
   end In_Ref_List;

   ------------------
   -- In_With_List --
   ------------------

   function In_With_List
     (Home_Etbl   : Entity_Table_Acc;
      Target_Etbl : Entity_Table_Acc)
      return        Boolean
   is
      W_Tmp : With_Acc;
      --  To store the current entity within the search loop.

   begin
      W_Tmp := Target_Etbl.First_With;

      while W_Tmp /= null
        and then W_Tmp.Withed_Etbl /= Home_Etbl
      loop
         W_Tmp := W_Tmp.Next_With;
      end loop;

      if W_Tmp = null then
         return False;
      else
         return True;
      end if;

   end In_With_List;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (The_Entity : Entity_Acc) return Boolean is
   begin
      return The_Entity = null;
   end Is_Null;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (The_Ref : Ref_Acc) return Boolean is
   begin
      return The_Ref = null;
   end Is_Null;

   ---------------
   -- Is_Pragma --
   ---------------

   function Is_Pragma (The_Ref : Ref_Acc) return Boolean is
   begin
      return The_Ref.Is_Pragma;
   end Is_Pragma;

   -----------------
   -- Mark_Entity --
   -----------------

   procedure Mark_Entity (Old_Entity : Entity_Acc) is
   begin
      if Old_Entity /= null then
         Old_Entity.Marks := Old_Entity.Marks + 1;
      end if;
   end Mark_Entity;

   --------------------------
   -- Mark_Withed_Entities --
   --------------------------

   procedure Mark_Withed_Entities (The_Etbl : Entity_Table_Acc) is
      R : Ref_Acc := The_Etbl.First_Entity.First_Ref;

      Current_Etbl  : Entity_Table_Acc;
      Previous_Etbl : Entity_Table_Acc;

      First : Boolean := True;
      --  To suppress multiple calls of Mark_Xrefs for the same client.

   begin
      --  We loop through all the references of the unit name entity.
      --  Each client must have at least one such reference in
      --  this list (the one of the with clause).

      while (R /= null) loop

         Current_Etbl := R.Etbl;

         if Current_Etbl /= Previous_Etbl
           and then Current_Etbl /= The_Etbl
           and then Current_Etbl.RU
         then
            --  If we find a cross reference of a new entity table then
            --  we mark the referenced entities.

            Clear_And_Mark_Xrefs (The_Etbl, Current_Etbl, First, False);
            First := False;
            Previous_Etbl := Current_Etbl;
         end if;

         R := R.Next_Ref;
      end loop;

   end Mark_Withed_Entities;

   ----------
   -- Next --
   ----------

   function Next (The_Entity : Entity_Acc) return Entity_Acc is
   begin
      if The_Entity = null then
         return null;
      else
         return The_Entity.Next_Entity;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   function Next (The_Ref : Ref_Acc) return Ref_Acc is
   begin
      if The_Ref = null then
         return null;
      else
         return The_Ref.Next_Ref;
      end if;
   end Next;

   ---------------------
   -- Number_Of_Marks --
   ---------------------

   function Number_Of_Marks (The_Entity : Entity_Acc) return Natural is
   begin
      if The_Entity = null then
         return 0;
      else
         return The_Entity.Marks;
      end if;
   end Number_Of_Marks;

   --------------------
   -- Number_Of_Refs --
   --------------------

   function Number_Of_Refs (The_Entity : Entity_Acc) return Natural is
   begin
      if The_Entity = null then
         return 0;
      else
         return The_Entity.Length;
      end if;
   end Number_Of_Refs;

   ----------------
   -- Scope_Path --
   ----------------

   function Scope_Path (The_Entity : Entity_Id) return String_Ptr is
      Scope_Node : Node_Id;

      Max_Buffer_Length : constant Natural := 100;
      --  The length of Buffer is limited to 100 characters.

      Buffer : String (1 .. Max_Buffer_Length);
      --  The buffer variable to enable formatted output.
      --  The string in Buffer is *not* NUL terminated!
      --
      --  Note: We fill the Buffer from the right to the left.
      --        So we can do with iteration instead of recursion.

      Buffer_Entry : Positive := Max_Buffer_Length;
      --  The current entry into Buffer (points to the last empty field).

      Loop_String  : constant String (1 .. 4) := "loop";
      Block_String : constant String (1 .. 5) := "block";

      procedure Insert_Char_In_Buffer (The_Char : Character);
      --  Insert one character in buffer if it fits, otherwise ignore call

      procedure Insert_Str_In_Buffer (Insert : String);
      --  Insert given string in buffer (does nothing if string is too long)

      procedure Insert_Char_In_Buffer (The_Char : Character) is
      begin
         if Buffer_Entry = 0 then
            null;
         else
            Buffer (Buffer_Entry) := The_Char;
            Buffer_Entry := Buffer_Entry - 1;
         end if;
      end Insert_Char_In_Buffer;

      procedure Insert_Str_In_Buffer (Insert : String) is
      begin
         if Insert'Length > Buffer_Entry then
            null;
         else
            Buffer (Buffer_Entry - Insert'Length + 1 .. Buffer_Entry) :=
              Insert (1 .. Insert'Length);
            Buffer_Entry := Buffer_Entry - Insert'Length;
         end if;
      end Insert_Str_In_Buffer;

   --  Start of processing for Scope_Path

   begin
      --  Insert_Char_In_Buffer ('/');

      --  If the entity is visible from outside we add a # to its scope.
      --  Thus we can easily distinguish between entities declared within
      --  the body and those declared within the spec.

      Scope_Node := The_Entity;
      Namet.Get_Decoded_Name_String (Chars (Scope_Node));
      Insert_Str_In_Buffer (Name_Buffer (1 .. Name_Len));
      Scope_Node := Scope (Scope_Node);

      if Scope_Node < Last_Standard_Node_Id then
         return new String'(Buffer (Buffer_Entry + 1 .. Max_Buffer_Length));
      end if;

      --  We stop adding scopes if we find a scope which is declared within
      --  the Standard package.

      while Scope (Scope_Node) > Last_Standard_Node_Id loop

         Insert_Char_In_Buffer ('.');

         Namet.Get_Name_String (Chars (Scope_Node));

         --  Following code knows far too much about encoding of names ???
         --  Probably so, since names are no longer of the form listed below???

         if Ekind (Scope_Node) = E_Loop
           and then not Comes_From_Source (Scope_Node)
         then
            --  Given 'loop__456' we suppress the '__456'.
            --  This is very suspicious, Xref knows too much here ???

            Insert_Str_In_Buffer (Loop_String);

         elsif Ekind (Scope_Node) = E_Block
           and then not Comes_From_Source (Scope_Node)
         then
            --  Given 'block__1002' we suppress the '__1002'.
            --  This is very suspicious, Xref knows too much here ???

            Insert_Str_In_Buffer (Block_String);

         else
            Insert_Str_In_Buffer (Name_Buffer (1 .. Name_Len));
         end if;

         Scope_Node := Scope (Scope_Node);
      end loop;

      return new String'(Buffer (Buffer_Entry + 1 .. Max_Buffer_Length));

   end Scope_Path;

   --------------
   -- The_Node --
   --------------

   function The_Node (The_Ref : Ref_Acc) return Node_Id is
   begin
      return The_Ref.Ref_Node;
   end The_Node;

   --------------------
   -- Unmark_Entity  --
   --------------------

   procedure Unmark_Entity
     (The_Table  : Entity_Table_Acc;
      Old_Entity : Entity_Acc)
   is
   begin
      if The_Table /= null
        and then Old_Entity /= null
      then
         Old_Entity.Marks := 0;
      end if;
   end Unmark_Entity;

   ----------------------
   -- Unmark_Reference --
   ----------------------

   procedure Unmark_Reference
     (The_Entity : Entity_Acc;
      Old_Ref    : Node_Id)
   is
      R_Tmp : Ref_Acc;
      --  To store the current references within the search loop.

   begin
      if The_Entity /= null then

         --  First we search the fitting reference,

         R_Tmp := The_Entity.First_Ref;

         while R_Tmp /= null
           and then R_Tmp.Ref_Node /= Old_Ref
         loop
            R_Tmp := R_Tmp.Next_Ref;
         end loop;

         --  Then we unmark it.

         if R_Tmp /= null and then R_Tmp.Marked = True then
            R_Tmp.Marked := False;
            The_Entity.Marks  := The_Entity.Marks - 1;
         end if;

      end if;
   end Unmark_Reference;

   -------------------
   -- Update_Entity --
   -------------------

   procedure Update_Entity
     (To_Etbl     : in     Entity_Table_Acc;
      Entity_Node : in     Entity_Id;
      New_Entity  : in out Entity_Acc)
   is
      New_Sloc : Source_Ptr;
      Found    : Boolean := False;
      Path_Ptr : String_Ptr;

   begin
      New_Entity := To_Etbl.First_Entity;
      New_Sloc   := Sloc (Entity_Node);


      --  In the case of generics we have to compare the whole
      --  path string since we have lots of entities with same
      --  line numbers and chars.

      if To_Etbl.Kind = Genr then
         Path_Ptr := Scope_Path (Entity_Node);

      --  Otherwise it's enough to compare the line numbers and chars.

      else
         Namet.Get_Name_String (Chars (Entity_Node));

      end if;

      --  First we look if the entity is already in the list.

      while not Found loop
         if New_Entity = null then

            Add_Entity (To_Etbl, Entity_Node, New_Entity);
            Found := True;

         elsif New_Entity.Entity_Sloc = New_Sloc then

            if To_Etbl.Kind = Genr then

               if New_Entity.Scope_Path.all = Path_Ptr.all then
                  New_Entity.Entity_Node := Entity_Node;
                  Found := True;
               else
                  New_Entity := New_Entity.Next_Entity;
               end if;

            else
               if New_Entity.Chars.all = Name_Buffer (1 .. Name_Len) then
                  New_Entity.Entity_Node := Entity_Node;
                  Found := True;
               else
                  New_Entity := New_Entity.Next_Entity;
               end if;

            end if;

         else
            New_Entity := New_Entity.Next_Entity;

         end if;
      end loop;

   end Update_Entity;

   ----------------------
   -- Update_Reference --
   ----------------------

   procedure Update_Reference
     (To_Entity :  Entity_Acc;
      New_Etbl  :  Entity_Table_Acc;
      New_Ref   :  Node_Id)
   is
      R_Tmp : Ref_Acc;
      --  To store the current values within the search loop.

      New_Sloc : Source_Ptr  := Sloc (New_Ref);
      Found    : Boolean     := False;

   begin
      if To_Entity /= null then

         --  We look if the reference is already in the list.

         R_Tmp := To_Entity.First_Ref;

         while not Found loop
            if R_Tmp = null then
               Add_Reference (To_Entity, New_Etbl, New_Ref);
               Found  := True;

            elsif R_Tmp.Etbl = New_Etbl
              and then R_Tmp.Sloc = New_Sloc
            then
               --  In this case we update only the Node_Id.

               R_Tmp.Ref_Node    := New_Ref;
               Found := True;

            else
               R_Tmp := R_Tmp.Next_Ref;
            end if;
         end loop;

      end if;
   end Update_Reference;

   ----------------------
   -- Write_Files_Info --
   ----------------------

   procedure Write_Files_Info (The_Etbl : Entity_Table_Acc) is
      The_Withs     : With_Acc;
      The_Includes  : Include_Acc;
      List_Etbl     : Entity_Table_Acc;
      Parent_Unit   : Unit_Number_Type;
      Unit_Node     : Node_Id;

   begin
      Buffer_Length := 0;

      Indent := Entity_Indent;

      --  Add the name of the file in the buffer

      Add_Str_To_Buffer (The_Etbl.File_Name.all);
      Add_Tabs_To_Buffer;

      --  Then add the time stamp of the file

      Add_Str_To_Buffer (String (The_Etbl.Time_Stamp));
      Add_Char_To_Buffer (' ');

      case The_Etbl.Status is

         when A_Spec | Withed_Spec =>

            --  We check if the Etbl is child library spec.

            Unit_Node := Unit (The_Etbl.Top_Node);

            if Nkind (Unit_Node) /= N_Subprogram_Body
              and then Nkind (Unit_Node) /= N_Package_Body
              and then Present (Parent_Spec (Unit_Node))
            then
               Parent_Unit := Get_Cunit_Unit_Number (Parent_Spec (Unit_Node));
               Add_Nat_To_Buffer (Nat (Parent_Unit) + 1);
               Add_Char_To_Buffer (' ');
            end if;

         when A_Body  =>

            --  The spec of a body must appear in the inclusion graph

            Add_Nat_To_Buffer (Nat (The_Etbl.Predecessor.Unit_Number) + 1);
            Add_Char_To_Buffer (' ');

            --  All the subunits of the body are included.

            List_Etbl := The_Etbl.Successor;
            while List_Etbl /= null loop
               Add_Nat_To_Buffer (Nat (List_Etbl.Unit_Number) + 1);
               Add_Char_To_Buffer (' ');
               List_Etbl := List_Etbl.Successor;
            end loop;

         when Sub_Body =>
            Add_Nat_To_Buffer (Nat (The_Etbl.Predecessor.Unit_Number) + 1);
            Add_Char_To_Buffer (' ');

         when others   =>
            null;

      end case;

      The_Includes := The_Etbl.First_Include;
      while The_Includes /= null loop

         --  In the case we find a generic instantiation, we have added to
         --  the include list the spec where the generic is defined
         --  in the inclusion graph we want to get

         --     - the spec, if it is not withed by the Etbl
         --     - the bodies corresponding to the spec

         List_Etbl := The_Includes.Included_Etbl;

         if not In_With_List (List_Etbl, The_Etbl) then
            Add_Nat_To_Buffer (Nat (List_Etbl.Unit_Number) + 1);
            Add_Char_To_Buffer (' ');
         end if;

         List_Etbl := List_Etbl.Successor;
         while List_Etbl /= null loop
            Add_Nat_To_Buffer (Nat (List_Etbl.Unit_Number) + 1);
            Add_Char_To_Buffer (' ');
            List_Etbl := List_Etbl.Successor;
         end loop;

         The_Includes := The_Includes.Next_Include;
      end loop;

      The_Withs := The_Etbl.First_With;
      while The_Withs /= null loop
         Add_Nat_To_Buffer (Nat (The_Withs.Withed_Etbl.Unit_Number) + 1);
         Add_Char_To_Buffer (' ');
         The_Withs := The_Withs.Next_With;
      end loop;

      Write_Xref_Info (Buffer (1 .. Buffer_Length));
      Buffer_Length := 0;

   end Write_Files_Info;

   -------------------
   -- Write_Version --
   -------------------

   procedure Write_Version is
   begin
      Indent := Entity_Indent;
      Add_Str_To_Buffer ("V ");
      Add_Char_To_Buffer ('"');
      Add_Str_To_Buffer (Gnatvsn.Xref_Version);
      Add_Char_To_Buffer ('"');
      Write_Xref_Info (Buffer (1 .. Buffer_Length));
      Buffer_Length := 0;

   end Write_Version;

   ----------
   -- Writ --
   ----------

   --  This procedure is over 1400 lines long with doubly nested internal
   --  subprograms, it should be simplified and flattened out, with less
   --  use (if necessary) of non-local variables. ???

   procedure Writ
     (The_Etbl   : Entity_Table_Acc;
      Level      : Output_Level;
      First_File : Boolean)
   is
      Warning_String_3  : constant String := "withed but unused";
      Warning_String_5  : constant String := " in ";
      Warning_String_10 : constant String := " should be withed";
      Warning_String_12 : constant String := "already withed in ";

      E_Tmp            : Entity_Acc;
      --  To store the current entity within the search loop.

      Has_Inlined      : Boolean := False;
      --  Flag set in the corresponding spec of a body or a subunit
      --  declares Inlined procedures or functions

      Etbl_Prec        : Entity_Table_Acc;

      Real_Checked     : Entity_Table_Acc;

      The_Withs        : With_Acc;

      First_Refs       : Boolean := True;

      --  The following variables allow to know, if a parent spec of a
      --  withed spec is used in the_etebl and which parent.

      Is_Parent_Used : Boolean;

      Parent_Used : Entity_Table_Acc;

      function Renamed_Etbl
        (Current_Etbl : Entity_Table_Acc)
         return         Entity_Table_Acc;
      --  Returns the entitiy table table corresponding to the unit
      --  renamed in Current_Etbl

      procedure Check_Parents (Withed_Etbl : Entity_Table_Acc);
      --  Checks if parent spec of withed spec have references in The_Etbl

      procedure Check_Withing_Units;
      --  Checks references to The_Etbl in entity tables that with The_Etbl

      procedure Check_Withed_Units;
      --  Check the correct use of the withed units of The_Etbl.

      procedure Write_References (First : Ref_Acc; Only_Marked : Boolean);
      --  Places the reference string of a given entity in Buffer. The
      --  reference string consists of source name files followed by the
      --  line numbers of the references within these files. Only_Marked
      --  is a flag to indicate if we cnsider all the references or only
      --  the marked references.

      procedure Write_Warning (The_Entity : Entity_Acc);
      --  Places a warning message in Buffer and writes a warning message to
      --  standard output if an entity is not used within its program unit.

      type Withed_Warning_Type is
        (Norm,
         Should,
         Already,
         Replace,
         Should_Replace);
      --  This need doumentation for each case

      procedure Write_Withed_Warning
        (Withing_Etbl : Entity_Table_Acc;
         Warning_Kind : Withed_Warning_Type;
         Extra_Etbl   : Entity_Table_Acc);
      --  Writes warnings messages on the standard error.
      --
      --   1.) a withed unit is not used (Norm).
      --         Here the field Extra_Etbl is redundant.
      --
      --   2.) the same with clause appears within a predecessor (Already).
      --        ' ->  already in Extra_Etbl'
      --
      --   3.) a with clause should be moved into a successor (Should).
      --        ' ->  should be in Extra_Etbl'.

      ------------------
      -- Renamed_Etbl --
      ------------------

      function Renamed_Etbl
        (Current_Etbl : Entity_Table_Acc)
         return         Entity_Table_Acc
      is
      begin
         if Current_Etbl.Renamed_Etbl /= null then
            return Current_Etbl.Renamed_Etbl;
         else
            return Current_Etbl;
         end if;
      end Renamed_Etbl;

      -------------------
      -- Check_Parents --
      -------------------
      procedure Check_Parents (Withed_Etbl : Entity_Table_Acc) is
         Parent_Etbl : Entity_Table_Acc;

      begin
         Parent_Etbl := Withed_Etbl.Predecessor;
         while Parent_Etbl /= null loop

            Real_Checked := Renamed_Etbl (Parent_Etbl);
            Clear_And_Mark_Xrefs (Real_Checked, The_Etbl, True, True);

            if The_Etbl.Marked then
               Is_Parent_Used := True;
               if Parent_Used = null then
                  Parent_Used := Parent_Etbl;
               end if;
            end if;

            Parent_Etbl := Parent_Etbl.Predecessor;
         end loop;
      end Check_Parents;

      -------------------------
      -- Check_Withing_Units --
      -------------------------
      procedure Check_Withing_Units is

         First        : Boolean := True;
         Etbl_Tmp     : Entity_Table_Acc;
         Withing_Etbl : Entity_Table_Acc;
         Etbl_Succ    : Entity_Table_Acc;

      begin
         Etbl_Tmp := First_Etbl;

         while Etbl_Tmp /= null loop
            if Etbl_Tmp.Predecessor = null then
               if Etbl_Tmp = The_Etbl then
                  Withing_Etbl := Etbl_Tmp.Successor;
               else
                  Withing_Etbl := Etbl_Tmp;
                  while Withing_Etbl /= null
                    and then not In_With_List (The_Etbl, Withing_Etbl)
                  loop
                     Withing_Etbl := Withing_Etbl.Successor;
                  end loop;
               end if;

               Etbl_Succ := Withing_Etbl;

               while Etbl_Succ /= null loop
                  if Etbl_Succ.RU then

                     Clear_And_Mark_Xrefs
                       (The_Etbl, Etbl_Succ, First, False, True);
                     First := False;
                  end if;
                  Etbl_Succ := Etbl_Succ.Successor;
               end loop;

            end if;
            Etbl_Tmp := Etbl_Tmp.Next_Etbl;
         end loop;
      end Check_Withing_Units;

      ------------------------
      -- Check_Withed_Units --
      ------------------------

      procedure Check_Withed_Units is
         The_Withs : With_Acc := The_Etbl.First_With;
         --  Used to scan all the with of The_Etbl

         Etbl_Prec : Entity_Table_Acc;
         --  Predecessor of The_Etbl

         Etbl_Succ : Entity_Table_Acc;
         --  Successor of The_Etbl

         Real_Checked  : Entity_Table_Acc;

         Succ_Used     : Boolean;
         Previous_Refs : Boolean;
         Higher_Prec   : Entity_Table_Acc;

         function With_Ref
           (Target_Etbl : Entity_Table_Acc;
            Home_Etbl   : Entity_Table_Acc)
            return        With_Acc;
         --  Looks for the With Clause of Home_Etb in Target_Etbl

         procedure Messages_For_Succ
           (The_Message   : Withed_Messages;
            Extra_Etbl    : Entity_Table_Acc);
         --  This procedure scans all the successors of an Entity Table
         --  If a successor with an Unit which is already withed by The_Etbl
         --  then Store a message to print it later

         procedure Messages_For_Child
           (Base_Etbl     : Entity_Table_Acc;
            The_Message   : Withed_Messages;
            Extra_Etbl    : Entity_Table_Acc);
         --  This procedure scans all the childs of an Entity table
         --  if one of these childs withs a unit withed by the Etbl
         --  then we store the appropriate message to print it later

         procedure Store_Messages
           (Withed_Etbl : Entity_Table_Acc;
            The_Message : Withed_Messages;
            Extra_Etbl  : Entity_Table_Acc);
         --  Update the warning messages concerning the with clause
         --  of withed_Etbl in the successors or the child libraries
         --  of the_etbl

         --------------
         -- With_Ref --
         --------------

         function With_Ref
           (Target_Etbl : Entity_Table_Acc;
            Home_Etbl   : Entity_Table_Acc)
            return        With_Acc
         is
            With_List : With_Acc := Target_Etbl.First_With;

         begin
            while With_List /= null loop
               if With_List.Withed_Etbl = Home_Etbl then
                  return With_List;
               end if;

               With_List := With_List.Next_With;
            end loop;

            --  Following code was put in since there was otherwise a
            --  missing return, but it is not clear it is right ???

            pragma Assert (False);
            raise Program_Error;
         end With_Ref;

         -----------------------
         -- Messages_For_Succ --
         -----------------------

         procedure Messages_For_Succ
           (The_Message   : Withed_Messages;
            Extra_Etbl    : Entity_Table_Acc)
         is
            Parent_Etbl : Entity_Table_Acc;

         begin
            --  The_Etbl with The_Withs.Withed_Etbl. For all the successors
            --  of The_Etbl, which with The_Withs.Withed_Etbl or one of
            --  its parent spec, we store The_Message in the corresponding
            --  with structure.

            while Etbl_Succ /= null loop
               if In_With_List (The_Withs.Withed_Etbl, Etbl_Succ) then
                  Store_Messages
                    (The_Withs.Withed_Etbl, The_Message, Extra_Etbl);
               end if;

               Parent_Etbl := The_Withs.Withed_Etbl.Predecessor;
               while Parent_Etbl /= null loop
                  if In_With_List (Parent_Etbl, Etbl_Succ) then
                     Store_Messages
                       (Parent_Etbl, The_Message, Extra_Etbl);
                  end if;
                  Parent_Etbl := Parent_Etbl.Predecessor;
               end loop;

               Etbl_Succ := Etbl_Succ.Successor;
            end loop;
         end Messages_For_Succ;

         ------------------------
         -- Messages_For_Child --
         ------------------------

         procedure Messages_For_Child
           (Base_Etbl   : Entity_Table_Acc;
            The_Message : Withed_Messages;
            Extra_Etbl  : Entity_Table_Acc)
         is
            Childs      : Child_Spec_Acc := Base_Etbl.First_Child;
            --  ??? does childs here mean child spec, or is it the plural
            --  of child, if the latter, the English word is children, if
            --  the former, childspec would be a much more comfortable name
            Parent_Etbl : Entity_Table_Acc;

         begin
            --  The_Etbl with The_Withs.Withed_Etbl. For all the child
            --  libraries of The_Etbl, which with The_Withs.Withed_Etbl
            --  or one of its parent spec, we store The_Message in the
            --  corresponding with structure.

            while Childs /= null loop
               if In_With_List (The_Withs.Withed_Etbl, Childs.Child_Etbl) then
                  Store_Messages
                    (The_Withs.Withed_Etbl, The_Message, Extra_Etbl);
               end if;

               Parent_Etbl := The_Withs.Withed_Etbl.Predecessor;
               while Parent_Etbl /= null loop
                  if In_With_List (Parent_Etbl, Etbl_Succ) then
                     Store_Messages
                       (Parent_Etbl, The_Message, Extra_Etbl);
                  end if;
                  Parent_Etbl := Parent_Etbl.Predecessor;
               end loop;

               Etbl_Succ := Childs.Child_Etbl.Successor;
               Messages_For_Succ (The_Message, Extra_Etbl);

               --  We use a recursive call to visit all the child specs
               --  and not only the child specs of Base_Etbl.

               Messages_For_Child (Childs.Child_Etbl, The_Message, Extra_Etbl);
               Childs := Childs.Next_Child;
            end loop;
         end Messages_For_Child;

         --------------------
         -- Store_Messages --
         --------------------

         procedure Store_Messages
           (Withed_Etbl : Entity_Table_Acc;
            The_Message : Withed_Messages;
            Extra_Etbl  : Entity_Table_Acc)
         is
            Other_With : With_Acc;

         begin

            Other_With := With_Ref (Etbl_Succ, Withed_Etbl);

            if Other_With.Prev_Msgs = None then
               Other_With.Prev_Msgs := The_Message;

               if Extra_Etbl /= null then
                  Other_With.Extra_Etbl := Extra_Etbl;
               end if;
            end if;

         end Store_Messages;

      begin
         --  Scans all the withed units

         while The_Withs /= null loop

            --  Looks for references of the withed unit in The_Etbl

            Real_Checked := Renamed_Etbl (The_Withs.Withed_Etbl);

            --  Check if a message has already been stored for this
            --  with clause

            if The_Withs.Prev_Msgs = Already_Withed then
               Write_Withed_Warning
                 (The_Withs.Withed_Etbl, Already, The_Withs.Extra_Etbl);
            elsif The_Withs.Prev_Msgs = Withed_Unused then
               Write_Withed_Warning (The_Withs.Withed_Etbl, Norm, null);
            elsif The_Withs.Is_Implicit then
               Clear_And_Mark_Xrefs
                 (The_Withs.Withed_Etbl, The_Etbl, True, True);
            elsif The_Withs.Prev_Msgs = None then

               Is_Parent_Used := False;
               Parent_Used := null;
               Clear_And_Mark_Xrefs (Real_Checked, The_Etbl, True, True);
               Succ_Used := The_Etbl.Marked;
               Check_Parents (The_Withs.Withed_Etbl);
               The_Etbl.Marked := Succ_Used;

               --  If the only references found in The_Etbl are references to
               --  entities defined in a parent spec of the withed unit then
               --  The_Etbl mustn't be considered as using the withed unit.

               Previous_Refs := False;
               Etbl_Prec := The_Etbl.Predecessor;

               while Etbl_Prec /= null loop
                  if In_With_List (The_Withs.Withed_Etbl, Etbl_Prec) then
                     Clear_And_Mark_Xrefs
                       (Real_Checked, Etbl_Prec, False, False);

                     if Etbl_Prec.Marked then
                        Previous_Refs := True;
                        Higher_Prec := Etbl_Prec;
                     end if;
                  end if;

                  Etbl_Prec := Etbl_Prec.Predecessor;
               end loop;

               if Previous_Refs or else The_Etbl.Marked then

                  --  If references are found in a predecessors,
                  --  Message : Already withed in the predecessor
                  --  If references are found in The_Etbl,
                  --  Message : Already withed in The_Etbl

                  if Previous_Refs then
                     Write_Withed_Warning
                       (The_Withs.Withed_Etbl, Already, Higher_Prec);
                  else
                     Higher_Prec := The_Etbl;
                  end if;

                  if The_Etbl.Status /= Sub_Body then
                     Etbl_Succ := The_Etbl.Successor;
                     Messages_For_Succ (Already_Withed, Higher_Prec);
                  end if;

                  if The_Etbl.Status in Spec_Status then
                     Messages_For_Child
                       (The_Etbl, Already_Withed, Higher_Prec);
                  end if;

               else
                  case The_Etbl.Status is
                     when Sub_Body =>
                        if not Is_Parent_Used then
                           Write_Withed_Warning
                             (The_Withs.Withed_Etbl, Norm, null);
                        else
                           Write_Withed_Warning
                             (The_Withs.Withed_Etbl, Replace, Parent_Used);
                        end if;

                     when others   =>

                        Succ_Used := False;
                        Etbl_Succ := The_Etbl.Successor;

                        --  We search the closest parent Parent_Used
                        --  of the withed unit which has references
                        --  in a successor of The_Etbl

                        while Etbl_Succ /= null loop

                           Etbl_Prec := The_Withs.Withed_Etbl;

                           while Etbl_Prec /= Parent_Used loop
                              Real_Checked := Renamed_Etbl (Etbl_Prec);
                              Clear_And_Mark_Xrefs
                                (Real_Checked, Etbl_Succ, False, False);

                              if Etbl_Succ.Marked then
                                 Succ_Used := True;
                                 Parent_Used := Etbl_Prec;

                                 --  As soon as we find a reference in a
                                 --  successor to a parent spec, we exit

                                 exit;
                              end if;

                              Etbl_Prec := Etbl_Prec.Predecessor;
                           end loop;

                           --  If the withed unit is not a child spec, as soon
                           --  as we find a reference in a successor, we can
                           --  exit otherwise, we scan all the successors to
                           --  see which parent should replace the withed unit.

                           if The_Withs.Withed_Etbl.Predecessor = null then
                              exit when Succ_Used;
                           end if;

                           Etbl_Succ := Etbl_Succ.Successor;
                        end loop;

                        if The_Etbl.Status in Spec_Status then

                           --  Many cases must be checked :

                           --   1 - The_Etbl uses some entities defined in
                           --   a parent spec of the withed unit. In this
                           --   case, the with clause is incorrect.
                           --   Parent_Used should be withed (if the withed
                           --   unit is not directly used in a successor);

                           --   2 - If no references are found in
                           --   The_Etbl but some references to the withed
                           --   unit are found in a successopr, then the
                           --   with clause shouldn't be in the spec,
                           --   but in the body

                           --   3 - if no references are found in any
                           --   successor,  Message : Withefd but unused

                           --   4 - If a reference to a parent spec of
                           --   the withed unit has been found in a
                           --   successor then Message : Parent_Used
                           --   should be withed in the body

                           if Is_Parent_Used then
                              if Parent_Used /= The_Withs.Withed_Etbl then
                                 if not In_With_List
                                          (Parent_Used, The_Etbl) then
                                    Write_Withed_Warning
                                      (The_Withs.Withed_Etbl, Replace,
                                       Parent_Used);
                                 else
                                    Write_Withed_Warning
                                      (The_Withs.Withed_Etbl, Norm, null);
                                 end if;

                                 Etbl_Succ := The_Etbl.Successor;
                                 Messages_For_Succ (Already_Withed, The_Etbl);
                              end if;

                           elsif Parent_Used = The_Withs.Withed_Etbl then
                              Etbl_Succ := The_Etbl.Successor;
                              if Etbl_Succ /= null then
                                 if not In_With_List
                                     (The_Withs.Withed_Etbl, Etbl_Succ)
                                 then
                                    Write_Withed_Warning
                                      (The_Withs.Withed_Etbl,
                                       Should, Etbl_Succ);
                                 end if;

                                 Etbl_Succ := Etbl_Succ.Successor;
                                 Messages_For_Succ (Already_Withed, The_Etbl);
                              end if;

                           elsif Parent_Used = null then
                              Write_Withed_Warning
                                (The_Withs.Withed_Etbl, Norm, null);
                              Etbl_Succ := The_Etbl.Successor;
                              Messages_For_Succ (Withed_Unused, null);

                           else
                              Etbl_Succ := The_Etbl.Successor;

                              if Etbl_Succ /= null then

                                 if not
                                   In_With_List (Parent_Used, Etbl_Succ)
                                 then
                                    Write_Withed_Warning
                                      (The_Withs.Withed_Etbl,
                                       Should_Replace, Parent_Used);
                                 end if;

                                 Etbl_Succ := Etbl_Succ.Successor;
                                 Messages_For_Succ (Already_Withed, The_Etbl);
                              end if;
                           end if;

                        else
                           --  If we found a reference to the withed unit in a
                           --  successor, then no message for the current Etbl
                           --  but messages "already withed" for successors.

                           --  If no reference to the withed unit or one of
                           --  its parents has been found then message
                           --  "withed unused".

                           --  If a reference to a parent of the withed
                           --  unit has been found, message "should be
                           --  replaced by"

                           if Parent_Used = The_Withs.Withed_Etbl then
                              Etbl_Succ := The_Etbl.Successor;
                              Messages_For_Succ (Already_Withed, The_Etbl);

                           elsif Parent_Used = null then
                              Write_Withed_Warning
                                (The_Withs.Withed_Etbl, Norm, null);
                              Etbl_Succ := The_Etbl.Successor;
                              Messages_For_Succ (Withed_Unused, null);

                           else
                              if not In_With_List (Parent_Used, The_Etbl) then
                                 Write_Withed_Warning
                                   (The_Withs.Withed_Etbl, Replace,
                                    Parent_Used);
                              else
                                 Write_Withed_Warning
                                   (The_Withs.Withed_Etbl, Norm, null);
                              end if;

                              Etbl_Succ := The_Etbl.Successor;
                              Messages_For_Succ (Already_Withed, The_Etbl);
                           end if;
                        end if;

                  end case;

               end if;
            end if;

            The_Withs.Prev_Msgs := Done;
            The_Withs := The_Withs.Next_With;

         end loop;
      end Check_Withed_Units;

      -----------------------------
      -- Write_References --
      -----------------------------

      procedure Write_References
        (First       : Ref_Acc;
         Only_Marked : Boolean)
      is
         Current_Ref   : Ref_Acc := First;
         Current_Etbl  : Entity_Table_Acc;
         Current_Line  : Logical_Line_Number;
         Current_Col   : Column_Number;

         Previous_Etbl : Entity_Table_Acc;
         Previous_Line : Int := -1;
         Previous_Col  : Int := -1;
         --  These variables are used to suppress the repetition of the
         --  same units and the same line numbers.

      begin
         --  Loop through all the references of the list.

         while Current_Ref /= null loop

            if (not Only_Marked or else Current_Ref.Marked)
              and then Current_Ref.Etbl.RU
            then
               Current_Etbl := Current_Ref.Etbl;

               --  If we find a reference in a new file we add the new
               --  file name and the line number.

               if Current_Etbl /= Previous_Etbl then

                  --  Suppress the } for the first file.

                  if Previous_Etbl /= null
                    and then Previous_Line /= -1
                  then
                     Add_Char_To_Buffer ('}');
                     Write_Xref_Info (Buffer (1 .. Buffer_Length));
                     Buffer_Length := 0;
                  end if;

                  Indent := Reference_Indent;

                  Previous_Etbl := Current_Etbl;
                  Previous_Line := -1;

                  if Global_Xref_File then
                     Add_Str_To_Buffer (Current_Etbl.File_Name.all);
                     Add_Char_To_Buffer (' ');
                  end if;

                  if Global_Xref_File or else Current_Etbl = The_Etbl then
                     Add_Char_To_Buffer ('{');
                  end if;

               end if;

               --  For each reference, we write in the xref file the line
               --  number and the column number where the reference was found
               --  in the following format : line(col)

               Current_Line := Current_Ref.Line_Number;
               Current_Col  := Current_Ref.Col_Number;

               if Global_Xref_File
                 or else Current_Ref.Etbl = The_Etbl
               then
                  --  Suppress the space for the first reference.

                  if (Previous_Line /= -1) then
                     Add_Char_To_Buffer (' ');
                  end if;

                  Previous_Line := Int (Current_Line);
                  Previous_Col  := Int (Current_Col);

                  Add_Nat_To_Buffer (Int (Current_Line));
                  Add_Char_To_Buffer (':');
                  Add_Nat_To_Buffer (Int (Current_Col));
               end if;
            end if;

            Current_Ref := Current_Ref.Next_Ref;
         end loop;

         if Previous_Etbl /= null
           and then Previous_Line /= -1 then
            Add_Char_To_Buffer ('}');
            Write_Xref_Info (Buffer (1 .. Buffer_Length));
            Buffer_Length := 0;
         end if;

      end Write_References;

      -------------------
      -- Write_Warning --
      -------------------

      procedure Write_Warning (The_Entity : Entity_Acc) is
         Parameters : Entity_Acc;

      begin
         if Entity_Warnings
           and then The_Entity.Length = 0
           and then The_Entity.Give_Warning
         then
            --  If the unused entity is a function or a procedure
            --  we don't want to output any messages about parameters

            if The_Entity.Entity_Type in Subprogram_Kind then
               Parameters := The_Entity.Next_Entity;
               while Parameters /= null
                 and then Parameters.Entity_Type in Formal_Kind
               loop
                  Parameters.Give_Warning := False;
                  Parameters := Parameters.Next_Entity;
               end loop;
            end if;

            --  Standard output

            The_Entity.Give_Warning := False;
            Error_Msg_Name_1 := The_Entity.Entity_Char;
            Error_Msg ("?% unused", The_Entity.Entity_Sloc);

         end if;
      end Write_Warning;

      --------------------------
      -- Write_Withed_Warning --
      --------------------------

      procedure Write_Withed_Warning
        (Withing_Etbl : Entity_Table_Acc;
         Warning_Kind : Withed_Warning_Type;
         Extra_Etbl   : Entity_Table_Acc)
      is
         Sloc_With        : Source_Ptr;

         Err_Length       : Integer;
         --  Length of the buffer which must be allocated for error
         --  messages.

         procedure Add_Str_To_Error_Buffer
           (The_String   : String;
            Error_Buffer : in out String);
         --  Adds The_String to the error Buffer;

         function Ref_Of_With return Source_Ptr;
         --  Searches and returns the node of the statement where The_Etbl
         --  is withed. If the node is not found the functions returns Empty
         --  and the compilation is abandonned

         procedure Add_Str_To_Error_Buffer
           (The_String   : String;
            Error_Buffer : in out String)
         is
         begin
            Error_Buffer (Err_Length .. Err_Length + The_String'Length - 1) :=
                            The_String;
            Err_Length := Err_Length + The_String'Length;
         end Add_Str_To_Error_Buffer;

         function Ref_Of_With return Source_Ptr is
            The_Ref : Ref_Acc := Withing_Etbl.First_Entity.First_Ref;

         begin
            while The_Ref /= null loop
               if The_Ref.Etbl = The_Etbl then
                  return The_Ref.Sloc;
               else
                  The_Ref := The_Ref.Next_Ref;
               end if;
            end loop;

            return No_Location;

         end Ref_Of_With;

      --  Start of processing for Write_Withed_Warning

      begin
         if With_Warnings then
            Sloc_With := Ref_Of_With;

            if Sloc_With /= No_Location then

               Error_Msg_Name_1 := Withing_Etbl.First_Entity.Entity_Char;

               if Warning_Kind = Replace
                 or else Warning_Kind = Should_Replace
               then
                  Error_Msg_Name_2 := Extra_Etbl.First_Entity.Entity_Char;
               end if;

               case Warning_Kind is
                  when Norm           =>
                     Err_Length := 3 + Warning_String_3'Length + 1;

                     declare
                        Err_Buffer : String (1 .. Err_Length);

                     begin
                        Err_Length := 1;
                        Add_Str_To_Error_Buffer ("?% ", Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Warning_String_3, Err_Buffer);
                        Err_Buffer (Err_Length .. Err_Length) := ".";
                        Error_Msg (Err_Buffer (1 .. Err_Length), Sloc_With);
                     end;

                  when Should         =>
                     Err_Length := 2 + Warning_String_10'Length
                       + Warning_String_5'Length
                       + Extra_Etbl.File_Name.all'Length + 1;

                     declare
                        Err_Buffer : String (1 .. Err_Length);

                     begin
                        Err_Length := 1;
                        Add_Str_To_Error_Buffer ("?%", Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Warning_String_10, Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Warning_String_5, Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Extra_Etbl.File_Name.all, Err_Buffer);
                        Err_Buffer (Err_Length .. Err_Length) := ".";
                        Error_Msg (Err_Buffer (1 .. Err_Length), Sloc_With);
                     end;

                  when Already        =>
                     Err_Length := 3 + Warning_String_12'Length
                       + Extra_Etbl.File_Name.all'Length + 1;

                     declare
                        Err_Buffer : String (1 .. Err_Length);

                     begin
                        Err_Length := 1;
                        Add_Str_To_Error_Buffer ("?% ", Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Warning_String_12, Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Extra_Etbl.File_Name.all, Err_Buffer);
                        Err_Buffer (Err_Length .. Err_Length) := ".";
                        Error_Msg (Err_Buffer (1 .. Err_Length), Sloc_With);
                     end;

                  when Replace        =>
                     Err_Length := 3 + Warning_String_3'Length + 3
                       + Warning_String_10'Length + 1;

                     declare
                        Err_Buffer : String (1 .. Err_Length);

                     begin
                        Err_Length := 1;
                        Add_Str_To_Error_Buffer ("?% ", Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Warning_String_3, Err_Buffer);
                        Add_Str_To_Error_Buffer (". %", Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Warning_String_10, Err_Buffer);
                        Err_Buffer (Err_Length .. Err_Length) := ".";
                        Error_Msg (Err_Buffer (1 .. Err_Length), Sloc_With);
                     end;

                  when Should_Replace =>
                     Err_Length := 3 + Warning_String_3'Length + 3
                       + Warning_String_10'Length + Warning_String_5'Length +
                       The_Etbl.Successor.File_Name.all'Length + 1;

                     declare
                        Err_Buffer : String (1 .. Err_Length);

                     begin
                        Err_Length := 1;
                        Add_Str_To_Error_Buffer ("?% ", Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Warning_String_3, Err_Buffer);
                        Add_Str_To_Error_Buffer (". %", Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Warning_String_10, Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (Warning_String_5, Err_Buffer);
                        Add_Str_To_Error_Buffer
                          (The_Etbl.Successor.File_Name.all, Err_Buffer);
                        Err_Buffer (Err_Length .. Err_Length) := ".";
                        Error_Msg (Err_Buffer (1 .. Err_Length), Sloc_With);
                     end;

                     --  General note: especially appropriately addressed
                     --  to France! It is undesirable to construct messages
                     --  in this way, because it will make them hard to
                     --  automatically translate. Use a single string with
                     --  insertions always ???

               end case;
            end if;
         end if;

      end Write_Withed_Warning;

   --------------------------------------
   -- Start Processing for Write_Table --
   --------------------------------------

   begin
      Compiler_State := Analyzing;
      --  ??? the above is very strange, needs some comments
      --  ??? this really must be looked at, it is certainly a bug!

      case Level is

         when Full_Xref =>

            if Global_Xref_File then

               --  Write a pretty heading

               Write_Unit_Info (Header_Full, The_Etbl);

               Check_Withed_Units;

               --  Loop through all the entities in Entity_Table.

               E_Tmp := The_Etbl.First_Entity;

               while E_Tmp /= null loop

                  --  First we write the entity,

                  Write_Entity_Info (E_Tmp);

                  --  Give warnings if entity is not used. Still write warnings
                  --  to standard output even for entities declared in a body.

                  Write_Warning (E_Tmp);

                  --  If some references exist, then write them.

                  Write_Xref_Info (Buffer (1 .. Buffer_Length));
                  Buffer_Length := 0;

                  if E_Tmp.First_Ref /= null then
                     Indent := Reference_Indent;
                     Write_References (E_Tmp.First_Ref, False);
                  end if;

                  E_Tmp := E_Tmp.Next_Entity;
               end loop;

            else
               --  In xref levels 3 and 4, we write the following:

               --  For specs : entities declared in the spec and references
               --  to external entities

               --  For bodies and subunits : references to external entities

               if Include_Inlined and then not All_Info_In_Xref then
                  if The_Etbl.Status = A_Body then
                     Has_Inlined := The_Etbl.Predecessor.Has_Inlined;

                  elsif The_Etbl.Status = Sub_Body then
                     Etbl_Prec := The_Etbl.Predecessor;
                     while Etbl_Prec /= null
                       and then Etbl_Prec.Status not in Spec_Status loop
                        Etbl_Prec := Etbl_Prec.Predecessor;
                     end loop;

                     if Etbl_Prec /= null then
                        Has_Inlined := Etbl_Prec.Has_Inlined;
                     end if;
                  end if;
               end if;

               if First_File then

                  --  Before writing any informations in the xref file
                  --  we write all the file that have been loaded during
                  --  the compilation

                  Write_Unit_Info (Header_Full, The_Etbl);

                  if The_Etbl.Status = Sub_Body
                    and then not All_Info_In_Xref
                    and then not (Include_Inlined
                                    and then The_Etbl.First_Entity.Is_Direct)
                  then
                     --  In the case of subunit we write the name of the file
                     --  and the information about the subprogram of this unit.

                     Write_Entity_Info (The_Etbl.First_Entity);

                     Write_Xref_Info (Buffer (1 .. Buffer_Length));
                     Buffer_Length := 0;
                  end if;
               end if;

               E_Tmp := The_Etbl.First_Entity;

               while E_Tmp /= null loop
                  --  Below conditional is too complex ???

                  if First_File
                    and then ((The_Etbl.Status in Spec_Status
                                and then E_Tmp.Entity_Type not in Formal_Kind
                                and then E_Tmp.Entity_Type /= E_Discriminant)
                              or else ((The_Etbl.Status = A_Body
                                        or else The_Etbl.Status = Sub_Body)
                                       and then Has_Inlined
                                       and then E_Tmp.Is_Direct)
                              or else All_Info_In_Xref)

                  then
                     Write_Entity_Info (E_Tmp);
                  end if;

                  Write_Warning (E_Tmp);

                  --  Below conditional is too complex ???

                  if First_File
                    and then ((The_Etbl.Status in Spec_Status
                                and then E_Tmp.Entity_Type not in Formal_Kind
                                and then E_Tmp.Entity_Type /= E_Discriminant)
                              or else ((The_Etbl.Status = A_Body
                                         or else The_Etbl.Status = Sub_Body)
                                       and then Has_Inlined
                                       and then E_Tmp.Is_Direct)
                              or else All_Info_In_Xref)
                  then
                     Write_Xref_Info (Buffer (1 .. Buffer_Length));
                     Buffer_Length := 0;

                     if E_Tmp.First_Ref /= null then
                        Indent := Reference_Indent;
                        Write_References (E_Tmp.First_Ref, false);
                     end if;
                  end if;

                  E_Tmp := E_Tmp.Next_Entity;

               end loop;

               Check_Withed_Units;

               --  if the curret Etbl is not the main one we don't need
               --  to search references that are exported from predecessors
               --  or from units that are withed by the predecessors

               if First_File then
                  Etbl_Prec := The_Etbl.Predecessor;

                  while Etbl_Prec /= null loop
                     if Etbl_Prec.Status /= Sub_Body then
                        The_Withs := Etbl_Prec.First_With;

                        while The_Withs /= null loop
                           if not
                             In_With_List (The_Withs.Withed_Etbl, The_Etbl)
                             and then not The_Withs.Withed_Etbl.Xref_Written
                           then
                              Real_Checked  :=
                                Renamed_Etbl (The_Withs.Withed_Etbl);

                              Clear_And_Mark_Xrefs
                                (Real_Checked, The_Etbl, True, True, False);

                              if not The_Withs.Is_Implicit then
                                 Check_Parents (The_Withs.Withed_Etbl);
                              end if;
                           end if;

                           The_Withs := The_Withs.Next_With;
                        end loop;

                        Real_Checked := Renamed_Etbl (Etbl_Prec);

                        Clear_And_Mark_Xrefs
                          (Etbl_Prec, The_Etbl, True, True, False);
                     end if;

                     Etbl_Prec := Etbl_Prec.Predecessor;
                  end loop;
               end if;

            end if;

         when Smart_Xref =>

            --  Write a pretty heading

            if The_Etbl.Status = Withed_Spec then
               Write_Unit_Info (Header_Stub, The_Etbl);
            end if;

            Check_Withing_Units;

            --  We don't test if The_Etbl.Status = Withed_Spec because
            --  the Smart_Xref case is only treated for Withed_Spec.

            --  Loop through all the entities in Entity_Table.

            --  In this case we write something only if the entity is used
            --  within the target compilation unit.

            E_Tmp := The_Etbl.First_Entity;

            while E_Tmp /= null loop

               --  Don't write unmarked entities.

               if E_Tmp.Marks > 0 then

                  if First_Refs and then The_Etbl.Status = A_Spec then
                     Write_Unit_Info (Header_Stub, The_Etbl);
                     First_Refs := False;
                  end if;

                  --  First we write the entity,

                  Write_Entity_Info (E_Tmp);

                  --  And finally its references.

                  Write_Xref_Info (Buffer (1 .. Buffer_Length));
                  Buffer_Length := 0;
                  Indent := Reference_Indent;
                  Write_References (E_Tmp.First_Ref, True);
               end if;

               E_Tmp := E_Tmp.Next_Entity;
            end loop;

         when Full_Only_Standout =>

            Check_Withed_Units;

            --  Loop through all the entities in Entity_Table.

            E_Tmp := The_Etbl.First_Entity;

            while E_Tmp /= null loop

               Write_Warning (E_Tmp);
               --  Give warnings if the entity is not used.

               E_Tmp := E_Tmp.Next_Entity;

            end loop;

      end case;

   end Writ;

end Xref_Tab;
