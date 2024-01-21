------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 X R E F                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.103 $                            --
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

with Atree;  use Atree;
with Einfo;  use Einfo;
with Elists; use Elists;
with Lib;    use Lib;
with Namet;  use Namet;
with Nlists; use Nlists;
with Osint;  use Osint;
with Opt;    use Opt;
with Output; use Output;
with Sinfo;  use Sinfo;
with Sinput; use Sinput;
with Snames; use Snames;
with Sprint; use Sprint;
with Stand;  use Stand;
with Unchecked_Deallocation;
with Xref_Tab; use Xref_Tab;

package body Xref is

   ---------------------
   --  File suffixes  --
   ---------------------

   Spec_REQ_Suffix : constant String := ".r";
   Body_REQ_Suffix : constant String := ".br";
   Org_Spec_Suffix : constant String := ".org";

   --  ??? the whole idea of a file suffix is too target dependent, and
   --  ??? the knowledge of suffixes should remain in OSINT.

   ------------------
   --  Other data  --
   ------------------

   type Phase_Type is (Add_Entities_And_Refs, Unmark_Refs);

   Phase : Phase_Type;
   --  Used to signal the Traverse_Node procedure what we're doing.
   --
   --    Add_Entities_And_REfs : normal mode to build the Entity_Tables.
   --
   --    Unmark_Refs           : special mode for multipass removal of
   --                            unused nodes within the program tree.

   type Hash_Record is record
      Serial : Nat;
      --  Serial number for hash table entry. A value of zero means that
      --  the entry is currently unused. The serial number is used for
      --  indexing tables of associated information for nodes.

      Id : Int;
      --  If serial number field is non-zero, contains corresponding Id value

      Prev_Inc_Typ_Def : Boolean := False;
      --  Set to True if the node corresponding to the serial
      --  number is the full declaration of an entity having
      --  a previous type definition.
   end record;

   Internal_Node : Boolean;

   type Hash_Table_Type is array (Nat range <>) of Hash_Record;
   type Access_Hash_Table_Type is access Hash_Table_Type;
   Hash_Table : Access_Hash_Table_Type;
   --  The hash table itself, see Serial_Number function for details of use

   Hash_Table_Len : Nat;
   --  Range of Hash_Table is from 0 .. Hash_Table_Len - 1 so that dividing
   --  by Hash_Table_Len gives a remainder that is in Hash_Table'Range.

   Hash_Slot : Nat;
   --  Set by an unsuccessful call to Serial_Number (one which returns zero)
   --  to save the slot that should be used if Set_Serial_Number is called.

   Next_Serial_Number : Nat;
   --  Number of last visited node or list. Used during the marking phase to
   --  set proper node numbers in the hash table.

   type Entity_Association_Type is array (Node_Id range <>) of Entity_Acc;
   type Access_Entity_Association_Type is access Entity_Association_Type;
   Entity_Association : Access_Entity_Association_Type;
   --  Array to associate the entities in the program tree with their
   --  Corresponding data structures.

   type File_Record is record
      Create_REQ : Boolean := False;
      Etbl       : Entity_Table_Acc;
   end record;

   type File_Array_Type is array (Unit_Number_Type range <>) of File_Record;
   type File_Array_Acc is access File_Array_Type;

   Loaded_Files : File_Array_Acc;
   --  Array that associates the loaded files with the Entity_Tables.
   --  Create_REQ is used in Write_Spec_REQs to build the REQ for a withed
   --  spec.

   type Ref_Array_Type is array (Int range <>) of Node_Id;
   type Ref_Array_Acc is access Ref_Array_Type;

   Ref_Buffer : Ref_Array_Acc;
   Ref_Length : Int;
   --  Array to buffer the found references until all entities are added
   --  to our entity lists.

   -------------------------
   --  Local subprograms  --
   -------------------------

   procedure Allocate_Memory;
   --  Allocates the memory on the heap which is needed for the dynamic
   --  arrays.

   procedure Collect_Withs (Unum : Unit_Number_Type);
   --  Collect with clauses in the context clause of the given compilation
   --  unit.

   procedure Deallocate_Memory;
   --  Frees the memory on the heap which is needed for the dynamic arrays.

   procedure Init_Numbers;
   --  Clear Node_Numbers and List_Numbers to False (= unmarked).
   --  Use Init_Numbers before each call of Traverse_Node_Hierarchic.

   procedure Link_Subunits (The_Unit : Unit_Number_Type);
   --  Links the main Entity_Table with its loaded subunits.

   procedure Remove_Entities
     (The_Etbl : Entity_Table_Acc;
      Found    : in out Boolean);
   --  Looks for entities with no references and removes both the fitting node
   --  and all its parents which also get redundant. This is done by a loop
   --  to provide succesive dead code removal.
   --  ??? need to document the function of the parameters, and Found in
   --  particular, what is it and why is it in out ???

   function Serial_Number (Id : Int) return Nat;
   --  Given a Node_Id, List_Id or Elist_Id, returns the previously assigned
   --  serial number, or zero if no serial number has yet been assigned.

   procedure Set_Serial_Number;
   --  Can be called only immediately following a call to Serial_Number that
   --  returned a value of zero. Causes the value of Next_Serial_Number to be
   --  placed in the hash table (corresponding to the Id argument used in the
   --  Serial_Number call), and increments Next_Serial_Number.

   procedure Set_Spec_REQ_Flags (The_Etbl : Entity_Table_Acc);
   --  Sets the Create_REQ fields to create a required interface for all
   --  withed specs as well as to select the transitive specs.

   procedure Traverse_Node_Hierarchic (N : Node_Id);
   --  Recursive procedure traversing the program tree.
   --  Can be used to add information to the Entity_Tables as well as
   --  to remove references of a dead program part (for dead code elimination).
   --  Before each call of Traverse_Node_Hierarchic the procedure
   --  Init_Numbers must be called to initialize the node and list flags.
   --  If an entity declaration is found, then the corresponding entity
   --  information gets immediately added to the fitting Entity_Table.
   --  Every reference which is found is buffered in Ref_Buffer to avoid
   --  the case of adding a reference pointing to an entity that doesn't yet
   --  appear in the fitting Entity_Table.

   procedure Write_Org (The_Etbl : Entity_Table_Acc);
   --  Writes the original source of the given Entity_Table (GNAT -dw conform).
   --  Used to compare the required interfaces with it and then to create the
   --  correct provided interface. File suffix changes to Org_Spec_Suffix.

   procedure Write_Body_REQs;
   --  Writes the REQs for the body and its subunits the disk.
   --  The file name of a Body_REQ is the file name of the compilation unit,
   --  the suffix '.adb' replaced by Body_REQ_Suffix.

   procedure Write_Spec_REQs;
   --  Writes the REQs (one for each withed spec) to the disk.
   --  The file name of a Spec_REQ is the file name of the withed
   --  compilation unit, the suffix '.ads' replaced by
   --  '.' & the name of the withing unit & Body_REQ_Suffix.
   --  e.g.   with Pack1;
   --         procedure Main is ...
   --  then we have the source file name main.adb and would get pack1.main.r
   --  with the Spec_REQ for Pack1 inside.

   procedure Write_Xref (The_Etbl : Entity_Table_Acc);
   --  Creates a cross reference list, containing all the entities and their
   --  corresponding references, which are found in the loaded files. The
   --  references are printed by file name and line numbers of their source
   --  locations. This call has no effect if the appropriate xref switchs
   --  are not set. What is the parameter ???

   -------------------
   -- Serial_Number --
   -------------------

   function Serial_Number (Id : Int) return Nat is
      H : Int := Id mod Hash_Table_Len;

   begin

      if Hash_Table (H).Id = Id and then Hash_Table (H).Prev_Inc_Typ_Def then
         Internal_Node := True;
         Hash_Slot := H;
         return Hash_Table (H).Serial;
      end if;

      while Hash_Table (H).Serial /= 0 loop

         if Id = Hash_Table (H).Id then
            return Hash_Table (H).Serial;
         end if;

         H := H + 1;

         if H > Hash_Table'Last then
            H := 0;
         end if;
      end loop;

      --  Entry was not found, save slot number for possible subsequent call
      --  to Set_Serial_Number, and unconditionally save the Id in this slot
      --  in case of such a call (the Id field is never read if the serial
      --  number of the slot is zero, so this is harmless in the case where
      --  Set_Serial_Number is not subsequently called).

      Hash_Slot := H;
      Hash_Table (H).Id := Id;
      return 0;

   end Serial_Number;

   -----------------------
   -- Set_Serial_Number --
   -----------------------

   procedure Set_Serial_Number is
   begin
      Hash_Table (Hash_Slot).Serial := Next_Serial_Number;
      Next_Serial_Number := Next_Serial_Number + 1;
   end Set_Serial_Number;

   ---------------------
   -- Allocate_Memory --
   ---------------------

   procedure Allocate_Memory is
   begin

      --  Pointers for direct entity access.

      Entity_Association := new Entity_Association_Type
        (Last_Standard_Node_Id + 1 .. Last_Node_Id);
      --  ??? THIS WON'T DO, YOU ARE ASSUMING Node_Id values are contiguous

      --  Array to associate the loaded files with their corresponding
      --  Entity_Table.

      Loaded_Files := new File_Array_Type (Main_Unit .. Last_Unit);

      --  Buffer for found references.

      Ref_Buffer := new Ref_Array_Type (1 ..
        Int (Last_Node_Id - Last_Standard_Node_Id));

      Ref_Length := 0;

      Hash_Table_Len := (150 * (Num_Nodes + Num_Lists + Num_Elists)) / 100;
      Hash_Table := new Hash_Table_Type  (0 .. Hash_Table_Len - 1);

   end Allocate_Memory;

   -------------------
   -- Collect_Withs --
   -------------------

   procedure Collect_Withs (Unum : Unit_Number_Type) is
      Item        : Node_Id;
      Withed_Unit : Unit_Number_Type;

   begin
      Item := First (Context_Items (Cunit (Unum)));
      while Present (Item) loop


         if Nkind (Item) = N_With_Clause
           and then not Implicit_With (Item)
         then
            Withed_Unit := Get_Cunit_Unit_Number (Library_Unit (Item));

            Add_With
              (Loaded_Files (Unum).Etbl, Loaded_Files (Withed_Unit).Etbl);
         elsif Nkind (Item) = N_With_Clause
           and then Implicit_With (Item)
         then
            Withed_Unit := Get_Cunit_Unit_Number (Library_Unit (Item));
            if Chars (Name (Item)) in Text_IO_Package_Name then
               Add_With
                 (Loaded_Files (Unum).Etbl, Loaded_Files (Withed_Unit).Etbl,
                  True);
               null;
            end if;
         end if;

         Item := Next (Item);
      end loop;
   end Collect_Withs;

   -----------------------
   -- Deallocate_Memory --
   -----------------------

   procedure Deallocate_Memory is

      procedure Free is new Unchecked_Deallocation (Entity_Association_Type,
        Access_Entity_Association_Type);

      procedure Free is new Unchecked_Deallocation (File_Array_Type,
        File_Array_Acc);

      procedure Free is new Unchecked_Deallocation (Ref_Array_Type,
        Ref_Array_Acc);

      procedure Free is new Unchecked_Deallocation (Hash_Table_Type,
        Access_Hash_Table_Type);

   begin
      Free (Entity_Association);
      Free (Loaded_Files);
      Free (Ref_Buffer);
      Free (Hash_Table);
   end Deallocate_Memory;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      if Operating_Mode = Check_Syntax then
         return;
      end if;

      --  If we already called Write_Xref from Gather_Xref_Info for each file
      --  in the case where we want only smart information in the xref file
      --  (switches x3 and x4 of gnatf), then nothing to do

      if Xref_Flag and then not Global_Xref_File then
         return;

      --  Otherwise write the xref information now

      else
         Write_Xref (Xref_Tab.First_Etbl);
      end if;

   end Finalize;

   ----------------------
   -- Gather_Xref_Info --
   ----------------------

   procedure Gather_Xref_Info (Top : Node_Id) is
      Spec          : Unit_Number_Type;
      Unum          : Unit_Number_Type;
      To_Unum       : Unit_Number_Type;
      To_Entity     : Entity_Id;
      The_Entity    : Entity_Acc;
      Ref_Node      : Node_Id;
      Main_Etbl     : Entity_Table_Acc;
      Etbl_Tmp      : Entity_Table_Acc;
      Etbl_List     : Entity_Table_Acc;
      Renamed_Node  : Node_Id;
      Renamed_Sloc  : Source_Ptr;
      Renamed_Unit  : Unit_Number_Type;
      Renamed_Found : Boolean;
      Unit_Node     : Node_Id;

   begin
      --  Immediate return if not collecting Xref information
      --  (should this be a check of Xref_Analyze?)

      if Operating_Mode = Check_Syntax
        or else
          not (With_Warnings    or
               Xref_Flag        or
               Spec_REQs_Flag   or
               Body_REQs_Flag)
      then
         return;
      end if;

      --  Allocate dynamic arrays

      Allocate_Memory;

      --  Associate the loaded files with the Entity_Tables.
      --  Ignore Zombies (e.g. the spec for a body acting as spec)!

      Etbl_Tmp := Xref_Tab.Last_Etbl;
      for J in Loaded_Files'Range loop
         Add_Etbl (First_Etbl, Last_Etbl, J, Loaded_Files (J).Etbl);
      end loop;

      Main_Etbl  := Loaded_Files (Main_Unit).Etbl;

      --  We perform research research only for the entity tables
      --  that have been created in the current compilation.

      if Etbl_Tmp = null then
         Etbl_Tmp := Main_Etbl;
      else
         Etbl_Tmp := Etbl_Tmp.Next_Etbl;
      end if;

      while Etbl_Tmp /= null loop

         if Nkind (Unit (Etbl_Tmp.Top_Node)) =
                             N_Package_Renaming_Declaration
         then
            Renamed_Node := Name (Unit (Etbl_Tmp.Top_Node));
            Renamed_Found := False;

            --  Searches the node of the renamed package and the name of
            --  the corresponding file where this package is declared.

            while not Renamed_Found loop
               Renamed_Node := Parent (Node_Id (Entity (Renamed_Node)));

               if Nkind (Renamed_Node) =
                  N_Package_Renaming_Declaration
               then
                  Renamed_Node := Name (Renamed_Node);
               else
                  Renamed_Found := True;
               end if;
            end loop;

            Renamed_Sloc := Sloc (Renamed_Node);
            Renamed_Unit := Get_Sloc_Unit_Number (Renamed_Sloc);
            Get_Name_String (Full_File_Name (Source_Index (Renamed_Unit)));

            --  Searches the corresponding Etbl

            Etbl_List := First_Etbl;
            while Etbl_List /= null loop
               if Etbl_List.File_Name.all = Name_Buffer (1 .. Name_Len) then
                  Etbl_Tmp.Renamed_Etbl := Etbl_List;
                  exit;
               end if;

               Etbl_List := Etbl_List.Next_Etbl;
            end loop;
         end if;

         Etbl_Tmp := Etbl_Tmp.Next_Etbl;
      end loop;

      --  We mark the main Entity_Table as a 'required unit', that is
      --  we create a fully detailed Xref for it.

      Main_Etbl.RU := True;

      --  Look for with clauses within the loaded files.

      --  Ignore Zombies (e.g. the spec for a body acting as spec) and
      --  do this only once ('Examined' signals a previous examination)!

      for J in Loaded_Files'Range loop
         if not Loaded_Files (J).Etbl.Examined then
            Collect_Withs (J);
         end if;
      end loop;

      --  Here we link the multiple parts of an object
      --  (i.e. Spec/Body/Subunits) to give correct With_Warnings.

      case Main_Etbl.Status is

         when A_Body =>

            Spec := Get_Cunit_Unit_Number (Library_Unit (Top));

            if Spec_REQs_Flag then
               Write_Org (Loaded_Files (Spec).Etbl);
            end if;

            Loaded_Files (Spec).Etbl.RU := True;
            Loaded_Files (Spec).Create_REQ := True;

            --  In multifile mode we create full output for this spec.
            --  Otherwise we print only what is used.

            Loaded_Files (Spec).Etbl.Status := Withed_Spec;

            Loaded_Files (Spec).Etbl.Successor := Main_Etbl;

            Main_Etbl.Predecessor := Loaded_Files (Spec).Etbl;
            Link_Subunits (Main_Unit);

         when Sub_Body =>

            Unum := Get_Cunit_Unit_Number (Library_Unit (Top));
            Loaded_Files (Unum).Etbl.RU := True;

            if Loaded_Files (Unum).Etbl.Status = A_Body then
               Spec := Get_Cunit_Unit_Number (Library_Unit (Cunit (Unum)));

               if Spec_REQs_Flag then
                  Write_Org (Loaded_Files (Spec).Etbl);
               end if;

               --  In multifile mode we create full output for this spec.
               --  Otherwise we print only what is used.
               Loaded_Files (Spec).Etbl.RU := True;

               Loaded_Files (Spec).Etbl.Status := Withed_Spec;

               Loaded_Files (Spec).Etbl.Successor :=
                 Loaded_Files (Unum).Etbl;

               Loaded_Files (Unum).Etbl.Predecessor :=
                 Loaded_Files (Spec).Etbl;
            end if;

            Link_Subunits (Unum);

         when Body_As_Spec =>

            Link_Subunits (Main_Unit);

         when A_Spec | Withed_Spec =>

            if Spec_REQs_Flag then
               Write_Org (Main_Etbl);
            end if;

      end case;

      --  After linking the main unit with its predecessors and successors
      --  some other units must also be examined
      --     - bodies of generic packages must be linked
      --     - Child spec must be linked with their parents

      for All_The_Units in Loaded_Files'Range loop

         --  if this entity table has not been linked yet then
         --  we must examine it

         if not Loaded_Files (All_The_Units).Etbl.Linked then
            case Loaded_Files (All_The_Units).Etbl.Status is
               when A_Spec | Withed_Spec =>
                  Unit_Node :=
                    Unit (Loaded_Files (All_The_Units).Etbl.Top_Node);
                  if Parent_Spec (Unit_Node) /= Empty then

                     --  If parent_spec is not empty then the current spec
                     --  is a child spec. So we get the parent spec and set up
                     --  the predecessor and successor links

                     Spec := Get_Cunit_Unit_Number (Parent_Spec (Unit_Node));

                     --  Adds Child_Spec to the list of children of the parent

                     Etbl_Tmp := Loaded_Files (Spec).Etbl;

                     if Etbl_Tmp.First_Child = null then
                        Etbl_Tmp.First_Child := new Child_Spec;
                        Etbl_Tmp.First_Child.Child_Etbl :=
                          Loaded_Files (All_The_Units).Etbl;
                     else
                        declare
                           Child_Units : Child_Spec_Acc :=
                                            Etbl_Tmp.First_Child;
                        begin
                           while Child_Units.Next_Child /= null loop
                              Child_Units := Child_Units.Next_Child;
                           end loop;

                           Child_Units.Next_Child := new Child_Spec;
                           Child_Units.Next_Child.Child_Etbl :=
                             Loaded_Files (All_The_Units).Etbl;
                        end;
                     end if;

                     Loaded_Files (All_The_Units).Etbl.Predecessor :=
                       Etbl_Tmp;

                  end if;

               when A_Body =>

                  --  We get the corresponding spec

                  Spec := Get_Cunit_Unit_Number
                            (Library_Unit (Cunit (All_The_Units)));

                  --  We set up the predecessor and successor links

                  Loaded_Files (All_The_Units).Etbl.Predecessor :=
                    Loaded_Files (Spec).Etbl;

                  Loaded_Files (Spec).Etbl.Successor :=
                    Loaded_Files (All_The_Units).Etbl;

               when Sub_Body =>
                  Unum := Get_Cunit_Unit_Number (Library_Unit
                                                   (Cunit (All_The_Units)));
                  Etbl_Tmp := Loaded_Files (Unum).Etbl;

                  Loaded_Files (All_The_Units).Etbl.Predecessor := Etbl_Tmp;

                  while Etbl_Tmp.Successor /= null loop
                     Etbl_Tmp := Etbl_Tmp.Successor;
                  end loop;

                  Etbl_Tmp.Successor := Loaded_Files (All_The_Units).Etbl;

               when others =>
                  null;

            end case;

         end if;

         Loaded_Files (All_The_Units).Etbl.Linked := True;

      end loop;

      Set_Spec_REQ_Flags (Main_Etbl);

      --  Search and add the entities to the entity tables.

      Phase := Add_Entities_And_Refs;

      --  Hierarchic scan of the nodes instead of a linear scan
      --  It must be checked that the hierarchic scan is not
      --  too much slower than the linear onw

      for All_The_Units in Main_Unit .. Last_Unit loop
         Init_Numbers;
         Traverse_Node_Hierarchic (Cunit (All_The_Units));
      end loop;

      --  Take the references from the buffer and add them to the
      --  fitting entities.

      for J in 1 .. Ref_Length loop
         Ref_Node := Ref_Buffer (J);

         --  if the reference node is matches a with clause. Then we skip
         --  this reference if the with clause is implicit.
         --  It's mainly the case when a child unit is withed. Implicit withs
         --  are generated for all the parent specs.

         if not (Nkind (Parent (Ref_Node)) = N_With_Clause
                   and then Implicit_With (Parent (Ref_Node)))
         then

            To_Entity := Entity (Ref_Node);

            Unum    := Get_Sloc_Unit_Number (Sloc (Ref_Node));
            To_Unum := Get_Sloc_Unit_Number (Sloc (Entity (Ref_Node)));

            --  We don't add reference :
            --     - for entities declared in Specs that are referenced in a
            --       successor of this spec
            --     - for entities declared in bodies

            To_Entity := Entity (Ref_Node);

            if not All_Info_In_Xref
              and then
                (Nkind (Parent (To_Entity)) = N_Enumeration_Type_Definition
                  or else
                    Nkind (Parent (To_Entity)) = N_Component_Declaration)
            then
               loop
                  To_Entity := Parent (To_Entity);
                  exit when Nkind (To_Entity) = N_Full_Type_Declaration;
               end loop;

               To_Entity := Defining_Identifier (To_Entity);
            end if;

            The_Entity := Entity_Association (To_Entity);

            if Unum = To_Unum
              and then Loaded_Files (Unum).Etbl.Status in Body_Status
              and then not All_Info_In_Xref
              and then not Include_Inlined

            then
               --  We update The_Entity.Length to avoid junk messages.

               if Nkind (Parent (Ref_Node)) /= N_Pragma_Argument_Association
                 and then The_Entity /= null
               then
                  The_Entity.Length := The_Entity.Length + 1;
               end if;

            else
               if not Loaded_Files (Unum).Etbl.Examined then

                  Add_Reference
                    (The_Entity, Loaded_Files (Unum).Etbl, Ref_Node);

                  if Nkind (Parent (Ref_Node)) in N_Generic_Instantiation then

                     declare
                        Ref_Etbl : constant Entity_Table_Acc :=
                                     Loaded_Files (Unum).Etbl;
                        Ent_Etbl : constant Entity_Table_Acc :=
                                     Loaded_Files (To_Unum).Etbl;
                        I_Tmp    : Include_Acc;

                     begin
                        I_Tmp := Ref_Etbl.First_Include;

                        if I_Tmp = null then
                           Ref_Etbl.First_Include := new Include;
                           Ref_Etbl.First_Include.Included_Etbl := Ent_Etbl;

                        else
                           while I_Tmp.Included_Etbl /= Ent_Etbl
                             and then I_Tmp.Next_Include /= null
                           loop
                              I_Tmp := I_Tmp.Next_Include;
                           end loop;

                           if I_Tmp.Included_Etbl /= Ent_Etbl then
                              I_Tmp.Next_Include := new Include;
                              I_Tmp.Next_Include.Included_Etbl := Ent_Etbl;
                           end if;
                        end if;
                     end;
                  end if;

               elsif Spec_REQs_Flag
                 and then Loaded_Files (Unum).Create_REQ
                 and then Unum = To_Unum
               then
                  Update_Reference
                    (The_Entity, Loaded_Files (Unum).Etbl, Ref_Node);
               end if;
            end if;
         end if;
      end loop;


      --  Write the Spec_REQs.

      if Spec_REQs_Flag then
         Write_Spec_REQs;
      end if;

      --  Write the Body_REQs.

      if Body_REQs_Flag
        and then Loaded_Files (Main_Unit).Etbl.Status in Body_Status
      then
         Write_Body_REQs;
      end if;

      --  Mark the units to be Examined.

      for J in Loaded_Files'Range loop
         if Loaded_Files (J).Etbl /= null then
            Loaded_Files (J).Etbl.Examined := True;
         end if;
      end loop;

      if Xref_Flag and then not Global_Xref_File then
         Write_Xref (Main_Etbl);
      end if;

      --  Deallocate dynamic arrays

      Deallocate_Memory;

   end Gather_Xref_Info;

   ------------------
   -- Init_Numbers --
   ------------------

   procedure Init_Numbers is
   begin
      --  Set node flags to unvisited (except those of Standard).

      --  Allocate and clear serial number hash table. The size is 150% of
      --  the maximum possible number of entries, so that the hash table
      --  cannot get significantly overloaded.

      for J in Hash_Table'Range loop
         Hash_Table (J).Serial := 0;
      end loop;

      Next_Serial_Number := 1;

   end Init_Numbers;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      With_Warnings       := Xref_Flag_1 or Xref_Flag_2;

      Entity_Warnings     := Xref_Flag_2;

      Xref_Flag           := Xref_Flag_3 or Xref_Flag_4 or Xref_Flag_5
                               or Xref_Flag_6;

      Entity_Info_In_Xref := Xref_Flag_6;

      All_Info_In_Xref    := Xref_Flag_5 or Xref_Flag_6;

      Global_Xref_File    := Xref_Flag_6;

      Include_Inlined     := Xref_Flag_4;

      Spec_REQs_Flag      := Xref_Flag_S;

      Body_REQs_Flag      := Xref_Flag_B;

      Xref_Analyze        := (With_Warnings
                                or else Spec_REQs_Flag
                                or else Body_REQs_Flag
                                or else Xref_Flag)
                             and then Operating_Mode /= Check_Semantics;

      --  Above test of Operating_Mode is incomprehensible!


   end Initialize;

   -------------------
   -- Link_Subunits --
   -------------------

   procedure Link_Subunits (The_Unit : Unit_Number_Type) is
      Top_Node  : constant Node_Id := Cunit (The_Unit);
      Etbl_Tmp  : Entity_Table_Acc;

   begin
      Etbl_Tmp := Loaded_Files (The_Unit).Etbl;

      for J in Loaded_Files'Range loop
         if Loaded_Files (J).Etbl /= null
           and then Loaded_Files (J).Etbl.Kind = Subunit
           and then J /= The_Unit
           and then Library_Unit (Cunit (J)) = Top_Node
           and then Loaded_Files (J).Etbl.Predecessor = null
         then
            while Etbl_Tmp.Successor /= null loop
               Etbl_Tmp := Etbl_Tmp.Successor;
            end loop;

            Etbl_Tmp.Successor := Loaded_Files (J).Etbl;

            --  Temporary The predecessor of a subunit is the body
            --  where the stub is defined  ???

            Loaded_Files (J).Etbl.Predecessor := Loaded_Files (The_Unit).Etbl;
            Loaded_Files (J).Etbl.Linked := True;
         end if;
      end loop;

      Loaded_Files (The_Unit).Etbl.Linked := True;
   end Link_Subunits;

   ---------------------
   -- Remove_Entities --
   ---------------------

   procedure Remove_Entities
     (The_Etbl : Entity_Table_Acc;
      Found    : in out Boolean)
   is
      E_Tmp : Entity_Acc;
      --  To store the current entity within the loop.

      procedure Remove_Element (The_Node : Node_Id);
      --  Removes the given node and also the surrounding Ada construct.

      procedure Remove_Pragmas (The_Entity : Entity_Acc);
      --  Removes all the pragma nodes within the program tree which
      --  apply on the given entity.

      --------------------
      -- Remove_Element --
      --------------------

      procedure Remove_Element (The_Node : Node_Id) is
         L_Tmp : List_Id;
         P_Tmp : Node_Id;

         procedure Remove_List;

         procedure Remove_List is
            L_Tmp : List_Id;
            P_Tmp : Node_Id;

         begin
            P_Tmp := Parent (The_Node);

            if Is_List_Member (The_Node) then
               L_Tmp := List_Containing (The_Node);

               --  Cancel the references in the subtree of The_Node;

               Traverse_Node_Hierarchic (The_Node);
               Remove (The_Node);

               if Is_Empty_List (L_Tmp) then
                  Remove_Element (P_Tmp);
               end if;

            else
               --  Cancel the references in the subtree of The_Node

               Traverse_Node_Hierarchic (The_Node);
               Remove_Element (P_Tmp);
            end if;
         end Remove_List;

      begin
         if Nkind (The_Node) in N_Entity then

         --  If we get a N_Defining_Identifier, N_Defining_Character_Literal or
         --  N_Defining_Operator_Symbol, then we have to distinguish between
         --  the various kinds of entities.

            case Ekind (The_Node) is

               --  We don't remove void entities since they are used internally

               when E_Void =>
                  null;

               --  Don't do anything with components, since it is very tricky
               --  to find a hidden reference for a record component (e.g. in
               --  an aggregate, all components are implicitly referenced).

               when E_Component =>
                  null;

               --  Main processing for most cases

               when E_Access_Protected_Subprogram_Type  |
                    E_Access_Subtype                    |
                    E_Access_Type                       |
                    E_Array_Subtype                     |
                    E_Array_Type                        |
                    E_Block                             |
                    E_Class_Wide_Subtype                |
                    E_Class_Wide_Type                   |
                    E_Constant                          |
                    E_Decimal_Fixed_Point_Type          |
                    E_Decimal_Fixed_Point_Subtype       |
                    E_Discriminant                      |
                    E_Entry                             |
                    E_Entry_Family                      |
                    E_Entry_Index_Parameter             |
                    E_Enum_Table_Type                   |
                    E_Enumeration_Subtype               |
                    E_Enumeration_Type                  |
                    E_Exception                         |
                    E_Exception_Type                    |
                    E_Floating_Point_Subtype            |
                    E_Floating_Point_Type               |
                    E_General_Access_Type               |
                    E_Incomplete_Type                   |
                    E_Label                             |
                    E_Loop                              |
                    E_Loop_Parameter                    |
                    E_Modular_Integer_Subtype           |
                    E_Modular_Integer_Type              |
                    E_Named_Integer                     |
                    E_Named_Real                        |
                    E_Ordinary_Fixed_Point_Type         |
                    E_Ordinary_Fixed_Point_Subtype      |
                    E_Protected_Object                  |
                    E_Protected_Subtype                 |
                    E_Protected_Type                    |
                    E_Record_Subtype                    |
                    E_Record_Type                       |
                    E_Signed_Integer_Subtype            |
                    E_Signed_Integer_Type               |
                    E_String_Literal_Subtype            |
                    E_String_Subtype                    |
                    E_String_Type                       |
                    E_Subprogram_Type                   |
                    E_Task_Subtype                      |
                    E_Task_Type                         |
                    E_Variable                          =>

                  Remove_List;

               --  Private type processing

               when E_Limited_Private_Subtype           |
                    E_Limited_Private_Type              |
                    E_Private_Subtype                   |
                    E_Private_Type                      |
                    E_Record_Subtype_With_Private       |
                    E_Record_Type_With_Private          =>

                  Remove_Element (Full_View (The_Node));
                  Remove_List;

               --  Since it's very tricky to find a hidden reference for an
               --  enumeration literal we do nothing (e.g. in a loop from
               --  type'First to type'Last all literals are automatically
               --  referenced).

               when E_Enumeration_Literal =>
                  null;

               --  Compilation unit nodes are not removed since
               --  this causes a lot of trouble.

               when E_Function                          |
                    E_Operator                          |
                    E_Package                           |
                    E_Procedure                         =>

                  if Scope (The_Node) > Last_Standard_Node_Id then

                     --  Cancel the references in the subtree of The_Node.

                     Traverse_Node_Hierarchic (The_Node);

                     P_Tmp := Parent (The_Node);
                     Remove_Element (P_Tmp);

                     --  There are only two possibilities in this case:
                     --  Either the parent is a N_Function_Specification or
                     --  a N_Procedure_Specification.

                  end if;

               --  Cannot delete a parameter of a subprogram (or at least not
               --  easily because it would have all sorts of consequences)

               when E_In_Out_Parameter                  |
                    E_In_Parameter                      |
                    E_Out_Parameter                     =>

                  null;

               --  For the moment, forget the case of generics and their formal
               --  parameters. Might try to do something later here ???

               when E_Generic_Function                  |
                    E_Generic_In_Out_Parameter          |
                    E_Generic_In_Parameter              |
                    E_Generic_Package                   |
                    E_Generic_Procedure                 =>

                  null;

               --  Body entities have no significance semantically, so ignore

               when E_Package_Body                      |
                    E_Protected_Body                    |
                    E_Subprogram_Body                   |
                    E_Task_Body                         =>

                  null;

               --  Internal allocator and access types should never appear
               --  at the stage a cross-reference is being computed

               when E_Access_Subprogram_Type            |
                    E_Allocator_Type                    |
                    E_Anonymous_Access_Type             =>

                  pragma Assert (False); null;
            end case;

         else
            case Nkind (The_Node) is

               when N_Empty =>
                  null;

               when N_Function_Specification .. N_Procedure_Specification =>

                  --  There are only two possibilities in this case:
                  --  Either the parent is a N_Subprogram_Declaration or
                  --  a N_Subprogram_Body.

                  P_Tmp := Parent (The_Node);
                  Remove_Element (P_Tmp);

               when N_Component_List | N_Record_Definition =>

                  --  These are intermediate nodes.
                  --  In this cases we don't remove the parent
                  --  (e.g. N_Record_Definition for a N_Component_List)
                  --  since the Record identifier is treated separately.

                  null;

               when N_Object_Declaration =>
                  if Is_List_Member (The_Node) then
                     L_Tmp := List_Containing (The_Node);
                     P_Tmp := Parent (The_Node);

                     Traverse_Node_Hierarchic (The_Node);
                     --  To cancel the references in the subtree of The_Node.

                     Remove (The_Node);

                     --  Here we do *not* remove the parent since then we
                     --  would delete the whole subprogram.
                  end if;

               when others =>
                  Remove_List;
            end case;
         end if;

      end Remove_Element;

      --------------------
      -- Remove_Pragmas --
      --------------------

      procedure Remove_Pragmas (The_Entity : Entity_Acc) is
         R_Tmp : Ref_Acc := Xref_Tab.First (The_Entity);

      begin
         while not Is_Null (R_Tmp) loop

            if Is_Pragma (R_Tmp) then
               Remove_Element (Parent (Parent (The_Node (R_Tmp))));
            end if;

            R_Tmp := Next (R_Tmp);
         end loop;

      end Remove_Pragmas;

   -------------------------------------------
   --  Start Processing for Remove_Entities --
   -------------------------------------------

   begin
      E_Tmp := The_Etbl.First_Entity;
      Found := False;

      Phase := Unmark_Refs;
      Init_Numbers;

      while not Is_Null (E_Tmp) loop

         if Number_Of_Marks (E_Tmp) = 0
           and then Give_Warning (E_Tmp)
         then
            --  Update the Entity_Table:

            Found := True;

            --  We mark the entity in our entity table for the next pass
            --  to say: Don't remove this entity twice!

            Mark_Entity (E_Tmp);

            --  Update the program tree:
            --
            --  Since we need E_Tmp to find the pragmas, we have to
            --  remove the pragmas here rather than in Remove_Element

            if Entity_Type (E_Tmp) in E_Function .. E_Procedure then
               Remove_Pragmas (E_Tmp);
            end if;

            --  Remove the entities with no references.

            Remove_Element (Entity_Node (E_Tmp));

         end if;

         E_Tmp := Next (E_Tmp);

      end loop;
   end Remove_Entities;

   -------------------------
   -- Set_Spec_REQ_Flags  --
   -------------------------

   procedure Set_Spec_REQ_Flags (The_Etbl : Entity_Table_Acc) is
      Etbl_Tmp    : Entity_Table_Acc;
      With_Clause : With_Acc;

   begin
      --  Mark a spec withed by The_Etbl or a predecessor.

      Etbl_Tmp := The_Etbl;
      while Etbl_Tmp /= null loop

         With_Clause := Etbl_Tmp.First_With;
         while With_Clause /= null loop

            if With_Clause.Withed_Etbl.Kind /= Genr then
               Loaded_Files (Get_Cunit_Unit_Number
                 (With_Clause.Withed_Etbl.Top_Node)).Create_REQ := True;
            end if;

            --  Note: we don't create a REQ for a generic.

            With_Clause.Withed_Etbl.Status := Withed_Spec;
            With_Clause := With_Clause.Next_With;
         end loop;

         Etbl_Tmp := Etbl_Tmp.Predecessor;
      end loop;

      --  Mark a spec withed by a successor.

      Etbl_Tmp := The_Etbl.Successor;

      while Etbl_Tmp /= null loop

         With_Clause := Etbl_Tmp.First_With;
         while With_Clause /= null loop

            if With_Clause.Withed_Etbl.Kind /= Genr then
               Loaded_Files (Get_Cunit_Unit_Number
                 (With_Clause.Withed_Etbl.Top_Node)).Create_REQ := True;
            end if;

            --  Note: we don't create a REQ for a generic.

            With_Clause.Withed_Etbl.Status := Withed_Spec;
            With_Clause := With_Clause.Next_With;
         end loop;

         Etbl_Tmp := Etbl_Tmp.Successor;
      end loop;

   end Set_Spec_REQ_Flags;

   ------------------------------
   -- Traverse_Node_Hierarchic --
   ------------------------------

   procedure Traverse_Node_Hierarchic (N : Node_Id) is
      Kind        : Node_Kind;
      Unum        : Unit_Number_Type;
      To_Unum     : Unit_Number_Type;
      To_Entity   : Entity_Id;
      --  Only to reduce the length of some function calls.

      procedure Mark_Full_Declaration (N : Node_Id);
      --  Marks the node corresponding to the full declaration of N
      --  as having a previous incomplete definition

      procedure Visit_List (L : List_Id);
      --  Visits the nodes of a list in the program tree

      procedure Visit_Elist (E : Elist_Id);

      procedure Visit_Descendent (D : Union_Id);
      pragma Inline (Visit_Descendent);
      --  Visits a descendent of a node, where D is the descendent

      -------------------
      -- Mark_Internal --
      -------------------

      procedure Mark_Full_Declaration (N : Node_Id) is
         Parent_Node : constant Node_Id := Parent (N);
         H           : Nat;

      begin

         if Nkind (Parent_Node) = N_Private_Type_Declaration
           or else Nkind (Parent_Node) = N_Private_Extension_Declaration
           or else (Nkind (Parent_Node) = N_Object_Declaration
                    and then Constant_Present (Parent_Node)
                    and then Expression (Parent_Node) = Empty)
         then
            H := Int (Full_View (N)) mod Hash_Table_Len;
            Hash_Table (H).Id := Int (Full_View (N));
            Hash_Table (H).Prev_Inc_Typ_Def := True;
         end if;
      end Mark_Full_Declaration;

      -----------------
      -- Visit_Elist --
      -----------------

      procedure Visit_Elist (E : Elist_Id) is
         M : Elmt_Id;
         S : constant Nat := Serial_Number (Int (E));

      begin
         if S /= 0 then
            return;
         else
            Set_Serial_Number;
         end if;

         M := First_Elmt (E);

         while Present (M) loop
            Traverse_Node_Hierarchic (Node (M));
            M := Next_Elmt (M);
         end loop;
      end Visit_Elist;

      ----------------
      -- Visit_List --
      ----------------

      procedure Visit_List (L : List_Id) is
         N : Node_Id;
         S : constant Nat := Serial_Number (Int (L));

      begin

         if S /= 0 then
            return;
         else
            Set_Serial_Number;
         end if;

         N := First (L);

         if N /= Empty then
            while Next (N) /= Empty loop
               Traverse_Node_Hierarchic (N);
               N := Next (N);
            end loop;
         end if;

         Traverse_Node_Hierarchic (N);
      end Visit_List;

      ----------------------
      -- Visit_Descendent --
      ----------------------

      procedure Visit_Descendent (D : Union_Id) is
      begin
         if D in Node_Range then
            if D <= Union_Id (Empty_Or_Error) then
               return;
            end if;

            if Sloc (Node_Id (D)) <= Standard_Location then
               if Sloc (N) > Standard_Location then
                  return;
               end if;
            else
               if Sloc (N) <= Standard_Location
                 or else Sloc (N) = No_Location
                 or else Sloc (Node_Id (D)) = No_Location
                 or else Get_Sloc_Unit_Number (Sloc (Node_Id (D))) /=
                         Get_Sloc_Unit_Number (Sloc (N))
               then
                  return;
               end if;
            end if;

            if Parent (Node_Id (D)) /= Empty
              and then Parent (Node_Id (D)) /= N
            then
               return;
            end if;

            Traverse_Node_Hierarchic (Node_Id (D));

         elsif D in List_Range then
            if D = Union_Id (No_List)
              or else D = Union_Id (Error_List)
              or else Is_Empty_List (List_Id (D))
            then
               return;
            else
               Visit_List (List_Id (D));
            end if;

         elsif D in Elist_Range then
            if D = Union_Id (No_Elist)
              or else Is_Empty_Elmt_List (Elist_Id (D))
            then
               return;
            else
               Visit_Elist (Elist_Id (D));
            end if;

         else
            null;
         end if;
      end Visit_Descendent;

   ------------------------------------------------------
   -- Start of Processing for Traverse_Node_Hierarchic --
   ------------------------------------------------------

   begin
      if N = Empty then
         return;
      end if;

      Internal_Node := False;

      if Serial_Number (Int (N)) /= 0 then
         return; -- already visited
      else
         Set_Serial_Number;
      end if;

      Kind := Nkind (N);

      --  Look for named numbers or other constructs which get transformed
      --  into the corresponding litterals during semantics.

      if Kind = N_Package_Declaration and then
        Generic_Parent (Specification (N)) /= Empty
      then
         Traverse_Node_Hierarchic
           (Defining_Unit_Name (Specification (N)));
      end if;

      if Is_Rewrite_Substitution (N) then
         Traverse_Node_Hierarchic (Original_Node (N));
      end if;

      if Kind in N_Entity
        and then Ekind (N) /= E_Void
        and then Comes_From_Source (N)
        and then Parent (N) /= Empty
      then
         --  In the case we don't want the whole cross reference list to be
         --  generated we don't add declaration of :
         --     - enumeration litterals
         --     - record components
         --     - Parameters

         if (All_Info_In_Xref
              or else
                (Nkind (Parent (N)) /= N_Enumeration_Type_Definition
                  and then Nkind (Parent (N)) /= N_Component_Declaration))
           and then not Internal_Node
         then
            Unum := Get_Sloc_Unit_Number (Sloc (N));

            case Phase is

               when Add_Entities_And_Refs =>

                  if Loaded_Files (Unum).Etbl.Examined then
                     Update_Entity (Loaded_Files (Unum).Etbl,
                       N, Entity_Association (N));
                  else
                     Add_Entity (Loaded_Files (Unum).Etbl,
                       N, Entity_Association (N));
                  end if;

                  Mark_Full_Declaration (N);

               when Unmark_Refs =>

                  --  We mark the entity and all its descendents in our
                  --  entity table for the next pass to say:
                  --  Don't remove this entity twice! It's already removed.

               Mark_Entity (In_E_List (Loaded_Files (Unum).Etbl, N));

            end case;
         end if;

      elsif Kind = N_Identifier then

         --  In the case we don't want the whole cross-reference list to be
         --  generated we don.t add reference to parameters.

         if Nkind (Parent (N)) = N_Range
           and then Parent (Parent (N)) = Empty
         then
            --  We don't accept things like an internal range declaration

            null;

         elsif Entity (N) > Last_Standard_Node_Id then

         --  We suppress references to standard entities (e.g. integer)

            case Phase is
               when Add_Entities_And_Refs =>

                  --  Check that we don't add an internal reference

                  if Sloc (N) > 0 then
                     Ref_Length := Ref_Length + 1;
                     Ref_Buffer (Ref_Length) := N;
                  end if;

               when Unmark_Refs          =>
                  To_Entity := Entity (N);
                  To_Unum := Get_Sloc_Unit_Number (Sloc (To_Entity));
                  Unmark_Reference (Entity_Association (To_Entity), N);
            end case;
         end if;

      elsif (Kind in N_Op
              or else Kind = N_Attribute_Reference
              or else Kind = N_Character_Literal
              or else Kind = N_Expanded_Name
              or else Kind = N_Operator_Symbol)
        and then Entity (N) > Last_Standard_Node_Id
      then
         case Phase is
            when Add_Entities_And_Refs =>

               --  Checks that we don't add an internal reference

               if Sloc (N) > 0 then
                  Ref_Length := Ref_Length + 1;
                  Ref_Buffer (Ref_Length) := N;
               end if;

            when Unmark_Refs =>
               To_Entity := Entity (N);
               To_Unum := Get_Sloc_Unit_Number (Sloc (To_Entity));
               Unmark_Reference (Entity_Association (To_Entity), N);
         end case;
      end if;

      declare
         use Unchecked_Access;

      begin
         Visit_Descendent (Field1 (N));
         Visit_Descendent (Field2 (N));
         Visit_Descendent (Field3 (N));
         Visit_Descendent (Field4 (N));
         Visit_Descendent (Field5 (N));

         if Atree.Has_Extension (N) then
            Visit_Descendent (Field6 (N));
            Visit_Descendent (Field7 (N));
            Visit_Descendent (Field8 (N));
            Visit_Descendent (Field9 (N));
            Visit_Descendent (Field10 (N));
            Visit_Descendent (Field11 (N));
            Visit_Descendent (Field12 (N));
            Visit_Descendent (Field13 (N));
            Visit_Descendent (Field14 (N));
         end if;
      end;
   end Traverse_Node_Hierarchic;

   ---------------
   -- Write_Org --
   ---------------

   procedure Write_Org (The_Etbl : Entity_Table_Acc) is
   begin
      Name_Len := The_Etbl.File_Name'Length;
      Name_Buffer (1 .. Name_Len) := The_Etbl.File_Name.all;
      Name_Buffer (Name_Len - 3 ..
                   Name_Len - 4 + Org_Spec_Suffix'Length) := Org_Spec_Suffix;
      Name_Buffer (Name_Len - 3 + Org_Spec_Suffix'Length) := Ascii.NUL;

      Create_Req_Output;

      Sprint_Node (The_Etbl.Top_Node);

      Write_Eol;
      Set_Standard_Output;
      Close_Xref_Output;
   end Write_Org;

   ---------------------
   -- Write_Body_REQs --
   ---------------------

   procedure Write_Body_REQs is
      Main_Etbl  : Entity_Table_Acc := Loaded_Files (Main_Unit).Etbl;
      Etbl_Tmp_1 : Entity_Table_Acc;
      Etbl_Tmp_2 : Entity_Table_Acc;

      First : Boolean;
      Found : Boolean;

      procedure Open_File (The_Etbl : Entity_Table_Acc);
      --  Open file to write Body_REQs

      procedure Close_File;
      --  Close the file after Body_REQs are written

      procedure Open_File (The_Etbl : Entity_Table_Acc) is
      begin
         --  Here we build the file name of the Body_REQ.
         --  The file name is the file name of the body,
         --  the suffix '.adb' changed into Body_REQ_Suffix !

         Name_Len := The_Etbl.File_Name.all'Length;
         Name_Buffer (1 .. Name_Len) := The_Etbl.File_Name.all;
         Name_Buffer (Name_Len - 3 .. Name_Len - 4 + Body_REQ_Suffix'Length)
           := Body_REQ_Suffix;
         Name_Buffer (Name_Len - 3 + Body_REQ_Suffix'Length) := Ascii.NUL;

         Create_Req_Output;
      end Open_File;

      procedure Close_File is
      begin
         Write_Eol;
         Set_Standard_Output;
         Close_Xref_Output;
      end Close_File;

   --  Start of processing for Write_Body_REQs

   begin
      Etbl_Tmp_1 := Main_Etbl;
      Etbl_Tmp_2 := Main_Etbl;
      First      := True;

      while Etbl_Tmp_1 /= null loop

         --  First we have to mark the body and its subunits across each
         --  others.

         while Etbl_Tmp_2 /= null loop

            Clear_And_Mark_Xrefs (Etbl_Tmp_1, Etbl_Tmp_2, First, False);
            First := False;

            Etbl_Tmp_2 := Etbl_Tmp_2.Successor;
         end loop;

         --  Then remove the entities with no references step by step until
         --  all the remaining entities have one or more references. This must
         --  be done within a loop because after having removed some entities
         --  an earlier defined entity may become removable.

         --  Write_Org (Etbl_Tmp_1);  can be inserted to measure dead code ???

         Found := True;
         while Found loop
            Remove_Entities (Etbl_Tmp_1, Found);
         end loop;

         Open_File (Etbl_Tmp_1);

         --  CC
         --  don't touch the indentation now.
         --  has to be fixed later  ???
         --
         --  Xsprint.Indent := 0;

         Sprint_Node (Etbl_Tmp_1.Top_Node);
         Close_File;

         Etbl_Tmp_1 := Etbl_Tmp_1.Successor;
         Etbl_Tmp_2 := Main_Etbl;
         First      := True;
      end loop;
   end Write_Body_REQs;

   ---------------------
   -- Write_Spec_REQs --
   ---------------------

   procedure Write_Spec_REQs is
      Buffer : String (1 .. 100);
      Length : Integer;
      Found  : Boolean;

      Etbl_Tmp   : Entity_Table_Acc;

      procedure Open_File (I : Unit_Number_Type);
      --  Open file to write Spec_REQs

      procedure Close_File;
      --  Close the file after Spec_REQs are written

      procedure Open_File (I : Unit_Number_Type) is
      begin
         --  Here we build the file name of the Spec_REQ.
         --  The file name is the file name of the withed unit,
         --  the suffix changed from '.ads' into
         --  '.' & withing_unit_name & Spec_REQ_Suffix !

         Get_Name_String (File_Name (Source_Index (Main_Unit)));
         Buffer (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
         Length := Name_Len - 3;

         Get_Name_String (File_Name (Source_Index (I)));
         Name_Buffer (Name_Len + 1 .. Length + Name_Len) :=
            Buffer (1 .. Length);
         Length := Length + Name_Len - 4;

         Name_Buffer (Length + 1 .. Length + Spec_REQ_Suffix'Length)
           := Spec_REQ_Suffix;
         Name_Buffer (Length + Spec_REQ_Suffix'Length + 1) := Ascii.NUL;

         Create_Req_Output;
      end Open_File;

      procedure Close_File is
      begin
         Write_Eol;
         Set_Standard_Output;
         Close_Xref_Output;
      end Close_File;

   --  Start of processing for Write_Spec_REQs

   begin
      for J in Loaded_Files'Range loop

         if Loaded_Files (J).Create_REQ then

            --  First we mark all the entities which are used by the withed
            --  spec itself.

            Clear_And_Mark_Xrefs (Loaded_Files (J).Etbl,
              Loaded_Files (J).Etbl, True, False);

            --  Then we mark all the entities which are used by the main object
            --  (Main_Unit, all predecessors and all successors).

            Etbl_Tmp := Loaded_Files (Main_Unit).Etbl;
            while Etbl_Tmp /= null loop
               Clear_And_Mark_Xrefs
                 (Loaded_Files (J).Etbl, Etbl_Tmp, False, False);
               Etbl_Tmp := Etbl_Tmp.Predecessor;
            end loop;

            Etbl_Tmp := Loaded_Files (Main_Unit).Etbl.Successor;
            while Etbl_Tmp /= null loop
               Clear_And_Mark_Xrefs
                 (Loaded_Files (J).Etbl, Etbl_Tmp, False, False);
               Etbl_Tmp := Etbl_Tmp.Successor;
            end loop;

            --  Then remove the entities with no references step by step until
            --  all the remaining entities have one or more references. This
            --  must be done within a loop because after having removed some
            --  entities an earlier defined entity may become removable.

            Found := True;
            while Found loop
               Remove_Entities (Loaded_Files (J).Etbl, Found);
            end loop;

            Open_File (J);

            --  CC
            --  don't touch the indentation now.
            --  has to be fixed later  ???
            --  Xsprint.Indent := 0;

            Sprint_Node (Cunit (J));
            Close_File;
         end if;

      end loop;
   end Write_Spec_REQs;

   ----------------
   -- Write_Xref --
   ----------------

   procedure Write_Xref (The_Etbl : Entity_Table_Acc) is
      Etbl_Tmp     : Entity_Table_Acc := The_Etbl;
      Etbl_Scanned : Entity_Table_Acc;

   begin
      if Xref_Flag then

         --  File and warning messages output

         Create_Xref_Output (Global_Xref_File);

         if Global_Xref_File then

            --  When we generate a complete Xref file then we scan
            --  all the entity table, but first do the requested units
            --  so that auxiliary units references will get marked.

            while Etbl_Tmp /= null loop
               if Etbl_Tmp.RU then
                  Writ (Etbl_Tmp, Full_Xref, False);
               end if;

               Etbl_Tmp := Etbl_Tmp.Next_Etbl;
            end loop;

            Etbl_Tmp := The_Etbl;
            while Etbl_Tmp /= null loop
               if not Etbl_Tmp.RU and then Etbl_Tmp.Status in Spec_Status then
                  Writ (Etbl_Tmp, Smart_Xref, False);
               end if;

               Etbl_Tmp := Etbl_Tmp.Next_Etbl;
            end loop;

            Close_Xref_Output;

         else
            --  Before writing any Informations in the xref file we write
            --  all the files names that have been loaded during the
            --  previous compilations

            Write_Version;

            for The_Units in Loaded_Files'Range loop
               Write_Files_Info (Loaded_Files (The_Units).Etbl);
            end loop;

            --  When we generate a .ref file for each amin file given in
            --  argument then we scan the main entity table and then all
            --  the successors and predecesssors which are required units.

            Writ (Etbl_Tmp, Full_Xref, True);
            Osint.Close_Xref_Output;

            Etbl_Scanned := Etbl_Tmp;
            while Etbl_Scanned.Predecessor /= null loop
               Etbl_Scanned := Etbl_Scanned.Predecessor;
            end loop;

            while Etbl_Scanned /= null loop
               if Etbl_Scanned.RU and then Etbl_Scanned /= Etbl_Tmp  then
                  Writ (Etbl_Scanned, Full_Xref, False);
               end if;

               Etbl_Scanned := Etbl_Scanned.Successor;
            end loop;

         end if;

      elsif With_Warnings or Entity_Warnings then

         --  Only warning messages output

         while Etbl_Tmp /= null loop
            if Etbl_Tmp.RU then
               Writ (Etbl_Tmp, Full_Only_Standout, False);
            end if;

            Etbl_Tmp := Etbl_Tmp.Next_Etbl;
         end loop;
      end if;

   end Write_Xref;

end Xref;
