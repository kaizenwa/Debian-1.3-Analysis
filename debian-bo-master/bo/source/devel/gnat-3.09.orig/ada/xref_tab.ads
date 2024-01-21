------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             X R E F _ T A B                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.40 $                             --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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

--  This package offers the data type Entity_Table and its methods.
--  Etable is used to store all the definition/use information we need
--  to build a cross reference file or required interfaces.

--  Note that we store here both the Node_Id and the corresponding entitiy
--  information like 'Chars', 'Sloc' or 'Line_Number'. We do this since
--  program trees get lost whenever we start to compile a new unit. Node_Id
--  is only a pointer into program trees.

with Einfo; use Einfo;
with Types; use Types;

package Xref_Tab is

   -------------------
   -- Entity Tables --
   -------------------

   --  An Entity_Table is used to store the information about entities
   --  and their references. We divide the huge amount of information into
   --  several parts: One Entity_Table for each compilation unit.

   --  An Entity_Table is defined as a twodimensional data list. The entities
   --  and their references appear in the same order as in the program tree
   --  (lower Node_Ids first). This represents also the order within the Ada
   --  programs and is important if we want to print the Xref or look for
   --  possible overloading effects.

   --     xxxxxxxxxxxxxxxx
   --     x Entity_Table x
   --     xxxxxxxxxxxxxxxx
   --            I
   --            I
   --            V
   --     xxxxxxxxxxxxxxxx       xxxxxxxxx       xxxxxxxxx
   --     x   Entity     x-----> x  Ref  x-----> x  Ref  x-----> null;
   --     xxxxxxxxxxxxxxxx       xxxxxxxxx       xxxxxxxxx
   --            I
   --            I
   --            V
   --     xxxxxxxxxxxxxxxx       xxxxxxxxx
   --     x   Entity     x-----> x  Ref  x-----> null;
   --     xxxxxxxxxxxxxxxx       xxxxxxxxx
   --            I
   --            I
   --            V
   --           null;

   --  The record behind an Entity_Table is accessible for all clients

   --  1) since there is no real danger in providing this access
   --  2) since it would be very awkward to write all the appropriate
   --     read and write functions.

   --  Of course, the data structures of entities and references are fully
   --  encapsulated - a client can access them only by subprogram calls.

   --  An Entity_Table has the following attributs:

   --   Marked        Flag showing the result of a call of Mark_Xref.
   --                 Set if cross references are found.

   --   RU            Required Unit flag.
   --                 Set if full Xref output is required.

   --   Examined      Flag, set if all entities are installed.

   --   Linked        Flag set if the entity table has been linked with
   --                 its predecessors and successor.

   --   File_Name     Pointer to the fitting file name.

   --   Unit_Name     Pointer to the fitting unit name.

   --   Time_Stamp    Time stamp of the file corresponding to the
   --                 entity table.

   --   Status        Shows if the compilation unit is a simple spec,
   --                 withed spec, a body which acts as its spec, a simple
   --                 body or a subunit.

   --   Kind          Shows if the compilation unit is a procedure,
   --                 function, generic, package, task, subunit or somethin
   --                 else.

   --   Top_Node      Set to the Node_Id of the compilation unit node.

   --   Has_Inlined   Flag indicating if the Entity table contains
   --                 declarations of inlined procedures or functions.

   --   Xref_Written  This flag is set as soon as xref informations are
   --                 for the entity table

   --   Renamed_Etbl  Points to the Etbl where is declared the package
   --                 which is renamed in the current Etbl

   --   First_With    Points to the first with-structure. First_With can
   --                 be used to start hangling through all with-clauses.

   --   First_Include Points to the first Etbl whichj is included in the
   --                 current one (for example a generic spec which is
   --                 instantiated.

   --   First_Child   Points to the first Child spec of a spec.

   --   Predecessor   Points to the immediate predecessor (SU -> B -> S).

   --   Successor     Points to the immediate successor (S -> B -> SU).

   --   Length        Contains the number of installed entities.

   --   First_Entity  Points to the first entity-structure. First_Entity
   --                 can be used to start hangling through all entities.

   --   Last_Entity   Points to the last entity-structure (internal use).

   --   Next_Etbl     Points to the immediate following Entity_Table.


   type Status_Type is (A_Spec, Withed_Spec, Body_As_Spec, A_Body, Sub_Body);
   type Kind_Type is (Proc, Func, Genr, Pack, Tsk, Subunit, Unknown);

   subtype Spec_Status is Status_Type range
     A_Spec .. Withed_Spec;

   subtype Body_Status is Status_Type range
     Body_As_Spec .. Sub_Body;

   type Entity_Table;
   type Entity_Table_Acc is access Entity_Table;

   type With_Clause;
   type With_Acc is access With_Clause;

   type Include;
   type Include_Acc is access Include;

   type Child_Spec;
   type Child_Spec_Acc is access Child_Spec;

   type Withed_Messages is (None, Done, Already_Withed, Withed_Unused);
   --  Type for messages about with warnings
   --   None : This with has not been examined yet

   --   Done : This with has already been examined

   --   Withed_Unused  : This With has already been examined for a previous
   --                     Entity_table and the scan of the_successor has
   --                     shown it was not used in this entity table

   --   Already Withed : Same thing as previously except that the with had
   --                    references in the previou entity table


   --  for Error messages tw new components have been added to this recors :
   --   Prev_Msgs  : Used to store withs warnings for Entities
   --                  which have not been examined yet
   --                  for example, if you check the Withed units a a spec
   --                  if references are found, then for all the successors
   --                  which also with this unit, we store a message
   --                  Already_Withed which willavoid reexamining
   --                  the withed unit for the successors

   --   Extra_Etbl : Used to store the Entity Table where a withed unit
   --                  is already withed
   --                  In the previous example, Extra_Etbl would contain the
   --                  Entity_table corresponding to the Spec

   type With_Clause is record
      Withed_Etbl   : Entity_Table_Acc;
      Next_With     : With_Acc;
      Is_Implicit   : Boolean;
      Prev_Msgs     : Withed_Messages := None;
      Extra_Etbl    : Entity_Table_Acc;
   end record;

   type Include is record
      Included_Etbl : Entity_Table_Acc;
      Next_Include  : Include_Acc;
   end record;

   type Child_Spec is record
      Child_Etbl    : Entity_Table_Acc;
      Next_Child    : Child_Spec_Acc;
   end record;

   --   type Entity_Acc  is private;
   --   type Ref_Acc     is private;
   --   was in the private part ??? put back when private types work???

   ----------------
   -- References --
   ----------------

   --   Marked        Flag showing the result of a call of Mark_Xref.
   --                 Set if the reference fits into the search pattern.

   --   Is_Pragma     Flag, set if the reference is a pragma that doesn't
   --                 use the entity but has to be removed if we declare the
   --                 entity as dead code.

   --   Is_An_Elaborated_With_Clause   Flag set to specify that a with clause
   --                 in a pragma ELABORATE in the Entity_Table Etbl

   --   Ref_Node      The Node_Id of the reference.

   --   Sloc          Points to the source location where the reference
   --                 was found relatively to the file where the reference
   --                 was found.

   --   Line_Number   The line number in which the reference was found.

   --   Col_Number   The column number in which the reference was found.

   --   Etbl          Points to the Entity_Table of the compilation unit
   --                 in which the reference was found.

   --   Next_Ref      Points to the immediate following reference structure.

   type Ref;
   type Ref_Acc is access Ref;

   type Ref is record
      Marked        : Boolean    := False;
      Is_Pragma     : Boolean    := False;
      Ref_Node      : Node_Id;
      Sloc          : Source_Ptr;
      Line_Number   : Logical_Line_Number;
      Col_Number    : Column_Number;
      Etbl          : Entity_Table_Acc;
      Is_An_Elaborated_With_Clause : Boolean := False;
      Next_Ref      : Ref_Acc;
   end record;

   --------------
   -- Entities --
   --------------

   --   Marks         Contains the number of found references (after a call
   --                 of Mark_Xrefs.

   --   Entity_Node   The Entity_Id of the entity.

   --   Entity_Type   The Entity_Kind of the entity.

   --   Entity_Char   The name_id of the entity

   --   Entity_Sloc   Source location of the Entity

   --   Chars         Pointer to the identifier string of the entity.

   --   Line_Number   The line number in which the entity was found.

   --   Col_Number    The column number in which the entity was found.

   --   Real_Line     The line number of the complete declaration of
   --                 private types, incomplete types or deferred constants.
   --                 Set to No_Line_Number to indicate no value stored

   --   Real_Col      The column number of the complete declaration of
   --                 private types, incomplete types or deferred constants
   --                 Set to No_Column_Number to indicate no value stored.

   --   Scope_Path    Pointer to the whole scope path of the entity.

   --   Give_Warning  Flag, set if a warning message should be given in the
   --                 case that we find no reference to this entity.

   --   Is_Direct     Flag set if the declarations is not encapsulated in
   --                 in an other one. This is used in the -x4 mode when
   --                 we generate in the xref file all the entities declared
   --                 in bodies and subunits if the spec declares Inlined
   --                 or generics. In this case we generate only basic
   --                 declarations (For example we don't mind about
   --                 variables declared in subprograms).

   --   Length        Contains the number of references to this entity.

   --   First_Ref     Points to the first reference to the entity.

   --   Last_Ref      Points to the last reference to the entity
   --                 (internal use).

   --   Next_Entity   Points to the immediate following entity structure.

   type An_Entity;
   type Entity_Acc is access An_Entity;

   type An_Entity is record
      Marks         : Natural             := 0;
      Entity_Node   : Entity_Id;
      Entity_Type   : Entity_Kind;
      Entity_Char   : Name_Id;
      Entity_Sloc   : Source_Ptr;
      Chars         : String_Ptr;
      Line_Number   : Logical_Line_Number;
      Col_Number    : Column_Number;
      Real_Line     : Logical_Line_Number := No_Line_Number;
      Real_Col      : Column_Number       := No_Column_Number;
      Scope_Path    : String_Ptr;
      Give_Warning  : Boolean             := True;
      Is_Direct     : Boolean             := False;
      Length        : Natural             := 0;
      First_Ref     : Ref_Acc;
      Last_Ref      : Ref_Acc;
      Next_Entity   : Entity_Acc;
   end record;

   --  End of old private part ???

   type Entity_Table is record
      Marked        : Boolean    := False;
      Unit_Number   : Unit_Number_Type;
      RU            : Boolean    := False;
      Examined      : Boolean    := False;
      Linked        : Boolean    := False;
      File_Name     : String_Ptr;
      Unit_Name     : String_Ptr;
      Time_Stamp    : Time_Stamp_Type;
      Status        : Status_Type;
      Kind          : Kind_Type;
      Top_Node      : Node_Id;
      Has_Inlined   : Boolean    := False;
      Xref_Written  : Boolean    := False;
      Renamed_Etbl  : Entity_Table_Acc;
      First_With    : With_Acc;
      First_Include : Include_Acc;
      First_Child   : Child_Spec_Acc;
      Predecessor   : Entity_Table_Acc;
      Successor     : Entity_Table_Acc;
      Length        : Natural    := 0;
      First_Entity  : Entity_Acc;
      Last_Entity   : Entity_Acc;
      Next_Etbl     : Entity_Table_Acc;
   end record;

   First_Etbl : Entity_Table_Acc;
   Last_Etbl  : Entity_Table_Acc;

   --  Some Flags to control the output

   With_Warnings : Boolean;
   --  If set, warning messages for wrong with clauses appear as part of the
   --  standard error message output (set for XREF levels 1,2,3,4,5).

   Entity_Warnings : Boolean;
   --  If set, warning messages for unused entities appear as part of the
   --  standard error message output (set for XREF levels 2,3,4,5)

   Xref_Flag : Boolean;
   --  If set, a cross reference file is written containing cross-reference
   --  lists for all entities referenced (set for XREF levels 3,4,5).

   Entity_Info_In_Xref : Boolean;
   --  If set, the cross reference file contains Entity_Kind information for
   --  all entities appearing in the listing (set for XREF levels 4,5).

   Include_Inlined     : Boolean;
   --  if set, the cross reference list with contain all entities declared
   --  in bodies and subunits if the spec declares inlined entities or
   --  generics.

   All_Info_In_Xref : Boolean;
   --  If set, the cross-reference file includes references to entities
   --  declared in bodies, and also references to declared entities in a
   --  parent unit (set for XREF level 5).

   Global_Xref_File : Boolean;
   --  If set, then a global xref file is generated.

   Spec_REQs_Flag : Boolean;
   --  If set (-xs switch), the REQs for all withed specs get written
   --  into files.

   Body_REQs_Flag : Boolean;
   --  If set (-xb switch), the REQs for the body and its subunits get
   --  written into files

   -----------------
   -- Subprograms --
   -----------------

   procedure Add_Entity
     (To_Etbl     : in     Entity_Table_Acc;
      Entity_Node : in     Entity_Id;
      New_Entity  : in out Entity_Acc);
   --  Adds an entity to the end of the entity list of To_Etbl.
   --  Finally New_Entity points to the new Entity structure.

   procedure Add_Etbl
     (First_Etbl  : in out Entity_Table_Acc;
      Last_Etbl   : in out Entity_Table_Acc;
      Unit_Number : in     Unit_Number_Type;
      New_Etbl    : in out Entity_Table_Acc);
   --  Maintains the coherence of loaded files and the E_Tables.If Unit_Number
   --  is already in the list only some data gets updated. Otherwise a new
   --  entity table gets added to the end of the entity table list. Finally,
   --  in every case New_Etbl points to the Entity_Table which corresponds to
   --  Unit_Number.

   procedure Add_Reference
     (To_Entity :  Entity_Acc;
      New_Etbl  :  Entity_Table_Acc;
      New_Ref   :  Node_Id);
   --  Adds a reference to the end of the reference list of the given entity.
   --  New_Etbl has to point to the entity table in which New_Ref appears.

   procedure Add_With
     (To_Etbl  : Entity_Table_Acc;
      New_Etbl : Entity_Table_Acc;
      Is_Implicit : Boolean := False);
   --  If Withed_Etbl is not already in the list, then a new With_Clause gets
   --  added to the end of the with list of To_Etbl

   procedure Clear_And_Mark_Xrefs
     (Home_Etbl   : Entity_Table_Acc;
      Target_Etbl : Entity_Table_Acc;
      First_Pass  : Boolean;
      In_Xref     : Boolean;
      Count_Marks : Boolean := True);

   --  Looks in Home_Etbl for references with a source location in Target_Etbl.
   --  If one or more such references are found then the fitting references
   --  and the fitting entity in Home_Etbl as well as Target_Etbl get marked.
   --  All other nodes of Home_Etbl get unmarked.

   procedure Delete_Table (Old_Etbl : Entity_Table_Acc);
   --  Not implemented yet.
   --  Originally to deallocate the space of an Entity_Table object.

   function Entity_Node (The_Entity : Entity_Acc) return Entity_Id;
   --  Returns the Entity_Id of the entity object to which The_Entity points.
   --  If The_Entity points to 'null' then Empty is returned.

   function Entity_Type (The_Entity : Entity_Acc) return Entity_Kind;
   --  Returns the Entity_Kind of the entity object to which The_Entity
   --  points. If The_Entity points to 'null' then E_Void is returned.

   function First (The_Etbl : Entity_Table_Acc) return Entity_Id;
   --  Returns the first entity of the given Entity_Table.

   function First (The_Entity : Entity_Acc) return Ref_Acc;
   --  Returns the first reference of the given entity.

   function Give_Warning (The_Entity : Entity_Acc) return Boolean;
   --  Tests if a warning message should appear if the given
   --  entity is unused.

   function In_E_List
     (The_Etbl   : Entity_Table_Acc;
      The_Entity : Entity_Id) return Entity_Acc;
   --  Checks if the given entity is already in the entity list.
   --  If the entity is found the pointer to it is returned.
   --  If the entity is not in the list 'null' is returned.

   function In_Ref_List
     (The_Entity : Entity_Acc;
      The_Ref    : Node_Id) return Boolean;
   --  Checks if the given reference is already in the reference list
   --  corresponding to the given entity.
   --  'True' is returned if the entity is found.
   --  'False' is returned if The_Entity points to 'null' or The_Entity
   --  is not in the list .

   function In_With_List
     (Home_Etbl   : Entity_Table_Acc;
      Target_Etbl : Entity_Table_Acc) return Boolean;
   --  Checks if Home_Etbl is withed by Target_Etbl.
   --  If Target_Etbl is not in the list then 'False' is returned.

   function Is_Null (The_Entity : Entity_Acc) return Boolean;
   --  Tests if the given Entity_Acc points to 'null'.

   function Is_Null (The_Ref : Ref_Acc) return Boolean;
   --  Tests if the given Ref_Acc points to 'null'.

   function Is_Pragma (The_Ref : Ref_Acc) return Boolean;
   --  Tests if the given Ref_Acc is a pragma reference.

   procedure Mark_Entity (Old_Entity : Entity_Acc);
   --  Marks an entity of the entity list as used.
   --  This procedure is sometimes called to prevent the multi pass removal
   --  process from removing an unused entity twice.

   function Number_Of_Marks (The_Entity : Entity_Acc) return Natural;
   --  Returns the number of the marked references in the given entity.
   --  If The_Entity points to 'null' then 0 is returned.

   function Number_Of_Refs (The_Entity : Entity_Acc) return Natural;
   --  Returns the length of the reference list of the given entity.
   --  If The_Entity points to 'null' then 0 is returned.

   procedure Mark_Withed_Entities (The_Etbl : Entity_Table_Acc);
   --  Used for a withed spec to mark the entities which are used by its
   --  clients. If the client uses at least one entity then the client entity
   --  table also gets marked to signal a 'dead with clause'.

   function Next (The_Entity : Entity_Acc) return Entity_Acc;
   --  Returns the entity which follows the given entity in Entity_Table.
   --  'null' is returned if the given entity is the last entity in the
   --  Entity_Table.

   function Next (The_Ref : Ref_Acc) return Ref_Acc;
   --  Returns the reference which follows the given reference in the list.
   --  'null' is returned if the given reference is the last entity in the
   --  list.

   function The_Node (The_Ref : Ref_Acc) return Node_Id;
   --  Returns the Node_Id of the given reference.

   procedure Unmark_Entity
     (The_Table  : Entity_Table_Acc;
      Old_Entity : Entity_Acc);
   --  Unmarks Old_Entity of the entity list.
   --  Nothing is done if Old_Entity or The_Table point to 'null'.

   procedure Unmark_Reference (The_Entity : Entity_Acc;
                               Old_Ref    : Node_Id);
   --  Unmarks a reference node of the reference list of The_Entity.
   --  Nothing is done if The_Entity points to 'null' or Old_Ref is
   --  not found in the list.

   procedure Update_Entity
     (To_Etbl     : in     Entity_Table_Acc;
      Entity_Node : in     Entity_Id;
      New_Entity  : in out Entity_Acc);
   --  If the entity is already in the list only the Entity_Id is updated.
   --  Otherwise Add_Entity is called. Used to sychnonize old Entity_Ids
   --  with new Unique_Names. Finally, in every case New_Entity points to
   --  the Entity structure which corresponds to Entity_Node.

   procedure Update_Reference
     (To_Entity :  Entity_Acc;
      New_Etbl  :  Entity_Table_Acc;
      New_Ref   :  Node_Id);
   --  If the reference is already in the list only the Node_Id is updated.
   --  Otherwise Add_Reference is called..
   --  Used to sychnonize old Node_Ids with the new Ids - only for creating
   --  the Spec_REQs.

   type Output_Level is
     (Full_Xref, Smart_Xref, Full_Only_Standout);
   --  Used only for Writ!
   --
   --  Full_Xref:        Empty reference lists cause WARNING messages.
   --  Smart_Xref:       The output of empty reference lists is supressed.
   --  ??? following documentation does not correspond to declaration
   --  Xref:             Both standard output and file output.
   --  Only_Standout:    Only standard output

   procedure Writ
     (The_Etbl   : Entity_Table_Acc;
      Level      : Output_Level;
      First_File : Boolean);
   --  Writes the information stored in the Entity_Table to the Xref Output
   --  file. Note: The output of anonymous entities is always supressed.

   procedure Write_Files_Info (The_Etbl : Entity_Table_Acc);
   --  Writes at the beginning of the .xr? file the name and the time
   --  stamp of the Entity table.
   --  Above comment knows too much about file names ???

   procedure Write_Version;
   --  Writes at the beginning of the .xr? files the version of sgnat.
   --  Anove comment knows too much about file names ???

private
   pragma Inline (Add_Entity);
   pragma Inline (Add_Etbl);
   pragma Inline (Add_Reference);
   pragma Inline (Add_With);
   pragma Inline (Clear_And_Mark_Xrefs);
   pragma Inline (Delete_Table);
   pragma Inline (Entity_Node);
   pragma Inline (Entity_Type);
   pragma Inline (First);
   pragma Inline (Give_Warning);
   pragma Inline (In_E_List);
   pragma Inline (In_Ref_List);
   pragma Inline (In_With_List);
   pragma Inline (Is_Null);
   pragma Inline (Is_Pragma);
   pragma Inline (Mark_Entity);
   pragma Inline (Number_Of_Marks);
   pragma Inline (Next);
   pragma Inline (The_Node);
   pragma Inline (Unmark_Entity);
   pragma Inline (Unmark_Reference);
   pragma Inline (Update_Entity);
   pragma Inline (Update_Reference);

end Xref_Tab;
