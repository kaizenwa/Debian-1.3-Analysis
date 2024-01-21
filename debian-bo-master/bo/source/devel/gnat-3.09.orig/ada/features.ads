------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F E A T U R E S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
--                                                                          --
--        Copyright (C) 1992,1993,1994 Free Software Foundation, Inc.       --
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

--  This package contains the routines for handling -x9 output and generating
--  the resulting listing of Ada 95 features used in the main source program(s)

with Types; use Types;

package Features is

   --  List of names of features. These names are listed in the output

   --  Note: entries marked ??? are not yet dealt with

   type Feature_Name is (
      Tagged_Types,
      Class_Wide_Types,
      Abstract_Types,
      Abstract_Subprograms,
      Tagged_Type_Conversion,
      Access_To_Subprogram_Types,
      Access_Parameters,
      General_Access_Types,
      Access_To_Constant_Types,
      Controlled_Types,
      Generic_Formal_Private_Types,
      Aliased_Objects,
      Access_Discriminants,
      Extension_Aggregates,

      Protected_Units_And_Operations,
      Exception_Handler_In_Accept,
      Requeue_Statement,
      Delay_Until,
      Asynchronous_Select,
      Task_Discriminants,

      Child_Units,
      Private_Child,
      Child_Renaming,                             --  ???
      Generic_Unit_Child,
      Generic_Instantiation_Child,                --  ???
      Library_Unit_Renaming,
      Relaxation_Of_Subunit_Naming,               --  ???

      Wide_Characters_And_Strings,
      Modular_Integer_Types,
      Decimal_Fixed_Point,
      Access_To_Protected_Subprogram,             --  ???
      Out_Parameters_Read,                        --  ???
      Subprogram_Bodies_By_Renaming,
      Deferred_Constants_Of_Any_Type,
      Limited_Record_Types,
      Use_Type,
      Generic_Unit_Renaming,                      --  ???
      Exception_Parameter,
      Generic_Matching_For_Access_Types,          --  ???
      Generic_Formal_Derived_Types,
      Generic_Formal_Packages,
      Generic_Formal_Decimal_Types,
      New_Representation_Clauses,
      New_Representation_Pragmas,
      User_Defined_Storage_Pools,
      Appending,                                  --  ???
      Modular_Input_Output,                       --  ???
      Generic_Contract_Rules,

      Unknown_Discriminant_Parts,
      Type_Freezing_Rules,                        --  ???
      Overloading_Of_Equality,                    --  ???
      Decimal_Input_Output,                       --  ???
      Exception_Choices,
      Image_Attribute_For_Real,
      Value_Attribute_For_Real,
      Pred_Succ_Attribute_For_Real,
      New_Attributes,
      Unconstrained_Variables,
      Latin_1,
      Inheritance_At_Local_Derivation,
      New_Pragmas,
      Non_Simple_Expressions,
      Later_Declaration_Ordering,
      New_Uses_Of_Others,
      Base_Attribute_In_Subtype_Mark,

      Implementation_Dependent_Attributes,
      Implementation_Dependent_Pragmas);

   subtype Code_Name is String (1 .. 4);

   type Code_Name_Array is array (Feature_Name) of Code_Name;

   Code_Names : constant Code_Name_Array := Code_Name_Array'(
      Tagged_Types                         => "A010",
      Class_Wide_Types                     => "A020",
      Abstract_Types                       => "A030",
      Abstract_Subprograms                 => "A040",
      Tagged_Type_Conversion               => "A050",
      Access_To_Subprogram_Types           => "A060",
      Access_Parameters                    => "A070",
      General_Access_Types                 => "A080",
      Access_To_Constant_Types             => "A081",
      Controlled_Types                     => "A090",
      Generic_Formal_Private_Types         => "A100",
      Aliased_Objects                      => "A110",
      Access_Discriminants                 => "A120",
      Extension_Aggregates                 => "A130",

      Protected_Units_And_Operations       => "B010",
      Exception_Handler_In_Accept          => "B020",
      Requeue_Statement                    => "B030",
      Delay_Until                          => "B040",
      Asynchronous_Select                  => "B050",
      Task_Discriminants                   => "B060",

      Child_Units                          => "C010",
      Private_Child                        => "C020",
      Child_Renaming                       => "C030",
      Generic_Unit_Child                   => "C040",
      Generic_Instantiation_Child          => "C050",
      Library_Unit_Renaming                => "C060",
      Relaxation_Of_Subunit_Naming         => "C070",

      Wide_Characters_And_Strings          => "D010",
      Modular_Integer_Types                => "D020",
      Decimal_Fixed_Point                  => "D030",
      Access_To_Protected_Subprogram       => "D040",
      Out_Parameters_Read                  => "D050",
      Subprogram_Bodies_By_Renaming        => "D060",
      Deferred_Constants_Of_Any_Type       => "D070",
      Limited_Record_Types                 => "D080",
      Use_Type                             => "D090",
      Generic_Unit_Renaming                => "D110",
      Exception_Parameter                  => "D120",
      Generic_Matching_For_Access_Types    => "D130",
      Generic_Formal_Derived_Types         => "D140",
      Generic_Formal_Packages              => "D160",
      Generic_Formal_Decimal_Types         => "D161",
      New_Representation_Clauses           => "D170",
      New_Representation_Pragmas           => "D180",
      User_Defined_Storage_Pools           => "D190",
      Appending                            => "D210",
      Modular_Input_Output                 => "D220",
      Generic_Contract_Rules               => "D250",
      Unknown_Discriminant_Parts           => "D260",
      Type_Freezing_Rules                  => "D270",
      Overloading_Of_Equality              => "D280",
      Decimal_Input_Output                 => "D290",
      Exception_Choices                    => "D300",
      Image_Attribute_For_Real             => "D310",
      Value_Attribute_For_Real             => "D311",
      Pred_Succ_Attribute_For_Real         => "D312",
      New_Attributes                       => "D315",
      Unconstrained_Variables              => "D320",
      Latin_1                              => "D350",
      Inheritance_At_Local_Derivation      => "D360",
      New_Pragmas                          => "D370",
      Non_Simple_Expressions               => "D380",
      Later_Declaration_Ordering           => "D390",
      New_Uses_Of_Others                   => "D400",
      Base_Attribute_In_Subtype_Mark       => "D420",

      Implementation_Dependent_Attributes  => "E010",
      Implementation_Dependent_Pragmas     => "E020");

   Features_On : Boolean := False;
   --  This flag is initialized by Par/Sem to be True if feature usage is to be
   --  collected (i.e. Xref_Flag_9 is set, and a main unit is being processed.
   --  Par and Sem take care of saving/restoring the flag so that recursion
   --  involving compilation of other units does not upset the proper setting.

   procedure Initialize;
   --  Initialize output of feature collection, must be called before any calls
   --  are made to Note_Feature or Note_With (sets up the required tables etc.)

   procedure Note_Feature (F : Feature_Name; Loc : Source_Ptr);
   --  Note use of feature F at source location Loc. This procedure should only
   --  be called if the Features_On flag is set to True.

   procedure Note_With (U : Unit_Name_Type; Loc : Source_Ptr);
   --  Note with of Ada or System child unit at location Loc. This procedure
   --  Should only be called if the Features_On flag is set to True.

   procedure Finalize;
   --  Outputs list of features used, if feature collection is enabled.

end Features;
