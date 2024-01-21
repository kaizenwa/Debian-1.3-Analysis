------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;
with System.Standard_Library;

package Ada.Exceptions is

   type Exception_Id is private;
   Null_Id : constant Exception_Id;

   type Exception_Occurrence is limited private;
   type Exception_Occurrence_Access is access all Exception_Occurrence;

   Null_Occurrence : constant Exception_Occurrence;

   --  Note: we altered the order to simplify the processing for the intrinsic
   --  routine Exception_Name, which needs to call the Exception_Occurrence
   --  version of the routine here, and Rtsfind will find the first matching
   --  occurrence when we have overloaded routines.

   function Exception_Identity (X : Exception_Occurrence) return Exception_Id;
   function Exception_Name     (X : Exception_Occurrence) return String;
   --  Same as Exception_Name (Exception_Identity (X))

   function Exception_Name (X : Exception_Id) return String;

   procedure Raise_Exception (E : in Exception_Id; Message : in String := "");
   --  Note: it would be really nice to give a pragma No_Return for this
   --  procedure, but it would be wrong, since Raise_Exception does return
   --  if given the null exception. However we do special case the name in
   --  the test in the compiler for issuing a warning for a missing return
   --  after this call. Program_Error seems reasonable enough in such a case.

   function  Exception_Message    (X : Exception_Occurrence) return String;
   procedure Reraise_Occurrence   (X : Exception_Occurrence);

   function Exception_Information (X : Exception_Occurrence) return String;

   procedure Save_Occurrence
     (Target :    out Exception_Occurrence;
      Source : in     Exception_Occurrence);

   function Save_Occurrence
     (Source : in Exception_Occurrence)
      return Exception_Occurrence_Access;

private
   package SSL renames System.Standard_Library;

   type Exception_Id is access all SSL.Exception_Data;
   Null_Id : constant Exception_Id := null;

   subtype Nat is Natural range 0 .. SSL.Exception_Message_Buffer'Last;
   --  ??? replace Nat by Natural when limited types with discriminants
   --  are implemented properly

   --  Exception_Occurrence is defined as a limited record so that when a
   --  default instance is allocated, the default discriminant value is used
   --  to determine the length (given that even the full view is limited, we
   --  do not need to allocate the maximum length).

   type Exception_Occurrence
     (Max_Length : Nat := SSL.Exception_Msg_Max)
   is limited record
      Id         : Exception_Id;
      Msg_Length : Natural;
      Msg        : String (1 .. Max_Length);
   end record;

   procedure Set_Exception_Occurrence (Occ : Exception_Occurrence_Access);
   pragma Export (C, Set_Exception_Occurrence, "__set_except_occ");
   --  Procedure called directly by gigi to copy in the exception occurrence
   --  the exception message kept in the task-Specific data.

   --  Note: the whole issue of stream attributes and exception occurrences
   --  is rather mixed up. We implement 'Read and 'Write as required in the
   --  RM, with the addition that Read is allowed to truncate a long message
   --  to the allowed 200 characters (just like Save_Occurrence).

   procedure Exception_Occurrence_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Exception_Occurrence);

   procedure Exception_Occurrence_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Exception_Occurrence);

   for Exception_Occurrence'Read  use Exception_Occurrence_Read;
   for Exception_Occurrence'Write use Exception_Occurrence_Write;

   --  We do not implement 'Input and 'Output for Exception_Occurrence,
   --  since they are not required, and 'Input in particular is not easy
   --  to implement (more properly is impossible), since there is no way
   --  to write a 'Input function for a limited record that does not have
   --  a serious storage leak problem.

   --  Instead, analogous to how Save_Occurrence works, we implement a
   --  special 'Input routine for Exception_Occurrence_Access that returns
   --  a pointer to an Exception_Occurrence that can hold a message of any
   --  length as originally written by Exception_Occurrence'Write.

   function Exception_Occurrence_Access_Input
     (Stream : access Ada.Streams.Root_Stream_Type'Class)
      return   Exception_Occurrence_Access;

   for Exception_Occurrence_Access'Input use Exception_Occurrence_Access_Input;

   --  Note: the following definition of Null_Occurrence is not legal Ada. In
   --  fact, given the clear requirement to make Exception_Occurrence a limited
   --  record (see above note), there is no way to complete Null_Occurrence in
   --  a legal way. There is a special kludge in the compiler to permit this
   --  particular bit of illegal Ada!

   Null_Occurrence : constant Exception_Occurrence := (
     Max_Length => 0,
     Id         => Null_Id,
     Msg_Length => 0,
     Msg        => "");

   function Exception_Name_Simple (X : Exception_Occurrence) return String;
   --  Like Exception_Name, but returns the simple non-qualified name of
   --  the exception. This is used to implement the Exception_Name function
   --  in Current_Exceptions (the DEC compatible unit). It is called from
   --  the compiler generated code (using Rtsfind, which does not respect
   --  the private barrier, so we can place this function in the private
   --  part where the compiler can find it, but the spec is unchanged.)

end Ada.Exceptions;
