------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.17 $                             --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with System.Task_Specific_Data; use System.Task_Specific_Data;
with System.Exception_Table;    use System.Exception_Table;
with Unchecked_Conversion;

package body Ada.Exceptions is

   procedure Internal_Raise (X : Exception_Id);
   pragma Import (C, Internal_Raise, "__gnat_raise_with_msg");

   type Buffer_Ptr is access SSL.Exception_Message_Buffer;
   --  A thin pointer to String

   function To_Buffer_Ptr is
     new Unchecked_Conversion (System.Address, Buffer_Ptr);
   --  Conversion from address to string access for exception msg manipulation

   ------------------------
   -- Exception_Identity --
   ------------------------

   function Exception_Identity
     (X    : Exception_Occurrence)
      return Exception_Id
   is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      return X.Id;
   end Exception_Identity;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   function Exception_Information (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      if X.Msg_Length < 1 then
         return Exception_Name (X) & " : <no message>";
      else
         return Exception_Name (X) & " : " & X.Msg (1 .. X.Msg_Length);
      end if;
   end Exception_Information;

   -----------------------
   -- Exception_Message --
   -----------------------

   function  Exception_Message (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      return X.Msg (1 .. X.Msg_Length);
   end Exception_Message;

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name (X : Exception_Id) return String is
   begin
      if X = Null_Id then
         raise Constraint_Error;
      end if;

      return X.Full_Name.all (1 .. X.Name_Length - 1);
   end Exception_Name;

   function Exception_Name (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      return X.Id.Full_Name.all (1 .. X.Id.Name_Length - 1);
   end Exception_Name;

   ---------------------------
   -- Exception_Name_Simple --
   ---------------------------

   function Exception_Name_Simple (X : Exception_Occurrence) return String is
      P : Natural;

   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      P := X.Id.Name_Length;
      while P > 1 loop
         exit when X.Id.Full_Name.all (P - 1) = '.';
         P := P - 1;
      end loop;

      return X.Id.Full_Name.all (P .. X.Id.Name_Length - 1);
   end Exception_Name_Simple;

   ---------------------------------------
   -- Exception_Occurrence_Access_Input --
   ---------------------------------------

   function Exception_Occurrence_Access_Input
     (Stream : access Ada.Streams.Root_Stream_Type'Class)
      return   Exception_Occurrence_Access
   is
      Max  : Natural      := Natural'Input      (Stream);
      Name : String       := String'Input       (Stream);
      Len  : Natural      := Natural'Input      (Stream);
      Msg  : String       := String'Input       (Stream);

      Item : Exception_Occurrence_Access := new Exception_Occurrence (Max);

   begin
      Item.Id := Exception_Id (Internal_Exception (Name));

      if Msg'Length /= Len or else Len > Max then
         raise Data_Error;
      end if;

      Item.Msg_Length := Len;
      Item.Msg        := Msg;

      return Item;

   end Exception_Occurrence_Access_Input;

   -------------------------------
   -- Exception_Occurrence_Read --
   -------------------------------

   procedure Exception_Occurrence_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Exception_Occurrence)
   is
      Name : constant String := String'Input (Stream);

   begin
      --  A null string is the representation of Null_Id

      if Name = "" then
         Item.Id         := Null_Id;
         Item.Msg_Length := 0;

      --  Otherwise we have a non-null exception

      else
         Item.Id := Exception_Id (Internal_Exception (Name));

         declare
            Msg : constant String := String'Input (Stream);

         begin
            --  Silently truncate message if it does not fit

            Item.Msg_Length := Natural'Min (Msg'Length, Item.Max_Length);
            Item.Msg (1 .. Item.Msg_Length) := Msg (1 .. Item.Msg_Length);
         end;
      end if;
   end Exception_Occurrence_Read;

   --------------------------------
   -- Exception_Occurrence_Write --
   --------------------------------

   procedure Exception_Occurrence_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Exception_Occurrence)
   is
   begin
      --  We use a null string as the representation of Null_Id

      if Item.Id = Null_Id then
         String'Output (Stream, "");

      --  For non-null exception output exception name and message

      else
         String'Output (Stream, Exception_Name (Item.Id));
         String'Output (Stream, Item.Msg (1 .. Item.Msg_Length));
      end if;
   end Exception_Occurrence_Write;

   ---------------------
   -- Raise_Exception --
   ---------------------

   --  Implementation Note:

   --  For now, we truncate messages that are too large. We could deal
   --  with larger messages later on by dynamically freeing the buffer
   --  (Exception_Message_Buffer) and reallocating it as required. This
   --  would require the size of the actual buffer to be saved in the TSD.

   procedure Raise_Exception
     (E       : in Exception_Id;
      Message : in String       := "")
   is
      Len : constant Natural :=
              Natural'Min
                (Message'Length, SSL.Exception_Message_Buffer'Length);

   begin
      if E = Null_Id then
         null;

      else
         Set_Message_Length (Len);
         To_Buffer_Ptr (Get_Message_Addr).all (1 .. Len) :=
           Message (Message'First .. Message'First + Len - 1);
         Internal_Raise (E);
      end if;
   end Raise_Exception;

   ------------------------
   -- Reraise_Occurrence --
   ------------------------

   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      if X.Id = Null_Id then
         return;

      else
         Raise_Exception (X.Id, X.Msg (1 .. X.Msg_Length));
      end if;
   end Reraise_Occurrence;

   ---------------------
   -- Save_Occurrence --
   ---------------------

   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : in  Exception_Occurrence)
   is
   begin
      Target.Id := Source.Id;

      --  Case of truncation required

      if Target.Max_Length < Source.Msg_Length then
         Target.Msg_Length := Target.Max_Length;
         Target.Msg        := Source.Msg (1 .. Target.Max_Length);

      --  Case of no truncation required

      else
         Target.Msg_Length := Source.Msg_Length;
         Target.Msg (1 .. Target.Msg_Length) :=
           Source.Msg (1 .. Target.Msg_Length);
      end if;
   end Save_Occurrence;

   function Save_Occurrence
     (Source : in Exception_Occurrence)
      return   Exception_Occurrence_Access
   is
      X : Exception_Occurrence_Access;

   begin
      X := new Exception_Occurrence (Source.Msg_Length);

      X.Id         := Source.Id;
      X.Msg_Length := Source.Msg_Length;
      X.Msg        := Source.Msg;

      return X;
   end Save_Occurrence;

   ------------------------------
   -- Set_Exception_Occurrence --
   ------------------------------

   procedure Set_Exception_Occurrence (Occ : Exception_Occurrence_Access) is
      use System.Task_Specific_Data;

      Len : constant Natural := Get_Message_Length;

   begin
      Occ.Msg_Length := Len;
      Occ.Msg (1 .. Len) := To_Buffer_Ptr (Get_Message_Addr).all (1 .. Len);
   end Set_Exception_Occurrence;
end Ada.Exceptions;
