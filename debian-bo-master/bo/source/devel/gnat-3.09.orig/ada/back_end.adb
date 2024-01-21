------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            B A C K _ E N D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.16 $                             --
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

--  Call the back end with all the information needed

with Elists;  use Elists;
with Lib;     use Lib;
with Namet;   use Namet;
with Nlists;  use Nlists;
with Sinput;  use Sinput;
with Stringt; use Stringt;
with System;  use System;
with Atree;   use Atree;
with Types;   use Types;

procedure Back_End is

   --  The File_Record type has a lot of components that are meaningless
   --  to the back end, so a new record is created here to contain the
   --  needed information for each file.

   type Needed_File_Info_Type is record
      File_Name        : File_Name_Type;
      First_Sloc       : Source_Ptr;
      Last_Sloc        : Source_Ptr;
      Num_Source_Lines : Nat;
   end record;

   File_Info_Array :
     array (Main_Unit .. Last_Unit) of Needed_File_Info_Type;

   procedure gigi (
      gnat_root        : Int;
      max_gnat_node    : Int;
      number_name      : Nat;
      nodes_ptr        : Address;
      next_node_ptr    : Address;
      prev_node_ptr    : Address;
      elists_ptr       : Address;
      elmts_ptr        : Address;
      names_ptr        : Address;
      strings_ptr      : Address;
      string_chars_ptr : Address;
      list_headers_ptr : Address;
      name_chars_ptr   : Address;
      number_units     : Int;
      file_info_ptr    : Address);

   pragma Import (C, gigi);

   S : Source_File_Index;

begin
   for J in Main_Unit .. Last_Unit loop
      S := Source_Index (J);
      File_Info_Array (J).File_Name        := File_Name (S);
      File_Info_Array (J).First_Sloc       := Source_Text (S)'First;
      File_Info_Array (J).Last_Sloc        := Source_Text (S)'Last;
      File_Info_Array (J).Num_Source_Lines := Num_Source_Lines (S);
   end loop;

   gigi (
      gnat_root        => Int (Cunit (Main_Unit)),
      max_gnat_node    => Int (Last_Node_Id - First_Node_Id + 1),
      number_name      => Name_Entries_Count,
      nodes_ptr        => Nodes_Address,
      next_node_ptr    => Next_Node_Address,
      prev_node_ptr    => Prev_Node_Address,
      elists_ptr       => Elists_Address,
      elmts_ptr        => Elmts_Address,
      names_ptr        => Name_Entries_Address,
      strings_ptr      => Strings_Address,
      string_chars_ptr => String_Chars_Address,
      list_headers_ptr => Lists_Address,
      name_chars_ptr   => Name_Chars_Address,
      number_units     => Num_Units,
      file_info_ptr    => File_Info_Array'Address);
end Back_End;
