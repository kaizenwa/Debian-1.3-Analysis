------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             R E S T R I C T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
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

with Atree;    use Atree;
with Casing;   use Casing;
with Errout;   use Errout;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Output;   use Output;
with Sem;      use Sem;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Tbuild;   use Tbuild;

package body Restrict is

   ---------------------------
   -- Check_Restricted_Unit --
   ---------------------------

   procedure Check_Restricted_Unit (U : Unit_Name_Type; Loc : Source_Ptr) is
   begin
      if Any_Restrictions and then Is_Spec_Name (U) then
         declare
            Fnam : constant File_Name_Type := Get_File_Name (U);

         begin
            if not Is_Predefined_File_Name (Fnam) then
               return;

            --  Ada child unit spec, needs checking against list

            else
               --  Pad name to 8 characters with blanks

               Get_Name_String (Fnam);
               Name_Len := Name_Len - 4;

               while Name_Len < 8 loop
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := ' ';
               end loop;

               for J in Unit_Array'Range loop
                  if Restrictions (Unit_Array (J).Res_Id)
                    and then Name_Buffer (3 .. 8) = Unit_Array (J).Filenm
                  then
                     declare
                        S : constant String :=
                              Restriction_Id'Image (Unit_Array (J).Res_Id);

                     begin
                        Name_Buffer (1 .. S'Last) := S;
                        Name_Len := S'Length;
                        Set_Casing (All_Lower_Case);
                        Error_Msg_Name_1 := Name_Enter;
                        Error_Msg_Unit_1 := U;
                        Error_Msg
                          ("loading unit$ violates restriction %", Loc);
                        return;
                     end;
                  end if;
               end loop;
            end if;
         end;
      end if;
   end Check_Restricted_Unit;

   -----------------------------------------
   -- Check_Restriction (Identifier Case) --
   -----------------------------------------

   procedure Check_Restriction (R : Restriction_Id; N : Node_Id) is
   begin
      if Restrictions (R) then
         declare
            S : constant String := Restriction_Id'Image (R);

         begin
            Name_Buffer (1 .. S'Last) := S;
            Name_Len := S'Length;
            Set_Casing (All_Lower_Case);
            Error_Msg_Name_1 := Name_Enter;
            Error_Msg_N ("violation of restriction %", N);
         end;
      end if;
   end Check_Restriction;

   -------------------------------------------------------------
   -- Check_Restriction_Parameter (Identifier Parameter Case) --
   -------------------------------------------------------------

   procedure Check_Restriction
     (R : Restriction_Parameter_Id;
      N : Node_Id)
   is
   begin
      if Restriction_Parameters (R) = Uint_0 then
         declare
            Loc : constant Source_Ptr := Sloc (N);
            S   : constant String :=
                    Restriction_Parameter_Id'Image (R);

         begin
            Error_Msg_NE
              ("& will be raised at runtime?!", N, Standard_Storage_Error);
            Name_Buffer (1 .. S'Last) := S;
            Name_Len := S'Length;
            Set_Casing (All_Lower_Case);
            Error_Msg_Name_1 := Name_Enter;
            Error_Msg_N ("violation of restriction %?!", N);

            Insert_Action (N,
              Make_Raise_Statement (Loc,
                Name => New_Occurrence_Of (Standard_Storage_Error, Loc)));
         end;
      end if;
   end Check_Restriction;

   ------------------------------------------------------------------------
   -- Check_Restriction_Parameter (Identifier Parameter Case with Count) --
   ------------------------------------------------------------------------

   procedure Check_Restriction
     (R : Restriction_Parameter_Id;
      V : Uint;
      N : Node_Id)
   is
   begin
      if Restriction_Parameters (R) >= Uint_0
        and then V > Restriction_Parameters (R)
      then
         declare
            S : constant String := Restriction_Parameter_Id'Image (R);

         begin
            Name_Buffer (1 .. S'Last) := S;
            Name_Len := S'Length;
            Set_Casing (All_Lower_Case);
            Error_Msg_Name_1 := Name_Enter;
            Error_Msg_N ("maximum value for restriction % exceeded", N);
         end;
      end if;
   end Check_Restriction;

   ------------------------
   -- Get_Restriction_Id --
   ------------------------

   function Get_Restriction_Id
     (N    : Name_Id)
      return Restriction_Id
   is
      J : Restriction_Id;

   begin
      Get_Name_String (N);
      Set_Casing (All_Upper_Case);

      J := Restriction_Id'First;
      while J /= Not_A_Restriction_Id loop
         declare
            S : constant String := Restriction_Id'Image (J);

         begin
            exit when S = Name_Buffer (1 .. Name_Len);
         end;

         J := Restriction_Id'Succ (J);
      end loop;

      return J;
   end Get_Restriction_Id;

   ----------------------------------
   -- Get_Restriction_Parameter_Id --
   ----------------------------------

   function Get_Restriction_Parameter_Id
     (N    : Name_Id)
      return Restriction_Parameter_Id
   is
      J : Restriction_Parameter_Id;

   begin
      Get_Name_String (N);
      Set_Casing (All_Upper_Case);

      J := Restriction_Parameter_Id'First;
      while J /= Not_A_Restriction_Parameter_Id loop
         declare
            S : constant String := Restriction_Parameter_Id'Image (J);

         begin
            exit when S = Name_Buffer (1 .. Name_Len);
         end;

         J := Restriction_Parameter_Id'Succ (J);
      end loop;

      return J;
   end Get_Restriction_Parameter_Id;

end Restrict;
