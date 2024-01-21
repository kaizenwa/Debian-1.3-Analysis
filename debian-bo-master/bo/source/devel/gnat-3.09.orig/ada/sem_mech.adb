------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ M E C H                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $                              --
--                                                                          --
--             Copyright (C) 1996 Free Software Foundation, Inc.            --
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
with Sem;    use Sem;
with Sinfo;  use Sinfo;
with Snames; use Snames;

package body Sem_Mech is

   --------------------
   -- Set_Mechanisms --
   --------------------

   procedure Set_Mechanisms (E : Entity_Id) is
      Formal : Entity_Id;
      Typ    : Entity_Id;

   begin
      --  Skip this processing if inside a generic template. Not only is
      --  it uneccessary (since neither extra formals nor mechanisms are
      --  relevant for the template itself), but at least at the moment,
      --  procedures get frozen early inside a template so attempting to
      --  look at the formal types does not work too well if they are
      --  private types that have not been frozen yet.

      if Inside_A_Generic then
         return;
      end if;

      --  Loop through formals

      Formal := First_Formal (E);
      while Present (Formal) loop

         if Mechanism (Formal) = Default_Mechanism then
            Typ := Underlying_Type (Etype (Formal));

            --  If there is no underlying type, then skip this processing and
            --  leave the convention set to Default_Mechanism. It seems odd
            --  that there should ever be such cases but there are (see
            --  comments for filed regression tests 1418-001 and 1912-009) ???

            if No (Typ) then
               goto Skip_Formal;
            end if;

            case Convention (E) is

               ---------
               -- Ada --
               ---------

               --  Note: all RM defined conventions are treated the same
               --  from the point of view of parameter passing mechanims

               when Convention_Ada       |
                    Convention_Intrinsic |
                    Convention_Entry     |
                    Convention_Protected =>

                  --  By reference types are passed by reference (RM 6.2(4))

                  if Is_By_Reference_Type (Typ) then
                     Set_Mechanism (Formal, By_Reference);

                  --  By copy types are passed by copy (RM 6.2(3))

                  elsif Is_By_Copy_Type (Typ) then
                     Set_Mechanism (Formal, By_Copy);

                  --  All other types we leave the Default_Mechanism set, so
                  --  that the backend can choose the appropriate method.

                  else
                     null;
                  end if;

               -------
               -- C --
               -------

               --  Note: Assembler, C++ and Stdcall also use the C conventions

               when Convention_Assembler |
                    Convention_C         |
                    Convention_CPP       |
                    Convention_Stdcall   =>

                  --  The following values are passed by copy

                  --    IN Scalar parameters (RM B.3(66))
                  --    IN parameters of access types (RM B.3(67))
                  --    Access parameters (RM B.3(68))
                  --    Access to subprogram types (RM B.3(71))

                  --  Note: in the case of access parameters, it is the
                  --  pointer that is passed by value. In GNAT access
                  --  parameters are treated as IN parameters of an
                  --  anonymous access type, so this falls out free.

                  --  The bottom line is that all IN elementary types
                  --  are passed by copy in GNAT.

                  if Is_Elementary_Type (Typ) then
                     if Ekind (Formal) = E_In_Parameter then
                        Set_Mechanism (Formal, By_Copy);

                     --  OUT and IN OUT parameters of elementary types are
                     --  passed by reference (RM B.3(68)). Note that we are
                     --  not following the advice to pass the address of a
                     --  copy to preserve by copy semantics.

                     else
                        Set_Mechanism (Formal, By_Reference);
                     end if;

                  --  Records are normally passed by reference (RM B.3(69)).
                  --  However, this can be overridden by the use of the
                  --  C_Pass_By_Copy pragma or C_Pass_By_Copy convention.

                  elsif Is_Record_Type (Typ) then

                     --  If the record is not convention C, then we always
                     --  pass by reference, C_Pass_By_Copy does not apply.

                     if Convention (Typ) /= Convention_C then
                        Set_Mechanism (Formal, By_Reference);

                     --  If convention C_Pass_By_Copy was specified for
                     --  the record type, then we pass by copy.

                     elsif C_Pass_By_Copy (Typ) then
                        Set_Mechanism (Formal, By_Copy);

                     --  Otherwise, for a C convention record, we set the
                     --  convention in accordance with a possible use of
                     --  the C_Pass_By_Copy pragma. Note that the value of
                     --  Default_C_Record_Mechanism in the absence of such
                     --  a pragma is By_Reference.

                     else
                        Set_Mechanism (Formal, Default_C_Record_Mechanism);
                     end if;

                  --  Array types are passed by reference (B.3 (71))

                  elsif Is_Array_Type (Typ) then
                     Set_Mechanism (Formal, By_Reference);

                  --  For all other types, use Default_Mechanism mechanism

                  else
                     null;
                  end if;

               -----------
               -- COBOL --
               -----------

               when Convention_COBOL =>

                  --  Access parameters (which in GNAT look like IN parameters
                  --  of an access type) are passed by copy (RM B.4(96)) as
                  --  are all other IN parameters of scalar type (RM B.4(97)).

                  if Is_Elementary_Type (Typ)
                    and then Ekind (Formal) = E_In_Parameter
                  then
                     Set_Mechanism (Formal, By_Copy);

                  --  All other parameters (i.e. all non-scalar types, and
                  --  all OUT or IN OUT parameters) are passed by reference.
                  --  Note that at the moment we are not bothering to make
                  --  copies of scalar types as recommended in the RM.

                  else
                     Set_Mechanism (Formal, By_Reference);
                  end if;

               -------------
               -- Fortran --
               -------------

               when Convention_Fortran =>

                  --  For now, we pass all parameters by reference. It is
                  --  not clear that this is right in the long run, but it
                  --  seems to correspond to what gnu f77 wants.

                  --  The only exception is if the parameter is already an
                  --  access type, in which case we leave the default method
                  --  set (presumably it will get passed by copy)

                  if not Is_Access_Type (Typ) then
                     Set_Mechanism (Formal, By_Reference);
                  end if;

            end case;
         end if;

         <<Skip_Formal>> -- remove this when problem above is fixed ???

         Formal := Next_Formal (Formal);
      end loop;

      --  Now deal with return type, we always leave the default mechanism
      --  set except for the case of returning a By_Reference type for an
      --  Ada convention, where we force return by reference

      if Ekind (E) = E_Function
        and then Mechanism (E) = Default_Mechanism
        and then not Has_Foreign_Convention (E)
        and then Is_By_Reference_Type (Etype (E))
      then
         Set_Mechanism (E, By_Reference);
      end if;

   end Set_Mechanisms;

end Sem_Mech;
