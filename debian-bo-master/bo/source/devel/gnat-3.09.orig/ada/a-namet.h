/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                              A - N A M E T                               */
/*                                                                          */
/*                              C Header File                               */
/*                                                                          */
/*                            $Revision: 1.27 $                             */
/*                                                                          */
/*   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/* This is the C file that corresponds to the Ada package specification
   Namet. It was created manually from files namet.ads and namet.adb.

/* Structure defining a names table entry.  */

struct Name_Entry
{
  Int Name_Chars_Index; /* Starting location of char in Name_Chars table. */
  Short Name_Len;         /* Length of this name in characters. */
  Byte Byte_Info;       /* Byte value associated with this name */
  Byte Spare;           /* Unused */
  Name_Id Hash_Link;    /* Link to next entry in names table for same hash
                           code. Not accessed by C routines.  */
  Int Int_Info;         /* Int value associated with this name */
};

/* Pointer to names table vector. */
#define Names_Ptr namet__name_entries__table
extern struct Name_Entry *Names_Ptr;

/* Pointer to name characters table. */
#define Name_Chars_Ptr namet__name_chars__table
extern char *Name_Chars_Ptr;

#define Name_Buffer namet__name_buffer
extern char Name_Buffer[];

extern Int namet__name_len;
#define Name_Len namet__name_len

/* Get_Name_String returns a null terminated C string for the specified name.
   We could use the official Ada routine for this purpose, but since the
   strings we want are sitting in the name strings table in exactly the form
   we need them (null terminated), we just point to the name directly. */

INLINE char *
Get_Name_String (Id)
     Name_Id Id;
{
  return Name_Chars_Ptr + Names_Ptr [Id - First_Name_Id].Name_Chars_Index + 1;
}

/* Get_Decoded_Name_String returns a null terminated C string in the same
   manner as Get_Name_String, except that it is decoded (i.e. upper half or
   wide characters are put back in their external form, and character literals
   are also returned in their external form (with surrounding apostrophes) */

extern void namet__get_decoded_name_string PROTO ((Name_Id));

INLINE char *
Get_Decoded_Name_String (Id)
     Name_Id Id;
{
  namet__get_decoded_name_string (Id);
  Name_Buffer [Name_Len] = 0;
  return Name_Buffer;
}

/* Likewise, but also upper-case the string unless it is a character
   literal.  This is used for building the enumeration literal table */

extern void casing__set_all_upper_case PROTO ((void));

INLINE char *
Get_Upper_Decoded_Name_String (Id)
     Name_Id Id;
{
  namet__get_decoded_name_string (Id);
  if (Name_Buffer [0] != '\'')
    casing__set_all_upper_case ();
  Name_Buffer [Name_Len] = 0;
  return Name_Buffer;
}

/* The following routine is not part of Namet, but we include the header here
   since it returns a Name_Id value */

#define Get_Encoded_Type_Name exp_dbug__get_encoded_type_name
extern Boolean Get_Encoded_Type_Name PROTO ((Entity_Id));

/* End of a-namet.h (C version of Namet package specification and body) */
