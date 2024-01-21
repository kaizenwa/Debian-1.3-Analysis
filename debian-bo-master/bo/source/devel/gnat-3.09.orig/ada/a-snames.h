/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - S N A M E S                              */
/*                                                                          */
/*                              C Header File                               */
/*                                                                          */
/*                            $Revision: 1.59 $                             */
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
   Snames. It was created manually from the file snames.ads. */

/* Name_Id values */

#define Name_uParent		(First_Name_Id + 256 + 0)
#define Name_uTag		(First_Name_Id + 256 + 1)
#define Name_Off		(First_Name_Id + 256 + 2)
#define Name_Space		(First_Name_Id + 256 + 3)
#define Name_Time		(First_Name_Id + 256 + 4)
#define Name_uInit_Proc		(First_Name_Id + 256 + 5)

/* Define the function to return one of the numeric values below. Note
   that it actually returns a char since an enumeration value of less
   than 256 entries is represented that way in Ada.  The operand is a Chars
   field value.  */

#define Get_Attribute_Id snames__get_attribute_id
extern char Get_Attribute_Id PROTO ((int));

/* Define the numeric values for the attributes.  */

#define  Attr_Abort_Signal                   0
#define  Attr_Access                         1
#define  Attr_Address                        2
#define  Attr_Address_Size                   3
#define  Attr_Adjacent                       4
#define  Attr_Aft                            5
#define  Attr_Alignment                      6
#define  Attr_Asm_Input                      7
#define  Attr_Asm_Output                     8
#define  Attr_AST_Entry                      9
#define  Attr_Bit                           10
#define  Attr_Bit_Order                     11
#define  Attr_Body_Version                  12
#define  Attr_Callable                      13
#define  Attr_Caller                        14
#define  Attr_Ceiling                       15
#define  Attr_Component_Size                16
#define  Attr_Compose                       17
#define  Attr_Constrained                   18
#define  Attr_Copy_Sign                     19
#define  Attr_Count                         20
#define  Attr_Default_Bit_Order             21
#define  Attr_Definite                      22
#define  Attr_Delta                         23
#define  Attr_Denorm                        24
#define  Attr_Digits                        25
#define  Attr_Emax                          26
#define  Attr_Enum_Rep                      27
#define  Attr_Epsilon                       28
#define  Attr_Exponent                      29
#define  Attr_External_Tag                  30
#define  Attr_First                         31
#define  Attr_First_Bit                     32
#define  Attr_Fixed_Value                   33
#define  Attr_Floor                         34
#define  Attr_Fore                          35
#define  Attr_Fraction                      36
#define  Attr_Has_Discriminants             37
#define  Attr_Identity                      38
#define  Attr_Image                         37
#define  Attr_Img                           40
#define  Attr_Input                         41
#define  Attr_Integer_Value                 42
#define  Attr_Large                         43
#define  Attr_Last                          44
#define  Attr_Last_Bit                      45
#define  Attr_Leading_Part                  46
#define  Attr_Length                        47
#define  Attr_Machine                       48
#define  Attr_Machine_Emax                  49
#define  Attr_Machine_Emin                  50
#define  Attr_Machine_Mantissa              51
#define  Attr_Machine_Overflows             52
#define  Attr_Machine_Radix                 53
#define  Attr_Machine_Rounds                54
#define  Attr_Machine_Size                  55
#define  Attr_Mantissa                      56
#define  Attr_Max                           57
#define  Attr_Max_Interrupt_Priority        58
#define  Attr_Max_Priority                  59
#define  Attr_Max_Size_In_Storage_Elements  60
#define  Attr_Maximum_Alignment             61
#define  Attr_Mechanism_Code                62
#define  Attr_Min                           63
#define  Attr_Model                         64
#define  Attr_Model_Emin                    65
#define  Attr_Model_Epsilon                 66
#define  Attr_Model_Mantissa                67
#define  Attr_Model_Small                   68
#define  Attr_Modulus                       69
#define  Attr_Null_Parameter                70
#define  Attr_Object_Size                   71
#define  Attr_Partition_Id                  72
#define  Attr_Passed_By_Reference           73
#define  Attr_Pos                           74
#define  Attr_Position                      75
#define  Attr_Pred                          76
#define  Attr_Range                         77
#define  Attr_Range_Length                  78
#define  Attr_Remainder                     79
#define  Attr_Round                         80
#define  Attr_Rounding                      81
#define  Attr_Safe_Emax                     82
#define  Attr_Safe_First                    83
#define  Attr_Safe_Large                    84
#define  Attr_Safe_Last                     85
#define  Attr_Safe_Small                    86
#define  Attr_Scale                         87
#define  Attr_Scaling                       88
#define  Attr_Signed_Zeros                  89
#define  Attr_Size                          90
#define  Attr_Small                         91
#define  Attr_Storage_Size                  92
#define  Attr_Storage_Unit                  93
#define  Attr_Succ                          94
#define  Attr_Tag                           95
#define  Attr_Terminated                    96
#define  Attr_Tick                          97
#define  Attr_Truncation                    98
#define  Attr_Type_Class                    99
#define  Attr_Unbiased_Rounding            100
#define  Attr_Unchecked_Access             101
#define  Attr_Universal_Literal_String     102
#define  Attr_Unrestricted_Access          103
#define  Attr_Val                          104
#define  Attr_Valid                        105
#define  Attr_Value                        106
#define  Attr_Value_Size                   107
#define  Attr_Version                      108
#define  Attr_Wide_Image                   109
#define  Attr_Wide_Value                   100
#define  Attr_Wide_Width                   101
#define  Attr_Width                        112
#define  Attr_Word_Size                    113

#define  Attr_Output                       114
#define  Attr_Read                         115
#define  Attr_Write                        116

#define  Attr_Elab_Body                    117
#define  Attr_Elab_Spec                    118
#define  Attr_Storage_Pool                 119

#define  Attr_Base                         120
#define  Attr_Class                        121

/* Define the function to return one of the numeric values below.  Note
   that it actually returns a char since an enumeration value of less
   than 256 entries is represented that way in Ada.  The operand is a Chars
   field value.  */

#define Get_Pragma_Id snames__get_pragma_id
extern char Get_Pragma_Id PROTO ((int));

/* Define the numeric values for the pragmas. */

/* Configuration pragmas first */

#define  Pragma_C_Pass_By_Copy               0
#define  Pragma_Component_Alignment          1
#define  Pragma_Discard_Names                2
#define  Pragma_Extend_System                3
#define  Pragma_Float_Representation         4
#define  Pragma_Locking_Policy               5
#define  Pragma_Long_Float                   6
#define  Pragma_Normalize_Scalars            7
#define  Pragma_Queuing_Policy               8
#define  Pragma_Restrictions                 9
#define  Pragma_Reviewable                  10
#define  Pragma_Source_File_Name            11
#define  Pragma_Suppress                    12
#define  Pragma_Task_Dispatching_Policy     13
#define  Pragma_Unsuppress                  14
#define  Pragma_Warnings                    15

/* Remaining pragmas */

#define  Pragma_Abort_Defer                 16
#define  Pragma_Ada_83                      17
#define  Pragma_Ada_95                      18
#define  Pragma_All_Calls_Remote            19
#define  Pragma_Annotate                    20
#define  Pragma_Assert                      21
#define  Pragma_Asynchronous                22
#define  Pragma_Atomic                      23
#define  Pragma_Atomic_Components           24
#define  Pragma_Attach_Handler              25
#define  Pragma_Common_Object               26
#define  Pragma_Complex_Representation      27
#define  Pragma_Controlled                  28
#define  Pragma_Convention                  29
#define  Pragma_CPP_Class                   30
#define  Pragma_CPP_Constructor             31
#define  Pragma_CPP_Destructor              32
#define  Pragma_CPP_Virtual                 33
#define  Pragma_CPP_Vtable                  34
#define  Pragma_Debug                       35
#define  Pragma_Elaborate                   36
#define  Pragma_Elaborate_All               37
#define  Pragma_Elaborate_Body              38
#define  Pragma_Export                      39
#define  Pragma_Export_Exception            40
#define  Pragma_Export_Function             41
#define  Pragma_Export_Object               42
#define  Pragma_Export_Procedure            43
#define  Pragma_Export_Valued_Procedure     44
#define  Pragma_Ident                       45
#define  Pragma_Import                      46
#define  Pragma_Import_Exception            47
#define  Pragma_Import_Function             48
#define  Pragma_Import_Object               49
#define  Pragma_Import_Procedure            50
#define  Pragma_Import_Valued_Procedure     51
#define  Pragma_Inline                      52
#define  Pragma_Inline_Generic              53
#define  Pragma_Inspection_Point            54
#define  Pragma_Interface                   55
#define  Pragma_Interface_Name              56
#define  Pragma_Interrupt_Handler           57
#define  Pragma_Interrupt_Priority          58
#define  Pragma_Linker_Alias                59
#define  Pragma_Linker_Options              60
#define  Pragma_Linker_Section              61
#define  Pragma_List                        62
#define  Pragma Main_Storage                63
#define  Pragma_Machine_Attribute           64
#define  Pragma_Memory_Size                 65
#define  Pragma_No_Return                   66
#define  Pragma_Optimize                    67
#define  Pragma_Pack                        68
#define  Pragma_Page                        69
#define  Pragma_Passive                     70
#define  Pragma_Preelaborate                71
#define  Pragma_Priority                    72
#define  Pragma_Psect_Object                73
#define  Pragma_Pure                        74
#define  Pragma_Pure_Function               75
#define  Pragma_Remote_Call_Interface       76
#define  Pragma_Remote_Types                77
#define  Pragma_Share_Generic               78
#define  Pragma_Shared                      79
#define  Pragma_Shared_Passive              80
#define  Pragma_Source_Reference            81
#define  Pragma_Subtitle                    82
#define  Pragma_Suppress_All                83
#define  Pragma_System_Name                 84
#define  Pragma_Task_Info                   85
#define  Pragma_Task_Storage                86
#define  Pragma_Time_Slice                  87
#define  Pragma_Title                       88
#define  Pragma_Unchecked_Union             89
#define  Pragma_Unimplemented_Unit          90
#define  Pragma_Volatile                    91
#define  Pragma_Volatile_Components         92
#define  Pragma_Weak_External               93

/* The following are deliberately out of alphabetical order, see Snames */

#define  Pragma_AST_Entry                   94
#define  Pragma_Storage_Size                95
#define  Pragma_Storage_Unit                96

/* Define the numeric values for the conventions.  */

#define  Convention_Ada                      0
#define  Convention_Intrinsic                1
#define  Convention_Entry                    2
#define  Convention_Protected                3
#define  Convention_Assembler                4
#define  Convention_C                        5
#define  Convention_COBOL                    6
#define  Convention_CPP                      7
#define  Convention_Fortran                  8
#define  Convention_Stdcall                  9

/* End of a-snames.h (C version of Snames package spec) */
