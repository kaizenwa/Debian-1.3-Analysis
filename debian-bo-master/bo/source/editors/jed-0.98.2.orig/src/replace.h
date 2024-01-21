/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
extern int replace_next(char *, char *);
extern int replace_chars (int *, char *);
extern int Replace_Preserve_Case;
extern SLang_Name_Type Jed_Other_Intrinsics [];
#ifndef Sixteen_Bit_System
extern void append_region_to_kill_array (int *);
extern void insert_from_kill_array (int *);
extern void copy_region_to_kill_array (int *);
extern int Kill_Array_Size;
#endif
