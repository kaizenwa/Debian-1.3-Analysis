/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */


extern void SLang_create_array(void);
extern void SLarray_putelem(void);
extern void SLarray_putelem_r (void);
extern void SLarray_getelem(void);
extern void SLarray_sort(char *);
extern void SLinit_char_array(void);
extern SLuser_Object_Type *SLang_pop_array (int *);
extern void SLpush_array (int);
extern SLArray_Type *SLarray_from_handle (int);
extern void SLcopy_array (void);
extern SLuser_Object_Type *SLcreate_array(long *, int, int, int, int, 
					  unsigned char, unsigned char);

