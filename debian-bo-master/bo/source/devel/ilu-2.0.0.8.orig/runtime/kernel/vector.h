/*
Copyright (c) 1991, 1992, 1993 Xerox Corporation.  All Rights Reserved.  

Unlimited use, reproduction, and distribution of this software is
permitted.  Any copy of this software must include both the above
copyright notice of Xerox Corporation and this paragraph.  Any
distribution of this software must comply with all applicable United
States export control laws.  This software is made available AS IS,
and XEROX CORPORATION DISCLAIMS ALL WARRANTIES, EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE, AND NOTWITHSTANDING ANY OTHER
PROVISION CONTAINED HEREIN, ANY LIABILITY FOR DAMAGES RESULTING FROM
THE SOFTWARE OR ITS USE IS EXPRESSLY DISCLAIMED, WHETHER ARISING IN
CONTRACT, TORT (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF
XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
*/
/* $Id: vector.h,v 1.3 1994/02/15 20:33:29 janssen Exp $ */
/* Last tweaked by Mike Spreitzer November 2, 1993 9:40 am PST */

#ifndef _ILU_VECTOR_
#define _ILU_VECTOR_

/*L2, Main unconstrained*/

typedef struct ilu_vector_s {
  /*L1 >= {some mutex that protects the vector}*/
  
  ilu_refany *ve_elements;
  ilu_cardinal ve_capacity;
  ilu_cardinal ve_size;
} *ilu_Vector;

/*L1 unconstrained*/
ilu_Vector _ilu_vector_new (ilu_cardinal capacity);

/*L1 >= {some mutex that protects the vector}*/

void _ilu_vector_destroy (ilu_Vector v, void (*f) (ilu_refany));
void _ilu_vector_add (ilu_Vector v, ilu_refany e);
void _ilu_vector_remove (ilu_Vector v, ilu_refany e);
ilu_cardinal _ilu_vector_size ( ilu_Vector v );
ilu_refany * _ilu_vector_elements ( ilu_Vector v );
ilu_Vector _ilu_vector_copy (ilu_Vector old);
void _ilu_vector_assign (ilu_Vector l, ilu_Vector r);
void _ilu_vector_add_if_not_present (ilu_Vector v, ilu_refany e);

#define VECTOR(a)	((ilu_Vector)(a))

#endif /* _ILU_VECTOR_ */
