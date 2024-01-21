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
/* $Id: method.c,v 1.17 1995/05/23 19:46:35 spreitze Exp $ */
/* Last edited by Mike Spreitzer May 23, 1995 12:46 pm PDT */

#define _POSIX_SOURCE

#include "iluntrnl.h"
#include "method.h"
#include "type.h"

/*L1, L2, Main unconstrained*/

ilu_Method ilu_FindMethodByID (ilu_Class intro_type, ilu_cardinal ID)
{
  ilu_Method m = NIL;
  register ilu_cardinal i;

  if (intro_type != _ilu_rootClass)
      m = ilu_FindMethodByID(_ilu_rootClass, ID);
  if (m == NIL) {
      if (intro_type != NIL AND class_methods(intro_type) != NIL) {
	  for (i = 0;  i < class_method_count(intro_type);  i += 1)
	    if ((class_methods(intro_type))[i].me_id == ID) {
		m = &(class_methods(intro_type))[i];
		break;
	      }
	}
    }
  return (m);
}

ilu_cardinal ilu_IDOfMethod (ilu_Method method)
{
  if (method != NIL)
    return (method_id(method));
  else
    return 0;
}

ilu_refany ilu_GetMethodStubProc (ilu_Method method)
{
  if (method != NIL)
    return ((ilu_refany) (method_stub_proc(method)));
  else
    return NIL;
}

ilu_string ilu_NameOfMethod (ilu_Method method)
{
  if (method != NIL)
    return (method_name(method));
  else
    return NIL;
}

ilu_cardinal ilu_ExceptionCountOfMethod (ilu_Method method)
{
  if (method == NIL)
    return (0);
  else
    return (method_exception_count(method));
}

ilu_Exception ilu_ExceptionOfMethod (ilu_Method method, ilu_cardinal index)
{
  if (method == NIL || index > method_exception_count(method))
    return (NIL);
  else
    return (method_exception_vector(method)[index - 1]);
}

void ilu_SetMethodStubProc (ilu_Method method, void (*proc)(ilu_Call))
{
  if (method != NIL)
    method_stub_proc(method) = proc;
}
