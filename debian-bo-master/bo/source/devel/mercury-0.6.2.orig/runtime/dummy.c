/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "dummy.h"

/*
** This dummy function is in a file of its own to ensure
** that gcc can't inline it. Similarly for the two pointers.
*/

void	*global_pointer;
void	*global_pointer_2;

void dummy_function_call(void)
{
}
