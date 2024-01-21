#include <ansidecl.h>
#include <stdio.h>
#include <gnu-stabs.h>
#include <mntent.h>

function_alias(setmntent, _IO_fopen, FILE *, (filename, mode),
	DEFUN(_IO_fopen, (filename, mode), CONST char *filename AND CONST char *mode))
