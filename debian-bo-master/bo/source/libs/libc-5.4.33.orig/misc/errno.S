#ifdef __ELF__
.globl _errno
.data

_errno:
	.long 0

.type _errno,@object
/* FIXME - why is this in assembly code, anyway.  On a 64 bit machine, the
.size will be incorrect, but we currently cannot use difference expressions
in .size pseudo-ops. */
.size _errno,4
.weak errno
errno = _errno

#else
.globl _errno
.data

_errno:
	.long 0

#endif
