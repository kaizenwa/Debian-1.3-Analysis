# This file contains stuff that goes at the end of the .init and .fini
# sections.  The idea is that the linker simply strings together the init
# sections from the various input files, and this end up at the end.


	.section .init
/*
 *	Setup profiling
 */
	pushl $_mcleanup
	call atexit
	addl $4,%esp
	pushl $_etext
	pushl $_start
	call monstartup
	addl $8,%esp
	ret $0

	.section .fini
	ret $0
