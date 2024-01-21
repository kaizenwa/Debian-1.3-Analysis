	.text
	.long 200

	.align	3
	.globl _rl_reset_terminal
_rl_reset_terminal:
	jmp	_rl_reset_terminal__LOCAL__

	.align	3
	.globl _rl_initialize
_rl_initialize:
	jmp	_rl_initialize__LOCAL__

	.align	3
	.globl _readline
_readline:
	jmp	_readline__LOCAL__

	.align	3
	.globl _add_history
_add_history:
	jmp	_add_history__LOCAL__

	.align	3
	.globl _rl_complete
_rl_complete:
	jmp	_rl_complete__LOCAL__

	.align	3
	.globl _rl_list_possib
_rl_list_possib:
	jmp	_rl_list_possib__LOCAL__

	.align	3
	.globl _rl_ttyset
_rl_ttyset:
	jmp	_rl_ttyset__LOCAL__

	.align	3
	.globl _rl_add_slash
_rl_add_slash:
	jmp	_rl_add_slash__LOCAL__

	.org 0x00003ff8
	jmp	___main

	.org 0x00004000
	.long __SHARABLE_CONFLICTS__
_GLOBAL_OFFSET_TABLE_:
	.long 0xfeeb1ed3
.globl __GOT__rl_meta_chars
__GOT__rl_meta_chars:
	.long _rl_meta_chars
.globl __GOT__rl_eof
__GOT__rl_eof:
	.long _rl_eof
.globl __GOT__rl_erase
__GOT__rl_erase:
	.long _rl_erase
.globl __GOT__rl_intr
__GOT__rl_intr:
	.long _rl_intr
.globl __GOT__rl_kill
__GOT__rl_kill:
	.long _rl_kill
.globl __GOT__rl_quit
__GOT__rl_quit:
	.long _rl_quit
.globl __GOT__version_string
__GOT__version_string:
	.long _version_string
.globl __GOT__string
__GOT__string:
	.long _string
.globl __GOT__foo_string
__GOT__foo_string:
	.long _foo_string
	.org 0x00006000
.globl _rl_meta_chars
_rl_meta_chars:
	.long 1

	.org 0x00006004
.globl _rl_eof
_rl_eof:
	.space 4

	.org 0x00006008
.globl _rl_erase
_rl_erase:
	.space 4

	.org 0x0000600c
.globl _rl_intr
_rl_intr:
	.space 4

	.org 0x00006010
.globl _rl_kill
_rl_kill:
	.space 4

	.org 0x00006014
.globl _rl_quit
_rl_quit:
	.space 4

	.org 0x00006018
.globl _version_string
_version_string:
	.ascii "1.234\0"

	.org 0x00006098
.globl _string
_string:
___STD_00000:
	.long _version_string+2

	.org 0x000060a0
.globl _foo_string
_foo_string:
	.long ___STV__00009__00000

.data
___fixup_list:
	.stabs "__BUILTIN_FIXUPS__",25,0,0,___fixup_list
	.long __GOT__version_string
	.long ___STD_00000
	.long 0
	.align	12
	.stabs "__SHARABLE_CONFLICTS__",23,0,0,_GLOBAL_OFFSET_TABLE_
