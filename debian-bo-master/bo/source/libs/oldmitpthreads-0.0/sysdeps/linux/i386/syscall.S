	.file	"syscall.S"

#ifdef __ELF__
#if defined(__i486__) || defined(i486)
#define ALIGN 16
#else
#define ALIGN 4
#endif
#else
#if defined(__i486__) || defined(i486)
#define ALIGN 4
#else
#define ALIGN 2
#endif
#endif

#ifdef __ELF__
#define SYMBOL_NAME(X) X
#define SYMBOL_NAME_LABEL(X) X##:
#else
#define SYMBOL_NAME(X) _##X
#define SYMBOL_NAME_LABEL(X) _##X##:
#endif

#ifdef __ELF__
#define	ENTRY(name)							      \
  .globl SYMBOL_NAME(name);						      \
  .align ALIGN;								      \
  SYMBOL_NAME_LABEL(name)
#else
#define	ENTRY(name)							      \
  .globl _##name##;							      \
  .align ALIGN;								      \
  SYMBOL_NAME_LABEL(name)
#endif

/* Use this for the different syntaxes for local assembler labels */
#ifdef __ELF__
#define L(X) .L##X
#define LF(X) .L##X
#define LL(X) .L##X##:
#else
#define L(X) X
#define LF(X) X##f
#define LL(X) X##:
#endif

#ifndef SYS_syscall
#define SYS_syscall	0
#endif

#ifdef PTHREAD_KERNEL

#ifdef __SVR4_I386_ABI_L1__

/* I don't know if it will work or not. IBCS doesn't say anything
 * about syscall. H.J. */
#define	PSEUDO(name, args)						      \
  .text;								      \
  ENTRY (name)								      \
    movl $(SYS_syscall),%eax						      \
    lcall $0x7,$0

#else /* __SVR4_I386_ABI_L1__ */

#define	PSEUDO(name, args)						      \
  .text;								      \
  ENTRY (name)								      \
    pushl %ebp;								      \
    movl %esp,%ebp;							      \
    PUSH_##args								      \
    MOVE_##args								      \
    int $0x80;								      \
    POP_##args								      \
    movl %ebp,%esp;							      \
    popl %ebp;
 
#endif /* __SVR4_I386_ABI_L1__ */

#else /* PTHREAD_KERNEL */

#define ERRNO_LOCATION SYMBOL_NAME(__errno_location)

#if defined(__PIC__) || defined (__pic__)

#ifdef __SVR4_I386_ABI_L1__

/* I don't know if it will work or not. IBCS doesn't say anything
 * about syscall. H.J. */
#define	PSEUDO(name, args)						      \
  .text;								      \
  ENTRY (name)								      \
    movl $(SYS_syscall),%eax						      \
    lcall $0x7,$0							      \
    cmpl $0xfffff000,%eax;						      \
    jb L(Lexit);							      \
    negl %eax;								      \
    pushl %eax;								      \
    call L(L4);								      \
   LL(L4)								      \
    popl %ebx;								      \
    addl $_GLOBAL_OFFSET_TABLE_+[.-L(L4)],%ebx;				      \
    call ERRNO_LOCATION@PLT;						      \
    popl %edx;								      \
    movl %edx,(%eax);							      \
    movl $-1,%eax;							      \
   LL(Lexit)

#else /* __SVR4_I386_ABI_L1__ */

#define	PSEUDO(name, args)						      \
  .text;								      \
  ENTRY (name)								      \
    pushl %ebp;								      \
    movl %esp,%ebp;							      \
    PUSH_##args								      \
    MOVE_##args								      \
    int $0x80;								      \
    cmpl $0xfffff000,%eax;						      \
    jb L(Lexit);							      \
    negl %eax;								      \
    pushl %eax;								      \
    call L(L4);								      \
   LL(L4)								      \
    popl %ebx;								      \
    addl $_GLOBAL_OFFSET_TABLE_+[.-L(L4)],%ebx;				      \
    call ERRNO_LOCATION@PLT;						      \
    popl %edx;								      \
    movl %edx,(%eax);							      \
    movl $-1,%eax;							      \
   LL(Lexit)								      \
    POP_##args 								      \
    movl %ebp,%esp;							      \
    popl %ebp;

#endif /* __SVR4_I386_ABI_L1__ */

#else /* PIC */

#ifdef __SVR4_I386_ABI_L1__

/* I don't know if it will work or not. IBCS doesn't say anything
 * about syscall. H.J. */
#define	PSEUDO(name, args)						      \
  .text;								      \
  ENTRY (name)								      \
    movl $(SYS_syscall),%eax						      \
    lcall $0x7,$0							      \
    cmpl $0xfffff000,%eax;						      \
    jb L(Lexit);							      \
    negl %eax;								      \
    pushl %eax;								      \
    call ERRNO_LOCATION;						      \
    popl %edx;								      \
    movl %edx,(%eax);							      \
    movl $-1,%eax;							      \
   LL(Lexit)

#else /* __SVR4_I386_ABI_L1__ */

#define	PSEUDO(name, args)						      \
  .text;								      \
  ENTRY (name)								      \
    pushl %ebp;								      \
    movl %esp,%ebp;							      \
    PUSH_##args								      \
    MOVE_##args								      \
    int $0x80;								      \
    cmpl $0xfffff000,%eax;						      \
    jb L(Lexit);							      \
    negl %eax;								      \
    pushl %eax;								      \
    call ERRNO_LOCATION;						      \
    popl %edx;								      \
    movl %edx,(%eax);							      \
    movl $-1,%eax;							      \
   LL(Lexit)								      \
    POP_##args								      \
    movl %ebp,%esp;							      \
    popl %ebp;
 
#endif /* __SVR4_I386_ABI_L1__ */

#endif /* PIC */

#endif /* PTHREAD_KERNEL */

/* Linux takes system call arguments in registers:
	0: %eax	This is the system call number.
   	1: %ebx This is the first argument.
	2: %ecx
	3: %edx
	4: %esi
	5: %edi
 */

#if defined(__PIC__) || defined (__pic__)
#define PUSH_0	pushl %ebx;
#else
#define PUSH_0	/* No arguments to push.  */
#endif
#define PUSH_1	pushl %ebx;
#define PUSH_2	PUSH_1
#define PUSH_3	PUSH_1
#define PUSH_4	pushl %esi; PUSH_3
#define PUSH_5	pushl %edi; PUSH_4

#define	MOVE_0	movl 8(%ebp),%eax;
#define	MOVE_1	MOVE_0 movl 12(%ebp),%ebx;
#define	MOVE_2	MOVE_1 movl 16(%ebp),%ecx;
#define	MOVE_3	MOVE_2 movl 20(%ebp),%edx;
#define	MOVE_4	MOVE_3 movl 24(%ebp),%esi;
#define	MOVE_5	MOVE_4 movl 28(%ebp),%edi;

#if defined(__PIC__) || defined (__pic__)
#define POP_0	popl %ebx;
#else
#define POP_0	/* No arguments to pop.  */
#endif
#define POP_1	popl %ebx;
#define POP_2	POP_1
#define POP_3	POP_1
#define POP_4	POP_3 popl %esi;
#define POP_5	POP_4 popl %edi;

#ifdef PTHREAD_KERNEL

PSEUDO(__machdep_syscall, 5)
	ret
#ifdef __ELF__
	.type __machdep_syscall,@function
.L_machdep_syscall_end:
	.size __machdep_syscall,.L_machdep_syscall_end - __machdep_syscall
#endif

#else /* PTHREAD_KERNEL */

/* No weak syscall?. */
#undef _POSIX_THREADS

PSEUDO (syscall, 5)
	ret
#ifdef __ELF__
#ifdef _POSIX_THREADS
	.weak syscall
#endif
	.type syscall,@function
.L_syscall_end:
	.size syscall,.L_syscall_end - syscall
#endif

#endif /* PTHREAD_KERNEL */
