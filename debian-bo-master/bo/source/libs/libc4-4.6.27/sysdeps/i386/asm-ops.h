#ifndef ASM_OPS_H
#define ASM_OPS_H

#ifdef __ELF__
#define L(X)  ".LX" #X
#define LF(X) ".LX" #X
#define LB(X) ".LX" #X
#define LL(X) ".LX" #X ":"
#define ASIDENT(X) #X
#else
#define L(X) #X
#define LF(X) #X "f"
#define LB(X) #X "b"
#define LL(X) #X ":"
#define ASIDENT(X) "_" #X
#endif

#if !defined(__i486__) && !defined(__i586__)
#ifdef __ELF__
#define ALIGN ".align 4,0x90"
#else  /* __ELF__ */
#define ALIGN ".align 2,0x90"
#endif /* __ELF__ */
#else  /* __i486__/__i586__ */
#ifdef __ELF__
#define ALIGN ".align 16,0x90"
#else  /* __ELF__ */
#define ALIGN ".align 4,0x90"
#endif /* __ELF__ */
#endif /* __i486__/__i586__ */

#endif
