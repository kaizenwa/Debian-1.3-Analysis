#ifndef ASM_OPS_H
#define ASM_OPS_H

#if !defined(__i486__) && !defined(__i586__)
#define ALIGNTO		"4"
#define ALIGNTO_LOG2	"2"
#else
#define ALIGNTO		"16"
#define ALIGNTO_LOG2	"4"
#endif

#ifdef __ELF__
#define L(X)  ".LX" #X
#define LF(X) ".LX" #X
#define LB(X) ".LX" #X
#define LL(X) ".LX" #X ":"
#define ASIDENT(X) #X
#define ALIGN ".align "ALIGNTO",0x90\n"
#else
#define L(X) #X
#define LF(X) #X "f"
#define LB(X) #X "b"
#define LL(X) #X ":"
#define ASIDENT(X) "_" #X
#define ALIGN ".align "ALIGNTO_LOG2",0x90\n"
#endif

#endif
