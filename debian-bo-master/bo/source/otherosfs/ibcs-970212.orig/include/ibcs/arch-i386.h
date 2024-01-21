#define get_syscall_parameter(regs,n) get_fs_long(((unsigned long *)regs->esp)+((n)+1))
#define set_error(regs,e)	{ regs->eax = (e); regs->eflags |= 1; }
#define clear_error(regs)	{ regs->eflags &= ~1; }
#define set_result(regs,r)	regs->eax = r
#define get_result(regs)	regs->eax
