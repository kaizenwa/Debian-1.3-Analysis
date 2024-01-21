#define get_syscall_parameter(regs,n) (regs)->u_regs [(n)+8]
#define set_error(regs,e)	(regs)->u_regs [8] = -(e)
#define set_result(regs,r)	regs->u_regs [8] = r
#define clear_error(regs)
#define get_result(regs)	regs->u_regs [8]
