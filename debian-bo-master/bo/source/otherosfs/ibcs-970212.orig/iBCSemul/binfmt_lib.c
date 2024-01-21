#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <linux/a.out.h>
#include <linux/malloc.h>
#include <linux/sched.h>
#include <linux/user.h>

#include <asm/pgtable.h>


#if LINUX_VERSION_CODE >= 66375


unsigned long *
create_ibcs_tables(char *p, struct linux_binprm *bprm, int ibcs)
{
	unsigned long *argv,*envp;
	unsigned long * sp;
	int argc = bprm->argc;
	int envc = bprm->envc;

	sp = (unsigned long *) ((-(unsigned long)sizeof(char *)) & (unsigned long) p);
	sp -= envc+1;
	envp = sp;
	sp -= argc+1;
	argv = sp;
	if (!ibcs) {
		put_user(envp,--sp);
		put_user(argv,--sp);
	}
	put_user(argc,--sp);
	current->mm->arg_start = (unsigned long) p;
	while (argc-->0) {
		put_user(p,argv++);
		while (get_user(p++)) /* nothing */ ;
	}
	put_user(NULL,argv);
	current->mm->arg_end = current->mm->env_start = (unsigned long) p;
	while (envc-->0) {
		put_user(p,envp++);
		while (get_user(p++)) /* nothing */ ;
	}
	put_user(NULL,envp);
	current->mm->env_end = (unsigned long) p;
	return sp;
}

#endif
