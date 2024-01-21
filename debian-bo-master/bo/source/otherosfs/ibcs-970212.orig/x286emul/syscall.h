#ifndef _EMU_SYSCALL_H_
#define _EMU_SYSCALL_H_

/* From emu_memory.c */
extern int emu_stkgro(struct sigcontext_struct *sc);
extern int emu_brkctl(struct sigcontext_struct *sc);

/* From emu_time.c */
extern int emu_time(struct sigcontext_struct *sc);

/* From emu_generic.c */
extern int emu_i_sas(struct sigcontext_struct *sc);
extern int emu_i_ass(struct sigcontext_struct *sc);
extern int emu_i_s(struct sigcontext_struct *sc);
extern int emu_i_sls(struct sigcontext_struct *sc);
extern int emu_i_a(struct sigcontext_struct *sc);
extern int emu_i_aa(struct sigcontext_struct *sc);
extern int emu_i_ssa(struct sigcontext_struct *sc);
extern int emu_i_v(struct sigcontext_struct *sc);

/* From emu_exec.c */
extern int emu_exec(struct sigcontext_struct *sc);

#endif /* _EMU_SYSCALL_H_ */
