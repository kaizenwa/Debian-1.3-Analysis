#if LINUX_VERSION_CODE >= 66375

extern unsigned long *
	create_ibcs_tables(char *p, struct linux_binprm *bprm, int ibcs);

#endif
