extern void ___chkr_init_chkr (int linked, int nlibs, char **libs,
				int argc, char *argv[], char *envp[]);
extern int chkr$main (int, char **, char **);

/* The real __main will be called by chkr$main.  */
void
__main(void)
{
}
		   
int
main (int argc, char *argv[], char *envp[])
{
  ___chkr_init_chkr (0, 0, 0, argc, argv, envp);
  return chkr$main (argc, argv, envp);
}
