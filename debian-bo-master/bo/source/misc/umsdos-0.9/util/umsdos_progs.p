/* main.c 13/01/95 01.20.46 */
/* numconst.c 20/01/94 01.31.18 */
int octal_toi (const char *parm, int &error);
int hexa_toi (const char *parm, int &error);
int deci_toi (const char *parm, int &error);
int xtoi (const char *parm, int &error);
/* printk.c 12/05/94 07.13.08 */
extern "C" int printk (const char *msg, ...);
/* udosctl.c 13/01/95 01.11.46 */
int udosctl_main (int argc, char *argv[]);
/* udump.c 28/08/93 00.25.02 */
/* umsdosio.c 19/01/95 23.30.52 */
/* umssetup.c 13/01/95 01.11.32 */
int umssetup_main (int argc, char *argv[]);
/* umssync.c 24/01/95 23.05.22 */
int umssync_main (int argc, char *argv[]);
