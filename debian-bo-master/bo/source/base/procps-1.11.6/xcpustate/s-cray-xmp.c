/* System dependent file for Cray X/MP and Y/MP Unicos >= 5.1 Systems */
/* Walter D. Poxon, CRI, Mendota Heights, MN  <wdp@cray.com> */

#include <nlist.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/unistd.h>
#include <sys/pws.h>
#include <errno.h>

#define UNICOS "/unicos"
#define KMEM "/dev/kmem"

#define NSTATES 6

static struct nlist nl[] = { {"cpuw"},{0} };
int kmem;
long bytes;
struct pw cpuw;

extern char *xmalloc(/* int nbytes */);

union cpu_name {
  word wval;
  struct {
    uint : 32,
    c    : 8,
    p    : 8,
    u    : 8,
    no   : 8;
  } cval;
};

/* Called at the beginning to inquire how many bars are needed. */
int
  num_bars()
{
  bzero(&cpuw,sizeof(cpuw));

  if (nlist(UNICOS,nl) == -1) {
    perror("nlist");
    return 0;
  }

  bytes = nl[0].n_value;

  if ((kmem = open(KMEM, O_RDONLY)) <0) {
    perror("open");
    return 0;
  }

  if (lseek(kmem, bytes, SEEK_SET) != bytes)
    perror("lseek");

  if (read(kmem, &cpuw, sizeof(cpuw)) != sizeof(cpuw))
    perror("read");

  return cpuw.pw_ccpu;       /* Number of CPU's configured in UNICOS */
/*return cpuw.pw_scpu;       /* Number of CPU's started */

}


char **
  label_bars(nbars)          /* Called after num_bars to ask for bar names */
{
  char **names;
  int i;
  extern char *strcpy(/* char *, const char * */);
  union cpu_name c;

  names = (char **) xmalloc(nbars * sizeof(char *));
  for(i = 0; i < nbars; i++) {
    char buf[8];
    c.wval = cpuw.pws[i].pw_cpu;
    (void) sprintf(buf, "%c%c%c%c", c.cval.c,c.cval.p,c.cval.u,c.cval.no);
    names[i] = strcpy(xmalloc(strlen(buf)+1), buf);
  }
  return names;
}

/* 
 *  Called after the bars are created to perform any machine dependent
 *  initializations.
 */
void
  init_bars(nbars)
int nbars;
{
  (void) display_bars(nbars);
}

/* 
 *  This procedure gets called every interval to compute and display the
 *  bars. It should call draw_bar() with the bar number, the array of
 *  integer values to display in the bar, and the number of values in
 *  the array.
 */

#define delta(cpustate) \
  ((int) (si[i]->cpu[(cpustate)] - last_si[i]->cpu[(cpustate)]))

  display_bars(nbars)
{
    int states[NSTATES];
    int nstates;
    int cpu;
    extern void draw_bar(/*int bar_num, int *states, int num_states*/);

    if (lseek(kmem, bytes, SEEK_SET) != bytes)
      perror("lseek");
    
    if (read(kmem, &cpuw, sizeof(cpuw)) != sizeof(cpuw))
      perror("read");

    for (cpu=0; cpu < nbars; cpu++) {
      nstates = 0;
      states[nstates++] = cpuw.pws[cpu].pw_syswe + cpuw.pws[cpu].pw_idlee;
      states[nstates++] = cpuw.pws[cpu].pw_usere;
      states[nstates++] = cpuw.pws[cpu].pw_unixe;
      draw_bar(cpu, states, nstates);
    }
}
