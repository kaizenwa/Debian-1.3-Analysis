#include "sysincludes.h"
#include "msdos.h"
#include "mtools.h"
#include "patchlevel.h"
#include "partition.h"
#include "vfat.h"

const char *mversion = VERSION;
const char *mdate = DATE;
const char *progname;

static const struct dispatch {
	const char *cmd;
	void (*fn)(int, char **, int);
	int type;
} dispatch[] = {
	{"mattrib",mattrib, 0},
	{"mbadblocks",mbadblocks, 0},
	{"mcd",mcd, 0},
	{"mcopy",mcopy, 0},
	{"mdel",mdel, 0},
	{"mdeltree",mdel, 2},
	{"mdir",mdir, 0},
	{"mformat",mformat, 0},
	{"minfo", minfo, 0},
	{"mlabel",mlabel, 0},
	{"mmd",mmd, 0},
	{"mmount",mmount, 0},
	{"mpartition",mpartition, 0},
	{"mrd",mdel, 1},
	{"mread",mcopy, 0},
	{"mmove",mmove, 0},
	{"mren",mmove, 1},
	{"mtoolstest", mtoolstest, 0},
	{"mtype",mcopy, 1},
	{"mwrite",mcopy, 0},
	{"mzip", mzip, 0}
};
#define NDISPATCH (sizeof dispatch / sizeof dispatch[0])



void main(int argc,char **argv)
{
	char *name;
	int i;

	init_privs();

	/* print the version */
	if(argc >= 2 && strcmp(argv[1], "-V") == 0) {
		printf("Mtools version %s, dated %s\n", mversion, mdate);
		exit(0);
	}

	/* check whether the compiler lays out structures in a sane way */
	if(sizeof(struct partition) != 16 ||
	   sizeof(struct directory) != 32 ||
	   sizeof(struct vfat_subentry) !=32) {
		fprintf(stderr,"Mtools has not been correctly compiled\n");
		fprintf(stderr,"Recompile it using a more recent compiler\n");
		exit(137);
	}

	if ((name = strrchr(argv[0],'/')))
		name++;
	else
		name = argv[0];
	progname = argv[0];

	/* this allows the different tools to be called as "mtools -c <command>"
	** where <command> is mdir, mdel, mcopy etcetera
	** Mainly done for the BeOS, which doesn't support links yet.
	*/

	if(argc >= 3 && 
	   !strcmp(argv[1], "-c") &&
	   !strcmp(name, "mtools")) {
		argc-=2;
		argv+=2;
		name = argv[0];
	}

	argv[0] = name;
	
	read_config();
	setup_signal();
	for (i = 0; i < NDISPATCH; i++) {
		if (!strcmp(name,dispatch[i].cmd))
			dispatch[i].fn(argc, argv, dispatch[i].type);
	}
	if (strcmp(name,"mtools"))
		fprintf(stderr,"Unknown mtools command '%s'\n",name);
	fprintf(stderr,"Supported commands:");
	for (i = 0; i < NDISPATCH; i++) {
		if (i%8 == 0) putc('\n', stderr);
		else fprintf(stderr, ", ");
		fprintf(stderr, "%s", dispatch[i].cmd);
	}
	putc('\n', stderr);

	exit(1);
}
