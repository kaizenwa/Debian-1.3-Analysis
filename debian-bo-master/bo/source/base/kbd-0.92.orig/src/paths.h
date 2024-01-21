/*
 * All data is in subdirectories of DATADIR, by default /usr/lib/kbd
 * The following four subdirectories are defined:
 */
#define KEYMAPDIR "keytables"
#define FONTDIR "consolefonts"
#define TRANSDIR "consoletrans"
#define VIDEOMODEDIR "videomodes"

/*
 * Default keymap, and where the kernel copy of it lives.
 */
#define DEFMAP "defkeymap.map"
#define KERNDIR "/usr/src/linux/drivers/char"

extern FILE *findfile(char *fnam, char **dirpath, char **suffixes);
extern char pathname[];

extern int verbose;
