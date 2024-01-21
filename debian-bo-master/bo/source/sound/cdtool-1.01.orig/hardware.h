/* hardware.h */

typedef struct {
    struct cdrom_tochdr tochdr;
    struct cdrom_tocentry tocentries[100];
    struct cdrom_subchnl subchnl;
} cdhw_t;

cdhw_t *read_hw(char *progname, int cdfile);
