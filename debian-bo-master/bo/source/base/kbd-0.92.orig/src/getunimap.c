#include <stdio.h>
#include <errno.h>
#include <linux/kd.h>

extern int getfd();
extern char *malloc();

main(){
    int fd, ct;
    struct unimapdesc ud;
    int i;

    fd = getfd();

    ud.entry_ct = ct = 0;
    ud.entries = 0;
    if (ioctl(fd, GIO_UNIMAP, &ud)) {
	if (errno != ENOMEM) {
	    perror("GIO_UNIMAP");
	    exit(1);
	}
	ct = ud.entry_ct;
	ud.entries = (struct unipair *) malloc(ct * sizeof(struct unipair));
	if (ioctl(fd, GIO_UNIMAP, &ud)) {
	    perror("2nd GIO_UNIMAP");
	    exit(1);
	}
	if (ct != ud.entry_ct)
	  fprintf(stderr, "strange... ct changed from %d to %d\n",
		  ct, ud.entry_ct);

	/* someone could change the unimap between our
	   first and second ioctl, so the above errors
	   are not impossible */
    }
    printf("count=%d\n", ud.entry_ct);
    for(i=0; i<ud.entry_ct && i<ct; i++)
      printf("0x%02x\tU+%04x\n", ud.entries[i].fontpos, ud.entries[i].unicode);
}
