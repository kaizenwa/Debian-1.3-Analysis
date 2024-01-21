/* showfont.c - aeb, 940207 */
#include <sys/types.h>
#include <linux/kd.h>
#include <stdio.h>
#include <sys/ioctl.h>

char obuf[E_TABSZ], nbuf[E_TABSZ];

void
leave(n) int n; {
  printf("\0338");		/* restore attributes */
  if (ioctl(0,PIO_SCRNMAP,obuf)) {
      perror("PIO_SCRNMAP");
      fprintf(stderr, "failed to restore original translation table\n");
      exit(1);
  }
  exit(n);
}

int
main (argc,argv) int argc; char **argv; {
  unsigned char i, j, k;

  if (argc != 1) {
      fprintf(stderr, "usage: showfont
(probably after loading a font with `setfont font')\n");
      exit(1);
  }

  if (ioctl(0,GIO_SCRNMAP,obuf)) {
      perror("GIO_SCRNMAP");
      exit(1);
  }
  for (i=0; i<25; i++) printf("\n"); /* go down */
  printf("\0337");		/* save attributes */

  /* Since control characters are treated specially, we must
     avoid printing control characters. Therefore we do the
     printing in two stages. */

  k = 128;
  for (i = 0; i < 8; i++)
      for (j = 0; j < 16; j++)
	nbuf[k++] = 16*j+i;
  nbuf[128] = 32;		/* it will not be shown anyway */
  nbuf[32] = 32;
  nbuf[10] = 0;
  if (ioctl(0,PIO_SCRNMAP,nbuf)) {
      perror("PIO_SCRNMAP");
      fprintf(stderr, "cannot change translation table\n");
      exit(1);
  }

  printf("\033%%@");		/* leave Unicode */
  printf("\033(K");		/* select user translation */

  /* show first half of font */
  printf("\n");
  k = 128;
  for (i = 0; i < 8; i++) {
      for (j = 0; j < 16; j++)
	  printf ("  %c", k++);
      printf ("\n");
  }

  /* construct second half */
  k = 128;
  for (i = 8; i < 16; i++)
      for (j = 0; j < 16; j++)
	nbuf[k++] = 16*j+i;
  if (ioctl(0,PIO_SCRNMAP,nbuf)) {
      perror("PIO_SCRNMAP");
      fprintf(stderr, "cannot change translation table??\n");
      leave(1);
  }

  /* show second half of font */
  printf("\n");
  k = 128;
  for (i = 0; i < 8; i++) {
      for (j = 0; j < 16; j++)
	  printf ("  %c", k++);
      printf ("\n");
  }
  printf ("\n");

#if 0
  printf("\033(B");		/* and return to usual translation */
#endif
  leave(0);

  exit(0);			/* make gcc happy */
}
