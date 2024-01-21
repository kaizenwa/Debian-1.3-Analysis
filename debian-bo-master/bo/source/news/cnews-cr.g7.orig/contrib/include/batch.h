extern sizeint bt_maxarts, bt_maxtime, bt_maxbytes;
extern sizeint bt_rej, bt_fail;

extern int batchadd(/* char *buf, sizeint buflen, sizeint artsize,
		       FILE *tmpfp, sizeint tmpbytes */);
extern int batchwrite(/* FILE *fp, char *buf, sizeint buflen, FILE *tmpfp,
		         sizeint tmpbytes */);
extern void batchend(/* void */);
extern void batchinit(/* FILE *fp */);

