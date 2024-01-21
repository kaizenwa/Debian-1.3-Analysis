/*
#ident	"@(#)smail/src:RELEASE-3_2:silly.c,v 1.7 1996/02/28 14:26:38 woods Exp"
 */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 * 
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * silly:
 *	silly is a silly file which is linked in last to overcome a
 *	fencepost error in gdb
 *
 * NOTE: this function is left in for hisorical reasons
 *
 *	external functions: silly
 */
#include <stdio.h>
#include <sys/types.h>
#ifdef STANDALONE
char *local_sender;
#else /* !STANDALONE */
#ifndef DEPEND
# include "extern.h"
#endif
extern char *local_sender;
#endif /* !STANDALONE */

/* local defines */
#define TOMBNAME (17)			/* max length of a tombsone name */

/*
 * silly - a very silly function
 *
 * display a silly message
 */
silly()
{
	char name[TOMBNAME+1];		/* name to put on tombstone */
	int sender_len;			/* length of the sender string */
	int pad_len;			/* spaces on left for centering */
	int i;				/* index */

	/*
	 * form the name in the center of the 'name' string
	 */
	compute_local_sender();
	sender_len = strlen(local_sender);
	sender_len = (sender_len > TOMBNAME) ? TOMBNAME : sender_len;
	pad_len = (TOMBNAME-sender_len)/2;
	for (i=0; i < pad_len; ++i) {
		name[i] = ' ';
	}
	strncpy(&name[pad_len], local_sender, sender_len);
	for (i=pad_len+sender_len; i < TOMBNAME; ++i) {
		name[i] = ' ';
	}
	name[TOMBNAME] = '\0';

	/*
	 * display a silly message
	 */
	printf("\n\nThe route-addr hits...");
	fflush(stdout);
	sleep(1);
	printf("\rYour mailer feels weeker");
	fflush(stdout);
	sleep(1);
	printf("\rThe route-addr hits...     ");
	fflush(stdout);
	sleep(1);
	printf("\n\n");
	printf("             ___________\n");
	printf("            /           \\\n");
	printf("           /    R I P    \\\n");
	printf("          /               \\\n");
	printf("         /                 \\\n");
	printf("        | %s |\n", name);
	printf("        |                   |\n");
	printf("        |    Eaten by a     |\n");
	printf("        |   chain letter    |\n");
	printf("        |    on level 1     |\n");
	printf("        |                   |\n");
	printf("        |       %4d        |\n", get_local_year());
	printf("        |                   |\n");
	printf("       *|      *  *  *      | *\n");
	printf(" ______)/\\/\\_//(\\/(/\\)/\\//\\/|_)______\n\n\n\n");
}

#ifdef STANDALONE

#ifndef HASH_STANDALONE
/*
 * main - see what the silly program does
 */
void
main()
{
	/* print the silly mssage */
	silly();
	exit(0);
}
#endif /* HASH_STANDALONE */

#endif /* STANDALONE */
