
/* Definitions for the network part of the score service */

#define SCORE_HOST	"192.215.96.59"
#define SCORE_PORT	4444

#define KEY_LEN		4	/* Auth key length in bytes */

/*			         CRC  Name    Score Wave */
#define SCOREFMT	"NEWSCORE\t%s\t%s\t%-3.1ld\t%d\n"
