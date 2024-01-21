#define NNTP_HI		"200 Hi"	/* Initial greeting */
#define NNTP_BYE	"205 Bye"	/* to QUIT */
#define NNTP_WANT	"335 Ok"	/* to IHAVE */
#define NNTP_NEXT	"435 Next"	/* to IHAVE */
#define NNTP_OK		"235 Next"	/* to IHAVE article xfer */
#define NNTP_FAIL	"436 Eeek"	/* to IHAVE article xfer */
#define NNTP_HERE	"220 0"		/* to ARTICLE */
#define NNTP_NO		"430 No"	/* to ARTICLE */
#define NNTP_EH		"500 Eh?"	/* to unrecognized command */
#define NNTP_DENIED	"400 Sorry."	/* Initial greeting */
#define NNTP_POST	"340 Ok"	/* to POST */
#define NNTP_NOPOST	"440 No"	/* to POST */
#define NNTP_POSTOK	"240 Next"	/* to POST article */
#define NNTP_POSTFAIL	"441 Eeek"	/* to POST article */
#define NNTP_POSTFAILC	"441-Eeek"	/* to POST article */
