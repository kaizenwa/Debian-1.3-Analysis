/***************************************************
 *      CHANGE THESE IF YOU WANT
 ***************************************************/

/* String and color for old messages */
#define OLDMESSAGE	"Alt"
#define OLDCOLOR	"yellow"

/* String and color for new messages */
#define NEWMESSAGE	"Neu"
#define NEWCOLOR	"green"

/* String for unknown phone numbers (0) */
#define NONDIGITAL	"ANALOG"

/* Abbreviation for seconds in your language */
#define SECONDSSTR	"s"

/* Abbreviation for KiloBytes in your language (is there any other?)*/
#define KILOBYTSTR	"kB"




/***************************************************
 *      DON'T TOUCH THINGS BELOW THIS LINE
 ***************************************************/

#define MAMVERSION	"0.0.3"
#define	NEEDEDVERSION	"1.0.4"

#define	NUMBERFIELD	"*numot"
#define	FLAGFIELD	"*flags"
#define	NAMEFIELD	"*name"
#define	CALLERFIELD	"*caller"
#define	SIZEFIELD	"*size"
#define	DATEFIELD	"*date"
#define	TIMEFIELD	"*time"
#define	MSGSFIELD	"*nummsg"
#define	ONOFFFIELD	"*onoff"
#define	PARKFIELD	"*parked"

#ifndef OUTDIR
#define OUTDIR		"messages"
#endif

#ifndef INDIR
#define INDIR		"incoming"
#endif

#ifndef VBOXSTOP
#define VBOXSTOP	".vboxstop"
#endif

#ifndef VBOXPARK
#define VBOXPARK	".vboxpark"
#endif

#define TRIGGERFILE	"/dev/isdninfo"

#define MAXNAMELEN                  (20)
#define MAXCALLERIDLEN              (20)
#define MAXUSERNAMELEN              (32)
#define MAXCOMMANDLEN               (10240)

#define VOICE_COMPRESSION_BASE  963.75        /* Basis fuer Kompression  */
#define VOICE_COMPRESSION_RATE  8000          /* Basis bei ULAW/ALAW */

