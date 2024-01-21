/* @(#) error.h,v 1.15 1996/05/29 18:48:23 woods Exp */

/*
 *    Copyright (C) 1987, 1988 Ronald S. Karr and Landon Curt Noll
 *    Copyright (C) 1992  Ronald S. Karr
 *
 * See the file COPYING, distributed with smail, for restriction
 * and warranty information.
 */

/*
 * error.h:
 *	error numbers used by smail, for address structures or for
 *	other purposes.  Error numbers up to 1000 are reserved.
 *	Numbers over 1000 may be used for local purposes.
 */
#define ERR_100		100L		/* unknown user */
#define ERR_101		101L		/* unknown host */
#define ERR_102		102L		/* method transport not found */
#define ERR_103		103L		/* default transport not found */
#define ERR_104		104L		/* security violation */
#define ERR_105		105L		/* no pipe transport */
#define ERR_106		106L		/* no file transport */
#define ERR_107		107L		/* director driver not found */
#define ERR_108		108L		/* error building path */
#define ERR_109		109L		/* router driver not found */
#define ERR_110		110L		/* no transport for router */
#define ERR_111		111L		/* address parse error */
#define ERR_112		112L		/* failed to open alias database */
#define ERR_113		113L		/* alias parsing error */
#define ERR_114		114L		/* forward file not an absolute path */
#define ERR_115		115L		/* forward file parse error */
#define ERR_116		116L		/* include name expansion failed */
#define ERR_117		117L		/* include file open failed */
#define ERR_118		118L		/* include file read failed */
#define ERR_119		119L		/* include file parse error */
#define ERR_120		120L		/* smartuser expansion failed */
#define ERR_121		121L		/* smartuser parse error */
#define ERR_122		122L		/* user transport not defined */
#define ERR_123		123L		/* failed to open pathalias database */
#define ERR_124		124L		/* bad entry in pathalias database */
#define ERR_125		125L		/* no smarthost information */
#define ERR_126		126L		/* smart_path parse error */
#define ERR_127		127L		/* uuname scanning error */
#define ERR_128		128L		/* username expansion failed */
#define ERR_129		129L		/* username contains / */
#define ERR_130		130L		/* file or dir attribute required */
#define ERR_131		131L		/* failed to expand file or dir */
#define ERR_132		132L		/* appendfile pathname not absolute */
#define ERR_133		133L		/* appendfile failed to open file */
#define ERR_134		134L		/* failed to lock mailbox */
#define ERR_135		135L		/* write to mailbox failed */
#define ERR_136		136L		/* rename failed for queue file */
#define ERR_137		137L		/* no cmd attribute for pipe */
#define ERR_138		138L		/* error in cmd attribute */
#define ERR_139		139L		/* absolute path for cmd required */
#define ERR_140		140L		/* could not create process */
#define ERR_141		141L		/* write on pipe failed */
#define ERR_142		142L		/* failed to reap child process */
#define ERR_143		143L		/* process killed by signal */
#define ERR_144		144L		/* process returned non-zero status */
#define ERR_145		145L		/* maximum hop count exceeded */
#define ERR_146		146L		/* transport driver not found */
#define ERR_147		147L		/* parse error in input address */
#define ERR_148		148L		/* smtp connection failure */
#define ERR_149		149L		/* smtp startup failed */
#define ERR_150		150L		/* smtp startup unsuccessful */
#define ERR_151		151L		/* SMTP timeout or read error */
#define ERR_152		152L		/* SMTP negative response */
#define ERR_153		153L		/* Remote host's storage is full */
#define ERR_154		154L		/* Error writing to remote host */
#define ERR_155		155L		/* Failed to read spooled message */
#define ERR_156		156L		/* Remote host returned bad address */
#define ERR_157		157L		/* Malformed domain literal */
#define ERR_158		158L		/* aliasfile lookup error */
#define ERR_159		159L		/* pathalias file lookup error */
#define ERR_160		160L		/* query_program cmd expansion error */
#define ERR_161 	161L		/* temporary failure execing command */
#define ERR_162		162L		/* transport not found */
#define ERR_163		163L		/* lost connect to BIND server */
#define ERR_164		164L		/* BIND server failure */
#define ERR_165		165L		/* BIND server packet format error */
#define ERR_166		166L		/* no SMTP service for host */
#define ERR_167		167L		/* unknown service or protocol */
#define ERR_168		168L		/* no valid MX records for host */
#define ERR_169		169L		/* MX record points to local host */
#define ERR_170		170L		/* uuname process creation error */
#define ERR_171		171L		/* queryprog command output error */
#define ERR_172		172L		/* no connection to remote SMTP */
#define ERR_173		173L		/* retry time expired */
#define ERR_174		174L		/* retry interval not reached */
#define ERR_175		175L		/* host locked */
#define ERR_176         176L            /* greybook hosts file lookup error */
#define ERR_177         177L            /* BIND - CNAME loop */
#define ERR_178		178L		/* host not found in the DNS */
#define ERR_179		179L		/* Malformed user part in remote address */
#define ERR_180		180L		/* BIND/DNS resolver initialisation failed */
#define ERR_181		181L		/* bad entry in reroute database */
#define ERR_182		182L		/* failed to open reroute database */
#define ERR_183		183L		/* reroute file lookup error */
#define ERR_184		184L		/* resolve timeout - too long in directors/routers */
#define ERR_185		185L		/* altuser passwd file is unparsable */
