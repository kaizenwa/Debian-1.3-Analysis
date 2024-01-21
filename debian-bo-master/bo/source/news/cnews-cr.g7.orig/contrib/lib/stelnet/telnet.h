#ifndef TELNET_H__
#define TELNET_H__

/* some telnet command codes -- RFC854 */
#define IAC	'\377'	/* "Interpret As Command" escape */
#define DONT	'\376'	/* stop performing option */
#define DO	'\375'	/* start performing option */
#define WONT	'\374'	/* refusal to perform option */
#define WILL	'\373'	/* desire to start performing */

/* telnet options */
#define ECHO	'\001'	/* telnet ECHO option -- RFC857 */
#define NO_GA	'\003'	/* telnet SUPPRESSS-GO-AHEAD option -- RFC858 */

/* general useful definitions */
#define CR	'\015'	/* ASCII carriage return */
#define LF	'\012'	/* ASCII newline */
#define NUL	'\000'	/* ASCII nul */

#endif /* TELNET_H__ */ /* Do not add anything after this line */
