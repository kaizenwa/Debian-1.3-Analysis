#ifndef _SYS_LOG_H
#define	_SYS_LOG_H

#include <sys/cdefs.h>
#define	LOG_EMERG	0	
#define	LOG_ALERT	1	
#define	LOG_CRIT	2	
#define	LOG_ERR		3	
#define	LOG_WARNING	4	
#define	LOG_NOTICE	5	
#define	LOG_INFO	6	
#define	LOG_DEBUG	7	

#define	LOG_PRIMASK	0x07	
				
#define	LOG_PRI(p)	((p) & LOG_PRIMASK)
#define	LOG_MAKEPRI(fac, pri)	(((fac) << 3) | (pri))

#define	LOG_KERN	(0<<3)	
#define	LOG_USER	(1<<3)	
#define	LOG_MAIL	(2<<3)	
#define	LOG_DAEMON	(3<<3)	
#define	LOG_AUTH	(4<<3)	
#define	LOG_SYSLOG	(5<<3)	
#define	LOG_LPR		(6<<3)	
#define	LOG_NEWS	(7<<3)	
#define	LOG_UUCP	(8<<3)	
#define	LOG_CRON	(9<<3)	
#define	LOG_AUTHPRIV	(10<<3)	

#define	LOG_NFACILITIES	10
#define	LOG_FACMASK	0x03f8	
#define	LOG_FAC(p)	(((p) & LOG_FACMASK) >> 3)

#define	LOG_MASK(pri)	(1 << (pri))		
#define	LOG_UPTO(pri)	((1 << ((pri)+1)) - 1)	

__BEGIN_DECLS


void	closelog (void);
void	openlog (const char *, int, int);
int	setlogmask (int);
void	syslog (int, const char *, ...);

__END_DECLS


#endif /* _SYS_LOG_H */
