#include "../inet/inetprivate.h"

int __ivaliduser(FILE *hostf, u_long raddr, const char *luser,
		 const char *ruser);


int
_validuser(FILE *hostf, const char *rhost, const char *luser,
        const char *ruser, int baselen)
{
	struct hostent *hp;
	u_long addr;
	char **ap;

	if ((hp = gethostbyname(rhost)) == NULL)
		return (-1);
	for (ap = hp->h_addr_list; *ap; ++ap) {
		bcopy(*ap, &addr, sizeof(addr));
		if (__ivaliduser(hostf, addr, luser, ruser) == 0)
			return (0);
	}
	return (-1);
}
