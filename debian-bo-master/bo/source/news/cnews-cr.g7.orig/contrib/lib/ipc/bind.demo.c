#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

main()
{
	int s1, s2;
	int one = 1, saddrlen;
	struct sockaddr_in sin;
	struct sockaddr saddr;

	s1 = socket(AF_INET, SOCK_STREAM, 0);
	if (s1 < 0) {
		perror("socket1");
		exit(1);
	}
	sin.sin_family = AF_INET;
	sin.sin_port = htons(11992);
	sin.sin_addr.s_addr = INADDR_ANY;
	if (bind(s1, &sin, sizeof sin) < 0) {
		perror("bind1");
		exit(1);
	}
	(void) listen(s1, 1);
	sin.sin_addr.s_addr = inet_addr("127.0.0.1");

	if (fork() == 0) {
		(void) close(s1);
		s1 = socket(AF_INET, SOCK_STREAM, 0);
		(void) connect(s1, &sin, sizeof sin);
		(void) sleep(1);
		exit(0);
	}

	saddrlen = sizeof saddr;
	s2 = accept(s1, &saddr, &saddrlen);
	(void) close(s1);
	(void) close(s2);
	(void) sleep(1);
	s1 = socket(AF_INET, SOCK_STREAM, 0);
	if (setsockopt(s1, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one) < 0) {
		perror("setsockopt");
		exit(1);
	}
	sin.sin_addr.s_addr = INADDR_ANY;
	if (bind(s1, &sin, sizeof sin) != 0)
		perror("bind2");
	exit(0);
}
