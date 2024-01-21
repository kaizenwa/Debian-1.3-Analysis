/*
 * macos.c -- support routines for Macintosh OS, using MacTCP
 *
 * Antony Courtney, 7/20/93
 */

int ilu_mac_accept(int srcfd,struct sockaddr *addr,int *len)
{
	/* eventually call to WaitForConnection() */
	return 0;
}

int ilu_mac_bind(int s,struct sockaddr_in *name,int namelen)
{
	/* we don't do anything here--addresses will be assigned
	 * automatically
	 */
	return 0;
}

int ilu_mac_connect(int s,struct sockaddr_in *name,int namelen)
{
	OSErr err;

	err=OpenConnection((unsigned long) s,
		name->sin_addr.s_addr,name->sin_port,20);
	if (err!=noErr) {
		printf("error creating stream: %d\n",err);
		exit(1);
	}
	return 0;
}

int ilu_mac
