gethostname (char *node, int nodelen)
{
	char	*p, tmp[32];

	strcpy (tmp, getenv("SYS$NODE"));	/* gets NODENAME:: */
	if (p = strchr (tmp, ':'))		/* chop off "::" */
	    *p = '\0';

	strncpy (node, tmp, nodelen);
	node[nodelen] = '\0';
}
               