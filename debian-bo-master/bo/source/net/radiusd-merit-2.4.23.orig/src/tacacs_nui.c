/* TACACS_NUI.C	X0.0
 | Read the NUI file into memory and sort it. When given a userID and password
 | look for it in the file's memory dump. If found (and password correct) return
 | the data to the caller. Otherwise return error code.
 */
#include <stdio.h>
#include "includes.h"

#define	NUI_FILE	"UTIL$:TACACS_NUI.DAT"
#define	LINESIZE	256
#define	MAX_USERS	2048	/* Maximum accounts we can hold */

/* Structure to hold the data for each user */
static struct TACACS_USERS
{
	char            username[LINESIZE],	/* the user's ID */
	                password[LINESIZE],
	                phone[LINESIZE];	/* Phone to call back. */
	int             flags;	/* In, out, Student, Research */
}               TacacsUsers[MAX_USERS];

static int      NumUsers;	/* How many users we have in database */




/*
 | Open for read the file and read its content. Distribute it between the
 | various structure elements and sort it alphbetically accroding to the
 | username. The username and password are kept all in upper case.
 */
read_tacacs_users_file ()
{
	int             Ifd;	/* Input file descriptor */
	char           *p,
	                line[LINESIZE],	/* Used to read file's data */
	                InOut,
	                StudentResearch;	/* Which type */

	NumUsers = 0;

	if ((Ifd = fopen (NUI_FILE, "r")) == NULL)
	{
		perror (NUI_FILE);
		return;
	}

	while (fgets (line, sizeof line, Ifd) != NULL)
	{
		if (*line == '*')
			continue;	/* Comment line */
		if ((p = strchr (line, '\n')) != NULL)
			*p = '\0';
/* Upcase the line */
		for (p = line; *p != '\0'; p++)
			if ((*p >= 'a') && (*p <= 'z'))
				*p -= ' ';

		if (sscanf (line, "%s %s %c %c %s", TacacsUsers[NumUsers].username,
		   TacacsUsers[NumUsers].password, &InOut, &StudentResearch,
			    TacacsUsers[NumUsers].phone) < 4)
		{
			printf ("Illegal line '%s'\n", line);
			continue;
		}
		TacacsUsers[NumUsers].flags = 0;
		if (InOut == 'P')
			TacacsUsers[NumUsers].flags |= F_PASSWORD;
		if (InOut == 'D')
			TacacsUsers[NumUsers].flags |= F_DIALBACK;
		if (StudentResearch == 'S')
			TacacsUsers[NumUsers].flags |= F_STUDENT;
		if (StudentResearch == 'R')
			TacacsUsers[NumUsers].flags |= F_RESEARCH;

		if (NumUsers++ >= MAX_USERS)
		{
			syslog (LOG_ERR, "No room for more users... Aborting\n");
			exit (1);
		}
	}
	fclose (Ifd);

	sort_users ();

	syslog (LOG_INFO, "  %d users read from authorization file '%s'\n",
		NumUsers, NUI_FILE);
}


/*
 | Sort the list of users in alphabetical order. Use the simple, old and good
 | neighbours sort.
 */
sort_users ()
{
	int             i,
	                j;	/* Sorting indexes */
	struct TACACS_USERS TempTacacsUsers;	/* Used to swap while sorting */

	for (i = NumUsers - 1; i >= 0; i--)
	{
		for (j = 0; j < i; j++)
		{
			if (strcmp (TacacsUsers[j].username,
				    TacacsUsers[j + 1].username) > 0)
			{
				/* Have to swap */
				memcpy (&TempTacacsUsers, &TacacsUsers[j],
					sizeof (struct TACACS_USERS));
				memcpy (&TacacsUsers[j], &TacacsUsers[j + 1],
					sizeof (struct TACACS_USERS));
				memcpy (&TacacsUsers[j + 1], &TempTacacsUsers,
					sizeof (struct TACACS_USERS));
			}
		}
	}

}

/*
 | Using a binary search locate the user's entry and return 0 if not found, 1
 | if found. If found and password is ok then return its type and phone numbe
 | (if a dial-back one).
 */
get_user_entry (username, password, flags, phone)
char           *username,		/* Username to  look for */
               *password,	/* Password to cxheck */
               *flags,		/* in/Out, Student/Research */
               *phone;		/* Phone number if this is an outcall */
{
	int             i,
	                status,
	                LowerBound,
	                UpperBound;
	static int      Median;
	char           *p,
	                TempUsername[LINESIZE],
	                TempPassword[LINESIZE];	/* So we do not ruin the
						 * originals */

	if (NumUsers < 1)
		return 0;	/* No users on list */

/* Upper-case the username and password */
	strcpy (TempUsername, username);
	strcpy (TempPassword, password);

	for (p = TempUsername; *p != '\0'; p++)
		if ((*p >= 'a') && (*p <= 'z'))
			*p -= ' ';
	for (p = TempPassword; *p != '\0'; p++)
		if ((*p >= 'a') && (*p <= 'z'))
			*p -= ' ';

/* Clear upper and lower bounds */
	LowerBound = 0;
	UpperBound = NumUsers - 1;

	while (LowerBound != UpperBound)
	{
		Median = (UpperBound - LowerBound) / 2 + LowerBound;
		if ((status = strcmp (TacacsUsers[Median].username, TempUsername)) == 0)
		{
/* Found - copy and return */
			if (strcmp (TacacsUsers[Median].password, TempPassword) != 0)
				return 0;	/* Not correct... */
			*flags = TacacsUsers[Median].flags;
			if (TacacsUsers[Median].flags & F_DIALBACK)
				strcpy (phone, TacacsUsers[Median].phone);
			return 1;
		}
/* Not found - check in which half */
		if (status > 0)
			UpperBound = Median;
		else
		{
			if (LowerBound == Median)
			{
				if (strcmp (TacacsUsers[UpperBound].username, TempUsername) == 0)
				{
					if (strcmp (TacacsUsers[UpperBound].password, TempPassword) != 0)
						return 0;	/* Not correct... */
					*flags = TacacsUsers[UpperBound].flags;
					if (TacacsUsers[UpperBound].flags & F_DIALBACK)
						strcpy (phone, TacacsUsers[UpperBound].phone);
					return 1;
				}
				return 0;
			}
			LowerBound = Median;
		}
	}
	return 0;
}
