/*
 * CFINGERD
 * Configuration script
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include "cfingerd.h"
#include "proto.h"
#include "privs.h"

/* These are defines for the less than perfect configuration script parser */
#define	DISPLAY_FILES		1
#define	FINGER_PROGRAMS		2
#define	FINGER_FAKEUSERS	3

/* Hosts */
#define	TRUSTED			1
#define	REJECTED		2
#define	FORWARD			3

#define	FINGER_DISPLAY		1
#define	INTERNAL_CONFIG		2
#define	SYSTEM_LIST_SITES	3
#define	FINGER_STRINGS		4
#define INTERNAL_STRINGS	5
#define	SERVICES_HEADER		6
#define	SERVICES_POSITIONS	7
#define	SIGNAL_STRINGS		8

typedef struct {
    char *item;
    int value, section;
} TYPEVAL;

TYPEVAL finger_display[] = {
	{ "HEADER_FILE", SHOW_TOP, 1 },
	{ "FOOTER_FILE", SHOW_BOTTOM, 1 },
	{ "LOGIN_ID", SHOW_UNAME, 1 },
	{ "REAL_NAME", SHOW_REALNAME, 1 },
	{ "DIRECTORY", SHOW_DIR, 1 },
	{ "SHELL", SHOW_SHELL, 1 },
	{ "ROOM_NUMBER", SHOW_ROOM, 1 },
	{ "WORK_NUMBER", SHOW_WPHONE, 1 },
	{ "HOME_NUMBER", SHOW_HPHONE, 1 },
	{ "OTHER", SHOW_OTHER, 1 },
	{ "LAST_TIME_ON", SHOW_LTON, 1 },
	{ "IF_ONLINE", SHOW_IFON, 1 },
	{ "TIME_MAIL_READ", SHOW_LRMAIL, 1 },
	{ "DAY_MAIL_READ", SHOW_MRDATE, 1 },
	{ "ORIGINATION", SHOW_FROM, 1 },
	{ "PLAN", SHOW_PLAN, 2 },
	{ "PROJECT", SHOW_PROJECT, 2 },
	{ "PGP", SHOW_PGPKEY, 2 },
	{ "NO_NAME_BANNER", SHOW_NN_BANNER, 2 },
	{ "REJECTED_BANNER", SHOW_REJECTED, 2 },
	{ "SYSTEM_LIST", SHOW_SYSTEMLIST, 2 },
	{ "NO_USER", SHOW_NOUSER, 2 },
	{ "XFACE", SHOW_XFACE, 3 },
	{ NULL, 0, 0 }
};

TYPEVAL internal_config[] = {
	{ "ALLOW_MULTIPLE_FINGER_DISPLAY", SHOW_MULTFING, 2 },
	{ "ALLOW_SEARCHABLE_FINGER", SHOW_SEARCHFING, 2 },
	{ "ALLOW_NO_IP_MATCH_FINGER", SHOW_IP_MATCH, 2 },
	{ "ALLOW_USER_OVERRIDE", SHOW_OVERRIDE, 2 },
	{ "ALLOW_STRICT_FORMATTING", SHOW_STRICTFMT, 2 },
	{ "ALLOW_VERBOSE_TIMESTAMPING", SHOW_TIMESTAMP, 2 },
	{ "ALLOW_FINGER_FORWARDING", SHOW_FINGERFWD, 2 },
	{ "ALLOW_USERLIST_ONLY", SHOW_ULISTONLY, 2 },
	{ "ALLOW_NONIDENT_ACCESS", SHOW_NOBODY1413, 2 },
	{ "ALLOW_FINGER_LOGGING", SHOW_LOG, 3 },
	{ "ALLOW_LINE_PARSING", SHOW_PARSING, 3 },
	{ "ALLOW_USERLOG", SHOW_USERLOG, 3 },
	{ "ALLOW_EXECUTION", SHOW_EXEC, 3 },
	{ "ALLOW_FAKEUSER_FINGER", SHOW_FAKEUSER, 3 },
	{ "ONLY_SHOW_HEADERS_IF_FILE_EXISTS", SHOW_HEADERS_FE, 3 },
	{ "ONLY_CREATE_FINGERLOG_IF_FILE_EXISTS", SHOW_CREATE_FLG, 3 },
	{ NULL, 0, 0 }
};

TYPEVAL finger_strings[] = {
	{ "USER_NAME", D_USERNAME, 0 },
	{ "REAL_NAME", D_REALNAME, 0 },
	{ "DIRECTORY", D_DIRECTORY, 0 },
	{ "SHELL", D_SHELL, 0 },
	{ "ROOM_NUMBER", D_ROOM, 0 },
	{ "WORK_NUMBER", D_WORK_PHONE, 0 },
	{ "HOME_NUMBER", D_HOME_PHONE, 0 },
	{ "OTHER", D_OTHER, 0 },
	{ "PLAN", D_PLAN, 0 },
	{ "PROJECT", D_PROJECT, 0 },
	{ "NO_PLAN", D_NO_PLAN, 0 },
	{ "NO_PROJECT", D_NO_PROJECT, 0 },
	{ "NO_PGP", D_NO_PGPKEY, 0 },
	{ "PGPKEY", D_PGPKEY, 0 },
	{ "WAIT", D_WAIT, 0 },
	{ "XFACE", D_XFACE, 0 },
	{ "NO_XFACE", D_NO_XFACE, 0 },
	{ NULL, 0, 0 }
};

TYPEVAL internal_strings[] = {
	{ "NO_IP_HOST", D_IP_NO_MATCH, 0 },
	{ "RENICE_FATAL", D_NICE_FATAL, 0 },
	{ "STDIN_EMPTY", D_STDIN_EMPTY, 0 },
	{ "TRUSTED_HOST", D_TRUST_HOST, 0 },
	{ "REJECTED_HOST", D_REJECT_HOST, 0 },
	{ "ROOT_FINGER", D_ROOT_FINGER, 0 },
	{ "SERVICE_FINGER", D_SVC_FINGER, 0 },
	{ "USER_LIST", D_ULIST_FINGER, 0 },
	{ "FAKE_USER", D_FAKE_USER, 0 },
	{ "WHOIS_USER", D_WHOIS_USER, 0 },
	{ "FORWARD_DENY", D_FORWARD_DENY, 0 },
	{ "IDENT_CONREF", D_IDENT_CONREF, 0 },
	{ "IDENT_ILLEGAL", D_IDENT_ILLEGAL, 0 },
	{ "IDENT_TIMEOUT", D_IDENT_TIMEOUT, 0 },
	{ NULL, 0, 0 }
};

TYPEVAL signal_strings[] = {
	{ "SIGHUP", S_SIGHUP, 0 },
	{ "SIGINT", S_SIGINT, 0 },
	{ "SIGQUIT", S_SIGQUIT, 0 },
	{ "SIGILL", S_SIGILL, 0 },
	{ "SIGTRAP", S_SIGTRAP, 0 },
	{ "SIGABRT", S_SIGABRT, 0 },
	{ "SIGFPE", S_SIGFPE, 0 },
	{ "SIGUSR1", S_SIGUSR1, 0 },
	{ "SIGSEGV", S_SIGSEGV, 0 },
	{ "SIGUSR2", S_SIGUSR2, 0 },
	{ "SIGPIPE", S_SIGPIPE, 0 },
	{ "SIGALRM", S_SIGALRM, 0 },
	{ "SIGTERM", S_SIGTERM, 0 },
	{ "SIGCONT", S_SIGCONT, 0 },
	{ "SIGTSTP", S_SIGTSTP, 0 },
	{ "SIGTTIN", S_SIGTTIN, 0 },
	{ "SIGTTOU", S_SIGTTOU, 0 },
	{ "SIGIO", S_SIGIO, 0 },
	{ "SIGXCPU", S_SIGXCPU, 0 },
	{ "SIGXFSZ", S_SIGXFSZ, 0 },
	{ "SIGVTALRM", S_SIGVTALRM, 0 },
	{ "SIGPROF", S_SIGPROF, 0 },
	{ "SIGWINCH", S_SIGWINCH, 0 },
	{ NULL, 0, 0 }
};

int trusted_host_num;
int rejected_host_num;
int forward_host_num;
int fakeuser_num;
int num_finger_sites;
int num_headers, can_log;

FILE *file, *logfile;

/*
 * DO_FILES
 *
 * This routine simply takes the parsed string, and handles all filenames,
 * putting them into the correct place in the files structure.
 */
void do_files(char *str)
{
    char *line;
    int done = 0, type = 0, errors = 0;

    line = (char *) malloc(80);
    sscanf(str, "FILES %[^ =] = {\r\n", line);

    if (!(strncasecmp(line, "display_files", 13)))
	type = DISPLAY_FILES;
    else if (!(strncasecmp(line, "finger_programs", 15)))
	type = FINGER_PROGRAMS;
    else if (!(strncasecmp(line, "finger_fakeusers", 16)))
	type = FINGER_FAKEUSERS;

    free(line);

    while(!done) {
	line = (char *) malloc(80);

	fscanf(file, "%[^\r\n]\r\n", line);
	if (!(strncmp(line, "}", 1)))
	    done++;
	else if (line[0] != '#') {
	    if (type == DISPLAY_FILES) {
		char *element, *filename;

		element = (char *) malloc(80);
		filename = (char *) malloc(80);

		sscanf(line, "\t%[^\t]\t= \"%[^\",]\",\r\n", element, filename);

		/* Erk - this needs to be optimized...  :/ */
		if (!(strncasecmp(element, "PLAN", 4)))
		    strmcpy(&prog_config.plan_file, (char *) filename);
		else if (!(strncasecmp(element, "PROJECT", 7)))
		    strmcpy(&prog_config.project_file, (char *) filename);
		else if (!(strncasecmp(element, "PGP_KEY", 7)))
		    strmcpy(&prog_config.pgpkey_file, (char *) filename);
		else if (!(strncasecmp(element, "XFACE", 5)))
		    strmcpy(&prog_config.xface_file, (char *) filename);
		else if (!(strncasecmp(element, "NO_FINGER", 9)))
		    strmcpy(&prog_config.no_finger_file, (char *) filename);
		else if (!(strncasecmp(element, "LOGFILE", 7))) {
		    strmcpy(&prog_config.log_file, (char *) filename);

		    PRIV_ROOT_START
		    logfile = fopen(filename, "a+");
		    PRIV_ROOT_END

		    if (logfile)
			can_log = TRUE;
		    else {
			syslog(LOG_ERR, "Cannot log to %s: %s", filename, strerror(errno));
			can_log = FALSE;
		    }
		} else if (!(strncasecmp(element, "HEADER_DISPLAY", 14)))
		    strmcpy(&prog_config.top_display_file, (char *) filename);
		else if (!(strncasecmp(element, "FOOTER_DISPLAY", 14)))
		    strmcpy(&prog_config.bottom_display_file, (char *) filename);
		else if (!(strncasecmp(element, "NO_NAME_BANNER", 14)))
		    strmcpy(&prog_config.no_name_banner_file, (char *) filename);
		else if (!(strncasecmp(element, "NO_USER_BANNER", 14)))
		    strmcpy(&prog_config.no_user_banner_file, (char *) filename);
		else if (!(strncasecmp(element, "REJECTED_BANNER", 15)))
		    strmcpy(&prog_config.rejected_file, (char *) filename);
		else if (!(strncasecmp(element, "USERLOG", 7)))
		    strmcpy(&prog_config.userlog_file, (char *) filename);
		else if (!(strncasecmp(element, "MAILBOX", 7)))
		    strmcpy(&prog_config.mailbox_file, (char *) filename);
		else if (element[0] == '#') {
		    /* Ignore commented out lines */
		} else {
		    errors++;
		    printf("Option %s unknown in display_files.\n", element);
		}

		free(element);
		free(filename);
	    } else if (type == FINGER_PROGRAMS) {
		char *element, *filename;

		if (line[0] != '#') {
		    element = (char *) malloc(80);
		    filename = (char *) malloc(80);

		    sscanf(line, "\t%[^\t]\t= \"%[^\",]\",\r\n", element, filename);

		    if (!(strncasecmp(element, "FINGER", 6)))
			strmcpy(&prog_config.finger_program, (char *) filename);
		    else if (!(strncasecmp(element, "WHOIS", 5)))
			strmcpy(&prog_config.whois_program, (char *) filename);
		    else {
			errors++;
			printf("Option %s unknown in finger_programs.\n", element);
		    }

		    free(element);
		    free(filename);
		}
	    } else if (type == FINGER_FAKEUSERS) {
		char *element, *fakename, *boolean, *filename;

		element = (char *) malloc(80);
		fakename = (char *) malloc(80);
		boolean = (char *) malloc(80);
		filename = (char *) malloc(80);

		sscanf(line, "\t\"%[^\"]\", \"%[^\"]\", %[^,], \"%[^\"]\",\r\n",
			element, fakename, boolean, filename);

		strmcpy(&prog_config.fusers[fakeuser_num].user, (char *) element);
		strmcpy(&prog_config.fusers[fakeuser_num].script, (char *) filename);
		strmcpy(&prog_config.fusers[fakeuser_num].description, (char *) fakename);
		if (!(strncasecmp(boolean, "TRUE", 4)))
		    prog_config.fusers[fakeuser_num].searchable = TRUE;
		else if (!(strncasecmp(boolean, "FALSE", 5)))
		    prog_config.fusers[fakeuser_num].searchable = FALSE;
		else {
		    printf("Wrong boolean in FAKEUSERS!\n");
		    fflush(stdout);
		    exit(-1);
		}

		fakeuser_num++;

		free(element);
		free(fakename);
		free(boolean);
		free(filename);
	    }
	}

	free(line);
    }

    if (errors != 0) {
	printf("%d error%sw%s found in FILES section.  Please fix %s.\n\n",
	errors, (errors == 1) ? " " : "s ", (errors == 1) ? "as" : "ere",
	(errors == 1) ? "it" : "them");
	exit(0);
    }
}

/*
 * HAS
 *
 * This routine simply returns a TRUE or FALSE as to whether or not the
 * specified STRing has the SEARCH character in it.
 */
int has(char *str, char search)
{
    int i;

    for (i = 0; i < strlen(str); i++)
	if (str[i] == search)
	    return 1;

    return 0;
}

/*
 * DO_HOSTS
 *
 * Like the DO_FILES routine, this routine simply takes any hosts that
 * are listed as rejected or trusted, and places them in their correct
 * spots in the memory tree.
 */
void do_hosts(char *str)
{
    char *line;
    int done = 0, type = 0;
    char *cp;

    line = (char *) malloc(80);
    sscanf(str, "HOSTS %[^ =] = {\r\n", line);

    if (!(strncmp(line, "trusted", 7)))
	type = TRUSTED;
    else if (!(strncmp(line, "rejected", 8)))
	type = REJECTED;
    else if (!(strncmp(line, "finger_forward", 14)))
	type = FORWARD;

    free(line);

    while(!done) {
	line = (char *) malloc(80);

	fscanf(file, "%[^\r\n]\r\n", line);
	if (!(strncmp(line, "}", 1)))
	    done++;
	else if (line[0] != '#') {
	    char *host;

	    host = (char *) malloc(80);
	    sscanf(line, "\t%[^,\r\n],\r\n", host);

	    for (cp=host; *cp; cp++)
		if (isupper(*cp))
		    *cp = tolower (*cp);

	    if (type == TRUSTED)
		strmcpy(&prog_config.trusted[trusted_host_num++], host);
	    else if (type == REJECTED)
		strmcpy(&prog_config.rejected[rejected_host_num++], host);
	    else if (type == FORWARD)
		strmcpy(&prog_config.forward[forward_host_num++], host);

	    free(host);
	}
    }
}

/*
 * DO_CONFIG
 *
 * This routine simply takes a string, and reads through the configuration
 * file for the configurations that are needed.
 */
void do_config(char *str)
{
    char *line;
    int done = 0, type = 0, errors = 0;

    line = (char *) malloc(80);
    sscanf(str, "CONFIG %[^ =] = {\r\n", line);

    /* Argh - I need to optimize THIS too! */
    if (!(strncasecmp(line, "finger_display", 14)))
	type = FINGER_DISPLAY;
    else if (!(strncasecmp(line, "internal_config", 15)))
	type = INTERNAL_CONFIG;
    else if (!(strncasecmp(line, "system_list_sites", 17)))
	type = SYSTEM_LIST_SITES;
    else if (!(strncasecmp(line, "finger_strings", 14)))
	type = FINGER_STRINGS;
    else if (!(strncasecmp(line, "internal_strings", 16)))
	type = INTERNAL_STRINGS;
    else if (!(strncasecmp(line, "services_header", 15)))
	type = SERVICES_HEADER;
    else if (!(strncasecmp(line, "services_positions", 18)))
	type = SERVICES_POSITIONS;
    else if (!(strncasecmp(line, "signal_strings", 14)))
	type = SIGNAL_STRINGS;

    free(line);

    while(!done) {
	line = (char *) malloc(80);

	fgets(line, 80, file);
	if (!(strncmp(line, "}", 1)))
	    done++;
	else if (line[0] != '#') {
	    if (type == FINGER_DISPLAY) {
		char *element, *opt1, *opt2, *opt3;
		int x, found = FALSE;

		element = (char *) malloc(80);
		opt1 = (char *) malloc(80);
		opt2 = (char *) malloc(80);
		opt3 = (char *) malloc(80);

		sscanf(line, "\t%[+-]%[^\t =]\t = \133%[^,], %[^\135,]\135,",
			opt3, element, opt1, opt2);

		for (x = 0; ((finger_display[x].item != NULL) &&
			     (!found)); x++) {
		    if (!(strncasecmp(element, finger_display[x].item,
			 strlen(finger_display[x].item)))) {
			if (!(strncasecmp(opt1, "TRUE", 4))) {
			    if (finger_display[x].section == 1)
				prog_config.config_bits1 |= finger_display[x].value;
			    else if (finger_display[x].section == 2)
				prog_config.config_bits2 |= finger_display[x].value;
			    else
				prog_config.config_bits3 |= finger_display[x].value;

			    found = TRUE;
			}

			if (!(strncasecmp(opt2, "TRUE", 4))) {
			    if (finger_display[x].section == 1)
				prog_config.local_config_bits1 |= finger_display[x].value;
			    else if (finger_display[x].section == 2)
				prog_config.local_config_bits2 |= finger_display[x].value;
			    else
				prog_config.local_config_bits3 |= finger_display[x].value;

			    found = TRUE;
			}

			if (!(strncasecmp(opt1, "FALSE", 5)))
			    found = TRUE;

			if (!(strncasecmp(opt2, "FALSE", 5)))
			    found = TRUE;

			if (opt3[0] == '+') {
			    if (finger_display[x].section == 1)
				prog_config.override_bits1 |= finger_display[x].value;
			    else if (finger_display[x].section == 2)
				prog_config.override_bits2 |= finger_display[x].value;
			    else
				prog_config.override_bits3 |= finger_display[x].value;

			    found = TRUE;
			}
		    }
		}

		if (!found) {
		    errors++;
		    printf("Option %s unknown in finger_display.\n", element);
		}

		free(element);
		free(opt1);
		free(opt2);
		free(opt3);
	    } else if (type == INTERNAL_CONFIG) {
		char *element, *opt;
		int x, found = FALSE;

		element = (char *) malloc(80);
		opt = (char *) malloc(2);

		sscanf(line, "\t%[+-]%[^,\r\n],\r\n", opt, element);

		for (x = 0; ((internal_config[x].item != NULL) &&
			     (!found)); x++) {
		    if (!(strncasecmp(element, internal_config[x].item,
			 strlen(internal_config[x].item)))) {
			if (opt[0] == '+') {
			    if (internal_config[x].section == 1)
				prog_config.config_bits1 |= internal_config[x].value;
			    else if (internal_config[x].section == 2)
				prog_config.config_bits2 |= internal_config[x].value;
			    else if (internal_config[x].section == 3)
				prog_config.config_bits3 |= internal_config[x].value;
			}

			found = TRUE;
		    }
		}

		if (!found) {
		    errors++;
		    printf("Option %s unknown in internal_config.\n", element);
		}

		free(element);
		free(opt);
	    } else if (type == SYSTEM_LIST_SITES) {
		char *element;

		element = (char *) malloc(80);

		sscanf(line, "\t%[^,\r\n],\r\n", element);

		strmcpy(&prog_config.finger_sites[num_finger_sites++], element);

		free(element);
	    } else if (type == FINGER_STRINGS) {
		char *element, *string;
		int x, found = FALSE;

		element = (char *) malloc(80);
		string = (char *) malloc(80);

		sscanf(line, "\t%[^\t=] = \"%[^\",\r\n]\",\r\n",
			element, string);

		for (x = 0; ((finger_strings[x].item != NULL) &&
			     (!found)); x++) {
		    if (!(strncasecmp(element, finger_strings[x].item,
			 strlen(finger_strings[x].item)))) {
			strmcpy(&prog_config.p_strings[finger_strings[x].value],
				string);

			found = TRUE;
		    }
		}

		if (!found) {
		    errors++;
		    printf("Option %s unknown in finger_strings.\n", element);
		}

		free(element);
		free(string);
	    } else if (type == INTERNAL_STRINGS) {
		char *element, *string;
		int x, found = FALSE;

		element = (char *) malloc(80);
		string = (char *) malloc(80);

		sscanf(line, "\t%[^\t=] = \"%[^\",\r\n]\",\r\n",
			element, string);

		for (x = 0; ((internal_strings[x].item != NULL) &&
			     (!found)); x++) {
		    if (!(strncasecmp(element, internal_strings[x].item,
			 strlen(internal_strings[x].item)))) {
			strmcpy(&prog_config.p_strings[internal_strings[x].value],
				string);

			found = TRUE;
		    }
		}

		if (!found) {
		    errors++;
		    printf("Option %s unknown in internal_strings.\n", element);
		}

		free(element);
		free(string);
	    } else if (type == SERVICES_HEADER) {
		char *element;
		int counter;

		element = (char *) malloc(80);

		sscanf(line, "%[^\r\n]\r\n", element);

		if (strlen(element) > 3) {
		    counter = strlen(element);
		    if (!has(element, '%')) {
			element[counter++] = '\n';
			element[counter] = '\0';
		    }

		    if (!has(element, '%')) {
			if (element[0] != 0)
			    strmcpy(&prog_config.services.header[num_headers++], element);
			else
			    strmcpy(&prog_config.services.header[num_headers++], " ");
		    } else
			strmcpy(&prog_config.services.display_string, element);

		    free(element);
		} else
		    prog_config.services.header[num_headers++] = " \n";
	    } else if (type == SERVICES_POSITIONS) {
		char *element, *num;

		element = (char *) malloc(80);
		num = (char *) malloc(80);

		sscanf(line, "\t%[^\t=] = %[^,\r\n],\r\n", element, num);

		if (!(strncasecmp(element, "USER", 4)))
		    prog_config.services.name_pos = atoi(num);
		else if (!(strncasecmp(element, "SERVICE", 7)))
		    prog_config.services.service_pos = atoi(num);
		else if (!(strncasecmp(element, "SEARCH", 6)))
		    prog_config.services.search_pos = atoi(num);
		else {
		    errors++;
		    printf("Option %s unknown in services_positions.\n", element);
		}

		free(element);
		free(num);
	    } else if (type == SIGNAL_STRINGS) {
		char *element, *string;
		int x, found = FALSE;

		element = (char *) malloc(80);
		string = (char *) malloc(80);

		sscanf(line, "\t%[^\t=] = \"%[^\",\r\n]\",\r\n",
			element, string);

		for (x = 0; ((signal_strings[x].item != NULL) &&
			     (!found)); x++) {
		    if (!(strncasecmp(element, signal_strings[x].item,
			 strlen(signal_strings[x].item)))) {
			strmcpy(&prog_config.siglist[signal_strings[x].value],
				string);

			found = TRUE;
		    }
		}

		if (!found) {
		    errors++;
		    printf("Option %s unknown in signal_strings.\n", element);
		}
	    }
	}
    }

    if (errors != 0) {
	printf("%d error%sw%s found in CONFIG section.  Please fix %s.\n\n",
	errors, (errors == 1) ? " " : "s ", (errors == 1) ? "as" : "ere",
	(errors == 1) ? "it" : "them");
	exit(0);
    }
}

/*
 * READ_CONFIGURATION
 *
 * This routine reads the configuration file.
 */
void read_configuration(void)
{
    PRIV_ROOT_START
    file = fopen(CFINGER_CONF, "r");
    PRIV_ROOT_END

    trusted_host_num = rejected_host_num = fakeuser_num = num_finger_sites = 0;
    forward_host_num = 0;
    num_headers = 1;

    if (file)
	while(!feof(file)) {
	    char *line, ch = fgetc(file);

	    /* Patch by Kevin Rosenberg 10/21/95 */
	    if (ch == EOF)
		break;

	    ungetc(ch, file);
	    line = (char *) malloc(80);
	    if ((ch != '#') && (ch != '\'')) {
		fgets(line, 80, file);
		if (!(strncasecmp(line, "FILES ", 6))) {
		    do_files(line);
		} else if (!(strncasecmp(line, "HOSTS ", 6))) {
		    do_hosts(line);
		} else if (!(strncasecmp(line, "CONFIG ", 7))) {
		    do_config(line);
		}
	    } else
		fgets(line, 80, file);

	    free(line);
	}
    else {
	printf("No cfingerd.conf file present.  Check your setup.\n");
	exit(1);
    }
}

void check_blank_configurations(void)
{
    if (trusted_host_num == 0) {
	trusted_host_num = 1;
	prog_config.trusted[0] = (char *) malloc(10);
	sprintf(prog_config.trusted[0], "localhost");
    }

    if (rejected_host_num == 0) {
	rejected_host_num = 1;
	prog_config.rejected[0] = (char *) malloc(8);
	sprintf(prog_config.rejected[0], "0.0.0.0");
    }

    if (forward_host_num == 0) {
	forward_host_num = 1;
	prog_config.forward[0] = (char *) malloc(10);
	sprintf(prog_config.forward[0], "localhost");

	/* Patch by Larry Daffner (vizzie@airmail.net) */
	if (prog_config.config_bits2 & SHOW_FINGERFWD)
	    prog_config.config_bits2 &= ~SHOW_FINGERFWD;
    }

    if (fakeuser_num == 0) {
	prog_config.fusers[fakeuser_num].user = (char *) malloc(5);
	prog_config.fusers[fakeuser_num].script = (char *) malloc(10);
	prog_config.fusers[fakeuser_num].description = (char *) malloc(5);

	sprintf(prog_config.fusers[fakeuser_num].user, "None");
	sprintf(prog_config.fusers[fakeuser_num].script, "/dev/null");
	sprintf(prog_config.fusers[fakeuser_num].description, "None");
	prog_config.fusers[fakeuser_num].searchable = FALSE;
	fakeuser_num = 1;
    }

    if (num_finger_sites == 0) {
	prog_config.finger_sites[num_finger_sites] = (char *) malloc(10);
	sprintf(prog_config.finger_sites[num_finger_sites], "localhost");
	num_finger_sites = 1;
    }

    if (prog_config.mailbox_file == NULL) {
	prog_config.mailbox_file = (char *) malloc(17);
	sprintf(prog_config.mailbox_file, "/usr/spool/mail/$USER");
    }
}
