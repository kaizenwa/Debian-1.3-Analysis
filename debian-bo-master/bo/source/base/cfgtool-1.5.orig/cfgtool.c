/*
 * File:	cfgtool.c
 * Purpose:	Tool for maintaining system configuration variables.
 * Author:	Lars Wirzenius
 * Version:	"@(#)cfgtool:$Id: cfgtool.c,v 1.11 1996/11/05 21:36:53 liw Exp $"
 */

#include <stdio.h>
#include <stdlib.h>
#include <publib.h>

/*
 * Version number of this program and the cfgtool library.
 */
static char version[] = "cfgtool version 1.4 by Lars Wirzenius";


#ifndef DEFAULT_REPOSITORY
#define DEFAULT_REPOSITORY	"/etc/cfgtool.d"
#endif


static int do_repository(char **argv, Cfgtool *r) {
	if (cfgtool_name_repository(r, argv[0]) == -1) {
		errormsg(0, -1, "couldn't set name of repository to %s",
			argv[0]);
		return -1;
	}
	return 0;
}


static int do_exists(char **argv, Cfgtool *r) {
	return cfgtool_exists(r, argv[0]);
}


static int do_get(char **argv, Cfgtool *r) {
	void *value;
	size_t size;

	if (cfgtool_get(r, argv[0], &value, &size) == -1) {
		errormsg(0, 0, "variable %s does not exist", argv[0]);
		return -1;
	}
	(void) fwrite(value, 1, size, stdout);
	return 0;
}


static int do_set(char **argv, Cfgtool *r) {
	if (cfgtool_set(r, argv[0], argv[1], strlen(argv[1])) == -1) {
		errormsg(0, -1, "couldn't set variable %s", argv[0]);
		return -1;
	}
	return 0;
}


static int create_or_set_all(char **argv, int force_set, Cfgtool *r) {
	FILE *f;
	int x;
	
	if (strcmp(argv[0], "-") == 0)
		f = stdin;
	else {
		f = fopen(argv[0], "r");
		if (f == NULL) {
			errormsg(0, -1, "couldn't open %s for reading", 
				argv[0]);
			return -1;
		}
	}

	x = cfgtool_read_all(r, f, force_set);
	if (x == -1)
		errormsg(0, 0, "error reading and setting variables");

	if (f != stdin && fclose(f) == EOF) {
		errormsg(0, -1, "error closing %s", argv[0]);
		return -1;
	}
	
	return x;
}


static int do_create_all(char **argv, Cfgtool *r) {
	return create_or_set_all(argv, 0, r);
}


static int do_set_all(char **argv, Cfgtool *r) {
	return create_or_set_all(argv, 1, r);
}


static int do_list_all(char **argv, Cfgtool *r) {
	return cfgtool_write_all(r, stdout);
}


static int do_list(char **argv, Cfgtool *r) {
	int i;
	char *name;
	
	for (i = 0; cfgtool_ith_name(r, i, &name) == 1; ++i) {
		(void) printf("%s\n", name);
		free(name);
	}
	return 0;
}


static int do_is_true(char **argv, Cfgtool *r) {
	return cfgtool_is_true(r, argv[0]);
}


static int do_is_false(char **argv, Cfgtool *r) {
	return cfgtool_is_false(r, argv[0]);
}


static int do_set_help(char **argv, Cfgtool *r) {
#if 0
	char path[FILENAME_MAX];
	FILE *f;
	
	var_to_help(argv[0], argv[2], path);
	f = fopen(path, "w");
	if (f == NULL) {
		errormsg(0, -1, "error opening `%s' for writing", path);
		return -1;
	}
	if (cat(argv[1], f) == -1) {
		(void) fclose(f);
		return -1;
	}
#endif
	return 0;
}


static int do_get_help(char **argv, Cfgtool *r) {
#if 0
	char path[FILENAME_MAX];
	FILE *f;
	
	var_to_help(argv[0], getenv("LANG"), path);
	f = fopen(path, "r");
	if (f == NULL) {
		var_to_help(argv[0], NULL, path);
		f = fopen(path, "r");
		if (f == NULL) {
			errormsg(0, -1, "can't open help file %s", path);
			return -1;
		}
	}
	(void) fclose(f);	/* let's hope we can open it again */
	return cat(path, stdout);
#endif
	return -1;
}


static int do_create(char **argv, Cfgtool *r) {
	if (do_exists(argv, r) == 0) {
		errormsg(0, 0, "variable %s exists already", argv[0]);
		return -1;
	}
	return do_set(argv, r);
}


static int do_destroy(char **argv, Cfgtool *r) {
	return cfgtool_destroy(r, argv[0]);
}


static int do_lock(char **argv, Cfgtool *r) {
	if (cfgtool_lock(r) == -1)
		return -1;
	cfgtool_keep_lock(r, 1);
	return 0;
}


static int do_unlock(char **argv, Cfgtool *r) {
	return cfgtool_unlock(r);
}


static int do_locked(char **argv, Cfgtool *r) {
	cfgtool_assume_lock(r);
	cfgtool_keep_lock(r, 0);
	return 0;
}


static int do_init_repository(char **argv, Cfgtool *r) {
	if (cfgtool_name_repository(r, argv[0]) == -1)
		return -1;
	return cfgtool_create_disk_repository(r);
}


static int do_version(char **argv, Cfgtool *r) {
	(void) printf("%s\n", version);
	return 0;
}


static struct cmd {
	const char *cmd;
	int args;
	int (*fun)(char **, Cfgtool *);
	int needs_lock;
	int needs_repo;
} tab[] = {
	{ "--init-repository", 1, do_init_repository, 0, 0 },
	{ "--repository", 1, do_repository, 0, 0 },
	{ "--get", 1, do_get, 0, 1 },
	{ "--set", 2, do_set, 1, 1 },
	{ "--create", 2, do_create, 1, 1 },
	{ "--destroy", 1, do_destroy, 1, 1 },
{ "--set-help", 3, do_set_help, 1, 0 },
{ "--get-help", 1, do_get_help, 0, 0 },
	{ "--is-true", 1, do_is_true, 0, 1 },
	{ "--is-false", 1, do_is_false, 0, 1 },
	{ "--lock", 0, do_lock, 0, 0 },
	{ "--unlock", 0, do_unlock, 0, 0 },
	{ "--locked", 0, do_locked, 0, 0 },
	{ "--list", 0, do_list, 0, 1 },
	{ "--exists", 1, do_exists, 0, 1 },
	{ "--list-all", 0, do_list_all, 0, 1 },
	{ "--set-all", 1, do_set_all, 1, 1 },
	{ "--create-all", 1, do_create_all, 1, 1 },
	{ "--version", 0, do_version, 0, 0 },
};
static const int tabsize = sizeof(tab) / sizeof(*tab);

static struct cmd *find_cmd(const char *name) {
	int i;
	
	for (i = 0; i < tabsize; ++i)
		if (strcmp(name, tab[i].cmd) == 0)
			return tab + i;
	errormsg(0, 0, "unknown command: %s", name);
	return NULL;
}


static int check_args(struct cmd *cmd, int i, int argc, char **argv) {
	if (i + cmd->args >= argc) {
		errormsg(0, 0, "%s needs %d arguments", cmd->cmd, cmd->args);
		return -1;
	}
	return 0;
}


static int check_lock(struct cmd *cmd, Cfgtool *r) {
	if (cmd->needs_lock && !cfgtool_is_locked(r))
		if (cfgtool_lock(r) == -1)
			return -1;
	return 0;
}


static int check_repo(struct cmd *cmd, Cfgtool **r) {
	if (cmd->needs_repo && !cfgtool_repository_is_loaded(*r)) {
		if (cfgtool_load_repository(*r) == -1)
			return -1;
	}
	return 0;
}


int main(int argc, char **argv) {
	int i, ret;
	Cfgtool *r;
	struct cmd *cmd;

	set_progname(argv[0], "cfgtool");
	__set_liberror(0);
	
	ret = 0;

	r = cfgtool_init_repository();
	if (r == NULL)
		ret = -1;
	if (ret == 0 && cfgtool_name_repository(r, DEFAULT_REPOSITORY) == -1)
		ret = -1;

	for (i = 1; ret == 0 && i < argc; ++i) {
		cmd = find_cmd(argv[i]);
		if (cmd == NULL)
			ret = -1;
		else {
			if (check_args(cmd, i, argc, argv) == -1 ||
			    check_lock(cmd, r) == -1 ||
			    check_repo(cmd, &r) == -1 ||
			    cmd->fun(argv+i+1, r) == -1)
				ret = -1;
			i += cmd->args;
		}
	}
			
	if (ret == 0 && cfgtool_repository_is_dirty(r)) {
		if (cfgtool_commit_repository(r) == -1)
			ret = -1;
	}

	if (r != NULL && cfgtool_close_repository(r) == -1)
		ret = -1;

	if (fflush(stdout) == EOF || ferror(stdout)) {
		errormsg(0, -1, "error writing to stdout");
		ret = -1;
	}
	
	return ret == -1 ? EXIT_FAILURE : 0;
}
