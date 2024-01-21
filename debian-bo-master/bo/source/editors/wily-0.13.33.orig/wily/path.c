#include "wily.h"
#include <pwd.h>

static char*	gettilde(const char*s);
static void	pathclean(char*path);

/*
 * Expand 'orig' into 'dest', using 'expansion'.
 * Expansion must take a char*, and return a static  char*,
 * i.e. one we don't have to free (and can't keep).
 */
static void
expand(char*dest, char*orig, char*(*expansion)(const char*)){
	Path	key;
	char	*val;
	char	*slash;
	
	assert(orig[0]=='$' || orig[0]=='~');
	strcpy(key,orig+1);
	if( (slash=strchr(key,'/')) ) {
		*slash = 0;
	}
	val = (*expansion)(key);
	if(slash)
		*slash = '/';
	if(val){
		sprintf(dest, "%s%s", val, slash? slash: "");
	} else {
		strcpy(dest, orig);
	}
}

/* Convert a 'label' to a 'path'.  Paths are always absolute. */
void
label2path(char*path, char*label) {
	if(!label){
		strcpy(path,wilydir);
		return;
	}
	switch(label[0]) {
	case '$':	expand(path, label, getenv); break;
	case '~':	expand(path, label, gettilde); break;
	case '/':	strcpy(path, label); break;
	default:	sprintf(path, "%s%s", wilydir, label); break;
	}
	pathclean(path);
}

/*********************************************************
	static functions
*********************************************************/

/* Clean up 'path' by removing components with '.' or '..' */
static void
pathclean(char*path){
	/* todo */
}

/* Clean up 'label' by removing components with '.' or '..' */
void
labelclean(char*label){
	Path	copy_path;
	char	*path,*new_path;
	char	c;
	
	/* Make a copy of the source path since we may need to modify it. */
	strcpy(copy_path, label);
	path = copy_path;
	new_path = label;
	
	/* leading component */
	while (path[0] != '/' && path[0] != '\0') {
		*new_path++ = *path++;
	}
	
	/* first slash */
	if(path[0] == '/')
		*new_path++ = *path++;
	
	/* Expand each slash-separated pathname component. */
	while (*path != '\0') {
		switch(path[0]){
		case '/':
			path++;
			continue;
		case '.':
			switch(path[1]){
			case '\0':
			case '/':	/* Ignore ".". */
				path++;
				continue;
			case '.':
				if (path[2] == '\0' || path[2] == '/') {
					path += 2;
					/* Ignore ".." at root. */
					if (new_path == label + 1)
						continue;
					/* Handle ".." by backing up. */
					while ((--new_path)[-1] != '/')
						;
					/* todo - handle $h/.. */
					continue;
				}
			}
		default:
			; /* pass */
		}
		/* Safely copy the next pathname component. */
		do {
			c = *new_path++ = *path++;
		} while (c != '\0' && c != '/');
	}
	*new_path='\0';
}

static char*
gettilde(const char*s) {
	struct passwd *pw;

	pw = strlen(s) ? getpwnam(s) : getpwuid(getuid());
	return pw ? pw->pw_dir : 0;
}

