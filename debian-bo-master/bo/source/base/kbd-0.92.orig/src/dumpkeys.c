/*
 * dumpkeys.c
 *
 * derived from version 0.81 - aeb@cwi.nl
 * Fix: escape quotes and backslashes in strings
 * Fix: after  dumpkeys > x; loadkeys x; dumpkeys > y
 *      the files x and y should be identical
 * Added: compose key support
 *
 * for 0.83: output a "+" for KT_LETTER
 * for 0.85: with -i option: also output MAX_DIACR
 * for 0.86: with -l option: also tell about synonyms
 * for 0.87: output "charset iso-8859-x" so that loadkeys
 *      can handle the output of dumpkeys -c
 * for 0.88: handle sparse keymaps
 */
#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <getopt.h>
#include <linux/types.h>
#include <linux/kd.h>
#include <linux/keyboard.h>
#include <sys/ioctl.h>
#include <string.h>
#include <errno.h>
#include "ksyms.h"

#ifndef KT_LETTER
#define KT_LETTER KT_LATIN
#endif

#ifndef MAX_NR_KEYMAPS
#define MAX_NR_KEYMAPS NR_KEYMAPS
#endif

#define VERSION "0.91"
extern int getfd();
static int fd;

char keymap_exists[MAX_NR_KEYMAPS];
int good_keymap[MAX_NR_KEYMAPS], keymapnr, allocct;

void get_keymaps(void) {
	int i, j;
	struct kbentry ke;

	keymapnr = allocct = 0;
	for (i=0; i<MAX_NR_KEYMAPS; i++) {
	    ke.kb_index = 0;
	    ke.kb_table = i;
	    j = ioctl(fd, KDGKBENT, (unsigned long)&ke);
	    if (j && errno != EINVAL) {
		fprintf(stderr, "KDGKBENT at index 0 in table %d: ", i);
		perror("");
		exit(1);
	    }
	    if (!j && ke.kb_value != K_NOSUCHMAP) {
		keymap_exists[i] = 1;
		good_keymap[keymapnr++] = i;
		if (ke.kb_value == K_ALLOCATED)
		  allocct++;
	    } else {
		keymap_exists[i] = 0;
	    }
	}
	if (keymapnr == 0) {
	    fprintf(stderr, "dumpkeys: cannot find any keymaps?\n");
	    exit(1);
	}
	if (good_keymap[0] != 0) {
	    fprintf(stderr,
		    "dumpkeys: plain map not allocated? very strange ...\n");
	    /* this is not fatal */
	}
}

void print_keymaps(void) {
	int i,m0,m;

	printf("keymaps ");
	for (i=0; i<keymapnr; i++) {
	    if (i)
	      printf(",");
	    m0 = m = good_keymap[i];
	    while (i+1 < keymapnr && good_keymap[i+1] == m+1)
	      i++, m++;
	    if (m0 == m)
	      printf("%d", m0);
	    else
	      printf("%d-%d", m0, m);
	}
	printf("\n");
}

int get_bind(u_char index, u_char table) {
	struct kbentry ke;

	ke.kb_index = index;
	ke.kb_table = table;
	if (ioctl(fd, KDGKBENT, (unsigned long)&ke)) {
		fprintf(stderr, "KDGKBENT at index %d in table %d: ",
			index, table);
		perror("");
		exit(1);
	}
	return ke.kb_value;
}

void print_keysym(int code, char numeric) {
	int t;
	int v;
	char *p;

	printf(" ");
	t = KTYP(code);
	v = KVAL(code);
	if (t > KT_LETTER) {
	        printf("U+%04x          ", code ^ 0xf000);
		return;
	}
	if (t == KT_LETTER) {
	        t = KT_LATIN;
		    printf("+");
	}
	if (
		!numeric &&
		t < syms_size &&
		v < syms[t].size &&
		(p = syms[t].table[v])[0]
	)
		printf("%-16s", p);
	else
		printf("0x%04x          ", code);
}

char valid_type(int t) {
	struct kbentry ke;
	char status;

	ke.kb_index = 0;
	ke.kb_table = 0;
	ke.kb_value = K(t, 0);
	status = (ioctl(fd, KDSKBENT, (unsigned long)&ke) == 0);
	return status;
}

u_char maximum_val(int t) {
	struct kbentry ke;
	int i;

	ke.kb_index = 0;
	ke.kb_table = 0;

	for (i = 0; i < 256; i++) {
		ke.kb_value = K(t, i);
		if (ioctl(fd, KDSKBENT, (unsigned long)&ke))
			break;
	}
	ke.kb_value = K_HOLE;
	ioctl(fd, KDSKBENT, (unsigned long)&ke); 	/* superfluous */

	return i - 1;
}

#define NR_TYPES 13
int maxval[NR_TYPES];

void show_short_info(void) {
	int i;

	printf("keycode range supported by kernel:           1 - %d\n",
	       NR_KEYS - 1);
	printf("max number of actions bindable to a key:         %d\n",
	       MAX_NR_KEYMAPS);
	get_keymaps();
	printf("number of keymaps in actual use:                 %d\n",
	       keymapnr);
	if (allocct)
	  printf("of which %d dynamically allocated\n", allocct);
	printf("ranges of action codes supported by kernel:\n");
	for (i = 0; i < NR_TYPES && valid_type(i); i++) {
	    maxval[i] = maximum_val(i);
	    printf("	0x%04x - 0x%04x\n", K(i, 0), K(i, maxval[i]));
	}
	printf("number of function keys supported by kernel: %d\n", MAX_NR_FUNC);
	printf("total space available for compose key definitions: %d bytes\n",
	        MAX_DIACR);
}

struct {
    char *name;
    int bit;
} modifiers[] = {
    { "shift",	KG_SHIFT  },
    { "altgr",	KG_ALTGR  },
    { "control",KG_CTRL   },
    { "alt",	KG_ALT    },
    { "shiftl",	KG_SHIFTL },
    { "shiftr",	KG_SHIFTR },
    { "ctrll",	KG_CTRLL  },
    { "ctrlr",	KG_CTRLR  }
};

void dump_symbols(void) {
	int t;
	int v;
	char *p;

	printf("Symbols recognized by dumpkeys:\n(numeric value, symbol)\n\n");
	for (t = 0; t < syms_size; t++)
		for (v = 0; v < syms[t].size; v++)
			if ((p = syms[t].table[v])[0])
				printf("0x%04x\t%s\n", K(t, v), p);
	printf("\nThe following synonyms are recognized:\n\n");
	for (t = 0; t < syn_size; t++)
	  printf("%-15s for %s\n", synonyms[t].synonym,
		 synonyms[t].official_name);
	printf("\nRecognized modifier names and their column numbers:\n");
	for (t = 0; t < sizeof(modifiers)/sizeof(modifiers[0]); t++)
	  printf("%s\t\t%3d\n", modifiers[t].name, 1 << modifiers[t].bit);
}

void print_mod(int x) {
        int t;

	for (t = 0; t < sizeof(modifiers)/sizeof(modifiers[0]); t++)
	  if (x & (1 << modifiers[t].bit))
	    printf("%s\t", modifiers[t].name);
}

void print_bind(int bufj, int i, int j, char numeric) {
	printf("\t");
	print_mod(j);
	printf("keycode %3d =", i);
	print_keysym(bufj, numeric);
	printf("\n");
}

void dump_keys(char full_table, char numeric) {
	int i, j, k;
	int buf[MAX_NR_KEYMAPS];
	int isletter, islatin, isasexpected;
	int typ, val;

	get_keymaps();
	print_keymaps();
	if (!keymapnr)
	  return;
	for (i = 1; i < NR_KEYS; i++) {
	    for (j = 0; j < keymapnr; j++)
	      buf[j] = get_bind(i, good_keymap[j]);

	    typ = KTYP(buf[0]);
	    val = KVAL(buf[0]);
	    islatin = (typ == KT_LATIN || typ == KT_LETTER);
	    isletter = (islatin &&
			((val >= 'A' && val <= 'Z') ||
			 (val >= 'a' && val <= 'z')));
	    isasexpected = 0;
	    if (isletter) {
		u_short defs[16];
		defs[0] = K(KT_LETTER, val);
		defs[1] = K(KT_LETTER, val ^ 32);
		defs[2] = defs[0];
		defs[3] = defs[1];
		for(j=4; j<8; j++)
		  defs[j] = K(KT_LATIN, val & ~96);
		for(j=8; j<16; j++)
		  defs[j] = K(KT_META, KVAL(defs[j-8]));

		for(j = 0; j < keymapnr; j++) {
		    k = good_keymap[j];
		    if ((k >= 16 && buf[j] != K_HOLE) || (k < 16 && buf[j] != defs[k]))
		      goto unexpected;
		}
		isasexpected = 1;
	    }
	  unexpected:

	    printf("keycode %3d =", i);
	    if (full_table) {
		for (j = 0; j < keymapnr; j++)
		  print_keysym(buf[j], numeric);
		printf("\n");
	    } else
	    if (isasexpected) {
		/* print only a single entry */
		/* suppress the + for ordinary a-zA-Z */
		print_keysym(K(KT_LATIN, val), numeric);
		printf("\n");
	    } else if (!islatin) {
		/* choose between single entry line followed by exceptions,
		   and long line followed by exceptions; avoid VoidSymbol */
		int bad = 0;
		int count = 0;
		for(j = 1; j < keymapnr; j++) {
		    if (buf[j] != buf[0])
		      bad++;
		    if (buf[j] != K_HOLE)
		      count++;
		}
		if (bad <= count) {
		    if (buf[0] != K_HOLE)
		      print_keysym(buf[0], numeric);
		    printf("\n");
		    for (j = 1; j < keymapnr; j++)
		      if (buf[j] != buf[0])
			print_bind(buf[j], i, good_keymap[j], numeric);
		} else {
		    for (j = 0; j < keymapnr && buf[j] != K_HOLE; j++)
		      print_keysym(buf[j], numeric);
		    printf("\n");
		    for ( ; j < keymapnr; j++)
		      if (buf[j] != K_HOLE)
			print_bind(buf[j], i, good_keymap[j], numeric);
		}
	    } else {
		for (j = 0; j < keymapnr && buf[j] != K_HOLE; j++)
		  print_keysym(buf[j], numeric);
		printf("\n");
		for ( ; j < keymapnr; j++)
		  if (buf[j] != K_HOLE)
		    print_bind(buf[j], i, good_keymap[j], numeric);
	    }
	}
}

void dump_funcs(void) {
	int i;
	struct kbsentry fbuf;
	char *p;

	for (i = 0; i < MAX_NR_FUNC; i++) {
		fbuf.kb_func = i;
		if (ioctl(fd, KDGKBSENT, (unsigned long)&fbuf)) {
		    if (errno == EINVAL && i > 0) /* an old kernel */
		      break;
		    fprintf(stderr, "KDGKBSENT at index %d: ", i);
		    perror("");
		    exit(1);
		}
		if (!fbuf.kb_string[0])
		        continue;
		printf("string %s = \"", syms[KT_FN].table[i]);
		for (p = fbuf.kb_string; *p; p++) {
		        if (*p == '"' || *p == '\\') {
			        putchar('\\'); putchar(*p);
			} else if (isgraph(*p) || *p == ' ')
				putchar(*p);
			else
				printf("\\%03o", *p);
		}
		printf("\"\n");
	}
}

#ifdef KDGKBDIACR
/* isgraph() does not know about iso-8859; printing the character
   unescaped makes the output easier to check. Maybe this should
   be an option. Use locale? */
void
outchar (unsigned char c) {
        printf("'");
        printf((c == '\'' || c == '\\') ? "\\%c"
	       : (isgraph(c) || c == ' ' || c >= 0200) ? "%c"
	       : "\\%03o", c);
	printf("'");
}

void dump_diacs(void) {
        struct kbdiacrs kd;
	int i;

	if(ioctl(fd, KDGKBDIACR, (unsigned long)&kd)) {
	    fprintf(stderr, "KDGKBDIACR failed\n");
	    perror("");
	    exit(1);
	}
	for (i = 0; i < kd.kb_cnt; i++) {
	        printf("compose ");
		outchar(kd.kbdiacr[i].diacr);
		printf(" ");
		outchar(kd.kbdiacr[i].base);
		printf(" to ");
		outchar(kd.kbdiacr[i].result);
		printf("\n");
	}
}
#endif        

void usage(void) {
	fprintf(stderr, "\
dumpkeys version " VERSION "\

usage: dumpkeys [options...]

valid options are:

	-h --help	display this help text
	-i --short-info	display information about keyboard driver
	-l --long-info	display above and symbols known to loadkeys
	-n --numeric	display keytable in hexadecimal notation
	-f --full-table	don't use short-hand notations
	   --funcs-only	display only the function key strings
	   --keys-only	display only key bindings
%s	-c --charset=iso-8859-{1,2,3,4,5,7,8}
			interpret character action codes to be from the
			specified character set
",
#ifdef KDGKBDIACR
"           --compose-only  display only compose key combinations\n"
#else
""
#endif
);
	exit(1);
}

int
main (int argc, char *argv[]) {
	const char *short_opts = "hilsnfc:";
	const struct option long_opts[] = {
		{ "help",	no_argument,		NULL, 'h' },
		{ "short-info",	no_argument,		NULL, 'i' },
		{ "long-info",	no_argument,		NULL, 'l' },
		{ "numeric",	no_argument,		NULL, 'n' },
		{ "full-table",	no_argument,		NULL, 'f' },
		{ "funcs-only",	no_argument,		NULL, 't' },
		{ "keys-only",	no_argument,		NULL, 'k' },
#ifdef KDGKBDIACR
		{ "compose-only",no_argument,           NULL, 'd' },
#endif
		{ "charset",	required_argument,	NULL, 'c' },
		{ NULL,	0, NULL, 0 }
	};
	int c;
	char long_info = 0;
	char short_info = 0;
	char numeric = 0;
	char full_table = 0;
	char funcs_only = 0;
	char keys_only = 0;
#ifdef KDGKBDIACR
	char diac_only = 0;
#endif
	char charset = 1;

	while ((c = getopt_long(argc, argv,
		short_opts, long_opts, NULL)) != -1) {
		switch (c) {
			case 'i':
				short_info = 1;
				break;
			case 's':
			case 'l':
				long_info = 1;
				break;
			case 'n':
				numeric = 1;
				break;
			case 'f':
				full_table = 1;
				break;
			case 't':
				funcs_only = 1;
				break;
			case 'k':
				keys_only = 1;
				break;
#ifdef KDGKBDIACR
			case 'd':
				diac_only = 1;
				break;
#endif
			case 'c':
				if (strlen(optarg) != 10 || /* iso-8859-x */
					strncmp(optarg, "iso-8859-", 9) ||
					!index("1234578", optarg[9]))
					usage();
				charset = optarg[9] - '0';
				printf("charset \"%s\"\n", optarg);
				set_charset(optarg);
				break;
			case 'h':
			case '?':
				usage();
		}
	}

	if (optind < argc)
		usage();

	fd = getfd();

	if (short_info || long_info) {
		show_short_info();
		if (long_info)
			dump_symbols();
		exit(0);
	}

#ifdef KDGKBDIACR
	if (!diac_only) {
#endif
	    if (!funcs_only)
		dump_keys(full_table, numeric);
	    if (!keys_only)
		dump_funcs();
#ifdef KDGKBDIACR
	}
	if (!funcs_only && !keys_only)
	        dump_diacs();
#endif

	exit(0);
}
