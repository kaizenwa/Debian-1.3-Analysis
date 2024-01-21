/*
 * ascii.c -- quick crossreference for ASCII character aliases
 *
 * by Eric S. Raymond <esr@snark.thyrsus.com>, v1.0 March 1990
 *	v1.1 November 1990 -- revised `slang' from the 2.2 pronunciation guide
 *	v1.2 March 1995    -- Fixed a core-dump bug
 *	v1.3 October 1995  -- Fixed a bug that shows up only under ELF.
 *	v2.0 December 1995 -- Interpret ISO-style code table escapes.
 *
 * Tries to interpret arguments as names or aliases of ascii characters
 * and dumps out *all* the aliases of each. Accepts literal characters,
 * standard mnemonics, C-style backslash escapes, caret notation for control
 * characters, numbers in hex/decimal/octal/binary, English names.
 *
 * The slang names used are selected from the 2.2 version of the USENET ascii
 * pronunciation guide.  Some additional ones were merged in from the Jargon
 * File.
 *
 * Reproduce, use, and modify this as you like, as long as you don't
 * remove the authorship notice and clearly mark your changes. Send me
 * your improvements, please.
 */
#include <stdio.h>
#include <ctype.h>

typedef char	*string;

static string cnames[128][15] =
{
    {"NUL",		"Null", 		(char *)NULL},
    {"SOH",		"Start Of Heading", 	(char *)NULL},
    {"STX",		"Start Text",		(char *)NULL},
    {"ETX",		"End Text",		(char *)NULL},
    {"EOT",		"End Of Transmission",	(char *)NULL},
    {"ENQ",		"Enquire",		(char *)NULL},
    {"ACK",		"Acknowledge",		(char *)NULL},
    {"BEL",		"Bell", "\\a", "Alert",	(char *)NULL},
    {"BS", 		"Backspace", "\\b",	(char *)NULL},
    {"HT",  "TAB",	"Horizontal Tab", "\\t",	(char *)NULL},
    {"LF",  "NL",	"Line Feed", "\\n", "Newline",	(char *)NULL},
    {"VT",		"Vertical Tab", "\\v",		(char *)NULL},
    {"FF",		"Form Feed", "\\f",		(char *)NULL},
    {"CR",		"Carriage Return", "\\r",	(char *)NULL},
    {"SO", "LS1",	"Shift Out", "Locking Shift 1",	(char *)NULL},
    {"SI", "LS0",	"Shift In", "Locking Shift 0",	(char *)NULL},
    {"DLE",		"Data Link Escape",	(char *)NULL},
    {"DC1",		"Direct Control 1",	(char *)NULL},
    {"DC2",		"Direct Control 2",	(char *)NULL},
    {"DC3",		"Direct Control 3",	(char *)NULL},
    {"DC4",		"Direct Control 4",	(char *)NULL},
    {"NAK",		"Negative Acknowledge",	(char *)NULL},
    {"SYN",		"Synchronous Idle",	(char *)NULL},
    {"ETB",		"End Transmission Block",	(char *)NULL},
    {"CAN",		"Cancel",		(char *)NULL},
    {"EM",		"End of Medium",	(char *)NULL},
    {"SUB",		"Substitute", 		(char *)NULL},
    {"ESC",		"Escape",		(char *)NULL},
    {"FS", 		"Form Separator",	(char *)NULL},
    {"GS", 		"Group Separator",	(char *)NULL},
    {"RS", 		"Record Separator",	(char *)NULL},
    {"US", 		"Group Separator",	(char *)NULL},
    {" ", "SP",		"Space", "Blank",	(char *)NULL},
    {"!",		"Exclamation Point",
	 			"Bang", "Excl", "Wow", "Factorial", "Shriek",
	 			"Pling", "Smash", "Cuss", (char *)NULL},
    {"\"",		"Double Quote",
	 			"Quote", "String Quote", "Dirk",
				"Literal Mark", "Double Glitch",
				"# See ' and ` for matching names.",
				(char *)NULL},
    {"#",		"Pound",
	 			"Number", "Sharp", "Crunch", "Mesh",
	 			"Hex", "Hash", "Flash", "Grid", "Octothorpe",
				(char *)NULL},
    {"$",		"Dollar",
				"Buck", "Cash", "Ding", (char *)NULL},
    {"%",		"Percent",
	 			"Mod", (char *)NULL},
    {"&",		"Ampersand",
				"Amper", "And", (char *)NULL},
    {"'",		"Apostrophe",
				"Single Quote", "Close Quote",  "Prime",
				"Tick", "Pop", "Spark", "Prime", "Glitch",
				"# See ` and \" for matching names.",
				(char *)NULL},
    {"(",		"Open Parenthesis",
				"Open", "Open Paren",
				"Left Paren", "Wane", "Sad",
				"# See `)' for matching names.",
	 			(char *)NULL},
    {")",		"Close Parenthesis",
				"Close", "Close Paren",
				"Right Paren", "Wax", "Happy",
				"# See `(' for matching names.",
	 			 (char *)NULL},
    {"*",		"Asterisk",
				"Star", "Splat", "Aster", "Times", "Gear",
				"Dingle", "Bug", "Twinkle", "Glob",  
				(char *)NULL},
    {"+",		"Plus",
				"Add", "Cross", (char *)NULL},
    {",",		"Comma",
				"Tail", (char *)NULL},
    {"-",		"Hyphen",
				"Dash", "Minus", "Worm", (char *)NULL}, 
    {".",		"Period",
				"Dot", "Decimal Point", "Radix Point",
				"Point", "Full Stop", "Spot", (char *)NULL},
    {"/",		"Virgule",
				"Slash", "Stroke", "Slant", "Diagonal",
				"Solidus", "Over", "Slat",
				"# See `\\' for matching names.",
				(char *)NULL},
    {"0",		"Numeral Zero", (char *)NULL},
    {"1",		"Numeral One", (char *)NULL},
    {"2",		"Numeral Two", (char *)NULL},
    {"3",		"Numeral Three", (char *)NULL},
    {"4",		"Numeral Four", (char *)NULL},
    {"5",		"Numeral Five", (char *)NULL},
    {"6",		"Numeral Six", (char *)NULL},
    {"7",		"Numeral Seven", (char *)NULL},
    {"8",		"Numeral Eight", (char *)NULL},
    {"9",		"Numeral Nine", (char *)NULL},
    {":",		"Colon",
				"Double-Dot", (char *)NULL},
    {";",		"Semicolon",
				"Semi", "Go-on", (char *)NULL},
    {"<",		"Less Than",
				"Left Angle Bracket", "Read From", "In",
				"From", "Comesfrom", "Left Funnel",
				"Left Broket", "Crunch", "Suck",
				"# See `<' for matching names.",
	 			 (char *)NULL},
    {"=",		"Equals",
				"Quadrathorp", "Gets", "Becomes", "Half-Mesh",
				(char *)NULL},
    {">",		"Greater Than",
				"Right Angle" , "Write To", "Into", "Toward",
				"Out", "To", "Gozinta", "Right Funnel",
				"Right Broket", "Zap", "Blow",
				"# See `>' for matching names.",
	 			 (char *)NULL},
    {"?",		"Question Mark",
				"Whatmark", "What", "Ques", (char *)NULL},
    {"@",		"At Sign",
				"At", "Each", "Vortex", "Whorl", "Whirlpool",
				"Cyclone", "Snail", "Rose", (char *)NULL},
    {"A",		"Majuscule A", "Capital A", "Uppercase A",(char*)NULL},
    {"B",		"Majuscule B", "Capital B", "Uppercase B",(char*)NULL},
    {"C",		"Majuscule C", "Capital C", "Uppercase C",(char*)NULL},
    {"D",		"Majuscule D", "Capital D", "Uppercase D",(char*)NULL},
    {"E",		"Majuscule E", "Capital E", "Uppercase E",(char*)NULL},
    {"F",		"Majuscule F", "Capital F", "Uppercase F",(char*)NULL},
    {"G",		"Majuscule G", "Capital G", "Uppercase G",(char*)NULL},
    {"H",		"Majuscule H", "Capital H", "Uppercase H",(char*)NULL},
    {"I",		"Majuscule I", "Capital I", "Uppercase I",(char*)NULL},
    {"J",		"Majuscule J", "Capital J", "Uppercase J",(char*)NULL},
    {"K",		"Majuscule K", "Capital K", "Uppercase K",(char*)NULL},
    {"L",		"Majuscule L", "Capital L", "Uppercase L",(char*)NULL},
    {"M",		"Majuscule M", "Capital M", "Uppercase M",(char*)NULL},
    {"N",		"Majuscule N", "Capital N", "Uppercase N",(char*)NULL},
    {"O",		"Majuscule O", "Capital O", "Uppercase O",(char*)NULL},
    {"P",		"Majuscule P", "Capital P", "Uppercase P",(char*)NULL},
    {"Q",		"Majuscule Q", "Capital Q", "Uppercase Q",(char*)NULL},
    {"R",		"Majuscule R", "Capital R", "Uppercase R",(char*)NULL},
    {"S",		"Majuscule S", "Capital S", "Uppercase S",(char*)NULL},
    {"T",		"Majuscule T", "Capital T", "Uppercase T",(char*)NULL},
    {"U",		"Majuscule U", "Capital U", "Uppercase U",(char*)NULL},
    {"V",		"Majuscule V", "Capital V", "Uppercase V",(char*)NULL},
    {"W",		"Majuscule W", "Capital W", "Uppercase W",(char*)NULL},
    {"X",		"Majuscule X", "Capital X", "Uppercase X",(char*)NULL},
    {"Y",		"Majuscule Y", "Capital Y", "Uppercase Y",(char*)NULL},
    {"Z",		"Majuscule Z", "Capital Z", "Uppercase Z",(char*)NULL},
    {"[",		"Left Square Bracket",
				"Bracket", "Bra", "Square",
				"# See `]' for matching names.",
	 			 (char *)NULL},
    {"\\",		"Reversed Virgule",
				"Backslash", "Bash", "Backslant", "Backwhack",
				"Backslat", "Literal", "Escape",
				"# See `/' for matching names.",
	 			 (char *)NULL},
    {"]",		"Right Square Bracket",
				"Unbracket", "Ket", "Unsquare",
				"# See `]' for matching names.",
	 			 (char *)NULL},
    {"^",		"Circumflex",
				"Caret", "Uparrow", "Hat", "Control", "Boink",
				"Chevron", "Hiccup",
				"Sharkfin", "Fang", (char*)NULL},
    {"_",		"Underscore",
				"Underline", "Underbar", "Under", "Score",
				"Backarrow", "Flatworm", 
	"# `Backarrow' refers to this character's graphic in 1963 ASCII.",
				(char*)NULL},
    {"`",		"Grave Accent",
				"Grave", "Backquote", "Left Quote",
				"Open Quote", "Backprime", "Unapostrophe",
				"Backspark", "Birk", "Blugle", "Back Tick",
				"Push",
				"# See ' and \" for matching names.",
				(char *)NULL},
    {"a",		"Miniscule a", "Small a", "Lowercase a",(char*)NULL},
    {"b",		"Miniscule b", "Small b", "Lowercase b",(char*)NULL},
    {"c",		"Miniscule c", "Small c", "Lowercase c",(char*)NULL},
    {"d",		"Miniscule d", "Small d", "Lowercase d",(char*)NULL},
    {"e",		"Miniscule e", "Small e", "Lowercase e",(char*)NULL},
    {"f",		"Miniscule f", "Small f", "Lowercase f",(char*)NULL},
    {"g",		"Miniscule g", "Small g", "Lowercase g",(char*)NULL},
    {"h",		"Miniscule h", "Small h", "Lowercase h",(char*)NULL},
    {"i",		"Miniscule i", "Small i", "Lowercase i",(char*)NULL},
    {"j",		"Miniscule j", "Small j", "Lowercase j",(char*)NULL},
    {"k",		"Miniscule k", "Small k", "Lowercase k",(char*)NULL},
    {"l",		"Miniscule l", "Small l", "Lowercase l",(char*)NULL},
    {"m",		"Miniscule m", "Small m", "Lowercase m",(char*)NULL},
    {"n",		"Miniscule n", "Small n", "Lowercase n",(char*)NULL},
    {"o",		"Miniscule o", "Small o", "Lowercase o",(char*)NULL},
    {"p",		"Miniscule p", "Small p", "Lowercase p",(char*)NULL},
    {"q",		"Miniscule q", "Small q", "Lowercase q",(char*)NULL},
    {"r",		"Miniscule r", "Small r", "Lowercase r",(char*)NULL},
    {"s",		"Miniscule s", "Small s", "Lowercase s",(char*)NULL},
    {"t",		"Miniscule t", "Small t", "Lowercase t",(char*)NULL},
    {"u",		"Miniscule u", "Small u", "Lowercase u",(char*)NULL},
    {"v",		"Miniscule v", "Small v", "Lowercase v",(char*)NULL},
    {"w",		"Miniscule w", "Small w", "Lowercase w",(char*)NULL},
    {"x",		"Miniscule x", "Small x", "Lowercase x",(char*)NULL},
    {"y",		"Miniscule y", "Small y", "Lowercase y",(char*)NULL},
    {"z",		"Miniscule z", "Small z", "Lowercase z",(char*)NULL},
    {"{",		"Open Brace",
				"Left Brace", "Brace", "Left Curly Bracket",
				"Curly", "Leftit", "Embrace",
				"# See `}' for matching names.",
	 			 (char *)NULL},
    {"|",		"Vertical Bar",
				"Pipe", "Bar", "Or", "V-Bar", "Spike",
				"Gozinta", "Thru", (char *)NULL},
    {"}",		"Close Brace",
				"Right Brace", "Unbrace","Right Curly Bracket",
				"Uncurly", "Rytit", "Bracelet",
				"# See `{' for matching names.",
	 			 (char *)NULL},
    {"~",		"Tilde",
				"Swung Dash", "Squiggle", "Approx", "Wiggle",
				"Twiddle", "Enyay", (char *)NULL},
    {"DEL",		"Delete", (char *)NULL},
};

static int atob(str)
/* ASCII-to-binary conversion */
char	*str;
{
    int	val;

    for (val = 0; *str; str++)
    {
	val *= 2;

	if (*str == '1')
	    val++;
        else if (*str != '0')
	    return(-1);
    }
    return(val);
}

static char *btoa(val)
/* binary-to-ASCII conversion */
{
#define BITSPERCHAR	8
    char	*rp;
    int		i;
    static char	rep[BITSPERCHAR + 1];

    /* write out external representation at least one char */
    *(rp = rep + BITSPERCHAR) = '\0';
    do {
       *--rp = (val & 1) + '0'; /* Is '1' == '0' + 1 in EBCDIC? */
       val /= 2;
    } while
	(val && rp > rep);

#ifndef SHORT_BINARY_REPRESENTATION
    while (rp > rep)
       *--rp = '0';
#endif
 
    return(rp);
}

static void speak(ch)
/* list all the names for a given character */
int	ch;
{
    char	**ptr = &cnames[ch][0];

    (void) printf(
	"ASCII %d/%d is decimal %03d, hex %02x, octal %03o, bits %s: ",
	ch / 16, ch % 16, ch, ch, ch, btoa(ch));

    /* display high-half characters */
    if (ch & 0x80)
    {
	ch &=~ 0x80;
	if (ch == 0x7f)
	    (void) printf("meta-^?\n");
	else if (isprint(ch))
	    (void) printf("meta-%c\n", ch);
	else
	    (void) printf("meta-^%c\n", '@' + (ch & 0x1f));
	return;
    }

    if (isprint(ch))
	(void) printf("prints as `%s'\n", *ptr++);
    else if (iscntrl(ch) || ch == 0x7F)
    {
	if (ch == 0x7f)
	    (void) printf("called ^?");
	else
	    (void) printf("called ^%c", '@' + (ch & 0x1f));
	for (; strlen(*ptr) < 4 && isupper(**ptr); ptr++)
	    (void) printf(", %s", *ptr);
	(void) putchar('\n');
    }

    (void) printf("Official name: %s\n", *ptr++);

    if (*ptr)
    {
	char	*commentary = (char *)NULL;

	if (**ptr == '\\')
	{
	    (void) printf("C escape: '%s'\n", *ptr);
	    ptr++;
	}

	(void) printf("Other names: ");
	for (; *ptr; ptr++)
	    if (**ptr == '#')
		commentary = *ptr;
	    else
		(void) printf("%s%s ", *ptr, ptr[1] ? "," : "");
	(void) putchar('\n');
	if (commentary)
	    (void) printf("Note: %s\n", commentary+2);
    }

    (void) putchar('\n');
}

static int stricmp(s, t)
/* case-blind string compare */
register char	*s, *t;
{
    while (*s && tolower(*s) == tolower(*t))
	s++, t++;
    return(*t - *s);
}

static void ascii(str)
char *str;
{
    int	ch, hi, lo;
    char **ptr;
    int len = strlen(str);

    /* interpret single characters as themselves */
    if (len == 1)
	speak(str[0]);

    /* interpret ^-escapes as control-character notation */
    if (strcmp(str, "^?") == 0)
	speak(0x7F);
    else if (str[0] == '^' && len == 2)
	speak(str[1] & 0x1f);

    /* interpret C-style backslash escapes */
    if (*str == '\\' &&  len == 2 && strchr("abfnrtv", str[1]))
	for (ch = 7; ch < 13; ch++)
	    for (ptr = &cnames[ch][1]; *ptr; ptr++)
		if (**ptr == '\\' && strcmp(str, *ptr) == 0)
		    speak(ch);

    /* interpret 2 and 3-character ASCII control mnemonics */
    if (len == 2 || len == 3)
    {
	/* first check for standard mnemonics */
	if (strcmp(str, "DEL") == 0)
	    speak(0x7f);
	if (strcmp(str, "BL") == 0)
	    speak(' ');
	else if (isalpha(str[0]))
	    for (ch = 0; ch <= 32; ch++)
		if (!stricmp(str,cnames[ch][0]) || !strcmp(str,cnames[ch][1]))
		    speak(ch);
    }

    /* OK, now try to interpret the string as a numeric */
    if (len > 1 && len < 9)
    {
	int hval, dval, oval, bval;

	dval = oval = hval = bval = -1;

	/* if it's all numeric it could be in one of three bases */
	if (len <= 2 && strspn(str,"0123456789ABCDEFabcdef") == len)
	    (void) sscanf(str, "%x", &hval);
	if (len <= 3 && strspn(str, "0123456789") == len)
	    (void) sscanf(str, "%d", &dval);
	if (len <= 3 && strspn(str, "01234567") == len)
	    (void) sscanf(str, "%o", &oval);
	if (len <= 9 && strspn(str, "01") == len)
	    bval = atob(str);

	/* accept 0xnn, \xnn and xnn forms for hex */
	if (hval == -1)
	    if ((str[0]=='0'||str[0]=='\\') && tolower(str[1]) == 'x')
		(void) sscanf(str + 2, "%x", &hval);
	    else if (tolower(str[0]) == 'x')
		(void) sscanf(str + 1, "%x", &hval);

	/* accept 0onn, \onnn and onnn forms for octal */
	if (oval == -1)
	    if ((str[0]=='0'||str[0]=='\\') && tolower(str[1]) == 'o')
		(void) sscanf(str + 2, "%o", &oval);
	    else if (tolower(str[0]) == 'o')
		(void) sscanf(str + 1, "%o", &oval);

	/* accept 0dnnn, \dnnn and dnnn forms for decimal */
	if (dval == -1)
	    if ((str[0]=='0'||str[0]=='\\') && tolower(str[1]) == 'd')
		(void) sscanf(str + 2, "%d", &dval);
	    else if (tolower(str[0]) == 'd')
		(void) sscanf(str + 1, "%d", &dval);

	/* accept 0bnnn, \bnnn and bnnn forms for binary */
	if (bval == -1)
	    if ((str[0]=='0'||str[0]=='\\') && tolower(str[1]) == 'b')
		bval = atob(str + 2);
	    else if (tolower(str[0]) == 'b')
		bval = atob(str + 1);

	/* OK, now output only unique values */
	if (hval > -1)
	    speak(hval & 0xff);
	if (dval > -1 && dval != hval)
	    speak(dval & 0xff);
	if (oval > -1 && oval != dval && oval != hval)
	    speak(oval & 0xff);
	if (bval > -1 && bval != oval && bval != dval && bval != hval)
	    speak(bval & 0xff);
    }

    if (sscanf(str, "%d/%d", &hi, &lo) == 2)    /* code table reference?  */
	speak(hi*16 + lo);
    else if (len >= 4 && isalpha(str[0]))	/* try to match long names */
    {
	char	canbuf[BUFSIZ], *ep;
	int	i;

	/* map dashes and other junk to spaces */
	for (i = 0; i <= len; i++)
	    if (str[i] == '-' || isspace(str[i]))
		canbuf[i] = ' ';
	    else
		canbuf[i] = str[i];

	/* strip `sign' or `Sign' suffix */
	ep = canbuf + strlen(canbuf) - 4;
	if (!strcmp(ep, "sign") || !strcmp(ep, "Sign"))
	    *ep = '\0';

	/* remove any trailing whitespace */
	while (canbuf[strlen(canbuf) - 1] == ' ')
	    canbuf[strlen(canbuf) - 1] = '\0';

	/* look through all long names for a match */
	for (ch = 0; ch < 128; ch++)
	    for (ptr = &cnames[ch][1]; *ptr; ptr++)
		if (!stricmp(*ptr, canbuf))
		    speak(ch);
    }
}

main(argc, argv)
int argc;
char **argv;
{
    if (argc == 1)
    {
	(void) fprintf(stderr, "usage: ascii char-alias...\n");
	(void) fprintf(stderr,
	"\tPrints all aliases of a given ASCII character.\n\n");
	(void) fprintf(stderr,
	"\tArgs may be single chars, C escape sequences, English names,\n");
	(void) fprintf(stderr,
	"\tcaret escapes for control characters, ASCII mnemonics,\n");
	(void) fprintf(stderr,
	"\tor numeric values in hex/octal/decimal/binary.\n");
    }
    else
	for(; argc > 0; argv++, argc--)
	    ascii(*argv);
}

/* ascii.c ends here */
