#include "getallopt.h"

/*
 * Define defaults
 */

struct MarginType {
	int Top, Bottom, Left, Right;
};

#define MARGIN		{72,72,36,36}
#define DEFAULT_MEDIA	"letter"

#define DEFAULT_LINES_PER_PAGE_PORTRAIT		96
#define DEFAULT_CHAR_PER_LINE_PORTRAIT		132
#define DEFAULT_COLUMNS_PORTRAIT		1

#define DEFAULT_LINES_PER_PAGE_LANDSCAPE	66
#define DEFAULT_CHAR_PER_LINE_LANDSCAPE		152
#define DEFAULT_COLUMNS_LANDSCAPE		2


/*
 * Define Constants that probably won't ever need to be changed
 */
#define COLUMN_SEPARATION	8
#define RATIO	0.60
#define INCH	72.0
#define ROTATE	"90 rotate 0 inch mul %2.3f inch mul translate"
#define FONT	"Courier"
#define SHELL	"/bin/sh"

/*
 * Define error levels
 */
#define DISABLED	0x00
#define FATAL		0x01
#define WARNING		0x02

#define ERROR_LEVELS { \
	{FATAL,"FATAL ERROR"},\
	{WARNING,"WARNING"},\
	{0,""}}



/*
 * Define error types
 */
#define NO_SUCH_OPTION		0x01
#define TOO_MANY		0x02
#define TOO_FEW                 0x03
#define UNDEFINED_OPTION	0x04
#define CANT_OPEN		0x05
#define EXEC_FAILED		0x06


struct ErrorType {
	unsigned int Number;
	char String[50];
};

#define ERROR_MESSAGES { \
	{NO_SUCH_OPTION,"Unrecognized Option: "},\
	{TOO_MANY,"Too many "},\
	{TOO_FEW,"Too few "},\
	{UNDEFINED_OPTION,"Undefined Option: "},\
	{CANT_OPEN,"Error opening: "},\
	{EXEC_FAILED,"Failed to execute: "},\
	{0,""}}




/*
 * Define media types
 */

struct MediaType
{
	char Name[10];
	unsigned int Width,Length;
};

#define MEDIAS { \
 {"letter", 612, 792 },\
 {"tabliod", 792, 1224 },\
 {"ledger", 1224, 792 },\
 {"legal", 612, 792 },\
 {"statement", 396, 612 },\
 {"executive", 540, 720 },\
 {"a3",  842, 1190 },\
 {"a4",  595, 842 },\
 {"a5",  420, 595 },\
 {"b4",  729, 1032 },\
 {"b5",  516, 729 },\
 {"folio", 612, 936 },\
 {"quarto", 610, 780 },\
 {"10x14", 720, 1008 },\
 {"", 0, 0 }}





/*
 * Define input short input options.
 */
#define IGNORED		'-'
#define TOP_MARGIN 	'T'
#define BOTTOM_MARGIN 	'B'
#define LEFT_MARGIN 	'L'
#define RIGHT_MARGIN	'R'
#define WIDTH		'w'
#define LINES_PER_PAGE	'l'
#define MEDIA		'm'
#define PORTRAIT	'p'
#define HEADER		'H'
#define NO_HEADER	'N'
#define TRUNCATE	't'
#define ACROSS		'a'
#define BALANCE		'b'
#define SHOW		'v'
#define DOUBLE_SPACE    'd'
#define NUMBER		'n'
#define INPUT		'f'
#define HELP		'h'
#define ONE_COLUMN	'1'
#define TWO_COLUMNS	'2'
#define THREE_COLUMNS	'3'
#define FOUR_COLUMNS	'4'
#define FIVE_COLUMNS	'5'
#define SIX_COLUMNS	'6'
#define SEVEN_COLUMNS	'7'
#define EIGHT_COLUMNS	'8'
#define NINE_COLUMNS	'9'
#define MANY_COLUMNS	'0'


/*
 * Define the long names for input options.
 */

#define	OPTIONS {\
	{TOP_MARGIN,	"top-margin",	1},\
	{TOP_MARGIN,	"top",		1},\
	{BOTTOM_MARGIN,	"bottom-margin",1},\
	{BOTTOM_MARGIN,	"bottom",	1},\
	{LEFT_MARGIN,	"left-margin",	1},\
	{LEFT_MARGIN,	"left",		1},\
	{RIGHT_MARGIN,	"right-margin",	1},\
	{RIGHT_MARGIN,	"right",	1},\
	{WIDTH,		"width",	1},\
	{LINES_PER_PAGE,"lines-per-page",1},\
	{LINES_PER_PAGE,"lines",	1},\
	{LINES_PER_PAGE,"length",	1},\
	{MEDIA,		"printing-media",1},\
	{MEDIA,		"paper-size",	1},\
	{MEDIA,		"media",	1},\
	{PORTRAIT,	"portrait",	0},\
	{IGNORED,	"landscape",	0},\
	{HEADER,	"header",	1},\
	{NO_HEADER,	"no-header",	0},\
	{NO_HEADER,	"noheader",	0},\
	{TRUNCATE,	"truncate",	0},\
	{TRUNCATE,	"no-wrap",	0},\
	{TRUNCATE,	"nowrap",	0},\
	{ACROSS,	"across",	0},\
	{BALANCE,	"balance",	0},\
	{SHOW,		"show",		0},\
	{DOUBLE_SPACE,	"double-space",	0},\
	{DOUBLE_SPACE,	"double",	0},\
	{NUMBER,	"number",	0},\
	{IGNORED,	"wrap-lines",	0},\
	{IGNORED,	"wrap",		0},\
	{IGNORED,	"stdin",	0},\
	{INPUT,		"input",	1},\
	{INPUT,		"file",		1},\
	{HELP,		"help",		0},\
	{ONE_COLUMN,	"one-column",	0},\
	{TWO_COLUMNS,	"two-columns",	0},\
	{THREE_COLUMNS,	"three-columns",0},\
	{FOUR_COLUMNS,	"four-columns",	0},\
	{FIVE_COLUMNS,	"five-columns",	0},\
	{SIX_COLUMNS,	"six-columns",	0},\
	{SEVEN_COLUMNS,	"seven-columns",0},\
	{EIGHT_COLUMNS,	"eight-columns",0},\
	{NINE_COLUMNS,	"nine-columns",	0},\
	{MANY_COLUMNS,	"many-columns",	1},\
	{'\0',		"",		-1}}




/*
 * Define macros to make the code more readable by humans
 */
#define loop			do{
#define until(condition)	} while ( (condition) )
#define TRUE  1
#define FALSE 0





/*
 * Define some handy macro functions
 */
#define iswap(i,j)	{int k;k=(i);(i)=(j);(j)=k;}

#define Chopt(c,s) (index((s),(c)) != NULL)

#define PrintDef(s,i) printf("/%s %2.3f inch mul def\n",(s),((float)(i))/INCH)

#define EnglishOrMetric(string) \
	( (strstr((string),"cm") == NULL ) ? INCH/2.54 : INCH )

#define ConvertUnits(string) \
	(Chopt('p',(string))) ? \
	atoi((string)) : (int)(atof((string)) * EnglishOrMetric(string))

#define USAGE "\
	a2gs - formats an ascii file for processing with\n\
	ghostscript.\n\
	a2gs [ -TBLRwlpNtabsdnfmHh1234567890 ] [ --across --portrait\n\
	--width	# --length # --no-header --truncate --balance --show\n\
	--double-space --number	--file file --many-columns #\n\
	--(top,bottom,left,right)-margin # --media type --help \n\
	--(one,two,...)-column(s) ] file\n"

