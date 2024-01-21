#include <unistd.h>
#if !defined(titan)
#include <stdlib.h>
#endif
#include <stdio.h>
#include <string.h>
#include "a2gs.h"

struct SizeType {
	int Width, Length;
};



void GetMediaType(char *media, struct SizeType *Paper){
	int i;
	static struct MediaType Media[] = MEDIAS;
	static char DefaultMedia[]=DEFAULT_MEDIA;

	if (media == NULL || !media[0] ) media=DefaultMedia;
	loop
		for (i=0;Media[i].Name != '\0';i++)
		  if (!strcasecmp(Media[i].Name,media)){
			Paper->Width=Media[i].Width;
			Paper->Length=Media[i].Length;
			break;
		};
		media=DefaultMedia;
        until ( Paper->Width < 10 );
};




/*
 *  Process errors
 */ 
void ProcessError
(unsigned int Error, unsigned int Level, char *StringInput) {
	static struct ErrorType ErrorMessage[]=ERROR_MESSAGES;
	static struct ErrorType ErrorLevel[]=ERROR_LEVELS;
	int i;
        
        if (Level == DISABLED) return;

	for (i=0;ErrorLevel[i].Number != 0;i++)
	  if (ErrorLevel[i].Number == Level){
		fprintf(stderr, "%s: ",ErrorLevel[i].String);
		break;
	};
	for (i=0;ErrorMessage[i].Number != 0;i++)
	  if (ErrorMessage[i].Number == Error){
		fprintf(stderr, "%s",ErrorMessage[i].String);
		break;
	};
	fprintf(stderr, "%s\n", StringInput);
	if ( Level == FATAL ) exit(1);
};



void main (int argc, char *argv []){
	static struct OptionType Option[]=OPTIONS;
	struct MarginType Margin=MARGIN;
	struct SizeType Paper={0,0};
	char Options[31], *File, *Header, *Media, Rotate[50], Filter[200];
	int i,j, 
	  LinesPerPage=0,CharPerLine=0,Columns=0,CharPerColumn=0;

	File=Media=NULL;
	Options[0]=Rotate[0]=Filter[0]='\0';
	Header=argv[0];
/*
 * Process input arguments
 */
	getallopt(&argc, argv, Options,sizeof(Options),Option);
	for (i=j=0;Options[i] != '\0';i++) switch ( Options[i] ){
		case HELP:
			printf("%s",USAGE);
			exit(1);
		case NINE_COLUMNS:
		case EIGHT_COLUMNS:
		case SEVEN_COLUMNS:
		case SIX_COLUMNS:
		case FIVE_COLUMNS:
		case FOUR_COLUMNS:
		case THREE_COLUMNS:
		case TWO_COLUMNS:
		case ONE_COLUMN:
			Columns=Options[i] - '0';
			break;
		default:
			if ( ++j < argc ) switch ( Options[i] ){
				case TOP_MARGIN:
					Margin.Top=ConvertUnits(argv[j]);
					break;
				case BOTTOM_MARGIN:
					Margin.Bottom=ConvertUnits(argv[j]);
					break;
				case LEFT_MARGIN:
					Margin.Left=ConvertUnits(argv[j]);
					break;
				case RIGHT_MARGIN:
					Margin.Right=ConvertUnits(argv[j]);
					break;
				case WIDTH:
					CharPerLine=atoi(argv[j]);
					break;
				case LINES_PER_PAGE:
					LinesPerPage=atoi(argv[j]);
					break;
				case MEDIA:
					Media=argv[j];
					break;
				case HEADER:
					Header=argv[j];
					break;
				case INPUT:
					File=argv[j];
					break;
				case MANY_COLUMNS:
					Columns=atoi(argv[j]);	
					break;
				default:
					j--;
					break;
			};
			/* Chopt options are handled below, so do nothing now */
			break;
	};
	if (++j > argc) {
		ProcessError( TOO_FEW, WARNING, "input arguments!"); \
	}else if (j < argc) File=argv[j];
/*
 * Open input
 */
	if (File != NULL && freopen(File, "r", stdin) == NULL)
	  ProcessError(CANT_OPEN,FATAL,File);

/*
 * Adjust paper dimensions
 */
	GetMediaType(Media,&Paper);
	if (Chopt(PORTRAIT,Options)){	/* Portrait Mode */
		if (LinesPerPage < 1) LinesPerPage=DEFAULT_LINES_PER_PAGE_PORTRAIT;
		if (CharPerLine < 1) CharPerLine=DEFAULT_CHAR_PER_LINE_PORTRAIT;
		if (Columns < 1) Columns=DEFAULT_COLUMNS_PORTRAIT;
	}else{					/* LandScape */
		sprintf(Rotate,ROTATE,-((float) Paper.Width)/INCH);
		if (LinesPerPage < 1) LinesPerPage=DEFAULT_LINES_PER_PAGE_LANDSCAPE;
		if (CharPerLine < 1) CharPerLine=DEFAULT_CHAR_PER_LINE_LANDSCAPE;
		if (Columns < 1) Columns=DEFAULT_COLUMNS_LANDSCAPE;
		iswap(Paper.Width,Paper.Length);
		iswap(Margin.Top,Margin.Right);
		iswap(Margin.Bottom,Margin.Left);
	};
/*
 * Calculate spacing parameters
 */
	if(CharPerLine < COLUMN_SEPARATION*(Columns + 1)) 
	  CharPerLine=COLUMN_SEPARATION * (Columns + 1);
	CharPerColumn=((COLUMN_SEPARATION + CharPerLine)/Columns) - COLUMN_SEPARATION;
/*
 *  Print Postscript Program
 */
	printf("%%!PS-%s of %s\n",argv[0],(File != NULL) ? File : "(standard input)");
	printf("/inch %d def /cm inch 2.54 div def /ratio %2.1f def\n\n", (int) INCH, RATIO);
	PrintDef("PaperWidth",	Paper.Width);
	PrintDef("PaperLength",	Paper.Length);
	PrintDef("TopMargin",	Margin.Top);
	PrintDef("BottomMargin",Margin.Bottom);
	PrintDef("LeftMargin",	Margin.Left);
	PrintDef("RightMargin",	Margin.Right);
	printf("/LinesPerPage %d def\n",LinesPerPage);
	printf("/CharPerLine %d ratio mul def\n",CharPerLine);
	printf("\n%s\n%s\n%s\n%s\n",
	  "/TopM PaperLength TopMargin sub def",
	  "/LeftM PaperWidth LeftMargin sub def",
	  "/LineSize TopM BottomMargin sub LinesPerPage div def",
	  "/FontSize LeftM RightMargin sub CharPerLine div def");
	printf("/%s findfont FontSize scalefont setfont\n",FONT);
	printf("%s %s %s\n%s %s\n%s\n%s\n%s\n%s\n%s\n",
 	  "/Incr {dup load 1 add def} def",
	  "/InFile (%stdin) (r) file def",
	  "/Buffer 5000 string def",
	  "/Line 1 def",
	  "/DoPage {gsave showpage grestore /Line 1 def} def",
	  "/NextLine {LeftMargin TopM LineSize Line mul sub moveto} def",
	  "/Get1 {InFile Buffer readline not exch} def",
	  "/Put1 {Line LinesPerPage gt {DoPage} if NextLine show /Line Incr} def",
	  "/Main {LeftMargin TopM moveto {Get1 Put1 {exit} if}loop",
	  "  Line 2 gt {DoPage} if} def");
	printf("%s\n%s\n",Rotate,"Main");
	fflush(stdout);

/*
 *  Shell to pr for reformatting of the text
 */
	sprintf(Filter,
	  "%s %d|pr -h %1.80s -l%d -w%d %d %s %s %s %s %s %s|expand|sed 's/^/	/'",
#if defined(titan)
	  (Chopt(TRUNCATE,Options)) ? "colrm" : "fold",
	  (Chopt(TRUNCATE,Options)) ? CharPerColumn : -CharPerColumn,
#else
	  (Chopt(TRUNCATE,Options)) ? "colrm" : "fold -s -w ", CharPerColumn,
#endif
	  Header, LinesPerPage, CharPerLine, -Columns,
	  (Chopt(NO_HEADER,Options)) ? "-t" : "",
	  (Chopt(ACROSS,Options)) ? "-a" : "",
	  (Chopt(BALANCE,Options)) ? "-b" : "",
	  (Chopt(SHOW,Options)) ? "-v" : "",
	  (Chopt(DOUBLE_SPACE,Options)) ? "-d" : "",
	  (Chopt(NUMBER,Options)) ? "-n" : "");
	execl(SHELL,SHELL,"-c",Filter,(char *) '\0');

	ProcessError(EXEC_FAILED,FATAL,Filter);
};
