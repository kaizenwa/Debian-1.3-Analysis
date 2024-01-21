% This file defines syntax highlighting for perl.
% The original author is Lars Marowsky-Bree <lmb@pointer.in-minden.de>

% Create and initialize the syntax tables.

$1 = "perl";

create_syntax_table ($1);
define_syntax ("#", "#", '%', $1);
define_syntax ("([{<", ")]}>", '(', $1);
define_syntax ('"', '"', $1);
define_syntax ('\'', '\'', $1);
define_syntax ('\\', '\\', $1);
define_syntax ("$0-9a-zA-Z_", 'w', $1);        % words
define_syntax ("-+0-9a-fA-F.xXL", '0', $1);   % Numbers
define_syntax (",;.?:", ',', $1);
define_syntax ("%-+/&*=<>|!~^", '+', $1);
set_syntax_flags ($1, 4);

#ifdef HAS_DFA_SYNTAX
enable_highlight_cache("perl.dfa", $1);
define_highlight_rule("#.*$", "comment", $1);
define_highlight_rule("([\\$%&@\\*]|\\$#)[A-Za-z_0-9]+", "normal", $1);
define_highlight_rule(strcat("\\$([_\\./,\"\\\\#\\*\\?\\]\\[;!@:\\$<>\\(\\)",
			     "%=\\-~\\^\\|&`'\\+]|\\^[A-Z])"), "normal", $1);
define_highlight_rule("[A-Za-z_][A-Za-z_0-9]*", "Knormal", $1);
define_highlight_rule("[0-9]+(\\.[0-9]+)?([Ee][\\+\\-]?[0-9]*)?", "number",
		      $1);
define_highlight_rule("0[xX][0-9A-Fa-f]*", "number", $1);
define_highlight_rule("[\\(\\[\\{\\<\\>\\}\\]\\),;\\.\\?:]", "delimiter", $1);
define_highlight_rule("[%\\-\\+/&\\*=<>\\|!~\\^]", "operator", $1);
define_highlight_rule("-[A-Za-z]", "keyword0", $1);
define_highlight_rule("'[^']*'", "string", $1);
define_highlight_rule("'[^']*$", "string", $1);
define_highlight_rule("\"([^\"\\\\]|\\\\.)*\"", "string", $1);
define_highlight_rule("\"([^\"\\\\]|\\\\.)*\\\\?$", "string", $1);
define_highlight_rule("m?/([^/\\\\]|\\\\.)*/[gio]*", "string", $1);
define_highlight_rule("m/([^/\\\\]|\\\\.)*\\\\?$", "string", $1);
define_highlight_rule("s/([^/\\\\]|\\\\.)*(/([^/\\\\]|\\\\.)*)?/[geio]*",
		      "string", $1);
define_highlight_rule("s/([^/\\\\]|\\\\.)*(/([^/\\\\]|\\\\.)*)?\\\\?$",
		      "string", $1);
define_highlight_rule("(tr|y)/([^/\\\\]|\\\\.)*(/([^/\\\\]|\\\\.)*)?/[cds]*",
		      "string", $1);
define_highlight_rule("(tr|y)/([^/\\\\]|\\\\.)*(/([^/\\\\]|\\\\.)*)?\\\\?$",
		      "string", $1);
define_highlight_rule(".", "normal", $1);
build_highlight_table ($1);
#endif

() = define_keywords ($1, 
		      strncat ("endhostentendserventgethostentgetservent",
			       "getsockoptsethostentsetserventsetsockopt",
			       "socketpair",
			       3),
		      10);
() = define_keywords ($1, 
		      strncat ("endprotoentgetpeernamegetprioritygetprotoent",
			       "getsocknamesetprioritysetprotoent",
			       2),
		      11);
() = define_keywords ($1, "getnetbyaddrgetnetbyname", 12);
() = define_keywords ($1, 
		      strncat ("gethostbyaddrgethostbynamegetservbyname",
			       "getservbyport",
			       2),
		      13);
() = define_keywords ($1, "getprotobyname", 14);
() = define_keywords ($1, "getprotobynumber", 16);
() = define_keywords ($1, "-Xdoiflcm/mynoq/s/ucy/", 2);
() = define_keywords ($1, 
		      strncat ("abschrcosdieeofexpforhexintlogmapoctordpoppos",
			       "qq/qw/qx/refsintietr/usevec",
			       2),
		      3);
() = define_keywords ($1, 
		      strncat ("bindchopdumpeachevalexecexitforkgetcglobgoto",
			       "grepjoinkeyskilllastlinknextopenpackpipepush",
			       "randreadrecvredoseeksendsortsqrtstattelltime",
			       "waitwarn",
			       4),
		      4);
() = define_keywords ($1, 
		      strncat ("alarmatan2blesschdirchmodchompchownclosecrypt",
			       "elsiffcntlflockindexioctllocallstatmkdirprint",
			       "resetrmdirsemopshiftsleepsplitsrandstudytimes",
			       "umaskundefuntieuntilutimewhilewrite", 
			       4),
		      5);
() = define_keywords ($1, 
		      strncat ("acceptcallerchrootdeleteexistsfilenogmtime",
			       "importlengthlistenmsgctlmsggetmsgrcvmsgsnd",
			       "printfrenamereturnrindexscalarselectsemctl",
			       "semgetshmctlshmgetsocketsplicesubstrsystem",
			       "unlinkunpackvalues",
			       5),
		      6);
() = define_keywords ($1, 
		      strncat ("binmodeconnectdbmopendefinedforeachgetpgrpgetppid",
			       "lcfirstopendirreaddirrequirereverseseekdir",
			       "setpgrpshmreadsprintfsymlinksyscallsysread",
			       "telldirucfirstunshiftwaitpid",
			       4),
		      7);
() = define_keywords ($1, 
		      strncat ("closedirdbmcloseendgrentendpwentformline",
			       "getgrentgetgrgidgetgrnamgetlogingetpwent",
			       "getpwnamgetpwuidreadlinksetgrentsetpwent",
			       "shmwriteshutdownsyswritetruncate",
			       4),
		      8);
() = define_keywords ($1, 
		      strncat ("endnetentgetnetentlocaltimequotemeta",
			       "rewinddirsetnetentwantarray",
			       2),
		      9);

define perl_mode ()
{
   variable kmap = "perl";
   set_mode (kmap, 4);
   use_syntax_table (kmap);
   runhooks("perl_mode_hook");
}
