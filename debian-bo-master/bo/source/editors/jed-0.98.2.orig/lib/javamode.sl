% This file is due to Marko Teittinen <marko@tekamah.com>.
% Java mode is just a superset of C mode so make sure it is loaded.
!if (keymap_p ("C")) () = evalfile ("cmode");

$1 = "java";

create_syntax_table ($1);
define_syntax ("/*", "*/", '%', $1);
define_syntax ("([{", ")]}", '(', $1);
define_syntax ('"', '"', $1);
define_syntax ('\'', '\'', $1);
define_syntax ('\\', '\\', $1);
define_syntax ("0-9a-zA-Z_", 'w', $1);        % words
define_syntax ("-+0-9a-fA-F.xXL", '0', $1);   % Numbers
define_syntax (",;.?:", ',', $1);
define_syntax ('#', '#', $1);
define_syntax ("%-+/&*=<>|!~^", '+', $1);
set_syntax_flags ($1, 4);

#ifdef HAS_DFA_SYNTAX
enable_highlight_cache("javamode.dfa", $1);
define_highlight_rule("^[ \t]*#", "PQpreprocess", $1);
define_highlight_rule("//.*", "comment", $1);
define_highlight_rule("/\\*.*\\*/", "Qcomment", $1);
define_highlight_rule("^([^/]|/[^\\*])*\\*/", "Qcomment", $1);
define_highlight_rule("/\\*.*", "comment", $1);
define_highlight_rule("^[ \t]*\\*+([ \t].*)?$", "comment", $1);
define_highlight_rule("[A-Za-z_\\$][A-Za-z_0-9\\$]*", "Knormal", $1);
define_highlight_rule("[0-9]+(\\.[0-9]*)?([Ee][\\+\\-]?[0-9]*)?",
                      "number", $1);
define_highlight_rule("0[xX][0-9A-Fa-f]*[LU]*", "number", $1);
define_highlight_rule("[0-9]+[LU]*", "number", $1);
define_highlight_rule("\"([^\"\\\\]|\\\\.)*\"", "string", $1);
define_highlight_rule("\"([^\"\\\\]|\\\\.)*\\\\?$", "string", $1);
define_highlight_rule("'([^'\\\\]|\\\\.)*'", "string", $1);
define_highlight_rule("'([^'\\\\]|\\\\.)*\\\\?$", "string", $1);
define_highlight_rule("[ \t]+", "normal", $1);
define_highlight_rule("[\\(\\[{}\\]\\),;\\.\\?:]", "delimiter", $1);
define_highlight_rule("[%\\-\\+/&\\*=<>\\|!~\\^]", "operator", $1);
build_highlight_table($1);
#endif

() = define_keywords_n ($1, "doif", 2, 0);
() = define_keywords_n ($1, "forintnewtry", 3, 0);
() = define_keywords_n ($1, "bytecasecharelselongthisvoid", 4, 0);
() = define_keywords_n ($1, "breakcatchclassfinalfloatshortsuperthrowwhile", 5, 0);
() = define_keywords_n ($1, "doubleimportnativepublicreturnstaticswitchthrows", 6, 0);
() = define_keywords_n ($1, "booleandefaultextendsfinallypackageprivatevirtual", 7, 0);
() = define_keywords_n ($1, "abstractcontinuevolatile", 8, 0);
() = define_keywords_n ($1, "interfaceprotected", 9, 0);
() = define_keywords_n ($1, "implementsinstanceof", 10, 0);
() = define_keywords_n ($1, "synchronized", 12, 0);

() = define_keywords_n ($1, "var", 3, 1);
() = define_keywords_n ($1, "castgotonullresttrue", 4, 1);
() = define_keywords_n ($1, "constfalseinnerouter", 5, 1);
() = define_keywords_n ($1, "future", 6, 1);
() = define_keywords_n ($1, "byvaluegeneric", 7, 1);
() = define_keywords_n ($1, "finalizeoperator", 8, 1);
() = define_keywords_n ($1, "transient", 9, 1);

define java_mode ()
{
   variable java = "java";
   c_mode ();
   set_mode (java, 2);
   use_syntax_table (java);
   runhooks("java_mode_hook");
}
