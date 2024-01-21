% S-Lang mode is just a superset of C mode so make sure it is loaded.
!if (keymap_p ("C")) () = evalfile ("cmode");

$1 = "SLANG";

create_syntax_table ($1);
define_syntax ("%", "", '%', $1);
define_syntax ("([{", ")]}", '(', $1);
define_syntax ('"', '"', $1);
define_syntax ('\'', '\'', $1);
define_syntax ('\\', '\\', $1);
define_syntax ("0-9a-zA-Z_", 'w', $1);        % words
define_syntax ("-+0-9a-fA-F.xX", '0', $1);   % Numbers
define_syntax (",;:", ',', $1);
define_syntax ('#', '#', $1);
define_syntax ("%-+/&*=<>|!~^", '+', $1);

#ifdef HAS_DFA_SYNTAX
enable_highlight_cache ("slmode.dfa", $1);
define_highlight_rule("^[ \t]*#", "PQpreprocess", $1);
define_highlight_rule("%.*$", "comment", $1);
define_highlight_rule("[A-Za-z_\\$][A-Za-z_0-9\\$]*", "Knormal", $1);
define_highlight_rule("[0-9]+(\\.[0-9]*)?([Ee][\\+\\-]?[0-9]*)?",
                      "number", $1);
define_highlight_rule("0[xX][0-9A-Fa-f]*", "number", $1);
define_highlight_rule("\"([^\"\\\\]|\\\\.)*\"", "string", $1);
define_highlight_rule("\"([^\"\\\\]|\\\\.)*\\\\?$", "string", $1);
define_highlight_rule("'([^'\\\\]|\\\\.)*'", "string", $1);
define_highlight_rule("'([^'\\\\]|\\\\.)*\\\\?$", "string", $1);
define_highlight_rule("[ \t]+", "normal", $1);
define_highlight_rule("[\\(\\[{}\\]\\),;\\.\\?:]", "delimiter", $1);
define_highlight_rule("[%\\-\\+/&\\*=<>\\|!~\\^]", "operator", $1);
define_highlight_rule("!if", "keyword0", $1);
build_highlight_table($1);
#endif

() = define_keywords ($1, "doifor", 2);
() = define_keywords ($1, "andfor", 3);
() = define_keywords ($1, "_forcaseelseloop", 4);
() = define_keywords ($1, "breakwhile", 5);
() = define_keywords ($1, "defineorelsereturnswitch", 6);
() = define_keywords ($1, "andelseforever", 7);
() = define_keywords ($1, "continuevariable", 8);
() = define_keywords ($1, "EXIT_BLOCK", 10);
() = define_keywords ($1, "ERROR_BLOCK", 11);
() = define_keywords ($1, "EXECUTE_ERROR_BLOCK", 19);

define slmode_insert_space ()
{
   variable cstr;
   EXIT_BLOCK
     {
	insert_single_space ();
     }
   
   !if (is_slang_mode ()) return;
   if (not (eolp ()) or (what_column () <= WRAP)) return;
   
   % we are at the end of line.
   cstr = "%!% ";
   bol ();
   !if (looking_at (cstr), eol ()) return;
   !if (bfind_char (' ')) return;
   trim ();
   newline ();
   insert (cstr);
   eol ();
}
   
   
define slang_mode ()
{
   c_mode ();
   set_mode("SLang", 2 | 8);
   use_syntax_table ("SLANG");
   local_setkey ("slmode_insert_space", " ");
   runhooks("slang_mode_hook");
}

