% TurboPascal Mode for the jed editor
% jed097b9 and later
%
% Version: minimal
% Update : 06 apr 1995
% Author : Carsten Tinggaard Nielsen, tinggard@iesd.auc.dk
%

variable tpas_objname = "*UnDef ObjectName*";
variable tpas_indent = 2;
variable tpas_tab_save = 0;

define tpas_is_comment() {
  return 0;
}

define tpas_getname(tellstring) {
   variable gname = read_mini(tellstring, Null_String, Null_String);
   return gname;
}

% --------------------------------------------------------------
% Utility routines
define ins_snlp(pos str) {
   % insert str, newline and indent to pos
   % note that str must be formatted with Sprintf
   insert(str);
   insert("\n");
   if (pos > 0)
     loop(pos) insert_single_space();
}

define tpas_pos() { return what_column() - 1;}

define tpas_paspf(p, name) {
   ins_snlp(p, "(* ");
   ins_snlp(p, " * ");
   ins_snlp(p, " *)");
   ins_snlp(p, "BEGIN");
   insert("END; (* ");
   insert(name);
   ins_snlp(p, " *)");
}

define tpas_delim_string() {
   return "--------------------------------------------------";
}

define tpas_prog_unit_start() {
   variable s = tpas_delim_string();
   vinsert("(* %s\n", s, 1);
   insert(" * Author : Carsten Tinggaard Nielsen\n");
   insert(" * Project: \n");
   insert(" * Module : \n * \n");
   insert(" * $Revision$ $Date$\n");
   insert(" * $Locker$ $Source$\n");
   vinsert(" * %s *)\n", s, 1);
   insert("(*$I-,V-,B+*)\n");
}

define tpas_prog_unit_end() {
   insert("\n  USES\n    DOS;\n\n");
   insert("BEGIN\n");
   insert("END.\n");
   insert("(* History:\n * --------\n * $Log$\n *)\n");
}

define tpas_main() {
   variable progname = tpas_getname("Name of program:");
   bob();
   vinsert ("PROGRAM %s;\n", progname, 1);
   tpas_prog_unit_start();
   insert("(*$M 16000, 128000, 512000 *)\n");
   tpas_prog_unit_end();
   bob();
}

define tpas_unit() {
   variable unitname = tpas_getname("Name of unit:");
   bob();
   vinsert ("UNIT %s;\n", unitname, 1);
   insert("INTERFACE\n\n  USES\n    DOS;\n\n");
   insert("IMPLEMENTATION\n");
   tpas_prog_unit_start();
   tpas_prog_unit_end();
   bob();
}

define tpas_proc() {
   variable p = tpas_pos();
   variable name = tpas_getname("Procedure:");
   ins_snlp(p, Sprintf("PROCEDURE %s();", name, 1));
   tpas_paspf(p, name);
   bsearch(");");
}

define tpas_func() {
   variable p = tpas_pos();
   variable name = tpas_getname("Function:");
   ins_snlp(p, Sprintf("FUNCTION %s() : ;", name, 1));
   tpas_paspf(p, name);
   bsearch(") :");
}

define tpas_wrap_hook() {
   variable p;
   push_spot();
   go_up_1 ();  bol_skip_white();
   p = POINT;
   if (looking_at("BEGIN")) {
      go_down_1 (); skip_white ();
      p = what_column ();
      bol_trim ();
      whitespace (p + tpas_indent);
   }
   pop_spot();
}

% --------------------------------------------------------------
% keymap definiiton
%
variable tpmode = "TPas";
!if (keymap_p(tpmode)) {
   make_keymap(tpmode);
}

define tpas_set_localkeys() {
   local_setkey("tpas_main", "^Cm");
   local_setkey("tpas_unit", "^Cu");
   local_setkey("tpas_proc", "^Cp");
   local_setkey("tpas_func", "^Cf");
}

create_syntax_table(tpmode);
define_syntax("(*", "*)", '%', tpmode);
define_syntax ("([", ")]", '(', tpmode);
define_syntax ('\'', '\'', tpmode);
define_syntax ("0-9a-zA-Z_", 'w', tpmode);        % words
define_syntax ("-+0-9a-FA-F.", '0', tpmode);   % Numbers
define_syntax (",;.?:", ',', tpmode);
define_syntax ("@$()[]%-+/*=<>^", '+', tpmode);
set_syntax_flags (tpmode, 5); % case insensitive + C-mode

#ifdef HAS_DFA_SYNTAX
enable_highlight_cache("tpascal.dfa", tpmode);
define_highlight_rule("\\(\\*.*\\*\\)", "Qcomment", tpmode);
define_highlight_rule("^([^\\(]|\\([^\\*])*\\*\\)", "Qcomment", tpmode);
define_highlight_rule("\\(\\*.*", "comment", tpmode);
define_highlight_rule("{.*}", "Qcomment", tpmode);
define_highlight_rule("^[^{]*}", "Qcomment", tpmode);
define_highlight_rule("{.*", "comment", tpmode);
define_highlight_rule("^[ \t]*\\*+([ \t].*)?$", "comment", tpmode);
define_highlight_rule("[A-Za-z_][A-Za-z_0-9]*", "Knormal", tpmode);
define_highlight_rule("[0-9]+(\\.[0-9]+)?([Ee][\\+\\-]?[0-9]*)?",
                      "number", tpmode);
define_highlight_rule("\\$[0-9A-Fa-f]*", "number", tpmode);
define_highlight_rule("'[^']*'", "string", tpmode);
define_highlight_rule("'[^']*$", "string", tpmode);
define_highlight_rule("#($[0-9A-Fa-f]+|[0-9]+)", "string", tpmode);
define_highlight_rule("[ \t]+", "normal", tpmode);
define_highlight_rule("[\\(\\[\\]\\),;\\.\\?:]", "delimiter", tpmode);
define_highlight_rule("[@\\-\\+/\\*=<>\\^]", "operator", tpmode);
build_highlight_table(tpmode);
#endif

() = define_keywords (tpmode, "doifofto", 2);
() = define_keywords (tpmode, "endfornewnilsetvar", 3);
() = define_keywords (tpmode, "bytecasecharelseexitfilehaltrealtextthentypeunituseswithword", 4);
() = define_keywords (tpmode, "arraybeginconstuntilwhile", 5);
() = define_keywords (tpmode, "downtoinlineobjectrecordrepeatstring", 6);
() = define_keywords (tpmode, "booleanintegerlongintpointerprogram", 7);
() = define_keywords (tpmode, "functionshortint", 8);
() = define_keywords (tpmode, "interfaceotherwiseprocedure", 9);
() = define_keywords (tpmode, "implementation", 14);

define tpas_par_sep_hook() {
   variable p;
   push_spot();
   pop_spot();
}

% --------------------------------------------------------------
% Main entry
%
define tpas_mode () {
   set_mode(tpmode, 2);
   use_keymap(tpmode);
   use_syntax_table(tpmode);
   set_buffer_hook("wrap_hook", "tpas_wrap_hook");
   set_buffer_hook("par_sep", "tpas_par_sep_hook");
   tpas_set_localkeys();
   runhooks("tpas_mode_hook");
}
