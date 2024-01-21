/*
 *  Copyright (c) 1992, 1995 John E. Davis  (davis@space.mit.edu)
 *  All Rights Reserved.
 */
#define PC_NULL	"\xE0^C"
#define PC_SLASH	"\033OQ"
#define PC_STAR		"\033OR"
#define PC_MINUS	"\033OS"
#define PC_PLUS		"\033Om"
#define PC_ENTER	"\033OM"
#define PC_KP5		"\033Ou"
#define PC_UP		"\xE0H"
#define PC_UP1		"\033Ox"
#define PC_DN		"\xE0P"
#define PC_DN1		"\033Or"
#define PC_RT		"\xE0M"
#define PC_RT1		"\033Ov"
#define PC_LT		"\xE0K"
#define PC_LT1		"\033Ot"
#define PC_PGUP		"\xE0I"
#define PC_PGUP1	"\033Oy"
#define PC_PGDN		"\xE0Q"
#define PC_PGDN1	"\033Os"
#define PC_INS		"\xE0R"
#define PC_INS1		"\033Op"
#define PC_DEL		"\xE0S"
#define PC_DEL1		"\033On"
#define PC_END		"\xE0O"
#define PC_END1		"\033Oq"
#define PC_HOME		"\xE0G"
#define PC_HOME1	"\033Ow"

#define PC_F1	"^@;"
#define PC_F2	"^@<"
#define PC_F3	"^@="
#define PC_F4	"^@>"
#define PC_F5	"^@?"
#define PC_F6	"^@@"
#define PC_F7	"^@A"
#define PC_F8	"^@B"
#define PC_F9	"^@C"
#define PC_F10	"^@D"

#define PC_ALT_F1	"^@h"
#define PC_ALT_F2	"^@i"
#define PC_ALT_F3	"^@j"
#define PC_ALT_F4	"^@k"
#define PC_ALT_F5	"^@l"
#define PC_ALT_F6	"^@m"
#define PC_ALT_F7	"^@n"
#define PC_ALT_F8	"^@o"
#define PC_ALT_F9	"^@p"
#define PC_ALT_F10	"^@q"

#define PC_SHIFT_F1	"^@T"
#define PC_SHIFT_F2	"^@U"
#define PC_SHIFT_F3	"^@V"
#define PC_SHIFT_F4	"^@W"
#define PC_SHIFT_F5	"^@X"
#define PC_SHIFT_F6	"^@Y"
#define PC_SHIFT_F7	"^@Z"
#define PC_SHIFT_F8	"^@["
#define PC_SHIFT_F9	"^@\\"
#define PC_SHIFT_F10	"^@]"

SLkm_define_key (PC_DEL, (FVOID_STAR) delete_char_cmd, Global_Map);
SLkm_define_key (PC_DEL1, (FVOID_STAR) delete_char_cmd, Global_Map);
SLkm_define_key (PC_NULL, (FVOID_STAR) set_mark_cmd, Global_Map);
SLkm_define_key (PC_LT, (FVOID_STAR) previous_char_cmd, Global_Map);
SLkm_define_key (PC_LT1, (FVOID_STAR) previous_char_cmd, Global_Map);
SLkm_define_key (PC_UP, (FVOID_STAR) previous_line_cmd, Global_Map);
SLkm_define_key (PC_UP1, (FVOID_STAR) previous_line_cmd, Global_Map);
SLkm_define_key (PC_DN, (FVOID_STAR) next_line_cmd, Global_Map);
SLkm_define_key (PC_DN1, (FVOID_STAR) next_line_cmd, Global_Map);
SLkm_define_key (PC_RT, (FVOID_STAR) next_char_cmd, Global_Map);
SLkm_define_key (PC_RT1, (FVOID_STAR) next_char_cmd, Global_Map);
SLkm_define_key (PC_PGUP, (FVOID_STAR) pageup_cmd, Global_Map);
SLkm_define_key (PC_PGUP1, (FVOID_STAR) pageup_cmd, Global_Map);
SLkm_define_key (PC_PGDN, (FVOID_STAR) pagedown_cmd, Global_Map);
SLkm_define_key (PC_PGDN1, (FVOID_STAR) pagedown_cmd, Global_Map);
SLkm_define_key (PC_HOME, (FVOID_STAR) bol, Global_Map);
SLkm_define_key (PC_HOME1, (FVOID_STAR) bol, Global_Map);
SLkm_define_key (PC_END, (FVOID_STAR) eol_cmd, Global_Map);
SLkm_define_key (PC_END1, (FVOID_STAR) eol_cmd, Global_Map);

/* Now special keypad stuff */
SLkm_define_key (PC_ENTER, (FVOID_STAR) newline, Global_Map);

/* wordperfect type stuff */
SLkm_define_key (PC_F1, (FVOID_STAR) kbd_quit, Global_Map);
/* SLkm_define_key (PC_F2, (FVOID_STAR) search_forward_cmd, Global_Map);
SLkm_define_key (PC_SHIFT_F2, (FVOID_STAR) search_backward_cmd, Global_Map); */
SLkm_define_key (PC_F4, (FVOID_STAR) indent_line, Global_Map);
SLkm_define_key (PC_ALT_F5, (FVOID_STAR) set_mark_cmd, Global_Map);
SLkm_define_key (PC_SHIFT_F4, (FVOID_STAR) narrow_paragraph, Global_Map);
SLkm_define_key (PC_SHIFT_F6, (FVOID_STAR) center_line, Global_Map);
SLkm_define_key (PC_F7, (FVOID_STAR) exit_jed, Global_Map);
