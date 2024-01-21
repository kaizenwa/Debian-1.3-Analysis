#ifndef __COLOR_H
#define __COLOR_H

void init_colors (void);
void toggle_color_mode (void);
void configure_colors_string (char *color_string);

extern int hascolors;
extern int use_colors;
extern int disable_colors;

extern int attr_pairs [];

#ifdef HAVE_SLANG
#   define MY_COLOR_PAIR(x) COLOR_PAIR(x)
#else
#   define MY_COLOR_PAIR(x) (COLOR_PAIR(x) | attr_pairs [x])
#endif

#define NORMAL_COLOR   (use_colors?MY_COLOR_PAIR (1): 0)
#define SELECTED_COLOR (use_colors?MY_COLOR_PAIR (2):A_REVERSE)
#define MARKED_COLOR   (use_colors?MY_COLOR_PAIR (3):A_BOLD)
#define MARKED_SELECTED_COLOR (use_colors?MY_COLOR_PAIR (4):A_REVERSE | A_BOLD)
#define ERROR_COLOR (use_colors?MY_COLOR_PAIR (5):0)
#define MENU_ENTRY_COLOR (use_colors?MY_COLOR_PAIR (6):A_REVERSE)
#define REVERSE_COLOR (use_colors?MY_COLOR_PAIR(7):A_REVERSE)
#define INPUT_COLOR (use_colors?MY_COLOR_PAIR(2):0)
#define Q_SELECTED_COLOR (use_colors?SELECTED_COLOR: 0)
#define Q_UNSELECTED_COLOR REVERSE_COLOR
#define VIEW_UNDERLINED_COLOR (use_colors?MY_COLOR_PAIR(12):A_UNDERLINE)
#define MENU_SELECTED_COLOR (use_colors?MY_COLOR_PAIR(13):A_BOLD)
#define MENU_HOT_COLOR (use_colors?MY_COLOR_PAIR(14):0)
#define MENU_HOTSEL_COLOR (use_colors?MY_COLOR_PAIR(15):0)
/* This should be selectable independently. Default has to be black bg.
   fg doesn't matter at all. */
#define GAUGE_COLOR (use_colors?MY_COLOR_PAIR(21):0)
#ifdef HAVE_SLANG
#    define DEFAULT_COLOR (use_colors?MY_COLOR_PAIR(31):0)
#   else
#     define DEFAULT_COLOR A_NORMAL
#endif
#define HELP_NORMAL_COLOR (use_colors?MY_COLOR_PAIR(16):A_REVERSE)
#define HELP_ITALIC_COLOR (use_colors?MY_COLOR_PAIR(17):A_REVERSE)
#define HELP_BOLD_COLOR (use_colors?MY_COLOR_PAIR(18):A_REVERSE)
#define HELP_LINK_COLOR (use_colors?MY_COLOR_PAIR(19):0)
#define HELP_SLINK_COLOR (use_colors?MY_COLOR_PAIR(20):A_BOLD)

extern int sel_mark_color [4];
extern int dialog_colors [4];

#define COLOR_NORMAL     (use_colors?MY_COLOR_PAIR (8):A_REVERSE)
#define COLOR_FOCUS      (use_colors?MY_COLOR_PAIR (9):A_BOLD)
#define COLOR_HOT_NORMAL (use_colors?MY_COLOR_PAIR (10):0)
#define COLOR_HOT_FOCUS  (use_colors?MY_COLOR_PAIR (11):0)

#endif /* __COLOR_H */

