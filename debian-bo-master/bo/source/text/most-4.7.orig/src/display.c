#include <stdio.h>
#include <slang.h>
#include "jdmacros.h"

#include "display.h"

void most_narrow_width(void)
{
   SLsmg_refresh ();
   SLtt_write_string ("\033[?3l");
}

void most_wide_width(void)
{
   SLsmg_refresh ();
   SLtt_write_string ("\033[?3h");
}

void most_tt_bold_video (void)
{
   SLsmg_set_color (2);
}


void most_tt_normal_video (void)
{
   SLsmg_set_color (0);
}

void most_tt_underline_video (void)
{
   SLsmg_set_color (3);
}

void most_tt_reverse_video (void)
{
   SLsmg_set_color (1);
}

void most_goto_rc (int r, int c)
{
   SLsmg_gotorc (r - 1, c - 1);
}


void most_setup_colors (void)
{
   SLtt_set_mono (1, NULL, SLTT_REV_MASK);
   SLtt_set_mono (2, NULL, SLTT_BOLD_MASK);
   SLtt_set_mono (3, NULL, SLTT_ULINE_MASK);
}
