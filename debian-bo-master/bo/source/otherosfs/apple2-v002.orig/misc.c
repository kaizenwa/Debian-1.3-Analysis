#include <stdio.h>
#include <stdlib.h>
#include <vga.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <vgakeyboard.h>

int			debug_start;
int			debug_end;

int			chipset;		/* VGA chipset		   */
int			page_2_offset;		/* SVGA page 2 screen off. */

#define C_Flag		0x1			/* Carry	 	   */
#define X_Flag		0x2			/* X-tra		   */
#define I_Flag		0x4			/* Interrupt disable       */
#define V_Flag		0x8			/* Overflow	           */
#define B_Flag		0x10			/* Break		   */
#define D_Flag		0x20			/* Decimal mode	           */
#define Z_Flag		0x40			/* Zero		   	   */
#define N_Flag		0x80			/* Neg		   	   */

#define C_Flag_6502	0x1			/* Carry on 6502	   */
#define X_Flag_6502	0x20			/* Xtra on 6502		   */
#define I_Flag_6502	0x4			/* Interrupt flag on 6502  */
#define V_Flag_6502	0x40			/* Overflow flag on 6502   */
#define B_Flag_6502	0x10			/* Break flag on 6502	   */
#define D_Flag_6502	0x8			/* Decimal mode flag on 6502 */
#define Z_Flag_6502	0x2			/* Zero flag on 6502	   */
#define N_Flag_6502	0x80			/* Neg flag on 6502	   */

#define	K *1024

typedef enum { False, True } Tr;	/* --- Domain of Truth values
					       (using the notation by
					       John Allen -
						"The Anatomy of LISP") --- */

typedef void (*Function)();

/* ----------------------------------
    External jump table functions
   ---------------------------------- */

extern void	read_ram_default();

extern void	write_ram_default(),
		write_ram_text_page0(),
		write_ram_text_mixed0(),
		write_ram_text_page1(),
		write_ram_text_mixed1(),
		write_ram_hires_page0_even(),
		write_ram_hires_page0_odd(),
		write_ram_hires_mixed0_even(),
		write_ram_hires_mixed0_odd(),
		write_ram_hires_page1_even(),
		write_ram_hires_page1_odd(),
		write_ram_hires_mixed1_even(),
		write_ram_hires_mixed1_odd(),

		ram_nop(),
		read_keyboard_strobe(),
		c_read_keyboard_strobe(),
		read_random(),
		read_speaker_toggle_pc(),
		read_switch_primary_page(),
		read_switch_secondary_page(),
		read_switch_graphics(),
		read_switch_text(),
		read_switch_no_mixed(),
		read_switch_mixed(),
		read_switch_lores(),
		read_switch_hires(),

		read_button0(),
		read_button1(),
		read_button2(),
		read_gc0(),
		read_gc1(),
		read_gc_strobe(),
    
		lc_c080(),
		lc_c081(),
		lc_c082(),
		lc_c083(),
		lc_c088(),
		lc_c089(),
		lc_c08a(),
		lc_c08b(),

		disk_read_nop(),
		disk_read_phase(),
		disk_read_motor_off(),
		disk_read_motor_on(),
		disk_read_select_a(),
		disk_read_select_b(),
		disk_read_byte(),
		disk_read_latch(),
		disk_write_latch(),
		disk_read_prepare_in(),
		disk_read_prepare_out();

extern void	c_init_6();

extern void	update_video_screen();

extern unsigned char read_keyboard();
extern void c_initialize_keyboard();
extern void c_initialize_mouse();

unsigned short	  video_line_offset[24] =
		   { 0x000, 0x080, 0x100, 0x180, 0x200, 0x280, 0x300, 0x380,
		     0x028, 0x0A8, 0x128, 0x1A8, 0x228, 0x2A8, 0x328, 0x3A8,
		     0x050, 0x0D0, 0x150, 0x1D0, 0x250, 0x2D0, 0x350, 0x3D0 };

unsigned char     *GM;	     /* --- Base address of graphic area --- */
unsigned char     *svga_GM;  /* --- SVGA base address of graphic area --- */

Tr		  normal_vga_mode = False;
Tr		  force_vga_mode = False;

unsigned char     char_rom[0x800];           /* --- Character ROM --- */
unsigned char     expanded_font[0x800*8];    /* --- Font --- */

unsigned char     expanded_col_highres_even[0x100*8]; /* --- Precalculated color
                                                         bytes for highres. --- */

unsigned char     expanded_col_highres_odd[0x100*8]; /* --- Precalculated color
                                                        bytes for highres. --- */

unsigned char     text_page_rows[1 K];	   /* --- Precalculated text page
						  rows given offset addr --- */
unsigned char	  text_page_cols[1 K];	   /* --- Precalculated text page
						  cols given offset addr --- */
unsigned short	  hires_page_offset[8 K];   /* --- Precalculated hi-res page
						   SVGA offsets given addr --- */

unsigned char	  table_encode_flags[256];
unsigned char	  table_decode_flags[256];

unsigned char	  apple_ii_64k[64 K];
unsigned char	  apple_ii_rom[12 K];
unsigned char	  language_card[8 K];
unsigned char	  language_bank0[4 K];
unsigned char	  language_bank1[4 K];

unsigned char	  language_current_bank;
unsigned char	  language_card_second;
unsigned char	  language_card_write;
unsigned char	  language_card_read;

unsigned char	  exception_type = 0;
unsigned char	  exception_flag = 0;

Function	  table_read_memory[64 K];
Function	  table_write_memory[64 K];

unsigned char	  vmode_text;
unsigned char	  vmode_mixed;
unsigned char	  vmode_page2;
unsigned char	  vmode_hires;
unsigned char	  vmode_active;

unsigned char	  current_slot;

unsigned short	  apple_speed = 1;
short	 	  color_mode = 1;
short		  sound_mode = 1;
extern short   	  joy_mode;
extern short	  joy_step;
extern short	  joy_center_x;
extern short	  joy_center_y;

unsigned char	  random_value = 0;

extern unsigned char	disk_path[ 1024 ];
unsigned char	 	system_path[ 2048 ] = ".";

unsigned char	  temp[ 4096 ];

#include "colors.h"

#define GR_AREA_PAGE_0		0
#define GR_AREA_PAGE_1          1

#define X_OFFSET		20
#define Y_OFFSET		4

unsigned char	vga_mem_page_0[ 64 K ];  /* Only used in standard VGA */
unsigned char	vga_mem_page_1[ 64 K ];	 /* Only used in standard VGA */

int		crnt_visual_svga_page = -1; /* Current visual SVGA page */
int		crnt_active_svga_page = -2; /* Current active SVGA page */

/* -------------------------------------------------------------------------
    c_setpage(p):    Set SVGA 64k page and update GM
                     (Graph Memory base address)
   ------------------------------------------------------------------------- */

void c_setpage(p)
{
    if (!normal_vga_mode)
    {
        vga_setpage( p );
        svga_GM = GM = vga_getgraphmem();
    }
    else
    {
        c_lock_menu_mode_on();
        svga_GM = vga_getgraphmem();
	GM = (crnt_visual_svga_page == p) ? svga_GM :
	     ((p == 0) ? vga_mem_page_0 : vga_mem_page_1);
        c_lock_menu_mode_off();

        crnt_active_svga_page = p;
    }
}

/* -------------------------------------------------------------------------
    c_setscreen(p):    Switch to screen page p
   ------------------------------------------------------------------------- */

void c_setscreen(int m)
{
    if (!normal_vga_mode)
        vga_setdisplaystart( m * page_2_offset );
    else
    {
        c_wait_menu_buf_empty();
        c_lock_menu_mode_on();
        if (m != crnt_visual_svga_page)
        {
	    memcpy( ((crnt_visual_svga_page == 0) ?
		        vga_mem_page_0 : vga_mem_page_1),
		    svga_GM, 64 K );
	    memcpy( svga_GM, ((m == 0) ? vga_mem_page_0 : vga_mem_page_1),
		    64 K );
	    crnt_visual_svga_page = m;
	    GM = (crnt_active_svga_page == m) ? svga_GM :
	             ((crnt_active_svga_page == 0) ?
		          vga_mem_page_0 : vga_mem_page_1);
	}
        c_lock_menu_mode_off();
    }
}

void c_update_video_screen()
{
    int		i;
    
    c_wait_menu_buf_empty();
    vga_clear();

    if (normal_vga_mode)
	memset( vga_mem_page_0, 0, 64 K ),
	memset( vga_mem_page_1, 0, 64 K );

    update_video_screen();
}

/* -------------------------------------------------------------------------
    c_initialize_highres_values()
   ------------------------------------------------------------------------- */

void c_initialize_highres_values()
{
    int     value, b, v, e, color_toggle, last_not_black;

    for (value = 0x00; value <= 0xFF; value++)
	for (e = value * 8, last_not_black = False, v = value, b = 0;
	     b < 7; b++, v >>= 1, e++)
	{
	    if (v & 1)
	    {
		expanded_col_highres_even[ e ] = last_not_black ?
					    COLOR_LIGHT_WHITE :
					((b & 1) ?
					    ((value & 0x80) ?
					         COLOR_LIGHT_RED :
						 COLOR_LIGHT_PURPLE) :
					    ((value & 0x80) ?
						 COLOR_LIGHT_BLUE :
						 COLOR_LIGHT_GREEN));

		expanded_col_highres_odd[ e ] = last_not_black ?
					    COLOR_LIGHT_WHITE :
					((b & 1) ?
					    ((value & 0x80) ?
					         COLOR_LIGHT_BLUE :
						 COLOR_LIGHT_GREEN) :
					    ((value & 0x80) ?
						 COLOR_LIGHT_RED :
						 COLOR_LIGHT_PURPLE));
		if (last_not_black && b > 0)
		    expanded_col_highres_even[ e - 1 ] = COLOR_LIGHT_WHITE,
		    expanded_col_highres_odd[ e - 1 ] = COLOR_LIGHT_WHITE;

		last_not_black = True;
	    }
	    else
		expanded_col_highres_even[ e ] = COLOR_BLACK,
		expanded_col_highres_odd[ e ] = COLOR_BLACK,
	        last_not_black = False;
	}

    if (color_mode == 0) /* No color */
    {
        for (value = 0x00; value <= 0xFF; value++)
	    for (b = 0, e = value * 8; b < 7; b++, e++)	
	    {
		if (expanded_col_highres_even[ e ] != COLOR_BLACK)
		    expanded_col_highres_even[ e ] = COLOR_LIGHT_WHITE;
		if (expanded_col_highres_odd[ e ] != COLOR_BLACK)
		    expanded_col_highres_odd[ e ] = COLOR_LIGHT_WHITE;
	    }
    }
    else
    if (color_mode == 2) /* Color and interpolated */
    {
        for (value = 0x00; value <= 0xFF; value++)
	{
	    for (b = 1, e = value * 8 + 1; b <= 5; b += 2, e += 2)	
	    {
		if (expanded_col_highres_even[ e ] == COLOR_BLACK &&
		    expanded_col_highres_even[ e - 1 ] != COLOR_BLACK &&
		    expanded_col_highres_even[ e + 1 ] != COLOR_BLACK)
		      expanded_col_highres_even[ e ] =
		      expanded_col_highres_even[ e - 1 ];

		if (expanded_col_highres_odd[ e ] == COLOR_BLACK &&
		    expanded_col_highres_odd[ e - 1 ] != COLOR_BLACK &&
		    expanded_col_highres_odd[ e + 1 ] != COLOR_BLACK)
		      expanded_col_highres_odd[ e ] =
		      expanded_col_highres_odd[ e - 1 ];
	    }

	    for (b = 0, e = value * 8; b <= 6; b += 2, e += 2)	
	    {
		if (expanded_col_highres_odd[ e ] == COLOR_BLACK)
		{
		    if (b > 0 && b < 6)
		        if (expanded_col_highres_even[ e + 1 ] != COLOR_BLACK &&
			    expanded_col_highres_even[ e - 1 ] != COLOR_BLACK &&
			    expanded_col_highres_even[ e + 1 ] != COLOR_LIGHT_WHITE &&
			    expanded_col_highres_even[ e - 1 ] != COLOR_LIGHT_WHITE)
		            expanded_col_highres_even[ e ] =
		                expanded_col_highres_even[ e - 1 ];
			else;
		    else
		    if (b == 0)
			if (expanded_col_highres_even[ e + 1 ] != COLOR_BLACK &&
			    expanded_col_highres_even[ e + 1 ] != COLOR_LIGHT_WHITE)
			    expanded_col_highres_even[ e ] =
				expanded_col_highres_even[ e + 1 ];
			else;
		    else
			if (expanded_col_highres_even[ e - 1 ] != COLOR_BLACK &&
			    expanded_col_highres_even[ e - 1 ] != COLOR_LIGHT_WHITE)
			    expanded_col_highres_even[ e ] =
				expanded_col_highres_even[ e - 1 ];
		}

		if (expanded_col_highres_odd[ e ] == COLOR_BLACK)
		{
		    if (b > 0 && b < 6)
		        if (expanded_col_highres_odd[ e + 1 ] != COLOR_BLACK &&
			    expanded_col_highres_odd[ e - 1 ] != COLOR_BLACK &&
			    expanded_col_highres_odd[ e + 1 ] != COLOR_LIGHT_WHITE &&
			    expanded_col_highres_odd[ e - 1 ] != COLOR_LIGHT_WHITE)
		            expanded_col_highres_odd[ e ] =
		                expanded_col_highres_odd[ e - 1 ];
			else;
		    else
		    if (b == 0)
			if (expanded_col_highres_odd[ e + 1 ] != COLOR_BLACK &&
			    expanded_col_highres_odd[ e + 1 ] != COLOR_LIGHT_WHITE)
			    expanded_col_highres_odd[ e ] =
				expanded_col_highres_odd[ e + 1 ];
			else;
		    else
			if (expanded_col_highres_odd[ e - 1 ] != COLOR_BLACK &&
			    expanded_col_highres_odd[ e - 1 ] != COLOR_LIGHT_WHITE)
			    expanded_col_highres_odd[ e ] =
				expanded_col_highres_odd[ e - 1 ];
		}
	    }
	}
    }
}

/* -------------------------------------------------------------------------
    c_initialize_font():     Initialize ROM character table
   ------------------------------------------------------------------------- */

void c_initialize_font()
{
    int			c, i, j, v, p;

    FILE *f;

    sprintf(temp, "%s/character.rom", system_path);
    if ((f = fopen(temp, "r")) == NULL)
    {
	printf("Cannot find file 'character.rom'.\n");
	exit(1);
    }
    fread( char_rom, 1, 0x800, f );
    fclose( f );

    for (p = 0, c = 0; c < 256; c++)
	for (i = c * 8; i < c * 8 + 8; i++, p++)
	    for (v = char_rom[ i ] >> 2, j = 0; j < 7; j++, v >>= 1, p++)
	        expanded_font[ p ] = (c < 128) ?
					  ((v & 1) ? 0 : COLOR_LIGHT_WHITE) :
					  ((v & 1) ? COLOR_LIGHT_WHITE : 0);

    for (c = 0x40, p = c * 64; c < 0x80; c++)
        for (i = c * 8; i < c * 8 + 64; i++, p++)
	    expanded_font[ p ] = (expanded_font[ p ] == COLOR_LIGHT_WHITE) ?
	    				COLOR_FLASHING_WHITE :
					COLOR_FLASHING_BLACK;
}

/* -------------------------------------------------------------------------
    c_initialize_row_col_tables()
   ------------------------------------------------------------------------- */

void c_initialize_row_col_tables()
{
    int			x, y, off;

    for (y = 0; y < 24; y++)
	for (x = 0; x < 40; x++)
	    text_page_rows[ video_line_offset[ y ] + x ] = y,
	    text_page_cols[ video_line_offset[ y ] + x ] = x;

    for (y = 0; y < 24; y++)
	for (off = 0; off < 8; off++)
	    for (x = 0; x < 40; x++)
		hires_page_offset[ video_line_offset[ y ] + 0x400 * off + x ]
		           = (y * 8 + off + 4) * 320 + x * 7 + 20;
}

/* -------------------------------------------------------------------------
    c_initialize_colors():    Initialize color palette
   ------------------------------------------------------------------------- */

void c_initialize_colors()
{
    static int			col[ 32 ] = { 0, 6, 10, 14, 18, 22, 25, 28,
					      31, 34, 37, 39, 41, 43, 45, 47,
					      48, 49, 50, 51, 52, 53, 54, 55,
					      56, 57, 58, 59, 60, 61, 62, 63 };

    static int			col2[ 3 ] = { 27, 40, 62 };

    int				i, j;

    for (i = 0; i < 8; i++)
	for (j = 0; j < 32; j++)
	    vga_setpalette( j+i*32, (i & 1) ? col[ j ] : 0,
                                    (i & 2) ? col[ j ] : 0,
                                    (i & 4) ? col[ j ] : 0 );

    for (i = 0; i < 8; i++)
	for (j = 0; j < 3; j++)
	    vga_setpalette( j+i*3+32, (i & 1) ? col2[ j ] : 0,
                                      (i & 2) ? col2[ j ] : 0,
                                      (i & 4) ? col2[ j ] : 0 );

    vga_setpalette( COLOR_FLASHING_BLACK, 0, 0, 0 );
    vga_setpalette( COLOR_FLASHING_WHITE, 63, 63, 63 );

    /* Low resolution colors */

    vga_setpalette( 1, 255, 0, 255 ); 		/* Magenta 		*/
    vga_setpalette( 2, 0, 0, 255 );		/* Medium blue		*/
    vga_setpalette( 3, 160, 32, 240 );		/* Purple		*/
    vga_setpalette( 4, 0, 100, 0 );		/* Dark green		*/
    vga_setpalette( 5, 127, 127, 127 );		/* Gray	(50%)		*/
    vga_setpalette( 6, 30, 144, 255 );		/* Dodger blue		*/
    vga_setpalette( 7, 173, 216, 230 );		/* Light blue		*/
    vga_setpalette( 8, 165, 42, 42 );		/* Brown		*/
    vga_setpalette( 9, 255, 69, 0 );		/* Orange red		*/
    vga_setpalette( 10, 127, 127, 127 );	/* Gray (50%)		*/
    vga_setpalette( 11, 255, 192, 203 );	/* Pink			*/
    vga_setpalette( 12, 0, 255, 0 );		/* Green		*/
    vga_setpalette( 13, 255, 255, 0 );		/* Yellow		*/
    vga_setpalette( 14, 127, 255, 212 );	/* Aquamarine		*/
    vga_setpalette( 15, 255, 255, 255 );	/* White		*/
}

/* -------------------------------------------------------------------------
    c_initialize_tables()
   ------------------------------------------------------------------------- */

void c_initialize_tables()
{
    int		i, x, y, off;

    for (i = 0; i < 256; i++)
    {
	unsigned char	val = 0;

	if (i & C_Flag)
	    val |= C_Flag_6502;
	if (i & X_Flag)
	    val |= X_Flag_6502;
	if (i & I_Flag)
	    val |= I_Flag_6502;
	if (i & V_Flag)
	    val |= V_Flag_6502;
	if (i & B_Flag)
	    val |= B_Flag_6502;
	if (i & D_Flag)
	    val |= D_Flag_6502;
	if (i & Z_Flag)
	    val |= Z_Flag_6502;
	if (i & N_Flag)
	    val |= N_Flag_6502;

	table_encode_flags[ i ] = val | 0x20;
	table_decode_flags[ val ] = i;
    }

    for (i = 0; i < 0x10000; i++)
	table_read_memory[ i ] = read_ram_default,
	table_write_memory[ i ] = write_ram_default;

    for (i = 0xC000; i < 0x10000; i++)
	table_write_memory[ i ] = ram_nop;

    for (y = 0; y < 24; y++)
	for (x = 0; x < 40; x++)
	{
	    table_write_memory[ video_line_offset[ y ] + x + 0x400] =
			(y < 20) ? write_ram_text_page0 :
				   write_ram_text_mixed0;

	    table_write_memory[ video_line_offset[ y ] + x + 0x800] =
			(y < 20) ? write_ram_text_page1 :
				   write_ram_text_mixed1;

	    for (i = 0; i < 8; i++)
	    {
	        table_write_memory[ 0x2000 + video_line_offset[ y ]
       				    + 0x400 * i + x ] =
		        (y < 20) ? ((x & 1) ? write_ram_hires_page0_odd :
					      write_ram_hires_page0_even)
				 : ((x & 1) ? write_ram_hires_mixed0_odd :
					      write_ram_hires_mixed0_even);

	        table_write_memory[ 0x4000 + video_line_offset[ y ]
       				    + 0x400 * i + x ] =
		        (y < 20) ? ((x & 1) ? write_ram_hires_page1_odd :
					      write_ram_hires_page1_even)
				 : ((x & 1) ? write_ram_hires_mixed1_odd :
					      write_ram_hires_mixed1_even);
	    }
	}

    for (i = 0xC000; i < 0xC010; i++)
        table_read_memory[ i ] = (Function) read_keyboard;
    for (i = 0xC000; i < 0xC010; i++)
	table_write_memory[ i ] = ram_nop;
#if 0
    for (i = 0xC010; i < 0xC020; i++)
        table_read_memory[ i ] = table_write_memory[ i ] = read_keyboard_strobe;
#else
    for (i = 0xC010; i < 0xC020; i++)
        table_read_memory[ i ] = table_write_memory[ i ] = c_read_keyboard_strobe;
#endif
    
    for (i = 0xC020; i < 0xC030; i++)
	table_read_memory[ i ] = table_write_memory[ i ] = read_random;

    table_read_memory[ 0xC054 ] = table_write_memory[ 0xC054 ] =
        read_switch_primary_page;
    table_read_memory[ 0xC055 ] = table_write_memory[ 0xC055 ] =
        read_switch_secondary_page;

    table_read_memory[ 0xC050 ] = table_write_memory[ 0xC050 ] =
	read_switch_graphics;
    table_read_memory[ 0xC051 ] = table_write_memory[ 0xC051 ] =
	read_switch_text;

    table_read_memory[ 0xC052 ] = table_write_memory[ 0xC052 ] =
	read_switch_no_mixed;
    table_read_memory[ 0xC053 ] = table_write_memory[ 0xC053 ] =
	read_switch_mixed;

    table_read_memory[ 0xC056 ] = table_write_memory[ 0xC056 ] =
	read_switch_lores;
    table_read_memory[ 0xC057 ] = table_write_memory[ 0xC057 ] =
	read_switch_hires;

    table_read_memory[ 0xC061 ] = table_read_memory[ 0xC069 ] =
        read_button0;
    table_read_memory[ 0xC062 ] = table_read_memory[ 0xC06A ] =
        read_button1;
    table_read_memory[ 0xC063 ] = table_read_memory[ 0xC06B ] =
        read_button2;
    table_read_memory[ 0xC064 ] = table_read_memory[ 0xC06C ] =
        read_gc0;
    table_read_memory[ 0xC065 ] = table_read_memory[ 0xC06D ] =
        read_gc1;
    for (i = 0xC070; i < 0xC080; i++)
	table_read_memory[ i ] = table_write_memory[ i ] = read_gc_strobe;

    table_read_memory[ 0xC080 ] = table_write_memory[ 0xC080 ] =
    table_read_memory[ 0xC084 ] = table_write_memory[ 0xC084 ] =
	lc_c080;
    table_read_memory[ 0xC081 ] = table_write_memory[ 0xC081 ] =
    table_read_memory[ 0xC085 ] = table_write_memory[ 0xC085 ] =
	lc_c081;
    table_read_memory[ 0xC082 ] = table_write_memory[ 0xC082 ] =
    table_read_memory[ 0xC086 ] = table_write_memory[ 0xC086 ] =
	lc_c082;
    table_read_memory[ 0xC083 ] = table_write_memory[ 0xC083 ] =
    table_read_memory[ 0xC087 ] = table_write_memory[ 0xC087 ] =
	lc_c083;

    table_read_memory[ 0xC088 ] = table_write_memory[ 0xC088 ] =
    table_read_memory[ 0xC08C ] = table_write_memory[ 0xC08C ] =
	lc_c088;
    table_read_memory[ 0xC089 ] = table_write_memory[ 0xC089 ] =
    table_read_memory[ 0xC08D ] = table_write_memory[ 0xC08D ] =
	lc_c089;
    table_read_memory[ 0xC08A ] = table_write_memory[ 0xC08A ] =
    table_read_memory[ 0xC08E ] = table_write_memory[ 0xC08E ] =
	lc_c08a;
    table_read_memory[ 0xC08B ] = table_write_memory[ 0xC08B ] =
    table_read_memory[ 0xC08F ] = table_write_memory[ 0xC08F ] =
	lc_c08b;

    table_read_memory[ 0xC0E0 ] = table_read_memory[ 0xC0E2 ] =
    table_read_memory[ 0xC0E4 ] = table_read_memory[ 0xC0E6 ] =
        disk_read_nop;

    table_read_memory[ 0xC0E1 ] = table_read_memory[ 0xC0E3 ] =
    table_read_memory[ 0xC0E5 ] = table_read_memory[ 0xC0E7 ] =
        disk_read_phase;

    table_read_memory[ 0xC0E8 ] = disk_read_motor_off;
    table_read_memory[ 0xC0E9 ] = disk_read_motor_on;
    table_read_memory[ 0xC0EA ] = disk_read_select_a;
    table_read_memory[ 0xC0EB ] = disk_read_select_b;
    table_read_memory[ 0xC0EC ] = disk_read_byte;
    table_read_memory[ 0xC0ED ] = disk_read_latch;
    table_read_memory[ 0xC0EE ] = disk_read_prepare_in;
    table_read_memory[ 0xC0EF ] = disk_read_prepare_out;

    for (i = 0xC0E0; i < 0xC0F0; i++)
        table_write_memory[ i ] = table_read_memory[ i ];

    table_write_memory[ 0xC0ED ] = disk_write_latch;
}

/* -------------------------------------------------------------------------
    c_initialize_apple_ii_memory()
   ------------------------------------------------------------------------- */

void c_initialize_apple_ii_memory()
{
     FILE	*f;
     int	i;
     static int rom_loaded = 0;

     for (i = 0; i < 64 K; i++)
         apple_ii_64k[ i ] = 0;
     for (i = 0; i < 8 K; i++)
         language_card[ i ] = 0;
     for (i = 0; i < 4 K; i++)
         language_bank0[ i ] = language_bank1[ i ] = 0;

     if (!rom_loaded)
     {
         sprintf(temp, "%s/apple_II.rom", system_path);
         if ((f = fopen( temp, "r" )) == NULL)
         {
             printf("Cannot not find file 'apple_II.rom'.\n");
	     exit( 0 );
         }

         fread( apple_ii_rom, 0x3000, 1, f );
         fclose( f );
     }

     for (i = 0xD000; i < 0x10000; i++)
         apple_ii_64k[ i ] = apple_ii_rom[ i - 0xD000 ];

     for (i = 0; i < 0x1000; i++)
	 language_bank0[ i ] = apple_ii_rom[ i ];
     for (i = 0; i < 0x2000; i++)
	 language_card[ i ] = apple_ii_rom[ i + 0x1000 ];

     for (i = 0xC100; i < 0xD000; i++)
         apple_ii_64k[ i ] = i & 0xFF;

     apple_ii_64k[ 0xC000 ] = 0x00;

     sprintf(temp, "%s/slot6.rom", system_path);
     if ((f = fopen( temp, "r" )) == NULL)
     {
         printf("Cannot not find file 'slot6.rom'.\n");
	 exit( 0 );
     }

     fread( apple_ii_64k + 0xC600, 0x100, 1, f );
     fclose( f );

     rom_loaded = 1;
}

/* -------------------------------------------------------------------------
    void c_initialize_sound()
   ------------------------------------------------------------------------- */

void c_initialize_sound()
{
    int		i;

    ioperm( 0x42, 1, 1 );
    ioperm( 0x61, 1, 1 );

    for (i = 0xC030; i < 0xC040; i++)
        table_read_memory[ i ] = table_write_memory[ i ] =
	    sound_mode ? read_speaker_toggle_pc : ram_nop;
}

/* -------------------------------------------------------------------------
    void c_initialize()
   ------------------------------------------------------------------------- */

#define my_pixel(x, y, c) GM[(y)*320+(x)]=(c)

void c_initialize()
{
    int		x, y;
    vga_modeinfo *modeinfo;

    /* vga_disabledriverreport(); */

    if (force_vga_mode) {
	printf("Warning: Forcing to standard VGA mode with some "
	       "performance degradation.\n");
	chipset = VGA;
	normal_vga_mode = True;
    } else {
	chipset = vga_getcurrentchipset();
	modeinfo = vga_getmodeinfo(G320x200x256);	
	page_2_offset = 0x10000;

	if (chipset == UNDEFINED) {
	    vga_setmode(TEXT);
	    printf("SVGAlib couldn't detect an appropriate chipset.\n");
	    exit( 0 );
	}
	
	/* Trident cards use a different scan line width */
	if (chipset == TVGA8900)
	    page_2_offset /= 4;
	
	if (page_2_offset > modeinfo->startaddressrange) {
	    printf("Warning: SVGAlib cannot use SVGA features on this chipset.\n"
		   "         Standard VGA is used instead with some performance "
		   "degradation.\n");
	    normal_vga_mode = True;
	}
    }
    
    if (normal_vga_mode) {
	printf("Press RETURN to continue...");
	getchar();
    }

    c_initialize_font();
    c_load_interface_font();
    c_initialize_highres_values();
    c_initialize_row_col_tables();
    c_initialize_tables();
    c_initialize_apple_ii_memory();
    c_initialize_sound();
    c_init_6();

    vga_setchipset( chipset );
    vga_setmode( G320x200x256 );
    vga_claimvideomemory( 131072 );

    c_initialize_keyboard();

    svga_GM = GM = vga_getgraphmem();
    c_initialize_colors();

    c_setpage( 1 );

    for (y = 0; y < 200; y++)
	for (x = 0; x < 320; x++)
            my_pixel( x, y, 0 );

    c_setpage( 0 );

    for (y = 0; y < 200; y++)
	for (x = 0; x < 320; x++)
            my_pixel( x, y, 0 );

    vmode_text = vmode_mixed = True;
    vmode_page2 = vmode_hires = False;
    vmode_active = False;

    language_card_read = language_card_write = language_card_second = 0;
    language_current_bank = 0;

    c_initialize_mouse();
}

void c_read_random()
{
   random_value = (unsigned char)(rand() >> 8);
}

typedef unsigned int Reg;

/*
void print_instruction( int p )
{
    switch (p)
    {
    case 0: printf("BRK"); break;
    case 1: printf("
*/

void debug_here()
{
      printf("I'm here\n");
}

void debug( Reg eax, Reg ebx, Reg ecx, Reg edx, Reg esi, Reg edi )
{
      static int cnt = 0;

      if (++cnt >= debug_end)
      {
	    for (cnt = 0; cnt < 256; cnt++)
	    {
	        printf("%02X ", apple_ii_64k[ cnt ] );
		if (cnt % 8 == 7)
		    printf("\n");
	    }
	    keyboard_close();
	    exit( 0 );
      }

      if (cnt < debug_start)
	  return;

    printf("(%8d) ", cnt);
    printf("PC = %04X; SP = %04X; X = %02X; Y = %02X; A = %02X; (%02X) ",
	    esi, edx, ebx & 0xff, (ebx >> 8) & 0xff, ecx & 0xff, apple_ii_64k[ esi ] );

    ecx >>= 8;
    if (ecx & C_Flag)
	printf("C");
    if (ecx & X_Flag)
	printf("X");
    if (ecx & I_Flag)
	printf("I");
    if (ecx & V_Flag)
	printf("V");
    if (ecx & B_Flag)
	printf("B");
    if (ecx & D_Flag)
	printf("D");
    if (ecx & Z_Flag)
	printf("Z");
    if (ecx & N_Flag)
	printf("N");

    printf("\n");
}

void z_debug( Reg eax, Reg ebx, Reg ecx, Reg edx, Reg esi, Reg edi, Reg esp )
{
    printf("-- EBX: %d, ECX: %d\n", ebx, ecx);
/*
    printf("-- PC = %04X; SP = %04X; X = %02X; Y = %02X; A = %02X; EADDR = %04X; ESP = %08X ",
	    esi, edx, ebx & 0xff, (ebx >> 8) & 0xff, ecx & 0xff, edi, esp );

    ecx >>= 8;
    if (ecx & C_Flag)
	printf("C");
    if (ecx & X_Flag)
	printf("X");
    if (ecx & I_Flag)
	printf("I");
    if (ecx & V_Flag)
	printf("V");
    if (ecx & B_Flag)
	printf("B");
    if (ecx & D_Flag)
	printf("D");
    if (ecx & Z_Flag)
	printf("Z");
    if (ecx & N_Flag)
	printf("N");
*/

    printf("\n");
}

unsigned char * find_equal_sign( char *s )
{
    char *p = s;

    for (p = s; *p != '\0' && *p != '='; p++)
	{ }

    if (*p == '\0')
	return NULL;

    for (p++; *p != '\0' && *p == ' '; p++)
	{ }

    return p;
}

void strip_percent( char *s )
{
    char *p = s + strlen(s) - 1;
    if (p >= s && *p == '%')
	*p = '\0';
}

void lowercase_string( char *s )
{
    char *p;

    for (p = s; *p != '\0'; p++)
	*p = tolower(*p);
}

void strip_string( char *s )
{
    char *p = s + strlen(s) - 1;

    if (*p == 10)
	*p = '\0';
}

void c_system_defaults( FILE *f )
{
    static char *parameters[] =
	{ "speed", "path", "color", "sound", "joystick",
	  "origin_x", "origin_y", "sensitivity",
	  "system_path", NULL };
    static char temp[1024];
    int	   cmd, line;

    for (line = 0;;line++)
    {
        if (fgets( temp, 1000, f ) == NULL)
	    break;

	for (cmd = 0; parameters[cmd] != NULL; cmd++)
	    if (strncmp(parameters[cmd], temp, strlen(parameters[cmd])) == 0)
		break;

	if (parameters[cmd] == NULL)
	    printf("Unknown command at line %d in .apple2\n", line);
	else
	{
	    unsigned char *p = find_equal_sign( temp );
	    if (p == NULL)
		printf("Missing '=' after command at line %d in .apple2\n", line);
	    else
	    switch (cmd)
	    {
	    case 0:
		strip_string( temp );
		strip_percent( temp );
	        apple_speed = 101 - atoi( p );
		if (apple_speed < 1)
		    apple_speed = 1;
		else
		if (apple_speed > 100)
		    apple_speed = 100;
		break;
	    case 1:
		strip_string( temp );
		strcpy( disk_path, p );
		break;
	    case 2:
		strip_string( temp );
		lowercase_string( p );
		if (strcmp( p, "off" ) == 0)
		    color_mode = 0;
		else
	        if (strcmp( p, "on" ) == 0)
		    color_mode = 1;
		else
		if (strcmp( p, "interpolated" ) == 0)
		    color_mode = 2;
		else
		printf("Illegal value to color parameter at line %d in .apple2\n", line);
		break;
	    case 3:
		strip_string( temp );
		lowercase_string( p );
		if (strcmp( p, "off" ) == 0)
		    sound_mode = 0;
		else
		if (strcmp( p, "on" ) == 0)
		    sound_mode = 1;
		else
		if (strcmp( p, "pc speaker" ) == 0)
		    sound_mode = 1;
		else
		printf("Illegal value to sound parameter at line %d in .apple2\n", line);
		break;
	    case 4:
		strip_string( temp );
		lowercase_string( p );
		if (strcmp( p, "linear" ) == 0)
		    joy_mode = 0;
		else
		if (strcmp( p, "digital" ) == 0)
		    joy_mode = 1;
		else
		if (strcmp( p, "off" ) == 0)
		    joy_mode = 2;
		else
		    printf("Illegal value to joystick parameter at line %d in .apple2\n", line);
		break;
	    case 5:
		strip_string( temp );
	        joy_center_x = atoi( p );
		if (joy_center_x < 0)
		    joy_center_x = 0;
		else
		if (joy_center_x > 255)
		    joy_center_x = 255;
		break;
	    case 6:
		strip_string( temp );
	        joy_center_y = atoi( p );
		if (joy_center_y < 0)
		    joy_center_y = 0;
		else
		if (joy_center_y > 255)
		    joy_center_y = 255;
		break;
	    case 7:
		strip_string( temp );
		strip_percent( p );
	        joy_step = atoi( p );
		if (joy_step < 1)
		    joy_step = 1;
		else
		if (joy_step > 100)
		    joy_step = 100;
		break;
	    case 8:
		strip_string( temp );
		strcpy( system_path, p );
		break;
	    }
	}
    }
}

int main(int argc, char *argv[])
{
    int		i;
    FILE	*f;

    for (i = 1; i < argc; i++)
    {
	lowercase_string( argv[ i ] );
	if (strcmp( argv[ i ], "-vga") == 0)
	    force_vga_mode = True;
    }

    f = fopen( ".apple2", "r");
    if (f == NULL)
	printf("Warning. Cannot find the .apple2 system defaults file.\n");
    else
    {
	c_system_defaults( f );
	fclose( f );
    }

    c_initialize();

    exception_flag = 1;
    exception_type = 0;

    vmode_active = False;
    c_setpage( 0 );
    c_setscreen( 0 );

    for (;;)
    {
        cpu6502();

        c_initialize_apple_ii_memory();
        c_init_6();

        vmode_text = vmode_mixed = True;
        vmode_page2 = vmode_hires = False;

        language_card_read = language_card_write = language_card_second = 0;
        language_current_bank = 0;

	exception_flag = 1;
	exception_type = 0;

        vmode_active = False;
	c_setpage( 0 );
	c_setscreen( 0 );

	c_update_video_screen();
    }

    vga_setmode(TEXT);
    keyboard_close();
    exit( 0 );
}
