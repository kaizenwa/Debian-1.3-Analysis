#include <vga.h>
#include <signal.h>
#include <sys/time.h>
#include <vgamouse.h>
#include <vgakeyboard.h>
#include "keys.h"
#include "colors.h"

#define	 TIMER_DELAY		30000L

typedef enum { False, True } Tr;	/* --- Domain of Truth values
					       (using the notation by
					       John Allen -
						"The Anatomy of LISP") --- */

#define	K *1024

extern unsigned char apple_ii_64k[64 K];

int flash_cnt = 0;

unsigned char		lock_menu_mode = False;

#define MAX_KEY_BUF_LEN	1 K

#define MenuKeyBufEmptyP() (menu_key_buf_ptr == 0)

int 			menu_key_buf[MAX_KEY_BUF_LEN];
int			menu_key_buf_ptr = 0;

/* ----------------------------------------------------
    Keymap. Mapping scancodes to Apple II+ US Keyboard
   ---------------------------------------------------- */

int apple_ii_keymap_plain[128] =
    { -1 , 27 , '1', '2', '3', '4', '5', '6',	/* 00-07   */
      '7', '8', '9', '0', ':', '-', 8  , 27 ,   /* 08-15   */
      'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I',   /* 16-23   */
      'O', 'P', REP, 8  , 13 , -1 , 'A', 'S',   /* 24-31   */
      'D', 'F', 'G', 'H', 'J', 'K', 'L', ';',   /* 32-39   */
      8  , -1 , -1 , 21 , 'Z', 'X', 'C', 'V',   /* 40-47   */
      'B', 'N', 'M', ',', '.', '/', -1 , -1 ,   /* 48-55   */
      -1 , ' ', -1 , F1 , F2 , F3 , F4 , F5 ,   /* 56-63   */
      F6 , F7 , F8 , F9 , F10, -1 , -1 , JUL,   /* 64-71   */
      J_U, JUR, S_D, J_L, J_C, J_R, S_I, JDL,   /* 72-79   */
      J_D, JDR, -1 , -1 , -1 , F11, F12, -1 ,   /* 80-87   */
      -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,   /* 88-95   */
      -1 , -1 , -1 , RST, -1 , -1 , -1 , -1 ,   /* 96-103  */
      -1 , 8  , 21 , JB1, -1 , JB2, -1 , JB0,   /* 104-111 */
      -1 , -1 , -1 , -1 , -1 , -1 , -1 , BOT,   /* 112-119 */
      -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 };  /* 120-127 */

int apple_ii_keymap_ctrl[128] =
    { -1 , 027, '1', '2', '3', '4', '5', '6',	/* 00-07   */
      '7', '8', '9', '0', ':', '-', 8  , 27 ,   /* 08-15   */
      17 , 23 , 5  , 18 , 20 , 25 , 21 , 9  ,   /* 16-23   */
      15 , 16 , REP, 8  , 13 , -1 , 1  , 19 ,   /* 24-31   */
      4  , 6  , 7  , 8  , 10 , 11 , 12 , ';',   /* 32-39   */
      8  , -1 , -1 , 21 , 26 , 24 , 3  , 22 ,   /* 40-47   */
      2  , 14 , 13 , ',', '.', '/', -1 , -1 ,   /* 48-55   */
      -1 , ' ', -1 , F1 , F2 , F3 , F4 , F5 ,   /* 56-63   */
      F6 , F7 , F8 , F9 , F10, -1 , -1 , JUL,   /* 64-71   */
      J_U, JUR, S_D, J_L, J_C, J_R, S_I, JDL,   /* 72-79   */
      J_D, JDR, -1 , -1 , -1 , F11, F12, -1 ,   /* 80-87   */
      -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,   /* 88-95   */
      -1 , -1 , -1 , RST, -1 , -1 , -1 , -1 ,   /* 96-103  */
      -1 , 8  , 21 , JB1, -1 , JB2, -1 , JB0,   /* 104-111 */
      -1 , -1 , -1 , -1 , -1 , -1 , -1 , BOT,   /* 112-119 */
      -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 };  /* 120-127 */

int apple_ii_keymap_shifted[128] =
    { -1 , 27 , '!', '"', '#', '$', '%', '&',	/* 00-07   */
      39 , '(', ')', '0', '*', '=', 8  , 27 ,   /* 08-15   */
      'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I',   /* 16-23   */
      'O', 'P', REP, 8  , 13 , -1 , 'A', 'S',   /* 24-31   */
      'D', 'F', 'G', 'H', 'J', 'K', 'L', '+',   /* 32-39   */
      8  , -1 , -1 , 21 , 'Z', 'X', 'C', 'V',   /* 40-47   */
      'B', 'N', 'M', '<', '>', '?', -1 , -1 ,   /* 48-55   */
      -1 , ' ', -1 , F1 , F2 , F3 , F4 , F5 ,   /* 56-63   */
      F6 , F7 , F8 , F9 , F10, -1 , -1 , JUL,   /* 64-71   */
      J_U, JUR, S_D, J_L, J_C, J_R, S_I, JDL,   /* 72-79   */
      J_D, JDR, -1 , -1 , -1 , F11, F12, -1 ,   /* 80-87   */
      -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,   /* 88-95   */
      -1 , -1 , -1 , RST, -1 , -1 , -1 , -1 ,   /* 96-103  */
      -1 , 8  , 21 , JB1, -1 , JB2, -1 , JB0,   /* 104-111 */
      -1 , -1 , -1 , -1 , -1 , -1 , -1 , BOT,   /* 112-119 */
      -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 };  /* 120-127 */

extern unsigned char	vmode_page2;
extern unsigned char	vmode_active;

extern unsigned char	exception_flag;
extern unsigned char	exception_type;

extern unsigned short	apple_speed;
unsigned short		max_speed = 0;

short		joy_step = 2;
short		joy_x = 127;
short		joy_y = 127;
short		joy_center_x = 127;
short		joy_center_y = 127;
unsigned char	joy_button0 = 0;
unsigned char	joy_button1 = 0;
unsigned char	joy_button2 = 0;
short   	joy_mode = 0;

char		key_pressed[ 256 ];

void c_read_raw_keyboard();

void c_wait_menu_buf_empty()
{
   while (menu_key_buf_ptr > 0) { }
}

void c_lock_menu_mode_on()
{
    lock_menu_mode = True;
}

void c_lock_menu_mode_off()
{
    lock_menu_mode = False;
}

void c_safety_quit()
{
    vga_setmode(TEXT);
    keyboard_close();
    exit( 0 );
}

void c_keyboard_off()
{
    static struct itimerval tval, old;

    tval.it_interval.tv_sec = 0L;
    tval.it_interval.tv_usec = 0L;
    tval.it_value.tv_sec = 0L;
    tval.it_value.tv_usec = 0L;

    setitimer( ITIMER_VIRTUAL, &tval, &old );
}

void c_keyboard_on()
{
    static struct itimerval tval, old;

    tval.it_interval.tv_sec = 0L;
    tval.it_interval.tv_usec = TIMER_DELAY;
    tval.it_value.tv_sec = 0L;
    tval.it_value.tv_usec = TIMER_DELAY;

    setitimer( ITIMER_VIRTUAL, &tval, &old );
    keyboard_seteventhandler( c_read_raw_keyboard );
}

void c_update_keyboard()
{
    static int	mouse_dx = 0, mouse_dy = 0, mouse_cnt = 0;
    static int	mouse_buttons_used;

    signal( SIGVTALRM, c_update_keyboard );

    keyboard_update();

    if (!lock_menu_mode && !MenuKeyBufEmptyP())
    {
        int	key = menu_key_buf[--menu_key_buf_ptr];
	menu_key_buf_ptr = 0;
        c_lock_menu_mode_on();
	switch (key)
        {
	case F1:
	    keyboard_close();
	    c_keyboard_off();
	    c_interface_select_diskette( 0 );
            if (keyboard_init())
            {
    	        printf("Error: Could not switch to RAW keyboard mode.\n");
	        exit(-1);
            }
	    c_keyboard_on();
	    break;
	case F2:
	    keyboard_close();
	    c_keyboard_off();
	    c_interface_select_diskette( 1 );
            if (keyboard_init())
            {
    	        printf("Error: Could not switch to RAW keyboard mode.\n");
	        exit(-1);
            }
	    c_keyboard_on();
	    break;
	case F4:
	    keyboard_close();
	    c_keyboard_off();
	    vga_getch();
            c_setpage( vmode_active );
            c_setscreen( vmode_page2 );
            c_update_video_screen();
            if (keyboard_init())
            {
    	        printf("Error: Could not switch to RAW keyboard mode.\n");
	        exit(-1);
            }
	    c_keyboard_on();
	    break;
	case F5:
	    keyboard_close();
	    c_keyboard_off();
	    c_interface_keyboard_layout();
            if (keyboard_init())
            {
    	        printf("Error: Could not switch to RAW keyboard mode.\n");
	        exit(-1);
            }
	    c_keyboard_on();
	    break;
	case F8:
	    keyboard_close();
	    c_keyboard_off();
	    c_interface_words();
            if (keyboard_init())
            {
    	        printf("Error: Could not switch to RAW keyboard mode.\n");
	        exit(-1);
            }
	    c_keyboard_on();
	    break;
	case F9:
	    if (max_speed != 0)
	        apple_speed = max_speed, max_speed = 0;
	    else
	        max_speed = apple_speed, apple_speed = 1;
	    break;
	case F10:
	    if (max_speed != 0)
		apple_speed = max_speed, max_speed = 0;
	    keyboard_close();
	    c_keyboard_off();
	    c_interface_parameters();
            if (keyboard_init())
            {
    	        printf("Error: Could not switch to RAW keyboard mode.\n");
	        exit(-1);
            }
	    c_keyboard_on();
	    break;
	}
        c_lock_menu_mode_off();
    }

#ifdef MOUSE_EMULATION
    mouse_update();

    mouse_dx = mouse_getx();
    mouse_dy = mouse_gety();
    if (mouse_dx != 0 || mouse_dy != 0)
    {
        mouse_setposition( 0, 0 );
	mouse_cnt = 4;

	joy_x = joy_center_x + mouse_dx * 8;
	if (joy_x < 0)
	    joy_x = 0;
	else
	if (joy_x > 255)
	    joy_x = 255;
	joy_y = joy_center_y + mouse_dy * 8;
	if (joy_y < 0)
	    joy_y = 0;
	else
	if (joy_y > 255)
	    joy_y = 255;
    }

    if (--mouse_cnt == 0)
	joy_x = joy_center_x, joy_y = joy_center_y;

    {
	int mouse_buttons = mouse_getbutton();
	if (mouse_buttons & MOUSE_LEFTBUTTON)
	    joy_button0 = 0x80, mouse_buttons_used |= MOUSE_LEFTBUTTON;
	else
	if ((mouse_buttons_used & MOUSE_LEFTBUTTON) && joy_button0 == 0x80)
	    joy_button0 = 0, mouse_buttons_used &= (~MOUSE_LEFTBUTTON);

	if (mouse_buttons & MOUSE_RIGHTBUTTON)
	    joy_button1 = 0x80, mouse_buttons_used |= MOUSE_RIGHTBUTTON;
	else
	if ((mouse_buttons_used & MOUSE_RIGHTBUTTON) && joy_button1 == 0x80)
	    joy_button1 = 0, mouse_buttons_used &= (~MOUSE_RIGHTBUTTON);

        if (mouse_buttons & MOUSE_MIDDLEBUTTON)
	    joy_button2 = 0x80, mouse_buttons_used |= MOUSE_MIDDLEBUTTON;
	else
	if ((mouse_buttons_used & MOUSE_MIDDLEBUTTON) && joy_button2 == 0x80)
	    joy_button2 = 0, mouse_buttons_used &= (~MOUSE_MIDDLEBUTTON);
    }

#endif
    flash_cnt += (int)TIMER_DELAY;
    if (flash_cnt > 333333)
    {
	flash_cnt = 0;
	vga_setpalette( COLOR_FLASHING_BLACK, 0, 0, 0 );
	vga_setpalette( COLOR_FLASHING_WHITE, 63, 63, 63 );
    }

    if (flash_cnt == (200000 / TIMER_DELAY) * TIMER_DELAY)
    {
	vga_setpalette( COLOR_FLASHING_BLACK, 63, 63, 63 );
	vga_setpalette( COLOR_FLASHING_WHITE, 0, 0, 0 );	
    }

    if (key_pressed[ SCODE_J_U ])
    {
        if (joy_mode == 0)
	    if (joy_y > joy_step)
	        joy_y -= joy_step;
	    else
	        joy_y = 0;
	else
	    joy_y = 0;
    }

    if (key_pressed[ SCODE_J_D ])
    {
        if (joy_mode == 0)
	    if (joy_y < 255 - joy_step)
	        joy_y += joy_step;
	    else
	        joy_y = 255;
	else
	    joy_y = 255;
    }

    if (key_pressed[ SCODE_J_L ])
    {
        if (joy_mode == 0)
	    if (joy_x > joy_step)
	        joy_x -= joy_step;
	    else
	        joy_x = 0;
	else
	    joy_x = 0;
    }

    if (key_pressed[ SCODE_J_R ])
    {
        if (joy_mode == 0)
	    if (joy_x < 255 - joy_step)
	        joy_x += joy_step;
	    else
	        joy_x = 255;
	else
	    joy_x = 255;
    }

    if (joy_mode == 2)
        joy_x = joy_y = 256;
}

void c_read_raw_keyboard( int scancode, int pressed )
{
    static char		file_name[1024];
    int			*keymap = apple_ii_keymap_plain;

    if (key_pressed[ SCODE_L_CTRL ] ||
	key_pressed[ SCODE_R_CTRL ])
	keymap = apple_ii_keymap_ctrl;
    else
    if (key_pressed[ SCODE_L_SHIFT ] ||
	key_pressed[ SCODE_R_SHIFT ])
        keymap = apple_ii_keymap_shifted;

    if (pressed)
    {
	if (!key_pressed[ scancode ])
	{
	    key_pressed[ scancode ] = True;
	    if ((unsigned int)(keymap[ scancode ]) < 255)
	    {
	        apple_ii_64k[ 0xC000 ] = keymap[ scancode ] | 0x80;
		return;
	    }
	}

	switch (keymap[ scancode ])
	{
	case RST:
	    exception_flag = 1;
	    exception_type = 0;
	    break;
	case BOT:
	    exception_flag = 1;
	    exception_type = 1;
	    break;
	case J_C:
	    joy_x = joy_center_x;
	    joy_y = joy_center_y;
	    break;
	case JB0:
	    joy_button0 = 0x80;
	    break;
	case JB1:
	    joy_button1 = 0x80;
	    break;
	case JB2:
	    joy_button2 = 0x80;
	    break;
	default:
	    break;
	}

	if (keymap[ scancode ] >= F1 &&
	    keymap[ scancode ] <= F10 &&
	    menu_key_buf_ptr < MAX_KEY_BUF_LEN)
	        menu_key_buf[ menu_key_buf_ptr++ ] = keymap[ scancode ];

    }
    else
    {
        key_pressed[ scancode ] = False;
	switch (keymap[ scancode ])
	{
	case JB0:
	    joy_button0 = 0x00;
	    break;
	case JB1:
	    joy_button1 = 0x00;
	    break;
	case JB2:
	    joy_button2 = 0x00;
	    break;
	}
    }
}


/* -------------------------------------------------------------------------
    void c_read_keyboard_strobe()
   ------------------------------------------------------------------------- */

void c_read_keyboard_strobe()
{
    if (key_pressed[ SCODE_REP ])
	return;

    apple_ii_64k[ 0xC000 ] &= ~0x80;
}

/* -------------------------------------------------------------------------
    void c_initialize_keyboard()
   ------------------------------------------------------------------------- */

void c_initialize_keyboard()
{
    if (keyboard_init())
    {
    	printf("Error: Could not switch to RAW keyboard mode.\n");
	exit(-1);
    }

    signal( SIGVTALRM, c_update_keyboard );
    keyboard_seteventhandler( c_read_raw_keyboard );
    signal( SIGALRM, c_safety_quit );

    keyboard_translatekeys(DONT_CATCH_CTRLC);
/*    alarm( 120 ); */

    c_keyboard_on();
}

void c_initialize_mouse()
{
#ifdef MOUSE_EMULATION
    mouse_init( "/dev/mouse", MOUSE_MICROSOFT, 1200 );
    mouse_setdefaulteventhandler();
    mouse_setposition( 0, 0 );
    mouse_setxrange( -32, 32 );
    mouse_setyrange( -32, 32 );
#endif
}

void c_mouse_close()
{
#ifdef MOUSE_EMULATION
    mouse_close();
#endif
}
