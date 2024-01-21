#include <stdio.h>
#include <stdlib.h>
#include <vga.h>
#include <dirent.h>
#include <termios.h>
#include "colors.h"

static struct termio    newtermio, original;

unsigned char     	interface_font[0x800*8];

extern unsigned char    char_rom[0x800];
extern unsigned char    *GM;	     /* --- Base address of graphic area --- */

extern unsigned char	vmode_text;
extern unsigned char    vmode_mixed;
extern unsigned char	vmode_page2;
extern unsigned char    vmode_hires;
extern unsigned char	vmode_active;

extern unsigned char	file_name_6[2][1024];
extern int		compressed[2];

unsigned char		disk_path[1024] = "./disks";

extern unsigned short	apple_speed;
extern short		joy_step;
extern short		joy_center_x;
extern short		joy_center_y;
extern short		joy_mode;
extern short		color_mode;
extern short		sound_mode;

void c_update_video_screen();

typedef enum { False, True } Tr;	/* --- Domain of Truth values
					       (using the notation by
					       John Allen -
						"The Anatomy of LISP") --- */

void c_load_character( int ascii, unsigned char bitmap[8][8] )
{
    int		i, j;

    for (i = 0; i < 8; i++)
	for (j = 0; j < 7; j++)
	    interface_font[64 * ascii + i * 8 + j] = 
	        (bitmap[ i ][ j ] == '*') ? COLOR_LIGHT_WHITE : 0;
}

/* -------------------------------------------------------------------------
    c_load_interface_font()
   ------------------------------------------------------------------------- */

void c_load_interface_font()
{
    int			c, i, j, v, p;

    static unsigned char ul_corner[8][8] = { ".......",
					     ".......",
					     ".......",
					     ".......",
					     "...****",
					     "...*...",
					     "...*...",
					     "...*..." };

    static unsigned char ur_corner[8][8] = { ".......",
					     ".......",
					     ".......",
					     ".......",
					     "****...",
					     "...*...",
					     "...*...",
					     "...*..." };

    static unsigned char dl_corner[8][8] = { "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "...****",
					     ".......",
					     ".......",
					     "......." };

    static unsigned char dr_corner[8][8] = { "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "****...",
					     ".......",
					     ".......",
					     "......." };

    static unsigned char    v_line[8][8] = { "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "...*..." };

    static unsigned char    h_line[8][8] = { ".......",
					     ".......",
					     ".......",
					     ".......",
					     "*******",
					     ".......",
					     ".......",
					     "......." };

    static unsigned char   tl_junc[8][8] = { "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "...****",
					     "...*...",
					     "...*...",
					     "...*..." };

    static unsigned char   tr_junc[8][8] = { "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "****...",
					     "...*...",
					     "...*...",
					     "...*..." };

    static unsigned char   tu_junc[8][8] = { ".......",
					     ".......",
					     ".......",
					     ".......",
					     "*******",
					     "...*...",
					     "...*...",
					     "...*..." };

    static unsigned char   td_junc[8][8] = { "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "*******",
					     ".......",
					     ".......",
					     "......." };

    static unsigned char   x_junc[8][8] =  { "...*...",
					     "...*...",
					     "...*...",
					     "...*...",
					     "*******",
					     "...*...",
					     "...*...",
					     "...*..." };

    for (p = 0, c = 0; c < 256; c++)
	for (i = c * 8; i < c * 8 + 8; i++, p++)
	    for (v = char_rom[ i ] >> 2, j = 0; j < 7; j++, v >>= 1, p++)
	        interface_font[ p ] = (c < 128) ?
					  ((v & 1) ? 0 : COLOR_LIGHT_WHITE) :
					  ((v & 1) ? COLOR_LIGHT_WHITE : 0);

    for (c = 0; c < 96; c++)
	for (i = 0; i < 64; i++)
	    interface_font[ (c + 32) * 64 + i ] =
	    interface_font[ (c + 160) * 64 + i ];

    c_load_character( 0x80, ul_corner );
    c_load_character( 0x81, ur_corner );
    c_load_character( 0x82, dl_corner );
    c_load_character( 0x83, dr_corner );
    c_load_character( 0x84, v_line );
    c_load_character( 0x85, h_line );
    c_load_character( 0x86, tl_junc );
    c_load_character( 0x87, tr_junc );
    c_load_character( 0x88, tu_junc );
    c_load_character( 0x89, td_junc );
    c_load_character( 0x8A, x_junc );

    for (c = 32; c < 128; c++)
	for (i = 0; i < 64; i++)
	    interface_font[ (c + 128) * 64 + i ] =
	    (interface_font[ c * 64 + i ] == 0) ? COLOR_LIGHT_WHITE : 0;
}

/* -------------------------------------------------------------------------
    c_interface_textcolor()
   ------------------------------------------------------------------------- */

void c_interface_textcolor( short foreground, short background )
{
    static short	prev_foreground = COLOR_LIGHT_WHITE;
    static short	prev_background = 0;

    int			i;

    for (i = 0; i < 16384; i++)
    {
	if (interface_font[ i ] == prev_foreground)
	    interface_font[ i ] = foreground;
	else
	if (interface_font[ i ] == prev_background)
	    interface_font[ i ] = background;
    }

    prev_background = background;
    prev_foreground = foreground;
}

/* -------------------------------------------------------------------------
    c_interface_print_char()
   ------------------------------------------------------------------------- */

void c_interface_print_char( int x, int y, unsigned char c )
{
    unsigned char *d = GM + y * 320 * 8 + x * 7 + 1300;
    unsigned char *s = interface_font + c * 64;

    *(unsigned long *)d = *(unsigned long *)s;
    d += 4, s += 4;
    *(unsigned int *)d = *(unsigned int *)s;
    d += 2, s += 2;
    *(unsigned char *)d = *(unsigned char *)s;
    d += 314, s += 2;

    *(unsigned long *)d = *(unsigned long *)s;
    d += 4, s += 4;
    *(unsigned int *)d = *(unsigned int *)s;
    d += 2, s += 2;
    *(unsigned char *)d = *(unsigned char *)s;
    d += 314, s += 2;

    *(unsigned long *)d = *(unsigned long *)s;
    d += 4, s += 4;
    *(unsigned int *)d = *(unsigned int *)s;
    d += 2, s += 2;
    *(unsigned char *)d = *(unsigned char *)s;
    d += 314, s += 2;

    *(unsigned long *)d = *(unsigned long *)s;
    d += 4, s += 4;
    *(unsigned int *)d = *(unsigned int *)s;
    d += 2, s += 2;
    *(unsigned char *)d = *(unsigned char *)s;
    d += 314, s += 2;

    *(unsigned long *)d = *(unsigned long *)s;
    d += 4, s += 4;
    *(unsigned int *)d = *(unsigned int *)s;
    d += 2, s += 2;
    *(unsigned char *)d = *(unsigned char *)s;
    d += 314, s += 2;

    *(unsigned long *)d = *(unsigned long *)s;
    d += 4, s += 4;
    *(unsigned int *)d = *(unsigned int *)s;
    d += 2, s += 2;
    *(unsigned char *)d = *(unsigned char *)s;
    d += 314, s += 2;

    *(unsigned long *)d = *(unsigned long *)s;
    d += 4, s += 4;
    *(unsigned int *)d = *(unsigned int *)s;
    d += 2, s += 2;
    *(unsigned char *)d = *(unsigned char *)s;
    d += 314, s += 2;

    *(unsigned long *)d = *(unsigned long *)s;
    d += 4, s += 4;
    *(unsigned int *)d = *(unsigned int *)s;
    d += 2, s += 2;
    *(unsigned char *)d = *(unsigned char *)s;
}

/* -------------------------------------------------------------------------
    c_interface_print_char_inverse()
   ------------------------------------------------------------------------- */

void c_interface_print_char_inverse( int x, int y, unsigned char c )
{
    c_interface_print_char( x, y, c | 0x80 );
}

/* -------------------------------------------------------------------------
    c_interface_print()
   ------------------------------------------------------------------------- */

void c_interface_print( int x, int y, unsigned char *s )
{
    int		i;

    for (i = x; *s; i++, s++)
	c_interface_print_char( i, y, *s );
}

/* -------------------------------------------------------------------------
    c_interface_print_inverse()
   ------------------------------------------------------------------------- */

void c_interface_print_inverse( int x, int y, unsigned char *s )
{
    int		i;

    for (i = x; *s; i++, s++)
	c_interface_print_char_inverse( i, y, *s );
}

/* -------------------------------------------------------------------------
    c_interface_translate_screen()
   ------------------------------------------------------------------------- */

#define IsGraphic(c) ((c) == '|' || ((c) >= 0x80 && (c) <= 0x8A))
#define IsInside(x,y) ((x) >= 0 && (x) <= 39 && (y) >= 0 && (y) <= 23)

void c_interface_translate_screen( unsigned char screen[24][41] )
{
    static char	map[11][3][4] ={ { "...",
				   ".||",
				   ".|." },

				 { "...",
				   "||.",
				   ".|." },

				 { ".|.",
				   ".||",
				   "..." },

				 { ".|.",
				   "||.",
				   "..." },

				 { ".|.",
				   ".|.",
				   ".|." },

				 { "...",
				   "|||",
				   "..." },

				 { ".|.",
				   ".||",
				   ".|." },

				 { ".|.",
				   "||.",
				   ".|." },

				 { "...",
				   "|||",
				   ".|." },

				 { ".|.",
				   "|||",
				   "..." },

				 { ".|.",
				   "|||",
				   ".|." } };

    int		x, y, i, j, k;

    for (y = 0; y < 24; y++)
	for (x = 0; x < 40; x++)
        {
	    if (screen[ y ][ x ] == '|')
	    {
		Tr flag = False;

		for (k = 10; !flag && k >= 0; flag ? : k--)
	        {
		    flag = True;

		    for (i = y - 1; flag && i <= y + 1; i++)
		        for (j = x - 1; flag && j <= x + 1; j++)
			    if (IsInside(j, i))
				if (!(IsGraphic( screen[ i ][ j ])) &&
			            (map[k][ i - y + 1 ][ j - x + 1 ] == '|'))
			            flag = False;
				else;
			    else
				if (map[k][ i - y + 1 ][ j - x + 1 ] == '|')
				    flag = False;
		}

		if (flag)
		    screen[ y ][ x ] = 0x80 + k;
	    }
	}
}

Tr c_interface_cut_name(char *name)
{
    char *p = name + strlen(name) - 1;
    Tr	is_gz = False;

    if (p >= name && *p == 'z')
    {
	p--;
	if (p >= name && *p == 'g')
	{
	    p--;
	    if (p >= name && *p == '.')
		p--, is_gz = True;
	}
    }

    for (; *p != '.'; p--) { }

    *p = '\0';

    return is_gz;
}

void c_interface_cut_gz(char *name)
{
    char *p = name + strlen(name) - 1;

    p--; 
    p--;
    *p = '\0';
}

#define GZ_EXT ".gz"
#define GZ_EXT_LEN 3

Tr c_interface_is_gz(const char *name)
{
    size_t len = strlen( name );

    if (len > GZ_EXT_LEN) {  /* can't have a file called ".gz", can we? */
	name += len - GZ_EXT_LEN;
	return ((strcmp(name, GZ_EXT) == 0) ? True : False);
    }
    
    return False;
}

#define DISK_EXT ".dsk"
#define DISK_EXT_LEN 4

int c_interface_disk_select(const struct dirent *e)
{
    static char		cmp[ 4096 ];
    size_t len;
    const char *p;

    strcpy( cmp, disk_path );
    strcat( cmp, "/" );
    strcat( cmp, e -> d_name );

    if (strcmp( cmp, file_name_6[0] ) == 0 ||
        strcmp( cmp, file_name_6[1] ) == 0)
	return False;

    p = e->d_name;
    len = strlen(p);
    
    if (len > GZ_EXT_LEN &&
	(strcmp(p + len - GZ_EXT_LEN, GZ_EXT) == 0)) {
	len -= GZ_EXT_LEN;
    }

    return ((strncmp(p + len - DISK_EXT_LEN, DISK_EXT, DISK_EXT_LEN) == 0) ?
	    True : False);
}

void c_interface_normal_keyboard_on()
{
    ioctl(fileno(stdin), TCGETA, &original);
    newtermio = original;
    newtermio.c_iflag &= (ISTRIP|IGNBRK);
    newtermio.c_cc[VMIN] = 0;
    newtermio.c_cc[VTIME] = 0;
    newtermio.c_lflag = 0;
    ioctl(fileno(stdin), TCSETA, &newtermio);
}

void c_interface_normal_keyboard_off()
{
    ioctl(fileno(stdin), TCSETA, &original);
}

int c_mygetch()
{
    char     c;

    if (read(fileno(stdin), &c, 1) == 1)
	return c;
    else
	return -1;
}

/* -------------------------------------------------------------------------
    c_interface_exit()
   ------------------------------------------------------------------------- */

void c_interface_exit()
{
    c_interface_normal_keyboard_off();
    c_setpage( vmode_active );
    c_setscreen( vmode_page2 );
    c_update_video_screen();
}

/* -------------------------------------------------------------------------
    c_interface_info()
   ------------------------------------------------------------------------- */

void c_interface_info( char *filename )
{
    static unsigned char screen[24][41] =
      { "||||||||||||||||||||||||||||||||||||||||",
	"|              Information             |",
	"||||||||||||||||||||||||||||||||||||||||",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"||||||||||||||||||||||||||||||||||||||||",
	"|     Use arrow keys to move cursor    |",
	"|          (Press ESC to exit)         |",
	"||||||||||||||||||||||||||||||||||||||||" };

    static char 	temp[4096];
    FILE 		*f;
    int			i, j, lines;
    int			x_pos, y_pos, x_cur, y_cur;
    static char		*info_buf[4096];
    static int		info_buf_len[4096];
    char		ch;
    Tr			found;

    c_interface_translate_screen( screen );

    c_interface_textcolor( COLOR_LIGHT_RED, 0 );
    for (i = 0; i < 24; i++)
	c_interface_print( 0, i, screen[ i ] );
    c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );

    sprintf( temp, "%s/dsk.info", disk_path );

    f = fopen( temp, "r" );
    if (f == NULL)
    {
	c_interface_print( 5, 11, "Could not find file dsk.info!" );
	usleep(1500000);
	return;
    }

    found = False;
    for (;;)
    {
	if (fgets( temp, 4000, f ) == NULL)
	    break;
	if (temp[ 0 ] == '{')
	{
	    int	 r_brace;

	    for (r_brace = 1;
		 temp[ r_brace ] != '}' && temp[ r_brace ] != '\0';
		 r_brace++) { }
            if (temp[ r_brace ] == '}' &&
		strncmp( filename, temp + 1, r_brace - 1 ) == 0)
	    {
		found = True;
		break;
	    }
	}
    }

    if (!found)
    {
	fclose( f );
	c_interface_print( 9, 11, "No information found!");
	usleep(1500000);
	return;
    }

    for (lines = 0; lines < 4096; lines++)
    {
	if (fgets(temp, 4000, f) == NULL)
	    break;
        if (temp[0] == '{')
	    break;

	info_buf_len[ lines ] = strlen( temp );
	if (temp[ info_buf_len[ lines ] - 1 ] == 10)
	    temp[ info_buf_len[ lines ] - 1 ] = '\0',
	    info_buf_len[ lines ]--;

	info_buf[ lines ] = malloc( info_buf_len[ lines ] + 1 );
	
	strcpy( info_buf[ lines ], temp );
    }

    x_pos = y_pos = x_cur = y_cur = 0;

    c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );

    for (;;)
    {
	for (i = 0; i < 17; i++)
	{
	    if (y_pos + i < lines)
	        for (j = 0; j < 38; j++)
	        {
		    if (i == y_cur && j == x_cur)
    		        c_interface_textcolor( COLOR_LIGHT_GREEN, COLOR_MEDIUM_BLUE );

		    if (x_pos + j >= info_buf_len[ y_pos + i ])
		        c_interface_print_char( j + 1, i + 3, ' ' );
		    else
			c_interface_print_char( j + 1, i + 3,
				    info_buf[ y_pos + i ][ x_pos + j ] );

		    if (i == y_cur && j == x_cur)
    		        c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );
		}
	    else
		c_interface_print( 1, i + 3, "                                      ");
	}

	do
	{
	    ch = c_mygetch();
	}
	while (ch == -1);

	if (ch == 27)
	{
	    char ch = c_mygetch();

	    if (ch == '[')
	    {
		char ch = c_mygetch();

		if (ch == 'A')		/* Arrow up */
	            if (y_cur > 0)
			y_cur--;
		    else	
		    if (y_pos > 0)
			y_pos--;
		    else;
		else
		if (ch == 'B')		/* Arrow down */
		    if (y_cur < 16)
			if (y_pos + y_cur < lines - 1)
			    y_cur++;
			else;
		    else
		    if (y_pos < lines - 17)
			y_pos++;
		    else;
		else
		if (ch == 'D')		/* Arrow left */
	            if (x_cur > 0)
			x_cur--;
		    else	
		    if (x_pos > 0)
			x_pos--;
		    else;
		else
		if (ch == 'C')		/* Arrow right */
		    if (x_cur < 37)
			if (x_pos + x_cur < 4000)
			    x_cur++;
			else;
		    else
		    if (x_pos < 3962)
			x_pos++;
		    else;
		else
		if (ch == '6')
		{
		    char ch = c_mygetch();
		    if (ch == '~')	/* Page down */
		    {
			y_pos += 16;
			if (y_pos > lines - 17)
			{
			    y_cur += (y_pos - (lines - 17));
			    if (y_cur > 16)
				y_cur = 16;
			    y_pos = lines - 17;
			}
		    }
		}
		else
		if (ch == '5')
		{
		    char ch = c_mygetch();
		    if (ch == '~')	/* Page up */
		    {
			y_pos -= 16;
			if (y_pos < 0)
			{
			    y_cur += y_pos;
			    if (y_cur < 0)
				y_cur = 0;
			    y_pos = 0;
			}
		    }
		}
		else
		if (ch == '1')
		{
		    char ch = c_mygetch();
		    if (ch == '~')	/* Home */
			x_pos = y_pos = x_cur = y_cur = 0;
		}
		else
		if (ch == '4')
		{
		    char ch = c_mygetch();
		    if (ch == '~')	/* End */
			y_pos = lines - 17;
		}
	    }
	    else
	    if (ch == -1)
		return;
	}
    }

    fclose( f );
}

/* -------------------------------------------------------------------------
    c_interface_select_diskette()
   ------------------------------------------------------------------------- */

void c_interface_select_diskette( int drive )
{
    static unsigned char screen[24][41] =
      { "||||||||||||||||||||||||||||||||||||||||",
	"| Insert diskette into Drive A, Slot 6 |",
	"||||||||||||||||||||||||||||||||||||||||",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"||||||||||||||||||||||||||||||||||||||||",
	"| Use arrow keys and RETURN to select  |",
	"| Press SPACE for info (ESC = cancel)  |",
	"||||||||||||||||||||||||||||||||||||||||" };

    static char		temp[4096], cmd[4096];

    struct dirent 	**namelist;
    int		  	entries;

    int			i;
    static int		curpos = 0;

    char		ch;

    screen[ 1 ][ 29 ] = (drive == 0) ? 'A' : 'B';

    c_setpage( 0 );
    c_setscreen( 0 );

    c_interface_translate_screen( screen );

    c_interface_textcolor( COLOR_LIGHT_RED, 0 );
    for (i = 0; i < 24; i++)
	c_interface_print( 0, i, screen[ i ] );
    c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );

    entries = scandir(disk_path, &namelist, c_interface_disk_select, alphasort );

    if (entries <= 0)
    {
	c_interface_print( 5, 11, "No Apple II+ diskettes found!" );
	usleep(1500000);
	c_interface_exit();
	return;
    }

    if (curpos >= entries)
	curpos = entries - 1;
    c_interface_normal_keyboard_on();

    for (;;)
    {
        c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );
        for (i = 0; i < 17; i++)
        {
	    int		ent_no = curpos - 8 + i, slen;

            strcpy( temp, " " );
	    if (ent_no >= 0 && ent_no < entries)
	    {
	        strcpy( temp + 1, namelist[ ent_no ] -> d_name );
	        if (c_interface_cut_name( temp ))
		    strcat( temp, " <gz>");
	    }

	    slen = strlen( temp );
	    while (slen < 38)
	        temp[ slen++ ] = ' ';
	    temp[ 38 ] = '\0';

	    if (ent_no == curpos)
	    {
                c_interface_textcolor( COLOR_LIGHT_GREEN, COLOR_MEDIUM_BLUE );
	        c_interface_print( 1, i + 3, temp );
                c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );
	    }
	    else
	        c_interface_print( 1, i + 3, temp );
	}

	do
	{
	    ch = c_mygetch();
	}
	while (ch == -1);

	if (ch == 27)
	{
	    char ch = c_mygetch();

	    if (ch == '[')
	    {
		char ch = c_mygetch();

		if (ch == 'A')		/* Arrow up */
	            if (curpos > 0)
			curpos--;
		    else;
		else
		if (ch == 'B')		/* Arrow down */
		    if (curpos < entries - 1)
			curpos++;
		    else;
		else
		if (ch == '6')
		{
		    char ch = c_mygetch();
		    if (ch == '~')	/* Page down */
		    {
			curpos += 16;
			if (curpos > entries - 1)
			    curpos = entries - 1;
		    }
		}
		else
		if (ch == '5')
		{
		    char ch = c_mygetch();
		    if (ch == '~')	/* Page up */
		    {
			curpos -= 16;
			if (curpos < 0)
			    curpos = 0;
		    }
		}
		else
		if (ch == '1')
		{
		    char ch = c_mygetch();
		    if (ch == '~')	/* Home */
			curpos = 0;
		}
		else
		if (ch == '4')
		{
		    char ch = c_mygetch();
		    if (ch == '~')	/* End */
			curpos = entries - 1;
		}
	    }
	    else
	    if (ch == -1)
	    {
	        for (i = 0; i < entries; i++)
		    free(namelist[ i ]);
		c_interface_exit();
		return;
	    }
	}
	else
	if (ch == 13) /* Return */
	{
	    int		cmpr = False;

            sprintf( temp, "%s/%s", disk_path, namelist[ curpos ] -> d_name );
	    if (c_interface_is_gz( temp ))
	    {
		c_interface_print( 1, 21,
			"            Uncompressing...          " );
	        c_interface_print( 1, 22,
			"                                      " );

		sprintf(cmd, "gunzip '%s'", temp );
		system(cmd);
		c_interface_cut_gz( temp );
		cmpr = True;
	    }
	    if (compressed[ drive ])
	    {
		c_interface_print( 1, 21,
			"      Compressing old diskette...     " );
	        c_interface_print( 1, 22,
			"                                      " );
	    }
	    c_new_diskette_6( drive, temp, cmpr );
	    for (i = 0; i < entries; i++)
		free(namelist[ i ]);
	    c_interface_exit();
	    return;
	}
	else
	if (ch == ' ')
	{
	     strcpy( temp, namelist[ curpos ] -> d_name );
	     c_interface_cut_name( temp );
	     c_interface_info( temp );

             c_interface_textcolor( COLOR_LIGHT_RED, 0 );
             for (i = 0; i < 24; i++)
	         c_interface_print( 0, i, screen[ i ] );
	}
    }
}

/* -------------------------------------------------------------------------
    c_interface_parameters()
   ------------------------------------------------------------------------- */

void c_interface_parameters()
{
    static unsigned char screen[24][41] =
      { "||||||||||||||||||||||||||||||||||||||||",
	"|     Apple II+ Emulator for Linux     |",
	"|   by Alexander Jean-Claude Bottema   |",
	"||||||||||||||||||||||||||||||||||||||||",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"||||||||||||||||||||||||||||||||||||||||",
	"| F1 : Drive A, Slot 6  F9 : Max speed |",
	"| F2 : Drive B, Slot 6       (On/Off)  |",
	"| F4 : Pause         Pause : Reboot    |",
	"| F5 : Keyb. layout  Pr.Scr: Reset     |",
	"| F8 : Words from the author           |",
	"| F10: This menu                       |",
	"||||||||||||||||||||||||||||||||||||||||",
	"| Use arrow keys (or Return) to modify |",
	"| parameters. (Press ESC to exit menu) |",
	"||||||||||||||||||||||||||||||||||||||||" };

    static char		temp[4096], cmd[4096];
    static char		*options[9] =
		{ " Speed    : ",
                  " Path     : ",
		  " Color    : ",
		  " Sound    : ",
		  " Joystick : ",
		  " Origin X : ",
		  " Origin Y : ",
		  " Sens.    : ", 
		  " Quit       " };

    int			i, j;
    char		ch;
    static int		option = 0;
    static int		cur_x = 0, cur_pos = 0;

    c_setpage( 0 );
    c_setscreen( 0 );

    c_interface_translate_screen( screen );

    c_interface_textcolor( COLOR_LIGHT_RED, 0 );
    for (i = 0; i < 24; i++)
	c_interface_print( 0, i, screen[ i ] );
    c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );

    c_interface_normal_keyboard_on();

    for (;;)
    {
	for (i = 0; i < 9; i++)
	{
	    if (option == i)
		c_interface_textcolor( COLOR_LIGHT_GREEN,
				       COLOR_MEDIUM_BLUE );
	    c_interface_print( 1, 4 + i, options[ i ] );
	    if (option == i)
		c_interface_textcolor( COLOR_LIGHT_GREEN,
				       0 );
	    switch (i)
	    {
	    case 0:
		sprintf(temp, "%03d%%", 101 - apple_speed );
		break;
	    case 1:
		strncpy( temp, disk_path + cur_pos, 24 );
		temp[24] = '\0';
		break;
	    case 2:
		sprintf(temp, "%s", (color_mode == 0) ? "Off         " :
				    (color_mode == 1) ? "On          " :
				                        "Interpolated");
		break;
	    case 3:
		sprintf(temp, "%s", (sound_mode == 0) ? "Off       " :
				                        "PC speaker");
		break;
	    case 4:
		sprintf(temp, "%s", (joy_mode == 0) ? "Linear " :
				    (joy_mode == 1) ? "Digital" :
                                                      "Off    ");
		break;
	    case 5:
		sprintf(temp, "%03d", joy_center_x);
		break;
	    case 6:
		sprintf(temp, "%03d", joy_center_y);
		break;
	    case 7:
		sprintf(temp, "%03d%%", joy_step );
		break;
	    case 8:
		strcpy( temp, "" );
		break;
	    default:
		break;
	    }

	    if (i != 1)
	        c_interface_print( 14, 4 + i, temp );
	    else
	    {
	        int	j;

		for (j = 0; j < 24; j++)
		    if (cur_x != j)
		    {
			if (temp[ j ] == '\0')
			{
		            c_interface_print_char( 14 + j, 5, ' ' );
			    j++;
			    break;
			}
			else
		            c_interface_print_char( 14 + j, 5, temp[ j ] );
		    }
		    else
		    {
			if (option == 1)
		            c_interface_textcolor( COLOR_LIGHT_GREEN,
				                   COLOR_MEDIUM_BLUE );
			if (temp[ j ] == '\0')
			{
		            c_interface_print_char( 14 + j, 5, ' ' );
			    if (option == 1)
		                c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );
			    j++;
			    break;
			}
			else
		            c_interface_print_char( 14 + j, 5, temp[ j ] );
			if (option == 1)
		            c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );
		    }
		for (; j < 24; j++)
		    c_interface_print_char( 14 + j, 5, ' ' );
	     }
        }

	do
	{
	    ch = c_mygetch();
	}
	while (ch == -1);

	if (ch == 27)
	{
	    char ch = c_mygetch();

	    if (ch == '[')
	    {
		char ch = c_mygetch();

		if (ch == 'A')		/* Arrow up */
	            if (option > 0)
			option--;
		    else
			option = 8;
		else
		if (ch == 'B')		/* Arrow down */
		    if (option < 8)
			option++;
		    else
			option = 0;
		if (ch == 'D')		/* Arrow left */
		{
		    switch (option)
		    {
		    case 0:
			if (apple_speed < 100)
			    apple_speed++;
			break;
		    case 1:
			if (cur_x > 0)
			    cur_x--;
			else
			if (cur_pos > 0)
			    cur_pos--;
			break;
		    case 2:
			color_mode--;
			if (color_mode < 0)
			    color_mode = 2;
			break;
		    case 3:
			sound_mode--;
			if (sound_mode < 0)
			    sound_mode = 1;
			break;
		    case 4:
			joy_mode--;
			if (joy_mode < 0)
			    joy_mode = 2;
			break;
		    case 5:
			if (joy_center_x > 0)
			    joy_center_x--;
			break;
		    case 6:
			if (joy_center_y > 0)
			    joy_center_y--;
			break;
		    case 7:
			if (joy_step > 1)
			    joy_step--;
			break;
		    }
		}
		else
		if (ch == 'C')		/* Arrow right */
		{
		    switch (option)
		    {
		    case 0:
			if (apple_speed > 1)
			    apple_speed--;
			break;
		    case 1:
			if (cur_x < 23)
			{
			    if (disk_path[cur_pos + cur_x] != '\0')
			        cur_x++;
			}
			else
			if (disk_path[cur_pos + cur_x] != '\0')
			    cur_pos++;
			break;
		    case 2:
			color_mode++;
			if (color_mode > 2)
			    color_mode = 0;
			break;
		    case 3:
			sound_mode++;
			if (sound_mode > 1)
			    sound_mode = 0;
			break;
		    case 4:
			joy_mode++;
			if (joy_mode > 2)
			    joy_mode = 0;
			break;
		    case 5:
			if (joy_center_x < 255)
			    joy_center_x++;
			break;
		    case 6:
			if (joy_center_y < 255)
			    joy_center_y++;
			break;
		    case 7:
			if (joy_step < 100)
			    joy_step++;
			break;
		    }
		}
		else;
	    }
	    else
	    if (ch == -1)
	    {
		c_initialize_sound();
		c_initialize_highres_values();
	        c_interface_exit();
	        return;
            }
	}
	else
	{
	    if (ch >= ' ' && ch < 127 && option == 1)
	    {
		int		i;

		strcpy(temp, disk_path);		
		for (i = strlen(temp); i >= cur_pos + cur_x; i--)
		    temp[ i + 1 ] = temp[ i ];
		temp[ cur_pos + cur_x ] = ch;
		strcpy(disk_path, temp);
		if (cur_x < 23)
		    cur_x++;
		else
		if (disk_path[cur_pos + cur_x] != '\0')
		    cur_pos++;
	    }

	    if ((ch == 127 || ch == 8) && cur_pos + cur_x - 1 >= 0) /* Backspace */
	    {
	        int		i;

		for (i = cur_pos + cur_x - 1; disk_path[ i ] != '\0'; i++)
		    disk_path[ i ] = disk_path[ i + 1 ];

		if (cur_x > 0)
		    cur_x--;
		else
		if (cur_pos > 0)
		    cur_pos--;
	    }

	    if (ch == 13 && option == 8)
	    {
		char ch;

		c_interface_print( 1, 22, "          Are you sure? (Y/N)         " );
		while ((ch = c_mygetch()) == -1) { }
		ch = toupper(ch);
		if (ch == 'Y')
		{
                    c_new_diskette_6( 0, "", False );
                    c_new_diskette_6( 1, "", False );
                    c_interface_normal_keyboard_off();
		    c_mouse_close();
		    vga_setmode(TEXT);
		    printf("Linux! ...and there were much rejoicing! oyeeeeh...\n");
		    exit( 0 );
		}
                c_interface_textcolor( COLOR_LIGHT_RED, 0 );
	        c_interface_print( 0, 22, screen[ 22 ] );
                c_interface_textcolor( COLOR_LIGHT_GREEN, 0 );
	    }
	}
    }
}

/* -------------------------------------------------------------------------
    c_interface_words()
   ------------------------------------------------------------------------- */

void c_interface_words()
{
    static unsigned char screen[24][41] =
      { "||||||||||||||||||||||||||||||||||||||||",
	"|    Apple II+ Emulator Version 0.01   |",
	"||||||||||||||||||||||||||||||||||||||||",
	"| If you have problems with your       |",
	"| keyboard concerning the mapping of   |",
	"| various keys, please let me know.    |",
	"| I use a Swedish keyboard for myself  |",
	"| and the scancodes may differ from US |",
	"| keyboards (or other countries as     |",
	"| well). Currently, my email address   |",
	"| is: d91a1bo@meryl.csd.uu.se. This    |",
	"| address is valid at least one more   |",
	"| year, i.e. as long as I am Computer  |",
	"| Science student at the University    |",
	"| of Uppsala. \"...and there were much  |",
	"| rejoicing! oyeeeeeh\"                 |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|                                      |",
	"|              / Alexander  Oct 9 1994 |",
	"||||||||||||||||||||||||||||||||||||||||",
	"|       (Press any key to exit)        |",
	"||||||||||||||||||||||||||||||||||||||||" };

    int		i;

    c_setpage( 0 );
    c_setscreen( 0 );

    c_interface_translate_screen( screen );

    c_interface_textcolor( COLOR_LIGHT_RED, 0 );
    for (i = 0; i < 24; i++)
	c_interface_print( 0, i, screen[ i ] );

    c_interface_normal_keyboard_on();
    while (c_mygetch() == -1) { }
    c_interface_exit();
}

/* -------------------------------------------------------------------------
    c_interface_keyboard_layout()
   ------------------------------------------------------------------------- */

void c_interface_keyboard_layout()
{
    static unsigned char screen[24][41] =
      { "||||||||||||||||||||||||||||||||||||||||",
	"|     Apple II+ US Keyboard Layout     |",
	"||||||||||||||||||||||||||||||||||||||||",
	"| Keyboard:                            |",
	"|                                      |",
	"| 1! 2\" 3# 4$ 5% 6& 7' 8( 9) 0 :* -=   |",
	"|  Q  W  E  R  T  Y  U  I  O  P Rep CR |",
	"|   A  S  D  F  G  H  J  K  L  ;+ <- ->|",
	"|    Z  X  C  V  B  N  M ,< .> /?      |",
	"| Where <- -> are the left and right   |",
	"| arrow keys respectively. Rep is the  |",
	"| \"repeat\" key.                        |",
	"||||||||||||||||||||||||||||||||||||||||",
	"| Joystick emulation on numeric keypad |",
	"|                                      |",
	"| 7  8  9  for various directions.     |",
	"| 4     6  Press 5 to center linear    |",
	"| 1  2  3  joystick.                   |",
	"|                                      |",
	"| Delete, End and Page Down emulates   |",
	"| pushbuttons 0, 1 and 2 respectively. |",
	"||||||||||||||||||||||||||||||||||||||||",
	"|       (Press any key to exit)        |",
	"||||||||||||||||||||||||||||||||||||||||" };

    int		i;

    c_setpage( 0 );
    c_setscreen( 0 );

    c_interface_translate_screen( screen );

    c_interface_textcolor( COLOR_LIGHT_RED, 0 );
    for (i = 0; i < 24; i++)
	c_interface_print( 0, i, screen[ i ] );

    c_interface_normal_keyboard_on();
    while (c_mygetch() == -1) { }
    c_interface_exit();
}


