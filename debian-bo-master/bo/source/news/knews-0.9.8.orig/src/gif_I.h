/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USER'S OWN RISK.
 */
#define NEXT_BYTE(g)	((g)->src_len--, *(g)->src_buf++)
#define GET_BYTE(g)	((g)->sp == 0 ? get_byte(g) : (g)->stack[--(g)->sp])

#define STACK_SIZE	1024
#define TABLE_SIZE	4096

#define NULL_CODE	0xffffu

struct gif_info {
    const unsigned char	*src_buf;
    long		 src_len;
    char		*err_str;
    /**/
    unsigned short	 width;
    unsigned short	 height;
    unsigned short	 cmap_size;
    unsigned char	 interlaced;
    CMAP_ENTRY		 cmap[256];
    /**/
    unsigned int	 min_code_size;
    unsigned int	 clear_code;
    unsigned int	 end_code;
    unsigned int	 code_size;
    unsigned int	 code_mask;
    unsigned int	 prev_code;
    unsigned int	 table_size;
    struct {
	unsigned short	prefix;
	unsigned short	suffix;
    }			 table[TABLE_SIZE];
    unsigned int	 sp;
    unsigned char	 stack[STACK_SIZE];
    const unsigned char	 *buf;
    unsigned int	 n_buf;
    unsigned long	 bits;
    unsigned int	 n_bits;
};

extern int	gif_read_header(struct gif_info*);
extern long	gif_read_image(struct gif_info*, unsigned char*);
extern void	gif_init(struct gif_info*, const unsigned char*, long);
