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
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#define PARSEDATE_ERROR ((time_t)(-1))

extern time_t		 parsedate(char *);
extern char		*time_t_to_date(time_t, char*);
extern char		*eat_re(char*);
extern const char	*parse_author(const char*, long*);

extern const char	month_names[];

typedef struct MimeArg {
    char	*name;
    char	*value;
} MimeArg;

typedef enum {
    MimeEncNone, /* 7bit, 8bit and binary */
    MimeEncBase64,
    MimeEncQP,
    MimeEncUue
} MimeEnc;

extern int	 parse_content_enc(char**);
extern int	 parse_content_type(char**, char*, int, char*, int,
				    MimeArg*, int, int);
extern char	*get_charset(MimeArg*);
extern char	*parse_content_disp(char**);

typedef struct {
    char	*word;
    char	*end;
    short	len;
    short	ch_len;
    short	is_qp;
} EncWordData;

extern char	*next_enc_word(char*, EncWordData*);
extern void	 decode_rfc1522(char*, const char*);
