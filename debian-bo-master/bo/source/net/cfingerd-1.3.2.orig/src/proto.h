/*
 * CFINGERD
 * Prototypes
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

void check_options(int, char *argv[]);
void start_cfingerd(void);
void show_version_info(void);
int send_ident(unsigned short, unsigned short);
int get_ident(int, char **);
char *process_ident(void);
void show_auth_info(void);
void check_stats(void);
int process_username(char *);
void strmcpy(char **, const char *);
void read_configuration(void);
void handle_internal(char *);
void start_handler(void);
void show_top(void);
void handle_standard(char *);
void display_file(FILE *);
BOOL check_exist(char *);
BOOL exist(char *);
void show_notexist(void);
void show_bottom(void);
char *inettos(long addr);
char *get_localhost(void);
void handle_userlist(void);
void handle_fakeuser(char *);
BOOL check_trusted(char *);
void check_rejected(char *);
void show_search(char *);
void become_nobody(void);
void become_user(char *);
int wildmat(char *, char *);
char *get_rfc1413_data(void);
void check_unknown(char *);
void log(int, char *, char *);
void userlog(uid_t, gid_t, char *, char *);
void check_blank_configurations(void);
int search_fake_pos(char *);
void safe_exec(char *);
void open_initial_files(void);
