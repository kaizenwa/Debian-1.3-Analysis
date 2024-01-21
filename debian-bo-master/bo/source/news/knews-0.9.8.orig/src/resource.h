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

extern void      res_initialize(void);
extern int       res_load(char*);
extern void      res_enter_group(char*);
extern void      res_set_pw_name(char*);

extern char	*res_newsrc_file(void);
extern char	*res_old_newsrc_file(void);
extern char	*res_kill_file(void);
extern char	*res_remote_newsrc_file(void);
extern char	*res_remote_kill_file(void);
extern char	*res_auto_subscribe(void);
extern char	*res_auth_info_user(void);
extern char	*res_auth_info_pass(void);
extern char	*res_posting_agent(void);
extern char	*res_cache_dir(void);
extern char	*res_descriptions_file(void);
extern char    **res_thread_ahead_groups(void);
extern long	 res_rescan_timeout(void);
extern int	 res_retrieve_descriptions(void);
extern int	 res_check_for_new_groups(void);
extern int	 res_read_active_file(void);
extern int	 res_ask_how_many(void);
extern int       res_fill_newsrc_file(void);
extern int       res_try_list_active(void);
extern int	 res_save_thread_info(void);
extern int	 res_group_name_columns(void);

extern char    **res_header_format(void);
extern regex_t	*res_quote_regexp(void);
extern char	*res_quote_string(void);
extern char	*res_quote_quote_string(void);
extern char	*res_attribution(void);
extern char	*res_full_name(void);
extern char	*res_signature_file(void);
extern char	*res_organization(void);
extern char	*res_reply_to(void);
extern char	*res_extra_headers(void);
extern char	*res_followup_headers(void);
extern char	*res_uu_dir(void);
extern char	*res_uu_program(void);
extern char	*res_distribution(void);
extern char	*res_sort_threads(void);
extern char	*res_default_charset(void);
extern char	*res_posted_and_mailed(void);
extern int	 res_full_header(void);
extern int	 res_process_xrefs(void);
extern int	 res_show_number_lines(void);
extern int	 res_keep_thread_info(int);
extern int	 res_expire_kills(void);
extern int	 res_assemble_partials(void);
extern int	 res_cache_ahead_size(void);
extern int	 res_cache_trail_size(void);
extern int	 res_subject_columns(void);

extern void      res_set_ask_how_many(int);
extern void	 res_set_full_header(int);
extern void	 res_set_keep_thread_info(int);
extern void	 res_set_default_charset(char*);
