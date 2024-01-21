/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* Prototypes for functions that appear in various modules. Gathered
together to avoid having a lot of tiddly little headers with only a
couple of lines in them. */

extern void  accept_add_recipient(char *);
extern void  accept_free_recipients(void);
extern BOOL  accept_msg(FILE *, FILE *, BOOL);

extern int   child_close(pid_t, int);
extern pid_t child_open(char **, char **, int, uid_t *, gid_t *, int *, int);

extern void  daemon_go(void);
extern void  debug_printf(char *, ...);
extern address_item *deliver_make_addr(char *);
extern int   deliver_message(char *, BOOL, BOOL, BOOL);
extern void  deliver_set_expansions(address_item *);
extern void  deliver_setlocal(address_item *);
extern void  deliver_succeeded(address_item *);
extern int   direct_address(address_item *, address_item **, address_item **,
               address_item **, address_item **, int);
extern BOOL  direct_findgroup(char *, gid_t *);
extern BOOL  direct_finduser(char *, struct passwd **, uid_t *);
extern void  direct_find_expanded_group(char *, char *, char *, gid_t *);
extern void  direct_find_expanded_user(char *, char *, char *,
               struct passwd **, uid_t *);
extern void  direct_init(void);
extern void  direct_tidyup(void);
extern void  directory_make(char *, char *, int);

extern char *expand_getkeyed(char *, char *);
extern char *expand_string(char *);

extern BOOL  filter_interpret(char *, address_item **, BOOL *, int *, char **,
               BOOL, BOOL, BOOL);
extern BOOL  filter_runtest(int);
extern BOOL  filter_system_interpret(address_item **, BOOL *, int *, char **);

extern void  header_add(int, char *, ...);
extern BOOL  header_checkname(header_line *, char *, int);

extern char *host_find_byaddr(char *);
extern BOOL  host_find_byname(host_item *, char **);
extern BOOL  host_find_bydns(host_item *, BOOL, BOOL, BOOL, BOOL, char **);
extern ip_address_item *host_find_interfaces(void);
extern int   host_scan_for_local_hosts(host_item *, host_item *);
extern int   host_self_action(address_item *, int, int, BOOL, char *);

extern void  log_close(void);
extern void  log_write(int, int, char *format, ...);

extern BOOL  match_address_list(char *, int, char *, re_block **, int, int);
extern int   match_exists(char *);
extern BOOL  match_isinlist(char *, char *, re_block **, BOOL);
extern BOOL  match_check_string(char *, char *, re_block **, int, BOOL);
extern char *moan_check_errorcopy(char *);
extern BOOL  moan_send_message(char *, int, error_block *, header_line *,
               FILE *);
extern void  moan_smtp_batch(char *, ...);
extern void  moan_tell_mailmaster(address_item *, char *, char *, ...);
extern BOOL  moan_to_sender(int, error_block *, header_line *, FILE *);

extern int   os_getloadavg(void);
extern void  os_restarting_signal(int, void (*)(int));

extern char *parse_extract_address(char *, char **, int *, int *, int *, BOOL);
extern int   parse_extract_addresses(char *, address_item **, char **, BOOL,
               BOOL, BOOL, char *, error_block **);
extern char *parse_find_address_end(char *, BOOL);
extern char *parse_find_at(char *);
extern char *parse_fix_phrase(char *);

extern BOOL  queue_action(char *, int, FILE *, char **, int, int);
extern queue_filename *queue_get_spool_list(BOOL);
extern void  queue_list(void);
extern void  queue_run(char *, char *);

extern int   random_number(int);
extern BOOL  readconf_depends(driver_instance *, char *);
extern void  readconf_driver_init(char *, driver_instance **,
               driver_info *, int, void *, int, optionlist *, int);
extern void  readconf_main(void);
extern void  readconf_print(char *, char *);
extern char *readconf_printtime(int);
extern char *readconf_readname(char *, int, char *);
extern int   readconf_readtime(char *, int);
extern void  readconf_retries(void);
extern char *readconf_retry_error(char *, char *, int *, int *);
extern void  readconf_rewrites(void);
extern void  retry_add_item(address_item *, char *, BOOL, char *, BOOL);
extern char *retry_check_address(host_item *);
extern retry_config *retry_find_config(char *, char *, int, int);
extern void  retry_update(address_item **, address_item **, address_item **);
extern char *rewrite_address(char *, BOOL, BOOL);
extern char *rewrite_address_for_filter(char *, int);
extern char *rewrite_address_qualify(char *, BOOL);
extern header_line *rewrite_header(header_line *, char *, char *);
extern void  rewrite_test(char *);
extern int   route_address(address_item *, address_item **, address_item **,
               address_item **, int);
extern void  route_init(void);
extern void  route_tidyup(void);

extern char *search_find(void *, char *, char *, int, int, int *, char **);
extern void *search_open(char *, int, int, int *, int *, char **);
extern void  search_tidyup(void);
extern void  set_process_info(char *, ...);
extern int   smtp_setup_msg(FILE *, FILE *, BOOL);
extern BOOL  spool_open_datafile(char *);
extern int   spool_open_temp(char *);
extern int   spool_read_header(char *, BOOL);
extern int   spool_write_header(char *);
extern void  store_free_3(void *, char *, int);
extern void *store_malloc_3(size_t, char *, int);
extern char *string_base62(unsigned long int);
extern char *string_cat(char *, int *, int *, char *, int);
extern char *string_copy(char *);
extern char *string_copylc(char *);
extern char *string_copyn(char *, int);
extern char *string_copynlc(char *, int);
extern char *string_firstinlist(char *, int);
extern char *string_formatsize(int, char *);
extern int   string_interpret_escape(char **);
extern char *string_nextinlist(int);
extern char *string_printing(char *, BOOL);
extern char *string_sprintf(char *, ...);
extern int   strcmpic(char *, char *);
extern int   strncmpic(char *, char *, int);
extern char *strstric(char *, char *, BOOL);

extern char *tod_stamp(int);
extern BOOL  transport_check_serialized(char *, char *);
extern BOOL  transport_check_waiting(char *, char *, int, char *);
extern void  transport_end_serialized(char *, char *);
extern void  transport_init(void);
extern BOOL  transport_pass_socket(char *, char *, char *, int);
extern void  transport_timeout_handler(int);
extern void  transport_update_waiting(host_item *, char *);
extern BOOL  transport_write_message(address_item *, int, int, char *, int);
extern void  tree_add_duplicate(char *, BOOL);
extern void  tree_add_nonrecipient(char *, BOOL);
extern void  tree_add_unusable(host_item *);
extern void  tree_free(tree_node *);
extern int   tree_insertnode(tree_node **, tree_node *);
extern void  tree_print(tree_node *, FILE *);
extern tree_node *tree_search(tree_node *, char *);
extern tree_node *tree_search_addr(tree_node *, char *, BOOL);
extern void  tree_write(tree_node *, FILE *);

extern int  verify_address(char *, BOOL, BOOL, FILE *, BOOL *, char **, BOOL,
              BOOL);
extern BOOL verify_check_host(char *, host_item **);
extern BOOL verify_check_net(char *, ip_net_item **);
extern void verify_get_ident(int);
extern BOOL verify_sender(int *, char **);
extern BOOL verify_sender_preliminary(int *, char **);
extern void verify_setup_hostlist(char *, host_item **);
extern void verify_setup_netlist(char *, ip_net_item **);
extern void version_init(void);

/* End of functions.h */
