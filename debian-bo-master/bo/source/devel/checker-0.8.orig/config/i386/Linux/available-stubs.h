
#define HAVE_ARPA_INET_H 1
#define HAVE_CTYPE_H 1
#define HAVE_NCURSES_H 1
#define HAVE_STDARG_H 1
#define HAVE_DIRENT_H 1
#define HAVE_FCNTL_H 1
#define HAVE_FNMATCH_H 1
#define HAVE_GETOPT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_GRP_H 1
#define HAVE_SYS_IOCTL_H 1
#define HAVE_STDDEF_H 1
#define HAVE_MATH_H 1
#define HAVE_NETDB_H 1
#define HAVE_NETINET_IN_H 1
#define HAVE_PWD_H 1
#define HAVE_SETJMP_H 1
#define HAVE_SIGNAL_H 1
#define HAVE_STRING_H 1
#define HAVE_STDIO_H 1
#define HAVE_STDLIB_H 1
#define HAVE_SYS_RESOURCE_H 1
#define HAVE_SYS_SOCKET_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TIMEB_H 1
#define HAVE_SYS_UIO_H 1
#define HAVE_SYS_UTSNAME_H 1
#define HAVE_SYS_WAIT_H 1
#define HAVE_TERMIOS_H 1
#define HAVE_TIME_H 1
#define HAVE_UNISTD_H 1
#define HAVE_SYS_PARAM_H 1
#define HAVE_UTIME_H 1
#define HAVE_TERMCAP_H 1
#define HAVE_SYS_MMAN_H 1
#define HAVE_MNTENT_H 1
#define HAVE_ASSERT_H 1

#ifdef NEVER
/* stubs-arpa_inet.c */
#define HAVE_inet_addr
#define HAVE_inet_ntoa
#define HAVE_inet_aton
#define HAVE_inet_lnaof
#define HAVE_inet_netof
#define HAVE_inet_makeaddr
#define HAVE_inet_network
/* stubs-curses.c */
#if 0
#define HAVE_box
#define HAVE_cbreak
#define HAVE_delwin
#define HAVE_echo
#define HAVE_endwin
#define HAVE_gettmode
#define HAVE_idlok
#define HAVE_initscr
#define HAVE_mvcur
#define HAVE_mvwin
#define HAVE_newwin
#define HAVE_nl
#define HAVE_nocbreak
#define HAVE_noecho
#define HAVE_nonl
#define HAVE_noraw
#define HAVE_overlay
#define HAVE_overwrite
#define HAVE_raw
#define HAVE_resetty
#define HAVE_savetty
#define HAVE_scroll
#define HAVE_subwin
/* #define HAVE_suspendwin */
#define HAVE_touchline
#define HAVE_touchoverlap
#define HAVE_touchwin
#define HAVE_vwprintw
#define HAVE_vwscanw
#define HAVE_waddch
#define HAVE_waddnstr
#define HAVE_wclear
#define HAVE_wclrtobot
#define HAVE_wclrtoeol
#define HAVE_wdelch
#define HAVE_wdeleteln
#define HAVE_werase
#define HAVE_wgetch
#define HAVE_winsch
#define HAVE_winsertln
#define HAVE_wmove
#define HAVE_wprintw
#define HAVE_wrefresh
#define HAVE_wscanw
#define HAVE_wstandend
#define HAVE_wstandout
#endif
/* stubs-dirent.c */
#define HAVE_chkr_func
#define HAVE_opendir
#define HAVE_closedir
#define HAVE_readdir
#define HAVE_rewinddir
#define HAVE_seekdir
#define HAVE_telldir
/* stubs-fcntl.c */
#define HAVE_creat
#define HAVE_open
#define HAVE_lockf
#define HAVE_fcntl
/* stubs-fnmatch.c */
#define HAVE_fnmatch
/* stubs-getopt.c */
#define HAVE_getopt
#define HAVE_getopt_long
#define HAVE_getopt_long_only
/* stubs-grp.c */
#define HAVE_chkr_func
#define HAVE_getgrgid
#define HAVE_getgrent
#define HAVE_setgrent
#define HAVE_endgrent
#define HAVE_getgrnam
/* stubs-netdb.c */
#define HAVE_gethostbyname
#define HAVE_gethostbyaddr
#define HAVE_getservbyname
#define HAVE_herror
#define HAVE_endhostent
#define HAVE_endnetent
#define HAVE_endprotoent
#define HAVE_endservent
#define HAVE_endrpcent
#define HAVE_gethostent
#define HAVE_getnetbyaddr
#define HAVE_getnetbyname
#define HAVE_getnetent
#define HAVE_getprotobyname
#define HAVE_getprotobynumber
#define HAVE_getprotoent
#define HAVE_getservbyport
#define HAVE_getservent
#define HAVE_getrpcent
#define HAVE_getrpcbyname
#define HAVE_getrpcbynumber
#define HAVE_sethostent
#define HAVE_setnetent
#define HAVE_setprotoent
#define HAVE_setservent
#define HAVE_setrpcent
/* stubs-netinet_in.c */
#define HAVE_ntohl
#define HAVE_ntohs
#define HAVE_htonl
#define HAVE_htons
/* stubs-pwd.c */
#define HAVE_getpwuid
#define HAVE_getpwnam
#define HAVE_endpwent
#define HAVE_setpwent
/* stubs-setjmp.c */
#define HAVE_longjmp
#define HAVE___setjmp
/* stubs-signal.c */
#define HAVE_signal
#define HAVE_kill
#define HAVE_raise
#define HAVE_sigblock
#define HAVE_sigsetmask
#define HAVE_sigaddset
#define HAVE_sigdelset
#define HAVE_sigemptyset
#define HAVE_sigfillset
#define HAVE_sigismember
#define HAVE_siggetmask
#define HAVE_psignal
#define HAVE_sigpause
#define HAVE_sigprocmask
/* stubs-stdio.c */
#define HAVE_printf
#define HAVE_fprintf
#define HAVE_scanf
#define HAVE_sscanf
#define HAVE_vasprintf
#define HAVE_asprintf
#define HAVE_vsnprintf
#define HAVE_snprintf
#define HAVE_vsscanf
#define HAVE_vscanf
#define HAVE_vfscanf
#define HAVE_vsprintf
#define HAVE_vprintf
#define HAVE_vfprintf
#define HAVE_sprintf
#define HAVE_fscanf
#define HAVE_fgets
#define HAVE_fputs
#define HAVE_puts
#define HAVE_fflush
#define HAVE_fputc
#define HAVE_putc
#define HAVE_setbuf
#define HAVE_putchar
#define HAVE_fileno
#define HAVE_getc
#define HAVE_fopen
#define HAVE_ungetc
#define HAVE_fclose
#define HAVE_perror
#define HAVE_getchar
#define HAVE_fwrite
#define HAVE_fread
#define HAVE_freopen
#define HAVE_ferror
#define HAVE_ftell
#define HAVE_popen
#define HAVE_pclose
#define HAVE_fdopen
#define HAVE_rewind
#define HAVE_rename
#define HAVE_remove
#define HAVE_gets
#define HAVE_fseek
#define HAVE_fgetc
#define HAVE_feof
#define HAVE_setlinebuf
#define HAVE_tmpnam
#define HAVE_tempnam
#define HAVE_putw
#define HAVE_getw
#define HAVE_tmpfile
#define HAVE_setvbuf
/* stubs-stdlib.c */
#define HAVE_exit
#define HAVE_abort
#define HAVE_getenv
#define HAVE_atoi
#define HAVE_atol
#define HAVE_atof
/* #define HAVE_atoq */
#define HAVE_strtof
#define HAVE_strtod
#define HAVE_strtold
#define HAVE_strtol
#define HAVE_strtoul
#define HAVE_strtoq
#define HAVE_strtouq
#define HAVE_qsort
#define HAVE_bsearch
#define HAVE_abs
#define HAVE_labs
#define HAVE_ldiv
#define HAVE_div
#define HAVE_unsetenv
#define HAVE_setenv
#define HAVE_putenv
#define HAVE_drand48
#define HAVE_erand48
#define HAVE_lrand48
#define HAVE_nrand48
#define HAVE_mrand48
#define HAVE_jrand48
#define HAVE_srand48
#define HAVE_seed48
#define HAVE_lcong48
#define HAVE_rand
#define HAVE_srand
#define HAVE_random
#define HAVE_srandom
#define HAVE_initstate
#define HAVE_setstate
#define HAVE_atexit
#define HAVE_on_exit
#define HAVE_system
/* stubs-string.c */
#define HAVE_strrchr
#define HAVE_strcat
#define HAVE_memmove
#define HAVE_memcpy
#define HAVE_strncmp
#define HAVE_strncpy
#define HAVE_strcpy
#define HAVE_strlen
#define HAVE_rindex
#define HAVE_strcmp
#define HAVE_strchr
#define HAVE_strerror
#define HAVE_index
#define HAVE_bcopy
#define HAVE_bcmp
#define HAVE_bzero
#define HAVE_strstr
#define HAVE_memset
#define HAVE_memcmp
#define HAVE_strncat
#define HAVE_stpcpy
#define HAVE_strdup
#define HAVE_strcoll
#define HAVE_memchr
#define HAVE_strspn
#define HAVE_strpbrk
#define HAVE_strtok
#define HAVE_strcspn
#define HAVE_strncasecmp
#define HAVE_strcasecmp
/* stubs-sys_resource.c */
#define HAVE_getrlimit
#define HAVE_setrlimit
#define HAVE_setpriority
#define HAVE_getpriority
#define HAVE_getrusage
/* stubs-sys_socket.c */
#define HAVE_chkr_func
#define HAVE_socket
#define HAVE_socketpair
#define HAVE_bind
#define HAVE_connect
#define HAVE_listen
#define HAVE_accept
#define HAVE_getsockopt
#define HAVE_setsockopt
#define HAVE_getsockname
#define HAVE_getpeername
#define HAVE_send
#define HAVE_recv
#define HAVE_sendto
#define HAVE_recvfrom
#define HAVE_shutdown
/* stubs-sys_stat.c */
#define HAVE_chkr_func
#define HAVE__xstat
#define HAVE__fxstat
#define HAVE__lxstat
#define HAVE__xmknod
#define HAVE_stat
#define HAVE_lstat
#define HAVE_fstat
#define HAVE_mknod
#define HAVE_umask
#define HAVE_mkfifo
#define HAVE_mkdir
#define HAVE_chmod
#define HAVE_fchmod
/* stubs-sys_time.c */
#define HAVE_chkr_func
#define HAVE_gettimeofday
#define HAVE_settimeofday
#define HAVE_getitimer
#define HAVE_setitimer
#define HAVE_select
#define HAVE_FD_SET
#define HAVE_FD_CLR
#define HAVE_FD_ISSET
#define HAVE_FD_ZERO
/* stubs-sys_timeb.c */
#define HAVE_ftime
/* stubs-sys_uio.c */
#define HAVE_readv
#define HAVE_writev
/* stubs-sys_utsname.c */
#define HAVE_uname
/* stubs-sys_wait.c */
#define HAVE_wait
#define HAVE_waitpid
/* stubs-termios.c */
#define HAVE_tcgetattr
#define HAVE_tcsetattr
/* stubs-time.c */
#define HAVE_stime
#define HAVE_clock
#define HAVE_time
#define HAVE_difftime
#define HAVE_mktime
#define HAVE_asctime
#define HAVE_ctime
#define HAVE_strftime
#define HAVE_tzset
#define HAVE_gmtime
#define HAVE_localtime
/* stubs-unistd.c */
#define HAVE_execl
#define HAVE_execvp
#define HAVE_execve
#define HAVE_read
#define HAVE_close
#define HAVE_write
#define HAVE_getuid
#define HAVE_geteuid
#define HAVE_getgid
#define HAVE_getegid
#define HAVE_getgroups
#define HAVE_getlogin
#define HAVE_ttyname
#define HAVE_access
#define HAVE_alarm
#define HAVE_sleep
#define HAVE_pause
#define HAVE_lseek
#define HAVE_pipe
#define HAVE_chown
#define HAVE_fchown
#define HAVE_chdir
#define HAVE_link
#define HAVE_dup
#define HAVE_dup2
#define HAVE_getpid
#define HAVE_getppid
#define HAVE_unlink
#define HAVE_mktemp
#define HAVE_execv
#define HAVE_execlp
#define HAVE_fork
#define HAVE_vfork
#define HAVE_llseek
#define HAVE_fchdir
#define HAVE_getcwd
#define HAVE_get_current_dir_name
#define HAVE_getwd
#define HAVE_pathconf
#define HAVE_fpathconf
#define HAVE_sysconf
#define HAVE_confstr
#define HAVE_getpgrp
#define HAVE_setpgid
#define HAVE_setpgrp
#define HAVE_setsid
#define HAVE_setuid
#define HAVE_setreuid
#define HAVE_seteuid
#define HAVE_setgid
#define HAVE_setregid
#define HAVE_setegid
#define HAVE_setfsuid
#define HAVE_symlink
#define HAVE_readlink
#define HAVE_rmdir
#define HAVE_tcgetpgrp
#define HAVE_tcsetpgrp
#define HAVE_gethostid
#define HAVE_sethostid
#define HAVE_getpagesize
#define HAVE_getdtablesize
#define HAVE_sync
#define HAVE_fsync
#define HAVE_vhangup
#define HAVE_acct
#define HAVE_ftruncate
#define HAVE_truncate
#define HAVE_nice
#define HAVE_usleep
#define HAVE_isatty
#define HAVE_gethostname
#define HAVE__exit
/* stubs-utime.c */
#define HAVE_utime
/* stubs-ctype.c */
#define HAVE_isalnum
#define HAVE_isalpha
#define HAVE_iscntrl
#define HAVE_isdigit
#define HAVE_islower
#define HAVE_isgraph
#define HAVE_isprint
#define HAVE_ispunct
#define HAVE_isspace
#define HAVE_isupper
#define HAVE_isxdigit
#define HAVE_isblank
#define HAVE_tolower
#define HAVE_toupper
#define HAVE_isascii
#define HAVE_toascii
#define HAVE__toupper
#define HAVE__tolower
/* stubs-math.c */
#define HAVE_acos
#define HAVE_asin
#define HAVE_atan
#define HAVE_atan2
#define HAVE_cos
#define HAVE_sin
#define HAVE_tan
#define HAVE_cosh
#define HAVE_sinh
#define HAVE_tanh
#define HAVE_acosh
#define HAVE_asinh
#define HAVE_atanh
#define HAVE_exp
#define HAVE_frexp
#define HAVE_ldexp
#define HAVE_log
#define HAVE_log10
#define HAVE_expm1
#define HAVE_log1p
#define HAVE_modf
#define HAVE_pow
#define HAVE_sqrt
#define HAVE_cbrt
#define HAVE_ceil
#define HAVE_fabs
#define HAVE_floor
#define HAVE_fmod
#define HAVE___isinf
#define HAVE___isnan
#define HAVE___finite
#define HAVE___infnan
#define HAVE___copysign
#define HAVE___rint
#define HAVE_rint
#define HAVE_hypot
#define HAVE_isinf
#define HAVE_isnan
#define HAVE_finite
#define HAVE_infnan
#define HAVE_copysign
#define HAVE_drem
#define HAVE_pow2
#define HAVE_pow10
#define HAVE_erf
#define HAVE_erfc
#define HAVE_j0
#define HAVE_j1
#define HAVE_jn
#define HAVE_lgamma
#define HAVE_y0
#define HAVE_y1
#define HAVE_yn
#define HAVE_acosl
#define HAVE_asinl
#define HAVE_atanl
#define HAVE_cosl
#define HAVE_sinl
#define HAVE_tanl
#define HAVE_coshl
#define HAVE_sinhl
#define HAVE_tanhl
#define HAVE_acoshl
#define HAVE_asinhl
#define HAVE_atanhl
#define HAVE_expl
#define HAVE_frexpl
#define HAVE_ldexpl
#define HAVE_logl
#define HAVE_log10l
#define HAVE_expm1l
#define HAVE_log1pl
#define HAVE_modfl
#define HAVE_powl
#define HAVE_atan2l
#define HAVE_sqrtl
#define HAVE_cbrtl
#define HAVE_log2l
#define HAVE_ceill
#define HAVE_fabsl
#define HAVE_floorl
#define HAVE_fmodl
#define HAVE___isinfl
#define HAVE___isnanl
#define HAVE_hypotl
#define HAVE_pow2l
#define HAVE_pow10l
#define HAVE_erfl
#define HAVE_erfcl
#define HAVE_j0l
#define HAVE_j1l
#define HAVE_jnl
#define HAVE_lgammal
#define HAVE_y0l
#define HAVE_y1l
#define HAVE_ynl
/* stubs-termcap.c */
#define HAVE_tgetent
#define HAVE_tgetnum
#define HAVE_tgetflag
#define HAVE_tgetstr
#define HAVE_tputs
#define HAVE_tgoto
/* stubs-sys_mman.c */
#define HAVE_mmap
#define HAVE_munmap
#define HAVE_mprotect
/* stubs-mntent.c */
#define HAVE_setmntent
#define HAVE_getmntent
#define HAVE_addmntent
#define HAVE_hasmntopt
#define HAVE_endmntent
/* stubs-assert.c */
#define HAVE___eprintf

#endif /* NEVER */