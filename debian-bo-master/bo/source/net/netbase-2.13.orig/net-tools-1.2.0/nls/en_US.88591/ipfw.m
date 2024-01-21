$set 9  #ipfw

$ #_ipf_blocking Original Message:(blocking)
# blocking

$ #_ipf_fwding Original Message:(forwarding)
# forwarding

$ #_ipf_accnting Original Message:(accounting)
# accounting

$ #_ipf_msqrading Original Message:(masquerading)
# masquerading

$ #_usage1 Original Message:(usage: ipfirewall [-n]\n\t\t  l[ist]  a[ccounting]\n)
# usage: ipfirewall [-n]\n\t\t  l[ist]  a[ccounting]\n

$ #_usage2 Original Message:(\t\t| l[ist]  b[locking]\n)
# \t\t| l[ist]  b[locking]\n

$ #_usage3 Original Message:(\t\t| l[ist]  f[irewall]\n)
# \t\t| l[ist]  f[irewall]\n

$ #_usage4 Original Message:(\t\t| f[lush] a[ccounting]\n)
# \t\t| f[lush] a[ccounting]\n

$ #_usage5 Original Message:(\t\t| f[lush] b[locking]\n)
# \t\t| f[lush] b[locking]\n

$ #_usage6 Original Message:(\t\t| f[lush] f[irewall]\n)
# \t\t| f[lush] f[irewall]\n

$ #_usage7 Original Message:(\t\t| c[heck] b[locking] <type> from <src> to <dst>\n)
# \t\t| c[heck] b[locking] <type> from <src> to <dst>\n

$ #_usage8 Original Message:(\t\t| c[heck] f[orwarding] <type> from <src> to <dst>\n)
# \t\t| c[heck] f[orwarding] <type> from <src> to <dst>\n

$ #_usage9 Original Message:(\t\t| p[olicy] b[locking] <accept|deny|reject>\n)
# \t\t| p[olicy] b[locking] <accept|deny|reject>\n

$ #_usage10 Original Message:(\t\t| p[olicy] f[orwarding] <accept|deny|reject>\n)
# \t\t| p[olicy] f[orwarding] <accept|deny|reject>\n

$ #_usage11 Original Message:(\t\t| a[dd]   a[ccounting] <type> [iface <addr>] from <src> to <dst>\n)
# \t\t| a[dd]   a[ccounting] <type> [iface <addr>] from <src> to <dst>\n

$ #_usage12 Original Message:(\t\t| a[dd]   b[locking]   <type> [iface <addr>] from <src> to <dst>\n)
# \t\t| a[dd]   b[locking]   <type> [iface <addr>] from <src> to <dst>\n

$ #_usage13 Original Message:(\t\t| a[dd]   f[orwarding] <type> [iface <addr>] from <src> to <dst>\n)
# \t\t| a[dd]   f[orwarding] <type> [iface <addr>] from <src> to <dst>\n

$ #_usage14 Original Message:(\t\t| a[dd]   m[asquerade] <type> from <src> to <dst>\n)
# \t\t| a[dd]   m[asquerade] <type> from <src> to <dst>\n

$ #_usage15 Original Message:(\t\t| d[el]   a[ccounting] <type> [iface <addr>] from <src> to <dst>\n)
# \t\t| d[el]   a[ccounting] <type> [iface <addr>] from <src> to <dst>\n

$ #_usage16 Original Message:(\t\t| d[el]   b[locking]   <type> [iface <addr>] from <src> to <dst>\n)
# \t\t| d[el]   b[locking]   <type> [iface <addr>] from <src> to <dst>\n

$ #_usage17 Original Message:(\t\t| d[el]   f[orwarding] <type> [iface <addr>] from <src> to <dst>\n)
# \t\t| d[el]   f[orwarding] <type> [iface <addr>] from <src> to <dst>\n

$ #_usage18 Original Message:(\t\t| d[el]   m[asquerade] <type> from <src> to <dst>\n)
# \t\t| d[el]   m[asquerade] <type> from <src> to <dst>\n

$ #_usage19 Original Message:(\t\t| zero[accounting]\n)
# \t\t| zero[accounting]\n

$ #_help1 Original Message:(where:\n)
# where:\n

$ #_help2 Original Message:(       <src> ::= <host> <port> /* for TCP or UDP */\n)
#        <src> ::= <host> <port> /* for TCP or UDP */\n

$ #_help3 Original Message:(       <src> ::= <host>        /* for ICMP */\n)
#        <src> ::= <host>        /* for ICMP */\n

$ #_help4 Original Message:(      <host> ::= <byte>.<byte>.<byte>.<byte>[/<width>]\n)
#       <host> ::= <byte>.<byte>.<byte>.<byte>[/<width>]\n

$ #_help5 Original Message:(               | <hostname>\n)
#                | <hostname>\n

$ #_help6 Original Message:(      <port> ::= <short> | <servicename>\n)
#       <port> ::= <short> | <servicename>\n

$ #_help7 Original Message:(     <short> ::= an integer in the range 1-65535\n)
#      <short> ::= an integer in the range 1-65535\n

$ #_help8 Original Message:(      <byte> ::= an integer in the range 0-255\n)
#       <byte> ::= an integer in the range 0-255\n

$ #_help9 Original Message:(     <width> ::= an integer in the range 0-32\n)
#      <width> ::= an integer in the range 0-32\n

$ #_range_set Original Message:(ipfw: range flag set but only %d ports\n)
# ipfw: range flag set but only %d ports\n

$ #_unkn Original Message:(unknown command (%d) passed to do_setsockopt - bye!\n)
# unknown command (%d) passed to do_setsockopt - bye!\n

$ #_ip Original Message:(ip header length %d, should be %d\n)
# ip header length %d, should be %d\n

$ #_data_ip Original Message:(data = struct iphdr : struct %shdr {\n)
# data = struct iphdr : struct %shdr {\n

$ #_data_ipfw Original Message:(data = struct ip_fw {\n)
# data = struct ip_fw {\n

$ #_accept Original Message:(\taccept )
# \taccept 

$ #_deny Original Message:(\tdeny )
# \tdeny 

$ #_univ Original Message:(\tuniversal\n)
# \tuniversal\n

$ #_missing Original Message:(ipfw: missing protocol name\n)
# ipfw: missing protocol name\n

$ #_illegal Original Message:(illegal protocol name \"%s\"\n)
# illegal protocol name "%s"\n

$ #_missing_ip Original Message:(ipfw: missing ip address\n)
# ipfw: missing ip address\n

$ #_periods Original Message:(ipfw: two periods in a row in ip address (%s)\n)
# ipfw: two periods in a row in ip address (%s)\n

$ #_unkn_host Original Message:(ipfw: unknown host \"%s\"\n)
# ipfw: unknown host "%s"\n

$ #_addr_length Original Message:(ipfw: hostentry addr length = %d, expected %d)
# ipfw: hostentry addr length = %d, expected %d

$ #_matched Original Message:(ipfw: Only %d fields matched in IP address!\n)
# ipfw: Only %d fields matched in IP address!\n

$ #_too_large Original Message:(ipfw: number too large in ip address (%s)\n)
# ipfw: number too large in ip address (%s)\n

$ #_inc_format Original Message:(ipfw: incorrect ip address format \"%s\" (expected 3 periods)\n)
# ipfw: incorrect ip address format "%s" (expected 3 periods)\n

$ #_not_allowed Original Message:(ipfw: ip netmask not allowed here (%s)\n)
# ipfw: ip netmask not allowed here (%s)\n

$ #_missing_mask Original Message:(ipfw: missing mask value (%s)\n)
# ipfw: missing mask value (%s)\n

$ #_non_num Original Message:(ipfw: non-numeric mask value (%s)\n)
# ipfw: non-numeric mask value (%s)\n

$ #_junk_mask Original Message:(ipfw: junk after mask (%s)\n)
# ipfw: ignore après le masque (%s)\n

$ #_out_range Original Message:(ipfw: mask length value out of range (%s)\n)
# ipfw: mask length value out of range (%s)\n

$ #_junk_ip Original Message:(ipfw: junk after ip address (%s)\n)
# ipfw: ignore après l'adresse IP (%s)\n

$ #_illegal_port Original Message:(ipfw: illegal port number (%s)\n)
# ipfw: illegal port number (%s)\n

$ #_portnum_out Original Message:(ipfw: port number out of range (%d)\n)
# ipfw: port number out of range (%d)\n

$ #_unkn_service Original Message:(ipfw: unknown %s service \"%s\"\n)
# ipfw: unknown %s service "%s"\n

$ #_too_port Original Message:(ipfw: too many port numbers (max %d, got at least %d, next parm="%s")\n)
# ipfw: too many port numbers (max %d, got at least %d, next parm="%s")\n

$ #_port_ranges Original Message:(port ranges are only allowed for the first port value pair (%s)\n)
# port ranges are only allowed for the first port value pair (%s)\n

$ #_no_range Original Message:(ipfw: port range not allowed here (%s)\n)
# ipfw: port range not allowed here (%s)\n

$ #_missing_port Original Message:(ipfw: missing port number%s\n)
# ipfw: missing port number%s\n

$ #_nomore_port Original Message:(ipfw: not enough port numbers (expected %d, got %d)\n)
# ipfw: not enough port numbers (expected %d, got %d)\n

$ #_usage20 Original Message:(usage: ipfirewall check %s ...\n)
# usage: ipfirewall check %s ...\n

$ #_check_blocking Original Message:(blocking)
# blocking

$ #_check_forwarding Original Message:(forwarding)
# forwarding

$ #_check Original Message:(check %s )
# check %s 

$ #_only_check Original Message:(ipfw: can only check TCP or UDP packets\n)
# ipfw: can only check TCP or UDP packets\n

$ #_missing_from Original Message:(ipfw: missing \"from\" keyword\n)
# ipfw: missing "from" keyword\n

$ #_expect_from Original Message:(ipfw: expected \"from\" keyword, got \"%s\"\n)
# ipfw: expected "from" keyword, got "%s"\n

$ #_missing_to Original Message:(ipfw: missing \"to\" keyword\n)
# ipfw: missing "to" keyword\n

$ #_expect_to Original Message:(ipfw: expected \"to\" keyword, got \"%s\"\n)
# ipfw: expected "to" keyword, got "%s"\n

$ #_paq_accept Original Message:(packet accepted by %s firewall\n)
# packet accepted by %s firewall\n

$ #_blocking Original Message:(blocking)
# blocking

$ #_forwarding Original Message:(forwarding)
# forwarding

$ #_paq_reject Original Message:(packet rejected by %s firewall\n)
# packet rejected by %s firewall\n

$ #_extra Original Message:(ipfw: extra parameters at end of command ()
# ipfw: extra parameters at end of command (

$ #_usage21 Original Message:(usage: ipfirewall add %s ...\n)
# usage: ipfirewall add %s ...\n

$ #_add Original Message:(add %s )
# add %s 

$ #_missing_acc Original Message:(ipfw: missing \"accept\" or \"deny\" keyword\n)
# ipfw: missing "accept" or "deny" keyword\n

$ #_expect_acc Original Message:(ipfw: expected \"accept\", \"deny\" or \"reject\", got \"%s\"\n)
# ipfw: expected "accept", "deny" or "reject", got "%s"\n

$ #_missing_proto Original Message:(ipfw: missing protocol name.\n)
# ipfw: missing protocol name.\n

$ #_missing_iface Original Message:(ipfw: missing interface address.\n)
# ipfw: missing interface address.\n

$ #_invalid_iface Original Message:(Invalid interface address.\n)
# Invalid interface address.\n

$ #_missing_from2 Original Message:(ipfw: missing \"from\" keyword\n)
# ipfw: missing "from" keyword\n

$ #_expect_from2 Original Message:(ipfw: expected \"from\", got \"%s\"\n)
# ipfw: expected "from", got "%s"\n

$ #_missing_to2 Original Message:(ipfw: missing \"to\" keyword\n)
# ipfw: missing "to" keyword\n

$ #_expect_to2 Original Message:(ipfw: expected \"to\", got \"%s\"\n)
# ipfw: expected "to", got "%s"\n

$ #_extra2 Original Message:(ipfw: extra parameters at end of command ()
# ipfw: extra parameters at end of command (

$ #_usage22 Original Message:(usage: ipfirewall delete %s ...\n)
# usage: ipfirewall delete %s ...\n

$ #_delete Original Message:(delete %s )
# delete %s 

$ #_missing_acc2 Original Message:(ipfw: missing \"accept\" or \"deny\" keyword\n)
# ipfw: missing "accept" or "deny" keyword\n

$ #_expect_acc2 Original Message:(ipfw: expected \"accept\" or \"deny\", got \"%s\"\n)
# ipfw: expected "accept" or "deny", got "%s"\n

$ #_missing_proto2 Original Message:(ipfw: missing protocol name.\n)
# ipfw: missing protocol name.\n

$ #_missing_iface2 Original Message:(ipfw: missing interface address.\n)
# ipfw: missing interface address.\n

$ #_invalid_iface2 Original Message:(Invalid interface address.\n)
# Invalid interface address.\n

$ #_missing_from3 Original Message:(ipfw: missing \"from\" keyword\n)
# ipfw: missing "from" keyword\n

$ #_expect_from3 Original Message:(ipfw: expected \"from\", got \"%s\"\n)
# ipfw: expected "from", got "%s"\n

$ #_missing_to3 Original Message:(ipfw: missing \"to\" keyword\n)
# ipfw: missing "to" keyword\n

$ #_expect_to3 Original Message:(ipfw: expected \"to\", got \"%s\"\n)
# ipfw: expected "to", got "%s"\n

$ #_extra3 Original Message:(ipfw: extra parameters at end of command ()
# ipfw: extra parameters at end of command (

$ #_anywhere Original Message:(anywhere)
# anywhere

$ #_bytes Original Message:(Packets\t Bytes\t)
# Packets\t Bytes\t

$ #_type Original Message:(Type\t)
# Type\t

$ #_proto Original Message:(Proto %19.19s %19.19s    Ports\n)
# Proto %19.19s %19.19s    Ports\n

$ #_print_from Original Message:(From        )
# From        

$ #_print_to Original Message:(To         )
# To         

$ #_masquerade Original Message:((masquerade)
# (masquerade

$ #_list_accept Original Message:(accept\t)
# accept\t

$ #_list_deny Original Message:(deny\t)
# deny\t

$ #_list_all Original Message:(all   )
# all   

$ #_list_any Original Message:(any)
# any

$ #_list_any2 Original Message:(any)
# any

$ #_expect_kwds Original Message:(blocking, forwarding or accounting keyword expected.\n)
# blocking, forwarding or accounting keyword expected.\n

$ #_found_kwds Original Message:(Found '%s': 'blocking', 'forwarding' or 'accounting' keyword expected.\n)
# Found '%s': 'blocking', 'forwarding' or 'accounting' keyword expected.\n

$ #_raw_socket Original Message:(ipfw: raw socket creation)
# ipfw: raw socket creation

$ #_expect_main_blocking Original Message:(ipfw: expected \"blocking\" or \"forwarding\".\n)
# ipfw: expected "blocking" or "forwarding".\n

$ #_expect_main_accept Original Message:(ipfw: expected \"accept\", \"deny\" or \"reject\".\n)
# ipfw: expected "accept", "deny" or "reject".\n

$ #_expect_main_accounting Original Message:(ipfw: expected \"accounting\", \"blocking\" or \"firewall\".\n)
# ipfw: expected "accounting", "blocking" or "firewall".\n

$ #_illegal_check Original Message:(ipfw: illegal `check' keyword: %s\n)
# ipfw: illegal `check' keyword: %s\n

$ #_main_missing Original Message:((missing))
# (missing)

$ #_unkn_cmd Original Message:(ipfw: unknown command `%s'\n\n)
# ipfw: unknown command `%s'\n\n

$ #_unkn_kwd Original Message:(ipfw: unknown `%s' keyword: `%s'\n)
# ipfw: unknown `%s' keyword: `%s'\n

