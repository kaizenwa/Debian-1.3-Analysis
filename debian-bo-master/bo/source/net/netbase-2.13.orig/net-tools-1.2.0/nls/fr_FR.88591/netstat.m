$set 4  #netstat

$ #_table Original Message:(Kernel routing table\n)
# Table de routage noyau\n

$ #_header Original Message:(Destination     Gateway         Genmask         Flags Metric Ref Use    Iface\n)
# Destination     Passerelle      Masque          Opts  MSS    Fenet. Use Iface\n

$ #_route Original Message:(route: unsupported address family %d !\n)
# route: famille d'adresse pas supportée %d !\n

$ #_args Original Message:(%s -> %d args)
# %s -> %d args

$ #_netstat Original Message:(netstat: unsupported address family %d !\n)
# netstat: famille d'adresse pas supporté %d !\n

$ #_UNKN Original Message:(UNKNOWN)
# INCONNU

$ #_off Original Message:(off (0.00/%ld))
# off (0.00/%ld)

$ #_on Original Message:(on (%2.2f/%ld))
# on (%2.2f/%ld)

$ #_unkn Original Message:(unkn-%d (%2.2f/%ld))
# inconnu-%d (%2.2f/%ld)

$ #_off2 Original Message:(off (0.00/%ld) %c)
# off (0.00/%ld) %c

$ #_on2 Original Message:(on (%2.2f/%ld) %c)
# on (%2.2f/%ld) %c

$ #_unkn2 Original Message:(unkn-%d (%2.2f/%ld) %c)
# inconnu-%d (%2.2f/%ld) %c

$ #_off3 Original Message:(off (0.00/%ld) %c)
# off (0.00/%ld) %c

$ #_on3 Original Message:(on (%2.2f/%ld) %c)
# on (%2.2f/%ld) %c

$ #_unkn3 Original Message:(unkn-%d (%2.2f/%ld) %c)
# inconnu-%d (%2.2f/%ld) %c

$ #_unix Original Message:(Active UNIX domain sockets\n)
# Prises du domaine UNIX actives\n

$ #_header_unix Original Message:(Proto RefCnt Flags      Type            State           Path\n)
# Proto CptRef Options    Type            Etat            Chemin\n

$ #_noax25 Original Message:(AX.25 not configured in this system.\n)
# AX.25 pas configuré sur ce système.\n

$ #_ax25 Original Message:(Activate AX.25 sockets\n)
# Prises AX.25 actives\n

$ #_header_ax25 Original Message:(Destination   Source      Type      State     Vr/Vs   Protocol\n)
# Destination   Source      Type      Etat      Vr/Vs   Protocole\n

$ #_noipx Original Message:(IPX not configured in this system.\n)
# IPX pas configuré sur ce système.\n

$ #_noflags Original Message:([NO FLAGS])
# [PAS D'OPTIONS]

$ #_interface Original Message:(Kernel Interface table\n)
# Table des interfaces du noyau\n

$ #_header_iface Original Message:(Iface   MTU Met  RX-OK RX-ERR RX-DRP RX-OVR  TX-OK TX-ERR TX-DRP TX-OVR Flags\n)
# Iface   MTU Met  RX-OK RX-ERR RX-DRP RX-OVR  TX-OK TX-ERR TX-DRP TX-OVR Opts\n

$ #_unkn_iface Original Message:(%s: unknown interface.\n)
# %s: interface inconnue.\n

$ #_usage1 Original Message:(Usage:\tnetstat [options]\n)
# Syntaxe:\tnetstat [options]\n

$ #_usage2 Original Message:(\t-a also listening sockets\n)
# \t\t-a également les prises en écoute\n

$ #_usage3 Original Message:(\t-c continous listing\n)
# \t\t-c listage continu\n

$ #_usage4 Original Message:(\t-i interface statistics\n)
# \t\t-i statistiques des interfaces\n

$ #_usage5 Original Message:(\t-n show network numbers instead of names\n)
# \t\t-n affiche les adresses réseaux au lieu des noms\n

$ #_usage6 Original Message:(\t-o show timer states\n)
# \t\t-o affiche les états des timers\n

$ #_usage7 Original Message:(\t-r show kernel routing table\n)
# \t\t-r affiche les tables de routage du noyau\n

$ #_usage8 Original Message:(\t-t show active tcp connections\n)
# \t\t-t affiche les connexions TCP actives\n

$ #_usage9 Original Message:(\t-u show active udp connections\n)
# \t\t-u affiche les connexions UDP actives\n

$ #_usage10 Original Message:(\t-v show version information\n)
# \t\t-v affiche les informations sur la version\n

$ #_usage11 Original Message:(\t-w show active raw connections\n)
# \t\t-w affiche les connexions RAW actives\n

$ #_usage12 Original Message:(\t-x show active unix sockets\n)
# \t\t-x affiche les prises du domaine UNIX actives\n

$ #_internet Original Message:(Active Internet connections)
# Connexions Internet Actives

$ #_servers Original Message:( (including servers))
#  (y compris les serveurs)

$ #_header_internet Original Message:(\nProto Recv-Q Send-Q Local Address          Foreign Address        (State)       User\n)
# \nProto Recv-Q Send-Q Adresse Locale         Adresse Distante       (Etat)        Utilisateur\n

$ #_header_ipx Original Message:(Active IPX sockets\nProto Recv-Q Send-Q Local Address          Foreign Address        (State)       User\n)
# Prises IPX actives\nProto Recv-Q Send-Q Adresse Locale         Adresse Distante       (Etat)        Utilisateur\n
