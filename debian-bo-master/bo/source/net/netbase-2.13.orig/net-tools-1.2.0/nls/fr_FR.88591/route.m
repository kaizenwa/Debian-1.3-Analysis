$set 2  #route

$ #_rresolve Original Message:(rresolve: unsupport address family %d !\n)
# rresolve: famille d'adresse pas supportée %d !\n

$ #_usage1 Original Message:(Usage: route [-nv]\n)
# Syntaxe: route [-nv]\n

$ #_usage2 Original Message:(       route [-v] del target\n)
#          route [-v] del cible\n

$ #_usage3 Original Message:(       route [-v] add {-net|-host} target [gw gateway]\n)
#          route [-v] add {-net|-host} cible [gw passerelle]\n

$ #_usage4 Original Message:(                  [metric NN] [netmask mask] [mss maxsegment] [window maxwindow]\n)
#                     [metric NN] [netmask mask] [mss segment_max] [window fenetre_max]\n

$ #_usage5 Original Message:(                  [[dev] device]\n)
#                     [[dev] périphérique]\n

$ #_table Original Message:(Kernel routing table\n)
# Table de routage noyau\n

$ #_header Original Message:(Destination     Gateway         Genmask         Flags MSS    Window Use Iface\n)
# Destination     Passerelle      Masque          Opts  MSS    Fenet. Use Iface\n

$ #_ignore Original Message:(metric %d ignored\n)
# métrique %d ignorée\n

$ #_cant_use Original Message:(%s: cannot use a NETWORK as gateway!\n)
# %s: ne peut utiliser un RESEAU comme passerelle !\n

$ #_MSS Original Message:(Invalid MSS.\n)
# MSS invalide.\n

$ #_window Original Message:(Invalid window.\n)
# Taille de fenetre invalide.\n

$ #_netmask1 Original Message:(route: netmask doesn't make sense with host route\n)
# route: netmask n'a pas de sens avec une route vers un hôte\n

$ #_netmask2 Original Message:(route: bogus netmask %s\n)
# route: mauvais netmask %s\n

$ #_netmask3 Original Message:(route: netmask doesn't match route address\n)
# route: netmask ne correspond pas à l'adresse de route\n
