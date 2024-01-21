$set 2  #route

$ #_rresolve Original Message:(rresolve: unsupport address family %d !\n)
# rresolve: unsupport address family %d !\n

$ #_usage1 Original Message:(Usage: route [-nv]\n)
# Usage: route [-nv]\n

$ #_usage2 Original Message:(       route [-v] del target\n)
#        route [-v] del target\n

$ #_usage3 Original Message:(       route [-v] add {-net|-host} target [gw gateway]\n)
#        route [-v] add {-net|-host} target [gw gateway]\n

$ #_usage4 Original Message:(                  [metric NN] [netmask mask] [mss maxsegment] [window maxwindow]\n)
#                   [metric NN] [netmask mask] [mss maxsegment] [window maxwindow]\n

$ #_usage5 Original Message:(                  [[dev] device]\n)
#                   [[dev] device]\n

$ #_table Original Message:(Kernel routing table\n)
# Kernel routing table\n

$ #_header Original Message:(Destination     Gateway         Genmask         Flags MSS    Window Use Iface\n)
# Destination     Gateway         Genmask         Flags MSS    Window Use Iface\n

$ #_ignore Original Message:(metric %d ignored\n)
# metric %d ignored\n

$ #_cant_use Original Message:(%s: cannot use a NETWORK as gateway!\n)
# %s: cannot use a NETWORK as gateway!\n

$ #_MSS Original Message:(Invalid MSS.\n)
# Invalid MSS.\n

$ #_window Original Message:(Invalid window.\n)
# Invalid window.\n

$ #_netmask1 Original Message:(route: netmask doesn't make sense with host route\n)
# route: netmask doesn't make sense with host route\n

$ #_netmask2 Original Message:(route: bogus netmask %s\n)
# route: bogus netmask %s\n

$ #_netmask3 Original Message:(route: netmask doesn't match route address\n)
# route: netmask doesn't match route address\n
