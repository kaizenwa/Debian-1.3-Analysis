# ==========================================================================
# Default values for keywords:
# ==========================================================================
#
#  devmap   : 1
#  daymap   : 1,2,3,4,5,6,7
#  housecode: a
#  mode     : normal
#  minute   : 0
#  hour     : 0
#  function : on
#
# ==========================================================================
# Allowable values for keywords:
# ==========================================================================
#
#  devmap   : Comma separated list of numbers in range of 1..16
#  daymap   : Comma separated list of numbers in range of 1..7 (1==monday)
#  housecode: a-p
#  mode     : normal, security, today, tomorrow, clear
#  minute   : 0-60
#  hour     : 0-23
#  function : on, off, dim
# ==========================================================================

event {
  devmap    2             # Comma separated list of devices to affect
  daymap    1,2,3,4,5,6,7 # ( 1 == Monday ) ... ( 7 == Sunday )
  housecode a             # Which housecode the devices are on
  mode      normal        # One of ( normal, security, today, tomorrow, clear )
  minute    39            # Minute of event
  hour      23            # Hour of event
  function  dim           # One of         ( on, off, dim )
  dimlevel  5             # Range of 0..15 ( 0 being the brightest )
}

event {
  devmap    1,3,5         # May address multiple devices at once
  daymap    1,2,3,4,5     # Maybe just the weekdays eh?
  hour      5             # At 5 am (Use 17 for 5pm)
  function  off           # turn off
}
