# Set "block" to a string of length 512 (one HFS block)

set block "0123456789abcdef"
set block "$block$block$block$block"
set block "$block$block$block$block"
set block "$block$block"
