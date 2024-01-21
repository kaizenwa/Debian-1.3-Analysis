# Change """ to "&quot;", "<" to "&lt;", and ">" to "&gt;". 
# within <PRE>...</PRE> in HTML code.  Character escapes must be lower case!!!
# The "&" character does not need anything special.
/<PRE>/,/<\/PRE>/ { s/"/\&quot;/g; }
/<PRE>/,/<\/PRE>/ { s/</\&lt;/g; s/\&lt;\([\/]*\)PRE>/<\1PRE>/g; }
/<PRE>/,/<\/PRE>/ { s/>/\&gt;/g; s/<\([\/]*\)PRE\&gt;/<\1PRE>/g; }
# Delete all empty <P></P> containers
s/<P><\/P>//g
