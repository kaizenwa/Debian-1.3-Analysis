# Delete all temporary tags
/^@tag@$/,/^@\/tag@$/	{ s/ /\\protected_separator /g; }
/^@[\/]*tag@$/ d
