#!/import/python/bin/python

import sys, string, pwd, os, regex
from crypt import *
from socket import *
from SOCKET import *
from select import *

user_pat = regex.compile("USER +\([^/]+\)/\([^ \r]+\) *\r?\n", regex.casefold)
pass_pat = regex.compile("PASS +\([^ \r]+\) *\r?\n", regex.casefold)
stat_pat = regex.compile("STAT *\r?\n", regex.casefold)
last_pat = regex.compile("LAST *\r?\n", regex.casefold)
top_pat = regex.compile("TOP +\([0-9]+\) +\([0-9]+\) *\r?\n", regex.casefold)
list_pat = regex.compile("LIST +\([0-9]*\) *\r?\n", regex.casefold)
retr_pat = regex.compile("RETR +\([0-9]+\) *\r?\n", regex.casefold)
quit_pat = regex.compile("QUIT *\r?\n", regex.casefold)

path_pat = regex.compile("Path:[ \t]+\([^ \t\n]+\)", regex.casefold)

def readc(cr):
    line = cr.readline()
    if line:
        if pass_pat.match(line) >= 0:
	    sys.stderr.write("<- PASS ****\n")
	else:
	    sys.stderr.write("<- " + line)
    return line

def writec(cw, line, *no_echo):
    line = line + "\r\n"
    if not no_echo: sys.stderr.write("-> " + line)
    cw.write(line)
    cw.flush()

def mh_dir():
    prof = open(".mh_profile", "r")
    for line in prof.readlines():
    	if path_pat.match(line) >= 0:
    	    return path_pat.group(1)
    return "Mail"

def serve(c):
    cr = c.makefile("r")
    cw = c.makefile("w")
    writec(cw, "+OK Python POP server ready")
    state = 1
    while 1:
        cmd = readc(cr)
        if not cmd:
    	    sys.exit(0)
    	elif quit_pat.match(cmd) >= 0:
    	    writec(cw, "+OK Bye!")
    	    if state == 3:
    	    	lastf = open("last_pop_retr", "w")
    	    	lastf.write("%d\n" % last)
    	    	lastf.close()
    	    sys.exit(0)
        elif state == 1 and user_pat.match(cmd) >= 0:
            (user, folder) = user_pat.group(1, 2)
            try: pe = pwd.getpwnam(user)
            except:
                writec(cw, "-ERR Unknown user...");
	    else:
		os.setgid(pe[3])
	        os.setuid(pe[2])
	        try:
    	    	    os.chdir(pe[5]); os.chdir(mh_dir()); os.chdir(folder)
		except:
		    writec(cw, "-ERR Can't cd to MH folder directory...")
		else:
		    msg = "+OK Ready in %(user)s's %(folder)s folder..."
		    writec(cw, msg % vars())
		    state = 2
        elif state == 2 and pass_pat.match(cmd) >= 0:
	    password = pass_pat.group(1)
    	    if len(password) < 2 or crypt(password, pe[1][0:2]) != pe[1]:
    	    	writec(cw, "-ERR Bad password...")
    	    else:
    	    	writec(cw, "+OK I believe you're you...")
    	    	state = 3
    	    	files = os.listdir(".")
    	    	msgs = filter(lambda n: regex.match("[0-9]+$", n) >= 0, files)
    	    	msg_nums = map(eval, msgs)
    	    	msg_nums.sort()
    	    	msgs = map(repr, msg_nums)
       	    	try:
    	    	    lastf = open("last_pop_retr", "r")
    	    	    last = eval(lastf.readline())
    	    	    lastf.close()
    	    	except:
    	    	    last = 0
        elif state == 3 and stat_pat.match(cmd) >= 0:
    	    size = reduce(lambda n, f: n + os.stat(f)[6], msgs, 0)
    	    writec(cw, "+OK %d %d" % (len(msg_nums), size))
        elif state == 3 and last_pat.match(cmd) >= 0:
    	    if last in msg_nums:
    	    	i = msg_nums.index(last) + 1
    	    else:
    	    	last = i = 0;
    	    writec(cw, "+OK %d" % (i,))
        elif state == 3 and top_pat.match(cmd) >= 0:
    	    (msg, lines) = top_pat.group(1, 2)
    	    msg = msg_nums[eval(msg) - 1]
    	    writec(cw, "+OK")
    	    if msg > last:
    	    	writec(cw, "Recent: Yes!")
    	    else:
    	    	writec(cw, "Status: RO")
    	    writec(cw, "", 1)
    	    writec(cw, ".", 1)
        elif state == 3 and list_pat.match(cmd) >= 0:
    	    msg = list_pat.group(1)
    	    i = eval(msg) - 1
    	    size = os.stat(msgs[i])[6]
    	    writec(cw, "+OK %s %d" % (msg, size))
        elif state == 3 and retr_pat.match(cmd) >= 0:
    	    msg = retr_pat.group(1)
    	    i = eval(msg) - 1
    	    f = open(msgs[i], "r")
    	    last = msg_nums[i]
    	    writec(cw, "+OK")
    	    for line in f.readlines():
    	    	line = line[:-1]
    	    	if line and line[0] == '.':
    	    	    writec(cw, '.' + line, 1)
    	    	else:
    	    	    writec(cw, line, 1)
    	    writec(cw, ".")
        else:
            writec(cw, "-ERR Unknown or mis-timed command...")

s = socket(AF_INET, SOCK_STREAM)
s.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
s.bind('', 110)
s.listen(5)
while 1:
    sys.stderr.write("Waiting for a connection...\n")
    (client, address) = s.accept()
    sys.stderr.write("Accepted a connection from %s on %d\n"
    	    	     % (`address`, s.fileno()))
    if not os.fork():
	if not os.fork():
	        # child, soon to be an orphan
	        serve(client)
	        os._exit(0)
	# child, here to allow orphaning the real child.
	os._exit(0)
    client.close()
