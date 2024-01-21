import nis, string

error = "error"

def username(passport):
	if not passport.has_key('sunrpc-unix'):
		raise error, 'no Sun RPC UNIX identity in ILU passport'
	SunRPCAuth = passport['sunrpc-unix']
	map = nis.cat('hosts.byaddr')
	if not map:
		raise error, 'no YP map "hosts.byaddr"'
	map = nis.cat('passwd.byuid')
	if not map:
		raise error, 'no YP map "passwd.byuid"'
	uid = str(SunRPCAuth['uid'])
	if not map.has_key(uid):
		raise error, 'unknown user ID ' + uid
	record = string.splitfields(map[uid], ':')
	return (record[4])

