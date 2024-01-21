/* Debian local customizations */

# undef SYS_DELIVER
# undef POST_DELIVER
# undef ERR_DELIVER

# define SYS_DELIVER	"/etc/deliver/sys"
# define POST_DELIVER	"/etc/deliver/post"
# define ERR_DELIVER	"/etc/deliver/err"

# undef LOG
# undef ERRLOG

# define LOG		"/var/log/deliver.log"
# define ERRLOG		"/var/log/deliver.errlog"

# undef MBX_DIR
# undef MBX_MODE
# undef MBX_GROUP

# define MBX_DIR	"/var/spool/mail"
# define MBX_MODE	0660
# define MBX_GROUP	"mail"
