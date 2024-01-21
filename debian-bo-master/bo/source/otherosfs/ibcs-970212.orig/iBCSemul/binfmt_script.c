/*
 *  linux/ibcs/binfmt_script.c
 *
 *  Copyright 1996  Mike Jagdis (jaggy@purplet.demon.co.uk)
 *
 * $Id: binfmt_script.c,v 1.1 1996/04/25 11:22:01 mike Exp $
 * $Source: /usr/CVS/ibcs/iBCSemul/binfmt_script.c,v $
 */

#include <linux/config.h>

#include <linux/module.h>
#include <linux/version.h>

#include <linux/string.h>
#include <linux/stat.h>
#include <linux/malloc.h>
#include <linux/binfmts.h>


static int load_script(struct linux_binprm *bprm,struct pt_regs *regs);

struct linux_binfmt ibcs_script_format = {
	NULL, &mod_use_count_, load_script, NULL, NULL
};


static int
do_load_script(struct linux_binprm *bprm, struct pt_regs *regs)
{
	char *sh_arg = "sh";
	int retval;

	if (bprm->sh_bang || (bprm->buf[0] == '#' && bprm->buf[1] == '!'))
		return -ENOEXEC;

	if ((bprm->buf[0] != ':')
	|| (bprm->buf[1] != '\n' && bprm->buf[1] != ' ' && bprm->buf[1] != '\t')) {
		/* Not an obvious script. If the header seems to consist
		 * of nothing but printable ASCII (plus nulls since the
		 * buffer was cleared initially and the script may be
		 * smaller) we will assume it is a script. This may break
		 * loaders below this one. Tough.
		 */
		int i;

		for (i=0; i<sizeof(bprm->buf); i++)
			if ((bprm->buf[i] < ' ' || bprm->buf[i] > '~')
			&& bprm->buf[i] != '\0'
			&& bprm->buf[i] != '\n'
			&& bprm->buf[i] != '\t')
				return -ENOEXEC;
	}

	bprm->sh_bang++;
	iput(bprm->inode);
	bprm->dont_iput=1;

	remove_arg_zero(bprm);
	bprm->p = copy_strings(1, &bprm->filename, bprm->page, bprm->p, 2);
	bprm->argc++;
	bprm->p = copy_strings(1, &sh_arg, bprm->page, bprm->p, 2);
	bprm->argc++;
	if (!bprm->p) 
		return -E2BIG;

	retval = open_namei("/bin/sh", 0, 0, &bprm->inode, NULL);
	if (retval)
		return retval;
	bprm->dont_iput=0;
	retval=prepare_binprm(bprm);
	if(retval<0)
		return retval;
	return search_binary_handler(bprm,regs);
}

static int load_script(struct linux_binprm *bprm,struct pt_regs *regs)
{
	int retval;
	MOD_INC_USE_COUNT;
	retval = do_load_script(bprm,regs);
	MOD_DEC_USE_COUNT;
	return retval;
}
