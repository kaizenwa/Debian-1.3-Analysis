/*
 * rpcmisc	Support for RPC startup and shutdown.
 *
 */

#ifndef RPCMISC_H
#define RPCMISC_H

extern int	_rpcpmstart;
extern int	_rpcfdtype;
extern int	_rpcsvcdirty;

extern _PRO (void rpc_init, (char *name, int prog, int *verstbl,
				void (*dispatch)(),
				int defport, int bufsize)		);
extern _PRO (void rpc_exit, (int prog, int *verstbl)			);
extern _PRO (void rpc_closedown, (void)					);

#endif /* RPCMISC_H */
