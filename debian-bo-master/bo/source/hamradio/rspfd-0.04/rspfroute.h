/*
 * rspfroute.h : Functions asscoaited with playing around with routing table
 */
 
void del_route(u_long daddr, u_char sigbits);
void nuke_routes(void);
void add_route(u_long daddr, u_char sigbits, u_long gateway, u_char cost);
struct rtentry *get_route(u_long addr, u_char sigbits);
