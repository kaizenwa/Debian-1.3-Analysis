
#include <cmpinclude.h>
#include "debug.h"
init_debug(){do_init(VV);}
/*	function definition for SHOW-BREAK-VARIABLES	*/

static L1()
{register object *base=vs_base;
	register object *sup=base+VM1; VC1
	vs_reserve(VM1);
	{register object V1;
	if(vs_top-vs_base>1) too_many_arguments();
	if(vs_base>=vs_top){vs_top=sup;goto T1;}
	V1=(base[0]);
	vs_top=sup;
	goto T2;
T1:;
	V1= small_fixnum(1);
T2:;
T5:;
	{register object V2;
	register object V3;
	V2= reverse(car(symbol_value(VV[0])));
	V3= car((V2));
T11:;
	if(!(endp((V2)))){
	goto T12;}
	goto T7;
T12:;
	base[3]= symbol_value(VV[1]);
	base[4]= VV[2];
	base[5]= car((V3));
	base[6]= cadr((V3));
	vs_top=(vs_base=base+3)+4;
	Lformat();
	vs_top=sup;
	V2= cdr((V2));
	V3= car((V2));
	goto T11;}
T7:;
	V1= number_plus((V1),small_fixnum(-1));
	if(!((fix((V1)))>(0))){
	goto T27;}
	goto T26;
T27:;
	vs_base=vs_top=base+1;
	vs_base[0]=Cnil;
	return;
T26:;
	vs_base=vs_top;
	(void) (*Lnk107)();
	vs_top=sup;
	goto T5;
	}
}
/*	local entry for function SHOW-ENVIRONMENT	*/

static object LI2(V5)

object V5;
{	 VMB2 VMS2 VMV2
TTL:;
	{object V6;
	base[1]= (V5);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	base[0]= vs_base[0];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V6= vs_base[0];
	if(!(type_of((V6))==t_cons||((V6))==Cnil)){
	goto T35;}
	{register object V7;
	register object V8;
	base[3]= (V5);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	base[2]= vs_base[0];
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V9= vs_base[0];
	V7= reverse(V9);
	V8= car((V7));
T43:;
	if(!(endp((V7)))){
	goto T44;}
	{object V10 = Cnil;
	VMR2(V10)}
T44:;
	base[2]= symbol_value(VV[1]);
	base[3]= VV[3];
	base[4]= car((V8));
	base[5]= cadr((V8));
	vs_top=(vs_base=base+2)+4;
	Lformat();
	vs_top=sup;
	V7= cdr((V7));
	V8= car((V7));
	goto T43;}
T35:;
	{object V11 = Cnil;
	VMR2(V11)}}
}
/*	function definition for SEARCH-STACK	*/

static L3()
{register object *base=vs_base;
	register object *sup=base+VM3; VC3
	vs_reserve(VM3);
	{object V12;
	check_arg(1);
	V12=(base[0]);
	vs_top=sup;
TTL:;
	{object V13;
	V13= Cnil;
	if(!(type_of((V12))==t_symbol)){
	goto T62;}
	V13= symbol_name((V12));
	goto T60;
T62:;
	V13= (V12);
T60:;
	{register int V14;
	register int V15;
	{object V16;
	vs_base=vs_top;
	(void) (*Lnk110)();
	vs_top=sup;
	V16= vs_base[0];{object V17;
	V17= (type_of((V16))==t_fixnum?Ct:Cnil);
	if(V17==Cnil)goto T67;
	goto T66;
T67:;}
	(void)((*(LnkLI111))());
T66:;
	V14= fix((V16));}
	V15= 2;
	{register object V18;
	V18= Cnil;
	{register object V19;
	V19= Cnil;
T71:;
	if((V14)<=(V15)){
	goto T74;}
	goto T73;
T74:;
	goto T72;
T73:;
	{register object V20;
	base[1]= make_fixnum(V14);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk112)();
	vs_top=sup;
	V20= vs_base[0];
	V18= (V20);}
	base[1]= (V18);
	vs_top=(vs_base=base+1)+1;
	Lcompiled_function_p();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T82;}
	base[1]= (V18);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk113)();
	vs_top=sup;
	V19= vs_base[0];
	goto T80;
T82:;
	if(!(type_of((V18))==t_symbol)){
	goto T88;}
	V19= (V18);
	goto T80;
T88:;
	if(!(type_of((V18))==t_cons||((V18))==Cnil)){
	goto T92;}
	{register object x= car((V18)),V21= VV[4];
	while(!endp(V21))
	if(eql(x,V21->c.c_car)){
	goto T95;
	}else V21=V21->c.c_cdr;
	goto T92;}
T95:;
	V19= cadr((V18));
	goto T80;
T92:;
	V19= VV[5];
T80:;
	V22= symbol_name((V19));
	if(((VFUN_NARGS=4,(*(LnkLI114))((V13),/* INLINE-ARGS */V22,VV[6],VV[7])))==Cnil){
	goto T98;}
	base[1]= make_fixnum(V14);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk115)();
	vs_top=sup;
	princ_char(10,Cnil);
	vs_base=vs_top;
	(void) (*Lnk116)();
	return;
T98:;
	V14= (V14)-(1);
	goto T71;
T72:;
	base[1]= symbol_value(VV[1]);
	base[2]= VV[8];
	base[3]= (V13);
	vs_top=(vs_base=base+1)+3;
	Lformat();
	vs_top=sup;
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;}}}}
	}
}
/*	function definition for BREAK-LOCALS	*/

static L4()
{register object *base=vs_base;
	register object *sup=base+VM4; VC4
	vs_reserve(VM4);
	bds_check;
	{object V23;
	if(vs_top-vs_base>1) too_many_arguments();
	if(vs_base>=vs_top){vs_top=sup;goto T111;}
	V23=(base[0]);
	vs_top=sup;
	goto T112;
T111:;
	V23= small_fixnum(1);
T112:;
	{object V24;
	object V25;
	object V26;
	object V27;
	object V28;
	object V29;
	V24= symbol_value(VV[9]);
	base[6]= (V24);
	vs_top=(vs_base=base+6)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	V25= vs_base[0];
	bds_bind(VV[11],symbol_value(VV[10]));
	bds_bind(VV[12],Cnil);
	bds_bind(VV[13],symbol_value(VV[10]));
	V26= symbol_value(VV[9]);
	base[6]= (V24);
	vs_top=(vs_base=base+6)+1;
	(void) (*Lnk112)();
	vs_top=sup;
	V27= vs_base[0];
	V28= Cnil;
	V29= Cnil;
	if(!((fix((V23)))>(1))){
	goto T123;}
	{register int V30;
	int V31;
	V30= 0;{object V32;
	V32= (type_of((V23))==t_fixnum?Ct:Cnil);
	if(V32==Cnil)goto T128;
	goto T127;
T128:;}
	(void)((*(LnkLI111))());
T127:;
	V31= fix((V23));
	{register int V33;
	int V34;{object V35;
	V35= (type_of((V26))==t_fixnum?Ct:Cnil);
	if(V35==Cnil)goto T132;
	goto T131;
T132:;}
	(void)((*(LnkLI111))());
T131:;
	V33= fix((V26));
	V34= 2;
T136:;
	if((V33)<=(V34)){
	goto T139;}
	goto T138;
T139:;
	goto T137;
T138:;
	if((V30)>=(V31)){
	goto T142;}
	goto T141;
T142:;
	goto T137;
T141:;
	bds_bind(VV[9],make_fixnum(V33));
	vs_base=vs_top;
	(void) (*Lnk116)();
	vs_top=sup;
	princ_char(10,Cnil);
	princ_char(10,Cnil);
	bds_unwind1;
	V30= (V30)+(1);
	V33= (V33)-(1);
	goto T136;
T137:;
	base[6]= Cnil;
	vs_top=(vs_base=base+6)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;
	base[6]= Cnil;
	vs_top=(vs_base=base+6)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;}}
T123:;
	base[6]= (V27);
	vs_top=(vs_base=base+6)+1;
	Lcompiled_function_p();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T154;}
	base[6]= (V27);
	vs_top=(vs_base=base+6)+1;
	(void) (*Lnk113)();
	vs_top=sup;
	V28= vs_base[0];
	goto T152;
T154:;
	V28= (V27);
T152:;
	if(!(type_of((V28))==t_symbol)){
	goto T160;}
	V29= get((V28),VV[14],Cnil);
T160:;
	{object V36;
	base[6]= make_fixnum((1)+(fix(symbol_value(VV[9]))));
	vs_top=(vs_base=base+6)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	V36= vs_base[0];
	if(((V36))==Cnil){
	goto T167;}
	base[6]= symbol_value(VV[1]);
	base[7]= VV[15];
	base[8]= (V28);
	vs_top=(vs_base=base+6)+3;
	Lformat();
	vs_top=sup;
	if(!(type_of((V28))==t_symbol)){
	goto T174;}
	{register int V37;
	int V38;{object V39;
	V39= (type_of((V25))==t_fixnum?Ct:Cnil);
	if(V39==Cnil)goto T178;
	goto T177;
T178:;}
	(void)((*(LnkLI111))());
T177:;
	V37= fix((V25));{object V40;
	V40= (type_of((V36))==t_fixnum?Ct:Cnil);
	if(V40==Cnil)goto T182;
	goto T181;
T182:;}
	(void)((*(LnkLI111))());
T181:;
	V38= fix((V36));
	{register int V41;
	V41= 0;
	{register object V42;
	V42= Cnil;
T186:;
	if((V37)>=(V38)){
	goto T189;}
	goto T188;
T189:;
	goto T187;
T188:;
	V42= Cnil;
	{register object x= small_fixnum(0),V43= (V29);
	while(!endp(V43))
	if(eql(x,V43->c.c_car)){
	goto T196;
	}else V43=V43->c.c_cdr;
	goto T195;}
T196:;
	base[6]= (V29);
	base[7]= make_fixnum(V41);
	vs_top=(vs_base=base+6)+2;
	Lgetf();
	vs_top=sup;
	V42= vs_base[0];
	goto T193;
T195:;
	V42= nth(V41,(V29));
T193:;
	if(((V42))==Cnil){
	goto T203;}
	base[6]= Ct;
	base[7]= VV[16];
	base[8]= make_fixnum(V41);
	base[9]= (V42);
	base[11]= make_fixnum(V37);
	vs_top=(vs_base=base+11)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	base[10]= vs_base[0];
	vs_top=(vs_base=base+6)+5;
	Lformat();
	vs_top=sup;
	goto T201;
T203:;
	base[6]= symbol_value(VV[1]);
	base[7]= VV[17];
	base[8]= make_fixnum(V41);
	base[10]= make_fixnum(V37);
	vs_top=(vs_base=base+10)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	base[9]= vs_base[0];
	vs_top=(vs_base=base+6)+4;
	Lformat();
	vs_top=sup;
T201:;
	V37= (V37)+(1);
	if(!((V41)<(2147483646))){
	goto T219;}
	goto T218;
T219:;
	(void)((*(LnkLI111))());
T218:;
	V41= (V41)+(1);
	goto T186;
T187:;
	base[6]= Cnil;
	vs_top=(vs_base=base+6)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;
	base[6]= Cnil;
	vs_top=(vs_base=base+6)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;}}}
T174:;
	if(!(type_of((V28))==t_cons||((V28))==Cnil)){
	goto T225;}
	base[6]= (*(LnkLI117))((V24));
	vs_top=(vs_base=base+6)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;
T225:;
	base[6]= symbol_value(VV[1]);
	base[7]= VV[19];
	vs_top=(vs_base=base+6)+2;
	Lformat();
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;
T167:;
	base[6]= Cnil;
	vs_top=(vs_base=base+6)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;}}
	}
}
/*	function definition for LOC	*/

static L5()
{register object *base=vs_base;
	register object *sup=base+VM5; VC5
	vs_reserve(VM5);
	{object V44;
	if(vs_top-vs_base>1) too_many_arguments();
	if(vs_base>=vs_top){vs_top=sup;goto T229;}
	V44=(base[0]);
	vs_top=sup;
	goto T230;
T229:;
	V44= small_fixnum(0);
T230:;
	{object V45;
	base[1]= symbol_value(VV[9]);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	V45= vs_base[0];
	if(!((fix((V44)))>=(0))){
	goto T235;}
	vs_base=vs_top;
	(void) (*Lnk110)();
	vs_top=sup;
	V47= vs_base[0];
	V48 = make_fixnum((1)+(fix(symbol_value(VV[9]))));
	base[1]= (number_compare(V47,V48)<=0?(V47):V48);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	V46= vs_base[0];
	if((fix((V44)))<((fix(V46))-(fix((V45))))){
	goto T234;}
T235:;
	base[1]= VV[20];
	vs_top=(vs_base=base+1)+1;
	Lerror();
	vs_top=sup;
T234:;
	base[1]= make_fixnum((fix((V44)))+(fix((V45))));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk109)();
	return;}
	}
}
/*	macro definition for MV-SETQ	*/

static L6()
{register object *base=vs_base;
	register object *sup=base+VM6; VC6
	vs_reserve(VM6);
	check_arg(2);
	vs_top=sup;
	{object V49=base[0]->c.c_cdr;
	if(endp(V49))invalid_macro_call();
	base[2]= (V49->c.c_car);
	V49=V49->c.c_cdr;
	if(endp(V49))invalid_macro_call();
	base[3]= (V49->c.c_car);
	V49=V49->c.c_cdr;
	if(!endp(V49))invalid_macro_call();}
	V50= list(3,VV[22],car(base[2]),base[3]);
	{register object V52;
	register object V53;
	register object V54;
	V52= cdr(base[2]);
	V53= small_fixnum(0);
	V54= Cnil;
T247:;
	if(((V52))!=Cnil){
	goto T248;}
	V51= reverse((V54));
	goto T244;
T248:;
	V55= car((V52));
	V56= list(3,VV[22],/* INLINE-ARGS */V55,list(2,VV[23],(V53)));
	V54= make_cons(/* INLINE-ARGS */V56,(V54));
	V52= cdr((V52));
	V53= one_plus((V53));
	goto T247;}
T244:;
	base[4]= listA(3,VV[21],/* INLINE-ARGS */V50,V51);
	vs_top=(vs_base=base+4)+1;
	return;
}
/*	macro definition for MV-VALUES	*/

static L7()
{register object *base=vs_base;
	register object *sup=base+VM7; VC7
	vs_reserve(VM7);
	check_arg(2);
	vs_top=sup;
	{object V57=base[0]->c.c_cdr;
	base[2]= V57;}
	V58= car(base[2]);
	{register object V60;
	register object V61;
	register object V62;
	V60= cdr(base[2]);
	V61= small_fixnum(0);
	V62= Cnil;
T261:;
	if(((V60))!=Cnil){
	goto T262;}
	V59= reverse((V62));
	goto T258;
T262:;
	V63= list(3,VV[24],(V61),car((V60)));
	V62= make_cons(/* INLINE-ARGS */V63,(V62));
	V60= cdr((V60));
	V61= one_plus((V61));
	goto T261;}
T258:;
	base[3]= listA(3,VV[21],/* INLINE-ARGS */V58,V59);
	vs_top=(vs_base=base+3)+1;
	return;
}
/*	function definition for DBL	*/

static L8()
{register object *base=vs_base;
	register object *sup=base+VM8; VC8
	vs_reserve(VM8);
	check_arg(0);
	vs_top=sup;
TTL:;
	base[0]= Cnil;
	base[1]= Cnil;
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk118)();
	return;
}
/*	function definition for MAKE-INSTREAM	*/

static L9()
{register object *base=vs_base;
	register object *sup=base+VM9; VC9
	vs_reserve(VM9);
	{object V64;
	object V65;
	object V66;
	parse_key(vs_base,FALSE,FALSE,3,VV[27],VV[36],VV[119]);
	vs_top=sup;
	V64=(base[0]);
	if(base[4]==Cnil){
	V65= small_fixnum(0);
	}else{
	V65=(base[1]);}
	V66=(base[2]);
	base[6]= VV[25];
	base[7]= (V64);
	base[8]= (V65);
	base[9]= (V66);
	vs_top=(vs_base=base+6)+4;
	siLmake_structure();
	return;
	}
}
/*	function definition for CLEANUP	*/

static L10()
{register object *base=vs_base;
	register object *sup=base+VM10; VC10
	vs_reserve(VM10);
	check_arg(0);
	vs_top=sup;
TTL:;
	{register object V67;
	register object V68;
	V67= symbol_value(VV[26]);
	V68= car((V67));
T282:;
	if(!(endp((V67)))){
	goto T283;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T283:;
	base[1]= structure_ref((V68),VV[25],0);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk120)();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T287;}
	setq(VV[26],(VFUN_NARGS=2,(*(LnkLI121))((V68),symbol_value(VV[26]))));
T287:;
	V67= cdr((V67));
	V68= car((V67));
	goto T282;}
}
/*	function definition for GET-INSTREAM	*/

static L11()
{register object *base=vs_base;
	register object *sup=base+VM11; VC11
	vs_reserve(VM11);
	{register object V69;
	check_arg(1);
	V69=(base[0]);
	vs_top=sup;
TTL:;{object V70;
	{register object V71;
	register object V72;
	V71= symbol_value(VV[26]);
	V72= car((V71));
T302:;
	if(!(endp((V71)))){
	goto T303;}
	V70= Cnil;
	goto T298;
T303:;
	V73= structure_ref((V72),VV[25],0);
	if(!(((V69))==(/* INLINE-ARGS */V73))){
	goto T307;}
	V70= (V72);
	goto T298;
T307:;
	V71= cdr((V71));
	V72= car((V71));
	goto T302;}
T298:;
	if(V70==Cnil)goto T297;
	base[1]= V70;
	vs_top=(vs_base=base+1)+1;
	return;
T297:;}
	base[1]= VV[27];
	base[2]= (V69);
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk122)();
	vs_top=sup;
	V74= vs_base[0];
	setq(VV[26],make_cons(V74,symbol_value(VV[26])));
	base[1]= car(symbol_value(VV[26]));
	vs_top=(vs_base=base+1)+1;
	return;
	}
}
/*	function definition for NEWLINE	*/

static L12()
{register object *base=vs_base;
	register object *sup=base+VM12; VC12
	vs_reserve(VM12);
	{object V75;
	object V76;
	check_arg(2);
	V75=(base[0]);
	V76=(base[1]);
	vs_top=sup;
TTL:;
	{object V77;
	base[2]= (V75);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk123)();
	vs_top=sup;
	V77= vs_base[0];
	{int V78= fix(structure_ref((V77),VV[25],1));
	V79 = make_fixnum((1)+(/* INLINE-ARGS */V78));
	(void)(structure_set((V77),VV[25],1,V79));}}
	base[2]= Cnil;
	base[3]= (V75);
	base[4]= Cnil;
	vs_top=(vs_base=base+2)+3;
	Lpeek_char();
	vs_top=sup;
	V80= vs_base[0];
	if(!(eql(V80,VV[29]))){
	goto T326;}
	setq(VV[28],VV[30]);
	goto T324;
T326:;
	setq(VV[28],Ct);
T324:;
	vs_base=vs_top=base+2;
	vs_base[0]=Cnil;
	return;
	}
}
/*	function definition for QUOTATION-READER	*/

static L13()
{register object *base=vs_base;
	register object *sup=base+VM13; VC13
	vs_reserve(VM13);
	{object V81;
	object V82;
	check_arg(2);
	V81=(base[0]);
	V82=(base[1]);
	vs_top=sup;
TTL:;
	{object V83;
	object V84;
	V83= (
	(type_of(symbol_value(VV[31])) == t_sfun ?(*(object (*)())((symbol_value(VV[31]))->sfn.sfn_self)):
	(fcall.fun=(symbol_value(VV[31])),fcall.argd=2,fcalln))((V81),(V82)));
	base[2]= (V81);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk123)();
	vs_top=sup;
	V84= vs_base[0];
	{object V86;
	V87= structure_ref(V84,VV[25],1);
	V88= (VFUN_NARGS=2,(*(LnkLI124))(VV[32],(V83)));
	V86= number_plus(/* INLINE-ARGS */V87,/* INLINE-ARGS */V88);
	(void)(structure_set(V84,VV[25],1,(V86)));}
	base[2]= (V83);
	vs_top=(vs_base=base+2)+1;
	return;}
	}
}
/*	function definition for NEW-SEMI-COLON-READER	*/

static L14()
{register object *base=vs_base;
	register object *sup=base+VM14; VC14
	vs_reserve(VM14);
	bds_check;
	{register object V89;
	object V90;
	check_arg(2);
	V89=(base[0]);
	V90=(base[1]);
	vs_top=sup;
TTL:;
	{register object V91;
	object V92;
	base[2]= (V89);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk123)();
	vs_top=sup;
	V91= vs_base[0];
	base[2]= Cnil;
	base[3]= (V89);
	base[4]= Cnil;
	base[5]= Cnil;
	vs_top=(vs_base=base+2)+4;
	Lpeek_char();
	vs_top=sup;
	V92= vs_base[0];
	{int V93= fix(structure_ref((V91),VV[25],1));
	V94 = make_fixnum((1)+(/* INLINE-ARGS */V93));
	(void)(structure_set((V91),VV[25],1,V94));}
	if(!(eql((V92),VV[33]))){
	goto T347;}
	base[2]= (V89);
	vs_top=(vs_base=base+2)+1;
	Lread_char();
	vs_top=sup;
	{register object V95;
	bds_bind(VV[34],symbol_value(VV[35]));
	base[5]= (V89);
	base[6]= Cnil;
	base[7]= Cnil;
	vs_top=(vs_base=base+5)+3;
	Lread_line();
	vs_top=sup;
	base[4]= vs_base[0];
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk125)();
	vs_top=sup;
	V95= vs_base[0];
	if(!(type_of((V95))==t_cons)){
	bds_unwind1;
	goto T345;}
	if(!((car((V95)))==(VV[36]))){
	bds_unwind1;
	goto T345;}
	if(!(type_of(cadr((V95)))==t_string)){
	bds_unwind1;
	goto T345;}
	if(!(type_of(caddr((V95)))==t_fixnum)){
	bds_unwind1;
	goto T345;}
	(void)(structure_set((V91),VV[25],2,cadr((V95))));
	V96= structure_set((V91),VV[25],1,caddr((V95)));
	bds_unwind1;
	goto T345;}
T347:;
	(void)((
	(type_of(symbol_value(VV[37])) == t_sfun ?(*(object (*)())((symbol_value(VV[37]))->sfn.sfn_self)):
	(fcall.fun=(symbol_value(VV[37])),fcall.argd=2,fcalln))((V89),(V90))));
T345:;
	base[2]= Cnil;
	base[3]= (V89);
	base[4]= Cnil;
	vs_top=(vs_base=base+2)+3;
	Lpeek_char();
	vs_top=sup;
	V97= vs_base[0];
	if(!(eql(V97,VV[29]))){
	goto T368;}
	setq(VV[28],VV[30]);
	goto T366;
T368:;
	setq(VV[28],Ct);
T366:;
	vs_base=vs_top=base+2;
	vs_base[0]=Cnil;
	return;}
	}
}
/*	function definition for SETUP-LINEINFO	*/

static L15()
{register object *base=vs_base;
	register object *sup=base+VM15; VC15
	vs_reserve(VM15);
	check_arg(0);
	vs_top=sup;
TTL:;
	base[0]= VV[32];
	base[1]= symbol_function(VV[126]);
	base[2]= Cnil;
	base[3]= symbol_value(VV[38]);
	vs_top=(vs_base=base+0)+4;
	Lset_macro_character();
	vs_top=sup;
	base[0]= VV[39];
	base[1]= symbol_function(VV[127]);
	base[2]= Cnil;
	base[3]= symbol_value(VV[38]);
	vs_top=(vs_base=base+0)+4;
	Lset_macro_character();
	vs_top=sup;
	base[0]= VV[29];
	base[1]= VV[40];
	base[2]= Cnil;
	base[3]= symbol_value(VV[38]);
	vs_top=(vs_base=base+0)+4;
	Lset_macro_character();
	vs_top=sup;
	base[0]= VV[41];
	base[1]= VV[42];
	base[2]= Cnil;
	base[3]= symbol_value(VV[38]);
	vs_top=(vs_base=base+0)+4;
	Lset_macro_character();
	return;
}
/*	function definition for NLOAD	*/

static L16()
{register object *base=vs_base;
	register object *sup=base+VM16; VC16
	vs_reserve(VM16);
	bds_check;
	{object V98;
	object V99;
	if(vs_top-vs_base<1) too_few_arguments();
	V98=(base[0]);
	vs_base=vs_base+1;
	vs_top[0]=Cnil;
	{object *p=vs_top;
	 for(;p>vs_base;p--)p[-1]=MMcons(p[-1],p[0]);}
	V99=(base[1]);
	vs_top=sup;
	base[2]= symbol_value(VV[43]);
	vs_top=(vs_base=base+2)+1;
	Lclrhash();
	vs_top=sup;
	vs_base=vs_top;
	(void) (*Lnk128)();
	vs_top=sup;
	base[2]= (V98);
	vs_top=(vs_base=base+2)+1;
	Ltruename();
	vs_top=sup;
	V98= vs_base[0];
	vs_base=vs_top;
	(void) (*Lnk129)();
	vs_top=sup;
	bds_bind(VV[34],symbol_value(VV[38]));
	base[3]=symbol_function(VV[130]);
	base[4]= (V98);
	{object V100;
	V100= (V99);
	 vs_top=base+5;
	 while(!endp(V100))
	 {vs_push(car(V100));V100=cdr(V100);}
	vs_base=base+4;}
	(void) (*Lnk130)();
	bds_unwind1;
	return;
	}
}
/*	function definition for LEFT-PARENTHESIS-READER	*/

static L17()
{register object *base=vs_base;
	register object *sup=base+VM17; VC17
	vs_reserve(VM17);
	{object V101;
	object V102;
	check_arg(2);
	V101=(base[0]);
	V102=(base[1]);
	vs_top=sup;
TTL:;
	{object V103;
	register object V104;
	V103= Cnil;
	V104= symbol_value(VV[28]);
	if(!((symbol_value(VV[28]))==(Ct))){
	goto T404;}
	setq(VV[28],Cnil);
T404:;
	if(((V104))==Cnil){
	goto T408;}
	base[2]= (V101);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk123)();
	vs_top=sup;
	V104= vs_base[0];
	V103= structure_ref((V104),VV[25],1);
T408:;
	{object V105;
	V105= (
	(type_of(symbol_value(VV[44])) == t_sfun ?(*(object (*)())((symbol_value(VV[44]))->sfn.sfn_self)):
	(fcall.fun=(symbol_value(VV[44])),fcall.argd=2,fcalln))((V101),(V102)));
	if(((V104))==Cnil){
	goto T416;}
	{object V107;
	object V108;
	V107= symbol_value(VV[43]);
	base[3]= (V104);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk131)();
	vs_top=sup;
	V109= vs_base[0];
	V108= make_cons(V109,(V103));
	base[3]= V105;
	base[4]= (V107);
	base[5]= (V108);
	vs_top=(vs_base=base+3)+3;
	siLhash_set();
	vs_top=sup;}
T416:;
	base[2]= (V105);
	vs_top=(vs_base=base+2)+1;
	return;}}
	}
}
/*	function definition for WALK-THROUGH	*/

static L18()
{register object *base=vs_base;
	register object *sup=base+VM18; VC18
	vs_reserve(VM18);
	{register object V110;
	check_arg(1);
	V110=(base[0]);
	vs_top=sup;
TTL:;
	{register object V111;
	V111= Cnil;
T428:;
	if(!(type_of((V110))==t_cons)){
	goto T429;}
	base[1]= (V110);
	base[2]= symbol_value(VV[43]);
	vs_top=(vs_base=base+1)+2;
	Lgethash();
	vs_top=sup;
	V111= vs_base[0];
	if(((V111))==Cnil){
	goto T432;}
	if(!(type_of(car((V110)))==t_cons)){
	goto T439;}
	if((caar((V110)))==(VV[45])){
	goto T439;}
	base[1]= (V110);
	base[2]= symbol_value(VV[43]);
	vs_top=(vs_base=base+1)+2;
	Lremhash();
	vs_top=sup;
	{register object V112;
	register object V113;
	register object V114;
	V112= car((V110));
	V113= symbol_value(VV[43]);
	V114= (V111);
	base[2]= (V112);
	base[3]= (V113);
	base[4]= (V114);
	vs_top=(vs_base=base+2)+3;
	siLhash_set();
	vs_top=sup;
	goto T432;}
T439:;
	V115= make_cons((V111),(V110));
	(void)((VFUN_NARGS=2,(*(LnkLI132))(/* INLINE-ARGS */V115,symbol_value(VV[46]))));
T432:;
	base[1]= car((V110));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk133)();
	vs_top=sup;
	V110= cdr((V110));
	goto T428;
T429:;
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;}
	}
}
/*	function definition for COMPILER-DEF-HOOK	*/

static L19()
{register object *base=vs_base;
	register object *sup=base+VM19; VC19
	vs_reserve(VM19);
	{object V116;
	object V117;
	check_arg(2);
	V116=(base[0]);
	V117=(base[1]);
	vs_top=sup;
TTL:;
	{object V118;
	register int V119;
	int V120;
	V118= symbol_value(VV[46]);
	V119= 2147483647;
	V120= -1;
	if(!(type_of((V117))!=t_cons)){
	goto T460;}
	base[2]= remprop((V116),VV[48]);
	vs_top=(vs_base=base+2)+1;
	return;
T460:;
	if(!((symbol_value(VV[34]))==(symbol_value(VV[38])))){
	goto T463;}
	base[2]= symbol_value(VV[46]);
	base[3]= small_fixnum(0);
	vs_top=(vs_base=base+2)+2;
	siLfill_pointer_set();
	vs_top=sup;
	base[2]= (V117);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk133)();
	vs_top=sup;
	{object V121;
	register int V122;
	V121= make_fixnum(length((V118)));
	V122= 0;
T474:;
	V123 = make_fixnum(V122);
	if(!(number_compare(V123,(V121))>=0)){
	goto T475;}
	goto T470;
T475:;
	{register int V124;
	V125= aref1((V118),V122);
	V124= fix(cdar(/* INLINE-ARGS */V125));
	if(!((V124)>(V120))){
	goto T481;}
	V120= V124;
T481:;
	if(!((V124)<(V119))){
	goto T479;}
	V119= V124;}
T479:;
	V122= (V122)+1;
	goto T474;}
T470:;
	if(!((length(symbol_value(VV[46])))>(0))){
	goto T492;}
	{object V126;
	object V127;
	V128 = make_fixnum(((V120)-(V119))+(2));
	V126= (VFUN_NARGS=3,(*(LnkLI134))(V128,VV[49],VV[50]));
	V127= get((V116),VV[48],Cnil);
	V129= aref1((V118),0);
	V130= caar(/* INLINE-ARGS */V129);
	V131 = make_fixnum(V119);
	V132= make_cons(/* INLINE-ARGS */V130,V131);
	(void)(aset1((V126),0,/* INLINE-ARGS */V132));
	V119= (V119)-(1);
	{object V133;
	register object V134;
	V133= make_fixnum(length((V118)));
	V134= small_fixnum(0);
T503:;
	if(!(number_compare((V134),(V133))>=0)){
	goto T504;}
	goto T499;
T504:;
	{object V135;
	V135= aref1((V118),fixint((V134)));
	(void)(aset1((V126),(fix(cdar((V135))))-(V119),cdr((V135))));}
	V134= one_plus((V134));
	goto T503;}
T499:;
	(void)(sputprop((V116),VV[48],(V126)));
	if(((V127))==Cnil){
	goto T515;}
	{object V136;
	object V137;
	V136= get((V116),VV[51],Cnil);
	V138= aref1((V127),0);
	V137= cdr(/* INLINE-ARGS */V138);
	{register object V139;
	object V140;
	V139= (V136);
	V140= car((V139));
T522:;
	if(!(endp((V139)))){
	goto T523;}
	base[3]= Cnil;
	vs_top=(vs_base=base+3)+1;
	return;
T523:;
	{object V141;
	object V142;
	object V143;
	V141= aref1(symbol_value(VV[52]),fixint((V140)));
	{object V144= cdr(cdr(cdr((V141))));
	if((type_of(V144)!=t_cons) && ((V141)!= Cnil))
	 FEwrong_type_argument(Scons,V144);
	V142= (V144->c.c_car);}
	{object V146= cdr(cdr((V141)));
	if((type_of(V146)!=t_cons) && ((V141)!= Cnil))
	 FEwrong_type_argument(Scons,V146);
	V145= fix((V146->c.c_car));}
	V143= make_fixnum((V145)-(fix((V137))));{object V147;
	V147= symbol_value(VV[52]);
	base[6]= (V142);
	base[7]= (V126);
	base[8]= (V143);
	vs_top=(vs_base=base+6)+3;
	(void) (*Lnk135)();
	vs_top=sup;
	V148= vs_base[0];
	(void)(aset1(V147,fixint((V140)),V148));}}
	V139= cdr((V139));
	V140= car((V139));
	goto T522;}}
T515:;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;}
T492:;
	{object V149;
	V149= get((V116),VV[51],Cnil);
	base[2]= (V149);
	base[3]= VV[53];
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk136)();
	return;}
T463:;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;}
	}
}
/*	function definition for INSTREAM-NAME	*/

static L20()
{register object *base=vs_base;
	register object *sup=base+VM20; VC20
	vs_reserve(VM20);
	{object V150;
	check_arg(1);
	V150=(base[0]);
	vs_top=sup;
TTL:;{object V151;
	V151= structure_ref((V150),VV[25],2);
	if(V151==Cnil)goto T544;
	base[1]= V151;
	vs_top=(vs_base=base+1)+1;
	return;
T544:;}
	base[1]= structure_ref((V150),VV[25],0);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk137)();
	return;
	}
}
static object stream_name(str) object str;{return str->sm.sm_object1; }
/*	function definition for STREAM-NAME	*/

static L21()
{	object *old_base=vs_base;
	object x;
	check_arg(1);
	x=
	stream_name(
	vs_base[0]);
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=x;
}
static object closedp(str) object str;{return (str->sm.sm_fp==0 ? Ct :Cnil); }
/*	function definition for CLOSEDP	*/

static L22()
{	object *old_base=vs_base;
	object x;
	check_arg(1);
	x=
	closedp(
	vs_base[0]);
	vs_top=(vs_base=old_base)+1;
	vs_base[0]=x;
}
/*	function definition for FIND-LINE-IN-FUN	*/

static L23()
{register object *base=vs_base;
	register object *sup=base+VM21; VC21
	vs_reserve(VM21);
	{object V152;
	object V153;
	register object V154;
	register object V155;
	check_arg(4);
	V152=(base[0]);
	V153=(base[1]);
	V154=(base[2]);
	V155=(base[3]);
	vs_top=sup;
TTL:;
	{object V156;
	V156= Cnil;
	V156= get((V154),VV[48],Cnil);
	if(((V156))==Cnil){
	goto T551;}
	{register object V157;
	V157= (V156);
	if(((V157))==Cnil){
	goto T554;}
	{object V158;
	register object V159;
	V158= make_fixnum(length((V157)));
	V159= small_fixnum(0);
T559:;
	if(!(number_compare((V159),(V158))>=0)){
	goto T560;}
	base[5]= Cnil;
	vs_top=(vs_base=base+5)+1;
	return;
T560:;
	V160= aref1((V157),fixint((V159)));
	if(!(((V152))==(/* INLINE-ARGS */V160))){
	goto T564;}
	if(((V155))==Cnil){
	goto T567;}
	{register object V161;
	register object V162;
	V161= (V155);
	V162= number_minus(car((V161)),small_fixnum(1));
	if(type_of((V161))!=t_cons)FEwrong_type_argument(Scons,(V161));
	((V161))->c.c_car = (V162);}
	if(!((fix(car((V155))))>(0))){
	goto T567;}
	base[5]= VV[54];
	vs_top=(vs_base=base+5)+1;
	return;
T567:;
	base[6]= (V154);
	base[7]= (V157);
	base[8]= (V159);
	vs_top=(vs_base=base+6)+3;
	(void) (*Lnk135)();
	vs_top=sup;
	setq(VV[55],vs_base[0]);
	base[5]= symbol_value(VV[55]);
	base[6]= (V153);
	vs_top=(vs_base=base+5)+2;
	(void) (*Lnk118)();
	vs_top=sup;
	base[5]= VV[54];
	vs_top=(vs_base=base+5)+1;
	return;
T564:;
	V159= one_plus((V159));
	goto T559;}
T554:;
	base[4]= Cnil;
	vs_top=(vs_base=base+4)+1;
	return;}
T551:;
	base[4]= Cnil;
	vs_top=(vs_base=base+4)+1;
	return;}
	}
}
/*	function definition for CURRENT-STEP-FUN	*/

static L24()
{register object *base=vs_base;
	register object *sup=base+VM22; VC22
	vs_reserve(VM22);
	{object V163;
	if(vs_top-vs_base>1) too_many_arguments();
	if(vs_base>=vs_top){vs_top=sup;goto T586;}
	V163=(base[0]);
	vs_top=sup;
	goto T587;
T586:;
	vs_base=vs_top;
	(void) (*Lnk110)();
	vs_top=sup;
	V163= vs_base[0];
T587:;
	{register object V164;
	V164= one_minus((V163));
T591:;
	if(!((fix((V164)))<=(0))){
	goto T592;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T592:;
	{register object V165;
	base[1]= (V164);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk138)();
	vs_top=sup;
	V165= vs_base[0];
	if((get((V165),VV[48],Cnil))==Cnil){
	goto T596;}
	base[1]= (V165);
	vs_top=(vs_base=base+1)+1;
	return;}
T596:;
	V164= make_fixnum((fix((V164)))-(1));
	goto T591;}
	}
}
/*	function definition for INIT-BREAK-POINTS	*/

static L25()
{register object *base=vs_base;
	register object *sup=base+VM23; VC23
	vs_reserve(VM23);
	check_arg(0);
	vs_top=sup;
TTL:;
	base[0]= symbol_value(VV[56]);
	base[1]= small_fixnum(0);
	vs_top=(vs_base=base+0)+2;
	siLfill_pointer_set();
	vs_top=sup;
	setq(VV[52],symbol_value(VV[56]));
	base[0]= symbol_value(VV[52]);
	vs_top=(vs_base=base+0)+1;
	return;
}
/*	function definition for STEP-INTO	*/

static L26()
{register object *base=vs_base;
	register object *sup=base+VM24; VC24
	vs_reserve(VM24);
	{object V166;
	if(vs_top-vs_base>1) too_many_arguments();
	if(vs_base>=vs_top){vs_top=sup;goto T608;}
	V166=(base[0]);
	vs_top=sup;
	goto T609;
T608:;
	V166= small_fixnum(1);
T609:;
	if(symbol_value(VV[52])!=Cnil){
	goto T611;}
	vs_base=vs_top;
	(void) (*Lnk139)();
	vs_top=sup;
T611:;
	setq(VV[57],VV[58]);
	base[1]= VV[59];
	vs_top=(vs_base=base+1)+1;
	return;
	}
}
/*	function definition for STEP-NEXT	*/

static L27()
{register object *base=vs_base;
	register object *sup=base+VM25; VC25
	vs_reserve(VM25);
	{object V167;
	if(vs_top-vs_base>1) too_many_arguments();
	if(vs_base>=vs_top){vs_top=sup;goto T614;}
	V167=(base[0]);
	vs_top=sup;
	goto T615;
T614:;
	V167= small_fixnum(1);
T615:;
	{object V168;
	vs_base=vs_top;
	(void) (*Lnk140)();
	vs_top=sup;
	V168= vs_base[0];
	setq(VV[60],make_cons((V167),(V168)));
	if(symbol_value(VV[52])!=Cnil){
	goto T620;}
	vs_base=vs_top;
	(void) (*Lnk139)();
	vs_top=sup;
T620:;
	setq(VV[57],VV[61]);
	base[1]= VV[59];
	vs_top=(vs_base=base+1)+1;
	return;}
	}
}
/*	function definition for MAYBE-BREAK	*/

static L28()
{register object *base=vs_base;
	register object *sup=base+VM26; VC26
	vs_reserve(VM26);
	{object V169;
	object V170;
	object V171;
	object V172;
	check_arg(4);
	V169=(base[0]);
	V170=(base[1]);
	V171=(base[2]);
	V172=(base[3]);
	vs_top=sup;
TTL:;
	{object V173;
	V173= Cnil;
	V173= (VFUN_NARGS=2,(*(LnkLI141))((V169),(V170)));
	if(((V173))==Cnil){
	goto T625;}
	setq(VV[57],Cnil);
	if(!((length(symbol_value(VV[52])))>(0))){
	goto T631;}
	goto T630;
T631:;
	setq(VV[52],Cnil);
T630:;
	base[5]= (V171);
	base[6]= (V170);
	base[7]= (V173);
	vs_top=(vs_base=base+5)+3;
	(void) (*Lnk135)();
	vs_top=sup;
	base[4]= vs_base[0];
	base[5]= (V172);
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk118)();
	vs_top=sup;
	base[4]= Ct;
	vs_top=(vs_base=base+4)+1;
	return;
T625:;
	base[4]= Cnil;
	vs_top=(vs_base=base+4)+1;
	return;}
	}
}
/*	function definition for BREAK-STEP-INTO	*/

static L29()
{register object *base=vs_base;
	register object *sup=base+VM27; VC27
	vs_reserve(VM27);
	{object V174;
	object V175;
	check_arg(2);
	V174=(base[0]);
	V175=(base[1]);
	vs_top=sup;
TTL:;
	{object V176;
	vs_base=vs_top;
	(void) (*Lnk140)();
	vs_top=sup;
	V176= vs_base[0];
	{object V177;
	V177= get((V176),VV[48],Cnil);
	base[2]= (V174);
	base[3]= (V177);
	base[4]= (V176);
	base[5]= (V175);
	vs_top=(vs_base=base+2)+4;
	(void) (*Lnk142)();
	return;}}
	}
}
/*	function definition for BREAK-STEP-NEXT	*/

static L30()
{register object *base=vs_base;
	register object *sup=base+VM28; VC28
	vs_reserve(VM28);
	{object V178;
	object V179;
	check_arg(2);
	V178=(base[0]);
	V179=(base[1]);
	vs_top=sup;
TTL:;
	{object V180;
	vs_base=vs_top;
	(void) (*Lnk140)();
	vs_top=sup;
	V180= vs_base[0];
	if(!(eql(cdr(symbol_value(VV[60])),(V180)))){
	goto T648;}
	{object V181;
	V181= get((V180),VV[48],Cnil);
	base[2]= (V178);
	base[3]= (V181);
	base[4]= (V180);
	base[5]= (V179);
	vs_top=(vs_base=base+2)+4;
	(void) (*Lnk142)();
	return;}
T648:;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;}
	}
}
/*	function definition for *BREAK-POINTS*	*/

static L31()
{register object *base=vs_base;
	register object *sup=base+VM29; VC29
	vs_reserve(VM29);
	{object V182;
	object V183;
	check_arg(2);
	V182=(base[0]);
	V183=(base[1]);
	vs_top=sup;
TTL:;
	{object V184;
	V184= (VFUN_NARGS=4,(*(LnkLI141))((V182),symbol_value(VV[52]),VV[62],VV[63]));
	base[2]= Ct;
	base[3]= VV[64];
	base[4]= (V184);
	vs_top=(vs_base=base+2)+3;
	Lformat();
	vs_top=sup;
	base[2]= aref1(symbol_value(VV[52]),fixint((V184)));
	base[3]= (V183);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk118)();
	return;}
	}
}
/*	function definition for DWIM	*/

static L32()
{register object *base=vs_base;
	register object *sup=base+VM30; VC30
	vs_reserve(VM30);
	{register object V185;
	check_arg(1);
	V185=(base[0]);
	vs_top=sup;
TTL:;
	{register object V186;
	register object V187;
	vs_base=vs_top;
	Llist_all_packages();
	vs_top=sup;
	V186= vs_base[0];
	V187= car((V186));
T666:;
	if(!(endp((V186)))){
	goto T667;}
	goto T662;
T667:;
	{register object V188;
	register object V189;
	base[3]= symbol_name((V185));
	base[4]= (V187);
	vs_top=(vs_base=base+3)+2;
	Lintern();
	if(vs_base>=vs_top){vs_top=sup;goto T675;}
	V188= vs_base[0];
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T676;}
	V189= vs_base[0];
	vs_top=sup;
	goto T677;
T675:;
	V188= Cnil;
T676:;
	V189= Cnil;
T677:;
	if((get((V188),VV[48],Cnil))==Cnil){
	goto T679;}
	base[3]= (V188);
	vs_top=(vs_base=base+3)+1;
	return;
T679:;
	if((V189)!=Cnil){
	goto T671;}
	base[3]= (V188);
	vs_top=(vs_base=base+3)+1;
	Lunintern();
	vs_top=sup;}
T671:;
	V186= cdr((V186));
	V187= car((V186));
	goto T666;}
T662:;
	base[1]= Ct;
	base[2]= VV[65];
	base[3]= (V185);
	vs_top=(vs_base=base+1)+3;
	Lformat();
	return;
	}
}
/*	function definition for BREAK-FUNCTION	*/

static L33()
{register object *base=vs_base;
	register object *sup=base+VM31; VC31
	vs_reserve(VM31);
	{register object V190;
	register object V191;
	object V192;
	if(vs_top-vs_base<1) too_few_arguments();
	if(vs_top-vs_base>3) too_many_arguments();
	V190=(base[0]);
	vs_base=vs_base+1;
	if(vs_base>=vs_top){vs_top=sup;goto T690;}
	V191=(base[1]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T691;}
	V192=(base[2]);
	vs_top=sup;
	goto T692;
T690:;
	V191= small_fixnum(1);
T691:;
	V192= Cnil;
T692:;
	{object V193;
	V193= Cnil;
	{register object V194;
	V194= get((V190),VV[48],Cnil);
	if(((V194))!=Cnil){
	goto T697;}
	base[3]= (V190);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk143)();
	vs_top=sup;
	V193= vs_base[0];
	if(((V193))==Cnil){
	goto T697;}
	base[3]= (V193);
	base[4]= (V191);
	base[5]= (V192);
	vs_top=(vs_base=base+3)+3;
	(void) (*Lnk144)();
	return;
T697:;
	if(!(type_of((V194))==t_array||
type_of((V194))==t_vector||
type_of((V194))==t_string||
type_of((V194))==t_bitvector)){
	goto T709;}
	goto T708;
T709:;
	base[3]= Ct;
	base[4]= VV[66];
	base[5]= (V190);
	vs_top=(vs_base=base+3)+3;
	Lformat();
	vs_top=sup;
	base[3]= Cnil;
	vs_top=(vs_base=base+3)+1;
	return;
T708:;
	{object V195;
	V196= aref1((V194),0);
	V195= cdr(/* INLINE-ARGS */V196);
	if(((V192))==Cnil){
	goto T716;}
	V191= make_fixnum((fix((V191)))-(fix((V195))));
T716:;{object V197;
	if((fix((V191)))>=(1)){
	goto T723;}
	V197= Cnil;
	goto T722;
T723:;
	V197= ((fix((V191)))<(length((V194)))?Ct:Cnil);
T722:;
	if(V197==Cnil)goto T721;
	goto T720;
T721:;}{object V198;
	base[3]= Ct;
	base[4]= VV[67];
	base[5]= (V190);
	vs_top=(vs_base=base+3)+3;
	Lformat();
	vs_top=sup;
	V198= vs_base[0];
	if(V198==Cnil)goto T725;
	goto T720;
T725:;}
	base[3]= Cnil;
	vs_top=(vs_base=base+3)+1;
	return;
T720:;
	if(!(eql((V191),small_fixnum(1)))){
	goto T730;}
	{object V199;
	base[3]= (V190);
	vs_top=(vs_base=base+3)+1;
	Lsymbol_function();
	vs_top=sup;
	V199= vs_base[0];
	if(!(type_of((V199))==t_cons)){
	goto T730;}
	if(!((car((V199)))==(VV[68]))){
	goto T730;}
	if((caddr((V199)))==Cnil){
	goto T730;}
	V191= small_fixnum(2);}
T730:;
	{register object V200;
	register object V201;
	V200= make_fixnum((length((V194)))-(fix((V191))));
	V201= small_fixnum(0);
T746:;
	if(!(number_compare((V201),(V200))>=0)){
	goto T747;}
	goto T742;
T747:;
	V202= aref1((V194),fixint((V201)));
	if((/* INLINE-ARGS */V202)==(VV[50])){
	goto T751;}
	base[6]= (V190);
	base[7]= (V194);
	base[8]= make_fixnum((fix((V191)))+(fix((V201))));
	vs_top=(vs_base=base+6)+3;
	(void) (*Lnk135)();
	vs_top=sup;
	base[5]= vs_base[0];
	vs_top=(vs_base=base+5)+1;
	(void) (*Lnk145)();
	vs_top=sup;
	base[4]= vs_base[0];
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk146)();
	vs_top=sup;
	vs_base=vs_top=base+4;
	vs_base[0]=Cnil;
	return;
T751:;
	V201= one_plus((V201));
	goto T746;}
T742:;
	base[3]= Ct;
	base[4]= VV[69];
	vs_top=(vs_base=base+3)+2;
	Lformat();
	return;}}}
	}
}
/*	function definition for INSERT-BREAK-POINT	*/

static L34()
{register object *base=vs_base;
	register object *sup=base+VM32; VC32
	vs_reserve(VM32);
	{object V203;
	check_arg(1);
	V203=(base[0]);
	vs_top=sup;
TTL:;
	{register object V204;
	V204= Cnil;
	if(symbol_value(VV[52])!=Cnil){
	goto T766;}
	vs_base=vs_top;
	(void) (*Lnk139)();
	vs_top=sup;
T766:;{object V205;
	V205= (VFUN_NARGS=2,(*(LnkLI141))(Cnil,symbol_value(VV[52])));
	if(V205==Cnil)goto T769;
	V204= V205;
	goto T768;
T769:;}
	{object V206;
	V206= make_fixnum(length(symbol_value(VV[52])));
	(void)((VFUN_NARGS=2,(*(LnkLI132))(Cnil,symbol_value(VV[52]))));
	V204= (V206);}
T768:;
	{object V207;
	{object V208= cdr(cdr(cdr((V203))));
	if((type_of(V208)!=t_cons) && ((V203)!= Cnil))
	 FEwrong_type_argument(Scons,V208);
	V207= (V208->c.c_car);}
	{object V211;
	V212= get(V207,VV[51],Cnil);
	V211= make_cons((V204),V212);
	(void)(sputprop(V207,VV[51],(V211)));}}
	(void)(aset1(symbol_value(VV[52]),fixint((V204)),(V203)));
	base[1]= (V204);
	vs_top=(vs_base=base+1)+1;
	return;}
	}
}
/*	function definition for SHORT-NAME	*/

static L35()
{register object *base=vs_base;
	register object *sup=base+VM33; VC33
	vs_reserve(VM33);
	{object V213;
	check_arg(1);
	V213=(base[0]);
	vs_top=sup;
TTL:;
	{object V214;
	V214= (VFUN_NARGS=4,(*(LnkLI141))(VV[70],(V213),VV[71],Ct));
	if(((V214))==Cnil){
	goto T780;}
	base[1]= (V213);
	base[2]= make_fixnum((1)+(fix((V214))));
	vs_top=(vs_base=base+1)+2;
	Lsubseq();
	return;
T780:;
	base[1]= (V213);
	vs_top=(vs_base=base+1)+1;
	return;}
	}
}
/*	function definition for SHOW-BREAK-POINT	*/

static L36()
{register object *base=vs_base;
	register object *sup=base+VM34; VC34
	vs_reserve(VM34);
	{object V215;
	check_arg(1);
	V215=(base[0]);
	vs_top=sup;
TTL:;
	{object V216;
	V216= Cnil;
	{register object V217;
	V217= aref1(symbol_value(VV[52]),fixint((V215)));
	if(((V217))==Cnil){
	goto T787;}
	if(!((car((V217)))==(Cnil))){
	goto T789;}
	V216= Ct;
	V217= cdr((V217));
T789:;
	base[1]= Ct;
	base[2]= VV[72];
	base[3]= (V215);
	base[5]= cadr((V217));
	vs_top=(vs_base=base+5)+1;
	(void) (*Lnk147)();
	vs_top=sup;
	base[4]= vs_base[0];
	base[5]= caddr((V217));
	base[6]= (V216);
	vs_top=(vs_base=base+1)+6;
	Lformat();
	vs_top=sup;
	{object V218;
	V218= cadddr((V217));
	base[1]= Ct;
	base[2]= VV[73];
	base[4]= (V218);
	base[5]= caddr((V217));
	vs_top=(vs_base=base+4)+2;
	(void) (*Lnk148)();
	vs_top=sup;
	base[3]= vs_base[0];
	base[4]= (V218);
	vs_top=(vs_base=base+1)+4;
	Lformat();
	return;}
T787:;
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;}}
	}
}
/*	function definition for ITERATE-OVER-BKPTS	*/

static L37()
{register object *base=vs_base;
	register object *sup=base+VM35; VC35
	vs_reserve(VM35);
	{register object V219;
	register object V220;
	check_arg(2);
	V219=(base[0]);
	V220=(base[1]);
	vs_top=sup;
TTL:;
	{register object V221;
	register object V222;
	V221= make_fixnum(length(symbol_value(VV[52])));
	V222= small_fixnum(0);
T813:;
	if(!(number_compare((V222),(V221))>=0)){
	goto T814;}
	base[3]= Cnil;
	vs_top=(vs_base=base+3)+1;
	return;
T814:;
	{register object x= (V222),V223= (V219);
	while(!endp(V223))
	if(eql(x,V223->c.c_car)){
	goto T819;
	}else V223=V223->c.c_cdr;}
	if(((V219))!=Cnil){
	goto T818;}
T819:;
	{register object V224;
	V224= aref1(symbol_value(VV[52]),fixint((V222)));{object V225;
	V225= symbol_value(VV[52]);
	{object V227= (V220);
	if((V227!= VV[53]))goto T825;
	if(((V224))==Cnil){
	goto T826;}
	{object V229= cdr(cdr(cdr((V224))));
	if((type_of(V229)!=t_cons) && ((V224)!= Cnil))
	 FEwrong_type_argument(Scons,V229);
	V228= (V229->c.c_car);}
	{object V232= cdr(cdr(cdr((V224))));
	if((type_of(V232)!=t_cons) && ((V224)!= Cnil))
	 FEwrong_type_argument(Scons,V232);
	V231= (V232->c.c_car);}
	V230= get(V231,VV[51],Cnil);
	V233= (VFUN_NARGS=2,(*(LnkLI121))((V222),V230));
	(void)(sputprop(V228,VV[51],/* INLINE-ARGS */V233));
T826:;
	V226= Cnil;
	goto T824;
T825:;
	if((V227!= VV[149]))goto T832;
	if(!((car((V224)))==(Cnil))){
	goto T834;}
	V226= cdr((V224));
	goto T824;
T834:;
	V226= Cnil;
	goto T824;
T832:;
	if((V227!= VV[150]))goto T836;
	if(((V224))==Cnil){
	goto T838;}
	if((car((V224)))==(Cnil)){
	goto T838;}
	V226= make_cons(Cnil,(V224));
	goto T824;
T838:;
	V226= (V224);
	goto T824;
T836:;
	if((V227!= VV[151]))goto T842;
	if(((V224))==Cnil){
	goto T843;}
	base[3]= (V222);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk146)();
	vs_top=sup;
	princ_char(10,Cnil);
T843:;
	V226= (V224);
	goto T824;
T842:;
	V226= Cnil;}
T824:;
	(void)(aset1(V225,fixint((V222)),V226));}}
T818:;
	V222= one_plus((V222));
	goto T813;}
	}
}
/*	function definition for DISPLAY-ENV	*/

static L38()
{register object *base=vs_base;
	register object *sup=base+VM36; VC36
	vs_reserve(VM36);
	{register object V234;
	object V235;
	check_arg(2);
	V234=(base[0]);
	V235=(base[1]);
	vs_top=sup;
TTL:;
	{register object V236;
	V236= reverse((V235));
T853:;
	if(!(type_of((V236))==t_cons)){
	goto T855;}
	base[2]= symbol_value(VV[74]);
	vs_top=(vs_base=base+2)+1;
	Lfill_pointer();
	vs_top=sup;
	V237= vs_base[0];
	if(!((fix(V237))>(fix((V234))))){
	goto T854;}
T855:;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;
T854:;{object V238;
	if(type_of(car((V236)))==t_cons){
	goto T865;}
	V238= Cnil;
	goto T864;
T865:;
	{object V239= cdar((V236));
	V238= (type_of(V239)==t_cons||(V239)==Cnil?Ct:Cnil);}
T864:;
	if(V238==Cnil)goto T863;
	goto T862;
T863:;}
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;
T862:;
	base[2]= symbol_value(VV[74]);
	base[3]= VV[75];
	base[4]= caar((V236));
	base[5]= cadar((V236));
	base[6]= cdr((V236));
	vs_top=(vs_base=base+2)+5;
	Lformat();
	vs_top=sup;
	V236= cdr((V236));
	goto T853;}
	}
}
/*	function definition for APPLY-DISPLAY-FUN	*/

static L39()
{register object *base=vs_base;
	register object *sup=base+VM37; VC37
	vs_reserve(VM37);
	bds_check;
	{object V240;
	object V241;
	object V242;
	check_arg(3);
	V240=(base[0]);
	V241=(base[1]);
	V242=(base[2]);
	vs_top=sup;
TTL:;
	bds_bind(VV[13],symbol_value(VV[10]));
	bds_bind(VV[11],symbol_value(VV[10]));
	bds_bind(VV[76],Cnil);
	bds_bind(VV[77],VV[78]);
	bds_bind(VV[12],Ct);
	base[8]= symbol_value(VV[74]);
	base[9]= small_fixnum(0);
	vs_top=(vs_base=base+8)+2;
	siLfill_pointer_set();
	vs_top=sup;
	base[8]= symbol_value(VV[74]);
	base[9]= VV[79];
	vs_top=(vs_base=base+8)+2;
	Lformat();
	vs_top=sup;
	(void)((
	(type_of((V240)) == t_sfun ?(*(object (*)())(((V240))->sfn.sfn_self)):
	(fcall.fun=((V240)),fcall.argd=2,fcalln))((V241),(V242))));
	base[8]= symbol_value(VV[74]);
	vs_top=(vs_base=base+8)+1;
	Lfill_pointer();
	vs_top=sup;
	V243= vs_base[0];
	if(!((fix(V243))>(fix((V241))))){
	goto T884;}
	base[8]= symbol_value(VV[74]);
	base[9]= (V241);
	vs_top=(vs_base=base+8)+2;
	siLfill_pointer_set();
	vs_top=sup;
	base[8]= symbol_value(VV[74]);
	base[9]= VV[80];
	vs_top=(vs_base=base+8)+2;
	Lformat();
	vs_top=sup;
T884:;
	base[8]= symbol_value(VV[74]);
	base[9]= VV[81];
	vs_top=(vs_base=base+8)+2;
	Lformat();
	vs_top=sup;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	base[3]= symbol_value(VV[74]);
	vs_top=(vs_base=base+3)+1;
	return;
	}
}
/*	function definition for GET-LINE-OF-FORM	*/

static L40()
{register object *base=vs_base;
	register object *sup=base+VM38; VC38
	vs_reserve(VM38);
	{object V244;
	object V245;
	check_arg(2);
	V244=(base[0]);
	V245=(base[1]);
	vs_top=sup;
TTL:;
	{object V246;
	V246= (VFUN_NARGS=2,(*(LnkLI141))((V244),(V245)));
	if(((V246))==Cnil){
	goto T898;}
	V247= aref1((V245),0);
	base[2]= make_fixnum((fix((V246)))+(fix(cdr(/* INLINE-ARGS */V247))));
	vs_top=(vs_base=base+2)+1;
	return;
T898:;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;}
	}
}
/*	function definition for GET-NEXT-VISIBLE-FUN	*/

static L41()
{register object *base=vs_base;
	register object *sup=base+VM39; VC39
	vs_reserve(VM39);
	{object V248;
	check_arg(1);
	V248=(base[0]);
	vs_top=sup;
TTL:;
	{register object V249;
	V249= (V248);
T901:;
	if(!((fix((V249)))<(fix(symbol_value(VV[82]))))){
	goto T902;}
	{register object V250;
	V250= Cnil;
	(void)((MVloc[(0)]=((V249))));
	base[1]= (V250);
	vs_top=(vs_base=base+1)+1;
	return;}
T902:;
	{register object V251;
	base[1]= (V249);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk138)();
	vs_top=sup;
	V251= vs_base[0];
	{register object V252;
	base[1]= (V251);
	vs_top=(vs_base=base+1)+1;
	Lspecial_form_p();
	vs_top=sup;
	V252= vs_base[0];
	if(((V252))==Cnil){
	goto T913;}
	goto T907;
T913:;
	{register object V253;
	V253= get((V251),VV[83],Cnil);
	if(((V253))==Cnil){
	goto T917;}
	goto T907;
T917:;
	base[1]= (V251);
	vs_top=(vs_base=base+1)+1;
	Lfboundp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T907;}
	{register object V254;
	V254= (V251);
	(void)((MVloc[(0)]=((V249))));
	base[1]= (V254);
	vs_top=(vs_base=base+1)+1;
	return;}}}}
T907:;
	V249= make_fixnum((fix((V249)))-(1));
	goto T901;}
	}
}
/*	function definition for DBL-WHAT-FRAME	*/

static L42()
{register object *base=vs_base;
	register object *sup=base+VM40; VC40
	vs_reserve(VM40);
	{register int V255;
	check_arg(1);
	V255=fix(base[0]);
	vs_top=sup;
TTL:;
	{register int V256;
	register int V257;
	register object V258;
	V256= fix(symbol_value(VV[84]));
	V257= 0;
	V258= Cnil;
T930:;
	{register object V259;
	base[1]= make_fixnum(V256);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk152)();
	vs_top=sup;
	V258= vs_base[0];
	V259= (V258);
	V256= fix((MVloc[(0)]));}
	if(!((V256)<=(V255))){
	goto T938;}
	base[1]= make_fixnum(V257);
	vs_top=(vs_base=base+1)+1;
	return;
T938:;
	V257= (V257)+(1);
	V256= (V256)-(1);
	goto T930;}
	}
}
/*	function definition for DBL-UP	*/

static L43()
{register object *base=vs_base;
	register object *sup=base+VM41; VC41
	vs_reserve(VM41);
	{register object V260;
	object V261;
	check_arg(2);
	V260=(base[0]);
	V261=(base[1]);
	vs_top=sup;
TTL:;
	{object V262;
	object V263;
	object V264;
	object V265;
	object V266;
	V262= Cnil;
	V263= Cnil;
	V264= Cnil;
	V265= Cnil;
	V266= Cnil;
	base[2]= (V261);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk153)();
	vs_top=sup;
	V262= vs_base[0];
	if(!((fix((V260)))>=(0))){
	goto T954;}
	{object V267;
	base[2]= (V260);
	base[3]= (V261);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk154)();
	vs_top=sup;
	setq(VV[9],vs_base[0]);
	V267= symbol_value(VV[9]);
	V260= (MVloc[(0)]);
	V263= (MVloc[(1)]);
	V264= (MVloc[(2)]);
	V265= (MVloc[(3)]);
	V266= (MVloc[(4)]);}
	(void)((*(LnkLI155))());
	base[2]= make_fixnum((fix((V262)))+(fix((V260))));
	base[3]= Ct;
	base[4]= symbol_value(VV[9]);
	base[5]= (V263);
	base[6]= (V264);
	base[7]= (V265);
	base[8]= (V266);
	vs_top=(vs_base=base+2)+7;
	(void) (*Lnk156)();
	return;
T954:;
	V260= make_fixnum((fix((V262)))+(fix((V260))));
	if(!((fix((V260)))>=(0))){
	goto T982;}
	goto T981;
T982:;
	V260= small_fixnum(0);
T981:;
	V261= symbol_value(VV[84]);
	goto TTL;}
	}
}
/*	function definition for NEXT-STACK-FRAME	*/

static L44()
{register object *base=vs_base;
	register object *sup=base+VM42; VC42
	vs_reserve(VM42);
	{object V268;
	check_arg(1);
	V268=(base[0]);
	vs_top=sup;
TTL:;
	{register object V269;
	register object V270;
	register object V271;
	register object V272;
	object V273;
	V269= Cnil;
	V270= Cnil;
	V271= Cnil;
	V272= Cnil;
	V273= Cnil;
	if(!((fix((V268)))<(fix(symbol_value(VV[82]))))){
	goto T994;}
	(void)((MVloc[(0)]=(Cnil)));
	(void)((MVloc[(1)]=(Cnil)));
	(void)((MVloc[(2)]=(Cnil)));
	(void)((MVloc[(3)]=(Cnil)));
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T994:;
	{object V275;
	V275= Cnil;
	{object V276;
	base[1]= (V268);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk152)();
	vs_top=sup;
	V275= vs_base[0];
	V276= (V275);
	V271= (MVloc[(0)]);}
	V273= (V275);
	{object V277;
	V269= get((V275),VV[48],Cnil);
	if(((V269))!=Cnil){
	goto T1009;}
	V277= Cnil;
	goto T1008;
T1009:;
	{register object V278;
	register object V279;
	V278= make_fixnum((fix((V268)))+(1));
	V279= Cnil;
T1014:;
	if(!(number_compare((V278),(V271))<=0)){
	goto T1015;}
	V277= Cnil;
	goto T1008;
T1015:;
	base[1]= (V278);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk112)();
	vs_top=sup;
	V279= vs_base[0];
	base[2]= (V278);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk112)();
	vs_top=sup;
	base[1]= vs_base[0];
	base[2]= (V269);
	vs_top=(vs_base=base+1)+2;
	(void) (*Lnk157)();
	vs_top=sup;
	V270= vs_base[0];
	if(((V270))==Cnil){
	goto T1022;}
	{register object V280;
	V280= (V271);
	(void)((MVloc[(0)]=((V275))));
	(void)((MVloc[(1)]=((V270))));
	V281= aref1((V269),0);
	(void)((MVloc[(2)]=(car(/* INLINE-ARGS */V281))));
	base[2]= (V278);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	V272= vs_base[0];
	base[1]= (V272);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V282= vs_base[0];
	base[1]= one_plus((V272));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V283= vs_base[0];
	base[1]= number_plus((V272),small_fixnum(2));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V284= vs_base[0];
	V285= list(3,V282,V283,V284);
	(void)((MVloc[(3)]=(/* INLINE-ARGS */V285)));
	base[1]= (V280);
	vs_top=(vs_base=base+1)+1;
	return;}
T1022:;
	V278= make_fixnum((fix((V278)))-(1));
	goto T1014;}
T1008:;
	if(((V277))==Cnil){
	goto T1045;}
	base[1]= (V277);
	vs_top=(vs_base=base+1)+1;
	return;
T1045:;
	base[1]= (V273);
	vs_top=(vs_base=base+1)+1;
	Lspecial_form_p();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T1048;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T1048:;
	{object V286;
	V286= get((V273),VV[83],Cnil);
	if(((V286))==Cnil){
	goto T1053;}
	base[1]= (V286);
	vs_top=(vs_base=base+1)+1;
	return;
T1053:;
	base[1]= (V273);
	vs_top=(vs_base=base+1)+1;
	Lfboundp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T1056;}
	(void)((MVloc[(0)]=((V273))));
	(void)((MVloc[(1)]=(Cnil)));
	(void)((MVloc[(2)]=(Cnil)));
	base[1]= (V271);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk158)();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T1065;}
	V288= Cnil;
	goto T1063;
T1065:;
	{object V289;
	base[1]= (V271);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	V289= vs_base[0];
	base[1]= (V289);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V290= vs_base[0];
	base[1]= one_plus((V289));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V291= vs_base[0];
	base[1]= make_fixnum((fix((V289)))+(2));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V292= vs_base[0];
	V288= list(3,V290,V291,V292);}
T1063:;
	(void)((MVloc[(3)]=(V288)));
	base[1]= V271;
	vs_top=(vs_base=base+1)+1;
	return;
T1056:;
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;}}}}
	}
}
/*	function definition for NTH-STACK-FRAME	*/

static L45()
{register object *base=vs_base;
	register object *sup=base+VM43; VC43
	vs_reserve(VM43);
	{register object V293;
	register object V294;
	if(vs_top-vs_base<1) too_few_arguments();
	if(vs_top-vs_base>2) too_many_arguments();
	V293=(base[0]);
	vs_base=vs_base+1;
	if(vs_base>=vs_top){vs_top=sup;goto T1076;}
	V294=(base[1]);
	vs_top=sup;
	goto T1077;
T1076:;
	V294= symbol_value(VV[84]);
T1077:;
	{register object V295;
	object V296;
	object V297;
	object V298;
	register object V299;
	V295= Cnil;
	V296= Cnil;
	V297= Cnil;
	V298= Cnil;
	V299= Cnil;
	if(!((fix((V293)))>=(0))){
	goto T1085;}
	goto T1084;
T1085:;
	V293= small_fixnum(0);
T1084:;
	{object V300;
	register object V301;
	V300= make_fixnum((fix((V293)))+(1));
	V301= small_fixnum(0);
T1092:;
	if(!(number_compare((V301),(V300))>=0)){
	goto T1093;}
	goto T1088;
T1093:;
	base[3]= (V294);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk159)();
	vs_top=sup;
	V299= vs_base[0];
	if(((V299))==Cnil){
	goto T1102;}
	{register object V302;
	V294= (V299);
	V302= (V294);
	V295= (MVloc[(0)]);
	V296= (MVloc[(1)]);
	V297= (MVloc[(2)]);
	V298= (MVloc[(3)]);}
	V294= make_fixnum((fix((V299)))-(1));
	goto T1100;
T1102:;
	V293= make_fixnum((fix((V301)))-(1));
	goto T1088;
T1100:;
	V301= one_plus((V301));
	goto T1092;}
T1088:;
	V294= make_fixnum((fix((V294)))+(1));
	base[2]= (V294);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk138)();
	vs_top=sup;
	V295= vs_base[0];
	(void)((MVloc[(0)]=((V293))));
	(void)((MVloc[(1)]=((V295))));
	(void)((MVloc[(2)]=((V296))));
	(void)((MVloc[(3)]=((V297))));
	(void)((MVloc[(4)]=((V298))));
	base[2]= V294;
	vs_top=(vs_base=base+2)+1;
	return;}
	}
}
/*	function definition for DBL-BACKTRACE	*/

static L46()
{register object *base=vs_base;
	register object *sup=base+VM44; VC44
	vs_reserve(VM44);
	{object V304;
	register object V305;
	if(vs_top-vs_base>2) too_many_arguments();
	if(vs_base>=vs_top){vs_top=sup;goto T1130;}
	V304=(base[0]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T1131;}
	V305=(base[1]);
	vs_top=sup;
	goto T1132;
T1130:;
	V304= small_fixnum(1000);
T1131:;
	V305= symbol_value(VV[84]);
T1132:;
	{register object V306;
	register object V307;
	register object V308;
	register object V309;
	register object V310;
	V306= Cnil;
	V307= Cnil;
	V308= Cnil;
	V309= Cnil;
	V310= small_fixnum(0);
T1142:;
	{object V311;
	base[2]= (V305);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk159)();
	vs_top=sup;
	V305= vs_base[0];
	V311= (V305);
	V306= (MVloc[(0)]);
	V308= (MVloc[(1)]);
	V307= (MVloc[(2)]);
	V309= (MVloc[(3)]);}
	if((V306)!=Cnil){
	goto T1156;}
	goto T1140;
T1156:;
	base[2]= (V310);
	base[3]= Cnil;
	base[4]= (V305);
	base[5]= (V306);
	base[6]= (V308);
	base[7]= (V307);
	base[8]= (V309);
	vs_top=(vs_base=base+2)+7;
	(void) (*Lnk156)();
	vs_top=sup;
	V310= number_plus((V310),small_fixnum(1));
	if(!((fix((V310)))>=(fix((V304))))){
	goto T1167;}
	vs_base=vs_top=base+2;
	vs_base[0]=Cnil;
	vs_top=sup;
	goto T1140;
T1167:;
	V305= make_fixnum((fix((V305)))-(1));
	goto T1142;
T1140:;
	vs_base=vs_top=base+2;
	vs_base[0]=Cnil;
	return;}
	}
}
/*	function definition for DISPLAY-COMPILED-ENV	*/

static L47()
{register object *base=vs_base;
	register object *sup=base+VM45; VC45
	vs_reserve(VM45);
	{register object V312;
	object V313;
	check_arg(2);
	V312=(base[0]);
	V313=(base[1]);
	vs_top=sup;
TTL:;
	{register object V314;
	register object V315;
	base[4]= (V313);
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	V314= vs_base[0];
	base[4]= one_plus((V313));
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	V316= vs_base[0];
	vs_base=vs_top;
	(void) (*Lnk160)();
	vs_top=sup;
	V317= vs_base[0];
	V315= (number_compare(V316,V317)<=0?(V316):V317);
	base[4]= symbol_value(VV[74]);
	base[5]= VV[85];
	vs_top=(vs_base=base+4)+2;
	Lformat();
	vs_top=sup;
	{register object V318;
	register object V319;
	base[4]= (V313);
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk138)();
	vs_top=sup;
	V320= vs_base[0];
	V319= get(V320,VV[14],Cnil);
	V318= (V314);
T1185:;
	if((fix((V318)))>=(fix((V315)))){
	goto T1187;}
	base[4]= symbol_value(VV[74]);
	vs_top=(vs_base=base+4)+1;
	Lfill_pointer();
	vs_top=sup;
	V321= vs_base[0];
	if(!((fix(V321))>(fix((V312))))){
	goto T1186;}
T1187:;
	base[4]= Cnil;
	vs_top=(vs_base=base+4)+1;
	return;
T1186:;
	base[4]= symbol_value(VV[74]);
	base[5]= VV[86];{object V322;
	V322= car((V319));
	if(V322==Cnil)goto T1198;
	base[6]= V322;
	goto T1197;
T1198:;}
	base[6]= VV[87];
T1197:;
	if((car((V319)))!=Cnil){
	goto T1202;}
	base[7]= make_fixnum((fix((V318)))-(fix((V314))));
	goto T1200;
T1202:;
	base[7]= Cnil;
T1200:;
	base[9]= (V318);
	vs_top=(vs_base=base+9)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	base[8]= vs_base[0];
	V318= make_fixnum((fix((V318)))+(1));
	base[9]= ((fix((V318)))<(fix((V315)))?Ct:Cnil);
	vs_top=(vs_base=base+4)+6;
	Lformat();
	vs_top=sup;
	V319= cdr((V319));
	goto T1185;}}
	}
}
/*	function definition for COMPUTING-ARGS-P	*/

static L48()
{register object *base=vs_base;
	register object *sup=base+VM46; VC46
	vs_reserve(VM46);
	{object V323;
	check_arg(1);
	V323=(base[0]);
	vs_top=sup;
TTL:;
	base[1]= (V323);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk112)();
	vs_top=sup;
	V324= vs_base[0];
	if(type_of(V324)==t_cons){
	goto T1211;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T1211:;
	if(number_compare((V323),small_fixnum(3))>0){
	goto T1215;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T1215:;
	base[1]= (V323);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk112)();
	vs_top=sup;
	V327= vs_base[0];
	{register object x= car(V327),V326= VV[88];
	while(!endp(V326))
	if(eql(x,V326->c.c_car)){
	V325= V326;
	goto T1217;
	}else V326=V326->c.c_cdr;
	V325= Cnil;}
T1217:;
	base[1]= ((V325)==Cnil?Ct:Cnil);
	vs_top=(vs_base=base+1)+1;
	return;
	}
}
/*	function definition for PRINT-STACK-FRAME	*/

static L49()
{register object *base=vs_base;
	register object *sup=base+VM47; VC47
	vs_reserve(VM47);
	{object V328;
	object V329;
	register object V330;
	object V331;
	object V332;
	object V333;
	object V334;
	if(vs_top-vs_base<4) too_few_arguments();
	if(vs_top-vs_base>7) too_many_arguments();
	V328=(base[0]);
	V329=(base[1]);
	V330=(base[2]);
	V331=(base[3]);
	vs_base=vs_base+4;
	if(vs_base>=vs_top){vs_top=sup;goto T1220;}
	V332=(base[4]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T1221;}
	V333=(base[5]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T1222;}
	V334=(base[6]);
	vs_top=sup;
	goto T1223;
T1220:;
	V332= Cnil;
T1221:;
	V333= Cnil;
T1222:;
	V334= Cnil;
T1223:;
	if(((V329))==Cnil){
	goto T1227;}
	if(((V332))==Cnil){
	goto T1227;}
	base[7]= symbol_value(VV[1]);
	base[8]= VV[89];
	base[9]= (V333);
	base[10]= (V332);
	vs_top=(vs_base=base+7)+4;
	Lformat();
	vs_top=sup;
T1227:;
	{object V335;
	base[7]= (V330);
	vs_top=(vs_base=base+7)+1;
	(void) (*Lnk161)();
	vs_top=sup;
	V335= vs_base[0];
	base[7]= symbol_value(VV[1]);
	base[8]= VV[90];
	base[9]= (V328);
	if((V335)==Cnil){
	base[10]= Cnil;
	goto T1242;}
	base[10]= VV[91];
T1242:;
	base[11]= (V331);
	base[13]= (V330);
	vs_top=(vs_base=base+13)+1;
	(void) (*Lnk158)();
	vs_top=sup;
	if((vs_base[0])!=Cnil){
	goto T1246;}
	base[13]= VV[92];
	base[14]= small_fixnum(80);
	base[17]= (V330);
	vs_top=(vs_base=base+17)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	base[16]= vs_base[0];
	vs_top=(vs_base=base+16)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V336= vs_base[0];
	base[15]= car(V336);
	vs_top=(vs_base=base+13)+3;
	(void) (*Lnk162)();
	vs_top=sup;
	base[12]= vs_base[0];
	goto T1244;
T1246:;
	base[13]= VV[93];
	base[14]= small_fixnum(80);
	base[15]= (V330);
	vs_top=(vs_base=base+13)+3;
	(void) (*Lnk162)();
	vs_top=sup;
	base[12]= vs_base[0];
T1244:;
	vs_top=(vs_base=base+7)+6;
	Lformat();
	vs_top=sup;
	if(((V333))==Cnil){
	goto T1258;}
	base[7]= symbol_value(VV[1]);
	base[8]= VV[94];
	base[9]= (V333);
	base[10]= (V332);
	vs_top=(vs_base=base+7)+4;
	Lformat();
	vs_top=sup;
T1258:;
	base[7]= symbol_value(VV[1]);
	base[8]= VV[95];
	base[9]= (V330);
	vs_top=(vs_base=base+7)+3;
	Lformat();
	return;}
	}
}
/*	function definition for MAKE-BREAK-POINT	*/

static L50()
{register object *base=vs_base;
	register object *sup=base+VM48; VC48
	vs_reserve(VM48);
	{object V337;
	object V338;
	object V339;
	check_arg(3);
	V337=(base[0]);
	V338=(base[1]);
	V339=(base[2]);
	vs_top=sup;
TTL:;
	V340= aref1((V338),fixint((V339)));
	V341= aref1((V338),0);
	V342= car(/* INLINE-ARGS */V341);
	V343= aref1((V338),0);
	V344 = make_fixnum((fix(cdr(/* INLINE-ARGS */V343)))+(fix((V339))));
	base[3]= list(4,/* INLINE-ARGS */V340,/* INLINE-ARGS */V342,V344,(V337));
	vs_top=(vs_base=base+3)+1;
	return;
	}
}
/*	function definition for RELATIVE-LINE	*/

static L51()
{register object *base=vs_base;
	register object *sup=base+VM49; VC49
	vs_reserve(VM49);
	{object V345;
	object V346;
	check_arg(2);
	V345=(base[0]);
	V346=(base[1]);
	vs_top=sup;
TTL:;
	{object V347;
	V347= get((V345),VV[48],Cnil);
	if(((V347))==Cnil){
	goto T1270;}
	V348= aref1((V347),0);
	base[2]= make_fixnum((fix((V346)))-(fix(cdr(/* INLINE-ARGS */V348))));
	vs_top=(vs_base=base+2)+1;
	return;
T1270:;
	base[2]= small_fixnum(0);
	vs_top=(vs_base=base+2)+1;
	return;}
	}
}
/*	function definition for SAFE-EVAL	*/

static L52()
{register object *base=vs_base;
	register object *sup=base+VM50; VC50
	vs_reserve(VM50);
	bds_check;
	{register object V349;
	object V350;
	check_arg(2);
	V349=(base[0]);
	V350=(base[1]);
	vs_top=sup;
TTL:;
	bds_bind(VV[96],Cnil);
	bds_bind(VV[97],symbol_value(VV[98]));
	bds_bind(VV[1],symbol_value(VV[98]));
	if(!(type_of((V349))==t_symbol)){
	goto T1272;}
	base[5]= (V349);
	vs_top=(vs_base=base+5)+1;
	Lboundp();
	vs_top=sup;
	if((vs_base[0])!=Cnil){
	goto T1272;}
	{register object x= (V349),V351= car((V350));
	while(!endp(V351))
	if(type_of(V351->c.c_car)==t_cons &&eql(x,V351->c.c_car->c.c_car)){
	goto T1272;
	}else V351=V351->c.c_cdr;}
	base[5]= VV[99];
	vs_top=(vs_base=base+5)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;
T1272:;
	{object V352;
	object V353;
	V354= list(2,VV[101],(V349));
	base[5]= list(5,VV[100],/* INLINE-ARGS */V354,Cnil,Cnil,list(2,VV[101],(V350)));
	vs_top=(vs_base=base+5)+1;
	(void) (*Lnk163)();
	if(vs_base>=vs_top){vs_top=sup;goto T1282;}
	V352= vs_base[0];
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T1283;}
	V353= vs_base[0];
	vs_top=sup;
	goto T1284;
T1282:;
	V352= Cnil;
T1283:;
	V353= Cnil;
T1284:;
	if(((V352))==Cnil){
	goto T1286;}
	base[5]= VV[99];
	vs_top=(vs_base=base+5)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;
T1286:;
	base[5]= (V353);
	vs_top=(vs_base=base+5)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;}
	}
}
/*	function definition for SET-BACK	*/

static L53()
{register object *base=vs_base;
	register object *sup=base+VM51; VC51
	vs_reserve(VM51);
	{register object V355;
	register object V356;
	check_arg(2);
	V355=(base[0]);
	V356=(base[1]);
	vs_top=sup;
TTL:;
	{object V357;
	V357= symbol_value(VV[9]);
	setq(VV[102],Cnil);
	setq(VV[9],(V357));
	if(((V356))==Cnil){
	goto T1295;}
	setq(VV[0],(V356));
	goto T1293;
T1295:;
	base[3]= (V357);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk108)();
	vs_top=sup;
	base[2]= vs_base[0];
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk109)();
	vs_top=sup;
	V358= vs_base[0];
	(void)(make_cons(V358,Cnil));
T1293:;
	if(((V355))==Cnil){
	goto T1301;}
	base[2]= symbol_value(VV[1]);
	base[3]= VV[103];
	base[4]= cadr((V355));
	base[5]= caddr((V355));
	vs_top=(vs_base=base+2)+4;
	Lformat();
	vs_top=sup;
	base[2]= symbol_value(VV[1]);
	base[3]= VV[104];
	base[4]= cadr((V355));
	base[5]= caddr((V355));
	vs_top=(vs_base=base+2)+4;
	Lformat();
	vs_top=sup;
T1301:;
	{register object V359;
	register object V360;
	V359= symbol_value(VV[105]);
	V360= car((V359));
T1316:;
	if(!(endp((V359)))){
	goto T1317;}
	base[3]= Cnil;
	vs_top=(vs_base=base+3)+1;
	return;
T1317:;
	{register object V361;
	base[3]= (V360);
	base[4]= (V356);
	vs_top=(vs_base=base+3)+2;
	(void) (*Lnk164)();
	vs_top=sup;
	V361= vs_base[0];
	if(!(((V361))==(VV[99]))){
	goto T1325;}
	goto T1321;
T1325:;
	base[3]= Ct;
	base[4]= VV[106];
	base[5]= (V360);
	base[6]= (V361);
	vs_top=(vs_base=base+3)+4;
	Lformat();
	vs_top=sup;}
T1321:;
	V359= cdr((V359));
	V360= car((V359));
	goto T1316;}}
	}
}
static LnkT164(){ call_or_link(VV[164],&Lnk164);} /* SAFE-EVAL */
static LnkT163(){ call_or_link(VV[163],&Lnk163);} /* ERROR-SET */
static LnkT162(){ call_or_link(VV[162],&Lnk162);} /* APPLY-DISPLAY-FUN */
static LnkT161(){ call_or_link(VV[161],&Lnk161);} /* COMPUTING-ARGS-P */
static LnkT160(){ call_or_link(VV[160],&Lnk160);} /* VS-TOP */
static LnkT159(){ call_or_link(VV[159],&Lnk159);} /* NEXT-STACK-FRAME */
static LnkT158(){ call_or_link(VV[158],&Lnk158);} /* IHS-NOT-INTERPRETED-ENV */
static LnkT157(){ call_or_link(VV[157],&Lnk157);} /* GET-LINE-OF-FORM */
static LnkT156(){ call_or_link(VV[156],&Lnk156);} /* PRINT-STACK-FRAME */
static object  LnkTLI155(){return call_proc0(VV[155],&LnkLI155);} /* SET-ENV */
static LnkT154(){ call_or_link(VV[154],&Lnk154);} /* NTH-STACK-FRAME */
static LnkT153(){ call_or_link(VV[153],&Lnk153);} /* DBL-WHAT-FRAME */
static LnkT152(){ call_or_link(VV[152],&Lnk152);} /* GET-NEXT-VISIBLE-FUN */
static LnkT148(){ call_or_link(VV[148],&Lnk148);} /* RELATIVE-LINE */
static LnkT147(){ call_or_link(VV[147],&Lnk147);} /* SHORT-NAME */
static LnkT146(){ call_or_link(VV[146],&Lnk146);} /* SHOW-BREAK-POINT */
static LnkT145(){ call_or_link(VV[145],&Lnk145);} /* INSERT-BREAK-POINT */
static LnkT144(){ call_or_link(VV[144],&Lnk144);} /* BREAK-FUNCTION */
static LnkT143(){ call_or_link(VV[143],&Lnk143);} /* DWIM */
static LnkT142(){ call_or_link(VV[142],&Lnk142);} /* MAYBE-BREAK */
static object  LnkTLI141(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_vproc(VV[141],&LnkLI141,ap);} /* POSITION */
static LnkT140(){ call_or_link(VV[140],&Lnk140);} /* CURRENT-STEP-FUN */
static LnkT139(){ call_or_link(VV[139],&Lnk139);} /* INIT-BREAK-POINTS */
static LnkT138(){ call_or_link(VV[138],&Lnk138);} /* IHS-FNAME */
static LnkT137(){ call_or_link(VV[137],&Lnk137);} /* STREAM-NAME */
static LnkT136(){ call_or_link(VV[136],&Lnk136);} /* ITERATE-OVER-BKPTS */
static LnkT135(){ call_or_link(VV[135],&Lnk135);} /* MAKE-BREAK-POINT */
static object  LnkTLI134(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_vproc(VV[134],&LnkLI134,ap);} /* MAKE-ARRAY */
static LnkT133(){ call_or_link(VV[133],&Lnk133);} /* WALK-THROUGH */
static object  LnkTLI132(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_vproc(VV[132],&LnkLI132,ap);} /* VECTOR-PUSH-EXTEND */
static LnkT131(){ call_or_link(VV[131],&Lnk131);} /* INSTREAM-NAME */
static LnkT130(){ call_or_link(VV[130],&Lnk130);} /* LOAD */
static LnkT129(){ call_or_link(VV[129],&Lnk129);} /* SETUP-LINEINFO */
static LnkT128(){ call_or_link(VV[128],&Lnk128);} /* CLEANUP */
static LnkT125(){ call_or_link(VV[125],&Lnk125);} /* READ-FROM-STRING */
static object  LnkTLI124(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_vproc(VV[124],&LnkLI124,ap);} /* COUNT */
static LnkT123(){ call_or_link(VV[123],&Lnk123);} /* GET-INSTREAM */
static LnkT122(){ call_or_link(VV[122],&Lnk122);} /* MAKE-INSTREAM */
static object  LnkTLI121(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_vproc(VV[121],&LnkLI121,ap);} /* DELETE */
static LnkT120(){ call_or_link(VV[120],&Lnk120);} /* CLOSEDP */
static LnkT118(){ call_or_link(VV[118],&Lnk118);} /* BREAK-LEVEL */
static object  LnkTLI117(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[117],&LnkLI117,1,ap);} /* SHOW-ENVIRONMENT */
static LnkT116(){ call_or_link(VV[116],&Lnk116);} /* BREAK-LOCALS */
static LnkT115(){ call_or_link(VV[115],&Lnk115);} /* BREAK-GO */
static object  LnkTLI114(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_vproc(VV[114],&LnkLI114,ap);} /* SEARCH */
static LnkT113(){ call_or_link(VV[113],&Lnk113);} /* COMPILED-FUNCTION-NAME */
static LnkT112(){ call_or_link(VV[112],&Lnk112);} /* IHS-FUN */
static object  LnkTLI111(){return call_proc0(VV[111],&LnkLI111);} /* TYPE-ERROR */
static LnkT110(){ call_or_link(VV[110],&Lnk110);} /* IHS-TOP */
static LnkT109(){ call_or_link(VV[109],&Lnk109);} /* VS */
static LnkT108(){ call_or_link(VV[108],&Lnk108);} /* IHS-VS */
static LnkT107(){ call_or_link(VV[107],&Lnk107);} /* BREAK-PREVIOUS */
