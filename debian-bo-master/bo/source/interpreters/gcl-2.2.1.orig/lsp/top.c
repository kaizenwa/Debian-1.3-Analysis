
#include <cmpinclude.h>
#include "top.h"
init_top(){do_init(VV);}
/*	local entry for function TOP-LEVEL	*/

static object LI1()

{	 VMB1 VMS1 VMV1
	bds_check;
TTL:;
	bds_bind(VV[0],Cnil);
	bds_bind(VV[1],Cnil);
	bds_bind(VV[2],Cnil);
	bds_bind(VV[3],Cnil);
	bds_bind(VV[4],Cnil);
	bds_bind(VV[5],Cnil);
	bds_bind(VV[6],Cnil);
	bds_bind(VV[7],Cnil);
	bds_bind(VV[8],Cnil);
	bds_bind(VV[9],Cnil);
	(VV[10]->s.s_dbind)= Ct;
	frs_push(FRS_CATCH,(VV[11]->s.s_dbind));
	if(nlj_active)
	{nlj_active=FALSE;frs_pop();
	vs_top=sup;
	goto T3;}
	else{
	if(((VV[12]->s.s_dbind))==Cnil){
	goto T8;}
	(VV[12]->s.s_dbind)= Cnil;
	goto T6;
T8:;
	if(!((file_exists(VV[13])))){
	goto T6;}
	base[10]= VV[14];
	vs_top=(vs_base=base+10)+1;
	(void) (*Lnk140)();
	vs_top=sup;
T6:;
	bds_bind(VV[15],Cnil);
	base[11]= (VV[16]->s.s_dbind);
	vs_top=(vs_base=base+11)+1;
	(void) (*Lnk141)();
	vs_top=sup;
	bds_unwind1;
	base[10]= (VV[17]->s.s_dbind);
	vs_top=(vs_base=base+10)+1;
	Lfunctionp();
	vs_top=sup;
	if((vs_base[0])!=Cnil){
	goto T15;}
	frs_pop();
	goto T3;
T15:;
	V1= (
	(type_of((VV[17]->s.s_dbind)) == t_sfun ?(*(object (*)())(((VV[17]->s.s_dbind))->sfn.sfn_self)):
	(fcall.fun=((VV[17]->s.s_dbind)),fcall.argd=0,fcalln))());
	frs_pop();}
T3:;
T19:;
	(VV[2]->s.s_dbind)= (VV[1]->s.s_dbind);
	(VV[1]->s.s_dbind)= (VV[0]->s.s_dbind);
	(VV[0]->s.s_dbind)= (VV[3]->s.s_dbind);
	if(((VV[18]->s.s_dbind))==Cnil){
	goto T29;}
	(VV[18]->s.s_dbind)= Cnil;
	goto T27;
T29:;
	base[10]= Ct;
	base[11]= VV[19];{VOL object V2;
	V2= (VV[20]->s.s_dbind);
	base[13]= VV[21];
	vs_top=(vs_base=base+13)+1;
	Lfind_package();
	vs_top=sup;
	V3= vs_base[0];
	if(!((V2)==(V3))){
	goto T36;}}
	base[12]= VV[22];
	goto T34;
T36:;
	base[13]= (VV[20]->s.s_dbind);
	vs_top=(vs_base=base+13)+1;
	Lpackage_name();
	vs_top=sup;
	base[12]= vs_base[0];
T34:;
	vs_top=(vs_base=base+10)+3;
	Lformat();
	vs_top=sup;
T27:;
	vs_base=vs_top;
	siLreset_stack_limits();
	vs_top=sup;
	if(((VV[12]->s.s_dbind))==Cnil){
	goto T42;}
	{object V4 = Cnil;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	VMR1(V4)}
T42:;
	frs_push(FRS_CATCH,(VV[11]->s.s_dbind));
	if(nlj_active)
	{nlj_active=FALSE;frs_pop();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T20;}
	goto T46;}
	else{
	base[10]= (VV[23]->s.s_dbind);
	base[11]= Cnil;
	base[12]= (VV[24]->s.s_dbind);
	(VV[3]->s.s_dbind)= simple_symlispcall(VV[142],base+10,3);
	if(!(((VV[3]->s.s_dbind))==((VV[24]->s.s_dbind)))){
	goto T53;}
	vs_base=vs_top;
	Lby();
	vs_top=sup;
T53:;
	{object V5;
	base[10]= (VV[3]->s.s_dbind);
	symlispcall(VV[143],base+10,1);
	Llist();
	vs_top=sup;
	V5= vs_base[0];
	(VV[9]->s.s_dbind)= (VV[8]->s.s_dbind);
	(VV[8]->s.s_dbind)= (VV[7]->s.s_dbind);
	(VV[7]->s.s_dbind)= (V5);
	(VV[6]->s.s_dbind)= (VV[5]->s.s_dbind);
	(VV[5]->s.s_dbind)= (VV[4]->s.s_dbind);
	(VV[4]->s.s_dbind)= CMPcar((VV[7]->s.s_dbind));
	vs_base=vs_top;
	Lfresh_line();
	vs_top=sup;
	{register object V6;
	register object V7;
	V6= (VV[7]->s.s_dbind);
	V7= CMPcar((V6));
T76:;
	if(!(((V6))==Cnil)){
	goto T77;}
	goto T72;
T77:;
	base[11]= (V7);
	(void)simple_symlispcall(VV[144],base+11,1);
	princ_char(10,Cnil);
	V6= CMPcdr((V6));
	V7= CMPcar((V6));
	goto T76;}
T72:;
	frs_pop();
	goto T20;}}
T46:;
	(VV[25]->s.s_dbind)= Cnil;
	(VV[26]->s.s_dbind)= Cnil;
	princ_char(10,VV[27]);
	vs_base=vs_top;
	(void) (*Lnk145)();
	vs_top=sup;
T20:;
	goto T19;
}
/*	function definition for PROCESS-SOME-ARGS	*/

static L2()
{register object *base=vs_base;
	register object *sup=base+VM2; VC2
	vs_check;
	{register object V8;
	V8=(base[0]);
	vs_top=sup;
TTL:;
T96:;
	{register object V9;
	register object V10;
	V9= CMPcar((V8));
	V10= Cnil;
	if(!(equal((V9),VV[28]))){
	goto T102;}
	base[1]= CMPcadr((V8));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk140)();
	vs_top=sup;
	goto T100;
T102:;
	if(!(equal((V9),VV[29]))){
	goto T106;}
	base[2]= CMPcadr((V8));
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk146)();
	vs_top=sup;
	base[1]= vs_base[0];
	vs_top=(vs_base=base+1)+1;
	Leval();
	vs_top=sup;
	goto T100;
T106:;
	V10= Ct;
T100:;
	if((V10)!=Cnil){
	goto T111;}
	V8= CMPcdr((V8));
T111:;
	V8= CMPcdr((V8));}
	if((V8)!=Cnil){
	goto T97;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T97:;
	goto T96;
	}
}
/*	function definition for DBL-READ	*/

static L3()
{register object *base=vs_base;
	register object *sup=base+VM3; VC3
	vs_check;
	{register object V11;
	register object V12;
	register object V13;
	if(vs_base>=vs_top){vs_top=sup;goto T115;}
	V11=(base[0]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T116;}
	V12=(base[1]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T117;}
	V13=(base[2]);
	vs_top=sup;
	goto T118;
T115:;
	V11= (VV[23]->s.s_dbind);
T116:;
	V12= Ct;
T117:;
	V13= Cnil;
T118:;
	{object V14;
	register object V15;
	V14= Cnil;
	V15= Cnil;
T126:;
	base[3]= (V11);
	base[4]= (V12);
	base[5]= (V13);
	vs_top=(vs_base=base+3)+3;
	Lread_char();
	vs_top=sup;
	V15= vs_base[0];
	if(!(eql((V15),VV[30]))){
	goto T134;}
	goto T126;
T134:;
	if(!(((V15))==((V13)))){
	goto T132;}
	base[3]= (V13);
	vs_top=(vs_base=base+3)+1;
	return;
T132:;
	base[3]= (V15);
	base[4]= (V11);
	vs_top=(vs_base=base+3)+2;
	Lunread_char();
	vs_top=sup;
	goto T124;
T124:;
	if(!(eql(VV[31],(V15)))){
	goto T142;}
	base[3]= VV[32];
	base[5]= (V11);
	base[6]= (V12);
	base[7]= (V13);
	vs_top=(vs_base=base+5)+3;
	Lread_line();
	vs_top=sup;
	base[4]= vs_base[0];
	base[5]= VV[33];
	vs_top=(vs_base=base+3)+3;
	(void) (*Lnk147)();
	vs_top=sup;
	V14= vs_base[0];
	base[4]= (V14);
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk148)();
	vs_top=sup;
	base[3]= vs_base[0];
	base[4]= (V12);
	base[5]= (V13);
	vs_top=(vs_base=base+3)+3;
	Lread();
	return;
T142:;
	base[3]= (V11);
	base[4]= (V12);
	base[5]= (V13);
	vs_top=(vs_base=base+3)+3;
	Lread();
	return;}
	}
}
/*	function definition for BREAK-LEVEL	*/

static L4()
{register object *VOL base=vs_base;
	register object *VOL sup=base+VM4; VC4
	vs_check;
	bds_check;
	{VOL object V16;
	VOL object V17;
	V16=(base[0]);
	vs_base=vs_base+1;
	if(vs_base>=vs_top){vs_top=sup;goto T159;}
	V17=(base[1]);
	vs_top=sup;
	goto T160;
T159:;
	V17= Cnil;
T160:;
	{VOL object V18;
	if(!(type_of((V16))==t_string)){
	goto T164;}
	bds_bind(VV[34],(V16));
	goto T162;
T164:;
	bds_bind(VV[34],(VV[34]->s.s_dbind));
T162:;
	V19= make_cons((VV[36]->s.s_dbind),(VV[11]->s.s_dbind));
	bds_bind(VV[35],make_cons(/* INLINE-ARGS */V19,(VV[35]->s.s_dbind)));
	bds_bind(VV[11],make_cons(Cnil,Cnil));
	if(((V16))!=Cnil){
	goto T170;}
	bds_bind(VV[36],(VV[36]->s.s_dbind));
	goto T168;
T170:;
	bds_bind(VV[36],make_cons(Ct,(VV[36]->s.s_dbind)));
T168:;
	bds_bind(VV[37],one_plus((VV[38]->s.s_dbind)));
	vs_base=vs_top;
	(void) (*Lnk149)();
	vs_top=sup;
	V20= vs_base[0];
	bds_bind(VV[38],one_minus(V20));
	bds_bind(VV[39],(VV[38]->s.s_dbind));{VOL object V21;
	base[25]= (VV[41]->s.s_dbind);
	base[26]= (VV[37]->s.s_dbind);
	vs_top=(vs_base=base+25)+2;
	(void) (*Lnk150)();
	vs_top=sup;
	V21= vs_base[0];
	if(V21==Cnil)goto T176;
	bds_bind(VV[40],V21);
	goto T175;
T176:;}
	vs_base=vs_top;
	(void) (*Lnk151)();
	vs_top=sup;
	V22= vs_base[0];
	bds_bind(VV[40],one_plus(V22));
T175:;
	vs_base=vs_top;
	(void) (*Lnk151)();
	vs_top=sup;
	bds_bind(VV[41],vs_base[0]);
	bds_bind(VV[42],Cnil);
	V18= (VV[43]->s.s_dbind);
	if(!(type_of((V16))==t_string)){
	goto T185;}
	bds_bind(VV[43],Cnil);
	goto T183;
T185:;
	bds_bind(VV[43],(V18));
T183:;
	if((VV[45]->s.s_dbind)!=Cnil){
	bds_bind(VV[44],(VV[45]->s.s_dbind));
	goto T187;}
	bds_bind(VV[44],(VV[44]->s.s_dbind));
T187:;
	bds_bind(VV[46],Cnil);
	bds_bind(VV[0],(VV[0]->s.s_dbind));
	bds_bind(VV[1],(VV[1]->s.s_dbind));
	bds_bind(VV[2],(VV[2]->s.s_dbind));
	bds_bind(VV[3],(VV[3]->s.s_dbind));
	bds_bind(VV[4],(VV[4]->s.s_dbind));
	bds_bind(VV[5],(VV[5]->s.s_dbind));
	bds_bind(VV[6],(VV[6]->s.s_dbind));
	bds_bind(VV[7],(VV[7]->s.s_dbind));
	bds_bind(VV[8],(VV[8]->s.s_dbind));
	bds_bind(VV[9],(VV[9]->s.s_dbind));
	if(((V18))!=Cnil){
	goto T188;}
	if(!(type_of((V16))==t_string)){
	goto T188;}
	vs_base=vs_top;
	(void) (*Lnk152)();
	vs_top=sup;
	base[25]= make_fixnum(length(CMPcdr((VV[36]->s.s_dbind))));
	vs_top=(vs_base=base+25)+1;
	(void) (*Lnk153)();
	vs_top=sup;
T188:;
	base[25]= small_fixnum(1);
	vs_top=(vs_base=base+25)+1;
	(void) (*Lnk154)();
	vs_top=sup;
	(VV[47]->s.s_dbind)= Ct;
	if(!(type_of((V16))==t_string)){
	goto T201;}
	(void)((*(LnkLI155))());
	princ_char(10,VV[27]);
	(VV[18]->s.s_dbind)= Cnil;
	goto T199;
T201:;
	base[25]= (V16);
	base[26]= (V17);
	vs_top=(vs_base=base+25)+2;
	(void) (*Lnk156)();
	vs_top=sup;
T199:;
T209:;
	(VV[2]->s.s_dbind)= (VV[1]->s.s_dbind);
	(VV[1]->s.s_dbind)= (VV[0]->s.s_dbind);
	(VV[0]->s.s_dbind)= (VV[3]->s.s_dbind);
	if(((VV[18]->s.s_dbind))==Cnil){
	goto T219;}
	(VV[18]->s.s_dbind)= Cnil;
	goto T217;
T219:;
	base[25]= (VV[48]->s.s_dbind);
	base[26]= VV[49];
	if(!(type_of((V16))==t_string)){
	goto T226;}
	base[27]= VV[50];
	goto T224;
T226:;
	base[27]= VV[51];
T224:;{VOL object V23;
	V23= (VV[20]->s.s_dbind);
	base[29]= VV[21];
	vs_top=(vs_base=base+29)+1;
	Lfind_package();
	vs_top=sup;
	V24= vs_base[0];
	if(!((V23)==(V24))){
	goto T230;}}
	base[28]= VV[52];
	goto T228;
T230:;
	base[29]= (VV[20]->s.s_dbind);
	vs_top=(vs_base=base+29)+1;
	Lpackage_name();
	vs_top=sup;
	base[28]= vs_base[0];
T228:;
	base[29]= (VV[36]->s.s_dbind);
	vs_top=(vs_base=base+25)+5;
	Lformat();
	vs_top=sup;
T217:;
	frs_push(FRS_CATCH,VV[53]);
	if(nlj_active)
	{nlj_active=FALSE;frs_pop();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T210;}
	goto T237;}
	else{
	frs_push(FRS_CATCH,(VV[11]->s.s_dbind));
	if(nlj_active)
	{nlj_active=FALSE;frs_pop();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	frs_pop();
	goto T210;}
	frs_pop();
	goto T237;}
	else{
	base[25]= (VV[48]->s.s_dbind);
	base[26]= Cnil;
	base[27]= (VV[24]->s.s_dbind);
	vs_top=(vs_base=base+25)+3;
	(void) (*Lnk157)();
	vs_top=sup;
	(VV[3]->s.s_dbind)= vs_base[0];
	if(!(((VV[3]->s.s_dbind))==((VV[24]->s.s_dbind)))){
	goto T245;}
	vs_base=vs_top;
	Lby();
	vs_top=sup;
T245:;
	{object V25;
	object V26;
	V25= Cnil;
	if(!((type_of((VV[3]->s.s_dbind))==t_symbol&&((VV[3]->s.s_dbind))->s.s_hpack==keyword_package))){
	goto T252;}
	(VV[3]->s.s_dbind)= make_cons((VV[3]->s.s_dbind),Cnil);
T252:;
	if(!(type_of((VV[3]->s.s_dbind))==t_cons)){
	goto T257;}
	{object V27= CMPcar((VV[3]->s.s_dbind));
	if(!((type_of(V27)==t_symbol&&(V27)->s.s_hpack==keyword_package))){
	goto T257;}}
	V25= Ct;
	base[26]= CMPcar((VV[3]->s.s_dbind));
	base[27]= CMPcdr((VV[3]->s.s_dbind));
	symlispcall(VV[158],base+26,2);
	goto T251;
T257:;
	base[26]= (VV[3]->s.s_dbind);
	base[27]= Cnil;
	base[28]= Cnil;
	base[29]= (VV[42]->s.s_dbind);
	symlispcall(VV[159],base+26,4);
T251:;
	Llist();
	vs_top=sup;
	V26= vs_base[0];
	if((V25)==Cnil){
	goto T269;}
	if((CMPcar((V26)))==(VV[54])){
	goto T270;}
	goto T269;
T270:;
	frs_pop();
	frs_pop();
	base[26]= Cnil;
	vs_top=(vs_base=base+26)+1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	return;
T269:;
	(VV[9]->s.s_dbind)= (VV[8]->s.s_dbind);
	(VV[8]->s.s_dbind)= (VV[7]->s.s_dbind);
	(VV[7]->s.s_dbind)= (V26);
	(VV[6]->s.s_dbind)= (VV[5]->s.s_dbind);
	(VV[5]->s.s_dbind)= (VV[4]->s.s_dbind);
	(VV[4]->s.s_dbind)= CMPcar((VV[7]->s.s_dbind));
	base[26]= (VV[48]->s.s_dbind);
	vs_top=(vs_base=base+26)+1;
	Lfresh_line();
	vs_top=sup;
	{register object V28;
	register object V29;
	V28= (VV[7]->s.s_dbind);
	V29= CMPcar((V28));
T289:;
	if(!(((V28))==Cnil)){
	goto T290;}
	goto T248;
T290:;
	base[27]= (V29);
	base[28]= (VV[48]->s.s_dbind);
	(void)simple_symlispcall(VV[144],base+27,2);
	princ_char(10,VV[48]);
	V28= CMPcdr((V28));
	V29= CMPcar((V28));
	goto T289;}}
T248:;
	frs_pop();
	frs_pop();
	goto T210;}}
T237:;
	princ_char(10,VV[48]);
	vs_base=vs_top;
	(void) (*Lnk145)();
	vs_top=sup;
T210:;
	goto T209;}
	}
}
/*	local entry for function WARN	*/

static object LI5(V30,va_alist)
	object V30;
	va_dcl 
{	
	va_list ap;
	int narg = VFUN_NARGS; VMB5 VMS5 VMV5
	bds_check;
	{object V31;
	object V32;
	V31= V30;
	narg= narg - 1;
	va_start(ap);
	V33 = list_vector(narg,ap);
	V32= V33;
	bds_bind(VV[55],small_fixnum(4));
	bds_bind(VV[56],small_fixnum(4));
	bds_bind(VV[57],VV[58]);
	if(((VV[59]->s.s_dbind))==Cnil){
	goto T306;}
	base[3]= (V31);
	{object V34;
	V34= (V32);
	 vs_top=base+4;
	 while(V34!=Cnil)
	 {vs_push((V34)->c.c_car);V34=(V34)->c.c_cdr;}
	vs_base=base+3;}
	(void) (*Lnk160)();
	vs_top=sup;
	{object V35 = vs_base[0];
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	VMR5(V35)}
T306:;
	base[3]= (VV[27]->s.s_dbind);
	base[4]= VV[60];
	vs_top=(vs_base=base+3)+2;
	Lformat();
	vs_top=sup;
	bds_bind(VV[61],Ct);
	base[4]= (VV[27]->s.s_dbind);
	base[5]= (V31);
	{object V36;
	V36= (V32);
	 vs_top=base+6;
	 while(V36!=Cnil)
	 {vs_push((V36)->c.c_car);V36=(V36)->c.c_cdr;}
	vs_base=base+4;}
	Lformat();
	vs_top=sup;
	bds_unwind1;
	{object V37 = Cnil;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	VMR5(V37)}}
	}
/*	local entry for function UNIVERSAL-ERROR-HANDLER	*/

static object LI6(V42,V41,V40,V39,V38,va_alist)
	object V42,V41,V40,V39,V38;
	va_dcl 
{	
	va_list ap;
	int narg = VFUN_NARGS; VMB6 VMS6 VMV6
	bds_check;
	{object V43;
	object V44;
	register object V45;
	object V46;
	register object V47;
	register object V48;
	V43= V42;
	V44= V41;
	V45= V40;
	V46= V39;
	V47= V38;
	narg= narg - 5;
	va_start(ap);
	V49 = list_vector(narg,ap);
	V48= V49;
	{object V50;
	V50= Cnil;
	bds_bind(VV[62],Cnil);
	bds_bind(VV[55],(VV[63]->s.s_dbind));
	bds_bind(VV[56],(VV[63]->s.s_dbind));
	bds_bind(VV[57],VV[58]);
	princ_char(10,VV[27]);
	if(((V44))==Cnil){
	goto T321;}
	if(((VV[43]->s.s_dbind))==Cnil){
	goto T321;}
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[64];
	vs_top=(vs_base=base+4)+2;
	Lformat();
	vs_top=sup;
	bds_bind(VV[61],Ct);
	base[5]= (VV[27]->s.s_dbind);
	base[6]= (V47);
	{object V51;
	V51= (V48);
	 vs_top=base+7;
	 while(V51!=Cnil)
	 {vs_push((V51)->c.c_car);V51=(V51)->c.c_cdr;}
	vs_base=base+5;}
	Lformat();
	vs_top=sup;
	bds_unwind1;
	princ_char(10,VV[27]);
	base[4]= Cnil;
	base[5]= (V47);
	{object V52;
	V52= (V48);
	 vs_top=base+6;
	 while(V52!=Cnil)
	 {vs_push((V52)->c.c_car);V52=(V52)->c.c_cdr;}
	vs_base=base+4;}
	Lformat();
	vs_top=sup;
	V50= vs_base[0];
	if(((V45))==Cnil){
	goto T340;}
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[65];
	base[6]= (V45);
	vs_top=(vs_base=base+4)+3;
	Lformat();
	vs_top=sup;
	goto T338;
T340:;
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[66];
	vs_top=(vs_base=base+4)+2;
	Lformat();
	vs_top=sup;
T338:;
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[67];
	vs_top=(vs_base=base+4)+2;
	Lformat();
	vs_top=sup;
	bds_bind(VV[61],Ct);
	base[5]= (VV[27]->s.s_dbind);
	base[6]= VV[68];
	base[7]= (V46);
	base[8]= (V48);
	vs_top=(vs_base=base+5)+4;
	Lformat();
	vs_top=sup;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	goto T318;
T321:;
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[69];
	vs_top=(vs_base=base+4)+2;
	Lformat();
	vs_top=sup;
	bds_bind(VV[61],Ct);
	base[5]= (VV[27]->s.s_dbind);
	base[6]= (V47);
	{object V53;
	V53= (V48);
	 vs_top=base+7;
	 while(V53!=Cnil)
	 {vs_push((V53)->c.c_car);V53=(V53)->c.c_cdr;}
	vs_base=base+5;}
	Lformat();
	vs_top=sup;
	bds_unwind1;
	princ_char(10,VV[27]);
	if(!((length((VV[70]->s.s_dbind)))>(0))){
	goto T362;}
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[71];
	vs_top=(vs_base=base+4)+2;
	Lformat();
	vs_top=sup;
T362:;
	base[4]= Cnil;
	base[5]= (V47);
	{object V54;
	V54= (V48);
	 vs_top=base+6;
	 while(V54!=Cnil)
	 {vs_push((V54)->c.c_car);V54=(V54)->c.c_cdr;}
	vs_base=base+4;}
	Lformat();
	vs_top=sup;
	V50= vs_base[0];
	if(((V45))==Cnil){
	goto T373;}
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[72];
	base[6]= (V45);
	vs_top=(vs_base=base+4)+3;
	Lformat();
	vs_top=sup;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	goto T318;
T373:;
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[73];
	vs_top=(vs_base=base+4)+2;
	Lformat();
	vs_top=sup;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
T318:;
	base[0]= (V50);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk161)();
	vs_top=sup;
	if(((V44))!=Cnil){
	goto T383;}
	{frame_ptr fr;
	fr=frs_sch_catch((VV[11]->s.s_dbind));
	if(fr==NULL) FEerror("The tag ~s is undefined.",1,(VV[11]->s.s_dbind));
	base[0]= (VV[11]->s.s_dbind);
	vs_top=(vs_base=base+0)+1;
	unwind(fr,(VV[11]->s.s_dbind));}
T383:;
	{object V55 = Cnil;
	VMR6(V55)}}}
	}
/*	local entry for function BREAK	*/

static object LI7(va_alist)
	va_dcl 
{	
	va_list ap;
	int narg = VFUN_NARGS; VMB7 VMS7 VMV7
	bds_check;
	{object V56;
	object V57;
	narg = narg - 0;
	if (narg <= 0) goto T386;
	else {
	va_start(ap);
	V56= va_arg(ap,object);}
	--narg; goto T387;
T386:;
	V56= Cnil;
T387:;
	V58 = list_vector(narg,ap);
	V57= V58;
	{object V59;
	V59= Cnil;
	bds_bind(VV[62],Cnil);
	bds_bind(VV[55],small_fixnum(4));
	bds_bind(VV[56],small_fixnum(4));
	bds_bind(VV[57],VV[58]);
	princ_char(10,VV[27]);
	if(((V56))==Cnil){
	goto T393;}
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[74];
	vs_top=(vs_base=base+4)+2;
	Lformat();
	vs_top=sup;
	bds_bind(VV[61],Ct);
	base[5]= (VV[27]->s.s_dbind);
	base[6]= (V56);
	{object V60;
	V60= (V57);
	 vs_top=base+7;
	 while(V60!=Cnil)
	 {vs_push((V60)->c.c_car);V60=(V60)->c.c_cdr;}
	vs_base=base+5;}
	Lformat();
	vs_top=sup;
	bds_unwind1;
	princ_char(10,VV[27]);
	base[4]= Cnil;
	base[5]= (V56);
	{object V61;
	V61= (V57);
	 vs_top=base+6;
	 while(V61!=Cnil)
	 {vs_push((V61)->c.c_car);V61=(V61)->c.c_cdr;}
	vs_base=base+4;}
	Lformat();
	vs_top=sup;
	V59= vs_base[0];
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	goto T390;
T393:;
	base[4]= (VV[27]->s.s_dbind);
	base[5]= VV[75];
	vs_top=(vs_base=base+4)+2;
	Lformat();
	vs_top=sup;
	V59= VV[76];
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
	bds_unwind1;
T390:;
	bds_bind(VV[43],Ct);
	base[1]= (V59);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk161)();
	vs_top=sup;
	bds_unwind1;
	{object V62 = Cnil;
	VMR7(V62)}}}
	}
/*	local entry for function TERMINAL-INTERRUPT	*/

static object LI8(V64)

object V64;
{	 VMB8 VMS8 VMV8
	bds_check;
TTL:;
	bds_bind(VV[43],Ct);
	if(((V64))==Cnil){
	goto T414;}
	base[1]= VV[77];
	base[2]= VV[78];
	vs_top=(vs_base=base+1)+2;
	Lcerror();
	vs_top=sup;
	{object V65 = vs_base[0];
	bds_unwind1;
	VMR8(V65)}
T414:;
	base[1]= VV[79];
	vs_top=(vs_base=base+1)+1;
	Lerror();
	vs_top=sup;
	{object V66 = vs_base[0];
	bds_unwind1;
	VMR8(V66)}
}
/*	local entry for function BREAK-CALL	*/

static object LI9(V69,V70)

object V69;object V70;
{	 VMB9 VMS9 VMV9
TTL:;
	{object V71;
	{object V72 =((V69))->s.s_plist;
	 object ind= VV[80];
	while(V72!=Cnil){
	if(V72->c.c_car==ind){
	V71= (V72->c.c_cdr->c.c_car);
	goto T419;
	}else V72=V72->c.c_cdr->c.c_cdr;}
	V71= Cnil;}
T419:;
	if(((V71))==Cnil){
	goto T421;}
	base[1]= make_cons((V71),(V70));
	base[2]= Cnil;
	base[3]= Cnil;
	base[4]= (VV[42]->s.s_dbind);
	vs_top=(vs_base=base+1)+4;
	Levalhook();
	vs_top=sup;
	{object V73 = vs_base[0];
	VMR9(V73)}
T421:;
	base[1]= (VV[48]->s.s_dbind);
	base[2]= VV[81];
	base[3]= (V69);
	vs_top=(vs_base=base+1)+3;
	Lformat();
	vs_top=sup;
	{object V74 = vs_base[0];
	VMR9(V74)}}
}
/*	function definition for BREAK-QUIT	*/

static L10()
{register object *base=vs_base;
	register object *sup=base+VM10; VC10
	vs_check;
	{object V75;
	if(vs_base>=vs_top){vs_top=sup;goto T430;}
	V75=(base[0]);
	vs_top=sup;
	goto T431;
T430:;
	V75= small_fixnum(0);
T431:;
	{object V76;
	V76= make_fixnum(length((VV[36]->s.s_dbind)));
	if(!(number_compare((V75),small_fixnum(0))>=0)){
	goto T434;}
	if(!(number_compare((V75),(V76))<0)){
	goto T434;}
	{object V77;
	base[2]= (V76);
	base[3]= (V75);
	base[4]= small_fixnum(1);
	vs_top=(vs_base=base+2)+3;
	Lminus();
	vs_top=sup;
	V78= vs_base[0];
	V77= nth(fix(V78),(VV[35]->s.s_dbind));
	{frame_ptr fr;
	base[2]= CMPcdr((V77));
	fr=frs_sch_catch(base[2]);
	if(fr==NULL) FEerror("The tag ~s is undefined.",1,base[2]);
	base[3]= CMPcdr((V77));
	vs_top=(vs_base=base+3)+1;
	unwind(fr,base[2]);}}
T434:;
	vs_base=vs_top;
	(void) (*Lnk145)();
	return;}
	}
}
/*	function definition for BREAK-PREVIOUS	*/

static L11()
{register object *base=vs_base;
	register object *sup=base+VM11; VC11
	vs_check;
	{register object V79;
	if(vs_base>=vs_top){vs_top=sup;goto T446;}
	V79=(base[0]);
	vs_top=sup;
	goto T447;
T446:;
	V79= small_fixnum(1);
T447:;
	{register object V80;
	V80= one_minus((VV[39]->s.s_dbind));
T451:;
	if(number_compare((V80),(VV[37]->s.s_dbind))<0){
	goto T453;}
	if(!(number_compare((V79),small_fixnum(0))<=0)){
	goto T452;}
T453:;
	(void)((*(LnkLI162))());
	vs_base=vs_top;
	(void) (*Lnk145)();
	return;
T452:;
	if(((*(LnkLI163))((V80)))==Cnil){
	goto T459;}
	(VV[39]->s.s_dbind)= (V80);
	V79= one_minus((V79));
T459:;
	V80= one_minus((V80));
	goto T451;}
	}
}
/*	local entry for function SET-CURRENT	*/

static object LI12()

{	 VMB12 VMS12 VMV12
TTL:;
	{register object V81;
	V81= (VV[39]->s.s_dbind);
T469:;
	if(((*(LnkLI163))((V81)))!=Cnil){
	goto T471;}
	if(!(number_compare((V81),(VV[37]->s.s_dbind))<=0)){
	goto T470;}
T471:;
	(VV[39]->s.s_dbind)= (V81);
	(void)((*(LnkLI162))());
	base[0]= (VV[48]->s.s_dbind);
	base[1]= VV[82];
	base[3]= (VV[39]->s.s_dbind);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk164)();
	vs_top=sup;
	base[2]= vs_base[0];
	base[3]= CMPcdr((VV[36]->s.s_dbind));
	vs_top=(vs_base=base+0)+4;
	Lformat();
	vs_top=sup;
	{object V82 = vs_base[0];
	VMR12(V82)}
T470:;
	V81= one_minus((V81));
	goto T469;}
}
/*	function definition for BREAK-NEXT	*/

static L13()
{register object *base=vs_base;
	register object *sup=base+VM13; VC13
	vs_check;
	{register object V83;
	if(vs_base>=vs_top){vs_top=sup;goto T487;}
	V83=(base[0]);
	vs_top=sup;
	goto T488;
T487:;
	V83= small_fixnum(1);
T488:;
	{register object V84;
	V84= (VV[39]->s.s_dbind);
T491:;
	if(number_compare((V84),(VV[38]->s.s_dbind))>0){
	goto T493;}
	if(!(number_compare((V83),small_fixnum(0))<0)){
	goto T492;}
T493:;
	(void)((*(LnkLI162))());
	vs_base=vs_top;
	(void) (*Lnk145)();
	return;
T492:;
	if(((*(LnkLI163))((V84)))==Cnil){
	goto T499;}
	(VV[39]->s.s_dbind)= (V84);
	V83= one_minus((V83));
T499:;
	V84= one_plus((V84));
	goto T491;}
	}
}
/*	function definition for BREAK-GO	*/

static L14()
{register object *base=vs_base;
	register object *sup=base+VM14; VC14
	vs_check;
	{object V85;
	V85=(base[0]);
	vs_top=sup;
TTL:;
	{object V86= (number_compare((V85),(VV[37]->s.s_dbind))>=0?((V85)):(VV[37]->s.s_dbind));
	(VV[39]->s.s_dbind)= (number_compare(V86,(VV[38]->s.s_dbind))<=0?(V86):(VV[38]->s.s_dbind));}
	if(((*(LnkLI163))((VV[39]->s.s_dbind)))==Cnil){
	goto T511;}
	(void)((*(LnkLI162))());
	vs_base=vs_top;
	(void) (*Lnk145)();
	return;
T511:;
	vs_base=vs_top;
	(void) (*Lnk165)();
	return;
	}
}
/*	function definition for BREAK-MESSAGE	*/

static L15()
{register object *base=vs_base;
	register object *sup=base+VM15; VC15
	vs_check;
	vs_top=sup;
TTL:;
	(void)(princ((VV[34]->s.s_dbind),(VV[48]->s.s_dbind)));
	princ_char(10,VV[48]);
	vs_base=vs_top=base+0;
	vs_base[0]=Cnil;
	return;
}
/*	function definition for DESCRIBE-ENVIRONMENT	*/

static L16()
{register object *base=vs_base;
	register object *sup=base+VM16; VC16
	vs_check;
	{object V87;
	register object V88;
	if(vs_base>=vs_top){vs_top=sup;goto T516;}
	V87=(base[0]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T517;}
	V88=(base[1]);
	vs_top=sup;
	goto T518;
T516:;
	V87= (VV[42]->s.s_dbind);
T517:;
	V88= (VV[48]->s.s_dbind);
T518:;
	if(!((length((V87)))==(3))){
	goto T522;}
	goto T521;
T522:;
	base[2]= VV[83];
	vs_top=(vs_base=base+2)+1;
	Lerror();
	vs_top=sup;
T521:;
	{register object V89;
	V89= VV[84];
	base[2]= (V88);
	base[3]= (V89);
	base[4]= VV[85];
	{object V90;
	{object V91;
	object V92= CMPcar((VV[42]->s.s_dbind));
	if(V92==Cnil){
	V90= Cnil;
	goto T529;}
	base[5]=V91=MMcons(Cnil,Cnil);
T530:;
	(V91->c.c_car)= CMPcar((V92->c.c_car));
	if((V92=MMcdr(V92))==Cnil){
	V90= base[5];
	goto T529;}
	V91=MMcdr(V91)=MMcons(Cnil,Cnil);
	goto T530;}
T529:;
	 vs_top=base+5;
	 while(V90!=Cnil)
	 {vs_push((V90)->c.c_car);V90=(V90)->c.c_cdr;}
	vs_base=base+2;}
	Lformat();
	vs_top=sup;
	base[2]= (V88);
	base[3]= (V89);
	base[4]= VV[86];
	{object V93;
	{object V94;
	object V95= CMPcadr((VV[42]->s.s_dbind));
	if(V95==Cnil){
	V93= Cnil;
	goto T536;}
	base[5]=V94=MMcons(Cnil,Cnil);
T537:;
	(V94->c.c_car)= CMPcar((V95->c.c_car));
	if((V95=MMcdr(V95))==Cnil){
	V93= base[5];
	goto T536;}
	V94=MMcdr(V94)=MMcons(Cnil,Cnil);
	goto T537;}
T536:;
	 vs_top=base+5;
	 while(V93!=Cnil)
	 {vs_push((V93)->c.c_car);V93=(V93)->c.c_cdr;}
	vs_base=base+2;}
	Lformat();
	vs_top=sup;
	base[2]= (V88);
	base[3]= (V89);
	base[4]= VV[87];
	{object V96;
	{object V97;
	object V98= CMPcaddr((VV[42]->s.s_dbind));
	if(V98==Cnil){
	V96= Cnil;
	goto T543;}
	base[5]=V97=MMcons(Cnil,Cnil);
T544:;
	if(!((CMPcadr((V98->c.c_car)))==(VV[88]))){
	goto T547;}
	(V97->c.c_cdr)= make_cons(CMPcar((V98->c.c_car)),Cnil);
	goto T545;
T547:;
	(V97->c.c_cdr)= Cnil;
T545:;
	while(MMcdr(V97)!=Cnil)V97=MMcdr(V97);
	if((V98=MMcdr(V98))==Cnil){
	base[5]=base[5]->c.c_cdr;
	V96= base[5];
	goto T543;}
	goto T544;}
T543:;
	 vs_top=base+5;
	 while(V96!=Cnil)
	 {vs_push((V96)->c.c_car);V96=(V96)->c.c_cdr;}
	vs_base=base+2;}
	Lformat();
	vs_top=sup;
	base[2]= (V88);
	base[3]= (V89);
	base[4]= VV[89];
	{object V100;
	{object V101;
	object V102= CMPcaddr((VV[42]->s.s_dbind));
	if(V102==Cnil){
	V100= Cnil;
	goto T552;}
	base[5]=V101=MMcons(Cnil,Cnil);
T553:;
	if(!((CMPcadr((V102->c.c_car)))==(VV[90]))){
	goto T556;}
	(V101->c.c_cdr)= make_cons(CMPcar((V102->c.c_car)),Cnil);
	goto T554;
T556:;
	(V101->c.c_cdr)= Cnil;
T554:;
	while(MMcdr(V101)!=Cnil)V101=MMcdr(V101);
	if((V102=MMcdr(V102))==Cnil){
	base[5]=base[5]->c.c_cdr;
	V100= base[5];
	goto T552;}
	goto T553;}
T552:;
	 vs_top=base+5;
	 while(V100!=Cnil)
	 {vs_push((V100)->c.c_car);V100=(V100)->c.c_cdr;}
	vs_base=base+2;}
	Lformat();
	return;}
	}
}
/*	function definition for BREAK-VS	*/

static L17()
{register object *base=vs_base;
	register object *sup=base+VM17; VC17
	vs_check;
	{register object V104;
	object V105;
	if(vs_base>=vs_top){vs_top=sup;goto T558;}
	V104=(base[0]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T559;}
	V105=(base[1]);
	vs_top=sup;
	goto T560;
T558:;
	base[2]= (VV[37]->s.s_dbind);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk166)();
	vs_top=sup;
	V104= vs_base[0];
T559:;
	base[2]= (VV[38]->s.s_dbind);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk166)();
	vs_top=sup;
	V105= vs_base[0];
T560:;
	base[2]= (VV[37]->s.s_dbind);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk166)();
	vs_top=sup;
	V106= vs_base[0];
	V104= (number_compare((V104),V106)>=0?((V104)):V106);
	base[2]= one_plus((VV[38]->s.s_dbind));
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk166)();
	vs_top=sup;
	V107= vs_base[0];
	V108= one_minus(V107);
	V105= (number_compare((V105),/* INLINE-ARGS */V108)<=0?((V105)):/* INLINE-ARGS */V108);
	{register object V109;
	V109= (VV[37]->s.s_dbind);
T574:;
	if(number_compare((V109),(VV[38]->s.s_dbind))>=0){
	goto T576;}
	base[2]= (V109);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk166)();
	vs_top=sup;
	V110= vs_base[0];
	if(!(number_compare(V110,(V104))>=0)){
	goto T575;}
T576:;
	{register object V111;
	V111= (V104);
T583:;
	if(!(number_compare((V111),(V105))>0)){
	goto T584;}
	vs_base=vs_top=base+2;
	vs_base[0]=Cnil;
	return;
T584:;
T590:;
	base[2]= (V109);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk166)();
	vs_top=sup;
	V112= vs_base[0];
	if(!(number_compare(V112,(V111))>0)){
	goto T591;}
	goto T588;
T591:;
	if(((*(LnkLI163))((V109)))==Cnil){
	goto T597;}
	(void)((*(LnkLI167))((V109)));
T597:;
	V109= number_plus((V109),small_fixnum(1));
	goto T590;
T588:;
	base[2]= (VV[48]->s.s_dbind);
	base[3]= VV[91];
	base[4]= (V111);
	base[6]= (V111);
	vs_top=(vs_base=base+6)+1;
	(void) (*Lnk168)();
	vs_top=sup;
	base[5]= vs_base[0];
	vs_top=(vs_base=base+2)+4;
	Lformat();
	vs_top=sup;
	V111= one_plus((V111));
	goto T583;}
T575:;
	V109= one_plus((V109));
	goto T574;}
	}
}
/*	function definition for BREAK-LOCAL	*/

static L18()
{register object *base=vs_base;
	register object *sup=base+VM18; VC18
	vs_check;
	{object V113;
	if(vs_base>=vs_top){vs_top=sup;goto T617;}
	V113=(base[0]);
	vs_top=sup;
	goto T618;
T617:;
	V113= small_fixnum(0);
T618:;
	{object V114;
	base[2]= (VV[39]->s.s_dbind);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk166)();
	vs_top=sup;
	V115= vs_base[0];
	V114= number_plus(V115,(V113));
	base[2]= (V114);
	base[3]= (V114);
	vs_top=(vs_base=base+2)+2;
	(void) (*Lnk169)();
	return;}
	}
}
/*	function definition for BREAK-BDS	*/

static L19()
{register object *base=vs_base;
	register object *sup=base+VM19; VC19
	vs_check;
	{register object V116;
	vs_top[0]=Cnil;
	{object *p=vs_top;
	 for(;p>vs_base;p--)p[-1]=MMcons(p[-1],p[0]);}
	V116=(base[0]);
	vs_top=sup;
	{register object V117;
	V117= (VV[40]->s.s_dbind);
	{register object V118;
	register object V119;
	base[1]= one_minus((VV[40]->s.s_dbind));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk170)();
	vs_top=sup;
	V120= vs_base[0];
	V118= one_plus(V120);
	base[1]= one_plus((VV[41]->s.s_dbind));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk170)();
	vs_top=sup;
	V119= vs_base[0];
T632:;
	if(!(number_compare((V118),(V119))>0)){
	goto T633;}
	vs_base=vs_top=base+1;
	vs_base[0]=Cnil;
	return;
T633:;
	if(((V116))==Cnil){
	goto T638;}
	base[1]= (V118);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk171)();
	vs_top=sup;
	V122= vs_base[0];
	{register object x= V122,V121= (V116);
	while(V121!=Cnil)
	if(eql(x,V121->c.c_car)){
	goto T641;
	}else V121=V121->c.c_cdr;
	goto T637;}
T641:;
T638:;
T646:;
	if(number_compare((V117),(VV[41]->s.s_dbind))>0){
	goto T648;}
	base[1]= (V117);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk170)();
	vs_top=sup;
	V123= vs_base[0];
	if(!(number_compare(V123,(V118))>0)){
	goto T647;}
T648:;
	goto T644;
T647:;
	(void)((*(LnkLI172))((V117)));
	V117= number_plus((V117),small_fixnum(1));
	goto T646;
T644:;
	base[1]= (VV[48]->s.s_dbind);
	base[2]= VV[92];
	base[3]= (V118);
	base[5]= (V118);
	vs_top=(vs_base=base+5)+1;
	(void) (*Lnk171)();
	vs_top=sup;
	base[4]= vs_base[0];
	base[6]= (V118);
	vs_top=(vs_base=base+6)+1;
	(void) (*Lnk173)();
	vs_top=sup;
	base[5]= vs_base[0];
	vs_top=(vs_base=base+1)+5;
	Lformat();
	vs_top=sup;
T637:;
	V118= one_plus((V118));
	goto T632;}}
	}
}
/*	function definition for SIMPLE-BACKTRACE	*/

static L20()
{register object *base=vs_base;
	register object *sup=base+VM20; VC20
	vs_check;
	vs_top=sup;
TTL:;
	princ_str("Backtrace: ",VV[48]);
	{register object V124;
	register object V125;
	V124= (VV[37]->s.s_dbind);
	V125= Cnil;
T674:;
	if(!(number_compare((V124),(VV[38]->s.s_dbind))>0)){
	goto T675;}
	princ_char(10,VV[48]);
	vs_base=vs_top=base+0;
	vs_base[0]=Cnil;
	return;
T675:;
	if(((*(LnkLI163))((V124)))==Cnil){
	goto T680;}
	if(((V125))==Cnil){
	goto T683;}
	princ_str(" > ",VV[48]);
T683:;
	base[1]= (V124);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk164)();
	vs_top=sup;
	base[0]= vs_base[0];
	base[1]= VV[93];
	base[2]= (VV[48]->s.s_dbind);
	base[3]= VV[94];
	base[4]= Ct;
	base[5]= VV[95];
	if(!(number_compare((V124),(VV[39]->s.s_dbind))==0)){
	goto T695;}
	base[6]= VV[58];
	goto T693;
T695:;
	base[6]= VV[96];
T693:;
	vs_top=(vs_base=base+0)+7;
	Lwrite();
	vs_top=sup;
T680:;
	V124= one_plus((V124));
	V125= Ct;
	goto T674;}
}
/*	function definition for IHS-BACKTRACE	*/

static L21()
{register object *base=vs_base;
	register object *sup=base+VM21; VC21
	vs_check;
	{object V126;
	register object V127;
	if(vs_base>=vs_top){vs_top=sup;goto T702;}
	V126=(base[0]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T703;}
	V127=(base[1]);
	vs_top=sup;
	goto T704;
T702:;
	V126= (VV[37]->s.s_dbind);
T703:;
	V127= (VV[38]->s.s_dbind);
T704:;
	V126= (number_compare((V126),(VV[37]->s.s_dbind))>=0?((V126)):(VV[37]->s.s_dbind));
	V127= (number_compare((V127),(VV[38]->s.s_dbind))<=0?((V127)):(VV[38]->s.s_dbind));
	{register object V128;
	register object V129;
	V128= (V126);{object V130;
	base[3]= (VV[40]->s.s_dbind);
	base[4]= (V126);
	vs_top=(vs_base=base+3)+2;
	(void) (*Lnk150)();
	vs_top=sup;
	V130= vs_base[0];
	if(V130==Cnil)goto T713;
	V129= V130;
	goto T712;
T713:;}
	V129= one_plus((VV[41]->s.s_dbind));
T712:;
T718:;
	if(!(number_compare((V128),(V127))>0)){
	goto T719;}
	vs_base=vs_top=base+3;
	vs_base[0]=Cnil;
	return;
T719:;
	if(((*(LnkLI163))((V128)))==Cnil){
	goto T723;}
	(void)((*(LnkLI167))((V128)));
T723:;
T728:;
	if(number_compare((V129),(VV[41]->s.s_dbind))>0){
	goto T730;}
	base[3]= (V129);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk174)();
	vs_top=sup;
	V131= vs_base[0];
	if(!(number_compare(V131,(V128))>0)){
	goto T729;}
T730:;
	goto T726;
T729:;
	(void)((*(LnkLI172))((V129)));
	V129= number_plus((V129),small_fixnum(1));
	goto T728;
T726:;
	V128= one_plus((V128));
	goto T718;}
	}
}
/*	local entry for function PRINT-IHS	*/

static object LI22(V133)

register object V133;
{	 VMB22 VMS22 VMV22
	bds_check;
TTL:;
	bds_bind(VV[55],small_fixnum(2));
	bds_bind(VV[56],small_fixnum(4));
	base[2]= Ct;
	base[3]= VV[97];
	base[4]= (number_compare((V133),(VV[39]->s.s_dbind))==0?Ct:Cnil);
	base[5]= (V133);
	{register object V134;
	base[7]= (V133);
	vs_top=(vs_base=base+7)+1;
	(void) (*Lnk175)();
	vs_top=sup;
	V134= vs_base[0];
	if(type_of((V134))==t_symbol){
	goto T752;}
	base[7]= (V134);
	vs_top=(vs_base=base+7)+1;
	Lcompiled_function_p();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T753;}
T752:;
	base[6]= (V134);
	goto T749;
T753:;
	if(!(type_of((V134))==t_cons)){
	goto T759;}
	{object V135= CMPcar((V134));
	if((V135!= VV[98]))goto T761;
	base[6]= (V134);
	goto T749;
T761:;
	if((V135!= VV[176])
	&& (V135!= VV[177]))goto T762;
	base[6]= CMPcdr((V134));
	goto T749;
T762:;
	if((V135!= VV[115]))goto T763;
	base[6]= make_cons(VV[98],CMPcddddr((V134)));
	goto T749;
T763:;
	if((V135!= VV[178]))goto T764;
	base[6]= CMPcddddr((V134));
	goto T749;
T764:;
	if(!(type_of(CMPcar((V134)))==t_symbol)){
	goto T766;}
	base[7]= CMPcar((V134));
	vs_top=(vs_base=base+7)+1;
	Lspecial_form_p();
	vs_top=sup;
	if((vs_base[0])!=Cnil){
	goto T765;}
	base[7]= CMPcar((V134));
	vs_top=(vs_base=base+7)+1;
	Lfboundp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T766;}
T765:;
	base[6]= CMPcar((V134));
	goto T749;
T766:;
	base[6]= VV[99];
	goto T749;}
T759:;
	(void)(print((V134),Cnil));
	base[6]= VV[100];}
T749:;
	base[8]= (V133);
	vs_top=(vs_base=base+8)+1;
	(void) (*Lnk166)();
	vs_top=sup;
	base[7]= vs_base[0];
	vs_top=(vs_base=base+2)+6;
	Lformat();
	vs_top=sup;
	{object V136 = vs_base[0];
	bds_unwind1;
	bds_unwind1;
	VMR22(V136)}
}
/*	local entry for function PRINT-FRS	*/

static object LI23(V138)

register object V138;
{	 VMB23 VMS23 VMV23
TTL:;
	base[0]= (VV[48]->s.s_dbind);
	base[1]= VV[101];
	base[2]= (V138);
	base[3]= (*(LnkLI179))((V138));
	base[5]= (V138);
	vs_top=(vs_base=base+5)+1;
	(void) (*Lnk174)();
	vs_top=sup;
	base[4]= vs_base[0];
	base[6]= (V138);
	vs_top=(vs_base=base+6)+1;
	(void) (*Lnk180)();
	vs_top=sup;
	base[5]= vs_base[0];
	base[7]= (V138);
	vs_top=(vs_base=base+7)+1;
	(void) (*Lnk170)();
	vs_top=sup;
	base[6]= vs_base[0];
	vs_top=(vs_base=base+0)+7;
	Lformat();
	vs_top=sup;
	{object V139 = vs_base[0];
	VMR23(V139)}
}
/*	local entry for function FRS-KIND	*/

static object LI24(V141)

register object V141;
{	 VMB24 VMS24 VMV24
TTL:;
	{register object V142;
	V142= Cnil;
	base[0]= (V141);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk181)();
	vs_top=sup;
	V144= vs_base[0];
	{object V143= V144;
	if((V143!= VV[182]))goto T790;
	base[1]= (V141);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	base[0]= vs_base[0];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk184)();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T792;}{object V145;
	base[1]= (V141);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	base[0]= vs_base[0];
	base[3]= (V141);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk180)();
	vs_top=sup;
	V146= vs_base[0];
	base[2]= number_plus(V146,small_fixnum(2));
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk168)();
	vs_top=sup;
	base[1]= vs_base[0];
	base[2]= VV[102];
	base[3]= (VV[185]->s.s_gfdef);
	base[4]= VV[103];
	base[5]= (VV[186]->s.s_gfdef);
	vs_top=(vs_base=base+0)+6;
	Lmember();
	vs_top=sup;
	V142= vs_base[0];
	if(((V142))!=Cnil){
	goto T798;}
	V145= Cnil;
	goto T797;
T798:;
	if(!((CMPcadar((V142)))==(VV[88]))){
	goto T812;}
	V145= list(3,VV[88],CMPcaar((V142)),VV[6]);
	goto T797;
T812:;
	base[1]= (V141);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	V150= vs_base[0];
	V151= (VV[186]->s.s_gfdef);
	V152= (VV[185]->s.s_gfdef);
	V153= (VFUN_NARGS=6,(*(LnkLI187))(V150,(V142),VV[105],V151,VV[102],V152));
	{object V148;
	object V149= /* INLINE-ARGS */V153;
	if(V149==Cnil){
	V147= Cnil;
	goto T814;}
	base[0]=V148=MMcons(Cnil,Cnil);
T815:;
	(V148->c.c_car)= CMPcar((V149->c.c_car));
	if((V149=MMcdr(V149))==Cnil){
	V147= base[0];
	goto T814;}
	V148=MMcdr(V148)=MMcons(Cnil,Cnil);
	goto T815;}
T814:;
	V154= reverse(V147);
	V155= append(/* INLINE-ARGS */V154,VV[106]);
	V145= make_cons(VV[104],/* INLINE-ARGS */V155);
T797:;
	if(V145==Cnil)goto T796;
	{object V156 = V145;
	VMR24(V156)}
T796:;}
	base[0]= (V141);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	V157= vs_base[0];
	{object V158 = list(2,VV[107],V157);
	VMR24(V158)}
T792:;
	base[0]= (V141);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	V159= vs_base[0];
	{object V160 = list(3,VV[108],list(2,VV[109],V159),VV[6]);
	VMR24(V160)}
T790:;
	if((V143!= VV[188]))goto T825;
	{object V161 = VV[110];
	VMR24(V161)}
T825:;
	base[0]= (V141);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	V162= vs_base[0];
	{object V163 = list(2,VV[111],V162);
	VMR24(V163)}}}
}
/*	function definition for BREAK-CURRENT	*/

static L25()
{register object *base=vs_base;
	register object *sup=base+VM25; VC25
	vs_check;
	vs_top=sup;
TTL:;
	if(((VV[36]->s.s_dbind))==Cnil){
	goto T830;}
	base[0]= (VV[48]->s.s_dbind);
	base[1]= VV[112];
	base[3]= (VV[39]->s.s_dbind);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk164)();
	vs_top=sup;
	base[2]= vs_base[0];
	vs_top=(vs_base=base+0)+3;
	Lformat();
	vs_top=sup;
	goto T828;
T830:;
	base[0]= (VV[48]->s.s_dbind);
	base[1]= VV[113];
	vs_top=(vs_base=base+0)+2;
	Lformat();
	vs_top=sup;
T828:;
	vs_base=vs_top=base+0;
	vs_base[0]=Cnil;
	return;
}
/*	local entry for function IHS-VISIBLE	*/

static object LI26(V165)

object V165;
{	 VMB26 VMS26 VMV26
TTL:;
	{object V166;
	base[1]= (V165);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk164)();
	vs_top=sup;
	V166= vs_base[0];
	if((V166)==Cnil){
	{object V167 = Cnil;
	VMR26(V167)}}
	{register object x= (V166),V169= (VV[114]->s.s_dbind);
	while(V169!=Cnil)
	if(eql(x,V169->c.c_car)){
	V168= V169;
	goto T840;
	}else V169=V169->c.c_cdr;
	V168= Cnil;}
T840:;
	{object V170 = ((V168)==Cnil?Ct:Cnil);
	VMR26(V170)}}
}
/*	function definition for IHS-FNAME	*/

static L27()
{register object *base=vs_base;
	register object *sup=base+VM27; VC27
	vs_check;
	{object V171;
	V171=(base[0]);
	vs_top=sup;
TTL:;
	{register object V172;
	base[1]= (V171);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk175)();
	vs_top=sup;
	V172= vs_base[0];
	if(!(type_of((V172))==t_symbol)){
	goto T844;}
	base[1]= (V172);
	vs_top=(vs_base=base+1)+1;
	return;
T844:;
	if(!(type_of((V172))==t_cons)){
	goto T847;}
	{object V173= CMPcar((V172));
	if((V173!= VV[98]))goto T849;
	base[1]= VV[98];
	vs_top=(vs_base=base+1)+1;
	return;
T849:;
	if((V173!= VV[176])
	&& (V173!= VV[177]))goto T850;
	base[1]= CMPcadr((V172));
	vs_top=(vs_base=base+1)+1;
	return;
T850:;
	if((V173!= VV[178]))goto T851;
	base[1]= CMPcar(CMPcddddr((V172)));
	vs_top=(vs_base=base+1)+1;
	return;
T851:;
	if((V173!= VV[115]))goto T852;
	base[1]= VV[115];
	vs_top=(vs_base=base+1)+1;
	return;
T852:;
	if(!(type_of(CMPcar((V172)))==t_symbol)){
	goto T854;}
	base[1]= CMPcar((V172));
	vs_top=(vs_base=base+1)+1;
	Lspecial_form_p();
	vs_top=sup;
	if((vs_base[0])!=Cnil){
	goto T853;}
	base[1]= CMPcar((V172));
	vs_top=(vs_base=base+1)+1;
	Lfboundp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T854;}
T853:;
	base[1]= CMPcar((V172));
	vs_top=(vs_base=base+1)+1;
	return;
T854:;
	base[1]= VV[100];
	vs_top=(vs_base=base+1)+1;
	return;}
T847:;
	base[1]= (V172);
	vs_top=(vs_base=base+1)+1;
	Lcompiled_function_p();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T863;}
	base[1]= (V172);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk189)();
	return;
T863:;
	base[1]= VV[100];
	vs_top=(vs_base=base+1)+1;
	return;}
	}
}
/*	function definition for IHS-NOT-INTERPRETED-ENV	*/

static L28()
{register object *base=vs_base;
	register object *sup=base+VM28; VC28
	vs_check;
	{object V174;
	V174=(base[0]);
	vs_top=sup;
TTL:;
	{object V175;
	base[1]= (V174);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk175)();
	vs_top=sup;
	V175= vs_base[0];
	if(!(type_of((V175))==t_cons)){
	goto T870;}
	if(!(number_compare((V174),small_fixnum(3))>0)){
	goto T870;}
	base[1]= Cnil;
	vs_top=(vs_base=base+1)+1;
	return;
T870:;
	base[1]= Ct;
	vs_top=(vs_base=base+1)+1;
	return;}
	}
}
/*	local entry for function SET-ENV	*/

static object LI29()

{	 VMB29 VMS29 VMV29
TTL:;
	base[0]= (VV[39]->s.s_dbind);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk190)();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T876;}
	(VV[42]->s.s_dbind)= Cnil;
	goto T874;
T876:;
	{object V176;
	base[0]= (VV[39]->s.s_dbind);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk166)();
	vs_top=sup;
	V176= vs_base[0];
	base[0]= (V176);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk168)();
	vs_top=sup;
	V177= vs_base[0];
	base[0]= one_plus((V176));
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk168)();
	vs_top=sup;
	V178= vs_base[0];
	base[0]= number_plus((V176),small_fixnum(2));
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk168)();
	vs_top=sup;
	V179= vs_base[0];
	(VV[42]->s.s_dbind)= list(3,V177,V178,V179);}
T874:;
	{object V180 = (VV[42]->s.s_dbind);
	VMR29(V180)}
}
/*	local entry for function LIST-DELQ	*/

static object LI30(V183,V184)

object V183;register object V184;
{	 VMB30 VMS30 VMV30
TTL:;
	if(((V184))!=Cnil){
	goto T888;}
	{object V185 = Cnil;
	VMR30(V185)}
T888:;
	if(!(((V183))==(CMPcar((V184))))){
	goto T891;}
	{object V186 = CMPcdr((V184));
	VMR30(V186)}
T891:;
	V187= (*(LnkLI191))((V183),CMPcdr((V184)));
	((V184))->c.c_cdr = /* INLINE-ARGS */V187;
	{object V188 = (V184);
	VMR30(V188)}
}
/*	local entry for function SUPER-GO	*/

static object LI31(V191,V192)

register object V191;register object V192;
{	 VMB31 VMS31 VMV31
TTL:;
	{object V193;
	V193= Cnil;
	if(!(number_compare((V191),(VV[40]->s.s_dbind))>=0)){
	goto T894;}
	if(!(number_compare((V191),(VV[41]->s.s_dbind))<=0)){
	goto T894;}
	base[1]= (V191);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	base[0]= vs_base[0];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk184)();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T894;}
	base[1]= (V191);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	base[0]= vs_base[0];
	base[3]= (V191);
	vs_top=(vs_base=base+3)+1;
	(void) (*Lnk180)();
	vs_top=sup;
	V194= vs_base[0];
	base[2]= number_plus(V194,small_fixnum(2));
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk168)();
	vs_top=sup;
	base[1]= vs_base[0];
	base[2]= VV[102];
	base[3]= (VV[185]->s.s_gfdef);
	base[4]= VV[103];
	base[5]= (VV[186]->s.s_gfdef);
	vs_top=(vs_base=base+0)+6;
	Lmember();
	vs_top=sup;
	V193= vs_base[0];
	if(((V193))==Cnil){
	goto T904;}
	if(!((CMPcadar((V193)))==(VV[90]))){
	goto T894;}
	base[1]= (V191);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	V199= vs_base[0];
	V200= (VV[186]->s.s_gfdef);
	V201= (VV[185]->s.s_gfdef);
	V202= (VFUN_NARGS=6,(*(LnkLI187))(V199,(V193),VV[105],V200,VV[102],V201));
	{object V197;
	object V198= /* INLINE-ARGS */V202;
	if(V198==Cnil){
	V196= Cnil;
	goto T921;}
	base[0]=V197=MMcons(Cnil,Cnil);
T922:;
	(V197->c.c_car)= CMPcar((V198->c.c_car));
	if((V198=MMcdr(V198))==Cnil){
	V196= base[0];
	goto T921;}
	V197=MMcdr(V197)=MMcons(Cnil,Cnil);
	goto T922;}
T921:;
	{register object x= (V192),V195= V196;
	while(V195!=Cnil)
	if(eql(x,V195->c.c_car)){
	goto T920;
	}else V195=V195->c.c_cdr;
	goto T894;}
T920:;
	base[1]= (V191);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	base[0]= vs_base[0];
	base[1]= (V192);
	base[2]= Ct;
	vs_top=(vs_base=base+0)+3;
	(void) (*Lnk192)();
	vs_top=sup;
	goto T894;
T904:;
	base[1]= (V191);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk183)();
	vs_top=sup;
	base[0]= vs_base[0];
	base[1]= (V192);
	base[2]= Cnil;
	vs_top=(vs_base=base+0)+3;
	(void) (*Lnk192)();
	vs_top=sup;
T894:;
	base[0]= (VV[48]->s.s_dbind);
	base[1]= VV[116];
	base[2]= (V191);
	base[3]= (V192);
	vs_top=(vs_base=base+0)+4;
	Lformat();
	vs_top=sup;
	{object V203 = vs_base[0];
	VMR31(V203)}}
}
/*	local entry for function BREAK-BACKWARD-SEARCH-STACK	*/

static object LI32(V205)

object V205;
{	 VMB32 VMS32 VMV32
TTL:;
	{register object V206;
	V206= Cnil;
	V206= coerce_to_string((V205));
	{register object V207;
	register object V208;
	V207= one_minus((VV[39]->s.s_dbind));
	base[2]= (V207);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk164)();
	vs_top=sup;
	V208= vs_base[0];
T947:;
	if(!(number_compare((V207),(VV[37]->s.s_dbind))<0)){
	goto T948;}
	base[2]= (VV[48]->s.s_dbind);
	base[3]= VV[117];
	base[4]= (V206);
	vs_top=(vs_base=base+2)+3;
	Lformat();
	vs_top=sup;
	{object V209 = vs_base[0];
	VMR32(V209)}
T948:;
	if(((*(LnkLI163))((V207)))==Cnil){
	goto T955;}
	V210= symbol_name((V208));
	V211= (VV[194]->s.s_gfdef);
	if(((VFUN_NARGS=4,(*(LnkLI193))((V206),/* INLINE-ARGS */V210,VV[103],V211)))==Cnil){
	goto T955;}
	base[2]= (V207);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk195)();
	vs_top=sup;
	{object V212 = Cnil;
	VMR32(V212)}
T955:;
	V207= one_minus((V207));
	base[2]= (V207);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk164)();
	vs_top=sup;
	V208= vs_base[0];
	goto T947;}}
}
/*	local entry for function BREAK-FORWARD-SEARCH-STACK	*/

static object LI33(V214)

object V214;
{	 VMB33 VMS33 VMV33
TTL:;
	{register object V215;
	V215= Cnil;
	V215= coerce_to_string((V214));
	{register object V216;
	register object V217;
	V216= one_plus((VV[39]->s.s_dbind));
	base[2]= (V216);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk164)();
	vs_top=sup;
	V217= vs_base[0];
T976:;
	if(!(number_compare((V216),(VV[38]->s.s_dbind))>0)){
	goto T977;}
	base[2]= (VV[48]->s.s_dbind);
	base[3]= VV[118];
	base[4]= (V215);
	vs_top=(vs_base=base+2)+3;
	Lformat();
	vs_top=sup;
	{object V218 = vs_base[0];
	VMR33(V218)}
T977:;
	if(((*(LnkLI163))((V216)))==Cnil){
	goto T984;}
	V219= symbol_name((V217));
	V220= (VV[194]->s.s_gfdef);
	if(((VFUN_NARGS=4,(*(LnkLI193))((V215),/* INLINE-ARGS */V219,VV[103],V220)))==Cnil){
	goto T984;}
	base[2]= (V216);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk195)();
	vs_top=sup;
	{object V221 = Cnil;
	VMR33(V221)}
T984:;
	V216= one_plus((V216));
	base[2]= (V216);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk164)();
	vs_top=sup;
	V217= vs_base[0];
	goto T976;}}
}
/*	local entry for function BREAK-HELP	*/

static object LI34()

{	 VMB34 VMS34 VMV34
TTL:;
	{register object V222;
	register object V223;
	V222= VV[119];
	V223= CMPcar((V222));
T1002:;
	if(!(((V222))==Cnil)){
	goto T1003;}
	goto T998;
T1003:;
	base[1]= (VV[48]->s.s_dbind);
	base[2]= (V223);
	vs_top=(vs_base=base+1)+2;
	Lformat();
	vs_top=sup;
	V222= CMPcdr((V222));
	V223= CMPcar((V222));
	goto T1002;}
T998:;
	vs_base=vs_top=base+0;
	vs_base[0]=Cnil;
	vs_top=sup;
	{object V224 = vs_base[0];
	VMR34(V224)}
}
/*	function definition for COERCE-SLASH-TERMINATED	*/

static L35()
{register object *base=vs_base;
	register object *sup=base+VM35; VC35
	vs_check;
	{register object V225;
	V225=(base[0]);
	vs_top=sup;
TTL:;
	if(!(type_of((V225))==t_string)){
	goto T1016;}
	goto T1015;
T1016:;
	base[1]= VV[120];
	base[2]= (V225);
	vs_top=(vs_base=base+1)+2;
	Lerror();
	vs_top=sup;
T1015:;
	{int V226;
	V226= ((V225))->v.v_fillp;
	if(!((V226)>(0))){
	goto T1022;}
	if((((V225))->ust.ust_self[(V226)-(1)])==(47)){
	goto T1020;}
T1022:;
	base[1]= Cnil;
	base[2]= VV[122];
	base[3]= (V225);
	vs_top=(vs_base=base+1)+3;
	Lformat();
	vs_top=sup;
	V225= vs_base[0];}
T1020:;
	base[1]= (V225);
	vs_top=(vs_base=base+1)+1;
	return;
	}
}
/*	function definition for FIX-LOAD-PATH	*/

static L36()
{register object *base=vs_base;
	register object *sup=base+VM36; VC36
	vs_check;
	{object V227;
	V227=(base[0]);
	vs_top=sup;
TTL:;
	if(equal((V227),(VV[123]->s.s_dbind))){
	goto T1030;}
	{register object V228;
	V228= (V227);
T1035:;
	if(!(type_of((V228))!=t_cons)){
	goto T1036;}
	goto T1033;
T1036:;
	{register object V230;
	base[2]= CMPcar((V228));
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk196)();
	vs_top=sup;
	V230= vs_base[0];
	(V228)->c.c_car = (V230);}
	V228= CMPcdr((V228));
	goto T1035;}
T1033:;
	{register object V231;
	V231= (V227);
T1048:;
	if(!(type_of((V231))!=t_cons)){
	goto T1049;}
	goto T1030;
T1049:;
	{register object V232;
	V232= (V231);
T1055:;
	if(!(type_of(CMPcdr((V232)))!=t_cons)){
	goto T1056;}
	goto T1053;
T1056:;
	if(!(equal(CMPcadr((V232)),CMPcar((V231))))){
	goto T1060;}
	{register object V233;
	register object V234;
	V233= (V232);
	V234= CMPcddr((V232));
	((V233))->c.c_cdr = (V234);}
T1060:;
	V232= CMPcdr((V232));
	goto T1055;}
T1053:;
	V231= CMPcdr((V231));
	goto T1048;}
T1030:;
	(VV[123]->s.s_dbind)= (V227);
	base[1]= (VV[123]->s.s_dbind);
	vs_top=(vs_base=base+1)+1;
	return;
	}
}
/*	function definition for FILE-SEARCH	*/

static L37()
{register object *base=vs_base;
	register object *sup=base+VM37; VC37
	vs_check;
	bds_check;
	{register object V235;
	object V236;
	object V237;
	object V238;
	V235=(base[0]);
	vs_base=vs_base+1;
	if(vs_base>=vs_top){vs_top=sup;goto T1073;}
	V236=(base[1]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T1074;}
	V237=(base[2]);
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T1075;}
	V238=(base[3]);
	vs_top=sup;
	goto T1076;
T1073:;
	V236= (VV[124]->s.s_dbind);
T1074:;
	V237= (VV[125]->s.s_dbind);
T1075:;
	V238= Ct;
T1076:;
	{register object V239;
	V239= Cnil;
	base[4]= (V236);
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk197)();
	vs_top=sup;
	{register object V240;
	register object V241;
	V240= (V236);
	V241= CMPcar((V240));
T1087:;
	if(!(((V240))==Cnil)){
	goto T1088;}
	goto T1083;
T1088:;
	{register object V242;
	register object V243;
	V242= (V237);
	V243= CMPcar((V242));
T1096:;
	if(!(((V242))==Cnil)){
	goto T1097;}
	goto T1092;
T1097:;
	base[6]= (V241);
	base[7]= (V235);
	base[8]= (V243);
	vs_top=(vs_base=base+6)+3;
	(void) (*Lnk147)();
	vs_top=sup;
	V239= vs_base[0];
	if(!((file_exists((V239))))){
	goto T1101;}
	base[6]= (V239);
	vs_top=(vs_base=base+6)+1;
	return;
T1101:;
	V242= CMPcdr((V242));
	V243= CMPcar((V242));
	goto T1096;}
T1092:;
	V240= CMPcdr((V240));
	V241= CMPcar((V240));
	goto T1087;}
T1083:;
	if(((V238))==Cnil){
	goto T1119;}
	bds_bind(VV[126],Cnil);
	base[5]= VV[127];
	base[6]= VV[128];
	base[7]= (V236);
	base[8]= (V235);
	base[9]= (V237);
	vs_top=(vs_base=base+5)+5;
	Lcerror();
	vs_top=sup;
	base[5]= (VV[126]->s.s_dbind);
	vs_top=(vs_base=base+5)+1;
	bds_unwind1;
	return;
T1119:;
	base[4]= Cnil;
	vs_top=(vs_base=base+4)+1;
	return;}
	}
}
/*	function definition for ALOAD	*/

static L38()
{register object *base=vs_base;
	register object *sup=base+VM38; VC38
	vs_check;
	{object V244;
	V244=(base[0]);
	vs_top=sup;
TTL:;
	base[2]= (V244);
	base[3]= (VV[124]->s.s_dbind);
	base[4]= (VV[125]->s.s_dbind);
	vs_top=(vs_base=base+2)+3;
	(void) (*Lnk198)();
	vs_top=sup;
	base[1]= vs_base[0];
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk140)();
	return;
	}
}
/*	function definition for AUTOLOAD	*/

static L39()
{register object *base=vs_base;
	register object *sup=base+VM39; VC39
	vs_check;
	bds_check;
	base[0]=MMcons(base[0],Cnil);
	base[1]=MMcons(base[1],base[0]);
	vs_top=sup;
	bds_bind(VV[129],Ct);
	base[3]= (base[0]->c.c_car);
	vs_top=(vs_base=base+3)+1;
	Lfboundp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T1131;}
	base[3]= Ct;
	vs_top=(vs_base=base+3)+1;
	bds_unwind1;
	return;
T1131:;
	base[3]= (base[0]->c.c_car);
	base[4]= 
	make_cclosure_new(LC45,Cnil,base[1],Cdata);
	vs_top=(vs_base=base+3)+2;
	siLfset();
	bds_unwind1;
	return;
}
/*	function definition for AUTOLOAD-MACRO	*/

static L40()
{register object *base=vs_base;
	register object *sup=base+VM40; VC40
	vs_check;
	bds_check;
	base[0]=MMcons(base[0],Cnil);
	base[1]=MMcons(base[1],base[0]);
	vs_top=sup;
	bds_bind(VV[129],Ct);
	base[3]= (base[0]->c.c_car);
	vs_top=(vs_base=base+3)+1;
	Lfboundp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T1136;}
	base[3]= Ct;
	vs_top=(vs_base=base+3)+1;
	bds_unwind1;
	return;
T1136:;
	{object V245;
	object V246;
	V245= (base[0]->c.c_car);
	V246= 
	make_cclosure_new(LC46,Cnil,base[1],Cdata);
	base[4]= (V245);
	base[5]= make_cons(VV[130],(V246));
	vs_top=(vs_base=base+4)+2;
	siLfset();
	vs_top=sup;
	base[4]= (V246);
	vs_top=(vs_base=base+4)+1;
	bds_unwind1;
	return;}
}
/*	function definition for GET-COMMAND-ARG	*/

static L41()
{register object *base=vs_base;
	register object *sup=base+VM41; VC41
	vs_check;
	{register object V247;
	register object V248;
	V247=(base[0]);
	vs_base=vs_base+1;
	if(vs_base>=vs_top){vs_top=sup;goto T1144;}
	V248=(base[1]);
	vs_top=sup;
	goto T1145;
T1144:;
	V248= Cnil;
T1145:;
	{register object V249;
	V249= (VV[16]->s.s_dbind);
T1149:;
	V249= CMPcdr((V249));
	if((V249)!=Cnil){
	goto T1153;}
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;
T1153:;
	{register object V250;
	V250= CMPcar((V249));
	if(!((((V250))->ust.ust_self[0])==(((V247))->ust.ust_self[0]))){
	goto T1150;}
	if(!((((V250))->ust.ust_self[1])==(((V247))->ust.ust_self[1]))){
	goto T1150;}
	if(!(equal((V250),(V247)))){
	goto T1150;}
	{register object V251;
	V251= (V248);
	if(((V251))==Cnil){
	goto T1162;}
	base[2]= (V251);
	vs_top=(vs_base=base+2)+1;
	return;
T1162:;
	if((CMPcadr((V249)))==Cnil){
	goto T1165;}
	base[2]= CMPcadr((V249));
	base[3]= CMPcdr((V249));
	vs_top=(vs_base=base+2)+2;
	return;
T1165:;
	base[2]= Ct;
	vs_top=(vs_base=base+2)+1;
	return;}}
T1150:;
	goto T1149;}
	}
}
/*	function definition for SET-DIR	*/

static L42()
{register object *base=vs_base;
	register object *sup=base+VM42; VC42
	vs_check;
	{object V252;
	object V253;
	V252=(base[0]);
	V253=(base[1]);
	vs_top=sup;
TTL:;
	{object V254;{object V255;
	base[2]= (V253);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk199)();
	vs_top=sup;
	V255= vs_base[0];
	if(V255==Cnil)goto T1171;
	V254= V255;
	goto T1170;
T1171:;}
	base[2]= (V252);
	vs_top=(vs_base=base+2)+1;
	Lsymbol_value();
	vs_top=sup;
	V254= vs_base[0];
T1170:;
	if(((V254))==Cnil){
	goto T1176;}
	base[2]= (V252);
	base[4]= (V254);
	vs_top=(vs_base=base+4)+1;
	(void) (*Lnk196)();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	Lset();
	return;
T1176:;
	base[2]= Cnil;
	vs_top=(vs_base=base+2)+1;
	return;}
	}
}
/*	function definition for SET-UP-TOP-LEVEL	*/

static L43()
{register object *base=vs_base;
	register object *sup=base+VM43; VC43
	vs_check;
	bds_check;
	vs_top=sup;
TTL:;
	{register int V256;
	register object V257;
	vs_base=vs_top;
	(void) (*Lnk200)();
	vs_top=sup;
	V256= fix(vs_base[0]);
	V257= Cnil;
T1185:;
	V256= (V256)-(1);
	if(!((V256)<(0))){
	goto T1190;}
	goto T1183;
T1190:;
	base[0]= make_fixnum(V256);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk201)();
	vs_top=sup;
	V258= vs_base[0];
	V257= make_cons(V258,(V257));
	goto T1185;
T1183:;
	(VV[16]->s.s_dbind)= (V257);
	V257= (VV[131]->s.s_dbind);
	{object V259;
	base[0]= VV[132];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk202)();
	vs_top=sup;
	V259= vs_base[0];{object V260;
	base[0]= VV[131];
	base[1]= VV[133];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk203)();
	vs_top=sup;
	V260= vs_base[0];
	if(V260==Cnil)goto T1203;
	goto T1202;
T1203:;}
	if(((V259))==Cnil){
	goto T1202;}
	base[0]= (V259);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk196)();
	vs_top=sup;
	(VV[131]->s.s_dbind)= vs_base[0];
T1202:;
	if(((VV[124]->s.s_dbind))==Cnil){
	goto T1212;}
	if(equal((V257),(VV[131]->s.s_dbind))){
	goto T1211;}
T1212:;
	base[0]= (VV[131]->s.s_dbind);
	base[1]= VV[134];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk147)();
	vs_top=sup;
	V261= vs_base[0];
	(VV[124]->s.s_dbind)= make_cons(V261,(VV[124]->s.s_dbind));
	base[0]= (VV[131]->s.s_dbind);
	base[1]= VV[135];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk147)();
	vs_top=sup;
	V262= vs_base[0];
	(VV[124]->s.s_dbind)= make_cons(V262,(VV[124]->s.s_dbind));
T1211:;
	base[0]= VV[136];
	base[1]= VV[137];
	vs_top=(vs_base=base+0)+2;
	(void) (*Lnk203)();
	vs_top=sup;
	base[0]= VV[138];
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk199)();
	if(vs_base<vs_top){
	V257= vs_base[0];
	vs_base++;
	}else{
	V257= Cnil;}
	if(vs_base<vs_top){
	V257= vs_base[0];
	}else{
	V257= Cnil;}
	vs_top=sup;
	if(((V257))==Cnil){
	goto T1229;}
	bds_bind(VV[15],Cnil);
	base[1]= (VV[16]->s.s_dbind);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk141)();
	vs_top=sup;
	(VV[16]->s.s_dbind)= (V257);
	base[1]= CMPcar((VV[16]->s.s_dbind));
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk204)();
	bds_unwind1;
	return;
T1229:;
	base[0]= Cnil;
	vs_top=(vs_base=base+0)+1;
	return;}}
}
/*	function definition for DO-F	*/

static L44()
{register object *VOL base=vs_base;
	register object *VOL sup=base+VM44; VC44
	vs_check;
	bds_check;
	{VOL object V263;
	V263=(base[0]);
	vs_top=sup;
TTL:;
	base[1]= VV[139];
	base[2]= Cnil;
	bds_bind(VV[43],Cnil);
	frs_push(FRS_CATCH,(VV[11]->s.s_dbind));
	if(nlj_active)
	{nlj_active=FALSE;frs_pop();
	vs_top=sup;
	goto T1238;}
	else{
	base[5]= (V263);
	vs_top=(vs_base=base+5)+1;
	(void) (*Lnk205)();
	vs_top=sup;
	base[4]= vs_base[0];
	{object tag;frame_ptr fr;object p;bool active;
	frs_push(FRS_PROTECT,Cnil);
	if(nlj_active){tag=nlj_tag;fr=nlj_fr;active=TRUE;}
	else{
	base[6]= base[4];
	vs_top=(vs_base=base+6)+1;
	Lread_line();
	vs_top=sup;
T1247:;
	base[6]= base[4];
	base[7]= Cnil;
	base[8]= base[1];
	vs_top=(vs_base=base+6)+3;
	Lread();
	vs_top=sup;
	base[2]= vs_base[0];
	if(!((base[1])==(base[2]))){
	goto T1254;}
	base[6]= Cnil;
	vs_top=(vs_base=base+6)+1;
	goto T1243;
T1254:;
	base[6]= base[2];
	vs_top=(vs_base=base+6)+1;
	Leval();
	vs_top=sup;
	goto T1247;
T1243:;
	active=FALSE;}
	base[5]=Cnil;
	while(vs_base<vs_top)
	{base[5]=MMcons(vs_top[-1],base[5]);vs_top--;}
	vs_top=sup;
	nlj_active=FALSE;frs_pop();
	base[6]= base[4];
	vs_top=(vs_base=base+6)+1;
	Lclose();
	vs_top=sup;
	vs_base=vs_top=base+6;
	for(p= base[5];!endp(p);p=MMcdr(p))vs_push(MMcar(p));
	if(active)unwind(fr,tag);else{
	vs_top=sup;}}
	vs_base=vs_top;
	Lby();
	vs_top=sup;
	frs_pop();}
T1238:;
	base[4]= small_fixnum(1);
	vs_top=(vs_base=base+4)+1;
	Lby();
	bds_unwind1;
	return;
	}
}
/*	local function CLOSURE	*/

static LC46(base0)
register object *base0;
{	register object *base=vs_base;
	register object *sup=base+VM45; VC45
	vs_check;
	{object V264;
	object V265;
	V264=(base[0]);
	V265=(base[1]);
	vs_top=sup;
	base[2]= (base0[0]->c.c_car);
	vs_top=(vs_base=base+2)+1;
	(void) (*Lnk206)();
	vs_top=sup;
	base[2]= (V264);
	base[3]= (V265);
	vs_top=(vs_base=base+2)+2;
	super_funcall_no_event((base0[1]->c.c_car));
	return;
	}
}
/*	local function CLOSURE	*/

static LC45(base0)
register object *base0;
{	register object *base=vs_base;
	register object *sup=base+VM46; VC46
	vs_check;
	{object V266;
	vs_top[0]=Cnil;
	{object *p=vs_top;
	 for(;p>vs_base;p--)p[-1]=MMcons(p[-1],p[0]);}
	V266=(base[0]);
	vs_top=sup;
	base[1]= (base0[0]->c.c_car);
	vs_top=(vs_base=base+1)+1;
	(void) (*Lnk206)();
	vs_top=sup;
	base[1]= (base0[1]->c.c_car);
	{object V267;
	V267= (V266);
	 vs_top=base+2;
	 while(V267!=Cnil)
	 {vs_push((V267)->c.c_car);V267=(V267)->c.c_cdr;}
	vs_base=base+2;}
	super_funcall_no_event(base[1]);
	return;
	}
}
static LnkT206(){ call_or_link(VV[206],&Lnk206);} /* ALOAD */
static LnkT205(){ call_or_link(VV[205],&Lnk205);} /* OPEN */
static LnkT204(){ call_or_link(VV[204],&Lnk204);} /* DO-F */
static LnkT203(){ call_or_link(VV[203],&Lnk203);} /* SET-DIR */
static LnkT202(){ call_or_link(VV[202],&Lnk202);} /* GETENV */
static LnkT201(){ call_or_link(VV[201],&Lnk201);} /* ARGV */
static LnkT200(){ call_or_link(VV[200],&Lnk200);} /* ARGC */
static LnkT199(){ call_or_link(VV[199],&Lnk199);} /* GET-COMMAND-ARG */
static LnkT198(){ call_or_link(VV[198],&Lnk198);} /* FILE-SEARCH */
static LnkT197(){ call_or_link(VV[197],&Lnk197);} /* FIX-LOAD-PATH */
static LnkT196(){ call_or_link(VV[196],&Lnk196);} /* COERCE-SLASH-TERMINATED */
static LnkT195(){ call_or_link(VV[195],&Lnk195);} /* BREAK-GO */
static object  LnkTLI193(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_vproc(VV[193],&LnkLI193,ap);} /* SEARCH */
static LnkT192(){ call_or_link(VV[192],&Lnk192);} /* INTERNAL-SUPER-GO */
static object  LnkTLI191(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[191],&LnkLI191,2,ap);} /* LIST-DELQ */
static LnkT190(){ call_or_link(VV[190],&Lnk190);} /* IHS-NOT-INTERPRETED-ENV */
static LnkT189(){ call_or_link(VV[189],&Lnk189);} /* COMPILED-FUNCTION-NAME */
static object  LnkTLI187(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_vproc(VV[187],&LnkLI187,ap);} /* REMOVE */
static LnkT184(){ call_or_link(VV[184],&Lnk184);} /* SPICEP */
static LnkT183(){ call_or_link(VV[183],&Lnk183);} /* FRS-TAG */
static LnkT181(){ call_or_link(VV[181],&Lnk181);} /* FRS-CLASS */
static LnkT180(){ call_or_link(VV[180],&Lnk180);} /* FRS-VS */
static object  LnkTLI179(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[179],&LnkLI179,1,ap);} /* FRS-KIND */
static LnkT175(){ call_or_link(VV[175],&Lnk175);} /* IHS-FUN */
static LnkT174(){ call_or_link(VV[174],&Lnk174);} /* FRS-IHS */
static LnkT173(){ call_or_link(VV[173],&Lnk173);} /* BDS-VAL */
static object  LnkTLI172(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[172],&LnkLI172,1,ap);} /* PRINT-FRS */
static LnkT171(){ call_or_link(VV[171],&Lnk171);} /* BDS-VAR */
static LnkT170(){ call_or_link(VV[170],&Lnk170);} /* FRS-BDS */
static LnkT169(){ call_or_link(VV[169],&Lnk169);} /* BREAK-VS */
static LnkT168(){ call_or_link(VV[168],&Lnk168);} /* VS */
static object  LnkTLI167(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[167],&LnkLI167,1,ap);} /* PRINT-IHS */
static LnkT166(){ call_or_link(VV[166],&Lnk166);} /* IHS-VS */
static LnkT165(){ call_or_link(VV[165],&Lnk165);} /* BREAK-PREVIOUS */
static LnkT164(){ call_or_link(VV[164],&Lnk164);} /* IHS-FNAME */
static object  LnkTLI163(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[163],&LnkLI163,1,ap);} /* IHS-VISIBLE */
static object  LnkTLI162(){return call_proc0(VV[162],&LnkLI162);} /* SET-ENV */
static LnkT161(){ call_or_link(VV[161],&Lnk161);} /* BREAK-LEVEL */
static LnkT160(){ call_or_link(VV[160],&Lnk160);} /* BREAK */
static LnkT157(){ call_or_link(VV[157],&Lnk157);} /* DBL-READ */
static LnkT156(){ call_or_link(VV[156],&Lnk156);} /* SET-BACK */
static object  LnkTLI155(){return call_proc0(VV[155],&LnkLI155);} /* SET-CURRENT */
static LnkT154(){ call_or_link(VV[154],&Lnk154);} /* CATCH-FATAL */
static LnkT153(){ call_or_link(VV[153],&Lnk153);} /* BREAK-QUIT */
static LnkT152(){ call_or_link(VV[152],&Lnk152);} /* SIMPLE-BACKTRACE */
static LnkT151(){ call_or_link(VV[151],&Lnk151);} /* FRS-TOP */
static LnkT150(){ call_or_link(VV[150],&Lnk150);} /* SCH-FRS-BASE */
static LnkT149(){ call_or_link(VV[149],&Lnk149);} /* IHS-TOP */
static LnkT148(){ call_or_link(VV[148],&Lnk148);} /* MAKE-STRING-INPUT-STREAM */
static LnkT147(){ call_or_link(VV[147],&Lnk147);} /* STRING-CONCATENATE */
static LnkT146(){ call_or_link(VV[146],&Lnk146);} /* READ-FROM-STRING */
static LnkT145(){ call_or_link(VV[145],&Lnk145);} /* BREAK-CURRENT */
static LnkT141(){ call_or_link(VV[141],&Lnk141);} /* PROCESS-SOME-ARGS */
static LnkT140(){ call_or_link(VV[140],&Lnk140);} /* LOAD */
