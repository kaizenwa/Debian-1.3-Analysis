
#include <cmpinclude.h>
#include "numlib.h"
init_numlib(){do_init(VV);}
/*	local entry for function ISQRT	*/

static object LI1(V2)

register object V2;
{	 VMB1 VMS1 VMV1
TTL:;
	if(!(type_of((V2))==t_fixnum||type_of((V2))==t_bignum)){
	goto T2;}
	if(number_compare((V2),small_fixnum(0))>=0){
	goto T1;}
T2:;
	base[0]= VV[0];
	base[1]= (V2);
	vs_top=(vs_base=base+0)+2;
	Lerror();
	vs_top=sup;
T1:;
	if(!(number_compare(small_fixnum(0),(V2))==0)){
	goto T9;}
	{object V3 = small_fixnum(0);
	VMR1(V3)}
T9:;
	{object V4;
	base[0]= (V2);
	vs_top=(vs_base=base+0)+1;
	Linteger_length();
	vs_top=sup;
	V4= vs_base[0];
	{register object V5;
	register object V6;
	base[0]= small_fixnum(1);
	base[2]= (V4);
	base[3]= small_fixnum(2);
	vs_top=(vs_base=base+2)+2;
	Lceiling();
	vs_top=sup;
	base[1]= vs_base[0];
	vs_top=(vs_base=base+0)+2;
	Lash();
	vs_top=sup;
	V5= vs_base[0];
	V6= Cnil;
T19:;
	base[0]= (V2);
	base[1]= (V5);
	vs_top=(vs_base=base+0)+2;
	Lfloor();
	vs_top=sup;
	V6= vs_base[0];
	if(!(number_compare((V5),(V6))<=0)){
	goto T26;}
	{object V7 = (V5);
	VMR1(V7)}
T26:;
	base[0]= number_plus((V5),(V6));
	base[1]= small_fixnum(2);
	vs_top=(vs_base=base+0)+2;
	Lfloor();
	vs_top=sup;
	V5= vs_base[0];
	goto T19;}}
}
/*	local entry for function ABS	*/

static object LI2(V9)

register object V9;
{	 VMB2 VMS2 VMV2
TTL:;
	base[0]= (V9);
	vs_top=(vs_base=base+0)+1;
	Lcomplexp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T36;}
	{register object V10;
	register object V11;
	base[2]= (V9);
	vs_top=(vs_base=base+2)+1;
	Lrealpart();
	vs_top=sup;
	V12= vs_base[0];
	V10= (*(LnkLI13))(V12);
	base[2]= (V9);
	vs_top=(vs_base=base+2)+1;
	Limagpart();
	vs_top=sup;
	V13= vs_base[0];
	V11= (*(LnkLI13))(V13);
	if(!(number_compare((V10),(V11))<0)){
	goto T45;}
	{object V14;
	object V15;
	V14= (V11);
	V15= (V10);
	V10= (V14);
	V11= (V15);}
T45:;
	if(!(number_compare(small_fixnum(0),(V10))==0)){
	goto T55;}
	{object V16 = (V10);
	VMR2(V16)}
T55:;
	{object V17;
	base[2]= (V11);
	base[3]= (V10);
	vs_top=(vs_base=base+2)+2;
	Ldivide();
	vs_top=sup;
	V17= vs_base[0];
	V19= number_times((V17),(V17));
	base[2]= number_plus(small_fixnum(1),/* INLINE-ARGS */V19);
	vs_top=(vs_base=base+2)+1;
	Lsqrt();
	vs_top=sup;
	V18= vs_base[0];
	{object V20 = number_times((V10),V18);
	VMR2(V20)}}}
T36:;
	if(!(number_compare(small_fixnum(0),(V9))>0)){
	goto T63;}
	{object V21 = number_negate((V9));
	VMR2(V21)}
T63:;
	{object V22 = (V9);
	VMR2(V22)}
}
/*	local entry for function PHASE	*/

static object LI3(V24)

object V24;
{	 VMB3 VMS3 VMV3
TTL:;
	base[1]= (V24);
	vs_top=(vs_base=base+1)+1;
	Limagpart();
	vs_top=sup;
	base[0]= vs_base[0];
	base[2]= (V24);
	vs_top=(vs_base=base+2)+1;
	Lrealpart();
	vs_top=sup;
	base[1]= vs_base[0];
	vs_top=(vs_base=base+0)+2;
	Latan();
	vs_top=sup;
	{object V25 = vs_base[0];
	VMR3(V25)}
}
/*	local entry for function SIGNUM	*/

static object LI4(V27)

register object V27;
{	 VMB4 VMS4 VMV4
TTL:;
	if(!(number_compare(small_fixnum(0),(V27))==0)){
	goto T70;}
	{object V28 = (V27);
	VMR4(V28)}
T70:;
	base[0]= (V27);
	base[1]= (*(LnkLI13))((V27));
	vs_top=(vs_base=base+0)+2;
	Ldivide();
	vs_top=sup;
	{object V29 = vs_base[0];
	VMR4(V29)}
}
/*	local entry for function CIS	*/

static object LI5(V31)

object V31;
{	 VMB5 VMS5 VMV5
TTL:;
	base[0]= number_times(VV[1],(V31));
	vs_top=(vs_base=base+0)+1;
	Lexp();
	vs_top=sup;
	{object V32 = vs_base[0];
	VMR5(V32)}
}
/*	local entry for function ASIN	*/

static object LI6(V34)

register object V34;
{	 VMB6 VMS6 VMV6
TTL:;
	{object V35;
	V37= number_times(VV[1],(V34));
	V39= number_times((V34),(V34));
	base[1]= number_minus(VV[2],/* INLINE-ARGS */V39);
	vs_top=(vs_base=base+1)+1;
	Lsqrt();
	vs_top=sup;
	V38= vs_base[0];
	base[0]= number_plus(/* INLINE-ARGS */V37,V38);
	vs_top=(vs_base=base+0)+1;
	Llog();
	vs_top=sup;
	V36= vs_base[0];
	V40= number_times(VV[1],V36);
	V35= number_negate(/* INLINE-ARGS */V40);
	base[0]= (V34);
	vs_top=(vs_base=base+0)+1;
	Lcomplexp();
	vs_top=sup;
	if((vs_base[0])!=Cnil){
	goto T82;}
	if(!(number_compare((V34),VV[2])<=0)){
	goto T82;}
	if(number_compare((V34),VV[3])>=0){
	goto T80;}
T82:;
	base[0]= (V35);
	vs_top=(vs_base=base+0)+1;
	Limagpart();
	vs_top=sup;
	V41= vs_base[0];
	if(!(number_compare(small_fixnum(0),V41)==0)){
	goto T81;}
T80:;
	base[0]= (V35);
	vs_top=(vs_base=base+0)+1;
	Lrealpart();
	vs_top=sup;
	{object V42 = vs_base[0];
	VMR6(V42)}
T81:;
	{object V43 = (V35);
	VMR6(V43)}}
}
/*	local entry for function ACOS	*/

static object LI7(V45)

register object V45;
{	 VMB7 VMS7 VMV7
TTL:;
	{object V46;
	V49= number_times((V45),(V45));
	base[1]= number_minus(VV[2],/* INLINE-ARGS */V49);
	vs_top=(vs_base=base+1)+1;
	Lsqrt();
	vs_top=sup;
	V48= vs_base[0];
	V50= number_times(VV[1],V48);
	base[0]= number_plus((V45),/* INLINE-ARGS */V50);
	vs_top=(vs_base=base+0)+1;
	Llog();
	vs_top=sup;
	V47= vs_base[0];
	V51= number_times(VV[1],V47);
	V46= number_negate(/* INLINE-ARGS */V51);
	base[0]= (V45);
	vs_top=(vs_base=base+0)+1;
	Lcomplexp();
	vs_top=sup;
	if((vs_base[0])!=Cnil){
	goto T100;}
	if(!(number_compare((V45),VV[2])<=0)){
	goto T100;}
	if(number_compare((V45),VV[3])>=0){
	goto T98;}
T100:;
	base[0]= (V46);
	vs_top=(vs_base=base+0)+1;
	Limagpart();
	vs_top=sup;
	V52= vs_base[0];
	if(!(number_compare(small_fixnum(0),V52)==0)){
	goto T99;}
T98:;
	base[0]= (V46);
	vs_top=(vs_base=base+0)+1;
	Lrealpart();
	vs_top=sup;
	{object V53 = vs_base[0];
	VMR7(V53)}
T99:;
	{object V54 = (V46);
	VMR7(V54)}}
}
/*	local entry for function SINH	*/

static object LI8(V56)

register object V56;
{	 VMB8 VMS8 VMV8
TTL:;
	base[0]= (V56);
	vs_top=(vs_base=base+0)+1;
	Lcomplexp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T112;}
	{object V57;
	object V58;
	base[0]= (V56);
	vs_top=(vs_base=base+0)+1;
	Lrealpart();
	vs_top=sup;
	V57= vs_base[0];
	base[0]= (V56);
	vs_top=(vs_base=base+0)+1;
	Limagpart();
	vs_top=sup;
	V58= vs_base[0];
	V59= (*(LnkLI14))((V57));
	base[1]= (V58);
	vs_top=(vs_base=base+1)+1;
	Lcos();
	vs_top=sup;
	V60= vs_base[0];
	base[0]= number_times(/* INLINE-ARGS */V59,V60);
	V61= (*(LnkLI15))((V57));
	base[2]= (V58);
	vs_top=(vs_base=base+2)+1;
	Lsin();
	vs_top=sup;
	V62= vs_base[0];
	base[1]= number_times(/* INLINE-ARGS */V61,V62);
	vs_top=(vs_base=base+0)+2;
	Lcomplex();
	vs_top=sup;
	{object V63 = vs_base[0];
	VMR8(V63)}}
T112:;
	base[0]= number_negate(VV[4]);
	base[1]= (V56);
	base[2]= VV[4];
	vs_top=(vs_base=base+0)+3;
	Lmonotonically_increasing();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T126;}
	base[0]= (V56);
	base[2]= (V56);
	vs_top=(vs_base=base+2)+1;
	Lexp();
	vs_top=sup;
	base[1]= vs_base[0];
	V65= number_times(VV[7],(V56));
	V66= number_minus(VV[6],/* INLINE-ARGS */V65);
	V67= number_times((V56),/* INLINE-ARGS */V66);
	V68= number_minus(VV[5],/* INLINE-ARGS */V67);
	V69= number_times((V56),/* INLINE-ARGS */V68);
	V70= number_minus(small_fixnum(1),/* INLINE-ARGS */V69);
	V71= number_times((V56),/* INLINE-ARGS */V70);
	base[2]= number_minus(small_fixnum(1),/* INLINE-ARGS */V71);
	vs_top=(vs_base=base+0)+3;
	Ltimes();
	vs_top=sup;
	{object V72 = vs_base[0];
	VMR8(V72)}
T126:;
	{object V73;
	base[0]= (V56);
	vs_top=(vs_base=base+0)+1;
	Lexp();
	vs_top=sup;
	V73= vs_base[0];
	base[0]= (V73);
	vs_top=(vs_base=base+0)+1;
	Ldivide();
	vs_top=sup;
	V74= vs_base[0];
	V75= number_minus((V73),V74);
	{object V76 = number_times(VV[8],/* INLINE-ARGS */V75);
	VMR8(V76)}}
}
/*	local entry for function COSH	*/

static object LI9(V78)

register object V78;
{	 VMB9 VMS9 VMV9
TTL:;
	base[0]= (V78);
	vs_top=(vs_base=base+0)+1;
	Lcomplexp();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T140;}
	{object V79;
	object V80;
	base[0]= (V78);
	vs_top=(vs_base=base+0)+1;
	Lrealpart();
	vs_top=sup;
	V79= vs_base[0];
	base[0]= (V78);
	vs_top=(vs_base=base+0)+1;
	Limagpart();
	vs_top=sup;
	V80= vs_base[0];
	V81= (*(LnkLI15))((V79));
	base[1]= (V80);
	vs_top=(vs_base=base+1)+1;
	Lcos();
	vs_top=sup;
	V82= vs_base[0];
	base[0]= number_times(/* INLINE-ARGS */V81,V82);
	V83= (*(LnkLI14))((V79));
	base[2]= (V80);
	vs_top=(vs_base=base+2)+1;
	Lsin();
	vs_top=sup;
	V84= vs_base[0];
	base[1]= number_times(/* INLINE-ARGS */V83,V84);
	vs_top=(vs_base=base+0)+2;
	Lcomplex();
	vs_top=sup;
	{object V85 = vs_base[0];
	VMR9(V85)}}
T140:;
	{object V86;
	base[0]= (V78);
	vs_top=(vs_base=base+0)+1;
	Lexp();
	vs_top=sup;
	V86= vs_base[0];
	base[0]= (V86);
	vs_top=(vs_base=base+0)+1;
	Ldivide();
	vs_top=sup;
	V87= vs_base[0];
	V88= number_plus((V86),V87);
	{object V89 = number_times(VV[8],/* INLINE-ARGS */V88);
	VMR9(V89)}}
}
/*	local entry for function TANH	*/

static object LI10(V91)

object V91;
{	 VMB10 VMS10 VMV10
TTL:;
	base[0]= (*(LnkLI14))((V91));
	base[1]= (*(LnkLI15))((V91));
	vs_top=(vs_base=base+0)+2;
	Ldivide();
	vs_top=sup;
	{object V92 = vs_base[0];
	VMR10(V92)}
}
/*	local entry for function ASINH	*/

static object LI11(V94)

object V94;
{	 VMB11 VMS11 VMV11
TTL:;
	V96= number_times((V94),(V94));
	base[1]= number_plus(VV[2],/* INLINE-ARGS */V96);
	vs_top=(vs_base=base+1)+1;
	Lsqrt();
	vs_top=sup;
	V95= vs_base[0];
	base[0]= number_plus((V94),V95);
	vs_top=(vs_base=base+0)+1;
	Llog();
	vs_top=sup;
	{object V97 = vs_base[0];
	VMR11(V97)}
}
/*	local entry for function ACOSH	*/

static object LI12(V99)

register object V99;
{	 VMB12 VMS12 VMV12
TTL:;
	V100= one_plus((V99));
	base[2]= one_minus((V99));
	base[3]= one_plus((V99));
	vs_top=(vs_base=base+2)+2;
	Ldivide();
	vs_top=sup;
	base[1]= vs_base[0];
	vs_top=(vs_base=base+1)+1;
	Lsqrt();
	vs_top=sup;
	V101= vs_base[0];
	V102= number_times(/* INLINE-ARGS */V100,V101);
	base[0]= number_plus((V99),/* INLINE-ARGS */V102);
	vs_top=(vs_base=base+0)+1;
	Llog();
	vs_top=sup;
	{object V103 = vs_base[0];
	VMR12(V103)}
}
/*	local entry for function ATANH	*/

static object LI13(V105)

register object V105;
{	 VMB13 VMS13 VMV13
TTL:;
	if(number_compare((V105),VV[2])==0){
	goto T168;}
	if(!(number_compare((V105),VV[3])==0)){
	goto T167;}
T168:;
	base[0]= VV[9];
	base[1]= (V105);
	vs_top=(vs_base=base+0)+2;
	Lerror();
	vs_top=sup;
T167:;
	base[1]= one_plus((V105));
	V106= number_times((V105),(V105));
	base[3]= number_minus(VV[2],/* INLINE-ARGS */V106);
	vs_top=(vs_base=base+3)+1;
	Lsqrt();
	vs_top=sup;
	base[2]= vs_base[0];
	vs_top=(vs_base=base+1)+2;
	Ldivide();
	vs_top=sup;
	base[0]= vs_base[0];
	vs_top=(vs_base=base+0)+1;
	Llog();
	vs_top=sup;
	{object V107 = vs_base[0];
	VMR13(V107)}
}
/*	local entry for function RATIONAL	*/

static object LI14(V109)

register object V109;
{	 VMB14 VMS14 VMV14
TTL:;
	{register object V110;
	V110= (V109);
	if(!(type_of((V110))==t_shortfloat||type_of((V110))==t_longfloat)){
	goto T179;}
	{object V111;
	object V112;
	object V113;
	base[0]= (V109);
	vs_top=(vs_base=base+0)+1;
	Linteger_decode_float();
	if(vs_base>=vs_top){vs_top=sup;goto T183;}
	V111= vs_base[0];
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T184;}
	V112= vs_base[0];
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T185;}
	V113= vs_base[0];
	vs_top=sup;
	goto T186;
T183:;
	V111= Cnil;
T184:;
	V112= Cnil;
T185:;
	V113= Cnil;
T186:;
	if(!(number_compare((V113),small_fixnum(0))>=0)){
	goto T188;}
	base[0]= (V109);
	vs_top=(vs_base=base+0)+1;
	Lfloat_radix();
	vs_top=sup;
	V114= vs_base[0];
	V115= number_expt(V114,(V112));
	{object V116 = number_times((V111),/* INLINE-ARGS */V115);
	VMR14(V116)}
T188:;
	base[0]= (V109);
	vs_top=(vs_base=base+0)+1;
	Lfloat_radix();
	vs_top=sup;
	V117= vs_base[0];
	V118= number_expt(V117,(V112));
	V119= number_times((V111),/* INLINE-ARGS */V118);
	{object V120 = number_negate(/* INLINE-ARGS */V119);
	VMR14(V120)}}
T179:;
	base[0]= (V110);
	vs_top=(vs_base=base+0)+1;
	(void) (*Lnk16)();
	vs_top=sup;
	if((vs_base[0])==Cnil){
	goto T195;}
	{object V121 = (V109);
	VMR14(V121)}
T195:;
	base[0]= (*(LnkLI17))(VV[10],(V110),VV[11]);
	vs_top=(vs_base=base+0)+1;
	Lerror();
	vs_top=sup;
	{object V122 = vs_base[0];
	VMR14(V122)}}
}
/*	function definition for FFLOOR	*/

static L15()
{register object *base=vs_base;
	register object *sup=base+VM15; VC15
	vs_reserve(VM15);
	{object V123;
	object V124;
	if(vs_top-vs_base<1) too_few_arguments();
	if(vs_top-vs_base>2) too_many_arguments();
	V123=(base[0]);
	vs_base=vs_base+1;
	if(vs_base>=vs_top){vs_top=sup;goto T199;}
	V124=(base[1]);
	vs_top=sup;
	goto T200;
T199:;
	V124= VV[12];
T200:;
	{object V125;
	object V126;
	base[3]= (V123);
	vs_top=(vs_base=base+3)+1;
	Lfloat();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= (V124);
	vs_top=(vs_base=base+4)+1;
	Lfloat();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	Lfloor();
	if(vs_base>=vs_top){vs_top=sup;goto T207;}
	V125= vs_base[0];
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T208;}
	V126= vs_base[0];
	vs_top=sup;
	goto T209;
T207:;
	V125= Cnil;
T208:;
	V126= Cnil;
T209:;
	base[3]= (V125);
	base[4]= (V126);
	vs_top=(vs_base=base+3)+2;
	Lfloat();
	vs_top=sup;
	base[2]= vs_base[0];
	base[3]= (V126);
	vs_top=(vs_base=base+2)+2;
	return;}
	}
}
/*	function definition for FCEILING	*/

static L16()
{register object *base=vs_base;
	register object *sup=base+VM16; VC16
	vs_reserve(VM16);
	{object V127;
	object V128;
	if(vs_top-vs_base<1) too_few_arguments();
	if(vs_top-vs_base>2) too_many_arguments();
	V127=(base[0]);
	vs_base=vs_base+1;
	if(vs_base>=vs_top){vs_top=sup;goto T214;}
	V128=(base[1]);
	vs_top=sup;
	goto T215;
T214:;
	V128= VV[12];
T215:;
	{object V129;
	object V130;
	base[3]= (V127);
	vs_top=(vs_base=base+3)+1;
	Lfloat();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= (V128);
	vs_top=(vs_base=base+4)+1;
	Lfloat();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	Lceiling();
	if(vs_base>=vs_top){vs_top=sup;goto T222;}
	V129= vs_base[0];
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T223;}
	V130= vs_base[0];
	vs_top=sup;
	goto T224;
T222:;
	V129= Cnil;
T223:;
	V130= Cnil;
T224:;
	base[3]= (V129);
	base[4]= (V130);
	vs_top=(vs_base=base+3)+2;
	Lfloat();
	vs_top=sup;
	base[2]= vs_base[0];
	base[3]= (V130);
	vs_top=(vs_base=base+2)+2;
	return;}
	}
}
/*	function definition for FTRUNCATE	*/

static L17()
{register object *base=vs_base;
	register object *sup=base+VM17; VC17
	vs_reserve(VM17);
	{object V131;
	object V132;
	if(vs_top-vs_base<1) too_few_arguments();
	if(vs_top-vs_base>2) too_many_arguments();
	V131=(base[0]);
	vs_base=vs_base+1;
	if(vs_base>=vs_top){vs_top=sup;goto T229;}
	V132=(base[1]);
	vs_top=sup;
	goto T230;
T229:;
	V132= VV[12];
T230:;
	{object V133;
	object V134;
	base[3]= (V131);
	vs_top=(vs_base=base+3)+1;
	Lfloat();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= (V132);
	vs_top=(vs_base=base+4)+1;
	Lfloat();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	Ltruncate();
	if(vs_base>=vs_top){vs_top=sup;goto T237;}
	V133= vs_base[0];
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T238;}
	V134= vs_base[0];
	vs_top=sup;
	goto T239;
T237:;
	V133= Cnil;
T238:;
	V134= Cnil;
T239:;
	base[3]= (V133);
	base[4]= (V134);
	vs_top=(vs_base=base+3)+2;
	Lfloat();
	vs_top=sup;
	base[2]= vs_base[0];
	base[3]= (V134);
	vs_top=(vs_base=base+2)+2;
	return;}
	}
}
/*	function definition for FROUND	*/

static L18()
{register object *base=vs_base;
	register object *sup=base+VM18; VC18
	vs_reserve(VM18);
	{object V135;
	object V136;
	if(vs_top-vs_base<1) too_few_arguments();
	if(vs_top-vs_base>2) too_many_arguments();
	V135=(base[0]);
	vs_base=vs_base+1;
	if(vs_base>=vs_top){vs_top=sup;goto T244;}
	V136=(base[1]);
	vs_top=sup;
	goto T245;
T244:;
	V136= VV[12];
T245:;
	{object V137;
	object V138;
	base[3]= (V135);
	vs_top=(vs_base=base+3)+1;
	Lfloat();
	vs_top=sup;
	base[2]= vs_base[0];
	base[4]= (V136);
	vs_top=(vs_base=base+4)+1;
	Lfloat();
	vs_top=sup;
	base[3]= vs_base[0];
	vs_top=(vs_base=base+2)+2;
	Lround();
	if(vs_base>=vs_top){vs_top=sup;goto T252;}
	V137= vs_base[0];
	vs_base++;
	if(vs_base>=vs_top){vs_top=sup;goto T253;}
	V138= vs_base[0];
	vs_top=sup;
	goto T254;
T252:;
	V137= Cnil;
T253:;
	V138= Cnil;
T254:;
	base[3]= (V137);
	base[4]= (V138);
	vs_top=(vs_base=base+3)+2;
	Lfloat();
	vs_top=sup;
	base[2]= vs_base[0];
	base[3]= (V138);
	vs_top=(vs_base=base+2)+2;
	return;}
	}
}
/*	local entry for function LOGNAND	*/

static object LI19(V141,V142)

object V141;object V142;
{	 VMB19 VMS19 VMV19
TTL:;
	base[0]= small_fixnum(14);
	base[1]= (V141);
	base[2]= (V142);
	vs_top=(vs_base=base+0)+3;
	Lboole();
	vs_top=sup;
	{object V143 = vs_base[0];
	VMR19(V143)}
}
/*	local entry for function LOGNOR	*/

static object LI20(V146,V147)

object V146;object V147;
{	 VMB20 VMS20 VMV20
TTL:;
	base[0]= small_fixnum(8);
	base[1]= (V146);
	base[2]= (V147);
	vs_top=(vs_base=base+0)+3;
	Lboole();
	vs_top=sup;
	{object V148 = vs_base[0];
	VMR20(V148)}
}
/*	local entry for function LOGANDC1	*/

static object LI21(V151,V152)

object V151;object V152;
{	 VMB21 VMS21 VMV21
TTL:;
	base[0]= small_fixnum(4);
	base[1]= (V151);
	base[2]= (V152);
	vs_top=(vs_base=base+0)+3;
	Lboole();
	vs_top=sup;
	{object V153 = vs_base[0];
	VMR21(V153)}
}
/*	local entry for function LOGANDC2	*/

static object LI22(V156,V157)

object V156;object V157;
{	 VMB22 VMS22 VMV22
TTL:;
	base[0]= small_fixnum(2);
	base[1]= (V156);
	base[2]= (V157);
	vs_top=(vs_base=base+0)+3;
	Lboole();
	vs_top=sup;
	{object V158 = vs_base[0];
	VMR22(V158)}
}
/*	local entry for function LOGORC1	*/

static object LI23(V161,V162)

object V161;object V162;
{	 VMB23 VMS23 VMV23
TTL:;
	base[0]= small_fixnum(13);
	base[1]= (V161);
	base[2]= (V162);
	vs_top=(vs_base=base+0)+3;
	Lboole();
	vs_top=sup;
	{object V163 = vs_base[0];
	VMR23(V163)}
}
/*	local entry for function LOGORC2	*/

static object LI24(V166,V167)

object V166;object V167;
{	 VMB24 VMS24 VMV24
TTL:;
	base[0]= small_fixnum(11);
	base[1]= (V166);
	base[2]= (V167);
	vs_top=(vs_base=base+0)+3;
	Lboole();
	vs_top=sup;
	{object V168 = vs_base[0];
	VMR24(V168)}
}
/*	local entry for function LOGNOT	*/

static object LI25(V170)

object V170;
{	 VMB25 VMS25 VMV25
TTL:;
	base[0]= small_fixnum(-1);
	base[1]= (V170);
	vs_top=(vs_base=base+0)+2;
	Llogxor();
	vs_top=sup;
	{object V171 = vs_base[0];
	VMR25(V171)}
}
/*	local entry for function LOGTEST	*/

static object LI26(V174,V175)

object V174;object V175;
{	 VMB26 VMS26 VMV26
TTL:;
	base[0]= (V174);
	base[1]= (V175);
	vs_top=(vs_base=base+0)+2;
	Llogand();
	vs_top=sup;
	V176= vs_base[0];
	{object V177 = (((number_compare(small_fixnum(0),V176)==0?Ct:Cnil))==Cnil?Ct:Cnil);
	VMR26(V177)}
}
/*	local entry for function BYTE	*/

static object LI27(V180,V181)

object V180;object V181;
{	 VMB27 VMS27 VMV27
TTL:;
	{object V182 = make_cons((V180),(V181));
	VMR27(V182)}
}
/*	local entry for function BYTE-SIZE	*/

static object LI28(V184)

object V184;
{	 VMB28 VMS28 VMV28
TTL:;
	{object V185 = car((V184));
	VMR28(V185)}
}
/*	local entry for function BYTE-POSITION	*/

static object LI29(V187)

object V187;
{	 VMB29 VMS29 VMV29
TTL:;
	{object V188 = cdr((V187));
	VMR29(V188)}
}
/*	local entry for function LDB	*/

static object LI30(V191,V192)

object V191;object V192;
{	 VMB30 VMS30 VMV30
TTL:;
	base[0]= (V192);
	V194= (*(LnkLI19))((V191));
	base[1]= number_negate(/* INLINE-ARGS */V194);
	vs_top=(vs_base=base+0)+2;
	Lash();
	vs_top=sup;
	V193= vs_base[0];
	base[0]= small_fixnum(1);
	base[1]= (*(LnkLI20))((V191));
	vs_top=(vs_base=base+0)+2;
	Lash();
	vs_top=sup;
	V195= vs_base[0];
	V196= number_negate(V195);
	{object V197 = (*(LnkLI18))(V193,/* INLINE-ARGS */V196);
	VMR30(V197)}
}
/*	local entry for function LDB-TEST	*/

static object LI31(V200,V201)

object V200;object V201;
{	 VMB31 VMS31 VMV31
TTL:;
	V202= (*(LnkLI21))((V200),(V201));
	{object V203 = (((number_compare(small_fixnum(0),/* INLINE-ARGS */V202)==0?Ct:Cnil))==Cnil?Ct:Cnil);
	VMR31(V203)}
}
/*	local entry for function MASK-FIELD	*/

static object LI32(V206,V207)

object V206;object V207;
{	 VMB32 VMS32 VMV32
TTL:;
	base[0]= (*(LnkLI21))((V206),(V207));
	base[1]= (*(LnkLI19))((V206));
	vs_top=(vs_base=base+0)+2;
	Lash();
	vs_top=sup;
	{object V208 = vs_base[0];
	VMR32(V208)}
}
/*	local entry for function DPB	*/

static object LI33(V212,V213,V214)

object V212;object V213;object V214;
{	 VMB33 VMS33 VMV33
TTL:;
	base[0]= (V214);
	base[1]= (*(LnkLI22))((V213),(V214));
	base[4]= small_fixnum(1);
	base[5]= (*(LnkLI20))((V213));
	vs_top=(vs_base=base+4)+2;
	Lash();
	vs_top=sup;
	V215= vs_base[0];
	V216= number_negate(V215);
	base[3]= (*(LnkLI18))((V212),/* INLINE-ARGS */V216);
	base[4]= (*(LnkLI19))((V213));
	vs_top=(vs_base=base+3)+2;
	Lash();
	vs_top=sup;
	base[2]= vs_base[0];
	vs_top=(vs_base=base+0)+3;
	Llogxor();
	vs_top=sup;
	{object V217 = vs_base[0];
	VMR33(V217)}
}
/*	local entry for function DEPOSIT-FIELD	*/

static object LI34(V221,V222,V223)

object V221;object V222;object V223;
{	 VMB34 VMS34 VMV34
TTL:;
	base[0]= (V221);
	V225= (*(LnkLI19))((V222));
	base[1]= number_negate(/* INLINE-ARGS */V225);
	vs_top=(vs_base=base+0)+2;
	Lash();
	vs_top=sup;
	V224= vs_base[0];
	{object V226 = (*(LnkLI23))(V224,(V222),(V223));
	VMR34(V226)}
}
static object  LnkTLI23(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[23],&LnkLI23,3,ap);} /* DPB */
static object  LnkTLI22(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[22],&LnkLI22,2,ap);} /* MASK-FIELD */
static object  LnkTLI21(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[21],&LnkLI21,2,ap);} /* LDB */
static object  LnkTLI20(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[20],&LnkLI20,1,ap);} /* BYTE-SIZE */
static object  LnkTLI19(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[19],&LnkLI19,1,ap);} /* BYTE-POSITION */
static object  LnkTLI18(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[18],&LnkLI18,2,ap);} /* LOGANDC2 */
static object  LnkTLI17(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[17],&LnkLI17,3,ap);} /* TYPECASE-ERROR-STRING */
static LnkT16(){ call_or_link(VV[16],&Lnk16);} /* RATIONALP */
static object  LnkTLI15(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[15],&LnkLI15,1,ap);} /* COSH */
static object  LnkTLI14(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[14],&LnkLI14,1,ap);} /* SINH */
static object  LnkTLI13(va_alist)va_dcl{va_list ap;va_start(ap);return(object )call_proc(VV[13],&LnkLI13,1,ap);} /* ABS */

#ifdef SYSTEM_SPECIAL_INIT
SYSTEM_SPECIAL_INIT
#endif

