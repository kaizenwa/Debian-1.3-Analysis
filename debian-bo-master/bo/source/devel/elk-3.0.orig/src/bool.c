#include "kernel.h"

Object P_Booleanp (x) Object x; {
    return TYPE(x) == T_Boolean ? True : False;
}

Object P_Not (x) Object x; {
    return Truep (x) ? False : True;
}

Object P_Eq (x1, x2) Object x1, x2; {
    return EQ(x1, x2) ? True : False;
}

Object P_Eqv (x1, x2) Object x1, x2; {
    return Eqv (x1, x2) ? True : False;
}

Object P_Equal (x1, x2) Object x1, x2; {
    return Equal (x1, x2) ? True : False;
}

Eqv (x1, x2) Object x1, x2; {
    register t1, t2;
    if (EQ(x1, x2))
	return 1;
    t1 = TYPE(x1);
    t2 = TYPE(x2);
    if (Numeric (t1) && Numeric (t2))
	return Generic_Equal (x1, x2);
    if (t1 != t2)
	return 0;
    switch (t1) {
    case T_String:
        return STRING(x1)->size == 0 && STRING(x2)->size == 0;
    case T_Vector:
	return VECTOR(x1)->size == 0 && VECTOR(x2)->size == 0;
    case T_Primitive:
	return strcmp (PRIM(x1)->name, PRIM(x2)->name) == 0;
    default:
	if (t1 < 0 || t1 >= Num_Types)
	    Panic ("bad type in eqv");
	if (Types[t1].eqv == NOFUNC)
	    return 0;
	return (Types[t1].eqv)(x1, x2);
    }
    /*NOTREACHED*/
}

Equal (x1, x2) Object x1, x2; {
    register t1, t2, i;

again:
    if (EQ(x1, x2))
	return 1;
    t1 = TYPE(x1);
    t2 = TYPE(x2);
    if (Numeric (t1) && Numeric (t2))
	return Generic_Equal (x1, x2);
    if (t1 != t2)
	return 0;
    switch (t1) {
    case T_Boolean:
    case T_Character:
    case T_Compound:
    case T_Control_Point:
    case T_Promise:
    case T_Port:
    case T_Macro:
	return 0;
    case T_Primitive:
    case T_Environment:
	return Eqv (x1, x2);
    case T_Symbol: {
	struct S_Symbol *p1 = SYMBOL(x1), *p2 = SYMBOL(x2);
	return Equal (p1->name, p2->name) && Equal (p1->plist, p2->plist);
    }
    case T_Pair:
	if (!Equal (Car (x1), Car (x2)))
	    return 0;
	x1 = Cdr (x1); x2 = Cdr (x2);
	goto again;
    case T_String: {
	struct S_String *p1 = STRING(x1), *p2 = STRING(x2);
	return p1->size == p2->size &&
	    bcmp (p1->data, p2->data, p1->size) == 0;
    }
    case T_Vector: {
	struct S_Vector *p1 = VECTOR(x1), *p2 = VECTOR(x2);
	if (p1->size != p2->size)
	    return 0;
	for (i = 0; i < p1->size; i++)
	    if (!Equal (p1->data[i], p2->data[i]))
		return 0;
	return 1;
    }
    default:
	if (t1 < 0 || t1 >= Num_Types)
	    Panic ("bad type in equal");
	if (Types[t1].equal == NOFUNC)
	    return 0;
	return (Types[t1].equal)(x1, x2);
    }
    /*NOTREACHED*/
}

Object P_Empty_List_Is_False (is_false) Object is_false; {
    Check_Type (is_false, T_Boolean);
    if (Truep (is_false))
	False2 = Null;
    else
	False2 = False;
    return Void;
}
