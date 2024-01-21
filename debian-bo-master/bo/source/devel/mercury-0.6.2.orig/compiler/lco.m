%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: zs

% This module looks for opportunities to apply the "last call modulo
% constructor application" optimization.

%-----------------------------------------------------------------------------%

:- module lco.

:- interface.

:- import_module hlds_module, hlds_pred.
:- import_module io.

:- pred lco_modulo_constructors(pred_id, proc_id, module_info,
	proc_info, proc_info, io__state, io__state).
:- mode lco_modulo_constructors(in, in, in, in, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_goal, passes_aux, hlds_out.
:- import_module list, std_util.

%-----------------------------------------------------------------------------%

lco_modulo_constructors(PredId, ProcId, ModuleInfo, ProcInfo0, ProcInfo) -->
	{ proc_info_goal(ProcInfo0, Goal0) },
	{ lco_in_goal(Goal0, ModuleInfo, Goal) },
	( { Goal = Goal0 } ->
		{ ProcInfo = ProcInfo0 }
	;
		{ ProcInfo = ProcInfo0 },			% for now
		% { proc_info_set_goal(ProcInfo0, Goal, ProcInfo) },
		io__write_string("% Can introduce LCO in "),
		hlds_out__write_pred_proc_id(ModuleInfo, PredId, ProcId),
		io__write_string("\n")
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred lco_in_goal(hlds__goal, module_info, hlds__goal).
:- mode lco_in_goal(in, in, out) is det.

lco_in_goal(Goal0 - GoalInfo, ModuleInfo, Goal - GoalInfo) :-
	lco_in_goal_2(Goal0, ModuleInfo, Goal).

%-----------------------------------------------------------------------------%

:- pred lco_in_goal_2(hlds__goal_expr, module_info, hlds__goal_expr).
:- mode lco_in_goal_2(in, in, out) is det.

lco_in_goal_2(conj(Goals0), ModuleInfo, conj(Goals)) :-
	list__reverse(Goals0, RevGoals0),
	lco_in_conj(RevGoals0, [], ModuleInfo, Goals).

lco_in_goal_2(disj(Goals0, SM), ModuleInfo, disj(Goals, SM)) :-
	lco_in_disj(Goals0, ModuleInfo, Goals).

lco_in_goal_2(switch(Var, Det, Cases0, SM), ModuleInfo,
		switch(Var, Det, Cases, SM)) :-
	lco_in_cases(Cases0, ModuleInfo, Cases).

lco_in_goal_2(if_then_else(Vars, Cond, Then0, Else0, SM), ModuleInfo,
		if_then_else(Vars, Cond, Then, Else, SM)) :-
	lco_in_goal(Then0, ModuleInfo, Then),
	lco_in_goal(Else0, ModuleInfo, Else).

lco_in_goal_2(some(Vars, Goal0), ModuleInfo, some(Vars, Goal)) :-
	lco_in_goal(Goal0, ModuleInfo, Goal).

lco_in_goal_2(not(Goal), _ModuleInfo, not(Goal)).

lco_in_goal_2(higher_order_call(A,B,C,D,E), _ModuleInfo,
		higher_order_call(A,B,C,D,E)).

lco_in_goal_2(call(A,B,C,D,E,F), _ModuleInfo, call(A,B,C,D,E,F)).

lco_in_goal_2(unify(A,B,C,D,E), _ModuleInfo, unify(A,B,C,D,E)).

lco_in_goal_2(pragma_c_code(A,B,C,D,E,F), _, pragma_c_code(A,B,C,D,E,F)).

%-----------------------------------------------------------------------------%

:- pred lco_in_disj(list(hlds__goal), module_info, list(hlds__goal)).
:- mode lco_in_disj(in, in, out) is det.

lco_in_disj([], __ModuleInfo, []).
lco_in_disj([Goal0 | Goals0], ModuleInfo, [Goal | Goals]) :-
	lco_in_goal(Goal0, ModuleInfo, Goal),
	lco_in_disj(Goals0, ModuleInfo, Goals).

%-----------------------------------------------------------------------------%

:- pred lco_in_cases(list(case), module_info, list(case)).
:- mode lco_in_cases(in, in, out) is det.

lco_in_cases([], __ModuleInfo, []).
lco_in_cases([case(Cons, Goal0) | Cases0], ModuleInfo,
		[case(Cons, Goal) | Cases]) :-
	lco_in_goal(Goal0, ModuleInfo, Goal),
	lco_in_cases(Cases0, ModuleInfo, Cases).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% lco_in_conj(RevGoals, Unifies, ModuleInfo, Goals)
%
% Given a conjunction whose structure is: "goals*,call,construct*",
% move the construction unifications before the call.
%
% For now the transformation results are usable by humans only.
% XXX Later we will have to modify the instantiation states
% recorded for the variables involved in the constructions.
% The ModuleInfo will be probably be needed by this code.
%
% We traverse the conjunction backwards (the caller has reversed the list).
% RevGoals is the list of remaining goals in the reversed conjunction list.
% RevUnifies is the list of assignments and constructions delayed by any
% previous recursive invocations of lco_in_conj. 
%
% invariant: append(reverse(RevGoals), Unifies) = original conjunction

:- pred lco_in_conj(list(hlds__goal), list(hlds__goal), module_info,
	list(hlds__goal)).
:- mode lco_in_conj(in, in, in, out) is det.

lco_in_conj([], Unifies, __ModuleInfo, Unifies).
lco_in_conj([Goal0 | Goals0], Unifies0, ModuleInfo, Goals) :-
	Goal0 = GoalExpr0 - _,
	(
		GoalExpr0 = unify(_, _, _, Unif, _),
		Unif = construct(_, _, _, _)
	->
		Unifies1 = [Goal0 | Unifies0],
		lco_in_conj(Goals0, Unifies1, ModuleInfo, Goals)
	;
		GoalExpr0 = call(_, _, _, _, _, _)
	->
		list__append(Unifies0, [Goal0], LaterGoals),
		list__reverse(Goals0, FrontGoals),
		list__append(FrontGoals, LaterGoals, Goals)
	;
		% The conjunction does not follow the pattern "unify*, goal"
		% so we cannot optimize it; reconstruct the original goal list
		list__reverse([Goal0 | Goals0], FrontGoals),
		list__append(FrontGoals, Unifies0, Goals)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
