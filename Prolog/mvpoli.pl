%%%% -*- Mode: Prolog -*-

%%%% mvpoli.pl --
%%%% Matricola Cognome Nome
%%%% Matricola Cognome Nome
%%%% Matricola Cognome Nome



%% type:poly
%
%  poly(Monos:list)

%% type:mono
%
%  m(Coefficient:number, TotalDegree:int, VarPowers:list)

%% type:varpower
%
%  v(Power:int, Variable:atom)

%% polynomials are monomials and viceversa
%
%  poly([m(C, TD, VPs)]) is the same as m(C, TD, VPs)



%! coefficients(+Poly:poly, -Coefficients:list)
%
%  coefficients/2 is true when
%  Coefficients is the list of Poly's coefficients;
%  fails when Poly can't be unified with a polynomial written in the
%  (previously defined) polynomial representation (or expression).
coefficients(Poly, Cs) :-
	monomials(Poly, Monos),
	monoscoefficients(Monos, Cs).

monoscoefficients([m(C, _TD, _VPs) | Monos], [C | Cs]) :-
	!,
	monoscoefficients(Monos, Cs).
monoscoefficients([], []).


%% Parsing predicates used to verify if the input can be unified with a
%% polynomial structure or expression

parse_polynomial(Poly, Poly) :-
	is_polynomial(Poly),
	!.
parse_polynomial(Mono, poly([Mono])) :-
	is_monomial(Mono),
	!.
parse_polynomial(Exp, Poly) :-
	as_polynomial(Exp, Poly).


%! variables(+Poly:poly, -Variables:list)
%
%  variables/2 is true when
%  Variables is the list of variables that appear in Poly;
%  Variables is lexicographically ordered (ascending);
%  fails when Poly can't be unified with a polynomial written in the
%  (previously defined) polynomial representation (or expression).
variables(Poly, Vars) :-
	% sort variables lexicographically (@< in sort/4 removes duplicates)
	monomials(Poly, Monos),
	monosvariables(Monos, LVars),
	sort(0, @<, LVars, Vars).

monosvariables([m(C, TD, [v(_P, Var) | VPs]) | Monos], [Var | Vars]) :-
	!,
	monosvariables([m(C, TD, VPs) | Monos], Vars).
monosvariables([m(_C, _TD, []) | Monos], Vars) :-
	!,
	monosvariables(Monos, Vars).
monosvariables([], []).


%! monomials(+Poly:poly, -Monomials:list)
%
%  monomials/2 is true when
%  Monomials is the list of monomials that appear in Poly;
%  fails when Poly can't be unified with a polynomial written in the
%  (previously defined) polynomial representation (or expression).
monomials(Poly, Result) :-
	parse_polynomial(Poly, poly(Monos)),
	monosnull(Monos, Result).

monosnull([], [m(0, 0, [])]) :- !.
monosnull(Monos, Monos).


%! maxdegree(+Poly:poly, -Degree:int)
%
%  maxdegree/2 is true when
%  Degree is the highest degree of variables that appears in Poly;
%  fails when Poly can't be unified with a polynomial written in the
%  (previously defined) polynomial representation (or expression).
maxdegree(Poly, Degree) :-
	monomials(Poly, Monos),
	monosdegrees(Monos, Degrees),
	degreesnull(Degrees, NDegrees),
	max_list(NDegrees, Degree).

monosdegrees([m(C, TD, [v(P, _Var) | VPs]) | Monos], [P | Powers]) :-
	!,
	monosdegrees([m(C, TD, VPs) | Monos], Powers).
monosdegrees([m(_C, _TD, []) | Monos], Powers) :-
	!,
	monosdegrees(Monos, Powers).
monosdegrees([], []).

degreesnull([], [0]) :- !.
degreesnull(Degrees, Degrees).


%! mindegree(+Poly:poly, -Degree:int)
%
%  mindegree/2 is true when
%  Degree is the lowest degree of variables (that appear in Poly;
%  fails when Poly can't be unified with a polynomial written in the
%  (previously defined) polynomial representation (or expression).
mindegree(Poly, Degree) :-
	monomials(Poly, Monos),
	monosdegrees(Monos, Degrees),
	degreesnull(Degrees, NDegrees),
	min_list(NDegrees, Degree).


%! polyplus(+Poly1:poly, +Poly2:poly, -Result:poly)
%
%  polyplus/3 is true when
%  Result is the polynomial calculated by the polynomial sum
%  between Poly1 and Poly2;
%  fails when Poly1 or Poly2 can't be unified with polynomials written in the
%  (previously defined) polynomial representation (or expression).
polyplus(Poly1, Poly2, poly(Result)) :-
	% append Poly1 and Poly2
	% normalize the list of monomials (sum liketerms and reorder)
	monomials(Poly1, Monos1),
	monomials(Poly2, Monos2),
	append(Monos1, Monos2, Monos),
	liketerms(Monos, Result).


%% Compare predicates used by predsort/3 to sort the list of monomials in
%% a polynomial; sorting order is:
%% order by TD (ascending) first,
%% order by VPs (ascending) second.
%% Ordering "by VPs" means checking each VP:
%% order by Variable (lexicographically ascending) first,
%% order by Power (ascending) second.

monocompare(<, m(_C1, TD, VPs1), m(_C2, TD, VPs2)) :-
	% same TD, compare VPs
	vpscompare(VPs1, VPs2),
	!.
monocompare(>, m(_C1, TD, _VPs1), m(_C2, TD, _VPs2)) :-
	% no need to compare VPs: vpscompare/2 failed
	!.
monocompare(<, m(_C1, TD1, _VPs1), m(_C2, TD2, _VPs2)) :-
	% different TD, sort @< (ascending)
	TD1 < TD2,
	!.
monocompare(>, m(_C1, _TD1, _VPs1), m(_C2, _TD2, _VPs2)).
	% no need to compare TD: TD1 < TD2 failed, and TDs are not equal

vpscompare([VP | VPs1], [VP | VPs2]) :-
	% same VP, keep recursing
	!,
	vpscompare(VPs1, VPs2).
vpscompare([VP1 | _VPs1], [VP2 | _VPs2]) :-
	% stop and compare the first VPs that are not equal
	vpcompare(VP1, VP2).

vpcompare(v(P1, Var), v(P2, Var)) :-
	!,
	P1 < P2.
vpcompare(v(_P1, Var1), v(_P2, Var2)) :-
	Var1 @< Var2.


%! liketerms(+Monos:list, -Result:list)
%
%  liketerms/2 is true when
%  Result is the list of sorted monomials in Monos;
%  fails when Monos can't be unified with a list of monomials.
liketerms(Monos, Result) :-
	% sort Monos with predsort/3 and monocompare/3
	% normalize "like terms"
	predsort(monocompare, Monos, SortedMonos),
	monosliketerms(SortedMonos, Result).


%% "Like terms" predicate called by liketerms/2 to normalize the list of
%% monomials Monos:
%% like-terms are added between eachother;
%% null monomials (0, or m(0, 0, [])) are removed.

monosliketerms([m(C1, TD, VPs), m(C2, TD, VPs) | Monos], LTMonos) :-
	% sum monomials with the same TD and VPs
	C is C1 + C2,
	!,
	monosliketerms([m(C, TD, VPs) | Monos], LTMonos).
monosliketerms([m(0, _TD, _VPs) | Monos], LTMonos) :-
	% remove null monomials
	!,
	monosliketerms(Monos, LTMonos).
monosliketerms([Mono | Monos], [Mono | LTMonos]) :-
	!,
	monosliketerms(Monos, LTMonos).
monosliketerms([], []).


%% "Like terms" predicate called by monotimes/3 and as_monomial/2 to normalize
%% the list of varpowers VPs:
%% same Variable VPs have their Powers added;
%% VPs with Power equal to 0 are removed.

varpowersliketerms([v(P1, Var), v(P2, Var) | VPs], LTVPs) :-
	P is P1 + P2,
	!,
	varpowersliketerms([v(P, Var) | VPs], LTVPs).
varpowersliketerms([v(0, _Var) | VPs], LTVPs) :-
	!,
	varpowersliketerms(VPs, LTVPs).
varpowersliketerms([VP | VPs], [VP | LTVPs]) :-
	!,
	varpowersliketerms(VPs, LTVPs).
	varpowersliketerms([], []).


%! polyminus(+Poly1:poly, +Poly2:poly, -Result:poly)
%
%  polyminus/3 is true when
%  Result is the polynomial calculated by the polynomial subtraction
%  between Poly1 and Poly2;
%  fails when Poly1 or Poly2 can't be unified with polynomials written in the
%  (previously defined) polynomial representation (or expression).
polyminus(Poly1, Poly2, Result) :-
	% invert all Poly2's monomials coefficients
	polyneg(Poly2, NegPoly2),
	polyplus(Poly1, NegPoly2, Result).

polyneg(Poly, poly(NegMonos)) :-
	monomials(Poly, Monos),
	monosneg(Monos, NegMonos).

monosneg([m(C, TD, VPs) | Monos], [m(NegC, TD, VPs) | NegMonos]) :-
	!,
	NegC is -C,
	monosneg(Monos, NegMonos).
monosneg([], []).


%! polytimes(+Poly1:poly, +Poly2:poly, -Result:poly)
%
%  polytimes/3 is true when
%  Result is the polynomial calculated by the polynomial multiplication
%  between Poly1 and Poly2;
%  fails when Poly1 or Poly2 can't be unified with polynomials written in the
%  (previously defined) polynomial representation (or expression).
polytimes(Poly1, Poly2, poly(Result)) :-
	monomials(Poly1, Monos1),
	monomials(Poly2, Monos2),
	monostimes(Monos1, Monos2, Monos),
	liketerms(Monos, Result).

monostimes([Mono | Monos1], Monos2, Result) :-
	% "multiply" each Mono in Monos1 with all Monos in Monos2
	monotimes(Mono, Monos2, Result1),
	!,
	monostimes(Monos1, Monos2, Result2),
	append(Result1, Result2, Result).
monostimes([], _Monos2, []).

monotimes(m(C1, TD1, VPs1), [m(C2, TD2, VPs2) | Ms], [m(CR, TDR, VPsR) | R]) :-
	% calculate new coefficient and totaldegree
	% append and normalize the two VPs
	CR is C1 * C2,
	TDR is TD1 + TD2,
	append(VPs1, VPs2, VPs),
	sort(2, @=<, VPs, SortedVPs),
	varpowersliketerms(SortedVPs, VPsR),
	!,
	monotimes(m(C1, TD1, VPs1), Ms, R).
monotimes(_Mono, [], []).


%! as_monomial(+Expression:exp, -Monomial:mono)
%
%  as_monomial/2 is true when
%  Monomial is the term representing the monomial obtained after parsing
%  the expression Expression;
%  Monomial's variables are lexicographically ordered (ascending);
%  fails when Expression can't be unified with an algebrically correct
%  expression representing a monomial.
as_monomial(Exp, m(C, NTD, Result)) :-
	% parse expression, calculate totaldegree, normalize VPs
	as_monomial_help(Exp, C, VPs),
	monodegree(VPs, TD),
	mononull(m(C, TD, VPs), m(C, NTD, NVPs)),
	sort(2, @=<, NVPs, SortedVPs),
	varpowersliketerms(SortedVPs, Result).

as_monomial_help(Exp * Var, C, [VP | VPs]) :-
	!,
	as_varpower(Var, VP),
	as_monomial_help(Exp, C, VPs).
as_monomial_help(C, C, []) :-
	number(C),
	!.
as_monomial_help(-Var, -1, [VP]) :-
	!,
	as_varpower(Var, VP).
as_monomial_help(Var, 1, [VP]) :-
	as_varpower(Var, VP).

as_varpower(Var ^ P, v(P, Var)) :-
	!,
	atom(Var),
	integer(P),
	P >= 0.
as_varpower(Var, v(1, Var)) :-
	atom(Var).

monodegree([v(P, _Var) | VPs], Result) :-
	% sum Power to the last calculated Degree
	!,
	monodegree(VPs, Degree),
	Result is Degree + P.
monodegree([], 0).

mononull(m(0, _TD, _VPs), m(0, 0, [])) :- !.
mononull(Mono, Mono).


%! as_polynomial(+Expression:exp, -Poly:poly)
%
%  as_polynomial/2 is true when
%  Polynomial is the term representing the polynomial obtained after parsing
%  the expression Expression;
%  Polynomial's monomials are ordered by total degree (descending);
%  fails when Expression can't be unified with an algebrically correct
%  expression representing a polynomial.
as_polynomial(Exp, poly(Result)) :-
	% parse all monomials, normalize the resulting list Monos
	as_polynomial_help(Exp, Monos),
	liketerms(Monos, Result).

as_polynomial_help(Exp + Mono, [M | Ms]) :-
	!,
	as_polynomial_help(Exp, Ms),
	as_monomial(Mono, M).
as_polynomial_help(Exp - Mono, [m(NegC, TD, VPs) | Ms]) :-
	!,
	as_polynomial_help(Exp, Ms),
	as_monomial(Mono, m(C, TD, VPs)),
	NegC is -C.
as_polynomial_help(Mono, [M]) :-
	as_monomial(Mono, M).


%! polyval(+Poly:poly, +VariableValues:list, -Value:number)
%
%  polyval/3 is true when
%  Value is the value of Poly in the n-dimensional point
%  represented by VariableValues;
%  VariableValues is a list of numbers directly mapped to variables/2;
%  fails when Poly can't be unified with a polynomial written in the
%  (previously defined) polynomial representation (or expression),
%  when VariableValues can't be mapped to variables/2 (different length) or
%  when VariableValues isn't a list of numbers.
polyval(Poly, VarVals, Value) :-
	% variables/2 gets called with poly(Monos) because monomials/2 has been called
	% already by polyval/3
	% (monomials/2 in variables/2 will just check is_polynomial/2)
	monomials(Poly, Monos),
	variables(poly(Monos), Vars),
	monoscalc(Monos, Vars, VarVals, Value).

monoscalc([Mono | Monos], Vars, VarVals, Result) :-
	!,
	monocalc(Mono, Vars, VarVals, MonoValue),
	monoscalc(Monos, Vars, VarVals, MonosValue),
	Result is MonoValue + MonosValue.
monoscalc([], _Vars, _VarVals, 0).

monocalc(m(C, TD, [v(P, Var) | VPs]), Vars, VarVals, Result) :-
	% search for Var in Vars (get Index with nth0/3)
	% using Index, get the value in VarVals
	nth0(Index, Vars, Var),
	nth0(Index, VarVals, Val),
	!,
	monocalc(m(C, TD, VPs), Vars, VarVals, Value),
	Result is Val ^ P * Value.
monocalc(m(C, _TD, []), _Vars, _VarVals, C).


%! pprint_polynomial(+Poly:poly)
%
%  pprint_polynomial/1 is true when
%  it prints on std_out a traditional (algebrical) representation of Poly;
%  fails when Poly can't be unified with a polynomial written in the
%  (previously defined) polynomial representation (or expression).
pprint_polynomial(Poly) :-
	% with_output_to/2 redirects pprint_monomials/1's format calls to a string,
	% making sure to not print anything if there's an error
	monomials(Poly, Monos),
	with_output_to(string(Out), pprint_monomials(Monos)),
	write(Out).

pprint_monomials([Mono]) :-
	!,
	pprint_monomial(Mono),
	format('~n').
pprint_monomials([Mono | Monos]) :-
	pprint_monomial(Mono),
	format(' + '),
	pprint_monomials(Monos).
	
pprint_monomial(m(C, 0, [])) :-
	% known term (no variables), no recursion
	!,
	format('~d', [C]).
pprint_monomial(m(1, _TD, VPs)) :-
	!,
	pprint_varpowers(VPs).
pprint_monomial(m(C, _TD, VPs)) :-
	format('~d * ', [C]),
	pprint_varpowers(VPs).

pprint_varpowers([v(1, Var)]) :-
	!,
	upcase_atom(Var, UPVar),
	format('~a', [UPVar]).
pprint_varpowers([v(P, Var)]) :-
	!,
	upcase_atom(Var, UPVar),
	format('~a^~d', [UPVar, P]).
pprint_varpowers([v(1, Var) | VPs]) :-
	!,
	upcase_atom(Var, UPVar),
	format('~a * ', [UPVar]),
	pprint_varpowers(VPs).
pprint_varpowers([v(P, Var) | VPs]) :-
	upcase_atom(Var, UPVar),
	format('~a^~d * ', [UPVar, P]),
	pprint_varpowers(VPs).


%% Given predicates

is_monomial(m(_C, TD, VPs)) :-
	integer(TD),
	TD >= 0,
	is_list(VPs).

is_varpower(v(Power, VarSymbol)) :-
	integer(Power),
	Power >= 0,
	atom(VarSymbol).

is_polynomial(poly(Monomials)) :-
	is_list(Monomials),
	foreach(member(M, Monomials), is_monomial(M)).

%%%% end of file -- mvpoli.pl --
