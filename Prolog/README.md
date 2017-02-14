# Multivariate Polynomials > Prolog

## Representation

Expression:

```
Variable   X          → x

Monomial   XY^3       → x * y^3

Monomial   5XW        → 5 * x * w

Polynomial XY^3 + 5XW → x * y^3 + 5 * x * w
```

VarPower: __v__(_Power_, _Variable_)  

```prolog
% X^5
v(5, x)
```

Monomial: __m__(_Coefficient_, _TotalDegree_, _VarPowers_)  

```prolog
% 2*X^4*Y
m(2, 5, [v(4, x), v(1, y)])
```

Polynomial: __poly__(_Monomials_)  

```prolog
% 4*X*Y + 2*Y*Z
poly(m(4, 2, [v(1, x), v(1, y)]), m(2, 2, [v(1, y), v(1, z)]))
```
&nbsp;

## Interface

__coefficients__(_+Poly_, _-Coefficients_)  

__variables__(_+Poly_, _-Variables_)  

__monomials__(_+Poly_, _-Monomials_)  

__maxdegree__(_+Poly_, _-Degree_)  

__mindegree__(_+Poly_, _-Degree_)  

__polyplus__(_+Poly1_, _+Poly2_, _-Result_)  

__polyminus__(_+Poly1_, _+Poly2_, _-Result_)  

__polytimes__(_+Poly1_, _+Poly2_, _-Result_)  

__as_monomial__(_+Expression_, _-Monomial_)  

__as_polynomial__(_+Expression_, _-Monomial_)  

__polyval__(_+Polynomial_, _+VariableValues_, _-Value_)  

__pprint_polynomial__(_+Polynomial_)  
&nbsp;

## Examples

```prolog
?- as_monomial(3 * y * w * u^3, M).
M = m(3, 5, [v(3, u), v(1, w), v(1, y)]).

?- as_monomial(42, QD).
QD = m(42, 0, []).

?- as_polynomial(-1 * x + x * y, P1), variables(P1, Vs).
P1 = poly([m(-1, 1, [v(1, x)]), m(1, 2, [v(1, x), v(1, y)])]),
Vs = [x, y].

?- as_monomial(y * s^3 * u^3, M1),
|  as_polynomial(-1 * x + x * y, P1),
|  polytimes(M1, P1, R),
|  pprint_polynomial(R).
-1 * S^3 * U^3 * X * Y + S^3 * U^3 * X * Y^2
M1 = m(1, 7, [v(3, s), v(3, u), v(1, y)]),
P1 = poly([m(-1, 1, [v(1, x)]), m(1, 2, [v(1, x), v(1, y)])]),
R = poly([m(-1, 8, [v(3, s), v(3, u), v(1, x), v(1, y)]),
          m(1, 9, [v(3, s), v(3, u), v(1, x), v(2, y)])]).
```
