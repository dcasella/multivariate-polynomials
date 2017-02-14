# Multivariate Polynomials

> Translated and adapted from MV-201701 PDF written by Marco Antoniotti and Gabriella Pasi.

One of the first and most important computer applications was the _symbolic_ manipulation of mathematical operations. In particular, known systems like _Computer Algebra Systems_ preoccupy to offer _multivariate polynomials_ manipulation functionalities.  
The Project consists in Common Lisp and Prolog libraries implementing multivariate polynomials manipulation.  
&nbsp;

## List of contents

- [Representation](#representation)  
- [Interface](#interface)  
- [Examples](#examples)  
&nbsp;

## Representation

### Common Lisp

Expression:

```
Variable   X          → 'x

Monomial   XY^3       → '(* x (expt y 3))

Monomial   5XW        → '(* 5 x w)

Polynomial XY^3 + 5XW → '(+ (* x (expt y 3)) (* 5 x w))
```

VarPower: (__V__ _Power_ _Variable_)  

```lisp
; X^5
(V 5 X)
```

Monomial: (__M__ _Coefficient_ _TotalDegree_ _VarPowers_)  

```lisp
; 2*X^4*Y
(M 2 5 ((V 4 X) (V 1 Y)))
```

Polynomial: (__P__ _Monomials_)  

```lisp
; 4*X*Y + 2*Y*Z
(P ((M 4 2 ((V 1 X) (V 1 Y))) (M 2 2 ((V 1 Y) (V 1 Z)))))
```
&nbsp;

### Prolog

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

### Common Lisp
(__coefficients__ _Poly_) → _Coefficients_  

(__variables__ _Poly_) → _Variables_  

(__monomials__ _Poly_) → _Monomials_  

(__maxdegree__ _Poly_) → _Degree_  

(__mindegree__ _Poly_) → _Degree_  

(__polyplus__ _Poly1_ _Poly2_) → _Result_  

(__polyminus__ _Poly1_ _Poly2_) → _Result_  

(__polytimes__ _Poly1_ _Poly2_) → _Result_  

(__as_monomial__ _Expression_) → _Monomial_  

(__as_polynomial__ _Expression_) → _Monomial_  

(__polyval__ _Polynomial_ _VariableValues_) → _Value_  

(__pprint_polynomial__ _Polynomial_) → _NIL_  
&nbsp;

### Prolog

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

### Common Lisp

```lisp
CL-USER> (as-monomial '(* 3 y w (expt u 3)))
(M 3 5 ((V 3 U) (V 1 W) (V 1 Y)))

CL-USER> (setf qd (as-monomial 42))
(M 42 0 NIL)

CL-USER> (setf m1 (as-monomial '(* y (expt s 3) (expt u 3))))
(M 1 7 ((V 3 S) (V 3 U) (V 1 Y)))

CL-USER> (setf p1 (as-polynomial '(+ (* -1 x) (* x y))))
(P ((M -1 1 ((V 1 X))) (M 1 2 ((V 1 X) (V 1 Y)))))

CL-USER> (setf p2 (as-polynomial '(+ (* y (expt s 3) (expt u 3)) -4 (* x y))))
(P ((M -4 0 NIL)
    (M 1 2 ((V 1 X) (V 1 Y)))
    (M 1 7 ((V 3 S) (V 3 U) (V 1 Y)))))

CL-USER> (polytimes m1 p1)
(P ((M -1 8 ((V 3 S) (V 3 U) (V 1 X) (V 1 Y)))
    (M 1 9 ((V 3 S) (V 3 U) (V 1 X) (V 2 Y)))))

CL-USER> (pprint-polynomial *)
-1 * S^3 * U^3 * X * Y + S^3 * U^3 * X * Y^2
NIL
```
&nbsp;

### Prolog

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
