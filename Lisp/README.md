# Multivariate Polynomials > Common Lisp

## Representation

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

## Interface

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

## Examples

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
