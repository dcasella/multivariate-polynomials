;;;; -*- Mode: Lisp -*-

;;;; mvpoli.lisp --
;;;; Matricola Cognome Nome
;;;; Matricola Cognome Nome
;;;; Matricola Cognome Nome



;; type: Poly
;; (POLY MONOS:list)

;; type: Mono
;; (M COEFFICIENT:number TOTALDEGREE:int VARPOWERS:list)

;; type: VarPower
;; (V POWER:int VARIABLE:symbol)

;; polynomials are monomials and viceversa
;; (POLY (M C TD VPS)) is the same as (M C TD VPS)



(defun varpower-power (vp)
  (if (and (listp vp)
           (eq 'v (first vp)))
      (second vp)
      (error "Invalid input (VarPower expected).")))

(defun varpower-symbol (vp)
  (if (and (listp vp)
           (eq 'v (first vp)))
      (third vp)
      (error "Invalid input (VarPower expected).")))

(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp)))
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))

(defun monomial-degree (m)
  "Return monomial's total degree.
   Input M, structure of type Mono."
  (if (and (listp m)
           (eq 'm (first m)))
      (third m)
      (error "Invalid input (Mono expected).")))

(defun varpowers (m)
  "Return monomial's varpowers list.
   Input M, structure of type Mono."
  (if (and (listp m)
           (eq 'm (first m)))
      (fourth m)
      (error "Invalid input (Mono expected).")))

(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-degree m))
             (vps (varpowers m)))
         (and (integerp mtd)
              (>= mtd 0)
              (listp vps)
              (every #'is-varpower vps)))))

;; Parsing functions
;; parse-monomial is used to parse a monomial or a s-exp (representing a
;; monomial) to a monomial
;; parse-monomial Mono  → Mono
;; parse-monomial S-Exp → Mono
(defun parse-monomial (m)
  (if (is-monomial m)
      m
      (as-monomial m)))

;; parse-polynomial is used to parse a polynomial, a monomial or a s-exp
;; (representing a polynomial or a monomial) to a polynomial
;; parse-polynomial Poly  → Poly
;; parse-polynomial Mono  → Poly
;; parse-polynomial S-Exp → Poly
(defun parse-polynomial (p)
  (cond ((and (listp p)
              (eq 'poly (first p)))
         p)
        ((is-monomial p)
         (list 'poly (cons p nil)))
        (t
         (as-polynomial p))))

(defun null-ms (ms)
  (if ms
      ;; ms is not nil
      ms
      ;; else (ms is nil)
      (cons (list 'm 0 0 nil) nil)))

(defun monomials (p)
  "Return the list of monomials that appear in P.
   Input P, structure of type Poly or S-Exp."
  (null-ms (first (rest (parse-polynomial p)))))

(defun is-polynomial (p)
  (and (listp p)
       (let ((ms (monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))

(defun variables-vps (vps)
  (if vps
      (cons (varpower-symbol (first vps))
            (variables-vps (rest vps)))))

;; Every "main" functon checks if the input is correct by calling let with
;; parse-monomial or parse-polynomial as the first variable

(defun vars-of (m)
  "Return the list of variables (symbols) that appear in M.
   Input M, structure of type Mono or S-Exp."
  (variables-vps (varpowers (parse-monomial m))))

(defun monomial-coefficient (m)
  "Return monomial's coefficient.
   Input M, structure of type Mono or S-Exp."
  (second (parse-monomial m)))

(defun coefficients-ms (ms)
  (if ms
      (cons (monomial-coefficient (first ms))
            (coefficients-ms (rest ms)))))

(defun coefficients (p)
  "Return the list of coefficients that appear in P.
   Input P, structure of type Poly or S-Exp."
  (coefficients-ms (monomials p)))

(defun variables-ms (ms)
  (if ms
      (append (vars-of (first ms))
              (variables-ms (rest ms)))))

(defun variables (p)
  "Return the list of variables (symbols) that appear in P.
   Input P, structure of type Poly or S-Exp."
  ;; copy-seq is needed to not degrade the input P when sorting
  ;; sort by symbol (string<)
  (sort (copy-seq (remove-duplicates (variables-ms (monomials p)))) #'string<))

(defun degree-vps (vps)
  (if vps
      (cons (varpower-power (first vps)) (degree-vps (rest vps)))
      nil))

(defun degree-ms (ms)
  (if ms
      (append (degree-vps (varpowers (first ms))) (degree-ms (rest ms)))
      nil))

(defun null-degree (degrees)
  (if degrees
      degrees
      (cons 0 nil)))

(defun maxdegree (p)
  "Return the highest variable degree in P.
   Input P, structure of type Poly or S-Exp."
  ;; input 0 as starting degree
  (apply #'max (null-degree (degree-ms (monomials p)))))

(defun mindegree (p)
  "Return the lowest variable degree in P.
   Input P, structure of type Poly or S-Exp."
  ;; input -1 as starting degree
  (apply #'min (null-degree (degree-ms (monomials p)))))

;; Sorting functions
;; vps< returns true when VPS1 is "less than" VPS2;
;; it checks each variable:
;; if it's the same, check the power (*)
;; else return true when V1 < V2 (symbol-wise)
;;
;; (*) if it's the same, check the other variables
;; else return true when P1 < P2
(defun vps< (vps1 vps2)
  (if (equal vps1 vps2)
      ;; order is unimportant
      t
      ;; else
      (let ((vp1 (first vps1))
            (vp2 (first vps2)))
        (cond ((equal vp1 vp2)
               (vps< (rest vps1) (rest vps2)))
              ((eq (varpower-symbol vp1) (varpower-symbol vp2))
               (< (varpower-power vp1) (varpower-power vp2)))
              (t
               (string< (varpower-symbol vp1) (varpower-symbol vp2)))))))

;; monomial< returs true when M1 is "less-than" M2;
;; it checks each TD:
;; if it's the same, check the variables (VPS) with vps<
;; else returns true when TD1 < TD2
(defun monomial< (m1 m2)
  (let ((td1 (monomial-degree m1))
        (td2 (monomial-degree m2)))
    (if (= td1 td2)
        (vps< (varpowers m1) (varpowers m2))
        (< td1 td2))))

;; Normalization functions
;; liketerms-vps removes all duplicate variables (by adding their powers)
;; and "null" variables (power 0)
(defun liketerms-vps (vps)
  (cond ((null vps)
         nil)
        ((not (null (second vps)))
         ;; vps is made up of at least two elements
         (let* ((vp1 (first vps))
                (vp2 (second vps))
                (p1 (varpower-power vp1))
                (p2 (varpower-power vp2))
                (v1 (varpower-symbol vp1))
                (v2 (varpower-symbol vp2)))
           (cond ((eq v1 v2)
                  (liketerms-vps (cons (list 'v (+ p1 p2) v1)
                                       (rest (rest vps)))))
                 ((= 0 p1)
                  (liketerms-vps (rest vps)))
                 (t
                  (cons vp1 (liketerms-vps (rest vps)))))))
        (t
         ;; vps only has one element
         ;; (no need to check for other variables to "normalize")
         (let ((vp (first vps)))
           (if (= 0 (varpower-power vp))
               (liketerms-vps (rest vps))
               (cons vp (liketerms-vps (rest vps))))))))

;; liketerms-ms removes all duplicate monomials (by adding their coefficients)
;; and "null" monomials (coefficient 0)
(defun liketerms-ms (ms)
  (cond ((null ms)
         nil)
        ((not (null (second ms)))
         (let* ((m1 (first ms))
                (m2 (second ms))
                (c1 (monomial-coefficient m1))
                (c2 (monomial-coefficient m2))
                (td (monomial-degree m1))
                (vps1 (varpowers m1))
                (vps2 (varpowers m2)))
           (cond ((= 0 c1)
                  (liketerms-ms (rest ms)))
                 ((equal vps1 vps2)
                  (liketerms-ms (cons (list 'm (+ c1 c2) td vps1)
                                      (rest (rest ms)))))
                 (t
                  (cons m1 (liketerms-ms (rest ms)))))))
        (t
         (let ((m (first ms)))
           (if (= 0 (monomial-coefficient m))
               (liketerms-ms (rest ms))
               (cons m (liketerms-ms (rest ms))))))))

(defun liketerms (ms)
  ;; sort the input (monomials) by monomial<, normalize and return it
  (liketerms-ms (sort (copy-seq ms) #'monomial<)))

(defun polyplus (p1 p2)
  "Return the polynomial produced by the polynomial sum of P1 and P2.
   Input P1, structure of type Poly or S-Exp;
   Input P2, structure of type Poly or S-Exp."
  (list 'poly (liketerms (append (monomials p1) (monomials p2)))))

;; Negation functions
;; neg-m inverts M's coefficient
(defun neg-m (m)
  (list 'm (* -1 (monomial-coefficient m)) (monomial-degree m) (varpowers m)))

;; neg-ms inverts all MS' coefficients
(defun neg-ms (ms)
  (if ms
      (cons (neg-m (first ms))
            (neg-ms (rest ms)))))

;; neg-poly inverts all monomials' coefficients in P
(defun neg-poly (p)
  (list 'poly (neg-ms (monomials p))))

(defun polyminus (p1 p2)
  "Return the polynomial produced by the polynomial subtraction of P1 and P2.
   Input P1, structure of type Poly or S-Exp;
   Input P2, structure of type Poly or S-Exp."
  (polyplus p1 (neg-poly p2)))

(defun monotimes (m1 ms2)
  (if ms2
      (let ((m2 (first ms2)))
        ;; append vps1 and vps2 (first ms2's vps), sort it by variable and
        ;; normalize it (liketerms-vps)
        (cons (list 'm
                    (* (monomial-coefficient m1) (monomial-coefficient m2))
                    (+ (monomial-degree m1) (monomial-degree m2))
                    (liketerms-vps (sort (copy-seq (append (varpowers m1)
                                                           (varpowers m2)))
                                         #'string< :key #'third)))
              (monotimes m1 (rest ms2))))))

(defun monostimes (ms1 ms2)
  (if ms1
      (append (monotimes (first ms1) ms2)
              (monostimes (rest ms1) ms2))))

(defun polytimes (p1 p2)
  "Return the polynomial produced by the polynomial multiplication of P1 and P2.
   Input P1, structure of type Poly or S-Exp;
   Input P2, structure of type Poly or S-Exp."
  (list 'poly (liketerms (monostimes (monomials p1) (monomials p2)))))

;; as- functions
;; as-vp Symbol              → (V 1 Symbol)
;; as-vp (expt Symbol Power) → (V Power Symbol)
(defun as-vp (exp)
  (cond ((and (listp exp)
              (eq 'expt (first exp))
              (symbolp (second exp))
              (integerp (third exp))
              (>= (third exp) 0))
         (list 'v (third exp) (second exp)))
        ((symbolp exp)
         (list 'v 1 exp))))

;; as-vps
;; list of recursive calls to as-vp
(defun as-vps (lexp)
  (if lexp
      (cons (as-vp (first lexp)) (as-vps (rest lexp)))))

(defun total-degree-vps (vps)
  (if vps
      (+ (varpower-power (first vps))
         (total-degree-vps (rest vps)))
      0))

;; as-monomial-help Number                  → (M Number 0 NIL)
;; as-monomial-help Symbol                  → (M 1 1 (V 1 Symbol))
;; as-monomial-help (* &rest Symbol)        → (M 1 TD VPS)
;; as-monomial-help (* Number &rest Symbol) → (M Number TD VPS)
(defun as-monomial-help (exp)
  (cond ((numberp exp)
         (list 'm exp 0 nil))
        ((or (symbolp exp)
             (and (listp exp)
                  (eq 'expt (first exp))))
          (let ((vps (cons (as-vp exp) nil)))
            (list 'm 1 (total-degree-vps vps) vps)))
        ((and (listp exp)
              (eq '* (first exp))
              (numberp (second exp)))
         (let* ((vps (liketerms-vps (sort (copy-seq (as-vps (rest (rest exp))))
                                          #'string< :key #'third)))
                (td (total-degree-vps vps)))
           (list 'm (second exp) td vps)))
        ((and (listp exp)
              (eq '* (first exp)))
         (let* ((vps (liketerms-vps (sort (copy-seq (as-vps (rest exp)))
                                          #'string< :key #'third)))
                (td (total-degree-vps vps)))
           (list 'm 1 td vps)))
         (t
          (error "Invalid input type."))))

(defun null-m (m)
  (if (= 0 (monomial-coefficient m))
      ;; null coefficient: ignore vps
      (list 'm 0 0 nil)
      m))

(defun as-monomial (exp)
  "Return a structure of type Mono representing the monomial parsed from EXP.
   Input EXP, Symbolic-Expression."
  (null-m (as-monomial-help exp)))

;; as-monomials-help
;; list of recursive calls to as-monomial
(defun as-monomials-help (exp)
  (if exp
      (cons (as-monomial (first exp))
            (as-monomials-help (rest exp)))))

;; as-monomials Mono         → (Mono . NIL)
;; as-monomials (&rest Mono) → Monos
(defun as-monomials (exp)
  (if (and (listp exp)
           (eq '+ (first exp)))
      (as-monomials-help (rest exp))
      (cons (as-monomial exp) nil)))

(defun as-polynomial (exp)
  "Return a structure of type Poly representing the polynomial parsed from EXP.
   Input EXP, Symbolic-Expression."
  (list 'poly (liketerms (as-monomials exp))))

(defun polyval-vps (vps vars varvals)
  (if vps
      (let* ((vp (first vps))
             (val (nth (position (varpower-symbol vp) vars) varvals)))
        ;; val is the element at position index in varvals
        ;; (where index is the position of varpower-symbol in vars)
        (if val
            (* (expt val (varpower-power vp))
               (polyval-vps (rest vps) vars varvals))
            (error "Invalid variable values (less than expected).")))
      1))

(defun polyval-m (m vars varvals)
  (* (monomial-coefficient m) (polyval-vps (varpowers m) vars varvals)))

(defun polval-ms (ms vars varvals)
  (if ms
      (+ (polyval-m (first ms) vars varvals)
         (polval-ms (rest ms) vars varvals))
      0))

(defun polyval (p varvals)
  "Return the value of P in the n-dimensional point represented by VARVALS.
   Input P, structure of type Poly or S-Exp;
   Input VARVALS, list of numbers (mapped to function variables' output)."
  (let ((ms (monomials p)))
    (polval-ms ms (variables (list 'poly ms)) varvals)))

;; PPrint functions
;; OUT is the output string
;; pprint-varpowers (V 1 Symbol)     → Symbol
;; pprint-varpowers (V Power Symbol) → Symbol^Power
;; pprint-varpowers (&rest VPs)      → VP * PrintedVPs
(defun pprint-varpowers (vps out)
  (let* ((vp (first vps))
         (p (varpower-power vp))
         (v (varpower-symbol vp)))
    (cond ((and (null (rest vps))
                (= p 1))
           (format out "~a" v))
          ((null (rest vps))
           (format out "~a^~d" v p))
          ((= p 1)
           (format out "~a * " v)
           (pprint-varpowers (rest vps) out))
          (t
           (format out "~a^~d * " v p)
           (pprint-varpowers (rest vps) out)))))

;; pprint-monomial (M Number 0 NIL)  → Number
;; pprint-monomial (M 1 TD VPs)      → PrintedVPs
;; pprint-monomial (M Number TD VPs) → Number * PrintedVPs
(defun pprint-monomial (m out)
  (let ((c (monomial-coefficient m))
        (vps (varpowers m)))
    (cond ((null vps)
           (format out "~d" c))
          ((= c 1)
           (pprint-varpowers vps out))
          (t
           (format out "~d * " c)
           (pprint-varpowers vps out)))))

;; pprint-monomials Mono         → PrintedMono\n
;; pprint-monomials (&rest Mono) → PrintedMono + PrintedMonos
(defun pprint-monomials (ms out)
  (if (null (rest ms))
      (progn (pprint-monomial (first ms) out)
             (format out "~%"))
      (progn (pprint-monomial (first ms) out)
             (format out " + ")
             (pprint-monomials (rest ms) out))))

(defun pprint-polynomial (p)
  "Output a traditional representation of P to *STANDARD-OUTPUT*.
   Input P, structure of type Poly or S-Exp."
  (let ((out (make-string-output-stream)))
      ;; out is a string output-stream
      ;; write on out with format calls in pprint-monomials
      (pprint-monomials (monomials p) out)
      ;; print out to standard-output (t)
      (format t "~a" (get-output-stream-string out))))

;;;; end of file -- mvpoli.lisp --
