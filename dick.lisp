;;;; Dit bestand bevat diverse functies.
;;;;
;;;; Het doel is te oefenen met Common Lisp syntax.

;;; dck-factorial: int -> int
(defun dck-factorial (x)		; De signatuur van de functie.
  "Berekend de factorial van het argument" ; De  beschrijving van het doel van de functie.
  (if ( = x 1)				; Het lichaam van de functie.
      1
      (* x (dck-factorial (- x 1)))))


;;; monty-hall(n): int -> int
;;; (bron: Vsevolod Dyomkin's blog)
(defun monty-hall (n)
  "Returns a ratio of success rate to failure rate, if we follow
the strategy of always switching the initially selected door in
the Monty Hall Problem"
  (let ((succ 0.0)
        (fail 0.0))
    (dotimes (i n)
      (let ((choice (random 3)) 
            (prize (random 3)))
        (if (= choice prize)
            (incf fail) ; we've chosen the right door, but will need to switch
            (incf succ)))) ; we've chosen the wrong door and after switching -- the right
    (/ succ fail)))


;;; our-equal(x,y): string -> string -> bool
(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
	   (consp y)
	   (our-equal (car x) (car y))
	   (our-equal (cdr x) (cdr y)))))

;;;------------------------------------------------------------------------------
;;; Functies voor compress (run-length coding; boek ANSI CL)
;;;------------------------------------------------------------------------------
(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n) (compr next 1 (cdr lst)))))))

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
	    (rest (uncompress (cdr lst))))
	(if (consp elt)
	    (append (apply #'list-of elt)
		    rest)
	    (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

;;;------------------------------------------------------------------------------

(defun dck-sum-tot (n)
  (cond ((eql n 1) 1)
	((eql n 2) 3)
	((and (> n 2) (evenp n)) (+ (* 3 (dck-sum-tot (/ n 2))) (dck-sum-tot (- (/ n 2) 1))))
	((and (> n 2) (not (evenp n))) (+ (* 3 (dck-sum-tot (/ (- n 1) 2))) (dck-sum-tot (/ (+ n 1)  2))))))

(defun dck-sum-tot2 (n)			; Stack overflow bij grote getallen
  (cond ((eql n 1) 1)
	((> n 1) (+ n (dck-sum-tot2 (- n 1))))))

;;;------------------------------------------------------------------------------

;; dck-power-lin(x,n): int->int->int (x^n)
;; Berekend in linaire tijd.
(defun dck-power-lin (x n)
  (cond ((eql n 0) 1)
	((> n 0) (* x (dck-power-lin x (1- n))))))

;; dck-power-lin(x,n): int->int->int (x^n)
;; Berekend in logaritmische tijd.
(defun dck-power-log (x n)
  (cond ((eql n 0) 1)
	((> n 0) 
	 (if (evenp n)
	     (dck-kwadraad (dck-power-log x (/ n 2)))
	     (* x (dck-kwadraad (dck-power-log x (/ (- n 1) 2))))))))

(defun dck-kwadraad (x)
  (* x x))
