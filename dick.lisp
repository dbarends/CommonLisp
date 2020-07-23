(defun hello-world ()
  (format t "Hello, World!"))

					; comment
(defvar db '(:voornaam "Dick" :achternaam "Barends"))

#|Dit is commentaar|#

(getf db :voornaam)

(defun fact (x)
  "Deze functie berekend de factorial"
  (if ( = x 1)
      1
      (* x (fact (- x 1)))))
