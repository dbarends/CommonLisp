;;;; The Archive functies

;;; /Users/dickbarends/Dropbox/KnowlegeManagement/*.*
;;; (lees-ta-bestand (caddr (lees-bestands-namen "./*.*")))

;; Zet de thuis folder naar de Knowledgemangement folder
(setf *default-pathname-defaults* #P"/Users/dickbarends/Dropbox/KnowlegeManagement/")


(defparameter *ht* (make-hash-table :test #'equal) "id en bestandsnaam hash-tabbel")

(defun lees-ta-bestand (file)
  "bestand -> list of strings"
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof) (read-line str nil 'eof))
	 (accu '() (cons line accu))) 
        ((eql line 'eof) (reverse accu)))))

(defun lees-bestands-namen (dir)
  (remove-if-not #'(lambda (x) (string= (subseq x 0 2) '"20"))
		 (remove-if #'(lambda (x) (string= (elt x (- (length x) 1)) '"~"))
			    (remove-if #'(lambda (x) (string= x '"")) ; Haal de empty strings er uit
				       (mapcar #'file-namestring (directory dir)))))) ; Alleen bestandsnaam met extensie

(defun lees-ids (lst)      ; lst is een lijst met TA bestandsnamen
  (let ((accu '()))
    (dolist (naam lst accu)    ;lopende var, lijst, return var
      (if (and (> (length naam) 12))
	  (string-equal (subseq naam 0 2) "20"))
      (push (subseq naam 0 11) accu))))

(defun lees-ids-ht (lst)      ; lst is een lijst met TA bestandsnamen
  (dolist (naam lst)    ;lopende var, lijst, return var
    (if (and (> (length naam) 12))
	(string-equal (subseq naam 0 2) "20"))
    (setf (gethash (subseq naam 0 12) *ht*) (subseq naam 13))))
