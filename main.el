(require 'cl-lib)

(defvar operator-precedence (list 'rightarrow 'and 'or 'not))

(defun string-to-operator
    (str)
  (cond
   ((string-equal str "\\rightarrow") 'rightarrow)
   ((string-equal str "\\&") 'and)
   ((string-equal str "\\vee") 'or)
   ((string-equal str "\\neg") 'not)))
(defun operator-to-precedence
    (op)
  (cond
   ((eq 'rightarrow op) 0)
   ((eq 'and op) 1)
   ((eq 'or op) 1)
   ((eq 'not op) 2)))

;; e.g. A, B \& A \vdash C, C \vee A =>
;;(("A" "B\&A") ("C" "C \vee A"))
(defun parse-logic (line)
  (mapcar
   (lambda (x) (split-string x ","))
   (split-string line "\\\\vdash")))

(defun remove-outer-parentheses (str)
  (let ((str (string-trim str)))
    (if
	(and
	 (= (elt str 0) (string-to-char "("))
	 (= (elt str (1- (length str))) (string-to-char ")")))
	(remove-outer-parentheses
	 (substring str 1 (1- (length str))))
      str)))

;; Get the lowest nested \ and then analyze it
(defun parse-expression
    (str)
  (let* ( (str (remove-outer-parentheses str))
	 ( operator-delimiter
	  (cl-reduce
	   (lambda (x y)
	     (if (<
		  (operator-to-precedence (car x))
		  (operator-to-precedence (car y)))     
		 x
	       y))
	   (mapcar
	    (lambda (x)
	      (list
	       (string-to-operator
		(substring
		 str
		 (car x)
		 (string-match "\s" str (car x))))
	       (car x)))
	    (cl-remove-if-not
	     (lambda (x) (= (cadr x) 0))
	     (mapcar
	      (lambda (x) (list x (count-nestation str x)))
	      (string-find-chars str (string-to-char "\\"))))))))
    (cl-remove-if-not
     (lambda (x) x)
     (list
      (car operator-delimiter)
      (unless (eq (car operator-delimiter) 'not)
	(string-trim (substring str 0 (cadr operator-delimiter))))
      (string-trim (substring str (string-match "\s" str (cadr operator-delimiter))))))))

(defun string-find-chars (str c)
  "Get a list with the indices"
  (car
   (cl-reduce
    ;; x = ((1 3 5 6) i)
    (lambda (x y)
      (list
       (if (= y c)
	   (append (car x) (cdr x))
	 (car x))
       (1+ (cadr x))))
    str :initial-value (list nil 0))))
;; How many parentheses the nth character is inside
(defun count-nestation
    (str n)
  (let
      ((substr (substring str 0 n) ))
    (-
     (seq-count
      (lambda (x) (= x (string-to-char "(")))
      substr)
     (seq-count
      (lambda (x) (= x (string-to-char ")")))
      substr))))

(message "%S"
	  (mapcar #'parse-expression (car (parse-logic "(A \\& B) \\rightarrow B \\vee C, C \\& D \\vdash C \\vee D"))))
