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
   (lambda (x)
     (unless (= (length (string-trim x)) 0)
       (split-string x ",")))
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
  "Takes an expression, e.g. \" A \\& B \" and returns its parsed rapresentation (in prefix rapresentation),
   in this example, ('and \"A\" \"B\") or itself if str is a complete expression"
  (if
      (is-complete-expression str)
      (string-trim str)
    (let* ((str (remove-outer-parentheses str))
	   (operator-delimiter
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
       #'identity
       (list
	(car operator-delimiter)
	(unless (eq (car operator-delimiter) 'not)
	  (string-trim (substring str 0 (cadr operator-delimiter))))
	(string-trim (substring str (string-match "\s" str (cadr operator-delimiter)))))))))

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

(defun is-complete-expression (str)
  "Returns non-nil when str is a complete expression, like 'A' (i.e. when str doesn't contain an operator)"
  (not (string-match "\\\\" str)))

(defun count-nestation
    (str n)
  "Counts how many parentheses the nth character is inside"
  (let
      ((substr (substring str 0 n) ))
    (-
     (seq-count
      (lambda (x) (= x (string-to-char "(")))
      substr)
     (seq-count
      (lambda (x) (= x (string-to-char ")")))
      substr))))

(defun apply-function (logic left-or-right)
  "Applies a function"
  (let
      ((exp 
	(cond
	 ((eq left-or-right 'left) (car (last (car logic))))
	 ((eq left-or-right 'right) (caadr logic)))))
    (cond
     ((eq (car exp) 'and)
      (if
	  (eq left-or-right 'left)
	  (list
	   (list
	    (append (butlast (car logic)) (cdr exp))
	    (cadr logic)))
	(list
	 (list
	  (car logic)
	  (append (list (cadr exp)) (cdadr logic)))
	 (list
	  (car logic)
	  (append (list (caddr exp)) (cdadr logic))))))
     ((eq (car exp) 'or)
      (if (eq left-or-right 'right)
	  (list
	   (list
	    (car logic)
	    (append (cdr exp) (cdadr logic))))
	(list
	 (list
	  (append (butlast (car logic)) (list (cadr exp)))
	  (cadr logic))
	 (list
	  (append (butlast (car logic)) (list (caddr exp)))
	  (cadr logic)))))
     ((eq (car exp) 'rightarrow)
      (if (eq left-or-right 'right)
	  (list
	   (list
	    (append (list (car logic)) (list (cadr exp)))
	    (append (cddr exp) (cdadr logic))))
	(list
	 (list
	  (append (butlast (car logic)) (cddr exp))
	  (cadr logic))
	 (list
	  (butlast (car logic))
	  (append (list (cadr exp)) (cadr logic))))))
     ((eq (car exp) 'not)
      (if
	  (eq left-or-right 'left)
	  (list
	   (list
	    (butlast (car logic))
	    (append (cdr exp) (cadr logic))))
	(list
	 (list
	  (append (car logic) (cdr exp))
	  (cdadr logic))))))))

(message "%S"
	 (car (apply-function
	  (mapcar
	   (lambda (x)
	     (mapcar
	      (lambda (x)
		(parse-expression x))
	      x))
	   (parse-logic
	    "(A \\& C), \\neg (B \\& C) \\vdash "))
	  'left)))

