(require 'cl-lib)

(defun string-to-operator
    (str)
  "Takes a string a string and returns its operator if it is one, 
   otherwise nil. For example \\rightarrow becomes 'rightarrow"
  (cond
   ((string-equal str "\\rightarrow") 'rightarrow)
   ((string-equal str "\\&") 'and)
   ((string-equal str "\\vee") 'or)
   ((string-equal str "\\neg") 'not)))

(defun operator-to-precedence
    (op)
  "Takes an operator and returns its precedence, with the highest
   precendece being 0. The lower the precedence the higher the number."
  (cond
   ((eq 'rightarrow op) 0)
   ((eq 'and op) 1)
   ((eq 'or op) 1)
   ((eq 'not op) 2)))

(defun parse-logic (line)
  "Parses a logic expression and returns a nested list like so: 
   A, B, \\& A \\vdash C, C \\vee A =>
   ((A \"B \\& A\") (C \"C \\vee A\"))"
  (mapcar
   (lambda (x)
     (unless (= (length (string-trim x)) 0)
       (split-string x ",")))
   (split-string line "\\\\vdash")))

(defun remove-outer-parentheses (str)
  "Trims the expression of any white spaces and removes the first and last
   if they're respectively an ( and a ) character"
  (let ((str (string-trim str)))
    (if
	(and
	 (= (elt str 0) (string-to-char "("))
	 (= (elt str (1- (length str))) (string-to-char ")")))
	(remove-outer-parentheses
	 (substring str 1 (1- (length str))))
      str)))

(defun parse-expression
    (str)
  "Takes an expression, e.g. \"A \\& B\" and returns its parsed rapresentation (in prefix rapresentation) or itself if str is a complete expression.
   In the above example, ('and \"A\" \"B\")"
  (if (listp str)
      str ;; str has already been parsed
    (if
	(is-complete-expression str)
	(string-trim str)
      (let*
	  ((str (remove-outer-parentheses str))
	   (operator-delimiter
	    (cl-reduce
	     (lambda (x y)
	       (if
		   (<
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
	  (string-trim
	   (substring
	    str
	    (string-match "\s" str (cadr operator-delimiter))))))))))

(defun string-find-chars (str c)
  "Get a list with the indices of where in the string c is present"
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
  (and
   (not (listp str)) ;; if str is a list, then of course it's not complete
   (not (string-match "\\\\" str))))

(defun count-nestation
    (str n)
  "Counts by how many parentheses the nth character is inside"
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
  "Applies a function. Logic is a `parse-logic`-like formatted list, with
   the outermost left or innermost right expression parsed with 
   parse-expression, depending if left-or-right is 'left or 'right.
   The target expression must not be a complete expression"
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

(defun move-expression
    (exps i left-or-right)
  "Moves the expression in the index i to either the end or the start"
  (cond
   ((eq left-or-right 'left)
    (append
     (seq-subseq exps 0 i)
     (seq-subseq exps (1+ i))
     (list (nth i exps))))
   ((eq left-or-right 'right)
    (cons
     (nth i exps)
     (append
      (seq-subseq exps 0 i)
      (seq-subseq exps (1+ i)))))))

(defun move-expression-logic (logic i left-or-right)
  (if (eq left-or-right 'left)
      (list (move-expression (car logic) i 'left) (cadr logic))
    (list (car logic) (move-expression (cadr logic) i 'right))))

(defun index-is-functionable (logic i left-or-right)
  "(I ran out of names). It returns non-nil if the index is 
at the end of the list of left exps. or at the beginnign of the list of
right exps."
  (if (eq left-or-right 'left)
      (eq i (1- (length (car logic))))
    (eq i 0)))

(defun step-logic (logic)
  "Either moves an expression or applies a function and returns the results"
  (let
      ((best-exp (get-best-exp logic)))
    (if (index-is-functionable logic (car best-exp) (cadr best-exp))
	(apply-function
	 (mapcar
	  (lambda (x)
	    (mapcar #'parse-expression x))
	  logic)
	 (cadr best-exp))
      (list (move-expression-logic
	     logic
	     (car best-exp)
	     (cadr best-exp)))
      )))


(defun get-best-exp (logic)
  "Returns a list with the index of the best expression and
'left or 'right"
  (let
      ((left-best
	(max-cadr
	 (seq-map-indexed
	  (lambda (x i) (exp-heuristic x i 'left))
	  (car logic))))
       (right-best
	(max-cadr
	 (seq-map-indexed
	  (lambda (x i) (exp-heuristic x i 'right))
	  (cadr logic)))))
    (if
	(> (cadr left-best) (cadr right-best))
	(list (car left-best) 'left)
      (list (car right-best) 'right))))

(defun exp-heuristic (exp i left-or-right)
  "Heurisitc"
  (list
   i
   (cond
    ((is-complete-expression exp) -10000) ;; if it's a complete exp. it is not the best exp to analyze at all
    ((eq left-or-right 'left) i) ;; if it's left, higher the index the better it is
    ((eq left-or-right 'right) (- i))))) ;; if it's on the right, the lower the index the better.

(defun max-cadr (seq)
  (cl-reduce
   (lambda (x y)
     (if
	 (> (cadr x) (cadr y))
	 x y))
   seq))

(message "%S"
	 (mapcar (lambda (x) (step-logic (car x))) (mapcar #'step-logic (step-logic (car (step-logic (car (step-logic (parse-logic
														       "(A \\& C), \\neg (B \\& C), C, A \\vdash A, B")))))))))

