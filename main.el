;; Known issues:
;; The code is ugly.
;; step-stop is not functional
;; Multi dimensional functions only work if you put ; instead of , (can be fixed by changing the parse-logic function)
;; Until then this will not be merged into master

(require 'cl-lib)

(defvar step-stop nil)

(defun string-to-operator
    (str)
  "Takes a string a string and returns its operator if it is one, 
   otherwise nil. For example \\rightarrow becomes 'rightarrow"
  (cond
   ((string-equal str "\\rightarrow") 'rightarrow)
   ((string-equal str "\\&") 'and)
   ((string-equal str "\\vee") 'or)
   ((string-equal str "\\neg") 'not)
   ((string-equal str "\\exists") 'exists)
   ((string-equal str "\\forall") 'forall)))

(defun operator-to-string (op)
  (cond
   ((eq op 'rightarrow) "\\rightarrow")
   ((eq op 'and) "\\&")
   ((eq op 'or) "\\vee")
   ((eq op 'not) "\\neg")
   ((eq op 'exists) "\\exists")
   ((eq op 'forall) "\\forall")))

(defun operator-to-precedence
    (op)
  "Takes an operator and returns its precedence, with the highest
   precendece being 0. The lower the precedence the higher the number."
  (cond
   ((eq 'rightarrow op) 0)
   ((eq 'and op) 1)
   ((eq 'or op) 1)
   ((eq 'not op) 2)
   ((eq 'exists op) 2)
   ((eq 'forall op) 2)))

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
(defun remove-outer-parentheses (str)
  "Trims the expression of any white spaces and removes the first and last
   if they're respectively an ( and a ) character"
  (let ((str (string-trim str)))
    (if
	(and
	 (= (elt str 0) (string-to-char "("))
	 (= (get-matching-parenthesis str 0) (1- (length str))))
	(remove-outer-parentheses
	 (substring str 1 (1- (length str))))
      str)))

(defun get-matching-parenthesis (str n)
  "Very ugly hack but it works"
  (caddr
   (cl-reduce
    (lambda
      (x y) ; x = (index paren-count result)
      (list (1+ (car x))
	    (cond
	     ((= y (string-to-char "(")) (1+ (cadr x)))
	     ((= y (string-to-char ")")) (1- (cadr x)))
	     ('t (cadr x)))
	    (if
		(and
		 (= y (string-to-char ")"))
		 (= (cadr x) 1)
		 (not (caddr x)))
		(car x)
	      (caddr x))))
    (substring str n) :initial-value '(0 0))))

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
		   (<=
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
	 (if
	     (or
	      (eq 'forall (car operator-delimiter))
	      (eq 'exists (car operator-delimiter)))
	     (let* (( thing
		      (string-trim
		       (substring
			str
			(string-match "\s" str (cadr operator-delimiter)))))
		    (i (string-match "\s" thing)))
	       
	       (list
		(car operator-delimiter)
		(substring thing 0 i)
		(substring thing (1+ i))))
	   (list
	    (car operator-delimiter)
	    (unless (eq (car operator-delimiter) 'not)
	      (string-trim (substring str 0 (cadr operator-delimiter))))
	    (string-trim
	     (substring
	      str
	      (string-match "\s" str (cadr operator-delimiter)))))))))))

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

(defun generate-label (logic left-or-right)
  (let
      ((op
	(car (cond
	      ((eq left-or-right 'left) (car (last (car logic))))
	      ((eq left-or-right 'right) (caadr logic))))))
    (string-join
     (list
      (operator-to-string op)
      "-"
      (if
	  (eq left-or-right 'left)
	  "S"
	"D")))))

(defun apply-function (logic left-or-right)
  "Applies a function. Logic is a `parse-logic`-like formatted list, with
   the outermost left or innermost right expression parsed with 
   parse-expression, depending if left-or-right is 'left or 'right.
   The target expression must not be a complete expression."
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
	    (append (car logic) (list (cadr exp)))
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
	  (cdadr logic)))))
     ((eq (car exp) 'forall)
      (if
	  (eq left-or-right 'right)
	  (list
	   (list
	    (car logic)
	    (cons
	     (replace-regexp-in-string
	      (string-join
	       (list
		"[\s(,;]\\(" (cadr exp) "\\)[\s),;]"))
	      (get-new-variable logic)
	      (caddr exp) nil nil 1)
	     (cdadr logic))))
	(list
	 (list
	  (cl-remove-if-not
	   #'identity
	   (append
	    (car logic)
	    (let ((var (interactively-get-variable logic)))
	      (list
	       (when var
		 (replace-regexp-in-string
		  (string-join
		   (list "[\s(,;]\\(" (cadr exp) "\\)[\s),;]"))
		  var
		  (caddr exp) nil nil 1))))))
	  (cadr logic)))))
     ((eq (car exp) 'exists)
      (if
	  (eq left-or-right 'left)
	  (list
	   (list
	    (append
	     (butlast (car logic))
	     (list (replace-regexp-in-string
		    (string-join
		     (list
		      "[\s(,;]\\(" (cadr exp) "\\)[\s),;]"))
		    (get-new-variable logic)
		    (caddr exp) nil nil 1)))
	    (cadr logic)))
	(list
	 (list
	  (car logic)
	  (cl-remove-if-not
	   #'identity
	   (append
	    (let ((var (interactively-get-variable logic)))
	      (list
	       (when var
		 (replace-regexp-in-string
		  (string-join
		   (list "[\s(,;]\\(" (cadr exp) "\\)[\s),;]"))
		  var
		  (caddr exp) nil nil 1))))
	    (cadr logic))))))))))


(defun interactively-get-variable (logic)
  (interactive)
  (let
      ((read-str (read-string
		  (format "Select a variable. %s is the exp"
			  (logic-to-str logic)))))
    (if
	(string-equal read-str "")
	(progn (setq step-stop 't) nil)
      read-str)))

(defun get-new-variable
    (logic &optional i)
  (if (string-match
       (string-join
	(list
	 "[\s(]x"
	 (when i "_")
	 (when i (number-to-string i))
	 "[\s)]"))
       (logic-to-str logic))
      (get-new-variable logic (1+ (if i i -1)))
    (if i
	(string-join (list "x_" (number-to-string i)))
      "x")))


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
      ((best-exp (get-best-exp logic))
       (parsed-logic
	(mapcar
	 (lambda (x)
	   (mapcar #'parse-expression x))
	 logic)))
    (if (index-is-functionable logic (car best-exp) (cadr best-exp))
	(list
	 (apply-function
	  parsed-logic
	  (cadr best-exp))
	 (generate-label parsed-logic (cadr best-exp)))
      (list (list (move-expression-logic
		   logic
		   (car best-exp)
		   (cadr best-exp)))
	    (string-join
	     (list
	      "sc_{"
	      (if (eq (cadr best-exp) 'left)
		  "sx}"
		"dx}")))))))

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
    (cond
     ((eq right-best nil) (list (car left-best) 'left))
     ((eq left-best nil) (list (car right-best) 'right))
     ((> (cadr left-best) (cadr right-best)) (list (car left-best) 'left))
     ('t (list (car right-best) 'right)))))

(defun exp-heuristic (exp i left-or-right)
  "Heurisitc"
  (list
   i
   (cond
    ((is-complete-expression exp) -10000) ;; if it's a complete exp. it is not the best exp to analyze at all
    ((eq left-or-right 'left) i) ;; if it's on the left, higher the index the better it is
    ((eq left-or-right 'right) (- i))))) ;; if it's on the right, the lower the index the better.

(defun max-cadr (seq)
  (unless (zerop (length seq))
    (cl-reduce
     (lambda (x y)
       (if
	   (> (cadr x) (cadr y))
	   x y))
     seq)))

(defun is-axiom (logic)
  (cl-some
   (lambda (x)
     (cl-some
      (lambda (y)
	(string-equal
	 (remove-outer-parentheses (expr-to-str y))
	 (remove-outer-parentheses (expr-to-str x))))
      (cadr logic)))
   (car logic)))

(defun logic-is-done (logic)
  (or
   (is-axiom logic)
   (and
    (cl-every #'is-complete-expression (car logic))
    (cl-every #'is-complete-expression (cadr logic)))))

(defun logic-to-str (logic)
  (string-join
   (list
    (cl-reduce
     (lambda (x y)
       (string-join
	(list x (expr-to-str y))
	(if
	    (zerop (length x))
	    ""
	  ", ")))
     (car logic) :initial-value "")
    " \\vdash "
    (cl-reduce
     (lambda (x y)
       (string-join
	(list x (expr-to-str y))
	(if (zerop (length x))
	    ""
	  ", ")))
     (cadr logic) :initial-value ""))))

(defun expr-to-str (expr)
  (cond
   ((stringp expr) (string-trim expr))
   ((eq (car expr) 'not) (string-join (list "\\neg " (cadr expr))))
   ((or (eq (car expr) 'forall) (eq (car expr) 'exists))
    (string-join
     (list
      (operator-to-string (car expr))
      (cadr expr)
      (caddr expr)) " "))
   ('t  (string-join
	 (list
	  (cadr expr)
	  (operator-to-string (car expr))
	  (caddr expr)) " "))))

(defun generate-logic (str)
  (interactive "M")
  (insert "\\begin{prooftree}\n")
  (insert (generate-tree (parse-logic str)))
  (insert "\\end{prooftree}\n"))

(defun generate-tree (logic)
  (if (or step-stop (logic-is-done logic))
      (progn
	(setq step-stop nil)
	(let ((is-axiom (is-axiom logic)))
	  (string-join
	   (list "\\AxiomC{$"
		 (when is-axiom
		   "\\overset{ax-id}{")
		 (logic-to-str logic)
		 (when is-axiom "}")
		 "$}\n"))))
    (let*
	( (stepped-logic (step-logic logic))
	  (next-step (car stepped-logic))
	  (label (cadr stepped-logic)))
      (string-join
       (if
	   (= (length next-step) 1)
	   (list
	    (generate-tree
	     (car
	      next-step))
	    "\\RightLabel{$"
	    label
	    "$}\n"
	    "\\UnaryInfC{$"
	    (logic-to-str logic)
	    "$}\n")
	 (list
	  (generate-tree
	   (car
	    next-step))
	  (generate-tree (cadr next-step))
	  "\\RightLabel{$"
	  label
	  "$}\n"
	  "\\BinaryInfC{$"
	  (logic-to-str logic)
	  "$}\n"))))))

(require 'tex-mode)
(bind-key "C-c C-e" #'generate-logic #'tex-mode-map)

