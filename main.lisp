
;
;	QUESTION 1
;
;	Simply return a list of all variables in the equation
;

;accumulate for a 2 argument function - append the two argument's variable accumulus
(defun find-vars-accum-bin (lhs_query rhs_query accum)
  (append
	(find-vars-accum lhs_query ()) 
	(append
	  (find-vars-accum rhs_query ()) 
	  accum
	  )
	)
  )

(defun find-vars-accum (query accum)
  (cond
	; keep searching one level further
	((eq `s-neg (car query)) (find-vars-accum (nth 1 query) accum)) 
	; keep searching in both arguments one level deeper
	((eq `s-sub (car query)) (find-vars-accum-bin (nth 1 query) (nth 2 query) accum)) 
	; keep searching in both arguments one level deeper
	((eq `s-add (car query)) (find-vars-accum-bin (nth 1 query) (nth 2 query) accum)) 
	; add variable to accumulator (base case)
	((eq `s-var (car query)) (list (nth 1 query))) 
	)
  )

(defun find-vars (query) 
	; remove duplicates of all variables we find
  	(remove-duplicates (find-vars-accum query ()))
	)
	
;
; QUESTION 2
;
; Question required to follow conversion rules
; '(s-neg a)	-> '(- 0 a)
; '(s-sub a b)	-> '(- a b)
; '(s-add a b)	-> '(+ a b)
; '(s-var a)	-> '(val(a)) [where val(a) refers to the value in env
;

; retrieves the value for a variable
(defun inserter (environ val)
	(if (eq (car (car environ)) val) (nth 1 (car environ)) (inserter (rest environ) val))
)

; concatenate two halves of transformers
(defun rec-transformer-bin (environ lhs rhs)
	(append (rec-transformer environ lhs) (rec-transformer environ rhs))

)

; construct a new query which is the exact same, but with the following rules:
; (s-neg X) -> (- 0 X)
; (s-sub X Y) -> (- X Y)
; (s-add X Y) -> (+ X Y)
; (s-var X) -> (val(X))
(defun rec-transformer (environ query)
	(cond
	 ((eq `s-neg (car query)) (list (cons `- (cons `0 (rec-transformer environ (nth 1 query)))))) ;add the negation function to the construct
	 ((eq `s-sub (car query)) (list (cons `- (rec-transformer-bin environ (nth 1 query) (nth 2 query) )))) ;add the subtraction function to the construct
	 ((eq `s-add (car query)) (list (cons `+ (rec-transformer-bin environ (nth 1 query) (nth 2 query) )))) ;add the add function to the construct
	 ((eq `s-var (car query)) (list (inserter environ (nth 1 query)))) ;insert variable
	 (T ())
	)
)

(defun transformer (query)
	(lambda (environ) (eval (car(rec-transformer environ query))))
)

;
; Question 3
;
;	Perform simplification to the minimum number of expressions
;


;assign values to each of the found variables and concatenate them 
(defun gen-tuple (vars index)
	(cond 
		((null vars) nil) 
		((eq 0 index) (append (list (list (car vars) 1)) (gen-tuple (cdr vars) (- index 1)))) 
		(T (append (list (list (car vars) 0)) (gen-tuple (cdr vars) (- index 1))))
		)
)

; generate index number tuples
(defun generate-tuples (vars index)
	(if (eq index (- (list-length vars) 1)) 
		(list (gen-tuple vars index))
		(append (list (gen-tuple vars index)) (generate-tuples vars (+ index 1)))
		)
)

;generate n adds with var
(defun add-gen (var n)
	(if (eq n 1)
		(list `s-var var)
		(list `s-add (list `s-var var) (add-gen var (- n 1)))
	)
)

;function to call add-gen when passed a list of zips
(defun zip-add-gen (var-coord)
	(cond
		; base case when we reach the end of the list
		((eq (list-length var-coord) 1) (add-gen (car (car var-coord)) (nth 1 (car var-coord))))
		; recursive case: return the s-add of the add-gen with the zip-add-gen of the rest of the list
		(T (list `s-add (add-gen (car (car var-coord)) (nth 1 (car var-coord))) (zip-add-gen (cdr var-coord)) ))
	)
)

; returns the zip of all positive coords
(defun get-pos (vars coord)
	(cond 
		; nothing left to retrieve, return the end of the list
		((null vars) nil)	
		; recurse and append the zip of var and coord
		((< 0 (car coord)) (append (list (list (car vars) (car coord))) (get-pos (cdr vars) (cdr coord)))) 
		; recurse and ignore the zip of var and coord if not positive
		(T (get-pos (cdr vars) (cdr coord))) 
	)
)
; returns the zip of all negative coords (but coords are returned as positive now)
(defun get-neg (vars coord)
	(cond 
		; nothing left to retrieve, return the end of the list
		((null vars) nil)	
		; recurse and append the zip of var and coord
		((> 0 (car coord)) (append (list (list (car vars) (- 0 (car coord)))) (get-neg (cdr vars) (cdr coord))))
		; recurse and ignore the zip of var and coord if not negative
		(T (get-neg (cdr vars) (cdr coord)))
	)
)

;generate the compact form
(defun generator (vars coord)
	(setq positive-zip (get-pos vars coord))
	(setq negative-zip (get-neg vars coord))
	
	(cond
		; if we don't have any negative or positive variable states, we simply return an equiv expr that evals to 0
		((and (null positive-zip) (null negative-zip)) (list `s-sub (list `s-var (car vars)) (list `s-var (car vars)))) 
		; if we don't have any positive, return (s-neg(negative states))
		((null positive-zip) (list `s-neg (zip-add-gen negative-zip)))
		; if we don't have any negative, return (positive states)
		((null negative-zip) (zip-add-gen positive-zip))
		; if both not null, return (`s-sub (positive states) (negative states))
		(T (list `s-sub (zip-add-gen positive-zip) (zip-add-gen negative-zip)))
	)
)

;simplify the query to minimal form
(defun simplify (query)
	(setq environments (generate-tuples (find-vars query) 0))	; generate all unit vectors of the environment
	(setq mapfn (transformer query))							; create our function to evaluate how many instances of each unit vec we need
	(setq var-coords (mapcar mapfn environments))				; pairs of variable-instance count over the environment
	(generator (find-vars query) var-coords)					; generate expression to represent instance count for each variable
)
