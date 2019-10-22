;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
  (+ c (* k (- n 1)))
  ;use the function provided in the spec
  )

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
(defun at-least-one-color (n c k)
  (cond ((> c k) '())
	;base case, return an empty list
	(t (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))
	;otherwise use "V" to connect all the individual variables
   );end cond 
  )

;helper functiuon for at-most-one-color, construct all the inidividual clauses
;that if cur is true, all other values of c are false
;for example, (one-true 5 3 7 3) returns:
;((31 -31) (-31 -32) (-31 -33) (-31 -34) (-31 -35))
(defun one-true (n c k cur)
  (let ((base (- 0 (node2var n c k))))
    (cond ((> cur k) '())
	  ;base case, return an empty list
	  ((= cur c) (cons (list (- 0 base) base) (one-true n c k (+ cur 1))))
	  ;return (a V ~a), do nothing, only for the case when c == k
          (t (cons (list base (- 0 (node2var n cur k))) (one-true n c k (+ cur 1))))
	  ;return all other cases for c < k
        );end cond
      );end let
  )

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
(defun at-most-one-color (n c k)
  (cond ((> c k) '())
	;base case return empty lilst
	(t (append (one-true n c k c) (at-most-one-color n (+ c 1) k)))
	;otherwise call one-true for all possible c
	);end cond
  )

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
  ;call at-most and at-least together to generate the clause that allows exactly one color
  )

;helper function for generate-edge-clauses, the acutal generate-edge-clauses is a wrapper
;for this, cur means the color we are currently dealing with
(defun helper-generate-edge-clauses (e k cur)
  (cond ((> cur k) '())
	;base case, return empty list
        (t (cons (list (- 0 (node2var (car e) cur k))(- 0 (node2var (second e) cur k))) (helper-generate-edge-clauses e k (+ cur 1)) ))
	;otherwise generate a clause in the form (~a V ~b) that does not allow both nodes to have the same color
        );end cond
  )

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
(defun generate-edge-clauses (e k)
  (helper-generate-edge-clauses e k 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
