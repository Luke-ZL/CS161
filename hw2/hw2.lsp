;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;TODO: comment code

; The function takes in a TREE structure (FRINGE). The leaves are visited and
; printed in a BFS order
(defun BFS (FRINGE) ; define function 
  (let ((ele (car FRINGE))) ; use ele to store the first element of FRINGE
    (cond ((null ele) '()) ; If the FRINGE is empty, return an empty list 
          ((atom ele) (cons ele (BFS (cdr FRINGE))))
	  ; if ele is an atom, the result is appended at the front of the
	  ; result of BFS(rest of FRINGE)	  
          ((listp ele) (BFS (append (cdr FRINGE) ele)))
	  ; If ele is a list (subtree), append the subtree to the end of FRINGE
    )
  )
)


;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
  (equal S '(T T T T)) ; check if S is euqal to finial state (T T T T)
  )
  
; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
  (let ((ret (cond ((equal A 'h) (cons (not (car S)) (cdr S)))
                   ((equal A 'b) (cond ((not (equal (first S) (second S))) NIL)
                                       ((equal (first S) (second S)) (cons (not (car S)) (cons (not (second S)) (cons (third S) (list (fourth S))))))
                                       )
                    )
                   ((equal A 'd) (cond ((not (equal (first S) (third S))) NIL)
                                       ((equal (first S) (third S)) (cons (not (car S)) (cons (second S) (cons (not (third S)) (list (fourth S))))))
                                       )
                    )
                   ((equal A 'p) (cond ((not (equal (first S) (fourth S))) NIL)
                                       ((equal (first S) (fourth S)) (cons (not (car S)) (cons (second S) (cons (third S) (list (not (fourth S)))))))
                                       )
                    )
                   )
             )
        )
        ; the above chunk make ret equal to the result of four possible actions
	; However, if the action is not legal, ret is set to NIL (e.g. homer and
        ; dog are on different side of the river
    (cond ((equal ret NIL) NIL)
          ((and (equal (second ret) (third ret)) (not (equal (first ret) (second ret)))) NIL)
          ((and (equal (second ret) (fourth ret)) (not (equal (first ret) (second ret)))) NIL)
          ((= 1 1) (list ret))
    )
    ; check if the final state is illegal, (baby and dog alone), if yes, return NIL
    ; otherwise return ret
  )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
  (let ((h (cond ((equal (NEXT-STATE S 'h) NIL) '())
                 ((= 1 1) (NEXT-STATE S 'h))
                 ))
        (b (cond ((equal (NEXT-STATE S 'b) NIL) '())
                 ((= 1 1) (NEXT-STATE S 'b))
                 ))
        (d (cond ((equal (NEXT-STATE S 'd) NIL) '())
                 ((= 1 1) (NEXT-STATE S 'd))
                 ))
        (p (cond ((equal (NEXT-STATE S 'p) NIL) '())
                 ((= 1 1) (NEXT-STATE S 'p))
                 ))
        )
    ; the above chunk check all the possible next states
    ; NIL state is mad into an empty list to make append easier
    (append h b d p)
    ; append the posible next states into a list
  )
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
  (cond ((equal STATES '()) NIL) ; If STATES is empty return NIL
        ((equal S (car STATES)) T) ; check S with the first element in STATES
        ((= 1 1) (ON-PATH S (cdr STATES))); otherwisee recursively call ON-PATH on the rest of STATES 
        )
  )

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
  (cond ((equal STATES '()) NIL)
	; check if STATES is empty, if yes return NIL
	((= 1 1) (cond ((ON-PATH (car STATES) PATH) (MULT-DFS (cdr STATES) PATH))
	; otherwise check if the first element is visited before, if yes continue with rest of STATES
		       ((= 1 1) (let ((ret (DFS (car STATES) (append PATH (list (car STATES))))))
        	       ; otherwise call DFS on the first element with an updated PATH
		       		  (cond ((equal ret NIL) (MULT-DFS (cdr STATES) PATH))
				  ; if ret is not NIL, return, otherwise continue with rest of STATES		  
					((= 1 1) ret)
					)
				  )
			)
		       )
	 )
	)
  )

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
  (let ((path_decode (cond ((equal PATH NIL) (list S))
			   ((= 1 1) PATH)
			   )
		     )
	)
    ; if the PATH is NIL (initial), make it a correct path
    (cond ((FINAL-STATE S) path_decode)
	  ((= 1 1) (MULT-DFS (SUCC-FN S) path_decode))
	  )
    )
    ; check if we have reached final state
    ; if not, call MULT-DFS on all possible success states
  )

    
