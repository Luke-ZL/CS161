;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ((equal s '()) T);return ture if s is empty
        (t (let ((ele (car (car s)))
                 );use ele to represent the current element in s we are checking
             (cond ((equal ele NIL) (goal-test (cdr s)))
		   ;if ele is NIL go to next row
                   ((isBox ele) NIL); if there is a box remaining, return NIL
                   (t (goal-test (cons (cdar s) (cdr s))))
		   ;otherwise go on check the rest
                   );end cond
             );end let
             ));end cond
  );end defun


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
; helper function for get-square, get the element in a list at position c
(defun get-list (l c)
  (cond ((= c 0) (car l))
        (t (get-list (cdr l) (- c 1)))
        );end cond
  );end defun

;get the value of s at (r,c) using get-list
(defun get-square (S r c)
  (cond ((= r 0) (get-list (car S) c))
        (t (get-square (cdr S) (- r 1) c))
        );end cond
  );end defun

; helper function for set-square, set the element to v in a list at position c
(defun set-list (l c v)
  (cond ((= c 0) (cons v (cdr l)))
        (t (cons (car l) (set-list (cdr l) (- c 1) v)))
        );end cond
  );end defun

;set the value of s to v at (r,c) using set-list
(defun set-square (S r c v)
  (cond ((= r 0) (cons (set-list (car S) c v) (cdr S)))
        (t (cons (car S) (set-square (cdr S) (- r 1) c v)))
   );end cond
  );end defun

;helper function for try-move
;update keeper's original position after move
(defun set-current (S x y)
  (let ((cur (get-square S x y)))
    (cond ((isKeeperStar cur) (set-square S x y star)) ;update to star
	  (t (set-square S x y blank));update to blank
     );end cond
   );end let
  )
;x y are the coordinates of the square we want to move to
(defun set-next (S x y)
  (let ((cur (get-square S x y)))
    (cond ((or (isStar cur) (isBoxStar cur)) (set-square S x y keeperstar));update to keeperstar
          ((or (isBlank cur) (isBox cur)) (set-square S x y keeper));update to keeper
     );end cond
   );end let
  );end defun
;x y are the coordinates of the square the box needs to be in
(defun set-box (S x y)
  (let ((cur (get-square S x y)))
    (cond ((isBlank cur) (set-square S x y box)) ;update to box
          (t (set-square S x y boxstar)); update to boxstar
     );end cond
   );end let
  );end defun

;try move the keeper at (x,y) of S
;the direction is specified by D
(defun try-move (S D x y)
  (cond ((equal D 'u) (cond ((= x 0) NIL);check boundary
		       (t (let ((up (get-square S (- x 1) y))) ;get the new keeper position
			    (cond ((isWall up) NIL);check wall
				  ((or (isStar up) (isBlank up)) (set-current (set-next S (- x 1) y) x y));if the square above is a star
				  ((or (isBox up) (isBoxStar up)) (cond ((= x 1) NIL);if the square above is a box. chekc boundary first
									(t (let ((up2 (get-square S (- x 2) y)));get the coordinate above the box above
									     (cond ((or (isStar up2) (isBlank up2)) (set-box (set-current (set-next S (- x 1) y) x y) (- x 2) y));check if we can push the box above
										   (t NIL);if not return NIL
										   );end cond
									     );end let
									   );end t
								   );end cond
				   );end box case
			     );end cond
			   );end let
			);end t
		  );end cond
	 );end up
;repeat the above steps for down, left ,right, all with same code structtures
	((equal D 'd) (cond ((= x (- (length S) 1)) NIL)
                       (t (let ((down (get-square S (+ x 1) y)))
                            (cond ((isWall down) NIL)
                                  ((or (isStar down) (isBlank down)) (set-current (set-next S (+ x 1) y) x y))
                                  ((or (isBox down) (isBoxStar down)) (cond ((= x (- (length S) 2)) NIL)
                                                                        (t (let ((down2 (get-square S (+ x 2) y)))
                                                                             (cond ((or (isStar down2) (isBlank down2)) (set-box (set-current (set-next S (+ x 1) y) x y) (+ x 2) y))
                                                                                   (t NIL)
                                                                                   );end cond
                                                                             );end let
                                                                           );end t
                                                                   );end cond
                                   );end box case
                             );end cond
                           );end let
                        );end t
		       );end cond
	 );end down

	((equal D 'l) (cond ((= y 0) NIL)
                       (t (let ((left (get-square S x (- y 1))))
                            (cond ((isWall left) NIL)
                                  ((or (isStar left) (isBlank left)) (set-current (set-next S x (- y 1)) x y))
                                  ((or (isBox left) (isBoxStar left)) (cond ((= y 1) NIL)
                                                                        (t (let ((left2 (get-square S x (- y 2))))
                                                                             (cond ((or (isStar left2) (isBlank left2)) (set-box (set-current (set-next S x (- y 1)) x y) x (- y 2)))
                                                                                   (t NIL)
                                                                                   );end cond
                                                                             );end let
                                                                           );end t
                                                                   );end cond
                                   );end box case
                             );end cond
                           );end let
                        );end t
                  );end cond
         );end left

	((equal D 'r) (cond ((= y (- (length (car S)) 1)) NIL)
                       (t (let ((right (get-square S x (+ y 1))))
                            (cond ((isWall right) NIL)
                                  ((or (isStar right) (isBlank right)) (set-current (set-next S x (+ y 1)) x y))
                                  ((or (isBox right) (isBoxStar right)) (cond ((= y (- (length (car S)) 2)) NIL)
                                                                        (t (let ((right2 (get-square S x (+ y 2))))
                                                                             (cond ((or (isStar right2) (isBlank right2)) (set-box (set-current (set-next S x (+ y 1)) x y) x (+ y 2)))
                                                                                   (t NIL)
                                                                                   );end cond
                                                                             );end let
                                                                           );end t
                                                                   );end cond
                                   );end box case
                             );end cond
                           );end let
                        );end t
                       );end cond
         );end right
	
   );end cond
  );end defun

;actual next-states function
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (y (car pos))
	 (x (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'u x y) (try-move s 'd x y) (try-move s 'l x y) (try-move s 'r x y)))
	 );combine the results of four directions of try-move into a list
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; the function is admissible, because we can only move one box for each move
; thus, for each misplaced box, we need at least 1 move to move them in place
; hence, h1 will never overestimate
(defun h1 (s)
  (cond ((equal s '()) 0)
	(t (+ (count box (car s)) (h1 (cdr s))))
   );end cond
  )

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

;helper function, less then, return (< a b)
;idea taken from class piazza
(defun lt (a b)
  (not (= (- a b) (abs(- a b))))
  );end defun

;helper function that gets all the coordinates of a item specified
; r c are 0 initially
(defun get-all (s item r c)
  (cond ((equal s '()) '())
        (t (let ((ele (car (car s)))
                 );assign current square value to ele
             (cond ((equal ele NIL) (get-all (cdr s) item (+ r 1) 0)) ;if ele is NIL check next row
                   ((equal ele item) (cons (list r c) (get-all (cons (cdar s) (cdr s)) item r (+ c 1))));if ele=item, count++, then check the rest
                   (t (get-all (cons (cdar s) (cdr s)) item r (+ c 1)));otherwise only check the rest
                   );end cond
             );end let
	   ));end cond
  );end defun

;helper function, get the min value in a list
(defun minls (ls)
  (cond ((equal ls '()) 0);return 0 for empty list
	((= (length ls) 1) (car ls));return the only item if list has only 1 element
	(t (let ((rest (minls (cdr ls))))
	     (cond ((lt (car ls) rest) (car ls))
		   (t rest) ;otherwise compare the head of list with (minls rest of list)
	      );end cond
	       );end let
	 ); t
   );end cond
  )

;helper function that finds the manhattan distance between two squares b and g
(defun find-dis (b g)
  (+ (abs(- (first b) (first g))) (abs(- (second b) (second g))))
  );end defun

;helper function that finds the distance of a box to all the goals. combine into a list
(defun form-list (b ls)
  (cond ((equal ls '()) '())
	(t (cons (find-dis b (car ls)) (form-list b (cdr ls)) ))
	)
  );end defun

;helper function that finds the shortest among all box-goal manhattan distance,
;bls is the list of all boxes, gls is the list of all goals
(defun shortest-list (bls gls)
  (cond ((equal bls '()) '())
	(t (cons (minls (form-list (car bls) gls)) (shortest-list (cdr bls) gls)))
   );end cond
  );end defun

;actual h304990072, it finds the sum of the shortest among all box-goal manhattan distance and multiply by the number of boxes
;the heuristic is always admissible because the value returned is always smaller than any goal-box pairing sum (manhattan distance), 
;which is already an underestimation of moves
(defun h304990072 (s)
  (let ((bls (get-all s box 0 0));get the list of all boxes
	(pos (getKeeperPosition s 0));get the list of keeper
	(gls (append (get-all s star 0 0) (get-all s keeperstar 0 0)));get the position of all goals
        )
    (cond ((equal bls '()) 0)
	  (t (* (minls (form-list (list (second pos) (first pos)) bls)) (length bls))) ;(minls (shortest-list bls gls))))
     );end cond
    );end let
  );end defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
