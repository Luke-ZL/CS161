(defun PAD (N) ;define function
   (cond ((<= N 2) 1) ;conditions: the first 3 cases just return 1
         ((> N 2) (+ (PAD (- N 2)) (PAD (- N 3)))) ;recursive case, recursively call PAD to evaluate when N > 2
)) ;close all brackets

(defun SUMS (N) ;define function
  (cond ((<= N 2) 0) ;;conditions: the first 3 cases require no plus operation
        ((> N 2) (+  (SUMS (- N 2)) (SUMS (- N 3)) 1)) ;recursive call, recursively call SUMS to evaluate when N > 2, the extra 1 is because we need another plus operation to add up the SUMS result for the two subcases
)) ;close all brackets

(defun ANON (TREE) ;define function
  ;there are 3 cases for a input 3: an empty list, an atom, a list with items inside
  (cond ((null TREE) '()) ;case 1: empty, just return an empty list 
        ((atom TREE) '?) ;case 2: atom, just return ?
	((listp TREE) (cons (ANON (car TREE)) (ANON (cdr TREE)))) ;case 3: a list with items inside, recursively call ANON on the first item and the rest of the input TREE, then concatenate the results.
)) ;close all brackets
