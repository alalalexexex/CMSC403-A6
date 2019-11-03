; Write a Common Lisp function named myList which creates the following list and returns it
; (4 (7 22) "art" ("math" (8) 99) 100)
(defun myList ()
    (cons 4 (cons (cons 7 (cons 22 () )) (cons "art" (cons (cons "math" (cons (cons 8 () ) (cons 99 () )) ) (cons 100 () ) ) ) )); this still works though   
)

; leapYear question, used pseudocode from wikipedia for the logic. Wasn't sure what a leap year was. 
(defun leapYear (&optional (year 1800) yearList)
    (if (= year 2019) 
        (RETURN-FROM leapYear yearList) ; return yearList if year is 2019 -> 2019 isn't a leap
        (if (not (= 0 (mod year 4))) ; if year isn't divisible by four it is regular
            (leapYear (+ year 1) yearList)
            (if (not (= 0 (mod year 100))) ; else if year isn't divisble by 100 it is leap
                (leapYear (+ year 1) (append yearList (list year)))
                (if (not (= 0 (mod year 400))) ; else if year isn't divisible by 400 it is regular
                    (leapYear (+ year 1) yearList)
                    (leapYear (+ year 1) (append yearList (list year))) ; else it is a leap year
                )
            )
        )
    )
)

; union function
(defun union- (l1 l2 &optional returnList)
    (if (null l2)
        (RETURN-FROM union- (append returnList l1))
        (union- (remove (car l2) l1) (cdr l2) (cons (car l2) returnList))
    )
)

; avg function
(defun avg (aList &optional (count- 0) (sum 0))
    (if (null aList) ; add another if in the true branch
        (if (= count- 0)
            (RETURN-FROM avg ())
            (RETURN-FROM avg (/ sum count-))
        )
        (avg (cdr aList) (+ count- 1) (+ (car aList) sum))
    )
)

; isType function 
(defun isType (dataType)
    (lambda (x)
        (typep x dataType)
    )
)

(defun taxCalculator (limit rate values-)
    (map 'list 
        #'(lambda (x)
            (if (> x limit)
                (* x rate)
                x
            )
        )
        values-
    )
)

; this is pure recursion
(defun clean (aFunc aList)
    (if (NULL aList)
        ()
        (if (typep (car aList) 'list)
            (append (list (clean aFunc (car aList))) (clean aFunc (cdr aList))) ; the list contains sublists (car aList) will be a list
            ; check aFunc as else if statement. this is where we evaluate each item. 
            (if (funcall aFunc (car aList))
                (append (list (car aList)) (clean aFunc (cdr aList)))
                (clean aFunc (cdr aList))
            )
        )
    )
)

; map the eval function to each element in the list, and the final will be the result
(defmacro threeWayBranch (x y toExecute)
    (if (< x y)
        (car (last (map 'list #'eval (first toExecute))))
        (if (> x y)
            (car (last (map 'list #'eval (second toExecute))))
            (car (last (map 'list #'eval (third toExecute))))
        )
    )
)

