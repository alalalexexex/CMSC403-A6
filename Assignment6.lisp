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

; union function will iterate through all elements in l2, removing each from l1 if they exist. 
; once the end of l2 is found, we return the returnList, where all elements from l2 are appended to those remaining from l1. 
(defun union- (l1 l2 &optional returnList)
    (if (null l2)
        (RETURN-FROM union- (append returnList l1)) ; return the final optional parameter list.
        (union- (remove (car l2) l1) (cdr l2) (cons (car l2) returnList)) ; remove the current l2 element from l1, and add the 
 									  ; l2 element to the return list. 
    )
)

; avg function will count all the elements in the given list and divide the sum of them by the count. 
(defun avg (aList &optional (count- 0) (sum 0))
    (if (null aList) ; add another if in the true branch
        (if (= count- 0)
            (RETURN-FROM avg ()) ; returns NIL if there are no elements in the list. 
            (RETURN-FROM avg (/ sum count-)) ; returns the sum divided by the count.
        )
        (avg (cdr aList) (+ count- 1) (+ (car aList) sum)) ; increments count and adds current element to the sum. 
    )
)

; isType function will return a lambda function that extends typep functionality to the given type parameter.
(defun isType (dataType)
    (lambda (x)
        (typep x dataType)
    ) ; returns the lambda function. 
)

; taxcalculator function will map the lambda function defined to all elements in the list. This will return the newly created 
; list made from the map function
(defun taxCalculator (limit rate values-)
    (map 'list 
        #'(lambda (x)
            (if (> x limit) ; if the current element is greater than limit, apply the rate to the element. 
                (* x rate) 
                x ; if the element doesn't exceed limit, the element stays the same. 
            )
        )
        values-
    )
)

; clean function. Definitely the hardest to implement.
(defun clean (aFunc aList)
    (if (NULL aList) ; first check if current list is finished. 
        () ; if the list is finished, do nothing. 
        (if (typep (car aList) 'list) ; if the current element in the list is a list, apply clean as usual. 
            (append (list (clean aFunc (car aList))) (clean aFunc (cdr aList))) ; the list contains sublists (car aList) will be a list
            ; check aFunc as else if statement. this is where we evaluate each item. 
            (if (funcall aFunc (car aList))
                (append (list (car aList)) (clean aFunc (cdr aList))) ; if aFunc returns true, then add element to the current list. 
                (clean aFunc (cdr aList)) ; if aFunc isn't true, skip and go to next item. 
            )
        )
    )
)

; map the eval function to each element in the list, and the final element in the list will be the result. 
(defmacro threeWayBranch (x y toExecute)
    (if (< x y) ; first check if x < y
        (car (last (map 'list #'eval (first toExecute)))) ; if it is, eval the first sublist. 
        (if (> x y) ; check if x < y 
            (car (last (map 'list #'eval (second toExecute)))) ; if x < y eval the second sublist. 
            (car (last (map 'list #'eval (third toExecute)))) ; if x == y eval the third sublist. 
        )
    )
)

