#lang racket
(provide (all-defined-out))
;Ethan Johnson 


;first part
(define (divisible-by-7? n ) (if (= (modulo n 7) 0) #t #f))             ;modulo 7 on the number given which is either 0 or 1 and if 0 it's true, else it's false

;second part
(define function-3  (lambda (x) (x 3)))                 ;take the item, give it to x, and do with with 3.

;3rd part
(define (my-map a lst)
  (cond
    [(empty? lst) empty]                      ;empty list, empty output
    [else
     (cons (a (first lst))                    ;else we take the first item and apply the func
           (my-map a(rest lst)))]))           ;next we call the same func with rest of the list

;forth
(define zipper
  (lambda (lst lst2)                                      ;function to continue
    (if (or (null? lst) (null? lst2))                     ; if either is null/empty
      empty                                               ; it's empty and we're stopping
      (cons
        (list (car lst) (car lst2))                       ;construct a list with the 1st of both lst and lst
        (zipper (cdr lst) (cdr lst2))))))                 ;continue on with the rest


;fifth
(define (segregate lst)
  (foldr (lambda (a b)                                  ;go through the whole list
           (if (even? a)                                ;if the item we're at is even
               (list (cons a (first b)) (second b))     ;construct a list while moving over the even.
               (list (first b) (cons a (second b)))))   ; else, move over the odd
         '(()()) lst))                                  ;make 2 lists and keep going


;sixth
(define (inty? val lst)                                                            ;int value
 (cond                                                                             ;switch
   [(empty? lst) false]                                                            ;Are we looking at a empty? just return false
   [(string? (car lst)) (inty? val (rest lst))]                                    ;Are we looking at a string? We're looking for int, just move on
   [(number? (first lst)) (if (= (first lst) val) true (inty? val (rest lst)))]    ;Are we looking at a number? Check if it fits! If not, move on
   [else (inty? val (rest lst))]))                                                 ;Otherwise, feed in the the rest of the list and keep trying

(define (stringy? val lst)                                                                ;string value
 (cond                                                                                    ;switch
   [(empty? lst) false]                                                                   ;Are we looking at a empty? just return fals
   [(number? (car lst)) (stringy? val (rest lst))]                                        ;Are we looking at a number? We're looking for string, just move on
   [(string? (first lst)) (if (string=? (car lst) val) true (stringy? val (rest lst)))]   ;Are we looking at a string? Check if it fits! If not, move on
   [else (stringy? val (rest lst))]))                                                     ;Otherwise, feed in the the rest of the list and keep trying

(define (is-member? val lst)                  ;is-member?
  (if (string? val)                           ;check if a string is in value to go from there
      (stringy? val lst)                      ;winner! It's a string, follow from here.
      (inty? val lst)))                       ;Not a string, follow from here


;seventh
(define (s-int? lst)                                                       ;int checker
  (define size (length lst))                                               ;define size as list length
  (if (< size 2) #t                                                        ;if the size is less than 2 (1 or 0) it's true
      (if (null? lst) #t                                                   ;if the list isn't null, it's true
          (if (number? (cadr lst))                                         ;if the SECOND number is a number
              (if (> (car lst) (car (rest lst))) #f                        ;if the first one is bigger, it's false
                   (s-int? (rest lst)))                                    ;otherwise pass to top of this func and go again from what we haven't compared
              (error "ERROR: List contains heterogenous data types")))))   ;Look, y'all done messed up. You gave me something non-matching. In this case, it's a string

(define (s-string? lst)                                                    ;string checker
  (define size (length lst))                                               ;define size as list length
  (if (< size 2) #t                                                        ;if the size is less than 2 (1 or 0) it's true
      (if (null? lst) #t                                                   ;if the list isn't null, it's true
          (if (string? (cadr lst))                                         ;if the SECOND string is a number
              (if (string>? (car lst) (car (rest lst))) #f                 ;if the first one is bigger, it's false
                   (s-string? (rest lst)))                                 ;otherwise pass to top of this func and go again from what we haven't compared
              (error "ERROR: List contains heterogenous data types")))))   ;Look, y'all done messed up. You gave me something non-matching. In this case, it's a number


(define (my-sorted? lst)                  ;my sorted
  (if (string? (car lst))                 ;check if a string is in first to go from there
      (s-string? lst)                     ;winner! It's a string, follow from here.
      (s-int? lst)))                      ;Not a string, follow from here

;eigth
(define (my-flatten lst)
  (cond ((null? lst) '())                                           ;Null and empty list? return empty list!
        ((pair? lst)                                                ;if it's a pair
         (append (my-flatten (car lst)) (my-flatten (cdr lst))))    ;append the 2 by getting the first and last of my list. Loop back to top
        (else (list lst))))                                         ;otherwise, just give me the list now please, thanks

;ninth
(define (upper-threshold lst threshold)
  (cond
    [(null? lst) '()]                                          ;if the list is null and empty, congratz, it's now an empty list
    [(< (car lst) threshold)                                   ;if the first item s less than threshold
     (cons (car lst) (upper-threshold (cdr lst) threshold))]   ;construct a list that has the first item and oh btw recursion time to see if we can add another item
    [else (upper-threshold (cdr lst) threshold)]))             ;if all else fails, move to next item and try again

;tenth
(define my-list-ref
    (lambda (lst index)                                      ;loop up to here
      (if (= index 0)                                        ;If our index our current spot
          (if (null? lst)                                    ;if the lst is null
              (error "ERROR: Index out of bounds")           ;We're out of things to go to. Throw error!
              (car lst))                                     ;otherwise car the first item of lst and ta da!
          ((if (null? lst)                                   ;double check if null lst
               (error "ERROR: Index out of bounds")          ;it's null! THROW ERROR! We're out of items and haven't found it
               (my-list-ref (cdr lst) (- index 1)))))))      ;(go up to first if) otherwise, call self again with rest of list and move index to check if they're 0s 

