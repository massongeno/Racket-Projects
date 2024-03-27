#lang racket

(provide (all-defined-out))

(define divisible-by-x?
  (λ (arg1)
    (λ (arg2)
      (if (equal? (remainder arg2 arg1) 0)
          (printf "#t")
          (printf "#f")))))

(define function-4
  (λ (arg1)
    (arg1 4)))

(define my-map
  (λ (foo lst)
    (if (null? lst)
        '()
        (cons (foo (first lst))
              (my-map foo (rest lst))))))

(define pair-up
  (λ (lst1 lst2)
    (if (or (null? lst1) (null? lst2))
        '()
        (cons (list (first lst1) (first lst2) ) (pair-up (rest lst1) (rest lst2))))))
         

(define
  (classify foo lst)
  (classify-helper foo lst '() '()))

(define (classify-helper foo lst lst1 lst2)
  (if (null? lst)
      (list lst1 lst2)
      (if (foo (first lst))
          (classify-helper foo (rest lst) (append lst1 (list (first lst))) lst2)
          (classify-helper foo (rest lst) lst1 (append lst2 (list (first lst)))))))

(define (luhn value)
      (equal? (remainder value 10) (- 10(remainder (luhn-helper (truncate (/ value 10)) 1 0 1) 10))))
          

(define (luhn-helper value digit sum iteration)
  (if (equal? value (remainder value digit))
      (+ sum(* (truncate (/ (remainder value digit) (/ digit 10))) (+ 1 (remainder (+ 1 iteration) 2))))
      (if (equal? (remainder iteration 2) 1)
          (luhn-helper value (* digit 10) (+ sum (truncate (/ (remainder value digit) (/ digit 10)))) (+ iteration 1))
          (if (> (truncate (/ (remainder value digit) (/ digit 10))) 4)
              (luhn-helper value (* digit 10) (+ sum(+ 1(remainder (* 2(truncate (/ (remainder value digit) (/ digit 10))))10))) (+ iteration 1))
              (luhn-helper value (* digit 10) (+ sum(* 2(truncate (/ (remainder value digit) (/ digit 10))))) (+ iteration 1))))))

(define (my-sorted? foo lst)
  (if (null? (rest (rest lst)))
      (foo (first lst) (first (rest lst)))
      (if (foo (first lst) (first (rest lst)))
         (my-sorted? foo (rest lst))
         (foo (first lst) (first (rest lst))))))

(define (my-flatten lst)
  (if (null? lst)
      '()
      (if (number? lst)
          (list lst)
          (append (my-flatten (first lst)) (my-flatten (rest lst))))))
      
(define (upper-threshold lst val)
  (if (null? lst)
      '()
      (if (number? lst)
          (list lst)
          (if (< (first lst) val)
              (append (upper-threshold (first lst) val) (upper-threshold (rest lst) val))
              (upper-threshold (rest lst) val)))))

(define (my-list-ref lst val)
  (my-list-ref-helper lst val 0))

(define (my-list-ref-helper lst val inc)
  (if (null? lst)
      (error "ERROR: Index out of bounds")
      (if (equal? val inc)
          (print (first lst))
          (my-list-ref-helper (rest lst) val (+ 1 inc)))))
          
      

          

               