#lang racket

;; helpers
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))
(define nil '())

;; read expenses
(define expenses
  (with-input-from-file "input.txt"
    (lambda ()
      (vector-sort
       (for/vector ([l (in-lines)])
         (string->number l))
       <))))

;; find the two-sum pair
(define (find-two-sum goal nums)
  (let iter-find-two-sum ([left-idx 0] [right-idx (- (vector-length nums) 1)])
    (if (>= left-idx right-idx)
        nil
        (let* ([left-val (vector-ref nums left-idx)]
               [right-val (vector-ref nums right-idx)]
               [sum-of-pair (+ left-val right-val)])
          (cond ([> sum-of-pair goal] (iter-find-two-sum left-idx (dec right-idx)))
                ([< sum-of-pair goal] (iter-find-two-sum (inc left-idx) right-idx))
                (else (cons left-val right-val)))))))

;; solve
(let ((two-sum (find-two-sum 2020 expenses)))
  (* (car two-sum) (cdr two-sum)))