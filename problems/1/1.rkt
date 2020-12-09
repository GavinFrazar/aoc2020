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
  (let iter-find-two-sum ([left-idx 0]
                          [right-idx (- (vector-length nums) 1)])
    (if (>= left-idx right-idx)
        nil
        (let* ([left-val (vector-ref nums left-idx)]
               [right-val (vector-ref nums right-idx)]
               [sum-of-pair (+ left-val right-val)])
          (cond ([> sum-of-pair goal] (iter-find-two-sum left-idx (dec right-idx)))
                ([< sum-of-pair goal] (iter-find-two-sum (inc left-idx) right-idx))
                (else (list left-val right-val)))))))

;; solve part 1
(define part-1-solution
  (let ([two-sum (find-two-sum 2020 expenses)])
    (* (car two-sum) (cadr two-sum))))

(define (find-three-sum goal nums)
  (define (scan-for-unique-value nums pos)
    (if (>= pos (vector-length nums))
        pos
        (let ([val (vector-ref nums pos)])
          (let iter-scan ([next-pos (inc pos)])
            (if (< val (vector-ref nums next-pos))
                next-pos
                (iter-scan (inc next-pos)))))))
  (let iter-find-three-sum ([i 0])
    (if (>= i (vector-length nums))
        nil
        (let ([two-sum (find-two-sum (- goal (vector-ref nums i))
                                     (vector-drop nums i))])
          (if (null? two-sum)
              (iter-find-three-sum (scan-for-unique-value nums i))
              (cons (vector-ref nums i) two-sum))))))

;; solve part 2
(define part-2-solution
  (let ([three-sum (find-three-sum 2020 expenses)])
    (apply * three-sum)))

(display (format "part1: ~a part2: ~a"
                 part-1-solution
                 part-2-solution))