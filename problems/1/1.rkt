#lang racket

(define expenses (with-input-from-file "input.txt"
    (lambda ()
      (vector-sort (for/vector ([l (in-lines)])
              (string->number l))
            <))))

(define (find-two-sum goal nums)
  (define (iter left-idx right-idx)
    (let ((sum-of-pair (+ (vector-ref nums left-idx)
                          (vector-ref nums right-idx))))
      (cond ([>= left-idx right-idx] '())
            ([> sum-of-pair goal] (iter left-idx (- right-idx 1)))
            ([< sum-of-pair goal] (iter (+ left-idx 1) right-idx))
            (else (cons (vector-ref nums left-idx)
                        (vector-ref nums right-idx))))))
  (iter 0 (- (vector-length nums) 1)))

(let ((two-sum (find-two-sum 2020 expenses)))
      (* (car two-sum) (cdr two-sum)))