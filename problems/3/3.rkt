#lang racket

(define (count-trees-hit grid dy dx)
  (define (iter row col tree-count)
    (if [>= row (vector-length grid)]
        tree-count
        (let ([next-row (+ row dx)]
              [next-col (modulo (+ col dy) (string-length (vector-ref grid 0)))]
              [next-tree-count (if (eq? #\# (string-ref (vector-ref grid row) col))
                                   (+ tree-count 1)
                                   tree-count)])
          (iter next-row next-col next-tree-count))))
  (iter 0 0 0))

(define (main)
  ;; get input
  (define lines (list->vector (file->lines "input.txt")))

  ;; solve part-1
  (define part-1-solution
    (count-trees-hit lines 3 1))

  ;; solve part-2
  (define part-2-solution
    (foldr * 1
           (map (lambda (slope)
                  (count-trees-hit lines (car slope) (cadr slope)))
                '((1 1)
                  (3 1)
                  (5 1)
                  (7 1)
                  (1 2)))))                                                                          

  (displayln (format "part-1: ~a\npart-2: ~a" part-1-solution part-2-solution)))

(main)