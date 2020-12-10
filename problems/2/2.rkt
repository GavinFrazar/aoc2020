#lang racket


;; tokenize the input lines and count the accepted lines using a predicate
(define (solve predicate lines)
  (count
   (lambda (line)
     (match-let ([(list str-min str-max str-letter password)
                  (string-split (string-replace line #px"[-:]" " "))])
       (let* ([min (string->number str-min)]
              [max (string->number str-max)]
              [letter (car (string->list str-letter))])
         (predicate min max letter password))))
   lines))






(define (main)
  ;; read input
  (define lines (file->lines "input.txt"))
  (define (count-occurrences line letter)
    (let ([line (string->list line)])
      (count (lambda (char) (eqv? char letter))
             line)))
  
  ;; solve part 1
  (define part-1-solution
    (solve (lambda (min max letter password)
             (let ([letter-count (count-occurrences password letter)])
               (and (>= letter-count min)
                    (<= letter-count max))))
           lines))
  
  ;; solve part 2
  (define part-2-solution
    (solve
     (lambda (first-position second-position letter password)
       #t)
     lines))

  ;; format the results nicely
  (format "part 1 solution: ~a\npart 2 solution: ~a"
          part-1-solution
          part-2-solution))

(displayln (main))