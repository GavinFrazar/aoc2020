#lang racket

(define (data->passports data)
  (let ([passports (foldl (lambda (line acc)
                            (if (= 0 (string-length line))
                                (cons '()
                                      (cons (string-join (car acc))
                                            (cdr acc)))
                                (cons (cons line (car acc))
                                      (cdr acc))))
                          '(())
                          data)])
    (cons (string-join (car passports))
          (cdr passports))))

(define (parse-passport passport-string)
  (define present-fields (make-hash))
  (let ([passport-fields (string-split passport-string)])
    (for ([passport-field passport-fields])
      (match-let ([(list key val) (string-split passport-field ":")])
        (hash-set! present-fields key val))))
  present-fields)

(define (parse-passports passport-strings)
  (map parse-passport passport-strings))

(define (valid-passport? passport)
  (and
   (hash-has-key? passport "byr")
   (hash-has-key? passport "iyr")
   (hash-has-key? passport "eyr")
   (hash-has-key? passport "hcl")
   (hash-has-key? passport "ecl")
   (hash-has-key? passport "hgt")
   (hash-has-key? passport "pid")))

(define (count-valid-passports passports)
  (foldl (lambda (passport valid-count)
           (if (valid-passport? passport)
               (+ valid-count 1)
               valid-count))
         0
         passports))

(define (strictly-valid-passport? passport)
  (let ([byr (hash-ref passport "byr")]
        [iyr (hash-ref passport "iyr")]
        [eyr (hash-ref passport "eyr")]
        [hcl (hash-ref passport "hcl")]
        [ecl (hash-ref passport "ecl")]
        [hgt (hash-ref passport "hgt")]
        [pid (hash-ref passport "pid")]
        )
    (and 1))) ;; TODO

(define (count-strictly-valid-passports passports)
  (foldl (lambda (passport valid-count)
           (if (and (valid-passport? passport)
                    (strictly-valid-passport? passport))
               (+ valid-count 1)
               valid-count))
         0
         passports))

;; get input
(define data (file->lines "input.txt"))
(define passport-strings (data->passports data))
(define passports (parse-passports passport-strings))

(define part-1-solution
  (count-valid-passports passports))

(define part-2-solution
  (count-strictly-valid-passports passports))

(displayln
 (format "part-1: ~a" part-1-solution))
(displayln
 (format "part-2: ~a" part-2-solution))
