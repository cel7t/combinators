#!/usr/bin/scheme --script

(define-syntax curry
  (syntax-rules (:lock)
    [(_ a :lock (b ...) . rest)
     (curry (a (curry b ...)) :lock . rest)]
    [(_ a :lock b . rest)
     (curry (a b) :lock . rest)]
    [(_ a :lock)
     a]
    [(_ (a ...) . rest)
     (curry (curry a ...) :lock . rest)]
    [(_ a . rest)
     (curry a :lock . rest)]))

(define S
  (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))

(define K
  (lambda (x) (lambda (y) x)))

(define I
  (lambda (x) x))

(define Y
  (lambda (h)
    ((lambda (x) (h (lambda (z) ((x x) z))))
     (lambda (x) (h (lambda (z) ((x x) z)))))))

(define C1
  (curry S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))))

(define C2
  (curry S (C1 S (C1 K (C1 S (S (C1 C1 I) (K I))))) (K (C1 K I))))

(define PAIR
  (curry  C2 (C1 C1 (C1 C2 (C1 (C2 I) I))) I))

(define ZERO
  (curry S K))

(define SUCC
  (curry S (S (K S) K)))

(define ADD
  (curry C2 (C1 C1 (C2 I SUCC)) I))

(define ADD2
  (curry (S ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) S ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) K ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) S (S ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) (S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) I) (K I))))) (K ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) K I))) ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) (S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) ((S ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) S ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) K ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) S (S ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) (S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) I) (K I))))) (K ((S (S (K K) (S (K S) (S (K K) I))) (K (S (S (K S) (S (K K) I)) (K I)))) K I))) I (S (S (K S) K)))) I))

(define MUL
  (curry C2 (C1 C2 (C2 (C1 C1 I) (C1 ADD I))) ZERO))

(define FIRST 
  (curry K))

(define SECOND
  (curry S K))

(define M
  (curry S (C1 MUL (C2 I FIRST)) (C2 I SECOND)))

(define I2
  (curry C1 SUCC (C2 I SECOND)))

(define ONE
  (curry (SUCC ZERO)))

(define FAC*
  (curry S (C1 PAIR M) I2))

(define FAC
  (curry C2 (C2 I FAC*) (PAIR ONE ONE)))

(define (TO-CHURCH n)
  (let loop ((i n))
    (if (> i 0)
      (SUCC (loop (- i 1)))
      ZERO)))

(let [(to-parse (cadr (command-line)))]
  (cond
    [(string? to-parse)
     (begin 
       (display "=> ")
       (display 
         (eval
           `(curry
              ,@(read
                  (open-input-string
                    (if (and (not (null? (cddr (command-line))))
                             (not (null? (cdddr (command-line))))
                             (equal? (cadddr (command-line))
                                     "number"))
                      (string-append "(" to-parse " 1+ 0)")
                      (string-append "(" to-parse ")")))))))
       (newline))]
    [else
      (begin 
        (display "The argument should be a string!")
        (display to-parse)
        (newline))]))
