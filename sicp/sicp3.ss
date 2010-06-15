; 3.1
(define (make-accumulator count)
  (lambda (amount)
    (set! count (+ count amount))
    count))

; 3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (arg)
      (cond ((eq? 'how-many-calls? arg) count)
            ((eq? 'reset-count arg) (set! count 0))
            (else 
             (set! count (+ count 1))
             (f arg))))))

; 3.3/3.4
(define (constant x)
  (lambda (a)
    x))

(define (call-the-cops)
  "911")

(define (make-account balance password)
  (define (withdraw x)
    (if (>= balance x)
        (begin
          (set! balance (- balance x))
          balance)
        "Error, insufficient funds"))
  
  (define (deposit x)
    (set! balance (+ balance x))
    balance)

  (let ((wrong-guesses 0))
    (lambda (password-attempt message)
      (if (eq? password password-attempt)
          (begin
            (set! wrong-guesses 0)
            (cond ((eq? message 'withdraw) withdraw)
                  ((eq? message 'deposit) deposit)))
          (begin
            (if (> wrong-guesses 7)
                (call-the-cops)
                (begin
                  (set! wrong-guesses (+ 1 wrong-guesses))
                  (constant "Error:wrong password"))))))))

; 3.5 Monte Carlo
(define (in-circle? x y r)
  (lambda (xa ya)
    (> (expt r 2)
       (+ (expt (- xa x) 2)
          (expt (- ya y) 2)))))



(define (random-in-range low high)
  (let ((diff (- high low)))
    (+ low (random diff))))